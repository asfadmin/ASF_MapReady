/*
	GetLoc.c-- part of JPL's earthloc software.
	Translated to C by Orion Lawlor, ASF, 6/97
	 a routine to determine the intersection of:
           1) an ellipsoidal Earth surface
           2) a range vector from an orbiting satellite
       and 3) the known proper Doppler frequency

In this file, you'll find:
	getLoc: external entry point.  Calculates location of specified point.
	getLook: called by getLoc.  Calculates look angle for given range.
	makeMatrix: creates matrix used by calcRange and getDoppler.
	calcRange: calculate range, given look and yaw angles.
	getDoppler: calculates doppler frequency and rate, given look and yaw.


c   Input:      statevec:  double precision 6 element x,y,z,xdot,ydot,zdot of s/c
c		   range:  Known slant range to target
c		      dop:  Known Doppler frequency between s/c and stationary
c     			   target on Earth's (ellipsoidal) surface
c		  aguess:  First guess for azimuth angle
c		 targetAccuracy:  Accuracy to which the iterated dop must predict
c  			   the target location.
c   Output:      deltadop:  difference between the iterated dop and the real
c		 	   dop  ("real dop" - dopguess)
c		 targlat:  geocentric latitude of target
c		 targphi:  geocentric longitude+hourangle of target
c		 targrad:  radius of ellipsoidal earth at target
c		       *:  return label in error condition
c

Converted from Howard Zebker's getloc.f Fortran code.

*/
#include "asf.h"
#include "asf_nan.h"
#include "earthloc.h"

double calcRange(GEOLOCATE_REC *g,double look,double yaw);

int getLoc(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
           double *latitude,double *phi,double *earthRadius) /*Outputs.*/
{
        int err;
	double yaw=0,look=0;
	vector target;
	err = getLookYaw(g,range,dop,&look,&yaw);
        if (err) return err;
	getDoppler(g,look,yaw,NULL,NULL,&target,NULL);
	cart2sph(target,earthRadius,latitude,phi);
        return 0;
}

int getLookYaw(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
               double *out_look,double *out_yaw)/* Outputs, in radians.*/
{
	int iterations=0,err=0,max_iter=10000;
	double yaw=0,deltaAz;
	double look=0;
	double dopGuess,dopDotGuess,deltaDop,prevDeltaDop=-9999999;
	vector target,vRel;

        while (1)
	{
		double relativeVelocity;
                //if (iterations>10) printf("GetLoc: Iteration %i, doppler=%f, yaw=%20.18f, look=%20.18f.\n",iterations,dopGuess,yaw*180/M_PI,look*180/M_PI);
		err=getLook(g,range,yaw,&look);
                if (err) break;
		getDoppler(g,look,yaw,&dopGuess,&dopDotGuess,&target,&vRel);
		deltaDop=dop-dopGuess;
		relativeVelocity=vecMagnitude(vRel);
		deltaAz=deltaDop*(g->lambda/(2*relativeVelocity));
                // handle the zero doppler case -- without this, we will
                // sometimes flip back&forth accross zero doppler, until max_iter
                if (fabs(deltaDop+prevDeltaDop)<.000001) deltaAz/=2.;
		if (fabs(deltaAz*range)<0.1)/*Require decimeter convergence*/
			break;
		yaw+=deltaAz;
                if (++iterations>max_iter) { /* Failed to converge */
                    err=1; break;
                }
                prevDeltaDop=deltaDop;
	}
	*out_look=look;
	*out_yaw=yaw;
        return err;
}

/*	getlook.c: part of JPL's earthloc software.
	Translated to C by Orion Lawlor, ASF, 6/97

function getlook (xsc,range,rprec,side,choice,a) -------
c
c   Dec 87
c
c   added double precision aug 88
c
c   Subroutine to calculate the look angle from the s/c to the "touch point"
c   of the range vector with the Earth's surface, where the
c   Earth's surface is defined by an ellipsoid with an equatorial radius (re)
c   and a polar radius (rp).  These radii are trasfered via the common block
c   labelled /earth/.
c
c   Input: xsc[2] = double precision x,y,z components of SAR position (m)
c         range  = magnitude (double precision) of slant range vector (m)
c         rprec  = precision to which iterational range matches input range (m)
c         side   = l or L for left looking, r or R for right looking (char*1)
c
c    Output: function returns last value of look that represents the
c            look angle (as defined below) that produces a touch point vector
c            that is within the specifed range of the input value range.
c            Units are in radians.
c
c   Definition of angles:  Begin with SAR in perfect orbital allignment
c               (0 Yaw,Pitch and Roll).  look is Roll + SAR look angle.
c		After look rotation, a yaw of angle a is performed on
c		the vector.  Forward yaw (toward vel. vector) is positive a.
c               The angle look is returned, the angle a is assumed as
c               specified in choice.
c
c   Procedure:  The slant range vector "touch" point is calculated by
c               iteratively varying the look angle until the magnitude
c               of the resulting vector is within one millimeter
c               of input argument range.  The resulting look angle is
c               returned.  Bails out if more than 100 iterations go by.
c
c
c*/

int getLook(GEOLOCATE_REC *g,double range,double yaw,double *plook)
{
  double ht,delta_range,look;
  double coslook;
  double earth_radius;
  int iter;

  /* The lowest point on Earth is the shore of the Dead Sea (Isreal/Jordan),
     and is 417.5 meters below sea level.  Worst case range should not
     indicate a level lower than this.  The 450.0 m tolerance below was
     selected as a number larger than this because a) the code uses a simple
     spherical model based on the earth radius at the center of the scene,
     and b) look angle results in a range vector longer than what the range
     would've been if the lowest spot on earth fell on the NADIR line.
  */
  double range_tolerance = 450.0; /* meters */

  /* Look angle in y'z' plane, Yaw angle in z''x' plane.
     First guess at a look angle-- a simple spherical guess: */
  /*
     NOTE: This function is only called when working with slant range
     and ground range (meta->sar->image_type == 'S' or 'G') images
     but not for map-projected data ...otherwise, the earth radius
     at center of scene may not be populated depending upon the source of
     the image data, e.g. if imported and geocoded entirely by ASF tools
     then the earth_radius field in the SAR portion of the metadata
     _would_ be populated, but if a GeoTIFF (for example) were imported
     then only the major/minor fields in the projection block would be
     populated, not the earth_radius field in the SAR block ...the
     init_geolocate_meta() takes this into account for the polar
     radius (g->rp) regardless, populates g->earth_radius for SAR
     images, but ignores g->earth_radius for projected images.  But as
     mentioned, this is OK since getLook() is only for 'S' and 'G'
     (SAR) images.
  */

  if (meta_is_valid_double(g->earth_radius)) {
    earth_radius = g->earth_radius;
  }
  else {
    earth_radius = g->rp;
    // FIXME: Else obtain from calcRange() ?  Else default to polar radius ?
  }

  ht=vecMagnitude(g->stVec.pos);
  if (range < (ht - earth_radius) - range_tolerance) {
      asfPrintWarning("getLook(): Range vector does not reach earth!\n");
      return 1;
  }
  if (ht < earth_radius - range_tolerance) {
      asfPrintWarning("getLook(): orbit is below earth's surface!\n");
      return 1;
  }

  /* Calculate look angle */
  coslook = (ht*ht+range*range-earth_radius*earth_radius)/(2.0*range*ht);
  if (!meta_is_valid_double(coslook)) {
      asfPrintWarning("getLook(): coslook was NaN!\n");
      return 1;
  }
  if (coslook > 1.0) {
      asfPrintWarning("getLook(): cosine(look angle) resulted in a value "
                      "larger than 1.0!\n");
      return 1;
  }
  look = acos(coslook);

  /* For a left-looking SAR, we define the look angle to be negative.*/
  if ( g->side == 'L')
    look=-look;

  /* FIXME: The use of an unweighted earth radius at center of scene will
     result in larger errors in regions of the image that are far (corners)
     from where the earth_radius value is accurate.  This may be an issue
     for wide-swath images such as ScanSAR.
  */
  for (iter=0;iter<100;iter++) {
    double sininc,taninc;
    delta_range = range - calcRange(g,look,yaw);
    /* Require decimeter convergence.  */
    if (abs(delta_range) < 0.1) {
      *plook = look;
      return 0;
    } else { /* Havn't converged yet, so update look angle.  */
      sininc = (ht/earth_radius)*sin(look);   /* sin of inci. angle(approx) */
      taninc = sininc/sqrt(1-sininc*sininc);   /* tan of inci. angle */
      /* The linear approximation of atan should be applicable for
         these small arguments.  */
      look += delta_range/(range*taninc);   /* update for look angle */
    }
  }

  /*If we get here, our look iteration hasn't converged.*/
  asfPrintWarning("Error in getLook(): look iteration didn't converge.\n");
  return 1;
}

/* Provide a wrapper with the old interface */
double getLook_abort(GEOLOCATE_REC *g,double range,double yaw)
{
    double look;
    int err;
    err = getLook(g,range,yaw,&look);
    if (err)
        bail("getLook(): Aborting...\n");
    return look;
}

/*
double calcRange(GEOLOCATE_REC *g, double look, double yaw);
	Calculates the slant range, at the given look and yaw angle,
	from the given satellite's position to the earth's surface.
*/
double calcRange(GEOLOCATE_REC *g,double look,double yaw)
{
	vector rvec;
	vector sarPos=g->stVec.pos;
	double ans1,ans2;
	double re2,rp2;
	double a,b,c,d;

	rvec.x= sin(yaw);
	rvec.y=-sin(look)*cos(yaw);
	rvec.z=-cos(look)*cos(yaw);
/* Unit vector rvec points to target.  Rotate into earth centered vector:*/
	vecMul(g->look_matrix,rvec,&rvec);
/* calculate range to intercept the earth (modeled as an ellipsoid).   */
	re2=g->re*g->re;
	rp2=g->rp*g->rp;
	a=(rvec.x*rvec.x+rvec.y*rvec.y)/re2 + rvec.z*rvec.z/rp2;
	b=2.0*((sarPos.x*rvec.x + sarPos.y*rvec.y)/re2 + sarPos.z*rvec.z/rp2);
	c=(sarPos.x*sarPos.x+sarPos.y*sarPos.y)/re2 + sarPos.z*sarPos.z/rp2 - 1.0;
/*  quadradic formula...save nearer range point (the first Earth intersection).*/
	d=(b*b-4.0*a*c);
	if (d<0) return -1.0; /* Path does not intersect earth. */
	ans1=(-b + sqrt(d))/(2.0*a);
	ans2=(-b - sqrt(d))/(2.0*a);
	if (ans1<ans2)
		return ans1;
	else
		return ans2;
}

/*
	GetDoppler:  part of JPL's earthloc software.
	Translated to C by Orion Lawlor, ASF, 6/97
c    subroutine getdop2(xlam,stateVec,look,a,fd,fdot) -------
c
c    Getdop performs vector arithmetic to calculate doppler
c    frequency and rate at a certain look vector look, yaw.
c
c       Input:
c               stateVec = x,y,z,xdot,ydot,zdot of SAR  (m,double precision)
c               look = look angle (radians,double precision)
c               yaw = yaw angle (radians,double precision) (aka squint angle)
c
c       Return: fd = Doppler center frequency
c               fdot = Doppler frequency rate
c               targPos = target point, in inertial coordinates
c               relVel=relative velocity between satellite and targPos.
c
C    5/5/88:    range is determined iteratively by function calcRange
c               instead of being input.
c
c       aug 88 made double precision
c*/

int getDoppler(GEOLOCATE_REC *g,double look,double yaw,
               double *fd, double *fdot,
               vector *out_targPos,vector *out_relVel)
{
	vector relPos, /*Vector from spacecraft to targPos.*/
		sarPos, /*Position of spacecraft*/
		targPos, /*Position of target point.*/
		relVel, /*Relative velocity vector.*/
		sarVel, /*Velocity of spacecraft*/
		targVel, /*Velocity of targPos*/
		sarAcc, /*Accelleration of spacecraft*/
		targAcc, /*Accelleration of targPos*/
		relAcc; /*Relative sarAccelleration*/
	double range,angVel=g->angularVelocity;
	sarPos=g->stVec.pos;
	sarVel=g->stVec.vel;
	relPos.x= sin(yaw);
	relPos.y=-sin(look)*cos(yaw);
	relPos.z=-cos(look)*cos(yaw);
/*c   relPos unit vector points from s/c to targPos.  Rotate into earth axes:*/
	vecMul(g->look_matrix,relPos,&relPos);
/*c   scale relPos so it reaches from s/c to targPos */
	range = calcRange(g,look,yaw);
	vecScale(&relPos,range);
	vecAdd(relPos,sarPos,&targPos);
/*c
c
c  Now we have all three vectors in earth centered coordinates:
c     sarPos = sar satellite position
c     relPos = range vector from sar to targPos
c     targPos = target position
c
c   calculate velocity vectors targVel and relVel.
c*/
	targVel.x= -angVel*targPos.y;
	targVel.y= angVel*targPos.x;
	targVel.z= 0.0;
	vecSub(targVel,sarVel,&relVel);
/*c
c  Calcuate accelerations of sar and targPos sarAcc,targAcc
c  */
	sarAcc.x=0.0;
	sarAcc.y=0.0;
/*c     sar sarAcceleration toward earth center, via
   orbital considerations (accelleration is straight down,
   at      -GxMe / h**2) */
	sarAcc.z=-g->gxMe/vecDot(sarPos,sarPos);
	vecMul(g->look_matrix,sarAcc,&sarAcc);/* !put in e.c. coordinates
c   calculate sarAcceleration of targPos on earth surface:*/
	targAcc.x=-targPos.x*angVel*angVel;
	targAcc.y=-targPos.y*angVel*angVel;
	targAcc.z=0.0;
	vecSub(targAcc,sarAcc,&relAcc);
/*c
c   calculate doppler parameters
c*/
	if (out_targPos)
		*out_targPos=targPos;
	if (out_relVel)
		*out_relVel=relVel;
	if (fd)
		*fd=-2.0/(g->lambda*range)*vecDot(relPos,relVel);
	if (fdot)
		*fdot=-2.0/(g->lambda*range)*(vecDot(relVel,relVel)+vecDot(relPos,relAcc));

        /* success */
        return 0;
}
