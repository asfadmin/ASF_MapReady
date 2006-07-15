/*
Metadata routines that relate to Synthetic Aperture Radar.

This includes:
	- Geolocation based on slant range, time, and doppler
	- Interferometry equations

Orion Sky Lawlor, olawlor@acm.org, 2006/06/20
*/
#include "asf_meta/metadata.h"
#include "asf/units.h"
using namespace asf;

namespace sar_geolocate {



/************** Big Routines *****************
	The below routines form the bulk of the geolocation
library. 

init_geolocate returns a GEOLOCATE_REC, which is a handle
passed to all other routines.  init_geolocate_meta (in asf_meta.h),
copies over the look side, wavelength, etc. from a meta_parameters
record, which is much better.

GetLatLongMeta (in asf_meta.h) is the main entry-- it takes a GEI
state vector, slant range, doppler, elevation (estimate), and
greenwich hour angle, and returns a lat,long, and earth radius.

The low-level routines revolve around two new angles: gamma, which is pi minus the
look angle, and azimuth (az), which is the yaw-rotation of the spacecraft, 
measured in radians clockwise (when viewed from the top) from a vector 
perpendicular to the spacecraft flight path.

getLook calculates gamma given a state vector, slant range, precision (m), side ('L' or 'R'),
and azimuth.  It assumes an ellipsoidal earth described in earthRec.

getLookYaw returns the look and yaw angles of the imaged target, in radians.

getDoppler calculates the target vector, relative velocity vector, doppler
frequency (fd), doppler rate (fdot), given a state vector, gamma angle, and azimuth
angle.

yaw2doppler computes an estimated doppler frequency based on the
yaw angle.  This frequency is returned in Hz.

getHeading computes the heading of a state vector-- the track angle
to true north, measured counterclockwise when viewed from above.
*/
/*Also see the geolocate initialization routines in asf_meta.h!*/

/***************** A "geolocate_rec", a private 
structure used in the geolocate library. *************/
struct GEOLOCATE_REC {
/*Satellite parameters:*/
	stateVector stVec;/* Inertial-coordinates-at-GHA-of-0 state vector: fixed-earth position, but with inertial velocity term */
	double lambda;/*Satellite radar wavelength*/
	char side;/*SAR look side ('L'->left; 'R'->right)*/	
	vector look_matrix[3];/*3x3 matrix to translate s/c-centered coords to earth-centered coords*/

/*Earth Parameters*/
	double re;/*Equator Radius, in meters*/
	double rp;/*Polar Radius, in meters*/
	double angularVelocity; /* 2*PI/dayLength: rotation rate in radians per second.*/
	double gxMe; /*Gravitational Constant times mass of planet (Kg).*/
};

/// Build a GEOLOCATE_REC from this metadata source.
void init_geolocate(const asf::metadata_source &meta,
	const asf::metaCoord_t &loc,GEOLOCATE_REC *g);

/// Build a GEOLOCATE_REC from this fixed-earth state vector.
void init_geolocate(const asf::metadata_source &meta,
	const asf::meta_state_t &fixed_stVec,double elev,GEOLOCATE_REC *g);

inline void free_geolocate(GEOLOCATE_REC *g) {}

			
/** Return the target point's location in body-fixed 3D meters */
vector getLocCart(GEOLOCATE_REC *g,double range,double dop,double *out_doppler_rate=0);


/** Internal routines */
void getLookYaw(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
			double *out_look,double *out_yaw);/* Outputs, in radians.*/
			
double getLook(GEOLOCATE_REC *g,double range,double yaw);

void getDoppler(GEOLOCATE_REC *g,double look,double yaw,
	double *doppler_hz, double *doppler_rate_hz_sec,
	vector *targPos,vector *relVel);

double yaw2doppler(GEOLOCATE_REC *g,double slantRange,double yawAngleDeg);

double getHeading(const stateVector *stVec);


/********************** Setup ************************/

/*
	MakeMatrix:
Constructs 3x3 matrix which rotates spacecraft-
centered coordinates to earth-centered coordinates.
*/

void makeMatrix(GEOLOCATE_REC *g)
{
	vector v,ax,ay,az;
	az=g->stVec.pos;
	v=g->stVec.vel;
	vecNormalize(&az);
	vecNormalize(&v);
	vecCross(az,v,&ay);
	vecCross(ay,az,&ax);
	g->look_matrix[0] = ax;
	g->look_matrix[1] = ay;
	g->look_matrix[2] = az;
}

void init_geolocate(const asf::metadata_source &meta,
	const asf::meta_state_t &fixed_stVec,double elev,GEOLOCATE_REC *g)
{
/* Read fields straight out of meta */
	asf::metaCoord_t loc(0,0,0); /**< Simple sample location--none of these fields are location-dependent */
	g->stVec=fixed_stVec;
	
	g->lambda=meta(WAVELENGTH,loc);
	g->side=meta(IS_RIGHT_LOOKING)?'R':'L';
	g->re=meta(ELLIPSOID_EQUATORIAL,loc);
	g->rp=meta(ELLIPSOID_POLAR,loc);
	g->angularVelocity=meta(SIDEREAL_ROTATION_RATE_RADIANS,loc);
	g->gxMe=meta(G_TIMES_MASS_PLANET,loc);
	
/* Compensate for elevation */
	/**
	If millimeter-scale geolocations are needed, adjust for different
	 up vector at different locations (see pp/pp_ssp/src/pp_correlator/seetarg.f):
	ratio = (r+elev/cos((lat-lat_d)*rad_per_deg))/r
	re = re*ratio
	rp = rp*ratio
	For normal purposes, just bump up the equatorial and polar radii;
	this has an error of about 1mm/km elevation at 45 degrees latitude.
	*/
	g->re+=elev; g->rp+=elev;
	
/* Convert state vector velocity to inertial from fixed-earth */
	g->stVec=g->stVec.rotate_coriolis(0,g->angularVelocity);
	makeMatrix(g);
}

void init_geolocate(const asf::metadata_source &meta,
	const asf::metaCoord_t &loc,GEOLOCATE_REC *g)
{
	meta3D_t std=meta(SLANT_TIME_DOPPLER,loc);
	double time=std.y;
	init_geolocate(meta,
		meta(SATELLITE_FROM_TIME,asf::meta3D_t(time,0,0)),
		loc.z,g);
}

/*
SAR Slant-Range Geolocation Routines

	from getLoc.c-- part of JPL's earthloc software.
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
and extensively modified by Orion Lawlor, olawlor@acm.org, 1997-2006
*/

#if 0 /* These aren't needed any more; getLocCart is all we should be using here... */
void getLatLon(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
			double *out_lat,double *out_lon,double *earthRadius) /*Outputs.*/
{
	double lat,lon;
	cart2sph(getLocCart(g,range,dop),earthRadius,&lat,&lon);
	
/*Convert longitude to (-pi, pi].*/
	if (lon < -pi)
		lon += 2*pi;
	
/*Convert latitude to geodetic, from geocentric.*/
	lat=atan(tan(lat)*(g->re/g->rp)*(g->re/g->rp));
	if (out_lon) *out_lon = lon*R2D;
	if (out_lat) *out_lat = lat*R2D;
}

/** Return the earth-fixed location for this point on (this) Earth's surface.
    lat and lon are in radians.  elev is meters from the ellipsoid.
    
See: Section 2.5.2 of
	http://www.ceegs.ohio-state.edu/gsreports/reports/report_453.pdf
or
	http://www.posc.org/Epicentre.2_2/DataModel/ExamplesofUsage/eu_cs35.html
*/
vector latLon2cart(GEOLOCATE_REC *g,double elev,double deg_lat,double deg_lon)
{
/*Convert latitude and longitude to radians */
	double lat=deg_lat*D2R;
	double lon=deg_lon*D2R;
	/* e2==Earth eccentricity, squared */
	double e2=1.0-(g->rp*g->rp)/(g->re*g->re);
	double cosLat=cos(lat), sinLat=sin(lat);
	/* "prime vertical radius of curvature" at our latitude */
	double N=g->re/sqrt(1-e2*sinLat*sinLat);
	vector ret;
	/* Cartesian to spherical, in a coordinate system stretched along Z by (1-e2) */
	ret.x=(N+elev)*cosLat*cos(lon);
	ret.y=(N+elev)*cosLat*sin(lon);
	ret.z=(N*(1-e2)+elev)*sinLat;
	return ret;
}
#endif


/************ SAR Geolocation Algorithm ************/

vector getLocCart(GEOLOCATE_REC *g,double range,double dop,double *out_doppler_rate)
{
	double yaw=0,look=0;
	double test_doppler; /* Reconstructed doppler */
	vector target;
	getLookYaw(g,range,dop,&look,&yaw);
	getDoppler(g,look,yaw,&test_doppler,out_doppler_rate,&target,NULL);
	if (fabs(test_doppler-dop)>1.0e-2) 
		asf::die("getLocCart> Bad doppler returned from getLookYaw!");
	return target;
}

double calcRange(GEOLOCATE_REC *g,double look,double yaw);

void getLookYaw(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
			double *out_look,double *out_yaw)/* Outputs, in radians.*/
{
	int iterations;
	double yaw=0,deltaAz;
	double look=0;
	double dopGuess,dopDotGuess,deltaDop;
	vector target,vRel;
	
	for (iterations=0;iterations<100;iterations++)
	{
		double relativeVelocity;
/*DEBUGF("GetLoc: Iteration %i, doppler=%f, yaw=%20.18f, look=%20.18f.\n",iterations,dopGuess,yaw*180/M_PI,look*180/M_PI);*/
		look=getLook(g,range,yaw);
		if (look!=look) { /* Look vector is a NaN!  Just stop. */
			break;
		}
		getDoppler(g,look,yaw,&dopGuess,&dopDotGuess,&target,&vRel);
		deltaDop=dop-dopGuess;
		relativeVelocity=vecMagnitude(vRel);
		deltaAz=deltaDop*(g->lambda/(2*relativeVelocity)); 
		if (fabs(deltaAz*range)<0.1)/*Require decimeter
convergence*/
			break;
		yaw+=deltaAz;
	}
	*out_look=look;
	*out_yaw=yaw;
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
c   Definition of angles:  Begin with SAR in perfect orbital alignment
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
double getLook(GEOLOCATE_REC *g,double range,double yaw)
{
  double ht,delta_range,look;
  int iter;

  /* Look angle in y'z' plane, Yaw angle in z''x' plane.
     First guess at a look angle-- a simple spherical guess: */
  ht=vecMagnitude(g->stVec.pos);
  if (range < (ht-g->rp)) 
    asf::die("getLook(): Range vector does not reach earth!\n");
  if (ht < g->rp)  
    asf::die("getLook(): orbit is below earth's surface!\n");
  look= acos((ht*ht+range*range-g->rp*g->rp)/(2.0*range*ht));
	
  /* For a left-looking SAR, we define the look angle to be negative.*/
  if ( g->side == 'L') 
    look=-look;
	
  for (iter=0;iter<100;iter++) {
    double sininc,taninc;
    delta_range = range - calcRange(g,look,yaw);
    /* Require decimeter convergence.  */
    if (fabs(delta_range) < 0.1) {
      return look;
    } else { /* Havn't converged yet, so update look angle.  */
      sininc = (ht/g->rp)*sin(look);   /* sin of inci. angle(approx) */
      taninc = sininc/sqrt(1-sininc*sininc);   /* tan of inci. angle */
      /* The linear approximation of atan should be applicable for
         these small arguments.  */
      look += delta_range/(range*taninc);   /* update for look angle */
    }
  }

  /*If we get here, our look iteration hasn't converged.*/
  printf("WARNING: asf_meta/geolocate.c/getLook(%.3f,%.3f) returning %.3f\n",range,yaw,look);
  if (0) asf::die("Error in getLook(): look iteration didn't converge.\n");
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
	
/* Calculate range to intercept the earth, modelled as an ellipsoid.   
    This is basically raytracing-- we solve a quadratic equation for
    distance to the Earth ellipsoid along the look vector (rvec).
*/                            
	re2=g->re*g->re;
	rp2=g->rp*g->rp;
	a=(rvec.x*rvec.x+rvec.y*rvec.y)/re2 + rvec.z*rvec.z/rp2;
	b=2.0*((sarPos.x*rvec.x + sarPos.y*rvec.y)/re2 + sarPos.z*rvec.z/rp2);
	c=(sarPos.x*sarPos.x+sarPos.y*sarPos.y)/re2 + sarPos.z*sarPos.z/rp2 - 1.0;
/*  quadratic formula...save nearer range point (the first Earth intersection).*/
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
c       Return: fd = Doppler center frequency (Hz) 
c               fdot = Doppler frequency rate (Hz/sec)
c               targPos = target point, in inertial coordinates
c               relVel=relative velocity between satellite and targPos.
c
C    5/5/88:    range is determined iteratively by function calcRange
c               instead of being input.
c                       
c       aug 88 made double precision 
c*/
void getDoppler(GEOLOCATE_REC *g,double look,double yaw,
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
/*  
   Spacecraft acceleration is towards Earth's center. 
   Because our spacecraft coordinate frame has z pointing
   to center of earth, this is just  
          -G mass_earth / height**2  (Newton's law of gravitation) 
*/
	sarAcc.z=-g->gxMe/vecDot(sarPos,sarPos);
	vecMul(g->look_matrix,sarAcc,&sarAcc);/* put in earth centered coordinates */

/*c   Acceleration of a point fixed to the earth's surface
      just comes from the equations for uniform circular motion.
*/
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
	if (fd) /* Doppler, in Hz */
		*fd=-2.0/(g->lambda*range)*vecDot(relPos,relVel);
	if (fdot) /* Doppler rate, Hz/sec */
		*fdot=-2.0/(g->lambda*range)*(vecDot(relVel,relVel)+vecDot(relPos,relAcc));
}

}; /* end namespace */




/*************** Apply these routine to the meta interface ***********/

double asf::metadata_sar::meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_sar::meta1D",*this);
	switch (v) {
	case SLANT_RANGE:
		return meta(SLANT_TIME_DOPPLER,loc).x;
	case TIME_SINCE_START:
		return meta(SLANT_TIME_DOPPLER,loc).y;
	case DOPPLER:
		return meta(SLANT_TIME_DOPPLER,loc).z;
	case DOPPLER_RATE: {
		sar_geolocate::GEOLOCATE_REC g;
		meta3D_t std=meta(SLANT_TIME_DOPPLER,loc);
		double slant=std.x, time=std.y, doppler=std.z;
		sar_geolocate::init_geolocate(meta,
			meta(SATELLITE_FROM_TIME,asf::meta3D_t(time,0,0)),
			loc.z,&g);
		double doppler_rate;
		getLocCart(&g,slant,doppler,&doppler_rate);
		return doppler_rate;
	}
	/* PRF is a bedrock field */
	/* wavelength is a bedrock field */
	case FREQUENCY: 
		return SPEED_OF_LIGHT/meta1D(WAVELENGTH,loc);
	case WAVENUMBER:
		return 2*M_PI/meta1D(WAVELENGTH,loc);
	/* everything else is fundamental */
	
	case INTERFEROMETRIC_REFERENCE_LOOK_RADIANS: {
		/* Arbitrary choice of reference angle: corner of image */
		return meta1D(LOOK_RADIANS,meta3D_t(0,0,0));
	}
	case INTERFEROMETRIC_LOOK_RADIANS: {
		/* Interferometric look angle difference is just actual look minus reference */
		return meta1D(LOOK_RADIANS,loc)-meta1D(INTERFEROMETRIC_REFERENCE_LOOK_RADIANS,loc);
	}
	case INTERFEROMETRIC_FLAT_PHASE: {
		/* flat-Earth look angle difference to reference range */
		double flat=meta1D(INTERFEROMETRIC_LOOK_RADIANS,loc);
		meta2D_t base=meta2D(INTERFEROMETRIC_BASELINE,loc);
		return 2.0*meta1D(WAVENUMBER,loc)*(base.x*cos(flat)+base.y*sin(flat));
	}
	
	case INTERFEROMETRIC_PHASE_RATE: {
		double sr=meta1D(SLANT_RANGE,loc);
		double flat=meta1D(INTERFEROMETRIC_LOOK_RADIANS,loc);
		double incid=meta1D(INCIDENCE_RADIANS,loc);
		meta2D_t base=meta2D(INTERFEROMETRIC_BASELINE,loc);
		
		/* This is the derivative of INTERFEROMETRIC_FLAT_PHASE with respect to "flat" */
		double deriv=2.0*meta1D(WAVENUMBER,loc)*(-base.x*sin(flat)+base.y*cos(flat));
		/* "sr*sin(incid)" is our lever arm length.  "deriv" converts look angle to phase difference */
		/* FIXME: there's a nonlinear version of this.  I just don't know what it is... */
		return (sr*sin(incid))/deriv;
	}
	default:
		return super::meta1D(v,loc);
	}
}

asf::meta2D_t asf::metadata_sar::meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const
{
	return super::meta2D(v,loc);
}
asf::meta3D_t asf::metadata_sar::meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const
{
	metasource_watcher watcher(v,"metadata_sar::meta3D",*this);
	switch (v) {
	/* The only actual geolocation call we support. Convert SAR parameters
	  (slant range, time, doppler, and a state vector) to a position.
	  Latitudes and longitudes are computed from here.
	*/
	case TARGET_POSITION:  {
		meta3D_t std=meta3D(SLANT_TIME_DOPPLER,loc);
		double slant=std.x, time=std.y, doppler=std.z, elev=loc.z;
		sar_geolocate::GEOLOCATE_REC g;
		sar_geolocate::init_geolocate(*this,
			meta_state(SATELLITE_FROM_TIME,asf::meta3D_t(time,0,0)),
			elev,&g);
		return getLocCart(&g,slant,doppler,0);
	}
	case IMAGE_FROM_XYZ: {
		return meta3D(IMAGE_FROM_STD, /* <- implemented by child */
		        meta3D(STD_FROM_XYZ, /* <- below */
			 loc));
	}
	case XYZ_FROM_STD: { /* Compute body-fixed location from slant/time/doppler */
		asf::die("FIXME in metadata_sar::meta3D: XYZ_FROM_STD not implemented");
	}
	case STD_FROM_XYZ: { /* FIXME: implement this, probably with an iterative search */
		asf::die("FIXME in metadata_sar::meta3D: STD_FROM_XYZ not implemented");
	}
	default:
		return super::meta3D(v,loc);
	}
}

