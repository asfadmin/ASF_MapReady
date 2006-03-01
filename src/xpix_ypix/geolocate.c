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
#include "asf.h"
#include "earthloc.h"

vector getLocCart(GEOLOCATE_REC *g,double range,double dop);

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


/************ SAR Geolocation Algorithm ************/

vector getLocCart(GEOLOCATE_REC *g,double range,double dop)
{
	double yaw=0,look=0;
	vector target;
	getLookYaw(g,range,dop,&look,&yaw);
	getDoppler(g,look,yaw,NULL,NULL,&target,NULL);
	return target;
}

double calcRange(GEOLOCATE_REC *g,double look,double yaw);

/**
void getLookYaw(GEOLOCATE_REC *g,double range,double dop,
			double *out_look,double *out_yaw)
**/
/**
   -- function was moved to src/asf_meta/getLoc.c
**/

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
/**
   -- function moved to src/asf_meta/getLoc.c
**/


/*
double calcRange(GEOLOCATE_REC *g, double look, double yaw);
	Calculates the slant range, at the given look and yaw angle,
	from the given satellite's position to the earth's surface.
*/
/**
   -- function was moved to src/asf_meta/getLoc.c
**/

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

/**
   -- function was moved to src/asf_meta/getLoc.c
**/
