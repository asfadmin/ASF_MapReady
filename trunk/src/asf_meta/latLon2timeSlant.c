#include <assert.h>
#include "asf.h"
#include "asf_meta.h"
#include "jpl_proj.h"

/* TOOL PARAMETERS*/
#define time_err 0.00001  	/* error bound for position (in seconds) */
#define SQR(x) ((x)*(x))

/* FUNCTION DECLARARIONS */
double fd(meta_parameters *meta,stateVector sat,stateVector target);
double time_of_radar(meta_parameters *meta,double t_guess,vector targ);
stateVector get_tv(double RE,double RP,double lat,double lon);
double angle_anb(vector a,vector b);
double dist(vector a, vector b);


/******************************************************
 * latLon2timeSlant:
 * Convert given latitude and longitude to time, slant
 * range, and doppler, using state vectors.*/
void latLon2timeSlant(meta_parameters *meta,
	double lat,double lon,
	double *time,double *slant,double *dop)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double t,r,dt=1.0;
	stateVector sat,targ;
	int    iterations = 0;
	double RE,RP;/*Radius of earth at equator and poles*/
	
	if (meta->sar->image_type=='P')
	{/*Map projected image has earth polar and equatorial radii*/
		RE=meta->projection->re_major;
		RP=meta->projection->re_minor;
	} else {/*Use WGS-84 ellipsoid*/
		RE=6378137.0;
		RP=6356752.314;
	}
	targ=get_tv(RE,RP,lat,lon); /* get target state vector*/
	t = 0.0;
	while ((fabs(dt) > time_err)&&(iterations<40))
	{
		dt=time_of_radar(meta,t,targ.pos);
		t += dt;
		iterations ++;
	}
	sat=meta_get_stVec(meta,t);
	r = dist(sat.pos,targ.pos);/*r: slant range to target.*/
	vecSub(targ.pos,sat.pos,&targ.pos);
	*time=t;
	*slant=r;
	if (dop!=NULL)
		*dop=fd(meta,sat,targ);
}

/******************************************************
 * fd:
 * Returns the doppler freqency, in Hz, between the
 * given satellite and the given point on the ground.*/
double fd(meta_parameters *meta,stateVector st,stateVector targ)
{
	vector relPos,relVel;
	vecSub(st.vel,targ.vel,&relVel);
	vecSub(st.pos,targ.pos,&relPos);
	return -2*vecDot(relVel,relPos)/
		(meta->sar->wavelength*vecMagnitude(relPos));
}

/******************************************************
 * time_of_radar:
 * Refines an estimate of the time the given target was
 * in the SAR's field of view.  It returns the difference
 * between the new time and the old time.*/
double time_of_radar(meta_parameters *meta,double t_guess,vector targ)
{
	stateVector sat;
	vector targ2sat;
	double vs, d_dis, ang;
	
	sat=meta_get_stVec(meta,t_guess);
	vecSub(sat.pos,targ,&targ2sat);
	ang = angle_anb(sat.vel,targ2sat);
	d_dis = vecMagnitude(targ2sat)*cosd(ang);
	vs = vecMagnitude(sat.vel);
	return -d_dis/vs;
}

/******************************************************
 * get_tv:
 * Returns a "target state vector", containing the
 * position and velocity of the given point on the
 * earth's surface.*/
stateVector get_tv(double RE,double RP,double lat,double lon)
{
  double Rn;
  stateVector v;
  double ecc2=1-RP*RP/(RE*RE);

  Rn = RE/sqrt(1-ecc2*SQR(sind(lat)));
  v.pos.x = (Rn)*cosd(lon)*cosd(lat);
  v.pos.y = (Rn)*sind(lon)*cosd(lat);
  v.pos.z = (Rn*(1-ecc2))*sind(lat);
  /*fixed2gei with gha 0.0 simply sets the velocity
  of the target point to the rotation rate of the earth.*/
  v.vel.x=v.vel.y=v.vel.z=0.0;
  fixed2gei(&v,0.0);
  return v;
}

/******************************************************
 * angle_anb:
 * Calculate angle between A and B, in degrees.*/
double angle_anb(vector a,vector b)
{
  double ang;
  ang = acos(vecDot(a,b)/(vecMagnitude(a)*vecMagnitude(b)))*R2D;
  if(ang<0.0) ang = 180.0+ang;
  return(ang);
}

/******************************************************
 * dist:
 * Calculate the distance between A and B */
double dist(vector a, vector b)
{
	vector c;
	vecSub(a,b,&c);
	return vecMagnitude(c);
}
