/******************************************************************************
NAME: cal_ssar

SYNOPSIS: calculates a look angle given a lat,lon for a geocoded
scansar image.  Uses a simple, 0 degree squint angle.


DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:
BUGS:

******************************************************************************/
#include "asf.h"
 

 
#include "asf_meta.h"
#include "jpl_proj.h"

/* TOOL PARAMETERS*/
#define time_err 0.00001  	/* error bound for position (in seconds) */
#define SQR(x) ((x)*(x))



/* FUNCTION DECLARARIONS */
double fd(meta_parameters *sar,stateVector sat,stateVector target);
double time_of_radar(meta_parameters *sar,double t_guess,vector targ);
stateVector get_tv(double RE,double RP,double lat,double lon);

double angle_anb(vector a,vector b);
double dist(vector a, vector b);



/*Convert given latitude and longitude to time, slant
range, and doppler, using state vectors.*/
void latLon2timeSlant(meta_parameters *sar,
	double lat,double lon,
	double *time,double *slant,double *dop)
{
	double t,r,dt=1.0;
	stateVector sat,targ;
	int    iterations = 0;
	double RE,RP;/*Radius of earth at equator and poles*/
	
	if (sar->geo->type=='P')
	{/*Map projected image has earth polar and equatorial radii*/
		RE=sar->geo->proj->re_major;
		RP=sar->geo->proj->re_minor;
	} else {/*Use WGS-84 ellipsoid*/
		RE=6378137.0;
		RP=6356752.314;
	}
	targ=get_tv(RE,RP,lat,lon); /* get target state vector*/
	t = 0.0;
	while ((fabs(dt) > time_err)&&(iterations<40))
	{
		dt=time_of_radar(sar,t,targ.pos);
		t += dt;
		iterations ++;
	}
	sat=meta_get_stVec(sar,t);
	r = dist(sat.pos,targ.pos);/*r: slant range to target.*/
	vecSub(targ.pos,sat.pos,&targ.pos);
	*time=t;
	*slant=r;
	if (dop!=NULL)
		*dop=fd(sar,sat,targ);
}

/*fd returns the doppler freqency, in Hz, between the
given satellite and the given point on the ground.*/
double fd(meta_parameters *sar,stateVector st,stateVector targ)
{
	vector relPos,relVel;
	vecSub(st.vel,targ.vel,&relVel);
	vecSub(st.pos,targ.pos,&relPos);
	return -2*vecDot(relVel,relPos)/
		(sar->geo->wavelen*vecMagnitude(relPos));
}
/*Time_of_radar refines an estimate of the time 
the given target was in the SAR's field of view.
It returns the difference between the new time and
the old time.*/
double time_of_radar(meta_parameters *sar,double t_guess,vector targ)
{
	stateVector sat;
	vector targ2sat;
	double vs, d_dis, ang;
	
	sat=meta_get_stVec(sar,t_guess);
	vecSub(sat.pos,targ,&targ2sat);
	ang = angle_anb(sat.vel,targ2sat);
	d_dis = vecMagnitude(targ2sat)*cosd(ang);
	vs = vecMagnitude(sat.vel);
	return -d_dis/vs;
}

/*Get_tv returns a "target state vector", 
containing the position and velocity of the 
given point on the earth's surface.*/
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

/* Calculate angle between A and B, in degrees.*/
double angle_anb(vector a,vector b)
{
  double ang;
  ang = acos(vecDot(a,b)/(vecMagnitude(a)*vecMagnitude(b)))*R2D;
  if(ang<0.0) ang = 180.0+ang;
  return(ang);
}

/*Calculate the distance between A and B*/
double dist(vector a, vector b)
{
	vector c;
	vecSub(a,b,&c);
	return vecMagnitude(c);
}
