/******************************************************************************
NAME:cal_ssar

SYNOPSIS:calculates a look angle based on a lat,lon for a scansar
image.  Uses the actual image doppler frequency to determine this.

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
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h> 
#include "ceos.h"
#include "const.h"
#include "geolocate.h"


#define sind(x) sin((x)*dtr)
#define cosd(x) cos((x)*dtr)
#define tand(x) tan((x)*dtr)
#define asind(x) (asin(x)*rtd)
#define acosd(x) (acos(x)*rtd)
#define atand(x) (atan(x)*rtd)
#define atan2d(y,x) (atan2(y,x)*rtd)

/* TOOL GLOBAL VARIABLES */
char *ceosName;
double t0;      	/* time associate w. initial state tor */
double h_mean = 0.0;  	/* Mean local elevation above ellipsoid */
double REL;     	/* Modified RE for reason of h_mean */
double RPL;     	/* Modified RP for reason of h_mean */
extern double dopplerChange; /*Doppler difference, Hz. */
double fd0;     	/* Doppler centroid coefficient, 0th order */
double fd1;     	/* Doppler centroid coefficient, 1st order */
double fd2;     	/* Doppler centroid coefficient, 2nd order */
double fd3;     	/* Doppler centroid coefficient, 3rd order */
double r0;		/* Slant Range to the First Image Pixel    */
double delr;		/* Slant Range Pixel Spacing	           */
double gha;
double da_err = 0.0001;  	/* error bound of A/T position (in seconds) */

/* PROJECTION GLOBAL VARIABLES */
double slat  = 70.0;    /* reference latitude for polar stereo */
double alon0;           /* reference longitude for polar stereo */
double plat1 = 15.0;    /* the first standard arallel for Lambert */
double plat2 = 1.0;     /* the second standard arallel for Lambert */
double lat0  = 1.0;     /* original lat for Lambert */
double lon0  = 0.0;     /* original long for Lambert */
char   proj_id[15];     /* the id number of the projection used */
int    zone;            /* the UTM zone number used */
char   hem;             /* sourth or north hemsphere */
double rlocal;

/* FUNCTION DECLARARIONS */
void proj_to_ll(double p1,double p2,double *lat,double *lat_d,double *lon);

double fd(stateVector sat,stateVector target);
double time_of_radar(vector targ);
double angle_anb(vector a,vector b);
double dist(vector a, vector b);
double dist(vector a, vector b)
{
	vector c;
	vecSub(a,b,&c);
	return vecMagnitude(c);
}
void get_tv(double lat, double lon, double lat_d,stateVector *v);
void ephemeris(double t, stateVector *v);

/*Convert latitude and longitude based on doppler change.*/
void convertL_for_doppler(double lat_d,double lon,double *lat_out,double *lon_out)
{
	double t,r,look_ang=0,dt=1.0;
	double earthRadius,fd_r;
	stateVector targ;
	stateVector sat;
	int    kcount = 0;
	lon*=deg2rad;
	lat_d*=deg2rad;
	get_tv(0,lon,lat_d,&targ); /* get target state vector*/
	t = time_of_radar(targ.pos); /* Estimate of the time when
	                      target perpendicular to satellite flight path. */
	while ((fabs(dt) > da_err)&&(kcount<20))
	{
		double fd_t,vst;
		ephemeris(t,&sat);
		r = dist(sat.pos,targ.pos);/*r: slant range to target.*/
		fd_t = fd(sat,targ); /* This position's doppler freq. */
		fd_r = fd0 + fd1*((r-r0)/delr) + fd2*SQR((r-r0)/delr); /*Ceos doppler freqency.*/
		vst = dist(sat.vel,targ.vel); /* vst: sensor-target relative velocity */
		dt = (r * ((fd_t - fd_r)*lambda/(2*vst)))/vst; /* dt: time correction factor  */
		t = t + dt;
		kcount = kcount + 1;
	}
	getLatLongGHA(sat,r,fd_r+dopplerChange,0.0,0.0,lat_out,lon_out,&earthRadius);
}

/* Convert projection coordinates based on doppler change. */
void convert_for_doppler(double p1,double p2,double *o1,double *o2)
{
	double lat, lon, lat_d=0;
	
	proj_to_ll(p1,p2,&lat,&lat_d,&lon);
	/*printf("pdx= %.0lf,  pd_y= %.0lf,  lat= %lf,  lon= %lf\n",p1,p2,lat_d,lon);*/
	
	convertL_for_doppler(lat_d,lon,&lat_d,&lon);
	
	ll_to_proj(lat_d,lon,o1,o2);
}

void ephemeris(double t, stateVector *v)
{
  double delta_t;
  double new_gha;

  delta_t = t - t0;
  get_ceos_stVec(ceosName,delta_t,v);

  new_gha = gha + WE*delta_t;
  gei2fixed(v,new_gha);
}

double fd(stateVector sat,stateVector targ)
{
	vector relPos,relVel;
	vecSub(sat.vel,targ.vel,&relVel);
	vecSub(sat.pos,targ.pos,&relPos);
	return -2*vecDot(relVel,relPos)/(lambda*vecMagnitude(relPos));
}

double time_of_radar(vector targ)
{
  double t;
  stateVector sat;
  vector targ2sat;
  double vs, d_dis, ang;

  t = 0.0;
  ephemeris(t,&sat);  /* state vector at time of t */
  vecSub(sat.pos,targ,&targ2sat);
  ang = angle_anb(sat.vel,targ);
  d_dis = vecMagnitude(targ2sat)*cos(ang*deg2rad);
  vs = vecMagnitude(sat.vel);
  if (ang>90.0) t = t+d_dis/vs;
  else t = t-d_dis/vs;

  return(t);
}

void get_tv(double lat, double lon, double lat_d,stateVector *v)
{
  double Rn;

  Rn = RE/sqrt(1-(SQR(ecc_e*sin(lat_d))));
  v->pos.x = (Rn+h_mean)*cos(lon)*cos(lat_d);
  v->pos.y = (Rn+h_mean)*sin(lon)*cos(lat_d);
  v->pos.z = (Rn*(1-ecc2)+h_mean)*sin(lat_d);
  /*fixed2gei with gha 0.0 simply sets the velocity
  of the target point to the rotation rate of the earth.*/
  v->vel.x=v->vel.y=v->vel.z=0.0;
  fixed2gei(v,0.0);
}

/* Calculate angle between A and B */
double angle_anb(vector a,vector b)
{
  double ang;
  ang = acos(vecDot(a,b)/(vecMagnitude(a)*vecMagnitude(b)))/deg2rad;
  if(ang<0.0) ang = 180.0+ang;
  return(ang);
}

void ssar_tool_init(char *inSAR, char *projection);
void ssar_tool_init(char *inSAR, char *projection)
{
  double Rn, tmp;
  double lat_d, lon, lat;
  struct VFDRECV facdr;
  struct pos_data_rec ppdr;

	ceosName=inSAR;
  strcpy(proj_id,projection);
  printf(" projection is %s\n",proj_id);

  get_facdr(inSAR,&facdr);
  get_ppdr(inSAR,&ppdr);
  t0   = 0.0;
  gha =0.0;

  lat = facdr.imgclat; lon = facdr.imgclon;
  
  if (lat > 0.0) hem='n'; else hem='s';
  lat_d = atand(tand(lat)/(1-ecc2));

  tmp = sqrt(1- SQR(ecc_e*sind(lat_d)));
  Rn  = RE / tmp; 
  REL = (Rn+h_mean)*tmp;
  RPL = RP/RE*REL;
  rlocal = RE*RP/sqrt(SQR(RP*cosd(lat))+SQR(RE*sind(lat)));

  dopplerChange*=facdr.prfreq;

  fd0 = facdr.dpplrfrq;          /* Doppler coefficient, 0th order */
  fd1 = facdr.dpplrslp;          /* Doppler coefficient, 1st order */
  fd2 = facdr.dpplrqdr;          /* Doppler coefficient, 2nd order */
  fd3 = 0.0;                     /* Doppler coefficient, 3rd order */
  r0  = facdr.sltrngfp*1000.0;		 /* Slant Range to the First Pixel */
  delr = 55;		 /* Slant Range Pixel Spacing    */

  if (strcmp(proj_id,"utm") == 0) zone = zone_det(lon);
  else if (strcmp(proj_id,"ATCT")==0) alpha123(ppdr.pos_vec[0][0]*1000.0,
						  ppdr.pos_vec[0][1]*1000.0,
						  ppdr.pos_vec[0][2]*1000.0,
						  ppdr.pos_vec[0][3],
						  ppdr.pos_vec[0][4],
						  ppdr.pos_vec[0][5]);
  
}
