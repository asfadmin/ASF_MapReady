/******************************************************************************
NAME:  ssar_proj_tools - SCANSAR map projection transformation tools


These Routines are the external entry points:
---------------------------------------------
ssar_tool_init  -  Initialize projection globals
ll_to_proj      -  Convert lat,lon to projection
proj_to_ll	-  Convert projectio to lat,lon

These Routines are meant to be called internally only
-----------------------------------------------------
lambert_ll	- lambert to lat,lon
ll_lambert	- lat,lon to lambert
ssmill		- polar stereo to lat,lon
llssmi		- lat,lon to polar stereo
utm_ll		- UTM to lat,lon
ll_utm		- lat,lont to UTM
zone_det	- determine UTM zone number

These routines are used internally for AT/CT coordinates:
---------------------------------------------------------
alpha123        - determines alpha angles for coordinate x-forms
rotate_z	- perform a rotation about the z axis
rotate_y	- perform a rotation about the y axis
ll_to_ebf	- convert lat,lon to psuedo-earth body fixed
ebf_to_ll	- convert psuedo-earth body fixed to lat,lon
ac_ll		- convert AT/CT to lat,lon
ll_ac		- convert lat,lon to AT/CT

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:  Original code from Michael Jin @ JPL

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h> 
#include "ceos.h"
#include "const.h"
#include "asf_meta.h"
#include "geolocate.h"

#define mag(x,y,z) sqrt(SQR(x)+SQR(y)+SQR(z))

/* TOOL GLOBAL VARIABLES */
double proj_x, proj_y, pd_x, pd_y;

/* PROJECTION GLOBAL VARIABLES */
double slat  = 70.0;    /* reference latitude for polar stereo */
double alon0;           /* reference longitude for polar stereo */
double plat1 = 15.0;    /* the first standard arallel for Lambert */
double plat2 = 1.0;     /* the second standard arallel for Lambert */
double lat0  = 1.0;     /* original lat for Lambert */
double lon0  = 0.0;     /* original int for Lambert */
char   proj_id[15];     /* the id number of the projection used */
int    zone;            /* the UTM zone number used */
char   hem;             /* sourth or north hemsphere */
double rlocal;

/*Prototypes:*/
int zone_det(double degs);
void alpha123(double x,double y,double z,double vx,double vy,double vz);
void ssar_tool_init(char *inSAR, int nl,int ns);
void lambert_ll(double x,double y,double *lat,double *lat_d,double *lon);
void ll_lambert(double lat,double lon,double *x,double *y);
void utm_ll(double x,double y,double *lat,double *lat_d,double *lon);
void ll_utm(double tlat, double tlon, double *op1, double *op2);
void ssmill(double xx,double yy,double *alat,double *alon);
void llssmi(double lat, double lon, double *x, double *y);
void ac_ll(double c1, double c2, double *lat, double *lat_d, double *lon);
void ll_ac(double lat, double lon, double *c1, double *c2);
void rotate_z(double *x0,double *y0,double *z0,double theta0,double sign);
void rotate_y(double *x0, double *y0, double *z0,double theta0, double sign);
void ll_to_ebf(double lat, double lon,double *x,double *y,double *z);
void ebf_to_ll(double x,double y,double z,double *lat,double *lat_d,double *lon);


/******************************************************************************/

void ssar_tool_init(char *inSAR, int nl,int ns)
{
  /*double Rn, tmp;*/
  double /*lat_d, lon,*/ lat;
  struct VFDRECV facdr;
  struct pos_data_rec ppdr;
  struct VMPDREC mpdr; 
  float version; 
  double gha;

  get_facdr(inSAR,&facdr);
  get_ppdr(inSAR,&ppdr);                
  get_mpdr(inSAR,&mpdr);

  if (strncmp(mpdr.mpdesig, "PS-SMM/I", 8) == 0) {
	strcpy(proj_id, "ssmi");
  } else if (strncmp(mpdr.mpdesig, "UTM", 3) == 0) {
	strcpy(proj_id, "utm");
  } else if (strncmp(mpdr.mpdesig, "LAMBERT", 7) == 0) {
	strcpy(proj_id, "lambert");
	printf("WARNING: Lambert-geocoded images may not be\n"
		"accurately geocoded!\n");
  } else if (strncmp(mpdr.mpdesig, "GROUND RANGE",12) == 0) {
	strcpy(proj_id, "atct");
  } else { 
	printf("Can not match projection\n\n"); 
	exit(1); 
  }

  /* take care of different version of SWB state vectors 
   ----------------------------------------------------*/
  sscanf(&(facdr.procvers[4]),"%f",&version);
  /*printf("Version of Scansar is %f\n",version);*/
  if (version < (float)2.51) { gha = 0.0; }
  else { gha = ppdr.hr_angle; }

  lat = facdr.imgclat; /*lon = facdr.imgclon;*/
  
  if (lat > 0.0) hem='n'; else hem='s';
  /*lat_d = atand(tand(lat)/(1-ecc2));*/
  
  slat=mpdr.upslat;
  alon0=mpdr.upslong;
  plat1=mpdr.nsppara1;
  plat2=mpdr.nsppara2;
  lat0=mpdr.blclat+0.023;
  lon0=mpdr.blclong+2.46;
  zone=zone_det(mpdr.utmpara1);

  /*tmp = sqrt(1- SQR(ecc_e*sind(lat_d)));*/
  /*Rn  = RE / tmp;*/ 
  rlocal = RE*RP/sqrt(SQR(RP*cosd(lat))+SQR(RE*sind(lat)));

  if (strcmp(proj_id,"atct")==0) 
  {
  	stateVector st;
  	st.pos.x=ppdr.pos_vec[0][0]*1000.0;
  	st.pos.y=ppdr.pos_vec[0][1]*1000.0;
  	st.pos.z=ppdr.pos_vec[0][2]*1000.0;
  	st.vel.x=ppdr.pos_vec[0][3];
  	st.vel.y=ppdr.pos_vec[0][4];
  	st.vel.z=ppdr.pos_vec[0][5];

        /* Convert to fixed-earth state vector */
	if (version >= (float)2.51) 
	{
  	  gei2fixed(&st,gha);
	  fixed2gei(&st,0.0);/*Remove earth's spin.  
			The AT/CT projection requires this.*/
	}

  	alpha123(st.pos.x,st.pos.y,st.pos.z, st.vel.x,st.vel.y,st.vel.z);
	proj_y = mpdr.tlceast;
	proj_x = mpdr.tlcnorth;
	pd_y   = (mpdr.blceast-mpdr.tlceast)/nl;
	pd_x   = (mpdr.trcnorth-mpdr.tlcnorth)/ns;
  } else {
	proj_y = mpdr.tlcnorth;
	proj_x = mpdr.tlceast;
	pd_y   = (mpdr.blcnorth-mpdr.tlcnorth)/nl;
	pd_x   = (mpdr.trceast-mpdr.tlceast)/ns;
  }
}

/*****************************************************************************/

void cross(double x1, double y1, double z1, double x2, double y2, double z2,
      double *u1, double *u2, double *u3)
{
  double r, t1, t2, t3;
  t1=y1*z2-z1*y2; t2=z1*x2-x1*z2; t3=x1*y2-y1*x2;
  r=sqrt(SQR(t1)+SQR(t2)+SQR(t3));
  *u1=t1/r; *u2=t2/r; *u3=t3/r;
}

/******************************************************************************/

void ll_to_proj(double lat_d,double lon,double *op1,double *op2)
{
  if (strcmp(proj_id,"utm")==0) 	 ll_utm(lat_d,lon,op1,op2);
  else if (strcmp(proj_id,"lambert")==0) ll_lambert(lat_d,lon,op1,op2);
  else if (strcmp(proj_id,"ps")==0||strcmp(proj_id,"ssmi")==0)
        llssmi(lat_d,lon,op1,op2);
  else if (strcmp(proj_id,"atct")==0)    ll_ac(lat_d,lon,op2,op1);
  else {
    printf("Unrecognized map projection '%s' passed to ll_to_proj!\n",proj_id);
    exit(1);
  }
}

/******************************************************************************/

void proj_to_ll(double p1, double p2, double *lat, double *lat_d, double *lon)
{
  if (strcmp(proj_id,"utm")==0) 	 utm_ll(p1,p2,lat,lat_d,lon);
  else if (strcmp(proj_id,"lambert")==0) lambert_ll(p1,p2,lat,lat_d,lon);
  else if (strcmp(proj_id,"ps")==0||strcmp(proj_id,"ssmi")==0)
    {
      ssmill(p1,p2,lat_d,lon);
      *lat = atand(tand((*lat_d))*(1.0-ecc2));
    }
  else if (strcmp(proj_id,"atct")==0)
  	ac_ll(p2,p1,lat,lat_d,lon);
  else {
    printf("Unrecognized map projection '%s' passed to proj_to_ll!\n",proj_id);
    exit(1);
  }
}

/***********************************************************************
  Module Name: lambert_ll
 
  Abstract: convert Lambert Conformal Conic(Ellipsoid) coordinates to lat/lon
 
  User-defined Input parameters:
          1).tlat  - array of latitude coordinates
          2).tlon  - array of longitude coordinates
          3).plat1 - the first standard parallels
          4).plat2 - the second standard parallels
          5).lat0  - original lat
          6).lon0 - original long
 
  Output parameters:
          1).x - array of easting coordinates
          2).y - array of northing coordinates
 
  Reference: Synder, J. P. 1984, Map projection used by the US
             geological survey(bulletin 1532)
 
***********************************************************************/
void lambert_ll(double x,double y,double *lat,double *lat_d,double *lon)
{
  double a = 6378206.4; /* mean equatorial radius in meters */
  double e = 0.0822719; /* earth eccentricity */
  double esq = ecc2;    /* read from key_const.inc */
  double m1, m2, t1, t2, t0;
  double n, F, rho_0, t, rho, theta, chi;

  m1 = cosd(plat1)/sqrt(1.0 - SQR(e*sind(plat1)));
  m2 = cosd(plat2)/sqrt(1.0 - SQR(e*sind(plat2)));

 t1=tand(45.0-plat1/2.0)/pow(((1.0-e*sind(plat1))/(1.0+e*sind(plat1))),(e/2.0));
 t2=tand(45.0-plat2/2.0)/pow(((1.0-e*sind(plat2))/(1.0+e*sind(plat2))),(e/2.0));
 t0=tand(45.0- lat0/2.0)/pow(((1.0-e*sind( lat0))/(1.0+e*sind( lat0))),(e/2.0));

  n = log(m1/m2) / log(t1/t2);
  F = m1 / ( n * pow(t1,n) );
  rho_0 = a * F * pow(t0,n);
  rho = sqrt(x*x + SQR(rho_0-y));

  /* If n is negative, the signs of x,y and rho_0 must be reversed
    (see notes in "Map projections used by the USGS) */
  theta = atand(x/(rho_0-y));
  *lon   = theta / n + lon0;
  t = pow((rho/(a*F)),(1.0/n));
 
  /* use a series to calculate the inverse formula */
  chi = 90.0 - 2.0*atand(t);
  chi = chi*M_PI/180.0;   	/* convert to radian */
  *lat_d = chi
            +  (pow(e,2.0)/2.0 + 5.0 * pow(e,4.0)/24.0
            +  pow(e,6.0)/12.0)*sin(2.0*chi)
            +  (7.0*pow(e,4.0)/48.0 +
            +  29.0*pow(e,6.0)/240.0)*sin(4.0*chi)
            +  (7.0*pow(e,6.0)/120.0)*sin(6.0*chi);

  *lat_d = (*lat_d) * 180.0/M_PI;	/* convert to degrees */  
  *lat  = atand((1-esq)*tand((*lat_d)));
}
/******************************************************************************/

void utm_ll(double x,double y,double *lat,double *lat_d,double *lon)
{
  double a   = RE;          /* Earth equatorial radius */
  double esq = ecc2;        /* Earth eccentricity squared */
  double k0  = 0.9996;      /* Center meridian scale factor */
  double u1, u2, u3, lat1, esqsin2, lat1d, long0;
  double rm, e1, u, epsq, t1, c1, rn1, r1, rm0;
  double tanlat1, d;
  double xadj = 500000.0, yadj;    /* Northing origin:(5.d5,0.d0) */
                                   /* Southing origin:(5.d5,1.d7) */
  rm0 = 0.0;
  if (hem=='n') 
  	yadj=0.0; 
  else 
  	yadj=1.0E7; /* Ref pt Southern Hemisphere */
  long0 = (double)(zone)*6.0-183.0;     /* zone from proj_const.inc   */

  rm = (y - yadj)/k0 + rm0;
  e1 = (1.0 - sqrt(1.0 - esq))/(1.0 + sqrt(1.0 - esq));
  u = rm/(a*(1.0-esq/4.0-(3.0*esq*esq/64.0) - (5.0*esq*esq*esq/256.0)));
  u1 = (3.0 * e1 / 2.0 - (27.0 * e1*e1*e1)/32.0) * sin(2.0*u);
  u2 = (21.0 * (e1*e1)/16.0 - (55.0 * e1*e1*e1*e1)/32.0) * sin(4.0*u);
  u3 = (151.0 * (e1*e1*e1) / 96.0) * sin(6.0*u);
  lat1 = u + u1 + u2 + u3;
  lat1d = lat1/dtr;

  esqsin2 = 1.0 - esq*SQR(sin(lat1));
  epsq = esq/(1.0 - esq);
  c1 = epsq * SQR(cos(lat1));
  tanlat1 = sin(lat1)/cos(lat1);
  t1 = tanlat1 * tanlat1;
  rn1 = a/sqrt(esqsin2);
  r1 = a*(1.0 - esq)/sqrt(esqsin2 * esqsin2 * esqsin2);
  d = (x - xadj)/(rn1 * k0);

  *lat_d = lat1d - ((rn1 * tanlat1/r1) * (d*d*0.50
                  - (5.0 + 3.0*t1 - 10.0*c1 + 4.0*c1*c1
                          - 9.0*epsq) * (d*d*d*d)/24.0
                  + (61.0 + 90.0*t1 + 298.0*c1 + 45.0*t1*t1
                           - 252.0*epsq - 3.0*c1*c1)
                      *(d*d*d*d*d*d)/720.0) )/dtr;

  *lat = atand((1-esq)*tand(*lat_d));

  *lon = long0 + ((1.0/cos(lat1)) * (d
          - (1.0 + 2.0*t1 + c1) * (d*d*d)/6.0
          + (5.0 - 2.0*c1 + 28.0*t1 - 3.0*c1*c1 + 8.0*epsq + 24.0*t1*t1)
                 *(d*d*d*d*d)/120.0) )/dtr;
}

/*****************************************************************************
  Description:
 
  This subroutine converts from polar stereographic (x,y) coordinates
  to geodetic latitude and longitude for the polar regions. The standard
  parallel (latitude with no distortion) is +/- 70 degrees. The equations
  are from Synder, J. P., 1982, map projections used by the U.S.
  geological survey, geological survey bulletin 1532, U.S. goverment
  printing office. See JPL technical memorandum 3349-85-101 for further
  details.
 
  Arguments:

  Variable    Type       I/O    Description
  xx,yy       real*8      I     polar sterographic x,y coordinate (m)
                                                                     ssp 8/89
  alat        real*8      O     geodetic latitude (degrees, +90 to -90)
  alon        real*8      O     geodetic longitude (degrees, 0 to 360)

  Written by C. S. Morris - April 29, 1985
******************************************************************************/

void ssmill(double xx,double yy,double *alat,double *alon)
{
  double re=6378.273;     /* radius of earth = 6378.273 km (huges ellipsoid) */
  double e2=0.006693883;  /* eccentricity of the earth (huges ellipsoid) */
  double e,xlam;
  double sn,rho,cm,t,chi,xpr,ypr,x,y,slatr;

  e=sqrt(e2);      
  slatr=slat*dtr;
  if (slat>=0.0) { sn=1.0; xlam=-45.0; } else { sn=-1.0; xlam=0.0;  }
  
  slatr*=sn;/*Make South Hemisphere work-- O. Lawlor*/

  /* Compute the latitude and longitude */
  x=xx*1.0E-3; y=yy*1.0E-3;
  rho=sqrt(x*x+y*y);
  if (rho <= 0.0001) { *alat = 90.0; if (sn < 0) *alat = -90.0; *alon = 0.0; }
  else {
    cm = cos(slatr)/sqrt(1.0-e2*(SQR(sin(slatr))));
    t = tan((M_PI/4.0)-(slatr/(2.0)))/
	   pow(((1.0-e*sin(slatr))/(1.0+e*sin(slatr))),(e/2.0));
    t = rho*t/(re*cm);
    chi=(M_PI/2.0)-2.0*atan(t);
 
    *alat = chi + ((e2/2.0)+(5.0*e2*e2/24.0)+
                   (e2*e2*e2/12.0))*sin(2.0*chi)+
          ((7.0*e2*e2/48.0)+(29.0*e2*e2*e2/240.0))*
          sin(4.0*chi)+(7.0*e2*e2*e2/120.0)*sin(6.0*chi);

    *alat = sn * (*alat) * rtd;
    xpr = sn*x;
    ypr = sn*y;
    *alon = atan2(xpr,-ypr)*rtd+sn*xlam;
    *alon = sn * (*alon);

    if (*alon <= -180.0) *alon = (*alon)+360.0;  /* ssp 8/89 */
    if (*alon <     0.0) *alon = (*alon)+360.0;  /* ssp 8/89 */
    if (*alon >   360.0) *alon = (*alon)-360.0;  /* ssp 8/89 */
    /* 10/16/96 Modify the convention of longitude to be between -180 and 180 */
    if (*alon >   180.0) *alon = (*alon)-360.0;
   }
}
 
/*********************************************************************
  Description:
 
  This subroutine converts from geodetic latitude and longitude to
  polar stereographic (x,y) coordinates for the polar regions. The
  standard parallel (latitude with no distortion) is +/- 70 degrees.
  The equations are from Synder, J. P., 1982, map projections used
  by the U.S. geological survey, geological survey bulletin 1532,
  U.S. government printing office. see jpl technical memorandum
  3349-85-101 for further details.
 
  Variable  Type          I/O     Descritpion
  --------------------------------------------------------------------------
  alat     double          I      geodetic latitude(degrees, +90 to -90)
  alon     double          I      geodetic longitude(degrees, 0 to 360)
  x        double          O      polar stereographic x coordinate(km)
  y        double          O      polar stereographic y coordinate(km)
 
  re       double          I      radius of the earth (km)
  e2       double          I      eccentricity square (m)
  slat     double          I      standard parallel
  xlam     double          I      meridian along positive y-axis
 
  Written by C. S. Morris - April 29, 1985
  modified for ers-1 project- genreate the xy table
  instead of one (x,y) per call to the subroutine.    ssp  Jan 1989.
  add re, e2, slat and xlam to the calling sequence.  ssp  Mar 1989.
  convert to c					      ASF  1/98
*****************************************************************************/
void llssmi(double lat, double lon, double *x, double *y)
{
  double re=6378.273;     /* radius of earth = 6378.273 km (huges ellipsoid) */
  double e2=0.006693883;  /* eccentricity of the earth (huges ellipsoid) */
  double ta[2];
  double sn,e,xlam,rlat,cm,rho;
  double alon, alat;
  int    i;

  /* Set constants for ssm/i grid */
  e=sqrt(e2);
  if (lon <= 0) alon = lon + 360; else alon = lon;
  if (lat>=0.0) { sn=1.0; xlam=-45.0; } else { sn=-1.0; xlam=0.0;  }

  /* Compute x and y in grid coordinates */
  alat=sn*lat; alon=sn*alon; rlat=alat;
  for (i=0; i<2; i++)
   { if (i==1) rlat=slat;
     ta[i]= tan((M_PI/4.0)-(rlat/(2.0*rtd)))/
 		pow (((1.0-e*sin(rlat*dtr))/(1.0+e*sin(rlat*dtr))),(e/2.0));
   }
  cm=cos(slat*dtr)/sqrt(1.0-e2*(SQR(sin(slat*dtr))));
  rho=re*cm*ta[0]/ta[1];

  *x=(rho*sn*sin((alon-xlam)*dtr))*1000.0;
  *y=-(rho*sn*cos((alon-xlam)*dtr))*1000.0;
}
/******************************************************************************/

void ll_utm(double tlat, double tlon, double *op1, double *op2)
{
  double   k0 = 0.9996;       /* Center meridian scale factor */
  double   epsq, lat, /*lon,*/ lon0, a1, a2, a3, rn;
  double   t, b1, b2, rm, rm0;
  double   tanlat, c;
  double   yadj, xadj = 500000.0;
  double   e4, e6, c1, c2, c3, c4;
  double   x,y;

  epsq = ecc2/(1.0 - ecc2);
  lon0 = (double)(zone-1)*6.0 + 3.0 - 180.0;   /* zone from proj constants */
  if (tlat>=0.0) yadj = 0.0; else yadj = 1.0E7; /* Ref pt Southern Hemisphere */
  
  lat = tlat * dtr; /*lon = tlon * dtr;*/

  rn =  RE / sqrt(1.0 - ecc2*SQR(sin(lat)));
  tanlat = tan(lat);
  t = SQR(tanlat);
  c = epsq * SQR(cos(lat));
  a1 = cos(lat) * ((tlon-lon0)*dtr);
  a2 = (1.0 - t + c) * (a1*a1*a1) / 6.0;
  a3 = (5.0 - 18.0*t + t*t + 72.0*c - 58.0*epsq) * ((a1*a1*a1*a1*a1) / 120.0);

  x = k0 * rn * (a1+a2+a3) + xadj;

  e4 = ecc2 * ecc2;
  e6 = e4 * ecc2;
  c1 = 1.0 - ecc2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0;
  c2 = 3.0*ecc2/8.0 + 3.0*e4/32.0 + 45.0*e6/1024.0;
  c3 = 15.0*e4/256.0 + 45.0*e6/1024.0;
  c4 = 35.0*e6/3072.0;
  rm = RE*(c1*lat-c2*sin(2.0*lat)+c3*sin(4.0*lat)- c4*sin(6.0*lat));
  rm0 = 0.0;
  b1 = (SQR(a1))/2.0 + (5.0 - t + 9.0*c + 4.0 *SQR(c)) * (a1*a1*a1*a1) / 24.0;
  b2 = (61.0-58.0*t+SQR(t)+600.0*c+330.0*epsq) * (a1*a1*a1*a1*a1*a1) / 720.0;

  y = k0 * (rm - rm0 + rn*tanlat*(b1 + b2)) + yadj;

  *op1 = x;
  *op2 = y;
}

/******************************************************************
  Module Name: ll_lambert
 
  Abstract:
            convert lat/lon to Lambert Conformal Conic (Ellipsoid)
	    coordinates (x:easting,y: northing)
 
  User-defined Input parameters:
          1).tlat  - array of latitude coordinates
          2).tlon  - array of longitude coordinates
          3).plat1 - the first standard parallels
          4).plat2 - the second standard parallels
          5).lat0  - original lat
          6).lon0 - original long
 
  Output parameters:
          1).x - array of easting coordinates
          2).y - array of northing coordinates
 
  Reference: Synder, J. P. 1984, Map projection used by the US
             geological survey(bulletin 1532)

 ******************************************************************/
void ll_lambert(double lat,double lon,double *x,double *y)
{
  double a = 6378206.4;  /* mean equatorial radius in meters */
  double e = 0.0822719;  /* earth eccentricity */
  double m1, m2, t1, t2, t0;
  double n, F, rho_0, t, rho, theta;

  m1 = cosd(plat1)/sqrt(1.0-SQR(e*sind(plat1)));
  m2 = cosd(plat2)/sqrt(1.0-SQR(e*sind(plat2)));

 t1=tand(45.0-plat1/2.0)/pow(((1.0-e*sind(plat1))/(1.0+e*sind(plat1))),(e/2.0));
 t2=tand(45.0-plat2/2.0)/pow(((1.0-e*sind(plat2))/(1.0+e*sind(plat2))),(e/2.0));
 t0=tand(45.0- lat0/2.0)/pow(((1.0-e*sind( lat0))/(1.0+e*sind( lat0))),(e/2.0));

  if (plat1!=plat2) n = log(m1/m2) / log(t1/t2); else n = sind(plat1);
  F = m1/(n*pow(t1,n)); rho_0 = a*F*pow(t0,n);

  t = tand(45.0-lat/2.0)/pow(((1.0-e*sind(lat))/(1.0+e*sind(lat))),(e/2.0));
  rho = a*F*pow(t,n);
  theta = n*(lon-lon0);

  *x = rho*sind(theta);
  *y = rho_0-rho*cosd(theta);
}

/******************************************************************************/
int zone_det(double lon) {return(((180+lon)/6+1));} 
/******************************************************************************/

static double alpha1, alpha2;
static double alpha3;

void alpha123(double x, double y, double z, double vx, double vy, double vz)
 {
  double z_orbit[3], y_axis[3], a[3];
  double nd1,nd2,nd3;
  double alpha3_sign;
  double r_mag;

  cross(x,y,z,vx,vy,vz,&z_orbit[0],&z_orbit[1],&z_orbit[2]);

  cross(z_orbit[0],z_orbit[1],z_orbit[2],0.0,0.0,1.0,
	&y_axis[0],&y_axis[1],&y_axis[2]);

  cross(y_axis[0],y_axis[1],y_axis[2],z_orbit[0],z_orbit[1],z_orbit[2],
	&a[0],&a[1],&a[2]);

  /*printf("ALPHA123 new x-vector: %e,%e,%e\n",a[0],a[1],a[2]);*/

  if (fabs(a[0])< 1.0e-10 && fabs(a[1])< 1.0e-10) alpha1 = 0.0;
  else alpha1 = atan2d(a[1],a[0]);
  alpha2 = -1.0 * asind(a[2]);
  if (z_orbit[2] < 0.0) 
     {
       alpha1 =  180.0 + alpha1;
       alpha2 = -1.0*(180.0-fabs(alpha2));
     }
  /* printf("alpha1,alpha2  %f %f\n",alpha1,alpha2);*/

  cross(a[0],a[1],a[2],x,y,z,&nd1,&nd2,&nd3);
  alpha3_sign = nd1*z_orbit[0]+nd2*z_orbit[1]+nd3*z_orbit[2];
  r_mag = mag(x,y,z);
  alpha3 = acosd((a[0]*x+a[1]*y+a[2]*z)/r_mag);
  if (alpha3_sign<0.0) alpha3 *= -1.0;
  /* printf("alpha3 %f\n",alpha3);*/

  return;
 }
/******************************************************************************/

void ac_ll(double c1, double c2, double *lat, double *lat_d, double *lon)
 {
   double qlat, qlon;
   double x, y, z;

   qlat = -1.0*(c2/rlocal)*rtd; /* RIGHT LOOKING */
   /* qlat = c2/rlocal*rtd;        LEFT LOOKING  */
   qlon = c1/(rlocal*cosd(qlat))*rtd;

   x = rlocal * cosd(qlat)*cosd(qlon);
   y = rlocal * cosd(qlat)*sind(qlon);
   z = rlocal * sind(qlat);

   rotate_z(&x,&y,&z,alpha3,-1.0);
   rotate_y(&x,&y,&z,alpha2,-1.0);
   rotate_z(&x,&y,&z,alpha1,-1.0);
  
   ebf_to_ll(x,y,z,lat,lat_d,lon);	/* These x,y,z are not real ebf */
 }
/******************************************************************************/

void ll_ac(double lat, double lon, double *c1, double *c2)
 {
  double x,y,z;
  double x_t,y_t,z_t;
  double qlat, qlon;

  ll_to_ebf(lat,lon,&x,&y,&z);

  x_t = x; y_t = y; z_t = z;
 
  rotate_z(&x_t,&y_t,&z_t,alpha1,1.0);
  rotate_y(&x_t,&y_t,&z_t,alpha2,1.0);
  rotate_z(&x_t,&y_t,&z_t,alpha3,1.0);

  qlat = asind(z_t/rlocal);
  qlon = atan2d(y_t,x_t);

  *c1 = qlon*rlocal*cosd(qlat)*dtr;
  *c2 = -1.0*qlat*rlocal*dtr;  /* right looking */

  /* c2 = qlat * rlocal * dtr;   left looking */

 }
/******************************************************************************/

void rotate_z(double *x0,double *y0,double *z0,double theta0,double sign)
 {
  double theta, x1,y1, x,y;

  theta = theta0*sign;
  x = *x0;
  y = *y0;
  x1 = x*cosd(theta)+y*sind(theta);
  y1 = -1.0*x*sind(theta)+y*cosd(theta);
  *x0 = x1; *y0 = y1;
 }
/******************************************************************************/

void rotate_y(double *x0, double *y0, double *z0,double theta0, double sign)
 {
  double theta, x1,z1, x,z;

  theta = theta0*sign;
  x = *x0; z = *z0;
  z1 = z*cosd(theta)+x*sind(theta);
  x1 = -1.0*z*sind(theta)+x*cosd(theta);
  *x0 = x1; *z0 = z1;
 }     
/******************************************************************************/

void ll_to_ebf(double lat, double lon,double *x,double *y,double *z)
 {
  double r_local;

  r_local = RE*RP / sqrt( SQR(RP*cosd(lat)) + SQR(RE*sind(lat)) );
  *x = r_local * cosd(lon) * cosd(lat);
  *y = r_local * sind(lon) * cosd(lat);
  *z = r_local * sind(lat);
 }
/******************************************************************************/

void ebf_to_ll(double x,double y,double z,double *lat,double *lat_d,double *lon)
 {
  *lat = asind(z/sqrt(SQR(x)+SQR(y)+SQR(z)));
  *lon = atan2d(y,x);
  *lat_d = atand(tand(*lat)/(1-ecc2));
 }
