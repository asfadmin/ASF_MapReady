/******************************************************************************
NAME:JPL_proj

SYNOPSIS:
	A set of map projection routines from JPL.
Includes Along-Track/Cross-Track (AT/CT), 
Universal Transverse Mercator,
Polar Sterographic, and Lambert.

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


#define SQR(A)	((A)*(A))
#define mag(x,y,z) sqrt(SQR(x)+SQR(y)+SQR(z))

#define RE (proj->re_major) /*Extract semimajor axis of earth (equatorial)*/
#define RP (proj->re_minor) /*Extract semiminor axis of earth (polar)*/
#define ecc2 (proj->ecc*proj->ecc) /*Extract first eccentricity, squared*/
#define ecc_e (proj->ecc) /*Extract first eccentricity*/

/*Projection Prototypes;*/
void ll_ac(meta_projection *proj, char look_dir, double lat, double lon, double *c1, double *c2);
void ll_lambert(meta_projection *proj,double lat,double lon,double *x,double *y);
void ll_ps(meta_projection *proj,double lat, double lon, double *x, double *y);
void ll_utm(meta_projection *proj,double tlat, double tlon, double *p1, double *p2);

void ac_ll(meta_projection *proj, char look_dir, double c1, double c2,double *lat_d, double *lon);
void lambert_ll(meta_projection *proj,double x,double y,double *lat_d,double *lon);
void ps_ll(meta_projection *proj,double xx,double yy,double *alat,double *alon);
void utm_ll(meta_projection *proj,double x,double y,double *lat_d,double *lon);

/*Convert projection units (meters) from geodetic latitude and longitude (degrees).*/
void ll_to_proj(meta_projection *proj,char look_dir,double lat_d,double lon,double *p1,double *p2)
{
	if (proj==NULL)
		bail("NULL projection parameter structure passed to ll_to_proj!\n");
	switch (proj->type)
	{
		case 'A': ll_ac(proj,look_dir,lat_d,lon,p2,p1); break;
		case 'L': ll_lambert(proj,lat_d,lon,p1,p2); break;
		case 'P': ll_ps(proj,lat_d,lon,p1,p2); break;
		case 'U': ll_utm(proj,lat_d,lon,p1,p2); break;
		default:
			printf("Unrecognized map projection '%c' passed to ll_to_proj!\n",proj->type);
			exit(1);
	}
}

/*Convert projection units (meters) to geodetic latitude and longitude (degrees).*/
void proj_to_ll(meta_projection *proj, char look_dir, double p1, double p2, double *lat_d, double *lon)
{
	if (proj==NULL)
		bail("NULL projection parameter structure passed to ll_to_proj!\n");
	switch(proj->type)
	{
		case 'A': ac_ll(proj,look_dir,p2,p1,lat_d,lon); break;
		case 'L': lambert_ll(proj,p1,p2,lat_d,lon); break;
		case 'P': ps_ll(proj,p1,p2,lat_d,lon); break;
		case 'U': utm_ll(proj,p1,p2,lat_d,lon); break;
		default:
			printf("Unrecognized map projection '%c' passed to proj_to_ll!\n",proj->type);
			exit(1);
	}
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
void lambert_init(proj_lambert *l,double e,
	double *m1,double *m2,double *t0,double *t1,double *t2,double *n)
{
	*m1 = cosd(l->plat1)/sqrt(1.0-SQR(e*sind(l->plat1)));
	*m2 = cosd(l->plat2)/sqrt(1.0-SQR(e*sind(l->plat2)));
	
	*t1=tand(45.0-l->plat1/2.0)
			/
		pow(((1.0-e*sind(l->plat1))/(1.0+e*sind(l->plat1))),(e/2.0));
	*t2=tand(45.0-l->plat2/2.0)
			/
		pow(((1.0-e*sind(l->plat2))/(1.0+e*sind(l->plat2))),(e/2.0));
	*t0=tand(45.0-l->lat0 /2.0)
			/
		pow(((1.0-e*sind(l->lat0 ))/(1.0+e*sind(l->lat0 ))),(e/2.0));
	
	if (l->plat1!=l->plat2) 
		*n = log(*m1/ *m2) / log(*t1/ *t2); 
	else 
		*n = sind(l->plat1);
}
void ll_lambert(meta_projection *proj,double lat,double lon,double *x,double *y)
{
	double F, rho_0, t, rho, theta;
	double m1, m2, t0, t1, t2, n;
	double e=ecc_e;
	lambert_init(&proj->param.lambert,e,
		&m1,&m2,&t0,&t1,&t2,&n);
	
	F = m1/(n*pow(t1,n)); rho_0 = RE*F*pow(t0,n);
	
	t = tand(45.0-lat/2.0)/pow(((1.0-e*sind(lat))/(1.0+e*sind(lat))),(e/2.0));
	rho = RE*F*pow(t,n);
	theta = n*(lon-proj->param.lambert.lon0);
	
	*x = rho*sind(theta);
	*y = rho_0-rho*cosd(theta);
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
void lambert_ll(meta_projection *proj,double x,double y,double *lat_d,double *lon)
{
	double F, rho_0, t, rho, theta, chi;
	double m1, m2, t0, t1, t2, n;
	double e=ecc_e,e2=ecc2;
	lambert_init(&proj->param.lambert,e,
		&m1,&m2,&t0,&t1,&t2,&n);
	
	F = m1 / ( n * pow(t1,n) );
	rho_0 = RE * F * pow(t0,n);
	rho = sqrt(x*x + SQR(rho_0-y));
	
	/* If n is negative, the signs of x,y and rho_0 must be reversed
	(see notes in "Map projections used by the USGS) */
	theta = atand(x/(rho_0-y));
	*lon   = theta / n + proj->param.lambert.lon0;
	t = pow((rho/(RE*F)),(1.0/n));
	
	/* use a series to calculate the inverse formula */
	chi = (M_PI/2.0) - 2.0*atan(t);
	
	*lat_d = chi
	        +  (e2 /2.0 +  e2*e2* 5.0/24.0+ e2*e2*e2 /12.0)
	        	*sin(2.0*chi)
	        +  (e2*e2* 7.0/48.0 + e2*e2*e2* 9.0/240.0)
	        	*sin(4.0*chi)
	        +  (e2*e2*e2* 7.0/120.0)
	        	*sin(6.0*chi);
	
	*lat_d = (*lat_d) * R2D;	/* convert to degrees */
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
  slon     double          I      meridian along positive y-axis
 
  Written by C. S. Morris - April 29, 1985
  modified for ers-1 project- genreate the xy table
  instead of one (x,y) per call to the subroutine.    ssp  Jan 1989.
  add re, e2, slat and slon to the calling sequence.  ssp  Mar 1989.
  convert to c					      ASF  1/98
*****************************************************************************/
void ll_ps(meta_projection *proj,double lat, double lon, double *x, double *y)
{
	double re=RE;     /* radius of earth */
	double e=ecc_e,e2=ecc2;  /* eccentricities of earth*/
	double ta[2];
	double sn,rlat,cm,rho;
	double alon, alat;
	int    i;
	
	/* Set constants for ssm/i grid */
	if (lon <= 0) alon = lon + 360; else alon = lon;
	if (lat>=0.0) sn=1.0; else sn=-1.0;
	
	/* Compute x and y in grid coordinates */
	alat=sn*lat; alon=sn*alon; rlat=alat;
	for (i=0; i<2; i++)
	{
		if (i==1) rlat=proj->param.ps.slat;
		ta[i]= tan((M_PI/4.0)-(rlat/(2.0*R2D)))/
			pow(((1.0-e*sin(rlat*D2R))/(1.0+e*sin(rlat*D2R))),(e/2.0));
	}

	cm=cos(proj->param.ps.slat*D2R)/
		sqrt(1.0-e2*(SQR(sin(proj->param.ps.slat*D2R))));
	rho=re*cm*ta[0]/ta[1];
	
	*x=(rho*sn*sin((alon-proj->param.ps.slon)*D2R));
	*y=-(rho*sn*cos((alon-proj->param.ps.slon)*D2R));
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

void ps_ll(meta_projection *proj,double x,double y,double *alat,double *alon)
{
	double re=RE;     /* radius of earth  */
	double e=ecc_e,e2=ecc2;  /* eccentricities of earth */
	double sn,rho,cm,t,chi,slatr;
	
	slatr=proj->param.ps.slat*D2R;
	if (proj->param.ps.slat>=0.0) sn=1.0; else sn=-1.0;
	
	slatr*=sn;/*Make South Hemisphere work-- O. Lawlor*/
	
	/* Compute the latitude and longitude */
	rho=sqrt(x*x+y*y);
	if (rho <= 0.01) { *alat=(sn < 0)?-90.0:90.0; *alon = 0.0; }
	else {
		cm = cos(slatr)/sqrt(1.0-e2*(SQR(sin(slatr))));
		t = tan(M_PI/4.0-slatr/2.0)/
			pow((1.0-e*sin(slatr))/(1.0+e*sin(slatr)),e/2.0);
		t = rho*t/(re*cm);
		chi=(M_PI/2.0)-2.0*atan(t);
		
		*alat = chi
	        +  (e2 /2.0 +  e2*e2* 5.0/24.0+ e2*e2*e2 /12.0)
	        	*sin(2.0*chi)
	        +  (e2*e2* 7.0/48.0 + e2*e2*e2* 9.0/240.0)
	        	*sin(4.0*chi)
	        +  (e2*e2*e2* 7.0/120.0)
	        	*sin(6.0*chi);
		
		*alat = sn * (*alat) * R2D;
		*alon = atan2(sn*x,-sn*y)*R2D+sn*proj->param.ps.slon;
		*alon = sn * (*alon);
		
		while (*alon <= -180.0) *alon += 360.0;  /* OSL 9/98 */
		while (*alon >   180.0) *alon -= 360.0;
	}
}
/**********************UTM Conversion Routines.*******************/
int UTM_zone(double lon) {return(((180.0+lon)/6.0+1.0));} 

void ll_utm(meta_projection *proj,double tlat, double tlon, double *p1, double *p2)
{
	double   k0 = 0.9996;       /* Center meridian scale factor */
	double   epsq, lat, /*lon,*/ lon0, a1, a2, a3, rn;
	double   t, b1, b2, rm, rm0;
	double   tanlat, c;
	double   yadj, xadj = 500000.0;
	double   e4, e6, c1, c2, c3, c4;
	double   x,y;
	
	epsq = ecc2/(1.0 - ecc2);
	lon0 = (double)(proj->param.utm.zone-1)*6.0 + 3.0 - 180.0;   /* zone from proj constants */
	if (proj->hem=='N') yadj = 0.0; else yadj = 1.0E7; /* Ref pt Southern Hemisphere */
	
	lat = tlat * D2R; /*lon = tlon * D2R;*/
	
	rn =  RE / sqrt(1.0 - ecc2*SQR(sin(lat)));
	tanlat = tan(lat);
	t = SQR(tanlat);
	c = epsq * SQR(cos(lat));
	a1 = cos(lat) * ((tlon-lon0)*D2R);
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
	
	*p1 = x;
	*p2 = y;
}
void utm_ll(meta_projection *proj,double x,double y,double *lat_d,double *lon)
{
	double esq = ecc2;        /* Earth eccentricity squared */
	double k0  = 0.9996;      /* Center meridian scale factor */
	double u1, u2, u3, lat1, esqsin2, lat1d, long0;
	double rm, e1, u, epsq, t1, c1, rn1, r1, rm0;
	double tanlat1, d;
	double xadj = 500000.0, yadj;    /* Northing origin:(5.d5,0.d0) */
	                               /* Southing origin:(5.d5,1.d7) */
	rm0 = 0.0;
	if (proj->hem=='N') 
		yadj=0.0; 
	else 
		yadj=1.0E7; /* Ref pt Southern Hemisphere */
	long0 = (double)(proj->param.utm.zone)*6.0-183.0;     /* zone from proj_const.inc   */
	
	rm = (y - yadj)/k0 + rm0;
	e1 = (1.0 - sqrt(1.0 - esq))/(1.0 + sqrt(1.0 - esq));
	u = rm/(RE*(1.0-esq/4.0-(3.0*esq*esq/64.0) - (5.0*esq*esq*esq/256.0)));
	u1 = (3.0 * e1 / 2.0 - (27.0 * e1*e1*e1)/32.0) * sin(2.0*u);
	u2 = (21.0 * (e1*e1)/16.0 - (55.0 * e1*e1*e1*e1)/32.0) * sin(4.0*u);
	u3 = (151.0 * (e1*e1*e1) / 96.0) * sin(6.0*u);
	lat1 = u + u1 + u2 + u3;
	lat1d = lat1/D2R;
	
	esqsin2 = 1.0 - esq*SQR(sin(lat1));
	epsq = esq/(1.0 - esq);
	c1 = epsq * SQR(cos(lat1));
	tanlat1 = sin(lat1)/cos(lat1);
	t1 = tanlat1 * tanlat1;
	rn1 = RE/sqrt(esqsin2);
	r1 = RE*(1.0 - esq)/sqrt(esqsin2 * esqsin2 * esqsin2);
	d = (x - xadj)/(rn1 * k0);
	
	*lat_d = lat1d - ((rn1 * tanlat1/r1) * (d*d*0.50
	              - (5.0 + 3.0*t1 - 10.0*c1 + 4.0*c1*c1
	                      - 9.0*epsq) * (d*d*d*d)/24.0
	              + (61.0 + 90.0*t1 + 298.0*c1 + 45.0*t1*t1
	                       - 252.0*epsq - 3.0*c1*c1)
	                  *(d*d*d*d*d*d)/720.0) )/D2R;
	
	*lon = long0 + ((1.0/cos(lat1)) * (d
	      - (1.0 + 2.0*t1 + c1) * (d*d*d)/6.0
	      + (5.0 - 2.0*c1 + 28.0*t1 - 3.0*c1*c1 + 8.0*epsq + 24.0*t1*t1)
	             *(d*d*d*d*d)/120.0) )/D2R;
}
/***************************AT/CT Conversion Routines*********************/
/*Along-Track/Cross-Track utilities:*/
void cross(double x1, double y1, double z1, double x2, double y2, double z2,
      double *u1, double *u2, double *u3)
{
  double r, t1, t2, t3;
  t1=y1*z2-z1*y2; t2=z1*x2-x1*z2; t3=x1*y2-y1*x2;
  r=sqrt(SQR(t1)+SQR(t2)+SQR(t3));
  *u1=t1/r; *u2=t2/r; *u3=t3/r;
}

void rotate_z(vector *v,double theta);
void rotate_y(vector *v,double theta);

/*atct_init calculates alpha1,alpha2, and alpha3,
which are some sort of coordinate rotation amounts,
in degrees.  This creates a latitude/longitude-style
coordinate system centered under the satellite at
the start of imaging.  You must pass it a state vector
from the start of imaging.*/
void atct_init(meta_projection *proj,stateVector st)
{
	vector up={0.0,0.0,1.0};
	vector z_orbit, y_axis, a, nd;
	double alpha3_sign;
	double alpha1,alpha2,alpha3;
	
	vecCross(st.pos,st.vel,&z_orbit);vecNormalize(&z_orbit);
	
	vecCross(z_orbit,up,&y_axis);vecNormalize(&y_axis);
	
	vecCross(y_axis,z_orbit,&a);vecNormalize(&a);
	
	printf("ALPHA123 new x-vector: %.3f,%.3f,%.3f\n",a.x,a.y,a.z);
	
	alpha1 = atan2_check(a.y,a.x)*R2D;
	alpha2 = -1.0 * asind(a.z);
	if (z_orbit.z < 0.0) 
	{
		alpha1 +=  180.0;
		alpha2 = -1.0*(180.0-fabs(alpha2));
	}
	
	vecCross(a,st.pos,&nd);vecNormalize(&nd);
	alpha3_sign = vecDot(nd,z_orbit);
	alpha3 = acosd(vecDot(a,st.pos)/vecMagnitude(st.pos));
	if (alpha3_sign<0.0) 
		alpha3 *= -1.0;
	
	printf("alpha1, alpha2, alpha3  %f %f %f\n",alpha1,alpha2,alpha3);
	proj->param.atct.alpha1=alpha1;
	proj->param.atct.alpha2=alpha2;
	proj->param.atct.alpha3=alpha3;
}

void ac_ll(meta_projection *proj, char look_dir, double c1, double c2, double *lat_d, double *lon)
{
	double qlat, qlon;
	double lat,radius;
	vector pos;
	
	if (look_dir=='R')
		qlat = -c2/proj->param.atct.rlocal; /* Right looking sar*/
	else
		qlat =  c2/proj->param.atct.rlocal;   /* Left looking sar  */
	qlon = c1/(proj->param.atct.rlocal*cos(qlat));
	
	sph2cart(proj->param.atct.rlocal,qlat,qlon,&pos);
	
	rotate_z(&pos,-proj->param.atct.alpha3);
	rotate_y(&pos,-proj->param.atct.alpha2);
	rotate_z(&pos,-proj->param.atct.alpha1);
	
	cart2sph(pos,&radius,&lat,lon);
	*lon *=R2D;
	lat*=R2D;
	*lat_d= atand(tand(lat)/(1 - ecc2));
}

void ll_ac(meta_projection *proj, char look_dir, double lat_d, double lon, double *c1, double *c2)
{
	double qlat, qlon;
	double lat,radius;
	vector pos;
	
	lat= atand(tand(lat_d)*(1 - ecc2));
	sph2cart(proj->param.atct.rlocal,lat*D2R,lon*D2R,&pos);
	
	rotate_z(&pos,proj->param.atct.alpha1);
	rotate_y(&pos,proj->param.atct.alpha2);
	rotate_z(&pos,proj->param.atct.alpha3);
	
	cart2sph(pos,&radius,&qlat,&qlon);
	
	*c1 = qlon*proj->param.atct.rlocal*cos(qlat);
	if (look_dir=='R')
		*c2 = -1.0*qlat*proj->param.atct.rlocal;  /* right looking */
	else
		*c2 = qlat * proj->param.atct.rlocal;   /* left looking */
}

void rotate_z(vector *v,double theta)
{
	double xNew,yNew;
	
	xNew = v->x*cosd(theta)+v->y*sind(theta);
	yNew = -v->x*sind(theta)+v->y*cosd(theta);
	v->x = xNew; v->y = yNew;
}

void rotate_y(vector *v,double theta)
{
	double xNew,zNew;
	
	zNew = v->z*cosd(theta)+v->x*sind(theta);
	xNew = -v->z*sind(theta)+v->x*cosd(theta);
	v->x = xNew; v->z = zNew;
}
