#ifndef _JPL_PROJ_H_
#define _JPL_PROJ_H_

/******************************************************************************
NAME:JPL_proj

SYNOPSIS:
	A set of map projection routines from JPL.
Includes Along-Track/Cross-Track (AT/CT), 
Universal Transverse Mercator,
Polar Sterographic, and Lambert.

******************************************************************************/

/*Prototypes:*/
/*
These now live in the proj_parameters structure:
#define RE 	6378144.0   GEM-06 Ellipsoid.
#define ecc_e	8.1827385e-2 GEM-06 Eccentricity
#define ecc2	SQR(ecc_e)
*/
#define cspeed	299.792458e6
#define omega_e 7.29211585e-5

#define sind(x) sin((x)*D2R)
#define cosd(x) cos((x)*D2R)
#define tand(x) tan((x)*D2R)
#define asind(x) (asin(x)*R2D)
#define acosd(x) (acos(x)*R2D)
#define atand(x) (atan(x)*R2D)
#define atan2d(y,x) (atan2(y,x)*R2D)

/*Initialize Along-Track/Cross-Track projection with
given fixed-earth state vector.*/
void atct_init(geo_parameters *geo,stateVector stVec);

/*Convert given longitude (degrees) to UTM zone code.*/
int UTM_zone(double longitude);

/*Convert projection units (meters) to geodetic latitude and longitude (degrees).*/
void ll_to_proj(geo_parameters *geo,double lat_d,double lon,double *p1,double *p2);
void proj_to_ll(geo_parameters *geo,double p1, double p2, double *lat_d, double *lon);
#endif
