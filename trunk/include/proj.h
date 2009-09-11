#ifndef _PROJ_H_
#define _PROJ_H_

/* Map Projection Routines (in asf_geolib.a) include
file:

These will generally be called as:*/
#if 0

#include "asf.h" /*Global Include file*/
#include "ddr.h" /*for DDR info.*/
#include "proj.h" /*for map projection stuff.*/

...
Inputs:
	char *ddrName;/*Name of DDR file, with or without extension*/
	int line,sample;/*Line and sample to convert to lat,lon*/
Output:
	double lat,lon;/*Latitude and longitude, in radians*/
...
	/*Temporary storage variables*/
	double projX,projY;/*Projection coordinates*/
	struct DDR ddr;
	int error;/*Returned error code (0==no error)*/
	inverse_transform trans[100];/*Array of function pointers*/
	
	c_getddr(ddrName,&ddr);/*Read DDR off disk*/
	
	/*Initialize map projection using parameters in DDR*/
	inv_init(ddr.proj_code,ddr.zone_code,ddr.proj_coef,
		ddr.datum_code,NULL,NULL,&error,trans);
	
	/*Convert input line,sample to projection coords*/
	projX=ddr.upleft[1]+((double)sample)/ddr.ns*(ddr.upright[1]-ddr.upleft[1]);
	projY=ddr.upleft[0]+((double)line)/ddr.nl*(ddr.loleft[0]-ddr.upleft[0]);
	
	/*Convert projection coords to lat,lon (in radians)*/
	trans[ddr.proj_code](projX,projY,&lon,&lat);

...


#endif
/*


   0 = Geographic
   1 = Universal Transverse Mercator (UTM)
   2 = State Plane Coordinates
   3 = Albers Conical Equal Area
   4 = Lambert Conformal Conic
   5 = Mercator
   6 = Polar Stereographic
   7 = Polyconic
   8 = Equidistant Conic
   9 = Transverse Mercator
  10 = Stereographic
  11 = Lambert Azimuthal Equal Area
  12 = Azimuthal Equidistant
  13 = Gnomonic
  14 = Orthographic
  15 = General Vertical Near-Side Perspective
  16 = Sinusiodal
  17 = Equirectangular
  18 = Miller Cylindrical
  19 = Van der Grinten
  20 = (Hotine) Oblique Mercator 
  21 = Robinson
  22 = Space Oblique Mercator (SOM)
  23 = Alaska Conformal
  24 = Interrupted Goode Homolosine 
  25 = Mollweide
  26 = Interrupted Mollweide
  27 = Hammer
  28 = Wagner IV
  29 = Wagner VII
  30 = Oblated Equal Area
  99 = User defined
*/

#define GEO 0
#define UTM 1
#define SPCS 2
#define ALBERS 3
#define LAMCC 4
#define MERCAT 5
#define PS 6
#define POLYC 7
#define EQUIDC 8
#define TM 9
#define STEREO 10
#define LAMAZ 11
#define AZMEQD 12
#define GNOMON 13
#define ORTHO 14
#define GVNSP 15
#define SNSOID 16
#define EQRECT 17
#define MILLER 18
#define VGRINT 19
#define HOM 20
#define ROBIN 21
#define SOM 22
#define ALASKA 23
#define GOOD 24
#define MOLL 25
#define IMOLL 26
#define HAMMER 27
#define WAGIV 28
#define WAGVII 29
#define OBEQA 30
#define USDEF 99 

#define IN_BREAK -2
#define COEFCT 15		/*  projection coefficient count	     */
#define PROJCT 31		/*  projection count			     */
#define DATMCT 20		/*  datum count				     */

#define MAXPROJ 30		/*  Maximum projection number */
#define MAXUNIT 5		/*  Maximum unit code number */
#define GEO_TERM 0		/*  Array index for print-to-term flag */
#define GEO_FILE 1		/*  Array index for print-to-file flag */
#define GEO_TRUE 1		/*  True value for geometric true/false flags */
#define GEO_FALSE -1		/*  False val for geometric true/false flags */

int calc_utm_zone(double lon); /*lon in degrees.*/
int c_steplr (int *npts, int *n_indep, double (*raw_data)[16], double *coefs, double *alpha); 
int c_mapedg (double *seast, double *eeast, double *snorth, double *enorth, int *inunits, int *proj_code, int *zone, double *parout, int *units, double *pxmin, double *pxmax, double *pymin, double *pymax); 
int c_proj (int *inproj, int *inunit, int *inzone, double *inparm, int *outproj, int *outunit, int *outzone, double *outparm, double *inx, double *iny, double *outx, double *outy); 
int c_projon (int *inproj, int *inunit, int *inzone, int *indatum, double *inparm, int *outproj, int *outunit, int *outzone, int *outdatum, double *outparm, int *prtprm, char *fname); 
void c_prostr (int *proj_code, char *string); 

/*A note about these projections:
	They expect latitude and longitude in radians
(not the usual degrees), and the projection coordinates are
(usually) meters.*/
typedef int (*forward_transform)(double lon,double lat,double *x,double *y);
typedef int (*inverse_transform)(double x,double y,double *lon,double *lat);


void for_init(
	int outsys,		/* output system code				*/
	int outzone,		/* output zone number				*/
	double *outparm,	/* output array of projection parameters	*/
	int outdatum,		/* output datum					*/
	char *fn27,		/* NAD 1927 parameter file			*/
	char *fn83,		/* NAD 1983 parameter file			*/
	int *iflg,		/* status flag			*/
/* forward function pointer array-- should contain at least 100
elements.  To use, call for_trans[outsys](..).*/
	forward_transform for_trans[]
	);

void inv_init(
	int insys,		/* input system code				*/
	int inzone,		/* input zone number				*/
	double *inparm,		/* input array of projection parameters		*/
	int indatum,		/* input datum code				*/
	char *fn27,		/* NAD 1927 parameter file			*/
	char *fn83,		/* NAD 1983 parameter file			*/
	int *iflg,		/* status flag					*/
/* Inverse function pointer array-- should contain at least 100
elements.  To use, call inv_trans[insys](..).*/
	inverse_transform inv_trans[]
	);
#endif
