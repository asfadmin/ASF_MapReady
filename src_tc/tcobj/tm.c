/****************************************************************************
NAME:        Tranverse Mercator Routines
 
DESCRIPTION: The routines in this file, utm_init, tm_forward, and tm_inverse,
             provide the ability to transform lon,lat coordinates to
             Universal Transverse Mercator coordinates and vice versa.
 
EXTERNAL ASSOCIATES:  cproj.c -- contains miscellaneous one-line routines
 
FILE REFERENCES:  None
 
PROGRAM HISTORY:
  VERS:   DATE:    AUTHOR:              PURPOSE:
  ---------------------------------------------------------------
  1.0     Nov, '91 D. Steinwand  EROS   Initial Implementation
  1.1     Oct, '94 Tom Logan      ASF   Documentation and Clean Up
 
HARDWARE/SOFTWARE LIMITATIONS:
 
ALGORITHM DESCRIPTION:
 
ALGORITHM REFERENCES:
 
1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Proffessional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987.
 
2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
    U.S. Geological Survey Professional Paper 1453 , United State Government
    Printing Office, Washington D.C., 1989.
 
BUGS:
*******************************************************************************/
#include "asf.h"
#include "sarsim.h"
 
/* Variables common to all subroutines in this code file
  -----------------------------------------------------*/
static double r_major;
static double r_minor;
static double scale_factor;
static double lon_center;	/* Center longitude (projection center) */
static double lat_origin;
static double e0,e1,e2;
static double e,es,esp;
static double ml0;
static double false_northing;
static double false_easting;
 
/****************************************************************
FUNCTION NAME:  utm_init
 
SYNTAX:         utm_init(int zone);
 
PARAMETER:      int zone  --  UTM Zone of image in question
 
DESCRIPTION:    Initializes the Universal Transverse Mercator
                (UTM) projection by setting global variables
                based on the UTM zone requested.
 
RETURN VALUE:   None
 
SPECIAL CONSIDERATIONS:  This routine should be called before either
                         of the others in this file are used.
****************************************************************/
int utm_init(zone) 
int zone;
{
double temp;
 
/* Place parameters in static storage for common use
  -------------------------------------------------*/
r_major = 6378206.4;
r_minor = 6356583.8;
scale_factor = 0.9996;
lon_center = ((6 * zone) - 183) * D2R;
lat_origin = 0.0;
false_northing = 0.0;
false_easting = 500000.0;
 
temp = r_minor / r_major;
es = 1.0 - SQUARE(temp);
e = sqrt(es);
e0 = e0fn(es);
e1 = e1fn(es);
e2 = e2fn(es);
ml0 = r_major * mlfn(e0, e1, e2, lat_origin);
esp = es / (1.0 - es);
 
/* Report parameters to the user
  -----------------------------*/
/*
printf("Universal Transverse Mercator (UTM)\n"); 
printf("\tZone                        = %li\n",zone);
printf("\tR_major,R_minor             = %lf,%lf\n",r_major, r_minor);
printf("\tScale Factor at C. Meridian = %lf\n",scale_factor);
printf("\tlon_center                  = %lf\n",lon_center);
printf("\tlat_origin                  = %lf\n",lat_origin);
printf("\tes                          = %lf\n",es);
printf("\te                           = %lf\n",e);
printf("\te0                          = %lf\n",e0);
printf("\te1                          = %lf\n",e1);
printf("\te2                          = %lf\n",e2);
printf("\tml0                         = %lf\n",ml0);
printf("\tesp                         = %lf\n",esp);
*/
return(1);
}
 
/****************************************************************
FUNCTION NAME:  tm_forward
 
SYNTAX: tm_forward(double lon, lat, *x, *y);
 
PARAMETERS:
    NAME:	TYPE:	   PURPOSE:
    --------------------------------------------------------
    lon, lat    double     longitude, latitude of point to transform
      x,   y    double *   x, y UTM coordinates of input point    
 
DESCRIPTION:  Transverse Mercator forward equations
    Transforms input lon,lat coordinates to output x,y UTM coordinates.  
 
RETURN VALUE:  None
 
SPECIAL CONSIDERATIONS:  None
****************************************************************/
int tm_forward(lon, lat, x, y)
double lon;		/* (I) Longitude */
double lat;		/* (I) Latitude */
double *x;		/* (O) X projection coordinate */
double *y;		/* (O) Y projection coordinate */
{
double delta_lon;	/* Delta longitude (Given longitude - center */
double sin_phi, cos_phi;
double al, als;
double c, t, tq;
double con, n, ml;
 
/* Forward equations
  -----------------*/
delta_lon = adjust_lon(lon - lon_center);
sincos(lat, &sin_phi, &cos_phi);
al = cos_phi * delta_lon;
als = SQUARE(al);
c = esp * SQUARE(cos_phi);
tq = tan(lat);
t = SQUARE(tq);
con = 1.0 - es * SQUARE(sin_phi);
n = r_major / sqrt(con);
ml = r_major * mlfn(e0, e1, e2, lat);
 
*x = scale_factor * n * al * (1.0 + als / 6.0 * (1.0 - t + c + als / 20.0 *
     (5.0 - 18.0 * t + SQUARE(t) + 72.0 * c - 58.0 * esp))) + false_easting;
 
*y = scale_factor * (ml - ml0 + n * tq * (als * (0.5 + als / 24.0 *
     (5.0 - t + 9.0 * c + 4.0 * SQUARE(c) + als / 30.0 * (61.0 - 58.0 * t
     + SQUARE(t) + 600.0 * c - 330.0 * esp))))) + false_northing;
 
return(OK);
}
 
/****************************************************************
FUNCTION NAME:  tm_inverse
 
SYNTAX:         tm_inverse(double x,y, *lon,*lat);
 
PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    x, y        double          Input x,y coordinates in UTM projection
    lon, lat    double *        Output lon, lat coordinates
 
DESCRIPTION:  Transverse Mercator inverse equations
    Transforms input x,y UTM coordinates to output lat,lon coordinates
 
RETURN VALUE:  None
 
SPECIAL CONSIDERATIONS:  None
****************************************************************/
int tm_inverse(x, y, lon, lat)
double x;		/* (I) X projection coordinate */
double y;		/* (I) Y projection coordinate */
double *lon;		/* (O) Longitude */
double *lat;		/* (O) Latitude */
{
double con,phi;
double delta_phi;
int i;
double sin_phi, cos_phi, tan_phi;
double c, cs, t, ts, n, r, d, ds;
int max_iter = 6;
 
/* Inverse equations
  -----------------*/
x = x - false_easting;
y = y - false_northing;
con = (ml0 + y / scale_factor) / r_major;
phi = con;
 
for (i=0;;i++)
   {
   delta_phi = ((con + e1 * sin(2.0*phi) - e2 * sin(4.0*phi)) / e0) - phi;
   phi += delta_phi;
   if (fabs(delta_phi) <= EPSLN) break;
   if (i >= max_iter) 
      { printf("TM-inverse : Latitude failed to converge\n"); return(ERROR);}
   }
 
if (fabs(phi) < HALF_PI)
   {
   sincos(phi, &sin_phi, &cos_phi); 
   tan_phi = tan(phi);
   c = esp * SQUARE(cos_phi);
   cs = SQUARE(c);
   t = SQUARE(tan_phi);
   ts = SQUARE(t);
   con = 1.0 - es * SQUARE(sin_phi); 
   n = r_major / sqrt(con);
   r = n * (1.0 - es) / con;
   d = x / (n * scale_factor);
   ds = SQUARE(d);
 
   *lat = phi - (n * tan_phi * ds / r) * (0.5 - ds / 24.0 * (5.0 + 3.0 * t + 
          10.0 * c - 4.0 * cs - 9.0 * esp - ds / 30.0 * (61.0 + 90.0 * t +
          298.0 * c + 45.0 * ts - 252.0 * esp - 3.0 * cs)));
 
   *lon = adjust_lon(lon_center + (d * (1.0 - (ds / 6.0) * (1.0 + (2.0 * t) +
          c - (ds / 20.0) * (5.0 - (2.0 * c) + (28.0 * t) - (3.0 * cs) +
          (8.0 * esp) + (24.0 * ts)))) / cos_phi));
   }
else
   {
   *lat = HALF_PI * sign(y);
   *lon = lon_center;
   }
 
 /*printf("TM_INVERSE:  l,s = %lf,%lf  lat,lon = %lf,%lf\n",x,y,*lat,*lon); */
 
return(OK);
}
