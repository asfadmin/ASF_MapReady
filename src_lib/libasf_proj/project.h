#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

#include "asf_meta.h"

/* test function only ... */
int project_poly(double phi_deg, double lam_deg, double * xx, double *yy);

/* set_avg_height
   The height parameter for the projection.  If this is not called prior
   to projecting, the average height will be 0.
****************************************************************************/
void set_avg_height(double height);

/****************************************************************************
  Universal Transverse Mercator
  www.remotesensing.org/geotiff/proj_list/transverse_mercator.html
****************************************************************************/

/* project_utm

   lon_0 : Longitude of natural origin (radians)

   lat, lon : Latitude, Longitude of point to transform (radians)
   x, y : Projected point [output]

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL

*/
int project_utm(double lon_0, double lat, double lon,
		double *x, double *y);

/* project_utm_arr

   lon_0 : Longitude of natural origin (radians)

   x, y : Array of points to transform in-place.  Input values
          are latitude (x), longitude (y) pairs (both in radians),
	  outputs are the projected points.
   len : Number of points in the x, y arrays.

   return value: TRUE if point projected ok, FALSE if not.
*/

int project_utm_arr(double lon_0, double *x, double *y, int length);

/* project_utm_inv

   lon_0 : Longitude of natural origin (radians)

   x, y: Point to (inverse) project
   lat, lon: (Inverse) projected point [output] (radians)

   return value: TRUE if point projected ok, FALSE if not.
*/
int project_utm_inv (double lon_0, double x, double y,
		     double *lat, double *lon);

int project_utm_arr_inv (double lon_0, double *x, double *y, int length);

/****************************************************************************
  Polar Stereographic
  www.remotesensing.org/geotiff/proj_list/polar_stereographic.html
****************************************************************************/

/* project_ps

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   lat : latitude of point to project (radians)
   lon : longitude of point to project (radians)
   x, y: Projected point [output]

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL
*/ 
int project_ps(double lat_ts, double lon_0, int is_north_pole,
	       double lat, double lon,
	       double *x, double *y);

/* project_ps_s

   ps : structure containing the latitude, longitude of the natural origin
        in radians.
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   lat : latitude of point to project (radians)
   lon : longitude of point to project (radians)
   x, y: Projected point [output]

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL
*/ 
int project_ps_s(proj_ps * ps, int is_north_pole,
		 double lat, double lon,
		 double *x, double *y);

/* project_ps_arr

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y : Array of points to transform in-place.  Input values are
         latitude (x), longitude (y) pairs (both in radians),
         outputs are the projected points
   len : Number of points in the x, y arrays.

   return value: TRUE if point projected ok, FALSE if not.
*/ 
int project_ps_arr(double lat_0, double lon_0, int is_north_pole,
		   double *x, double *y, int len);

/* project_ps_inv

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y: Point to (inverse) project
   lat, lon: (Inverse) projected point [output] (radians)

   return value: TRUE if point projected ok, FALSE if not.
*/
int project_ps_inv(double lat_ts, double lon_0, int is_north_pole,
		   double x, double y, double *lat, double *lon);

/* project_ps_inv

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y: Array of points to (inverse) project.  On output, latitude
         is in x, longitude in y, both in radians.
   len : Number of points in the x,y arrays.
*/
int project_ps_arr_inv(double lat_ts, double lon_0, int is_north_pole,
		       double *x, double *y, int len);

/****************************************************************************
  Lambert Azimuthal Equal Area
  www.remotesensing.org/geotiff/proj_list/lambert_azimuthal_equal_area.html
****************************************************************************/
int project_lamaz(double lat_0, double lon_0,
		  double lat, double lon, double *x, double *y);
int project_lamaz_s(proj_lamaz * lamaz,
		    double lat, double lon, double *x, double *y);
int project_lamaz_arr(double lat_0, double lon_0,
		      double *x, double *y, int len);

/****************************************************************************
  Lambert Conformal Conic
  www.remotesensing.org/geotiff/proj_list/lambert_conic_conformal_2sp.html
****************************************************************************/
int project_lamcc(double lat_1, double lat_2, double lat_0, double lon_0, 
		  double lat, double lon, double *x, double *y);
int project_lamcc_s(proj_lamcc * lamcc,
		    double lat, double lon, double *x, double *y);
int project_lamcc_arr(double lat_1, double lat_2, double lat_0, double lon_0,
		      double *x, double *y, int length);

/****************************************************************************
  Albers Equal-Area Conic
  www.remotesensing.org/geotiff/proj_list/albers_equal_area_conic.html
****************************************************************************/
int project_albers(double lat_1, double lat_2, double lat_0, double lon_0,
		   double lat, double lon, double *x, double *y);
int project_albers_s(proj_albers * alb, 
		     double lat, double lon, double *x, double *y);
int project_albers_arr(double lat_1, double lat_2, double lat_0, double lon_0,
		       double *x, double *y, int length);

#endif
