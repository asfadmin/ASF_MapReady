#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

#include "asf_meta.h"

/* test function only ... */
int project_poly(double phi_deg, double lam_deg, double * xx, double *yy);

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
int project_utm(project_parameters_t * pps, double lat, double lon,
		double *x, double *y);

/* project_utm_arr

   lon_0 : Longitude of natural origin (radians)

   lat, lon : Array of points to transform (radians).
   projected_x, projected_y : the projected points.

   length : Number of points in the arrays.

   return value: TRUE if point projected ok, FALSE if not.
*/

int project_utm_arr (project_parameters_t * pps,
		     double *lat, double *lon, 
		     double *projected_x, double *projected_y,
		     long length);

/* project_utm_inv

   lon_0 : Longitude of natural origin (radians)

   x, y: Point to (inverse) project
   lat, lon: (Inverse) projected point [output] (radians)

   return value: TRUE if point projected ok, FALSE if not.
*/
int project_utm_inv (project_parameters_t * pps, double x, double y,
		     double *lat, double *lon);

int project_utm_arr_inv (project_parameters_t * pps, 
			 double *x, double *y,
			 double *lat, double *lon,
			 long length);

/****************************************************************************
  Polar Stereographic
  www.remotesensing.org/geotiff/proj_list/polar_stereographic.html
****************************************************************************/

/* project_ps

   pps : structure containing the latitude, longitude of the natural origin
         in radians.

   lat : latitude of point to project (radians)
   lon : longitude of point to project (radians)
   x, y: Projected point [output]

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL
*/ 
int project_ps(project_parameters_t * pps,
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
int project_ps_arr(project_parameters_t * pps,
		   double *lat, double *lon,
		   double *x, double *y,
		   long length);

/* project_ps_inv

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y: Point to (inverse) project
   lat, lon: (Inverse) projected point [output] (radians)

   return value: TRUE if point projected ok, FALSE if not.
*/
int project_ps_inv(project_parameters_t * pps,
		   double x, double y, double *lat, double *lon);

/* project_ps_inv

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y: Array of points to (inverse) project.  On output, latitude
         is in x, longitude in y, both in radians.
   len : Number of points in the x,y arrays.
*/
int project_ps_arr_inv(project_parameters_t * pps,
		       double *x, double *y,
		       double *lat, double *lon,
		       long length);

/****************************************************************************
  Lambert Azimuthal Equal Area
  www.remotesensing.org/geotiff/proj_list/lambert_azimuthal_equal_area.html
****************************************************************************/
int project_lamaz(double lat_0, double lon_0,
		  double lat, double lon, double *x, double *y);
int project_lamaz_s(project_parameters_t * lamaz,
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
