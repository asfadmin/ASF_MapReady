#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

#include "asf_meta.h"

/* test function only ... */
int project_poly(double phi_deg, double lam_deg, double * xx, double *yy);

/****************************************************************************
  Universal Transverse Mercator
  www.remotesensing.org/geotiff/proj_list/transverse_mercator.html
****************************************************************************/

/*--------------------------------------------------------------------------
   project_utm

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   lat, lon : Latitude, Longitude of point to transform (radians)
   x, y : Projected point [output]

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int project_utm(project_parameters_t * pps, double lat, double lon,
		double *x, double *y);

/*--------------------------------------------------------------------------
   project_utm_arr

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   lat, lon : Array of points to transform (radians).
   projected_x, projected_y : the projected points.

      NOTE: If these (projected_x and projected_y) are NULL, they
            will be allocated and the caller must free.  If non-null,
            it is assumed that the caller has pre-allocated enough
            storage to hold the projected points.

   length : Number of points in the arrays.

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/

int project_utm_arr (project_parameters_t * pps,
		     double *lat, double *lon, 
		     double **projected_x, double **projected_y,
		     long length);

/*--------------------------------------------------------------------------
   project_utm_inv

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   x, y: Point to (inverse) project
   lat, lon: (Inverse) projected point [output] (radians)

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *lat and *lon will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int project_utm_inv (project_parameters_t * pps, double x, double y,
		     double *lat, double *lon);

/*--------------------------------------------------------------------------
   project_utm_arr_inv

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   x, y: Array of points to (inverse) project
   lat, lon: (Inverse) projected points [output] (radians)

      NOTE: If these (lat and lon) are NULL, they
            will be allocated and the caller must free.  If non-null,
            it is assumed that the caller has pre-allocated enough
            storage to hold the projected points.

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/
int project_utm_arr_inv (project_parameters_t * pps, 
			 double *x, double *y,
			 double **lat, double **lon,
			 long length);

/****************************************************************************
  Polar Stereographic
  www.remotesensing.org/geotiff/proj_list/polar_stereographic.html
****************************************************************************/

/*--------------------------------------------------------------------------
   project_ps

   pps : projection parameters structure with the "ps" union member
         populated, containing the latitude, longitude of the natural
	 origin (ps.slat, ps.slon) in radians, and ps.is_north_pole set
	 to 1 if the projection is centered on the north pole, 0 if
         the south pole.

   lat : latitude of point to project (radians)
   lon : longitude of point to project (radians)
   x, y: Projected point [output]

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL

   Please see project.t.c for usage examples
*/ 
int project_ps(project_parameters_t * pps,
	       double lat, double lon,
	       double *x, double *y);

/*--------------------------------------------------------------------------
   project_ps_arr

   pps : projection parameters structure with the "ps" union member
         populated, containing the latitude, longitude of the natural
	 origin (ps.slat, ps.slon) in radians, and ps.is_north_pole set
	 to 1 if the projection is centered on the north pole, 0 if
         the south pole.

   lat, lon : Array of points to transform.  Radians.
   projected_x, projected_y : Projected points.

      NOTE: If these (projected_x and projected_y) are NULL, they
            will be allocated and the caller must free.  If non-null,
            it is assumed that the caller has pre-allocated enough
            storage to hold the projected points.

   len : Number of points in the x, y arrays.

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/ 
int project_ps_arr(project_parameters_t * pps,
		   double *lat, double *lon,
		   double **projected_x, double **projected_y,
		   long length);

/*--------------------------------------------------------------------------
   project_ps_inv

   pps : projection parameters structure with the "ps" union member
         populated, containing the latitude, longitude of the natural
	 origin (ps.slat, ps.slon) in radians, and ps.is_north_pole set
	 to 1 if the projection is centered on the north pole, 0 if
         the south pole.

   x, y: Point to (inverse) project
   lat, lon: (Inverse) projected point [output] (radians)

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/
int project_ps_inv(project_parameters_t * pps,
		   double x, double y, double *lat, double *lon);

/*--------------------------------------------------------------------------
   project_ps_arr_inv

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y: Array of points to (inverse) project.  On output, latitude
         is in x, longitude in y, both in radians.
   lat, lon: (Inverse) projected points [output] (radians)

      NOTE: If these (lat and lon) are NULL, they
            will be allocated and the caller must free.  If non-null,
            it is assumed that the caller has pre-allocated enough
            storage to hold the projected points.

   len : Number of points in the arrays.

   Please see project.t.c for usage examples
*/
int project_ps_arr_inv(project_parameters_t * pps,
		       double *x, double *y,
		       double **lat, double **lon,
		       long length);

/****************************************************************************
  Lambert Azimuthal Equal Area
  www.remotesensing.org/geotiff/proj_list/lambert_azimuthal_equal_area.html
****************************************************************************/
int project_lamaz(project_parameters_t * pps,
		    double lat, double lon, double *x, double *y);
int project_lamaz_arr(project_parameters_t * pps,
		      double * lat, double * lon,
		      double ** projected_x, double ** projected_y, 
		      int len);
int project_lamaz_inv(project_parameters_t * pps,
		      double x, double y, double *lat, double *lon);
int project_lamaz_arr_inv(project_parameters_t * pps,
			  double * x, double * y,
			  double ** lat, double ** lon, 
			  int len);

/****************************************************************************
  Lambert Conformal Conic
  www.remotesensing.org/geotiff/proj_list/lambert_conic_conformal_2sp.html
****************************************************************************/
int project_lamcc(project_parameters_t * pps,
		  double lat, double lon, double *x, double *y);
int project_lamcc_arr(project_parameters_t * pps,
		      double * lat, double * lon,
		      double ** projected_x, double ** projected_y, 
		      int len);
int project_lamcc_inv(project_parameters_t * pps,
		      double x, double y, double *lat, double *lon);
int project_lamcc_arr_inv(project_parameters_t * pps,
			  double * x, double * y,
			  double ** lat, double ** lon, 
			  int len);

/****************************************************************************
  Albers Equal-Area Conic
  www.remotesensing.org/geotiff/proj_list/albers_equal_area_conic.html
****************************************************************************/
int project_albers(project_parameters_t * pps,
		   double lat, double lon, double *x, double *y);
int project_albers_arr(project_parameters_t * pps,
		       double * lat, double * lon,
		       double ** projected_x, double ** projected_y, 
		       int len);
int project_albers_inv(project_parameters_t * pps,
		       double x, double y, double *lat, double *lon);
int project_albers_arr_inv(project_parameters_t * pps,
			   double * x, double * y,
			   double ** lat, double ** lon, 
			   int len);

#endif
