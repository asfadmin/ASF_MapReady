#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

#include "asf_meta.h"

/* test function only ... */
int project_poly(double phi_deg, double lam_deg, double * xx, double *yy);

/**************************************************************************
   project_set_avg_height

   The height parameter to use for projection or inverse projection,
   if height data is not passed.  If this is not called prior to
   projecting, the average height will be 0.  The average height is
   always overridden by the actual height if supplied (i.e. if
   approximately ASF_PROJ_NO_HEIGHT is not used for an
   individual point or NULL not passed for an array transformation).
****************************************************************************/
void project_set_avg_height(double height);

#define ASF_PROJ_NO_HEIGHT -100000.0

/**************************************************************************
   project_set_input_spheroid

   The spheroid to which the geodetic lat/long pairs used as the input
   to a forward projection or output of reverse projection are
   relative.  If this is not called prior to projecting, a GEM6
   spheroid will be used.
****************************************************************************/
void project_set_input_spheroid(spheroid_type_t spheroid);

/**************************************************************************
   project_set_datum

   The datum parameter for the target map projection.  If this is not
   called prior to projecting, an WGS84 ellipsoidal datum will be
   used.
****************************************************************************/
void project_set_datum(datum_type_t datum);

/****************************************************************************
  Universal Transverse Mercator
  www.remotesensing.org/geotiff/proj_list/transverse_mercator.html
****************************************************************************/

/*--------------------------------------------------------------------------
   project_utm

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   lat, lon : Latitude, Longitude of point to transform (radians)
   height : Height above the input spheroid of the point to transform (meters)
   x, y, z : Projected point [output] and height (z)

   If z information is not required, ASF_PROJ_NO_HEIGHT may be passed
   for height, and NULL for z.

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int project_utm(project_parameters_t * pps, double lat, double lon,
		double height, double *x, double *y, double *z);

/*--------------------------------------------------------------------------
   project_utm_arr

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   lat, lon : Array of points to transform (radians).
   height: Heights above the input spheroid of the points to
           transforms (meters)
   projected_x, projected_y, projected_z : the projected points.

      NOTE: If these (projected_x, projected_y, and projected_z) are
            NULL, they will be allocated and the caller must free.  If
            non-null, it is assumed that the caller has pre-allocated
            enough storage to hold the projected points.

   length : Number of points in the arrays of points to be transformed.

   If z information is not required, NULL may be passed for both
   height and projected_z (in which case no space will be allocated to
   hold returned z values).

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/

int project_utm_arr (project_parameters_t * pps,
                     double *lat, double *lon, double *height,
                     double **projected_x, double **projected_y, 
		     double **projected_z, long length);

/*--------------------------------------------------------------------------
   project_utm_inv

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   x, y: Point to (inverse) project
   z: height above projection datum of point to transform (meters)
   lat, lon: (Inverse) projected point [output] (radians)
   height: height above input spheroid (meters)

   If height information is not required, ASF_PROJ_NO_HEIGHT may be
   passed for z, and NULL for height.

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *lat and *lon will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int project_utm_inv (project_parameters_t * pps, double x, double y, double z,
                     double *lat, double *lon, double *height);

/*--------------------------------------------------------------------------
   project_utm_arr_inv

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   x, y: Array of points to (inverse) project
   z: heights above projection datum of points to transform (meters)
   lat, lon: (Inverse) projected points [output] (radians)
   height: heights above input shperoid (meters)

      NOTE: If these (lat, lon, and height) are NULL, they
            will be allocated and the caller must free.  If non-null,
            it is assumed that the caller has pre-allocated enough
            storage to hold the projected points.

   length : Number of points in the arrays of points to be transformed.

   If height information is not required, NULL may be passed for both
   z and height (in which case no space will be allocated to hold
   returned z values).

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/
int project_utm_arr_inv (project_parameters_t * pps,
                         double *x, double *y, double *z,
                         double **lat, double **lon, double **height,
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
   height: height of point to project above input spheroid (meters)
   x, y, z: Projected point [output]

   If z information is not required, ASF_PROJ_NO_HEIGHT may be passed
   for height, and NULL for z.

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int project_ps(project_parameters_t * pps,
               double lat, double lon, double height,
               double *x, double *y, double *z);

/*--------------------------------------------------------------------------
   project_ps_arr

   pps : projection parameters structure with the "ps" union member
         populated, containing the latitude, longitude of the natural
         origin (ps.slat, ps.slon) in radians, and ps.is_north_pole set
         to 1 if the projection is centered on the north pole, 0 if
         the south pole.

   lat, lon : Array of points to transform.  Radians.
   heights: heights of points to transform above input spheroid (meters)
   projected_x, projected_y, projected_z : Projected points.

      NOTE: If these (projected_x, projected_y, and projected_z) are
            NULL, they will be allocated and the caller must free.  If
            non-null, it is assumed that the caller has pre-allocated
            enough storage to hold the projected points.

   length : Number of points in the arrays of points to be transformed.

   If z information is not required, NULL may be passed for both
   height and projected_z (in which case no space will be allocated to
   hold returned z values).

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/
int project_ps_arr(project_parameters_t * pps,
                   double *lat, double *lon, double *height,
                   double **projected_x, double **projected_y,
		   double **projected_z, long length);

/*--------------------------------------------------------------------------
   project_ps_inv

   pps : projection parameters structure with the "ps" union member
         populated, containing the latitude, longitude of the natural
         origin (ps.slat, ps.slon) in radians, and ps.is_north_pole set
         to 1 if the projection is centered on the north pole, 0 if
         the south pole.

   x, y: Point to (inverse) project
   z: height above projection datum of point to transform (meters)
   lat, lon: (Inverse) projected point [output] (radians)
   height: height above input spheroid of inverse projected point (meters)

   If height information is not required, ASF_PROJ_NO_HEIGHT may be
   passed for z, and NULL for height.

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/
int project_ps_inv(project_parameters_t * pps, double x, double y, double z,
		   double *lat, double *lon, double *height);

/*--------------------------------------------------------------------------
   project_ps_arr_inv

   lat_ts : Latitude at natural origin (radians)
   lon_0 : Longitude at natural origin (radians)
   is_north_pole: 1 if projecting from North Pole, 0 if South Pole

   x, y: Array of points to (inverse) project.
   z: heights above projection datum of points to transform (meters)
   lat, lon: (Inverse) projected points [output] (radians)
   height: heights above input shperoid (meters)

      NOTE: If these (lat, lon, and height) are NULL, they will be
            allocated and the caller must free.  If non-null, it is
            assumed that the caller has pre-allocated enough storage
            to hold the projected points.

   length : Number of points in the arrays of points to be transformed.

   If height information is not required, NULL may be passed for both
   z and height (in which case no space will be allocated to hold
   returned z values).

   Please see project.t.c for usage examples
*/
int project_ps_arr_inv(project_parameters_t * pps,
                       double *x, double *y, double *z,
                       double **lat, double **lon, double **height,
                       long length);

/****************************************************************************
  Lambert Azimuthal Equal Area
  www.remotesensing.org/geotiff/proj_list/lambert_azimuthal_equal_area.html
****************************************************************************/
int project_lamaz(project_parameters_t * pps,
		  double lat, double lon, double height, 
		  double *x, double *y, double *z);
int project_lamaz_arr(project_parameters_t *pps,
                      double *lat, double *lon, double *height,
                      double **projected_x, double **projected_y,
		      double **projected_z, long length);
int project_lamaz_inv(project_parameters_t * pps,
                      double x, double y, double z, double *lat, double *lon,
		      double *height);
int project_lamaz_arr_inv(project_parameters_t * pps,
                          double *x, double *y, double *z,
                          double **lat, double **lon, double **height,
                          long length);

/****************************************************************************
  Lambert Conformal Conic
  www.remotesensing.org/geotiff/proj_list/lambert_conic_conformal_2sp.html
****************************************************************************/
int project_lamcc(project_parameters_t * pps,
                  double lat, double lon, double height, 
		  double *x, double *y, double *z);
int project_lamcc_arr(project_parameters_t *pps,
                      double *lat, double *lon, double *height,
                      double **projected_x, double **projected_y,
		      double **projected_z, long length);
int project_lamcc_inv(project_parameters_t * pps,
                      double x, double y, double z,
		      double *lat, double *lon, double *height);
int project_lamcc_arr_inv(project_parameters_t *pps,
                          double *x, double *y, double *z,
                          double **lat, double **lon, double **height,
                          long length);

/****************************************************************************
  Albers Equal-Area Conic
  www.remotesensing.org/geotiff/proj_list/albers_equal_area_conic.html
****************************************************************************/
int project_albers(project_parameters_t *pps,
                   double lat, double lon, double height,
		   double *x, double *y, double *z);
int project_albers_arr(project_parameters_t *pps,
                       double *lat, double *lon, double *height,
                       double **projected_x, double **projected_y,
		       double **projected_z, long length);
int project_albers_inv(project_parameters_t *pps,
                       double x, double y, double z,
		       double *lat, double *lon, double *height);
int project_albers_arr_inv(project_parameters_t *pps,
                           double *x, double *y, double *z,
                           double **lat, double **lon, double **height,
                           long length);

/******************************************************************************
  Pseudo Projection

  Why would you ever translate between lat/longs on an an input
  spheroid and "pseudoprojected" lat/longs with respect to a datum?
  Possibly to perform a datum translation.  Or possibly just to allow
  higher level code that wants to handle pseudoprojections together
  with projections to work properly.

  IMPORTANT: In keeping with the tradition of used for pseudoprojected
  images which want north to be up, the x, y, and z coordinates refer
  to lon, lat, height and height, respectively.  Note that this order
  is different than the order used for the lat/longs on the other side
  of the transformation.  The pseudoprojected coordinates are still in
  radians, not degrees.

  Note that although these functions take a project_parameters_t
  pointer argument for signature compatability with other functions in
  this library, they don't use it (since pseudoprojected images don't
  need any projection parameters).

******************************************************************************/

int project_pseudo (project_parameters_t *pps, double lat, double lon,
		    double height, double *x, double *y, double *z);

int project_pseudo_inv (project_parameters_t *pps, double x, double y,
			double z, double *lat, double *lon, double *height);

int project_pseudo_arr (project_parameters_t *pps, double *lat, double *lon,
			double *height, double **x, double **y, double **z,
			long length);

int project_pseudo_arr_inv (project_parameters_t *pps, double *x, double *y,
			    double *z, double **lat, double **lon,
			    double **height, long length);

/***************************************************************************
  General conversion functions between projection coordinates and geographic
  coordinates.
***************************************************************************/
void proj_to_latlon(meta_projection *proj, char look_dir,
		    double x, double y, double z,
		    double *lat, double *lon, double *height);
void latlon_to_proj(meta_projection *proj, char look_dir,
		    double lat, double lon, double height,
                    double *x, double *y, double *z);


#endif
