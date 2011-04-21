#ifndef INCLUDED_PROJ_H
#define INCLUDED_PROJ_H

#include <stdio.h>
#include "asf.h"
#include "asf_tiff.h"

typedef enum {
  UNIVERSAL_TRANSVERSE_MERCATOR,
  POLAR_STEREOGRAPHIC,
  ALBERS_EQUAL_AREA,
  LAMBERT_CONFORMAL_CONIC,
  LAMBERT_AZIMUTHAL_EQUAL_AREA,
  STATE_PLANE,
 /* along-track/across-track is ScanSAR specific projection */
  SCANSAR_PROJECTION,
  /* A simple "projection" in which the image pixels are arranged such
     that latitude and longitude lines form an regular rectangular
     grid over the image.  */
  LAT_LONG_PSEUDO_PROJECTION,
  MERCATOR,
  EQUI_RECTANGULAR,
  EQUIDISTANT,
  SINUSOIDAL,
  UNKNOWN_PROJECTION
} projection_type_t;

typedef enum {
  BESSEL_SPHEROID,
  CLARKE1866_SPHEROID,
  CLARKE1880_SPHEROID,
  GEM6_SPHEROID,
  GEM10C_SPHEROID,
  GRS1967_SPHEROID,
  GRS1980_SPHEROID,
  INTERNATIONAL1924_SPHEROID,
  INTERNATIONAL1967_SPHEROID,
  INTERNATIONAL_TERRESTRIAL_REFERENCE_FRAME_1997_SPHEROID,
  WGS66_SPHEROID,
  WGS72_SPHEROID,
  WGS84_SPHEROID,
  HUGHES_SPHEROID,
  TOKYO_SPHEROID,
  JGD2000_SPHEROID,
  SPHERE,
  UNKNOWN_SPHEROID
} spheroid_type_t;

typedef enum {
  EGM96_DATUM,   /* Earth Gravity Model 1996 (spheroid: WGS84) */
  ED50_DATUM,    /* European Datum 1950 (International 1924) */
  ETRF89_DATUM,  /* European Terrestrial Reference Frame 1989 (WGS84) */
  ETRS89_DATUM,  /* European Terrestrial Reference System 1989 (GRS 1980) */
  ITRF97_DATUM,  /* International Terrestrial Reference Frame (GRS 1980) */
  NAD27_DATUM,   /* North American Datum 1927 (Clarke 1866) */
  NAD83_DATUM,   /* North American Datum 1983 (GRS 1980) */
  WGS72_DATUM,   /* World Geodetic System 1972 (WGS72) */
  WGS84_DATUM,   /* World Geodetic System 1984 (WGS84) */
  TOKYO_DATUM,   /* Tokyo Datum (based on Bessel 1841) */
  JGD2000_DATUM, /* JGD2000 Datum (currently used by Japan) */
  HUGHES_DATUM,
  SAD69_DATUM,   // South American Datum 1969
  UNKNOWN_DATUM
} datum_type_t;

 /* Albers Conical Equal Area. */
  typedef struct {
    double std_parallel1;     /* First standard parallel           */
    double std_parallel2;     /* Second standard parallel          */
    double center_meridian;   /* Longitude of center meridian      */
    double orig_latitude;     /* Latitude of the projection origin */
    double false_easting;     /* False Easting                     */
    double false_northing;    /* False Northing                    */
  } proj_albers;
 /* Along-track/cross-track.*/
  typedef struct {
    double rlocal;              /* Radius of earth at scene center (meters)*/
    double alpha1,alpha2,alpha3;/* Rotation angles, in degrees             */
  } proj_atct;
 /* Lambert Azimuthal Equal Area. */
  typedef struct {
    double center_lon;        /* Longitude at center of projection */
    double center_lat;        /* Latitude at center of projection  */
    double false_easting;     /* False Easting                     */
    double false_northing;    /* False Northing                    */
  } proj_lamaz;
 /* Lambert Conformal Conic.*/
  typedef struct {
    double plat1;             /* First standard parallel  */
    double plat2;             /* Second standard parallel */
    double lat0;              /* Latitude of Origin       */
    double lon0;              /* Center Meridian          */
    double false_easting;     /* False Easting            */
    double false_northing;    /* False Northing           */
    double scale_factor;      /* Scale Factor             */
  } proj_lamcc;
 /* Polar Sterographic.  */
  typedef struct {
    double slat;              /* Standard Parallel 1                        */
    double slon;              /* Center Meridian                            */
    int is_north_pole;        /* 1 if centered on North Pole, 0 if South    */
    double false_easting;     /* False Easting                              */
    double false_northing;    /* False Northing                             */
  } proj_ps;
 /* Universal Transverse Mercator.*/
  typedef struct {
    int zone;                 /* Zone                                       */
    double false_easting;     /* False Easting                              */
    double false_northing;    /* False Northing                             */
    double lat0;              /* Latitude                                   */
    double lon0;              /* Longitude                                  */
    double scale_factor;      /* Scale factor: 0.9996 by definition         */
  } proj_utm;
 /* State Plane. */
  typedef struct {
    int zone;
    char projection[50];      // Projection type: Transverse Mercator
    double orig_latitude;     // Latitude of origin
    double central_meridian;  // Central meridian
    double scale_factor;      // Scale factor
    double false_easting;     // False easting
    double false_northing;    // False northing
  } proj_state;
  // Mercator
  typedef struct {
    double orig_latitude;     // Latitude of origin
    double central_meridian;  // Central meridian
    double standard_parallel; // Standard parallel
    double scale_factor;      // Scale factor (alternative)
    double false_easting;     // False easting
    double false_northing;    // False northing
  } proj_mer;
  // Equi Rectangular
  typedef struct {
    double orig_latitude;     // Latitude of origin
    double central_meridian;  // Central meridian
    double false_easting;     // False easting
    double false_northing;    // False northing
  } proj_eqr;
  // Equidistant
  typedef struct {
    double orig_latitude;     // Latitude of origin
    double central_meridian;  // Central meridian
  } proj_eqc;
  // Sinusoidal
  typedef struct {
    double longitude_center;  // Longitude at projection center
    double false_easting;     // False easting
    double false_northing;    // False northing
    double sphere;            // Spherical radius
  } proj_sin;

 
/* For lat long pseudo projected images, no additional parameters are
   required, so they don't have their own structure type.  */

 /* Projection parameters for the projection in use.  */
  typedef union {
    proj_albers   albers;   /* Albers Conical Equal Area     */
    proj_atct     atct;     /* Along-track/cross-track       */
    proj_lamaz    lamaz;    /* Lambert Azimuthal Equal Area  */
    proj_lamcc    lamcc;    /* Lambert Conformal Conic       */
    proj_ps       ps;       /* Polar Sterographic            */
    proj_utm      utm;      /* Universal Transverse Mercator */
    proj_state    state;    /* State Plane                   */
    proj_mer      mer;      // Mercator
    proj_eqr      eqr;      // Equi Rectangular
    proj_eqc      eqc;      // Equidistant
    proj_sin      sin;      // Sinusoidal
  } param_t;
typedef param_t project_parameters_t;

typedef struct {
  double h;     // Meridian scale factor
  double k;     // Parallel scale factor
  double s;     // Areal scale factor
  double omega; // Angular distortion
  double a;     // Tissot indicatrix major axis
  double b;     // Tissot indicatrix minor axis
} distortion_t;

/* Return the spheroid generally associated with a given datum.
   Unfortunately, in the larger world, a given datum isn't really
   always necessarily associated with a particular spheroid.  For the
   purposes of the data on the insdie of the ASF tools world, however,
   we consider the correspondence to be as described in this function.
   So be sure to test on import if this correspondence is really
   true!

   This function fails if given a datum it hasn't been taught about
   yet.  */
spheroid_type_t
datum_spheroid (datum_type_t datum);

/* Return the semimajor and semiminor axes lengths of spheroid in
   meters.  Fails for spheroids it hasn't been taught about yet.  */
void
spheroid_axes_lengths (spheroid_type_t spheroid, double *major, double *minor);

/* String identifying the datum */
const char *datum_toString(datum_type_t);
const char *spheroid_toString(spheroid_type_t spheroid);
datum_type_t getDatum(char datum_str);

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

/* open a projection file */
FILE *fopen_proj_file(const char *file, const char *mode);

#define ASF_PROJ_NO_HEIGHT -100000.0

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
   datum : The datum used in projected space.  Note that at this time, the
           datum in latlong space is WGS84.

   If z information is not required, ASF_PROJ_NO_HEIGHT may be passed
   for height, and NULL for z.

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *x and *y will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int
project_utm(project_parameters_t * pps, double lat, double lon,
            double height, double *x, double *y, double *z,
            datum_type_t datum);

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
   datum : The datum used in projected space.  Note that at this time, the
           datum in latlong space is WGS84.

   If z information is not required, NULL may be passed for both
   height and projected_z (in which case no space will be allocated to
   hold returned z values).

   return value: TRUE if point projected ok, FALSE if not.

   Please see project.t.c for usage examples
*/

int project_utm_arr (project_parameters_t * pps,
                     double *lat, double *lon, double *height,
                     double **projected_x, double **projected_y,
                 double **projected_z, long length,
                     datum_type_t datum);

/*--------------------------------------------------------------------------
   project_utm_inv

   pps : projection parameters structure with the "utm" union member
         populated with the "zone" to use.

   x, y: Point to (inverse) project
   z: height above projection datum of point to transform (meters)
   lat, lon: (Inverse) projected point [output] (radians)
   height: height above input spheroid (meters)
   datum : The datum used in projected space.  Note that at this time, the
           datum in latlong space is WGS84.

   If height information is not required, ASF_PROJ_NO_HEIGHT may be
   passed for z, and NULL for height.

   return value: TRUE if point projected ok, FALSE if not.
                 In this situation, *lat and *lon will be HUGE_VAL

   Please see project.t.c for usage examples
*/
int project_utm_inv (project_parameters_t * pps, double x, double y, double z,
                     double *lat, double *lon, double *height,
                     datum_type_t datum);

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
                         long length, datum_type_t datum);

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
               double *x, double *y, double *z, datum_type_t datum);

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
int
project_ps_arr(project_parameters_t * pps,
               double *lat, double *lon, double *height,
               double **projected_x, double **projected_y,
               double **projected_z, long length, datum_type_t datum);

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
int
project_ps_inv(project_parameters_t * pps, double x, double y, double z,
               double *lat, double *lon, double *height, datum_type_t datum);

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
                       long length, datum_type_t datum);

/****************************************************************************
  Lambert Azimuthal Equal Area
  www.remotesensing.org/geotiff/proj_list/lambert_azimuthal_equal_area.html
****************************************************************************/
int project_lamaz(project_parameters_t * pps,
                  double lat, double lon, double height,
                  double *x, double *y, double *z, datum_type_t datum);
int project_lamaz_arr(project_parameters_t *pps,
                      double *lat, double *lon, double *height,
                      double **projected_x, double **projected_y,
                      double **projected_z, long length, datum_type_t datum);
int project_lamaz_inv(project_parameters_t * pps,
                      double x, double y, double z, double *lat, double *lon,
                      double *height, datum_type_t datum);
int project_lamaz_arr_inv(project_parameters_t * pps,
                          double *x, double *y, double *z,
                          double **lat, double **lon, double **height,
                          long length, datum_type_t datum);

/****************************************************************************
  Lambert Conformal Conic
  www.remotesensing.org/geotiff/proj_list/lambert_conic_conformal_2sp.html
****************************************************************************/
int project_lamcc(project_parameters_t * pps,
                  double lat, double lon, double height,
                  double *x, double *y, double *z, datum_type_t datum);
int project_lamcc_arr(project_parameters_t *pps,
                      double *lat, double *lon, double *height,
                      double **projected_x, double **projected_y,
                      double **projected_z, long length, datum_type_t datum);
int project_lamcc_inv(project_parameters_t * pps,
                      double x, double y, double z,
                      double *lat, double *lon, double *height, datum_type_t datum);
int project_lamcc_arr_inv(project_parameters_t *pps,
                          double *x, double *y, double *z,
                          double **lat, double **lon, double **height,
                          long length, datum_type_t datum);

/****************************************************************************
  Albers Equal-Area Conic
  www.remotesensing.org/geotiff/proj_list/albers_equal_area_conic.html
****************************************************************************/
int project_albers(project_parameters_t *pps,
                   double lat, double lon, double height,
                   double *x, double *y, double *z, datum_type_t datum);
int project_albers_arr(project_parameters_t *pps,
                       double *lat, double *lon, double *height,
                       double **projected_x, double **projected_y,
                       double **projected_z, long length, datum_type_t datum);
int project_albers_inv(project_parameters_t *pps,
                       double x, double y, double z,
                       double *lat, double *lon, double *height, datum_type_t datum);
int project_albers_arr_inv(project_parameters_t *pps,
                           double *x, double *y, double *z,
                           double **lat, double **lon, double **height,
                           long length, datum_type_t datum);

// Mercator
// www.remotesensing.org/geotiff/proj_list/mercator_2sp.html
int project_mer(project_parameters_t *pps,
		double lat, double lon, double height,
		double *x, double *y, double *z, datum_type_t datum);
int project_mer_arr(project_parameters_t *pps,
		    double *lat, double *lon, double *height,
		    double **projected_x, double **projected_y,
		    double **projected_z, long length, datum_type_t datum);
int project_mer_inv(project_parameters_t *pps,
		    double x, double y, double z,
		    double *lat, double *lon, double *height, datum_type_t datum);
int project_mer_arr_inv(project_parameters_t *pps,
			double *x, double *y, double *z,
			double **lat, double **lon, double **height,
			long length, datum_type_t datum);


// Equirectangular
// www.remotesensing.org/geotiff/proj_list/equirectangular.html
int project_eqr(project_parameters_t *pps,
		double lat, double lon, double height,
		double *x, double *y, double *z, datum_type_t datum);
int project_eqr_arr(project_parameters_t *pps,
		    double *lat, double *lon, double *height,
		    double **projected_x, double **projected_y,
		    double **projected_z, long length, datum_type_t datum);
int project_eqr_inv(project_parameters_t *pps,
		    double x, double y, double z,
		    double *lat, double *lon, double *height, datum_type_t datum);
int project_eqr_arr_inv(project_parameters_t *pps,
                           double *x, double *y, double *z,
                           double **lat, double **lon, double **height,
                           long length, datum_type_t datum);

// Equidistant
int project_eqc(project_parameters_t *pps,
		double lat, double lon, double height,
		double *x, double *y, double *z, datum_type_t datum);
int project_eqc_arr(project_parameters_t *pps,
		    double *lat, double *lon, double *height,
		    double **projected_x, double **projected_y,
		    double **projected_z, long length, datum_type_t datum);
int project_eqc_inv(project_parameters_t *pps,
		    double x, double y, double z,
		    double *lat, double *lon, double *height, datum_type_t datum);
int project_eqc_arr_inv(project_parameters_t *pps,
			double *x, double *y, double *z,
			double **lat, double **lon, double **height,
			long length, datum_type_t datum);

// Sinusoidal
// www.remotesensing.org/geotiff/proj_list/sinusoidal.html
int project_sin(project_parameters_t *pps,
		double lat, double lon, double height,
		double *x, double *y, double *z);
int project_sin_arr(project_parameters_t *pps,
		    double *lat, double *lon, double *height,
		    double **projected_x, double **projected_y,
		    double **projected_z, long length);
int project_sin_inv(project_parameters_t *pps, double x, double y, double z,
		    double *lat, double *lon, double *height);
int project_sin_arr_inv(project_parameters_t *pps,
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
                    double height, double *x, double *y, double *z,
                    datum_type_t datum);

int project_pseudo_inv (project_parameters_t *pps, double x, double y,
                        double z, double *lat, double *lon, double *height,
                        datum_type_t datum);

int project_pseudo_arr (project_parameters_t *pps, double *lat, double *lon,
                        double *height, double **x, double **y, double **z,
                        long length, datum_type_t datum);

int project_pseudo_arr_inv (project_parameters_t *pps, double *x, double *y,
                            double *z, double **lat, double **lon,
                            double **height, long length, datum_type_t datum);

int utm_zone(double lon);
int test_nad27(double lat, double lon);

char *albers_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *utm_projection_description(project_parameters_t *pps, datum_type_t datum);
char *ps_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *lamaz_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *lamcc_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *mer_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *eqr_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *eqc_projection_desc(project_parameters_t *pps, datum_type_t datum);
char *sin_projection_desc(project_parameters_t *pps);

int get_tiff_data_config(TIFF *tif, short *sample_format, 
			 short *bits_per_sample, short *planar_config,
                         data_type_t *data_type, short *num_bands,
                         int *is_scanline_format, int *is_palette_color_tiff, 
			 report_level_t report_level);
int get_bands_from_citation(int *num_bands, char **band_str, int *empty, 
			    char *citation, int num_expected);

#endif
