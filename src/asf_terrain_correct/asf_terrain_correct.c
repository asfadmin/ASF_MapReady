// The essential function of this program is to take a SAR image,
// complete with information about the orbital geometry from which it
// was acquired, and use it to color the pixels of a map projected
// digital elevation model (DEM) with radar backscatter values.

// Standard headers.
#include <complex.h>
#include <stdlib.h>

// Headers from external libraries.
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_vector.h>

// Headers from custom ASF libraries and code in this package.
#include "ITRS_platform_path.h"
#include "ITRS_point.h"
#include <asf_meta.h>
#include <asf_nan.h>
#include "earth_constants.h"
#include <libasf_proj.h>
#include "map_projected_dem.h"
#include "orbital_state_vector.h"
#include "platform_path.h"
#include "progress_meter.h"
#ifdef BK_DEBUG
#  include <scratchplot.h>
#endif
#include "slant_range_image.h"
#include "dem_geom_info.h"
#include "mask_image.h"

#define KH_DEBUG2

// Print user information to error output location and exit with a
// non-zero exit code.
static void
usage (void)
{
  g_printerr ("usage: asf_terrain_correct dem_base_name image_base_name "
	      "output_base_name\n");
  exit (EXIT_FAILURE);
}

// Convenience functions so we don't have to type long method names.
/*  commented out for now, since it doesn't appear to be used. --KH
static void
SP (FloatImage *i, ssize_t xi, ssize_t yi, float value)
{
  float_image_set_pixel (i, xi, yi, value);
}
*/
static float
GP (FloatImage *i, ssize_t xi, ssize_t yi)
{
  return float_image_get_pixel (i, xi, yi);
}

// Factors for going between degrees and radians.
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	0.0174532925199432958

// Marshalled form of arguments that need to go to a callback (see
// below).
typedef struct {
  Vector *target;
  ITRSPlatformPath *pp;
} target_distance_params;

// Range function we want to minimize.  Returns the distance in meters
// between the platform represented by ((target_distance_params *)
// params)->pp at time and target ((target_distance_params *)
// params)->arget.
static double 
target_distance (double time, void *params)
{
  Vector *target = ((target_distance_params *) params)->target;
  ITRSPlatformPath *pp = ((target_distance_params *) params)->pp;

  static Vector platform_position;
  ITRS_platform_path_position_at_time (pp, time, &platform_position);

  static Vector difference;
  vector_set (&difference, platform_position.x, platform_position.y, 
	      platform_position.z);
  vector_subtract (&difference, target);

  return vector_magnitude (&difference);
}

// Type to hold some information about a tile of a SAR image, a
// corresponding tile of a simulated image, and a score indicating the
// autocorrelation of the simulated image tile with itself under
// rotation.
typedef struct {
  // Rotational autocorrelation score (see above).
  long double ras;
  FloatImage *sim_img;		// Simulated image tile.
  FloatImage *sri_img;		// Slant range SAR image tile.
  size_t txi, tyi;		// x and y indicies of tile in image.
} image_tile_record_type;

// Determine the extent of ground range image with metadata imd in
// projection coordinates space of projection_type having
// projection_parameters, returning results in *min_x, *min_y, *max_x,
// *max_y.
static void
get_extents_in_projection_coordinate_space 
  (meta_parameters *imd, 
   MapProjectedDEM *dem,
   double *min_x, double *max_x,
   double *min_y, double *max_y)
{
  // We are expecting a ground range SAR image.
  g_assert (imd->sar->image_type == 'G');

  // Determine the projection function to use to convert lat/longs to
  // map projection coordinates.
  int (*project_arr) (project_parameters_t *pps, double *lat, double *lon,
		      double **projected_x, double **projected_y, 
		      long length);
  switch ( dem->projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    project_arr = project_utm_arr;
    break;
  case POLAR_STEREOGRAPHIC:
    project_arr = project_ps_arr;
    break;
  case ALBERS_EQUAL_AREA:
    project_arr = project_albers_arr;
    break;
  case LAMBERT_CONFORMAL_CONIC:
    project_arr = project_lamcc_arr;
    break;
  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    project_arr = project_lamaz_arr;
    break;
  default:
    project_arr = NULL;
    g_assert_not_reached ();
    break;
  }

  *min_x = DBL_MAX;
  *max_x = -DBL_MAX;
  *min_y = DBL_MAX;
  *max_y = -DBL_MAX;

  // Input image dimensions in pixels in x and y directions.
  size_t ii_size_x = imd->general->sample_count;
  size_t ii_size_y = imd->general->line_count;

  // Index variables.
  size_t ii, jj;

  // We will need to find the lat/long of every image edge pixel at
  // both the minimum and maximum height present in the DEM, to be
  // sure we don't miss any portion of the image due to the height
  // argument required by meta_get_latLon.
  double min_height = DBL_MAX, max_height = -DBL_MAX;
  for ( ii = 0 ; ii < dem->data->size_y ; ii++ ) {
    for ( jj = 0 ; jj < dem->data->size_x ; jj++ ) {
      float ch = GP (dem->data, jj, ii);
      if ( ch < min_height ) { min_height = ch; }
      if ( ch > max_height ) { max_height = ch; }
    }
  }

  // The actual number of edge points of the ground range image the
  // projection coordinates of which we will be considereing.  We need
  // twice as many lat/lon storage positions as we have edge pixels
  // because we will be computing the lat lon for each image edge
  // pixel at both the minimum and maximum elevations found in the
  // DEM.
  size_t edge_point_count = 2 * (2 * ii_size_x + 2 * ii_size_y - 4);
  double *lats = g_new (double, edge_point_count);
  double *lons = g_new (double, edge_point_count);
  size_t current_edge_point = 0;
  ii = 0;
  jj = 0;
  for ( ; ii < ii_size_x - 1 ; ii++ ) {
    meta_get_latLon (imd, (double) jj, (double) ii, min_height, 
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double) jj, (double) ii, max_height, 
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  for ( ; jj < ii_size_y - 1 ; jj++ ) {
    meta_get_latLon (imd, (double)jj, (double)ii, min_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double)jj, (double)ii, max_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  for ( ; ii > 0 ; ii-- ) {
    meta_get_latLon (imd, (double)jj, (double)ii, min_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double)jj, (double)ii, max_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  for ( ; jj > 0 ; jj-- ) {
    meta_get_latLon (imd, (double)jj, (double)ii, min_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
    meta_get_latLon (imd, (double)jj, (double)ii, max_height,
		     &(lats[current_edge_point]), 
		     &(lons[current_edge_point]));
    lats[current_edge_point] *= DEG_TO_RAD;
    lons[current_edge_point] *= DEG_TO_RAD;
    current_edge_point++;
  }
  g_assert (current_edge_point == edge_point_count);

  // Pointers to arrays of projected coordinates to be filled in.
  // The projection function will allocate this memory itself.
  double *x = NULL, *y = NULL;
  x = y = NULL;
  // Project all the edge pixels.
  int return_code = project_arr (&(dem->projection_parameters), lats, lons, &x,
				 &y, edge_point_count);
  g_assert (return_code == TRUE);
  // Find the extents of the image in projection coordinates.
  for ( ii = 0 ; ii < edge_point_count ; ii++ ) {
    if ( x[ii] < *min_x ) { *min_x = x[ii]; }
    if ( x[ii] > *max_x ) { *max_x = x[ii]; }
    if ( y[ii] < *min_y ) { *min_y = y[ii]; }
    if ( y[ii] > *max_y ) { *max_y = y[ii]; }
  }
  
  free (y);
  free (x);
  g_free (lons);
  g_free (lats);
}

static Vector *
calculate_geometry(Vector *n, ITRSPlatformPath *pp_fixed, Vector *c, 
		   meta_parameters *imd, double it, 
		   double *look_angle, double *slope, double *incidence_angle)
{
#ifdef KH_DEBUG
  ITRSPoint * cp_target = ITRS_point_new(c->x, c->y, c->z);
  double cp_target_lat, cp_target_lon;
  ITRS_point_get_geodetic_lat_long(cp_target, &cp_target_lat,
				   &cp_target_lon);
  double line, samp;
  meta_get_lineSamp(imd, cp_target_lat * R2D, cp_target_lon * R2D, 0,
		    &line, &samp);
//  double o_look_angle = meta_look(imd, line, samp);
  it = meta_get_time(imd, line, samp);
//  meta_slant = meta_get_slant(imd, line, samp);
#else
  imd = imd;
#endif

  // Compute a vector from the current target to the platform at
  // the point of closest aproach.
  Vector poca;
  ITRS_platform_path_position_at_time (pp_fixed, it, &poca);
  Vector *target_to_poca = vector_copy (&poca);
  vector_subtract (target_to_poca, c);

  // compute some more stuff
  ITRSPoint *sat_loc = ITRS_point_new(poca.x, poca.y, poca.z);
  double poca_lat, poca_lon;
  ITRS_point_get_geodetic_lat_long (sat_loc, &poca_lat, &poca_lon);
  ITRS_point_free (sat_loc);

  ITRSPoint *poca0 =
    ITRS_point_new_from_geodetic_lat_long_height(poca_lat, poca_lon, 0);
  
  Vector *poca_to_poca0 = vector_new (poca0->x, poca0->y, poca0->z);
  vector_subtract (poca_to_poca0, &poca);

  // double satellite_height = vector_magnitude(poca_to_poca0);
  Vector * projected_normal =
    vector_project (n, target_to_poca, poca_to_poca0);

  Vector *vertical = vector_copy(poca_to_poca0);
  vector_multiply(vertical, -1.0);
  *slope = vector_angle(projected_normal, vertical);

  Vector * target_to_poca0 = vector_new(poca0->x, poca0->y, poca0->z);
  vector_subtract (target_to_poca0, c);
      
  // negative slope means back-facing
  if (vector_angle(target_to_poca0, projected_normal) >= M_PI / 2.0)
    *slope = - *slope;

  *look_angle = M_PI - vector_angle(target_to_poca, poca_to_poca0);

  vector_free(vertical);
  vector_free(projected_normal);
  vector_free(poca_to_poca0);
  ITRS_point_free(poca0);
  
  // The incidence angle is the angle between the mean normal of
  // the facets and the line between the current target (DEM
  // pixel) and the point of closest approach.
  *incidence_angle = vector_angle (n, target_to_poca);

  vector_normalize(target_to_poca0);
  return target_to_poca0;
}

static Vector *
compute_normal_at(DEMGeomInfo *dgi, size_t jj, size_t ii)
{
  // Vectors for the current DEM pixel, and the pixels above, to
  // the right, below, and to the left respectively, in ITRS
  // coordinates.
  Vector c, a, r, b, l;
  vector_set (&c, float_image_get_pixel (dgi->cp_target_x, jj,  ii),
	      float_image_get_pixel (dgi->cp_target_y, jj, ii),
	      float_image_get_pixel (dgi->cp_target_z, jj, ii));
  vector_set (&a, float_image_get_pixel (dgi->cp_target_x, jj, ii - 1),
	      float_image_get_pixel (dgi->cp_target_y, jj, ii - 1),
	      float_image_get_pixel (dgi->cp_target_z, jj, ii - 1));
  vector_set (&r, float_image_get_pixel (dgi->cp_target_x, jj + 1, ii),
	      float_image_get_pixel (dgi->cp_target_y, jj + 1, ii),
	      float_image_get_pixel (dgi->cp_target_z, jj + 1, ii));
  vector_set (&b, float_image_get_pixel (dgi->cp_target_x, jj, ii + 1),
	      float_image_get_pixel (dgi->cp_target_y, jj, ii + 1),
	      float_image_get_pixel (dgi->cp_target_z, jj, ii + 1));
  vector_set (&l, float_image_get_pixel (dgi->cp_target_x, jj - 1, ii),
	      float_image_get_pixel (dgi->cp_target_y, jj - 1, ii),
	      float_image_get_pixel (dgi->cp_target_z, jj - 1, ii));
  
  // We now change the vectors to the neighbors st instead of
  // pointing from the ITRS origin to the target DEM pixel, they
  // point from the target DEM pixel to the appropriate neighbor
  // DEM pixel.
  vector_subtract (&a, &c);
  vector_subtract (&r, &c);
  vector_subtract (&b, &c);
  vector_subtract (&l, &c);
  
  // Compute the normal vectors for each of the facets.
  Vector *uln = vector_cross (&a, &l);
  Vector *urn = vector_cross (&r, &a);
  Vector *lrn = vector_cross (&b, &r);
  Vector *lln = vector_cross (&l, &b);
  
  Vector *n = vector_copy (uln);
  vector_add (n, urn);
  vector_add (n, lrn);
  vector_add (n, lln);
  
  if ( vector_magnitude (n) < 0.00000001 ) {
    g_warning ("tiny normal vector magnitude at DEM pixel %ld, %ld\n",
	       (long int) jj, (long int) ii);
    g_print ("c: %lf %lf %lf  \n"
	     "a: %lf %lf %lf  \n"
	     "r: %lf %lf %lf  \n"
	     "b: %lf %lf %lf  \n"
	     "l: %lf %lf %lf  \n",
	     c.x, c.y, c.z,
	     a.x, a.y, a.z,
	     r.x, r.y, r.z,
	     b.x, b.y, b.z,
	     l.x, l.y, l.z);
  }
  else {
    vector_normalize (n);
  }
  
  vector_free (uln);
  vector_free (urn);
  vector_free (lrn);
  vector_free (lln);

  return n;
}

static Vector *
compute_pixel_offsets_to_satellite(size_t size_x, size_t size_y,
				   meta_parameters *imd, DEMGeomInfo *dgi,
				   ITRSPlatformPath *pp_fixed)
{
  size_t center_x = size_x / 2;
  size_t center_y = size_y / 2;

  Vector *c = dem_geom_info_get_cp_target (dgi, center_y, center_x);
  Vector *c2 = dem_geom_info_get_cp_target (dgi, center_y + 25, center_x + 25);
  vector_subtract(c2, c);
  double f = vector_magnitude(c2);

  Vector *n = compute_normal_at (dgi, center_y, center_x);
  double it =
    dem_geom_info_get_imaging_time (dgi, center_y, center_x);

  double look_angle, slope, incidence_angle;
  Vector *d =
    calculate_geometry(n, pp_fixed, c, imd, it,
		       &look_angle, &slope, &incidence_angle);

  vector_normalize (d);
  vector_multiply(d, f);

  Vector *p = vector_copy(c);
  vector_add(p, d);

  ITRSPoint * cp_target = ITRS_point_new(p->x, p->y, p->z);

  double cp_target_lat, cp_target_lon;
  ITRS_point_get_geodetic_lat_long(cp_target, &cp_target_lat, &cp_target_lon);

  double line2, samp2;
  meta_get_lineSamp(imd, cp_target_lat * R2D, cp_target_lon * R2D,
		    0, &line2, &samp2);
  ITRS_point_free(cp_target);

  cp_target = ITRS_point_new(c->x, c->y, c->z);
  ITRS_point_get_geodetic_lat_long(cp_target, &cp_target_lat, &cp_target_lon);
  ITRS_point_free(cp_target);

  double line, samp;
  meta_get_lineSamp(imd, cp_target_lat * R2D, cp_target_lon * R2D,
		    0, &line, &samp);

  Vector *v = vector_new(samp - samp2, line - line2, 0);
  vector_normalize(v);

  vector_free(c);
  vector_free(c2);
  vector_free(n);
  vector_free(d);
  vector_free(p);

  return v;
}

// Main program.
int
main (int argc, char **argv)
{
  // Three arguments are required.
  if ( argc != 4 ) {
    usage ();
  }

  // Get the reference DEM base name argument.
  GString *reference_dem = g_string_new (argv[argc - 3]);
  reference_dem = reference_dem; /* Remove this compiler reassurance.  */

  // Form the names of the input data and metadata files by adding
  // extensions to the image_base_name argument.
  GString *input_meta_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_meta_file, ".meta");
  GString *input_data_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_data_file, ".img");

  // Form the names of the output metadata and data files by adding
  // extensions to the output_base_name argument.
  GString *output_meta_file = g_string_new (argv[argc - 1]);
  g_string_append_printf (output_meta_file, ".meta");
  GString *output_ddr_file_base_name = g_string_new (argv[argc - 1]);
  GString *output_data_file = g_string_new (argv[argc - 1]);
  g_string_append_printf (output_data_file, ".img");
  GString *output_jpeg_file = g_string_new (argv[argc - 1]);
  g_string_append_printf (output_jpeg_file, ".jpg");

  // Load the reference DEM.  FIXME: at the moment we can only handle
  // LAS dems in UTM projection, as provided by Joanne Groves
  // Obviously this must change.
  g_print ("Loading reference DEM... ");
  GString *reference_dem_ddr = g_string_new (reference_dem->str);
  g_string_append (reference_dem_ddr, ".ddr");
  GString *reference_dem_img = g_string_new (reference_dem->str);
  g_string_append (reference_dem_img, ".img");
  MapProjectedDEM *dem
    = map_projected_dem_new_from_las (reference_dem->str,
				      reference_dem_img->str);
  g_print ("done.\n");

  // We will need a slant range version of the image being terrain
  // corrected.  Defining BK_DEBUG will cause the program to try to
  // use a serialized version of the slant range image, to save the
  // time otherwise needed to load it.  This speeds debgging.
#ifndef BK_DEBUG
  g_print ("Loading SAR image and converting to slant range... ");
  SlantRangeImage *sri 
    = slant_range_image_new_from_ground_range_image (input_meta_file->str,
    						     input_data_file->str);
  g_print ("done.\n");
#else
  SlantRangeImage *sri;
  if ( g_file_test ("bk_debug_sri_freeze", G_FILE_TEST_EXISTS) ) {
    g_print ("Loading serialized slant range image file... ");
    sri = slant_range_image_thaw ("bk_debug_sri_freeze");
    g_print ("done.\n");
  }
  else {
    g_print ("Loading SAR image and converting to slant range... ");    
    sri = slant_range_image_new_from_ground_range_image (input_meta_file->str,
							 input_data_file->str);
    g_print ("done.\n");
    g_print ("Serializing SAR slant range image for future use... ");
    slant_range_image_freeze (sri, "bk_debug_sri_freeze");
    g_print ("done.\n");
  }
#endif

  // Take a quick look at the slant range image for FIXME: debug
  // purposes.
  float_image_export_as_jpeg (sri->data, "sri.jpg", 2000, NAN);

  char cmd[256];
  float_image_store(sri->data, "sri.img",
		    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  sprintf(cmd, "makeddr sri.ddr %i %i float", sri->data->size_x,
	  sri->data->size_y);
  system(cmd);

  // Read the image metadata.
  meta_parameters *imd = meta_read (input_meta_file->str);
  // We are expecting a ground range SAR image.
  g_assert (imd->sar->image_type == 'G');

  // We will essentially be coloring the DEM with radar backscatter
  // values.  But we won't need to worry about portions of the DEM for
  // which we don't have corresponding imagery.  So here we determine
  // the part of the DEM which is covered by the image and crop off
  // the parts of the DEM that we don't need.
  double min_x, max_x, min_y, max_y; // Projection coordinate range endpoints.
  get_extents_in_projection_coordinate_space (imd, dem, &min_x, &max_x,
					      &min_y, &max_y);
  MapProjectedDEM *tmp_dem = map_projected_dem_new_subdem (dem, min_x, max_x,
							   min_y, max_y);
  map_projected_dem_free (dem);
  dem = tmp_dem;

  // Take a look at the cropped DEM.
  float_image_export_as_jpeg (dem->data, "cropped_dem.jpg", 
			      GSL_MAX (dem->data->size_x, dem->data->size_y),
			      NAN);

  // If the DEM is significanly lower resolution than the SAR image,
  // we will need to generate a lower resolution version of the image
  // by averaging pixels together.  The trouble is sparsely sampling a
  // speckled thing like a SAR image gives really bad results.
  double dem_pixel_size = GSL_MAX (dem->projection_coordinates_per_x_pixel,
				   dem->projection_coordinates_per_y_pixel);
  double sar_pixel_size = GSL_MAX (imd->general->x_pixel_size, 
				   imd->general->y_pixel_size);
  // FIXME: look into this 1.5 rule of thumb and verify that its a
  // decent way to go.
  if ( dem_pixel_size > 1.5 * sar_pixel_size ) {
    long int scale_factor = ceil (dem_pixel_size / sar_pixel_size);
    if ( scale_factor % 2 != 1 ) {
      scale_factor++;
    }
    g_print ("SAR image resolution is significantly higher than DEM\n"
	     "resolution.  Scaling SAR image by a factor of %ld by \n"
	     "averaging blocks of image pixels together... ", 
	     scale_factor);
    SlantRangeImage *sri_reduced
      = slant_range_image_new_from_model_scaled (sri, scale_factor);
    g_print ("done.\n");
    slant_range_image_free (sri);
    sri = sri_reduced;

    // Take a quick look at the reduced resolution slant range image
    // for FIXME: debug purposes.
    float_image_export_as_jpeg (sri->data, "sri_reduced_res_view.jpg", 2000,
				NAN);
  }

  int svc = imd->state_vectors->vector_count;   // State vector count.
  g_assert (svc >= 3);

  double *observation_times = g_new (double, svc);
  OrbitalStateVector **observations = g_new (OrbitalStateVector *, svc);
  
  // Load the observation times, positions, and velocities from the
  // metadata, converting the latter into Geocentric equitorial
  // inertial coordinates.  Using the matrix method to convert things
  // is overkill for this simple case, but it corresponds closely with
  // the way these operation are described in the literature, so we do
  // it.
  int ii;
  // International terrestrial reference system (ITRS) coordinates of
  // state vector (the form they come in in the metadata).
  gsl_vector *itrs_pos = gsl_vector_alloc (3);
  gsl_vector *itrs_vel = gsl_vector_alloc (3);
  // Corresponding geocentric equitorial inertial (GEI) coordinates of
  // state vector (the form we need in order to propagate them).
  gsl_vector *gei_pos = gsl_vector_alloc (3);
  gsl_vector *gei_vel = gsl_vector_alloc (3);
  // Earth angle rotation matrix (see below for details).
  gsl_matrix *earm = gsl_matrix_alloc (3,3);
  // Temporary vector.
  gsl_vector *vtmp = gsl_vector_alloc (3);
  DateTime *observation_date = NULL;
  for ( ii = 0 ; ii < svc ; ii++ ) {

    observation_times[ii] = imd->state_vectors->vecs[ii].time;

    // If the observation date isn't set yet (because this is our
    // first iteration of this loop), load the date and time from the
    // metadata.
    if ( observation_date == NULL ) {
      // Note that the 'julDay' field of imd->state_vectors is badly
      // misnamed (it is actually the day of year).
      observation_date = date_time_new (imd->state_vectors->year,
					imd->state_vectors->julDay,
					imd->state_vectors->second, UTC);
    }
    // Otherwise, just add the difference in observations times to the
    // previous date.
    else {
      date_time_add_seconds (observation_date, (observation_times[ii] 
						- observation_times[ii - 1]));
    }

    // Indicies of x, y, and z vector components in gsl_vector type.
    const size_t xi = 0, yi = 1, zi = 2;

    // Load position and velocity vectors in earth fixed form into
    // vectors which we can rotate.
    gsl_vector_set (itrs_pos, xi, imd->state_vectors->vecs[ii].vec.pos.x);
    gsl_vector_set (itrs_pos, yi, imd->state_vectors->vecs[ii].vec.pos.y);
    gsl_vector_set (itrs_pos, zi, imd->state_vectors->vecs[ii].vec.pos.z);
    gsl_vector_set (itrs_vel, xi, imd->state_vectors->vecs[ii].vec.vel.x);
    gsl_vector_set (itrs_vel, yi, imd->state_vectors->vecs[ii].vec.vel.y);
    gsl_vector_set (itrs_vel, zi, imd->state_vectors->vecs[ii].vec.vel.z);

    // Get the angle of the earth during this observation.
    double theta = date_time_earth_angle (observation_date);

    // Create sidereal time (earth angle) rotation matrix as described
    // in "Satellite Geodesy, 2nd Edition" by Gunter Seeber, section
    // 2.1.2, except with the angle reversed, since we are going from
    // earth fixed back to geocentric equitorial inertial (GEI)
    // coordinates.
    gsl_matrix_set (earm, 0, 0, cos (-theta));
    gsl_matrix_set (earm, 0, 1, sin (-theta));
    gsl_matrix_set (earm, 0, 2, 0.0);
    gsl_matrix_set (earm, 1, 0, -sin (-theta));
    gsl_matrix_set (earm, 1, 1, cos (-theta));
    gsl_matrix_set (earm, 1, 2, 0.0);
    gsl_matrix_set (earm, 2, 0, 0.0);
    gsl_matrix_set (earm, 2, 1, 0.0);
    gsl_matrix_set (earm, 2, 2, 1.0);

    // Perform rotation from earth fixed back to GEI coordinates.
    gsl_vector_set_zero (gei_pos);
    int return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, itrs_pos, 0.0, 
				      gei_pos);
    g_assert (return_code == GSL_SUCCESS);

    // The fixed earth velocity vectors are affected by the rotation
    // of the earth itself, so first we have to subtract this term
    // out.
    gsl_vector_set (vtmp, xi, (gsl_vector_get (itrs_vel, xi) 
			      - (EARTH_ROTATION_RATE 
				 * gsl_vector_get (itrs_pos, yi))));
    gsl_vector_set (vtmp, yi, (gsl_vector_get (itrs_vel, yi) 
			      + (EARTH_ROTATION_RATE 
				 * gsl_vector_get (itrs_pos, xi))));
    gsl_vector_set (vtmp, zi, gsl_vector_get (itrs_vel, zi));

    // Now we can rotate the remaining velocity back into the GEI
    // system.  FIXME: we use a slightly different (by ~10
    // microdegrees) earth angle than the code in asf_meta, so the
    // velocity ends up being different by as much as 10 m/s in some
    // components -- generally not an issue for a 15 second frame but
    // bad practice nevertheless.  We ought to change things so the
    // correct values are used everywhere.
    return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, vtmp, 0.0, 
				  gei_vel);
    g_assert (return_code == GSL_SUCCESS);

    // Store the result as an OrbitalStateVector instance.
    observations[ii] = orbital_state_vector_new (gsl_vector_get (gei_pos, 0),
						 gsl_vector_get (gei_pos, 1),
						 gsl_vector_get (gei_pos, 2),
						 gsl_vector_get (gei_vel, 0),
						 gsl_vector_get (gei_vel, 1),
						 gsl_vector_get (gei_vel, 2));
  }
  date_time_free (observation_date);
  gsl_vector_free (vtmp);
  gsl_matrix_free (earm);
  gsl_vector_free (gei_vel);
  gsl_vector_free (gei_pos);
  gsl_vector_free (itrs_vel);
  gsl_vector_free (itrs_pos);

  g_print ("Creating orbital arc model... ");

  // Number of control points to use for the cubic splines that
  // approximate the satellite motion in the ITRSPlatformPath.
  const int cpc = 10000;
  // Guard time in seconds to add on either side of the first and last
  // observations.  This will save us in case the point of closest
  // approach to some pixel is actually outside the time window for
  // which we are provided state vectors. (though cleanup of some sort
  // will still have to be done).
  const double gt = 0.5;
  DateTime *base_date = date_time_new (imd->state_vectors->year,
				       imd->state_vectors->julDay,
				       imd->state_vectors->second,
				       UTC);

  // Create orbital arc model.
  ITRSPlatformPath *pp_fixed 
    = ITRS_platform_path_new (cpc, observation_times[0] - gt,
  			      observation_times[svc - 1] + gt,
  			      svc, base_date, observation_times, observations);

  g_print ("done.\n");

  // Now we are ready to actually paint the DEM with backscatter.
  // FIXME: actually we aren't really ready to do that yet, since we
  // have to do coregistration to line up image and DEM somehow first.
  // How isn't worked out yet though.  We go ahead and create the
  // painted DEM (that would be right if image and DEM lined up
  // correctly) anyway (unless a frozen version of dgi is revived),
  // just so we can take a loot at it if desired.
  g_assert (dem->size_y <= LONG_MAX);

  // Backscatter painted DEM.
  FloatImage *pd = float_image_new (dem->size_x, dem->size_y);  

  // Storage for converting one row of DEM pixels to geodetic
  // lat/longs/heights.
  double *lats = g_new (double, dem->size_x);
  double *lons = g_new (double, dem->size_x);
  double *heights = g_new (double, dem->size_x);

  // Find the closest point of approach for each DEM pixel, look up
  // the corresponding backscatter value from the slant range image,
  // and use it to paint the DEM.

  // Current pixel target point, in earth fixed cartesian coordinates.
  Vector cp_target;

  // Set up the GNU Scientific Library minimizer.
  int status;   // Status of the solver.
  //  const gsl_min_fminimizer_type *mimimizer_type = gsl_min_fminimizer_brent;
  const gsl_min_fminimizer_type *mimimizer_type 
    = gsl_min_fminimizer_brent;
  gsl_min_fminimizer *minimizer = gsl_min_fminimizer_alloc (mimimizer_type);
  gsl_function distance_function;
  distance_function.function = target_distance;
  target_distance_params tdp;
  tdp.target = &cp_target;
  tdp.pp = pp_fixed;
  distance_function.params = &tdp;
  // Convergence tolerance we will try for.  This amounts to about
  // 1/20 th of a pixel in the time direction for your average ERS-1
  // or ERS-2 image.
  double tolerance = 0.0001;
  // Pixels that don't converge at least this well are extra bad, we
  // coun them separately.
  double bad_tolerance = 10 * tolerance;
  // Count of number of pixels which fail to meet the convergence
  // tolerance requirements.
  long int failed_pixel_count = 0;
  // Pixels which don't even converge to within ten times the desired
  // tolerance.
  long int extra_bad_pixel_count = 0;
  // We will keep track of the mean tolerance of the convergence of
  // the different pixels so we can report that as well.
  long double mean_tolerance = 0;

  // stats on number of iterations
  int total_iterations = 0;
  int iteration_count = 0;
  int min_iters = 200;
  int max_iters = 0;

  // We now go through the DEM and sort out a bunch of useful
  // information from the geometrical relationships of things.
  DEMGeomInfo *dgi;

  // If possible, we just restore a debugging version that we have
  // around.
  if ( g_file_test ("bk_debug_dgi_freeze", G_FILE_TEST_EXISTS) ) {
    FILE *tmp = fopen ("bk_debug_dgi_freeze", "r");
    g_assert (tmp != NULL);
    dgi = dem_geom_info_thaw (tmp);
    int return_code = fclose (tmp);
    g_assert (return_code == 0);


  } else {
    // Otherwise, we have to actually calculate a new instance.
    dgi = dem_geom_info_new(dem->size_x, dem->size_y);

  g_print ("Gathering DEM geometry information: \n");
  ProgressMeter *progress_meter 
    = progress_meter_new_with_callback (g_print, dem->size_y);

  // For each DEM row...
  for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {

    // Get the latitude and longitude of each pixel in this row.
    g_assert (ii <= SSIZE_MAX);
    map_projected_dem_get_latitudes_longitudes_heights (dem, ii, lats, lons, 
							heights);

    // The time of the point of closest approach (minimum discovered
    // by solver) for a DEM pixel.  We have this outside the loop so
    // we can use it as the starting point for solution to
    // neighboring pixels and save some iterations.
    double min = -DBL_MAX;

    size_t jj;
    for ( jj = 0 ; jj < dem->size_x ; jj++ ) {
      double cp_lat = lats[jj];   // Current pixel latitude.
      double cp_lon = lons[jj];   // Current pixel longitude.
      // Current pixel height.
      double cp_height = heights[jj];
      // Current target point in earth fixed coordinates.
      ITRSPoint *ctp
	= ITRS_point_new_from_geodetic_lat_long_height (cp_lat, cp_lon, 
							cp_height);

#ifdef KH_DEBUG
      {
	double test_lat, test_lon;
	ITRS_point_get_geodetic_lat_long(ctp, &test_lat, &test_lon);
	if (fabs(cp_lat - test_lat) > .0001 || fabs(cp_lon - test_lon) > .0001)
	  printf("Gah! %d %d %g %g %g %g\n", ii, jj, test_lat, cp_lat,
		 test_lon, cp_lon);
      }
#endif

      // Copy earth fixed coordinate values to the current pixel
      // target vector.
      cp_target.x = ctp->x;
      cp_target.y = ctp->y;
      cp_target.z = ctp->z;

      ITRS_point_free (ctp);

      // Current iteration, maximum number of iterations to try.
      int iteration = 0, max_iterations = 200;  

      double sor;   // Start of time range in which to look for minimum.
      double eor;   // End of time range in which to look for minimum.

      // If this is the first pixel in a row, we are conservative and
      // search the whole orbital arc segment for the point of closest
      // approach,
      if ( jj == 0 ) {
	sor = observation_times[0] - gt;
	eor = observation_times[svc - 1] + gt;
	min = sor + eor / 2.0;
      }
      // otherwise, we use the results from the pixel right next to
      // the current pixel as a pretty good guess where the point of
      // closest approach will fall, being sure not to let the range
      // fall out of the interval supported by the arc model.
      else {
	const double max_pixel_seperation_in_time = 0.1;
	sor = min - max_pixel_seperation_in_time;
	if ( sor < observation_times[0] - gt ) {
	  sor = observation_times[0] - gt;
	}
	eor = min + max_pixel_seperation_in_time;
	if ( eor > observation_times[0] + gt ) {
	  eor = observation_times[svc - 1] + gt;
	}
      }

      gsl_set_error_handler_off ();

      int return_code = gsl_min_fminimizer_set (minimizer, &distance_function, 
						min, sor, eor);
      // If there is no minimum in this range, it means our orbital
      // arc model doesn't cover this part of the DEM, so just set the
      // painted dem pixel to zero, set a sentinal value in the dem
      // geometry informationi record, and go on to the next pixel.
      if ( return_code == GSL_FAILURE ) {
	float_image_set_pixel (pd, jj, ii, 0.0);
	float_image_set_pixel (dgi->slant_range_value, jj, ii, -1);
	break;
      }
	
      gsl_set_error_handler (NULL);

      do {
	iteration++;
	status = gsl_min_fminimizer_iterate (minimizer);
    
	min = gsl_min_fminimizer_x_minimum (minimizer);
	sor = gsl_min_fminimizer_x_lower (minimizer);
	eor = gsl_min_fminimizer_x_upper (minimizer);
    
	status = gsl_min_test_interval (sor, eor, tolerance, 0.0);
      }
      while (status == GSL_CONTINUE && iteration < max_iterations);

      iteration_count++;
      total_iterations += iteration;
      if (iteration < min_iters) min_iters = iteration;
      if (iteration > max_iters) max_iters = iteration;

      // How close did the convergence come to perfection?
      double error = sor - eor;

      // We want to keep some statistics on how many pixels fail to
      // converge to within our desired tolerance.
      if ( status != GSL_SUCCESS ) {
	if ( fabs (sor - eor) < bad_tolerance ) {
	  failed_pixel_count++;
	}
	else {
	  extra_bad_pixel_count++;
	}
      }

      // Update our notion of the mean tolerance.
      if ( G_UNLIKELY (ii == 0 && jj == 0) ) {
	mean_tolerance = error;
      }
      else {
	// The recurence relation we use to compute the running mean
	// needs the current 1-based pixel number.
	unsigned long int pixel_number = ii * dem->size_x + jj + 1;
	mean_tolerance += (error - mean_tolerance) / pixel_number;
      }
    
      // The resulting minimum is time in the arc model of the point
      // of closest approach.  FIXME: how to verify input images are
      // zero-doppler processed?  Possible with meta->sar->deskewed.
      // Also probably need to check meta->sar->look_count and make
      // sure it's 1.
      double solved_time = min;

      // The slant range can be found from the distance between the
      // target and the platform at the point of closest approach.
      Vector poca; 
      ITRS_platform_path_position_at_time (pp_fixed, solved_time, &poca);
      Vector *poca_to_target = vector_copy (&poca);
      vector_subtract (poca_to_target, &cp_target);
      double solved_slant_range = vector_magnitude (poca_to_target);
      vector_free (poca_to_target);

      // If the DEM pixel falls in the slant range image, set the
      // discovered geometry information.
      if ( slant_range_image_contains (sri, solved_slant_range, 
				       solved_time, 1e-3) ) {
	dem_geom_info_set (dgi, jj, ii, &cp_target, solved_time, 
			   solved_slant_range, cp_height, &poca);
      }
      else {
	// otherwise, since we aren't absolutely sure we have image
	// over this portion of the DEM, set a sentinal value in the
	// geometry information to indicate this,
	float_image_set_pixel (dgi->slant_range_value, jj, ii, -1);
      }
    }

    // Print progress update every so many lines.
    const int lines_per_progress_update = 100;
    if ( (ii + 1) % lines_per_progress_update == 0 
	 || (size_t) (ii + 1) == dem->size_y ) {
      progress_meter_advance (progress_meter, 100);
    }
  }

  progress_meter_free (progress_meter);

  printf("Iteration statistics:\n Avg: %g\n Max: %d\n Min: %d\n",
	 (double) total_iterations / iteration_count,
	 max_iters, min_iters);

  FILE *tmp = fopen ("bk_debug_dgi_freeze", "w");
  g_assert (tmp != NULL);
  dem_geom_info_freeze (dgi, tmp);
  int return_code = fclose (tmp);
  g_assert (return_code == 0);
  
  // Test to see if the thaw 'ed version is is the same as the one we
  // just froze.
  tmp = fopen ("bk_debug_dgi_freeze", "r");
  g_assert (tmp != NULL);
  DEMGeomInfo *dgi_thaw_test = dem_geom_info_thaw (tmp);
  return_code = fclose (tmp);
  g_assert (return_code == 0);

  g_assert (dem_geom_info_equals (dgi, dgi_thaw_test, 0.0000001));

  } // End of dgi construction by calculation.

  // Because the geolocation of images is often quite bad, we will
  // march throught the DEM and create a simulated SAR image, then
  // measure the offset of the simulated image from the actual slant
  // range image.  We can then subject the slant range image pixel
  // lookups used to color the DEM to the discovered offset.
  g_print ("Generating simulated SAR image...\n");
  SlantRangeImage *sim_img 
    = slant_range_image_new_empty (sri->upper_left_pixel_range,
				   sri->upper_left_pixel_time,
				   sri->slant_range_per_pixel,
				   sri->time_per_pixel,
				   sri->data->size_x,
				   sri->data->size_y);

  MaskImage *mask = mask_image_new (dem->size_x, dem->size_y);
  FloatImage *angles = float_image_new (dem->size_x, dem->size_y);

  // We can hopefully get away with ignoring the backscatter
  // contributions of the very edge facets, which saves the pain of
  // special handling for them.

#ifdef KH_DEBUG
  int countem = 0;
#endif

  // For each DEM row except the first and last...
  int neg_sim_pixels = 0;
  double max_lams = -10;
  int nlayover = 0, nshadow = 0;
  g_print ("Size: %d,%d\n", dem->size_y, dem->size_x);

  // find a vector from the center of the image, to the point on
  // the ground below the satellite
  Vector *poff = compute_pixel_offsets_to_satellite(dem->size_x, dem->size_y,
						    imd, dgi, pp_fixed);

  for ( ii = 0 ; (size_t) ii < dem->size_y; ii++ ) {
    long int jj;
    // For every other DEM pixel except the first and last two...
    //    for ( jj = 1 ; (size_t) jj < dem->size_x - 2; jj += 2 ) {
    for ( jj = 0 ; (size_t) jj < dem->size_x; jj++ ) {

      size_t xi = jj;

      // If we are in shadow this DEM pixel contributes nothing to the
      // backscatter in the simulated image.
      // For the moment we just assume this doesn't happen.
      //if ( lsm_image_mask_value_is_shadow (lsm, ii, xi) ) {
      //continue;
      //}
      

      // If this DEM pixel and all its neighbors that we will be
      // considering don't all fall in the image, it contributes
      // nothing.  The image edges are a special case of this.
      if ( ii == 0 || xi == 0
	   || (size_t) ii == dem->size_y - 1 || xi == dem->size_x - 1
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii - 1) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi + 1, ii) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii + 1) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi - 1, ii) < 0 ) {
	mask_image_set_pixel_no_dem_data(mask, xi, ii);
	continue;
      }

      // We want to compute the normals of the upper left, upper
      // right, lower left, and lower right triangular facets, then
      // average them together to get a normal at a given dem pixel.
      // We can do this by forming vectors from the current dem pixel
      // to its four non-diagonal neighbors, then taking cross
      // products of adjacent pairs of vectors, then adding the
      // results together and normalizing.

      Vector *c = dem_geom_info_get_cp_target(dgi, xi, ii);
      Vector *n = compute_normal_at(dgi, xi, ii);      

      // Recall the slant range and imaging time for this DEM pixel.
      double sr = dem_geom_info_get_slant_range_value (dgi, xi, ii);
      double it = dem_geom_info_get_imaging_time (dgi, xi, ii);

      // Calculate!
      double look_angle, slope, incidence_angle;
      Vector *target_to_poca0 =
	calculate_geometry(n, pp_fixed, c, imd, it,
			   &look_angle, &slope, &incidence_angle);

      float_image_set_pixel(angles, xi, ii, slope);

      if (look_angle - slope > max_lams)
	max_lams = look_angle - slope;

      if (look_angle - slope > M_PI / 2)
      {
	++nshadow;
	mask_image_set_pixel_shadow(mask, xi, ii);

	int iii, jjj, kkk = 0;
	while (1) {
	  ++kkk;
	  iii = (int) round(ii - kkk * poff->y);
	  jjj = (int) round(xi - kkk * poff->x);

	  if (iii < 0 || iii > (int) dem->size_y - 1) break;
	  if (jjj < 0 || jjj > (int) dem->size_x - 1) break;

	  double sr_val = dem_geom_info_get_slant_range_value(dgi, jjj, iii);

	  // if we reach a dem hole, quit now
	  if (sr_val < 0)
	    break;
	  if (kkk > 200)
	    break;

	  Vector *c2 = dem_geom_info_get_cp_target(dgi, jjj, iii);      
	  Vector *n2 = compute_normal_at(dgi, jjj, iii);      

	  // Calculate!
	  double look_angle2, slope2, incidence_angle2;
	  calculate_geometry(n2, pp_fixed, c2, imd,
			     dem_geom_info_get_imaging_time (dgi, jjj, iii),
			     &look_angle2, &slope2, &incidence_angle2);

	  vector_free(c2);
	  vector_free(n2);

	  if (look_angle2 >= look_angle)
	    break;

	  if (mask_image_get_pixel(mask, jjj, iii) == MASK_NORMAL_VALUE)
	    mask_image_set_pixel(mask, jjj, iii, MASK_SHADOW_PASSIVE);
	}
      }
      else if (slope > look_angle)
      {
	++nlayover;
	mask_image_set_pixel_layover(mask, xi, ii);

	int iii, jjj, kkk = 0;
	while (1) {
	  ++kkk;
	  iii = (int) round(ii + kkk * poff->y);
	  jjj = (int) round(xi + kkk * poff->x);

	  if (iii < 0 || iii > (int) dem->size_y - 1) break;
	  if (jjj < 0 || jjj > (int) dem->size_x - 1) break;

	  double sr_val = dem_geom_info_get_slant_range_value(dgi, jjj, iii);

	  if (sr_val <= sr)
	    break;
	  if (kkk > 200)
	    break;

	  if (mask_image_get_pixel(mask, jjj, iii) == MASK_NORMAL_VALUE)
	    mask_image_set_pixel(mask, jjj, iii, MASK_LAYOVER_PASSIVE);
	}
      }

      vector_free(target_to_poca0);

      // We will assume that the backscatter returned by the dirt in
      // this DEM pixel is proportional to the cosine of the angle
      // between the normal vector and the vector between the
      // platform and the target, i.e. the return is proportional to
      // the cosine of the local incidence angle at the point of
      // closest approach.
      
      // FIXME: for the moment we exclude backslopes this way, since
      // the shadow mask isn't finished yet.
      if ( !(incidence_angle > M_PI / 2 ) ) {
	if ( slant_range_image_contains (sim_img, sr, it, 1e-3) ) {
	  if ( cos (incidence_angle) < 0 ) {
	    neg_sim_pixels++;
	  }
	  slant_range_image_add_energy (sim_img, sr, it, 
					pow (cos (incidence_angle), 2.0));
	}
      }
  
      vector_free (c);
      vector_free (n);
    }
  }
  
  g_print ("done.\n");
  g_print ("Negative simulator energy additions: %d\n", neg_sim_pixels);

  g_print ("Layover pixels: %d\n", nlayover);
  g_print ("Shadow pixels: %d\n", nshadow);

/*  Skip this for now... 
  // Change no_dem_data values to background_fill
  int bg_fill_count = 0;

  mask_image_export_as_ppm(mask, "mask1.ppm");
  for ( ii = 0 ; (size_t) ii < dem->size_y; ii++ ) {
    size_t jj = 0;
    while (mask_image_get_pixel(mask, jj, ii) == MASK_NO_DEM_DATA &&
           jj < dem->size_x - 2) {
      ++bg_fill_count;
      mask_image_set_pixel(mask, jj, ii, MASK_BACKGROUND_FILL);
      ++jj;
    }
    jj = dem->size_x - 1;
    while (mask_image_get_pixel(mask, jj, ii) == MASK_NO_DEM_DATA && jj > 0) {
      ++bg_fill_count;
      mask_image_set_pixel(mask, jj, ii, MASK_BACKGROUND_FILL);
      --jj;
    }
  }
  for ( ii = 0 ; (size_t) ii < dem->size_x; ii++ ) {
    size_t jj = 0;
    while ((mask_image_get_pixel(mask, ii, jj) == MASK_NO_DEM_DATA ||
	    mask_image_get_pixel(mask, ii, jj) == MASK_BACKGROUND_FILL) &&
           jj < dem->size_y - 2) {
      ++bg_fill_count;
      mask_image_set_pixel(mask, ii, jj, MASK_BACKGROUND_FILL);
      ++jj;
    }
    jj = dem->size_y - 1;
    while ((mask_image_get_pixel(mask, ii, jj) == MASK_NO_DEM_DATA ||
	    mask_image_get_pixel(mask, ii, jj) == MASK_BACKGROUND_FILL) &&
	    jj > 0) {
      ++bg_fill_count;
      mask_image_set_pixel(mask, ii, jj, MASK_BACKGROUND_FILL);
      --jj;
    }
  }
  g_print ("Converted %d pixels from no_dem_data to background fill.\n",
	   bg_fill_count);
*/

  // Take a look at the simulated image for (FIXME) debug purposes.
  float_image_export_as_jpeg (sim_img->data, "sim_img.jpg",
			      GSL_MIN (sim_img->data->size_x,
				       sim_img->data->size_y), NAN);

  float_image_store(sim_img->data, "sim_img.img",
		    FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  sprintf(cmd, "makeddr sim_img.ddr %i %i float", sim_img->data->size_x,
	  sim_img->data->size_y);
  system(cmd);

  float_image_store(angles, "angles.img", FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  sprintf(cmd, "makeddr angles.ddr %i %i float", angles->size_y,
	  angles->size_x);
  system(cmd);

  mask_image_export_as_ppm(mask, "mask.ppm");

  g_print ("Painting %ld DEM pixel rows with SAR image pixel values...\n",
	   (long int) dem->size_y);

  ProgressMeter *progress_meter 
    = progress_meter_new_with_callback (g_print, dem->size_y);

  // For each DEM row...
  for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {

    g_assert (ii <= SSIZE_MAX);

    size_t jj;
    // For each pixel in the row...
    for ( jj = 0 ; jj < dem->size_x ; jj++ ) {

      // Pull out previously calculated values
      double solved_slant_range =
	dem_geom_info_get_slant_range_value(dgi, jj, ii);
      double solved_time =
	dem_geom_info_get_imaging_time(dgi, jj, ii);

      // Look up the backscatter value for the found slant range and
      // time.
      float backscatter;
      if ( slant_range_image_contains (sri, solved_slant_range, 
				       solved_time, 1e-3) ) {
	backscatter 
	  = slant_range_image_sample (sri, solved_slant_range, solved_time,
				      FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC);
      }
      else {
	// We don't have image over this part of the DEM, so set to
	// the mask value (FIXME: better mask handling needed).
	backscatter = 0.0;
      }

      // Set the pixel in the painted dem.
      float_image_set_pixel (pd, jj, ii, backscatter);
    }

    // Print progress update every so many lines.
    const int lines_per_progress_update = 100;
    if ( (ii + 1) % lines_per_progress_update == 0 
	 || (size_t) (ii + 1) == dem->size_y ) {
      progress_meter_advance (progress_meter, 100);
    }
  }

  g_print ("Done painting DEM.\n");

  g_print ("Average convergence tolerance of the time of point of closest\n"
	   "approach minimization (TOPOCAM) was %Lg seconds.\n",
	   mean_tolerance);
  g_print ("For a total of %ld pixel(s), the TOPOCAM did not converge to\n"
	   "within the desired precision of %lg seconds.\n", 
	   failed_pixel_count, 
	   tolerance);
  g_print ("For a total of %ld pixel(s) the TOPOCAM did not even converge to\n"
	   "within %lg seconds.\n", extra_bad_pixel_count, 
	   bad_tolerance);

  g_print ("Log scaling the painted DEM... ");
  // Log scale the painted DEM.
  size_t jj;
  for ( ii = 0 ; ii < (int) pd->size_x ; ii++ ) {
    for ( jj = 0 ; jj < pd->size_y ; jj++ ) {
      float cp = float_image_get_pixel (pd, ii, jj);
      float opv;   // Output pixel value.	
      if ( cp <= 0 ) {
	opv = 0.0;
      }
      else {
	float_image_set_pixel (pd, ii, jj, 10 * log10 (cp));
      }
    }
  }
  g_print ("done.\n");

  g_print ("Exporting painted DEM as a JPEG image... ");
  float_image_export_as_jpeg (pd, output_jpeg_file->str, 
			      GSL_MAX (pd->size_x, pd->size_y), 0.0);
  g_print ("done.\n");

  // Generate a LAS version of the output metadata.  The only works if
  // we have a LAS DEM given as the input.  It intended for testing
  // work with Joanne.
  struct DDR output_ddr;
  lasErr error_code = c_getddr (reference_dem->str, &output_ddr);
  g_assert (error_code == 0);
  output_ddr.dtype = 4;		/* Means floating point samples.  */
  output_ddr.nl = pd->size_y;
  output_ddr.ns = pd->size_x;
  error_code = c_putddr (output_ddr_file_base_name->str, &output_ddr);
  g_assert (error_code == 0);
  
  g_print ("Saving painted DEM as raw data... ");
  float_image_store (pd, output_data_file->str,
		     FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_print ("done.\n");
  g_print ("Wrote image: %d by %d\n", pd->size_x, pd->size_y);

  // Free all the memory and resources we used.
  g_print ("Freeing resources... ");
  g_string_free (reference_dem, TRUE);
  g_string_free (input_meta_file, TRUE);
  g_string_free (input_data_file, TRUE);
  g_string_free (output_meta_file, TRUE);
  g_string_free (output_data_file, TRUE);
  g_string_free (output_jpeg_file, TRUE);
  g_string_free (reference_dem_ddr, TRUE);
  g_string_free (reference_dem_img, TRUE);
  map_projected_dem_free (dem);
  slant_range_image_free (sri);
  meta_free (imd);
  g_free (observation_times);
  for ( ii = 0 ; ii < svc ; ii++ ) {
    orbital_state_vector_free (observations[ii]);
  }
  g_free (observations);
  date_time_free (base_date);
  ITRS_platform_path_free (pp_fixed);
  float_image_free (pd);
  g_free (lats);
  g_free (lons);
  g_free (heights);
  gsl_min_fminimizer_free (minimizer);
  progress_meter_free (progress_meter);
  g_print ("done.\n");

  exit (EXIT_SUCCESS);
}
