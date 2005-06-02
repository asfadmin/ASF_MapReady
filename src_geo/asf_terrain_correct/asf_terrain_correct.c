// The essential function of this program is to take a SAR image,
// complete with information about the orbital geometry from which it
// was acquired, and use it to color the pixels of a map projected
// digital elevation model (DEM) with radad backscatter values.

#include "slant_range_image_cspline.h"

// Standard headers.
#include <complex.h>
#include <stdlib.h>

// Headers from external libraries.
#include <fftw3.h>
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_vector.h>

// Headers from custom ASF libraries and code in this package.
#include "ITRS_platform_path.h"
#include "ITRS_point.h"
#include <asf_meta.h>
#include "earth_constants.h"
#include "libasf_proj.h"
#include "map_projected_dem.h"
#include "orbital_state_vector.h"
#include "platform_path.h"
#include "progress_meter.h"
#ifdef BK_DEBUG
#  include <scratchplot.h>
#endif
#include "slant_range_image.h"
#include "dem_geom_info.h"
#include "lsm.h"

// We use the FloatImage class a lot, so we have these convenience
// macros.
#define SP(i, xi, yi, pv) float_image_set_pixel (i, xi, yi, pv)
#define GP(i, xi, yi) (float_image_get_pixel (i, xi, yi))


// Print user information to error output location and exit with a
// non-zero exit code.
static void
usage (void)
{
  g_printerr ("usage: asf_terrain_correct dem_base_name image_base_name "
	      "output_base_name\n");
  exit (EXIT_FAILURE);
}

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

// Compute a score that increases as the autocorrelation of square
// image under rotation increases.  The score is nothing standard, but
// is useful for comparing to the scores of other images.  The basic
// idea here is to use the fact that since mountains are bright on one
// side, they decorrelate nicely under rotation.  This lets us find
// the nice mountainous regions in SAR images simulated from DEMs that
// we expect will coregister well with real SAR images (since radar
// backscatter is determined mostly from terrain incidence angle only
// in steeper regions, and is dominated by land cover radiometric
// characteristics elsewhere).
static long double
rotational_autocorrelation_score (FloatImage *image)
{
  g_assert (image->size_x == image->size_y);

  FloatImage *im = image;	// Convenience alias.

  size_t sz = im->size_x;	// Convenience alias.

  // Form normalized version of the image (pixels sum to 1.0).
  FloatImage *ni = float_image_new (sz, sz);
  double pixel_sum = 0;
  size_t ii, jj;
  for ( ii = 0 ; ii < sz; ii++ ) {
    for ( jj = 0 ; jj < sz ;  jj++ ) {
      pixel_sum += GP (im, jj, ii);
    }
  }
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ;  jj++ ) {
      SP (ni, jj, ii, GP (im, jj, ii) / pixel_sum);
    }
  }

  // We will sample the images every sample_stride, rather than
  // computing correlations based on every pixel in the image.
  const int sample_stride = 3;

  // Compute unrotated autocorrelation as the sum of pixel values
  // squared, for a sparse subset of pixels.
  long double correlation_0 = 0;
  for ( ii = 0 ; ii < sz ; ii += sample_stride ) {
    for ( jj = 0 ; jj < sz ; jj += sample_stride ) {
      correlation_0 += pow (GP (ni, jj, ii), 2);
    }
  }

  // Form a version of the normalized image tile rotated clockwise
  // ninety degrees.
  FloatImage *rni = float_image_new (sz, sz);
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      SP (rni, jj, ii, GP (ni, ii, sz - jj - 1));
    }
  }

  // Compute the correlation between the rotated and unrotated
  // normalized images.
  long double correlation_90 = 0;
  for ( ii = 0 ; ii < sz ; ii += sample_stride ) {
    for ( jj = 0 ; jj < sz ; jj += sample_stride ) {
      correlation_90 += GP (ni, jj, ii) * GP (rni, jj, ii);
    }
  }  

  // We are done with the intermediate images.
  float_image_free (ni);
  float_image_free (rni);

  // The score we are interested in is hopefully ordered about the
  // same as this ratio of characteristics.
  return correlation_90 / correlation_0;
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
} image_tile_record_type;

// Compare the autocorrelation scores of *a and *b, returning -1 if
// (*a)->ras < (*b)->ras, 0 if (*a)->ras == (*b)->ras, or 1 if
// (*a)->ras > (*b)->ras.
static gint
compare_rotational_autocorrelation_scores (image_tile_record_type **a,
					   image_tile_record_type **b)
{
  if ( (*a)->ras < (*b)->ras ) {
    return -1;
  }
  else if ( (*a)->ras > (*b)->ras ) {
    return 1;
  }
  else {
    return 0;
  }
}

// Find the offset of square image b from square image a.  Images a
// and b must be equal sized.  The output values x_offset and y_offset
// are the distances that the pixels of b are offset from the pixels
// of a, in the FloatImage coordinate directions (i.e. top left pixel
// is (0, 0), and x increases as we move right and y increases as we
// move down).  The coregistration is done in memory using the fftw
// library.  On success 0 is returned, otherwise nonzero is returned.
static int
coregister (double *x_offset, double *y_offset, FloatImage *a, FloatImage *b)
{
  g_assert (a->size_x == a->size_y);
  g_assert (b->size_x == b->size_y);
  g_assert (a->size_x == b->size_x);

  size_t sz = a->size_x;	// Convenience alias.

  // FIXME: fftw supports real-data-specific trasforms which expoit
  // hermitian symetry, in-place transforms, better performance at
  // certain transform sizes, etc., but these are a pain to sort out
  // and I'm lazy.

  // Memory gauranteed to have fftw preferred alignment.
  fftw_complex *a_in = fftw_malloc (sz * sz * sizeof (fftw_complex));
  fftw_complex *b_in = fftw_malloc (sz * sz * sizeof (fftw_complex));
  fftw_complex *a_out = fftw_malloc (sz * sz * sizeof (fftw_complex));
  fftw_complex *b_out = fftw_malloc (sz * sz * sizeof (fftw_complex));

  size_t ii, jj;
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      a_in[sz * ii + jj] = GP (a, jj, ii) + I * 0.0;
      b_in[sz * ii + jj] = GP (b, jj, ii) + I * 0.0;
    }
  }

  fftw_plan a_plan = fftw_plan_dft_2d (sz, sz, a_in, a_out, FFTW_FORWARD,
				       FFTW_MEASURE);
  fftw_execute (a_plan);
  fftw_plan b_plan = fftw_plan_dft_2d (sz, sz, b_in, b_out, FFTW_FORWARD,
				       FFTW_MEASURE);
  fftw_execute (b_plan);

  // Conjugate b_out.
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      size_t ci = sz * ii + jj;   // Current index.
      b_out[ci] = creal (b_out[ci]) - cimag (b_out[ci]);
    }
  }

  fftw_complex *a_by_b_freq = fftw_malloc (sz * sz * sizeof (fftw_complex));

  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      a_by_b_freq[sz * ii + jj] = a_out[sz * ii + jj] * b_out[sz * ii + jj];
    }
  }

  fftw_complex *a_convolve_b = fftw_malloc (sz * sz * sizeof (fftw_complex));

  fftw_plan reverse_plan = fftw_plan_dft_2d (sz, sz, a_by_b_freq, a_convolve_b,
					     FFTW_BACKWARD, FFTW_MEASURE);
  fftw_execute (reverse_plan);

  FloatImage *correlation_image = float_image_new (sz, sz);

  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      SP (correlation_image, jj, ii, 
	  (float) creal (a_convolve_b[sz * ii + jj]));
    }
  }

  // FIXME: For debugging purposes, take a look at the correlation image.
  float_image_export_as_jpeg (a, "image_tiles/image_a.jpg", sz, NAN);
  float_image_export_as_jpeg (b, "image_tiles/image_b.jpg", sz, NAN);
  float_image_export_as_jpeg (correlation_image, 
			      "image_tiles/correlation_image.jpg", sz, NAN);

  // Form an image which shows the peak(s) in the correlation image.
  float cmin, cmax, cmean, csdev;
  float_image_statistics (correlation_image, &cmin, &cmax, &cmean, &csdev,
			  NAN);
  FloatImage *corr_peaks = float_image_new (sz, sz);
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      float pv = GP (correlation_image, jj, ii);
      if ( pv > cmax - csdev ) {
	SP (corr_peaks, jj, ii, pv - (cmax - csdev));
      }
      else {
	SP (corr_peaks, jj, ii, 0.0);
      }
    }
  }
  float_image_export_as_jpeg (corr_peaks, "image_tiles/corr_peaks.jpg", sz, 
			      NAN);

  // Find the actual peak of the correlation image.
  size_t peak_x = -1000, peak_y = -1000; /* Initializers reassure compiler.  */
  float peak_value = -100000000;
  for ( ii = 0 ; ii < sz ; ii++ ) {
    for ( jj = 0 ; jj < sz ; jj++ ) {
      float cv = GP (correlation_image, jj, ii);
      if ( cv > peak_value ) {
	peak_x = jj;
	peak_y = ii;
	peak_value = cv;
      }
    }
  }
  g_print ("Peak position: %ld, %ld\n", (long int) peak_x, (long int) peak_y);
  g_print ("Peak value: %g\n", peak_value);
  ssize_t ia = peak_y - 1, ib = peak_y + 1, il = peak_x - 1, ir = peak_x + 1;
  g_assert (sz <= SSIZE_MAX);
  if ( ia >= 0 && ib <= (ssize_t) sz - 1 && il >= 0 
       && ir <= (ssize_t) sz - 1 ) {
    g_print ("Neighbors: %g %g %g %g %g %g %g %g\n",
	     GP (correlation_image, il, ia),
	     GP (correlation_image, peak_x, ia),
	     GP (correlation_image, ir, ia),
	     GP (correlation_image, il, peak_y),
	     GP (correlation_image, ir, peak_y),
	     GP (correlation_image, il, ib),
	     GP (correlation_image, peak_x, ib),
	     GP (correlation_image, ir, ib));
  }


  float_image_free (correlation_image);
  float_image_free (corr_peaks);

  fftw_free (a_in);
  fftw_free (b_in);
  fftw_free (a_out);
  fftw_free (b_out);
  fftw_free (a_by_b_freq);
  fftw_free (a_convolve_b);

  // FIXME: these aren't filled in yet.  
  *x_offset = -DBL_MAX;
  *y_offset = -DBL_MAX;
  return 0;			// Return success code;
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
  // LAS dems in UTM projection.  Obviously this must change.
  g_print ("Loading reference DEM... ");
  GString *reference_dem_ddr = g_string_new (reference_dem->str);
  g_string_append (reference_dem_ddr, ".ddr");
  GString *reference_dem_img = g_string_new (reference_dem->str);
  g_string_append (reference_dem_img, ".img");
  MapProjectedDEM *dem
    = map_projected_dem_new_from_las (reference_dem->str,
				      reference_dem_img->str);
  g_print ("done.\n");

  // Take a quick look at the DEM for FIXME: debug purposes.
  //float_image_export_as_jpeg (dem->data, "dem_view.jpg", 
  //			      GSL_MAX (dem->data->size_x, dem->data->size_y), 
  //			      0.0);

  // We will need a slant range version of the image being terrain
  // corrected.
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

  FILE *tf = fopen ("test_ft", "w");
  g_assert (tf != NULL);
  float_image_freeze (sri->data, tf);
  int return_code = fclose (tf);
  g_assert (return_code == 0);
  
  tf = fopen ("test_ft", "r");
  g_assert (tf != NULL);
  FloatImage *ci = float_image_thaw (tf);
  g_assert (float_image_equals (sri->data, ci, 1e-4));

  // Take a quick look at the slant range image for FIXME: debug
  // purposes.
  //  float_image_export_as_jpeg (sri->data, "sri_view.jpg", 2000, NAN);

  // Read the image metadata.
  meta_parameters *imd = meta_read (input_meta_file->str);
  // We are expecting a ground range SAR image.
  g_assert (imd->sar->image_type == 'G');

  // If the DEM is significanly lower resolution than the SAR image,
  // we will need to generate a lower resolution version of the image
  // by averaging pixels together.  The trouble is sparsely sampling a
  // speckled thing like a SAR image gives really bad results.
  double dem_pixel_size = GSL_MAX (dem->projection_coordinates_per_x_pixel,
				   dem->projection_coordinates_per_y_pixel);
  double sar_pixel_size = GSL_MAX (imd->general->x_pixel_size, 
				   imd->general->y_pixel_size);
  // FIXME: look into this 1.5 ruls of thumb and verify that its a
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
    float_image_export_as_jpeg (sri->data, "sri_reduced_view.jpg", 2000, NAN);
  }

  int svc = imd->state_vectors->vector_count;   // State vector count.
  g_assert (svc >= 3);

  double *observation_times = g_new (double, svc);
  OrbitalStateVector **observations = g_new (OrbitalStateVector *, svc);
  
  // Load the observation times, positions, and velocities from the
  // metadata, converting the latter into Geocentric equitorial
  // inertial coordinates.  Ssing the matrix method to convert things
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
  gsl_vector *tmp = gsl_vector_alloc (3);
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
    gsl_vector_set (tmp, xi, (gsl_vector_get (itrs_vel, xi) 
			      - (EARTH_ROTATION_RATE 
				 * gsl_vector_get (itrs_pos, yi))));
    gsl_vector_set (tmp, yi, (gsl_vector_get (itrs_vel, yi) 
			      + (EARTH_ROTATION_RATE 
				 * gsl_vector_get (itrs_pos, xi))));
    gsl_vector_set (tmp, zi, gsl_vector_get (itrs_vel, zi));

    // Now we can rotate the remaining velocity back into the GEI
    // system.  FIXME: we use a slightly different (by ~10
    // microdegrees) earth angle than the code in asf_meta, so the
    // velocity ends up being different by as much as 10 m/s in some
    // components -- generally not an issue for a 15 second frame but
    // bad practice nevertheless.  We ought to change things so the
    // correct values are used everywhere.
    return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, tmp, 0.0, 
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
  gsl_vector_free (tmp);
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

#ifdef BK_DEBUG

  // For debuggin purposes, this code lets you take a look at the
  // behavior of the arc model.  There is an irritating problem here:
  // the ITRSPlatformPath model works by getting the earth angle for
  // each spline control point independently, using
  // date_time_earth_angle, and then rotating each control point from
  // GEI to ITRS (earth fixed) coordinates.  Unfortunately this
  // calculation seems to be highly sensitive at a fine scale, at
  // least for the pre-2003 definition of universal time (UT1).  This
  // creates tiny discontinuities in the earth angle as a function of
  // time, which propagate into other calculations and keep the point
  // of closest approach algorithm from converging to as tight a
  // solution as we would like.  The solution is to rewrite things to
  // use a fixed earth rotation rate over the time span of the image,
  // and just add to the earth angle increment between time steps in
  // the ITRSPlatformPath model, performing rotations bach to ITRS in
  // that class, rather than in the OrbitalStateVector class.  As an
  // interim measure, using double precision in the earth angle
  // computations helps things quite a bit, so now almost all pixels
  // converge to within 0.0001 seconds (~ 1/20 pixel in time direction
  // for ERS-1/2 images).

  double *sxvs = g_new (double, 500);
  double *syvs = g_new (double, 500);
  double *szvs = g_new (double, 500);
  double *sx_vels = g_new (double, 500);
  double *sy_vels = g_new (double, 500);
  double *sz_vels = g_new (double, 500);
  double *sx_pp = g_new (double, 500);
  double *sy_pp = g_new (double, 500);
  double *sz_pp = g_new (double, 500);
  double *stimes = g_new (double, 500);
  size_t zz;
  for ( zz = 0 ; zz < 500 ; zz++ ) {
    Vector this_pos;
    double ct = 1.7 + 0.2 * zz / 500.0;
    stimes[zz] = ct;
    ITRS_platform_path_position_at_time (pp_fixed, ct, &this_pos);
    sxvs[zz] = this_pos.x;
    syvs[zz] = this_pos.y;
    szvs[zz] = this_pos.z;
    Vector this_vel;
    ITRS_platform_path_velocity_at_time (pp_fixed, ct, &this_vel);
    sx_vels[zz] = this_vel.x;
    sy_vels[zz] = this_vel.y;
    sz_vels[zz] = this_vel.z;
    if ( zz != 0 ) {
      sx_pp[zz] = sx_vels[zz] - sx_vels[zz - 1];
      sy_pp[zz] = sy_vels[zz] - sy_vels[zz - 1];
      sz_pp[zz] = sz_vels[zz] - sz_vels[zz - 1];
    }
  }
  sx_pp[0] = sx_pp[1];
  sy_pp[0] = sy_pp[1];
  sz_pp[0] = sz_pp[1];

  // sp_basic_plot (500, stimes, sxvs, "line");
  // sp_basic_plot (500, stimes, syvs, "line");
  // sp_basic_plot (500, stimes, szvs, "line");
  // sp_basic_plot (500, stimes, sx_vels, "line");
  // sp_basic_plot (500, stimes, sy_vels, "line");
  // sp_basic_plot (500, stimes, sz_vels, "line");
  // sp_basic_plot (500, stimes, sx_pp, "line");
  // sp_basic_plot (500, stimes, sy_pp, "line");
  // sp_basic_plot (500, stimes, sz_pp, "line");

#endif // BK_DEBUG

  // Now we are ready to actually paint the DEM with backscatter.
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

      // Copy earth fixed coordinate values to the current pixel
      // target vector.
      cp_target.x = ctp->x;
      cp_target.y = ctp->y;
      cp_target.z = ctp->z;

      if ( ii == 1685 && jj == 1683 ) {
	g_print ("we're here!! x: %lg, y: %lg, z: %lg\n", cp_target.x,
		 cp_target.y, cp_target.z);
      }

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

      if ( jj == 1500 && ii == 1800 ) {
	g_print ("At pixel (1500, 1800), give or take.\n");
      }

      if ( jj == 1354 && ii == 1806 ) {
	g_print ("At pixel ( 1353, 1806).\n");
      }

      // If the DEM pixel falls in the slant range image, set the
      // discovered geometry information.
      if ( slant_range_image_contains (sri, solved_slant_range, 
				       solved_time, 1e-3) ) {
	// FIXME: is cp_height (the height above the WGS84 ellipsoid)
	// really what's wanted here?
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

  FILE *tmp = fopen ("bk_debug_dgi_freeze", "w");
  g_assert (tmp != NULL);
  dem_geom_info_freeze (dgi, tmp);
  int return_code = fclose (tmp);
  g_assert (return_code == 0);


  tmp = fopen ("bk_debug_dgi_freeze", "r");
  g_assert (tmp != NULL);
  DEMGeomInfo *dgi_revived = dem_geom_info_thaw (tmp);
  return_code = fclose (tmp);
  g_assert (return_code == 0);

  g_assert (dem_geom_info_equals (dgi, dgi_revived, 1e-10));

  } // End of dgi construction by calculation.

  // Make a quick jpeg showing the part of the DEM we believe the
  // image covers.
  /*
  FloatImage *dem_coverage = float_image_new (dem->size_x, dem->size_y);
  // For each DEM row except the first and last...
  for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {
    long int jj;
    // For every other DEM pixel except the first and last two...
    for ( jj = 0 ; (size_t) jj < dem->size_x ; jj++ ) {
      if ( dem_geom_info_get_slant_range_value (dgi, jj, ii) < 0 ) {
	SP (dem_coverage, jj, ii, GP (dem->data, jj, ii));
      }
      else {
	SP (dem_coverage, jj, ii, 2 * GP (dem->data, jj, ii));
      }
    }
  }
  float_image_export_as_jpeg (dem_coverage, "dem_coverage.jpg",
			      GSL_MAX (dem_coverage->size_x,
				       dem_coverage->size_y), NAN);
  */

  //  FloatImage * lsm = lsm_generate_mask(dgi);
  //  lsm = lsm;			/* FIXME: remove compiler reassurance.  */

  // Because the geolocation of images is often quite bad, we will
  // march throught the DEM and create a simulated SAR image, then
  // measure the offset of the simulated image from the actual slant
  // range image.  We can then subject the slant range image pixel
  // lookups used to color the DEM to the discovered offset.
  g_print ("Generating simulated SAR image... ");
  SlantRangeImage *sim_img 
    = slant_range_image_new_empty (sri->upper_left_pixel_range,
				   sri->upper_left_pixel_time,
				   sri->slant_range_per_pixel,
				   sri->time_per_pixel,
				   sri->data->size_x,
				   sri->data->size_y);

  // We can hopefully get away with ignoring the backscatter
  // contributions of the very edge facets, which saves the pain of
  // special handling for them.

  // For each DEM row except the first and last...
  for ( ii = 1 ; (size_t) ii < dem->size_y  - 1; ii++ ) {
    long int jj;
    // For every other DEM pixel except the first and last two...
    //    for ( jj = 1 ; (size_t) jj < dem->size_x - 2; jj += 2 ) {
    for ( jj = 1 ; (size_t) jj < dem->size_x - 1; jj++ ) {


      // We are interested in the four triangular facets defined by
      // the non-diagonal neighbors, so to entirely cover the image,
      // we need only considerer every other pixel in each row.  We
      // need to cover even pixels in one row, odd pixels in the next,
      // etc.  So here we compute the DEM x index being considered the
      // center of the four facet for each pixel in this row of
      // pixels.
      size_t xi;
      if ( ii % 2 == 1 ) {
	xi = jj;
      }
      else {
	xi = jj + 1;
      }

      if ( ii == 1685 && xi == 1683 ) {
	g_print ("We're here 2!\n");
      }

      // If we are in shadow this DEM pixel contributes nothing to the
      // backscatter in the simulated image.
      /* For the moment we just assume this doesn't happen.
      if ( lsm_image_mask_value_is_shadow (lsm, ii, xi) ) {
	continue;
      }
      */

      // If this DEM pixel and all its neighbors that we will be
      // considering don't all fall in the image, it contributes
      // nothing.
      if ( dem_geom_info_get_slant_range_value (dgi, xi, ii) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii - 1) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi + 1, ii) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi, ii + 1) < 0 
	   || dem_geom_info_get_slant_range_value (dgi, xi - 1, ii) < 0 ) {
	continue;
      }

      if ( xi == 1500 && ii == 1800 ) {
	g_print ("At xi == 1500, ii = 1800 in simulator.\n");
      }

      // We want to compute the normals of the upper left, upper
      // right, lower left, and lower right triangular facets, then
      // average them together to get a normal at a given dem pixel.
      // We can do this by forming vectors from the current dem pixel
      // to its four non-diagonal neighbors, then taking cross
      // products of adjacent pairs of vectors, then adding the
      // results together and normalizing.

      // Vectors for the current DEM pixel, and the pixels above, to
      // the right, below, and to the left respectively, in ITRS
      // coordinates.
      Vector c, a, r, b, l;
      vector_set (&c, float_image_get_pixel (dgi->cp_target_x, xi,  ii),
		  float_image_get_pixel (dgi->cp_target_y, xi, ii),
		  float_image_get_pixel (dgi->cp_target_z, xi, ii));
      vector_set (&a, float_image_get_pixel (dgi->cp_target_x, xi, ii - 1),
		  float_image_get_pixel (dgi->cp_target_y, xi, ii - 1),
		  float_image_get_pixel (dgi->cp_target_z, xi, ii - 1));
      vector_set (&r, float_image_get_pixel (dgi->cp_target_x, xi + 1, ii),
		  float_image_get_pixel (dgi->cp_target_y, xi + 1, ii),
		  float_image_get_pixel (dgi->cp_target_z, xi + 1, ii));
      vector_set (&b, float_image_get_pixel (dgi->cp_target_x, xi, ii + 1),
		  float_image_get_pixel (dgi->cp_target_y, xi, ii + 1),
		  float_image_get_pixel (dgi->cp_target_z, xi, ii + 1));
      vector_set (&l, float_image_get_pixel (dgi->cp_target_x, xi - 1, ii),
		  float_image_get_pixel (dgi->cp_target_y, xi - 1, ii),
		  float_image_get_pixel (dgi->cp_target_z, xi - 1, ii));

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
	vector_multiply (n, 1.0 / vector_magnitude (n));
      }
      /*
      vector_multiply (n, 1.0 / vector_magnitude (n));
      g_assert (vector_magnitude (n) > 0.5);
      */

      // Recall the slant range and imaging time for this DEM pixel.
      double sr = dem_geom_info_get_slant_range_value (dgi, xi, ii);
      double it = dem_geom_info_get_imaging_time (dgi, xi, ii);

      // Compute a vector from the current target to the platform at
      // the point of closest aproach.
      Vector pp;
      ITRS_platform_path_position_at_time (pp_fixed, it, &pp);
      Vector *target_to_poca = vector_copy (&pp);
      vector_subtract (target_to_poca, &c);

      // The incidence angle is the angle between the mean normal of
      // the facets and the line between the current target (DEM
      // pixel) and the point of closest approach.
      double incidence_angle = vector_angle (n, target_to_poca);

      // We will assume that the backscatter returned by the dirt in
      // this DEM pixel is proportional to the cosine of the angle
      // between the normal vector and the vector between the
      // platform and the target, i.e. the return is proportional to
      // the cosine of the local incidence angle at the point of
      // closest approach.
      
      // FIXME: for the moment we exclude backslopes this way, since
      // the shadow mask isn't finished yet.
      if ( !(incidence_angle > M_PI) ) {
	if ( slant_range_image_contains (sim_img, sr, it, 1e-3) ) {
	  slant_range_image_add_energy (sim_img, sr, it, 
					cos (incidence_angle));
	}
      }

      vector_free (uln);
      vector_free (urn);
      vector_free (lrn);
      vector_free (lln);
      vector_free (n);
      vector_free (target_to_poca);
    }
  }
  g_print ("done.\n");

  // Take a look at the simulated image for (FIXME) debug purposes.
  float_image_export_as_jpeg (sim_img->data, "sim_img.jpg",
			      GSL_MIN (sim_img->data->size_x,
				       sim_img->data->size_y), NAN);

  const int tile_size = 100;
  GPtrArray *image_tile_records = g_ptr_array_new ();
  for ( ii = 0 ; (size_t) ii < sim_img->data->size_y / tile_size ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; (size_t) jj < sim_img->data->size_x / tile_size ; jj++ ) {
      FloatImage *cst = float_image_new_subimage (sim_img->data, 
						  jj * tile_size,
						  ii * tile_size, tile_size,
						  tile_size);
      FloatImage *cit = float_image_new_subimage (sri->data, jj * tile_size,
						  ii * tile_size, tile_size,
						  tile_size);
      image_tile_record_type *itr = g_new (image_tile_record_type, 1);
      itr->ras = rotational_autocorrelation_score (cst);
      itr->sim_img = cst;
      itr->sri_img = cit;
      g_ptr_array_add (image_tile_records, itr);
    }
  }

  g_ptr_array_sort (image_tile_records, 
		    (GCompareFunc) compare_rotational_autocorrelation_scores);

  for ( ii = 0 ; (size_t) ii < image_tile_records->len ; ii++ ) {
    GString *ctn = g_string_new ("image_tiles/image_tile_");
    g_string_append_printf (ctn, "%d.jpg", (int) ii);
    FloatImage *csimt = ((image_tile_record_type *) 
			 g_ptr_array_index (image_tile_records, ii))->sim_img;
    float min, max, mean, sdev;
    float_image_statistics (csimt, &min, &max, &mean, &sdev, NAN);
    if ( min != max ) {
      float_image_export_as_jpeg (csimt, ctn->str, 
				  GSL_MAX (csimt->size_x, csimt->size_y), 
				  NAN);
    }
    g_string_free (ctn, TRUE);
  }

  for ( ii = 0 ; (size_t) ii < image_tile_records->len ; ii++ ) {
    double x_offset, y_offset;
    image_tile_record_type *itr = g_ptr_array_index (image_tile_records, ii);
    int return_code 
      = coregister (&x_offset, &y_offset, itr->sim_img, itr->sri_img);
    g_assert (return_code == 0);
  }

  g_print("Generating layover/shadow mask... \n");
  FloatImage * lsm = lsm_generate_mask(dgi);
  lsm = lsm;
  g_print("done\n");
  
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
  error_code = c_putddr (output_ddr_file_base_name->str, &output_ddr);
  g_assert (error_code == 0);
  
  g_print ("Saving painted DEM as raw data... ");
  float_image_store (pd, output_data_file->str,
		     FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_print ("done.\n");

  // FIXME: this debugging schlop can safely be removed.
  /*
  FloatImage *volcano = float_image_new_subimage (pd, 2100, 3100, 300, 300);
  float_image_export_as_jpeg (volcano, "volcano.jpg",
			      GSL_MAX (volcano->size_x, volcano->size_y), 0.0);
  */

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
