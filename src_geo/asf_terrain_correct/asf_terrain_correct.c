// Placeholder file for the moment.

#include <assert.h>
#include <stdlib.h>

#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_vector.h>

#include "ITRS_platform_path.h"
#include "ITRS_point.h"
#include <asf_meta.h>
#include "dem.h"
#include "earth_constants.h"
#include "libasf_proj.h"
#include "orbital_state_vector.h"
#include "platform_path.h"
#include "slant_range_image.h"

typedef struct {
  Vector *target;
  ITRSPlatformPath *pp;
  //  ITRSPlatformPath *pp;
} target_distance_params;

// Range function we want to minimize.
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

int
main (int argc, char **argv)
{
  assert (argc == 4);

  GString *reference_dem = g_string_new (argv[argc - 3]);
  reference_dem = reference_dem; /* Remove this compiler reassurance.  */

  GString *input_meta_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_meta_file, ".meta");
  GString *input_data_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_data_file, ".img");

  // A small test DEM custom made by Joanne.  The number from the LAS
  // data descriptor record are wired in here.
  const size_t tdsx = 700, tdsy = 220; /* Test DEM size x and size y.  */
  FloatImage *td 
    = (float_image_new_from_file_with_sample_type 
       (tdsx, tdsy, "test_data/dem_over_delta/cut1.img",
	0, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN,
	FLOAT_IMAGE_SAMPLE_TYPE_SIGNED_TWO_BYTE_INTEGER));
  float_image_export_as_jpeg (td, "test_dem.jpg", 4000);
  // Test DEM ul x and y projection coordinates.
  //  const double td_ul_x = 4.00632767000000E+05;
  //  const double td_ul_y = 1.57235253300000E+06;
  // Test DEM per x and per y projection coordinates per pixel.
  //  const double td_px = 60;
  //  const double td_py = -60;
  project_parameters_t projection_parameters;
  const double radians_per_degree = M_PI / 180;
  projection_parameters.albers.std_parallel1 = 55 * radians_per_degree;
  projection_parameters.albers.std_parallel2 = 65 * radians_per_degree;
  projection_parameters.albers.center_meridian = -154 * radians_per_degree;
  projection_parameters.albers.orig_latitude = 50 * radians_per_degree;
  projection_parameters.albers.false_easting = 0;
  projection_parameters.albers.false_northing = 0;

  // Load the reference DEM.
  //  Dem *dem = dem_new_from_file (reference_dem->str);
  //  float_image_export_as_jpeg (dem->float_image, "dem_view.jpg",
  //			      GSL_MAX (dem->float_image->size_x,
  //				       dem->float_image->size_y));
  //  dem = dem;		/* FIXME: remove compiler reassurance.  */

  // We will need a slant range version of the image being terrain
  // corrected.
  SlantRangeImage *sri 
    = slant_range_image_new_from_ground_range_image (input_meta_file->str,
						     input_data_file->str);
  
  meta_parameters *imd = meta_read (input_meta_file->str);

  int svc = imd->state_vectors->vector_count;   // State vector count.
  g_assert (svc >= 3);

  double *observation_times = g_new (double, svc);
  OrbitalStateVector **observations = g_new (OrbitalStateVector *, svc);
  
  // Load the observation times, positions, and velocities from the
  // metadata, converting the latter into Geocentric equitorial
  // inertial coordinates.
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
    // Otherwise, just add thee difference in observations times to
    // the previous date.
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

  /*
  observations[1] = orbital_state_vector_new (-1069.806762695312500e3,
					      3105.852783203125000e3,
					      6365.356445312500000e3,
					      252.925354003906250,
					      6718.875000000000000,
					      -3229.095458984375000);
  observations[2] = orbital_state_vector_new (-1067.813598632812500e3,
					      3157.776855468750000e3,
					      6340.146484375000000e3,
					      261.901123046875000,
					      6692.574218750000000,
					      -3282.445312500000000);
  */

  // Quick hack to check how differenc propagation is from linear
  // interpolation.
  double dt = observation_times[2] - observation_times[1];
  Vector *sv = vector_copy (observations[2]->position);
  vector_subtract (sv, observations[1]->position);
  vector_multiply (sv, 0.5);
  Vector *mpbi = vector_copy (observations[1]->position);
  vector_add (mpbi, sv);
  OrbitalStateVector *tmp2 = orbital_state_vector_copy (observations[1]);
  orbital_state_vector_propagate (tmp2, dt / 2.0);
  Vector *ev = vector_copy (mpbi);
  vector_subtract (ev, tmp2->position);
  double error_magnitude = vector_magnitude (ev);
  printf ("error magnitude (linear interpolation versus propagation): %lf\n",
	  error_magnitude);

  // Number of control points to use for the cubic splines that
  // approximate the satellite motion in the ITRSPlatformPath.
  const int cpc = 1000;
  // Guard time in seconds to add on either side of the first and last
  // observations.  This will save us in case the point of closest
  // approach to some pixel is actually outside the time window for
  // which we are provided state vectors. (though cleanup of some sort
  // will still have to be done).
  const double gt = 2.0;
  DateTime *base_date = date_time_new (imd->state_vectors->year,
				       imd->state_vectors->julDay,
				       imd->state_vectors->second,
				       UTC);

  // Create orbital arc model.
  ITRSPlatformPath *pp_fixed 
    = ITRS_platform_path_new (cpc, observation_times[0] - gt,
  			      observation_times[svc - 1] + gt,
  			      svc, base_date, observation_times, observations);

  double target_point_albers_x;
  double target_point_albers_y;
  int return_code = project_albers (&projection_parameters, 
				    63.80514 * M_PI / 180.0,
				    -145.006 * M_PI / 180.0,
				    &target_point_albers_x,
				    &target_point_albers_y);
  g_assert (return_code == TRUE);

  // FIXME: This is a test point for a single location in delta
  // junction.  Eventually a computation like this will have to be
  // done for evey pixel in the image.
  ITRSPoint *target_point 
    = ITRS_point_new_from_geodetic_lat_long_height (63.80514 * M_PI / 180.0,
						    -145.006 * M_PI / 180.0,
						    448.4);
  //  double target_height_according_to_dem 
  //    = dem_get_height (dem, 63.80514 * M_PI / 180.0, -145.006 * M_PI / 180.0);
  //  printf ("target height according to dem: %lf\n",
  //	  target_height_according_to_dem);
  Vector *target = vector_new (target_point->x, target_point->y, 
			       target_point->z);

  // Find the time of the point of closest approach for this pixel.
  int status;   // Status of the solver.
  // Current iteration, maximum number of iterations.
  int iteration = 0, max_iterations = 100;  
  const gsl_min_fminimizer_type *mimimizer_type = gsl_min_fminimizer_brent;
  gsl_min_fminimizer *minimizer = gsl_min_fminimizer_alloc (mimimizer_type);
  gsl_function distance_function; 
  distance_function.function = &target_distance;
  target_distance_params tdp;
  tdp.target = target;
  tdp.pp = pp_fixed;
  distance_function.params = &tdp;

  // Start and end of range in which to search for minimum, and
  // initial guess for minimum (we will use the middle of the range as
  // an initial guess for the minima).  These are updated dynamically
  // as we improve our estimation.
  double sor = observation_times[0] - gt;
  double eor = observation_times[svc - 1] + gt;
  double min = sor + eor / 2.0;
  gsl_min_fminimizer_set (minimizer, &distance_function, min, sor, eor);

  printf ("using %s method\n",
	  gsl_min_fminimizer_name (minimizer));
  
  printf ("%5s [%9s, %9s] %9s %9s\n",
	  "iter", "lower", "upper", "min", "err(est)");
  
  printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
	  iteration, sor, eor, min, eor - sor);

  do {
    iteration++;
    status = gsl_min_fminimizer_iterate (minimizer);
    
    min = gsl_min_fminimizer_x_minimum (minimizer);
    sor = gsl_min_fminimizer_x_lower (minimizer);
    eor = gsl_min_fminimizer_x_upper (minimizer);
    
    status = gsl_min_test_interval (sor, eor, 0.001, 0.0);
    
    if (status == GSL_SUCCESS)
      printf ("Converged:\n");
    
    printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
    	    iteration, sor, eor, min, eor - sor);
  }
  while (status == GSL_CONTINUE && iteration < max_iterations);
  
  // We need to have the convergence work.  
  assert (status == GSL_SUCCESS);

  printf ("Imaging time for CR1: %lf +/- %lf\n", min, eor - sor);
  
  double time_of_cr_pixel = meta_get_time (imd, 4293, 1933);
  printf ("Solved time: %lf,  meta_get_time: %lf\n", min, time_of_cr_pixel);

  Vector poca;
  ITRS_platform_path_position_at_time (pp_fixed, min, &poca);
  Vector *poca_to_target = vector_copy (&poca);
  vector_subtract (poca_to_target, target);
  double solved_sr = vector_magnitude (poca_to_target);
  double slant_range_of_cr_pixel = meta_get_slant (imd, 4293, 1933);
  printf ("Solved slant range: %lf, meta_get_slant: %lf\n",
	  solved_sr, slant_range_of_cr_pixel);

  double cr_reflectivity
    = slant_range_image_sample (sri, time_of_cr_pixel, slant_range_of_cr_pixel,
				FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR);
  printf ("cr_reflectivity: %lf\n", cr_reflectivity);


  // Here we take a crack at actually painting a DEM.

  FloatImage *pd = float_image_new (tdsx, tdsy);   // Backscatter painted DEM.
  double *x_buffer = g_new (double, tdsx);
  double *y_buffer = g_new (double, tdsx);
  double *lats = g_new (double, tdsx);
  double *lons = g_new (double, tdsx);

  for ( ii = 0 ; (size_t) ii < tdsy ; ii++ ) {

    // Get the latitude and longitude of each pixel in this row.
    size_t jj;
    for ( jj = 0 ; jj < tdsx ; jj++ ) {
      x_buffer[jj] = target_point_albers_x;
      y_buffer[jj] = target_point_albers_y;
      //      x_buffer[jj] = td_ul_x + jj * td_px;
      //      y_buffer[jj] = td_ul_y + ii * td_py;
    }
    g_assert (tdsx < LONG_MAX);
    return_code = project_albers_arr_inv (&projection_parameters,
					  x_buffer, y_buffer, &lats, 
					  &lons, tdsx);
    g_assert (return_code == TRUE);

    // Find the closest point of approach for each DEM pixel, look up
    // the corresponding backscatter value from the slant range image,
    // and use it to paint the DEM.
    Vector cp_target;		/* Current pixel target point.  */
    // We take advantage of all the settings needed are already made
    // for the test case above.
    tdp.target = &cp_target;
    for ( jj = 0 ; jj < tdsx ; jj++ ) {
      double p_lat = lats[jj];
      double p_lon = lons[jj];
      double p_height = float_image_get_pixel (td, jj, ii);
      ITRSPoint *ctp
	= ITRS_point_new_from_geodetic_lat_long_height (p_lat, p_lon, 
							p_height);
      cp_target.x = ctp->x;
      cp_target.y = ctp->y;
      cp_target.z = ctp->z;

      ITRS_point_free (ctp);

      iteration = 0;
      sor = observation_times[0] - gt;
      eor = observation_times[svc - 1] + gt;
      min = sor + eor / 2.0;
      gsl_min_fminimizer_set (minimizer, &distance_function, min, sor, eor);

      //      printf ("using %s method\n",
      //	      gsl_min_fminimizer_name (minimizer));
  
      //      printf ("%5s [%9s, %9s] %9s %9s\n",
      //	      "iter", "lower", "upper", "min", "err(est)");
      
      //      printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
      //	      iteration, sor, eor, min, eor - sor);

      do {
	iteration++;
	status = gsl_min_fminimizer_iterate (minimizer);
    
	min = gsl_min_fminimizer_x_minimum (minimizer);
	sor = gsl_min_fminimizer_x_lower (minimizer);
	eor = gsl_min_fminimizer_x_upper (minimizer);
    
	status = gsl_min_test_interval (sor, eor, 0.001, 0.0);
    
	//	if (status == GSL_SUCCESS)
	  //	  printf ("Converged:\n");
    
	  //	printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
	  //		iteration, sor, eor, min, eor - sor);
      }
      while (status == GSL_CONTINUE && iteration < max_iterations);
  
      // We need to have the convergence work.  
      assert (status == GSL_SUCCESS);

      // The resulting minimum is time in the arc model of the point
      // of closest approach.  FIXME: how to verify input images are
      // zero-doppler processed?
      double solved_time = min;

      // The slant range can be found from the distance between the
      // target and the platform at the point of closest approach.
      // FIXME: this delcaration currently shadows declaration in test case
      //      Vector poca; 
      ITRS_platform_path_position_at_time (pp_fixed, solved_time, &poca);
      // FIXME: this delcaration currently shadows declaration in test case
      //      Vector *
      poca_to_target = vector_copy (&poca);
      vector_subtract (poca_to_target, target);
      double solved_slant_range = vector_magnitude (poca_to_target);
      solved_slant_range = solved_slant_range; /* FIXME: remove. */
      vector_free (poca_to_target);
    
      // Look up the backscatter value for the found slant range and
      // time.
      float backscatter 
      	= slant_range_image_sample (sri, solved_time, solved_slant_range,
      				    FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR);

      // Set the pixel in the painted dem.
      float_image_set_pixel (pd, jj, ii, backscatter);

    }
  }

  float_image_export_as_jpeg (pd, "painted_dem.jpg", GSL_MAX (pd->size_x,
							      pd->size_y));

  exit (EXIT_SUCCESS);
}
