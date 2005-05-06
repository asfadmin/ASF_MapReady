// Placeholder file for the moment.

#include <assert.h>
#include <stdlib.h>

#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_statistics_double.h>
#include <gsl/gsl_vector.h>

#include "ITRS_platform_path.h"
#include "ITRS_point.h"
#include <asf_meta.h>
#include "earth_constants.h"
#include "libasf_proj.h"
#include "map_projected_dem.h"
#include "orbital_state_vector.h"
#include "platform_path.h"
#include "scratchplot.h"
#include "slant_range_image.h"

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

  // Load the reference DEM.
  GString *reference_dem_ddr = g_string_new (reference_dem->str);
  g_string_append (reference_dem_ddr, ".ddr");
  GString *reference_dem_img = g_string_new (reference_dem->str);
  g_string_append (reference_dem_img, ".img");
  MapProjectedDEM *dem
    = map_projected_dem_new_from_las (reference_dem->str,
				      reference_dem_img->str);
  // Take a quick look at the DEM for FIXME: debug purposes.
  float_image_export_as_jpeg (dem->data, "dem_view.jpg", 2000, 0.0);

  // We will need a slant range version of the image being terrain
  // corrected.
#ifdef BK_DEBUG
  SlantRangeImage *sri 
    = slant_range_image_new_from_ground_range_image (input_meta_file->str,
						     input_data_file->str);
#else
  //  slant_range_image_freeze (sri, "test_sri_freeze");
  SlantRangeImage *sri = slant_range_image_thaw ("test_sri_freeze");
#endif
  //  FloatImage *volcano_sri = float_image_new_subimage (sri->data, 2100, 3100, 
  ///						      300, 300);
  //float_image_export_as_jpeg (volcano_sri, "volcano_sri.jpg",
  //			      GSL_MAX (volcano_sri->size_x,
  //				       volcano_sri->size_y), 0.0); 
  //g_assert (slant_range_image_equal (sri, thawed));

  // Take a quick look at the slant range image for FIXME: debug
  // purposes.
  // float_image_export_as_jpeg (sri->data, "sri_view.jpg", 2000);

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

  // FIXME: debug test case.
#ifdef BK_DEBUG
  orbital_state_vector_propagate (observations[0], -0.49);
#endif

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

  //  sp_basic_plot (500, stimes, sxvs, "line");
  //  sp_basic_plot (500, stimes, syvs, "line");
  //  sp_basic_plot (500, stimes, szvs, "line");
  //  sp_basic_plot (500, stimes, sx_vels, "line");
  //  sp_basic_plot (500, stimes, sy_vels, "line");
  //  sp_basic_plot (500, stimes, sz_vels, "line");
  //  sp_basic_plot (500, stimes, sx_pp, "line");
  //  sp_basic_plot (500, stimes, sy_pp, "line");
  //  sp_basic_plot (500, stimes, sz_pp, "line");

  // Now we are ready to actually paint the DEM with backscatter.

  // Backscatter painted DEM.
  FloatImage *pd = float_image_new (dem->size_x, dem->size_y);  

  // Storage for converting one row of DEM pixels to geodetic
  // lat/longs/heights.
  double *lats = g_new (double, dem->size_x);
  double *lons = g_new (double, dem->size_x);
  double *heights = g_new (double, dem->size_x);

  // Test crud.
  {
    map_projected_dem_get_latitudes_longitudes_heights (dem, 3201, lats, lons, 
							heights);
    //    sp_basic_plot (dem->size_x, NULL, heights, "line");

    double px, py, ph;
    map_projected_dem_get_x_y_h (dem, 2262.085, 3201.407, &px, &py, &ph);
    //    printf ("x: %lg, y: %lg, h: %lg\n", px, py, ph);
    ITRSPoint *ttp
      = ITRS_point_new_from_geodetic_lat_long_height (lats[2262], lons[2262], 
						      heights[2262]);
    target_distance_params tpparms;
    tpparms.target = vector_new (ttp->x, ttp->y, ttp->z);
    tpparms.pp = pp_fixed;
    double *times = g_new (double, dem->size_x);
    double *ranges = g_new (double, dem->size_x);
    size_t ts;
    for ( ts = 0 ; ts < dem->size_x ; ts++ ) {
      times[ts] = ts * 16.0 / dem->size_x;
      ranges[ts] = target_distance (times[ts], &tpparms);
    }

    //    sp_basic_plot (dem->size_x, times, ranges, "line");
  }

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

  double *solved_time_plot = g_new (double, 100);

  // For each DEM row...
  //FIXME: remove  for ( ii = 3201 ; (size_t) ii < dem->size_y ; ii++ ) {
  // for ( ii = 2212 ; (size_t) ii < dem->size_y ; ii++ ) {
  for ( ii = 1500 ; (size_t) ii < dem->size_y ; ii++ ) {
      //for ( ii = 0 ; (size_t) ii < dem->size_y ; ii++ ) {

    // Get the latitude and longitude of each pixel in this row.
    g_assert (ii <= SSIZE_MAX);
    map_projected_dem_get_latitudes_longitudes_heights (dem, ii, lats, lons, 
							heights);

    // The time of the point of closest approach (minimum discovered
    // by solver) for a DEM pixel.  We have this outside the loop so
    // we can use it as the starting point for solution to
    // neighborint pixels and save some iterations.
    double min = -DBL_MAX;

    size_t jj;
    //FIXME:remove    for ( jj = 2262 ; jj < dem->size_x ; jj++ ) {
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

      ITRS_point_free (ctp);

      // Current iteration, maximum number of iterations to try.
      int iteration = 0, max_iterations = 80;  

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
      // If there is no minimum in this range, just set the painted
      // dem pixel to zero and go on to the next pixel (FIXME:
      // probably need to do something smarter here).
      if ( return_code == GSL_FAILURE ) {
	fprintf (stderr, "No minimum in starting range for pixel %ld, %ld\n",
		 (long int) jj, (long int) ii);
	float_image_set_pixel (pd, jj, ii, 0.0);
	break;
      }
	
      gsl_set_error_handler (NULL);

      do {
	iteration++;
	status = gsl_min_fminimizer_iterate (minimizer);
    
	min = gsl_min_fminimizer_x_minimum (minimizer);
	sor = gsl_min_fminimizer_x_lower (minimizer);
	eor = gsl_min_fminimizer_x_upper (minimizer);
    
	status = gsl_min_test_interval (sor, eor, 0.0001, 0.0);
      }
      while (status == GSL_CONTINUE && iteration < max_iterations);

      // We need to have the convergence work.  
      if ( status != GSL_SUCCESS ) {
	fprintf (stderr, "Convergence failed for pixel %ld, %ld: %s\n", 
		 (long int) jj, (long int) ii, gsl_strerror (status));
      	fprintf (stderr, "sor: %lf, eor: %lf\n", sor, eor);
	
	if ( fabs (sor - (-0.48025236508010827)) < 0.001 ) {
	  printf ("Yep that's a bad one\n");
	}

	printf ("DEM pixel: %ld, %ld\n", (long int) jj, (long int) ii);

  double *t_sxvs = g_new (double, 500);
  double *t_syvs = g_new (double, 500);
  double *t_szvs = g_new (double, 500);
  double *t_sx_vels = g_new (double, 500);
  double *t_sy_vels = g_new (double, 500);
  double *t_prop_sy_vels = g_new (double, 500);
  t_prop_sy_vels = t_prop_sy_vels;
  double *t_sz_vels = g_new (double, 500);
  double *t_sx_pp = g_new (double, 500);
  double *t_sy_pp = g_new (double, 500);
  double *t_sz_pp = g_new (double, 500);
  double *t_stimes = g_new (double, 500);
  size_t t_zz;
  for ( t_zz = 0 ; t_zz < 500 ; t_zz++ ) {
    Vector this_pos;
    double mr = (eor + sor) / 2.0; /* Middle of x range to plot.  */
    double spr = mr - 0.1; /* Start of range to plot.  */
    if ( spr < pp_fixed->start_time ) { spr = pp_fixed->start_time; }
    double epr = mr + 0.1; /* End of range to plot.  */
    if ( epr > pp_fixed->end_time ) { epr = pp_fixed->end_time; }
    
    double ct = spr + (epr - spr) * t_zz / 500.0;

    t_stimes[t_zz] = ct;
    ITRS_platform_path_position_at_time (pp_fixed, ct, &this_pos);
    t_sxvs[t_zz] = this_pos.x;
    t_syvs[t_zz] = this_pos.y;
    t_szvs[t_zz] = this_pos.z;
    Vector this_vel;
    ITRS_platform_path_velocity_at_time (pp_fixed, ct, &this_vel);
    t_sx_vels[t_zz] = this_vel.x;
    t_sy_vels[t_zz] = this_vel.y;
    t_sz_vels[t_zz] = this_vel.z;
    if ( t_zz != 0 ) {
      t_sx_pp[t_zz] = t_sx_vels[t_zz] - t_sx_vels[t_zz - 1];
      t_sy_pp[t_zz] = t_sy_vels[t_zz] - t_sy_vels[t_zz - 1];
      t_sz_pp[t_zz] = t_sz_vels[t_zz] - t_sz_vels[t_zz - 1];
    }
  }
  t_sx_pp[0] = t_sx_pp[1];
  t_sy_pp[0] = t_sy_pp[1];
  t_sz_pp[0] = t_sz_pp[1];

  //  sp_basic_plot (500, t_stimes, t_sxvs, "line");
  //  sp_basic_plot (500, t_stimes, t_syvs, "line");
  //  sp_basic_plot (500, t_stimes, t_szvs, "line");
  //  sp_basic_plot (500, t_stimes, t_sx_vels, "line");
  //  sp_basic_plot (500, t_stimes, t_sy_vels, "line");
  //  sp_basic_plot (500, t_stimes, t_sz_vels, "line");
  //  sp_basic_plot (500, t_stimes, t_sx_pp, "line");
  //  sp_basic_plot (500, t_stimes, t_sy_pp, "line");
  //  sp_basic_plot (500, t_stimes, t_sz_pp, "line");
  g_free (t_sxvs);
  g_free (t_syvs);
  g_free (t_szvs);
  g_free (t_sx_vels);
  g_free (t_sy_vels);
  g_free (t_prop_sy_vels);
  g_free (t_sz_vels);
  g_free (t_sx_pp);
  g_free (t_sy_pp);
  g_free (t_sz_pp);
  g_free (t_stimes);

	ITRSPoint *ttp
	  = ITRS_point_new_from_geodetic_lat_long_height (lats[2190], 
							  lons[2190], 
							  heights[2190]);
	target_distance_params tpparms;
	tpparms.target = vector_new (ttp->x, ttp->y, ttp->z);
	tpparms.pp = pp_fixed;
	double *times = g_new (double, dem->size_x);
	double *ranges = g_new (double, dem->size_x);
	size_t ts;
	for ( ts = 0 ; ts < dem->size_x ; ts++ ) {
	  times[ts] = ts * 16.0 / dem->size_x;
	  ranges[ts] = target_distance (times[ts], &tpparms);
	}
	g_free (times);
	g_free (ranges);
	//	sp_basic_plot (dem->size_x, times, ranges, "line");
      }

      // The resulting minimum is time in the arc model of the point
      // of closest approach.  FIXME: how to verify input images are
      // zero-doppler processed?
      double solved_time = min;

      // The slant range can be found from the distance between the
      // target and the platform at the point of closest approach.
      Vector poca; 
      ITRS_platform_path_position_at_time (pp_fixed, solved_time, &poca);
      Vector *poca_to_target = vector_copy (&poca);
      vector_subtract (poca_to_target, &cp_target);
      double solved_slant_range = vector_magnitude (poca_to_target);
      vector_free (poca_to_target);

      // Look up the backscatter value for the found slant range and
      // time.
      float backscatter;
      if ( slant_range_image_contains (sri, solved_slant_range, 
				       solved_time, 1e-3) ) {
	backscatter 
	  = slant_range_image_sample (sri, solved_slant_range, solved_time,
				      FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR);
      }
      else {
	// We don't have image over this part of the DEM, so set to
	// the mask value (FIXME: better mask handling neede).
	if ( ii % 1000 == 0 && jj % 1000 == 0 ) {
	  printf ("No image at %ld, %ld\n", (long int) jj, (long int) ii);
	  printf ("(Slant range %lg, time %lg)\n", solved_slant_range,
		  solved_time);
	}
	backscatter = 0.0;
      }

      // Set the pixel in the painted dem.
      float_image_set_pixel (pd, jj, ii, backscatter);
      if ( backscatter != 0 ) {
	//	printf ("Backscatter: %g\n", backscatter);
      }

      if ( jj == 2200 && ii >= 3200 && ii < 3300 ) {
	solved_time_plot[ii - 3200] = solved_time;
      }
    }

    if ( (ii + 1) % 100 == 0 ) {
      printf ("Finished row %d\n", ii + 1);
    }
  }

  //  sp_basic_plot (100, NULL, solved_time_plot, "line");
  g_free (solved_time_plot);

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

  float_image_export_as_jpeg (pd, "painted_dem.jpg", 
			      GSL_MAX (pd->size_x, pd->size_y), 0.0);

  /*
  FloatImage *volcano = float_image_new_subimage (pd, 2100, 3100, 300, 300);
  float_image_export_as_jpeg (volcano, "volcano.jpg",
			      GSL_MAX (volcano->size_x, volcano->size_y), 0.0);
  */

  /* FIXME: MOSTLY NOTHING IS (g_)free () 'd.  If this should become a
     library function this would have to change.  */

  exit (EXIT_SUCCESS);
}
