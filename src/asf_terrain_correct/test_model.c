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
#include <asf_meta.h>
#include "earth_constants.h"
#include "orbital_state_vector.h"
#include "platform_path.h"
#include "scratchplot.h"

static void
gei_to_itrs (DateTime *dt, Vector *gei_pos, Vector *itrs_pos)
{
  /* The current GEI coordinates in GNU Scientific Library vector
     form.  */
  gsl_vector *gei_p = gsl_vector_alloc (3);
  gsl_vector_set (gei_p, 0, gei_pos->x);
  gsl_vector_set (gei_p, 1, gei_pos->y);
  gsl_vector_set (gei_p, 2, gei_pos->z);

  double theta = date_time_earth_angle (dt);
    
  /* Sidereal time (earth angle) rotation matrix as described in
     "Satellite Geodesy".  */
  gsl_matrix *earm = gsl_matrix_alloc (3, 3);
  gsl_matrix_set (earm, 0, 0, cos (theta));
  gsl_matrix_set (earm, 0, 1, sin (theta));
  gsl_matrix_set (earm, 0, 2, 0.0);
  gsl_matrix_set (earm, 1, 0, -sin (theta));
  gsl_matrix_set (earm, 1, 1, cos (theta));
  gsl_matrix_set (earm, 1, 2, 0.0);
  gsl_matrix_set (earm, 2, 0, 0.0);
  gsl_matrix_set (earm, 2, 1, 0.0);
  gsl_matrix_set (earm, 2, 2, 1.0);

  /* Equivalent position in desired itrs reference frame.  */
  gsl_vector *itrs_p = gsl_vector_alloc (3);
  gsl_vector_set_zero (itrs_p);

  /* Perform the rotation.  */
  int return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, gei_p, 0.0, 
				    itrs_p);
  assert (return_code == GSL_SUCCESS);
  itrs_pos->x = gsl_vector_get (itrs_p, 0);
  itrs_pos->y = gsl_vector_get (itrs_p, 1);
  itrs_pos->z = gsl_vector_get (itrs_p, 2);
}

int
main (void)
{
  meta_parameters *imd 
    = meta_read ("test_data/dem_over_akutan/R144978315G1S003.meta");

  double observation_time = imd->state_vectors->vecs[0].time;
  observation_time = observation_time;

  DateTime *ob_date = date_time_new (imd->state_vectors->year,
				     imd->state_vectors->julDay,
				     imd->state_vectors->second, UTC);


  // Get the angle of the earth during this observation.
  double theta = date_time_earth_angle (ob_date);
  
  // Earth angle rotation matrix (see below for details).
  gsl_matrix *earm = gsl_matrix_alloc (3,3);

  // International terrestrial reference system (ITRS) coordinates of
  // state vector (the form they come in in the metadata).
  gsl_vector *itrs_pos = gsl_vector_alloc (3);
  gsl_vector *itrs_vel = gsl_vector_alloc (3);

  // Corresponding geocentric equitorial inertial (GEI) coordinates of
  // state vector (the form we need in order to propagate them).
  gsl_vector *gei_pos = gsl_vector_alloc (3);
  gsl_vector *gei_vel = gsl_vector_alloc (3);

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


  // Indicies of x, y, and z vector components in gsl_vector type.
  const size_t xi = 0, yi = 1, zi = 2;

  // Load position and velocity vectors in earth fixed form into
  // vectors which we can rotate.
  gsl_vector_set (itrs_pos, xi, imd->state_vectors->vecs[0].vec.pos.x);
  gsl_vector_set (itrs_pos, yi, imd->state_vectors->vecs[0].vec.pos.y);
  gsl_vector_set (itrs_pos, zi, imd->state_vectors->vecs[0].vec.pos.z);
  gsl_vector_set (itrs_vel, xi, imd->state_vectors->vecs[0].vec.vel.x);
  gsl_vector_set (itrs_vel, yi, imd->state_vectors->vecs[0].vec.vel.y);
  gsl_vector_set (itrs_vel, zi, imd->state_vectors->vecs[0].vec.vel.z);

  // Temporary vector.
  gsl_vector *tmp = gsl_vector_alloc (3);
  
  // Perform rotation from earth fixed back to GEI coordinates.
  gsl_vector_set_zero (gei_pos);
  int return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, itrs_pos, 0.0, 
				    gei_pos);
  g_assert (return_code == GSL_SUCCESS);

  // The fixed earth velocity vectors are affected by the rotation of
  // the earth itself, so first we have to subtract this term out.
  gsl_vector_set (tmp, xi, (gsl_vector_get (itrs_vel, xi) 
			    - (EARTH_ROTATION_RATE 
			       * gsl_vector_get (itrs_pos, yi))));
  gsl_vector_set (tmp, yi, (gsl_vector_get (itrs_vel, yi) 
			    + (EARTH_ROTATION_RATE 
			       * gsl_vector_get (itrs_pos, xi))));
  gsl_vector_set (tmp, zi, gsl_vector_get (itrs_vel, zi));

  // Now we can rotate the remaining velocity back into the GEI
  // system.  FIXME: we use a slightly different (by ~10 microdegrees)
  // earth angle than the code in asf_meta, so the velocity ends up
  // being different by as much as 10 m/s in some components --
  // generally not an issue for a 15 second frame but bad practice
  // nevertheless.  We ought to change things so the correct values
  // are used everywhere.
  return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, tmp, 0.0, 
				gei_vel);
  g_assert (return_code == GSL_SUCCESS);

  OrbitalStateVector *observation 
    = orbital_state_vector_new (gsl_vector_get (gei_pos, 0),
				gsl_vector_get (gei_pos, 1),
				gsl_vector_get (gei_pos, 2),
				gsl_vector_get (gei_vel, 0),
				gsl_vector_get (gei_vel, 1),
				gsl_vector_get (gei_vel, 2));

#define TEST_POINTS 50

  double *y = g_new (double, TEST_POINTS);
  double *vy = g_new (double, TEST_POINTS);

  double *y_ef = g_new (double, TEST_POINTS);
  double *vy_ef = g_new (double, TEST_POINTS);

  double *earth_angle = g_new (double, TEST_POINTS);
  double *earth_angle_p = g_new (double, TEST_POINTS);

  int ii;
  for ( ii = 0 ; ii < TEST_POINTS ; ii++ ) {
    double time = -0.3 - 0.2 * (double) ii / TEST_POINTS;
    OrbitalStateVector *tmp2 = orbital_state_vector_copy (observation);
    orbital_state_vector_propagate (tmp2, time);
    y[ii] = tmp2->position->y;
    vy[ii] = tmp2->velocity->y;
    DateTime *tmp_time = date_time_copy (ob_date);
    date_time_subtract_seconds (tmp_time, time);

    // Rotate back into earth fixed form.
    Vector *tmp3 = vector_copy (tmp2->position);
    Vector *tmp5 = vector_new (0, 0, 0);
    earth_angle[ii] = date_time_earth_angle (tmp_time);
    printf ("earth_angle[%d]: %.12lf\n", ii, earth_angle[ii]);
    if ( ii != 0 ) {
      earth_angle_p[ii] = earth_angle[ii] - earth_angle[ii - 1];
    }
    gei_to_itrs (tmp_time, tmp3, tmp5);

    y_ef[ii] = tmp5->y;
    if ( ii != 0 ) {
      vy_ef[ii] = y_ef[ii] - y_ef[ii - 1];
    }
    date_time_free (tmp_time);
    orbital_state_vector_free (tmp2);
    vector_free (tmp3);
    vector_free (tmp5);
  }
  earth_angle_p[0] = earth_angle_p[1];
  vy_ef[0] = vy_ef[1];

  //sp_basic_plot (TEST_POINTS, NULL, y, "line");
  //sp_basic_plot (TEST_POINTS, NULL, vy, "line");
  sp_basic_plot (TEST_POINTS, NULL, earth_angle, "line");
  sp_basic_plot (TEST_POINTS, NULL, earth_angle_p, "line");
  sp_basic_plot (TEST_POINTS, NULL, y_ef, "line");
  sp_basic_plot (TEST_POINTS, NULL, vy_ef, "line");

  double *observation_times = g_new (double, 1);
  observation_times[0] = observation_time;

  PlatformPath *pp 
    = platform_path_new (TEST_POINTS, -0.5, -0.3, 1, observation_times, 
			 &observation);

  ITRSPlatformPath *ppfe
    = ITRS_platform_path_new (TEST_POINTS, -0.5, -0.3, 1, ob_date, 
			      observation_times, &observation);

  double *pp_y = g_new (double, TEST_POINTS);
  double *pp_vy = g_new (double, TEST_POINTS);

  double *ppef_y = g_new (double, TEST_POINTS);
  double *ppef_vy = g_new (double, TEST_POINTS);
  
  for ( ii = 0 ; ii < TEST_POINTS ; ii++ ) {
    double time = -0.3 - 0.2 * (double) ii / TEST_POINTS;
    Vector this_position;
    platform_path_position_at_time (pp, time, &this_position);
    pp_y[ii] = this_position.y;
    Vector this_velocity;
    platform_path_velocity_at_time (pp, time, &this_velocity);
    pp_vy[ii] = this_velocity.y;
    Vector ef_position;
    Vector ef_velocity;
    ITRS_platform_path_position_at_time (ppfe, time, &ef_position);
    ITRS_platform_path_velocity_at_time (ppfe, time, &ef_velocity);    
    ppef_y[ii] = ef_position.y;
    ppef_vy[ii] = ef_velocity.y;
  }

  //sp_basic_plot (TEST_POINTS, NULL, pp_y, "line");
  //sp_basic_plot (TEST_POINTS, NULL, pp_vy, "line");
  sp_basic_plot (TEST_POINTS, NULL, ppef_y, "line");
  sp_basic_plot (TEST_POINTS, NULL, ppef_vy, "line");


  exit (EXIT_SUCCESS);
}
