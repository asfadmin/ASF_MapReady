/* Tests for the OrbitalStateVector class.  */

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include <gsl/gsl_const_mksa.h>

#include "basic_types.h"
#include "orbital_state_vector.h"

/* Initial position and velocity of the satellite, in cartesian
   geocentric equitorial inertial (GEI) coordinates.  This test point
   is from an ERS-1 precision state vector from the European Space
   Agency (ESA), day -84.5 from 1/1/2000 12h, 6934.184000 seconds
   since 0:00 terrestrial dynamical time.  */
#define INITIAL_POSITION_X -6110369.705
#define INITIAL_POSITION_Y 307.276
#define INITIAL_POSITION_Z 3731924.788
#define INITIAL_VELOCITY_X -3838.984250 
#define INITIAL_VELOCITY_Y 1300.283917
#define INITIAL_VELOCITY_Z -6266.699787

// FIXME: This value needs to be more independently verified (it is
// currently copied from the output of the program after a program
// tweak...)
#define INITIAL_HEIGHT_ABOVE_WGS84_ELLIPSOID 787545.579
// These are as good as they were originally.
#define INITIAL_A 7163570.1316334093
#define INITIAL_E 0.0014278553721458226
#define INITIAL_I 1.720014202603227
#define INITIAL_CAPITAL_OMEGA 0.091997495102675686
#define INITIAL_LOWER_CASE_OMEGA 1.3833966073328821
#define INITIAL_TRUE_ANOMALY 1.2030299825263508
#define INITIAL_ECCENTRIC_ANOMALY 1.2016979453507912
#define INITIAL_MEAN_ANOMALY 1.2003662517461209

/* Second test point, T2 seconds after the initial point.  This is
   from the same batch of precision ERS-1 state vectors as the initial
   test point.  */
#define T2 150	
#define T2_POSITION_X -6609404.299
#define T2_POSITION_Y 194552.463
#define T2_POSITION_Z 2750150.087
#define T2_VELOCITY_X -2801.231476
#define T2_VELOCITY_Y 1284.377186
#define T2_VELOCITY_Z -6796.920479

/* Third test point, T3 seconds after the initial point.  This is from
   the same batch of precision ERS-1 state vectors as the initial test
   point.  */
#define T3 5970
#define T3_POSITION_X -5842870.981
#define T3_POSITION_Y -92478.481
#define T3_POSITION_Z 4136227.065
#define T3_VELOCITY_X -4268.589817
#define T3_VELOCITY_Y 1291.991019
#define T3_VELOCITY_Z -5984.052523

/* Evaluate true iff doubles a and b are within c of each other.  */
#define COMPARE_DOUBLES(a, b, c) (fabs (a - b) <= c ? 1 : 0)

/* Assert that the x, y, and z components of osv are all within
   error_margin of the corresponding x, y, or z argument.  */
static void
assert_position (OrbitalStateVector *osv, double x, double y, double z, 
		 double error_margin)
{
  double position_error = sqrt (pow (osv->position->x - x, 2)
				+ pow (osv->position->y - y, 2)
				+ pow (osv->position->z - z, 2));
  assert (position_error <= error_margin);
}

int
main (void)
{
  /* Test creation method.  */
  OrbitalStateVector *osv 
    = orbital_state_vector_new (INITIAL_POSITION_X, INITIAL_POSITION_Y,
				INITIAL_POSITION_Z, INITIAL_VELOCITY_X,
				INITIAL_VELOCITY_Y, INITIAL_VELOCITY_Z);
  /* The values have just been set, so there had better not be much
     error.  */
  double super_tiny_error = 1e-30;
  assert_position (osv, INITIAL_POSITION_X, INITIAL_POSITION_Y, 
		   INITIAL_POSITION_Z, super_tiny_error);

  /* Test the copy method.  */
  OrbitalStateVector *osv_copy = orbital_state_vector_copy (osv);
  assert_position (osv_copy, INITIAL_POSITION_X, INITIAL_POSITION_Y, 
		   INITIAL_POSITION_Z, super_tiny_error);

  /* Test the height_above_ellipsoid method.  */
  double height_error = 0.1;
  assert (COMPARE_DOUBLES (orbital_state_vector_height_above_ellipsoid (osv),
			   INITIAL_HEIGHT_ABOVE_WGS84_ELLIPSOID, 
			   height_error));

  /* Test the get_keplerian_elements method.  */
  double a, e, i, capital_omega, lower_case_omega, nu, E, M;
  orbital_state_vector_get_keplerian_elements (osv, &a, &e, &i, &capital_omega,
					       &lower_case_omega, &nu, &E, &M);
  double keplerian_component_error = 1e-12;
  assert (COMPARE_DOUBLES (a, INITIAL_A, keplerian_component_error));
  assert (COMPARE_DOUBLES (e, INITIAL_E, keplerian_component_error));
  assert (COMPARE_DOUBLES (i, INITIAL_I, keplerian_component_error));
  assert (COMPARE_DOUBLES (capital_omega, INITIAL_CAPITAL_OMEGA,
			   keplerian_component_error));
  assert (COMPARE_DOUBLES (lower_case_omega, INITIAL_LOWER_CASE_OMEGA,
			   keplerian_component_error));
  assert (COMPARE_DOUBLES (nu, INITIAL_TRUE_ANOMALY,
			   keplerian_component_error));
  assert (COMPARE_DOUBLES (E, INITIAL_ECCENTRIC_ANOMALY,
			   keplerian_component_error));
  assert (COMPARE_DOUBLES (M, INITIAL_MEAN_ANOMALY,
			   keplerian_component_error));

  /* Test propagation a short distance forward in time.  */
  orbital_state_vector_propagate (osv, T2);             
  double short_propagation_error = 2;                  
  assert_position (osv, T2_POSITION_X, T2_POSITION_Y,
		   T2_POSITION_Z, short_propagation_error);

  /* Test propagation a bit further forward in time.  */
  orbital_state_vector_propagate (osv_copy, T3);
  double long_propagation_error = 400;
  assert_position (osv_copy, T3_POSITION_X, T3_POSITION_Y, T3_POSITION_Z,
		   long_propagation_error);
		   
  /* Test propagation back in time.  */
  orbital_state_vector_propagate (osv_copy, T2 - T3);
  /* We should be really close, since we are going back from the
     position the propagator calculated in the forward direction to
     the starting position of that forward propagation.  The accuracy
     here tests the accuracy of the algorithm running in reverse.  */
  double long_reverse_propagation_error = 2.0;
  assert_position (osv_copy, T2_POSITION_X, T2_POSITION_Y, T2_POSITION_Z,
		   long_reverse_propagation_error);

  exit (EXIT_SUCCESS);
}
