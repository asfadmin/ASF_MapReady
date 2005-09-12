// Tests for the PlatformPath class.

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include "orbital_state_vector.h"
#include "platform_path.h"
#include "vector.h"

// We will consider these tests to be passing if the results from a
// propagation from the nearest observation match the values returned
// from the path spline to within these magnitudes.
#define POSITION_TOLERANCE 5.0
// This relatively low velocity accuracy is probably due to the way
// the results of forward and backward propagations are averaged.
// Exponential mixing is used in order to give positional preference
// to the nearer state vector, but this introduces a fairly sharp
// wiggle around the point midway between the observations.
#define VELOCITY_TOLERANCE 150.0

// Three observations taken from a Radarsat product (i.e. probably not
// that accurate but will do for testing this class).  Better state
// vectors might well allow the above tolerances to be tightened.

#define OBSERVATION_COUNT 3

#define OBSERVATION_1_TIME 0.0
#define OBSERVATION_1_POSITION_X -2413790.3815
#define OBSERVATION_1_POSITION_Y -2155799.4092
#define OBSERVATION_1_POSITION_Z 6390152.83
#define OBSERVATION_1_VELOCITY_X -6484.7658646
#define OBSERVATION_1_VELOCITY_Y -2171.9538493 
#define OBSERVATION_1_VELOCITY_Z -3175.5349121

#define OBSERVATION_2_TIME 7.7431793213
#define OBSERVATION_2_POSITION_X -2463930.4534
#define OBSERVATION_2_POSITION_Y -2172522.6675
#define OBSERVATION_2_POSITION_Z 6365356.4453
#define OBSERVATION_2_VELOCITY_X -6466.8196843
#define OBSERVATION_2_VELOCITY_Y -2146.5651324
#define OBSERVATION_2_VELOCITY_Z -3229.095459

#define OBSERVATION_3_TIME 15.486358643
#define OBSERVATION_3_POSITION_X -2513930.0475
#define OBSERVATION_3_POSITION_Y -2189049.1695
#define OBSERVATION_3_POSITION_Z 6340146.4844
#define OBSERVATION_3_VELOCITY_X -6448.4266957
#define OBSERVATION_3_VELOCITY_Y -2121.058232
#define OBSERVATION_3_VELOCITY_Z -3282.4453125

// Number of control points of approximating splines.
#define CONTROL_POINT_COUNT 10000

// Start and end times for the arc approximation.
#define START_TIME -5.0
#define END_TIME 20

// Return true iff |a - b| <= magnitude.
static int
compare_vectors (Vector *a, Vector *b, double magnitude)
{
  Vector *difference = vector_copy (a);
  vector_subtract (difference, b);

  return vector_magnitude (difference) <= magnitude;
}

int 
main (void)
{
  double *observation_times = calloc (OBSERVATION_COUNT, sizeof (double));
  observation_times[0] = OBSERVATION_1_TIME;
  observation_times[1] = OBSERVATION_2_TIME;
  observation_times[2] = OBSERVATION_3_TIME;
  

  OrbitalStateVector **observations 
    = calloc (OBSERVATION_COUNT, sizeof (OrbitalStateVector *));
  observations[0] = orbital_state_vector_new (OBSERVATION_1_POSITION_X, 
					      OBSERVATION_1_POSITION_Y,
					      OBSERVATION_1_POSITION_Z,
					      OBSERVATION_1_VELOCITY_X,
					      OBSERVATION_1_VELOCITY_Y,
					      OBSERVATION_1_VELOCITY_Z);
  observations[1] = orbital_state_vector_new (OBSERVATION_2_POSITION_X, 
					      OBSERVATION_2_POSITION_Y,
					      OBSERVATION_2_POSITION_Z,
					      OBSERVATION_2_VELOCITY_X,
					      OBSERVATION_2_VELOCITY_Y,
					      OBSERVATION_2_VELOCITY_Z);
  observations[2] = orbital_state_vector_new (OBSERVATION_3_POSITION_X, 
					      OBSERVATION_3_POSITION_Y,
					      OBSERVATION_3_POSITION_Z,
					      OBSERVATION_3_VELOCITY_X,
					      OBSERVATION_3_VELOCITY_Y,
					      OBSERVATION_3_VELOCITY_Z);

  PlatformPath *pp = platform_path_new (CONTROL_POINT_COUNT, START_TIME, 
					END_TIME, OBSERVATION_COUNT,
					observation_times, observations);

  // We know the control points are evenly spaced in time, and we
  // would like to be sure to test how well the splines do away from
  // the control points.
  double time_span = END_TIME - START_TIME;
  // Control point spacing.
  double cp_spacing = time_span / (CONTROL_POINT_COUNT - 1);
  
  // Places to store the results of position and velocity lookups.
  Vector position, velocity;

  // For holding the results of propagations which can be compared
  // against the results given by the path interpolator.
  OrbitalStateVector *tmp;

  // Test some positions before the first control point for agreement
  // with the results of the propagator.

  // A time before the first observation.
  double tbfo = (START_TIME 
		 + (floor ((OBSERVATION_1_TIME - START_TIME) / 2.0
			   / cp_spacing)) * cp_spacing 
		 + cp_spacing / 2.0);
  platform_path_position_at_time (pp, tbfo, &position);
  platform_path_velocity_at_time (pp, tbfo, &velocity);
  tmp = orbital_state_vector_copy (observations[0]);
  orbital_state_vector_propagate (tmp, tbfo);
  assert (compare_vectors (&position, tmp->position, POSITION_TOLERANCE));
  assert (compare_vectors (&velocity, tmp->velocity, VELOCITY_TOLERANCE));
  orbital_state_vector_free (tmp);

  // A time after the last observation.
  double talo = (END_TIME
		 - (floor ((END_TIME - OBSERVATION_3_TIME) / 2.0
			   / cp_spacing)) * cp_spacing
		 + cp_spacing / 2.0);
  platform_path_position_at_time (pp, talo, &position);
  platform_path_velocity_at_time (pp, talo, &velocity);
  tmp = orbital_state_vector_copy (observations[2]);
  orbital_state_vector_propagate (tmp, talo - observation_times[2]);
  assert (compare_vectors (&position, tmp->position, POSITION_TOLERANCE));
  assert (compare_vectors (&velocity, tmp->velocity, VELOCITY_TOLERANCE));
  orbital_state_vector_free (tmp);

  // A time near the beginning of the span between the first two observations.
  double sffo = 0.1;		// Seconds from first observation (of the two).
  double tnbos = observation_times[0] + sffo;
  platform_path_position_at_time (pp, tnbos, &position);
  platform_path_velocity_at_time (pp, tnbos, &velocity);
  tmp = orbital_state_vector_copy (observations[0]);
  orbital_state_vector_propagate (tmp, tnbos - observation_times[0]);
  assert (compare_vectors (&position, tmp->position, POSITION_TOLERANCE));
  assert (compare_vectors (&velocity, tmp->velocity, VELOCITY_TOLERANCE));
  orbital_state_vector_free (tmp);

  // A time near the middle of the span between the first two observations.
  double tnmos = observation_times[0] + observation_times[1] / 2.0;
  platform_path_position_at_time (pp, tnmos, &position);
  platform_path_velocity_at_time (pp, tnmos, &velocity);
  tmp = orbital_state_vector_copy (observations[0]);
  orbital_state_vector_propagate (tmp, tnmos - observation_times[0]);

  OrbitalStateVector *tmp2 = orbital_state_vector_copy (observations[1]);
  orbital_state_vector_propagate (tmp2, tnmos - observation_times[1]);
  assert (compare_vectors (&position, tmp->position, POSITION_TOLERANCE));
  assert (compare_vectors (&velocity, tmp->velocity, VELOCITY_TOLERANCE));
  orbital_state_vector_free (tmp);
  // We want to test this time against propagations from both
  // neighboring observations.
  tmp = orbital_state_vector_copy (observations[1]);
  orbital_state_vector_propagate (tmp, tnmos - observation_times[1]);
  assert (compare_vectors (&position, tmp->position, POSITION_TOLERANCE));
  assert (compare_vectors (&velocity, tmp->velocity, VELOCITY_TOLERANCE));
  orbital_state_vector_free (tmp);

  // A time near the end of the span between the second and third
  // obseravtions.
  double sfto = 0.1;		// Seconds from third observation.
  double tneos = observation_times[2] - sfto;
  platform_path_position_at_time (pp, tneos, &position);
  platform_path_velocity_at_time (pp, tneos, &velocity);
  tmp = orbital_state_vector_copy (observations[2]);
  orbital_state_vector_propagate (tmp, tneos - observation_times[2]);
  assert (compare_vectors (&position, tmp->position, POSITION_TOLERANCE));
  assert (compare_vectors (&velocity, tmp->velocity, VELOCITY_TOLERANCE));
  orbital_state_vector_free (tmp);
  
  exit (EXIT_SUCCESS);
}
