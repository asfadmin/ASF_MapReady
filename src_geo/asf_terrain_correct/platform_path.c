// Implementation of the interface described in platform_path.h.

#include <assert.h>
#include <math.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_interp.h>

#include "platform_path.h"

PlatformPath *
platform_path_new (int control_point_count, double start_time, double end_time,
		   int observation_count, double *observation_times, 
		   OrbitalStateVector **observations)
{
  // Its a spline, we need at least three control points.
  assert (control_point_count >= 3);

  // Time runs forward as far as this model is concerned.
  assert (start_time < end_time);

  // We must have some actual data to feed to the propagator.
  assert (observation_count >= 1);

  // How far do we trust our propagator to take us through time?  This
  // is the maximum we will allow the start time or end time to fall
  // outside the range of time covered by observation_times, and one
  // half the maximum time we will allow between observations.  See
  // the comments in orbital_state_vector.h for a notion of how much
  // error we are talking about here.
  const double max_free_propagation_time = 5 * 60;

  assert (observation_times[0] - start_time <= max_free_propagation_time);
  assert (end_time - observation_times[observation_count - 1] 
	  <= max_free_propagation_time);

  PlatformPath *self = calloc (1, sizeof (PlatformPath));

  // Storage for the data the splines will be based on (x control
  // points, etc.).
  double *times = calloc (control_point_count, sizeof (double));
  double *xcp = calloc (control_point_count, sizeof (double));
  double *ycp = calloc (control_point_count, sizeof (double));
  double *zcp = calloc (control_point_count, sizeof (double));

  double time_range = end_time - start_time;

  int ii, jj;			// Index variables.

  // The control point times are evenly spaced over the time interval
  // modeled.
  double cp_separation = time_range / (control_point_count - 1);

  // Initialize an array of control point times.
  for ( ii = 0 ; ii < control_point_count ; ii++ ) {
    times[ii] = start_time + ii * cp_separation;
  }

  // Find the values of interest at all the control points.
  for ( ii = 0 ; ii < control_point_count ; ii++ ) {

    double ct = times[ii];	// Current time.

    // Find the nearest observation after the current time.
    for ( jj = 0 ; jj < observation_count ; jj++ ) {
      if ( observation_times[jj] >= ct ) {
	break;
      }
    }

    // If the current time is before the time of the first
    // observation, or after the time of the last observation, all we
    // can do is propagate backward or forward, respectively.
    if ( jj == 0 || jj == observation_count ) {
      /* Initializer reassures compiler without making pointer usable.  */
      OrbitalStateVector *tmp = NULL;
      if ( jj == 0 ) {
	tmp = orbital_state_vector_copy (observations[jj]);
	orbital_state_vector_propagate (tmp, ct - observation_times[jj]);
      }
      else if ( jj == observation_count ) {
	tmp = orbital_state_vector_copy (observations[jj - 1]);
	orbital_state_vector_propagate (tmp, ct - observation_times[jj - 1]);
      }
      xcp[ii] = tmp->position->x;
      ycp[ii] = tmp->position->y;
      zcp[ii] = tmp->position->z;
      orbital_state_vector_free (tmp);      
    }

    // Otherwise, we can smash together some information from a
    // forward propagation and a backward propagation to come up with
    // the result.
    else {

      // We will keep track of which pair of observations we are
      // between so we don't have to propagate things over the same
      // path multiple times.  This magic number and static value keep
      // track of which segment we are currently in.
#define NO_CURRENT_SEGMENT -1
      static int current_segment = NO_CURRENT_SEGMENT;
      // Times of the observations that bound the current segment.
      static double current_segment_start, current_segment_end;
      // Last observation and next observation.  These terms are a bit
      // inappropriate since we propagate these guys incrementally
      // as we compute the positions for the spline control points.
      static OrbitalStateVector *lo, *no;

      if ( current_segment == NO_CURRENT_SEGMENT 
	   || ct > current_segment_end ) {
	if ( current_segment != NO_CURRENT_SEGMENT ) {
	  orbital_state_vector_free (lo);
	  orbital_state_vector_free (no);
	}
	current_segment = jj - 1;
	lo = orbital_state_vector_copy (observations[current_segment]);
	no = orbital_state_vector_copy (observations[current_segment + 1]);
	current_segment_start = observation_times[current_segment];
	current_segment_end = observation_times[current_segment + 1];
	orbital_state_vector_propagate (lo, ct - current_segment_start);
	orbital_state_vector_propagate (no, ct - current_segment_end);
      }
      else {
	orbital_state_vector_propagate (lo, cp_separation);
	orbital_state_vector_propagate (no, cp_separation);
      }

      // We now have two opinions about where the satellite is at the
      // current time, one based on a forward propagation and the
      // other based on a backward propagation.  We want to reconcile
      // the two in such a way that the overall flight path model
      // curve is smooth -- a sudden jerk at 50% between observations
      // as we stop trusting one propagation and start trusing the
      // other would be bad.  A linear weighted interpolation also
      // seems inappropriate however, since the propagation from the
      // nearer observation almost certainly deserves more than linear
      // weight.  As a rough but probably somewhat reasonable guess
      // what to do, we take y = x^(1/3) as the interpolation control
      // (i.e. x == -1 => y == -1 => trust the previous observation
      // entirely, x == 1 => y == 1 => trust the next observation
      // entirely).

      // "Fraction of path", except we make this term wrong by
      // immediately mapping it into [-1, 1].
      double fop = ((ct - current_segment_start) 
		    / (current_segment_end - current_segment_start));
      fop = (fop * 2) - 1;

      // "Weighting of next observation".  Here we apply the function
      // discussed above, then map the result back into [0, 1].
      double wono = ((GSL_SIGN (fop) * pow (fabs (fop), 1.0 / 3.0)) + 1) / 2.0;
      
      // Finally we can compute the control point for this time.
      xcp[ii] = lo->position->x * (1.0 - wono) + no->position->x * wono;
      ycp[ii] = lo->position->y * (1.0 - wono) + no->position->y * wono;
      zcp[ii] = lo->position->z * (1.0 - wono) + no->position->z * wono;
    }
  }

  self->xp_accel = gsl_interp_accel_alloc ();
  self->yp_accel = gsl_interp_accel_alloc ();
  self->zp_accel = gsl_interp_accel_alloc ();
  self->xp = gsl_spline_alloc (gsl_interp_cspline, control_point_count);
  self->yp = gsl_spline_alloc (gsl_interp_cspline, control_point_count);
  self->zp = gsl_spline_alloc (gsl_interp_cspline, control_point_count);
  
  int return_code 
    = gsl_spline_init (self->xp, times, xcp, control_point_count);
  assert (return_code == GSL_SUCCESS);
  return_code = gsl_spline_init (self->yp, times, ycp, control_point_count);
  assert (return_code == GSL_SUCCESS);
  return_code = gsl_spline_init (self->zp, times, zcp, control_point_count);
  assert (return_code == GSL_SUCCESS);
    
  return self;
}

void
platform_path_position_at_time (PlatformPath *self, double time, 
				Vector *position)
{
  int return_code = gsl_spline_eval_e (self->xp, time, self->xp_accel, 
				       &(position->x));
  assert (return_code == GSL_SUCCESS);

  return_code = gsl_spline_eval_e (self->yp, time, self->yp_accel, 
				       &(position->y));
  assert (return_code == GSL_SUCCESS);

  return_code = gsl_spline_eval_e (self->zp, time, self->zp_accel, 
				       &(position->z));
  assert (return_code == GSL_SUCCESS);
}

void
platform_path_velocity_at_time (PlatformPath *self, double time, 
				Vector *velocity)
{
  int return_code = gsl_spline_eval_deriv_e (self->xp, time, self->xp_accel,
					     &(velocity->x));
  assert (return_code == GSL_SUCCESS);

  return_code = gsl_spline_eval_deriv_e (self->yp, time, self->yp_accel,
					 &(velocity->y));
  assert (return_code == GSL_SUCCESS);
  return_code = gsl_spline_eval_deriv_e (self->zp, time, self->zp_accel,
					 &(velocity->z));
  assert (return_code == GSL_SUCCESS);
}
