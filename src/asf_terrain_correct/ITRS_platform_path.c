// Implementation of the interface described in ITRS_platform_path.h.

#include <assert.h>
#include <math.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_interp.h>

#include "ITRS_platform_path.h"

ITRSPlatformPath *
ITRS_platform_path_new (int control_point_count, double start_time, 
			double end_time, int observation_count, 
			DateTime *base_date, double *observation_times, 
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

  ITRSPlatformPath *self = calloc (1, sizeof (ITRSPlatformPath));

  // Record the interval modeled in the object.
  self->start_time = start_time;
  self->end_time = end_time;

  // To avoid problems with floating point comparisons, we will
  // actually model a slightly larger time window that advertised.
  const double time_window_guard_time = 0.0001;
  double true_start_time = start_time - time_window_guard_time;
  double true_end_time = end_time + time_window_guard_time;

  // Storage for the data the splines will be based on (x control
  // points, etc.).
  double *times = calloc (control_point_count, sizeof (double));
  double *xcp = calloc (control_point_count, sizeof (double));
  double *ycp = calloc (control_point_count, sizeof (double));
  double *zcp = calloc (control_point_count, sizeof (double));

  double time_range = true_end_time - true_start_time;

  int ii, jj;			// Index variables.

  // The control point times are evenly spaced over the time interval
  // modeled.
  double cp_separation = time_range / (control_point_count - 1);

  // Initialize an array of control point times.
  times[0] = true_start_time;
  for ( ii = 1 ; ii < control_point_count ; ii++ ) {
    times[ii] = times[ii - 1] + cp_separation;
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

      // Store the geocentric equitorial inertial (GEI) form of the
      // spline control point.  We still need to transform this to the
      // international terrestrial reference system (ITRS) earth fixed
      // form, but we will do that later once all the GEI contol points
      // have been computed.
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

      /* FIXME this exponential crap seems to have a problem
	 somewhere, so for the moment we just use a linear arrangment
	 instead (see below).

      // "Fraction of path", except we make this term wrong by
      // immediately mapping it into [-1, 1].
      double fop = ((ct - current_segment_start) 
		    / (current_segment_end - current_segment_start));
      fop = (fop * 2) - 1;

      // "Weighting of next observation".  Here we apply the function
      // discussed above, then map the result back into [0, 1].
      double wono = ((GSL_SIGN (fop) * pow (fabs (fop), 1.0 / 3.0)) + 1) / 2.0;
      
      // Create the weighted average state vector.
      OrbitalStateVector *average
	= orbital_state_vector_new (lo->position->x * (1.0 - wono) 
				    + no->position->x * wono,
				    lo->position->y * (1.0 - wono) 
				    + no->position->y * wono,
				    lo->position->z * (1.0 - wono) 
				    + no->position->z * wono,
				    lo->velocity->x * (1.0 - wono) 
				    + no->velocity->x * wono,
				    lo->velocity->y * (1.0 - wono) 
				    + no->velocity->y * wono,
				    lo->velocity->z * (1.0 - wono) 
				    + no->velocity->z * wono);

      */
	
      // Create the weighted average state vector.
      double fop = ((ct - current_segment_start) 
		    / (current_segment_end - current_segment_start));
      OrbitalStateVector *average
	= orbital_state_vector_new (lo->position->x * (1.0 - fop)
				    + no->position->x * (fop),
				    lo->position->y * (1.0 - fop)
				    + no->position->y * (fop),
				    lo->position->z * (1.0 - fop)
				    + no->position->z * (fop),
				    lo->velocity->x * (1.0 - fop) 
				    + no->velocity->x * (fop),
				    lo->velocity->y * (1.0 - fop) 
				    + no->velocity->y * (fop),
				    lo->velocity->z * (1.0 - fop) 
				    + no->velocity->z * (fop));
			  				    
      // Store the GEI form of the spline control point.  We still
      // need to transform this to the ITRS earth fixed form, but we
      // will do that later once all the GEI contol points have been
      // computed.
      xcp[ii] = average->position->x;
      ycp[ii] = average->position->y;
      zcp[ii] = average->position->z;
    }
  }

  // Now we need to rotate the spline control points from GEI space to
  // ITRS space.  To do this, we need to determine the angle between
  // coordinate systems at each point in time.  Unfortunately, the
  // earch angle computation available in date_time_earth_angle
  // contains some complicated calculations and is not completely
  // smooth at a fine scale, which can make trouble for algorithms
  // that expect a completely smooth orbit trajectory (point of closes
  // approach minimization algorithms, for example).  To get around
  // this problem, we find the average earth rotation rate (the
  // rotation rate should be almost constant over the time span
  // modeled by a given platform path) and increment the earth angle
  // between spline control points.

  // The assumption that the earth rotation rate is constant is only
  // good if the platform path segment we are modeling isn't that
  // long.  This class could otherwise model paths of arbitrary
  // length.  But we don't need to do that at the moment anyway.  If
  // we ever did, we could just take local averages rather than the
  // single average used below.
  const double half_day = 60.0 * 60.0 * 12.0;
  assert (end_time - start_time < half_day);

  // We will further assume that a reasonable number of control points
  // are being used.
  const int minimum_control_point_count = 100; 
  assert (control_point_count > minimum_control_point_count);

  // We will take the average rotation rate over this many different
  // segments.
  const int segments_to_average = 10;
  // Initializer here is just to reassure compiler.
  long double mean_rotation_rate = 0;
  for ( ii = 0 ; ii < segments_to_average ; ii++ ) {

    // First contol point index.
    int fcp = ii * control_point_count / segments_to_average;
    assert (fcp < control_point_count);
    // Second control point intex.
    int scp = (ii + 1) * control_point_count / segments_to_average - 1;
    assert (scp < control_point_count);

    // Get the earth angle at each of the control points which define
    // the end points of this segment.
    DateTime *tmp = date_time_copy (base_date);
    date_time_add_seconds (tmp, times[fcp]);
    long double fcp_earth_angle = date_time_earth_angle (tmp);
    date_time_add_seconds (tmp, times[scp] - times[fcp]);
    long double scp_earth_angle = date_time_earth_angle (tmp);
    date_time_free (tmp);

    long double rotation_rate_this_segment
      = (scp_earth_angle - fcp_earth_angle) / (times[scp] - times[fcp]);

    // Refine out notion of the mean rotation rate.
    if ( ii == 0 ) {
      mean_rotation_rate = rotation_rate_this_segment;
    }
    else {
      mean_rotation_rate 
	+= (rotation_rate_this_segment - mean_rotation_rate) / (ii + 1);
    }
  }

  // Contol point separation in time as a long double.
  long double cp_separation_ld 
    = (long double) time_range / (control_point_count - 1);

  // Earth rotation per control point.
  long double earth_rotation_per_cp = cp_separation_ld * mean_rotation_rate;

  // Find the earth angle at the first control point.
  DateTime *tmp = date_time_copy (base_date);
  date_time_add_seconds (tmp, times[0]);
  long double earth_angle_at_start_time = date_time_earth_angle (tmp);
  date_time_free (tmp);

  // Now we are ready to rotate all the contol points into ITRS.
  long double earth_angle = earth_angle_at_start_time;
  for ( ii = 0 ; ii < control_point_count ; ii++ ) {
    double tmp2 = xcp[ii];
    xcp[ii] = xcp[ii] * cosl (earth_angle) + ycp[ii] * sinl (earth_angle);
    ycp[ii] = ycp[ii] * cosl (earth_angle) - tmp2 * sinl (earth_angle);
    earth_angle += earth_rotation_per_cp;
  }

  // Now we are ready to create the splines.

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
ITRS_platform_path_position_at_time (ITRSPlatformPath *self, double time, 
				     Vector *position)
{
  // Ensure that the time queried is within the interval modeled.
  assert (time >= self->start_time && time <= self->end_time);

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

// Set velocity to the velocity of platform at time.
void
ITRS_platform_path_velocity_at_time (ITRSPlatformPath *self, double time, 
				     Vector *velocity)
{
  // Ensure that the time queried is within the interval modeled.
  assert (time >= self->start_time && time <= self->end_time);

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

void
ITRS_platform_path_free (ITRSPlatformPath *self)
{
  gsl_interp_accel_free (self->xp_accel);
  gsl_spline_free (self->xp);
  gsl_interp_accel_free (self->yp_accel);
  gsl_spline_free (self->yp);
  gsl_interp_accel_free (self->zp_accel);
  gsl_spline_free (self->zp);

  free (self);
}
