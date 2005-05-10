// A satellite path modelled in International Terrestrial Reference
// System (ITRS) coordinates.  This class is analogous to the
// platform_path class the interface of which is described in
// platform_path.h, but models the position and velocity of a
// satellite in a different reference frame.  However, Geocentric
// Equitorial Inertial (GEI) coordinates are still required for the
// observations from which the model is constructed.

#ifndef ITRS_PLATFORM_PATH_H
#define ITRS_PLATFORM_PATH_H

#include <gsl/gsl_spline.h>

#include "date_time.h"
#include "orbital_state_vector.h"
#include "vector.h"

// It may be useful to read the start_time and end_time members
// directly, everything else in this instance structure is private.
typedef struct {
  double start_time;
  double end_time;
  gsl_interp_accel *xp_accel;
  gsl_spline *xp;
  gsl_interp_accel *yp_accel;
  gsl_spline *yp;
  gsl_interp_accel *zp_accel;
  gsl_spline *zp;
} ITRSPlatformPath;

// Create a new instance modeling platform position with
// control_point_count point splines from start_time to end_time using
// the observation_count Geocentric Equitorial Inertial (GEI)
// observations having times observation_times and positions
// observation_positions, where the start_time and end_time and the
// observation times are in seconds relative to base_date.
ITRSPlatformPath *
ITRS_platform_path_new (int control_point_count, double start_time, 
			double end_time, int observation_count, 
			DateTime *base_date, double *observation_times, 
			OrbitalStateVector **observations);

// Set position to the position of platform at time.  The time
// argument must fall between start_time and end_time used on
// creation.
void
ITRS_platform_path_position_at_time (ITRSPlatformPath *self, double time, 
				     Vector *position);

// Set velocity to the velocity of platform at time.  The time
// argument must fall between start_time and end_time used on
// creation.
void
ITRS_platform_path_velocity_at_time (ITRSPlatformPath *self, double time, 
				     Vector *velocity);

// Free self.
void
ITRS_platform_path_free (ITRSPlatformPath *self);

#endif // ITRS_PLATFORM_PATH_H
