// Placeholder file for the moment.

#include <stdlib.h>

#include <glib.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_min.h>

#include <asf_meta.h>

#include "orbital_state_vector.h"
#include "platform_path.h"

typedef struct {
  Vector *target;
  PlatformPath *pp;
} target_distance_params;

// Range function we want to minimize.
static double 
target_distance (double time, void *params)
{
  Vector *target = ((target_distance_params *) params)->target;
  PlatformPath *pp = ((target_distance_params *) params)->pp;

  static Vector platform_position;
  platform_path_position_at_time (pp, time, &platform_position);

  static Vector difference;
  vector_set (&difference, platform_position.x, platform_position.y, 
	      platform_position.z);

  vector_subtract (&difference, target);

  return vector_magnitude (&difference);
}

int
main (int argc, char **argv)
{
  argc = argc;
  argv = argv;

  GString *reference_dem = g_string_new (argv[argc - 3]);
  reference_dem = reference_dem; /* Remove this compiler reassurance.  */

  GString *input_meta_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_meta_file, ".meta");
  GString *input_data_file = g_string_new (argv[argc - 2]);
  g_string_append_printf (input_data_file, ".img");

  meta_parameters *imd = meta_read (input_meta_file->str);

  int svc = imd->state_vectors->vector_count;   // State vector count.

  double *observation_times = g_new (double, svc);
  OrbitalStateVector **observations = g_new (OrbitalStateVector *, svc);
  int ii;
  for ( ii = 0 ; ii < svc ; ii++ ) {
    observation_times[ii] = imd->state_vectors->vecs[ii].time;
    observations[ii] 
      = orbital_state_vector_new (imd->state_vectors->vecs[ii].vec.pos.x,
				  imd->state_vectors->vecs[ii].vec.pos.y,
				  imd->state_vectors->vecs[ii].vec.pos.z,
				  imd->state_vectors->vecs[ii].vec.vel.x,
				  imd->state_vectors->vecs[ii].vec.vel.y,
				  imd->state_vectors->vecs[ii].vec.vel.z);
  }

  // Number of control points to use for the cubic splines that
  // approximate the satellite motion in the PlatformPath.
  const int cpc = 10000;
  // Guard time in seconds to add on either side of the first and last
  // observations.  This will save us in case the point of closest
  // approach to some pixel is actually outside the time window for
  // which we are provided state vectors. (though cleanup of some sort
  // will still have to be done).
  const double gt = 2.0;
  PlatformPath *pp = platform_path_new (cpc, observation_times[0] - gt,
					observation_times[svc - 1] + gt,
					svc, observation_times, observations);
					
  // FIXME: remove this crud which is only meant to get us to the
  // point where things compile.
  double tx = 1.0, ty = 1.0, tz = 1.0;

  Vector *target = vector_new (tx, ty, tz);
  
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
  tdp.pp = pp;
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
  
  return status;

  
  exit (EXIT_SUCCESS);
}
