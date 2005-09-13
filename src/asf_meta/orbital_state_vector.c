/* Implementation of the interface in orbital_state_vector.h.  */

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_odeiv.h>

#include "basic_types.h"
#include "earth_constants.h"
#include "orbital_state_vector.h"

OrbitalStateVector *
orbital_state_vector_new (double x, double y, double z, double vx, double vy,
			  double vz)
{
  OrbitalStateVector *self = malloc (sizeof (OrbitalStateVector));

  self->position = vector_new (x, y, z);
  self->velocity = vector_new (vx, vy, vz);

  return self;
}

OrbitalStateVector *
orbital_state_vector_copy (OrbitalStateVector *a)
{
  OrbitalStateVector *new = malloc (sizeof (OrbitalStateVector));

  new->position = vector_copy (a->position);
  new->velocity = vector_copy (a->velocity);

  return new;
}

void
orbital_state_vector_get_keplerian_elements 
(OrbitalStateVector *self, double *a, double *e, double *i, 
 double *capital_omega, double *lower_case_omega, double *nu, double *E, 
 double *M)
{
  /* Shorthand.  */
  static const double gm = EARTH_GRAVITATIONAL_CONSTANT;

  /* Fundamental vectors as described in "Satellite Geodesy".  */
  Vector *h = vector_cross (self->position, self->velocity);
  Vector *X = vector_new (1.0, 0.0, 0.0);
  Vector *Z = vector_new (0.0, 0.0, 1.0);
  Vector *n = vector_cross (Z, h);
  double r_dot_v = vector_dot (self->position, self->velocity);
  double r_mag = vector_magnitude (self->position);
  double v_mag = vector_magnitude (self->velocity);
  Vector *tmp1 = vector_copy (self->position);
  Vector *tmp2 = vector_copy (self->velocity);
  /* Fundamental vector 'e' ('_fv' is appended to dodge name clash).  */
  Vector *e_fv;
  double h_mag;
  double n_mag;

  vector_multiply (tmp1, pow (v_mag, 2) - gm / r_mag);
  vector_multiply (tmp2, r_dot_v);
  vector_subtract (tmp1, tmp2);
  vector_multiply (tmp1, (1.0 / gm));
  e_fv = vector_copy (tmp1);

  /* Compute keplerian elements.  */
  *e = vector_magnitude (e_fv);
  h_mag = vector_magnitude (h);
  *a = (pow (h_mag, 2) / gm) / (1 - pow (*e, 2));
  *i = acos (vector_dot (h, Z) / h_mag);
  n_mag = vector_magnitude (n);
  *capital_omega = acos (vector_dot (n, X) / n_mag);
  *lower_case_omega = acos (vector_dot (n, e_fv) / (n_mag * (*e)));
  *nu = acos (vector_dot (e_fv, self->position) 
	      / ((*e) * vector_magnitude (self->position)));
  *E = acos ((*e + cos (*nu)) / (1.0 + (*e) * cos (*nu)));
  *M = *E - (*e) * sin (*E);

  /* Free fundamental and temporary vectors used in computation.  */
  free (h);
  free (Z);
  free (X);
  free (n);
  free (tmp1);
  free (tmp2);
  free (e_fv);
}

double
orbital_state_vector_height_above_ellipsoid (OrbitalStateVector *self)
{
  double r = vector_magnitude (self->position);

  /* Compute height of satellite above the WGS84 ellipsoid.  */
  static Vector z_unit = {0.0, 0.0, 1.0};
  /* Angle between position vector and equatorial plane.  */
  double eq_angle = M_PI / 2 - vector_angle (self->position, &z_unit);
  double ae = EARTH_SEMIMAJOR_AXIS;
  /* Earth semiminor axis.  */
  double be = sqrt (pow (ae, 2) * (1 - pow (EARTH_ECCENTRICITY, 2)));
  /* Radius of ellipsoid at point under satellite.  */
  double ellipsoid_radius = sqrt (pow (ae * cos (eq_angle), 2)
				  + pow (be * sin (eq_angle), 2));

  return r - ellipsoid_radius;
}

/* Compute derivatives f from functions y(t), t, and other
   miscellaneous parameters.  Return GSL_SUCCESS after successful
   computation.  This code mostly follows the example in the GNU
   Scientific Library documentation, which uses a y array for the
   function values.  The y[1] value is the only one that corresponds
   to the y coordinate, and the y[4] vale the only one that
   corresponds to the y velocity.  */
static int
func (double t, const double y[], double f[], void *params)
{
  /* Shorthand for some earth constants.  */
  static const double gm = EARTH_GRAVITATIONAL_CONSTANT;
  static const double ae = EARTH_SEMIMAJOR_AXIS;

  /* Create position and velocity vectors.  We use static vectors to
     avoid allocation overhead.  */
  static Vector p_st;
  static Vector v_st;
  static Vector *p = &p_st;
  static Vector *v = &v_st;

  double r;
  double j2;
  double a_j2_x, a_j2_y, a_j2_z;
  double ksx, ksy, ksz;

  vector_set (p, y[0], y[1], y[2]);
  vector_set (v, y[3], y[4], y[5]);

  /* Current range to satellife from center of earth.  */
  r = vector_magnitude (p);

  /* The so-called first zonal harmonic is used to account for the
     effects of earth oblatedness (see below for references).  */
  j2 = 1082.63e-6;	/* First zonal harmonic coefficient.  */
  a_j2_x = (gm * p->x / pow (r, 3)) * j2 * (3.0 / 2) * pow ((ae / r), 2)
    * (5.0 * (pow (p->z, 2) / pow (r, 2)) - 1.0);
  a_j2_y = (p->y / p->x) * a_j2_x;
  a_j2_z = (-gm * p->z / pow (r, 3)) * j2 * (3.0 / 2) 
    * pow ((ae / r), 2) * (3.0 - 5.0 * (pow (p->z, 2) / pow (r, 2)));

  /* Total perturbations.  */
  ksx = a_j2_x;
  ksy = a_j2_y;
  ksz = a_j2_z;

  f[0] = v->x;
  f[1] = v->y;
  f[2] = v->z;
  f[3] = ksx - gm * (p->x / pow (r, 3));
  f[4] = ksy - gm * (p->y / pow (r, 3));
  f[5] = ksz - gm * (p->z / pow (r, 3));

  return GSL_SUCCESS;
}

void
orbital_state_vector_propagate (OrbitalStateVector *self, double time)
{
  /* We need to solve the following six dimensional system of ordinary
     differential equations:

         dx/dt = vx
	 dy/dt = vy
	 dz/dt = vz

	 dvx/dt = ksx - (GM / r^3) * x
	 dvy/dt = ksy - (GM / r^3) * y
	 dvz/dt = ksz - (GM / r^3) * z

     where

         r = sqrt (x^2 + y^2 + z^2)

     with initial conditions given by the current state.  The values
     ksx, ksy, and ksz represent the perturbations due to earth
     oblatedness as described in section 3.2.2.3 respectively of
     "Satellite Geodesy", 2nd edition, by Gunter Seeber.  An ordinary
     differential equation solver from the GNU Scientific Library
     (GSL) is used to solve the system.  It is well described in the
     GSL documentation section "Ordinary Differential Equations".  */

  const int dimension = 6;
  /* Create some things the ODE solver uses.  */
  const gsl_odeiv_step_type *step_type = gsl_odeiv_step_rkf45;  
  gsl_odeiv_step *ode_step = gsl_odeiv_step_alloc (step_type, dimension);
  /* Hold the absolute integration error for each coordinate for each
     step to within this value.  This value was chosen using the
     following sophisticated methodology: Keep tightening the
     tolerance until the answer no longer changes with further
     tightenings, for an short (~2.5 minute) propagation.  */
  double mae = 0.000001;
  gsl_odeiv_control *ode_control = gsl_odeiv_control_y_new (mae, 0.0);
  gsl_odeiv_evolve *ode_evolve = gsl_odeiv_evolve_alloc (dimension);
  gsl_odeiv_system ode_system;
  double t0 = 0.0, t1 = time;	/* Start and end times.  */
 /* Initial guess for step size.  */
  const double initial_step_size = GSL_SIGN (t1) * 1.0;
  double step_size = initial_step_size;
  double *y = malloc (dimension * sizeof (double));
  double t = t0;		/* Current time.  */

  ode_system.function = func;
  ode_system.jacobian = NULL;
  ode_system.dimension = dimension;
  ode_system.params = NULL;

  y[0] = self->position->x;
  y[1] = self->position->y;
  y[2] = self->position->z;
  y[3] = self->velocity->x;
  y[4] = self->velocity->y;
  y[5] = self->velocity->z;
  /* Here is the actual propagation.  */
  while ( fabs (t) < fabs (t1) ) {
    int status = gsl_odeiv_evolve_apply (ode_evolve, ode_control, ode_step, 
					 &ode_system, &t, t1, &step_size, y);
    assert (status == GSL_SUCCESS);
  }
  self->position->x = y[0];
  self->position->y = y[1];
  self->position->z = y[2];
  self->velocity->x = y[3];
  self->velocity->y = y[4];
  self->velocity->z = y[5];  

  free (y);
  gsl_odeiv_evolve_free (ode_evolve);
  gsl_odeiv_control_free (ode_control);
  gsl_odeiv_step_free (ode_step);
}

void
orbital_state_vector_propagate_interpolate (OrbitalStateVector *self, 
					    double time, 
					    OrbitalStateVector *other)
{
  /* Not implemented.  */
  assert (FALSE);
}
