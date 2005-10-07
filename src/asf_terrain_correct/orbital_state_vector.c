/* Implementation of the interface in orbital_state_vector.h.  */

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include <gsl/gsl_blas.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv.h>
#include <gsl/gsl_vector.h>

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

  /* FIXME: improve reference.  */
  /* Fundamental vectors as described in "Satellite Geodesy".  */
  Vector *h = vector_cross (self->position, self->velocity);
  Vector *X = vector_new (1.0, 0.0, 0.0);
  Vector *Z = vector_new (0.0, 0.0, 1.0);
  Vector *n = vector_cross (Z, h);
  double r_dot_v = vector_dot (self->position, self->velocity);
  double r_mag = vector_magnitude (self->position);
  double v_mag = vector_magnitude (self->velocity);
  Vector *tmp1 = vector_copy (self->position);
  vector_multiply (tmp1, pow (v_mag, 2) - gm / r_mag);
  Vector *tmp2 = vector_copy (self->velocity);
  vector_multiply (tmp2, r_dot_v);
  vector_subtract (tmp1, tmp2);
  vector_multiply (tmp1, (1.0 / gm));
  /* Fundamental vector 'e' ('_fv' is appended to dodge name clash).  */
  Vector *e_fv = vector_copy (tmp1);

  /* Compute keplerian elements.  */
  *e = vector_magnitude (e_fv);
  double h_mag = vector_magnitude (h);
  *a = (pow (h_mag, 2) / gm) / (1 - pow (*e, 2));
  *i = acos (vector_dot (h, Z) / h_mag);
  double n_mag = vector_magnitude (n);
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

void
orbital_state_vector_get_itrs_coordinates (OrbitalStateVector *self, 
					   double theta,
					   double *x, double *y, double *z)
{
  /* The current GEI coordinates in GNU Scientific Library vector
     form.  */
  gsl_vector *gei_pos = gsl_vector_alloc (3);
  gsl_vector_set (gei_pos, 0, self->position->x);
  gsl_vector_set (gei_pos, 1, self->position->y);
  gsl_vector_set (gei_pos, 2, self->position->z);

  /* FIXME: improve reference.  */
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
  gsl_vector *itrs_pos = gsl_vector_alloc (3);
  gsl_vector_set_zero (itrs_pos);

  /* Perform the rotation.  */
  int return_code = gsl_blas_dgemv (CblasNoTrans, 1.0, earm, gei_pos, 0.0, 
				    itrs_pos);
  assert (return_code == GSL_SUCCESS);
  
  // Assign results into object.
  *x = gsl_vector_get (itrs_pos, 0);
  *y = gsl_vector_get (itrs_pos, 1);
  *z = gsl_vector_get (itrs_pos, 2);

  // Free intermediate objects.
  gsl_vector_free (itrs_pos);
  gsl_matrix_free (earm);
  gsl_vector_free (gei_pos);
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
  double be = sqrt (pow (ae, 2) * (1 - EARTH_ECCENTRICITY_SQUARED));
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
  /* The perturbations we consider are time-independent, so we don't
     use t.  We build in the couple of parameters we need, so we don't
     use the params argument either.  Reassure the compiler about all
     this non-use of arguments.  */
  t = t;
  params = params;
  
  /* Shorthand for some earth constants.  */
  static const double gm = EARTH_GRAVITATIONAL_CONSTANT;
  static const double ae = EARTH_SEMIMAJOR_AXIS;

  /* Create position and velocity vectors.  We use static vectors to
     avoid allocation overhead.  */
  static Vector p_st;
  static Vector v_st;
  static Vector *p = &p_st;
  static Vector *v = &v_st;
  vector_set (p, y[0], y[1], y[2]);
  vector_set (v, y[3], y[4], y[5]);

  /* Current range to satellife from center of earth.  */
  double r = vector_magnitude (p);

  /* The so-called first zonal harmonic is used to account for the
     effects of earth oblatedness (see below for references).  */
  double j2 = 1082.63e-6;	/* First zonal harmonic coefficient.  */

  /* Shorthand for some terms that show up in the expressions for the
     perturbations.  */
  double r2 = pow (r, 2);
  double r3 = pow (r, 3);
  double px = p->x;
  double py = p->y;
  double pz = p->z;
  double pz2 = pow (p->z, 2);

  /* Perturbations due to earth oblatedness.  */
  double a_j2_x = ((gm * px / r3)
			* ((j2 * (3.0 / 2)
			    * pow ((ae / r), 2)
			    * (5.0 * pz2 / r2 - 1.0))));
  double a_j2_y = py / px * a_j2_x;
  double a_j2_z = ((-gm * pz / r3)
			* ((j2 * (3.0 / 2)
			    * pow ((ae / r), 2)
			    * (3.0 - 5.0 * pz2 / r2))));

  /* Total perturbations.  */
  double ksx = a_j2_x;
  double ksy = a_j2_y;
  double ksz = a_j2_z;

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
     oblatedness as described in section 3.2.2.3 of "Satellite
     Geodesy", 2nd edition, by Gunter Seeber.  An ordinary
     differential equation solver from the GNU Scientific Library
     (GSL) is used to solve the system.  It is well described in the
     GSL documentation section "Ordinary Differential Equations".  */

  const int dimension = 6;
  /* Create some things the ODE solver uses.  */
  //  const gsl_odeiv_step_type *step_type = gsl_odeiv_step_rkf45;  
  const gsl_odeiv_step_type *step_type = gsl_odeiv_step_rk4;
  gsl_odeiv_step *ode_step = gsl_odeiv_step_alloc (step_type, dimension);
  /* Set the absolute and relative integration error bounds for each
     step.  See the GNU Scientific Library documentation for
     detaile.  */
  double mae = 0.000001;
  double mre = 0.0001;
  gsl_odeiv_control *ode_control 
    = gsl_odeiv_control_standard_new (mae, mre, 1.0, 1.0);
    //  gsl_odeiv_control *ode_control = gsl_odeiv_control_y_new (mae, 0.0);
  gsl_odeiv_evolve *ode_evolve = gsl_odeiv_evolve_alloc (dimension);
  const gsl_odeiv_system ode_system = {func, NULL, dimension, NULL};
  double t0 = 0.0, t1 = time;	/* Start and end times.  */
 /* Initial guess for step size.  */
  const double initial_step_size = GSL_SIGN (t1) * 1.0;
  double step_size = initial_step_size;
  double *y = malloc (dimension * sizeof (double));
  y[0] = self->position->x;
  y[1] = self->position->y;
  y[2] = self->position->z;
  y[3] = self->velocity->x;
  y[4] = self->velocity->y;
  y[5] = self->velocity->z;
  double t = t0;		/* Current time.  */
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
  /* Not implemented yet.  */
  self = self; time = time; other = other; /* Compiler reassurance.  */

  assert (FALSE);
}

void
orbital_state_vector_free (OrbitalStateVector *self)
{
  vector_free (self->position);
  vector_free (self->velocity);

  free (self);
}
