/* The position and velocity of an earth orbiting (FIXME: define orbit
   type more precisely) satellite in geocentric equitorial inertial
   (GEI) form.  This is also sometimes confusingly called the space
   fixed form, and it records the satellite's position in a reference
   system defined by axes through the earth's pole, equator, and a
   line pointing off in a direction defined relative to a bunch of
   quasars and other distant space gunk.  Note that THIS SYSTEM IS NOT
   THE SAME as the International Celestial Reference System (ICRS),
   which doesn't force the Z axis to follow he precession and nutation
   or the earth's pole.  The only thing this system is independent of
   is short time day to day rotation of the earth.  All algorithms
   used in this class are from Chapters 1 through 3 of the book
   "Satellite Geodesy", 2nd edition, by Gunter Seeber.  This reference
   doesn't mention GEI explicitly, but the "equatorial system"
   mentioned is the GEI system.  */

#ifndef ORBITAL_STATE_VECTOR_H
#define ORBITAL_STATE_VECTOR_H

#include "vector.h"

/* The position and velocity data members may be read directly, all
   others should always be accessed through the accessor functions.  */
typedef struct {
  Vector *position;
  Vector *velocity;
} OrbitalStateVector;

/* Create a new state vector with position x, y, z and velocity vx,
   vy, vz.  */
OrbitalStateVector *
orbital_state_vector_new (double x, double y, double z, double vx, double vy,
			  double vz);

/* Create a new independent copy of model OrbitalStateVector a.  */
OrbitalStateVector *
orbital_state_vector_copy (OrbitalStateVector *a);

/* Get the equivalent keplerian elements, as described in section
   3.1.2.1 of the above referenced "Satellite Geodesy".  The elements
   are: a: semi-major axis, e: numerical eccentricity, i: orbit
   inclination, capital_omega: right ascension of ascending node,
   lower_case_omega: argument of perigee, nu: true anomaly, E:
   eccentric anomaly, and M: mean anomaly.  The odd names honor well
   established convention.  Note that nu, E, and M are different
   statements of the same parameter, as described in section 3.1.3 of
   the above referenced "Satellite Geodesy".  */
void
orbital_state_vector_get_keplerian_elements 
(OrbitalStateVector *self, double *a, double *e, double *i, 
 double *capital_omega, double *lower_case_omega, double *nu, double *E, 
 double *M);

/* Get the equivalent International Terrestrial Reference System
   coordinates, given earth rotation angle theta.  This method should
   account for precession and nutation, since they are built into the
   GEI system, but does not account for pole offsets (see secion
   2.1.2.3 of the above referenced Satellite Geodesy).  The pole
   offsets are typically ~0.3 arc seconds, equivalent to ~9 m on the
   earth's surface.  */
void
orbital_state_vector_get_itrs_coordinates (OrbitalStateVector *self, 
					   double theta,
					   double *x, double *y, double *z);

/* Height in meters of the satellite above the WGS84 ellipsoid.  */
double
orbital_state_vector_height_above_ellipsoid (OrbitalStateVector *self);

/* Propagate self forward time seconds.  This method performs a
   newtonian propagation forward or backward in time with a single
   perturbing force: the non-central force due to the eccentricity of
   the earth.  Only the so-called first zonal harmonic is used.
   Accounting for this basic perturbation will probably put us within
   400 meters or so for a two hour propagation, or within 2 meters for
   a 2.5 minute propagation, buuut... for simplicity we use cartesian
   instead of polar coordinates for the propagation, and the method of
   Cowell is used, i.e. we solve the differential equation in one
   piece, mixing the central force and the relatively small
   perturbations.  Both of these choices potentially cause lots of
   rounding error.  It all seems to work, but if there is a problem,
   adding test cases appropriate to your particular application to the
   propagator tests might be a good first step.  */
void
orbital_state_vector_propagate (OrbitalStateVector *self, double time);

/* This is an unimplemented placeholder for a popular method where you
   propage one state vector forward, and the other backward, and then
   perform an appropriately weighted interpolation between the two
   results.  It isn't implemented yet because its not yet clear that
   state vectors should carry times around with them.  Iff they do,
   then a relative time for other isn't needed.  */
void 
orbital_state_vector_propagate_interpolate (OrbitalStateVector *self, 
					    double time, 
					    OrbitalStateVector *other);

/* Destroy self.  */
void
orbital_state_vector_free (OrbitalStateVector *self);

#endif /* ORBITAL_STATE_VECTOR_H */
