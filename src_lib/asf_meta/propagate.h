
#define GM 3.9860045e+5		/* Gravitational constant.  */

typedef struct {
  double a;			/* Semi-major axis.  */
  double e;			/* Eccentricity.  */
  double i;			/* Inclination.  */
  double cap_omega;		/* Longitude of ascending node.  */
  double omega;			/* Argument of periapsis.  */
} keplerian_orbit_t;

typedef struct {
  double x, y, z, vx, vy, vz;
} cartesian_orbit_t;

typedef struct {
  double dx, dy, dz, dvx, dvy, dvz;
} orbit_derivatives_t;

typedef struct {
  double h;			/* E * SIN(W + NODE) */
  double k;			/* E * COS(W + NODE) */
  double p;			/* TAN(I/2) * SIN(NODE) */
  double q;			/* TAN(I/2) * COS(NODE) */
  double lambda;		/* M + NODE + W (RAD) */
} equinoctial_elements_t;

typedef struct {
  double mean;
  double true;
  double eccentric;
} anomaly_t;

