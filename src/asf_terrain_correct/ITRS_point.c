// Implementation of the interface described in ITRS_point.h.

// FIXME: this is here only for debugging, remove it.
#include <stdio.h>

#include <assert.h>
#include <stdlib.h>

#include <gsl/gsl_math.h>

#include "ITRS_point.h"
#include "earth_constants.h"
#include "vector.h"

ITRSPoint *
ITRS_point_new (double x, double y, double z)
{
  ITRSPoint *self = calloc (1, sizeof (ITRSPoint));

  self->x = x;
  self->y = y;
  self->z = z;

  return self;
}

ITRSPoint *
ITRS_point_new_from_geodetic_lat_long_height (double latitude,
					      double longitude,
					      double height)
{
  ITRSPoint *self = calloc (1, sizeof (ITRSPoint));

  double e2 = EARTH_ECCENTRICITY_SQUARED;

  // Length of semimajor (equitorial) axis of WGS84 ellipsoid.
  double re = EARTH_SEMIMAJOR_AXIS;

  double r = re / (sqrt (1.0 - e2 * pow (sin (latitude), 2.0)));

  // Static vector objects (so we don't have to allocate things every
  // time through.
  static Vector tmp1, tmp2;

  // Vector of the point on the surface of the ellipsoid.
  vector_set (&tmp1, 
	      r * cos (longitude) * cos (latitude),
	      r * sin (longitude) * cos (latitude),
	      r * (1 - e2) * sin (latitude));

  // Unit vector in the direction of surface vector.
  vector_set (&tmp2, tmp1.x, tmp1.y, tmp1.z);
  vector_multiply (&tmp2, (1.0 / vector_magnitude (&tmp1)));

  // Add height to surface vector to get the result desired.
  vector_multiply (&tmp2, height);
  vector_add (&tmp1, &tmp2);

  self->x = tmp1.x;
  self->y = tmp1.y;
  self->z = tmp1.z;

  return self;
}

void
ITRS_point_get_geodetic_lat_long (ITRSPoint *self, double *latitude, 
				  double *longitude)

{
  // Convenience aliases.
  double x = self->x, y = self->y, z = self->z;  

  double geocentric_lat = atan (z / sqrt (pow (x, 2.0) + pow (y, 2.0)));
  *longitude = atan2 (y, x);

  // For angles near zero or pi / 2, the normal geocentric to geodetic
  // calculation doesn't work, and indeed geocentric angles in these
  // regimes are almost equal to the geodetic angles anyway.  So for
  // angles within this many radians of 0.0 or +/- pi / 2, we just return
  // the geocentric latitude.
  double sa = 0.000001;

  if ( fabs (geocentric_lat) < sa || fabs (geocentric_lat) > M_PI / 2 - sa ) {
    *latitude = geocentric_lat;
  }
  else {
    // Ellipticity squared of WGS84 ellipsoid.
    double e2 = 0.00669437999014;

    *latitude = atan (tan(geocentric_lat) / ( 1.0 - e2));
  }  
}

void
ITRS_point_free (ITRSPoint *self)
{
  free (self);
}
