// A point in the International Terrestrial Reference System (ITRS).

#ifndef ITRS_POINT_H
#define ITRS_POINT_H

// Instance structure.  The data members may be read and set directly.
typedef struct {
  double x, y, z;
} ITRSPoint;
  
// Create a new instance.
ITRSPoint *
ITRS_point_new (double x, double y, double z);

// Create a new instance from geodetic latitude and longitude in
// radians and height in meters above (or below) the WGS84 ellipsoid.
ITRSPoint *
ITRS_point_new_from_geodetic_lat_long_height (double latitude, 
					      double longitude,
					      double height);

// Get geodetic latitude and longitude of self, assuming WGS84 ellipsoid.
void
ITRS_point_get_geodetic_lat_long (ITRSPoint *self, double *latiude, 
				  double *longitude);

// Free instance.
void
ITRS_point_free (ITRSPoint *self);

#endif // ITRS_POINT_H
