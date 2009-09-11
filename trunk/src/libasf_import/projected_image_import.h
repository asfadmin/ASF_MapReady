// Miscellaneous support routines to ease importing of projected images.

#ifndef PROJECTED_IMAGE_IMPORT_H
#define PROJECTED_IMAGE_IMPORT_H

// Given utm zone, return the central meridian (longitude of center of
// zone extent in longitude).
double
utm_zone_to_central_meridian (int zone);

// What the function name says :) The hemisphere return value is 'N'
// for a northern hemishpere zone and 'S' for a southern hemisphere
// zone.  This function with print and error and exit the program if
// the cs_type is not a WGS84 relative ProjectedCSTypeGeoKey code.
void
utm_zone_and_hemisphere_from_projected_coordinate_system_type_geotiff_geokey
  (short cs_type, char *hemisphere, int *zone);

#endif // PROJECTED_IMAGE_IMPORT_H
