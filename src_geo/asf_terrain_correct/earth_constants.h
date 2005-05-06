/* Constants due to the nature and shape of the earth.  These mostly
   come from the WGS84 earth model.  */

#ifndef EARTH_CONSTANTS_H
#define EARTH_CONSTANTS_H

#define EARTH_GRAVITATIONAL_CONSTANT 398.6004418e12
/* Note that this value of the earth rotation rate is approximate and
   steadily getting worse (the earth is slowing down), making in
   unsuitable for many calculations (often observational data is
   needed, for exmaple, to find the earth orientation).  */
#define EARTH_ROTATION_RATE 7.292115e-5
#define EARTH_SEMIMAJOR_AXIS 6378137
#define EARTH_FLATTENING (1 / 298.257223563)
#define EARTH_ECCENTRICITY_SQUARED (2 * EARTH_FLATTENING \
                                    - EARTH_FLATTENING * EARTH_FLATTENING)

#endif /* EARTH_CONSTANTS_H */
