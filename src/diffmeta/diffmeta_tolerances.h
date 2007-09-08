#ifndef _DIFFMETA_TOLERANCES_H_
#define _DIFFMETA_TOLERANCES_H_

// General Block
#define DM_MAX_ORBIT          99999
#define DM_MIN_FRAME          -1
#define DM_MAX_FRAME          7200
#define DM_MIN_BANDCOUNT      1
#define DM_MAX_BANDCOUNT      MAX_BANDS // Rudi says 1000
#define DM_MIN_LINECOUNT      10
#define DM_MAX_LINECOUNT      1000000
#define DM_MIN_SAMPLECOUNT    10
#define DM_MAX_SAMPLECOUNT    10000
#define DM_MIN_STARTLINE      0
#define DM_MAX_STARTLINE      DM_MAX_LINECOUNT
#define DM_MIN_STARTSAMPLE    0
#define DM_MAX_STARTSAMPLE    DM_MAX_SAMPLECOUNT
#define DM_MIN_PIXELSIZE      1.0
#define DM_MAX_PIXELSIZE      1500.0
#define DM_MIN_MAJOR_AXIS     6377500.0
#define DM_MAX_MAJOR_AXIS     6378500.0
#define DM_MIN_MINOR_AXIS     6356000.0
#define DM_MAX_MINOR_AXIS     6357000.0
#define DM_MIN_BIT_ERROR_RATE 0.0
#define DM_MAX_BIT_ERROR_RATE 1.0
#define DM_MIN_MISSING_LINES  0
#define DM_MAX_MISSING_LINES  10000
#define DM_MIN_LATITUDE       -90.0
#define DM_MAX_LATITUDE       90.0
#define DM_MIN_LONGITUDE      -180.0
#define DM_MAX_LONGITUDE      180.0

// SAR Block
#define DM_MIN_LOOK_COUNT               1
#define DM_MAX_LOOK_COUNT               5
#define DM_MIN_DESKEWED                 0
#define DM_MAX_DESKEWED                 1
#define DM_MIN_ORIGINAL_LINE_COUNT      DM_MIN_LINECOUNT
#define DM_MAX_ORIGINAL_LINE_COUNT      DM_MAX_LINECOUNT
#define DM_MIN_ORIGINAL_SAMPLE_COUNT    DM_MIN_SAMPLECOUNT
#define DM_MAX_ORIGINAL_SAMPLE_COUNT    DM_MAX_SAMPLECOUNT
#define DM_MIN_LINE_INCREMENT           1.0
#define DM_MAX_LINE_INCREMENT           100.0
#define DM_MIN_SAMPLE_INCREMENT         1.0
#define DM_MAX_SAMPLE_INCREMENT         100.0
#define DM_MIN_RANGE_TIME_PER_PIXEL     1.0e-08
#define DM_MAX_RANGE_TIME_PER_PIXEL     1.0e-07
#define DM_MIN_AZIMUTH_TIME_PER_PIXEL   -0.001
#define DM_MAX_AZIMUTH_TIME_PER_PIXEL   0.001
#define DM_MIN_SLANT_SHIFT              -100.0
#define DM_MAX_SLANT_SHIFT              100.0
#define DM_MIN_TIME_SHIFT               -100.0
#define DM_MAX_TIME_SHIFT               100.0
#define DM_MIN_SLANT_RANGE_FIRST_PIXEL  750000.0
#define DM_MAX_SLANT_RANGE_FIRST_PIXEL  1200000.0
#define DM_MIN_WAVELENGTH               0.01
#define DM_MAX_WAVELENGTH               1.1
#define DM_MIN_PRF                      100.0
#define DM_MAX_PRF                      10000.0
#define DM_MIN_EARTH_RADIUS             6355000.0
#define DM_MAX_EARTH_RADIUS             6380000.0
#define DM_MIN_SATELLITE_HEIGHT         7000000.0
#define DM_MAX_SATELLITE_HEIGHT         7500000.0
#define DM_MIN_DOP_RANGE_CENTROID       0.0
#define DM_MAX_DOP_RANGE_CENTROID       3000.0
#define DM_MIN_DOP_RANGE_PER_PIXEL      -0.1
#define DM_MAX_DOP_RANGE_PER_PIXEL      0.1
#define DM_MIN_DOP_RANGE_QUAD           -1.0e-5
#define DM_MAX_DOP_RANGE_QUAD           1.0e-5
#define DM_MIN_DOP_AZIMUTH_CENTROID     0.0
#define DM_MAX_DOP_AZIMUTH_CENTROID     3000.0
#define DM_MIN_DOP_AZIMUTH_PER_PIXEL    -0.1
#define DM_MAX_DOP_AZIMUTH_PER_PIXEL    0.1
#define DM_MIN_DOP_AZIMUTH_QUAD         -1.0e-5
#define DM_MAX_DOP_AZIMUTH_QUAD         1.0e-5

// Optical Block
#define DM_MIN_OFF_NADIR_ANGLE      0.0
#define DM_MAX_OFF_NADIR_ANGLE      50.0
#define DM_MIN_CLOUD_PERCENTAGE     0.0
#define DM_MAX_CLOUD_PERCENTAGE     100.0
#define DM_MIN_SUN_AZIMUTH_ANGLE    0.0
#define DM_MAX_SUN_AZIMUTH_ANGLE    360.0
#define DM_MIN_SUN_ELEVATION_ANGLE  -90.0
#define DM_MAX_SUN_ELEVATION_ANGLE  90.0

// Thermal Block
#define DM_MIN_BAND_GAIN          0.0
#define DM_MAX_BAND_GAIN          1000000.0
#define DM_DAY                    1
#define DM_NIGHT                  0

// Projection Block
#define DM_MIN_STARTX             -1.0e6
#define DM_MAX_STARTX             1.0e6
#define DM_MIN_STARTY             -1.0e6
#define DM_MAX_STARTY             1.0e6
#define DM_MIN_PERX               1.0
#define DM_MAX_PERX               100.0
#define DM_MIN_PERY               1.0
#define DM_MAX_PERY               100.0
#define DM_MIN_TERRAIN_HEIGHT     -1000.0
#define DM_MAX_TERRAIN_HEIGHT     25000.0
//      UTM
#define DM_MIN_UTM_ZONE           1
#define DM_MAX_UTM_ZONE           60
#define DM_UTM_FALSE_EASTING      500000.0
#define DM_N_UTM_FALSE_NORTHING   0.0
#define DM_S_UTM_FALSE_NORTHING   10000000.0
#define DM_UTM_SCALE_FACTOR       0.9996
#define DM_DEFAULT_SCALE_FACTOR   1.0
//      STATE PLANE
#define DM_MIN_STATE_PLANE_ZONE   1
#define DM_MAX_STATE_PLANE_ZONE   100
//      ATCT
#define DM_MIN_ROTATION_ANGLE     -180.0
#define DM_MAX_ROTATION_ANGLE     180.0
//      LAMCC
// Note: The LAMCC scale factor only applies to the single standard
// parallel case for LAMCC.  If used, then it will be 1.0 to
// indicate that the latitude of origin is not scaled.  But if
// it is made a non-unity number then it is usually set slightly
// less than 1.0 and then 2 other latitudes where unity scale
// exists can be derived, consequently converting a single-standard
// parallel LAMCC into a 2-standard parallel LAMCC.  This is actually
// quite rare ...in fact, except for some early French projections
// that utilized the 1-SP LAMCC and non-unity scale factor, you don't
// see 1-SP projections very often.
#define DM_MIN_LAMCC_SCALE_FACTOR 0.0
#define DM_MAX_LAMCC_SCALE_FACTOR 1.0



#endif // _DIFFMETA_TOLERANCES_H_

