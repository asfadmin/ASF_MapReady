#ifndef _DIFFMETA_TOLERANCES_H_
#define _DIFFMETA_TOLERANCES_H_

///////////////////////////////////////////////////////////
// PRECHECK
///////////////////////////////////////////////////////////

// General Block
#define DM_MAX_ORBIT          99999
#define DM_MIN_FRAME          -1
#define DM_MAX_FRAME          7200
#define DM_MIN_BAND_COUNT     1
#define DM_MAX_BAND_COUNT     MAX_BANDS // Rudi says 1000
#define DM_MIN_LINE_COUNT     10
#define DM_MAX_LINE_COUNT     1000000
#define DM_MIN_SAMPLE_COUNT   10
#define DM_MAX_SAMPLE_COUNT   40000
#define DM_MIN_START_LINE     0
#define DM_MAX_START_LINE     DM_MAX_LINE_COUNT
#define DM_MIN_START_SAMPLE   0
#define DM_MAX_START_SAMPLE   DM_MAX_SAMPLE_COUNT
#define DM_MIN_PIXEL_SIZE     0
#define DM_MAX_PIXEL_SIZE     1500.0
#define DM_MIN_X_PIXEL_SIZE   DM_MIN_PIXEL_SIZE
#define DM_MAX_X_PIXEL_SIZE   DM_MAX_PIXEL_SIZE
#define DM_MIN_Y_PIXEL_SIZE   DM_MIN_PIXEL_SIZE
#define DM_MAX_Y_PIXEL_SIZE   DM_MAX_PIXEL_SIZE
#define DM_MIN_RE_MAJOR       6377000.0
#define DM_MAX_RE_MAJOR       6378500.0
#define DM_MIN_RE_MINOR       6356000.0
#define DM_MAX_RE_MINOR       6357000.0
#define DM_MIN_BIT_ERROR_RATE 0.0
#define DM_MAX_BIT_ERROR_RATE 1.0
#define DM_MIN_MISSING_LINES  0
#define DM_MAX_MISSING_LINES  10000
#define DM_MIN_LATITUDE       -90.0
#define DM_MAX_LATITUDE       90.0
#define DM_MIN_LONGITUDE      -359.999999
#define DM_MAX_LONGITUDE      359.999999

// SAR Block
#define DM_MIN_LOOK_COUNT               1
#define DM_MAX_LOOK_COUNT               8
#define DM_MIN_DESKEWED                 0
#define DM_MAX_DESKEWED                 1
#define DM_MIN_ORIGINAL_LINE_COUNT      DM_MIN_LINE_COUNT
#define DM_MAX_ORIGINAL_LINE_COUNT      DM_MAX_LINE_COUNT
#define DM_MIN_ORIGINAL_SAMPLE_COUNT    DM_MIN_SAMPLE_COUNT
#define DM_MAX_ORIGINAL_SAMPLE_COUNT    DM_MAX_SAMPLE_COUNT
#define DM_MIN_LINE_INCREMENT           1.0
#define DM_MAX_LINE_INCREMENT           100.0
#define DM_MIN_SAMPLE_INCREMENT         1.0
#define DM_MAX_SAMPLE_INCREMENT         100.0
#define DM_MIN_RANGE_TIME_PER_PIXEL     1.0e-08
#define DM_MAX_RANGE_TIME_PER_PIXEL     1.0e-05
#define DM_MIN_AZIMUTH_TIME_PER_PIXEL   -0.07
#define DM_MAX_AZIMUTH_TIME_PER_PIXEL   0.07
#define DM_MIN_SLANT_SHIFT              -1100.0
#define DM_MAX_SLANT_SHIFT              1100.0
#define DM_MIN_TIME_SHIFT               -10000000.0
#define DM_MAX_TIME_SHIFT               10000000.0
#define DM_MIN_SLANT_RANGE_FIRST_PIXEL  650000.0
#define DM_MAX_SLANT_RANGE_FIRST_PIXEL  1200000.0
#define DM_MIN_WAVELENGTH               0.01
#define DM_MAX_WAVELENGTH               1.1
#define DM_MIN_PRF                      100.0
#define DM_MAX_PRF                      10000.0
#define DM_MIN_EARTH_RADIUS             6355000.0
#define DM_MAX_EARTH_RADIUS             6380000.0
#define DM_MIN_SATELLITE_HEIGHT         6800000.0
#define DM_MAX_SATELLITE_HEIGHT         7500000.0
#define DM_MIN_DOP_RANGE_CENTROID       -15000.0
#define DM_MAX_DOP_RANGE_CENTROID       15000.0
#define DM_MIN_DOP_RANGE_PER_PIXEL      -1.10
#define DM_MAX_DOP_RANGE_PER_PIXEL      1.10
#define DM_MIN_DOP_RANGE_QUAD           -4.0e-5
#define DM_MAX_DOP_RANGE_QUAD           4.0e-5
#define DM_MIN_DOP_AZIMUTH_CENTROID     -15000.0
#define DM_MAX_DOP_AZIMUTH_CENTROID     15000.0
#define DM_MIN_DOP_AZIMUTH_PER_PIXEL    -0.80
#define DM_MAX_DOP_AZIMUTH_PER_PIXEL    0.80
#define DM_MIN_DOP_AZIMUTH_QUAD         -4.0e-5
#define DM_MAX_DOP_AZIMUTH_QUAD         4.0e-5

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
#define DM_MIN_BAND_GAIN_CHANGE   DM_MIN_BAND_GAIN
#define DM_MAX_BAND_GAIN_CHANGE   DM_MAX_BAND_GAIN
#define DM_MIN_DAY                0
#define DM_MAX_DAY                1

// Projection Block
#define DM_MIN_STARTX             -1.0e7
#define DM_MAX_STARTX             1.0e7
#define DM_MIN_STARTY             -1.0e7
#define DM_MAX_STARTY             1.0e7
#define DM_MIN_PERX               0.2
#define DM_MAX_PERX               1500.0
#define DM_MIN_PERY               -DM_MAX_PERX
#define DM_MAX_PERY               -DM_MIN_PERX
#define DM_GEOCENTRIC_MIN_PERX    0.0001
#define DM_GEOCENTRIC_MAX_PERX    0.0010
#define DM_GEOCENTRIC_MIN_PERY    -DM_GEOCENTRIC_MAX_PERX
#define DM_GEOCENTRIC_MAX_PERY    -DM_GEOCENTRIC_MIN_PERX
#define DM_MIN_HEIGHT             -2000.0
#define DM_MAX_HEIGHT             25000.0
#define DM_MIN_SPHERE             6370000.0
#define DM_MAX_SPHERE             6378500.0
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
#define DM_MIN_ROTATION_ANGLE     -360.0
#define DM_MAX_ROTATION_ANGLE     360.0
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

// State Vector Blocks
#define DM_MIN_YEAR               1970
#define DM_MAX_YEAR               2099
#define DM_MIN_JULIAN_DAY         1
#define DM_MAX_JULIAN_DAY         366
#define DM_MIN_SECOND             0.0
#define DM_MAX_SECOND             86400.0
#define DM_MIN_VECTOR_COUNT       1
#define DM_MAX_VECTOR_COUNT       100
#define DM_MIN_VEC_TIME           -10000.0
#define DM_MAX_VEC_TIME           10000.0
#define DM_MIN_ECR_COORD          -10000000.0
#define DM_MAX_ECR_COORD          10000000.0
#define DM_MIN_ECR_VEL            -10000.0
#define DM_MAX_ECR_VEL            10000.0

// AirSAR Block
#define DM_MIN_AIRSAR_SCALE_FACTOR      0.0
#define DM_MAX_AIRSAR_SCALE_FACTOR      10.0
#define DM_MIN_GPS_ALTITUDE             -2000.0
#define DM_MAX_GPS_ALTITUDE             25000.0
#define DM_MIN_LAT_PEG_POINT            -90.0
#define DM_MAX_LAT_PEG_POINT            90.0
#define DM_MIN_LON_PEG_POINT            -180.0
#define DM_MAX_LON_PEG_POINT            360.0
#define DM_MIN_HEADING_PEG_POINT        0.0
#define DM_MAX_HEADING_PEG_POINT        360.0
#define DM_MIN_ALONG_TRACK_OFFSET       -500000.0
#define DM_MAX_ALONG_TRACK_OFFSET       500000.0
#define DM_MIN_CROSS_TRACK_OFFSET       -500000.0
#define DM_MAX_CROSS_TRACK_OFFSET       500000.0

// Calibration Block
#define DM_MIN_COEFFICIENT_A1           -10.0
#define DM_MAX_COEFFICIENT_A1           10.0
#define DM_MIN_COEFFICIENT_A2           -10.0
#define DM_MAX_COEFFICIENT_A2           10.0
#define DM_MIN_COEFFICIENT_A3           -10.0
#define DM_MAX_COEFFICIENT_A3           10.0

///////////////////////////////////////////////////////////
// BASELINE COMPARISON
///////////////////////////////////////////////////////////

// General Block
#define DM_PIXEL_SIZE_M_TOL             0.0025

// SAR Block
#define DM_RANGE_TIME_PER_PIXEL_TOL     2.5e-09
#define DM_SLANT_SHIFT_TOL              1.0
#define DM_TIME_SHIFT_TOL               0.5
#define DM_SLANT_RANGE_FIRST_PIXEL_TOL  10.0
#define DM_WAVELENGTH_TOL               0.0025
#define DM_PRF_TOL                      0.5
#define DM_EARTH_RADIUS_TOL             3.0
#define DM_SATELLITE_HEIGHT_TOL         3.0
#define DM_DOP_RANGE_CENTROID_TOL       1.0
#define DM_DOP_RANGE_PER_PIXEL_TOL      0.005
#define DM_DOP_RANGE_QUAD_TOL           5.0e-07
#define DM_DOP_AZIMUTH_CENTROID_TOL     1.0
#define DM_DOP_AZIMUTH_PER_PIXEL_TOL    0.005
#define DM_DOP_AZIMUTH_QUAD_TOL         5.0e-07
#define DM_OFF_NADIR_ANGLE_TOL          0.25
#define DM_CLOUD_PERCENTAGE_TOL         15.0
#define DM_SUN_AZIMUTH_ANGLE_TOL        0.05
#define DM_SUN_ELEVATION_ANGLE_TOL      0.05

// Thermal Block
#define DM_BAND_GAIN_TOL                10.0
#define DM_BAND_GAIN_CHANGE_TOL         DM_BAND_GAIN_TOL

// Transform Block
#define DM_PHI0_TOL                     0.0001
#define DM_PHI1_TOL                     0.00000001
#define DM_PHI2_TOL                     1.0e-09
#define DM_PHI3_TOL                     1.0e-16
#define DM_PHI4_TOL                     0.0001
#define DM_PHI5_TOL                     0.0001
#define DM_PHI6_TOL                     0.0001
#define DM_PHI7_TOL                     0.0001
#define DM_PHI8_TOL                     0.0001
#define DM_PHI9_TOL                     0.0001
#define DM_PHI10_TOL                    0.0001
#define DM_LAMBDA0_TOL                  0.0001
#define DM_LAMBDA1_TOL                  0.0001
#define DM_LAMBDA2_TOL                  0.00000001
#define DM_LAMBDA3_TOL                  1.0e-15
#define DM_LAMBDA4_TOL                  0.0001
#define DM_LAMBDA5_TOL                  0.0001
#define DM_LAMBDA6_TOL                  0.0001
#define DM_LAMBDA7_TOL                  0.0001
#define DM_LAMBDA8_TOL                  0.0001
#define DM_LAMBDA9_TOL                  0.0001
#define DM_LAMBDA10_TOL                 0.0001
#define DM_I0_TOL                       0.0001
#define DM_I1_TOL                       0.0001
#define DM_I2_TOL                       0.0001
#define DM_I3_TOL                       0.0001
#define DM_I4_TOL                       0.0001
#define DM_I5_TOL                       0.0001
#define DM_I6_TOL                       0.0001
#define DM_I7_TOL                       0.0001
#define DM_I8_TOL                       0.0001
#define DM_I9_TOL                       0.0001
#define DM_I10_TOL                      0.0001
#define DM_J0_TOL                       0.0001
#define DM_J1_TOL                       0.0001
#define DM_J2_TOL                       0.0001
#define DM_J3_TOL                       0.0001
#define DM_J4_TOL                       0.0001
#define DM_J5_TOL                       0.0001
#define DM_J6_TOL                       0.0001
#define DM_J7_TOL                       0.0001
#define DM_J8_TOL                       0.0001
#define DM_J9_TOL                       0.0001
#define DM_J10_TOL                      0.0001
#define DM_INCID_A0_TOL                 0.0001
#define DM_INCID_A1_TOL                 0.0001
#define DM_INCID_A2_TOL                 0.0001
#define DM_INCID_A3_TOL                 0.0001
#define DM_INCID_A4_TOL                 0.0001
#define DM_INCID_A5_TOL                 0.0001
#define DM_MAP2LS_A0_TOL                0.0001
#define DM_MAP2LS_A1_TOL                0.0001
#define DM_MAP2LS_A2_TOL                0.0001
#define DM_MAP2LS_A3_TOL                0.0001
#define DM_MAP2LS_A4_TOL                0.0001
#define DM_MAP2LS_A5_TOL                0.0001
#define DM_MAP2LS_A6_TOL                0.0001
#define DM_MAP2LS_A7_TOL                0.0001
#define DM_MAP2LS_A8_TOL                0.0001
#define DM_MAP2LS_A9_TOL                0.0001
#define DM_MAP2LS_B0_TOL                0.0001
#define DM_MAP2LS_B1_TOL                0.0001
#define DM_MAP2LS_B2_TOL                0.0001
#define DM_MAP2LS_B3_TOL                0.0001
#define DM_MAP2LS_B4_TOL                0.0001
#define DM_MAP2LS_B5_TOL                0.0001
#define DM_MAP2LS_B6_TOL                0.0001
#define DM_MAP2LS_B7_TOL                0.0001
#define DM_MAP2LS_B8_TOL                0.0001
#define DM_MAP2LS_B9_TOL                0.0001

// Projection Block
#define DM_STARTX_TOL                   10.0
#define DM_STARTY_TOL                   10.0
#define DM_PERX_TOL                     0.001
#define DM_PERY_TOL                     0.001
#define DM_HEIGHT_TOL                   1.0
#define DM_LATITUDE_TOL                 0.0005
#define DM_LONGITUDE_TOL                0.0005
#define DM_SCALE_FACTOR_TOL             0.0001
#define DM_ALPHA_ROTATION_TOL           0.0005

// Stats Block
#define DM_STATS_MIN_TOL                0.2
#define DM_STATS_MAX_TOL                0.2
#define DM_STATS_MEAN_TOL               0.2
#define DM_STATS_STD_DEVIATION_TOL      0.2

// State Vector Block
#define DM_SECONDS_TOL                  0.001
#define DM_XYZ_TOL                      0.005
#define DM_XYZ_VEL_TOL                  0.005

// AirSAR Block
#define DM_GPS_ALTITUDE_TOL             10.0
#define DM_LAT_PEG_POINT_TOL            0.0005
#define DM_LON_PEG_POINT_TOL            0.0005
#define DM_HEAD_PEG_POINT_TOL           0.0005
#define DM_ALONG_TRACK_OFFSET_TOL       10.0
#define DM_CROSS_TRACK_OFFSET_TOL       10.0

// Calibration Block
#define DM_COEFFICIENT_A1_TOL           0.001
#define DM_COEFFICIENT_A2_TOL           0.001
#define DM_COEFFICIENT_A3_TOL           0.001

#endif // _DIFFMETA_TOLERANCES_H_

