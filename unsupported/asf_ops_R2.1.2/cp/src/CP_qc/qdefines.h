/*  #define'd variables for the QC app... */

#ifndef _QDEFINES_H
#define _QDEFINES_H

static char sccsid_qdefines_h[] = "@(#)qdefines.h	1.22 96/12/30 15:29:13";

/* User definable stuff */

#include "qexit_stati.h"

/* Logging looks */
#define  QC_SYSLOG_VARIABLE    syslog_tag

#define  QC_DEFAULT_SCALE      0.2

/* For the projection value */
#define  QC_GROUND_RANGE_STRING "GROUND_RANGE"
#define  QC_SLANT_RANGE_STRING  "SLANT_RANGE"
#define  QC_PS_STRING           "PS"
#define  QC_UTM_STRING          "UTM"
#define  QC_LAMBERT_STRING      "LAMBERT"

/* Other stuff... */
#define SAMPLE_IN_8_BITS          8

#define FULL_REZ_PIXEL_SPACING    12.5

#define SCREEN_SIZE_X             1280
#define SCREEN_SIZE_Y             1024

#define ZOOM_IN_WIDTH             1024
#define DEFAULT_SAR_DATA_LENGTH   1024

#define SCALED_IMAGE_MAX_HEIGHT   1024

#define ZOOM_BOX_FOREGROUND    255 
#define ZOOM_BOX_BACKGROUND    0 

/*  Offsets for the image file */

#define FIRST_RECORD_LENGTH             450
#define HISTOGRAM_TABLE_SET_OFFSET      36


#define SAR_DATA_RECS_COUNT_LENGTH      6
#define SAR_DATA_RECS_COUNT_OFFSET      180

#define SAR_DATA_RECS_LENGTH_LENGTH     6
#define SAR_DATA_RECS_LENGTH_OFFSET     186

#define BITS_PER_SAMPLE_LENGTH          4
#define BITS_PER_SAMPLE_OFFSET          216

#define LINES_PER_CHANNEL_LENGTH        8
#define LINES_PER_CHANNEL_OFFSET        236

#define LEFT_BORDER_PIXELS_LENGTH       4
#define LEFT_BORDER_PIXELS_OFFSET       244

#define N_PREFIX_LENGTH                 4
#define N_PREFIX_OFFSET                 276

#define PIXELS_PER_LINE_LENGTH          8
#define PIXELS_PER_LINE_OFFSET          248

#define RIGHT_BORDER_PIXELS_LENGTH      4
#define RIGHT_BORDER_PIXELS_OFFSET      256

#define N_SUFFIX_LENGTH                 4
#define N_SUFFIX_OFFSET                 288

#define TOP_BORDER_LINES_LENGTH         4
#define TOP_BORDER_LINES_OFFSET         260

#define BOTTOM_BORDER_LINES_LENGTH      4
#define BOTTOM_BORDER_LINES_OFFSET      264

#define PREFIX_DATA_LENGTH              4
#define PREFIX_DATA_OFFSET              276

#define SAR_DATA_BYTE_COUNT_LENGTH      8
#define SAR_DATA_BYTE_COUNT_OFFSET      280

#define SUFFIX_DATA_LENGTH              4
#define SUFFIX_DATA_OFFSET              288

#define FORMAT_TYPE_INDICATOR_LENGTH    28
#define FORMAT_TYPE_INDICATOR_OFFSET    400

/*  Offsets for the leader file */

#define LEADER_FILE_IDEN_OFFSET          4
#define LEADER_FILE_IDEN_VALUE           10

#define DATA_SET_SUMMARY_RECORD_OFFSET   720
#define DATA_SET_SUMMARY_RECORD_SIZE     4096

#define SYS_ID_OFFSET                    1062
#define SYS_ID_LENGTH                    8

#define LINE_SPACING_OFFSET              1686
#define LINE_SPACING_LENGTH              16

#define PIXEL_SPACING_OFFSET             1702
#define PIXEL_SPACING_LENGTH             16

#define LATITUDE_OFFSET                  116
#define LATITUDE_LENGTH                  16

#define LONGITUDE_OFFSET                 132
#define LONGITUDE_LENGTH                 16

#define TIME_OFFSET                      68
#define TIME_LENGTH                      21

#define SCENE_ID_OFFSET                  20
#define SCENE_ID_LENGTH                  16

#define SATELLITE_OFFSET                 396
#define SATELLITE_LENGTH                 16

#define REVOLUTION_OFFSET                444
#define REVOLUTION_LENGTH                8

#define LEADER_FILE_DESCRIPTOR_RECORD_OFFSET               0
#define LEADER_FILE_DESCRIPTOR_RECORD_SIZE                 720

#define FACILITY_DATA_RECORD_SIZE                          1615

#define GND_SLANT_FLAG_OFFSET                              1078
#define GND_SLANT_FLAG_LENGTH                              7

#define MAP_PROJECTION_DATA_RECORD_OFFSET                  4816
#define MAP_PROJECTION_DATA_RECORD_SIZE                    943

#define MAP_DESC_OFFSET                                    412
#define MAP_DESC_LENGTH                                    32

#define NUMBER_OF_DATA_SET_SUMMARY_RECORDS_LENGTH          6
#define NUMBER_OF_DATA_SET_SUMMARY_RECORDS_OFFSET          180

#define DATA_SET_SUMMARY_RECORDS_SIZE_LENGTH               6
#define DATA_SET_SUMMARY_RECORDS_SIZE_OFFSET               186


#define NUMBER_OF_MAP_PROJECTION_RECORDS_LENGTH            6
#define NUMBER_OF_MAP_PROJECTION_RECORDS_OFFSET            192

#define MAP_PROJECTION_RECORDS_SIZE_LENGTH                 6
#define MAP_PROJECTION_RECORDS_SIZE_OFFSET                 198

#define NUMBER_OF_PLATFORM_POSITION_DATA_RECORDS_LENGTH    6
#define NUMBER_OF_PLATFORM_POSITION_DATA_RECORDS_OFFSET    204

#define PLATFORM_POSITION_RECORDS_SIZE_LENGTH              6
#define PLATFORM_POSITION_RECORDS_SIZE_OFFSET              210

#define NUMBER_OF_ATTITUDE_DATA_RECORDS_LENGTH             6
#define NUMBER_OF_ATTITUDE_DATA_RECORDS_OFFSET             216

#define ATTITUDE_RECORDS_SIZE_LENGTH                       6
#define ATTITUDE_RECORDS_SIZE_OFFSET                       222

#define NUMBER_OF_RADIOMETRIC_DATA_RECORDS_LENGTH          6
#define NUMBER_OF_RADIOMETRIC_DATA_RECORDS_OFFSET          228

#define RADIOMETRIC_RECORDS_SIZE_LENGTH                    6
#define RADIOMETRIC_RECORDS_SIZE_OFFSET                    234

#define NUMBER_OF_RADIOMETRIC_COMPENSATION_DATA_RECORDS_LENGTH  6
#define NUMBER_OF_RADIOMETRIC_COMPENSATION_DATA_RECORDS_OFFSET  240

#define RADIOMETRIC_COMPENSATION_RECORDS_SIZE_LENGTH       6
#define RADIOMETRIC_COMPENSATION_RECORDS_SIZE_OFFSET       246

#define NUMBER_OF_DATA_QUALITY_SUMMARY_RECORDS_LENGTH      6
#define NUMBER_OF_DATA_QUALITY_SUMMARY_RECORDS_OFFSET      252

#define DATA_QUALITY_SUMMARY_RECORDS_SIZE_LENGTH           6
#define DATA_QUALITY_SUMMARY_RECORDS_SIZE_OFFSET           258

#define NUMBER_OF_DATA_HISTOGRAM_RECORDS_LENGTH            6
#define NUMBER_OF_DATA_HISTOGRAM_RECORDS_OFFSET            264

#define DATA_HISTOGRAM_RECORDS_SIZE_LENGTH                 6
#define DATA_HISTOGRAM_RECORDS_SIZE_OFFSET                 270

#define NUMBER_OF_RANGE_SPECTRA_RECORDS_LENGTH             6
#define NUMBER_OF_RANGE_SPECTRA_RECORDS_OFFSET             276

#define RANGE_SPECTRA_RECORDS_SIZE_LENGTH                  6
#define RANGE_SPECTRA_RECORDS_SIZE_OFFSET                  282

#define NUMBER_OF_DEM_DESCRIPTOR_RECORDS_LENGTH            6
#define NUMBER_OF_DEM_DESCRIPTOR_RECORDS_OFFSET            288

#define DEM_DESCRIPTOR_RECORDS_SIZE_LENGTH                 6
#define DEM_DESCRIPTOR_RECORDS_SIZE_OFFSET                 294

#define NUMBER_OF_RADAR_PARAMETER_RECORDS_LENGTH           6
#define NUMBER_OF_RADAR_PARAMETER_RECORDS_OFFSET           300

#define RADAR_PARAMETER_RECORDS_SIZE_LENGTH                6
#define RADAR_PARAMETER_RECORDS_SIZE_OFFSET                306

#define NUMBER_OF_ANNOTATION_DATA_RECORDS_LENGTH           6
#define NUMBER_OF_ANNOTATION_DATA_RECORDS_OFFSET           312

#define ANNOTATION_DATA_RECORDS_SIZE_LENGTH                6
#define ANNOTATION_DATA_RECORDS_SIZE_OFFSET                318

#define NUMBER_OF_DETAILED_PROCESSING_RECORDS_LENGTH       6
#define NUMBER_OF_DETAILED_PROCESSING_RECORDS_OFFSET       324

#define DETAILED_PROCESSING_RECORDS_SIZE_LENGTH            6
#define DETAILED_PROCESSING_RECORDS_SIZE_OFFSET            330

#define NUMBER_OF_CALIBRATION_RECORDS_LENGTH               6
#define NUMBER_OF_CALIBRATION_RECORDS_OFFSET               336

#define CALIBRATION_RECORDS_SIZE_LENGTH                    6
#define CALIBRATION_RECORDS_SIZE_OFFSET                    342

#define NUMBER_OF_GCP_RECORDS_LENGTH                       6
#define NUMBER_OF_GCP_RECORDS_OFFSET                       348

#define GCP_RECORDS_SIZE_LENGTH                            6
#define GCP_RECORDS_SIZE_OFFSET                            354

#define NUMBER_OF_FACILITY_DATA_RECORDS_LENGTH             6
#define NUMBER_OF_FACILITY_DATA_RECORDS_OFFSET             420

/* For the histogram record... */

#define TOTAL_NUMBER_IN_HISTOGRAM_LENGTH                   8
#define TOTAL_NUMBER_IN_HISTOGRAM_OFFSET                   76

#define HIST_DESC_LENGTH                                   32
#define HIST_DESC_OFFSET                                   36

#define I_HIST_DESC_LENGTH                                 32
#define I_HIST_DESC_OFFSET                                 36

#define SAMPLES_PER_GROUP_LENGTH                           8
#define SAMPLES_PER_GROUP_OFFSET                           124

#define MINIMUM_SAMPLE_VALUE_LENGTH                        16
#define MINIMUM_SAMPLE_VALUE_OFFSET                        132

#define MAXIMUM_SAMPLE_VALUE_LENGTH                        16
#define MAXIMUM_SAMPLE_VALUE_OFFSET                        148

#define MEAN_SAMPLE_VALUE_LENGTH                           16
#define MEAN_SAMPLE_VALUE_OFFSET                           164

#define SDEV_SAMPLE_VALUE_LENGTH                           16
#define SDEV_SAMPLE_VALUE_OFFSET                           180

#define MINIMUM_HISTOGRAM_VALUE_LENGTH                     16
#define MINIMUM_HISTOGRAM_VALUE_OFFSET                     212

#define MAXIMUM_HISTOGRAM_VALUE_LENGTH                     16
#define MAXIMUM_HISTOGRAM_VALUE_OFFSET                     228

#define HISTOGRAM_SIZE_LENGTH                              8
#define HISTOGRAM_SIZE_OFFSET                              276

#define FIRST_HISTOGRAM_VALUE_LENGTH                       8
#define FIRST_HISTOGRAM_VALUE_OFFSET                       284

/* for the data quality summary record */

#define SNR_LENGTH                                         16
#define SNR_OFFSET                                         94

#define BER_LENGTH                                         16
#define BER_OFFSET                                         110


/* Non-User definable stuff */

/* for change_stretching() */

#define LOW         1
#define HIGH        2
#define INITIALIZE  3
#define LOW_INPUT   4
#define HIGH_INPUT  5




/* for change_all_sensitivity() */

#define DESENSITIZE_ALL         1
#define RESENSITIZE_AS_NEEDED   2


/* for read_n_bits() */

#define CAN_BE_ZERO     1
#define CANNOT_BE_ZERO  2
#define VERBOSE         7
#define NOT_VERBOSE     8


/* for differentiating between full / low rez / complex images */

#define FULL_REZ        1
#define LOW_REZ         2
#define COMPLEX         3

/* for computing the offsets in a leader file... */

#define COMPUTE_HISTOGRAM_OFFSET              1
#define COMPUTE_DATA_QUALITY_SUMMARY_OFFSET   2
#define COMPUTE_MAP_PROJECTION_OFFSET         3
#define COMPUTE_FACILITY_DATA_RECORD_OFFSET   4

#define LOG     666
#define NO_LOG  6666
#define INFO    66666

#define DO_EXIT   77
#define NO_EXIT   777

#define DEFAULT_CMAP     11
#define NEW_CMAP         22


#define MaxGrey         255	/* limits on the grey levels used */
#define Threshold       128	/* in the dithering process */
#define MinGrey           0

#endif /* _QDEFINES_H */
