#ifndef _validate_h__
#define _validate_h__

static char sccsid_validate_h[] =  "@(#)validate.h	1.13 96/12/11 12:50:57";

#define DUAL_GPR_SUPPORT /* allow old PRODUCT_TYPE keyword values even */
                         /* though they're not in the new SIS.  whatever. */

                                                /* function prototypes */

int validateDouble(ODL msg, char *keyword, double allowed[], int numAllowed);
int validateStr(ODL msg, char *keyword, char* allowed[], int numAllowed);
int doubleInRange(double val, double dmin, double dmax);
int intInRange(int val, int min, int max);

                                                /* allowed input values */


static char *SPS_outputFormats[] = {"CEOS", "HDF"};

static char *SPS_frameModes[] = {"ARCTIC", "ANTARCTIC"};

static char *SPS_sv_coord_sys[] = {"TRUE_EQUATORIAL", "MEAN_OF_DATE"};

static char *SPS_sv_precisions[] = {"PREDICTED", "RESTITUTED"};

static char *SPS_stationIDs[] = {"FA", "MC"};

static char *SPS_recorderIDs[] = {"E1", "H1", "H2", "QL",
                                  "D1", "D2", "D3", "D4", "D5",
                                  "S0", "S1", "S2", "S3", "S4",
                                  "S5", "S6", "S7", "S8", "S9",
                                  };
static char *SPS_dataDirections[] = {"FORWARD", "REVERSE", "UNKNOWN"};


#define SPS_modeStr_ST "ST"
#define SPS_modeStr_SW "SW"
#define SPS_modeStr_SN "SN"
#define SPS_modeStr_WD "WD"
#define SPS_modeStr_FN "FN"
#define SPS_modeStr_EH "EH"
#define SPS_modeStr_EL "EL1"

#define SPS_mode_ST1 0
#define SPS_mode_ST2 SPS_mode_ST1 + 1
#define SPS_mode_ST3 SPS_mode_ST2 + 1
#define SPS_mode_ST4 SPS_mode_ST3 + 1
#define SPS_mode_ST5 SPS_mode_ST4 + 1
#define SPS_mode_ST6 SPS_mode_ST5 + 1
#define SPS_mode_ST7 SPS_mode_ST6 + 1

#define SPS_mode_SWA SPS_mode_ST7 + 1
#define SPS_mode_SWB SPS_mode_SWA + 1
#define SPS_mode_SNA SPS_mode_SWB + 1
#define SPS_mode_SNB SPS_mode_SNA + 1
static char *SPS_modes[] = {"ST1","ST2","ST3","ST4","ST5","ST6","ST7",
                            "SWA", "SWB", "SNA", "SNB", 
                            "WD1", "WD2", "WD3",
                            "FN1", "FN2", "FN3", "FN4", "FN5",
                            "EL1",
                            "EH1", "EH2", "EH3", "EH4", "EH5", "EH6", 
                            "STD"};

#define SPS_platform_R1 0
#define SPS_platform_J1 SPS_platform_R1 + 1
#define SPS_platform_E1 SPS_platform_J1 + 1
#define SPS_platform_E2 SPS_platform_E1 + 1
static char *SPS_platforms[] = {"R1", "J1", "E1", "E2" };

#define SPS_dataset_R1 "RADARSAT"
#define SPS_dataset_J1 "JERS-1"
#define SPS_dataset_E1 "ERS-1"
#define SPS_dataset_E2 "ERS-2"

#define SPS_product_STD     0 
#define SPS_product_CX      SPS_product_STD + 1
#define SPS_product_CCSD    SPS_product_CX + 1
#define SPS_product_CAL_SET SPS_product_CCSD + 1
#define SPS_product_RAMP    SPS_product_CAL_SET + 1

static char *SPS_productTypes[] = {"STANDARD", "COMPLEX", "CCSD",
                                   "CAL_SET" , "RAMP" 
#ifdef DUAL_GPR_SUPPORT
   /* the CP will ignore these values but will pass them on to the */
   /* processors until everyone gets up tp speed with the new keywords */
                                   , "COMPENSATED", "QUICKLOOK",
#endif
};


#define SPS_mediaType_DISK  0
#define SPS_mediaType_DCRSI SPS_mediaType_DISK + 1 
#define SPS_mediaType_SONY  SPS_mediaType_DCRSI + 1
static char *SPS_mediaTypes[] = {"DISK", "DCRSI", "ID-1" };

static char *SPS_mediaLocations[] = {"SHELF"};

static char *SPS_projections[] = {"GROUND_RANGE", "SLANT_RANGE", 
                  "LAMBERT", "PS", "UTM" };

static char *SPS_activityIDs[]= {"RLT", "DMP", "REC"};

static char *SPS_priorities[] = {"ROUTINE", "LOW", "HIGH", "URGENT"};

static char *SPS_yesNoChoices[] = {"YES", "NO"};

#define SPS_JOB_ID_MIN 1
#define SPS_JOB_ID_MAX 99999999

#define SPS_FRAME_ID_MIN 1
#define SPS_FRAME_ID_MAX 900

/* #define SPS_SUBFRAME_ID_MIN 1 */
#define SPS_SUBFRAME_ID_MIN -1
#define SPS_SUBFRAME_ID_MAX 8

#define SPS_UTM_ZONE_MIN 1
#define SPS_UTM_ZONE_MAX 60

#define SPS_REV_MIN 1
#define SPS_REV_MAX 99999

#define SPS_PROCESSING_GAIN_MIN -48
#define SPS_PROCESSING_GAIN_MAX  48

#define SPS_AVG_TERRAIN_HT_MIN -0.5
/* #define SPS_AVG_TERRAIN_HT_MAX  1000.0 */
#define SPS_AVG_TERRAIN_HT_MAX  10.0

#define SPS_PS_REFERENCE_LAT_MIN -90.0
#define SPS_PS_REFERENCE_LAT_MAX  90.0

#define SPS_PS_REFERENCE_LON_MIN -180.0
#define SPS_PS_REFERENCE_LON_MAX  180.0

#define SPS_LAMBERT_LAT_N_MIN -90.0
#define SPS_LAMBERT_LAT_N_MAX  90.0

#define SPS_LAMBERT_LAT_S_MIN -90.0
#define SPS_LAMBERT_LAT_S_MAX  90.0

#define SPS_POSITION_MIN -7500.0
#define SPS_POSITION_MAX  7500.0

#define SPS_VELOCITY_MIN -7800.0
#define SPS_VELOCITY_MAX  7800.0

#define SPS_PLATFORM_TIME_MIN 0
#define SPS_PLATFORM_TIME_MAX 4294967295  /* might have to make a Long const */

#define SPS_EXPERIMENTAL_BEAM_MIN 0
#define SPS_EXPERIMENTAL_BEAM_MAX 4

static double SPS_pixelSpacings[] = {6.25, 12.5 , 25.0, 50.0, 
                                     100.0, 200.0, 400.0 };

static int SPS_clockCycles[] = {0, -2147483647, 2147483647 };

#endif /* _validate_h__ */

