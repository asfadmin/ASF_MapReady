#ifndef _cpbuildMsg_h__
#define _cpbuildMsg_h__

static char sccsid_buildmsg_h[] = "@(#)cpbuildMsg.h	1.29 97/04/14 12:24:12";


/*----------------------------------------------------------
 * NAME:
 *  cpbuildMsg.h
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/

#include "listpid.h"
#include "listmstrq.h"
#include "que_sys.h"

/* max number of pvs calibration products generated */
#define MAX_CAL_PRODUCTS 4

/* number of beams for the different products: standard, scansar */
#define SS_NUM_ST_BEAMS 1
#define SS_NUM_SW_BEAMS MAX_CAL_PRODUCTS+1
#define SS_NUM_SNA_BEAMS 3
#define SS_NUM_SNB_BEAMS 4
#define SS_NUM_EH_BEAMS 1

/* the following are placeholders containing default values
   for some user-defined parameters */
#define DEF_AVG_TERRAIN_HT 0.0
#define DEF_DESKEW "YES"
#define DEF_LAMBERT_LAT_N 65.0
#define DEF_LAMBERT_LAT_S 55.0
#define DEF_OUTPUT_FORMAT "CEOS"
#define DEF_PROCESSING_GAIN 0
#define DEF_PS_REF_LAT 70.0
#define DEF_PS_REF_LON 45.0
#define DEF_SITE_NAME ""
#define DEF_TERRAIN_CORR "NO"
/* #define DEF_UTM_ZONE  ??? */


/* abbreviations for message fields */

#define FRAME_DEST           "SPS_FRAME_REQUEST.COMMON_HEADER.DESTINATION"
#define FRAME_FILE_VERSION   "SPS_FRAME_REQUEST.BODY.FILE_VERSION"
#define FRAME_CALPARMS       "SPS_FRAME_REQUEST.BODY.CALPARMS_FILE_PRIMARY"

#define CLEANUP_HDR_SOURCE        "CLEANUP_REQUEST.COMMON_HEADER.SOURCE"
#define CLEANUP_HDR_DEST          "CLEANUP_REQUEST.COMMON_HEADER.DESTINATION"

#define CLEANUP_REQ_JOB_ID        "CLEANUP_REQUEST.BODY.JOB_ID"
#define CLEANUP_REQ_SAVE_FLAG     "CLEANUP_REQUEST.BODY.SAVE_FLAG"
#define CLEANUP_REQ_SAVE_DIR      "CLEANUP_REQUEST.BODY.SAVE_DIR"
#define CLEANUP_REQ_PLATFORM      "CLEANUP_REQUEST.BODY.PLATFORM"
#define CLEANUP_REQ_REV           "CLEANUP_REQUEST.BODY.REVOLUTION"

#define CATALOG_HDR_SOURCE        "CATALOG_REQUEST.COMMON_HEADER.SOURCE"
#define CATALOG_HDR_DEST          "CATALOG_REQUEST.COMMON_HEADER.DESTINATION"
#define CATALOG_HDR_MSG_TYPE      "CATALOG_REQUEST.COMMON_HEADER.MSG_TYPE"

#define CATALOG_REQ_JOB_ID        "CATALOG_REQUEST.BODY.JOB_ID"
#define CATALOG_REQ_MODE          "CATALOG_REQUEST.BODY.MODE"
#define CATALOG_REQ_PRODUCT_ID    "CATALOG_REQUEST.BODY.PRODUCT_ID"
#define CATALOG_REQ_PRODUCT_TYPE  "CATALOG_REQUEST.BODY.PRODUCT_TYPE"
#define CATALOG_REQ_CALPARMS      "CATALOG_REQUEST.BODY.CALPARMS_FILE_PRIMARY"
#define CATALOG_REQ_SCAN_RESULTS  "CATALOG_REQUEST.BODY.SCAN_RESULTS_FILE"
#define CATALOG_REQ_QC_STATUS     "CATALOG_REQUEST.BODY.QC_STATUS"
#define CATALOG_REQ_IMAGE_FILE    "CATALOG_REQUEST.BODY.IMAGE_FILE"
#define CATALOG_REQ_CEOS_FILE     "CATALOG_REQUEST.BODY.CEOS_LEADER_FILE"
#define CATALOG_REQ_PMF_FILE      "CATALOG_REQUEST.BODY.PMF_FILE"
#define CATALOG_REQ_NAME          "CATALOG_REQUEST.BODY.PRODUCT_BASE_NAME"
#define CATALOG_REQ_PLATFORM      "CATALOG_REQUEST.BODY.PLATFORM"
#define CATALOG_REQ_REV           "CATALOG_REQUEST.BODY.REVOLUTION"
#define CATALOG_REQ_FILE_VER      "CATALOG_REQUEST.BODY.FILE_VERSION"
#define CATALOG_REQ_FRAME_MODE      "CATALOG_REQUEST.BODY.FRAME_MODE"
#define CATALOG_REQ_COMPENSATION_FLAG  "CATALOG_REQUEST.BODY.COMPENSATION_FLAG"


#define JOB_REQ_HDR_SOURCE        "SPS_JOB_REQUEST.COMMON_HEADER.SOURCE"
#define JOB_REQ_HDR_DEST          "SPS_JOB_REQUEST.COMMON_HEADER.DESTINATION"
#define JOB_REQ_MODE              "SPS_JOB_REQUEST.BODY.PROCESSOR_MODE"
#define JOB_REQ_TYPE              "SPS_JOB_REQUEST.BODY.REQUEST_TYPE"

#define JOB_STATUS_HDR_SOURCE        "SPS_JOB_STATUS.COMMON_HEADER.SOURCE"
#define JOB_STATUS_HDR_DEST          "SPS_JOB_STATUS.COMMON_HEADER.DESTINATION"

#define JOB_STATUS_REQ_TYPE      "SPS_JOB_STATUS.BODY.REQUEST_TYPE"
#define JOB_STATUS_DATASET       "SPS_JOB_STATUS.BODY.DATASET"
#define JOB_STATUS_PRODUCT_ID    "SPS_JOB_STATUS.BODY.PRODUCT_ID"
#define JOB_STATUS_STATUS_TYPE   "SPS_JOB_STATUS.BODY.STATUS_TYPE"
#define JOB_STATUS_STATUS        "SPS_JOB_STATUS.BODY.STATUS"
#define JOB_STATUS_COMMENT       "SPS_JOB_STATUS.BODY.COMMENT"
#define JOB_STATUS_JOB_ID        "SPS_JOB_STATUS.BODY.JOB_ID"

#define JOB_STATUS_SCAN_START    "SPS_JOB_STATUS.BODY.SCAN_JOB_START"
#define JOB_STATUS_SCAN_END      "SPS_JOB_STATUS.BODY.SCAN_JOB_END"
#define JOB_STATUS_FRAME_START   "SPS_JOB_STATUS.BODY.FRAME_JOB_START"
#define JOB_STATUS_FRAME_END     "SPS_JOB_STATUS.BODY.FRAME_JOB_END"
#define JOB_STATUS_DECODE_START  "SPS_JOB_STATUS.BODY.DECODE_JOB_START"
#define JOB_STATUS_DECODE_END    "SPS_JOB_STATUS.BODY.DECODE_JOB_END"
#define JOB_STATUS_QC_START      "SPS_JOB_STATUS.BODY.QC_JOB_START"
#define JOB_STATUS_QC_END        "SPS_JOB_STATUS.BODY.QC_JOB_END"
#define JOB_STATUS_CATALOG_START "SPS_JOB_STATUS.BODY.CATALOG_JOB_START"
#define JOB_STATUS_CATALOG_END   "SPS_JOB_STATUS.BODY.CATALOG_JOB_END"

typedef struct {
   int segment;
   int found;  /* set to 1 if frame exists in scan results file; 0 otherwise */
   int startAddr;
   int endAddr;
   struct timeval startTime;
   struct timeval endTime;
   double centerLat;
   double centerLon;
   struct timeval centerTime;
   double nearStartLat;
   double nearStartLon;
   double nearEndLat;
   double nearEndLon;
   double farStartLat;
   double farStartLon;
   double farEndLat;
   double farEndLon;
   char *asc_desc;
   char *sv_precision;
} frameInfo;

typedef struct{
               int             jobId;
               char            *priorityStr;
               int             insertTop;
/*               int             fileVersion; */
               char            *mediaId;
               char            *mediaType;
               char            *scanResultsFile;
               char            *pmfFile;
               char            *dataDirection;
               char            *experimentalBeam;
               char            *compensated;
               char            *quicklook;

/* added these for frame_request */
               char            *calParamsFile;
               char            *calParamsFile_2; /* secondary cal params */
               char            *imageFile;
               int             subframe_id;
               double          lambert_lat_n;
               double          lambert_lat_s;

               char            *site_nameBuf;
               char            *stationID;
               char            *sat;
               char            *sv;
               char            *mode_buf;
               int             priority;
               char            *productT_buf;
               double          pixelSpacing;
               int             frameId;
               double          standardLat;
               double          standardLon;
               char            *frame_mode_buf;
               char            *output_format_buf;
               int             processingGain;
               double          avgTerrainHt;
               char            *projection_buf; 
               char            *skew_buf;
               char            *dtm_satBuf;
               char            *sensorBuf;
               int             rev;
               int             seq;
               char            *activityBuf;
               int             startAddr;
               int             endAddr;
               struct timeval  startTime;
               struct timeval  endTime;
               char            *recorderBuf;
               struct timeval  gha_time_val;
               double          gha_angle;
               int             sv_num_vectors;
               char            *sv_satBuf;
               int             sv_rev;
               char            *sv_typeBuf;
               char            *sv_coordSysBuf;
               struct timeval  sv_time_val;
               int             time_corr_rev;
               struct timeval  time_corr_time_val;
               unsigned int    sat_time;
               double          sv_x_pos;
               double          sv_y_pos;
               double          sv_z_pos;
               double          sv_x_vel;
               double          sv_y_vel;
               double          sv_z_vel;
               char            *terrain_correction_buf;
               int             UTM_zone;

               char            *rt_buf;
               char            *tapeBuf;
               char            *clock_cycle;

} menuStateType;



ODL buildSubsys_Msg(char *namePtr, sysQueElemType *sqel, subsysFilesType *sf);
ODL buildCleanup_Msg(char *namePtr, int jobid, char *platform, int rev, 
                     int discard);

char *GetIMSmsgTypeAsText(int status);

long getExpectedTime(char *productType, char *compensated);
void decrementSSP2Time(int whichSSP2, long productSize);

#endif
