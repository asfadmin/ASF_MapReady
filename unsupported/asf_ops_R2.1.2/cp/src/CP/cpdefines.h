#ifndef _cpdefines_h__
#define _cpdefines_h__

static char sccsid_cpdefines_h[] = "@(#)cpdefines.h	1.1 97/11/12 11:05:13";

/*----------------------------------------------------------
 * NAME:
 *  cpdefines.h
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
#define Min(a,b) (((a) < (b)) ? (a) : (b)) 
#define Max(a,b) (((a) > (b)) ? (a) : (b)) 
#define is_CP(s) ( (strcmp(s, ASF_CP) == 0) ? 1 : 0)

/* begin type definitions */
/* end type definintions */

/* begin subsystem invocation names */
/* end subsystem names ......*/

/* begin process class and subsystem category names */
/* these strings must match the OBJECT names in the configfile.odl */
/* ----- with the exception of SSP and PP --------------- */
/* ----- for some reason the CP considers them the same -------- */
#define RDS_CATEGORY              "RDS"
#define SSP2_CATEGORY             "SSP"
#define PP_CATEGORY               "PP" 
#define ASP_CATEGORY              "ASP"
#define PPS_CATEGORY              "PPS"
#define IMS_CATEGORY              "IMS"


#define RDS_CATEGORY_ID           1 
#define SSP2_CATEGORY_ID          3
#define ASP_CATEGORY_ID           4 
#define PPS_CATEGORY_ID           5 
#define IMS_CATEGORY_ID           6 

#define SUBSYS_CLASS_ID           0 
#define CP_CLASS_ID               1 
#define RUTIL_CLASS_ID            2 
#define QC_CLASS_ID               3 
#define THREAD_CLASS_ID           4 
#define LOG_BROWSER_CLASS_ID      5
#define PPS_CLASS_ID              6
#define IMS_CLASS_ID              7

/* end process class and subsystem category names */

/* x window entities **********/
#define MAX_MAIN_LIST_ITEMS 8  /* this corresponds to items 0-7 below */

#define MAIN_JOB_ID_LIST              0  /* these first 8 MUST be 0-7 */
#define MAIN_PLAT_REV_SEQ_LIST        1
#define MAIN_FRAME_LIST               2
#define MAIN_MEDIA_LIST               3
#define MAIN_MODE_LIST                4
#define MAIN_REQ_LIST                 5
#define MAIN_TYPE_LIST                6
#define MAIN_STATUS_LIST              7
#define WP_ERROR_BOX                  10
#define WP_CLEANUP_BOX                11 
#define WP_QUESTION_BOX               12
#define WP_INFO_BOX                   13
#define WP_ERROR_QUESTION_BOX         14
#define WP_TEXT_BOX                   15
#define WP_MAX_JOBS_TEXT_ID           16
#define WP_NUM_JOBS_LABEL_ID          17

#define QUE_STATUS_LABEL_ID          19
#define QUE_HEARTBEAT_LABEL_ID       20
#define QUE_JOB_ID_LIST              21 
#define QUE_PLAT_REV_SEQ_LIST        22
#define QUE_FRAME_LIST               23
#define QUE_MEDIA_LIST               24
#define QUE_MODE_LIST                25
#define QUE_REQ_LIST                 26
#define QUE_TYPE_LIST                27
#define QUE_STATUS_LIST              28




#define XWP_SRC_STR          "tmpSrcString"

/* begin mode & scan process request types */
#define MODE_SCANSAR_ID       0
#define MODE_CONTINUOUS_ID    1
#define MODE_WIDE_ID          2
#define MODE_FINE_ID          3
#define MODE_EXT_LOW_ID       4
#define MODE_EXT_HIGH_ID      5
#define MODE_RLT_ID           6

#define REQ_TYPE_SCAN_ID          0
#define REQ_TYPE_PROCESS_ID       1
#define REQ_TYPE_DO_NEW_PPS_ID    2
#define REQ_TYPE_FRAME_ID       3
#define REQ_TYPE_DECODE_ID       4

/* end scan/process request types */

/* end x window entitites.....*/

/* processing status **********/
#define Q_RECEIVED_ID                  0
#define Q_M_PLACED_ON_SYSQ_ID	       1
#define Q_M_PROCESSING_ID	       2
#define Q_S_DONE_ID		       3
#define Q_M_NEXT_STEP_ID	       4
#define Q_M_SENT_ID	               5
#define Q_S_RCVD_ACK_ID                6
#define Q_M_RCVD_ACK_ID                7
#define Q_M_CYCLE_COMPLETED_ID         8
#define Q_S_GOT_ITEM_OFF_ID	       9
#define Q_M_IMAGE_READY_FOR_QC_ID     10
#define Q_S_IMAGE_QC_ON_HOLD_ID       11
#define Q_M_IMAGE_QC_ON_HOLD_ID       12
#define Q_S_IMAGE_QC_ACCEPT_ID        13
#define Q_S_IMAGE_QC_REJECT_ID        14
#define Q_M_IMAGE_QC_DONE_REJECT_ID   15
#define Q_M_INTERRUPTED_ID            16
#define Q_M_RESTART_HERE_ID           17
#define Q_M_ENTRY_DELETED_ID          18
#define Q_M_SCAN_COMPLETED_ID         19 
#define Q_M_HANDLING_ERROR_ID         20 
#define Q_M_FINAL_ID                  21
#define Q_M_PROCESSING_RESET_ID       22 
#define Q_M_DONE_ERROR_ID             23 
#define Q_M_INPUT_SOURCE_READY_ID     24
#define Q_M_INPUT_SOURCE_NOT_READY_ID 25
#define Q_M_REPEAT_LAST_STEP_ID       26
#define Q_M_PLACED_ON_HOLD_ERROR_ID   27
#define Q_M_PLACED_ON_HOLD_DATA_ID    28
#define Q_S_READY_TO_SEND_ID          29
#define Q_M_READY_TO_SEND_ID          30
#define Q_M_GOT_ITEM_OFF_ID	      31
#define Q_S_PLACED_ON_SYSQ_ID	      32
#define Q_M_IMAGE_QC_DONE_ACCEPT_ID   33
#define Q_M_SCAN_READY_FOR_QC_ID      34
#define Q_M_IMAGE_PRE_QC_ID     35
#define Q_S_IMAGE_QC_READY_ID       36
#define Q_M_CHECKING_TAPE_ID        37
#define Q_S_CHECKING_TAPE_ID        38
#define Q_M_INCOMING_ID             39
#define Q_M_CLEANING_UP_ID          40

/* end processing status......*/

/* processing status queue display messages */

#define MAX_LEN_Q_FMT 30  /* longest of format strings for the STATUS list  */

#define MAIN_JOB_ID_FMT        "%s"  
#define MAIN_PLAT_REV_SEQ_FMT  "%s"
#define MAIN_FRAME_FMT         "%s"
#define MAIN_MEDIA_FMT         "%s"
#define MAIN_MODE_FMT          "%s"
#define MAIN_REQ_FMT           "%s"
#define MAIN_TYPE_FMT          "%s"
#define MAIN_STATUS_FMT        "%s"

                   /* max length of a display message in the STATUS list */
#define MAX_LEN_Q_STATUS_TEXT MAX_SUBSYS_NAME_LEN + MAX_LEN_Q_FMT  

#define Q_RECEIVED_TEXT                ""
#define Q_M_PLACED_ON_SYSQ_TEXT        "WAITING FOR %s"
#define Q_M_PROCESSING_TEXT            "%s PROCESSING"
#define Q_S_DONE_TEXT                  "%s COMPLETED"
#define Q_M_INCOMING_TEXT              "INCOMING"
#define Q_M_NEXT_STEP_TEXT             "TO NEXT STATE"
#define Q_M_NEXT_STEP_COMP_TEXT        "%s COMPLETED"
#define Q_M_SENT_TEXT                  "%s SENT"
#define Q_S_RCVD_ACK_TEXT              "SUBSYS %s ACK"
#define Q_M_RCVD_ACK_TEXT              "MASTER %s ACK"
#define Q_M_CYCLE_COMPLETED_TEXT       "CYCLE COMPLETED"
#define Q_S_GOT_ITEM_OFF_TEXT          "FIRST FOR %s"
#define Q_M_IMAGE_READY_FOR_QC_TEXT   "READY FOR Q/C"
#define Q_S_IMAGE_QC_ON_HOLD_TEXT     "%s Q/C HOLD"
#define Q_M_IMAGE_QC_ON_HOLD_TEXT     Q_S_IMAGE_QC_ON_HOLD_TEXT
#define Q_S_IMAGE_QC_ACCEPT_TEXT      "Q/C ACCEPTED" 
#define Q_S_IMAGE_QC_REJECT_TEXT      "Q/C REJECTED"
#define Q_M_IMAGE_QC_DONE_REJECT_TEXT Q_S_IMAGE_QC_REJECT_TEXT
#define Q_M_INTERRUPTED_TEXT          "%s STOPPED"
#define Q_M_RESTART_HERE_TEXT         "%s RESTART HERE"
#define Q_M_ENTRY_DELETED_TEXT        "%s ENTRY DELETED"
#define Q_M_SCAN_COMPLETED_TEXT       "SCAN COMPLETED"
#define Q_M_HANDLING_ERROR_TXT        "%s HANDLING ERROR"
#define Q_M_FINAL_TEXT                ""
#define Q_M_PROCESSING_RESET_TEXT     "%s RESET"
#define Q_M_DONE_ERROR_TEXT           "%s ERROR/CLEANUP"
#define Q_M_INPUT_SOURCE_READY_TEXT   "%s: DATA READY"
#define Q_M_INPUT_SOURCE_NOT_READY_TEXT "%s: DATA NOT READY"
#define Q_M_REPEAT_LAST_STEP_TEXT     "%s TO REPEAT"
#define Q_M_PLACED_ON_HOLD_ERROR_TEXT "%s ON HOLD/ERROR"
#define Q_M_PLACED_ON_HOLD_DATA_TEXT  "%s ON HOLD/DATA"
#define Q_S_READY_TO_SEND_TEXT        "%s READY TO SEND"
#define Q_M_READY_TO_SEND_TEXT        Q_S_READY_TO_SEND_TEXT
#define Q_M_GOT_ITEM_OFF_TEXT	      Q_S_GOT_ITEM_OFF_TEXT
#define Q_S_PLACED_ON_SYSQ_TEXT	      Q_M_PLACED_ON_SYSQ_TEXT
#define Q_M_IMAGE_QC_DONE_ACCEPT_TEXT Q_S_IMAGE_QC_ACCEPT_TEXT
#define Q_M_SCAN_READY_FOR_QC_TEXT    Q_M_IMAGE_READY_FOR_QC_TEXT
#define Q_M_IMAGE_PRE_QC_TEXT         "IMAGE AVERAGING"
#define Q_S_IMAGE_QC_READY_TEXT       Q_M_IMAGE_READY_FOR_QC_TEXT
#define Q_M_PERFORMING_QC_TEXT        "PERFORMING Q/C"
#define Q_M_CHECKING_TAPE_TEXT        "%s CHECKING TAPE"
#define Q_M_CLEANING_UP_TEXT          "%s CLEANING UP"


/* end processing status queue display messages */

/* subsystem status **********/
 /* unsure if strings will be useful yet */
#define SUBSYS_DORMANT          "NOT RUNNING           "	
#define SUBSYS_WAITING          "WAITING               "
#define SUBSYS_READY            "READY                 "
#define SUBSYS_NOT_RUNNING      "NOT RUNNING           "	
#define SUBSYS_STARTED		"STARTING              "
#define SUBSYS_RUNNING		"RUNNING               "

/* end processing status queue display messages */

/* subsystem status **********/

#define SUBSYS_DORMANT_ID          0	
#define SUBSYS_NOT_RUNNING_ID      1	
#define SUBSYS_STARTED_ID          2	
#define SUBSYS_WAITING_ID          3
#define SUBSYS_READY_ID            4
#define SUBSYS_RUNNING_ID          5
#define SUBSYS_HALTING_ID          6
#define SUBSYS_HEALTHING_ID        7
#define SUBSYS_STOPPING_ID         8
#define SUBSYS_PAUSING_ID          9
#define EXIT_ALL_ID	          12
#define SUBSYS_DIDNT_START_ID     13	
#define SUBSYS_ERROR_ID           15
#define SUBSYS_WRONG_TAPE_ID      16
#define SUBSYS_UNRECOGNIZED_ID    17
#define SUBSYS_PPS_ERROR_ID       18
#define SUBSYS_HUNG_ID            19 /* when doesn't respond to HALT */

/* values for {SUBSYSTEM_COMPLETED|SUBSYSTEM_STATUS}.BODY.STATUS */

#define SUBSYS_RETURN_OK_ID 0 
#define SUBSYS_RETURN_ERROR_ID -1 
#define SUBSYS_RETURN_WRONG_TAPE_ID -2 
#define SUBSYS_RETURN_UNRECOGNIZED_ID -3
#define SUBSYS_RETURN_MALLOC_ID -4
#define SUBSYS_RETURN_OK_TEXT "COMPLETED"
#define SUBSYS_RETURN_ERROR_TEXT "CANCEL/FAIL"
#define SUBSYS_RETURN_WRONG_TAPE_TEXT "WRONG_TAPE"

/* values for {SUBSYSTEM_COMPLETED|SUBSYSTEM_STATUS}.BODY.STATUS from pps*/
#define PPS_RETURN_NO_MSGS 0
#define PPS_RETURN_ERROR_ID  -1 /*like to keep same as SUBSYS_RETURN_ERROR_ID*/

/* values for SUBSYSTEM_ACK.BODY.STATUS from pps*/
#define PPS_JOB_REQUEST_ACK_JOB_ID 0

/* these #defines are for determining which column on the color-display
   should get set to depict the particular subsystem states */
#define DISP_COL_NAME        0
#define DISP_COL_NOT_RUNNING 1
#define DISP_COL_WAITING     2
#define DISP_COL_READY       3
#define DISP_COL_RUNNING     4
#define DISP_COL_QC          5
#define DISP_COL_HOLD        6
#define DISP_COL_ERROR       7
#define DISP_COL_MAX DISP_COL_ERROR

/* QC types */
#define QC_TYPE_NONE  0
#define QC_TYPE_IMAGE 1
#define QC_TYPE_SCAN  2
#define QC_TYPE_ACCEPT  3      /* used for both image and scan qc */
#define QC_TYPE_SCAN_REJECT  4
#define QC_TYPE_DONE 5 /* for when no qc is to be done, but need to catalog */


/* IMS Catalog types */

#define IMS_DEFAULT_EXE   0
#define IMS_IMAGE_EXE     1

#define IMS_TYPE_NONE               -1  /* the order of these catalog step   */ 
#define IMS_TYPE_GET_VERSION         0  /* values are important.  for the CP */
#define IMS_TYPE_GET_SCAN_RESULT     1  /* to operate properly, we need for  */
#define IMS_TYPE_GET_CAL_PARAMS      2  /* them to be arranged in the order  */
#define IMS_TYPE_STORE_CAL_PRODUCTS  3  /* -- hope inserting doesnt break it */
#define IMS_TYPE_STORE_PRODUCTS      4  /* they are used when processing a   */
#define IMS_TYPE_STORE_SCAN_RESULT   5  /* job.  also the two STORE_SCAN_xxx */
#define IMS_TYPE_STORE_SCAN_METADATA 6  /* items must be in the order listed,*/
#define IMS_TYPE_CP_WORKING          7  /* and must be numbered higher than  */
#define IMS_TYPE_DONE                8  /* the STORE_PRODUCTS item           */

#define MAX_IMS_MSG_TEXT 25 /* max length of the six IMS_TYPE strings below */
#define IMS_TYPE_GET_VERSION_TEXT        "RETRIEVE_FILE_VERSION"
#define IMS_TYPE_GET_SCAN_RESULT_TEXT    "RETRIEVE_SCAN_RESULTS"
#define IMS_TYPE_GET_CAL_PARAMS_TEXT     "RETRIEVE_CAL_PARAMS"
#define IMS_TYPE_STORE_PRODUCTS_TEXT     "STORE_SAR_PRODUCTS"
#define IMS_TYPE_STORE_CAL_PRODUCTS_TEXT IMS_TYPE_STORE_PRODUCTS_TEXT
#define IMS_TYPE_STORE_SCAN_RESULT_TEXT  "STORE_SCAN_RESULTS"
#define IMS_TYPE_STORE_SCAN_META_TEXT    "STORE_SCAN_METADATA"
#define IMS_TYPE_CP_WORKING_TEXT         "IMS_TYPE_CP_WORKING"
#define IMS_TYPE_DONE_TEXT               "IMS_TYPE_DONE"

/* -- define the reasons for termination */
#define NORMAL_TERMINATION_ID       0
#define ABNORMAL_TERMINATION_ID     1
#define RETAIN_TERM_REASON          2  /* retain the value of term reason */
/* end subsystem status .....*/

/* begin subsystem timer identification */
#define START_INTERVAL_ID	    0
#define STOP_INTERVAL_ID	    1
#define HALT_INTERVAL_ID	    2
#define HEALTH_INTERVAL_ID          3
#define MSG_SENT_INTERVAL_ID        4
#define CLEANUP_INTERVAL_ID         5
/* end subsystem timer identification */

/* command line argument flags */

#define GPR_DATA_DIR_ARGFLG    "-asfGPRdata"
#define GPR_SAVRST_DIR_ARGFLG  "-asfGPRsavrstDir"
#define ASF_CP_INET_ARGFLG     "-asf_CP_inet"

/* end command line argument flags */

/* misc **********************/
#define HEARTBEAT_ID          0
#define ASYNC_ID              1
#define CLEANUP_ID            2
#define CATALOG_ID            3

#define ADD_ITEM              0
#define DELETE_ITEM           1
#define REPLACE_ITEM          2
#define INSERT_ITEM           3
#define DELETE_ALL            4
#define SELECT_ITEM           5
#define ADD_SCROLLED_ITEM     6

#define PPS_RETRY_TIMEOUT_VALUE   60*1000   /* 60 seconds */
#define CP_EVENT_TIMEOUT_VALUE   .25*1000 
#define FILL_MODE             0
#define SINGLE_LINE_MODE      1
#define ANYKID                -1
#define DEFAULT_PORT_STRING   "6000"

#define UNMODIFIED             0
#define MODIFIED               1

#define DO_COPY                0
#define DO_REMOVE              1 
#define DO_GET_REAL            2 

#define DO_TIMEOUT             0
#define NO_TIMEOUT             1 

#define TRUE                   1
#define FALSE                  0

#define ON                     1
#define OFF                    0

#define RESTORE                1
#define SAVE                   0

#define MAX_SUBSYS_NAME_LEN    255 
#define MAX_PLATFORM_NAME_LEN  5    /* really might be 2 or 3 */
#define MAX_CP_THREADS         12

#define NOT_SET                "NOT_SET"
#define UNDEFINED              "UNDEFINED"
#define NOT_SET_ID             -1
#define PID_NOT_SET_ID         0
#define MAX_DISPLAY_LEN        512
#define MAX_PPS_COMMENT_LENGTH 255
#define MAX_INV_NAME_LEN       100
#define DEFAULT_GPR_DATA_PATH     "./"
#define DEFAULT_GPR_CONFIG_FILE     "./configfile.odl.gpr"

/* end misc ..................*/

/* message fields *************************************************/


#define ACK_HDR_SOURCE           "SUBSYSTEM_ACK.COMMON_HEADER.SOURCE"
#define ACK_HDR_DEST             "SUBSYSTEM_ACK.COMMON_HEADER.DESTINATION"
#define ACK_BODY_JOB_ID          "SUBSYSTEM_ACK.BODY.JOB_ID"

#define STOP_HDR_SOURCE          "SUBSYSTEM_STOP.COMMON_HEADER.SOURCE"
#define STOP_HDR_DEST            "SUBSYSTEM_STOP.COMMON_HEADER.DESTINATION"

#define HALT_HDR_SOURCE          "SUBSYSTEM_HALT.COMMON_HEADER.SOURCE"
#define HALT_HDR_DEST            "SUBSYSTEM_HALT.COMMON_HEADER.DESTINATION"

#define HEARTBEAT_HDR_SOURCE     "SUBSYSTEM_HEARTBEAT.COMMON_HEADER.SOURCE"
#define HEARTBEAT_HDR_DEST       "SUBSYSTEM_HEARTBEAT.COMMON_HEADER.DESTINATION"
#define HEARTBEAT_JOB_ID         "SUBSYSTEM_HEARTBEAT.BODY.JOB_ID"



                       /* MSG_TYPE already defined in asfcommon.h */
#define HDR_MSG_TYPE "COMMON_HEADER.MSG_TYPE" 
#define HDR_TIME     "COMMON_HEADER.TIME"

#define BODY_JOB_ID              "BODY.JOB_ID"
#define BODY_MODE                "BODY.MODE" 
#define BODY_PRODUCT_TYPE        "BODY.PRODUCT_TYPE"
#define BODY_PRODUCT_ID          "BODY.PRODUCT_ID"
#define BODY_PLATFORM            "BODY.PLATFORM"
#define BODY_MEDIA_ID            "BODY.MEDIA_ID"
#define BODY_REV                 "BODY.REVOLUTION"
#define BODY_SEQ                 "BODY.SEQUENCE"
#define BODY_FRAME_ID            "BODY.FRAME_ID"
#define BODY_SUBFRAME_ID         "BODY.SUBFRAME_ID"
#define BODY_INSERT_TOP          "BODY.INSERT_TOP"
#define BODY_MEDIA_TYPE          "BODY.MEDIA_TYPE"
#define BODY_MEDIA_LOC           "BODY.MEDIA_LOCATION"
#define BODY_QUICKLOOK_FLAG      "BODY.QUICKLOOK_FLAG"
#define BODY_COMPENSATION_FLAG   "BODY.COMPENSATION_FLAG"
#define BODY_DATA_DIRECTION      "BODY.DATA_DIRECTION"


#define BODY_STATUS              "BODY.STATUS" 
#define BODY_SUB_TYPE            "BODY.SUB_TYPE" 
#define BODY_COMMENT             "BODY.COMMENT" 
#define BODY_FILE_VERSION        "BODY.FILE_VERSION" 

#define BODY_SEGMENT_COUNT       "SCAN_RESULTS_FILE.BODY.SEGMENT_COUNT" 

#define BODY_ECHO_FILE           "BODY.ECHO_DATA_FILE"
#define BODY_AUX_FILE            "BODY.AUXILIARY_DATA_FILE"
#define BODY_REPL_FILE           "BODY.REPLICA_DATA_FILE"
#define BODY_EPHEM_FILE          "BODY.EPHEMERIS_DATA_FILE"
#define BODY_CEOS_LEADER_FILE    "BODY.CEOS_LEADER_FILE"
#define BODY_IMAGE_FILE          "BODY.IMAGE_FILE"
#define BODY_PMF_FILE            "BODY.PMF_FILE"
#define BODY_BURST_FILE          "BODY.BURST_OFFSET_DATA_FILE"
#define BODY_CALPARMS_FILE       "BODY.CALPARMS_FILE_PRIMARY"
#define BODY_CALPARMS_FILE_2     "BODY.CALPARMS_FILE_SECONDARY"
#define SCAN_SCAN_RESULTS_FILE   "SPS_SCAN_REQUEST.BODY.SCAN_RESULTS_FILE"
#define FRAME_SCAN_RESULTS_FILE  "SPS_FRAME_REQUEST.BODY.SCAN_RESULTS_FILE"
#define DECODE_SCAN_RESULTS_FILE "SPS_DECODE_REQUEST.BODY.SCAN_RESULTS_FILE"
#define CATALOG_SCAN_RESULTS_FILE "CATALOG_REQUEST.BODY.SCAN_RESULTS_FILE"

         /* added for domain validation */
#define BODY_PIXEL_SPACING      "BODY.PIXEL_SPACING"
#define BODY_DESKEW             "BODY.DESKEW"
#define BODY_TERRAIN_CORRECTION "BODY.TERRAIN_CORRECTION"
#define BODY_PROJECTION         "BODY.PROJECTION"
#define BODY_OUTPUT_FORMAT      "BODY.OUTPUT_FORMAT"
#define BODY_AVG_TERRAIN_HT     "BODY.AVG_TERRAIN_HT"
#define BODY_PS_REFERENCE_LAT   "BODY.PS_REFERENCE_LAT"
#define BODY_PS_REFERENCE_LON   "BODY.PS_REFERENCE_LON"
#define BODY_LAMBERT_LAT_N      "BODY.LAMBERT_LATITUDE_N"
#define BODY_LAMBERT_LAT_S      "BODY.LAMBERT_LATITUDE_S"
#define BODY_UTM_ZONE           "BODY.UTM_ZONE"
#define BODY_PROCESSING_GAIN    "BODY.PROCESSING_GAIN"

/***************************************************************************/
/* things for reading from scan results file */

#define BODY_SVR_NUM_VEC       "BODY.STATE_VECTOR_RECORD.NUMBER_OF_VECTORS" 
#define BODY_SVR_SVM_PLATFORM  \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM" 
#define BODY_SVR_SVM_PRECISION \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION" 
#define BODY_SVR_SVM_COORDSYS  \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM" 
#define BODY_SVR_SVM_REV       \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION" 
#define BODY_SVR_SVD_X_POS \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION" 
#define BODY_SVR_SVD_Y_POS \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION" 
#define BODY_SVR_SVD_Z_POS \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION" 
#define BODY_SVR_SVD_X_VEL \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY" 
#define BODY_SVR_SVD_Y_VEL \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY" 
#define BODY_SVR_SVD_Z_VEL \
       "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY" 
#define BODY_SVR_SVD_TIME "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME" 

#define STATE_VECTOR_RECORD "STATE_VECTOR_RECORD"
#define NUMBER_OF_VECTORS "NUMBER_OF_VECTORS"
#define SVR_SVM_PLATFORM  "STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM" 
#define SVR_SVM_PRECISION \
       "STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION" 
#define SVR_SVM_COORDSYS  \
       "STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM" 
#define SVR_SVM_REV   "STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION" 
#define SVR_SVD_X_POS "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION" 
#define SVR_SVD_Y_POS "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION" 
#define SVR_SVD_Z_POS "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION" 
#define SVR_SVD_X_VEL "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY" 
#define SVR_SVD_Y_VEL "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY" 
#define SVR_SVD_Z_VEL "STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY" 
#define SVR_SVD_TIME "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME" 


#define BODY_TC_TIME "BODY.TIME_CORRELATION.TIME" 
#define BODY_TC_PLATFORM_TIME "BODY.TIME_CORRELATION.PLATFORM_TIME" 
#define BODY_TC_REV "BODY.TIME_CORRELATION.REVOLUTION" 
#define BODY_TC_CLOCK_CYCLE "BODY.TIME_CORRELATION.CLOCK_CYCLE" 

#define SRF_BODY                 "SCAN_RESULTS_FILE.BODY"

#define SRF_ACTIVITY_ID          "SCAN_RESULTS_FILE.BODY.ACTIVITY_ID"
#define SRF_RECORDER_ID          "SCAN_RESULTS_FILE.BODY.RECORDER_ID"
#define SRF_STATION_ID           "SCAN_RESULTS_FILE.BODY.STATION_ID"
#define SRF_FRAME_MODE           "SCAN_RESULTS_FILE.BODY.FRAME_MODE"
#define SRF_START_ADDRESS        "SCAN_RESULTS_FILE.BODY.START_ADDRESS"
#define SRF_END_ADDRESS          "SCAN_RESULTS_FILE.BODY.END_ADDRESS"
#define SRF_START_TIME           "SCAN_RESULTS_FILE.BODY.START_TIME"
#define SRF_END_TIME             "SCAN_RESULTS_FILE.BODY.END_TIME"

#define BODY_ACTIVITY_ID         "BODY.ACTIVITY_ID"
#define BODY_RECORDER_ID         "BODY.RECORDER_ID"
#define BODY_STATION_ID          "BODY.STATION_ID"
#define BODY_FRAME_MODE          "BODY.FRAME_MODE"
#define BODY_START_ADDRESS       "BODY.START_ADDRESS"
#define BODY_END_ADDRESS         "BODY.END_ADDRESS"
#define BODY_START_TIME          "BODY.START_TIME"
#define BODY_END_TIME            "BODY.END_TIME"

#define START_ADDRESS            "START_ADDRESS"
#define END_ADDRESS              "END_ADDRESS"
#define START_TIME               "START_TIME"
#define END_TIME                 "END_TIME"
#define CENTER_LAT               "CENTER_LAT"
#define CENTER_LON               "CENTER_LON"
#define CENTER_TIME              "CENTER_TIME"
#define NEAR_START_LAT           "NEAR_START_LAT"
#define NEAR_START_LON           "NEAR_START_LON"
#define NEAR_END_LAT             "NEAR_END_LAT"
#define NEAR_END_LON             "NEAR_END_LON"
#define FAR_START_LAT            "FAR_START_LAT"
#define FAR_START_LON            "FAR_START_LON"
#define FAR_END_LAT              "FAR_END_LAT"
#define FAR_END_LON              "FAR_END_LON"
#define ASC_DESC                 "ASC_DESC"
/* end of stuff for scan results file */
/***************************************************************************/



/* end messge fields........*/

/* message types ************/
#define SUBSYSTEM_READY		        "SUBSYSTEM_READY"	
#define SUBSYSTEM_ACK                   "SUBSYSTEM_ACK" 
#define SUBSYSTEM_STATUS                "SUBSYSTEM_STATUS" 
#define SUBSYSTEM_COMPLETED             "SUBSYSTEM_COMPLETED"
/* new (R1B) messages */
#define FRAME_REQUEST                 "FRAME_REQUEST" 
#define SPS_JOB_REQUEST               "SPS_JOB_REQUEST" 
#define SCAN_REQUEST                  "SCAN_REQUEST" 
#define SCAN_RESULTS_METADATA         "SCAN_RESULTS_METADATA" 
#define SCAN_RESULTS_FILE             "SCAN_RESULTS_FILE" 
#define SPS_DECODE_REQUEST            "SPS_DECODE_REQUEST" 
#define SPS_FRAME_REQUEST             "SPS_FRAME_REQUEST" 
#define SPS_SCAN_REQUEST              "SPS_SCAN_REQUEST" 
#define STATUS_MESSAGE                "STATUS_MESSAGE" 
#define CLEANUP_REQUEST               "CLEANUP_REQUEST" 
#define CATALOG_REQUEST               "CATALOG_REQUEST" 
#define SPS_JOB_STATUS                "SPS_JOB_STATUS" 


#define SUBSYSTEM_READY_ID		1
#define SUBSYSTEM_STATUS_ID		3
#define SUBSYSTEM_ACK_ID		4
#define SUBSYSTEM_COMPLETED_ID 		5
#define RDS_REQUEST_ACK_ID              6
#define SSP_REQUEST_ACK_ID              7
#define ASP_REQUEST_ACK_ID              8
#define PPS_REQUEST_ACK_ID              9
#define RDS_REQUEST_STATUS_ID          10
#define SSP_REQUEST_STATUS_ID          11 
#define ASP_REQUEST_STATUS_ID          12 
#define PPS_REQUEST_STATUS_ID          13 
/* new (R1B) messages */
#define FRAME_REQUEST_ID                  14
#define SPS_JOB_REQUEST_ID                15
#define SCAN_REQUEST_ID                   16
#define SCAN_RESULTS_METADATA_ID          17
#define SCAN_RESULTS_FILE_ID              18
#define SPS_DECODE_REQUEST_ID             19
#define SPS_FRAME_REQUEST_ID              20
#define SPS_SCAN_REQUEST_ID               21
#define STATUS_MESSAGE_ID                 22
#define CLEANUP_REQUEST_ID                23
#define CATALOG_REQUEST_ID                24
#define SPS_JOB_STATUS_ID                 25


/* end message types ********/

/*  messsage text *******/

#define CP_ARG_ERROR \
   "ERROR::Insufficient Number of Commandline Arguments, No %s\n"

	/* delete message ............................................*/
#define DELETE_MESSAGE_TEXT     "%d delete - for compatability !!"
	/* end delete message.........................................*/

#define CP_PLAIN_TEXT             "%s"

	/* subsys message window */
#define SYS_NOT_RUNNING_TEXT          "Status: Not Running"
#define SYS_STARTED_TEXT              "Status: Started"
#define SYS_READY_TEXT                "Status: Ready for jobs"
#define SYS_RESET_PROCESSING_TEXT     "Status: Reset during job %d"
#define SYS_CURRENT_PROCESSING_TEXT   "Status: Processing job %d"
#define SYS_CURRENT_COMPLETED_TEXT    "Status: Completed job %d"
#define SYS_CURRENT_INTERRUPTED_TEXT  "Status: Will restart processing at job %d"
	/* CP error box ............................................  */
#define SELECT_A_PROCESS_FIRST_TEXT " Please select a process to %s first"
#define PROCESS_NOT_STARTED_TEXT    "Process %s has not started, cannot %s"
#define PROCESS_INVALID_STATE_TEXT "Process %s is in invalid state (%d) for %s"
#define COULD_NOT_START_TEXT        "Could not start subsystem %s"
#define SUBSYSTEM_DID_NOT_START_TEXT  "%s did not start"
#define SUBSYSTEM_DID_NOT_STOP_TEXT   "%s did not shutdown, attempting remote kill"
#define SUBSYSTEM_DID_NOT_HALT_TEXT   "%s did not reset"
#define SUBSYSTEM_DID_NOT_HEALTH_TEXT "%s did not return health message"
#define CANT_PERFORM_REMOTE_KILL_TEXT  "Cannot perform remote kill"
#define PERFORMED_REMOTE_KILL_TEXT  "Performed remote kill on %s"
#define REMOTE_KILL_CMD_TEXT  "Attempting remote kill on %s, cmd: %s"

#define  CANNOT_GET_JOB_REQ_VALUES_TEXT "CP cannot get all values from incoming job request.\nPlease see debug log for more details."

#define CANNOT_BUILD_MSG_TEXT "CP cannot build %s message for job %d.\nPlease see system log files for more information."

#define CP_CANT_ACCESS_FILE_TEXT      "CP cannot access file %s"
#define QC_HAS_TERMINATED_BECAUSE_TEXT "Cannot perform QC because: %s"
#define QC_CANNOT_START "Cannot start QC because display not set properly.\nPlease fix configuration file and restart the CP."

/****#define SUBSYS_STATUS_ERROR "\n%s Error: %s\n" */
#define SUBSYS_STATUS_ERROR "%s Error: %s\n"
#define SUBSYS_STATUS_ERROR_CANCEL "%s %s Status Error: %s\n"

#define PPS_DONE_ERROR_CANCEL "%s job %d error: %s.\nSelect OK to allow resubmission of this job at a later time.\nSelect Cancel to terminate this job completely,\nwithout delivering a status message to PPS."

#define SUBSYS_DONE_ERROR_CANCEL "%s job %d error: %s.\nSelect OK to allow resubmission of this job at a later time.\nSelect Cancel to terminate this job completely."


	/* CP question box...........................................*/
#define WRONG_TAPE_MSG "Tape label mismatch.\nSelect OK to continue with this tape;\nSelect Cancel to proceed no further."

#define INVALID_STATUS_MSG "Received invalid status value (%s) for job %d.\nSelect OK to allow resubmission of this job at a later time.\nSelect Cancel to terminate this job completely."

#define PLEASE_MOUNT  "Please mount tape %s before proceeding with request %d.\nIf tape is not available, select cancel to defer processing of this job."
#define REALLY_WANT_TO_DELETE  "Do you really want to delete job %d?"
#define REALLY_WANT_TO_STOP_TEXT  "Do you really want to shutdown %s?"
#define REALLY_WANT_TO_HALT_TEXT  "Do you really want to reset %s?"
#define REALLY_WANT_TO_PAUSE_TEXT  "Do you really want to pause %s?"
#define REALLY_WANT_TO_EXIT_TEXT  "Do you really want to exit?"
#define MUST_HALT_SUBSYS_TEXT     "Remove all jobs?"
	/* end CP question box.......................................*/

	/* CP info box...........................................*/
#define NOT_ALL_STOPPED  \
   "Not all subsystems are stopped.\nPlease exit all subsystems before exiting"

#define SUBSYSTEM_ALREADY_RUNNING_TEXT  "Subsystem %s already running"
#define CP_SHUTTING_DOWN_TEXT "%s shutting down all, please wait"
#define JOB_IN_PROCESS_STOP \
  "Cannot shutdown %s when processing a job.\nFirst reset the subsystem to abort the job,\n and then shutdown the subsystem."
#define JOB_IN_PROCESS_REMOVE \
  "Cannot remove a job in process.\nFirst reset the subsystem and then remove this job."
#define CANNOT_REMOVE_HOLD     "Cannot remove job from hold state"

#define ALL_JOBS_COMPLETED "All jobs completed, queue empty"
#define MQ_NOT_EMPTY "Master Queue must be empty for RESTORE operation.\nPlease use the 'Queue|Cancel Job' menu item to remove jobs."

#define FEATURE_NOT_IMPLEMENTED_TEXT "Feature not implemented yet"
	/* end CP info box...........................................*/

	/* CP control subsystems box.................................*/
#define LINE_IN_CONTROL_SUBSYSTEM_TEXT   "%s"
#define STATE_IN_CONTROL_SUBSYSTEM_TEXT   "%s"
#define JOB_IN_CONTROL_SUBSYSTEM_TEXT   "%s"
	/* end CP control subsystems box.............................*/

        /* Subsystem label text .....................................*/
#define SUBSYSTEM_HEARTBEAT_LABEL_TEXT "Heartbeat: NONE"
#define SUBSYSTEM_RUNNING_ON_HOST_TEXT   "%s Currently Running On Host: %s"
#define SUBSYSTEM_REPORTS_STATUS_TEXT    "Heartbeat: %s"
        /* end Subsystem label text .....................................*/

/* end log message text....*/

#endif   /* !_cpdefines_h__ */
