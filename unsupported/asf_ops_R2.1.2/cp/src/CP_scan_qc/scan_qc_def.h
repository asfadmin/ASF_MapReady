/*----------------------------------------------------------
 * NAME:
 *  scan_qc_def.h
 *
 * DESCRIPTION:
 *
 * NOTES:
 *
 *---------------------------------------------------------*/
#ifndef _SCAN_QC_DEF_H
#define _SCAN_QC_DEF_H

static char sccsid_reqdefines_h[] = "@(#)scan_qc_def.h	1.24 97/06/05 11:34:27";

#include "qexit_stati.h"

static  int  rv;
static  int  mode;
static  int  frameN;
static  int  segmentN;
static  int  segment_ID;

typedef enum {radarsat, ers_1, ers_2, jers_1, seasat} satelliteType;

typedef enum {asf, csa, wallops} stationType;

typedef enum {predicted, restituted, undefined} stateVectorType;

typedef enum {continuous, scansar} modeType;

typedef enum {scan, frame, scanAndProcess} requestType;

typedef enum {st1, st2, st3, st4, st5, st6, st7,
              ss1, ss2, ss3, ss4, 
              eh1, eh2, eh3, eh4, eh5, eh6,
              wd1, wd2, wd3,
              fn1, fn2, fn3, fn4, fn5,
              el1, std_e} instrumentModeType;

typedef enum {ss_00, ss_50, ss_100, ss_400,
              s_00,  s_12_5, s_100} pixelSpacingType;

typedef enum {arctic, antarctic} frameModeType;

typedef enum {scanQcScreenHelpOverview, 
              scanQcScreenHelpUsingHelp, 
              scanQcScreenHelpProductInfo} helpMenuType;
 
/* exit status */
#define  SQC_HOLD               50
#define  SQC_REJECT             51
#define  SQC_ACCEPT             52
#define  SQC_HELP               53

#define  SQC_SCAN_ERROR         SCAN_QC_ERROR  /* CP_scan_qc error                          */
#define  SQC_SRF_ERROR          SCAN_QC_ERROR  /* scan results file not exist               */
#define  SQC_PARSE_SRF_ERROR    SCAN_QC_ERROR  /* ODL parse scan results file error         */
#define  SQC_BODY_SRF_ERROR     SCAN_QC_ERROR  /* no BODY in scan results file              */
#define  SQC_CFG_ERROR          SCAN_QC_ERROR  /* config file not exist                     */
#define  SQC_CFG_RD_ERROR       SCAN_QC_ERROR  /* config file read error                    */
#define  SQC_NO_CFG_ERROR       SCAN_QC_ERROR  /* config file not specified                 */
#define  SQC_PARSE_CFG_ERROR    SCAN_QC_ERROR  /* ODL parse config file error               */
#define  SQC_SEG_CNT_LESS_ERROR SCAN_QC_ERROR  /* SEGMENT_COUNT < # of SEGMENT              */
#define  SQC_SEG_CNT_LARG_ERROR SCAN_QC_ERROR  /* SEGMENT_COUNT > # of SEGMENT              */
#define  SQC_SEG_CNT_SRF_ERROR  SCAN_QC_ERROR  /* no SEGMENT_COUNT in scan results file     */
#define  SQC_BAD_SEG_CNT_ERROR  SCAN_QC_ERROR  /* incorrect SEGMENT_COUNT value             */
#define  SQC_FRM_ID_SRF_ERROR   SCAN_QC_ERROR  /* no FRAME_ID in scan results file          */
#define  SQC_SCLAT_SRF_ERROR    SCAN_QC_ERROR  /* no SCENE_CENTER_LAT in scan results file  */
#define  SQC_SCLON_SRF_ERROR    SCAN_QC_ERROR  /* no SCENE_CENTER_LON in scan results file  */
#define  SQC_SCTM_SRF_ERROR     SCAN_QC_ERROR  /* no SCENE_CENTER_TIME in scan results file */
#define  SQC_MALLOC_FRM_ERROR   SCAN_QC_ERROR  /* error on malloc for frame list            */
#define  SQC_PLATFM_SRF_ERROR   SCAN_QC_ERROR  /* no PLATFORM in scan results file          */
#define  SQC_STATION_SRF_ERROR  SCAN_QC_ERROR  /* no STATION_ID in scan results file        */
#define  SQC_SEGCNT_SRF_ERROR   SCAN_QC_ERROR  /* no SEGMENT_COUNT in scan results file     */
#define  SQC_FRM_MD_SRF_ERROR   SCAN_QC_ERROR  /* no FRAME_MODE in scan results file        */
#define  SQC_REV_SRF_ERROR      SCAN_QC_ERROR  /* no REVOLUTION in scan results file        */
#define  SQC_JOBID_SRF_ERROR    SCAN_QC_ERROR  /* no JOB ID in scan results file        */
#define  SQC_SEQ_SRF_ERROR      SCAN_QC_ERROR  /* no SEQUENCE in scan results file          */
#define  SQC_SRT_TM_SRF_ERROR   SCAN_QC_ERROR  /* no START_TIME in scan results file        */
#define  SQC_END_TM_SRF_ERROR   SCAN_QC_ERROR  /* no END_TIME in scan results file          */
#define  SQC_SEG_ID_SRF_ERROR   SCAN_QC_ERROR  /* no SEGMENT_ID in ith SEGMENT              */
#define  SQC_FRM_CT_SRF_ERROR   SCAN_QC_ERROR  /* no FRAME_COUNT in ith SEGMENT             */
#define  SQC_SEG_SRT_TM_ERROR   SCAN_QC_ERROR  /* no SEGMENT_START_TIME in ith SEGMENT      */
#define  SQC_SEG_STP_TM_ERROR   SCAN_QC_ERROR  /* no SEGMENT_STOP_TIME in ith SEGMENT       */
#define  SQC_LARGE_TM_GAP_ERROR SCAN_QC_ERROR  /* time gap too large in ith SEGMENT         */
#define  SQC_MALLOC_FRAME_ERROR SCAN_QC_ERROR  /* malloc for frame err in ith SEGMENT       */
#define  SQC_WRG_SEG_TM_ERROR   SCAN_QC_ERROR  /* stop < start SEGMENT time in ith SEGMENT  */
#define  SQC_WRG_FRM_TM_ERROR   SCAN_QC_ERROR  /* stop < start FRAME time in ith FRAME      */
#define  SQC_IVL_SRT_TM_ERROR   SCAN_QC_ERROR  /* invalid SEGMENT_START_TIME in ith SEGMENT */
#define  SQC_IVL_STP_TM_ERROR   SCAN_QC_ERROR  /* invalid SEGMENT_STOP_TIME in ith SEGMENT  */
#define  SQC_IVL_SCN_SRT_ERROR  SCAN_QC_ERROR  /* invalid SCAN_START_TIME*/
#define  SQC_IVL_SCN_STP_ERROR  SCAN_QC_ERROR  /* invalid SCAN_STOP_TIME*/
#define  SQC_IVL_CNT_TM_ERROR   SCAN_QC_ERROR  /* invalid SCENE_CENTER_TIME*/

/* define Datatake message structure for select acquisition */

#define platformID_SZ          2
#define revolutionN_SZ         5
#define sequenceN_SZ	       2
#define tapeID_SZ              2
#define SCN_FILE_RECORD_SZ     20
#define N_REQ_FILE_ERR         10
#define mode_SZ                3
#define frameMode_SZ           1
#define mediaType_SZ           1
#define versionN_SZ            3

#define revolutionList         1
#define sequenceList           2
#define mediaList              3
#define versionList            4

#define segmentList            1
#define frameList              2
#define latitudeList           3
#define longitudeList          4
#define timeList               5

#define mediaID_SZ             7

typedef struct media_struct {
   char mediaID[mediaID_SZ];
} media_record;

#define FRM_FILE_RECORD_SZ     53
#define segmentID_SZ           5
#define frameID_SZ             5
#define centerLat_SZ           13
#define centerLon_SZ           13
#define centerTime_SZ          22
#define duration_SZ            6
 
typedef struct segment_struct {
   char segmentID[segmentID_SZ];
   char frameID[frameID_SZ];
   char duration[duration_SZ];
   char startTime[centerTime_SZ];
   char stopTime[centerTime_SZ];
} segment_file_record;

typedef struct frame_struct {
   char segmentID[segmentID_SZ];
   char frameID[frameID_SZ];
   char centerLat[centerLat_SZ];
   char centerLon[centerLon_SZ];
   char centerTime[centerTime_SZ];
} frame_file_record;


/* begin radar defines */
#define RADARSAT               "RADARSAT"
#define ERS_1                  "ERS-1"
#define ERS_2                  "ERS-2"
#define JERS_1                 "JERS-1"
#define SEASAT                 "SEASAT"
#define HOST                   "HOST"
#define SAVE_DIR               "SAVE_DIR"
#define RESTORE_DIR            "RESTORE_DIR"
/* station id's */
#define RS                     "RS"
#define R1                     "R1"
#define E1                     "E1"
#define E2                     "E2"
#define J1                     "J1"
#define SE                     "SE"
#define DISK                   "DISK"
#define BLANK                  ""
/* end radar defines */

/* begin misc defines */
#define REV_LEN                5
#define SEQ_LEN                2
#define LOG                    666
#define SCAN_REQUEST           "SCAN_REQUEST"
#define FRAME_REQUEST          "FRAME_REQUEST"
#define PREDICTED              "PREDICTED"
#define RESTITUTED             "RESTITUTED"
#define SEGMENT                "SEGMENT"
#define FRAME_COUNT            "FRAME_COUNT"
#define SEGMENT_ID             "SEGMENT_ID"
#define SEGMENT_START_TIME     "START_TIME"
#define SEGMENT_STOP_TIME      "END_TIME"
#define FRAME                  "FRAME"
#define FRAME_ID               "FRAME_ID"
#define SCENE_CENTER_LAT       "CENTER_LAT"
#define SCENE_CENTER_LON       "CENTER_LON"
#define SCENE_CENTER_TIME      "CENTER_TIME"
#define PLATFORM               "PLATFORM"
#define STATION                "STATION"
#define STATION_ID             "STATION_ID"
#define DATATAKE_DIR           "DATATAKE_DIR"
#define PRINT_COMMAND	       "CONFIG_FILE.PRINT_COMMAND"
#define DEF_PRINT_CMD          "$ASF/bin/xgrabsc -id %d | lpr -h"
/* end  misc defines */

/*---------------------------------------------------------------------------
 *
 *  Help Information
 *
 *--------------------------------------------------------------------------*/
#define SCAN_QC_ON_OVERVIEW                 \
"Menus\n\
\n\
    File\n\
        Print:  Print a hard copy of this window\n\
        Exit:  Exit this application\n\
\n\
    Help\n\
        Overview:  Display this window\n\
        Using Help:  Display instructions for using the help menu\n\
        Product Information:  Display the current version number\n\
\n\
Annotation\n\
\n\
    Platform:  Platform that acquired the data\n\
    Revolution:  Revolution (orbit) number\n\
    Sequence:  Sequence number of the datatake within the orbit\n\
    Mode:  Beam or ScanSAR mode used for the acquisition\n\
    Frame size:  Nominal frame size, in km\n\
    Station:  Station that acquired the data\n\
    Number of Segments:  Number of segments found during the scan\n\
    Total Datatake Length:  Nominal (scheduled) datatake length, in seconds\n\
    Nominal Frame Length:  Length of a frame, in seconds\n\
    Nominal Segment Length:  Minimum required length for each segment, in seconds\n\
\n\
Segment Table\n\
\n\
    This table lists each segment of the expected mode found during the\n\
    scan.  Segments are numbered starting at 1.  For each segment, the\n\
    table shows its length in seconds, the number of frames found, and the\n\
    start and end times of the segment.  Use the vertical scrollbar at the\n\
    right to scroll through the list.\n\
\n\
Frame Table\n\
\n\
    This table lists each frame of the expected mode found during the\n\
    scan, ordered by segments.  Frames are numbered according to their\n\
    relative locations within the orbit.  For each frame, the table shows\n\
    the frame number, its center latitude and longitude in degrees, and\n\
    its center time in GMT.  Use the vertical scrollbar at the right to\n\
    scroll through the list.\n\
\n\
Actions\n\
\n\
    Selecting any of these buttons exits the application.\n\
\n\
    Accept:  Accept the scan results\n\
    Reject:  Reject the scan results\n\
    Hold:  Hold the scan results for later examination"

#define SCAN_QC_ON_USE_HELP                 \
"Refer to the Overview window for more information about this application."

/*---------------------------------------------------------------------------
 *
 * messages definition
 *
 *--------------------------------------------------------------------------*/
#define EXIT_ERROR                \
        "CP_scan_qc exits with error"
#define WRONG_SEGMENT_TIME                \
        "stop time less than start time in %dth SEGMENT"
#define WRONG_FRAME_TIME                  \
        "stop time less than start time in FRAME of %dth SEGMENT"
#define TOO_LARGE_TIME_GAP                \
        "Start/end time gap too large in segment %d"
#define CANT_FIND_SEGMENT_ID              \
        "SEGMENT_ID keyword missing from segment %d"
#define INVALID_SCENE_CENTER_TIME         \
        "Invalid scene center time"
#define INVALID_SCANT_START_TIME          \
        "Invalid scan start time"
#define INVALID_SCANT_STOP_TIME           \
        "Invalid scan end time"
#define INVALID_SEGMENT_START_TIME        \
        "Invalid start time for segment %d"
#define INVALID_SEGMENT_STOP_TIME         \
        "Invalid end time for segment %d"
#define CANT_FIND_SEGMENT_START_TIME      \
        "Can't read start time for segment %d"
#define CANT_FIND_SEGMENT_STOP_TIME       \
        "Can't read end time for segment %d"
#define CANT_FIND_SCAN_START_TIME         \
        "Can't read datatake start time"
#define CANT_FIND_MODE_FROM_DATATAKE      \
        "MODE keyword missing for this datatake"
#define CANT_FIND_SEGMENT_IN_SCAN         \
        "WARNING: no segments found in scan results file %s"
#define CANT_FIND_SCAN_STOP_TIME          \
        "Can't read datatake end time"
#define CANT_FIND_INSTRUMNET_MODE         \
        "Can't read instrument mode from scan results file -- reading from filename"
#define CANT_MALLOC_FRAME_ENTRY           \
        "Not enough memory"
#define CANT_FIND_FRAME_COUNT             \
        "Can't read number of frames from segment %d"
#define SEGMENT_COUNT_LARGER              \
        "Segment count is greater than number of segments in %s"
#define SEGMENT_COUNT_SMALLER             \
        "Segment count is less than number of segments in %s"
#define INCORRECT_SEGMENT_COUNT           \
        "Segment count is less than zero in %s"
#define CANT_FIND_SEQ_NAME                \
        "Can't read sequence from scan results file"
#define CANT_FIND_PLATFORM                \
        "Can't read platform from scan results file -- reading from filename"
#define CANT_FIND_PLAT_NAME               \
        "Can't read platform from scan results file"
#define CANT_FIND_REV_NAME                \
        "Can't read revolution from scan results file"
#define CANT_FIND_JOB_ID              \
        "Can't read job id from scan results file -- reading from filename"
#define CANT_FIND_REVOLUTION              \
        "Can't read revolution from scan results file -- reading from filename"
#define CANT_FIND_SEQUENCE                \
        "Can't read sequence from scan results file -- reading from filename"
#define CANT_FIND_SEGMENT_COUNT           \
        "Can't read segment count from scan results file"
#define CANT_OPEN_SCAN_RESULT_FILE        \
        "Can't open scan results file %s: %s"
#define CANT_FIND_STATE_VECTOR_PRECISION  \
        "Can't read state vector precision from scan results file"
#define CANT_FIND_SCENE_CENTER_TIME       \
        "Can't read scene center time from scan results file"
#define CANT_FIND_FRAME_MODE              \
        "Can't read frame mode from scan results file"
#define CANT_FIND_STATION_ID              \
        "Can't read station ID from scan results file"
#define CANT_FIND_SRF_BODY                \
        "Can't read BODY object from scan results file %s"
#define CANT_FIND_SCENE_CENTER_LON        \
        "Can't read scene center longitude from scan results file"
#define CANT_ACCESS_FILE                  \
        "file %s does not exist"
#define CANT_FIND_FRAME_ID                \
        "Can't read frame ID from scan results file"
#define CANT_FIND_SCENE_CENTER_LAT        \
        "Can't read scene center latitude from scan results file"
#define CFG_FILE_NOT_SPECIFIED            \
        "Input needs to specify configuration file"
#define CANT_OPEN_CFG_FILE                \
        "Can't open configuration file : %s"
#define CFG_FILE_STATUS_ERROR             \
        "Configuration file %s : file status error"
#define CFG_FILE_READ_ERROR               \
        "Can't read %d from configuration file %s"
#define CANT_PARSE_CFG_FILE               \
        "Can't parse configuration file"
#define ASSIGN_STATUS_TO_ERROR            \
        "Assigned status ERROR(%d) to scan results file %s"
#define ASSIGN_STATUS_TO_ACCEPT           \
        "Assigned status ACCEPT to scan results file %s"
#define ASSIGN_STATUS_TO_REJECT           \
        "Assigned status REJECT to scan results file %s"
#define ASSIGN_STATUS_TO_HOLD             \
        "Assigned status HOLD to scan results file %s"
#define PRINT_SCREEN_DUMP                 \
        "printing screen dump with command: %s"

#endif /* ! _SCAN_QC_DEF_H */
