/* Exit stati for qc and scan qc apps */

#ifndef _QEXIT_STATI_H
#define _QEXIT_STATI_H

static char sccsid_qexit_stati_h[] = "@(#)qexit_stati.h	1.8 97/08/11 09:52:12";

/* Exit stati */
#define  SCAN_QC_ERROR        -1
#define  QC_HOLD               50
#define  QC_REJECT             51
#define  QC_ACCEPT             52
#define  QC_HELP               53
#define  QC_DONE               54

/* exit values for qc app only */
#define  QC_NO_LEADER_FILE     4
#define  QC_NO_CEOS_FILE       444
#define  QC_MALLOC_ERROR       5
#define  QC_DISPLAY_ERROR      6
#define  QC_CALLOC_ERROR       7
#define  QC_XAllocColor_ERROR  8

#define  QC_OPEN_LEADER_FILE_ERROR         9
#define  QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR         10

#define  QC_PROGRAMMER_ERROR               11

#define  QC_OPEN_CEOS_FILE_ERROR           12
#define  QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR           13
#define  QC_NON_8_BIT_IMAGE                14

#define  QC_LINE_SIZE_MISMATCH             15
#define  QC_CHANNEL_SIZE_MISMATCH          16

#define  QC_OPEN_AVERAGE_FILE_ERROR        17
#define  QC_AVERAGE_FILE_UNEXPECTED_READ_RETURN_ERROR        18

#define  QC_IMPROPER_CEOS_VALUE            19
#define  QC_UNKNOWN_IMAGE_TYPE             20

#define  QC_PIXMAP_CREATE_PROBLEM          21

/*#define  QC_OPEN_AVG_OUT_FILE_ERROR        22 */

#define  QC_XAllocColorCells_ERROR         23


/* descriptive strings for qc app only */
#define  QC_HOLD_TXT                 "Hold"
#define  QC_REJECT_TXT               "Reject"
#define  QC_ACCEPT_TXT               "Accept"
#define  QC_HELP_TXT                 "Help"
#define  QC_DONE_TXT                 "Done"
#define  QC_NO_LEADER_FILE_TXT       "Leader file does not exist"
#define  QC_NO_CEOS_FILE_TXT         "Image file does not exist"
#define  QC_MALLOC_ERROR_TXT         "Not enough memory to continue"
#define  QC_DISPLAY_ERROR_TXT        "Cannot open X display"
#define  QC_CALLOC_ERROR_TXT         "Not enough memory to continue"
#define  QC_XAllocColor_ERROR_TXT    "Cannot allocate colormap"

#define  QC_OPEN_LEADER_FILE_ERROR_TXT  "Cannot read leader file"
#define  QC_LEADER_FILE_UNEXPECTED_READ_RETURN_ERROR_TXT \
                                "Cannot read complete record from leader file"

#define  QC_PROGRAMMER_ERROR_TXT     "Internal software error"

#define  QC_OPEN_CEOS_FILE_ERROR_TXT "Cannot read image file"
#define  QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR_TXT \
                          "Cannot read complete record from image file"
#define  QC_NON_8_BIT_IMAGE_TXT      "Sample size must be 8 bits"

#define  QC_LINE_SIZE_MISMATCH_TXT   "Inconsistent record length in image file"
#define  QC_CHANNEL_SIZE_MISMATCH_TXT \
                            "Inconsistent number of records in image file"

#define  QC_OPEN_AVERAGE_FILE_ERROR_TXT "Cannot read intermediate file"
#define  QC_AVERAGE_FILE_UNEXPECTED_READ_RETURN_ERROR_TXT  \
                         "Cannot read complete record from intermediate file"

#define  QC_IMPROPER_CEOS_VALUE_TXT     "QC_IMPROPER_CEOS_VALUE"
#define  QC_UNKNOWN_IMAGE_TYPE_TXT      "Unrecognized image type"

#define  QC_PIXMAP_CREATE_PROBLEM_TXT   "Cannot create X pixmap"

#define  QC_OPEN_AVG_OUT_FILE_ERROR_TXT "QC_OPEN_AVG_OUT_FILE_ERROR"

#define  QC_XAllocColorCells_ERROR_TXT  "Cannot allocate colormap"

/* exit values for scan qc app only */

#define  QC_SCAN_ERROR         100  /* CP_scan_qc error */
#define  QC_SRF_ERROR          101  /* scan results file not exist */
#define  QC_PARSE_SRF_ERROR    102  /* ODL parse scan results file error */
#define  QC_BODY_SRF_ERROR     103  /* no BODY in scan results file */
#define  QC_CFG_ERROR          104  /* config file not exist */
#define  QC_CFG_RD_ERROR       105  /* config file read error */
#define  QC_NO_CFG_ERROR       106  /* config file not specified */
#define  QC_PARSE_CFG_ERROR    107  /* ODL parse config file error */
#define  QC_SEG_CNT_LESS_ERROR 108  /* SEGMENT_COUNT < # of SEGMENT */
#define  QC_SEG_CNT_LARG_ERROR 109  /* SEGMENT_COUNT > # of SEGMENT */
#define  QC_SEG_CNT_SRF_ERROR  110  /* no SEGMENT_COUNT in scan results file */
#define  QC_BAD_SEG_CNT_ERROR  111  /* incorrect SEGMENT_COUNT value */
#define  QC_FRM_ID_SRF_ERROR   112  /* no FRAME_ID in scan results file */
#define  QC_SCLAT_SRF_ERROR    113  /* no SCENE_CENTER_LAT in srf */
#define  QC_SCLON_SRF_ERROR    114  /* no SCENE_CENTER_LON in srf */
#define  QC_SCTM_SRF_ERROR     115  /* no SCENE_CENTER_TIME in srf */
#define  QC_MALLOC_FRM_ERROR   116  /* error on malloc for frame list */
#define  QC_PLATFM_SRF_ERROR   117  /* no PLATFORM in scan results file */
#define  QC_STATION_SRF_ERROR  118  /* no STATION_ID in scan results file */
#define  QC_SEGCNT_SRF_ERROR   119  /* no SEGMENT_COUNT in scan results file */
#define  QC_FRM_MD_SRF_ERROR   120  /* no FRAME_MODE in scan results file */
#define  QC_REV_SRF_ERROR      121  /* no REVOLUTION in scan results file */
#define  QC_SEQ_SRF_ERROR      122  /* no SEQUENCE in scan results file */
#define  QC_SRT_TM_SRF_ERROR   123  /* no START_TIME in scan results file */
#define  QC_END_TM_SRF_ERROR   124  /* no END_TIME in scan results file */
#define  QC_SEG_ID_SRF_ERROR   125  /* no SEGMENT_ID in ith SEGMENT */
#define  QC_FRM_CT_SRF_ERROR   126  /* no FRAME_COUNT in ith SEGMENT */
#define  QC_SEG_SRT_TM_ERROR   127  /* no SEGMENT_START_TIME in ith SEGMENT */
#define  QC_SEG_STP_TM_ERROR   128  /* no SEGMENT_STOP_TIME in ith SEGMENT */
#define  QC_LARGE_TM_GAP_ERROR 129  /* time gap too large in ith SEGMENT */
#define  QC_MALLOC_FRAME_ERROR 130  /* malloc for frame err in ith SEGMENT */
#define  QC_WRG_SEG_TM_ERROR   131  /* stop < start SEGMENT time in ith SEGMENT */
#define  QC_WRG_FRM_TM_ERROR   132  /* stop < start FRAME time in ith FRAME */
#define  QC_IVL_SRT_TM_ERROR   133  /* invalid SEGMENT_START_TIME in ith SEGMENT */
#define  QC_IVL_STP_TM_ERROR   134  /* invalid SEGMENT_STOP_TIME in ith SEGMENT */
#define  QC_IVL_SCN_SRT_ERROR  135  /* invalid SCAN_START_TIME*/ 
#define  QC_IVL_SCN_STP_ERROR  136  /* invalid SCAN_STOP_TIME*/
#define  QC_IVL_CNT_TM_ERROR   137  /* invalid SCENE_CENTER_TIME*/

/* descriptive strings for scan qc app only */

#define  QC_SCAN_ERROR_TXT         "General error"
#define  QC_SRF_ERROR_TXT          "Scan results file did not exist "
#define  QC_PARSE_SRF_ERROR_TXT    "Scan results file parse error "
#define  QC_BODY_SRF_ERROR_TXT     "No BODY in scan results file "
#define  QC_CFG_ERROR_TXT          "Config file did not exist"
#define  QC_CFG_RD_ERROR_TXT       "Error reading config file"
#define  QC_NO_CFG_ERROR_TXT       "No config file specified"
#define  QC_PARSE_CFG_ERROR_TXT    "Config file parse error"
#define  QC_SEG_CNT_LESS_ERROR_TXT "SEGMENT_COUNT < # of SEGMENTs"
#define  QC_SEG_CNT_LARG_ERROR_TXT "SEGMENT_COUNT > # of SEGMENTs"
#define  QC_SEG_CNT_SRF_ERROR_TXT  "No SEGMENT_COUNT in scan results file"
#define  QC_BAD_SEG_CNT_ERROR_TXT  "Incorrect SEGMENT_COUNT value"
#define  QC_FRM_ID_SRF_ERROR_TXT   "No FRAME_ID in scan results file"
#define  QC_SCLAT_SRF_ERROR_TXT    "No SCENE_CENTER_LAT in scan results file"
#define  QC_SCLON_SRF_ERROR_TXT    "No SCENE_CENTER_LON in scan results file"
#define  QC_SCTM_SRF_ERROR_TXT     "No SCENE_CENTER_TIME in scan results file"
#define  QC_MALLOC_FRM_ERROR_TXT   "Not enough memory to process frame list"
#define  QC_PLATFM_SRF_ERROR_TXT   "No PLATFORM in scan results file"
#define  QC_STATION_SRF_ERROR_TXT  "No STATION_ID in scan results file"
#define  QC_SEGCNT_SRF_ERROR_TXT   "No SEGMENT_COUNT in scan results file"
#define  QC_FRM_MD_SRF_ERROR_TXT   "No FRAME_MODE in scan results file"
#define  QC_REV_SRF_ERROR_TXT      "No REVOLUTION in scan results file"
#define  QC_SEQ_SRF_ERROR_TXT      "No SEQUENCE in scan results file"
#define  QC_SRT_TM_SRF_ERROR_TXT   "No START_TIME in scan results file"
#define  QC_END_TM_SRF_ERROR_TXT   "No END_TIME in scan results file"
#define  QC_SEG_ID_SRF_ERROR_TXT   "No SEGMENT_ID in ith SEGMENT"
#define  QC_FRM_CT_SRF_ERROR_TXT   "No FRAME_COUNT in ith SEGMENT"
#define  QC_SEG_SRT_TM_ERROR_TXT   "No SEGMENT_START_TIME in ith SEGMENT"
#define  QC_SEG_STP_TM_ERROR_TXT   "No SEGMENT_STOP_TIME in ith SEGMENT"
#define  QC_LARGE_TM_GAP_ERROR_TXT "Time gap too large in ith SEGMENT"
#define  QC_MALLOC_FRAME_ERROR_TXT "Not enough memory to process frame in ith SEGMENT"
#define  QC_WRG_SEG_TM_ERROR_TXT   "Stop < start SEGMENT time in ith SEGMENT"
#define  QC_WRG_FRM_TM_ERROR_TXT   "Stop < start FRAME time in ith FRAME"
#define  QC_IVL_SRT_TM_ERROR_TXT   "Invalid SEGMENT_START_TIME in ith SEGMENT"
#define  QC_IVL_STP_TM_ERROR_TXT   "Invalid SEGMENT_STOP_TIME in ith SEGMENT"
#define  QC_IVL_SCN_SRT_ERROR_TXT  "Invalid SCAN_START_TIME"
#define  QC_IVL_SCN_STP_ERROR_TXT  "Invalid SCAN_STOP_TIME"
#define  QC_IVL_CNT_TM_ERROR_TXT   "Invalid SCENE_CENTER_TIME"


#endif /* _QEXIT_STATI_H */
