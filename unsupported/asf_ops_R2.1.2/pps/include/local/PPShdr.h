/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	PPShdr.h
Description:	
		Header file for IMS, CP functions.  Includes IMS, CP keywords
	 
Creator:        Nadia Adhami (nadia.adhami@jpl.nasa.gov)
Notes:
==============================================================================*/

#ifndef _PPSHDR_
#define _PPSHDR_

#pragma ident "@(#)PPShdr.h	1.3  04/23/97"

#ifndef ERROR
#include "defs.h"
#endif

/* default configuration file */

#define PPS_DEFAULT_CONFIG	"pps.config"

/* satelite name */

#define R1_SATNAME		"R1"

/* configuration file keywords */

#define	NUM_THREADS_KEYWD	"NUM_THREADS"

#define	PPS_DBNAME_KEYWD	"PPS_DBNAME"
#define	PPS_USERID_KEYWD	"PPS_USERID"
#define	PPS_PASSWD_KEYWD	"PPS_PASSWD"
#define	PPS_SERVER_KEYWD	"PPS_SERVER"

#define	IMS_DBNAME_KEYWD	"IMS_DBNAME"
#define	IMS_USERID_KEYWD	"IMS_USERID"
#define	IMS_PASSWD_KEYWD	"IMS_PASSWD"
#define	IMS_SERVER_KEYWD	"IMS_SERVER"

#define	APPLICATION_NAME	"pps_server"

/* table names in Sybase 
 * Note: a change in these names requires a
 * corresponding update to the name in libpps_db */

#define L1_ORDERS_TABLENAME	"L1_orders"
#define SCAN_ORDERS_TABLENAME	"scan_orders"
#define JOBS_TABLENAME		"jobs"
#define POLICY_TABLENAME	"policy"
#define SCHEDULE_TABLENAME	"schedule"
#define L1_PROC_PARMS_TABLENAME	"L1_procparms"

/* IMS  id + Keyword record structure */

typedef struct _keywords
{
   int id ;                                      /* Keyword Id */
   char *keyword ;                               /* Keyword string */

} IMS_Keywords ;

/* Maximum IMS message size */

#define MSG_SIZE	5000

/* maximum number of retries for PPS query deadlock */
 
#define PPS_MAX_RETRY   5
 
/* maximum number of retries for IMS Queries */

#define	IMS_MAX_RETRY	3

/* maximum number of threads */

#define	MAX_NUM_THREADS	3

/* PPS bound file types */

#define IMS_L1PR 		300  /* Level 1 Product Request */
#define IMS_SCAN        	301  /* Scan Request */
#define IMS_SV_AVAIL		303  /* State Vector Availabe */
#define IMS_CANCEL		304  /* Cancel Request */

#define CP_SCAN			305  /* SCan Request */
#define CP_FRAME 		306  /* Level 1 Product Request */
#define CP_JOB_REQUEST		307  /* Job Request */
#define CP_STATUS_MESSAGE	308  /* Status Message */

/* IMS file type strings */

#define FRAME_KEYWD       	"FRAME"
#define L1_KEYWD          	"L1"
#define SCAN_KEYWD        	"SCAN"
#define CP_SCAN_KEYWD     	"CP_SCAN"
#define CP_FRAME_KEYWD    	"CP_FRAME"
#define CP_SCAN_COMP_KEYWD   	"CP_SCAN_COMPLETE"
#define JOB_REQUEST_KEYWD 	"SPS_JOB_REQUEST"
#define STATUS_MESSAGE_KEYWD    "SPS_JOB_STATUS"
#define CANCEL_KEYWD    	"CANCEL"

#define CANCEL_ORDER_KEYWD      "CANCEL_ORDER"
#define FRAME_ORDER_KEYWD       "FRAME_ORDER"
#define L1PR_ORDER_KEYWD        "FRAME_ORDER"
#define SCAN_ORDER_KEYWD        "SCAN_ORDER"

 
/*
-- Note : Modify these as necessary.  
--        Ex. MAX_KEYTABLE_SIZE when 
--            adding entries.
*/
#define HEADER_KEYWDS       8 
#define MAX_KEYTABLE_SIZE  32 
 
#define COMMON_HDR        0
#define TIME              1
#define MSG_TYPE          2
#define DESTINATION       3
#define SOURCE            4
#define NUM_RECS          5
#define L1PR	          6
#define SCAN	          7
#define STVEC		  8
#define CANCEL_ORDER      9 
#define JOB_REQUEST       10 
#define STATUS_MESSAGE    11 

/* IMS, CP defined identifiers */

#define IMS_KEYWD          	"IMS"
#define PPS_KEYWD         	"PPS"
#define CP_KEYWD           	"CP"
#define COMMON_HDR_KEYWD   	"COMMON_HEADER"
#define TIME_KEYWD         	"TIME"
#define MSG_TYPE_KEYWD     	"MSG_TYPE"
#define DESTINATION_KEYWD  	"DESTINATION"
#define SOURCE_KEYWD       	"SOURCE"
#define NUM_RECS_KEYWD     	"NUMBER_OF_RECORDS"
#define STVEC_KEYWD        	"STATE_VECTOR_AVAIL"
#define SOURCE_MEDIA_KEYWD 	"SOURCE_MEDIA"
#define BODY_KEYWD         	"BODY"
#define TYPE_KEYWD         	"TYPE"
#define QUICKLOOK_FLAG_KEYWD	"QUICKLOOK_FLAG"

/* CP jobs */

#define FRAME_JOB_KEYWD   "FRAME_REQUEST"
#define SCAN_JOB_KEYWD    "SCAN_REQUEST"

/* Valid IMS parameters (Keywords inside ODL Object Aggregations) */

#define SATELLITE      	0
#define SENSOR         	1
#define MODE           	2
#define REV            	3
#define DATATAKES      	4
#define ACTIVITY_ID    	5
#define AGENCY         	6
#define RECORDED       	7
#define MEDIA_ID       	8
#define START_ADDRESS  	9
#define END_ADDRESS   	10
#define START_TIME    	11
#define END_TIME      	12
#define RECORDER_ID   	13
#define TIMEON        	14
#define TIMEOFF       	15
#define TRANSMITTERID 	16
#define SITE_NAME      	17
#define ORDER_ID      	18
#define ORDER_TYPE    	19
#define PRIORITY      	20
#define MEDIA_ID_TYPE  	21
#define MEDIA_ID_NUMBER	22
#define APPEND_MODE   	23
#define ITEM_ID       	24
#define DESKEW		25
#define FRAME_ID	26
#define FRAME_MODE	27
#define OUTPUT_FORMAT	28
#define PRODUCT_TYPE	29
#define PIXEL_SPACING	30
#define PROCESSING_GAIN	31
#define PROJECTION	32
#define SEQUENCE	33
#define SUBFRAME_ID	34
#define STATION_ID	35


/* Corresponding keywords for IMS, CP parameters */

#define ACTIVITY_ID_KEYWD     "ACTIVITY_ID"
#define AGENCY_KEYWD          "AGENCY"
#define ANGLE_KEYWD           "ANGLE"
#define APPEND_MODE_KEYWD     "APPEND_MODE"
#define AVG_TERRAIN_HT_KEYWD  "AVG_TERRAIN_HT"
#define CENTER_TIME_KEYWD     "CENTER_TIME"
#define CLOCK_CYCLE_KEYWD     "CLOCK_CYCLE"
#define COMMENT_KEYWD         "COMMENT"
#define COMPENSATION_KEYWD    "COMPENSATION_FLAG"
#define COMPLEX_KEYWD         "COMPLEX"
#define COORDINATE_SYSTEM_KEYWD "COORDINATE_SYSTEM"
#define DATADIRECTION_KEYWD   "DATA_DIRECTION"
#define DATASET_KEYWD         "DATASET"
#define DATATAKES_KEYWD       "DATATAKES"
#define DATATAKE_SEQ_KEYWD    "DATATAKE_SEQ"
#define DESKEW_KEYWD          "DESKEW"
#define DISK_KEYWD            "DISK"
#define DCRSI_KEYWD           "DCRSI"
#define END_ADDRESS_KEYWD     "END_ADDRESS"
#define END_REV_KEYWD         "END_REV"
#define END_TIME_KEYWD        "END_TIME"
#define FRAME_ID_KEYWD        "FRAME_ID"
#define FRAME_MODE_KEYWD      "FRAME_MODE"
#define GHA_CORRECTION_KEYWD  "GHA_CORRECTION"
#define	IMS_ORDER_STATUS_KEYWD          "IMS_ORDER_STATUS"
#define INSERT_TOP_KEYWD      "INSERT_TOP"
#define ITEM_ID_KEYWD         "ITEM_ID"
#define JOB_ID_KEYWD          "JOB_ID"
#define LAMBERT_KEYWD  	      "LAMBERT"
#define LAMBERT_LATITUDE_N_KEYWD  	"LAMBERT_LATITUDE_N"
#define LAMBERT_LATITUDE_S_KEYWD  	"LAMBERT_LATITUDE_S"
#define MEDIA_ID_KEYWD        "MEDIA_ID"
#define MEDIA_ID_NUMBER_KEYWD "MEDIA_ID_NUMBER"
#define MEDIA_ID_TYPE_KEYWD   "MEDIA_ID_TYPE"
#define MEDIA_LOCATION_KEYWD  "MEDIA_LOCATION"
#define MEDIA_TYPE_KEYWD      "MEDIA_TYPE"
#define MODE_KEYWD            "MODE"
#define ORDER_ID_KEYWD        "ORDER_ID"
#define ORDER_TYPE_KEYWD      "ORDER_TYPE"
#define OUTPUT_FORMAT_KEYWD   "OUTPUT_FORMAT"
#define PIXEL_SPACING_KEYWD   "PIXEL_SPACING"
#define PLATFORM_KEYWD        "PLATFORM"
#define PLATFORM_TIME_KEYWD   "PLATFORM_TIME"
#define PRIORITY_KEYWD        "PRIORITY"
#define PROCESSING_GAIN_KEYWD "PROCESSING_GAIN"
#define PROCESSOR_MODE_KEYWD  "PROCESSOR_MODE"
#define PRODUCT_FILENAME_KEYWD    "PRODUCT_FILENAME"
#define PRODUCT_ID_KEYWD      "PRODUCT_ID"
#define PRODUCT_TYPE_KEYWD    "PRODUCT_TYPE"
#define PROJECTION_KEYWD      "PROJECTION"
#define PS_KEYWD              "PS"
#define PS_REFERENCE_LAT_KEYWD  	"PS_REFERENCE_LAT"
#define PS_REFERENCE_LON_KEYWD  	"PS_REFERENCE_LON"
#define PRIM_CALPARMS_KEYWD   "CALPARMS_FILE_PRIMARY"
#define SEC_CALPARMS_KEYWD    "CALPARMS_FILE_SECONDARY"
#define RECORDED_KEYWD        "RECORDED"
#define RECORDER_ID_KEYWD     "RECORDER_ID"
#define REQUEST_TYPE_KEYWD    "REQUEST_TYPE"
#define REV_KEYWD             "REV"
#define REVOLUTION_KEYWD      "REVOLUTION"
#define SATELLITE_KEYWD       "SATELLITE"
#define SENSOR_KEYWD          "SENSOR"
#define SEQUENCE_KEYWD        "SEQUENCE"
#define SCAN_RESULTS_FILE_KEYWD  "SCAN_RESULTS_FILE"
#define SITE_NAME_KEYWD       "SITE_NAME"
#define START_ADDRESS_KEYWD   "START_ADDRESS"
#define START_REV_KEYWD       "START_REV"
#define START_TIME_KEYWD      "START_TIME"
#define STATE_VECTOR_DATA_KEYWD      "STATE_VECTOR_DATA"
#define STATE_VECTOR_METADATA_KEYWD  "STATE_VECTOR_METADATA"
#define STATE_VECTOR_RECORD_KEYWD    "STATE_VECTOR_RECORD"
#define STATE_VECTOR_PRECISION_KEYWD	"STATE_VECTOR_PRECISION"
#define STATION_ID_KEYWD      "STATION_ID"
#define STATUS_ID_KEYWD       "STATUS_ID"
#define STATUS_KEYWD          "STATUS"
#define STATUS_TYPE_KEYWD     "STATUS_TYPE"
#define SUBFRAME_ID_KEYWD     "SUBFRAME_ID"
#define TERRAIN_CORRECTION_KEYWD  	"TERRAIN_CORRECTION"
#define TIME_CORRELATION_KEYWD  	"TIME_CORRELATION"
#define TIMEOFF_KEYWD         "TIME_OFF"
#define TIMEON_KEYWD          "TIME_ON"
#define TRANSMITTERID_KEYWD   "TRANSMITTER_ID"
#define UTM_ZONE_KEYWD        "UTM_ZONE"
#define UTM_KEYWD             "UTM"
#define X_POSITION_KEYWD      "X_POSITION"
#define Y_POSITION_KEYWD      "Y_POSITION"
#define Z_POSITION_KEYWD      "Z_POSITION"
#define X_VELOCITY_KEYWD      "X_VELOCITY"
#define Y_VELOCITY_KEYWD      "Y_VELOCITY"
#define Z_VELOCITY_KEYWD      "Z_VELOCITY"

/* string length of fields within records */

#define	ACTIVITY_ID_STRLEN			3 
#define	APP_MODE_STRLEN				3 
#define	CAL_PARAMS_FILE_STRLEN			40
#define	COMMENT_STRLEN				255 
#define	COORD_SYS_STRLEN			15 
#define DATA_DIRECTION_STRLEN			7
#define	FRAME_MODE_STRLEN			9 
#define	INSERT_TOP_FLAG_STRLEN			3 
#define	LOGICAL_STRLEN				3 
#define	MEDIA_ID_STRLEN				12 
#define	MEDIA_ID_TYPE_STRLEN			2 
#define	MEDIA_LOCATION_STRLEN			128 
#define	MEDIA_TYPE_STRLEN			5 
#define	MODE_STRLEN				3 
#define	ORDER_STATUS_STRLEN			15 
#define	ORDER_TYPE_STRLEN			3
#define	OUTPUT_FMT_STRLEN			4 
#define	PLAY_DIR_STRLEN				7 
#define	PLATFORM_STRLEN				2 
#define	PN_TYPE_STRLEN				8 
#define	PRIORITY_STRLEN				10 
#define	PROCESSOR_MODE_STRLEN			10 
#define	PROJECTION_STRLEN			12 
#define	PRODUCT_TYPE_STRLEN			13 
#define	RECORDER_ID_STRLEN			16 
#define	REQUEST_TYPE_STRLEN			10 
#define	SATELLITE_STRLEN			2 
#define	SCAN_RESULTS_FILE_STRLEN		255
#define	SENSOR_STRLEN				1 
#define	SITE_NAME_STRLEN			128 
#define	STATION_ID_STRLEN			2 
#define	SVEC_TYPE_STRLEN			10
#define TIME_STRLEN 				21

/* * * * * * * * * * * * * * * * * * * * * * * * */
/* These values correspond to the database rules */
/* * * * * * * * * * * * * * * * * * * * * * * * */

/* job types */

#define	SCAN_JOB_TYPE		"SCAN"
#define	L1_JOB_TYPE		"L1"

/* Order Status */

#define AVAILABLE		"AVAILABLE"
#define CANCELFAIL		"CANCEL/FAIL"
#define FAILED			"FAILED"
#define COMPLETED		"COMPLETED"
#define PENDING			"PENDING"
#define READY			"READY"
#define RETRY			"RETRY"
#define SUBMITTED		"SUBMITTED"
#define INTERMEDIATE		"INTERMEDIATE"
#define FINAL			"FINAL"

/* State Vector Types */

#define RESTITUTED		"RESTITUTED"
#define PREDICTED		"PREDICTED"

/* macros to convert time string to ODL format */
/* ODL date : yyyy-dddThh:mm:ss.sss */
/* Time Rec : yyyy-ddd-hh:mm:ss.sss */

#define CONVERT_ODL_TO_TIMESTR(str,ptr) if (ptr = strchr((str), 'T')) *ptr = '-'
#define CONVERT_TIMESTR_TO_ODL(str,ptr) if (ptr = strchr((str), '-')) *ptr = 'T'


#endif _PPSHDR_
