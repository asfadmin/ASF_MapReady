/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	PPSdefs.h

Description:	
	 This contains the data structure definition for the IMS file.
	 
Creator: Nadia Adhami (nadia.adhami@jpl.nasa.gov)
Notes:
==============================================================================*/

#ifndef _PPSDEFS_
#define _PPSDEFS_

#pragma ident "@(#)PPSdefs.h	1.3  12/16/96"

#ifndef ERROR
#include "defs.h"
#endif

#include "PPShdr.h"
#include "timerec.h"

#ifndef MIN_OF_TWO
#define MIN_OF_TWO(a,b) ((a) < (b) ? (a) : (b))
#endif
#ifndef MAX_OF_TWO
#define MAX_OF_TWO(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef PPS_STRNCPY
#define PPS_STRNCPY(to,from,len) \
{ \
    (void)memset((to),0,((len)+1)); \
    (void)strncpy((to),(from),(len)); \
}
#endif /* PPS_STRNCPY */

#ifndef CHECK_DEADLOCK
#define CHECK_DEADLOCK(errormsg) \
{ \
	if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE) \
	{ \
		pps_logMsg(ProgName, PPS_ERROR, errormsg); \
		return (ER_DEADLOCK); \
	} \
}
#endif /* CHECK_DEADLOCK */

typedef enum
{
        PPS_USERID = 0,
        PPS_PASSWD,
	PPS_SERVER,
        PPS_DBNAME,
        IMS_USERID,
        IMS_PASSWD,
        IMS_SERVER,
        IMS_DBNAME,
	NUM_THREADS,
        AUTH_USERS,
        NUM_CONFIGS
 
} PPS_CONFIG_ENUM;

typedef struct pps_config_struct
{
    char    isRequired;     /* TRUE or FALSE */
    char    isSingleValue;  /* TRUE or FALSE */
    char    keyword[MAXLINE];
    char    value[MAXLINE];

} PPSConfigStruct;

typedef char CONFIG_VALUES[NUM_CONFIGS][MAXLINE];

typedef struct db_proc_handle
{
   void*	dbproc_server;		/* database login handle */
   void*	dbproc_commit;		/* used for commit calls */
   void*	dblogin;		/* database login rec */
   void*	ims_aux_query;		/* IMS query struct */
   int 		free;			/* flag used/free */
} DB_Proc;

/* IMS record structure */

typedef struct GHA_Correction 
{
   Time_Record	time ;			/* time */
   float  	angle ;			/* angle */

} GHA_Correction;

typedef struct Time_Correlation
{
   int    rev ;				/* rev */
   Time_Record time ;			/* time */
   int    sat_time ;			/* satellite time */
   int    clock_cycle ;			/* clock cycle */
} Time_Correlation ;

typedef struct State_Vector
{
   char precision[SVEC_TYPE_STRLEN+1] ;	/* restituted,predicted*/
   char coord_sys[COORD_SYS_STRLEN+1] ;	/* coord system */
   char platform[PLATFORM_STRLEN+1] ;   /* platform E1,E2,J1,R1 */
   int  rev ;				/* rev */
   Time_Record time ;			/* time */
   float  x_pos, y_pos, z_pos ;		/* position */
   float  x_vel, y_vel, z_vel ;		/* velocity */
} State_Vector ;

typedef struct State_Vector_Avail
{
   char file_name[MAX_FILENAME_LEN] ;
   char platform[PLATFORM_STRLEN+1] ;   	/* platform E1,E2,J1,R1 */
   char type[SVEC_TYPE_STRLEN+1] ;		/* predicted/restituted */
   int  start_rev ;				/* rev */
   int  end_rev ;				/* rev */
   Time_Record start_time ;			/* time */
   Time_Record end_time ;			/* time */

} IMS_SVecAvail_Record;

typedef struct ims_hdr_rec
{
   int  num_records ;                            /* Number of records */
   char file_name[MAX_FILENAME_LEN] ;		/* File name */
   int  file_type_id ;				/* Filt Type Id Number */
   char file_type[MAXLINE] ;			/* File Type */
   char file_creation_time[TIME_STRING_LEN+1] ;	/* File Creation Time */
   Time_Record date_time ;			/* creation time */

} Common_Header_Record ;


typedef struct ims_cancel_req
{
 
   char file_name[MAX_FILENAME_LEN] ;
   int  order_id ;                              /* order id */
   int  item_id ;                               /* item id */
   char request_type[REQUEST_TYPE_STRLEN+1] ;
} IMS_CancelReq_Record ;


typedef struct ims_l1pr_req
{

   char file_name[MAX_FILENAME_LEN] ;

   int  order_id ;				/* order id */
   int  item_id ;				/* item id */
   int  job_id ;				/* job id */

   char order_type[ORDER_TYPE_STRLEN+1] ;	/* order type */
   char priority[PRIORITY_STRLEN+1] ;		/* priority */
   char platform[PLATFORM_STRLEN+1] ;           /* platform E1,E2,J1,R1 */
   char sensor[SENSOR_STRLEN+1] ;         	/* sensor S,D,O,V */
   int  rev ;					/* rev */
   char mode[MODE_STRLEN+1] ;            	/* mode S1..S7,SS1..SS4,A */
   char quicklook_flag[LOGICAL_STRLEN+1];	/* yes/no */

   int  datatake_seq;				/* sequence */
   char activity_id[ACTIVITY_ID_STRLEN+1] ;    	/* activity id */
   char product_type[PRODUCT_TYPE_STRLEN+1] ; 	/* product type */
   float  pixel_spacing ;				/* pixel spacing */
   int  frame_id ;				/* 0..899 */
   int  subframe_id ;				/* 0..8 */
   char frame_mode[FRAME_MODE_STRLEN+1] ; 	/* ARCTIC,ANTARCTIC */
   char output_format[OUTPUT_FMT_STRLEN+1] ; 	/* CEOS,HDF */
   char site_name[SITE_NAME_STRLEN+1] ; 	/* site name */
   char projection[PROJECTION_STRLEN+1] ; 	/* projection */
   int  processing_gain ;			/* processing gain */
   char deskew[LOGICAL_STRLEN+1] ; 		/* yes/no */
   char compensation_flag[LOGICAL_STRLEN+1] ;   /* yes/no */
   float  avg_terrain_ht ;			/* terrain hit */
   float  ps_reference_lat ;			/* reference lat */
   float  ps_reference_lon ;			/* reference lon */
   float  lambert_latitude_n ;			/* lambert lat */
   float  lambert_latitude_s ;			/* lambert lat */
   int  utm_zone ;				/* UTM zone */
   char terrain_correction[LOGICAL_STRLEN+1] ; 	/* yes/no */

   char cal_params_file[CAL_PARAMS_FILE_STRLEN+1] ; /* calibration params 1*/
   char cal_params_file2[CAL_PARAMS_FILE_STRLEN+1] ; /* calibration params 2 */
   char scan_results_file[SCAN_RESULTS_FILE_STRLEN+1] ; /* scan results */

   char media_id[MEDIA_ID_STRLEN+1] ;		/* media id */
   char media_type[MEDIA_TYPE_STRLEN+1] ;	/* media type */
   char media_location[MEDIA_LOCATION_STRLEN+1];/* media type */

   char data_direction[DATA_DIRECTION_STRLEN+1];/* data direction */

   Time_Record center_time ;			/* center time */
   Time_Record start_time ;			/* start time */
   Time_Record end_time ;			/* end time */
   char recorder_id[RECORDER_ID_STRLEN+1] ;	/* recorder id */
   char station_id[STATION_ID_STRLEN+1] ;	/* station id */

   char insert_top_flag[LOGICAL_STRLEN+1] ;	/* yes,no*/
   char comment[MAXLINE];

   struct GHA_Correction gha; 			/* GHA Correction */

   struct Time_Correlation  tc; 		/* Time Correlation */

   struct State_Vector 	sv; 			/* State Vectors */

} IMS_L1PReq_Record ;


typedef struct ims_scan_req
{

   char file_name[MAX_FILENAME_LEN] ;

   int  order_id ;				/* order id */
   int  item_id ;				/* item id */
   int  job_id ;				/* job id */
   char quicklook_flag[LOGICAL_STRLEN+1];	/* yes/no */
   char order_type[ORDER_TYPE_STRLEN+1] ;	/* order type */
   char job_state[ORDER_STATUS_STRLEN+1];	/* order state */
   char priority[PRIORITY_STRLEN+1] ;		/* priority */

   char platform[PLATFORM_STRLEN+1] ;           /* platform E1,E2,J1,R1 */
   char sensor[SENSOR_STRLEN+1] ;         	/* sensor S,D,O,V */
   int  rev ;					/* rev */
   char mode[MODE_STRLEN+1] ;            	/* mode S1..S7,SS1..SS4,A */

   int  datatake_seq ;				/* sequence */
   char activity_id[ACTIVITY_ID_STRLEN+1] ;    	/* activity id */
   char frame_mode[FRAME_MODE_STRLEN+1] ; 	/* ARCTIC,ANTARCTIC */
   char site_name[SITE_NAME_STRLEN+1] ; 	/* site name */

   char media_id[MEDIA_ID_STRLEN+1] ;		/* media id */
   char media_type[MEDIA_TYPE_STRLEN+1] ;	/* media type */
   char media_location[MEDIA_LOCATION_STRLEN+1];/* media type */

   int  start_address ;				/* start address */
   int  end_address ;				/* end address */
   Time_Record start_time ;			/* start time */
   Time_Record end_time ;			/* end time */
   char recorder_id[RECORDER_ID_STRLEN+1] ;	/* recorder id */
   char station_id[STATION_ID_STRLEN+1] ;	/* station id */
   char data_direction[DATA_DIRECTION_STRLEN+1];/* data direction */

   char insert_top_flag[INSERT_TOP_FLAG_STRLEN+1] ;/* insert top flag (yes,no)*/
   char comment[MAXLINE];

   struct State_Vector 	sv; 			/* State Vectors */

   struct GHA_Correction gha; 			/* GHA Correction */

   struct Time_Correlation  tc; 		/* Time Correlation */

} IMS_ScanReq_Record ;

typedef struct cp_job_req
{

   char request_type[REQUEST_TYPE_STRLEN+1] ;
   char processor_mode[PROCESSOR_MODE_STRLEN+1] ;
 
} CP_JobReq_Record ;

typedef struct cp_job_status
{

   int  job_id;
   char dataset[MAXLINE] ;		/* dataset */
   char platform[MAXLINE] ;		/* patform */
   char sensor[MAXLINE] ;		/* sensor */
   char status[MAXLINE] ;
   char status_type[MAXLINE] ;
   char comment[MAXLINE] ;
   char request_type[MAXLINE] ;
   char product_filename[MAX_FILENAME_LEN] ;
 
} CP_JobStatus_Record ;


typedef struct ims_order_status
{

   char order_id[MAXLINE] ;		/* order id */
   char item_id[MAXLINE] ;		/* item id */
   char status_type[MAXLINE] ;		/* status type */
   char status_id[MAXLINE] ;		/* status id */
   char comment[MAXLINE] ;		/* comment */
   char platform[MAXLINE] ;		/* patform */
   char sensor[MAXLINE] ;		/* sensor */
   char dataset[MAXLINE] ;		/* dataset */
   char product_filename[MAXLINE] ;	/* product file */

} IMS_Order_Status;

#endif _PPSDEFS_
