/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	pps_db_table.c

Description:	
	Global variables needed for PPS database access

External Functions:
	None
	
Static Functions:
	None
	
External Variables Defined:
	scan_order_columns_
	policy_columns_
	l1_order_columns_
	l1_procparms_columns_
	jobs_columns_
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)pps_db_table.c	1.2    12/16/96";


#include "db_sybint.h"
#include "PPShdr.h"

#include "db_jobs.h"
COLUMN_DEFS jobs_columns_[NUM_JOBS_COLS+1] =
{
	{"jobs.job_state",          ORDER_STATUS_STRLEN+1,  STRINGBIND, "%15s"},
	{"jobs.priority", 		    PRIORITY_STRLEN+1,      STRINGBIND, "%10s"},
	{"jobs.insert_top_flag",	INSERT_TOP_FLAG_STRLEN+1, STRINGBIND, "%3s"},
	{"jobs.job_id",     		sizeof(DBINT),  INTBIND,    "%d"},
	{NULL, 0, 0, NULL}
} ;

#include "db_policy.h"
COLUMN_DEFS policy_columns_[NUM_POLICY_COLS+1] =
{
	{"policy.job_type",   		REQUEST_TYPE_STRLEN+1,  STRINGBIND, "%10s"},
	{"policy.job_priority",   	PRIORITY_STRLEN+1,      STRINGBIND, "%10s"},
	{"policy.auto_flag",   		LOGICAL_STRLEN+1,       STRINGBIND, "%10s"},
	{NULL, 0, 0, NULL}
} ;

#include "db_l1_order.h"
COLUMN_DEFS l1_order_columns_[NUM_L1_ORDER_COLS+1] =
{
	{"L1_orders.job_id",     	sizeof(DBINT),  INTBIND,    "%d"},
	{"L1_orders.order_id",     	sizeof(DBINT),  INTBIND,    "%d"},
	{"L1_orders.item_id",     	sizeof(DBINT),  INTBIND,    "%d"},
	{NULL, 0, 0, NULL}
} ;

#include "db_scan_order.h"
COLUMN_DEFS scan_order_columns_[NUM_SCAN_ORDER_COLS+1] =
{
	{"scan_orders.job_id",     	sizeof(DBINT),  INTBIND,    "%d"},
	{"scan_orders.order_id",   	sizeof(DBINT),  INTBIND,    "%d"},
	{"scan_orders.item_id",    	sizeof(DBINT),  INTBIND,    "%d"},

	{"scan_orders.quicklook_flag", 	LOGICAL_STRLEN+1, STRINGBIND, "%3s"},
	{"scan_orders.order_type", 	ORDER_TYPE_STRLEN+1,  STRINGBIND, "%4s"},
	{"scan_orders.platform", 	PLATFORM_STRLEN+1,    STRINGBIND, "%2s"},
	{"scan_orders.sensor", 		SENSOR_STRLEN+1,      STRINGBIND, "%1s"},
	{"scan_orders.rev", 		sizeof(DBINT),        INTBIND,    "%d"},
	{"scan_orders.mode", 		MODE_STRLEN+1,        STRINGBIND, "%3s"},
	{"scan_orders.sequence",	sizeof(DBINT),        INTBIND,    "%d"},
	{"scan_orders.activity_id", ACTIVITY_ID_STRLEN+1, STRINGBIND, "%3s"},
	{"scan_orders.frame_mode", 	FRAME_MODE_STRLEN+1,  STRINGBIND, "%10s"},
	{"scan_orders.site_name", 	SITE_NAME_STRLEN+1,   STRINGBIND, "%128s"},

	{"scan_orders.media_type", 	MEDIA_TYPE_STRLEN+1,  STRINGBIND, "%5s"},
	{"scan_orders.start_address",	sizeof(DBINT),    INTBIND,    "%d"},
	{"scan_orders.end_address",	sizeof(DBINT),        INTBIND,    "%d"},
	{"scan_orders.start_time", 	TIME_STRLEN+1,        STRINGBIND, "%21s"},
	{"scan_orders.end_time", 	TIME_STRLEN+1,        STRINGBIND, "%21s"},
	{"scan_orders.recorder_id", RECORDER_ID_STRLEN+1, STRINGBIND, "%16s"},
	{"scan_orders.station_id", 	STATION_ID_STRLEN+1,  STRINGBIND, "%3s"},

	{"scan_orders.gha_time", 	TIME_STRLEN+1,        STRINGBIND, "%21s"},
	{"scan_orders.gha_angle", 	sizeof(DBREAL),       REALBIND,   "%f"},

	{"scan_orders.sv_type", 	SVEC_TYPE_STRLEN+1,   STRINGBIND, "%10s"},
	{"scan_orders.sv_coord_sys",COORD_SYS_STRLEN+1,   STRINGBIND, "%15s"},
	{"scan_orders.sv_rev",		sizeof(DBINT),        INTBIND,    "%d"},
	{"scan_orders.sv_time", 	TIME_STRLEN+1,        STRINGBIND, "%21s"},
	{"scan_orders.sv_x_pos", 	sizeof(DBREAL),       REALBIND  , "%f"},
	{"scan_orders.sv_y_pos", 	sizeof(DBREAL),       REALBIND  , "%f"},
	{"scan_orders.sv_z_pos", 	sizeof(DBREAL),       REALBIND  , "%f"},
	{"scan_orders.sv_x_velocity", 	sizeof(DBREAL),   REALBIND  , "%f"},
	{"scan_orders.sv_y_velocity", 	sizeof(DBREAL),   REALBIND  , "%f"},
	{"scan_orders.sv_z_velocity", 	sizeof(DBREAL),   REALBIND  , "%f"},

	{"scan_orders.tce_rev", 	sizeof(DBINT),        INTBIND,    "%d"},
	{"scan_orders.tce_time", 	TIME_STRLEN+1,        STRINGBIND, "%21s"},
	{"scan_orders.tce_sat_time", 	sizeof(DBINT),    INTBIND,    "%d"},
	{"scan_orders.tce_clock_cycle",	sizeof(DBINT),    INTBIND,    "%d"},

	{"scan_orders.media_location", MEDIA_LOCATION_STRLEN+1, STRINGBIND, "%128s"},
	{"scan_orders.media_id", 	MEDIA_ID_STRLEN+1,      STRINGBIND, "%12s"},
	{"scan_orders.data_direction", DATA_DIRECTION_STRLEN+1, STRINGBIND, "%7s"},

	{NULL, 0, 0, NULL}
} ;

#include "db_l1_procparms.h"
COLUMN_DEFS l1_procparms_columns_[NUM_L1_PROCPARMS_COLS+1] =
{
	{"L1_procparms.job_id", 	    sizeof(DBINT),          INTBIND,    "%d"},

	{"L1_procparms.quicklook_flag",	LOGICAL_STRLEN+1,		STRINGBIND, "%3s"},
	{"L1_procparms.platform", 	    PLATFORM_STRLEN+1,		STRINGBIND, "%2s"},
	{"L1_procparms.sensor", 	    SENSOR_STRLEN+1,		STRINGBIND, "%3s"},
	{"L1_procparms.rev", 		    sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.mode", 		    MODE_STRLEN+1,     	    STRINGBIND, "%3s"},
	{"L1_procparms.sequence",	    sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.activity_id", 	ACTIVITY_ID_STRLEN+1,   STRINGBIND, "%3s"},
	{"L1_procparms.frame_mode", 	FRAME_MODE_STRLEN+1,    STRINGBIND, "%10s"},
	{"L1_procparms.site_name", 	    SITE_NAME_STRLEN+1,    	STRINGBIND, "%128s"},

	{"L1_procparms.media_type", 	MEDIA_TYPE_STRLEN+1,    STRINGBIND, "%5s"},
	{"L1_procparms.product_type", 	PRODUCT_TYPE_STRLEN+1,  STRINGBIND, "%13s"},
	{"L1_procparms.pixel_spacing", 	sizeof(DBREAL),      	REALBIND,   "%f"},
	{"L1_procparms.frame_id",       sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.subframe_id",	sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.output_format", 	OUTPUT_FMT_STRLEN+1,	STRINGBIND, "%4s"},
	{"L1_procparms.projection", 	PROJECTION_STRLEN+1,	STRINGBIND, "%12s"},

	{"L1_procparms.gha_time", 	    TIME_STRLEN+1,     	    STRINGBIND, "%21s"},
	{"L1_procparms.gha_angle", 	    sizeof(DBREAL),      	REALBIND,   "%f"},

	{"L1_procparms.sv_type", 	    SVEC_TYPE_STRLEN+1,     STRINGBIND, "%10s"},
	{"L1_procparms.sv_coord_sys", 	COORD_SYS_STRLEN+1,    	STRINGBIND, "%15s"},
	{"L1_procparms.sv_rev",		    sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.sv_time", 	    TIME_STRLEN+1,          STRINGBIND, "%21s"},
	{"L1_procparms.sv_x_pos", 		sizeof(DBREAL),         REALBIND  , "%f"},
	{"L1_procparms.sv_y_pos", 		sizeof(DBREAL),         REALBIND  , "%f"},
	{"L1_procparms.sv_z_pos", 		sizeof(DBREAL),         REALBIND  , "%f"},
	{"L1_procparms.sv_x_velocity", 	sizeof(DBREAL),         REALBIND  , "%f"},
	{"L1_procparms.sv_y_velocity", 	sizeof(DBREAL),         REALBIND  , "%f"},
	{"L1_procparms.sv_z_velocity", 	sizeof(DBREAL),         REALBIND  , "%f"},

	{"L1_procparms.tce_rev", 	    sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.tce_time", 	    TIME_STRLEN+1,          STRINGBIND, "%21s"},
	{"L1_procparms.tce_sat_time", 	sizeof(DBINT),          INTBIND,    "%d"},
	{"L1_procparms.tce_clock_cycle",sizeof(DBINT),          INTBIND,    "%d"},

	{"L1_procparms.processing_gain", sizeof(DBINT),         INTBIND,    "%d"},
	{"L1_procparms.avg_terrain_ht",	 sizeof(DBREAL),		REALBIND  , "%f"},
	{"L1_procparms.ps_reference_lat",sizeof(DBREAL),		REALBIND  , "%f"},
	{"L1_procparms.ps_reference_lon",sizeof(DBREAL),		REALBIND  , "%f"},
	{"L1_procparms.utm_zone",		sizeof(DBINT), 			INTBIND,    "%d"},
	{"L1_procparms.deskew", 	     LOGICAL_STRLEN+1,      STRINGBIND, "%3s"},
	{"L1_procparms.terrain_correction",LOGICAL_STRLEN+1,    STRINGBIND, "%3s"},
	{"L1_procparms.lambert_latitude_n",sizeof(DBREAL),		REALBIND  , "%f"},
	{"L1_procparms.lambert_latitude_s",sizeof(DBREAL),		REALBIND  , "%f"},

	{"L1_procparms.scan_results_file", SCAN_RESULTS_FILE_STRLEN+1,STRINGBIND, "%255s"},
	{"L1_procparms.cal_params_file", CAL_PARAMS_FILE_STRLEN+1,STRINGBIND, "%40s"},
        {"L1_procparms.cal_params_file2", CAL_PARAMS_FILE_STRLEN+1,STRINGBIND, "%40s"},
	{"L1_procparms.media_location",	MEDIA_LOCATION_STRLEN+1,STRINGBIND, "%128s"},
	{"L1_procparms.media_id", 	    MEDIA_ID_STRLEN+1,      STRINGBIND, "%12s"},
        {"L1_procparms.data_direction", DATA_DIRECTION_STRLEN+1, STRINGBIND, "%7s"},
        {"L1_procparms.compensation_flag", LOGICAL_STRLEN+1, STRINGBIND, "%3s"},
	{NULL, 0, 0, NULL}
} ;

/* End of File */
