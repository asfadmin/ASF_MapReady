/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	bind_dbrec.c

Description:
	This module contains the routines used for binding dbrec retrieved
	from database queries to local data structures.

External Functions:
	bind_dbrec_to_l1
	bind_dbrec_to_scan
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)bind_dbrec.c	1.2    12/16/96";

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "db_scan_order.h"
#include "db_l1_order.h"
#include "db_l1_procparms.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"


/*==============================================================================
Function:	
	int bind_dbrec_to_l1( DB_RECORD **db_rec, IMS_L1PReq_Record *rec)

Description:	
	This function extracts the fields within a db record for l1 order.

Parameters:

Returns:	PPS  error_code

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/
#ifdef __STDC__
void bind_dbrec_to_l1( DB_RECORD **db_rec, IMS_L1PReq_Record *rec)
#else
void bind_dbrec_to_l1( db_rec, rec )
        DB_RECORD          **db_rec ;
	IMS_L1PReq_Record *rec ;
#endif
{
	char	time_str[30];

	rec->job_id   = *(DBINT *) db_rec[L1_PROCPARMS_JOB_ID];

	PPS_STRNCPY(rec->quicklook_flag, 
		(char *) db_rec[L1_PROCPARMS_QUICKLOOK_FLAG], LOGICAL_STRLEN);

	PPS_STRNCPY(rec->platform, (char *) db_rec[L1_PROCPARMS_PLATFORM],
		                                  PLATFORM_STRLEN);

	PPS_STRNCPY(rec->sensor, (char *) db_rec[L1_PROCPARMS_SENSOR],
                                                  SENSOR_STRLEN);

	rec->rev      =  *(DBINT *) db_rec[L1_PROCPARMS_REV];
	rec->datatake_seq =  *(DBINT *) db_rec[L1_PROCPARMS_SEQUENCE];

	PPS_STRNCPY(rec->activity_id, 
	    (char *) db_rec[L1_PROCPARMS_ACTIVITY_ID], ACTIVITY_ID_STRLEN);

        PPS_STRNCPY(rec->frame_mode, 
            (char *) db_rec[L1_PROCPARMS_FRAME_MODE], FRAME_MODE_STRLEN);


	PPS_STRNCPY(rec->media_id, (char *) db_rec[L1_PROCPARMS_MEDIA_ID],
		                                     MEDIA_ID_STRLEN);

	PPS_STRNCPY(rec->media_type, (char *) db_rec[L1_PROCPARMS_MEDIA_TYPE],
		                                     MEDIA_TYPE_STRLEN);

	PPS_STRNCPY(rec->media_location, 
		(char *) db_rec[L1_PROCPARMS_MEDIA_LOCATION],
                                    MEDIA_LOCATION_STRLEN);

	PPS_STRNCPY(rec->site_name, (char *) db_rec[L1_PROCPARMS_SITE_NAME],
		SITE_NAME_STRLEN);

	PPS_STRNCPY(rec->product_type,
                                  (char *) db_rec[L1_PROCPARMS_PRODUCT_TYPE],
                                  PRODUCT_TYPE_STRLEN);

	rec->pixel_spacing     = *(DBREAL *) db_rec[L1_PROCPARMS_PIXEL_SPACING];
	rec->frame_id = *(DBINT *) db_rec[L1_PROCPARMS_FRAME_ID];
	rec->subframe_id   = *(DBINT *) db_rec[L1_PROCPARMS_SUBFRAME_ID];
	PPS_STRNCPY(rec->output_format,
                                  (char *) db_rec[L1_PROCPARMS_OUTPUT_FORMAT],
		                  OUTPUT_FMT_STRLEN);
	PPS_STRNCPY(rec->projection, (char *) db_rec[L1_PROCPARMS_PROJECTION],
		PROJECTION_STRLEN);

	PPS_STRNCPY(rec->mode, (char *) db_rec[L1_PROCPARMS_MODE],
		MODE_STRLEN);

	PPS_STRNCPY(time_str, (char *) db_rec[L1_PROCPARMS_GHA_TIME],
		sizeof(time_str) - 1);
	convert_date_string( time_str, &rec->gha.time);
	rec->gha.angle     = *(DBREAL *) db_rec[L1_PROCPARMS_GHA_ANGLE];

	rec->sv.rev        =  *(DBINT *) db_rec[L1_PROCPARMS_SV_REV];
	PPS_STRNCPY(rec->sv.precision, (char *) db_rec[L1_PROCPARMS_SV_TYPE],
		SVEC_TYPE_STRLEN);

	PPS_STRNCPY(rec->sv.coord_sys,
                                 (char *) db_rec[L1_PROCPARMS_SV_COORD_SYS],
                                 COORD_SYS_STRLEN);
	PPS_STRNCPY(rec->sv.platform, (char *) db_rec[L1_PROCPARMS_PLATFORM],
		                 PLATFORM_STRLEN);

	PPS_STRNCPY(time_str, (char *) db_rec[L1_PROCPARMS_SV_TIME],
		                 sizeof(time_str) - 1);
	convert_date_string( time_str, &rec->sv.time);

	rec->sv.x_pos =  *(DBREAL *) db_rec[L1_PROCPARMS_SV_X_POS];
	rec->sv.y_pos =  *(DBREAL *) db_rec[L1_PROCPARMS_SV_Y_POS];
	rec->sv.z_pos =  *(DBREAL *) db_rec[L1_PROCPARMS_SV_Z_POS];
	rec->sv.x_vel =  *(DBREAL *) db_rec[L1_PROCPARMS_SV_X_VEL];
	rec->sv.y_vel =  *(DBREAL *) db_rec[L1_PROCPARMS_SV_Y_VEL];
	rec->sv.z_vel =  *(DBREAL *) db_rec[L1_PROCPARMS_SV_Z_VEL];

	PPS_STRNCPY(time_str, (char *) db_rec[L1_PROCPARMS_TCE_TIME],
		sizeof(time_str) - 1);
	convert_date_string( time_str, &rec->tc.time);

	rec->tc.rev         =  *(DBINT *) db_rec[L1_PROCPARMS_TCE_REV];
	rec->tc.sat_time    =  *(DBINT *) db_rec[L1_PROCPARMS_TCE_SAT_TIME];
	rec->tc.clock_cycle =  *(DBINT *) db_rec[L1_PROCPARMS_TCE_CLOCK_CYCLE];

	rec->processing_gain = *(DBINT *) db_rec[L1_PROCPARMS_PROCESSING_GAIN];
	rec->avg_terrain_ht = *(DBREAL *) db_rec[L1_PROCPARMS_AVG_TERRAIN_HT];
	rec->ps_reference_lat = *(DBREAL *)db_rec[L1_PROCPARMS_PS_REFERENCE_LAT];
	rec->ps_reference_lon = *(DBREAL *)db_rec[L1_PROCPARMS_PS_REFERENCE_LON];
	rec->utm_zone =  *(DBINT *) db_rec[L1_PROCPARMS_UTM_ZONE];
	PPS_STRNCPY(rec->deskew, (char *) db_rec[L1_PROCPARMS_DESKEW],
		LOGICAL_STRLEN);
	PPS_STRNCPY(rec->terrain_correction, 
		(char *) db_rec[L1_PROCPARMS_TERRAIN_CORRECTION],
		LOGICAL_STRLEN);

	rec->lambert_latitude_n = *(DBREAL *)db_rec[L1_PROCPARMS_LAMBERT_LATITUDE_N];
	rec->lambert_latitude_s = *(DBREAL *)db_rec[L1_PROCPARMS_LAMBERT_LATITUDE_S];

	PPS_STRNCPY(rec->scan_results_file, 
		(char *) db_rec[L1_PROCPARMS_SCAN_RESULTS_FILE],
		SCAN_RESULTS_FILE_STRLEN);

	PPS_STRNCPY(rec->cal_params_file, 
		(char *) db_rec[L1_PROCPARMS_CAL_PARAMS_FILE],
		CAL_PARAMS_FILE_STRLEN);
        PPS_STRNCPY(rec->cal_params_file2,
                (char *) db_rec[L1_PROCPARMS_CAL_PARAMS_FILE2],
                CAL_PARAMS_FILE_STRLEN);

	PPS_STRNCPY(rec->data_direction,
		(char *) db_rec[L1_PROCPARMS_DATA_DIRECTION],
		DATA_DIRECTION_STRLEN);

        PPS_STRNCPY(rec->compensation_flag, 
		(char *) db_rec[L1_PROCPARMS_COMPENSATION_FLAG],
                LOGICAL_STRLEN);

	rec->comment[0] = '\0';

} /* bind_dbrec_to_l1 */


/*==============================================================================
Function:	
	int bind_dbrec_to_scan( DB_RECORD **db_rec, IMS_ScanReq_Record *rec)

Description:	
	This function extracts the fields within a db record for scan order.

Parameters:

Returns:	PPS  error_code

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/
#ifdef __STDC__
void bind_dbrec_to_scan( DB_RECORD **db_rec, IMS_ScanReq_Record *rec)
#else
void bind_dbrec_to_scan( db_rec, rec )
        DB_RECORD          **db_rec ;
	IMS_ScanReq_Record *rec ;
#endif
{
	char	time_str[30];

	rec->job_id   = *(DBINT *) db_rec[SCAN_ORDER_JOB_ID];
	rec->order_id = *(DBINT *) db_rec[SCAN_ORDER_ORDER_ID];
	rec->item_id  = *(DBINT *) db_rec[SCAN_ORDER_ITEM_ID];

	PPS_STRNCPY(rec->quicklook_flag,
                               (char *)db_rec[SCAN_ORDER_QUICKLOOK_FLAG],
		               LOGICAL_STRLEN);
	PPS_STRNCPY(rec->platform, (char *) db_rec[SCAN_ORDER_PLATFORM],
		               PLATFORM_STRLEN);
	PPS_STRNCPY(rec->sensor, (char *) db_rec[SCAN_ORDER_SENSOR],
		               SENSOR_STRLEN);
	rec->rev      =  *(DBINT *) db_rec[SCAN_ORDER_REV];
	rec->datatake_seq =  *(DBINT *) db_rec[SCAN_ORDER_SEQUENCE];
	PPS_STRNCPY(rec->activity_id, (char *) db_rec[SCAN_ORDER_ACTIVITY_ID],
		               ACTIVITY_ID_STRLEN);

	PPS_STRNCPY(rec->media_id, (char *) db_rec[SCAN_ORDER_MEDIA_ID],
		               MEDIA_ID_STRLEN);
	PPS_STRNCPY(rec->media_type, (char *) db_rec[SCAN_ORDER_MEDIA_TYPE],
                               MEDIA_TYPE_STRLEN);
	PPS_STRNCPY(rec->media_location,
                               (char *) db_rec[SCAN_ORDER_MEDIA_LOCATION],
                               MEDIA_LOCATION_STRLEN);

	rec->start_address = *(DBINT *) db_rec[SCAN_ORDER_START_ADDRESS];
	rec->end_address   = *(DBINT *) db_rec[SCAN_ORDER_END_ADDRESS];

	/* convert ascii string to Time_Record */
	PPS_STRNCPY(time_str, (char *) db_rec[SCAN_ORDER_START_TIME],
                              sizeof(time_str)-1);
	convert_date_string( time_str, &rec->start_time);

	PPS_STRNCPY(time_str, (char *) db_rec[SCAN_ORDER_END_TIME],
                              sizeof(time_str)-1);
	convert_date_string( time_str, &rec->end_time);

	PPS_STRNCPY(time_str, (char *) db_rec[SCAN_ORDER_START_TIME],
                              sizeof(time_str)-1);
	PPS_STRNCPY(rec->recorder_id, (char *) db_rec[SCAN_ORDER_RECORDER_ID],
                              RECORDER_ID_STRLEN);
	PPS_STRNCPY(rec->station_id, (char *) db_rec[SCAN_ORDER_STATION_ID],
                              STATION_ID_STRLEN);

	PPS_STRNCPY(time_str, (char *) db_rec[SCAN_ORDER_GHA_TIME],
                              sizeof(time_str)-1);
	convert_date_string( time_str, &rec->gha.time);
	rec->gha.angle     = *(DBREAL *) db_rec[SCAN_ORDER_GHA_ANGLE];

	rec->sv.rev        =  *(DBINT *) db_rec[SCAN_ORDER_SV_REV];
	PPS_STRNCPY(rec->sv.precision, (char *) db_rec[SCAN_ORDER_SV_TYPE],
                              SVEC_TYPE_STRLEN);
	PPS_STRNCPY(rec->sv.coord_sys, (char *) db_rec[SCAN_ORDER_SV_COORD_SYS],
                              COORD_SYS_STRLEN);
	PPS_STRNCPY(rec->sv.platform, (char *) db_rec[SCAN_ORDER_PLATFORM],
                              PLATFORM_STRLEN);

	PPS_STRNCPY(time_str, (char *) db_rec[SCAN_ORDER_SV_TIME],
                              sizeof(time_str)-1);
	convert_date_string( time_str, &rec->sv.time);
	rec->sv.x_pos =  *(DBREAL *) db_rec[SCAN_ORDER_SV_X_POS];
	rec->sv.y_pos =  *(DBREAL *) db_rec[SCAN_ORDER_SV_Y_POS];
	rec->sv.z_pos =  *(DBREAL *) db_rec[SCAN_ORDER_SV_Z_POS];
	rec->sv.x_vel =  *(DBREAL *) db_rec[SCAN_ORDER_SV_X_VEL];
	rec->sv.y_vel =  *(DBREAL *) db_rec[SCAN_ORDER_SV_Y_VEL];
	rec->sv.z_vel =  *(DBREAL *) db_rec[SCAN_ORDER_SV_Z_VEL];

	PPS_STRNCPY(time_str, (char *) db_rec[SCAN_ORDER_TCE_TIME],
                              sizeof(time_str)-1);
	convert_date_string( time_str, &rec->tc.time);
	rec->tc.rev         =  *(DBINT *) db_rec[SCAN_ORDER_TCE_REV];
	rec->tc.sat_time    =  *(DBINT *) db_rec[SCAN_ORDER_TCE_SAT_TIME];
	rec->tc.clock_cycle =  *(DBINT *) db_rec[SCAN_ORDER_TCE_CLOCK_CYCLE];

	PPS_STRNCPY(rec->frame_mode, (char *) db_rec[SCAN_ORDER_FRAME_MODE],
                              FRAME_MODE_STRLEN);
	PPS_STRNCPY(rec->mode, (char *) db_rec[SCAN_ORDER_MODE],
                              MODE_STRLEN);
	PPS_STRNCPY(rec->site_name, (char *) db_rec[SCAN_ORDER_SITE_NAME],
                              SITE_NAME_STRLEN);
        PPS_STRNCPY(rec->data_direction,
                (char *) db_rec[SCAN_ORDER_DATA_DIRECTION],
                DATA_DIRECTION_STRLEN);


	rec->comment[0] = '\0';

} /* bind_dbrec_to_scan */



/* End of File */

