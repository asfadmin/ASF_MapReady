static char SccsFileId[] = "@(#)pps_check_params.c	1.10    10/31/97";

#include "pps_util.h"
#include "pps_db.h"
#include "pps_check_params.h"
#include "defs.h"
#include "PPSerr.h"
#include "PPSdefs.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

extern char	ProgName[];

extern IMS_CMN_QUERY   *ims_query;
extern int             ims_connected;

extern CS_CONNECTION *query_connection;
extern CS_CONNECTION *exec_connection;

/* structure for sybase communication */ 
static struct  pps_db_exec_dcl pps_query;

/* structure for storing data retrieved from IMS -- this structure is
   used for both L1 and scan jobs */
typedef struct Params_Rec
{
        struct GHA_Correction   GHA_data;
        struct Time_Correlation TCE_data;
        struct State_Vector     SV_data;
        char                    cal_params_file[CAL_PARAMS_FILE_STRLEN+1];
	char                    cal_params_file2[CAL_PARAMS_FILE_STRLEN+1];
        char                    scan_results_file[SCAN_RESULTS_FILE_STRLEN+1];
        struct Params_Avail     avail_indicators;
} Params_Rec;

/* structure used to send order status to IMS -- need to be global
   because they are used in database callback routines */
IMS_Order_Status 	ims_status;
int			Order_Id;
int			Item_Id;

/****************************************************************************
 * "init_query_data" initializes the structure used for storing data
 * returned from sybase
 ***************************************************************************/
void init_query_data(Job_Rec	*query_data)
{
        *query_data->start_time = '\0';
        *query_data->end_time = '\0';
        *query_data->center_time = '\0';
        *query_data->platform = '\0';
        *query_data->sensor = '\0';
        *query_data->mode = '\0';
        query_data->rev = 0;
	query_data->sequence = 0;
	*query_data->media_id = '\0';
        *query_data->quicklook_flag = '\0';
	*query_data->job_comment = '\0';
        *query_data->auto_schedulable = '\0';
}

/*****************************************************************************
 * routine to read the required data from PPS server database tables
 * to perform IMS queries -- this routine returns ER_NO_ERROR if data is
 * successfully retrieved, and returns ER_DB_ACCESS when there is an error
 * in accessing the database. 
 *****************************************************************************/
int get_pending_job_data
	(int            job_id,
	 char           *job_type,
	 Job_Rec 	*query_data)
{
        int		retcode;
        char            cmdbuf[MAXBIGBUF];

	pps_query.num_items = 0;
        pps_query.callback = 0;

	/* initialize the structure used for storing data returned from sybase */
	init_query_data(query_data);
	
	/* set up the bindings for retrieving data */
	pps_db_bind_char(&pps_query, query_data->start_time, TIME_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->end_time, TIME_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->center_time, TIME_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->platform, PLATFORM_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->sensor, SENSOR_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->mode, MODE_STRLEN+1);
	pps_db_bind_int(&pps_query, &query_data->rev);
	pps_db_bind_int(&pps_query, &query_data->sequence);
	pps_db_bind_char(&pps_query, query_data->media_id, MEDIA_ID_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->quicklook_flag, LOGICAL_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->job_comment, COMMENT_STRLEN+1);
	pps_db_bind_char(&pps_query, query_data->auto_schedulable, LOGICAL_STRLEN+ 1);
	
	/* make the actual sql command */
        sprintf(cmdbuf, "exec sp_get_data_for_ims_queries %d, \"%s\"",
                        job_id, job_type);


	/* execute the sql command */
	retcode = db_exec(&query_connection, cmdbuf, &pps_query);

	if (retcode == ER_NO_ERROR)
	{
	        /* set appropriate flags to tell the caller 
       	         * when rev and sequence are null  */
	        query_data->rev_is_null = (int) pps_query.item[5].indicator;
	        query_data->sequence_is_null = (int) pps_query.item[6].indicator;
		return (ER_NO_ERROR);
	}
	else
		return (ER_DB_ACCESS);
}

/*****************************************************************************
 * routine to update job params data in the PPS database with data retrieved
 * from IMS.  This routine also updates the job state if the job is now
 * "ready".  
*****************************************************************************/

int update_job_params (int        job_id,
 		      char       *job_type,
		      char       *job_state,	
		      char	 *previous_job_state,
	              Params_Rec JobParams,
		      IMS_FRAME_SOURCE_MEDIA_STRUCT frame_info)
{
        int		retcode;
        char            cmdbuf[MAXBIGBUF];
	char		buf[MAXBIGBUF];
	char		tablename[MAXSMALLBUF];	
        struct          pps_db_exec_dcl pps_exec;

	if (strcmp(job_type,SCAN_JOB_TYPE) == 0)
		strcpy(tablename,SCAN_ORDERS_TABLENAME);
	else
		strcpy(tablename,L1_PROC_PARMS_TABLENAME);

	sprintf(cmdbuf, "update %s set ", tablename);

	/**** update GHA info based on availability *****/
	if (strcmp(JobParams.avail_indicators.GHA_avail,"YES") == 0)
		sprintf(buf, "gha_time=\"%s\",gha_angle=%f",
			JobParams.GHA_data.time.time_string, 
			JobParams.GHA_data.angle);

	else
		strcpy(buf, "gha_time=NULL, gha_angle = NULL");
	
	strcat(cmdbuf, buf);

	/******* update TCE info based on availability *****/
	if (strcmp(JobParams.avail_indicators.TCE_avail,"YES") == 0)
	{
		sprintf(buf,",tce_rev=%d,tce_sat_time=%d,tce_clock_cycle=%d,tce_time =\"%s\"",
		JobParams.TCE_data.rev, JobParams.TCE_data.sat_time, 
		JobParams.TCE_data.clock_cycle,JobParams.TCE_data.time.time_string);

		strcat(cmdbuf, buf);
        } 
        else if (strcmp(JobParams.avail_indicators.TCE_avail,"NO") == 0)
	{
		strcpy(buf,",tce_rev=NULL,tce_sat_time=NULL,tce_clock_cycle=NULL,tce_time =NULL"); 
        	strcat(cmdbuf, buf);
	}

	/******* update state vector info based on availability *****/
	if (strcmp(JobParams.avail_indicators.SV_avail,"YES") == 0)
		sprintf(buf,",sv_type=\"%s\",sv_rev=%d,sv_time=\"%s\",sv_x_pos=%f,"
		"sv_y_pos=%f,sv_z_pos=%f,sv_x_velocity=%f,sv_y_velocity=%f,"
		"sv_z_velocity=%f,sv_coord_sys=\"%s\"",
                JobParams.SV_data.precision, JobParams.SV_data.rev, 
		JobParams.SV_data.time.time_string, JobParams.SV_data.x_pos, 
		JobParams.SV_data.y_pos, JobParams.SV_data.z_pos, 
		JobParams.SV_data.x_vel, JobParams.SV_data.y_vel, 
		JobParams.SV_data.z_vel, JobParams.SV_data.coord_sys);
        else
		 strcpy(buf,",sv_type=NULL,sv_rev=NULL,sv_time=NULL,"
		 "sv_x_pos=NULL,sv_y_pos=NULL,sv_z_pos=NULL,sv_x_velocity=NULL,"
		 "sv_y_velocity=NULL,sv_z_velocity=NULL,sv_coord_sys=NULL"); 
        
	strcat(cmdbuf, buf);

        /******* update calibration params file info based on availability 
		 for L1 job only *****/
	if (strcmp(job_type,L1_JOB_TYPE) == 0)
	{
        	if (strcmp(JobParams.avail_indicators.Params_File_avail,"YES") == 0)
			sprintf(buf,",cal_params_file = \"%s\",cal_params_file2 = \"%s\"",
				JobParams.cal_params_file,
				JobParams.cal_params_file2);
		else
			strcpy(buf,",cal_params_file = '',cal_params_file2=''");

		strcat(cmdbuf, buf);
	}

        /******* update scan results file info based on availability 
                 for L1 job only *****/
        if (strcmp(job_type,L1_JOB_TYPE) == 0)
        {
                if (strcmp(JobParams.avail_indicators.Scan_File_avail,"YES") == 0)
                        sprintf(buf,",scan_results_file = \"%s\"",
                                JobParams.scan_results_file);
                else
                        strcpy(buf,",scan_results_file= ''");
		strcat(cmdbuf, buf);
        }

	/******* update media info based on availability for L1 job only ****/
	if (strcmp(job_type,L1_JOB_TYPE) == 0)
	{
		if (strcmp(frame_info.frame_status,"SCANNED") == 0)
		{
			sprintf(buf,",media_id = \"%s\", start_time = \"%s\","
				"end_time = \"%s\",center_time = \"%s\","
				"data_direction = \"%s\",media_location = \"%s\","
				"media_type = \"%s\", recorder_id = \"%s\","
				"station_id = \"%s\"",
				frame_info.media_id, frame_info.start_time,
				frame_info.end_time,frame_info.center_time,
				frame_info.data_direction, frame_info.media_location,
				frame_info.media_type, frame_info.recorder_id,
				frame_info.station_id);
			strcat(cmdbuf, buf);
		}
		else if (frame_info.frame_status[0] != '\0')
		{
			strcpy(buf,",media_id=NULL, start_time=NULL,"
				   "end_time=NULL, center_time=NULL");
			strcat(cmdbuf, buf);
		}
	}

	sprintf(buf, " where job_id = %d ", job_id);
	strcat (cmdbuf, buf);

	if (strcmp(job_type,L1_JOB_TYPE) == 0)
	{
		if (strcmp(frame_info.frame_status,"SCANNED") == 0)
		{
			sprintf(buf, "update ppsgui_orders set media_id = \"%s\","
				     "station_id = \"%s\",media_type = \"%s\","
				     "media_location = \"%s\",data_direction = \"%s\" "
				     "where job_id = %d ",
				frame_info.media_id, frame_info.station_id,
				frame_info.media_type, frame_info.media_location,
				frame_info.data_direction, job_id);
			strcat(cmdbuf, buf);
		}
		else if (frame_info.frame_status[0] != '\0')
		{
			sprintf(buf, "update ppsgui_orders set media_id = null "
				     "where job_id = %d ", job_id);
			strcat(cmdbuf, buf);
		}
	}

	/* make job available if the policy table says so */
        if (strcmp(job_state,AVAILABLE) == 0)
        {
                sprintf(buf, " exec sp_auto_avail %d", job_id);
                strcat (cmdbuf, buf);
        }

	/* update the job_state if job now is changed to a different state */
	else if (strcmp(job_state,previous_job_state) != 0)
	{
		sprintf(buf, " exec sp_update_job_state %d, '%s'",
			job_id, job_state); 
		strcat (cmdbuf, buf);
	}
printf(cmdbuf);

	/* execute the update sql statement */
        retcode = db_exec(&exec_connection, cmdbuf, &pps_exec);

	if (retcode != ER_NO_ERROR)
		return (ER_DB_ACCESS);

	else
		return (ER_NO_ERROR);
 
}


/*****************************************************************************
 * The "send_status" routine creates the ODL buffer which contains the order
 * status and sends the order status to IMS
 ****************************************************************************/
void send_status()
{
	char		buf[MAXBIGBUF];
	int		ret_code;

        /* create buffer containing order status */
        create_order_status_buf( &ims_status, buf, sizeof(buf));
#ifdef DEBUG
	printf("order status = %s\n", buf);
#endif

	/* send order status to IMS */
	ret_code = wrap_ims_orderStatus (&ims_query, buf);

	if (ret_code == ER_NO_ERROR)
	{
		sprintf(buf,
			"order[%s], item[%s],"
            " new state=[%s] successfully sent to IMS",
			ims_status.order_id, ims_status.item_id,
			ims_status.status_id);
		pps_logMsg(ProgName, PPS_INFO, buf);

	}
	else
	{
		if (ret_code == ER_IMS_ORDER_NOT_FOUND)
		{
			sprintf(buf, "IMS failed to update status for order id [%s],"
						 " item id [%s], new state [%s] because order "
                         "does not exist found in IMS database\n",
				ims_status.order_id, ims_status.item_id,
			    ims_status.status_id); 	
			pps_logMsg(ProgName, PPS_ERROR, buf);
		}
		else
		{
			sprintf(buf, "Unable to send IMS order status for"
                        " order id [%s], item id [%s], status [%s]\n",
				ims_status.order_id, ims_status.item_id, 
				ims_status.status_id);         
			pps_logMsg(ProgName, PPS_ERROR, buf);
		}
	}
} /* send_status */

/*****************************************************************************
 * "send_scan_status" is called as a callback from db_exec get_rows each time
 * a result row is returned from sybase for SCAN order_id and item_id
 ****************************************************************************/
void send_scan_status()
{
        IMS_ScanReq_Record      rec;

        strcpy(rec.platform, ims_status.platform);
        strcpy(rec.sensor, ims_status.sensor);
	strcpy(rec.comment, ims_status.comment);
	rec.order_id =  Order_Id;
	rec.item_id =  Item_Id;
 
        /* create order status to be sent to IMS */
        fill_scanorder_status (&rec, &ims_status,
                INTERMEDIATE, ims_status.status_id, NULL, NULL);
 
        send_status();
 
}

/*****************************************************************************
 * "send_L1_status" is called as a callback from do_query get_rows each time
 * a result row is returned from sybase for L1 order_id and item_id
 ****************************************************************************/ 
void send_L1_status()
{
	IMS_L1PReq_Record	rec;

	strcpy(rec.platform, ims_status.platform);
	strcpy(rec.sensor, ims_status.sensor);
	strcpy(rec.comment, ims_status.comment);
        rec.order_id = Order_Id;
        rec.item_id  = Item_Id;
 
	/* create order status to be sent to IMS */
	fill_L1order_status (&rec, &ims_status,
		INTERMEDIATE, ims_status.status_id, NULL, NULL);

	send_status();
}

/*****************************************************************************
 * The "send_IMS_order_status" sends to IMS the status of the order 
 * corresponding to the specified job_id.  Multiple status messages are
 * sent for collapsed L1 orders.   
 *****************************************************************************/
void send_IMS_order_status 
		(int       job_id,
		 char      *job_type,
		 char	   *job_state,
  		 Job_Rec   query_data)
{
        int		retcode;
        char            cmdbuf[MAXSMALLBUF];

	/* fill the order status with info already available */
	strcpy(ims_status.sensor, query_data.sensor);
	strcpy(ims_status.platform, query_data.platform);
	strcpy(ims_status.status_id, job_state);
	strcpy(ims_status.comment, query_data.job_comment);

        /* populate pps_query structure  */
        pps_query.num_items = 0;
	if (strcmp(job_type,L1_JOB_TYPE) == 0)
	        pps_query.callback = send_L1_status;
	else
		pps_query.callback = send_scan_status;
 
        /* set up the bindings for retrieving data */
        pps_db_bind_int(&pps_query, &Order_Id);
        pps_db_bind_int(&pps_query, &Item_Id);

        /* make the actual sql command */
        sprintf(cmdbuf, "exec sp_get_order_id %d, \"%s\"",
                        job_id, job_type);
 
        /* execute the sql command */
        retcode = db_exec(&query_connection, cmdbuf, &pps_query);

	if (retcode != ER_NO_ERROR)
		fprintf(stderr,"Unable to get order/item id to send "
			"IMS order state for %s job id %d", job_type, job_id);


}

/************************** testing code only ********************************/
#ifdef PUSH_JOB_READY
void perform_ims_queries (Params_Rec *JobParams,
	         	Params_Avail *avail_indicators )
{
        strcpy(JobParams->avail_indicators.GHA_avail,"YES");
        strcpy(JobParams->GHA_data.time.time_string,"1996-154T16:00:00.000");
        JobParams->GHA_data.angle = 22.9;
 
        strcpy(JobParams->avail_indicators.TCE_avail,"YES");
        JobParams->TCE_data.rev = 22;
        JobParams->TCE_data.sat_time = 22;
        JobParams->TCE_data.clock_cycle = 2;
        strcpy(JobParams->TCE_data.time.time_string,"1996-154T16:00:00.000");
 
        strcpy(JobParams->avail_indicators.SV_avail,"YES");
        strcpy(JobParams->SV_data.precision,"RESTITUTED");
        JobParams->SV_data.rev = 12;
        strcpy(JobParams->SV_data.time.time_string,"1996-154T06:00:00.000");
        JobParams->SV_data.x_pos = 12.0;
        JobParams->SV_data.y_pos = 12.1;
        JobParams->SV_data.z_pos = 12.2;
        JobParams->SV_data.x_vel = 12.3;
        JobParams->SV_data.y_vel = 12.4;
        JobParams->SV_data.z_vel = 12.5;
        strcpy(JobParams->SV_data.coord_sys,"TRUE_EQUATORIAL");
 
        strcpy(JobParams->avail_indicators.Params_File_avail,"YES");
        strcpy(JobParams->cal_params_file,"prim_params_file");
	strcpy(JobParams->cal_params_file2,"sec_params_file");
 
        strcpy(JobParams->avail_indicators.Scan_File_avail,"YES");
        strcpy(JobParams->scan_results_file,"scan_results_file");

	strcpy(avail_indicators->GHA_avail, JobParams->avail_indicators.GHA_avail);
	strcpy(avail_indicators->TCE_avail, JobParams->avail_indicators.TCE_avail); 
	strcpy(avail_indicators->SV_avail, JobParams->avail_indicators.SV_avail);
	
	strcpy(avail_indicators->Scan_File_avail, 
  	       JobParams->avail_indicators.Scan_File_avail);
	strcpy(avail_indicators->Params_File_avail, 
	       JobParams->avail_indicators.Params_File_avail);
}
#endif

/*****************************************************************************
 * The "check_params" routine is called when the user issues a request to
 * Check Params for a pending or ready job.  The caller of this routine should 
 * ensure that the selected job is in pending or ready state before calling it.  
 * This routine queries IMS for TCE, GHA, SV, scan results file (for L1 only), 
 * calibration params file (for L1 only).
 *****************************************************************************/
check_params (int		  order_id,
	      int		  item_id,
	      int                 job_id,               
              char                *job_type,		 
	      char 		  *previous_job_state,
	      Params_Avail 	  *avail_indicators)  /* output */
{
	Job_Rec		query_data;      /* input data for IMS queries */
	Params_Rec  	params_data;     /* data retrieved from IMS */
	int		ret_code_gha = ER_IMS_GHA_QUERY_NODATA;
	int		ret_code_sv  = ER_IMS_SV_QUERY_NODATA;
	int		ret_code_tce = ER_IMS_TCE_QUERY_NODATA;
	int		ret_code_cal = ER_IMS_CAL_QUERY_NODATA;
	int		ret_code_scan = ER_IMS_SCAN_QUERY_NODATA;
	int		ims_ret_code;
	int		ret_code;
        char    	precision[SVEC_TYPE_STRLEN+1];
	char		job_state[ORDER_STATUS_STRLEN];
	IMS_FRAME_SOURCE_MEDIA_STRUCT	 frame_info;
	int		do_gha_query = 1;
	int		do_tce_query = 1;
	int		do_sv_query = 1;
	int		do_scan_query = 1;
	int		do_calparams_query = 1;

        /* establish a database connection with IMS server if previous
           attempt failed */
        if (! ims_connected)
        {
                if ((ims_ret_code = ims_db_connect(&ims_query)) != ER_NO_ERROR)
                        /* cannot connect to IMS db server,
                           no point in proceeding */
                        return(ims_ret_code);
 
                else
                        ims_connected = TRUE;
        }
 	
	/* get all necessary info to perform IMS queries ,
		if failed, return immediately to the caller */
	if (get_pending_job_data
		(job_id, job_type, &query_data) != ER_NO_ERROR)
		return (ER_DB_ACCESS);

	/* 
	** if tape info is not available:
	** 1. it is an error condition for scan job, return
	** 2. for L1 job, query IMS for frame status
	**    a) if the frame status is MISSED or REJECTED,
	**       return to the caller who will display a 
	**       message to the operator.
	**    b) if the frame status is SCANNED, PLANNED, SCHEDULED
	**       save the time and media id info returned from the query   
	**       (media id is valid only for SCANNED frame)
	*/ 
	if (strcmp(query_data.start_time,"") == 0 ||
            strcmp(query_data.end_time,"") == 0)
	{
		if (strcmp(job_type,L1_JOB_TYPE) == 0)
		{
			ret_code = wrap_ims_frameStatus
				(&ims_query, order_id, item_id, &frame_info);
			if (ret_code != ER_NO_ERROR)
				return(ret_code);
		}
		else
			return (ER_INSUFFICIENT_MEDIA_INFO);

		printf("frame status = %s, start_time = %s,"
			"end_time = %s, media_id = %s, center_time = %s, "
			"media_location = %s, media_type = %s, data direction = %s,"
			"station_id = %s, recorder_id = %s, scan file = %s\n",
			frame_info.frame_status, frame_info.start_time,
			frame_info.end_time, frame_info.media_id,
			frame_info.center_time, frame_info.media_location,
			frame_info.media_type, frame_info.data_direction,
			frame_info.station_id, frame_info.recorder_id,
			frame_info.scan_results_file );	

		if (strcmp(frame_info.frame_status,"MISSED") == 0) 
		{
			return (ER_MISSED_FRAME);
		}
		else if (strcmp(frame_info.frame_status,"REJECTED") == 0)
		{
			return (ER_REJECTED_FRAME);
		}
		strcpy(query_data.start_time, frame_info.start_time);
		strcpy(query_data.end_time, frame_info.end_time);
		strcpy(query_data.center_time, frame_info.center_time);
		if (strcmp(frame_info.frame_status,"SCANNED") == 0)
		{
			strcpy(query_data.media_id, frame_info.media_id);
			strcpy(avail_indicators->media_id,frame_info.media_id);
		}
		else
		{
			strcpy(query_data.media_id,"");
			strcpy(avail_indicators->media_id,"");
		}
	}
	/*
	** Tape info is available so we will not do IMS frameStatus query.
	** Initialize the frame_status to null string which tells the
	** update_job_params routine not to overwrite existing media info 
	*/ 
	else
	{
		frame_info.frame_status[0] = '\0';
	}


	/*
	** Determine what queries will be performed based on the
	** availability of the required input parameters 
	*/
	if ((strcmp(query_data.start_time,"") == 0) ||
	    (strcmp(query_data.end_time,"") == 0))
	{
		do_gha_query = 0;
		do_tce_query = 0;
		do_sv_query  = 0;
	}
	if (strcmp(query_data.center_time,"") == 0)
	{
		do_calparams_query = 0;
	}
	if (strcmp(query_data.media_id,"") == 0)
	{
		do_scan_query = 0;
	}

		
        /*************** perform GHA query **********************/

	if (do_gha_query)
	{
        	ret_code_gha = wrap_ims_ghaRangeQuery
			(&ims_query,
                          query_data.start_time,
                          query_data.end_time, 
	         	  &(params_data.GHA_data));
	}

        if (ret_code_gha == ER_IMS_GHA_QUERY_NODATA)
                strcpy(avail_indicators->GHA_avail,"NO");

	else if (ret_code_gha ==  ER_NO_ERROR) 
		strcpy(avail_indicators->GHA_avail,"YES");	

	else /* ER_IMS_GHA_QUERY_FAIL or ER_BAD_ODL_TIME_FORMAT */
	{
		/* we want to stop and report to user */
		return (ret_code_gha); 
	}
		
	/*************** perform TCE query ******************/

        if (strcmp(query_data.platform, R1_SATNAME) == 0)
	{
		strcpy(avail_indicators->TCE_avail,"N/A");
		ret_code_tce = ER_NO_ERROR;
	}
        else
	{
		if (do_tce_query)
		{
			ret_code_tce = wrap_ims_tceQuery(&ims_query,
                                query_data.start_time,
                                query_data.end_time,
                                query_data.platform,
                                &params_data.TCE_data);
		}
		if (ret_code_tce == ER_IMS_TCE_QUERY_NODATA)
	                strcpy(avail_indicators->TCE_avail,"NO");
 
	        else if (ret_code_tce ==  ER_NO_ERROR) 
	                strcpy(avail_indicators->TCE_avail,"YES");

		else /* ER_IMS_TCE_QUERY_FAIL or ER_BAD_ODL_TIME_FORMAT */
		{
			/* we want to stop here and report to user */	
			return (ret_code_tce);
		}
	}	
	
	/**************** perform SV query ***********************/

	strcpy(avail_indicators->SV_avail,"NO");
	if (do_sv_query)
	{
        	/* determine the type of state vector needed */
        	if (strcmp(query_data.quicklook_flag, "YES") == 0)
               		strcpy (precision, PREDICTED);
		else
	                strcpy (precision, RESTITUTED);
 
	        /* use restituted state vectors for non quick look products */
	        /* use predicted/restituted state vectors for quick look products */

		ret_code_sv = wrap_ims_svQuery(&ims_query,
                        query_data.platform,
                        precision,
                        query_data.start_time,
                        query_data.end_time,
                        &params_data.SV_data);
	}
	if (ret_code_sv == ER_BAD_ODL_TIME_FORMAT ||
            ret_code_sv == ER_IMS_SV_QUERY_FAIL)
	{
		/* we want to stop here and report to user */
		return (ret_code_sv);
	}	

	if (ret_code_sv == ER_NO_ERROR)
		strcpy(avail_indicators->SV_avail,"YES");

	else if (ret_code_sv == ER_IMS_SV_QUERY_NODATA && do_sv_query)
	{
              if (strcmp(query_data.quicklook_flag, "YES") == 0)
       	      { 

	                ret_code_sv = wrap_ims_svQuery(&ims_query,
       		                 query_data.platform,
				 RESTITUTED,
               		         query_data.start_time,
                       		 query_data.end_time,
				&params_data.SV_data);
	                if (ret_code_sv == ER_BAD_ODL_TIME_FORMAT ||
         	            ret_code_sv == ER_IMS_SV_QUERY_FAIL)
			{
                		/* we want to stop here and report to user */
				return (ret_code_sv);
			}
 
	      }	
	      if (ret_code_sv == ER_IMS_SV_QUERY_NODATA)
		strcpy(avail_indicators->SV_avail,"NO");
	      if (ret_code_sv == ER_NO_ERROR)
		strcpy(avail_indicators->SV_avail,"YES");	
	} 

	/********** get the calibration params file (for L1 only) ******/

	if (strcmp(job_type,L1_JOB_TYPE) == 0)
	{
		if (do_calparams_query)
		{
			ret_code_cal = wrap_ims_calParamQuery(&ims_query,
                        	query_data.platform,
	                        query_data.mode,
	                        query_data.center_time,
	                        params_data.cal_params_file,
				params_data.cal_params_file2);
		}
		if (ret_code_cal == ER_IMS_CAL_QUERY_FAIL)
		{
			/* stop here and report to user */
			return (ER_IMS_CAL_QUERY_FAIL);
		}

		else if (ret_code_cal == ER_NO_ERROR)
			strcpy(avail_indicators->Params_File_avail ,"YES");

		else
			strcpy(avail_indicators->Params_File_avail ,"NO");

	}
	else
	{
		strcpy(avail_indicators->Params_File_avail ,"N/A");
		ret_code_cal = ER_NO_ERROR;
	}

	/******** get the scan results file (for L1 only) *********/

        if (strcmp(job_type,L1_JOB_TYPE) == 0)
        {
		if (do_scan_query)
		{
			ret_code_scan = wrap_ims_scanQuery(&ims_query,
               		         query_data.platform,
                       		 query_data.rev,
                       		 query_data.sequence,
	                         query_data.media_id,
        	                 query_data.mode,
                	         params_data.scan_results_file);
		} 
                if (ret_code_scan == ER_IMS_SCAN_QUERY_FAIL)
		{
                        /* stop here and report to user */
                        return (ER_IMS_SCAN_QUERY_FAIL);
		}
 
                else if (ret_code_scan == ER_NO_ERROR)
                        strcpy(avail_indicators->Scan_File_avail ,"YES");
 
                else
                        strcpy(avail_indicators->Scan_File_avail ,"NO");
 
        }
	else
	{
		strcpy(avail_indicators->Scan_File_avail ,"N/A");
		ret_code_scan = ER_NO_ERROR;
	}


	/* determine if the new state for this job  based on the params
		availability */
	if (ret_code_gha  == ER_NO_ERROR &&
	    ret_code_sv   == ER_NO_ERROR &&
	    ret_code_tce  == ER_NO_ERROR &&
	    ret_code_cal  == ER_NO_ERROR &&
	    ret_code_scan == ER_NO_ERROR)
	{
		if (strcmp(query_data.auto_schedulable,"YES") == 0)
			strcpy(job_state,AVAILABLE);
		else
			strcpy(job_state,READY);
	}

	else
		strcpy(job_state,PENDING);

#ifdef PUSH_JOB_READY
	/* This code portion is intended for testing purpose only */
	perform_ims_queries(&params_data, avail_indicators);
	if (strcmp(query_data.auto_schedulable,"YES") == 0)
		strcpy(job_state,AVAILABLE);
	else
		strcpy(job_state,READY);
	if (strcmp(job_type,SCAN_JOB_TYPE) == 0)
	{
		strcpy(avail_indicators->Scan_File_avail ,"N/A");
		strcpy(avail_indicators->Params_File_avail , "N/A");
	}
	/* End test code portion */
#endif

        /*** if we get this far, all IMS queries were successfully made,
                 update the PPS database with data retrieved from IMS */

	params_data.avail_indicators = *avail_indicators;
        ret_code = update_job_params 
			(job_id, job_type, job_state, previous_job_state,
			 params_data, frame_info);

	if (ret_code != ER_NO_ERROR)
	{
		return (ER_DB_ACCESS);
	}

        /* inform IMS about the job state change  */
        if (strcmp(job_state,previous_job_state) != 0)
                send_IMS_order_status (job_id, job_type, job_state, query_data);

	if (strcmp(job_state,READY) == 0)
	{
		if (strcmp(job_state,previous_job_state) == 0)
			return (ER_JOB_IS_STILL_READY);
		else
			return (ER_JOB_IS_NOW_READY);
	}
	else if (strcmp(job_state,AVAILABLE) == 0)
	{
		return (ER_JOB_IS_NOW_AVAIL);
	}

	else if (strcmp(job_state,PENDING) == 0)	
	{
		if (strcmp(job_state,previous_job_state) == 0)
       	                 return (ER_JOB_IS_STILL_PENDING);
                else
                        return (ER_JOB_IS_NOW_PENDING);
	}	
		
}
