/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_ims_l1pr.c

Description:
	This module contains the routines used for processing IMS L1
	product requests. 

External Functions:
	process_IMS_L1PReq
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_ims_l1pr.c	1.3    02/21/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPShdr.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"


/*==============================================================================
Function:	
	int process_IMS_L1PReq(DBPROCESS *dbproc_server,
		DBPROCESS *dbproc_commit,
		IMS_L1PReq_Record *rec)

Description:	
	This function processes an IMS L1 Product Request.

Parameters:
	dbproc_server	connection to the db server
	dbproc_commit	connection to the commit service
	rec		extracted data

Returns:	PPS  error_code
Creator:	Nadia Adhami	
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int process_IMS_L1PReq( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
	IMS_L1PReq_Record 	*rec)
#else
int process_IMS_L1PReq(dbproc_server, dbproc_commit, ims_query, 
		rec)
	DBPROCESS		*dbproc_server, 
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
   	IMS_L1PReq_Record 	*rec)
#endif
{
        int 	ret = 0, nrecs = 0;
        int 	ret_code_sv,ret_code_gha,ret_code_tce;
        int 	ret_code_cal,ret_code_scan;
        char 	buf[2048] ;
        char 	col_names[1024] ;
        char 	col_values[1024] ;
        char 	precision[15] ;
	int	commid; 	/* commit service id */
	char	job_state[ORDER_STATUS_STRLEN];
	IMS_Order_Status	ims_status;
	char	logmsg[MAX_SYSLOG_MSGLEN];
	char    errormsg[MAXLINE];

        /* construct the error message for reporting deadlock error */
        (void)sprintf(errormsg, " %s : %s [order_id = %d, item_id = %d]",
                        pps_err_msgs[ER_DEADLOCK], "process_IMS_L1PReq",
                        rec->order_id, rec->item_id);

	/* check if another job with the same processing	*
         * parameters exists in the processing parameters table */

	ret = match_L1request(dbproc_server, rec, &rec->job_id);
	if (ret)
	{
		/* find the job state of the given jobid */
		ret = get_jobstate(dbproc_server, rec->job_id, job_state);
		if (ret == ERROR)
		{
			(void)sprintf(logmsg,
				"job [%d/%s] not found in jobs table",
				rec->job_id, job_state);
           		pps_logMsg(ProgName, PPS_ERROR, logmsg);
			abortall(dbproc_server, dbproc_commit, commid);
			return(ER_JOBID_NOT_FOUND);
		}
		strcpy( ims_status.status_id, job_state );

		/* start the destributed transaction on the commit service */
		pps_start_xact(dbproc_server,dbproc_commit,&commid,L1_KEYWD);

		/* form the query string containing the keywords */
		sprintf(col_names, "order_id,item_id,job_id");

		/* form the string containing the corresponding values */
		sprintf(col_values, "%d,%d,%d",
			rec->order_id, rec->item_id, rec->job_id);

        	/* insert a new record into L1 order table */
		/* with the same job_id */
        	sprintf(buf, "insert into %s (%s) values (%s)",
               		L1_ORDERS_TABLENAME, col_names,col_values);
	        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
		CHECK_DEADLOCK(errormsg);

        	if (nrecs != 1)
        	{
			(void)sprintf(buf, "%s [order=%d, item=%d, job=%d]\n",
				pps_err_msgs[ER_INSERT_L1PR],
				rec->order_id, rec->item_id, rec->job_id);
			sprintf(buf,
                	"    Probably a record with the same key exists.");
           		pps_logMsg(ProgName, PPS_ERROR, buf);
			abortall(dbproc_server, dbproc_commit, commid);
			return(ER_INSERT_L1PR);
        	}
		/* create order status to be sent to IMS */
		fill_L1order_status (rec, &ims_status, 
			INTERMEDIATE, ims_status.status_id, NULL, NULL);

        	/* create buffer containing order status */
        	create_order_status_buf_mx( &ims_status, buf, sizeof(buf));
 
        	/* send order status to IMS */
        	ret = wrap_ims_orderStatus ( ims_query, buf);
	        if (ret != ER_NO_ERROR)
        	{
                	sprintf(buf,"%s [order=%s,item=%s,job_id=%d,status=%s]",
                        	"Failed to send order status to IMS",
                        	ims_status.order_id,ims_status.item_id,
				rec->job_id,
                        	ims_status.status_id
                        	);
           		pps_logMsg(ProgName, PPS_ERROR, buf);
        	}


		/* commit transaction */
		pps_commit_xact(dbproc_server, dbproc_commit, commid);

		return (ER_NO_ERROR);
	}

	/* if job has a tape id, perform IMS queries */
	if (rec->media_id[0] != '\0')
	{

		/* query IMS */
       		ret_code_gha = wrap_ims_ghaRangeQuery( ims_query, 
			rec->start_time.time_string, 
			rec->end_time.time_string, &rec->gha);

    		if (ret_code_gha != ER_NO_ERROR)
        	{
                        (void)sprintf(buf, "%s [order=%d, item=%d, "
				"start=%s, end=%s]",
                                pps_err_msgs[ret_code_gha],
                                rec->order_id, rec->item_id,
			        rec->start_time.time_string,
				rec->end_time.time_string);
			if (ret_code_gha == ER_IMS_GHA_QUERY_NODATA)
			{
				pps_logMsg(ProgName, PPS_INFO, buf);
			}
			else
			{
           			pps_logMsg(ProgName, PPS_ERROR, buf);
				return (ret_code_gha);
			}
			/* go on if ER_IMS_GHA_QUERY_NODATA */
        	}

		/* determine the type of state vector needed */
		if (! strcmp (rec->quicklook_flag, "YES"))
			strcpy (precision, "PREDICTED");
		else
			strcpy (precision, "RESTITUTED");

		/* use restituted state vectors for quick look products */
		/* use predicted/restituted state vectors for quick look products */
		ret_code_sv = wrap_ims_svQuery( ims_query,
			rec->platform, 
			precision, 
			rec->start_time.time_string, 
			rec->end_time.time_string, 
			&rec->sv);
		if (ret_code_sv != ER_NO_ERROR)
		{
           		if (ret_code_sv == ER_BAD_ODL_TIME_FORMAT ||
               			ret_code_sv == ER_IMS_SV_QUERY_FAIL)
			{
				(void)sprintf(buf, "%s [order=%d, item=%d, "
					"platform=%s, start=%s, end=%s]",
					pps_err_msgs[ret_code_sv],
					rec->order_id, rec->item_id,
					rec->platform,
					rec->start_time.time_string,
					rec->end_time.time_string);
				pps_logMsg(ProgName, PPS_ERROR, buf);
	   			return (ret_code_sv);
			}
			else if (ret_code_sv == ER_IMS_SV_QUERY_NODATA)
			{
                            if (! strcmp(rec->quicklook_flag, "YES"))
                            {
				(void)sprintf(buf, "%s [order=%d, item=%d]",
                                          "PREDICTED SV not available for QL, "
                                          "trying RESTITUTED SV",
                                          rec->order_id, rec->item_id);
				pps_logMsg(ProgName, PPS_INFO, buf);

                                ret_code_sv = wrap_ims_svQuery( ims_query,
						rec->platform, 
						"RESTITUTED", 
						rec->start_time.time_string, 
						rec->end_time.time_string, 
						&rec->sv);
				if (ret_code_sv != ER_NO_ERROR)
				{
					(void)sprintf(buf,"%s [order=%d, item=%d,"
						 " platform=%s,start=%s, end=%s]",
                                                 pps_err_msgs[ret_code_sv],
                                                 rec->order_id, rec->item_id,
						 rec->platform,
						 rec->start_time.time_string,
						 rec->end_time.time_string);
                                	if (ret_code_sv == ER_IMS_SV_QUERY_NODATA)
                                	{
                                    		pps_logMsg(ProgName,PPS_INFO,buf);
					}
					else
					{
						pps_logMsg(ProgName,PPS_ERROR,buf);
						return (ret_code_sv);
					}
                                }
                            }
                            else
                            {
				(void)sprintf(buf, "%s [order=%d, item=%d,"
					" platform=%s, start=%s, end=%s]",
					pps_err_msgs[ret_code_sv],
					rec->order_id, rec->item_id,
					rec->platform,
					rec->start_time.time_string,
					rec->end_time.time_string);
				pps_logMsg(ProgName, PPS_INFO, buf);
                            }
			}
		}

                /* TCE not needed for R1 */
                if (! strcmp(rec->platform, R1_SATNAME))
                        ret_code_tce = ER_NO_ERROR;
                else
			ret_code_tce = wrap_ims_tceQuery( ims_query, 
				rec->start_time.time_string, 
				rec->end_time.time_string, 
				rec->platform, 
				&rec->tc);
		if (ret_code_tce != ER_NO_ERROR)
		{
			(void)sprintf(buf, "%s [order=%d, item=%d,"
				" platform=%s, start=%s, end=%s]",
				pps_err_msgs[ret_code_tce],
				rec->order_id, rec->item_id,
				rec->platform,
				rec->start_time.time_string,
				rec->end_time.time_string);
			if (ret_code_tce == ER_IMS_TCE_QUERY_NODATA)
			{
				pps_logMsg(ProgName, PPS_INFO, buf);
			}
			else
			{
				pps_logMsg(ProgName, PPS_ERROR, buf);
				return (ret_code_tce);
			}
			/* go on if ER_IMS_TCE_QUERY_NODATA */
		}

		ret_code_cal = wrap_ims_calParamQuery( ims_query, 
			rec->platform, 
			rec->mode, 
			rec->center_time.time_string,
			rec->cal_params_file,
			rec->cal_params_file2);
		if (ret_code_cal != ER_NO_ERROR)
		{
                        (void)sprintf(buf, "%s [order=%d, item=%d,"
				" platform=%s, mode=%s, centertime=%s]",
                                pps_err_msgs[ret_code_cal],
                                rec->order_id, rec->item_id,
				rec->platform, rec->mode, 
				rec->center_time.time_string);
			if (ret_code_cal == ER_IMS_CAL_QUERY_NODATA)
			{
				pps_logMsg(ProgName, PPS_INFO, buf);
			}
			else
			{
				pps_logMsg(ProgName, PPS_ERROR, buf);
				return (ret_code_cal);
			}
		}

		ret_code_scan = wrap_ims_scanQuery( ims_query, 
			rec->platform, 
			rec->rev, 
			rec->datatake_seq,
			rec->media_id, 
			rec->mode,
			rec->scan_results_file);
		if (ret_code_scan != ER_NO_ERROR)
		{
			(void)sprintf(buf, "%s [order=%d, item=%d]",
                                pps_err_msgs[ret_code_scan],
                                rec->order_id, rec->item_id);
			if (ret_code_scan == ER_IMS_SCAN_QUERY_NODATA)
			{
				pps_logMsg(ProgName, PPS_INFO, buf);
			}
			else
			{
	                        pps_logMsg(ProgName, PPS_ERROR, buf);
       		                return (ret_code_scan);
			}
		}

		if (ret_code_gha == ER_IMS_GHA_QUERY_NODATA ||
			ret_code_tce == ER_IMS_TCE_QUERY_NODATA ||
			ret_code_cal == ER_IMS_CAL_QUERY_NODATA ||
			ret_code_scan == ER_IMS_SCAN_QUERY_NODATA ||
			ret_code_sv == ER_IMS_SV_QUERY_NODATA )
		{
#if 0

/* sally: will keep the old tape info for check-params in GUI */
/*        Why should we have to erase it anyway?              */

			/* erase tape information */
			l1req_erase_tape( rec );
#endif
			/* set job state to pending */
			strcpy(job_state, PENDING);
		}
		else /* ER_NO_ERROR */
			strcpy(job_state, READY);
	}
	else
		strcpy(job_state, PENDING);

	/*-------------------------------------------------*/
        /* execute stored procedure to get the next job_id */
	/*-------------------------------------------------*/
        db_exec_cmd (dbproc_server, "sp_get_next_job_id", &rec->job_id,&nrecs);
	CHECK_DEADLOCK(errormsg);

#ifdef DEBUG
        printf("Next JOB_ID for L1 [order=%d, item=%d]: %d\n",
			rec->order_id, rec->item_id, rec->job_id);
#endif

	/* Transaction Processing Starts */

	/* start the destributed transaction on the commit service */
	pps_start_xact(dbproc_server, dbproc_commit, &commid, L1_KEYWD);

	/* form the query string containing the keywords */
	sprintf(col_names, "order_id,item_id,job_id");
	sprintf(col_values, "%d,%d,%d",
		rec->order_id, rec->item_id, rec->job_id);

        /* insert a new record into L1 order table */
        sprintf(buf, "insert into %s (%s) values (%s)",
                L1_ORDERS_TABLENAME, col_names,col_values);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
	CHECK_DEADLOCK(errormsg);
        if (nrecs != 1)
        {
                (void)sprintf(buf,
			"Can not add [order=%d, item=%d, job=%d]\n",
			rec->order_id, rec->item_id, rec->job_id);
		strcat(buf,"    to L1_orders table,\n");
		strcat(buf,
                	"    Probably a record with the same key(s) exists.");
           	pps_logMsg(ProgName, PPS_ERROR, buf);
		abortall(dbproc_server, dbproc_commit, commid);
		return(ER_INSERT_L1PR);
        }

	sprintf(col_names, "job_id,activity_id,");
	sprintf(buf, "quicklook_flag,platform,sensor,rev,mode,sequence,");
	strcat(col_names, buf);
	sprintf(buf, "product_type,frame_mode,site_name,");
	strcat(col_names, buf);
	sprintf(buf, "pixel_spacing,frame_id,output_format,projection,");
	strcat(col_names, buf);
	sprintf(buf, "processing_gain,avg_terrain_ht,subframe_id,deskew, ");
	strcat(col_names, buf);
	sprintf(buf, "ps_reference_lat,ps_reference_lon,utm_zone,");
	strcat(col_names, buf);
	sprintf(buf,"terrain_correction,lambert_latitude_n,lambert_latitude_s");
	strcat(col_names, buf);
	sprintf(buf," ,compensation_flag,scan_results_file,cal_params_file,cal_params_file2");
	strcat(col_names, buf);

	/* form the string containing the corresponding values */
	sprintf(col_values, 
	"%d,\"%s\",\"%s\",\"%s\",\"%s\",%d,\"%s\",%d,\"%s\",\"%s\",\"%s\",",
		rec->job_id, rec->activity_id,
		rec->quicklook_flag,
		rec->platform, rec->sensor, rec->rev, 
		rec->mode, rec->datatake_seq, rec->product_type,
		rec->frame_mode, rec->site_name
		);
	sprintf(buf, "%f,%d,\"%s\",\"%s\",%d,%f,%d,",
		rec->pixel_spacing, rec->frame_id, rec->output_format, 
		rec->projection, rec->processing_gain, rec->avg_terrain_ht, 
		rec->subframe_id);
	strcat(col_values, buf);
	sprintf(buf, "\"%s\",%f,%f,%d,\"%s\",%f,%f,",
		rec->deskew,
		rec->ps_reference_lat, rec->ps_reference_lon, 
		rec->utm_zone, rec->terrain_correction,
		rec->lambert_latitude_n, rec->lambert_latitude_s);
	strcat(col_values, buf);

	

	sprintf(buf, " \"%s\",\"%s\",\"%s\",\"%s\"",
		rec->compensation_flag,
		rec->scan_results_file,rec->cal_params_file,
		rec->cal_params_file2);
	strcat(col_values, buf);

	if (rec->media_type[0])
	{
		sprintf(buf, ",media_type,media_id,media_location");
		strcat(col_names, buf);
		sprintf(buf, ",start_time,end_time,center_time,recorder_id,station_id,data_direction");
		strcat(col_names, buf);

		sprintf(buf, ",\"%s\",\"%s\",\"%s\"",
			rec->media_type, rec->media_id, 
			rec->media_location);
		strcat(col_values, buf);
		sprintf(buf, ",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"",
			rec->start_time, rec->end_time, rec->center_time,
			rec->recorder_id, rec->station_id,
			rec->data_direction);
		strcat(col_values, buf);
	}

        /* store queried parameters if they are available */
 
        if ( rec->tc.rev > 0 )
        {
                sprintf(buf," ,tce_rev,tce_sat_time,tce_clock_cycle,tce_time");
                strcat(col_names, buf);
 
                sprintf(buf, " ,%d,%d,%d,\"%s\"",
                        rec->tc.rev,rec->tc.sat_time,
                        rec->tc.clock_cycle,rec->tc.time);
                strcat(col_values, buf);
        }
        if ( rec->gha.angle > 0 )
        {
                sprintf(buf," ,gha_time,gha_angle");
                strcat(col_names, buf);
 
                sprintf(buf, " ,\"%s\",%f", rec->gha.time,rec->gha.angle);
                strcat(col_values, buf);
        }
        if ( rec->sv.rev > 0 )
        {
                sprintf(buf," ,sv_type,sv_rev,sv_time,");
                strcat(col_names, buf);
                sprintf(buf,"sv_x_pos,sv_y_pos,sv_z_pos,");
                strcat(col_names, buf);
                sprintf(buf,"sv_x_velocity,sv_y_velocity,sv_z_velocity,sv_coord_sys");
                strcat(col_names, buf);
 
                sprintf(buf, " ,\"%s\",%d,\"%s\", %f,%f,%f, %f,%f,%f,\"%s\"",
                        rec->sv.precision,rec->sv.rev,rec->sv.time,
                        rec->sv.x_pos,rec->sv.y_pos,rec->sv.z_pos,
                        rec->sv.x_vel,rec->sv.y_vel,rec->sv.z_vel,
			rec->sv.coord_sys);
                strcat(col_values, buf);
        }

        /* insert a new record into L1 procparms table */
        sprintf(buf, "insert into %s (%s) values (%s)",
                L1_PROC_PARMS_TABLENAME, col_names,col_values);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
	CHECK_DEADLOCK(errormsg);
        if (nrecs != 1)
        {
                (void)sprintf(buf,
			"Can not add [column_names=%s, column_values=%s]",
			col_names, col_values);
		strcat(buf,"    to L1 Proc Parms table,\n");
		strcat(buf,
                	"    Probably a record with the same key(s) exists.");
           	pps_logMsg(ProgName, PPS_ERROR, buf);
		abortall(dbproc_server, dbproc_commit, commid);
		return(ER_INSERT_PROC_PARMS);
        }

	/* form the SQL string for the following database operation */
	sprintf(col_names, "job_id,job_state,job_type");
	strcat(col_names,",quicklook_flag,priority,insert_top_flag,state_date");
	sprintf(col_values, 
		"%d, \"%s\",\"%s\",\"%s\",\"%s\",\"%s\",getdate()",
		rec->job_id, 
		job_state, 
		L1_KEYWD,
		rec->quicklook_flag, 
		rec->priority,
		"NO");

        /* insert a new record into jobs table */
        sprintf(buf, "insert into %s (%s) values (%s)",
                JOBS_TABLENAME, col_names,col_values);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
	CHECK_DEADLOCK(errormsg);
        if (nrecs != 1)
        {
		(void)sprintf(logmsg, "%s [job=%d]",
			pps_err_msgs[ER_INSERT_JOBS], rec->job_id);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		abortall(dbproc_server, dbproc_commit, commid);
		return(ER_INSERT_JOBS);
        }

	/* for ready jobs, see if they have to be auto submitted */
	if (! strcmp (job_state, READY))
	{
                /* execute stored procedure to auto submit */
                /* the job if policy table indicates so */
#ifdef DEBUG
                printf("sp_auto_avail( L1 job_id=%d %s )\n",
                        rec->job_id,rec->priority);
#endif
                sprintf(buf, "sp_auto_avail %d", rec->job_id);
                db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
		CHECK_DEADLOCK(errormsg);
                if (ret == 1)
		{
#ifdef DEBUG
                        printf("L1 job_id %d %s is available\n", 
				rec->job_id, rec->priority);
#endif
			strcpy(job_state, AVAILABLE);
		}
                else
		if (ret < 0)
                {
                        printf("FAILED to make L1 job_id %d %s available\n", 
                                rec->job_id, rec->priority);
			(void)sprintf(logmsg, "%s [job=%d, priority=%s]",
				pps_err_msgs[ER_L1_AVAIL_FAIL],
				rec->job_id, rec->priority);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
                }

	}

	/* Transaction Processing Ends */
	/* commit transaction */
	pps_commit_xact(dbproc_server, dbproc_commit, commid);

	/* create order status to be sent to IMS */
	fill_L1order_status (rec, &ims_status, INTERMEDIATE, job_state,NULL,NULL);

        /* create buffer containing order status */
       	create_order_status_buf_mx( &ims_status, buf, sizeof(buf));
 
       	/* send order status to IMS */
       	ret = wrap_ims_orderStatus ( ims_query, buf);
        if (ret != ER_NO_ERROR)
        {
                sprintf(buf,"%s [order_id=%s,item_id=%s,job_id=%d,status=%s]",
                        "Failed to send order status to IMS",
                        ims_status.order_id, ims_status.item_id,
			rec->job_id,
                        ims_status.status_id);
		pps_logMsg(ProgName, PPS_ERROR, buf);
        }


	return(ER_NO_ERROR);

} /* process_IMS_L1PReq */

/* End of File */
