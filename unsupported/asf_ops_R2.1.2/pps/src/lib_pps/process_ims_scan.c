/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_ims_scan.c

Description:
	This module contains the routines used for processing IMS scan requests. 

External Functions:
	process_IMS_ScanReq
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_ims_scan.c	1.3    02/21/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <pthread.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

/*==============================================================================
Function:	
	int process_IMS_ScanReq(DBPROCESS *dbproc_server,
		DBPROCESS *dbproc_commit,
		IMS_ScanReq_Record *rec)

Description:	
	This function processes an IMS Scan Request.

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
int process_IMS_ScanReq( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
	IMS_ScanReq_Record 	*rec)
#else
int process_IMS_ScanReq(dbproc_server, dbproc_commit, ims_query, rec)
	DBPROCESS		*dbproc_server;
	DBPROCESS		*dbproc_commit; 
	IMS_CMN_QUERY		**ims_query;
   	IMS_ScanReq_Record 	*rec;
#endif
{
        int 	ret, nrecs;
        int 	ret_code_sv,ret_code_gha,ret_code_tce;
	int	commid; 	/* commit service id */
        char 	col_names[1024] ;
        char 	col_values[1024] ;
        char 	buf[2048] ;
        char 	precision[15] ;
	IMS_Order_Status	ims_status;
	char	logmsg[MAX_SYSLOG_MSGLEN];
	char	errormsg[MAXLINE];  /* to report deadlock error */ 

	/* construct the error message for reporting deadlock error */
	(void)sprintf(errormsg, " %s : %s [order_id = %d, item_id = %d]",
			pps_err_msgs[ER_DEADLOCK], "process_IMS_ScanReq", 
			rec->order_id, rec->item_id);

	/* query IMS for GHA, SVEC, TCE */
       	ret_code_gha = wrap_ims_ghaRangeQuery( ims_query, 
		rec->start_time.time_string, 
		rec->end_time.time_string, &rec->gha);

	if (ret_code_gha != ER_NO_ERROR) 
	{
		(void)sprintf(logmsg, "%s [order=%d, item=%d, start=%s, end=%s]",
                        pps_err_msgs[ret_code_gha],
			rec->order_id, rec->item_id,
                        rec->start_time.time_string,
                        rec->end_time.time_string);
		if (ret_code_gha == ER_IMS_GHA_QUERY_NODATA)
		{
			pps_logMsg(ProgName, PPS_INFO, logmsg);
		}
		else
		{
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
			return (ret_code_gha);
		}	
		/* go on if ER_IMS_GHA_QUERY_NODATA */
	}

	/* determine the type of state vector needed */
	if (! strcmp (rec->quicklook_flag, "YES"))
		strcpy (precision, "PREDICTED");
	else
		strcpy (precision, "RESTITUTED");

	/* use restituted state vectors for non quick look products */
	/* use predicted/restituted state vectors for quick look products */

	ret_code_sv = wrap_ims_svQuery( ims_query,
		rec->platform, precision, rec->start_time.time_string, 
		rec->end_time.time_string, &rec->sv);
	
	if (ret_code_sv != ER_NO_ERROR)
	{
           if (ret_code_sv == ER_BAD_ODL_TIME_FORMAT ||
               ret_code_sv == ER_IMS_SV_QUERY_FAIL)
	   {
	      (void)sprintf(logmsg, "%s [order=%d, item=%d, "
			 "platform=%s, start=%s, end=%s]",
                         pps_err_msgs[ret_code_sv],
			 rec->order_id, rec->item_id,
			 rec->platform,
                         rec->start_time.time_string,
                         rec->end_time.time_string);
	       pps_logMsg(ProgName, PPS_ERROR, logmsg);
               return (ret_code_sv);
	   }

           /* try "restituted" state vectors for quicklook orders if
              ER_IMS_SV_QUERY_NODATA  returned for first attempt,
	      otherwise, go on */ 	
	   else if (ret_code_sv == ER_IMS_SV_QUERY_NODATA)
	   {
	      if (! strcmp (rec->quicklook_flag, "YES"))
	      {
	        (void)sprintf(buf, "%s [order=%d, item=%d, "
			     "platform=%s, start=%s, end=%s]",
                             "PREDICTED SV not available for QL, "
                             "trying RESTITUTED SV",
                             rec->order_id, rec->item_id,
			     rec->platform,
		             rec->start_time.time_string,
			     rec->end_time.time_string);
		pps_logMsg(ProgName, PPS_INFO, buf);

	   	ret_code_sv = wrap_ims_svQuery( ims_query,
			rec->platform,"RESTITUTED",
			rec->start_time.time_string, 
			rec->end_time.time_string, &rec->sv);
		if (ret_code_sv != ER_NO_ERROR)
		{
	           (void)sprintf(logmsg, "%s [order=%d, item=%d, "
			 "platform=%s, start=%s, end=%s]",
                         pps_err_msgs[ret_code_sv],
			 rec->order_id, rec->item_id,
			 rec->platform,
                         rec->start_time.time_string,
                         rec->end_time.time_string);
		   if (ret_code_sv == ER_IMS_SV_QUERY_NODATA)
		   {
		   	pps_logMsg(ProgName, PPS_INFO, logmsg);
		   }
		   else
		   {
			 pps_logMsg(ProgName, PPS_ERROR, logmsg);
			 return (ret_code_sv);
		   }
		}
	      }
	      else /* this job is not quick-look */
	      {
                   (void)sprintf(logmsg, "%s [order=%d, item=%d, "
		         "platform=%s, start=%s, end=%s]",
                         pps_err_msgs[ret_code_sv],
			 rec->order_id, rec->item_id,
			 rec->platform,
                         rec->start_time.time_string,
                         rec->end_time.time_string);
                   pps_logMsg(ProgName, PPS_INFO, logmsg);
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
		rec->platform, &rec->tc);
        if (ret_code_tce != ER_NO_ERROR)
        {
           (void)sprintf(logmsg, "%s [order=%d, item=%d, "
			"platform=%s, start=%s, end=%s]",
                        pps_err_msgs[ret_code_tce],
			rec->order_id, rec->item_id,
			rec->platform,
                        rec->start_time.time_string,
                        rec->end_time.time_string);
	   if (ret_code_tce == ER_IMS_TCE_QUERY_NODATA)
	   {
		pps_logMsg(ProgName, PPS_INFO, logmsg);
	   }
	   else
	   {
           	pps_logMsg(ProgName, PPS_ERROR, logmsg);
                return (ret_code_tce);
	   }
           /* go on if ER_IMS_TCE_QUERY_NODATA */
        }

	if (ret_code_gha == ER_IMS_GHA_QUERY_NODATA ||
	    ret_code_tce == ER_IMS_TCE_QUERY_NODATA ||
	    ret_code_sv  == ER_IMS_SV_QUERY_NODATA )
	{
#if 0
/* thuy: will keep the state vector info for check-params in GUI */
/*        Why should we have to erase it anyway?              */

		/* erase state vector information */
		scanreq_erase_svec( rec );
#endif
		/* set job state to pending */
		strcpy(rec->job_state, PENDING);
	}
	else /* ER_NO_ERROR */
	{
		strcpy(rec->job_state, READY);
        }

	/* the following SQL procedure is a single transaction */
	/* execute stored procedure to get the next job_id */
	db_exec_cmd (dbproc_server, "sp_get_next_job_id", &rec->job_id, &nrecs);

	CHECK_DEADLOCK (errormsg);

#ifdef DEBUG
        printf("Next JOB_ID for Scan [order=%d, item=%d]: %d\n",
			rec->order_id, rec->item_id, rec->job_id);
#endif

	/* form the query string containing the keywords */
	sprintf(col_names, "order_id,item_id,job_id,order_type,");
	sprintf(buf, "quicklook_flag,platform,sensor,rev,mode,sequence,");
	strcat(col_names, buf);
	sprintf(buf, "activity_id,frame_mode,site_name,");
	strcat(col_names, buf);
	sprintf(buf, "media_id,media_type,media_location,");
	strcat(col_names, buf);
	sprintf(buf, "start_address,end_address,");
	strcat(col_names, buf);
	sprintf(buf, "start_time,end_time,recorder_id,station_id,data_direction");
	strcat(col_names, buf);


	/* form the query string containing the corresponding values */
	sprintf(col_values, 
	"%d,%d,%d,\"%s\",\"%s\",\"%s\",\"%s\",%d,\"%s\",%d,\"%s\",",
		rec->order_id, rec->item_id, rec->job_id, 
		rec->order_type, rec->quicklook_flag,
		rec->platform, rec->sensor, rec->rev, 
		rec->mode, rec->datatake_seq, rec->activity_id
		);
	sprintf(buf,
	"\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%d,%d,\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"",
		rec->frame_mode,
		rec->site_name, 
		rec->media_id, rec->media_type, rec->media_location,
		rec->start_address, rec->end_address, 
		rec->start_time, rec->end_time, 
		rec->recorder_id, rec->station_id, rec->data_direction
		);
	strcat(col_values, buf);

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

	/** Transaction Processing Starts **/
	/* start the destributed transaction on the commit service */
	pps_start_xact(dbproc_server, dbproc_commit, &commid, SCAN_KEYWD);

	/* insert a new record into scan order table */
        sprintf(buf, "insert into %s (%s) values (%s)", 
		SCAN_ORDERS_TABLENAME, col_names,col_values);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

	CHECK_DEADLOCK (errormsg);

        if (nrecs != 1)
        {
		(void)sprintf(logmsg, "%s [order=%d, item=%d]",
					pps_err_msgs[ER_INSERT_SCAN],
					rec->order_id, rec->item_id);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		abortall(dbproc_server, dbproc_commit, commid);
		return(ER_INSERT_SCAN);
        }

	/* form the SQL string for the following database operation */
	sprintf(col_names, "job_id,job_state,job_type");
	strcat(col_names,",quicklook_flag,priority,insert_top_flag,state_date");
	sprintf(col_values, 
		"%d, \"%s\", \"%s\",\"%s\",\"%s\",\"%s\", getdate()",
		rec->job_id, 
		rec->job_state, 
		SCAN_KEYWD,
		rec->quicklook_flag, 
		rec->priority,
		"NO");

	/* insert a new record into jobs table */
	sprintf(buf, "insert into %s (%s) values (%s)",
		JOBS_TABLENAME, col_names,col_values);
	db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

	CHECK_DEADLOCK (errormsg);

        if (nrecs != 1)
        {
	  	(void)sprintf(logmsg,
			"Can not add scan order to jobs table [job=%d]",
			rec->job_id);
	   	pps_logMsg(ProgName, PPS_ERROR, logmsg);
		abortall(dbproc_server, dbproc_commit, commid);
		return(ER_INSERT_JOBS);
        }


	/* for ready jobs, see if they have to be auto submitted */
	if (! strcmp (rec->job_state, READY))
	{
 		/* execute stored procedure to make the */
		/* job available if policy table indicates so */
		sprintf(buf, "sp_auto_avail %d", rec->job_id);
		db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

		CHECK_DEADLOCK (errormsg);

		if (ret == 1)
		{
#ifdef DEBUG
			printf("SCAN job_id %d %s is available\n", 
				rec->job_id, rec->priority);
#endif
			strcpy(rec->job_state, AVAILABLE);
		}
		else if (ret < 0)
		{
			printf("FAILED to make SCAN job_id %d %s available\n", 
				rec->job_id, rec->priority);
			(void)sprintf(logmsg, "%s [job=%d, priority=%s]",
					pps_err_msgs[ER_SCAN_AVAIL_FAIL],
					rec->job_id, rec->priority);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
		}
	
	}

	/** Transaction Processing Ends  **/
	/* commit transaction */
	pps_commit_xact(dbproc_server, dbproc_commit, commid);

	/* create order status to be sent to IMS */
	fill_scanorder_status (rec, &ims_status, INTERMEDIATE, rec->job_state, NULL, NULL);

	/* create buffer containing order status */
	create_order_status_buf_mx( &ims_status, buf, sizeof(buf));

	/* send order status to IMS */
	ret = wrap_ims_orderStatus ( ims_query, buf);
        if (ret != ER_NO_ERROR)
        {
		(void)sprintf(logmsg, "%s [order=%s,item=%s,job=%d,status=%s]",
					"Failed to send order status to IMS",
					ims_status.order_id,
					ims_status.item_id,
					rec->job_id,
					ims_status.status_id);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
        }

	return(ER_NO_ERROR);

} /* process_IMS_ScanReq */

/* End of File */
