/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_ims_svavail.c

Description:
	This module contains the routines used for processing IMS State
	Vector Available message. 

External Functions:
	process_IMS_SvAvail
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_ims_svavail.c	1.5    12/08/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPShdr.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "db_jobs.h"
#include "db_scan_order.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

extern COLUMN_DEFS      scan_order_columns_[];

#define CHECK_DEADLOCK_AND_CLEANUP(errormsg) \
        if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE) \
        { \
                pps_logMsg(ProgName, PPS_ERROR, errormsg); \
		free(rec); \
		DEL_LIST( llistptr ); \
                return (ER_DEADLOCK); \
        } \

/*==============================================================================
Function:	
	int process_IMS_SvAvail(DBPROCESS *dbproc_server,
		DBPROCESS *dbproc_commit,
		IMS_SVecAvail_Record *svrec)

Description:	
	This function processes an IMS State Vector Available message.

Parameters:
	dbproc_server	connection to the db server
	dbproc_commit	connection to the commit service
	svrec		extracted data

Returns:	PPS  error_code
Creator:	Nadia Adhami	
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int process_IMS_SvAvail( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
	IMS_SVecAvail_Record 	*svrec)
#else
int process_IMS_SvAvail(dbproc_server, dbproc_commit, ims_query, svrec)
	DBPROCESS		*dbproc_server, 
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
   	IMS_SVecAvail_Record 	*svrec)
#endif
{
        int 			ret = 0, nrecs = 0;
        int 			ret_code_sv,ret_code_gha,ret_code_tce;
	int			commid;
        char 			buf[2048] ;
        char 			col_names[1024] ;
        char 			col_values[1024] ;
        char 			precision[15];
	char			job_state[20];
        char    		time_str[TIME_STRING_LEN+1];
	llist   		*llistptr = NULL;
	cursor  		ptr ;
        DB_RECORD       	**db_rec ;
	IMS_Order_Status	ims_status;
   	IMS_ScanReq_Record 	*rec; 
	char			logmsg[MAX_SYSLOG_MSGLEN];
	char			errormsg[MAXLINE];
        char            	adjusted_end_time[TIME_STRING_LEN+1];
        time_t          	seconds;
        unsigned short  	milliseconds;


	/* compute adjusted_end_time = svrec.end_time + 100 minutes  */
        (void) odl_timestring_to_seconds(svrec->end_time, &seconds,
                                &milliseconds);
        seconds += 6000;
        (void) seconds_to_odl_timestring(adjusted_end_time, seconds,
                                milliseconds);

	/* 
	** Find the pending scan jobs matching this state vector 
	** match criteria:
	**   1. platform
	**   2. quicklook_flag = "no" if predicted statec vector
	**   3. start_time between sv.start_time and sv.end_time + 100 minutes
	*/
 
	sprintf(buf, "where platform = \"%s\""
			" and start_time between '%s' and '%s' ", 
			svrec->platform,
			svrec->start_time.time_string, adjusted_end_time);
		
	/* 
	** QLK  Scans need Predicted or Restitued Svec
	** NQLK Scans need Restitued Svec
	*/
	if (! strcmp(svrec->type , PREDICTED))
			strcat (buf, " and quicklook_flag = \"YES\"");

	/*
	** join with jobs table to retrieve only PENDING jobs
	*/
	strcat (buf, " and job_id in (select job_id from jobs where "
		     " job_state = 'PENDING' and job_type = 'SCAN')");

#ifdef DEBUG
	printf("sql => %s\n", buf);
#endif

	/*
	** send the sql command to the server and get the return records
	*/	
	llistptr = db_get_records(dbproc_server, SCAN_ORDERS_TABLENAME,
               		buf, NULL, scan_order_columns_, ALL_COLS);


	/*
	** Return if no matching records found
	*/
	if (NUMELTS(llistptr) <= 0)
	{
		DEL_LIST (llistptr);
		sprintf(logmsg,"No pending scan jobs matched this state vector");
		pps_logMsg(ProgName, PPS_INFO, logmsg);
		return(ER_NO_ERROR);
	}

	/*
	** Allocate working space for a scan record
	*/
	rec = malloc(sizeof(IMS_ScanReq_Record));
	if (rec == (IMS_ScanReq_Record *)NULL)
	{
		DEL_LIST (llistptr);
		sprintf(logmsg,"process_IMS_SvAvail: Failed to "
				"allocate memory for IMS_ScanReq_Record");
		pps_logMsg(ProgName, PPS_CRITICAL, logmsg);
		return(ER_PPS_MALLOC);
	}

	/*
	** For each matching scan record, query IMS for StateVector, TCE, and GHA
	*/
	for ( db_rec = (DB_RECORD **) FIRST(llistptr, ptr);
	      db_rec;
	      db_rec = (DB_RECORD **) NEXT(llistptr, ptr) )
	{
		/* 
		** copy the data into the rec working space 
		*/
		rec->order_id = *(DBINT *) db_rec[SCAN_ORDER_ORDER_ID];
		rec->item_id  = *(DBINT *) db_rec[SCAN_ORDER_ITEM_ID];
		rec->job_id   = *(DBINT *) db_rec[SCAN_ORDER_JOB_ID];
		PPS_STRNCPY(rec->platform, 
				(char *) db_rec[SCAN_ORDER_PLATFORM],
				PLATFORM_STRLEN);
		PPS_STRNCPY(rec->sensor,
                                (char *)db_rec[SCAN_ORDER_SENSOR],
				SENSOR_STRLEN);
		PPS_STRNCPY(rec->sv.precision, 
				(char *) db_rec[SCAN_ORDER_SV_TYPE],
				SVEC_TYPE_STRLEN);
		PPS_STRNCPY(time_str,
                                (char *)db_rec[SCAN_ORDER_START_TIME],
				sizeof(time_str));
		time_str[sizeof(time_str) - 1] = '\0';
		convert_date_string( time_str, &(rec->start_time));
		PPS_STRNCPY(time_str,
                                (char *)db_rec[SCAN_ORDER_END_TIME],
				sizeof(time_str));
		time_str[sizeof(time_str) - 1] = '\0';
		convert_date_string( time_str, &(rec->end_time));
		rec->comment[0] = '\0';

#ifdef DEBUG
		printf("Scan job [order=%d, item=%d, job=%d, startime=%s, "	
			"endtime=%s] matched this state vector\n",
			rec->order_id, rec->item_id, rec->job_id,
			rec->start_time, rec->end_time);
#endif
       		ret_code_gha = wrap_ims_ghaRangeQuery( ims_query, 
			rec->start_time.time_string, 
			rec->end_time.time_string, &(rec->gha));
		if (ret_code_gha != ER_NO_ERROR)
		{
			(void)sprintf(logmsg, "%s [order=%d, item=%d,"
				" start=%s, end=%s]",
				pps_err_msgs[ret_code_gha],
				rec->order_id, rec->item_id,
				rec->start_time.time_string,
				rec->end_time.time_string);
			if (ret_code_gha == ER_IMS_GHA_QUERY_NODATA)
				pps_logMsg(ProgName, PPS_INFO, logmsg);
			else
				pps_logMsg(ProgName, PPS_ERROR, logmsg);
			continue; /* go on to the next job in the list */
		}

		/* determine the type of state vector needed */
		if (! strcmp (rec->quicklook_flag, "YES"))
			strcpy(precision, "PREDICTED");
		else
			strcpy(precision, "RESTITUTED");

		ret_code_sv = wrap_ims_svQuery( ims_query,
			rec->platform, precision, 
			rec->start_time.time_string, 
			rec->end_time.time_string, 
			&(rec->sv));
		if (ret_code_sv != ER_NO_ERROR)
		{
			if (ret_code_sv != ER_IMS_SV_QUERY_NODATA)
			{
	                        (void)sprintf(logmsg, "%s [order=%d, item=%d,"
       		                         " start=%s, end=%s]",
               		                 pps_err_msgs[ret_code_sv],
                       		         rec->order_id, rec->item_id,
                               		 rec->start_time.time_string,
                                	 rec->end_time.time_string);
				pps_logMsg(ProgName, PPS_ERROR, logmsg);
				continue; /* go to next job */
			}
                        /* for QLK Scans, try Restitued State Vectors
                         * if Predicted ones are not available
                         */
			if (!strcmp (rec->quicklook_flag, "YES"))
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
					rec->platform, "RESTITUTED", 
					rec->start_time.time_string, 
					rec->end_time.time_string, 
					&(rec->sv));
				if (ret_code_sv != ER_NO_ERROR)
				{
					(void)sprintf(logmsg, "%s [order=%d, "
						"item=%d, start=%s,end=%s]",
						pps_err_msgs[ret_code_sv],
						rec->order_id, rec->item_id,
						rec->start_time.time_string,
						rec->end_time.time_string);
					if (ret_code_sv == ER_IMS_SV_QUERY_NODATA)
					{
						pps_logMsg(ProgName, PPS_INFO, logmsg);
					}
					else
					{
						pps_logMsg(ProgName, PPS_ERROR, logmsg);
					}
					/* go on to the next job on the list */
					continue;
				}
			}
			else /* not a quicklook job */
			{
                                (void)sprintf(logmsg, "%s [order=%d, item=%d,"
                                         " start=%s, end=%s]",
                                         pps_err_msgs[ret_code_sv],
                                         rec->order_id, rec->item_id,
                                         rec->start_time.time_string,
                                         rec->end_time.time_string);
                                pps_logMsg(ProgName, PPS_INFO, logmsg);
                                continue; /* go on to the next job in the list */
			}
		}

		/* TCE not needed for R1 */
		if (! strcmp(rec->platform, R1_SATNAME))
		{
			ret_code_tce = ER_NO_ERROR;
			rec->tc.rev = 0;
		}
		else
		{
			ret_code_tce = wrap_ims_tceQuery( ims_query, 
			rec->start_time.time_string, 
			rec->end_time.time_string, 
			rec->platform, 
			&(rec->tc));
		}
		if (ret_code_tce != ER_NO_ERROR)
		{
			(void)sprintf(logmsg, "%s [order=%d, item=%d,"
				" platform=%s, start=%s, end=%s]",
				pps_err_msgs[ret_code_tce],
				rec->order_id, rec->item_id,rec->platform,
				rec->start_time.time_string,
				rec->end_time.time_string);
			if (ret_code_tce == ER_IMS_TCE_QUERY_NODATA)
			{
				pps_logMsg(ProgName, PPS_INFO, logmsg);
			}
			else
			{
				pps_logMsg(ProgName, PPS_ERROR, logmsg);
			}
			continue;
		}

		/* initialize query strings */
		col_names[0] = '\0';
		col_values[0] = '\0';

        	/* store queried parameters */
 
                sprintf(buf,"gha_time=\"%s\",gha_angle=%f",
                	rec->gha.time,rec->gha.angle);
                strcat(col_values, buf);

        	if ( rec->tc.rev > 0 )
        	{
                	sprintf(buf,
			",tce_rev=%d,tce_sat_time=%d,tce_clock_cycle=%d,tce_time=\"%s\"",
                        	rec->tc.rev,rec->tc.sat_time,
                        	rec->tc.clock_cycle,rec->tc.time);
                	strcat(col_values, buf);
        	}
        	if ( rec->sv.rev > 0 )
        	{
                	sprintf(buf,",sv_type=\"%s\",sv_rev=%d,sv_time=\"%s\",",
                           rec->sv.precision,rec->sv.rev,rec->sv.time);
                	strcat(col_values, buf);

                	sprintf(buf,"sv_x_pos=%f,sv_y_pos=%f,sv_z_pos=%f,",
                           rec->sv.x_pos,rec->sv.y_pos,rec->sv.z_pos);
                	strcat(col_values, buf);

                	sprintf(buf,
                           "sv_x_velocity=%f,sv_y_velocity=%f,sv_z_velocity=%f",
                           rec->sv.x_vel,rec->sv.y_vel,rec->sv.z_vel);
                	strcat(col_values, buf);

			sprintf(buf,
                           ",sv_coord_sys=\"%s\"", rec->sv.coord_sys);
                	strcat(col_values, buf);
	 
        	}
		/* start a transaction */ 
                pps_start_xact(dbproc_server,dbproc_commit,&commid,STVEC_KEYWD);
	
        	/* update the entry in scan_orders table */
        	sprintf(buf, "update %s set %s where job_id = %d",
                	SCAN_ORDERS_TABLENAME, 
			col_values,
			rec->job_id);
		
		/* when executing sql commands, check nrecs */
        	db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

		CHECK_DEADLOCK_AND_CLEANUP(errormsg);

        	if (nrecs != 1)
        	{
			abortall(dbproc_server, dbproc_commit, commid);
			(void)sprintf(logmsg, "%s [job=%d]",
				pps_err_msgs[ER_UPDATE_SCAN], rec->job_id);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
			free(rec);
			DEL_LIST( llistptr );
			return(ER_UPDATE_SCAN);
        	}

		/* all queries were successful so
		 * set job state to READY 
		 */
		strcpy(job_state, READY);

        	/* update the job state */
        	sprintf(buf, "sp_update_job_state %d , \"%s\"",
                	rec->job_id, job_state);
		/* when executing stored procedure, check ret */
        	db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

		CHECK_DEADLOCK_AND_CLEANUP(errormsg);

        	if (ret != 1)
		{
			abortall(dbproc_server, dbproc_commit, commid);
			(void)sprintf(logmsg, "%s [job=%d, state=%s]",
				pps_err_msgs[ER_UPDATE_JOBSTATE],
				rec->job_id, job_state);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
			continue;
		}

#ifdef DEBUG
		printf("Scan job [order=%d, item=%d, job=%d] is ready\n",
                                rec->order_id, rec->item_id, rec->job_id);
#endif

                /* execute stored procedure to make the */
                /* job available if policy table indicates so */
                sprintf(buf, "sp_auto_avail %d", rec->job_id);
                db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
		CHECK_DEADLOCK_AND_CLEANUP(errormsg);

                if (ret == 1)
		{
#ifdef DEBUG
                       	printf("Scan job [order=%d, item=%d, job=%d is now available\n", 
				rec->order_id, rec->item_id, rec->job_id);
#endif
			strcpy(job_state, AVAILABLE);
		}
                else if (ret < 0)
                {
			/* job stays at "READY" state */
			(void)sprintf(logmsg, "%s [job=%d]",
				pps_err_msgs[ER_SCAN_AVAIL_FAIL],
				rec->job_id);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
                }

		/* commit the transaction*/
		pps_commit_xact(dbproc_server, dbproc_commit, commid);
	
		/* create order status to be sent to IMS */
		fill_scanorder_status (rec, &ims_status, INTERMEDIATE, 
			job_state, NULL, NULL);
	
        	/* create buffer containing order status */
       		create_order_status_buf_mx( &ims_status, buf, sizeof(buf));
 
       		/* send order status to IMS */
       		ret = wrap_ims_orderStatus ( ims_query, buf);
	        if (ret != ER_NO_ERROR)
        	{
			(void)sprintf(logmsg,
				"%s [order=%s, item=%s, job=%d, status=%s]",
                        	"Failed to send order status to IMS",
                        	ims_status.order_id, ims_status.item_id,
				rec->job_id,	
                        	ims_status.status_id);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
        	}

	}

	/* free the working space */
	free (rec);
	DEL_LIST( llistptr );

	return(ER_NO_ERROR);

} /* process_IMS_SvAvail */

/* End of File */
