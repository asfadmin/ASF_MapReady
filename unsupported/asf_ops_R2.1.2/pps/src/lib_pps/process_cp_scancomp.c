/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_cp_scancomp.c

Description:
	This module contains the routines used for processing IMS scan requests. 

External Functions:
	process_CP_ScanComplete
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_cp_scancomp.c	1.4    04/23/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"
#include "db_jobs.h"
#include "db_scan_order.h"
#include "db_l1_procparms.h"
#include "db_l1_order.h"


extern COLUMN_DEFS      scan_order_columns_[];
extern COLUMN_DEFS      jobs_columns_[];
extern COLUMN_DEFS      l1_procparms_columns_[];
extern COLUMN_DEFS	l1_order_columns_[];

#define REPORT_IF_DEADLOCK(errormsg) \
        if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE) \
        { \
                pps_logMsg(ProgName, PPS_ERROR, errormsg); \
		DEL_LIST(L1_listptr); \
                continue; \
        } \

/*==============================================================================
Description:	
	This function processes a CP Scan Complete Request.

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
int process_CP_ScanComplete( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
	CP_JobStatus_Record 	*cp_jobstat)
#else
int process_CP_ScanComplete(dbproc_server, dbproc_commit, ims_query, cp_jobstat)
	DBPROCESS		*dbproc_server, 
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
   	CP_JobStatus_Record 	*cp_jobstat)
#endif
{
        int 			i, ret = 0, nrecs = 0;
	int			commid;
	int			ret_code_cal, ret_code_scan;
	int			frame_count = 0;
	int			L1_job_id;
        char 			buf[1000];
        char 			cal_params_file1[CAL_PARAMS_FILE_STRLEN+1] ;
	char                    cal_params_file2[CAL_PARAMS_FILE_STRLEN+1] ;
        char 			scan_results_file[SCAN_RESULTS_FILE_STRLEN+1] ;
        char 			job_state[20] ;
	char			imstatbuf[2048];
        llist 			*llistptr = NULL;
	llist			*L1_listptr = NULL;
        cursor			ptr, L1_ptr ;
	DB_RECORD       	**db_rec, **L1_db_rec ;
	IMS_ScanReq_Record      rec;
	IMS_L1PReq_Record       L1_rec;
	IMS_FRAME_STRUCT	*frameptr, *frameList;
        IMS_Order_Status  	ims_status;
	char    		errormsg[MAXLINE];


        /* construct the error message for reporting deadlock error */
        (void)sprintf(errormsg, " %s : %s [job_id = %d]",
                        pps_err_msgs[ER_DEADLOCK], "process_CP_ScanComplete",
                        cp_jobstat->job_id);

        /* Update the job final status and info reported from CP */
        sprintf (buf, "sp_update_status_from_cp %d, '%s', '%s','%s','%s'",
                        cp_jobstat->job_id, cp_jobstat->status,
                        cp_jobstat->dataset, cp_jobstat->product_filename,
			cp_jobstat->comment);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
        CHECK_DEADLOCK (errormsg);
 
        /* if failed to update job state because the job is already 
	       in one of the final states, report the error and return
           error status code to CP */
        if (ret != 1)
        {
                (void)sprintf(buf, "%s [job_id=%d, status=%s]",
                               pps_err_msgs[ER_UPDATE_JOBSTATE],
                                cp_jobstat->job_id,
                                cp_jobstat->status);
                pps_logMsg(ProgName, PPS_ERROR, buf);
                return (ER_UPDATE_JOBSTATE);
        }
 
	/* find the corresponding scan request */
        sprintf(buf, "where job_id = %d", cp_jobstat->job_id);
        llistptr = db_get_records(dbproc_server, SCAN_ORDERS_TABLENAME,
                buf, NULL, scan_order_columns_, ALL_COLS);
        if (NUMELTS(llistptr) > 0)
        {
                db_rec = (DB_RECORD **) FIRST( llistptr, ptr );
                bind_dbrec_to_scan(db_rec, &rec);
                DEL_LIST( llistptr );
        }
	else
	{
                DEL_LIST( llistptr );
		sprintf(buf,"%s [job = %d]", pps_err_msgs[ER_FIND_SCAN_ORDER],
			cp_jobstat->job_id);
                pps_logMsg(ProgName, PPS_ERROR, buf);
		return (ER_FIND_SCAN_ORDER);
	}
		

	/* query IMS for frame_ids generated by this scan job */
	ret = wrap_ims_frameQuery (ims_query, rec.platform, rec.rev, 
		rec.datatake_seq, rec.media_id, rec.mode, rec.frame_mode,
		&frame_count );
	if (ret != ER_NO_ERROR) 
        {
                sprintf(buf,"%s [order=%d, item=%d, job = %d]",
                        pps_err_msgs[ret], rec.order_id, rec.item_id,
                        cp_jobstat->job_id);
		if (ret == ER_IMS_FRAME_QUERY_NODATA) 
		{
			pps_logMsg(ProgName, PPS_INFO, buf);
		}
		else
		{
			pps_logMsg(ProgName, PPS_ERROR, buf);
		}
        }
	else if (frame_count <= 0)
	{
	        /* ret == ER_NO_ERROR but no frames returned -
	        ** this case should not happen, so we'll log it to syslog */
		sprintf(buf,"No frame ids returned from ims_frameQuery "
		 	"[order=%d, item=%d, job = %d]",
			rec.order_id, rec.item_id, cp_jobstat->job_id);
		pps_logMsg(ProgName, PPS_WARNING, buf);	
	}

	frameList = (IMS_FRAME_STRUCT *)(*ims_query)->retPtr;

	/* query L1 proc parms table and jobs table for pending L1
	** jobs waiting for the data made available by this scan job
	*/

        for   (	frameptr = frameList;
		frameptr != (IMS_FRAME_STRUCT *)NULL ;
		frameptr = frameptr->next )
        {
        	sprintf(buf, 
		"where platform=\"%s\" and sensor=\"%s\" and rev=%d and frame_id=%d and frame_mode=\"%s\"",
		rec.platform, rec.sensor, rec.rev, frameptr->frame_id,
		rec.frame_mode);

		/* QLK  Scans match QLK L1 Jobs
		 * NQLK Scans match any L1 Job
		 */
		if (! strcmp(rec.quicklook_flag, "YES"))
			strcat (buf, " and quicklook_flag=\"YES\"");

        	llistptr = db_get_records(dbproc_server, L1_PROC_PARMS_TABLENAME,
                	buf, NULL, l1_procparms_columns_, ALL_COLS);
		if (NUMELTS(llistptr) == 0)
		{
			DEL_LIST (llistptr);
#ifdef DEBUG
			printf("No matching jobs found for frame_id %d\n",
				frameptr->frame_id);
#endif
			continue;
		}
        	for ( 	db_rec = (DB_RECORD **) FIRST(llistptr, ptr);
			db_rec ; 
			db_rec = (DB_RECORD **) NEXT(llistptr, ptr) )
        	{

			L1_job_id = *(DBINT *) db_rec[L1_PROCPARMS_JOB_ID];

	                /* search the jobs table to see if this L1 job
                   	   is still pending (0 is returned if not) */
                	sprintf (buf, "sp_is_job_pending %d ", L1_job_id);
                	db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

                        /* skip the following processing if the L1 job is
                                not in pending state */

                	if (ret == 0)
                	{
				continue;
                	}

                        /* Query IMS for Calibration Parameter Filename */
                        ret_code_cal = wrap_ims_calParamQuery( ims_query,
                                rec.platform,
                                rec.mode,
                                frameptr->center_time,
                                cal_params_file1,
				cal_params_file2);
                        if (ret_code_cal != ER_NO_ERROR)
                        {
                                sprintf(buf,"%s [L1 job=%d, platform=%s,"
					" mode=%s, centertime=%s]",
                                        pps_err_msgs[ret_code_cal],
                                        L1_job_id, rec.platform,
					rec.mode, frameptr->center_time);
				if (ret_code_cal == ER_IMS_CAL_QUERY_NODATA)
				{
                                	pps_logMsg(ProgName, PPS_INFO, buf);
				}
				else
				{
					pps_logMsg(ProgName, PPS_ERROR, buf);
				}
                                continue;
                        }
                        /* Query IMS for Scan Results Filename */
                        ret_code_scan = wrap_ims_scanQuery( ims_query,
                                rec.platform,
                                rec.rev,
                                rec.datatake_seq,
                                rec.media_id,
                                rec.mode,
                                scan_results_file);
                        if (ret_code_scan != ER_NO_ERROR)
                        {
                                sprintf(buf,"%s [L1 job = %d]",
                                        pps_err_msgs[ret_code_scan],
                                        L1_job_id);
				if (ret_code_scan == ER_IMS_SCAN_QUERY_NODATA)
                                {
                                        pps_logMsg(ProgName, PPS_INFO, buf);
                                }
                                else
                                {
                                        pps_logMsg(ProgName, PPS_ERROR, buf);
                                }
                                continue;
                        }

			sprintf(errormsg,"%s : %s [scan_job_id = %d,\
				 L1_job_id = %d]",
				 pps_err_msgs[ER_DEADLOCK],
				 "process_CP_ScanComplete", 
				 cp_jobstat->job_id, L1_job_id);  
			/* start a transaction */
			pps_start_xact(dbproc_server,dbproc_commit,&commid,
				CP_SCAN_COMP_KEYWD);

			/* copy STV,TCE,GHA info from Scan job to L1 
		           and frame info returned by the IMS frame query */
			sprintf(buf, "sp_update_L1_using_scan %d , %d, \
				'%s', '%s', '%s', '%s', '%s','%s','%s','%s'",
				L1_job_id, cp_jobstat->job_id,
				cal_params_file1, cal_params_file2, 
				scan_results_file,
				frameptr->station_id,
				frameptr->start_time,
				frameptr->end_time,
				frameptr->center_time,
                                frameptr->data_direction);
                	db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
			REPORT_IF_DEADLOCK (errormsg);
                	if (ret != 0)
                	{
				sprintf(buf,"%s [job = %d]",
                                        pps_err_msgs[ER_UPDATE_L1_USING_SCAN],
		                        cp_jobstat->job_id);
                		pps_logMsg(ProgName, PPS_ERROR, buf);
				abortall(dbproc_server, dbproc_commit, commid);
				continue;
			}

			
			/* all info is available -> job is ready */
			strcpy(job_state, READY);

		        /* update job_state */
        		sprintf (buf, "sp_update_job_state %d , \"%s\"", 
				L1_job_id, job_state);
        		db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
			REPORT_IF_DEADLOCK(errormsg);
	                if (ret != 1)
			{
				sprintf(buf,"%s [job = %d]",
                                        pps_err_msgs[ER_UPDATE_JOBSTATE],
		                        L1_job_id);
                		pps_logMsg(ProgName, PPS_ERROR, buf);
				abortall(dbproc_server, dbproc_commit, commid);
				continue;
			}


			/* check policy table to see if job has
			 * to become available for processing  
			 */
		        sprintf(buf, "sp_auto_avail %d", L1_job_id);
                	db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
			REPORT_IF_DEADLOCK(errormsg);
                	if (ret == 1)
                	{
#ifdef DEBUG
                        	printf("L1 job_id %d is available\n", L1_job_id);
#endif
                        	strcpy(job_state, AVAILABLE);
                	}
			else if (ret < 0)
                	{
                        	sprintf(buf,"%s [job = %d]",
					pps_err_msgs[ER_L1_AVAIL_FAIL],
                               		L1_job_id);
                		pps_logMsg(ProgName, PPS_ERROR, buf);
                	} 
			/* if ret == 0 ; job remains in READY state */

			/* commit transaction */
			pps_commit_xact(dbproc_server, dbproc_commit, commid);

		        /* find the order_id and item_id corresponding to
			 * this L1 job in the L1 Orders table */
        		sprintf(buf, "where job_id = %d", L1_job_id);
        		L1_listptr = db_get_records(dbproc_server, 
				L1_ORDERS_TABLENAME, buf, NULL, 
				l1_order_columns_, ALL_COLS);
		        if ( NUMELTS(L1_listptr) == 0 )
        		{
				/* unable to get the order_id and item_id,
				 * log the error */
                		(void)sprintf(buf, "%s [job=%d]",
                                pps_err_msgs[ER_FIND_L1_ORDER], L1_job_id);
                		pps_logMsg(ProgName, PPS_ERROR, buf);
        		}
			else
			{
        			L1_db_rec = (DB_RECORD **) FIRST( L1_listptr, L1_ptr );
		        	L1_rec.order_id = *(DBINT *) L1_db_rec[L1_ORDER_ORDER_ID];
		        	L1_rec.item_id  = *(DBINT *) L1_db_rec[L1_ORDER_ITEM_ID];
				strcpy(L1_rec.platform,rec.platform); 
				strcpy(L1_rec.sensor,rec.sensor);
				L1_rec.comment[0] = '\0';	
				
				/* create order status to be sent to IMS */
        			fill_L1order_status (L1_rec, &ims_status, 
					INTERMEDIATE, job_state, NULL, NULL);
 
        			/* create buffer containing order status */
        			create_order_status_buf_mx( &ims_status, imstatbuf, 
					sizeof(imstatbuf));
 
        			/* send order status to IMS */
#ifdef DEBUG
				printf("Sending Order Status to IMS: "
					"job_id=%d, order_id=%s, "
                                        "item_id=%s, status=%s\n",
                                        L1_job_id,
                                        ims_status.order_id,
                                        ims_status.item_id,
                                       	ims_status.status_id);
#endif

        			ret = wrap_ims_orderStatus (ims_query, imstatbuf);
        			if (ret != ER_NO_ERROR)
        			{
                             		sprintf(buf,"%s[order_id=%s,item_id=%s,job_id=%d,status=%s]",
                                  	"Failed to send order status to IMS",
                                  	ims_status.order_id,ims_status.item_id,
					L1_job_id,
                                  	ims_status.status_id);
                             		pps_logMsg(ProgName, PPS_ERROR, buf);
				}
        		}
			DEL_LIST( L1_listptr );
		}
                DEL_LIST( llistptr );
	}
	
	/* free the linked list returned by the IMS Frame Query */
	if (frameList)
		ims_frameFree (frameList);  

        /* create order status to be sent to IMS */
        fill_scanorder_status (rec, &ims_status, FINAL, COMPLETED,
                cp_jobstat->dataset,  cp_jobstat->product_filename);
 
        /* create buffer containing order status */
	strcpy(ims_status.comment,cp_jobstat->comment);
        create_order_status_buf_mx(&ims_status, imstatbuf, sizeof(imstatbuf));
 
        /* send order status to IMS */
#ifdef DEBUG
	printf("Sending Order Status to IMS: "
                       "job_id=%d, order_id=%s, "
                       "item_id=%s, status=%s\n",
                       cp_jobstat->job_id,
                       ims_status.order_id,
                       ims_status.item_id,
                       ims_status.status_id);
#endif

        ret = wrap_ims_orderStatus(ims_query, imstatbuf);
	/* if failed to send ims order status, return to CP now,
 	 	don't remove the job from the DB */
        if (ret != ER_NO_ERROR)
        {
                sprintf(buf,"%s [order_id=%s,item_id=%s,job_id=%d,status=%s]",
                        "Failed to send order status to IMS",
                        ims_status.order_id,ims_status.item_id,
			cp_jobstat->job_id,
                        ims_status.status_id);
                pps_logMsg(ProgName, PPS_ERROR, buf);
		return (ER_NO_ERROR);
        }

        /* delete job from schedule table, delete job from jobs table
        ** and delete corresponding row from Scan Orders table
        */
        sprintf (buf, "sp_remove_scan %d", cp_jobstat->job_id);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

	/* if the cmd was dropped due to deadlock, or the cmd did not
	   execute successfully, just log the error.  The recovery program
	   will cleanup later */
	if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE)
        {
        	(void)sprintf(errormsg, " %s : %s [job_id = %d]",
                        pps_err_msgs[ER_DEADLOCK], "process_CP_ScanComplete",
                        cp_jobstat->job_id);
		pps_logMsg(ProgName, PPS_ERROR, errormsg);

	} 
        if (ret != 1)
        {
                (void)sprintf(buf, "%s [job = %d]",
                         pps_err_msgs[ER_REMOVE_SCAN_COMPLETE],
                         cp_jobstat->job_id);
                pps_logMsg(ProgName, PPS_ERROR, buf);
        }
 
	return(ER_NO_ERROR);

} /* process_CP_ScanComplete */

/* End of File */
