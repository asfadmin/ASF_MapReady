/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_cp_scancancel.c

Description:
	This module contains the routines used for processing CP scan cancel requests. 

External Functions:
	process_cp_scancancel
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_cp_scancancel.c	1.3    04/23/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "db_l1_order.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"
#include "db_scan_order.h"

extern COLUMN_DEFS      scan_order_columns_[];

/*==============================================================================
Description:	
	This function processes an CP Scan Cancel Request.

Returns:	PPS  error_code

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/
#ifdef __STDC__
int process_CP_ScanCancel( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
	CP_JobStatus_Record 	*cp_status
	)
#else
int process_CP_ScanCancel(dbproc_server, dbproc_commit, ims_query, cp_status)
	DBPROCESS		*dbproc_server, 
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
   	CP_JobStatus_Record 	*cp_status
	)
#endif
{
        int 	          ret = 0, nrecs = 0;
        char              buf[200];          
        char              imstatbuf[2048];          
        char              where_clause[100];
        DB_RECORD         **db_rec ;
        IMS_Order_Status  ims_status;
        llist             *llistptr = NULL;
        cursor            ptr ;
	char    	  errormsg[MAXLINE];
	int		  keep_job = FALSE;

	/* construct the error message for reporting deadlock error */
	(void)sprintf(errormsg, " %s : %s [job_id = %d]",
                        pps_err_msgs[ER_DEADLOCK], "process_CP_ScanCancel",
                        cp_status->job_id);

        /* Update the job final status and info reported from CP */
        sprintf (buf, "sp_update_status_from_cp %d, '%s', '%s','%s','%s'",
                        cp_status->job_id, cp_status->status,
                        cp_status->dataset, cp_status->product_filename,
                        cp_status->comment);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
        CHECK_DEADLOCK (errormsg);
 
        /* if failed to update job state because the job is already 
		   in one of the final states, report the error and return
           error status code to CP */
        if (ret != 1)
        {
                (void)sprintf(buf, "%s [job_id=%d, status=%s]",
                               pps_err_msgs[ER_UPDATE_JOBSTATE],
                                cp_status->job_id,
                                cp_status->status);
                pps_logMsg(ProgName, PPS_ERROR, buf);
                return (ER_UPDATE_JOBSTATE);
 
        }

	/* for each matching row in scan orders table,
	**    report scan order completion to IMS
	**    remove row from table
	*/
        sprintf(where_clause, "where job_id = %d", cp_status->job_id);
        llistptr = db_get_records(dbproc_server, SCAN_ORDERS_TABLENAME,
                where_clause, NULL, scan_order_columns_, ALL_COLS);
        if (NUMELTS(llistptr) > 0)
        {
                db_rec = (DB_RECORD **) FIRST( llistptr, ptr );
        	/* create order status to be sent to IMS */

		/* fill in fields retreived from the database */
		sprintf(ims_status.order_id, "%d",
			*(DBINT *)db_rec[SCAN_ORDER_ORDER_ID] );
		sprintf(ims_status.item_id, "%d", 
			*(DBINT *)db_rec[SCAN_ORDER_ITEM_ID] );

		/* fill in info from CP status rec */
		strcpy(ims_status.dataset, cp_status->dataset);
 		strcpy(ims_status.product_filename, 
			cp_status->product_filename);
		strcpy(ims_status.platform, cp_status->platform);
		strcpy(ims_status.sensor, cp_status->sensor);
		strcpy(ims_status.comment, cp_status->comment) ;
		/* TODO concat comment from jobs table too*/

		/* fill in status info */
        	strcpy(ims_status.status_type, FINAL);

		/* CP status -> IMS status */
		strcpy(ims_status.status_id, CANCELFAIL);

       		/* create buffer containing order status */
       		create_order_status_buf_mx(&ims_status, imstatbuf, sizeof(imstatbuf));
 
        	/* send order status to IMS */
#ifdef DEBUG
		printf("Sending Order Status to IMS: "
                               "job_id=%d, order_id=%s, "
                               "item_id=%s, status=%s\n",
                               cp_status->job_id,
                               ims_status.order_id,
                               ims_status.item_id,
                               ims_status.status_id);
#endif

        	ret = wrap_ims_orderStatus(ims_query, imstatbuf);
	        if (ret != ER_NO_ERROR)
        	{
                	sprintf(buf,"%s [order_id=%s,item_id=%s,job_id=%d,status=%s]",
                        	"Failed to report order status to IMS",	
                        	ims_status.order_id,ims_status.item_id,
                        	cp_status->job_id,ims_status.status_id
                        	);
                	pps_logMsg(ProgName, PPS_ERROR, buf);
			/* set flag to keep the job data in the DB */
			keep_job = TRUE;
        	}
                DEL_LIST( llistptr );
        }
	else
        {
		DEL_LIST (llistptr);
		(void)sprintf(buf, "%s [job=%d]",
                       pps_err_msgs[ER_FIND_SCAN_ORDER], cp_status->job_id);
                pps_logMsg(ProgName, PPS_ERROR, buf);
                return ( ER_FIND_SCAN_ORDER );
        }

	/* delete job from the database if IMS was successfully notified */
	if (! keep_job)
	{
		sprintf (buf, "sp_remove_scan %d", cp_status->job_id);
		db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

		/* if the cmd was dropped due to deadlock or the cmd did not
		   execute successfully, log the error.  The recovery deamon
   		   will cleanup later */

	        if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE)
       		{
			(void)sprintf(buf,"%s : 'process_CP_ScanCancel' : %s",
				pps_err_msgs[ER_DEADLOCK], buf);
			pps_logMsg(ProgName, PPS_ERROR, buf);
		}
		else if (ret != 1)
		{
			(void)sprintf(buf, "%s [job = %d]",
	                       pps_err_msgs[ER_REMOVE_SCAN_COMPLETE],
	                       cp_status->job_id);
	                pps_logMsg(ProgName, PPS_ERROR, buf);
		}
	}


	/* return good status to CP */
	return( ER_NO_ERROR );

} /* process_CP_ScanCancel */
