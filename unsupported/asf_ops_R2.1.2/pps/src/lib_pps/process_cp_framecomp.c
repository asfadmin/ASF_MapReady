/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_cp_framecomp.c

Description:
	This module contains the routines used for processing IMS scan requests. 

External Functions:
	process_CP_FrameComplete
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_cp_framecomp.c	1.3    02/19/97";

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

extern COLUMN_DEFS      l1_order_columns_[];

/*============================================================================*/

#ifdef __STDC__
int process_order_final_status( 
	DBPROCESS 		*dbproc_server,
        IMS_CMN_QUERY           **ims_query,
        CP_JobStatus_Record     *cp_status,
	DB_RECORD         	**db_rec	
        )
#else
int process_order_final_status (dbproc_server, ims_query, cp_status, db_rec)
        DBPROCESS               *dbproc_server,
        IMS_CMN_QUERY           **ims_query,
        CP_JobStatus_Record     *cp_status,
	DB_RECORD		**db_rec
        )
#endif
{
        IMS_Order_Status  ims_status;
	char              imstatbuf[2048];
	int		  ret, nrecs;
	char		  buf[MAXLINE];

	/* create order status to be sent to IMS */
	/* fill in fields retreived from the database */
	sprintf(ims_status.order_id, "%d",
		*(DBINT *)db_rec[L1_ORDER_ORDER_ID]) ;
	sprintf(ims_status.item_id, "%d",
		*(DBINT *)db_rec[L1_ORDER_ITEM_ID]) ;
 
	/* fill in info from CP status rec */
	strcpy(ims_status.dataset, cp_status->dataset);
	strcpy(ims_status.product_filename,
		cp_status->product_filename);
	strcpy(ims_status.platform, cp_status->platform);
	strcpy(ims_status.sensor, cp_status->sensor);
	strcpy(ims_status.comment, cp_status->comment) ;
 
	/* fill in status info */
	strcpy(ims_status.status_type, FINAL);

	/* CP status -> IMS status */
	if (strcmp(cp_status->status, COMPLETED) == 0)
		strcpy(ims_status.status_id, COMPLETED);
	else
		strcpy(ims_status.status_id, CANCEL_KEYWD);
 
	/* create buffer containing order status */
	create_order_status_buf_mx(&ims_status, imstatbuf, sizeof(imstatbuf));

	/* send order status to IMS */
#ifdef DEBUG
	printf("Sending Order Status to IMS: "
		"job_id=%d, order_id=%s, "
                "item_id=%s, status=%s\n",
		cp_status->job_id, ims_status.order_id,
		ims_status.item_id, ims_status.status_id);
#endif

	ret = wrap_ims_orderStatus(ims_query, imstatbuf);
	if (ret != ER_NO_ERROR)
	{
		/* Failed to notify IMS of orders status, log the
                   error and return error code to the caller.
		   The caller will then keep the job data in the database
	           so it can be used to notify IMS at a later time. */

		sprintf(buf, "%s [order_id=%s,item_id=%s,job_id=%d,status=%s]",
		 	"Failed to report order status to IMS",	
			ims_status.order_id,ims_status.item_id,
			cp_status->job_id, ims_status.status_id);
		pps_logMsg(ProgName, PPS_ERROR, buf);
		return (ret);
		/* ret = ER_IMS_SEND_STATUS_FAIL or ER_IMS_ORDER_NOT_FOUND */
	}

	/* IMS has been successfully notified, delete row from L1 order table */

	sprintf(buf, "sp_remove_l1_order %s , %s",
		ims_status.order_id, ims_status.item_id);
	db_exec_cmd(dbproc_server, buf, &ret, &nrecs);
 
	/* If the cmd was dropped due to deadlock,
	   or the cmd did not execute sucessfully,
           log error and return error code to the caller (ER_DEADLOCK) or
	   (ER_REMOVE_L1_ORDER).  This error code tells the caller to
	   go ahead and delete the job data and order data (again). */
 
	if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE)
	{
		(void)sprintf(buf, " %s : 'process_CP_FrameComplete' : %s",
				pps_err_msgs[ER_DEADLOCK], buf);
		pps_logMsg(ProgName, PPS_ERROR, buf);
		return (ER_DEADLOCK);
	}

	else if (ret != 1)
	{
                (void)sprintf(buf, " %s : 'process_CP_FrameComplete' : %s",
                                pps_err_msgs[ER_REMOVE_L1_ORDER], buf);
		pps_logMsg(ProgName, PPS_ERROR, buf);
		return (ER_REMOVE_L1_ORDER);
	}
	return (ER_NO_ERROR);
}

/*==============================================================================
Description:	
	This function processes an CP Frame Complete Request.

Returns:	PPS  error_code

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/
#ifdef __STDC__
int process_CP_FrameComplete( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
	CP_JobStatus_Record 	*cp_status
	)
#else
int process_CP_FrameComplete(dbproc_server, dbproc_commit, ims_query, cp_status)
	DBPROCESS		*dbproc_server, 
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		**ims_query,
   	CP_JobStatus_Record 	*cp_status
	)
#endif
{
        int 	          ret = 0, nrecs = 0;
	int		  keep_job = FALSE;
	char		  *keywd;
        char              buf[1000];          
        llist             *llistptr = NULL;
	cursor            ptr;
        DB_RECORD         **db_rec ;
        IMS_Order_Status  ims_status;
        char    	  errormsg[MAXLINE];  /* to report deadlock error */ 
 
        /* construct the error message for reporting deadlock error */
        (void)sprintf(errormsg, " %s : %s [job_id = %d, status = %s]",
                        pps_err_msgs[ER_DEADLOCK], "process_CP_FrameComplete", 
                        cp_status->job_id, cp_status->status);


	/* this function process both messages :
	 * 	CP Frame Complete and CP Frame Cancel
	 * so pick the right keyword for registering the xact processing
	 */
	if (! strcmp(cp_status->status, COMPLETED))
		keywd = CP_FRAME_COMP_KEYWD;
	else
		keywd = CP_FRAME_CANCEL_KEYWD;

	/* Update the job final status and info reported from CP */
        sprintf (buf, "sp_update_job_final_status %d, '%s', '%s','%s','%s'", 
                        cp_status->job_id, cp_status->status,
			cp_status->dataset, cp_status->product_filename,
			cp_status->comment);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
        CHECK_DEADLOCK (errormsg);

	/* if failed to update job state because the job is not in
	   SUBMITTED state, report the error and return error status 
 	   code to CP */
	if (ret != 1)
	{
                (void)sprintf(buf, "%s [job_id=%d, status=%s]",
                               pps_err_msgs[ER_UPDATE_JOBSTATE],
                                cp_status->job_id,
				cp_status->status);
                pps_logMsg(ProgName, PPS_ERROR, buf);
		return (ER_UPDATE_JOBSTATE);
	}
	

	/* for each matching row in L1 orders table,
	**    report L1 order completion/cancellation to IMS
	**    remove row from L1_order table if succesfully notify IMS 
	*/
        sprintf(buf, "where job_id = %d", cp_status->job_id);
        llistptr = db_get_records(dbproc_server, L1_ORDERS_TABLENAME,
                buf, NULL, l1_order_columns_, ALL_COLS);
        if (NUMELTS(llistptr) > 0)
        {
                for (   db_rec = (DB_RECORD **) FIRST(llistptr, ptr);
                        db_rec ;
                        db_rec = (DB_RECORD **) NEXT(llistptr, ptr) )
                {
			ret = process_order_final_status 
				(dbproc_server, ims_query, cp_status, db_rec);
			/* If the return code indicates that IMS order status
			   message failed, set flag to keep the job data */ 
			if (ret == ER_IMS_SEND_STATUS_FAIL ||
			    ret == ER_IMS_ORDER_NOT_FOUND )
			{
				keep_job = TRUE;
			}

                }
                DEL_LIST( llistptr );
        }
	else
        {
                DEL_LIST( llistptr );
		(void)sprintf(buf,"%s [job_id = %d]",
                        pps_err_msgs[ER_FIND_L1_ORDER],
			cp_status->job_id);
               	pps_logMsg(ProgName, PPS_ERROR, buf);
                return ( ER_FIND_L1_ORDER );
        }

	/* delete all records for this job if IMS has been successfully
	   notified */ 
	if (! keep_job)
	{
		sprintf (buf, "sp_remove_l1_complete %d", cp_status->job_id);
		db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

	        /* If the cmd was dropped due to deadlock,
       		 * or the cmd did not execute sucessfully,
		 * log the error.  The job data remains in the database.
	         * The PPS recovery daemon will clean it up later. */

	        if (*((DBBOOL *) dbgetuserdata(dbproc_server)) == TRUE)
		{
	                (void)sprintf(buf, " %s : 'process_CP_FrameComplete' : %s",
				pps_err_msgs[ER_DEADLOCK], buf);
			pps_logMsg(ProgName, PPS_ERROR, buf);
		}

		else if (ret != 1)
		{
			(void)sprintf(buf, "%s [job=%d]",
        	               pps_err_msgs[ER_REMOVE_L1_COMPLETE],
				cp_status->job_id);
	          	pps_logMsg(ProgName, PPS_ERROR, buf);
		}
	}

	/* return good status to CP */
	return( ER_NO_ERROR );

} /* process_CP_FrameComplete */

/* End of File */
