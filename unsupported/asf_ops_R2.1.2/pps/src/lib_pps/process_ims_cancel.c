/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	process_ims_cancel.c

Description:
	This module contains the routines used for processing IMS Cancel requests. 

External Functions:
	process_IMS_CancelReq
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)process_ims_cancel.c	1.1    11/21/96";

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


/*==============================================================================
Function:	
	int process_IMS_CancelReq(DBPROCESS *dbproc_server,
		DBPROCESS 		*dbproc_commit,
		IMS_CMN_QUERY 		*ims_query,
		IMS_CancelReq_Record 	*rec)

Description:	
	This function processes an IMS Scan Request.

Parameters:
	dbproc_server	connection to the db server
	dbproc_commit	connection to the commit service
	ims_query	connection to the IMS db server
	rec		extracted data

Returns:	PPS  error_code

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/
#ifdef __STDC__
int process_IMS_CancelReq( DBPROCESS *dbproc_server,
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		*ims_query,
	IMS_CancelReq_Record 	*rec)
#else
int process_IMS_CancelReq(dbproc_server, dbproc_commit, ims_query, 
		rec)
	DBPROCESS		*dbproc_server, 
	DBPROCESS		*dbproc_commit, 
	IMS_CMN_QUERY		*ims_query,
   	IMS_CancelReq_Record 	*rec)
#endif
{
        int 	ret = 0, nrecs = 0;
        char 	buf[80] ;
	int	commid; 	/* commit service id */
	int	job_id = 0; 	
	char	errormsg[MAXLINE];

        /* construct the error message for reporting deadlock error */
        (void)sprintf(errormsg, " %s : %s %s [order_id = %d, item_id = %d]",
                        pps_err_msgs[ER_DEADLOCK], "process_IMS_CancelReq",
                        rec->request_type, rec->order_id, rec->item_id);

	pps_start_xact(dbproc_server, dbproc_commit, &commid, CANCEL_KEYWD);

	if (! strcmp (rec->request_type, SCAN_KEYWD))
	{
		 /* check if job exists in the scan orders table */
		sprintf (buf, "sp_get_scan_order_jobid %d , %d ", 
			rec->order_id, rec->item_id);
		db_exec_cmd (dbproc_server, buf, &job_id, &nrecs);
		if (job_id <= 0)
		{
			(void)sprintf(buf,
                               "Cannot find scan order [order=%d, item=%d]",
                               rec->order_id, rec->item_id);
           		pps_logMsg(ProgName, PPS_ERROR, buf);
			abortall(dbproc_server, dbproc_commit, commid);
			return(ER_FIND_SCAN_ORDER);
		}

		/* search the schedule table and see if job has 
		   already been submitted, returns 1 if submitted */
		sprintf (buf, "sp_is_job_submitted %d ", job_id);
		db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
		if (ret == 1)
		{
			(void)sprintf(buf,
                               "Scan Job has been submitted [job=%d]",
                               job_id);
           		pps_logMsg(ProgName, PPS_INFO, buf);
			abortall(dbproc_server, dbproc_commit, commid);
			return(ER_JOB_SUBMITTED);
		}

		/* log the cancelled order data in syslog */
		(void)sprintf (buf, "Scan order [order=%d, item=%d, job=%d] cancelled by IMS",
			rec->order_id, rec->item_id, job_id);
		pps_logMsg(ProgName, PPS_INFO, buf);	

		/* delete job from all tables */
		sprintf (buf, "sp_remove_scan %d ", job_id);
		db_exec_cmd(dbproc_server, buf, &ret, &nrecs);
		CHECK_DEADLOCK(errormsg);
	}
	else if (! strcmp (rec->request_type, FRAME_KEYWD))
	{
		 /* check if job exists in the L1 orders table */
		sprintf (buf, "sp_get_l1_order_jobid %d , %d", 
			rec->order_id, rec->item_id);
		db_exec_cmd (dbproc_server, buf, &job_id, &nrecs);
		if (job_id <= 0)
		{
			(void)sprintf(buf,
                               "Cannot find L1 order [order=%d, item=%d]",
                               rec->order_id, rec->item_id);
           		pps_logMsg(ProgName, PPS_ERROR, buf);
			abortall(dbproc_server, dbproc_commit, commid);
			return(ER_FIND_L1_ORDER);
		}

		/* search the schedule table to see if job has 
		   already been submitted. returns 1 if submitted */
		sprintf (buf, "sp_is_job_submitted %d ", job_id);
		db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
		if (ret == 1)
		{
			(void)sprintf(buf,
                               "L1 Job has been submitted [job=%d]",
                               job_id);
           		pps_logMsg(ProgName, PPS_INFO, buf);
			abortall(dbproc_server, dbproc_commit, commid);
			return(ER_JOB_SUBMITTED);
		}

                /* log the cancelled order data in syslog */
                (void)sprintf (buf, "L1 order [order=%d, item=%d, job=%d] cancelled by IMS",
                        rec->order_id, rec->item_id, job_id);
                pps_logMsg(ProgName, PPS_INFO, buf);    

		/* if matching L1 orders are found, delete from L1 order
			table only, otherwise, delete from all tables */ 
		sprintf (buf, "sp_remove_l1_cancel %d, %d, %d", 
			rec->order_id, rec->item_id, job_id);
		db_exec_cmd(dbproc_server, buf, &ret, &nrecs);
		CHECK_DEADLOCK(errormsg);
	}
	else
	{
		(void)sprintf(buf,
		       "Invalid request_type[%s]", rec->request_type);
		pps_logMsg(ProgName, PPS_ERROR, buf);
		abortall(dbproc_server, dbproc_commit, commid);
		return(ER_PROCESS_CANCEL);
	}
 
	/* commit transaction */
	pps_commit_xact(dbproc_server, dbproc_commit, commid);

	return(ER_NO_ERROR);

} /* process_IMS_CancelReq */

/* End of File */
