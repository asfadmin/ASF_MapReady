/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       process_cp_frame.c
 
Description:
        This module contains the routines used for processing cp requests
for frame jobs.
 
External Functions:
        process_CP_FrameReq
 
Static Functions:
        None
 
External Variables Defined:
        None
 
File Scope Static Variables:
        None
Notes:
==============================================================================*/
 
static char SccsFileId[] = "@(#)process_cp_frame.c	1.2    12/16/96";
 
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <pthread.h>
#include "defs.h"
#include "PPSdefs.h"
#include "PPSextern.h"
#include "PPSerr.h"
#include "db_sybint.h"
#include "db_jobs.h"
#include "db_l1_order.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"
 
extern COLUMN_DEFS      l1_procparms_columns_[];
extern COLUMN_DEFS      jobs_columns_[];
extern COLUMN_DEFS      l1_order_columns_[];

/*==============================================================================
Description:	
	This function processes an CP Job Request.
Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/
#ifdef __STDC__
int process_CP_FrameReq( DBPROCESS *dbproc_server,
        DBPROCESS               *dbproc_commit,
        IMS_CMN_QUERY           **ims_query,
	CP_JobReq_Record 	*cp_jobreq,
	char 			*output_buf, 
	int  			*output_buf_size)
#else
int process_CP_FrameReq(dbproc_server,dbproc_commit,ims_query,rec, output_buf, output_buf_size)
	DBPROCESS 		*dbproc_server;
        DBPROCESS               *dbproc_commit;
        IMS_CMN_QUERY           **ims_query;
        CP_JobReq_Record        *cp_jobreq;
        char                    *output_buf;
        int                     *output_buf_size;

#endif
{
	int 			ret = OK;
	int     		commid;         /* commit service id */
	int			job_id;
        char    		where_clause[100];
        cursor  		ptr ;
        llist   		*llistptr = NULL;
	int			nrecs;
	char			buf[2048];
        DB_RECORD      		**db_rec ;
	IMS_Order_Status        ims_status;
	IMS_L1PReq_Record       rec;
	char			logmsg[MAX_SYSLOG_MSGLEN] ;
        char    		errormsg[MAXLINE];  
 
        /* construct the error message for reporting deadlock error */
        (void)sprintf(errormsg, " %s : %s [mode = %s]",
                        pps_err_msgs[ER_DEADLOCK], "process_CP_FrameReq", 
                        cp_jobreq->processor_mode);


        /* start transaction */
        pps_start_xact(dbproc_server, dbproc_commit, &commid, CP_FRAME_KEYWD);
 
        /* get the next top available job */
	sprintf (buf, "sp_get_next_frame_job \"%s\" , \"%s\"", 
		L1_KEYWD, cp_jobreq->processor_mode);
        db_exec_cmd (dbproc_server, buf, &job_id, &nrecs);

	CHECK_DEADLOCK(errormsg);
		
	if (job_id <= 0)
	{
#ifdef DEBUG
        	printf("%s : none\n", buf);
#endif
                if (! strcmp(cp_jobreq->processor_mode,"SCANSAR"))
			pps_logMsg(ProgName, PPS_INFO,
			      pps_err_msgs[ER_NO_AVAIL_FRAME_SCANSAR]);
		else
			pps_logMsg(ProgName, PPS_INFO,
			      pps_err_msgs[ER_NO_AVAIL_FRAME_CONT]);
        	pps_commit_xact(dbproc_server, dbproc_commit, commid);
        	output_buf[0] = '\0';;
        	*output_buf_size = 0;
		return (ER_NO_ERROR);
	}

#ifdef DEBUG
        printf("%s : job_id =  %d\n", buf, job_id);
#endif

        /* find the corresponding record in L1 ProcParms table */
	sprintf(where_clause, "where job_id = %d", job_id);
        llistptr = db_get_records(dbproc_server, L1_PROC_PARMS_TABLENAME,
                where_clause, NULL, l1_procparms_columns_, ALL_COLS);
        if ( NUMELTS(llistptr) == 0 )
	{
		DEL_LIST( llistptr );
		(void)sprintf(logmsg, "%s [job=%d]",
				pps_err_msgs[ER_FIND_L1PP], job_id);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		abortall(dbproc_server, dbproc_commit, commid);
		return (ER_FIND_L1PP);
	}
        db_rec = (DB_RECORD **) FIRST( llistptr, ptr );
	bind_dbrec_to_l1(db_rec, &rec);
        DEL_LIST( llistptr );
 
	/* get the 'priority' and 'insert_top_flag' from Jobs table */ 
        sprintf(where_clause, "where job_id = %d", job_id);
        llistptr = db_get_records(dbproc_server, JOBS_TABLENAME,
                where_clause, NULL, jobs_columns_, ALL_COLS);
        if (NUMELTS(llistptr) == 0)
	{
		DEL_LIST( llistptr );
		(void)sprintf(logmsg, "%s [job=%d]",
				pps_err_msgs[ER_FIND_JOB], job_id);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		abortall(dbproc_server, dbproc_commit, commid);
		return (ER_FIND_JOB);
	}
        db_rec = (DB_RECORD **) FIRST( llistptr, ptr );
	PPS_STRNCPY(rec.priority, (char *)db_rec[JOBS_PRIORITY],
		PRIORITY_STRLEN);
	PPS_STRNCPY(rec.insert_top_flag,(char *)db_rec[JOBS_INSERT_TOP_FLAG],
		LOGICAL_STRLEN);
        DEL_LIST( llistptr );

	/* change job_state to submitted */
	sprintf (buf, "sp_update_job_state %d , \"%s\"", job_id, SUBMITTED);
        db_exec_cmd (dbproc_server, buf, &ret, &nrecs);

	CHECK_DEADLOCK(errormsg);

        if (ret != 1)
        {
	        (void)sprintf(logmsg, "%s [job=%d, state=%s]",
                                pps_err_msgs[ER_UPDATE_JOBSTATE],
                                job_id, SUBMITTED);
       		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		abortall(dbproc_server, dbproc_commit, commid);
		return (ER_UPDATE_JOBSTATE);
        }


        /* initialize output buffer */
        output_buf[0] = '\0';

	/* create buffer containing the job */
	create_CP_Framejob_buf_mx(&rec, output_buf, output_buf_size);

        /* check if output buffer was big enough */
        if (output_buf[0] == '\0')
        {
                /* set the message size to zero */
                *output_buf_size = 0;
		pps_logMsg(ProgName, PPS_ERROR,
                              pps_err_msgs[ER_BUFFER_TOO_SMALL]);
                abortall(dbproc_server, dbproc_commit, commid);
                return (ER_BUFFER_TOO_SMALL);
        }
 
        /* commit transaction */
        pps_commit_xact(dbproc_server, dbproc_commit, commid);

        /* find all L1 orders associated with the submitted job -
	   send IMS order status for each order */
        sprintf(where_clause, "where job_id = %d", job_id);
        llistptr = db_get_records(dbproc_server, L1_ORDERS_TABLENAME,
                where_clause, NULL, l1_order_columns_, ALL_COLS);
        if ( NUMELTS(llistptr) == 0 )
        {
		/* this case should never happen though */
                DEL_LIST( llistptr );
                (void)sprintf(logmsg, "%s [job=%d]",
                                pps_err_msgs[ER_FIND_L1_ORDER], job_id);
                pps_logMsg(ProgName, PPS_ERROR, logmsg);
                return (ER_NO_ERROR);
        }

	for (db_rec = (DB_RECORD **) FIRST(llistptr, ptr);
             db_rec ;
             db_rec = (DB_RECORD **) NEXT(llistptr, ptr) )
	{
        	rec.order_id = *(DBINT *) db_rec[L1_ORDER_ORDER_ID];
	        rec.item_id  = *(DBINT *) db_rec[L1_ORDER_ITEM_ID];

       		/* create order status to be sent to IMS */
	        fill_L1order_status(&rec, &ims_status, INTERMEDIATE, 
					SUBMITTED,NULL,NULL);
 
	        /* create buffer containing order status */
	        create_order_status_buf_mx( &ims_status, buf, sizeof(buf));
 
	        /* send order status to IMS */
	        ret = wrap_ims_orderStatus ( ims_query, buf );
		if (ret != ER_NO_ERROR)
		{
			sprintf(buf,"%s[order_id=%s,item_id=%s,job_id=%d,\
 			        status=%s]",
 				"Failed to report order status to IMS",	
				ims_status.order_id,ims_status.item_id,job_id,
				ims_status.status_id);
			pps_logMsg(ProgName, PPS_ERROR, buf);
		}
	}
	DEL_LIST( llistptr );

	return(ER_NO_ERROR);

} /* process_CP_FrameReq */

/* End of File */
