/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static char SccsFileId[] = "@(#)process_cp_retry.c	1.1    04/23/97";

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "db_l1_order.h"
#include "db_scan_order.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

extern COLUMN_DEFS      l1_order_columns_[];
extern COLUMN_DEFS      scan_order_columns_[];

/*----------------------------------------------------------*/
/* create the order status ODL, and send it to IMS,         */
/* syslog the error, if any, and return the error code.     */
/*----------------------------------------------------------*/
int
report_status_to_ims( 
int						order_id,
int						item_id,
IMS_CMN_QUERY           **ims_query,
CP_JobStatus_Record     *cp_status)
{
    IMS_Order_Status  ims_status;
    char              imstatbuf[2048];
    int               ret;
    char              buf[MAXLINE];

    sprintf(ims_status.order_id, "%d", order_id);
    sprintf(ims_status.item_id, "%d", item_id);
 
    /* fill in info from CP status rec */
    strcpy(ims_status.dataset, cp_status->dataset);
    strcpy(ims_status.product_filename, cp_status->product_filename);
    strcpy(ims_status.platform, cp_status->platform);
    strcpy(ims_status.sensor, cp_status->sensor);
    strcpy(ims_status.comment, cp_status->comment) ;
    strcpy(ims_status.status_type, cp_status->status_type);
    strcpy(ims_status.status_id, cp_status->status);
 
    /* create buffer containing order status */
    create_order_status_buf_mx(&ims_status, imstatbuf, sizeof(imstatbuf));

#ifdef DEBUG
    printf("Sending Order Status to IMS: "
        "job_id=%d, order_id=%s, item_id=%s, status=%s\n",
        cp_status->job_id, ims_status.order_id,
        ims_status.item_id, ims_status.status_id);
#endif

    /* send order status to IMS */
    ret = wrap_ims_orderStatus(ims_query, imstatbuf);
    if (ret != ER_NO_ERROR)
    {
        /*----------------------------------------------------------*/
        /* Failed to notify IMS of orders status, log the           */
        /*         error and return error code to the caller.       */
        /* ret = ER_IMS_SEND_STATUS_FAIL or ER_IMS_ORDER_NOT_FOUND  */
        /*----------------------------------------------------------*/
        sprintf(buf, "%s [order_id=%s,item_id=%s,job_id=%d,status=%s]",
                        "Failed to report order status to IMS", 
                        ims_status.order_id, ims_status.item_id,
                        cp_status->job_id, ims_status.status_id);
        pps_logMsg(ProgName, PPS_ERROR, buf);
        return (ret);
    }

    return (ER_NO_ERROR);

} /* report_status_to_ims*/

int
update_job_state_from_cp(
DBPROCESS               *dbproc_server,
CP_JobStatus_Record     *cp_status)
{
    int                 ret=0, nrecs=0;
    char                errormsg[MAXLINE];
    char                buf[MAXLINE];          

    /* construct the error message for reporting deadlock error */
    (void)sprintf(errormsg, " %s : %s [job_id = %d, status = %s]",
                                  pps_err_msgs[ER_DEADLOCK],
                                  "process_CP_FrameRetry", 
                                  cp_status->job_id,
                                  cp_status->status);

    /* Update the job final status and info reported from CP */
    sprintf (buf, "sp_update_status_from_cp %d, '%s', '%s','%s','%s'", 
                                  cp_status->job_id,
                                  cp_status->status,
                                  cp_status->dataset,
                                  cp_status->product_filename,
                                  cp_status->comment);

    db_exec_cmd (dbproc_server, buf, &ret, &nrecs);
    CHECK_DEADLOCK (errormsg);

    /*----------------------------------------------------------*/
    /* if failed to update job state because the job is not in  */
    /* SUBMITTED or RETRY state, report the error and           */
    /* return error status code to CP                           */
    /*----------------------------------------------------------*/
    if (ret != 1)
    {
        (void)sprintf(buf, "%s [job_id=%d, status=%s]",
                               pps_err_msgs[ER_UPDATE_JOBSTATE],
                               cp_status->job_id,
                               cp_status->status);
        pps_logMsg(ProgName, PPS_ERROR, buf);
        return (ER_UPDATE_JOBSTATE);
    }
	else
		return(ER_NO_ERROR);

} /* update_job_state_from_cp */

int
process_CP_FrameRetry(
DBPROCESS               *dbproc_server,
DBPROCESS               *dbproc_commit, 
IMS_CMN_QUERY           **ims_query,
CP_JobStatus_Record     *cp_status)
{
    int               ret = 0, nrecs = 0;
    char              buf[1000];          
    llist             *llistptr = NULL;
    cursor            ptr;
    DB_RECORD         **db_rec ;
    char              errormsg[MAXLINE];  /* to report deadlock error */ 
 
	if ((ret = update_job_state_from_cp(dbproc_server,cp_status)) 
                                   != ER_NO_ERROR)
		return(ret);

    /*-----------------------------------------------*/
    /* for each matching row in L1 orders table,     */
    /* report L1 order RETRY to IMS                  */
    /*-----------------------------------------------*/
    sprintf(buf, "where job_id = %d", cp_status->job_id);
    llistptr = db_get_records(dbproc_server, L1_ORDERS_TABLENAME,
                buf, NULL, l1_order_columns_, ALL_COLS);
    if (NUMELTS(llistptr) > 0)
    {
        for (db_rec = (DB_RECORD **) FIRST(llistptr, ptr);
                        db_rec ;
                        db_rec = (DB_RECORD **) NEXT(llistptr, ptr) )
        {
            (void)report_status_to_ims(
                        *(DBINT *)db_rec[L1_ORDER_ORDER_ID],
                        *(DBINT *)db_rec[L1_ORDER_ITEM_ID],
                        ims_query, cp_status);
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

    /* return good status to CP */
    return( ER_NO_ERROR );

} /* process_CP_FrameRetry */

int 
process_CP_ScanRetry(
DBPROCESS               *dbproc_server,
DBPROCESS               *dbproc_commit, 
IMS_CMN_QUERY           **ims_query,
CP_JobStatus_Record     *cp_status)
{
    int                 ret = 0, nrecs = 0;
    char                buf[200];          
    char                imstatbuf[2048];          
    char                where_clause[100];
    DB_RECORD           **db_rec ;
    IMS_Order_Status    ims_status;
    llist               *llistptr = NULL;
    cursor              ptr ;
    char                errormsg[MAXLINE];

	if ((ret = update_job_state_from_cp(dbproc_server, cp_status)) 
                                     != ER_NO_ERROR)
		return(ret);

    /*----------------------------------------------------------*/
    /* report scan order RETRY to IMS                           */
    /*----------------------------------------------------------*/
    sprintf(where_clause, "where job_id = %d", cp_status->job_id);
    llistptr = db_get_records(dbproc_server, SCAN_ORDERS_TABLENAME,
                where_clause, NULL, scan_order_columns_, ALL_COLS);
    if (NUMELTS(llistptr) > 0)
    {
        db_rec = (DB_RECORD **) FIRST( llistptr, ptr );
        (void)report_status_to_ims(
                        *(DBINT *)db_rec[SCAN_ORDER_ORDER_ID],
                        *(DBINT *)db_rec[SCAN_ORDER_ITEM_ID],
                        ims_query, cp_status);

        DEL_LIST( llistptr );
    }
    else
    {
        DEL_LIST (llistptr);
        (void)sprintf(buf, "%s [job=%d]",
                        pps_err_msgs[ER_FIND_SCAN_ORDER],
                        cp_status->job_id);
        pps_logMsg(ProgName, PPS_ERROR, buf);
        return ( ER_FIND_SCAN_ORDER );
    }

    return( ER_NO_ERROR );

} /* process_CP_ScanRetry */
