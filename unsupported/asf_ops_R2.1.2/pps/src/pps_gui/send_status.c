/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static char SccsFileId[] = "@(#)send_status.c	1.2  02/19/97";

#include "send_status.h"
#include "pps_db.h"
#include "PPSerr.h"
#include "PPSdefs.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

extern char ProgName[];

/*-------------------------------------------------------*/
/* private structures                                    */
/*-------------------------------------------------------*/
static PPSJobStatus				jobStatus;
static struct pps_db_exec_dcl	ppsQuery;
static IMS_CMN_QUERY			*imsQueryConnection=0;
static int						sendIMSOk = False;

#ifdef IMSTEST

/*------------------------------------------------*/
/* for testing only                               */
/*------------------------------------------------*/
static int
wrap_ims_orderStatus(
IMS_CMN_QUERY		**imsQuery,
char				*buffer)
{
	return(ER_NO_ERROR);
} /* wrap_ims_orderStatus */

#endif /* IMSTEST */


/*-------------------------------------------------------------------------
 * initJobStatusData initializes the internal structure used for storing data
 * returned from sybase.  It takes job_id, job_type and status from args
---------------------------------------------------------------------------*/
static void
initJobStatusData(
int		jobId,
char	*jobType,
char	*status)
{
	jobStatus.jobId = jobId;
	(void) strcpy(jobStatus.jobType, jobType);
	(void) strcpy(jobStatus.status, status);

	jobStatus.orderId = 0;
	jobStatus.itemId = 0;
	jobStatus.platform[0] = '\0';
	jobStatus.sensor[0] = '\0';
	jobStatus.dataset[0] = '\0';
	jobStatus.productId[0] = '\0';
	jobStatus.comment[0] = '\0';

} /* initJobStatusData */

/*-------------------------------------------------------------------------
* The "send_status" routine creates the ODL buffer which contains the order
* status and sends the order status to IMS
---------------------------------------------------------------------------*/
static void
sendOrderStatus(
IMS_Order_Status	*orderStatus)
{
	char	buf[MAXBIGBUF];
	char 	logmsg[MAX_SYSLOG_MSGLEN];
	int		ret_code;

	/* create buffer containing order status */
	create_order_status_buf(orderStatus, buf, sizeof(buf));

#ifdef DEBUG
	printf("order status ODL = %s\n", buf);
	fflush(stdout);
#endif

	/* send order status to IMS */
	ret_code = wrap_ims_orderStatus (&imsQueryConnection, buf);

	if (ret_code == ER_NO_ERROR)
	{
		(void) sprintf(logmsg,
				"IMS order status sent: order[%s], item[%s], status[%s]",
				orderStatus->order_id, orderStatus->item_id, 
				orderStatus->status_id);
		pps_logMsg(ProgName, PPS_INFO, logmsg);
		sendIMSOk = True;
	}
	else
	{
		if (ret_code == ER_IMS_ORDER_NOT_FOUND)
		{
			(void) sprintf(logmsg, "order[%s], item[%s] not found in IMS",
					orderStatus->order_id, orderStatus->item_id);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);

		}
		else
		{
			(void) sprintf("Failed to send IMS order status: order[%s], "
					"item[%s], status[%s]",
					orderStatus->order_id, orderStatus->item_id, 
					orderStatus->status_id);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
		}
	}
} /* sendOrderStatus */

/*****************************************************************************
 * "sendScanStatus" is called as a callback from db_exec get_rows each time
 * a result row is returned from sybase for SCAN order_id and item_id
 ****************************************************************************/
static void
sendScanStatus()
{
	IMS_ScanReq_Record      scanRec;
	IMS_Order_Status		orderStatus;

	scanRec.job_id = jobStatus.jobId;
	scanRec.order_id = jobStatus.orderId;
	scanRec.item_id = jobStatus.itemId;
	(void)strcpy(scanRec.platform, jobStatus.platform);
	(void)strcpy(scanRec.sensor, jobStatus.sensor);
	(void)strcpy(scanRec.comment, jobStatus.comment);

	/* create order status to be sent to IMS */
	fill_scanorder_status (&scanRec, &orderStatus, FINAL,
			jobStatus.status, jobStatus.dataset, jobStatus.productId);

	sendOrderStatus(&orderStatus);


} /* sendScanStatus */

/*****************************************************************************
 * "sendL1Status" is called as a callback from do_query get_rows each time
 * a result row is returned from sybase for L1 order_id and item_id
 ****************************************************************************/
static void
sendL1Status()
{
	IMS_L1PReq_Record	l1Rec;
	IMS_Order_Status	orderStatus;

	l1Rec.job_id = jobStatus.jobId;
	l1Rec.order_id = jobStatus.orderId;
	l1Rec.item_id = jobStatus.itemId;
	(void)strcpy(l1Rec.platform, jobStatus.platform);
	(void)strcpy(l1Rec.sensor, jobStatus.sensor);
	(void)strcpy(l1Rec.comment, jobStatus.comment);

	/* create order status to be sent to IMS */
	fill_L1order_status (&l1Rec, &orderStatus, FINAL,
			jobStatus.status, jobStatus.dataset, jobStatus.productId);

	sendOrderStatus(&orderStatus);

} /* sendL1Status */

/*-------------------------------------------------------------------------
 * The "sendFinalOrderStatusToIMS" sends to IMS the status of the order 
 * corresponding to the specified job_id.  Multiple status messages are
 * sent for collapsed L1 orders.   
 -------------------------------------------------------------------------*/
int
sendFinalOrderStatusToIMS (
CS_CONNECTION		*queryConnection,
IMS_CMN_QUERY		*imsQuery,
int					jobId,
char				*jobType,
char				*status)
{
	int		retcode;
	char	cmdbuf[MAXSMALLBUF];

	/* initialize the internal structure */
	sendIMSOk = False;
	imsQueryConnection = imsQuery;
	initJobStatusData(jobId, jobType, status);

	/* populate ppsQuery structure  */
	ppsQuery.num_items = 0;
	if (strcmp(jobType, L1_JOB_TYPE) == 0)
		ppsQuery.callback = sendL1Status;
	else
		ppsQuery.callback = sendScanStatus;

	/* set up the bindings for retrieving data */
	pps_db_bind_int(&ppsQuery, &(jobStatus.orderId));
	pps_db_bind_int(&ppsQuery, &(jobStatus.itemId));
	pps_db_bind_char(&ppsQuery, jobStatus.platform, MAXLINE);
	pps_db_bind_char(&ppsQuery, jobStatus.sensor, MAXLINE);
	pps_db_bind_char(&ppsQuery, jobStatus.dataset, MAXLINE);
	pps_db_bind_char(&ppsQuery, jobStatus.productId, MAXLINE);
	pps_db_bind_char(&ppsQuery, jobStatus.comment, MAXLINE);

	/* make the actual sql command */
    sprintf(cmdbuf, "exec sp_get_data_for_ims_status %d, '%s'",
									jobId, jobType);

	/* execute the sql command */
	retcode = db_exec(&queryConnection, cmdbuf, &ppsQuery);

	if (sendIMSOk)
		return(ER_NO_ERROR);
	else
		return(ER_IMS_SEND_STATUS_FAIL);

} /* sendFinalOrderStatusToIMS */
