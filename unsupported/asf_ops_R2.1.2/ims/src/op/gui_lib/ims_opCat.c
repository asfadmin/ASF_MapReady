static char *sccs = "@(#)ims_opCat.c	5.10  09/15/97";
/*****************************************************************************
**
** File:			ims_opCat.c
**
** Function: 	Catalog database access facility for the ims_op.
**
** Author:		Jennifer Ting
**
** Date: 			Dec 14, 1994
**
** Notes:
**			2/13/95 - replaced ims_qiDescInit() with ims_qiDescAlloc()
**
**			4/26/96 - Added functions updateItemStepInfo, getOrderItemInfo
**                and getItemStepInfo to update item step information
**                when an item needs to be regenerated from the Film
**                Generation Screen and Photo Job Screen.
**
**      4/30/96 - Modified removeFireQueueItem and removeLaserQueueItem.
**                When an item is regenerated from the PhotoJob Screen,
**                we call these two functions to remove the item from
**                either the fire or laser queue.
**
**      5/09/96 - Added function getStepNamePPSStatus, this function
**                returns the step name and process status of an item.
**                This function is called in canceling an item.
**
**      6/04/96 - Modified function getOrderItemList to get v0_process_type
**                from order_item table. This is the change for PR 900
**
**      7/12/96 - Modified functions getOrderLock to call stored procedure
**                get_order_lock.
**
**      7/12/96 - Modified functions getItemLock to call stored procedure
**                get_order_lock.
**
**      7/12/96 - Added function restartItem.
**
**      7/19/96 - Modified function getCatalogItems for PR 986 - display
**                user_type on screen title.
**
**      10/29/96 - Modified function restartItem for PR 2184 
**
**      03/28/97 - Created getKeywordValueInfo(), getDownlinkList().
**      05/02/97 - Created getDownlinkDTKList().
**      05/09/97 - Created getAuxiliaryLock(), updateDTKProcessAuthFlag(),
**                 issueDownlinkScanRequest().
**
**		  07/28/97 - Create getPhotoJobid()
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/utsname.h>

#include <Xm/Xm.h>

/* 
** #include <IK_Network.h>
** #include <IK_Syslog.h> 
*/

#include <odldef.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_order.h>

#include <ims_const.h>
#include <ims_odl.h>
#include <ims_msg.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_util.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>

/*
** Definition of local constants
*/
#define BUF_SIZE 1024   /* Maximum size of the cmd buffer */
#define EDEADLOCK 1205	/* Sybase DeadLock error number */

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in the modules where they
** are called. They are listed here for reference.
**
** int op_cat (OP_CAT_STRUCT *, OP_CAT_EVENT, char *);
*/

/*
** Local Functions
*/
static int useDatabase (OP_CAT_STRUCT *);
static int getOrderList (OP_CAT_STRUCT *);
static int getOrderLock (OP_CAT_STRUCT *);
static int getItemLock (OP_CAT_STRUCT *);
static int getItemStatus (OP_CAT_STRUCT *);
static int getOrderStatus (OP_CAT_STRUCT *);
static int rollbackItemStatus (OP_CAT_STRUCT *);
static int getOrderItemList (OP_CAT_STRUCT *);
static int getOrderItemStats (OP_CAT_STRUCT *);
static int getCatalogItems (OP_CAT_STRUCT *);
static int getPPSGranuleSizeStepTime (OP_CAT_STRUCT *);
static int updateOrderPriority (OP_CAT_STRUCT *);
static int updateItemPriority (OP_CAT_STRUCT *);
static int getNewOrderPriority (OP_CAT_STRUCT *);
static int getNewOrderStatus (OP_CAT_STRUCT *);
static int getUserData (OP_CAT_STRUCT *);
static int updateOrderComment (OP_CAT_STRUCT *);
static int updateItemComment (OP_CAT_STRUCT *);
static int updateOrderStatus (OP_CAT_STRUCT *);
static int updateItemStatus (OP_CAT_STRUCT *);
static int validateOrder (OP_CAT_STRUCT *);
static int validateItem (OP_CAT_STRUCT *);
static int getAccountIdList (OP_CAT_STRUCT *);
static int getUserIdList (OP_CAT_STRUCT *);
static int getPhotoQueueList (OP_CAT_STRUCT *);
static int getPhotoItemStatus (OP_CAT_STRUCT *);
static int createPhotoJob (OP_CAT_STRUCT *);
static int updatePhotoQueue (OP_CAT_STRUCT *);
static int updatePhotoJob (OP_CAT_STRUCT *);
static int getDatasetGranule(OP_CAT_STRUCT *);
static int getGranulesTable(OP_CAT_STRUCT *);
static int getProductName(OP_CAT_STRUCT *);
static int getPhotoJobIdList (OP_CAT_STRUCT *);
static int getPhotoJobList (OP_CAT_STRUCT *);
static int updatePhotoComment (OP_CAT_STRUCT *);
static int updatePhotoQuality (OP_CAT_STRUCT *);
static int completePhotoJob (OP_CAT_STRUCT *);
static int updatePhotoJobCost (OP_CAT_STRUCT *);
static int getTapeItems (OP_CAT_STRUCT *);
static int getMediaCapacity (OP_CAT_STRUCT *);
static int getItemStepSequence (OP_CAT_STRUCT *);
static int getItemAccountCost (OP_CAT_STRUCT *);
static int getFireQueueList (OP_CAT_STRUCT *);
static int getLaserQueueList (OP_CAT_STRUCT *);
static int getFireItemStatus (OP_CAT_STRUCT *);
static int getLaserItemStatus (OP_CAT_STRUCT *);
static int updateFireItemStatus (OP_CAT_STRUCT *);
static int updateLaserItemStatus (OP_CAT_STRUCT *);
static int updateFireItemComment (OP_CAT_STRUCT *);
static int updateLaserItemComment (OP_CAT_STRUCT *);
static int removeFireQueueItem (OP_CAT_STRUCT *);
static int removeLaserQueueItem (OP_CAT_STRUCT *);
static int getGeneralLock (OP_CAT_STRUCT *);
static int getShipItems (OP_CAT_STRUCT *);
static int getShippingId (OP_CAT_STRUCT *);
static int verifyItemShippingStatus (OP_CAT_STRUCT *);
static int getShippingData (OP_CAT_STRUCT *);
static int updateItemShippingStatus (OP_CAT_STRUCT *);
static int updateShippingComment (OP_CAT_STRUCT *);
static int cancelShipment (OP_CAT_STRUCT *);
static int getOrderShipIdList (OP_CAT_STRUCT *);
static int getBillingId (OP_CAT_STRUCT *);
static int getBillItems (OP_CAT_STRUCT *);
static int verifyItemBillingStatus (OP_CAT_STRUCT *);
static int getBillingData (OP_CAT_STRUCT *);
static int updateItemBillingStatus (OP_CAT_STRUCT *);
static int cancelBilling (OP_CAT_STRUCT *);
static int getOrderBillIdList (OP_CAT_STRUCT *);
static int getOpComment (OP_CAT_STRUCT *);
static int updateBillingComment (OP_CAT_STRUCT *);
static int getDarData (OP_CAT_STRUCT *);
static int getPhotoJobid (OP_CAT_STRUCT *);
static int updateItemMediaId (OP_CAT_STRUCT *);
static int getUserType (OP_CAT_STRUCT *);
static int updateCostDebitFlag (OP_CAT_STRUCT *);
static int updateItemStepInfo (OP_CAT_STRUCT *);
static int getOrderItemInfo (OP_CAT_STRUCT *);
static int getItemStepInfo (OP_CAT_STRUCT *);
static int getStepNamePPSStatus (OP_CAT_STRUCT *);
static int restartItem (OP_CAT_STRUCT *);
static int beginTransaction (OP_CAT_STRUCT *);
static int commitTransaction (OP_CAT_STRUCT *);
static int rollbackTransaction (OP_CAT_STRUCT *);
static int execCmd (IMS_QI_DESC_OBJ *);
static int processRetStatus (IMS_QI_DESC_OBJ *);
static int getKeywordValueInfo (OP_CAT_STRUCT *);
static int getDownlinkList (OP_CAT_STRUCT *);
static int getDownlinkDTKList (OP_CAT_STRUCT *);
static int getAuxiliaryLock (OP_CAT_STRUCT *);
static int updateDTKProcessAuthFlag (OP_CAT_STRUCT *);
static int issueDownlinkScanRequest (OP_CAT_STRUCT *);
static OP_KEYWORD_VAL_DATA *setValuesFromKeyword (IMS_QI_DESC_OBJ *,
				IMS_MSG_STRUCT *, int *, int *);

/*
** External Functions
*/

/*
** Global variables
*/
char cmdBuf[BUF_SIZE];
extern OP_GLOBAL_DATA glbData;

/******************************************************************************
**
** op_cat ()
**
** Main function handling catalog queries.
**
******************************************************************************/

int ims_opCat (OP_CAT_STRUCT *catReq, OP_CAT_EVENT event)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	int status;

	msgDesc = catReq->msgDesc;

	/*
	** We must first make sure that we have a descriptor if the event is
	** anything BUT OPENCONNECTION.
	*/

	if (catReq->qDesc == (IMS_QI_DESC_OBJ *)NULL) 
	{
		if (event == OP_OPENCONNECTION)
		{
			if ((catReq->qDesc = ims_qiDescAlloc (msgDesc)) 
				== (IMS_QI_DESC_OBJ *)NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not initialize query descriptor.");
				return (IMS_FATAL);
			}
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"First cat event must be OP_OPENCONNECTION.");
			return (IMS_FATAL);
		}
	}
	qDesc = catReq->qDesc;
	qDesc->cmd = cmdBuf;

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{
	case OP_OPENCONNECTION:
		/*
		** We also want to stay logged into the catalog until the 
		** OP_CLOSECONNECTION event is called.
		*/

		/*
		** Set the process' catalog login name
		*/
		IMS_SETUSER (qDesc, catReq->userSpec.dbUserName);

		/*
		** Set the process' catalog password
		*/
		IMS_SETPSWD (qDesc, catReq->userSpec.dbPassword);

		/*
		** Set the database name.
		*/
		if ((catReq->userSpec.dbName != (char *)NULL) &&
				(strlen (catReq->userSpec.dbName) != 0))
		{
			IMS_SETDBNAME (qDesc, catReq->userSpec.dbName);
		}

		/*
		** Set the program name 
		*/
		if ((catReq->userSpec.program != (char *)NULL) &&
				(strlen (catReq->userSpec.program) != 0))
		{
			IMS_SETPROG (qDesc, catReq->userSpec.program);
		}

		/*
		** Set the server name.
		*/
		if ((catReq->userSpec.server != (char *)NULL) &&
				(strlen (catReq->userSpec.server) != 0))
		{
			IMS_SETSERVER (qDesc, catReq->userSpec.server);
		}

		/*
		** Do a login to the database.
		*/
		if ((status = ims_qiLogin (qDesc)) < IMS_OK)
		{
			return (status);
		}

		/*
		** Assign msgDesc to be the user defined data portion of the
		** dbproc structure.  This is needed by the message and
		** error handlers for SQLServer connections of the ims_op.
		*/
		(void) dbsetuserdata (qDesc->dbproc, (BYTE *)msgDesc);
		return (IMS_OK);

	case OP_USEDATABASE:
		status = useDatabase (catReq);
		break;

	case OP_GETORDERLIST:
		status = getOrderList (catReq);
		break;

	case OP_GETORDERITEMLIST:
		status = getOrderItemList (catReq);
		break;

	case OP_GETORDERITEMSTATS:
		status = getOrderItemStats (catReq);
		break;

	case OP_GETORDERLOCK:
		status = getOrderLock (catReq);
		break;

	case OP_GETITEMLOCK:
		status = getItemLock (catReq);
		break;

	case OP_GETITEMSTATUS:
		status = getItemStatus (catReq);
		break;

	case OP_GETORDERSTATUS:
		status = getOrderStatus (catReq);
		break;

	case OP_ROLLBACKITEMSTATUS:
		status = rollbackItemStatus (catReq);
		break;

	case OP_GETPPSGRANULESIZESTEPTIME:
		status = getPPSGranuleSizeStepTime (catReq);
		break;

	case OP_GETCATALOGITEMS:
		status = getCatalogItems (catReq);
		break;

	case OP_UPDATEORDERPRIORITY:
		status = updateOrderPriority (catReq);
		break;

	case OP_UPDATEITEMPRIORITY:
		status = updateItemPriority (catReq);
		break;

	case OP_GETNEWORDERPRIORITY:
		status = getNewOrderPriority (catReq);
		break;

	case OP_GETNEWORDERSTATUS:
		status = getNewOrderStatus (catReq);
		break;

	case OP_GETUSERDATA:
		status = getUserData (catReq);
		break;

	case OP_UPDATEORDERCOMMENT:
		status = updateOrderComment (catReq);
		break;

	case OP_UPDATEITEMCOMMENT:
		status = updateItemComment (catReq);
		break;

	case OP_UPDATEORDERSTATUS:
		status = updateOrderStatus (catReq);
		break;

	case OP_UPDATEITEMSTATUS:
		status = updateItemStatus (catReq);
		break;

	case OP_VALIDATEORDER:
		status = validateOrder (catReq);
		break;

	case OP_VALIDATEITEM:
		status = validateItem (catReq);
		break;

	case OP_GETACCOUNTIDLIST:
		status = getAccountIdList(catReq);
		break;

	case OP_GETUSERIDLIST:
		status = getUserIdList(catReq);
		break;

	case OP_GETPHOTOQUEUELIST:
		status = getPhotoQueueList(catReq);
		break;

	case OP_GETPHOTOITEMSTATUS:
		status = getPhotoItemStatus(catReq);
		break;

	case OP_CREATEPHOTOJOB:
		status = createPhotoJob(catReq);
		break;

	case OP_UPDATEPHOTOQUEUE:
		status = updatePhotoQueue(catReq);
		break;

	case OP_UPDATEPHOTOJOB:
		status = updatePhotoJob(catReq);
		break;

	case OP_GETDATASETGRANULE:
		status = getDatasetGranule(catReq);
		break;

	case OP_GETGRANULESTABLE:
		status = getGranulesTable(catReq);
		break;

	case OP_GETPRODUCTNAME:
		status = getProductName(catReq);
		break;

	case OP_GETPHOTOJOBIDLIST:
		status = getPhotoJobIdList(catReq);
		break;

	case OP_GETPHOTOJOBLIST:
		status = getPhotoJobList(catReq);
		break;

	case OP_UPDATEPHOTOCOMMENT:
		status = updatePhotoComment (catReq);
		break;

	case OP_UPDATEPHOTOQUALITY:
		status = updatePhotoQuality (catReq);
		break;

	case OP_COMPLETEPHOTOJOB:
		status = completePhotoJob (catReq);
		break;

	case OP_UPDATEPHOTOJOBCOST:
		status = updatePhotoJobCost (catReq);
		break;

	case OP_GETTAPEITEMS:
		status = getTapeItems (catReq);
		break;

	case OP_GETMEDIACAPACITY:
		status = getMediaCapacity (catReq);
		break;

	case OP_GETITEMSTEPSEQUENCE:
		status = getItemStepSequence (catReq);
		break;

	case OP_GETITEMACCOUNTCOST:
		status = getItemAccountCost (catReq);
		break;

	case OP_GETFIREQUEUELIST:
		status = getFireQueueList(catReq);
		break;

	case OP_GETLASERQUEUELIST:
		status = getLaserQueueList(catReq);
		break;

	case OP_GETFIREITEMSTATUS:
		status = getFireItemStatus(catReq);
		break;

	case OP_GETLASERITEMSTATUS:
		status = getLaserItemStatus(catReq);
		break;

	case OP_UPDATEFIREITEMSTATUS:
		status = updateFireItemStatus(catReq);
		break;

	case OP_UPDATELASERITEMSTATUS:
		status = updateLaserItemStatus(catReq);
		break;

	case OP_UPDATEFIREITEMCOMMENT:
		status = updateFireItemComment(catReq);
		break;

	case OP_UPDATELASERITEMCOMMENT:
		status = updateLaserItemComment(catReq);
		break;

	case OP_REMOVEFIREQUEUEITEM:
		status = removeFireQueueItem(catReq);
		break;

	case OP_REMOVELASERQUEUEITEM:
		status = removeLaserQueueItem(catReq);
		break;

	case OP_GETGENERALLOCK:
		status = getGeneralLock (catReq);
		break;

	case OP_GETSHIPITEMS:
		status = getShipItems (catReq);
		break;

	case OP_GETSHIPPINGID:
		status = getShippingId (catReq);
		break;

	case OP_VERIFYITEMSHIPPINGSTATUS:
		status = verifyItemShippingStatus (catReq);
		break;

	case OP_GETSHIPPINGDATA:
		status = getShippingData (catReq);
		break;

	case OP_UPDATEITEMSHIPPINGSTATUS:
		status = updateItemShippingStatus (catReq);
		break;

	case OP_UPDATESHIPPINGCOMMENT:
		status = updateShippingComment (catReq);
		break;

	case OP_CANCELSHIPMENT:
		status = cancelShipment (catReq);
		break;

	case OP_GETORDERSHIPIDLIST:
		status = getOrderShipIdList (catReq);
		break;

	case OP_GETBILLINGID:
		status = getBillingId (catReq);
		break;

	case OP_GETBILLITEMS:
		status = getBillItems (catReq);
		break;

	case OP_VERIFYITEMBILLINGSTATUS:
		status = verifyItemBillingStatus (catReq);
		break;

	case OP_GETBILLINGDATA:
		status = getBillingData (catReq);
		break;

	case OP_UPDATEITEMBILLINGSTATUS:
		status = updateItemBillingStatus (catReq);
		break;

	case OP_CANCELBILLING:
		status = cancelBilling (catReq);
		break;

	case OP_GETORDERBILLIDLIST:
		status = getOrderBillIdList (catReq);
		break;

	case OP_GETOPCOMMENT:
		status = getOpComment (catReq);
		break;

	case OP_UPDATEBILLINGCOMMENT:
		status = updateBillingComment (catReq);
		break;

	case OP_GETDARDATA:
		status = getDarData (catReq);
		break;

	case OP_GETPHOTOJOBID:
		status = getPhotoJobid (catReq);
		break;

	case OP_UPDATEITEMMEDIAID:
		status = updateItemMediaId (catReq);
		break;

	case OP_GETUSERTYPE:
		status = getUserType (catReq);
		break;

	case OP_UPDATECOSTDEBITFLAG:
		status = updateCostDebitFlag (catReq);
		break;

	case OP_UPDATEITEMSTEPINFO:
		status = updateItemStepInfo (catReq);
		break;

	case OP_GETORDERITEMINFO:
		status = getOrderItemInfo (catReq);
		break;

	case OP_GETITEMSTEPINFO:
		status = getItemStepInfo (catReq);
		break;

	case OP_GETSTEPNAMEPPSSTATUS:
		status = getStepNamePPSStatus (catReq);
		break;

	case OP_RESTARTITEM:
		status = restartItem (catReq);
		break;

	case OP_GETKEYWORDVALUES:
		status = getKeywordValueInfo (catReq);
		break;

	case OP_GETDOWNLINKLIST:
		status = getDownlinkList (catReq);
		break;

	case OP_GETDOWNLINKDTKLIST:
		status = getDownlinkDTKList (catReq);
		break;

	case OP_GETAUXILIARYLOCK:
		status = getAuxiliaryLock (catReq);
		break;

	case OP_UPDATEDTKPROCESSAUTHFLAG:
		status = updateDTKProcessAuthFlag (catReq);
		break;

	case OP_ISSUEDOWNLINKSCANREQUEST:
		status = issueDownlinkScanRequest (catReq);
		break;

	case OP_BEGINTRANSACTION:
		status = beginTransaction (catReq);
		break;

	case OP_ROLLBACKTRANSACTION:
		status = rollbackTransaction (catReq);
		break;

	case OP_COMMITTRANSACTION:
		status = commitTransaction (catReq);
		break;

	case OP_CLOSECONNECTION:
		/*
		** Close the catalog connection.  
		*/
		status = ims_qiLogoff (qDesc);
		/* Free qDesc and associated allocated memory */
		ims_qiFreeDesc (qDesc);
		catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;
		return (status); 

	default:
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat: Invalid catalog event passed to op_cat.");
		status = IMS_FATAL;
		break;
	}

	/*
	** Release all query-allocated space and re-initialize qDesc for the
	** next time in, leaving open the connection to the catalog.
	*/
	if (qDesc->dbproc != (DBPROCESS *) NULL)
	{
		/*
		** Re-initialize query descriptor for next command, but do
		** not cancel previous command
		*/
		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK) return (status);


	return (IMS_OK);
} /* op_cat */

/***********************************************************************
**
** useDatabase -  
**
***********************************************************************/
static int useDatabase (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_CAT_USERSPEC *userSpec;
	int status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	userSpec = &(catReq->userSpec);

	(void) sprintf (qDesc->cmd, "use %s", userSpec->dbName);

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}
	return (IMS_OK);
}

/***********************************************************************
**
** getOrderList - get order queue listing  
**
***********************************************************************/
static int getOrderList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currPtr, *prevPtr;
	OP_ORDER_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_ORDER_LIST *)NULL;
	firstPtr = lastPtr = (OP_ORDER_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_ORDER_LIST *)
			malloc (sizeof (OP_ORDER_LIST))) == (OP_ORDER_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getOrderList: Memory allocation for OP_ORDER_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->itemList = (OP_ORDER_ITEM_LIST *)NULL;
		currPtr->next = (OP_ORDER_LIST *)NULL;
		currPtr->prev = (OP_ORDER_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)currPtr->user_id,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->user_id[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->user_id);

		(void) memcpy ((char *)currPtr->account_id,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->account_id[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->account_id);

		(void) memcpy ((char *)currPtr->received_time,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->received_time[qDesc->valLength[3]] = '\0';
		ims_truncStr (currPtr->received_time);

		(void) memcpy ((char *)currPtr->completed_time,
			qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->completed_time[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->completed_time);

		(void) memcpy ((char *)&(currPtr->priority),
			qDesc->valAddr[5], qDesc->valLength[5]);

		(void) memcpy ((char *)&(currPtr->item_count),
			qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[7], qDesc->valLength[7]);

		(void) memcpy ((char *)currPtr->op_comment,
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->op_comment[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->op_comment);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getOrderList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_ORDER_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getOrderList */


/****************************************************************************
**
** beginTransaction ()
**
** Open a transaction.
**
****************************************************************************/
static int beginTransaction (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	(void) sprintf (qDesc->cmd, "begin transaction");

	if (execCmd (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__beginTransaction: Could not begin transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}


/***********************************************************************
**
** getOrderItemList - get order item listing for a specified order. 
**
**
** Modifications: 04/09/1996 - add p_data_kbytes and p_metadata_kbytes.
**														 Note that p_data_kbytes = data_kbytes.
**
**                06/04/96 - add v0_process_type
**
***********************************************************************/
static int getOrderItemList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_ITEM_LIST *currPtr, *prevPtr;
	OP_ORDER_ITEM_LIST *firstPtr, *lastPtr;
	OP_ORDER_LIST *currOrder;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	prevPtr = currPtr = (OP_ORDER_ITEM_LIST *)NULL;
	firstPtr = lastPtr = (OP_ORDER_ITEM_LIST *)NULL;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];
	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getOrderItemList: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}

	(void) sprintf (qDesc->cmd,
		"select t1.order_id, t1.item_id, t1.order_item_type,\
		t1.cost, t1.shipping_id, \
		t1.billing_id, t1.priority, t1.dataset_idx, \
		t1.granule_idx, t1.p_dataset_idx, t1.p_granule_idx, \
		t1.media_type, t1.media_fmt_type, t1.validated_p, \
		t1.cost_debited_p, t1.shipped_p, t1.billed_p, \
		t1.quicklook_p, t1.media_id, t1.process_type, \
		t1.p_granule_name, t1.process_status, t1.status, \
		t1.step_name, t1.step_sequence, t1.step_started_p, \
		t1.granule_name, ");
	(void) sprintf (qDesc->cmd, "%s\
		t1.op_comment, \
		t1.quantity, t1.process_comment, t1.media_class, \
		t1.platform, t1.sensor, t1.dataset, \
		t1.p_data_kbytes, t1.p_metadata_kbytes, \
		t1.v0_process_type \
		from order_item t1 \
		where t1.order_id = %d \
		order by t1.item_id", 
		qDesc->cmd, currOrder->order_id);

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_ORDER_ITEM_LIST *)
			malloc (sizeof (OP_ORDER_ITEM_LIST))) == (OP_ORDER_ITEM_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getOrderItemList: Memory allocation for OP_ORDER_ITEM_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_ORDER_ITEM_LIST *)NULL;
		currPtr->prev = (OP_ORDER_ITEM_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[1], qDesc->valLength[1]);

		if ((qDesc->valLength[2] == 0) || (qDesc->valAddr[2] == (char *)NULL))
			currPtr->order_item_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->order_item_type),
				qDesc->valAddr[2], qDesc->valLength[2]);
		}

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
			currPtr->cost = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->cost),
				qDesc->valAddr[3], qDesc->valLength[3]);
		}

		if ((qDesc->valLength[4] == 0) || (qDesc->valAddr[4] == (char *)NULL))
			currPtr->shipping_id = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->shipping_id),
				qDesc->valAddr[4], qDesc->valLength[4]);
		}

		if ((qDesc->valLength[5] == 0) || (qDesc->valAddr[5] == (char *)NULL))
			currPtr->billing_id = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->billing_id),
				qDesc->valAddr[5], qDesc->valLength[5]);
		}

		if ((qDesc->valLength[6] == 0) || (qDesc->valAddr[6] == (char *)NULL))
			currPtr->priority = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->priority),
				qDesc->valAddr[6], qDesc->valLength[6]);
		}

		if ((qDesc->valLength[7] == 0) || (qDesc->valAddr[7] == (char *)NULL))
			currPtr->dataset_idx = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->dataset_idx),
				qDesc->valAddr[7], qDesc->valLength[7]);
		}

		if ((qDesc->valLength[8] == 0) || (qDesc->valAddr[8] == (char *)NULL))
			currPtr->granule_idx = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->granule_idx),
				qDesc->valAddr[8], qDesc->valLength[8]);
		}

		if ((qDesc->valLength[9] == 0) || (qDesc->valAddr[9] == (char *)NULL))
			currPtr->p_dataset_idx = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->p_dataset_idx),
				qDesc->valAddr[9], qDesc->valLength[9]);
		}

		if ((qDesc->valLength[10] == 0) || (qDesc->valAddr[10] == (char *)NULL))
			currPtr->p_granule_idx = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->p_granule_idx),
				qDesc->valAddr[10], qDesc->valLength[10]);
		}

		if ((qDesc->valLength[11] == 0) || (qDesc->valAddr[11] == (char *)NULL))
			currPtr->media_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_type),
				qDesc->valAddr[11], qDesc->valLength[11]);
		}

		if ((qDesc->valLength[12] == 0) || (qDesc->valAddr[12] == (char *)NULL))
			currPtr->media_fmt_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_fmt_type),
				qDesc->valAddr[12], qDesc->valLength[12]);
		}

		if ((qDesc->valLength[13] == 0) || (qDesc->valAddr[13] == (char *)NULL))
			currPtr->validated_p = (char)NULL;
		else
		{
			(void) memcpy ((char *)&currPtr->validated_p,
				qDesc->valAddr[13], qDesc->valLength[13]);
		}

		if ((qDesc->valLength[14] == 0) || (qDesc->valAddr[14] == (char *)NULL))
			currPtr->cost_debited_p = (char)NULL;
		else
		{
			(void) memcpy ((char *)&currPtr->cost_debited_p,
				qDesc->valAddr[14], qDesc->valLength[14]);
		}

		if ((qDesc->valLength[15] == 0) || (qDesc->valAddr[15] == (char *)NULL))
			currPtr->shipped_p = (char)NULL;
		else
		{
			(void) memcpy ((char *)&currPtr->shipped_p,
				qDesc->valAddr[15], qDesc->valLength[15]);
		}

		if ((qDesc->valLength[16] == 0) || (qDesc->valAddr[16] == (char *)NULL))
			currPtr->billed_p = (char)NULL;
		else
		{
			(void) memcpy ((char *)&currPtr->billed_p,
				qDesc->valAddr[16], qDesc->valLength[16]);
		}

		if ((qDesc->valLength[17] == 0) || (qDesc->valAddr[17] == (char *)NULL))
			currPtr->quicklook_p = (char)NULL;
		else
		{
			(void) memcpy ((char *)&currPtr->quicklook_p,
				qDesc->valAddr[17], qDesc->valLength[17]);
		}

		(void) memcpy ((char *)currPtr->media_id,
			qDesc->valAddr[18], qDesc->valLength[18]);
		currPtr->media_id[qDesc->valLength[18]] = '\0';
		ims_truncStr (currPtr->media_id);

		if ((qDesc->valLength[19] == 0) || (qDesc->valAddr[19] == (char *)NULL))
			currPtr->process_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->process_type),
				qDesc->valAddr[19], qDesc->valLength[19]);
		}

		(void) memcpy ((char *)currPtr->p_granule_name,
			qDesc->valAddr[20], qDesc->valLength[20]);
		currPtr->p_granule_name[qDesc->valLength[20]] = '\0';
		ims_truncStr (currPtr->p_granule_name);

		if ((qDesc->valLength[21] == 0) || (qDesc->valAddr[21] == (char *)NULL))
			currPtr->process_status = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->process_status),
				qDesc->valAddr[21], qDesc->valLength[21]);
		}

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[22], qDesc->valLength[22]);

		(void) memcpy ((char *)currPtr->step_name,
			qDesc->valAddr[23], qDesc->valLength[23]);
		currPtr->step_name[qDesc->valLength[23]] = '\0';
		ims_truncStr (currPtr->step_name);

		if ((qDesc->valLength[24] == 0) || (qDesc->valAddr[24] == (char *)NULL))
			currPtr->step_sequence = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->step_sequence),
				qDesc->valAddr[24], qDesc->valLength[24]);
		}

		if ((qDesc->valLength[25] == 0) || (qDesc->valAddr[25] == (char *)NULL))
			currPtr->step_started_p = (char)NULL;
		else
		{
			(void) memcpy ((char *)&(currPtr->step_started_p),
				qDesc->valAddr[25], qDesc->valLength[25]);
		}

		(void) memcpy ((char *)currPtr->granule_name,
			qDesc->valAddr[26], qDesc->valLength[26]);
		currPtr->granule_name[qDesc->valLength[26]] = '\0';
		ims_truncStr (currPtr->granule_name);

		(void) memcpy ((char *)currPtr->op_comment,
			qDesc->valAddr[27], qDesc->valLength[27]);
		currPtr->op_comment[qDesc->valLength[27]] = '\0';
		ims_truncStr (currPtr->op_comment);

		if ((qDesc->valLength[28] == 0) || (qDesc->valAddr[28] == (char *)NULL))
			currPtr->quantity = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->quantity),
				qDesc->valAddr[28], qDesc->valLength[28]);
		}

		(void) memcpy ((char *)currPtr->process_comment,
			qDesc->valAddr[29], qDesc->valLength[29]);
		currPtr->process_comment[qDesc->valLength[29]] = '\0';
		ims_truncStr (currPtr->process_comment);

		if ((qDesc->valLength[30] == 0) || (qDesc->valAddr[30] == (char *)NULL))
			currPtr->media_class = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_class),
				qDesc->valAddr[30], qDesc->valLength[30]);
		}

		/*
		** 2/19/96 - added platform, sensor and dataset
		*/
		(void) memcpy ((char *)currPtr->platform,
			qDesc->valAddr[31], qDesc->valLength[31]);
		currPtr->platform[qDesc->valLength[31]] = '\0';
		ims_truncStr (currPtr->platform);

		(void) memcpy ((char *)currPtr->sensor,
			qDesc->valAddr[32], qDesc->valLength[32]);
		currPtr->sensor[qDesc->valLength[32]] = '\0';
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((char *)currPtr->dataset,
			qDesc->valAddr[33], qDesc->valLength[33]);
		currPtr->dataset[qDesc->valLength[33]] = '\0';
		ims_truncStr (currPtr->dataset);


		/* 
		** 04/09/1996 - added p_data_kbytes and p_metadata_kbytes.
		** Please note that these two fields are exactly the same
		** as data_kbytes and metadata_kbytes in granules tables.
		** data_kbytes name is used because it has been in the 
		** Order Item Data Structure since day one.
		*/
		if ((qDesc->valLength[34] == 0) || (qDesc->valAddr[34] == (char *)NULL))
			currPtr->data_kbytes = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->data_kbytes),
				qDesc->valAddr[34], qDesc->valLength[34]);
		}


		if ((qDesc->valLength[35] == 0) || (qDesc->valAddr[35] == (char *)NULL))
			currPtr->p_metadata_kbytes = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->p_metadata_kbytes),
				qDesc->valAddr[35], qDesc->valLength[35]);
		}

		/*
		** 06/04/96 - Added v0_process_type
		*/
		(void) memcpy ((char *)currPtr->v0_process_type,
			qDesc->valAddr[36], qDesc->valLength[36]);
		currPtr->v0_process_type[qDesc->valLength[36]] = '\0';
		ims_truncStr (currPtr->v0_process_type);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getOrderList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_ORDER_ITEM_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getOrderItemList */



/***********************************************************************
**
** getOrderItemStats - 	For a particular order_id, get the count for
**											each of the following item status: 
**											QUICKLOOK, GENERATED, ONLINE, HOLD, ERROR
**
** Modified: 12/07/95 - Schema version 3.30
**											QUICKLOOK, GENERATED, INMEDIA, CANCELLED, ERROR
**											and FAILED
**					
***********************************************************************/
static int getOrderItemStats (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];

	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getOrderItemStats: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}


	/*
	** Get count of items with status of GENERATED 
	*/
	(void) sprintf (qDesc->cmd, "op_get_item_stats %d", currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/* Get the returned data */
	(void) memcpy ((char *)&(currOrder->quicklook),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	(void) memcpy ((char *)&(currOrder->item_generated),
		 qDesc->valAddr[1], qDesc->valLength[1]);

	(void) memcpy ((char *)&(currOrder->item_online),
		 qDesc->valAddr[2], qDesc->valLength[2]);

	(void) memcpy ((char *)&(currOrder->item_hold),
		 qDesc->valAddr[3], qDesc->valLength[3]);

	(void) memcpy ((char *)&(currOrder->item_error),
		 qDesc->valAddr[4], qDesc->valLength[4]);

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getOrderItemStats: Could not reinitialize query descriptor.");

		return(IMS_FATAL);
	}


	return(IMS_OK);

} /* getOrderItemStats */


/***********************************************************************
**
** getCatalogItems - 	
**
** 07/19/96 - get user_type to display on screen title, see PR 986
**
***********************************************************************/
static int getCatalogItems (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int rowCount;
	struct utsname uname_info;  /* structure for uname() */
	char hostName[IMS_HOST_LEN+1];

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Get order status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'order_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.order_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.order_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.order_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.order_status[rowCount].item_name);
		ims_toUpper (glbData.order_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.order_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/*
	** Get item status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'item_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.item_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.item_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.item_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.item_status[rowCount].item_name);
		ims_toUpper (glbData.item_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.item_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/*
	** Get priority valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'priority' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.priority[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.priority[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.priority[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.priority[rowCount].item_name);
		ims_toUpper (glbData.priority[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.priority_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get pps status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'process_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.pps_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.pps_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.pps_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.pps_status[rowCount].item_name);
		ims_toUpper (glbData.pps_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.pps_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get photo type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'photo_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.photo_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.photo_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.photo_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.photo_type[rowCount].item_name);
		ims_toUpper (glbData.photo_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.photo_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get photo queue status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'photo_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.photo_queue_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.photo_queue_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.photo_queue_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.photo_queue_status[rowCount].item_name);
		ims_toUpper (glbData.photo_queue_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.photo_queue_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get photo job status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'photojob_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.photojob_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.photojob_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.photojob_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.photojob_status[rowCount].item_name);
		ims_toUpper (glbData.photojob_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.photojob_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get media_type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'media_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.media_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.media_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.media_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.media_type[rowCount].item_name);
		ims_toUpper (glbData.media_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.media_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get media_fmt_type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'media_fmt_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.media_fmt_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.media_fmt_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.media_fmt_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.media_fmt_type[rowCount].item_name);
		ims_toUpper (glbData.media_fmt_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.media_fmt_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get order_item_type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'item_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.order_item_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.order_item_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.order_item_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.order_item_type[rowCount].item_name);
		ims_toUpper (glbData.order_item_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.order_item_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	/* 
	** Get process_type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'process_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.process_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.process_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.process_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.process_type[rowCount].item_name);
		ims_toUpper (glbData.process_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.process_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get resource_type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'resource_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.resource_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.resource_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.resource_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.resource_type[rowCount].item_name);
		ims_toUpper (glbData.resource_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.resource_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get fire_status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'fire_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.fire_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.fire_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.fire_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.fire_status[rowCount].item_name);
		ims_toUpper (glbData.fire_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.fire_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get laser_status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'laser_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.laser_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.laser_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.laser_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.laser_status[rowCount].item_name);
		ims_toUpper (glbData.laser_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.laser_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get list of devices from device_policy table for local HOST 
	*/

	/* Get Local host name */
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';

	(void) sprintf (qDesc->cmd,
		"select device_id, name from device_policy where host = '%s' order by device_id", hostName);

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.device_list[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy (&(glbData.device_list[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.device_list_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get dar_status valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'dar_status' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.dar_status[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.dar_status[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.dar_status[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.dar_status[rowCount].item_name);
		ims_toUpper (glbData.dar_status[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.dar_status_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** Get spatial type valids
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'spatial_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.spatial_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.spatial_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.spatial_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (glbData.spatial_type[rowCount].item_name);
		ims_toUpper (glbData.spatial_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.spatial_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/* 
	** 07/19/96 - get user types so we can display this information 
	** on screen title bar.
	**
	** Get user types 
	*/
	(void) strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'user_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.op_user_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.op_user_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.op_user_type[rowCount].item_name[qDesc->valLength[1]] = '\0';

		ims_truncStr (glbData.op_user_type[rowCount].item_name);
		ims_toUpper (glbData.op_user_type[rowCount].item_name);

		/* a row is returned */
		rowCount += 1;

	}
	glbData.op_user_type_count = rowCount;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* getCatalogItems */


/******************************************************************************
**
** getOrderLock ()
**
** Execute stored procedure getOrderLock to update table order_lock
**
** 2/22/96 - Operator Interface uses op_order_lock.
** 7/12/96 - Change to use order_lock.
**
******************************************************************************/

static int getOrderLock (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Execute stored procedure get_order_lock
	*/
	(void) sprintf (qDesc->cmd, "get_order_lock");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_order_lock failed.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}


/******************************************************************************
**
** getItemLock ()
**
** Execute stored procedure getItemLock to update table item_lock
**
** 2/22/96 - Operator Interface uses op_order_lock
** 7/12/96 - Change to use order_lock.
**
******************************************************************************/

static int getItemLock (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Execute stored procedure get_order_lock
	*/
	(void) sprintf (qDesc->cmd, "get_order_lock");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_order_lock failed.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}


/***********************************************************************
**
** updateOrderPriority - update the priority of an order
**
***********************************************************************/
static int updateOrderPriority (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	int status; 
	short priority_id;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];
	priority_id = *(short *)catReq->item[1];

	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updateOrderPriority: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** Update Order Priority
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_queue set priority = %d where order_id = %d",
			 priority_id, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderPriority: update order_queue priority failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderPriority: No row was affected in order_queue table.");
		return(IMS_FATAL);
	}


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderPriority: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Update Order Item Priority
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_item set priority = %d where order_id = %d",
			 priority_id, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderPriority: update order_item priority failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderPriority: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateOrderPriority */



/***********************************************************************
**
** updateItemPriority - update the priority of an item
**
***********************************************************************/
static int updateItemPriority (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_ITEM_LIST *currItemPtr;
	int status; 
	short priority_id;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currItemPtr = (OP_ORDER_ITEM_LIST *) catReq->item[0];
	priority_id = *(short *)catReq->item[1];

	if (currItemPtr == (OP_ORDER_ITEM_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updateItemPriority : Invalid/Null OP_ORDER_ITEM_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** Update Item Priority 
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_item set priority = %d where order_id = %d and item_id = %d",
			 priority_id, currItemPtr->order_id, currItemPtr->item_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemPriority: update order_item priority failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemPriority: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateItemPriority */


/***********************************************************************
**
** getNewOrderPriority - get the new priority of an order after item
**											 priority being changed
**
***********************************************************************/
static int getNewOrderPriority (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	int status; 
	short priority_id;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];
	priority_id = *(short *)catReq->item[1];

	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getNewOrderPriority: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** Update Order Priority
	*/
	(void) sprintf (qDesc->cmd,
				"update order_queue set priority = (select max(priority) "
				"from order_item where order_id = %d) where order_id = %d",
			  currOrder->order_id, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getNewOrderPriority: update order_queue priority failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getNewOrderPriority: No row was affected in order_queue table.");
		return(IMS_FATAL);
	}


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getNewOrderPriority: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	(void) sprintf (qDesc->cmd,
		"select priority from order_queue where order_id = %d",
		currOrder->order_id);

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getNewOrderPriority: Select priority from order_queue failed.");
		return (status);
	}

	/* check to see if any row returned */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getNewOrderPriority: No priority entry found in order_queue.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&(currOrder->priority),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	return(IMS_OK);

} /* getNewOrderPriority */


/***********************************************************************
**
** getNewOrderStatus - get the new status of an order after item
**										 status being changed
**
***********************************************************************/
static int getNewOrderStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status, order_id; 
	short new_status;
	short status_id;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	order_id = *(int *)catReq->item[0];
	status_id = *(short *)catReq->item[1];


	/*
	** execute stored procedure get_new_order_status 
	*/
	(void) sprintf (qDesc->cmd, "get_new_order_status %d, %d",
									 order_id, status_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_new_order_status failed.");
		return(IMS_FATAL);
	}

	(void) memcpy ((char *)&(new_status),
		qDesc->valAddr[0], qDesc->valLength[0]);

	/* Return new status to the calling routine */
	*(short *)catReq->item[2] = (short)new_status;

	return(IMS_OK);

} /* getNewOrderStatus */


/***********************************************************************
**
** updateOrderComment - update the comment of an order
**
***********************************************************************/
static int updateOrderComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updateOrderComment: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}


	/*
	** Update Order comment 
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_queue set op_comment = '%s' where order_id = %d",
			 text, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderComment: update order_queue comment failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderComment: No row was affected in order_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateOrderComment */


/***********************************************************************
**
** updateItemComment - update the comment of an item
**
***********************************************************************/
static int updateItemComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_ITEM_LIST *currItemPtr;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currItemPtr = (OP_ORDER_ITEM_LIST *) catReq->item[0];

	if (currItemPtr == (OP_ORDER_ITEM_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updateItemComment : Invalid/Null OP_ORDER_ITEM_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	/*
	** Update Item comment 
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_item set op_comment = '%s' where order_id = %d and item_id = %d",
			 text, currItemPtr->order_id, currItemPtr->item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemComment: update order_item comment failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemComment: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateItemComment */


/***********************************************************************
**
** updateOrderStatus - update the status of an order
**
***********************************************************************/
static int updateOrderStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	int status; 
	DBSMALLINT status_id; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];
	status_id = *(DBSMALLINT *)catReq->item[1];

	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updateOrderStatus: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** Update Order Status
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_queue set status = %d where order_id = %d",
			 status_id, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderStatus: update order_queue status failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderStatus: No row was affected in order_queue table.");
		return(IMS_FATAL);
	}


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderStatus: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Update Order Item Status
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_item set status = %d where order_id = %d",
			 status_id, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderStatus: update order_item status failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateOrderStatus: No row was affected in order_item table.");
		return(IMS_FATAL);
	}


	return(IMS_OK);

} /* updateOrderStatus */


/***********************************************************************
**
** updateItemStatus - update the status of an item
**
***********************************************************************/
static int updateItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT item_status_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status_id = *(DBSMALLINT *)catReq->item[2];

	/*
	** update the item status
	*/
/*
	(void) sprintf (qDesc->cmd, "op_update_item_status %d, %d, %d",
								 order_id, item_id, item_status_id);
*/

	(void) sprintf (qDesc->cmd,
		 "update order_item set status = %d where order_id = %d and item_id = %d",
		 item_status_id, order_id, item_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemStatus: update order_item status failed.");
		return(IMS_FATAL);
	}


	/* return error if no row was affected */
	if (IMS_AFFECTED (qDesc) != 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemStatus: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return (IMS_OK);

} /* updateItemStatus */


/***********************************************************************
**
** validateOrder - validate an order and all items associated
** 02/12/96 - not used in R1B prime.
**
***********************************************************************/
static int validateOrder (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	char value;
	int status, count; 

	count = 0;
	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];
	value = *(char *)catReq->item[1];

	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__validateOrder: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** validate all items of an order
	*/
	(void) sprintf (qDesc->cmd,
			 "update order_item set validated_p = '%c' where order_id = %d",
			 value, currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateOrder: update order_item validated_p failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateOrder: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateOrder: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** 02/06/96 - if order_item_type is DAR, update the status of 
	** items in the dar table to be VALIDATED.
	** (1) determine whether this order has order_item_type DAR
	** (2) update the dar table status to VALIDATED if validated_p = 'Y'
	**		 update the dar table status to NEW if validated_p = 'N'
	*/
	(void) sprintf (qDesc->cmd,
		 "select count(item_id) from order_item where order_id = %d "
		 "and order_item_type = 7", currOrder->order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_validateOrder: Could not get order_item_type from order_item.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&(count),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateOrder: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** order has DAR order_item_type
	** if validated_p is set to 'Y', then status in the dar table 
	** is set to VALIDATED, if validated_p is set to 'N', then 
	** status in the dar table is set to NEW.
	*/
	if (count > 0)
	{
		if (value == 'Y')
		{
			(void) sprintf (qDesc->cmd,
				 "update dar set status = 2 where order_id = %d",
					currOrder->order_id);
		}
		else 
		{
			(void) sprintf (qDesc->cmd,
				 "update dar set status = 1 where order_id = %d",
					currOrder->order_id);
		}

		/* Execute the sql command */
		if ((status = execCmd (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateOrder: update dar table status failed.\n");
			return(IMS_FATAL);
		}

		/* check to see if any update took place */
		if (IMS_AFFECTED (qDesc) <= 0)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_validateOrder: No row was affected in dar table.");
			return(IMS_FATAL);
		}
	}

	return(IMS_OK);

} /* validateOrder */


/***********************************************************************
**
** validateItem - validate an item
**
***********************************************************************/
static int validateItem (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_ITEM_LIST *currItemPtr;
	int status; 
	char value;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currItemPtr = (OP_ORDER_ITEM_LIST *) catReq->item[0];
	value = *(char *)catReq->item[1];

	if (currItemPtr == (OP_ORDER_ITEM_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__validateItem: Invalid/Null OP_ORDER_ITEM_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** validate the item
	** 02/12/96 - if call from validate function, set the 
	** validated_p flag to Y and item status to VALIDATED (2)
	** if call from unvalidate function, set the validated_p
	** flag to N and item status to NEW (1). Unvalidate
	** function is not available to the user in R1B prime.
	*/

	if (value == 'Y')
	{
		(void) sprintf (qDesc->cmd,
 			"update order_item set status = 2, validated_p = 'Y' "
			"where order_id = %d and item_id = %d",
			currItemPtr->order_id, currItemPtr->item_id);
	}
	else
	{
		(void) sprintf (qDesc->cmd,
 			"update order_item set status = 1, validated_p = 'N' "
			"where order_id = %d and item_id = %d",
 		  currItemPtr->order_id, currItemPtr->item_id);
	}


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateItem: update order_item status and validated_p failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateItem: No row was affected in order_item table.");
		return(IMS_FATAL);
	}


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_validateItem: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** 02/06/96 - if item has order_item_type DAR, update the status
	** of this item in table dar to 2, VALIDATED
	*/
	if (currItemPtr->order_item_type == DAR)
	{
		if (value == 'Y')
		{
			(void) sprintf (qDesc->cmd,
 				"update dar set status = 2 where order_id = %d and item_id = %d",
				 currItemPtr->order_id, currItemPtr->item_id);
		}
		else
		{
			(void) sprintf (qDesc->cmd,
 				"update dar set status = 1 where order_id = %d and item_id = %d",
				 currItemPtr->order_id, currItemPtr->item_id);
		}

		/* Execute the sql command */
		if ((status = execCmd (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_validateItem: update dar table item status failed.");
			return(IMS_FATAL);
		}

		/* check to see if any update took place */
		if (IMS_AFFECTED (qDesc) <= 0)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_validateItem: No row was affected in dar table.");
			return(IMS_FATAL);
		}
		
	}

	return(IMS_OK);

} /* validateItem */


/***********************************************************************
**
** getFrameId - get the name and version for a specific granule
**
**							1/3/96 - not needed because granule_name is in order_item
***********************************************************************/
/***********************************************************************
static int getFrameId (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_ITEM_LIST *currItemPtr;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	** Input from the calling routine **
	currItemPtr = (OP_ORDER_ITEM_LIST *) catReq->item[0];

	if (currItemPtr == (OP_ORDER_ITEM_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getFrameId: Invalid/Null OP_ORDER_ITEM_LIST struct.");
		return (IMS_FATAL);
	}

	**
	** get name from the granules_table 
	**
	(void) sprintf (qDesc->cmd,
			 "select name from %s where granule_idx = %d and dataset_idx = %d",
		currItemPtr->granules_table, 
		currItemPtr->granule_idx, currItemPtr->dataset_idx);

	** Execute the sql command **
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}

	(void) memcpy ((char *)currItemPtr->frame_id,
		qDesc->valAddr[0], qDesc->valLength[0]);
		currItemPtr->frame_id[qDesc->valLength[0]] = '\0';
		ims_truncStr (currItemPtr->frame_id);

	**
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	**
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getFrameId: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	**
	** get data_kbytes from the granules_table 
	**
	(void) sprintf (qDesc->cmd,
			 "select data_kbytes from %s where granule_idx = %d and dataset_idx = %d",
		currItemPtr->granules_table, 
		currItemPtr->p_granule_idx, currItemPtr->p_dataset_idx);

	** Execute the sql command **
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}

	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		currItemPtr->data_kbytes = -1;
	else
	{
		(void) memcpy ((char *)&(currItemPtr->data_kbytes),
			qDesc->valAddr[0], qDesc->valLength[0]);
	}

	**
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	**
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getFrameId: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	**
	** get step_avgtime from the order_item_step table 
	**
	(void) sprintf (qDesc->cmd,
			 "select step_avgtime from order_item_step where "
			 "order_item_type = %d and media_class = %d and "
			 "process_type = %d",
		currItemPtr->order_item_type, 
		currItemPtr->media_class,
		currItemPtr->process_type);

	*** Execute the sql command ***
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}

	(void) memcpy ((char *)currItemPtr->step_avgtime,
		qDesc->valAddr[0], qDesc->valLength[0]);
		currItemPtr->step_avgtime[qDesc->valLength[0]] = '\0';
		ims_truncStr (currItemPtr->step_avgtime);


	return(IMS_OK);

} *** getFrameId ***
***********************************************************************/

/***********************************************************************
**
** getPPSGranuleSizeStepTime - get the data_kbytes of a specific granule
**														 and step_avgtime from order_item_step
**
***********************************************************************/
static int getPPSGranuleSizeStepTime (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_ITEM_LIST *currItemPtr;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currItemPtr = (OP_ORDER_ITEM_LIST *) catReq->item[0];

	if (currItemPtr == (OP_ORDER_ITEM_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getPPSGranuleSize: Invalid/Null OP_ORDER_ITEM_LIST struct.");
		return (IMS_FATAL);
	}
	
	/*
	** 4/9/96 - p_data_kbytes and p_metadata_kbytes are now added
	** in the order_item table.  We are getting these two information
	** in getOrderItemList function.  For now, we will initialize
	** field granules_table to NULL, we will remove it later when
	** there is absolutely no need for this data field.
	*/

	currItemPtr->granules_table[0] = '\0';

	if (currItemPtr->order_item_type == DAR)
	{
		currItemPtr->data_kbytes = -1;
	}

	/*******************************************************************
	**
	** 1/26/96 - if the item has order_item_type DAR, dataset_idx, 
	** granule_idx are 0 in the order_item table, and granule_name
	** is NULL in the order_item table.
	**
	if (currItemPtr->order_item_type == DAR)
	{
		currItemPtr->granules_table[0] = '\0';
		currItemPtr->data_kbytes = -1;
	}
	else
	{
		**
		** get granules_table from dataset_policy table
		**

		(void) sprintf (qDesc->cmd,
			 "select granules_table from dataset_policy t1, order_item t2 "
			 "where t2.order_id = %d and t2.item_id = %d and "
			 "t1.dataset_idx = t2.dataset_idx",
			 currItemPtr->order_id, currItemPtr->item_id);

		** Execute the sql command **
		if ((status = execCmd (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat__getPPSGranuleSize: granules_table retrieval failed.");
			return (status);
		}

		(void) memcpy ((char *)currItemPtr->granules_table,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currItemPtr->granules_table[qDesc->valLength[0]] = '\0';
		ims_truncStr (currItemPtr->granules_table);


		**
		** Re-initialize query descriptor for next command, but do
		** not cancel previous command
		**
		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat__getPPSGranuleSizeStepTime: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}

		**
		** Let's get the data_kbytes from granules_table only if 
		** the granules_table is not NULL
		**

		if (currItemPtr->granules_table[0] == '\0')
		{
			currItemPtr->data_kbytes = -1;
		}
		else
		{
			**
			** get data_kbytes from the granules_table 
			**
			(void) sprintf (qDesc->cmd,
				 "select data_kbytes from %s where granule_idx = %d and dataset_idx = %d",
				currItemPtr->granules_table, 
				currItemPtr->p_granule_idx, currItemPtr->p_dataset_idx);
	
			** Execute the sql command **
			if ((status = execCmd (qDesc)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"op_cat__getPPSGranuleSize: data_kbytes retrieval failed.");
				return (status);
			}

			if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
				currItemPtr->data_kbytes = -1;
			else
			{
				(void) memcpy ((char *)&(currItemPtr->data_kbytes),
					qDesc->valAddr[0], qDesc->valLength[0]);
			}

			**
			** Re-initialize query descriptor for next command, but do
			** not cancel previous command
			**
			if (ims_qiResetDesc(qDesc) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"op_cat__getPPSGranuleSizeStepTime: Could not reinitialize query descriptor.");
				return(IMS_FATAL);
			}
		}

	}
	*******************************************************************/

	/*
	** get step_avgtime from the order_item_step table 
	*/
	(void) sprintf (qDesc->cmd,
			 "select step_avgtime from order_item_step where "
			 "order_item_type = %d and media_class = %d and "
			 "process_type = %d",
		currItemPtr->order_item_type, 
		currItemPtr->media_class,
		currItemPtr->process_type);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getPPSGranuleSizeStepTime: Step average time retrieval failed.");
		return (status);
	}

	(void) memcpy ((char *)currItemPtr->step_avgtime,
		qDesc->valAddr[0], qDesc->valLength[0]);
		currItemPtr->step_avgtime[qDesc->valLength[0]] = '\0';
		ims_truncStr (currItemPtr->step_avgtime);

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getPPSGranuleSizeStepTime: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/*
	** get acronym from platforms
	** 1/26/96 - if the order_item_type is DAR, platform information
	** is in the dar table.
	*/
	if (currItemPtr->order_item_type == DAR)
	{
		(void) sprintf (qDesc->cmd,
		 "select acronym from platforms t1, dar t2 where t2.order_id = %d "
		 "and t2.item_id = %d and t2.platform = t1.platform",
		 currItemPtr->order_id, currItemPtr->item_id);
	}
	else
	{
		(void) sprintf (qDesc->cmd,
			"select acronym from platforms t1 where t1.platform = '%s'",
			 currItemPtr->platform);
	}

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getPPSGranuleSizeStepTime: platform acronym retrieval failed.");
		return (status);
	}

	(void) memcpy ((char *)currItemPtr->platacronym,
		qDesc->valAddr[0], qDesc->valLength[0]);
		currItemPtr->platacronym[qDesc->valLength[0]] = '\0';
		ims_truncStr (currItemPtr->platacronym);


	return(IMS_OK);

} /* getPPSGranuleSizeStepTime */


/***********************************************************************
**
** getUserData - get the user and account info for an order
**
***********************************************************************/
static int getUserData (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ORDER_LIST *currOrder;
	OP_USER_ACCOUNT_DATA *orderUserData;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	currOrder = (OP_ORDER_LIST *) catReq->item[0];

	if (currOrder == (OP_ORDER_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getUserData: Invalid/Null OP_ORDER_LIST struct.");
		return (IMS_FATAL);
	}

	orderUserData = &(currOrder->userData);

	/*
	** get information from user_profile table, account table 
	*/
	(void) sprintf (qDesc->cmd,
			 "select distinct "
			 "first_name, initial_name, last_name, organization,"
			 "street, city, state, country, zipcode, phone, fax, email,"
			 "resource_type, curr_balance from user_profile, account where "
			 "user_id = '%s' and account_id = '%s'",
			 currOrder->user_id, currOrder->account_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getUserData: user profile data retrieval failed.");
		return (status);
	}

	(void) memcpy ((char *)orderUserData->first_name,
		qDesc->valAddr[0], qDesc->valLength[0]);
		orderUserData->first_name[qDesc->valLength[0]] = '\0';
		ims_truncStr (orderUserData->first_name);

	(void) memcpy ((char *)orderUserData->initial_name,
		qDesc->valAddr[1], qDesc->valLength[1]);
		orderUserData->initial_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (orderUserData->initial_name);

	(void) memcpy ((char *)orderUserData->last_name,
		qDesc->valAddr[2], qDesc->valLength[2]);
		orderUserData->last_name[qDesc->valLength[2]] = '\0';
		ims_truncStr (orderUserData->last_name);

	(void) memcpy ((char *)orderUserData->organization,
		qDesc->valAddr[3], qDesc->valLength[3]);
		orderUserData->organization[qDesc->valLength[3]] = '\0';
		ims_truncStr (orderUserData->organization);

	(void) memcpy ((char *)orderUserData->street,
		qDesc->valAddr[4], qDesc->valLength[4]);
		orderUserData->street[qDesc->valLength[4]] = '\0';
		ims_truncStr (orderUserData->street);

	(void) memcpy ((char *)orderUserData->city,
		qDesc->valAddr[5], qDesc->valLength[5]);
		orderUserData->city[qDesc->valLength[5]] = '\0';
		ims_truncStr (orderUserData->city);

	(void) memcpy ((char *)orderUserData->state,
		qDesc->valAddr[6], qDesc->valLength[6]);
		orderUserData->state[qDesc->valLength[6]] = '\0';
		ims_truncStr (orderUserData->state);

	(void) memcpy ((char *)orderUserData->country,
		qDesc->valAddr[7], qDesc->valLength[7]);
		orderUserData->country[qDesc->valLength[7]] = '\0';
		ims_truncStr (orderUserData->country);

	(void) memcpy ((char *)orderUserData->zipcode,
		qDesc->valAddr[8], qDesc->valLength[8]);
		orderUserData->zipcode[qDesc->valLength[8]] = '\0';
		ims_truncStr (orderUserData->zipcode);

	(void) memcpy ((char *)orderUserData->phone,
		qDesc->valAddr[9], qDesc->valLength[9]);
		orderUserData->phone[qDesc->valLength[9]] = '\0';
		ims_truncStr (orderUserData->phone);

	(void) memcpy ((char *)orderUserData->fax,
		qDesc->valAddr[10], qDesc->valLength[10]);
		orderUserData->fax[qDesc->valLength[10]] = '\0';
		ims_truncStr (orderUserData->fax);

	(void) memcpy ((char *)orderUserData->email,
		qDesc->valAddr[11], qDesc->valLength[11]);
		orderUserData->email[qDesc->valLength[11]] = '\0';
		ims_truncStr (orderUserData->email);

	if ((qDesc->valLength[12] == 0) || (qDesc->valAddr[12] == (char *)NULL))
		orderUserData->resource_type = -1;
	else
	{
		(void) memcpy ((char *)&(orderUserData->resource_type),
			qDesc->valAddr[12], qDesc->valLength[12]);
	}

	if ((qDesc->valLength[13] == 0) || (qDesc->valAddr[13] == (char *)NULL))
		orderUserData->curr_balance = -1;
	else
	{
		(void) memcpy ((char *)&(orderUserData->curr_balance),
			qDesc->valAddr[13], qDesc->valLength[13]);
	}


	return(IMS_OK);

} /* getUserData */


/******************************************************************************
**
** commitTransaction ()
**
** Commit the transaction opened by openTransaction.
**
******************************************************************************/

static int commitTransaction (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	(void) sprintf (qDesc->cmd, "commit transaction");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not commit transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** rollbackTransaction ()
**
** Rollback the transaction opened by openTransaction.
**
******************************************************************************/

static int rollbackTransaction (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Rollback this transaction.
	*/
	(void) sprintf (qDesc->cmd, "rollback transaction");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not rollback transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** execCmd ()
**
** Execute an SQL procedure that writes data into the catalog database.
** We don't pass a parameter, but assume that when this function is called,
** the declared static buffer 'qDesc->cmd' has been properly filled in with
** the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL NOT RETURN
** ROWS FROM THE DATABASE.
**
** If a deadlock occurs, reexecute the operation from the restart point.
**
******************************************************************************/

static int execCmd (IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	RETCODE status;
	int severity;

	msgDesc = qDesc->msgDesc;

	while ( (status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/*
		** Check the returned status
		*/
		if (status < IMS_OK)
		{
			/*
			** ims_qiNextRow returns a bad status value when no
			** row results returned from the stored procedure.
			** Therefore, the following is added to correct the
			** situation.  This situation must be resolved before
			** delivering the code.
			** 
			** Check the stored procedure status returned value.
			**
			** if ((severity = processRetStatus (qDesc)) < IMS_OK)
			** {
			** 	return (severity);
			** }
			*/

			return (status);
		}
	}

	/*
	** Check the stored procedure status returned value.
	*/
	if ((severity = processRetStatus (qDesc)) < IMS_OK)
	{
		return (severity);
	}

	if (qDesc->msgNo != 0)
	{
		return(ims_msgGetSeverity(msgDesc));
	}
	return (IMS_OK);
}

/****************************************************************************
**
** processRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
****************************************************************************/

static int processRetStatus (IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	int procReturn, severity;

	msgDesc = qDesc->msgDesc;

	/*
	** Check to see if the Sybase procedure returned a status.  If it did
	** and it is not 0 (the OK value for a return), deal with the error.
	** Return status of less than -100 correspond to message facility
	** severity levels modulo 100.
	** Return status of less than -200 correspond to stored procedure
	** error messages.
	*/
	if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
	{
		if ((procReturn = IMS_PROCRETURN (qDesc)) != 0)
		{
			switch (procReturn)
			{
				case -101:
					severity = IMS_WARNING;
					break;

				case -102:
					severity = IMS_ERROR;
					break;

				case -103:
					severity = IMS_FATAL;
					break;

				default:
					severity = IMS_ERROR;
					(void) ims_msg (msgDesc, severity,
					"Procedure '%s' returned an unrecognized status of '%d'.",
						qDesc->cmd, procReturn);
					break;
			}
			return (severity);
		}
	}
	return (IMS_OK);
}


/***********************************************************************
**
** getAccountIdList - get account Id list
**
***********************************************************************/
static int getAccountIdList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ACCOUNT_LIST *currPtr, *prevPtr;
	OP_ACCOUNT_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_ACCOUNT_LIST *)NULL;
	firstPtr = lastPtr = (OP_ACCOUNT_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_ACCOUNT_LIST *)
			malloc (sizeof (OP_ACCOUNT_LIST))) == (OP_ACCOUNT_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getAccountIdList: Memory allocation for OP_ACCOUNT_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_ACCOUNT_LIST *)NULL;
		currPtr->prev = (OP_ACCOUNT_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)currPtr->account_id,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->account_id[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->account_id);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getAccountIdList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_ACCOUNT_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getAccountIdList */


/***********************************************************************
**
** getUserIdList - get user Id list
**
***********************************************************************/
static int getUserIdList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_USER_LIST *currPtr, *prevPtr;
	OP_USER_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_USER_LIST *)NULL;
	firstPtr = lastPtr = (OP_USER_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_USER_LIST *)
			malloc (sizeof (OP_USER_LIST))) == (OP_USER_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getUserIdList: Memory allocation for OP_USER_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_USER_LIST *)NULL;
		currPtr->prev = (OP_USER_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)currPtr->user_id,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->user_id[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->user_id);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getUserIdList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_USER_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getUserIdList */


/***********************************************************************
**
** getPhotoQueueList - get photo queue listing  
**
***********************************************************************/
static int getPhotoQueueList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTO_QUEUE_LIST *currPtr, *prevPtr;
	OP_PHOTO_QUEUE_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_PHOTO_QUEUE_LIST *)NULL;
	firstPtr = lastPtr = (OP_PHOTO_QUEUE_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_PHOTO_QUEUE_LIST *)
			malloc (sizeof (OP_PHOTO_QUEUE_LIST))) == (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getPhotoQueueList: Memory allocation for OP_PHOTO_QUEUE_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_PHOTO_QUEUE_LIST *)NULL;
		currPtr->prev = (OP_PHOTO_QUEUE_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *)&(currPtr->photo_type),
			qDesc->valAddr[2], qDesc->valLength[2]);

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
			currPtr->photojob_id = -1;
		else
		{
			(void) memcpy ((char *)&currPtr->photojob_id,
				qDesc->valAddr[3], qDesc->valLength[3]);
		}

		(void) memcpy ((char *)&currPtr->quantity,
			qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy ((char *)(currPtr->quality),
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->quality[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->quality);

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((char *)currPtr->op_comment,
			qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->op_comment[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->op_comment);

		(void) memcpy ((char *)currPtr->user_id,
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->user_id[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->user_id);

		(void) memcpy ((char *)currPtr->product_id,
			qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->product_id[qDesc->valLength[9]] = '\0';
		ims_truncStr (currPtr->product_id);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getPhotoQueueList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getPhotoQueueList */



/******************************************************************************
**
** createPhotoJob ()
**
** Execute stored procedure op_create_photo_job 
**
******************************************************************************/

static int createPhotoJob (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int photo_type, no_items;
	int total_prints, photo_job_id;
	float total_cost;
	char work_order[IMS_COL10_LEN+1];
	char order_date[IMS_COL15_LEN+1];

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Execute stored procedure op_create_photo_job 
	*/
  photo_type 	 = 	*(int *)catReq->item[0];
  no_items		 = 	*(int *)catReq->item[1];
  total_prints = 	*(int *)catReq->item[2];
	*(int *)catReq->item[3] = 0;
	*(char *)catReq->item[4] = '\0';
	*(float *)catReq->item[5] = 0;
	*(char *)catReq->item[6] = '\0';

	(void) sprintf (qDesc->cmd, "op_create_photo_job %d, %d, %d",
												photo_type, no_items, total_prints);

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_create_photo_job failed.");
		return(IMS_FATAL);
	}

	/* Get the new job_id */
	(void) memcpy ((char *)&(photo_job_id),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	(void) memcpy ((char *)(work_order),
		 qDesc->valAddr[1], qDesc->valLength[1]);
	work_order[qDesc->valLength[1]] = '\0';
	ims_truncStr (work_order);

	(void) memcpy ((char *)&(total_cost),
		 qDesc->valAddr[2], qDesc->valLength[2]);

	(void) memcpy ((char *)(order_date),
		 qDesc->valAddr[3], qDesc->valLength[3]);
	order_date[qDesc->valLength[3]] = '\0';
	ims_truncStr (order_date);


	/* Return photo_job_id , work_order, total_cost */
	*(int *)catReq->item[3] = (int)photo_job_id;
  (void) strcpy ((char *)catReq->item[4], work_order);
	*(float *)catReq->item[5] = (float)total_cost;
  (void) strcpy ((char *)catReq->item[6], order_date);

	return(IMS_OK);

} /* createPhotoJob */


/******************************************************************************
**
** updatePhotoQueue()  
**
******************************************************************************/

static int updatePhotoQueue (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTO_QUEUE_LIST *qPtr, *tPtr;
	int photojob_id, status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	qPtr = tPtr = (OP_PHOTO_QUEUE_LIST *) catReq->item[0];
	photojob_id = *(int *)catReq->item[1]; 

	if (qPtr == (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoQueue: Invalid/Null OP_PHOTO_QUEUE_LIST struct.");
		return (IMS_FATAL);
	}

	while (tPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		(void) sprintf (qDesc->cmd,
			 "update photo_queue set photojob_id = %d, status = 2, quality = 'GOOD' "
			 "where order_id = %d and item_id = %d and "
			 "photojob_id = NULL",
			  photojob_id, tPtr->order_id, tPtr->item_id); 

		if ((status = execCmd (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat__updatePhotoQueue: update photo_queue failed.");
			return (status);
		}

		/* check to see if any update took place */
		if (IMS_AFFECTED (qDesc) <= 0)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_updatePhotoQueue: No row was affected in photo_queue table.");
			return(IMS_FATAL);
		}


		tPtr->photojob_id = photojob_id;
		tPtr = tPtr->next;

		/*
		** Re-initialize query descriptor for next command, but do
		** not cancel previous command
		*/
		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_updatePhotoQueue: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}

	}

	return(IMS_OK);

} /* updatePhotoQueue */


/******************************************************************************
**
** updatePhotoJob()
**
******************************************************************************/

static int updatePhotoJob (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char description[IMS_COL255_LEN+1];
	int photojob_id, status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	photojob_id = *(int *)catReq->item[0]; 
	*(char *)catReq->item[1] = '\0';

	(void) sprintf (qDesc->cmd,
			 "update photo_job set status = 2 where photojob_id = %d",
			  photojob_id); 

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoJob: update photo_job failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoJob: No row was affected in photo_job table.");
		return(IMS_FATAL);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoJob: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	(void) sprintf (qDesc->cmd,
	 	 "select description from photo_work_order where "
		 "photo_type = (select photo_type from photo_job where photojob_id = %d)",
		  photojob_id); 

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoJob: select description failed.");
		return (status);
	}

	(void) memcpy ((char *)(description),
		 qDesc->valAddr[0], qDesc->valLength[0]);
	description[qDesc->valLength[0]] = '\0';
	ims_truncStr (description);

  (void) strcpy ((char *)catReq->item[1], description);
	return(IMS_OK);

} /* updatePhotoJob */


/******************************************************************************
**
** getDatasetGranule() - given order_id, item_id, return the matching
**										   p_dataset_idx, p_granule_idx from order_item table
**
** Note:  12/06/1995 - This function is not used in schema version 3.30
** 
******************************************************************************/

static int getDatasetGranule (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int p_granule_idx, status;
	short int p_dataset_idx;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getDatasetGranule: data retrieval failed.");
		return (status);
	}

	/* Get the p_dataset_idx */
	(void) memcpy ((char *)&(p_dataset_idx),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	(void) memcpy ((char *)&(p_granule_idx),
		 qDesc->valAddr[1], qDesc->valLength[1]);

	/* Return p_dataset_idx, p_granule_idx */
	*(short int *)catReq->item[1] = p_dataset_idx;
	*(int *)catReq->item[2] = (int)p_granule_idx;

	return(IMS_OK);

} /* getDatasetGranule */


/******************************************************************************
**
** getGranulesTable() - given a dataset_idx, return the matching 
**											granules_table from dataset_policy table
**
** Note:  12/06/1995 - This function is not used in schema version 3.30
**
******************************************************************************/

static int getGranulesTable (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char granules_table[IMS_COL30_LEN+1];
	int status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];
	*(char *)catReq->item[1] = '\0';

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getGranulesTable: data retrieval failed.");
		return (status);
	}

	/* Get granules_table */
	(void) memcpy ((char *)(granules_table),
		 qDesc->valAddr[0], qDesc->valLength[0]);
	granules_table[qDesc->valLength[0]] = '\0';
	ims_truncStr (granules_table);

	/* Return granules_table */
  (void) strcpy ((char *)catReq->item[1], granules_table);

	return(IMS_OK);

} /* getGranulesTable */


/******************************************************************************
**
** getProductName() - given p_granule_idx, p_dataset_idx return the matching
**										name from granules_? table
**
** Note:  12/06/1995 - This function is not used in schema version 3.30
**
******************************************************************************/

static int getProductName (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char product_name [IMS_COL30_LEN+1];
	int status, p_granule_idx;
	short int p_dataset_idx;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];
	p_granule_idx = *(int *)catReq->item[1];
	p_dataset_idx = *(short int *)catReq->item[2];
	*(char *)catReq->item[3] = '\0';

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getProductName: data retrieval failed.");
		return (status);
	}

	/* Get product name */
	(void) memcpy ((char *)(product_name),
		 qDesc->valAddr[0], qDesc->valLength[0]);
	product_name[qDesc->valLength[0]] = '\0';
	ims_truncStr (product_name);

	/* Return product name */
  (void) strcpy ((char *)catReq->item[3], product_name);

	return(IMS_OK);

} /* getProductName */


/***********************************************************************
**
** getPhotoJobIdList - get PhotoJob Id list
**
***********************************************************************/
static int getPhotoJobIdList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTOJOB_ID_LIST *currPtr, *prevPtr;
	OP_PHOTOJOB_ID_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_PHOTOJOB_ID_LIST *)NULL;
	firstPtr = lastPtr = (OP_PHOTOJOB_ID_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_PHOTOJOB_ID_LIST *)
			malloc (sizeof (OP_PHOTOJOB_ID_LIST))) == (OP_PHOTOJOB_ID_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getPhotoJobIdList: Memory allocation for OP_PHOTOJOB_ID_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_PHOTOJOB_ID_LIST *)NULL;
		currPtr->prev = (OP_PHOTOJOB_ID_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&currPtr->photojob_id,
			qDesc->valAddr[0], qDesc->valLength[0]);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getPhotoJobIdList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_PHOTOJOB_ID_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getPhotoJobIdList */



/***********************************************************************
**
** getPhotoJobList - get complete photo job listing  
**
***********************************************************************/
static int getPhotoJobList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTO_JOB_LIST *currPtr, *prevPtr;
	OP_PHOTO_JOB_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_PHOTO_JOB_LIST *)NULL;
	firstPtr = lastPtr = (OP_PHOTO_JOB_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_PHOTO_JOB_LIST *)
			malloc (sizeof (OP_PHOTO_JOB_LIST))) == (OP_PHOTO_JOB_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getPhotoJobList: Memory allocation for OP_PHOTO_JOB_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_PHOTO_JOB_LIST *)NULL;
		currPtr->prev = (OP_PHOTO_JOB_LIST *)NULL;
		currPtr->photoQueueList = (OP_PHOTO_QUEUE_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->photojob_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)&(currPtr->photo_type),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *)&(currPtr->no_items),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((char *)&(currPtr->total_prints),
			qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((char *)&currPtr->total_cost,
			qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy ((char *)(currPtr->start_time),
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->start_time[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->start_time);

		(void) memcpy ((char *)(currPtr->end_time),
			qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->end_time[qDesc->valLength[6]] = '\0';
		ims_truncStr (currPtr->end_time);

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[7], qDesc->valLength[7]);

		(void) memcpy ((char *)currPtr->op_comment,
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->op_comment[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->op_comment);

		(void) memcpy ((char *)currPtr->work_order,
			qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->work_order[qDesc->valLength[9]] = '\0';
		ims_truncStr (currPtr->work_order);

		(void) memcpy ((char *)&currPtr->print_cost,
			qDesc->valAddr[10], qDesc->valLength[10]);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getPhotoJobList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_PHOTO_JOB_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getPhotoJobList */



/***********************************************************************
**
** updatePhotoComment - update the comment of a photo item
**
***********************************************************************/
static int updatePhotoComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	photoQPtr = (OP_PHOTO_QUEUE_LIST *) catReq->item[0];

	if (photoQPtr == (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoComment : Invalid/Null OP_PHOTO_QUEUE_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	/*
	** Update Item comment 
	*/
	(void) sprintf (qDesc->cmd,
			 "update photo_queue set op_comment = '%s' where order_id = %d and item_id = %d and photojob_id = %d",
			 text, photoQPtr->order_id, photoQPtr->item_id, photoQPtr->photojob_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoComment : update photo_queue comment failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoComment: No row was affected in photo_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updatePhotoComment */



/***********************************************************************
**
** updatePhotoQuality - update the quality of a photo item
**
***********************************************************************/
static int updatePhotoQuality (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	photoQPtr = (OP_PHOTO_QUEUE_LIST *) catReq->item[0];

	if (photoQPtr == (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoQuality : Invalid/Null OP_PHOTO_QUEUE_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** Update Photo Item Quality
	*/
	(void) sprintf (qDesc->cmd,
			 "update photo_queue set quality = '%s' where order_id = %d and item_id = %d and photojob_id = %d",
			 photoQPtr->quality, photoQPtr->order_id,
			 photoQPtr->item_id, photoQPtr->photojob_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoQuality : update photo_queue quality failed.");
		return (status);
	}


	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoQuality: No row was affected in photo_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updatePhotoQuality */


/******************************************************************************
**
** completePhotoJob()
**
******************************************************************************/

static int completePhotoJob (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status;
	OP_PHOTO_JOB_LIST *photoJobPtr;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;


	/* Input from the calling routine */
	photoJobPtr = (OP_PHOTO_JOB_LIST *) catReq->item[0];

	if (photoJobPtr == (OP_PHOTO_JOB_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoComment : Invalid/Null OP_PHOTO_QUEUE_LIST struct.");
		return (IMS_FATAL);
	}

	/* update photojob status to COMPLETE */
	(void) sprintf (qDesc->cmd,
									"update photo_job set status = 4 where photojob_id = %d",
									 photoJobPtr->photojob_id);

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_completePhotoJob: update photo_job status failed.");
		return (IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_completePhotoJob: No row was affected in photo_job table.");
		return(IMS_FATAL);
	}


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_completePhotoJob: Could not reinitialize query descriptor.");

		return(IMS_FATAL);
	}

	/* update photo_queue status to COMPLETE */
	(void) sprintf (qDesc->cmd,
								"update photo_queue set status = 4 where photojob_id = %d",
								 photoJobPtr->photojob_id);

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_completePhotoJob: update photo_queue status failed.");
		return (IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_completePhotoJob: No row was affected in photo_queue table.");
		return(IMS_FATAL);
	}


	return(IMS_OK);

} /* completePhotoJob */


/******************************************************************************
**
** updatePhotoJobCost()
**
******************************************************************************/

static int updatePhotoJobCost (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status;
	OP_PHOTO_JOB_LIST *photoJobPtr;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;


	/* Input from the calling routine */
	photoJobPtr = (OP_PHOTO_JOB_LIST *) catReq->item[0];

	if (photoJobPtr == (OP_PHOTO_JOB_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__updatePhotoJobCost : Invalid/Null OP_PHOTO_JOB_LIST struct.");
		return (IMS_FATAL);
	}

	/* adjust total_costs and total_prints */
	(void) sprintf (qDesc->cmd,
							"update photo_job set total_prints = %d, total_cost_$ = %f "
							"where photojob_id = %d",
							photoJobPtr->total_prints,
							photoJobPtr->total_cost,
						  photoJobPtr->photojob_id);

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoJobCost: update photo job prints, costs failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updatePhotoJobCost: No row was affected in photo_job table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updatePhotoJobCost */


/***********************************************************************
**
** getTapeItems - 
**
** Modifications: 04/09/1996 - data_kbytes is now p_data_kbytes in
**								order_item table.
**
***********************************************************************/
static int getTapeItems (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int order_id;
	OP_TAPE_ITEM_LIST *currPtr, *prevPtr;
	OP_TAPE_ITEM_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(int *)catReq->item[0];

	prevPtr = currPtr = (OP_TAPE_ITEM_LIST *)NULL;
	firstPtr = lastPtr = (OP_TAPE_ITEM_LIST *)NULL;

	(void) sprintf (qDesc->cmd,
		"select t1.order_id, t1.item_id, t1.status,"
		" t1.media_type, t1.media_fmt_type,"
		" t1.p_data_kbytes"
		" from order_item t1" 
		" where t1.order_id = %d"  
		" and t1.status = 8"
		" and t1.media_type in (1, 2, 3, 4, 5, 6, 7)"
		" order by order_id, item_id",
		order_id);


	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_TAPE_ITEM_LIST *)
			malloc (sizeof (OP_TAPE_ITEM_LIST))) == (OP_TAPE_ITEM_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getTapeItemList: Memory allocation for OP_TAPE_ITEM_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_TAPE_ITEM_LIST *)NULL;
		currPtr->prev = (OP_TAPE_ITEM_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[2], qDesc->valLength[2]);

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
			currPtr->media_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_type),
				qDesc->valAddr[3], qDesc->valLength[3]);
		}

		if ((qDesc->valLength[4] == 0) || (qDesc->valAddr[4] == (char *)NULL))
			currPtr->media_fmt_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_fmt_type),
				qDesc->valAddr[4], qDesc->valLength[4]);
		}

		if ((qDesc->valLength[5] == 0) || (qDesc->valAddr[5] == (char *)NULL))
			currPtr->data_kbytes = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->data_kbytes),
				qDesc->valAddr[5], qDesc->valLength[5]);
		}


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getTapeItems: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_TAPE_ITEM_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/*
	** 04/09/96 - change data_kbytes to p_data_kbytes, comment this
	** part out for now.
	*/

	/*********************************************************************
	** get p_data_kbytes for each item 
	**
	currPtr = firstPtr;
	while (currPtr != (OP_TAPE_ITEM_LIST *)NULL)
	{
		(void) sprintf (qDesc->cmd,
		 " select data_kbytes from %s where"
		 " granule_idx = (select p_granule_idx from order_item"
		 " where order_id = %d and item_id = %d)"
		 " and dataset_idx = (select p_dataset_idx from order_item"
		 " where order_id = %d and item_id = %d)",
			currPtr->granules_table, 
			currPtr->order_id, currPtr->item_id,
			currPtr->order_id, currPtr->item_id);

		** Execute the sql command **
		if ((status = execCmd (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_getTapeItems: getDataBytes failed.");
			return (IMS_FATAL);
		}

		if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
			currPtr->data_kbytes = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->data_kbytes),
				qDesc->valAddr[0], qDesc->valLength[0]);
		}

		**
		** Re-initialize query descriptor for next command, but do
		** not cancel previous command
		**
		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_getTapeItems: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}

		currPtr = currPtr->next;
	}
	*******************************************************************/


	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getTapeItems */



/***********************************************************************
**
** getItemStatus - 
**
***********************************************************************/
static int getItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int order_id;
	DBSMALLINT item_id, item_status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(int *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status = 0;

	/*
	** get the item status
	*/

	(void) sprintf (qDesc->cmd, "op_get_item_status %d, %d", order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_get_item_status failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&item_status,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)item_status;

	return(IMS_OK);

} /* getItemStatus */



/***********************************************************************
**
** rollbackItemStatus - 
**
***********************************************************************/
static int rollbackItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int order_id;
	DBSMALLINT item_id, item_status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(int *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status = 0;

	/*
	** update the item status
	*/

	(void) sprintf (qDesc->cmd, "rollback_item_status %d, %d", order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure rollback_item_status failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&item_status,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)item_status;

	return(IMS_OK);

} /* rollbackItemStatus */


/***********************************************************************
**
** getMediaCapacity - 
**
***********************************************************************/
static int getMediaCapacity (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int capacity;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	capacity = *(int *)catReq->item[1] = 0;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	/*
	** select capacity from media_policy where media_type = %d
	*/

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getMediaCapacity: execution of query failed.");
		return(IMS_FATAL);
	}

	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getMediaCapacity: media capacity not found in media_policy.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&capacity,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(int *)catReq->item[1] = (int)capacity;

	return(IMS_OK);

} /* getMediaCapacity */


/***********************************************************************
**
** getOrderStatus - 
**
***********************************************************************/
static int getOrderStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT order_status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];

	/*
	** get the order status
	*/

	(void) sprintf (qDesc->cmd, "op_get_order_status %d", order_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_get_order_status failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&order_status,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBSMALLINT *)catReq->item[1] = (DBSMALLINT)order_status;

	return(IMS_OK);

} /* getOrderStatus */


/***********************************************************************
**
** getItemStepSequence - 
**
***********************************************************************/
static int getItemStepSequence (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int order_id;
	DBSMALLINT item_id, step_sequence;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(int *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	step_sequence = 0;

	/*
	** get item step sequence
	*/

	(void) sprintf (qDesc->cmd, "op_get_step_sequence %d, %d", order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_get_step_sequence failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		step_sequence = 0;
	else
	{
		(void) memcpy ((char *)&step_sequence,
			 qDesc->valAddr[0], qDesc->valLength[0]);
	}

	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)step_sequence;

	return(IMS_OK);

} /* getItemStepSequence */


/***********************************************************************
**
** getItemAccountCost - 
**
***********************************************************************/
static int getItemAccountCost (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	char account_id[IMS_COL15_LEN+1];
	float cost;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	*(char *)catReq->item[1]  = '\0';
	*(float *)catReq->item[2]  = 0.0;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of getItemAccountCost query failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)(account_id),
								 qDesc->valAddr[0], qDesc->valLength[0]);
	account_id[qDesc->valLength[0]] = '\0';
	ims_truncStr (account_id);


	if ((qDesc->valLength[1] == 0) || (qDesc->valAddr[1] == (char *)NULL))
	{
		cost = 0.0;
	}
	else
	{
		(void) memcpy ((char *)&(cost),
			qDesc->valAddr[1], qDesc->valLength[1]);
	}

	/* Return account_id and cost */
  (void) strcpy ((char *)catReq->item[1], account_id);
	
	*(float *)catReq->item[2] = (float)cost;

	return(IMS_OK);

} /* getItemAccountCost */


/***********************************************************************
**
** getFireQueueList - get fire queue listing  
**
***********************************************************************/
static int getFireQueueList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_FIRE_QUEUE_LIST *currPtr, *prevPtr;
	OP_FIRE_QUEUE_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_FIRE_QUEUE_LIST *)NULL;
	firstPtr = lastPtr = (OP_FIRE_QUEUE_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_FIRE_QUEUE_LIST *)
			malloc (sizeof (OP_FIRE_QUEUE_LIST))) == (OP_FIRE_QUEUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getFireQueueList: Memory allocation for OP_FIRE_QUEUE_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_FIRE_QUEUE_LIST *)NULL;
		currPtr->prev = (OP_FIRE_QUEUE_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((char *)currPtr->op_comment,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->op_comment[qDesc->valLength[3]] = '\0';
		ims_truncStr (currPtr->op_comment);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getFireQueueList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_FIRE_QUEUE_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getFireQueueList */


/***********************************************************************
**
** getLaserQueueList - get laser queue listing  
**
***********************************************************************/
static int getLaserQueueList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_LASER_QUEUE_LIST *currPtr, *prevPtr;
	OP_LASER_QUEUE_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_LASER_QUEUE_LIST *)NULL;
	firstPtr = lastPtr = (OP_LASER_QUEUE_LIST *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_LASER_QUEUE_LIST *)
			malloc (sizeof (OP_LASER_QUEUE_LIST))) == (OP_LASER_QUEUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getLaserQueueList: Memory allocation for OP_LASER_QUEUE_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_LASER_QUEUE_LIST *)NULL;
		currPtr->prev = (OP_LASER_QUEUE_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *)&(currPtr->status),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((char *)currPtr->op_comment,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->op_comment[qDesc->valLength[3]] = '\0';
		ims_truncStr (currPtr->op_comment);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getLaserQueueList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_LASER_QUEUE_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getLaserQueueList */



/*************************************************************************
**
** getFireItemStatus - get the status of an item in the fire_queue
**
*************************************************************************/
static int getFireItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT item_status_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status_id = *(DBSMALLINT *)catReq->item[2];

	/*
	** get the item status from fire_queue table 
	*/

	(void) sprintf (qDesc->cmd,
		"select status from fire_queue where order_id = %d and item_id = %d",
			  order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getFireItemStatus: Get fire_queue item status failed.");
		return(IMS_FATAL);
	}

	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getFireItemStatus: Item status not found in fire_queue.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&item_status_id,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)item_status_id;

	return(IMS_OK);

} /* getFireItemStatus */



/*************************************************************************
**
** getLaserItemStatus - get the status of an item in the laser_queue
**
*************************************************************************/
static int getLaserItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT item_status_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status_id = *(DBSMALLINT *)catReq->item[2];

	/*
	** get the item status from laser_queue table 
	*/

	(void) sprintf (qDesc->cmd,
		"select status from laser_queue where order_id = %d and item_id = %d",
			  order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getLaserItemStatus: Get laser_queue item status failed.");
		return(IMS_FATAL);
	}

	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getLaserItemStatus: Item status not found in laser_queue.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&item_status_id,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)item_status_id;

	return(IMS_OK);

} /* getLaserItemStatus */



/*************************************************************************
**
** updateFireItemStatus - update the status of an item in the fire_queue
**
*************************************************************************/
static int updateFireItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT item_status_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status_id = *(DBSMALLINT *)catReq->item[2];

	/*
	** update the item status
	*/

	(void) sprintf (qDesc->cmd,
		"update fire_queue set status = %d where order_id = %d and item_id = %d",
	  item_status_id, order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateFireItemStatus: update fire_queue status failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateFireItemStatus: No row was affected in fire_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateFireItemStatus */



/**************************************************************************
**
** updateLaserItemStatus - update the status of an item in the laser_queue
**
**************************************************************************/
static int updateLaserItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT item_status_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status_id = *(DBSMALLINT *)catReq->item[2];

	/*
	** update the item status
	*/

	(void) sprintf (qDesc->cmd,
			"update laser_queue set status = %d where order_id = %d and item_id = %d",
		  item_status_id, order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateLaserItemStatus: update laser_queue status failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_updateLaserItemStatus: No row was affected in laser_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateLaserItemStatus */



/***********************************************************************
**
** updateFireItemComment - update the comment of an item
**
***********************************************************************/
static int updateFireItemComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	fireQPtr = (OP_FIRE_QUEUE_LIST *) catReq->item[0];

	if (fireQPtr == (OP_FIRE_QUEUE_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateFireItemComment : Invalid/Null OP_FIRE_QUEUE_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	/*
	** Update Item comment 
	*/
	(void) sprintf (qDesc->cmd,
			 "update fire_queue set op_comment = '%s' where order_id = %d and item_id = %d",
			 text, fireQPtr->order_id, fireQPtr->item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateFireItemComment : update fire queue comment failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateFireItemComment: No row was affected in fire_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateFireItemComment */



/***********************************************************************
**
** updateLaserItemComment - update the comment of an item in laser_queue
**
***********************************************************************/
static int updateLaserItemComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_LASER_QUEUE_LIST *laserQPtr;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	laserQPtr = (OP_LASER_QUEUE_LIST *) catReq->item[0];

	if (laserQPtr == (OP_LASER_QUEUE_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_updateLaserItemComment : Invalid/Null OP_LASER_QUEUE_LIST struct.");
		return (IMS_FATAL);
	}

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	/*
	** Update Item comment 
	*/
	(void) sprintf (qDesc->cmd,
			 "update laser_queue set op_comment = '%s' where order_id = %d and item_id = %d",
			 text, laserQPtr->order_id, laserQPtr->item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_updateLaserItemComment: update laser_queue comment failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_updateLaserItemComment: No row was affected in laser_queue table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateLaserItemComment */



/*************************************************************************
**
** removeFireQueueItem - remove an item from the fire_queue
**
** Modified:  04/30/96  We are now calling this function an item is
**                      being regenerated from the Photo Screen,
**                      since we are not sure whether the item is
**                      in the fire queue or not, the IMS_AFFECTED
**                      check will return IMS_WARNING if no row is
**                      affected.
**
*************************************************************************/
static int removeFireQueueItem(OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];

	/*
	** remove the item from fire_queue table
	*/

	(void) sprintf (qDesc->cmd,
		"delete fire_queue where order_id = %d and item_id = %d",
	  order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_removeFireQueueItem: Remove item from fire_queue failed.");
		return(IMS_FATAL);
	}

	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
		"op_cat_removeFireQueueItem: No rows found in fire_queue to be removed.");
		return(IMS_WARNING);
	}

	return(IMS_OK);

} /* removeFireQueueItem */


/*************************************************************************
**
** removeLaserQueueItem - remove an item from the laser_queue
**
** Modified:  04/30/96  We are now calling this function an item is
**                      being regenerated from the Photo Screen,
**                      since we are not sure whether the item is
**                      in the laser queue or not, the IMS_AFFECTED
**                      check will return IMS_WARNING if no row is
**                      affected.
**
*************************************************************************/
static int removeLaserQueueItem(OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];

	/*
	** remove the item from laser_queue table
	*/

	(void) sprintf (qDesc->cmd,
		"delete laser_queue where order_id = %d and item_id = %d",
	  order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_removeLaserQueueItem: remove item from laser_queue failed.");
		return(IMS_FATAL);
	}

	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
		"op_cat_removeLaserQueueItem: No rows found in laser_queue to be removed.");
		return(IMS_WARNING);
	}

	return(IMS_OK);

} /* removeLaserQueueItem */


/******************************************************************************
**
** getGeneralLock ()
**
** Execute stored procedure getGeneralLock 
**
******************************************************************************/

static int getGeneralLock (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Execute stored procedure getGeneralLock
	*/
	(void) sprintf (qDesc->cmd, "get_general_lock");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_general_lock failed.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}


/***********************************************************************
**
** getShipItems - 
**
***********************************************************************/
static int getShipItems (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_SHIP_ITEM_LIST *currPtr, *prevPtr;
	OP_SHIP_ITEM_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	prevPtr = currPtr = (OP_SHIP_ITEM_LIST *)NULL;
	firstPtr = lastPtr = (OP_SHIP_ITEM_LIST *)NULL;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_SHIP_ITEM_LIST *)
			malloc (sizeof (OP_SHIP_ITEM_LIST))) == (OP_SHIP_ITEM_LIST *)NULL)
		{
			ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getShipItemList: Memory allocation for OP_SHIP_ITEM_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_SHIP_ITEM_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)currPtr->name,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->name[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->name);

		if ((qDesc->valLength[2] == 0) || (qDesc->valAddr[2] == (char *)NULL))
			currPtr->cost = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->cost),
				qDesc->valAddr[2], qDesc->valLength[2]);
		}

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
			currPtr->process_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->process_type),
				qDesc->valAddr[3], qDesc->valLength[3]);
		}

		if ((qDesc->valLength[4] == 0) || (qDesc->valAddr[4] == (char *)NULL))
			currPtr->quantity = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->quantity),
				qDesc->valAddr[4], qDesc->valLength[4]);
		}

		if ((qDesc->valLength[5] == 0) || (qDesc->valAddr[5] == (char *)NULL))
			currPtr->media_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_type),
				qDesc->valAddr[5], qDesc->valLength[5]);
		}

		if ((qDesc->valLength[6] == 0) || (qDesc->valAddr[6] == (char *)NULL))
			currPtr->item_status = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->item_status),
				qDesc->valAddr[6], qDesc->valLength[6]);
		}

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getShipItems: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_SHIP_ITEM_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getShipItems */


/***********************************************************************
**
** verifyItemShippingStatus - 
**
***********************************************************************/
static int verifyItemShippingStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT order_id;
	DBSMALLINT item_id;
	int status; 
	int count;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	count = 0;

	(void) sprintf (qDesc->cmd,
		"select count(*) from order_item where "
		"order_id = %d and item_id = %d and status in (10, 12) and "
		"shipped_p = 'N' and shipping_id = NULL",
		order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_verifyItemShippingStatus: Execution of query failed.");
		return (status);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&(count),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[2] = (int)count;


	return(IMS_OK);

} /* verifyItemShippingStatus */



/***********************************************************************
**
** getShippingId - 
**
***********************************************************************/
static int getShippingId (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT shipping_id;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	shipping_id = *(DBINT *)catReq->item[0];

	/*
	** Get shipping_id by executing Hoshyar's incr_shipping_id 
	** stored procedure 
	*/

	(void) strcpy (qDesc->cmd, "incr_shipping_id");

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure incr_shipping_id failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&shipping_id,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBINT *)catReq->item[0] = (DBINT)shipping_id;

	return(IMS_OK);

} /* getShippingId */


/***********************************************************************
**
** getShippingData - get shipping and shipping profile information
**
***********************************************************************/
static int getShippingData (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_SHIPPING_DATA *shippingData;
	int status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	shippingData = (OP_SHIPPING_DATA *) catReq->item[0];

	if (shippingData == (OP_SHIPPING_DATA *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getShippingData: Invalid/Null OP_SHIPPING_DATA struct.");
		return (IMS_FATAL);
	}

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_get_shipping_data failed.");
		return(IMS_FATAL);
	}

	/* Get the returned data */
	(void) memcpy ((char *)&(shippingData->shipping_id),
		qDesc->valAddr[0], qDesc->valLength[0]);

	(void) memcpy ((char *)shippingData->shipping_time,
		qDesc->valAddr[1], qDesc->valLength[1]);
	shippingData->shipping_time[qDesc->valLength[1]] = '\0';
	ims_truncStr (shippingData->shipping_time);

	(void) memcpy ((char *)shippingData->carrier,
		qDesc->valAddr[2], qDesc->valLength[2]);
	shippingData->carrier[qDesc->valLength[2]] = '\0';
	ims_truncStr (shippingData->carrier);

	(void) memcpy ((char *)shippingData->op_comment,
		qDesc->valAddr[3], qDesc->valLength[3]);
	shippingData->op_comment[qDesc->valLength[3]] = '\0';
	ims_truncStr (shippingData->op_comment);

	(void) memcpy ((char *)&(shippingData->profile_id),
		qDesc->valAddr[4], qDesc->valLength[4]);

	(void) memcpy ((char *)shippingData->first_name,
		qDesc->valAddr[5], qDesc->valLength[5]);
	shippingData->first_name[qDesc->valLength[5]] = '\0';
	ims_truncStr (shippingData->first_name);

	(void) memcpy ((char *)shippingData->initial_name,
		qDesc->valAddr[6], qDesc->valLength[6]);
	shippingData->initial_name[qDesc->valLength[6]] = '\0';
	ims_truncStr (shippingData->initial_name);

	(void) memcpy ((char *)shippingData->last_name,
		qDesc->valAddr[7], qDesc->valLength[7]);
	shippingData->last_name[qDesc->valLength[7]] = '\0';
	ims_truncStr (shippingData->last_name);

	(void) memcpy ((char *)shippingData->title,
		qDesc->valAddr[8], qDesc->valLength[8]);
	shippingData->title[qDesc->valLength[8]] = '\0';
	ims_truncStr (shippingData->title);

	(void) memcpy ((char *)shippingData->organization,
		qDesc->valAddr[9], qDesc->valLength[9]);
	shippingData->organization[qDesc->valLength[9]] = '\0';
	ims_truncStr (shippingData->organization);

	(void) memcpy ((char *)shippingData->street,
		qDesc->valAddr[10], qDesc->valLength[10]);
	shippingData->street[qDesc->valLength[10]] = '\0';
	ims_truncStr (shippingData->street);

	(void) memcpy ((char *)shippingData->city,
		qDesc->valAddr[11], qDesc->valLength[11]);
	shippingData->city[qDesc->valLength[11]] = '\0';
	ims_truncStr (shippingData->city);

	(void) memcpy ((char *)shippingData->state,
		qDesc->valAddr[12], qDesc->valLength[12]);
	shippingData->state[qDesc->valLength[12]] = '\0';
	ims_truncStr (shippingData->state);

	(void) memcpy ((char *)shippingData->country,
		qDesc->valAddr[13], qDesc->valLength[13]);
	shippingData->country[qDesc->valLength[13]] = '\0';
	ims_truncStr (shippingData->country);

	(void) memcpy ((char *)shippingData->zipcode,
		qDesc->valAddr[14], qDesc->valLength[14]);
	shippingData->zipcode[qDesc->valLength[14]] = '\0';
	ims_truncStr (shippingData->zipcode);

	(void) memcpy ((char *)shippingData->phone,
		qDesc->valAddr[15], qDesc->valLength[15]);
	shippingData->phone[qDesc->valLength[15]] = '\0';
	ims_truncStr (shippingData->phone);

	(void) memcpy ((char *)shippingData->fax,
		qDesc->valAddr[16], qDesc->valLength[16]);
	shippingData->fax[qDesc->valLength[16]] = '\0';
	ims_truncStr (shippingData->fax);

	(void) memcpy ((char *)shippingData->email,
		qDesc->valAddr[17], qDesc->valLength[17]);
	shippingData->email[qDesc->valLength[17]] = '\0';
	ims_truncStr (shippingData->email);

	(void) memcpy ((char *)shippingData->account_id,
		qDesc->valAddr[18], qDesc->valLength[18]);
	shippingData->account_id[qDesc->valLength[18]] = '\0';
	ims_truncStr (shippingData->account_id);

	(void) memcpy ((char *)shippingData->order_date,
		qDesc->valAddr[19], qDesc->valLength[19]);
	shippingData->order_date[qDesc->valLength[19]] = '\0';
	ims_truncStr (shippingData->order_date);


	return(IMS_OK);

} /* getShippingData */


/***********************************************************************
**
** updateItemShippingStatus - update item's shipping flag and shipping id 
**
***********************************************************************/
static int updateItemShippingStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBINT shipping_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	shipping_id = *(DBINT *)catReq->item[2];

	/*
	** update shipped_p and shipping_id in order_item table for item
	*/

	(void) sprintf (qDesc->cmd, "op_update_item_shipping_status %d, %d, %d",
								 order_id, item_id, shipping_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_update_item_shipping_status failed.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateItemShippingStatus */


/***********************************************************************
**
** updateShippingComment - update the comment of an shipping entry in
**												 table shipping
**
***********************************************************************/
static int updateShippingComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT shipping_id;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	shipping_id = *(DBINT *) catReq->item[0];

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	/*
	** Update Shipping comment in table shipping
	*/
	(void) sprintf (qDesc->cmd,
			 "update shipping set op_comment = '%s' where shipping_id = %d",
			 text, shipping_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateShippingComment: Update shipping comment failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateShippingComment: No row was affected in shipping table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateShippingComment */



/***********************************************************************
**
** cancelShipment - clean up table shipping, shipping_of with the 
**									specified shipping_id, update order_item table
**								  set shipped_p = 'N' and shipping_id = NULL for 
**									items with the specified shipping_id.
**									This is done through stored procedure op_cancel_shipment
**
***********************************************************************/
static int cancelShipment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT shipping_id;
	int status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	shipping_id = *(DBINT *) catReq->item[0];

	/*
	** Update Shipping comment in table shipping
	*/
	(void) sprintf (qDesc->cmd, "op_cancel_shipment %d", shipping_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_cancel_shipment failed.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* cancelShipment */



/***********************************************************************
**
** getOrderShipIdList - 
**
***********************************************************************/
static int getOrderShipIdList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT order_id;
	OP_ORDER_SHIPID_LIST *currPtr, *prevPtr;
	OP_ORDER_SHIPID_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];

	prevPtr = currPtr = (OP_ORDER_SHIPID_LIST *)NULL;
	firstPtr = lastPtr = (OP_ORDER_SHIPID_LIST *)NULL;

	(void) sprintf (qDesc->cmd,
		"select distinct shipping_id, convert(char(12), create_time, 100) "
		"from shipping where order_id = %d order by shipping_id", order_id); 

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_ORDER_SHIPID_LIST *)
			malloc (sizeof (OP_ORDER_SHIPID_LIST))) == (OP_ORDER_SHIPID_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getOrderShipIdList: Memory allocation for OP_ORDER_SHIPID_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_ORDER_SHIPID_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->shipping_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)currPtr->create_time,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->create_time[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->create_time);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getOrderShipIdList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_ORDER_SHIPID_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getOrderShipIdLIST */



/***********************************************************************
**
** getBillItems - 
**
***********************************************************************/
static int getBillItems (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_BILL_ITEM_LIST *currPtr, *prevPtr;
	OP_BILL_ITEM_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	prevPtr = currPtr = (OP_BILL_ITEM_LIST *)NULL;
	firstPtr = lastPtr = (OP_BILL_ITEM_LIST *)NULL;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_BILL_ITEM_LIST *)
			malloc (sizeof (OP_BILL_ITEM_LIST))) == (OP_BILL_ITEM_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getBillItemList: Memory allocation for OP_BILL_ITEM_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_BILL_ITEM_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)currPtr->name,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->name[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->name);

		if ((qDesc->valLength[2] == 0) || (qDesc->valAddr[2] == (char *)NULL))
			currPtr->cost = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->cost),
				qDesc->valAddr[2], qDesc->valLength[2]);
		}

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
			currPtr->process_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->process_type),
				qDesc->valAddr[3], qDesc->valLength[3]);
		}

		if ((qDesc->valLength[4] == 0) || (qDesc->valAddr[4] == (char *)NULL))
			currPtr->quantity = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->quantity),
				qDesc->valAddr[4], qDesc->valLength[4]);
		}

		if ((qDesc->valLength[5] == 0) || (qDesc->valAddr[5] == (char *)NULL))
			currPtr->media_type = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->media_type),
				qDesc->valAddr[5], qDesc->valLength[5]);
		}

		if ((qDesc->valLength[6] == 0) || (qDesc->valAddr[6] == (char *)NULL))
			currPtr->item_status = -1;
		else
		{
			(void) memcpy ((char *)&(currPtr->item_status),
				qDesc->valAddr[6], qDesc->valLength[6]);
		}

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getBillItems: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_BILL_ITEM_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getBillItems */


/***********************************************************************
**
** verifyItemBillingStatus - 
**
***********************************************************************/
static int verifyItemBillingStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT order_id;
	DBSMALLINT item_id;
	int status; 
	int count;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	count = 0;

	(void) sprintf (qDesc->cmd,
		"select count(*) from order_item where "
		"order_id = %d and item_id = %d and status in (10, 12) and "
		"billed_p = 'N' and billing_id = NULL and shipped_p = 'Y' and "
		"shipping_id != NULL",
		order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&(count),
		 qDesc->valAddr[0], qDesc->valLength[0]);

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[2] = (int)count;


	return(IMS_OK);

} /* verifyItemBillingStatus */


/***********************************************************************
**
** getBillingData - get billing and billing profile information
**
***********************************************************************/
static int getBillingData (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_BILLING_DATA *billingData;
	int status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	billingData = (OP_BILLING_DATA *) catReq->item[0];

	if (billingData == (OP_BILLING_DATA *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getBillingData: Invalid/Null OP_BILLING_DATA struct.");
		return (IMS_FATAL);
	}

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_get_billing_data failed.");
		return(IMS_FATAL);
	}

	/* Get the returned data */
	(void) memcpy ((char *)&(billingData->billing_id),
		qDesc->valAddr[0], qDesc->valLength[0]);

	(void) memcpy ((char *)billingData->create_time,
		qDesc->valAddr[1], qDesc->valLength[1]);
	billingData->create_time[qDesc->valLength[1]] = '\0';
	ims_truncStr (billingData->create_time);

	(void) memcpy ((char *)billingData->op_comment,
		qDesc->valAddr[2], qDesc->valLength[2]);
	billingData->op_comment[qDesc->valLength[2]] = '\0';
	ims_truncStr (billingData->op_comment);

	(void) memcpy ((char *)&(billingData->profile_id),
		qDesc->valAddr[3], qDesc->valLength[3]);

	(void) memcpy ((char *)billingData->first_name,
		qDesc->valAddr[4], qDesc->valLength[4]);
	billingData->first_name[qDesc->valLength[4]] = '\0';
	ims_truncStr (billingData->first_name);

	(void) memcpy ((char *)billingData->initial_name,
		qDesc->valAddr[5], qDesc->valLength[5]);
	billingData->initial_name[qDesc->valLength[5]] = '\0';
	ims_truncStr (billingData->initial_name);

	(void) memcpy ((char *)billingData->last_name,
		qDesc->valAddr[6], qDesc->valLength[6]);
	billingData->last_name[qDesc->valLength[6]] = '\0';
	ims_truncStr (billingData->last_name);

	(void) memcpy ((char *)billingData->title,
		qDesc->valAddr[7], qDesc->valLength[7]);
	billingData->title[qDesc->valLength[7]] = '\0';
	ims_truncStr (billingData->title);

	(void) memcpy ((char *)billingData->organization,
		qDesc->valAddr[8], qDesc->valLength[8]);
	billingData->organization[qDesc->valLength[8]] = '\0';
	ims_truncStr (billingData->organization);

	(void) memcpy ((char *)billingData->street,
		qDesc->valAddr[9], qDesc->valLength[9]);
	billingData->street[qDesc->valLength[9]] = '\0';
	ims_truncStr (billingData->street);

	(void) memcpy ((char *)billingData->city,
		qDesc->valAddr[10], qDesc->valLength[10]);
	billingData->city[qDesc->valLength[10]] = '\0';
	ims_truncStr (billingData->city);

	(void) memcpy ((char *)billingData->state,
		qDesc->valAddr[11], qDesc->valLength[11]);
	billingData->state[qDesc->valLength[11]] = '\0';
	ims_truncStr (billingData->state);

	(void) memcpy ((char *)billingData->country,
		qDesc->valAddr[12], qDesc->valLength[12]);
	billingData->country[qDesc->valLength[12]] = '\0';
	ims_truncStr (billingData->country);

	(void) memcpy ((char *)billingData->zipcode,
		qDesc->valAddr[13], qDesc->valLength[13]);
	billingData->zipcode[qDesc->valLength[13]] = '\0';
	ims_truncStr (billingData->zipcode);

	(void) memcpy ((char *)billingData->phone,
		qDesc->valAddr[14], qDesc->valLength[14]);
	billingData->phone[qDesc->valLength[14]] = '\0';
	ims_truncStr (billingData->phone);

	(void) memcpy ((char *)billingData->fax,
		qDesc->valAddr[15], qDesc->valLength[15]);
	billingData->fax[qDesc->valLength[15]] = '\0';
	ims_truncStr (billingData->fax);

	(void) memcpy ((char *)billingData->email,
		qDesc->valAddr[16], qDesc->valLength[16]);
	billingData->email[qDesc->valLength[16]] = '\0';
	ims_truncStr (billingData->email);

	(void) memcpy ((char *)billingData->account_id,
		qDesc->valAddr[17], qDesc->valLength[17]);
	billingData->account_id[qDesc->valLength[17]] = '\0';
	ims_truncStr (billingData->account_id);

	(void) memcpy ((char *)billingData->order_date,
		qDesc->valAddr[18], qDesc->valLength[18]);
	billingData->order_date[qDesc->valLength[18]] = '\0';
	ims_truncStr (billingData->order_date);

	if ((qDesc->valLength[19] == 0) || (qDesc->valAddr[19] == (char *)NULL))
		billingData->resource_type = -1;
	else
	{
		(void) memcpy ((char *)&(billingData->resource_type),
			qDesc->valAddr[19], qDesc->valLength[19]);
	}

	if ((qDesc->valLength[20] == 0) || (qDesc->valAddr[20] == (char *)NULL))
		billingData->curr_balance = -1;
	else
	{
		(void) memcpy ((char *)&(billingData->curr_balance),
			qDesc->valAddr[20], qDesc->valLength[20]);
	}

	(void) memcpy ((char *)billingData->user_first_name,
		qDesc->valAddr[21], qDesc->valLength[21]);
	billingData->user_first_name[qDesc->valLength[21]] = '\0';
	ims_truncStr (billingData->user_first_name);

	(void) memcpy ((char *)billingData->user_initial_name,
		qDesc->valAddr[22], qDesc->valLength[22]);
	billingData->user_initial_name[qDesc->valLength[22]] = '\0';
	ims_truncStr (billingData->user_initial_name);

	(void) memcpy ((char *)billingData->user_last_name,
		qDesc->valAddr[23], qDesc->valLength[23]);
	billingData->user_last_name[qDesc->valLength[23]] = '\0';
	ims_truncStr (billingData->user_last_name);


	return(IMS_OK);

} /* getBillingData */


/***********************************************************************
**
** updateItemBillingStatus - update item's billing flag and billing id 
**
***********************************************************************/
static int updateItemBillingStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBINT billing_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	billing_id = *(DBINT *)catReq->item[2];

	/*
	** update billed_p and billing_id in order_item table for item
	*/

	(void) sprintf (qDesc->cmd, "op_update_item_billing_status %d, %d, %d",
								 order_id, item_id, billing_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_update_item_billing_status failed.");
		return(IMS_FATAL);
	}


	return(IMS_OK);

} /* updateItemBillingStatus */



/***********************************************************************
**
** getBillingId - 
**
***********************************************************************/
static int getBillingId (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT billing_id;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	billing_id = *(DBINT *)catReq->item[0];

	/*
	** Get billing_id by executing Hoshyar Sayah's incr_billing_id 
	** stored procedure 
	*/

	(void) strcpy (qDesc->cmd, "incr_billing_id");

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure incr_billing_id failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&billing_id,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBINT *)catReq->item[0] = (DBINT)billing_id;


	return(IMS_OK);

} /* getBillingId */


/***********************************************************************
**
** cancelBilling  - clean up table billing, billing_of with the 
**									specified billing_id, update order_item table
**								  set billed_p = 'N' and billing_id = NULL for 
**									items with the specified billing_id.
**									This is done through stored procedure op_cancel_billing
**
***********************************************************************/
static int cancelBilling (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT billing_id;
	int status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	billing_id = *(DBINT *) catReq->item[0];

	/*
	** execute stored procedure op_cancel_shipment 
	*/
	(void) sprintf (qDesc->cmd, "op_cancel_billing %d", billing_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure op_cancel_billing failed.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* cancelBilling */



/***********************************************************************
**
** getOrderBillIdList - 
**
***********************************************************************/
static int getOrderBillIdList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT order_id;
	OP_ORDER_BILLID_LIST *currPtr, *prevPtr;
	OP_ORDER_BILLID_LIST *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];

	prevPtr = currPtr = (OP_ORDER_BILLID_LIST *)NULL;
	firstPtr = lastPtr = (OP_ORDER_BILLID_LIST *)NULL;

	(void) sprintf (qDesc->cmd,
		"select distinct billing_id, convert(char(12), create_time, 100) "
		"from billing where order_id = %d order by billing_id", order_id); 

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_ORDER_BILLID_LIST *)
			malloc (sizeof (OP_ORDER_BILLID_LIST))) == (OP_ORDER_BILLID_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getOrderBillIdList: Memory allocation for OP_ORDER_BILLID_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next = (OP_ORDER_BILLID_LIST *)NULL;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->billing_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)currPtr->create_time,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->create_time[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->create_time);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getOrderBillIdList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (OP_ORDER_BILLID_LIST *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getOrderBillIdLIST */



/***********************************************************************
**
** getOpComment - used by shipping and billing screens to get op_comment
**
***********************************************************************/
static int getOpComment(OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	char text[IMS_COL512_LEN+1];

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	*(char *)catReq->item[1]  = '\0';

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getOpComment: data retrieval failed.");
		return (status);
	}

	(void) memcpy ((char *)text,
		qDesc->valAddr[0], qDesc->valLength[0]);
	text[qDesc->valLength[0]] = '\0';
	ims_truncStr (text);

	(void) strcpy ((char *)catReq->item[1], text);

	return(IMS_OK);

} /* getOpComment */



/***********************************************************************
**
** updateBillingComment - update the comment of an entry in
**												 table billing 
**
***********************************************************************/
static int updateBillingComment (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT billing_id;
	int status; 
	char text[IMS_COL512_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	billing_id = *(DBINT *) catReq->item[0];

	/*
	** 2/16/96 - Format the string to support single quote.
	*/

	(void) ims_formatQuotedString (catReq->item[1], text);
	text[255] = '\0';


	/*
	** Update Billing comment in table billing
	*/
	(void) sprintf (qDesc->cmd,
			 "update billing set op_comment = '%s' where billing_id = %d",
			 text, billing_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateBillingComment: update billing comment failed.");
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateBillingComment: No row was affected in billing table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateBillingComment */

/***********************************************************************
**
** getPhotoJobid - get photojob_id for an order item
**
***********************************************************************/
static int getPhotoJobid (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_PHOTOJOB_DATA *photojob;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	photojob = (OP_PHOTOJOB_DATA *) catReq->item[0];

	if (photojob == (OP_PHOTOJOB_DATA *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getPhotoJobid: Invalid/Null OP_PHOTOJOB_DATA struct.");
		return (IMS_FATAL);
	}


	/*
	** get information from photo_queue table 
	*/
	(void) sprintf (qDesc->cmd,
			 "select  photojob_id"
			 " from photo_queue where order_id = %d and item_id = %d",
			 photojob->order_id, photojob->item_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getPhotoJobid: execution of select photo_queue query failed.");
		return (status);
	}

	(void) memcpy ((char *)&photojob->photojob_id,
		qDesc->valAddr[0], qDesc->valLength[0]);


	return(IMS_OK);

} /* getPhotoJobid */



/***********************************************************************
**
** getDarData - get DAR data for an order item
**
***********************************************************************/
static int getDarData (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_DAR_DATA *darData;
	int status; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	darData = (OP_DAR_DATA *) catReq->item[0];

	if (darData == (OP_DAR_DATA *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getDarData: Invalid/Null OP_DAR_DATA struct.");
		return (IMS_FATAL);
	}


	/*
	** get information from dar table 
	*/
	(void) sprintf (qDesc->cmd,
			 "select platform, sensor, mode, asc_desc, "
			 "convert (char(20), start_date, 100), "
			 "convert (char(20), end_date, 100), "
			 "site_name, spatial_type, radius, center_lat, "
			 "center_lon, north_west_lat, north_west_lon, north_east_lat, "
			 "north_east_lon, south_west_lat, south_west_lon, south_east_lat, "
			 "south_east_lon, observation_freq, observation_num, pi_name, "
			 "pi_discipline, active_p, activity_start_date, activity_end_date, "
			 "status, user_comment, planner_comment, op_comment "
			 " from dar where order_id = %d and item_id = %d",
			 darData->order_id, darData->item_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getDarData: execution of select dar query failed.");
		return (status);
	}

	(void) memcpy ((char *)darData->platform,
		qDesc->valAddr[0], qDesc->valLength[0]);
		darData->platform[qDesc->valLength[0]] = '\0';
		ims_truncStr (darData->platform);

	(void) memcpy ((char *)darData->sensor,
		qDesc->valAddr[1], qDesc->valLength[1]);
		darData->sensor[qDesc->valLength[1]] = '\0';
		ims_truncStr (darData->sensor);

	(void) memcpy ((char *)darData->mode,
		qDesc->valAddr[2], qDesc->valLength[2]);
		darData->mode[qDesc->valLength[2]] = '\0';
		ims_truncStr (darData->mode);

	if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
		darData->asc_desc = (char)NULL;
	else
	{
		(void) memcpy ((char *)&darData->asc_desc,
			qDesc->valAddr[3], qDesc->valLength[3]);
	}

	(void) memcpy ((char *)darData->start_date,
		qDesc->valAddr[4], qDesc->valLength[4]);
		darData->start_date[qDesc->valLength[4]] = '\0';
		ims_truncStr (darData->start_date);

	(void) memcpy ((char *)darData->end_date,
		qDesc->valAddr[5], qDesc->valLength[5]);
		darData->end_date[qDesc->valLength[5]] = '\0';
		ims_truncStr (darData->end_date);

	(void) memcpy ((char *)darData->site_name,
		qDesc->valAddr[6], qDesc->valLength[6]);
		darData->site_name[qDesc->valLength[6]] = '\0';
		ims_truncStr (darData->site_name);

	if ((qDesc->valLength[7] == 0) || (qDesc->valAddr[7] == (char *)NULL))
		darData->spatial_type = -1;
	else
	{
		(void) memcpy ((char *)&(darData->spatial_type),
			qDesc->valAddr[7], qDesc->valLength[7]);
	}

	if ((qDesc->valLength[8] == 0) || (qDesc->valAddr[8] == (char *)NULL))
		darData->radius = -999;
	else
	{
		(void) memcpy ((char *)&(darData->radius),
			qDesc->valAddr[8], qDesc->valLength[8]);
	}

	if ((qDesc->valLength[9] == 0) || (qDesc->valAddr[9] == (char *)NULL))
		darData->center_lat = -999;
	else
	{
		(void) memcpy ((char *)&(darData->center_lat),
			qDesc->valAddr[9], qDesc->valLength[9]);
	}

	if ((qDesc->valLength[10] == 0) || (qDesc->valAddr[10] == (char *)NULL))
		darData->center_lon = -999;
	else
	{
		(void) memcpy ((char *)&(darData->center_lon),
			qDesc->valAddr[10], qDesc->valLength[10]);
	}

	if ((qDesc->valLength[11] == 0) || (qDesc->valAddr[11] == (char *)NULL))
		darData->north_west_lat = -999;
	else
	{
		(void) memcpy ((char *)&(darData->north_west_lat),
			qDesc->valAddr[11], qDesc->valLength[11]);
	}

	if ((qDesc->valLength[12] == 0) || (qDesc->valAddr[12] == (char *)NULL))
		darData->north_west_lon = -999;
	else
	{
		(void) memcpy ((char *)&(darData->north_west_lon),
			qDesc->valAddr[12], qDesc->valLength[12]);
	}

	if ((qDesc->valLength[13] == 0) || (qDesc->valAddr[13] == (char *)NULL))
		darData->north_east_lat = -999;
	else
	{
		(void) memcpy ((char *)&(darData->north_east_lat),
			qDesc->valAddr[13], qDesc->valLength[13]);
	}

	if ((qDesc->valLength[14] == 0) || (qDesc->valAddr[14] == (char *)NULL))
		darData->north_east_lon = -999;
	else
	{
		(void) memcpy ((char *)&(darData->north_east_lon),
			qDesc->valAddr[14], qDesc->valLength[14]);
	}

	if ((qDesc->valLength[15] == 0) || (qDesc->valAddr[15] == (char *)NULL))
		darData->south_west_lat = -999;
	else
	{
		(void) memcpy ((char *)&(darData->south_west_lat),
			qDesc->valAddr[15], qDesc->valLength[15]);
	}

	if ((qDesc->valLength[16] == 0) || (qDesc->valAddr[16] == (char *)NULL))
		darData->south_west_lon = -999;
	else
	{
		(void) memcpy ((char *)&(darData->south_west_lon),
			qDesc->valAddr[16], qDesc->valLength[16]);
	}

	if ((qDesc->valLength[17] == 0) || (qDesc->valAddr[17] == (char *)NULL))
		darData->south_east_lat = -999;
	else
	{
		(void) memcpy ((char *)&(darData->south_east_lat),
			qDesc->valAddr[17], qDesc->valLength[17]);
	}

	if ((qDesc->valLength[18] == 0) || (qDesc->valAddr[18] == (char *)NULL))
		darData->south_east_lon = -999;
	else
	{
		(void) memcpy ((char *)&(darData->south_east_lon),
			qDesc->valAddr[18], qDesc->valLength[18]);
	}

	(void) memcpy ((char *)darData->observation_freq,
		qDesc->valAddr[19], qDesc->valLength[19]);
		darData->observation_freq[qDesc->valLength[19]] = '\0';
		ims_truncStr (darData->observation_freq);

	if ((qDesc->valLength[20] == 0) || (qDesc->valAddr[20] == (char *)NULL))
		darData->observation_num = -1;
	else
	{
		(void) memcpy ((char *)&(darData->observation_num),
			qDesc->valAddr[20], qDesc->valLength[20]);
	}

	(void) memcpy ((char *)darData->pi_name,
		qDesc->valAddr[21], qDesc->valLength[21]);
		darData->pi_name[qDesc->valLength[21]] = '\0';
		ims_truncStr (darData->pi_name);

	(void) memcpy ((char *)darData->pi_discipline,
		qDesc->valAddr[22], qDesc->valLength[22]);
		darData->pi_discipline[qDesc->valLength[22]] = '\0';
		ims_truncStr (darData->pi_discipline);

	if ((qDesc->valLength[23] == 0) || (qDesc->valAddr[23] == (char *)NULL))
		darData->active_p = (char)NULL;
	else
	{
		(void) memcpy ((char *)&darData->active_p,
			qDesc->valAddr[23], qDesc->valLength[23]);
	}

	(void) memcpy ((char *)darData->activity_start_date,
		qDesc->valAddr[24], qDesc->valLength[24]);
		darData->activity_start_date[qDesc->valLength[24]] = '\0';
		ims_truncStr (darData->activity_start_date);

	(void) memcpy ((char *)darData->activity_end_date,
		qDesc->valAddr[25], qDesc->valLength[25]);
		darData->activity_end_date[qDesc->valLength[25]] = '\0';
		ims_truncStr (darData->activity_end_date);

	if ((qDesc->valLength[26] == 0) || (qDesc->valAddr[26] == (char *)NULL))
		darData->status = -1;
	else
	{
		(void) memcpy ((char *)&(darData->status),
			qDesc->valAddr[26], qDesc->valLength[26]);
	}

	(void) memcpy ((char *)darData->user_comment,
		qDesc->valAddr[27], qDesc->valLength[27]);
		darData->user_comment[qDesc->valLength[27]] = '\0';
		ims_truncStr (darData->user_comment);

	(void) memcpy ((char *)darData->planner_comment,
		qDesc->valAddr[28], qDesc->valLength[28]);
		darData->planner_comment[qDesc->valLength[28]] = '\0';
		ims_truncStr (darData->planner_comment);

	(void) memcpy ((char *)darData->op_comment,
		qDesc->valAddr[29], qDesc->valLength[29]);
		darData->op_comment[qDesc->valLength[29]] = '\0';
		ims_truncStr (darData->op_comment);

	return(IMS_OK);

} /* getDarData */



/*************************************************************************
**
** getPhotoItemStatus - get the status of an item in the photo_queue
**
*************************************************************************/
static int getPhotoItemStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT item_status_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	item_status_id = *(DBSMALLINT *)catReq->item[2];

	/*
	** get the item status from fire_queue table 
	*/

	(void) sprintf (qDesc->cmd,
		"select status from photo_queue where order_id = %d and item_id = %d",
			  order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getPhotoItemStatus: get photo_queue item status failed.");
		return(IMS_FATAL);
	}

	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getPhotoItemStatus: Item status not found in photo_queue.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	(void) memcpy ((char *)&item_status_id,
		 qDesc->valAddr[0], qDesc->valLength[0]);

	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)item_status_id;

	return(IMS_OK);

} /* getPhotoItemStatus */



/***********************************************************************
**
** updateItemMediaId - update the media Id of an item.
**
***********************************************************************/
static int updateItemMediaId (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	DBINT order_id;
	DBSMALLINT item_id; 
	int status; 
	char mediaId[IMS_COL15_LEN+1];


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* Input from the calling routine */
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	(void) strcpy (mediaId, catReq->item[2]);

	/*
	** Update Media Id in table order_item
	*/
	(void) sprintf (qDesc->cmd,
  "update order_item set media_id = '%s' where order_id = %d and item_id = %d",
  mediaId, order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemMediaId: Update media id in order_item table failed"
			" for order %d, item %d", order_id, item_id);
		return (status);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemMediaId: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateItemMediaId */



/***********************************************************************
**
** getUserType 
**
***********************************************************************/
static int getUserType (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBSMALLINT user_type;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	user_type = *(DBSMALLINT *)catReq->item[1] = 0;

	/* query to be executed is input through catReq->item[0] */
	qDesc->cmd = (char *)catReq->item[0];

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_getUserType: Retrieving user_type from table user_profile failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		user_type = 0;
	else
	{
		(void) memcpy ((char *)&(user_type),
			qDesc->valAddr[0], qDesc->valLength[0]);
	}

	*(DBSMALLINT *)catReq->item[1] = (DBSMALLINT)user_type;

	return(IMS_OK);

} /* getUserType */


/*************************************************************************
**
** updateCostDebitFlag - update the cost_debited_p in order_item table
**
*************************************************************************/
static int updateCostDebitFlag (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];

	/*
	** update the item status
	*/

	(void) sprintf (qDesc->cmd,
		"update order_item set cost_debited_p = 'Y' where order_id = %d and item_id = %d",
	  order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_cat_updateCostDebitFlag: update cost_debited_p in order_item failed.");
		return(IMS_FATAL);
	}

	/* check to see if any update took place */
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateCostDebitFlag: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return(IMS_OK);

} /* updateCostDebitFlag */



/***********************************************************************
**
** updateItemStepInfo - update the order item step_sequence, step_name 
**                  		and flag step_started_p
**
** 04/26/96 - J. Ting
**
***********************************************************************/
static int updateItemStepInfo(OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBSMALLINT step_sequence; 


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	step_sequence = *(DBSMALLINT *)catReq->item[2];

	/*
	** update the item step_sequence, step_name and step_started_p.
	*/

	(void) sprintf (qDesc->cmd,
		 "update order_item set step_sequence = %d, step_name = NULL, step_started_p = 'N' where order_id = %d and item_id = %d", step_sequence, order_id, item_id);


	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemStepInfo: update order_item step_sequence, step_name, step_started_p failed.");
		return(IMS_FATAL);
	}


	/* return error if no row was affected */
	if (IMS_AFFECTED (qDesc) != 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateItemStepInfo: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	return (IMS_OK);

} /* updateItemStepInfo */



/***********************************************************************
**
** getOrderItemInfo - This function is created to get order_item_type,
**                    process_type and media_class information for items
**                    in the Film Generation and Photo Job screens.
**
** 04/26/1996       - J. Ting
**
***********************************************************************/
static int getOrderItemInfo (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int order_id;
	DBSMALLINT item_id, process_type;
	DBSMALLINT order_item_type, media_class;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(int *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];

	/*
	** get the following order item information: order_item_type, 
	** process_type and media_class.
	*/

	(void) sprintf (qDesc->cmd, "select order_item_type, process_type, media_class from order_item where order_id = %d and item_id = %d", order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getOrderItemInfo: cannot retrieve order item information.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		order_item_type = -1;
	else
	{
		(void) memcpy ((char *)&order_item_type,
			 qDesc->valAddr[0], qDesc->valLength[0]);
	}

	if ((qDesc->valLength[1] == 0) || (qDesc->valAddr[1] == (char *)NULL))
		process_type = -1;
	else
	{
		(void) memcpy ((char *)&process_type,
			 qDesc->valAddr[1], qDesc->valLength[1]);
	}

	if ((qDesc->valLength[2] == 0) || (qDesc->valAddr[2] == (char *)NULL))
		media_class = -1;
	else
	{
		(void) memcpy ((char *)&media_class,
			 qDesc->valAddr[2], qDesc->valLength[2]);
	}


	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)order_item_type;
	*(DBSMALLINT *)catReq->item[3] = (DBSMALLINT)process_type;
	*(DBSMALLINT *)catReq->item[4] = (DBSMALLINT)media_class;

	return(IMS_OK);

} /* getOrderItemInfo */



/***********************************************************************
**
** getItemStepInfo - executes stored procedure get_step_info to 
**                   get the step_sequence.  Note that we ignore
**                   step_name, start_status and end_status returned
**                   from the stored procedure.
**
** 04/26/1996      - J. Ting
**
***********************************************************************/
static int getItemStepInfo (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBSMALLINT order_item_type, media_class;
	DBSMALLINT process_type, start_status;
	DBSMALLINT step_sequence;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_item_type = *(DBSMALLINT *)catReq->item[0];
	media_class = *(DBSMALLINT *)catReq->item[1];
	process_type = *(DBSMALLINT *)catReq->item[2];
	start_status = *(DBSMALLINT *)catReq->item[3];

	/*
	** get item step sequence
	*/

	(void) sprintf (qDesc->cmd, "get_step_info %d, %d, %d, %d", order_item_type, media_class, process_type, start_status);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_step_info failed.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		step_sequence = -1;
	else
	{
		(void) memcpy ((char *)&step_sequence,
			 qDesc->valAddr[0], qDesc->valLength[0]);
	}

	*(DBSMALLINT *)catReq->item[4] = (DBSMALLINT)step_sequence;

	return(IMS_OK);

} /* getItemStepInfo */


/***********************************************************************
**
** getStepNamePPSStatus - This function is created to get the process 
**                        status and step name of an item selected to be 
**                        cancelled.
**
** 05/09/1996       - J. Ting
**
***********************************************************************/
static int getStepNamePPSStatus (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int order_id;
	DBSMALLINT item_id;
	DBSMALLINT process_status;
	DBCHAR step_name[IMS_COL30_LEN+1];

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(int *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];

	/*
	** get the following order item information: process_status, 
	** and step_name.
	*/

	(void) sprintf (qDesc->cmd, "select process_status, step_name from order_item where order_id = %d and item_id = %d", order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getStepNamePPSStatus: cannot retrieve order item information.");
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		process_status = -1;
	else
	{
		(void) memcpy ((char *)&process_status,
			 qDesc->valAddr[0], qDesc->valLength[0]);
	}

	(void) memcpy ((char *)step_name, qDesc->valAddr[1], qDesc->valLength[1]);
	step_name[qDesc->valLength[1]] = '\0';
	ims_truncStr (step_name);


	*(DBSMALLINT *)catReq->item[2] = (DBSMALLINT)process_status;
	(void) strcpy ((char *)catReq->item[3], step_name);

	return(IMS_OK);

} /* getStepNamePPSStatus */



/***********************************************************************
**
** restartItem - update the following in order_item table to restart
**               an item: status, validated_p, process_status, 
**               step_name, step_sequence, and step_started_p.
**
** 07/12/96 - J. Ting
** 10/29/96 -    Modified to update process_status in a new update stmt.
**               This is to correct PR 2184.
**
***********************************************************************/
static int restartItem(OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	DBINT order_id;
	DBSMALLINT item_id; 
	DBCHAR op_validate_p;
	DBCHAR account_id[IMS_COL30_LEN+1];

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	order_id = *(DBINT *)catReq->item[0];
	item_id = *(DBSMALLINT *)catReq->item[1];
	(void) strcpy (account_id, catReq->item[2]);

  /*
	** 11/11/96 - PR 2184.  Let's get op_validate_p value from
	** the account table for this order.  If op_validate_p is 'N',
	** change validate_p to 'Y' for the selected items.  If 
	** op_validate_p is 'Y', we leave the items to the operators
	** to manully validate them.
	*/
	(void) sprintf (qDesc->cmd, 
	 "select op_validate_p from account where account_id = '%s'",
	 account_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: Could not retrieve op_validate_p value for Order Id: %d", order_id);
		return(IMS_FATAL);
	}

	/* copy in the returned data */
	if ((qDesc->valLength[0] == 0) || (qDesc->valAddr[0] == (char *)NULL))
		op_validate_p = 'N';
	else
	{
		(void) memcpy ((char *)&op_validate_p,
			 qDesc->valAddr[0], qDesc->valLength[0]);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: Could not reinitialize query descriptor.");

		return(IMS_FATAL);
	}

	/*
	** Update the following in order_item table: status, validated_p,
	** process_status, step_name, step_sequence, and step_started_p.
	**
	** 11/11/96  PR 2184 - if op_validate_p from the account table 
	** is 'Y', we leave the item to the operator to perform validation.
	** If op_validate_p from the account table is 'N', we validate
	** the item here by setting the validated_p flag in order_item
	** table to Y.
	*/

	if (op_validate_p == 'Y')
	{
		(void) sprintf (qDesc->cmd,
	 	"update order_item set status = 1, validated_p = 'N', "
	 	"step_name = NULL, step_sequence = 0, step_started_p = 'N' where "
	 	"order_id = %d and item_id = %d", order_id, item_id);
	}
	else
	{
		(void) sprintf (qDesc->cmd,
	 	"update order_item set status = 2, validated_p = 'Y', "
	 	"step_name = NULL, step_sequence = 0, step_started_p = 'N' where "
	 	"order_id = %d and item_id = %d", order_id, item_id);
	}

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: update order_item table failed. for Order Id: %d, Item Id: %d", order_id, item_id);
		return(IMS_FATAL);
	}


	/* return error if no row was affected */
	if (IMS_AFFECTED (qDesc) != 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: Could not reinitialize query descriptor.");

		return(IMS_FATAL);
	}

	/*
	** Update the following in order_item table: process_status.
	*/

	(void) sprintf (qDesc->cmd,
	 "update order_item set process_status = 0 where "
	 "order_id = %d and item_id = %d", order_id, item_id);

	/* Execute the sql command */
	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: update process_status failed. for Order Id: %d, Item Id: %d", order_id, item_id);
		return(IMS_FATAL);
	}


	/* return error if no row was affected */
	if (IMS_AFFECTED (qDesc) != 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_restartItem: No row was affected in order_item table.");
		return(IMS_FATAL);
	}

	*(DBCHAR *)catReq->item[3] = (DBCHAR)op_validate_p;
	return (IMS_OK);

} /* restartItem */


/***********************************************************************
**
** getKeywordValueInfo - 	gets the values from the keyword_value table
**												for the following keywords:
**														platform, sensor, activity id,
**														station id, antenna id, transmitter id,
**														and downlink status
**
***********************************************************************/
static int
getKeywordValueInfo (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ	*qDesc;
	IMS_MSG_STRUCT	*msgDesc;
	int							status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* 
	** Get platform acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'PLATFORM'" ) ;
	glbData.platform = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.platform_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getKeywordValueInfo: Could not reinitialize query descriptor.");
		return( IMS_FATAL );
	}

	/* 
	** Get sensor acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'SENSOR'" ) ;
	glbData.sensor = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.sensor_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getKeywordValueInfo: Could not reinitialize query descriptor.");
		return( IMS_FATAL );
	}

	/* 
	** Get activity id acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'ACTIVITY_ID'" ) ;
	glbData.activity_id = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.activity_id_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getKeywordValueInfo: Could not reinitialize query descriptor.");
		return( IMS_FATAL );
	}

	/* 
	** Get station id acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'STATION_ID'" ) ;
	glbData.station_id = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.station_id_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getKeywordValueInfo: Could not reinitialize query descriptor.");
		return( IMS_FATAL );
	}

	/* 
	** Get antenna id acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'ANTENNA_ID'" ) ;
	glbData.antenna_id = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.antenna_id_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getKeywordValueInfo: Could not reinitialize query descriptor.");
		return( IMS_FATAL );
	}

	/* 
	** Get transmitter id acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'TRANSMITTER_ID'" ) ;
	glbData.transmitter_id = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.transmitter_id_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_getKeywordValueInfo: Could not reinitialize query descriptor.");
		return( IMS_FATAL );
	}

	/* 
	** Get downlink status acronym valids
	*/
	(void) strcpy( qDesc->cmd, "op_get_keyword_value 'DOWNLINK_STATUS'" ) ;
	glbData.downlink_status = setValuesFromKeyword ( qDesc, msgDesc,
		&glbData.downlink_status_count, &status ) ;
	if (status < IMS_OK)	/* any errors, stop (msg was already output) */
		return (NULL) ;

	return (IMS_OK) ;
}	/* getKeywordValueInfo */

/***********************************************************************
**
** getDownlinkList - get downlink entry listing  
**
***********************************************************************/
static int
getDownlinkList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ		*qDesc;
	IMS_MSG_STRUCT		*msgDesc;
	OP_DL_LIST				*firstPtr, *lastPtr;
	OP_DL_LIST				*currPtr;
	int								status; 
	int								rowCount;

	qDesc										= catReq->qDesc;
	msgDesc									= catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd							= (char *)catReq->item[1];

	firstPtr								= lastPtr = (OP_DL_LIST *)NULL;
	*(int *)catReq->item[0]	= (int)0;
	catReq->item[2]					= (void *)NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
			return (status);

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_DL_LIST *)
			malloc (sizeof (OP_DL_LIST))) == (OP_DL_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat__getDownlinkList: Memory allocation for OP_DL_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->dtkList		= NULL;
		currPtr->dtkChanged	= NULL;
		currPtr->next				= NULL;
		currPtr->prev				= NULL;
		currPtr->position		= rowCount;
		currPtr->selectFlag	= 0;

		rowCount += 1;	/* now can update row count, a row was returned */

		/* copy in the returned data */
		(void) memcpy ((char *)&(currPtr->platform),
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->platform[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->platform);

		(void) memcpy ((char *)currPtr->sensor,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->sensor[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((char *)&(currPtr->revolution),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((char *)&(currPtr->sequence),
			qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((char *)currPtr->downlink_status,
			qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->downlink_status[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->downlink_status);

		(void) memcpy ((char *)&(currPtr->activity_id),
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->activity_id[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->activity_id);

		(void) memcpy ((char *)&(currPtr->station_id),
			qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->station_id[qDesc->valLength[6]] = '\0';
		ims_truncStr (currPtr->station_id);

		(void) memcpy ((char *)&(currPtr->antenna_id),
			qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->antenna_id[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->antenna_id);

		(void) memcpy ((char *)&(currPtr->transmitter_id),
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->transmitter_id[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->transmitter_id);

		(void) memcpy ((char *)&(currPtr->fa_schedule_link),
			qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->fa_schedule_link[qDesc->valLength[9]] = '\0';
		ims_truncStr (currPtr->fa_schedule_link);

		(void) memcpy ((char *)&(currPtr->time_on),
			qDesc->valAddr[10], qDesc->valLength[10]);
		currPtr->time_on[qDesc->valLength[10]] = '\0';
		ims_truncStr (currPtr->time_on);

		(void) memcpy ((char *)&(currPtr->time_off),
			qDesc->valAddr[11], qDesc->valLength[11]);
		currPtr->time_off[qDesc->valLength[11]] = '\0';
		ims_truncStr (currPtr->time_off);

		(void) memcpy ((char *)&(currPtr->time_aos),
			qDesc->valAddr[12], qDesc->valLength[12]);
		currPtr->time_aos[qDesc->valLength[12]] = '\0';
		ims_truncStr (currPtr->time_aos);

		(void) memcpy ((char *)&(currPtr->time_los),
			qDesc->valAddr[13], qDesc->valLength[13]);
		currPtr->time_los[qDesc->valLength[13]] = '\0';
		ims_truncStr (currPtr->time_los);

		(void) memcpy ((char *)&(currPtr->number_of_dtk_entry),
			qDesc->valAddr[14], qDesc->valLength[14]);

		(void) memcpy ((char *)&(currPtr->received_time),
			qDesc->valAddr[15], qDesc->valLength[15]);
		currPtr->received_time[qDesc->valLength[15]] = '\0';
		ims_truncStr (currPtr->received_time);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getDownlinkList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* getDownlinkList */


/***********************************************************************
**
** getDownlinkDTKList - get DTK listing for a specified downlink. 
**
***********************************************************************/
static int
getDownlinkDTKList (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ	*qDesc;
	IMS_MSG_STRUCT	*msgDesc;
	OP_DL_DTK_LIST	*firstPtr, *lastPtr;
	OP_DL_DTK_LIST	*currPtr;
	OP_DL_LIST			*currDL;
	int							status; 
	int							rowCount;

	qDesc		= catReq->qDesc;
	msgDesc	= catReq->msgDesc;
	currPtr	= firstPtr = lastPtr = NULL ;

	/* Get the input from the calling routine */
	currDL = (OP_DL_LIST *) catReq->item[0];
	if (currDL == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getDownlinkDTKList: Invalid/Null OP_DL_LIST struct.");
		return (IMS_FATAL);
	}

	/* formulate the Sybase query */
	(void) sprintf( qDesc->cmd,
		"select t1.PLATFORM, t1.SENSOR, t1.REVOLUTION,"
		" t1.SEQUENCE, t1.DT_SENSOR,"
		" t1.DT_REVOLUTION, t1.DT_SEQUENCE, t1.DT_PLATFORM,\n"
		" t1.QUICKLOOK_FLAG, t1.PROCESS_AUTH_FLAG, t1.MODE,"
		" t1.FRAME_MODE, t1.TIME_ON, t1.TIME_OFF,"
		" t1.SITE_NAME, convert( char(20), t1.updated_time, 100 )" ) ;
	/* NOTE: sensor is included, below, for backwards compatibility only */
	(void) sprintf( qDesc->cmd, "%s\n"
		"from datatake_entry t1\n"
		"where t1.PLATFORM = '%s' and t1.SENSOR = '%s'"
		" and t1.REVOLUTION = %d and t1.SEQUENCE = %d\n"
		"order by t1.DT_SENSOR, t1.DT_REVOLUTION, t1.DT_SEQUENCE",
		qDesc->cmd, currDL->platform, currDL->sensor,
		currDL->revolution, (int) currDL->sequence ) ;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
			return (status);

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_DL_DTK_LIST *) malloc( sizeof (OP_DL_DTK_LIST) ))
				== NULL)
		{
			(void) ims_msg( msgDesc, IMS_FATAL, 
				"op_cat__getDownlinkDTKList: Memory allocation"
				" for OP_ORDER_ITEM_LIST failed." );
			return (IMS_FATAL);
		}

		currPtr->next = currPtr->prev = (OP_DL_DTK_LIST *)NULL;
		currPtr->position = rowCount;

		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */

		(void) memcpy ((char *)&(currPtr->platform),
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->platform[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->platform);

		(void) memcpy ((char *)currPtr->sensor,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->sensor[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((char *)&(currPtr->revolution),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((char *)&(currPtr->sequence),
			qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((char *)currPtr->dt_sensor,
			qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->dt_sensor[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->dt_sensor);

		(void) memcpy ((char *)&(currPtr->dt_revolution),
			qDesc->valAddr[5], qDesc->valLength[5]);

		(void) memcpy ((char *)&(currPtr->dt_sequence),
			qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((char *)&(currPtr->dt_platform),
			qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->dt_platform[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->dt_platform);

		(void) memcpy ((char *)&(currPtr->quicklook),
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->quicklook[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->quicklook);

		(void) memcpy ((char *)&(currPtr->process_auth_flag),
			qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->process_auth_flag[qDesc->valLength[9]] = '\0';
		ims_truncStr (currPtr->process_auth_flag);

		(void) memcpy ((char *)&(currPtr->mode),
			qDesc->valAddr[10], qDesc->valLength[10]);
		currPtr->mode[qDesc->valLength[10]] = '\0';
		ims_truncStr (currPtr->mode);

		(void) memcpy ((char *)&(currPtr->frame_mode),
			qDesc->valAddr[11], qDesc->valLength[11]);
		currPtr->frame_mode[qDesc->valLength[11]] = '\0';
		ims_truncStr (currPtr->frame_mode);

		(void) memcpy ((char *)&(currPtr->time_on),
			qDesc->valAddr[12], qDesc->valLength[12]);
		currPtr->time_on[qDesc->valLength[12]] = '\0';
		ims_truncStr (currPtr->time_on);

		(void) memcpy ((char *)&(currPtr->time_off),
			qDesc->valAddr[13], qDesc->valLength[13]);
		currPtr->time_off[qDesc->valLength[13]] = '\0';
		ims_truncStr (currPtr->time_off);

		(void) memcpy ((char *)&(currPtr->site_name),
			qDesc->valAddr[14], qDesc->valLength[14]);
		currPtr->site_name[qDesc->valLength[14]] = '\0';
		ims_truncStr (currPtr->site_name);

		(void) memcpy ((char *)&(currPtr->updated_time),
			qDesc->valAddr[15], qDesc->valLength[15]);
		currPtr->updated_time[qDesc->valLength[15]] = '\0';
		ims_truncStr (currPtr->updated_time);


		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__getDownlinkDTKList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK);

} /* getDownlinkDTKList */


/***********************************************************************
**
** getAuxiliaryLock - execute stored procedure get_auxiliary_lock
**
***********************************************************************/
static int
getAuxiliaryLock (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ	*qDesc;
	IMS_MSG_STRUCT	*msgDesc;

	qDesc		= catReq->qDesc;
	msgDesc	= catReq->msgDesc;

	/*
	**Execute stored procedure get_auxiliary_lock
	*/

	(void) sprintf (qDesc->cmd, "get_auxiliary_lock");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_auxiliary_lock failed.");
		return(IMS_FATAL);
	}

	return(IMS_OK);
} /* getAuxiliaryLock */


/***********************************************************************
**
** updateDTKProcessAuthFlag - update a DTK's process auth flag
**
***********************************************************************/
static int
updateDTKProcessAuthFlag (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ		*qDesc;
	IMS_MSG_STRUCT		*msgDesc;
	DBCHAR						platform[IMS_COL2_LEN+1] ;
	DBCHAR						dt_sensor[IMS_COL1_LEN+1] ;
	DBINT							dt_revolution ;
	DBSMALLINT				dt_sequence ;
	DBCHAR						process_auth_flag[IMS_COL3_LEN+1] ;

	qDesc					= catReq->qDesc;
	msgDesc				= catReq->msgDesc;
	dt_revolution	= *(DBINT *)catReq->item[2];
	dt_sequence		= *(DBSMALLINT *)catReq->item[3];
	(void) strcpy( platform, catReq->item[0] );
	(void) strcpy( dt_sensor, catReq->item[1] );
	(void) strcpy( process_auth_flag, catReq->item[4] );

	/*
	** update the process auth flag status
	*/

	(void) sprintf( qDesc->cmd,
			"update datatake_entry set PROCESS_AUTH_FLAG = '%s'\n"
			" where PLATFORM = '%s' and DT_SENSOR = '%s'"
			" and DT_REVOLUTION = %d and DT_SEQUENCE = %d",
			process_auth_flag, platform, dt_sensor, dt_revolution, dt_sequence ) ;

	/* Execute the sql command */
	if (execCmd (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateDTKProcessAuthFlag: update process_auth_flag failed.");
		return(IMS_FATAL);
	}

	/* return error if no row was affected */
	if (IMS_AFFECTED (qDesc) != 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat_updateDTKProcessAuthFlag: No row was affected"
			" in datatake_entry table.");
		return(IMS_FATAL);
	}

	return (IMS_OK);
} /* updateDTKProcessAuthFlag */


/***********************************************************************
**
** issueDownlinkScanRequest - issue a scan request for a downlink
**
***********************************************************************/
static int
issueDownlinkScanRequest (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ		*qDesc;
	IMS_MSG_STRUCT		*msgDesc;
	/* values from the downlink */
	DBCHAR						platformAcronym[IMS_COL2_LEN+1] ;
	DBCHAR						sensor[IMS_COL1_LEN+1] ;
	DBINT							revolution ;
	DBSMALLINT				sequence ;
	/* values for ODL keywords */
	/* Note: the next four are 'hardcoded', so they are set here */
	DBCHAR						*first_name		= "IMS" ;
	DBCHAR						*last_name			= "Client" ;
	DBCHAR						*authenticator	= "ims_clnt" ;
	DBCHAR						*billing_id		= "ACCT_IMS" ;
	DBCHAR						platformName[IMS_COL30_LEN+1] ;
	DBCHAR						dataset[IMS_COL80_LEN+1] ;
	DBCHAR						granule_name[IMS_COL30_LEN+1] ;
	DBCHAR						quicklook[IMS_COL1_LEN+1] ;
	/* scan request variables */
	AGGREGATE					RxTree ;	/* ODL data structure for call to ims_order() */
	IMS_ODL_TREE			*scan_request ;	/* scan request ODL */
	IMS_ODL_TREE			*user_info ;		/* user info ODL */
	IMS_ODL_TREE			*scan_item ;		/* scan item ODL */
	int								orderId ;				/* id of the successful scan request, or 0*/
	/* misc. variables */
	DBCHAR						granules_table[IMS_COL30_LEN+1] ;

	qDesc					= catReq->qDesc;
	msgDesc				= catReq->msgDesc;
	revolution		= *(DBINT *)catReq->item[2];
	sequence			= *(DBSMALLINT *)catReq->item[3];
	(void) strcpy( platformAcronym, catReq->item[0] );
	(void) strcpy( sensor, catReq->item[1] );
	(void) strcpy( quicklook, catReq->item[4] );

	/*
	** Get the values for the ODL keywords
	*/

	/* get the platformName from the acronym */

	(void) sprintf( qDesc->cmd,
			"select platform from platforms where acronym = '%s'",
			platformAcronym ) ;

	/* Execute the sql command & make sure a row is returned */
	if (execCmd (qDesc) < IMS_OK || IMS_AFFECTED( qDesc ) <= 0)
	{
		(void) ims_msg( msgDesc, IMS_FATAL,
				"op_cat_issueDownlinkScanRequest:"
				" get platform name from acronym failed.\n"
				"    For downlink, platform: '%s', sensor: '%s', rev: %d, sequence %d",
				platformAcronym, sensor, revolution, (int) sequence );
		return(IMS_FATAL);
	}

	/* copy in the platformName */
	(void) memcpy( platformName, qDesc->valAddr[0], qDesc->valLength[0] );
	platformName[qDesc->valLength[0]] = '\0';
	ims_truncStr( platformName ) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_issueDownlinkScanRequest:"
			" Could not reinitialize query descriptor.");

		return(IMS_FATAL);
	}


	/* get the dataset */

	(void) sprintf( dataset, "%s RAW SIGNAL SEGMENT", platformName ) ;


	/* get the granule name */

	(void) sprintf( qDesc->cmd,
			"select dp.granules_table\n"
			"from dataset_relation dr, dataset_policy dp\n"
			"where dr.dataset_idx = dp.dataset_idx\n"
			" and dr.dataset = '%s'",
			dataset ) ;

	/* Execute the sql command & make sure one row is returned */
	if (execCmd (qDesc) < IMS_OK || IMS_AFFECTED( qDesc ) <= 0)
	{
		(void) ims_msg( msgDesc, IMS_FATAL,
				"op_cat_issueDownlinkScanRequest: get granules table name failed."
				"    For downlink, platform: '%s', sensor: '%s', rev: %d, sequence %d",
				platformAcronym, sensor, revolution, (int) sequence );
		return(IMS_FATAL);
	}

	/* copy in the granules table name */
	(void) memcpy( granules_table, qDesc->valAddr[0], qDesc->valLength[0] );
	granules_table[qDesc->valLength[0]] = '\0';
	ims_truncStr( granules_table ) ;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"op_cat_issueDownlinkScanRequest:"
			" Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}

	(void) sprintf( qDesc->cmd,
			"select ds.name from %s ds, granules_406 ta\n"
			"where ds.PLATFORM = '%s' and ds.SENSOR = '%s'"
			" and ds.REVOLUTION = %d and ds.SEQUENCE = %d\n"
			" and ds.MEDIA_ID = ta.MEDIA_ID"
			" and ta.MEDIA_ID_TYPE_NAME in ('WORKING_SIGNAL', 'QUICKLOOK', 'TEST')\n"
			" and ta.STATUS = 'HST_S_AVAILABLE'"
			" and (ds.RECORDER_TYPE = 'ID-1'"
				" or (ds.RECORDER_TYPE = 'DCRSI' and ds.DATA_DIRECTION = 'FORWARD'))"
			" and ds.STATUS = 'HST_S_OK'",
			granules_table, platformAcronym, sensor, revolution, (int) sequence ) ;

	/* Execute the sql command & make sure a row is returned */
	if (execCmd (qDesc) < IMS_OK || IMS_AFFECTED( qDesc ) <= 0)
	{
		(void) ims_msg( msgDesc, IMS_FATAL,
				"op_cat_issueDownlinkScanRequest: get granule name failed."
				"    For downlink, platform: '%s', sensor: '%s', rev: %d, sequence %d",
				platformAcronym, sensor, revolution, (int) sequence );
		return(IMS_FATAL);
	}

	/* copy in the granule name */
	(void) memcpy( granule_name, qDesc->valAddr[0], qDesc->valLength[0] );
	granule_name[qDesc->valLength[0]] = '\0';
	ims_truncStr( granule_name ) ;


	/*
	** Build the scan_request ODL
	*/

	(void) ims_addODLObject( msgDesc, NULL, &scan_request,
			"SCAN_REQUEST", FALSE, IMS_OBJECT ) ;


	/*
	** Build the user_info ODL
	*/

	(void) ims_addODLObject( msgDesc, scan_request,  &user_info,
			"USER_INFO", TRUE, IMS_OBJECT ) ;

	(void) ims_addODLKeyword( msgDesc, user_info, "FIRST_NAME",
			TV_STRING, first_name ) ;

	(void) ims_addODLKeyword( msgDesc, user_info, "LAST_NAME",
			TV_STRING, last_name ) ;

	(void) ims_addODLKeyword( msgDesc, user_info, "AUTHENTICATOR",
			TV_STRING, authenticator ) ;

	(void) ims_addODLKeyword( msgDesc, user_info, "BILLING_ID",
			TV_STRING, billing_id ) ;


	/*
	** Build the scan_item ODL.
	*/

	(void) ims_addODLObject( msgDesc, scan_request,  &scan_item,
			"SCAN_ITEM", TRUE, IMS_OBJECT ) ;

	(void) ims_addODLKeyword( msgDesc, scan_item, "PLATFORM",
			TV_STRING, platformName ) ;

	(void) ims_addODLKeyword( msgDesc, scan_item, "DATASET",
			TV_STRING, dataset ) ;

	(void) ims_addODLKeyword( msgDesc, scan_item, "FILENAME",
			TV_STRING, granule_name ) ;

	(void) ims_addODLKeyword( msgDesc, scan_item, "QUICK_LOOK",
			TV_STRING, quicklook ) ;


	/*
	** call ims_order() to issue the scan request
	*/

	if (ims_buildAggregate( msgDesc, scan_request, &RxTree ) < IMS_OK)
	{
		(void) ims_msg( msgDesc, IMS_FATAL,
				"op_cat_issueDownlinkScanRequest: build ims_order() aggregate failed."
				"    For downlink, platform: '%s', sensor: '%s', rev: %d, sequence %d",
				platformAcronym, sensor, revolution, (int) sequence );
		return(IMS_FATAL);
	}

	if (ims_order( msgDesc, RxTree, &orderId ) < IMS_OK)
	{
		(void) ims_msg( msgDesc, IMS_FATAL,
				"op_cat_issueDownlinkScanRequest: ims_order() failed."
				"    For downlink, platform: '%s', sensor: '%s', rev: %d, sequence %d",
				platformAcronym, sensor, revolution, (int) sequence );
		return(IMS_FATAL);
	}

	return (IMS_OK);
} /* issueDownlinkScanRequest */


/***********************************************************************
**
** setKeywordValuesFromCmd -	gets the values from the keyword_value
**														table, using the cmd in the input qDesc;
**														then places the values in an array of
**														structs and returns the array.
**
**														Note: does not reset query descriptor,
**														i.e., doesn't call ims_qiResetDesc().
**
** OUTPUT PARAMETERS
**			rowCount	- number of rows
**			status		- >= IMS_OK; if an error, < IMS_OK
**
** RETURNS
**			array of value structures, one per value; if an error, returns NULL
**
***********************************************************************/
static OP_KEYWORD_VAL_DATA *
setValuesFromKeyword (
		IMS_QI_DESC_OBJ			*qDesc,
		IMS_MSG_STRUCT			*msgDesc,
		int									*rowCount,
		int									*status)
{
	OP_KEYWORD_VAL_DATA *valuesArray ;
	OP_KEYWORD_VAL_DATA *tmpValuesArray ;
	char								tmpstr[BUFSIZ] ;
	int									numChars ;

	/* 
	** Get keyword's acronym valids
	*/

	valuesArray	= NULL ;
	*rowCount		= 0;
	while ((*status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (*status < IMS_OK) 
		{
			return (NULL);
		}
		if (*status == IMS_ENDOFQUERY) continue;

		/* manipulate to get the final string */
		(void) memcpy (tmpstr, qDesc->valAddr[0], qDesc->valLength[0]);
		tmpstr[qDesc->valLength[0]] = '\0';
		ims_truncStr( tmpstr );
		ims_toUpper( tmpstr ) ;

		/* allocate the storage space */
		if ((valuesArray =
				(OP_KEYWORD_VAL_DATA *) realloc( (tmpValuesArray = valuesArray),
				(*rowCount+1) * sizeof(OP_KEYWORD_VAL_DATA) )) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"op_cat_getKeywordValueInfo: Could not allocate enough memory.");
			*status = IMS_FATAL ;
			free( tmpValuesArray ) ;
			return(NULL);
		}

		/* copy to values array, truncating, if necessary (Shouldn't be necessary)*/
		numChars = sizeof(valuesArray[*rowCount].value) - 1;
		(void) strncpy( valuesArray[*rowCount].value, tmpstr, numChars ) ;
		valuesArray[*rowCount].value[numChars] = '\0';

		/* a row is returned */
		*rowCount += 1;
	}

	return (valuesArray) ;
}	/* setValuesFromKeyword */
