static char *sccs = "@(#)ims_opBillViewCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opBillViewCb.c

	Function:	Callback functions for Bill View Screen

	Author:		Jennifer Ting

	Date:			11/1995

	Revision: 6/10/1996 - Modified function billView_closeCb to correct PR 942.

*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <X11/Shell.h>
#include <UxXt.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>

#define _IMS_OP_BILLVIEWCB_C
#include "ims_opCb.h"

/* 
** Local Functions 
*/

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static int sel_billing_id = 0;
extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opBillView.h>
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: billView_browseSelectCb
**
** Description:		callback function for browse selection in BillId list
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void billView_browseSelectCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	_UxCbillView            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	XmListCallbackStruct *cbs = (XmListCallbackStruct *)cb;

	char *choice;
	char sel_bill_date[IMS_COL30_LEN+1];

	UxSaveCtx = UxBillViewContext;
	UxBillViewContext = UxContext =
			(_UxCbillView *) UxGetContext( UxWidget );
	{
		XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
		sscanf (choice, "%d %s", &sel_billing_id, sel_bill_date);

		XtFree (choice);

	}
 	UxBillViewContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: billView_viewCb
**
** Description:		callback function for the view button in bill view screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void billView_viewCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	_UxCbillView            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_BILL_ITEM_LIST *billItemList, *billItemPtr, *bPtr;
	OP_BILLING_DATA *billingData;
	DBINT order_id;
	int count, status;
	char Msg[IMS_COL1024_LEN+1];
	char label[IMS_COL128_LEN+1];
	char query[IMS_COL255_LEN+1];

	UxSaveCtx = UxBillViewContext;
	UxBillViewContext = UxContext =
			(_UxCbillView *) UxGetContext( UxWidget );
 {

	/* clean up all wigets */
	XmTextSetString (billToST, NULL);
	XmTextFieldSetString (invoiceIdTF, NULL);
	XmTextFieldSetString (accountIdTF, NULL);
	XmTextFieldSetString (billDateTF, NULL);
	XmTextFieldSetString (amountTF, NULL);
	XmTextFieldSetString (orderIdTF, NULL);
	XmTextFieldSetString (userNameTF, NULL);
	XmTextFieldSetString (orderDateTF, NULL);
	XmTextFieldSetString (balanceTF, NULL);
	XmTextFieldSetString (resourceTF, NULL);
	XmListDeleteAllItems (billItemSL);
	XtSetSensitive (printPB, False);
		
	/* If sel_billing_id is 0, then no billing_id is selected. */
	if (!sel_billing_id)
	{
		strcpy (Msg, "No billing id is selected.\n"); 
		msgBoxDlg_popupCb (glbData.billViewW, IMS_INFO, Msg); 
		return;
	}

	/* Timeout cursor */
	timeOutCursors (True);

	/* assign client to orderClientData from glbData structure */
	XtVaGetValues (glbData.billViewW, XmNuserData, &order_id, NULL);
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	billItemList = (OP_BILL_ITEM_LIST *)NULL;
	billingData = (OP_BILLING_DATA *)NULL;
	count = 0;

	if ((billingData = (OP_BILLING_DATA *) malloc
			((unsigned) sizeof (OP_BILLING_DATA))) ==
				(OP_BILLING_DATA *) NULL)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "View Billing Reports:\n"
					"Internal Error: memory allocation failed.\n" 
					"Please contact the DBA and exit the program.\n");   
		msgBoxDlg_popupCb (glbData.billViewW, IMS_FATAL, Msg); 

		return;
	}

	/* initialize billingData */
	memset (billingData, 0, sizeof(OP_BILLING_DATA));

	/*
	** call cat event OP_GETBILLINGDATA to get billing data
	*/
	sprintf (query,
	"select t1.billing_id, convert (char(12), t1.create_time, 100),"
	"	t1.op_comment,"
	"	t2.profile_id, t3.first_name, t3.initial_name, t3.last_name,"
	" t3.title, t3.organization, t3.street, t3.city, t3.state,"
	" t3.country, t3.zipcode, t3.phone, t3.fax, t3.email," 
	" t4.account_id, convert(char(12), t4.received_time, 100),"
	" t5.resource_type, t5.curr_balance,"
	" t6.first_name, t6.initial_name, t6.last_name"
	" from billing t1, billing_of t2, billing_profile t3, order_queue t4,"
	" account t5, user_profile t6 "
	" where t1.billing_id = %d"
	" and t2.billing_id = %d"
	" and t2.order_id = %d"
	" and t3.order_id = %d"
	" and t3.profile_id = t2.profile_id"
	" and t4.order_id = %d"
	" and t5.account_id = t4.account_id"
  "	and t6.user_id = t4.user_id",
	sel_billing_id, sel_billing_id, order_id, order_id, order_id);

	catReq->item[0] = (OP_BILLING_DATA *)billingData;
	catReq->item[1] = (char *)query;
	if ((status = ims_opCat(catReq, OP_GETBILLINGDATA)) < 0)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/*
		** Internal error, free up billingData allocated,
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: View Billing Reports failed.\n"
								 "OP_GETBILLINGDATA failed for billing Id: %d\n", 
								 sel_billing_id); 
		msgBoxDlg_popupCb (glbData.billViewW, IMS_ERROR, Msg);

		if (billingData != (OP_BILLING_DATA *)NULL)
		{
			free (billingData);
		}

		return;
	}


	/* 
	** call cat event OP_GETBILLITEMS to get the billItemList 
	*/
	sprintf (query,
	"select t1.item_id, t1.p_granule_name, t1.cost, t1.process_type, "
	" t1.quantity, t1.media_type, t1.status "
	" from order_item t1 " 
	" where t1.billing_id = %d and t1.order_id = %d"  
	" order by item_id",
	sel_billing_id, order_id);

	catReq->item[0] = (char *)query;
	catReq->item[1] = (int *)&count;
	catReq->item[2] = (OP_BILL_ITEM_LIST *)billItemList;
	if ((status = ims_opCat (catReq, OP_GETBILLITEMS)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Billing Reports:\n"
					  "Internal Error: OP_GETBILLITEMS failed for billing_id: %d\n",
					  sel_billing_id);
		msgBoxDlg_popupCb (glbData.billViewW, IMS_FATAL, Msg); 

		free (billingData);

		return;
	}

	/* assign count and billItemList returned from query */
	count = *(int *)catReq->item[1];
	billItemList = (OP_BILL_ITEM_LIST *)catReq->item[2];

	billingData->order_id = order_id;
	billingData->billItemList = billItemList;

	if ((count == 0) || (billItemList == (OP_BILL_ITEM_LIST *)NULL))
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Billing Reports:\n"
					  "Internal Error: Could not locate items for billing_id %d.\n",
					  sel_billing_id);
		msgBoxDlg_popupCb (glbData.billViewW, IMS_INFO, Msg); 

		free (billingData);

		return;
	}

	bPtr = billItemList;
	while (bPtr != (OP_BILL_ITEM_LIST *)NULL)
	{
		bPtr->status = 1;

		if (bPtr->item_status == ITEM_CANCELLED)
		{
			bPtr->cost = 0;
			bPtr->quantity = 0;
		}

		billingData->invoice_amount += bPtr->cost;
		billingData->total_qty += bPtr->quantity;
		bPtr = bPtr->next;
	}

	/* display billing data  & billing item list */
	if ((status = display_billingData(billingData, 0)) < IMS_OK)
	{
		sprintf (Msg, "Could not display Billing Folder for billing_id %d.\n",
									sel_billing_id); 

		msgBoxDlg_popupCb (glbData.billViewW, IMS_ERROR, Msg); 
	}
	else
	{
		XtSetSensitive (printPB, True);
	}

	/* free billingData and billItemList */
	free (billingData);

	if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
	{
		free_billItemList (&billItemList);
	}

	/* normal cursor */
	timeOutCursors (False);
 }

 	UxBillViewContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: billView_closeCb
**
** Description:		Callback function for the close button in bill view
**								Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - took out BillView context assignment
**
**==========================================================================*/

void billView_closeCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	XtPopdown (XtParent(glbData.billViewW));
	glbData.billViewFlag = 0;
}


/*===========================================================================*
** 
** Function Name: display_billViewScreen
**
** Description:		Function for displaying the billView Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

int display_billViewScreen(
		DBINT order_id,
		OP_ORDER_BILLID_LIST *billIdList)
{
	_UxCbillView            *UxSaveCtx, *UxContext;

	XmString listStr;
	int position;
	XmString label;
	char buffer[IMS_COL255_LEN+1];
	OP_ORDER_BILLID_LIST *bPtr;

	UxSaveCtx = UxBillViewContext;
	UxBillViewContext = UxContext =
			(_UxCbillView *) UxGetContext( glbData.billViewW );
	{

	/* initialize sel_billing_id to zero */
	sel_billing_id = 0;

	/* 
	** pop up the billing folder screen with billing id list
	*/
	sprintf (buffer, "Order     %d      Billing       Folder", order_id);
	label = XmStringCreateLocalized (buffer);
	XtVaSetValues (billViewLB, XmNlabelString, label, NULL);

	position = 0;
	XmListDeleteAllItems (billIdSL);
	bPtr = billIdList;
	while (bPtr != (OP_ORDER_BILLID_LIST *)NULL)
	{	
		position++;
		sprintf (buffer, "  %-10d            %-12s",
						 bPtr->billing_id, bPtr->create_time); 

		listStr = XmStringCreateLocalized (buffer);
		XmListAddItemUnselected (billIdSL, listStr, position);
		XmStringFree (listStr);

		bPtr = bPtr->next;
	}

	/* clean up all wigets */
	XmTextSetString (billToST, NULL);
	XmTextFieldSetString (invoiceIdTF, NULL);
	XmTextFieldSetString (accountIdTF, NULL);
	XmTextFieldSetString (billDateTF, NULL);
	XmTextFieldSetString (amountTF, NULL);
	XmTextFieldSetString (orderIdTF, NULL);
	XmTextFieldSetString (balanceTF, NULL);
	XmTextFieldSetString (orderDateTF, NULL);
	XmTextFieldSetString (userNameTF, NULL);
	XmTextFieldSetString (resourceTF, NULL);
	XmListDeleteAllItems (billItemSL);
	XtSetSensitive (printPB, False);
		
	glbData.billViewFlag = 1;
	XtPopup (XtParent(glbData.billViewW), XtGrabNone);
	XtVaSetValues (glbData.billViewW, XmNuserData, order_id, NULL);

	}
 	UxBillViewContext = UxSaveCtx;

	return (IMS_OK);
}

