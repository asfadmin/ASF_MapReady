static char *sccs = "@(#)ims_opShipViewCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opShipViewCb.c

	Function:	Callback functions for Ship View Screen

	Author:		Jennifer Ting

	Date:			11/1995

	Revision: 6/10/1996 - Modified function shipView_closeCb to correct PR 942.

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

#define _IMS_OP_SHIPVIEWCB_C
#include "ims_opCb.h"

/* 
** Local Functions 
*/

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static int sel_shipping_id = 0;
extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opShipView.h>
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: shipView_browseSelectCb
**
** Description:		callback function for browse selection in ShipId list
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

void shipView_browseSelectCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	_UxCshipView            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	XmListCallbackStruct *cbs = (XmListCallbackStruct *)cb;

	char *choice;
	char sel_ship_date[IMS_COL30_LEN+1];

	UxSaveCtx = UxShipViewContext;
	UxShipViewContext = UxContext =
			(_UxCshipView *) UxGetContext( UxWidget );
	{
		XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
		sscanf (choice, "%d %s", &sel_shipping_id, sel_ship_date);

		XtFree (choice);

	}
 	UxShipViewContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: shipView_viewCb
**
** Description:		callback function for the view button in ship view screen
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

void shipView_viewCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	_UxCshipView            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_SHIP_ITEM_LIST *shipItemList, *shipItemPtr, *sPtr;
	OP_SHIPPING_DATA *shippingData;
	DBINT order_id;
	int count, status;
	char Msg[IMS_COL1024_LEN+1];
	char label[IMS_COL128_LEN+1];
	char query[IMS_COL255_LEN+1];

	UxSaveCtx = UxShipViewContext;
	UxShipViewContext = UxContext =
			(_UxCshipView *) UxGetContext( UxWidget );
 {

	/* clean up all wigets */
	XmTextSetString (shipToST, NULL);
	XmTextFieldSetString (shippingIdTF, NULL);
	XmTextFieldSetString (accountIdTF, NULL);
	XmTextFieldSetString (shipDateTF, NULL);
	XmTextFieldSetString (totalQtyTF, NULL);
	XmTextFieldSetString (orderIdTF, NULL);
	XmTextFieldSetString (carrierTF, NULL);
	XmTextFieldSetString (orderDateTF, NULL);
	XmTextFieldSetString (totalCostTF, NULL);
	XmListDeleteAllItems (shipItemSL);
	XtSetSensitive (printPB, False);
		
	/* If sel_shipping_id is 0, then no shipping_id is selected. */
	if (!sel_shipping_id)
	{
		strcpy (Msg, "No shipping id is selected.\n"); 
		msgBoxDlg_popupCb (glbData.shipViewW, IMS_INFO, Msg); 
		return;
	}

	/* Timeout cursor */
	timeOutCursors (True);

	/* assign client to orderClientData from glbData structure */
	XtVaGetValues (glbData.shipViewW, XmNuserData, &order_id, NULL);
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	shipItemList = (OP_SHIP_ITEM_LIST *)NULL;
	shippingData = (OP_SHIPPING_DATA *)NULL;
	count = 0;

	if ((shippingData = (OP_SHIPPING_DATA *) malloc
			((unsigned) sizeof (OP_SHIPPING_DATA))) ==
				(OP_SHIPPING_DATA *) NULL)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "View Shipping Reports:\n"
					"Internal Error: memory allocation failed.\n" 
					"Please contact the DBA and exit the program.\n");   
		msgBoxDlg_popupCb (glbData.shipViewW, IMS_FATAL, Msg); 

		return;
	}

	/* initialize shippingData */
	memset (shippingData, 0, sizeof(OP_SHIPPING_DATA));

	/*
	** call cat event OP_GETSHIPPINGDATA to get shipping data
	*/
	sprintf (query,
	"select t1.shipping_id, convert(char(12), t1.create_time, 100), "
	" t1.carrier, t1.op_comment,"
	" t2.profile_id, t3.first_name, t3.initial_name, t3.last_name,"
	" t3.title, t3.organization, t3.street, t3.city, t3.state,"
	" t3.country, t3.zipcode, t3.phone, t3.fax, t3.email,"
	" t4.account_id, convert (char(12), t4.received_time, 100)"
	" from shipping t1, shipping_of t2, shipping_profile t3, order_queue t4 " 
	" where t1.shipping_id = %d and t2.shipping_id = %d and " 
	" t2.order_id = %d and t3.order_id = %d and "
	" t3.profile_id = t2.profile_id and t4.order_id = %d",
	sel_shipping_id, sel_shipping_id, order_id, order_id, order_id);

	catReq->item[0] = (OP_SHIPPING_DATA *)shippingData;
	catReq->item[1] = (char *)query;
	if ((status = ims_opCat(catReq, OP_GETSHIPPINGDATA)) < 0)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/*
		** Internal error, free up shippingData allocated,
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: View Shipping Reports failed.\n"
								 "OP_GETSHIPPINGDATA failed for shipping Id: %d\n", 
								 sel_shipping_id); 
		msgBoxDlg_popupCb (glbData.shipViewW, IMS_ERROR, Msg);

		if (shippingData != (OP_SHIPPING_DATA *)NULL)
		{
			free (shippingData);
		}

		return;
	}


	/* 
	** call cat event OP_GETSHIPITEMS to get the shipItemList 
	*/
	sprintf (query,
	"select t1.item_id, t1.p_granule_name, t1.cost, t1.process_type, "
	" t1.quantity, t1.media_type, t1.status "
	" from order_item t1 " 
	" where t1.shipping_id = %d and t1.order_id = %d"  
	" order by item_id",
	sel_shipping_id, order_id);

	catReq->item[0] = (char *)query;
	catReq->item[1] = (int *)&count;
	catReq->item[2] = (OP_SHIP_ITEM_LIST *)shipItemList;
	if ((status = ims_opCat (catReq, OP_GETSHIPITEMS)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Shipping Reports:\n"
					  "Internal Error: OP_GETSHIPITEMS failed for shipping_id: %d\n",
					  sel_shipping_id);
		msgBoxDlg_popupCb (glbData.shipViewW, IMS_FATAL, Msg); 

		free (shippingData);

		return;
	}

	/* assign count and shipItemList returned from query */
	count = *(int *)catReq->item[1];
	shipItemList = (OP_SHIP_ITEM_LIST *)catReq->item[2];

	shippingData->order_id = order_id;
	shippingData->shipItemList = shipItemList;

	if ((count == 0) || (shipItemList == (OP_SHIP_ITEM_LIST *)NULL))
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Shipping Reports:\n"
					  "Internal Error: Could not locate items for shipping_id %d.\n",
					  sel_shipping_id);
		msgBoxDlg_popupCb (glbData.shipViewW, IMS_INFO, Msg); 

		free (shippingData);

		return;
	}

	sPtr = shipItemList;
	while (sPtr != (OP_SHIP_ITEM_LIST *)NULL)
	{
		sPtr->status = 1;

		if (sPtr->item_status == ITEM_CANCELLED)
		{
			sPtr->cost = 0;
			sPtr->quantity = 0;
		}

		shippingData->total_cost += sPtr->cost;
		shippingData->total_qty += sPtr->quantity;
		sPtr = sPtr->next;
	}

	/* display shipping data  & shipping item list */
	if ((status = display_shippingData(shippingData, 0)) < IMS_OK)
	{
		sprintf (Msg, "Could not display Shipping Folder for shipping_id %d.\n",
									sel_shipping_id); 

		msgBoxDlg_popupCb (glbData.shipViewW, IMS_ERROR, Msg); 
	}
	else
	{
		XtSetSensitive (printPB, True);
	}

	/* free shippingData and shipItemList */
	free (shippingData);

	if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
	{
		free_shipItemList (&shipItemList);
	}

	/* normal cursor */
	timeOutCursors (False);
 }

 	UxShipViewContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: shipView_closeCb
**
** Description:		Callback function for the close button in ship view
**								Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - took out ShipView context assignment
**
**==========================================================================*/

void shipView_closeCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	XtPopdown (XtParent(glbData.shipViewW));
	glbData.shipViewFlag = 0;
}


/*===========================================================================*
** 
** Function Name: display_shipViewScreen
**
** Description:		Function for displaying the shipView Screen.
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

int display_shipViewScreen(
		DBINT order_id,
		OP_ORDER_SHIPID_LIST *shipIdList)
{
	_UxCshipView            *UxSaveCtx, *UxContext;

	XmString listStr;
	int position;
	XmString label;
	char buffer[IMS_COL255_LEN+1];
	OP_ORDER_SHIPID_LIST *sPtr;

	UxSaveCtx = UxShipViewContext;
	UxShipViewContext = UxContext =
			(_UxCshipView *) UxGetContext( glbData.shipViewW );
	{

	/* Initialize sel_shipping_id to zero */
	sel_shipping_id = 0;
	/* 
	** pop up the shipping folder screen with shipping id list
	*/
	sprintf (buffer, "Order     %d      Shipping       Folder", order_id);
	label = XmStringCreateLocalized (buffer);
	XtVaSetValues (shipViewLB, XmNlabelString, label, NULL);

	position = 0;
	XmListDeleteAllItems (shipIdSL);
	sPtr = shipIdList;
	while (sPtr != (OP_ORDER_SHIPID_LIST *)NULL)
	{	
		position++;
		sprintf (buffer, "  %-10d            %-12s",
						 sPtr->shipping_id, sPtr->create_time); 

		listStr = XmStringCreateLocalized (buffer);
		XmListAddItemUnselected (shipIdSL, listStr, position);
		XmStringFree (listStr);

		sPtr = sPtr->next;
	}

	/* clean up all wigets */
	XmTextSetString (shipToST, NULL);
	XmTextFieldSetString (shippingIdTF, NULL);
	XmTextFieldSetString (accountIdTF, NULL);
	XmTextFieldSetString (shipDateTF, NULL);
	XmTextFieldSetString (totalQtyTF, NULL);
	XmTextFieldSetString (orderIdTF, NULL);
	XmTextFieldSetString (carrierTF, NULL);
	XmTextFieldSetString (orderDateTF, NULL);
	XmTextFieldSetString (totalCostTF, NULL);
	XmListDeleteAllItems (shipItemSL);
	XtSetSensitive (printPB, False);
		
	glbData.shipViewFlag = 1;
	XtPopup (XtParent(glbData.shipViewW), XtGrabNone);
	XtVaSetValues (glbData.shipViewW, XmNuserData, order_id, NULL);

	}
 	UxShipViewContext = UxSaveCtx;

	return (IMS_OK);
}

