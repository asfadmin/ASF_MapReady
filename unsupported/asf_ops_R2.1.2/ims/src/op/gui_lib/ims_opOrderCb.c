static char *sccs = "@(#)ims_opOrderCb.c	6.2  03/19/98";
/*******************************************************************************

	File:			ims_opOrderCb.c

	Function:	Callback functions for order display screen

	Author:		Jennifer Ting

	Date:			3/1995

	Modified: 6/4/96 - Function order_browse_itemDetailsCb, add v0_process_type
										 This is the change for PR 900

						6/5/96 - Created functions order_save_resultsCb and 
										 order_save_results_okCb for saving query results to
										 a file.  This is done as per Michelle Barr's request.

						6/11/96 - Modified function order_save_resultsCb to correct
											PR 942.

            6/11/96 - Added function order_save_results_cancelCb to 
											correct PR 942.

            7/9/96  - Modified function order_processMediaCb to correct
											PR 980.

            7/12/96 - Added function order_restart_itemCb. 

						7/19/96 - Modified function start_mediaJob to increase timeout
											to 21600 (6 hrs) seconds for ims_addJob, this is to 
											correct PR 1002.

            9/09/96 - Modified function order_displayResultsCb for PR 85.

						9/17/96 - Stored procedure rollback_item_status has been dropped
											for the new schema patch C.  Modified the following
											two functions to use update_item_status function to
											reset the tape items back to status IN-MEDIA-Q:
											rollback_tapeItemsStatus, rollback_batchItemsStatus

						11/11/96 - Modified order_restart_itemCb for PR 2184.

						3/4/97  - Removed erroneous free'ing of the scrolled list
											compound strings (via XmStringFree()) if couldn't
											allocate memory to an XmStringTable.
										- adjusted while-loop to free scrolled list compound strings
											so also releases the first string (array element 0)

						3/10/97	- Modified order_restart_itemCb to restart failed as well
											as error items.

            10/27/97- PR#2490 MAX_TAPE_ITEMS to 1000

						1/27/98 - Re-fixed PR#2490

						3/19/98 - Modified function start_mediaJob to increase timeout
											to 43200 (12 hrs) seconds for ims_addJob.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
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
#include <Xm/FileSB.h>

#define _IMS_OP_ORDERCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

/*
** #define IMS_DEBUG
*/

extern OP_GLOBAL_DATA glbData;
extern Widget		UxTopLevel;
extern int glbDefaultSearchFlag;
extern int glbWelcomeOrderFlag;

#define OP_ROWS 8 
#define NEW			1 
#define PENDING 2 
#define STARTED 3 
#define COMPLETED 4 
#define PROCESS_ORDER  1 
#define PROCESS_ITEMS  2
#define SHIP_ORDER  1 
#define SHIP_ITEMS  2
#define BILL_ORDER  1 
#define BILL_ITEMS  2

static int orderWindowTop = 0;
static int itemWindowTop = 0;
void do_search_dir(Widget, XtPointer);

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include <ims_opOrder.h>
#undef CONTEXT_MACRO_ACCESS
#include <ims_opSearch.h>

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: order_scroll_orderListsCb
**
** Description:		Updates the Order list widgets (11 of them) with new orders
**								when user scrolls. Scrolling method is application-defined. 
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

void	order_scroll_orderListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	int orderCount, count, memory;
	int i, temp, j, k, highLightPos[OP_ROWS];
	Widget sbar;
	XmStringTable orderIdStr, userIdStr, receivedStr, priorityStr,
								qlkStr, statusStr, totalStr, generatedStr, onlineStr,
								holdStr, errorStr;
	char buffer[IMS_COL255_LEN+1];
	/*char Msg[IMS_COL1024_LEN+1];*/
	char Msg[10240+1];
	int max;


	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;
	j = 0;

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
	/*		(_UxCorder *) UxGetContext(widget); */
			(_UxCorder *) UxGetContext(glbData.orderW);
	{

		orderWindowTop = cbs->value;
		clientData->orderWindowTop = orderWindowTop;
		count = orderCount - orderWindowTop;

		if (count > OP_ROWS)
			count = OP_ROWS;

		/* If no rows to display, clean up order lists and text widgets */
		if ((orderCount == 0) || (orderPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (orderIdList);
			XmListDeleteAllItems (userIdList);
			XmListDeleteAllItems (receivedList);
			XmListDeleteAllItems (orderPriorityList);
			XmListDeleteAllItems (orderQlkList);
			XmListDeleteAllItems (orderStatusList);
			XmListDeleteAllItems (totalList);
			XmListDeleteAllItems (generatedList);
			XmListDeleteAllItems (onlineList);
			XmListDeleteAllItems (holdList);
			XmListDeleteAllItems (errorList);
			XmListDeleteAllItems (orderDummyList);

			XmTextFieldSetString (totalOrdersTF, "");
			XtSetSensitive (viewItemsPB, False);
			XtSetSensitive (viewOrderDetailsMPB, False);
			XtSetSensitive (validateOrderMPB, False);
			/*
			XtSetSensitive (unvalidateOrderMPB, False);
			*/
			XtSetSensitive (updateOrderPriorityMPB, False);
			XtSetSensitive (updateOrderStatusMPB, False);
			XtSetSensitive (editOrderCommentMPB, False);
			XtSetSensitive (processMediaMPB, False);
			XtSetSensitive (shipOrderMPB, False);
			XtSetSensitive (billOrderMPB, False);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		orderIdStr   = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		userIdStr    = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		receivedStr  = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		priorityStr  = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		qlkStr  	   = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		statusStr    = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		totalStr 	   = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		generatedStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		onlineStr		 = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		holdStr			 = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		errorStr		 = (XmStringTable)XtMalloc(count *sizeof(XmString *));

#if 0
/* 3/10/97: tlm: *** commenting out the following code ***
		REASON: XtMalloc, if fails, prints an error message and EXITs;
		the following code does not work to catch an "allocation failed"
		condition
*/
		memory = orderIdStr && userIdStr && receivedStr && 
						 priorityStr && qlkStr && statusStr && totalStr &&
						 generatedStr && onlineStr && holdStr && errorStr;

		if (!memory)
		{
			/* Display error messages in message window */
			sprintf(Msg, "Internal Error: memory allocation failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			XtFree ((char *)orderIdStr);
			XtFree ((char *)userIdStr);
			XtFree ((char *)receivedStr);
			XtFree ((char *)priorityStr);
			XtFree ((char *)qlkStr);
			XtFree ((char *)statusStr);
			XtFree ((char *)totalStr);
			XtFree ((char *)generatedStr);
			XtFree ((char *)onlineStr);
			XtFree ((char *)holdStr);
			XtFree ((char *)errorStr);
			return;
		}
#endif
	
		while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
		{
			orderPtr = orderPtr->next;
		}
		
		for (i = 0; i < count && orderPtr != (OP_ORDER_LIST *)NULL; i++)
		{
			if (orderPtr->order_id)
			{
				sprintf (buffer, "%d", orderPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else 
				orderIdStr[i] = XmStringCreateLocalized("");

			if (orderPtr->user_id)
				userIdStr[i] = XmStringCreateLocalized(orderPtr->user_id);
			else
				userIdStr[i] = XmStringCreateLocalized("");

			if (orderPtr->received_time)
				receivedStr[i] = XmStringCreateLocalized(orderPtr->received_time);
			else
				receivedStr[i] = XmStringCreateLocalized("");

			if (orderPtr->priority)
			{
				k = 0;
				while ((k < glbData.priority_count) && 
							 (orderPtr->priority != glbData.priority[k].item_id)) 
							k++;

				if (k < glbData.priority_count)
				{
					priorityStr[i] = XmStringCreateLocalized
													 (glbData.priority[k].item_name);
				}
				else
					/* did not find the matching priority id */
					priorityStr[i] = XmStringCreateLocalized("");
					
			}
			else
				priorityStr[i] = XmStringCreateLocalized("");

			if (orderPtr->quicklook)
			{
				sprintf (buffer, "%c", orderPtr->quicklook);
				qlkStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				qlkStr[i] = XmStringCreateLocalized("");

			if (orderPtr->status)
			{
				k = 0;
				while ((k < glbData.order_status_count) && 
							 (orderPtr->status != glbData.order_status[k].item_id)) 
							k++;

				if (k < glbData.order_status_count)
				{
					statusStr[i] = XmStringCreateLocalized
												(glbData.order_status[k].item_name);
				}
				else
					/* did not find the matching order_status id */
					statusStr[i] = XmStringCreateLocalized("");

			}
			else
				statusStr[i] = XmStringCreateLocalized("");

			if (orderPtr->item_count >= 0)
			{
				sprintf (buffer, "%d", orderPtr->item_count);
				totalStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				totalStr[i] = XmStringCreateLocalized("");

			if (orderPtr->item_generated >= 0)
			{
				sprintf (buffer, "%d", orderPtr->item_generated);
				generatedStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				generatedStr[i] = XmStringCreateLocalized("");

			if (orderPtr->item_online >= 0)
			{
				sprintf (buffer, "%d", orderPtr->item_online);
				onlineStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				onlineStr[i] = XmStringCreateLocalized("");

			if (orderPtr->item_hold >= 0)
			{
				sprintf (buffer, "%d", orderPtr->item_hold);
				holdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				holdStr[i] = XmStringCreateLocalized("");

			if (orderPtr->item_error >= 0)
			{
				sprintf (buffer, "%d", orderPtr->item_error);
				errorStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				errorStr[i] = XmStringCreateLocalized("");


			if (orderPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			orderPtr = orderPtr->next;

		}

		/* Load all the synchronized arrays to order list widgets */

		XtVaSetValues(
				orderIdList, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				userIdList, XmNitems, userIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				receivedList, XmNitems, receivedStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				orderPriorityList, XmNitems, priorityStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				orderQlkList, XmNitems, qlkStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				orderStatusList, XmNitems, statusStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				totalList, XmNitems, totalStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				generatedList, XmNitems, generatedStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				onlineList, XmNitems, onlineStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				holdList, XmNitems, holdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				errorList, XmNitems, errorStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				orderDummyList, XmNitems, orderIdStr, XmNitemCount, count, NULL);


		/* Free compound strings */
		while (--i >= 0)
		{
			XmStringFree (orderIdStr[i]);
			XmStringFree (userIdStr[i]);
			XmStringFree (receivedStr[i]);
			XmStringFree (priorityStr[i]);
			XmStringFree (qlkStr[i]);
			XmStringFree (statusStr[i]);
			XmStringFree (totalStr[i]);
			XmStringFree (generatedStr[i]);
			XmStringFree (onlineStr[i]);
			XmStringFree (holdStr[i]);
			XmStringFree (errorStr[i]);
		}

		XtFree ((char *)orderIdStr);
		XtFree ((char *)userIdStr);
		XtFree ((char *)receivedStr);
		XtFree ((char *)priorityStr);
		XtFree ((char *)qlkStr);
		XtFree ((char *)statusStr);
		XtFree ((char *)totalStr);
		XtFree ((char *)generatedStr);
		XtFree ((char *)onlineStr);
		XtFree ((char *)holdStr);
		XtFree ((char *)errorStr);
	

		/* Update Scrollbar position */
		XtVaGetValues(orderDummySW, XmNverticalScrollBar, &sbar, NULL);

		temp = orderCount;
		if (orderCount < OP_ROWS)
			temp = orderCount;

		XtVaSetValues(sbar, 
			XmNmaximum, temp,
			XmNvalue, orderWindowTop,
			NULL);

		/*
		** sensitize all order menu items
		*/

		/*
		** 1/6/96 - sensitize MPB depends on userType
		*/

		switch (glbData.userSpec.userType)
		{
			case OP_GENERAL:
				XtSetSensitive (viewOrderDetailsMPB, True);
				XtSetSensitive (viewItemsPB, True);
			break;

			case OP_PRODUCTION:
				XtSetSensitive (viewItemsPB, True);
				XtSetSensitive (viewOrderDetailsMPB, True);
				XtSetSensitive (updateOrderStatusMPB, True);
				XtSetSensitive (editOrderCommentMPB, True);
				XtSetSensitive (processMediaMPB, True);
			break;

			case OP_GDC:
				XtSetSensitive (viewItemsPB, True);
				XtSetSensitive (viewOrderDetailsMPB, True);
				XtSetSensitive (validateOrderMPB, True);
				/*
				XtSetSensitive (unvalidateOrderMPB, True);
				*/
				XtSetSensitive (updateOrderPriorityMPB, True);
				XtSetSensitive (updateOrderStatusMPB, True);
				XtSetSensitive (editOrderCommentMPB, True);
				XtSetSensitive (shipOrderMPB, True);
				XtSetSensitive (billOrderMPB, True);
			break;

			case OP_SYSTEM:
				XtSetSensitive (viewItemsPB, True);
				XtSetSensitive (viewOrderDetailsMPB, True);
				XtSetSensitive (validateOrderMPB, True);
				/*
				XtSetSensitive (unvalidateOrderMPB, True);
				*/
				XtSetSensitive (updateOrderPriorityMPB, True);
				XtSetSensitive (updateOrderStatusMPB, True);
				XtSetSensitive (editOrderCommentMPB, True);
				XtSetSensitive (shipOrderMPB, True);
				XtSetSensitive (billOrderMPB, True);
				XtSetSensitive (processMediaMPB, True);
			break;

			default:
				XtSetSensitive (viewItemsPB, True);
				XtSetSensitive (viewOrderDetailsMPB, True);
			break;
		}
				

		/*
		** For R1B, it has been decided to set the selection policy
		** of all the ORDER LISTS to be BROWSE SELECTION, this may
		** change in later releases.
		*/

		/* Set selectionPolicy of order list widgets to MultipleSelect */
		/*
		XmListDeselectAllItems(orderIdList);
		XtVaSetValues(orderIdList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(userIdList);
		XtVaSetValues(userIdList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(receivedList);
		XtVaSetValues(receivedList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(orderPriorityList);
		XtVaSetValues(orderPriorityList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(orderQlkList);
		XtVaSetValues(orderQlkList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(orderStatusList);
		XtVaSetValues(orderStatusList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		XmListDeselectAllItems(totalList);
		XtVaSetValues(totalList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		XmListDeselectAllItems(generatedList);
		XtVaSetValues(generatedList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		XmListDeselectAllItems(onlineList);
		XtVaSetValues(onlineList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		XmListDeselectAllItems(holdList);
		XtVaSetValues(holdList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		XmListDeselectAllItems(errorList);
		XtVaSetValues(errorList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
		*/

		XmListDeselectAllItems(orderIdList);

		XmListDeselectAllItems(userIdList);

		XmListDeselectAllItems(receivedList);

		XmListDeselectAllItems(orderPriorityList);

		XmListDeselectAllItems(orderQlkList);

		XmListDeselectAllItems(orderStatusList);
	
		XmListDeselectAllItems(totalList);
	
		XmListDeselectAllItems(generatedList);
	
		XmListDeselectAllItems(onlineList);
	
		XmListDeselectAllItems(holdList);
	
		XmListDeselectAllItems(errorList);

		while (j--)
		{
			XmListSelectPos(orderIdList, highLightPos[j], False);
			XmListSelectPos(userIdList, highLightPos[j], False);
			XmListSelectPos(receivedList, highLightPos[j], False);
			XmListSelectPos(orderPriorityList, highLightPos[j], False);
			XmListSelectPos(orderQlkList, highLightPos[j], False);
			XmListSelectPos(orderStatusList, highLightPos[j], False);
			XmListSelectPos(totalList, highLightPos[j], False);
			XmListSelectPos(generatedList, highLightPos[j], False);
			XmListSelectPos(onlineList, highLightPos[j], False);
			XmListSelectPos(holdList, highLightPos[j], False);
			XmListSelectPos(errorList, highLightPos[j], False);
		}


		/*
		** For R1B, it has been decided to set the selection policy
		** of all the ORDER LISTS to be SINGLE SELECTION, this may
		** change in later releases.
		*/

		/* Change list widgets selectionPolicy back to extendedSelect */
		/*
		XtVaSetValues(orderIdList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(userIdList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(receivedList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(orderPriorityList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(orderQlkList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(orderStatusList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(totalList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(generatedList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(onlineList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(holdList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(errorList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		*/

	}
	UxOrderContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: order_orderLists_selectionCb
**
** Description:		Updates the selection of the order(s) in all the order list
**								widgets, synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each order list widget is associated with
** 									 							a unique number, for example, list widget
**																orderId is listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	 None
** 
** Revision History:
**
**==========================================================================*/

void order_orderLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr, *tempPtr;
	int listNo;
	int i, k, count, orderCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( widget );

	{
		clientData = &(glbData.orderClientData);
		orderPtr = clientData->orderList;
		orderCount = clientData->orderCount;
		listNo = (int)listNumber;
		itemPosition = cbs->item_position;

		/* Not used for single selction policy */
		/*
		selectedItemPos = cbs->selected_item_positions;
		totalItemsSelected = cbs->selected_item_count;
		*/
		

		/*
		** For R1B, it has been decided to set the selection policy
		** of all the ORDER LISTS to be SINGLE SELECTION, this may
		** change in later releases.
		*/

		/* Set selectionPolicy of order list widgets to MultipleSelect */
		/*
		if(listNo != 1)
		{
			XmListDeselectAllItems(orderIdList);
			XtVaSetValues(orderIdList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(userIdList);
			XtVaSetValues(userIdList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(receivedList);
			XtVaSetValues(receivedList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(orderPriorityList);
			XtVaSetValues(orderPriorityList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 5)
		{
			XmListDeselectAllItems(orderQlkList);
			XtVaSetValues(orderQlkList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 6)
		{
			XmListDeselectAllItems(orderStatusList);
			XtVaSetValues(orderStatusList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 7)
		{
			XmListDeselectAllItems(totalList);
			XtVaSetValues(totalList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 8)
		{
			XmListDeselectAllItems(generatedList);
			XtVaSetValues(generatedList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 9)
		{
			XmListDeselectAllItems(onlineList);
			XtVaSetValues(onlineList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 10)
		{
			XmListDeselectAllItems(holdList);
			XtVaSetValues(holdList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 11)
		{
			XmListDeselectAllItems(errorList);
			XtVaSetValues(errorList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		*/

		if(listNo != 1)
		{
			XmListDeselectAllItems(orderIdList);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(userIdList);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(receivedList);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(orderPriorityList);
		}
		if(listNo != 5)
		{
			XmListDeselectAllItems(orderQlkList);
		}
		if(listNo != 6)
		{
			XmListDeselectAllItems(orderStatusList);
		}
		if(listNo != 7)
		{
			XmListDeselectAllItems(totalList);
		}
		if(listNo != 8)
		{
			XmListDeselectAllItems(generatedList);
		}
		if(listNo != 9)
		{
			XmListDeselectAllItems(onlineList);
		}
		if(listNo != 10)
		{
			XmListDeselectAllItems(holdList);
		}
		if(listNo != 11)
		{
			XmListDeselectAllItems(errorList);
		}


		/*
		** Reset the selectFlag for each item in the orderList
		*/
		tempPtr = orderPtr;
		while (tempPtr != (OP_ORDER_LIST *) NULL) 
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		/* Locate screen items in orderList */
		count = orderCount - orderWindowTop;

		if (count > OP_ROWS)
			count = OP_ROWS;

		while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
		{
			orderPtr = orderPtr->next;
		}

  	/* Locate selected order in orderList to set the selectFlag */
		tempPtr = orderPtr;

		for (k = 0;  (tempPtr != NULL) && (k < itemPosition-1); k++)
			tempPtr = tempPtr->next;

		tempPtr->selectFlag = 1;


		/* Select the item across all order list wigets */
		if (listNo != 1)
			XmListSelectPos(orderIdList, itemPosition, False);
		if (listNo != 2)
			XmListSelectPos(userIdList, itemPosition, False);
		if (listNo != 3)
			XmListSelectPos(receivedList, itemPosition, False);
		if (listNo != 4)
			XmListSelectPos(orderPriorityList, itemPosition, False);
		if (listNo != 5)
			XmListSelectPos(orderQlkList, itemPosition, False);
		if (listNo != 6)
			XmListSelectPos(orderStatusList, itemPosition, False);
		if (listNo != 7)
			XmListSelectPos(totalList, itemPosition, False);
		if (listNo != 8)
			XmListSelectPos(generatedList, itemPosition, False);
		if (listNo != 9)
			XmListSelectPos(onlineList, itemPosition, False);
		if (listNo != 10)
			XmListSelectPos(holdList, itemPosition, False);
		if (listNo != 11)
			XmListSelectPos(errorList, itemPosition, False);
			


		/*
		** For R1B, it has been decided to set the selection policy
		** of all the ORDER LISTS to be SINGLE SELECTION, this may
		** change in later releases.
		*/

		/* Change list widgets selectionPolicy back to extendedSelect */
		/*
		if(listNo != 1)
		{
			XtVaSetValues(orderIdList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(userIdList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(receivedList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XtVaSetValues(orderPriorityList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 5)
		{
			XtVaSetValues(orderQlkList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 6)
		{
			XtVaSetValues(orderStatusList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 7)
		{
			XtVaSetValues(totalList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 8)
		{
			XtVaSetValues(generatedList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 9)
		{
			XtVaSetValues(onlineList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 10)
		{
			XtVaSetValues(holdList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 11)
		{
			XtVaSetValues(errorList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		*/

	}
	UxOrderContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: order_scroll_itemListsCb
**
** Description:		Updates the item list widgets (9 of them) with new items
**								when user scrolls. Scrolling method is application-defined. 
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_scroll_itemListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr;
	int itemCount, count, memory;
	int i, temp, j, k, highLightPos[OP_ROWS];
	Widget sbar;
	XmStringTable itemIdStr, frameIdStr, satStr, procTypeStr, 
								priorityStr, qlkStr, statusStr, mediaStr, 
								typeStr, costStr, validateStr, shipStr, billStr;
	char buffer[IMS_COL255_LEN+1];


	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;
	j = 0;

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
	/*		(_UxCorder *) UxGetContext(widget); */
			(_UxCorder *) UxGetContext(glbData.orderW);
	{

		itemWindowTop = cbs->value;
		clientData->itemWindowTop = itemWindowTop;
		count = itemCount - itemWindowTop;

		if (count > OP_ROWS)
			count = OP_ROWS;


		/* If no rows to display, clean up item list and text widgets */
		if ((itemCount == 0) || (itemPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (itemNoList);
			XmListDeleteAllItems (frameIdList);
			XmListDeleteAllItems (satList);
			XmListDeleteAllItems (procTypeList);
			XmListDeleteAllItems (itemPriorityList);
			XmListDeleteAllItems (itemQlkList);
			XmListDeleteAllItems (itemStatusList);
			XmListDeleteAllItems (mediaList);
			XmListDeleteAllItems (typeList);
			XmListDeleteAllItems (costList);
			XmListDeleteAllItems (validateList);
			XmListDeleteAllItems (shipList);
			XmListDeleteAllItems (billList);
			XmListDeleteAllItems (itemDummyList);
			XmTextFieldSetString (orderIdTF, "");
			XmTextFieldSetString (totalItemsTF, "");

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		itemIdStr 	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		frameIdStr 	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		satStr 			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		procTypeStr	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		priorityStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		qlkStr 			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		statusStr		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		mediaStr		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		typeStr			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		costStr			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		validateStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		shipStr			= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		billStr			= (XmStringTable)XtMalloc(count *sizeof(XmString *));

#if 0
/* 3/10/97: tlm: *** commenting out the following code ***
		REASON: XtMalloc, if fails, prints an error message and EXITs;
		the following code does not work to catch an "allocation failed"
		condition
*/
		memory = itemIdStr && frameIdStr && satStr && procTypeStr &&
						 priorityStr && qlkStr && statusStr && mediaStr && typeStr &&
						 costStr && validateStr && shipStr && billStr;

		if (!memory)
		{
			XtFree ((char *)itemIdStr);
			XtFree ((char *)frameIdStr);
			XtFree ((char *)satStr);
			XtFree ((char *)procTypeStr);
			XtFree ((char *)priorityStr);
			XtFree ((char *)qlkStr);
			XtFree ((char *)statusStr);
			XtFree ((char *)mediaStr);
			XtFree ((char *)typeStr);
			XtFree ((char *)costStr);
			XtFree ((char *)validateStr);
			XtFree ((char *)shipStr);
			XtFree ((char *)billStr);
			return;
		}
#endif

	
		while ((itemPtr != NULL) && (itemPtr->position != itemWindowTop))
		{
			itemPtr = itemPtr->next;
		}
		
		for (i = 0; i < count && itemPtr != (OP_ORDER_ITEM_LIST *)NULL; i++)
		{
			if (itemPtr->item_id)
			{
				sprintf (buffer, "%d", itemPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else 
				itemIdStr[i] = XmStringCreateLocalized("");

			if (*itemPtr->granule_name)
				frameIdStr[i] = XmStringCreateLocalized(itemPtr->granule_name);
			else
			{
				strcpy (buffer, "N/A");
				frameIdStr[i] = XmStringCreateLocalized(buffer);
			}

			if (*itemPtr->platacronym)
				satStr[i] = XmStringCreateLocalized(itemPtr->platacronym);
			else
			{
				strcpy (buffer, "N/A");
				satStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->process_type > 0)
			{
				k = 0;
				while ((k < glbData.process_type_count) && 
							 (itemPtr->process_type != glbData.process_type[k].item_id)) 
							k++;

				if (k < glbData.process_type_count)
				{
					procTypeStr[i] = XmStringCreateLocalized
													 (glbData.process_type[k].item_name);
				}
				else
				{
					/* did not find the matching process_type id */
					/* changed from blank string to NA - oct 95 */
					strcpy (buffer, "N/A");
					procTypeStr[i] = XmStringCreateLocalized(buffer);
				}
			}
			else
			{
				strcpy (buffer, "N/A");
				procTypeStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->priority > 0)
			{
				k = 0;
				while ((k < glbData.priority_count) && 
							 (itemPtr->priority != glbData.priority[k].item_id)) 
							k++;

				if (k < glbData.priority_count)
				{
					priorityStr[i] = XmStringCreateLocalized
													 (glbData.priority[k].item_name);
				}
				else
				{
					/* did not find the matching priority id */
					strcpy (buffer, "N/A");
					priorityStr[i] = XmStringCreateLocalized(buffer);
				}

			}
			else
			{
				/* did not find the matching priority id */
				strcpy (buffer, "N/A");
				priorityStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->quicklook_p)
			{
				sprintf (buffer, "%c", itemPtr->quicklook_p);
				qlkStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				qlkStr[i] = XmStringCreateLocalized("");

			if (itemPtr->status)
			{
				k = 0;
				while ((k < glbData.item_status_count) && 
							 (itemPtr->status != glbData.item_status[k].item_id)) 
							k++;

				if (k < glbData.item_status_count)
				{
					statusStr[i] = XmStringCreateLocalized
												(glbData.item_status[k].item_name);
				}
				else
				{
					/* did not find the matching status id */
					strcpy (buffer, "N/A");
					statusStr[i] = XmStringCreateLocalized(buffer);
				}
			}
			else
			{
				/* did not find the matching status id */
				strcpy (buffer, "N/A");
				statusStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->media_type)
			{
				k = 0;
				while ((k < glbData.media_type_count) && 
							 (itemPtr->media_type != glbData.media_type[k].item_id)) 
							k++;

				if (k < glbData.media_type_count)
				{
					mediaStr[i] = XmStringCreateLocalized
													 (glbData.media_type[k].item_name);
				}
				else
				{
					/* did not find the matching media_type id */
					strcpy (buffer, "N/A");
					mediaStr[i] = XmStringCreateLocalized(buffer);
				}
			}
			else
			{
				/* did not find the matching media_type id */
				strcpy (buffer, "N/A");
				mediaStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->order_item_type)
			{
				k = 0;
				while ((k < glbData.order_item_type_count) && 
					 (itemPtr->order_item_type != glbData.order_item_type[k].item_id)) 
				k++;

				if (k < glbData.order_item_type_count)
				{
					typeStr[i] = XmStringCreateLocalized
													(glbData.order_item_type[k].item_name);
				}
				else
				{
					/* did not find the matching order_item_type id */
					strcpy (buffer, "N/A");
					typeStr[i] = XmStringCreateLocalized(buffer);
				}
			}
			else
			{
				/* did not find the matching order_item_type id */
				strcpy (buffer, "N/A");
				typeStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->cost)
			{
				sprintf (buffer, "%-.2f", itemPtr->cost);
				costStr[i] = XmStringCreateLocalized(buffer);
			}
			else
			{
				strcpy (buffer, "N/A");
				costStr[i] = XmStringCreateLocalized(buffer);
			}

			if (itemPtr->validated_p)
			{
				sprintf (buffer, "%c", itemPtr->validated_p);
				validateStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				validateStr[i] = XmStringCreateLocalized("");

			if (itemPtr->shipped_p)
			{
				sprintf (buffer, "%c", itemPtr->shipped_p);
				shipStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				shipStr[i] = XmStringCreateLocalized("");

			if (itemPtr->billed_p)
			{
				sprintf (buffer, "%c", itemPtr->billed_p);
				billStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				billStr[i] = XmStringCreateLocalized("");

			if (itemPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			itemPtr = itemPtr->next;

		}

		/* Load all the synchronized arrays to item list widgets */

		XtVaSetValues(
				itemNoList, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				frameIdList, XmNitems, frameIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				satList, XmNitems, satStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				procTypeList, XmNitems, procTypeStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemPriorityList, XmNitems, priorityStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemQlkList, XmNitems, qlkStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemStatusList, XmNitems, statusStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				mediaList, XmNitems, mediaStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				typeList, XmNitems, typeStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				costList, XmNitems, costStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				validateList, XmNitems, validateStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				shipList, XmNitems, shipStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				billList, XmNitems, billStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemDummyList, XmNitems, billStr, XmNitemCount, count, NULL);


		/* Free compound strings */
		while (--i >= 0)
		{
			XmStringFree (itemIdStr[i]);
			XmStringFree (frameIdStr[i]);
			XmStringFree (satStr[i]);
			XmStringFree (procTypeStr[i]);
			XmStringFree (priorityStr[i]);
			XmStringFree (qlkStr[i]);
			XmStringFree (statusStr[i]);
			XmStringFree (mediaStr[i]);
			XmStringFree (typeStr[i]);
			XmStringFree (costStr[i]);
			XmStringFree (validateStr[i]);
			XmStringFree (shipStr[i]);
			XmStringFree (billStr[i]);
		}

		XtFree ((char *)itemIdStr);
		XtFree ((char *)frameIdStr);
		XtFree ((char *)satStr);
		XtFree ((char *)procTypeStr);
		XtFree ((char *)priorityStr);
		XtFree ((char *)qlkStr);
		XtFree ((char *)statusStr);
		XtFree ((char *)mediaStr);
		XtFree ((char *)typeStr);
		XtFree ((char *)costStr);
		XtFree ((char *)validateStr);
		XtFree ((char *)shipStr);
		XtFree ((char *)billStr);
	
		/* Update Scrollbar position */
		XtVaGetValues(itemDummySW, XmNverticalScrollBar, &sbar, NULL);

		temp = itemCount;
		if (itemCount < OP_ROWS)
			temp = itemCount;

		XtVaSetValues(sbar, 
			XmNmaximum, temp,
			XmNvalue, itemWindowTop,
			NULL);

		/* Set selectionPolicy of order list widgets to MultipleSelect */
		XmListDeselectAllItems(itemNoList);
		XtVaSetValues(itemNoList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(frameIdList);
		XtVaSetValues(frameIdList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(satList);
		XtVaSetValues(satList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(procTypeList);
		XtVaSetValues(procTypeList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(itemPriorityList);
		XtVaSetValues(itemPriorityList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(itemQlkList);
		XtVaSetValues(itemQlkList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(itemStatusList);
		XtVaSetValues(itemStatusList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		XmListDeselectAllItems(mediaList);
		XtVaSetValues(mediaList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(typeList);
		XtVaSetValues(typeList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(costList);
		XtVaSetValues(costList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(validateList);
		XtVaSetValues(validateList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(shipList);
		XtVaSetValues(shipList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(billList);
		XtVaSetValues(billList, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

	
	
		while (j--)
		{
			XmListSelectPos(itemNoList, highLightPos[j], False);
			XmListSelectPos(frameIdList, highLightPos[j], False);
			XmListSelectPos(satList, highLightPos[j], False);
			XmListSelectPos(procTypeList, highLightPos[j], False);
			XmListSelectPos(itemPriorityList, highLightPos[j], False);
			XmListSelectPos(itemQlkList, highLightPos[j], False);
			XmListSelectPos(itemStatusList, highLightPos[j], False);
			XmListSelectPos(mediaList, highLightPos[j], False);
			XmListSelectPos(typeList, highLightPos[j], False);
			XmListSelectPos(costList, highLightPos[j], False);
			XmListSelectPos(validateList, highLightPos[j], False);
			XmListSelectPos(shipList, highLightPos[j], False);
			XmListSelectPos(billList, highLightPos[j], False);
		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		XtVaSetValues(itemNoList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(frameIdList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(satList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(procTypeList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(itemPriorityList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(itemQlkList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(itemStatusList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(mediaList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(typeList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(costList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(validateList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(shipList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(billList, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);

	}
	UxOrderContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: order_itemLists_selectionCb
**
** Description:		Updates the selection of the item(s) in all the item list
**								widgets (9 of them), synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each item list widget is associated with
** 									 							a unique number, for example, list widget
**																itemNo has listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void order_itemLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr, *tempPtr;
	int listNo;
	int i, k, count, itemCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );

	{
		clientData = &(glbData.orderClientData);
		itemPtr = clientData->currOrder->itemList;
		itemCount = clientData->currOrder->item_count;
		listNo = (int)listNumber;
		selectedItemPos = cbs->selected_item_positions;
		itemPosition = cbs->item_position;
		totalItemsSelected = cbs->selected_item_count;
		

		/* Set selectionPolicy of item list widgets to MultipleSelect */
		if(listNo != 1)
		{
			XmListDeselectAllItems(itemNoList);
			XtVaSetValues(itemNoList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(frameIdList);
			XtVaSetValues(frameIdList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(satList);
			XtVaSetValues(satList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(procTypeList);
			XtVaSetValues(procTypeList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 5)
		{
			XmListDeselectAllItems(itemPriorityList);
			XtVaSetValues(itemPriorityList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 6)
		{
			XmListDeselectAllItems(itemQlkList);
			XtVaSetValues(itemQlkList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 7)
		{
			XmListDeselectAllItems(itemStatusList);
			XtVaSetValues(itemStatusList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 8)
		{
			XmListDeselectAllItems(mediaList);
			XtVaSetValues(mediaList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 9)
		{
			XmListDeselectAllItems(typeList);
			XtVaSetValues(typeList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 10)
		{
			XmListDeselectAllItems(costList);
			XtVaSetValues(costList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 11)
		{
			XmListDeselectAllItems(validateList);
			XtVaSetValues(validateList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 12)
		{
			XmListDeselectAllItems(shipList);
			XtVaSetValues(shipList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 13)
		{
			XmListDeselectAllItems(billList);
			XtVaSetValues(billList, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}


		/* Locate screen items in item lists */
		count = itemCount - itemWindowTop;

		if (count > OP_ROWS)
			count = OP_ROWS;

		while ((itemPtr != NULL) && (itemPtr->position != itemWindowTop))
		{
			itemPtr = itemPtr->next;
		}

		/* Reset the selectFlag for each item first */
		tempPtr = itemPtr;
		for (i = 0; i < count && tempPtr != NULL; i++)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		for (i = 0; i < totalItemsSelected; i++)
		{
			if (listNo != 1)
				XmListSelectPos(itemNoList, selectedItemPos[i], False);
			if (listNo != 2)
				XmListSelectPos(frameIdList, selectedItemPos[i], False);
			if (listNo != 3)
				XmListSelectPos(satList, selectedItemPos[i], False);
			if (listNo != 4)
				XmListSelectPos(procTypeList, selectedItemPos[i], False);
			if (listNo != 5)
				XmListSelectPos(itemPriorityList, selectedItemPos[i], False);
			if (listNo != 6)
				XmListSelectPos(itemQlkList, selectedItemPos[i], False);
			if (listNo != 7)
				XmListSelectPos(itemStatusList, selectedItemPos[i], False);
			if (listNo != 8)
				XmListSelectPos(mediaList, selectedItemPos[i], False);
			if (listNo != 9)
				XmListSelectPos(typeList, selectedItemPos[i], False);
			if (listNo != 10)
				XmListSelectPos(costList, selectedItemPos[i], False);
			if (listNo != 11)
				XmListSelectPos(validateList, selectedItemPos[i], False);
			if (listNo != 12)
				XmListSelectPos(shipList, selectedItemPos[i], False);
			if (listNo != 13)
				XmListSelectPos(billList, selectedItemPos[i], False);
			

			/* Locate selected items in item lists to set the selectFlag */
			tempPtr = itemPtr;
			for (k = 0;  (k < selectedItemPos[i]-1) && (tempPtr != NULL); k++)
				tempPtr = tempPtr->next;

			tempPtr->selectFlag = 1;

		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		if(listNo != 1)
		{
			XtVaSetValues(itemNoList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(frameIdList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(satList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XtVaSetValues(procTypeList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 5)
		{
			XtVaSetValues(itemPriorityList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 6)
		{
			XtVaSetValues(itemQlkList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 7)
		{
			XtVaSetValues(itemStatusList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 8)
		{
			XtVaSetValues(mediaList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 9)
		{
			XtVaSetValues(typeList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 10)
		{
			XtVaSetValues(costList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 11)
		{
			XtVaSetValues(validateList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 12)
		{
			XtVaSetValues(shipList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 13)
		{
			XtVaSetValues(billList, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}


	}
	UxOrderContext = UxSaveCtx;
}

/*===========================================================================*
** 
** Function Name: order_show_orderItemsCb
**
** Description:		Display the items of the order selected.  
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_show_orderItemsCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *tempPtr;
	OP_USER_ACCOUNT_DATA *orderUserData;
	char buffer[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	char *userDataBuf;
	char *text;
	int k, status;
	int i, count, orderCount, itemCount;
	XmScrollBarCallbackStruct *cbs;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW);
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;

	/* Locate screen items in orderList */
	count = orderCount - orderWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
	{
		orderPtr = orderPtr->next;
	}

	/* Find the first selected order in the current screen */
	i = 0;
	while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < count) && 
			(orderPtr->selectFlag == 0))
	{
		i++;
		orderPtr = orderPtr->next;
	}

	if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= count))
	{
		/* No order is selected, display message, return */
		sprintf(Msg, "No order is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	
	clientData->currOrder = orderPtr;
	if (clientData->currOrder->itemList == (OP_ORDER_ITEM_LIST *)NULL)
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/*
		** Get itemList from database for current order.		
		** Initialize catalog request structure 
		*/
		catReq = &(clientData->catReq);
		catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
		catReq->item[1] = (int *)&itemCount;


		/* 
		** Get user profile and account data for the order
		*/
		if ((status = ims_opCat (catReq, OP_GETUSERDATA)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Internal Error: getUserData failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}


		/*
		** Get order item list for the order
		*/
		if ((status = ims_opCat (catReq, OP_GETORDERITEMLIST)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Internal Error: item retrieval failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}

		/* assign returned items */
		orderPtr->item_count = *(int *)catReq->item[1];
		orderPtr->itemList = (OP_ORDER_ITEM_LIST *)catReq->item[2];
		clientData->currOrder->item_count = *(int *)catReq->item[1];
		clientData->currOrder->itemList = (OP_ORDER_ITEM_LIST *)catReq->item[2];


		/*
		** get PPS granule size(data_kbytes) from granules table  
		** and step_avgtime from order_item_step table
		*/
		tempPtr = orderPtr->itemList;
		while (tempPtr != (OP_ORDER_ITEM_LIST *) NULL)
		{
			catReq->item[0] = (OP_ORDER_ITEM_LIST *)tempPtr;
			if ((status = ims_opCat (catReq, OP_GETPPSGRANULESIZESTEPTIME)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				sprintf(Msg, "Internal Error: Function getPPSGranuleSize failed.");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
				return;
			}

			tempPtr = tempPtr->next;
		}

	}


	/*
	** no items to display, desensitize all item menu items
	*/
	if ((orderPtr->itemList == (OP_ORDER_ITEM_LIST *)NULL) ||
			(orderPtr->item_count <= 0))
	{
		/* No items to display, desensitize menu item ViewItemDetails */
		XtSetSensitive (viewItemDetailsMPB, False);
		XtSetSensitive (validateItemMPB, False);
		/*
		XtSetSensitive (unvalidateItemMPB, False);
		*/
		XtSetSensitive (updateItemPriorityMPB, False);
		XtSetSensitive (updateItemStatusMPB, False);
		XtSetSensitive (editItemCommentMPB, False);
		XtSetSensitive (itemProcessMediaMPB, False);
		XtSetSensitive (itemShipItemsMPB, False);
		XtSetSensitive (itemBillItemsMPB, False);
		XtSetSensitive (restartItemMPB, False);
	}
	else
	{
		/* initialize item selectFlag */
		tempPtr = orderPtr->itemList;
		while (tempPtr != (OP_ORDER_ITEM_LIST *) NULL)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		/* Sensitize menu item ViewOrderDetails 
		**
		** 1/5/96 - sensitize MPB depends on UserType
		*/
		switch (glbData.userSpec.userType)
		{
			case OP_GENERAL:
				XtSetSensitive (viewItemDetailsMPB, True);
			break;

			case OP_PRODUCTION:
				XtSetSensitive (viewItemDetailsMPB, True);
				XtSetSensitive (updateItemStatusMPB, True);
				XtSetSensitive (editItemCommentMPB, True);
				XtSetSensitive (itemProcessMediaMPB, True);
				XtSetSensitive (restartItemMPB, True);
			break;

			case OP_GDC:
				XtSetSensitive (viewItemDetailsMPB, True);
				XtSetSensitive (validateItemMPB, True);
				/*
				XtSetSensitive (unvalidateItemMPB, True);
				*/
				XtSetSensitive (updateItemPriorityMPB, True);
				XtSetSensitive (updateItemStatusMPB, True);
				XtSetSensitive (editItemCommentMPB, True);
				XtSetSensitive (itemShipItemsMPB, True);
				XtSetSensitive (itemBillItemsMPB, True);
				XtSetSensitive (restartItemMPB, True);
		 	break;

			case OP_SYSTEM:
				XtSetSensitive (viewItemDetailsMPB, True);
				XtSetSensitive (validateItemMPB, True);
				/*
				XtSetSensitive (unvalidateItemMPB, True);
				*/
				XtSetSensitive (updateItemPriorityMPB, True);
				XtSetSensitive (updateItemStatusMPB, True);
				XtSetSensitive (editItemCommentMPB, True);
				XtSetSensitive (itemShipItemsMPB, True);
				XtSetSensitive (itemBillItemsMPB, True);
				XtSetSensitive (itemProcessMediaMPB, True);
				XtSetSensitive (restartItemMPB, True);
			break;

			default:
				XtSetSensitive (viewItemDetailsMPB, True);
			break;
		}

	}

	/* call order_scroll_itemListsCb to load data in item list widgets */
	cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
	cbs->value = 0;
	order_scroll_itemListsCb(widget, NULL, cbs);
	free (cbs);

	/*
	** Display order id in orderIdTF, no of items in totalItemsTF 
	*/
	sprintf (buffer, "%d", orderPtr->order_id );
	XmTextFieldSetString (orderIdTF, buffer);
	sprintf (buffer, "%d", orderPtr->item_count);
	XmTextFieldSetString (totalItemsTF, buffer);


	/*
	** Display User Profile and Account Information 
	*/
	if ((userDataBuf = XtMalloc ((unsigned int) 1024)) == (char *)NULL)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* display error message, return */
		sprintf(Msg, "Could not allocate space for userDat buffer.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/* Write user data and account information */
	orderUserData = &(orderPtr->userData);
	text = userDataBuf;

	if (orderUserData->first_name[0] != '\0') 
	{
		sprintf (text, "User Name:\t\t%s  ", orderUserData->first_name);
		text = text + strlen(text);
	}

	if (orderUserData->initial_name[0] != '\0')
	{
		sprintf (text, "%s  ", orderUserData->initial_name);
		text = text + strlen(text);
	}

	if (orderUserData->last_name[0] != '\0')
	{
		sprintf (text, "%s\n", orderUserData->last_name);
		text = text + strlen(text);
	}

	if ((orderUserData->first_name[0] == '\0') &&
			(orderUserData->initial_name[0] == '\0') &&
			(orderUserData->last_name[0] == '\0'))
	{
		strcpy (text, "User Name:\t\tN/A\n");
		text = text + strlen(text);
	}

	if (orderUserData->organization[0] != '\0')
		sprintf (text, "Organization:\t\t%s\n", orderUserData->organization);
	else
		strcpy (text, "Organization:\t\tN/A\n");
	text = text + strlen(text);
	
	if (orderPtr->account_id[0] != '\0')
		sprintf (text, "Account Number:\t\t%s\n", orderPtr->account_id);
	else
		strcpy (text, "Account Number :\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->resource_type == -1)
		strcpy (text, "Resource Type:\t\tN/A\n");
	else
		{
			k = 0;
			while ((k < glbData.resource_type_count) && 
				 (orderUserData->resource_type != glbData.resource_type[k].item_id)) 
						k++;

			if (k < glbData.resource_type_count)
			{
				sprintf (text, "Resource Type:\t\t%s\n",
												glbData.resource_type[k].item_name);
			}
			else
				/* did not find the matching resource type id */
				strcpy (text, "Resource Type:\t\tN/A\n");
					
		}
	text = text + strlen(text);


	if (orderUserData->curr_balance == -1)
		strcpy (text, "Current Balance:\t\tN/A\n");
	else
		sprintf (text, "Current Balance:\t\t%-.2f\n",
						 orderUserData->curr_balance);
	text = text + strlen(text);

	if (orderUserData->street[0] != '\0')
		sprintf (text, "Address:\t\t\t%s\n", orderUserData->street);
	else
		strcpy (text, "Address:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->city[0] != '\0')
		sprintf (text, "City:\t\t\t%s\n", orderUserData->city);
	else
		strcpy (text, "City:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->state[0] != '\0')
		sprintf (text, "State:\t\t\t%s\n", orderUserData->state);
	else
		strcpy (text, "State:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->zipcode[0] != '\0')
		sprintf (text, "ZipCode:\t\t\t%s\n", orderUserData->zipcode);
	else
		strcpy (text, "ZipCode:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->country[0] != '\0')
		sprintf (text, "Country:\t\t\t%s\n", orderUserData->country);
	else
		strcpy (text, "Country:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->phone[0] != '\0')
		sprintf (text, "Phone:\t\t\t%s\n", orderUserData->phone);
	else
		strcpy (text, "Phone:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->fax[0] != '\0')
		sprintf (text, "Fax:\t\t\t%s\n", orderUserData->fax);
	else
		strcpy (text, "Fax:\t\t\tN/A\n");
	text = text + strlen(text);

	if (orderUserData->email[0] != '\0')
		sprintf (text, "Elec Mail:\t\t%s\n", orderUserData->email);
	else
		strcpy (text, "Elec Mail:\t\tN/A\n");
	text = text + strlen(text);

	XmTextSetString (userInfoST, userDataBuf);

	/* Change cursor back to normal */
	timeOutCursors (False);

	XtFree (userDataBuf);

	}
	UxOrderContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: order_displayResults
**
** Description:		Searches the catalog database for the orders that meet 
**								the search criteria specified by the user. 
**								The catalog event is OP_GETORDERLIST.
**								Function order_scroll_orderListsCb is called to display 
**								results in the order list widgets.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History: 09/09/96 - PR 85. If glbDefaultSearchFlag is set to 0,
**                              and if the query is from welcome_order
**                              callback, we pop up a blank Order 
**                              Production screen.
**==========================================================================*/

void order_displayResults( 
	Widget widget) 

{
	_UxCorder               *UxSaveCtx, *UxContext;
	_UxCsearch							*UxSearchContext;
	OP_CLIENT_DATA *clientData;
	Widget sbar;
	int status;
	int orderCount;
	char buffer[IMS_COL255_LEN+1];
	XmScrollBarCallbackStruct *cbs;


	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext (glbData.orderW);
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/* call order_scroll_orderListsCb to load data in order list widgets */
		cbs = (XmScrollBarCallbackStruct *)
					malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		order_scroll_orderListsCb(widget, NULL, cbs);
		free (cbs);

		/* Clean up item lists and text widgets */
		XmListDeleteAllItems (itemNoList);
		XmListDeleteAllItems (frameIdList);
		XmListDeleteAllItems (satList);
		XmListDeleteAllItems (procTypeList);
		XmListDeleteAllItems (itemPriorityList);
		XmListDeleteAllItems (itemQlkList);
		XmListDeleteAllItems (itemStatusList);
		XmListDeleteAllItems (mediaList);
		XmListDeleteAllItems (typeList);
		XmListDeleteAllItems (costList);
		XmListDeleteAllItems (validateList);
		XmListDeleteAllItems (shipList);
		XmListDeleteAllItems (billList);
		XmListDeleteAllItems (itemDummyList);
		XmTextFieldSetString (orderIdTF, "");
		XmTextFieldSetString (totalItemsTF, "");
		XmTextSetString (userInfoST, "");

		/* desensitize item menu */
		XtSetSensitive (viewItemDetailsMPB, False);
		XtSetSensitive (validateItemMPB, False);
		/*
		XtSetSensitive (unvalidateItemMPB, False);
		*/
		XtSetSensitive (updateItemPriorityMPB, False);
		XtSetSensitive (updateItemStatusMPB, False);
		XtSetSensitive (editItemCommentMPB, False);
		XtSetSensitive (itemProcessMediaMPB, False);
		XtSetSensitive (itemShipItemsMPB, False);
		XtSetSensitive (itemBillItemsMPB, False);
		XtSetSensitive (restartItemMPB, False);

		/*
		** 09/09/96 - pop up a blank Order Production Screen
		*/
		if (glbWelcomeOrderFlag && !glbDefaultSearchFlag)
		{
			/* no search query to display in Order Search Parameter ScrollText */
			XmTextSetString (searchParamST, NULL);

			/* disable refresh search push button */
			XtSetSensitive (refreshPB, False);

			/* disable refresh search menu item under Screen Functions */
			XtSetSensitive (refreshSearchMPB, False);

			/* disable save results menu item under Screen Functions */
			XtSetSensitive (saveResultsMPB, False);

			/* no order count */
			XmTextFieldSetString (totalOrdersTF, NULL);

			/* reset the glbWelcomeOrderFlag */
			glbWelcomeOrderFlag = 0;
		}
		else
		{
			/* need to display total number of orders in textfield widget */
			sprintf (buffer, "%d", clientData->orderCount);
			XmTextFieldSetString (totalOrdersTF, buffer);

			/* need to display query in Order Search Parameter ScrollText*/
			XmTextSetString (searchParamST, clientData->queryStruct.sqlBuf);

			/* sensitize refresh search push button */
			XtSetSensitive (refreshPB, True);

			/* sensitize refresh search menu item under Screen Functions */
			XtSetSensitive (refreshSearchMPB, True);

			/* sensitize save results menu item under Screen Functions */
			XtSetSensitive (saveResultsMPB, True);

		}

		XtPopup (XtParent(glbData.orderW), XtGrabNone);
		glbData.orderFlag = 1;

		/* Sensitize menu GoTo Order Display Screen in the Search Screen */
		if (glbData.searchFlag)
		{
			UxSearchContext = 
					(_UxCsearch *) UxGetContext( glbData.searchW );
			XtSetSensitive (UxSearchContext->UxgotoOrderMPB, True);
		}
		
		/* Change cursor back to normal */
		timeOutCursors (False);

	}

	UxOrderContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: order_closeCb
**
** Description:		Exit from the Order Screen.  
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/


void	order_closeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	_UxCsearch							*UxSearchContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	Widget sbar;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr, *orderNextPtr;
	OP_ORDER_ITEM_LIST *itemPtr, *itemNextPtr;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );
	{
		/*
		** data structure cleanup and database 
		*/

		clientData = &(glbData.orderClientData);

		/* Clean up item list and text widgets */
		XmListDeleteAllItems (itemNoList);
		XmListDeleteAllItems (frameIdList);
		XmListDeleteAllItems (satList);
		XmListDeleteAllItems (procTypeList);
		XmListDeleteAllItems (itemPriorityList);
		XmListDeleteAllItems (itemQlkList);
		XmListDeleteAllItems (itemStatusList);
		XmListDeleteAllItems (mediaList);
		XmListDeleteAllItems (typeList);
		XmListDeleteAllItems (costList);
		XmListDeleteAllItems (validateList);
		XmListDeleteAllItems (shipList);
		XmListDeleteAllItems (billList);
		XmListDeleteAllItems (itemDummyList);
		XmTextFieldSetString (orderIdTF, "");
		XmTextFieldSetString (totalItemsTF, "");
	
		/* Clean up order list and text widgets */
		XmListDeleteAllItems (orderIdList);
		XmListDeleteAllItems (userIdList);
		XmListDeleteAllItems (receivedList);
		XmListDeleteAllItems (orderPriorityList);
		XmListDeleteAllItems (orderQlkList);
		XmListDeleteAllItems (orderStatusList);
		XmListDeleteAllItems (totalList);
		XmListDeleteAllItems (generatedList);
		XmListDeleteAllItems (onlineList);
		XmListDeleteAllItems (holdList);
		XmListDeleteAllItems (errorList);
		XmListDeleteAllItems (orderDummyList);
		XmTextFieldSetString (totalOrdersTF, "");
		XmTextSetString (searchParamST, "");
		XmTextSetString (userInfoST, "");


		/* Desensitize order menu */
		XtSetSensitive (viewItemsPB, False);
		XtSetSensitive (viewOrderDetailsMPB, False);
		XtSetSensitive (validateOrderMPB, False);
		/*
		XtSetSensitive (unvalidateOrderMPB, False);
		*/
		XtSetSensitive (updateOrderPriorityMPB, False);
		XtSetSensitive (updateOrderStatusMPB, False);
		XtSetSensitive (editOrderCommentMPB, False);
		XtSetSensitive (processMediaMPB, False);
		XtSetSensitive (shipOrderMPB, False);
		XtSetSensitive (billOrderMPB, False);

		/* Desensitize item menu */
		XtSetSensitive (viewItemDetailsMPB, False);
		XtSetSensitive (validateItemMPB, False);
		/*
		XtSetSensitive (unvalidateItemMPB, False);
		*/
		XtSetSensitive (updateItemPriorityMPB, False);
		XtSetSensitive (updateItemStatusMPB, False);
		XtSetSensitive (editItemCommentMPB, False);
		XtSetSensitive (itemProcessMediaMPB, False);
		XtSetSensitive (itemShipItemsMPB, False);
		XtSetSensitive (itemBillItemsMPB, False);
		XtSetSensitive (restartItemMPB, False);


		/* free up the clientData structure */
		(void) free_orderList();

		/*
		orderPtr = clientData->orderList;
		while (orderPtr != (OP_ORDER_LIST *)NULL)
		{
			itemPtr = orderPtr->itemList;
			while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
			{
				itemNextPtr = itemPtr->next;
				free(itemPtr);
				itemPtr = itemNextPtr;
			} 
			orderPtr->itemList = (OP_ORDER_ITEM_LIST *)NULL;

			orderNextPtr = orderPtr->next;
			free(orderPtr);
			orderPtr = orderNextPtr;
		}

		clientData->orderList = (OP_ORDER_LIST *)NULL;
		clientData->orderCount = 0;
		*/

		if (clientData->currOrder != (OP_ORDER_LIST *)NULL)
		{
		/*
			itemPtr = clientData->currOrder->itemList;
			while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
			{
				itemNextPtr = itemPtr->next;
				free(itemPtr);
				itemPtr = itemNextPtr;
			}
			clientData->currOrder->itemList = (OP_ORDER_ITEM_LIST *)NULL;
			clientData->currOrder->item_count = 0;
		*/
			clientData->currOrder = (OP_ORDER_LIST *)NULL;
		}

		XtPopdown (XtParent(glbData.orderW));
		glbData.orderFlag = 0;

		/* desensitize menu GoTo Order Display Screen in the Search Screen */
		if (glbData.searchFlag)
		{
			UxSearchContext = 
					(_UxCsearch *) UxGetContext( glbData.searchW );
			XtSetSensitive (UxSearchContext->UxgotoOrderMPB, False);
		}
		
	}
	UxOrderContext = UxSaveCtx;

}

/*===========================================================================*
** 
** Function Name: order_browse_itemDetailsCb
**
** Description:		Display the details of the first item selected 
**								on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History: 04/09/1996 - added p_metadata_kbytes.
**                   06/04/1996 - added v0_process_type.
**
**==========================================================================*/

void	order_browse_itemDetailsCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	char *buffer;
	char *text;
	char label[IMS_COL30_LEN+1];
	int k, status;
	int i, count, itemCount;
	OP_DAR_DATA *darData;
  OP_PHOTOJOB_DATA *photoJobData;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	/* assign itemPtr to the item list of the current order */

	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;
	darData = (OP_DAR_DATA *)NULL;


	/* Locate screen items in itemList */
	count = itemCount - itemWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((itemPtr != NULL) && (itemPtr->position != itemWindowTop))
	{
		itemPtr = itemPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < count) && 
			(itemPtr->selectFlag == 0))
	{
		i++;
		itemPtr = itemPtr->next;
	}

	if ((itemPtr == (OP_ORDER_ITEM_LIST *)NULL) || (i >= count))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

  /* Added for PR2514*/
		if ((photoJobData = (OP_PHOTOJOB_DATA *) malloc
				((unsigned) sizeof (OP_PHOTOJOB_DATA))) == (OP_PHOTOJOB_DATA *) NULL)
		{
			/* display error message, return */
			strcpy(Msg, "Browse Item Details: Memory allocation failed.\n"
									 "Please contact DBA and exit the program.\n");
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}

		/* assign order_id, item_id to Photojob */
		memset (photoJobData, 0, sizeof(OP_PHOTOJOB_DATA));
		photoJobData->order_id = itemPtr->order_id;
		photoJobData->item_id = itemPtr->item_id;
		photoJobData->photojob_id = -1;

		/* 
		** call CAT function OP_GETDARDATA to populate darData
		*/
		catReq->item[0] = (OP_PHOTOJOB_DATA *)photoJobData;
		if ((status = ims_opCat (catReq, OP_GETPHOTOJOBID)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Internal Error: getPhotoJobid failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}
		
/*end of added*/

	/*
	** For DAR order_item_type, get information from table dar
	*/
	if (itemPtr->order_item_type == DAR)
	{
		if ((darData = (OP_DAR_DATA *) malloc
				((unsigned) sizeof (OP_DAR_DATA))) == (OP_DAR_DATA *) NULL)
		{
			/* display error message, return */
			strcpy(Msg, "Browse Item Details: Memory allocation failed.\n"
									 "Please contact DBA and exit the program.\n");
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}

		/* assign order_id, item_id to darData */
		memset (darData, 0, sizeof(OP_DAR_DATA));
		darData->order_id = itemPtr->order_id;
		darData->item_id = itemPtr->item_id;

		/* 
		** call CAT function OP_GETDARDATA to populate darData
		*/
		catReq->item[0] = (OP_DAR_DATA *)darData;
		if ((status = ims_opCat (catReq, OP_GETDARDATA)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Internal Error: getDarData failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}
		
	}


	/* Found the first selected item, allocate buffer storage space */
	if ((buffer = XtMalloc ((unsigned int) 4049)) == (char *)NULL)
	{
		/* display error message, return */
		sprintf(Msg, "Could not allocate space for text buffer.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return;
	}

	/* Write item information */
	text = buffer;
	sprintf (text,"\n    		Order ID: %d         Item ID: %d\n\n",
		itemPtr->order_id, itemPtr->item_id);
	text = text + strlen(text);

	strcpy (text, "====================================================================\n\n");
	text = text + strlen(text);
	
	if (itemPtr->order_item_type == DAR)
	{
		/*PR2514*/
		if (photoJobData->photojob_id != -1 )
			sprintf (text, "Photo Job id:\t\t%d\n\n", photoJobData->photojob_id);
		else
			strcpy (text, "Photo Job id:\t\tN/A\n\n");
		text = text + strlen(text);
		/*PR2514*/
		if (darData->platform[0] != '\0')
			sprintf (text, "Platform:\t\t\t%s\n\n", darData->platform);
		else
			strcpy (text, "Platform:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->sensor[0] != '\0')
			sprintf (text, "Sensor:\t\t\t%s\n\n", darData->sensor);
		else
			strcpy (text, "Sensor:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->mode[0] != '\0')
			sprintf (text, "Mode:\t\t\t%s\n\n", darData->mode);
		else
			strcpy (text, "Mode:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->asc_desc)
			sprintf (text, "Asc Desc:\t\t%c\n\n", darData->asc_desc);
		else
			strcpy (text, "Asc Desc:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->start_date[0] != '\0')
			sprintf (text, "Start Date:\t\t%s\n\n", darData->start_date);
		else
			strcpy (text, "Start Date:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->end_date[0] != '\0')
			sprintf (text, "End Date:\t\t%s\n\n", darData->end_date);
		else
			strcpy (text, "End Date:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->site_name[0] != '\0')
			sprintf (text, "Site Name:\t\t%s\n\n", darData->site_name);
		else
			strcpy (text, "Site Name:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->spatial_type == -1)
		{
			strcpy (text, "Spatial Type:\t\tN/A\n\n");
		}
		else
		{
			k = 0;
			while ((k < glbData.spatial_type_count) && 
						 (darData->spatial_type != glbData.spatial_type[k].item_id)) 
					k++;

			if (k < glbData.spatial_type_count)
			{
				sprintf (text, "Spatial Type:\t\t%s\n\n", 
											 glbData.spatial_type[k].item_name);
			}
			else
				/* did not find the matching spatial type id */
				strcpy (text, "Spatial Type:\t\tN/A\n\n");
		}
		text = text + strlen(text);

		if (darData->radius == -999)
			strcpy (text, "Radius:\t\t\tN/A\n\n");
		else
			sprintf (text, "Radius:\t\t\t%-.2f\n\n", darData->radius);
		text = text + strlen(text);

		if (darData->center_lat == -999)
			strcpy (text, "Center Lat:\t\tN/A\n\n");
		else
			sprintf (text, "Center Lat:\t\t%-.2f\n\n", darData->center_lat);
		text = text + strlen(text);

		if (darData->center_lon == -999)
			strcpy (text, "Center Lon:\t\tN/A\n\n");
		else
			sprintf (text, "Center Lon:\t\t%-.2f\n\n", darData->center_lon);
		text = text + strlen(text);

		if (darData->north_west_lat == -999)
			strcpy (text, "North West Lat:\t\tN/A\n\n");
		else
			sprintf (text, "North West Lat:\t\t%-.2f\n\n", darData->north_west_lat);
		text = text + strlen(text);

		if (darData->north_west_lon == -999)
			strcpy (text, "North West Lon:\t\tN/A\n\n");
		else
			sprintf (text, "North West Lon:\t\t%-.2f\n\n", darData->north_west_lon);
		text = text + strlen(text);

		if (darData->north_east_lat == -999)
			strcpy (text, "North East Lat:\t\tN/A\n\n");
		else
			sprintf (text, "North East Lat:\t\t%-.2f\n\n", darData->north_east_lat);
		text = text + strlen(text);

		if (darData->north_east_lon == -999)
			strcpy (text, "North East Lon:\t\tN/A\n\n");
		else
			sprintf (text, "North East Lon:\t\t%-.2f\n\n", darData->north_east_lon);
		text = text + strlen(text);

		if (darData->south_west_lat == -999)
			strcpy (text, "South West Lat:\t\tN/A\n\n");
		else
			sprintf (text, "South West Lat:\t\t%-.2f\n\n", darData->south_west_lat);
		text = text + strlen(text);

		if (darData->south_west_lon == -999)
			strcpy (text, "South West Lon:\t\tN/A\n\n");
		else
			sprintf (text, "South West Lon:\t\t%-.2f\n\n", darData->south_west_lon);
		text = text + strlen(text);

		if (darData->south_east_lat == -999)
			strcpy (text, "South East Lat:\t\tN/A\n\n");
		else
			sprintf (text, "South East Lat:\t\t%-.2f\n\n", darData->south_east_lat);
		text = text + strlen(text);

		if (darData->south_east_lon == -999)
			strcpy (text, "South East Lon:\t\tN/A\n\n");
		else
			sprintf (text, "South East Lon:\t\t%-.2f\n\n", darData->south_east_lon);
		text = text + strlen(text);

		if (darData->observation_freq[0] != '\0')
			sprintf (text, "Observation Frequency:\t%s\n\n",
										 darData->observation_freq);
		else
			strcpy (text, "Observation Frequency:\tN/A\n\n");
		text = text + strlen(text);

		if (darData->observation_num == -1)
			strcpy (text, "Observation Number:\tN/A\n\n");
		else
			sprintf (text, "Observation Number:\t%d\n\n", darData->observation_num);
		text = text + strlen(text);

		if (darData->pi_name[0] != '\0')
			sprintf (text, "PI Name:\t\t%s\n\n", darData->pi_name);
		else
			strcpy (text, "PI Name:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->pi_discipline[0] != '\0')
			sprintf (text, "PI Discipline:\t\t%s\n\n", darData->pi_discipline);
		else
			strcpy (text, "PI Discipline:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->active_p)
			sprintf (text, "Active:\t\t\t%c\n\n", darData->active_p);
		else
			strcpy (text, "Active:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->activity_start_date[0] != '\0')
			sprintf (text, "Activity Start Date:\t%s\n\n",
													 darData->activity_start_date);
		else
			strcpy (text, "Activity Start Date:\tN/A\n\n");
		text = text + strlen(text);

		if (darData->activity_end_date[0] != '\0')
			sprintf (text, "Activity End Date:\t%s\n\n",
													 darData->activity_end_date);
		else
			strcpy (text, "Activity End Date:\tN/A\n\n");
		text = text + strlen(text);

		if (darData->status == -1)
		{
			strcpy (text, "Dar Status:\t\tN/A\n\n");
		}
		else
		{
			k = 0;
			while ((k < glbData.dar_status_count) && 
						 (darData->status != glbData.dar_status[k].item_id)) 
					k++;

			if (k < glbData.dar_status_count)
			{
				sprintf (text, "Dar Status:\t\t%s\n\n", 
											 glbData.dar_status[k].item_name);
			}
			else
				/* did not find the matching dar status id */
				strcpy (text, "Dar Status:\t\tN/A\n\n");
		}
		text = text + strlen(text);

		if (darData->user_comment[0] != '\0')
			sprintf (text, "User Comment:\t\t%s\n\n", darData->user_comment);
		else
			strcpy (text, "User Comment:\t\tN/A\n\n");
		text = text + strlen(text);

		if (darData->planner_comment[0] != '\0')
			sprintf (text, "Planner Comment:\t%s\n\n", darData->planner_comment);
		else
			strcpy (text, "Planner Comment:\tN/A\n\n");
		text = text + strlen(text);

		if (darData->op_comment[0] != '\0')
			sprintf (text, "Operator Comment:\t%s\n\n", darData->op_comment);
		else
			strcpy (text, "Operator Comment:\tN/A\n\n");
		text = text + strlen(text);

	}
	else
	{
		if (itemPtr->cost_debited_p)
			sprintf (text, "Cost Debited:\t\t\t%c\n\n", itemPtr->cost_debited_p);
		else
			strcpy (text, "Cost Debited:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->platform[0] != '\0')
			sprintf(text, "Platform:\t\t\t\t%s\n\n", itemPtr->platform);
		else
			strcpy (text, "Platform:\t\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->sensor[0] != '\0')
			sprintf(text, "Sensor:\t\t\t\t%s\n\n", itemPtr->sensor);
		else
			strcpy (text, "Sensor:\t\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->dataset[0] != '\0')
			sprintf(text, "Dataset:\t\t\t\t%s\n\n", itemPtr->dataset);
		else
			strcpy (text, "Dataset:\t\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->v0_process_type[0] != '\0')
			sprintf(text, "V0 Process Type:\t\t\t%s\n\n", itemPtr->v0_process_type);
		else
			strcpy (text, "V0 Process Type:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->dataset_idx == -1)
			strcpy (text, "Dataset ID:\t\t\tN/A\n\n");
		else
			sprintf (text, "Dataset ID:\t\t\t%d\n\n", itemPtr->dataset_idx); 
		text = text + strlen(text);

		if (itemPtr->granule_idx == -1)
			strcpy (text, "Granule ID:\t\t\tN/A\n\n");
		else
			sprintf (text, "Granule ID:\t\t\t%d\n\n", itemPtr->granule_idx);
		text = text + strlen(text);

		if (itemPtr->granule_name[0] != '\0')
			sprintf(text, "Granule Name:\t\t\t%s\n\n", itemPtr->granule_name);
		else
			strcpy (text, "Granule Name:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->p_dataset_idx == -1)
			strcpy (text, "Product Dataset ID:\t\tN/A\n\n");
		else
			sprintf (text, "Product Dataset ID:\t\t%d\n\n", itemPtr->p_dataset_idx);
		text = text + strlen(text);

		if (itemPtr->p_granule_idx == -1)
			strcpy (text, "Product Granule ID:\t\tN/A\n\n");
		else
			sprintf (text, "Product Granule ID:\t\t%d\n\n", itemPtr->p_granule_idx);
		text = text + strlen(text);

		if (itemPtr->p_granule_name[0] != '\0')
			sprintf(text, "Product Granule Name:\t\t%s\n\n", itemPtr->p_granule_name);
		else
			strcpy (text, "Product Granule Name:\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->media_id[0] != '\0')
			sprintf (text, "Media ID:\t\t\t%s\n\n", itemPtr->media_id);
		else
			strcpy (text, "Media ID:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->media_fmt_type == -1)
		{
			strcpy (text, "Media Format:\t\t\tN/A\n\n");
		}
		else
		{
			k = 0;
			while ((k < glbData.media_fmt_type_count) && 
						 (itemPtr->media_fmt_type != glbData.media_fmt_type[k].item_id)) 
					k++;

			if (k < glbData.media_fmt_type_count)
			{
				sprintf (text, "Media Format:\t\t\t%s\n\n", 
											 glbData.media_fmt_type[k].item_name);
			}
			else
				/* did not find the matching media_fmt_type id */
				strcpy (text, "Media Format:\t\t\tN/A\n\n");
		}
		text = text + strlen(text);

		if (itemPtr->data_kbytes == -1)
			strcpy (text, "Product Data Size:\t\tN/A\n\n");
		else
			sprintf (text, "Product Data Size:\t\t%d kbytes\n\n",
							 itemPtr->data_kbytes);

		text = text + strlen(text);

		if (itemPtr->p_metadata_kbytes == -1)
			strcpy (text, "Product Metadata Size:\t\tN/A\n\n");
		else
			sprintf (text, "Product Metadata Size:\t\t%d kbytes\n\n", 
							 itemPtr->p_metadata_kbytes);

		text = text + strlen(text);

		/*PR2514*/
		if (photoJobData->photojob_id != -1 )
			sprintf (text, "Photo Job id:\t\t\t%d\n\n", photoJobData->photojob_id);
		else
			strcpy (text, "Photo Job id:\t\t\tN/A\n\n");
		text = text + strlen(text);
		/*PR2514*/
		if (itemPtr->billing_id == -1)
			strcpy (text, "Billing ID:\t\t\tN/A\n\n");
		else
			sprintf (text, "Billing ID:\t\t\t%d\n\n", itemPtr->billing_id);
		text = text + strlen(text);

		if (itemPtr->shipping_id == -1)
			strcpy (text, "Shipping ID:\t\t\tN/A\n\n");
		else
			sprintf (text, "Shipping ID:\t\t\t%d\n\n", itemPtr->shipping_id);
		text = text + strlen(text);

		if (itemPtr->step_name[0] != '\0')
			sprintf (text, "Step Name:\t\t\t%s\n\n", itemPtr->step_name);
		else
			strcpy (text, "Step Name:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->step_avgtime[0] != '\0')
			sprintf (text, "Step Average Time:\t\t%s\n\n", itemPtr->step_avgtime);
		else
			strcpy (text, "Step Average Time:\t\tN/A\n\n");
		text = text + strlen(text);
	
		if (itemPtr->step_sequence == -1)
			strcpy (text, "Step Sequence:\t\t\tN/A\n\n");
		else
			sprintf (text, "Step Sequence:\t\t\t%d\n\n", itemPtr->step_sequence);
		text = text + strlen(text);

		if (itemPtr->step_started_p)
			sprintf (text, "Step Started:\t\t\t%c\n\n", itemPtr->step_started_p);
		else
			strcpy (text, "Step Started:\t\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->process_status == -1)
			strcpy (text, "Process Status:\t\t\tN/A\n\n");
		else
		{
			k = 0;
			while ((k < glbData.pps_status_count) && 
						 (itemPtr->process_status != glbData.pps_status[k].item_id)) 
					k++;

			if (k < glbData.pps_status_count)
			{
				sprintf (text, "Process Status:\t\t\t%s\n\n", 
											 glbData.pps_status[k].item_name);
			}
			else
				/* did not find the matching process status id */
				strcpy (text, "Process Status:\t\t\tN/A\n\n");
		}
		text = text + strlen(text);

		if (itemPtr->process_comment[0] != '\0')
			sprintf (text, "Process Comment:\t\t%s\n\n", itemPtr->process_comment);
		else
			strcpy (text, "Process Comment:\t\tN/A\n\n");
		text = text + strlen(text);

		if (itemPtr->op_comment[0] != '\0')
			sprintf (text, "Operator Comment:\t\t%s\n\n", itemPtr->op_comment);
		else
			strcpy (text, "Operator Comment:\t\tN/A\n\n");
		text = text + strlen(text);
	} /* if (order_item_type) */

	strcpy (label, "Item    Detail    Information");
	browseDlg_popupCb (glbData.orderW, buffer, label); 

	XtFree (buffer);

	if (darData != (OP_DAR_DATA *)NULL)
	{
		free(darData);
	}

 }
	UxOrderContext = UxSaveCtx;
}

/*===========================================================================*
** 
** Function Name: order_browse_orderDetailsCb
**
** Description:		Display the details of the first order selected 
**								on the screen
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_browse_orderDetailsCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	char Msg[IMS_COL1024_LEN+1];
	char *buffer;
	char *text;
	char label[IMS_COL30_LEN+1];
	int status;
	int i, count, orderCount;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;


	/* Locate screen items in orderList */
	count = orderCount - orderWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
	{
		orderPtr = orderPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < count) && 
			(orderPtr->selectFlag == 0))
	{
		i++;
		orderPtr = orderPtr->next;
	}

	if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= count))
	{
		/* No order is selected, display message, return */
		sprintf(Msg, "No order is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}


	/* Found the first selected order, allocate buffer storage space */
	if ((buffer = XtMalloc ((unsigned int) 4049)) == (char *)NULL)
	{
		/* display error message, return */
		sprintf(Msg, "Could not allocate space for text buffer.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return;
	}

	/* Write order information */
	text = buffer;
	sprintf (text,"\n\t\t     Order ID:   %d\n\n", orderPtr->order_id);
	text = text + strlen(text);

	strcpy (text, "====================================================================\n\n");

	text = text + strlen(text);

	if (orderPtr->user_id[0] != '\0')
		sprintf (text, "User ID:\t\t\t%s\n\n", orderPtr->user_id);
	else
		strcpy (text, "User ID:\t\t\tN/A\n\n");
	text = text + strlen(text);
	
	if (orderPtr->account_id[0] != '\0')
		sprintf (text, "Account ID:\t\t%s\n\n", orderPtr->account_id);
	else
		strcpy (text, "Account ID:\t\tN/A\n\n");
	text = text + strlen(text);

	if (orderPtr->completed_time[0] != '\0')
		sprintf (text, "Completed Time:\t\t%s\n\n", orderPtr->completed_time);
	else
		strcpy (text, "Completed Time:\t\tN/A\n\n");
	text = text + strlen(text);
	
	if (orderPtr->op_comment[0] != '\0')
		sprintf (text, "Operator Comment:\t%s\n\n", orderPtr->op_comment);
	else
		strcpy (text, "Operator Comment:\tN/A\n\n");
	text = text + strlen(text);

	strcpy (label, "Order    Detail    Information");
	browseDlg_popupCb (glbData.orderW, buffer, label); 

	XtFree (buffer);

	}
	UxOrderContext = UxSaveCtx;
}

/*===========================================================================*
** 
** Function Name: order_update_orderPriorityCb
**
** Description:		Update the priority of the first selected order 
**								on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_update_orderPriorityCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_LIST *orderPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, count, orderCount;
	DBSMALLINT current_status;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;


	/* Locate screen items in orderList */
	count = orderCount - orderWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
	{
		orderPtr = orderPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < count) && 
			(orderPtr->selectFlag == 0))
	{
		i++;
		orderPtr = orderPtr->next;
	}

	if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= count))
	{
		/* No order is selected, display message, return */
		sprintf(Msg, "No order is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/*
	** Found the first selected order, let's verify the order status,
	** update item priority is allowed for orders with the following
	** status values: NEW, PEDNING, GENERATED.
	** If order status is qualified, popup the priority selectiondialog
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Priority,\n"
					 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Lock the order_queue table
	*/
	if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Priority,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

		
	/*
	** call CAT event OP_GETORDERSTATUS to get order's current status
	*/
	catReq->item[0] = (DBINT *)&orderPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Priority,\n"
									"OP_GETORDERSTATUS failed for Order ID: %d.\n",
									orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** if order status is not one of the following values, 
	** Roll Back the transaction, popup error message
	** dialog box, return.
	*/
	if ((current_status != ORDER_NEW) && 
			(current_status != ORDER_PENDING) &&
			(current_status != ORDER_GENERATED))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Display error messages */
		sprintf (Msg,"Update Order Priority:\n\n"
								 "Update Order Priority function is allowed only on "
								 "orders with the following status values:\n"
								 "NEW, PENDING, GENERATED.\n\n"
								 "Order: %d does not qualify for this operation.\n",
								 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Priority,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Update order priority is allowed only when items are
	** currently in view.
	*/
	if (orderPtr != clientData->currOrder)
	{
		sprintf(Msg,
		 "The items of order %d are not currently in view.\n\n"
		 "Please use View Items function to view the items\n\n" 
		 "of this order before updating the order priority.\n", 
		 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/* 
	** Order is corrupted, item list is NULL
	*/
	if (orderPtr->itemList == (OP_ORDER_ITEM_LIST *)NULL)
	{
		sprintf(Msg,
		 	"Order %d does not have any items in order_item queue.\n\n"
			"Please contact DBA.",
		 	orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

	/* Found the first selected order, create Priority list strings */
	selectionDlg_popupCb (glbData.orderW, (void *)orderPtr, ORDER_PRIORITY); 
	
	}
	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_update_itemPriorityCb
**
** Description:		Update the priority of the all the items selected
**								on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_update_itemPriorityCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, itemCount;
	DBSMALLINT current_status;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;

	/* Find the first selected item in the item list */
	i = 0;
	while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < itemCount) && 
			(itemPtr->selectFlag == 0))
	{
		i++;
		itemPtr = itemPtr->next;
	}

	if ((itemPtr == (OP_ORDER_ITEM_LIST *)NULL) || (i >= itemCount))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}


	itemPtr = clientData->currOrder->itemList;

	/*
	** Found the first selected item, let's verify the order status,
	** update item priority is allowed for orders with the following
	** status values: NEW, PEDNING, GENERATED.
	** If order status is qualified, popup the priority selectiondialog
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Priority,\n"
					 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
					 	itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Lock the order_queue table
	*/
	if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Priority,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

		
	/*
	** call CAT event OP_GETORDERSTATUS to get order's current status
	*/
	catReq->item[0] = (DBINT *)&itemPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Priority,\n"
									"OP_GETORDERSTATUS failed for Order ID: %d.\n",
									itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** if order status is not one of the following values, 
	** Roll Back the transaction, popup error message
	** dialog box, return.
	*/
	if ((current_status != ORDER_NEW) && 
			(current_status != ORDER_PENDING) &&
			(current_status != ORDER_GENERATED))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf (Msg,"Update Item Priority:\n\n"
								 "Update Item Priority function is allowed only on "
								 "orders with the following status values:\n"
								 "NEW, PENDING, GENERATED.\n\n"
								 "Order: %d does not qualify for this operation.\n",
								 itemPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Priority,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
					 	itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** pop up selection dialog with priority valids
	*/
	selectionDlg_popupCb (glbData.orderW, (void *)itemPtr, ITEM_PRIORITY);
	
	}
	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_edit_orderCommentCb
**
** Description:		Edit operator comment for the first order selected
**								on the screen
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_edit_orderCommentCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_LIST *orderPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, count, orderCount;
	DBSMALLINT current_status;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;


	/* Locate screen items in orderList */
	count = orderCount - orderWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
	{
		orderPtr = orderPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < count) && 
			(orderPtr->selectFlag == 0))
	{
		i++;
		orderPtr = orderPtr->next;
	}

	if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= count))
	{
		/* No order is selected, display message, return */
		sprintf(Msg, "No order is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/*
	** Found the first selected order, let's verify the order status,
	** edit order comment is not allowed for orders with the following
	** status values: COMPLETE, CANCEL, CANCELLED, and LOCKED.
	** If order status is qualified, popup the comment dialog
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Order Comment,\n"
					 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Lock the order_queue table
	*/
	if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Order Comment,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

		
	/*
	** call CAT event OP_GETORDERSTATUS to get order's current status
	*/
	catReq->item[0] = (DBINT *)&orderPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Order Comment,\n"
									"OP_GETORDERSTATUS failed for Order ID: %d.\n",
									orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** if order status is one of the following values, 
	** Roll Back the transaction, then popup error
	** message dialog box, return.
	*/
	if ((current_status == ORDER_COMPLETE) || 
			(current_status == ORDER_CANCELLED) ||
			(current_status == ORDER_CANCEL) ||
			(current_status == ORDER_LOCKED))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf (Msg,"Edit Order Comment:\n\n"
								 "Edit Order Comment function is not allowed on "
								 "orders with the following status values:\n"
								 "COMPLETE, CANCELLED, CANCEL, LOCKED.\n\n"
								 "Order: %d does not qualify for this operation.\n",
								 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Order Comment,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/* call commentDlg_popupCb to popup the comment dialog */
	commentDlg_popupCb (glbData.orderW, (void *)orderPtr, ORDER_COMMENT);

 }
 UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_edit_itemCommentCb
**
** Description:		Edit operator comment for the first item selected
**								on the screen
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_edit_itemCommentCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, count, itemCount;
	DBSMALLINT current_status;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;


	/* Locate screen items in itemList */
	count = itemCount - itemWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((itemPtr != NULL) && (itemPtr->position != itemWindowTop))
	{
		itemPtr = itemPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < count) && 
			(itemPtr->selectFlag == 0))
	{
		i++;
		itemPtr = itemPtr->next;
	}

	if ((itemPtr == (OP_ORDER_ITEM_LIST *)NULL) || (i >= count))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}


	/*
	** Found the first selected item, let's verify the item status,
	** edit item comment is not allowed for items with the following
	** status values: COMPLETE, CANCEL, CANCELLED, and LOCKED.
	** If item status is qualified, popup the comment dialog
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Item Comment,\n"
			 	"OP_BEGINTRANSACTION failed for Order ID: %d, Item ID: %d\n", 
			 	itemPtr->order_id, itemPtr->item_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Lock the order_item table
	*/
	if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Item Comment,\n" 
								 "OP_GETITEMLOCK failed for Order ID: %d, Item ID: %d\n", 
								 itemPtr->order_id, itemPtr->item_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

		
	/*
	** call CAT event OP_GETITEMSTATUS to get item's current status
	*/
	catReq->item[0] = (DBINT *)&itemPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
	catReq->item[2] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETITEMSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Edit Item Comment,\n"
						"OP_GETORDERITEMSTATUS failed for Order ID: %d, Item ID: %d\n",
						itemPtr->order_id, itemPtr->item_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** if order status is one of the following values, 
	** Roll Back the transaction, then popup error
	** message dialog box, return.
	*/
	if ((current_status == ITEM_COMPLETE) || 
			(current_status == ITEM_CANCELLED) ||
			(current_status == ITEM_CANCEL) ||
			(current_status == ITEM_LOCKED))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf (Msg,"Edit Item Comment:\n\n"
								 "Edit Item Comment function is not allowed on "
								 "items with the following status values:\n"
								 "COMPLETE, CANCELLED, CANCEL, LOCKED.\n\n"
								 "Order: %d Item: %d does not qualify for this operation.\n",
								 itemPtr->order_id, itemPtr->item_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Comment,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d, Item ID: %d\n", 
			 			itemPtr->order_id, itemPtr->item_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/* popup the comment dialog box */
	commentDlg_popupCb (glbData.orderW, (void *)itemPtr, ITEM_COMMENT);

 }
 UxOrderContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: order_update_orderStatusCb
**
** Description:		Update the status of the first selected order 
**								on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_update_orderStatusCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_LIST *orderPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, count, orderCount;
	DBSMALLINT current_status;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;


	/* Locate screen items in orderList */
	count = orderCount - orderWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
	{
		orderPtr = orderPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < count) && 
			(orderPtr->selectFlag == 0))
	{
		i++;
		orderPtr = orderPtr->next;
	}

	if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= count))
	{
		/* No order is selected, display message, return */
		sprintf(Msg, "No order is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}


	/*
	** Found the first selected order, let's verify the order status,
	** update order status is not allowed for orders with the following
	** status values: COMPLETE, CANCEL, CANCELLED.
	** If order status is qualified, popup the status selection dialog
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Status,\n"
					 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Lock the order_queue table
	*/
	if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Status,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

		
	/*
	** call CAT event OP_GETORDERSTATUS to get order's current status
	*/
	catReq->item[0] = (DBINT *)&orderPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Status,\n"
									"OP_GETORDERSTATUS failed for Order ID: %d.\n",
									orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** if order status is one of the following values, 
	** Roll Back the transaction, then popup error
	** message dialog box, return.
	*/
	if ((current_status == ORDER_COMPLETE) || 
			(current_status == ORDER_CANCELLED) ||
			(current_status == ORDER_CANCEL))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf (Msg,"Update Order Status:\n\n"
								 "Update Order Status function is not allowed on "
								 "orders with the following status values:\n"
								 "COMPLETE, CANCELLED, CANCEL.\n\n"
								 "Order: %d does not qualify for this operation.\n",
								 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Order Status,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Update order status is allowed only when items are
	** currently in view.
	*/
	if (orderPtr != clientData->currOrder)
	{
		sprintf(Msg,
		 "The items of order %d are not currently in view.\n\n"
		 "Please note that updating the status of an order\n\n"
		 "changes the statuses of all items in the order.\n\n"
		 "Please view the items of this order before proceeding the update.",
		 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/* 
	** Order is corrupted, item list is NULL
	*/
	if (orderPtr->itemList == (OP_ORDER_ITEM_LIST *)NULL)
	{
		sprintf(Msg,
		 	"Order %d does not have any items in order_item queue.\n\n"
			"Please contact DBA.",
		 	orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/* Found the first selected order, popup status valids dialog */
	selectionDlg_popupCb (glbData.orderW, (void *)orderPtr, ORDER_STATUS); 
	
	}
	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_update_itemStatusCb
**
** Description:		Update the status of the all the items selected
**								on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_update_itemStatusCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, itemCount;
	DBSMALLINT current_status;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;


	/* Find the first selected item in the item list */
	i = 0;
	while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < itemCount) && 
			(itemPtr->selectFlag == 0))
	{
		i++;
		itemPtr = itemPtr->next;
	}

	if ((itemPtr == (OP_ORDER_ITEM_LIST *)NULL) || (i >= itemCount))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	itemPtr = clientData->currOrder->itemList;

	/*
	** Found the first selected item, let's verify the order status,
	** update order status is not allowed for orders with the following
	** status values: COMPLETE, CANCEL, CANCELLED.
	** If order status is qualified, popup the status selection dialog
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Status,\n"
					 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
					 	itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Lock the order_queue table
	*/
	if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Status,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}

		
	/*
	** call CAT event OP_GETORDERSTATUS to get order's current status
	*/
	catReq->item[0] = (DBINT *)&itemPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Status,\n"
									"OP_GETORDERSTATUS failed for Order ID: %d.\n",
									itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** if order status is one of the following values, 
	** Roll Back the transaction, then popup error
	** message dialog box, return.
	*/
	if ((current_status == ORDER_COMPLETE) || 
			(current_status == ORDER_CANCELLED) ||
			(current_status == ORDER_CANCEL))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf (Msg,"Update Item Status:\n\n"
								 "Update Item Status function is not allowed on "
								 "orders with the following status values:\n"
								 "COMPLETE, CANCELLED, CANCEL.\n\n"
								 "Order: %d does not qualify for this operation.\n",
								 itemPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Item Status,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
					 	itemPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/* Found the first selected item, create selection dialog */
	selectionDlg_popupCb (glbData.orderW, (void *)itemPtr, ITEM_STATUS);
	
	}
	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_validate_orderCb
**
** Description:		Set validated_p to 'Y' for all the items of the order
**								selected on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_validate_orderCb( 
	Widget widget, 
	int field, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	char value;
	int status, modifyFlag;
	int len = 0;
	int i, count, orderCount;
	DBSMALLINT current_status;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
 {

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;
	modifyFlag = 0;


	/* Locate screen items in orderList */
	count = orderCount - orderWindowTop;
	if (count > OP_ROWS)
		count = OP_ROWS;

	while ((orderPtr != NULL) && (orderPtr->position != orderWindowTop))
	{
		orderPtr = orderPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < count) && 
			(orderPtr->selectFlag == 0))
	{
		i++;
		orderPtr = orderPtr->next;
	}

	if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= count))
	{
		/* No order is selected, display message, return */
		sprintf(Msg, "No order is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}


	/*
	** validate order is allowed only when order has status 
	** NEW or PENDING.
	*/
	if ((orderPtr->status != ORDER_NEW) &&
			(orderPtr->status != ORDER_PENDING))
	{
		sprintf(Msg, 
			"Validate Order function is only allowed\n\n"
			"on orders with status NEW or PENDING.\n");

		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}


	/*
	** Validate order is allowed only when items are
	** currently in view.
	*/
	if (orderPtr != clientData->currOrder)
	{
		sprintf(Msg,
		 "The items of order %d are not currently in view.\n\n"
		 "Please use View Items function to view the items\n\n" 
		 "of this order before proceeding the validation.\n", 
		 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/* 
	** Order is corrupted, item list is NULL
	*/
	if (orderPtr->itemList == (OP_ORDER_ITEM_LIST *)NULL)
	{
		sprintf(Msg,
		 	"Order %d does not have any items in order_item queue.\n\n"
			"Please contact DBA.",
		 	orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
		return;
	}


	/*
	** Found the first selected order, validate all items 
	** associated with the order
	*/
	
	/* Change cursor to watch cursor */
	timeOutCursors (True);

	itemPtr = orderPtr->itemList;
	while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
	{
		/*
		** Begin the update transaction 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(new_msg,
							"Internal Error: Validate Order, \n"	
			 				"OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n\n", 
				 			itemPtr->order_id, itemPtr->item_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			itemPtr = itemPtr->next;
			continue;
		}


		/*
		** Lock the order_item table 
		*/
		if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			sprintf(new_msg,
							"Internal Error: Validate Order, \n"	
						  "OP_GETITEMLOCK failed for Order: %d, Item: %d\n\n", 
							itemPtr->order_id, itemPtr->item_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			itemPtr = itemPtr->next;
			continue;
		}

		/* 
		** Get current item status from order_item table
		*/
		catReq->item[0] = (DBINT *)&itemPtr->order_id;
		catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
		catReq->item[2] = (DBSMALLINT *)&current_status;
		if ((status = ims_opCat(catReq, OP_GETITEMSTATUS)) < 0)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			sprintf(new_msg,
							"Internal Error: Validate Order, \n"	
						 	"OP_GETITEMSTATUS failed for Order: %d, Item: %d\n\n", 
						 	itemPtr->order_id, itemPtr->item_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			itemPtr = itemPtr->next;
			continue;
		}

		/*
		** if item status is not NEW, we cannot validate it.
		*/
		if (current_status != ITEM_NEW)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			sprintf(new_msg,
							"Order %d, Item %d does not have status NEW.\n"	
							"Validatation is allowed only on items with status NEW.\n\n", 
						 	itemPtr->order_id, itemPtr->item_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			itemPtr = itemPtr->next;
			continue;

		}


		/*
		** call CAT event OP_VALIDATEITEM 
		** 02/12/96 - pass itemPtr and value 
		*/

		catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;

		if (field)
		{
			value = 'N'; 
			catReq->item[1] = (char *)&value;
		}
		else
		{
			value = 'Y';
			catReq->item[1] = (char *)&value;
		}

		if ((status = ims_opCat(catReq, OP_VALIDATEITEM)) < 0)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(new_msg,
							"Internal Error: Validate Order, \n"	
						  "OP_VALIDATEITEM failed for Order: %d, Item: %d\n\n", 
							itemPtr->order_id, itemPtr->item_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			itemPtr = itemPtr->next;
			continue;

		}

		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(new_msg, 
				"Internal Error: Validate Order\n"
				"OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n\n",
				itemPtr->order_id, itemPtr->item_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

			itemPtr = itemPtr->next;
			continue;
		}


		/*
		** update status of the item in memory 
		*/
		if (field)
		{
			itemPtr->validated_p = 'N';
			itemPtr->status = ITEM_NEW;
		}
		else
		{
			itemPtr->validated_p = 'Y';
			itemPtr->status = ITEM_VALIDATED;
		}

		/* set modifyFlag */
		modifyFlag = 1;

		itemPtr = itemPtr->next;

	}  /* while */

	
	/*
	** if item got validated and if items are currently being  
	** displayed, refresh item lists 
	*/
	if (modifyFlag)
	{
		/*
		** Refresh the Item lists first
		*/
		if ((orderPtr == clientData->currOrder ) && 
			(clientData->currOrder->itemList != (OP_ORDER_ITEM_LIST *)NULL))
		{
			scroll_cbs = (XmScrollBarCallbackStruct *)
						malloc(sizeof(XmScrollBarCallbackStruct));
			scroll_cbs->value = clientData->itemWindowTop;
			order_scroll_itemListsCb (glbData.orderW, NULL, scroll_cbs);  
			free (scroll_cbs);
		}

		/*
		** 03/08/96 - Let's get the new order status
		*/

		/*
		** Begin transacation
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Order,\n"
						 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
						 	orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}


		/*
		** Lock the order_queue table
		*/
		if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Order,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}

		
		/*
		** call CAT event OP_GETORDERSTATUS to get order's current status
		*/
		catReq->item[0] = (DBINT *)&orderPtr->order_id;
		catReq->item[1] = (DBSMALLINT *)&orderPtr->status;
		if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Order,\n"
									 "OP_GETORDERSTATUS failed for Order ID: %d.\n",
									 orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}


		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Order,\n"
						 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
						 	orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}

		/*
		** refresh order lists 
		*/
		scroll_cbs = (XmScrollBarCallbackStruct *)
						malloc(sizeof(XmScrollBarCallbackStruct));
		scroll_cbs->value = clientData->orderWindowTop;
		order_scroll_orderListsCb (glbData.orderW, NULL, scroll_cbs);  
		free (scroll_cbs);

	}


	/* Display all messages in browse dialog box */
	if (concat_msg.Msg != NULL)
	{
		if ((len = strlen(concat_msg.Msg)) > 1024)
		{
			strcpy (label, "Validate  Order  Message  Box");
			browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
		}
		else
		{
			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
		}
	}

	/* free up concat_msg space allocated */
	OP_FREECHARPTR(concat_msg.Msg);

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_validate_itemCb
**
** Description:		validate all the items selected on the screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_validate_itemCb( 
	Widget widget, 
	int field, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_ORDER_LIST *orderPtr;
	char Msg[IMS_COL1024_LEN+1];
	char value;
	int len = 0;
	int status, foundFlag;
	int i, itemCount, modifyFlag;
	DBSMALLINT current_status;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;
	orderPtr = clientData->currOrder;

	/*
	** 02/12/96 - let's verify that the current order has
	** status NEW or PENDING.
	** validate order is allowed only when order has status 
	** NEW or PENDING.
	*/
	if ((clientData->currOrder->status != ORDER_NEW) &&
			(clientData->currOrder->status != ORDER_PENDING))
	{
		sprintf(Msg, 
			"Validate Item function is only allowed\n\n"
			"on orders with status NEW or PENDING.\n");

		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/* Change cursor to watch cursor */
	timeOutCursors (True);


	/*
	** 02/12/96 - proceed to validate selected items
	*/
	i = 0;
	foundFlag = 0;
	modifyFlag = 0;

	while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < itemCount)) 
  {
		if (itemPtr->selectFlag) 
		{
			foundFlag = 1;

			/*
			** 02/12/96 - field 0 = ValidateItem, field 1 = Unvalidate Item
			*/
			if (((itemPtr->validated_p == 'N') && (field == 0)) || 
				((itemPtr->validated_p == 'Y') && (field == 1)))
			{
				/*
				** Begin Transacation
				*/
				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* Display error messages */
					sprintf(new_msg, "Internal Error:\nValidate Item,\n"
								 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n\n", 
								 itemPtr->order_id, itemPtr->item_id);   
	
					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** Lock the order_item table
				*/
				if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* Display error messages */
					sprintf(new_msg, "Internal Error:\nValidate Item,\n"
								 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n\n", 
								 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
	
				}


				/* 
				** Get current item status from order_item table
				*/
				catReq->item[0] = (DBINT *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&current_status;
				if ((status = ims_opCat(catReq, OP_GETITEMSTATUS)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(new_msg,
							"Internal Error: Validate Item, \n"	
						 	"OP_GETITEMSTATUS failed for Order: %d, Item: %d\n\n", 
						 	itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** if item status is not NEW, we cannot validate it.
				*/
				if (current_status != ITEM_NEW)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					sprintf(new_msg,
							"Order %d, Item %d does not have status NEW.\n"	
							"Validatation is allowed only on items with status NEW.\n\n", 
						 	itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;

				}


				/*
				** 02/12/96 - call CAT event OP_VALIDATEITEM to 
				** update item status, validated_p flag and 
				** to update dar table if the item has order_item_type
				** DAR.
				*/

				catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;

				if (field)
				{
					value = 'N'; 
					catReq->item[1] = (char *)&value;
				}
				else
				{
					value = 'Y';
					catReq->item[1] = (char *)&value;
				}

				if ((status = ims_opCat(catReq, OP_VALIDATEITEM)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* Display error messages */
					sprintf(new_msg, "Internal Error:\nValidate Item,\n"
								 "OP_VALIDATEITEM failed for Order: %d, Item: %d\n\n", 
								 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** Commit Transaction
				*/
				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* Display error messages */
					sprintf(new_msg, "Internal Error:\nValidate Item,\n"
								 "OP_VALIDATEITEM failed for Order: %d, Item: %d\n\n", 
								 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}
				

				/*
				** modify validated_p of item in memory
				*/

				modifyFlag = 1;

				if (field)
				{
					itemPtr->validated_p = 'N';
					itemPtr->status = ITEM_NEW;
				}
				else
				{
					itemPtr->validated_p = 'Y';
					itemPtr->status = ITEM_VALIDATED;
				}
			}
			
		}
		i++;
		itemPtr = itemPtr->next;
	}

	if (!foundFlag)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/*
	** Refresh item lists 
	*/
	if (modifyFlag)
	{
		/*
		** Refresh the Item lists first
		*/
		scroll_cbs = (XmScrollBarCallbackStruct *)
					malloc(sizeof(XmScrollBarCallbackStruct));
		scroll_cbs->value = clientData->itemWindowTop;
		order_scroll_itemListsCb (glbData.orderW, NULL, scroll_cbs);  
		free (scroll_cbs);

		/*
		** 03/08/96 - Let's get the new order status
		*/

		/*
		** Begin transacation
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Item,\n"
						 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
						 	orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}


		/*
		** Lock the order_queue table
		*/
		if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Item,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}

		
		/*
		** call CAT event OP_GETORDERSTATUS to get order's current status
		*/
		catReq->item[0] = (DBINT *)&orderPtr->order_id;
		catReq->item[1] = (DBSMALLINT *)&orderPtr->status;
		if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Item,\n"
									 "OP_GETORDERSTATUS failed for Order ID: %d.\n",
									 orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}


		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: Validate Item,\n"
						 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
						 	orderPtr->order_id);   

			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 
			return;
		}

		/*
		** refresh order lists 
		*/
		scroll_cbs = (XmScrollBarCallbackStruct *)
						malloc(sizeof(XmScrollBarCallbackStruct));
		scroll_cbs->value = clientData->orderWindowTop;
		order_scroll_orderListsCb (glbData.orderW, NULL, scroll_cbs);  
		free (scroll_cbs);

	}

	/* Display all messages in browse dialog box */
	if (concat_msg.Msg != NULL)
	{
		if ((len = strlen(concat_msg.Msg)) > 1024)
		{
			strcpy (label, "Validate  Items  Message  Box");
			browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
		}
		else
		{
			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
		}
	}

	/* free up concat_msg space allocated */
	OP_FREECHARPTR(concat_msg.Msg);

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_goto_searchCb
**
** Description:		Pop up the search screen from order screen  
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_goto_searchCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	_UxCsearch 							*UxSearchContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );
	{
		/* Sensitize menu GoTo Order Display Screen in the Search Screen */
		UxSearchContext = 
				(_UxCsearch *) UxGetContext( glbData.searchW );
		XtSetSensitive (UxSearchContext->UxgotoOrderMPB, True);

		XtPopup(XtParent(glbData.searchW), XtGrabNone);
		glbData.searchFlag = 1;
	}

	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_goto_welcomeCb
**
** Description:		Pop up the welcome screen from order screen  
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_goto_welcomeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );
	{
		XtPopup(XtParent(glbData.welcomeW), XtGrabNone);
		glbData.welcomeFlag = 1;
	}

	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_refreshCb
**
** Description:		refresh the order screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/


void	order_refreshCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	OP_CLIENT_DATA *clientData;
	char Msg[IMS_COL1024_LEN+1];
	int status;

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/* assign client to orderClientData from glbData structure */
		clientData = &(glbData.orderClientData);
		
		if ((status = search_executeQuery(glbData.orderW)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);
			return;
		}
		else
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* re-display order screen with results from the query */
			order_displayResults (glbData.orderW);
		}

	}

	UxOrderContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: order_printScreenCb
**
** Description:		print the current order screen
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/


void	order_printScreenCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.orderW);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}

	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_processMediaCb
**
** Description:		callback function for Process Media push button 
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History: 07/09/96 - Verify device existence on the host,
**                              this is done to correct PR 980.
** 
**==========================================================================*/

void	order_processMediaCb(
	Widget wgt, 
	int fieldId,
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_TAPE_ITEM_LIST *tapeItemList, *tapeItemPtr;
	OP_TAPE_ITEM_LIST *t1Ptr, *t2Ptr;
	OP_MEDIA_ITEM_TYPE mediaItemArray[MAX_TAPE_ITEMS];
	IMS_MSG_STRUCT *batchMsgDesc;
	char query[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	double new_capacity;
	int status;
	int i, j, count, orderCount;
	int mediaItemCount;
	int capacity, itemCount;
	int moreToProcess, currDataBytes;
	OP_MEDIA_JOB_LIST  *mediaJobPtr, *sPtr, *tPtr, *jobPtr;
	OP_MEDIA_BATCH_LIST *bPtr, *b2Ptr, *newBatchPtr, *batchPtr;
	DBSMALLINT item_status, step_sequence;
	int found;
	int mediaSpecCount;
	Display *dpy;
	struct 
	{
		DBSMALLINT media_type;
		DBSMALLINT media_fmt_type;
	} mediaSpec[IMS_COL128_LEN+1];


 UxSaveCtx = UxOrderContext;
 UxOrderContext = UxContext =
 		(_UxCorder *) UxGetContext( glbData.orderW );
{
	/* 
	** 07/09/96 - this is to correct PR 980.
	** Let's check whether there is any media device 
	** on the current host before further processing.
	*/
	if (glbData.device_list_count <= 0)
	{
		/* There is no media device on this machine, return */
		strcpy (Msg, 
			"There is no device for media processing on the host.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
		return;
	}

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;

	tapeItemList = (OP_TAPE_ITEM_LIST *)NULL;
	memset (&mediaSpec, 0, sizeof(mediaSpec));

	/* userSpec assignment */
	catReq = &(clientData->catReq);
	userSpec = &(catReq->userSpec);


	if (fieldId == PROCESS_ORDER) 
	{
		/* process media called from order level */
		/* Locate the selected order in orderList */
		i = 0;
		while ((orderPtr != (OP_ORDER_LIST *)NULL) && (i < orderCount) && 
					(orderPtr->selectFlag == 0))
		{
			i++;
			orderPtr = orderPtr->next;
		}

		if ((orderPtr == (OP_ORDER_LIST *)NULL) || (i >= orderCount))
		{
			/* No order is selected, display message, return */
			sprintf(Msg, "No order is selected.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}

		/* Mag Tape-have status IN-MEDIA */

		/* execute stored procedure get_tape_items to see if there  
		** are items ready for tape distribution, update item status
		** from ON-LINE to LOCKED to prevent another operator from 
		** distributing the same item to tape
		*/

		catReq->item[0] = (int *)&orderPtr->order_id;
		catReq->item[1] = (int *)&count;
		catReq->item[2] = (OP_TAPE_ITEM_LIST *)tapeItemList;
		if ((status = ims_opCat (catReq, OP_GETTAPEITEMS)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error: OP_GETTAPEITEM failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}
		count = *(int *)catReq->item[1];
		tapeItemList = (OP_TAPE_ITEM_LIST *)catReq->item[2];

		/*
		** for each item in the tapeItemList do the following:
		**   	Being transaction 
		**  	exec get_order_lock
		**    exec op_update_item_status to update item status to LOCKED
		**    Commit transaction
		*/
		tapeItemPtr = tapeItemList;
		while (tapeItemPtr != (OP_TAPE_ITEM_LIST *)NULL)
		{
			if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nProcess Media,\n"
										 "OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
										 orderPtr->order_id);   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		
				/*
				** Internal error, roll back all tape items' statuses 
				** do not continue processing 
				*/
				if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
				{
					rollback_tapeItemsStatus (tapeItemList);
					free_tapeItemList (&tapeItemList);
				}

				return; 
			}

			if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nProcess Media,\n"
										 "OP_GETITEMLOCK failed for Order ID: %d\n", 
										 orderPtr->order_id);   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
				/*
				** Internal error, roll back all tape items' statuses 
				** do not continue processing 
				*/
				if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
				{
					rollback_tapeItemsStatus (tapeItemList);
					free_tapeItemList (&tapeItemList);
				}

				return;
			}
	
			/* call op_update_item_status to set item status to LOCKED */
			catReq->item[0] = (DBINT *)&tapeItemPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&tapeItemPtr->item_id;
			tapeItemPtr->status = ITEM_LOCKED;
			catReq->item[2] = (DBSMALLINT *)&tapeItemPtr->status;

			if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf (Msg, "Internal Error: Update Order Item Status: "
										  "OP_UPDATEITEMSTATUS failed for \n "
									 	  "Order: %d, Item: %d\n.", 
										  tapeItemPtr->order_id, tapeItemPtr->item_id);   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

				/* mark this tape item as ERROR, proceed to the next item */
				/* 09/17/96 changed from 12 to ITEM_ERROR 13 */
				tapeItemPtr->status = ITEM_ERROR;
				tapeItemPtr = tapeItemPtr->next;
				continue;

			}

			if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nProcess Media,\n"
										 "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
										 orderPtr->order_id);   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

				/*
				** Internal error, roll back all tape items' statuses 
				** do not continue processing 
				*/
				if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
				{
					rollback_tapeItemsStatus (tapeItemList);
					free_tapeItemList (&tapeItemList);
				}

				return;
			}

			tapeItemPtr = tapeItemPtr->next;
		}


		if ((count <= 0) || (tapeItemList == (OP_TAPE_ITEM_LIST *)NULL))
		{
			/* Display error messages */
			sprintf(Msg, "No item is ready for media distribution.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}
	
	}

	if (fieldId == PROCESS_ITEMS)
	{
		/* process media called from item level */
		itemPtr = clientData->currOrder->itemList;
		itemCount = clientData->currOrder->item_count;
		
		found = False;
		count = 0;

		while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL) 
 		{
			/*
			** 12/15/95 - Only items with status IN-MEDIA can be distributed
			** Also verify the item's media type is one of the followings:
			** 1: 4-MM, 2: 4-MM HD, 3: 8-MM, 4: 8-MM HD, 5: 9-TRACK, 
			** 6: 9-TRACK HD, 7: DISK
			** If item qualifies the above conditions, lock the item by 
			** updating the item status to LOCKED.
			*/
			if ((itemPtr->selectFlag) && (itemPtr->status == ITEM_IN_MEDIA) &&
					(itemPtr->media_type >= IMS_4_MM) &&
					(itemPtr->media_type <= IMS_DISK)) 
			{
				found = True;
	
				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nProcess Media,\n"
										 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
										 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
					/*
					** Internal error, roll back all tape items' statuses 
					** do not continue processing 
					*/
					if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return; 
				}

				if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nProcess Media,\n"
											 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
											 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			
					/*
					** Internal error, roll back all tape items' statuses 
					** do not continue processing 
					*/
					if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return;
				}

				/*
				** Let's do another check just to verify that the item
				** status has not been changed since the operator selected
				** this item for media processing.
				** call OP_GETITEMSTATUS to verify that the 
				** item status is 8 - IN-MEDIA.
				*/
				catReq->item[0] = (int *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&item_status;
				if ((status = ims_opCat(catReq, OP_GETITEMSTATUS)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nProcess Media,\n"
											 "OP_GETITEMSTATUS failed for Order: %d, Item: %d\n", 
											 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb(glbData.orderW, IMS_FATAL, Msg);

					/*
					** Internal error, roll back all tape items' statuses
					** do not continue processing 
					*/
					if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return;
				}

				if (item_status != ITEM_IN_MEDIA)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nProcess Media:\n"
											 "Order: %d, Item: %d\n not ready for media.", 
											 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb(glbData.orderW, IMS_FATAL, Msg);

					/* update item status in currOrder, proceed to the next item */
					itemPtr->status = item_status;
					itemPtr = itemPtr->next;
					continue;
				}

				/*****************************************************************
				** 12/15/95 - No need to verify Step Sequence with Schema v3.30
				** call OP_GETITEMSTEPSEQUENCE to verify that the 
				** step sequence is 5 - copy to media
				******************************************************************/

				/*****************************************************************
				catReq->item[0] = (int *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_GETITEMSTEPSEQUENCE)) < 0)
				{
					*** rollback transaction ***
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					*** Display error messages ***
					sprintf(Msg, "Internal Error:\nProcess Media,\n"
											 "OP_GETITEMSTATUS failed for Order: %d, Item: %d\n", 
											 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb(glbData.orderW, IMS_FATAL, Msg);

					**
					** Internal error, roll back all tape items' statuses 
					** do not continue processing 
					**
					if (tapeItemList != (OP_TAPE_ITEM_LIST *)NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return;
				}

				if (step_sequence != 5)
				{
					*** rollback transaction ***
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					*** Display error messages ***
					sprintf(Msg, "Internal Error:\nProcess Media:\n"
											 "Order: %d, Item: %d\n not ready for media.", 
											 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb(glbData.orderW, IMS_FATAL, Msg);

					itemPtr->step_sequence = step_sequence;
					itemPtr = itemPtr->next;

					continue;
				}

				*****************************************************************/

				/*
				** call op_update_item_status to set item status to LOCKED
				*/
				catReq->item[0] = (DBINT *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				item_status = ITEM_LOCKED; /* item status 20 == LOCKED */
				catReq->item[2] = (DBSMALLINT *)&item_status;

				if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf (Msg, "Internal Error: Update Order Item Status: "
											  "OP_UPDATEITEMSTATUS failed for \n"
										 	  "Order: %d, Item: %d\n.", 
												 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					/* proceed to the next item */
					itemPtr = itemPtr->next;

					continue;
				}

				if ((tapeItemPtr = (OP_TAPE_ITEM_LIST *) malloc
				((unsigned) sizeof (OP_TAPE_ITEM_LIST))) ==
						(OP_TAPE_ITEM_LIST *) NULL)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error: memory allocation failed.");   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					/* rollback all other item status */
					if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return;
				}
				else
				{
					/* add item into the tapeItemList */

					tapeItemPtr->order_id = itemPtr->order_id;
					tapeItemPtr->item_id = itemPtr->item_id;
					tapeItemPtr->status = item_status;
					tapeItemPtr->media_type = itemPtr->media_type;
					tapeItemPtr->media_fmt_type = itemPtr->media_fmt_type;
					tapeItemPtr->data_kbytes = itemPtr->data_kbytes;
					tapeItemPtr->prev = NULL;
					tapeItemPtr->next = NULL;

					if (tapeItemList == (OP_TAPE_ITEM_LIST *)NULL)
					{
						/* first item in the list */
						tapeItemList = tapeItemPtr;
					}
					else
					{
						t1Ptr = t2Ptr = tapeItemList; 
						while (t2Ptr != (OP_TAPE_ITEM_LIST *)NULL)
						{
							t1Ptr = t2Ptr;
							t2Ptr = t2Ptr->next;
						}
						/* add new entry */
						t1Ptr->next = tapeItemPtr;
						tapeItemPtr->prev = t1Ptr;
					}

				}


				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nProcess Media,\n"
											 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
											 itemPtr->order_id, itemPtr->item_id);   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					/* rollback all other item status */
					if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return;
				}

			} /* if select flag */
		
			itemPtr = itemPtr->next;
		}

		if (!found)
		{
			/* No item is selected, display message, return */
			strcpy(Msg,
						 "Please verify selections are valid for media distribution.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}

		if (tapeItemList == (OP_TAPE_ITEM_LIST *)NULL)
		{
			/* Display error messages */
			sprintf(Msg, "No item is ready for media distribution.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}
	
 	}


	/*
	** Search through tapeItemList, find all distinct pairs of   
	** media_type & media_fmt_type. Stores the count in mediaSpecCount
	** and populate mediaSpec array.
	*/
	mediaSpecCount = 0;
	tapeItemPtr = tapeItemList;

	while (tapeItemPtr != (OP_TAPE_ITEM_LIST *)NULL)
	{
		i = 0;
		found = False;
		while (!found && i < mediaSpecCount)
		{
			if ((tapeItemPtr->media_type == mediaSpec[i].media_type) &&
					(tapeItemPtr->media_fmt_type == mediaSpec[i].media_fmt_type))
			{
				found = True;
			}
			else
			{
				i++; 
			}
		}
		if (!found)
		{
			mediaSpec[mediaSpecCount].media_type = tapeItemPtr->media_type;
			mediaSpec[mediaSpecCount].media_fmt_type = tapeItemPtr->media_fmt_type;
			mediaSpecCount++;
		}

		tapeItemPtr = tapeItemPtr->next;
	}

	/*
	** Search through the clientData->mediaJobList, if this 
	** order is not in the list, create a new media job entry
	** otherwise, do nothing.  
	*/
	tapeItemPtr = tapeItemList;

	found = False;
	jobPtr = clientData->mediaJobList;

	while ((jobPtr != (OP_MEDIA_JOB_LIST *)NULL) && (!found))
	{
		if (jobPtr->order_id == tapeItemPtr->order_id)
			found = True;
		else
			jobPtr = jobPtr->next;
	}

	if (!found)
	{
		/* create an entry for this order in clientData->mediaJobList */
		if ((mediaJobPtr = (OP_MEDIA_JOB_LIST *) malloc
		((unsigned) sizeof (OP_MEDIA_JOB_LIST))) == (OP_MEDIA_JOB_LIST *) NULL)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error: memory allocation failed.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			/* rollback all tape item status and free tapeItemList */
			if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
			{
				rollback_tapeItemsStatus (tapeItemList);
				free_tapeItemList (&tapeItemList);
			}

			return;
		}
		else
		{
			/* initialize the structure */
			mediaJobPtr->order_id = tapeItemPtr->order_id;
			mediaJobPtr->mediaBatchList = NULL;
			mediaJobPtr->totalCount = 0;
			mediaJobPtr->startedCount = 0;
			mediaJobPtr->completedCount = 0;
			mediaJobPtr->next = NULL;
			mediaJobPtr->prev = NULL;

			if (clientData->mediaJobList == (OP_MEDIA_JOB_LIST *)NULL)
			{
				/* first entry in the mediaJobList */
				clientData->mediaJobList = mediaJobPtr;
			}
			else
			{
				sPtr = tPtr = clientData->mediaJobList; 
				while (tPtr != (OP_MEDIA_JOB_LIST *)NULL)
				{
					sPtr = tPtr;
					tPtr = tPtr->next;
				}
				/* add new entry */
				sPtr->next = mediaJobPtr;
				mediaJobPtr->prev = sPtr;
			}

			/* assign jobPtr pointing to the media job */
			jobPtr = mediaJobPtr;

		}
	} /* if (!found) */


	/*
	** Construct media batches from tapeItemList
	*/

	/* loop through all media specs */
	for (i = 0; i < mediaSpecCount; i++)
	{
		/* Get the media capacity only if media type is not DISK */
		if (mediaSpec[i].media_type != IMS_DISK)
		{
			sprintf (query, "select capacity from media_policy "
											"where media_type = %d", mediaSpec[i].media_type);

			catReq->item[0] = (char *)query;
			catReq->item[1] = (int *)&capacity;

			if ((status = ims_opCat (catReq, OP_GETMEDIACAPACITY)) < IMS_OK)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error: OP_GETMEDIACAPACITY failed.");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

				/* rollback all tape item status and free tapeItemList */
				if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
				{
					rollback_tapeItemsStatus (tapeItemList);
					free_tapeItemList (&tapeItemList);
				}

				return;
			}

			if (capacity <= 0)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error: Invalid Media Capacity. Contact DBA.");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

				/* rollback all tape item status and free tapeItemList */
				if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
				{
					rollback_tapeItemsStatus (tapeItemList);
					free_tapeItemList (&tapeItemList);
				}

				return;
			}

			/* Convert capacity from megaBytes to kbytes */
			new_capacity = (double)capacity * 1000;

		}

		do
		{
			/* initialize */
			currDataBytes = 0;
			mediaItemCount = 0;
			moreToProcess = False;		
			tapeItemPtr = tapeItemList;

			/*
			** Create mediaItemArray for the batch, 
			** the maximum number of items for a batch is 100
			*/
		 	while ((tapeItemPtr != (OP_TAPE_ITEM_LIST *)NULL) &&
						 (mediaItemCount < MAX_TAPE_ITEMS)) 
			{
				if ((tapeItemPtr->status == ITEM_LOCKED) && 
						(tapeItemPtr->media_type == mediaSpec[i].media_type) && 
 					  (tapeItemPtr->media_fmt_type == mediaSpec[i].media_fmt_type))
				{
					/* Handle media type DISK separately from tapes */
					if (tapeItemPtr->media_type == IMS_DISK)
					{
						mediaItemArray[mediaItemCount].item_id = tapeItemPtr->item_id;
						mediaItemArray[mediaItemCount].status = tapeItemPtr->status;

						/* change tapeItemPtr status to 99 to indicate this item 
						** has been added to mediaItemArray.
						*/
						tapeItemPtr->status = 99;

						/* increment mediaItemCount */
						mediaItemCount += 1;

						/*
						** The maximum no of items per batch is 100, if 
						** we reach the limit, start another batch.
						*/
						if (mediaItemCount == MAX_TAPE_ITEMS)
							moreToProcess = True;

						tapeItemPtr = tapeItemPtr->next;

					}
					else
					{
						/*
						** if item size exceeds the capacity, update item
						** status to 13 ERROR, output an error message to user 
						*/
						if (tapeItemPtr->data_kbytes > new_capacity)
						{
							tapeItemPtr->status = ITEM_ERROR;

							sprintf(Msg, "Error: Order Id: %d, Item: %d, size of item "   
													 "exceeds the capacity of media.\n",
													 tapeItemPtr->order_id, tapeItemPtr->item_id);
							msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

							if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
							{
								/* Display error messages */
								sprintf(Msg, "Internal Error:\nProcess Media,\n"
														 "OP_BEGINTRANSACTION failed for "
														 "Order: %d, Item: %d\n", 
														 tapeItemPtr->order_id, tapeItemPtr->item_id);   
								msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
								/* rollback all tape item status and free tapeItemList */
								if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
								{
									rollback_tapeItemsStatus (tapeItemList);
									free_tapeItemList (&tapeItemList);
								}
	
								return; 
							}

							if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
							{
								/* rollback transaction */
								ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
								/* Display error messages */
								sprintf(Msg, "Internal Error:\nProcess Media,\n"
														 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
														 tapeItemPtr->order_id, tapeItemPtr->item_id);   
								msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
				
								/* rollback all tape item status and free tapeItemList */
								if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
								{
									rollback_tapeItemsStatus (tapeItemList);
									free_tapeItemList (&tapeItemList);
								}

								return;
							}

							/* call op_update_item_status to set item status to ERROR */
							catReq->item[0] = (DBINT *)&tapeItemPtr->order_id;
							catReq->item[1] = (DBSMALLINT *)&tapeItemPtr->item_id;
							catReq->item[2] = (DBSMALLINT *)&tapeItemPtr->status;

							if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
							{
								/* rollback transaction */
								ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

								/* Display error messages */
								sprintf (Msg,"Internal Error: Update Order Item Status: "
														 "OP_UPDATEITEMSTATUS failed for \n"
													 	 "Order: %d, Item: %d\n.", 
												 		 tapeItemPtr->order_id, tapeItemPtr->item_id);   
								msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

								/* rollback all tape item status and free tapeItemList */
								if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
								{
									rollback_tapeItemsStatus (tapeItemList);
									free_tapeItemList (&tapeItemList);
								}
		
								return; 
							}
	
							if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
							{
								/* rollback transaction */
								ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
								/* Display error messages */
								sprintf(Msg, "Internal Error:\nProcess Media,\n"
														 "OP_COMMITTRANSACTION failed for "
														 "Order: %d, Item: %d\n", 
														 tapeItemPtr->order_id, tapeItemPtr->item_id);   
								msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

								/* rollback all tape item status and free tapeItemList */
								if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
								{
									rollback_tapeItemsStatus (tapeItemList);
									free_tapeItemList (&tapeItemList);
								}

								return;
							}

							tapeItemPtr = tapeItemPtr->next;
							continue;

						}

						if (currDataBytes + tapeItemPtr->data_kbytes <= new_capacity)
						{
							/* this item fits on the tape, add it to mediaItemArray */ 
							currDataBytes += tapeItemPtr->data_kbytes;
	
							mediaItemArray[mediaItemCount].item_id = tapeItemPtr->item_id;
							mediaItemArray[mediaItemCount].status = tapeItemPtr->status;

							/* change tapeItemPtr status to 99 to indicate this item 
							** has been added to mediaItemArray.
							*/
							tapeItemPtr->status = 99;

							/* increment mediaItemCount */
							mediaItemCount += 1;

							/*
							** The maximum no of items per batch is 100, if 
							** we reach the limit, start another batch.
							*/
							if (mediaItemCount == MAX_TAPE_ITEMS)
								moreToProcess = True;

							tapeItemPtr = tapeItemPtr->next;

						}
						else
						{
							/* this item does not fit on the tape, process later */ 
							moreToProcess = True;

							tapeItemPtr = tapeItemPtr->next;
							continue;
						}

					} /* if (media_type == IMS_DISK ) */

				} /* if status == ITEM_LOCKED */
				else
				{
					/* tape item has status other than ITEM_LOCKED */
					tapeItemPtr = tapeItemPtr->next;
				}

			} /* while (tapeItemList != NULL) */ 


			/* if mediaItemCount is greater than 0, create a batch */
			if (mediaItemCount > 0)
			{
				/* allocate a new batch */
				if ((newBatchPtr = (OP_MEDIA_BATCH_LIST *) malloc
						((unsigned) sizeof (OP_MEDIA_BATCH_LIST)))
						== (OP_MEDIA_BATCH_LIST *) NULL)
				{
					/* Display error messages */
					sprintf(Msg, "Internal Error: memory allocation failed.");   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					/* rollback all tape item status and free tapeItemList */
					if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}

					return;
				}

				/* Each batch has its own message descriptor */
				/* allocate space for msgDesc structure */
				if ((batchMsgDesc = ims_msgStructAlloc()) == (IMS_MSG_STRUCT *)NULL)
				{
					sprintf(Msg, "Internal Error: memory allocation failed.");   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					/* rollback all tape item status and free tapeItemList */
					if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
					{
						rollback_tapeItemsStatus (tapeItemList);
						free_tapeItemList (&tapeItemList);
					}
	
					/* free newBatchPtr that was allocated */
					if (newBatchPtr != (OP_MEDIA_BATCH_LIST *)NULL)
					{
						free (newBatchPtr);
					}

					return;
				}
				else
				{
					/*
					** initialize message descriptor error handler flags 
					** turn off all sybase error handling flags 
					*/
					ims_msgStderrFlag (batchMsgDesc, IMS_OFF);
					ims_msgQueueFlag (batchMsgDesc, IMS_ON);
					ims_msgSybErrHndlFlag (batchMsgDesc, IMS_OFF);
					ims_msgSybMsgHndlFlag (batchMsgDesc, IMS_OFF);
				}

				/* get new batch id */
				bPtr = jobPtr->mediaBatchList;

				if (bPtr == (OP_MEDIA_BATCH_LIST *)NULL)
				{
					/* first batch in the mediaBatchList */
					newBatchPtr->batch_id = 1;
					newBatchPtr->media_type = mediaSpec[i].media_type;
					newBatchPtr->media_fmt_type = mediaSpec[i].media_fmt_type;
					newBatchPtr->status = NEW;
					newBatchPtr->msgDesc = batchMsgDesc;
					newBatchPtr->deviceInfo = NULL;
					newBatchPtr->prev = NULL;
					newBatchPtr->next = NULL;
					newBatchPtr->mediaId[0] = '\0';
					jobPtr->mediaBatchList = newBatchPtr;
				}
				else
				{
					b2Ptr = bPtr;
					while (b2Ptr != (OP_MEDIA_BATCH_LIST *)NULL)
					{
						bPtr = b2Ptr;
						b2Ptr = b2Ptr->next;
					}
					newBatchPtr->batch_id = bPtr->batch_id + 1;
					newBatchPtr->media_type = mediaSpec[i].media_type;
					newBatchPtr->media_fmt_type = mediaSpec[i].media_fmt_type;
					newBatchPtr->status = NEW;
					newBatchPtr->msgDesc = batchMsgDesc;
					newBatchPtr->deviceInfo = NULL;
					bPtr->next = newBatchPtr;
					newBatchPtr->prev = bPtr;
					newBatchPtr->next = NULL;
					newBatchPtr->mediaId[0] = '\0';
				}

				/* assign batch mediaItemArray */
 				newBatchPtr->no_items = mediaItemCount;
				for (j = 0; j < mediaItemCount; j++)
				{
					newBatchPtr->mediaItemArray[j] = mediaItemArray[j];
				}

				/* increment mediaJob total batch count by 1 */
				jobPtr->totalCount += 1;

			} /* if (mediaItemCount > 0) */
	
		} while (moreToProcess); /* while moreToProcess */

	} /* for all media_types */


	/*
	** if the media job has no batch formed, free this mediaJob 
	** and remove it from clientData->mediaJobList
	*/

	if ((jobPtr->mediaBatchList == (OP_MEDIA_BATCH_LIST *)NULL) && 
			(jobPtr->totalCount == 0))
	{
		(void) free_mediaJob(jobPtr->order_id);
	}
	else
	{
		/* Complete creating media batches from the tapeItemList */ 
		/* pop up mediaStatus Screen */
		if ((status = display_mediaStatusScreen()) < IMS_OK)
		{
			/* rollback all tape item status and free tapeItemList */
			if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
			{
				rollback_tapeItemsStatus (tapeItemList);
				free_tapeItemList (&tapeItemList);
			}

			/* 
			** 07/09/96 - this is to correct PR 980.
			** If the media screen is not popped up yet, let's free
			** the media job entry.
			*/
			if (!glbData.mediaFlag)
			{
				(void) free_mediaJob(jobPtr->order_id);
			}

			return;
		}
		else
		{
			dpy = XtDisplay (glbData.mediaW);
			XFlush (dpy);
			XmUpdateDisplay (glbData.mediaW);

			while (XtAppPending(UxAppContext))
			{
				XtAppProcessEvent(UxAppContext, XtIMAll);
			}
		}

		/* call order_processMediaBatch() to start batch processing */
		/* Register the event with Xt to make sure no interruption occurs */
		(void) XtAppAddTimeOut
			(UxAppContext, 0, (XtTimerCallbackProc)order_processMediaBatch, NULL);
	}

	/* free up tapeItemList */
	if (tapeItemList != (OP_TAPE_ITEM_LIST *) NULL)
	{
		free_tapeItemList (&tapeItemList);
	}

 }

	UxOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: order_processMediaBatch
**
** Description:		function for processing media batches
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void order_processMediaBatch()
{
	_UxCorder               *UxSaveCtx, *UxContext;

	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_CLIENT_DATA *clientData;
	char Msg[10240+1];
	/*char Msg[IMS_COL1024_LEN+1];*/
	char msgSTName[IMS_COL30_LEN+1];
	char msgSWName[IMS_COL30_LEN+1];
	char tapeItemStr[10240+1];
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char *tempPtr, *msgPtr;
	int len = 0;
	int i, lastScanFlag, notCompleteFlag;
	int status, jobCancelled, device_id;
	int tapeIsMounted, tapeIsRemoved;
	int tempcount;
	Widget label_w, msgST;
	XmTextPosition curpos;
	OP_MEDIA_JOB_LIST  *mediaJobPtr, *sPtr, *tPtr, *jobPtr;
	OP_MEDIA_BATCH_LIST *bPtr, *b2Ptr, *newBatchPtr, *batchPtr;
	DEVICE_INFO *deviceInfo, *devicePtr;
	DBINT order_id;
	int stopbatchproc; /* PR2492 */

	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =
			(_UxCorder *) UxGetContext( glbData.orderW );
{

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	lastScanFlag = 1; 
	notCompleteFlag = 0;
	deviceInfo = NULL;

	if (clientData->mediaJobList == NULL)
	{
		return;
	}

	/*
	** Go through clientData->mediaJobList and process
	** each mediaJob's mediaBatchList
	*/
	jobPtr = clientData->mediaJobList;
	stopbatchproc = False;
	while (jobPtr != (OP_MEDIA_JOB_LIST *)NULL) 
	{
		batchPtr = jobPtr->mediaBatchList;

		while ((batchPtr != (OP_MEDIA_BATCH_LIST *)NULL) && (stopbatchproc == False) ) /* PR# 2492 */
		{
			/* process the batch only if its status is NEW */
			if (batchPtr->status == NEW)
			{
				/* allocate space for DEVICE_INFO structure */
				if ((deviceInfo = (DEVICE_INFO *) malloc
						((unsigned) sizeof (DEVICE_INFO))) == (DEVICE_INFO *) NULL)
				{
					/* Display error messages */
					sprintf(Msg, "Internal Error: memory allocation failed.");   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					return;
				}

				/*
				** call ims_deviceAlloc() to allocate device for 
				** this batch media_type 
				*/

				/* block critical database access */
				sighold (SIGCLD);

				if ((status = ims_deviceAlloc (catReq->msgDesc,
											(char *)&catReq->userSpec, batchPtr->media_type, 
											jobPtr->order_id, deviceInfo))
										< IMS_OK)
				{
					/* release signal */
					sigrelse (SIGCLD);

					/* set notCompleteFlag */
					notCompleteFlag = 1;

					if (status == IMS_WARNING)
					{
						/* 
						** no device is available right now, try later. 
						** Proceed to the next batch. 	
						*/
						if (deviceInfo != (DEVICE_INFO *)NULL)
						{
							free(deviceInfo);
							deviceInfo = NULL;
						}

						batchPtr = batchPtr->next;
						continue;
					}
					else
					{
						sprintf(Msg, "Internal Error: ims_deviceAlloc failed.");   
						msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

						/*
						** internal error occured in ims_deviceAlloc function,
						** continue processing other batches, the last batch
						** will clean up. 
						*/
						if (deviceInfo != (DEVICE_INFO *)NULL)
						{
							free(deviceInfo);
							deviceInfo = NULL;
						}

						batchPtr = batchPtr->next;
						continue;
					}

				}
				else
				{
					/* 
					** update batch status to be PENDING 
					** before releasing SIGCLD to make sure other
					** process will not pick up this batch to do 
					** another device allocation.
					*/
					batchPtr->status = PENDING;

					/*
					** 02/07/96 - Increment the Job startedCount by 1
					** as soon as a device is allocated for this batch.
					*/

					/* increment media job startedCount by 1 */
					jobPtr->startedCount += 1;

					/* set lastScanFlag to 0 */
					lastScanFlag = 0;

					/* release signal */
					sigrelse (SIGCLD);

					/* assign deviceInfo structure to batchPtr */
					batchPtr->deviceInfo = deviceInfo;

					/*
					** update Media Status Screen, highlight the
					** allocated device label 
					*/
					label_w = glbData.deviceStats[deviceInfo->device_id].allocated;
					(void) order_clearDeviceLabel(label_w, deviceInfo->device_id);
					(void) order_blueDeviceLabel(label_w, deviceInfo->device_id);

					/*
					** 02/14/96 - FTP does not update device_state table with
					** order_id, so deviceInfo does not have order_id set, let's
					** use jobPtr->order_id.
					*/
					if (batchPtr->media_type == IMS_DISK)
					{
						sprintf (Msg, "Device %d : Allocated to process order %d\n",
									deviceInfo->device_id, jobPtr->order_id);
					}
					else
					{
						sprintf (Msg, "Device %d : Allocated to process order %d\n",
									deviceInfo->device_id, deviceInfo->order_id);
					}

					curpos = XmTextGetLastPosition
										(glbData.deviceStats[deviceInfo->device_id].msgText);
					XmTextInsert (glbData.deviceStats[deviceInfo->device_id].msgText,
												curpos, Msg);
					XmTextScroll(glbData.deviceStats[deviceInfo->device_id].msgText, 1);

					/*
					** 12/29/1995 - We call allocate device for all media type.
					** If media type is DISK, no need to ask user to mount tapes.
					** Call start_mediaJob to start FTP 
					*/
					if (batchPtr->media_type == IMS_DISK)
					{
						if ((status = start_mediaJob(batchPtr, jobPtr->order_id, 
								(char *)&catReq->userSpec, order_endMediaJobCb))
								< IMS_OK)
						{
							/* 
							** start_mediaJob failed, do the following:
							** set FTP label from Allocated to Available
							** set batch status back to NEW,
							** set notCompleteFlag to 1
							** free up deviceInfo allocated
							** the last scan will rollback batch item status
							*/ 
					
							/* Update Media Status Screen, set FTP state to Available */
							label_w = glbData.deviceStats[deviceInfo->device_id].available;
							(void) order_clearDeviceLabel(label_w, deviceInfo->device_id);
							(void) order_yellowDeviceLabel(label_w, deviceInfo->device_id);

							/* reset batch status to NEW */
							batchPtr->status = NEW;

							/* decrement media job startedCount by 1 */
							jobPtr->startedCount -= 1;

							/* reset lastScanFlag to 1 */
							lastScanFlag = 1;

							/* set notCompleteFlag */
							notCompleteFlag = 1;

							/* free up deviceInfo allocated */
							if (deviceInfo != (DEVICE_INFO *)NULL)
							{
								free(deviceInfo);
								deviceInfo = NULL;
							}

							/* reset batch deviceInfo to NULL */
							batchPtr->deviceInfo = (DEVICE_INFO *)NULL;

						}
						else
						{
							/* batch started successfully */
							batchPtr->status = STARTED;

							/*
							** 02/07/96 - jobPtr startedCount has already 
							** been incremented above
							*/

							/* increment media job startedCount by 1 */

							/*
						  ** jobPtr->startedCount += 1;
							*/

							/* set lastScanFlag to 0 */
							lastScanFlag = 0;

							/* update Media Status Screen, update label to Job Started */
							label_w = glbData.deviceStats[deviceInfo->device_id].jobStart;
							(void) order_clearDeviceLabel(label_w, deviceInfo->device_id);
							(void) order_blueDeviceLabel(label_w, deviceInfo->device_id);

							sprintf (Msg,
							 "Electronic File Transfer started for order %d, batch %d.\n",
							 jobPtr->order_id, batchPtr->batch_id);

							curpos = XmTextGetLastPosition
									(glbData.deviceStats[deviceInfo->device_id].msgText);
							XmTextInsert 
									(glbData.deviceStats[deviceInfo->device_id].msgText,
									 curpos, Msg);
							XmTextScroll
								(glbData.deviceStats[deviceInfo->device_id].msgText, 1);

						} /* if (start_mediaJob) */

						batchPtr = batchPtr->next;
						continue; 

					} /* if (media_type == DISK) */


					/*
					** call function AskUser to ask the user to mount a tape
					*/
					tempPtr = tapeItemStr;
					tempcount = 0;
					for (i = 0; (i < batchPtr->no_items) && (i < 100); i ++)
					{
						sprintf (tempPtr, " %d",  
														batchPtr->mediaItemArray[i].item_id);

						tempPtr += strlen(tempPtr);

						tempcount ++;
						if (tempcount == 10)
						{
							/* put a new line */
							strcpy (tempPtr, "\n");
							tempPtr += strlen(tempPtr);
							tempcount = 0;
						}

					}
					if ( i >= 100 )
							sprintf(tempPtr, " .... %d\n", batchPtr->mediaItemArray[batchPtr->no_items -1].item_id);

					tapeIsMounted = False;
					jobCancelled = False;

					while ((!tapeIsMounted) && (!jobCancelled))
					{
						sprintf(Msg, "Please mount %s tape in device %d,\n\n" 
										 "Order:  %d, Batch: %d\n\n" 
										 "Item Count: %d\n\n"
										 "Item Id:\n%s\n\n"
										 "Press OK after tape is mounted.\n\n"
										 "Press CANCEL to cancel batch processing.\n",
										 glbData.media_type[batchPtr->media_type-1].item_name,
										 deviceInfo->device_id, deviceInfo->order_id,
										 batchPtr->batch_id, batchPtr->no_items, tapeItemStr);

						if (askUser(glbData.mediaW, Msg) == IMS_OK)
						{
							/*
							** call ims_deviceTapeCheck() to see
							** if tape is in the device, if returns IMS_OK,
							** tape is mounted in the device.
							** Otherwise, keep asking user to mount the 
							** tape until either the tape is mounted or
							** user cancel the batch job processing.
							*/
							if ((status = ims_deviceTapeCheck(catReq->msgDesc,
														batchPtr->deviceInfo))
													== IMS_OK)
							{
								tapeIsMounted = True;
							}
						}
						else 
						{
							/*
							** user has decided to cancel the media processing
							** action for this batch, do the following:
							** call ims_deviceTapeCheck() to see whether 
							** the tape is mounted in the device, if it is,
							** call ims_deviceTapeEject to rewind & eject the tape,
							** if ims_deviceTapeEject() fails, pop AskUser dialog
							** to ask the user to remove the tape, 
							** free up the device allocated only if 
							** ims_deviceTapeCheck returns IMS_WARNING 
							** update the batch status to COMPLETED
							** set jobCancelled to True
							** call rollback_batchItemsStatus to roll back item status 
							** adjust jobPtr->completedCount and jobPtr->startedCount
							** clean up structures and free up memory allocated  
							*/

							tapeIsRemoved = False;
							if ((status = ims_deviceTapeCheck(catReq->msgDesc,
														batchPtr->deviceInfo))
													== IMS_WARNING)
							{
								tapeIsRemoved = True;
							}

							/*
							** ims_deviceTapeEject() takes a while, so change 
							** the cursor while waiting
							*/
							if (!tapeIsRemoved)
							{
								/* Change cursor to watch cursor */
								timeOutCursors (True);
							}

							while (!tapeIsRemoved)
							{
								/*	 
								** call ims_deviceTapeEject to rewind & eject the tape
								*/
								if ((status = ims_deviceTapeEject(catReq->msgDesc,
															batchPtr->deviceInfo)) < IMS_OK)
								{
									/* Change cursor back to normal */
									timeOutCursors (False);

									sprintf(Msg,
										"Internal Error: ims_deviceTapeEject failed.\n\n" 
							  		"Order: %d, Batch: %d is cancelled.\n\n" 
										"Please remove %s tape from device %d.\n\n" 
										"Press OK after tape is removed.\n",
									 	deviceInfo->order_id, batchPtr->batch_id,
										glbData.media_type[batchPtr->media_type-1].item_name,
									 	deviceInfo->device_id);

									/*
									** ims_deviceTapeEject() failed, pop up AskUser dialog to 
									** ask the user to manually eject and remove the tape.
									*/

									/* don't care whether the user press OK or CANCEL */
									if (askUser(glbData.mediaW, Msg) != 0)
									{
										if ((status = ims_deviceTapeCheck(catReq->msgDesc,
																	batchPtr->deviceInfo))
																== IMS_WARNING)
										{
											tapeIsRemoved = True;
										}
									}
								}
								else
								{
									/* Change cursor back to normal */
									timeOutCursors (False);

								  sprintf(Msg, "Order: %d, Batch: %d is cancelled.\n\n" 
										"The tape was rewound and ejected.\n\n"	
										"Please remove %s tape from device %d.", 
								 		deviceInfo->order_id, batchPtr->batch_id,
										glbData.media_type[batchPtr->media_type-1].item_name,
								 		deviceInfo->device_id);

									msgBoxDlg_popupCb (glbData.mediaW, IMS_INFO, Msg); 
									tapeIsRemoved = True;

								}
							}

							/*
							** This part is replaced by ims_deviceTapeEject() 
							** call ims_deviceTapeCheck to see if there is
							** a tape in the device.
							*/
				/*
							tapeIsRemoved = False;
							if ((status = ims_deviceTapeCheck(catReq->msgDesc,
														batchPtr->deviceInfo)
													== IMS_WARNING))
							{
								tapeIsRemoved = True;
							}

							while (!tapeIsRemoved)
							{
							  sprintf(Msg, "Order: %d, Batch: %d is cancelled.\n\n" 
												 "Please remove %s tape from device %d.\n\n" 
												 "Press OK after tape is removed.\n",
										 		 deviceInfo->order_id, batchPtr->batch_id,
												 glbData.media_type[batchPtr->media_type-1].item_name,
										 		 deviceInfo->device_id);


								* don't care whether the user press OK or CANCEL *
								if (askUser(glbData.mediaW, Msg) != 0)
								{
									if ((status = ims_deviceTapeCheck(catReq->msgDesc,
																batchPtr->deviceInfo)
															== IMS_WARNING))
									{
										tapeIsRemoved = True;
									}
								}
							}
				*/


							/*
							** tape is removed from the device, 
							** call ims_deviceFree to free device  
							** ims_deviceFree returns IMS_WARNING if the 
							** device is already available.
							*/

							/* block critical database access */
							sighold (SIGCLD);

							if ((status = ims_deviceFree (catReq->msgDesc, 
									(char *)&catReq->userSpec, deviceInfo->device_id))
									< IMS_WARNING)
							{
								/* release signal */
								sigrelse (SIGCLD);

								sprintf(Msg, "Internal Error: Free device %d failed.",
														 deviceInfo->device_id);   
								msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
							}
							else
							{
								/* release signal */
								sigrelse (SIGCLD);
	
								/* device Free successful, update Media Status Screen */
								label_w = glbData.deviceStats[deviceInfo->device_id].available;
								(void) order_clearDeviceLabel(label_w, deviceInfo->device_id);
								(void) order_yellowDeviceLabel(label_w, deviceInfo->device_id);
							}

							/* rollback item status for this batch */
							(void) rollback_batchItemsStatus (jobPtr->order_id, batchPtr);

							/* set batchPtr->status to COMPLETED */
							batchPtr->status = COMPLETED;

							/*
							** 02/07/96 - since we incremented job startedCount 
							** after device allocation is successful, let's 
							** decrement it now because the user has decided to 
							** cancel the batch processing.
							*/

							/* decrement media job startedCount by 1 */
							jobPtr->startedCount -= 1;

							/*
							** reset lastScanFlag, since we set it to 0 when
							** the device allocation is successful 
							*/
							lastScanFlag = 1;

							/*
							** we consider this batch as completed, so let's 
							** increment media job completedCount by 1 
							*/
							/*jobPtr->completedCount += 1; 2492 */

							jobPtr->completedCount = jobPtr->totalCount; /*#2492 */

							jobCancelled = True;

							stopbatchproc = True; /* PR2492 */
						  batchPtr = batchPtr->next;
		          while (batchPtr != (OP_MEDIA_BATCH_LIST *)NULL)
							{
							    /* rollback item status for this batch */
							    (void) rollback_batchItemsStatus (jobPtr->order_id, batchPtr);
    
							    /* set batchPtr->status to COMPLETED */
							    batchPtr->status = COMPLETED;

							    /* decrement media job startedCount by 1 */
							    jobPtr->startedCount -= 1;

						      batchPtr = batchPtr->next;
							}
									

						}

					} /* while (askUser to mount tape) */


					if (jobCancelled)
					{
						/*batchPtr = batchPtr->next;*/ /*2492*/
						continue;
					}
					else
					{
						if ((status = start_mediaJob(batchPtr, jobPtr->order_id, 
								(char *)&catReq->userSpec, order_endMediaJobCb))
								< IMS_OK)
						{
							/* 
							** start_mediaJob failed, do the following:
							** Call ims_deviceTapeEject to rewind and eject tape,
							** if ims_deviceTapeEject fails, pop up AskUser dialog
							** to ask user to remove the tape. 
							** Call ims_deviceFree to free the device allocated.
							** set batch status back to NEW,
							** and set notCompleteFlag to 1
							** the last scan will rollback batch item status
							*/ 

							tapeIsRemoved = False;
							if ((status = ims_deviceTapeCheck(catReq->msgDesc,
														batchPtr->deviceInfo))
													== IMS_WARNING)
							{
								tapeIsRemoved = True;
							}

							/*
							** ims_deviceTapeEject() takes a while, so change 
							** the cursor while waiting
							*/
							if (!tapeIsRemoved)
							{
								/* Change cursor to watch cursor */
								timeOutCursors (True);
							}

							while (!tapeIsRemoved)
							{
								/*	 
								** call ims_deviceTapeEject to rewind & eject the tape
								*/
								if ((status = ims_deviceTapeEject(catReq->msgDesc,
															batchPtr->deviceInfo)) < IMS_OK)
								{
									/* Change cursor back to normal */
									timeOutCursors (False);

									sprintf(Msg,
										"Internal Error: ims_deviceTapeEject failed.\n\n" 
							  		"Order: %d, Batch: %d is cancelled.\n\n" 
										"Please remove %s tape from device %d.\n\n" 
										"Press OK after tape is removed.\n",
									 	deviceInfo->order_id, batchPtr->batch_id,
										glbData.media_type[batchPtr->media_type-1].item_name,
									 	deviceInfo->device_id);

									/*
									** ims_deviceTapeEject() failed, pop up AskUser dialog to 
									** ask the user to manually eject and remove the tape.
									*/

									/* don't care whether the user press OK or CANCEL */

									if (askUser(glbData.mediaW, Msg) != 0)
									{
										if ((status = ims_deviceTapeCheck(catReq->msgDesc,
																	batchPtr->deviceInfo))
																== IMS_WARNING)
										{
											tapeIsRemoved = True;
										}
									}
								}
								else
								{
									/* Change cursor back to normal */
									timeOutCursors (False);

								  sprintf(Msg, "Order: %d, Batch: %d is cancelled.\n\n" 
										"The tape was rewound and ejected.\n\n"	
										"Please remove %s tape from device %d.", 
								 		deviceInfo->order_id, batchPtr->batch_id,
										glbData.media_type[batchPtr->media_type-1].item_name,
								 		deviceInfo->device_id);

									msgBoxDlg_popupCb (glbData.mediaW, IMS_INFO, Msg); 
									tapeIsRemoved = True;
								}

							}

							/*
							** call ims_deviceTapeCheck to see if there is
							** a tape in the device.
							** This part is replaced by ims_deviceTapeEject()
							*/
			/*
							tapeIsRemoved = False;
							if ((status = ims_deviceTapeCheck(catReq->msgDesc,
															batchPtr->deviceInfo)
													== IMS_WARNING))
							{
								tapeIsRemoved = True;
							}

							while (!tapeIsRemoved)
							{
							  sprintf(Msg, "Order: %d, Batch: %d is cancelled, " 
														 "please remove tape from device %d. " 
														 "Press OK after tape is removed.\n",
												 		 deviceInfo->order_id, batchPtr->batch_id,
												 		 deviceInfo->device_id);

								* don't care whether the user press OK or CANCEL *
								if (askUser(glbData.mediaW, Msg) != 0)
								{
									if ((status = ims_deviceTapeCheck(catReq->msgDesc,
																batchPtr->deviceInfo)
															== IMS_WARNING))
									{
										tapeIsRemoved = True;
									}
								}
							}
			*/

							/*
							** call ims_deviceFree to free device  
							** ims_deviceFree returns IMS_WARNING if the 
							** device is already available.
							*/

							/* block critical database access */
							sighold (SIGCLD);
	
							if ((status = ims_deviceFree (catReq->msgDesc, 
									(char *)&catReq->userSpec, deviceInfo->device_id))
									< IMS_WARNING)
							{
								/* release signal */
								sigrelse (SIGCLD);

								sprintf(Msg, "Internal Error: Free device %d failed.",
														 deviceInfo->device_id);   
								msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
							}
							else
							{
								/* release signal */
								sigrelse (SIGCLD);

								/* device Free successful, update Media Status Screen */
								label_w = glbData.deviceStats[deviceInfo->device_id].available;
								(void) order_clearDeviceLabel(label_w, deviceInfo->device_id);
								(void) order_yellowDeviceLabel(label_w, deviceInfo->device_id);
							}
						
							/* free up deviceInfo allocated */
							if (deviceInfo != (DEVICE_INFO *)NULL)
							{
								free(deviceInfo);
								deviceInfo = NULL;
							}

							/* reset batch deviceInfo to NULL */
							batchPtr->deviceInfo = (DEVICE_INFO *)NULL;

							/* reset batch status to NEW */
							batchPtr->status = NEW;

							/* decrement media job startedCount by 1 */
							jobPtr->startedCount -= 1;

							/* reset lastScanFlag to 1 */
							lastScanFlag = 1;

							/* set notCompleteFlag */
							notCompleteFlag = 1;

							batchPtr = batchPtr->next;
							continue;
						}
						else
						{
							/* batch started successfully */
							batchPtr->status = STARTED;

							/*
							** 02/07/96 - we increment the job startedCount
							** after a device is allocated successfully, no
							** need to increment it again.
							*/

							/*
							** jobPtr->startedCount += 1;
							*/

							/* set lastScanFlag to 0 */
							lastScanFlag = 0;

							/* update Media Status Screen, update label to Job Started */
							label_w = glbData.deviceStats[deviceInfo->device_id].jobStart;
							(void) order_clearDeviceLabel(label_w, deviceInfo->device_id);
							(void) order_blueDeviceLabel(label_w, deviceInfo->device_id);
							sprintf (Msg, "Device %d : Processing order %d\n",
											deviceInfo->device_id, deviceInfo->order_id);
							curpos = XmTextGetLastPosition
										(glbData.deviceStats[deviceInfo->device_id].msgText);
							XmTextInsert (glbData.deviceStats[deviceInfo->device_id].msgText,
												curpos, Msg);
							XmTextScroll(glbData.deviceStats[deviceInfo->device_id].msgText, 1);

						} /* if (start_mediaJob) */

					} /* if (jobCancelled) */

				} /* else deviceAlloc successful */

			} /* if batchPtr->status == NEW */

			batchPtr = batchPtr->next;

		} /* while (batchPtr) */

		/*
		** 02/07/96 - in case all batches of a job are cancelled,
		** that is, if all the batches for this job are considered
		** to be completed, let's free the job entry before we proceed
		** to the next job.
		*/
		if ((jobPtr->totalCount == jobPtr->completedCount) && (jobCancelled))
		{
			order_id = jobPtr->order_id;

			/* advance jobPtr here */
			jobPtr = jobPtr->next;
		
			(void) free_mediaJob(order_id);
			deviceInfo = NULL;

			continue;
		}

		jobPtr = jobPtr->next;

	} /* while (jobPtr) */


	/*
	** 1/24/96 - if this job is the only entry, we are done, return.
	** case where all batches of a job are cancelled.
	*/
	if (clientData->mediaJobList == NULL)
	{
		return;
	}


	/*
	** 02/08/96 - Just to be sure, let's walk through mediaJobList
	** quickly, if any job has startedCount > 0, then this is not
	** the last scan.
	*/
	lastScanFlag = 1;
	jobPtr = clientData->mediaJobList;
	while ((jobPtr != (OP_MEDIA_JOB_LIST *)NULL))
	{
		if (jobPtr->startedCount > 0)
		{
			lastScanFlag = 0;
		}
		jobPtr = jobPtr->next;
	}


	/*
	** To take care of jobs that cannot be started anymore 
	** Last Scan will roll back item status if necessary
	** then clean up the clientData->mediaJobList
	*/
	if (lastScanFlag && notCompleteFlag)
	{
		/* last scan of mediaJobList, there are jobs that cannot 
		** be started anymore, print out the order_id, batch_id,
		** items for the batch
		** rollback item status for each batch
		** should we free up clientData->mediaJobList?
		*/

		jobPtr = clientData->mediaJobList;
		while ((jobPtr != (OP_MEDIA_JOB_LIST *)NULL))
		{
			if (jobPtr->completedCount != jobPtr->totalCount)
			{
				batchPtr = jobPtr->mediaBatchList;
		
				while (batchPtr != (OP_MEDIA_BATCH_LIST *)NULL)
				{
					if ((batchPtr->status == NEW) ||
							(batchPtr->status == PENDING)) 
					{
						/* if status is NEW, device allocation failed */
						/* if status is PENDING, start_mediaJob failed */

						/* print out all the item ids of this batch */
						tempPtr = tapeItemStr;
						tempcount = 0;
						for (i = 0; i < batchPtr->no_items; i ++)
						{
							sprintf (tempPtr, " %d",  
											batchPtr->mediaItemArray[i].item_id);
							tempPtr += strlen(tempPtr);

							tempcount ++;
							if (tempcount == 10)
							{
								/* put a new line */
								strcpy (tempPtr, "\n");
								tempPtr += strlen(tempPtr);
								tempcount = 0;
							}

						}

						sprintf(Msg,
										"The following items could not be processed, "
										"please try again.\n\n"
									  "Order:  %d, Batch: %d\n\n" 
										"Items:\n%s\n\n",
								 		jobPtr->order_id,
								 		batchPtr->batch_id, tapeItemStr);

						if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up */
							OP_FREECHARPTR(concat_msg.Msg);
						}

						/* rollback item status for this batch */
						(void) rollback_batchItemsStatus (jobPtr->order_id, batchPtr);
					}

					batchPtr = batchPtr->next;
				}
			}

			jobPtr = jobPtr->next;

		} /* while jobPtr */


		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 512)
			{
				strcpy (label, "Media  Distribution  Message  Box");
				browseDlg_popupCb (glbData.mediaW, concat_msg.Msg, label);
			}
			else
			{
				msgBoxDlg_popupCb (glbData.mediaW, IMS_ERROR, concat_msg.Msg);
			}
		}

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);


		/* free up clientData->mediaJobList */
		jobPtr = clientData->mediaJobList;
		while (jobPtr != (OP_MEDIA_JOB_LIST *)NULL)
		{
			(void)free_mediaJob(jobPtr->order_id);
			jobPtr = clientData->mediaJobList;
		}
	} /* if (lastScanFlag && notCompleteFlag) */

}

UxOrderContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name:	order_addDeviceStatsWidget
**
** Description:		Function to add device status widgets to glb deviceStatus
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_addDeviceStatsWidget(
 Widget w,
 XtPointer statusType,
 XtPointer call)
{
  char *type;
	int deviceType, deviceId;

	type = (char *)statusType;

	deviceId = atoi (&type[1]);

  if (type[0] == 'v')
    glbData.deviceStats[deviceId].device = w;
  else if (type[0] == 'a')
    glbData.deviceStats[deviceId].available = w;
  else if (type[0] == 'l')
    glbData.deviceStats[deviceId].allocated = w;
  else if (type[0] == 's')
    glbData.deviceStats[deviceId].jobStart = w;
  else if (type[0] == 'q')
    glbData.deviceStats[deviceId].qualCheck = w;
  else if (type[0] == 'd')
    glbData.deviceStats[deviceId].jobDone = w;
  else if (type[0] == 'f')
    glbData.deviceStats[deviceId].jobFail = w;
  else if (type[0] == 'o')
    glbData.deviceStats[deviceId].offLine = w;
  else if (type[0] == 't')
    glbData.deviceStats[deviceId].msgText = w;
}


/*===========================================================================*
** 
** Function Name:	display_mediaStatusScreen
**
** Description:		Function to get status for each device and display screen
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

int display_mediaStatusScreen()
{
	_UxCorder               *UxSaveCtx, *UxContext;

	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	DEVICE_INFO *deviceInfo, *devicePtr;
	char Msg[IMS_COL1024_LEN+1];
	char msgSTName[IMS_COL30_LEN+1];
	char msgSWName[IMS_COL30_LEN+1];
	Widget label_w, msgST;
	XmTextPosition curpos;
	int status;

 UxSaveCtx = UxOrderContext;
 UxOrderContext = UxContext =
		(_UxCorder *) UxGetContext( glbData.orderW );
 {
	if (glbData.mediaFlag)
	{
		/* media screen is already pop up, return IMS_OK */
		return (IMS_OK);
	}

	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);

	/*
	** Find out statuses of all tape devices, update the labels, pop up
	** media status screen 
	*/

	/* allocate space for DEVICE_INFO structure */
	/* deviceInfoList is freed up when closing the Media Status Screen */
	/* see free_deviceInfoList() in ims_opMediaStatusCb.c */

	if ((clientData->deviceInfoList = (DEVICE_INFO *) malloc
			((unsigned) sizeof (DEVICE_INFO))) == (DEVICE_INFO *) NULL)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: memory allocation failed.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}

	if ((status = ims_deviceStatusList (catReq->msgDesc,
		(char *)&catReq->userSpec, 0, clientData->deviceInfoList))
		< IMS_OK)			
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: ims_deviceStatusList failed.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}

	devicePtr = clientData->deviceInfoList;
	while (devicePtr != (DEVICE_INFO *)NULL)
	{
		/*
		** If device database status conflicts with the real status,
		** set device status to be 2 to indicate that it is in use.
		*/
		if (devicePtr->status_conflict == IMS_TRUE)
		{
			devicePtr->status = DEVICE_INUSE;
		}

		switch (devicePtr->status)
		{
			case DEVICE_AVAILABLE: /* device is available */
				label_w = 
					glbData.deviceStats[devicePtr->device_id].available;

				(void) order_clearDeviceLabel(label_w, devicePtr->device_id);
				(void) order_yellowDeviceLabel(label_w, devicePtr->device_id); 

				sprintf (Msg, "Device %d : Available.\n",
								devicePtr->device_id);

				curpos = XmTextGetLastPosition
								 (glbData.deviceStats[devicePtr->device_id].msgText);
				XmTextInsert (glbData.deviceStats[devicePtr->device_id].msgText,
											curpos, Msg);
				XmTextScroll(glbData.deviceStats[devicePtr->device_id].msgText, 1);

			break;

			case DEVICE_INUSE: /* device is in-use */
				label_w = 
					glbData.deviceStats[devicePtr->device_id].allocated;

				(void) order_clearDeviceLabel(label_w, devicePtr->device_id);
				(void) order_blueDeviceLabel(label_w, devicePtr->device_id); 

				/*
				** 02/16/96 - if the order_id is -1, that means the device
				** is not allocated but has a tape in it.
				*/
				if (devicePtr->order_id == -1)
				{
					sprintf (Msg, "Device %d : Device is not currently in media "
												"processing but has a tape mounted.\n",
									devicePtr->device_id);
				}
				else
				{
					sprintf (Msg, "Device %d : Allocated to process order %d\n",
									devicePtr->device_id, devicePtr->order_id);
				}

				curpos = XmTextGetLastPosition
								 (glbData.deviceStats[devicePtr->device_id].msgText);
				XmTextInsert (glbData.deviceStats[devicePtr->device_id].msgText,
											curpos, Msg);
				XmTextScroll(glbData.deviceStats[devicePtr->device_id].msgText, 1);

			break;

			default : /* device is off-line or maintenance */
				label_w = 
				glbData.deviceStats[devicePtr->device_id].offLine;

				(void) order_clearDeviceLabel(label_w, devicePtr->device_id);
				(void) order_redDeviceLabel(label_w, devicePtr->device_id); 

				sprintf (Msg, "Device %d : Off-Line.\n",
								 devicePtr->device_id);
				curpos = XmTextGetLastPosition
								 (glbData.deviceStats[devicePtr->device_id].msgText);
				XmTextInsert (glbData.deviceStats[devicePtr->device_id].msgText,
											curpos, Msg);
				XmTextScroll(glbData.deviceStats[devicePtr->device_id].msgText, 1);

			break;

		}

		devicePtr = devicePtr->next;

	}

	/* Pop up the screen, flush the display, set glbData.mediaFlag */
	XtPopup (XtParent(glbData.mediaW), XtGrabNone);
	glbData.mediaFlag = 1;

 }
 UxOrderContext = UxSaveCtx;

 return (IMS_OK);

}

/*===========================================================================*
** 
** Function Name:	order_clearDeviceLabel
**
** Description:		Function to clear a device's label color.
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_clearDeviceLabel(
 Widget label_w,
 int device_id)
{
	Colormap cmap;
	XColor steel, exact;
	  
	if (label_w == NULL)
	{
		return;
	}

	XtVaGetValues (label_w, XmNcolormap, &cmap, NULL);
	XAllocNamedColor
			(XtDisplay(glbData.mediaW), cmap, "LightSteelBlue3", &steel, &exact); 
	XtVaSetValues (glbData.deviceStats[device_id].available,
									XmNbackground, steel.pixel, NULL);
	
	XtVaSetValues (glbData.deviceStats[device_id].allocated,
									XmNbackground, steel.pixel, NULL);

	XtVaSetValues (glbData.deviceStats[device_id].jobStart,
									XmNbackground, steel.pixel, NULL);

	XtVaSetValues (glbData.deviceStats[device_id].qualCheck,
									XmNbackground, steel.pixel, NULL);

	XtVaSetValues (glbData.deviceStats[device_id].jobDone,
									XmNbackground, steel.pixel, NULL);

	XtVaSetValues (glbData.deviceStats[device_id].jobFail,
									XmNbackground, steel.pixel, NULL);

	XtVaSetValues (glbData.deviceStats[device_id].offLine,
									XmNbackground, steel.pixel, NULL);

}


/*===========================================================================*
** 
** Function Name:	order_yellowDeviceLabel
**
** Description:		Function to change label color to yellow.
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_yellowDeviceLabel(
 Widget label_w,
 int device_id)
{
	Colormap cmap;
	XColor yellow, exact;
	Display *dpy;
	  
	if (label_w == NULL)
	{
		return;
	}

	XtVaGetValues (label_w, XmNcolormap, &cmap, NULL);
	XAllocNamedColor
			(XtDisplay(glbData.mediaW), cmap, "LightGoldenrod3", &yellow, &exact); 
	XtVaSetValues (label_w, XmNbackground, yellow.pixel, NULL);

	dpy = XtDisplay (glbData.mediaW);
	XFlush (dpy);
	XmUpdateDisplay (glbData.mediaW);
	
	while (XtAppPending(UxAppContext))
	{
		XtAppProcessEvent(UxAppContext, XtIMAll);
	}

}


/*===========================================================================*
** 
** Function Name:	order_blueDeviceLabel
**
** Description:		Function to change label color to blue.
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_blueDeviceLabel(
 Widget label_w,
 int device_id)
{
	Colormap cmap;
	XColor blue, exact;
	Display *dpy;
	  
	if (label_w == NULL)
	{
		return;
	}

	XtVaGetValues (label_w, XmNcolormap, &cmap, NULL);
	XAllocNamedColor
			(XtDisplay(glbData.mediaW), cmap, "#5050a0", &blue, &exact); 
	XtVaSetValues (label_w, XmNbackground, blue.pixel, NULL);

	dpy = XtDisplay (glbData.mediaW);
	XFlush (dpy);
	XmUpdateDisplay (glbData.mediaW);
	
	while (XtAppPending(UxAppContext))
	{
		XtAppProcessEvent(UxAppContext, XtIMAll);
	}

}


/*===========================================================================*
** 
** Function Name:	order_redDeviceLabel
**
** Description:		Function to change label color to red.
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_redDeviceLabel(
 Widget label_w,
 int device_id)
{
	Colormap cmap;
	XColor red, exact;
	Display *dpy;
	  
	if (label_w == NULL)
	{
		return;
	}

	XtVaGetValues (label_w, XmNcolormap, &cmap, NULL);
	XAllocNamedColor
			(XtDisplay(glbData.mediaW), cmap, "VioletRed4", &red, &exact); 
	XtVaSetValues (label_w, XmNbackground, red.pixel, NULL);

	dpy = XtDisplay (glbData.mediaW);
	XFlush (dpy);
	XmUpdateDisplay (glbData.mediaW);

	while (XtAppPending(UxAppContext))
	{
		XtAppProcessEvent(UxAppContext, XtIMAll);
	}

}


/*===========================================================================*
** 
** Function Name:	order_endMediaJobCb
**
** Description: This callback is activated when a child process terminates.
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_endMediaJobCb(
	int job_id,
	int order_id,
	int status,
	int shmid,
	char *data)
{
	OP_MEDIA_BATCH_DATA *mediaBatchData;
	char Msg[IMS_COL1024_LEN+1];

	if ((mediaBatchData = (OP_MEDIA_BATCH_DATA *) malloc
		((unsigned) sizeof (OP_MEDIA_BATCH_DATA))) == (OP_MEDIA_BATCH_DATA *) NULL)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: memory allocation failed.");   
		msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
		return;
	}

	/* data passed from ims_end_job */
	mediaBatchData->job_id = job_id;
	mediaBatchData->order_id = order_id;
	mediaBatchData->status = status;
	mediaBatchData->shmid = shmid;
	mediaBatchData->data = data;

	/* Add callback order_mediaStatusUpdateCb to X Event Queue */
	(void) XtAppAddTimeOut (UxAppContext, 10,
													(XtTimerCallbackProc)order_mediaStatusUpdateCb, 
													(void *)mediaBatchData);


}

/*===========================================================================*
** 
** Function Name:	order_mediaStatusUpdateCb
**
** Description:   This callback handles the real work when a media batch
**								processing is completed.
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void order_mediaStatusUpdateCb(
		void *mediaData)

{
	char Msg[IMS_COL1024_LEN+1];
	Display *dpy;
	Widget label_w;
	Colormap cmap;
	XColor blue, red, gray2, exact;
	XmTextPosition curpos;
	char *ptr, *ptr2;
	int i, status, device_id;
  OP_MEDIA_BATCH_DATA	*mediaBatchData; 
	OP_CLIENT_DATA *clientData;
	OP_MEDIA_JOB_LIST  *jobPtr;
	OP_MEDIA_BATCH_LIST  *batchPtr;
	IMS_MEDIA_PARAMS *mediaParams;
	OP_CAT_STRUCT *catReq;
	IMS_MSG_QUEUE *tempMsgQueue;
	char msgSTName[IMS_COL30_LEN+1];
	char msgSWName[IMS_COL30_LEN+1];
	Widget msgST;
	int tapeIsRemoved;


	mediaBatchData = (OP_MEDIA_BATCH_DATA *)mediaData;
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	jobPtr = clientData->mediaJobList;

	/* locate the job in the mediaJobList */
	while ((jobPtr != (OP_MEDIA_JOB_LIST *)NULL) &&
				 (jobPtr->order_id != mediaBatchData->order_id))
	{
			jobPtr = jobPtr->next;
	}

	if (jobPtr == (OP_MEDIA_JOB_LIST *)NULL)
	{
		/* Display error messages */
		sprintf (Msg, "Internal Error: Could not locate job for order: %d",
								 mediaBatchData->order_id);
		msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
		return;
	}

	/* found the job, now locate the batch */
	batchPtr = jobPtr->mediaBatchList;
	while ((batchPtr != (OP_MEDIA_BATCH_LIST *)NULL) &&
				 (batchPtr->job_id != mediaBatchData->job_id))
	{
			batchPtr = batchPtr->next;
	}	

	if (batchPtr == (OP_MEDIA_BATCH_LIST *)NULL)
	{
		/* Display error messages */
		sprintf (Msg, "Internal Error: Could not locate batch for job %d",
								 mediaBatchData->job_id);
		msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
		return;
	}

	/* update batch status to be COMPLETED */
	batchPtr->status = COMPLETED;

	/*
	** Retrieve data from shared memory region
	** Shared memory calls 
	** exclusive lock of shared memory region
	*/
	ptr = (void *) ims_shm_lock(mediaBatchData->shmid);

	mediaParams = (void *)ptr;

	/*
	** 12/29/95 - We are now allocating device for all media types 
	** including IMS_DISK, no need to set device_id to 0
	*/
	device_id = mediaParams->deviceInfo.device_id;
	

	/* assign mediaParams->msgDesc to batchPtr */
	batchPtr->msgDesc = mediaParams->msgDesc;


	/*
	** 12/27/95 - get mediaId only if the media job has 
	**						status IMS_JOB_COMPLETE
	*/
	if (mediaBatchData->status == IMS_JOB_COMPLETE)
	{
		strcpy (batchPtr->mediaId, mediaParams->mediaId);
	}


	/*
	** get status for each item, update the orderPtr->itemList status and
	** update the order_item table status 
	*/

	for (i = 0; i < batchPtr->no_items; i++)
	{
		if (mediaParams->mediaItemArray[i].item_id ==
										 batchPtr->mediaItemArray[i].item_id)
		{
			batchPtr->mediaItemArray[i].status =
										mediaParams->mediaItemArray[i].status;
		}
	}

	/* unlock shared memory region */
	(void) ims_shm_unlock(mediaBatchData->shmid, ptr);

	/* remove shared memory id */
	(void) ims_shm_remove(mediaBatchData->shmid);


	/* 
	** 12/15/95 - For R1b Prime, only FTP jobs are updating 
	** status for each item. CEOS and TAR jobs are not returning
	** status even if the item failed the media distribution.
	**
	** (1) If job status is IMS_JOB_COMPLETE, update each item
	** status to ITEM_GENERATED. Update mediaId for each item.
	** (2) If job status is IMS_JOB_ABORTED, if the job is FTP
	** job, the item that failed would have status ITEM_ERROR,
	** the rest would have status ITEM_LOCKED.  Update the ones
	** with status ITEM_LOCKED to be ITEM_IN_MEDIA.
	** (3) If job status is IMS_JOB_ABORTED and the job is either
	** CEOS or TAR, since CEOS and TAR are not returning individual
	** item status, we update each item status back to ITEM_IN_MEDIA.
	** 
	** go through each item in the mediaItemArray, update status 
	*/

	i = 0;
	while ( i < batchPtr->no_items )
	{
		/*
		** Begin the update transaction 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Order Item Status: "
									 "OP_BEGINTRANSACTION failed.\n");
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

			i++;
			continue;
		}

		/*
		** Lock the order_item table 
		*/
		if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Get Order Item Lock failed: "
										"OP_GETITEMLOCK failed.\n");
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

			i++;
			continue;
		}

		/*
		** Update the status in order_item table 
		*/
		if (mediaBatchData->status == IMS_JOB_COMPLETE)
		{
			/* media job was successful, update each item status to GENERATED */
			batchPtr->mediaItemArray[i].status = ITEM_GENERATED;
		}
		else
		{
			/* media job failed. */
			if (batchPtr->mediaItemArray[i].status == ITEM_LOCKED)
			{
				/* Item did not get processed, update status to IN-MEDIA */
				batchPtr->mediaItemArray[i].status = ITEM_IN_MEDIA;
			}
		}

		catReq->item[0] = (DBINT *)&mediaBatchData->order_id;
		catReq->item[1] = (DBSMALLINT *)&batchPtr->mediaItemArray[i].item_id;
		catReq->item[2] = (DBSMALLINT *)&batchPtr->mediaItemArray[i].status;
		if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Order Item Status: "
									 "OP_UPDATEITEMSTATUS failed.\n");
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

			i++;
			continue;
		}

		/*
		** If the item was successfully distributed to the media,
		** update the media id in order_item table 
		*/
		if (batchPtr->mediaItemArray[i].status == ITEM_GENERATED)
		{
			catReq->item[0] = (DBINT *)&mediaBatchData->order_id;
			catReq->item[1] = (DBSMALLINT *)&batchPtr->mediaItemArray[i].item_id;
			catReq->item[2] = (DBCHAR *)batchPtr->mediaId;

			if ((status = ims_opCat (catReq, OP_UPDATEITEMMEDIAID)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf (Msg, "Internal Error: Update Order Item Media ID: "
										 "OP_UPDATEITEMMEDIAID failed for order %d, item %d.",
						 mediaBatchData->order_id, batchPtr->mediaItemArray[i].item_id);

				msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
	
				i++;
				continue;
			}
		}


		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Order Item Status: "
									 "OP_COMMITTRANSACTION failed. \n");
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

			i++;
			continue;
		}

		/* proceed to the next item update */
		i++;
	}


	/* 
	** if mediaBatchData->status is IMS_JOB_ABORTED, 
	** call ims_msgQueueExtract () to extract error messages
	** display messages in the text window 
	*/

	if (mediaBatchData->status == IMS_JOB_ABORTED) 
	{
		/* update label to show that job failed */
		label_w = glbData.deviceStats[device_id].jobFail;
		(void) order_clearDeviceLabel(label_w, device_id);
		(void) order_redDeviceLabel(label_w, device_id);

		/* 
		** Media job failed, output an error message to Media Status Screen
		** device message window. 12/29/95 - This is done in case there
		** is no error message from message Queue.
		*/
		sprintf (Msg, "Order: %d, media batch job %d failed.\n",
								jobPtr->order_id, batchPtr->job_id);

		curpos = XmTextGetLastPosition
						(glbData.deviceStats[device_id].msgText);
		XmTextInsert (glbData.deviceStats[device_id].msgText,
									curpos, Msg);
		XmTextScroll(glbData.deviceStats[device_id].msgText, 1);


		/* If job status is IMS_JOB_ABORTED, extract messages and display */
		while ((tempMsgQueue = ims_msgQueueExtract(batchPtr->msgDesc)) != NULL)
		{
			/* write to log file */
			ims_msg (catReq->msgDesc, IMS_FATAL, tempMsgQueue->rawMsg);

			/* write to Media Status Screen message window */
			sprintf (Msg, "Order: %d, media batch job %d failed: %s\n",
									jobPtr->order_id, batchPtr->job_id, tempMsgQueue->rawMsg);

			curpos = XmTextGetLastPosition
							(glbData.deviceStats[device_id].msgText);
			XmTextInsert (glbData.deviceStats[device_id].msgText,
										curpos, Msg);
			XmTextScroll(glbData.deviceStats[device_id].msgText, 1);

			(void)ims_msgQueueFree(tempMsgQueue);

		}

	}
	else
	{
		/*
		** Perform a quality check if applicable.
		** For R1B, Dan will call ims_qc in ims_mediaJob
		*/


		/**********************************************************************
		if (batchPtr->media_fmt_type == 2)
		{
			label_w = glbData.deviceStats[device_id].qualCheck;
			(void) order_clearDeviceLabel(label_w, device_id);
			(void) order_blueDeviceLabel(label_w, device_id);

			*** block critical database access ***
			sighold (SIGCLD);

			if ((status = ims_qc (catReq->msgDesc, (char *)&catReq->userSpec,
													  FULL_REPORT, batchPtr->deviceInfo, IMS_FALSE))
									< IMS_OK)
			{
				*** release signal ***
				sigrelse (SIGCLD);

				sprintf(Msg, "Quality check failed.");   
				msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
			}
			else
			{
				*** release signal ***
				sigrelse (SIGCLD);
			}
		}
		***********************************************************************/


		/* Update label to Job Done */
		label_w = glbData.deviceStats[device_id].jobDone;
		(void) order_clearDeviceLabel(label_w, device_id);
		(void) order_blueDeviceLabel(label_w, device_id);

		/* if qualcheck is complete, job done */
		sprintf (Msg,
					"Device %d : Order %d, media batch job %d completed successfully.\n",
					device_id, jobPtr->order_id, batchPtr->job_id);

		curpos = XmTextGetLastPosition
						(glbData.deviceStats[device_id].msgText);
		XmTextInsert (glbData.deviceStats[device_id].msgText,
									curpos, Msg);
		XmTextScroll(glbData.deviceStats[device_id].msgText, 1);

	}


	/* 
	** 12/29/95 - We don't call ims_deviceFree for media type DISK.
	** The following operations does not apply to media_type DISK.
	** call ims_deviceTapeEject to rewind and eject the tape.
	** If ims_deviceTapeEject fails, do the following:
	** pop up askUser dialog box to ask user to remove the tape 
	** call ims_tapeCheck() to make sure the tape is removed 
	** before calling ims_deviceFree()
	*/
	if (batchPtr->media_type == IMS_DISK)
	{
		/* Update FTP device state to Available*/
		label_w = glbData.deviceStats[device_id].available;
		(void) order_clearDeviceLabel(label_w, device_id);
		(void) order_yellowDeviceLabel(label_w, device_id);
	}
	else
	{
		tapeIsRemoved = False;
		if ((status = ims_deviceTapeCheck(catReq->msgDesc, batchPtr->deviceInfo))
								== IMS_WARNING)
		{
			tapeIsRemoved = True;
		}

		/*
		** ims_deviceTapeEject() takes a while, so change 
		** the cursor while waiting
		*/
		if (!tapeIsRemoved)
		{
			/* Change cursor to watch cursor */
			timeOutCursors (True);
		}

		while (!tapeIsRemoved)
		{
			/*	 
			** call ims_deviceTapeEject to rewind & eject the tape
			*/
			if ((status = ims_deviceTapeEject(catReq->msgDesc,
										batchPtr->deviceInfo)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				sprintf(Msg,
								"Internal Error: ims_deviceTapeEject failed.\n\n" 
								"Media Processing for Order %d, Batch: %d is now complete.\n\n" 
								"Please remove %s tape from device %d. ", 
								jobPtr->order_id, batchPtr->batch_id,
								glbData.media_type[batchPtr->media_type-1].item_name,
 								device_id);

				/*
				** ims_deviceTapeEject() failed, pop up AskUser dialog to 
				** ask the user to manually eject and remove the tape.
				*/

				/* don't care whether the user press OK or CANCEL */
				if (askUser(glbData.mediaW, Msg) != 0)
				{
					if ((status = ims_deviceTapeCheck(catReq->msgDesc,
												batchPtr->deviceInfo))
												== IMS_WARNING)
					{
						tapeIsRemoved = True;
					}
				}
			}
			else
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				sprintf(Msg,
								"Device %d : The %s tape was rewound and ejected.  "  
								"Please remove the tape from device.\n", 
								device_id, 
								glbData.media_type[batchPtr->media_type-1].item_name);
	
				curpos = XmTextGetLastPosition
								(glbData.deviceStats[device_id].msgText);
				XmTextInsert (glbData.deviceStats[device_id].msgText,
											curpos, Msg);
/* do not scroll, so user can see the message 
				XmTextScroll(glbData.deviceStats[device_id].msgText, 1);
*/
	/* Message dialog box overlaps with askUser, later. */
	/*
		sprintf(Msg,
						"Media Processing for Order %d, Batch: %d is now complete.\n\n" 
						"The tape was rewound and ejected.\n\n"
						"Please remove %s tape from device %d. ", 
						jobPtr->order_id, batchPtr->batch_id,
						glbData.media_type[batchPtr->media_type-1].item_name,
 						device_id);
	
				msgBoxDlg_popupCb (glbData.mediaW, IMS_INFO, Msg); 
	*/
				tapeIsRemoved = True;
			}

		}


		/*
		** tape is removed from the device, 
		** call ims_deviceFree to free device  
		** ims_deviceFree returns IMS_WARNING if the 
		** device is already available.
		*/

		/* block critical database access */
		sighold (SIGCLD);

		if ((status = ims_deviceFree (catReq->msgDesc, 
				(char *)&catReq->userSpec, device_id))
				< IMS_WARNING)
		{
			/* release signal */
			sigrelse (SIGCLD);

			sprintf(Msg, "Internal Error: ims_deviceFree failed.");   
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
		}
		else
		{
			/* release signal */
			sigrelse (SIGCLD);

			/* device Free successful */
			label_w = glbData.deviceStats[device_id].available;
			(void) order_clearDeviceLabel(label_w, device_id);
			(void) order_yellowDeviceLabel(label_w, device_id);
		}

	} /* if (media_type == IMS_DISK) */

	/* decrement media job startedCount by 1 */
	jobPtr->startedCount -= 1;

	/* increment media job completedCount by 1 */
	jobPtr->completedCount += 1;

	/* if all the batches for this job are complete, free the job entry */
	if (jobPtr->totalCount == jobPtr->completedCount)
	{
		(void) free_mediaJob(jobPtr->order_id);
	}

	/* free mediaBatchData allocated in order_endMediaJobCb */
	if (mediaBatchData != NULL)
	{	
		free (mediaBatchData);
		mediaBatchData = NULL;
	}

	/* register X event to start another batch processing */
	(void) XtAppAddTimeOut
		(UxAppContext, 10, (XtTimerCallbackProc)order_processMediaBatch, NULL);

}



/*===========================================================================*
** 
** Function Name:	start_mediaJob
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

int start_mediaJob(
	OP_MEDIA_BATCH_LIST *batchPtr, 
	DBINT order_id,
	char *specPtr,
	void (*child_callback)() )
{
	char Msg[IMS_COL1024_LEN+1];
	int size, shmid, job_id;
	char *ptr, *ptr2;
	OP_CAT_USERSPEC *userSpec;
	IMS_MEDIA_PARAMS *mediaParams;
	IMS_JOB_USER_SPEC jobControlUserSpec;
	char fullpathName[IMS_PATH_LEN+1];
	char *execDir;
	XmTextPosition curpos;

#ifdef IMS_DEBUG
fprintf(stdout, "\nTEST start media job begin\n");
fflush(stdout);
#endif
	/*
	** Shared Memory calls
	*/

	/* get new share memory id */
	shmid = ims_shm_getid();

	if (shmid <= 0)
	{
		/* Display error messages */
		sprintf(Msg,
						"Internal Error: Could not get new shared memory id.");   
		msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

		return (IMS_ERROR);
	}
	else
	{
		userSpec = (OP_CAT_USERSPEC *)specPtr;

		/* get the size of memory needed for ims_mediaDist() call */
		size = sizeof(IMS_MEDIA_PARAMS);

		/* create the share memory region */
		ims_shm_create(shmid, size);

		/* exclusive lock of shared memory region */
		ptr = (void *) ims_shm_lock(shmid);

		mediaParams = (void *) ptr;

		/* populate shared memory region */
		mediaParams->msgDesc = batchPtr->msgDesc;

		memcpy ((void *)mediaParams->catSpec.username,
						(void *)userSpec->dbUserName, IMS_NAME_LEN+1);

		memcpy ((void *)mediaParams->catSpec.password,
						(void *)userSpec->dbPassword, IMS_NAME_LEN+1);

		memcpy ((void *)mediaParams->catSpec.program,
						(void *)userSpec->program, IMS_PROGRAM_LEN+1);

		if (userSpec->server == (char *)NULL)
		{
			mediaParams->catSpec.server[0] = '\0'; 
		}
		else
		{
			memcpy ((void *)mediaParams->catSpec.server,
							(void *)userSpec->server, IMS_NAME_LEN+1);
		}

		if (userSpec->dbName == (char *)NULL)
		{
			mediaParams->catSpec.database[0] = '\0';
		}
		else
		{
			memcpy ((void *)mediaParams->catSpec.database,
							(void *)userSpec->dbName, IMS_NAME_LEN+1);
		}

		if (batchPtr->deviceInfo == NULL)
		{
			memset (&mediaParams->deviceInfo, 0, sizeof(DEVICE_INFO));
		}
		else
		{
			memcpy ((void *)&mediaParams->deviceInfo, 
						(void *)batchPtr->deviceInfo, sizeof(DEVICE_INFO));
		}

		mediaParams->order_id = order_id;

		mediaParams->media_type = batchPtr->media_type;

		mediaParams->media_fmt_type = batchPtr->media_fmt_type;

		mediaParams->mediaId[0] = '\0';

		mediaParams->mediaItemCount = batchPtr->no_items;

#ifdef IMS_DEBUG
		fprintf(stdout, "\nTEST no_items = %d\n", batchPtr->no_items);
		fflush(stdout);
#endif

		memcpy ((void *)mediaParams->mediaItemArray,
						(void *)batchPtr->mediaItemArray, 
						 sizeof(batchPtr->mediaItemArray));

		/* unlock shared memory region */
		(void) ims_shm_unlock(shmid, ptr);

		/* populate jobControlUserSpec */
		memset(&jobControlUserSpec, 0, sizeof(jobControlUserSpec));
		strcpy (jobControlUserSpec.username, userSpec->dbUserName);
		strcpy (jobControlUserSpec.password, userSpec->dbPassword);
		strcpy (jobControlUserSpec.program, userSpec->program);

		if (userSpec->server == (char *)NULL)
		{
			jobControlUserSpec.server[0] = '\0';
		}
		else
		{
			strcpy (jobControlUserSpec.server, userSpec->server);
		}

		if (userSpec->dbName == (char *)NULL)
		{
			jobControlUserSpec.database[0] = '\0';
		}
		else
		{
			strcpy (jobControlUserSpec.database, userSpec->dbName);
		}


		/*
		** 1/29/96 - Instead of popping up a message dialog box, 
		** display error messages from ims_addJob and ims_startJob
		** to the message text window of the device
		** batchPtr->deviceInfo should not be NULL, but in case it
		** is, let's pop up the message dialog box.
		**
		** 07/19/96 - Increase the timeout from 2000 to 21600 secs (6 hrs).
		** 03/19/98 - Increase the timeout from 21600 to 43200 secs (12 hrs).
		*/

		/* add the job to job queue */
		if (ims_addJob (batchPtr->msgDesc, &jobControlUserSpec, order_id, -1,
										 &job_id, shmid, 43200, child_callback, NULL) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg,
							"Internal Error: ims_addJob failed.\n"
							"Cound not add order %d, batch %d to IMS media job queue.\n",   
							order_id, batchPtr->batch_id);

			if (batchPtr->deviceInfo == NULL)
			{
				msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
			}
			else
			{
				curpos = XmTextGetLastPosition
						(glbData.deviceStats[batchPtr->deviceInfo->device_id].msgText);
				XmTextInsert 
						(glbData.deviceStats[batchPtr->deviceInfo->device_id].msgText,
						 curpos, Msg);
				XmTextScroll
						(glbData.deviceStats[batchPtr->deviceInfo->device_id].msgText, 1);
			}

			return (IMS_ERROR);
		}

		/* assign job_id to the batch */
		batchPtr->job_id = job_id;

		/*
		** 1/3/1996 - Get the executable path for ims_mediaJob 
		*/

		execDir = getenv("IMS_EXEC_PATH");
		if (execDir == NULL)
		{
			execDir = ".";
		}

		ims_concatFilePath (fullpathName, execDir, "ims_mediaJob");

#ifdef IMS_DEBUG
fprintf(stdout, "\nTEST start media job\n");
fflush(stdout);
#endif

		if (ims_startJob(batchPtr->msgDesc, job_id, order_id, shmid, 
								 fullpathName) <  IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg,
							"Internal Error: ims_startJob failed.\n"
							"Could not start media job for order %d, batch %d, job %d\n", 
							order_id, batchPtr->batch_id, batchPtr->job_id);

			if (batchPtr->deviceInfo == NULL)
			{
				msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
			}
			else
			{
				curpos = XmTextGetLastPosition
						(glbData.deviceStats[batchPtr->deviceInfo->device_id].msgText);
				XmTextInsert 
						(glbData.deviceStats[batchPtr->deviceInfo->device_id].msgText,
							 curpos, Msg);
				XmTextScroll
						(glbData.deviceStats[batchPtr->deviceInfo->device_id].msgText, 1);
			}

			return (IMS_ERROR);
		}


		return (IMS_OK);

	}

}


/*===========================================================================*
** 
** Function Name:	free_mediaJob
**
** Description:		Function to free the media job and associated batchlist
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_mediaJob(
			DBINT order_id)
{
	OP_CLIENT_DATA *clientData;
	OP_MEDIA_JOB_LIST *jobPtr, *jPtr2, *jPtr3; 
	OP_MEDIA_BATCH_LIST *batchPtr, *batchNextPtr; 
	int found;

	clientData = &(glbData.orderClientData);

		/* free up the clientData media job list and associated batch lists */
	found = False;
	jobPtr = clientData->mediaJobList;

	while ((jobPtr != (OP_MEDIA_JOB_LIST *)NULL) && (!found))
	{
		if (jobPtr->order_id != order_id)
		{
			jobPtr = jobPtr->next;
		}
		else
		{
			found = True;

			/* free up the batch list associated with the job */
			batchPtr = jobPtr->mediaBatchList;
			while (batchPtr != (OP_MEDIA_BATCH_LIST *)NULL)
			{
				/* free up the message descriptor allocated */
				(void) ims_msgStructFree(batchPtr->msgDesc);

				/* free up device Info allocated */
				if (batchPtr->deviceInfo != (DEVICE_INFO *)NULL)
				{
					free(batchPtr->deviceInfo);
					batchPtr->deviceInfo = NULL;
				}

				batchNextPtr = batchPtr->next;
				free(batchPtr);
				batchPtr = batchNextPtr;

			} 

			jobPtr->mediaBatchList = (OP_MEDIA_BATCH_LIST *)NULL;

			/* remove this job entry from clientData->mediaJobList */
			jPtr2 = jobPtr->prev;
			jPtr3 = jobPtr->next;
			if ((jPtr2 != (OP_MEDIA_JOB_LIST *)NULL) &&
					(jPtr3 != (OP_MEDIA_JOB_LIST *)NULL))
			{
				jPtr2->next = jPtr3;
				jPtr3->prev = jPtr2;
				free (jobPtr);
			}
			else if ((jPtr2 == (OP_MEDIA_JOB_LIST *)NULL) &&
							 (jPtr3 == (OP_MEDIA_JOB_LIST *)NULL))
			{
				/* the only entry in the job list */
				free (jobPtr);
				jobPtr = (OP_MEDIA_JOB_LIST *)NULL;
				clientData->mediaJobList = (OP_MEDIA_JOB_LIST *) NULL;

			}
			else if ((jPtr2 != (OP_MEDIA_JOB_LIST *)NULL))
			{
				/* remove the last entry in the jobList */
				free (jobPtr);
				jobPtr = (OP_MEDIA_JOB_LIST *)NULL;
				jPtr2->next = jobPtr;
			}
			else
			{
				/* remove the first entry in the list */
				clientData->mediaJobList = jobPtr->next;
				jPtr3->prev = (OP_MEDIA_JOB_LIST *)NULL;
				free (jobPtr);
				jobPtr = (OP_MEDIA_JOB_LIST *)NULL;
			}

		}

	}

}


/*===========================================================================*
** 
** Function Name:	rollback_tapeItemsStatus
**
** Description:		Function to rollback item status
**
** Return Value: 	None
** 
** Revision History: 09/17/96 stored procedure rollback_item_status has
**                   been removed in new schema patch C.  Call 
**                   update_item_status to update the item status back
**                   to IN-MEDIA-Q directly.
**
**==========================================================================*/

void	rollback_tapeItemsStatus(
	OP_TAPE_ITEM_LIST *tapeItemList)

{
	OP_TAPE_ITEM_LIST *tPtr, *nextPtr;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	char Msg[IMS_COL1024_LEN+1];
	int status;

	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);

	tPtr = tapeItemList;
	while (tPtr != (OP_TAPE_ITEM_LIST *) NULL)
	{
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error:\nProcess Media,\n"
									 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
									 tPtr->order_id, tPtr->item_id);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
			return; 
		}

		if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			sprintf(Msg, "Internal Error:\nProcess Media,\n"
									 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
									 tPtr->order_id, tPtr->item_id);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			
			return;
		}

		/*
		** 09/17/96 - Changed to use OP_UPDATEITEMSTATUS 
		*/
		catReq->item[0] = (DBINT *)&tPtr->order_id;
		catReq->item[1] = (DBSMALLINT *)&tPtr->item_id;
		tPtr->status = ITEM_IN_MEDIA;
		catReq->item[2] = (DBSMALLINT *)&tPtr->status;
		if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS))
				< IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			sprintf (Msg, "Internal Error: Process Media: "
									 "OP_UPDATEITEMSTATUS failed for \n"
								 	 "Order: %d, Item: %d\n.", 
									 tPtr->order_id, tPtr->item_id);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
							
			return;
		}

		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error:\nProcess Media,\n"
									 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
									 tPtr->order_id, tPtr->item_id);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			return;
		}

		tPtr= tPtr->next;

	}

}


/*===========================================================================*
** 
** Function Name:	rollback_batchItemsStatus
**
** Description:		Function to rollback item status
**
** Return Value: 	None
** 
** Revision History: 09/17/96 - stored procedure rollback_item_status has
**                   been dropped in patch C.  Call update_item_status to
**                   update item status back to IN-MEDIA-Q directly.
**
**==========================================================================*/

void	rollback_batchItemsStatus(
	DBINT order_id,
	OP_MEDIA_BATCH_LIST *batchPtr)

{
	OP_MEDIA_ITEM_TYPE *mediaItemPtr;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	char Msg[IMS_COL1024_LEN+1];
	int i, status;
	DBSMALLINT new_item_status;

	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);

	/* roll back status for items in the batchPtr->mediaItemArray */
	for (i = 0; i < batchPtr->no_items; i++)
	{
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error:\nProcess Media,\n"
									 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
									 order_id, batchPtr->mediaItemArray[i].item_id);   
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
	
			return; 
		}

		if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error:\nProcess Media,\n"
									 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
									 order_id, batchPtr->mediaItemArray[i].item_id);   
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 
			
			return;
		}

		/*
		** 09/17/96 - Changed to use OP_UPDATEITEMSTATUS 
		*/
		catReq->item[0] = (DBINT *)&order_id;
		catReq->item[1] = (DBSMALLINT *)&batchPtr->mediaItemArray[i].item_id;
		batchPtr->mediaItemArray[i].status = ITEM_IN_MEDIA;
		catReq->item[2] = (DBSMALLINT *)&batchPtr->mediaItemArray[i].status;
		if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS))
				< IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
			sprintf (Msg, "Internal Error: Process Media: "
									 "OP_UPDATEITEMSTATUS failed for \n"
								 	 "Order: %d, Item: %d\n.", 
									 order_id, batchPtr->mediaItemArray[i].item_id);   
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

			return;
							
		}

		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Display error messages */
			sprintf(Msg, "Internal Error:\nProcess Media,\n"
									 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
									 order_id, batchPtr->mediaItemArray[i].item_id);   
			msgBoxDlg_popupCb (glbData.mediaW, IMS_FATAL, Msg); 

			return;
		}


	}

}


/*===========================================================================*
** 
** Function Name:	free_tapeItemList
**
** Description:		Function to free up tapeItemList
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_tapeItemList(
	OP_TAPE_ITEM_LIST **tapeItemList)

{
	OP_TAPE_ITEM_LIST *tPtr, *nextPtr;

	tPtr = *tapeItemList;
	while (tPtr != (OP_TAPE_ITEM_LIST *) NULL)
	{
		nextPtr = tPtr->next;
		free (tPtr);
		tPtr = nextPtr;
	}

	*tapeItemList = (OP_TAPE_ITEM_LIST *)NULL;

}


/*===========================================================================*
** 
** Function Name:	free_orderList
**
** Description:		Function to free the order list and associated item lists
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_orderList()
{
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr, *orderNextPtr; 
	OP_ORDER_ITEM_LIST *itemPtr, *itemNextPtr; 

	clientData = &(glbData.orderClientData);

	if (clientData->orderList != (OP_ORDER_LIST *)NULL)
	{
		/* free up the clientData order list and associated item lists */
		orderPtr = clientData->orderList;
		while (orderPtr != (OP_ORDER_LIST *)NULL)
		{
			/* free up the item list associated with the order */
			itemPtr = orderPtr->itemList;
			while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
			{
				itemNextPtr = itemPtr->next;
				free(itemPtr);
				itemPtr = itemNextPtr;
			} 
			orderPtr->itemList = (OP_ORDER_ITEM_LIST *)NULL;
	
			orderNextPtr = orderPtr->next;
			free(orderPtr);
			orderPtr = orderNextPtr;
		}

		clientData->orderCount = 0;
		clientData->orderList = (OP_ORDER_LIST *)NULL;
		clientData->currOrder = (OP_ORDER_LIST *)NULL;

	}

}


/*===========================================================================*
** 
** Function Name:	free_itemList
**
** Description:		Function to free the order item list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_itemList(
	OP_ORDER_LIST *orderPtr) 
{
	OP_ORDER_LIST  *orderNextPtr; 
	OP_ORDER_ITEM_LIST *itemPtr, *itemNextPtr; 


	/* free up the item list associated with orderPtr */
	itemPtr = orderPtr->itemList;

	while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
	{
		itemNextPtr = itemPtr->next;
		free(itemPtr);
		itemPtr = itemNextPtr;
	} 

	orderPtr->itemList = (OP_ORDER_ITEM_LIST *)NULL;

}


/*===========================================================================*
** 
** Function Name: order_create_shipmentCb
**
** Description:		callback function for shipping & billing menu item
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_create_shipmentCb( 
	Widget widget, 
	int fieldId,
	XtPointer cb) 
{
	_UxCorder               *UxSaveCtx, *UxContext;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_SHIP_ITEM_LIST *shipItemList, *shipItemPtr, *s1Ptr, *s2Ptr;
	OP_SHIPPING_DATA *shippingData;
	char Msg[IMS_COL1024_LEN+1];
	char label[IMS_COL128_LEN+1];
	char query[IMS_COL255_LEN+1];
	CONCAT_STR concat_msg = {0};
	DBINT order_id;
	int status, len, count, found;
	int ship_count = 0;

 UxSaveCtx = UxOrderContext;
 UxOrderContext = UxContext =
 		(_UxCorder *) UxGetContext( glbData.orderW );
{
	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	shipItemList = (OP_SHIP_ITEM_LIST *)NULL;
	shippingData = (OP_SHIPPING_DATA *)NULL;
	count = 0;

	if (glbData.shippingFlag)
	{
		/* Display error messages */
		strcpy (Msg, "Create New Shipment: Shipping screen is already popped up.\n"
								 "Please complete the current shipment.\n"); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
	
		return; 
	}


	/* Timeout cursor */
	timeOutCursors (True);

	/*
	** Begin Transaction
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy (Msg, "Internal Error: Create New Shipment\n"
								 "OP_BEGINTRANSACTION failed.\n"); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
		return; 
	}

	/*
	** Get General Lock
	*/
	if ((status = ims_opCat (catReq, OP_GETGENERALLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "Internal Error: Create New Shipment\n"
								 "OP_GETGENERALLOCK failed.\n"); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
		return;
	}

	/* 
	** Construct shipItemList
	** Order Level - fieldId = SHIP_ORDER 
	** Item Level  - fieldId = SHIP_ITEMS
	*/
	if (fieldId == SHIP_ORDER) 
	{
		/* Create New Shipment called from order level */
		/* Locate the selected order in orderList */

		orderPtr = clientData->orderList;
		while ((orderPtr != (OP_ORDER_LIST *)NULL) && (orderPtr->selectFlag == 0))
		{
			orderPtr = orderPtr->next;
		}

		if ((orderPtr == (OP_ORDER_LIST *)NULL))
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* No order is selected, display message, return */
			sprintf(Msg, "Create New Shipment: No order is selected.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}

		order_id = orderPtr->order_id;

		sprintf (query,
		"select t1.item_id, t1.p_granule_name, t1.cost, t1.process_type, "
		" t1.quantity, t1.media_type, t1.status "
		" from order_item t1 " 
		" where t1.order_id = %d"  
		" and t1.status in (10, 12)" 
		" and t1.shipped_p = 'N'"
		" and t1.shipping_id = NULL"
		" order by item_id",
		order_id);

		catReq->item[0] = (char *)query;
		catReq->item[1] = (int *)&count;
		catReq->item[2] = (OP_SHIP_ITEM_LIST *)shipItemList;
		if ((status = ims_opCat (catReq, OP_GETSHIPITEMS)) < IMS_OK)
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Create New Shipment:\n"
									 "Internal Error: OP_GETSHIPITEMS failed for order: %d\n",
									 order_id);
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			return;
		}

		/* assign count and shipItemList returned from query */
		count = *(int *)catReq->item[1];
		shipItemList = (OP_SHIP_ITEM_LIST *)catReq->item[2];

		if ((count == 0) || (shipItemList == (OP_SHIP_ITEM_LIST *)NULL))
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Create New Shipment:\n"
									 "Order %d, no item is ready to be shipped.\n",
										orderPtr->order_id);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 

			return;
		}
	}


	if (fieldId == SHIP_ITEMS)
	{
		/* Create New Shipment called from item level */
		itemPtr = clientData->currOrder->itemList;
		order_id = itemPtr->order_id;
		
		while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL) 
		{
			if (itemPtr->selectFlag)
			{
				/*
				** call OP_VERIFYITEMSHIPPINGSTATUS to verify that the 
				** item status is COMPLETE, item shipped_p is 'N'
				** and the item shipping_id is NULL
				** OP_VERIFYITEMSHIPPINGSTATUS returns count = 1 if the
				** item is available for shipping, if the item
				** is not available for shipping, count is 0
				*/
				catReq->item[0] = (DBINT *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (int *)&count;
				if ((status = ims_opCat(catReq, OP_VERIFYITEMSHIPPINGSTATUS)) < 0)
				{
					/* Display error messages */
					sprintf(Msg, "Create New Shipment:\nInternal Error: "
							 "OP_VERIFYITEMSHIPPINGSTATUS failed for Order: %d, Item: %d\n", 
							 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up */
						OP_FREECHARPTR(concat_msg.Msg);

						/*
						** Internal error, free up shipItemList
						** do not continue processing 
						*/
						if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
						{
							free_shipItemList (&shipItemList);
						}

						/* rollback transaction, release lock */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Change cursor back to normal */
						timeOutCursors (False);

						/* Display error messages */
						strcpy(Msg, "Create New Shipment:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

						return;
					}

					itemPtr = itemPtr->next;
					continue;

				}
				else
				{
					if (count > 0) 
					{
						/* This item is ready for shipping, add it to the shipItemList */

						if ((shipItemPtr = (OP_SHIP_ITEM_LIST *) malloc
								((unsigned) sizeof (OP_SHIP_ITEM_LIST))) ==
									(OP_SHIP_ITEM_LIST *) NULL)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Change cursor back to normal */
							timeOutCursors (False);

							/* memory allocation failed, clean up */
							OP_FREECHARPTR(concat_msg.Msg);

							/*
							** Internal error, free up shipItemList
							** do not continue processing 
							*/
							if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
							{
								free_shipItemList (&shipItemList);
							}

							/* Display error messages */
							strcpy(Msg, "Create New Shipment:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
							msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

							return;
						}

						/* add item into the shipItemList */
						shipItemPtr->item_id = itemPtr->item_id;
						shipItemPtr->process_type = itemPtr->process_type;
						(void) strcpy (shipItemPtr->name, itemPtr->p_granule_name);
						shipItemPtr->media_type = itemPtr->media_type;
						shipItemPtr->quantity = itemPtr->quantity;
						shipItemPtr->cost = itemPtr->cost;
						shipItemPtr->item_status  = itemPtr->status;
						shipItemPtr->next = NULL;

						if (shipItemList == (OP_SHIP_ITEM_LIST *)NULL)
						{
							/* first item in the list */
							shipItemList = shipItemPtr;
						}
						else
						{
							s1Ptr = s2Ptr = shipItemList; 
							while (s2Ptr != (OP_SHIP_ITEM_LIST *)NULL)
							{
								s1Ptr = s2Ptr;
								s2Ptr = s2Ptr->next;
							}
							/* add new entry to the end of shipItemList */
							s1Ptr->next = shipItemPtr;
						}

					}
					else
					{
						/* This item is not ready for shipping, error message */ 
						sprintf(Msg, "Order %d, item %d is not ready for shipping.\n",
									 	itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up */
							OP_FREECHARPTR(concat_msg.Msg);

							/*
							** Internal error, free up shipItemList
							** do not continue processing 
							*/
							if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
							{
								free_shipItemList (&shipItemList);
							}

							/* rollback transaction, release lock */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
							/* Change cursor back to normal */
							timeOutCursors (False);

							/* Display error messages */
							strcpy(Msg, "Create New Shipment:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
							msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

							return;
						}

					}

				}

			} /* if (itemPtr->selectFlag) */

			itemPtr = itemPtr->next;

		} /* while */

		if (shipItemList == (OP_SHIP_ITEM_LIST *)NULL)
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Create New Shipment cannot proceed due to one of the "
									 "following reasons: \n"
									 "No item is selected or " 
									 "No item is ready to be shipped.\n");   

			if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Display error messages */
				strcpy(Msg, "Create New Shipment:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
				return;
			}

			/* Display all messages in browse dialog box */
			if (concat_msg.Msg != NULL)
			{
				if ((len = strlen(concat_msg.Msg)) > 512)
				{
					strcpy (label, "Ship  Items  Message  Box");
					browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
				}
				else
				{
					msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
				}
			}

			/* free up concat_msg space allocated */
			OP_FREECHARPTR(concat_msg.Msg);

			return;
		}

	} /* Ship Items */

	/*
	** We have the shipItemList constructed, the following 
	** parts are common to both order and item levels
	*/

	/*
	** execute stored procedure op_get_shipping_data to do the following:
	** Get new shipping_id   
	** insert new entry into table shipping
	** Get maximum profile_id
	** Insert new entry into table shipping_of
	** Populate Shipping Data Spec
	** Get info from table shipping
	** Get profile info from shipping_profile table
	*/

	/* free shippingData later */
	if ((shippingData = (OP_SHIPPING_DATA *) malloc
			((unsigned) sizeof (OP_SHIPPING_DATA))) ==
				(OP_SHIPPING_DATA *) NULL)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* memory allocation failed, clean up */
		OP_FREECHARPTR(concat_msg.Msg);

		/*
		** Internal error, free up shipItemList
		** do not continue processing 
		*/
		if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
		{
			free_shipItemList (&shipItemList);
		}

		/* Display error messages */
		strcpy(Msg, "Create New Shipment:\n"
					"Internal Error: memory allocation failed.\n" 
					"Please contact the DBA and exit the program.\n");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

		return;
	}

	/* initialize shippingData */
	memset (shippingData, 0, sizeof(OP_SHIPPING_DATA));

	shippingData->order_id = order_id;
	shippingData->shipItemList = shipItemList;

	/*
	** execute stored procedure incr_shipping_id to get
	** the new shipping_id.
	*/
	catReq->item[0] = (DBINT *)&shippingData->shipping_id;
	if ((status = ims_opCat(catReq, OP_GETSHIPPINGID)) < 0)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/*
		** Internal error, free up shipItemList
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: Create new shipment failed.\n"
								 "OP_GETSHIPPINGID failed for Order: %d\n", 
								 shippingData->order_id); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg);

		if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
		{
			free_shipItemList (&shipItemList);
		}
		
		/* free shippingData allocated */
		free (shippingData);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		return;
	}


	/* 
	** execute stored procedure op_get_shipping_data to populate
	** the shippingData data structure.
	*/
	sprintf
	(query, "op_get_shipping_data %d, %d", order_id, shippingData->shipping_id);

	catReq->item[0] = (OP_SHIPPING_DATA *)shippingData;
	catReq->item[1] = (char *)query;
	if ((status = ims_opCat(catReq, OP_GETSHIPPINGDATA)) < 0)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/*
		** Internal error, free up shipItemList
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: Create new shipment failed.\n"
								 "OP_GETSHIPPINGDATA failed for Order: %d\n", 
								 shippingData->order_id); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg);

		if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
		{
			free_shipItemList (&shipItemList);
		}
		
		/* free shippingData allocated */
		free (shippingData);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		return;
	}

	/*
	** Go through the shipItemList, for each item, update
	** shipping_id, shipped_p in table order_item
	*/

 	s1Ptr = shipItemList;
	while (s1Ptr != (OP_SHIP_ITEM_LIST *)NULL)
	{
		catReq->item[0] = (DBINT *)&shippingData->order_id;
		catReq->item[1] = (DBSMALLINT *)&s1Ptr->item_id;
		catReq->item[2] = (DBINT *)&shippingData->shipping_id;
		if ((status = ims_opCat(catReq, OP_UPDATEITEMSHIPPINGSTATUS)) < 0)
		{
			/* Display error messages */
			sprintf (Msg,
							 "Internal Error: Could not update shipping flag, shipping_id "
							 "for order %d, item %d\n",
							 shippingData->order_id, s1Ptr->item_id);   

			if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
			{
				/* rollback transaction, release lock */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy(Msg, "Create New Shipment:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

				/* memory allocation failed, clean up */
				OP_FREECHARPTR(concat_msg.Msg);

				/* free shippingData allocated */
				free (shippingData);

				/*
				** Internal error, free up shipItemList
				** do not continue processing 
				*/
				if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
				{
					free_shipItemList (&shipItemList);
				}

				return;
			}

			/*
			** if update item shipping flag and shipping_id fails, should not
			** count this item in the shipItemList, set the status of this 
			** ship item to be 0, so it is ignored.   
			*/
			s1Ptr->status = 0; 
		}
		else
		{
			/* 
			** set the status to be 1, 
			** if the item has status CANCELLED, cost and quantity
			** are set to zero here because they are not taken into
			** account for the total quantity and total cost of the 
			** new shipment.
			** add the item's quantity and cost to the total quantity
			** and total cost of the shipping spec. 
			*/

			/*
			** update shipped_p, shipping_id if order is 
			** currently being displayed.
			*/
			if (clientData->currOrder != NULL) 
			{
				if (clientData->currOrder->itemList != (OP_ORDER_ITEM_LIST *)NULL)
				{
					if (order_id == clientData->currOrder->order_id)
					{
						itemPtr = clientData->currOrder->itemList;

						found = IMS_FALSE;
						while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (!found))
						{
							if (itemPtr->item_id == s1Ptr->item_id)
							{
								itemPtr->shipped_p = 'Y';
								itemPtr->shipping_id = shippingData->shipping_id;
								found = IMS_TRUE;
							}
							itemPtr = itemPtr->next;
						}
					}
				}
			}

			s1Ptr->status = 1; 
			ship_count++;

			if (s1Ptr->item_status == ITEM_CANCELLED)
			{
				s1Ptr->cost = 0;
				s1Ptr->quantity = 0;
			}

			shippingData->total_qty += s1Ptr->quantity;
			shippingData->total_cost += s1Ptr->cost;
		}

		s1Ptr = s1Ptr->next;
	}

	/*
	** Check to see whether processing should continue at this point.   
	*/
	if (ship_count <= 0)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy (Msg, "Create New Shipment cannot proceed.\n");   

		if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
		{
			/* memory allocation failed, clean up */
			OP_FREECHARPTR(concat_msg.Msg);

			if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
			{
				free_shipItemList (&shipItemList);
			}

			/* free shippingData allocated */
			free (shippingData);

			/* Display error messages */
			strcpy(Msg, "Create New Shipment:\n"
									"Internal Error: memory allocation failed.\n" 
									"Please contact the DBA and exit the program.\n");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			return;
		}

		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 512)
			{
				strcpy (label, "Ship  Items  Message  Box");
				browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
			}
			else
			{
				msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
			}
		}

		if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
		{
			free_shipItemList (&shipItemList);
		}

		/* free shippingData allocated */
		free (shippingData);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		return;
	}


	/* 
	** Call function display_shippingData to populate the 
	** Shipping Screen with information from the shipping Spec 
	** and to pop up the shipping screen.
	** If malloc failed in display_shippingData, error message
	** box is popped up from display_shippingData function, 
	** here we rollback all transactions.
	*/
	if ((status = display_shippingData (shippingData, 1)) < IMS_OK)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/*
		** Internal error, free up shipItemList
		** do not continue processing 
		*/
		if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
		{
			free_shipItemList (&shipItemList);
		}

		/* free billingData allocated */
		free (shippingData);

		return;
	}


	/*
	** Commit Transaction
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/*
		** Internal error, free up shipItemList
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: Create New Shipment,\n"
								 "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
								 order_id);   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
		if (shipItemList != (OP_SHIP_ITEM_LIST *)NULL)
		{
			free_shipItemList (&shipItemList);
		}

		/* free shippingData allocated */
		free (shippingData);

		return;
	}
	

	/* Display all messages in browse dialog box */
	if (concat_msg.Msg != NULL)
	{
		if ((len = strlen(concat_msg.Msg)) > 512)
		{
			strcpy (label, "Create  New  Shipment  Message  Box");
			browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
		}
		else
		{
			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
		}
	}

	/* free up concat_msg space allocated */
	OP_FREECHARPTR(concat_msg.Msg);

	/* free up shipItemList */
	if (shipItemList != (OP_SHIP_ITEM_LIST *) NULL)
	{
		free_shipItemList (&shipItemList);
	}

	/* free shippingData allocated */
	free (shippingData);

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
 UxOrderContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: order_view_shippingReportsCb
**
** Description:		callback function for view shipping reports menu item
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_view_shippingReportsCb( 
	Widget widget, 
	int fieldId,
	XtPointer cb) 
{
	_UxCorder               *UxSaveCtx, *UxContext;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	XmString label;
	char buffer[IMS_COL255_LEN+1];
	CONCAT_STR concat_msg = {0};
	DBINT order_id;
	int status, len, count;
	static OP_ORDER_SHIPID_LIST *shipIdList;


 UxSaveCtx = UxOrderContext;
 UxOrderContext = UxContext =
 		(_UxCorder *) UxGetContext( glbData.orderW );
{
	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	shipIdList = (OP_ORDER_SHIPID_LIST *)NULL;
	count = 0;

	/* Timeout cursor */
	timeOutCursors (True);

	if (fieldId == SHIP_ORDER) 
	{
		/* View Shipping Reports called from order level */
		/* Locate the selected order in orderList */

		orderPtr = clientData->orderList;
		while ((orderPtr != (OP_ORDER_LIST *)NULL) && (orderPtr->selectFlag == 0))
		{
			orderPtr = orderPtr->next;
		}

		if ((orderPtr == (OP_ORDER_LIST *)NULL))
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* No order is selected, display message, return */
			sprintf(Msg, "View Shipping Reports: No order is selected.\n");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}

		order_id = orderPtr->order_id;
	}

	if (fieldId == SHIP_ITEMS)
	{
		order_id = clientData->currOrder->order_id;
	}

	/* 
	** we have the order_id now, retrieve distinct shipping_id and
	** create_time from table shipping then display in the shipIdSL. 
	*/
	catReq->item[0] = (DBINT *)&order_id;
	catReq->item[1] = (int *)&count;
	catReq->item[2] = (OP_ORDER_SHIPID_LIST *)shipIdList;
	if ((status = ims_opCat (catReq, OP_GETORDERSHIPIDLIST)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Create New Shipment:\n"
					 "Internal Error: OP_GETORDERSHIPIDLIST failed for order: %d\n",
					 order_id);
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

		return;
	}

	/* assign count and shipItemList returned from query */
	count = *(int *)catReq->item[1];
	shipIdList = (OP_ORDER_SHIPID_LIST *)catReq->item[2];

	if ((count == 0) || (shipIdList == (OP_ORDER_SHIPID_LIST *)NULL))
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Shipping Reports:\n"
								 "No shipping report found in the catalog for order: %d.\n",
								 order_id);   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 

		return;
	}
		
	/* 
	** pop up the shipping folder screen with shipping id list
	*/
	if ((status = display_shipViewScreen(order_id, shipIdList)) < IMS_OK)
	{
		if (shipIdList != (OP_ORDER_SHIPID_LIST *) NULL)
		{
			free_shipIdList (&shipIdList);
		}
	}
		
	if (shipIdList != (OP_ORDER_SHIPID_LIST *) NULL)
	{
		free_shipIdList (&shipIdList);
	}

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
 UxOrderContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: order_create_invoiceCb
**
** Description:		callback function for shipping & billing menu item
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_create_invoiceCb( 
	Widget widget, 
	int fieldId,
	XtPointer cb) 
{
	_UxCorder               *UxSaveCtx, *UxContext;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_BILL_ITEM_LIST *billItemList, *billItemPtr, *b1Ptr, *b2Ptr;
	OP_BILLING_DATA *billingData;
	char Msg[IMS_COL1024_LEN+1];
	char label[IMS_COL128_LEN+1];
	char query[IMS_COL255_LEN+1];
	CONCAT_STR concat_msg = {0};
	DBINT order_id;
	int status, len, count, found;
	int bill_count = 0;

 UxSaveCtx = UxOrderContext;
 UxOrderContext = UxContext =
 		(_UxCorder *) UxGetContext( glbData.orderW );
{
	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	billItemList = (OP_BILL_ITEM_LIST *)NULL;
	billingData = (OP_BILLING_DATA *)NULL;
	count = 0;

	if (glbData.billingFlag)
	{
		/* Display error messages */
		strcpy (Msg, "Create New Invoice: Billing screen is already popped up.\n"
								 "Please complete the current invoice.\n"); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
	
		return; 
	}

	/* Timeout cursor */
	timeOutCursors (True);

	/*
	** Begin Transaction
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy (Msg, "Internal Error: Create New Invoice\n"
								 "OP_BEGINTRANSACTION failed.\n"); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
		return; 
	}

	/*
	** Get General Lock
	*/
	if ((status = ims_opCat (catReq, OP_GETGENERALLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "Internal Error: Create New Invoice\n"
								 "OP_GETGENERALLOCK failed.\n"); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
		return;
	}

	/* 
	** Construct billItemList
	** Order Level - fieldId = BILL_ORDER 
	** Item Level  - fieldId = BILL_ITEMS
	*/
	if (fieldId == BILL_ORDER) 
	{
		/* Create New Invoice called from order level */
		/* Locate the selected order in orderList */

		orderPtr = clientData->orderList;
		while ((orderPtr != (OP_ORDER_LIST *)NULL) && (orderPtr->selectFlag == 0))
		{
			orderPtr = orderPtr->next;
		}

		if ((orderPtr == (OP_ORDER_LIST *)NULL))
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* No order is selected, display message, return */
			sprintf(Msg, "Create New Invoice: No order is selected.");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}

		order_id = orderPtr->order_id;

		sprintf (query,
		"select t1.item_id, t1.p_granule_name, t1.cost, t1.process_type, "
		" t1.quantity, t1.media_type, t1.status "
		" from order_item t1 " 
		" where t1.order_id = %d"  
		" and t1.status in (10, 12)" 
		" and t1.billed_p = 'N'"
		" and t1.billing_id = NULL"
		" and t1.shipped_p = 'Y'"
		" and t1.shipping_id != NULL"
		" order by item_id",
		order_id);

		catReq->item[0] = (char *)query;
		catReq->item[1] = (int *)&count;
		catReq->item[2] = (OP_BILL_ITEM_LIST *)billItemList;
		if ((status = ims_opCat (catReq, OP_GETBILLITEMS)) < IMS_OK)
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Create New Invoice:\n"
									 "Internal Error: OP_GETBILLITEMS failed for order: %d\n",
									 order_id);
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			return;
		}

		/* assign count and billItemList returned from query */
		count = *(int *)catReq->item[1];
		billItemList = (OP_BILL_ITEM_LIST *)catReq->item[2];

		if ((count == 0) || (billItemList == (OP_BILL_ITEM_LIST *)NULL))
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			sprintf(Msg, "Create New Invoice:\n"
									 "Order %d, no item is ready to be billed.\n",
										orderPtr->order_id);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 

			return;
		}
	}


	if (fieldId == BILL_ITEMS)
	{
		/* Create New Invoice called from item level */
		itemPtr = clientData->currOrder->itemList;
		order_id = itemPtr->order_id;
		
		while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL) 
		{
			if (itemPtr->selectFlag)
			{
				/*
				** call OP_VERIFYITEMBILLINGSTATUS to verify that the 
				** item status is COMPLETE or CANCELLED, item billed_p is 'N'
				** and the item billing_id is NULL
				** OP_VERIFYITEMBILLINGSTATUS returns count = 1 if the
				** item is available for billing, if the item
				** is not available for billing, count is 0
				*/
				catReq->item[0] = (DBINT *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (int *)&count;
				if ((status = ims_opCat(catReq, OP_VERIFYITEMBILLINGSTATUS)) < 0)
				{
					/* Display error messages */
					sprintf(Msg, "Create New Invoice:\nInternal Error: "
								 "OP_VERIFYITEMBILLINGSTATUS failed for Order: %d, Item: %d\n", 
								 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up */
						OP_FREECHARPTR(concat_msg.Msg);

						/*
						** Internal error, free up shipItemList
						** do not continue processing 
						*/
						if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
						{
							free_billItemList (&billItemList);
						}

						/* rollback transaction, release lock */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Change cursor back to normal */
						timeOutCursors (False);

						/* Display error messages */
						strcpy(Msg, "Create New Invoice:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

						return;
					}

					itemPtr = itemPtr->next;
					continue;

				}
				else
				{
					if (count > 0) 
					{
						/* This item is ready for billing, add it to the billItemList */

						if ((billItemPtr = (OP_BILL_ITEM_LIST *) malloc
								((unsigned) sizeof (OP_BILL_ITEM_LIST))) ==
									(OP_BILL_ITEM_LIST *) NULL)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Change cursor back to normal */
							timeOutCursors (False);

							/* memory allocation failed, clean up */
							OP_FREECHARPTR(concat_msg.Msg);

							/*
							** Internal error, free up shipItemList
							** do not continue processing 
							*/
							if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
							{
								free_billItemList (&billItemList);
							}

							/* Display error messages */
							strcpy(Msg, "Create New Invoice:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
							msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

							return;
						}

						/* add item into the billItemList */
						billItemPtr->item_id = itemPtr->item_id;
						billItemPtr->process_type = itemPtr->process_type;
						(void) strcpy (billItemPtr->name, itemPtr->p_granule_name);
						billItemPtr->media_type = itemPtr->media_type;
						billItemPtr->quantity = itemPtr->quantity;
						billItemPtr->cost = itemPtr->cost;
						billItemPtr->item_status  = itemPtr->status;
						billItemPtr->next = NULL;

						if (billItemList == (OP_BILL_ITEM_LIST *)NULL)
						{
							/* first item in the list */
							billItemList = billItemPtr;
						}
						else
						{
							b1Ptr = b2Ptr = billItemList; 
							while (b2Ptr != (OP_BILL_ITEM_LIST *)NULL)
							{
								b1Ptr = b2Ptr;
								b2Ptr = b2Ptr->next;
							}
							/* add new entry to the end of billItemList */
							b1Ptr->next = billItemPtr;
						}

					}
					else
					{
						/* This item is not ready for billing, error message */ 
						sprintf(Msg, "Order %d, item %d is not ready for billing.\n",
									 	itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up */
							OP_FREECHARPTR(concat_msg.Msg);

							/*
							** Internal error, free up billItemList
							** do not continue processing 
							*/
							if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
							{
								free_billItemList (&billItemList);
							}

							/* rollback transaction, release lock */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
							/* Change cursor back to normal */
							timeOutCursors (False);

							/* Display error messages */
							strcpy(Msg, "Create New Invoice:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
							msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

							return;
						}

					}

				}

			} /* if (itemPtr->selectFlag) */

			itemPtr = itemPtr->next;

		} /* while */

		if (billItemList == (OP_BILL_ITEM_LIST *)NULL)
		{
			/* rollback transaction, release lock */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Create New Invoice cannot proceed due to one of the "
									 "following reasons: \n"
									 "No item is selected or " 
									 "No item is ready to be billed.");   

			if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Display error messages */
				strcpy(Msg, "Create New Invoice:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
				return;
			}

			/* Display all messages in browse dialog box */
			if (concat_msg.Msg != NULL)
			{
				if ((len = strlen(concat_msg.Msg)) > 512)
				{
					strcpy (label, "Bill  Items  Message  Box");
					browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
				}
				else
				{
					msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
				}
			}

			/* free up concat_msg space allocated */
			OP_FREECHARPTR(concat_msg.Msg);

			return;
		}

	} /* Bill Items */

	/*
	** We have the billItemList constructed, the following 
	** parts are common to both order and item levels
	*/

	/*
	** execute stored procedure op_get_billing_data to do the following:
	** Get new billing_id   
	** insert new entry into table billing  
	** Get maximum profile_id
	** Insert new entry into table billing_of
	** Populate Billing Data Spec
	** Get info from table billing 
	** Get profile info from billing_profile table
	*/

	if ((billingData = (OP_BILLING_DATA *) malloc
			((unsigned) sizeof (OP_BILLING_DATA))) ==
				(OP_BILLING_DATA *) NULL)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* memory allocation failed, clean up */
		OP_FREECHARPTR(concat_msg.Msg);

		/*
		** Internal error, free up billItemList
		** do not continue processing 
		*/
		if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
		{
			free_billItemList (&billItemList);
		}

		/* Display error messages */
		strcpy(Msg, "Create New Invoice:\n"
					"Internal Error: memory allocation failed.\n" 
					"Please contact the DBA and exit the program.\n");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

		return;
	}

	/* initialize billingData */
	memset (billingData, 0, sizeof(OP_BILLING_DATA));

	billingData->order_id = order_id;
	billingData->billItemList = billItemList;

	/*
	** execute stored procedure incr_billing_id to get
	** the new billing_id.
	*/
	catReq->item[0] = (DBINT *)&billingData->billing_id;
	if ((status = ims_opCat(catReq, OP_GETBILLINGID)) < 0)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/*
		** Internal error, free up billItemList
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: Create new invoice failed.\n"
								 "OP_GETBILLINGID failed for Order: %d\n", 
								 billingData->order_id); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg);

		if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
		{
			free_billItemList (&billItemList);
		}
		
		/* free billingData allocated */
		free (billingData);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		return;
	}


	/* 
	** execute stored procedure op_get_billing_data to populate
	** the billingData data structure.
	*/
	sprintf
	(query, "op_get_billing_data %d, %d", order_id, billingData->billing_id);

	catReq->item[0] = (OP_SHIPPING_DATA *)billingData;
	catReq->item[1] = (char *)query;
	if ((status = ims_opCat(catReq, OP_GETBILLINGDATA)) < 0)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/*
		** Internal error, free up billItemList
		** do not continue processing 
		*/

		/* Display error messages */
		sprintf(Msg, "Internal Error: Create new invoice failed.\n"
								 "OP_GETBILLINGDATA failed for Order: %d\n", 
								 billingData->order_id); 
		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg);

		if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
		{
			free_billItemList (&billItemList);
		}
		
		/* free billingData allocated */
		free (billingData);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		return;
	}

	/*
	** Go through the billItemList, for each item, update
	** billing_id, billed_p in table order_item
	*/

 	b1Ptr = billItemList;
	while (b1Ptr != (OP_BILL_ITEM_LIST *)NULL)
	{
		catReq->item[0] = (DBINT *)&billingData->order_id;
		catReq->item[1] = (DBSMALLINT *)&b1Ptr->item_id;
		catReq->item[2] = (DBINT *)&billingData->billing_id;
		if ((status = ims_opCat(catReq, OP_UPDATEITEMBILLINGSTATUS)) < 0)
		{
			/* Display error messages */
			sprintf (Msg,
							 "Internal Error: Could not update billing flag, billing_id "
							 "for order %d, item %d\n",
							 billingData->order_id, b1Ptr->item_id);   

			if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
			{
				/* rollback transaction, release lock */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy(Msg, "Create New Invoice:\n"
										"Internal Error: memory allocation failed.\n" 
										"Please contact the DBA and exit the program.\n");   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

				/* memory allocation failed, clean up */
				OP_FREECHARPTR(concat_msg.Msg);

				/* free billingData allocated */
				free (billingData);

				/*
				** Internal error, free up billItemList
				** do not continue processing 
				*/
				if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
				{
					free_billItemList (&billItemList);
				}

				return;
			}

			/*
			** if update item billing flag and billing_id fails, should not
			** count this item in the billItemList, set the status of this 
			** bill item to be 0, so it is ignored.   
			*/
			b1Ptr->status = 0; 
		}
		else
		{
			/* 
			** set the status to be 1, add the item's cost & quantity 
			** if item has status CANCELLED, set the quantity and cost
			** to be zero because they are not taken into account of 
			** the total amount for the invoice.
			** to the total quantity and total amount of the billingData spec.
			*/
			b1Ptr->status = 1; 
			bill_count++;

			if (b1Ptr->item_status == ITEM_CANCELLED)
			{
				b1Ptr->quantity = 0;
				b1Ptr->cost = 0;
			}

			billingData->invoice_amount += b1Ptr->cost;
			billingData->total_qty += b1Ptr->quantity;

			/*
			** update billed_p and billing_id, if the order is 
			** currently being displayed.
			*/
			if (clientData->currOrder != NULL) 
			{
				if (clientData->currOrder->itemList != (OP_ORDER_ITEM_LIST *)NULL)
				{
					if (order_id == clientData->currOrder->order_id)
					{
						itemPtr = clientData->currOrder->itemList;

						found = IMS_FALSE;
						while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (!found))
						{
							if (itemPtr->item_id == b1Ptr->item_id)
							{
								itemPtr->billed_p = 'Y';
								itemPtr->billing_id = billingData->billing_id;
								found = IMS_TRUE;
							}
							itemPtr = itemPtr->next;
						}
					}
				}
			}

		}

		b1Ptr = b1Ptr->next;
	}

	/*
	** Check to see whether processing should continue at this point.   
	*/
	if (bill_count <= 0)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy (Msg, "Create New Invoice cannot proceed.\n");   

		if ((status = concatString(&concat_msg, Msg)) < IMS_OK)
		{
			/* memory allocation failed, clean up */
			OP_FREECHARPTR(concat_msg.Msg);

			if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
			{
				free_billItemList (&billItemList);
			}

			/* free billingData allocated */
			free (billingData);

			/* Display error messages */
			strcpy(Msg, "Create New Invoice:\n"
									"Internal Error: memory allocation failed.\n" 
									"Please contact the DBA and exit the program.\n");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

			return;
		}

		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 512)
			{
				strcpy (label, "Bill  Items  Message  Box");
				browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
			}
			else
			{
				msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
			}
		}

		if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
		{
			free_billItemList (&billItemList);
		}

		/* free billingData allocated */
		free (billingData);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		return;
	}


	/* 
	** Call function display_billingData to populate the 
	** Billing Screen with information from the billing Spec 
	** and to pop up the Billing screen.
	** If malloc failed in display_billingData, error message
	** box is popped up from display_billingData function, 
	** here we rollback all transactions.
	*/
	if ((status = display_billingData (billingData, 1)) < IMS_OK)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/*
		** Internal error, free up billItemList
		** do not continue processing 
		*/
		if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
		{
			free_billItemList (&billItemList);
		}

		/* free billingData allocated */
		free (billingData);

		return;
	}


	/*
	** Commit Transaction
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction, release lock */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Internal Error: Create New Invoice,\n"
								 "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
								 order_id);   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/*
		** Internal error, free up billItemList
		** do not continue processing 
		*/
		if (billItemList != (OP_BILL_ITEM_LIST *)NULL)
		{
			free_billItemList (&billItemList);
		}

		/* free billingData allocated */
		free (billingData);

		return;
	}
	

	/* Display all messages in browse dialog box */
	if (concat_msg.Msg != NULL)
	{
		if ((len = strlen(concat_msg.Msg)) > 512)
		{
			strcpy (label, "Create  New  Invoice  Message  Box");
			browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
		}
		else
		{
			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
		}
	}

	/* free up concat_msg space allocated */
	OP_FREECHARPTR(concat_msg.Msg);

	/* free up billItemList */
	if (billItemList != (OP_BILL_ITEM_LIST *) NULL)
	{
		free_billItemList (&billItemList);
	}

	/* free billingData allocated */
	free (billingData);

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
 UxOrderContext = UxSaveCtx;
	
}

/*===========================================================================*
** 
** Function Name: order_view_invoicesCb
**
** Description:		callback function for shipping & billing menu item
**								
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_view_invoiceCb( 
	Widget widget, 
	int    fieldId,
	XtPointer cb) 
{
	_UxCorder               *UxSaveCtx, *UxContext;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	XmString label;
	char buffer[IMS_COL255_LEN+1];
	CONCAT_STR concat_msg = {0};
	DBINT order_id;
	int status, len, count;
	OP_ORDER_BILLID_LIST *billIdList, *bPtr;


 UxSaveCtx = UxOrderContext;
 UxOrderContext = UxContext =
 		(_UxCorder *) UxGetContext( glbData.orderW );
{
	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	billIdList = (OP_ORDER_BILLID_LIST *)NULL;
	count = 0;

	/* Timeout cursor */
	timeOutCursors (True);

	if (fieldId == BILL_ORDER) 
	{
		/* View Shipping Reports called from order level */
		/* Locate the selected order in orderList */

		orderPtr = clientData->orderList;
		while ((orderPtr != (OP_ORDER_LIST *)NULL) && (orderPtr->selectFlag == 0))
		{
			orderPtr = orderPtr->next;
		}

		if ((orderPtr == (OP_ORDER_LIST *)NULL))
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* No order is selected, display message, return */
			sprintf(Msg, "View Invoices: No order is selected.\n");   
			msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 
			return;
		}

		order_id = orderPtr->order_id;
	}

	if (fieldId == BILL_ITEMS)
	{
		order_id = clientData->currOrder->order_id;
	}

	/* 
	** we have the order_id now, retrieve distinct billing_id and
	** create_time from table billing then display in the billIdSL. 
	*/
	catReq->item[0] = (DBINT *)&order_id;
	catReq->item[1] = (int *)&count;
	catReq->item[2] = (OP_ORDER_BILLID_LIST *)billIdList;
	if ((status = ims_opCat (catReq, OP_GETORDERBILLIDLIST)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Invoices:\n"
					 "Internal Error: OP_GETORDERBILLIDLIST failed for order: %d\n",
					 order_id);
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

		return;
	}

	/* assign count and billItemList returned from query */
	count = *(int *)catReq->item[1];
	billIdList = (OP_ORDER_BILLID_LIST *)catReq->item[2];

	if ((count == 0) || (billIdList == (OP_ORDER_BILLID_LIST *)NULL))
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "View Invoices:\n"
								 "No invoices found in the catalog for order: %d.\n",
								 order_id);   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 

		return;
	}
		
	/* 
	** pop up the invoice folder screen with billing id list
	*/
	if ((status = display_billViewScreen(order_id, billIdList)) < IMS_OK)
	{
		if (billIdList != (OP_ORDER_BILLID_LIST *) NULL)
		{
			free_billIdList (&billIdList);
		}
	}
		
	if (billIdList != (OP_ORDER_BILLID_LIST *) NULL)
	{
		free_billIdList (&billIdList);
	}

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
 UxOrderContext = UxSaveCtx;

	
}

/*===========================================================================*
** 
** Function Name:	free_shipItemList
**
** Description:		Function to free up shipItemList
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_shipItemList(
	OP_SHIP_ITEM_LIST **shipItemList)

{
	OP_SHIP_ITEM_LIST *sPtr, *nextPtr;

	sPtr = *shipItemList;
	while (sPtr != (OP_SHIP_ITEM_LIST *) NULL)
	{
		nextPtr = sPtr->next;
		free (sPtr);
		sPtr = nextPtr;
	}

	*shipItemList = (OP_SHIP_ITEM_LIST *)NULL;

}


/*===========================================================================*
** 
** Function Name:	free_shipIdList
**
** Description:		Function to free up shipIdList
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_shipIdList(
	OP_ORDER_SHIPID_LIST **shipIdList)

{
	OP_ORDER_SHIPID_LIST *sPtr, *nextPtr;

	sPtr = *shipIdList;
	while (sPtr != (OP_ORDER_SHIPID_LIST *) NULL)
	{
		nextPtr = sPtr->next;
		free (sPtr);
		sPtr = nextPtr;
	}

	*shipIdList = (OP_ORDER_SHIPID_LIST *)NULL;

}


/*===========================================================================*
** 
** Function Name:	free_billItemList
**
** Description:		Function to free up billItemList
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_billItemList(
	OP_BILL_ITEM_LIST **billItemList)

{
	OP_BILL_ITEM_LIST *bPtr, *nextPtr;

	bPtr = *billItemList;
	while (bPtr != (OP_BILL_ITEM_LIST *) NULL)
	{
		nextPtr = bPtr->next;
		free (bPtr);
		bPtr = nextPtr;
	}

	*billItemList = (OP_BILL_ITEM_LIST *)NULL;

}


/*===========================================================================*
** 
** Function Name:	free_billIdList
**
** Description:		Function to free up billIdList
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	free_billIdList(
	OP_ORDER_BILLID_LIST **billIdList)

{
	OP_ORDER_BILLID_LIST *bPtr, *nextPtr;

	bPtr = *billIdList;
	while (bPtr != (OP_ORDER_BILLID_LIST *) NULL)
	{
		nextPtr = bPtr->next;
		free (bPtr);
		bPtr = nextPtr;
	}

	*billIdList = (OP_ORDER_BILLID_LIST *)NULL;

}

/*===========================================================================*
** 
** Function Name: order_save_resultsCb
**
** Description:		Pop up the file selection dialog to save query results
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History: 06/11/96 - Modified to correct PR 942.
**
**==========================================================================*/

void	order_save_resultsCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	Arg args[3];
	int n = 0;
	
	XtSetArg (args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
	glbData.fileSelectDlg = (Widget) XmCreateFileSelectionDialog(glbData.orderW,
												"Save Results...", args, n);

	XtAddCallback(glbData.fileSelectDlg, XmNcancelCallback, 
					(XtCallbackProc)order_save_results_cancelCb, NULL);

	XtAddCallback(glbData.fileSelectDlg, XmNokCallback, 
					(XtCallbackProc)order_save_results_okCb, NULL);

	XtSetSensitive(
		XmFileSelectionBoxGetChild (glbData.fileSelectDlg, XmDIALOG_HELP_BUTTON), False);

	/*
	** This is to add the callbacks to the window manager quit
	** button for each screen, this is to correct PR 942
	*/
	addWinMgrCloseCB (glbData.fileSelectDlg, order_save_results_cancelCb, NULL);

	XtManageChild(glbData.fileSelectDlg);
	XtPopup(XtParent(glbData.fileSelectDlg), XtGrabExclusive);

}


/*===========================================================================*
** 
** Function Name: order_save_results_okCb
**
** Description:		callback function for the ok button in the file
**                selection dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
**==========================================================================*/

void	order_save_results_okCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	XmFileSelectionBoxCallbackStruct *cbs
	 = (XmFileSelectionBoxCallbackStruct *) cb;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	char *file = NULL;
	char process_type[IMS_COL30_LEN+1];
	char media_type[IMS_COL30_LEN+1];
	char granule_name[IMS_COL30_LEN+1];
	OP_ORDER_LIST *orderPtr, *tempPtr;
	FILE *fp;
	int k, orderCount, itemCount, status;
	char cmdbuf[IMS_COL255_LEN+1];

	/*
	** Get the file name and verify that it is writable.
	** If not writable, error message and return.
	*/
	if (!XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
		return;

	if (*file != '/')
	{
		/*
		** if it is not a directory, determine the full pathname
		** of the selection by concatenating it to the "dir" part
		*/
		char *dir, *newfile;
		if (XmStringGetLtoR (cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
		{
			newfile = XtMalloc (strlen(dir) + 1 + strlen(file) +1);
			sprintf (newfile, "%s/%s", dir, file);
			XtFree (file);
			XtFree (dir);
			file = newfile;
		}
	}

	if (!(fp = fopen(file, "w")))
	{
		XtFree (file);

		/* Display error messages */
		sprintf(Msg, "File %s is not writable.", file);   
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return;
	}


	/* Change cursor to watch cursor */
	timeOutCursors (True);

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	orderPtr = clientData->orderList;
	orderCount = clientData->orderCount;
	
	/*
	** Go through the order list, for each order, if the 
	** item list is NULL, then call OP_GETUSERDATA and
	** OP_GETORDERITEMLIST events to get the user and account
	** information and the item list for that order.
	*/
	if ((orderCount > 0) && (orderPtr != NULL))
	{
		tempPtr = orderPtr;
		while (tempPtr != (OP_ORDER_LIST *)NULL)
		{
			if (tempPtr->itemList == (OP_ORDER_ITEM_LIST *)NULL)
			{
				/*
				** Get itemList from database for this order.		
				** Initialize catalog request structure 
				*/
				catReq->item[0] = (OP_ORDER_LIST *)tempPtr;
				catReq->item[1] = (int *)&itemCount;

				/* 
				** Get user profile and account data for the order
				*/
				if ((status = ims_opCat (catReq, OP_GETUSERDATA)) < IMS_OK)
				{
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* close the file */
					fclose (fp);
					XtFree (file);

					/* Display error messages */
					sprintf(Msg, 
						"Internal Error: getUserData failed for order %d.",
						tempPtr->order_id);   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

					return;
				}


				/*
				** Get order item list for the order
				*/
				catReq->item[0] = (OP_ORDER_LIST *)tempPtr;
				catReq->item[1] = (int *)&itemCount;
				if ((status = ims_opCat (catReq, OP_GETORDERITEMLIST)) < IMS_OK)
				{
					/* Change cursor back to normal */
					timeOutCursors (False);

					/* close the file */
					fclose (fp);
					XtFree (file);

					/* Display error messages */
					sprintf(Msg, "Internal Error: Order %d item retrieval failed.");   
					msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
					return;
				}

				/* assign returned items */
				tempPtr->item_count = *(int *)catReq->item[1];
				tempPtr->itemList = (OP_ORDER_ITEM_LIST *)catReq->item[2];

				/*
				** get PPS granule size(data_kbytes) from granules table  
				** and step_avgtime from order_item_step table
				*/
				itemPtr = tempPtr->itemList;
				while (itemPtr != (OP_ORDER_ITEM_LIST *) NULL)
				{
					catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;
					if ((status = ims_opCat (catReq, OP_GETPPSGRANULESIZESTEPTIME)) < IMS_OK)
					{
						/* Change cursor back to normal */
						timeOutCursors (False);

						/* close the file */
						fclose (fp);
						XtFree (file);

						/* Display error messages */
						sprintf(Msg, "Internal Error: Function getPPSGranuleSize failed.");   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
						return;
					}

					itemPtr = itemPtr->next;
				}

			} /* if */

			tempPtr = tempPtr->next;

		} /* while */
	}

	/*
	** Write the query to the first page, and the total number
	** of orders returned by the query.
	** 
	** Page Break.
	** 
	** Go through the order list, for each order, write out the
	** following information: Order Id, Account Id, User Id, 
	** Received Time, and the total number of items in that order.
	**
	** For each order, go through the order item list, write out
	** the following information: Item Id, Description(Process type),
	** Product ID, Media, Quantity, and Linecost.
	**
	** Page Break.
	*/

	/*
 	** fprintf (fp, "Query: %s\n\n\n", clientData->queryStruct.sqlBuf);
	** fprintf (fp, "Order Count: %d\f", orderCount);
	*/

	tempPtr = orderPtr;
	while (tempPtr != (OP_ORDER_LIST *)NULL)
	{
		fprintf (fp, "OrderId: %d\t\t\t\tAccountId: %s\n", 
								 tempPtr->order_id, tempPtr->account_id);

		fprintf (fp, "UserId: %s\t\t\tReceived Time: %s\n",
								 tempPtr->user_id, tempPtr->received_time);

		fprintf (fp, "Item Count: %d\n\n", tempPtr->item_count);

	fprintf
		(fp, "Item   Description         Product ID            Media        Qty  Cost\n");
	fprintf
		(fp, "=======================================================================================\n\n");

		itemPtr = tempPtr->itemList;
		while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
		{
			/* map description - process_type */ 
			if (itemPtr->process_type > 0)
			{
				k = 0;
				while ((k < glbData.process_type_count) && 
							 (itemPtr->process_type != glbData.process_type[k].item_id)) 
							k++;

				if (k < glbData.process_type_count)
				{
					strcpy (process_type, glbData.process_type[k].item_name);
				}
				else
				{
					/* did not find the matching process_type id */
					/* changed from blank string to NA - oct 95 */
					strcpy (process_type, "N/A");
				}
			}
			else
			{
				strcpy (process_type, "N/A");
			}

			/* map media_type */
			if (itemPtr->media_type)
			{
				k = 0;
				while ((k < glbData.media_type_count) && 
							 (itemPtr->media_type != glbData.media_type[k].item_id)) 
							k++;

				if (k < glbData.media_type_count)
				{
				  strcpy (media_type, glbData.media_type[k].item_name);
				}
				else
				{
					/* did not find the matching media_type id */
				  strcpy (media_type, "N/A");
				}
			}
			else
			{
			  strcpy (media_type, "N/A");
			}

			if (itemPtr->granule_name[0] != '\0')
			{
			  strcpy (granule_name, itemPtr->granule_name);
			}
			else
			{
				strcpy (granule_name, "N/A");
			}

			fprintf	(fp, "%4d   %-20.20s%-16.16s      %-11.11s  %-3d  %.1f\n",
			 itemPtr->item_id, process_type, granule_name, media_type, 
			 itemPtr->quantity, itemPtr->cost);

			itemPtr = itemPtr->next;
		}

		fprintf (fp, "\f");
		tempPtr = tempPtr->next;
	}

	/* close file, clean up */
	fclose (fp);
	
	/*
	** 07/12/96 - do not print automatically - David Cuddy.
	** sprintf (cmdbuf, "lpr %s", file);
	** system (cmdbuf);
	*/

	/* Change cursor back to normal */
	timeOutCursors (False);

	XtFree (file);

	/* Destroy the file selection dialog and reset glbData.fileSelectDlg */
	XtDestroyWidget (wgt);
	glbData.fileSelectDlg = (Widget) NULL;

}

/*===========================================================================*
** 
** Function Name: order_save_results_cancelCb
**
** Description:		callback function for the cancel button in the file
**                selection dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void	order_save_results_cancelCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	XtDestroyWidget (wgt);
	glbData.fileSelectDlg = (Widget) NULL;
}


/*===========================================================================*
** 
** Function Name: order_restart_itemCb
**
** Description:		callback function for Item Function Restart Item
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value:	None
** 
** Revision History: 
**
**==========================================================================*/

void	order_restart_itemCb( 
	Widget widget, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCorder               *UxSaveCtx, *UxContext;
	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_ORDER_LIST *orderPtr;
	char Msg[IMS_COL512_LEN+1];
	int status, modifyFlag;
	int i, len, itemCount;
	DBSMALLINT current_status;
	DBCHAR op_validate_p;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;


	UxSaveCtx = UxOrderContext;
	UxOrderContext = UxContext =

			(_UxCorder *) UxGetContext( widget );
	{

	/* change cursor to timeout cursor */
	timeOutCursors (True);

	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	orderPtr = clientData->currOrder;
	itemPtr = clientData->currOrder->itemList;
	itemCount = clientData->currOrder->item_count;
	modifyFlag = 0;


	/* Find the first selected item in the item list */
	i = 0;
	while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < itemCount) && 
			(itemPtr->selectFlag == 0))
	{
		i++;
		itemPtr = itemPtr->next;
	}

	if ((itemPtr == (OP_ORDER_ITEM_LIST *)NULL) || (i >= itemCount))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.orderW, IMS_INFO, Msg); 

		/* change cursor back to normal */
		timeOutCursors (False);
		return;
	}

	/*
	** Found the first selected item, let's verify the order status,
	** restart item is not allowed for orders with the following
	** status values: COMPLETE, CANCEL, CANCELLED.
	** If order status is qualified, then we proceed with the restart.
	*/
	
	/*
	** Begin transacation
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Display error messages */
		sprintf(Msg, "Internal Error: Restart Item,\n"
					 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 

		/* change cursor back to normal */
		timeOutCursors (False);
		return;
	}


	/*
	** Lock the order_queue table
	*/
	if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Restart Item,\n" 
								 "OP_GETORDERLOCK failed for Order ID: %d\n", 
								 orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 

		/* change cursor back to normal */
		timeOutCursors (False);
		return;
	}

		
	/*
	** call CAT event OP_GETORDERSTATUS to get order's current status
	*/
	catReq->item[0] = (DBINT *)&orderPtr->order_id;
	catReq->item[1] = (DBSMALLINT *)&current_status;
	if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Restart Item,\n"
									"OP_GETORDERSTATUS failed for Order ID: %d.\n",
									orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 

		/* change cursor back to normal */
		timeOutCursors (False);
		return;
	}


	/*
	** if order status is one of the following values, 
	** Roll Back the transaction, then popup error
	** message dialog box, return.
	*/
	if ((current_status == ORDER_COMPLETE) || 
			(current_status == ORDER_CANCELLED) ||
			(current_status == ORDER_CANCEL))
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf (Msg,"Restart Item:\n\n"
								 "Restart Item function is not allowed on "
								 "orders with the following status values:\n"
								 "COMPLETE, CANCELLED, CANCEL.\n\n"
								 "Order: %d does not qualify for this operation.\n",
								 orderPtr->order_id);

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 

		/* change cursor back to normal */
		timeOutCursors (False);
		return;
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
		/* Display error messages */
		sprintf(Msg, "Internal Error: Restart Item,\n"
					 	"OP_COMMITTRANSACTION failed for Order ID: %d.\n", 
					 	orderPtr->order_id);   

		msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, Msg); 

		/* change cursor back to normal */
		timeOutCursors (False);
		return;
	}


	/*
	** Let's go through the item list, for each item selected, 
	** verify that the item status is ERROR.  If the item status
	** is not ERROR or FAILED, restart is not allowed.  If the item status
	** is ERROR or FAILED, call CAT function OP_RESTARTITEM to rollback
	** the item status to NEW, validated_p flag to N, process_status 
	** to 0, step_name to NULL, step_sequence to 0 and step_started_p
	** to N.
	*/

	for ( i = 0, itemPtr = clientData->currOrder->itemList
			; (itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (i < itemCount)
			; itemPtr = itemPtr->next, i++ )
	{
		if (itemPtr->selectFlag)
		{
			/*
			** Begin the update transaction 
			*/
			if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
			{
				/* Display error messages */
				sprintf(new_msg,
								"Internal Error:\nRestart Item,\n"
								"OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
								itemPtr->order_id, itemPtr->item_id);   
	
				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;

			}


			/*
			** Lock the order_item table 
			*/
			if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(new_msg,
							  "Internal Error:\nRestart Item,\n" 
							  "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
								itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}


			/* 
			** Restarting an item is only allowed for item with status
			** value ERROR.
			*/
			catReq->item[0] = (DBINT *)&itemPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&current_status;
			if ((status = ims_opCat(catReq, OP_GETITEMSTATUS)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(new_msg,
							 "OP_GETITEMSTATUS failed for Order: %d, Item: %d\n", 
							 itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}


			/*
			** continue to the next item if current item status 
			** does not qualify this operation.
			*/
			if (current_status != ITEM_ERROR && current_status != ITEM_FAILED)
			{
				/* release lock, rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(new_msg,
					 "Restart Item is only allowed on items\n"
					 "with status value ERROR or FAILED:\n"
					 "Order: %d, Item: %d does not qualify this operation.\n\n", 
					 itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				continue;
			}


			/*
			** call CAT function restartItem 
			** 11-13-96 add account_id to support PR2184
			*/
			catReq->item[0] = (DBINT *)&itemPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
			catReq->item[2] = (DBCHAR *)orderPtr->account_id;
			catReq->item[3] = (DBCHAR *)&op_validate_p;
			if ((status = ims_opCat (catReq, OP_RESTARTITEM)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(new_msg, 
							"Internal Error:\nRestart Item,\n"
							"OP_RESTARTITEM failed for Order: %d, Item: %d\n",
							itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}


			/*
			** Commit transaction 
			*/
			if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(new_msg, 
								"Internal Error:\nRestart Item,\n"
							  "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n",
								itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}


			/*
			** restart has been successful, let's update the following
			** fields of the item in memory: status = ITEM_NEW, 
			** validated_p = N, process_status = 0, 
			** step_name = NULL, step_sequence = 0,
			** step_started_p = N.
			*/
			modifyFlag = 1;
			if (op_validate_p == 'Y')
			{
				itemPtr->status = ITEM_NEW;
				itemPtr->validated_p = 'N';
			}
			else
			{
				itemPtr->status = ITEM_VALIDATED;
				itemPtr->validated_p = 'Y';
			}
			itemPtr->process_status = ITEM_NO_PPS_STATUS;
			itemPtr->step_name[0] = '\0';
			itemPtr->step_sequence = 0;
			itemPtr->step_started_p = 'N';

		} /* if (selectFlag) */

	}  /* for */


	if (modifyFlag)
	{
		/*
		** Refresh item lists 
		*/
		scroll_cbs = (XmScrollBarCallbackStruct *)
						malloc(sizeof(XmScrollBarCallbackStruct));
		scroll_cbs->value = clientData->itemWindowTop;
			order_scroll_itemListsCb (glbData.orderW, NULL, scroll_cbs);  
		free (scroll_cbs);

		/*
		** get new order status 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(new_msg, "Internal Error: Get New Order Status, "
						 	"OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
						 	orderPtr->order_id);   

			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}

		}
		else
		if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Display error messages */
				sprintf(new_msg, "Internal Error: Get New Order Status, " 
								"OP_GETORDERLOCK failed for Order ID: %d\n", 
								orderPtr->order_id);   
	
			if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
			{
				/* memory allocation failed, clean up, return */
				OP_FREECHARPTR(concat_msg.Msg);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return;
			}
		}
		else
		{
			catReq->item[0] = (DBINT *)&orderPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&orderPtr->status;
			if ((status = ims_opCat (catReq, OP_GETORDERSTATUS)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(new_msg, "Internal Error: Get New Order Status, "
								"OP_GETORDERSTATUS failed for Order ID: %d.\n",
								orderPtr->order_id);   
	
				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}
			}
			else
			{
				/*
				** Get Order Item Stats
				*/
				catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
				if ((status = ims_opCat (catReq, OP_GETORDERITEMSTATS)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(new_msg, "Internal Error: Restart Item, "
								 "OP_GETORDERITEMSTATS failed for Order ID: %d.\n",
								 orderPtr->order_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

				}
				else
				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg, "Internal Error: Get New Order Status, "
								  "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
								  orderPtr->order_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

				}
				else
				{
					/*
					** refresh order lists 
					*/
					scroll_cbs = (XmScrollBarCallbackStruct *)
								malloc(sizeof(XmScrollBarCallbackStruct));
					scroll_cbs->value = clientData->orderWindowTop;
					order_scroll_orderListsCb (glbData.orderW, NULL, scroll_cbs);  
					free (scroll_cbs);

				}
			}
		}
	} /* if (modifyFlag) */


	/* Display all messages in browse dialog box */
	if (concat_msg.Msg != NULL)
	{
		if ((len = strlen(concat_msg.Msg)) > 1024)
		{
			strcpy (label, "Restart  Item  Message  Box");
			browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
		}
		else
		{
			msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
		}
	}

	/* free up concat_msg space allocated */
	OP_FREECHARPTR(concat_msg.Msg);

	/* change cursor back to normal */
	timeOutCursors (False);
	
	}
	UxOrderContext = UxSaveCtx;

}


