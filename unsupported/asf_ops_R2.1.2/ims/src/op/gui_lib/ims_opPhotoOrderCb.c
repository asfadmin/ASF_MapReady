static char *sccs = "@(#)ims_opPhotoOrderCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opPhotoOrder.c

	Function:	Callback functions for Photo Products Order Screen

	Author:		Jennifer Ting

	Date:			5/1995

	Revision: 6/10/1996 - Modified functions photoOrder_closeCb and 
												photoOrder_clearCb to correct PR 942.

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

#define _IMS_OP_PHOTOORDERCB_C
#include "ims_opCb.h"

#define PHOTO_ROWS 24
#define PHOTO_JOB_ROWS 22

static int queueWindowTop = 0;
static int jobWindowTop = 0;

static void free_photo_selectList();
static void free_photo_queueList();

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opPhotoOrder.h>
#undef CONTEXT_MACRO_ACCESS

#include <ims_opPhotoJob.h>

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
** 
** Function Name:	photoOrder_photoType_validsCb 
**
** Description:		Callback function for the Photo Type List button,
**								pops up the valids selection dialog.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_photoType_validsCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{
		selectionDlg_popupCb (glbData.photoOrderW, (Widget)photoTypeTF, PHOTO_TYPE);
	}
	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: photoOrder_scroll_queueListsCb
**
** Description:		Updates the item list widgets (5 of them) with new items
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

void	photoOrder_scroll_queueListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCphotoOrder               *UxSaveCtx, *UxContext;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	int photoQCount, count, memory;
	int i, temp, j, k, highLightPos[PHOTO_ROWS];
	Widget sbar;
	XmStringTable userIdStr, orderIdStr, itemIdStr, productIdStr, qtyStr;
	char buffer[IMS_COL255_LEN+1];
	int slider;


	/* assign client to photoClientData from glbData structure */
	clientData = &(glbData.photoClientData);
	photoQPtr = clientData->photoQueueList;
	photoQCount = clientData->photoQueueCount;
	memory = 1;
	j = 0;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext(glbData.photoOrderW);
	{

		queueWindowTop = cbs->value;
		count = photoQCount - queueWindowTop;

		if (count > PHOTO_ROWS)
			count = PHOTO_ROWS;


		/* If no rows to display, clean up photo queue lists */
		if ((photoQCount == 0) || (photoQPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (userIdSL1);
			XmListDeleteAllItems (orderIdSL1);
			XmListDeleteAllItems (itemIdSL1);
			XmListDeleteAllItems (productIdSL1);
			XmListDeleteAllItems (qtySL1);

			/* Update Scrollbar position */
			XtVaGetValues(dummySW1, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, queueWindowTop,
				XmNsliderSize, 1, 
				NULL);

			XtSetSensitive (addPB, False);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		userIdStr 		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		orderIdStr 		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		itemIdStr 		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		productIdStr	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		qtyStr 				= (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && userIdStr && orderIdStr && itemIdStr && 
						 productIdStr && qtyStr; 

		if (!memory)
		{
			/* Display error messages in message window */

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (userIdStr[i]);
				XmStringFree (orderIdStr[i]);
				XmStringFree (itemIdStr[i]);
				XmStringFree (productIdStr[i]);
				XmStringFree (qtyStr[i]);
			}

			XtFree ((char *)userIdStr);
			XtFree ((char *)orderIdStr);
			XtFree ((char *)itemIdStr);
			XtFree ((char *)productIdStr);
			XtFree ((char *)qtyStr);
			return;
		}

	
		while ((photoQPtr != NULL) && (photoQPtr->position != queueWindowTop))
		{
			photoQPtr = photoQPtr->next;
		}
		
		for (i = 0; i < count && photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL; i++)
		{
			if (photoQPtr->user_id[0] != '\0')
				userIdStr[i] = XmStringCreateLocalized(photoQPtr->user_id);
			else 
				userIdStr[i] = XmStringCreateLocalized("");

			if (photoQPtr->order_id)
			{
				sprintf (buffer, "%d", photoQPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				orderIdStr[i] = XmStringCreateLocalized("");

			if (photoQPtr->item_id)
			{
				sprintf (buffer, "%d", photoQPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				itemIdStr[i] = XmStringCreateLocalized("");

			if (photoQPtr->product_id[0] != '\0')
				productIdStr[i] = XmStringCreateLocalized(photoQPtr->product_id);
			else
			{	
				/* No p_granule_name found */
				strcpy (buffer, "N/A");
				productIdStr[i] = XmStringCreateLocalized(buffer);
			}

			if (photoQPtr->quantity >= 0)
			{
				sprintf (buffer, "%d", photoQPtr->quantity);
				qtyStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				qtyStr[i] = XmStringCreateLocalized("");


			if (photoQPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			photoQPtr = photoQPtr->next;

		}

		/* Load all the synchronized arrays to photo queue list widgets */

		XtVaSetValues(
				userIdSL1, XmNitems, userIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				orderIdSL1, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemIdSL1, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				productIdSL1, XmNitems, productIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				qtySL1, XmNitems, qtyStr, XmNitemCount, count, NULL);

		/* Free compound strings */
		while (--i)
		{
			XmStringFree (userIdStr[i]);
			XmStringFree (orderIdStr[i]);
			XmStringFree (itemIdStr[i]);
			XmStringFree (productIdStr[i]);
			XmStringFree (qtyStr[i]);
		}

		XtFree ((char *)userIdStr);
		XtFree ((char *)orderIdStr);
		XtFree ((char *)itemIdStr);
		XtFree ((char *)productIdStr);
		XtFree ((char *)qtyStr);
	
		/* Update Scrollbar position */
		XtVaGetValues(dummySW1, XmNverticalScrollBar, &sbar, NULL);

		temp = photoQCount;
		if (photoQCount > PHOTO_ROWS)
			slider = PHOTO_ROWS;
		else
			slider = photoQCount;
		
		if (photoQCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, photoQCount,
				XmNvalue, queueWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, queueWindowTop,
				XmNsliderSize, 1, 
				NULL);

		/* Set selectionPolicy of queue list widgets to MultipleSelect */
		XmListDeselectAllItems(userIdSL1);
		XtVaSetValues(userIdSL1, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(orderIdSL1);
		XtVaSetValues(orderIdSL1, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(itemIdSL1);
		XtVaSetValues(itemIdSL1, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(productIdSL1);
		XtVaSetValues(productIdSL1, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(qtySL1);
		XtVaSetValues(qtySL1, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		while (j--)
		{
			XmListSelectPos(userIdSL1, highLightPos[j], False);
			XmListSelectPos(orderIdSL1, highLightPos[j], False);
			XmListSelectPos(itemIdSL1, highLightPos[j], False);
			XmListSelectPos(productIdSL1, highLightPos[j], False);
			XmListSelectPos(qtySL1, highLightPos[j], False);
		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		XtVaSetValues(userIdSL1, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(orderIdSL1, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(itemIdSL1, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(productIdSL1, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(qtySL1, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
	}

	UxPhotoOrderContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: photoOrder_queueLists_selectionCb
**
** Description:		Updates the selection of the item(s) in all the photo
**								queue list widgets (5 of them), synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each list widget is associated with
** 									 							a unique number, for example, list widget
**																userIdSL1 has listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void photoOrder_queueLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCphotoOrder               *UxSaveCtx, *UxContext;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr, *tempPtr;
	int listNo;
	int i, k, count, photoQCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;


	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( widget );
	{
		clientData = &(glbData.photoClientData);
		photoQPtr = clientData->photoQueueList;
		photoQCount = clientData->photoQueueCount;
		listNo = (int)listNumber;
		selectedItemPos = cbs->selected_item_positions;
		itemPosition = cbs->item_position;
		totalItemsSelected = cbs->selected_item_count;
		

		/* Set selectionPolicy of item list widgets to MultipleSelect */
		if(listNo != 1)
		{
			XmListDeselectAllItems(userIdSL1);
			XtVaSetValues(userIdSL1, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(orderIdSL1);
			XtVaSetValues(orderIdSL1, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(itemIdSL1);
			XtVaSetValues(itemIdSL1, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(productIdSL1);
			XtVaSetValues(productIdSL1, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 5)
		{
			XmListDeselectAllItems(qtySL1);
			XtVaSetValues(qtySL1, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}


		/* Locate screen items in photo queue lists */
		count = photoQCount - queueWindowTop;

		if (count > PHOTO_ROWS)
			count = PHOTO_ROWS;

		while ((photoQPtr != NULL) && (photoQPtr->position != queueWindowTop))
		{
			photoQPtr = photoQPtr->next;
		}

		/* Reset the selectFlag for each item first */
		tempPtr = photoQPtr;
		for (i = 0; i < count && tempPtr != NULL; i++)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		for (i = 0; i < totalItemsSelected; i++)
		{
			if (listNo != 1)
				XmListSelectPos(userIdSL1, selectedItemPos[i], False);
			if (listNo != 2)
				XmListSelectPos(orderIdSL1, selectedItemPos[i], False);
			if (listNo != 3)
				XmListSelectPos(itemIdSL1, selectedItemPos[i], False);
			if (listNo != 4)
				XmListSelectPos(productIdSL1, selectedItemPos[i], False);
			if (listNo != 5)
				XmListSelectPos(qtySL1, selectedItemPos[i], False);


			/* Locate selected items in queue lists to set the selectFlag */
			tempPtr = photoQPtr;
			for (k = 0;  (k < selectedItemPos[i]-1) && (tempPtr != NULL); k++)
				tempPtr = tempPtr->next;

			tempPtr->selectFlag = 1;

		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		if(listNo != 1)
		{
			XtVaSetValues(userIdSL1, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(orderIdSL1, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(itemIdSL1, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XtVaSetValues(productIdSL1, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 5)
		{
			XtVaSetValues(qtySL1, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}

	}

	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoOrder_searchCb 
**
** Description:		Callback function for the search push button,
**								search for items with photo type specified.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_searchCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *value;
	char Msg[IMS_COL1024_LEN+1];
	char query[IMS_COL255_LEN+1];
	char buffer[IMS_COL10_LEN+1];
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	OP_CAT_STRUCT *catReq;
	int i, status;
	int photo_type_id;
	int photoCount;
	short int p_dataset_idx;
	int p_granule_idx;
	char granules_table[IMS_COL30_LEN+1];
	XmScrollBarCallbackStruct *cbs;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{
		value = XmTextFieldGetString (photoTypeTF);
		if (!*value)
		{
			/* Display error messages */
			strcpy(Msg, "Please specify a photo processing type."); 
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_INFO, Msg); 
	
			XtFree (value);
			return; 
		}
		else
		{

			/*
			** Find the matching photo_type_id for the priority string 
			*/
			i = 0;
			while ((i < glbData.photo_type_count) &&
						 (strcmp (value, glbData.photo_type[i].item_name) != 0))
					i++;

			if (i < glbData.photo_type_count)
			{
				photo_type_id = glbData.photo_type[i].item_id;
			}
			else 
			{
				/* Display error messages */
				strcpy(Msg, "Photo processing type is not valid."); 
				msgBoxDlg_popupCb (glbData.photoOrderW, IMS_ERROR, Msg); 
	
				XtFree (value);
				return; 
			}


			/* Change cursor to watch cursor */
			timeOutCursors (True);

			/*
			** Initialize catalog request structure 
			*/
			clientData = &(glbData.photoClientData);
			
			/* clean up the current photo_queue_list first */
			(void)free_photo_queueList();

			catReq = &(clientData->catReq);
			catReq->item[0] = (int *)&photoCount;

			/* display p_granule_name from order_item table */
			sprintf (query,
							 "select t1.order_id, t1.item_id, t1.photo_type, "
							 "t1.photojob_id, t1.quantity, t1.quality, t1.status, "
							 "t1.op_comment, t2.user_id, t3.p_granule_name "
							 "from photo_queue t1, order_queue t2, order_item t3 "
							 "where t1.photo_type = %d and "
							 "t1.photojob_id = NULL and "
							 "t1.status = 1 and "
							 "t1.order_id = t2.order_id and "
							 "t3.order_id = t1.order_id and "
							 "t3.item_id = t1.item_id "
							 "order by t1.order_id, t1.item_id", photo_type_id);

			catReq->item[1] = (char *)query;
			
			if ((status = ims_opCat (catReq, OP_GETPHOTOQUEUELIST)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				sprintf(Msg, "Internal Error: photo queue retrieval failed.");   
				msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

				XtFree (value);
				return;
			}
			else
			{
				/* assign returned photo_queue to opPhotoClientData */
				photoQPtr = (OP_PHOTO_QUEUE_LIST *)catReq->item[2]; 
				clientData->photoQueueCount = *(int *)catReq->item[0];
				clientData->photoQueueList = (OP_PHOTO_QUEUE_LIST *)catReq->item[2];
			}


			if ((photoQPtr == (OP_PHOTO_QUEUE_LIST *)NULL) ||  
					(photoCount <= 0))
			{
				/* No items to display, desensitize ADD, COMMENT,
				** CREATE, DELETE and CANCEL buttons.
				*/
				XtSetSensitive (addPB, False); 
				XtSetSensitive (createPB, False); 
				XtSetSensitive (deletePB, False); 
				XtSetSensitive (cancelPB, False); 
			}
			else
			{
				/* Initialize selectFlag for each item */
				photoQPtr = clientData->photoQueueList;
				while (photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
				{
					/* initialize select flag */
					photoQPtr->selectFlag = 0;

					/* call get_productID to get product Id */ 
					/*
					** This is not necessary anymore - schema version 3.30
					if ((status = get_productID(glbData.photoOrderW, photoQPtr)) < IMS_OK)
					{
						timeOutCursors (False);
						return;
					}
					*/
					photoQPtr = photoQPtr->next;
				}

				/* sensitize ADD button */
				XtSetSensitive (addPB, True);

			}

			/* display data in the scroll lists */
			cbs = (XmScrollBarCallbackStruct *)
				malloc(sizeof(XmScrollBarCallbackStruct));
			cbs->value = 0;
			photoOrder_scroll_queueListsCb (wgt, NULL, cbs);
			free (cbs);

			/* display photo queue item count */
			sprintf (buffer, "%d", photoCount);
			XmTextFieldSetString (totalItemsTF, buffer);
	
			/* Change cursor back to normal */
			timeOutCursors (False);
		}
	}
	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoOrder_clearCb 
**
** Description:		Callback function for the clear push button,
**								clears up the list and text widgets.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - pass glbData.photoOrderW to 
**                   photoOrder_scroll_queueListsCb and photoOrder_cancelCb()
**
**==========================================================================*/

void	photoOrder_clearCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *value;
	int slider;
	Widget sbar;
	char Msg[IMS_COL1024_LEN+1];
	XmScrollBarCallbackStruct *cbs;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( glbData.photoOrderW );
	{
	
		/* free up clientdata->photoQList */
		free_photo_queueList();

		/* call photoOrder_scroll_queueListsCb to clean up Queue lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		photoOrder_scroll_queueListsCb (glbData.photoOrderW, NULL, cbs);
		free (cbs);

		XmTextFieldSetString (photoTypeTF, "");
		XmTextFieldSetString (totalItemsTF, "");
		XtSetSensitive (searchPB, True);

		/* call cancel callback to clean up the job window */
		photoOrder_cancelCb (glbData.photoOrderW, cd, cb);

	}
	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoOrder_closeCb 
**
** Description:		Callback function for the CLOSE button,
**								pops down the photo order screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - took out photoOrder context assignment
**
**==========================================================================*/

void	photoOrder_closeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	queueWindowTop = 0;
	jobWindowTop = 0;

	/* call photoOrder_clearCb to clear up the widgets and free
	** up data structure
	*/
	(void) photoOrder_clearCb (glbData.photoOrderW, NULL, NULL);

	XtPopdown (XtParent(glbData.photoOrderW));
	glbData.photoOrderFlag = 0;
}


/*===========================================================================*
** 
** Function Name:	photoOrder_printCb 
**
** Description:		Callback function for the PRINT button,
**								prints the photo order screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_printCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.photoOrderW);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxPhotoOrderContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	photoOrder_addCb 
**
** Description:		Callback function for the ADD button,
**								adds the selected items from photo queue lists to 
**								the job list widgets.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_addCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *qPtr, *photoQPtr;
	OP_PHOTO_QUEUE_LIST *sPtr, *nPtr, *tempPtr;
	char Msg[IMS_COL1024_LEN+1];
	int sCount;
	XmScrollBarCallbackStruct *cbs;


	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{

		clientData = &(glbData.photoClientData);
		photoQPtr = qPtr = clientData->photoQueueList;
		sPtr = nPtr = tempPtr = (OP_PHOTO_QUEUE_LIST *)NULL;

		/* find out how many items are selected */
		sCount = 0;
		while (qPtr != (OP_PHOTO_QUEUE_LIST *)NULL) 
		{
			if (qPtr->selectFlag)
			{
				/* check for duplicate copies in select list */
				
				sCount++;   
				sPtr = clientData->photoQSelectList;
				if (sPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
				{	
					/* Find the right place in the select list to insert */
					while ((sPtr->next != (OP_PHOTO_QUEUE_LIST *)NULL) &&
								 ((qPtr->order_id > sPtr->order_id)	|| 
									((qPtr->order_id == sPtr->order_id)&& 
									 (qPtr->item_id > sPtr->item_id))))
					{
						sPtr = sPtr->next;
					}
					if ((qPtr->order_id == sPtr->order_id) &&
							(qPtr->item_id == sPtr->item_id))
					{
						/* item already exists in select list */
						qPtr = qPtr->next;
						continue;
					}
				}

				/* we have a new item to add to the select list */
				if ((nPtr = malloc(sizeof(OP_PHOTO_QUEUE_LIST))) == NULL)
				{
					/* malloc failed, display message and return */
					strcpy(Msg, "Memory allocation failed.");
					msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

					/* Change cursor back to normal */
					timeOutCursors (False);
					return; 
				}
				else
				{
					nPtr->prev = (OP_PHOTO_QUEUE_LIST *)NULL;
					nPtr->next = (OP_PHOTO_QUEUE_LIST *)NULL;
					nPtr->order_id = qPtr->order_id;
					nPtr->item_id = qPtr->item_id;
					nPtr->photojob_id = qPtr->photojob_id;
					nPtr->photo_type = qPtr->photo_type;
					nPtr->quantity = qPtr->quantity;
					nPtr->status = qPtr->status;
					nPtr->selectFlag = 0;
					nPtr->position = 0;
					strcpy (nPtr->op_comment, qPtr->op_comment);
					strcpy (nPtr->user_id, qPtr->user_id);
					strcpy (nPtr->product_id, qPtr->product_id);

					
					/* insert new item into the select list */
					if (sPtr == (OP_PHOTO_QUEUE_LIST *)NULL)
					{
						clientData->photoQSelectList = nPtr;
					}
					else
					{
						if ((nPtr->order_id < sPtr->order_id) ||
								((nPtr->order_id == sPtr->order_id) &&
								 (nPtr->item_id < sPtr->item_id)))
						{
							/* insert before */
							tempPtr = sPtr->prev;
							sPtr->prev = nPtr;
							nPtr->prev = tempPtr;
							nPtr->next = sPtr;


							if (tempPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
								tempPtr->next = nPtr;
							else 
								clientData->photoQSelectList = nPtr;

						}
						else
						{
							/* insert after */
							tempPtr = sPtr->next;
							sPtr->next = nPtr;
							nPtr->prev = sPtr;
							nPtr->next = tempPtr;
							if (tempPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
								tempPtr->prev = nPtr;
						}
					}
				}
			}

			qPtr = qPtr->next;

		}

		if (sCount <= 0) 
		{
			/* nothing has been selected, display message and return */
			strcpy(Msg, "No item has been selected.");
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_INFO, Msg); 

			/* Change cursor back to normal */
			timeOutCursors (False);
			return; 
		}
		else
		{
			/* update position for each item in the photoQSelectList */
			tempPtr = clientData->photoQSelectList;
			sCount = 0;
			while (tempPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
			{
				tempPtr->position = sCount++;
				tempPtr->selectFlag = 0;
				tempPtr = tempPtr->next;
			}
			clientData->photoQSelectCount = sCount;

		}

		/* display data in the scroll lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		photoOrder_scroll_jobListsCb (wgt, NULL, cbs);
		free (cbs);

		/* desensitize search widget */
		XtSetSensitive (searchPB, False); 

	}
	UxPhotoOrderContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: photoOrder_scroll_jobListsCb
**
** Description:		Updates the job list widgets (4 of them) with new items
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

void	photoOrder_scroll_jobListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCphotoOrder               *UxSaveCtx, *UxContext;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *sPtr;
	int photoQSelectCount, count, memory;
	int i, temp, j, k, highLightPos[PHOTO_ROWS];
	Widget sbar;
	XmStringTable orderIdStr, itemIdStr, productIdStr, qtyStr;
	char buffer[IMS_COL255_LEN+1];
	int slider;


	/* assign client to photoClientData from glbData structure */
	clientData = &(glbData.photoClientData);
	sPtr = clientData->photoQSelectList;
	photoQSelectCount = clientData->photoQSelectCount;
	memory = 1;
	j = 0;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext(glbData.photoOrderW);
	{

		jobWindowTop = cbs->value;
		count = photoQSelectCount - jobWindowTop;

		if (count > PHOTO_JOB_ROWS)
			count = PHOTO_JOB_ROWS;


		/* If no rows to display, clean up photo queue lists */
		if ((photoQSelectCount == 0) || (sPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (orderIdSL2);
			XmListDeleteAllItems (itemIdSL2);
			XmListDeleteAllItems (productIdSL2);
			XmListDeleteAllItems (qtySL2);

			/* Update Scrollbar position */
			XtVaGetValues(dummySW2, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, jobWindowTop,
				XmNsliderSize, 1, 
				NULL);

			/* desensitize create, delete and cancel widgets */
			XtSetSensitive (createPB, False);
			XtSetSensitive (deletePB, False);
			XtSetSensitive (cancelPB, False);

			/* clean up text widgets */
			XmTextFieldSetString (jobIdTF, "");
			XmTextFieldSetString (workOrderTF, "");
			XmTextFieldSetString (totalPrintsTF, "");
			XmTextFieldSetString (orderDateTF, "");
			XmTextFieldSetString (totalCostTF, "");

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		orderIdStr 		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		itemIdStr 		= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		productIdStr	= (XmStringTable)XtMalloc(count *sizeof(XmString *));
		qtyStr 				= (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && orderIdStr && itemIdStr && 
						 productIdStr && qtyStr; 

		if (!memory)
		{
			/* Display error messages in message window */

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (orderIdStr[i]);
				XmStringFree (itemIdStr[i]);
				XmStringFree (productIdStr[i]);
				XmStringFree (qtyStr[i]);
			}

			XtFree ((char *)orderIdStr);
			XtFree ((char *)itemIdStr);
			XtFree ((char *)productIdStr);
			XtFree ((char *)qtyStr);
			return;
		}

	
		while ((sPtr != NULL) && (sPtr->position != jobWindowTop))
		{
			sPtr = sPtr->next;
		}
		
		for (i = 0; i < count && sPtr != (OP_PHOTO_QUEUE_LIST *)NULL; i++)
		{

			if (sPtr->order_id)
			{
				sprintf (buffer, "%d", sPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				orderIdStr[i] = XmStringCreateLocalized("");

			if (sPtr->item_id)
			{
				sprintf (buffer, "%d", sPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				itemIdStr[i] = XmStringCreateLocalized("");

			if (sPtr->product_id[0] != '\0')
				productIdStr[i] = XmStringCreateLocalized(sPtr->product_id);
			else
			{
				/* No p_granule_name found */
				strcpy (buffer, "N/A");
				productIdStr[i] = XmStringCreateLocalized(buffer);
			}

			if (sPtr->quantity >= 0)
			{
				sprintf (buffer, "%d", sPtr->quantity);
				qtyStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				qtyStr[i] = XmStringCreateLocalized("");


			if (sPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			sPtr = sPtr->next;

		}

		/* Load all the synchronized arrays to photo job list widgets */

		XtVaSetValues(
				orderIdSL2, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				itemIdSL2, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				productIdSL2, XmNitems, productIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				qtySL2, XmNitems, qtyStr, XmNitemCount, count, NULL);

		/* Free compound strings */
		while (--i)
		{
			XmStringFree (orderIdStr[i]);
			XmStringFree (itemIdStr[i]);
			XmStringFree (productIdStr[i]);
			XmStringFree (qtyStr[i]);
		}

		XtFree ((char *)orderIdStr);
		XtFree ((char *)itemIdStr);
		XtFree ((char *)productIdStr);
		XtFree ((char *)qtyStr);
	
		/* Update Scrollbar position */
		XtVaGetValues(dummySW2, XmNverticalScrollBar, &sbar, NULL);

		temp = photoQSelectCount;
		if (photoQSelectCount > PHOTO_JOB_ROWS)
			slider = PHOTO_JOB_ROWS;
		else
			slider = photoQSelectCount;
		
		if (photoQSelectCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, photoQSelectCount,
				XmNvalue, jobWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, jobWindowTop,
				XmNsliderSize, 1, 
				NULL);

		/* Set selectionPolicy of job list widgets to MultipleSelect */
		XmListDeselectAllItems(orderIdSL2);
		XtVaSetValues(orderIdSL2, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(itemIdSL2);
		XtVaSetValues(itemIdSL2, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(productIdSL2);
		XtVaSetValues(productIdSL2, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(qtySL2);
		XtVaSetValues(qtySL2, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);
	
		while (j--)
		{
			XmListSelectPos(orderIdSL2, highLightPos[j], False);
			XmListSelectPos(itemIdSL2, highLightPos[j], False);
			XmListSelectPos(productIdSL2, highLightPos[j], False);
			XmListSelectPos(qtySL2, highLightPos[j], False);
		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		XtVaSetValues(orderIdSL2, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(itemIdSL2, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(productIdSL2, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(qtySL2, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);

		/* sensitize create and delete widgets */
		XtSetSensitive (createPB, True);
		XtSetSensitive (deletePB, True);
		XtSetSensitive (cancelPB, True);

	}

	UxPhotoOrderContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: photoOrder_jobLists_selectionCb
**
** Description:		Updates the selection of the item(s) in all the photo
**								job list widgets (4 of them), synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each list widget is associated with
** 									 							a unique number, for example, list widget
**																orderIdSL2 has listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void photoOrder_jobLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCphotoOrder               *UxSaveCtx, *UxContext;
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *sPtr, *tempPtr;
	int listNo;
	int i, k, count, sCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;


	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( widget );
	{
		clientData = &(glbData.photoClientData);
		sPtr = clientData->photoQSelectList;
		sCount = clientData->photoQSelectCount;
		listNo = (int)listNumber;
		selectedItemPos = cbs->selected_item_positions;
		itemPosition = cbs->item_position;
		totalItemsSelected = cbs->selected_item_count;
		

		/* Set selectionPolicy of item list widgets to MultipleSelect */
		if(listNo != 1)
		{
			XmListDeselectAllItems(orderIdSL2);
			XtVaSetValues(orderIdSL2, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(itemIdSL2);
			XtVaSetValues(itemIdSL2, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(productIdSL2);
			XtVaSetValues(productIdSL2, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XmListDeselectAllItems(qtySL2);
			XtVaSetValues(qtySL2, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}


		/* Locate screen items in photo queue lists */
		count = sCount - jobWindowTop;

		if (count > PHOTO_JOB_ROWS)
			count = PHOTO_JOB_ROWS;

		while ((sPtr != NULL) && (sPtr->position != jobWindowTop))
		{
			sPtr = sPtr->next;
		}

		/* Reset the selectFlag for each item first */
		tempPtr = sPtr;
		for (i = 0; i < count && tempPtr != NULL; i++)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		for (i = 0; i < totalItemsSelected; i++)
		{
			if (listNo != 1)
				XmListSelectPos(orderIdSL2, selectedItemPos[i], False);
			if (listNo != 2)
				XmListSelectPos(itemIdSL2, selectedItemPos[i], False);
			if (listNo != 3)
				XmListSelectPos(productIdSL2, selectedItemPos[i], False);
			if (listNo != 4)
				XmListSelectPos(qtySL2, selectedItemPos[i], False);


			/* Locate selected items in queue lists to set the selectFlag */
			tempPtr = sPtr;
			for (k = 0;  (k < selectedItemPos[i]-1) && (tempPtr != NULL); k++)
				tempPtr = tempPtr->next;

			tempPtr->selectFlag = 1;

		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		if(listNo != 1)
		{
			XtVaSetValues(orderIdSL2, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(itemIdSL2, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(productIdSL2, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 4)
		{
			XtVaSetValues(qtySL2, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}

	}

	UxPhotoOrderContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	photoOrder_deleteCb 
**
** Description:		Callback function for the DELETE widget
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_deleteCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *sPtr1, *sPtr2, *sPtr3, *sPtr4;
	char Msg[IMS_COL1024_LEN+1];
	XmScrollBarCallbackStruct *cbs;
	int sCount;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{
		clientData = &(glbData.photoClientData);
		sPtr1 = clientData->photoQSelectList;
		sCount = 0;

		while (sPtr1 != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			if (sPtr1->selectFlag)
			{
				sCount++;
				/* delete this entry */
				sPtr2 = sPtr1->prev;
				sPtr3 = sPtr1->next;

				if ((sPtr2 != (OP_PHOTO_QUEUE_LIST *)NULL) &&
						(sPtr3 != (OP_PHOTO_QUEUE_LIST *)NULL))
				{
					/* item in between two items */
					sPtr2->next = sPtr3;
					sPtr3->prev = sPtr2;
					free (sPtr1);
					sPtr1 = sPtr3;
				}
				else if ((sPtr2 == (OP_PHOTO_QUEUE_LIST *)NULL) &&
								 (sPtr3 == (OP_PHOTO_QUEUE_LIST *)NULL))
				{
					/* the only item in the list */ 
					free (sPtr1);
					sPtr1 = (OP_PHOTO_QUEUE_LIST *)NULL;
					clientData->photoQSelectList = (OP_PHOTO_QUEUE_LIST *)NULL;
				}
				else if ((sPtr2 != (OP_PHOTO_QUEUE_LIST *)NULL))
				{
					/* last item in the list */
					free (sPtr1);
					sPtr1 = (OP_PHOTO_QUEUE_LIST *)NULL;
					sPtr2->next = (OP_PHOTO_QUEUE_LIST *)NULL;
				}
				else
				{
					/* first item in the list */
					sPtr3->prev = (OP_PHOTO_QUEUE_LIST *)NULL;
					free (sPtr1);
					sPtr1 = sPtr3;
					clientData->photoQSelectList = sPtr1;
				}
			}
			else
			{
				sPtr1 = sPtr1->next;
			}
		}

		if (sCount <= 0) 
		{
			/* nothing has been selected, display message and return */
			strcpy(Msg, "No item has been selected.");
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_INFO, Msg); 
			return; 
		}
		else
		{
			/* update position for each item in the photoQSelectList */
			sPtr1 = clientData->photoQSelectList;
			sCount = 0;
			while (sPtr1 != (OP_PHOTO_QUEUE_LIST *)NULL)
			{
				sPtr1->position = sCount++;
				sPtr1->selectFlag = 0;
				sPtr1 = sPtr1->next;
			}
			clientData->photoQSelectCount = sCount;

		}

		/* display data in the scroll lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		photoOrder_scroll_jobListsCb (wgt, NULL, cbs);
		free (cbs);


	}
	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoOrder_cancelCb 
**
** Description:		Callback function for the CANCEL widget
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_cancelCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *sPtr, *sNextPtr; 
	char Msg[IMS_COL1024_LEN+1];
	XmScrollBarCallbackStruct *cbs;
	int sCount;

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{

		/* call function free_photo_selectList to free the select list */
		(void) free_photo_selectList();

		/* display data in the scroll lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		photoOrder_scroll_jobListsCb (wgt, NULL, cbs);
		free (cbs);

		/* user can search again, sensitize search widget */
		XtSetSensitive (searchPB, True);

	}
	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	photoOrder_createCb 
**
** Description:		Callback function for the CREATE widget
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void	photoOrder_createCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCphotoOrder          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST  *sPtr, *tempPtr; 
	OP_CAT_STRUCT *catReq;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL30_LEN+1];
	int status, sCount, photo_type, total_prints;
	int photojob_id;
	float total_cost;
	char work_order[IMS_COL10_LEN+1];
	char order_date[IMS_COL15_LEN+1];

	UxSaveCtx = UxPhotoOrderContext;
	UxPhotoOrderContext = UxContext =
			(_UxCphotoOrder *) UxGetContext( UxWidget );
	{
		
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		clientData = &(glbData.photoClientData);
		catReq = &(clientData->catReq);

		sPtr = clientData->photoQSelectList;
		sCount = clientData->photoQSelectCount;

		/* calculate total prints */
		tempPtr = sPtr;
		total_prints = 0;

		while (tempPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			total_prints += tempPtr->quantity;
			tempPtr = tempPtr->next;
		}


		/* execute stored procedure op_create_photo_job to get
		** new photo_job_id and to update photo_job table 
		*/

		/*
		** Begin the insert transaction 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Create Photo Job: "
									 "OP_BEGINTRANSACTION failed.\n");
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

			return; 
		}

		/*
		** CAT function create photo job 
		*/
		photo_type = sPtr->photo_type;
		catReq->item[0] = (int *)&photo_type;
		catReq->item[1] = (int *)&sCount;
		catReq->item[2] = (int *)&total_prints;
		catReq->item[3] = (int *)&photojob_id;
		catReq->item[4] = (char *)work_order;
		catReq->item[5] = (float *)&total_cost;
		catReq->item[6] = (char *)order_date;

		if ((status = ims_opCat (catReq, OP_CREATEPHOTOJOB)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Create Photo Job: "
									 	"OP_CREATEPHOTOJOB failed.\n");   
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

			return;
		}

		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Create Photo Job: "
									 "OP_COMMITTRANSACTION failed. \n");
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

			return;
		}

		/* photo job has been created successfully, now
		** for each item in the select list, update the 
		** photojob_id and status in photo_queue table
		*/

		/*
		** Begin the update transaction 
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Queue: "
									 "OP_BEGINTRANSACTION failed.\n");
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

			return; 
		}

		/*
		** CAT function update photo queue  
		*/
		catReq->item[0] = (OP_PHOTO_QUEUE_LIST *)sPtr;
		catReq->item[1] = (int *)&photojob_id;
		if ((status = ims_opCat (catReq, OP_UPDATEPHOTOQUEUE)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Queue: "
									 "OP_UPDATEPHOTOQUEUE failed.\n");   
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

			return;
		}

		/*
		** Commit transaction 
		*/
		if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Update Photo Queue: "
									 "OP_COMMITTRANSACTION failed\n");
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 

			return;
		}


		/* update the screen text fields with info and
		** sensitize & desensitize widgets
		*/

		sprintf (buffer, "%s", work_order);
		XmTextFieldSetString (workOrderTF, buffer);

		sprintf (buffer, "%d", photojob_id);
		XmTextFieldSetString (jobIdTF, buffer);

		sprintf (buffer, "%-.2f", total_cost);
		XmTextFieldSetString (totalCostTF, buffer);

		sprintf (buffer, "%d", total_prints);
		XmTextFieldSetString (totalPrintsTF, buffer);

		sprintf (buffer, "%s", order_date);
		XmTextFieldSetString (orderDateTF, buffer);

		XtSetSensitive (addPB, False);
		XtSetSensitive (createPB, False);
		XtSetSensitive (deletePB, False);
		XtSetSensitive (cancelPB, False);

		/* Display message */
		sprintf(Msg, "Photo Job %d has been created,\n\n "
								 "select << Process Now >> to print out "
								 "order form.\n", photojob_id); 
		msgBoxDlg_popupCb (glbData.photoOrderW, 100, Msg); 

		/* free the select list? */

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxPhotoOrderContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	free_photo_selectList
**
** Description:		Function to free the photo_select_list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_photo_selectList()
{
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *sPtr, *sNextPtr; 

	clientData = &(glbData.photoClientData);
	sPtr = clientData->photoQSelectList;

	/* free up the clientData photoQSelectList */
	if (sPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		while (sPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			sNextPtr = sPtr->next;
			free(sPtr);
			sPtr = sNextPtr;
		}

		clientData->photoQSelectList = (OP_PHOTO_QUEUE_LIST *)NULL;
		clientData->photoQSelectCount = 0;
	}
}


/*===========================================================================*
** 
** Function Name:	free_photo_queueList
**
** Description:		Function to free the photo_queue_list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_photo_queueList()
{
	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *qPtr, *qNextPtr; 

	clientData = &(glbData.photoClientData);
	qPtr = clientData->photoQueueList;

	/* free up the clientData photoQueueList */
	if (qPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		while (qPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
		{
			qNextPtr = qPtr->next;
			free(qPtr);
			qPtr = qNextPtr;
		}

		clientData->photoQueueList = (OP_PHOTO_QUEUE_LIST *)NULL;
		clientData->photoQueueCount = 0;
	}
}


/*===========================================================================*
** 
** Function Name:	get_productID
**
** Description:		Function to get the Product ID
**
** Return Value: 	None
** 
** Revision History:
**  
** Note: This function is not needed for schema 3.30 because field
**			 order_item.p_granule_name is used as product_id.
**
**==========================================================================*/

int get_productID(
		Widget wgt,
		OP_PHOTO_QUEUE_LIST  *itemPtr)

{
	OP_CAT_STRUCT *catReq;
	OP_PHOTO_CLIENT_DATA *clientData;
	short int p_dataset_idx;
	int p_granule_idx, status;
	char granules_table[IMS_COL30_LEN+1];
	char query[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];


	clientData = &(glbData.photoClientData);
	catReq = &(clientData->catReq);

	/*
	** Get p_dataset_idx, p_granule_idx from order_item table
	*/
	sprintf (query, 
					 "select distinct p_dataset_idx, p_granule_idx "
					 "from order_item "
					 "where order_id = %d and item_id = %d", 
					 itemPtr->order_id, itemPtr->item_id);

	catReq->item[0] = (char *)query;
	catReq->item[1] = (short int *)&p_dataset_idx;
	catReq->item[2] = (int *)&p_granule_idx;

	if ((status = ims_opCat (catReq, OP_GETDATASETGRANULE)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "Internal Error: OP_GETDATASETGRANULE failed.");   
		msgBoxDlg_popupCb (wgt, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}


	/*
	** Get granules_table from dataset_policy table
	*/
	sprintf (query,
					 "select granules_table from dataset_policy "
					 "where dataset_idx = %d",
					 p_dataset_idx);

	catReq->item[0] = (char *)query;
	catReq->item[1] = (char *)granules_table;

	if ((status = ims_opCat (catReq, OP_GETGRANULESTABLE)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "Internal Error: OP_GETGRANULESTABLE failed.");   
		msgBoxDlg_popupCb (wgt, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}


	/*
	** Get product name from granules_table
	*/
	sprintf (query,
					 "select name from %s "    
					 "where granule_idx = %d and dataset_idx = %d",
					 granules_table, p_granule_idx, p_dataset_idx);

	catReq->item[0] = (char *)query;
	catReq->item[1] = (int *)&p_granule_idx;
	catReq->item[2] = (short int *)&p_dataset_idx;
	catReq->item[3] = (char *)itemPtr->product_id;
			
	if ((status = ims_opCat (catReq, OP_GETPRODUCTNAME)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		strcpy(Msg, "Internal Error: OP_GETPRODUCTNAME failed.");   
		msgBoxDlg_popupCb (wgt, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}

	return (IMS_OK);

}


/*===========================================================================*
** 
** Function Name:	process_photoOrder
**
** Description:		Function to process the new photo order
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

int process_photoOrder(
		int screenID,
		OP_PHOTO_QUEUE_LIST  *photoQPtr)

{
	_UxCphotoOrder          *UxPhotoOrderContext;
	_UxCphotoJob          *UxPhotoJobContext;

	OP_PHOTO_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL30_LEN+1];
	char cmdbuf[IMS_COL255_LEN+1];
	char photo_type_desc[IMS_COL30_LEN+1];
	char tempFileName[IMS_COL255_LEN+1];
	char *value;
	FILE *fp;
	int status, photojob_id;


	if (screenID)
	{
		UxPhotoJobContext = 
					(_UxCphotoJob *) UxGetContext( glbData.photoJobW );
	}
	else
	{
		UxPhotoOrderContext = 
					(_UxCphotoOrder *) UxGetContext( glbData.photoOrderW );
	} 

	/* Change cursor to watch cursor */
	timeOutCursors (True);
		
	clientData = &(glbData.photoClientData);
	catReq = &(clientData->catReq);


	/* update photo_job status to be IN-PHOTO */
	/* get photo_type description */

	if (screenID)
	{
		value = XmTextFieldGetString (UxPhotoJobContext->UxjobIdTF2);
	}
	else
	{
		value = XmTextFieldGetString (UxPhotoOrderContext->UxjobIdTF);
	}

	/*
	** Begin the update transaction 
	*/
	if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Internal Error:\nUpdate Photo Job Status,\n"
								 "OP_BEGINTRANSACTION failed.\n"); 
		if (screenID)
		{
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
		}
		else
		{
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 
		}

		return (IMS_FATAL);
	}

	/*
	** CAT function update photo job 
	*/
	photojob_id = atoi(value);
	catReq->item[0] = (int *)&photojob_id;
	catReq->item[1] = (char *)photo_type_desc;

	if ((status = ims_opCat (catReq, OP_UPDATEPHOTOJOB)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Internal Error: Update Photo Job Status failed.");   
		if (screenID)
		{
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
		}
		else
		{
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 
		}

		return (IMS_FATAL);
	}


	/*
	** Commit transaction 
	*/
	if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
	{
		/* rollback transaction */
		ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Internal Error:\nUpdate Photo Job Status \n"
								 "OP_COMMITTRANSACTION failed for Photo Job Id: %d.\n",
								 photojob_id);   
		if (screenID)
		{
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
		}
		else
		{
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 
		}
	
		return (IMS_FATAL);
	}


	/* put temporary print file in tmp directory */
	ims_concatFilePath (tempFileName, DEF_PRINT_DIRECTORY, "ims_opPhoto.tmp");

	if ((fp = fopen(tempFileName, "a")) == (FILE *)NULL)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Internal Error: create temp file failed.");   
		if (screenID)
		{
			msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
		}
		else
		{
			msgBoxDlg_popupCb (glbData.photoOrderW, IMS_FATAL, Msg); 
		}

		return (IMS_FATAL);
	}

	fprintf (fp, "\n\t\t\t  ASF PHOTO PRODUCTS ORDER REPORT\n");
	fprintf (fp, "\t\t\t  ===============================\n\n");

	if (screenID)
	{
		value = XmTextFieldGetString (UxPhotoJobContext->UxjobIdTF2);
	}
	else
	{
		value = XmTextFieldGetString (UxPhotoOrderContext->UxjobIdTF);
	}

	fprintf (fp, "\t\t\t\tPHOTO JOB ID:\t%s\n\n", value);


	if (screenID)
	{
		value = XmTextFieldGetString (UxPhotoJobContext->UxorderDateTF);
	}
	else
	{
		value = XmTextFieldGetString (UxPhotoOrderContext->UxorderDateTF);
	}

	fprintf (fp, "\t\t\t   DATE OF ORDER:  %s\n\n\n", value);

	fprintf (fp, "---------------------------------------------------------------------------------------\n\n");

	fprintf (fp, "PHOTO TYPE:   %-25s", photo_type_desc);
	fprintf (fp, "\tPHOTO LAB OPERATOR:___________________________\n\n"); 

	if (screenID)
	{
		value = XmTextFieldGetString (UxPhotoJobContext->UxworkOrderTF);
	}
	else
	{
		value = XmTextFieldGetString (UxPhotoOrderContext->UxworkOrderTF);
	}

	fprintf (fp, "WORK ORDER:   %s", value);
	fprintf (fp, "\t\t\tLABOR (Hours):     ___________________________\n\n");

	if (screenID)
	{
		value = XmTextFieldGetString (UxPhotoJobContext->UxtotalPrintsTF);
	}
	else
	{
		value = XmTextFieldGetString (UxPhotoOrderContext->UxtotalPrintsTF);
	}

	fprintf (fp, "TOTAL PRINTS:  %d", atoi(value));
	fprintf (fp, "\t\t\tMATERIALS ($):     ___________________________\n\n"); 

	if (screenID)
	{
		value = XmTextFieldGetString (UxPhotoJobContext->UxtotalCostTF);
	}
	else
	{
		value = XmTextFieldGetString (UxPhotoOrderContext->UxtotalCostTF);
	}

	fprintf (fp, "TOTAL COST:   %-.2f", atof(value));
	fprintf (fp, "\t\t\tCOMPLETION DATE:   ___________________________\n\n\n");

	fprintf (fp, "---------------------------------------------------------------------------------------\n\n\n");

	while (photoQPtr != (OP_PHOTO_QUEUE_LIST *)NULL)
	{
		fprintf (fp, "=======================================================================================\n");
		fprintf (fp, "ORDER/ITEM ID:	%d  %d\n",
								 photoQPtr->order_id, photoQPtr->item_id);

		if (photoQPtr->product_id[0] != '\0')
			fprintf (fp, "PRODUCT ID:	%s\n", photoQPtr->product_id);
		else
			fprintf (fp, "PRODUCT ID:	N/A\n");
			
		fprintf (fp, "QUANTITY:	%d\n", photoQPtr->quantity);

	/* print out user comments */
		if (photoQPtr->op_comment[0] != '\0')
			fprintf (fp, "REMARKS:	%s\n", photoQPtr->op_comment);
		else
			fprintf (fp, "REMARKS:	N/A\n\n");

		photoQPtr = photoQPtr->next;

	}

	fprintf (fp, "=======================================================================================\n");
	fflush (fp);
	sprintf (cmdbuf, "lpr %s", tempFileName);
	system (cmdbuf);

	fclose (fp);
	remove (tempFileName);

	/* Change cursor back to normal */
	timeOutCursors (False);
	
	return (IMS_OK);
}


/*===========================================================================*
** 
** Function Name: goto_photoJobScreen
**
** Description:		Pop up the photo Job screen from Photo order screen  
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

void	goto_photoJobScreen(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

		XtPopup(XtParent(glbData.photoJobW), XtGrabNone);
		glbData.photoJobFlag = 1;

}


/*===========================================================================*
** 
** Function Name: goto_welcomeScreen
**
** Description:		Pop up the welcome screen 
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

void	goto_welcomeScreen(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

		XtPopup(XtParent(glbData.welcomeW), XtGrabNone);
		glbData.welcomeFlag = 1;

}

