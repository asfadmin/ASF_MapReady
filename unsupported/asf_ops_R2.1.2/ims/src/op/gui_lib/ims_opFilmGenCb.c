static char *sccs = "@(#)ims_opFilmGenCb.c	5.3  05/12/97";
/*******************************************************************************

	File:			ims_opFilmGen.c

	Function:	Callback functions for Film Generation Screen

	Author:		Jennifer Ting

	Date:			9/1995

	Revision: 4/26/96 - Modified filmGen_fire_regenCb and filmGen_laser_regenCb
											to update item step information in the order_item table
											when the item's status is being rolled back in the
											regenerate callback.

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

#define _IMS_OP_FILMGENCB_C
#include "ims_opCb.h"

#define FIRE_ROWS 21
#define LASER_ROWS 21
#define TTDL_ROWS 24

static int fireWindowTop = 0;
static int laserWindowTop = 0;
static int ttdlWindowTop = 0;
static int fireSearchStatus_flag = 0;
static int laserSearchStatus_flag = 0;

/* Function local to this file */
int filmGen_fire_createQuery (Widget wgt);
int filmGen_fire_executeQuery (Widget wgt);

int filmGen_laser_createQuery (Widget wgt);
int filmGen_laser_executeQuery (Widget wgt);

static void free_fire_queueList();
static void free_laser_queueList();
static void free_ttdl_queueList();
static void free_film_itemList();

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opFilmGen.h>
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
** 
** Function Name:	filmGen_create_optionMenuCb 
**
** Description:		Callback function to create status option menu
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

void	filmGen_create_optionMenuCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int i;
	Dimension  marginLeft, marginRight;
	Dimension  marginWidth;
	Pixel      selectColor;
	Pixel      background;
	XmString   label;
	Widget		 menu_item;
	XmFontList fontList;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		/*
		** Create Fire Queue Search Status Option Menu
		*/

		/* Get resources from all dummyPB */
		XtVaGetValues(fireDummyPB,
						XmNfontList, &fontList,
						XmNbackground, &background,
						XmNmarginLeft, &marginLeft,
						XmNmarginRight, &marginRight,
						XmNmarginWidth, &marginWidth,
						NULL);

		for (i = 0; i < glbData.fire_status_count; i++)
		{
			label = XmStringCreateLocalized (glbData.fire_status[i].item_name);
			menu_item = XtVaCreateManagedWidget 
									(glbData.fire_status[i].item_name, xmPushButtonWidgetClass,
									 fireSearchStatusOM_pane,
									 XmNlabelString, label,
									 XmNfontList, fontList,
									 XmNbackground, background,
									 XmNmarginLeft, marginLeft,
									 XmNmarginRight, marginRight,
									 XmNmarginWidth, marginWidth,
									 NULL);

		 	XtAddCallback (menu_item, XmNactivateCallback,
										 (XtCallbackProc) filmGen_optionmenu_toggledCb, 
										 (XtPointer) glbData.fire_status[i].item_id);

			XmStringFree (label);
		}


		/*
		** Create Laser Queue Search Status Option Menu
		*/

		/* Get resources from all dummyPB */
		XtVaGetValues(laserDummyPB,
						XmNfontList, &fontList,
						XmNbackground, &background,
						XmNmarginLeft, &marginLeft,
						XmNmarginRight, &marginRight,
						XmNmarginWidth, &marginWidth,
						NULL);

		for (i = 0; i < glbData.laser_status_count; i++)
		{
			label = XmStringCreateLocalized (glbData.laser_status[i].item_name);
			menu_item = XtVaCreateManagedWidget 
									(glbData.laser_status[i].item_name, xmPushButtonWidgetClass,
									 laserSearchStatusOM_pane,
									 XmNlabelString, label,
									 XmNfontList, fontList,
									 XmNbackground, background,
									 XmNmarginLeft, marginLeft,
									 XmNmarginRight, marginRight,
									 XmNmarginWidth, marginWidth,
									 NULL);

		 	XtAddCallback (menu_item, XmNactivateCallback,
										 (XtCallbackProc) filmGen_optionmenu_toggledCb, 
										 (XtPointer) glbData.laser_status[i].item_id);

			XmStringFree (label);
		}

	}
	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: filmGen_optionmenu_toggledCb
**
** Description:		This callback function stores the most recently
**								selected menu item in a global variable matching
**								the option menu.
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

void	filmGen_optionmenu_toggledCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int optionMenu_no;
	int which;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		which = (int) cd;

		XtVaGetValues (XtParent(wgt), XmNuserData, &optionMenu_no, NULL);

		switch (optionMenu_no)
		{
			case 1 : /* Fire Recorder Search Status Option Menu */
				fireSearchStatus_flag = which;
				break;

			case 3 : /* Laser Tech Search Status Option Menu */
				laserSearchStatus_flag = which;
				break;

			default:
				break;
		}
	}
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: filmGen_scroll_fireListsCb
**
** Description:		Updates the item list widgets (3 of them) with new items
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

void	filmGen_scroll_fireListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	int fireQCount, count, memory;
	int i, temp, j, k, highLightPos[FIRE_ROWS];
	Widget sbar;
	XmStringTable orderIdStr, itemIdStr, statusStr;
	char buffer[IMS_COL255_LEN+1];
	int slider;


	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	fireQPtr = clientData->fireQueueList;
	fireQCount = clientData->fireQueueCount;
	memory = 1;
	j = 0;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		fireWindowTop = cbs->value;
		clientData->fireWindowTop = fireWindowTop;
		count = fireQCount - fireWindowTop;

		if (count > FIRE_ROWS)
			count = FIRE_ROWS;


		/* If no rows to display, clean up fire queue lists */
		if ((fireQCount == 0) || (fireQPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (fireOrderIdSL);
			XmListDeleteAllItems (fireItemSL);
			XmListDeleteAllItems (fireStatusSL);

			/* Update Scrollbar position */
			XtVaGetValues(fireDummySW, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, fireWindowTop,
				XmNsliderSize, 1, 
				NULL);

			XtSetSensitive (fireAddPB, False);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		orderIdStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		itemIdStr  = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		statusStr	 = (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && orderIdStr && itemIdStr && statusStr;

		if (!memory)
		{
			/* Display error messages in message window */

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (orderIdStr[i]);
				XmStringFree (itemIdStr[i]);
				XmStringFree (statusStr[i]);
			}

			XtFree ((char *)orderIdStr);
			XtFree ((char *)itemIdStr);
			XtFree ((char *)statusStr);
			return;
		}

	
		while ((fireQPtr != NULL) && (fireQPtr->position != fireWindowTop))
		{
			fireQPtr = fireQPtr->next;
		}
		
		for (i = 0; i < count && fireQPtr != (OP_FIRE_QUEUE_LIST *)NULL; i++)
		{
			if (fireQPtr->order_id)
			{
				sprintf (buffer, "%d", fireQPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				orderIdStr[i] = XmStringCreateLocalized("");

			if (fireQPtr->item_id)
			{
				sprintf (buffer, "%d", fireQPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				itemIdStr[i] = XmStringCreateLocalized("");

			if (fireQPtr->status)
			{
				k = 0;
				while ((k < glbData.fire_status_count) && 
				 (fireQPtr->status != glbData.fire_status[k].item_id))
							k++;

				if (k < glbData.fire_status_count)
				{
					statusStr[i] = XmStringCreateLocalized
												(glbData.fire_status[k].item_name);
				}
				else
					/* did not find the matching order_status id */
					statusStr[i] = XmStringCreateLocalized("");

			}
			else
				statusStr[i] = XmStringCreateLocalized("");


			if (fireQPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			fireQPtr = fireQPtr->next;

		}

		/* Load all the synchronized arrays to fire queue list widgets */

		XtVaSetValues(
				fireOrderIdSL, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				fireItemSL, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				fireStatusSL, XmNitems, statusStr, XmNitemCount, count, NULL);

		/* Free compound strings */
		while (--i)
		{
			XmStringFree (orderIdStr[i]);
			XmStringFree (itemIdStr[i]);
			XmStringFree (statusStr[i]);
		}

		XtFree ((char *)orderIdStr);
		XtFree ((char *)itemIdStr);
		XtFree ((char *)statusStr);
	
		/* Update Scrollbar position */
		XtVaGetValues(fireDummySW, XmNverticalScrollBar, &sbar, NULL);

		temp = fireQCount;
		if (fireQCount > FIRE_ROWS)
			slider = FIRE_ROWS;
		else
			slider = fireQCount;
		
		if (fireQCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, fireQCount,
				XmNvalue, fireWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, fireWindowTop,
				XmNsliderSize, 1, 
				NULL);


		/* Set selectionPolicy of fire list widgets to MultipleSelect */
		XmListDeselectAllItems(fireOrderIdSL);
		XtVaSetValues(fireOrderIdSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(fireItemSL);
		XtVaSetValues(fireItemSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(fireStatusSL);
		XtVaSetValues(fireStatusSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		while (j--)
		{
			XmListSelectPos(fireOrderIdSL, highLightPos[j], False);
			XmListSelectPos(fireItemSL, highLightPos[j], False);
			XmListSelectPos(fireStatusSL, highLightPos[j], False);
		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		XtVaSetValues(fireOrderIdSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(fireItemSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(fireStatusSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);

	}

	UxFilmGenerationContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: filmGen_fireLists_selectionCb
**
** Description:		Updates the selection of the item(s) in all the fire
**								queue list widgets (3 of them), synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each list widget is associated with
** 									 							a unique number, for example, list widget
**																orderIdSL1 has listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void filmGen_fireLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *fireQPtr, *tempPtr;
	int listNo;
	int i, k, count, fireQCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		clientData = &(glbData.filmClientData);
		fireQPtr = clientData->fireQueueList;
		fireQCount = clientData->fireQueueCount;
		listNo = (int)listNumber;
		selectedItemPos = cbs->selected_item_positions;
		itemPosition = cbs->item_position;
		totalItemsSelected = cbs->selected_item_count;
		

		/* Set selectionPolicy of item list widgets to MultipleSelect */
		if(listNo != 1)
		{
			XmListDeselectAllItems(fireOrderIdSL);
			XtVaSetValues(fireOrderIdSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(fireItemSL);
			XtVaSetValues(fireItemSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(fireStatusSL);
			XtVaSetValues(fireStatusSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}


		/* Locate screen items in fire queue lists */
		count = fireQCount - fireWindowTop;

		if (count > FIRE_ROWS)
			count = FIRE_ROWS;

		while ((fireQPtr != NULL) && (fireQPtr->position != fireWindowTop))
		{
			fireQPtr = fireQPtr->next;
		}

		/* Reset the selectFlag for each item first */
		tempPtr = fireQPtr;
		for (i = 0; i < count && tempPtr != NULL; i++)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		for (i = 0; i < totalItemsSelected; i++)
		{
			if (listNo != 1)
				XmListSelectPos(fireOrderIdSL, selectedItemPos[i], False);
			if (listNo != 2)
				XmListSelectPos(fireItemSL, selectedItemPos[i], False);
			if (listNo != 3)
				XmListSelectPos(fireStatusSL, selectedItemPos[i], False);

			/* Locate selected items in queue lists to set the selectFlag */
			tempPtr = fireQPtr;
			for (k = 0;  (k < selectedItemPos[i]-1) && (tempPtr != NULL); k++)
				tempPtr = tempPtr->next;

			tempPtr->selectFlag = 1;

		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		if(listNo != 1)
		{
			XtVaSetValues(fireOrderIdSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(fireItemSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(fireStatusSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}

	}

	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	filmGen_fire_clearCb 
**
** Description:		Callback function for the Clear button
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

void	filmGen_fire_clearCb(
	Widget wgt,
	XtPointer cd, 
	XtPointer cb)

{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	XmScrollBarCallbackStruct *cbs;
	Widget sbar;
	XmString label;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/* free up fire_queue_list */ 
		(void) free_fire_queueList();

		fireWindowTop = 0;

		/* clean up list & text widgets */
		XmListDeleteAllItems (fireOrderIdSL);
		XmListDeleteAllItems (fireItemSL);
		XmListDeleteAllItems (fireStatusSL);

		XmTextFieldSetString (fireTotalItemsTF, "");

		XtSetSensitive (fireAddPB, False);
		XtSetSensitive (fireClearPB, True);
		XtSetSensitive (updateFireStatusMPB, False);
		XtSetSensitive (editFireCommentMPB, False);
		XtSetSensitive (fireItemRegenMPB, False);

		/* Update Scrollbar position */
		XtVaGetValues(fireDummySW, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
			XmNmaximum, 1,
			XmNvalue, fireWindowTop,
			XmNsliderSize, 1, 
			NULL);

		/* Reset Fire Search Status Option Menu */
		XtVaSetValues (fireSearchStatusOM, XmNmenuHistory,
									 fireDummyPB, NULL);

		fireSearchStatus_flag = 0;

	}
	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: filmGen_fire_createQuery 
**
** Description:		Create query to search for fire queue items.
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

int filmGen_fire_createQuery(
	Widget wgt) 
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_FILM_CLIENT_DATA *clientData;
	OP_QUERY_STRUCT *sql;
	int status;
	char *sqlPtr;
	char Msg[IMS_COL1024_LEN+1];
	

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);

	{
		clientData = &(glbData.filmClientData);
		sql = &(clientData->queryStruct);

		/*
		** Initialize op_query_struct 
		*/
		sql->select[0] = sql->from[0] = sql->where[0] = sql->sqlBuf[0] = '\0';
		sql->sPtr = sql->select;
		sql->fPtr = sql->from;
		sql->wPtr = sql->where;
		sqlPtr = sql->sqlBuf;

		strcpy (sql->sPtr, 
						"distinct order_id, item_id, status, op_comment ");

		sql->sPtr = sql->sPtr + strlen(sql->sPtr);

		strcpy (sql->fPtr, "fire_queue");
		sql->fPtr = sql->fPtr + strlen(sql->fPtr);

		/*
		**  Fire Queue Status OptionMenu 
		*/
		if (fireSearchStatus_flag)
		{
			sprintf (sql->wPtr, " status = %d", fireSearchStatus_flag);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}

		/*
		** complete the sql statement
		*/

		if (fireSearchStatus_flag)
		{
			sprintf (sqlPtr,
							 "select %s\nfrom %s\nwhere %s\norder by order_id, item_id",
							 sql->select, sql->from, sql->where);
		}
		else
		{
			sprintf (sqlPtr, "select %s\nfrom %s\norder by order_id, item_id",
											 sql->select, sql->from);
		}
		
		sqlPtr = sqlPtr + strlen(sqlPtr);

	}
	UxFilmGenerationContext = UxSaveCtx;

	return (IMS_OK);
}


/*===========================================================================*
** 
** Function Name: filmGen_fire_executeQuery 
**
** Description:		Execute the query created by filmGen_fire_createQuery.
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

int filmGen_fire_executeQuery(
	Widget wgt) 
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_FILM_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_FIRE_QUEUE_LIST *fireQueuePtr;
	int status;
	int itemCount;
	char Msg[IMS_COL1024_LEN+1];


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/*
		** Initialize catalog request structure 
		*/
		clientData = &(glbData.filmClientData);
		catReq = &(clientData->catReq);
		catReq->item[0] = (int *)&itemCount;
		catReq->item[1] = (char *)clientData->queryStruct.sqlBuf;

		if ((status = ims_opCat (catReq, OP_GETFIREQUEUELIST)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error: fire queue retrieval failed.");   
			msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
			return (IMS_FATAL);
		}

		/* assign returned items to glbData.filmClientData->fireQueueList */
		clientData->fireQueueCount = *(int *)catReq->item[0];
		clientData->fireQueueList = (OP_FIRE_QUEUE_LIST *)catReq->item[2];

		if ((clientData->fireQueueList != (OP_FIRE_QUEUE_LIST *)NULL) ||
				(clientData->fireQueueCount > 0))
		{
			/* initialize select flag */
			fireQueuePtr = clientData->fireQueueList;

			while (fireQueuePtr != (OP_FIRE_QUEUE_LIST *)NULL)
			{
				fireQueuePtr->selectFlag = 0;
				fireQueuePtr = fireQueuePtr->next;

			}
		}
	}

	UxFilmGenerationContext = UxSaveCtx;

	return (IMS_OK);

}


/*===========================================================================*
** 
** Function Name: filmGen_fire_searchCb
**
** Description:		callback function for fire queue search PB.
**								1. create query
**								2. execute query
**								3. display results
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

void	filmGen_fire_searchCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	int k, status, new_id, generated_id;
	Widget sbar;
	char buffer[IMS_COL10_LEN+1];
	XmScrollBarCallbackStruct *cbs;
	XmString label;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		clientData = &(glbData.filmClientData);

		/* free up fire_queue_list  */ 
		(void) free_fire_queueList();

		fireWindowTop = 0;

		/* clean up list & text widgets */
		XmListDeleteAllItems (fireOrderIdSL);
		XmListDeleteAllItems (fireItemSL);
		XmListDeleteAllItems (fireStatusSL);
		XmTextFieldSetString (fireTotalItemsTF, "");

		/* Update Scrollbar position */
		XtVaGetValues(fireDummySW, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
			XmNmaximum, 1,
			XmNvalue, fireWindowTop,
			XmNsliderSize, 1, 
			NULL);

		if ((status = filmGen_fire_createQuery (glbData.filmW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);
			return;
		}

		if ((status = filmGen_fire_executeQuery (glbData.filmW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);
			return;
		}

		/* display fire queue list */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		filmGen_scroll_fireListsCb (glbData.filmW, NULL, cbs);
		free (cbs);

		/* display fire queue item count */
		sprintf (buffer, "%d", clientData->fireQueueCount);
		XmTextFieldSetString (fireTotalItemsTF, buffer);

		/*
		** sensitize fireAddPB and updateFireStatusMPB if 
		** count is > 0, desensitize them otherwise. 
		*/
		if (clientData->fireQueueCount > 0)
		{
			XtSetSensitive (updateFireStatusMPB, True);
			XtSetSensitive (editFireCommentMPB, True);
			XtSetSensitive (fireItemRegenMPB, True);
			XtSetSensitive (fireAddPB, True);
		}
		else
		{
			XtSetSensitive (updateFireStatusMPB, False);
			XtSetSensitive (editFireCommentMPB, False);
			XtSetSensitive (fireItemRegenMPB, False);
			XtSetSensitive (fireAddPB, False);
		}

	
		/* Change cursor back to normal*/
		timeOutCursors (False);

	}

	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	free_fire_queueList
**
** Description:		Function to free the fire_queue_list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_fire_queueList()
{
	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *qPtr, *qNextPtr; 

	clientData = &(glbData.filmClientData);
	qPtr = clientData->fireQueueList;

	/* free up the clientData fireQueueList */
	if (qPtr != (OP_FIRE_QUEUE_LIST *)NULL)
	{
		while (qPtr != (OP_FIRE_QUEUE_LIST *)NULL)
		{
			qNextPtr = qPtr->next;
			free(qPtr);
			qPtr = qNextPtr;
		}

		clientData->fireQueueList = (OP_FIRE_QUEUE_LIST *)NULL;
		clientData->fireQueueCount = 0;
	}
}


/*===========================================================================*
** 
** Function Name: filmGen_scroll_ttdlListsCb
**
** Description:		Updates the ttdl list widgets (3 of them) with new items
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

void	filmGen_scroll_ttdlListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_TTDL_QUEUE_LIST *ttdlQPtr;
	int ttdlQCount, count, memory;
	int i, temp, j, k, highLightPos[FIRE_ROWS];
	Widget sbar;
	XmStringTable orderIdStr, itemIdStr, queueTypeStr;
	char buffer[IMS_COL255_LEN+1];
	int slider;


	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	ttdlQPtr = clientData->ttdlQueueList;
	ttdlQCount = clientData->ttdlQueueCount;
	memory = 1;
	j = 0;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		ttdlWindowTop = cbs->value;
		count = ttdlQCount - ttdlWindowTop;

		if (count > TTDL_ROWS)
			count = TTDL_ROWS;


		/* If no rows to display, clean up ttdl queue lists */
		if ((ttdlQCount == 0) || (ttdlQPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (ttdlOrderIdSL);
			XmListDeleteAllItems (ttdlItemSL);
			XmListDeleteAllItems (ttdlQueueTypeSL);

			/* Update Scrollbar position */
			XtVaGetValues(ttdlDummySW, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, ttdlWindowTop,
				XmNsliderSize, 1, 
				NULL);

			XtSetSensitive (ttdlDeletePB, False);
			XtSetSensitive (ttdlProcessPB, False);

			/* display total item count in ttdl lists */
			XmTextFieldSetString (ttdlTotalItemsTF, NULL);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		orderIdStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		itemIdStr  = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		queueTypeStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && orderIdStr && itemIdStr && queueTypeStr;

		if (!memory)
		{
			/* Display error messages in message window */

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (orderIdStr[i]);
				XmStringFree (itemIdStr[i]);
				XmStringFree (queueTypeStr[i]);
			}

			XtFree ((char *)orderIdStr);
			XtFree ((char *)itemIdStr);
			XtFree ((char *)queueTypeStr);
			return;
		}

	
		while ((ttdlQPtr != NULL) && (ttdlQPtr->position != ttdlWindowTop))
		{
			ttdlQPtr = ttdlQPtr->next;
		}
		
		for (i = 0; i < count && ttdlQPtr != (OP_TTDL_QUEUE_LIST *)NULL; i++)
		{
			if (ttdlQPtr->order_id)
			{
				sprintf (buffer, "%d", ttdlQPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				orderIdStr[i] = XmStringCreateLocalized("");

			if (ttdlQPtr->item_id)
			{
				sprintf (buffer, "%d", ttdlQPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				itemIdStr[i] = XmStringCreateLocalized("");

			if (ttdlQPtr->queue_type == LASER)
			{
				strcpy (buffer, "LASER  TECH");
				queueTypeStr[i] = XmStringCreateLocalized(buffer);
			}
			else 
				if (ttdlQPtr->queue_type == FIRE)
				{
					strcpy (buffer, "FIRE  RECORDER");
					queueTypeStr[i] = XmStringCreateLocalized(buffer);
				}
				else
					queueTypeStr[i] = XmStringCreateLocalized("");


			if (ttdlQPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			ttdlQPtr = ttdlQPtr->next;

		}

		/* Load all the synchronized arrays to ttdl queue list widgets */

		XtVaSetValues(
				ttdlOrderIdSL, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				ttdlItemSL, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				ttdlQueueTypeSL, XmNitems, queueTypeStr, XmNitemCount, count, NULL);

		/* Free compound strings */
		while (--i)
		{
			XmStringFree (orderIdStr[i]);
			XmStringFree (itemIdStr[i]);
			XmStringFree (queueTypeStr[i]);
		}

		XtFree ((char *)orderIdStr);
		XtFree ((char *)itemIdStr);
		XtFree ((char *)queueTypeStr);
	
		/* Update Scrollbar position */
		XtVaGetValues(ttdlDummySW, XmNverticalScrollBar, &sbar, NULL);

		temp = ttdlQCount;
		if (ttdlQCount > TTDL_ROWS)
			slider = TTDL_ROWS;
		else
			slider = ttdlQCount;
		
		if (ttdlQCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, ttdlQCount,
				XmNvalue, ttdlWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, ttdlWindowTop,
				XmNsliderSize, 1, 
				NULL);


		/* Set selectionPolicy of ttdl list widgets to MultipleSelect */
		XmListDeselectAllItems(ttdlOrderIdSL);
		XtVaSetValues(ttdlOrderIdSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(ttdlItemSL);
		XtVaSetValues(ttdlItemSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(ttdlQueueTypeSL);
		XtVaSetValues(ttdlQueueTypeSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		while (j--)
		{
			XmListSelectPos(ttdlOrderIdSL, highLightPos[j], False);
			XmListSelectPos(ttdlItemSL, highLightPos[j], False);
			XmListSelectPos(ttdlQueueTypeSL, highLightPos[j], False);
		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		XtVaSetValues(ttdlOrderIdSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(ttdlItemSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(ttdlQueueTypeSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
	}

	UxFilmGenerationContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: filmGen_ttdlLists_selectionCb
**
** Description:		Updates the selection of the item(s) in all the ttdl
**								queue list widgets (3 of them), synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each list widget is associated with
** 									 							a unique number, for example, list widget
**																orderIdSL1 has listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void filmGen_ttdlLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_TTDL_QUEUE_LIST *ttdlQPtr, *tempPtr;
	int listNo;
	int i, k, count, ttdlQCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		clientData = &(glbData.filmClientData);
		ttdlQPtr = clientData->ttdlQueueList;
		ttdlQCount = clientData->ttdlQueueCount;
		listNo = (int)listNumber;
		selectedItemPos = cbs->selected_item_positions;
		itemPosition = cbs->item_position;
		totalItemsSelected = cbs->selected_item_count;
		

		/* Set selectionPolicy of item list widgets to MultipleSelect */
		if(listNo != 1)
		{
			XmListDeselectAllItems(ttdlOrderIdSL);
			XtVaSetValues(ttdlOrderIdSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(ttdlItemSL);
			XtVaSetValues(ttdlItemSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(ttdlQueueTypeSL);
			XtVaSetValues(ttdlQueueTypeSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}


		/* Locate screen items in ttdl queue lists */
		count = ttdlQCount - ttdlWindowTop;

		if (count > TTDL_ROWS)
			count = TTDL_ROWS;

		while ((ttdlQPtr != NULL) && (ttdlQPtr->position != ttdlWindowTop))
		{
			ttdlQPtr = ttdlQPtr->next;
		}

		/* Reset the selectFlag for each item first */
		tempPtr = ttdlQPtr;
		for (i = 0; i < count && tempPtr != NULL; i++)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		for (i = 0; i < totalItemsSelected; i++)
		{
			if (listNo != 1)
				XmListSelectPos(ttdlOrderIdSL, selectedItemPos[i], False);
			if (listNo != 2)
				XmListSelectPos(ttdlItemSL, selectedItemPos[i], False);
			if (listNo != 3)
				XmListSelectPos(ttdlQueueTypeSL, selectedItemPos[i], False);

			/* Locate selected items in queue lists to set the selectFlag */
			tempPtr = ttdlQPtr;
			for (k = 0;  (k < selectedItemPos[i]-1) && (tempPtr != NULL); k++)
				tempPtr = tempPtr->next;

			tempPtr->selectFlag = 1;

		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		if(listNo != 1)
		{
			XtVaSetValues(ttdlOrderIdSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(ttdlItemSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(ttdlQueueTypeSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}

	}

	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	filmGen_fire_addCb 
**
** Description:		Callback function for the fire queue ADD button,
**								adds the selected items from either fire queue or
**								the laser queue to the ttdl lists.
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

void	filmGen_fire_addCb(
	Widget wgt, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_FIRE_QUEUE_LIST *fPtr, *fireQPtr;
	OP_TTDL_QUEUE_LIST *sPtr, *nPtr, *tempPtr;
	OP_FIRE_QUEUE_LIST *qPtr;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL10_LEN+1];
	int status, sCount, invalid_status;
	DBSMALLINT current_status;
	XmScrollBarCallbackStruct *cbs;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	clientData = &(glbData.filmClientData);
	catReq = &(clientData->catReq);
	qPtr = (OP_FIRE_QUEUE_LIST *)clientData->fireQueueList;
	sPtr = nPtr = tempPtr = (OP_TTDL_QUEUE_LIST *)NULL;

	/* find out how many items are selected */
	sCount = 0;
	invalid_status = IMS_FALSE;
	while (qPtr != NULL) 
	{
		/* item is added to the TTDL only if it has status NEW */
		if (qPtr->selectFlag)
		{
			/*
			** call getFireQueueStatus to verify that the item status
			** is NEW in the fire_queue table 
			*/
			catReq->item[0] = (DBINT *)&qPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&qPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&current_status;
			if ((status = ims_opCat(catReq, OP_GETFIREITEMSTATUS)) < 0)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nAdd Fire Queue Items to TTDL Queue,\n"
						 "OP_GETFIREITEMSTATUS failed for Order: %d, Item: %d\n", 
						 qPtr->order_id, qPtr->item_id);   
				msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

				qPtr = qPtr->next;
				continue;

			}

			/* 
			** Add to TTDL queue is allowed only if the item's
			** current status is NEW 
			*/
			if (current_status != FIRE_NEW)
			{
				invalid_status = IMS_TRUE;
			}
			else
			{
				/* check for duplicate copies in select list */
				sCount++;   
				sPtr = clientData->ttdlQueueList;
				if (sPtr != (OP_TTDL_QUEUE_LIST *)NULL)
				{	
					/* Find the right place in the select list to insert */
					while ((sPtr->next != (OP_TTDL_QUEUE_LIST *)NULL) &&
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

				/* we have a new item to add to the ttdl list */
				if ((nPtr = malloc(sizeof(OP_TTDL_QUEUE_LIST))) == NULL)
				{
					/* malloc failed, display message and return */
					strcpy(Msg, "Memory allocation failed.");
					msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

					return; 
				}
				else
				{
					nPtr->prev = (OP_TTDL_QUEUE_LIST *)NULL;
					nPtr->next = (OP_TTDL_QUEUE_LIST *)NULL;
					nPtr->order_id = qPtr->order_id;
					nPtr->item_id = qPtr->item_id;
					nPtr->status = qPtr->status;
					nPtr->selectFlag = 0;
					nPtr->position = 0;
					nPtr->queue_type = FIRE;
					
					/* insert new item into the ttdl list */
					if (sPtr == (OP_TTDL_QUEUE_LIST *)NULL)
					{
						clientData->ttdlQueueList = nPtr;
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


							if (tempPtr != (OP_TTDL_QUEUE_LIST *)NULL)
								tempPtr->next = nPtr;
							else 
								clientData->ttdlQueueList = nPtr;

						}
						else
						{
							/* insert after */
							tempPtr = sPtr->next;
							sPtr->next = nPtr;
							nPtr->prev = sPtr;
							nPtr->next = tempPtr;
							if (tempPtr != (OP_TTDL_QUEUE_LIST *)NULL)
								tempPtr->prev = nPtr;
						}
					}
				}
			}
		}
		qPtr = qPtr->next;

	} /* while */

	if (sCount > 0)
	{
		/* update position for each item in the ttdlQueueList */
		tempPtr = clientData->ttdlQueueList;
		sCount = 0;
		while (tempPtr != (OP_TTDL_QUEUE_LIST *)NULL)
		{
			tempPtr->position = sCount++;
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}
		clientData->ttdlQueueCount = sCount;

		/* display data in the ttdl lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		filmGen_scroll_ttdlListsCb (wgt, NULL, cbs);
		free (cbs);

		/* display total item count in ttdl lists */
		sprintf (buffer, "%d", clientData->ttdlQueueCount);
		XmTextFieldSetString (ttdlTotalItemsTF, buffer);

		/* sensitize Delete & Process pushbuttons */
		XtSetSensitive (ttdlDeletePB, True);
		XtSetSensitive (ttdlProcessPB, True);
	}

	if ((sCount <= 0) || (invalid_status)) 
	{
		/* nothing has been selected, display message and return */
		strcpy(Msg, "Only items with status NEW can be added to TTDL Queue.\n\n"  
			"Please verify selections from the Fire Recorder Queue are valid.\n\n"
			"Statuses of the items may have been changed.\n\n"
			"Please execute Search again to verify.\n");
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
	
		return; 
	}

 }
 UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	filmGen_ttdl_deleteCb 
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

void	filmGen_ttdl_deleteCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	OP_TTDL_QUEUE_LIST *sPtr1, *sPtr2, *sPtr3, *sPtr4;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL10_LEN+1];
	XmScrollBarCallbackStruct *cbs;
	int sCount;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		clientData = &(glbData.filmClientData);
		sPtr1 = clientData->ttdlQueueList;
		sCount = 0;

		while (sPtr1 != (OP_TTDL_QUEUE_LIST *)NULL)
		{
			if (sPtr1->selectFlag)
			{
				sCount++;
				/* delete this entry */
				sPtr2 = sPtr1->prev;
				sPtr3 = sPtr1->next;

				if ((sPtr2 != (OP_TTDL_QUEUE_LIST *)NULL) &&
						(sPtr3 != (OP_TTDL_QUEUE_LIST *)NULL))
				{
					/* item in between two items */
					sPtr2->next = sPtr3;
					sPtr3->prev = sPtr2;
					free (sPtr1);
					sPtr1 = sPtr3;
				}
				else if ((sPtr2 == (OP_TTDL_QUEUE_LIST *)NULL) &&
								 (sPtr3 == (OP_TTDL_QUEUE_LIST *)NULL))
				{
					/* the only item in the list */ 
					free (sPtr1);
					sPtr1 = (OP_TTDL_QUEUE_LIST *)NULL;
					clientData->ttdlQueueList = (OP_TTDL_QUEUE_LIST *)NULL;
				}
				else if ((sPtr2 != (OP_TTDL_QUEUE_LIST *)NULL))
				{
					/* last item in the list */
					free (sPtr1);
					sPtr1 = (OP_TTDL_QUEUE_LIST *)NULL;
					sPtr2->next = (OP_TTDL_QUEUE_LIST *)NULL;
				}
				else
				{
					/* first item in the list */
					sPtr3->prev = (OP_TTDL_QUEUE_LIST *)NULL;
					free (sPtr1);
					sPtr1 = sPtr3;
					clientData->ttdlQueueList = sPtr1;
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
			msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
			return; 
		}
		else
		{
			/* update position for each item in the ttdlQueueList */
			sPtr1 = clientData->ttdlQueueList;
			sCount = 0;
			while (sPtr1 != (OP_TTDL_QUEUE_LIST *)NULL)
			{
				sPtr1->position = sCount++;
				sPtr1->selectFlag = 0;
				sPtr1 = sPtr1->next;
			}
			clientData->ttdlQueueCount = sCount;

		}

		/* display data in the scroll lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		filmGen_scroll_ttdlListsCb (wgt, NULL, cbs);
		free (cbs);

		/* display total item count in ttdl lists */
		sprintf (buffer, "%d", clientData->ttdlQueueCount);
		XmTextFieldSetString (ttdlTotalItemsTF, buffer);

	}
	UxFilmGenerationContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: filmGen_scroll_laserListsCb
**
** Description:		Updates the laser list widgets (3 of them) with new items
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

void	filmGen_scroll_laserListsCb(
	Widget widget, 
	XtPointer cd, 
	XmScrollBarCallbackStruct *cbs)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *laserQPtr;
	int laserQCount, count, memory;
	int i, temp, j, k, highLightPos[LASER_ROWS];
	Widget sbar;
	XmStringTable orderIdStr, itemIdStr, statusStr;
	char buffer[IMS_COL255_LEN+1];
	int slider;


	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	laserQPtr = clientData->laserQueueList;
	laserQCount = clientData->laserQueueCount;
	memory = 1;
	j = 0;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		laserWindowTop = cbs->value;
		clientData->laserWindowTop = laserWindowTop;
		count = laserQCount - laserWindowTop;

		if (count > LASER_ROWS)
			count = LASER_ROWS;


		/* If no rows to display, clean up fire queue lists */
		if ((laserQCount == 0) || (laserQPtr == NULL) || (count == 0))
		{
			XmListDeleteAllItems (laserOrderIdSL);
			XmListDeleteAllItems (laserItemSL);
			XmListDeleteAllItems (laserStatusSL);

			/* Update Scrollbar position */
			XtVaGetValues(laserDummySW, XmNverticalScrollBar, &sbar, NULL);
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, laserWindowTop,
				XmNsliderSize, 1, 
				NULL);

			XtSetSensitive (laserAddPB, False);

			return;
		}

		/* Allocate memory for Motif Compound Strings */

		orderIdStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		itemIdStr  = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		statusStr	 = (XmStringTable)XtMalloc(count *sizeof(XmString *));

		memory = memory && orderIdStr && itemIdStr && statusStr;

		if (!memory)
		{
			/* Display error messages in message window */

			/* Free compound strings */
			i = count;
			while (--i)
			{
				XmStringFree (orderIdStr[i]);
				XmStringFree (itemIdStr[i]);
				XmStringFree (statusStr[i]);
			}

			XtFree ((char *)orderIdStr);
			XtFree ((char *)itemIdStr);
			XtFree ((char *)statusStr);
			return;
		}

	
		while ((laserQPtr != NULL) && (laserQPtr->position != laserWindowTop))
		{
			laserQPtr = laserQPtr->next;
		}
		
		for (i = 0; i < count && laserQPtr != (OP_LASER_QUEUE_LIST *)NULL; i++)
		{
			if (laserQPtr->order_id)
			{
				sprintf (buffer, "%d", laserQPtr->order_id);
				orderIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				orderIdStr[i] = XmStringCreateLocalized("");

			if (laserQPtr->item_id)
			{
				sprintf (buffer, "%d", laserQPtr->item_id);
				itemIdStr[i] = XmStringCreateLocalized(buffer);
			}
			else
				itemIdStr[i] = XmStringCreateLocalized("");

			if (laserQPtr->status)
			{
				k = 0;
				while ((k < glbData.laser_status_count) && 
				 (laserQPtr->status != glbData.laser_status[k].item_id))
							k++;

				if (k < glbData.laser_status_count)
				{
					statusStr[i] = XmStringCreateLocalized
												(glbData.laser_status[k].item_name);
				}
				else
					/* did not find the matching order_status id */
					statusStr[i] = XmStringCreateLocalized("");

			}
			else
				statusStr[i] = XmStringCreateLocalized("");


			if (laserQPtr->selectFlag)
			{
				highLightPos[j] = i+1;
				j++;
			}

			laserQPtr = laserQPtr->next;

		}

		/* Load all the synchronized arrays to laser queue list widgets */

		XtVaSetValues(
				laserOrderIdSL, XmNitems, orderIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				laserItemSL, XmNitems, itemIdStr, XmNitemCount, count, NULL);
		XtVaSetValues(
				laserStatusSL, XmNitems, statusStr, XmNitemCount, count, NULL);

		/* Free compound strings */
		while (--i)
		{
			XmStringFree (orderIdStr[i]);
			XmStringFree (itemIdStr[i]);
			XmStringFree (statusStr[i]);
		}

		XtFree ((char *)orderIdStr);
		XtFree ((char *)itemIdStr);
		XtFree ((char *)statusStr);
	
		/* Update Scrollbar position */
		XtVaGetValues(laserDummySW, XmNverticalScrollBar, &sbar, NULL);

		temp = laserQCount;
		if (laserQCount > LASER_ROWS)
			slider = LASER_ROWS;
		else
			slider = laserQCount;
		
		if (laserQCount > 0)
			XtVaSetValues(sbar, 
				XmNmaximum, laserQCount,
				XmNvalue, laserWindowTop,
				XmNsliderSize, slider, 
				NULL);
		else
			XtVaSetValues (sbar,
				XmNmaximum, 1,
				XmNvalue, laserWindowTop,
				XmNsliderSize, 1, 
				NULL);


		/* Set selectionPolicy of laser list widgets to MultipleSelect */
		XmListDeselectAllItems(laserOrderIdSL);
		XtVaSetValues(laserOrderIdSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(laserItemSL);
		XtVaSetValues(laserItemSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		XmListDeselectAllItems(laserStatusSL);
		XtVaSetValues(laserStatusSL, 
									XmNselectionPolicy, XmMULTIPLE_SELECT,
									NULL);

		while (j--)
		{
			XmListSelectPos(laserOrderIdSL, highLightPos[j], False);
			XmListSelectPos(laserItemSL, highLightPos[j], False);
			XmListSelectPos(laserStatusSL, highLightPos[j], False);
		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		XtVaSetValues(laserOrderIdSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(laserItemSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
		XtVaSetValues(laserStatusSL, 
									XmNselectionPolicy, XmEXTENDED_SELECT,
									NULL);
	}

	UxFilmGenerationContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: filmGen_laserLists_selectionCb
**
** Description:		Updates the selection of the item(s) in all the laser
**								queue list widgets (3 of them), synchronously.
**
** Arguments:			1. widget 		- Widget that is calling this callback
**								2. listNumber - Each list widget is associated with
** 									 							a unique number, for example, list widget
**																orderIdSL1 has listNumber 1.  
**								3. cbs 				- XmListCallbackStructure
** 
** Return Value:	None
** 
** Revision History:
**
**==========================================================================*/

void filmGen_laserLists_selectionCb(
	Widget widget,
	XtPointer listNumber,
	XmListCallbackStruct *cbs)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *laserQPtr, *tempPtr;
	int listNo;
	int i, k, count, laserQCount;
	int totalItemsSelected; 
	int *selectedItemPos;
	int itemPosition;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		clientData = &(glbData.filmClientData);
		laserQPtr = clientData->laserQueueList;
		laserQCount = clientData->laserQueueCount;
		listNo = (int)listNumber;
		selectedItemPos = cbs->selected_item_positions;
		itemPosition = cbs->item_position;
		totalItemsSelected = cbs->selected_item_count;
		

		/* Set selectionPolicy of item list widgets to MultipleSelect */
		if(listNo != 1)
		{
			XmListDeselectAllItems(laserOrderIdSL);
			XtVaSetValues(laserOrderIdSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XmListDeselectAllItems(laserItemSL);
			XtVaSetValues(laserItemSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XmListDeselectAllItems(laserStatusSL);
			XtVaSetValues(laserStatusSL, 
										XmNselectionPolicy, XmMULTIPLE_SELECT,
										NULL);
		}


		/* Locate screen items in fire queue lists */
		count = laserQCount - laserWindowTop;

		if (count > LASER_ROWS)
			count = LASER_ROWS;

		while ((laserQPtr != NULL) && (laserQPtr->position != laserWindowTop))
		{
			laserQPtr = laserQPtr->next;
		}

		/* Reset the selectFlag for each item first */
		tempPtr = laserQPtr;
		for (i = 0; i < count && tempPtr != NULL; i++)
		{
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}

		for (i = 0; i < totalItemsSelected; i++)
		{
			if (listNo != 1)
				XmListSelectPos(laserOrderIdSL, selectedItemPos[i], False);
			if (listNo != 2)
				XmListSelectPos(laserItemSL, selectedItemPos[i], False);
			if (listNo != 3)
				XmListSelectPos(laserStatusSL, selectedItemPos[i], False);

			/* Locate selected items in queue lists to set the selectFlag */
			tempPtr = laserQPtr;
			for (k = 0;  (k < selectedItemPos[i]-1) && (tempPtr != NULL); k++)
				tempPtr = tempPtr->next;

			tempPtr->selectFlag = 1;

		}

		/* Change list widgets selectionPolicy back to extendedSelect */
		if(listNo != 1)
		{
			XtVaSetValues(laserOrderIdSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 2)
		{
			XtVaSetValues(laserItemSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}
		if(listNo != 3)
		{
			XtVaSetValues(laserStatusSL, 
										XmNselectionPolicy, XmEXTENDED_SELECT,
										NULL);
		}

	}

	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	filmGen_laser_clearCb 
**
** Description:		Callback function for the laser clear button
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

void	filmGen_laser_clearCb(
	Widget wgt,
	XtPointer cd, 
	XtPointer cb)

{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *laserQPtr;
	XmScrollBarCallbackStruct *cbs;
	Widget sbar;
	XmString label;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/* free up fire_queue_list */ 
		(void) free_laser_queueList();

		laserWindowTop = 0;

		/* clean up list & text widgets */
		XmListDeleteAllItems (laserOrderIdSL);
		XmListDeleteAllItems (laserItemSL);
		XmListDeleteAllItems (laserStatusSL);

		XmTextFieldSetString (laserTotalItemsTF, "");

		XtSetSensitive (laserAddPB, False);
		XtSetSensitive (laserClearPB, True);
		XtSetSensitive (updateLaserStatusMPB, False);
		XtSetSensitive (editLaserCommentMPB, False);
		XtSetSensitive (laserItemRegenMPB, False);

		/* Update Scrollbar position */
		XtVaGetValues(laserDummySW, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
			XmNmaximum, 1,
			XmNvalue, laserWindowTop,
			XmNsliderSize, 1, 
			NULL);

		/* Reset Laser Search Status Option Menu */
		XtVaSetValues (laserSearchStatusOM, XmNmenuHistory,
									 laserDummyPB, NULL);

		laserSearchStatus_flag = 0;

	}
	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: filmGen_laser_createQuery 
**
** Description:		Create query to search for laser queue items.
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

int filmGen_laser_createQuery(
	Widget wgt) 
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_FILM_CLIENT_DATA *clientData;
	OP_QUERY_STRUCT *sql;
	int status;
	char *sqlPtr;
	char Msg[IMS_COL1024_LEN+1];
	

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);

	{
		clientData = &(glbData.filmClientData);
		sql = &(clientData->queryStruct);

		/*
		** Initialize op_query_struct 
		*/
		sql->select[0] = sql->from[0] = sql->where[0] = sql->sqlBuf[0] = '\0';
		sql->sPtr = sql->select;
		sql->fPtr = sql->from;
		sql->wPtr = sql->where;
		sqlPtr = sql->sqlBuf;

		strcpy (sql->sPtr, 
						"distinct order_id, item_id, status, op_comment ");

		sql->sPtr = sql->sPtr + strlen(sql->sPtr);

		strcpy (sql->fPtr, "laser_queue");
		sql->fPtr = sql->fPtr + strlen(sql->fPtr);

		/*
		**  Laser Queue Status OptionMenu 
		*/
		if (laserSearchStatus_flag)
		{
			sprintf (sql->wPtr, " status = %d", laserSearchStatus_flag);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}

		/*
		** complete the sql statement
		*/

		if (laserSearchStatus_flag)
		{
			sprintf (sqlPtr,
							 "select %s\nfrom %s\nwhere %s\norder by order_id, item_id",
							 sql->select, sql->from, sql->where);
		}
		else
		{
			sprintf (sqlPtr, "select %s\nfrom %s\norder by order_id, item_id",
											 sql->select, sql->from);
		}
		
		sqlPtr = sqlPtr + strlen(sqlPtr);

	}
	UxFilmGenerationContext = UxSaveCtx;

	return (IMS_OK);
}


/*===========================================================================*
** 
** Function Name: filmGen_laser_executeQuery 
**
** Description:		Execute the query created by filmGen_laser_createQuery.
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

int filmGen_laser_executeQuery(
	Widget wgt) 
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_FILM_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_LASER_QUEUE_LIST *laserQueuePtr;
	int status;
	int itemCount;
	char Msg[IMS_COL1024_LEN+1];


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/*
		** Initialize catalog request structure 
		*/
		clientData = &(glbData.filmClientData);
		catReq = &(clientData->catReq);
		catReq->item[0] = (int *)&itemCount;
		catReq->item[1] = (char *)clientData->queryStruct.sqlBuf;

		if ((status = ims_opCat (catReq, OP_GETLASERQUEUELIST)) < IMS_OK)
		{
			/* Display error messages */
			sprintf(Msg, "Internal Error: laser queue retrieval failed.");   
			msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
			return (IMS_FATAL);
		}

		/* assign returned items to glbData.filmClientData->laserQueueList */
		clientData->laserQueueCount = *(int *)catReq->item[0];
		clientData->laserQueueList = (OP_LASER_QUEUE_LIST *)catReq->item[2];

		if ((clientData->laserQueueList != (OP_LASER_QUEUE_LIST *)NULL) ||
				(clientData->laserQueueCount > 0))
		{
			/* initialize select flag */
			laserQueuePtr = clientData->laserQueueList;

			while (laserQueuePtr != (OP_LASER_QUEUE_LIST *)NULL)
			{
				laserQueuePtr->selectFlag = 0;
				laserQueuePtr = laserQueuePtr->next;

			}
		}
	}

	UxFilmGenerationContext = UxSaveCtx;

	return (IMS_OK);

}


/*===========================================================================*
** 
** Function Name: filmGen_laser_searchCb
**
** Description:		callback function for laser queue search PB.
**								1. create query
**								2. execute query
**								3. display results
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

void	filmGen_laser_searchCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	int k, status, new_id, generated_id;
	Widget sbar;
	char buffer[IMS_COL10_LEN+1];
	XmScrollBarCallbackStruct *cbs;
	XmString label;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		clientData = &(glbData.filmClientData);

		/* free up laser_queue_list  */ 
		(void) free_laser_queueList();

		laserWindowTop = 0;

		/* clean up list & text widgets */
		XmListDeleteAllItems (laserOrderIdSL);
		XmListDeleteAllItems (laserItemSL);
		XmListDeleteAllItems (laserStatusSL);
		XmTextFieldSetString (laserTotalItemsTF, "");

		/* Update Scrollbar position */
		XtVaGetValues(laserDummySW, XmNverticalScrollBar, &sbar, NULL);
		XtVaSetValues (sbar,
			XmNmaximum, 1,
			XmNvalue, laserWindowTop,
			XmNsliderSize, 1, 
			NULL);

		if ((status = filmGen_laser_createQuery (glbData.filmW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);
			return;
		}

		if ((status = filmGen_laser_executeQuery (glbData.filmW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);
			return;
		}

		/* display laser queue list */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		filmGen_scroll_laserListsCb (glbData.filmW, NULL, cbs);
		free (cbs);

		/* display laser queue item count */
		sprintf (buffer, "%d", clientData->laserQueueCount);
		XmTextFieldSetString (laserTotalItemsTF, buffer);


		/*
		** sensitize laserAddPB and updateLaserStatusMPB if 
		** count is > 0, desensitize them otherwise. 
		*/
		if (clientData->laserQueueCount > 0)
		{
			XtSetSensitive (updateLaserStatusMPB, True);
			XtSetSensitive (editLaserCommentMPB, True);
			XtSetSensitive (laserItemRegenMPB, True);
			XtSetSensitive (laserAddPB, True);
		}
		else
		{
			XtSetSensitive (updateLaserStatusMPB, False);
			XtSetSensitive (editLaserCommentMPB, False);
			XtSetSensitive (laserItemRegenMPB, False);
			XtSetSensitive (laserAddPB, False);
		}

	
		/* Change cursor back to normal*/
		timeOutCursors (False);

	}

	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	filmGen_laser_addCb 
**
** Description:		Callback function for the laser queue ADD button,
**								adds the selected items from laser queue to ttdl queue.
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

void	filmGen_laser_addCb(
	Widget wgt, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_FILM_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_LASER_QUEUE_LIST *lPtr, *laserQPtr;
	OP_TTDL_QUEUE_LIST *sPtr, *nPtr, *tempPtr;
	OP_LASER_QUEUE_LIST *qPtr;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL10_LEN+1];
	int status, sCount, invalid_status;
	DBSMALLINT current_status;
	XmScrollBarCallbackStruct *cbs;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	clientData = &(glbData.filmClientData);
	catReq = &(clientData->catReq);
	qPtr = (OP_LASER_QUEUE_LIST *)clientData->laserQueueList;
	sPtr = nPtr = tempPtr = (OP_TTDL_QUEUE_LIST *)NULL;

	/* find out how many items are selected */
	sCount = 0;
	invalid_status = IMS_FALSE;
	while (qPtr != NULL) 
	{
		/* item is added to the TTDL only if it has status NEW */
		if (qPtr->selectFlag)
		{
			/*
			** call getLaserQueueStatus to verify that the item status
			** is NEW in the laser_queue table 
			*/
			catReq->item[0] = (DBINT *)&qPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&qPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&current_status;
			if ((status = ims_opCat(catReq, OP_GETLASERITEMSTATUS)) < 0)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nAdd Laser Queue Items to TTDL Queue,\n"
						 "OP_GETLASERITEMSTATUS failed for Order: %d, Item: %d\n", 
						 qPtr->order_id, qPtr->item_id);   
				msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

				qPtr = qPtr->next;
				continue;

			}

			/* 
			** Add to TTDL queue is allowed only if the item's
			** current status is NEW 
			*/
			if (current_status != LASER_NEW)
			{
				invalid_status = IMS_TRUE;
			}
			else
			{
				/* check for duplicate copies in select list */
				sCount++;   
				sPtr = clientData->ttdlQueueList;
				if (sPtr != (OP_TTDL_QUEUE_LIST *)NULL)
				{	
					/* Find the right place in the select list to insert */
					while ((sPtr->next != (OP_TTDL_QUEUE_LIST *)NULL) &&
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

				/* we have a new item to add to the ttdl list */
				if ((nPtr = malloc(sizeof(OP_TTDL_QUEUE_LIST))) == NULL)
				{
					/* malloc failed, display message and return */
					strcpy(Msg, "Memory allocation failed.");
					msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

					return; 
				}
				else
				{
					nPtr->prev = (OP_TTDL_QUEUE_LIST *)NULL;
					nPtr->next = (OP_TTDL_QUEUE_LIST *)NULL;
					nPtr->order_id = qPtr->order_id;
					nPtr->item_id = qPtr->item_id;
					nPtr->status = qPtr->status;
					nPtr->selectFlag = 0;
					nPtr->position = 0;
					nPtr->queue_type = LASER;
					
					/* insert new item into the ttdl list */
					if (sPtr == (OP_TTDL_QUEUE_LIST *)NULL)
					{
						clientData->ttdlQueueList = nPtr;
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


							if (tempPtr != (OP_TTDL_QUEUE_LIST *)NULL)
								tempPtr->next = nPtr;
							else 
								clientData->ttdlQueueList = nPtr;

						}
						else
						{
							/* insert after */
							tempPtr = sPtr->next;
							sPtr->next = nPtr;
							nPtr->prev = sPtr;
							nPtr->next = tempPtr;
							if (tempPtr != (OP_TTDL_QUEUE_LIST *)NULL)
								tempPtr->prev = nPtr;
						}
					}
				}
			}
		}
		qPtr = qPtr->next;

	} /* while */

	if (sCount > 0)
	{
		/* update position for each item in the ttdlQueueList */
		tempPtr = clientData->ttdlQueueList;
		sCount = 0;
		while (tempPtr != (OP_TTDL_QUEUE_LIST *)NULL)
		{
			tempPtr->position = sCount++;
			tempPtr->selectFlag = 0;
			tempPtr = tempPtr->next;
		}
		clientData->ttdlQueueCount = sCount;

		/* display data in the ttdl lists */
		cbs = (XmScrollBarCallbackStruct *)
			malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		filmGen_scroll_ttdlListsCb (wgt, NULL, cbs);
		free (cbs);

		/* display total item count in ttdl lists */
		sprintf (buffer, "%d", clientData->ttdlQueueCount);
		XmTextFieldSetString (ttdlTotalItemsTF, buffer);

		/* sensitize Delete & Process pushbuttons */
		XtSetSensitive (ttdlDeletePB, True);
		XtSetSensitive (ttdlProcessPB, True);
	}

	if ((sCount <= 0) || (invalid_status)) 
	{
		/* nothing has been selected, display message and return */
		strcpy(Msg, "Only items with status NEW can be added to TTDL Queue.\n\n"  
								"Please verify selections from the Laser Queue are valid.\n\n"
								"Status of the items may have been changed.\n\n"
								"Please execute Search again to verify.\n");
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
	
		return; 
	}

 }
 UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	free_laser_queueList
**
** Description:		Function to free the laser_queue_list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_laser_queueList()
{
	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *qPtr, *qNextPtr; 

	clientData = &(glbData.filmClientData);
	qPtr = clientData->laserQueueList;

	/* free up the clientData fireQueueList */
	if (qPtr != (OP_LASER_QUEUE_LIST *)NULL)
	{
		while (qPtr != (OP_LASER_QUEUE_LIST *)NULL)
		{
			qNextPtr = qPtr->next;
			free(qPtr);
			qPtr = qNextPtr;
		}

		clientData->laserQueueList = (OP_LASER_QUEUE_LIST *)NULL;
		clientData->laserQueueCount = 0;
	}
}


/*===========================================================================*
** 
** Function Name: filmGen_fireStatus_updateCb
**
** Description:		Callback function for fire queue status update 
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
/* ARGSUSED0 */
void	filmGen_fireStatus_updateCb( 
	Widget widget, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, itemCount;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	catReq = &(clientData->catReq);
	fireQPtr = clientData->fireQueueList;
	itemCount = clientData->fireQueueCount;


	/* Find the first selected item in the fire queue list */
	i = 0;
	while ((fireQPtr != (OP_FIRE_QUEUE_LIST *)NULL) && (i < itemCount) && 
			(fireQPtr->selectFlag == 0))
	{
		i++;
		fireQPtr = fireQPtr->next;
	}

	if ((fireQPtr == (OP_FIRE_QUEUE_LIST *)NULL) || (i >= itemCount))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
		return;
	}

	fireQPtr = clientData->fireQueueList;
	/* Found the first selected item, create selection dialog */
	selectionDlg_popupCb (glbData.filmW, (void *)fireQPtr, FIRE_STATUS);
	

 }
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: filmGen_laserStatus_updateCb
**
** Description:		Callback function for laser queue status update 
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
/* ARGSUSED0 */
void	filmGen_laserStatus_updateCb( 
	Widget widget, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *laserQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, itemCount;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	catReq = &(clientData->catReq);
	laserQPtr = clientData->laserQueueList;
	itemCount = clientData->laserQueueCount;


	/* Find the first selected item in the laser queue list */
	i = 0;
	while ((laserQPtr != (OP_LASER_QUEUE_LIST *)NULL) && (i < itemCount) && 
			(laserQPtr->selectFlag == 0))
	{
		i++;
		laserQPtr = laserQPtr->next;
	}

	if ((laserQPtr == (OP_LASER_QUEUE_LIST *)NULL) || (i >= itemCount))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
		return;
	}

	laserQPtr = clientData->laserQueueList;
	/* Found the first selected item, create selection dialog */
	selectionDlg_popupCb (glbData.filmW, (void *)laserQPtr, LASER_STATUS);
	

 }
	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	filmGen_printCb
**
** Description:		Callback function for the PRINT button,
**								prints the Film Generation screen.
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

void	filmGen_printCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.filmW);

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	filmGen_goto_welcomeCb
**
** Description:		Callback function for the Go To Welcome button.
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

void	filmGen_goto_welcomeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		XtPopup(XtParent(glbData.welcomeW), XtGrabNone);
		glbData.welcomeFlag = 1;

	}
	UxFilmGenerationContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name:	filmGen_closeCb 
**
** Description:		Callback function for the CLOSE button,
**								pops down the Film Generation screen.
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

void	filmGen_closeCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	XmScrollBarCallbackStruct *cbs;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

		/*
		** call filmGen_fire_clearCb to clear up the widgets and free
		** up data structure
		*/
		(void) filmGen_fire_clearCb (glbData.filmW, NULL, NULL);

		/*
		** call filmGen_laser_clearCb to clear up the widgets and free
		** up data structure
		*/
		(void) filmGen_laser_clearCb (glbData.filmW, NULL, NULL);

		/* free up ttdl queue lists */
		(void) free_ttdl_queueList();

		/* clean up TTDL Queue lists */
		cbs = (XmScrollBarCallbackStruct *)
						malloc(sizeof(XmScrollBarCallbackStruct));
		cbs->value = 0;
		filmGen_scroll_ttdlListsCb (wgt, NULL, cbs);
		free (cbs);

		/* pop down the screen */
		XtPopdown (XtParent(glbData.filmW));
		glbData.filmFlag = 0;

	}
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	filmGen_ttdl_processCb
**
** Description:		Callback function for the Process button,
**				        calls ims_filmList() to process the ttdl list.
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

void	filmGen_ttdl_processCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_FILM_CLIENT_DATA *clientData;
	OP_TTDL_QUEUE_LIST *ttdlQPtr;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	OP_LASER_QUEUE_LIST *laserQPtr;
	FILM_REQUEST_LIST *filmItemList, *nPtr, *fPtr, *flastPtr;
	XmScrollBarCallbackStruct *cbs;
	int k, found, status, ttdlQCount;
	int laserCount, fireCount;
	char Msg[IMS_COL1024_LEN+1];
	char buffer[IMS_COL10_LEN+1];
	DBSMALLINT in_fire_id, in_laser_id;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		/* assign client to filmClientData from glbData structure */
		clientData = &(glbData.filmClientData);
		catReq = &(clientData->catReq);
		userSpec = &(catReq->userSpec);
		ttdlQPtr = clientData->ttdlQueueList;
		ttdlQCount = clientData->ttdlQueueCount;
		filmItemList = (FILM_REQUEST_LIST *)NULL;
		laserCount = fireCount = 0;

		/* go through the clientData->ttdlQueueList and build filmList */
		while (ttdlQPtr != (OP_TTDL_QUEUE_LIST *)NULL)
		{
			/* we have a new item to add to the film list */
			if ((nPtr = malloc(sizeof(FILM_REQUEST_LIST))) == NULL)
			{
				/* malloc failed, display message and return */
				strcpy(Msg, "Memory allocation failed.");
				msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

				/* Change cursor back to normal */
				timeOutCursors (False);
				return; 
			}
			else
			{
				nPtr->next = (FILM_REQUEST_LIST *)NULL;
				nPtr->order_id = ttdlQPtr->order_id;
				nPtr->item_id = ttdlQPtr->item_id;
				nPtr->film_target = ttdlQPtr->queue_type;
				nPtr->status = ttdlQPtr->status;

				/* insert new item into the ttdl list */
				if (filmItemList == (FILM_REQUEST_LIST *)NULL)
				{
					filmItemList = nPtr;
				}
				else
				{
					fPtr = filmItemList;
					while (fPtr != (FILM_REQUEST_LIST *)NULL)
					{
						flastPtr = fPtr;
						fPtr = fPtr->next;
					}
					/* insert item at the end of filmItemList */ 
					flastPtr->next = nPtr;	

				}
			}

			ttdlQPtr = ttdlQPtr->next;
		}


		/*
		** call ims_filmList() to process ttdlQueueList and generate 
		** TTDL file. If ims_filmList() fails, status of each item in
		** the ttdlQueueList is not changed, error message is post to
		** inform the user which item fails.  If ims_filmList() is 
		** successful, update each ttdlQueueList status to be either
		** IN-FIRE or IN-LASER in the fire_queue or laser_queue table
		** accordingly.
		*/

		if (filmItemList == (FILM_REQUEST_LIST *)NULL)
		{
			/* Could not generate filmItemList */
			strcpy(Msg, "Internal Error, could not generate film item list.");
			msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

			/* Change cursor back to normal */
			timeOutCursors (False);
			return; 
		}
		else
		{
			if ((status = ims_filmList (catReq->msgDesc,
								(char *)&catReq->userSpec, filmItemList)) 
								< IMS_OK)
			{
				/* Search through filmItemList and see which one failed */
				/* post error message to inform the user which item failed */
				/* and ask user to remove that entry from ttdl_list */
				/* then they can process again. */

				found = IMS_FALSE;
				fPtr = filmItemList;
				while ((fPtr != (FILM_REQUEST_LIST *)NULL) && (!found))
				{
					if (fPtr->status == ITEM_ERROR)
						found = IMS_TRUE;
					else	
						fPtr = fPtr->next;
				}

				if ((fPtr != (FILM_REQUEST_LIST *)NULL) && (found))
				{
					sprintf(Msg,
									"Order ID: %d, Item ID: %d could not be added to TTDL.\n\n"
									"FPS things-to-do-list could not be generated.\n\n"
									"Please remove this item from the TTDL queue and try to "
									"process again.\n",
									fPtr->order_id, fPtr->item_id);	
				}
				else
				{
					/* ims_filmList() failed, display message and return */
					strcpy(Msg, "Internal Error, ims_filmList() failed.\n\n"
									"FPS things-to-do-list could not be generated.\n\n"
									"Please check the log file for detailed error messages.\n\n");
				}

				msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

				/* free up filmItemList */
				(void) free_film_itemList (&filmItemList);

				/* Change cursor back to normal */
				timeOutCursors (False);
				return; 

			}
			else
			{
				/*
				** ims_filmList() returns IMS_OK, update each item status 
				** in the TTDL list to be IN-FIRE or IN-LASER.
				*/

				in_fire_id = FIRE_IN_FIRE;
				in_laser_id = LASER_IN_LASER;
				
				/*
				** update the status for each item in the fire_queue
			  ** or the laser_queue table to be IN-FIRE or IN-LASER
				*/
				ttdlQPtr = clientData->ttdlQueueList;
				while (ttdlQPtr != (OP_TTDL_QUEUE_LIST *)NULL)
				{
					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Change cursor back to normal */
						timeOutCursors (False);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate TTDL Queue Item Status,\n"
										 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
										 ttdlQPtr->order_id, ttdlQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

						/* continue to process the next item */
						ttdlQPtr = ttdlQPtr->next;
						continue;

					}

					if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Change cursor back to normal */
						timeOutCursors (False);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate TTDL Queue Item Status,\n"
												 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
												 ttdlQPtr->order_id, ttdlQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
						/* continue to process the next item */
						ttdlQPtr = ttdlQPtr->next;
						continue;

					}


					if (ttdlQPtr->queue_type == FIRE) 
					{
						catReq->item[0] = (DBINT *)&ttdlQPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&ttdlQPtr->item_id;
						catReq->item[2] = (DBSMALLINT *)&in_fire_id;
						if ((status = ims_opCat(catReq, OP_UPDATEFIREITEMSTATUS)) < 0)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Change cursor back to normal */
							timeOutCursors (False);

							/* Display error messages */
							sprintf(Msg, "Internal Error:\nUpdate Fire Queue Item Status,\n"
									 "OP_UPDATEFIREITEMSTATUS failed for Order: %d, Item: %d\n", 
									 ttdlQPtr->order_id, ttdlQPtr->item_id);   
							msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

							/* continue to process the next item */
							ttdlQPtr = ttdlQPtr->next;
							continue;

						}

						/* increment fireCount */
						fireCount++;

					} 
					else
					{
						catReq->item[0] = (DBINT *)&ttdlQPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&ttdlQPtr->item_id;
						catReq->item[2] = (DBSMALLINT *)&in_laser_id;
						if ((status = ims_opCat(catReq, OP_UPDATELASERITEMSTATUS)) < 0)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Change cursor back to normal */
							timeOutCursors (False);

							/* Display error messages */
							sprintf(Msg, "Internal Error:\nUpdate Laser Queue Item Status,\n"
								 "OP_UPDATELASERITEMSTATUS failed for Order: %d, Item: %d\n", 
								 ttdlQPtr->order_id, ttdlQPtr->item_id);   
							msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

							/* continue to process the next item */
							ttdlQPtr = ttdlQPtr->next;
							continue;

						}

						/* increment laserCount */
						laserCount++;

					}

					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Change cursor back to normal */
						timeOutCursors (False);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate TTDL Queue Item Status,\n"
									 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
									 ttdlQPtr->order_id, ttdlQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

						return;
					}

					ttdlQPtr = ttdlQPtr->next;
				}
				

				/* successfully generated filmList */
			  strcpy(Msg, "FPS things-to-do-list has been generated successfully.");
				msgBoxDlg_popupCb (glbData.filmW, IMS_OK, Msg); 


				/* update item status to be IN-FIRE in the fireQueueList */	
				if (fireCount)
				{
					fireQPtr = clientData->fireQueueList;
					ttdlQPtr = clientData->ttdlQueueList;
					fireCount = 0;
					while (ttdlQPtr != (OP_TTDL_QUEUE_LIST *)NULL)
					{
						fireQPtr = clientData->fireQueueList;
						while (fireQPtr != (OP_FIRE_QUEUE_LIST *)NULL)
						{
							if ((fireQPtr->order_id == ttdlQPtr->order_id) &&
								  (fireQPtr->item_id == ttdlQPtr->item_id))
							{
								fireQPtr->status = in_fire_id;
								fireCount++;
							}

							fireQPtr = fireQPtr->next;
						}

						ttdlQPtr = ttdlQPtr->next;
					}

					/* refresh the Fire Recorder Queue only if one or more */
					/* item status is updated */ 
					if (fireCount)
					{
						cbs = (XmScrollBarCallbackStruct *)
									malloc(sizeof(XmScrollBarCallbackStruct));
						cbs->value = 0;
						filmGen_scroll_fireListsCb (wgt, NULL, cbs);
						free (cbs);
					}

				}


				/* update item status to be IN-LASER in the laserQueueList */	
				if (laserCount)
				{
					laserQPtr = clientData->laserQueueList;
					ttdlQPtr = clientData->ttdlQueueList;
					laserCount = 0;
					while (ttdlQPtr != (OP_TTDL_QUEUE_LIST *)NULL)
					{
						laserQPtr = clientData->laserQueueList;
						while (laserQPtr != (OP_LASER_QUEUE_LIST *)NULL)
						{
							if ((laserQPtr->order_id == ttdlQPtr->order_id) &&
								  (laserQPtr->item_id == ttdlQPtr->item_id))
							{
								laserQPtr->status = in_laser_id;
								laserCount++;
							}

							laserQPtr = laserQPtr->next;
						}

						ttdlQPtr = ttdlQPtr->next;
					}

					/* refresh the Laser Tech Queue only if one or more */
					/* item status is updated */ 
					if (laserCount)
					{
						cbs = (XmScrollBarCallbackStruct *)
									malloc(sizeof(XmScrollBarCallbackStruct));
						cbs->value = 0;
						filmGen_scroll_laserListsCb (wgt, NULL, cbs);
						free (cbs);
					}

				}

				/* clean up filmItemList */
				(void) free_film_itemList (&filmItemList);

				/* delete TTDL queue list */
				(void) free_ttdl_queueList();

				/* clean up TTDL Queue lists */
				cbs = (XmScrollBarCallbackStruct *)
							malloc(sizeof(XmScrollBarCallbackStruct));
				cbs->value = 0;
				filmGen_scroll_ttdlListsCb (wgt, NULL, cbs);
				free (cbs);

				/* display total item count in ttdl lists */
				sprintf (buffer, "%d", clientData->ttdlQueueCount);
				XmTextFieldSetString (ttdlTotalItemsTF, buffer);

				/* de-sensitize Delete & Process pushbuttons */
				XtSetSensitive (ttdlDeletePB, False);
				XtSetSensitive (ttdlProcessPB, False);

			}

		} /* if (filmList) */

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxFilmGenerationContext = UxSaveCtx;
}




/*===========================================================================*
** 
** Function Name:	filmGen_fire_commentCb 
**
** Description:		Callback function for the View Fire Item Comment menu item.
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
/* ARGSUSED0 */
void	filmGen_fire_commentCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, count, fireQCount;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	fireQPtr = clientData->fireQueueList;
	fireQCount = clientData->fireQueueCount;


	/* Locate screen items in fireQueueList */
	count = fireQCount - fireWindowTop;
	if (count > FIRE_ROWS)
		count = FIRE_ROWS;

	while ((fireQPtr != NULL) && (fireQPtr->position != fireWindowTop))
	{
		fireQPtr = fireQPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((fireQPtr != (OP_FIRE_QUEUE_LIST *)NULL) && (i < count) && 
			(fireQPtr->selectFlag == 0))
	{
		i++;
		fireQPtr = fireQPtr->next;
	}

	if ((fireQPtr == (OP_FIRE_QUEUE_LIST *)NULL) || (i >= count))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
		return;
	}


	/* Found the first selected fire queue item, popup the comment dialog */
	
	commentDlg_popupCb (glbData.filmW, (void *)fireQPtr, FIRE_COMMENT);

	}
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	filmGen_laser_commentCb 
**
** Description:		Callback function for the View Laser Item Comment menu item.
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
/* ARGSUSED0 */
void	filmGen_laser_commentCb(
	Widget wgt, 
	XtPointer cd, 
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *laserQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status;
	int i, count, laserQCount;

	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	laserQPtr = clientData->laserQueueList;
	laserQCount = clientData->laserQueueCount;


	/* Locate screen items in fireQueueList */
	count = laserQCount - laserWindowTop;
	if (count > LASER_ROWS)
		count = LASER_ROWS;

	while ((laserQPtr != NULL) && (laserQPtr->position != laserWindowTop))
	{
		laserQPtr = laserQPtr->next;
	}

	/* Find the first selected item in the current screen */
	i = 0;
	while ((laserQPtr != (OP_LASER_QUEUE_LIST *)NULL) && (i < count) && 
			(laserQPtr->selectFlag == 0))
	{
		i++;
		laserQPtr = laserQPtr->next;
	}

	if ((laserQPtr == (OP_LASER_QUEUE_LIST *)NULL) || (i >= count))
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.");   
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 
		return;
	}


	/* Found the first selected fire queue item, popup the comment dialog */
	
	commentDlg_popupCb (glbData.filmW, (void *)laserQPtr, LASER_COMMENT);

	}
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: filmGen_fire_regenCb
**
** Description:		Callback function for fire item regeneration 
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
/* ARGSUSED0 */
void	filmGen_fire_regenCb( 
	Widget widget, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_FILM_CLIENT_DATA *clientData;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status, sCount, invalid_status;
	int i, modifyFlag;
	DBSMALLINT current_status, order_item_status;
	XmScrollBarCallbackStruct *scroll_cbs;
	DBSMALLINT order_item_type, media_class;
	DBSMALLINT process_type, step_sequence;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	/* Change cursor back to normal */
	timeOutCursors (True);

	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	catReq = &(clientData->catReq);
	fireQPtr = clientData->fireQueueList;

	/* regenerate the selected item(s) in the fire queue list */
	sCount = 0;
	modifyFlag = IMS_FALSE;
	invalid_status = IMS_FALSE;

	while (fireQPtr != (OP_FIRE_QUEUE_LIST *)NULL)
	{
		if (fireQPtr->selectFlag)
		{
			sCount++;  
			if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nRegenerate Fire Item,\n"
							 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
				msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

				fireQPtr = fireQPtr->next;
				continue;
			}

			if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nRegenerate Fire Item,\n"
										 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
										 fireQPtr->order_id, fireQPtr->item_id);   
				msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
				fireQPtr = fireQPtr->next;
				continue;
			}

			/*
			** call getFireItemStatus to verify that the item status
			** is NEW in the fire_queue table 
			*/
			catReq->item[0] = (DBINT *)&fireQPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&current_status;
			if ((status = ims_opCat(catReq, OP_GETFIREITEMSTATUS)) < 0)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nRegenerate Fire Item,\n"
						 "OP_GETFIREITEMSTATUS failed for Order: %d, Item: %d\n", 
						 fireQPtr->order_id, fireQPtr->item_id);   
				msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

				fireQPtr = fireQPtr->next;
				continue;
			}

			/*
			** PPS Regenerate is allowed only for items with status 
			** values NEW, GENERATED, ERROR, FAILED
			*/

			if ((current_status == FIRE_NEW) || (current_status == FIRE_GENERATED) 
				  || (current_status == FIRE_ERROR) || (current_status == FIRE_FAILED))
			{
				/*
				** Update item status to VALIDATED in order_item table
				*/
				order_item_status = ITEM_VALIDATED;
				catReq->item[0] = (DBINT *)&fireQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&order_item_status;
				if ((status = ims_opCat(catReq, OP_UPDATEITEMSTATUS)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Fire Item PPS Regeneration,\n"
							 "OP_UPDATEITEMSTATUS failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					fireQPtr = fireQPtr->next;
					continue;
				}

				/*
				** 04/26/96 - get order item information necessary to
				** get the step info.
				*/
				catReq->item[0] = (DBINT *)&fireQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&order_item_type;
				catReq->item[3] = (DBSMALLINT *)&process_type;
				catReq->item[4] = (DBSMALLINT *)&media_class;
				if ((status = ims_opCat(catReq, OP_GETORDERITEMINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Fire Item PPS Regeneration,\n"
							 "OP_GETORDERITEMINFO failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					fireQPtr = fireQPtr->next;
					continue;
				}

				/*
				** 04/26/96 - get step sequence by executing get_step_info
				** stored procedure.
				*/
				catReq->item[0] = (DBSMALLINT *)&order_item_type;
				catReq->item[1] = (DBSMALLINT *)&media_class;
				catReq->item[2] = (DBSMALLINT *)&process_type;
				catReq->item[3] = (DBSMALLINT *)&order_item_status;
				catReq->item[4] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_GETITEMSTEPINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Fire Item PPS Regeneration,\n"
							 "OP_GETITEMSTEPINFO failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					fireQPtr = fireQPtr->next;
					continue;
				}

				/*
				** The new step_sequence is the current step_sequence - 1
				*/
				step_sequence-- ;

				/*
				** 04/26/96 - Update item step_sequence, step_name and step_started_p
				** This is to correct problem where ims_do_next_step reported
				** inconsistent step started status.
				*/
				catReq->item[0] = (DBINT *)&fireQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_UPDATEITEMSTEPINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Fire Item PPS Regeneration,\n"
							 "OP_UPDATEITEMSTEPINFO failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					fireQPtr = fireQPtr->next;
					continue;
				}


				/*
				** call removeFireQueueItem to remove the item from fire_queue
				*/
				catReq->item[0] = (DBINT *)&fireQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
				if ((status = ims_opCat(catReq, OP_REMOVEFIREQUEUEITEM)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nRegenerate Fire Item,\n"
							 "OP_REMOVEFIREQUEUEITEM failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					fireQPtr = fireQPtr->next;
					continue;
				}


				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nRegenerate Fire Item,\n"
								 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
								 fireQPtr->order_id, fireQPtr->item_id);   
					msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

					fireQPtr = fireQPtr->next;
					continue;
				}

				modifyFlag = IMS_TRUE;

			}
			else
			{
				/* item status does not allow PPS status rollback */
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
				invalid_status = IMS_TRUE;
			}
			
		} /* if (selectFlag) */
		fireQPtr = fireQPtr->next;

	} /* while */


	if (sCount == 0)
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.\n");   

		/* Change cursor back to normal */
		timeOutCursors (False);
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

		return;
	}

	if ((invalid_status) && (modifyFlag))
	{
		/*
		** Refresh fire queue lists 
		*/
		(void) filmGen_fire_searchCb(glbData.filmW, NULL, NULL);

		sprintf(Msg, "Fire Item PPS Regeneration:\n\n"
		 "PPS regeneration is allowed for items with the \n\n"
		 "following status values: NEW, GENERATED, FAILED, ERROR.\n\n"
		 "Part of the items selected in the Fire Recorder Queue do not qualify "
		 "this condition.\n\n\n"
		 "Items qualify the condition are removed from the Fire Recorder Queue.\n\n"
		 "Statuses of these items have been updated to VALIDATED in the "
		 "order item queue.\n\n"
		 "These items will be processed by PPS in the near future.\n");

		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

	}
	else
		if (invalid_status)
		{
			/* Display error messages */
			sprintf(Msg, "Fire Item PPS Regeneration:\n\n"
				 "PPS regeneration is allowed for items with the \n\n"
				 "following status values: NEW, GENERATED, FAILED, ERROR.\n\n"
				 "Selections in the Fire Recorder Queue do not qualify the "
				 "condition.\n");

			msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

		}
		else	
			if (modifyFlag)
			{
				/*
				** Refresh fire queue lists 
				*/
				(void) filmGen_fire_searchCb(glbData.filmW, NULL, NULL);

				sprintf(Msg, "Fire Item PPS Regeneration:\n\n"
					"Selected items are removed from the Fire Recorder Queue.\n\n"
			 		"Statuses of these items have been updated to VALIDATED in the "
			 		"order item queue.\n\n"
					"These items will be processed by PPS in the near future.\n");

				msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

			}

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: filmGen_laser_regenCb
**
** Description:		Callback function for laser item regeneration 
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
/* ARGSUSED0 */
void	filmGen_laser_regenCb( 
	Widget widget, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCfilmGeneration 			*UxSaveCtx, *UxContext;
	OP_CAT_STRUCT *catReq;
	OP_FILM_CLIENT_DATA *clientData;
	OP_LASER_QUEUE_LIST *laserQPtr;
	char Msg[IMS_COL1024_LEN+1];
	int status, sCount, invalid_status;
	int i, modifyFlag;
	DBSMALLINT current_status, order_item_status;
	XmScrollBarCallbackStruct *scroll_cbs;
	DBSMALLINT order_item_type, media_class;
	DBSMALLINT process_type, step_sequence;


	UxSaveCtx = UxFilmGenerationContext;
	UxFilmGenerationContext = UxContext =
			(_UxCfilmGeneration *) UxGetContext(glbData.filmW);
	{

	/* Change cursor back to normal */
	timeOutCursors (True);

	/* assign client to filmClientData from glbData structure */
	clientData = &(glbData.filmClientData);
	catReq = &(clientData->catReq);
	laserQPtr = clientData->laserQueueList;

	/* regenerate the selected item(s) in the laser queue list */
	sCount = 0;
	modifyFlag = IMS_FALSE;
	invalid_status = IMS_FALSE;

	while (laserQPtr != (OP_LASER_QUEUE_LIST *)NULL)
	{
		if (laserQPtr->selectFlag)
		{
			sCount++;  
			if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
			{
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nRegenerate Laser Item,\n"
							 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   
				msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

				laserQPtr = laserQPtr->next;
				continue;
			}

			if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nRegenerate Laser Item,\n"
										 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
										 laserQPtr->order_id, laserQPtr->item_id);   
				msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
				laserQPtr = laserQPtr->next;
				continue;
			}

			/*
			** call getLaserItemStatus to verify item status in 
			** laser_queue table 
			*/
			catReq->item[0] = (DBINT *)&laserQPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&current_status;
			if ((status = ims_opCat(catReq, OP_GETLASERITEMSTATUS)) < 0)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(Msg, "Internal Error:\nRegenerate Laser Item,\n"
						 "OP_GETLASERITEMSTATUS failed for Order: %d, Item: %d\n", 
						 laserQPtr->order_id, laserQPtr->item_id);   
				msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

				laserQPtr = laserQPtr->next;
				continue;
			}

			/*
			** PPS Regenerate is allowed only for items with status 
			** values NEW, GENERATED, ERROR, FAILED 
			*/

			if ((current_status == LASER_NEW) || (current_status == LASER_GENERATED) 
			  || (current_status == LASER_ERROR) || (current_status == LASER_FAILED))
			{
				/*
				** Update item status to VALIDATED in order_item table
				*/
				order_item_status = ITEM_VALIDATED;
				catReq->item[0] = (DBINT *)&laserQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&order_item_status;
				if ((status = ims_opCat(catReq, OP_UPDATEITEMSTATUS)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Laser Item PPS Regeneration,\n"
							 "OP_UPDATEITEMSTATUS failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					laserQPtr = laserQPtr->next;
					continue;
				}


				/*
				** 04/26/96 - get order item information necessary to
				** get the step info.
				*/
				catReq->item[0] = (DBINT *)&laserQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&order_item_type;
				catReq->item[3] = (DBSMALLINT *)&process_type;
				catReq->item[4] = (DBSMALLINT *)&media_class;
				if ((status = ims_opCat(catReq, OP_GETORDERITEMINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Laser Item PPS Regeneration,\n"
							 "OP_GETORDERITEMINFO failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					laserQPtr = laserQPtr->next;
					continue;
				}

				/*
				** 04/26/96 - get step sequence by executing get_step_info
				** stored procedure.
				*/
				catReq->item[0] = (DBSMALLINT *)&order_item_type;
				catReq->item[1] = (DBSMALLINT *)&media_class;
				catReq->item[2] = (DBSMALLINT *)&process_type;
				catReq->item[3] = (DBSMALLINT *)&order_item_status;
				catReq->item[4] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_GETITEMSTEPINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Laser Item PPS Regeneration,\n"
							 "OP_GETITEMSTEPINFO failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					laserQPtr = laserQPtr->next;
					continue;
				}

				/*
				** The new step_sequence is the current step_sequence - 1
				*/
				step_sequence-- ;

				/*
				** 04/26/96 - Update item step_sequence, step_name and step_started_p
				** This is to correct problem where ims_do_next_step reported
				** inconsistent step started status.
				*/
				catReq->item[0] = (DBINT *)&laserQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&step_sequence;
				if ((status = ims_opCat(catReq, OP_UPDATEITEMSTEPINFO)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Laser Item PPS Regeneration,\n"
							 "OP_UPDATEITEMSTEPINFO failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					laserQPtr = laserQPtr->next;
					continue;
				}

				/*
				** call removeLaserQueueItem to remove the item from laser_queue
				*/
				catReq->item[0] = (DBINT *)&laserQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
				if ((status = ims_opCat(catReq, OP_REMOVELASERQUEUEITEM)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nRegenerate Laser Item,\n"
							 "OP_REMOVELASERQUEUEITEM failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   
					msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

					laserQPtr = laserQPtr->next;
					continue;
				}


				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nRegenerate Laser Item,\n"
								 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
								 laserQPtr->order_id, laserQPtr->item_id);   
					msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

					laserQPtr = laserQPtr->next;
					continue;
				}

				modifyFlag = IMS_TRUE;

			}
			else
			{
				/* item status does not allow PPS status rollback */
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
				invalid_status = IMS_TRUE;
			}
			
		} /* if (selectFlag) */

		laserQPtr = laserQPtr->next;

	} /* while */


	if (sCount == 0)
	{
		/* No item is selected, display message, return */
		sprintf(Msg, "No item is selected.\n");   

		/* Change cursor back to normal */
		timeOutCursors (False);
		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

		return;
	}

	if ((invalid_status) && (modifyFlag))
	{
		/*
		** Refresh laser queue lists 
		*/
		(void) filmGen_laser_searchCb(glbData.filmW, NULL, NULL);

		sprintf(Msg, "Laser Item PPS Regeneration:\n\n"
		 "PPS regeneration is allowed for items with the \n\n"
		 "following status values: NEW, GENERATED, FAILED, ERROR.\n\n"
		 "Part of the items selected in the Laser Tech Queue do not qualify "
		 "this condition.\n\n\n"
		 "Items qualify the condition are removed from the Laser Tech Queue.\n\n"
		 "Statuses of these items have been updated to VALIDATED in the "
		 "order item queue.\n\n"
		 "These items will be processed by PPS in the near future.\n");

		msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

	}
	else
		if (invalid_status)
		{
			/* Display error messages */
			sprintf(Msg, "Laser Item PPS Regeneration:\n\n"
				 "PPS regeneration is allowed for items with the \n\n"
				 "following status values: NEW, GENERATED, FAILED, ERROR.\n\n"
				 "Selections in the Laser Tech Queue do not qualify the "
				 "condition.\n");

			msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

		}
		else	
			if (modifyFlag)
			{
				/*
				** Refresh laser queue lists 
				*/
				(void) filmGen_laser_searchCb(glbData.filmW, NULL, NULL);

				sprintf(Msg, "Laser Item PPS Regeneration:\n\n"
					"Selected items are removed from the Laser Tech Queue.\n\n"
			 		"Statuses of these items have been updated to VALIDATED in the "
			 		"order item queue.\n\n"
					"These items will be processed by PPS in the near future.\n");

				msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, Msg); 

			}

	/* Change cursor back to normal */
	timeOutCursors (False);

 }
	UxFilmGenerationContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name:	free_ttdl_queueList
**
** Description:		Function to free the ttdl_queue_list
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_ttdl_queueList()
{
	OP_FILM_CLIENT_DATA *clientData;
	OP_TTDL_QUEUE_LIST *qPtr, *qNextPtr; 

	clientData = &(glbData.filmClientData);
	qPtr = clientData->ttdlQueueList;

	/* free up the clientData ttdlQueueList */
	if (qPtr != (OP_TTDL_QUEUE_LIST *)NULL)
	{
		while (qPtr != (OP_TTDL_QUEUE_LIST *)NULL)
		{
			qNextPtr = qPtr->next;
			free(qPtr);
			qPtr = qNextPtr;
		}

		clientData->ttdlQueueList = (OP_TTDL_QUEUE_LIST *)NULL;
		clientData->ttdlQueueCount = 0;
	}
}



/*===========================================================================*
** 
** Function Name:	free_film_itemList
**
** Description:		Function to free the FPS filmItemList
**
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

static void	free_film_itemList(
	FILM_REQUEST_LIST **filmItemList)

{
	OP_FILM_CLIENT_DATA *clientData;
	FILM_REQUEST_LIST *qPtr, *qNextPtr; 

	qPtr = *filmItemList;

	/* free up the clientData ttdlQueueList */
	if (qPtr != (FILM_REQUEST_LIST *)NULL)
	{
		while (qPtr != (FILM_REQUEST_LIST *)NULL)
		{
			qNextPtr = qPtr->next;
			free(qPtr);
			qPtr = qNextPtr;
		}

		*filmItemList = (FILM_REQUEST_LIST *)NULL;
	}

}


