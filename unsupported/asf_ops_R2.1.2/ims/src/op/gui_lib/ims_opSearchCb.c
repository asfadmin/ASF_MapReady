static char *sccs = "@(#)ims_opSearchCb.c	5.3  05/12/97";
/*******************************************************************************

	File:			ims_opSearchCb.c

	Function:	Callback functions for order search screen

	Author:		Jennifer Ting

	Date:			3/1995

	Revision: 6/10/96 - Modified function search_closeCb to correct PR 942.

*******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#include <UxXt.h>

#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/ToggleBG.h>
#include <Xm/ToggleB.h>

#define _IMS_OP_SEARCHCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/* schema V3.00 has process_type defined in table items
OP_CATALOG_ITEM process_type[] =
{
	{"CCSD", 1}, {"COMPLEX", 2},
	{"FULL-RES", 3}, {"GEOCODED FULL-RES(UTM)", 4},
	{"GEOCODED FULL-RES(PS)", 5}, {"LO-RES", 6},
	{"GEOCODED LO-RES(UTM)", 7}, {"GEOCODED LO-RES(PS)", 8},
	{"VECTOR", 9}, {"GRID", 10},
	{"IMAGE", 11}, {"WAVE", 12}
};
*/

/* schema V3.00 has media_type defined in table items
OP_CATALOG_ITEM media_type[] =
{
	{"9-TRACK 6250 BPI", 1}, {"9-TRACK 1600 BPI", 2},
	{"8MM 2GB CARTRIDEG", 3}, {"8MM 2GB CARTRIDGE", 4},
	{"TRANSPARENCY POS", 5}, {"TRANSPARENCY NEG", 6},
	{"B&W PAPER PRINT", 7}, {"2X PRINT", 8},
	{"5X PRINT", 9}, {"FTP", 10},
	{"DRY-SILVER PRINT", 11}
};
*/

OP_CHECKBOX_TOGGLE		order_status_toggleA[IMS_COL30_LEN];
OP_CHECKBOX_TOGGLE		item_status_toggleA[IMS_COL30_LEN];
OP_CHECKBOX_TOGGLE		process_status_toggleA[IMS_COL30_LEN];
OP_CHECKBOX_TOGGLE		process_type_toggleA[IMS_COL30_LEN];
OP_CHECKBOX_TOGGLE		media_type_toggleA[IMS_COL30_LEN];
OP_CHECKBOX_TOGGLE		item_type_toggleA[IMS_COL30_LEN];

static int order_status_toggleCount;
static int item_status_toggleCount;
static int process_status_toggleCount;
static int process_type_toggleCount;
static int media_type_toggleCount;
static int item_type_toggleCount;
static int priority_flag = 0;
static int quickLook_flag = 0;

int validated_toggle_set = 2;
int shipped_toggle_set = 2;
int debited_toggle_set = 2;
int billed_toggle_set = 2;

/* Function local to this file */
static int createQuery (Widget wgt);

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include <ims_opSearch.h>
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
**
** Function Name: search_optionmenu_toggledCb
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

void	search_optionmenu_toggledCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int optionMenu_no;
	int which;

	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		which = (int) cd;
		XtVaGetValues (XtParent(wgt), XmNuserData, &optionMenu_no, NULL);

		switch (optionMenu_no)
		{
			case 1 : /* Processing Type Option Menu */
				quickLook_flag = which;
				break;

			case 2 : /* Priority Option Menu */
				priority_flag = which;
				break;

			default:
				break;
		}

	}
	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: search_create_checkbox_togglesCb
**
** Description:		create toggles for the following checkboxes:
**								order status, item status, process status,
**								processing option and media option.
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


void	search_create_checkbox_togglesCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
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
	Dimension  indicatorSize;
	XmFontList fontList;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		/* Get resources from dummyTB, then unmanage dummyTB */
		XtVaGetValues(dummyTB,
						XmNfontList, &fontList,
						XmNindicatorSize, &indicatorSize,
						XmNselectColor, &selectColor,
						NULL);
		XtUnmanageChild (dummyTB);

		for (i = 0; i < glbData.order_status_count; i++)
		{
			order_status_toggleA[i].toggle_w =
					XtVaCreateManagedWidget(glbData.order_status[i].item_name,
							xmToggleButtonGadgetClass, orderStatusRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL);

			(void) strcpy (order_status_toggleA[i].name,
							glbData.order_status[i].item_name);
			order_status_toggleA[i].dbID = glbData.order_status[i].item_id;
		}
		order_status_toggleCount = glbData.order_status_count;


		for (i = 0; i < glbData.item_status_count; i++)
		{
			item_status_toggleA[i].toggle_w =
					XtVaCreateManagedWidget(glbData.item_status[i].item_name,
							xmToggleButtonGadgetClass, itemStatusRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL);

			(void) strcpy (item_status_toggleA[i].name,
							glbData.item_status[i].item_name);
			item_status_toggleA[i].dbID = glbData.item_status[i].item_id;
		}
		item_status_toggleCount = glbData.item_status_count;


		for (i = 0; i < glbData.pps_status_count; i++)
		{
			process_status_toggleA[i].toggle_w =
					XtVaCreateManagedWidget(glbData.pps_status[i].item_name,
							xmToggleButtonGadgetClass, processStatusRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL);

			(void) strcpy (process_status_toggleA[i].name,
								glbData.pps_status[i].item_name);
			process_status_toggleA[i].dbID = glbData.pps_status[i].item_id;
		}
		process_status_toggleCount = glbData.pps_status_count;


		for (i = 0; i < glbData.process_type_count; i++)
		{
			process_type_toggleA[i].toggle_w =
					XtVaCreateManagedWidget(glbData.process_type[i].item_name,
							xmToggleButtonGadgetClass, processOptionRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL);

			(void) strcpy (process_type_toggleA[i].name,
									glbData.process_type[i].item_name);
			process_type_toggleA[i].dbID = glbData.process_type[i].item_id;
		}
		process_type_toggleCount = glbData.process_type_count;


		for (i = 0; i < glbData.media_type_count; i++)
		{
			media_type_toggleA[i].toggle_w =
					XtVaCreateManagedWidget(glbData.media_type[i].item_name,
							xmToggleButtonGadgetClass, mediaTypeRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL);

			(void) strcpy (media_type_toggleA[i].name,
								glbData.media_type[i].item_name);
			media_type_toggleA[i].dbID = glbData.media_type[i].item_id;
		}
		media_type_toggleCount = glbData.media_type_count;

		for (i = 0; i < glbData.order_item_type_count; i++)
		{
			item_type_toggleA[i].toggle_w =
					XtVaCreateManagedWidget(glbData.order_item_type[i].item_name,
							xmToggleButtonGadgetClass, itemTypeRC,
							XmNindicatorSize, indicatorSize,
							XmNfontList, fontList,
							XmNselectColor, selectColor,
							NULL);

			(void) strcpy (item_type_toggleA[i].name,
								glbData.order_item_type[i].item_name);
			item_type_toggleA[i].dbID = glbData.order_item_type[i].item_id;
		}
		item_type_toggleCount = glbData.order_item_type_count;


		/* Get resources from prirority dummyPB, then unmanage dummyPB */
		XtVaGetValues(orderPriorityAllPB,
						XmNfontList, &fontList,
						XmNbackground, &background,
						XmNmarginLeft, &marginLeft,
						XmNmarginRight, &marginRight,
						XmNmarginWidth, &marginWidth,
						NULL);

		for (i = 0; i < glbData.priority_count; i++)
		{
			label = XmStringCreateLocalized (glbData.priority[i].item_name);
			menu_item = XtVaCreateManagedWidget
									(glbData.priority[i].item_name,
									 xmPushButtonWidgetClass, priorityOM_pane,
									 XmNlabelString, label,
									 XmNfontList, fontList,
									 XmNbackground, background,
									 XmNmarginLeft, marginLeft,
									 XmNmarginRight, marginRight,
									 XmNmarginWidth, marginWidth,
									 NULL);

		 	XtAddCallback (menu_item, XmNactivateCallback,
										 (XtCallbackProc) search_optionmenu_toggledCb,
										 (XtPointer) glbData.priority[i].item_id);

			XmStringFree (label);
		}

	}
	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: search_radiobox_toggledCb
**
** Description:		This callback function stores the most recently
**								selected toggle in a global variable matching
**								the radiobox.
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

void	search_radiobox_toggledCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int radiobox_no;
	int which;

	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		which = (int) cd;
		XtVaGetValues (XtParent(wgt), XmNuserData, &radiobox_no, NULL);

		switch (radiobox_no)
		{
			case 1 : /* validated radiobox */
				validated_toggle_set = which;
				break;

			case 2 : /* shipped radiobox */
				shipped_toggle_set = which;
				break;

			case 3 : /* debited radiobox */
				debited_toggle_set = which;
				break;

			case 4 : /* billed radiobox */
				billed_toggle_set = which;
				break;

			default:
				break;
		}

	}
	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: createQuery
**
** Description:		Create Search Query
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

static int
createQuery( Widget wgt )

{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_CLIENT_DATA *clientData;
	OP_QUERY_STRUCT *sql;
	int i, count, set_count;
	int order_itemFlag, status;
	char *value;
	char *sqlPtr;
	char *first_name, *middle_name, *last_name;
	char *startDate, *endDate;
	char sDate[9], eDate[9];
	char sDateValue[IMS_COL15_LEN+1];
	char eDateValue[IMS_COL15_LEN+1];
	char Msg[IMS_COL1024_LEN+1];


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		clientData = &(glbData.orderClientData);
		sql = &(clientData->queryStruct);

		/*
		** Initialize op_query_struct
		*/
		sql->select[0] = sql->from[0] = sql->where[0] = sql->sqlBuf[0] = '\0';
		sql->sPtr = sql->select;
		sql->fPtr = sql->from;
		sql->wPtr = sql->where;
		sqlPtr = sql->sqlBuf;
		order_itemFlag = 0;

		(void) strcpy (sql->sPtr,
				"distinct t1.order_id, "
				"t1.user_id, "
				"t1.account_id, "
				"convert (char(20), t1.received_time, 100), "
				"convert (char(20), t1.completed_time, 100), "
				"t1.priority, "
				"t1.item_count, "
				"t1.status, "
				"t1.op_comment ");

		sql->sPtr = sql->sPtr + strlen(sql->sPtr);

		(void) strcpy (sql->fPtr, "order_queue t1 ");
		sql->fPtr = sql->fPtr + strlen(sql->fPtr);


		/*
		** Get text from orderId textField
		*/
		value = XmTextFieldGetString (srchOrderIdTF);
		ims_truncStr (value);

		if (value && *value)
		{
			if ((status = ims_isInteger(value)) != IMS_OK)
			{
				/* Display error messages */
				(void) strcpy (Msg, "Order ID is invalid.");
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg);
				return(IMS_FATAL);
			}

			(void) sprintf (sql->wPtr, "t1.order_id = %s", value);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (value);
		}


		/*
		** Get text from accountId textField
		*/
		value = XmTextFieldGetString (accountIdTF);
		ims_truncStr (value);

		if (value && *value)
		{
			order_itemFlag = 1;

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr, "t1.account_id = '%s'", value);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (value);
		}


		/*
		** Get text from userId textField
		*/
		value = XmTextFieldGetString (userIdTF);
		ims_truncStr (value);

		if (value && *value)
		{

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			(void) sprintf (sql->wPtr, "t1.user_id = '%s'", value);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			XtFree (value);
		}



		/*
		** Get order status toggles selected
		*/
		for (i = count = 0; i < order_status_toggleCount; i++)
			if (XmToggleButtonGetState(order_status_toggleA[i].toggle_w))
				count++;

		if (count > 0)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			if (count == 1)
			{
				i = 0;
				while ((!XmToggleButtonGetState(order_status_toggleA[i].toggle_w))
							 	&& i < order_status_toggleCount)
						i++;

				(void) sprintf (sql->wPtr, "t1.status = %d ",
								 order_status_toggleA[i].dbID);
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			else
			{
				(void) strcpy (sql->wPtr, "t1.status in (");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

				set_count = 0;
				for (i = 0; i < order_status_toggleCount; i++)
					if (XmToggleButtonGetState(order_status_toggleA[i].toggle_w))
					{
						(void) sprintf (sql->wPtr,(set_count? ",%d" : "%d"),
											order_status_toggleA[i].dbID);
						sql->wPtr = sql->wPtr + strlen(sql->wPtr);
						set_count++;
					}

				(void) strcpy (sql->wPtr, ")");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
		}



		/*
		** Get item status toggles selected
		*/
		for (i = count = 0; i < item_status_toggleCount; i++)
			if (XmToggleButtonGetState(item_status_toggleA[i].toggle_w))
				count++;


		if (count > 0)
		{
			order_itemFlag = 1;

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			if (count == 1)
			{
				i = 0;
				while ((!XmToggleButtonGetState(item_status_toggleA[i].toggle_w))
							 	&& i < item_status_toggleCount)
						i++;

				(void) sprintf (sql->wPtr, "t2.status = %d ",
								 item_status_toggleA[i].dbID);
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			else
			{
				(void) strcpy (sql->wPtr, "t2.status in (");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

				set_count = 0;
				for (i = 0; i < item_status_toggleCount; i++)
					if (XmToggleButtonGetState(item_status_toggleA[i].toggle_w))
					{
						(void) sprintf (sql->wPtr,(set_count? ",%d" : "%d"),
										 item_status_toggleA[i].dbID);
						sql->wPtr = sql->wPtr + strlen(sql->wPtr);
						set_count++;
					}

				(void) strcpy (sql->wPtr, ")");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
		}


		/*
		** Get process status toggles selected
		*/
		for (i = count = 0; i < process_status_toggleCount; i++)
			if (XmToggleButtonGetState(process_status_toggleA[i].toggle_w))
				count++;


		if (count > 0)
		{
			order_itemFlag = 1;

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void)strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			if (count == 1)
			{
				i = 0;
				while ((!XmToggleButtonGetState(process_status_toggleA[i].toggle_w))
							 	&& i < process_status_toggleCount)
						i++;

				(void) sprintf (sql->wPtr, "t2.process_status = %d ",
								 process_status_toggleA[i].dbID);
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			else
			{
				(void) strcpy (sql->wPtr, "t2.process_status in (");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

				set_count = 0;
				for (i = 0; i < process_status_toggleCount; i++)
					if (XmToggleButtonGetState(process_status_toggleA[i].toggle_w))
					{
						(void) sprintf (sql->wPtr,(set_count? ",%d" : "%d"),
									   process_status_toggleA[i].dbID);
						sql->wPtr = sql->wPtr + strlen(sql->wPtr);
						set_count++;
					}

				(void) strcpy (sql->wPtr, ")");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
		}


		/*
		** Get process option toggles selected
		*/
		for (i = count = 0; i < process_type_toggleCount; i++)
			if (XmToggleButtonGetState(process_type_toggleA[i].toggle_w))
				count++;


		if (count > 0)
		{
			order_itemFlag = 1;

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			if (count == 1)
			{
				i = 0;
				while ((!XmToggleButtonGetState(process_type_toggleA[i].toggle_w))
							 	&& i < process_type_toggleCount)
						i++;

				(void) sprintf (sql->wPtr, "t2.process_type = %d ",
								 process_type_toggleA[i].dbID);
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

			}
			else
			{
				(void) strcpy (sql->wPtr, "t2.process_type in (");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

				set_count = 0;
				for (i = 0; i < process_type_toggleCount; i++)
					if (XmToggleButtonGetState(process_type_toggleA[i].toggle_w))
					{
						(void) sprintf (sql->wPtr, (set_count? ",%d" : "%d"),
										 process_type_toggleA[i].dbID);
						sql->wPtr = sql->wPtr + strlen(sql->wPtr);
						set_count++;
					}

				(void) strcpy (sql->wPtr, ")");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
		}



		/*
		** Get media type toggles selected
		*/
		for (i = count = 0; i < media_type_toggleCount; i++)
			if (XmToggleButtonGetState(media_type_toggleA[i].toggle_w))
				count++;


		if (count > 0)
		{
			order_itemFlag = 1;

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void)strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			if (count == 1)
			{
				i = 0;
				while ((!XmToggleButtonGetState(media_type_toggleA[i].toggle_w))
							 	&& i < media_type_toggleCount)
						i++;

				(void) sprintf (sql->wPtr, "t2.media_type = %d ",
								 media_type_toggleA[i].dbID);
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			else
			{
				(void) strcpy (sql->wPtr, "t2.media_type in (");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

				set_count = 0;
				for (i = 0; i < media_type_toggleCount; i++)
					if (XmToggleButtonGetState(media_type_toggleA[i].toggle_w))
					{
						(void) sprintf (sql->wPtr, (set_count? ",%d" : "%d"),
										 media_type_toggleA[i].dbID);
						sql->wPtr = sql->wPtr + strlen(sql->wPtr);
						set_count++;
					}

				(void) strcpy (sql->wPtr, ")");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
		}

		/*
		** Get item type toggles selected
		*/
		for (i = count = 0; i < item_type_toggleCount; i++)
			if (XmToggleButtonGetState(item_type_toggleA[i].toggle_w))
				count++;


		if (count > 0)
		{
			order_itemFlag = 1;

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			if (count == 1)
			{
				i = 0;
				while ((!XmToggleButtonGetState(item_type_toggleA[i].toggle_w))
							 	&& i < item_type_toggleCount)
						i++;

				(void) sprintf (sql->wPtr, "t2.order_item_type = %d ",
								 item_type_toggleA[i].dbID);
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			else
			{
				(void) strcpy (sql->wPtr, "t2.order_item_type in (");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);

				set_count = 0;
				for (i = 0; i < item_type_toggleCount; i++)
					if (XmToggleButtonGetState(item_type_toggleA[i].toggle_w))
					{
						(void) sprintf (sql->wPtr,(set_count? ",%d" : "%d"),
										 item_type_toggleA[i].dbID);
						sql->wPtr = sql->wPtr + strlen(sql->wPtr);
						set_count++;
					}

				(void) strcpy (sql->wPtr, ")");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
		}


		/*
		** Get User last name and first name
		*/
		first_name = XmTextFieldGetString (srchFirstNameTF);
		ims_truncStr (first_name);
		last_name  = XmTextFieldGetString (srchLastNameTF);
		ims_truncStr (last_name);
		middle_name  = XmTextFieldGetString (middleNameTF);
		ims_truncStr (middle_name);

		if (*first_name)
		{

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			(void) sprintf (sql->wPtr, " user_id in (select user_id from"
													" user_profile where first_name = '%s')",
													ims_truncStr(first_name));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);

			XtFree (first_name);

		}

		if (*middle_name)
		{

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			(void) sprintf (sql->wPtr, " user_id in (select user_id from"
													" user_profile where initial_name = '%s')",
													ims_truncStr(middle_name));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);

			XtFree (middle_name);

		}

		if (*last_name)
		{

			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}

			(void) sprintf (sql->wPtr, " user_id in (select user_id from"
													" user_profile where last_name = '%s')",
													ims_truncStr(last_name));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);

			XtFree (last_name);

		}


		/*
		** Get order received dates
		** Need to call date validation
		*/
		startDate = XmTextGetString (ordrRecStartDateText);
		ims_truncStr (startDate);
		endDate = XmTextGetString (ordrRecEndDateText);
		ims_truncStr (endDate);

		if ((*startDate) && (*endDate))
		{
			(void) strcpy (sDateValue, startDate);
			(void) strcpy (eDateValue, endDate);

			sDate[0] = sDateValue[0];
			sDate[1] = sDateValue[1];
			sDate[2] = sDateValue[2];
			sDate[3] = sDateValue[3];
			sDate[4] = sDateValue[5];
			sDate[5] = sDateValue[6];
			sDate[6] = sDateValue[8];
			sDate[7] = sDateValue[9];
			sDate[8] = '\0';

			eDate[0] = eDateValue[0];
			eDate[1] = eDateValue[1];
			eDate[2] = eDateValue[2];
			eDate[3] = eDateValue[3];
			eDate[4] = eDateValue[5];
			eDate[5] = eDateValue[6];
			eDate[6] = eDateValue[8];
			eDate[7] = eDateValue[9];
			eDate[8] = '\0';

			if (atoi(sDate) > atoi(eDate))
			{
				/* Display error messages */
				(void) sprintf(Msg, "Order Received:\nStart Date exceeds End Date!");
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg);

				XtFree(startDate);
				XtFree(endDate);
				return (IMS_FATAL);
			}
		}

		if (*startDate)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr, "t1.received_time >= '%s'", startDate);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}

		if (*endDate)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr, "t1.received_time <= '%s'", endDate);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}

		XtFree (startDate);
		XtFree (endDate);


		/*
		** Get order completed dates
		** Need to call date validation
		*/

		startDate = XmTextGetString (ordrComStartDateText);
		ims_truncStr(startDate);
		endDate = XmTextGetString (ordrComEndDateText);
		ims_truncStr(endDate);

		if ((*startDate) && (*endDate))
		{
			(void) strcpy (sDateValue, startDate);
			(void) strcpy (eDateValue, endDate);
			ims_truncStr (sDateValue);
			ims_truncStr (eDateValue);

			sDate[0] = sDateValue[0];
			sDate[1] = sDateValue[1];
			sDate[2] = sDateValue[2];
			sDate[3] = sDateValue[3];
			sDate[4] = sDateValue[5];
			sDate[5] = sDateValue[6];
			sDate[6] = sDateValue[8];
			sDate[7] = sDateValue[9];
			sDate[8] = '\0';

			eDate[0] = eDateValue[0];
			eDate[1] = eDateValue[1];
			eDate[2] = eDateValue[2];
			eDate[3] = eDateValue[3];
			eDate[4] = eDateValue[5];
			eDate[5] = eDateValue[6];
			eDate[6] = eDateValue[8];
			eDate[7] = eDateValue[9];
			eDate[8] = '\0';

			if (atoi(sDate) > atoi(eDate))
			{
				/* Display error messages */
				(void) sprintf(Msg, "Order Completed:\nStart Date exceeds End Date!");
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg);

				XtFree (startDate);
				XtFree (endDate);
				return(IMS_FATAL);
			}
		}

		if (*startDate)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr, "t1.completed_time >= '%s'", startDate);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}

		if (*endDate)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr, "t1.completed_time <= '%s'", endDate);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}

		XtFree (startDate);
		XtFree (endDate);


		/*
		** Get order item flags selected
		*/
		if (validated_toggle_set != 2)
		{
			order_itemFlag = 1;
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr,
							 (validated_toggle_set?
								"t2.validated_p = 'N'" : "t2.validated_p = 'Y'"));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}


		if (shipped_toggle_set != 2)
		{
			order_itemFlag = 1;
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr,
							 (shipped_toggle_set?
								"t2.shipped_p = 'N'" : "t2.shipped_p = 'Y'"));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}


		if (debited_toggle_set != 2)
		{
			order_itemFlag = 1;
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr,
							 (debited_toggle_set?
								"t2.cost_debited_p = 'N'" : "t2.cost_debited_p = 'Y'"));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}


		if (billed_toggle_set != 2)
		{
			order_itemFlag = 1;
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr,
							 (billed_toggle_set?
								"t2.billed_p = 'N'" : "t2.billed_p = 'Y'"));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}


		/*
		** Quick-Look
		*/
		if (quickLook_flag)
		{
			order_itemFlag = 1;
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr,
							 (quickLook_flag >1 ?
							 "t2.quicklook_p = 'Y'" : "t2.quicklook_p = 'N'"));

			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}


		/*
		** Order Priority
		*/
		if (priority_flag)
		{
			if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			{
				(void) strcpy (sql->wPtr, " and\n");
				sql->wPtr = sql->wPtr + strlen(sql->wPtr);
			}
			(void) sprintf (sql->wPtr, "t1.priority = %d", priority_flag);
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}



		if (order_itemFlag)
		{
			/* add table order_item to select statement */
			(void) strcpy (sql->fPtr, ", order_item t2");
			sql->fPtr = sql->fPtr + strlen(sql->fPtr);

			/* join table order_queue and order_item */
			(void) strcpy (sql->wPtr, " and\nt1.order_id = t2.order_id");
			sql->wPtr = sql->wPtr + strlen(sql->wPtr);
		}



		/*
		** complete the sql statement
		*/

		if ((sql->where != (char *) NULL) && (sql->where[0] != '\0'))
			(void) sprintf (sqlPtr,
										"select %s\nfrom %s\nwhere %s\norder by t1.order_id",
										sql->select, sql->from, sql->where);
		else
			(void) sprintf (sqlPtr, "select %s\nfrom %s\norder by t1.order_id",
										sql->select, sql->from);

		sqlPtr = sqlPtr + strlen(sqlPtr);

	}

	UxSearchContext = UxSaveCtx;
	return (IMS_OK);
}


/*===========================================================================*
**
** Function Name: search_executeQuery
**
** Description:		execute Search Query
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

int search_executeQuery(
	Widget wgt)

{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;

	OP_CLIENT_DATA *clientData;
	OP_CAT_STRUCT *catReq;
	OP_ORDER_LIST *orderPtr;
	int status;
	int orderCount;
	char Msg[IMS_COL1024_LEN+1];


	/*
	** Initialize catalog request structure
	*/
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);
	catReq->item[0] = (int *)&orderCount;
	catReq->item[1] = (char *)clientData->queryStruct.sqlBuf;

	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		/* free up the current clientData->orderList first */
		(void) free_orderList();

		if ((status = ims_opCat (catReq, OP_GETORDERLIST)) < IMS_OK)
		{
			/* Display error messages */
			(void) sprintf(Msg, "Internal Error: order retrieval failed.");
			msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg);
			return (IMS_FATAL);
		}
		else
		{
			/* assign returned orders to glbData.orderClientData->orderList */
			clientData->orderCount = *(int *)catReq->item[0];
			clientData->orderList = (OP_ORDER_LIST *)catReq->item[2];

			if ((clientData->orderList != (OP_ORDER_LIST *)NULL) ||
				(clientData->orderCount > 0))
			{
				orderPtr = clientData->orderList;

				/* Get item status stats && initialize selectFlag */
				while (orderPtr != (OP_ORDER_LIST *)NULL)
				{
					catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
					if ((status = ims_opCat (catReq, OP_GETORDERITEMSTATS)) < IMS_OK)
					{
						/* Display error messages */
						(void) sprintf(Msg, "Internal Error: order retrieval failed.");
						msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg);
						return (IMS_FATAL);
					}

					orderPtr->selectFlag = 0;
					orderPtr = orderPtr->next;

				}
			}
		}
	}

	UxSearchContext = UxSaveCtx;
	return (IMS_OK);
}


/*===========================================================================*
**
** Function Name: search_executeCb
**
** Description:		Execute Search Function
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


void	search_executeCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CLIENT_DATA *clientData;
	int status;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		/* Change cursor to watch cursor */
		timeOutCursors (True);

		clientData = &(glbData.orderClientData);

		if ((status = createQuery (glbData.searchW)) < IMS_OK)
		{
			/* Change cursor back to normal*/
			timeOutCursors (False);

			return;
		}
		else
			if ((status = search_executeQuery (glbData.searchW)) < IMS_OK)
			{
				/* Change cursor back to normal*/
				timeOutCursors (False);

				return;
			}
			else
			{
				/* Change cursor back to normal*/
				timeOutCursors (False);

				/* popup order display screen */
				order_displayResults (glbData.orderW);

			}

	}
	UxSearchContext = UxSaveCtx;
	return;
}


/*===========================================================================*
**
** Function Name: search_check_date
**
** Description:		Check Date Function
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

void	search_check_date(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;

	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{
		cbs = (XmTextVerifyCallbackStruct *)cb;
		len = XmTextGetLastPosition(wgt);

		if (cbs->reason == XmCR_MOVING_INSERT_CURSOR)
		{
			if (cbs->newInsert != len)
				cbs->doit = False;
			return;
		}

		/* no backspacing or typing in the middle of the string */
		if (cbs->currInsert < len)
		{
			cbs->doit = False;
			return;
		}

		if (cbs->text->length == 0) /* backspace */
		{
			if ((cbs->startPos == 4) || (cbs->startPos == 7))
				cbs->startPos--;
			return;
		}

		if (cbs->text->length >1) /* don't allow clipboard copies */
		{
			cbs->doit = False;
			return;
		}

		/* don't allow non-digits or let the input exceed 10 chars */
		if (!isdigit(c = cbs->text->ptr[0]) || len >= 10)
			cbs->doit = False;
		else
			if (len == 3 || len == 6)
			{
				cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
				cbs->text->length = 2;
				cbs->text->ptr[0] = c;
				cbs->text->ptr[1] = '-';
			}

	}
	UxSearchContext = UxSaveCtx;
	return;
}


/*===========================================================================*
**
** Function Name: search_date_loseFocusCb
**
** Description:		Date validation function, this is called whenever
**								the focus is moved away from the date text field.
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

void	search_date_loseFocusCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	int  status;
	char *fieldValue;
	char Msg[IMS_COL1024_LEN+1];


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{

		fieldValue = XmTextGetString (UxWidget);

		if ((status = isDateFieldValid (fieldValue)) < IMS_OK)
		{
			/* Clear the date field */
			XmTextSetString (UxWidget, "");

			/* Display error messages */
			(void) sprintf(Msg, "Invalid Date!");
			msgBoxDlg_popupCb (glbData.searchW, IMS_ERROR, Msg);

			XtFree(fieldValue);
			return;
		}

	}
	UxSearchContext = UxSaveCtx;
	return;

}


/*===========================================================================*
**
** Function Name: search_clearCb
**
** Description:		Clear the search screen
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


void	search_clearCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	int i;
	WidgetList toggle_list;

	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( glbData.searchW );
	{

		/* Clear all the text field and text widgets */
		XmTextFieldSetString (srchOrderIdTF, "");
		XmTextFieldSetString (accountIdTF, "");
		XmTextFieldSetString (userIdTF, "");
		XmTextFieldSetString (srchLastNameTF, "");
		XmTextFieldSetString (srchFirstNameTF, "");
		XmTextFieldSetString (middleNameTF, "");
		XmTextSetString (ordrRecStartDateText, "");
		XmTextSetString (ordrRecEndDateText, "");
		XmTextSetString (ordrComStartDateText, "");
		XmTextSetString (ordrComEndDateText, "");

		/* Unset the toggle that is selected for each radiobox */
		XtVaGetValues (srchValidatedRC, XmNchildren, &toggle_list, NULL);
		XmToggleButtonSetState(toggle_list[validated_toggle_set], False, False);
		XtVaGetValues (srchShippedRC, XmNchildren, &toggle_list, NULL);
		XmToggleButtonSetState(toggle_list[shipped_toggle_set], False, False);
		XtVaGetValues (srchDebitedRC, XmNchildren, &toggle_list, NULL);
		XmToggleButtonSetState(toggle_list[debited_toggle_set], False, False);
		XtVaGetValues (srchBilledRC, XmNchildren, &toggle_list, NULL);
		XmToggleButtonSetState(toggle_list[billed_toggle_set], False, False);

		/* Reset toggle states to Both for order items */
		XmToggleButtonSetState(srchValidatedBothTB, True, False);
		validated_toggle_set = 2;
		XmToggleButtonSetState(srchShippedBothTB, True, False);
		shipped_toggle_set = 2;
		XmToggleButtonSetState(srchDebitedBothTB, True, False);
		debited_toggle_set = 2;
		XmToggleButtonSetState(srchBilledBothTB, True, False);
		billed_toggle_set  = 2;

		/* Deselect all toggles in the checkboxes */
		for (i = 0; i < order_status_toggleCount; i++)
		{
			XmToggleButtonSetState(order_status_toggleA[i].toggle_w,
														 False, False);
		}

		for (i = 0; i < item_status_toggleCount; i++)
		{
			XmToggleButtonSetState(item_status_toggleA[i].toggle_w,
														 False, False);
		}

		for (i = 0; i < process_status_toggleCount; i++)
		{
			XmToggleButtonSetState(process_status_toggleA[i].toggle_w,
														 False, False);
		}

		for (i = 0; i < process_type_toggleCount; i++)
		{
			XmToggleButtonSetState(process_type_toggleA[i].toggle_w,
														 False, False);
		}

		for (i = 0; i < media_type_toggleCount; i++)
		{
			XmToggleButtonSetState(media_type_toggleA[i].toggle_w,
														 False, False);
		}

		for (i = 0; i < item_type_toggleCount; i++)
		{
			XmToggleButtonSetState(item_type_toggleA[i].toggle_w,
														 False, False);
		}

		/* Reset processingTypeOM and orderPriorityOM */
		XtVaSetValues (processingTypeOM, XmNmenuHistory, processTypeAllPB, NULL);
		XtVaSetValues (orderPriorityOM, XmNmenuHistory, orderPriorityAllPB, NULL);
		priority_flag = 0;
		quickLook_flag = 0;

	}
	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: search_closeCb
**
** Description:		Exit from the Search Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
**
** Return Value:	None
**
** Revision History: 06/10/96 - took out search context assignment
**
**==========================================================================*/


void	search_closeCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	/*
	** data structure cleanup and database
	** close connection must be done here.
	*/
	search_clearCb(glbData.searchW, cd, cb);

	XtPopdown (XtParent(glbData.searchW));
	glbData.searchFlag = 0;
}


/*===========================================================================*
**
** Function Name: search_accountId_validsCb
**
** Description:		displays the account id selection dialog for the user
**								to search on valid account ids.
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

void	search_accountId_validsCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{
		/* call srchSelDlg_popupCb to manage the selection dialog */
		srchSelDlg_popupCb (glbData.searchW, 1, NULL);
	}
	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: search_userId_validsCb
**
** Description:		displays the user id selection dialog for the user
**								to search on valid user ids.
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

void	search_userId_validsCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{
		/* call srchSelDlg_popupCb to manage the selection dialog */
		srchSelDlg_popupCb (glbData.searchW, 2, NULL);
	}
	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: search_goto_orderCb
**
** Description:		Pop up the order screen from order screen
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

void	search_goto_orderCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{
		XtPopup(XtParent(glbData.orderW), XtGrabNone);
		glbData.orderFlag = 1;
	}

	UxSearchContext = UxSaveCtx;
}


/*===========================================================================*
**
** Function Name: search_goto_welcomeCb
**
** Description:		Pop up the welcome screen from search screen
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

void	search_goto_welcomeCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{
		XtPopup(XtParent(glbData.welcomeW), XtGrabNone);
		glbData.welcomeFlag = 1;
	}

	UxSearchContext = UxSaveCtx;

}


/*===========================================================================*
**
** Function Name: search_printScreenCb
**
** Description:		print the current search screen
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

void	search_printScreenCb(
	Widget wgt,
	XtPointer cd,
	XtPointer cb)
{
	_UxCsearch              *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	UxSaveCtx = UxSearchContext;
	UxSearchContext = UxContext =
			(_UxCsearch *) UxGetContext( UxWidget );
	{
		/* Change cursor to watch cursor */
		timeOutCursors (True);

		printScreen(glbData.searchW);

		/* Change cursor back to normal */
		timeOutCursors (False);
	}

	UxSearchContext = UxSaveCtx;

}
