static char *sccs = "@(#)ims_opShippingCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opShippingCb.c

	Function:	Callback functions for Shipping Screen

	Author:		Jennifer Ting

	Date:			11/1995

	Revision: 6/10/1996 - Modified function shipping_closeCb to correct PR 942.

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

#define _IMS_OP_SHIPPINGCB_C
#include "ims_opCb.h"

/* 
** Local Functions 
*/


/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opShipping.h>
#undef CONTEXT_MACRO_ACCESS

#include <ims_opShipView.h>

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: shipping_printFormCb
**
** Description:		Create callback to print the new shipping form
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

void shipping_printFormCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
  _UxCshipping            *UxShippingContext;
	_UxCshipView						*UxShipViewContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *text;
	char cmdbuf[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	char tempFileName[IMS_COL255_LEN+1];
	FILE *fp;
	int screenID, i, itemCount;
	XmStringTable str_list;
	OP_CLIENT_DATA *clientData;
	DBINT order_id;
	XmScrollBarCallbackStruct *scroll_cbs;


	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);

	screenID = (int)cd;
	if (screenID)
	{
		UxShippingContext = 
					(_UxCshipping *) UxGetContext( glbData.shippingW );
	}
	else
	{
		UxShipViewContext = 
					(_UxCshipView *) UxGetContext( glbData.shipViewW );
	} 

	/* timeout cursor */
	timeOutCursors (True);

	/* put temporary print file in tmp directory */
	ims_concatFilePath (tempFileName, DEF_PRINT_DIRECTORY, 
			"ims_opShip.tmp");

	if ((fp = fopen(tempFileName, "a")) == (FILE *)NULL)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Print Shipping Form: cannot create printing temp file\n.");   
			msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 

		return; 
	}

	fprintf (fp, "\n\t\t\t        ALASKA  SAR  FACILITY\n");
	fprintf (fp, "\n\t\t\t           SHIPPING  REPORT\n");
	fprintf (fp, "=======================================================================================\n");

	if (screenID)
	{
		text = XmTextGetString (UxShippingContext->UxshipToST);
	}
	else
	{
		text = XmTextGetString (UxShipViewContext->UxshipToST);
	}

	fprintf (fp, "SHIP  TO:\n");
	fprintf (fp, "=========\n\n");
	fprintf (fp, "%s\n", text);
	fprintf (fp, "=======================================================================================\n");

	fprintf (fp, "INFORMATION:\n");
	fprintf (fp, "============\n\n");

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxshippingIdTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxshippingIdTF);
	}
	fprintf (fp, "SHIPPING ID:        %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxorderIdTF);
		order_id = atoi (text);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxorderIdTF);
	}
	fprintf (fp, "ORDER ID:           %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxaccountIdTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxaccountIdTF);
	}
	fprintf (fp, "ACCOUNT ID:         %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxorderDateTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxorderDateTF);
	}
	fprintf (fp, "ORDER DATE:         %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxshipDateTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxshipDateTF);
	}
	fprintf (fp, "SHIP DATE:          %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxcarrierTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxcarrierTF);
	}
	fprintf (fp, "CARRIER:            %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxtotalQtyTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxtotalQtyTF);
	}
	fprintf (fp, "TOTAL QUANTITY:     %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxShippingContext->UxtotalCostTF);
	}
	else
	{
		text = XmTextFieldGetString (UxShipViewContext->UxtotalCostTF);
	}
	fprintf (fp, "TOTAL COST:         %s\n\n", text);

	fprintf (fp, "=======================================================================================\n");

	fprintf (fp, "SHIP  ITEMS:\n");
	fprintf (fp, "============\n\n");
	fprintf
		(fp, "ITEM   DESCRIPTION       NAME              MEDIA        STATUS    QTY  COST\n");
	fprintf
		(fp, "=======================================================================================\n\n");

	/* get list items */
	if (screenID)
	{
		XtVaGetValues (UxShippingContext->UxshipItemSL, XmNitemCount, &itemCount,
									 XmNitems, &str_list, NULL);
	}
	else
	{
		XtVaGetValues (UxShipViewContext->UxshipItemSL, XmNitemCount, &itemCount,
									 XmNitems, &str_list, NULL);
	}

	for (i = 0; i < itemCount; i++)
	{
		/* convert item to C string */
		XmStringGetLtoR(str_list[i], XmFONTLIST_DEFAULT_TAG, &text);
		fprintf (fp, "%s\n", text);
		free (text);
	}

	fflush (fp);
	sprintf (cmdbuf, "lpr %s", tempFileName);
	system (cmdbuf);

	fclose (fp);
	remove (tempFileName);
	
	/*
	** popdown Shipping Screen 
	** refresh the item list widgets to display shipping_ids 
	** and update shipped_p flags.
	*/
	if (screenID)
	{
		/* 
		** call closedCb to clean up and reset the flag.
		*/
		(void) shipping_closeCb(wgt, cd, cb);

		if (clientData->currOrder != NULL) 
		{
			if (clientData->currOrder->itemList != (OP_ORDER_ITEM_LIST *)NULL)
			{
				if (order_id == clientData->currOrder->order_id)
				{
					/*
					** if items are currently being displayed, refresh item lists 
					*/
					scroll_cbs = (XmScrollBarCallbackStruct *)
								malloc(sizeof(XmScrollBarCallbackStruct));
					scroll_cbs->value = clientData->itemWindowTop;
					order_scroll_itemListsCb (glbData.orderW, NULL, scroll_cbs);  
					free (scroll_cbs);
				}
			}
		}
	}

	/* Change cursor back to normal */
	timeOutCursors (False);

}


/*===========================================================================*
** 
** Function Name: shipping_editCommentsCb
**
** Description:		Callback function for the edit comments button in shipping
**								Screen.
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

void shipping_editCommentsCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
  _UxCshipping            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *value;
	DBINT shipping_id;

	UxSaveCtx = UxShippingContext;
	UxShippingContext = UxContext =
			(_UxCshipping *) UxGetContext( UxWidget );
	{
		/* get shipping_id from shippingIdTF */
		value = XmTextFieldGetString (shippingIdTF);
		shipping_id = atoi (value);

		commentDlg_popupCb 
				(glbData.shippingW, (void *)&shipping_id, SHIPPING_COMMENT);

	}
 	UxShippingContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: shipping_cancelFormCb
**
** Description:		Callback function for the cancel form button in shipping
**								Screen.
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

void shipping_cancelFormCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
  _UxCshipping            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *value;
	int status;
	DBINT shipping_id;
	DBINT order_id;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr;
	char Msg[IMS_COL1024_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;

	UxSaveCtx = UxShippingContext;
	UxShippingContext = UxContext =
			(_UxCshipping *) UxGetContext( glbData.shippingW );
	{
		/* timeout cursor */
		timeOutCursors (True);

		/* assign client to orderClientData from glbData structure */
		clientData = &(glbData.orderClientData);
		catReq = &(clientData->catReq);

		/* get shipping_id from shippingIdTF */
		value = XmTextFieldGetString (shippingIdTF);
		shipping_id = atoi (value);

		value = XmTextFieldGetString (orderIdTF);
		order_id = atoi (value);

		/*
		** delete shipping entry from shipping, shipping_of tables 
		** update order_item shipping_id, shipped_p for items with
		** the specified shipping_id 
		*/

		/*
		** Begin Transaction
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Cancel Shipment\n"
									 "OP_BEGINTRANSACTION failed.\n"); 
			msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 
		
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
			strcpy(Msg, "Internal Error: Cancel Shipment\n"
								 "OP_GETGENERALLOCK failed.\n"); 
			msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 
		
			return;
		}

		/*
		** CAT Event OP_CANCELSHIPMENT
		*/
		catReq->item[0] = (DBINT *)&shipping_id;
		if ((status = ims_opCat(catReq, OP_CANCELSHIPMENT)) < 0)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy(Msg, "Internal Error: Cancel Shipment\n"
								 "OP_CANCELSHIPMENT failed.\n"); 
			msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 
		
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
			sprintf(Msg, "Internal Error: Cancel Shipment,\n"
									 "OP_COMMITTRANSACTION failed for Shipping ID: %d.\n",
									 shipping_id);   
			msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 
	
			return;
		}
	

		/* popdown shipping screen , refresh item list */
		(void) shipping_closeCb(wgt, cd, cb);

		if (clientData->currOrder != NULL) 
		{
			if (clientData->currOrder->itemList != (OP_ORDER_ITEM_LIST *)NULL)
			{
				if (order_id == clientData->currOrder->order_id)
				{
					itemPtr = clientData->currOrder->itemList;
					while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL) 
					{
						if (itemPtr->shipping_id == shipping_id)
						{	
							itemPtr->shipped_p = 'N';
							itemPtr->shipping_id = -1;
						}

						itemPtr = itemPtr->next;
					}

					/*
					** if items are currently being displayed, refresh item lists 
					*/
					scroll_cbs = (XmScrollBarCallbackStruct *)
								malloc(sizeof(XmScrollBarCallbackStruct));
					scroll_cbs->value = clientData->itemWindowTop;
					order_scroll_itemListsCb (glbData.orderW, NULL, scroll_cbs);  
					free (scroll_cbs);

				}
			}
		}

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
 	UxShippingContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: shipping_closeCb
**
** Description:		Callback function for the close button in shipping
**								Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - took out Shipping context assignment
**
**==========================================================================*/

void shipping_closeCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	XtPopdown (XtParent(glbData.shippingW));
	glbData.shippingFlag = 0;
}


/*===========================================================================*
** 
** Function Name: display_shippingData
**
** Description:		Callback function to populate and popup the Shipping Screen
**
** Arguments:			1. shippingData - shipping data spec
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

int display_shippingData(
		OP_SHIPPING_DATA *shippingData,
		int screenID)

{
  _UxCshipping            *UxShippingContext;
	_UxCshipView						*UxShipViewContext;

	char buffer[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	char *shipProfileBuf, *text;
	char process_type[IMS_COL30_LEN+1];
	char media_type[IMS_COL30_LEN+1];
	char ship_item_id[IMS_COL10_LEN+1];
	char order_item_status[IMS_COL10_LEN+1];
	int k, position;
	OP_SHIP_ITEM_LIST *sPtr;
	XmString listStr;


	if (screenID)
	{
		UxShippingContext = 
					(_UxCshipping *) UxGetContext( glbData.shippingW );
	}
	else
	{
		UxShipViewContext = 
					(_UxCshipView *) UxGetContext( glbData.shipViewW );
	} 


	/*
	** Display Shipping Profile Information in ShipTo scrolled text
	*/
	if ((shipProfileBuf = XtMalloc ((unsigned int) 1024)) == (char *)NULL)
	{
		/* display error message, return */
		strcpy(Msg, "display_shippingData:\n"
								"Internal Error: memory allocation failed.\n"
								"Please contact the DBA and exit the program.\n");
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}

	/* Write shipping profile information */
	text = shipProfileBuf;

	if (shippingData->first_name[0] != '\0') 
	{
		sprintf (text, "USER NAME:          %s  ", shippingData->first_name);
		text = text + strlen(text);
	}

	if (shippingData->initial_name[0] != '\0')
	{
		sprintf (text, "%s  ", shippingData->initial_name);
		text = text + strlen(text);
	}

	if (shippingData->last_name[0] != '\0')
	{
		sprintf (text, "%s\n", shippingData->last_name);
		text = text + strlen(text);
	}

	if ((shippingData->first_name[0] == '\0') &&
			(shippingData->initial_name[0] == '\0') &&
			(shippingData->last_name[0] == '\0'))
	{
		strcpy (text, "USER NAME:          N/A\n");
		text = text + strlen(text);
	}

	if (shippingData->title[0] != '\0')
		sprintf (text, "TITLE:              %s\n", shippingData->title);
	else
		strcpy (text, "TITLE:              N/A\n");
	text = text + strlen(text);
	
	if (shippingData->organization[0] != '\0')
		sprintf (text, "ORGANIZATION:       %s\n", shippingData->organization);
	else
		strcpy (text, "ORGANIZATION:       N/A\n");
	text = text + strlen(text);
	
	if (shippingData->street[0] != '\0')
		sprintf (text, "ADDRESS:            %s\n", shippingData->street);
	else
		strcpy (text, "ADDRESS:            N/A\n");
	text = text + strlen(text);

	if (shippingData->city[0] != '\0')
		sprintf (text, "CITY:               %s\n", shippingData->city);
	else
		strcpy (text, "CITY:               N/A\n");
	text = text + strlen(text);

	if (shippingData->state[0] != '\0')
		sprintf (text, "STATE:              %s\n", shippingData->state);
	else
		strcpy (text, "STATE:              N/A\n");
	text = text + strlen(text);

	if (shippingData->zipcode[0] != '\0')
		sprintf (text, "ZIPCODE:            %s\n", shippingData->zipcode);
	else
		strcpy (text, "ZIPCODE:            N/A\n");
	text = text + strlen(text);

	if (shippingData->country[0] != '\0')
		sprintf (text, "COUNTRY:            %s\n", shippingData->country);
	else
		strcpy (text, "COUNTRY:            N/A\n");
	text = text + strlen(text);

	if (shippingData->phone[0] != '\0')
		sprintf (text, "PHONE:              %s\n", shippingData->phone);
	else
		strcpy (text, "PHONE:              N/A\n");
	text = text + strlen(text);

	if (shippingData->fax[0] != '\0')
		sprintf (text, "FAX:                %s\n", shippingData->fax);
	else
		strcpy (text, "FAX:                N/A\n");
	text = text + strlen(text);

	if (shippingData->email[0] != '\0')
		sprintf (text, "ELEC MAIL:          %s\n", shippingData->email);
	else
		strcpy (text, "ELEC MAIL:          N/A\n");
	text = text + strlen(text);

	if (screenID)
	{
		XmTextSetString (UxShippingContext->UxshipToST, shipProfileBuf);
	}
	else
	{
		XmTextSetString (UxShipViewContext->UxshipToST, shipProfileBuf);
	}

	XtFree (shipProfileBuf);


	/*
	** set data to text fields
	*/
	if (shippingData->shipping_id)
	{
		sprintf (buffer, "%d", shippingData->shipping_id);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxshippingIdTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxshippingIdTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxshippingIdTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxshippingIdTF, "");
		}
	}
		
	if (shippingData->account_id)
	{
		sprintf (buffer, "%s", shippingData->account_id);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxaccountIdTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxaccountIdTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxaccountIdTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxaccountIdTF, "");
		}
	}

	if (shippingData->shipping_time)
	{
		sprintf (buffer, "%s", shippingData->shipping_time);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxshipDateTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxshipDateTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxshipDateTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxshipDateTF, "");
		}
	}
		
	if (shippingData->total_qty >= 0)
	{
		sprintf (buffer, "%d", shippingData->total_qty);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxtotalQtyTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxtotalQtyTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxtotalQtyTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxtotalQtyTF, "");
		}
	}

	if (shippingData->order_id)
	{
		sprintf (buffer, "%d", shippingData->order_id);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxorderIdTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxorderIdTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxorderIdTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxorderIdTF, "");
		}
	}

	if (shippingData->carrier)
	{
		sprintf (buffer, "%s", shippingData->carrier);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxcarrierTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxcarrierTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxcarrierTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxcarrierTF, "");
		}
	}

	if (shippingData->order_date)
	{
		sprintf (buffer, "%s", shippingData->order_date);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxorderDateTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxorderDateTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxorderDateTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxorderDateTF, "");
		}
	}

	if (shippingData->total_cost >= 0) 
	{
		sprintf (buffer, "%-.2f", shippingData->total_cost);
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxtotalCostTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxtotalCostTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxShippingContext->UxtotalCostTF, "");
		}
		else
		{
			XmTextFieldSetString (UxShipViewContext->UxtotalCostTF, "");
		}
	}


	/*
	** set data to ship item scrolled list
	*/
	position = 0;
	if (screenID)
	{
		XmListDeleteAllItems (UxShippingContext->UxshipItemSL);
	}
	else
	{
		XmListDeleteAllItems (UxShipViewContext->UxshipItemSL);
	}

	sPtr = shippingData->shipItemList;
	while (sPtr != (OP_SHIP_ITEM_LIST *)NULL)
	{	
		/* ignore the items with status 0 */
		if (sPtr->status)
		{
			/* map description - process_type */ 
			if (sPtr->process_type > 0)
			{
				k = 0;
				while ((k < glbData.process_type_count) && 
							 (sPtr->process_type != glbData.process_type[k].item_id)) 
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
			if (sPtr->media_type)
			{
				k = 0;
				while ((k < glbData.media_type_count) && 
							 (sPtr->media_type != glbData.media_type[k].item_id)) 
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

			/* map item status */
			if (sPtr->item_status)
			{
				k = 0;
				while ((k < glbData.item_status_count) && 
							 (sPtr->item_status != glbData.item_status[k].item_id)) 
							k++;

				if (k < glbData.item_status_count)
				{
				  strcpy (order_item_status, glbData.item_status[k].item_name);
				}
				else
				{
					/* did not find the matching media_type id */
				  strcpy (order_item_status, "N/A");
				}
			}
			else
			{
			  strcpy (order_item_status, "N/A");
			}


			position++;
			sprintf
				(buffer, "%-4d%-20.20s %-16.16s  %-11.11s  %-10.10s%-3d %.1f",
				 sPtr->item_id, process_type, sPtr->name, media_type, 
				 order_item_status, sPtr->quantity, sPtr->cost);

			listStr = XmStringCreateLocalized (buffer);
			if (screenID)
			{
				XmListAddItemUnselected
						(UxShippingContext->UxshipItemSL, listStr, position);
			}
			else
			{
				XmListAddItemUnselected
						(UxShipViewContext->UxshipItemSL, listStr, position);
			}

			XmStringFree (listStr);
		}

		sPtr = sPtr->next;
	}
		

	if (screenID)
	{
		glbData.shippingFlag = 1;
		XtPopup (XtParent(glbData.shippingW), XtGrabNone);
	}
	else
	{
		glbData.shipViewFlag = 1;
		XtPopup (XtParent(glbData.shipViewW), XtGrabNone);
	}
		

 return (IMS_OK);

}


