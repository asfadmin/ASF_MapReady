static char *sccs = "@(#)ims_opBillingCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opBillingCb.c

	Function:	Callback functions for Billing Screen

	Author:		Jennifer Ting

	Date:			11/1995

	Revision: 6/10/1996 - Modified function billing_closeCb to correct PR 942.

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

#define _IMS_OP_BILLINGCB_C
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
#include <ims_opBilling.h>
#undef CONTEXT_MACRO_ACCESS

#include <ims_opBillView.h>


/*===========================================================================*
** 
** Function Name: billinging_printFormCb
**
** Description:		Create callback to print the new billing form.
**								This callback is activated when OK button is pushed.
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

void billing_printFormCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
  _UxCbilling             *UxBillingContext;
	_UxCbillView						*UxBillViewContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *text;
	char cmdbuf[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	char tempFileName[IMS_COL255_LEN+1];
	FILE *fp;
	int screenID, i, itemCount;
	OP_CLIENT_DATA *clientData;
	XmStringTable str_list;
	DBINT order_id;
	XmScrollBarCallbackStruct *scroll_cbs;


	/* assign client to orderClientData from glbData structure */
	clientData = &(glbData.orderClientData);

	screenID = (int)cd;
	if (screenID)
	{
		UxBillingContext = 
					(_UxCbilling *) UxGetContext( glbData.billingW );
	}
	else
	{
		UxBillViewContext = 
					(_UxCbillView *) UxGetContext( glbData.billViewW );
	} 

	/* timeout cursor */
	timeOutCursors (True);

	/* put temporary print file in tmp directory */
	ims_concatFilePath (tempFileName, DEF_PRINT_DIRECTORY, 
			"ims_opBill.tmp");

	if ((fp = fopen(tempFileName, "a")) == (FILE *)NULL)
	{
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* Display error messages */
		sprintf(Msg, "Print Billing Form: cannot create printing temp file\n.");   
			msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 

		return; 
	}

	fprintf (fp, "\n\t\t\t        ALASKA  SAR  FACILITY\n");
	fprintf (fp, "\n\t\t\t                INVOICE      \n");
	fprintf (fp, "=======================================================================================\n");

	if (screenID)
	{
		text = XmTextGetString (UxBillingContext->UxbillToST);
	}
	else
	{
		text = XmTextGetString (UxBillViewContext->UxbillToST);
	}

	fprintf (fp, "BILL TO:\n");
	fprintf (fp, "=========\n\n");
	fprintf (fp, "%s\n", text);
	fprintf (fp, "=======================================================================================\n");

	fprintf (fp, "INFORMATION:\n");
	fprintf (fp, "============\n\n");

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxinvoiceIdTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxinvoiceIdTF);
	}
	fprintf (fp, "INVOICE ID:              %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxorderIdTF);
		order_id = atoi (text);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxorderIdTF);
	}
	fprintf (fp, "ORDER ID:                %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxorderDateTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxorderDateTF);
	}
	fprintf (fp, "ORDER DATE:              %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxbillDateTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxbillDateTF);
	}
	fprintf (fp, "BILL DATE:               %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxaccountIdTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxaccountIdTF);
	}
	fprintf (fp, "ACCOUNT ID:              %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxresourceTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxresourceTF);
	}
	fprintf (fp, "RESOURCE TYPE:           %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxbalanceTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxbalanceTF);
	}
	fprintf (fp, "ACCOUNT BALANCE:         %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxamountTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxamountTF);
	}
	fprintf (fp, "INVOICE AMOUNT:          %s\n", text);

	if (screenID)
	{
		text = XmTextFieldGetString (UxBillingContext->UxuserNameTF);
	}
	else
	{
		text = XmTextFieldGetString (UxBillViewContext->UxuserNameTF);
	}
	fprintf (fp, "USER NAME:               %s\n\n", text);


	fprintf (fp, "=======================================================================================\n");

	fprintf (fp, "BILL  ITEMS:\n");
	fprintf (fp, "============\n\n");
	fprintf
		(fp, "ITEM   DESCRIPTION       NAME              MEDIA        STATUS    QTY  COST\n");
	fprintf
		(fp, "=======================================================================================\n\n");

	/* get list items */
	if (screenID)
	{
		XtVaGetValues (UxBillingContext->UxbillItemSL, XmNitemCount, &itemCount,
									 XmNitems, &str_list, NULL);
	}
	else
	{
		XtVaGetValues (UxBillViewContext->UxbillItemSL, XmNitemCount, &itemCount,
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
	** popdown Billing Screen , refresh item list 
	*/ 
	if (screenID)
	{
		(void) billing_closeCb(wgt, cd, cb);
		/*
		** popdown Shipping Screen 
		** refresh the item list widgets to display shipping_ids 
		** and update shipped_p flags.
		*/
		if (screenID)
		{
			(void) billing_closeCb(wgt, cd, cb);
	
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
	}

	/* Change cursor back to normal */
	timeOutCursors (False);

}



/*===========================================================================*
** 
** Function Name: billing_cancelFormCb
**
** Description:		Callback function for the cancel form button in billing
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

void billing_cancelFormCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
  _UxCbilling            *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *value;
	int status;
	DBINT billing_id;
	DBINT order_id;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	char Msg[IMS_COL1024_LEN+1];
	OP_ORDER_ITEM_LIST *itemPtr;
	XmScrollBarCallbackStruct *scroll_cbs;

	UxSaveCtx = UxBillingContext;
	UxBillingContext = UxContext =
			(_UxCbilling *) UxGetContext( glbData.billingW );
	{
		/* timeout cursor */
		timeOutCursors (True);

		/* assign client to orderClientData from glbData structure */
		clientData = &(glbData.orderClientData);
		catReq = &(clientData->catReq);

		/* get billing_id from billingIdTF */
		value = XmTextFieldGetString (invoiceIdTF);
		billing_id = atoi (value);

		value = XmTextFieldGetString (orderIdTF);
		order_id = atoi (value);

		/*
		** delete billing entry from billing, billing_of tables 
		** update order_item billing_id, billed_p for items with
		** the specified billing_id 
		*/

		/*
		** Begin Transaction
		*/
		if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: Cancel Billing\n"
									 "OP_BEGINTRANSACTION failed.\n"); 
			msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 
		
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
			strcpy(Msg, "Internal Error: Cancel Billing\n"
								 "OP_GETGENERALLOCK failed.\n"); 
			msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 
		
			return;
		}

		/*
		** CAT Event OP_CANCELBILLING
		*/
		catReq->item[0] = (DBINT *)&billing_id;
		if ((status = ims_opCat(catReq, OP_CANCELBILLING)) < 0)
		{
			/* rollback transaction */
			ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy(Msg, "Internal Error: Cancel Billing\n"
								 "OP_CANCELBILLING failed.\n"); 
			msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 
		
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
			sprintf(Msg, "Internal Error: Cancel Billing,\n"
									 "OP_COMMITTRANSACTION failed for Billing ID: %d.\n",
									 billing_id);   
			msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 
	
			return;
		}
	
		/* Change cursor back to normal */
		timeOutCursors (False);

		/* popdown billing screen , refresh item list */
		(void) billing_closeCb(wgt, cd, cb);

		if (clientData->currOrder != NULL) 
		{
			if (clientData->currOrder->itemList != (OP_ORDER_ITEM_LIST *)NULL)
			{
				if (order_id == clientData->currOrder->order_id)
				{
					itemPtr = clientData->currOrder->itemList;
					while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL) 
					{
						if (itemPtr->billing_id == billing_id)
						{	
							itemPtr->billed_p = 'N';
							itemPtr->billing_id = -1;
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


	}
 	UxBillingContext = UxSaveCtx;
}



/*===========================================================================*
** 
** Function Name: billing_editCommentsCb
**
** Description:		Callback function for the edit comments button in billing
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

void billing_editCommentsCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
  _UxCbilling             *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	char *value;
	DBINT billing_id;

	UxSaveCtx = UxBillingContext;
	UxBillingContext = UxContext =
			(_UxCbilling *) UxGetContext( UxWidget );
	{
		/* get billing from billingIdTF */
		value = XmTextFieldGetString (invoiceIdTF);
		billing_id = atoi (value);

		commentDlg_popupCb (glbData.billingW, (void *)&billing_id, BILLING_COMMENT);

	}
 	UxBillingContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: billing_closeCb
**
** Description:		Callback function for the close button in billing 
**								Screen.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History: 06/10/96 - took out Billing context assignment
**
**==========================================================================*/

void billing_closeCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)

{
	XtPopdown (XtParent(glbData.billingW));
	glbData.billingFlag = 0;
}


/*===========================================================================*
** 
** Function Name: display_billingData
**
** Description:		Callback function to populate and popup the Billing Screen
**
** Arguments:			1. billingData - billing data spec
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

int display_billingData(
		OP_BILLING_DATA *billingData,
		int screenID)

{
  _UxCbilling             *UxBillingContext;
	_UxCbillView						*UxBillViewContext;

	char buffer[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];
	char *billProfileBuf, *text;
	char process_type[IMS_COL30_LEN+1];
	char media_type[IMS_COL30_LEN+1];
	char bill_item_id[IMS_COL10_LEN+1];
	char order_item_status[IMS_COL10_LEN+1];
	int k, position;
	OP_BILL_ITEM_LIST *bPtr;
	XmString listStr;


	if (screenID)
	{
		UxBillingContext = 
					(_UxCbilling *) UxGetContext( glbData.billingW );
	}
	else
	{
		UxBillViewContext = 
					(_UxCbillView *) UxGetContext( glbData.billViewW );
	} 


	/*
	** Display Billing Profile Information in BillTo scrolled text
	*/
	if ((billProfileBuf = XtMalloc ((unsigned int) 1024)) == (char *)NULL)
	{
		/* display error message, return */
		strcpy(Msg, "display_billingData:\n"
								"Internal Error: memory allocation failed.\n"
								"Please contact the DBA and exit the program.\n");
		msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
		return (IMS_FATAL);
	}

	/* Write billing profile information */
	text = billProfileBuf;

	if (billingData->first_name[0] != '\0') 
	{
		sprintf (text, "USER NAME:          %s  ", billingData->first_name);
		text = text + strlen(text);
	}

	if (billingData->initial_name[0] != '\0')
	{
		sprintf (text, "%s  ", billingData->initial_name);
		text = text + strlen(text);
	}

	if (billingData->last_name[0] != '\0')
	{
		sprintf (text, "%s\n", billingData->last_name);
		text = text + strlen(text);
	}

	if ((billingData->first_name[0] == '\0') &&
			(billingData->initial_name[0] == '\0') &&
			(billingData->last_name[0] == '\0'))
	{
		strcpy (text, "USER NAME:          N/A\n");
		text = text + strlen(text);
	}

	if (billingData->title[0] != '\0')
		sprintf (text, "TITLE:              %s\n", billingData->title);
	else
		strcpy (text, "TITLE:              N/A\n");
	text = text + strlen(text);
	
	if (billingData->organization[0] != '\0')
		sprintf (text, "ORGANIZATION:       %s\n", billingData->organization);
	else
		strcpy (text, "ORGANIZATION:       N/A\n");
	text = text + strlen(text);
	
	if (billingData->street[0] != '\0')
		sprintf (text, "ADDRESS:            %s\n", billingData->street);
	else
		strcpy (text, "ADDRESS:            N/A\n");
	text = text + strlen(text);

	if (billingData->city[0] != '\0')
		sprintf (text, "CITY:               %s\n", billingData->city);
	else
		strcpy (text, "CITY:               N/A\n");
	text = text + strlen(text);

	if (billingData->state[0] != '\0')
		sprintf (text, "STATE:              %s\n", billingData->state);
	else
		strcpy (text, "STATE:              N/A\n");
	text = text + strlen(text);

	if (billingData->zipcode[0] != '\0')
		sprintf (text, "ZIPCODE:            %s\n", billingData->zipcode);
	else
		strcpy (text, "ZIPCODE:            N/A\n");
	text = text + strlen(text);

	if (billingData->country[0] != '\0')
		sprintf (text, "COUNTRY:            %s\n", billingData->country);
	else
		strcpy (text, "COUNTRY:            N/A\n");
	text = text + strlen(text);

	if (billingData->phone[0] != '\0')
		sprintf (text, "PHONE:              %s\n", billingData->phone);
	else
		strcpy (text, "PHONE:              N/A\n");
	text = text + strlen(text);

	if (billingData->fax[0] != '\0')
		sprintf (text, "FAX:                %s\n", billingData->fax);
	else
		strcpy (text, "FAX:                N/A\n");
	text = text + strlen(text);

	if (billingData->email[0] != '\0')
		sprintf (text, "ELEC MAIL:          %s\n", billingData->email);
	else
		strcpy (text, "ELEC MAIL:          N/A\n");
	text = text + strlen(text);

	if (screenID)
	{
		XmTextSetString (UxBillingContext->UxbillToST, billProfileBuf);
	}
	else
	{
		XmTextSetString (UxBillViewContext->UxbillToST, billProfileBuf);
	}

	XtFree (billProfileBuf);


	/*
	** set data to text fields
	*/
	if (billingData->billing_id)
	{
		sprintf (buffer, "%d", billingData->billing_id);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxinvoiceIdTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxinvoiceIdTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxinvoiceIdTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxinvoiceIdTF, "");
		}
	}
		
	if (billingData->account_id)
	{
		sprintf (buffer, "%s", billingData->account_id);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxaccountIdTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxaccountIdTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxaccountIdTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxaccountIdTF, "");
		}
	}

	if (billingData->curr_balance >= 0)
	{
		sprintf (buffer, "%-.2f", billingData->curr_balance);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxbalanceTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxbalanceTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxbalanceTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxbalanceTF, "");
		}
	}


	if (billingData->create_time)
	{
		sprintf (buffer, "%s", billingData->create_time);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxbillDateTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxbillDateTF, buffer);
		}
	}
	else 
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxbillDateTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxbillDateTF, "");
		}
	}
		

	if (billingData->order_id)
	{
		sprintf (buffer, "%d", billingData->order_id);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxorderIdTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxorderIdTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxorderIdTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxorderIdTF, "");
		}
	}


	if (billingData->order_date)
	{
		sprintf (buffer, "%s", billingData->order_date);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxorderDateTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxorderDateTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxorderDateTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxorderDateTF, "");
		}
	}

	if (billingData->invoice_amount >= 0) 
	{
		sprintf (buffer, "%-.2f", billingData->invoice_amount);
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxamountTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxamountTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxamountTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxamountTF, "");
		}
	}

	/* map resource type */ 
	if (billingData->resource_type > 0)
	{
		k = 0;
		while ((k < glbData.resource_type_count) && 
					 (billingData->resource_type != glbData.resource_type[k].item_id)) 
					k++;

		if (k < glbData.resource_type_count)
		{
			strcpy (buffer, glbData.resource_type[k].item_name);
		}
		else
		{
			/* did not find the matching resource_type id */
			/* changed from blank string to NA - oct 95 */
			strcpy (buffer, "N/A");
		}
	}
	else
	{
		strcpy (buffer, "N/A");
	}

	if (screenID)
	{
		XmTextFieldSetString (UxBillingContext->UxresourceTF, buffer);
	}
	else
	{
		XmTextFieldSetString (UxBillViewContext->UxresourceTF, buffer);
	}


	/* concat user's first, m, last names */
	strcpy (buffer, billingData->user_first_name);
	strcat (buffer, "  ");
	strcat (buffer, billingData->user_initial_name);
	strcat (buffer, "  ");
	strcat (buffer, billingData->user_last_name);
	if (buffer)
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxuserNameTF, buffer);
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxuserNameTF, buffer);
		}
	}
	else
	{
		if (screenID)
		{
			XmTextFieldSetString (UxBillingContext->UxuserNameTF, "");
		}
		else
		{
			XmTextFieldSetString (UxBillViewContext->UxuserNameTF, "");
		}
	}



	/*
	** set data to bill item scrolled list
	*/
	position = 0;
	if (screenID)
	{
		XmListDeleteAllItems (UxBillingContext->UxbillItemSL);
	}
	else
	{
		XmListDeleteAllItems (UxBillViewContext->UxbillItemSL);
	}

	bPtr = billingData->billItemList;
	while (bPtr != (OP_BILL_ITEM_LIST *)NULL)
	{	
		/* ignore the items with status 0 */
		if (bPtr->status)
		{
			/* map description - process_type */ 
			if (bPtr->process_type > 0)
			{
				k = 0;
				while ((k < glbData.process_type_count) && 
							 (bPtr->process_type != glbData.process_type[k].item_id)) 
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
			if (bPtr->media_type)
			{
				k = 0;
				while ((k < glbData.media_type_count) && 
							 (bPtr->media_type != glbData.media_type[k].item_id)) 
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
			if (bPtr->item_status)
			{
				k = 0;
				while ((k < glbData.item_status_count) && 
							 (bPtr->item_status != glbData.item_status[k].item_id)) 
							k++;

				if (k < glbData.item_status_count)
				{
				  strcpy (order_item_status, glbData.item_status[k].item_name);
				}
				else
				{
					/* did not find the matching item status id */
				  strcpy (order_item_status, "N/A");
				}
			}
			else
			{
			  strcpy (order_item_status, "N/A");
			}


			position++;
			sprintf
				(buffer, "%-4d%-20.20s %-16.16s  %-11.11s  %-10.10s%-3d  %.1f",
				 bPtr->item_id, process_type, bPtr->name, media_type, 
				 order_item_status, bPtr->quantity, bPtr->cost);

			listStr = XmStringCreateLocalized (buffer);
			if (screenID)
			{
				XmListAddItemUnselected
						(UxBillingContext->UxbillItemSL, listStr, position);
			}
			else
			{
				XmListAddItemUnselected
						(UxBillViewContext->UxbillItemSL, listStr, position);
			}

			XmStringFree (listStr);
		}

		bPtr = bPtr->next;
	}
		

	if (screenID)
	{
		glbData.billingFlag = 1;
		XtPopup (XtParent(glbData.billingW), XtGrabNone);
	}
	else
	{
		glbData.billViewFlag = 1;
		XtPopup (XtParent(glbData.billViewW), XtGrabNone);
	}
		

 return (IMS_OK);

}


