static char *sccs = "@(#)ims_opCommentDlgCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opCommentDlgCb.c

	Function:	Callback functions for comment dialog box 

	Author:		Jennifer Ting

	Date:			3/1995

	Revision: 6/11/1996 - Modified function commentDlg_popupCb to correct PR 942.

*******************************************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/SelectioB.h>
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

#define _IMS_OP_COMMENTDLGCB_H
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opCommentDlg.h>
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: commentDlg_cancelCb
**
** Description:		Callback function for the Cancel button in selectionDlg,
**								destroys the selection dialog box.
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

void commentDlg_cancelCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCcommentDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxCommentDlgContext;
	UxCommentDlgContext = UxContext =
			(_UxCcommentDlg *) UxGetContext( UxWidget );
	{
		XtDestroyWidget(wgt);	
	}
	UxCommentDlgContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: commentDlg_popupCb
**
** Description:		Callback function to popup the comment dialog.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. text   - text to be displayed in the browse dlg.
**								3. cb 		- not used.
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void commentDlg_popupCb(
	Widget wgt, 
	void *ptr,
	int fieldId)
{
	_UxCcommentDlg 			          *UxSaveCtx, *UxContext;
	Widget         				         UxWidget = wgt;
	Widget comment_dlg_w;
	XmString label, title;
	char buffer[IMS_COL255_LEN+1];
	int status;
	DBINT shipping_id, billing_id;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	OP_LASER_QUEUE_LIST *laserQPtr;
	char query[IMS_COL255_LEN+1];
	char text[IMS_COL255_LEN+1];
	char Msg[IMS_COL255_LEN+1];


	comment_dlg_w = create_commentDlg(wgt);
	clientData = &(glbData.orderClientData);
	catReq = &(clientData->catReq);

	UxSaveCtx = UxCommentDlgContext;
	UxCommentDlgContext = UxContext =
		(_UxCcommentDlg *) UxGetContext(comment_dlg_w);

	{

		switch (fieldId)
		{
			case ORDER_COMMENT:
				orderPtr = (OP_ORDER_LIST *)ptr;
				title = XmStringCreateLocalized ("Edit Order Comment");
				sprintf (buffer, "Order ID:   %d", orderPtr->order_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, orderPtr, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, ORDER_COMMENT, NULL);
				XmTextSetString (commentOldST, orderPtr->op_comment);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			case ITEM_COMMENT:
				itemPtr = (OP_ORDER_ITEM_LIST *)ptr;
				title = XmStringCreateLocalized ("Edit Item Comment");
				sprintf (buffer, "Order ID:   %d    Item ID:   %d",
												 itemPtr->order_id, itemPtr->item_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, itemPtr, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, ITEM_COMMENT, NULL);
				XmTextSetString (commentOldST, itemPtr->op_comment);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			case EDIT_PHOTO_COMMENT:
				photoQPtr = (OP_PHOTO_QUEUE_LIST *)ptr;
				title = XmStringCreateLocalized ("Edit Photo Item Comment");
				sprintf (buffer, "Photo Job:  %d    Order ID:  %d    Item ID:  %d",
												 photoQPtr->photojob_id,
												 photoQPtr->order_id, photoQPtr->item_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, photoQPtr, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, EDIT_PHOTO_COMMENT, NULL);
				XmTextSetString (commentOldST, photoQPtr->op_comment);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			case VIEW_PHOTO_COMMENT:
				photoQPtr = (OP_PHOTO_QUEUE_LIST *)ptr;
				title = XmStringCreateLocalized ("View Photo Item Comment");
				sprintf (buffer, "Photo Job:  %d    Order ID:  %d    Item ID:  %d",
												 photoQPtr->photojob_id,
												 photoQPtr->order_id, photoQPtr->item_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XmTextSetString (commentOldST, photoQPtr->op_comment);
				XtSetSensitive (commentUpdatePB, False);
				XtVaSetValues (commentST, XmNeditable, False, NULL);
				XtSetSensitive (commentNewLB, False);
			break;

			case FIRE_COMMENT:
				fireQPtr = (OP_FIRE_QUEUE_LIST *)ptr;
				title = XmStringCreateLocalized ("View Fire Queue Item Comment");
				sprintf (buffer, "Order ID:   %d    Item ID:   %d",
												 fireQPtr->order_id, fireQPtr->item_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, fireQPtr, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, FIRE_COMMENT, NULL);
				XmTextSetString (commentOldST, fireQPtr->op_comment);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			case LASER_COMMENT:
				laserQPtr = (OP_LASER_QUEUE_LIST *)ptr;
				title = XmStringCreateLocalized ("View Laser Queue Item Comment");
				sprintf (buffer, "Order ID:   %d    Item ID:   %d",
												 laserQPtr->order_id, laserQPtr->item_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, laserQPtr, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, LASER_COMMENT, NULL);
				XmTextSetString (commentOldST, laserQPtr->op_comment);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			case SHIPPING_COMMENT:
				shipping_id = *(DBINT *)ptr;
				/* 
				** retrieve op_comment from shipping table
				*/
				sprintf (query,
					"select op_comment from shipping where shipping_id = %d ",
					shipping_id);
				catReq->item[0] = (char *)query;
				catReq->item[1] = (char *)text;
				if ((status = ims_opCat (catReq, OP_GETOPCOMMENT)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(Msg,
					 "Create New Shipment: operator comment retrieval failed.\n"
					 "Internal Error: OP_GETOPCOMMENT failed for shipping_id: %d\n",
					 shipping_id);
					msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 
					return;
				}

				title = XmStringCreateLocalized ("Edit Shipping Comment");
				sprintf (buffer, "Shipping ID:   %d", shipping_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, shipping_id, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, SHIPPING_COMMENT, NULL);
				XmTextSetString (commentOldST, text);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			case BILLING_COMMENT:
				billing_id = *(DBINT *)ptr;
				/* 
				** retrieve op_comment from billing table
				*/
				sprintf (query,
					"select op_comment from billing where billing_id = %d ",
					billing_id);
				catReq->item[0] = (char *)query;
				catReq->item[1] = (char *)text;
				if ((status = ims_opCat (catReq, OP_GETOPCOMMENT)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(Msg,
					 "Create New Invoice: operator comment retrieval failed.\n"
					 "Internal Error: OP_GETOPCOMMENT failed for billing_id: %d\n",
					 billing_id);
					msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 
					return;
				}

				title = XmStringCreateLocalized ("Edit New Invoice Comment");
				sprintf (buffer, "Billing ID:   %d", billing_id);
				label = XmStringCreateLocalized (buffer);
				XtVaSetValues (comment_dlg_w, XmNdialogTitle, title, NULL);
				XtVaSetValues (commentLB, XmNlabelString, label, NULL);
				XtVaSetValues (comment_dlg_w, XmNuserData, billing_id, NULL);
				XtVaSetValues (commentUpdatePB, XmNuserData, BILLING_COMMENT, NULL);
				XmTextSetString (commentOldST, text);
				XtSetSensitive (commentUpdatePB, True);
				XtVaSetValues (commentST, XmNeditable, True, NULL);
				XtSetSensitive (commentNewLB, True);
			break;

			default:
			break;

		}

		/*
		** This is to add the callbacks to the window manager quit
		** button for the screen, this is to correct PR 942
		*/
		addWinMgrCloseCB (comment_dlg_w, commentDlg_cancelCb, NULL);

		XtManageChild(comment_dlg_w);

		XmStringFree (title);
		XmStringFree (label);
	}

	UxCommentDlgContext = UxSaveCtx;

}
	
/*===========================================================================*
** 
** Function Name: commentDlg_updateCb
**
** Description:		Callback function for the Update button in selectionDlg,
**								execute catalog comment update function.
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

void commentDlg_updateCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCcommentDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_PHOTO_CLIENT_DATA *photoClientData;
	OP_FILM_CLIENT_DATA *filmClientData;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	OP_PHOTO_QUEUE_LIST *photoQPtr;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	OP_LASER_QUEUE_LIST *laserQPtr;
	char Msg[IMS_COL1024_LEN+1];
	char *text;
	int fieldId, status, value;
	Widget comment_dlg_w;
	DBINT shipping_id, billing_id;


	UxSaveCtx = UxCommentDlgContext;
	UxCommentDlgContext = UxContext =
			(_UxCcommentDlg *) UxGetContext( UxWidget );
	{

		comment_dlg_w = XtParent(wgt);
		XtVaGetValues (commentUpdatePB, XmNuserData, &fieldId, NULL);
		text = XmTextGetString (commentST);

		clientData = &(glbData.orderClientData);
		photoClientData = &(glbData.photoClientData);
		filmClientData = &(glbData.filmClientData);

		switch (fieldId)
		{
			case (ORDER_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, 
											 &orderPtr, NULL);

				/* update the comment only if modification occured */
				if (strcmp (orderPtr->op_comment, text) != 0)
				{
					catReq = &(clientData->catReq);
					catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
					catReq->item[1] = (char *)text;

					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Order Comment,\n"
												 "OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
												 orderPtr->order_id);   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return; 
					}

					if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Order Comment,\n" 
												 "OP_GETORDERLOCK failed for Order ID: %d\n", 
												 orderPtr->order_id);   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return;
					}

					if ((status = ims_opCat(catReq, OP_UPDATEORDERCOMMENT)) < 0)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Order Comment,\n" 
												 "OP_UPDATEORDERCOMMENT failed for Order ID: %d\n", 
													orderPtr->order_id);
						msgBoxDlg_popupCb(glbData.orderW, IMS_FATAL, Msg);

						XtFree (text);
						return;
					}

					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Order Comment,\n"
												 "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
												 orderPtr->order_id);   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

						XtFree (text);
						return;
					}

					strcpy (orderPtr->op_comment, text);

				}

			break;

			case (ITEM_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, 
											 &itemPtr, NULL);

				/* update the comment only if modification occured */
				if (strcmp (itemPtr->op_comment, text) != 0)
				{
					catReq = &(clientData->catReq);
					catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;
					catReq->item[1] = (char *)text;

					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Item Comment,\n"
												 "OP_BEGINTRANSACTION failed for\n"
												 "Order: %d, item %d.\n", 
												 itemPtr->order_id, itemPtr->item_id);   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return; 
					}

					if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Item Comment,\n" 
												 "OP_GETITEMLOCK failed for\n"
												 "Order: %d, item %d.\n", 
												 itemPtr->order_id, itemPtr->item_id);   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return;
					}

					if ((status = ims_opCat(catReq, OP_UPDATEITEMCOMMENT)) < 0)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Item Comment,\n" 
												 "OP_UPDATEITEMCOMMENT failed for\n"
												 "Order: %d, item %d.\n", 
												 itemPtr->order_id, itemPtr->item_id);   
						msgBoxDlg_popupCb(glbData.orderW, IMS_FATAL, Msg);

						XtFree (text);
						return;
					}

					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Item Comment,\n"
												 "OP_COMMITTRANSACTION failed for\n"
												 "Order: %d, item %d.\n",
												 itemPtr->order_id, itemPtr->item_id);   
						msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 

						XtFree (text);
						return;
					}
					strcpy (itemPtr->op_comment, text);

				}

			break;


			case (EDIT_PHOTO_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, 
											 &photoQPtr, NULL);

				/* update the comment only if modification occured */
				if (strcmp (photoQPtr->op_comment, text) != 0)
				{
					catReq = &(photoClientData->catReq);
					catReq->item[0] = (OP_PHOTO_QUEUE_LIST *)photoQPtr;
					catReq->item[1] = (char *)text;

					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Photo Item Comment,\n"
												 "OP_BEGINTRANSACTION failed for\n"
												 "Photo Job: %d, Order: %d, Item %d.\n", 
												 photoQPtr->photojob_id,
												 photoQPtr->order_id, photoQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return; 
					}

					if ((status = ims_opCat(catReq, OP_UPDATEPHOTOCOMMENT)) < 0)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Photo Item Comment,\n" 
												 "OP_UPDATEPHOTOCOMMENT failed for\n"
												 "Photo Job: %d, Order: %d, Item %d.\n", 
												 photoQPtr->photojob_id,
												 photoQPtr->order_id, photoQPtr->item_id);   
						msgBoxDlg_popupCb(glbData.photoJobW, IMS_FATAL, Msg);

						XtFree (text);
						return;
					}

					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Photo Item Comment,\n"
												 "OP_COMMITTRANSACTION failed for\n"
												 "Photo Job: %d, Order: %d, item %d.\n", 
												 photoQPtr->photojob_id,
												 photoQPtr->order_id, photoQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

						XtFree (text);
						return;
					}
					strcpy (photoQPtr->op_comment, text);

				}

			break;

			case (FIRE_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, 
											 &fireQPtr, NULL);

				/* update the comment only if modification occured */
				if (strcmp (fireQPtr->op_comment, text) != 0)
				{
					catReq = &(filmClientData->catReq);
					catReq->item[0] = (OP_FIRE_QUEUE_LIST *)fireQPtr;
					catReq->item[1] = (char *)text;

					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Fire Item Comment,\n"
												 "OP_BEGINTRANSACTION failed for\n"
												 "Order: %d, item %d.\n", 
												 fireQPtr->order_id, fireQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return; 
					}

					if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Fire Item Comment,\n" 
												 "OP_GETITEMLOCK failed for\n"
												 "Order: %d, item %d.\n", 
												 fireQPtr->order_id, fireQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return;
					}

					if ((status = ims_opCat(catReq, OP_UPDATEFIREITEMCOMMENT)) < 0)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Fire Item Comment,\n" 
												 "OP_UPDATEFIREITEMCOMMENT failed for\n"
												 "Order: %d, item %d.\n", 
												 fireQPtr->order_id, fireQPtr->item_id);   
						msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

						XtFree (text);
						return;
					}

					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Fire Item Comment,\n"
												 "OP_COMMITTRANSACTION failed for\n"
												 "Order: %d, item %d.\n",
												 fireQPtr->order_id, fireQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

						XtFree (text);
						return;
					}
					strcpy (fireQPtr->op_comment, text);

				}

			break;


			case (LASER_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, 
											 &laserQPtr, NULL);

				/* update the comment only if modification occured */
				if (strcmp (laserQPtr->op_comment, text) != 0)
				{
					catReq = &(filmClientData->catReq);
					catReq->item[0] = (OP_LASER_QUEUE_LIST *)laserQPtr;
					catReq->item[1] = (char *)text;

					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Laser Item Comment,\n"
												 "OP_BEGINTRANSACTION failed for\n"
												 "Order: %d, item %d.\n", 
												 laserQPtr->order_id, laserQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return; 
					}

					if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Laser Item Comment,\n" 
												 "OP_GETITEMLOCK failed for\n"
												 "Order: %d, item %d.\n", 
												 laserQPtr->order_id, laserQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 
	
						XtFree (text);
						return;
					}

					if ((status = ims_opCat(catReq, OP_UPDATELASERITEMCOMMENT)) < 0)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Laser Item Comment,\n" 
												 "OP_UPDATELASERITEMCOMMENT failed for\n"
												 "Order: %d, item %d.\n", 
												 laserQPtr->order_id, laserQPtr->item_id);   
						msgBoxDlg_popupCb(glbData.filmW, IMS_FATAL, Msg);

						XtFree (text);
						return;
					}

					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(Msg, "Internal Error:\nUpdate Laser Item Comment,\n"
												 "OP_COMMITTRANSACTION failed for\n"
												 "Order: %d, item %d.\n",
												 laserQPtr->order_id, laserQPtr->item_id);   
						msgBoxDlg_popupCb (glbData.filmW, IMS_FATAL, Msg); 

						XtFree (text);
						return;
					}
					strcpy (laserQPtr->op_comment, text);

				}

			break;


			case (SHIPPING_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, 
											 &shipping_id, NULL);

				/* For shipping screen the comments are always new */
				catReq = &(clientData->catReq);
				catReq->item[0] = (DBINT *)&shipping_id;
				catReq->item[1] = (char *)text;

				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Shipping Comment,\n"
											 "OP_BEGINTRANSACTION failed for\n"
											 "Shipping ID: %d.\n", 
											 shipping_id);
					msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 

					XtFree (text);
					return; 
				}

				if ((status = ims_opCat (catReq, OP_GETGENERALLOCK)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Shipping Comment,\n" 
											 "OP_GETGENERALLOCK failed for\n"
											 "Shipping ID: %d.\n", 
											 shipping_id);
					msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 

					XtFree (text);
					return;
				}

				if ((status = ims_opCat(catReq, OP_UPDATESHIPPINGCOMMENT)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Shipping Comment,\n" 
											 "OP_UPDATESHIPPINGCOMMENT failed for\n"
											 "Shipping ID: %d.\n", 
											 shipping_id);
					msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 

					XtFree (text);
					return;
				}

				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Shipping Comment,\n"
											 "OP_COMMITTRANSACTION failed for\n"
											 "Shipping ID: %d.\n", 
											 shipping_id);
					msgBoxDlg_popupCb (glbData.shippingW, IMS_FATAL, Msg); 

					XtFree (text);
					return;
				}

			break;


			case (BILLING_COMMENT):
				XtVaGetValues (comment_dlg_w, XmNuserData, &billing_id, NULL);
				catReq = &(clientData->catReq);

				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Invoice Comment,\n"
											 "OP_BEGINTRANSACTION failed for\n"
											 "Invoice ID: %d.\n", 
											 billing_id);
					msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 

					XtFree (text);
					return; 
				}

				if ((status = ims_opCat (catReq, OP_GETGENERALLOCK)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Invoice Comment,\n" 
											 "OP_GETGENERALLOCK failed for\n"
											 "Invoice ID: %d.\n", 
											 billing_id);
					msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 

					XtFree (text);
					return;
				}

				catReq->item[0] = (DBINT *)&billing_id;
				catReq->item[1] = (char *)text;
				if ((status = ims_opCat(catReq, OP_UPDATEBILLINGCOMMENT)) < 0)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Invoice Comment,\n" 
											 "OP_UPDATEBILLINGCOMMENT failed for\n"
											 "Invoice ID: %d.\n", 
											 billing_id);
					msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 

					XtFree (text);
					return;
				}

				if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(Msg, "Internal Error:\nUpdate Invoice Comment,\n"
											 "OP_COMMITTRANSACTION failed for\n"
											 "Invoice ID: %d.\n", 
											 billing_id);
					msgBoxDlg_popupCb (glbData.billingW, IMS_FATAL, Msg); 

					XtFree (text);
					return;
				}

			break;

			default:
			break;
		}

		XtDestroyWidget(wgt);	
		XtFree (text);

	}
	UxCommentDlgContext = UxSaveCtx;
}
	
