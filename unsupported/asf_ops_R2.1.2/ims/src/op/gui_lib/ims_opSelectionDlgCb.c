static char *sccs = "@(#)ims_opSelectionDlgCb.c	5.5  09/18/96";
/*******************************************************************************

	File:			ims_opSelectionDlgCb.c

	Function:	Callback functions for selection dialog box 

	Author:		Jennifer Ting

	Date:			3/1995

	Revision: 05-08-96  J. Ting  Modified function selectionDlg_orderStatus_okCb
															 Added additional checking upon canceling an 
															 order. 

	Revision: 05-08-96  J. Ting  Modified function selectionDlg_itemStatus_okCb
															 Added additional checking upon canceling an
															 item.

						07-15-96  J. Ting  Modified function selectionDlg_orderStatus_okC
						                   b to correct PR 990.  Order Status CANCEL and
															 CANCELLED have been removed.

						07-15-96  J. Ting  Modified the following functions to verify
															 internal id mapping:
															 selectionDlg_itemStatus_okCb
															 selectionDlg_fireStatus_okCb
															 selectionDlg_laserStatus_okCb

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

#define _IMS_OP_SELECTIONDLGCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

/*
char *status_valids[] = {"CANCEL", "HOLD", "COMPLETE"};
*/
char *status_valids[] = {"CANCEL", "COMPLETE"};
char *fire_status_valids[] = {"NEW", "COMPLETE", "CANCEL"};
char *laser_status_valids[] = {"NEW", "COMPLETE", "CANCEL"};

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opSelectionBoxDlg.h>
#undef CONTEXT_MACRO_ACCESS

#include <ims_opPhotoOrder.h>

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

	
/*===========================================================================*
** 
** Function Name: selectionDlg_orderPriority_okCb
**
** Description:		Callback function for the OK button in order priority
**								selection dialog.  Update order priority in the database,
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

void selectionDlg_orderPriority_okCb(
			Widget wgt, 
			OP_ORDER_LIST *orderPtr,
			XtPointer cb)
{
	_UxCselectionBoxDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;

	char *value;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr;
	int i, status; 
	short priority_id;
	int len = 0;
	int errorFlag, modifyFlag;
	DBSMALLINT current_status;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	char *op_comment;
	Widget selection_dlg_w;
	XmScrollBarCallbackStruct *scroll_cbs;
	char Msg[IMS_COL80_LEN+1];


	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
	{

		/*
		** Get valid selected 
		*/
		selection_dlg_w = wgt;
		XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
		op_comment = XmTextGetString (selection_commentST);
		ims_truncStr(op_comment);
		XtDestroyWidget(wgt);	
		timeOutCursors (True);

		if ((len = strlen(value)) > 0)
		{
			/*
			** Find the matching item_id for the priority string 
			*/
			i = 0;
			while ((strcmp (value, glbData.priority[i].item_name) != 0) &&
						 (i < glbData.priority_count)) i++;

			priority_id = glbData.priority[i].item_id;

			/*
			** assign client to orderClientData from glbData structure 
			*/
			clientData = &(glbData.orderClientData);
			catReq = &(clientData->catReq);
			itemPtr = orderPtr->itemList;
			modifyFlag = 0;

			while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
			{
				/*
				** Begin the update transaction 
				*/
				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(new_msg,
									"Internal Error:\nUpdate Order Priority,\n"
									"OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
									itemPtr->order_id, itemPtr->item_id);   
	
					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

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
								  "Internal Error:\nUpdate Order Priority,\n" 
								  "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
									itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/* 
				** 02/12/96 - update item priority is allowed only
				** for the following status values: NEW, IN-MEDIA,
				** and GENERATED.
				** Get current item status from order_item table
				** to verify the item status 
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
						XtFree(value);
						XtFree (op_comment);

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
				if ((current_status != ITEM_NEW) &&
						(current_status != ITEM_IN_MEDIA) &&
						(current_status != ITEM_GENERATED))
				{
					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(new_msg,
						 "Update Order Priority is allowed only on items\n"
					 	 "with the following status values:\n"
						 "NEW, IN-MEDIA, GENERATED\n"
						 "Order: %d, Item: %d does not qualify this operation.\n\n", 
						 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** Update item priority 
				*/
				catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;
				catReq->item[1] = (short *)&priority_id;
				if ((status = ims_opCat (catReq, OP_UPDATEITEMPRIORITY)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg, 
									"Internal Error:\nUpdate Order Priority,\n"
									"OP_UPDATEITEMPRIORITY failed for Order: %d, Item: %d\n",
									itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

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
									"Internal Error:\nUpdate Order Priority,\n"
								  "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n",
									itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** update priority and comment of the item in memory 
				*/
				modifyFlag = 1;
				itemPtr->priority = priority_id; 

				itemPtr = itemPtr->next;

			}  /* while */


			/*
			** if none of the items got modified, no need to continue
			*/
			if (modifyFlag)
			{
				errorFlag = IMS_FALSE;

				/*
				** Begin the update transaction 
				*/
				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					errorFlag = IMS_TRUE;

					/* Display error messages */
					sprintf(new_msg, "Internal Error:\nUpdate Order Priority,\n"
										 "OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
										 orderPtr->order_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

				}
				else
					if ((status = ims_opCat (catReq, OP_GETORDERLOCK)) < IMS_OK)
					{
						errorFlag = IMS_TRUE;

						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg, "Internal Error:\nUpdate Order Priority,\n" 
														 "OP_GETORDERLOCK failed for Order ID: %d\n", 
														 orderPtr->order_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

					}
					else
					{
						/*
						** Get new order priority 
						*/
						catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
						catReq->item[1] = (short *)&priority_id;
						if ((status = ims_opCat (catReq, OP_GETNEWORDERPRIORITY)) < IMS_OK)
						{
							errorFlag = IMS_TRUE;

							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf(new_msg, "Internal Error:\nUpdate Order Priority,\n"
												 "OP_UPDATEORDERPRIORITY failed for Order ID: %d.\n",
												 orderPtr->order_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);
								XtFree (op_comment);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

						}
						else
						{
							if (*op_comment)
							{
								catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
								catReq->item[1] = (char *)op_comment;
								if ((status = ims_opCat (catReq, OP_UPDATEORDERCOMMENT)) < IMS_OK)
								{
									errorFlag = IMS_TRUE;

									/* rollback transaction */
									ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
									/* Display error messages */
									sprintf(new_msg, "Internal Error:\nUpdate Order Priority,\n"
												 "OP_UPDATEORDERCOMMENT failed for Order ID: %d.\n",
												 orderPtr->order_id);   

									if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
									{
										/* memory allocation failed, clean up, return */
										XtFree(value);
										XtFree (op_comment);
	
										OP_FREECHARPTR(concat_msg.Msg);

										/* Change cursor back to normal */
										timeOutCursors (False);
										return;
									}
	
								}
							}

							/*
							** Commit transaction 
							*/
							if (!errorFlag)
							{
								if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
								{
									errorFlag = IMS_TRUE;

									/* rollback transaction */
									ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

									/* Display error messages */
									sprintf(new_msg, "Internal Error:\nUpdate Order Priority,\n"
												 "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
												 orderPtr->order_id);   

									if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
									{
										/* memory allocation failed, clean up, return */
										XtFree(value);
										XtFree (op_comment);

										OP_FREECHARPTR(concat_msg.Msg);

										/* Change cursor back to normal */
										timeOutCursors (False);
										return;
									}
								}
							}
						}
					}

				/*
				** update priority and comment of the order in data structure
				*/
				if (!errorFlag)
				{
					if (*op_comment)
						strcpy (orderPtr->op_comment, op_comment);

					/*
					** refresh order lists 
					*/
					scroll_cbs = (XmScrollBarCallbackStruct *)
								malloc(sizeof(XmScrollBarCallbackStruct));
					scroll_cbs->value = clientData->orderWindowTop;
					order_scroll_orderListsCb (glbData.orderW, NULL, scroll_cbs);  
					free (scroll_cbs);
				}

				/*
				** if items are currently being displayed, refresh item lists 
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
			}


			/* Display all messages in browse dialog box */
			if (concat_msg.Msg != NULL)
			{
				if ((len = strlen(concat_msg.Msg)) > 1024)
				{
					strcpy (label, "Update  Order  Priority  Message  Box");
					browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
				}
				else
				{
					msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
				}
			}

			/* free up concat_msg space allocated */
			OP_FREECHARPTR(concat_msg.Msg);

		}

		XtFree (value);	
		XtFree (op_comment);

		/* change cursor back to normal */
		timeOutCursors (False);

 }
 UxSelectionBoxDlgContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: selectionDlg_itemPriority_okCb
**
** Description:		Callback function for the OK button in item priority
**								selection dialog.  Update item priority in the database,
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

void selectionDlg_itemPriority_okCb(
			Widget wgt, 
			OP_ORDER_ITEM_LIST *itemPtr,
			XtPointer cb)
{
	_UxCselectionBoxDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;

	char *value;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	int itemCount;
	short priority_id;
	int i, count, status, modifyFlag;
	int len = 0;
	DBSMALLINT current_status;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	char *op_comment;
	Widget selection_dlg_w;
	XmScrollBarCallbackStruct *scroll_cbs;


	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
	{

		/*
		** Get valid selected 
		*/
		selection_dlg_w = wgt;
		XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
		op_comment = XmTextGetString (selection_commentST);
		ims_truncStr(op_comment);
		XtDestroyWidget(wgt);	
		timeOutCursors (True);

		if ((len = strlen(value)) > 0)
		{
			/*
			** Find the matching item_id for the priority string 
			*/
			i = 0;
			while ((strcmp (value, glbData.priority[i].item_name) != 0) &&
						 (i < glbData.priority_count)) i++;

			priority_id = glbData.priority[i].item_id;

			/*
			** assign client to orderClientData from glbData structure 
			*/
			clientData = &(glbData.orderClientData);
			catReq = &(clientData->catReq);
			orderPtr = clientData->currOrder;
			itemCount = clientData->currOrder->item_count;
			modifyFlag = 0;
			count = 0;

			while ((itemPtr != (OP_ORDER_ITEM_LIST *)NULL) && (count < itemCount))
			{
				/*
				** Update item priority only if it has changed 
				*/
				if ((itemPtr->selectFlag) && (priority_id != itemPtr->priority))
				{
					/*
					** Begin the update transaction 
					*/
					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(new_msg,
										"Internal Error:\nUpdate Item Priority,\n"
										"OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
										itemPtr->order_id, itemPtr->item_id);   
	
						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

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
									  "Internal Error:\nUpdate Item Priority,\n" 
									  "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
										itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}



					/* 
					** 02/12/96 - update item priority is allowed only
					** for the following status values: NEW, IN-MEDIA,
					** and GENERATED.
					** Get current item status from order_item table
					** to verify the item status 
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
							XtFree(value);
							XtFree (op_comment);

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
					if ((current_status != ITEM_NEW) &&
							(current_status != ITEM_IN_MEDIA) &&
							(current_status != ITEM_GENERATED))
					{
						/* release lock, rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(new_msg,
							 "Update Item Priority is allowed only on items\n"
						 	 "with the following status values:\n"
							 "NEW, IN-MEDIA, GENERATED\n"
							 "Order: %d, Item: %d does not qualify this operation.\n\n", 
							 itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}


					/*
					** Update item priority 
					*/
					catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;
					catReq->item[1] = (short *)&priority_id;
					if ((status = ims_opCat (catReq, OP_UPDATEITEMPRIORITY)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg, 
										"Internal Error:\nUpdate Item Priority,\n"
										"OP_UPDATEITEMPRIORITY failed for Order: %d, Item: %d\n",
										itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}


					/*
					** Update op_comment if it is not null 
					*/
					if (*op_comment)
					{
						catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;
						catReq->item[1] = (char *)op_comment;
						if ((status = ims_opCat (catReq, OP_UPDATEITEMCOMMENT)) < IMS_OK)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf(new_msg,
											"Internal Error:\nUpdate Item Priority,\n"
 										  "OP_UPDATEITEMCOMMENT failed for Order: %d, Item: %d\n",
											itemPtr->order_id, itemPtr->item_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);
								XtFree (op_comment);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

							itemPtr = itemPtr->next;
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
						sprintf(new_msg, 
										"Internal Error:\nUpdate Item Priority,\n"
									  "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n",
										itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}


					/*
					** update priority and comment of the item in memory 
					*/
					modifyFlag = 1;
					itemPtr->priority = priority_id; 
					if (*op_comment)
						strcpy (itemPtr->op_comment, op_comment);


				} /* if (selectFlag) */

				itemPtr = itemPtr->next;
				count++;

			}  /* while */


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
				** get new order priority 
				*/
				if (priority_id != orderPtr->priority)
				{
					if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
					{
						/* Display error messages */
						sprintf(new_msg, "Internal Error:\nGet New Order Priority,\n"
												 "OP_BEGINTRANSACTION failed for Order ID: %d.\n", 
												 orderPtr->order_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

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
							sprintf(new_msg, "Internal Error:\nGet New Order Priority,\n" 
													 "OP_GETORDERLOCK failed for Order ID: %d\n", 
													 orderPtr->order_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);
								XtFree (op_comment);
	
								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

						}
						else
						{
							catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
							catReq->item[1] = (short *)&priority_id;
							if ((status = ims_opCat (catReq, OP_GETNEWORDERPRIORITY)) < IMS_OK)
							{
								/* rollback transaction */
								ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
								/* Display error messages */
								sprintf(new_msg, "Internal Error:\nGet New Order Priority,\n"
											 "OP_GETNEWORDERPRIORITY failed for Order ID: %d.\n",
											 orderPtr->order_id);   

								if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
								{
									/* memory allocation failed, clean up, return */
									XtFree(value);
									XtFree (op_comment);

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
								sprintf(new_msg, "Internal Error:\nGet New Order Priority,\n"
											 "OP_COMMITTRANSACTION failed for Order ID: %d.\n",
											 orderPtr->order_id);   

								if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
								{
									/* memory allocation failed, clean up, return */
									XtFree(value);
									XtFree (op_comment);

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

				} /* if priority_id != */

			} /* if modifyFlag */


			/* Display all messages in browse dialog box */
			if (concat_msg.Msg != NULL)
			{
				if ((len = strlen(concat_msg.Msg)) > 1024)
				{
					strcpy (label, "Update  Item  Priority  Message  Box");
					browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
				}
				else
				{
					msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
				}
			}

			/* free up concat_msg space allocated */
			OP_FREECHARPTR(concat_msg.Msg);

		}

		XtFree (value);	
		XtFree (op_comment);

		/* change cursor back to normal */
		timeOutCursors (False);
	}
	UxSelectionBoxDlgContext = UxSaveCtx;

}
	

	
/*===========================================================================*
** 
** Function Name: selectionDlg_orderStatus_okCb
**
** Description:		Callback function for the OK button in order status
**								selection dialog.  Update order status in the database,
**								the status for each item of that order is updated to
**								the valid selected (CANCEL or HOLD).  Note that 
**								trigger order_item_status will update the order status
**								automatically when all the items of that order have 
**								the new status.  
**	
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision:      05-08-96 When canceling an order, if there is any item
**                         with status IN-PROCESS and item step name is
**                         PPF PROCESS, and if the process_status is not
**                         PENDING or READY, cancel is not allowed.
**
**                07-15-96 Correct PR 990 - order status CANCEL and 
**                         CANCELLED no longer exist.
**==========================================================================*/

void selectionDlg_orderStatus_okCb(
			Widget wgt, 
			OP_ORDER_LIST *orderPtr,
			XtPointer cb)
{
	_UxCselectionBoxDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;

	char *value;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_ITEM_LIST *itemPtr;
	int i, status, modifyFlag;
	int errorFlag, len = 0;
	char *op_comment;
	Widget selection_dlg_w;
	DBSMALLINT status_id, current_status;
	DBSMALLINT new_item_status, process_status;
	DBCHAR step_name[IMS_COL30_LEN+1];
	char account_id[IMS_COL15_LEN+1];
	char query[IMS_COL255_LEN+1];
	float item_cost;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;
	char Msg[IMS_COL80_LEN+1];


	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
 {

	/*
	** Get valid selected 
	*/
	selection_dlg_w = wgt;
	XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
	op_comment = XmTextGetString (selection_commentST);
	ims_truncStr(op_comment);
	XtDestroyWidget(wgt);	

	if ((len = strlen(value)) <= 0)
	{
		/* no valid was selected, return */
		XtFree (value);
		XtFree (op_comment);

		return;
	}
	else
	{
		/* Change cursor to timeout cursor */
		timeOutCursors (True);

		if (strcmp (value, "CANCEL") == 0)
		{
			status_id = ORDER_CANCEL;
		}
		else
		{
			/*
			** Find the matching item_id for the status string 
			*/
			i = 0;
			while ((strcmp (value, glbData.order_status[i].item_name) != 0) &&
						 (i < glbData.order_status_count)) i++;

			/*
			** 7/15/96 - check to see if we find the status or not
			*/
			if (i < glbData.order_status_count)
			{
				status_id = glbData.order_status[i].item_id;
			}
			else
			{
				/* display error message, return */
				timeOutCursors (False);
				XtFree (value);
				XtFree (op_comment);

				sprintf (Msg, "Internal Error: internal status id not found "
											"for status %s ", value);   
				msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
				return;
			}
		}

		/* 
		** order status id CANCEL = item status id CANCEL.
		** order status id HOLD = item status id HOLD.
		** order status id COMPLETE != item status id COMPLETE
		*/
		if (status_id == ORDER_COMPLETE)
			new_item_status = ITEM_COMPLETE;
		else
			if (status_id == ORDER_CANCEL)
				new_item_status = ITEM_CANCEL;
			else 
				if (status_id == ORDER_HOLD)
					new_item_status = ITEM_HOLD;


		/*
		** assign client to orderClientData from glbData structure 
		*/
		clientData = &(glbData.orderClientData);
		catReq = &(clientData->catReq);
		itemPtr = orderPtr->itemList;
		modifyFlag = 0;


		/*
		** Go through the order item list and update each item status
		** to be the status valid selected.  Trigger order_item_update
		** will update the status of the order when all items' statuses
		** are changed.
		*/

		while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
		{
			/*
			** Begin the update transaction 
			*/
			if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
			{
				/* Display error messages */
				sprintf(new_msg,
						 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
						 itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

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
							  "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
								itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

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
							 "OP_GETITEMSTATUS failed for Order: %d, Item: %d\n", 
							 itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

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
			** is the same as the status to be updated to.
			*/
			if (current_status == new_item_status)
			{
				/* release lock, rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				itemPtr = itemPtr->next;
				continue;
			}

			/*
			** 12/12/95 - trigger order_item_update takes care of item 
			** status verification.  If the item has status COMPLETE or
			** CANCELLED, update the status of that item should fail.
			** Let's check the status here, if the item has status 
			** COMPLETE or CANCELLED, bypass this item and continue
			** to process the next one.
			*/

			if ((current_status == ITEM_COMPLETE) ||
					(current_status == ITEM_CANCEL) ||
					(current_status == ITEM_CANCELLED))
			{
				/* release lock, rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				if (current_status == ITEM_COMPLETE)
				{
					sprintf(new_msg, 
									"Order: %d, Item: %d has status COMPLETE, "
									"no status update on this item is allowed.\n",
									itemPtr->order_id, itemPtr->item_id);   
				}
				else
					if (current_status == ITEM_CANCELLED)
					{
						sprintf(new_msg, 
									"Order: %d, Item: %d has status CANCELLED, "
									"no status update on this item is allowed.\n",
									itemPtr->order_id, itemPtr->item_id);   
					}
					else
						if (current_status == ITEM_CANCEL)
						{
							sprintf(new_msg, 
									"Order: %d, Item: %d has status CANCEL, "
									"no status update on this item is allowed.\n",
									itemPtr->order_id, itemPtr->item_id);   
						}


				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}


			/*
			** Operator wants to update the order status to COMPLETE.
			** if the status_id is ORDER_COMPLETE, an item status can
			** be updated to COMPLETE only when the current item status
			** is GENERATED.
			*/
			if ((new_item_status == ITEM_COMPLETE) && 
					(current_status != ITEM_GENERATED))
			{
				/* release lock, rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

				/* Display error messages */
				sprintf(new_msg,
								"Order: %d, Item: %d does not have status GENERATED,\n"
								"cannot update the status of this item to COMPLETE.\n",
								itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}


			/*
			** 05-08-96 If the item is to be cancelled, the following check
			** is needed: If the current item status is IN-PROCESS, and the
			** step name is PPF PROCESS and the item's process_status is
      ** not PENDING or READY, then canceling the item is not allowed.
			*/
			if ((new_item_status == ITEM_CANCEL) &&
					(current_status  == ITEM_IN_PROCESS))
			{
				/* let's verify the item's step_name, and process_status */
				catReq->item[0] = (DBINT *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&process_status;
				catReq->item[3] = (DBCHAR *)step_name;
				if ((status = ims_opCat (catReq, OP_GETSTEPNAMEPPSSTATUS)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg, 
						"OP_GETSTEPNAMEPPSSTATUS failed for Order: %d, Item: %d\n",
						itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}

				if ((strcmp (step_name, "PPF PROCESS") != 0) ||
						(process_status != ITEM_PPS_PENDING && 
						 process_status != ITEM_PPS_READY))
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg,
						"Cannot cancel an item with status IN-PROCESS and\n"
						"process status other than PENDING or READY.\n"
						"Canceling Order: %d, Item: %d failed.\n",
						itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}

			}


			/*
			** Update item status
			*/
			catReq->item[0] = (DBINT *)&itemPtr->order_id;
			catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
			catReq->item[2] = (DBSMALLINT *)&new_item_status;
			if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
			{
				/* rollback transaction */
				ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
				/* Display error messages */
				sprintf(new_msg, 
								"OP_UPDATEITEMSTATUS failed for Order: %d, Item: %d\n",
								itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

				itemPtr = itemPtr->next;
				continue;
			}

		 
			/*****************************************************************
			** NOTE: 12/12/1995 - Accounting Adjustment is done by the Server
			** if canceled, call accounting transaction rollback function
			** to adjust the cost for that order
			** ims_acctTran() transaction type 5 - DEBIT-ROLLBACK
			** The following parameters are passed: qDesc, msgDesc, 
			** account_id, order_id, cost of item, transaction type.
			*****************************************************************/

			/*****************************************************************
			** NOTE: 1/4/96 - if item status is updated to COMPLETE, 
			** call accounting transaction function to adjust the user account.
			** ims_acctTran() transaction type 4 - DEBIT-END, this will
			** update the hold balance of the user's account.
			** The following parameters are passed: qDesc, msgDesc, 
			** account_id, order_id, cost of item, transaction type.
			*****************************************************************/

			/****************************************************************
			** NOTE: 2/21/96 - update cost_debited_p in order_item table to
			** to 'Y' after the item cost is debited by the above accounting
			** transaction function.
			*****************************************************************/

			if (new_item_status == ITEM_COMPLETE)
			{
				/* Get the account_id and cost for this item. */
				sprintf (query, "select t1.account_id, t2.cost from "
												"order_queue t1, order_item t2 "
												"where t1.order_id = %d and t2.item_id = %d and "
												"t1.order_id = t2.order_id",
												itemPtr->order_id, itemPtr->item_id);

				catReq->item[0] = (char *)query;
				catReq->item[1] = (char *)account_id;
				catReq->item[2] = (float *)&item_cost;
				if ((status = ims_opCat (catReq, OP_GETITEMACCOUNTCOST)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf (new_msg,
								"OP_GETITEMACCOUNTCOST failed for Order: %d, Item: %d\n",
								itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;

				}


				/***************************************************************
				** 4/2/96 - call ims_acctTran function only when the item cost
				** is greater than zero.
				***************************************************************/

				if (item_cost > 0)
				{
					if ((status = ims_acctTran(catReq->qDesc, catReq->msgDesc, 
																	account_id, itemPtr->order_id,
																	item_cost, DEBIT_END)) 
																	< IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf (new_msg, "ims_acctTran() failed. "
										"Could not adjust account %s for Order: %d Item: %d,\n"
									 	"item status cannot be updated to COMPELTE.\n"
										"Please contact the DBA.\n",
									 	account_id, itemPtr->order_id, itemPtr->item_id);

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}


					catReq->item[0] = (DBINT *)&itemPtr->order_id;
					catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
					if ((status = ims_opCat (catReq, OP_UPDATECOSTDEBITFLAG)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf (new_msg, "OP_UPDATECOSTDEBITFLAG() failed. "
									"Could not update cost_debited_p for Order: %d Item: %d,\n"
								 	"item status cannot be updated to COMPLETE.\n"
									"Please contact the DBA.\n",
								 	itemPtr->order_id, itemPtr->item_id);

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}

				} /* if item_cost > 0 */

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
					  "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n",
						itemPtr->order_id, itemPtr->item_id);   

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);
					XtFree (op_comment);

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
			itemPtr->status = new_item_status; 

			/*
			** 2/21/96 - update the cost_debited_p in memory
			*/
			if (new_item_status == ITEM_COMPLETE)
			{
				itemPtr->cost_debited_p = 'Y';
			}

			/* set modifyFlag */
			modifyFlag = 1;

			itemPtr = itemPtr->next;

		}  /* while */


		if (modifyFlag)
		{
			errorFlag = 0; 

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
					XtFree(value);
					XtFree (op_comment);

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
					XtFree(value);
					XtFree (op_comment);
		
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
						XtFree(value);
						XtFree (op_comment);
	
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
						sprintf(new_msg, "Internal Error: Update Item Status, "
										"OP_GETORDERITEMSTATS failed for Order ID: %d.\n",
									  orderPtr->order_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);
	
							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

					}
					else
					if (*op_comment)
					{
						/*
						** Update op_comment if it is not null 
						*/
						catReq->item[0] = (OP_ORDER_LIST *)orderPtr;
						catReq->item[1] = (char *)op_comment;
						if ((status = ims_opCat (catReq, OP_UPDATEORDERCOMMENT)) < IMS_OK)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							errorFlag = 1;
	
							/* Display error messages */
							sprintf(new_msg,
 										  "OP_UPDATEORDERCOMMENT failed for Order: %d\n",
											orderPtr->order_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);
								XtFree (op_comment);
		
								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

						}
					}

					if (!errorFlag)
					{
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
								XtFree(value);
								XtFree (op_comment);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

						}
						else
						{
							/*
							** update status, and comment of the order
							*/
							if (*op_comment)
								strcpy (orderPtr->op_comment, op_comment);

							/*
							** refresh order lists 
							*/
							scroll_cbs = (XmScrollBarCallbackStruct *)
										malloc(sizeof(XmScrollBarCallbackStruct));
							scroll_cbs->value = clientData->orderWindowTop;
							order_scroll_orderListsCb (glbData.orderW, NULL, scroll_cbs);  
							free (scroll_cbs);

							/*
							** Refresh item lists 
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
		} /* if modifyFlag */


		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 1024)
			{
				strcpy (label, "Update  Order  Status  Message  Box");
				browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
			}
			else
			{
				msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
			}
		}

		XtFree (value);	
		XtFree (op_comment);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/* change cursor back to normal */
		timeOutCursors (False);

	}
			
 }
 UxSelectionBoxDlgContext = UxSaveCtx;

}


/*===========================================================================*
** 
** Function Name: selectionDlg_itemStatus_okCb
**
** Description:		Callback function for the OK button in item Status
**								selection dialog.  Update item status in the database,
**								destroys the selection dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision:      05-08-96 When canceling an item, if the item has 
**                         status IN-PROCESS and item step name is
**                         PPF PROCESS, and if the process_status is 
**                         not PENDING or READY, cancel is not allowed.
**
**                07-15-96 Modified to verify that internal status id
**                         is found to match the status value selected.
**
**==========================================================================*/

void selectionDlg_itemStatus_okCb(
			Widget wgt, 
			OP_ORDER_ITEM_LIST *itemPtr,
			XtPointer cb)
{
	_UxCselectionBoxDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;

	char *value;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_ORDER_LIST *orderPtr;
	DBSMALLINT status_id, current_status;
	DBSMALLINT process_status;
	DBCHAR step_name[IMS_COL30_LEN+1];
	int i, status, modifyFlag;
	int len = 0;
	char *op_comment;
	Widget selection_dlg_w;
	char account_id[IMS_COL15_LEN+1];
	char query[IMS_COL255_LEN+1];
	float item_cost;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL512_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;
	char Msg[IMS_COL80_LEN+1];


	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
 {

	/*
	** Get valid selected 
	*/
	selection_dlg_w = wgt;
	XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
	op_comment = XmTextGetString (selection_commentST);
	ims_truncStr(op_comment);
	XtDestroyWidget(wgt);	

	if ((len = strlen(value)) <= 0)
	{
		/* no valid was selected, return */
		XtFree (value);
		XtFree (op_comment);

		return;
	}
	else
	{
		/* change cursor to timeout cursor */
		timeOutCursors (True);

		/*
		** Find the matching item_id for the status string 
		*/
		i = 0;
		while ((strcmp (value, glbData.item_status[i].item_name) != 0) &&
					 (i < glbData.item_status_count)) i++;

		status_id = glbData.item_status[i].item_id;

		/*
		** 7/15/96 - check to see if we find the status or not
		*/
		if (i < glbData.item_status_count)
		{
			status_id = glbData.item_status[i].item_id;
		}
		else
		{
			/* display error message, return */
			timeOutCursors (False);
			XtFree (value);
			XtFree (op_comment);

			sprintf (Msg, "Internal Error: internal status id not found "
										"for status %s ", value);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}

		/*
		** assign client to orderClientData from glbData structure 
		*/
		clientData = &(glbData.orderClientData);
		catReq = &(clientData->catReq);
		orderPtr = clientData->currOrder;
		modifyFlag = 0;

		while (itemPtr != (OP_ORDER_ITEM_LIST *)NULL)
		{
			/*
			** Process items that are selected
			*/
			if (itemPtr->selectFlag)
			{
				/*
				** Begin the update transaction 
				*/
				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					/* Display error messages */
					sprintf(new_msg,
							 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
							 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

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
								  "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
									itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

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
								 "OP_GETITEMSTATUS failed for Order: %d, Item: %d\n", 
								 itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

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
				** is the same as the status to be updated to.
				*/
				if (current_status == status_id)
				{
					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					itemPtr = itemPtr->next;
					continue;
				}

				/*
				** 12/12/95 - trigger order_item_update takes care of item 
				** status verification.  If the item has status COMPLETE or
				** CANCELLED, update the status of that item should fail.
				** Let's check the status here, if the item has status 
				** COMPLETE or CANCELLED, bypass this item and continue
				** to process the next one.
				*/

				if ((current_status == ITEM_COMPLETE) ||
						(current_status == ITEM_CANCEL) ||
						(current_status == ITEM_CANCELLED))
				{
					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					if (current_status == ITEM_COMPLETE)
					{
						sprintf(new_msg, 
										"Order: %d, Item: %d has status COMPLETE, "
										"no status update on this item is allowed.\n",
										itemPtr->order_id, itemPtr->item_id);   
					}
					else
						if (current_status == ITEM_CANCELLED)
						{
							sprintf(new_msg, 
										"Order: %d, Item: %d has status CANCELLED, "
										"no status update on this item is allowed.\n",
										itemPtr->order_id, itemPtr->item_id);   
						}
						else
							if (current_status == ITEM_CANCEL)
							{
								sprintf(new_msg, 
										"Order: %d, Item: %d has status CANCEL, "
										"no status update on this item is allowed.\n",
										itemPtr->order_id, itemPtr->item_id);   
							}

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** Operator wants to update the item status to COMPLETE.
				** if the status_id is ITEM_COMPLETE, an item status can
				** be updated to COMPLETE only when the current item status
				** is GENERATED.
				*/
				if ((status_id == ITEM_COMPLETE) && 
						(current_status != ITEM_GENERATED))
				{
					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(new_msg,
								"Order: %d, Item: %d does not have status GENERATED,\n"
								"cannot update the status of this item to COMPLETE.\n",
								itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}


				/*
				** 05-08-96 If the item is to be cancelled, the following check
				** is needed: If the current item status is IN-PROCESS, and the
				** step name is PPF PROCESS and the item's process_status is
 		    ** not PENDING or READY, then canceling the item is not allowed.
				*/
				if ((status_id == ITEM_CANCEL) &&
						(current_status  == ITEM_IN_PROCESS))
				{
					/* let's verify the item's step_name, and process_status */
					catReq->item[0] = (DBINT *)&itemPtr->order_id;
					catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
					catReq->item[2] = (DBSMALLINT *)&process_status;
					catReq->item[3] = (DBCHAR *)step_name;
					if ((status = ims_opCat (catReq, OP_GETSTEPNAMEPPSSTATUS)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg, 
							"OP_GETSTEPNAMEPPSSTATUS failed for Order: %d, Item: %d\n",
							itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}

					if ((strcmp (step_name, "PPF PROCESS") != 0) ||
							(process_status != ITEM_PPS_PENDING && 
							 process_status != ITEM_PPS_READY))
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg,
							"Cannot cancel an item with status IN-PROCESS and\n"
							"process status other than PENDING or READY.\n"
							"Canceling Order: %d, Item: %d failed.\n",
							itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;
					}

				}

				/*
				** Update item status
				*/
				catReq->item[0] = (DBINT *)&itemPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&status_id;
				if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
				{
					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg, 
									"OP_UPDATEITEMSTATUS failed for Order: %d, Item: %d\n",
									itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;
				}

		 
				/*****************************************************************
				** NOTE: 12/12/1995 - Accounting Adjustment is done by the Server
				** if canceled, call accounting transaction rollback function
				** to adjust the cost for that order
				** ims_acctTran() transaction type 5 - DEBIT-ROLLBACK
				** The following parameters are passed: qDesc, msgDesc, 
				** account_id, order_id, cost of item, transaction type.
				*****************************************************************/

				/*****************************************************************
				** NOTE: 1/4/96 - if item status is updated to COMPLETE, 
				** call accounting transaction function to adjust the user account.
				** ims_acctTran() transaction type 4 - DEBIT-END, this will
				** update the hold balance of the user's account.
				** The following parameters are passed: qDesc, msgDesc, 
				** account_id, order_id, cost of item, transaction type.
				*****************************************************************/

				/****************************************************************
				** NOTE: 2/21/96 - update cost_debited_p in order_item table to
				** to 'Y' after the item cost is debited by the above accounting
				** transaction function.
				*****************************************************************/

				if (status_id == ITEM_COMPLETE)
				{
					/* Get the account_id and cost for this item. */
					sprintf (query, "select t1.account_id, t2.cost from "
													"order_queue t1, order_item t2 "
													"where t1.order_id = %d and t2.item_id = %d and "
													"t1.order_id = t2.order_id",
													itemPtr->order_id, itemPtr->item_id);

					catReq->item[0] = (char *)query;
					catReq->item[1] = (char *)account_id;
					catReq->item[2] = (float *)&item_cost;
					if ((status = ims_opCat (catReq, OP_GETITEMACCOUNTCOST)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf (new_msg,
									"OP_GETITEMACCOUNTCOST failed for Order: %d, Item: %d\n",
									itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
						continue;

					}

					/***************************************************************
					** 4/2/96 - call ims_acctTran function only when the item cost
					** is greater than zero.
					***************************************************************/

					if (item_cost > 0)
					{

						if ((status = ims_acctTran(catReq->qDesc, catReq->msgDesc, 
																		account_id, itemPtr->order_id,
																		item_cost, DEBIT_END)) 
																		< IMS_OK)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf (new_msg, "ims_acctTran() failed. "
										"Could not adjust account %s for Order: %d Item: %d,\n"
									 	"item status cannot be updated to COMPLETE.\n"
										"Please contact the DBA.\n",
									 	account_id, itemPtr->order_id, itemPtr->item_id);

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);
								XtFree (op_comment);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

							itemPtr = itemPtr->next;
							continue;
						}


						catReq->item[0] = (DBINT *)&itemPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&itemPtr->item_id;
						if ((status = ims_opCat (catReq, OP_UPDATECOSTDEBITFLAG)) < IMS_OK)
						{
							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf (new_msg, "OP_UPDATECOSTDEBITFLAG() failed. "
										"Could not update cost_debited_p for Order: %d Item: %d,\n"
									 	"item status cannot be updated to COMPLETE.\n"
										"Please contact the DBA.\n",
									 	itemPtr->order_id, itemPtr->item_id);

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);
								XtFree (op_comment);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

							itemPtr = itemPtr->next;
							continue;
						}

					} /* if item_cost > 0 */
				}


				/*
				** Update op_comment if it is not null 
				*/
				if (*op_comment)
				{
					catReq->item[0] = (OP_ORDER_ITEM_LIST *)itemPtr;
					catReq->item[1] = (char *)op_comment;
					if ((status = ims_opCat (catReq, OP_UPDATEITEMCOMMENT)) < IMS_OK)
					{
						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg,
 								  "OP_UPDATEITEMCOMMENT failed for Order: %d, Item: %d\n",
									itemPtr->order_id, itemPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						itemPtr = itemPtr->next;
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
					sprintf(new_msg, 
							  "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n",
								itemPtr->order_id, itemPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);
						XtFree (op_comment);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					itemPtr = itemPtr->next;
					continue;

				}
		 

				/*
				** update status and comment of the item in memory 
				*/
				itemPtr->status = status_id; 

				if (*op_comment)
					strcpy (itemPtr->op_comment, op_comment);

				/*
				** 2/21/96 - update the cost_debited_p in memory
				*/
				if (status_id == ITEM_COMPLETE)
				{
					itemPtr->cost_debited_p = 'Y';
				}

				/* set modifyFlag */
				modifyFlag = 1;

			} /* if (selectFlag) */

			itemPtr = itemPtr->next;

		}  /* while */


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
					XtFree(value);
					XtFree (op_comment);

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
					XtFree(value);
					XtFree (op_comment);
		
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
						XtFree(value);
						XtFree (op_comment);
	
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
						sprintf(new_msg, "Internal Error: Update Item Status, "
												 "OP_GETORDERITEMSTATS failed for Order ID: %d.\n",
												 orderPtr->order_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);
							XtFree (op_comment);
	
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
							XtFree(value);
							XtFree (op_comment);
	
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
				strcpy (label, "Update  Order  Item  Status Message  Box");
				browseDlg_popupCb (glbData.orderW, concat_msg.Msg, label);
			}
			else
			{
				msgBoxDlg_popupCb (glbData.orderW, IMS_ERROR, concat_msg.Msg);
			}
		}

		XtFree (value);	
		XtFree (op_comment);

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/* change cursor back to normal */
		timeOutCursors (False);
	}
			
 }
	UxSelectionBoxDlgContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: selectionDlg_photo_okCb
**
** Description:		Callback function for the OK button in photo type
**								selection dialog. Paste photo type selected to textfield,
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

void selectionDlg_photo_okCb(
			Widget wgt, 
			Widget textFieldW,
			XtPointer cb)
{
	_UxCselectionBoxDlg           *UxSaveCtx, *UxContext;
	_UxCphotoOrder          			*UxPhotoOrderContext;
	Widget                  			UxWidget = wgt;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;
	char *value;


	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
	{
		/*
		** Get valid selected 
		*/
		XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
		XtDestroyWidget(wgt);	

		XmTextFieldSetString (textFieldW, value);

		XtFree (value);	
	}
	UxSelectionBoxDlgContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: selectionDlg_fireStatus_okCb
**
** Description:		Callback function for the OK button in Fire Status
**								selection dialog.  Update item status in the fire_queue
**								table, destroys the selection dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History: 07/15/96 Verify that internal status id found for 
**                            selected status value.
**
**==========================================================================*/

void selectionDlg_fireStatus_okCb(
			Widget wgt, 
			OP_FIRE_QUEUE_LIST *fireQPtr,
			XtPointer cb)
{
	_UxCselectionBoxDlg     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;

	char *value;
	OP_CAT_STRUCT *catReq;
	OP_FILM_CLIENT_DATA *clientData;
	int itemCount;
	DBSMALLINT status_id, current_status;
	DBSMALLINT order_item_status;
	int i, k, status, modifyFlag;
	int invalid_status, len = 0;
	Widget selection_dlg_w;
	char account_id[IMS_COL15_LEN+1];
	char query[IMS_COL255_LEN+1];
	float item_cost;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL255_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;
	char Msg[IMS_COL80_LEN+1];
	int errorCount;

	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
	{

	/*
	** Get valid selected 
	*/
	selection_dlg_w = wgt;
	XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
	XtDestroyWidget(wgt);	

	if ((len = strlen(value)) <= 0)
	{
		/* no valid was selected, return */
		XtFree (value);
		return;
	}
	else
	{
		/* change cursor to timeout cursor */
		timeOutCursors (True);

		/*
		** Find the matching item_id for the status string 
		*/
		i = 0;
		while ((strcmp (value, glbData.fire_status[i].item_name) != 0) &&
					 (i < glbData.fire_status_count)) i++;

		/*
		** 7/15/96 - check to see if we find the status or not
		*/
		if (i < glbData.fire_status_count)
		{
			status_id = glbData.fire_status[i].item_id;
		}
		else
		{
			/* display error message, return */
			timeOutCursors (False);
			XtFree (value);

			sprintf (Msg, "Internal Error: internal status id not found "
										"for status %s ", value);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}

		/*
		** assign client to orderClientData from glbData structure 
		*/
		clientData = &(glbData.filmClientData);
		catReq = &(clientData->catReq);
		itemCount = clientData->fireQueueCount;
		modifyFlag = 0;
		invalid_status = IMS_FALSE;
		errorCount = 0;

		while (fireQPtr != (OP_FIRE_QUEUE_LIST *)NULL)
		{
			/*
			** process selected items
			*/
			if (fireQPtr->selectFlag)
			{

				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					errorCount++;

					/* Display error messages */
					sprintf(new_msg,
							 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					fireQPtr = fireQPtr->next;
					continue;

				}

				if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
				{
					errorCount++;

					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg,
							 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   
	
					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					fireQPtr = fireQPtr->next;
					continue;

				}

				catReq->item[0] = (DBINT *)&fireQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&current_status;
				if ((status = ims_opCat(catReq, OP_GETFIREITEMSTATUS)) < 0)
				{
					errorCount++;

					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(new_msg,
							 "OP_GETFIREITEMSTATUS failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					fireQPtr = fireQPtr->next;
					continue;

				}

				/*
				** continue to the next item if current item status 
				** is the same as the status to be updated to.
				*/
				if (current_status == status_id)
				{
					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					fireQPtr = fireQPtr->next;
					continue;
				}

				/* 
				** Update fire_queue status is allowed only if the item's
				** current status is NEW, GENERATED, FAILED, ERROR.
				** If item status is NEW, it can only be updated to CANCEL. 
				** If item status is GENERATED, it can be updated to NEW,
				** COMPLETE or CANCEL.
				** If item status is FAILED it can only be updated to CANCEL.
				** If item status is ERROR it can be updated to NEW, CANCEL.
				*/
				if (((current_status != FIRE_NEW) && (current_status != FIRE_GENERATED) 
			 	 && (current_status != FIRE_ERROR) && (current_status != FIRE_FAILED)) 
				 || ((current_status == FIRE_NEW) && (status_id != FIRE_CANCEL)) 
				 || ((current_status == FIRE_FAILED) && (status_id != FIRE_CANCEL)) 
				 || ((current_status == FIRE_ERROR) && ((status_id != FIRE_NEW) &&
																								(status_id != FIRE_CANCEL)))) 
						
				{
					errorCount++;

					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					invalid_status = IMS_TRUE;

					/* Display error messages */
					k = 0;
					while ((k < glbData.fire_status_count) && 
							 (current_status != glbData.fire_status[k].item_id)) 
							k++;

					sprintf(new_msg,
				 					"Order: %d, Item: %d current status is %s, " 
									"cannot be updated to %s.\n",
									fireQPtr->order_id, fireQPtr->item_id,
									glbData.fire_status[k].item_name,
				 					value);

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

				}
				else
				{
					catReq->item[0] = (DBINT *)&fireQPtr->order_id;
					catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
					catReq->item[2] = (DBSMALLINT *)&status_id;
					if ((status = ims_opCat(catReq, OP_UPDATEFIREITEMSTATUS)) < 0)
					{
						errorCount++;

						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(new_msg,
							 "OP_UPDATEFIREITEMSTATUS failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						fireQPtr = fireQPtr->next;
						continue;

					}

					/*
					** If status is updated to COMPLETE, then update item status
					** in order_item table to ON-FILM 
					** If status is updated to CANCEL, then update item status
					** in order_item table to CANCEL
					*/
					if (status_id == FIRE_COMPLETE)
					{
						order_item_status = ITEM_ON_FILM;
					}
					if (status_id == FIRE_CANCEL)
					{
						order_item_status = ITEM_CANCEL;
					}

					if ((status_id == FIRE_COMPLETE) || (status_id == FIRE_CANCEL))
					{
						catReq->item[0] = (DBINT *)&fireQPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
						catReq->item[2] = (DBSMALLINT *)&order_item_status;
						if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
						{
							errorCount++;

							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf(new_msg,
								 "OP_UPDATEITEMSTATUS failed for Order: %d, Item: %d\n", 
								 fireQPtr->order_id, fireQPtr->item_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

							fireQPtr = fireQPtr->next;
							continue;
						}

					}

					/***************************************************************
					** NOTE: 12/12/1995 - This part is not necessary in R1B prime:
					** 										Server will adjust accounting. 
					** if canceled, call accounting transaction rollback function
					** to adjust the cost for that order
					** ims_acctTran() transaction type 5 - DEBIT-ROLLBACK
					** The following parameters are passed: qDesc, msgDesc, 
					** account_id, order_id, cost of item, transaction type.
					***************************************************************/

					/***************************************************************
					if (status_id  == FIRE_CANCEL)
					{
						*** Get the account_id and cost for this item. ***
						sprintf (query, "select t1.account_id, t2.cost from "
													"order_queue t1, order_item t2 "
													"where t1.order_id = %d and t2.item_id = %d and "
													"t1.order_id = t2.order_id",
													fireQPtr->order_id, fireQPtr->item_id);

						catReq->item[0] = (char *)query;
						catReq->item[1] = (char *)account_id;
						catReq->item[2] = (float *)&item_cost;
						if ((status = ims_opCat (catReq, OP_GETITEMACCOUNTCOST)) < IMS_OK)
						{
							*** rollback transaction ***
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
							*** Display error messages ***
							strcpy (new_msg, "OP_GETITEMACCOUNTCOST failed, "
										 "could not get item account id and cost.\n");

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								*** memory allocation failed, clean up, return ***
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								*** Change cursor back to normal ***
								timeOutCursors (False);
								return;
							}

							fireQPtr = fireQPtr->next;
							continue;
						}

						if ((status = ims_acctTran(catReq->qDesc, catReq->msgDesc, 
																			account_id, fireQPtr->order_id,
																			item_cost, DEBIT_ROLLBACK)) 
																			< IMS_OK)
						{
							*** rollback transaction ***
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
							*** Display error messages ***
							sprintf (new_msg, "ims_acctTran() failed. "
												"Could not adjust account %s for cancelled "
											 	"Order: %d Item: %d\n",
											 	account_id, fireQPtr->order_id, fireQPtr->item_id);

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								*** memory allocation failed, clean up, return ***
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								*** Change cursor back to normal ***
								timeOutCursors (False);
								return;
							}

							fireQPtr = fireQPtr->next;
							continue;
						}

					}
					**************************************************************/


					/**************************************************************
					** NOTE: 12/12/1995 - Removing items is done by the Server
					**										in R1B prime.
					** If status is updated to COMPLETE or CANCEL,
					** call removeFireQueueItem to remove the item from fire_queue
					**************************************************************/

					/**************************************************************
					if ((status_id == FIRE_COMPLETE) || (status_id == FIRE_CANCEL))
					{
						catReq->item[0] = (DBINT *)&fireQPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&fireQPtr->item_id;
						if ((status = ims_opCat(catReq, OP_REMOVEFIREQUEUEITEM)) < 0)
						{
							*** rollback transaction ***
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
							*** Display error messages ***
							sprintf(new_msg,
									 "OP_REMOVEFIREQUEUEITEM failed for Order: %d, Item: %d\n", 
									 fireQPtr->order_id, fireQPtr->item_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								*** memory allocation failed, clean up, return ***
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								*** Change cursor back to normal ***
								timeOutCursors (False);
								return;
							}
	
							fireQPtr = fireQPtr->next;
							continue;
						}
					}
					***************************************************************/


					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						errorCount++;

						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg, 
							 "OP_UPDATEFIREQUEUESTATUS failed for Order: %d, Item: %d\n", 
							 fireQPtr->order_id, fireQPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						fireQPtr = fireQPtr->next;
						continue;
					}

					/* update item status in fire_queue_list */
					fireQPtr->status = status_id;

					/* set modifyFlag so Fire Recorder Queue will be refreshed */
					modifyFlag = 1;

				} /* if (!invalid_status) */					

			} /* if (selectFlag) */

			fireQPtr = fireQPtr->next;

		}  /* while */

		if (modifyFlag)
		{
			/*
			** Refresh fire queue lists 
			*/
			(void) filmGen_fire_searchCb(glbData.filmW, NULL, NULL);

			if (status_id == FIRE_COMPLETE)
			{
				strcpy (new_msg, "\nFire Recorder Item Status Update:\n"
					"Completed items have been updated with status COMPLETE in the "
					"fire queue table.\n\n"
		 			"Completed items have been updated with status ON-FILM in the "
		 			"order item queue table.\n\n");

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

			}

			if (status_id == FIRE_CANCEL)
			{
				strcpy (new_msg, "\nFire Recorder Item Status Update:\n"
					"Cancelled items have been updated with status CANCEL in the "
					"fire queue table.\n\n"
		 			"Cancelled items have been updated with status CANCEL in the "
		 			"order item queue table.\n\n");

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

			}

		}

		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 1024)
			{
				strcpy (label, "Update  Fire  Item  Status  Message  Box");
				browseDlg_popupCb (glbData.filmW, concat_msg.Msg, label); 
			}
			else
				if (!errorCount)
				{
					msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, concat_msg.Msg); 
				}
				else
				{
					msgBoxDlg_popupCb (glbData.filmW, IMS_ERROR, concat_msg.Msg); 
				}

		}

		XtFree (value);	

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/* change cursor back to normal */
		timeOutCursors (False);

	} 

 }
	UxSelectionBoxDlgContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: selectionDlg_laserStatus_okCb
**
** Description:		Callback function for the OK button in Laser Status
**								selection dialog.  Update item status in the laser_queue
**								table, destroys the selection dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History: 07/15/96 Verify that internal status id found for 
**                            selected status value.
**
**==========================================================================*/

void selectionDlg_laserStatus_okCb(
			Widget wgt, 
			OP_LASER_QUEUE_LIST *laserQPtr,
			XtPointer cb)
{
	_UxCselectionBoxDlg     *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs = 
			(XmSelectionBoxCallbackStruct *)cb;

	char *value;
	OP_CAT_STRUCT *catReq;
	OP_FILM_CLIENT_DATA *clientData;
	int itemCount;
	DBSMALLINT status_id, current_status;
	DBSMALLINT order_item_status;
	int i, k, status, modifyFlag;
	int invalid_status, len = 0;
	Widget selection_dlg_w;
	char account_id[IMS_COL15_LEN+1];
	char query[IMS_COL255_LEN+1];
	float item_cost;
	CONCAT_STR concat_msg = {0};
	char label[IMS_COL128_LEN+1];
	char new_msg[IMS_COL255_LEN+1];
	XmScrollBarCallbackStruct *scroll_cbs;
	char Msg[IMS_COL80_LEN+1];
	int errorCount;

	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
			(_UxCselectionBoxDlg *) UxGetContext( UxWidget );
	{

	/*
	** Get valid selected 
	*/
	selection_dlg_w = wgt;
	XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &value);
	XtDestroyWidget(wgt);	

	if ((len = strlen(value)) <= 0)
	{
		/* no valid was selected, return */
		XtFree (value);
		return;
	}
	else
	{
		/* change cursor to timeout cursor */
		timeOutCursors (True);

		/*
		** Find the matching item_id for the status string 
		*/
		i = 0;
		while ((strcmp (value, glbData.laser_status[i].item_name) != 0) &&
					 (i < glbData.laser_status_count)) i++;

		/*
		** 7/15/96 - check to see if we find the status or not
		*/
		if (i < glbData.laser_status_count)
		{
			status_id = glbData.laser_status[i].item_id;
		}
		else
		{
			/* display error message, return */
			timeOutCursors (False);
			XtFree (value);

			sprintf (Msg, "Internal Error: internal status id not found "
										"for status %s ", value);   
			msgBoxDlg_popupCb (glbData.orderW, IMS_FATAL, Msg); 
			return;
		}


		/*
		** assign client to filmClientData from glbData structure 
		*/
		clientData = &(glbData.filmClientData);
		catReq = &(clientData->catReq);
		itemCount = clientData->laserQueueCount;
		modifyFlag = 0;
		invalid_status = IMS_FALSE;
		errorCount = 0;

		while (laserQPtr != (OP_LASER_QUEUE_LIST *)NULL)
		{
			/*
			** process selected items
			*/
			if (laserQPtr->selectFlag)
			{

				if ((status = ims_opCat (catReq, OP_BEGINTRANSACTION)) < IMS_OK)
				{
					errorCount++;

					/* Display error messages */
					sprintf(new_msg, 
									 "OP_BEGINTRANSACTION failed for Order: %d, Item: %d\n", 
									 laserQPtr->order_id, laserQPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					laserQPtr = laserQPtr->next;
					continue;

				}

				if ((status = ims_opCat (catReq, OP_GETITEMLOCK)) < IMS_OK)
				{
					errorCount++;

					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
					/* Display error messages */
					sprintf(new_msg,
									 "OP_GETITEMLOCK failed for Order: %d, Item: %d\n", 
									 laserQPtr->order_id, laserQPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}

					laserQPtr = laserQPtr->next;
					continue;

				}

				catReq->item[0] = (DBINT *)&laserQPtr->order_id;
				catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
				catReq->item[2] = (DBSMALLINT *)&current_status;
				if ((status = ims_opCat(catReq, OP_GETLASERITEMSTATUS)) < 0)
				{
					errorCount++;

					/* rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					/* Display error messages */
					sprintf(new_msg,
									 "OP_GETLASERITEMSTATUS failed for Order: %d, Item: %d\n", 
									 laserQPtr->order_id, laserQPtr->item_id);   

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}


					laserQPtr = laserQPtr->next;
					continue;

				}

				/*
				** continue to the next item if current item status 
				** is the same as the status to be updated to.
				*/
				if (current_status == status_id)
				{
					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					laserQPtr = laserQPtr->next;
					continue;
				}

				/* 
				** Update laser_queue status is allowed only if the item's
				** current status is NEW, GENERATED, FAILED, ERROR.
				** If item status is NEW, it can only be updated to CANCEL. 
				** If item status is GENERATED, it can be updated to NEW,
				** COMPLETE or CANCEL.
				** If item status is FAILED it can only be updated to CANCEL.
				** If item status is ERROR it can be updated to NEW, CANCEL.
				*/
				if (((current_status != LASER_NEW) && 
						 (current_status != LASER_GENERATED) &&
			 	 (current_status != LASER_ERROR) && (current_status != LASER_FAILED)) ||
				 ((current_status == LASER_NEW) && (status_id != LASER_CANCEL)) ||
				 ((current_status == LASER_FAILED) && (status_id != LASER_CANCEL)) ||
				 ((current_status == LASER_ERROR) && ((status_id != LASER_NEW) &&
																							(status_id != LASER_CANCEL)))) 
				{
					errorCount++;

					/* release lock, rollback transaction */
					ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

					invalid_status = IMS_TRUE;

					/* Display error messages */
					k = 0;
					while ((k < glbData.laser_status_count) && 
							 (current_status != glbData.laser_status[k].item_id)) 
							k++;

					sprintf(new_msg,
				 					"Order: %d, Item: %d current status is %s, " 
									"cannot be updated to %s.\n",
									laserQPtr->order_id, laserQPtr->item_id,
									glbData.laser_status[k].item_name,
				 					value);

					if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
					{
						/* memory allocation failed, clean up, return */
						XtFree(value);

						OP_FREECHARPTR(concat_msg.Msg);

						/* Change cursor back to normal */
						timeOutCursors (False);
						return;
					}


				}
				else
				{
					catReq->item[0] = (DBINT *)&laserQPtr->order_id;
					catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
					catReq->item[2] = (DBSMALLINT *)&status_id;
					if ((status = ims_opCat(catReq, OP_UPDATELASERITEMSTATUS)) < 0)
					{
						errorCount++;

						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

						/* Display error messages */
						sprintf(new_msg,
							 "OP_UPDATELASERITEMSTATUS failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}

						laserQPtr = laserQPtr->next;
						continue;

					}


					/*
					** If status is updated to COMPLETE, then update item status
					** in order_item table to ON-FILM 
					** If status is updated to CANCEL, then update item status
					** in order_item table to CANCEL
					*/
					if (status_id == LASER_COMPLETE)
					{
						order_item_status = ITEM_ON_FILM;
					}

					if (status_id == LASER_CANCEL)
					{
						order_item_status = ITEM_CANCEL;
					}
					

					if ((status_id == LASER_COMPLETE) || (status_id == LASER_CANCEL))
					{
						catReq->item[0] = (DBINT *)&laserQPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
						catReq->item[2] = (DBSMALLINT *)&order_item_status;
						if ((status = ims_opCat (catReq, OP_UPDATEITEMSTATUS)) < IMS_OK)
						{
							errorCount++;

							/* rollback transaction */
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							/* Display error messages */
							sprintf (new_msg, 
									 "OP_UPDATEITEMSTATUS failed, could not update status "
									 "for Order: %d, Item: %d in order_item table.\n", 
									 laserQPtr->order_id, laserQPtr->item_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								/* memory allocation failed, clean up, return */
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								/* Change cursor back to normal */
								timeOutCursors (False);
								return;
							}

							laserQPtr = laserQPtr->next;
							continue;

						}

					}


					/***************************************************************
					** NOTE: 12/12/1995 - This part is not necessary in R1B prime:
					** 										Server will adjust accounting. 
					** if canceled, call accounting transaction rollback function
					** to adjust the cost for that order
					** ims_acctTran() transaction type 5 - DEBIT-ROLLBACK
					** The following parameters are passed: qDesc, msgDesc, 
					** account_id, order_id, cost of item, transaction type.
					***************************************************************/

					/***************************************************************
					if (status_id  == LASER_CANCEL)
					{
						*** Get the account_id and cost for this item. ***
						sprintf (query, "select t1.account_id, t2.cost from "
													"order_queue t1, order_item t2 "
													"where t1.order_id = %d and t2.item_id = %d and "
													"t1.order_id = t2.order_id",
													laserQPtr->order_id, laserQPtr->item_id);

						catReq->item[0] = (char *)query;
						catReq->item[1] = (char *)account_id;
						catReq->item[2] = (float *)&item_cost;
						if ((status = ims_opCat (catReq, OP_GETITEMACCOUNTCOST)) < IMS_OK)
						{
							*** rollback transaction ***
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							*** Display error messages ***
							sprintf (new_msg, "OP_GETITEMACCOUNTCOST failed, could not "
								 "get account_id and cost for "
								 "Order: %d, Item: %d\n", 
								 laserQPtr->order_id, laserQPtr->item_id);   


							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								*** memory allocation failed, clean up, return ***
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								*** Change cursor back to normal ***
								timeOutCursors (False);
								return;
							}

							laserQPtr = laserQPtr->next;
							continue;
						}


						if ((status = ims_acctTran(catReq->qDesc, catReq->msgDesc, 
																			account_id, laserQPtr->order_id,
																			item_cost, DEBIT_ROLLBACK)) 
																			< IMS_OK)
						{
							*** rollback transaction ***
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);

							*** Display error messages ***
							sprintf (new_msg, 
										"ims_acctTran() failed, could not cancel "
										"Order: %d Item: %d\n",
										laserQPtr->order_id, laserQPtr->item_id);

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								*** memory allocation failed, clean up, return ***
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								*** Change cursor back to normal ***
								timeOutCursors (False);
								return;
							}

							laserQPtr = laserQPtr->next;
							continue;
						}

					}
					**************************************************************/


					/****************************************************************
					** NOTE: 12/12/1995 - Removing items is done by the Server
					**										in R1B prime.
					** If status is updated to COMPLETE or CANCEL,
					** call removeLaserQueueItem to remove the item from laser_queue
					****************************************************************/

					/**************************************************************
					if ((status_id == LASER_COMPLETE) || (status_id == LASER_CANCEL))
					{
						catReq->item[0] = (DBINT *)&laserQPtr->order_id;
						catReq->item[1] = (DBSMALLINT *)&laserQPtr->item_id;
						if ((status = ims_opCat(catReq, OP_REMOVELASERQUEUEITEM)) < 0)
						{
							*** rollback transaction ***
							ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
		
							*** Display error messages ***
							sprintf(new_msg,
									 "OP_REMOVELASERQUEUEITEM failed, could not remove "
									 "Order: %d, Item: %d from table laser_queue.\n", 
									 laserQPtr->order_id, laserQPtr->item_id);   

							if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
							{
								*** memory allocation failed, clean up, return ***
								XtFree(value);

								OP_FREECHARPTR(concat_msg.Msg);

								*** Change cursor back to normal ***
								timeOutCursors (False);
								return;
							}

							laserQPtr = laserQPtr->next;
							continue;
						}

					}
					****************************************************************/


					if ((status = ims_opCat (catReq, OP_COMMITTRANSACTION)) < IMS_OK)
					{
						errorCount++;

						/* rollback transaction */
						ims_opCat (catReq, OP_ROLLBACKTRANSACTION);
	
						/* Display error messages */
						sprintf(new_msg,
							 "OP_COMMITTRANSACTION failed for Order: %d, Item: %d\n", 
							 laserQPtr->order_id, laserQPtr->item_id);   

						if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
						{
							/* memory allocation failed, clean up, return */
							XtFree(value);

							OP_FREECHARPTR(concat_msg.Msg);

							/* Change cursor back to normal */
							timeOutCursors (False);
							return;
						}


						laserQPtr = laserQPtr->next;
						continue;

					}

					/* update item status in laser_queue_list */
					laserQPtr->status = status_id;

					/* set modifyFlag so Laser Tech Queue will be refreshed. */
					modifyFlag = 1;

				} /* if (!invalid_status) */					 

			} /* if (selectFlag) */

			laserQPtr = laserQPtr->next;

		}  /* while */


		if (modifyFlag)
		{

			/*
			** Refresh laser queue lists 
			*/
			(void) filmGen_laser_searchCb(glbData.filmW, NULL, NULL);

			if (status_id == LASER_COMPLETE)
			{
				strcpy (new_msg, "\nLaser Tech Item Status Update:\n"
					"Completed items have been updated with status COMPLETE in the "
					"laser queue table.\n\n"
		 			"Completed items have been updated with status ON-FILM in the "
		 			"order item queue table.\n\n");

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

			}

			if (status_id == LASER_CANCEL)
			{
				strcpy (new_msg, "\nLaser Tech Item Status Update:\n"
					"Cancelled items have been updated with status CANCEL in the "
					"laser queue table.\n\n"
		 			"Cancelled items have been updated with status CANCEL in the "
		 			"order item queue table.\n\n");

				if ((status = concatString(&concat_msg, new_msg)) < IMS_OK)
				{
					/* memory allocation failed, clean up, return */
					XtFree(value);

					OP_FREECHARPTR(concat_msg.Msg);

					/* Change cursor back to normal */
					timeOutCursors (False);
					return;
				}

			}

		}

		/* Display all messages in browse dialog box */
		if (concat_msg.Msg != NULL)
		{
			if ((len = strlen(concat_msg.Msg)) > 1024)
			{
				strcpy (label, "Update  Laser  Item  Status  Message  Box");
				browseDlg_popupCb (glbData.filmW, concat_msg.Msg, label); 
			}
			else
				if (!errorCount)
				{
					msgBoxDlg_popupCb (glbData.filmW, IMS_INFO, concat_msg.Msg); 
				}
				else
				{
					msgBoxDlg_popupCb (glbData.filmW, IMS_ERROR, concat_msg.Msg); 
				}
		}


		XtFree (value);	

		/* free up concat_msg space allocated */
		OP_FREECHARPTR(concat_msg.Msg);

		/* change cursor back to normal */
		timeOutCursors (False);
	}

 }
	UxSelectionBoxDlgContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: selectionDlg_popupCb
**
** Description:		Callback function to popup the selection box dialog.
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

void selectionDlg_popupCb(
	Widget wgt, 
	void *ptr,
	int fieldId)
{
	_UxCselectionBoxDlg           *UxSaveCtx, *UxContext;
	Widget         				         UxWidget = wgt;
	int i, n, status, memory;
	Widget selection_dlg_w;
	XmString label, title;
	XmStringTable str_list;
	OP_ORDER_LIST *orderPtr;
	OP_ORDER_ITEM_LIST *itemPtr;
	Pixel background;
	Widget textFieldW;
	OP_CAT_STRUCT *catReq;
	OP_PHOTO_CLIENT_DATA *photoClientData;
	OP_PHOTOJOB_ID_LIST *photoJobPtr, *pPtr;
	OP_FIRE_QUEUE_LIST *fireQPtr;
	OP_LASER_QUEUE_LIST *laserQPtr;
	char buffer[IMS_COL255_LEN+1];
	char query[IMS_COL255_LEN+1];
	char Msg[IMS_COL1024_LEN+1];


	selection_dlg_w = create_selectionBoxDlg(wgt);

	UxSaveCtx = UxSelectionBoxDlgContext;
	UxSelectionBoxDlgContext = UxContext =
		(_UxCselectionBoxDlg *) UxGetContext(selection_dlg_w);
	{
	
		switch (fieldId) 
		{
			case ORDER_PRIORITY:
				label = XmStringCreateLocalized ("Order Priority Valids:");
				title = XmStringCreateLocalized ("Priority Valids Selection Dialog");

				/* Create priority valids list strings */
				n = glbData.priority_count;
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));

				for (i = 0; i < n; i++)
				{
					str_list[i] = 
						XmStringCreateLocalized (glbData.priority[i].item_name);
				}

				orderPtr = (OP_ORDER_LIST *)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
										(XtCallbackProc)selectionDlg_orderPriority_okCb, orderPtr);

			break;

			case ITEM_PRIORITY:
				label = XmStringCreateLocalized ("Item Priority Valids:");
				title = XmStringCreateLocalized ("Priority Valids Selection Dialog");

				/* Create priority valids list strings */
				n = glbData.priority_count;
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));

				for (i = 0; i < n; i++)
				{
					str_list[i] = 
						XmStringCreateLocalized (glbData.priority[i].item_name);
				}

				itemPtr = (OP_ORDER_ITEM_LIST *)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
										(XtCallbackProc)selectionDlg_itemPriority_okCb, itemPtr);
			break;

			case ORDER_STATUS:
				label = XmStringCreateLocalized ("Order Status Valids:");
				title = XmStringCreateLocalized ("Status Valids Selection Dialog");

				n = XtNumber(status_valids);
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));
				for (i = 0; i < n; i++)
				{
					str_list[i] =
						XmStringCreateLocalized (status_valids[i]);
				}

				orderPtr = (OP_ORDER_LIST *)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
										(XtCallbackProc)selectionDlg_orderStatus_okCb, orderPtr);
			break;

			case ITEM_STATUS:
				label = XmStringCreateLocalized ("Item Status Valids:");
				title = XmStringCreateLocalized ("Status Valids Selection Dialog");

				n = XtNumber(status_valids);
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));
				for (i = 0; i < n; i++)
				{
					str_list[i] =
						XmStringCreateLocalized (status_valids[i]);
				}

				itemPtr = (OP_ORDER_ITEM_LIST *)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
										(XtCallbackProc)selectionDlg_itemStatus_okCb, itemPtr);
			break;

			case PHOTO_TYPE:
				label = XmStringCreateLocalized ("Photo Processing Valids:");
				title = XmStringCreateLocalized ("Photo Processing Selection Dialog");

				/* Create photo_type valids list strings */
				n = glbData.photo_type_count;
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));

				for (i = 0; i < n; i++)
				{
					str_list[i] = 
						XmStringCreateLocalized (glbData.photo_type[i].item_name);
				}

				textFieldW = (Widget)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
										(XtCallbackProc)selectionDlg_photo_okCb, textFieldW);
			break;


			/*
			** PHOTO JOB ID Valids
			*/
			case PHOTOJOB_ID:

			photoClientData = &(glbData.photoClientData);

			strcpy (query, "select distinct photojob_id from photo_job "
										 "order by photojob_id");

			catReq = &(photoClientData->catReq);
			catReq->item[0] = (int *)&n;
			catReq->item[1] = (char *)query;

			if ((status = ims_opCat (catReq, OP_GETPHOTOJOBIDLIST)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy (Msg, "Internal Error: OP_GETPHOTOJOBIDLIST failed.\n");
				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

				return; 
			}

			/*
			** assign returned photoJobIDs to photoJobPtr
			*/
			n = *(int *)catReq->item[0];
			photoJobPtr = (OP_PHOTOJOB_ID_LIST *)catReq->item[2];
			
			memory = 1;
			str_list = (XmStringTable)XtMalloc(n * sizeof(XmString *));
			memory = memory && str_list;
			if (!memory)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages in message window */
				sprintf(Msg, "Internal Error: memory allocation failed.");   
				msgBoxDlg_popupCb (glbData.photoJobW, IMS_FATAL, Msg); 

				/* Free compound strings */
				i = n;
				while (--i)
					XmStringFree (str_list[i]);

				XtFree ((char *)str_list);
				return;
			}

			pPtr = photoJobPtr;
			for (i = 0; i < n && pPtr != (OP_PHOTOJOB_ID_LIST *)NULL; i++)
			{
				sprintf (buffer, "%d", pPtr->photojob_id);
				str_list[i] = XmStringCreateLocalized(buffer);
				pPtr = pPtr->next;
			}


			/* free memory allocated with malloc() */
			pPtr = photoJobPtr;
			while (pPtr != (OP_PHOTOJOB_ID_LIST *)NULL)
			{
				photoJobPtr = photoJobPtr->next;
				free (pPtr);
				pPtr = photoJobPtr;
			}

			label = XmStringCreateLocalized ("Photo Job ID Valids:");
			title = XmStringCreateLocalized ("PhotoJob ID Valids Selection Dialog");
			textFieldW = (Widget)ptr;
			XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
			XtAddCallback (selection_dlg_w, XmNokCallback, 
									(XtCallbackProc)selectionDlg_photo_okCb, textFieldW);

			break;


			case FIRE_STATUS:
				label = XmStringCreateLocalized ("Fire Queue Status Valids:");
				title = XmStringCreateLocalized ("Status Valids Selection Dialog");

				n = XtNumber(fire_status_valids);
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));
				for (i = 0; i < n; i++)
				{
					str_list[i] =
						XmStringCreateLocalized (fire_status_valids[i]);
				}

				fireQPtr = (OP_FIRE_QUEUE_LIST *)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
							(XtCallbackProc)selectionDlg_fireStatus_okCb, fireQPtr);
			break;


			case LASER_STATUS:
				label = XmStringCreateLocalized ("Laser Queue Status Valids:");
				title = XmStringCreateLocalized ("Status Valids Selection Dialog");

				n = XtNumber(laser_status_valids);
				str_list = (XmStringTable) XtMalloc (n * sizeof (XmString *));
				for (i = 0; i < n; i++)
				{
					str_list[i] =
						XmStringCreateLocalized (laser_status_valids[i]);
				}

				laserQPtr = (OP_LASER_QUEUE_LIST *)ptr;
				XtRemoveAllCallbacks (selection_dlg_w, XmNokCallback);
				XtAddCallback (selection_dlg_w, XmNokCallback, 
							(XtCallbackProc)selectionDlg_laserStatus_okCb, laserQPtr);
			break;

			default:
			break;

		}

		XtVaSetValues (selection_dlg_w,
				XmNdialogTitle, title,
				XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
				XmNlistLabelString, label,
				XmNlistItems, str_list,
				XmNlistItemCount, n, 
				XmNmustMatch, True,
				NULL);

		XtUnmanageChild 
				(XmSelectionBoxGetChild (selection_dlg_w, XmDIALOG_APPLY_BUTTON));
		XtUnmanageChild 
				(XmSelectionBoxGetChild (selection_dlg_w, XmDIALOG_HELP_BUTTON));

		/*
		** When called from Photo Product and Film Generation screens
		** no comment text widget necessary.
		*/
		if ((fieldId == PHOTO_TYPE) || (fieldId == PHOTOJOB_ID) ||
				(fieldId == FIRE_STATUS) || (fieldId == LASER_STATUS))
			XtUnmanageChild (commentFormW);
		else
			XtManageChild (commentFormW);

		XtVaSetValues(XmSelectionBoxGetChild(selection_dlg_w, XmDIALOG_TEXT),
									XmNeditable, False,
									NULL);

		XtManageChild(selection_dlg_w);


		/* Free Compound Strings */
		XmStringFree (label);
		XmStringFree (title);
		while (--n >= 0)
			XmStringFree(str_list[n]);
		XtFree ((char *)str_list);


	}
	UxSelectionBoxDlgContext = UxSaveCtx;

}
	
