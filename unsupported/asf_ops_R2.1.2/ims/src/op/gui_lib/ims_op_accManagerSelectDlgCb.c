static char *sccs = "@(#)ims_op_accManagerSelectDlgCb.c	1.1  11/01/96";
/*******************************************************************************
**
** File:		ims_op_accManagerSelectDlgCb.c
**
** Function:	Callback functions for the account manager selection dialog.
**
** Author:		Jennifer Ting
**
** Date:			October, 1996 - R2.1
**
******************************************************************************/#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/TextF.h>
#include <Xm/Text.h>
#include <Xm/MenuShell.h>
#include "UxXt.h"

#include <Xm/SelectioB.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>
#include <ims_acct.h>

extern OP_GLOBAL_DATA glbData ;
extern Widget msg_dlg;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 1
#include "ims_op_accManagerSelectDlg.h"
#undef CONTEXT_MACRO_ACCESS

#include <ims_op_accAccountData.h>
#include <ims_op_accSearchAccount.h>

/*************************************************************************
** NAME :	account_manager_select_dlg_popupCb
**
** DESCRIPTION: Call back for the ok button of account manager select dlg
**
*************************************************************************/
void  account_manager_select_dlg_popupCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
    _UxCmanagerSelectionDlg *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    int               UxClientData = (int) cd;

		_UxCsearch_accounts *UxSearch_accountsContext;
		_UxCaccount_data *UxAccount_dataContext;
		XmStringTable userIdStr;
		OP_CAT_STRUCT *catReq;
		int status, i, count, memory;
		char Msg[IMS_COL1024_LEN+1];
		char query[IMS_COL255_LEN+1];
		char buffer[IMS_COL255_LEN+1];
		OP_USER_LIST *userPtr, *uPtr;
		Widget selection_dlg_w;
		int screenId;

		selection_dlg_w = create_managerSelectionDlg (NO_PARENT);
		screenId = (int) cd;

		if (screenId)
		{
			UxSearch_accountsContext = 
			(_UxCsearch_accounts *) UxGetContext (wgt);

			XtVaSetValues (selection_dlg_w, XmNuserData, 
										 UxSearch_accountsContext->UxmanagerTF, NULL);
		}
		else
		{
			UxAccount_dataContext = 
				(_UxCaccount_data *) UxGetContext (wgt);

			XtVaSetValues (selection_dlg_w, XmNuserData, 
										 UxAccount_dataContext->UxmanagerTF, NULL);
		}

		userPtr = (OP_USER_LIST *)NULL;
		catReq = &(glbData.accounts_users_data.catReq);

		/*
		** User ID Valids
		*/
		sprintf (query, "select distinct user_id from user_profile "
										"order by user_id");

		catReq->item[0] = (int *)&count;
		catReq->item[1] = (char *)query;

		if ((status = ims_opCat (catReq, OP_GETUSERIDLIST)) < IMS_OK)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages */
			strcpy (Msg, "Internal Error: OP_GETUSERIDLIST failed.\n");
		  msg_dlgCb (msg_dlg, IMS_FATAL, Msg); 

			return; 
		}

		/*
		** assign returned accounts to accountPtr
		*/
		count = *(int *)catReq->item[0];
		userPtr = (OP_USER_LIST *)catReq->item[2];

		/*
		** display users in list widget
		*/

		if ((count <= 0) || (userPtr == (OP_USER_LIST *)NULL))
		{
			/* Change cursor back to normal */
			timeOutCursors (False);
				
			XmListDeleteAllItems (XmSelectionBoxGetChild
														(selection_dlg_w, XmDIALOG_LIST));

			XmTextSetString (XmSelectionBoxGetChild
												(selection_dlg_w, XmDIALOG_TEXT), "");

			/* Display error messages in message window */
			sprintf(Msg, "No match found! ");
			msg_dlgCb (msg_dlg, IMS_INFO, Msg); 

			return; 
		}
			
		memory = 1;
		userIdStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
		memory = memory && userIdStr;
		if (!memory)
		{
			/* Change cursor back to normal */
			timeOutCursors (False);

			/* Display error messages in message window */
			strcpy (Msg, "Internal Error: memory allocation failed.");   
			msg_dlgCb (msg_dlg, IMS_FATAL, Msg); 

			/* Free compound strings */
			i = count;
			while (--i)
				XmStringFree (userIdStr[i]);

			XtFree ((char *)userIdStr);

			return;
		}

		uPtr = userPtr;
		for (i = 0; i < count && uPtr != (OP_USER_LIST *)NULL; i++)
		{
			sprintf (buffer, "%s", uPtr->user_id);
			userIdStr[i] = XmStringCreateLocalized(buffer);
			uPtr = uPtr->next;
		}

		XtVaSetValues(selection_dlg_w,
				XmNlistItems, userIdStr,
				XmNlistItemCount, count,
				NULL);

		/* free memory allocated with malloc() */
		uPtr = userPtr;
		while (uPtr != (OP_USER_LIST *)NULL)
		{
			userPtr = userPtr->next;
			free (uPtr);
			uPtr = userPtr;
		}

		/* free compound strings */
		while (--i)
			XmStringFree (userIdStr[i]);

		XtFree ((char *)userIdStr);

		XtUnmanageChild
			(XmSelectionBoxGetChild (selection_dlg_w, XmDIALOG_HELP_BUTTON));

		XtUnmanageChild
			(XmSelectionBoxGetChild (selection_dlg_w, XmDIALOG_APPLY_BUTTON));

		XtManageChild(selection_dlg_w);

}

/*************************************************************************
** NAME :	account_manager_select_dlg_okCb
**
** DESCRIPTION: Call back for the ok button of account manager select dlg
**
*************************************************************************/
void  account_manager_select_dlg_okCb ( 
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
    _UxCmanagerSelectionDlg *UxSaveCtx, *UxContext;
    Widget                  UxWidget = wgt;
    int               UxClientData = (int) cd;

		XmSelectionBoxCallbackStruct *cbs =
			(XmSelectionBoxCallbackStruct *)cb;

		Widget selection_dlg_w, manager_TF;
		char *text;

		UxSaveCtx = UxManagerSelectionDlgContext;
    UxManagerSelectionDlgContext = UxContext =
	       (_UxCmanagerSelectionDlg *) UxGetContext( UxWidget ) ;
		{
			selection_dlg_w = wgt;
			XtVaGetValues (selection_dlg_w, XmNuserData, &manager_TF, NULL);

			/*
			** Get valid selected 
			*/
			XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &text);
			XmTextFieldSetString (manager_TF, text);

			/* clean up */
			XtFree (text);
			XtDestroyWidget(wgt);	
    }
    UxManagerSelectionDlgContext = UxSaveCtx;
}

