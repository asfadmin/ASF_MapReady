static char *sccs = "@(#)ims_opSrchSelDlgCb.c	5.1  03/17/96";
/*******************************************************************************

	File:			ims_opSrchSelDlgCb.c

	Function:	Callback functions for search valids selection dialog

	Author:		Jennifer Ting

	Date:			3/1995

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

#define _IMS_OP_SRCHSELDLGCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#define ACCOUNT_ID	1
#define USER_ID			2

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opSrchSelDlg.h>
#undef CONTEXT_MACRO_ACCESS

#include <ims_opSearch.h>

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
** 
** Function Name: srchSelDlg_popupCb
**
** Description:		Callback function to popup the selection box dialog.
**
** Arguments:			1. widget  - Widget that is calling this callback
**								2. fieldId - identify accountId or userId PB
**								3. cb 		 - not used.
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void srchSelDlg_popupCb(
	Widget wgt, 
	int fieldId,
	XtPointer cb)
{
	_UxCsrchSelDlg           *UxSaveCtx, *UxContext;
	Widget         				         UxWidget = wgt;
	int i, n;
	Widget selection_dlg_w;
	XmString label, title;


	selection_dlg_w = create_srchSelDlg(wgt);

	UxSaveCtx = UxSrchSelDlgContext;
	UxSrchSelDlgContext = UxContext =
		(_UxCsrchSelDlg *) UxGetContext(selection_dlg_w);
	{

		switch (fieldId) 
		{
			case ACCOUNT_ID:
				label = XmStringCreateLocalized ("Account ID Valids:");
				title = XmStringCreateLocalized ("Account ID Valids Selection Dialog");
				XtVaSetValues (selection_dlg_w, XmNuserData, ACCOUNT_ID, NULL);

			break;

			case USER_ID:
				label = XmStringCreateLocalized ("User ID Valids:");
				title = XmStringCreateLocalized ("User ID Valids Selection Dialog");
				XtVaSetValues (selection_dlg_w, XmNuserData, USER_ID, NULL);
			break;

			default:
			break;

		}

		XtVaSetValues (selection_dlg_w,
				XmNdialogTitle, title,
				XmNlistLabelString, label,
				NULL);

		XtUnmanageChild 
				(XmSelectionBoxGetChild (selection_dlg_w, XmDIALOG_HELP_BUTTON));

		XtVaSetValues(XmSelectionBoxGetChild(selection_dlg_w, XmDIALOG_TEXT),
									XmNeditable, False,
									NULL);


		XtManageChild(selection_dlg_w);

		/* Free Compound Strings */
		XmStringFree (label);
		XmStringFree (title);


	}
	UxSrchSelDlgContext = UxSaveCtx;

}
	

/*===========================================================================*
** 
** Function Name: srchSelDlg_filterCb
**
** Description:		Callback function of the Filter push botton
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

void srchSelDlg_filterCb(
	Widget wgt, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCsrchSelDlg           *UxSaveCtx, *UxContext;
	Widget         				         UxWidget = wgt;
	Widget selection_dlg_w;
	char *text;
	char *cPtr;
	char Msg[IMS_COL1024_LEN+1];
	char query[IMS_COL255_LEN+1];
	char buffer[IMS_COL255_LEN+1];
	int fieldId, status;
	int i, count, memory;
	OP_CAT_STRUCT *catReq;
	OP_CLIENT_DATA *clientData;
	OP_USER_LIST   *userPtr, *uPtr;
	OP_ACCOUNT_LIST *accountPtr, *aPtr;
	XmStringTable accountIdStr, userIdStr;



	UxSaveCtx = UxSrchSelDlgContext;
	UxSrchSelDlgContext = UxContext =
		(_UxCsrchSelDlg *) UxGetContext(UxWidget);
	{

		selection_dlg_w = wgt;
		XtVaGetValues (selection_dlg_w, XmNuserData, &fieldId, NULL);
		clientData = &(glbData.orderClientData);
		userPtr = (OP_USER_LIST *)NULL;
		accountPtr = (OP_ACCOUNT_LIST *)NULL;

		text = XmTextFieldGetString (filterTF);
		if (!*text)
		{
			/* Display error messages */
			strcpy(Msg, "Please enter a filter value."); 
			msgBoxDlg_popupCb (glbData.searchW, IMS_INFO, Msg); 
	
			XtFree (text);
			return; 
		}
		else
		{
			cPtr = text;
			while (*cPtr != '\0')
			{
				if (*cPtr == '*')
				{
					*cPtr = '%';
				}
				cPtr++;
			}
		}


		/* Change cursor to watch cursor */
		timeOutCursors (True);

		switch (fieldId) 
		{
			/*
			** Account ID Valids
			*/
			case ACCOUNT_ID:

			sprintf (query, "select distinct account_id from account where "
											"account_id like '%s' order by account_id", text);

			catReq = &(clientData->catReq);
			catReq->item[0] = (int *)&count;
			catReq->item[1] = (char *)query;

			if ((status = ims_opCat (catReq, OP_GETACCOUNTIDLIST)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy (Msg, "Internal Error: OP_GETACCOUNTIDLIST failed.\n");
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg); 

				XtFree (text);
				return; 
			}

			/*
			** assign returned accounts to accountPtr
			*/
			count = *(int *)catReq->item[0];
			accountPtr = (OP_ACCOUNT_LIST *)catReq->item[2];

			/*
			** display accounts in list widget
			*/

			if ((count <= 0) || (accountPtr == (OP_ACCOUNT_LIST *)NULL))
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				XmListDeleteAllItems (XmSelectionBoxGetChild
															(selection_dlg_w, XmDIALOG_LIST));

				XmTextSetString (XmSelectionBoxGetChild
													(selection_dlg_w, XmDIALOG_TEXT), "");

				/* Display error messages in message window */
				sprintf(Msg, "No match found! ");
				msgBoxDlg_popupCb (glbData.searchW, IMS_INFO, Msg); 

				XtFree (text);
				return; 
			}
			
			memory = 1;
			accountIdStr = (XmStringTable)XtMalloc(count *sizeof(XmString *));
			memory = memory && accountIdStr;
			if (!memory)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages in message window */
				sprintf(Msg, "Internal Error: memory allocation failed.");   
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg); 

				/* Free compound strings */
				i = count;
				while (--i)
					XmStringFree (accountIdStr[i]);

				XtFree ((char *)accountIdStr);
				XtFree (text);
				return;
			}

			aPtr = accountPtr;
			for (i = 0; i < count && aPtr != (OP_ACCOUNT_LIST *)NULL; i++)
			{
				sprintf (buffer, "%s", aPtr->account_id);
				accountIdStr[i] = XmStringCreateLocalized(buffer);
				aPtr = aPtr->next;
			}

			XtVaSetValues(selection_dlg_w,
					XmNlistItems, accountIdStr,
					XmNlistItemCount, count,
					NULL);

			/* free memory allocated with malloc() */
			aPtr = accountPtr;
			while (aPtr != (OP_ACCOUNT_LIST *)NULL)
			{
				accountPtr = accountPtr->next;
				free (aPtr);
				aPtr = accountPtr;
			}

			/* free compound strings */
			while (--i)
				XmStringFree (accountIdStr[i]);

			XtFree ((char *)accountIdStr);
			XtFree (text);
			break;


			/*
			** User ID Valids
			*/
			case USER_ID:

			sprintf (query, "select distinct user_id from user_profile where "
											"user_id like '%s' order by user_id", text);

			catReq = &(clientData->catReq);
			catReq->item[0] = (int *)&count;
			catReq->item[1] = (char *)query;

			if ((status = ims_opCat (catReq, OP_GETUSERIDLIST)) < IMS_OK)
			{
				/* Change cursor back to normal */
				timeOutCursors (False);

				/* Display error messages */
				strcpy (Msg, "Internal Error: OP_GETUSERIDLIST failed.\n");
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg); 

				XtFree (text);
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
				msgBoxDlg_popupCb (glbData.searchW, IMS_INFO, Msg); 

				XtFree (text);
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
				sprintf(Msg, "Internal Error: memory allocation failed.");   
				msgBoxDlg_popupCb (glbData.searchW, IMS_FATAL, Msg); 

				/* Free compound strings */
				i = count;
				while (--i)
					XmStringFree (userIdStr[i]);

				XtFree ((char *)userIdStr);
				XtFree (text);
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
			XtFree (text);

			break;

			default:
			break;

		}

		/* Change cursor back to normal */
		timeOutCursors (False);

	}
	UxSrchSelDlgContext = UxSaveCtx;

}
	
/*===========================================================================*
** 
** Function Name: srchSelDlg_okCb
**
** Description:		Callback function of the OK push botton
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

void srchSelDlg_okCb(
	Widget wgt, 
	XtPointer cd,
	XtPointer cb)
{
	_UxCsrchSelDlg           *UxSaveCtx, *UxContext;
	_UxCsearch           		 *UxSearchContext;
	Widget         				         UxWidget = wgt;
	XtPointer											 UxCallbackArg = cb;

	XmSelectionBoxCallbackStruct *cbs =
			(XmSelectionBoxCallbackStruct *)cb;

	Widget selection_dlg_w;
	char *text;
	int fieldId;


	UxSaveCtx = UxSrchSelDlgContext;
	UxSrchSelDlgContext = UxContext =
		(_UxCsrchSelDlg *) UxGetContext(UxWidget);
	{

		selection_dlg_w = wgt;
		XtVaGetValues (selection_dlg_w, XmNuserData, &fieldId, NULL);

		/*
		** Get valid selected 
		*/
		XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &text);


		UxSearchContext = (_UxCsearch *) UxGetContext(glbData.searchW);
		switch (fieldId) 
		{
			case ACCOUNT_ID:
				XmTextFieldSetString (UxSearchContext->UxaccountIdTF, text);
			break;

			case USER_ID:
				XmTextFieldSetString (UxSearchContext->UxuserIdTF, text);
			break;

			default:
			break;
	
		}

		/* clean up */
		XtFree (text);
		XtDestroyWidget(wgt);	

	}
	UxSrchSelDlgContext = UxSaveCtx;

}
