static char *sccs = "@(#)ims_opMsgBoxDlgCb.c	5.2  07/23/96";
/*******************************************************************************

	File:			ims_opMsgBoxDlgCb.c

	Function:	Callback functions for message dialog box 

	Author:		Jennifer Ting

	Date:			3/1995

	Revision: 6/11/1996 - Modified function msgBoxDlg_popupCb to correct PR 942.
						6/11/1996 - Modified function msgBoxDlg_okCb to correct PR 942.

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

#define _IMS_OP_MSGBOXDLGCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern OP_GLOBAL_DATA glbData;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opMsgBoxDlg.h>
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: msgBoxDlg_okCb
**
** Description:		Callback function for the OK button in msgBoxDlg,
**								destroys the message dialog box.
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

void msgBoxDlg_okCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	XtUnmanageChild (glbData.msgDlgW);
}
	


/*===========================================================================*
** 
** Function Name: msgBoxDlg_photoOkCb
**
** Description:		Special callback function for the OK button in msgBoxDlg,
**								when called by photoOrder_create widget.
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

void msgBoxDlg_photoOkCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCmsgBoxDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;


	OP_PHOTO_CLIENT_DATA *clientData;
	OP_PHOTO_QUEUE_LIST *photoQPtr;

	UxSaveCtx = UxMsgBoxDlgContext;
	UxMsgBoxDlgContext = UxContext =
			(_UxCmsgBoxDlg *) UxGetContext( UxWidget );
	{
		XtUnmanageChild (wgt);
		clientData = &(glbData.photoClientData);
		photoQPtr = clientData->photoQSelectList;

		/* call process_photoOrder, screen id is 0 */  
		process_photoOrder (0, photoQPtr) ;

	}
	UxMsgBoxDlgContext = UxSaveCtx;
}

/*===========================================================================*
** 
** Function Name: msgBoxDlg_popupCb
**
** Description:		Callback function to popup the message dialog box.
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. status - IMS_ status type
**								3. cbs 		- message to display
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void msgBoxDlg_popupCb(
	Widget wgt, 
	int status, 
	char *msg)
{
	_UxCmsgBoxDlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	Widget msg_dlg_w;
	XmString text, title;
	XmString ok_label, cancel_label;
	int dialog_type;

	UxSaveCtx = UxMsgBoxDlgContext;
	UxMsgBoxDlgContext = UxContext =
			(_UxCmsgBoxDlg *) UxGetContext(glbData.msgDlgW);
	{
		switch (status)
		{
			case IMS_ERROR:
			case IMS_FATAL:
				dialog_type = XmDIALOG_ERROR;
				title = XmStringCreateLocalized ("IMS_ERROR");
				break;
			case IMS_WARNING:
				dialog_type = XmDIALOG_WARNING;
				title = XmStringCreateLocalized ("IMS_WARNING");
				break;
			case 100:
				dialog_type = XmDIALOG_INFORMATION;
				title = XmStringCreateLocalized ("IMS_INFORMATION");
				break;
			default:
				dialog_type = XmDIALOG_INFORMATION;
				title = XmStringCreateLocalized ("IMS_INFORMATION");
				break;
		}

		text = XmStringCreateLtoR (msg, XmFONTLIST_DEFAULT_TAG);
		XtVaSetValues(msgBoxDlg,
			XmNdialogType, dialog_type,
			XmNdialogTitle, title,
			XmNmessageString, text,
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			NULL);

		if (status != 100)
		{
			XtUnmanageChild
				(XmMessageBoxGetChild (msgBoxDlg, XmDIALOG_CANCEL_BUTTON));

			XtRemoveAllCallbacks (msgBoxDlg, XmNokCallback);
			XtAddCallback (msgBoxDlg, XmNokCallback, 
									(XtCallbackProc)msgBoxDlg_okCb, NULL);

			ok_label = XmStringCreateLocalized ("OK");
			XtVaSetValues (msgBoxDlg, XmNokLabelString, ok_label, NULL);
		}
		else
		{
			XtManageChild
				(XmMessageBoxGetChild (msgBoxDlg, XmDIALOG_CANCEL_BUTTON));

			XtRemoveAllCallbacks (msgBoxDlg, XmNokCallback);
			XtAddCallback (msgBoxDlg, XmNokCallback, 
									(XtCallbackProc)msgBoxDlg_photoOkCb, NULL);

			ok_label = XmStringCreateLocalized ("Process Now");
			XtVaSetValues (msgBoxDlg, XmNokLabelString, ok_label, NULL);

			cancel_label = XmStringCreateLocalized ("Process Later");
			XtVaSetValues (msgBoxDlg, XmNcancelLabelString, cancel_label, NULL);

			XmStringFree (cancel_label);
		}

		XtUnmanageChild (XmMessageBoxGetChild (msgBoxDlg, XmDIALOG_HELP_BUTTON));

		XmStringFree (text);
		XmStringFree (title);
		XmStringFree (ok_label);
		
		/*
		** This is to add the callbacks to the window manager quit
		** button for each screen, this is to correct PR 942
		*/
		addWinMgrCloseCB (msgBoxDlg, msgBoxDlg_okCb, NULL);

		XtManageChild(msgBoxDlg);

	}
	UxMsgBoxDlgContext = UxSaveCtx;
}
	
