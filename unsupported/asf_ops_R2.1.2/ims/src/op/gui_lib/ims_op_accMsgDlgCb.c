static char *sccs = "@(#)ims_op_accMsgDlgCb.c	5.1  03/17/96";
/*******************************************************************************

	File:			ims_op_acc_msg_dlgCb.c

	Function:	Callback functions for the msg_dlg box 

	Author:		Armando Cardona. Adapted from Jennifer Ting version.

	Date:			3/1995

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


/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/
#include <ims_query.h>
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include "ims_op_accMsgDlg.h"
#undef CONTEXT_MACRO_ACCESS

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*===========================================================================*
** 
** Function Name: msg_dlg_okCb
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

void msg_dlg_okCb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCmsg_dlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxMsg_dlgContext;
	UxMsg_dlgContext = UxContext =
			(_UxCmsg_dlg *) UxGetContext( UxWidget );
	{
		/*XtDestroyWidget(wgt);	*/
                 XtUnmanageChild ( wgt ) ;
	}
	UxMsg_dlgContext = UxSaveCtx;
}
	

/*===========================================================================*
** 
** Function Name: msg_dlgCb
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

void msg_dlgCb(
	Widget wgt, 
	int status, 
	char *msg)
{
	_UxCmsg_dlg           *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	Widget msg_dlg_w;
	XmString text, title;
	int dialog_type;

	UxSaveCtx = UxMsg_dlgContext;
	UxMsg_dlgContext = UxContext =
			(_UxCmsg_dlg *) UxGetContext( UxWidget );
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
			default:
				dialog_type = XmDIALOG_INFORMATION;
				title = XmStringCreateLocalized 
                                                     ("IMS_INFORMATION");
				break;
		}
                
		text = XmStringCreateLtoR (msg,
                                     XmFONTLIST_DEFAULT_TAG);
		XtVaSetValues(msg_dlg/*msg_dlg_w*/,
			XmNdialogType, dialog_type,
			XmNdialogTitle, title,
			XmNmessageString, text,
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			NULL);

		XtUnmanageChild (XmMessageBoxGetChild (msg_dlg/*msg_dlg_w*/, XmDIALOG_CANCEL_BUTTON));
		XtUnmanageChild (XmMessageBoxGetChild (msg_dlg/*msg_dlg_w*/, XmDIALOG_HELP_BUTTON));

		XmStringFree (text);
		XmStringFree (title);
		
		XtManageChild(msg_dlg);

	}
	UxMsg_dlgContext = UxSaveCtx;
}
	
