
/*******************************************************************************
       reqHelpDialog.c
       (Generated from interface file reqHelpDialog.i)
       Associated Header file: reqHelpDialog.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/MessageB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include "scan_qc_def.h"
static char sccsid_reqHelpDialog_i[] = "@(#)reqHelpDialog.i	1.11 97/04/30 14:44:02";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "reqHelpDialog.h"
#undef CONTEXT_MACRO_ACCESS

Widget	reqHelpDialog;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_reqHelpDialog( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCreqHelpDialog       *UxSaveCtx, *UxContext;

	UxSaveCtx = UxReqHelpDialogContext;
	UxContext = UxReqHelpDialogContext;
	{
	XtUnmanageChild(XmMessageBoxGetChild(UxWidget,
	                                     XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(UxWidget,
	                                     XmDIALOG_HELP_BUTTON));
	}
	UxReqHelpDialogContext = UxSaveCtx;
}

static void  okCallback_reqHelpDialog( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCreqHelpDialog       *UxSaveCtx, *UxContext;

	UxSaveCtx = UxReqHelpDialogContext;
	UxReqHelpDialogContext = UxContext =
			(_UxCreqHelpDialog *) UxGetContext( UxWidget );
	{
	XtDestroyWidget(UxWidget);
	}
	UxReqHelpDialogContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_reqHelpDialog()
{
	Widget		_UxParent;


	/* Creation of reqHelpDialog */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "reqHelpDialog_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 0,
			XmNy, 0,
			XmNwidth, 700,
			XmNheight, 300,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "reqHelpDialog",
			NULL );

	reqHelpDialog = XtVaCreateWidget( "reqHelpDialog",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_INFORMATION,
			XmNunitType, XmPIXELS,
			XmNwidth, 700,
			XmNheight, 300,
			RES_CONVERT( XmNdialogTitle, "CP_scan_qc -- Help" ),
			RES_CONVERT( XmNmessageString, "CP_scan_qc Help Message" ),
			XmNdefaultPosition, TRUE,
			NULL );
	XtAddCallback( reqHelpDialog, XmNokCallback,
		(XtCallbackProc) okCallback_reqHelpDialog,
		(XtPointer) UxReqHelpDialogContext );

	UxPutContext( reqHelpDialog, (char *) UxReqHelpDialogContext );
	UxPutClassCode( reqHelpDialog, _UxIfClassId );

	createCB_reqHelpDialog( reqHelpDialog,
			(XtPointer) UxReqHelpDialogContext, (XtPointer) NULL );


	XtAddCallback( reqHelpDialog, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxReqHelpDialogContext);


	return ( reqHelpDialog );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_reqHelpDialog( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCreqHelpDialog       *UxContext;
	static int		_Uxinit = 0;

	UxReqHelpDialogContext = UxContext =
		(_UxCreqHelpDialog *) UxNewContext( sizeof(_UxCreqHelpDialog), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_reqHelpDialog();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

