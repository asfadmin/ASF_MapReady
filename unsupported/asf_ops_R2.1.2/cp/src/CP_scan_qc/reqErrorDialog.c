
/*******************************************************************************
       reqErrorDialog.c
       (Generated from interface file reqErrorDialog.i)
       Associated Header file: reqErrorDialog.h
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
static char sccsid_reqErrorDialog_i[] = "@(#)reqErrorDialog.i	1.15 97/04/30 14:44:15";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "reqErrorDialog.h"
#undef CONTEXT_MACRO_ACCESS

Widget	reqErrorDialog;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_reqErrorDialog( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCreqErrorDialog      *UxSaveCtx, *UxContext;

	UxSaveCtx = UxReqErrorDialogContext;
	UxContext = UxReqErrorDialogContext;
	{
	XtUnmanageChild(XmMessageBoxGetChild(UxWidget,
	                                     XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(UxWidget,
	                                     XmDIALOG_HELP_BUTTON));
	}
	UxReqErrorDialogContext = UxSaveCtx;
}

static void  okCallback_reqErrorDialog( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCreqErrorDialog      *UxSaveCtx, *UxContext;

	UxSaveCtx = UxReqErrorDialogContext;
	UxReqErrorDialogContext = UxContext =
			(_UxCreqErrorDialog *) UxGetContext( UxWidget );
	{
	error_cb();
	}
	UxReqErrorDialogContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_reqErrorDialog()
{
	Widget		_UxParent;


	/* Creation of reqErrorDialog */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "reqErrorDialog_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 191,
			XmNy, 549,
			XmNwidth, 600,
			XmNheight, 160,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "reqErrorDialog",
			NULL );

	reqErrorDialog = XtVaCreateWidget( "reqErrorDialog",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_ERROR,
			XmNunitType, XmPIXELS,
			XmNwidth, 600,
			XmNheight, 160,
			XmNbuttonFontList, UxConvertFontList("-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso2022-l442r641" ),
			XmNlabelFontList, UxConvertFontList("-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-adobe-helvetica-bold-o-normal--14-100-100-100-p-82-iso8859-1" ),
			RES_CONVERT( XmNdialogTitle, "CP_scan_qc -- Error" ),
			RES_CONVERT( XmNmessageString, "CP_scan_qc Error Message" ),
			RES_CONVERT( XmNbackground, "#ff0000" ),
			NULL );
	XtAddCallback( reqErrorDialog, XmNokCallback,
		(XtCallbackProc) okCallback_reqErrorDialog,
		(XtPointer) UxReqErrorDialogContext );

	UxPutContext( reqErrorDialog, (char *) UxReqErrorDialogContext );
	UxPutClassCode( reqErrorDialog, _UxIfClassId );

	createCB_reqErrorDialog( reqErrorDialog,
			(XtPointer) UxReqErrorDialogContext, (XtPointer) NULL );


	XtAddCallback( reqErrorDialog, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxReqErrorDialogContext);


	return ( reqErrorDialog );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_reqErrorDialog( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCreqErrorDialog      *UxContext;
	static int		_Uxinit = 0;

	UxReqErrorDialogContext = UxContext =
		(_UxCreqErrorDialog *) UxNewContext( sizeof(_UxCreqErrorDialog), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_reqErrorDialog();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

