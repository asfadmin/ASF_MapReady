
/*******************************************************************************
       CPinfoBox.c
       (Generated from interface file CPinfoBox.i)
       Associated Header file: CPinfoBox.h
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

static char sccsid_CPinfoBox_i[] = "@(#)CPinfoBox.i	2.8 96/05/08 17:58:29";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPinfoBox.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPinfoBox;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_CPinfoBox( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPinfoBox           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPinfoBoxContext;
	UxContext = UxCPinfoBoxContext;
	{
	  XtUnmanageChild(XmMessageBoxGetChild(UxThisWidget,
	                                       XmDIALOG_HELP_BUTTON) );
	
	  XtUnmanageChild(XmMessageBoxGetChild(UxThisWidget,
	                                       XmDIALOG_CANCEL_BUTTON) );
	
	}
	UxCPinfoBoxContext = UxSaveCtx;
}

static void  okCallback_CPinfoBox( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPinfoBox           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPinfoBoxContext;
	UxCPinfoBoxContext = UxContext =
			(_UxCCPinfoBox *) UxGetContext( UxWidget );
	{
	XtDestroyWidget(UxWidget);
	}
	UxCPinfoBoxContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPinfoBox()
{
	Widget		_UxParent;


	/* Creation of CPinfoBox */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "CPinfoBox_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 453,
			XmNy, 382,
			XmNheight, 146,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CPinfoBox",
			NULL );

	CPinfoBox = XtVaCreateWidget( "CPinfoBox",
			xmMessageBoxWidgetClass,
			_UxParent,
			RES_CONVERT( XmNdialogTitle, "Information" ),
			XmNdialogType, XmDIALOG_INFORMATION,
			XmNunitType, XmPIXELS,
			XmNheight, 146,
			RES_CONVERT( XmNmessageString, "default info string" ),
			XmNdefaultPosition, FALSE,
			NULL );
	XtAddCallback( CPinfoBox, XmNokCallback,
		(XtCallbackProc) okCallback_CPinfoBox,
		(XtPointer) UxCPinfoBoxContext );

	UxPutContext( CPinfoBox, (char *) UxCPinfoBoxContext );
	UxPutClassCode( CPinfoBox, _UxIfClassId );

	createCB_CPinfoBox( CPinfoBox,
			(XtPointer) UxCPinfoBoxContext, (XtPointer) NULL );


	XtAddCallback( CPinfoBox, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPinfoBoxContext);


	return ( CPinfoBox );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPinfoBox( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCCPinfoBox           *UxContext;
	static int		_Uxinit = 0;

	UxCPinfoBoxContext = UxContext =
		(_UxCCPinfoBox *) UxNewContext( sizeof(_UxCCPinfoBox), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CPinfoBox();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

