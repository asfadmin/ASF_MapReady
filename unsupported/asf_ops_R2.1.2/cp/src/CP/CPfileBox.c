
/*******************************************************************************
       CPfileBox.c
       (Generated from interface file CPfileBox.i)
       Associated Header file: CPfileBox.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/FileSB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static char sccsid_CPfileBox_i[] = "@(#)CPfileBox.i	2.7 96/02/21 13:53:04";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPfileBox.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPfileBox;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_CPfileBox( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPfileBox           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPfileBoxContext;
	UxContext = UxCPfileBoxContext;
	{
	doCreateMainWindowFileBox(UxWidget);
	}
	UxCPfileBoxContext = UxSaveCtx;
}

static void  okCallback_CPfileBox( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPfileBox           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPfileBoxContext;
	UxCPfileBoxContext = UxContext =
			(_UxCCPfileBox *) UxGetContext( UxWidget );
	{
	doSaveRestore(UxWidget, UxClientData, UxCallbackArg);
	}
	UxCPfileBoxContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPfileBox()
{
	Widget		_UxParent;


	/* Creation of CPfileBox */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "CPfileBox_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 457,
			XmNy, 305,
			XmNwidth, 315,
			XmNheight, 373,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CPfileBox",
			NULL );

	CPfileBox = XtVaCreateWidget( "CPfileBox",
			xmFileSelectionBoxWidgetClass,
			_UxParent,
			XmNwidth, 315,
			XmNheight, 373,
			XmNdialogType, XmDIALOG_FILE_SELECTION,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNdialogTitle, "-- file select --" ),
			XmNautoUnmanage, TRUE,
			XmNdefaultPosition, FALSE,
			NULL );
	XtAddCallback( CPfileBox, XmNokCallback,
		(XtCallbackProc) okCallback_CPfileBox,
		(XtPointer) UxCPfileBoxContext );

	UxPutContext( CPfileBox, (char *) UxCPfileBoxContext );
	UxPutClassCode( CPfileBox, _UxIfClassId );

	createCB_CPfileBox( CPfileBox,
			(XtPointer) UxCPfileBoxContext, (XtPointer) NULL );


	XtAddCallback( CPfileBox, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPfileBoxContext);


	return ( CPfileBox );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPfileBox( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCCPfileBox           *UxContext;
	static int		_Uxinit = 0;

	UxCPfileBoxContext = UxContext =
		(_UxCCPfileBox *) UxNewContext( sizeof(_UxCCPfileBox), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CPfileBox();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

