
/*******************************************************************************
       CPerrorBox.c
       (Generated from interface file CPerrorBox.i)
       Associated Header file: CPerrorBox.h
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

static char sccsid_CPerrorBox_i[] = "@(#)CPerrorBox.i	2.4 95/03/24 18:47:50";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPerrorBox.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_CPerrorBox( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPerrorBox          *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPerrorBoxContext;
	UxContext = UxCPerrorBoxContext;
	{
	XtUnmanageChild(XmMessageBoxGetChild(UxWidget,
	                                     XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(UxWidget,
	                                     XmDIALOG_HELP_BUTTON));
	
	}
	UxCPerrorBoxContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPerrorBox()
{
	Widget		_UxParent;


	/* Creation of CPerrorBox */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	_UxParent = XtVaCreatePopupShell( "CPerrorBox_shell",
			xmDialogShellWidgetClass, _UxParent,
			XmNx, 384,
			XmNy, 398,
			XmNheight, 160,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CPerrorBox",
			NULL );

	CPerrorBox = XtVaCreateWidget( "CPerrorBox",
			xmMessageBoxWidgetClass,
			_UxParent,
			XmNdialogType, XmDIALOG_ERROR,
			XmNheight, 160,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNmessageString, "default process died" ),
			XmNdialogStyle, XmDIALOG_MODELESS,
			RES_CONVERT( XmNdialogTitle, "Error" ),
			XmNmessageAlignment, XmALIGNMENT_CENTER,
			XmNdefaultPosition, FALSE,
			NULL );
	UxPutContext( CPerrorBox, (char *) UxCPerrorBoxContext );
	UxPutClassCode( CPerrorBox, _UxIfClassId );

	createCB_CPerrorBox( CPerrorBox,
			(XtPointer) UxCPerrorBoxContext, (XtPointer) NULL );


	XtAddCallback( CPerrorBox, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPerrorBoxContext);


	return ( CPerrorBox );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPerrorBox( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCCPerrorBox          *UxContext;
	static int		_Uxinit = 0;

	UxCPerrorBoxContext = UxContext =
		(_UxCCPerrorBox *) UxNewContext( sizeof(_UxCCPerrorBox), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CPerrorBox();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

