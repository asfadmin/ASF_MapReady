
/*******************************************************************************
	vc_fileselectionbox.c

       Associated Header file: vc_fileselectionbox.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/FileSB.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:
 
Description:
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_fileselectionbox.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_fileselectionbox.c"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_fileselectionbox.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  createCB_APSFileSelection(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSFileSelection    *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSFileSelectionContext;
	UxContext = UxAPSFileSelectionContext;
	{
	    XtSetSensitive (
	        XmFileSelectionBoxGetChild (UxWidget, XmDIALOG_HELP_BUTTON), False) ;
	
	}
	UxAPSFileSelectionContext = UxSaveCtx;
}

static void  cancelCB_APSFileSelection(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSFileSelection    *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSFileSelectionContext;
	UxAPSFileSelectionContext = UxContext =
			(_UxCAPSFileSelection *) UxGetContext( UxWidget );
	{
	XtPopdown(XtParent(UxWidget)) ;
	
	}
	UxAPSFileSelectionContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSFileSelection()
{
	Widget		_UxParent;


	/* Creation of APSFileSelection */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "APSFileSelection_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 660,
			XmNy, 460,
			XmNwidth, 405,
			XmNheight, 395,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "APSFileSelection",
			XmNiconName, "APSFileSelection",
			NULL );

	}

	APSFileSelection = XtVaCreateManagedWidget( "APSFileSelection",
			xmFileSelectionBoxWidgetClass,
			_UxParent,
			XmNwidth, 405,
			XmNheight, 395,
			XmNunitType, XmPIXELS,
			RES_CONVERT( XmNtextString, "/ua/aps/rgreen/aps" ),
			NULL );
	XtAddCallback( APSFileSelection, XmNcancelCallback,
		(XtCallbackProc) cancelCB_APSFileSelection,
		(XtPointer) UxAPSFileSelectionContext );

	UxPutContext( APSFileSelection, (char *) UxAPSFileSelectionContext );
	UxPutClassCode( APSFileSelection, _UxIfClassId );

	createCB_APSFileSelection( APSFileSelection,
			(XtPointer) UxAPSFileSelectionContext, (XtPointer) NULL );


	XtAddCallback( APSFileSelection, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSFileSelectionContext);


	return ( APSFileSelection );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSFileSelection( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSFileSelection    *UxContext;
	static int		_Uxinit = 0;

	UxAPSFileSelectionContext = UxContext =
		(_UxCAPSFileSelection *) UxNewContext( sizeof(_UxCAPSFileSelection), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_APSFileSelection();

	XtSetSensitive(
	XmFileSelectionBoxGetChild(rtrn, XmDIALOG_HELP_BUTTON), False) ;
	return(rtrn) ;
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

