
/*******************************************************************************
	vc_fileviewer.c

       Associated Header file: vc_fileviewer.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>

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
#pragma ident   "@(#)vc_fileviewer.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_fileviewer.c"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_fileviewer.h"
#undef CONTEXT_MACRO_ACCESS


/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_pushButton30(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCFileViewer          *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxFileViewerContext;
	UxFileViewerContext = UxContext =
			(_UxCFileViewer *) UxGetContext( UxWidget );
	{
	UxPopdownInterface( UxThisWidget );
	}
	UxFileViewerContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_FileViewer()
{
	Widget		_UxParent;


	/* Creation of FileViewer */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "FileViewer_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 320,
			XmNy, 170,
			XmNwidth, 570,
			XmNheight, 280,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "FileViewer",
			XmNiconName, "FileViewer",
			NULL );

	}

	FileViewer = XtVaCreateManagedWidget( "FileViewer",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 570,
			XmNheight, 280,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			NULL );
	UxPutContext( FileViewer, (char *) UxFileViewerContext );
	UxPutClassCode( FileViewer, _UxIfClassId );


	/* Creation of scrolledWindowFileText */
	scrolledWindowFileText = XtVaCreateManagedWidget( "scrolledWindowFileText",
			xmScrolledWindowWidgetClass,
			FileViewer,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNx, 10,
			XmNy, 20,
			XmNheight, 200,
			XmNwidth, 550,
			NULL );
	UxPutContext( scrolledWindowFileText, (char *) UxFileViewerContext );


	/* Creation of scrolledText_messages */
	scrolledText_messages = XtVaCreateManagedWidget( "scrolledText_messages",
			xmTextWidgetClass,
			scrolledWindowFileText,
			XmNwidth, 530,
			XmNheight, 230,
			XmNeditMode, XmMULTI_LINE_EDIT ,
			XmNeditable, FALSE,
			XmNwordWrap, TRUE,
			NULL );
	UxPutContext( scrolledText_messages, (char *) UxFileViewerContext );


	/* Creation of pushButton30 */
	pushButton30 = XtVaCreateManagedWidget( "pushButton30",
			xmPushButtonWidgetClass,
			FileViewer,
			XmNx, 223,
			XmNy, 234,
			XmNwidth, 115,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNfontList, UxConvertFontList("h12bluci" ),
			NULL );
	XtAddCallback( pushButton30, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton30,
		(XtPointer) UxFileViewerContext );

	UxPutContext( pushButton30, (char *) UxFileViewerContext );


	XtAddCallback( FileViewer, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxFileViewerContext);


	return ( FileViewer );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_FileViewer( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCFileViewer          *UxContext;
	static int		_Uxinit = 0;

	UxFileViewerContext = UxContext =
		(_UxCFileViewer *) UxNewContext( sizeof(_UxCFileViewer), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_FileViewer();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

