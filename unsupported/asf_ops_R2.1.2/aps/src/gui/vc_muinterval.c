
/*******************************************************************************
	vc_muinterval.c

       Associated Header file: vc_muinterval.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:			vc_muinterval.c
 
Description:

External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_muinterval.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_muinterval.c"

#include "gui_utils.h"
#include "cb_permstatus.h"

extern swidget	MUPermissionStatus;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_muinterval.h"
#undef CONTEXT_MACRO_ACCESS

Widget	MUIntervalDialog;
Widget	Interval_tf;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_MUInterval_dialogShell()
{
	Widget		_UxParent;


	/* Creation of MUInterval_dialogShell */
	_UxParent = MUPermissionStatus;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	MUInterval_dialogShell = XtVaCreatePopupShell( "MUInterval_dialogShell",
			xmDialogShellWidgetClass,
			_UxParent,
			XmNwidth, 245,
			XmNheight, 135,
			XmNx, 78,
			XmNy, 617,
			NULL );
	UxPutContext( MUInterval_dialogShell, (char *) UxMUInterval_dialogShellContext );
	UxPutClassCode( MUInterval_dialogShell, _UxIfClassId );


	/* Creation of MUIntervalDialog */
	MUIntervalDialog = XtVaCreateWidget( "MUIntervalDialog",
			xmMessageBoxWidgetClass,
			MUInterval_dialogShell,
			XmNwidth, 243,
			XmNheight, 113,
			XmNunitType, XmPIXELS,
			XmNdialogType, XmDIALOG_TEMPLATE,
			RES_CONVERT( XmNcancelLabelString, "Cancel" ),
			RES_CONVERT( XmNhelpLabelString, "Reset" ),
			RES_CONVERT( XmNokLabelString, "Ok" ),
			XmNx, 459,
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			NULL );
	XtAddCallback( MUIntervalDialog, XmNhelpCallback,
		(XtCallbackProc) cb_undo_interval_popup,
		(XtPointer) MU_INTERVAL_RESET );
	XtAddCallback( MUIntervalDialog, XmNokCallback,
		(XtCallbackProc) cb_interval_popup_OK,
		(XtPointer) UxMUInterval_dialogShellContext );
	XtAddCallback( MUIntervalDialog, XmNcancelCallback,
		(XtCallbackProc) cb_undo_interval_popup,
		(XtPointer) MU_INTERVAL_CANCEL );

	UxPutContext( MUIntervalDialog, (char *) UxMUInterval_dialogShellContext );


	/* Creation of MU_Interval_rc */
	MU_Interval_rc = XtVaCreateManagedWidget( "MU_Interval_rc",
			xmRowColumnWidgetClass,
			MUIntervalDialog,
			XmNwidth, 364,
			XmNheight, 28,
			XmNx, 14,
			XmNy, 30,
			XmNorientation, XmHORIZONTAL,
			NULL );
	UxPutContext( MU_Interval_rc, (char *) UxMUInterval_dialogShellContext );


	/* Creation of label42 */
	label42 = XtVaCreateManagedWidget( "label42",
			xmLabelWidgetClass,
			MU_Interval_rc,
			XmNx, 11,
			XmNy, 10,
			XmNwidth, 86,
			XmNheight, 16,
			RES_CONVERT( XmNlabelString, "auto-update every " ),
			NULL );
	UxPutContext( label42, (char *) UxMUInterval_dialogShellContext );


	/* Creation of Interval_tf */
	Interval_tf = XtVaCreateManagedWidget( "Interval_tf",
			xmTextFieldWidgetClass,
			MU_Interval_rc,
			XmNwidth, 54,
			XmNx, 111,
			XmNy, 0,
			XmNheight, 32,
			XmNcolumns, 5,
			XmNvalue, "5",
			XmNcursorPosition, 1,
			NULL );
	UxPutContext( Interval_tf, (char *) UxMUInterval_dialogShellContext );


	/* Creation of label62 */
	label62 = XtVaCreateManagedWidget( "label62",
			xmLabelWidgetClass,
			MU_Interval_rc,
			XmNx, 171,
			XmNy, 10,
			XmNwidth, 97,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, " seconds" ),
			NULL );
	UxPutContext( label62, (char *) UxMUInterval_dialogShellContext );

	XtVaSetValues(MUIntervalDialog,
			XmNinitialFocus, Interval_tf,
			NULL );


	XtAddCallback( MUInterval_dialogShell, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxMUInterval_dialogShellContext);


	return ( MUInterval_dialogShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_MUInterval_dialogShell( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCMUInterval_dialogShell *UxContext;
	static int		_Uxinit = 0;

	UxMUInterval_dialogShellContext = UxContext =
		(_UxCMUInterval_dialogShell *) UxNewContext( sizeof(_UxCMUInterval_dialogShell), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_MUInterval_dialogShell();

	cb_init_permstatus( rtrn, NULL, NULL ) ;
	
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

