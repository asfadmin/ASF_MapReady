
/*******************************************************************************
	vc_permstatus.c

       Associated Header file: vc_permstatus.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/ToggleB.h>
#include <Xm/Form.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:	vc_permstatus.c

Description:	the gui builder code for reporting the status of the
		multi-user permissions.
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident   "@(#)vc_permstatus.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_permstatus.c"

#include "cb_permstatus.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_permstatus.h"
#undef CONTEXT_MACRO_ACCESS

Widget	MUPermissionStatus;
Widget	AutoPoll_tb;
Widget	MU_perm_scrolledList;
Widget	PollTime_lbl;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_MUPermissionStatus()
{
	Widget		_UxParent;


	/* Creation of MUPermissionStatus */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "MUPermissionStatus_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNx, 0,
			XmNy, 565,
			XmNwidth, 767,
			XmNheight, 225,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "MUPermissionStatus",
			XmNiconName, "MUPermissionStatus",
			NULL );

	}

	MUPermissionStatus = XtVaCreateManagedWidget( "MUPermissionStatus",
			xmFormWidgetClass,
			_UxParent,
			XmNwidth, 767,
			XmNheight, 225,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNunitType, XmPIXELS,
			NULL );
	UxPutContext( MUPermissionStatus, (char *) UxMUPermissionStatusContext );
	UxPutClassCode( MUPermissionStatus, _UxIfClassId );


	/* Creation of AutoPoll_tb */
	AutoPoll_tb = XtVaCreateManagedWidget( "AutoPoll_tb",
			xmToggleButtonWidgetClass,
			MUPermissionStatus,
			XmNx, 34,
			XmNy, 179,
			XmNwidth, 220,
			XmNheight, 20,
			XmNindicatorOn, TRUE,
			RES_CONVERT( XmNlabelString, "Auto-update every 10 seconds" ),
			XmNset, TRUE,
			XmNsensitive, TRUE,
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	XtAddCallback( AutoPoll_tb, XmNvalueChangedCallback,
		(XtCallbackProc) cb_autoUpdate_toggled,
		(XtPointer) UxMUPermissionStatusContext );

	UxPutContext( AutoPoll_tb, (char *) UxMUPermissionStatusContext );


	/* Creation of MU_perm_scrolledWindow */
	MU_perm_scrolledWindow = XtVaCreateManagedWidget( "MU_perm_scrolledWindow",
			xmScrolledWindowWidgetClass,
			MUPermissionStatus,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNx, 36,
			XmNy, 39,
			XmNheight, 130,
			NULL );
	UxPutContext( MU_perm_scrolledWindow, (char *) UxMUPermissionStatusContext );


	/* Creation of MU_perm_scrolledList */
	MU_perm_scrolledList = XtVaCreateManagedWidget( "MU_perm_scrolledList",
			xmListWidgetClass,
			MU_perm_scrolledWindow,
			XmNwidth, 691,
			XmNheight, 130,
			XmNvisibleItemCount, 10,
			XmNfontList, UxConvertFontList("-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNitemCount, 0,
			NULL );
	UxPutContext( MU_perm_scrolledList, (char *) UxMUPermissionStatusContext );


	/* Creation of MU_permStatusRefresh_pb */
	MU_permStatusRefresh_pb = XtVaCreateManagedWidget( "MU_permStatusRefresh_pb",
			xmPushButtonWidgetClass,
			MUPermissionStatus,
			XmNx, 3,
			XmNy, 39,
			XmNwidth, 30,
			XmNheight, 130,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "R\nE\nF\nR\nE\nS\nH" ),
			NULL );
	XtAddCallback( MU_permStatusRefresh_pb, XmNactivateCallback,
		(XtCallbackProc) cb_refresh_perms,
		(XtPointer) UxMUPermissionStatusContext );

	UxPutContext( MU_permStatusRefresh_pb, (char *) UxMUPermissionStatusContext );


	/* Creation of MU_Quit_pb */
	MU_Quit_pb = XtVaCreateManagedWidget( "MU_Quit_pb",
			xmPushButtonWidgetClass,
			MUPermissionStatus,
			XmNx, 635,
			XmNy, 179,
			XmNwidth, 90,
			XmNheight, 40,
			XmNfontList, UxConvertFontList("rockwell-bold" ),
			RES_CONVERT( XmNlabelString, "QUIT" ),
			XmNleftOffset, 455,
			XmNtopOffset, 471,
			NULL );
	XtAddCallback( MU_Quit_pb, XmNactivateCallback,
		(XtCallbackProc) cb_quit_permstatus,
		(XtPointer) UxMUPermissionStatusContext );

	UxPutContext( MU_Quit_pb, (char *) UxMUPermissionStatusContext );


	/* Creation of label74 */
	label74 = XtVaCreateManagedWidget( "label74",
			xmLabelWidgetClass,
			MUPermissionStatus,
			XmNx, 37,
			XmNy, 8,
			XmNwidth, 691,
			XmNheight, 31,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "User     Node       Command/Activity                       Darid/     Start Time            Stop Time            \n                                                           Station" ),
			NULL );
	UxPutContext( label74, (char *) UxMUPermissionStatusContext );


	/* Creation of PollTime_lbl */
	PollTime_lbl = XtVaCreateManagedWidget( "PollTime_lbl",
			xmLabelWidgetClass,
			MUPermissionStatus,
			XmNx, 126,
			XmNy, 205,
			XmNwidth, 144,
			XmNheight, 14,
			RES_CONVERT( XmNlabelString, "                     )" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( PollTime_lbl, (char *) UxMUPermissionStatusContext );


	/* Creation of label73 */
	label73 = XtVaCreateManagedWidget( "label73",
			xmLabelWidgetClass,
			MUPermissionStatus,
			XmNx, 34,
			XmNy, 205,
			XmNwidth, 95,
			XmNheight, 14,
			RES_CONVERT( XmNlabelString, "(Last Updated:" ),
			NULL );
	UxPutContext( label73, (char *) UxMUPermissionStatusContext );


	XtAddCallback( MUPermissionStatus, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxMUPermissionStatusContext);


	return ( MUPermissionStatus );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_MUPermissionStatus( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCMUPermissionStatus  *UxContext;
	static int		_Uxinit = 0;

	UxMUPermissionStatusContext = UxContext =
		(_UxCMUPermissionStatus *) UxNewContext( sizeof(_UxCMUPermissionStatus), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_MUPermissionStatus();

	XtAddCallback( XtParent( rtrn ), XtNpopupCallback,
			cb_popup_permstatus, NULL ) ;
	
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

