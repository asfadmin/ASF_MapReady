
/*******************************************************************************
	vc_apsmenu.c

       Associated Header file: vc_apsmenu.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

#ifdef MOTIF
#include <Xm/RepType.h>
#endif /* MOTIF */

#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <X11/Shell.h>

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
#pragma ident   "@(#)vc_apsmenu.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.vc_apsmenu.c"

#include <stdlib.h>

#include "aps_defs.h"
#include "gui_defs.h"
#include "cb_apsmenu.h"
#include "cb_rgsdntimes.h"
#include "gui_utils.h"

Widget cnomorb_form ;
Widget cnomcov_form ;
Widget cdtakeopps_form ;
Widget apsfilegen_form ;
Widget apsfileproc_form ;
Widget DAR_manager ;
Widget DTK_manager ;
Widget DownTime_manager ;
Widget AntennaDownTime_manager ;
Widget PermStatus_viewer ;
Widget PermStatusInterval_popup ;
Widget con_roundup_form ;
Widget apswoscompare_form ;
Widget apsphaseselect_form ;
Widget filebox ;
Widget File_viewer ;
char display_string[APS_GUI_DISPLAY_STR_SIZE] ;


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "vc_apsmenu.h"
#undef CONTEXT_MACRO_ACCESS

Widget	APSMainMenu;
Widget	Pixmap_applWorkArea;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_FileGeneration_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(apsfilegen_form), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_FileProcessing_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(apsfileproc_form), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_ExitAPS_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	if (AskUser(UxWidget, "Do you really want to exit?", NO) == YES)
	    exit(APS_EXIT_OK) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_CreateNominalOrbit_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(cnomorb_form), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_CreateNominalCvrg_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(cnomcov_form), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_DARManager_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(DAR_manager), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_SiteCoverage_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(cdtakeopps_form), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_DTKManager_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent(DTK_manager), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_AntennaDowntimes_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	    XtPopup(XtParent(AntennaDownTime_manager), XtGrabNone) ;
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_MUPermissionStatus_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent( PermStatus_viewer ), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_APSWOSCompare_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup(XtParent( apswoscompare_form ), XtGrabNone) ;
	
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_pushButton_CON_ROUNDUP(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	XtPopup( gui_GetShellWidget( con_roundup_form ), XtGrabNone );
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

static void  activateCB_APSPhaseSelection_pb(
			Widget wgt, 
			XtPointer cd, 
			XtPointer cb)
{
	_UxCAPSMainMenu         *UxSaveCtx, *UxContext;
	Widget                  UxWidget = wgt;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxAPSMainMenuContext;
	UxAPSMainMenuContext = UxContext =
			(_UxCAPSMainMenu *) UxGetContext( UxWidget );
	{
	extern Widget apsphaseselect_form;
	XtPopup(XtParent(apsphaseselect_form), XtGrabNone) ;
	}
	UxAPSMainMenuContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_APSMainMenu()
{
	Widget		File_applMenuBar_shell;
	Widget		Coverage_applMenuBar_shell;
	Widget		Planning_applMenuBar_shell;
	Widget		Downtime_applMenuBar_shell;
	Widget		Tools_applMenuBar_shell;


	/* Creation of APSMainMenu */
	APSMainMenu = XtVaCreatePopupShell( "APSMainMenu",
			applicationShellWidgetClass,
			UxTopLevel,
			XmNx, 0,
			XmNy, -12,
			XmNtitle, "APS Main Menu",
			XmNiconName, "APS Main Menu",
			XmNminHeight, 340,
			XmNminWidth, 435,
			NULL );
	UxPutContext( APSMainMenu, (char *) UxAPSMainMenuContext );
	UxPutClassCode( APSMainMenu, _UxIfClassId );

	cb_create_aps_interfaces( APSMainMenu,
			(XtPointer) UxAPSMainMenuContext, (XtPointer) NULL );


	/* Creation of applMainWin1 */
	applMainWin1 = XtVaCreateManagedWidget( "applMainWin1",
			xmMainWindowWidgetClass,
			APSMainMenu,
			XmNunitType, XmPIXELS,
			NULL );
	UxPutContext( applMainWin1, (char *) UxAPSMainMenuContext );


	/* Creation of applMenuBar1 */
	applMenuBar1 = XtVaCreateManagedWidget( "applMenuBar1",
			xmRowColumnWidgetClass,
			applMainWin1,
			XmNrowColumnType, XmMENU_BAR,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( applMenuBar1, (char *) UxAPSMainMenuContext );


	/* Creation of File_applMenuBar */
	File_applMenuBar_shell = XtVaCreatePopupShell ("File_applMenuBar_shell",
			xmMenuShellWidgetClass, applMenuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	File_applMenuBar = XtVaCreateWidget( "File_applMenuBar",
			xmRowColumnWidgetClass,
			File_applMenuBar_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( File_applMenuBar, (char *) UxAPSMainMenuContext );


	/* Creation of FileGeneration_pb */
	FileGeneration_pb = XtVaCreateManagedWidget( "FileGeneration_pb",
			xmPushButtonWidgetClass,
			File_applMenuBar,
			RES_CONVERT( XmNlabelString, "APS File Generation" ),
			RES_CONVERT( XmNmnemonic, "G" ),
			NULL );
	XtAddCallback( FileGeneration_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FileGeneration_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( FileGeneration_pb, (char *) UxAPSMainMenuContext );


	/* Creation of FileProcessing_pb */
	FileProcessing_pb = XtVaCreateManagedWidget( "FileProcessing_pb",
			xmPushButtonWidgetClass,
			File_applMenuBar,
			RES_CONVERT( XmNlabelString, "APS File Processing" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( FileProcessing_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FileProcessing_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( FileProcessing_pb, (char *) UxAPSMainMenuContext );


	/* Creation of separator2 */
	separator2 = XtVaCreateManagedWidget( "separator2",
			xmSeparatorWidgetClass,
			File_applMenuBar,
			NULL );
	UxPutContext( separator2, (char *) UxAPSMainMenuContext );


	/* Creation of ExitAPS_pb */
	ExitAPS_pb = XtVaCreateManagedWidget( "ExitAPS_pb",
			xmPushButtonWidgetClass,
			File_applMenuBar,
			RES_CONVERT( XmNlabelString, "Exit APS" ),
			RES_CONVERT( XmNmnemonic, "x" ),
			NULL );
	XtAddCallback( ExitAPS_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ExitAPS_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( ExitAPS_pb, (char *) UxAPSMainMenuContext );


	/* Creation of file_pane1 */
	file_pane1 = XtVaCreateManagedWidget( "file_pane1",
			xmCascadeButtonWidgetClass,
			applMenuBar1,
			RES_CONVERT( XmNlabelString, "File " ),
			RES_CONVERT( XmNmnemonic, "F" ),
			XmNsubMenuId, File_applMenuBar,
			NULL );
	UxPutContext( file_pane1, (char *) UxAPSMainMenuContext );


	/* Creation of Coverage_applMenuBar */
	Coverage_applMenuBar_shell = XtVaCreatePopupShell ("Coverage_applMenuBar_shell",
			xmMenuShellWidgetClass, applMenuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Coverage_applMenuBar = XtVaCreateWidget( "Coverage_applMenuBar",
			xmRowColumnWidgetClass,
			Coverage_applMenuBar_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( Coverage_applMenuBar, (char *) UxAPSMainMenuContext );


	/* Creation of CreateNominalOrbit_pb */
	CreateNominalOrbit_pb = XtVaCreateManagedWidget( "CreateNominalOrbit_pb",
			xmPushButtonWidgetClass,
			Coverage_applMenuBar,
			RES_CONVERT( XmNlabelString, "Create Nominal Orbit" ),
			RES_CONVERT( XmNmnemonic, "O" ),
			NULL );
	XtAddCallback( CreateNominalOrbit_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_CreateNominalOrbit_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( CreateNominalOrbit_pb, (char *) UxAPSMainMenuContext );


	/* Creation of CreateNominalCvrg_pb */
	CreateNominalCvrg_pb = XtVaCreateManagedWidget( "CreateNominalCvrg_pb",
			xmPushButtonWidgetClass,
			Coverage_applMenuBar,
			RES_CONVERT( XmNlabelString, "Create Nominal Coverage" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			NULL );
	XtAddCallback( CreateNominalCvrg_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_CreateNominalCvrg_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( CreateNominalCvrg_pb, (char *) UxAPSMainMenuContext );


	/* Creation of applMenuBar1_top_b1 */
	applMenuBar1_top_b1 = XtVaCreateManagedWidget( "applMenuBar1_top_b1",
			xmCascadeButtonWidgetClass,
			applMenuBar1,
			RES_CONVERT( XmNlabelString, "Coverage " ),
			RES_CONVERT( XmNmnemonic, "C" ),
			XmNsubMenuId, Coverage_applMenuBar,
			NULL );
	UxPutContext( applMenuBar1_top_b1, (char *) UxAPSMainMenuContext );


	/* Creation of Planning_applMenuBar */
	Planning_applMenuBar_shell = XtVaCreatePopupShell ("Planning_applMenuBar_shell",
			xmMenuShellWidgetClass, applMenuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Planning_applMenuBar = XtVaCreateWidget( "Planning_applMenuBar",
			xmRowColumnWidgetClass,
			Planning_applMenuBar_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( Planning_applMenuBar, (char *) UxAPSMainMenuContext );


	/* Creation of DARManager_pb */
	DARManager_pb = XtVaCreateManagedWidget( "DARManager_pb",
			xmPushButtonWidgetClass,
			Planning_applMenuBar,
			RES_CONVERT( XmNlabelString, "DAR Manager" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			NULL );
	XtAddCallback( DARManager_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_DARManager_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( DARManager_pb, (char *) UxAPSMainMenuContext );


	/* Creation of SiteCoverage_pb */
	SiteCoverage_pb = XtVaCreateManagedWidget( "SiteCoverage_pb",
			xmPushButtonWidgetClass,
			Planning_applMenuBar,
			RES_CONVERT( XmNlabelString, "Site Coverage" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			NULL );
	XtAddCallback( SiteCoverage_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SiteCoverage_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( SiteCoverage_pb, (char *) UxAPSMainMenuContext );


	/* Creation of Mapper_pb */
	Mapper_pb = XtVaCreateManagedWidget( "Mapper_pb",
			xmPushButtonWidgetClass,
			Planning_applMenuBar,
			RES_CONVERT( XmNlabelString, "Mapper" ),
			RES_CONVERT( XmNmnemonic, "M" ),
			NULL );
	XtAddCallback( Mapper_pb, XmNactivateCallback,
		(XtCallbackProc) cb_start_mapper,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( Mapper_pb, (char *) UxAPSMainMenuContext );


	/* Creation of DTKManager_pb */
	DTKManager_pb = XtVaCreateManagedWidget( "DTKManager_pb",
			xmPushButtonWidgetClass,
			Planning_applMenuBar,
			RES_CONVERT( XmNlabelString, "DL/DTK Manager" ),
			RES_CONVERT( XmNmnemonic, "a" ),
			NULL );
	XtAddCallback( DTKManager_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_DTKManager_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( DTKManager_pb, (char *) UxAPSMainMenuContext );


	/* Creation of applMenuBar1_top_b2 */
	applMenuBar1_top_b2 = XtVaCreateManagedWidget( "applMenuBar1_top_b2",
			xmCascadeButtonWidgetClass,
			applMenuBar1,
			RES_CONVERT( XmNlabelString, "Planning " ),
			RES_CONVERT( XmNmnemonic, "P" ),
			XmNsubMenuId, Planning_applMenuBar,
			NULL );
	UxPutContext( applMenuBar1_top_b2, (char *) UxAPSMainMenuContext );


	/* Creation of Downtime_applMenuBar */
	Downtime_applMenuBar_shell = XtVaCreatePopupShell ("Downtime_applMenuBar_shell",
			xmMenuShellWidgetClass, applMenuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Downtime_applMenuBar = XtVaCreateWidget( "Downtime_applMenuBar",
			xmRowColumnWidgetClass,
			Downtime_applMenuBar_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( Downtime_applMenuBar, (char *) UxAPSMainMenuContext );


	/* Creation of RGSDowntimes_pb */
	RGSDowntimes_pb = XtVaCreateManagedWidget( "RGSDowntimes_pb",
			xmPushButtonWidgetClass,
			Downtime_applMenuBar,
			RES_CONVERT( XmNlabelString, "RGS Down Times" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( RGSDowntimes_pb, XmNactivateCallback,
		(XtCallbackProc) cb_popup_asfdntime_form,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( RGSDowntimes_pb, (char *) UxAPSMainMenuContext );


	/* Creation of AntennaDowntimes_pb */
	AntennaDowntimes_pb = XtVaCreateManagedWidget( "AntennaDowntimes_pb",
			xmPushButtonWidgetClass,
			Downtime_applMenuBar,
			RES_CONVERT( XmNlabelString, "Antenna Down Times" ),
			RES_CONVERT( XmNmnemonic, "A" ),
			NULL );
	XtAddCallback( AntennaDowntimes_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_AntennaDowntimes_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( AntennaDowntimes_pb, (char *) UxAPSMainMenuContext );


	/* Creation of applMenuBar1_top_b3 */
	applMenuBar1_top_b3 = XtVaCreateManagedWidget( "applMenuBar1_top_b3",
			xmCascadeButtonWidgetClass,
			applMenuBar1,
			RES_CONVERT( XmNlabelString, "Downtime " ),
			RES_CONVERT( XmNmnemonic, "D" ),
			XmNsubMenuId, Downtime_applMenuBar,
			NULL );
	UxPutContext( applMenuBar1_top_b3, (char *) UxAPSMainMenuContext );


	/* Creation of Tools_applMenuBar */
	Tools_applMenuBar_shell = XtVaCreatePopupShell ("Tools_applMenuBar_shell",
			xmMenuShellWidgetClass, applMenuBar1,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Tools_applMenuBar = XtVaCreateWidget( "Tools_applMenuBar",
			xmRowColumnWidgetClass,
			Tools_applMenuBar_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( Tools_applMenuBar, (char *) UxAPSMainMenuContext );


	/* Creation of MUPermissionStatus_pb */
	MUPermissionStatus_pb = XtVaCreateManagedWidget( "MUPermissionStatus_pb",
			xmPushButtonWidgetClass,
			Tools_applMenuBar,
			RES_CONVERT( XmNlabelString, "MU Permission Status" ),
			RES_CONVERT( XmNmnemonic, "M" ),
			NULL );
	XtAddCallback( MUPermissionStatus_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_MUPermissionStatus_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( MUPermissionStatus_pb, (char *) UxAPSMainMenuContext );


	/* Creation of APSWOSCompare_pb */
	APSWOSCompare_pb = XtVaCreateManagedWidget( "APSWOSCompare_pb",
			xmPushButtonWidgetClass,
			Tools_applMenuBar,
			RES_CONVERT( XmNlabelString, "MWOS Comparison" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( APSWOSCompare_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_APSWOSCompare_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( APSWOSCompare_pb, (char *) UxAPSMainMenuContext );


	/* Creation of pushButton_CON_ROUNDUP */
	pushButton_CON_ROUNDUP = XtVaCreateManagedWidget( "pushButton_CON_ROUNDUP",
			xmPushButtonWidgetClass,
			Tools_applMenuBar,
			RES_CONVERT( XmNlabelString, "DL/DTK CON Roundup" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			NULL );
	XtAddCallback( pushButton_CON_ROUNDUP, XmNactivateCallback,
		(XtCallbackProc) activateCB_pushButton_CON_ROUNDUP,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( pushButton_CON_ROUNDUP, (char *) UxAPSMainMenuContext );


	/* Creation of APSPhaseSelection_pb */
	APSPhaseSelection_pb = XtVaCreateManagedWidget( "APSPhaseSelection_pb",
			xmPushButtonWidgetClass,
			Tools_applMenuBar,
			RES_CONVERT( XmNlabelString, "REQQ Phase Selection" ),
			NULL );
	XtAddCallback( APSPhaseSelection_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_APSPhaseSelection_pb,
		(XtPointer) UxAPSMainMenuContext );

	UxPutContext( APSPhaseSelection_pb, (char *) UxAPSMainMenuContext );


	/* Creation of applMenuBar1_top_b4 */
	applMenuBar1_top_b4 = XtVaCreateManagedWidget( "applMenuBar1_top_b4",
			xmCascadeButtonWidgetClass,
			applMenuBar1,
			RES_CONVERT( XmNlabelString, "Tools " ),
			RES_CONVERT( XmNmnemonic, "T" ),
			XmNsubMenuId, Tools_applMenuBar,
			NULL );
	UxPutContext( applMenuBar1_top_b4, (char *) UxAPSMainMenuContext );


	/* Creation of applForm1 */
	applForm1 = XtVaCreateManagedWidget( "applForm1",
			xmFormWidgetClass,
			applMainWin1,
			XmNautoUnmanage, FALSE,
			XmNresizePolicy, XmRESIZE_NONE,
			NULL );
	UxPutContext( applForm1, (char *) UxAPSMainMenuContext );


	/* Creation of Pixmap_applWorkArea */
	Pixmap_applWorkArea = XtVaCreateManagedWidget( "Pixmap_applWorkArea",
			xmLabelWidgetClass,
			applForm1,
			XmNx, 10,
			XmNy, 13,
			XmNlabelType, XmPIXMAP,
			NULL );
	UxPutContext( Pixmap_applWorkArea, (char *) UxAPSMainMenuContext );

	cb_get_pixmap( Pixmap_applWorkArea,
			(XtPointer) UxAPSMainMenuContext, (XtPointer) NULL );


	XtAddCallback( APSMainMenu, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxAPSMainMenuContext);

	XmMainWindowSetAreas( applMainWin1, applMenuBar1, (Widget) NULL,
			(Widget) NULL, (Widget) NULL, applForm1 );

	return ( APSMainMenu );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_APSMainMenu( swidget _UxUxParent )
{
	Widget                  rtrn;
	_UxCAPSMainMenu         *UxContext;
	static int		_Uxinit = 0;

	UxAPSMainMenuContext = UxContext =
		(_UxCAPSMainMenu *) UxNewContext( sizeof(_UxCAPSMainMenu), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		XmRepTypeInstallTearOffModelConverter();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_APSMainMenu();

	XtVaSetValues( gui_GetShellWidget( rtrn ),
		XmNdeleteResponse, XmDO_NOTHING,
		NULL ) ;
	
	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

