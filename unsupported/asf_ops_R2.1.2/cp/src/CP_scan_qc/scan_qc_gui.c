
/*******************************************************************************
       scan_qc_gui.c
       (Generated from interface file scan_qc_gui.i)
       Associated Header file: scan_qc_gui.h
*******************************************************************************/

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/SeparatoG.h>
#include <Xm/LabelG.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DialogS.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

/*----------------------------------------------------------
 * SCCS Header
 * File:selectFrame.i   Rev:1.3.0.0   Date:95/09/25
 *
 *---------------------------------------------------------*/
#include "scan_qc_def.h"
static char sccsid_selectFrame_i[] = "@(#)scan_qc_gui.i	1.13 97/04/30 14:45:06";


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "scan_qc_gui.h"
#undef CONTEXT_MACRO_ACCESS

Widget	scanQcDialogShell;
Widget	scanQcMainWindow;
Widget	platformData;
Widget	stationData;
Widget	revolutionData;
Widget	instrumentModeData;
Widget	sequenceData;
Widget	frameSizeData;
Widget	segmentNbrData;
Widget	OKSelectFrame;
Widget	frameScrollBar;
Widget	frameCenterTimeScrollList;
Widget	frameCenterLonScrollList;
Widget	frameCenterLatScrollList;
Widget	frameFrameScrollList;
Widget	frameSegmentScrollList;
Widget	backSelectFrame;
Widget	segmentSegmentScrollList;
Widget	segmentStartTimeScrollList;
Widget	segmentDurationScrollList;
Widget	segmentStopTimeScrollList;
Widget	segmentScrollBar;
Widget	totalDatatakeData;
Widget	nominalFrameData;
Widget	segmentFrameScrollList;
Widget	nominalSegmentData;
Widget	jobIDData;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  destroyCB_scanQcDialogShell( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	doExit();
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  popdownCB_scanQcDialogShell( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	doExit();
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_printScanQcWindow( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	print_cb(scanQcDialogShell);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_exitScanQc( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	hold_cb();
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_helpOnOverview( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	help_cb(0);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_helpOnUsingHelp( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	help_cb(1);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_helpOnProductInfo( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	help_cb(2);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_OKSelectFrame( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	{
	accept_cb ();
	}
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  valueChangedCB_frameScrollBar( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	moveFrameScrollBar();
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_frameCenterTimeScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickFrameListEntry(timeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_frameCenterLonScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickFrameListEntry(longitudeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  singleSelectionCB_frameCenterLatScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickFrameListEntry(latitudeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_frameCenterLatScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickFrameListEntry(latitudeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_frameFrameScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickFrameListEntry(frameList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_frameSegmentScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickFrameListEntry(segmentList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_backSelectFrame( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	{
	reject_cb();
	}
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  activateCB_cancelSelectFrame( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	{
	hold_cb();
	}
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_segmentSegmentScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickSegmentListEntry(segmentList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_segmentStartTimeScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickSegmentListEntry(longitudeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_segmentDurationScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickSegmentListEntry(latitudeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_segmentStopTimeScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickSegmentListEntry(timeList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  valueChangedCB_segmentScrollBar( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	moveSegmentScrollBar();
	UxScanQcDialogShellContext = UxSaveCtx;
}

static void  browseSelectionCB_segmentFrameScrollList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCscanQcDialogShell   *UxSaveCtx, *UxContext;

	UxSaveCtx = UxScanQcDialogShellContext;
	UxScanQcDialogShellContext = UxContext =
			(_UxCscanQcDialogShell *) UxGetContext( UxWidget );
	pickSegmentListEntry(frameList);
	UxScanQcDialogShellContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_scanQcDialogShell()
{
	Widget		_UxParent;
	Widget		file_shell;
	Widget		help_shell;


	/* Creation of scanQcDialogShell */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	scanQcDialogShell = XtVaCreatePopupShell( "scanQcDialogShell",
			xmDialogShellWidgetClass,
			_UxParent,
			XmNx, 0,
			XmNy, 0,
			XmNwidth, 700,
			XmNheight, 880,
			XmNbuttonFontList, UxConvertFontList("-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1" ),
			XmNlabelFontList, UxConvertFontList("-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1" ),
			XmNtextFontList, UxConvertFontList("-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1" ),
			XmNtitle, "CP_scan_qc",
			XmNdefaultFontList, UxConvertFontList("-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1" ),
			NULL );
	XtAddCallback( scanQcDialogShell, XmNdestroyCallback,
		(XtCallbackProc) destroyCB_scanQcDialogShell,
		(XtPointer) UxScanQcDialogShellContext );
	XtAddCallback( scanQcDialogShell, XmNpopdownCallback,
		(XtCallbackProc) popdownCB_scanQcDialogShell,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( scanQcDialogShell, (char *) UxScanQcDialogShellContext );
	UxPutClassCode( scanQcDialogShell, _UxIfClassId );


	/* Creation of scanQcMainWindow */
	scanQcMainWindow = XtVaCreateWidget( "scanQcMainWindow",
			xmMainWindowWidgetClass,
			scanQcDialogShell,
			XmNunitType, XmPIXELS,
			XmNx, 0,
			XmNy, 0,
			XmNwidth, 700,
			XmNheight, 880,
			NULL );
	UxPutContext( scanQcMainWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of scanQcMenu */
	scanQcMenu = XtVaCreateManagedWidget( "scanQcMenu",
			xmRowColumnWidgetClass,
			scanQcMainWindow,
			XmNrowColumnType, XmMENU_BAR,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( scanQcMenu, (char *) UxScanQcDialogShellContext );


	/* Creation of file */
	file_shell = XtVaCreatePopupShell ("file_shell",
			xmMenuShellWidgetClass, scanQcMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	file = XtVaCreateWidget( "file",
			xmRowColumnWidgetClass,
			file_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( file, (char *) UxScanQcDialogShellContext );


	/* Creation of printScanQcWindow */
	printScanQcWindow = XtVaCreateManagedWidget( "printScanQcWindow",
			xmPushButtonGadgetClass,
			file,
			RES_CONVERT( XmNlabelString, "Print" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( printScanQcWindow, XmNactivateCallback,
		(XtCallbackProc) activateCB_printScanQcWindow,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( printScanQcWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of exitScanQc */
	exitScanQc = XtVaCreateManagedWidget( "exitScanQc",
			xmPushButtonGadgetClass,
			file,
			RES_CONVERT( XmNlabelString, "Exit" ),
			RES_CONVERT( XmNmnemonic, "x" ),
			NULL );
	XtAddCallback( exitScanQc, XmNactivateCallback,
		(XtCallbackProc) activateCB_exitScanQc,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( exitScanQc, (char *) UxScanQcDialogShellContext );


	/* Creation of menu5_top_b1 */
	menu5_top_b1 = XtVaCreateManagedWidget( "menu5_top_b1",
			xmCascadeButtonWidgetClass,
			scanQcMenu,
			RES_CONVERT( XmNlabelString, "File" ),
			RES_CONVERT( XmNmnemonic, "F" ),
			XmNsubMenuId, file,
			NULL );
	UxPutContext( menu5_top_b1, (char *) UxScanQcDialogShellContext );


	/* Creation of help */
	help_shell = XtVaCreatePopupShell ("help_shell",
			xmMenuShellWidgetClass, scanQcMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	help = XtVaCreateWidget( "help",
			xmRowColumnWidgetClass,
			help_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( help, (char *) UxScanQcDialogShellContext );


	/* Creation of helpOnOverview */
	helpOnOverview = XtVaCreateManagedWidget( "helpOnOverview",
			xmPushButtonGadgetClass,
			help,
			RES_CONVERT( XmNlabelString, "Overview" ),
			RES_CONVERT( XmNmnemonic, "O" ),
			NULL );
	XtAddCallback( helpOnOverview, XmNactivateCallback,
		(XtCallbackProc) activateCB_helpOnOverview,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( helpOnOverview, (char *) UxScanQcDialogShellContext );


	/* Creation of helpOnUsingHelp */
	helpOnUsingHelp = XtVaCreateManagedWidget( "helpOnUsingHelp",
			xmPushButtonGadgetClass,
			help,
			RES_CONVERT( XmNlabelString, "Using Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			NULL );
	XtAddCallback( helpOnUsingHelp, XmNactivateCallback,
		(XtCallbackProc) activateCB_helpOnUsingHelp,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( helpOnUsingHelp, (char *) UxScanQcDialogShellContext );


	/* Creation of helpOnProductInfo */
	helpOnProductInfo = XtVaCreateManagedWidget( "helpOnProductInfo",
			xmPushButtonGadgetClass,
			help,
			RES_CONVERT( XmNlabelString, "Product Information" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( helpOnProductInfo, XmNactivateCallback,
		(XtCallbackProc) activateCB_helpOnProductInfo,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( helpOnProductInfo, (char *) UxScanQcDialogShellContext );


	/* Creation of menu5_top_b2 */
	menu5_top_b2 = XtVaCreateManagedWidget( "menu5_top_b2",
			xmCascadeButtonWidgetClass,
			scanQcMenu,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, help,
			NULL );
	UxPutContext( menu5_top_b2, (char *) UxScanQcDialogShellContext );


	/* Creation of scanQcBulletinBoard */
	scanQcBulletinBoard = XtVaCreateManagedWidget( "scanQcBulletinBoard",
			xmBulletinBoardWidgetClass,
			scanQcMainWindow,
			XmNwidth, 700,
			NULL );
	UxPutContext( scanQcBulletinBoard, (char *) UxScanQcDialogShellContext );


	/* Creation of platformLbl */
	platformLbl = XtVaCreateManagedWidget( "platformLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 60,
			XmNy, 10,
			XmNwidth, 90,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Platform:" ),
			NULL );
	UxPutContext( platformLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of platformData */
	platformData = XtVaCreateManagedWidget( "platformData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 150,
			XmNy, 10,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( platformData, (char *) UxScanQcDialogShellContext );


	/* Creation of stationLbl */
	stationLbl = XtVaCreateManagedWidget( "stationLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 500,
			XmNy, 40,
			XmNwidth, 80,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Station:" ),
			NULL );
	UxPutContext( stationLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of stationData */
	stationData = XtVaCreateManagedWidget( "stationData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 580,
			XmNy, 40,
			XmNwidth, 80,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( stationData, (char *) UxScanQcDialogShellContext );


	/* Creation of revolutionLbl */
	revolutionLbl = XtVaCreateManagedWidget( "revolutionLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 60,
			XmNy, 40,
			XmNwidth, 90,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Revolution:" ),
			NULL );
	UxPutContext( revolutionLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of revolutionData */
	revolutionData = XtVaCreateManagedWidget( "revolutionData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 150,
			XmNy, 40,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( revolutionData, (char *) UxScanQcDialogShellContext );


	/* Creation of instrumentModeLbl */
	instrumentModeLbl = XtVaCreateManagedWidget( "instrumentModeLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 280,
			XmNy, 10,
			XmNwidth, 80,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Mode:" ),
			NULL );
	UxPutContext( instrumentModeLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of instrumentModeData */
	instrumentModeData = XtVaCreateManagedWidget( "instrumentModeData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 360,
			XmNy, 10,
			XmNwidth, 80,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( instrumentModeData, (char *) UxScanQcDialogShellContext );


	/* Creation of sequenceLbl */
	sequenceLbl = XtVaCreateManagedWidget( "sequenceLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 280,
			XmNy, 40,
			XmNwidth, 80,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Sequence:" ),
			NULL );
	UxPutContext( sequenceLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of sequenceData */
	sequenceData = XtVaCreateManagedWidget( "sequenceData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 360,
			XmNy, 40,
			XmNwidth, 80,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "" ),
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( sequenceData, (char *) UxScanQcDialogShellContext );


	/* Creation of frameSizeData */
	frameSizeData = XtVaCreateManagedWidget( "frameSizeData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 150,
			XmNy, 70,
			XmNwidth, 50,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( frameSizeData, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentNbrLbl */
	segmentNbrLbl = XtVaCreateManagedWidget( "segmentNbrLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 60,
			XmNy, 122,
			XmNwidth, 200,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Number of Segments:" ),
			NULL );
	UxPutContext( segmentNbrLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentNbrData */
	segmentNbrData = XtVaCreateManagedWidget( "segmentNbrData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 260,
			XmNy, 122,
			XmNwidth, 60,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "" ),
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( segmentNbrData, (char *) UxScanQcDialogShellContext );


	/* Creation of segemntSeparator */
	segemntSeparator = XtVaCreateManagedWidget( "segemntSeparator",
			xmSeparatorGadgetClass,
			scanQcBulletinBoard,
			XmNx, 10,
			XmNy, 101,
			XmNwidth, 680,
			XmNheight, 10,
			NULL );
	UxPutContext( segemntSeparator, (char *) UxScanQcDialogShellContext );


	/* Creation of frameSeparator */
	frameSeparator = XtVaCreateManagedWidget( "frameSeparator",
			xmSeparatorGadgetClass,
			scanQcBulletinBoard,
			XmNx, 10,
			XmNy, 414,
			XmNwidth, 680,
			XmNheight, 10,
			NULL );
	UxPutContext( frameSeparator, (char *) UxScanQcDialogShellContext );


	/* Creation of OKSelectFrame */
	OKSelectFrame = XtVaCreateManagedWidget( "OKSelectFrame",
			xmPushButtonGadgetClass,
			scanQcBulletinBoard,
			XmNx, 90,
			XmNy, 770,
			XmNwidth, 100,
			XmNheight, 50,
			RES_CONVERT( XmNlabelString, "Accept" ),
			NULL );
	XtAddCallback( OKSelectFrame, XmNactivateCallback,
		(XtCallbackProc) activateCB_OKSelectFrame,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( OKSelectFrame, (char *) UxScanQcDialogShellContext );


	/* Creation of frameScrollBar */
	frameScrollBar = XtVaCreateManagedWidget( "frameScrollBar",
			xmScrollBarWidgetClass,
			scanQcBulletinBoard,
			XmNx, 639,
			XmNy, 533,
			XmNwidth, 20,
			XmNheight, 200,
			XmNshadowThickness, 3,
			NULL );
	XtAddCallback( frameScrollBar, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_frameScrollBar,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( frameScrollBar, (char *) UxScanQcDialogShellContext );


	/* Creation of centerTimeLbl */
	centerTimeLbl = XtVaCreateManagedWidget( "centerTimeLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 459,
			XmNy, 483,
			XmNwidth, 160,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Center Time" ),
			NULL );
	UxPutContext( centerTimeLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of frameCurrentTimeScrollWindow */
	frameCurrentTimeScrollWindow = XtVaCreateManagedWidget( "frameCurrentTimeScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 459,
			XmNy, 533,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNheight, 200,
			XmNwidth, 160,
			NULL );
	UxPutContext( frameCurrentTimeScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of frameCenterTimeScrollList */
	frameCenterTimeScrollList = XtVaCreateManagedWidget( "frameCenterTimeScrollList",
			xmListWidgetClass,
			frameCurrentTimeScrollWindow,
			XmNwidth, 180,
			XmNheight, 200,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 10,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( frameCenterTimeScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_frameCenterTimeScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( frameCenterTimeScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of frameCenterLonScrollWindow */
	frameCenterLonScrollWindow = XtVaCreateManagedWidget( "frameCenterLonScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 339,
			XmNy, 533,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNheight, 200,
			XmNwidth, 100,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			NULL );
	UxPutContext( frameCenterLonScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of frameCenterLonScrollList */
	frameCenterLonScrollList = XtVaCreateManagedWidget( "frameCenterLonScrollList",
			xmListWidgetClass,
			frameCenterLonScrollWindow,
			XmNwidth, 100,
			XmNheight, 200,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 10,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( frameCenterLonScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_frameCenterLonScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( frameCenterLonScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of frameCenterLatScrollWindow */
	frameCenterLatScrollWindow = XtVaCreateManagedWidget( "frameCenterLatScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 219,
			XmNy, 533,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNheight, 200,
			XmNwidth, 100,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			NULL );
	UxPutContext( frameCenterLatScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of frameCenterLatScrollList */
	frameCenterLatScrollList = XtVaCreateManagedWidget( "frameCenterLatScrollList",
			xmListWidgetClass,
			frameCenterLatScrollWindow,
			XmNwidth, 100,
			XmNheight, 200,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 10,
			XmNlistSizePolicy, XmCONSTANT,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( frameCenterLatScrollList, XmNsingleSelectionCallback,
		(XtCallbackProc) singleSelectionCB_frameCenterLatScrollList,
		(XtPointer) UxScanQcDialogShellContext );
	XtAddCallback( frameCenterLatScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_frameCenterLatScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( frameCenterLatScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of frameFrameScrollWindow */
	frameFrameScrollWindow = XtVaCreateManagedWidget( "frameFrameScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 139,
			XmNy, 533,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNheight, 200,
			XmNwidth, 60,
			NULL );
	UxPutContext( frameFrameScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of frameFrameScrollList */
	frameFrameScrollList = XtVaCreateManagedWidget( "frameFrameScrollList",
			xmListWidgetClass,
			frameFrameScrollWindow,
			XmNwidth, 60,
			XmNheight, 200,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 10,
			XmNlistSizePolicy, XmCONSTANT,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			NULL );
	XtAddCallback( frameFrameScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_frameFrameScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( frameFrameScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of frameSegmentScrollWindow */
	frameSegmentScrollWindow = XtVaCreateManagedWidget( "frameSegmentScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 59,
			XmNy, 533,
			XmNvisualPolicy, XmVARIABLE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNheight, 200,
			XmNwidth, 60,
			NULL );
	UxPutContext( frameSegmentScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of frameSegmentScrollList */
	frameSegmentScrollList = XtVaCreateManagedWidget( "frameSegmentScrollList",
			xmListWidgetClass,
			frameSegmentScrollWindow,
			XmNwidth, 60,
			XmNheight, 200,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 10,
			XmNlistSizePolicy, XmCONSTANT,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			NULL );
	XtAddCallback( frameSegmentScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_frameSegmentScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( frameSegmentScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of centerLogDegLbl */
	centerLogDegLbl = XtVaCreateManagedWidget( "centerLogDegLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 339,
			XmNy, 503,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "(deg)" ),
			NULL );
	UxPutContext( centerLogDegLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of centerLatDegLbl */
	centerLatDegLbl = XtVaCreateManagedWidget( "centerLatDegLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 219,
			XmNy, 503,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "(deg)" ),
			NULL );
	UxPutContext( centerLatDegLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of frameNbrLbl3 */
	frameNbrLbl3 = XtVaCreateManagedWidget( "frameNbrLbl3",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 119,
			XmNy, 483,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Frame" ),
			NULL );
	UxPutContext( frameNbrLbl3, (char *) UxScanQcDialogShellContext );


	/* Creation of centerLogLbl */
	centerLogLbl = XtVaCreateManagedWidget( "centerLogLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 329,
			XmNy, 483,
			XmNwidth, 120,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Center Longitude" ),
			NULL );
	UxPutContext( centerLogLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of centerLatLbl */
	centerLatLbl = XtVaCreateManagedWidget( "centerLatLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 209,
			XmNy, 483,
			XmNwidth, 120,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Center Latitude" ),
			NULL );
	UxPutContext( centerLatLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of centerTimeGMTLbl */
	centerTimeGMTLbl = XtVaCreateManagedWidget( "centerTimeGMTLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 489,
			XmNy, 503,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "(GMT)" ),
			NULL );
	UxPutContext( centerTimeGMTLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of backSelectFrame */
	backSelectFrame = XtVaCreateManagedWidget( "backSelectFrame",
			xmPushButtonGadgetClass,
			scanQcBulletinBoard,
			XmNx, 490,
			XmNy, 770,
			XmNwidth, 100,
			XmNheight, 50,
			RES_CONVERT( XmNlabelString, "Reject" ),
			NULL );
	XtAddCallback( backSelectFrame, XmNactivateCallback,
		(XtCallbackProc) activateCB_backSelectFrame,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( backSelectFrame, (char *) UxScanQcDialogShellContext );


	/* Creation of cancelSelectFrame */
	cancelSelectFrame = XtVaCreateManagedWidget( "cancelSelectFrame",
			xmPushButtonGadgetClass,
			scanQcBulletinBoard,
			XmNx, 290,
			XmNy, 770,
			XmNwidth, 100,
			XmNheight, 50,
			RES_CONVERT( XmNlabelString, "Hold" ),
			NULL );
	XtAddCallback( cancelSelectFrame, XmNactivateCallback,
		(XtCallbackProc) activateCB_cancelSelectFrame,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( cancelSelectFrame, (char *) UxScanQcDialogShellContext );


	/* Creation of commandSeparator */
	commandSeparator = XtVaCreateManagedWidget( "commandSeparator",
			xmSeparatorGadgetClass,
			scanQcBulletinBoard,
			XmNx, 10,
			XmNy, 747,
			XmNwidth, 680,
			XmNheight, 10,
			NULL );
	UxPutContext( commandSeparator, (char *) UxScanQcDialogShellContext );


	/* Creation of frameTableLbl */
	frameTableLbl = XtVaCreateManagedWidget( "frameTableLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 239,
			XmNy, 443,
			XmNwidth, 190,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Frame Table" ),
			XmNfontList, UxConvertFontList("-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( frameTableLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentSegmentScrollWindow */
	segmentSegmentScrollWindow = XtVaCreateManagedWidget( "segmentSegmentScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 58,
			XmNy, 294,
			XmNwidth, 60,
			XmNheight, 110,
			XmNshadowThickness, 0,
			XmNvisualPolicy, XmVARIABLE,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			XmNsensitive, TRUE,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	UxPutContext( segmentSegmentScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentSegmentScrollList */
	segmentSegmentScrollList = XtVaCreateManagedWidget( "segmentSegmentScrollList",
			xmListWidgetClass,
			segmentSegmentScrollWindow,
			XmNwidth, 60,
			XmNheight, 110,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 5,
			XmNlistSizePolicy, XmCONSTANT,
			XmNselectionPolicy, XmBROWSE_SELECT,
			XmNsensitive, TRUE,
			NULL );
	XtAddCallback( segmentSegmentScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_segmentSegmentScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( segmentSegmentScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentStartTimeScrollWindow */
	segmentStartTimeScrollWindow = XtVaCreateManagedWidget( "segmentStartTimeScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 298,
			XmNy, 294,
			XmNwidth, 150,
			XmNheight, 110,
			XmNvisualPolicy, XmVARIABLE,
			XmNshadowThickness, 0,
			NULL );
	UxPutContext( segmentStartTimeScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentStartTimeScrollList */
	segmentStartTimeScrollList = XtVaCreateManagedWidget( "segmentStartTimeScrollList",
			xmListWidgetClass,
			segmentStartTimeScrollWindow,
			XmNwidth, 180,
			XmNheight, 110,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 5,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( segmentStartTimeScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_segmentStartTimeScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( segmentStartTimeScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentDurationScrollWindow */
	segmentDurationScrollWindow = XtVaCreateManagedWidget( "segmentDurationScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 138,
			XmNy, 294,
			XmNwidth, 60,
			XmNheight, 110,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNvisualPolicy, XmVARIABLE,
			NULL );
	UxPutContext( segmentDurationScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentDurationScrollList */
	segmentDurationScrollList = XtVaCreateManagedWidget( "segmentDurationScrollList",
			xmListWidgetClass,
			segmentDurationScrollWindow,
			XmNwidth, 60,
			XmNheight, 110,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 5,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( segmentDurationScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_segmentDurationScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( segmentDurationScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentStopTimeScrollWindow */
	segmentStopTimeScrollWindow = XtVaCreateManagedWidget( "segmentStopTimeScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 468,
			XmNy, 294,
			XmNwidth, 150,
			XmNheight, 110,
			XmNvisualPolicy, XmVARIABLE,
			XmNshadowThickness, 0,
			NULL );
	UxPutContext( segmentStopTimeScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentStopTimeScrollList */
	segmentStopTimeScrollList = XtVaCreateManagedWidget( "segmentStopTimeScrollList",
			xmListWidgetClass,
			segmentStopTimeScrollWindow,
			XmNwidth, 180,
			XmNheight, 110,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 5,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( segmentStopTimeScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_segmentStopTimeScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( segmentStopTimeScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentScrollBar */
	segmentScrollBar = XtVaCreateManagedWidget( "segmentScrollBar",
			xmScrollBarWidgetClass,
			scanQcBulletinBoard,
			XmNx, 638,
			XmNy, 294,
			XmNwidth, 20,
			XmNheight, 102,
			XmNshadowThickness, 3,
			XmNsliderSize, 10,
			XmNmaximum, 50,
			XmNpageIncrement, 5,
			NULL );
	XtAddCallback( segmentScrollBar, XmNvalueChangedCallback,
		(XtCallbackProc) valueChangedCB_segmentScrollBar,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( segmentScrollBar, (char *) UxScanQcDialogShellContext );


	/* Creation of segmnetLbl2 */
	segmnetLbl2 = XtVaCreateManagedWidget( "segmnetLbl2",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 38,
			XmNy, 264,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Number" ),
			NULL );
	UxPutContext( segmnetLbl2, (char *) UxScanQcDialogShellContext );


	/* Creation of frameNbrLbl2 */
	frameNbrLbl2 = XtVaCreateManagedWidget( "frameNbrLbl2",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 198,
			XmNy, 264,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Frames" ),
			NULL );
	UxPutContext( frameNbrLbl2, (char *) UxScanQcDialogShellContext );


	/* Creation of durationSecLbl */
	durationSecLbl = XtVaCreateManagedWidget( "durationSecLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 113,
			XmNy, 264,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "(sec)" ),
			NULL );
	UxPutContext( durationSecLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of frameNbrLbl1 */
	frameNbrLbl1 = XtVaCreateManagedWidget( "frameNbrLbl1",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 198,
			XmNy, 244,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "# of" ),
			NULL );
	UxPutContext( frameNbrLbl1, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentLbl1 */
	segmentLbl1 = XtVaCreateManagedWidget( "segmentLbl1",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 38,
			XmNy, 244,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Segment" ),
			NULL );
	UxPutContext( segmentLbl1, (char *) UxScanQcDialogShellContext );


	/* Creation of durationLbl */
	durationLbl = XtVaCreateManagedWidget( "durationLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 118,
			XmNy, 244,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Duration" ),
			NULL );
	UxPutContext( durationLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of startTimeGMTLbl */
	startTimeGMTLbl = XtVaCreateManagedWidget( "startTimeGMTLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 323,
			XmNy, 264,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "(GMT)" ),
			NULL );
	UxPutContext( startTimeGMTLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of stopTimeLbl */
	stopTimeLbl = XtVaCreateManagedWidget( "stopTimeLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 498,
			XmNy, 244,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Stop Time" ),
			NULL );
	UxPutContext( stopTimeLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of startTimeLbl */
	startTimeLbl = XtVaCreateManagedWidget( "startTimeLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 323,
			XmNy, 244,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Start Time" ),
			NULL );
	UxPutContext( startTimeLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of stopTimeGMTLbl */
	stopTimeGMTLbl = XtVaCreateManagedWidget( "stopTimeGMTLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 498,
			XmNy, 264,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "(GMT)" ),
			NULL );
	UxPutContext( stopTimeGMTLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentTableLbl */
	segmentTableLbl = XtVaCreateManagedWidget( "segmentTableLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 248,
			XmNy, 204,
			XmNwidth, 190,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Segment Table" ),
			XmNfontList, UxConvertFontList("-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-1" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( segmentTableLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentLbl3 */
	segmentLbl3 = XtVaCreateManagedWidget( "segmentLbl3",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 39,
			XmNy, 483,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Segment" ),
			NULL );
	UxPutContext( segmentLbl3, (char *) UxScanQcDialogShellContext );


	/* Creation of segmnetLbl4 */
	segmnetLbl4 = XtVaCreateManagedWidget( "segmnetLbl4",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 39,
			XmNy, 503,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Number" ),
			NULL );
	UxPutContext( segmnetLbl4, (char *) UxScanQcDialogShellContext );


	/* Creation of frameNbrLbl4 */
	frameNbrLbl4 = XtVaCreateManagedWidget( "frameNbrLbl4",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 119,
			XmNy, 503,
			XmNwidth, 100,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Number" ),
			NULL );
	UxPutContext( frameNbrLbl4, (char *) UxScanQcDialogShellContext );


	/* Creation of totalDatatakeLbl */
	totalDatatakeLbl = XtVaCreateManagedWidget( "totalDatatakeLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 60,
			XmNy, 147,
			XmNwidth, 200,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Total Datatake Length (sec):" ),
			NULL );
	UxPutContext( totalDatatakeLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of totalDatatakeData */
	totalDatatakeData = XtVaCreateManagedWidget( "totalDatatakeData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 260,
			XmNy, 147,
			XmNwidth, 60,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "" ),
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( totalDatatakeData, (char *) UxScanQcDialogShellContext );


	/* Creation of nominalFrameLbl */
	nominalFrameLbl = XtVaCreateManagedWidget( "nominalFrameLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 360,
			XmNy, 122,
			XmNwidth, 220,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Nominal Frame Length (sec):" ),
			NULL );
	UxPutContext( nominalFrameLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of nominalFrameData */
	nominalFrameData = XtVaCreateManagedWidget( "nominalFrameData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 580,
			XmNy, 122,
			XmNwidth, 60,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "" ),
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( nominalFrameData, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentFrameScrollWindow */
	segmentFrameScrollWindow = XtVaCreateManagedWidget( "segmentFrameScrollWindow",
			xmScrolledWindowWidgetClass,
			scanQcBulletinBoard,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 218,
			XmNy, 294,
			XmNwidth, 60,
			XmNheight, 110,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNshadowThickness, 0,
			XmNvisualPolicy, XmVARIABLE,
			NULL );
	UxPutContext( segmentFrameScrollWindow, (char *) UxScanQcDialogShellContext );


	/* Creation of segmentFrameScrollList */
	segmentFrameScrollList = XtVaCreateManagedWidget( "segmentFrameScrollList",
			xmListWidgetClass,
			segmentFrameScrollWindow,
			XmNwidth, 60,
			XmNheight, 110,
			XmNshadowThickness, 3,
			XmNvisibleItemCount, 5,
			XmNstringDirection, XmSTRING_DIRECTION_R_TO_L,
			XmNlistSizePolicy, XmCONSTANT,
			NULL );
	XtAddCallback( segmentFrameScrollList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_segmentFrameScrollList,
		(XtPointer) UxScanQcDialogShellContext );

	UxPutContext( segmentFrameScrollList, (char *) UxScanQcDialogShellContext );


	/* Creation of segemntSeparator1 */
	segemntSeparator1 = XtVaCreateManagedWidget( "segemntSeparator1",
			xmSeparatorGadgetClass,
			scanQcBulletinBoard,
			XmNx, 10,
			XmNy, 184,
			XmNwidth, 680,
			XmNheight, 10,
			NULL );
	UxPutContext( segemntSeparator1, (char *) UxScanQcDialogShellContext );


	/* Creation of nominalSegmentLbl */
	nominalSegmentLbl = XtVaCreateManagedWidget( "nominalSegmentLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 360,
			XmNy, 147,
			XmNwidth, 220,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Nominal Segment Length (sec):" ),
			NULL );
	UxPutContext( nominalSegmentLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of nominalSegmentData */
	nominalSegmentData = XtVaCreateManagedWidget( "nominalSegmentData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 580,
			XmNy, 147,
			XmNwidth, 60,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "" ),
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( nominalSegmentData, (char *) UxScanQcDialogShellContext );


	/* Creation of jobIDLbl */
	jobIDLbl = XtVaCreateManagedWidget( "jobIDLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 500,
			XmNy, 10,
			XmNwidth, 80,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Job ID:" ),
			NULL );
	UxPutContext( jobIDLbl, (char *) UxScanQcDialogShellContext );


	/* Creation of jobIDData */
	jobIDData = XtVaCreateManagedWidget( "jobIDData",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 580,
			XmNy, 10,
			XmNwidth, 80,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNmarginHeight, 8,
			NULL );
	UxPutContext( jobIDData, (char *) UxScanQcDialogShellContext );


	/* Creation of frameSizeKM */
	frameSizeKM = XtVaCreateManagedWidget( "frameSizeKM",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 200,
			XmNy, 70,
			XmNwidth, 40,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "(km)" ),
			NULL );
	UxPutContext( frameSizeKM, (char *) UxScanQcDialogShellContext );


	/* Creation of frameSizeLbl */
	frameSizeLbl = XtVaCreateManagedWidget( "frameSizeLbl",
			xmLabelGadgetClass,
			scanQcBulletinBoard,
			XmNx, 60,
			XmNy, 70,
			XmNwidth, 90,
			XmNheight, 30,
			XmNalignment, XmALIGNMENT_BEGINNING,
			RES_CONVERT( XmNlabelString, "Frame Size:" ),
			NULL );
	UxPutContext( frameSizeLbl, (char *) UxScanQcDialogShellContext );

	XtVaSetValues(scanQcMenu,
			XmNmenuHelpWidget, menu5_top_b2,
			NULL );


	XtAddCallback( scanQcDialogShell, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxScanQcDialogShellContext);

	XmMainWindowSetAreas( scanQcMainWindow, scanQcMenu, (Widget) NULL,
			(Widget) NULL, (Widget) NULL, scanQcBulletinBoard );

	return ( scanQcDialogShell );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_scanQcDialogShell( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCscanQcDialogShell   *UxContext;
	static int		_Uxinit = 0;

	UxScanQcDialogShellContext = UxContext =
		(_UxCscanQcDialogShell *) UxNewContext( sizeof(_UxCscanQcDialogShell), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_scanQcDialogShell();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

