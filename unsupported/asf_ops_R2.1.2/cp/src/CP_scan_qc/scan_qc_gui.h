
/*******************************************************************************
       scan_qc_gui.h
       (Generated from interface file scan_qc_gui.i)
       This header file is included by scan_qc_gui.c

*******************************************************************************/

#ifndef	_SCAN_QC_GUI_INCLUDED
#define	_SCAN_QC_GUI_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	Widget	UxscanQcMenu;
	Widget	Uxfile;
	Widget	UxprintScanQcWindow;
	Widget	UxexitScanQc;
	Widget	Uxmenu5_top_b1;
	Widget	Uxhelp;
	Widget	UxhelpOnOverview;
	Widget	UxhelpOnUsingHelp;
	Widget	UxhelpOnProductInfo;
	Widget	Uxmenu5_top_b2;
	Widget	UxscanQcBulletinBoard;
	Widget	UxplatformLbl;
	Widget	UxstationLbl;
	Widget	UxrevolutionLbl;
	Widget	UxinstrumentModeLbl;
	Widget	UxsequenceLbl;
	Widget	UxsegmentNbrLbl;
	Widget	UxsegemntSeparator;
	Widget	UxframeSeparator;
	Widget	UxcenterTimeLbl;
	Widget	UxframeCurrentTimeScrollWindow;
	Widget	UxframeCenterLonScrollWindow;
	Widget	UxframeCenterLatScrollWindow;
	Widget	UxframeFrameScrollWindow;
	Widget	UxframeSegmentScrollWindow;
	Widget	UxcenterLogDegLbl;
	Widget	UxcenterLatDegLbl;
	Widget	UxframeNbrLbl3;
	Widget	UxcenterLogLbl;
	Widget	UxcenterLatLbl;
	Widget	UxcenterTimeGMTLbl;
	Widget	UxcancelSelectFrame;
	Widget	UxcommandSeparator;
	Widget	UxframeTableLbl;
	Widget	UxsegmentSegmentScrollWindow;
	Widget	UxsegmentStartTimeScrollWindow;
	Widget	UxsegmentDurationScrollWindow;
	Widget	UxsegmentStopTimeScrollWindow;
	Widget	UxsegmnetLbl2;
	Widget	UxframeNbrLbl2;
	Widget	UxdurationSecLbl;
	Widget	UxframeNbrLbl1;
	Widget	UxsegmentLbl1;
	Widget	UxdurationLbl;
	Widget	UxstartTimeGMTLbl;
	Widget	UxstopTimeLbl;
	Widget	UxstartTimeLbl;
	Widget	UxstopTimeGMTLbl;
	Widget	UxsegmentTableLbl;
	Widget	UxsegmentLbl3;
	Widget	UxsegmnetLbl4;
	Widget	UxframeNbrLbl4;
	Widget	UxtotalDatatakeLbl;
	Widget	UxnominalFrameLbl;
	Widget	UxsegmentFrameScrollWindow;
	Widget	UxsegemntSeparator1;
	Widget	UxnominalSegmentLbl;
	Widget	UxjobIDLbl;
	Widget	UxframeSizeKM;
	Widget	UxframeSizeLbl;
	swidget	UxUxParent;
} _UxCscanQcDialogShell;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCscanQcDialogShell   *UxScanQcDialogShellContext;
#define scanQcMenu              UxScanQcDialogShellContext->UxscanQcMenu
#define file                    UxScanQcDialogShellContext->Uxfile
#define printScanQcWindow       UxScanQcDialogShellContext->UxprintScanQcWindow
#define exitScanQc              UxScanQcDialogShellContext->UxexitScanQc
#define menu5_top_b1            UxScanQcDialogShellContext->Uxmenu5_top_b1
#define help                    UxScanQcDialogShellContext->Uxhelp
#define helpOnOverview          UxScanQcDialogShellContext->UxhelpOnOverview
#define helpOnUsingHelp         UxScanQcDialogShellContext->UxhelpOnUsingHelp
#define helpOnProductInfo       UxScanQcDialogShellContext->UxhelpOnProductInfo
#define menu5_top_b2            UxScanQcDialogShellContext->Uxmenu5_top_b2
#define scanQcBulletinBoard     UxScanQcDialogShellContext->UxscanQcBulletinBoard
#define platformLbl             UxScanQcDialogShellContext->UxplatformLbl
#define stationLbl              UxScanQcDialogShellContext->UxstationLbl
#define revolutionLbl           UxScanQcDialogShellContext->UxrevolutionLbl
#define instrumentModeLbl       UxScanQcDialogShellContext->UxinstrumentModeLbl
#define sequenceLbl             UxScanQcDialogShellContext->UxsequenceLbl
#define segmentNbrLbl           UxScanQcDialogShellContext->UxsegmentNbrLbl
#define segemntSeparator        UxScanQcDialogShellContext->UxsegemntSeparator
#define frameSeparator          UxScanQcDialogShellContext->UxframeSeparator
#define centerTimeLbl           UxScanQcDialogShellContext->UxcenterTimeLbl
#define frameCurrentTimeScrollWindow UxScanQcDialogShellContext->UxframeCurrentTimeScrollWindow
#define frameCenterLonScrollWindow UxScanQcDialogShellContext->UxframeCenterLonScrollWindow
#define frameCenterLatScrollWindow UxScanQcDialogShellContext->UxframeCenterLatScrollWindow
#define frameFrameScrollWindow  UxScanQcDialogShellContext->UxframeFrameScrollWindow
#define frameSegmentScrollWindow UxScanQcDialogShellContext->UxframeSegmentScrollWindow
#define centerLogDegLbl         UxScanQcDialogShellContext->UxcenterLogDegLbl
#define centerLatDegLbl         UxScanQcDialogShellContext->UxcenterLatDegLbl
#define frameNbrLbl3            UxScanQcDialogShellContext->UxframeNbrLbl3
#define centerLogLbl            UxScanQcDialogShellContext->UxcenterLogLbl
#define centerLatLbl            UxScanQcDialogShellContext->UxcenterLatLbl
#define centerTimeGMTLbl        UxScanQcDialogShellContext->UxcenterTimeGMTLbl
#define cancelSelectFrame       UxScanQcDialogShellContext->UxcancelSelectFrame
#define commandSeparator        UxScanQcDialogShellContext->UxcommandSeparator
#define frameTableLbl           UxScanQcDialogShellContext->UxframeTableLbl
#define segmentSegmentScrollWindow UxScanQcDialogShellContext->UxsegmentSegmentScrollWindow
#define segmentStartTimeScrollWindow UxScanQcDialogShellContext->UxsegmentStartTimeScrollWindow
#define segmentDurationScrollWindow UxScanQcDialogShellContext->UxsegmentDurationScrollWindow
#define segmentStopTimeScrollWindow UxScanQcDialogShellContext->UxsegmentStopTimeScrollWindow
#define segmnetLbl2             UxScanQcDialogShellContext->UxsegmnetLbl2
#define frameNbrLbl2            UxScanQcDialogShellContext->UxframeNbrLbl2
#define durationSecLbl          UxScanQcDialogShellContext->UxdurationSecLbl
#define frameNbrLbl1            UxScanQcDialogShellContext->UxframeNbrLbl1
#define segmentLbl1             UxScanQcDialogShellContext->UxsegmentLbl1
#define durationLbl             UxScanQcDialogShellContext->UxdurationLbl
#define startTimeGMTLbl         UxScanQcDialogShellContext->UxstartTimeGMTLbl
#define stopTimeLbl             UxScanQcDialogShellContext->UxstopTimeLbl
#define startTimeLbl            UxScanQcDialogShellContext->UxstartTimeLbl
#define stopTimeGMTLbl          UxScanQcDialogShellContext->UxstopTimeGMTLbl
#define segmentTableLbl         UxScanQcDialogShellContext->UxsegmentTableLbl
#define segmentLbl3             UxScanQcDialogShellContext->UxsegmentLbl3
#define segmnetLbl4             UxScanQcDialogShellContext->UxsegmnetLbl4
#define frameNbrLbl4            UxScanQcDialogShellContext->UxframeNbrLbl4
#define totalDatatakeLbl        UxScanQcDialogShellContext->UxtotalDatatakeLbl
#define nominalFrameLbl         UxScanQcDialogShellContext->UxnominalFrameLbl
#define segmentFrameScrollWindow UxScanQcDialogShellContext->UxsegmentFrameScrollWindow
#define segemntSeparator1       UxScanQcDialogShellContext->UxsegemntSeparator1
#define nominalSegmentLbl       UxScanQcDialogShellContext->UxnominalSegmentLbl
#define jobIDLbl                UxScanQcDialogShellContext->UxjobIDLbl
#define frameSizeKM             UxScanQcDialogShellContext->UxframeSizeKM
#define frameSizeLbl            UxScanQcDialogShellContext->UxframeSizeLbl
#define UxParent                UxScanQcDialogShellContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	scanQcDialogShell;
extern Widget	scanQcMainWindow;
extern Widget	platformData;
extern Widget	stationData;
extern Widget	revolutionData;
extern Widget	instrumentModeData;
extern Widget	sequenceData;
extern Widget	frameSizeData;
extern Widget	segmentNbrData;
extern Widget	OKSelectFrame;
extern Widget	frameScrollBar;
extern Widget	frameCenterTimeScrollList;
extern Widget	frameCenterLonScrollList;
extern Widget	frameCenterLatScrollList;
extern Widget	frameFrameScrollList;
extern Widget	frameSegmentScrollList;
extern Widget	backSelectFrame;
extern Widget	segmentSegmentScrollList;
extern Widget	segmentStartTimeScrollList;
extern Widget	segmentDurationScrollList;
extern Widget	segmentStopTimeScrollList;
extern Widget	segmentScrollBar;
extern Widget	totalDatatakeData;
extern Widget	nominalFrameData;
extern Widget	segmentFrameScrollList;
extern Widget	nominalSegmentData;
extern Widget	jobIDData;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_scanQcDialogShell();

#endif	/* _SCAN_QC_GUI_INCLUDED */
