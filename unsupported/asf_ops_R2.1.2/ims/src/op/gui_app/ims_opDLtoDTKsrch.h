
/*******************************************************************************
       ims_opDLtoDTKsrch.h
       This header file is included by ims_opDLtoDTKsrch.c

*******************************************************************************/

#ifndef	_IMS_OPDLTODTKSRCH_INCLUDED
#define	_IMS_OPDLTODTKSRCH_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
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
	Widget	UxDLtoDTKsearch;
	Widget	UxmenuBar1;
	Widget	UxmenuBar_p1;
	Widget	UxsrchWelcomeMPB;
	Widget	UxmenuBar_p1_b2;
	Widget	UxgotoDL2DTK_MPB;
	Widget	UxmenuBar_p1_b4;
	Widget	UxcloseScreenMPB;
	Widget	UxmenuBar_top_b1;
	Widget	UxmenuBar1_p2;
	Widget	UxsrchExecuteSearchMPB;
	Widget	UxmenuBar1_p2_b7;
	Widget	UxsrchClearSearchMPB;
	Widget	UxmenuBar1_p2_b8;
	Widget	UxsrchPrintScreenMPB;
	Widget	UxmenuBar1_top_b1;
	Widget	UxmenuBar1_p3;
	Widget	UxmenuBar1_p3_b1;
	Widget	UxmenuBar1_top_b2;
	Widget	UxdlSearchPlatformLBL;
	Widget	UxdlSearchSensorLBL;
	Widget	UxdlSearchActivityLBL;
	Widget	UxdlSearchStationLBL;
	Widget	Uxdk2dtkAntennaLBL;
	Widget	UxdlSearchTransmitterLBL;
	Widget	UxdlSearchPlatformSW;
	Widget	UxplatformRC;
	Widget	UxdummyTB;
	Widget	UxdlSearchSensorSW;
	Widget	UxsensorRC;
	Widget	UxdlSearchActivitySW;
	Widget	UxactivityRC;
	Widget	UxdlSearchStationSW;
	Widget	UxstationRC;
	Widget	UxdlSearchAntennaSW;
	Widget	UxantennaRC;
	Widget	UxdlSearchTransmitterSW;
	Widget	UxtransmitterRC;
	Widget	UxdlSearchSeparator1;
	Widget	UxdlSearchExecuteSearchPB;
	Widget	UxdlSearchClearSearchPB;
	Widget	UxdlSearchCloseSearchPB;
	Widget	UxdlSearchPrintSearchPB;
	Widget	UxDownlinkSearchLBL;
	Widget	UxdlSearchTimeOnFrame;
	Widget	UxdlSearchTimeOnForm;
	Widget	UxdlSearchTimeOnStartLBL;
	Widget	UxdlSearchTimeOnEndLBL;
	Widget	UxdlSearchTimeOnStartText;
	Widget	UxdlSearchTimeOnEndText;
	Widget	UxdlSearchTimeOnLBL;
	Widget	UxdlSearchTimeOffLBL;
	Widget	UxdlSearchTimeOffFrame;
	Widget	UxdlSearchTimeOffForm;
	Widget	UxdlSearchTimeOffStartLBL;
	Widget	UxdlSearchTimeOffEndLBL;
	Widget	UxdlSearchTimeOffStartText;
	Widget	UxdlSearchTimeOffEndText;
	Widget	Uxframe2;
	Widget	Uxlabel2;
	Widget	UxdlSearchSeparator2;
	Widget	UxdlSearchScheduleLinkLBL;
	Widget	UxdlSearchScheduleLinkTF;
	Widget	UxdlSearchRevolutionLBL;
	Widget	UxdlSearchRevFrame;
	Widget	UxdlSearchRevForm;
	Widget	UxdlSearchRevStartLBL;
	Widget	UxdlSearchRevEndLBL;
	Widget	UxdlSearchRevStartText;
	Widget	UxdlSearchRevEndText;
	Widget	UxdlSearchSequenceLBL;
	Widget	UxdlSearchSequenceFrame;
	Widget	UxdlSearchSequenceForm;
	Widget	UxdlSearchSequenceStartLBL;
	Widget	UxdlSearchSequenceEndLBL;
	Widget	UxdlSearchSequenceStartText;
	Widget	UxdlSearchSequenceEndText;
	Widget	UxdlSearchStatusLBL;
	Widget	UxdlSearchStatusSW;
	Widget	UxstatusRC;
	swidget	UxUxParent;
} _UxCDLtoDTKsearch;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCDLtoDTKsearch       *UxDLtoDTKsearchContext;
#define DLtoDTKsearch           UxDLtoDTKsearchContext->UxDLtoDTKsearch
#define menuBar1                UxDLtoDTKsearchContext->UxmenuBar1
#define menuBar_p1              UxDLtoDTKsearchContext->UxmenuBar_p1
#define srchWelcomeMPB          UxDLtoDTKsearchContext->UxsrchWelcomeMPB
#define menuBar_p1_b2           UxDLtoDTKsearchContext->UxmenuBar_p1_b2
#define gotoDL2DTK_MPB          UxDLtoDTKsearchContext->UxgotoDL2DTK_MPB
#define menuBar_p1_b4           UxDLtoDTKsearchContext->UxmenuBar_p1_b4
#define closeScreenMPB          UxDLtoDTKsearchContext->UxcloseScreenMPB
#define menuBar_top_b1          UxDLtoDTKsearchContext->UxmenuBar_top_b1
#define menuBar1_p2             UxDLtoDTKsearchContext->UxmenuBar1_p2
#define srchExecuteSearchMPB    UxDLtoDTKsearchContext->UxsrchExecuteSearchMPB
#define menuBar1_p2_b7          UxDLtoDTKsearchContext->UxmenuBar1_p2_b7
#define srchClearSearchMPB      UxDLtoDTKsearchContext->UxsrchClearSearchMPB
#define menuBar1_p2_b8          UxDLtoDTKsearchContext->UxmenuBar1_p2_b8
#define srchPrintScreenMPB      UxDLtoDTKsearchContext->UxsrchPrintScreenMPB
#define menuBar1_top_b1         UxDLtoDTKsearchContext->UxmenuBar1_top_b1
#define menuBar1_p3             UxDLtoDTKsearchContext->UxmenuBar1_p3
#define menuBar1_p3_b1          UxDLtoDTKsearchContext->UxmenuBar1_p3_b1
#define menuBar1_top_b2         UxDLtoDTKsearchContext->UxmenuBar1_top_b2
#define dlSearchPlatformLBL     UxDLtoDTKsearchContext->UxdlSearchPlatformLBL
#define dlSearchSensorLBL       UxDLtoDTKsearchContext->UxdlSearchSensorLBL
#define dlSearchActivityLBL     UxDLtoDTKsearchContext->UxdlSearchActivityLBL
#define dlSearchStationLBL      UxDLtoDTKsearchContext->UxdlSearchStationLBL
#define dk2dtkAntennaLBL        UxDLtoDTKsearchContext->Uxdk2dtkAntennaLBL
#define dlSearchTransmitterLBL  UxDLtoDTKsearchContext->UxdlSearchTransmitterLBL
#define dlSearchPlatformSW      UxDLtoDTKsearchContext->UxdlSearchPlatformSW
#define platformRC              UxDLtoDTKsearchContext->UxplatformRC
#define dummyTB                 UxDLtoDTKsearchContext->UxdummyTB
#define dlSearchSensorSW        UxDLtoDTKsearchContext->UxdlSearchSensorSW
#define sensorRC                UxDLtoDTKsearchContext->UxsensorRC
#define dlSearchActivitySW      UxDLtoDTKsearchContext->UxdlSearchActivitySW
#define activityRC              UxDLtoDTKsearchContext->UxactivityRC
#define dlSearchStationSW       UxDLtoDTKsearchContext->UxdlSearchStationSW
#define stationRC               UxDLtoDTKsearchContext->UxstationRC
#define dlSearchAntennaSW       UxDLtoDTKsearchContext->UxdlSearchAntennaSW
#define antennaRC               UxDLtoDTKsearchContext->UxantennaRC
#define dlSearchTransmitterSW   UxDLtoDTKsearchContext->UxdlSearchTransmitterSW
#define transmitterRC           UxDLtoDTKsearchContext->UxtransmitterRC
#define dlSearchSeparator1      UxDLtoDTKsearchContext->UxdlSearchSeparator1
#define dlSearchExecuteSearchPB UxDLtoDTKsearchContext->UxdlSearchExecuteSearchPB
#define dlSearchClearSearchPB   UxDLtoDTKsearchContext->UxdlSearchClearSearchPB
#define dlSearchCloseSearchPB   UxDLtoDTKsearchContext->UxdlSearchCloseSearchPB
#define dlSearchPrintSearchPB   UxDLtoDTKsearchContext->UxdlSearchPrintSearchPB
#define DownlinkSearchLBL       UxDLtoDTKsearchContext->UxDownlinkSearchLBL
#define dlSearchTimeOnFrame     UxDLtoDTKsearchContext->UxdlSearchTimeOnFrame
#define dlSearchTimeOnForm      UxDLtoDTKsearchContext->UxdlSearchTimeOnForm
#define dlSearchTimeOnStartLBL  UxDLtoDTKsearchContext->UxdlSearchTimeOnStartLBL
#define dlSearchTimeOnEndLBL    UxDLtoDTKsearchContext->UxdlSearchTimeOnEndLBL
#define dlSearchTimeOnStartText UxDLtoDTKsearchContext->UxdlSearchTimeOnStartText
#define dlSearchTimeOnEndText   UxDLtoDTKsearchContext->UxdlSearchTimeOnEndText
#define dlSearchTimeOnLBL       UxDLtoDTKsearchContext->UxdlSearchTimeOnLBL
#define dlSearchTimeOffLBL      UxDLtoDTKsearchContext->UxdlSearchTimeOffLBL
#define dlSearchTimeOffFrame    UxDLtoDTKsearchContext->UxdlSearchTimeOffFrame
#define dlSearchTimeOffForm     UxDLtoDTKsearchContext->UxdlSearchTimeOffForm
#define dlSearchTimeOffStartLBL UxDLtoDTKsearchContext->UxdlSearchTimeOffStartLBL
#define dlSearchTimeOffEndLBL   UxDLtoDTKsearchContext->UxdlSearchTimeOffEndLBL
#define dlSearchTimeOffStartText UxDLtoDTKsearchContext->UxdlSearchTimeOffStartText
#define dlSearchTimeOffEndText  UxDLtoDTKsearchContext->UxdlSearchTimeOffEndText
#define frame2                  UxDLtoDTKsearchContext->Uxframe2
#define label2                  UxDLtoDTKsearchContext->Uxlabel2
#define dlSearchSeparator2      UxDLtoDTKsearchContext->UxdlSearchSeparator2
#define dlSearchScheduleLinkLBL UxDLtoDTKsearchContext->UxdlSearchScheduleLinkLBL
#define dlSearchScheduleLinkTF  UxDLtoDTKsearchContext->UxdlSearchScheduleLinkTF
#define dlSearchRevolutionLBL   UxDLtoDTKsearchContext->UxdlSearchRevolutionLBL
#define dlSearchRevFrame        UxDLtoDTKsearchContext->UxdlSearchRevFrame
#define dlSearchRevForm         UxDLtoDTKsearchContext->UxdlSearchRevForm
#define dlSearchRevStartLBL     UxDLtoDTKsearchContext->UxdlSearchRevStartLBL
#define dlSearchRevEndLBL       UxDLtoDTKsearchContext->UxdlSearchRevEndLBL
#define dlSearchRevStartText    UxDLtoDTKsearchContext->UxdlSearchRevStartText
#define dlSearchRevEndText      UxDLtoDTKsearchContext->UxdlSearchRevEndText
#define dlSearchSequenceLBL     UxDLtoDTKsearchContext->UxdlSearchSequenceLBL
#define dlSearchSequenceFrame   UxDLtoDTKsearchContext->UxdlSearchSequenceFrame
#define dlSearchSequenceForm    UxDLtoDTKsearchContext->UxdlSearchSequenceForm
#define dlSearchSequenceStartLBL UxDLtoDTKsearchContext->UxdlSearchSequenceStartLBL
#define dlSearchSequenceEndLBL  UxDLtoDTKsearchContext->UxdlSearchSequenceEndLBL
#define dlSearchSequenceStartText UxDLtoDTKsearchContext->UxdlSearchSequenceStartText
#define dlSearchSequenceEndText UxDLtoDTKsearchContext->UxdlSearchSequenceEndText
#define dlSearchStatusLBL       UxDLtoDTKsearchContext->UxdlSearchStatusLBL
#define dlSearchStatusSW        UxDLtoDTKsearchContext->UxdlSearchStatusSW
#define statusRC                UxDLtoDTKsearchContext->UxstatusRC
#define UxParent                UxDLtoDTKsearchContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_DLtoDTKsearch( swidget _UxUxParent );

#endif	/* _IMS_OPDLTODTKSRCH_INCLUDED */
