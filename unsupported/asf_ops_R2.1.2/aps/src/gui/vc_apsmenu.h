
/*******************************************************************************
       vc_apsmenu.h
       This header file is included by vc_apsmenu.c

*******************************************************************************/

#ifndef	_VC_APSMENU_INCLUDED
#define	_VC_APSMENU_INCLUDED

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
	Widget	UxapplMainWin1;
	Widget	UxapplMenuBar1;
	Widget	UxFile_applMenuBar;
	Widget	UxFileGeneration_pb;
	Widget	UxFileProcessing_pb;
	Widget	Uxseparator2;
	Widget	UxExitAPS_pb;
	Widget	Uxfile_pane1;
	Widget	UxCoverage_applMenuBar;
	Widget	UxCreateNominalOrbit_pb;
	Widget	UxCreateNominalCvrg_pb;
	Widget	UxapplMenuBar1_top_b1;
	Widget	UxPlanning_applMenuBar;
	Widget	UxDARManager_pb;
	Widget	UxSiteCoverage_pb;
	Widget	UxMapper_pb;
	Widget	UxDTKManager_pb;
	Widget	UxapplMenuBar1_top_b2;
	Widget	UxDowntime_applMenuBar;
	Widget	UxRGSDowntimes_pb;
	Widget	UxAntennaDowntimes_pb;
	Widget	UxapplMenuBar1_top_b3;
	Widget	UxTools_applMenuBar;
	Widget	UxMUPermissionStatus_pb;
	Widget	UxAPSWOSCompare_pb;
	Widget	UxpushButton_CON_ROUNDUP;
	Widget	UxAPSPhaseSelection_pb;
	Widget	UxapplMenuBar1_top_b4;
	Widget	UxapplForm1;
	swidget	UxUxParent;
} _UxCAPSMainMenu;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCAPSMainMenu         *UxAPSMainMenuContext;
#define applMainWin1            UxAPSMainMenuContext->UxapplMainWin1
#define applMenuBar1            UxAPSMainMenuContext->UxapplMenuBar1
#define File_applMenuBar        UxAPSMainMenuContext->UxFile_applMenuBar
#define FileGeneration_pb       UxAPSMainMenuContext->UxFileGeneration_pb
#define FileProcessing_pb       UxAPSMainMenuContext->UxFileProcessing_pb
#define separator2              UxAPSMainMenuContext->Uxseparator2
#define ExitAPS_pb              UxAPSMainMenuContext->UxExitAPS_pb
#define file_pane1              UxAPSMainMenuContext->Uxfile_pane1
#define Coverage_applMenuBar    UxAPSMainMenuContext->UxCoverage_applMenuBar
#define CreateNominalOrbit_pb   UxAPSMainMenuContext->UxCreateNominalOrbit_pb
#define CreateNominalCvrg_pb    UxAPSMainMenuContext->UxCreateNominalCvrg_pb
#define applMenuBar1_top_b1     UxAPSMainMenuContext->UxapplMenuBar1_top_b1
#define Planning_applMenuBar    UxAPSMainMenuContext->UxPlanning_applMenuBar
#define DARManager_pb           UxAPSMainMenuContext->UxDARManager_pb
#define SiteCoverage_pb         UxAPSMainMenuContext->UxSiteCoverage_pb
#define Mapper_pb               UxAPSMainMenuContext->UxMapper_pb
#define DTKManager_pb           UxAPSMainMenuContext->UxDTKManager_pb
#define applMenuBar1_top_b2     UxAPSMainMenuContext->UxapplMenuBar1_top_b2
#define Downtime_applMenuBar    UxAPSMainMenuContext->UxDowntime_applMenuBar
#define RGSDowntimes_pb         UxAPSMainMenuContext->UxRGSDowntimes_pb
#define AntennaDowntimes_pb     UxAPSMainMenuContext->UxAntennaDowntimes_pb
#define applMenuBar1_top_b3     UxAPSMainMenuContext->UxapplMenuBar1_top_b3
#define Tools_applMenuBar       UxAPSMainMenuContext->UxTools_applMenuBar
#define MUPermissionStatus_pb   UxAPSMainMenuContext->UxMUPermissionStatus_pb
#define APSWOSCompare_pb        UxAPSMainMenuContext->UxAPSWOSCompare_pb
#define pushButton_CON_ROUNDUP  UxAPSMainMenuContext->UxpushButton_CON_ROUNDUP
#define APSPhaseSelection_pb    UxAPSMainMenuContext->UxAPSPhaseSelection_pb
#define applMenuBar1_top_b4     UxAPSMainMenuContext->UxapplMenuBar1_top_b4
#define applForm1               UxAPSMainMenuContext->UxapplForm1
#define UxParent                UxAPSMainMenuContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	APSMainMenu;
extern Widget	Pixmap_applWorkArea;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_APSMainMenu( swidget _UxUxParent );

#endif	/* _VC_APSMENU_INCLUDED */
