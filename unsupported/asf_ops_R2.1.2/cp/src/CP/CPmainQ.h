
/*******************************************************************************
       CPmainQ.h
       (Generated from interface file CPmainQ.i)
       This header file is included by CPmainQ.c

*******************************************************************************/

#ifndef	_CPMAINQ_INCLUDED
#define	_CPMAINQ_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
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
	Widget	UxCPmainMenu;
	Widget	UxFile_pane;
	Widget	UxFileOpen_pb;
	Widget	UxFileSave_pb;
	Widget	UxFilePrint_pb;
	Widget	UxFile_sep1;
	Widget	UxFileExit_pb;
	Widget	UxFile_cb;
	Widget	UxTools_pane;
	Widget	UxStart_GPR_button;
	Widget	UxCPlogBrowser_info_pb;
	Widget	UxCPlogBrowser_debug_pb;
	Widget	UxCPlogBrowser_error_pb;
	Widget	UxCP_logBrowser_cb;
	Widget	UxSPSlogBrowser_info_pb;
	Widget	UxSPSlogBrowser_debug_pb;
	Widget	UxSPSlogBrowser_error_pb;
	Widget	UxSPS_logBrowser_cb;
	Widget	UxTools_cb;
	Widget	UxSubsystems_pane;
	Widget	UxSub_StartAll_pb;
	Widget	UxSub_ReadyAll_pb;
	Widget	UxSub_sep1;
	Widget	UxSub_Start_pb;
	Widget	UxSub_Ready_pb;
	Widget	UxSub_Reset_pb;
	Widget	UxSub_Pause;
	Widget	UxSub_Shutdown_pb;
	Widget	UxSub_sep2;
	Widget	UxSub_Raise;
	Widget	UxSubsystems_cb;
	Widget	UxQueue_pane;
	Widget	UxRemoveHold_pb;
	Widget	UxPerformQC_pb;
	Widget	UxMoveJob_pb;
	Widget	UxShowInfo_pb;
	Widget	UxDeleteJob_pb;
	Widget	UxQueue_cb;
	Widget	UxExternal_pane;
	Widget	UxExt_StartAll_pb;
	Widget	UxExt_ReadyAll_pb;
	Widget	UxExt_sep1;
	Widget	UxExt_Start_pb;
	Widget	UxExt_Ready_pb;
	Widget	UxExt_Reset_pb;
	Widget	UxExt_Pause_pb;
	Widget	UxExt_Stop_pb;
	Widget	UxExt_sep2;
	Widget	UxExt_Raise_pb;
	Widget	UxExternal_cb;
	Widget	UxOptions_pane;
	Widget	UxScanDest_pane;
	Widget	UxScanDest_Radio_pane;
	Widget	UxQC_invocation_pane;
	Widget	UxQC_invocation_Radio_pane;
	Widget	UxOptions_cb;
	Widget	UxHelp_pane;
	Widget	UxMain_win_pb;
	Widget	UxSubsysWin_pb;
	Widget	UxJobStates_pb;
	Widget	UxEventTrans_pb;
	Widget	UxMenuTrans_pb;
	Widget	UxProdInfo_pb;
	Widget	UxHelpPane_cb;
	Widget	UxpanedWindow1;
	Widget	UxCPjobs_label;
	Widget	UxCPmainJobIdLabel;
	Widget	UxCPmainPlatRevSeqLabel;
	Widget	UxCPmainFrameLabel;
	Widget	UxCPmainMediaLabel;
	Widget	UxCPmainModeLabel;
	Widget	UxCPmainRequestLabel;
	Widget	UxCPmainTypeLabel;
	Widget	UxCPmainStatusLabel;
	Widget	UxCPstatusForm_label;
	Widget	UxCPexternForm_label;
	Widget	UxCPqueueSize_frame;
	Widget	UxCPqueueSize_form;
	Widget	UxqueueInfo_rc2;
	Widget	UxmaxQueueSize_rc2;
	Widget	UxmaxQueueSize_label2;
	Widget	UxcurrentQueueSize_rc2;
	Widget	UxcurrentQueueSize_label2;
	Widget	UxPPSjobControl_frame;
	Widget	UxPPSjobControlRC_form;
	Widget	UxPPSjobControlAction_label;
	Widget	UxppsJobRequestFrameCts_pb;
	Widget	UxppsJobRequestScanCts_pb;
	Widget	UxppsJobRequestFrameSS_pb;
	Widget	UxppsJobRequestScanSS_pb;
	swidget	UxUxParent;
} _UxCCPmainQ;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCPmainQ             *UxCPmainQContext;
#define CPmainMenu              UxCPmainQContext->UxCPmainMenu
#define File_pane               UxCPmainQContext->UxFile_pane
#define FileOpen_pb             UxCPmainQContext->UxFileOpen_pb
#define FileSave_pb             UxCPmainQContext->UxFileSave_pb
#define FilePrint_pb            UxCPmainQContext->UxFilePrint_pb
#define File_sep1               UxCPmainQContext->UxFile_sep1
#define FileExit_pb             UxCPmainQContext->UxFileExit_pb
#define File_cb                 UxCPmainQContext->UxFile_cb
#define Tools_pane              UxCPmainQContext->UxTools_pane
#define Start_GPR_button        UxCPmainQContext->UxStart_GPR_button
#define CPlogBrowser_info_pb    UxCPmainQContext->UxCPlogBrowser_info_pb
#define CPlogBrowser_debug_pb   UxCPmainQContext->UxCPlogBrowser_debug_pb
#define CPlogBrowser_error_pb   UxCPmainQContext->UxCPlogBrowser_error_pb
#define CP_logBrowser_cb        UxCPmainQContext->UxCP_logBrowser_cb
#define SPSlogBrowser_info_pb   UxCPmainQContext->UxSPSlogBrowser_info_pb
#define SPSlogBrowser_debug_pb  UxCPmainQContext->UxSPSlogBrowser_debug_pb
#define SPSlogBrowser_error_pb  UxCPmainQContext->UxSPSlogBrowser_error_pb
#define SPS_logBrowser_cb       UxCPmainQContext->UxSPS_logBrowser_cb
#define Tools_cb                UxCPmainQContext->UxTools_cb
#define Subsystems_pane         UxCPmainQContext->UxSubsystems_pane
#define Sub_StartAll_pb         UxCPmainQContext->UxSub_StartAll_pb
#define Sub_ReadyAll_pb         UxCPmainQContext->UxSub_ReadyAll_pb
#define Sub_sep1                UxCPmainQContext->UxSub_sep1
#define Sub_Start_pb            UxCPmainQContext->UxSub_Start_pb
#define Sub_Ready_pb            UxCPmainQContext->UxSub_Ready_pb
#define Sub_Reset_pb            UxCPmainQContext->UxSub_Reset_pb
#define Sub_Pause               UxCPmainQContext->UxSub_Pause
#define Sub_Shutdown_pb         UxCPmainQContext->UxSub_Shutdown_pb
#define Sub_sep2                UxCPmainQContext->UxSub_sep2
#define Sub_Raise               UxCPmainQContext->UxSub_Raise
#define Subsystems_cb           UxCPmainQContext->UxSubsystems_cb
#define Queue_pane              UxCPmainQContext->UxQueue_pane
#define RemoveHold_pb           UxCPmainQContext->UxRemoveHold_pb
#define PerformQC_pb            UxCPmainQContext->UxPerformQC_pb
#define MoveJob_pb              UxCPmainQContext->UxMoveJob_pb
#define ShowInfo_pb             UxCPmainQContext->UxShowInfo_pb
#define DeleteJob_pb            UxCPmainQContext->UxDeleteJob_pb
#define Queue_cb                UxCPmainQContext->UxQueue_cb
#define External_pane           UxCPmainQContext->UxExternal_pane
#define Ext_StartAll_pb         UxCPmainQContext->UxExt_StartAll_pb
#define Ext_ReadyAll_pb         UxCPmainQContext->UxExt_ReadyAll_pb
#define Ext_sep1                UxCPmainQContext->UxExt_sep1
#define Ext_Start_pb            UxCPmainQContext->UxExt_Start_pb
#define Ext_Ready_pb            UxCPmainQContext->UxExt_Ready_pb
#define Ext_Reset_pb            UxCPmainQContext->UxExt_Reset_pb
#define Ext_Pause_pb            UxCPmainQContext->UxExt_Pause_pb
#define Ext_Stop_pb             UxCPmainQContext->UxExt_Stop_pb
#define Ext_sep2                UxCPmainQContext->UxExt_sep2
#define Ext_Raise_pb            UxCPmainQContext->UxExt_Raise_pb
#define External_cb             UxCPmainQContext->UxExternal_cb
#define Options_pane            UxCPmainQContext->UxOptions_pane
#define ScanDest_pane           UxCPmainQContext->UxScanDest_pane
#define ScanDest_Radio_pane     UxCPmainQContext->UxScanDest_Radio_pane
#define QC_invocation_pane      UxCPmainQContext->UxQC_invocation_pane
#define QC_invocation_Radio_pane UxCPmainQContext->UxQC_invocation_Radio_pane
#define Options_cb              UxCPmainQContext->UxOptions_cb
#define Help_pane               UxCPmainQContext->UxHelp_pane
#define Main_win_pb             UxCPmainQContext->UxMain_win_pb
#define SubsysWin_pb            UxCPmainQContext->UxSubsysWin_pb
#define JobStates_pb            UxCPmainQContext->UxJobStates_pb
#define EventTrans_pb           UxCPmainQContext->UxEventTrans_pb
#define MenuTrans_pb            UxCPmainQContext->UxMenuTrans_pb
#define ProdInfo_pb             UxCPmainQContext->UxProdInfo_pb
#define HelpPane_cb             UxCPmainQContext->UxHelpPane_cb
#define panedWindow1            UxCPmainQContext->UxpanedWindow1
#define CPjobs_label            UxCPmainQContext->UxCPjobs_label
#define CPmainJobIdLabel        UxCPmainQContext->UxCPmainJobIdLabel
#define CPmainPlatRevSeqLabel   UxCPmainQContext->UxCPmainPlatRevSeqLabel
#define CPmainFrameLabel        UxCPmainQContext->UxCPmainFrameLabel
#define CPmainMediaLabel        UxCPmainQContext->UxCPmainMediaLabel
#define CPmainModeLabel         UxCPmainQContext->UxCPmainModeLabel
#define CPmainRequestLabel      UxCPmainQContext->UxCPmainRequestLabel
#define CPmainTypeLabel         UxCPmainQContext->UxCPmainTypeLabel
#define CPmainStatusLabel       UxCPmainQContext->UxCPmainStatusLabel
#define CPstatusForm_label      UxCPmainQContext->UxCPstatusForm_label
#define CPexternForm_label      UxCPmainQContext->UxCPexternForm_label
#define CPqueueSize_frame       UxCPmainQContext->UxCPqueueSize_frame
#define CPqueueSize_form        UxCPmainQContext->UxCPqueueSize_form
#define queueInfo_rc2           UxCPmainQContext->UxqueueInfo_rc2
#define maxQueueSize_rc2        UxCPmainQContext->UxmaxQueueSize_rc2
#define maxQueueSize_label2     UxCPmainQContext->UxmaxQueueSize_label2
#define currentQueueSize_rc2    UxCPmainQContext->UxcurrentQueueSize_rc2
#define currentQueueSize_label2 UxCPmainQContext->UxcurrentQueueSize_label2
#define PPSjobControl_frame     UxCPmainQContext->UxPPSjobControl_frame
#define PPSjobControlRC_form    UxCPmainQContext->UxPPSjobControlRC_form
#define PPSjobControlAction_label UxCPmainQContext->UxPPSjobControlAction_label
#define ppsJobRequestFrameCts_pb UxCPmainQContext->UxppsJobRequestFrameCts_pb
#define ppsJobRequestScanCts_pb UxCPmainQContext->UxppsJobRequestScanCts_pb
#define ppsJobRequestFrameSS_pb UxCPmainQContext->UxppsJobRequestFrameSS_pb
#define ppsJobRequestScanSS_pb  UxCPmainQContext->UxppsJobRequestScanSS_pb
#define UxParent                UxCPmainQContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	CPmainQ;
extern Widget	CP_logBrowser_pane;
extern Widget	SPS_logBrowser_pane;
extern Widget	toggleScanDest_RDS;
extern Widget	toggleScanDest_ASP;
extern Widget	toggleQC_automatic;
extern Widget	toggleQC_manual;
extern Widget	CPjobs_form;
extern Widget	CPmainJobIdScrolledWin;
extern Widget	CPmainJobIdList;
extern Widget	CPmainPlatRevSeqScrolledWin;
extern Widget	CPmainPlatRevSeqList;
extern Widget	CPmainFrameScrolledWin;
extern Widget	CPmainFrameList;
extern Widget	CPmainMediaScrolledWin;
extern Widget	CPmainMediaList;
extern Widget	CPmainModeScrolledWin;
extern Widget	CPmainModeList;
extern Widget	CPmainRequestScrolledWin;
extern Widget	CPmainRequestList;
extern Widget	CPmainTypeScrolledWin;
extern Widget	CPmainTypeList;
extern Widget	CPmainStatusScrolledWin;
extern Widget	CPmainStatusList;
extern Widget	CPstatusForm;
extern Widget	CPstatusRC;
extern Widget	CPstatusBlank_label;
extern Widget	CPstatusNotRunning_label;
extern Widget	CPstatusStarted_label;
extern Widget	CPstatusReady_label;
extern Widget	CPstatusRunning_label;
extern Widget	CPstatusQC_label;
extern Widget	CPstatusHold_label;
extern Widget	CPstatusError_label;
extern Widget	CPexternForm;
extern Widget	CPexternRC;
extern Widget	CPexternBlank_label;
extern Widget	CPexternNotRunning_label;
extern Widget	CPexternStarted_label;
extern Widget	CPexternReady_label;
extern Widget	CPexternRunning_label;
extern Widget	CPexternQC_label;
extern Widget	CPexternHold_label;
extern Widget	CPexternError_label;
extern Widget	CPoptionsForm;
extern Widget	maxQueueSize_tf;
extern Widget	maxQueueSizeReset_pb;
extern Widget	currentQueueSizeValue_label;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CPmainQ();

#endif	/* _CPMAINQ_INCLUDED */
