
/*******************************************************************************
       CPsubsysQ.h
       (Generated from interface file CPsubsysQ.i)
       This header file is included by CPsubsysQ.c

*******************************************************************************/

#ifndef	_CPSUBSYSQ_INCLUDED
#define	_CPSUBSYSQ_INCLUDED

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
	Widget	UxCPsubsysQ;
	Widget	UxmainWindow1;
	Widget	UxFilePrint_pb;
	Widget	UxFile_sep1;
	Widget	UxFileExit_pb;
	Widget	UxFile_cb;
	Widget	UxCPsubsysLogBrowser_pane;
	Widget	UxSubsysLogBrowser_info_pb;
	Widget	UxSubsysLogBrowser_debug_pb;
	Widget	UxSubsysLogBrowser_error_pb;
	Widget	UxTaskLogBrowser_cb;
	Widget	UxTask_cb;
	Widget	UxMain_win_pb;
	Widget	UxSubsysWin_pb;
	Widget	UxJobStates_pb;
	Widget	UxEventTrans_pb;
	Widget	UxMenuTrans_pb;
	Widget	UxProdInfo_pb;
	Widget	UxHelp_cb;
	Widget	UxSubsystems_pane;
	Widget	UxSubsysReady_pb;
	Widget	UxSubsysRest_pb;
	Widget	UxCPsubsysMenu_p6_b3;
	Widget	UxCPsubsysMenu_p6_b4;
	Widget	UxCPsubsysMenu_top_b1;
	Widget	UxQueueRemoveHold_pb;
	Widget	UxQueueMoveJob_pb;
	Widget	UxsubsysQC_item;
	Widget	UxQueueShowInfo_pb;
	Widget	UxQueueCancelJob_pb;
	Widget	UxQueue_cb;
	Widget	Uxform1;
	Widget	Uxseparator1;
	Widget	Uxform2;
	Widget	UxCPqueJobIdLabel;
	Widget	UxCPquePlatRevSeqLabel;
	Widget	UxCPqueFrameLabel;
	Widget	UxCPqueMediaLabel;
	Widget	UxCPqueModeLabel;
	Widget	UxCPqueRequestLabel;
	Widget	UxCPqueTypeLabel;
	Widget	UxCPqueStatusLabel;
	swidget	UxUxParent;
	char	*UxnamePtr;
} _UxCCPsubsysQ;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCCPsubsysQ           *UxCPsubsysQContext;
#define CPsubsysQ               UxCPsubsysQContext->UxCPsubsysQ
#define mainWindow1             UxCPsubsysQContext->UxmainWindow1
#define FilePrint_pb            UxCPsubsysQContext->UxFilePrint_pb
#define File_sep1               UxCPsubsysQContext->UxFile_sep1
#define FileExit_pb             UxCPsubsysQContext->UxFileExit_pb
#define File_cb                 UxCPsubsysQContext->UxFile_cb
#define CPsubsysLogBrowser_pane UxCPsubsysQContext->UxCPsubsysLogBrowser_pane
#define SubsysLogBrowser_info_pb UxCPsubsysQContext->UxSubsysLogBrowser_info_pb
#define SubsysLogBrowser_debug_pb UxCPsubsysQContext->UxSubsysLogBrowser_debug_pb
#define SubsysLogBrowser_error_pb UxCPsubsysQContext->UxSubsysLogBrowser_error_pb
#define TaskLogBrowser_cb       UxCPsubsysQContext->UxTaskLogBrowser_cb
#define Task_cb                 UxCPsubsysQContext->UxTask_cb
#define Main_win_pb             UxCPsubsysQContext->UxMain_win_pb
#define SubsysWin_pb            UxCPsubsysQContext->UxSubsysWin_pb
#define JobStates_pb            UxCPsubsysQContext->UxJobStates_pb
#define EventTrans_pb           UxCPsubsysQContext->UxEventTrans_pb
#define MenuTrans_pb            UxCPsubsysQContext->UxMenuTrans_pb
#define ProdInfo_pb             UxCPsubsysQContext->UxProdInfo_pb
#define Help_cb                 UxCPsubsysQContext->UxHelp_cb
#define Subsystems_pane         UxCPsubsysQContext->UxSubsystems_pane
#define SubsysReady_pb          UxCPsubsysQContext->UxSubsysReady_pb
#define SubsysRest_pb           UxCPsubsysQContext->UxSubsysRest_pb
#define CPsubsysMenu_p6_b3      UxCPsubsysQContext->UxCPsubsysMenu_p6_b3
#define CPsubsysMenu_p6_b4      UxCPsubsysQContext->UxCPsubsysMenu_p6_b4
#define CPsubsysMenu_top_b1     UxCPsubsysQContext->UxCPsubsysMenu_top_b1
#define QueueRemoveHold_pb      UxCPsubsysQContext->UxQueueRemoveHold_pb
#define QueueMoveJob_pb         UxCPsubsysQContext->UxQueueMoveJob_pb
#define subsysQC_item           UxCPsubsysQContext->UxsubsysQC_item
#define QueueShowInfo_pb        UxCPsubsysQContext->UxQueueShowInfo_pb
#define QueueCancelJob_pb       UxCPsubsysQContext->UxQueueCancelJob_pb
#define Queue_cb                UxCPsubsysQContext->UxQueue_cb
#define form1                   UxCPsubsysQContext->Uxform1
#define separator1              UxCPsubsysQContext->Uxseparator1
#define form2                   UxCPsubsysQContext->Uxform2
#define CPqueJobIdLabel         UxCPsubsysQContext->UxCPqueJobIdLabel
#define CPquePlatRevSeqLabel    UxCPsubsysQContext->UxCPquePlatRevSeqLabel
#define CPqueFrameLabel         UxCPsubsysQContext->UxCPqueFrameLabel
#define CPqueMediaLabel         UxCPsubsysQContext->UxCPqueMediaLabel
#define CPqueModeLabel          UxCPsubsysQContext->UxCPqueModeLabel
#define CPqueRequestLabel       UxCPsubsysQContext->UxCPqueRequestLabel
#define CPqueTypeLabel          UxCPsubsysQContext->UxCPqueTypeLabel
#define CPqueStatusLabel        UxCPsubsysQContext->UxCPqueStatusLabel
#define UxParent                UxCPsubsysQContext->UxUxParent
#define namePtr                 UxCPsubsysQContext->UxnamePtr

#endif /* CONTEXT_MACRO_ACCESS */

extern Widget	CPsubsysMenu;
extern Widget	File_pane;
extern Widget	Task_pane;
extern Widget	Help_pane;
extern Widget	Queue_pane;
extern Widget	CPsubsysQform;
extern Widget	QueueCurrentJobInfoLabel;
extern Widget	QueBottomInfoLabel_0;
extern Widget	CPqueJobIdScrolledWin;
extern Widget	CPqueJobIdList;
extern Widget	CPquePlatRevSeqScrolledWin;
extern Widget	CPquePlatRevSeqList;
extern Widget	CPqueFrameScrolledWin;
extern Widget	CPqueFrameList;
extern Widget	CPqueMediaScrolledWin;
extern Widget	CPqueMediaList;
extern Widget	CPqueModeScrolledWin;
extern Widget	CPqueModeList;
extern Widget	CPqueRequestScrolledWin;
extern Widget	CPqueRequestList;
extern Widget	CPqueTypeScrolledWin;
extern Widget	CPqueTypeList;
extern Widget	CPqueStatusScrolledWin;
extern Widget	CPqueStatusList;

/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_CPsubsysQ();

#endif	/* _CPSUBSYSQ_INCLUDED */
