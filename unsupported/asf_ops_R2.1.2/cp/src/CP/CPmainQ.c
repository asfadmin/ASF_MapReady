
/*******************************************************************************
       CPmainQ.c
       (Generated from interface file CPmainQ.i)
       Associated Header file: CPmainQ.h
       Associated Resource file: CPmainQ.rf
*******************************************************************************/

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

#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/PanedW.h>
#include <Xm/CascadeB.h>
#include <Xm/ToggleBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static char sccsid_CPmainQ_i[] = "@(#)CPmainQ.i	2.61 97/02/06 16:09:58"; 
#include "version.h"
#include "help.h"
#include "cpdefines.h"
#include "pps.h"

typedef void (*VoidProc)();


extern void handleMainMenuDelete();
extern void handleMainMenuPerformQC();
extern void handleMainMenuRemoveHold();
extern void handleMainMenuMoveJob();
extern void handleMainMenuDetailedInfo();
extern void handleMenuPrintCB(); 
extern void handleMenuStartAllCB(); 
extern void handleMenuStartAllExtCB();
extern void handleMenuReadyAllCB(); 
extern void handleMenuReadyAllExtCB();
extern void syncLists();
extern char *getSelectedExternal();
extern void check_number();


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPmainQ.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPmainQ;
Widget	CP_logBrowser_pane;
Widget	SPS_logBrowser_pane;
Widget	toggleScanDest_RDS;
Widget	toggleScanDest_ASP;
Widget	toggleQC_automatic;
Widget	toggleQC_manual;
Widget	CPjobs_form;
Widget	CPmainJobIdScrolledWin;
Widget	CPmainJobIdList;
Widget	CPmainPlatRevSeqScrolledWin;
Widget	CPmainPlatRevSeqList;
Widget	CPmainFrameScrolledWin;
Widget	CPmainFrameList;
Widget	CPmainMediaScrolledWin;
Widget	CPmainMediaList;
Widget	CPmainModeScrolledWin;
Widget	CPmainModeList;
Widget	CPmainRequestScrolledWin;
Widget	CPmainRequestList;
Widget	CPmainTypeScrolledWin;
Widget	CPmainTypeList;
Widget	CPmainStatusScrolledWin;
Widget	CPmainStatusList;
Widget	CPstatusForm;
Widget	CPstatusRC;
Widget	CPstatusBlank_label;
Widget	CPstatusNotRunning_label;
Widget	CPstatusStarted_label;
Widget	CPstatusReady_label;
Widget	CPstatusRunning_label;
Widget	CPstatusQC_label;
Widget	CPstatusHold_label;
Widget	CPstatusError_label;
Widget	CPexternForm;
Widget	CPexternRC;
Widget	CPexternBlank_label;
Widget	CPexternNotRunning_label;
Widget	CPexternStarted_label;
Widget	CPexternReady_label;
Widget	CPexternRunning_label;
Widget	CPexternQC_label;
Widget	CPexternHold_label;
Widget	CPexternError_label;
Widget	CPoptionsForm;
Widget	maxQueueSize_tf;
Widget	maxQueueSizeReset_pb;
Widget	currentQueueSizeValue_label;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  destroyCB_CPmainQ( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_FileOpen_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	handleMenuFileAsCB("Restore");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_FileSave_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	handleMenuFileAsCB("Save As");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_FilePrint_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	performMenuItem(handleMenuPrintCB,NULL,CPmainQ);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_FileExit_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	
	 handleMenuExitCB(UxWidget, 
	                  UxClientData, 
	                  UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Start_GPR_button( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{spawnRequestUtil(); }
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_CPlogBrowser_info_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	spawnLogBrowser("info", UxWidget);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_CPlogBrowser_debug_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	spawnLogBrowser("debug", UxWidget);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_CPlogBrowser_error_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	spawnLogBrowser("error", UxWidget);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_SPSlogBrowser_info_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	spawnLogBrowser("info", UxWidget);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_SPSlogBrowser_debug_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	spawnLogBrowser("debug", UxWidget);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_SPSlogBrowser_error_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	spawnLogBrowser("error", UxWidget);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_StartAll_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMenuStartAllCB,NULL,NULL);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_ReadyAll_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMenuReadyAllCB,NULL,NULL);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_Start_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	  char *getSelectedSubsystem();
	
	  handleToggleStart(getSelectedSubsystem(), UxWidget);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_Ready_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	  char *getSelectedSubsystem();
	
	  handleToggleReady(getSelectedSubsystem());
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_Reset_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	  char *getSelectedSubsystem();
	
	  handleToggleReset(getSelectedSubsystem(), UxWidget);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_Pause( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	  char *getSelectedSubsystem();
	
	  handleTogglePause(getSelectedSubsystem(), UxWidget);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_Shutdown_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	  char *getSelectedSubsystem();
	
	  handleToggleStop(getSelectedSubsystem(), UxWidget);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Sub_Raise( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	  char *getSelectedSubsystem();
	
	  handleToggleRaise(getSelectedSubsystem(), UxWidget);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_RemoveHold_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMainMenuRemoveHold, NULL,NULL);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_PerformQC_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMainMenuPerformQC, NULL,NULL);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_MoveJob_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMainMenuMoveJob, NULL,NULL);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_ShowInfo_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	performMenuItem(handleMainMenuDetailedInfo, NULL,NULL);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_DeleteJob_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMainMenuDelete, NULL,NULL); }
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_StartAll_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMenuStartAllExtCB,NULL,NULL); }
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_ReadyAll_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{performMenuItem(handleMenuReadyAllExtCB,NULL,NULL); }
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_Start_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{handleToggleStart(getSelectedExternal(), UxWidget);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_Ready_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{handleToggleReady(getSelectedExternal(), UxWidget);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_Reset_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{handleToggleReset(getSelectedExternal(), UxWidget);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_Pause_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{handleTogglePause(getSelectedExternal(), UxWidget);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_Stop_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{handleToggleStop(getSelectedExternal(), UxWidget);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Ext_Raise_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{handleToggleRaise(getSelectedExternal(), UxWidget);}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_Main_win_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	showHelp(HELP_SPS_QUEUE_WIN, "");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_SubsysWin_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	showHelp(HELP_SUBSYS_WIN, "");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_JobStates_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	showHelp(HELP_JOB_STATES, "");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_EventTrans_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	showHelp(HELP_EVENT_TRANSITIONS, "");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_MenuTrans_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	showHelp(HELP_MENU_TRANSITIONS, "");
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_ProdInfo_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	showHelp(HELP_INFO, CP_version_id);
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainJobIdScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainJobIdList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainJobIdList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainPlatRevSeqScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainPlatRevSeqList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainPlatRevSeqList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainFrameScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainFrameList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainFrameList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainMediaScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainMediaList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainMediaList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainModeScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainModeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainModeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainRequestScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainRequestList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainRequestList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainTypeScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainTypeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainTypeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  createCB_CPmainStatusScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxContext = UxCPmainQContext;
	{
	
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  defaultActionCB_CPmainStatusList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	showDetailedInfo(UxWidget,  UxCallbackArg); 
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPmainStatusList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	syncLists(UxWidget, CPmainQ, UxCallbackArg);
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_maxQueueSizeReset_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	resetQueueDisplaySize(maxQueueSize_tf);
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_ppsJobRequestFrameCts_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	extern int ppsLastRequested;
	
	ppsLastRequested = P_CTS_FRAME;
	requestPPSmsg("CONTINUOUS", "FRAME");
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_ppsJobRequestScanCts_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	extern int ppsLastRequested;
	
	ppsLastRequested = P_CTS_SCAN;
	requestPPSmsg("CONTINUOUS", "SCAN");
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_ppsJobRequestFrameSS_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	extern int ppsLastRequested;
	
	ppsLastRequested = P_SS_FRAME;
	requestPPSmsg("SCANSAR", "FRAME");
	}
	UxCPmainQContext = UxSaveCtx;
}

static void  activateCB_ppsJobRequestScanSS_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPmainQ             *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPmainQContext;
	UxCPmainQContext = UxContext =
			(_UxCCPmainQ *) UxGetContext( UxWidget );
	{
	extern int ppsLastRequested;
	
	ppsLastRequested = P_SS_SCAN;
	requestPPSmsg("SCANSAR", "SCAN");
	}
	UxCPmainQContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPmainQ()
{
	Widget		_UxParent;
	Widget		File_pane_shell;
	Widget		Tools_pane_shell;
	Widget		CP_logBrowser_pane_shell;
	Widget		SPS_logBrowser_pane_shell;
	Widget		Subsystems_pane_shell;
	Widget		Queue_pane_shell;
	Widget		External_pane_shell;
	Widget		Options_pane_shell;
	Widget		ScanDest_pane_shell;
	Widget		QC_invocation_pane_shell;
	Widget		Help_pane_shell;


	/* Creation of CPmainQ */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = XtVaCreatePopupShell( "CPmainQ_shell",
			topLevelShellWidgetClass, UxTopLevel,
			XmNshellUnitType, XmPIXELS,
			XmNtitle, "CPmainQ",
			XmNiconName, "CPmainQ",
			NULL );

	}

	CPmainQ = XtVaCreateManagedWidget( "CPmainQ",
			xmMainWindowWidgetClass,
			_UxParent,
			XmNunitType, XmPIXELS,
			NULL );
	XtAddCallback( CPmainQ, XmNdestroyCallback,
		(XtCallbackProc) destroyCB_CPmainQ,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainQ, (char *) UxCPmainQContext );
	UxPutClassCode( CPmainQ, _UxIfClassId );


	/* Creation of CPmainMenu */
	CPmainMenu = XtVaCreateManagedWidget( "CPmainMenu",
			xmRowColumnWidgetClass,
			CPmainQ,
			XmNrowColumnType, XmMENU_BAR,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( CPmainMenu, (char *) UxCPmainQContext );


	/* Creation of File_pane */
	File_pane_shell = XtVaCreatePopupShell ("File_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	File_pane = XtVaCreateWidget( "File_pane",
			xmRowColumnWidgetClass,
			File_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( File_pane, (char *) UxCPmainQContext );


	/* Creation of FileOpen_pb */
	FileOpen_pb = XtVaCreateManagedWidget( "FileOpen_pb",
			xmPushButtonGadgetClass,
			File_pane,
			RES_CONVERT( XmNlabelString, "Open ..." ),
			RES_CONVERT( XmNmnemonic, "O" ),
			NULL );
	XtAddCallback( FileOpen_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FileOpen_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( FileOpen_pb, (char *) UxCPmainQContext );


	/* Creation of FileSave_pb */
	FileSave_pb = XtVaCreateManagedWidget( "FileSave_pb",
			xmPushButtonGadgetClass,
			File_pane,
			RES_CONVERT( XmNlabelString, "Save As ..." ),
			RES_CONVERT( XmNmnemonic, "A" ),
			NULL );
	XtAddCallback( FileSave_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FileSave_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( FileSave_pb, (char *) UxCPmainQContext );


	/* Creation of FilePrint_pb */
	FilePrint_pb = XtVaCreateManagedWidget( "FilePrint_pb",
			xmPushButtonGadgetClass,
			File_pane,
			RES_CONVERT( XmNlabelString, "Print" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( FilePrint_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FilePrint_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( FilePrint_pb, (char *) UxCPmainQContext );


	/* Creation of File_sep1 */
	File_sep1 = XtVaCreateManagedWidget( "File_sep1",
			xmSeparatorGadgetClass,
			File_pane,
			NULL );
	UxPutContext( File_sep1, (char *) UxCPmainQContext );


	/* Creation of FileExit_pb */
	FileExit_pb = XtVaCreateManagedWidget( "FileExit_pb",
			xmPushButtonGadgetClass,
			File_pane,
			RES_CONVERT( XmNlabelString, "Exit" ),
			RES_CONVERT( XmNmnemonic, "x" ),
			NULL );
	XtAddCallback( FileExit_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FileExit_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( FileExit_pb, (char *) UxCPmainQContext );


	/* Creation of File_cb */
	File_cb = XtVaCreateManagedWidget( "File_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "File" ),
			RES_CONVERT( XmNmnemonic, "F" ),
			XmNsubMenuId, File_pane,
			NULL );
	UxPutContext( File_cb, (char *) UxCPmainQContext );


	/* Creation of Tools_pane */
	Tools_pane_shell = XtVaCreatePopupShell ("Tools_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Tools_pane = XtVaCreateWidget( "Tools_pane",
			xmRowColumnWidgetClass,
			Tools_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( Tools_pane, (char *) UxCPmainQContext );


	/* Creation of Start_GPR_button */
	Start_GPR_button = XtVaCreateManagedWidget( "Start_GPR_button",
			xmPushButtonGadgetClass,
			Tools_pane,
			RES_CONVERT( XmNlabelString, "Generate Production Request (GPR)" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( Start_GPR_button, XmNactivateCallback,
		(XtCallbackProc) activateCB_Start_GPR_button,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Start_GPR_button, (char *) UxCPmainQContext );


	/* Creation of CP_logBrowser_pane */
	CP_logBrowser_pane_shell = XtVaCreatePopupShell ("CP_logBrowser_pane_shell",
			xmMenuShellWidgetClass, Tools_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	CP_logBrowser_pane = XtVaCreateWidget( "CP_logBrowser_pane",
			xmRowColumnWidgetClass,
			CP_logBrowser_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( CP_logBrowser_pane, (char *) UxCPmainQContext );


	/* Creation of CPlogBrowser_info_pb */
	CPlogBrowser_info_pb = XtVaCreateManagedWidget( "CPlogBrowser_info_pb",
			xmPushButtonGadgetClass,
			CP_logBrowser_pane,
			RES_CONVERT( XmNlabelString, "Info" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( CPlogBrowser_info_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_CPlogBrowser_info_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPlogBrowser_info_pb, (char *) UxCPmainQContext );


	/* Creation of CPlogBrowser_debug_pb */
	CPlogBrowser_debug_pb = XtVaCreateManagedWidget( "CPlogBrowser_debug_pb",
			xmPushButtonGadgetClass,
			CP_logBrowser_pane,
			RES_CONVERT( XmNlabelString, "Debug" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			NULL );
	XtAddCallback( CPlogBrowser_debug_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_CPlogBrowser_debug_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPlogBrowser_debug_pb, (char *) UxCPmainQContext );


	/* Creation of CPlogBrowser_error_pb */
	CPlogBrowser_error_pb = XtVaCreateManagedWidget( "CPlogBrowser_error_pb",
			xmPushButtonGadgetClass,
			CP_logBrowser_pane,
			RES_CONVERT( XmNlabelString, "Error" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			NULL );
	XtAddCallback( CPlogBrowser_error_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_CPlogBrowser_error_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPlogBrowser_error_pb, (char *) UxCPmainQContext );


	/* Creation of CP_logBrowser_cb */
	CP_logBrowser_cb = XtVaCreateManagedWidget( "CP_logBrowser_cb",
			xmCascadeButtonGadgetClass,
			Tools_pane,
			RES_CONVERT( XmNlabelString, "CP Log Browser" ),
			XmNsubMenuId, CP_logBrowser_pane,
			RES_CONVERT( XmNmnemonic, "C" ),
			NULL );
	UxPutContext( CP_logBrowser_cb, (char *) UxCPmainQContext );


	/* Creation of SPS_logBrowser_pane */
	SPS_logBrowser_pane_shell = XtVaCreatePopupShell ("SPS_logBrowser_pane_shell",
			xmMenuShellWidgetClass, Tools_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	SPS_logBrowser_pane = XtVaCreateWidget( "SPS_logBrowser_pane",
			xmRowColumnWidgetClass,
			SPS_logBrowser_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( SPS_logBrowser_pane, (char *) UxCPmainQContext );


	/* Creation of SPSlogBrowser_info_pb */
	SPSlogBrowser_info_pb = XtVaCreateManagedWidget( "SPSlogBrowser_info_pb",
			xmPushButtonGadgetClass,
			SPS_logBrowser_pane,
			RES_CONVERT( XmNlabelString, "Info" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( SPSlogBrowser_info_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SPSlogBrowser_info_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( SPSlogBrowser_info_pb, (char *) UxCPmainQContext );


	/* Creation of SPSlogBrowser_debug_pb */
	SPSlogBrowser_debug_pb = XtVaCreateManagedWidget( "SPSlogBrowser_debug_pb",
			xmPushButtonGadgetClass,
			SPS_logBrowser_pane,
			RES_CONVERT( XmNlabelString, "Debug" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			NULL );
	XtAddCallback( SPSlogBrowser_debug_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SPSlogBrowser_debug_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( SPSlogBrowser_debug_pb, (char *) UxCPmainQContext );


	/* Creation of SPSlogBrowser_error_pb */
	SPSlogBrowser_error_pb = XtVaCreateManagedWidget( "SPSlogBrowser_error_pb",
			xmPushButtonGadgetClass,
			SPS_logBrowser_pane,
			RES_CONVERT( XmNlabelString, "Error" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			NULL );
	XtAddCallback( SPSlogBrowser_error_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SPSlogBrowser_error_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( SPSlogBrowser_error_pb, (char *) UxCPmainQContext );


	/* Creation of SPS_logBrowser_cb */
	SPS_logBrowser_cb = XtVaCreateManagedWidget( "SPS_logBrowser_cb",
			xmCascadeButtonGadgetClass,
			Tools_pane,
			RES_CONVERT( XmNlabelString, "SPS Log Browser" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, SPS_logBrowser_pane,
			NULL );
	UxPutContext( SPS_logBrowser_cb, (char *) UxCPmainQContext );


	/* Creation of Tools_cb */
	Tools_cb = XtVaCreateManagedWidget( "Tools_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "Tools" ),
			XmNsubMenuId, Tools_pane,
			RES_CONVERT( XmNmnemonic, "T" ),
			NULL );
	UxPutContext( Tools_cb, (char *) UxCPmainQContext );


	/* Creation of Subsystems_pane */
	Subsystems_pane_shell = XtVaCreatePopupShell ("Subsystems_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Subsystems_pane = XtVaCreateWidget( "Subsystems_pane",
			xmRowColumnWidgetClass,
			Subsystems_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( Subsystems_pane, (char *) UxCPmainQContext );


	/* Creation of Sub_StartAll_pb */
	Sub_StartAll_pb = XtVaCreateManagedWidget( "Sub_StartAll_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Start All" ),
			RES_CONVERT( XmNmnemonic, "A" ),
			NULL );
	XtAddCallback( Sub_StartAll_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_StartAll_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_StartAll_pb, (char *) UxCPmainQContext );


	/* Creation of Sub_ReadyAll_pb */
	Sub_ReadyAll_pb = XtVaCreateManagedWidget( "Sub_ReadyAll_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Ready All" ),
			RES_CONVERT( XmNmnemonic, "d" ),
			NULL );
	XtAddCallback( Sub_ReadyAll_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_ReadyAll_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_ReadyAll_pb, (char *) UxCPmainQContext );


	/* Creation of Sub_sep1 */
	Sub_sep1 = XtVaCreateManagedWidget( "Sub_sep1",
			xmSeparatorGadgetClass,
			Subsystems_pane,
			NULL );
	UxPutContext( Sub_sep1, (char *) UxCPmainQContext );


	/* Creation of Sub_Start_pb */
	Sub_Start_pb = XtVaCreateManagedWidget( "Sub_Start_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Start" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			NULL );
	XtAddCallback( Sub_Start_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_Start_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_Start_pb, (char *) UxCPmainQContext );


	/* Creation of Sub_Ready_pb */
	Sub_Ready_pb = XtVaCreateManagedWidget( "Sub_Ready_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Ready" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( Sub_Ready_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_Ready_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_Ready_pb, (char *) UxCPmainQContext );


	/* Creation of Sub_Reset_pb */
	Sub_Reset_pb = XtVaCreateManagedWidget( "Sub_Reset_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Reset" ),
			RES_CONVERT( XmNmnemonic, "t" ),
			NULL );
	XtAddCallback( Sub_Reset_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_Reset_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_Reset_pb, (char *) UxCPmainQContext );


	/* Creation of Sub_Pause */
	Sub_Pause = XtVaCreateManagedWidget( "Sub_Pause",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Pause" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( Sub_Pause, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_Pause,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_Pause, (char *) UxCPmainQContext );


	/* Creation of Sub_Shutdown_pb */
	Sub_Shutdown_pb = XtVaCreateManagedWidget( "Sub_Shutdown_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Shutdown" ),
			RES_CONVERT( XmNmnemonic, "h" ),
			NULL );
	XtAddCallback( Sub_Shutdown_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_Shutdown_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_Shutdown_pb, (char *) UxCPmainQContext );


	/* Creation of Sub_sep2 */
	Sub_sep2 = XtVaCreateManagedWidget( "Sub_sep2",
			xmSeparatorGadgetClass,
			Subsystems_pane,
			NULL );
	UxPutContext( Sub_sep2, (char *) UxCPmainQContext );


	/* Creation of Sub_Raise */
	Sub_Raise = XtVaCreateManagedWidget( "Sub_Raise",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Raise Window" ),
			RES_CONVERT( XmNmnemonic, "i" ),
			NULL );
	XtAddCallback( Sub_Raise, XmNactivateCallback,
		(XtCallbackProc) activateCB_Sub_Raise,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Sub_Raise, (char *) UxCPmainQContext );


	/* Creation of Subsystems_cb */
	Subsystems_cb = XtVaCreateManagedWidget( "Subsystems_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "Subsystems" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, Subsystems_pane,
			NULL );
	UxPutContext( Subsystems_cb, (char *) UxCPmainQContext );


	/* Creation of Queue_pane */
	Queue_pane_shell = XtVaCreatePopupShell ("Queue_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Queue_pane = XtVaCreateWidget( "Queue_pane",
			xmRowColumnWidgetClass,
			Queue_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( Queue_pane, (char *) UxCPmainQContext );


	/* Creation of RemoveHold_pb */
	RemoveHold_pb = XtVaCreateManagedWidget( "RemoveHold_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Remove Hold" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( RemoveHold_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_RemoveHold_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( RemoveHold_pb, (char *) UxCPmainQContext );


	/* Creation of PerformQC_pb */
	PerformQC_pb = XtVaCreateManagedWidget( "PerformQC_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Perform QC" ),
			RES_CONVERT( XmNmnemonic, "Q" ),
			NULL );
	XtAddCallback( PerformQC_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_PerformQC_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( PerformQC_pb, (char *) UxCPmainQContext );


	/* Creation of MoveJob_pb */
	MoveJob_pb = XtVaCreateManagedWidget( "MoveJob_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Move Job" ),
			RES_CONVERT( XmNmnemonic, "v" ),
			NULL );
	XtAddCallback( MoveJob_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_MoveJob_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( MoveJob_pb, (char *) UxCPmainQContext );


	/* Creation of ShowInfo_pb */
	ShowInfo_pb = XtVaCreateManagedWidget( "ShowInfo_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Show Detailed Info" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( ShowInfo_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ShowInfo_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( ShowInfo_pb, (char *) UxCPmainQContext );


	/* Creation of DeleteJob_pb */
	DeleteJob_pb = XtVaCreateManagedWidget( "DeleteJob_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Cancel Job" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			NULL );
	XtAddCallback( DeleteJob_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_DeleteJob_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( DeleteJob_pb, (char *) UxCPmainQContext );


	/* Creation of Queue_cb */
	Queue_cb = XtVaCreateManagedWidget( "Queue_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "Jobs" ),
			XmNsubMenuId, Queue_pane,
			RES_CONVERT( XmNmnemonic, "J" ),
			NULL );
	UxPutContext( Queue_cb, (char *) UxCPmainQContext );


	/* Creation of External_pane */
	External_pane_shell = XtVaCreatePopupShell ("External_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	External_pane = XtVaCreateWidget( "External_pane",
			xmRowColumnWidgetClass,
			External_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNtearOffModel, XmTEAR_OFF_ENABLED,
			NULL );
	UxPutContext( External_pane, (char *) UxCPmainQContext );


	/* Creation of Ext_StartAll_pb */
	Ext_StartAll_pb = XtVaCreateManagedWidget( "Ext_StartAll_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Start All" ),
			RES_CONVERT( XmNmnemonic, "A" ),
			NULL );
	XtAddCallback( Ext_StartAll_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_StartAll_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_StartAll_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_ReadyAll_pb */
	Ext_ReadyAll_pb = XtVaCreateManagedWidget( "Ext_ReadyAll_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Ready All" ),
			RES_CONVERT( XmNmnemonic, "d" ),
			NULL );
	XtAddCallback( Ext_ReadyAll_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_ReadyAll_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_ReadyAll_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_sep1 */
	Ext_sep1 = XtVaCreateManagedWidget( "Ext_sep1",
			xmSeparatorGadgetClass,
			External_pane,
			NULL );
	UxPutContext( Ext_sep1, (char *) UxCPmainQContext );


	/* Creation of Ext_Start_pb */
	Ext_Start_pb = XtVaCreateManagedWidget( "Ext_Start_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Start" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			NULL );
	XtAddCallback( Ext_Start_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_Start_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_Start_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_Ready_pb */
	Ext_Ready_pb = XtVaCreateManagedWidget( "Ext_Ready_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Ready" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( Ext_Ready_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_Ready_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_Ready_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_Reset_pb */
	Ext_Reset_pb = XtVaCreateManagedWidget( "Ext_Reset_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Reset" ),
			RES_CONVERT( XmNmnemonic, "t" ),
			NULL );
	XtAddCallback( Ext_Reset_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_Reset_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_Reset_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_Pause_pb */
	Ext_Pause_pb = XtVaCreateManagedWidget( "Ext_Pause_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Pause" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( Ext_Pause_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_Pause_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_Pause_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_Stop_pb */
	Ext_Stop_pb = XtVaCreateManagedWidget( "Ext_Stop_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Shutdown" ),
			RES_CONVERT( XmNmnemonic, "h" ),
			NULL );
	XtAddCallback( Ext_Stop_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_Stop_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_Stop_pb, (char *) UxCPmainQContext );


	/* Creation of Ext_sep2 */
	Ext_sep2 = XtVaCreateManagedWidget( "Ext_sep2",
			xmSeparatorGadgetClass,
			External_pane,
			NULL );
	UxPutContext( Ext_sep2, (char *) UxCPmainQContext );


	/* Creation of Ext_Raise_pb */
	Ext_Raise_pb = XtVaCreateManagedWidget( "Ext_Raise_pb",
			xmPushButtonGadgetClass,
			External_pane,
			RES_CONVERT( XmNlabelString, "Raise Window" ),
			RES_CONVERT( XmNmnemonic, "i" ),
			NULL );
	XtAddCallback( Ext_Raise_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Ext_Raise_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Ext_Raise_pb, (char *) UxCPmainQContext );


	/* Creation of External_cb */
	External_cb = XtVaCreateManagedWidget( "External_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "External" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			XmNsubMenuId, External_pane,
			NULL );
	UxPutContext( External_cb, (char *) UxCPmainQContext );


	/* Creation of Options_pane */
	Options_pane_shell = XtVaCreatePopupShell ("Options_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Options_pane = XtVaCreateWidget( "Options_pane",
			xmRowColumnWidgetClass,
			Options_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( Options_pane, (char *) UxCPmainQContext );


	/* Creation of ScanDest_pane */
	ScanDest_pane_shell = XtVaCreatePopupShell ("ScanDest_pane_shell",
			xmMenuShellWidgetClass, Options_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	ScanDest_pane = XtVaCreateWidget( "ScanDest_pane",
			xmRowColumnWidgetClass,
			ScanDest_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNradioBehavior, TRUE,
			NULL );
	UxPutContext( ScanDest_pane, (char *) UxCPmainQContext );


	/* Creation of toggleScanDest_RDS */
	toggleScanDest_RDS = XtVaCreateManagedWidget( "toggleScanDest_RDS",
			xmToggleButtonGadgetClass,
			ScanDest_pane,
			RES_CONVERT( XmNlabelString, "RDS" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	UxPutContext( toggleScanDest_RDS, (char *) UxCPmainQContext );


	/* Creation of toggleScanDest_ASP */
	toggleScanDest_ASP = XtVaCreateManagedWidget( "toggleScanDest_ASP",
			xmToggleButtonGadgetClass,
			ScanDest_pane,
			RES_CONVERT( XmNlabelString, "ASP" ),
			RES_CONVERT( XmNmnemonic, "A" ),
			NULL );
	UxPutContext( toggleScanDest_ASP, (char *) UxCPmainQContext );


	/* Creation of ScanDest_Radio_pane */
	ScanDest_Radio_pane = XtVaCreateManagedWidget( "ScanDest_Radio_pane",
			xmCascadeButtonGadgetClass,
			Options_pane,
			RES_CONVERT( XmNlabelString, "Scan Destination" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			XmNsubMenuId, ScanDest_pane,
			NULL );
	UxPutContext( ScanDest_Radio_pane, (char *) UxCPmainQContext );


	/* Creation of QC_invocation_pane */
	QC_invocation_pane_shell = XtVaCreatePopupShell ("QC_invocation_pane_shell",
			xmMenuShellWidgetClass, Options_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	QC_invocation_pane = XtVaCreateWidget( "QC_invocation_pane",
			xmRowColumnWidgetClass,
			QC_invocation_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNradioBehavior, TRUE,
			NULL );
	UxPutContext( QC_invocation_pane, (char *) UxCPmainQContext );


	/* Creation of toggleQC_automatic */
	toggleQC_automatic = XtVaCreateManagedWidget( "toggleQC_automatic",
			xmToggleButtonGadgetClass,
			QC_invocation_pane,
			RES_CONVERT( XmNlabelString, "Automatic" ),
			RES_CONVERT( XmNmnemonic, "A" ),
			NULL );
	UxPutContext( toggleQC_automatic, (char *) UxCPmainQContext );


	/* Creation of toggleQC_manual */
	toggleQC_manual = XtVaCreateManagedWidget( "toggleQC_manual",
			xmToggleButtonGadgetClass,
			QC_invocation_pane,
			RES_CONVERT( XmNlabelString, "Interactive" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	UxPutContext( toggleQC_manual, (char *) UxCPmainQContext );


	/* Creation of QC_invocation_Radio_pane */
	QC_invocation_Radio_pane = XtVaCreateManagedWidget( "QC_invocation_Radio_pane",
			xmCascadeButtonGadgetClass,
			Options_pane,
			RES_CONVERT( XmNlabelString, "QC Invocation" ),
			RES_CONVERT( XmNmnemonic, "Q" ),
			XmNsubMenuId, QC_invocation_pane,
			NULL );
	UxPutContext( QC_invocation_Radio_pane, (char *) UxCPmainQContext );


	/* Creation of Options_cb */
	Options_cb = XtVaCreateManagedWidget( "Options_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "Options" ),
			XmNsubMenuId, Options_pane,
			RES_CONVERT( XmNmnemonic, "O" ),
			NULL );
	UxPutContext( Options_cb, (char *) UxCPmainQContext );


	/* Creation of Help_pane */
	Help_pane_shell = XtVaCreatePopupShell ("Help_pane_shell",
			xmMenuShellWidgetClass, CPmainMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Help_pane = XtVaCreateWidget( "Help_pane",
			xmRowColumnWidgetClass,
			Help_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( Help_pane, (char *) UxCPmainQContext );


	/* Creation of Main_win_pb */
	Main_win_pb = XtVaCreateManagedWidget( "Main_win_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "SPS Processing Queue Window" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( Main_win_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Main_win_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( Main_win_pb, (char *) UxCPmainQContext );


	/* Creation of SubsysWin_pb */
	SubsysWin_pb = XtVaCreateManagedWidget( "SubsysWin_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Subsystem Windows" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			NULL );
	XtAddCallback( SubsysWin_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysWin_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( SubsysWin_pb, (char *) UxCPmainQContext );


	/* Creation of JobStates_pb */
	JobStates_pb = XtVaCreateManagedWidget( "JobStates_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Job States" ),
			RES_CONVERT( XmNmnemonic, "J" ),
			NULL );
	XtAddCallback( JobStates_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_JobStates_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( JobStates_pb, (char *) UxCPmainQContext );


	/* Creation of EventTrans_pb */
	EventTrans_pb = XtVaCreateManagedWidget( "EventTrans_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Event Transitions" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			NULL );
	XtAddCallback( EventTrans_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_EventTrans_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( EventTrans_pb, (char *) UxCPmainQContext );


	/* Creation of MenuTrans_pb */
	MenuTrans_pb = XtVaCreateManagedWidget( "MenuTrans_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Menu Transitions" ),
			RES_CONVERT( XmNmnemonic, "M" ),
			NULL );
	XtAddCallback( MenuTrans_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_MenuTrans_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( MenuTrans_pb, (char *) UxCPmainQContext );


	/* Creation of ProdInfo_pb */
	ProdInfo_pb = XtVaCreateManagedWidget( "ProdInfo_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Product Information" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( ProdInfo_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ProdInfo_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( ProdInfo_pb, (char *) UxCPmainQContext );


	/* Creation of HelpPane_cb */
	HelpPane_cb = XtVaCreateManagedWidget( "HelpPane_cb",
			xmCascadeButtonWidgetClass,
			CPmainMenu,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, Help_pane,
			NULL );
	UxPutContext( HelpPane_cb, (char *) UxCPmainQContext );


	/* Creation of panedWindow1 */
	panedWindow1 = XtVaCreateManagedWidget( "panedWindow1",
			xmPanedWindowWidgetClass,
			CPmainQ,
			XmNseparatorOn, TRUE,
			XmNmarginWidth, 0,
			RES_CONVERT( XmNbackground, "#a050a0" ),
			NULL );
	UxPutContext( panedWindow1, (char *) UxCPmainQContext );


	/* Creation of CPjobs_form */
	CPjobs_form = XtVaCreateManagedWidget( "CPjobs_form",
			xmFormWidgetClass,
			panedWindow1,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 0,
			XmNy, 342,
			XmNallowResize, TRUE,
			NULL );
	UxPutContext( CPjobs_form, (char *) UxCPmainQContext );


	/* Creation of CPmainJobIdScrolledWin */
	CPmainJobIdScrolledWin = XtVaCreateManagedWidget( "CPmainJobIdScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 8,
			XmNy, 30,
			XmNwidth, 85,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainJobIdScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainJobIdScrolledWin( CPmainJobIdScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainJobIdList */
	CPmainJobIdList = XtVaCreateManagedWidget( "CPmainJobIdList",
			xmListWidgetClass,
			CPmainJobIdScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainJobIdList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainJobIdList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainJobIdList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainJobIdList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainJobIdList, (char *) UxCPmainQContext );


	/* Creation of CPmainPlatRevSeqScrolledWin */
	CPmainPlatRevSeqScrolledWin = XtVaCreateManagedWidget( "CPmainPlatRevSeqScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 125,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainJobIdScrolledWin,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainPlatRevSeqScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainPlatRevSeqScrolledWin( CPmainPlatRevSeqScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainPlatRevSeqList */
	CPmainPlatRevSeqList = XtVaCreateManagedWidget( "CPmainPlatRevSeqList",
			xmListWidgetClass,
			CPmainPlatRevSeqScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainPlatRevSeqList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainPlatRevSeqList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainPlatRevSeqList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainPlatRevSeqList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainPlatRevSeqList, (char *) UxCPmainQContext );


	/* Creation of CPmainFrameScrolledWin */
	CPmainFrameScrolledWin = XtVaCreateManagedWidget( "CPmainFrameScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 75,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainPlatRevSeqScrolledWin,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainFrameScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainFrameScrolledWin( CPmainFrameScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainFrameList */
	CPmainFrameList = XtVaCreateManagedWidget( "CPmainFrameList",
			xmListWidgetClass,
			CPmainFrameScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainFrameList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainFrameList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainFrameList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainFrameList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainFrameList, (char *) UxCPmainQContext );


	/* Creation of CPmainMediaScrolledWin */
	CPmainMediaScrolledWin = XtVaCreateManagedWidget( "CPmainMediaScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 200,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainFrameScrolledWin,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainMediaScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainMediaScrolledWin( CPmainMediaScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainMediaList */
	CPmainMediaList = XtVaCreateManagedWidget( "CPmainMediaList",
			xmListWidgetClass,
			CPmainMediaScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainMediaList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainMediaList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainMediaList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainMediaList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainMediaList, (char *) UxCPmainQContext );


	/* Creation of CPmainModeScrolledWin */
	CPmainModeScrolledWin = XtVaCreateManagedWidget( "CPmainModeScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 65,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainMediaScrolledWin,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainModeScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainModeScrolledWin( CPmainModeScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainModeList */
	CPmainModeList = XtVaCreateManagedWidget( "CPmainModeList",
			xmListWidgetClass,
			CPmainModeScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainModeList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainModeList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainModeList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainModeList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainModeList, (char *) UxCPmainQContext );


	/* Creation of CPmainRequestScrolledWin */
	CPmainRequestScrolledWin = XtVaCreateManagedWidget( "CPmainRequestScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainModeScrolledWin,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainRequestScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainRequestScrolledWin( CPmainRequestScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainRequestList */
	CPmainRequestList = XtVaCreateManagedWidget( "CPmainRequestList",
			xmListWidgetClass,
			CPmainRequestScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainRequestList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainRequestList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainRequestList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainRequestList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainRequestList, (char *) UxCPmainQContext );


	/* Creation of CPmainTypeScrolledWin */
	CPmainTypeScrolledWin = XtVaCreateManagedWidget( "CPmainTypeScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 145,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainRequestScrolledWin,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainTypeScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainTypeScrolledWin( CPmainTypeScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainTypeList */
	CPmainTypeList = XtVaCreateManagedWidget( "CPmainTypeList",
			xmListWidgetClass,
			CPmainTypeScrolledWin,
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainTypeList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainTypeList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainTypeList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainTypeList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainTypeList, (char *) UxCPmainQContext );


	/* Creation of CPmainStatusScrolledWin */
	CPmainStatusScrolledWin = XtVaCreateManagedWidget( "CPmainStatusScrolledWin",
			xmScrolledWindowWidgetClass,
			CPjobs_form,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNwidth, 215,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 60,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPmainTypeScrolledWin,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 10,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPmainStatusScrolledWin, (char *) UxCPmainQContext );

	createCB_CPmainStatusScrolledWin( CPmainStatusScrolledWin,
			(XtPointer) UxCPmainQContext, (XtPointer) NULL );


	/* Creation of CPmainStatusList */
	CPmainStatusList = XtVaCreateManagedWidget( "CPmainStatusList",
			xmListWidgetClass,
			CPmainStatusScrolledWin,
			XmNlistSizePolicy, XmCONSTANT,
			XmNhighlightThickness, 0,
			XmNshadowThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPmainStatusList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPmainStatusList,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( CPmainStatusList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPmainStatusList,
		(XtPointer) UxCPmainQContext );

	UxPutContext( CPmainStatusList, (char *) UxCPmainQContext );


	/* Creation of CPjobs_label */
	CPjobs_label = XtVaCreateManagedWidget( "CPjobs_label",
			xmLabelWidgetClass,
			CPjobs_form,
			XmNx, 0,
			XmNy, 3,
			XmNwidth, 975,
			XmNheight, 30,
			XmNfontList, UxConvertFontList("lucibis18" ),
			RES_CONVERT( XmNlabelString, "Jobs" ),
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( CPjobs_label, (char *) UxCPmainQContext );


	/* Creation of CPmainJobIdLabel */
	CPmainJobIdLabel = XtVaCreateManagedWidget( "CPmainJobIdLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 8,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Job ID" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainJobIdScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainJobIdScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainJobIdLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainPlatRevSeqLabel */
	CPmainPlatRevSeqLabel = XtVaCreateManagedWidget( "CPmainPlatRevSeqLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 100,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Plat/Rev/Seq" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainPlatRevSeqScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainPlatRevSeqScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainPlatRevSeqLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainFrameLabel */
	CPmainFrameLabel = XtVaCreateManagedWidget( "CPmainFrameLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 232,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Frame" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainFrameScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainFrameScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainFrameLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainMediaLabel */
	CPmainMediaLabel = XtVaCreateManagedWidget( "CPmainMediaLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 314,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Media" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainMediaScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainMediaScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainMediaLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainModeLabel */
	CPmainModeLabel = XtVaCreateManagedWidget( "CPmainModeLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 406,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Mode" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainModeScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainModeScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainModeLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainRequestLabel */
	CPmainRequestLabel = XtVaCreateManagedWidget( "CPmainRequestLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 478,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Request" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainRequestScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainRequestScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainRequestLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainTypeLabel */
	CPmainTypeLabel = XtVaCreateManagedWidget( "CPmainTypeLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 560,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Product Type" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainTypeScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainTypeScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainTypeLabel, (char *) UxCPmainQContext );


	/* Creation of CPmainStatusLabel */
	CPmainStatusLabel = XtVaCreateManagedWidget( "CPmainStatusLabel",
			xmLabelGadgetClass,
			CPjobs_form,
			XmNx, 677,
			XmNy, 10,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Status" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPmainStatusScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPmainStatusScrolledWin,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, CPjobs_label,
			XmNtopOffset, 6,
			NULL );
	UxPutContext( CPmainStatusLabel, (char *) UxCPmainQContext );


	/* Creation of CPstatusForm */
	CPstatusForm = XtVaCreateManagedWidget( "CPstatusForm",
			xmFormWidgetClass,
			panedWindow1,
			XmNx, 280,
			XmNy, 20,
			XmNallowResize, TRUE,
			XmNrubberPositioning, TRUE,
			NULL );
	UxPutContext( CPstatusForm, (char *) UxCPmainQContext );


	/* Creation of CPstatusForm_label */
	CPstatusForm_label = XtVaCreateManagedWidget( "CPstatusForm_label",
			xmLabelWidgetClass,
			CPstatusForm,
			XmNx, 0,
			XmNy, 10,
			XmNwidth, 974,
			XmNheight, 26,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			XmNfontList, UxConvertFontList("lucibis18" ),
			XmNtopOffset, 6,
			RES_CONVERT( XmNlabelString, "Subsystems" ),
			NULL );
	UxPutContext( CPstatusForm_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusRC */
	CPstatusRC = XtVaCreateManagedWidget( "CPstatusRC",
			xmRowColumnWidgetClass,
			CPstatusForm,
			XmNx, 680,
			XmNy, 20,
			XmNbottomOffset, 2,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNtopOffset, 6,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNpacking, XmPACK_COLUMN,
			XmNrightAttachment, XmATTACH_FORM,
			XmNorientation, XmHORIZONTAL,
			XmNadjustLast, FALSE,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 50,
			XmNtopWidget, CPstatusForm_label,
			XmNisAligned, FALSE,
			NULL );
	UxPutContext( CPstatusRC, (char *) UxCPmainQContext );


	/* Creation of CPstatusBlank_label */
	CPstatusBlank_label = XtVaCreateManagedWidget( "CPstatusBlank_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 60,
			XmNy, 10,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusBlank_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusNotRunning_label */
	CPstatusNotRunning_label = XtVaCreateManagedWidget( "CPstatusNotRunning_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Not Running" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusNotRunning_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusStarted_label */
	CPstatusStarted_label = XtVaCreateManagedWidget( "CPstatusStarted_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 56,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Started" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusStarted_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusReady_label */
	CPstatusReady_label = XtVaCreateManagedWidget( "CPstatusReady_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Ready" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusReady_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusRunning_label */
	CPstatusRunning_label = XtVaCreateManagedWidget( "CPstatusRunning_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 99,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Running" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusRunning_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusQC_label */
	CPstatusQC_label = XtVaCreateManagedWidget( "CPstatusQC_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 142,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Q/C" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusQC_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusHold_label */
	CPstatusHold_label = XtVaCreateManagedWidget( "CPstatusHold_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Hold" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusHold_label, (char *) UxCPmainQContext );


	/* Creation of CPstatusError_label */
	CPstatusError_label = XtVaCreateManagedWidget( "CPstatusError_label",
			xmLabelWidgetClass,
			CPstatusRC,
			XmNx, 56,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Error" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPstatusError_label, (char *) UxCPmainQContext );


	/* Creation of CPexternForm */
	CPexternForm = XtVaCreateManagedWidget( "CPexternForm",
			xmFormWidgetClass,
			panedWindow1,
			XmNx, 10,
			XmNy, 13,
			XmNrubberPositioning, TRUE,
			NULL );
	UxPutContext( CPexternForm, (char *) UxCPmainQContext );


	/* Creation of CPexternForm_label */
	CPexternForm_label = XtVaCreateManagedWidget( "CPexternForm_label",
			xmLabelWidgetClass,
			CPexternForm,
			XmNx, 22,
			XmNy, 87,
			XmNwidth, 20,
			XmNheight, 19,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			XmNfontList, UxConvertFontList("lucibis18" ),
			RES_CONVERT( XmNlabelString, "External Interfaces" ),
			XmNtopOffset, 10,
			NULL );
	UxPutContext( CPexternForm_label, (char *) UxCPmainQContext );


	/* Creation of CPexternRC */
	CPexternRC = XtVaCreateManagedWidget( "CPexternRC",
			xmRowColumnWidgetClass,
			CPexternForm,
			XmNx, 680,
			XmNy, 20,
			XmNbottomOffset, 2,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNtopOffset, 10,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNpacking, XmPACK_COLUMN,
			XmNorientation, XmHORIZONTAL,
			XmNadjustLast, FALSE,
			XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 50,
			XmNtopWidget, CPexternForm_label,
			XmNisAligned, FALSE,
			XmNrightAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( CPexternRC, (char *) UxCPmainQContext );


	/* Creation of CPexternBlank_label */
	CPexternBlank_label = XtVaCreateManagedWidget( "CPexternBlank_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 60,
			XmNy, 10,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternBlank_label, (char *) UxCPmainQContext );


	/* Creation of CPexternNotRunning_label */
	CPexternNotRunning_label = XtVaCreateManagedWidget( "CPexternNotRunning_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Not Running" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternNotRunning_label, (char *) UxCPmainQContext );


	/* Creation of CPexternStarted_label */
	CPexternStarted_label = XtVaCreateManagedWidget( "CPexternStarted_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 56,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Started" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternStarted_label, (char *) UxCPmainQContext );


	/* Creation of CPexternReady_label */
	CPexternReady_label = XtVaCreateManagedWidget( "CPexternReady_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Ready" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternReady_label, (char *) UxCPmainQContext );


	/* Creation of CPexternRunning_label */
	CPexternRunning_label = XtVaCreateManagedWidget( "CPexternRunning_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 99,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Running" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternRunning_label, (char *) UxCPmainQContext );


	/* Creation of CPexternQC_label */
	CPexternQC_label = XtVaCreateManagedWidget( "CPexternQC_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 142,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Q/C" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNsensitive, FALSE,
			NULL );
	UxPutContext( CPexternQC_label, (char *) UxCPmainQContext );


	/* Creation of CPexternHold_label */
	CPexternHold_label = XtVaCreateManagedWidget( "CPexternHold_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 13,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Hold" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternHold_label, (char *) UxCPmainQContext );


	/* Creation of CPexternError_label */
	CPexternError_label = XtVaCreateManagedWidget( "CPexternError_label",
			xmLabelWidgetClass,
			CPexternRC,
			XmNx, 56,
			XmNy, 13,
			XmNwidth, 110,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Error" ),
			XmNalignment, XmALIGNMENT_CENTER,
			NULL );
	UxPutContext( CPexternError_label, (char *) UxCPmainQContext );


	/* Creation of CPoptionsForm */
	CPoptionsForm = XtVaCreateManagedWidget( "CPoptionsForm",
			xmFormWidgetClass,
			panedWindow1,
			XmNx, 10,
			XmNy, 419,
			NULL );
	UxPutContext( CPoptionsForm, (char *) UxCPmainQContext );


	/* Creation of CPqueueSize_frame */
	CPqueueSize_frame = XtVaCreateManagedWidget( "CPqueueSize_frame",
			xmFrameWidgetClass,
			CPoptionsForm,
			XmNx, 5,
			XmNy, -1,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNrightPosition, 45,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( CPqueueSize_frame, (char *) UxCPmainQContext );


	/* Creation of CPqueueSize_form */
	CPqueueSize_form = XtVaCreateManagedWidget( "CPqueueSize_form",
			xmFormWidgetClass,
			CPqueueSize_frame,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 7,
			XmNy, 8,
			NULL );
	UxPutContext( CPqueueSize_form, (char *) UxCPmainQContext );


	/* Creation of queueInfo_rc2 */
	queueInfo_rc2 = XtVaCreateManagedWidget( "queueInfo_rc2",
			xmRowColumnWidgetClass,
			CPqueueSize_form,
			XmNx, 1,
			XmNy, 3,
			XmNwidth, 453,
			XmNorientation, XmHORIZONTAL,
			XmNborderWidth, 0,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			XmNnumColumns, 2,
			XmNpacking, XmPACK_COLUMN,
			NULL );
	UxPutContext( queueInfo_rc2, (char *) UxCPmainQContext );


	/* Creation of maxQueueSize_rc2 */
	maxQueueSize_rc2 = XtVaCreateManagedWidget( "maxQueueSize_rc2",
			xmRowColumnWidgetClass,
			queueInfo_rc2,
			XmNx, -11,
			XmNy, 2,
			XmNheight, 37,
			XmNorientation, XmHORIZONTAL,
			XmNadjustLast, FALSE,
			NULL );
	UxPutContext( maxQueueSize_rc2, (char *) UxCPmainQContext );


	/* Creation of maxQueueSize_label2 */
	maxQueueSize_label2 = XtVaCreateManagedWidget( "maxQueueSize_label2",
			xmLabelWidgetClass,
			maxQueueSize_rc2,
			XmNx, 20,
			XmNy, 5,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Max Queue Size:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( maxQueueSize_label2, (char *) UxCPmainQContext );


	/* Creation of maxQueueSize_tf */
	maxQueueSize_tf = XtVaCreateManagedWidget( "maxQueueSize_tf",
			xmTextFieldWidgetClass,
			maxQueueSize_rc2,
			XmNx, 88,
			XmNy, 3,
			XmNwidth, 119,
			XmNheight, 31,
			XmNcolumns, 10,
			NULL );
	XtAddCallback( maxQueueSize_tf, XmNactivateCallback,
		(XtCallbackProc) check_number,
		(XtPointer) UxCPmainQContext );
	XtAddCallback( maxQueueSize_tf, XmNmodifyVerifyCallback,
		(XtCallbackProc) check_number,
		(XtPointer) UxCPmainQContext );

	UxPutContext( maxQueueSize_tf, (char *) UxCPmainQContext );


	/* Creation of maxQueueSizeReset_pb */
	maxQueueSizeReset_pb = XtVaCreateManagedWidget( "maxQueueSizeReset_pb",
			xmPushButtonWidgetClass,
			maxQueueSize_rc2,
			XmNx, 208,
			XmNy, 15,
			XmNwidth, 27,
			XmNheight, 13,
			RES_CONVERT( XmNlabelString, "Reset" ),
			XmNsensitive, FALSE,
			NULL );
	XtAddCallback( maxQueueSizeReset_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_maxQueueSizeReset_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( maxQueueSizeReset_pb, (char *) UxCPmainQContext );


	/* Creation of currentQueueSize_rc2 */
	currentQueueSize_rc2 = XtVaCreateManagedWidget( "currentQueueSize_rc2",
			xmRowColumnWidgetClass,
			queueInfo_rc2,
			XmNx, -11,
			XmNy, 42,
			XmNheight, 37,
			XmNorientation, XmHORIZONTAL,
			XmNpacking, XmPACK_COLUMN,
			NULL );
	UxPutContext( currentQueueSize_rc2, (char *) UxCPmainQContext );


	/* Creation of currentQueueSize_label2 */
	currentQueueSize_label2 = XtVaCreateManagedWidget( "currentQueueSize_label2",
			xmLabelWidgetClass,
			currentQueueSize_rc2,
			XmNx, 20,
			XmNy, 5,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "Current # Jobs:" ),
			XmNalignment, XmALIGNMENT_END,
			NULL );
	UxPutContext( currentQueueSize_label2, (char *) UxCPmainQContext );


	/* Creation of currentQueueSizeValue_label */
	currentQueueSizeValue_label = XtVaCreateManagedWidget( "currentQueueSizeValue_label",
			xmLabelWidgetClass,
			currentQueueSize_rc2,
			XmNx, 13,
			XmNy, 13,
			XmNheight, 30,
			RES_CONVERT( XmNlabelString, "0" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			NULL );
	UxPutContext( currentQueueSizeValue_label, (char *) UxCPmainQContext );


	/* Creation of PPSjobControl_frame */
	PPSjobControl_frame = XtVaCreateManagedWidget( "PPSjobControl_frame",
			xmFrameWidgetClass,
			CPoptionsForm,
			XmNrightAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 45,
			NULL );
	UxPutContext( PPSjobControl_frame, (char *) UxCPmainQContext );


	/* Creation of PPSjobControlRC_form */
	PPSjobControlRC_form = XtVaCreateManagedWidget( "PPSjobControlRC_form",
			xmFormWidgetClass,
			PPSjobControl_frame,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 3,
			XmNy, 3,
			XmNwidth, 329,
			XmNheight, 184,
			NULL );
	UxPutContext( PPSjobControlRC_form, (char *) UxCPmainQContext );


	/* Creation of PPSjobControlAction_label */
	PPSjobControlAction_label = XtVaCreateManagedWidget( "PPSjobControlAction_label",
			xmLabelWidgetClass,
			PPSjobControlRC_form,
			RES_CONVERT( XmNlabelString, "Select an illuminated button to request a specific job type." ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNwidth, 336,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopOffset, 3,
			XmNfontList, UxConvertFontList("-adobe-helvetica-bold-o-normal--14-140-75-75-p-82-iso8859-1" ),
			NULL );
	UxPutContext( PPSjobControlAction_label, (char *) UxCPmainQContext );


	/* Creation of ppsJobRequestFrameCts_pb */
	ppsJobRequestFrameCts_pb = XtVaCreateManagedWidget( "ppsJobRequestFrameCts_pb",
			xmPushButtonWidgetClass,
			PPSjobControlRC_form,
			RES_CONVERT( XmNlabelString, "Frame\nContinuous Mode" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, PPSjobControlAction_label,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 75,
			NULL );
	XtAddCallback( ppsJobRequestFrameCts_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ppsJobRequestFrameCts_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( ppsJobRequestFrameCts_pb, (char *) UxCPmainQContext );


	/* Creation of ppsJobRequestScanCts_pb */
	ppsJobRequestScanCts_pb = XtVaCreateManagedWidget( "ppsJobRequestScanCts_pb",
			xmPushButtonWidgetClass,
			PPSjobControlRC_form,
			RES_CONVERT( XmNlabelString, "Scan\nContinuous Mode" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, PPSjobControlAction_label,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNrightPosition, 25,
			XmNleftAttachment, XmATTACH_FORM,
			NULL );
	XtAddCallback( ppsJobRequestScanCts_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ppsJobRequestScanCts_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( ppsJobRequestScanCts_pb, (char *) UxCPmainQContext );


	/* Creation of ppsJobRequestFrameSS_pb */
	ppsJobRequestFrameSS_pb = XtVaCreateManagedWidget( "ppsJobRequestFrameSS_pb",
			xmPushButtonWidgetClass,
			PPSjobControlRC_form,
			RES_CONVERT( XmNlabelString, "Frame\nScanSar Mode" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, PPSjobControlAction_label,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 50,
			XmNrightPosition, 75,
			NULL );
	XtAddCallback( ppsJobRequestFrameSS_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ppsJobRequestFrameSS_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( ppsJobRequestFrameSS_pb, (char *) UxCPmainQContext );


	/* Creation of ppsJobRequestScanSS_pb */
	ppsJobRequestScanSS_pb = XtVaCreateManagedWidget( "ppsJobRequestScanSS_pb",
			xmPushButtonWidgetClass,
			PPSjobControlRC_form,
			RES_CONVERT( XmNlabelString, "Scan\nScanSar Mode" ),
			XmNalignment, XmALIGNMENT_CENTER,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 25,
			XmNrightPosition, 50,
			XmNtopWidget, PPSjobControlAction_label,
			NULL );
	XtAddCallback( ppsJobRequestScanSS_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ppsJobRequestScanSS_pb,
		(XtPointer) UxCPmainQContext );

	UxPutContext( ppsJobRequestScanSS_pb, (char *) UxCPmainQContext );

	XtVaSetValues(CPmainMenu,
			XmNmenuHistory, NULL,
			XmNmenuHelpWidget, HelpPane_cb,
			NULL );

	XtVaSetValues(CPjobs_form,
			XmNpositionIndex, 1,
			NULL );

	/*** change dimple1 to black6 ***/
	XtVaSetValues(CPstatusForm,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/black6" ),
			NULL );

	XtVaSetValues(CPstatusForm_label,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/black6" ),
			NULL );

	XtVaSetValues(CPstatusRC,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/black6" ),
			NULL );

	XtVaSetValues(CPexternForm,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/black6" ),
			XmNpositionIndex, XmLAST_POSITION,
			NULL );

	XtVaSetValues(CPexternForm_label,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/black6" ),
			NULL );

	XtVaSetValues(CPexternRC,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/black6" ),
			NULL );

	XtVaSetValues(CPoptionsForm,
			XmNpositionIndex, 0,
			NULL );

	XtVaSetValues(CPqueueSize_form,
			XmNbackgroundPixmap, UxConvertPixmap("/usr/include/X11/bitmaps/2x2" ),
			NULL );


	XtAddCallback( CPmainQ, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPmainQContext);

	XmMainWindowSetAreas( CPmainQ, CPmainMenu, (Widget) NULL,
			(Widget) NULL, (Widget) NULL, panedWindow1 );

	return ( CPmainQ );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPmainQ( _UxUxParent )
	swidget	_UxUxParent;
{
	Widget                  rtrn;
	_UxCCPmainQ             *UxContext;
	static int		_Uxinit = 0;

	UxCPmainQContext = UxContext =
		(_UxCCPmainQ *) UxNewContext( sizeof(_UxCCPmainQ), False );

	UxParent = _UxUxParent;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		XmRepTypeInstallTearOffModelConverter();
		UxLoadResources( "CPmainQ.rf" );
		_Uxinit = 1;
	}

	rtrn = _Uxbuild_CPmainQ();

	return(rtrn);
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

