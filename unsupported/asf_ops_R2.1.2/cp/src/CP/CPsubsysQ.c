
/*******************************************************************************
       CPsubsysQ.c
       (Generated from interface file CPsubsysQ.i)
       Associated Header file: CPsubsysQ.h
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
#include <Xm/Separator.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DialogS.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

static char sccsid_CPsubsysQ_i[] = "@(#)CPsubsysQ.i	2.28 96/12/31 09:29:07"; 
extern void handleSubsysMenuDelete();
extern void handleSubsysMenuPerformQC();
extern void handleSubsysMenuRemoveHold();
extern void handleSubsysMenuMoveJob();
extern void handleSubsysMenuDetailedInfo();
extern void handleSubsysMenuReady(); 
extern void handleSubsysMenuQCspawn(); 
extern void handleMenuPrintCB(); 
extern void handleMenuStop(); 
extern void handleMenuReset(); 
extern void handleMenuPause();
extern void syncLists();
#include "version.h"
#include "help.h"


static	int _UxIfClassId;
/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

#define CONTEXT_MACRO_ACCESS 1
#include "CPsubsysQ.h"
#undef CONTEXT_MACRO_ACCESS

Widget	CPsubsysMenu;
Widget	File_pane;
Widget	Task_pane;
Widget	Help_pane;
Widget	Queue_pane;
Widget	CPsubsysQform;
Widget	QueueCurrentJobInfoLabel;
Widget	QueBottomInfoLabel_0;
Widget	CPqueJobIdScrolledWin;
Widget	CPqueJobIdList;
Widget	CPquePlatRevSeqScrolledWin;
Widget	CPquePlatRevSeqList;
Widget	CPqueFrameScrolledWin;
Widget	CPqueFrameList;
Widget	CPqueMediaScrolledWin;
Widget	CPqueMediaList;
Widget	CPqueModeScrolledWin;
Widget	CPqueModeList;
Widget	CPqueRequestScrolledWin;
Widget	CPqueRequestList;
Widget	CPqueTypeScrolledWin;
Widget	CPqueTypeList;
Widget	CPqueStatusScrolledWin;
Widget	CPqueStatusList;

/*******************************************************************************
Auxiliary code from the Declarations Editor:
*******************************************************************************/

Widget    getQueWidget(Widget mainWid, char *widName)
{
  Widget retWid = NULL;
  swidget UxThisWidget;
  _UxCCPsubsysQ           *UxSaveCtx, *UxContext;

    UxThisWidget = UxWidgetToSwidget(mainWid);
    UxSaveCtx = UxCPsubsysQContext;

    UxCPsubsysQContext = UxContext = (_UxCCPsubsysQ *)UxGetContext(UxThisWidget);
      
    if(strcmp(widName, "subsysQC_item") == 0)
           retWid = (Widget) subsysQC_item;

     if(strcmp(widName, "JobIdList") == 0)
           retWid = (Widget) CPqueJobIdList;

    if(strcmp(widName, "PlatRevSeqList") == 0)
           retWid = (Widget) CPquePlatRevSeqList;

    if(strcmp(widName, "FrameList") == 0)
           retWid = (Widget) CPqueFrameList;

    if(strcmp(widName, "ModeList") == 0)
           retWid = (Widget) CPqueModeList;

    if(strcmp(widName, "MediaList") == 0)
           retWid = (Widget) CPqueMediaList;

    if(strcmp(widName, "RequestList") == 0)
           retWid = (Widget) CPqueRequestList;

    if(strcmp(widName, "TypeList") == 0)
           retWid = (Widget) CPqueTypeList;

    if(strcmp(widName, "StatusList") == 0)
           retWid = (Widget) CPqueStatusList;

    if(strcmp(widName, "ScrollBar") == 0)
      XtVaGetValues(CPqueStatusList, XmNverticalScrollBar, &retWid, NULL);

    if(strcmp(widName, "InfoLabel") == 0)
           retWid = (Widget) QueueCurrentJobInfoLabel;
   
    if(strcmp(widName, "QueBottomInfoLabel_0") == 0)
           retWid = (Widget) QueBottomInfoLabel_0;

     UxCPsubsysQContext = UxSaveCtx;

    return(retWid);
}

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/

static void  activateCB_FilePrint_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleMenuPrintCB,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_FileExit_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{ performMenuItem(handleMenuStop,NULL,CPsubsysQ); }
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_SubsysLogBrowser_info_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{spawnLogBrowser("info", CPsubsysQ);}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_SubsysLogBrowser_debug_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{spawnLogBrowser("debug", CPsubsysQ);}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_SubsysLogBrowser_error_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{spawnLogBrowser("error", CPsubsysQ);}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_Main_win_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	showHelp(HELP_SPS_QUEUE_WIN, "");
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_SubsysWin_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	showHelp(HELP_SUBSYS_WIN, "");
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_JobStates_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	showHelp(HELP_JOB_STATES, CP_version_id);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_EventTrans_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	showHelp(HELP_EVENT_TRANSITIONS, "");
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_MenuTrans_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	showHelp(HELP_MENU_TRANSITIONS, "");
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_ProdInfo_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	showHelp(HELP_INFO, CP_version_id);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_SubsysReady_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleSubsysMenuReady,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_SubsysRest_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleMenuReset,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_CPsubsysMenu_p6_b3( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleMenuPause,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_CPsubsysMenu_p6_b4( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	doSendHeartbeat(CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_QueueRemoveHold_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleSubsysMenuRemoveHold,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_QueueMoveJob_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleSubsysMenuMoveJob,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_subsysQC_item( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleSubsysMenuQCspawn,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_QueueShowInfo_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleSubsysMenuDetailedInfo,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  activateCB_QueueCancelJob_pb( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	performMenuItem(handleSubsysMenuDelete,NULL,CPsubsysQ);
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueJobIdScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueJobIdList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueJobIdList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPquePlatRevSeqScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPquePlatRevSeqList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPquePlatRevSeqList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueFrameScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueFrameList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueFrameList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueMediaScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueMediaList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueMediaList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueModeScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueModeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueModeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueRequestScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueRequestList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueRequestList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueTypeScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueTypeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueTypeList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  createCB_CPqueStatusScrolledWin( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxContext = UxCPsubsysQContext;
	{
	/* setupScrollBars(CPsubsysQ); */
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  defaultActionCB_CPqueStatusList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); 
	}
	UxCPsubsysQContext = UxSaveCtx;
}

static void  browseSelectionCB_CPqueStatusList( UxWidget, UxClientData, UxCallbackArg )
	Widget		UxWidget;
	XtPointer	UxClientData, UxCallbackArg;

{
	_UxCCPsubsysQ           *UxSaveCtx, *UxContext;

	UxSaveCtx = UxCPsubsysQContext;
	UxCPsubsysQContext = UxContext =
			(_UxCCPsubsysQ *) UxGetContext( UxWidget );
	{
	/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;
	selectLine(CPsubsysQ, cbs->item_position); */
	syncLists(UxWidget, CPsubsysQ, UxCallbackArg);
	
	}
	UxCPsubsysQContext = UxSaveCtx;
}

/*******************************************************************************
       The 'build_' function creates all the widgets
       using the resource values specified in the Property Editor.
*******************************************************************************/

static Widget	_Uxbuild_CPsubsysQ()
{
	Widget		_UxParent;
	Widget		File_pane_shell;
	Widget		Task_pane_shell;
	Widget		CPsubsysLogBrowser_pane_shell;
	Widget		Help_pane_shell;
	Widget		Subsystems_pane_shell;
	Widget		Queue_pane_shell;


	/* Creation of CPsubsysQ */
	_UxParent = UxParent;
	if ( _UxParent == NULL )
	{
		_UxParent = UxTopLevel;
	}

	CPsubsysQ = XtVaCreatePopupShell( "CPsubsysQ",
			xmDialogShellWidgetClass,
			_UxParent,
			XmNx, 158,
			XmNtitle, "SUBSYS_xx Queue",
			XmNwidth, 975,
			XmNy, 86,
			XmNheight, 285,
			XmNallowShellResize, FALSE,
			XmNinitialState, IconicState,
			NULL );
	UxPutContext( CPsubsysQ, (char *) UxCPsubsysQContext );
	UxPutClassCode( CPsubsysQ, _UxIfClassId );


	/* Creation of mainWindow1 */
	mainWindow1 = XtVaCreateWidget( "mainWindow1",
			xmMainWindowWidgetClass,
			CPsubsysQ,
			XmNunitType, XmPIXELS,
			XmNheight, 210,
			XmNx, 0,
			XmNy, 0,
			NULL );
	UxPutContext( mainWindow1, (char *) UxCPsubsysQContext );


	/* Creation of CPsubsysMenu */
	CPsubsysMenu = XtVaCreateManagedWidget( "CPsubsysMenu",
			xmRowColumnWidgetClass,
			mainWindow1,
			XmNrowColumnType, XmMENU_BAR,
			XmNmenuAccelerator, "<KeyUp>F10",
			NULL );
	UxPutContext( CPsubsysMenu, (char *) UxCPsubsysQContext );


	/* Creation of File_pane */
	File_pane_shell = XtVaCreatePopupShell ("File_pane_shell",
			xmMenuShellWidgetClass, CPsubsysMenu,
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
	UxPutContext( File_pane, (char *) UxCPsubsysQContext );


	/* Creation of FilePrint_pb */
	FilePrint_pb = XtVaCreateManagedWidget( "FilePrint_pb",
			xmPushButtonGadgetClass,
			File_pane,
			RES_CONVERT( XmNlabelString, "Print" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( FilePrint_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FilePrint_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( FilePrint_pb, (char *) UxCPsubsysQContext );


	/* Creation of File_sep1 */
	File_sep1 = XtVaCreateManagedWidget( "File_sep1",
			xmSeparatorGadgetClass,
			File_pane,
			NULL );
	UxPutContext( File_sep1, (char *) UxCPsubsysQContext );


	/* Creation of FileExit_pb */
	FileExit_pb = XtVaCreateManagedWidget( "FileExit_pb",
			xmPushButtonGadgetClass,
			File_pane,
			RES_CONVERT( XmNlabelString, "Exit" ),
			RES_CONVERT( XmNmnemonic, "x" ),
			NULL );
	XtAddCallback( FileExit_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_FileExit_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( FileExit_pb, (char *) UxCPsubsysQContext );


	/* Creation of File_cb */
	File_cb = XtVaCreateManagedWidget( "File_cb",
			xmCascadeButtonWidgetClass,
			CPsubsysMenu,
			RES_CONVERT( XmNlabelString, "File" ),
			XmNsubMenuId, File_pane,
			RES_CONVERT( XmNmnemonic, "F" ),
			NULL );
	UxPutContext( File_cb, (char *) UxCPsubsysQContext );


	/* Creation of Task_pane */
	Task_pane_shell = XtVaCreatePopupShell ("Task_pane_shell",
			xmMenuShellWidgetClass, CPsubsysMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Task_pane = XtVaCreateWidget( "Task_pane",
			xmRowColumnWidgetClass,
			Task_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( Task_pane, (char *) UxCPsubsysQContext );


	/* Creation of CPsubsysLogBrowser_pane */
	CPsubsysLogBrowser_pane_shell = XtVaCreatePopupShell ("CPsubsysLogBrowser_pane_shell",
			xmMenuShellWidgetClass, Task_pane,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	CPsubsysLogBrowser_pane = XtVaCreateWidget( "CPsubsysLogBrowser_pane",
			xmRowColumnWidgetClass,
			CPsubsysLogBrowser_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( CPsubsysLogBrowser_pane, (char *) UxCPsubsysQContext );


	/* Creation of SubsysLogBrowser_info_pb */
	SubsysLogBrowser_info_pb = XtVaCreateManagedWidget( "SubsysLogBrowser_info_pb",
			xmPushButtonWidgetClass,
			CPsubsysLogBrowser_pane,
			RES_CONVERT( XmNlabelString, "Info" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( SubsysLogBrowser_info_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysLogBrowser_info_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( SubsysLogBrowser_info_pb, (char *) UxCPsubsysQContext );


	/* Creation of SubsysLogBrowser_debug_pb */
	SubsysLogBrowser_debug_pb = XtVaCreateManagedWidget( "SubsysLogBrowser_debug_pb",
			xmPushButtonWidgetClass,
			CPsubsysLogBrowser_pane,
			RES_CONVERT( XmNlabelString, "Debug" ),
			RES_CONVERT( XmNmnemonic, "D" ),
			NULL );
	XtAddCallback( SubsysLogBrowser_debug_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysLogBrowser_debug_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( SubsysLogBrowser_debug_pb, (char *) UxCPsubsysQContext );


	/* Creation of SubsysLogBrowser_error_pb */
	SubsysLogBrowser_error_pb = XtVaCreateManagedWidget( "SubsysLogBrowser_error_pb",
			xmPushButtonWidgetClass,
			CPsubsysLogBrowser_pane,
			RES_CONVERT( XmNlabelString, "Error" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			NULL );
	XtAddCallback( SubsysLogBrowser_error_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysLogBrowser_error_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( SubsysLogBrowser_error_pb, (char *) UxCPsubsysQContext );


	/* Creation of TaskLogBrowser_cb */
	TaskLogBrowser_cb = XtVaCreateManagedWidget( "TaskLogBrowser_cb",
			xmCascadeButtonWidgetClass,
			Task_pane,
			RES_CONVERT( XmNlabelString, "Log Browser" ),
			XmNsubMenuId, CPsubsysLogBrowser_pane,
			RES_CONVERT( XmNmnemonic, "L" ),
			NULL );
	UxPutContext( TaskLogBrowser_cb, (char *) UxCPsubsysQContext );


	/* Creation of Task_cb */
	Task_cb = XtVaCreateManagedWidget( "Task_cb",
			xmCascadeButtonWidgetClass,
			CPsubsysMenu,
			RES_CONVERT( XmNlabelString, "Tools" ),
			RES_CONVERT( XmNmnemonic, "T" ),
			XmNsubMenuId, Task_pane,
			NULL );
	UxPutContext( Task_cb, (char *) UxCPsubsysQContext );


	/* Creation of Help_pane */
	Help_pane_shell = XtVaCreatePopupShell ("Help_pane_shell",
			xmMenuShellWidgetClass, CPsubsysMenu,
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
	UxPutContext( Help_pane, (char *) UxCPsubsysQContext );


	/* Creation of Main_win_pb */
	Main_win_pb = XtVaCreateManagedWidget( "Main_win_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "SPS Processing Queue Window" ),
			RES_CONVERT( XmNmnemonic, "W" ),
			NULL );
	XtAddCallback( Main_win_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_Main_win_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( Main_win_pb, (char *) UxCPsubsysQContext );


	/* Creation of SubsysWin_pb */
	SubsysWin_pb = XtVaCreateManagedWidget( "SubsysWin_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Subsystem Windows" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			NULL );
	XtAddCallback( SubsysWin_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysWin_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( SubsysWin_pb, (char *) UxCPsubsysQContext );


	/* Creation of JobStates_pb */
	JobStates_pb = XtVaCreateManagedWidget( "JobStates_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Job States" ),
			RES_CONVERT( XmNmnemonic, "J" ),
			NULL );
	XtAddCallback( JobStates_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_JobStates_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( JobStates_pb, (char *) UxCPsubsysQContext );


	/* Creation of EventTrans_pb */
	EventTrans_pb = XtVaCreateManagedWidget( "EventTrans_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Event Transitions" ),
			RES_CONVERT( XmNmnemonic, "E" ),
			NULL );
	XtAddCallback( EventTrans_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_EventTrans_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( EventTrans_pb, (char *) UxCPsubsysQContext );


	/* Creation of MenuTrans_pb */
	MenuTrans_pb = XtVaCreateManagedWidget( "MenuTrans_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Menu Transitions" ),
			RES_CONVERT( XmNmnemonic, "M" ),
			NULL );
	XtAddCallback( MenuTrans_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_MenuTrans_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( MenuTrans_pb, (char *) UxCPsubsysQContext );


	/* Creation of ProdInfo_pb */
	ProdInfo_pb = XtVaCreateManagedWidget( "ProdInfo_pb",
			xmPushButtonGadgetClass,
			Help_pane,
			RES_CONVERT( XmNlabelString, "Product Information" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( ProdInfo_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_ProdInfo_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( ProdInfo_pb, (char *) UxCPsubsysQContext );


	/* Creation of Help_cb */
	Help_cb = XtVaCreateManagedWidget( "Help_cb",
			xmCascadeButtonWidgetClass,
			CPsubsysMenu,
			RES_CONVERT( XmNlabelString, "Help" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			XmNsubMenuId, Help_pane,
			NULL );
	UxPutContext( Help_cb, (char *) UxCPsubsysQContext );


	/* Creation of Subsystems_pane */
	Subsystems_pane_shell = XtVaCreatePopupShell ("Subsystems_pane_shell",
			xmMenuShellWidgetClass, CPsubsysMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Subsystems_pane = XtVaCreateWidget( "Subsystems_pane",
			xmRowColumnWidgetClass,
			Subsystems_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( Subsystems_pane, (char *) UxCPsubsysQContext );


	/* Creation of SubsysReady_pb */
	SubsysReady_pb = XtVaCreateManagedWidget( "SubsysReady_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Ready" ),
			RES_CONVERT( XmNmnemonic, "R" ),
			NULL );
	XtAddCallback( SubsysReady_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysReady_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( SubsysReady_pb, (char *) UxCPsubsysQContext );


	/* Creation of SubsysRest_pb */
	SubsysRest_pb = XtVaCreateManagedWidget( "SubsysRest_pb",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Reset" ),
			RES_CONVERT( XmNmnemonic, "t" ),
			NULL );
	XtAddCallback( SubsysRest_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_SubsysRest_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( SubsysRest_pb, (char *) UxCPsubsysQContext );


	/* Creation of CPsubsysMenu_p6_b3 */
	CPsubsysMenu_p6_b3 = XtVaCreateManagedWidget( "CPsubsysMenu_p6_b3",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Pause" ),
			RES_CONVERT( XmNmnemonic, "P" ),
			NULL );
	XtAddCallback( CPsubsysMenu_p6_b3, XmNactivateCallback,
		(XtCallbackProc) activateCB_CPsubsysMenu_p6_b3,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPsubsysMenu_p6_b3, (char *) UxCPsubsysQContext );


	/* Creation of CPsubsysMenu_p6_b4 */
	CPsubsysMenu_p6_b4 = XtVaCreateManagedWidget( "CPsubsysMenu_p6_b4",
			xmPushButtonGadgetClass,
			Subsystems_pane,
			RES_CONVERT( XmNlabelString, "Send Heartbeat" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			NULL );
	XtAddCallback( CPsubsysMenu_p6_b4, XmNactivateCallback,
		(XtCallbackProc) activateCB_CPsubsysMenu_p6_b4,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPsubsysMenu_p6_b4, (char *) UxCPsubsysQContext );


	/* Creation of CPsubsysMenu_top_b1 */
	CPsubsysMenu_top_b1 = XtVaCreateManagedWidget( "CPsubsysMenu_top_b1",
			xmCascadeButtonWidgetClass,
			CPsubsysMenu,
			RES_CONVERT( XmNlabelString, "Subsystem" ),
			RES_CONVERT( XmNmnemonic, "S" ),
			XmNsubMenuId, Subsystems_pane,
			NULL );
	UxPutContext( CPsubsysMenu_top_b1, (char *) UxCPsubsysQContext );


	/* Creation of Queue_pane */
	Queue_pane_shell = XtVaCreatePopupShell ("Queue_pane_shell",
			xmMenuShellWidgetClass, CPsubsysMenu,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL );

	Queue_pane = XtVaCreateWidget( "Queue_pane",
			xmRowColumnWidgetClass,
			Queue_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			NULL );
	UxPutContext( Queue_pane, (char *) UxCPsubsysQContext );


	/* Creation of QueueRemoveHold_pb */
	QueueRemoveHold_pb = XtVaCreateManagedWidget( "QueueRemoveHold_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Remove Hold" ),
			RES_CONVERT( XmNmnemonic, "H" ),
			NULL );
	XtAddCallback( QueueRemoveHold_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_QueueRemoveHold_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( QueueRemoveHold_pb, (char *) UxCPsubsysQContext );


	/* Creation of QueueMoveJob_pb */
	QueueMoveJob_pb = XtVaCreateManagedWidget( "QueueMoveJob_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Move Job" ),
			RES_CONVERT( XmNmnemonic, "M" ),
			NULL );
	XtAddCallback( QueueMoveJob_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_QueueMoveJob_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( QueueMoveJob_pb, (char *) UxCPsubsysQContext );


	/* Creation of subsysQC_item */
	subsysQC_item = XtVaCreateManagedWidget( "subsysQC_item",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Perform QC" ),
			RES_CONVERT( XmNmnemonic, "Q" ),
			NULL );
	XtAddCallback( subsysQC_item, XmNactivateCallback,
		(XtCallbackProc) activateCB_subsysQC_item,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( subsysQC_item, (char *) UxCPsubsysQContext );


	/* Creation of QueueShowInfo_pb */
	QueueShowInfo_pb = XtVaCreateManagedWidget( "QueueShowInfo_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Show Detailed Info" ),
			RES_CONVERT( XmNmnemonic, "I" ),
			NULL );
	XtAddCallback( QueueShowInfo_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_QueueShowInfo_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( QueueShowInfo_pb, (char *) UxCPsubsysQContext );


	/* Creation of QueueCancelJob_pb */
	QueueCancelJob_pb = XtVaCreateManagedWidget( "QueueCancelJob_pb",
			xmPushButtonGadgetClass,
			Queue_pane,
			RES_CONVERT( XmNlabelString, "Cancel Job" ),
			RES_CONVERT( XmNmnemonic, "C" ),
			NULL );
	XtAddCallback( QueueCancelJob_pb, XmNactivateCallback,
		(XtCallbackProc) activateCB_QueueCancelJob_pb,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( QueueCancelJob_pb, (char *) UxCPsubsysQContext );


	/* Creation of Queue_cb */
	Queue_cb = XtVaCreateManagedWidget( "Queue_cb",
			xmCascadeButtonWidgetClass,
			CPsubsysMenu,
			RES_CONVERT( XmNlabelString, "Jobs" ),
			RES_CONVERT( XmNmnemonic, "J" ),
			XmNsubMenuId, Queue_pane,
			NULL );
	UxPutContext( Queue_cb, (char *) UxCPsubsysQContext );


	/* Creation of CPsubsysQform */
	CPsubsysQform = XtVaCreateManagedWidget( "CPsubsysQform",
			xmFormWidgetClass,
			mainWindow1,
			NULL );
	UxPutContext( CPsubsysQform, (char *) UxCPsubsysQContext );


	/* Creation of form1 */
	form1 = XtVaCreateManagedWidget( "form1",
			xmFormWidgetClass,
			CPsubsysQform,
			XmNresizePolicy, XmRESIZE_NONE,
			XmNx, 280,
			XmNy, 10,
			XmNwidth, 210,
			XmNheight, 30,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 5,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 5,
			NULL );
	UxPutContext( form1, (char *) UxCPsubsysQContext );


	/* Creation of QueueCurrentJobInfoLabel */
	QueueCurrentJobInfoLabel = XtVaCreateManagedWidget( "QueueCurrentJobInfoLabel",
			xmLabelGadgetClass,
			form1,
			XmNx, 20,
			XmNy, 3,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Status: Started" ),
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNrightPosition, 50,
			XmNtopAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( QueueCurrentJobInfoLabel, (char *) UxCPsubsysQContext );


	/* Creation of QueBottomInfoLabel_0 */
	QueBottomInfoLabel_0 = XtVaCreateManagedWidget( "QueBottomInfoLabel_0",
			xmLabelGadgetClass,
			form1,
			XmNx, 450,
			XmNy, 3,
			XmNheight, 25,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftOffset, 0,
			XmNleftPosition, 50,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_FORM,
			NULL );
	UxPutContext( QueBottomInfoLabel_0, (char *) UxCPsubsysQContext );


	/* Creation of separator1 */
	separator1 = XtVaCreateManagedWidget( "separator1",
			xmSeparatorWidgetClass,
			CPsubsysQform,
			XmNx, 0,
			XmNy, 30,
			XmNheight, 10,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 5,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 5,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, form1,
			XmNseparatorType, XmSHADOW_ETCHED_OUT,
			NULL );
	UxPutContext( separator1, (char *) UxCPsubsysQContext );


	/* Creation of form2 */
	form2 = XtVaCreateManagedWidget( "form2",
			xmFormWidgetClass,
			CPsubsysQform,
			XmNx, 0,
			XmNallowOverlap, FALSE,
			XmNrubberPositioning, FALSE,
			XmNresizePolicy, XmRESIZE_ANY,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 5,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 5,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator1,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 5,
			NULL );
	UxPutContext( form2, (char *) UxCPsubsysQContext );


	/* Creation of CPqueJobIdScrolledWin */
	CPqueJobIdScrolledWin = XtVaCreateManagedWidget( "CPqueJobIdScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 14,
			XmNy, 32,
			XmNwidth, 85,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNleftOffset, 10,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueJobIdScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueJobIdScrolledWin( CPqueJobIdScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueJobIdList */
	CPqueJobIdList = XtVaCreateManagedWidget( "CPqueJobIdList",
			xmListWidgetClass,
			CPqueJobIdScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueJobIdList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueJobIdList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueJobIdList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueJobIdList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueJobIdList, (char *) UxCPsubsysQContext );


	/* Creation of CPquePlatRevSeqScrolledWin */
	CPquePlatRevSeqScrolledWin = XtVaCreateManagedWidget( "CPquePlatRevSeqScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 106,
			XmNy, 32,
			XmNwidth, 125,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPqueJobIdScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPquePlatRevSeqScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPquePlatRevSeqScrolledWin( CPquePlatRevSeqScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPquePlatRevSeqList */
	CPquePlatRevSeqList = XtVaCreateManagedWidget( "CPquePlatRevSeqList",
			xmListWidgetClass,
			CPquePlatRevSeqScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPquePlatRevSeqList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPquePlatRevSeqList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPquePlatRevSeqList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPquePlatRevSeqList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPquePlatRevSeqList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueFrameScrolledWin */
	CPqueFrameScrolledWin = XtVaCreateManagedWidget( "CPqueFrameScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 236,
			XmNy, 32,
			XmNwidth, 75,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPquePlatRevSeqScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueFrameScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueFrameScrolledWin( CPqueFrameScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueFrameList */
	CPqueFrameList = XtVaCreateManagedWidget( "CPqueFrameList",
			xmListWidgetClass,
			CPqueFrameScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueFrameList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueFrameList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueFrameList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueFrameList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueFrameList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueMediaScrolledWin */
	CPqueMediaScrolledWin = XtVaCreateManagedWidget( "CPqueMediaScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 316,
			XmNy, 32,
			XmNwidth, 200,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPqueFrameScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueMediaScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueMediaScrolledWin( CPqueMediaScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueMediaList */
	CPqueMediaList = XtVaCreateManagedWidget( "CPqueMediaList",
			xmListWidgetClass,
			CPqueMediaScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueMediaList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueMediaList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueMediaList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueMediaList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueMediaList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueModeScrolledWin */
	CPqueModeScrolledWin = XtVaCreateManagedWidget( "CPqueModeScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 406,
			XmNy, 32,
			XmNwidth, 65,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPqueMediaScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueModeScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueModeScrolledWin( CPqueModeScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueModeList */
	CPqueModeList = XtVaCreateManagedWidget( "CPqueModeList",
			xmListWidgetClass,
			CPqueModeScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueModeList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueModeList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueModeList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueModeList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueModeList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueRequestScrolledWin */
	CPqueRequestScrolledWin = XtVaCreateManagedWidget( "CPqueRequestScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 456,
			XmNy, 32,
			XmNwidth, 90,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPqueModeScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueRequestScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueRequestScrolledWin( CPqueRequestScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueRequestList */
	CPqueRequestList = XtVaCreateManagedWidget( "CPqueRequestList",
			xmListWidgetClass,
			CPqueRequestScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueRequestList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueRequestList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueRequestList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueRequestList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueRequestList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueTypeScrolledWin */
	CPqueTypeScrolledWin = XtVaCreateManagedWidget( "CPqueTypeScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 536,
			XmNy, 32,
			XmNwidth, 145,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPqueRequestScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueTypeScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueTypeScrolledWin( CPqueTypeScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueTypeList */
	CPqueTypeList = XtVaCreateManagedWidget( "CPqueTypeList",
			xmListWidgetClass,
			CPqueTypeScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNstringDirection, XmSTRING_DIRECTION_L_TO_R,
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueTypeList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueTypeList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueTypeList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueTypeList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueTypeList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueStatusScrolledWin */
	CPqueStatusScrolledWin = XtVaCreateManagedWidget( "CPqueStatusScrolledWin",
			xmScrolledWindowWidgetClass,
			form2,
			XmNscrollingPolicy, XmAPPLICATION_DEFINED,
			XmNx, 651,
			XmNy, 32,
			XmNwidth, 215,
			XmNheight, 90,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftOffset, 5,
			XmNleftWidget, CPqueTypeScrolledWin,
			XmNvisualPolicy, XmVARIABLE,
			XmNspacing, 0,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 10,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNbottomOffset, 10,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 30,
			XmNshadowThickness, 2,
			NULL );
	UxPutContext( CPqueStatusScrolledWin, (char *) UxCPsubsysQContext );

	createCB_CPqueStatusScrolledWin( CPqueStatusScrolledWin,
			(XtPointer) UxCPsubsysQContext, (XtPointer) NULL );


	/* Creation of CPqueStatusList */
	CPqueStatusList = XtVaCreateManagedWidget( "CPqueStatusList",
			xmListWidgetClass,
			CPqueStatusScrolledWin,
			XmNfontList, UxConvertFontList("helvb10" ),
			XmNlistSizePolicy, XmCONSTANT,
			XmNshadowThickness, 0,
			XmNhighlightThickness, 0,
			XmNlistMarginWidth, 2,
			XmNscrollBarDisplayPolicy, XmSTATIC,
			NULL );
	XtAddCallback( CPqueStatusList, XmNdefaultActionCallback,
		(XtCallbackProc) defaultActionCB_CPqueStatusList,
		(XtPointer) UxCPsubsysQContext );
	XtAddCallback( CPqueStatusList, XmNbrowseSelectionCallback,
		(XtCallbackProc) browseSelectionCB_CPqueStatusList,
		(XtPointer) UxCPsubsysQContext );

	UxPutContext( CPqueStatusList, (char *) UxCPsubsysQContext );


	/* Creation of CPqueJobIdLabel */
	CPqueJobIdLabel = XtVaCreateManagedWidget( "CPqueJobIdLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 10,
			XmNy, 10,
			XmNwidth, 55,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Job ID" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNrightWidget, CPqueJobIdScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueJobIdScrolledWin,
			NULL );
	UxPutContext( CPqueJobIdLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPquePlatRevSeqLabel */
	CPquePlatRevSeqLabel = XtVaCreateManagedWidget( "CPquePlatRevSeqLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 100,
			XmNy, 10,
			XmNwidth, 92,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Plat/Rev/Seq" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPquePlatRevSeqScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPquePlatRevSeqScrolledWin,
			NULL );
	UxPutContext( CPquePlatRevSeqLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPqueFrameLabel */
	CPqueFrameLabel = XtVaCreateManagedWidget( "CPqueFrameLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 230,
			XmNy, 10,
			XmNwidth, 53,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Frame" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueFrameScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPqueFrameScrolledWin,
			NULL );
	UxPutContext( CPqueFrameLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPqueMediaLabel */
	CPqueMediaLabel = XtVaCreateManagedWidget( "CPqueMediaLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 310,
			XmNy, 10,
			XmNwidth, 51,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Media" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueMediaScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPqueMediaScrolledWin,
			NULL );
	UxPutContext( CPqueMediaLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPqueModeLabel */
	CPqueModeLabel = XtVaCreateManagedWidget( "CPqueModeLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 390,
			XmNy, 10,
			XmNwidth, 40,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Mode" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueModeScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPqueModeScrolledWin,
			NULL );
	UxPutContext( CPqueModeLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPqueRequestLabel */
	CPqueRequestLabel = XtVaCreateManagedWidget( "CPqueRequestLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 450,
			XmNy, 10,
			XmNwidth, 77,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Request" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueRequestScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPqueRequestScrolledWin,
			NULL );
	UxPutContext( CPqueRequestLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPqueTypeLabel */
	CPqueTypeLabel = XtVaCreateManagedWidget( "CPqueTypeLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 530,
			XmNy, 10,
			XmNwidth, 100,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Product Type" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueTypeScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPqueTypeScrolledWin,
			NULL );
	UxPutContext( CPqueTypeLabel, (char *) UxCPsubsysQContext );


	/* Creation of CPqueStatusLabel */
	CPqueStatusLabel = XtVaCreateManagedWidget( "CPqueStatusLabel",
			xmLabelGadgetClass,
			form2,
			XmNx, 650,
			XmNy, 8,
			XmNwidth, 43,
			XmNheight, 20,
			RES_CONVERT( XmNlabelString, "Status" ),
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNfontList, UxConvertFontList("helvbo10" ),
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, CPqueStatusScrolledWin,
			XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNrightWidget, CPqueStatusScrolledWin,
			NULL );
	UxPutContext( CPqueStatusLabel, (char *) UxCPsubsysQContext );

	XtVaSetValues(CPsubsysMenu,
			XmNmenuHelpWidget, Help_cb,
			NULL );

	XtVaSetValues(File_pane,
			XmNpositionIndex, 0,
			NULL );

	XtVaSetValues(Task_pane,
			XmNpositionIndex, 1,
			NULL );

	XtVaSetValues(Subsystems_pane,
			XmNpositionIndex, 3,
			NULL );

	XtVaSetValues(Queue_pane,
			XmNpositionIndex, 2,
			NULL );


	XtAddCallback( CPsubsysQ, XmNdestroyCallback,
		(XtCallbackProc) UxDestroyContextCB,
		(XtPointer) UxCPsubsysQContext);

	XmMainWindowSetAreas( mainWindow1, CPsubsysMenu, (Widget) NULL,
			(Widget) NULL, (Widget) NULL, CPsubsysQform );

	return ( CPsubsysQ );
}

/*******************************************************************************
       The following is the 'Interface function' which is the
       external entry point for creating this interface.
       This function should be called from your application or from
       a callback function.
*******************************************************************************/

Widget	create_CPsubsysQ( _UxUxParent, _UxnamePtr )
	swidget	_UxUxParent;
	char	*_UxnamePtr;
{
	Widget                  rtrn;
	_UxCCPsubsysQ           *UxContext;
	static int		_Uxinit = 0;

	UxCPsubsysQContext = UxContext =
		(_UxCCPsubsysQ *) UxNewContext( sizeof(_UxCCPsubsysQ), False );

	UxParent = _UxUxParent;
	namePtr = _UxnamePtr;

	if ( ! _Uxinit )
	{
		_UxIfClassId = UxNewInterfaceClassId();
		_Uxinit = 1;
	}

	{
		Widget widlist[10];
		int i;
		rtrn = _Uxbuild_CPsubsysQ();

		i = 0;
		
		widlist[i++] = (Widget) CPsubsysQform;
		widlist[i++] = (Widget) CPsubsysMenu;
		widlist[i++] = (Widget) File_pane;
		widlist[i++] = (Widget) Help_pane;
		widlist[i++] = (Widget) Task_pane;
		widlist[i++] = (Widget) Queue_pane;
		widlist[i++] = (Widget) Subsystems_pane;
		widlist[i++] = (Widget) CPsubsysMenu;
		widlist[i++] = (Widget) CPsubsysLogBrowser_pane;
		widlist[i++] = NULL;
		
		colorCodeQUEwindow(namePtr,  widlist); 
		return(rtrn);
	}
}

/*******************************************************************************
       END OF FILE
*******************************************************************************/

