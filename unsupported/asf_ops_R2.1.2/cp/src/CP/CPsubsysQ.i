! UIMX ascii 2.9 key: 5315                                                      

*CPsubsysQ.class: dialogShell
*CPsubsysQ.classinc:
*CPsubsysQ.classspec:
*CPsubsysQ.classmembers:
*CPsubsysQ.classconstructor:
*CPsubsysQ.classdestructor:
*CPsubsysQ.gbldecl: #include <stdio.h>\
static char sccsid_CPsubsysQ_i[] = "@(#)CPsubsysQ.i	2.28 96/12/31 09:29:07"; \
extern void handleSubsysMenuDelete();\
extern void handleSubsysMenuPerformQC();\
extern void handleSubsysMenuRemoveHold();\
extern void handleSubsysMenuMoveJob();\
extern void handleSubsysMenuDetailedInfo();\
extern void handleSubsysMenuReady(); \
extern void handleSubsysMenuQCspawn(); \
extern void handleMenuPrintCB(); \
extern void handleMenuStop(); \
extern void handleMenuReset(); \
extern void handleMenuPause();\
extern void syncLists();\
#include "version.h"\
#include "help.h"\

*CPsubsysQ.ispecdecl:
*CPsubsysQ.funcdecl: swidget create_CPsubsysQ(UxParent, namePtr)\
swidget UxParent;\
char *namePtr;
*CPsubsysQ.funcname: create_CPsubsysQ
*CPsubsysQ.funcdef: "swidget", "<create_CPsubsysQ>(%)"
*CPsubsysQ.argdecl: swidget UxParent;\
char *namePtr;
*CPsubsysQ.arglist: UxParent, namePtr
*CPsubsysQ.arglist.UxParent: "swidget", "%UxParent%"
*CPsubsysQ.arglist.namePtr: "char", "*%namePtr%"
*CPsubsysQ.icode: Widget widlist[10];\
int i;
*CPsubsysQ.fcode: i = 0;\
\
widlist[i++] = (Widget) CPsubsysQform;\
widlist[i++] = (Widget) CPsubsysMenu;\
widlist[i++] = (Widget) File_pane;\
widlist[i++] = (Widget) Help_pane;\
widlist[i++] = (Widget) Task_pane;\
widlist[i++] = (Widget) Queue_pane;\
widlist[i++] = (Widget) Subsystems_pane;\
widlist[i++] = (Widget) CPsubsysMenu;\
widlist[i++] = (Widget) CPsubsysLogBrowser_pane;\
widlist[i++] = NULL;\
\
colorCodeQUEwindow(namePtr,  widlist); \
return(rtrn);\

*CPsubsysQ.auxdecl: Widget    getQueWidget(Widget mainWid, char *widName)\
{\
  Widget retWid = NULL;\
  swidget UxThisWidget;\
  _UxCCPsubsysQ           *UxSaveCtx, *UxContext;\
\
    UxThisWidget = UxWidgetToSwidget(mainWid);\
    UxSaveCtx = UxCPsubsysQContext;\
\
    UxCPsubsysQContext = UxContext = (_UxCCPsubsysQ *)UxGetContext(UxThisWidget);\
      \
    if(strcmp(widName, "subsysQC_item") == 0)\
           retWid = (Widget) subsysQC_item;\
\
     if(strcmp(widName, "JobIdList") == 0)\
           retWid = (Widget) CPqueJobIdList;\
\
    if(strcmp(widName, "PlatRevSeqList") == 0)\
           retWid = (Widget) CPquePlatRevSeqList;\
\
    if(strcmp(widName, "FrameList") == 0)\
           retWid = (Widget) CPqueFrameList;\
\
    if(strcmp(widName, "ModeList") == 0)\
           retWid = (Widget) CPqueModeList;\
\
    if(strcmp(widName, "MediaList") == 0)\
           retWid = (Widget) CPqueMediaList;\
\
    if(strcmp(widName, "RequestList") == 0)\
           retWid = (Widget) CPqueRequestList;\
\
    if(strcmp(widName, "TypeList") == 0)\
           retWid = (Widget) CPqueTypeList;\
\
    if(strcmp(widName, "StatusList") == 0)\
           retWid = (Widget) CPqueStatusList;\
\
    if(strcmp(widName, "ScrollBar") == 0)\
      XtVaGetValues(CPqueStatusList, XmNverticalScrollBar, &retWid, NULL);\
\
    if(strcmp(widName, "InfoLabel") == 0)\
           retWid = (Widget) QueueCurrentJobInfoLabel;\
   \
    if(strcmp(widName, "QueBottomInfoLabel_0") == 0)\
           retWid = (Widget) QueBottomInfoLabel_0;\
\
     UxCPsubsysQContext = UxSaveCtx;\
\
    return(retWid);\
}\

*CPsubsysQ.static: true
*CPsubsysQ.name: CPsubsysQ
*CPsubsysQ.parent: NO_PARENT
*CPsubsysQ.parentExpression: UxParent
*CPsubsysQ.x: 158
*CPsubsysQ.title: "SUBSYS_xx Queue"
*CPsubsysQ.createManaged: "false"
*CPsubsysQ.width: 975
*CPsubsysQ.y: 86
*CPsubsysQ.height: 285
*CPsubsysQ.allowShellResize: "false"
*CPsubsysQ.initialState: "IconicState"

*mainWindow1.class: mainWindow
*mainWindow1.static: true
*mainWindow1.name: mainWindow1
*mainWindow1.parent: CPsubsysQ
*mainWindow1.unitType: "pixels"
*mainWindow1.height: 210
*mainWindow1.x: 0
*mainWindow1.y: 0

*CPsubsysMenu.class: rowColumn
*CPsubsysMenu.name.source: public
*CPsubsysMenu.static: false
*CPsubsysMenu.name: CPsubsysMenu
*CPsubsysMenu.parent: mainWindow1
*CPsubsysMenu.rowColumnType: "menu_bar"
*CPsubsysMenu.menuAccelerator: "<KeyUp>F10"
*CPsubsysMenu.menuHelpWidget: "Help_cb"

*File_pane.class: rowColumn
*File_pane.name.source: public
*File_pane.static: false
*File_pane.name: File_pane
*File_pane.parent: CPsubsysMenu
*File_pane.rowColumnType: "menu_pulldown"
*File_pane.positionIndex: 0

*FilePrint_pb.class: pushButtonGadget
*FilePrint_pb.static: true
*FilePrint_pb.name: FilePrint_pb
*FilePrint_pb.parent: File_pane
*FilePrint_pb.labelString: "Print"
*FilePrint_pb.mnemonic: "P"
*FilePrint_pb.activateCallback: performMenuItem(handleMenuPrintCB,NULL,CPsubsysQ);

*File_sep1.class: separatorGadget
*File_sep1.static: true
*File_sep1.name: File_sep1
*File_sep1.parent: File_pane

*FileExit_pb.class: pushButtonGadget
*FileExit_pb.static: true
*FileExit_pb.name: FileExit_pb
*FileExit_pb.parent: File_pane
*FileExit_pb.labelString: "Exit"
*FileExit_pb.mnemonic: "x"
*FileExit_pb.activateCallback: { performMenuItem(handleMenuStop,NULL,CPsubsysQ); }

*Task_pane.class: rowColumn
*Task_pane.name.source: public
*Task_pane.static: false
*Task_pane.name: Task_pane
*Task_pane.parent: CPsubsysMenu
*Task_pane.rowColumnType: "menu_pulldown"
*Task_pane.positionIndex: 1

*TaskLogBrowser_cb.class: cascadeButton
*TaskLogBrowser_cb.static: true
*TaskLogBrowser_cb.name: TaskLogBrowser_cb
*TaskLogBrowser_cb.parent: Task_pane
*TaskLogBrowser_cb.labelString: "Log Browser"
*TaskLogBrowser_cb.subMenuId: "CPsubsysLogBrowser_pane"
*TaskLogBrowser_cb.mnemonic: "L"

*CPsubsysLogBrowser_pane.class: rowColumn
*CPsubsysLogBrowser_pane.static: true
*CPsubsysLogBrowser_pane.name: CPsubsysLogBrowser_pane
*CPsubsysLogBrowser_pane.parent: Task_pane
*CPsubsysLogBrowser_pane.rowColumnType: "menu_pulldown"

*SubsysLogBrowser_info_pb.class: pushButton
*SubsysLogBrowser_info_pb.static: true
*SubsysLogBrowser_info_pb.name: SubsysLogBrowser_info_pb
*SubsysLogBrowser_info_pb.parent: CPsubsysLogBrowser_pane
*SubsysLogBrowser_info_pb.labelString: "Info"
*SubsysLogBrowser_info_pb.mnemonic: "I"
*SubsysLogBrowser_info_pb.activateCallback: {spawnLogBrowser("info", CPsubsysQ);}

*SubsysLogBrowser_debug_pb.class: pushButton
*SubsysLogBrowser_debug_pb.static: true
*SubsysLogBrowser_debug_pb.name: SubsysLogBrowser_debug_pb
*SubsysLogBrowser_debug_pb.parent: CPsubsysLogBrowser_pane
*SubsysLogBrowser_debug_pb.labelString: "Debug"
*SubsysLogBrowser_debug_pb.mnemonic: "D"
*SubsysLogBrowser_debug_pb.activateCallback: {spawnLogBrowser("debug", CPsubsysQ);}

*SubsysLogBrowser_error_pb.class: pushButton
*SubsysLogBrowser_error_pb.static: true
*SubsysLogBrowser_error_pb.name: SubsysLogBrowser_error_pb
*SubsysLogBrowser_error_pb.parent: CPsubsysLogBrowser_pane
*SubsysLogBrowser_error_pb.labelString: "Error"
*SubsysLogBrowser_error_pb.mnemonic: "E"
*SubsysLogBrowser_error_pb.activateCallback: {spawnLogBrowser("error", CPsubsysQ);}

*Help_pane.class: rowColumn
*Help_pane.name.source: public
*Help_pane.static: false
*Help_pane.name: Help_pane
*Help_pane.parent: CPsubsysMenu
*Help_pane.rowColumnType: "menu_pulldown"

*Main_win_pb.class: pushButtonGadget
*Main_win_pb.static: true
*Main_win_pb.name: Main_win_pb
*Main_win_pb.parent: Help_pane
*Main_win_pb.labelString: "SPS Processing Queue Window"
*Main_win_pb.mnemonic: "W"
*Main_win_pb.activateCallback: showHelp(HELP_SPS_QUEUE_WIN, "");

*SubsysWin_pb.class: pushButtonGadget
*SubsysWin_pb.static: true
*SubsysWin_pb.name: SubsysWin_pb
*SubsysWin_pb.parent: Help_pane
*SubsysWin_pb.labelString: "Subsystem Windows"
*SubsysWin_pb.mnemonic: "S"
*SubsysWin_pb.activateCallback: showHelp(HELP_SUBSYS_WIN, "");

*JobStates_pb.class: pushButtonGadget
*JobStates_pb.static: true
*JobStates_pb.name: JobStates_pb
*JobStates_pb.parent: Help_pane
*JobStates_pb.labelString: "Job States"
*JobStates_pb.mnemonic: "J"
*JobStates_pb.activateCallback: showHelp(HELP_JOB_STATES, CP_version_id);

*EventTrans_pb.class: pushButtonGadget
*EventTrans_pb.static: true
*EventTrans_pb.name: EventTrans_pb
*EventTrans_pb.parent: Help_pane
*EventTrans_pb.labelString: "Event Transitions"
*EventTrans_pb.mnemonic: "E"
*EventTrans_pb.activateCallback: showHelp(HELP_EVENT_TRANSITIONS, "");

*MenuTrans_pb.class: pushButtonGadget
*MenuTrans_pb.static: true
*MenuTrans_pb.name: MenuTrans_pb
*MenuTrans_pb.parent: Help_pane
*MenuTrans_pb.labelString: "Menu Transitions"
*MenuTrans_pb.mnemonic: "M"
*MenuTrans_pb.activateCallback: showHelp(HELP_MENU_TRANSITIONS, "");

*ProdInfo_pb.class: pushButtonGadget
*ProdInfo_pb.static: true
*ProdInfo_pb.name: ProdInfo_pb
*ProdInfo_pb.parent: Help_pane
*ProdInfo_pb.labelString: "Product Information"
*ProdInfo_pb.mnemonic: "I"
*ProdInfo_pb.activateCallback: showHelp(HELP_INFO, CP_version_id);

*Subsystems_pane.class: rowColumn
*Subsystems_pane.static: true
*Subsystems_pane.name: Subsystems_pane
*Subsystems_pane.parent: CPsubsysMenu
*Subsystems_pane.rowColumnType: "menu_pulldown"
*Subsystems_pane.positionIndex: 3

*SubsysReady_pb.class: pushButtonGadget
*SubsysReady_pb.static: true
*SubsysReady_pb.name: SubsysReady_pb
*SubsysReady_pb.parent: Subsystems_pane
*SubsysReady_pb.labelString: "Ready"
*SubsysReady_pb.mnemonic: "R"
*SubsysReady_pb.activateCallback: performMenuItem(handleSubsysMenuReady,NULL,CPsubsysQ);

*SubsysRest_pb.class: pushButtonGadget
*SubsysRest_pb.static: true
*SubsysRest_pb.name: SubsysRest_pb
*SubsysRest_pb.parent: Subsystems_pane
*SubsysRest_pb.labelString: "Reset"
*SubsysRest_pb.mnemonic: "t"
*SubsysRest_pb.activateCallback: performMenuItem(handleMenuReset,NULL,CPsubsysQ);

*CPsubsysMenu_p6_b3.class: pushButtonGadget
*CPsubsysMenu_p6_b3.static: true
*CPsubsysMenu_p6_b3.name: CPsubsysMenu_p6_b3
*CPsubsysMenu_p6_b3.parent: Subsystems_pane
*CPsubsysMenu_p6_b3.labelString: "Pause"
*CPsubsysMenu_p6_b3.mnemonic: "P"
*CPsubsysMenu_p6_b3.activateCallback: performMenuItem(handleMenuPause,NULL,CPsubsysQ);

*CPsubsysMenu_p6_b4.class: pushButtonGadget
*CPsubsysMenu_p6_b4.static: true
*CPsubsysMenu_p6_b4.name: CPsubsysMenu_p6_b4
*CPsubsysMenu_p6_b4.parent: Subsystems_pane
*CPsubsysMenu_p6_b4.labelString: "Send Heartbeat"
*CPsubsysMenu_p6_b4.mnemonic: "H"
*CPsubsysMenu_p6_b4.activateCallback: doSendHeartbeat(CPsubsysQ);

*Queue_pane.class: rowColumn
*Queue_pane.name.source: public
*Queue_pane.static: false
*Queue_pane.name: Queue_pane
*Queue_pane.parent: CPsubsysMenu
*Queue_pane.rowColumnType: "menu_pulldown"
*Queue_pane.positionIndex: 2

*QueueRemoveHold_pb.class: pushButtonGadget
*QueueRemoveHold_pb.static: true
*QueueRemoveHold_pb.name: QueueRemoveHold_pb
*QueueRemoveHold_pb.parent: Queue_pane
*QueueRemoveHold_pb.labelString: "Remove Hold"
*QueueRemoveHold_pb.mnemonic: "H"
*QueueRemoveHold_pb.activateCallback: performMenuItem(handleSubsysMenuRemoveHold,NULL,CPsubsysQ);

*QueueMoveJob_pb.class: pushButtonGadget
*QueueMoveJob_pb.static: true
*QueueMoveJob_pb.name: QueueMoveJob_pb
*QueueMoveJob_pb.parent: Queue_pane
*QueueMoveJob_pb.labelString: "Move Job"
*QueueMoveJob_pb.mnemonic: "M"
*QueueMoveJob_pb.activateCallback: performMenuItem(handleSubsysMenuMoveJob,NULL,CPsubsysQ);

*subsysQC_item.class: pushButtonGadget
*subsysQC_item.static: true
*subsysQC_item.name: subsysQC_item
*subsysQC_item.parent: Queue_pane
*subsysQC_item.labelString: "Perform QC"
*subsysQC_item.mnemonic: "Q"
*subsysQC_item.activateCallback: performMenuItem(handleSubsysMenuQCspawn,NULL,CPsubsysQ);\


*QueueShowInfo_pb.class: pushButtonGadget
*QueueShowInfo_pb.static: true
*QueueShowInfo_pb.name: QueueShowInfo_pb
*QueueShowInfo_pb.parent: Queue_pane
*QueueShowInfo_pb.labelString: "Show Detailed Info"
*QueueShowInfo_pb.mnemonic: "I"
*QueueShowInfo_pb.activateCallback: performMenuItem(handleSubsysMenuDetailedInfo,NULL,CPsubsysQ);

*QueueCancelJob_pb.class: pushButtonGadget
*QueueCancelJob_pb.static: true
*QueueCancelJob_pb.name: QueueCancelJob_pb
*QueueCancelJob_pb.parent: Queue_pane
*QueueCancelJob_pb.labelString: "Cancel Job"
*QueueCancelJob_pb.mnemonic: "C"
*QueueCancelJob_pb.activateCallback: performMenuItem(handleSubsysMenuDelete,NULL,CPsubsysQ);

*File_cb.class: cascadeButton
*File_cb.static: true
*File_cb.name: File_cb
*File_cb.parent: CPsubsysMenu
*File_cb.labelString: "File"
*File_cb.subMenuId: "File_pane"
*File_cb.mnemonic: "F"

*Task_cb.class: cascadeButton
*Task_cb.static: true
*Task_cb.name: Task_cb
*Task_cb.parent: CPsubsysMenu
*Task_cb.labelString: "Tools"
*Task_cb.mnemonic: "T"
*Task_cb.subMenuId: "Task_pane"

*Help_cb.class: cascadeButton
*Help_cb.static: true
*Help_cb.name: Help_cb
*Help_cb.parent: CPsubsysMenu
*Help_cb.labelString: "Help"
*Help_cb.mnemonic: "H"
*Help_cb.subMenuId: "Help_pane"

*CPsubsysMenu_top_b1.class: cascadeButton
*CPsubsysMenu_top_b1.static: true
*CPsubsysMenu_top_b1.name: CPsubsysMenu_top_b1
*CPsubsysMenu_top_b1.parent: CPsubsysMenu
*CPsubsysMenu_top_b1.labelString: "Subsystem"
*CPsubsysMenu_top_b1.mnemonic: "S"
*CPsubsysMenu_top_b1.subMenuId: "Subsystems_pane"

*Queue_cb.class: cascadeButton
*Queue_cb.static: true
*Queue_cb.name: Queue_cb
*Queue_cb.parent: CPsubsysMenu
*Queue_cb.labelString: "Jobs"
*Queue_cb.mnemonic: "J"
*Queue_cb.subMenuId: "Queue_pane"

*CPsubsysQform.class: form
*CPsubsysQform.name.source: public
*CPsubsysQform.static: false
*CPsubsysQform.name: CPsubsysQform
*CPsubsysQform.parent: mainWindow1

*form1.class: form
*form1.static: true
*form1.name: form1
*form1.parent: CPsubsysQform
*form1.resizePolicy: "resize_none"
*form1.x: 280
*form1.y: 10
*form1.width: 210
*form1.height: 30
*form1.leftAttachment: "attach_form"
*form1.leftOffset: 5
*form1.rightAttachment: "attach_form"
*form1.rightOffset: 5

*QueueCurrentJobInfoLabel.class: labelGadget
*QueueCurrentJobInfoLabel.name.source: public
*QueueCurrentJobInfoLabel.static: false
*QueueCurrentJobInfoLabel.name: QueueCurrentJobInfoLabel
*QueueCurrentJobInfoLabel.parent: form1
*QueueCurrentJobInfoLabel.x: 20
*QueueCurrentJobInfoLabel.y: 3
*QueueCurrentJobInfoLabel.height: 20
*QueueCurrentJobInfoLabel.labelString: "Status: Started"
*QueueCurrentJobInfoLabel.bottomAttachment: "attach_form"
*QueueCurrentJobInfoLabel.leftAttachment: "attach_form"
*QueueCurrentJobInfoLabel.rightAttachment: "attach_position"
*QueueCurrentJobInfoLabel.rightPosition: 50
*QueueCurrentJobInfoLabel.topAttachment: "attach_form"

*QueBottomInfoLabel_0.class: labelGadget
*QueBottomInfoLabel_0.name.source: public
*QueBottomInfoLabel_0.static: false
*QueBottomInfoLabel_0.name: QueBottomInfoLabel_0
*QueBottomInfoLabel_0.parent: form1
*QueBottomInfoLabel_0.x: 450
*QueBottomInfoLabel_0.y: 3
*QueBottomInfoLabel_0.height: 25
*QueBottomInfoLabel_0.bottomAttachment: "attach_form"
*QueBottomInfoLabel_0.leftAttachment: "attach_position"
*QueBottomInfoLabel_0.leftOffset: 0
*QueBottomInfoLabel_0.leftPosition: 50
*QueBottomInfoLabel_0.rightAttachment: "attach_form"
*QueBottomInfoLabel_0.topAttachment: "attach_form"

*separator1.class: separator
*separator1.static: true
*separator1.name: separator1
*separator1.parent: CPsubsysQform
*separator1.x: 0
*separator1.y: 30
*separator1.height: 10
*separator1.leftAttachment: "attach_form"
*separator1.leftOffset: 5
*separator1.rightAttachment: "attach_form"
*separator1.rightOffset: 5
*separator1.topAttachment: "attach_widget"
*separator1.topWidget: "form1"
*separator1.separatorType: "shadow_etched_out"

*form2.class: form
*form2.static: true
*form2.name: form2
*form2.parent: CPsubsysQform
*form2.x: 0
*form2.allowOverlap: "false"
*form2.rubberPositioning: "false"
*form2.resizePolicy: "resize_any"
*form2.leftAttachment: "attach_form"
*form2.leftOffset: 5
*form2.rightAttachment: "attach_form"
*form2.rightOffset: 5
*form2.topAttachment: "attach_widget"
*form2.topWidget: "separator1"
*form2.bottomAttachment: "attach_form"
*form2.bottomOffset: 5

*CPqueJobIdScrolledWin.class: scrolledWindow
*CPqueJobIdScrolledWin.name.source: public
*CPqueJobIdScrolledWin.static: false
*CPqueJobIdScrolledWin.name: CPqueJobIdScrolledWin
*CPqueJobIdScrolledWin.parent: form2
*CPqueJobIdScrolledWin.scrollingPolicy: "application_defined"
*CPqueJobIdScrolledWin.x: 14
*CPqueJobIdScrolledWin.y: 32
*CPqueJobIdScrolledWin.width: 85
*CPqueJobIdScrolledWin.height: 90
*CPqueJobIdScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueJobIdScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueJobIdScrolledWin.visualPolicy: "variable"
*CPqueJobIdScrolledWin.spacing: 0
*CPqueJobIdScrolledWin.bottomAttachment: "attach_form"
*CPqueJobIdScrolledWin.bottomOffset: 10
*CPqueJobIdScrolledWin.topAttachment: "attach_form"
*CPqueJobIdScrolledWin.topOffset: 30
*CPqueJobIdScrolledWin.leftOffset: 10
*CPqueJobIdScrolledWin.shadowThickness: 2

*CPqueJobIdList.class: scrolledList
*CPqueJobIdList.name.source: public
*CPqueJobIdList.static: false
*CPqueJobIdList.name: CPqueJobIdList
*CPqueJobIdList.parent: CPqueJobIdScrolledWin
*CPqueJobIdList.fontList: "helvb10"
*CPqueJobIdList.stringDirection: "string_direction_l_to_r"
*CPqueJobIdList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueJobIdList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueJobIdList.listSizePolicy: "constant"
*CPqueJobIdList.shadowThickness: 0
*CPqueJobIdList.highlightThickness: 0
*CPqueJobIdList.listMarginWidth: 2
*CPqueJobIdList.scrollBarDisplayPolicy: "static"

*CPquePlatRevSeqScrolledWin.class: scrolledWindow
*CPquePlatRevSeqScrolledWin.name.source: public
*CPquePlatRevSeqScrolledWin.static: false
*CPquePlatRevSeqScrolledWin.name: CPquePlatRevSeqScrolledWin
*CPquePlatRevSeqScrolledWin.parent: form2
*CPquePlatRevSeqScrolledWin.scrollingPolicy: "application_defined"
*CPquePlatRevSeqScrolledWin.x: 106
*CPquePlatRevSeqScrolledWin.y: 32
*CPquePlatRevSeqScrolledWin.width: 125
*CPquePlatRevSeqScrolledWin.height: 90
*CPquePlatRevSeqScrolledWin.scrollBarDisplayPolicy: "static"
*CPquePlatRevSeqScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPquePlatRevSeqScrolledWin.leftAttachment: "attach_widget"
*CPquePlatRevSeqScrolledWin.leftOffset: 5
*CPquePlatRevSeqScrolledWin.leftWidget: "CPqueJobIdScrolledWin"
*CPquePlatRevSeqScrolledWin.visualPolicy: "variable"
*CPquePlatRevSeqScrolledWin.spacing: 0
*CPquePlatRevSeqScrolledWin.bottomAttachment: "attach_form"
*CPquePlatRevSeqScrolledWin.bottomOffset: 10
*CPquePlatRevSeqScrolledWin.topAttachment: "attach_form"
*CPquePlatRevSeqScrolledWin.topOffset: 30
*CPquePlatRevSeqScrolledWin.shadowThickness: 2

*CPquePlatRevSeqList.class: scrolledList
*CPquePlatRevSeqList.name.source: public
*CPquePlatRevSeqList.static: false
*CPquePlatRevSeqList.name: CPquePlatRevSeqList
*CPquePlatRevSeqList.parent: CPquePlatRevSeqScrolledWin
*CPquePlatRevSeqList.fontList: "helvb10"
*CPquePlatRevSeqList.stringDirection: "string_direction_l_to_r"
*CPquePlatRevSeqList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPquePlatRevSeqList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPquePlatRevSeqList.listSizePolicy: "constant"
*CPquePlatRevSeqList.shadowThickness: 0
*CPquePlatRevSeqList.highlightThickness: 0
*CPquePlatRevSeqList.listMarginWidth: 2
*CPquePlatRevSeqList.scrollBarDisplayPolicy: "static"

*CPqueFrameScrolledWin.class: scrolledWindow
*CPqueFrameScrolledWin.name.source: public
*CPqueFrameScrolledWin.static: false
*CPqueFrameScrolledWin.name: CPqueFrameScrolledWin
*CPqueFrameScrolledWin.parent: form2
*CPqueFrameScrolledWin.scrollingPolicy: "application_defined"
*CPqueFrameScrolledWin.x: 236
*CPqueFrameScrolledWin.y: 32
*CPqueFrameScrolledWin.width: 75
*CPqueFrameScrolledWin.height: 90
*CPqueFrameScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueFrameScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueFrameScrolledWin.leftAttachment: "attach_widget"
*CPqueFrameScrolledWin.leftOffset: 5
*CPqueFrameScrolledWin.leftWidget: "CPquePlatRevSeqScrolledWin"
*CPqueFrameScrolledWin.visualPolicy: "variable"
*CPqueFrameScrolledWin.spacing: 0
*CPqueFrameScrolledWin.bottomAttachment: "attach_form"
*CPqueFrameScrolledWin.bottomOffset: 10
*CPqueFrameScrolledWin.topAttachment: "attach_form"
*CPqueFrameScrolledWin.topOffset: 30
*CPqueFrameScrolledWin.shadowThickness: 2

*CPqueFrameList.class: scrolledList
*CPqueFrameList.name.source: public
*CPqueFrameList.static: false
*CPqueFrameList.name: CPqueFrameList
*CPqueFrameList.parent: CPqueFrameScrolledWin
*CPqueFrameList.fontList: "helvb10"
*CPqueFrameList.stringDirection: "string_direction_l_to_r"
*CPqueFrameList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueFrameList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueFrameList.listSizePolicy: "constant"
*CPqueFrameList.shadowThickness: 0
*CPqueFrameList.highlightThickness: 0
*CPqueFrameList.listMarginWidth: 2
*CPqueFrameList.scrollBarDisplayPolicy: "static"

*CPqueMediaScrolledWin.class: scrolledWindow
*CPqueMediaScrolledWin.name.source: public
*CPqueMediaScrolledWin.static: false
*CPqueMediaScrolledWin.name: CPqueMediaScrolledWin
*CPqueMediaScrolledWin.parent: form2
*CPqueMediaScrolledWin.scrollingPolicy: "application_defined"
*CPqueMediaScrolledWin.x: 316
*CPqueMediaScrolledWin.y: 32
*CPqueMediaScrolledWin.width: 200
*CPqueMediaScrolledWin.height: 90
*CPqueMediaScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueMediaScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueMediaScrolledWin.leftAttachment: "attach_widget"
*CPqueMediaScrolledWin.leftOffset: 5
*CPqueMediaScrolledWin.leftWidget: "CPqueFrameScrolledWin"
*CPqueMediaScrolledWin.visualPolicy: "variable"
*CPqueMediaScrolledWin.spacing: 0
*CPqueMediaScrolledWin.bottomAttachment: "attach_form"
*CPqueMediaScrolledWin.bottomOffset: 10
*CPqueMediaScrolledWin.topAttachment: "attach_form"
*CPqueMediaScrolledWin.topOffset: 30
*CPqueMediaScrolledWin.shadowThickness: 2

*CPqueMediaList.class: scrolledList
*CPqueMediaList.name.source: public
*CPqueMediaList.static: false
*CPqueMediaList.name: CPqueMediaList
*CPqueMediaList.parent: CPqueMediaScrolledWin
*CPqueMediaList.fontList: "helvb10"
*CPqueMediaList.stringDirection: "string_direction_l_to_r"
*CPqueMediaList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueMediaList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueMediaList.listSizePolicy: "constant"
*CPqueMediaList.shadowThickness: 0
*CPqueMediaList.highlightThickness: 0
*CPqueMediaList.listMarginWidth: 2
*CPqueMediaList.scrollBarDisplayPolicy: "static"

*CPqueModeScrolledWin.class: scrolledWindow
*CPqueModeScrolledWin.name.source: public
*CPqueModeScrolledWin.static: false
*CPqueModeScrolledWin.name: CPqueModeScrolledWin
*CPqueModeScrolledWin.parent: form2
*CPqueModeScrolledWin.scrollingPolicy: "application_defined"
*CPqueModeScrolledWin.x: 406
*CPqueModeScrolledWin.y: 32
*CPqueModeScrolledWin.width: 65
*CPqueModeScrolledWin.height: 90
*CPqueModeScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueModeScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueModeScrolledWin.leftAttachment: "attach_widget"
*CPqueModeScrolledWin.leftOffset: 5
*CPqueModeScrolledWin.leftWidget: "CPqueMediaScrolledWin"
*CPqueModeScrolledWin.visualPolicy: "variable"
*CPqueModeScrolledWin.spacing: 0
*CPqueModeScrolledWin.bottomAttachment: "attach_form"
*CPqueModeScrolledWin.bottomOffset: 10
*CPqueModeScrolledWin.topAttachment: "attach_form"
*CPqueModeScrolledWin.topOffset: 30
*CPqueModeScrolledWin.shadowThickness: 2

*CPqueModeList.class: scrolledList
*CPqueModeList.name.source: public
*CPqueModeList.static: false
*CPqueModeList.name: CPqueModeList
*CPqueModeList.parent: CPqueModeScrolledWin
*CPqueModeList.fontList: "helvb10"
*CPqueModeList.stringDirection: "string_direction_l_to_r"
*CPqueModeList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueModeList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueModeList.listSizePolicy: "constant"
*CPqueModeList.shadowThickness: 0
*CPqueModeList.highlightThickness: 0
*CPqueModeList.listMarginWidth: 2
*CPqueModeList.scrollBarDisplayPolicy: "static"

*CPqueRequestScrolledWin.class: scrolledWindow
*CPqueRequestScrolledWin.name.source: public
*CPqueRequestScrolledWin.static: false
*CPqueRequestScrolledWin.name: CPqueRequestScrolledWin
*CPqueRequestScrolledWin.parent: form2
*CPqueRequestScrolledWin.scrollingPolicy: "application_defined"
*CPqueRequestScrolledWin.x: 456
*CPqueRequestScrolledWin.y: 32
*CPqueRequestScrolledWin.width: 90
*CPqueRequestScrolledWin.height: 90
*CPqueRequestScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueRequestScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueRequestScrolledWin.leftAttachment: "attach_widget"
*CPqueRequestScrolledWin.leftOffset: 5
*CPqueRequestScrolledWin.leftWidget: "CPqueModeScrolledWin"
*CPqueRequestScrolledWin.visualPolicy: "variable"
*CPqueRequestScrolledWin.spacing: 0
*CPqueRequestScrolledWin.bottomAttachment: "attach_form"
*CPqueRequestScrolledWin.bottomOffset: 10
*CPqueRequestScrolledWin.topAttachment: "attach_form"
*CPqueRequestScrolledWin.topOffset: 30
*CPqueRequestScrolledWin.shadowThickness: 2

*CPqueRequestList.class: scrolledList
*CPqueRequestList.name.source: public
*CPqueRequestList.static: false
*CPqueRequestList.name: CPqueRequestList
*CPqueRequestList.parent: CPqueRequestScrolledWin
*CPqueRequestList.fontList: "helvb10"
*CPqueRequestList.stringDirection: "string_direction_l_to_r"
*CPqueRequestList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueRequestList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueRequestList.listSizePolicy: "constant"
*CPqueRequestList.shadowThickness: 0
*CPqueRequestList.highlightThickness: 0
*CPqueRequestList.listMarginWidth: 2
*CPqueRequestList.scrollBarDisplayPolicy: "static"

*CPqueTypeScrolledWin.class: scrolledWindow
*CPqueTypeScrolledWin.name.source: public
*CPqueTypeScrolledWin.static: false
*CPqueTypeScrolledWin.name: CPqueTypeScrolledWin
*CPqueTypeScrolledWin.parent: form2
*CPqueTypeScrolledWin.scrollingPolicy: "application_defined"
*CPqueTypeScrolledWin.x: 536
*CPqueTypeScrolledWin.y: 32
*CPqueTypeScrolledWin.width: 145
*CPqueTypeScrolledWin.height: 90
*CPqueTypeScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueTypeScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueTypeScrolledWin.leftAttachment: "attach_widget"
*CPqueTypeScrolledWin.leftOffset: 5
*CPqueTypeScrolledWin.leftWidget: "CPqueRequestScrolledWin"
*CPqueTypeScrolledWin.visualPolicy: "variable"
*CPqueTypeScrolledWin.spacing: 0
*CPqueTypeScrolledWin.bottomAttachment: "attach_form"
*CPqueTypeScrolledWin.bottomOffset: 10
*CPqueTypeScrolledWin.topAttachment: "attach_form"
*CPqueTypeScrolledWin.topOffset: 30
*CPqueTypeScrolledWin.shadowThickness: 2

*CPqueTypeList.class: scrolledList
*CPqueTypeList.name.source: public
*CPqueTypeList.static: false
*CPqueTypeList.name: CPqueTypeList
*CPqueTypeList.parent: CPqueTypeScrolledWin
*CPqueTypeList.fontList: "helvb10"
*CPqueTypeList.stringDirection: "string_direction_l_to_r"
*CPqueTypeList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueTypeList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueTypeList.listSizePolicy: "constant"
*CPqueTypeList.shadowThickness: 0
*CPqueTypeList.highlightThickness: 0
*CPqueTypeList.listMarginWidth: 2
*CPqueTypeList.scrollBarDisplayPolicy: "static"

*CPqueStatusScrolledWin.class: scrolledWindow
*CPqueStatusScrolledWin.name.source: public
*CPqueStatusScrolledWin.static: false
*CPqueStatusScrolledWin.name: CPqueStatusScrolledWin
*CPqueStatusScrolledWin.parent: form2
*CPqueStatusScrolledWin.scrollingPolicy: "application_defined"
*CPqueStatusScrolledWin.x: 651
*CPqueStatusScrolledWin.y: 32
*CPqueStatusScrolledWin.width: 215
*CPqueStatusScrolledWin.height: 90
*CPqueStatusScrolledWin.scrollBarDisplayPolicy: "static"
*CPqueStatusScrolledWin.createCallback: {\
/* setupScrollBars(CPsubsysQ); */\
}
*CPqueStatusScrolledWin.leftAttachment: "attach_widget"
*CPqueStatusScrolledWin.leftOffset: 5
*CPqueStatusScrolledWin.leftWidget: "CPqueTypeScrolledWin"
*CPqueStatusScrolledWin.visualPolicy: "variable"
*CPqueStatusScrolledWin.spacing: 0
*CPqueStatusScrolledWin.rightAttachment: "attach_form"
*CPqueStatusScrolledWin.rightOffset: 10
*CPqueStatusScrolledWin.bottomAttachment: "attach_form"
*CPqueStatusScrolledWin.bottomOffset: 10
*CPqueStatusScrolledWin.topAttachment: "attach_form"
*CPqueStatusScrolledWin.topOffset: 30
*CPqueStatusScrolledWin.shadowThickness: 2

*CPqueStatusList.class: scrolledList
*CPqueStatusList.name.source: public
*CPqueStatusList.static: false
*CPqueStatusList.name: CPqueStatusList
*CPqueStatusList.parent: CPqueStatusScrolledWin
*CPqueStatusList.fontList: "helvb10"
*CPqueStatusList.defaultActionCallback: {\
showSubsysDetailedInfo(CPsubsysQ,  UxCallbackArg); \
}
*CPqueStatusList.browseSelectionCallback: {\
/* XmListCallbackStruct *cbs = (XmListCallbackStruct *) UxCallbackArg;\
selectLine(CPsubsysQ, cbs->item_position); */\
syncLists(UxWidget, CPsubsysQ, UxCallbackArg);\
\
}
*CPqueStatusList.listSizePolicy: "constant"
*CPqueStatusList.shadowThickness: 0
*CPqueStatusList.highlightThickness: 0
*CPqueStatusList.listMarginWidth: 2
*CPqueStatusList.scrollBarDisplayPolicy: "static"

*CPqueJobIdLabel.class: labelGadget
*CPqueJobIdLabel.static: true
*CPqueJobIdLabel.name: CPqueJobIdLabel
*CPqueJobIdLabel.parent: form2
*CPqueJobIdLabel.x: 10
*CPqueJobIdLabel.y: 10
*CPqueJobIdLabel.width: 55
*CPqueJobIdLabel.height: 20
*CPqueJobIdLabel.labelString: "Job ID"
*CPqueJobIdLabel.alignment: "alignment_beginning"
*CPqueJobIdLabel.fontList: "helvbo10"
*CPqueJobIdLabel.rightWidget: "CPqueJobIdScrolledWin"
*CPqueJobIdLabel.rightAttachment: "attach_opposite_widget"
*CPqueJobIdLabel.leftAttachment: "attach_opposite_widget"
*CPqueJobIdLabel.leftWidget: "CPqueJobIdScrolledWin"

*CPquePlatRevSeqLabel.class: labelGadget
*CPquePlatRevSeqLabel.static: true
*CPquePlatRevSeqLabel.name: CPquePlatRevSeqLabel
*CPquePlatRevSeqLabel.parent: form2
*CPquePlatRevSeqLabel.x: 100
*CPquePlatRevSeqLabel.y: 10
*CPquePlatRevSeqLabel.width: 92
*CPquePlatRevSeqLabel.height: 20
*CPquePlatRevSeqLabel.labelString: "Plat/Rev/Seq"
*CPquePlatRevSeqLabel.alignment: "alignment_beginning"
*CPquePlatRevSeqLabel.fontList: "helvbo10"
*CPquePlatRevSeqLabel.leftAttachment: "attach_opposite_widget"
*CPquePlatRevSeqLabel.leftWidget: "CPquePlatRevSeqScrolledWin"
*CPquePlatRevSeqLabel.rightAttachment: "attach_opposite_widget"
*CPquePlatRevSeqLabel.rightWidget: "CPquePlatRevSeqScrolledWin"

*CPqueFrameLabel.class: labelGadget
*CPqueFrameLabel.static: true
*CPqueFrameLabel.name: CPqueFrameLabel
*CPqueFrameLabel.parent: form2
*CPqueFrameLabel.x: 230
*CPqueFrameLabel.y: 10
*CPqueFrameLabel.width: 53
*CPqueFrameLabel.height: 20
*CPqueFrameLabel.labelString: "Frame"
*CPqueFrameLabel.alignment: "alignment_beginning"
*CPqueFrameLabel.fontList: "helvbo10"
*CPqueFrameLabel.leftAttachment: "attach_opposite_widget"
*CPqueFrameLabel.leftWidget: "CPqueFrameScrolledWin"
*CPqueFrameLabel.rightAttachment: "attach_opposite_widget"
*CPqueFrameLabel.rightWidget: "CPqueFrameScrolledWin"

*CPqueMediaLabel.class: labelGadget
*CPqueMediaLabel.static: true
*CPqueMediaLabel.name: CPqueMediaLabel
*CPqueMediaLabel.parent: form2
*CPqueMediaLabel.x: 310
*CPqueMediaLabel.y: 10
*CPqueMediaLabel.width: 51
*CPqueMediaLabel.height: 20
*CPqueMediaLabel.labelString: "Media"
*CPqueMediaLabel.alignment: "alignment_beginning"
*CPqueMediaLabel.fontList: "helvbo10"
*CPqueMediaLabel.leftAttachment: "attach_opposite_widget"
*CPqueMediaLabel.leftWidget: "CPqueMediaScrolledWin"
*CPqueMediaLabel.rightAttachment: "attach_opposite_widget"
*CPqueMediaLabel.rightWidget: "CPqueMediaScrolledWin"

*CPqueModeLabel.class: labelGadget
*CPqueModeLabel.static: true
*CPqueModeLabel.name: CPqueModeLabel
*CPqueModeLabel.parent: form2
*CPqueModeLabel.x: 390
*CPqueModeLabel.y: 10
*CPqueModeLabel.width: 40
*CPqueModeLabel.height: 20
*CPqueModeLabel.labelString: "Mode"
*CPqueModeLabel.alignment: "alignment_beginning"
*CPqueModeLabel.fontList: "helvbo10"
*CPqueModeLabel.leftAttachment: "attach_opposite_widget"
*CPqueModeLabel.leftWidget: "CPqueModeScrolledWin"
*CPqueModeLabel.rightAttachment: "attach_opposite_widget"
*CPqueModeLabel.rightWidget: "CPqueModeScrolledWin"

*CPqueRequestLabel.class: labelGadget
*CPqueRequestLabel.static: true
*CPqueRequestLabel.name: CPqueRequestLabel
*CPqueRequestLabel.parent: form2
*CPqueRequestLabel.x: 450
*CPqueRequestLabel.y: 10
*CPqueRequestLabel.width: 77
*CPqueRequestLabel.height: 20
*CPqueRequestLabel.labelString: "Request"
*CPqueRequestLabel.alignment: "alignment_beginning"
*CPqueRequestLabel.fontList: "helvbo10"
*CPqueRequestLabel.leftAttachment: "attach_opposite_widget"
*CPqueRequestLabel.leftWidget: "CPqueRequestScrolledWin"
*CPqueRequestLabel.rightAttachment: "attach_opposite_widget"
*CPqueRequestLabel.rightWidget: "CPqueRequestScrolledWin"

*CPqueTypeLabel.class: labelGadget
*CPqueTypeLabel.static: true
*CPqueTypeLabel.name: CPqueTypeLabel
*CPqueTypeLabel.parent: form2
*CPqueTypeLabel.x: 530
*CPqueTypeLabel.y: 10
*CPqueTypeLabel.width: 100
*CPqueTypeLabel.height: 20
*CPqueTypeLabel.labelString: "Product Type"
*CPqueTypeLabel.alignment: "alignment_beginning"
*CPqueTypeLabel.fontList: "helvbo10"
*CPqueTypeLabel.leftAttachment: "attach_opposite_widget"
*CPqueTypeLabel.leftWidget: "CPqueTypeScrolledWin"
*CPqueTypeLabel.rightAttachment: "attach_opposite_widget"
*CPqueTypeLabel.rightWidget: "CPqueTypeScrolledWin"

*CPqueStatusLabel.class: labelGadget
*CPqueStatusLabel.static: true
*CPqueStatusLabel.name: CPqueStatusLabel
*CPqueStatusLabel.parent: form2
*CPqueStatusLabel.x: 650
*CPqueStatusLabel.y: 8
*CPqueStatusLabel.width: 43
*CPqueStatusLabel.height: 20
*CPqueStatusLabel.labelString: "Status"
*CPqueStatusLabel.alignment: "alignment_beginning"
*CPqueStatusLabel.fontList: "helvbo10"
*CPqueStatusLabel.leftAttachment: "attach_opposite_widget"
*CPqueStatusLabel.leftWidget: "CPqueStatusScrolledWin"
*CPqueStatusLabel.rightAttachment: "attach_opposite_widget"
*CPqueStatusLabel.rightWidget: "CPqueStatusScrolledWin"

