! UIMX ascii 2.9 key: 1311                                                      

*CPmainQ.class: mainWindow
*CPmainQ.classinc:
*CPmainQ.classspec:
*CPmainQ.classmembers:
*CPmainQ.classconstructor:
*CPmainQ.classdestructor:
*CPmainQ.gbldecl: #include <stdio.h>\
static char sccsid_CPmainQ_i[] = "@(#)CPmainQ.i	2.61 97/02/06 16:09:58"; \
#include "version.h"\
#include "help.h"\
#include "cpdefines.h"\
#include "pps.h"\
\
typedef void (*VoidProc)();\
\
\
extern void handleMainMenuDelete();\
extern void handleMainMenuPerformQC();\
extern void handleMainMenuRemoveHold();\
extern void handleMainMenuMoveJob();\
extern void handleMainMenuDetailedInfo();\
extern void handleMenuPrintCB(); \
extern void handleMenuStartAllCB(); \
extern void handleMenuStartAllExtCB();\
extern void handleMenuReadyAllCB(); \
extern void handleMenuReadyAllExtCB();\
extern void syncLists();\
extern char *getSelectedExternal();\
extern void check_number();\

*CPmainQ.ispecdecl:
*CPmainQ.funcdecl: swidget create_CPmainQ(UxParent)\
swidget UxParent;
*CPmainQ.funcname: create_CPmainQ
*CPmainQ.funcdef: "swidget", "<create_CPmainQ>(%)"
*CPmainQ.argdecl: swidget UxParent;
*CPmainQ.arglist: UxParent
*CPmainQ.arglist.UxParent: "swidget", "%UxParent%"
*CPmainQ.icode:
*CPmainQ.fcode: return(rtrn);\

*CPmainQ.auxdecl:
*CPmainQ.name.source: public
*CPmainQ.static: false
*CPmainQ.name: CPmainQ
*CPmainQ.parent: NO_PARENT
*CPmainQ.parentExpression: UxParent
*CPmainQ.defaultShell: topLevelShell
*CPmainQ.x.source: public
*CPmainQ.x: 20
*CPmainQ.y.source: public
*CPmainQ.y: 150
*CPmainQ.unitType: "pixels"
*CPmainQ.destroyCallback: {\
\
}
*CPmainQ.width.source: public
*CPmainQ.width: 1000
*CPmainQ.height.source: public
*CPmainQ.height: 725

*CPmainMenu.class: rowColumn
*CPmainMenu.static: true
*CPmainMenu.name: CPmainMenu
*CPmainMenu.parent: CPmainQ
*CPmainMenu.rowColumnType: "menu_bar"
*CPmainMenu.menuAccelerator: "<KeyUp>F10"
*CPmainMenu.menuHistory: ""
*CPmainMenu.menuHelpWidget: "HelpPane_cb"
*CPmainMenu.background.source: public
*CPmainMenu.background: "#dddddddddddd"

*File_pane.class: rowColumn
*File_pane.static: true
*File_pane.name: File_pane
*File_pane.parent: CPmainMenu
*File_pane.rowColumnType: "menu_pulldown"
*File_pane.background.source: public
*File_pane.background: "#dddddddddddd"

*FileOpen_pb.class: pushButtonGadget
*FileOpen_pb.static: true
*FileOpen_pb.name: FileOpen_pb
*FileOpen_pb.parent: File_pane
*FileOpen_pb.labelString: "Open ..."
*FileOpen_pb.mnemonic: "O"
*FileOpen_pb.activateCallback: handleMenuFileAsCB("Restore");

*FileSave_pb.class: pushButtonGadget
*FileSave_pb.static: true
*FileSave_pb.name: FileSave_pb
*FileSave_pb.parent: File_pane
*FileSave_pb.labelString: "Save As ..."
*FileSave_pb.mnemonic: "A"
*FileSave_pb.activateCallback: handleMenuFileAsCB("Save As");

*FilePrint_pb.class: pushButtonGadget
*FilePrint_pb.static: true
*FilePrint_pb.name: FilePrint_pb
*FilePrint_pb.parent: File_pane
*FilePrint_pb.labelString: "Print"
*FilePrint_pb.mnemonic: "P"
*FilePrint_pb.activateCallback: performMenuItem(handleMenuPrintCB,NULL,CPmainQ);

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
*FileExit_pb.activateCallback: {\
\
 handleMenuExitCB(UxWidget, \
                  UxClientData, \
                  UxCallbackArg);\
}

*Tools_pane.class: rowColumn
*Tools_pane.static: true
*Tools_pane.name: Tools_pane
*Tools_pane.parent: CPmainMenu
*Tools_pane.rowColumnType: "menu_pulldown"
*Tools_pane.background.source: public
*Tools_pane.background: "#dddddddddddd"

*Start_GPR_button.class: pushButtonGadget
*Start_GPR_button.static: true
*Start_GPR_button.name: Start_GPR_button
*Start_GPR_button.parent: Tools_pane
*Start_GPR_button.labelString: "Generate Production Request (GPR)"
*Start_GPR_button.mnemonic: "R"
*Start_GPR_button.activateCallback: {spawnRequestUtil(); }
*Start_GPR_button.sensitive.source: public
*Start_GPR_button.sensitive: "true"

*CP_logBrowser_cb.class: cascadeButtonGadget
*CP_logBrowser_cb.static: true
*CP_logBrowser_cb.name: CP_logBrowser_cb
*CP_logBrowser_cb.parent: Tools_pane
*CP_logBrowser_cb.labelString: "CP Log Browser"
*CP_logBrowser_cb.subMenuId: "CP_logBrowser_pane"
*CP_logBrowser_cb.mnemonic: "C"

*SPS_logBrowser_cb.class: cascadeButtonGadget
*SPS_logBrowser_cb.static: true
*SPS_logBrowser_cb.name: SPS_logBrowser_cb
*SPS_logBrowser_cb.parent: Tools_pane
*SPS_logBrowser_cb.labelString: "SPS Log Browser"
*SPS_logBrowser_cb.mnemonic: "S"
*SPS_logBrowser_cb.subMenuId: "SPS_logBrowser_pane"

*CP_logBrowser_pane.class: rowColumn
*CP_logBrowser_pane.name.source: public
*CP_logBrowser_pane.static: false
*CP_logBrowser_pane.name: CP_logBrowser_pane
*CP_logBrowser_pane.parent: Tools_pane
*CP_logBrowser_pane.rowColumnType: "menu_pulldown"
*CP_logBrowser_pane.background.source: public
*CP_logBrowser_pane.background: "#dddddddddddd"

*CPlogBrowser_info_pb.class: pushButtonGadget
*CPlogBrowser_info_pb.static: true
*CPlogBrowser_info_pb.name: CPlogBrowser_info_pb
*CPlogBrowser_info_pb.parent: CP_logBrowser_pane
*CPlogBrowser_info_pb.labelString: "Info"
*CPlogBrowser_info_pb.mnemonic: "I"
*CPlogBrowser_info_pb.activateCallback: spawnLogBrowser("info", UxWidget);

*CPlogBrowser_debug_pb.class: pushButtonGadget
*CPlogBrowser_debug_pb.static: true
*CPlogBrowser_debug_pb.name: CPlogBrowser_debug_pb
*CPlogBrowser_debug_pb.parent: CP_logBrowser_pane
*CPlogBrowser_debug_pb.labelString: "Debug"
*CPlogBrowser_debug_pb.mnemonic: "D"
*CPlogBrowser_debug_pb.activateCallback: spawnLogBrowser("debug", UxWidget);

*CPlogBrowser_error_pb.class: pushButtonGadget
*CPlogBrowser_error_pb.static: true
*CPlogBrowser_error_pb.name: CPlogBrowser_error_pb
*CPlogBrowser_error_pb.parent: CP_logBrowser_pane
*CPlogBrowser_error_pb.labelString: "Error"
*CPlogBrowser_error_pb.mnemonic: "E"
*CPlogBrowser_error_pb.activateCallback: spawnLogBrowser("error", UxWidget);

*SPS_logBrowser_pane.class: rowColumn
*SPS_logBrowser_pane.name.source: public
*SPS_logBrowser_pane.static: false
*SPS_logBrowser_pane.name: SPS_logBrowser_pane
*SPS_logBrowser_pane.parent: Tools_pane
*SPS_logBrowser_pane.rowColumnType: "menu_pulldown"
*SPS_logBrowser_pane.background.source: public
*SPS_logBrowser_pane.background: "#dddddddddddd"

*SPSlogBrowser_info_pb.class: pushButtonGadget
*SPSlogBrowser_info_pb.static: true
*SPSlogBrowser_info_pb.name: SPSlogBrowser_info_pb
*SPSlogBrowser_info_pb.parent: SPS_logBrowser_pane
*SPSlogBrowser_info_pb.labelString: "Info"
*SPSlogBrowser_info_pb.mnemonic: "I"
*SPSlogBrowser_info_pb.activateCallback: spawnLogBrowser("info", UxWidget);

*SPSlogBrowser_debug_pb.class: pushButtonGadget
*SPSlogBrowser_debug_pb.static: true
*SPSlogBrowser_debug_pb.name: SPSlogBrowser_debug_pb
*SPSlogBrowser_debug_pb.parent: SPS_logBrowser_pane
*SPSlogBrowser_debug_pb.labelString: "Debug"
*SPSlogBrowser_debug_pb.mnemonic: "D"
*SPSlogBrowser_debug_pb.activateCallback: spawnLogBrowser("debug", UxWidget);

*SPSlogBrowser_error_pb.class: pushButtonGadget
*SPSlogBrowser_error_pb.static: true
*SPSlogBrowser_error_pb.name: SPSlogBrowser_error_pb
*SPSlogBrowser_error_pb.parent: SPS_logBrowser_pane
*SPSlogBrowser_error_pb.labelString: "Error"
*SPSlogBrowser_error_pb.mnemonic: "E"
*SPSlogBrowser_error_pb.activateCallback: spawnLogBrowser("error", UxWidget);

*Subsystems_pane.class: rowColumn
*Subsystems_pane.static: true
*Subsystems_pane.name: Subsystems_pane
*Subsystems_pane.parent: CPmainMenu
*Subsystems_pane.rowColumnType: "menu_pulldown"
*Subsystems_pane.tearOffModel: "tear_off_enabled"

*Sub_StartAll_pb.class: pushButtonGadget
*Sub_StartAll_pb.static: true
*Sub_StartAll_pb.name: Sub_StartAll_pb
*Sub_StartAll_pb.parent: Subsystems_pane
*Sub_StartAll_pb.labelString: "Start All"
*Sub_StartAll_pb.mnemonic: "A"
*Sub_StartAll_pb.activateCallback: {performMenuItem(handleMenuStartAllCB,NULL,NULL);}\


*Sub_ReadyAll_pb.class: pushButtonGadget
*Sub_ReadyAll_pb.static: true
*Sub_ReadyAll_pb.name: Sub_ReadyAll_pb
*Sub_ReadyAll_pb.parent: Subsystems_pane
*Sub_ReadyAll_pb.labelString: "Ready All"
*Sub_ReadyAll_pb.mnemonic: "d"
*Sub_ReadyAll_pb.activateCallback: {performMenuItem(handleMenuReadyAllCB,NULL,NULL);}\


*Sub_sep1.class: separatorGadget
*Sub_sep1.static: true
*Sub_sep1.name: Sub_sep1
*Sub_sep1.parent: Subsystems_pane

*Sub_Start_pb.class: pushButtonGadget
*Sub_Start_pb.static: true
*Sub_Start_pb.name: Sub_Start_pb
*Sub_Start_pb.parent: Subsystems_pane
*Sub_Start_pb.labelString: "Start"
*Sub_Start_pb.mnemonic: "S"
*Sub_Start_pb.activateCallback: {\
  char *getSelectedSubsystem();\
\
  handleToggleStart(getSelectedSubsystem(), UxWidget);\
}

*Sub_Ready_pb.class: pushButtonGadget
*Sub_Ready_pb.static: true
*Sub_Ready_pb.name: Sub_Ready_pb
*Sub_Ready_pb.parent: Subsystems_pane
*Sub_Ready_pb.labelString: "Ready"
*Sub_Ready_pb.mnemonic: "R"
*Sub_Ready_pb.activateCallback: {\
  char *getSelectedSubsystem();\
\
  handleToggleReady(getSelectedSubsystem());\
}

*Sub_Reset_pb.class: pushButtonGadget
*Sub_Reset_pb.static: true
*Sub_Reset_pb.name: Sub_Reset_pb
*Sub_Reset_pb.parent: Subsystems_pane
*Sub_Reset_pb.labelString: "Reset"
*Sub_Reset_pb.mnemonic: "t"
*Sub_Reset_pb.activateCallback: {\
  char *getSelectedSubsystem();\
\
  handleToggleReset(getSelectedSubsystem(), UxWidget);\
}

*Sub_Pause.class: pushButtonGadget
*Sub_Pause.static: true
*Sub_Pause.name: Sub_Pause
*Sub_Pause.parent: Subsystems_pane
*Sub_Pause.labelString: "Pause"
*Sub_Pause.mnemonic: "P"
*Sub_Pause.activateCallback: {\
  char *getSelectedSubsystem();\
\
  handleTogglePause(getSelectedSubsystem(), UxWidget);\
}

*Sub_Shutdown_pb.class: pushButtonGadget
*Sub_Shutdown_pb.static: true
*Sub_Shutdown_pb.name: Sub_Shutdown_pb
*Sub_Shutdown_pb.parent: Subsystems_pane
*Sub_Shutdown_pb.labelString: "Shutdown"
*Sub_Shutdown_pb.mnemonic: "h"
*Sub_Shutdown_pb.activateCallback: {\
  char *getSelectedSubsystem();\
\
  handleToggleStop(getSelectedSubsystem(), UxWidget);\
}

*Sub_sep2.class: separatorGadget
*Sub_sep2.static: true
*Sub_sep2.name: Sub_sep2
*Sub_sep2.parent: Subsystems_pane

*Sub_Raise.class: pushButtonGadget
*Sub_Raise.static: true
*Sub_Raise.name: Sub_Raise
*Sub_Raise.parent: Subsystems_pane
*Sub_Raise.labelString: "Raise Window"
*Sub_Raise.mnemonic: "i"
*Sub_Raise.activateCallback: {\
  char *getSelectedSubsystem();\
\
  handleToggleRaise(getSelectedSubsystem(), UxWidget);\
}

*Queue_pane.class: rowColumn
*Queue_pane.static: true
*Queue_pane.name: Queue_pane
*Queue_pane.parent: CPmainMenu
*Queue_pane.rowColumnType: "menu_pulldown"
*Queue_pane.background.source: public
*Queue_pane.background: "#dddddddddddd"
*Queue_pane.tearOffModel: "tear_off_enabled"

*RemoveHold_pb.class: pushButtonGadget
*RemoveHold_pb.static: true
*RemoveHold_pb.name: RemoveHold_pb
*RemoveHold_pb.parent: Queue_pane
*RemoveHold_pb.labelString: "Remove Hold"
*RemoveHold_pb.mnemonic: "R"
*RemoveHold_pb.activateCallback: {performMenuItem(handleMainMenuRemoveHold, NULL,NULL);}

*PerformQC_pb.class: pushButtonGadget
*PerformQC_pb.static: true
*PerformQC_pb.name: PerformQC_pb
*PerformQC_pb.parent: Queue_pane
*PerformQC_pb.labelString: "Perform QC"
*PerformQC_pb.mnemonic: "Q"
*PerformQC_pb.activateCallback: {performMenuItem(handleMainMenuPerformQC, NULL,NULL);}

*MoveJob_pb.class: pushButtonGadget
*MoveJob_pb.static: true
*MoveJob_pb.name: MoveJob_pb
*MoveJob_pb.parent: Queue_pane
*MoveJob_pb.labelString: "Move Job"
*MoveJob_pb.mnemonic: "v"
*MoveJob_pb.activateCallback: {performMenuItem(handleMainMenuMoveJob, NULL,NULL);}

*ShowInfo_pb.class: pushButtonGadget
*ShowInfo_pb.static: true
*ShowInfo_pb.name: ShowInfo_pb
*ShowInfo_pb.parent: Queue_pane
*ShowInfo_pb.labelString: "Show Detailed Info"
*ShowInfo_pb.mnemonic: "I"
*ShowInfo_pb.activateCallback: performMenuItem(handleMainMenuDetailedInfo, NULL,NULL);\


*DeleteJob_pb.class: pushButtonGadget
*DeleteJob_pb.static: true
*DeleteJob_pb.name: DeleteJob_pb
*DeleteJob_pb.parent: Queue_pane
*DeleteJob_pb.labelString: "Cancel Job"
*DeleteJob_pb.mnemonic: "C"
*DeleteJob_pb.activateCallback: {performMenuItem(handleMainMenuDelete, NULL,NULL); }

*External_pane.class: rowColumn
*External_pane.static: true
*External_pane.name: External_pane
*External_pane.parent: CPmainMenu
*External_pane.rowColumnType: "menu_pulldown"
*External_pane.background.source: public
*External_pane.background: "#dddddddddddd"
*External_pane.tearOffModel: "tear_off_enabled"

*Ext_StartAll_pb.class: pushButtonGadget
*Ext_StartAll_pb.static: true
*Ext_StartAll_pb.name: Ext_StartAll_pb
*Ext_StartAll_pb.parent: External_pane
*Ext_StartAll_pb.labelString: "Start All"
*Ext_StartAll_pb.mnemonic: "A"
*Ext_StartAll_pb.activateCallback: {performMenuItem(handleMenuStartAllExtCB,NULL,NULL); }

*Ext_ReadyAll_pb.class: pushButtonGadget
*Ext_ReadyAll_pb.static: true
*Ext_ReadyAll_pb.name: Ext_ReadyAll_pb
*Ext_ReadyAll_pb.parent: External_pane
*Ext_ReadyAll_pb.labelString: "Ready All"
*Ext_ReadyAll_pb.mnemonic: "d"
*Ext_ReadyAll_pb.activateCallback: {performMenuItem(handleMenuReadyAllExtCB,NULL,NULL); }

*Ext_sep1.class: separatorGadget
*Ext_sep1.static: true
*Ext_sep1.name: Ext_sep1
*Ext_sep1.parent: External_pane

*Ext_Start_pb.class: pushButtonGadget
*Ext_Start_pb.static: true
*Ext_Start_pb.name: Ext_Start_pb
*Ext_Start_pb.parent: External_pane
*Ext_Start_pb.labelString: "Start"
*Ext_Start_pb.activateCallback: {handleToggleStart(getSelectedExternal(), UxWidget);}
*Ext_Start_pb.mnemonic: "S"

*Ext_Ready_pb.class: pushButtonGadget
*Ext_Ready_pb.static: true
*Ext_Ready_pb.name: Ext_Ready_pb
*Ext_Ready_pb.parent: External_pane
*Ext_Ready_pb.labelString: "Ready"
*Ext_Ready_pb.mnemonic: "R"
*Ext_Ready_pb.activateCallback: {handleToggleReady(getSelectedExternal(), UxWidget);}

*Ext_Reset_pb.class: pushButtonGadget
*Ext_Reset_pb.static: true
*Ext_Reset_pb.name: Ext_Reset_pb
*Ext_Reset_pb.parent: External_pane
*Ext_Reset_pb.labelString: "Reset"
*Ext_Reset_pb.mnemonic: "t"
*Ext_Reset_pb.activateCallback: {handleToggleReset(getSelectedExternal(), UxWidget);}

*Ext_Pause_pb.class: pushButtonGadget
*Ext_Pause_pb.static: true
*Ext_Pause_pb.name: Ext_Pause_pb
*Ext_Pause_pb.parent: External_pane
*Ext_Pause_pb.labelString: "Pause"
*Ext_Pause_pb.mnemonic: "P"
*Ext_Pause_pb.activateCallback: {handleTogglePause(getSelectedExternal(), UxWidget);}

*Ext_Stop_pb.class: pushButtonGadget
*Ext_Stop_pb.static: true
*Ext_Stop_pb.name: Ext_Stop_pb
*Ext_Stop_pb.parent: External_pane
*Ext_Stop_pb.labelString: "Shutdown"
*Ext_Stop_pb.mnemonic: "h"
*Ext_Stop_pb.activateCallback: {handleToggleStop(getSelectedExternal(), UxWidget);}

*Ext_sep2.class: separatorGadget
*Ext_sep2.static: true
*Ext_sep2.name: Ext_sep2
*Ext_sep2.parent: External_pane

*Ext_Raise_pb.class: pushButtonGadget
*Ext_Raise_pb.static: true
*Ext_Raise_pb.name: Ext_Raise_pb
*Ext_Raise_pb.parent: External_pane
*Ext_Raise_pb.labelString: "Raise Window"
*Ext_Raise_pb.mnemonic: "i"
*Ext_Raise_pb.activateCallback: {handleToggleRaise(getSelectedExternal(), UxWidget);}

*Options_pane.class: rowColumn
*Options_pane.static: true
*Options_pane.name: Options_pane
*Options_pane.parent: CPmainMenu
*Options_pane.rowColumnType: "menu_pulldown"
*Options_pane.background.source: public
*Options_pane.background: "#dddddddddddd"

*ScanDest_Radio_pane.class: cascadeButtonGadget
*ScanDest_Radio_pane.static: true
*ScanDest_Radio_pane.name: ScanDest_Radio_pane
*ScanDest_Radio_pane.parent: Options_pane
*ScanDest_Radio_pane.labelString: "Scan Destination"
*ScanDest_Radio_pane.mnemonic: "D"
*ScanDest_Radio_pane.subMenuId: "ScanDest_pane"

*QC_invocation_Radio_pane.class: cascadeButtonGadget
*QC_invocation_Radio_pane.static: true
*QC_invocation_Radio_pane.name: QC_invocation_Radio_pane
*QC_invocation_Radio_pane.parent: Options_pane
*QC_invocation_Radio_pane.labelString: "QC Invocation"
*QC_invocation_Radio_pane.mnemonic: "Q"
*QC_invocation_Radio_pane.subMenuId: "QC_invocation_pane"

*ScanDest_pane.class: rowColumn
*ScanDest_pane.static: true
*ScanDest_pane.name: ScanDest_pane
*ScanDest_pane.parent: Options_pane
*ScanDest_pane.rowColumnType: "menu_pulldown"
*ScanDest_pane.radioBehavior: "true"
*ScanDest_pane.background.source: public
*ScanDest_pane.background: "#dddddddddddd"

*toggleScanDest_RDS.class: toggleButtonGadget
*toggleScanDest_RDS.name.source: public
*toggleScanDest_RDS.static: false
*toggleScanDest_RDS.name: toggleScanDest_RDS
*toggleScanDest_RDS.parent: ScanDest_pane
*toggleScanDest_RDS.labelString: "RDS"
*toggleScanDest_RDS.mnemonic: "R"
*toggleScanDest_RDS.set.source: public
*toggleScanDest_RDS.set: "true"
*toggleScanDest_RDS.valueChangedCallback.source: public
*toggleScanDest_RDS.valueChangedCallback: 

*toggleScanDest_ASP.class: toggleButtonGadget
*toggleScanDest_ASP.name.source: public
*toggleScanDest_ASP.static: false
*toggleScanDest_ASP.name: toggleScanDest_ASP
*toggleScanDest_ASP.parent: ScanDest_pane
*toggleScanDest_ASP.labelString: "ASP"
*toggleScanDest_ASP.mnemonic: "A"
*toggleScanDest_ASP.set.source: public
*toggleScanDest_ASP.set: "false"
*toggleScanDest_ASP.valueChangedCallback.source: public
*toggleScanDest_ASP.valueChangedCallback: 

*QC_invocation_pane.class: rowColumn
*QC_invocation_pane.static: true
*QC_invocation_pane.name: QC_invocation_pane
*QC_invocation_pane.parent: Options_pane
*QC_invocation_pane.rowColumnType: "menu_pulldown"
*QC_invocation_pane.radioBehavior: "true"
*QC_invocation_pane.background.source: public
*QC_invocation_pane.background: "#dddddddddddd"

*toggleQC_automatic.class: toggleButtonGadget
*toggleQC_automatic.name.source: public
*toggleQC_automatic.static: false
*toggleQC_automatic.name: toggleQC_automatic
*toggleQC_automatic.parent: QC_invocation_pane
*toggleQC_automatic.labelString: "Automatic"
*toggleQC_automatic.mnemonic: "A"
*toggleQC_automatic.set.source: public
*toggleQC_automatic.set: "false"
*toggleQC_automatic.valueChangedCallback.source: public
*toggleQC_automatic.valueChangedCallback: 

*toggleQC_manual.class: toggleButtonGadget
*toggleQC_manual.name.source: public
*toggleQC_manual.static: false
*toggleQC_manual.name: toggleQC_manual
*toggleQC_manual.parent: QC_invocation_pane
*toggleQC_manual.labelString: "Interactive"
*toggleQC_manual.mnemonic: "I"
*toggleQC_manual.set.source: public
*toggleQC_manual.set: "true"
*toggleQC_manual.valueChangedCallback.source: public
*toggleQC_manual.valueChangedCallback: 

*Help_pane.class: rowColumn
*Help_pane.static: true
*Help_pane.name: Help_pane
*Help_pane.parent: CPmainMenu
*Help_pane.rowColumnType: "menu_pulldown"
*Help_pane.background.source: public
*Help_pane.background: "#dddddddddddd"

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
*JobStates_pb.activateCallback: showHelp(HELP_JOB_STATES, "");

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

*File_cb.class: cascadeButton
*File_cb.static: true
*File_cb.name: File_cb
*File_cb.parent: CPmainMenu
*File_cb.labelString: "File"
*File_cb.mnemonic: "F"
*File_cb.subMenuId: "File_pane"
*File_cb.background.source: public
*File_cb.background: "#dddddddddddd"

*Tools_cb.class: cascadeButton
*Tools_cb.static: true
*Tools_cb.name: Tools_cb
*Tools_cb.parent: CPmainMenu
*Tools_cb.labelString: "Tools"
*Tools_cb.subMenuId: "Tools_pane"
*Tools_cb.mnemonic: "T"
*Tools_cb.background.source: public
*Tools_cb.background: "#dddddddddddd"

*Subsystems_cb.class: cascadeButton
*Subsystems_cb.static: true
*Subsystems_cb.name: Subsystems_cb
*Subsystems_cb.parent: CPmainMenu
*Subsystems_cb.labelString: "Subsystems"
*Subsystems_cb.mnemonic: "S"
*Subsystems_cb.subMenuId: "Subsystems_pane"
*Subsystems_cb.background.source: public
*Subsystems_cb.background: "#dddddddddddd"

*Queue_cb.class: cascadeButton
*Queue_cb.static: true
*Queue_cb.name: Queue_cb
*Queue_cb.parent: CPmainMenu
*Queue_cb.labelString: "Jobs"
*Queue_cb.subMenuId: "Queue_pane"
*Queue_cb.mnemonic: "J"
*Queue_cb.background.source: public
*Queue_cb.background: "#dddddddddddd"

*External_cb.class: cascadeButton
*External_cb.static: true
*External_cb.name: External_cb
*External_cb.parent: CPmainMenu
*External_cb.labelString: "External"
*External_cb.mnemonic: "E"
*External_cb.subMenuId: "External_pane"
*External_cb.background.source: public
*External_cb.background: "#dddddddddddd"

*Options_cb.class: cascadeButton
*Options_cb.static: true
*Options_cb.name: Options_cb
*Options_cb.parent: CPmainMenu
*Options_cb.labelString: "Options"
*Options_cb.subMenuId: "Options_pane"
*Options_cb.mnemonic: "O"
*Options_cb.background.source: public
*Options_cb.background: "#dddddddddddd"

*HelpPane_cb.class: cascadeButton
*HelpPane_cb.static: true
*HelpPane_cb.name: HelpPane_cb
*HelpPane_cb.parent: CPmainMenu
*HelpPane_cb.labelString: "Help"
*HelpPane_cb.mnemonic: "H"
*HelpPane_cb.subMenuId: "Help_pane"
*HelpPane_cb.background.source: public
*HelpPane_cb.background: "#dddddddddddd"

*panedWindow1.class: panedWindow
*panedWindow1.static: true
*panedWindow1.name: panedWindow1
*panedWindow1.parent: CPmainQ
*panedWindow1.separatorOn: "true"
*panedWindow1.marginWidth: 0
*panedWindow1.background: "#a050a0"

*CPjobs_form.class: form
*CPjobs_form.name.source: public
*CPjobs_form.static: false
*CPjobs_form.name: CPjobs_form
*CPjobs_form.parent: panedWindow1
*CPjobs_form.resizePolicy: "resize_none"
*CPjobs_form.x: 0
*CPjobs_form.y: 342
*CPjobs_form.positionIndex: 1
*CPjobs_form.allowResize: "true"
*CPjobs_form.labelFontList.source: public
*CPjobs_form.labelFontList: "helvbo10"
*CPjobs_form.height.source: public
*CPjobs_form.height: 175

*CPmainJobIdScrolledWin.class: scrolledWindow
*CPmainJobIdScrolledWin.name.source: public
*CPmainJobIdScrolledWin.static: false
*CPmainJobIdScrolledWin.name: CPmainJobIdScrolledWin
*CPmainJobIdScrolledWin.parent: CPjobs_form
*CPmainJobIdScrolledWin.scrollingPolicy: "application_defined"
*CPmainJobIdScrolledWin.x: 8
*CPmainJobIdScrolledWin.y: 30
*CPmainJobIdScrolledWin.width: 85
*CPmainJobIdScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainJobIdScrolledWin.createCallback: {\
\
}
*CPmainJobIdScrolledWin.visualPolicy: "variable"
*CPmainJobIdScrolledWin.spacing: 0
*CPmainJobIdScrolledWin.bottomAttachment: "attach_form"
*CPmainJobIdScrolledWin.bottomOffset: 5
*CPmainJobIdScrolledWin.topAttachment: "attach_form"
*CPmainJobIdScrolledWin.topOffset: 60
*CPmainJobIdScrolledWin.shadowThickness: 2

*CPmainJobIdList.class: scrolledList
*CPmainJobIdList.name.source: public
*CPmainJobIdList.static: false
*CPmainJobIdList.name: CPmainJobIdList
*CPmainJobIdList.parent: CPmainJobIdScrolledWin
*CPmainJobIdList.fontList.source: public
*CPmainJobIdList.fontList: "helvb10"
*CPmainJobIdList.stringDirection: "string_direction_l_to_r"
*CPmainJobIdList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainJobIdList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainJobIdList.listSizePolicy: "constant"
*CPmainJobIdList.highlightThickness: 0
*CPmainJobIdList.shadowThickness: 0
*CPmainJobIdList.listMarginWidth: 2
*CPmainJobIdList.scrollBarDisplayPolicy: "static"
*CPmainJobIdList.visibleItemCount.source: public
*CPmainJobIdList.visibleItemCount: 7
*CPmainJobIdList.foreground.source: public
*CPmainJobIdList.foreground: "black"

*CPmainPlatRevSeqScrolledWin.class: scrolledWindow
*CPmainPlatRevSeqScrolledWin.name.source: public
*CPmainPlatRevSeqScrolledWin.static: false
*CPmainPlatRevSeqScrolledWin.name: CPmainPlatRevSeqScrolledWin
*CPmainPlatRevSeqScrolledWin.parent: CPjobs_form
*CPmainPlatRevSeqScrolledWin.scrollingPolicy: "application_defined"
*CPmainPlatRevSeqScrolledWin.width: 125
*CPmainPlatRevSeqScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainPlatRevSeqScrolledWin.createCallback: {\
\
}
*CPmainPlatRevSeqScrolledWin.visualPolicy: "variable"
*CPmainPlatRevSeqScrolledWin.spacing: 0
*CPmainPlatRevSeqScrolledWin.bottomAttachment: "attach_form"
*CPmainPlatRevSeqScrolledWin.bottomOffset: 5
*CPmainPlatRevSeqScrolledWin.topAttachment: "attach_form"
*CPmainPlatRevSeqScrolledWin.topOffset: 60
*CPmainPlatRevSeqScrolledWin.leftAttachment: "attach_widget"
*CPmainPlatRevSeqScrolledWin.leftOffset: 5
*CPmainPlatRevSeqScrolledWin.leftWidget: "CPmainJobIdScrolledWin"
*CPmainPlatRevSeqScrolledWin.shadowThickness: 2

*CPmainPlatRevSeqList.class: scrolledList
*CPmainPlatRevSeqList.name.source: public
*CPmainPlatRevSeqList.static: false
*CPmainPlatRevSeqList.name: CPmainPlatRevSeqList
*CPmainPlatRevSeqList.parent: CPmainPlatRevSeqScrolledWin
*CPmainPlatRevSeqList.fontList.source: public
*CPmainPlatRevSeqList.fontList: "helvb10"
*CPmainPlatRevSeqList.stringDirection: "string_direction_l_to_r"
*CPmainPlatRevSeqList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainPlatRevSeqList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainPlatRevSeqList.listSizePolicy: "constant"
*CPmainPlatRevSeqList.highlightThickness: 0
*CPmainPlatRevSeqList.shadowThickness: 0
*CPmainPlatRevSeqList.listMarginWidth: 2
*CPmainPlatRevSeqList.scrollBarDisplayPolicy: "static"
*CPmainPlatRevSeqList.visibleItemCount.source: public
*CPmainPlatRevSeqList.visibleItemCount: 7
*CPmainPlatRevSeqList.foreground.source: public
*CPmainPlatRevSeqList.foreground: "black"

*CPmainFrameScrolledWin.class: scrolledWindow
*CPmainFrameScrolledWin.name.source: public
*CPmainFrameScrolledWin.static: false
*CPmainFrameScrolledWin.name: CPmainFrameScrolledWin
*CPmainFrameScrolledWin.parent: CPjobs_form
*CPmainFrameScrolledWin.scrollingPolicy: "application_defined"
*CPmainFrameScrolledWin.width: 75
*CPmainFrameScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainFrameScrolledWin.createCallback: {\
\
}
*CPmainFrameScrolledWin.visualPolicy: "variable"
*CPmainFrameScrolledWin.spacing: 0
*CPmainFrameScrolledWin.bottomAttachment: "attach_form"
*CPmainFrameScrolledWin.bottomOffset: 5
*CPmainFrameScrolledWin.topAttachment: "attach_form"
*CPmainFrameScrolledWin.topOffset: 60
*CPmainFrameScrolledWin.leftAttachment: "attach_widget"
*CPmainFrameScrolledWin.leftOffset: 5
*CPmainFrameScrolledWin.leftWidget: "CPmainPlatRevSeqScrolledWin"
*CPmainFrameScrolledWin.shadowThickness: 2

*CPmainFrameList.class: scrolledList
*CPmainFrameList.name.source: public
*CPmainFrameList.static: false
*CPmainFrameList.name: CPmainFrameList
*CPmainFrameList.parent: CPmainFrameScrolledWin
*CPmainFrameList.fontList.source: public
*CPmainFrameList.fontList: "helvb10"
*CPmainFrameList.stringDirection: "string_direction_l_to_r"
*CPmainFrameList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainFrameList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainFrameList.listSizePolicy: "constant"
*CPmainFrameList.highlightThickness: 0
*CPmainFrameList.shadowThickness: 0
*CPmainFrameList.listMarginWidth: 2
*CPmainFrameList.scrollBarDisplayPolicy: "static"
*CPmainFrameList.visibleItemCount.source: public
*CPmainFrameList.visibleItemCount: 7
*CPmainFrameList.foreground.source: public
*CPmainFrameList.foreground: "black"

*CPmainMediaScrolledWin.class: scrolledWindow
*CPmainMediaScrolledWin.name.source: public
*CPmainMediaScrolledWin.static: false
*CPmainMediaScrolledWin.name: CPmainMediaScrolledWin
*CPmainMediaScrolledWin.parent: CPjobs_form
*CPmainMediaScrolledWin.scrollingPolicy: "application_defined"
*CPmainMediaScrolledWin.width: 200
*CPmainMediaScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainMediaScrolledWin.createCallback: {\
\
}
*CPmainMediaScrolledWin.visualPolicy: "variable"
*CPmainMediaScrolledWin.spacing: 0
*CPmainMediaScrolledWin.bottomAttachment: "attach_form"
*CPmainMediaScrolledWin.bottomOffset: 5
*CPmainMediaScrolledWin.topAttachment: "attach_form"
*CPmainMediaScrolledWin.topOffset: 60
*CPmainMediaScrolledWin.leftAttachment: "attach_widget"
*CPmainMediaScrolledWin.leftOffset: 5
*CPmainMediaScrolledWin.leftWidget: "CPmainFrameScrolledWin"
*CPmainMediaScrolledWin.shadowThickness: 2

*CPmainMediaList.class: scrolledList
*CPmainMediaList.name.source: public
*CPmainMediaList.static: false
*CPmainMediaList.name: CPmainMediaList
*CPmainMediaList.parent: CPmainMediaScrolledWin
*CPmainMediaList.fontList.source: public
*CPmainMediaList.fontList: "helvb10"
*CPmainMediaList.stringDirection: "string_direction_l_to_r"
*CPmainMediaList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainMediaList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainMediaList.listSizePolicy: "constant"
*CPmainMediaList.highlightThickness: 0
*CPmainMediaList.shadowThickness: 0
*CPmainMediaList.listMarginWidth: 2
*CPmainMediaList.scrollBarDisplayPolicy: "static"
*CPmainMediaList.visibleItemCount.source: public
*CPmainMediaList.visibleItemCount: 7
*CPmainMediaList.foreground.source: public
*CPmainMediaList.foreground: "black"

*CPmainModeScrolledWin.class: scrolledWindow
*CPmainModeScrolledWin.name.source: public
*CPmainModeScrolledWin.static: false
*CPmainModeScrolledWin.name: CPmainModeScrolledWin
*CPmainModeScrolledWin.parent: CPjobs_form
*CPmainModeScrolledWin.scrollingPolicy: "application_defined"
*CPmainModeScrolledWin.width: 65
*CPmainModeScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainModeScrolledWin.createCallback: {\
\
}
*CPmainModeScrolledWin.visualPolicy: "variable"
*CPmainModeScrolledWin.spacing: 0
*CPmainModeScrolledWin.bottomAttachment: "attach_form"
*CPmainModeScrolledWin.bottomOffset: 5
*CPmainModeScrolledWin.topAttachment: "attach_form"
*CPmainModeScrolledWin.topOffset: 60
*CPmainModeScrolledWin.leftAttachment: "attach_widget"
*CPmainModeScrolledWin.leftOffset: 5
*CPmainModeScrolledWin.leftWidget: "CPmainMediaScrolledWin"
*CPmainModeScrolledWin.shadowThickness: 2

*CPmainModeList.class: scrolledList
*CPmainModeList.name.source: public
*CPmainModeList.static: false
*CPmainModeList.name: CPmainModeList
*CPmainModeList.parent: CPmainModeScrolledWin
*CPmainModeList.fontList.source: public
*CPmainModeList.fontList: "helvb10"
*CPmainModeList.stringDirection: "string_direction_l_to_r"
*CPmainModeList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainModeList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainModeList.listSizePolicy: "constant"
*CPmainModeList.highlightThickness: 0
*CPmainModeList.shadowThickness: 0
*CPmainModeList.listMarginWidth: 2
*CPmainModeList.scrollBarDisplayPolicy: "static"
*CPmainModeList.visibleItemCount.source: public
*CPmainModeList.visibleItemCount: 7
*CPmainModeList.foreground.source: public
*CPmainModeList.foreground: "black"

*CPmainRequestScrolledWin.class: scrolledWindow
*CPmainRequestScrolledWin.name.source: public
*CPmainRequestScrolledWin.static: false
*CPmainRequestScrolledWin.name: CPmainRequestScrolledWin
*CPmainRequestScrolledWin.parent: CPjobs_form
*CPmainRequestScrolledWin.scrollingPolicy: "application_defined"
*CPmainRequestScrolledWin.width: 90
*CPmainRequestScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainRequestScrolledWin.createCallback: {\
\
}
*CPmainRequestScrolledWin.visualPolicy: "variable"
*CPmainRequestScrolledWin.spacing: 0
*CPmainRequestScrolledWin.bottomAttachment: "attach_form"
*CPmainRequestScrolledWin.bottomOffset: 5
*CPmainRequestScrolledWin.topAttachment: "attach_form"
*CPmainRequestScrolledWin.topOffset: 60
*CPmainRequestScrolledWin.leftAttachment: "attach_widget"
*CPmainRequestScrolledWin.leftOffset: 5
*CPmainRequestScrolledWin.leftWidget: "CPmainModeScrolledWin"
*CPmainRequestScrolledWin.shadowThickness: 2

*CPmainRequestList.class: scrolledList
*CPmainRequestList.name.source: public
*CPmainRequestList.static: false
*CPmainRequestList.name: CPmainRequestList
*CPmainRequestList.parent: CPmainRequestScrolledWin
*CPmainRequestList.fontList.source: public
*CPmainRequestList.fontList: "helvb10"
*CPmainRequestList.stringDirection: "string_direction_l_to_r"
*CPmainRequestList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainRequestList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainRequestList.listSizePolicy: "constant"
*CPmainRequestList.highlightThickness: 0
*CPmainRequestList.shadowThickness: 0
*CPmainRequestList.listMarginWidth: 2
*CPmainRequestList.scrollBarDisplayPolicy: "static"
*CPmainRequestList.visibleItemCount.source: public
*CPmainRequestList.visibleItemCount: 7
*CPmainRequestList.foreground.source: public
*CPmainRequestList.foreground: "black"

*CPmainTypeScrolledWin.class: scrolledWindow
*CPmainTypeScrolledWin.name.source: public
*CPmainTypeScrolledWin.static: false
*CPmainTypeScrolledWin.name: CPmainTypeScrolledWin
*CPmainTypeScrolledWin.parent: CPjobs_form
*CPmainTypeScrolledWin.scrollingPolicy: "application_defined"
*CPmainTypeScrolledWin.width: 145
*CPmainTypeScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainTypeScrolledWin.createCallback: {\
\
}
*CPmainTypeScrolledWin.visualPolicy: "variable"
*CPmainTypeScrolledWin.spacing: 0
*CPmainTypeScrolledWin.bottomAttachment: "attach_form"
*CPmainTypeScrolledWin.bottomOffset: 5
*CPmainTypeScrolledWin.topAttachment: "attach_form"
*CPmainTypeScrolledWin.topOffset: 60
*CPmainTypeScrolledWin.leftAttachment: "attach_widget"
*CPmainTypeScrolledWin.leftOffset: 5
*CPmainTypeScrolledWin.leftWidget: "CPmainRequestScrolledWin"
*CPmainTypeScrolledWin.shadowThickness: 2

*CPmainTypeList.class: scrolledList
*CPmainTypeList.name.source: public
*CPmainTypeList.static: false
*CPmainTypeList.name: CPmainTypeList
*CPmainTypeList.parent: CPmainTypeScrolledWin
*CPmainTypeList.fontList.source: public
*CPmainTypeList.fontList: "helvb10"
*CPmainTypeList.stringDirection: "string_direction_l_to_r"
*CPmainTypeList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainTypeList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainTypeList.listSizePolicy: "constant"
*CPmainTypeList.highlightThickness: 0
*CPmainTypeList.shadowThickness: 0
*CPmainTypeList.listMarginWidth: 2
*CPmainTypeList.scrollBarDisplayPolicy: "static"
*CPmainTypeList.visibleItemCount.source: public
*CPmainTypeList.visibleItemCount: 7
*CPmainTypeList.foreground.source: public
*CPmainTypeList.foreground: "black"

*CPmainStatusScrolledWin.class: scrolledWindow
*CPmainStatusScrolledWin.name.source: public
*CPmainStatusScrolledWin.static: false
*CPmainStatusScrolledWin.name: CPmainStatusScrolledWin
*CPmainStatusScrolledWin.parent: CPjobs_form
*CPmainStatusScrolledWin.scrollingPolicy: "application_defined"
*CPmainStatusScrolledWin.width: 215
*CPmainStatusScrolledWin.scrollBarDisplayPolicy: "static"
*CPmainStatusScrolledWin.createCallback: {\
\
}
*CPmainStatusScrolledWin.visualPolicy: "variable"
*CPmainStatusScrolledWin.spacing: 0
*CPmainStatusScrolledWin.bottomAttachment: "attach_form"
*CPmainStatusScrolledWin.bottomOffset: 5
*CPmainStatusScrolledWin.topAttachment: "attach_form"
*CPmainStatusScrolledWin.topOffset: 60
*CPmainStatusScrolledWin.leftAttachment: "attach_widget"
*CPmainStatusScrolledWin.leftOffset: 5
*CPmainStatusScrolledWin.leftWidget: "CPmainTypeScrolledWin"
*CPmainStatusScrolledWin.rightAttachment: "attach_form"
*CPmainStatusScrolledWin.rightOffset: 10
*CPmainStatusScrolledWin.shadowThickness: 2

*CPmainStatusList.class: scrolledList
*CPmainStatusList.name.source: public
*CPmainStatusList.static: false
*CPmainStatusList.name: CPmainStatusList
*CPmainStatusList.parent: CPmainStatusScrolledWin
*CPmainStatusList.fontList.source: public
*CPmainStatusList.fontList: "helvb10"
*CPmainStatusList.defaultActionCallback: {\
showDetailedInfo(UxWidget,  UxCallbackArg); \
}
*CPmainStatusList.browseSelectionCallback: {\
syncLists(UxWidget, CPmainQ, UxCallbackArg);\
}
*CPmainStatusList.listSizePolicy: "constant"
*CPmainStatusList.highlightThickness: 0
*CPmainStatusList.shadowThickness: 0
*CPmainStatusList.listMarginWidth: 2
*CPmainStatusList.scrollBarDisplayPolicy: "static"
*CPmainStatusList.visibleItemCount.source: public
*CPmainStatusList.visibleItemCount: 7
*CPmainStatusList.foreground.source: public
*CPmainStatusList.foreground: "black"

*CPjobs_label.class: label
*CPjobs_label.static: true
*CPjobs_label.name: CPjobs_label
*CPjobs_label.parent: CPjobs_form
*CPjobs_label.x: 0
*CPjobs_label.y: 3
*CPjobs_label.width: 975
*CPjobs_label.height: 30
*CPjobs_label.fontList: "lucibis18"
*CPjobs_label.labelString: "Jobs"
*CPjobs_label.leftAttachment: "attach_form"
*CPjobs_label.rightAttachment: "attach_form"

*CPmainJobIdLabel.class: labelGadget
*CPmainJobIdLabel.static: true
*CPmainJobIdLabel.name: CPmainJobIdLabel
*CPmainJobIdLabel.parent: CPjobs_form
*CPmainJobIdLabel.x: 8
*CPmainJobIdLabel.y: 10
*CPmainJobIdLabel.height: 20
*CPmainJobIdLabel.labelString: "Job ID"
*CPmainJobIdLabel.alignment: "alignment_beginning"
*CPmainJobIdLabel.fontList.source: public
*CPmainJobIdLabel.fontList: "helvbo10"
*CPmainJobIdLabel.leftAttachment: "attach_opposite_widget"
*CPmainJobIdLabel.leftWidget: "CPmainJobIdScrolledWin"
*CPmainJobIdLabel.rightAttachment: "attach_opposite_widget"
*CPmainJobIdLabel.rightWidget: "CPmainJobIdScrolledWin"
*CPmainJobIdLabel.topAttachment: "attach_widget"
*CPmainJobIdLabel.topWidget: "CPjobs_label"
*CPmainJobIdLabel.topOffset: 6

*CPmainPlatRevSeqLabel.class: labelGadget
*CPmainPlatRevSeqLabel.static: true
*CPmainPlatRevSeqLabel.name: CPmainPlatRevSeqLabel
*CPmainPlatRevSeqLabel.parent: CPjobs_form
*CPmainPlatRevSeqLabel.x: 100
*CPmainPlatRevSeqLabel.y: 10
*CPmainPlatRevSeqLabel.height: 20
*CPmainPlatRevSeqLabel.labelString: "Plat/Rev/Seq"
*CPmainPlatRevSeqLabel.alignment: "alignment_beginning"
*CPmainPlatRevSeqLabel.fontList.source: public
*CPmainPlatRevSeqLabel.fontList: "helvbo10"
*CPmainPlatRevSeqLabel.leftAttachment: "attach_opposite_widget"
*CPmainPlatRevSeqLabel.leftWidget: "CPmainPlatRevSeqScrolledWin"
*CPmainPlatRevSeqLabel.rightAttachment: "attach_opposite_widget"
*CPmainPlatRevSeqLabel.rightWidget: "CPmainPlatRevSeqScrolledWin"
*CPmainPlatRevSeqLabel.topAttachment: "attach_widget"
*CPmainPlatRevSeqLabel.topWidget: "CPjobs_label"
*CPmainPlatRevSeqLabel.topOffset: 6

*CPmainFrameLabel.class: labelGadget
*CPmainFrameLabel.static: true
*CPmainFrameLabel.name: CPmainFrameLabel
*CPmainFrameLabel.parent: CPjobs_form
*CPmainFrameLabel.x: 232
*CPmainFrameLabel.y: 10
*CPmainFrameLabel.height: 20
*CPmainFrameLabel.labelString: "Frame"
*CPmainFrameLabel.alignment: "alignment_beginning"
*CPmainFrameLabel.fontList.source: public
*CPmainFrameLabel.fontList: "helvbo10"
*CPmainFrameLabel.leftAttachment: "attach_opposite_widget"
*CPmainFrameLabel.leftWidget: "CPmainFrameScrolledWin"
*CPmainFrameLabel.rightAttachment: "attach_opposite_widget"
*CPmainFrameLabel.rightWidget: "CPmainFrameScrolledWin"
*CPmainFrameLabel.topAttachment: "attach_widget"
*CPmainFrameLabel.topWidget: "CPjobs_label"
*CPmainFrameLabel.topOffset: 6

*CPmainMediaLabel.class: labelGadget
*CPmainMediaLabel.static: true
*CPmainMediaLabel.name: CPmainMediaLabel
*CPmainMediaLabel.parent: CPjobs_form
*CPmainMediaLabel.x: 314
*CPmainMediaLabel.y: 10
*CPmainMediaLabel.height: 20
*CPmainMediaLabel.labelString: "Media"
*CPmainMediaLabel.alignment: "alignment_beginning"
*CPmainMediaLabel.fontList.source: public
*CPmainMediaLabel.fontList: "helvbo10"
*CPmainMediaLabel.leftAttachment: "attach_opposite_widget"
*CPmainMediaLabel.leftWidget: "CPmainMediaScrolledWin"
*CPmainMediaLabel.rightAttachment: "attach_opposite_widget"
*CPmainMediaLabel.rightWidget: "CPmainMediaScrolledWin"
*CPmainMediaLabel.topAttachment: "attach_widget"
*CPmainMediaLabel.topWidget: "CPjobs_label"
*CPmainMediaLabel.topOffset: 6

*CPmainModeLabel.class: labelGadget
*CPmainModeLabel.static: true
*CPmainModeLabel.name: CPmainModeLabel
*CPmainModeLabel.parent: CPjobs_form
*CPmainModeLabel.x: 406
*CPmainModeLabel.y: 10
*CPmainModeLabel.height: 20
*CPmainModeLabel.labelString: "Mode"
*CPmainModeLabel.alignment: "alignment_beginning"
*CPmainModeLabel.fontList.source: public
*CPmainModeLabel.fontList: "helvbo10"
*CPmainModeLabel.leftAttachment: "attach_opposite_widget"
*CPmainModeLabel.leftWidget: "CPmainModeScrolledWin"
*CPmainModeLabel.rightAttachment: "attach_opposite_widget"
*CPmainModeLabel.rightWidget: "CPmainModeScrolledWin"
*CPmainModeLabel.topAttachment: "attach_widget"
*CPmainModeLabel.topWidget: "CPjobs_label"
*CPmainModeLabel.topOffset: 6

*CPmainRequestLabel.class: labelGadget
*CPmainRequestLabel.static: true
*CPmainRequestLabel.name: CPmainRequestLabel
*CPmainRequestLabel.parent: CPjobs_form
*CPmainRequestLabel.x: 478
*CPmainRequestLabel.y: 10
*CPmainRequestLabel.height: 20
*CPmainRequestLabel.labelString: "Request"
*CPmainRequestLabel.alignment: "alignment_beginning"
*CPmainRequestLabel.fontList.source: public
*CPmainRequestLabel.fontList: "helvbo10"
*CPmainRequestLabel.leftAttachment: "attach_opposite_widget"
*CPmainRequestLabel.leftWidget: "CPmainRequestScrolledWin"
*CPmainRequestLabel.rightAttachment: "attach_opposite_widget"
*CPmainRequestLabel.rightWidget: "CPmainRequestScrolledWin"
*CPmainRequestLabel.topAttachment: "attach_widget"
*CPmainRequestLabel.topWidget: "CPjobs_label"
*CPmainRequestLabel.topOffset: 6

*CPmainTypeLabel.class: labelGadget
*CPmainTypeLabel.static: true
*CPmainTypeLabel.name: CPmainTypeLabel
*CPmainTypeLabel.parent: CPjobs_form
*CPmainTypeLabel.x: 560
*CPmainTypeLabel.y: 10
*CPmainTypeLabel.height: 20
*CPmainTypeLabel.labelString: "Product Type"
*CPmainTypeLabel.alignment: "alignment_beginning"
*CPmainTypeLabel.fontList.source: public
*CPmainTypeLabel.fontList: "helvbo10"
*CPmainTypeLabel.leftAttachment: "attach_opposite_widget"
*CPmainTypeLabel.leftWidget: "CPmainTypeScrolledWin"
*CPmainTypeLabel.rightAttachment: "attach_opposite_widget"
*CPmainTypeLabel.rightWidget: "CPmainTypeScrolledWin"
*CPmainTypeLabel.topAttachment: "attach_widget"
*CPmainTypeLabel.topWidget: "CPjobs_label"
*CPmainTypeLabel.topOffset: 6

*CPmainStatusLabel.class: labelGadget
*CPmainStatusLabel.static: true
*CPmainStatusLabel.name: CPmainStatusLabel
*CPmainStatusLabel.parent: CPjobs_form
*CPmainStatusLabel.x: 677
*CPmainStatusLabel.y: 10
*CPmainStatusLabel.height: 20
*CPmainStatusLabel.labelString: "Status"
*CPmainStatusLabel.alignment: "alignment_beginning"
*CPmainStatusLabel.fontList.source: public
*CPmainStatusLabel.fontList: "helvbo10"
*CPmainStatusLabel.leftAttachment: "attach_opposite_widget"
*CPmainStatusLabel.leftWidget: "CPmainStatusScrolledWin"
*CPmainStatusLabel.rightAttachment: "attach_opposite_widget"
*CPmainStatusLabel.rightWidget: "CPmainStatusScrolledWin"
*CPmainStatusLabel.topAttachment: "attach_widget"
*CPmainStatusLabel.topWidget: "CPjobs_label"
*CPmainStatusLabel.topOffset: 6

*CPstatusForm.class: form
*CPstatusForm.name.source: public
*CPstatusForm.static: false
*CPstatusForm.name: CPstatusForm
*CPstatusForm.parent: panedWindow1
*CPstatusForm.x: 280
*CPstatusForm.y: 20
*CPstatusForm.backgroundPixmap: "/usr/include/X11/bitmaps/black6"
*CPstatusForm.allowResize: "true"
*CPstatusForm.height.source: public
*CPstatusForm.height: 215
*CPstatusForm.rubberPositioning: "true"
*CPstatusForm.labelFontList.source: public
*CPstatusForm.labelFontList: "helvb12"
*CPstatusForm.buttonFontList.source: public
*CPstatusForm.buttonFontList: "helvb12"

*CPstatusForm_label.class: label
*CPstatusForm_label.static: true
*CPstatusForm_label.name: CPstatusForm_label
*CPstatusForm_label.parent: CPstatusForm
*CPstatusForm_label.x: 0
*CPstatusForm_label.y: 10
*CPstatusForm_label.width: 974
*CPstatusForm_label.height: 26
*CPstatusForm_label.leftAttachment: "attach_form"
*CPstatusForm_label.rightAttachment: "attach_form"
*CPstatusForm_label.topAttachment: "attach_form"
*CPstatusForm_label.backgroundPixmap: "/usr/include/X11/bitmaps/black6"
*CPstatusForm_label.fontList: "lucibis18"
*CPstatusForm_label.topOffset: 6
*CPstatusForm_label.labelString: "Subsystems"

*CPstatusRC.class: rowColumn
*CPstatusRC.name.source: public
*CPstatusRC.static: false
*CPstatusRC.name: CPstatusRC
*CPstatusRC.parent: CPstatusForm
*CPstatusRC.x: 680
*CPstatusRC.y: 20
*CPstatusRC.bottomOffset: 2
*CPstatusRC.bottomAttachment: "attach_form"
*CPstatusRC.topOffset: 6
*CPstatusRC.topAttachment: "attach_widget"
*CPstatusRC.packing: "pack_column"
*CPstatusRC.rightAttachment: "attach_form"
*CPstatusRC.orientation: "horizontal"
*CPstatusRC.adjustLast: "false"
*CPstatusRC.backgroundPixmap: "/usr/include/X11/bitmaps/black6"
*CPstatusRC.entryAlignment: "alignment_center"
*CPstatusRC.leftAttachment: "attach_form"
*CPstatusRC.leftOffset: 50
*CPstatusRC.topWidget: "CPstatusForm_label"
*CPstatusRC.isAligned: "false"

*CPstatusBlank_label.class: label
*CPstatusBlank_label.name.source: public
*CPstatusBlank_label.static: false
*CPstatusBlank_label.name: CPstatusBlank_label
*CPstatusBlank_label.parent: CPstatusRC
*CPstatusBlank_label.x: 60
*CPstatusBlank_label.y: 10
*CPstatusBlank_label.width: 110
*CPstatusBlank_label.height: 30
*CPstatusBlank_label.labelString: ""
*CPstatusBlank_label.alignment: "alignment_center"
*CPstatusBlank_label.fontList.source: public
*CPstatusBlank_label.fontList: "helvb12"

*CPstatusNotRunning_label.class: label
*CPstatusNotRunning_label.name.source: public
*CPstatusNotRunning_label.static: false
*CPstatusNotRunning_label.name: CPstatusNotRunning_label
*CPstatusNotRunning_label.parent: CPstatusRC
*CPstatusNotRunning_label.x: 13
*CPstatusNotRunning_label.y: 13
*CPstatusNotRunning_label.width: 110
*CPstatusNotRunning_label.height: 30
*CPstatusNotRunning_label.labelString: "Not Running"
*CPstatusNotRunning_label.alignment: "alignment_center"
*CPstatusNotRunning_label.fontList.source: public
*CPstatusNotRunning_label.fontList: "helvb12"

*CPstatusStarted_label.class: label
*CPstatusStarted_label.name.source: public
*CPstatusStarted_label.static: false
*CPstatusStarted_label.name: CPstatusStarted_label
*CPstatusStarted_label.parent: CPstatusRC
*CPstatusStarted_label.x: 56
*CPstatusStarted_label.y: 13
*CPstatusStarted_label.width: 110
*CPstatusStarted_label.height: 30
*CPstatusStarted_label.labelString: "Started"
*CPstatusStarted_label.alignment: "alignment_center"
*CPstatusStarted_label.fontList.source: public
*CPstatusStarted_label.fontList: "helvb12"

*CPstatusReady_label.class: label
*CPstatusReady_label.name.source: public
*CPstatusReady_label.static: false
*CPstatusReady_label.name: CPstatusReady_label
*CPstatusReady_label.parent: CPstatusRC
*CPstatusReady_label.x: 13
*CPstatusReady_label.y: 13
*CPstatusReady_label.width: 110
*CPstatusReady_label.height: 30
*CPstatusReady_label.labelString: "Ready"
*CPstatusReady_label.alignment: "alignment_center"
*CPstatusReady_label.fontList.source: public
*CPstatusReady_label.fontList: "helvb12"

*CPstatusRunning_label.class: label
*CPstatusRunning_label.name.source: public
*CPstatusRunning_label.static: false
*CPstatusRunning_label.name: CPstatusRunning_label
*CPstatusRunning_label.parent: CPstatusRC
*CPstatusRunning_label.x: 99
*CPstatusRunning_label.y: 13
*CPstatusRunning_label.width: 110
*CPstatusRunning_label.height: 30
*CPstatusRunning_label.labelString: "Running"
*CPstatusRunning_label.alignment: "alignment_center"
*CPstatusRunning_label.fontList.source: public
*CPstatusRunning_label.fontList: "helvb12"

*CPstatusQC_label.class: label
*CPstatusQC_label.name.source: public
*CPstatusQC_label.static: false
*CPstatusQC_label.name: CPstatusQC_label
*CPstatusQC_label.parent: CPstatusRC
*CPstatusQC_label.x: 142
*CPstatusQC_label.y: 13
*CPstatusQC_label.width: 110
*CPstatusQC_label.height: 30
*CPstatusQC_label.labelString: "Q/C"
*CPstatusQC_label.alignment: "alignment_center"
*CPstatusQC_label.fontList.source: public
*CPstatusQC_label.fontList: "helvb12"

*CPstatusHold_label.class: label
*CPstatusHold_label.name.source: public
*CPstatusHold_label.static: false
*CPstatusHold_label.name: CPstatusHold_label
*CPstatusHold_label.parent: CPstatusRC
*CPstatusHold_label.x: 13
*CPstatusHold_label.y: 13
*CPstatusHold_label.width: 110
*CPstatusHold_label.height: 30
*CPstatusHold_label.labelString: "Hold"
*CPstatusHold_label.alignment: "alignment_center"
*CPstatusHold_label.fontList.source: public
*CPstatusHold_label.fontList: "helvb12"

*CPstatusError_label.class: label
*CPstatusError_label.name.source: public
*CPstatusError_label.static: false
*CPstatusError_label.name: CPstatusError_label
*CPstatusError_label.parent: CPstatusRC
*CPstatusError_label.x: 56
*CPstatusError_label.y: 13
*CPstatusError_label.width: 110
*CPstatusError_label.height: 30
*CPstatusError_label.labelString: "Error"
*CPstatusError_label.alignment: "alignment_center"
*CPstatusError_label.fontList.source: public
*CPstatusError_label.fontList: "helvb12"

*CPexternForm.class: form
*CPexternForm.name.source: public
*CPexternForm.static: false
*CPexternForm.name: CPexternForm
*CPexternForm.parent: panedWindow1
*CPexternForm.x: 10
*CPexternForm.y: 13
*CPexternForm.backgroundPixmap: "/usr/include/X11/bitmaps/black6"
*CPexternForm.height.source: public
*CPexternForm.height: 155
*CPexternForm.rubberPositioning: "true"
*CPexternForm.labelFontList.source: public
*CPexternForm.labelFontList: "helvb12"
*CPexternForm.buttonFontList.source: public
*CPexternForm.buttonFontList: "helvb12"
*CPexternForm.positionIndex: XmLAST_POSITION

*CPexternForm_label.class: label
*CPexternForm_label.static: true
*CPexternForm_label.name: CPexternForm_label
*CPexternForm_label.parent: CPexternForm
*CPexternForm_label.x: 22
*CPexternForm_label.y: 87
*CPexternForm_label.width: 20
*CPexternForm_label.height: 19
*CPexternForm_label.leftAttachment: "attach_form"
*CPexternForm_label.rightAttachment: "attach_form"
*CPexternForm_label.topAttachment: "attach_form"
*CPexternForm_label.fontList: "lucibis18"
*CPexternForm_label.labelString: "External Interfaces"
*CPexternForm_label.backgroundPixmap: "/usr/include/X11/bitmaps/black6"
*CPexternForm_label.topOffset: 10

*CPexternRC.class: rowColumn
*CPexternRC.name.source: public
*CPexternRC.static: false
*CPexternRC.name: CPexternRC
*CPexternRC.parent: CPexternForm
*CPexternRC.x: 680
*CPexternRC.y: 20
*CPexternRC.bottomOffset: 2
*CPexternRC.bottomAttachment: "attach_form"
*CPexternRC.topOffset: 10
*CPexternRC.topAttachment: "attach_widget"
*CPexternRC.packing: "pack_column"
*CPexternRC.orientation: "horizontal"
*CPexternRC.adjustLast: "false"
*CPexternRC.backgroundPixmap: "/usr/include/X11/bitmaps/black6"
*CPexternRC.entryAlignment: "alignment_center"
*CPexternRC.leftAttachment: "attach_form"
*CPexternRC.leftOffset: 50
*CPexternRC.topWidget: "CPexternForm_label"
*CPexternRC.isAligned: "false"
*CPexternRC.rightAttachment: "attach_form"

*CPexternBlank_label.class: label
*CPexternBlank_label.name.source: public
*CPexternBlank_label.static: false
*CPexternBlank_label.name: CPexternBlank_label
*CPexternBlank_label.parent: CPexternRC
*CPexternBlank_label.x: 60
*CPexternBlank_label.y: 10
*CPexternBlank_label.width: 110
*CPexternBlank_label.height: 30
*CPexternBlank_label.labelString: ""
*CPexternBlank_label.alignment: "alignment_center"
*CPexternBlank_label.fontList.source: public
*CPexternBlank_label.fontList: "helvb12"

*CPexternNotRunning_label.class: label
*CPexternNotRunning_label.name.source: public
*CPexternNotRunning_label.static: false
*CPexternNotRunning_label.name: CPexternNotRunning_label
*CPexternNotRunning_label.parent: CPexternRC
*CPexternNotRunning_label.x: 13
*CPexternNotRunning_label.y: 13
*CPexternNotRunning_label.width: 110
*CPexternNotRunning_label.height: 30
*CPexternNotRunning_label.labelString: "Not Running"
*CPexternNotRunning_label.alignment: "alignment_center"
*CPexternNotRunning_label.fontList.source: public
*CPexternNotRunning_label.fontList: "helvb12"

*CPexternStarted_label.class: label
*CPexternStarted_label.name.source: public
*CPexternStarted_label.static: false
*CPexternStarted_label.name: CPexternStarted_label
*CPexternStarted_label.parent: CPexternRC
*CPexternStarted_label.x: 56
*CPexternStarted_label.y: 13
*CPexternStarted_label.width: 110
*CPexternStarted_label.height: 30
*CPexternStarted_label.labelString: "Started"
*CPexternStarted_label.alignment: "alignment_center"
*CPexternStarted_label.fontList.source: public
*CPexternStarted_label.fontList: "helvb12"

*CPexternReady_label.class: label
*CPexternReady_label.name.source: public
*CPexternReady_label.static: false
*CPexternReady_label.name: CPexternReady_label
*CPexternReady_label.parent: CPexternRC
*CPexternReady_label.x: 13
*CPexternReady_label.y: 13
*CPexternReady_label.width: 110
*CPexternReady_label.height: 30
*CPexternReady_label.labelString: "Ready"
*CPexternReady_label.alignment: "alignment_center"
*CPexternReady_label.fontList.source: public
*CPexternReady_label.fontList: "helvb12"

*CPexternRunning_label.class: label
*CPexternRunning_label.name.source: public
*CPexternRunning_label.static: false
*CPexternRunning_label.name: CPexternRunning_label
*CPexternRunning_label.parent: CPexternRC
*CPexternRunning_label.x: 99
*CPexternRunning_label.y: 13
*CPexternRunning_label.width: 110
*CPexternRunning_label.height: 30
*CPexternRunning_label.labelString: "Running"
*CPexternRunning_label.alignment: "alignment_center"
*CPexternRunning_label.fontList.source: public
*CPexternRunning_label.fontList: "helvb12"

*CPexternQC_label.class: label
*CPexternQC_label.name.source: public
*CPexternQC_label.static: false
*CPexternQC_label.name: CPexternQC_label
*CPexternQC_label.parent: CPexternRC
*CPexternQC_label.x: 142
*CPexternQC_label.y: 13
*CPexternQC_label.width: 110
*CPexternQC_label.height: 30
*CPexternQC_label.labelString: "Q/C"
*CPexternQC_label.alignment: "alignment_center"
*CPexternQC_label.fontList.source: public
*CPexternQC_label.fontList: "helvb12"
*CPexternQC_label.sensitive: "false"

*CPexternHold_label.class: label
*CPexternHold_label.name.source: public
*CPexternHold_label.static: false
*CPexternHold_label.name: CPexternHold_label
*CPexternHold_label.parent: CPexternRC
*CPexternHold_label.x: 13
*CPexternHold_label.y: 13
*CPexternHold_label.width: 110
*CPexternHold_label.height: 30
*CPexternHold_label.labelString: "Hold"
*CPexternHold_label.alignment: "alignment_center"
*CPexternHold_label.fontList.source: public
*CPexternHold_label.fontList: "helvb12"

*CPexternError_label.class: label
*CPexternError_label.name.source: public
*CPexternError_label.static: false
*CPexternError_label.name: CPexternError_label
*CPexternError_label.parent: CPexternRC
*CPexternError_label.x: 56
*CPexternError_label.y: 13
*CPexternError_label.width: 110
*CPexternError_label.height: 30
*CPexternError_label.labelString: "Error"
*CPexternError_label.alignment: "alignment_center"
*CPexternError_label.fontList.source: public
*CPexternError_label.fontList: "helvb12"

*CPoptionsForm.class: form
*CPoptionsForm.name.source: public
*CPoptionsForm.static: false
*CPoptionsForm.name: CPoptionsForm
*CPoptionsForm.parent: panedWindow1
*CPoptionsForm.x: 10
*CPoptionsForm.y: 419
*CPoptionsForm.height.source: public
*CPoptionsForm.height: 80
*CPoptionsForm.labelFontList.source: public
*CPoptionsForm.labelFontList: "helvb12"
*CPoptionsForm.buttonFontList.source: public
*CPoptionsForm.buttonFontList: "helvb12"
*CPoptionsForm.positionIndex: 0

*CPqueueSize_frame.class: frame
*CPqueueSize_frame.static: true
*CPqueueSize_frame.name: CPqueueSize_frame
*CPqueueSize_frame.parent: CPoptionsForm
*CPqueueSize_frame.x: 5
*CPqueueSize_frame.y: -1
*CPqueueSize_frame.bottomAttachment: "attach_form"
*CPqueueSize_frame.topAttachment: "attach_form"
*CPqueueSize_frame.rightAttachment: "attach_position"
*CPqueueSize_frame.rightPosition: 45
*CPqueueSize_frame.leftAttachment: "attach_form"

*CPqueueSize_form.class: form
*CPqueueSize_form.static: true
*CPqueueSize_form.name: CPqueueSize_form
*CPqueueSize_form.parent: CPqueueSize_frame
*CPqueueSize_form.resizePolicy: "resize_none"
*CPqueueSize_form.x: 7
*CPqueueSize_form.y: 8
*CPqueueSize_form.backgroundPixmap: "/usr/include/X11/bitmaps/2x2"

*queueInfo_rc2.class: rowColumn
*queueInfo_rc2.static: true
*queueInfo_rc2.name: queueInfo_rc2
*queueInfo_rc2.parent: CPqueueSize_form
*queueInfo_rc2.x: 1
*queueInfo_rc2.y: 3
*queueInfo_rc2.width: 453
*queueInfo_rc2.orientation: "horizontal"
*queueInfo_rc2.borderWidth: 0
*queueInfo_rc2.rightAttachment: "attach_form"
*queueInfo_rc2.leftAttachment: "attach_form"
*queueInfo_rc2.bottomAttachment: "attach_form"
*queueInfo_rc2.topAttachment: "attach_form"
*queueInfo_rc2.numColumns: 2
*queueInfo_rc2.packing: "pack_column"

*maxQueueSize_rc2.class: rowColumn
*maxQueueSize_rc2.static: true
*maxQueueSize_rc2.name: maxQueueSize_rc2
*maxQueueSize_rc2.parent: queueInfo_rc2
*maxQueueSize_rc2.x: -11
*maxQueueSize_rc2.y: 2
*maxQueueSize_rc2.height: 37
*maxQueueSize_rc2.orientation: "horizontal"
*maxQueueSize_rc2.adjustLast: "false"

*maxQueueSize_label2.class: label
*maxQueueSize_label2.static: true
*maxQueueSize_label2.name: maxQueueSize_label2
*maxQueueSize_label2.parent: maxQueueSize_rc2
*maxQueueSize_label2.x: 20
*maxQueueSize_label2.y: 5
*maxQueueSize_label2.height: 30
*maxQueueSize_label2.labelString: "Max Queue Size:"
*maxQueueSize_label2.alignment: "alignment_end"

*maxQueueSize_tf.class: textField
*maxQueueSize_tf.name.source: public
*maxQueueSize_tf.static: false
*maxQueueSize_tf.name: maxQueueSize_tf
*maxQueueSize_tf.parent: maxQueueSize_rc2
*maxQueueSize_tf.x: 88
*maxQueueSize_tf.y: 3
*maxQueueSize_tf.width: 119
*maxQueueSize_tf.height: 31
*maxQueueSize_tf.columns: 10
*maxQueueSize_tf.activateCallback.source: public
*maxQueueSize_tf.activateCallback: check_number
*maxQueueSize_tf.modifyVerifyCallback.source: public
*maxQueueSize_tf.modifyVerifyCallback: check_number

*maxQueueSizeReset_pb.class: pushButton
*maxQueueSizeReset_pb.name.source: public
*maxQueueSizeReset_pb.static: false
*maxQueueSizeReset_pb.name: maxQueueSizeReset_pb
*maxQueueSizeReset_pb.parent: maxQueueSize_rc2
*maxQueueSizeReset_pb.x: 208
*maxQueueSizeReset_pb.y: 15
*maxQueueSizeReset_pb.width: 27
*maxQueueSizeReset_pb.height: 13
*maxQueueSizeReset_pb.labelString: "Reset"
*maxQueueSizeReset_pb.activateCallback: resetQueueDisplaySize(maxQueueSize_tf);
*maxQueueSizeReset_pb.sensitive: "false"

*currentQueueSize_rc2.class: rowColumn
*currentQueueSize_rc2.static: true
*currentQueueSize_rc2.name: currentQueueSize_rc2
*currentQueueSize_rc2.parent: queueInfo_rc2
*currentQueueSize_rc2.x: -11
*currentQueueSize_rc2.y: 42
*currentQueueSize_rc2.height: 37
*currentQueueSize_rc2.orientation: "horizontal"
*currentQueueSize_rc2.packing: "pack_column"

*currentQueueSize_label2.class: label
*currentQueueSize_label2.static: true
*currentQueueSize_label2.name: currentQueueSize_label2
*currentQueueSize_label2.parent: currentQueueSize_rc2
*currentQueueSize_label2.x: 20
*currentQueueSize_label2.y: 5
*currentQueueSize_label2.height: 30
*currentQueueSize_label2.labelString: "Current # Jobs:"
*currentQueueSize_label2.alignment: "alignment_end"

*currentQueueSizeValue_label.class: label
*currentQueueSizeValue_label.name.source: public
*currentQueueSizeValue_label.static: false
*currentQueueSizeValue_label.name: currentQueueSizeValue_label
*currentQueueSizeValue_label.parent: currentQueueSize_rc2
*currentQueueSizeValue_label.x: 13
*currentQueueSizeValue_label.y: 13
*currentQueueSizeValue_label.height: 30
*currentQueueSizeValue_label.labelString: "0"
*currentQueueSizeValue_label.alignment: "alignment_beginning"

*PPSjobControl_frame.class: frame
*PPSjobControl_frame.static: true
*PPSjobControl_frame.name: PPSjobControl_frame
*PPSjobControl_frame.parent: CPoptionsForm
*PPSjobControl_frame.isCompound: "true"
*PPSjobControl_frame.compoundIcon: "frame.xpm"
*PPSjobControl_frame.compoundName: "frame_"
*PPSjobControl_frame.rightAttachment: "attach_form"
*PPSjobControl_frame.leftAttachment: "attach_position"
*PPSjobControl_frame.leftPosition: 45

*PPSjobControlRC_form.class: form
*PPSjobControlRC_form.static: true
*PPSjobControlRC_form.name: PPSjobControlRC_form
*PPSjobControlRC_form.parent: PPSjobControl_frame
*PPSjobControlRC_form.resizePolicy: "resize_none"
*PPSjobControlRC_form.x: 3
*PPSjobControlRC_form.y: 3
*PPSjobControlRC_form.width: 329
*PPSjobControlRC_form.height: 184

*PPSjobControlAction_label.class: label
*PPSjobControlAction_label.static: true
*PPSjobControlAction_label.name: PPSjobControlAction_label
*PPSjobControlAction_label.parent: PPSjobControlRC_form
*PPSjobControlAction_label.labelString: "Select an illuminated button to request a specific job type."
*PPSjobControlAction_label.alignment: "alignment_center"
*PPSjobControlAction_label.width: 336
*PPSjobControlAction_label.leftAttachment: "attach_form"
*PPSjobControlAction_label.rightAttachment: "attach_form"
*PPSjobControlAction_label.topOffset: 3
*PPSjobControlAction_label.fontList: "-adobe-helvetica-bold-o-normal--14-140-75-75-p-82-iso8859-1"

*ppsJobRequestFrameCts_pb.class: pushButton
*ppsJobRequestFrameCts_pb.static: true
*ppsJobRequestFrameCts_pb.name: ppsJobRequestFrameCts_pb
*ppsJobRequestFrameCts_pb.parent: PPSjobControlRC_form
*ppsJobRequestFrameCts_pb.labelString: "Frame\nContinuous Mode"
*ppsJobRequestFrameCts_pb.alignment: "alignment_center"
*ppsJobRequestFrameCts_pb.rightAttachment: "attach_form"
*ppsJobRequestFrameCts_pb.topAttachment: "attach_widget"
*ppsJobRequestFrameCts_pb.topWidget: "PPSjobControlAction_label"
*ppsJobRequestFrameCts_pb.leftAttachment: "attach_position"
*ppsJobRequestFrameCts_pb.leftPosition: 75
*ppsJobRequestFrameCts_pb.fontList.source: public
*ppsJobRequestFrameCts_pb.fontList: "-Adobe-Helvetica-Medium-R-Normal--14-100-100-100-P-76-ISO8859-1=FONTLIST_DEFAULT_TAG_STRING"
*ppsJobRequestFrameCts_pb.activateCallback: {\
extern int ppsLastRequested;\
\
ppsLastRequested = P_CTS_FRAME;\
requestPPSmsg("CONTINUOUS", "FRAME");\
}

*ppsJobRequestScanCts_pb.class: pushButton
*ppsJobRequestScanCts_pb.static: true
*ppsJobRequestScanCts_pb.name: ppsJobRequestScanCts_pb
*ppsJobRequestScanCts_pb.parent: PPSjobControlRC_form
*ppsJobRequestScanCts_pb.labelString: "Scan\nContinuous Mode"
*ppsJobRequestScanCts_pb.alignment: "alignment_center"
*ppsJobRequestScanCts_pb.topAttachment: "attach_widget"
*ppsJobRequestScanCts_pb.topWidget: "PPSjobControlAction_label"
*ppsJobRequestScanCts_pb.rightAttachment: "attach_position"
*ppsJobRequestScanCts_pb.rightPosition: 25
*ppsJobRequestScanCts_pb.leftAttachment: "attach_form"
*ppsJobRequestScanCts_pb.fontList.source: public
*ppsJobRequestScanCts_pb.fontList: "-Adobe-Helvetica-Medium-R-Normal--14-100-100-100-P-76-ISO8859-1=FONTLIST_DEFAULT_TAG_STRING"
*ppsJobRequestScanCts_pb.activateCallback: {\
extern int ppsLastRequested;\
\
ppsLastRequested = P_CTS_SCAN;\
requestPPSmsg("CONTINUOUS", "SCAN");\
}

*ppsJobRequestFrameSS_pb.class: pushButton
*ppsJobRequestFrameSS_pb.static: true
*ppsJobRequestFrameSS_pb.name: ppsJobRequestFrameSS_pb
*ppsJobRequestFrameSS_pb.parent: PPSjobControlRC_form
*ppsJobRequestFrameSS_pb.labelString: "Frame\nScanSar Mode"
*ppsJobRequestFrameSS_pb.alignment: "alignment_center"
*ppsJobRequestFrameSS_pb.rightAttachment: "attach_position"
*ppsJobRequestFrameSS_pb.topAttachment: "attach_widget"
*ppsJobRequestFrameSS_pb.topWidget: "PPSjobControlAction_label"
*ppsJobRequestFrameSS_pb.leftAttachment: "attach_position"
*ppsJobRequestFrameSS_pb.leftPosition: 50
*ppsJobRequestFrameSS_pb.rightPosition: 75
*ppsJobRequestFrameSS_pb.fontList.source: public
*ppsJobRequestFrameSS_pb.fontList: "-Adobe-Helvetica-Medium-R-Normal--14-100-100-100-P-76-ISO8859-1=FONTLIST_DEFAULT_TAG_STRING"
*ppsJobRequestFrameSS_pb.activateCallback: {\
extern int ppsLastRequested;\
\
ppsLastRequested = P_SS_FRAME;\
requestPPSmsg("SCANSAR", "FRAME");\
}

*ppsJobRequestScanSS_pb.class: pushButton
*ppsJobRequestScanSS_pb.static: true
*ppsJobRequestScanSS_pb.name: ppsJobRequestScanSS_pb
*ppsJobRequestScanSS_pb.parent: PPSjobControlRC_form
*ppsJobRequestScanSS_pb.labelString: "Scan\nScanSar Mode"
*ppsJobRequestScanSS_pb.alignment: "alignment_center"
*ppsJobRequestScanSS_pb.topAttachment: "attach_widget"
*ppsJobRequestScanSS_pb.rightAttachment: "attach_position"
*ppsJobRequestScanSS_pb.leftAttachment: "attach_position"
*ppsJobRequestScanSS_pb.leftPosition: 25
*ppsJobRequestScanSS_pb.rightPosition: 50
*ppsJobRequestScanSS_pb.topWidget: "PPSjobControlAction_label"
*ppsJobRequestScanSS_pb.fontList.source: public
*ppsJobRequestScanSS_pb.fontList: "-Adobe-Helvetica-Medium-R-Normal--14-100-100-100-P-76-ISO8859-1=FONTLIST_DEFAULT_TAG_STRING"
*ppsJobRequestScanSS_pb.activateCallback: {\
extern int ppsLastRequested;\
\
ppsLastRequested = P_SS_SCAN;\
requestPPSmsg("SCANSAR", "SCAN");\
}

