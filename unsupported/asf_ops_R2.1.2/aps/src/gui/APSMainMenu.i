! UIMX ascii 2.9 key: 7179                                                      

*APSMainMenu.class: applicationShell
*APSMainMenu.classinc:
*APSMainMenu.classspec:
*APSMainMenu.classmembers:
*APSMainMenu.classconstructor:
*APSMainMenu.classdestructor:
*APSMainMenu.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
U.S. Government Sponsorship acknowledged.\
#endif\
 \
/*==============================================================================\
Filename:\
 \
Description:\
 \
External Functions Defined:\
 \
File Scope Functions:\
 \
External Variables Defined:\
 \
File Scope Variables:\
 \
Notes:\
 \
==============================================================================*/\
#pragma ident   "@(#)APSMainMenu.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.APSMainMenu.i"\
\
#include <stdlib.h>\
\
#include "aps_defs.h"\
#include "gui_defs.h"\
#include "cb_apsmenu.h"\
#include "cb_rgsdntimes.h"\
#include "gui_utils.h"\
\
Widget cnomorb_form ;\
Widget cnomcov_form ;\
Widget cdtakeopps_form ;\
Widget apsfilegen_form ;\
Widget apsfileproc_form ;\
Widget DAR_manager ;\
Widget DTK_manager ;\
Widget DownTime_manager ;\
Widget AntennaDownTime_manager ;\
Widget PermStatus_viewer ;\
Widget PermStatusInterval_popup ;\
Widget con_roundup_form ;\
Widget apswoscompare_form ;\
Widget apsphaseselect_form ;\
Widget filebox ;\
Widget File_viewer ;\
char display_string[APS_GUI_DISPLAY_STR_SIZE] ;\

*APSMainMenu.ispecdecl:
*APSMainMenu.funcdecl: swidget create_APSMainMenu(swidget UxParent)
*APSMainMenu.funcname: create_APSMainMenu
*APSMainMenu.funcdef: "swidget", "<create_APSMainMenu>(%)"
*APSMainMenu.argdecl: swidget UxParent;
*APSMainMenu.arglist: UxParent
*APSMainMenu.arglist.UxParent: "swidget", "%UxParent%"
*APSMainMenu.icode:
*APSMainMenu.fcode: XtVaSetValues( gui_GetShellWidget( rtrn ),\
	XmNdeleteResponse, XmDO_NOTHING,\
	NULL ) ;\
\
return(rtrn);\

*APSMainMenu.auxdecl:
*APSMainMenu.name.source: public
*APSMainMenu.static: false
*APSMainMenu.name: APSMainMenu
*APSMainMenu.parent: NO_PARENT
*APSMainMenu.canHaveChildren: "true"
*APSMainMenu.compoundEditor: {\
extern swidget UxGUINoviceMePopup UXPROTO((swidget));\
UxGUINoviceMePopup(UxThisWidget);\
}
*APSMainMenu.compoundEditorName: "Menu Editor..."
*APSMainMenu.compoundIcon: "appliW.xpm"
*APSMainMenu.compoundName: "appl_Window"
*APSMainMenu.compoundResourceSet: "Background:applForm1.background;Foreground:applForm1.foreground;MenuBackground:applMenuBar1.background,applMenuBar1.propagate;MenuForeground:applMenuBar1.foreground,applMenuBar1.propagate;MenuFontList:applMenuBar1.fontList,applMenuBar1.propagate"
*APSMainMenu.background.lock: true
*APSMainMenu.isCompound: "true"
*APSMainMenu.isNovice: "true"
*APSMainMenu.isRegion: "true"
*APSMainMenu.isReorderable: "false"
*APSMainMenu.isReparentable: "false"
*APSMainMenu.x: 0
*APSMainMenu.y: -12
*APSMainMenu.title: "APS Main Menu"
*APSMainMenu.iconName: "APS Main Menu"
*APSMainMenu.createCallback.source: public
*APSMainMenu.createCallback: cb_create_aps_interfaces
*APSMainMenu.minHeight: 340
*APSMainMenu.minWidth: 435

*applMainWin1.class: mainWindow
*applMainWin1.static: true
*applMainWin1.name: applMainWin1
*applMainWin1.parent: APSMainMenu
*applMainWin1.canHaveChildren: "false"
*applMainWin1.dragRecursion: "up"
*applMainWin1.isCompound: "false"
*applMainWin1.isInCompound: "true"
*applMainWin1.isNovice: "true"
*applMainWin1.isDeletable: "false"
*applMainWin1.isRegion: "false"
*applMainWin1.isReorderable: "false"
*applMainWin1.isReparentable: "false"
*applMainWin1.isSelectable: "false"
*applMainWin1.resizeRecursion: "up"
*applMainWin1.unitType: "pixels"
*applMainWin1.showInBrowser: "false"
*applMainWin1.usePropEditor: "false"

*applMenuBar1.class: rowColumn
*applMenuBar1.static: true
*applMenuBar1.name: applMenuBar1
*applMenuBar1.parent: applMainWin1
*applMenuBar1.dragRecursion: "up"
*applMenuBar1.isCompound: "false"
*applMenuBar1.isInCompound: "true"
*applMenuBar1.isNovice: "true"
*applMenuBar1.isRegion: "false"
*applMenuBar1.isReorderable: "false"
*applMenuBar1.isReparentable: "false"
*applMenuBar1.isSelectable: "false"
*applMenuBar1.resizeRecursion: "up"
*applMenuBar1.rowColumnType: "menu_bar"
*applMenuBar1.showInBrowser: "false"
*applMenuBar1.usePropEditor: "false"
*applMenuBar1.menuAccelerator: "<KeyUp>F10"

*File_applMenuBar.class: rowColumn
*File_applMenuBar.static: true
*File_applMenuBar.name: File_applMenuBar
*File_applMenuBar.parent: applMenuBar1
*File_applMenuBar.isNovice: "true"
*File_applMenuBar.rowColumnType: "menu_pulldown"
*File_applMenuBar.showInBrowser: "true"
*File_applMenuBar.tearOffModel: "tear_off_enabled"

*FileGeneration_pb.class: pushButton
*FileGeneration_pb.static: true
*FileGeneration_pb.name: FileGeneration_pb
*FileGeneration_pb.parent: File_applMenuBar
*FileGeneration_pb.labelString: "APS File Generation"
*FileGeneration_pb.mnemonic: "G"
*FileGeneration_pb.activateCallback: {\
XtPopup(XtParent(apsfilegen_form), XtGrabNone) ;\
\
}

*FileProcessing_pb.class: pushButton
*FileProcessing_pb.static: true
*FileProcessing_pb.name: FileProcessing_pb
*FileProcessing_pb.parent: File_applMenuBar
*FileProcessing_pb.labelString: "APS File Processing"
*FileProcessing_pb.mnemonic: "P"
*FileProcessing_pb.activateCallback: {\
XtPopup(XtParent(apsfileproc_form), XtGrabNone) ;\
\
}

*separator2.class: separator
*separator2.static: true
*separator2.name: separator2
*separator2.parent: File_applMenuBar

*ExitAPS_pb.class: pushButton
*ExitAPS_pb.static: true
*ExitAPS_pb.name: ExitAPS_pb
*ExitAPS_pb.parent: File_applMenuBar
*ExitAPS_pb.isNovice: "true"
*ExitAPS_pb.labelString: "Exit APS"
*ExitAPS_pb.mnemonic: "x"
*ExitAPS_pb.showInBrowser: "true"
*ExitAPS_pb.activateCallback: {\
if (AskUser(UxWidget, "Do you really want to exit?", NO) == YES)\
    exit(APS_EXIT_OK) ;\
\
}

*Coverage_applMenuBar.class: rowColumn
*Coverage_applMenuBar.static: true
*Coverage_applMenuBar.name: Coverage_applMenuBar
*Coverage_applMenuBar.parent: applMenuBar1
*Coverage_applMenuBar.rowColumnType: "menu_pulldown"
*Coverage_applMenuBar.tearOffModel: "tear_off_enabled"

*CreateNominalOrbit_pb.class: pushButton
*CreateNominalOrbit_pb.static: true
*CreateNominalOrbit_pb.name: CreateNominalOrbit_pb
*CreateNominalOrbit_pb.parent: Coverage_applMenuBar
*CreateNominalOrbit_pb.labelString: "Create Nominal Orbit"
*CreateNominalOrbit_pb.mnemonic: "O"
*CreateNominalOrbit_pb.activateCallback: {\
XtPopup(XtParent(cnomorb_form), XtGrabNone) ;\
\
}

*CreateNominalCvrg_pb.class: pushButton
*CreateNominalCvrg_pb.static: true
*CreateNominalCvrg_pb.name: CreateNominalCvrg_pb
*CreateNominalCvrg_pb.parent: Coverage_applMenuBar
*CreateNominalCvrg_pb.labelString: "Create Nominal Coverage"
*CreateNominalCvrg_pb.mnemonic: "C"
*CreateNominalCvrg_pb.activateCallback: {\
XtPopup(XtParent(cnomcov_form), XtGrabNone) ;\
\
}

*Planning_applMenuBar.class: rowColumn
*Planning_applMenuBar.static: true
*Planning_applMenuBar.name: Planning_applMenuBar
*Planning_applMenuBar.parent: applMenuBar1
*Planning_applMenuBar.rowColumnType: "menu_pulldown"
*Planning_applMenuBar.tearOffModel: "tear_off_enabled"

*DARManager_pb.class: pushButton
*DARManager_pb.static: true
*DARManager_pb.name: DARManager_pb
*DARManager_pb.parent: Planning_applMenuBar
*DARManager_pb.labelString: "DAR Manager"
*DARManager_pb.mnemonic: "D"
*DARManager_pb.activateCallback: {\
XtPopup(XtParent(DAR_manager), XtGrabNone) ;\
\
}

*SiteCoverage_pb.class: pushButton
*SiteCoverage_pb.static: true
*SiteCoverage_pb.name: SiteCoverage_pb
*SiteCoverage_pb.parent: Planning_applMenuBar
*SiteCoverage_pb.labelString: "Site Coverage"
*SiteCoverage_pb.mnemonic: "S"
*SiteCoverage_pb.activateCallback: {\
XtPopup(XtParent(cdtakeopps_form), XtGrabNone) ;\
\
}

*Mapper_pb.class: pushButton
*Mapper_pb.static: true
*Mapper_pb.name: Mapper_pb
*Mapper_pb.parent: Planning_applMenuBar
*Mapper_pb.labelString: "Mapper"
*Mapper_pb.mnemonic: "M"
*Mapper_pb.activateCallback.source: public
*Mapper_pb.activateCallback: cb_start_mapper

*DTKManager_pb.class: pushButton
*DTKManager_pb.static: true
*DTKManager_pb.name: DTKManager_pb
*DTKManager_pb.parent: Planning_applMenuBar
*DTKManager_pb.labelString: "DL/DTK Manager"
*DTKManager_pb.mnemonic: "a"
*DTKManager_pb.activateCallback: {\
XtPopup(XtParent(DTK_manager), XtGrabNone) ;\
\
}

*Downtime_applMenuBar.class: rowColumn
*Downtime_applMenuBar.static: true
*Downtime_applMenuBar.name: Downtime_applMenuBar
*Downtime_applMenuBar.parent: applMenuBar1
*Downtime_applMenuBar.rowColumnType: "menu_pulldown"
*Downtime_applMenuBar.tearOffModel: "tear_off_enabled"

*RGSDowntimes_pb.class: pushButton
*RGSDowntimes_pb.static: true
*RGSDowntimes_pb.name: RGSDowntimes_pb
*RGSDowntimes_pb.parent: Downtime_applMenuBar
*RGSDowntimes_pb.labelString: "RGS Down Times"
*RGSDowntimes_pb.mnemonic: "R"
*RGSDowntimes_pb.activateCallback.source: public
*RGSDowntimes_pb.activateCallback: cb_popup_asfdntime_form

*AntennaDowntimes_pb.class: pushButton
*AntennaDowntimes_pb.static: true
*AntennaDowntimes_pb.name: AntennaDowntimes_pb
*AntennaDowntimes_pb.parent: Downtime_applMenuBar
*AntennaDowntimes_pb.labelString: "Antenna Down Times"
*AntennaDowntimes_pb.mnemonic: "A"
*AntennaDowntimes_pb.activateCallback: {\
    XtPopup(XtParent(AntennaDownTime_manager), XtGrabNone) ;\
}

*Tools_applMenuBar.class: rowColumn
*Tools_applMenuBar.static: true
*Tools_applMenuBar.name: Tools_applMenuBar
*Tools_applMenuBar.parent: applMenuBar1
*Tools_applMenuBar.rowColumnType: "menu_pulldown"
*Tools_applMenuBar.tearOffModel: "tear_off_enabled"

*MUPermissionStatus_pb.class: pushButton
*MUPermissionStatus_pb.static: true
*MUPermissionStatus_pb.name: MUPermissionStatus_pb
*MUPermissionStatus_pb.parent: Tools_applMenuBar
*MUPermissionStatus_pb.labelString: "MU Permission Status"
*MUPermissionStatus_pb.mnemonic: "M"
*MUPermissionStatus_pb.activateCallback: {\
XtPopup(XtParent( PermStatus_viewer ), XtGrabNone) ;\
\
}

*APSWOSCompare_pb.class: pushButton
*APSWOSCompare_pb.static: true
*APSWOSCompare_pb.name: APSWOSCompare_pb
*APSWOSCompare_pb.parent: Tools_applMenuBar
*APSWOSCompare_pb.labelString: "MWOS Comparison"
*APSWOSCompare_pb.mnemonic: "W"
*APSWOSCompare_pb.activateCallback: {\
XtPopup(XtParent( apswoscompare_form ), XtGrabNone) ;\
\
}

*pushButton_CON_ROUNDUP.class: pushButton
*pushButton_CON_ROUNDUP.static: true
*pushButton_CON_ROUNDUP.name: pushButton_CON_ROUNDUP
*pushButton_CON_ROUNDUP.parent: Tools_applMenuBar
*pushButton_CON_ROUNDUP.labelString: "DL/DTK CON Roundup"
*pushButton_CON_ROUNDUP.mnemonic: "C"
*pushButton_CON_ROUNDUP.activateCallback: {\
XtPopup( gui_GetShellWidget( con_roundup_form ), XtGrabNone );\
}

*APSPhaseSelection_pb.class: pushButton
*APSPhaseSelection_pb.static: true
*APSPhaseSelection_pb.name: APSPhaseSelection_pb
*APSPhaseSelection_pb.parent: Tools_applMenuBar
*APSPhaseSelection_pb.labelString: "REQQ Phase Selection"
*APSPhaseSelection_pb.activateCallback: {\
extern Widget apsphaseselect_form;\
XtPopup(XtParent(apsphaseselect_form), XtGrabNone) ;\
}

*file_pane1.class: cascadeButton
*file_pane1.static: true
*file_pane1.name: file_pane1
*file_pane1.parent: applMenuBar1
*file_pane1.dragRecursion: "up"
*file_pane1.isInCompound: "true"
*file_pane1.isNovice: "true"
*file_pane1.isRegion: "false"
*file_pane1.isSelectable: "false"
*file_pane1.labelString: "File "
*file_pane1.mnemonic: "F"
*file_pane1.resizeRecursion: "up"
*file_pane1.showInBrowser: "false"
*file_pane1.subMenuId: "File_applMenuBar"
*file_pane1.usePropEditor: "false"

*applMenuBar1_top_b1.class: cascadeButton
*applMenuBar1_top_b1.static: true
*applMenuBar1_top_b1.name: applMenuBar1_top_b1
*applMenuBar1_top_b1.parent: applMenuBar1
*applMenuBar1_top_b1.dragRecursion: "up"
*applMenuBar1_top_b1.isInCompound: "true"
*applMenuBar1_top_b1.isNovice: "true"
*applMenuBar1_top_b1.isRegion: "false"
*applMenuBar1_top_b1.isSelectable: "false"
*applMenuBar1_top_b1.resizeRecursion: "up"
*applMenuBar1_top_b1.showInBrowser: "false"
*applMenuBar1_top_b1.usePropEditor: "false"
*applMenuBar1_top_b1.labelString: "Coverage "
*applMenuBar1_top_b1.mnemonic: "C"
*applMenuBar1_top_b1.subMenuId: "Coverage_applMenuBar"

*applMenuBar1_top_b2.class: cascadeButton
*applMenuBar1_top_b2.static: true
*applMenuBar1_top_b2.name: applMenuBar1_top_b2
*applMenuBar1_top_b2.parent: applMenuBar1
*applMenuBar1_top_b2.dragRecursion: "up"
*applMenuBar1_top_b2.isInCompound: "true"
*applMenuBar1_top_b2.isNovice: "true"
*applMenuBar1_top_b2.isRegion: "false"
*applMenuBar1_top_b2.isSelectable: "false"
*applMenuBar1_top_b2.resizeRecursion: "up"
*applMenuBar1_top_b2.showInBrowser: "false"
*applMenuBar1_top_b2.usePropEditor: "false"
*applMenuBar1_top_b2.labelString: "Planning "
*applMenuBar1_top_b2.mnemonic: "P"
*applMenuBar1_top_b2.subMenuId: "Planning_applMenuBar"

*applMenuBar1_top_b3.class: cascadeButton
*applMenuBar1_top_b3.static: true
*applMenuBar1_top_b3.name: applMenuBar1_top_b3
*applMenuBar1_top_b3.parent: applMenuBar1
*applMenuBar1_top_b3.dragRecursion: "up"
*applMenuBar1_top_b3.isInCompound: "true"
*applMenuBar1_top_b3.isNovice: "true"
*applMenuBar1_top_b3.isRegion: "false"
*applMenuBar1_top_b3.isSelectable: "false"
*applMenuBar1_top_b3.resizeRecursion: "up"
*applMenuBar1_top_b3.showInBrowser: "false"
*applMenuBar1_top_b3.usePropEditor: "false"
*applMenuBar1_top_b3.labelString: "Downtime "
*applMenuBar1_top_b3.mnemonic: "D"
*applMenuBar1_top_b3.subMenuId: "Downtime_applMenuBar"

*applMenuBar1_top_b4.class: cascadeButton
*applMenuBar1_top_b4.static: true
*applMenuBar1_top_b4.name: applMenuBar1_top_b4
*applMenuBar1_top_b4.parent: applMenuBar1
*applMenuBar1_top_b4.dragRecursion: "up"
*applMenuBar1_top_b4.isInCompound: "true"
*applMenuBar1_top_b4.isNovice: "true"
*applMenuBar1_top_b4.isRegion: "false"
*applMenuBar1_top_b4.isSelectable: "false"
*applMenuBar1_top_b4.resizeRecursion: "up"
*applMenuBar1_top_b4.showInBrowser: "false"
*applMenuBar1_top_b4.usePropEditor: "false"
*applMenuBar1_top_b4.labelString: "Tools "
*applMenuBar1_top_b4.mnemonic: "T"
*applMenuBar1_top_b4.subMenuId: "Tools_applMenuBar"

*applForm1.class: form
*applForm1.static: true
*applForm1.name: applForm1
*applForm1.parent: applMainWin1
*applForm1.autoUnmanage: "false"
*applForm1.dragRecursion: "up"
*applForm1.isCompound: "false"
*applForm1.isInCompound: "true"
*applForm1.isDeletable: "false"
*applForm1.isNovice: "true"
*applForm1.isRegion: "false"
*applForm1.isReorderable: "false"
*applForm1.isReparentable: "false"
*applForm1.isSelectable: "false"
*applForm1.resizePolicy: "resize_none"
*applForm1.resizeRecursion: "up"
*applForm1.showInBrowser: "false"
*applForm1.usePropEditor: "false"

*Pixmap_applWorkArea.class: label
*Pixmap_applWorkArea.name.source: public
*Pixmap_applWorkArea.static: false
*Pixmap_applWorkArea.name: Pixmap_applWorkArea
*Pixmap_applWorkArea.parent: applForm1
*Pixmap_applWorkArea.isCompound: "true"
*Pixmap_applWorkArea.compoundIcon: "label.xpm"
*Pixmap_applWorkArea.compoundName: "label_"
*Pixmap_applWorkArea.x: 10
*Pixmap_applWorkArea.y: 13
*Pixmap_applWorkArea.labelType: "pixmap"
*Pixmap_applWorkArea.createCallback.source: public
*Pixmap_applWorkArea.createCallback: cb_get_pixmap

