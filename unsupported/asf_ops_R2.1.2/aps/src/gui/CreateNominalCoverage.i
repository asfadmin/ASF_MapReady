! UIMX ascii 2.9 key: 9032                                                      

*CreateNominalCoverage.class: form
*CreateNominalCoverage.classinc:
*CreateNominalCoverage.classspec:
*CreateNominalCoverage.classmembers:
*CreateNominalCoverage.classconstructor:
*CreateNominalCoverage.classdestructor:
*CreateNominalCoverage.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.\
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
#pragma ident   "@(#)CreateNominalCoverage.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.CreateNominalCoverage.i"\
\
#include <stdlib.h>\
\
#include "cb_cnomcov.h"\
#include "cb_datetime.h"\
#include "satmenus.h"\
\
extern Widget cnomcov_form ;\
extern Widget datetime_form ;\

*CreateNominalCoverage.ispecdecl:
*CreateNominalCoverage.funcdecl: swidget create_CreateNominalCoverage(swidget UxParent)
*CreateNominalCoverage.funcname: create_CreateNominalCoverage
*CreateNominalCoverage.funcdef: "swidget", "<create_CreateNominalCoverage>(%)"
*CreateNominalCoverage.argdecl: swidget UxParent;
*CreateNominalCoverage.arglist: UxParent
*CreateNominalCoverage.arglist.UxParent: "swidget", "%UxParent%"
*CreateNominalCoverage.icode: OPTION_MENU_WIDGETS *sensor_menu ;\
PERIOD_WIDGETS *ASFperiod ;\
\
Widget selected_sat ;\
char *sat_name ;\
char *sat_code ;
*CreateNominalCoverage.fcode: sensor_menu =\
	(OPTION_MENU_WIDGETS *) malloc(sizeof(OPTION_MENU_WIDGETS)) ;\
ASFperiod = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
ASFperiod->start = (Widget) T_PHASE_START ;\
ASFperiod->stop = (Widget) T_PHASE_END ;\
\
sensor_menu->optionmenu = (Widget) optionMenu_ncov_sensor ;\
sensor_menu->submenu = (Widget) subMenu_ncov_sensor ;\
\
/* set the coverage type based on which sat is selected */\
XtVaGetValues( subMenu_ncov_sat,\
	XmNmenuHistory, &selected_sat,\
	NULL ) ;\
sat_name = XtName( selected_sat ) ;\
sat_code = get_satellite_code( sat_name ) ;\
/* if can't set coverage type, leave it as is */\
if (sat_code != NULL)\
	(void) set_coverage_type( sat_code ) ;\
\
XtUnmanageChild(toggleButton_stoptime) ;\
\
#ifndef DEBUG\
/* not DEBUG: don't create a coverage file (see the cb_*.c file also) */\
XtUnmanageChild( LBL_coverage_filename ) ;\
XtUnmanageChild( TF_coverage_filename ) ;\
#endif\
 \
/* \
-- add the callback for the refresh button here\
-- this ensures the ScrolledList widget has been \
-- created and can be properly passed as client\
-- data\
*/\
XtAddCallback( pushButton_refresh, XmNactivateCallback,\
        (XtCallbackProc) cb_show_coverage_relations,\
        (XtPointer) scrolledList_ephm );\
 \
XtAddCallback(subMenu_ncov_sat, XmNentryCallback,\
	(XtCallbackProc) cb_set_cvrg_allowed_sensor_menus,\
	(XtPointer) sensor_menu) ;\
\
XtAddCallback(TF_total_days, XmNactivateCallback,\
	(XtCallbackProc) cb_adjust_ASF_datetimes,\
	(XtPointer) ASFperiod) ;\
\
/* add cb for popup */\
XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
	cb_show_coverage_relations, (XtPointer *) scrolledList_ephm) ;\
\
return(rtrn);
*CreateNominalCoverage.auxdecl:
*CreateNominalCoverage.static: true
*CreateNominalCoverage.name: CreateNominalCoverage
*CreateNominalCoverage.parent: NO_PARENT
*CreateNominalCoverage.parentExpression: UxParent
*CreateNominalCoverage.defaultShell: topLevelShell
*CreateNominalCoverage.width: 631
*CreateNominalCoverage.height: 709
*CreateNominalCoverage.resizePolicy: "resize_any"
*CreateNominalCoverage.isCompound: "true"
*CreateNominalCoverage.compoundIcon: "form.xpm"
*CreateNominalCoverage.compoundName: "form_"
*CreateNominalCoverage.x: 309
*CreateNominalCoverage.y: 90
*CreateNominalCoverage.unitType: "pixels"
*CreateNominalCoverage.noResize: "true"
*CreateNominalCoverage.dialogTitle: "APS:CREATE NOMINAL COVERAGE"

*label29.class: label
*label29.static: true
*label29.name: label29
*label29.parent: CreateNominalCoverage
*label29.isCompound: "true"
*label29.compoundIcon: "label.xpm"
*label29.compoundName: "label_"
*label29.x: 10
*label29.y: 10
*label29.width: 600
*label29.height: 45
*label29.labelString: "CREATE  NOMINAL  COVERAGE"
*label29.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*label30.class: label
*label30.static: true
*label30.name: label30
*label30.parent: CreateNominalCoverage
*label30.isCompound: "true"
*label30.compoundIcon: "label.xpm"
*label30.compoundName: "label_"
*label30.x: 65
*label30.y: 70
*label30.width: 520
*label30.height: 20
*label30.labelString: "EPHEMERIS          SAT     PHASE      START TIME       START REV"
*label30.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label30.alignment: "alignment_beginning"

*label39.class: label
*label39.static: true
*label39.name: label39
*label39.parent: CreateNominalCoverage
*label39.isCompound: "true"
*label39.compoundIcon: "label.xpm"
*label39.compoundName: "label_"
*label39.x: 320
*label39.y: 359
*label39.width: 70
*label39.height: 30
*label39.labelString: "TOTAL DAYS:"
*label39.alignment: "alignment_end"

*label41.class: label
*label41.static: true
*label41.name: label41
*label41.parent: CreateNominalCoverage
*label41.isCompound: "true"
*label41.compoundIcon: "label.xpm"
*label41.compoundName: "label_"
*label41.x: 395
*label41.y: 225
*label41.width: 125
*label41.height: 20
*label41.labelString: "COVERAGE PERIOD"
*label41.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"

*TF_ephemeris_file.class: textField
*TF_ephemeris_file.static: true
*TF_ephemeris_file.name: TF_ephemeris_file
*TF_ephemeris_file.parent: CreateNominalCoverage
*TF_ephemeris_file.isCompound: "true"
*TF_ephemeris_file.compoundIcon: "textfield.xpm"
*TF_ephemeris_file.compoundName: "text_Field"
*TF_ephemeris_file.x: 95
*TF_ephemeris_file.y: 258
*TF_ephemeris_file.height: 32
*TF_ephemeris_file.columns: 21
*TF_ephemeris_file.cursorPositionVisible: "false"
*TF_ephemeris_file.editable: "false"
*TF_ephemeris_file.sensitive: "false"
*TF_ephemeris_file.resizeWidth: "false"
*TF_ephemeris_file.text: ""
*TF_ephemeris_file.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_ephemeris_file.width: 170
*TF_ephemeris_file.traversalOn: "false"

*label40.class: label
*label40.static: true
*label40.name: label40
*label40.parent: CreateNominalCoverage
*label40.isCompound: "true"
*label40.compoundIcon: "label.xpm"
*label40.compoundName: "label_"
*label40.x: 21
*label40.y: 259
*label40.width: 70
*label40.height: 30
*label40.labelString: "SELECTED \nEPHEMERIS:"
*label40.alignment: "alignment_end"

*separator3.class: separator
*separator3.static: true
*separator3.name: separator3
*separator3.parent: CreateNominalCoverage
*separator3.width: 633
*separator3.height: 10
*separator3.isCompound: "true"
*separator3.compoundIcon: "sep.xpm"
*separator3.compoundName: "separator_"
*separator3.x: 0
*separator3.y: 210

*separator4.class: separator
*separator4.static: true
*separator4.name: separator4
*separator4.parent: CreateNominalCoverage
*separator4.width: 633
*separator4.height: 14
*separator4.isCompound: "true"
*separator4.compoundIcon: "sep.xpm"
*separator4.compoundName: "separator_"
*separator4.x: 0
*separator4.y: 493

*label43.class: label
*label43.static: true
*label43.name: label43
*label43.parent: CreateNominalCoverage
*label43.isCompound: "true"
*label43.compoundIcon: "label.xpm"
*label43.compoundName: "label_"
*label43.x: 94
*label43.y: 225
*label43.width: 165
*label43.height: 20
*label43.labelString: "COVERAGE INFORMATION"
*label43.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"

*toggleButton_stoptime.class: toggleButton
*toggleButton_stoptime.static: true
*toggleButton_stoptime.name: toggleButton_stoptime
*toggleButton_stoptime.parent: CreateNominalCoverage
*toggleButton_stoptime.isCompound: "true"
*toggleButton_stoptime.compoundIcon: "toggle.xpm"
*toggleButton_stoptime.compoundName: "toggle_Button"
*toggleButton_stoptime.x: 335
*toggleButton_stoptime.y: 400
*toggleButton_stoptime.labelString: "REPLICATE ORBITS FROM EPHEMERIS FILE"
*toggleButton_stoptime.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_stoptime.valueChangedCallback.source: public
*toggleButton_stoptime.valueChangedCallback: cb_adjust_phase_stoptime
*toggleButton_stoptime.createManaged: "true"
*toggleButton_stoptime.set: "true"
*toggleButton_stoptime.indicatorSize: 20

*pushButton_cnomcov_quit.class: pushButton
*pushButton_cnomcov_quit.static: true
*pushButton_cnomcov_quit.name: pushButton_cnomcov_quit
*pushButton_cnomcov_quit.parent: CreateNominalCoverage
*pushButton_cnomcov_quit.isCompound: "true"
*pushButton_cnomcov_quit.compoundIcon: "push.xpm"
*pushButton_cnomcov_quit.compoundName: "push_Button"
*pushButton_cnomcov_quit.x: 395
*pushButton_cnomcov_quit.y: 450
*pushButton_cnomcov_quit.width: 120
*pushButton_cnomcov_quit.height: 40
*pushButton_cnomcov_quit.labelString: "QUIT"
*pushButton_cnomcov_quit.fontList: "rockwell-bold"
*pushButton_cnomcov_quit.activateCallback: {\
XtPopdown(XtParent(cnomcov_form)) ;\
}

*pushButton_create_coverage.class: pushButton
*pushButton_create_coverage.static: true
*pushButton_create_coverage.name: pushButton_create_coverage
*pushButton_create_coverage.parent: CreateNominalCoverage
*pushButton_create_coverage.isCompound: "true"
*pushButton_create_coverage.compoundIcon: "push.xpm"
*pushButton_create_coverage.compoundName: "push_Button"
*pushButton_create_coverage.x: 95
*pushButton_create_coverage.y: 450
*pushButton_create_coverage.width: 120
*pushButton_create_coverage.height: 40
*pushButton_create_coverage.labelString: "CREATE"
*pushButton_create_coverage.activateCallback.source: public
*pushButton_create_coverage.activateCallback: cb_do_create_coverage
*pushButton_create_coverage.fontList: "rockwell-bold"
*pushButton_create_coverage.sensitive: "false"

*TF_total_days.class: textField
*TF_total_days.static: true
*TF_total_days.name: TF_total_days
*TF_total_days.parent: CreateNominalCoverage
*TF_total_days.x: 395
*TF_total_days.y: 358
*TF_total_days.height: 32
*TF_total_days.columns: 8
*TF_total_days.resizeWidth: "false"
*TF_total_days.text: "00000.00"
*TF_total_days.cursorPositionVisible: "false"
*TF_total_days.editable: "false"
*TF_total_days.sensitive: "false"
*TF_total_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_total_days.maxLength: 8
*TF_total_days.width: 83
*TF_total_days.modifyVerifyCallback.source: public
*TF_total_days.modifyVerifyCallback: cb_filter_text
*TF_total_days.modifyVerifyCallbackClientData: (XtPointer) valid_float_chars
*TF_total_days.focusCallback.source: public
*TF_total_days.focusCallback: cb_toggle_cursor
*TF_total_days.focusCallbackClientData: (XtPointer) TRUE
*TF_total_days.losingFocusCallback.source: public
*TF_total_days.losingFocusCallback: cb_toggle_cursor
*TF_total_days.losingFocusCallbackClientData: (XtPointer) FALSE

*label44.class: label
*label44.static: true
*label44.name: label44
*label44.parent: CreateNominalCoverage
*label44.isCompound: "true"
*label44.compoundIcon: "label.xpm"
*label44.compoundName: "label_"
*label44.x: 21
*label44.y: 508
*label44.width: 590
*label44.height: 20
*label44.labelString: "MESSAGES"
*label44.fontList: "-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1"

*scrolledWindowText1.class: scrolledWindow
*scrolledWindowText1.static: true
*scrolledWindowText1.name: scrolledWindowText1
*scrolledWindowText1.parent: CreateNominalCoverage
*scrolledWindowText1.scrollingPolicy: "application_defined"
*scrolledWindowText1.visualPolicy: "variable"
*scrolledWindowText1.scrollBarDisplayPolicy: "static"
*scrolledWindowText1.isCompound: "true"
*scrolledWindowText1.compoundIcon: "scrltext.xpm"
*scrolledWindowText1.compoundName: "scrolled_Text"
*scrolledWindowText1.x: 28
*scrolledWindowText1.y: 532
*scrolledWindowText1.width: 590
*scrolledWindowText1.height: 160

*scrolledText_cnomcov_status.class: scrolledText
*scrolledText_cnomcov_status.static: true
*scrolledText_cnomcov_status.name: scrolledText_cnomcov_status
*scrolledText_cnomcov_status.parent: scrolledWindowText1
*scrolledText_cnomcov_status.width: 571
*scrolledText_cnomcov_status.height: 150
*scrolledText_cnomcov_status.editMode: "multi_line_edit"
*scrolledText_cnomcov_status.editable: "false"

*pushButton_refresh.class: pushButton
*pushButton_refresh.static: true
*pushButton_refresh.name: pushButton_refresh
*pushButton_refresh.parent: CreateNominalCoverage
*pushButton_refresh.isCompound: "true"
*pushButton_refresh.compoundIcon: "push.xpm"
*pushButton_refresh.compoundName: "push_Button"
*pushButton_refresh.x: 35
*pushButton_refresh.y: 90
*pushButton_refresh.width: 25
*pushButton_refresh.height: 110
*pushButton_refresh.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton_refresh.fontList: "rockwell-bold"

*optionMenu_ncov_sensor.class: rowColumn
*optionMenu_ncov_sensor.static: true
*optionMenu_ncov_sensor.name: optionMenu_ncov_sensor
*optionMenu_ncov_sensor.parent: CreateNominalCoverage
*optionMenu_ncov_sensor.rowColumnType: "menu_option"
*optionMenu_ncov_sensor.subMenuId: "subMenu_ncov_sensor"
*optionMenu_ncov_sensor.isCompound: "true"
*optionMenu_ncov_sensor.compoundIcon: "optionM.xpm"
*optionMenu_ncov_sensor.compoundName: "option_Menu"
*optionMenu_ncov_sensor.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_ncov_sensor.x: 198
*optionMenu_ncov_sensor.y: 303
*optionMenu_ncov_sensor.width: 215
*optionMenu_ncov_sensor.height: 35
*optionMenu_ncov_sensor.labelString: "/"
*optionMenu_ncov_sensor.sensitive: "true"

*subMenu_ncov_sensor.class: rowColumn
*subMenu_ncov_sensor.static: true
*subMenu_ncov_sensor.name: subMenu_ncov_sensor
*subMenu_ncov_sensor.parent: optionMenu_ncov_sensor
*subMenu_ncov_sensor.rowColumnType: "menu_pulldown"
*subMenu_ncov_sensor.labelString: ""
*subMenu_ncov_sensor.sensitive: "true"
*subMenu_ncov_sensor.entryCallback.source: public
*subMenu_ncov_sensor.entryCallback: cb_update_coverage_filename
*subMenu_ncov_sensor.entryCallbackClientData: (XtPointer) "SENSOR"
*subMenu_ncov_sensor.x: 0
*subMenu_ncov_sensor.y: 335
*subMenu_ncov_sensor.mappedWhenManaged: "true"

*subMenu_ncov_sensor_SAR.class: pushButton
*subMenu_ncov_sensor_SAR.static: true
*subMenu_ncov_sensor_SAR.name: subMenu_ncov_sensor_SAR
*subMenu_ncov_sensor_SAR.parent: subMenu_ncov_sensor
*subMenu_ncov_sensor_SAR.labelString: "SAR"
*subMenu_ncov_sensor_SAR.fontList: "rockwell-bold"
*subMenu_ncov_sensor_SAR.x: 2
*subMenu_ncov_sensor_SAR.y: 335
*subMenu_ncov_sensor_SAR.createCallback.source: public
*subMenu_ncov_sensor_SAR.createCallback: cb_build_cvrg_allowed_sensor_option_menu

*scrolledWindowList2.class: scrolledWindow
*scrolledWindowList2.static: true
*scrolledWindowList2.name: scrolledWindowList2
*scrolledWindowList2.parent: CreateNominalCoverage
*scrolledWindowList2.scrollingPolicy: "application_defined"
*scrolledWindowList2.visualPolicy: "variable"
*scrolledWindowList2.scrollBarDisplayPolicy: "static"
*scrolledWindowList2.shadowThickness: 0
*scrolledWindowList2.isCompound: "true"
*scrolledWindowList2.compoundIcon: "scrllist.xpm"
*scrolledWindowList2.compoundName: "scrolled_List"
*scrolledWindowList2.x: 66
*scrolledWindowList2.y: 95
*scrolledWindowList2.width: 515

*scrolledList_ephm.class: scrolledList
*scrolledList_ephm.static: true
*scrolledList_ephm.name: scrolledList_ephm
*scrolledList_ephm.parent: scrolledWindowList2
*scrolledList_ephm.width: 460
*scrolledList_ephm.height: 100
*scrolledList_ephm.scrollBarDisplayPolicy: "as_needed"
*scrolledList_ephm.listSizePolicy: "resize_if_possible"
*scrolledList_ephm.visibleItemCount: 5
*scrolledList_ephm.automaticSelection: "false"
*scrolledList_ephm.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_ephm.selectionPolicy: "browse_select"
*scrolledList_ephm.browseSelectionCallback.source: public
*scrolledList_ephm.browseSelectionCallback: cb_update_cnomcov_form

*LBL_coverage_filename.class: label
*LBL_coverage_filename.static: true
*LBL_coverage_filename.name: LBL_coverage_filename
*LBL_coverage_filename.parent: CreateNominalCoverage
*LBL_coverage_filename.isCompound: "true"
*LBL_coverage_filename.compoundIcon: "label.xpm"
*LBL_coverage_filename.compoundName: "label_"
*LBL_coverage_filename.x: 21
*LBL_coverage_filename.y: 399
*LBL_coverage_filename.width: 70
*LBL_coverage_filename.height: 30
*LBL_coverage_filename.labelString: "OUTPUT \nFILENAME:"
*LBL_coverage_filename.alignment: "alignment_end"
*LBL_coverage_filename.mappedWhenManaged: "true"

*TF_coverage_filename.class: textField
*TF_coverage_filename.static: true
*TF_coverage_filename.name: TF_coverage_filename
*TF_coverage_filename.parent: CreateNominalCoverage
*TF_coverage_filename.isCompound: "true"
*TF_coverage_filename.compoundIcon: "textfield.xpm"
*TF_coverage_filename.compoundName: "text_Field"
*TF_coverage_filename.x: 95
*TF_coverage_filename.y: 398
*TF_coverage_filename.height: 32
*TF_coverage_filename.columns: 21
*TF_coverage_filename.cursorPositionVisible: "false"
*TF_coverage_filename.editable: "false"
*TF_coverage_filename.sensitive: "false"
*TF_coverage_filename.resizeWidth: "false"
*TF_coverage_filename.text: ""
*TF_coverage_filename.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_coverage_filename.width: 215
*TF_coverage_filename.traversalOn: "false"
*TF_coverage_filename.mappedWhenManaged: "true"

*optionMenu_ncov_sat.class: rowColumn
*optionMenu_ncov_sat.static: true
*optionMenu_ncov_sat.name: optionMenu_ncov_sat
*optionMenu_ncov_sat.parent: CreateNominalCoverage
*optionMenu_ncov_sat.rowColumnType: "menu_option"
*optionMenu_ncov_sat.subMenuId: "subMenu_ncov_sat"
*optionMenu_ncov_sat.isCompound: "true"
*optionMenu_ncov_sat.compoundIcon: "optionM.xpm"
*optionMenu_ncov_sat.compoundName: "option_Menu"
*optionMenu_ncov_sat.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_ncov_sat.x: 25
*optionMenu_ncov_sat.y: 302
*optionMenu_ncov_sat.width: 215
*optionMenu_ncov_sat.height: 35
*optionMenu_ncov_sat.labelString: "SATELLITE\n  /SENSOR:"
*optionMenu_ncov_sat.sensitive: "false"

*subMenu_ncov_sat.class: rowColumn
*subMenu_ncov_sat.static: true
*subMenu_ncov_sat.name: subMenu_ncov_sat
*subMenu_ncov_sat.parent: optionMenu_ncov_sat
*subMenu_ncov_sat.rowColumnType: "menu_pulldown"
*subMenu_ncov_sat.labelString: ""
*subMenu_ncov_sat.sensitive: "true"
*subMenu_ncov_sat.x: 0
*subMenu_ncov_sat.y: 335
*subMenu_ncov_sat.mappedWhenManaged: "true"
*subMenu_ncov_sat.menuPost: ""

*subMenu_ncov_sat_ERS1.class: pushButton
*subMenu_ncov_sat_ERS1.static: true
*subMenu_ncov_sat_ERS1.name: subMenu_ncov_sat_ERS1
*subMenu_ncov_sat_ERS1.parent: subMenu_ncov_sat
*subMenu_ncov_sat_ERS1.labelString: "RADARSAT"
*subMenu_ncov_sat_ERS1.fontList: "rockwell-bold"
*subMenu_ncov_sat_ERS1.x: 2
*subMenu_ncov_sat_ERS1.y: 335
*subMenu_ncov_sat_ERS1.createCallback.source: public
*subMenu_ncov_sat_ERS1.createCallback: cb_build_cvrg_allowed_satellite_option_menu

*optionMenu_covtype.class: rowColumn
*optionMenu_covtype.name.source: public
*optionMenu_covtype.static: false
*optionMenu_covtype.name: optionMenu_covtype
*optionMenu_covtype.parent: CreateNominalCoverage
*optionMenu_covtype.rowColumnType: "menu_option"
*optionMenu_covtype.subMenuId: "subMenu_covtype"
*optionMenu_covtype.isCompound: "true"
*optionMenu_covtype.compoundIcon: "optionM.xpm"
*optionMenu_covtype.compoundName: "option_Menu"
*optionMenu_covtype.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_covtype.x: 31
*optionMenu_covtype.y: 351
*optionMenu_covtype.width: 215
*optionMenu_covtype.height: 35
*optionMenu_covtype.labelString: "COVERAGE\n    TYPE:"
*optionMenu_covtype.sensitive: "true"

*subMenu_covtype.class: rowColumn
*subMenu_covtype.static: true
*subMenu_covtype.name: subMenu_covtype
*subMenu_covtype.parent: optionMenu_covtype
*subMenu_covtype.rowColumnType: "menu_pulldown"
*subMenu_covtype.labelString: ""
*subMenu_covtype.height: 48
*subMenu_covtype.resizeHeight: "false"
*subMenu_covtype.x: 0
*subMenu_covtype.y: 327
*subMenu_covtype.sensitive: "true"
*subMenu_covtype.mappedWhenManaged: "true"
*subMenu_covtype.entryCallback.source: public
*subMenu_covtype.entryCallback: cb_set_coverage_type

*subMenu_covtype_STN.class: pushButton
*subMenu_covtype_STN.name.source: public
*subMenu_covtype_STN.static: false
*subMenu_covtype_STN.name: subMenu_covtype_STN
*subMenu_covtype_STN.parent: subMenu_covtype
*subMenu_covtype_STN.labelString: "STN MASK"
*subMenu_covtype_STN.fontList: "rockwell-bold"
*subMenu_covtype_STN.x: 2
*subMenu_covtype_STN.y: 353

*subMenu_covtype_GBL.class: pushButton
*subMenu_covtype_GBL.name.source: public
*subMenu_covtype_GBL.static: false
*subMenu_covtype_GBL.name: subMenu_covtype_GBL
*subMenu_covtype_GBL.parent: subMenu_covtype
*subMenu_covtype_GBL.labelString: "GLOBAL"
*subMenu_covtype_GBL.fontList: "rockwell-bold"
*subMenu_covtype_GBL.x: 2
*subMenu_covtype_GBL.y: 353

*T_PHASE_END.class: textField
*T_PHASE_END.static: true
*T_PHASE_END.name: T_PHASE_END
*T_PHASE_END.parent: CreateNominalCoverage
*T_PHASE_END.isCompound: "true"
*T_PHASE_END.compoundIcon: "textfield.xpm"
*T_PHASE_END.compoundName: "text_Field"
*T_PHASE_END.x: 395
*T_PHASE_END.y: 310
*T_PHASE_END.height: 32
*T_PHASE_END.columns: 21
*T_PHASE_END.cursorPositionVisible: "false"
*T_PHASE_END.editable: "false"
*T_PHASE_END.sensitive: "false"
*T_PHASE_END.resizeWidth: "false"
*T_PHASE_END.text: ""
*T_PHASE_END.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*T_PHASE_END.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*T_PHASE_END.modifyVerifyCallback.source: public
*T_PHASE_END.modifyVerifyCallback: cb_filter_text
*T_PHASE_END.maxLength: 21
*T_PHASE_END.activateCallback.source: public
*T_PHASE_END.activateCallback: cb_validate_ASF_datetime
*T_PHASE_END.activateCallbackClientData: (XtPointer) "Coverage Stop Time"
*T_PHASE_END.focusCallback.source: public
*T_PHASE_END.focusCallback: cb_toggle_cursor
*T_PHASE_END.focusCallbackClientData: (XtPointer) TRUE
*T_PHASE_END.losingFocusCallback.source: public
*T_PHASE_END.losingFocusCallback: cb_toggle_cursor
*T_PHASE_END.losingFocusCallbackClientData: (XtPointer) FALSE

*T_PHASE_START.class: textField
*T_PHASE_START.static: true
*T_PHASE_START.name: T_PHASE_START
*T_PHASE_START.parent: CreateNominalCoverage
*T_PHASE_START.isCompound: "true"
*T_PHASE_START.compoundIcon: "textfield.xpm"
*T_PHASE_START.compoundName: "text_Field"
*T_PHASE_START.x: 395
*T_PHASE_START.y: 260
*T_PHASE_START.height: 32
*T_PHASE_START.columns: 21
*T_PHASE_START.cursorPositionVisible: "false"
*T_PHASE_START.editable: "false"
*T_PHASE_START.sensitive: "false"
*T_PHASE_START.resizeWidth: "false"
*T_PHASE_START.text: ""
*T_PHASE_START.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*T_PHASE_START.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*T_PHASE_START.modifyVerifyCallback.source: public
*T_PHASE_START.modifyVerifyCallback: cb_filter_text
*T_PHASE_START.maxLength: 21
*T_PHASE_START.activateCallback.source: public
*T_PHASE_START.activateCallback: cb_validate_ASF_datetime
*T_PHASE_START.activateCallbackClientData: (XtPointer) "Coverage Start Time"
*T_PHASE_START.focusCallback.source: public
*T_PHASE_START.focusCallback: cb_toggle_cursor
*T_PHASE_START.focusCallbackClientData: (XtPointer) TRUE
*T_PHASE_START.losingFocusCallback.source: public
*T_PHASE_START.losingFocusCallback: cb_toggle_cursor
*T_PHASE_START.losingFocusCallbackClientData: (XtPointer) FALSE

*label28.class: label
*label28.static: true
*label28.name: label28
*label28.parent: CreateNominalCoverage
*label28.x: 395
*label28.y: 292
*label28.width: 185
*label28.height: 15
*label28.labelString: "yyyy:ddd:hh:mm:ss:ccc"
*label28.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*label31.class: label
*label31.static: true
*label31.name: label31
*label31.parent: CreateNominalCoverage
*label31.x: 395
*label31.y: 341
*label31.width: 185
*label31.height: 15
*label31.labelString: "yyyy:ddd:hh:mm:ss:ccc"
*label31.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*label32.class: label
*label32.static: true
*label32.name: label32
*label32.parent: CreateNominalCoverage
*label32.isCompound: "true"
*label32.compoundIcon: "label.xpm"
*label32.compoundName: "label_"
*label32.x: 320
*label32.y: 261
*label32.width: 70
*label32.height: 30
*label32.labelString: "START TIME:"
*label32.alignment: "alignment_end"

*label35.class: label
*label35.static: true
*label35.name: label35
*label35.parent: CreateNominalCoverage
*label35.isCompound: "true"
*label35.compoundIcon: "label.xpm"
*label35.compoundName: "label_"
*label35.x: 320
*label35.y: 311
*label35.width: 70
*label35.height: 30
*label35.labelString: " STOP TIME:"
*label35.alignment: "alignment_end"

