! UIMX ascii 2.9 key: 9619                                                      

*pps_query.class: form
*pps_query.classinc:
*pps_query.classspec:
*pps_query.classmembers:
*pps_query.classconstructor: parent
*pps_query.classdestructor: buf
*pps_query.gbldecl: #include <stdio.h>\
#include "pps_common.h"\
#include "pps_util.h"\
\
static char SccsFileId[] = "@(#)pps_query.i	1.8  10/31/97";\
\
char query_labelPixmapString[MAXSMALLBUF];\
extern char rootPath[];\
extern swidget nojoy;\
\
extern char IsAuthorizedUser;\
\
extern void cb_resubmit_to_top(Widget, XtPointer, XtPointer);\
extern void cb_resubmit_to_bottom(Widget, XtPointer, XtPointer);
*pps_query.ispecdecl:
*pps_query.funcdecl: swidget create_pps_query(swidget UxParent)
*pps_query.funcname: create_pps_query
*pps_query.funcdef: "swidget", "<create_pps_query>(%)"
*pps_query.argdecl: swidget UxParent;
*pps_query.arglist: UxParent
*pps_query.arglist.UxParent: "swidget", "%UxParent%"
*pps_query.icode: (void)sprintf(query_labelPixmapString, "%s/%s/bearhd0.xpm",\
rootPath, PPS_PIXMAP_SUBPATH);
*pps_query.fcode: if ( ! IsAuthorizedUser)\
	XtSetSensitive(UxGetWidget(mb_query_top_b1), False);\
\
return(rtrn);\

*pps_query.auxdecl:
*pps_query.name.source: public
*pps_query.static: false
*pps_query.name: pps_query
*pps_query.parent: NO_PARENT
*pps_query.parentExpression: UxParent
*pps_query.defaultShell: transientShell
*pps_query.width: 1144
*pps_query.height: 782
*pps_query.resizePolicy: "resize_none"
*pps_query.isCompound: "true"
*pps_query.compoundIcon: "form.xpm"
*pps_query.compoundName: "form_"
*pps_query.x: 0
*pps_query.y: 0
*pps_query.unitType: "pixels"
*pps_query.defaultButton: "pb_query_query"

*mb_query.class: rowColumn
*mb_query.static: true
*mb_query.name: mb_query
*mb_query.parent: pps_query
*mb_query.rowColumnType: "menu_bar"
*mb_query.isCompound: "true"
*mb_query.compoundIcon: "pulldownM.xpm"
*mb_query.compoundName: "menu_Bar"
*mb_query.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*mb_query.x: 0
*mb_query.y: -1
*mb_query.width: 504
*mb_query.height: 36
*mb_query.menuAccelerator: "<KeyUp>F10"
*mb_query.leftOffset: 0
*mb_query.rightAttachment: "attach_form"
*mb_query.topOffset: 0
*mb_query.resizable: "false"
*mb_query.leftAttachment: "attach_form"
*mb_query.spacing: 30

*pb_query_file.class: rowColumn
*pb_query_file.static: true
*pb_query_file.name: pb_query_file
*pb_query_file.parent: mb_query
*pb_query_file.rowColumnType: "menu_pulldown"

*pb_query_print_results.class: pushButton
*pb_query_print_results.static: true
*pb_query_print_results.name: pb_query_print_results
*pb_query_print_results.parent: pb_query_file
*pb_query_print_results.labelString: "Print Results..."
*pb_query_print_results.mnemonic: "R"
*pb_query_print_results.activateCallback: {\
cb_query_print_results();\
}
*pb_query_print_results.sensitive: "true"
*pb_query_print_results.accelerator: "Ctrl<Key>R"
*pb_query_print_results.acceleratorText: "^R"

*pb_query_print_screen.class: pushButton
*pb_query_print_screen.static: true
*pb_query_print_screen.name: pb_query_print_screen
*pb_query_print_screen.parent: pb_query_file
*pb_query_print_screen.labelString: "Print Screen"
*pb_query_print_screen.activateCallback: {\
extern void pps_print_screen(swidget sw);\
\
pps_print_screen(pps_query);\
}
*pb_query_print_screen.mnemonic: "P"
*pb_query_print_screen.accelerator: "Ctrl<Key>P"
*pb_query_print_screen.acceleratorText: "^P"

*pb_query_exit.class: pushButton
*pb_query_exit.static: true
*pb_query_exit.name: pb_query_exit
*pb_query_exit.parent: pb_query_file
*pb_query_exit.labelString: "Exit"
*pb_query_exit.mnemonic: "x"
*pb_query_exit.activateCallback: {\
UxPopdownInterface(pps_query);\
\
}
*pb_query_exit.accelerator: "Ctrl<Key>X"
*pb_query_exit.acceleratorText: "^X"

*mb_query_edit.class: rowColumn
*mb_query_edit.static: true
*mb_query_edit.name: mb_query_edit
*mb_query_edit.parent: mb_query
*mb_query_edit.rowColumnType: "menu_pulldown"

*pb_query_check_params.class: pushButton
*pb_query_check_params.static: true
*pb_query_check_params.name: pb_query_check_params
*pb_query_check_params.parent: mb_query_edit
*pb_query_check_params.labelString: "Check Params"
*pb_query_check_params.mnemonic: "C"
*pb_query_check_params.accelerator: "Ctrl<Key>C"
*pb_query_check_params.acceleratorText: "^C"
*pb_query_check_params.activateCallback: recheck_pending();

*pb_query_resubmit.class: cascadeButton
*pb_query_resubmit.static: true
*pb_query_resubmit.name: pb_query_resubmit
*pb_query_resubmit.parent: mb_query_edit
*pb_query_resubmit.labelString: "Resubmit"
*pb_query_resubmit.mnemonic: "R"
*pb_query_resubmit.subMenuId: "mb_query_resubmit"

*mb_query_resubmit.class: rowColumn
*mb_query_resubmit.static: true
*mb_query_resubmit.name: mb_query_resubmit
*mb_query_resubmit.parent: mb_query_edit
*mb_query_resubmit.rowColumnType: "menu_pulldown"

*mb_query_resubmit_top.class: pushButton
*mb_query_resubmit_top.static: true
*mb_query_resubmit_top.name: mb_query_resubmit_top
*mb_query_resubmit_top.parent: mb_query_resubmit
*mb_query_resubmit_top.labelString: "To Top of Plan"
*mb_query_resubmit_top.activateCallback.source: public
*mb_query_resubmit_top.activateCallback: cb_resubmit_to_top
*mb_query_resubmit_top.mnemonic: "T"
*mb_query_resubmit_top.accelerator: "Ctrl<Key>T"
*mb_query_resubmit_top.acceleratorText: "^T"

*mb_query_resubmit_bottom.class: pushButton
*mb_query_resubmit_bottom.static: true
*mb_query_resubmit_bottom.name: mb_query_resubmit_bottom
*mb_query_resubmit_bottom.parent: mb_query_resubmit
*mb_query_resubmit_bottom.labelString: "To Bottom of Plan"
*mb_query_resubmit_bottom.activateCallback.source: public
*mb_query_resubmit_bottom.activateCallback: cb_resubmit_to_bottom
*mb_query_resubmit_bottom.mnemonic: "B"
*mb_query_resubmit_bottom.accelerator: "Ctrl<Key>B"
*mb_query_resubmit_bottom.acceleratorText: "^B"

*pb_query_setting.class: rowColumn
*pb_query_setting.static: true
*pb_query_setting.name: pb_query_setting
*pb_query_setting.parent: mb_query
*pb_query_setting.rowColumnType: "menu_pulldown"

*pb_query_load_query.class: pushButton
*pb_query_load_query.static: true
*pb_query_load_query.name: pb_query_load_query
*pb_query_load_query.parent: pb_query_setting
*pb_query_load_query.labelString: "Load Query..."
*pb_query_load_query.mnemonic: "L"
*pb_query_load_query.accelerator: "Ctrl<Key>L"
*pb_query_load_query.acceleratorText: "^L"
*pb_query_load_query.activateCallback: cb_query_load_query();

*pb_query_save_query.class: pushButton
*pb_query_save_query.static: true
*pb_query_save_query.name: pb_query_save_query
*pb_query_save_query.parent: pb_query_setting
*pb_query_save_query.labelString: "Save Query"
*pb_query_save_query.mnemonic: "S"
*pb_query_save_query.accelerator: "Ctrl<Key>S"
*pb_query_save_query.acceleratorText: "^S"
*pb_query_save_query.activateCallback: cb_query_save_query();\


*pb_query_save_query_as.class: pushButton
*pb_query_save_query_as.static: true
*pb_query_save_query_as.name: pb_query_save_query_as
*pb_query_save_query_as.parent: pb_query_setting
*pb_query_save_query_as.labelString: "Save Query As..."
*pb_query_save_query_as.mnemonic: "A"
*pb_query_save_query_as.accelerator: "Ctrl<Key>A"
*pb_query_save_query_as.acceleratorText: "^A"
*pb_query_save_query_as.activateCallback: cb_query_save_query_as();

*pb_query_clear.class: pushButton
*pb_query_clear.static: true
*pb_query_clear.name: pb_query_clear
*pb_query_clear.parent: pb_query_setting
*pb_query_clear.labelString: "Clear Query"
*pb_query_clear.mnemonic: "C"
*pb_query_clear.accelerator: "Ctrl<Key>C"
*pb_query_clear.acceleratorText: "^C"
*pb_query_clear.activateCallback: cb_query_clear_search();

*mb_query_file.class: cascadeButton
*mb_query_file.static: true
*mb_query_file.name: mb_query_file
*mb_query_file.parent: mb_query
*mb_query_file.labelString: "File"
*mb_query_file.subMenuId: "pb_query_file"
*mb_query_file.mnemonic: "F"

*mb_query_top_b1.class: cascadeButton
*mb_query_top_b1.static: true
*mb_query_top_b1.name: mb_query_top_b1
*mb_query_top_b1.parent: mb_query
*mb_query_top_b1.labelString: "Edit"
*mb_query_top_b1.mnemonic: "E"
*mb_query_top_b1.subMenuId: "mb_query_edit"

*mb_query_top_b2.class: cascadeButton
*mb_query_top_b2.static: true
*mb_query_top_b2.name: mb_query_top_b2
*mb_query_top_b2.parent: mb_query
*mb_query_top_b2.labelString: "QuerySetting"
*mb_query_top_b2.mnemonic: "Q"
*mb_query_top_b2.subMenuId: "pb_query_setting"

*f_query_query_settings.class: frame
*f_query_query_settings.static: true
*f_query_query_settings.name: f_query_query_settings
*f_query_query_settings.parent: pps_query
*f_query_query_settings.width: 860
*f_query_query_settings.height: 290
*f_query_query_settings.isCompound: "true"
*f_query_query_settings.compoundIcon: "frame.xpm"
*f_query_query_settings.compoundName: "frame_"
*f_query_query_settings.x: 10
*f_query_query_settings.y: 70

*bb_query_query_settings.class: bulletinBoard
*bb_query_query_settings.static: true
*bb_query_query_settings.name: bb_query_query_settings
*bb_query_query_settings.parent: f_query_query_settings
*bb_query_query_settings.resizePolicy: "resize_none"
*bb_query_query_settings.width: 880
*bb_query_query_settings.height: 286
*bb_query_query_settings.isCompound: "true"
*bb_query_query_settings.compoundIcon: "bboard.xpm"
*bb_query_query_settings.compoundName: "bulletin_Board"
*bb_query_query_settings.x: 0
*bb_query_query_settings.y: 0

*tb_query_L1_Orders.class: toggleButton
*tb_query_L1_Orders.name.source: public
*tb_query_L1_Orders.static: false
*tb_query_L1_Orders.name: tb_query_L1_Orders
*tb_query_L1_Orders.parent: bb_query_query_settings
*tb_query_L1_Orders.x: 16
*tb_query_L1_Orders.y: 10
*tb_query_L1_Orders.width: 89
*tb_query_L1_Orders.height: 24
*tb_query_L1_Orders.labelString: "L1 Orders"
*tb_query_L1_Orders.sensitive: "true"
*tb_query_L1_Orders.valueChangedCallback: do_query_new_order_type();

*tb_query_L1_QLK.class: toggleButton
*tb_query_L1_QLK.name.source: public
*tb_query_L1_QLK.static: false
*tb_query_L1_QLK.name: tb_query_L1_QLK
*tb_query_L1_QLK.parent: bb_query_query_settings
*tb_query_L1_QLK.x: 115
*tb_query_L1_QLK.y: 9
*tb_query_L1_QLK.width: 73
*tb_query_L1_QLK.height: 26
*tb_query_L1_QLK.labelString: "L1 QLK"
*tb_query_L1_QLK.sensitive: "true"
*tb_query_L1_QLK.valueChangedCallback: do_query_new_order_type();

*tb_query_Scan_Orders.class: toggleButton
*tb_query_Scan_Orders.name.source: public
*tb_query_Scan_Orders.static: false
*tb_query_Scan_Orders.name: tb_query_Scan_Orders
*tb_query_Scan_Orders.parent: bb_query_query_settings
*tb_query_Scan_Orders.x: 201
*tb_query_Scan_Orders.y: 10
*tb_query_Scan_Orders.width: 105
*tb_query_Scan_Orders.height: 24
*tb_query_Scan_Orders.labelString: "Scan Orders"
*tb_query_Scan_Orders.valueChangedCallback: do_query_new_order_type();

*tb_query_Scan_QLK.class: toggleButton
*tb_query_Scan_QLK.name.source: public
*tb_query_Scan_QLK.static: false
*tb_query_Scan_QLK.name: tb_query_Scan_QLK
*tb_query_Scan_QLK.parent: bb_query_query_settings
*tb_query_Scan_QLK.x: 319
*tb_query_Scan_QLK.y: 10
*tb_query_Scan_QLK.width: 88
*tb_query_Scan_QLK.height: 24
*tb_query_Scan_QLK.labelString: "Scan QLK"
*tb_query_Scan_QLK.valueChangedCallback: do_query_new_order_type();

*om_query_sat.class: rowColumn
*om_query_sat.name.source: public
*om_query_sat.static: false
*om_query_sat.name: om_query_sat
*om_query_sat.parent: bb_query_query_settings
*om_query_sat.rowColumnType: "menu_option"
*om_query_sat.subMenuId: "rc_query_sat"
*om_query_sat.isCompound: "true"
*om_query_sat.compoundIcon: "optionM.xpm"
*om_query_sat.compoundName: "option_Menu"
*om_query_sat.x: 15
*om_query_sat.y: 44
*om_query_sat.width: 48
*om_query_sat.height: 60
*om_query_sat.labelString: "Sat"
*om_query_sat.sensitive: "false"

*rc_query_sat.class: rowColumn
*rc_query_sat.name.source: public
*rc_query_sat.static: false
*rc_query_sat.name: rc_query_sat
*rc_query_sat.parent: om_query_sat
*rc_query_sat.rowColumnType: "menu_pulldown"
*rc_query_sat.sensitive: "false"

*pb_query_sat_any.class: pushButton
*pb_query_sat_any.name.source: public
*pb_query_sat_any.static: false
*pb_query_sat_any.name: pb_query_sat_any
*pb_query_sat_any.parent: rc_query_sat
*pb_query_sat_any.labelString: "Any"
*pb_query_sat_any.sensitive: "false"
*pb_query_sat_any.activateCallback.source: public
*pb_query_sat_any.activateCallback: 

*pb_query_sat_e1.class: pushButton
*pb_query_sat_e1.name.source: public
*pb_query_sat_e1.static: false
*pb_query_sat_e1.name: pb_query_sat_e1
*pb_query_sat_e1.parent: rc_query_sat
*pb_query_sat_e1.labelString: "E1"
*pb_query_sat_e1.sensitive: "false"
*pb_query_sat_e1.activateCallback.source: public
*pb_query_sat_e1.activateCallback: 

*pb_query_sat_e2.class: pushButton
*pb_query_sat_e2.name.source: public
*pb_query_sat_e2.static: false
*pb_query_sat_e2.name: pb_query_sat_e2
*pb_query_sat_e2.parent: rc_query_sat
*pb_query_sat_e2.labelString: "E2"
*pb_query_sat_e2.sensitive: "false"
*pb_query_sat_e2.activateCallback.source: public
*pb_query_sat_e2.activateCallback: 

*pb_query_sat_j1.class: pushButton
*pb_query_sat_j1.name.source: public
*pb_query_sat_j1.static: false
*pb_query_sat_j1.name: pb_query_sat_j1
*pb_query_sat_j1.parent: rc_query_sat
*pb_query_sat_j1.labelString: "J1"
*pb_query_sat_j1.sensitive: "false"

*pb_query_sat_r1.class: pushButton
*pb_query_sat_r1.name.source: public
*pb_query_sat_r1.static: false
*pb_query_sat_r1.name: pb_query_sat_r1
*pb_query_sat_r1.parent: rc_query_sat
*pb_query_sat_r1.labelString: "R1"
*pb_query_sat_r1.sensitive: "false"
*pb_query_sat_r1.activateCallback.source: public
*pb_query_sat_r1.activateCallback: 

*om_query_sens.class: rowColumn
*om_query_sens.name.source: public
*om_query_sens.static: false
*om_query_sens.name: om_query_sens
*om_query_sens.parent: bb_query_query_settings
*om_query_sens.rowColumnType: "menu_option"
*om_query_sens.subMenuId: "rc_query_sens"
*om_query_sens.isCompound: "true"
*om_query_sens.compoundIcon: "optionM.xpm"
*om_query_sens.compoundName: "option_Menu"
*om_query_sens.x: 120
*om_query_sens.y: 45
*om_query_sens.width: 60
*om_query_sens.height: 54
*om_query_sens.labelString: "Sens"
*om_query_sens.sensitive: "false"

*rc_query_sens.class: rowColumn
*rc_query_sens.name.source: public
*rc_query_sens.static: false
*rc_query_sens.name: rc_query_sens
*rc_query_sens.parent: om_query_sens
*rc_query_sens.rowColumnType: "menu_pulldown"
*rc_query_sens.sensitive: "false"

*pb_query_sens_any.class: pushButton
*pb_query_sens_any.name.source: public
*pb_query_sens_any.static: false
*pb_query_sens_any.name: pb_query_sens_any
*pb_query_sens_any.parent: rc_query_sens
*pb_query_sens_any.labelString: "Any"
*pb_query_sens_any.sensitive: "false"
*pb_query_sens_any.activateCallback.source: public
*pb_query_sens_any.activateCallback: 

*pb_query_sens_s.class: pushButton
*pb_query_sens_s.name.source: public
*pb_query_sens_s.static: false
*pb_query_sens_s.name: pb_query_sens_s
*pb_query_sens_s.parent: rc_query_sens
*pb_query_sens_s.labelString: "S"
*pb_query_sens_s.sensitive: "false"
*pb_query_sens_s.activateCallback.source: public
*pb_query_sens_s.activateCallback: 

*rc_query_sens_o.class: pushButton
*rc_query_sens_o.static: true
*rc_query_sens_o.name: rc_query_sens_o
*rc_query_sens_o.parent: rc_query_sens
*rc_query_sens_o.labelString: "O"

*rc_query_sens_v.class: pushButton
*rc_query_sens_v.static: true
*rc_query_sens_v.name: rc_query_sens_v
*rc_query_sens_v.parent: rc_query_sens
*rc_query_sens_v.labelString: "V"
*rc_query_sens_v.activateCallback.source: public
*rc_query_sens_v.activateCallback: 

*rc_query_sens_z.class: pushButton
*rc_query_sens_z.static: true
*rc_query_sens_z.name: rc_query_sens_z
*rc_query_sens_z.parent: rc_query_sens
*rc_query_sens_z.labelString: "Z"
*rc_query_sens_z.activateCallback.source: public
*rc_query_sens_z.activateCallback: 

*om_query_activity.class: rowColumn
*om_query_activity.name.source: public
*om_query_activity.static: false
*om_query_activity.name: om_query_activity
*om_query_activity.parent: bb_query_query_settings
*om_query_activity.rowColumnType: "menu_option"
*om_query_activity.subMenuId: "rc_query_activity"
*om_query_activity.isCompound: "true"
*om_query_activity.compoundIcon: "optionM.xpm"
*om_query_activity.compoundName: "option_Menu"
*om_query_activity.x: 457
*om_query_activity.y: 43
*om_query_activity.width: 60
*om_query_activity.height: 54
*om_query_activity.labelString: "Activity"
*om_query_activity.sensitive: "false"

*rc_query_activity.class: rowColumn
*rc_query_activity.name.source: public
*rc_query_activity.static: false
*rc_query_activity.name: rc_query_activity
*rc_query_activity.parent: om_query_activity
*rc_query_activity.rowColumnType: "menu_pulldown"
*rc_query_activity.sensitive: "false"

*pb_query_activity_any.class: pushButton
*pb_query_activity_any.name.source: public
*pb_query_activity_any.static: false
*pb_query_activity_any.name: pb_query_activity_any
*pb_query_activity_any.parent: rc_query_activity
*pb_query_activity_any.labelString: "Any"
*pb_query_activity_any.sensitive: "false"
*pb_query_activity_any.activateCallback.source: public
*pb_query_activity_any.activateCallback: 

*pb_query_activity_rlt.class: pushButton
*pb_query_activity_rlt.name.source: public
*pb_query_activity_rlt.static: false
*pb_query_activity_rlt.name: pb_query_activity_rlt
*pb_query_activity_rlt.parent: rc_query_activity
*pb_query_activity_rlt.labelString: "RLT"
*pb_query_activity_rlt.sensitive: "false"
*pb_query_activity_rlt.activateCallback.source: public
*pb_query_activity_rlt.activateCallback: 

*pb_query_activity_dmp.class: pushButton
*pb_query_activity_dmp.name.source: public
*pb_query_activity_dmp.static: false
*pb_query_activity_dmp.name: pb_query_activity_dmp
*pb_query_activity_dmp.parent: rc_query_activity
*pb_query_activity_dmp.labelString: "DMP"
*pb_query_activity_dmp.sensitive: "false"

*om_query_station.class: rowColumn
*om_query_station.name.source: public
*om_query_station.static: false
*om_query_station.name: om_query_station
*om_query_station.parent: bb_query_query_settings
*om_query_station.rowColumnType: "menu_option"
*om_query_station.subMenuId: "rc_query_station"
*om_query_station.isCompound: "true"
*om_query_station.compoundIcon: "optionM.xpm"
*om_query_station.compoundName: "option_Menu"
*om_query_station.x: 650
*om_query_station.y: 40
*om_query_station.width: 60
*om_query_station.height: 54
*om_query_station.labelString: "Station"
*om_query_station.sensitive: "false"

*rc_query_station.class: rowColumn
*rc_query_station.name.source: public
*rc_query_station.static: false
*rc_query_station.name: rc_query_station
*rc_query_station.parent: om_query_station
*rc_query_station.rowColumnType: "menu_pulldown"
*rc_query_station.sensitive: "false"

*pb_query_station_any.class: pushButton
*pb_query_station_any.name.source: public
*pb_query_station_any.static: false
*pb_query_station_any.name: pb_query_station_any
*pb_query_station_any.parent: rc_query_station
*pb_query_station_any.labelString: "Any"
*pb_query_station_any.sensitive: "false"
*pb_query_station_any.activateCallback.source: public
*pb_query_station_any.activateCallback: 

*pb_query_station_fa.class: pushButton
*pb_query_station_fa.name.source: public
*pb_query_station_fa.static: false
*pb_query_station_fa.name: pb_query_station_fa
*pb_query_station_fa.parent: rc_query_station
*pb_query_station_fa.labelString: "FA"
*pb_query_station_fa.sensitive: "false"
*pb_query_station_fa.activateCallback.source: public
*pb_query_station_fa.activateCallback: 

*pb_query_station_mc.class: pushButton
*pb_query_station_mc.name.source: public
*pb_query_station_mc.static: false
*pb_query_station_mc.name: pb_query_station_mc
*pb_query_station_mc.parent: rc_query_station
*pb_query_station_mc.labelString: "MC"
*pb_query_station_mc.sensitive: "false"

*pb_query_station_gh.class: pushButton
*pb_query_station_gh.static: true
*pb_query_station_gh.name: pb_query_station_gh
*pb_query_station_gh.parent: rc_query_station
*pb_query_station_gh.labelString: "GH"

*pb_query_station_ph.class: pushButton
*pb_query_station_ph.static: true
*pb_query_station_ph.name: pb_query_station_ph
*pb_query_station_ph.parent: rc_query_station
*pb_query_station_ph.labelString: "PH"

*pb_query_station_as.class: pushButton
*pb_query_station_as.static: true
*pb_query_station_as.name: pb_query_station_as
*pb_query_station_as.parent: rc_query_station
*pb_query_station_as.labelString: "AS"

*pb_query_station_be.class: pushButton
*pb_query_station_be.static: true
*pb_query_station_be.name: pb_query_station_be
*pb_query_station_be.parent: rc_query_station
*pb_query_station_be.labelString: "BE"

*pb_query_station_co.class: pushButton
*pb_query_station_co.static: true
*pb_query_station_co.name: pb_query_station_co
*pb_query_station_co.parent: rc_query_station
*pb_query_station_co.labelString: "CO"

*pb_query_station_cu.class: pushButton
*pb_query_station_cu.static: true
*pb_query_station_cu.name: pb_query_station_cu
*pb_query_station_cu.parent: rc_query_station
*pb_query_station_cu.labelString: "CU"

*pb_query_station_es.class: pushButton
*pb_query_station_es.static: true
*pb_query_station_es.name: pb_query_station_es
*pb_query_station_es.parent: rc_query_station
*pb_query_station_es.labelString: "ES"

*pb_query_station_fs.class: pushButton
*pb_query_station_fs.static: true
*pb_query_station_fs.name: pb_query_station_fs
*pb_query_station_fs.parent: rc_query_station
*pb_query_station_fs.labelString: "FS"

*pb_query_station_ha.class: pushButton
*pb_query_station_ha.static: true
*pb_query_station_ha.name: pb_query_station_ha
*pb_query_station_ha.parent: rc_query_station
*pb_query_station_ha.labelString: "HA"

*pb_query_station_ho.class: pushButton
*pb_query_station_ho.static: true
*pb_query_station_ho.name: pb_query_station_ho
*pb_query_station_ho.parent: rc_query_station
*pb_query_station_ho.labelString: "HO"

*pb_query_station_hy.class: pushButton
*pb_query_station_hy.static: true
*pb_query_station_hy.name: pb_query_station_hy
*pb_query_station_hy.parent: rc_query_station
*pb_query_station_hy.labelString: "HY"

*pb_query_station_is.class: pushButton
*pb_query_station_is.static: true
*pb_query_station_is.name: pb_query_station_is
*pb_query_station_is.parent: rc_query_station
*pb_query_station_is.labelString: "IS"

*pb_query_station_in.class: pushButton
*pb_query_station_in.static: true
*pb_query_station_in.name: pb_query_station_in
*pb_query_station_in.parent: rc_query_station
*pb_query_station_in.labelString: "IN"

*pb_query_station_jo.class: pushButton
*pb_query_station_jo.static: true
*pb_query_station_jo.name: pb_query_station_jo
*pb_query_station_jo.parent: rc_query_station
*pb_query_station_jo.labelString: "JO"

*pb_query_station_ks.class: pushButton
*pb_query_station_ks.static: true
*pb_query_station_ks.name: pb_query_station_ks
*pb_query_station_ks.parent: rc_query_station
*pb_query_station_ks.labelString: "KS"

*pb_query_station_ku.class: pushButton
*pb_query_station_ku.static: true
*pb_query_station_ku.name: pb_query_station_ku
*pb_query_station_ku.parent: rc_query_station
*pb_query_station_ku.labelString: "KU"

*pb_query_station_ma.class: pushButton
*pb_query_station_ma.static: true
*pb_query_station_ma.name: pb_query_station_ma
*pb_query_station_ma.parent: rc_query_station
*pb_query_station_ma.labelString: "MA"

*pb_query_station_ms.class: pushButton
*pb_query_station_ms.static: true
*pb_query_station_ms.name: pb_query_station_ms
*pb_query_station_ms.parent: rc_query_station
*pb_query_station_ms.labelString: "MS"

*pb_query_station_pp.class: pushButton
*pb_query_station_pp.static: true
*pb_query_station_pp.name: pb_query_station_pp
*pb_query_station_pp.parent: rc_query_station
*pb_query_station_pp.labelString: "PP"

*pb_query_station_sa.class: pushButton
*pb_query_station_sa.static: true
*pb_query_station_sa.name: pb_query_station_sa
*pb_query_station_sa.parent: rc_query_station
*pb_query_station_sa.labelString: "SA"

*pb_query_station_se.class: pushButton
*pb_query_station_se.static: true
*pb_query_station_se.name: pb_query_station_se
*pb_query_station_se.parent: rc_query_station
*pb_query_station_se.labelString: "SE"

*pb_query_station_sy.class: pushButton
*pb_query_station_sy.static: true
*pb_query_station_sy.name: pb_query_station_sy
*pb_query_station_sy.parent: rc_query_station
*pb_query_station_sy.labelString: "SY"

*pb_query_station_tf.class: pushButton
*pb_query_station_tf.static: true
*pb_query_station_tf.name: pb_query_station_tf
*pb_query_station_tf.parent: rc_query_station
*pb_query_station_tf.labelString: "TF"

*pb_query_station_tg.class: pushButton
*pb_query_station_tg.static: true
*pb_query_station_tg.name: pb_query_station_tg
*pb_query_station_tg.parent: rc_query_station
*pb_query_station_tg.labelString: "TG"

*pb_query_station_th.class: pushButton
*pb_query_station_th.static: true
*pb_query_station_th.name: pb_query_station_th
*pb_query_station_th.parent: rc_query_station
*pb_query_station_th.labelString: "TH"

*pb_query_station_to.class: pushButton
*pb_query_station_to.static: true
*pb_query_station_to.name: pb_query_station_to
*pb_query_station_to.parent: rc_query_station
*pb_query_station_to.labelString: "TO"

*pb_query_station_ts.class: pushButton
*pb_query_station_ts.static: true
*pb_query_station_ts.name: pb_query_station_ts
*pb_query_station_ts.parent: rc_query_station
*pb_query_station_ts.labelString: "TS"

*pb_query_station_wf.class: pushButton
*pb_query_station_wf.static: true
*pb_query_station_wf.name: pb_query_station_wf
*pb_query_station_wf.parent: rc_query_station
*pb_query_station_wf.labelString: "WF"

*pb_query_station_wh.class: pushButton
*pb_query_station_wh.static: true
*pb_query_station_wh.name: pb_query_station_wh
*pb_query_station_wh.parent: rc_query_station
*pb_query_station_wh.labelString: "WH"

*om_query_priority.class: rowColumn
*om_query_priority.name.source: public
*om_query_priority.static: false
*om_query_priority.name: om_query_priority
*om_query_priority.parent: bb_query_query_settings
*om_query_priority.rowColumnType: "menu_option"
*om_query_priority.subMenuId: "rc_query_priority"
*om_query_priority.isCompound: "true"
*om_query_priority.compoundIcon: "optionM.xpm"
*om_query_priority.compoundName: "option_Menu"
*om_query_priority.x: 463
*om_query_priority.y: 91
*om_query_priority.width: 60
*om_query_priority.height: 54
*om_query_priority.labelString: "Priority"
*om_query_priority.sensitive: "false"

*rc_query_priority.class: rowColumn
*rc_query_priority.name.source: public
*rc_query_priority.static: false
*rc_query_priority.name: rc_query_priority
*rc_query_priority.parent: om_query_priority
*rc_query_priority.rowColumnType: "menu_pulldown"
*rc_query_priority.sensitive: "false"

*pb_query_priority_any.class: pushButton
*pb_query_priority_any.name.source: public
*pb_query_priority_any.static: false
*pb_query_priority_any.name: pb_query_priority_any
*pb_query_priority_any.parent: rc_query_priority
*pb_query_priority_any.labelString: "Any"
*pb_query_priority_any.sensitive: "false"
*pb_query_priority_any.activateCallback.source: public
*pb_query_priority_any.activateCallback: 

*pb_query_priority_low.class: pushButton
*pb_query_priority_low.name.source: public
*pb_query_priority_low.static: false
*pb_query_priority_low.name: pb_query_priority_low
*pb_query_priority_low.parent: rc_query_priority
*pb_query_priority_low.labelString: "LOW"
*pb_query_priority_low.sensitive: "false"
*pb_query_priority_low.activateCallback.source: public
*pb_query_priority_low.activateCallback: 

*pb_query_priority_routine.class: pushButton
*pb_query_priority_routine.name.source: public
*pb_query_priority_routine.static: false
*pb_query_priority_routine.name: pb_query_priority_routine
*pb_query_priority_routine.parent: rc_query_priority
*pb_query_priority_routine.labelString: "ROUTINE"
*pb_query_priority_routine.sensitive: "false"
*pb_query_priority_routine.activateCallback.source: public
*pb_query_priority_routine.activateCallback: 

*pb_query_priority_high.class: pushButton
*pb_query_priority_high.name.source: public
*pb_query_priority_high.static: false
*pb_query_priority_high.name: pb_query_priority_high
*pb_query_priority_high.parent: rc_query_priority
*pb_query_priority_high.labelString: "HIGH"
*pb_query_priority_high.sensitive: "false"
*pb_query_priority_high.activateCallback.source: public
*pb_query_priority_high.activateCallback: 

*pb_query_priority_urgent.class: pushButton
*pb_query_priority_urgent.name.source: public
*pb_query_priority_urgent.static: false
*pb_query_priority_urgent.name: pb_query_priority_urgent
*pb_query_priority_urgent.parent: rc_query_priority
*pb_query_priority_urgent.labelString: "URGENT"
*pb_query_priority_urgent.sensitive: "false"
*pb_query_priority_urgent.activateCallback.source: public
*pb_query_priority_urgent.activateCallback: 

*l_query_rev.class: label
*l_query_rev.name.source: public
*l_query_rev.static: false
*l_query_rev.name: l_query_rev
*l_query_rev.parent: bb_query_query_settings
*l_query_rev.isCompound: "true"
*l_query_rev.compoundIcon: "label.xpm"
*l_query_rev.compoundName: "label_"
*l_query_rev.x: 232
*l_query_rev.y: 47
*l_query_rev.width: 34
*l_query_rev.height: 32
*l_query_rev.labelString: "Rev"
*l_query_rev.sensitive: "false"

*tf_query_rev.class: textField
*tf_query_rev.name.source: public
*tf_query_rev.static: false
*tf_query_rev.name: tf_query_rev
*tf_query_rev.parent: bb_query_query_settings
*tf_query_rev.width: 67
*tf_query_rev.isCompound: "true"
*tf_query_rev.compoundIcon: "textfield.xpm"
*tf_query_rev.compoundName: "text_Field"
*tf_query_rev.x: 268
*tf_query_rev.y: 48
*tf_query_rev.height: 32
*tf_query_rev.sensitive: "false"

*l_query_seq.class: label
*l_query_seq.name.source: public
*l_query_seq.static: false
*l_query_seq.name: l_query_seq
*l_query_seq.parent: bb_query_query_settings
*l_query_seq.isCompound: "true"
*l_query_seq.compoundIcon: "label.xpm"
*l_query_seq.compoundName: "label_"
*l_query_seq.x: 345
*l_query_seq.y: 46
*l_query_seq.width: 34
*l_query_seq.height: 32
*l_query_seq.labelString: "Seq"
*l_query_seq.sensitive: "false"

*tf_query_seq.class: textField
*tf_query_seq.name.source: public
*tf_query_seq.static: false
*tf_query_seq.name: tf_query_seq
*tf_query_seq.parent: bb_query_query_settings
*tf_query_seq.width: 55
*tf_query_seq.isCompound: "true"
*tf_query_seq.compoundIcon: "textfield.xpm"
*tf_query_seq.compoundName: "text_Field"
*tf_query_seq.x: 383
*tf_query_seq.y: 47
*tf_query_seq.height: 32
*tf_query_seq.sensitive: "false"

*l_query_job_id.class: label
*l_query_job_id.name.source: public
*l_query_job_id.static: false
*l_query_job_id.name: l_query_job_id
*l_query_job_id.parent: bb_query_query_settings
*l_query_job_id.isCompound: "true"
*l_query_job_id.compoundIcon: "label.xpm"
*l_query_job_id.compoundName: "label_"
*l_query_job_id.x: 9
*l_query_job_id.y: 94
*l_query_job_id.width: 52
*l_query_job_id.height: 32
*l_query_job_id.labelString: "Job ID"
*l_query_job_id.sensitive: "false"

*tf_query_job_id.class: textField
*tf_query_job_id.name.source: public
*tf_query_job_id.static: false
*tf_query_job_id.name: tf_query_job_id
*tf_query_job_id.parent: bb_query_query_settings
*tf_query_job_id.width: 99
*tf_query_job_id.isCompound: "true"
*tf_query_job_id.compoundIcon: "textfield.xpm"
*tf_query_job_id.compoundName: "text_Field"
*tf_query_job_id.x: 67
*tf_query_job_id.y: 94
*tf_query_job_id.height: 32
*tf_query_job_id.sensitive: "false"

*l_query_order_id.class: label
*l_query_order_id.name.source: public
*l_query_order_id.static: false
*l_query_order_id.name: l_query_order_id
*l_query_order_id.parent: bb_query_query_settings
*l_query_order_id.isCompound: "true"
*l_query_order_id.compoundIcon: "label.xpm"
*l_query_order_id.compoundName: "label_"
*l_query_order_id.x: 171
*l_query_order_id.y: 94
*l_query_order_id.width: 73
*l_query_order_id.height: 32
*l_query_order_id.labelString: "Order ID"
*l_query_order_id.sensitive: "false"

*tf_query_order_id.class: textField
*tf_query_order_id.name.source: public
*tf_query_order_id.static: false
*tf_query_order_id.name: tf_query_order_id
*tf_query_order_id.parent: bb_query_query_settings
*tf_query_order_id.width: 94
*tf_query_order_id.isCompound: "true"
*tf_query_order_id.compoundIcon: "textfield.xpm"
*tf_query_order_id.compoundName: "text_Field"
*tf_query_order_id.x: 245
*tf_query_order_id.y: 94
*tf_query_order_id.height: 32
*tf_query_order_id.sensitive: "false"

*l_query_item_id.class: label
*l_query_item_id.name.source: public
*l_query_item_id.static: false
*l_query_item_id.name: l_query_item_id
*l_query_item_id.parent: bb_query_query_settings
*l_query_item_id.isCompound: "true"
*l_query_item_id.compoundIcon: "label.xpm"
*l_query_item_id.compoundName: "label_"
*l_query_item_id.x: 344
*l_query_item_id.y: 94
*l_query_item_id.width: 61
*l_query_item_id.height: 32
*l_query_item_id.labelString: "Item ID"
*l_query_item_id.sensitive: "false"

*tf_query_item_id.class: textField
*tf_query_item_id.name.source: public
*tf_query_item_id.static: false
*tf_query_item_id.name: tf_query_item_id
*tf_query_item_id.parent: bb_query_query_settings
*tf_query_item_id.width: 43
*tf_query_item_id.isCompound: "true"
*tf_query_item_id.compoundIcon: "textfield.xpm"
*tf_query_item_id.compoundName: "text_Field"
*tf_query_item_id.x: 409
*tf_query_item_id.y: 93
*tf_query_item_id.height: 32
*tf_query_item_id.sensitive: "false"

*l_query_frame_id.class: label
*l_query_frame_id.name.source: public
*l_query_frame_id.static: false
*l_query_frame_id.name: l_query_frame_id
*l_query_frame_id.parent: bb_query_query_settings
*l_query_frame_id.isCompound: "true"
*l_query_frame_id.compoundIcon: "label.xpm"
*l_query_frame_id.compoundName: "label_"
*l_query_frame_id.x: 5
*l_query_frame_id.y: 142
*l_query_frame_id.width: 76
*l_query_frame_id.height: 32
*l_query_frame_id.labelString: "Frame ID"
*l_query_frame_id.sensitive: "false"

*tf_query_frame_id.class: textField
*tf_query_frame_id.name.source: public
*tf_query_frame_id.static: false
*tf_query_frame_id.name: tf_query_frame_id
*tf_query_frame_id.parent: bb_query_query_settings
*tf_query_frame_id.width: 67
*tf_query_frame_id.isCompound: "true"
*tf_query_frame_id.compoundIcon: "textfield.xpm"
*tf_query_frame_id.compoundName: "text_Field"
*tf_query_frame_id.x: 78
*tf_query_frame_id.y: 142
*tf_query_frame_id.height: 32
*tf_query_frame_id.sensitive: "false"

*l_query_subframe_id.class: label
*l_query_subframe_id.name.source: public
*l_query_subframe_id.static: false
*l_query_subframe_id.name: l_query_subframe_id
*l_query_subframe_id.parent: bb_query_query_settings
*l_query_subframe_id.isCompound: "true"
*l_query_subframe_id.compoundIcon: "label.xpm"
*l_query_subframe_id.compoundName: "label_"
*l_query_subframe_id.x: 152
*l_query_subframe_id.y: 141
*l_query_subframe_id.width: 109
*l_query_subframe_id.height: 32
*l_query_subframe_id.labelString: "Subframe ID"
*l_query_subframe_id.sensitive: "false"

*tf_query_subframe_id.class: textField
*tf_query_subframe_id.name.source: public
*tf_query_subframe_id.static: false
*tf_query_subframe_id.name: tf_query_subframe_id
*tf_query_subframe_id.parent: bb_query_query_settings
*tf_query_subframe_id.width: 43
*tf_query_subframe_id.isCompound: "true"
*tf_query_subframe_id.compoundIcon: "textfield.xpm"
*tf_query_subframe_id.compoundName: "text_Field"
*tf_query_subframe_id.x: 262
*tf_query_subframe_id.y: 140
*tf_query_subframe_id.height: 32
*tf_query_subframe_id.sensitive: "false"

*om_query_product_type.class: rowColumn
*om_query_product_type.name.source: public
*om_query_product_type.static: false
*om_query_product_type.name: om_query_product_type
*om_query_product_type.parent: bb_query_query_settings
*om_query_product_type.rowColumnType: "menu_option"
*om_query_product_type.subMenuId: "rc_query_product_type"
*om_query_product_type.isCompound: "true"
*om_query_product_type.compoundIcon: "optionM.xpm"
*om_query_product_type.compoundName: "option_Menu"
*om_query_product_type.x: 350
*om_query_product_type.y: 140
*om_query_product_type.width: 60
*om_query_product_type.height: 54
*om_query_product_type.labelString: "Product Type"
*om_query_product_type.sensitive: "false"

*rc_query_product_type.class: rowColumn
*rc_query_product_type.name.source: public
*rc_query_product_type.static: false
*rc_query_product_type.name: rc_query_product_type
*rc_query_product_type.parent: om_query_product_type
*rc_query_product_type.rowColumnType: "menu_pulldown"
*rc_query_product_type.x: 0
*rc_query_product_type.y: 88
*rc_query_product_type.sensitive: "false"

*pb_query_product_type_any.class: pushButton
*pb_query_product_type_any.name.source: public
*pb_query_product_type_any.static: false
*pb_query_product_type_any.name: pb_query_product_type_any
*pb_query_product_type_any.parent: rc_query_product_type
*pb_query_product_type_any.labelString: "Any"
*pb_query_product_type_any.x: 2
*pb_query_product_type_any.y: 145
*pb_query_product_type_any.sensitive: "false"
*pb_query_product_type_any.activateCallback.source: public
*pb_query_product_type_any.activateCallback: 

*pb_query_product_type_standard.class: pushButton
*pb_query_product_type_standard.name.source: public
*pb_query_product_type_standard.static: false
*pb_query_product_type_standard.name: pb_query_product_type_standard
*pb_query_product_type_standard.parent: rc_query_product_type
*pb_query_product_type_standard.labelString: "STANDARD"
*pb_query_product_type_standard.x: 2
*pb_query_product_type_standard.y: 145
*pb_query_product_type_standard.sensitive: "false"
*pb_query_product_type_standard.activateCallback.source: public
*pb_query_product_type_standard.activateCallback: 

*rc_query_product_type_calset.class: pushButton
*rc_query_product_type_calset.static: true
*rc_query_product_type_calset.name: rc_query_product_type_calset
*rc_query_product_type_calset.parent: rc_query_product_type
*rc_query_product_type_calset.labelString: "CAL_SET"
*rc_query_product_type_calset.activateCallback.source: public
*rc_query_product_type_calset.activateCallback: 

*rc_query_product_type_ramp.class: pushButton
*rc_query_product_type_ramp.static: true
*rc_query_product_type_ramp.name: rc_query_product_type_ramp
*rc_query_product_type_ramp.parent: rc_query_product_type
*rc_query_product_type_ramp.labelString: "RAMP"
*rc_query_product_type_ramp.activateCallback.source: public
*rc_query_product_type_ramp.activateCallback: 

*rc_query_product_type_scansar.class: pushButton
*rc_query_product_type_scansar.static: true
*rc_query_product_type_scansar.name: rc_query_product_type_scansar
*rc_query_product_type_scansar.parent: rc_query_product_type
*rc_query_product_type_scansar.labelString: "SCANSAR"

*pb_query_product_type_complex.class: pushButton
*pb_query_product_type_complex.name.source: public
*pb_query_product_type_complex.static: false
*pb_query_product_type_complex.name: pb_query_product_type_complex
*pb_query_product_type_complex.parent: rc_query_product_type
*pb_query_product_type_complex.labelString: "COMPLEX"
*pb_query_product_type_complex.x: 2
*pb_query_product_type_complex.y: 145
*pb_query_product_type_complex.sensitive: "false"
*pb_query_product_type_complex.activateCallback.source: public
*pb_query_product_type_complex.activateCallback: 

*pb_query_product_type_ccsd.class: pushButton
*pb_query_product_type_ccsd.name.source: public
*pb_query_product_type_ccsd.static: false
*pb_query_product_type_ccsd.name: pb_query_product_type_ccsd
*pb_query_product_type_ccsd.parent: rc_query_product_type
*pb_query_product_type_ccsd.labelString: "CCSD"
*pb_query_product_type_ccsd.x: 2
*pb_query_product_type_ccsd.y: 145
*pb_query_product_type_ccsd.sensitive: "false"
*pb_query_product_type_ccsd.activateCallback.source: public
*pb_query_product_type_ccsd.activateCallback: 

*l_query_pixel_spacing.class: label
*l_query_pixel_spacing.name.source: public
*l_query_pixel_spacing.static: false
*l_query_pixel_spacing.name: l_query_pixel_spacing
*l_query_pixel_spacing.parent: bb_query_query_settings
*l_query_pixel_spacing.isCompound: "true"
*l_query_pixel_spacing.compoundIcon: "label.xpm"
*l_query_pixel_spacing.compoundName: "label_"
*l_query_pixel_spacing.x: 616
*l_query_pixel_spacing.y: 140
*l_query_pixel_spacing.width: 119
*l_query_pixel_spacing.height: 32
*l_query_pixel_spacing.labelString: "Pixel Spacing"
*l_query_pixel_spacing.sensitive: "false"

*tf_query_pixel_spacing.class: textField
*tf_query_pixel_spacing.name.source: public
*tf_query_pixel_spacing.static: false
*tf_query_pixel_spacing.name: tf_query_pixel_spacing
*tf_query_pixel_spacing.parent: bb_query_query_settings
*tf_query_pixel_spacing.width: 43
*tf_query_pixel_spacing.isCompound: "true"
*tf_query_pixel_spacing.compoundIcon: "textfield.xpm"
*tf_query_pixel_spacing.compoundName: "text_Field"
*tf_query_pixel_spacing.x: 744
*tf_query_pixel_spacing.y: 141
*tf_query_pixel_spacing.height: 32
*tf_query_pixel_spacing.sensitive: "false"

*om_query_projection.class: rowColumn
*om_query_projection.name.source: public
*om_query_projection.static: false
*om_query_projection.name: om_query_projection
*om_query_projection.parent: bb_query_query_settings
*om_query_projection.rowColumnType: "menu_option"
*om_query_projection.subMenuId: "rc_query_projection"
*om_query_projection.isCompound: "true"
*om_query_projection.compoundIcon: "optionM.xpm"
*om_query_projection.compoundName: "option_Menu"
*om_query_projection.x: 14
*om_query_projection.y: 187
*om_query_projection.width: 60
*om_query_projection.height: 54
*om_query_projection.labelString: "Projection"
*om_query_projection.sensitive: "false"

*rc_query_projection.class: rowColumn
*rc_query_projection.name.source: public
*rc_query_projection.static: false
*rc_query_projection.name: rc_query_projection
*rc_query_projection.parent: om_query_projection
*rc_query_projection.rowColumnType: "menu_pulldown"
*rc_query_projection.x: 0
*rc_query_projection.y: 77
*rc_query_projection.sensitive: "false"

*pb_query_projection_any.class: pushButton
*pb_query_projection_any.name.source: public
*pb_query_projection_any.static: false
*pb_query_projection_any.name: pb_query_projection_any
*pb_query_projection_any.parent: rc_query_projection
*pb_query_projection_any.labelString: "Any"
*pb_query_projection_any.x: 2
*pb_query_projection_any.y: 145
*pb_query_projection_any.sensitive: "false"
*pb_query_projection_any.activateCallback.source: public
*pb_query_projection_any.activateCallback: 

*pb_query_projection_ground_range.class: pushButton
*pb_query_projection_ground_range.name.source: public
*pb_query_projection_ground_range.static: false
*pb_query_projection_ground_range.name: pb_query_projection_ground_range
*pb_query_projection_ground_range.parent: rc_query_projection
*pb_query_projection_ground_range.labelString: "GROUND_RANGE"
*pb_query_projection_ground_range.x: 2
*pb_query_projection_ground_range.y: 145
*pb_query_projection_ground_range.sensitive: "false"
*pb_query_projection_ground_range.activateCallback.source: public
*pb_query_projection_ground_range.activateCallback: 

*pb_query_projection_slant_range.class: pushButton
*pb_query_projection_slant_range.name.source: public
*pb_query_projection_slant_range.static: false
*pb_query_projection_slant_range.name: pb_query_projection_slant_range
*pb_query_projection_slant_range.parent: rc_query_projection
*pb_query_projection_slant_range.labelString: "SLANT_RANGE"
*pb_query_projection_slant_range.x: 2
*pb_query_projection_slant_range.y: 145
*pb_query_projection_slant_range.sensitive: "false"
*pb_query_projection_slant_range.activateCallback.source: public
*pb_query_projection_slant_range.activateCallback: 

*pb_query_projection_lambert.class: pushButton
*pb_query_projection_lambert.name.source: public
*pb_query_projection_lambert.static: false
*pb_query_projection_lambert.name: pb_query_projection_lambert
*pb_query_projection_lambert.parent: rc_query_projection
*pb_query_projection_lambert.labelString: "LAMBERT"
*pb_query_projection_lambert.x: 2
*pb_query_projection_lambert.y: 145
*pb_query_projection_lambert.sensitive: "false"
*pb_query_projection_lambert.activateCallback.source: public
*pb_query_projection_lambert.activateCallback: 

*pb_query_projection_ps.class: pushButton
*pb_query_projection_ps.name.source: public
*pb_query_projection_ps.static: false
*pb_query_projection_ps.name: pb_query_projection_ps
*pb_query_projection_ps.parent: rc_query_projection
*pb_query_projection_ps.labelString: "PS"
*pb_query_projection_ps.x: 2
*pb_query_projection_ps.y: 145
*pb_query_projection_ps.sensitive: "false"
*pb_query_projection_ps.activateCallback.source: public
*pb_query_projection_ps.activateCallback: 

*pb_query_projection_utm.class: pushButton
*pb_query_projection_utm.name.source: public
*pb_query_projection_utm.static: false
*pb_query_projection_utm.name: pb_query_projection_utm
*pb_query_projection_utm.parent: rc_query_projection
*pb_query_projection_utm.labelString: "UTM"
*pb_query_projection_utm.x: 2
*pb_query_projection_utm.y: 145
*pb_query_projection_utm.sensitive: "false"
*pb_query_projection_utm.activateCallback.source: public
*pb_query_projection_utm.activateCallback: 

*l_query_proc_gain.class: label
*l_query_proc_gain.name.source: public
*l_query_proc_gain.static: false
*l_query_proc_gain.name: l_query_proc_gain
*l_query_proc_gain.parent: bb_query_query_settings
*l_query_proc_gain.isCompound: "true"
*l_query_proc_gain.compoundIcon: "label.xpm"
*l_query_proc_gain.compoundName: "label_"
*l_query_proc_gain.x: 360
*l_query_proc_gain.y: 190
*l_query_proc_gain.width: 88
*l_query_proc_gain.height: 32
*l_query_proc_gain.labelString: "Proc Gain"
*l_query_proc_gain.sensitive: "false"

*tf_query_proc_gain.class: textField
*tf_query_proc_gain.name.source: public
*tf_query_proc_gain.static: false
*tf_query_proc_gain.name: tf_query_proc_gain
*tf_query_proc_gain.parent: bb_query_query_settings
*tf_query_proc_gain.width: 49
*tf_query_proc_gain.isCompound: "true"
*tf_query_proc_gain.compoundIcon: "textfield.xpm"
*tf_query_proc_gain.compoundName: "text_Field"
*tf_query_proc_gain.x: 460
*tf_query_proc_gain.y: 190
*tf_query_proc_gain.height: 32
*tf_query_proc_gain.sensitive: "false"

*om_query_state.class: rowColumn
*om_query_state.name.source: public
*om_query_state.static: false
*om_query_state.name: om_query_state
*om_query_state.parent: bb_query_query_settings
*om_query_state.rowColumnType: "menu_option"
*om_query_state.subMenuId: "rc_query_state"
*om_query_state.isCompound: "true"
*om_query_state.compoundIcon: "optionM.xpm"
*om_query_state.compoundName: "option_Menu"
*om_query_state.x: 655
*om_query_state.y: 89
*om_query_state.width: 60
*om_query_state.height: 54
*om_query_state.labelString: "State"
*om_query_state.sensitive: "false"

*rc_query_state.class: rowColumn
*rc_query_state.name.source: public
*rc_query_state.static: false
*rc_query_state.name: rc_query_state
*rc_query_state.parent: om_query_state
*rc_query_state.rowColumnType: "menu_pulldown"
*rc_query_state.sensitive: "false"

*pb_query_state_any.class: pushButton
*pb_query_state_any.name.source: public
*pb_query_state_any.static: false
*pb_query_state_any.name: pb_query_state_any
*pb_query_state_any.parent: rc_query_state
*pb_query_state_any.labelString: "Any"
*pb_query_state_any.sensitive: "false"
*pb_query_state_any.activateCallback.source: public
*pb_query_state_any.activateCallback: 

*pb_query_state_pending.class: pushButton
*pb_query_state_pending.name.source: public
*pb_query_state_pending.static: false
*pb_query_state_pending.name: pb_query_state_pending
*pb_query_state_pending.parent: rc_query_state
*pb_query_state_pending.labelString: "PENDING"
*pb_query_state_pending.sensitive: "false"
*pb_query_state_pending.activateCallback.source: public
*pb_query_state_pending.activateCallback: 

*pb_query_state_ready.class: pushButton
*pb_query_state_ready.name.source: public
*pb_query_state_ready.static: false
*pb_query_state_ready.name: pb_query_state_ready
*pb_query_state_ready.parent: rc_query_state
*pb_query_state_ready.labelString: "READY"
*pb_query_state_ready.sensitive: "false"
*pb_query_state_ready.activateCallback.source: public
*pb_query_state_ready.activateCallback: 

*pb_query_state_available.class: pushButton
*pb_query_state_available.name.source: public
*pb_query_state_available.static: false
*pb_query_state_available.name: pb_query_state_available
*pb_query_state_available.parent: rc_query_state
*pb_query_state_available.labelString: "AVAILABLE"
*pb_query_state_available.sensitive: "false"
*pb_query_state_available.activateCallback.source: public
*pb_query_state_available.activateCallback: 

*pb_query_state_submitted.class: pushButton
*pb_query_state_submitted.name.source: public
*pb_query_state_submitted.static: false
*pb_query_state_submitted.name: pb_query_state_submitted
*pb_query_state_submitted.parent: rc_query_state
*pb_query_state_submitted.labelString: "SUBMITTED"
*pb_query_state_submitted.sensitive: "false"
*pb_query_state_submitted.activateCallback.source: public
*pb_query_state_submitted.activateCallback: 

*pb_query_state_retry.class: pushButton
*pb_query_state_retry.static: true
*pb_query_state_retry.name: pb_query_state_retry
*pb_query_state_retry.parent: rc_query_state
*pb_query_state_retry.labelString: "RETRY"

*rc_query_state_completed.class: pushButton
*rc_query_state_completed.static: true
*rc_query_state_completed.name: rc_query_state_completed
*rc_query_state_completed.parent: rc_query_state
*rc_query_state_completed.labelString: "COMPLETED"
*rc_query_state_completed.activateCallback.source: public
*rc_query_state_completed.activateCallback: 

*rc_query_state_fail.class: pushButton
*rc_query_state_fail.static: true
*rc_query_state_fail.name: rc_query_state_fail
*rc_query_state_fail.parent: rc_query_state
*rc_query_state_fail.labelString: "CANCEL/FAIL"
*rc_query_state_fail.activateCallback.source: public
*rc_query_state_fail.activateCallback: 

*om_query_media_type.class: rowColumn
*om_query_media_type.name.source: public
*om_query_media_type.static: false
*om_query_media_type.name: om_query_media_type
*om_query_media_type.parent: bb_query_query_settings
*om_query_media_type.rowColumnType: "menu_option"
*om_query_media_type.subMenuId: "rc_query_media_type"
*om_query_media_type.isCompound: "true"
*om_query_media_type.compoundIcon: "optionM.xpm"
*om_query_media_type.compoundName: "option_Menu"
*om_query_media_type.x: 360
*om_query_media_type.y: 240
*om_query_media_type.width: 60
*om_query_media_type.height: 54
*om_query_media_type.labelString: "Media Type"
*om_query_media_type.sensitive: "false"

*rc_query_media_type.class: rowColumn
*rc_query_media_type.name.source: public
*rc_query_media_type.static: false
*rc_query_media_type.name: rc_query_media_type
*rc_query_media_type.parent: om_query_media_type
*rc_query_media_type.rowColumnType: "menu_pulldown"
*rc_query_media_type.x: 0
*rc_query_media_type.y: 217
*rc_query_media_type.sensitive: "false"

*pb_query_media_type_any.class: pushButton
*pb_query_media_type_any.name.source: public
*pb_query_media_type_any.static: false
*pb_query_media_type_any.name: pb_query_media_type_any
*pb_query_media_type_any.parent: rc_query_media_type
*pb_query_media_type_any.labelString: "Any"
*pb_query_media_type_any.x: 2
*pb_query_media_type_any.y: 241
*pb_query_media_type_any.sensitive: "false"
*pb_query_media_type_any.activateCallback.source: public
*pb_query_media_type_any.activateCallback: 

*pb_query_media_type_disk.class: pushButton
*pb_query_media_type_disk.name.source: public
*pb_query_media_type_disk.static: false
*pb_query_media_type_disk.name: pb_query_media_type_disk
*pb_query_media_type_disk.parent: rc_query_media_type
*pb_query_media_type_disk.labelString: "DISK"
*pb_query_media_type_disk.x: 2
*pb_query_media_type_disk.y: 241
*pb_query_media_type_disk.sensitive: "false"
*pb_query_media_type_disk.activateCallback.source: public
*pb_query_media_type_disk.activateCallback: 

*pb_query_media_type_dcrsi.class: pushButton
*pb_query_media_type_dcrsi.name.source: public
*pb_query_media_type_dcrsi.static: false
*pb_query_media_type_dcrsi.name: pb_query_media_type_dcrsi
*pb_query_media_type_dcrsi.parent: rc_query_media_type
*pb_query_media_type_dcrsi.labelString: "DCRSI"
*pb_query_media_type_dcrsi.x: 2
*pb_query_media_type_dcrsi.y: 241
*pb_query_media_type_dcrsi.sensitive: "false"
*pb_query_media_type_dcrsi.activateCallback.source: public
*pb_query_media_type_dcrsi.activateCallback: 

*rc_query_media_type_id1.class: pushButton
*rc_query_media_type_id1.static: true
*rc_query_media_type_id1.name: rc_query_media_type_id1
*rc_query_media_type_id1.parent: rc_query_media_type
*rc_query_media_type_id1.labelString: "ID-1"

*l_query_media_id.class: label
*l_query_media_id.name.source: public
*l_query_media_id.static: false
*l_query_media_id.name: l_query_media_id
*l_query_media_id.parent: bb_query_query_settings
*l_query_media_id.isCompound: "true"
*l_query_media_id.compoundIcon: "label.xpm"
*l_query_media_id.compoundName: "label_"
*l_query_media_id.x: 634
*l_query_media_id.y: 239
*l_query_media_id.width: 87
*l_query_media_id.height: 32
*l_query_media_id.labelString: "Media ID"
*l_query_media_id.sensitive: "false"

*tf_query_media_id.class: textField
*tf_query_media_id.name.source: public
*tf_query_media_id.static: false
*tf_query_media_id.name: tf_query_media_id
*tf_query_media_id.parent: bb_query_query_settings
*tf_query_media_id.width: 83
*tf_query_media_id.isCompound: "true"
*tf_query_media_id.compoundIcon: "textfield.xpm"
*tf_query_media_id.compoundName: "text_Field"
*tf_query_media_id.x: 730
*tf_query_media_id.y: 236
*tf_query_media_id.height: 32
*tf_query_media_id.sensitive: "false"

*om_query_processor_mode.class: rowColumn
*om_query_processor_mode.name.source: public
*om_query_processor_mode.static: false
*om_query_processor_mode.name: om_query_processor_mode
*om_query_processor_mode.parent: bb_query_query_settings
*om_query_processor_mode.rowColumnType: "menu_option"
*om_query_processor_mode.subMenuId: "rc_query_processor_mode"
*om_query_processor_mode.isCompound: "true"
*om_query_processor_mode.compoundIcon: "optionM.xpm"
*om_query_processor_mode.compoundName: "option_Menu"
*om_query_processor_mode.x: 600
*om_query_processor_mode.y: 190
*om_query_processor_mode.labelString: "Processor Mode"
*om_query_processor_mode.sensitive: "false"

*rc_query_processor_mode.class: rowColumn
*rc_query_processor_mode.name.source: public
*rc_query_processor_mode.static: false
*rc_query_processor_mode.name: rc_query_processor_mode
*rc_query_processor_mode.parent: om_query_processor_mode
*rc_query_processor_mode.rowColumnType: "menu_pulldown"
*rc_query_processor_mode.sensitive: "false"

*pb_query_processor_mode_any.class: pushButton
*pb_query_processor_mode_any.name.source: public
*pb_query_processor_mode_any.static: false
*pb_query_processor_mode_any.name: pb_query_processor_mode_any
*pb_query_processor_mode_any.parent: rc_query_processor_mode
*pb_query_processor_mode_any.labelString: "Any"
*pb_query_processor_mode_any.sensitive: "false"
*pb_query_processor_mode_any.activateCallback.source: public
*pb_query_processor_mode_any.activateCallback: 

*pb_query_processor_mode_continuous.class: pushButton
*pb_query_processor_mode_continuous.name.source: public
*pb_query_processor_mode_continuous.static: false
*pb_query_processor_mode_continuous.name: pb_query_processor_mode_continuous
*pb_query_processor_mode_continuous.parent: rc_query_processor_mode
*pb_query_processor_mode_continuous.labelString: "CONTINUOUS"
*pb_query_processor_mode_continuous.sensitive: "false"
*pb_query_processor_mode_continuous.activateCallback.source: public
*pb_query_processor_mode_continuous.activateCallback: 

*pb_query_processor_mode_scansar.class: pushButton
*pb_query_processor_mode_scansar.name.source: public
*pb_query_processor_mode_scansar.static: false
*pb_query_processor_mode_scansar.name: pb_query_processor_mode_scansar
*pb_query_processor_mode_scansar.parent: rc_query_processor_mode
*pb_query_processor_mode_scansar.labelString: "SCANSAR"
*pb_query_processor_mode_scansar.sensitive: "false"
*pb_query_processor_mode_scansar.activateCallback.source: public
*pb_query_processor_mode_scansar.activateCallback: 

*om_query_data_direction.class: rowColumn
*om_query_data_direction.name.source: public
*om_query_data_direction.static: false
*om_query_data_direction.name: om_query_data_direction
*om_query_data_direction.parent: bb_query_query_settings
*om_query_data_direction.rowColumnType: "menu_option"
*om_query_data_direction.subMenuId: "rc_query_data_direction"
*om_query_data_direction.isCompound: "true"
*om_query_data_direction.compoundIcon: "optionM.xpm"
*om_query_data_direction.compoundName: "option_Menu"
*om_query_data_direction.x: 20
*om_query_data_direction.y: 240
*om_query_data_direction.labelString: "Data Direction"
*om_query_data_direction.sensitive: "false"

*rc_query_data_direction.class: rowColumn
*rc_query_data_direction.name.source: public
*rc_query_data_direction.static: false
*rc_query_data_direction.name: rc_query_data_direction
*rc_query_data_direction.parent: om_query_data_direction
*rc_query_data_direction.rowColumnType: "menu_pulldown"
*rc_query_data_direction.sensitive: "false"

*pb_query_data_direction_any.class: pushButton
*pb_query_data_direction_any.name.source: public
*pb_query_data_direction_any.static: false
*pb_query_data_direction_any.name: pb_query_data_direction_any
*pb_query_data_direction_any.parent: rc_query_data_direction
*pb_query_data_direction_any.labelString: "Any"
*pb_query_data_direction_any.sensitive: "false"
*pb_query_data_direction_any.activateCallback.source: public
*pb_query_data_direction_any.activateCallback: 

*pb_query_data_direction_forward.class: pushButton
*pb_query_data_direction_forward.name.source: public
*pb_query_data_direction_forward.static: false
*pb_query_data_direction_forward.name: pb_query_data_direction_forward
*pb_query_data_direction_forward.parent: rc_query_data_direction
*pb_query_data_direction_forward.labelString: "FORWARD"
*pb_query_data_direction_forward.sensitive: "false"
*pb_query_data_direction_forward.activateCallback.source: public
*pb_query_data_direction_forward.activateCallback: 

*pb_query_data_direction_reverse.class: pushButton
*pb_query_data_direction_reverse.name.source: public
*pb_query_data_direction_reverse.static: false
*pb_query_data_direction_reverse.name: pb_query_data_direction_reverse
*pb_query_data_direction_reverse.parent: rc_query_data_direction
*pb_query_data_direction_reverse.labelString: "REVERSE"
*pb_query_data_direction_reverse.sensitive: "false"
*pb_query_data_direction_reverse.activateCallback.source: public
*pb_query_data_direction_reverse.activateCallback: 

*pb_query_data_direction_unknown.class: pushButton
*pb_query_data_direction_unknown.name.source: public
*pb_query_data_direction_unknown.static: false
*pb_query_data_direction_unknown.name: pb_query_data_direction_unknown
*pb_query_data_direction_unknown.parent: rc_query_data_direction
*pb_query_data_direction_unknown.labelString: "UNKNOWN"
*pb_query_data_direction_unknown.sensitive: "false"

*f_query_query_results.class: frame
*f_query_query_results.static: true
*f_query_query_results.name: f_query_query_results
*f_query_query_results.parent: pps_query
*f_query_query_results.width: 1122
*f_query_query_results.height: 368
*f_query_query_results.isCompound: "true"
*f_query_query_results.compoundIcon: "frame.xpm"
*f_query_query_results.compoundName: "frame_"
*f_query_query_results.x: 8
*f_query_query_results.y: 412

*bb_query_query_results.class: bulletinBoard
*bb_query_query_results.static: true
*bb_query_query_results.name: bb_query_query_results
*bb_query_query_results.parent: f_query_query_results
*bb_query_query_results.resizePolicy: "resize_none"
*bb_query_query_results.width: 850
*bb_query_query_results.height: 352
*bb_query_query_results.isCompound: "true"
*bb_query_query_results.compoundIcon: "bboard.xpm"
*bb_query_query_results.compoundName: "bulletin_Board"
*bb_query_query_results.x: 2
*bb_query_query_results.y: 2
*bb_query_query_results.marginHeight: 0
*bb_query_query_results.marginWidth: 0

*l_query_priority.class: label
*l_query_priority.static: true
*l_query_priority.name: l_query_priority
*l_query_priority.parent: bb_query_query_results
*l_query_priority.isCompound: "true"
*l_query_priority.compoundIcon: "label.xpm"
*l_query_priority.compoundName: "label_"
*l_query_priority.x: 76
*l_query_priority.y: 8
*l_query_priority.width: 68
*l_query_priority.height: 32
*l_query_priority.labelString: "Priority"

*l_query_media.class: label
*l_query_media.static: true
*l_query_media.name: l_query_media
*l_query_media.parent: bb_query_query_results
*l_query_media.isCompound: "true"
*l_query_media.compoundIcon: "label.xpm"
*l_query_media.compoundName: "label_"
*l_query_media.x: 156
*l_query_media.y: 8
*l_query_media.width: 68
*l_query_media.height: 32
*l_query_media.labelString: "Media ID"

*l_query_mode.class: label
*l_query_mode.static: true
*l_query_mode.name: l_query_mode
*l_query_mode.parent: bb_query_query_results
*l_query_mode.isCompound: "true"
*l_query_mode.compoundIcon: "label.xpm"
*l_query_mode.compoundName: "label_"
*l_query_mode.x: 238
*l_query_mode.y: 8
*l_query_mode.width: 56
*l_query_mode.height: 32
*l_query_mode.labelString: "Mode"

*l_query_sat_sens_rev.class: label
*l_query_sat_sens_rev.static: true
*l_query_sat_sens_rev.name: l_query_sat_sens_rev
*l_query_sat_sens_rev.parent: bb_query_query_results
*l_query_sat_sens_rev.isCompound: "true"
*l_query_sat_sens_rev.compoundIcon: "label.xpm"
*l_query_sat_sens_rev.compoundName: "label_"
*l_query_sat_sens_rev.x: 304
*l_query_sat_sens_rev.y: 8
*l_query_sat_sens_rev.width: 88
*l_query_sat_sens_rev.height: 32
*l_query_sat_sens_rev.labelString: "Datatake ID"

*l_query_frame.class: label
*l_query_frame.static: true
*l_query_frame.name: l_query_frame
*l_query_frame.parent: bb_query_query_results
*l_query_frame.isCompound: "true"
*l_query_frame.compoundIcon: "label.xpm"
*l_query_frame.compoundName: "label_"
*l_query_frame.x: 420
*l_query_frame.y: 10
*l_query_frame.width: 48
*l_query_frame.height: 32
*l_query_frame.labelString: "Frame\nID"

*l_query_job.class: label
*l_query_job.static: true
*l_query_job.name: l_query_job
*l_query_job.parent: bb_query_query_results
*l_query_job.isCompound: "true"
*l_query_job.compoundIcon: "label.xpm"
*l_query_job.compoundName: "label_"
*l_query_job.x: 470
*l_query_job.y: 10
*l_query_job.width: 62
*l_query_job.height: 32
*l_query_job.labelString: "Job ID"

*l_query_order.class: label
*l_query_order.static: true
*l_query_order.name: l_query_order
*l_query_order.parent: bb_query_query_results
*l_query_order.isCompound: "true"
*l_query_order.compoundIcon: "label.xpm"
*l_query_order.compoundName: "label_"
*l_query_order.x: 540
*l_query_order.y: 10
*l_query_order.width: 110
*l_query_order.height: 30
*l_query_order.labelString: "Order & Item ID"

*l_query_state.class: label
*l_query_state.static: true
*l_query_state.name: l_query_state
*l_query_state.parent: bb_query_query_results
*l_query_state.isCompound: "true"
*l_query_state.compoundIcon: "label.xpm"
*l_query_state.compoundName: "label_"
*l_query_state.x: 670
*l_query_state.y: 10
*l_query_state.width: 64
*l_query_state.height: 32
*l_query_state.labelString: "State"

*l_query_age.class: label
*l_query_age.static: true
*l_query_age.name: l_query_age
*l_query_age.parent: bb_query_query_results
*l_query_age.isCompound: "true"
*l_query_age.compoundIcon: "label.xpm"
*l_query_age.compoundName: "label_"
*l_query_age.x: 750
*l_query_age.y: 10
*l_query_age.width: 48
*l_query_age.height: 32
*l_query_age.labelString: "Age\n(Days)"

*l_query_order_type.class: label
*l_query_order_type.static: true
*l_query_order_type.name: l_query_order_type
*l_query_order_type.parent: bb_query_query_results
*l_query_order_type.isCompound: "true"
*l_query_order_type.compoundIcon: "label.xpm"
*l_query_order_type.compoundName: "label_"
*l_query_order_type.x: 4
*l_query_order_type.y: 8
*l_query_order_type.width: 68
*l_query_order_type.height: 32
*l_query_order_type.labelString: "Order\nType"

*scrolledWindowList2.class: scrolledWindow
*scrolledWindowList2.static: true
*scrolledWindowList2.name: scrolledWindowList2
*scrolledWindowList2.parent: bb_query_query_results
*scrolledWindowList2.scrollingPolicy: "application_defined"
*scrolledWindowList2.visualPolicy: "variable"
*scrolledWindowList2.scrollBarDisplayPolicy: "static"
*scrolledWindowList2.shadowThickness: 0
*scrolledWindowList2.isCompound: "true"
*scrolledWindowList2.compoundIcon: "scrllist.xpm"
*scrolledWindowList2.compoundName: "scrolled_List"
*scrolledWindowList2.x: 3
*scrolledWindowList2.y: 46

*sw_query_results_list.class: scrolledList
*sw_query_results_list.name.source: public
*sw_query_results_list.static: false
*sw_query_results_list.name: sw_query_results_list
*sw_query_results_list.parent: scrolledWindowList2
*sw_query_results_list.width: 1090
*sw_query_results_list.height: 300
*sw_query_results_list.scrollBarDisplayPolicy: "static"
*sw_query_results_list.listSizePolicy: "constant"
*sw_query_results_list.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1"
*sw_query_results_list.selectionPolicy: "single_select"

*l_query_tce.class: label
*l_query_tce.static: true
*l_query_tce.name: l_query_tce
*l_query_tce.parent: bb_query_query_results
*l_query_tce.isCompound: "true"
*l_query_tce.compoundIcon: "label.xpm"
*l_query_tce.compoundName: "label_"
*l_query_tce.x: 820
*l_query_tce.y: 8
*l_query_tce.width: 40
*l_query_tce.height: 32
*l_query_tce.labelString: "TCE"

*l_query_gha.class: label
*l_query_gha.static: true
*l_query_gha.name: l_query_gha
*l_query_gha.parent: bb_query_query_results
*l_query_gha.isCompound: "true"
*l_query_gha.compoundIcon: "label.xpm"
*l_query_gha.compoundName: "label_"
*l_query_gha.x: 870
*l_query_gha.y: 8
*l_query_gha.width: 40
*l_query_gha.height: 32
*l_query_gha.labelString: "GHA"

*l_query_sv.class: label
*l_query_sv.static: true
*l_query_sv.name: l_query_sv
*l_query_sv.parent: bb_query_query_results
*l_query_sv.isCompound: "true"
*l_query_sv.compoundIcon: "label.xpm"
*l_query_sv.compoundName: "label_"
*l_query_sv.x: 920
*l_query_sv.y: 8
*l_query_sv.width: 40
*l_query_sv.height: 32
*l_query_sv.labelString: "State\nVector"

*l_query_scan_results.class: label
*l_query_scan_results.static: true
*l_query_scan_results.name: l_query_scan_results
*l_query_scan_results.parent: bb_query_query_results
*l_query_scan_results.isCompound: "true"
*l_query_scan_results.compoundIcon: "label.xpm"
*l_query_scan_results.compoundName: "label_"
*l_query_scan_results.x: 960
*l_query_scan_results.y: 8
*l_query_scan_results.width: 72
*l_query_scan_results.height: 32
*l_query_scan_results.labelString: "Scan\nResults"
*l_query_scan_results.recomputeSize: "true"

*l_query_calib_params.class: label
*l_query_calib_params.static: true
*l_query_calib_params.name: l_query_calib_params
*l_query_calib_params.parent: bb_query_query_results
*l_query_calib_params.isCompound: "true"
*l_query_calib_params.compoundIcon: "label.xpm"
*l_query_calib_params.compoundName: "label_"
*l_query_calib_params.x: 1030
*l_query_calib_params.y: 8
*l_query_calib_params.width: 69
*l_query_calib_params.height: 32
*l_query_calib_params.labelString: "Calib\nParams"
*l_query_calib_params.recomputeSize: "true"

*l_query_query_results.class: label
*l_query_query_results.static: true
*l_query_query_results.name: l_query_query_results
*l_query_query_results.parent: pps_query
*l_query_query_results.isCompound: "true"
*l_query_query_results.compoundIcon: "label.xpm"
*l_query_query_results.compoundName: "label_"
*l_query_query_results.x: 10
*l_query_query_results.y: 370
*l_query_query_results.width: 108
*l_query_query_results.height: 30
*l_query_query_results.labelString: "Query Results"

*f_query_items_found.class: frame
*f_query_items_found.static: true
*f_query_items_found.name: f_query_items_found
*f_query_items_found.parent: pps_query
*f_query_items_found.width: 142
*f_query_items_found.height: 34
*f_query_items_found.isCompound: "true"
*f_query_items_found.compoundIcon: "frame.xpm"
*f_query_items_found.compoundName: "frame_"
*f_query_items_found.x: 118
*f_query_items_found.y: 370

*bb_query_items_found.class: bulletinBoard
*bb_query_items_found.static: true
*bb_query_items_found.name: bb_query_items_found
*bb_query_items_found.parent: f_query_items_found
*bb_query_items_found.resizePolicy: "resize_none"
*bb_query_items_found.width: 132
*bb_query_items_found.height: 28
*bb_query_items_found.isCompound: "true"
*bb_query_items_found.compoundIcon: "bboard.xpm"
*bb_query_items_found.compoundName: "bulletin_Board"
*bb_query_items_found.x: 2
*bb_query_items_found.y: 2
*bb_query_items_found.marginHeight: 0
*bb_query_items_found.marginWidth: 0

*l_query_items_found.class: label
*l_query_items_found.static: true
*l_query_items_found.name: l_query_items_found
*l_query_items_found.parent: bb_query_items_found
*l_query_items_found.isCompound: "true"
*l_query_items_found.compoundIcon: "label.xpm"
*l_query_items_found.compoundName: "label_"
*l_query_items_found.x: 0
*l_query_items_found.y: 0
*l_query_items_found.width: 80
*l_query_items_found.height: 32
*l_query_items_found.labelString: "Items Found"

*l_query_num_items.class: label
*l_query_num_items.name.source: public
*l_query_num_items.static: false
*l_query_num_items.name: l_query_num_items
*l_query_num_items.parent: bb_query_items_found
*l_query_num_items.isCompound: "true"
*l_query_num_items.compoundIcon: "label.xpm"
*l_query_num_items.compoundName: "label_"
*l_query_num_items.x: 90
*l_query_num_items.y: 6
*l_query_num_items.width: 43
*l_query_num_items.height: 20
*l_query_num_items.labelString: " 999999"
*l_query_num_items.alignment: "alignment_end"
*l_query_num_items.recomputeSize: "false"

*l_query_query_settings.class: label
*l_query_query_settings.static: true
*l_query_query_settings.name: l_query_query_settings
*l_query_query_settings.parent: pps_query
*l_query_query_settings.isCompound: "true"
*l_query_query_settings.compoundIcon: "label.xpm"
*l_query_query_settings.compoundName: "label_"
*l_query_query_settings.x: 8
*l_query_query_settings.y: 40
*l_query_query_settings.width: 116
*l_query_query_settings.height: 30
*l_query_query_settings.labelString: "Query Settings"

*pb_query_query.class: pushButton
*pb_query_query.name.source: public
*pb_query_query.static: false
*pb_query_query.name: pb_query_query
*pb_query_query.parent: pps_query
*pb_query_query.isCompound: "true"
*pb_query_query.compoundIcon: "push.xpm"
*pb_query_query.compoundName: "push_Button"
*pb_query_query.x: 568
*pb_query_query.y: 360
*pb_query_query.width: 80
*pb_query_query.height: 40
*pb_query_query.labelString: "Query"
*pb_query_query.activateCallback: {\
do_query_query();\
}
*pb_query_query.fontList: "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"
*pb_query_query.sensitive: "false"
*pb_query_query.showAsDefault: 1

*frame2.class: frame
*frame2.static: true
*frame2.name: frame2
*frame2.parent: pps_query
*frame2.width: 228
*frame2.height: 290
*frame2.isCompound: "true"
*frame2.compoundIcon: "frame.xpm"
*frame2.compoundName: "frame_"
*frame2.x: 894
*frame2.y: 70

*bulletinBoard1.class: bulletinBoard
*bulletinBoard1.static: true
*bulletinBoard1.name: bulletinBoard1
*bulletinBoard1.parent: frame2
*bulletinBoard1.resizePolicy: "resize_none"
*bulletinBoard1.width: 250
*bulletinBoard1.height: 286
*bulletinBoard1.isCompound: "true"
*bulletinBoard1.compoundIcon: "bboard.xpm"
*bulletinBoard1.compoundName: "bulletin_Board"
*bulletinBoard1.x: -47
*bulletinBoard1.y: 0

*om_query_order_first.class: rowColumn
*om_query_order_first.name.source: public
*om_query_order_first.static: false
*om_query_order_first.name: om_query_order_first
*om_query_order_first.parent: bulletinBoard1
*om_query_order_first.rowColumnType: "menu_option"
*om_query_order_first.subMenuId: "rc_query_order_first"
*om_query_order_first.isCompound: "true"
*om_query_order_first.compoundIcon: "optionM.xpm"
*om_query_order_first.compoundName: "option_Menu"
*om_query_order_first.x: 12
*om_query_order_first.y: 37
*om_query_order_first.width: 60
*om_query_order_first.height: 54
*om_query_order_first.labelString: "First    "
*om_query_order_first.sensitive: "false"

*rc_query_order_first.class: rowColumn
*rc_query_order_first.name.source: public
*rc_query_order_first.static: false
*rc_query_order_first.name: rc_query_order_first
*rc_query_order_first.parent: om_query_order_first
*rc_query_order_first.rowColumnType: "menu_pulldown"
*rc_query_order_first.sensitive: "false"
*rc_query_order_first.x: 93
*rc_query_order_first.y: 0

*pb_query_order_first_none.class: pushButton
*pb_query_order_first_none.name.source: public
*pb_query_order_first_none.static: false
*pb_query_order_first_none.name: pb_query_order_first_none
*pb_query_order_first_none.parent: rc_query_order_first
*pb_query_order_first_none.labelString: "None"
*pb_query_order_first_none.sensitive: "false"
*pb_query_order_first_none.activateCallback.source: public
*pb_query_order_first_none.activateCallback: 
*pb_query_order_first_none.x: 97
*pb_query_order_first_none.y: 2

*pb_query_order_first_order_type.class: pushButton
*pb_query_order_first_order_type.name.source: public
*pb_query_order_first_order_type.static: false
*pb_query_order_first_order_type.name: pb_query_order_first_order_type
*pb_query_order_first_order_type.parent: rc_query_order_first
*pb_query_order_first_order_type.labelString: "Order Type"
*pb_query_order_first_order_type.x: 97
*pb_query_order_first_order_type.y: 24

*pb_query_order_first_priority.class: pushButton
*pb_query_order_first_priority.name.source: public
*pb_query_order_first_priority.static: false
*pb_query_order_first_priority.name: pb_query_order_first_priority
*pb_query_order_first_priority.parent: rc_query_order_first
*pb_query_order_first_priority.labelString: "Priority"
*pb_query_order_first_priority.x: 97
*pb_query_order_first_priority.y: 46

*pb_query_order_first_media_id.class: pushButton
*pb_query_order_first_media_id.name.source: public
*pb_query_order_first_media_id.static: false
*pb_query_order_first_media_id.name: pb_query_order_first_media_id
*pb_query_order_first_media_id.parent: rc_query_order_first
*pb_query_order_first_media_id.labelString: "Media ID"
*pb_query_order_first_media_id.x: 97
*pb_query_order_first_media_id.y: 68

*pb_query_order_first_mode.class: pushButton
*pb_query_order_first_mode.name.source: public
*pb_query_order_first_mode.static: false
*pb_query_order_first_mode.name: pb_query_order_first_mode
*pb_query_order_first_mode.parent: rc_query_order_first
*pb_query_order_first_mode.labelString: "Mode"
*pb_query_order_first_mode.x: 97
*pb_query_order_first_mode.y: 90

*pb_query_order_first_sat_sens_rev.class: pushButton
*pb_query_order_first_sat_sens_rev.name.source: public
*pb_query_order_first_sat_sens_rev.static: false
*pb_query_order_first_sat_sens_rev.name: pb_query_order_first_sat_sens_rev
*pb_query_order_first_sat_sens_rev.parent: rc_query_order_first
*pb_query_order_first_sat_sens_rev.labelString: "Datatake ID"
*pb_query_order_first_sat_sens_rev.x: 97
*pb_query_order_first_sat_sens_rev.y: 112

*pb_query_order_first_frame_subframe.class: pushButton
*pb_query_order_first_frame_subframe.name.source: public
*pb_query_order_first_frame_subframe.static: false
*pb_query_order_first_frame_subframe.name: pb_query_order_first_frame_subframe
*pb_query_order_first_frame_subframe.parent: rc_query_order_first
*pb_query_order_first_frame_subframe.labelString: "Frame ID"
*pb_query_order_first_frame_subframe.activateCallback.source: public
*pb_query_order_first_frame_subframe.activateCallback: 
*pb_query_order_first_frame_subframe.x: 97
*pb_query_order_first_frame_subframe.y: 134

*pb_query_order_first_job_id.class: pushButton
*pb_query_order_first_job_id.name.source: public
*pb_query_order_first_job_id.static: false
*pb_query_order_first_job_id.name: pb_query_order_first_job_id
*pb_query_order_first_job_id.parent: rc_query_order_first
*pb_query_order_first_job_id.labelString: "Job ID"
*pb_query_order_first_job_id.activateCallback.source: public
*pb_query_order_first_job_id.activateCallback: 
*pb_query_order_first_job_id.x: 97
*pb_query_order_first_job_id.y: 156

*pb_query_order_first_order_item.class: pushButton
*pb_query_order_first_order_item.name.source: public
*pb_query_order_first_order_item.static: false
*pb_query_order_first_order_item.name: pb_query_order_first_order_item
*pb_query_order_first_order_item.parent: rc_query_order_first
*pb_query_order_first_order_item.labelString: "Order & Item ID"
*pb_query_order_first_order_item.activateCallback.source: public
*pb_query_order_first_order_item.activateCallback: 
*pb_query_order_first_order_item.x: 97
*pb_query_order_first_order_item.y: 178

*pb_query_order_first_state.class: pushButton
*pb_query_order_first_state.name.source: public
*pb_query_order_first_state.static: false
*pb_query_order_first_state.name: pb_query_order_first_state
*pb_query_order_first_state.parent: rc_query_order_first
*pb_query_order_first_state.labelString: "State"
*pb_query_order_first_state.activateCallback.source: public
*pb_query_order_first_state.activateCallback: 
*pb_query_order_first_state.x: 97
*pb_query_order_first_state.y: 200

*pb_query_order_first_age.class: pushButton
*pb_query_order_first_age.static: true
*pb_query_order_first_age.name: pb_query_order_first_age
*pb_query_order_first_age.parent: rc_query_order_first
*pb_query_order_first_age.labelString: "Age (Days)"
*pb_query_order_first_age.x: 97
*pb_query_order_first_age.y: 222
*pb_query_order_first_age.activateCallback.source: public
*pb_query_order_first_age.activateCallback: 

*om_query_order_second.class: rowColumn
*om_query_order_second.name.source: public
*om_query_order_second.static: false
*om_query_order_second.name: om_query_order_second
*om_query_order_second.parent: bulletinBoard1
*om_query_order_second.rowColumnType: "menu_option"
*om_query_order_second.subMenuId: "rc_query_order_second"
*om_query_order_second.isCompound: "true"
*om_query_order_second.compoundIcon: "optionM.xpm"
*om_query_order_second.compoundName: "option_Menu"
*om_query_order_second.x: 11
*om_query_order_second.y: 90
*om_query_order_second.width: 60
*om_query_order_second.height: 54
*om_query_order_second.labelString: "Second"
*om_query_order_second.sensitive: "false"

*rc_query_order_second.class: rowColumn
*rc_query_order_second.name.source: public
*rc_query_order_second.static: false
*rc_query_order_second.name: rc_query_order_second
*rc_query_order_second.parent: om_query_order_second
*rc_query_order_second.rowColumnType: "menu_pulldown"
*rc_query_order_second.sensitive: "false"
*rc_query_order_second.x: 93
*rc_query_order_second.y: 0

*pb_query_order_second_none.class: pushButton
*pb_query_order_second_none.name.source: public
*pb_query_order_second_none.static: false
*pb_query_order_second_none.name: pb_query_order_second_none
*pb_query_order_second_none.parent: rc_query_order_second
*pb_query_order_second_none.labelString: "None"
*pb_query_order_second_none.sensitive: "false"
*pb_query_order_second_none.x: 97
*pb_query_order_second_none.y: 2
*pb_query_order_second_none.activateCallback.source: public
*pb_query_order_second_none.activateCallback: 

*pb_query_order_second_order_type.class: pushButton
*pb_query_order_second_order_type.name.source: public
*pb_query_order_second_order_type.static: false
*pb_query_order_second_order_type.name: pb_query_order_second_order_type
*pb_query_order_second_order_type.parent: rc_query_order_second
*pb_query_order_second_order_type.labelString: "Order Type"
*pb_query_order_second_order_type.x: 97
*pb_query_order_second_order_type.y: 24
*pb_query_order_second_order_type.activateCallback.source: public
*pb_query_order_second_order_type.activateCallback: 

*pb_query_order_second_priority.class: pushButton
*pb_query_order_second_priority.name.source: public
*pb_query_order_second_priority.static: false
*pb_query_order_second_priority.name: pb_query_order_second_priority
*pb_query_order_second_priority.parent: rc_query_order_second
*pb_query_order_second_priority.labelString: "Priority"
*pb_query_order_second_priority.x: 97
*pb_query_order_second_priority.y: 46
*pb_query_order_second_priority.activateCallback.source: public
*pb_query_order_second_priority.activateCallback: 

*pb_query_order_second_media_id.class: pushButton
*pb_query_order_second_media_id.name.source: public
*pb_query_order_second_media_id.static: false
*pb_query_order_second_media_id.name: pb_query_order_second_media_id
*pb_query_order_second_media_id.parent: rc_query_order_second
*pb_query_order_second_media_id.labelString: "Media ID"
*pb_query_order_second_media_id.x: 97
*pb_query_order_second_media_id.y: 68
*pb_query_order_second_media_id.activateCallback.source: public
*pb_query_order_second_media_id.activateCallback: 

*pb_query_order_second_mode.class: pushButton
*pb_query_order_second_mode.name.source: public
*pb_query_order_second_mode.static: false
*pb_query_order_second_mode.name: pb_query_order_second_mode
*pb_query_order_second_mode.parent: rc_query_order_second
*pb_query_order_second_mode.labelString: "Mode"
*pb_query_order_second_mode.x: 97
*pb_query_order_second_mode.y: 90
*pb_query_order_second_mode.activateCallback.source: public
*pb_query_order_second_mode.activateCallback: 

*pb_query_order_second_sat_sens_rev.class: pushButton
*pb_query_order_second_sat_sens_rev.name.source: public
*pb_query_order_second_sat_sens_rev.static: false
*pb_query_order_second_sat_sens_rev.name: pb_query_order_second_sat_sens_rev
*pb_query_order_second_sat_sens_rev.parent: rc_query_order_second
*pb_query_order_second_sat_sens_rev.labelString: "Datatake ID"
*pb_query_order_second_sat_sens_rev.x: 97
*pb_query_order_second_sat_sens_rev.y: 112

*pb_query_order_second_frame_subframe.class: pushButton
*pb_query_order_second_frame_subframe.name.source: public
*pb_query_order_second_frame_subframe.static: false
*pb_query_order_second_frame_subframe.name: pb_query_order_second_frame_subframe
*pb_query_order_second_frame_subframe.parent: rc_query_order_second
*pb_query_order_second_frame_subframe.labelString: "Frame ID"
*pb_query_order_second_frame_subframe.x: 97
*pb_query_order_second_frame_subframe.y: 134
*pb_query_order_second_frame_subframe.activateCallback.source: public
*pb_query_order_second_frame_subframe.activateCallback: 

*pb_query_order_second_job_id.class: pushButton
*pb_query_order_second_job_id.name.source: public
*pb_query_order_second_job_id.static: false
*pb_query_order_second_job_id.name: pb_query_order_second_job_id
*pb_query_order_second_job_id.parent: rc_query_order_second
*pb_query_order_second_job_id.labelString: "Job ID"
*pb_query_order_second_job_id.x: 97
*pb_query_order_second_job_id.y: 156
*pb_query_order_second_job_id.activateCallback.source: public
*pb_query_order_second_job_id.activateCallback: 

*pb_query_order_second_order_item.class: pushButton
*pb_query_order_second_order_item.name.source: public
*pb_query_order_second_order_item.static: false
*pb_query_order_second_order_item.name: pb_query_order_second_order_item
*pb_query_order_second_order_item.parent: rc_query_order_second
*pb_query_order_second_order_item.labelString: "Order & Item ID"
*pb_query_order_second_order_item.x: 97
*pb_query_order_second_order_item.y: 178
*pb_query_order_second_order_item.activateCallback.source: public
*pb_query_order_second_order_item.activateCallback: 

*pb_query_order_second_state.class: pushButton
*pb_query_order_second_state.name.source: public
*pb_query_order_second_state.static: false
*pb_query_order_second_state.name: pb_query_order_second_state
*pb_query_order_second_state.parent: rc_query_order_second
*pb_query_order_second_state.labelString: "State"
*pb_query_order_second_state.x: 97
*pb_query_order_second_state.y: 200
*pb_query_order_second_state.activateCallback.source: public
*pb_query_order_second_state.activateCallback: 

*pb_query_order_second_age.class: pushButton
*pb_query_order_second_age.static: true
*pb_query_order_second_age.name: pb_query_order_second_age
*pb_query_order_second_age.parent: rc_query_order_second
*pb_query_order_second_age.labelString: "Age (Days)"
*pb_query_order_second_age.x: 97
*pb_query_order_second_age.y: 222
*pb_query_order_second_age.activateCallback.source: public
*pb_query_order_second_age.activateCallback: 

*om_query_order_third.class: rowColumn
*om_query_order_third.name.source: public
*om_query_order_third.static: false
*om_query_order_third.name: om_query_order_third
*om_query_order_third.parent: bulletinBoard1
*om_query_order_third.rowColumnType: "menu_option"
*om_query_order_third.subMenuId: "rc_query_order_third"
*om_query_order_third.isCompound: "true"
*om_query_order_third.compoundIcon: "optionM.xpm"
*om_query_order_third.compoundName: "option_Menu"
*om_query_order_third.x: 11
*om_query_order_third.y: 143
*om_query_order_third.width: 60
*om_query_order_third.height: 54
*om_query_order_third.labelString: "Third    "
*om_query_order_third.sensitive: "false"

*rc_query_order_third.class: rowColumn
*rc_query_order_third.name.source: public
*rc_query_order_third.static: false
*rc_query_order_third.name: rc_query_order_third
*rc_query_order_third.parent: om_query_order_third
*rc_query_order_third.rowColumnType: "menu_pulldown"
*rc_query_order_third.sensitive: "false"
*rc_query_order_third.x: 93
*rc_query_order_third.y: 0

*pb_query_order_third_none.class: pushButton
*pb_query_order_third_none.name.source: public
*pb_query_order_third_none.static: false
*pb_query_order_third_none.name: pb_query_order_third_none
*pb_query_order_third_none.parent: rc_query_order_third
*pb_query_order_third_none.labelString: "None"
*pb_query_order_third_none.sensitive: "false"
*pb_query_order_third_none.x: 97
*pb_query_order_third_none.y: 2
*pb_query_order_third_none.activateCallback.source: public
*pb_query_order_third_none.activateCallback: 

*pb_query_order_third_order_type.class: pushButton
*pb_query_order_third_order_type.name.source: public
*pb_query_order_third_order_type.static: false
*pb_query_order_third_order_type.name: pb_query_order_third_order_type
*pb_query_order_third_order_type.parent: rc_query_order_third
*pb_query_order_third_order_type.labelString: "Order Type"
*pb_query_order_third_order_type.x: 97
*pb_query_order_third_order_type.y: 24
*pb_query_order_third_order_type.activateCallback.source: public
*pb_query_order_third_order_type.activateCallback: 

*pb_query_order_third_priority.class: pushButton
*pb_query_order_third_priority.name.source: public
*pb_query_order_third_priority.static: false
*pb_query_order_third_priority.name: pb_query_order_third_priority
*pb_query_order_third_priority.parent: rc_query_order_third
*pb_query_order_third_priority.labelString: "Priority"
*pb_query_order_third_priority.x: 97
*pb_query_order_third_priority.y: 46
*pb_query_order_third_priority.activateCallback.source: public
*pb_query_order_third_priority.activateCallback: 

*pb_query_order_third_media_id.class: pushButton
*pb_query_order_third_media_id.name.source: public
*pb_query_order_third_media_id.static: false
*pb_query_order_third_media_id.name: pb_query_order_third_media_id
*pb_query_order_third_media_id.parent: rc_query_order_third
*pb_query_order_third_media_id.labelString: "Media ID"
*pb_query_order_third_media_id.x: 97
*pb_query_order_third_media_id.y: 68
*pb_query_order_third_media_id.activateCallback.source: public
*pb_query_order_third_media_id.activateCallback: 

*pb_query_order_third_mode.class: pushButton
*pb_query_order_third_mode.name.source: public
*pb_query_order_third_mode.static: false
*pb_query_order_third_mode.name: pb_query_order_third_mode
*pb_query_order_third_mode.parent: rc_query_order_third
*pb_query_order_third_mode.labelString: "Mode"
*pb_query_order_third_mode.x: 97
*pb_query_order_third_mode.y: 90
*pb_query_order_third_mode.activateCallback.source: public
*pb_query_order_third_mode.activateCallback: 

*pb_query_order_third_sat_sens_rev.class: pushButton
*pb_query_order_third_sat_sens_rev.name.source: public
*pb_query_order_third_sat_sens_rev.static: false
*pb_query_order_third_sat_sens_rev.name: pb_query_order_third_sat_sens_rev
*pb_query_order_third_sat_sens_rev.parent: rc_query_order_third
*pb_query_order_third_sat_sens_rev.labelString: "Datatake ID"
*pb_query_order_third_sat_sens_rev.x: 97
*pb_query_order_third_sat_sens_rev.y: 112

*pb_query_order_third_frame_subframe.class: pushButton
*pb_query_order_third_frame_subframe.name.source: public
*pb_query_order_third_frame_subframe.static: false
*pb_query_order_third_frame_subframe.name: pb_query_order_third_frame_subframe
*pb_query_order_third_frame_subframe.parent: rc_query_order_third
*pb_query_order_third_frame_subframe.labelString: "Frame ID"
*pb_query_order_third_frame_subframe.x: 97
*pb_query_order_third_frame_subframe.y: 134
*pb_query_order_third_frame_subframe.activateCallback.source: public
*pb_query_order_third_frame_subframe.activateCallback: 

*pb_query_order_third_job_id.class: pushButton
*pb_query_order_third_job_id.name.source: public
*pb_query_order_third_job_id.static: false
*pb_query_order_third_job_id.name: pb_query_order_third_job_id
*pb_query_order_third_job_id.parent: rc_query_order_third
*pb_query_order_third_job_id.labelString: "Job ID"
*pb_query_order_third_job_id.x: 97
*pb_query_order_third_job_id.y: 156
*pb_query_order_third_job_id.activateCallback.source: public
*pb_query_order_third_job_id.activateCallback: 

*pb_query_order_third_order_item.class: pushButton
*pb_query_order_third_order_item.name.source: public
*pb_query_order_third_order_item.static: false
*pb_query_order_third_order_item.name: pb_query_order_third_order_item
*pb_query_order_third_order_item.parent: rc_query_order_third
*pb_query_order_third_order_item.labelString: "Order & Item ID"
*pb_query_order_third_order_item.x: 97
*pb_query_order_third_order_item.y: 178
*pb_query_order_third_order_item.activateCallback.source: public
*pb_query_order_third_order_item.activateCallback: 

*pb_query_order_third_state.class: pushButton
*pb_query_order_third_state.name.source: public
*pb_query_order_third_state.static: false
*pb_query_order_third_state.name: pb_query_order_third_state
*pb_query_order_third_state.parent: rc_query_order_third
*pb_query_order_third_state.labelString: "State"
*pb_query_order_third_state.x: 97
*pb_query_order_third_state.y: 200
*pb_query_order_third_state.activateCallback.source: public
*pb_query_order_third_state.activateCallback: 

*pb_query_order_third_age.class: pushButton
*pb_query_order_third_age.static: true
*pb_query_order_third_age.name: pb_query_order_third_age
*pb_query_order_third_age.parent: rc_query_order_third
*pb_query_order_third_age.labelString: "Age (Days)"
*pb_query_order_third_age.x: 97
*pb_query_order_third_age.y: 222
*pb_query_order_third_age.activateCallback.source: public
*pb_query_order_third_age.activateCallback: 

*om_query_order_fourth.class: rowColumn
*om_query_order_fourth.name.source: public
*om_query_order_fourth.static: false
*om_query_order_fourth.name: om_query_order_fourth
*om_query_order_fourth.parent: bulletinBoard1
*om_query_order_fourth.rowColumnType: "menu_option"
*om_query_order_fourth.subMenuId: "rc_query_order_fourth"
*om_query_order_fourth.isCompound: "true"
*om_query_order_fourth.compoundIcon: "optionM.xpm"
*om_query_order_fourth.compoundName: "option_Menu"
*om_query_order_fourth.x: 11
*om_query_order_fourth.y: 198
*om_query_order_fourth.width: 60
*om_query_order_fourth.height: 54
*om_query_order_fourth.labelString: "Fourth  "
*om_query_order_fourth.sensitive: "false"

*rc_query_order_fourth.class: rowColumn
*rc_query_order_fourth.name.source: public
*rc_query_order_fourth.static: false
*rc_query_order_fourth.name: rc_query_order_fourth
*rc_query_order_fourth.parent: om_query_order_fourth
*rc_query_order_fourth.rowColumnType: "menu_pulldown"
*rc_query_order_fourth.sensitive: "false"
*rc_query_order_fourth.x: 93
*rc_query_order_fourth.y: 0

*pb_query_order_fourth_none.class: pushButton
*pb_query_order_fourth_none.name.source: public
*pb_query_order_fourth_none.static: false
*pb_query_order_fourth_none.name: pb_query_order_fourth_none
*pb_query_order_fourth_none.parent: rc_query_order_fourth
*pb_query_order_fourth_none.labelString: "None"
*pb_query_order_fourth_none.sensitive: "false"
*pb_query_order_fourth_none.activateCallback.source: public
*pb_query_order_fourth_none.activateCallback: 
*pb_query_order_fourth_none.x: 97
*pb_query_order_fourth_none.y: 2

*pb_query_order_fourth_order_type.class: pushButton
*pb_query_order_fourth_order_type.name.source: public
*pb_query_order_fourth_order_type.static: false
*pb_query_order_fourth_order_type.name: pb_query_order_fourth_order_type
*pb_query_order_fourth_order_type.parent: rc_query_order_fourth
*pb_query_order_fourth_order_type.labelString: "Order Type"
*pb_query_order_fourth_order_type.activateCallback.source: public
*pb_query_order_fourth_order_type.activateCallback: 
*pb_query_order_fourth_order_type.x: 97
*pb_query_order_fourth_order_type.y: 24

*pb_query_order_fourth_priority.class: pushButton
*pb_query_order_fourth_priority.name.source: public
*pb_query_order_fourth_priority.static: false
*pb_query_order_fourth_priority.name: pb_query_order_fourth_priority
*pb_query_order_fourth_priority.parent: rc_query_order_fourth
*pb_query_order_fourth_priority.labelString: "Priority"
*pb_query_order_fourth_priority.activateCallback.source: public
*pb_query_order_fourth_priority.activateCallback: 
*pb_query_order_fourth_priority.x: 97
*pb_query_order_fourth_priority.y: 46

*pb_query_order_fourth_media_id.class: pushButton
*pb_query_order_fourth_media_id.name.source: public
*pb_query_order_fourth_media_id.static: false
*pb_query_order_fourth_media_id.name: pb_query_order_fourth_media_id
*pb_query_order_fourth_media_id.parent: rc_query_order_fourth
*pb_query_order_fourth_media_id.labelString: "Media ID"
*pb_query_order_fourth_media_id.activateCallback.source: public
*pb_query_order_fourth_media_id.activateCallback: 
*pb_query_order_fourth_media_id.x: 97
*pb_query_order_fourth_media_id.y: 68

*pb_query_order_fourth_mode.class: pushButton
*pb_query_order_fourth_mode.name.source: public
*pb_query_order_fourth_mode.static: false
*pb_query_order_fourth_mode.name: pb_query_order_fourth_mode
*pb_query_order_fourth_mode.parent: rc_query_order_fourth
*pb_query_order_fourth_mode.labelString: "Mode"
*pb_query_order_fourth_mode.activateCallback.source: public
*pb_query_order_fourth_mode.activateCallback: 
*pb_query_order_fourth_mode.x: 97
*pb_query_order_fourth_mode.y: 90

*pb_query_order_fourth_sat_sens_rev.class: pushButton
*pb_query_order_fourth_sat_sens_rev.name.source: public
*pb_query_order_fourth_sat_sens_rev.static: false
*pb_query_order_fourth_sat_sens_rev.name: pb_query_order_fourth_sat_sens_rev
*pb_query_order_fourth_sat_sens_rev.parent: rc_query_order_fourth
*pb_query_order_fourth_sat_sens_rev.labelString: "Datatake ID"
*pb_query_order_fourth_sat_sens_rev.x: 97
*pb_query_order_fourth_sat_sens_rev.y: 112

*pb_query_order_fourth_frame_subframe.class: pushButton
*pb_query_order_fourth_frame_subframe.name.source: public
*pb_query_order_fourth_frame_subframe.static: false
*pb_query_order_fourth_frame_subframe.name: pb_query_order_fourth_frame_subframe
*pb_query_order_fourth_frame_subframe.parent: rc_query_order_fourth
*pb_query_order_fourth_frame_subframe.labelString: "Frame ID"
*pb_query_order_fourth_frame_subframe.activateCallback.source: public
*pb_query_order_fourth_frame_subframe.activateCallback: 
*pb_query_order_fourth_frame_subframe.x: 97
*pb_query_order_fourth_frame_subframe.y: 134

*pb_query_order_fourth_job_id.class: pushButton
*pb_query_order_fourth_job_id.name.source: public
*pb_query_order_fourth_job_id.static: false
*pb_query_order_fourth_job_id.name: pb_query_order_fourth_job_id
*pb_query_order_fourth_job_id.parent: rc_query_order_fourth
*pb_query_order_fourth_job_id.labelString: "Job ID"
*pb_query_order_fourth_job_id.activateCallback.source: public
*pb_query_order_fourth_job_id.activateCallback: 
*pb_query_order_fourth_job_id.x: 97
*pb_query_order_fourth_job_id.y: 156

*pb_query_order_fourth_order_item.class: pushButton
*pb_query_order_fourth_order_item.name.source: public
*pb_query_order_fourth_order_item.static: false
*pb_query_order_fourth_order_item.name: pb_query_order_fourth_order_item
*pb_query_order_fourth_order_item.parent: rc_query_order_fourth
*pb_query_order_fourth_order_item.labelString: "Order & Item ID"
*pb_query_order_fourth_order_item.activateCallback.source: public
*pb_query_order_fourth_order_item.activateCallback: 
*pb_query_order_fourth_order_item.x: 97
*pb_query_order_fourth_order_item.y: 178

*pb_query_order_fourth_state.class: pushButton
*pb_query_order_fourth_state.name.source: public
*pb_query_order_fourth_state.static: false
*pb_query_order_fourth_state.name: pb_query_order_fourth_state
*pb_query_order_fourth_state.parent: rc_query_order_fourth
*pb_query_order_fourth_state.labelString: "State"
*pb_query_order_fourth_state.activateCallback.source: public
*pb_query_order_fourth_state.activateCallback: 
*pb_query_order_fourth_state.x: 97
*pb_query_order_fourth_state.y: 200

*pb_query_order_fourth_age.class: pushButton
*pb_query_order_fourth_age.static: true
*pb_query_order_fourth_age.name: pb_query_order_fourth_age
*pb_query_order_fourth_age.parent: rc_query_order_fourth
*pb_query_order_fourth_age.labelString: "Age (Days)"
*pb_query_order_fourth_age.x: 97
*pb_query_order_fourth_age.y: 222
*pb_query_order_fourth_age.activateCallback.source: public
*pb_query_order_fourth_age.activateCallback: 

*l_query_order_by.class: label
*l_query_order_by.static: true
*l_query_order_by.name: l_query_order_by
*l_query_order_by.parent: pps_query
*l_query_order_by.isCompound: "true"
*l_query_order_by.compoundIcon: "label.xpm"
*l_query_order_by.compoundName: "label_"
*l_query_order_by.x: 891
*l_query_order_by.y: 42
*l_query_order_by.width: 71
*l_query_order_by.height: 27
*l_query_order_by.labelString: "Order By"

