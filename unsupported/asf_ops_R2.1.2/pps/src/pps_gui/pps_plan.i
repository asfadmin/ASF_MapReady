! UIMX ascii 2.9 key: 3002                                                      

*pps_plan.class: form
*pps_plan.gbldecl: #include <stdio.h>\
extern swidget nojoy;
*pps_plan.ispecdecl:
*pps_plan.funcdecl: swidget create_pps_plan(swidget UxParent)
*pps_plan.funcname: create_pps_plan
*pps_plan.funcdef: "swidget", "<create_pps_plan>(%)"
*pps_plan.argdecl: swidget UxParent;
*pps_plan.arglist: UxParent
*pps_plan.arglist.UxParent: "swidget", "%UxParent%"
*pps_plan.icode:
*pps_plan.fcode: return(rtrn);\

*pps_plan.auxdecl:
*pps_plan.name.source: public
*pps_plan.static: false
*pps_plan.name: pps_plan
*pps_plan.parent: NO_PARENT
*pps_plan.parentExpression: UxParent
*pps_plan.defaultShell: transientShell
*pps_plan.width: 1145
*pps_plan.height: 866
*pps_plan.resizePolicy: "resize_none"
*pps_plan.isCompound: "true"
*pps_plan.compoundIcon: "form.xpm"
*pps_plan.compoundName: "form_"
*pps_plan.x: 0
*pps_plan.y: 0
*pps_plan.unitType: "pixels"

*mb_plan.class: rowColumn
*mb_plan.static: true
*mb_plan.name: mb_plan
*mb_plan.parent: pps_plan
*mb_plan.rowColumnType: "menu_bar"
*mb_plan.isCompound: "true"
*mb_plan.compoundIcon: "pulldownM.xpm"
*mb_plan.compoundName: "menu_Bar"
*mb_plan.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*mb_plan.x: 0
*mb_plan.y: 0
*mb_plan.width: 504
*mb_plan.height: 36
*mb_plan.menuAccelerator: "<KeyUp>F10"
*mb_plan.rightAttachment: "attach_form"
*mb_plan.leftAttachment: "attach_form"
*mb_plan.spacing: 20

*pb_plan_file.class: rowColumn
*pb_plan_file.static: true
*pb_plan_file.name: pb_plan_file
*pb_plan_file.parent: mb_plan
*pb_plan_file.rowColumnType: "menu_pulldown"

*pb_plan_print_results.class: pushButton
*pb_plan_print_results.static: true
*pb_plan_print_results.name: pb_plan_print_results
*pb_plan_print_results.parent: pb_plan_file
*pb_plan_print_results.labelString: "Print Results..."
*pb_plan_print_results.mnemonic: "R"
*pb_plan_print_results.activateCallback: {\
cb_plan_print_results();\
}

*pb_plan_print_screen.class: pushButton
*pb_plan_print_screen.static: true
*pb_plan_print_screen.name: pb_plan_print_screen
*pb_plan_print_screen.parent: pb_plan_file
*pb_plan_print_screen.labelString: "Print Screen"
*pb_plan_print_screen.activateCallback: {\
extern void pps_print_screen(swidget sw);\
\
pps_print_screen(pps_plan);\
\
}
*pb_plan_print_screen.mnemonic: "P"

*pb_plan_exit.class: pushButton
*pb_plan_exit.static: true
*pb_plan_exit.name: pb_plan_exit
*pb_plan_exit.parent: pb_plan_file
*pb_plan_exit.labelString: "Exit"
*pb_plan_exit.mnemonic: "x"
*pb_plan_exit.activateCallback: UxPopdownInterface(pps_plan);

*pb_plan_query_setting.class: rowColumn
*pb_plan_query_setting.static: true
*pb_plan_query_setting.name: pb_plan_query_setting
*pb_plan_query_setting.parent: mb_plan
*pb_plan_query_setting.rowColumnType: "menu_pulldown"
*pb_plan_query_setting.x: 150

*pb_plan_load_query.class: pushButton
*pb_plan_load_query.static: true
*pb_plan_load_query.name: pb_plan_load_query
*pb_plan_load_query.parent: pb_plan_query_setting
*pb_plan_load_query.labelString: "Load Query..."
*pb_plan_load_query.mnemonic: "L"
*pb_plan_load_query.activateCallback: cb_plan_load_query();

*pb_plan_save_query.class: pushButton
*pb_plan_save_query.static: true
*pb_plan_save_query.name: pb_plan_save_query
*pb_plan_save_query.parent: pb_plan_query_setting
*pb_plan_save_query.labelString: "Save Query"
*pb_plan_save_query.mnemonic: "S"
*pb_plan_save_query.activateCallback: cb_plan_save_query();

*pb_plan_save_query_as.class: pushButton
*pb_plan_save_query_as.static: true
*pb_plan_save_query_as.name: pb_plan_save_query_as
*pb_plan_save_query_as.parent: pb_plan_query_setting
*pb_plan_save_query_as.labelString: "Save Query As..."
*pb_plan_save_query_as.mnemonic: "A"
*pb_plan_save_query_as.activateCallback: cb_plan_save_query_as();

*pb_plan_clear_query.class: pushButton
*pb_plan_clear_query.static: true
*pb_plan_clear_query.name: pb_plan_clear_query
*pb_plan_clear_query.parent: pb_plan_query_setting
*pb_plan_clear_query.labelString: "Clear Query"
*pb_plan_clear_query.mnemonic: "C"
*pb_plan_clear_query.activateCallback: cb_plan_clear_query();

*mb_plan_file.class: cascadeButton
*mb_plan_file.static: true
*mb_plan_file.name: mb_plan_file
*mb_plan_file.parent: mb_plan
*mb_plan_file.labelString: "File"
*mb_plan_file.subMenuId: "pb_plan_file"
*mb_plan_file.mnemonic: "F"

*mb_plan_top_b1.class: cascadeButton
*mb_plan_top_b1.static: true
*mb_plan_top_b1.name: mb_plan_top_b1
*mb_plan_top_b1.parent: mb_plan
*mb_plan_top_b1.labelString: "QuerySetting"
*mb_plan_top_b1.mnemonic: "Q"
*mb_plan_top_b1.subMenuId: "pb_plan_query_setting"
*mb_plan_top_b1.x: 100

*f_plan_query_settings.class: frame
*f_plan_query_settings.static: true
*f_plan_query_settings.name: f_plan_query_settings
*f_plan_query_settings.parent: pps_plan
*f_plan_query_settings.width: 860
*f_plan_query_settings.height: 233
*f_plan_query_settings.isCompound: "true"
*f_plan_query_settings.compoundIcon: "frame.xpm"
*f_plan_query_settings.compoundName: "frame_"
*f_plan_query_settings.x: 12
*f_plan_query_settings.y: 64

*bb_plan_query_settings.class: bulletinBoard
*bb_plan_query_settings.static: true
*bb_plan_query_settings.name: bb_plan_query_settings
*bb_plan_query_settings.parent: f_plan_query_settings
*bb_plan_query_settings.resizePolicy: "resize_none"
*bb_plan_query_settings.width: 847
*bb_plan_query_settings.height: 228
*bb_plan_query_settings.isCompound: "true"
*bb_plan_query_settings.compoundIcon: "bboard.xpm"
*bb_plan_query_settings.compoundName: "bulletin_Board"
*bb_plan_query_settings.x: 5
*bb_plan_query_settings.y: 1

*tb_plan_L1_Orders.class: toggleButton
*tb_plan_L1_Orders.name.source: public
*tb_plan_L1_Orders.static: false
*tb_plan_L1_Orders.name: tb_plan_L1_Orders
*tb_plan_L1_Orders.parent: bb_plan_query_settings
*tb_plan_L1_Orders.x: 16
*tb_plan_L1_Orders.y: 10
*tb_plan_L1_Orders.width: 94
*tb_plan_L1_Orders.height: 26
*tb_plan_L1_Orders.labelString: "L1 Orders"
*tb_plan_L1_Orders.sensitive: "true"
*tb_plan_L1_Orders.valueChangedCallback: do_plan_new_order_type();

*tb_plan_L1_QLK.class: toggleButton
*tb_plan_L1_QLK.name.source: public
*tb_plan_L1_QLK.static: false
*tb_plan_L1_QLK.name: tb_plan_L1_QLK
*tb_plan_L1_QLK.parent: bb_plan_query_settings
*tb_plan_L1_QLK.x: 160
*tb_plan_L1_QLK.y: 10
*tb_plan_L1_QLK.width: 73
*tb_plan_L1_QLK.height: 26
*tb_plan_L1_QLK.labelString: "L1 QLK"
*tb_plan_L1_QLK.sensitive: "true"
*tb_plan_L1_QLK.valueChangedCallback: do_plan_new_order_type();

*tb_plan_Scan_Orders.class: toggleButton
*tb_plan_Scan_Orders.name.source: public
*tb_plan_Scan_Orders.static: false
*tb_plan_Scan_Orders.name: tb_plan_Scan_Orders
*tb_plan_Scan_Orders.parent: bb_plan_query_settings
*tb_plan_Scan_Orders.x: 280
*tb_plan_Scan_Orders.y: 10
*tb_plan_Scan_Orders.width: 109
*tb_plan_Scan_Orders.height: 26
*tb_plan_Scan_Orders.labelString: "Scan Orders"
*tb_plan_Scan_Orders.valueChangedCallback: do_plan_new_order_type();

*tb_plan_Scan_QLK.class: toggleButton
*tb_plan_Scan_QLK.name.source: public
*tb_plan_Scan_QLK.static: false
*tb_plan_Scan_QLK.name: tb_plan_Scan_QLK
*tb_plan_Scan_QLK.parent: bb_plan_query_settings
*tb_plan_Scan_QLK.x: 440
*tb_plan_Scan_QLK.y: 10
*tb_plan_Scan_QLK.width: 88
*tb_plan_Scan_QLK.height: 26
*tb_plan_Scan_QLK.labelString: "Scan QLK"
*tb_plan_Scan_QLK.valueChangedCallback: do_plan_new_order_type();

*om_plan_sat.class: rowColumn
*om_plan_sat.name.source: public
*om_plan_sat.static: false
*om_plan_sat.name: om_plan_sat
*om_plan_sat.parent: bb_plan_query_settings
*om_plan_sat.rowColumnType: "menu_option"
*om_plan_sat.subMenuId: "rc_plan_sat"
*om_plan_sat.isCompound: "true"
*om_plan_sat.compoundIcon: "optionM.xpm"
*om_plan_sat.compoundName: "option_Menu"
*om_plan_sat.x: 15
*om_plan_sat.y: 44
*om_plan_sat.width: 48
*om_plan_sat.height: 60
*om_plan_sat.labelString: "Sat"
*om_plan_sat.sensitive: "false"

*rc_plan_sat.class: rowColumn
*rc_plan_sat.name.source: public
*rc_plan_sat.static: false
*rc_plan_sat.name: rc_plan_sat
*rc_plan_sat.parent: om_plan_sat
*rc_plan_sat.rowColumnType: "menu_pulldown"
*rc_plan_sat.sensitive: "false"

*pb_plan_sat_any.class: pushButton
*pb_plan_sat_any.name.source: public
*pb_plan_sat_any.static: false
*pb_plan_sat_any.name: pb_plan_sat_any
*pb_plan_sat_any.parent: rc_plan_sat
*pb_plan_sat_any.labelString: "Any"
*pb_plan_sat_any.sensitive: "false"

*pb_plan_sat_e1.class: pushButton
*pb_plan_sat_e1.name.source: public
*pb_plan_sat_e1.static: false
*pb_plan_sat_e1.name: pb_plan_sat_e1
*pb_plan_sat_e1.parent: rc_plan_sat
*pb_plan_sat_e1.labelString: "E1"
*pb_plan_sat_e1.sensitive: "false"

*pb_plan_sat_e2.class: pushButton
*pb_plan_sat_e2.name.source: public
*pb_plan_sat_e2.static: false
*pb_plan_sat_e2.name: pb_plan_sat_e2
*pb_plan_sat_e2.parent: rc_plan_sat
*pb_plan_sat_e2.labelString: "E2"
*pb_plan_sat_e2.sensitive: "false"

*pb_plan_sat_j1.class: pushButton
*pb_plan_sat_j1.name.source: public
*pb_plan_sat_j1.static: false
*pb_plan_sat_j1.name: pb_plan_sat_j1
*pb_plan_sat_j1.parent: rc_plan_sat
*pb_plan_sat_j1.labelString: "J1"
*pb_plan_sat_j1.sensitive: "false"

*pb_plan_sat_r1.class: pushButton
*pb_plan_sat_r1.name.source: public
*pb_plan_sat_r1.static: false
*pb_plan_sat_r1.name: pb_plan_sat_r1
*pb_plan_sat_r1.parent: rc_plan_sat
*pb_plan_sat_r1.labelString: "R1"
*pb_plan_sat_r1.sensitive: "false"

*om_plan_sens.class: rowColumn
*om_plan_sens.name.source: public
*om_plan_sens.static: false
*om_plan_sens.name: om_plan_sens
*om_plan_sens.parent: bb_plan_query_settings
*om_plan_sens.rowColumnType: "menu_option"
*om_plan_sens.subMenuId: "rc_plan_sens"
*om_plan_sens.isCompound: "true"
*om_plan_sens.compoundIcon: "optionM.xpm"
*om_plan_sens.compoundName: "option_Menu"
*om_plan_sens.x: 120
*om_plan_sens.y: 45
*om_plan_sens.width: 60
*om_plan_sens.height: 54
*om_plan_sens.labelString: "Sens"
*om_plan_sens.sensitive: "false"

*rc_plan_sens.class: rowColumn
*rc_plan_sens.name.source: public
*rc_plan_sens.static: false
*rc_plan_sens.name: rc_plan_sens
*rc_plan_sens.parent: om_plan_sens
*rc_plan_sens.rowColumnType: "menu_pulldown"
*rc_plan_sens.sensitive: "false"

*pb_plan_sens_any.class: pushButton
*pb_plan_sens_any.name.source: public
*pb_plan_sens_any.static: false
*pb_plan_sens_any.name: pb_plan_sens_any
*pb_plan_sens_any.parent: rc_plan_sens
*pb_plan_sens_any.labelString: "Any"
*pb_plan_sens_any.sensitive: "false"
*pb_plan_sens_any.activateCallback.source: public
*pb_plan_sens_any.activateCallback: 

*pb_plan_sens_s.class: pushButton
*pb_plan_sens_s.name.source: public
*pb_plan_sens_s.static: false
*pb_plan_sens_s.name: pb_plan_sens_s
*pb_plan_sens_s.parent: rc_plan_sens
*pb_plan_sens_s.labelString: "S"
*pb_plan_sens_s.sensitive: "false"
*pb_plan_sens_s.activateCallback.source: public
*pb_plan_sens_s.activateCallback: 

*rc_plan_sens_o.class: pushButton
*rc_plan_sens_o.static: true
*rc_plan_sens_o.name: rc_plan_sens_o
*rc_plan_sens_o.parent: rc_plan_sens
*rc_plan_sens_o.labelString: "O"

*rc_plan_sens_v.class: pushButton
*rc_plan_sens_v.static: true
*rc_plan_sens_v.name: rc_plan_sens_v
*rc_plan_sens_v.parent: rc_plan_sens
*rc_plan_sens_v.labelString: "V"
*rc_plan_sens_v.activateCallback.source: public
*rc_plan_sens_v.activateCallback: 

*rc_plan_sens_z.class: pushButton
*rc_plan_sens_z.static: true
*rc_plan_sens_z.name: rc_plan_sens_z
*rc_plan_sens_z.parent: rc_plan_sens
*rc_plan_sens_z.labelString: "Z"
*rc_plan_sens_z.activateCallback.source: public
*rc_plan_sens_z.activateCallback: 

*om_plan_activity.class: rowColumn
*om_plan_activity.name.source: public
*om_plan_activity.static: false
*om_plan_activity.name: om_plan_activity
*om_plan_activity.parent: bb_plan_query_settings
*om_plan_activity.rowColumnType: "menu_option"
*om_plan_activity.subMenuId: "rc_plan_activity"
*om_plan_activity.isCompound: "true"
*om_plan_activity.compoundIcon: "optionM.xpm"
*om_plan_activity.compoundName: "option_Menu"
*om_plan_activity.x: 490
*om_plan_activity.y: 46
*om_plan_activity.width: 60
*om_plan_activity.height: 54
*om_plan_activity.labelString: "Activity"
*om_plan_activity.sensitive: "false"

*rc_plan_activity.class: rowColumn
*rc_plan_activity.name.source: public
*rc_plan_activity.static: false
*rc_plan_activity.name: rc_plan_activity
*rc_plan_activity.parent: om_plan_activity
*rc_plan_activity.rowColumnType: "menu_pulldown"
*rc_plan_activity.sensitive: "false"

*pb_plan_activity_any.class: pushButton
*pb_plan_activity_any.name.source: public
*pb_plan_activity_any.static: false
*pb_plan_activity_any.name: pb_plan_activity_any
*pb_plan_activity_any.parent: rc_plan_activity
*pb_plan_activity_any.labelString: "Any"
*pb_plan_activity_any.sensitive: "false"
*pb_plan_activity_any.activateCallback.source: public
*pb_plan_activity_any.activateCallback: 

*pb_plan_activity_rlt.class: pushButton
*pb_plan_activity_rlt.name.source: public
*pb_plan_activity_rlt.static: false
*pb_plan_activity_rlt.name: pb_plan_activity_rlt
*pb_plan_activity_rlt.parent: rc_plan_activity
*pb_plan_activity_rlt.labelString: "RLT"
*pb_plan_activity_rlt.sensitive: "false"
*pb_plan_activity_rlt.activateCallback.source: public
*pb_plan_activity_rlt.activateCallback: 

*pb_plan_activity_dmp.class: pushButton
*pb_plan_activity_dmp.name.source: public
*pb_plan_activity_dmp.static: false
*pb_plan_activity_dmp.name: pb_plan_activity_dmp
*pb_plan_activity_dmp.parent: rc_plan_activity
*pb_plan_activity_dmp.labelString: "DMP"
*pb_plan_activity_dmp.sensitive: "false"

*om_plan_station.class: rowColumn
*om_plan_station.name.source: public
*om_plan_station.static: false
*om_plan_station.name: om_plan_station
*om_plan_station.parent: bb_plan_query_settings
*om_plan_station.rowColumnType: "menu_option"
*om_plan_station.subMenuId: "rc_plan_station"
*om_plan_station.isCompound: "true"
*om_plan_station.compoundIcon: "optionM.xpm"
*om_plan_station.compoundName: "option_Menu"
*om_plan_station.x: 670
*om_plan_station.y: 44
*om_plan_station.width: 60
*om_plan_station.height: 54
*om_plan_station.labelString: "Station"
*om_plan_station.sensitive: "false"

*rc_plan_station.class: rowColumn
*rc_plan_station.name.source: public
*rc_plan_station.static: false
*rc_plan_station.name: rc_plan_station
*rc_plan_station.parent: om_plan_station
*rc_plan_station.rowColumnType: "menu_pulldown"
*rc_plan_station.sensitive: "false"

*pb_plan_station_any.class: pushButton
*pb_plan_station_any.name.source: public
*pb_plan_station_any.static: false
*pb_plan_station_any.name: pb_plan_station_any
*pb_plan_station_any.parent: rc_plan_station
*pb_plan_station_any.labelString: "Any"
*pb_plan_station_any.sensitive: "false"
*pb_plan_station_any.activateCallback.source: public
*pb_plan_station_any.activateCallback: 

*pb_plan_station_fa.class: pushButton
*pb_plan_station_fa.name.source: public
*pb_plan_station_fa.static: false
*pb_plan_station_fa.name: pb_plan_station_fa
*pb_plan_station_fa.parent: rc_plan_station
*pb_plan_station_fa.labelString: "FA"
*pb_plan_station_fa.sensitive: "false"
*pb_plan_station_fa.activateCallback.source: public
*pb_plan_station_fa.activateCallback: 

*pb_plan_station_mc.class: pushButton
*pb_plan_station_mc.name.source: public
*pb_plan_station_mc.static: false
*pb_plan_station_mc.name: pb_plan_station_mc
*pb_plan_station_mc.parent: rc_plan_station
*pb_plan_station_mc.labelString: "MC"
*pb_plan_station_mc.sensitive: "false"

*pb_plan_station_gh.class: pushButton
*pb_plan_station_gh.static: true
*pb_plan_station_gh.name: pb_plan_station_gh
*pb_plan_station_gh.parent: rc_plan_station
*pb_plan_station_gh.labelString: "GH"

*pb_plan_station_ph.class: pushButton
*pb_plan_station_ph.static: true
*pb_plan_station_ph.name: pb_plan_station_ph
*pb_plan_station_ph.parent: rc_plan_station
*pb_plan_station_ph.labelString: "PH"

*pb_plan_station_as.class: pushButton
*pb_plan_station_as.static: true
*pb_plan_station_as.name: pb_plan_station_as
*pb_plan_station_as.parent: rc_plan_station
*pb_plan_station_as.labelString: "AS"

*pb_plan_station_be.class: pushButton
*pb_plan_station_be.static: true
*pb_plan_station_be.name: pb_plan_station_be
*pb_plan_station_be.parent: rc_plan_station
*pb_plan_station_be.labelString: "BE"

*pb_plan_station_co.class: pushButton
*pb_plan_station_co.static: true
*pb_plan_station_co.name: pb_plan_station_co
*pb_plan_station_co.parent: rc_plan_station
*pb_plan_station_co.labelString: "CO"

*pb_plan_station_cu.class: pushButton
*pb_plan_station_cu.static: true
*pb_plan_station_cu.name: pb_plan_station_cu
*pb_plan_station_cu.parent: rc_plan_station
*pb_plan_station_cu.labelString: "CU"

*pb_plan_station_es.class: pushButton
*pb_plan_station_es.static: true
*pb_plan_station_es.name: pb_plan_station_es
*pb_plan_station_es.parent: rc_plan_station
*pb_plan_station_es.labelString: "ES"

*pb_plan_station_fs.class: pushButton
*pb_plan_station_fs.static: true
*pb_plan_station_fs.name: pb_plan_station_fs
*pb_plan_station_fs.parent: rc_plan_station
*pb_plan_station_fs.labelString: "FS"

*pb_plan_station_ha.class: pushButton
*pb_plan_station_ha.static: true
*pb_plan_station_ha.name: pb_plan_station_ha
*pb_plan_station_ha.parent: rc_plan_station
*pb_plan_station_ha.labelString: "HA"

*pb_plan_station_ho.class: pushButton
*pb_plan_station_ho.static: true
*pb_plan_station_ho.name: pb_plan_station_ho
*pb_plan_station_ho.parent: rc_plan_station
*pb_plan_station_ho.labelString: "HO"

*pb_plan_station_hy.class: pushButton
*pb_plan_station_hy.static: true
*pb_plan_station_hy.name: pb_plan_station_hy
*pb_plan_station_hy.parent: rc_plan_station
*pb_plan_station_hy.labelString: "HY"

*pb_plan_station_is.class: pushButton
*pb_plan_station_is.static: true
*pb_plan_station_is.name: pb_plan_station_is
*pb_plan_station_is.parent: rc_plan_station
*pb_plan_station_is.labelString: "IS"

*pb_plan_station_in.class: pushButton
*pb_plan_station_in.static: true
*pb_plan_station_in.name: pb_plan_station_in
*pb_plan_station_in.parent: rc_plan_station
*pb_plan_station_in.labelString: "IN"

*pb_plan_station_jo.class: pushButton
*pb_plan_station_jo.static: true
*pb_plan_station_jo.name: pb_plan_station_jo
*pb_plan_station_jo.parent: rc_plan_station
*pb_plan_station_jo.labelString: "JO"

*pb_plan_station_ks.class: pushButton
*pb_plan_station_ks.static: true
*pb_plan_station_ks.name: pb_plan_station_ks
*pb_plan_station_ks.parent: rc_plan_station
*pb_plan_station_ks.labelString: "KS"

*pb_plan_station_ku.class: pushButton
*pb_plan_station_ku.static: true
*pb_plan_station_ku.name: pb_plan_station_ku
*pb_plan_station_ku.parent: rc_plan_station
*pb_plan_station_ku.labelString: "KU"

*pb_plan_station_ma.class: pushButton
*pb_plan_station_ma.static: true
*pb_plan_station_ma.name: pb_plan_station_ma
*pb_plan_station_ma.parent: rc_plan_station
*pb_plan_station_ma.labelString: "MA"

*pb_plan_station_ms.class: pushButton
*pb_plan_station_ms.static: true
*pb_plan_station_ms.name: pb_plan_station_ms
*pb_plan_station_ms.parent: rc_plan_station
*pb_plan_station_ms.labelString: "MS"

*pb_plan_station_pp.class: pushButton
*pb_plan_station_pp.static: true
*pb_plan_station_pp.name: pb_plan_station_pp
*pb_plan_station_pp.parent: rc_plan_station
*pb_plan_station_pp.labelString: "PP"

*pb_plan_station_sa.class: pushButton
*pb_plan_station_sa.static: true
*pb_plan_station_sa.name: pb_plan_station_sa
*pb_plan_station_sa.parent: rc_plan_station
*pb_plan_station_sa.labelString: "SA"

*pb_plan_station_se.class: pushButton
*pb_plan_station_se.static: true
*pb_plan_station_se.name: pb_plan_station_se
*pb_plan_station_se.parent: rc_plan_station
*pb_plan_station_se.labelString: "SE"

*pb_plan_station_sy.class: pushButton
*pb_plan_station_sy.static: true
*pb_plan_station_sy.name: pb_plan_station_sy
*pb_plan_station_sy.parent: rc_plan_station
*pb_plan_station_sy.labelString: "SY"

*pb_plan_station_tf.class: pushButton
*pb_plan_station_tf.static: true
*pb_plan_station_tf.name: pb_plan_station_tf
*pb_plan_station_tf.parent: rc_plan_station
*pb_plan_station_tf.labelString: "TF"

*pb_plan_station_tg.class: pushButton
*pb_plan_station_tg.static: true
*pb_plan_station_tg.name: pb_plan_station_tg
*pb_plan_station_tg.parent: rc_plan_station
*pb_plan_station_tg.labelString: "TG"

*pb_plan_station_th.class: pushButton
*pb_plan_station_th.static: true
*pb_plan_station_th.name: pb_plan_station_th
*pb_plan_station_th.parent: rc_plan_station
*pb_plan_station_th.labelString: "TH"

*pb_plan_station_to.class: pushButton
*pb_plan_station_to.static: true
*pb_plan_station_to.name: pb_plan_station_to
*pb_plan_station_to.parent: rc_plan_station
*pb_plan_station_to.labelString: "TO"

*pb_plan_station_ts.class: pushButton
*pb_plan_station_ts.static: true
*pb_plan_station_ts.name: pb_plan_station_ts
*pb_plan_station_ts.parent: rc_plan_station
*pb_plan_station_ts.labelString: "TS"

*pb_plan_station_wf.class: pushButton
*pb_plan_station_wf.static: true
*pb_plan_station_wf.name: pb_plan_station_wf
*pb_plan_station_wf.parent: rc_plan_station
*pb_plan_station_wf.labelString: "WF"

*pb_plan_station_wh.class: pushButton
*pb_plan_station_wh.static: true
*pb_plan_station_wh.name: pb_plan_station_wh
*pb_plan_station_wh.parent: rc_plan_station
*pb_plan_station_wh.labelString: "WH"

*om_plan_priority.class: rowColumn
*om_plan_priority.name.source: public
*om_plan_priority.static: false
*om_plan_priority.name: om_plan_priority
*om_plan_priority.parent: bb_plan_query_settings
*om_plan_priority.rowColumnType: "menu_option"
*om_plan_priority.subMenuId: "rc_plan_priority"
*om_plan_priority.isCompound: "true"
*om_plan_priority.compoundIcon: "optionM.xpm"
*om_plan_priority.compoundName: "option_Menu"
*om_plan_priority.x: 520
*om_plan_priority.y: 90
*om_plan_priority.width: 60
*om_plan_priority.height: 54
*om_plan_priority.labelString: "Priority"
*om_plan_priority.sensitive: "false"

*rc_plan_priority.class: rowColumn
*rc_plan_priority.name.source: public
*rc_plan_priority.static: false
*rc_plan_priority.name: rc_plan_priority
*rc_plan_priority.parent: om_plan_priority
*rc_plan_priority.rowColumnType: "menu_pulldown"
*rc_plan_priority.sensitive: "false"

*pb_plan_priority_any.class: pushButton
*pb_plan_priority_any.name.source: public
*pb_plan_priority_any.static: false
*pb_plan_priority_any.name: pb_plan_priority_any
*pb_plan_priority_any.parent: rc_plan_priority
*pb_plan_priority_any.labelString: "Any"
*pb_plan_priority_any.sensitive: "false"

*pb_plan_priority_low.class: pushButton
*pb_plan_priority_low.name.source: public
*pb_plan_priority_low.static: false
*pb_plan_priority_low.name: pb_plan_priority_low
*pb_plan_priority_low.parent: rc_plan_priority
*pb_plan_priority_low.labelString: "LOW"
*pb_plan_priority_low.sensitive: "false"

*pb_plan_priority_routine.class: pushButton
*pb_plan_priority_routine.name.source: public
*pb_plan_priority_routine.static: false
*pb_plan_priority_routine.name: pb_plan_priority_routine
*pb_plan_priority_routine.parent: rc_plan_priority
*pb_plan_priority_routine.labelString: "ROUTINE"
*pb_plan_priority_routine.sensitive: "false"

*pb_plan_priority_high.class: pushButton
*pb_plan_priority_high.name.source: public
*pb_plan_priority_high.static: false
*pb_plan_priority_high.name: pb_plan_priority_high
*pb_plan_priority_high.parent: rc_plan_priority
*pb_plan_priority_high.labelString: "HIGH"
*pb_plan_priority_high.sensitive: "false"

*pb_plan_priority_urgent.class: pushButton
*pb_plan_priority_urgent.name.source: public
*pb_plan_priority_urgent.static: false
*pb_plan_priority_urgent.name: pb_plan_priority_urgent
*pb_plan_priority_urgent.parent: rc_plan_priority
*pb_plan_priority_urgent.labelString: "URGENT"
*pb_plan_priority_urgent.sensitive: "false"

*l_plan_rev.class: label
*l_plan_rev.name.source: public
*l_plan_rev.static: false
*l_plan_rev.name: l_plan_rev
*l_plan_rev.parent: bb_plan_query_settings
*l_plan_rev.isCompound: "true"
*l_plan_rev.compoundIcon: "label.xpm"
*l_plan_rev.compoundName: "label_"
*l_plan_rev.x: 232
*l_plan_rev.y: 47
*l_plan_rev.width: 34
*l_plan_rev.height: 32
*l_plan_rev.labelString: "Rev"
*l_plan_rev.sensitive: "false"

*tf_plan_rev.class: textField
*tf_plan_rev.name.source: public
*tf_plan_rev.static: false
*tf_plan_rev.name: tf_plan_rev
*tf_plan_rev.parent: bb_plan_query_settings
*tf_plan_rev.width: 67
*tf_plan_rev.isCompound: "true"
*tf_plan_rev.compoundIcon: "textfield.xpm"
*tf_plan_rev.compoundName: "text_Field"
*tf_plan_rev.x: 268
*tf_plan_rev.y: 48
*tf_plan_rev.height: 32
*tf_plan_rev.sensitive: "false"

*l_plan_seq.class: label
*l_plan_seq.name.source: public
*l_plan_seq.static: false
*l_plan_seq.name: l_plan_seq
*l_plan_seq.parent: bb_plan_query_settings
*l_plan_seq.isCompound: "true"
*l_plan_seq.compoundIcon: "label.xpm"
*l_plan_seq.compoundName: "label_"
*l_plan_seq.x: 345
*l_plan_seq.y: 46
*l_plan_seq.width: 34
*l_plan_seq.height: 32
*l_plan_seq.labelString: "Seq"
*l_plan_seq.sensitive: "false"

*tf_plan_seq.class: textField
*tf_plan_seq.name.source: public
*tf_plan_seq.static: false
*tf_plan_seq.name: tf_plan_seq
*tf_plan_seq.parent: bb_plan_query_settings
*tf_plan_seq.width: 55
*tf_plan_seq.isCompound: "true"
*tf_plan_seq.compoundIcon: "textfield.xpm"
*tf_plan_seq.compoundName: "text_Field"
*tf_plan_seq.x: 383
*tf_plan_seq.y: 47
*tf_plan_seq.height: 32
*tf_plan_seq.sensitive: "false"

*l_plan_job_id.class: label
*l_plan_job_id.name.source: public
*l_plan_job_id.static: false
*l_plan_job_id.name: l_plan_job_id
*l_plan_job_id.parent: bb_plan_query_settings
*l_plan_job_id.isCompound: "true"
*l_plan_job_id.compoundIcon: "label.xpm"
*l_plan_job_id.compoundName: "label_"
*l_plan_job_id.x: 15
*l_plan_job_id.y: 94
*l_plan_job_id.width: 56
*l_plan_job_id.height: 32
*l_plan_job_id.labelString: "Job ID"
*l_plan_job_id.sensitive: "false"

*tf_plan_job_id.class: textField
*tf_plan_job_id.name.source: public
*tf_plan_job_id.static: false
*tf_plan_job_id.name: tf_plan_job_id
*tf_plan_job_id.parent: bb_plan_query_settings
*tf_plan_job_id.width: 78
*tf_plan_job_id.isCompound: "true"
*tf_plan_job_id.compoundIcon: "textfield.xpm"
*tf_plan_job_id.compoundName: "text_Field"
*tf_plan_job_id.x: 74
*tf_plan_job_id.y: 92
*tf_plan_job_id.height: 32
*tf_plan_job_id.sensitive: "false"

*l_plan_order_id.class: label
*l_plan_order_id.name.source: public
*l_plan_order_id.static: false
*l_plan_order_id.name: l_plan_order_id
*l_plan_order_id.parent: bb_plan_query_settings
*l_plan_order_id.isCompound: "true"
*l_plan_order_id.compoundIcon: "label.xpm"
*l_plan_order_id.compoundName: "label_"
*l_plan_order_id.x: 164
*l_plan_order_id.y: 93
*l_plan_order_id.width: 72
*l_plan_order_id.height: 32
*l_plan_order_id.labelString: "Order ID"
*l_plan_order_id.sensitive: "false"

*tf_plan_order_id.class: textField
*tf_plan_order_id.name.source: public
*tf_plan_order_id.static: false
*tf_plan_order_id.name: tf_plan_order_id
*tf_plan_order_id.parent: bb_plan_query_settings
*tf_plan_order_id.width: 83
*tf_plan_order_id.isCompound: "true"
*tf_plan_order_id.compoundIcon: "textfield.xpm"
*tf_plan_order_id.compoundName: "text_Field"
*tf_plan_order_id.x: 237
*tf_plan_order_id.y: 93
*tf_plan_order_id.height: 32
*tf_plan_order_id.sensitive: "false"

*l_plan_item_id.class: label
*l_plan_item_id.name.source: public
*l_plan_item_id.static: false
*l_plan_item_id.name: l_plan_item_id
*l_plan_item_id.parent: bb_plan_query_settings
*l_plan_item_id.isCompound: "true"
*l_plan_item_id.compoundIcon: "label.xpm"
*l_plan_item_id.compoundName: "label_"
*l_plan_item_id.x: 329
*l_plan_item_id.y: 93
*l_plan_item_id.width: 75
*l_plan_item_id.height: 32
*l_plan_item_id.labelString: "Item ID"
*l_plan_item_id.sensitive: "false"

*tf_plan_item_id.class: textField
*tf_plan_item_id.name.source: public
*tf_plan_item_id.static: false
*tf_plan_item_id.name: tf_plan_item_id
*tf_plan_item_id.parent: bb_plan_query_settings
*tf_plan_item_id.width: 43
*tf_plan_item_id.isCompound: "true"
*tf_plan_item_id.compoundIcon: "textfield.xpm"
*tf_plan_item_id.compoundName: "text_Field"
*tf_plan_item_id.x: 402
*tf_plan_item_id.y: 93
*tf_plan_item_id.height: 32
*tf_plan_item_id.sensitive: "false"

*l_plan_frame_id.class: label
*l_plan_frame_id.name.source: public
*l_plan_frame_id.static: false
*l_plan_frame_id.name: l_plan_frame_id
*l_plan_frame_id.parent: bb_plan_query_settings
*l_plan_frame_id.isCompound: "true"
*l_plan_frame_id.compoundIcon: "label.xpm"
*l_plan_frame_id.compoundName: "label_"
*l_plan_frame_id.x: 10
*l_plan_frame_id.y: 141
*l_plan_frame_id.width: 73
*l_plan_frame_id.height: 32
*l_plan_frame_id.labelString: "Frame ID"
*l_plan_frame_id.sensitive: "false"

*tf_plan_frame_id.class: textField
*tf_plan_frame_id.name.source: public
*tf_plan_frame_id.static: false
*tf_plan_frame_id.name: tf_plan_frame_id
*tf_plan_frame_id.parent: bb_plan_query_settings
*tf_plan_frame_id.width: 67
*tf_plan_frame_id.isCompound: "true"
*tf_plan_frame_id.compoundIcon: "textfield.xpm"
*tf_plan_frame_id.compoundName: "text_Field"
*tf_plan_frame_id.x: 83
*tf_plan_frame_id.y: 141
*tf_plan_frame_id.height: 32
*tf_plan_frame_id.sensitive: "false"

*l_plan_subframe_id.class: label
*l_plan_subframe_id.name.source: public
*l_plan_subframe_id.static: false
*l_plan_subframe_id.name: l_plan_subframe_id
*l_plan_subframe_id.parent: bb_plan_query_settings
*l_plan_subframe_id.isCompound: "true"
*l_plan_subframe_id.compoundIcon: "label.xpm"
*l_plan_subframe_id.compoundName: "label_"
*l_plan_subframe_id.x: 159
*l_plan_subframe_id.y: 141
*l_plan_subframe_id.width: 100
*l_plan_subframe_id.height: 32
*l_plan_subframe_id.labelString: "Subframe ID"
*l_plan_subframe_id.sensitive: "false"

*tf_plan_subframe_id.class: textField
*tf_plan_subframe_id.name.source: public
*tf_plan_subframe_id.static: false
*tf_plan_subframe_id.name: tf_plan_subframe_id
*tf_plan_subframe_id.parent: bb_plan_query_settings
*tf_plan_subframe_id.width: 43
*tf_plan_subframe_id.isCompound: "true"
*tf_plan_subframe_id.compoundIcon: "textfield.xpm"
*tf_plan_subframe_id.compoundName: "text_Field"
*tf_plan_subframe_id.x: 261
*tf_plan_subframe_id.y: 140
*tf_plan_subframe_id.height: 32
*tf_plan_subframe_id.sensitive: "false"

*om_plan_product_type.class: rowColumn
*om_plan_product_type.name.source: public
*om_plan_product_type.static: false
*om_plan_product_type.name: om_plan_product_type
*om_plan_product_type.parent: bb_plan_query_settings
*om_plan_product_type.rowColumnType: "menu_option"
*om_plan_product_type.subMenuId: "rc_plan_product_type"
*om_plan_product_type.isCompound: "true"
*om_plan_product_type.compoundIcon: "optionM.xpm"
*om_plan_product_type.compoundName: "option_Menu"
*om_plan_product_type.x: 330
*om_plan_product_type.y: 141
*om_plan_product_type.width: 60
*om_plan_product_type.height: 54
*om_plan_product_type.labelString: "Product Type"
*om_plan_product_type.sensitive: "false"

*rc_plan_product_type.class: rowColumn
*rc_plan_product_type.name.source: public
*rc_plan_product_type.static: false
*rc_plan_product_type.name: rc_plan_product_type
*rc_plan_product_type.parent: om_plan_product_type
*rc_plan_product_type.rowColumnType: "menu_pulldown"
*rc_plan_product_type.x: 0
*rc_plan_product_type.y: 88
*rc_plan_product_type.sensitive: "false"

*pb_plan_product_type_any.class: pushButton
*pb_plan_product_type_any.name.source: public
*pb_plan_product_type_any.static: false
*pb_plan_product_type_any.name: pb_plan_product_type_any
*pb_plan_product_type_any.parent: rc_plan_product_type
*pb_plan_product_type_any.labelString: "Any"
*pb_plan_product_type_any.x: 2
*pb_plan_product_type_any.y: 145
*pb_plan_product_type_any.sensitive: "false"
*pb_plan_product_type_any.activateCallback.source: public
*pb_plan_product_type_any.activateCallback: 

*pb_plan_product_type_standard.class: pushButton
*pb_plan_product_type_standard.name.source: public
*pb_plan_product_type_standard.static: false
*pb_plan_product_type_standard.name: pb_plan_product_type_standard
*pb_plan_product_type_standard.parent: rc_plan_product_type
*pb_plan_product_type_standard.labelString: "STANDARD"
*pb_plan_product_type_standard.x: 2
*pb_plan_product_type_standard.y: 145
*pb_plan_product_type_standard.sensitive: "false"
*pb_plan_product_type_standard.activateCallback.source: public
*pb_plan_product_type_standard.activateCallback: 

*rc_plan_product_type_calset.class: pushButton
*rc_plan_product_type_calset.static: true
*rc_plan_product_type_calset.name: rc_plan_product_type_calset
*rc_plan_product_type_calset.parent: rc_plan_product_type
*rc_plan_product_type_calset.labelString: "CAL_SET"
*rc_plan_product_type_calset.activateCallback.source: public
*rc_plan_product_type_calset.activateCallback: 

*rc_plan_product_type_ramp.class: pushButton
*rc_plan_product_type_ramp.static: true
*rc_plan_product_type_ramp.name: rc_plan_product_type_ramp
*rc_plan_product_type_ramp.parent: rc_plan_product_type
*rc_plan_product_type_ramp.labelString: "RAMP"
*rc_plan_product_type_ramp.activateCallback.source: public
*rc_plan_product_type_ramp.activateCallback: 

*rc_plan_product_type_scansar.class: pushButton
*rc_plan_product_type_scansar.static: true
*rc_plan_product_type_scansar.name: rc_plan_product_type_scansar
*rc_plan_product_type_scansar.parent: rc_plan_product_type
*rc_plan_product_type_scansar.labelString: "SCANSAR"

*pb_plan_product_type_complex.class: pushButton
*pb_plan_product_type_complex.name.source: public
*pb_plan_product_type_complex.static: false
*pb_plan_product_type_complex.name: pb_plan_product_type_complex
*pb_plan_product_type_complex.parent: rc_plan_product_type
*pb_plan_product_type_complex.labelString: "COMPLEX"
*pb_plan_product_type_complex.x: 2
*pb_plan_product_type_complex.y: 145
*pb_plan_product_type_complex.sensitive: "false"
*pb_plan_product_type_complex.activateCallback.source: public
*pb_plan_product_type_complex.activateCallback: 

*pb_plan_product_type_ccsd.class: pushButton
*pb_plan_product_type_ccsd.name.source: public
*pb_plan_product_type_ccsd.static: false
*pb_plan_product_type_ccsd.name: pb_plan_product_type_ccsd
*pb_plan_product_type_ccsd.parent: rc_plan_product_type
*pb_plan_product_type_ccsd.labelString: "CCSD"
*pb_plan_product_type_ccsd.x: 2
*pb_plan_product_type_ccsd.y: 145
*pb_plan_product_type_ccsd.sensitive: "false"
*pb_plan_product_type_ccsd.activateCallback.source: public
*pb_plan_product_type_ccsd.activateCallback: 

*om_plan_media_type.class: rowColumn
*om_plan_media_type.name.source: public
*om_plan_media_type.static: false
*om_plan_media_type.name: om_plan_media_type
*om_plan_media_type.parent: bb_plan_query_settings
*om_plan_media_type.rowColumnType: "menu_option"
*om_plan_media_type.subMenuId: "rc_plan_media_type"
*om_plan_media_type.isCompound: "true"
*om_plan_media_type.compoundIcon: "optionM.xpm"
*om_plan_media_type.compoundName: "option_Menu"
*om_plan_media_type.x: 9
*om_plan_media_type.y: 182
*om_plan_media_type.width: 60
*om_plan_media_type.height: 54
*om_plan_media_type.labelString: "Media Type"
*om_plan_media_type.sensitive: "false"

*rc_plan_media_type.class: rowColumn
*rc_plan_media_type.name.source: public
*rc_plan_media_type.static: false
*rc_plan_media_type.name: rc_plan_media_type
*rc_plan_media_type.parent: om_plan_media_type
*rc_plan_media_type.rowColumnType: "menu_pulldown"
*rc_plan_media_type.x: 0
*rc_plan_media_type.y: 217
*rc_plan_media_type.sensitive: "false"

*pb_plan_media_type_any.class: pushButton
*pb_plan_media_type_any.name.source: public
*pb_plan_media_type_any.static: false
*pb_plan_media_type_any.name: pb_plan_media_type_any
*pb_plan_media_type_any.parent: rc_plan_media_type
*pb_plan_media_type_any.labelString: "Any"
*pb_plan_media_type_any.x: 2
*pb_plan_media_type_any.y: 241
*pb_plan_media_type_any.sensitive: "false"
*pb_plan_media_type_any.activateCallback.source: public
*pb_plan_media_type_any.activateCallback: 

*pb_plan_media_type_disk.class: pushButton
*pb_plan_media_type_disk.name.source: public
*pb_plan_media_type_disk.static: false
*pb_plan_media_type_disk.name: pb_plan_media_type_disk
*pb_plan_media_type_disk.parent: rc_plan_media_type
*pb_plan_media_type_disk.labelString: "DISK"
*pb_plan_media_type_disk.x: 2
*pb_plan_media_type_disk.y: 241
*pb_plan_media_type_disk.sensitive: "false"
*pb_plan_media_type_disk.activateCallback.source: public
*pb_plan_media_type_disk.activateCallback: 

*pb_plan_media_type_dcrsi.class: pushButton
*pb_plan_media_type_dcrsi.name.source: public
*pb_plan_media_type_dcrsi.static: false
*pb_plan_media_type_dcrsi.name: pb_plan_media_type_dcrsi
*pb_plan_media_type_dcrsi.parent: rc_plan_media_type
*pb_plan_media_type_dcrsi.labelString: "DCRSI"
*pb_plan_media_type_dcrsi.x: 2
*pb_plan_media_type_dcrsi.y: 241
*pb_plan_media_type_dcrsi.sensitive: "false"
*pb_plan_media_type_dcrsi.activateCallback.source: public
*pb_plan_media_type_dcrsi.activateCallback: 

*rc_plan_media_type_id1.class: pushButton
*rc_plan_media_type_id1.static: true
*rc_plan_media_type_id1.name: rc_plan_media_type_id1
*rc_plan_media_type_id1.parent: rc_plan_media_type
*rc_plan_media_type_id1.labelString: "ID-1"

*l_plan_media_id.class: label
*l_plan_media_id.name.source: public
*l_plan_media_id.static: false
*l_plan_media_id.name: l_plan_media_id
*l_plan_media_id.parent: bb_plan_query_settings
*l_plan_media_id.isCompound: "true"
*l_plan_media_id.compoundIcon: "label.xpm"
*l_plan_media_id.compoundName: "label_"
*l_plan_media_id.x: 218
*l_plan_media_id.y: 185
*l_plan_media_id.width: 82
*l_plan_media_id.height: 32
*l_plan_media_id.labelString: "Media ID"
*l_plan_media_id.sensitive: "false"

*tf_plan_media_id.class: textField
*tf_plan_media_id.name.source: public
*tf_plan_media_id.static: false
*tf_plan_media_id.name: tf_plan_media_id
*tf_plan_media_id.parent: bb_plan_query_settings
*tf_plan_media_id.width: 83
*tf_plan_media_id.isCompound: "true"
*tf_plan_media_id.compoundIcon: "textfield.xpm"
*tf_plan_media_id.compoundName: "text_Field"
*tf_plan_media_id.x: 311
*tf_plan_media_id.y: 185
*tf_plan_media_id.height: 32
*tf_plan_media_id.sensitive: "false"

*pb_plan_query.class: pushButton
*pb_plan_query.name.source: public
*pb_plan_query.static: false
*pb_plan_query.name: pb_plan_query
*pb_plan_query.parent: bb_plan_query_settings
*pb_plan_query.isCompound: "true"
*pb_plan_query.compoundIcon: "push.xpm"
*pb_plan_query.compoundName: "push_Button"
*pb_plan_query.x: 768
*pb_plan_query.y: 183
*pb_plan_query.width: 80
*pb_plan_query.height: 40
*pb_plan_query.labelString: "Query"
*pb_plan_query.activateCallback: {\
do_plan_ready_query();\
}
*pb_plan_query.fontList: "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"
*pb_plan_query.sensitive: "false"

*om_plan_processor_mode.class: rowColumn
*om_plan_processor_mode.name.source: public
*om_plan_processor_mode.static: false
*om_plan_processor_mode.name: om_plan_processor_mode
*om_plan_processor_mode.parent: bb_plan_query_settings
*om_plan_processor_mode.rowColumnType: "menu_option"
*om_plan_processor_mode.subMenuId: "rc_plan_processor_mode"
*om_plan_processor_mode.isCompound: "true"
*om_plan_processor_mode.compoundIcon: "optionM.xpm"
*om_plan_processor_mode.compoundName: "option_Menu"
*om_plan_processor_mode.x: 423
*om_plan_processor_mode.y: 182
*om_plan_processor_mode.width: 202
*om_plan_processor_mode.height: 35
*om_plan_processor_mode.labelString: "Processor Mode"
*om_plan_processor_mode.sensitive: "false"

*rc_plan_processor_mode.class: rowColumn
*rc_plan_processor_mode.name.source: public
*rc_plan_processor_mode.static: false
*rc_plan_processor_mode.name: rc_plan_processor_mode
*rc_plan_processor_mode.parent: om_plan_processor_mode
*rc_plan_processor_mode.rowColumnType: "menu_pulldown"
*rc_plan_processor_mode.sensitive: "false"

*pb_plan_processor_mode_any.class: pushButton
*pb_plan_processor_mode_any.name.source: public
*pb_plan_processor_mode_any.static: false
*pb_plan_processor_mode_any.name: pb_plan_processor_mode_any
*pb_plan_processor_mode_any.parent: rc_plan_processor_mode
*pb_plan_processor_mode_any.labelString: "Any"
*pb_plan_processor_mode_any.sensitive: "false"
*pb_plan_processor_mode_any.activateCallback.source: public
*pb_plan_processor_mode_any.activateCallback: 

*pb_plan_processor_mode_continuous.class: pushButton
*pb_plan_processor_mode_continuous.name.source: public
*pb_plan_processor_mode_continuous.static: false
*pb_plan_processor_mode_continuous.name: pb_plan_processor_mode_continuous
*pb_plan_processor_mode_continuous.parent: rc_plan_processor_mode
*pb_plan_processor_mode_continuous.labelString: "CONTINUOUS"
*pb_plan_processor_mode_continuous.sensitive: "false"

*pb_plan_processor_mode_scansar.class: pushButton
*pb_plan_processor_mode_scansar.name.source: public
*pb_plan_processor_mode_scansar.static: false
*pb_plan_processor_mode_scansar.name: pb_plan_processor_mode_scansar
*pb_plan_processor_mode_scansar.parent: rc_plan_processor_mode
*pb_plan_processor_mode_scansar.labelString: "SCANSAR"
*pb_plan_processor_mode_scansar.sensitive: "false"

*om_plan_data_direction.class: rowColumn
*om_plan_data_direction.name.source: public
*om_plan_data_direction.static: false
*om_plan_data_direction.name: om_plan_data_direction
*om_plan_data_direction.parent: bb_plan_query_settings
*om_plan_data_direction.rowColumnType: "menu_option"
*om_plan_data_direction.subMenuId: "rc_plan_data_direction"
*om_plan_data_direction.isCompound: "true"
*om_plan_data_direction.compoundIcon: "optionM.xpm"
*om_plan_data_direction.compoundName: "option_Menu"
*om_plan_data_direction.x: 600
*om_plan_data_direction.y: 140
*om_plan_data_direction.width: 60
*om_plan_data_direction.height: 54
*om_plan_data_direction.labelString: "Data Direction"
*om_plan_data_direction.sensitive: "false"

*rc_plan_data_direction.class: rowColumn
*rc_plan_data_direction.name.source: public
*rc_plan_data_direction.static: false
*rc_plan_data_direction.name: rc_plan_data_direction
*rc_plan_data_direction.parent: om_plan_data_direction
*rc_plan_data_direction.rowColumnType: "menu_pulldown"
*rc_plan_data_direction.sensitive: "false"

*pb_plan_data_direction_any.class: pushButton
*pb_plan_data_direction_any.name.source: public
*pb_plan_data_direction_any.static: false
*pb_plan_data_direction_any.name: pb_plan_data_direction_any
*pb_plan_data_direction_any.parent: rc_plan_data_direction
*pb_plan_data_direction_any.labelString: "Any"
*pb_plan_data_direction_any.sensitive: "false"

*pb_plan_data_direction_forward.class: pushButton
*pb_plan_data_direction_forward.name.source: public
*pb_plan_data_direction_forward.static: false
*pb_plan_data_direction_forward.name: pb_plan_data_direction_forward
*pb_plan_data_direction_forward.parent: rc_plan_data_direction
*pb_plan_data_direction_forward.labelString: "FORWARD"
*pb_plan_data_direction_forward.sensitive: "false"

*pb_plan_data_direction_reverse.class: pushButton
*pb_plan_data_direction_reverse.name.source: public
*pb_plan_data_direction_reverse.static: false
*pb_plan_data_direction_reverse.name: pb_plan_data_direction_reverse
*pb_plan_data_direction_reverse.parent: rc_plan_data_direction
*pb_plan_data_direction_reverse.labelString: "REVERSE"
*pb_plan_data_direction_reverse.sensitive: "false"

*pb_plan_data_direction_unknown.class: pushButton
*pb_plan_data_direction_unknown.name.source: public
*pb_plan_data_direction_unknown.static: false
*pb_plan_data_direction_unknown.name: pb_plan_data_direction_unknown
*pb_plan_data_direction_unknown.parent: rc_plan_data_direction
*pb_plan_data_direction_unknown.labelString: "UNKNOWN"
*pb_plan_data_direction_unknown.sensitive: "false"

*l_plan_query_settings.class: label
*l_plan_query_settings.static: true
*l_plan_query_settings.name: l_plan_query_settings
*l_plan_query_settings.parent: pps_plan
*l_plan_query_settings.isCompound: "true"
*l_plan_query_settings.compoundIcon: "label.xpm"
*l_plan_query_settings.compoundName: "label_"
*l_plan_query_settings.x: 12
*l_plan_query_settings.y: 33
*l_plan_query_settings.width: 116
*l_plan_query_settings.height: 30
*l_plan_query_settings.labelString: "Query Settings"

*l_plan_planned.class: label
*l_plan_planned.static: true
*l_plan_planned.name: l_plan_planned
*l_plan_planned.parent: pps_plan
*l_plan_planned.isCompound: "true"
*l_plan_planned.compoundIcon: "label.xpm"
*l_plan_planned.compoundName: "label_"
*l_plan_planned.x: 12
*l_plan_planned.y: 586
*l_plan_planned.width: 72
*l_plan_planned.height: 30
*l_plan_planned.labelString: "Planned"

*l_plan_ready.class: label
*l_plan_ready.static: true
*l_plan_ready.name: l_plan_ready
*l_plan_ready.parent: pps_plan
*l_plan_ready.isCompound: "true"
*l_plan_ready.compoundIcon: "label.xpm"
*l_plan_ready.compoundName: "label_"
*l_plan_ready.x: 14
*l_plan_ready.y: 307
*l_plan_ready.width: 185
*l_plan_ready.height: 30
*l_plan_ready.labelString: "Ready to be Planned"

*frame6.class: frame
*frame6.static: true
*frame6.name: frame6
*frame6.parent: pps_plan
*frame6.width: 838
*frame6.height: 241
*frame6.isCompound: "true"
*frame6.compoundIcon: "frame.xpm"
*frame6.compoundName: "frame_"
*frame6.x: 12
*frame6.y: 339

*bulletinBoard15.class: bulletinBoard
*bulletinBoard15.static: true
*bulletinBoard15.name: bulletinBoard15
*bulletinBoard15.parent: frame6
*bulletinBoard15.resizePolicy: "resize_none"
*bulletinBoard15.width: 838
*bulletinBoard15.height: 239
*bulletinBoard15.isCompound: "true"
*bulletinBoard15.compoundIcon: "bboard.xpm"
*bulletinBoard15.compoundName: "bulletin_Board"
*bulletinBoard15.x: 2
*bulletinBoard15.y: 0
*bulletinBoard15.marginHeight: 0
*bulletinBoard15.marginWidth: 0

*scrolledWindowList12.class: scrolledWindow
*scrolledWindowList12.static: true
*scrolledWindowList12.name: scrolledWindowList12
*scrolledWindowList12.parent: bulletinBoard15
*scrolledWindowList12.scrollingPolicy: "application_defined"
*scrolledWindowList12.visualPolicy: "variable"
*scrolledWindowList12.scrollBarDisplayPolicy: "static"
*scrolledWindowList12.shadowThickness: 0
*scrolledWindowList12.isCompound: "true"
*scrolledWindowList12.compoundIcon: "scrllist.xpm"
*scrolledWindowList12.compoundName: "scrolled_List"
*scrolledWindowList12.x: 4
*scrolledWindowList12.y: 44
*scrolledWindowList12.height: 188

*sw_plan_ready_list.class: scrolledList
*sw_plan_ready_list.name.source: public
*sw_plan_ready_list.static: false
*sw_plan_ready_list.name: sw_plan_ready_list
*sw_plan_ready_list.parent: scrolledWindowList12
*sw_plan_ready_list.width: 810
*sw_plan_ready_list.height: 170
*sw_plan_ready_list.scrollBarDisplayPolicy: "static"
*sw_plan_ready_list.itemCount: 0
*sw_plan_ready_list.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1"
*sw_plan_ready_list.listSizePolicy: "constant"
*sw_plan_ready_list.automaticSelection: "false"
*sw_plan_ready_list.selectionPolicy: "extended_select"
*sw_plan_ready_list.extendedSelectionCallback: {\
extern cb_plan_ready_extsel(XmListCallbackStruct *cbs);\
extern cb_plan_available_extsel(XmListCallbackStruct *cbs);\
extern swidget sw_plan_ready_list;\
\
if (UxThisWidget == sw_plan_ready_list)\
	cb_plan_ready_extsel((XmListCallbackStruct *)UxCallbackArg);\
else\
	cb_plan_available_extsel((XmListCallbackStruct *)UxCallbackArg);\
\
}

*l_plan_ready_order_type.class: label
*l_plan_ready_order_type.static: true
*l_plan_ready_order_type.name: l_plan_ready_order_type
*l_plan_ready_order_type.parent: bulletinBoard15
*l_plan_ready_order_type.isCompound: "true"
*l_plan_ready_order_type.compoundIcon: "label.xpm"
*l_plan_ready_order_type.compoundName: "label_"
*l_plan_ready_order_type.x: 3
*l_plan_ready_order_type.y: 5
*l_plan_ready_order_type.width: 64
*l_plan_ready_order_type.height: 32
*l_plan_ready_order_type.labelString: "Order\nType"

*l_plan_ready_priority.class: label
*l_plan_ready_priority.static: true
*l_plan_ready_priority.name: l_plan_ready_priority
*l_plan_ready_priority.parent: bulletinBoard15
*l_plan_ready_priority.isCompound: "true"
*l_plan_ready_priority.compoundIcon: "label.xpm"
*l_plan_ready_priority.compoundName: "label_"
*l_plan_ready_priority.x: 75
*l_plan_ready_priority.y: 5
*l_plan_ready_priority.width: 68
*l_plan_ready_priority.height: 32
*l_plan_ready_priority.labelString: "Priority"

*l_plan_ready_age.class: label
*l_plan_ready_age.static: true
*l_plan_ready_age.name: l_plan_ready_age
*l_plan_ready_age.parent: bulletinBoard15
*l_plan_ready_age.isCompound: "true"
*l_plan_ready_age.compoundIcon: "label.xpm"
*l_plan_ready_age.compoundName: "label_"
*l_plan_ready_age.x: 143
*l_plan_ready_age.y: 5
*l_plan_ready_age.width: 48
*l_plan_ready_age.height: 32
*l_plan_ready_age.labelString: "Age\n(Days)"

*l_plan_ready_sat_sens_rev.class: label
*l_plan_ready_sat_sens_rev.static: true
*l_plan_ready_sat_sens_rev.name: l_plan_ready_sat_sens_rev
*l_plan_ready_sat_sens_rev.parent: bulletinBoard15
*l_plan_ready_sat_sens_rev.isCompound: "true"
*l_plan_ready_sat_sens_rev.compoundIcon: "label.xpm"
*l_plan_ready_sat_sens_rev.compoundName: "label_"
*l_plan_ready_sat_sens_rev.x: 203
*l_plan_ready_sat_sens_rev.y: 5
*l_plan_ready_sat_sens_rev.width: 88
*l_plan_ready_sat_sens_rev.height: 32
*l_plan_ready_sat_sens_rev.labelString: "Datatake ID"

*l_plan_ready_job.class: label
*l_plan_ready_job.static: true
*l_plan_ready_job.name: l_plan_ready_job
*l_plan_ready_job.parent: bulletinBoard15
*l_plan_ready_job.isCompound: "true"
*l_plan_ready_job.compoundIcon: "label.xpm"
*l_plan_ready_job.compoundName: "label_"
*l_plan_ready_job.x: 570
*l_plan_ready_job.y: 4
*l_plan_ready_job.width: 68
*l_plan_ready_job.height: 32
*l_plan_ready_job.labelString: "Job ID"

*l_plan_ready_insert_top.class: label
*l_plan_ready_insert_top.static: true
*l_plan_ready_insert_top.name: l_plan_ready_insert_top
*l_plan_ready_insert_top.parent: bulletinBoard15
*l_plan_ready_insert_top.isCompound: "true"
*l_plan_ready_insert_top.compoundIcon: "label.xpm"
*l_plan_ready_insert_top.compoundName: "label_"
*l_plan_ready_insert_top.x: 760
*l_plan_ready_insert_top.y: 4
*l_plan_ready_insert_top.width: 49
*l_plan_ready_insert_top.height: 32
*l_plan_ready_insert_top.labelString: "Insert\nTop"

*l_plan_ready_mode.class: label
*l_plan_ready_mode.static: true
*l_plan_ready_mode.name: l_plan_ready_mode
*l_plan_ready_mode.parent: bulletinBoard15
*l_plan_ready_mode.isCompound: "true"
*l_plan_ready_mode.compoundIcon: "label.xpm"
*l_plan_ready_mode.compoundName: "label_"
*l_plan_ready_mode.x: 330
*l_plan_ready_mode.y: 4
*l_plan_ready_mode.width: 56
*l_plan_ready_mode.height: 32
*l_plan_ready_mode.labelString: "Mode"

*l_plan_ready_pixel_spacing.class: label
*l_plan_ready_pixel_spacing.static: true
*l_plan_ready_pixel_spacing.name: l_plan_ready_pixel_spacing
*l_plan_ready_pixel_spacing.parent: bulletinBoard15
*l_plan_ready_pixel_spacing.isCompound: "true"
*l_plan_ready_pixel_spacing.compoundIcon: "label.xpm"
*l_plan_ready_pixel_spacing.compoundName: "label_"
*l_plan_ready_pixel_spacing.x: 400
*l_plan_ready_pixel_spacing.y: 4
*l_plan_ready_pixel_spacing.width: 52
*l_plan_ready_pixel_spacing.height: 32
*l_plan_ready_pixel_spacing.labelString: "Pixel\nSpacing"

*l_plan_ready_media_id.class: label
*l_plan_ready_media_id.static: true
*l_plan_ready_media_id.name: l_plan_ready_media_id
*l_plan_ready_media_id.parent: bulletinBoard15
*l_plan_ready_media_id.isCompound: "true"
*l_plan_ready_media_id.compoundIcon: "label.xpm"
*l_plan_ready_media_id.compoundName: "label_"
*l_plan_ready_media_id.x: 480
*l_plan_ready_media_id.y: 4
*l_plan_ready_media_id.width: 76
*l_plan_ready_media_id.height: 31
*l_plan_ready_media_id.labelString: "Media ID"

*l_plan_available_job2.class: label
*l_plan_available_job2.static: true
*l_plan_available_job2.name: l_plan_available_job2
*l_plan_available_job2.parent: bulletinBoard15
*l_plan_available_job2.isCompound: "true"
*l_plan_available_job2.compoundIcon: "label.xpm"
*l_plan_available_job2.compoundName: "label_"
*l_plan_available_job2.x: 640
*l_plan_available_job2.y: 0
*l_plan_available_job2.width: 110
*l_plan_available_job2.height: 40
*l_plan_available_job2.labelString: "Order & Item ID"

*f_plan_ready.class: frame
*f_plan_ready.static: true
*f_plan_ready.name: f_plan_ready
*f_plan_ready.parent: pps_plan
*f_plan_ready.width: 254
*f_plan_ready.height: 126
*f_plan_ready.isCompound: "true"
*f_plan_ready.compoundIcon: "frame.xpm"
*f_plan_ready.compoundName: "frame_"
*f_plan_ready.x: 874
*f_plan_ready.y: 339

*bb_plan_ready.class: bulletinBoard
*bb_plan_ready.static: true
*bb_plan_ready.name: bb_plan_ready
*bb_plan_ready.parent: f_plan_ready
*bb_plan_ready.resizePolicy: "resize_none"
*bb_plan_ready.width: 339
*bb_plan_ready.height: 171
*bb_plan_ready.isCompound: "true"
*bb_plan_ready.compoundIcon: "bboard.xpm"
*bb_plan_ready.compoundName: "bulletin_Board"
*bb_plan_ready.x: 2
*bb_plan_ready.y: 2
*bb_plan_ready.marginHeight: 0
*bb_plan_ready.marginWidth: 0

*pb_plan_ready_top.class: pushButton
*pb_plan_ready_top.static: true
*pb_plan_ready_top.name: pb_plan_ready_top
*pb_plan_ready_top.parent: bb_plan_ready
*pb_plan_ready_top.isCompound: "true"
*pb_plan_ready_top.compoundIcon: "push.xpm"
*pb_plan_ready_top.compoundName: "push_Button"
*pb_plan_ready_top.x: 10
*pb_plan_ready_top.y: 10
*pb_plan_ready_top.width: 112
*pb_plan_ready_top.height: 36
*pb_plan_ready_top.labelString: "TOP of Plan"
*pb_plan_ready_top.activateCallback: cb_plan_ready_top();
*pb_plan_ready_top.sensitive: "true"

*pb_plan_ready_IT_on.class: pushButton
*pb_plan_ready_IT_on.static: true
*pb_plan_ready_IT_on.name: pb_plan_ready_IT_on
*pb_plan_ready_IT_on.parent: bb_plan_ready
*pb_plan_ready_IT_on.isCompound: "true"
*pb_plan_ready_IT_on.compoundIcon: "push.xpm"
*pb_plan_ready_IT_on.compoundName: "push_Button"
*pb_plan_ready_IT_on.x: 130
*pb_plan_ready_IT_on.y: 10
*pb_plan_ready_IT_on.width: 112
*pb_plan_ready_IT_on.height: 36
*pb_plan_ready_IT_on.labelString: "Insert Top On"
*pb_plan_ready_IT_on.activateCallback: cb_plan_ready_IT_on();
*pb_plan_ready_IT_on.sensitive: "true"

*pb_plan_ready_IT_off.class: pushButton
*pb_plan_ready_IT_off.static: true
*pb_plan_ready_IT_off.name: pb_plan_ready_IT_off
*pb_plan_ready_IT_off.parent: bb_plan_ready
*pb_plan_ready_IT_off.isCompound: "true"
*pb_plan_ready_IT_off.compoundIcon: "push.xpm"
*pb_plan_ready_IT_off.compoundName: "push_Button"
*pb_plan_ready_IT_off.x: 130
*pb_plan_ready_IT_off.y: 60
*pb_plan_ready_IT_off.width: 112
*pb_plan_ready_IT_off.height: 36
*pb_plan_ready_IT_off.labelString: "Insert Top Off"
*pb_plan_ready_IT_off.activateCallback: cb_plan_ready_IT_off();
*pb_plan_ready_IT_off.sensitive: "true"

*pb_plan_ready_bottom.class: pushButton
*pb_plan_ready_bottom.static: true
*pb_plan_ready_bottom.name: pb_plan_ready_bottom
*pb_plan_ready_bottom.parent: bb_plan_ready
*pb_plan_ready_bottom.isCompound: "true"
*pb_plan_ready_bottom.compoundIcon: "push.xpm"
*pb_plan_ready_bottom.compoundName: "push_Button"
*pb_plan_ready_bottom.x: 10
*pb_plan_ready_bottom.y: 60
*pb_plan_ready_bottom.width: 112
*pb_plan_ready_bottom.height: 36
*pb_plan_ready_bottom.labelString: "BOTTOM of Plan"
*pb_plan_ready_bottom.activateCallback: cb_plan_ready_bottom();
*pb_plan_ready_bottom.sensitive: "true"

*frame7.class: frame
*frame7.static: true
*frame7.name: frame7
*frame7.parent: pps_plan
*frame7.width: 838
*frame7.height: 243
*frame7.isCompound: "true"
*frame7.compoundIcon: "frame.xpm"
*frame7.compoundName: "frame_"
*frame7.x: 12
*frame7.y: 617

*bulletinBoard2.class: bulletinBoard
*bulletinBoard2.static: true
*bulletinBoard2.name: bulletinBoard2
*bulletinBoard2.parent: frame7
*bulletinBoard2.resizePolicy: "resize_none"
*bulletinBoard2.width: 838
*bulletinBoard2.height: 241
*bulletinBoard2.isCompound: "true"
*bulletinBoard2.compoundIcon: "bboard.xpm"
*bulletinBoard2.compoundName: "bulletin_Board"
*bulletinBoard2.x: 2
*bulletinBoard2.y: 0
*bulletinBoard2.marginHeight: 0
*bulletinBoard2.marginWidth: 0

*scrolledWindowList1.class: scrolledWindow
*scrolledWindowList1.static: true
*scrolledWindowList1.name: scrolledWindowList1
*scrolledWindowList1.parent: bulletinBoard2
*scrolledWindowList1.scrollingPolicy: "application_defined"
*scrolledWindowList1.visualPolicy: "variable"
*scrolledWindowList1.scrollBarDisplayPolicy: "static"
*scrolledWindowList1.shadowThickness: 0
*scrolledWindowList1.isCompound: "true"
*scrolledWindowList1.compoundIcon: "scrllist.xpm"
*scrolledWindowList1.compoundName: "scrolled_List"
*scrolledWindowList1.x: 3
*scrolledWindowList1.y: 42
*scrolledWindowList1.height: 188

*sw_plan_available_list.class: scrolledList
*sw_plan_available_list.name.source: public
*sw_plan_available_list.static: false
*sw_plan_available_list.name: sw_plan_available_list
*sw_plan_available_list.parent: scrolledWindowList1
*sw_plan_available_list.width: 810
*sw_plan_available_list.height: 170
*sw_plan_available_list.scrollBarDisplayPolicy: "static"
*sw_plan_available_list.itemCount: 0
*sw_plan_available_list.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1"
*sw_plan_available_list.listSizePolicy: "constant"
*sw_plan_available_list.automaticSelection: "false"
*sw_plan_available_list.selectionPolicy: "extended_select"
*sw_plan_available_list.extendedSelectionCallback: {\
extern cb_plan_ready_extsel(XmListCallbackStruct *cbs);\
extern cb_plan_available_extsel(XmListCallbackStruct *cbs);\
extern swidget sw_plan_ready_list;\
\
if (UxThisWidget == sw_plan_ready_list)\
	cb_plan_ready_extsel((XmListCallbackStruct *)UxCallbackArg);\
else\
	cb_plan_available_extsel((XmListCallbackStruct *)UxCallbackArg);\
\
}

*l_plan_available_order_type.class: label
*l_plan_available_order_type.static: true
*l_plan_available_order_type.name: l_plan_available_order_type
*l_plan_available_order_type.parent: bulletinBoard2
*l_plan_available_order_type.isCompound: "true"
*l_plan_available_order_type.compoundIcon: "label.xpm"
*l_plan_available_order_type.compoundName: "label_"
*l_plan_available_order_type.x: 4
*l_plan_available_order_type.y: 6
*l_plan_available_order_type.width: 64
*l_plan_available_order_type.height: 32
*l_plan_available_order_type.labelString: "Order\nType"

*l_plan_available_priority.class: label
*l_plan_available_priority.static: true
*l_plan_available_priority.name: l_plan_available_priority
*l_plan_available_priority.parent: bulletinBoard2
*l_plan_available_priority.isCompound: "true"
*l_plan_available_priority.compoundIcon: "label.xpm"
*l_plan_available_priority.compoundName: "label_"
*l_plan_available_priority.x: 76
*l_plan_available_priority.y: 6
*l_plan_available_priority.width: 68
*l_plan_available_priority.height: 32
*l_plan_available_priority.labelString: "Priority"

*l_plan_available_age.class: label
*l_plan_available_age.static: true
*l_plan_available_age.name: l_plan_available_age
*l_plan_available_age.parent: bulletinBoard2
*l_plan_available_age.isCompound: "true"
*l_plan_available_age.compoundIcon: "label.xpm"
*l_plan_available_age.compoundName: "label_"
*l_plan_available_age.x: 144
*l_plan_available_age.y: 6
*l_plan_available_age.width: 48
*l_plan_available_age.height: 32
*l_plan_available_age.labelString: "Age\n(Days)"

*l_plan_available_sat_sens_rev.class: label
*l_plan_available_sat_sens_rev.static: true
*l_plan_available_sat_sens_rev.name: l_plan_available_sat_sens_rev
*l_plan_available_sat_sens_rev.parent: bulletinBoard2
*l_plan_available_sat_sens_rev.isCompound: "true"
*l_plan_available_sat_sens_rev.compoundIcon: "label.xpm"
*l_plan_available_sat_sens_rev.compoundName: "label_"
*l_plan_available_sat_sens_rev.x: 204
*l_plan_available_sat_sens_rev.y: 6
*l_plan_available_sat_sens_rev.width: 88
*l_plan_available_sat_sens_rev.height: 32
*l_plan_available_sat_sens_rev.labelString: "Datatake ID"

*l_plan_available_mode.class: label
*l_plan_available_mode.static: true
*l_plan_available_mode.name: l_plan_available_mode
*l_plan_available_mode.parent: bulletinBoard2
*l_plan_available_mode.isCompound: "true"
*l_plan_available_mode.compoundIcon: "label.xpm"
*l_plan_available_mode.compoundName: "label_"
*l_plan_available_mode.x: 330
*l_plan_available_mode.y: 6
*l_plan_available_mode.width: 49
*l_plan_available_mode.height: 32
*l_plan_available_mode.labelString: "Mode"

*l_plan_available_pixel_spacing.class: label
*l_plan_available_pixel_spacing.static: true
*l_plan_available_pixel_spacing.name: l_plan_available_pixel_spacing
*l_plan_available_pixel_spacing.parent: bulletinBoard2
*l_plan_available_pixel_spacing.isCompound: "true"
*l_plan_available_pixel_spacing.compoundIcon: "label.xpm"
*l_plan_available_pixel_spacing.compoundName: "label_"
*l_plan_available_pixel_spacing.x: 400
*l_plan_available_pixel_spacing.y: 6
*l_plan_available_pixel_spacing.width: 60
*l_plan_available_pixel_spacing.height: 34
*l_plan_available_pixel_spacing.labelString: "Pixel\nSpacing"

*l_plan_available_media_id.class: label
*l_plan_available_media_id.static: true
*l_plan_available_media_id.name: l_plan_available_media_id
*l_plan_available_media_id.parent: bulletinBoard2
*l_plan_available_media_id.isCompound: "true"
*l_plan_available_media_id.compoundIcon: "label.xpm"
*l_plan_available_media_id.compoundName: "label_"
*l_plan_available_media_id.x: 480
*l_plan_available_media_id.y: 6
*l_plan_available_media_id.width: 60
*l_plan_available_media_id.height: 32
*l_plan_available_media_id.labelString: "Media ID"

*l_plan_available_job.class: label
*l_plan_available_job.static: true
*l_plan_available_job.name: l_plan_available_job
*l_plan_available_job.parent: bulletinBoard2
*l_plan_available_job.isCompound: "true"
*l_plan_available_job.compoundIcon: "label.xpm"
*l_plan_available_job.compoundName: "label_"
*l_plan_available_job.x: 570
*l_plan_available_job.y: 6
*l_plan_available_job.width: 60
*l_plan_available_job.height: 32
*l_plan_available_job.labelString: "Job ID"

*l_plan_available_insert_top.class: label
*l_plan_available_insert_top.static: true
*l_plan_available_insert_top.name: l_plan_available_insert_top
*l_plan_available_insert_top.parent: bulletinBoard2
*l_plan_available_insert_top.isCompound: "true"
*l_plan_available_insert_top.compoundIcon: "label.xpm"
*l_plan_available_insert_top.compoundName: "label_"
*l_plan_available_insert_top.x: 760
*l_plan_available_insert_top.y: 6
*l_plan_available_insert_top.width: 63
*l_plan_available_insert_top.height: 32
*l_plan_available_insert_top.labelString: "Insert\nTop"

*l_plan_available_job1.class: label
*l_plan_available_job1.static: true
*l_plan_available_job1.name: l_plan_available_job1
*l_plan_available_job1.parent: bulletinBoard2
*l_plan_available_job1.isCompound: "true"
*l_plan_available_job1.compoundIcon: "label.xpm"
*l_plan_available_job1.compoundName: "label_"
*l_plan_available_job1.x: 640
*l_plan_available_job1.y: 6
*l_plan_available_job1.width: 110
*l_plan_available_job1.height: 34
*l_plan_available_job1.labelString: "Order & Item ID"

*frame3.class: frame
*frame3.static: true
*frame3.name: frame3
*frame3.parent: pps_plan
*frame3.width: 216
*frame3.height: 236
*frame3.isCompound: "true"
*frame3.compoundIcon: "frame.xpm"
*frame3.compoundName: "frame_"
*frame3.x: 893
*frame3.y: 62

*bulletinBoard4.class: bulletinBoard
*bulletinBoard4.static: true
*bulletinBoard4.name: bulletinBoard4
*bulletinBoard4.parent: frame3
*bulletinBoard4.resizePolicy: "resize_none"
*bulletinBoard4.width: 250
*bulletinBoard4.height: 228
*bulletinBoard4.isCompound: "true"
*bulletinBoard4.compoundIcon: "bboard.xpm"
*bulletinBoard4.compoundName: "bulletin_Board"
*bulletinBoard4.x: 0
*bulletinBoard4.y: 0

*om_plan_order_first.class: rowColumn
*om_plan_order_first.name.source: public
*om_plan_order_first.static: false
*om_plan_order_first.name: om_plan_order_first
*om_plan_order_first.parent: bulletinBoard4
*om_plan_order_first.rowColumnType: "menu_option"
*om_plan_order_first.subMenuId: "rc_plan_order_first"
*om_plan_order_first.isCompound: "true"
*om_plan_order_first.compoundIcon: "optionM.xpm"
*om_plan_order_first.compoundName: "option_Menu"
*om_plan_order_first.x: 12
*om_plan_order_first.y: 14
*om_plan_order_first.width: 60
*om_plan_order_first.height: 54
*om_plan_order_first.labelString: "First    "
*om_plan_order_first.sensitive: "false"

*rc_plan_order_first.class: rowColumn
*rc_plan_order_first.name.source: public
*rc_plan_order_first.static: false
*rc_plan_order_first.name: rc_plan_order_first
*rc_plan_order_first.parent: om_plan_order_first
*rc_plan_order_first.rowColumnType: "menu_pulldown"
*rc_plan_order_first.sensitive: "false"
*rc_plan_order_first.x: 93
*rc_plan_order_first.y: 0

*pb_plan_order_first_none.class: pushButton
*pb_plan_order_first_none.name.source: public
*pb_plan_order_first_none.static: false
*pb_plan_order_first_none.name: pb_plan_order_first_none
*pb_plan_order_first_none.parent: rc_plan_order_first
*pb_plan_order_first_none.labelString: "None"
*pb_plan_order_first_none.sensitive: "false"
*pb_plan_order_first_none.x: 97
*pb_plan_order_first_none.y: 2
*pb_plan_order_first_none.activateCallback.source: public
*pb_plan_order_first_none.activateCallback: 

*pb_plan_order_first_order_type.class: pushButton
*pb_plan_order_first_order_type.name.source: public
*pb_plan_order_first_order_type.static: false
*pb_plan_order_first_order_type.name: pb_plan_order_first_order_type
*pb_plan_order_first_order_type.parent: rc_plan_order_first
*pb_plan_order_first_order_type.labelString: "Order Type"
*pb_plan_order_first_order_type.x: 97
*pb_plan_order_first_order_type.y: 24
*pb_plan_order_first_order_type.activateCallback.source: public
*pb_plan_order_first_order_type.activateCallback: 

*pb_plan_order_first_priority.class: pushButton
*pb_plan_order_first_priority.name.source: public
*pb_plan_order_first_priority.static: false
*pb_plan_order_first_priority.name: pb_plan_order_first_priority
*pb_plan_order_first_priority.parent: rc_plan_order_first
*pb_plan_order_first_priority.labelString: "Priority"
*pb_plan_order_first_priority.x: 97
*pb_plan_order_first_priority.y: 46
*pb_plan_order_first_priority.activateCallback.source: public
*pb_plan_order_first_priority.activateCallback: 

*pb_plan_order_first_age.class: pushButton
*pb_plan_order_first_age.static: true
*pb_plan_order_first_age.name: pb_plan_order_first_age
*pb_plan_order_first_age.parent: rc_plan_order_first
*pb_plan_order_first_age.labelString: "Age (Days)"
*pb_plan_order_first_age.x: 97
*pb_plan_order_first_age.y: 222
*pb_plan_order_first_age.activateCallback.source: public
*pb_plan_order_first_age.activateCallback: 

*pb_plan_order_first_sat_sens_rev.class: pushButton
*pb_plan_order_first_sat_sens_rev.name.source: public
*pb_plan_order_first_sat_sens_rev.static: false
*pb_plan_order_first_sat_sens_rev.name: pb_plan_order_first_sat_sens_rev
*pb_plan_order_first_sat_sens_rev.parent: rc_plan_order_first
*pb_plan_order_first_sat_sens_rev.labelString: "Datatake ID"
*pb_plan_order_first_sat_sens_rev.x: 97
*pb_plan_order_first_sat_sens_rev.y: 112

*pb_plan_order_first_mode.class: pushButton
*pb_plan_order_first_mode.name.source: public
*pb_plan_order_first_mode.static: false
*pb_plan_order_first_mode.name: pb_plan_order_first_mode
*pb_plan_order_first_mode.parent: rc_plan_order_first
*pb_plan_order_first_mode.labelString: "Mode"
*pb_plan_order_first_mode.x: 97
*pb_plan_order_first_mode.y: 90
*pb_plan_order_first_mode.activateCallback.source: public
*pb_plan_order_first_mode.activateCallback: 

*pb_plan_order_first_pixel_spacing.class: pushButton
*pb_plan_order_first_pixel_spacing.name.source: public
*pb_plan_order_first_pixel_spacing.static: false
*pb_plan_order_first_pixel_spacing.name: pb_plan_order_first_pixel_spacing
*pb_plan_order_first_pixel_spacing.parent: rc_plan_order_first
*pb_plan_order_first_pixel_spacing.labelString: "Pixel Spacing"
*pb_plan_order_first_pixel_spacing.x: 97
*pb_plan_order_first_pixel_spacing.y: 134
*pb_plan_order_first_pixel_spacing.activateCallback.source: public
*pb_plan_order_first_pixel_spacing.activateCallback: 

*pb_plan_order_first_media_id.class: pushButton
*pb_plan_order_first_media_id.name.source: public
*pb_plan_order_first_media_id.static: false
*pb_plan_order_first_media_id.name: pb_plan_order_first_media_id
*pb_plan_order_first_media_id.parent: rc_plan_order_first
*pb_plan_order_first_media_id.labelString: "Media ID"
*pb_plan_order_first_media_id.x: 97
*pb_plan_order_first_media_id.y: 68
*pb_plan_order_first_media_id.activateCallback.source: public
*pb_plan_order_first_media_id.activateCallback: 

*pb_plan_order_first_job_id.class: pushButton
*pb_plan_order_first_job_id.name.source: public
*pb_plan_order_first_job_id.static: false
*pb_plan_order_first_job_id.name: pb_plan_order_first_job_id
*pb_plan_order_first_job_id.parent: rc_plan_order_first
*pb_plan_order_first_job_id.labelString: "Job ID"
*pb_plan_order_first_job_id.x: 97
*pb_plan_order_first_job_id.y: 156
*pb_plan_order_first_job_id.activateCallback.source: public
*pb_plan_order_first_job_id.activateCallback: 

*pb_plan_order_first_order_item.class: pushButton
*pb_plan_order_first_order_item.static: true
*pb_plan_order_first_order_item.name: pb_plan_order_first_order_item
*pb_plan_order_first_order_item.parent: rc_plan_order_first
*pb_plan_order_first_order_item.labelString: "Order & Item ID"
*pb_plan_order_first_order_item.activateCallback.source: public
*pb_plan_order_first_order_item.activateCallback: 

*pb_plan_order_first_insert_top.class: pushButton
*pb_plan_order_first_insert_top.name.source: public
*pb_plan_order_first_insert_top.static: false
*pb_plan_order_first_insert_top.name: pb_plan_order_first_insert_top
*pb_plan_order_first_insert_top.parent: rc_plan_order_first
*pb_plan_order_first_insert_top.labelString: "Insert Top"
*pb_plan_order_first_insert_top.x: 97
*pb_plan_order_first_insert_top.y: 200
*pb_plan_order_first_insert_top.activateCallback.source: public
*pb_plan_order_first_insert_top.activateCallback: 

*om_plan_order_second.class: rowColumn
*om_plan_order_second.name.source: public
*om_plan_order_second.static: false
*om_plan_order_second.name: om_plan_order_second
*om_plan_order_second.parent: bulletinBoard4
*om_plan_order_second.rowColumnType: "menu_option"
*om_plan_order_second.subMenuId: "rc_plan_order_second"
*om_plan_order_second.isCompound: "true"
*om_plan_order_second.compoundIcon: "optionM.xpm"
*om_plan_order_second.compoundName: "option_Menu"
*om_plan_order_second.x: 11
*om_plan_order_second.y: 67
*om_plan_order_second.width: 60
*om_plan_order_second.height: 54
*om_plan_order_second.labelString: "Second"
*om_plan_order_second.sensitive: "false"

*rc_plan_order_second.class: rowColumn
*rc_plan_order_second.name.source: public
*rc_plan_order_second.static: false
*rc_plan_order_second.name: rc_plan_order_second
*rc_plan_order_second.parent: om_plan_order_second
*rc_plan_order_second.rowColumnType: "menu_pulldown"
*rc_plan_order_second.sensitive: "false"
*rc_plan_order_second.x: 93
*rc_plan_order_second.y: 0

*pb_plan_order_second_none.class: pushButton
*pb_plan_order_second_none.name.source: public
*pb_plan_order_second_none.static: false
*pb_plan_order_second_none.name: pb_plan_order_second_none
*pb_plan_order_second_none.parent: rc_plan_order_second
*pb_plan_order_second_none.labelString: "None"
*pb_plan_order_second_none.sensitive: "false"
*pb_plan_order_second_none.x: 97
*pb_plan_order_second_none.y: 2
*pb_plan_order_second_none.activateCallback.source: public
*pb_plan_order_second_none.activateCallback: 

*pb_plan_order_second_order_type.class: pushButton
*pb_plan_order_second_order_type.name.source: public
*pb_plan_order_second_order_type.static: false
*pb_plan_order_second_order_type.name: pb_plan_order_second_order_type
*pb_plan_order_second_order_type.parent: rc_plan_order_second
*pb_plan_order_second_order_type.labelString: "Order Type"
*pb_plan_order_second_order_type.x: 97
*pb_plan_order_second_order_type.y: 24
*pb_plan_order_second_order_type.activateCallback.source: public
*pb_plan_order_second_order_type.activateCallback: 

*pb_plan_order_second_priority.class: pushButton
*pb_plan_order_second_priority.name.source: public
*pb_plan_order_second_priority.static: false
*pb_plan_order_second_priority.name: pb_plan_order_second_priority
*pb_plan_order_second_priority.parent: rc_plan_order_second
*pb_plan_order_second_priority.labelString: "Priority"
*pb_plan_order_second_priority.x: 97
*pb_plan_order_second_priority.y: 46
*pb_plan_order_second_priority.activateCallback.source: public
*pb_plan_order_second_priority.activateCallback: 

*pb_plan_order_second_age.class: pushButton
*pb_plan_order_second_age.static: true
*pb_plan_order_second_age.name: pb_plan_order_second_age
*pb_plan_order_second_age.parent: rc_plan_order_second
*pb_plan_order_second_age.labelString: "Age (Days)"
*pb_plan_order_second_age.x: 97
*pb_plan_order_second_age.y: 222
*pb_plan_order_second_age.activateCallback.source: public
*pb_plan_order_second_age.activateCallback: 

*pb_plan_order_second_sat_sens_rev.class: pushButton
*pb_plan_order_second_sat_sens_rev.name.source: public
*pb_plan_order_second_sat_sens_rev.static: false
*pb_plan_order_second_sat_sens_rev.name: pb_plan_order_second_sat_sens_rev
*pb_plan_order_second_sat_sens_rev.parent: rc_plan_order_second
*pb_plan_order_second_sat_sens_rev.labelString: "Datatake ID"
*pb_plan_order_second_sat_sens_rev.x: 97
*pb_plan_order_second_sat_sens_rev.y: 112

*pb_plan_order_second_mode.class: pushButton
*pb_plan_order_second_mode.name.source: public
*pb_plan_order_second_mode.static: false
*pb_plan_order_second_mode.name: pb_plan_order_second_mode
*pb_plan_order_second_mode.parent: rc_plan_order_second
*pb_plan_order_second_mode.labelString: "Mode"
*pb_plan_order_second_mode.x: 97
*pb_plan_order_second_mode.y: 90
*pb_plan_order_second_mode.activateCallback.source: public
*pb_plan_order_second_mode.activateCallback: 

*pb_plan_order_second_pixel_spacing.class: pushButton
*pb_plan_order_second_pixel_spacing.name.source: public
*pb_plan_order_second_pixel_spacing.static: false
*pb_plan_order_second_pixel_spacing.name: pb_plan_order_second_pixel_spacing
*pb_plan_order_second_pixel_spacing.parent: rc_plan_order_second
*pb_plan_order_second_pixel_spacing.labelString: "Pixel Spacing"
*pb_plan_order_second_pixel_spacing.x: 97
*pb_plan_order_second_pixel_spacing.y: 134
*pb_plan_order_second_pixel_spacing.activateCallback.source: public
*pb_plan_order_second_pixel_spacing.activateCallback: 

*pb_plan_order_second_media_id.class: pushButton
*pb_plan_order_second_media_id.name.source: public
*pb_plan_order_second_media_id.static: false
*pb_plan_order_second_media_id.name: pb_plan_order_second_media_id
*pb_plan_order_second_media_id.parent: rc_plan_order_second
*pb_plan_order_second_media_id.labelString: "Media ID"
*pb_plan_order_second_media_id.x: 97
*pb_plan_order_second_media_id.y: 68
*pb_plan_order_second_media_id.activateCallback.source: public
*pb_plan_order_second_media_id.activateCallback: 

*pb_plan_order_second_job_id.class: pushButton
*pb_plan_order_second_job_id.name.source: public
*pb_plan_order_second_job_id.static: false
*pb_plan_order_second_job_id.name: pb_plan_order_second_job_id
*pb_plan_order_second_job_id.parent: rc_plan_order_second
*pb_plan_order_second_job_id.labelString: "Job ID"
*pb_plan_order_second_job_id.x: 97
*pb_plan_order_second_job_id.y: 156
*pb_plan_order_second_job_id.activateCallback.source: public
*pb_plan_order_second_job_id.activateCallback: 

*pb_plan_order_second_order_item.class: pushButton
*pb_plan_order_second_order_item.static: true
*pb_plan_order_second_order_item.name: pb_plan_order_second_order_item
*pb_plan_order_second_order_item.parent: rc_plan_order_second
*pb_plan_order_second_order_item.labelString: "Order & Item ID"
*pb_plan_order_second_order_item.activateCallback.source: public
*pb_plan_order_second_order_item.activateCallback: 

*pb_plan_order_second_insert_top.class: pushButton
*pb_plan_order_second_insert_top.name.source: public
*pb_plan_order_second_insert_top.static: false
*pb_plan_order_second_insert_top.name: pb_plan_order_second_insert_top
*pb_plan_order_second_insert_top.parent: rc_plan_order_second
*pb_plan_order_second_insert_top.labelString: "Insert Top"
*pb_plan_order_second_insert_top.x: 97
*pb_plan_order_second_insert_top.y: 200
*pb_plan_order_second_insert_top.activateCallback.source: public
*pb_plan_order_second_insert_top.activateCallback: 

*om_plan_order_third.class: rowColumn
*om_plan_order_third.name.source: public
*om_plan_order_third.static: false
*om_plan_order_third.name: om_plan_order_third
*om_plan_order_third.parent: bulletinBoard4
*om_plan_order_third.rowColumnType: "menu_option"
*om_plan_order_third.subMenuId: "rc_plan_order_third"
*om_plan_order_third.isCompound: "true"
*om_plan_order_third.compoundIcon: "optionM.xpm"
*om_plan_order_third.compoundName: "option_Menu"
*om_plan_order_third.x: 11
*om_plan_order_third.y: 120
*om_plan_order_third.width: 60
*om_plan_order_third.height: 54
*om_plan_order_third.labelString: "Third    "
*om_plan_order_third.sensitive: "false"

*rc_plan_order_third.class: rowColumn
*rc_plan_order_third.name.source: public
*rc_plan_order_third.static: false
*rc_plan_order_third.name: rc_plan_order_third
*rc_plan_order_third.parent: om_plan_order_third
*rc_plan_order_third.rowColumnType: "menu_pulldown"
*rc_plan_order_third.sensitive: "false"
*rc_plan_order_third.x: 93
*rc_plan_order_third.y: 0

*pb_plan_order_third_none.class: pushButton
*pb_plan_order_third_none.name.source: public
*pb_plan_order_third_none.static: false
*pb_plan_order_third_none.name: pb_plan_order_third_none
*pb_plan_order_third_none.parent: rc_plan_order_third
*pb_plan_order_third_none.labelString: "None"
*pb_plan_order_third_none.sensitive: "false"
*pb_plan_order_third_none.x: 97
*pb_plan_order_third_none.y: 2
*pb_plan_order_third_none.activateCallback.source: public
*pb_plan_order_third_none.activateCallback: 

*pb_plan_order_third_order_type.class: pushButton
*pb_plan_order_third_order_type.name.source: public
*pb_plan_order_third_order_type.static: false
*pb_plan_order_third_order_type.name: pb_plan_order_third_order_type
*pb_plan_order_third_order_type.parent: rc_plan_order_third
*pb_plan_order_third_order_type.labelString: "Order Type"
*pb_plan_order_third_order_type.x: 97
*pb_plan_order_third_order_type.y: 24
*pb_plan_order_third_order_type.activateCallback.source: public
*pb_plan_order_third_order_type.activateCallback: 

*pb_plan_order_third_priority.class: pushButton
*pb_plan_order_third_priority.name.source: public
*pb_plan_order_third_priority.static: false
*pb_plan_order_third_priority.name: pb_plan_order_third_priority
*pb_plan_order_third_priority.parent: rc_plan_order_third
*pb_plan_order_third_priority.labelString: "Priority"
*pb_plan_order_third_priority.x: 97
*pb_plan_order_third_priority.y: 46
*pb_plan_order_third_priority.activateCallback.source: public
*pb_plan_order_third_priority.activateCallback: 

*pb_plan_order_third_age.class: pushButton
*pb_plan_order_third_age.static: true
*pb_plan_order_third_age.name: pb_plan_order_third_age
*pb_plan_order_third_age.parent: rc_plan_order_third
*pb_plan_order_third_age.labelString: "Age (Days)"
*pb_plan_order_third_age.x: 97
*pb_plan_order_third_age.y: 222
*pb_plan_order_third_age.activateCallback.source: public
*pb_plan_order_third_age.activateCallback: 

*pb_plan_order_third_sat_sens_rev.class: pushButton
*pb_plan_order_third_sat_sens_rev.name.source: public
*pb_plan_order_third_sat_sens_rev.static: false
*pb_plan_order_third_sat_sens_rev.name: pb_plan_order_third_sat_sens_rev
*pb_plan_order_third_sat_sens_rev.parent: rc_plan_order_third
*pb_plan_order_third_sat_sens_rev.labelString: "Datatake ID"
*pb_plan_order_third_sat_sens_rev.x: 97
*pb_plan_order_third_sat_sens_rev.y: 112

*pb_plan_order_third_mode.class: pushButton
*pb_plan_order_third_mode.name.source: public
*pb_plan_order_third_mode.static: false
*pb_plan_order_third_mode.name: pb_plan_order_third_mode
*pb_plan_order_third_mode.parent: rc_plan_order_third
*pb_plan_order_third_mode.labelString: "Mode"
*pb_plan_order_third_mode.x: 97
*pb_plan_order_third_mode.y: 90
*pb_plan_order_third_mode.activateCallback.source: public
*pb_plan_order_third_mode.activateCallback: 

*pb_plan_order_third_pixel_spacing.class: pushButton
*pb_plan_order_third_pixel_spacing.name.source: public
*pb_plan_order_third_pixel_spacing.static: false
*pb_plan_order_third_pixel_spacing.name: pb_plan_order_third_pixel_spacing
*pb_plan_order_third_pixel_spacing.parent: rc_plan_order_third
*pb_plan_order_third_pixel_spacing.labelString: "Pixel Spacing"
*pb_plan_order_third_pixel_spacing.x: 97
*pb_plan_order_third_pixel_spacing.y: 178
*pb_plan_order_third_pixel_spacing.activateCallback.source: public
*pb_plan_order_third_pixel_spacing.activateCallback: 

*pb_plan_order_third_media_id.class: pushButton
*pb_plan_order_third_media_id.name.source: public
*pb_plan_order_third_media_id.static: false
*pb_plan_order_third_media_id.name: pb_plan_order_third_media_id
*pb_plan_order_third_media_id.parent: rc_plan_order_third
*pb_plan_order_third_media_id.labelString: "Media ID"
*pb_plan_order_third_media_id.x: 97
*pb_plan_order_third_media_id.y: 68
*pb_plan_order_third_media_id.activateCallback.source: public
*pb_plan_order_third_media_id.activateCallback: 

*pb_plan_order_third_job_id.class: pushButton
*pb_plan_order_third_job_id.name.source: public
*pb_plan_order_third_job_id.static: false
*pb_plan_order_third_job_id.name: pb_plan_order_third_job_id
*pb_plan_order_third_job_id.parent: rc_plan_order_third
*pb_plan_order_third_job_id.labelString: "Job ID"
*pb_plan_order_third_job_id.x: 97
*pb_plan_order_third_job_id.y: 156
*pb_plan_order_third_job_id.activateCallback.source: public
*pb_plan_order_third_job_id.activateCallback: 

*pb_plan_order_third_order_item.class: pushButton
*pb_plan_order_third_order_item.static: true
*pb_plan_order_third_order_item.name: pb_plan_order_third_order_item
*pb_plan_order_third_order_item.parent: rc_plan_order_third
*pb_plan_order_third_order_item.labelString: "Order & Item ID"
*pb_plan_order_third_order_item.activateCallback.source: public
*pb_plan_order_third_order_item.activateCallback: 

*pb_plan_order_third_insert_top.class: pushButton
*pb_plan_order_third_insert_top.name.source: public
*pb_plan_order_third_insert_top.static: false
*pb_plan_order_third_insert_top.name: pb_plan_order_third_insert_top
*pb_plan_order_third_insert_top.parent: rc_plan_order_third
*pb_plan_order_third_insert_top.labelString: "Insert Top"
*pb_plan_order_third_insert_top.x: 97
*pb_plan_order_third_insert_top.y: 200
*pb_plan_order_third_insert_top.activateCallback.source: public
*pb_plan_order_third_insert_top.activateCallback: 

*om_plan_order_fourth.class: rowColumn
*om_plan_order_fourth.name.source: public
*om_plan_order_fourth.static: false
*om_plan_order_fourth.name: om_plan_order_fourth
*om_plan_order_fourth.parent: bulletinBoard4
*om_plan_order_fourth.rowColumnType: "menu_option"
*om_plan_order_fourth.subMenuId: "rc_plan_order_fourth"
*om_plan_order_fourth.isCompound: "true"
*om_plan_order_fourth.compoundIcon: "optionM.xpm"
*om_plan_order_fourth.compoundName: "option_Menu"
*om_plan_order_fourth.x: 11
*om_plan_order_fourth.y: 175
*om_plan_order_fourth.width: 60
*om_plan_order_fourth.height: 54
*om_plan_order_fourth.labelString: "Fourth  "
*om_plan_order_fourth.sensitive: "false"

*rc_plan_order_fourth.class: rowColumn
*rc_plan_order_fourth.name.source: public
*rc_plan_order_fourth.static: false
*rc_plan_order_fourth.name: rc_plan_order_fourth
*rc_plan_order_fourth.parent: om_plan_order_fourth
*rc_plan_order_fourth.rowColumnType: "menu_pulldown"
*rc_plan_order_fourth.sensitive: "false"
*rc_plan_order_fourth.x: 93
*rc_plan_order_fourth.y: 0

*pb_plan_order_fourth_none.class: pushButton
*pb_plan_order_fourth_none.name.source: public
*pb_plan_order_fourth_none.static: false
*pb_plan_order_fourth_none.name: pb_plan_order_fourth_none
*pb_plan_order_fourth_none.parent: rc_plan_order_fourth
*pb_plan_order_fourth_none.labelString: "None"
*pb_plan_order_fourth_none.sensitive: "false"
*pb_plan_order_fourth_none.x: 97
*pb_plan_order_fourth_none.y: 2
*pb_plan_order_fourth_none.activateCallback.source: public
*pb_plan_order_fourth_none.activateCallback: 

*pb_plan_order_fourth_order_type.class: pushButton
*pb_plan_order_fourth_order_type.name.source: public
*pb_plan_order_fourth_order_type.static: false
*pb_plan_order_fourth_order_type.name: pb_plan_order_fourth_order_type
*pb_plan_order_fourth_order_type.parent: rc_plan_order_fourth
*pb_plan_order_fourth_order_type.labelString: "Order Type"
*pb_plan_order_fourth_order_type.x: 97
*pb_plan_order_fourth_order_type.y: 24
*pb_plan_order_fourth_order_type.activateCallback.source: public
*pb_plan_order_fourth_order_type.activateCallback: 

*pb_plan_order_fourth_priority.class: pushButton
*pb_plan_order_fourth_priority.name.source: public
*pb_plan_order_fourth_priority.static: false
*pb_plan_order_fourth_priority.name: pb_plan_order_fourth_priority
*pb_plan_order_fourth_priority.parent: rc_plan_order_fourth
*pb_plan_order_fourth_priority.labelString: "Priority"
*pb_plan_order_fourth_priority.x: 97
*pb_plan_order_fourth_priority.y: 46
*pb_plan_order_fourth_priority.activateCallback.source: public
*pb_plan_order_fourth_priority.activateCallback: 

*pb_plan_order_fourth_age.class: pushButton
*pb_plan_order_fourth_age.static: true
*pb_plan_order_fourth_age.name: pb_plan_order_fourth_age
*pb_plan_order_fourth_age.parent: rc_plan_order_fourth
*pb_plan_order_fourth_age.labelString: "Age (Days)"
*pb_plan_order_fourth_age.x: 97
*pb_plan_order_fourth_age.y: 222
*pb_plan_order_fourth_age.activateCallback.source: public
*pb_plan_order_fourth_age.activateCallback: 

*pb_plan_order_fourth_sat_sens_rev.class: pushButton
*pb_plan_order_fourth_sat_sens_rev.name.source: public
*pb_plan_order_fourth_sat_sens_rev.static: false
*pb_plan_order_fourth_sat_sens_rev.name: pb_plan_order_fourth_sat_sens_rev
*pb_plan_order_fourth_sat_sens_rev.parent: rc_plan_order_fourth
*pb_plan_order_fourth_sat_sens_rev.labelString: "Datatake ID"
*pb_plan_order_fourth_sat_sens_rev.x: 97
*pb_plan_order_fourth_sat_sens_rev.y: 112

*pb_plan_order_fourth_mode.class: pushButton
*pb_plan_order_fourth_mode.name.source: public
*pb_plan_order_fourth_mode.static: false
*pb_plan_order_fourth_mode.name: pb_plan_order_fourth_mode
*pb_plan_order_fourth_mode.parent: rc_plan_order_fourth
*pb_plan_order_fourth_mode.labelString: "Mode"
*pb_plan_order_fourth_mode.x: 97
*pb_plan_order_fourth_mode.y: 90
*pb_plan_order_fourth_mode.activateCallback.source: public
*pb_plan_order_fourth_mode.activateCallback: 

*pb_plan_order_fourth_pixel_spacing.class: pushButton
*pb_plan_order_fourth_pixel_spacing.name.source: public
*pb_plan_order_fourth_pixel_spacing.static: false
*pb_plan_order_fourth_pixel_spacing.name: pb_plan_order_fourth_pixel_spacing
*pb_plan_order_fourth_pixel_spacing.parent: rc_plan_order_fourth
*pb_plan_order_fourth_pixel_spacing.labelString: "Pixel Spacing"
*pb_plan_order_fourth_pixel_spacing.x: 97
*pb_plan_order_fourth_pixel_spacing.y: 134
*pb_plan_order_fourth_pixel_spacing.activateCallback.source: public
*pb_plan_order_fourth_pixel_spacing.activateCallback: 

*pb_plan_order_fourth_media_id.class: pushButton
*pb_plan_order_fourth_media_id.name.source: public
*pb_plan_order_fourth_media_id.static: false
*pb_plan_order_fourth_media_id.name: pb_plan_order_fourth_media_id
*pb_plan_order_fourth_media_id.parent: rc_plan_order_fourth
*pb_plan_order_fourth_media_id.labelString: "Media ID"
*pb_plan_order_fourth_media_id.x: 97
*pb_plan_order_fourth_media_id.y: 68
*pb_plan_order_fourth_media_id.activateCallback.source: public
*pb_plan_order_fourth_media_id.activateCallback: 

*pb_plan_order_fourth_job_id.class: pushButton
*pb_plan_order_fourth_job_id.name.source: public
*pb_plan_order_fourth_job_id.static: false
*pb_plan_order_fourth_job_id.name: pb_plan_order_fourth_job_id
*pb_plan_order_fourth_job_id.parent: rc_plan_order_fourth
*pb_plan_order_fourth_job_id.labelString: "Job ID"
*pb_plan_order_fourth_job_id.x: 97
*pb_plan_order_fourth_job_id.y: 156
*pb_plan_order_fourth_job_id.activateCallback.source: public
*pb_plan_order_fourth_job_id.activateCallback: 

*pb_plan_order_fourth_order_item.class: pushButton
*pb_plan_order_fourth_order_item.static: true
*pb_plan_order_fourth_order_item.name: pb_plan_order_fourth_order_item
*pb_plan_order_fourth_order_item.parent: rc_plan_order_fourth
*pb_plan_order_fourth_order_item.labelString: "Order & Item ID"
*pb_plan_order_fourth_order_item.activateCallback.source: public
*pb_plan_order_fourth_order_item.activateCallback: 

*pb_plan_order_fourth_insert_top.class: pushButton
*pb_plan_order_fourth_insert_top.name.source: public
*pb_plan_order_fourth_insert_top.static: false
*pb_plan_order_fourth_insert_top.name: pb_plan_order_fourth_insert_top
*pb_plan_order_fourth_insert_top.parent: rc_plan_order_fourth
*pb_plan_order_fourth_insert_top.labelString: "Insert Top"
*pb_plan_order_fourth_insert_top.x: 97
*pb_plan_order_fourth_insert_top.y: 178
*pb_plan_order_fourth_insert_top.activateCallback.source: public
*pb_plan_order_fourth_insert_top.activateCallback: 

*l_plan_order_by.class: label
*l_plan_order_by.static: true
*l_plan_order_by.name: l_plan_order_by
*l_plan_order_by.parent: pps_plan
*l_plan_order_by.isCompound: "true"
*l_plan_order_by.compoundIcon: "label.xpm"
*l_plan_order_by.compoundName: "label_"
*l_plan_order_by.x: 894
*l_plan_order_by.y: 35
*l_plan_order_by.width: 71
*l_plan_order_by.height: 24
*l_plan_order_by.labelString: "Order By"

*f_plan_items_ready.class: frame
*f_plan_items_ready.static: true
*f_plan_items_ready.name: f_plan_items_ready
*f_plan_items_ready.parent: pps_plan
*f_plan_items_ready.width: 260
*f_plan_items_ready.height: 56
*f_plan_items_ready.isCompound: "true"
*f_plan_items_ready.compoundIcon: "frame.xpm"
*f_plan_items_ready.compoundName: "frame_"
*f_plan_items_ready.x: 870
*f_plan_items_ready.y: 484

*bb_plan_items_ready.class: bulletinBoard
*bb_plan_items_ready.static: true
*bb_plan_items_ready.name: bb_plan_items_ready
*bb_plan_items_ready.parent: f_plan_items_ready
*bb_plan_items_ready.resizePolicy: "resize_none"
*bb_plan_items_ready.width: 276
*bb_plan_items_ready.height: 64
*bb_plan_items_ready.isCompound: "true"
*bb_plan_items_ready.compoundIcon: "bboard.xpm"
*bb_plan_items_ready.compoundName: "bulletin_Board"
*bb_plan_items_ready.x: 2
*bb_plan_items_ready.y: 2
*bb_plan_items_ready.marginHeight: 0
*bb_plan_items_ready.marginWidth: 0

*l_query_items_found1.class: label
*l_query_items_found1.static: true
*l_query_items_found1.name: l_query_items_found1
*l_query_items_found1.parent: bb_plan_items_ready
*l_query_items_found1.isCompound: "true"
*l_query_items_found1.compoundIcon: "label.xpm"
*l_query_items_found1.compoundName: "label_"
*l_query_items_found1.x: 50
*l_query_items_found1.y: 0
*l_query_items_found1.width: 108
*l_query_items_found1.height: 22
*l_query_items_found1.labelString: "Items Displayed"

*l_plan_ready_num_displayed.class: label
*l_plan_ready_num_displayed.name.source: public
*l_plan_ready_num_displayed.static: false
*l_plan_ready_num_displayed.name: l_plan_ready_num_displayed
*l_plan_ready_num_displayed.parent: bb_plan_items_ready
*l_plan_ready_num_displayed.isCompound: "true"
*l_plan_ready_num_displayed.compoundIcon: "label.xpm"
*l_plan_ready_num_displayed.compoundName: "label_"
*l_plan_ready_num_displayed.x: 170
*l_plan_ready_num_displayed.y: 0
*l_plan_ready_num_displayed.width: 60
*l_plan_ready_num_displayed.height: 20
*l_plan_ready_num_displayed.labelString: "9999"
*l_plan_ready_num_displayed.alignment: "alignment_beginning"
*l_plan_ready_num_displayed.recomputeSize: "false"

*l_query_items_found4.class: label
*l_query_items_found4.static: true
*l_query_items_found4.name: l_query_items_found4
*l_query_items_found4.parent: bb_plan_items_ready
*l_query_items_found4.isCompound: "true"
*l_query_items_found4.compoundIcon: "label.xpm"
*l_query_items_found4.compoundName: "label_"
*l_query_items_found4.x: 50
*l_query_items_found4.y: 30
*l_query_items_found4.width: 108
*l_query_items_found4.height: 22
*l_query_items_found4.labelString: "Items Selected"

*l_plan_ready_num_selected.class: label
*l_plan_ready_num_selected.name.source: public
*l_plan_ready_num_selected.static: false
*l_plan_ready_num_selected.name: l_plan_ready_num_selected
*l_plan_ready_num_selected.parent: bb_plan_items_ready
*l_plan_ready_num_selected.isCompound: "true"
*l_plan_ready_num_selected.compoundIcon: "label.xpm"
*l_plan_ready_num_selected.compoundName: "label_"
*l_plan_ready_num_selected.x: 170
*l_plan_ready_num_selected.y: 30
*l_plan_ready_num_selected.width: 46
*l_plan_ready_num_selected.height: 19
*l_plan_ready_num_selected.labelString: "9999"
*l_plan_ready_num_selected.alignment: "alignment_beginning"
*l_plan_ready_num_selected.recomputeSize: "false"

*f_plan_available.class: frame
*f_plan_available.static: true
*f_plan_available.name: f_plan_available
*f_plan_available.parent: pps_plan
*f_plan_available.width: 255
*f_plan_available.height: 170
*f_plan_available.isCompound: "true"
*f_plan_available.compoundIcon: "frame.xpm"
*f_plan_available.compoundName: "frame_"
*f_plan_available.x: 871
*f_plan_available.y: 616

*bb_plan_available.class: bulletinBoard
*bb_plan_available.static: true
*bb_plan_available.name: bb_plan_available
*bb_plan_available.parent: f_plan_available
*bb_plan_available.resizePolicy: "resize_none"
*bb_plan_available.width: 248
*bb_plan_available.height: 168
*bb_plan_available.isCompound: "true"
*bb_plan_available.compoundIcon: "bboard.xpm"
*bb_plan_available.compoundName: "bulletin_Board"
*bb_plan_available.x: 2
*bb_plan_available.y: 42
*bb_plan_available.marginHeight: 0
*bb_plan_available.marginWidth: 0

*pb_plan_available_top.class: pushButton
*pb_plan_available_top.static: true
*pb_plan_available_top.name: pb_plan_available_top
*pb_plan_available_top.parent: bb_plan_available
*pb_plan_available_top.isCompound: "true"
*pb_plan_available_top.compoundIcon: "push.xpm"
*pb_plan_available_top.compoundName: "push_Button"
*pb_plan_available_top.x: 8
*pb_plan_available_top.y: 12
*pb_plan_available_top.width: 112
*pb_plan_available_top.height: 36
*pb_plan_available_top.labelString: "TOP of Plan"
*pb_plan_available_top.activateCallback: cb_plan_available_top();
*pb_plan_available_top.sensitive: "true"

*pb_plan_available_IT_on.class: pushButton
*pb_plan_available_IT_on.static: true
*pb_plan_available_IT_on.name: pb_plan_available_IT_on
*pb_plan_available_IT_on.parent: bb_plan_available
*pb_plan_available_IT_on.isCompound: "true"
*pb_plan_available_IT_on.compoundIcon: "push.xpm"
*pb_plan_available_IT_on.compoundName: "push_Button"
*pb_plan_available_IT_on.x: 130
*pb_plan_available_IT_on.y: 10
*pb_plan_available_IT_on.width: 112
*pb_plan_available_IT_on.height: 36
*pb_plan_available_IT_on.labelString: "Insert Top On"
*pb_plan_available_IT_on.activateCallback: cb_plan_available_IT_on();
*pb_plan_available_IT_on.sensitive: "true"

*pb_plan_available_IT_off.class: pushButton
*pb_plan_available_IT_off.static: true
*pb_plan_available_IT_off.name: pb_plan_available_IT_off
*pb_plan_available_IT_off.parent: bb_plan_available
*pb_plan_available_IT_off.isCompound: "true"
*pb_plan_available_IT_off.compoundIcon: "push.xpm"
*pb_plan_available_IT_off.compoundName: "push_Button"
*pb_plan_available_IT_off.x: 130
*pb_plan_available_IT_off.y: 60
*pb_plan_available_IT_off.width: 112
*pb_plan_available_IT_off.height: 36
*pb_plan_available_IT_off.labelString: "Insert Top Off"
*pb_plan_available_IT_off.activateCallback: cb_plan_available_IT_off();
*pb_plan_available_IT_off.sensitive: "true"

*pb_plan_available_remove.class: pushButton
*pb_plan_available_remove.static: true
*pb_plan_available_remove.name: pb_plan_available_remove
*pb_plan_available_remove.parent: bb_plan_available
*pb_plan_available_remove.isCompound: "true"
*pb_plan_available_remove.compoundIcon: "push.xpm"
*pb_plan_available_remove.compoundName: "push_Button"
*pb_plan_available_remove.x: 8
*pb_plan_available_remove.y: 118
*pb_plan_available_remove.width: 119
*pb_plan_available_remove.height: 36
*pb_plan_available_remove.labelString: "Remove from Plan"
*pb_plan_available_remove.activateCallback: cb_plan_available_remove();
*pb_plan_available_remove.sensitive: "true"

*pb_plan_available_bottom.class: pushButton
*pb_plan_available_bottom.static: true
*pb_plan_available_bottom.name: pb_plan_available_bottom
*pb_plan_available_bottom.parent: bb_plan_available
*pb_plan_available_bottom.isCompound: "true"
*pb_plan_available_bottom.compoundIcon: "push.xpm"
*pb_plan_available_bottom.compoundName: "push_Button"
*pb_plan_available_bottom.x: 8
*pb_plan_available_bottom.y: 64
*pb_plan_available_bottom.width: 112
*pb_plan_available_bottom.height: 36
*pb_plan_available_bottom.labelString: "BOTTOM of Plan"
*pb_plan_available_bottom.activateCallback: cb_plan_available_bottom();
*pb_plan_available_bottom.sensitive: "true"

*f_plan_items_found.class: frame
*f_plan_items_found.static: true
*f_plan_items_found.name: f_plan_items_found
*f_plan_items_found.parent: pps_plan
*f_plan_items_found.width: 245
*f_plan_items_found.height: 60
*f_plan_items_found.isCompound: "true"
*f_plan_items_found.compoundIcon: "frame.xpm"
*f_plan_items_found.compoundName: "frame_"
*f_plan_items_found.x: 871
*f_plan_items_found.y: 800

*bb_plan_items_available.class: bulletinBoard
*bb_plan_items_available.static: true
*bb_plan_items_available.name: bb_plan_items_available
*bb_plan_items_available.parent: f_plan_items_found
*bb_plan_items_available.resizePolicy: "resize_none"
*bb_plan_items_available.width: 248
*bb_plan_items_available.height: 64
*bb_plan_items_available.isCompound: "true"
*bb_plan_items_available.compoundIcon: "bboard.xpm"
*bb_plan_items_available.compoundName: "bulletin_Board"
*bb_plan_items_available.x: 30
*bb_plan_items_available.y: 0
*bb_plan_items_available.marginHeight: 0
*bb_plan_items_available.marginWidth: 0

*l_query_items_found2.class: label
*l_query_items_found2.static: true
*l_query_items_found2.name: l_query_items_found2
*l_query_items_found2.parent: bb_plan_items_available
*l_query_items_found2.isCompound: "true"
*l_query_items_found2.compoundIcon: "label.xpm"
*l_query_items_found2.compoundName: "label_"
*l_query_items_found2.x: 50
*l_query_items_found2.y: 0
*l_query_items_found2.width: 95
*l_query_items_found2.height: 22
*l_query_items_found2.labelString: "Items Planned"

*l_plan_available_num_displayed.class: label
*l_plan_available_num_displayed.name.source: public
*l_plan_available_num_displayed.static: false
*l_plan_available_num_displayed.name: l_plan_available_num_displayed
*l_plan_available_num_displayed.parent: bb_plan_items_available
*l_plan_available_num_displayed.isCompound: "true"
*l_plan_available_num_displayed.compoundIcon: "label.xpm"
*l_plan_available_num_displayed.compoundName: "label_"
*l_plan_available_num_displayed.x: 170
*l_plan_available_num_displayed.y: 0
*l_plan_available_num_displayed.width: 46
*l_plan_available_num_displayed.height: 19
*l_plan_available_num_displayed.labelString: "9999"
*l_plan_available_num_displayed.alignment: "alignment_beginning"
*l_plan_available_num_displayed.recomputeSize: "false"

*l_query_items_found7.class: label
*l_query_items_found7.static: true
*l_query_items_found7.name: l_query_items_found7
*l_query_items_found7.parent: bb_plan_items_available
*l_query_items_found7.isCompound: "true"
*l_query_items_found7.compoundIcon: "label.xpm"
*l_query_items_found7.compoundName: "label_"
*l_query_items_found7.x: 50
*l_query_items_found7.y: 30
*l_query_items_found7.width: 108
*l_query_items_found7.height: 22
*l_query_items_found7.labelString: "Items Selected"

*l_plan_available_num_selected.class: label
*l_plan_available_num_selected.name.source: public
*l_plan_available_num_selected.static: false
*l_plan_available_num_selected.name: l_plan_available_num_selected
*l_plan_available_num_selected.parent: bb_plan_items_available
*l_plan_available_num_selected.isCompound: "true"
*l_plan_available_num_selected.compoundIcon: "label.xpm"
*l_plan_available_num_selected.compoundName: "label_"
*l_plan_available_num_selected.x: 170
*l_plan_available_num_selected.y: 30
*l_plan_available_num_selected.width: 46
*l_plan_available_num_selected.height: 19
*l_plan_available_num_selected.labelString: "9999"
*l_plan_available_num_selected.alignment: "alignment_beginning"
*l_plan_available_num_selected.recomputeSize: "false"

*pb_plan_ready_select_all.class: pushButton
*pb_plan_ready_select_all.name.source: public
*pb_plan_ready_select_all.static: false
*pb_plan_ready_select_all.name: pb_plan_ready_select_all
*pb_plan_ready_select_all.parent: pps_plan
*pb_plan_ready_select_all.isCompound: "true"
*pb_plan_ready_select_all.compoundIcon: "push.xpm"
*pb_plan_ready_select_all.compoundName: "push_Button"
*pb_plan_ready_select_all.x: 610
*pb_plan_ready_select_all.y: 310
*pb_plan_ready_select_all.width: 119
*pb_plan_ready_select_all.height: 27
*pb_plan_ready_select_all.labelString: "Select All"
*pb_plan_ready_select_all.activateCallback: cb_plan_ready_select_all();
*pb_plan_ready_select_all.sensitive: "false"

*pb_plan_ready_deselect_all.class: pushButton
*pb_plan_ready_deselect_all.name.source: public
*pb_plan_ready_deselect_all.static: false
*pb_plan_ready_deselect_all.name: pb_plan_ready_deselect_all
*pb_plan_ready_deselect_all.parent: pps_plan
*pb_plan_ready_deselect_all.isCompound: "true"
*pb_plan_ready_deselect_all.compoundIcon: "push.xpm"
*pb_plan_ready_deselect_all.compoundName: "push_Button"
*pb_plan_ready_deselect_all.x: 730
*pb_plan_ready_deselect_all.y: 310
*pb_plan_ready_deselect_all.width: 119
*pb_plan_ready_deselect_all.height: 27
*pb_plan_ready_deselect_all.labelString: "Deselect All"
*pb_plan_ready_deselect_all.activateCallback: cb_plan_ready_deselect_all();
*pb_plan_ready_deselect_all.sensitive: "false"

*pb_plan_available_select_all.class: pushButton
*pb_plan_available_select_all.name.source: public
*pb_plan_available_select_all.static: false
*pb_plan_available_select_all.name: pb_plan_available_select_all
*pb_plan_available_select_all.parent: pps_plan
*pb_plan_available_select_all.isCompound: "true"
*pb_plan_available_select_all.compoundIcon: "push.xpm"
*pb_plan_available_select_all.compoundName: "push_Button"
*pb_plan_available_select_all.x: 620
*pb_plan_available_select_all.y: 590
*pb_plan_available_select_all.width: 102
*pb_plan_available_select_all.height: 27
*pb_plan_available_select_all.labelString: "Select All"
*pb_plan_available_select_all.activateCallback: cb_plan_available_select_all();
*pb_plan_available_select_all.sensitive: "true"

*pb_plan_available_deselect_all.class: pushButton
*pb_plan_available_deselect_all.name.source: public
*pb_plan_available_deselect_all.static: false
*pb_plan_available_deselect_all.name: pb_plan_available_deselect_all
*pb_plan_available_deselect_all.parent: pps_plan
*pb_plan_available_deselect_all.isCompound: "true"
*pb_plan_available_deselect_all.compoundIcon: "push.xpm"
*pb_plan_available_deselect_all.compoundName: "push_Button"
*pb_plan_available_deselect_all.x: 728
*pb_plan_available_deselect_all.y: 590
*pb_plan_available_deselect_all.width: 119
*pb_plan_available_deselect_all.height: 27
*pb_plan_available_deselect_all.labelString: "Deselect All"
*pb_plan_available_deselect_all.activateCallback: cb_plan_available_deselect_all();
*pb_plan_available_deselect_all.sensitive: "true"

*l_plan_modify_ready.class: label
*l_plan_modify_ready.static: true
*l_plan_modify_ready.name: l_plan_modify_ready
*l_plan_modify_ready.parent: pps_plan
*l_plan_modify_ready.isCompound: "true"
*l_plan_modify_ready.compoundIcon: "label.xpm"
*l_plan_modify_ready.compoundName: "label_"
*l_plan_modify_ready.x: 930
*l_plan_modify_ready.y: 310
*l_plan_modify_ready.width: 131
*l_plan_modify_ready.height: 24
*l_plan_modify_ready.labelString: "Update Ready Jobs"

*l_plan_modify_available.class: label
*l_plan_modify_available.static: true
*l_plan_modify_available.name: l_plan_modify_available
*l_plan_modify_available.parent: pps_plan
*l_plan_modify_available.isCompound: "true"
*l_plan_modify_available.compoundIcon: "label.xpm"
*l_plan_modify_available.compoundName: "label_"
*l_plan_modify_available.x: 940
*l_plan_modify_available.y: 590
*l_plan_modify_available.width: 140
*l_plan_modify_available.height: 20
*l_plan_modify_available.labelString: "Update Planned Jobs"

*pb_plan_available_refresh.class: pushButton
*pb_plan_available_refresh.name.source: public
*pb_plan_available_refresh.static: false
*pb_plan_available_refresh.name: pb_plan_available_refresh
*pb_plan_available_refresh.parent: pps_plan
*pb_plan_available_refresh.isCompound: "true"
*pb_plan_available_refresh.compoundIcon: "push.xpm"
*pb_plan_available_refresh.compoundName: "push_Button"
*pb_plan_available_refresh.x: 512
*pb_plan_available_refresh.y: 590
*pb_plan_available_refresh.width: 102
*pb_plan_available_refresh.height: 27
*pb_plan_available_refresh.labelString: "Refresh"
*pb_plan_available_refresh.activateCallback: do_plan_available_query();
*pb_plan_available_refresh.sensitive: "true"

