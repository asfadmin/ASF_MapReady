! UIMX ascii 2.9 key: 8283                                                      

*pps_policy.class: form
*pps_policy.classinc:
*pps_policy.classspec:
*pps_policy.classmembers:
*pps_policy.classconstructor:
*pps_policy.classdestructor:
*pps_policy.gbldecl: #include <stdio.h>\
#include "pps_common.h"\
#include "pps_util.h"\
\
static char SccsFileId[] = "@(#)pps_policy.i	1.1    11/21/96";\
\
char policy_labelPixmapString[MAXSMALLBUF];\
extern char rootPath[];\
extern swidget nojoy;
*pps_policy.ispecdecl:
*pps_policy.funcdecl: swidget create_pps_policy(swidget UxParent)
*pps_policy.funcname: create_pps_policy
*pps_policy.funcdef: "swidget", "<create_pps_policy>(%)"
*pps_policy.argdecl: swidget UxParent;
*pps_policy.arglist: UxParent
*pps_policy.arglist.UxParent: "swidget", "%UxParent%"
*pps_policy.icode: (void)sprintf(policy_labelPixmapString, "%s/%s/happybr0.xpm",\
rootPath, PPS_PIXMAP_SUBPATH);
*pps_policy.fcode: return(rtrn);\

*pps_policy.auxdecl:
*pps_policy.name.source: public
*pps_policy.static: false
*pps_policy.name: pps_policy
*pps_policy.parent: NO_PARENT
*pps_policy.parentExpression: UxParent
*pps_policy.defaultShell: transientShell
*pps_policy.width: 900
*pps_policy.height: 729
*pps_policy.resizePolicy: "resize_none"
*pps_policy.isCompound: "true"
*pps_policy.compoundIcon: "form.xpm"
*pps_policy.compoundName: "form_"
*pps_policy.x: 45
*pps_policy.y: 52
*pps_policy.unitType: "pixels"

*mb_policy.class: rowColumn
*mb_policy.static: true
*mb_policy.name: mb_policy
*mb_policy.parent: pps_policy
*mb_policy.rowColumnType: "menu_bar"
*mb_policy.isCompound: "true"
*mb_policy.compoundIcon: "pulldownM.xpm"
*mb_policy.compoundName: "menu_Bar"
*mb_policy.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*mb_policy.x: 0
*mb_policy.y: 0
*mb_policy.width: 504
*mb_policy.height: 36
*mb_policy.menuAccelerator: "<KeyUp>F10"
*mb_policy.rightAttachment: "attach_form"
*mb_policy.leftAttachment: "attach_form"

*pb_policy_file.class: rowColumn
*pb_policy_file.static: true
*pb_policy_file.name: pb_policy_file
*pb_policy_file.parent: mb_policy
*pb_policy_file.rowColumnType: "menu_pulldown"

*pb_policy_print_results.class: pushButton
*pb_policy_print_results.static: true
*pb_policy_print_results.name: pb_policy_print_results
*pb_policy_print_results.parent: pb_policy_file
*pb_policy_print_results.labelString: "Print Results..."
*pb_policy_print_results.mnemonic: "R"
*pb_policy_print_results.activateCallback: {\
cb_policy_print_results();\
}

*pb_policy_print_screen.class: pushButton
*pb_policy_print_screen.static: true
*pb_policy_print_screen.name: pb_policy_print_screen
*pb_policy_print_screen.parent: pb_policy_file
*pb_policy_print_screen.labelString: "Print Screen"
*pb_policy_print_screen.activateCallback: {\
extern void pps_print_screen(swidget sw);\
\
pps_print_screen(pps_policy);\
}
*pb_policy_print_screen.mnemonic: "P"

*pb_policy_exit.class: pushButton
*pb_policy_exit.static: true
*pb_policy_exit.name: pb_policy_exit
*pb_policy_exit.parent: pb_policy_file
*pb_policy_exit.labelString: "Exit"
*pb_policy_exit.mnemonic: "x"
*pb_policy_exit.activateCallback: UxPopdownInterface(pps_policy);

*cb_policy.class: cascadeButton
*cb_policy.static: true
*cb_policy.name: cb_policy
*cb_policy.parent: mb_policy
*cb_policy.labelString: "File"
*cb_policy.subMenuId: "pb_policy_file"
*cb_policy.mnemonic: "F"

*l_policy_matrix.class: label
*l_policy_matrix.static: true
*l_policy_matrix.name: l_policy_matrix
*l_policy_matrix.parent: pps_policy
*l_policy_matrix.isCompound: "true"
*l_policy_matrix.compoundIcon: "label.xpm"
*l_policy_matrix.compoundName: "label_"
*l_policy_matrix.x: 9
*l_policy_matrix.y: 42
*l_policy_matrix.width: 106
*l_policy_matrix.height: 30
*l_policy_matrix.labelString: "Policy Matrix"

*f_policy_matrix.class: frame
*f_policy_matrix.static: true
*f_policy_matrix.name: f_policy_matrix
*f_policy_matrix.parent: pps_policy
*f_policy_matrix.width: 339
*f_policy_matrix.height: 230
*f_policy_matrix.isCompound: "true"
*f_policy_matrix.compoundIcon: "frame.xpm"
*f_policy_matrix.compoundName: "frame_"
*f_policy_matrix.x: 11
*f_policy_matrix.y: 77

*bb_policy_matrix.class: bulletinBoard
*bb_policy_matrix.static: true
*bb_policy_matrix.name: bb_policy_matrix
*bb_policy_matrix.parent: f_policy_matrix
*bb_policy_matrix.resizePolicy: "resize_none"
*bb_policy_matrix.width: 335
*bb_policy_matrix.height: 211
*bb_policy_matrix.isCompound: "true"
*bb_policy_matrix.compoundIcon: "bboard.xpm"
*bb_policy_matrix.compoundName: "bulletin_Board"
*bb_policy_matrix.x: 2
*bb_policy_matrix.y: 2
*bb_policy_matrix.shadowThickness: 1

*rc_policy_matrix.class: rowColumn
*rc_policy_matrix.static: true
*rc_policy_matrix.name: rc_policy_matrix
*rc_policy_matrix.parent: bb_policy_matrix
*rc_policy_matrix.width: 230
*rc_policy_matrix.height: 163
*rc_policy_matrix.isCompound: "true"
*rc_policy_matrix.compoundIcon: "row.xpm"
*rc_policy_matrix.compoundName: "row_Column"
*rc_policy_matrix.x: 90
*rc_policy_matrix.y: 40
*rc_policy_matrix.numColumns: 4
*rc_policy_matrix.radioAlwaysOne: "false"
*rc_policy_matrix.orientation: "vertical"
*rc_policy_matrix.packing: "pack_column"
*rc_policy_matrix.spacing: 16

*tb_policy_Urgent_L1.class: toggleButton
*tb_policy_Urgent_L1.name.source: public
*tb_policy_Urgent_L1.static: false
*tb_policy_Urgent_L1.name: tb_policy_Urgent_L1
*tb_policy_Urgent_L1.parent: rc_policy_matrix
*tb_policy_Urgent_L1.isCompound: "true"
*tb_policy_Urgent_L1.compoundIcon: "toggle.xpm"
*tb_policy_Urgent_L1.compoundName: "toggle_Button"
*tb_policy_Urgent_L1.x: 3
*tb_policy_Urgent_L1.y: 3
*tb_policy_Urgent_L1.width: 20
*tb_policy_Urgent_L1.height: 137
*tb_policy_Urgent_L1.labelString: ""
*tb_policy_Urgent_L1.indicatorSize: 16
*tb_policy_Urgent_L1.marginWidth: 10
*tb_policy_Urgent_L1.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "NO ", "URGENT", "YES");\
else\
	cb_policy_change("L1", "NO ", "URGENT", "NO ");\
\
\
\
}

*tb_policy_Urgent_L1_QLK.class: toggleButton
*tb_policy_Urgent_L1_QLK.name.source: public
*tb_policy_Urgent_L1_QLK.static: false
*tb_policy_Urgent_L1_QLK.name: tb_policy_Urgent_L1_QLK
*tb_policy_Urgent_L1_QLK.parent: rc_policy_matrix
*tb_policy_Urgent_L1_QLK.isCompound: "true"
*tb_policy_Urgent_L1_QLK.compoundIcon: "toggle.xpm"
*tb_policy_Urgent_L1_QLK.compoundName: "toggle_Button"
*tb_policy_Urgent_L1_QLK.x: 3
*tb_policy_Urgent_L1_QLK.y: 3
*tb_policy_Urgent_L1_QLK.width: 20
*tb_policy_Urgent_L1_QLK.height: 137
*tb_policy_Urgent_L1_QLK.labelString: ""
*tb_policy_Urgent_L1_QLK.indicatorSize: 16
*tb_policy_Urgent_L1_QLK.marginWidth: 10
*tb_policy_Urgent_L1_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "YES", "URGENT", "YES");\
else\
	cb_policy_change("L1", "YES", "URGENT", "NO ");\
}

*tb_policy_Urgent_Scan.class: toggleButton
*tb_policy_Urgent_Scan.name.source: public
*tb_policy_Urgent_Scan.static: false
*tb_policy_Urgent_Scan.name: tb_policy_Urgent_Scan
*tb_policy_Urgent_Scan.parent: rc_policy_matrix
*tb_policy_Urgent_Scan.isCompound: "true"
*tb_policy_Urgent_Scan.compoundIcon: "toggle.xpm"
*tb_policy_Urgent_Scan.compoundName: "toggle_Button"
*tb_policy_Urgent_Scan.x: 3
*tb_policy_Urgent_Scan.y: 3
*tb_policy_Urgent_Scan.width: 20
*tb_policy_Urgent_Scan.height: 137
*tb_policy_Urgent_Scan.labelString: ""
*tb_policy_Urgent_Scan.indicatorSize: 16
*tb_policy_Urgent_Scan.marginWidth: 10
*tb_policy_Urgent_Scan.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "NO ", "URGENT", "YES");\
else\
	cb_policy_change("SCAN", "NO ", "URGENT", "NO ");\
}

*tb_policy_Urgent_Scan_QLK.class: toggleButton
*tb_policy_Urgent_Scan_QLK.name.source: public
*tb_policy_Urgent_Scan_QLK.static: false
*tb_policy_Urgent_Scan_QLK.name: tb_policy_Urgent_Scan_QLK
*tb_policy_Urgent_Scan_QLK.parent: rc_policy_matrix
*tb_policy_Urgent_Scan_QLK.isCompound: "true"
*tb_policy_Urgent_Scan_QLK.compoundIcon: "toggle.xpm"
*tb_policy_Urgent_Scan_QLK.compoundName: "toggle_Button"
*tb_policy_Urgent_Scan_QLK.x: 3
*tb_policy_Urgent_Scan_QLK.y: 3
*tb_policy_Urgent_Scan_QLK.width: 20
*tb_policy_Urgent_Scan_QLK.height: 137
*tb_policy_Urgent_Scan_QLK.labelString: ""
*tb_policy_Urgent_Scan_QLK.indicatorSize: 16
*tb_policy_Urgent_Scan_QLK.marginWidth: 10
*tb_policy_Urgent_Scan_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "YES", "URGENT", "YES");\
else\
	cb_policy_change("SCAN", "YES", "URGENT", "NO ");\
}

*tb_policy_High_L1.class: toggleButton
*tb_policy_High_L1.name.source: public
*tb_policy_High_L1.static: false
*tb_policy_High_L1.name: tb_policy_High_L1
*tb_policy_High_L1.parent: rc_policy_matrix
*tb_policy_High_L1.isCompound: "true"
*tb_policy_High_L1.compoundIcon: "toggle.xpm"
*tb_policy_High_L1.compoundName: "toggle_Button"
*tb_policy_High_L1.x: 3
*tb_policy_High_L1.y: 3
*tb_policy_High_L1.width: 20
*tb_policy_High_L1.height: 137
*tb_policy_High_L1.labelString: ""
*tb_policy_High_L1.indicatorSize: 16
*tb_policy_High_L1.marginWidth: 10
*tb_policy_High_L1.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "NO ", "HIGH", "YES");\
else\
	cb_policy_change("L1", "NO ", "HIGH", "NO ");\
	\
}

*tb_policy_High_L1_QLK.class: toggleButton
*tb_policy_High_L1_QLK.name.source: public
*tb_policy_High_L1_QLK.static: false
*tb_policy_High_L1_QLK.name: tb_policy_High_L1_QLK
*tb_policy_High_L1_QLK.parent: rc_policy_matrix
*tb_policy_High_L1_QLK.isCompound: "true"
*tb_policy_High_L1_QLK.compoundIcon: "toggle.xpm"
*tb_policy_High_L1_QLK.compoundName: "toggle_Button"
*tb_policy_High_L1_QLK.x: 3
*tb_policy_High_L1_QLK.y: 3
*tb_policy_High_L1_QLK.width: 20
*tb_policy_High_L1_QLK.height: 137
*tb_policy_High_L1_QLK.labelString: ""
*tb_policy_High_L1_QLK.indicatorSize: 16
*tb_policy_High_L1_QLK.marginWidth: 10
*tb_policy_High_L1_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "YES", "HIGH", "YES");\
else\
	cb_policy_change("L1", "YES", "HIGH", "NO ");\
}

*tb_policy_High_Scan.class: toggleButton
*tb_policy_High_Scan.name.source: public
*tb_policy_High_Scan.static: false
*tb_policy_High_Scan.name: tb_policy_High_Scan
*tb_policy_High_Scan.parent: rc_policy_matrix
*tb_policy_High_Scan.isCompound: "true"
*tb_policy_High_Scan.compoundIcon: "toggle.xpm"
*tb_policy_High_Scan.compoundName: "toggle_Button"
*tb_policy_High_Scan.x: 3
*tb_policy_High_Scan.y: 3
*tb_policy_High_Scan.width: 20
*tb_policy_High_Scan.height: 137
*tb_policy_High_Scan.labelString: ""
*tb_policy_High_Scan.indicatorSize: 16
*tb_policy_High_Scan.marginWidth: 10
*tb_policy_High_Scan.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "NO ", "HIGH", "YES");\
else\
	cb_policy_change("SCAN", "NO ", "HIGH", "NO ");\
}

*tb_policy_High_Scan_QLK.class: toggleButton
*tb_policy_High_Scan_QLK.name.source: public
*tb_policy_High_Scan_QLK.static: false
*tb_policy_High_Scan_QLK.name: tb_policy_High_Scan_QLK
*tb_policy_High_Scan_QLK.parent: rc_policy_matrix
*tb_policy_High_Scan_QLK.isCompound: "true"
*tb_policy_High_Scan_QLK.compoundIcon: "toggle.xpm"
*tb_policy_High_Scan_QLK.compoundName: "toggle_Button"
*tb_policy_High_Scan_QLK.x: 3
*tb_policy_High_Scan_QLK.y: 3
*tb_policy_High_Scan_QLK.width: 20
*tb_policy_High_Scan_QLK.height: 137
*tb_policy_High_Scan_QLK.labelString: ""
*tb_policy_High_Scan_QLK.indicatorSize: 16
*tb_policy_High_Scan_QLK.marginWidth: 10
*tb_policy_High_Scan_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "YES", "HIGH", "YES");\
else\
	cb_policy_change("SCAN", "YES", "HIGH", "NO ");\
}

*tb_policy_Routine_L1.class: toggleButton
*tb_policy_Routine_L1.name.source: public
*tb_policy_Routine_L1.static: false
*tb_policy_Routine_L1.name: tb_policy_Routine_L1
*tb_policy_Routine_L1.parent: rc_policy_matrix
*tb_policy_Routine_L1.isCompound: "true"
*tb_policy_Routine_L1.compoundIcon: "toggle.xpm"
*tb_policy_Routine_L1.compoundName: "toggle_Button"
*tb_policy_Routine_L1.x: 3
*tb_policy_Routine_L1.y: 3
*tb_policy_Routine_L1.width: 20
*tb_policy_Routine_L1.height: 137
*tb_policy_Routine_L1.labelString: ""
*tb_policy_Routine_L1.indicatorSize: 16
*tb_policy_Routine_L1.marginWidth: 10
*tb_policy_Routine_L1.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "NO ", "ROUTINE", "YES");\
else\
	cb_policy_change("L1", "NO ", "ROUTINE", "NO ");\
}

*tb_policy_Routine_L1_QLK.class: toggleButton
*tb_policy_Routine_L1_QLK.name.source: public
*tb_policy_Routine_L1_QLK.static: false
*tb_policy_Routine_L1_QLK.name: tb_policy_Routine_L1_QLK
*tb_policy_Routine_L1_QLK.parent: rc_policy_matrix
*tb_policy_Routine_L1_QLK.isCompound: "true"
*tb_policy_Routine_L1_QLK.compoundIcon: "toggle.xpm"
*tb_policy_Routine_L1_QLK.compoundName: "toggle_Button"
*tb_policy_Routine_L1_QLK.x: 3
*tb_policy_Routine_L1_QLK.y: 3
*tb_policy_Routine_L1_QLK.width: 20
*tb_policy_Routine_L1_QLK.height: 137
*tb_policy_Routine_L1_QLK.labelString: ""
*tb_policy_Routine_L1_QLK.indicatorSize: 16
*tb_policy_Routine_L1_QLK.marginWidth: 10
*tb_policy_Routine_L1_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "YES", "ROUTINE", "YES");\
else\
	cb_policy_change("L1", "YES", "ROUTINE", "NO ");\
}

*tb_policy_Routine_Scan.class: toggleButton
*tb_policy_Routine_Scan.name.source: public
*tb_policy_Routine_Scan.static: false
*tb_policy_Routine_Scan.name: tb_policy_Routine_Scan
*tb_policy_Routine_Scan.parent: rc_policy_matrix
*tb_policy_Routine_Scan.isCompound: "true"
*tb_policy_Routine_Scan.compoundIcon: "toggle.xpm"
*tb_policy_Routine_Scan.compoundName: "toggle_Button"
*tb_policy_Routine_Scan.x: 3
*tb_policy_Routine_Scan.y: 3
*tb_policy_Routine_Scan.width: 20
*tb_policy_Routine_Scan.height: 137
*tb_policy_Routine_Scan.labelString: ""
*tb_policy_Routine_Scan.indicatorSize: 16
*tb_policy_Routine_Scan.marginWidth: 10
*tb_policy_Routine_Scan.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "NO ", "ROUTINE", "YES");\
else\
	cb_policy_change("SCAN", "NO ", "ROUTINE", "NO ");\
}

*tb_policy_Routine_Scan_QLK.class: toggleButton
*tb_policy_Routine_Scan_QLK.name.source: public
*tb_policy_Routine_Scan_QLK.static: false
*tb_policy_Routine_Scan_QLK.name: tb_policy_Routine_Scan_QLK
*tb_policy_Routine_Scan_QLK.parent: rc_policy_matrix
*tb_policy_Routine_Scan_QLK.isCompound: "true"
*tb_policy_Routine_Scan_QLK.compoundIcon: "toggle.xpm"
*tb_policy_Routine_Scan_QLK.compoundName: "toggle_Button"
*tb_policy_Routine_Scan_QLK.x: 3
*tb_policy_Routine_Scan_QLK.y: 3
*tb_policy_Routine_Scan_QLK.width: 20
*tb_policy_Routine_Scan_QLK.height: 137
*tb_policy_Routine_Scan_QLK.labelString: ""
*tb_policy_Routine_Scan_QLK.indicatorSize: 16
*tb_policy_Routine_Scan_QLK.marginWidth: 10
*tb_policy_Routine_Scan_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "YES", "ROUTINE", "YES");\
else\
	cb_policy_change("SCAN", "YES", "ROUTINE", "NO ");\
}

*tb_policy_Low_L1.class: toggleButton
*tb_policy_Low_L1.name.source: public
*tb_policy_Low_L1.static: false
*tb_policy_Low_L1.name: tb_policy_Low_L1
*tb_policy_Low_L1.parent: rc_policy_matrix
*tb_policy_Low_L1.isCompound: "true"
*tb_policy_Low_L1.compoundIcon: "toggle.xpm"
*tb_policy_Low_L1.compoundName: "toggle_Button"
*tb_policy_Low_L1.x: 3
*tb_policy_Low_L1.y: 3
*tb_policy_Low_L1.width: 20
*tb_policy_Low_L1.height: 137
*tb_policy_Low_L1.labelString: ""
*tb_policy_Low_L1.indicatorSize: 16
*tb_policy_Low_L1.marginWidth: 10
*tb_policy_Low_L1.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "NO ", "LOW", "YES");\
else\
	cb_policy_change("L1", "NO ", "LOW", "NO ");\
}

*tb_policy_Low_L1_QLK.class: toggleButton
*tb_policy_Low_L1_QLK.name.source: public
*tb_policy_Low_L1_QLK.static: false
*tb_policy_Low_L1_QLK.name: tb_policy_Low_L1_QLK
*tb_policy_Low_L1_QLK.parent: rc_policy_matrix
*tb_policy_Low_L1_QLK.isCompound: "true"
*tb_policy_Low_L1_QLK.compoundIcon: "toggle.xpm"
*tb_policy_Low_L1_QLK.compoundName: "toggle_Button"
*tb_policy_Low_L1_QLK.x: 3
*tb_policy_Low_L1_QLK.y: 3
*tb_policy_Low_L1_QLK.width: 20
*tb_policy_Low_L1_QLK.height: 137
*tb_policy_Low_L1_QLK.labelString: ""
*tb_policy_Low_L1_QLK.indicatorSize: 16
*tb_policy_Low_L1_QLK.marginWidth: 10
*tb_policy_Low_L1_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("L1", "YES", "LOW", "YES");\
else\
	cb_policy_change("L1", "YES", "LOW", "NO ");\
}

*tb_policy_Low_Scan.class: toggleButton
*tb_policy_Low_Scan.name.source: public
*tb_policy_Low_Scan.static: false
*tb_policy_Low_Scan.name: tb_policy_Low_Scan
*tb_policy_Low_Scan.parent: rc_policy_matrix
*tb_policy_Low_Scan.isCompound: "true"
*tb_policy_Low_Scan.compoundIcon: "toggle.xpm"
*tb_policy_Low_Scan.compoundName: "toggle_Button"
*tb_policy_Low_Scan.x: 3
*tb_policy_Low_Scan.y: 3
*tb_policy_Low_Scan.width: 20
*tb_policy_Low_Scan.height: 137
*tb_policy_Low_Scan.labelString: ""
*tb_policy_Low_Scan.indicatorSize: 16
*tb_policy_Low_Scan.marginWidth: 10
*tb_policy_Low_Scan.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "NO ", "LOW", "YES");\
else\
	cb_policy_change("SCAN", "NO ", "LOW", "NO ");\
}

*tb_policy_Low_Scan_QLK.class: toggleButton
*tb_policy_Low_Scan_QLK.name.source: public
*tb_policy_Low_Scan_QLK.static: false
*tb_policy_Low_Scan_QLK.name: tb_policy_Low_Scan_QLK
*tb_policy_Low_Scan_QLK.parent: rc_policy_matrix
*tb_policy_Low_Scan_QLK.isCompound: "true"
*tb_policy_Low_Scan_QLK.compoundIcon: "toggle.xpm"
*tb_policy_Low_Scan_QLK.compoundName: "toggle_Button"
*tb_policy_Low_Scan_QLK.x: 80
*tb_policy_Low_Scan_QLK.y: 24
*tb_policy_Low_Scan_QLK.width: 92
*tb_policy_Low_Scan_QLK.height: 76
*tb_policy_Low_Scan_QLK.labelString: ""
*tb_policy_Low_Scan_QLK.indicatorSize: 16
*tb_policy_Low_Scan_QLK.marginWidth: 10
*tb_policy_Low_Scan_QLK.valueChangedCallback: {\
if (XmToggleButtonGetState(UxWidget))\
	cb_policy_change("SCAN", "YES", "LOW", "YES");\
else\
	cb_policy_change("SCAN", "YES", "LOW", "NO ");\
}

*label9.class: label
*label9.static: true
*label9.name: label9
*label9.parent: bb_policy_matrix
*label9.isCompound: "true"
*label9.compoundIcon: "label.xpm"
*label9.compoundName: "label_"
*label9.x: 84
*label9.y: 12
*label9.width: 56
*label9.height: 24
*label9.labelString: "Urgent"

*label10.class: label
*label10.static: true
*label10.name: label10
*label10.parent: bb_policy_matrix
*label10.isCompound: "true"
*label10.compoundIcon: "label.xpm"
*label10.compoundName: "label_"
*label10.x: 148
*label10.y: 12
*label10.width: 56
*label10.height: 24
*label10.labelString: "High"

*label11.class: label
*label11.static: true
*label11.name: label11
*label11.parent: bb_policy_matrix
*label11.isCompound: "true"
*label11.compoundIcon: "label.xpm"
*label11.compoundName: "label_"
*label11.x: 208
*label11.y: 12
*label11.width: 56
*label11.height: 24
*label11.labelString: "Routine"

*label12.class: label
*label12.static: true
*label12.name: label12
*label12.parent: bb_policy_matrix
*label12.isCompound: "true"
*label12.compoundIcon: "label.xpm"
*label12.compoundName: "label_"
*label12.x: 268
*label12.y: 12
*label12.width: 56
*label12.height: 24
*label12.labelString: "Low"

*label13.class: label
*label13.static: true
*label13.name: label13
*label13.parent: bb_policy_matrix
*label13.isCompound: "true"
*label13.compoundIcon: "label.xpm"
*label13.compoundName: "label_"
*label13.x: -24
*label13.y: 48
*label13.width: 44
*label13.height: 24
*label13.labelString: "L1"

*label14.class: label
*label14.static: true
*label14.name: label14
*label14.parent: bb_policy_matrix
*label14.isCompound: "true"
*label14.compoundIcon: "label.xpm"
*label14.compoundName: "label_"
*label14.x: -28
*label14.y: 88
*label14.width: 56
*label14.height: 24
*label14.labelString: "L1 QLK"

*label15.class: label
*label15.static: true
*label15.name: label15
*label15.parent: bb_policy_matrix
*label15.isCompound: "true"
*label15.compoundIcon: "label.xpm"
*label15.compoundName: "label_"
*label15.x: -28
*label15.y: 132
*label15.width: 56
*label15.height: 24
*label15.labelString: "Scan"

*label16.class: label
*label16.static: true
*label16.name: label16
*label16.parent: bb_policy_matrix
*label16.isCompound: "true"
*label16.compoundIcon: "label.xpm"
*label16.compoundName: "label_"
*label16.x: -36
*label16.y: 176
*label16.width: 72
*label16.height: 24
*label16.labelString: "Scan QLK"

*f_policy_query_results.class: frame
*f_policy_query_results.static: true
*f_policy_query_results.name: f_policy_query_results
*f_policy_query_results.parent: pps_policy
*f_policy_query_results.width: 886
*f_policy_query_results.height: 356
*f_policy_query_results.isCompound: "true"
*f_policy_query_results.compoundIcon: "frame.xpm"
*f_policy_query_results.compoundName: "frame_"
*f_policy_query_results.x: 9
*f_policy_query_results.y: 366

*bb_policy_query_results.class: bulletinBoard
*bb_policy_query_results.static: true
*bb_policy_query_results.name: bb_policy_query_results
*bb_policy_query_results.parent: f_policy_query_results
*bb_policy_query_results.resizePolicy: "resize_none"
*bb_policy_query_results.width: 877
*bb_policy_query_results.height: 352
*bb_policy_query_results.isCompound: "true"
*bb_policy_query_results.compoundIcon: "bboard.xpm"
*bb_policy_query_results.compoundName: "bulletin_Board"
*bb_policy_query_results.x: 2
*bb_policy_query_results.y: 2
*bb_policy_query_results.marginHeight: 0
*bb_policy_query_results.marginWidth: 0

*l_policy_priority.class: label
*l_policy_priority.static: true
*l_policy_priority.name: l_policy_priority
*l_policy_priority.parent: bb_policy_query_results
*l_policy_priority.isCompound: "true"
*l_policy_priority.compoundIcon: "label.xpm"
*l_policy_priority.compoundName: "label_"
*l_policy_priority.x: 76
*l_policy_priority.y: 8
*l_policy_priority.width: 68
*l_policy_priority.height: 32
*l_policy_priority.labelString: "Priority"

*l_policy_media_id.class: label
*l_policy_media_id.static: true
*l_policy_media_id.name: l_policy_media_id
*l_policy_media_id.parent: bb_policy_query_results
*l_policy_media_id.isCompound: "true"
*l_policy_media_id.compoundIcon: "label.xpm"
*l_policy_media_id.compoundName: "label_"
*l_policy_media_id.x: 156
*l_policy_media_id.y: 8
*l_policy_media_id.width: 68
*l_policy_media_id.height: 32
*l_policy_media_id.labelString: "Media ID"

*l_policy_mode.class: label
*l_policy_mode.static: true
*l_policy_mode.name: l_policy_mode
*l_policy_mode.parent: bb_policy_query_results
*l_policy_mode.isCompound: "true"
*l_policy_mode.compoundIcon: "label.xpm"
*l_policy_mode.compoundName: "label_"
*l_policy_mode.x: 238
*l_policy_mode.y: 8
*l_policy_mode.width: 56
*l_policy_mode.height: 32
*l_policy_mode.labelString: "Mode"

*l_policy_sat_sens_rev.class: label
*l_policy_sat_sens_rev.static: true
*l_policy_sat_sens_rev.name: l_policy_sat_sens_rev
*l_policy_sat_sens_rev.parent: bb_policy_query_results
*l_policy_sat_sens_rev.isCompound: "true"
*l_policy_sat_sens_rev.compoundIcon: "label.xpm"
*l_policy_sat_sens_rev.compoundName: "label_"
*l_policy_sat_sens_rev.x: 304
*l_policy_sat_sens_rev.y: 8
*l_policy_sat_sens_rev.width: 88
*l_policy_sat_sens_rev.height: 32
*l_policy_sat_sens_rev.labelString: "Sat Sens Rev"

*l_policy_frame_id.class: label
*l_policy_frame_id.static: true
*l_policy_frame_id.name: l_policy_frame_id
*l_policy_frame_id.parent: bb_policy_query_results
*l_policy_frame_id.isCompound: "true"
*l_policy_frame_id.compoundIcon: "label.xpm"
*l_policy_frame_id.compoundName: "label_"
*l_policy_frame_id.x: 400
*l_policy_frame_id.y: 8
*l_policy_frame_id.width: 48
*l_policy_frame_id.height: 32
*l_policy_frame_id.labelString: "Frame\nID"

*l_policy_job_id.class: label
*l_policy_job_id.static: true
*l_policy_job_id.name: l_policy_job_id
*l_policy_job_id.parent: bb_policy_query_results
*l_policy_job_id.isCompound: "true"
*l_policy_job_id.compoundIcon: "label.xpm"
*l_policy_job_id.compoundName: "label_"
*l_policy_job_id.x: 458
*l_policy_job_id.y: 8
*l_policy_job_id.width: 68
*l_policy_job_id.height: 32
*l_policy_job_id.labelString: "Job ID"

*l_policy_order_item.class: label
*l_policy_order_item.static: true
*l_policy_order_item.name: l_policy_order_item
*l_policy_order_item.parent: bb_policy_query_results
*l_policy_order_item.isCompound: "true"
*l_policy_order_item.compoundIcon: "label.xpm"
*l_policy_order_item.compoundName: "label_"
*l_policy_order_item.x: 526
*l_policy_order_item.y: 8
*l_policy_order_item.width: 100
*l_policy_order_item.height: 32
*l_policy_order_item.labelString: "Order & Item ID"

*l_policy_state.class: label
*l_policy_state.static: true
*l_policy_state.name: l_policy_state
*l_policy_state.parent: bb_policy_query_results
*l_policy_state.isCompound: "true"
*l_policy_state.compoundIcon: "label.xpm"
*l_policy_state.compoundName: "label_"
*l_policy_state.x: 638
*l_policy_state.y: 8
*l_policy_state.width: 64
*l_policy_state.height: 32
*l_policy_state.labelString: "State"

*l_policy_age.class: label
*l_policy_age.static: true
*l_policy_age.name: l_policy_age
*l_policy_age.parent: bb_policy_query_results
*l_policy_age.isCompound: "true"
*l_policy_age.compoundIcon: "label.xpm"
*l_policy_age.compoundName: "label_"
*l_policy_age.x: 706
*l_policy_age.y: 8
*l_policy_age.width: 48
*l_policy_age.height: 32
*l_policy_age.labelString: "Age\n(Days)"

*l_policy_prod_time.class: label
*l_policy_prod_time.static: true
*l_policy_prod_time.name: l_policy_prod_time
*l_policy_prod_time.parent: bb_policy_query_results
*l_policy_prod_time.isCompound: "true"
*l_policy_prod_time.compoundIcon: "label.xpm"
*l_policy_prod_time.compoundName: "label_"
*l_policy_prod_time.x: 764
*l_policy_prod_time.y: 8
*l_policy_prod_time.width: 40
*l_policy_prod_time.height: 32
*l_policy_prod_time.labelString: "Prod\nTime"

*l_policy_order_type.class: label
*l_policy_order_type.static: true
*l_policy_order_type.name: l_policy_order_type
*l_policy_order_type.parent: bb_policy_query_results
*l_policy_order_type.isCompound: "true"
*l_policy_order_type.compoundIcon: "label.xpm"
*l_policy_order_type.compoundName: "label_"
*l_policy_order_type.x: 4
*l_policy_order_type.y: 8
*l_policy_order_type.width: 68
*l_policy_order_type.height: 32
*l_policy_order_type.labelString: "Order\nType"

*sw_policy_query_results.class: scrolledWindow
*sw_policy_query_results.static: true
*sw_policy_query_results.name: sw_policy_query_results
*sw_policy_query_results.parent: bb_policy_query_results
*sw_policy_query_results.scrollingPolicy: "application_defined"
*sw_policy_query_results.visualPolicy: "variable"
*sw_policy_query_results.scrollBarDisplayPolicy: "static"
*sw_policy_query_results.shadowThickness: 0
*sw_policy_query_results.isCompound: "true"
*sw_policy_query_results.compoundIcon: "scrllist.xpm"
*sw_policy_query_results.compoundName: "scrolled_List"
*sw_policy_query_results.x: 2
*sw_policy_query_results.y: 46

*sw_policy_query_results_list.class: scrolledList
*sw_policy_query_results_list.name.source: public
*sw_policy_query_results_list.static: false
*sw_policy_query_results_list.name: sw_policy_query_results_list
*sw_policy_query_results_list.parent: sw_policy_query_results
*sw_policy_query_results_list.width: 850
*sw_policy_query_results_list.height: 303
*sw_policy_query_results_list.scrollBarDisplayPolicy: "static"
*sw_policy_query_results_list.listSizePolicy: "constant"
*sw_policy_query_results_list.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-12-120-72-72-m-70-iso8859-1"
*sw_policy_query_results_list.selectionPolicy: "extended_select"
*sw_policy_query_results_list.automaticSelection: "false"
*sw_policy_query_results_list.extendedSelectionCallback: {\
\
}

*labelGadget1.class: labelGadget
*labelGadget1.static: true
*labelGadget1.name: labelGadget1
*labelGadget1.parent: bb_policy_query_results
*labelGadget1.x: 800
*labelGadget1.y: 7
*labelGadget1.width: 54
*labelGadget1.height: 35
*labelGadget1.labelString: "Insert\n Top"

*pb_policy_select_all.class: pushButton
*pb_policy_select_all.name.source: public
*pb_policy_select_all.static: false
*pb_policy_select_all.name: pb_policy_select_all
*pb_policy_select_all.parent: pps_policy
*pb_policy_select_all.isCompound: "true"
*pb_policy_select_all.compoundIcon: "push.xpm"
*pb_policy_select_all.compoundName: "push_Button"
*pb_policy_select_all.x: 362
*pb_policy_select_all.y: 331
*pb_policy_select_all.width: 93
*pb_policy_select_all.height: 27
*pb_policy_select_all.labelString: "Select All"
*pb_policy_select_all.activateCallback: cb_policy_select_all();
*pb_policy_select_all.sensitive: "true"

*pb_policy_IT_on.class: pushButton
*pb_policy_IT_on.static: true
*pb_policy_IT_on.name: pb_policy_IT_on
*pb_policy_IT_on.parent: pps_policy
*pb_policy_IT_on.isCompound: "true"
*pb_policy_IT_on.compoundIcon: "push.xpm"
*pb_policy_IT_on.compoundName: "push_Button"
*pb_policy_IT_on.x: 603
*pb_policy_IT_on.y: 317
*pb_policy_IT_on.width: 127
*pb_policy_IT_on.height: 40
*pb_policy_IT_on.labelString: "Insert Top On"
*pb_policy_IT_on.activateCallback: cb_policy_IT_on();
*pb_policy_IT_on.sensitive: "true"

*l_policy_pending_jobs.class: label
*l_policy_pending_jobs.static: true
*l_policy_pending_jobs.name: l_policy_pending_jobs
*l_policy_pending_jobs.parent: pps_policy
*l_policy_pending_jobs.isCompound: "true"
*l_policy_pending_jobs.compoundIcon: "label.xpm"
*l_policy_pending_jobs.compoundName: "label_"
*l_policy_pending_jobs.x: 11
*l_policy_pending_jobs.y: 332
*l_policy_pending_jobs.width: 280
*l_policy_pending_jobs.height: 30
*l_policy_pending_jobs.labelString: "Pending Jobs which match the Policy Matrix"

*pb_policy_deselect_all.class: pushButton
*pb_policy_deselect_all.name.source: public
*pb_policy_deselect_all.static: false
*pb_policy_deselect_all.name: pb_policy_deselect_all
*pb_policy_deselect_all.parent: pps_policy
*pb_policy_deselect_all.isCompound: "true"
*pb_policy_deselect_all.compoundIcon: "push.xpm"
*pb_policy_deselect_all.compoundName: "push_Button"
*pb_policy_deselect_all.x: 466
*pb_policy_deselect_all.y: 331
*pb_policy_deselect_all.width: 93
*pb_policy_deselect_all.height: 27
*pb_policy_deselect_all.labelString: "Deselect All"
*pb_policy_deselect_all.activateCallback: cb_policy_deselect_all();
*pb_policy_deselect_all.sensitive: "true"

*pb_policy_IT_off.class: pushButton
*pb_policy_IT_off.static: true
*pb_policy_IT_off.name: pb_policy_IT_off
*pb_policy_IT_off.parent: pps_policy
*pb_policy_IT_off.isCompound: "true"
*pb_policy_IT_off.compoundIcon: "push.xpm"
*pb_policy_IT_off.compoundName: "push_Button"
*pb_policy_IT_off.x: 747
*pb_policy_IT_off.y: 318
*pb_policy_IT_off.width: 127
*pb_policy_IT_off.height: 40
*pb_policy_IT_off.labelString: "Insert Top Off"
*pb_policy_IT_off.activateCallback: cb_policy_IT_off();
*pb_policy_IT_off.sensitive: "true"

*f_policy_bear.class: frame
*f_policy_bear.static: true
*f_policy_bear.name: f_policy_bear
*f_policy_bear.parent: pps_policy
*f_policy_bear.width: 455
*f_policy_bear.height: 254
*f_policy_bear.isCompound: "true"
*f_policy_bear.compoundIcon: "frame.xpm"
*f_policy_bear.compoundName: "frame_"
*f_policy_bear.x: 375
*f_policy_bear.y: 54

*l_policy_bear.class: label
*l_policy_bear.static: true
*l_policy_bear.name: l_policy_bear
*l_policy_bear.parent: f_policy_bear
*l_policy_bear.isCompound: "true"
*l_policy_bear.compoundIcon: "label.xpm"
*l_policy_bear.compoundName: "label_"
*l_policy_bear.x: 0
*l_policy_bear.y: 0
*l_policy_bear.width: 453
*l_policy_bear.height: 252
*l_policy_bear.labelPixmap: policy_labelPixmapString
*l_policy_bear.labelType: "pixmap"

