! UIMX ascii 2.9 key: 2172                                                      

*pps_main.class: topLevelShell
*pps_main.classinc:
*pps_main.classspec:
*pps_main.classmembers:
*pps_main.classconstructor:
*pps_main.classdestructor:
*pps_main.gbldecl: #include <stdio.h>\
#include "pps_common.h"\
#include "pps_util.h"\
\
static char SccsFileId[] = "@(#)pps_main.i	1.2    12/16/96";\
\
char main_labelPixmapString[MAXSMALLBUF];\
extern char rootPath[];\
extern swidget pps_query, pps_plan, pps_policy;\
extern swidget nojoy;\
extern char IsAuthorizedUser;\
\
swidget create_pps_query(), create_pps_plan(), create_pps_policy();\
swidget create_nojoy();\

*pps_main.ispecdecl:
*pps_main.funcdecl: swidget create_pps_main(swidget UxParent)
*pps_main.funcname: create_pps_main
*pps_main.funcdef: "swidget", "<create_pps_main>(%)"
*pps_main.argdecl: swidget UxParent;
*pps_main.arglist: UxParent
*pps_main.arglist.UxParent: "swidget", "%UxParent%"
*pps_main.icode: (void)sprintf(main_labelPixmapString, "%s/%s/whitebear3.xpm",\
rootPath, PPS_PIXMAP_SUBPATH);
*pps_main.fcode: pps_query = create_pps_query(NO_PARENT);\
if (IsAuthorizedUser)\
{\
    pps_plan = create_pps_plan(NO_PARENT);\
    pps_policy = create_pps_policy(NO_PARENT);\
    nojoy = create_nojoy(NO_PARENT);\
}\
else\
{\
    XtSetSensitive(UxGetWidget(pb_main_plan), False);\
    XtSetSensitive(UxGetWidget(pb_main_policy), False);\
}\
return(rtrn);\

*pps_main.auxdecl:
*pps_main.name.source: public
*pps_main.static: false
*pps_main.name: pps_main
*pps_main.parent: NO_PARENT
*pps_main.parentExpression: UxParent
*pps_main.width: 699
*pps_main.height: 570
*pps_main.isCompound: "true"
*pps_main.compoundIcon: "toplevelS.xpm"
*pps_main.compoundName: "topLevel_Shell"
*pps_main.x: 225
*pps_main.y: 225
*pps_main.iconName: "pps"

*f_main.class: form
*f_main.static: true
*f_main.name: f_main
*f_main.parent: pps_main
*f_main.width: 436
*f_main.height: 228
*f_main.resizePolicy: "resize_none"
*f_main.isCompound: "true"
*f_main.compoundIcon: "form.xpm"
*f_main.compoundName: "form_"
*f_main.x: 60
*f_main.y: 60
*f_main.unitType: "pixels"

*l_main_bear.class: label
*l_main_bear.static: true
*l_main_bear.name: l_main_bear
*l_main_bear.parent: f_main
*l_main_bear.isCompound: "true"
*l_main_bear.compoundIcon: "label.xpm"
*l_main_bear.compoundName: "label_"
*l_main_bear.x: 138
*l_main_bear.y: 205
*l_main_bear.width: 448
*l_main_bear.height: 256
*l_main_bear.labelPixmap: main_labelPixmapString
*l_main_bear.labelType: "pixmap"

*l_main_title.class: label
*l_main_title.static: true
*l_main_title.name: l_main_title
*l_main_title.parent: f_main
*l_main_title.isCompound: "true"
*l_main_title.compoundIcon: "label.xpm"
*l_main_title.compoundName: "label_"
*l_main_title.x: 60
*l_main_title.y: 52
*l_main_title.width: 604
*l_main_title.height: 44
*l_main_title.labelString: "Alaska SAR Facility Production Planning System"
*l_main_title.fontList: "-adobe-helvetica-bold-r-normal--24-240-75-75-p-138-iso8859-1"

*pb_main_query.class: pushButton
*pb_main_query.static: true
*pb_main_query.name: pb_main_query
*pb_main_query.parent: f_main
*pb_main_query.isCompound: "true"
*pb_main_query.compoundIcon: "push.xpm"
*pb_main_query.compoundName: "push_Button"
*pb_main_query.x: 38
*pb_main_query.y: 490
*pb_main_query.width: 180
*pb_main_query.height: 56
*pb_main_query.labelString: "Query"
*pb_main_query.fontList: "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"
*pb_main_query.activateCallback: {\
cb_main_query();\
}

*pb_main_plan.class: pushButton
*pb_main_plan.static: true
*pb_main_plan.name: pb_main_plan
*pb_main_plan.parent: f_main
*pb_main_plan.isCompound: "true"
*pb_main_plan.compoundIcon: "push.xpm"
*pb_main_plan.compoundName: "push_Button"
*pb_main_plan.x: 267
*pb_main_plan.y: 492
*pb_main_plan.width: 180
*pb_main_plan.height: 56
*pb_main_plan.labelString: "Plan"
*pb_main_plan.fontList: "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"
*pb_main_plan.activateCallback: cb_main_plan();

*pb_main_policy.class: pushButton
*pb_main_policy.static: true
*pb_main_policy.name: pb_main_policy
*pb_main_policy.parent: f_main
*pb_main_policy.isCompound: "true"
*pb_main_policy.compoundIcon: "push.xpm"
*pb_main_policy.compoundName: "push_Button"
*pb_main_policy.x: 477
*pb_main_policy.y: 493
*pb_main_policy.width: 196
*pb_main_policy.height: 56
*pb_main_policy.labelString: "Policy"
*pb_main_policy.fontList: "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1"
*pb_main_policy.activateCallback: cb_main_policy();

*mb_main.class: rowColumn
*mb_main.static: true
*mb_main.name: mb_main
*mb_main.parent: f_main
*mb_main.rowColumnType: "menu_bar"
*mb_main.isCompound: "true"
*mb_main.compoundIcon: "pulldownM.xpm"
*mb_main.compoundName: "menu_Bar"
*mb_main.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*mb_main.x: 0
*mb_main.y: 0
*mb_main.width: 504
*mb_main.height: 36
*mb_main.menuAccelerator: "<KeyUp>F10"
*mb_main.rightAttachment: "attach_form"
*mb_main.leftAttachment: "attach_form"

*pb_main_file.class: rowColumn
*pb_main_file.static: true
*pb_main_file.name: pb_main_file
*pb_main_file.parent: mb_main
*pb_main_file.rowColumnType: "menu_pulldown"

*pb_main_print_screen.class: pushButton
*pb_main_print_screen.static: true
*pb_main_print_screen.name: pb_main_print_screen
*pb_main_print_screen.parent: pb_main_file
*pb_main_print_screen.labelString: "Print Screen"
*pb_main_print_screen.activateCallback: {\
extern void pps_print_screen(swidget sw);\
\
pps_print_screen(pps_main);\
\
}

*pb_main_exit.class: pushButton
*pb_main_exit.static: true
*pb_main_exit.name: pb_main_exit
*pb_main_exit.parent: pb_main_file
*pb_main_exit.labelString: "Exit"
*pb_main_exit.mnemonic: "x"
*pb_main_exit.activateCallback: {\
UxPopdownInterface(pps_main);\
closelog();   /* close the syslog */\
exit(0);\
}

*mb_main_file.class: cascadeButton
*mb_main_file.static: true
*mb_main_file.name: mb_main_file
*mb_main_file.parent: mb_main
*mb_main_file.labelString: "File"
*mb_main_file.subMenuId: "pb_main_file"
*mb_main_file.mnemonic: "F"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: f_main
*label1.x: 92
*label1.y: 99
*label1.width: 541
*label1.height: 73
*label1.labelString: "Copyright (c) 1996, California Institute of Technology.\nALL RIGHTS RESERVED.\nU.S. Government Sponsorship acknowledged."

