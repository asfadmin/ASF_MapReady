! UIMX ascii 2.9 key: 2651                                                      

*MUInterval_dialogShell.class: dialogShell
*MUInterval_dialogShell.classinc:
*MUInterval_dialogShell.classspec:
*MUInterval_dialogShell.classmembers:
*MUInterval_dialogShell.classconstructor:
*MUInterval_dialogShell.classdestructor:
*MUInterval_dialogShell.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.\
#endif\
 \
/*==============================================================================\
Filename:			vc_muinterval.c\
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
#pragma ident   "@(#)MUIntervalDialog.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.MUIntervalDialog.i"\
\
#include "gui_utils.h"\
#include "cb_permstatus.h"\
\
extern swidget	MUPermissionStatus;\

*MUInterval_dialogShell.ispecdecl:
*MUInterval_dialogShell.funcdecl: swidget create_MUInterval_dialogShell(swidget UxParent)
*MUInterval_dialogShell.funcname: create_MUInterval_dialogShell
*MUInterval_dialogShell.funcdef: "swidget", "<create_MUInterval_dialogShell>(%)"
*MUInterval_dialogShell.argdecl: swidget UxParent;
*MUInterval_dialogShell.arglist: UxParent
*MUInterval_dialogShell.arglist.UxParent: "swidget", "%UxParent%"
*MUInterval_dialogShell.icode:
*MUInterval_dialogShell.fcode: cb_init_permstatus( rtrn, NULL, NULL ) ;\
\
return(rtrn);\

*MUInterval_dialogShell.auxdecl:
*MUInterval_dialogShell.static: true
*MUInterval_dialogShell.name: MUInterval_dialogShell
*MUInterval_dialogShell.parent: NO_PARENT
*MUInterval_dialogShell.parentExpression: MUPermissionStatus
*MUInterval_dialogShell.width: 245
*MUInterval_dialogShell.height: 135
*MUInterval_dialogShell.isCompound: "true"
*MUInterval_dialogShell.compoundIcon: "dialogS.xpm"
*MUInterval_dialogShell.compoundName: "dialog_Shell"
*MUInterval_dialogShell.x: 78
*MUInterval_dialogShell.y: 617

*MUIntervalDialog.class: messageBox
*MUIntervalDialog.name.source: public
*MUIntervalDialog.static: false
*MUIntervalDialog.name: MUIntervalDialog
*MUIntervalDialog.parent: MUInterval_dialogShell
*MUIntervalDialog.width: 243
*MUIntervalDialog.height: 113
*MUIntervalDialog.isCompound: "true"
*MUIntervalDialog.compoundIcon: "message.xpm"
*MUIntervalDialog.compoundName: "message_Box"
*MUIntervalDialog.unitType: "pixels"
*MUIntervalDialog.msgDialogType: "dialog_template"
*MUIntervalDialog.cancelLabelString: "Cancel"
*MUIntervalDialog.helpLabelString: "Reset"
*MUIntervalDialog.okLabelString: "Ok"
*MUIntervalDialog.initialFocus: "Interval_tf"
*MUIntervalDialog.helpCallback.source: public
*MUIntervalDialog.helpCallback: cb_undo_interval_popup
*MUIntervalDialog.helpCallbackClientData: (XtPointer) MU_INTERVAL_RESET
*MUIntervalDialog.okCallback.source: public
*MUIntervalDialog.okCallback: cb_interval_popup_OK
*MUIntervalDialog.cancelCallback.source: public
*MUIntervalDialog.cancelCallback: cb_undo_interval_popup
*MUIntervalDialog.cancelCallbackClientData: (XtPointer) MU_INTERVAL_CANCEL
*MUIntervalDialog.x: 459
*MUIntervalDialog.dialogStyle: "dialog_full_application_modal"

*MU_Interval_rc.class: rowColumn
*MU_Interval_rc.static: true
*MU_Interval_rc.name: MU_Interval_rc
*MU_Interval_rc.parent: MUIntervalDialog
*MU_Interval_rc.width: 364
*MU_Interval_rc.height: 28
*MU_Interval_rc.isCompound: "true"
*MU_Interval_rc.compoundIcon: "row.xpm"
*MU_Interval_rc.compoundName: "row_Column"
*MU_Interval_rc.x: 14
*MU_Interval_rc.y: 30
*MU_Interval_rc.orientation: "horizontal"

*label42.class: label
*label42.static: true
*label42.name: label42
*label42.parent: MU_Interval_rc
*label42.isCompound: "true"
*label42.compoundIcon: "label.xpm"
*label42.compoundName: "label_"
*label42.x: 11
*label42.y: 10
*label42.width: 86
*label42.height: 16
*label42.labelString: "auto-update every "

*Interval_tf.class: textField
*Interval_tf.name.source: public
*Interval_tf.static: false
*Interval_tf.name: Interval_tf
*Interval_tf.parent: MU_Interval_rc
*Interval_tf.width: 54
*Interval_tf.isCompound: "true"
*Interval_tf.compoundIcon: "textfield.xpm"
*Interval_tf.compoundName: "text_Field"
*Interval_tf.x: 111
*Interval_tf.y: 0
*Interval_tf.height: 32
*Interval_tf.columns: 5
*Interval_tf.text: "5"
*Interval_tf.cursorPosition: 1

*label62.class: label
*label62.static: true
*label62.name: label62
*label62.parent: MU_Interval_rc
*label62.isCompound: "true"
*label62.compoundIcon: "label.xpm"
*label62.compoundName: "label_"
*label62.x: 171
*label62.y: 10
*label62.width: 97
*label62.height: 20
*label62.labelString: " seconds"

