! UIMX ascii 2.9 key: 4589                                                      

*reqHelpDialog.class: informationDialog
*reqHelpDialog.classinc:
*reqHelpDialog.classspec:
*reqHelpDialog.classmembers:
*reqHelpDialog.classconstructor:
*reqHelpDialog.classdestructor:
*reqHelpDialog.gbldecl: #include <stdio.h>\
#include "scan_qc_def.h"\
static char sccsid_reqHelpDialog_i[] = "@(#)reqHelpDialog.i	1.11 97/04/30 14:44:02";\

*reqHelpDialog.ispecdecl:
*reqHelpDialog.funcdecl: swidget create_reqHelpDialog(swidget UxParent)
*reqHelpDialog.funcname: create_reqHelpDialog
*reqHelpDialog.funcdef: "swidget", "<create_reqHelpDialog>(%)"
*reqHelpDialog.argdecl: swidget UxParent;
*reqHelpDialog.arglist: UxParent
*reqHelpDialog.arglist.UxParent: "swidget", "%UxParent%"
*reqHelpDialog.icode:
*reqHelpDialog.fcode: return(rtrn);\

*reqHelpDialog.auxdecl:
*reqHelpDialog.name.source: public
*reqHelpDialog.static: false
*reqHelpDialog.name: reqHelpDialog
*reqHelpDialog.parent: NO_PARENT
*reqHelpDialog.parentExpression: UxParent
*reqHelpDialog.defaultShell: applicationShell
*reqHelpDialog.msgDialogType: "dialog_information"
*reqHelpDialog.unitType: "pixels"
*reqHelpDialog.x: 0
*reqHelpDialog.y: 0
*reqHelpDialog.width: 700
*reqHelpDialog.height: 300
*reqHelpDialog.dialogTitle: "CP_scan_qc -- Help"
*reqHelpDialog.messageString: "CP_scan_qc Help Message"
*reqHelpDialog.createCallback: {\
XtUnmanageChild(XmMessageBoxGetChild(UxWidget,\
                                     XmDIALOG_CANCEL_BUTTON));\
XtUnmanageChild(XmMessageBoxGetChild(UxWidget,\
                                     XmDIALOG_HELP_BUTTON));\
}
*reqHelpDialog.okCallback: {\
XtDestroyWidget(UxWidget);\
}
*reqHelpDialog.defaultPosition: "true"

