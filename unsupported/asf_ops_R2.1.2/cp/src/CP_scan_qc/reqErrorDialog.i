! UIMX ascii 2.9 key: 5692                                                      

*reqErrorDialog.class: errorDialog
*reqErrorDialog.classinc:
*reqErrorDialog.classspec:
*reqErrorDialog.classmembers:
*reqErrorDialog.classconstructor:
*reqErrorDialog.classdestructor:
*reqErrorDialog.gbldecl: #include <stdio.h>\
#include "scan_qc_def.h"\
static char sccsid_reqErrorDialog_i[] = "@(#)reqErrorDialog.i	1.15 97/04/30 14:44:15"; 
*reqErrorDialog.ispecdecl:
*reqErrorDialog.funcdecl: swidget create_reqErrorDialog(swidget UxParent)
*reqErrorDialog.funcname: create_reqErrorDialog
*reqErrorDialog.funcdef: "swidget", "<create_reqErrorDialog>(%)"
*reqErrorDialog.argdecl: swidget UxParent;
*reqErrorDialog.arglist: UxParent
*reqErrorDialog.arglist.UxParent: "swidget", "%UxParent%"
*reqErrorDialog.icode:
*reqErrorDialog.fcode: return(rtrn);\

*reqErrorDialog.auxdecl:
*reqErrorDialog.name.source: public
*reqErrorDialog.static: false
*reqErrorDialog.name: reqErrorDialog
*reqErrorDialog.parent: NO_PARENT
*reqErrorDialog.parentExpression: UxParent
*reqErrorDialog.defaultShell: applicationShell
*reqErrorDialog.msgDialogType: "dialog_error"
*reqErrorDialog.unitType: "pixels"
*reqErrorDialog.x: 191
*reqErrorDialog.y: 549
*reqErrorDialog.width: 600
*reqErrorDialog.height: 160
*reqErrorDialog.buttonFontList: "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso2022-l442r641"
*reqErrorDialog.labelFontList: "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
*reqErrorDialog.textFontList: "-adobe-helvetica-bold-o-normal--14-100-100-100-p-82-iso8859-1"
*reqErrorDialog.dialogTitle: "CP_scan_qc -- Error"
*reqErrorDialog.messageString: "CP_scan_qc Error Message"
*reqErrorDialog.createCallback: {\
XtUnmanageChild(XmMessageBoxGetChild(UxWidget,\
                                     XmDIALOG_CANCEL_BUTTON));\
XtUnmanageChild(XmMessageBoxGetChild(UxWidget,\
                                     XmDIALOG_HELP_BUTTON));\
}
*reqErrorDialog.background: "#ff0000"
*reqErrorDialog.okCallback: {\
error_cb();\
}

