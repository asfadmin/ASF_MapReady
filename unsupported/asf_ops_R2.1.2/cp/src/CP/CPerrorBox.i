! UIMX ascii 2.9 key: 1725                                                      

*CPerrorBox.class: errorDialog
*CPerrorBox.gbldecl: #include <stdio.h>\
static char sccsid_CPerrorBox_i[] = "@(#)CPerrorBox.i	2.4 95/03/24 18:47:50"; \
\

*CPerrorBox.ispecdecl:
*CPerrorBox.funcdecl: swidget create_CPerrorBox(UxParent)\
swidget UxParent;
*CPerrorBox.funcname: create_CPerrorBox
*CPerrorBox.funcdef: "swidget", "<create_CPerrorBox>(%)"
*CPerrorBox.argdecl: swidget UxParent;
*CPerrorBox.arglist: UxParent
*CPerrorBox.arglist.UxParent: "swidget", "%UxParent%"
*CPerrorBox.icode:
*CPerrorBox.fcode: return(rtrn);\

*CPerrorBox.auxdecl:
*CPerrorBox.static: true
*CPerrorBox.name: CPerrorBox
*CPerrorBox.parent: NO_PARENT
*CPerrorBox.parentExpression: UxParent
*CPerrorBox.defaultShell: topLevelShell
*CPerrorBox.msgDialogType: "dialog_error"
*CPerrorBox.height: 160
*CPerrorBox.isCompound: "true"
*CPerrorBox.compoundIcon: "errorD.xpm"
*CPerrorBox.compoundName: "error_Dialog"
*CPerrorBox.x: 384
*CPerrorBox.y: 398
*CPerrorBox.unitType: "pixels"
*CPerrorBox.messageString: "default process died"
*CPerrorBox.dialogStyle: "dialog_modeless"
*CPerrorBox.dialogTitle: "Error"
*CPerrorBox.createCallback: {\
XtUnmanageChild(XmMessageBoxGetChild(UxWidget,\
                                     XmDIALOG_CANCEL_BUTTON));\
XtUnmanageChild(XmMessageBoxGetChild(UxWidget,\
                                     XmDIALOG_HELP_BUTTON));\
\
}
*CPerrorBox.messageAlignment: "alignment_center"
*CPerrorBox.defaultPosition: "false"

