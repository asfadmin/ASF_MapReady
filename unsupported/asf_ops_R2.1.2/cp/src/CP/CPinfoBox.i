! UIMX ascii 2.6 key: 7113                                                      

*CPinfoBox.class: informationDialog
*CPinfoBox.gbldecl: #include <stdio.h>\
static char sccsid_CPinfoBox_i[] = "@(#)CPinfoBox.i	2.8 96/05/08 17:58:29"; \
\

*CPinfoBox.ispecdecl:
*CPinfoBox.funcdecl: swidget create_CPinfoBox(UxParent)\
swidget UxParent;
*CPinfoBox.funcname: create_CPinfoBox
*CPinfoBox.funcdef: "swidget", "<create_CPinfoBox>(%)"
*CPinfoBox.argdecl: swidget UxParent;
*CPinfoBox.arglist: UxParent
*CPinfoBox.arglist.UxParent: "swidget", "%UxParent%"
*CPinfoBox.icode:
*CPinfoBox.fcode: return(rtrn);\

*CPinfoBox.auxdecl:
*CPinfoBox.name.source: public
*CPinfoBox.static: false
*CPinfoBox.name: CPinfoBox
*CPinfoBox.parent: NO_PARENT
*CPinfoBox.parentExpression: UxParent
*CPinfoBox.defaultShell: topLevelShell
*CPinfoBox.dialogTitle: "Information"
*CPinfoBox.msgDialogType: "dialog_information"
*CPinfoBox.unitType: "pixels"
*CPinfoBox.x: 453
*CPinfoBox.y: 382
*CPinfoBox.height: 146
*CPinfoBox.createCallback: {\
  XtUnmanageChild(XmMessageBoxGetChild(UxThisWidget,\
                                       XmDIALOG_HELP_BUTTON) );\
\
  XtUnmanageChild(XmMessageBoxGetChild(UxThisWidget,\
                                       XmDIALOG_CANCEL_BUTTON) );\
\
}
*CPinfoBox.messageString: "default info string"
*CPinfoBox.okCallback: {\
XtDestroyWidget(UxWidget);\
}
*CPinfoBox.defaultPosition: "false"

