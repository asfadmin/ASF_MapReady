! UIMX ascii 2.6 key: 8157                                                      

*CPfileBox.class: fileSelectionBoxDialog
*CPfileBox.gbldecl: #include <stdio.h>\
static char sccsid_CPfileBox_i[] = "@(#)CPfileBox.i	2.7 96/02/21 13:53:04"; \
\

*CPfileBox.ispecdecl:
*CPfileBox.funcdecl: swidget create_CPfileBox(UxParent)\
swidget UxParent;
*CPfileBox.funcname: create_CPfileBox
*CPfileBox.funcdef: "swidget", "<create_CPfileBox>(%)"
*CPfileBox.argdecl: swidget UxParent;
*CPfileBox.arglist: UxParent
*CPfileBox.arglist.UxParent: "swidget", "%UxParent%"
*CPfileBox.icode:
*CPfileBox.fcode: return(rtrn);\

*CPfileBox.auxdecl:
*CPfileBox.name.source: public
*CPfileBox.static: false
*CPfileBox.name: CPfileBox
*CPfileBox.parent: NO_PARENT
*CPfileBox.parentExpression: UxParent
*CPfileBox.defaultShell: topLevelShell
*CPfileBox.width: 315
*CPfileBox.height: 373
*CPfileBox.dialogType: "dialog_file_selection"
*CPfileBox.isCompound: "true"
*CPfileBox.compoundIcon: "fileboxD.xpm"
*CPfileBox.compoundName: "fileSBox_Dialog"
*CPfileBox.x: 457
*CPfileBox.y: 305
*CPfileBox.unitType: "pixels"
*CPfileBox.dialogTitle: "-- file select --"
*CPfileBox.autoUnmanage: "true"
*CPfileBox.createCallback: {\
doCreateMainWindowFileBox(UxWidget);\
}
*CPfileBox.okCallback: {\
doSaveRestore(UxWidget, UxClientData, UxCallbackArg);\
}
*CPfileBox.defaultPosition: "false"

