! UIMX ascii 2.9 key: 904                                                       

*CPhelp.class: topLevelShell
*CPhelp.gbldecl: #include <stdio.h>\
static char sccsid_CPhelp_i[] = "@(#)CPhelp.i	1.3 96/07/18 19:02:01"; \

*CPhelp.funcdecl: swidget create_CPhelp(swidget UxParent)
*CPhelp.funcname: create_CPhelp
*CPhelp.funcdef: "swidget", "<create_CPhelp>(%)"
*CPhelp.argdecl: swidget UxParent;
*CPhelp.arglist: UxParent
*CPhelp.arglist.UxParent: "swidget", "%UxParent%"
*CPhelp.fcode: return(rtrn);\

*CPhelp.name.source: public
*CPhelp.static: false
*CPhelp.name: CPhelp
*CPhelp.parent: NO_PARENT
*CPhelp.parentExpression: UxParent
*CPhelp.x: 470
*CPhelp.y: 120
*CPhelp.width: 700
*CPhelp.height: 510
*CPhelp.deleteResponse: "unmap"
*CPhelp.iconName: "SPS Help"

*mainWindow2.class: mainWindow
*mainWindow2.static: true
*mainWindow2.name: mainWindow2
*mainWindow2.parent: CPhelp
*mainWindow2.unitType: "pixels"
*mainWindow2.x: 0
*mainWindow2.y: -30
*mainWindow2.width: 10
*mainWindow2.height: 40

*menu1.class: rowColumn
*menu1.static: true
*menu1.name: menu1
*menu1.parent: mainWindow2
*menu1.rowColumnType: "menu_bar"
*menu1.menuAccelerator: "<KeyUp>F10"

*menu1_p1.class: rowColumn
*menu1_p1.static: true
*menu1_p1.name: menu1_p1
*menu1_p1.parent: menu1
*menu1_p1.rowColumnType: "menu_pulldown"

*menu1_p1_b1.class: pushButtonGadget
*menu1_p1_b1.static: true
*menu1_p1_b1.name: menu1_p1_b1
*menu1_p1_b1.parent: menu1_p1
*menu1_p1_b1.labelString: "Exit"
*menu1_p1_b1.mnemonic: "x"
*menu1_p1_b1.activateCallback: UxPopdownInterface(CPhelp);

*menu1_top_b1.class: cascadeButton
*menu1_top_b1.static: true
*menu1_top_b1.name: menu1_top_b1
*menu1_top_b1.parent: menu1
*menu1_top_b1.labelString: "File"
*menu1_top_b1.subMenuId: "menu1_p1"
*menu1_top_b1.mnemonic: "F"

*form1.class: form
*form1.static: true
*form1.name: form1
*form1.parent: mainWindow2
*form1.horizontalSpacing: 5
*form1.verticalSpacing: 5

*scrolledWindow1.class: scrolledWindow
*scrolledWindow1.static: true
*scrolledWindow1.name: scrolledWindow1
*scrolledWindow1.parent: form1
*scrolledWindow1.scrollBarDisplayPolicy: "static"
*scrolledWindow1.height: 362
*scrolledWindow1.width: 380
*scrolledWindow1.bottomAttachment: "attach_form"
*scrolledWindow1.leftAttachment: "attach_form"
*scrolledWindow1.rightAttachment: "attach_form"
*scrolledWindow1.y: 2
*scrolledWindow1.topAttachment: "attach_form"

*CPhelpText.class: scrolledText
*CPhelpText.name.source: public
*CPhelpText.static: false
*CPhelpText.name: CPhelpText
*CPhelpText.parent: scrolledWindow1
*CPhelpText.width.source: public
*CPhelpText.width: 441
*CPhelpText.height.source: public
*CPhelpText.height: 453
*CPhelpText.editMode: "multi_line_edit"
*CPhelpText.editable: "false"
*CPhelpText.text: "This is a sample help message"
*CPhelpText.fontList: "-misc-fixed-*-*-*-*-14-*"

