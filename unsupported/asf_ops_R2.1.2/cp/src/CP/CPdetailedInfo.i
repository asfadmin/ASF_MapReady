! UIMX ascii 2.9 key: 7890                                                      

*CPdetailedInfo.class: topLevelShell
*CPdetailedInfo.classinc:
*CPdetailedInfo.classspec:
*CPdetailedInfo.classmembers:
*CPdetailedInfo.classconstructor:
*CPdetailedInfo.classdestructor:
*CPdetailedInfo.gbldecl: #include <stdio.h>\
static char sccsid_CPdetailedInfo_i[] = "@(#)CPdetailedInfo.i	1.8 96/07/16 16:50:05";
*CPdetailedInfo.ispecdecl:
*CPdetailedInfo.funcdecl: swidget create_CPdetailedInfo(swidget UxParent)
*CPdetailedInfo.funcname: create_CPdetailedInfo
*CPdetailedInfo.funcdef: "swidget", "<create_CPdetailedInfo>(%)"
*CPdetailedInfo.argdecl: swidget UxParent;
*CPdetailedInfo.arglist: UxParent
*CPdetailedInfo.arglist.UxParent: "swidget", "%UxParent%"
*CPdetailedInfo.icode:
*CPdetailedInfo.fcode: return(rtrn);\

*CPdetailedInfo.auxdecl:
*CPdetailedInfo.name.source: public
*CPdetailedInfo.static: false
*CPdetailedInfo.name: CPdetailedInfo
*CPdetailedInfo.parent: NO_PARENT
*CPdetailedInfo.parentExpression: UxParent
*CPdetailedInfo.x: 470
*CPdetailedInfo.y: 120
*CPdetailedInfo.width: 470
*CPdetailedInfo.height: 510
*CPdetailedInfo.deleteResponse: "unmap"
*CPdetailedInfo.iconName: "SPS Job Detailed Information"
*CPdetailedInfo.allowShellResize: "true"

*CPdetailedMainWindow.class: mainWindow
*CPdetailedMainWindow.name.source: public
*CPdetailedMainWindow.static: false
*CPdetailedMainWindow.name: CPdetailedMainWindow
*CPdetailedMainWindow.parent: CPdetailedInfo
*CPdetailedMainWindow.unitType: "pixels"
*CPdetailedMainWindow.x: 350
*CPdetailedMainWindow.y: 130

*menu1.class: rowColumn
*menu1.static: true
*menu1.name: menu1
*menu1.parent: CPdetailedMainWindow
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
*menu1_p1_b1.activateCallback: UxPopdownInterface(CPdetailedInfo);

*menu1_top_b1.class: cascadeButton
*menu1_top_b1.static: true
*menu1_top_b1.name: menu1_top_b1
*menu1_top_b1.parent: menu1
*menu1_top_b1.labelString: "File"
*menu1_top_b1.subMenuId: "menu1_p1"
*menu1_top_b1.mnemonic: "F"

*CPdetailedForm.class: form
*CPdetailedForm.name.source: public
*CPdetailedForm.static: false
*CPdetailedForm.name: CPdetailedForm
*CPdetailedForm.parent: CPdetailedMainWindow
*CPdetailedForm.horizontalSpacing: 5
*CPdetailedForm.verticalSpacing: 5

*DetailedJob_RC.class: rowColumn
*DetailedJob_RC.static: true
*DetailedJob_RC.name: DetailedJob_RC
*DetailedJob_RC.parent: CPdetailedForm
*DetailedJob_RC.width: 171
*DetailedJob_RC.numColumns: 2
*DetailedJob_RC.packing: "pack_column"
*DetailedJob_RC.topOffset: 20
*DetailedJob_RC.leftAttachment: "attach_position"
*DetailedJob_RC.leftPosition: 30

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: DetailedJob_RC
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: -10
*label1.y: 10
*label1.width: 110
*label1.height: 20
*label1.alignment: "alignment_end"
*label1.labelString: "Job Id"

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: DetailedJob_RC
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: -10
*label2.y: 10
*label2.width: 110
*label2.height: 20
*label2.labelString: "Subsystem"
*label2.alignment: "alignment_end"
*label2.positionIndex: 1

*CPdetailedJobId.class: label
*CPdetailedJobId.name.source: public
*CPdetailedJobId.static: false
*CPdetailedJobId.name: CPdetailedJobId
*CPdetailedJobId.parent: DetailedJob_RC
*CPdetailedJobId.isCompound: "true"
*CPdetailedJobId.compoundIcon: "label.xpm"
*CPdetailedJobId.compoundName: "label_"
*CPdetailedJobId.x: 13
*CPdetailedJobId.y: 13
*CPdetailedJobId.width: 110
*CPdetailedJobId.height: 20
*CPdetailedJobId.labelString: "1234"
*CPdetailedJobId.positionIndex: 2

*CPdetailedSubsys.class: label
*CPdetailedSubsys.name.source: public
*CPdetailedSubsys.static: false
*CPdetailedSubsys.name: CPdetailedSubsys
*CPdetailedSubsys.parent: DetailedJob_RC
*CPdetailedSubsys.isCompound: "true"
*CPdetailedSubsys.compoundIcon: "label.xpm"
*CPdetailedSubsys.compoundName: "label_"
*CPdetailedSubsys.x: 13
*CPdetailedSubsys.y: 33
*CPdetailedSubsys.width: 110
*CPdetailedSubsys.height: 20
*CPdetailedSubsys.labelString: "ASP"
*CPdetailedSubsys.positionIndex: XmLAST_POSITION

*CPdecodedFiles_label.class: label
*CPdecodedFiles_label.static: true
*CPdecodedFiles_label.name: CPdecodedFiles_label
*CPdecodedFiles_label.parent: CPdetailedForm
*CPdecodedFiles_label.isCompound: "true"
*CPdecodedFiles_label.compoundIcon: "label.xpm"
*CPdecodedFiles_label.compoundName: "label_"
*CPdecodedFiles_label.x: 22
*CPdecodedFiles_label.y: 73
*CPdecodedFiles_label.height: 20
*CPdecodedFiles_label.labelString: "Decoded Files:"
*CPdecodedFiles_label.alignment: "alignment_end"
*CPdecodedFiles_label.topAttachment: "attach_widget"
*CPdecodedFiles_label.topWidget: "DetailedJob_RC"
*CPdecodedFiles_label.leftAttachment: "attach_position"
*CPdecodedFiles_label.leftPosition: 10

*CPproductFiles_label.class: label
*CPproductFiles_label.static: true
*CPproductFiles_label.name: CPproductFiles_label
*CPproductFiles_label.parent: CPdetailedForm
*CPproductFiles_label.isCompound: "true"
*CPproductFiles_label.compoundIcon: "label.xpm"
*CPproductFiles_label.compoundName: "label_"
*CPproductFiles_label.x: 23
*CPproductFiles_label.y: 93
*CPproductFiles_label.height: 20
*CPproductFiles_label.labelString: "Product Files:"
*CPproductFiles_label.alignment: "alignment_end"
*CPproductFiles_label.topAttachment: "attach_widget"
*CPproductFiles_label.topWidget: "CPdecodedFiles_label"
*CPproductFiles_label.leftAttachment: "attach_opposite_widget"
*CPproductFiles_label.rightOffset: 0
*CPproductFiles_label.leftWidget: "CPdecodedFiles_label"
*CPproductFiles_label.leftOffset: 0

*CPdetailedScrolledWin.class: scrolledWindow
*CPdetailedScrolledWin.name.source: public
*CPdetailedScrolledWin.static: false
*CPdetailedScrolledWin.name: CPdetailedScrolledWin
*CPdetailedScrolledWin.parent: CPdetailedForm
*CPdetailedScrolledWin.x: 40
*CPdetailedScrolledWin.y: 100
*CPdetailedScrolledWin.scrollBarDisplayPolicy: "static"
*CPdetailedScrolledWin.bottomAttachment: "attach_form"
*CPdetailedScrolledWin.leftAttachment: "attach_form"
*CPdetailedScrolledWin.rightAttachment: "attach_form"
*CPdetailedScrolledWin.topAttachment: "attach_widget"
*CPdetailedScrolledWin.topWidget: "CPproductFiles_label"

*CPdetailedOdlText.class: scrolledText
*CPdetailedOdlText.name.source: public
*CPdetailedOdlText.static: false
*CPdetailedOdlText.name: CPdetailedOdlText
*CPdetailedOdlText.parent: CPdetailedScrolledWin
*CPdetailedOdlText.width: 360
*CPdetailedOdlText.height: 380
*CPdetailedOdlText.editMode: "multi_line_edit"
*CPdetailedOdlText.editable: "false"

*CPdetailedInputFiles.class: label
*CPdetailedInputFiles.name.source: public
*CPdetailedInputFiles.static: false
*CPdetailedInputFiles.name: CPdetailedInputFiles
*CPdetailedInputFiles.parent: CPdetailedForm
*CPdetailedInputFiles.isCompound: "true"
*CPdetailedInputFiles.compoundIcon: "label.xpm"
*CPdetailedInputFiles.compoundName: "label_"
*CPdetailedInputFiles.x: 147
*CPdetailedInputFiles.y: 69
*CPdetailedInputFiles.width: 110
*CPdetailedInputFiles.height: 20
*CPdetailedInputFiles.labelString: "None"
*CPdetailedInputFiles.alignment: "alignment_beginning"
*CPdetailedInputFiles.leftAttachment: "attach_widget"
*CPdetailedInputFiles.leftWidget: "CPdecodedFiles_label"
*CPdetailedInputFiles.topAttachment: "attach_opposite_widget"
*CPdetailedInputFiles.topWidget: "CPdecodedFiles_label"
*CPdetailedInputFiles.topOffset: 0
*CPdetailedInputFiles.rightAttachment: "attach_form"
*CPdetailedInputFiles.rightOffset: 20

*CPdetailedOutputFiles.class: label
*CPdetailedOutputFiles.name.source: public
*CPdetailedOutputFiles.static: false
*CPdetailedOutputFiles.name: CPdetailedOutputFiles
*CPdetailedOutputFiles.parent: CPdetailedForm
*CPdetailedOutputFiles.isCompound: "true"
*CPdetailedOutputFiles.compoundIcon: "label.xpm"
*CPdetailedOutputFiles.compoundName: "label_"
*CPdetailedOutputFiles.x: 169
*CPdetailedOutputFiles.y: 87
*CPdetailedOutputFiles.width: 110
*CPdetailedOutputFiles.height: 20
*CPdetailedOutputFiles.labelString: "None"
*CPdetailedOutputFiles.alignment: "alignment_beginning"
*CPdetailedOutputFiles.leftAttachment: "attach_widget"
*CPdetailedOutputFiles.leftWidget: "CPproductFiles_label"
*CPdetailedOutputFiles.topAttachment: "attach_opposite_widget"
*CPdetailedOutputFiles.topWidget: "CPproductFiles_label"
*CPdetailedOutputFiles.topOffset: 0
*CPdetailedOutputFiles.rightAttachment: "attach_form"
*CPdetailedOutputFiles.rightOffset: 20

