! UIMX ascii 2.9 key: 2745                                                      

*assign_datasets.class: form
*assign_datasets.classinc:
*assign_datasets.classspec:
*assign_datasets.classmembers:
*assign_datasets.classconstructor:
*assign_datasets.classdestructor:
*assign_datasets.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>\
\
\
\
\
\
\

*assign_datasets.ispecdecl:
*assign_datasets.funcdecl: swidget create_assign_datasets(swidget UxParent)
*assign_datasets.funcname: create_assign_datasets
*assign_datasets.funcdef: "swidget", "<create_assign_datasets>(%)"
*assign_datasets.argdecl: swidget UxParent;
*assign_datasets.arglist: UxParent
*assign_datasets.arglist.UxParent: "swidget", "%UxParent%"
*assign_datasets.icode:
*assign_datasets.fcode: return(rtrn);\

*assign_datasets.auxdecl:
*assign_datasets.name.source: public
*assign_datasets.static: false
*assign_datasets.name: assign_datasets
*assign_datasets.parent: NO_PARENT
*assign_datasets.parentExpression: UxParent
*assign_datasets.defaultShell: transientShell
*assign_datasets.width: 1036
*assign_datasets.height: 770
*assign_datasets.resizePolicy: "resize_none"
*assign_datasets.isCompound: "true"
*assign_datasets.compoundIcon: "form.xpm"
*assign_datasets.compoundName: "form_"
*assign_datasets.x: 9
*assign_datasets.y: 82
*assign_datasets.unitType: "pixels"
*assign_datasets.background: "#9ac0cd"
*assign_datasets.dialogTitle: "Assign Datasets"
*assign_datasets.sensitive: "true"
*assign_datasets.allowShellResize: "true"
*assign_datasets.createManaged: "true"
*assign_datasets.autoUnmanage: "true"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: assign_datasets
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 382
*label1.y: 18
*label1.width: 282
*label1.background: "#9ac0cd"
*label1.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label1.labelString: "Assign Datasets to Account"

*frame1.class: frame
*frame1.static: true
*frame1.name: frame1
*frame1.parent: assign_datasets
*frame1.width: 608
*frame1.height: 516
*frame1.isCompound: "true"
*frame1.compoundIcon: "frame.xpm"
*frame1.compoundName: "frame_"
*frame1.x: 402
*frame1.y: 204
*frame1.shadowThickness: 3
*frame1.background: "#9ac0cd"

*form1.class: form
*form1.static: true
*form1.name: form1
*form1.parent: frame1
*form1.width: 599
*form1.height: 510
*form1.resizePolicy: "resize_none"
*form1.isCompound: "true"
*form1.compoundIcon: "form.xpm"
*form1.compoundName: "form_"
*form1.x: 30
*form1.y: 3
*form1.background: "#9ac0cd"

*assigned_datasetsSW.class: scrolledWindow
*assigned_datasetsSW.static: true
*assigned_datasetsSW.name: assigned_datasetsSW
*assigned_datasetsSW.parent: form1
*assigned_datasetsSW.scrollingPolicy: "automatic"
*assigned_datasetsSW.visualPolicy: "constant"
*assigned_datasetsSW.scrollBarDisplayPolicy: "as_needed"
*assigned_datasetsSW.shadowThickness: 1
*assigned_datasetsSW.isCompound: "true"
*assigned_datasetsSW.compoundIcon: "scrllist.xpm"
*assigned_datasetsSW.compoundName: "scrolled_List"
*assigned_datasetsSW.x: 20
*assigned_datasetsSW.y: 40
*assigned_datasetsSW.width: 290
*assigned_datasetsSW.height: 398
*assigned_datasetsSW.background: "#9ac0cd"
*assigned_datasetsSW.borderWidth: 0

*assigned_datasetsSL.class: scrolledList
*assigned_datasetsSL.static: true
*assigned_datasetsSL.name: assigned_datasetsSL
*assigned_datasetsSL.parent: assigned_datasetsSW
*assigned_datasetsSL.width: 600
*assigned_datasetsSL.height: 369
*assigned_datasetsSL.background: "LightSkyBlue3"
*assigned_datasetsSL.listSizePolicy: "variable"
*assigned_datasetsSL.itemCount: 0
*assigned_datasetsSL.selectionPolicy: "extended_select"
*assigned_datasetsSL.scrollBarDisplayPolicy: "as_needed"
*assigned_datasetsSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*assigned_datasetsSL.extendedSelectionCallback.source: public
*assigned_datasetsSL.extendedSelectionCallback: assign_datasets_assigned_listCb

*scrolledWindowList4.class: scrolledWindow
*scrolledWindowList4.static: true
*scrolledWindowList4.name: scrolledWindowList4
*scrolledWindowList4.parent: form1
*scrolledWindowList4.scrollingPolicy: "automatic"
*scrolledWindowList4.visualPolicy: "constant"
*scrolledWindowList4.scrollBarDisplayPolicy: "as_needed"
*scrolledWindowList4.shadowThickness: 1
*scrolledWindowList4.isCompound: "true"
*scrolledWindowList4.compoundIcon: "scrllist.xpm"
*scrolledWindowList4.compoundName: "scrolled_List"
*scrolledWindowList4.x: 322
*scrolledWindowList4.y: 40
*scrolledWindowList4.width: 39
*scrolledWindowList4.height: 373
*scrolledWindowList4.background: "#9ac0cd"
*scrolledWindowList4.borderWidth: 0

*orderSL.class: scrolledList
*orderSL.static: true
*orderSL.name: orderSL
*orderSL.parent: scrolledWindowList4
*orderSL.width: 36
*orderSL.height: 371
*orderSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderSL.background: "LightSkyBlue3"
*orderSL.listSizePolicy: "constant"
*orderSL.itemCount: 0
*orderSL.selectionPolicy: "extended_select"
*orderSL.extendedSelectionCallback.source: public
*orderSL.extendedSelectionCallback: assign_datasets_assigned_listCb
*orderSL.defaultActionCallbackClientData: (XtPointer) 0
*orderSL.extendedSelectionCallbackClientData: (XtPointer) 1

*scrolledWindowList5.class: scrolledWindow
*scrolledWindowList5.static: true
*scrolledWindowList5.name: scrolledWindowList5
*scrolledWindowList5.parent: form1
*scrolledWindowList5.scrollingPolicy: "automatic"
*scrolledWindowList5.visualPolicy: "constant"
*scrolledWindowList5.scrollBarDisplayPolicy: "as_needed"
*scrolledWindowList5.shadowThickness: 1
*scrolledWindowList5.isCompound: "true"
*scrolledWindowList5.compoundIcon: "scrllist.xpm"
*scrolledWindowList5.compoundName: "scrolled_List"
*scrolledWindowList5.x: 372
*scrolledWindowList5.y: 40
*scrolledWindowList5.width: 39
*scrolledWindowList5.height: 373
*scrolledWindowList5.background: "#9ac0cd"
*scrolledWindowList5.borderWidth: 0

*addSL.class: scrolledList
*addSL.static: true
*addSL.name: addSL
*addSL.parent: scrolledWindowList5
*addSL.width: 36
*addSL.height: 371
*addSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*addSL.background: "LightSkyBlue3"
*addSL.listSizePolicy: "constant"
*addSL.itemCount: 0
*addSL.selectionPolicy: "extended_select"
*addSL.extendedSelectionCallback.source: public
*addSL.extendedSelectionCallback: assign_datasets_assigned_listCb
*addSL.defaultActionCallbackClientData: (XtPointer) 0
*addSL.extendedSelectionCallbackClientData: (XtPointer) 2

*scrolledWindowList6.class: scrolledWindow
*scrolledWindowList6.static: true
*scrolledWindowList6.name: scrolledWindowList6
*scrolledWindowList6.parent: form1
*scrolledWindowList6.scrollingPolicy: "automatic"
*scrolledWindowList6.visualPolicy: "constant"
*scrolledWindowList6.scrollBarDisplayPolicy: "as_needed"
*scrolledWindowList6.shadowThickness: 1
*scrolledWindowList6.isCompound: "true"
*scrolledWindowList6.compoundIcon: "scrllist.xpm"
*scrolledWindowList6.compoundName: "scrolled_List"
*scrolledWindowList6.x: 422
*scrolledWindowList6.y: 40
*scrolledWindowList6.width: 39
*scrolledWindowList6.height: 373
*scrolledWindowList6.background: "#9ac0cd"
*scrolledWindowList6.borderWidth: 0

*getSL.class: scrolledList
*getSL.static: true
*getSL.name: getSL
*getSL.parent: scrolledWindowList6
*getSL.width: 36
*getSL.height: 371
*getSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*getSL.background: "LightSkyBlue3"
*getSL.listSizePolicy: "constant"
*getSL.itemCount: 0
*getSL.selectionPolicy: "extended_select"
*getSL.extendedSelectionCallback.source: public
*getSL.extendedSelectionCallback: assign_datasets_assigned_listCb
*getSL.defaultActionCallbackClientData: (XtPointer) 0
*getSL.extendedSelectionCallbackClientData: (XtPointer) 3

*scrolledWindowList7.class: scrolledWindow
*scrolledWindowList7.static: true
*scrolledWindowList7.name: scrolledWindowList7
*scrolledWindowList7.parent: form1
*scrolledWindowList7.scrollingPolicy: "automatic"
*scrolledWindowList7.visualPolicy: "constant"
*scrolledWindowList7.scrollBarDisplayPolicy: "as_needed"
*scrolledWindowList7.shadowThickness: 1
*scrolledWindowList7.isCompound: "true"
*scrolledWindowList7.compoundIcon: "scrllist.xpm"
*scrolledWindowList7.compoundName: "scrolled_List"
*scrolledWindowList7.x: 472
*scrolledWindowList7.y: 40
*scrolledWindowList7.width: 39
*scrolledWindowList7.height: 373
*scrolledWindowList7.background: "#9ac0cd"
*scrolledWindowList7.borderWidth: 0

*deleteSL.class: scrolledList
*deleteSL.static: true
*deleteSL.name: deleteSL
*deleteSL.parent: scrolledWindowList7
*deleteSL.width: 36
*deleteSL.height: 371
*deleteSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*deleteSL.background: "LightSkyBlue3"
*deleteSL.listSizePolicy: "constant"
*deleteSL.itemCount: 0
*deleteSL.selectionPolicy: "extended_select"
*deleteSL.extendedSelectionCallback.source: public
*deleteSL.extendedSelectionCallback: assign_datasets_assigned_listCb
*deleteSL.defaultActionCallbackClientData: (XtPointer) 0
*deleteSL.extendedSelectionCallbackClientData: (XtPointer) 4

*replaceSW.class: scrolledWindow
*replaceSW.static: true
*replaceSW.name: replaceSW
*replaceSW.parent: form1
*replaceSW.scrollingPolicy: "automatic"
*replaceSW.visualPolicy: "constant"
*replaceSW.scrollBarDisplayPolicy: "as_needed"
*replaceSW.shadowThickness: 1
*replaceSW.isCompound: "true"
*replaceSW.compoundIcon: "scrllist.xpm"
*replaceSW.compoundName: "scrolled_List"
*replaceSW.x: 522
*replaceSW.y: 40
*replaceSW.width: 39
*replaceSW.height: 373
*replaceSW.background: "#9ac0cd"
*replaceSW.borderWidth: 0

*replaceSL.class: scrolledList
*replaceSL.static: true
*replaceSL.name: replaceSL
*replaceSL.parent: replaceSW
*replaceSL.width: 36
*replaceSL.height: 371
*replaceSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*replaceSL.background: "LightSkyBlue3"
*replaceSL.listSizePolicy: "constant"
*replaceSL.itemCount: 0
*replaceSL.selectionPolicy: "extended_select"
*replaceSL.extendedSelectionCallback.source: public
*replaceSL.extendedSelectionCallback: assign_datasets_assigned_listCb
*replaceSL.defaultActionCallbackClientData: (XtPointer) 0
*replaceSL.extendedSelectionCallbackClientData: (XtPointer) 5

*guide_sbSW.class: scrolledWindow
*guide_sbSW.static: true
*guide_sbSW.name: guide_sbSW
*guide_sbSW.parent: form1
*guide_sbSW.scrollingPolicy: "application_defined"
*guide_sbSW.visualPolicy: "variable"
*guide_sbSW.scrollBarDisplayPolicy: "static"
*guide_sbSW.shadowThickness: 1
*guide_sbSW.isCompound: "true"
*guide_sbSW.compoundIcon: "scrllist.xpm"
*guide_sbSW.compoundName: "scrolled_List"
*guide_sbSW.x: 570
*guide_sbSW.y: 40
*guide_sbSW.width: 15
*guide_sbSW.height: 373
*guide_sbSW.background: "#9ac0cd"
*guide_sbSW.borderWidth: 0

*guide_sbSL.class: scrolledList
*guide_sbSL.static: true
*guide_sbSL.name: guide_sbSL
*guide_sbSL.parent: guide_sbSW
*guide_sbSL.width: 2
*guide_sbSL.height: 378
*guide_sbSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*guide_sbSL.background: "LightSkyBlue3"
*guide_sbSL.listSizePolicy: "variable"
*guide_sbSL.itemCount: 0
*guide_sbSL.selectionPolicy: "extended_select"
*guide_sbSL.scrollBarDisplayPolicy: "static"
*guide_sbSL.createCallback.source: public
*guide_sbSL.createCallback: assign_datasets_assigned_sbCb
*guide_sbSL.createCallbackClientData: (XtPointer) 1

*label63.class: label
*label63.static: true
*label63.name: label63
*label63.parent: form1
*label63.isCompound: "true"
*label63.compoundIcon: "label.xpm"
*label63.compoundName: "label_"
*label63.x: 332
*label63.y: 10
*label63.background: "#9ac0cd"
*label63.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label63.labelString: "O"

*label64.class: label
*label64.static: true
*label64.name: label64
*label64.parent: form1
*label64.isCompound: "true"
*label64.compoundIcon: "label.xpm"
*label64.compoundName: "label_"
*label64.x: 384
*label64.y: 10
*label64.background: "#9ac0cd"
*label64.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label64.labelString: "A"

*label65.class: label
*label65.static: true
*label65.name: label65
*label65.parent: form1
*label65.isCompound: "true"
*label65.compoundIcon: "label.xpm"
*label65.compoundName: "label_"
*label65.x: 432
*label65.y: 10
*label65.background: "#9ac0cd"
*label65.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label65.labelString: "G"

*label66.class: label
*label66.static: true
*label66.name: label66
*label66.parent: form1
*label66.isCompound: "true"
*label66.compoundIcon: "label.xpm"
*label66.compoundName: "label_"
*label66.x: 480
*label66.y: 10
*label66.background: "#9ac0cd"
*label66.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label66.labelString: "D"

*label67.class: label
*label67.static: true
*label67.name: label67
*label67.parent: form1
*label67.isCompound: "true"
*label67.compoundIcon: "label.xpm"
*label67.compoundName: "label_"
*label67.x: 532
*label67.y: 10
*label67.background: "#9ac0cd"
*label67.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label67.labelString: "R"

*deletePB.class: pushButton
*deletePB.static: true
*deletePB.name: deletePB
*deletePB.parent: form1
*deletePB.isCompound: "true"
*deletePB.compoundIcon: "push.xpm"
*deletePB.compoundName: "push_Button"
*deletePB.x: 104
*deletePB.y: 456
*deletePB.width: 138
*deletePB.height: 34
*deletePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*deletePB.labelString: "Delete"
*deletePB.shadowThickness: 4
*deletePB.background: "CadetBlue"
*deletePB.activateCallback.source: public
*deletePB.activateCallback: assign_datasets_delete_datasetsCb

*label75.class: label
*label75.static: true
*label75.name: label75
*label75.parent: form1
*label75.isCompound: "true"
*label75.compoundIcon: "label.xpm"
*label75.compoundName: "label_"
*label75.x: 32
*label75.y: 12
*label75.background: "#9ac0cd"
*label75.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label75.labelString: "DataSet,"

*label76.class: label
*label76.static: true
*label76.name: label76
*label76.parent: form1
*label76.isCompound: "true"
*label76.compoundIcon: "label.xpm"
*label76.compoundName: "label_"
*label76.x: 140
*label76.y: 12
*label76.background: "#9ac0cd"
*label76.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label76.labelString: "Platform,"

*label77.class: label
*label77.static: true
*label77.name: label77
*label77.parent: form1
*label77.isCompound: "true"
*label77.compoundIcon: "label.xpm"
*label77.compoundName: "label_"
*label77.x: 246
*label77.y: 12
*label77.background: "#9ac0cd"
*label77.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label77.labelString: "Sensor"

*frame7.class: frame
*frame7.static: true
*frame7.name: frame7
*frame7.parent: form1
*frame7.width: 266
*frame7.height: 62
*frame7.isCompound: "true"
*frame7.compoundIcon: "frame.xpm"
*frame7.compoundName: "frame_"
*frame7.x: 322
*frame7.y: 434
*frame7.background: "#9ac0cd"

*form7.class: form
*form7.static: true
*form7.name: form7
*form7.parent: frame7
*form7.width: 200
*form7.height: 200
*form7.resizePolicy: "resize_none"
*form7.isCompound: "true"
*form7.compoundIcon: "form.xpm"
*form7.compoundName: "form_"
*form7.x: 0
*form7.y: -2
*form7.background: "#9ac0cd"

*label68.class: label
*label68.static: true
*label68.name: label68
*label68.parent: form7
*label68.isCompound: "true"
*label68.compoundIcon: "label.xpm"
*label68.compoundName: "label_"
*label68.x: 6
*label68.y: 6
*label68.background: "#9ac0cd"
*label68.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label68.labelString: "O: Order"

*label69.class: label
*label69.static: true
*label69.name: label69
*label69.parent: form7
*label69.isCompound: "true"
*label69.compoundIcon: "label.xpm"
*label69.compoundName: "label_"
*label69.x: 106
*label69.y: 6
*label69.background: "#9ac0cd"
*label69.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label69.labelString: "A: Add"

*label70.class: label
*label70.static: true
*label70.name: label70
*label70.parent: form7
*label70.isCompound: "true"
*label70.compoundIcon: "label.xpm"
*label70.compoundName: "label_"
*label70.x: 196
*label70.y: 6
*label70.background: "#9ac0cd"
*label70.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label70.labelString: "G: Get"

*label71.class: label
*label71.static: true
*label71.name: label71
*label71.parent: form7
*label71.isCompound: "true"
*label71.compoundIcon: "label.xpm"
*label71.compoundName: "label_"
*label71.x: 0
*label71.y: 32
*label71.background: "#9ac0cd"
*label71.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label71.labelString: "D: Delete"
*label71.width: 92

*label72.class: label
*label72.static: true
*label72.name: label72
*label72.parent: form7
*label72.isCompound: "true"
*label72.compoundIcon: "label.xpm"
*label72.compoundName: "label_"
*label72.x: 106
*label72.y: 32
*label72.background: "#9ac0cd"
*label72.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label72.labelString: "R: Replace"

*closeButton.class: pushButton
*closeButton.static: true
*closeButton.name: closeButton
*closeButton.parent: assign_datasets
*closeButton.isCompound: "true"
*closeButton.compoundIcon: "push.xpm"
*closeButton.compoundName: "push_Button"
*closeButton.x: 870
*closeButton.y: 728
*closeButton.width: 138
*closeButton.height: 34
*closeButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closeButton.labelString: "Cancel"
*closeButton.shadowThickness: 4
*closeButton.background: "CadetBlue"
*closeButton.activateCallback.source: public
*closeButton.activateCallback: assign_datasets_closeCb

*frame4.class: frame
*frame4.static: true
*frame4.name: frame4
*frame4.parent: assign_datasets
*frame4.width: 988
*frame4.height: 104
*frame4.isCompound: "true"
*frame4.compoundIcon: "frame.xpm"
*frame4.compoundName: "frame_"
*frame4.x: 24
*frame4.y: 58
*frame4.background: "#9ac0cd"
*frame4.shadowThickness: 3

*form6.class: form
*form6.static: true
*form6.name: form6
*form6.parent: frame4
*form6.width: 200
*form6.height: 200
*form6.resizePolicy: "resize_none"
*form6.isCompound: "true"
*form6.compoundIcon: "form.xpm"
*form6.compoundName: "form_"
*form6.x: 0
*form6.y: 0
*form6.background: "#9ac0cd"

*label44.class: label
*label44.static: true
*label44.name: label44
*label44.parent: form6
*label44.isCompound: "true"
*label44.compoundIcon: "label.xpm"
*label44.compoundName: "label_"
*label44.x: 78
*label44.y: 18
*label44.background: "#9ac0cd"
*label44.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label44.labelString: "Account ID:"

*account_idTF.class: textField
*account_idTF.static: true
*account_idTF.name: account_idTF
*account_idTF.parent: form6
*account_idTF.width: 220
*account_idTF.isCompound: "true"
*account_idTF.compoundIcon: "textfield.xpm"
*account_idTF.compoundName: "text_Field"
*account_idTF.x: 192
*account_idTF.y: 12
*account_idTF.background: "LightSkyBlue3"
*account_idTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*account_idTF.maxLength: 20
*account_idTF.height: 35
*account_idTF.editable: "false"
*account_idTF.cursorPositionVisible: "false"

*label56.class: label
*label56.static: true
*label56.name: label56
*label56.parent: form6
*label56.isCompound: "true"
*label56.compoundIcon: "label.xpm"
*label56.compoundName: "label_"
*label56.x: 532
*label56.y: 14
*label56.background: "#9ac0cd"
*label56.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label56.labelString: "Current Balance:"

*current_balanceTF.class: textField
*current_balanceTF.static: true
*current_balanceTF.name: current_balanceTF
*current_balanceTF.parent: form6
*current_balanceTF.width: 220
*current_balanceTF.isCompound: "true"
*current_balanceTF.compoundIcon: "textfield.xpm"
*current_balanceTF.compoundName: "text_Field"
*current_balanceTF.x: 684
*current_balanceTF.y: 12
*current_balanceTF.background: "LightSkyBlue3"
*current_balanceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*current_balanceTF.maxLength: 50
*current_balanceTF.columns: 50
*current_balanceTF.height: 35
*current_balanceTF.cursorPositionVisible: "false"
*current_balanceTF.editable: "false"

*label59.class: label
*label59.static: true
*label59.name: label59
*label59.parent: form6
*label59.isCompound: "true"
*label59.compoundIcon: "label.xpm"
*label59.compoundName: "label_"
*label59.x: 78
*label59.y: 56
*label59.background: "#9ac0cd"
*label59.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label59.labelString: "Creation:"

*creationTF.class: textField
*creationTF.static: true
*creationTF.name: creationTF
*creationTF.parent: form6
*creationTF.width: 220
*creationTF.isCompound: "true"
*creationTF.compoundIcon: "textfield.xpm"
*creationTF.compoundName: "text_Field"
*creationTF.x: 192
*creationTF.y: 52
*creationTF.background: "LightSkyBlue3"
*creationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*creationTF.maxLength: 50
*creationTF.columns: 50
*creationTF.height: 35
*creationTF.cursorPositionVisible: "false"
*creationTF.editable: "false"

*expirationTF.class: textField
*expirationTF.static: true
*expirationTF.name: expirationTF
*expirationTF.parent: form6
*expirationTF.width: 220
*expirationTF.isCompound: "true"
*expirationTF.compoundIcon: "textfield.xpm"
*expirationTF.compoundName: "text_Field"
*expirationTF.x: 684
*expirationTF.y: 52
*expirationTF.background: "LightSkyBlue3"
*expirationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*expirationTF.maxLength: 50
*expirationTF.columns: 50
*expirationTF.height: 35
*expirationTF.cursorPositionVisible: "false"
*expirationTF.editable: "false"

*label60.class: label
*label60.static: true
*label60.name: label60
*label60.parent: form6
*label60.isCompound: "true"
*label60.compoundIcon: "label.xpm"
*label60.compoundName: "label_"
*label60.x: 532
*label60.y: 56
*label60.background: "#9ac0cd"
*label60.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label60.labelString: "Expiration:"

*frame5.class: frame
*frame5.static: true
*frame5.name: frame5
*frame5.parent: assign_datasets
*frame5.width: 355
*frame5.height: 516
*frame5.isCompound: "true"
*frame5.compoundIcon: "frame.xpm"
*frame5.compoundName: "frame_"
*frame5.x: 25
*frame5.y: 204
*frame5.shadowThickness: 3
*frame5.background: "#9ac0cd"

*form4.class: form
*form4.static: true
*form4.name: form4
*form4.parent: frame5
*form4.width: 349
*form4.height: 510
*form4.resizePolicy: "resize_none"
*form4.isCompound: "true"
*form4.compoundIcon: "form.xpm"
*form4.compoundName: "form_"
*form4.x: 3
*form4.y: 3
*form4.background: "#9ac0cd"

*label57.class: label
*label57.static: true
*label57.name: label57
*label57.parent: form4
*label57.isCompound: "true"
*label57.compoundIcon: "label.xpm"
*label57.compoundName: "label_"
*label57.x: 26
*label57.y: 12
*label57.background: "#9ac0cd"
*label57.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label57.labelString: "DataSet,"

*assign_datasetsSW.class: scrolledWindow
*assign_datasetsSW.static: true
*assign_datasetsSW.name: assign_datasetsSW
*assign_datasetsSW.parent: form4
*assign_datasetsSW.scrollingPolicy: "automatic"
*assign_datasetsSW.visualPolicy: "constant"
*assign_datasetsSW.scrollBarDisplayPolicy: "as_needed"
*assign_datasetsSW.shadowThickness: 1
*assign_datasetsSW.isCompound: "true"
*assign_datasetsSW.compoundIcon: "scrllist.xpm"
*assign_datasetsSW.compoundName: "scrolled_List"
*assign_datasetsSW.x: 14
*assign_datasetsSW.y: 40
*assign_datasetsSW.width: 290
*assign_datasetsSW.height: 398
*assign_datasetsSW.background: "#9ac0cd"
*assign_datasetsSW.borderWidth: 0

*assign_datasetsSL.class: scrolledList
*assign_datasetsSL.static: true
*assign_datasetsSL.name: assign_datasetsSL
*assign_datasetsSL.parent: assign_datasetsSW
*assign_datasetsSL.width: 598
*assign_datasetsSL.height: 368
*assign_datasetsSL.background: "LightSkyBlue3"
*assign_datasetsSL.listSizePolicy: "variable"
*assign_datasetsSL.itemCount: 0
*assign_datasetsSL.selectionPolicy: "extended_select"
*assign_datasetsSL.defaultActionCallback.source: public
*assign_datasetsSL.defaultActionCallback: assign_datasets_datasets_listCb
*assign_datasetsSL.extendedSelectionCallback.source: public
*assign_datasetsSL.extendedSelectionCallback: assign_datasets_datasets_listCb
*assign_datasetsSL.doubleClickInterval: 200
*assign_datasetsSL.scrollBarDisplayPolicy: "as_needed"
*assign_datasetsSL.createCallbackClientData: (XtPointer) 0x0
*assign_datasetsSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"

*assign_datasets_sbSW.class: scrolledWindow
*assign_datasets_sbSW.static: true
*assign_datasets_sbSW.name: assign_datasets_sbSW
*assign_datasets_sbSW.parent: form4
*assign_datasets_sbSW.scrollingPolicy: "application_defined"
*assign_datasets_sbSW.visualPolicy: "variable"
*assign_datasets_sbSW.scrollBarDisplayPolicy: "static"
*assign_datasets_sbSW.shadowThickness: 1
*assign_datasets_sbSW.isCompound: "true"
*assign_datasets_sbSW.compoundIcon: "scrllist.xpm"
*assign_datasets_sbSW.compoundName: "scrolled_List"
*assign_datasets_sbSW.x: 314
*assign_datasets_sbSW.y: 40
*assign_datasets_sbSW.width: 15
*assign_datasets_sbSW.height: 373
*assign_datasets_sbSW.background: "#9ac0cd"
*assign_datasets_sbSW.borderWidth: 0

*assign_datasets_sbSL.class: scrolledList
*assign_datasets_sbSL.static: true
*assign_datasets_sbSL.name: assign_datasets_sbSL
*assign_datasets_sbSL.parent: assign_datasets_sbSW
*assign_datasets_sbSL.width: 2
*assign_datasets_sbSL.height: 378
*assign_datasets_sbSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*assign_datasets_sbSL.background: "LightSkyBlue3"
*assign_datasets_sbSL.listSizePolicy: "variable"
*assign_datasets_sbSL.itemCount: 0
*assign_datasets_sbSL.selectionPolicy: "extended_select"
*assign_datasets_sbSL.scrollBarDisplayPolicy: "static"
*assign_datasets_sbSL.createCallback.source: public
*assign_datasets_sbSL.createCallback: assign_datasets_datasets_sbCb
*assign_datasets_sbSL.createCallbackClientData: (XtPointer) 1

*addPB.class: pushButton
*addPB.static: true
*addPB.name: addPB
*addPB.parent: form4
*addPB.isCompound: "true"
*addPB.compoundIcon: "push.xpm"
*addPB.compoundName: "push_Button"
*addPB.x: 96
*addPB.y: 456
*addPB.width: 138
*addPB.height: 34
*addPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*addPB.labelString: "Add"
*addPB.shadowThickness: 4
*addPB.background: "CadetBlue"
*addPB.activateCallback.source: public
*addPB.activateCallback: assign_datasets_add_datasetsCb

*label73.class: label
*label73.static: true
*label73.name: label73
*label73.parent: form4
*label73.isCompound: "true"
*label73.compoundIcon: "label.xpm"
*label73.compoundName: "label_"
*label73.x: 132
*label73.y: 12
*label73.background: "#9ac0cd"
*label73.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label73.labelString: "Platform,"

*label74.class: label
*label74.static: true
*label74.name: label74
*label74.parent: form4
*label74.isCompound: "true"
*label74.compoundIcon: "label.xpm"
*label74.compoundName: "label_"
*label74.x: 242
*label74.y: 12
*label74.background: "#9ac0cd"
*label74.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label74.labelString: "Sensor"

*label61.class: label
*label61.static: true
*label61.name: label61
*label61.parent: assign_datasets
*label61.isCompound: "true"
*label61.compoundIcon: "label.xpm"
*label61.compoundName: "label_"
*label61.x: 158
*label61.y: 176
*label61.background: "#9ac0cd"
*label61.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label61.labelString: "Datasets"

*label62.class: label
*label62.static: true
*label62.name: label62
*label62.parent: assign_datasets
*label62.isCompound: "true"
*label62.compoundIcon: "label.xpm"
*label62.compoundName: "label_"
*label62.x: 584
*label62.y: 176
*label62.background: "#9ac0cd"
*label62.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label62.labelString: "Datasets Assigned to Account"

*updateButton.class: pushButton
*updateButton.static: true
*updateButton.name: updateButton
*updateButton.parent: assign_datasets
*updateButton.isCompound: "true"
*updateButton.compoundIcon: "push.xpm"
*updateButton.compoundName: "push_Button"
*updateButton.x: 26
*updateButton.y: 728
*updateButton.width: 138
*updateButton.height: 34
*updateButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*updateButton.labelString: "Update"
*updateButton.shadowThickness: 4
*updateButton.background: "CadetBlue"
*updateButton.activateCallback.source: public
*updateButton.activateCallback: assign_datasets_updateCb

