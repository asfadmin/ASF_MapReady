! UIMX ascii 2.9 key: 7520                                                      

*photoOrder.class: form
*photoOrder.classinc:
*photoOrder.classspec:
*photoOrder.classmembers:
*photoOrder.classconstructor:
*photoOrder.classdestructor:
*photoOrder.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*photoOrder.ispecdecl:
*photoOrder.funcdecl: swidget create_photoOrder(swidget UxParent)
*photoOrder.funcname: create_photoOrder
*photoOrder.funcdef: "swidget", "<create_photoOrder>(%)"
*photoOrder.argdecl: swidget UxParent;
*photoOrder.arglist: UxParent
*photoOrder.arglist.UxParent: "swidget", "%UxParent%"
*photoOrder.icode:
*photoOrder.fcode: return(rtrn);\

*photoOrder.auxdecl:
*photoOrder.static: true
*photoOrder.name: photoOrder
*photoOrder.parent: NO_PARENT
*photoOrder.parentExpression: UxParent
*photoOrder.defaultShell: transientShell
*photoOrder.width: 1040
*photoOrder.height: 790
*photoOrder.resizePolicy: "resize_none"
*photoOrder.isCompound: "true"
*photoOrder.compoundIcon: "form.xpm"
*photoOrder.compoundName: "form_"
*photoOrder.x: 50
*photoOrder.y: 20
*photoOrder.unitType: "pixels"
*photoOrder.background: "#9ac0cd"
*photoOrder.allowShellResize: "false"

*label20.class: label
*label20.static: true
*label20.name: label20
*label20.parent: photoOrder
*label20.isCompound: "true"
*label20.compoundIcon: "label.xpm"
*label20.compoundName: "label_"
*label20.x: 368
*label20.y: 36
*label20.background: "#9ac0cd"
*label20.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label20.labelString: "Create   Photo   Jobs   Screen"
*label20.height: 32

*frame2.class: frame
*frame2.static: true
*frame2.name: frame2
*frame2.parent: photoOrder
*frame2.width: 490
*frame2.height: 640
*frame2.isCompound: "true"
*frame2.compoundIcon: "frame.xpm"
*frame2.compoundName: "frame_"
*frame2.x: 8
*frame2.y: 74
*frame2.background: "#9ac0cd"
*frame2.shadowThickness: 4
*frame2.shadowType: "shadow_etched_out"

*form3.class: form
*form3.static: true
*form3.name: form3
*form3.parent: frame2
*form3.width: 450
*form3.height: 720
*form3.resizePolicy: "resize_none"
*form3.isCompound: "true"
*form3.compoundIcon: "form.xpm"
*form3.compoundName: "form_"
*form3.x: 336
*form3.y: 140
*form3.background: "#9ac0cd"

*label22.class: label
*label22.static: true
*label22.name: label22
*label22.parent: form3
*label22.isCompound: "true"
*label22.compoundIcon: "label.xpm"
*label22.compoundName: "label_"
*label22.x: 132
*label22.y: 112
*label22.background: "#9ac0cd"
*label22.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label22.labelString: "Order ID"
*label22.height: 24

*orderIdSW1.class: scrolledWindow
*orderIdSW1.static: true
*orderIdSW1.name: orderIdSW1
*orderIdSW1.parent: form3
*orderIdSW1.scrollingPolicy: "automatic"
*orderIdSW1.visualPolicy: "constant"
*orderIdSW1.scrollBarDisplayPolicy: "as_needed"
*orderIdSW1.shadowThickness: 0
*orderIdSW1.isCompound: "true"
*orderIdSW1.compoundIcon: "scrllist.xpm"
*orderIdSW1.compoundName: "scrolled_List"
*orderIdSW1.x: 124
*orderIdSW1.y: 136
*orderIdSW1.background: "LightSkyBlue3"
*orderIdSW1.height: 440
*orderIdSW1.width: 108

*orderIdSL1.class: scrolledList
*orderIdSL1.static: true
*orderIdSL1.name: orderIdSL1
*orderIdSL1.parent: orderIdSW1
*orderIdSL1.width: 108
*orderIdSL1.height: 440
*orderIdSL1.background: "LightSkyBlue3"
*orderIdSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderIdSL1.listSizePolicy: "constant"
*orderIdSL1.selectionPolicy: "extended_select"
*orderIdSL1.x: 0
*orderIdSL1.y: 136
*orderIdSL1.extendedSelectionCallback.source: public
*orderIdSL1.extendedSelectionCallback: photoOrder_queueLists_selectionCb
*orderIdSL1.extendedSelectionCallbackClientData: (XtPointer) 2
*orderIdSL1.listSpacing: 1
*orderIdSL1.visibleItemCount: 24

*label28.class: label
*label28.static: true
*label28.name: label28
*label28.parent: form3
*label28.isCompound: "true"
*label28.compoundIcon: "label.xpm"
*label28.compoundName: "label_"
*label28.x: 232
*label28.y: 114
*label28.background: "#9ac0cd"
*label28.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label28.labelString: "Item"

*itemIdSW1.class: scrolledWindow
*itemIdSW1.static: true
*itemIdSW1.name: itemIdSW1
*itemIdSW1.parent: form3
*itemIdSW1.scrollingPolicy: "automatic"
*itemIdSW1.visualPolicy: "constant"
*itemIdSW1.scrollBarDisplayPolicy: "as_needed"
*itemIdSW1.shadowThickness: 0
*itemIdSW1.isCompound: "true"
*itemIdSW1.compoundIcon: "scrllist.xpm"
*itemIdSW1.compoundName: "scrolled_List"
*itemIdSW1.x: 236
*itemIdSW1.y: 136
*itemIdSW1.background: "LightSkyBlue3"
*itemIdSW1.height: 440
*itemIdSW1.width: 37

*itemIdSL1.class: scrolledList
*itemIdSL1.static: true
*itemIdSL1.name: itemIdSL1
*itemIdSL1.parent: itemIdSW1
*itemIdSL1.width: 37
*itemIdSL1.height: 440
*itemIdSL1.background: "LightSkyBlue3"
*itemIdSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemIdSL1.listSizePolicy: "constant"
*itemIdSL1.selectionPolicy: "extended_select"
*itemIdSL1.x: 0
*itemIdSL1.y: 136
*itemIdSL1.extendedSelectionCallback.source: public
*itemIdSL1.extendedSelectionCallback: photoOrder_queueLists_selectionCb
*itemIdSL1.extendedSelectionCallbackClientData: (XtPointer) 3
*itemIdSL1.listSpacing: 1
*itemIdSL1.visibleItemCount: 24

*label29.class: label
*label29.static: true
*label29.name: label29
*label29.parent: form3
*label29.isCompound: "true"
*label29.compoundIcon: "label.xpm"
*label29.compoundName: "label_"
*label29.x: 286
*label29.y: 114
*label29.background: "#9ac0cd"
*label29.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label29.labelString: "Product ID"

*productIdSW1.class: scrolledWindow
*productIdSW1.static: true
*productIdSW1.name: productIdSW1
*productIdSW1.parent: form3
*productIdSW1.scrollingPolicy: "automatic"
*productIdSW1.visualPolicy: "constant"
*productIdSW1.scrollBarDisplayPolicy: "as_needed"
*productIdSW1.shadowThickness: 0
*productIdSW1.isCompound: "true"
*productIdSW1.compoundIcon: "scrllist.xpm"
*productIdSW1.compoundName: "scrolled_List"
*productIdSW1.x: 276
*productIdSW1.y: 136
*productIdSW1.background: "LightSkyBlue3"
*productIdSW1.height: 440
*productIdSW1.width: 135

*productIdSL1.class: scrolledList
*productIdSL1.static: true
*productIdSL1.name: productIdSL1
*productIdSL1.parent: productIdSW1
*productIdSL1.width: 135
*productIdSL1.height: 440
*productIdSL1.background: "LightSkyBlue3"
*productIdSL1.listSizePolicy: "constant"
*productIdSL1.selectionPolicy: "extended_select"
*productIdSL1.x: 0
*productIdSL1.y: 136
*productIdSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*productIdSL1.extendedSelectionCallback.source: public
*productIdSL1.extendedSelectionCallback: photoOrder_queueLists_selectionCb
*productIdSL1.extendedSelectionCallbackClientData: (XtPointer) 4
*productIdSL1.listSpacing: 1
*productIdSL1.visibleItemCount: 24
*productIdSL1.itemCount: 0

*label30.class: label
*label30.static: true
*label30.name: label30
*label30.parent: form3
*label30.isCompound: "true"
*label30.compoundIcon: "label.xpm"
*label30.compoundName: "label_"
*label30.x: 422
*label30.y: 114
*label30.background: "#9ac0cd"
*label30.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label30.labelString: "Qty"

*qtySW1.class: scrolledWindow
*qtySW1.static: true
*qtySW1.name: qtySW1
*qtySW1.parent: form3
*qtySW1.scrollingPolicy: "automatic"
*qtySW1.visualPolicy: "constant"
*qtySW1.scrollBarDisplayPolicy: "as_needed"
*qtySW1.shadowThickness: 0
*qtySW1.isCompound: "true"
*qtySW1.compoundIcon: "scrllist.xpm"
*qtySW1.compoundName: "scrolled_List"
*qtySW1.x: 416
*qtySW1.y: 136
*qtySW1.background: "LightSkyBlue3"
*qtySW1.height: 440
*qtySW1.width: 36

*qtySL1.class: scrolledList
*qtySL1.static: true
*qtySL1.name: qtySL1
*qtySL1.parent: qtySW1
*qtySL1.width: 36
*qtySL1.height: 440
*qtySL1.background: "LightSkyBlue3"
*qtySL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*qtySL1.listSizePolicy: "constant"
*qtySL1.selectionPolicy: "extended_select"
*qtySL1.x: 0
*qtySL1.y: 136
*qtySL1.extendedSelectionCallback.source: public
*qtySL1.extendedSelectionCallback: photoOrder_queueLists_selectionCb
*qtySL1.extendedSelectionCallbackClientData: (XtPointer) 5
*qtySL1.listSpacing: 1
*qtySL1.visibleItemCount: 24

*dummySW1.class: scrolledWindow
*dummySW1.static: true
*dummySW1.name: dummySW1
*dummySW1.parent: form3
*dummySW1.scrollingPolicy: "application_defined"
*dummySW1.visualPolicy: "variable"
*dummySW1.scrollBarDisplayPolicy: "static"
*dummySW1.shadowThickness: 0
*dummySW1.isCompound: "true"
*dummySW1.compoundIcon: "scrllist.xpm"
*dummySW1.compoundName: "scrolled_List"
*dummySW1.x: 452
*dummySW1.y: 136
*dummySW1.width: 15
*dummySW1.background: "LightSkyBlue3"
*dummySW1.height: 435

*dummySL1.class: scrolledList
*dummySL1.static: true
*dummySL1.name: dummySL1
*dummySL1.parent: dummySW1
*dummySL1.width: 1
*dummySL1.height: 420
*dummySL1.background: "LightSkyBlue3"
*dummySL1.listSizePolicy: "variable"
*dummySL1.scrollBarDisplayPolicy: "static"
*dummySL1.selectionPolicy: "extended_select"
*dummySL1.x: 0
*dummySL1.y: 92
*dummySL1.mappedWhenManaged: "true"
*dummySL1.visibleItemCount: 24
*dummySL1.listSpacing: 1

*label44.class: label
*label44.static: true
*label44.name: label44
*label44.parent: form3
*label44.isCompound: "true"
*label44.compoundIcon: "label.xpm"
*label44.compoundName: "label_"
*label44.x: 56
*label44.y: 20
*label44.background: "#9ac0cd"
*label44.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label44.labelString: "Processing Type:"

*photoTypeTF.class: textField
*photoTypeTF.static: true
*photoTypeTF.name: photoTypeTF
*photoTypeTF.parent: form3
*photoTypeTF.width: 112
*photoTypeTF.isCompound: "true"
*photoTypeTF.compoundIcon: "textfield.xpm"
*photoTypeTF.compoundName: "text_Field"
*photoTypeTF.x: 196
*photoTypeTF.y: 12
*photoTypeTF.background: "LightSkyBlue3"
*photoTypeTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*photoTypePB.class: pushButton
*photoTypePB.static: true
*photoTypePB.name: photoTypePB
*photoTypePB.parent: form3
*photoTypePB.isCompound: "true"
*photoTypePB.compoundIcon: "push.xpm"
*photoTypePB.compoundName: "push_Button"
*photoTypePB.x: 308
*photoTypePB.y: 12
*photoTypePB.background: "LightSkyBlue3"
*photoTypePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*photoTypePB.labelString: "List..."
*photoTypePB.shadowThickness: 3
*photoTypePB.height: 36
*photoTypePB.width: 56
*photoTypePB.activateCallback.source: public
*photoTypePB.activateCallback: photoOrder_photoType_validsCb
*photoTypePB.labelType: "string"

*searchPB.class: pushButton
*searchPB.static: true
*searchPB.name: searchPB
*searchPB.parent: form3
*searchPB.isCompound: "true"
*searchPB.compoundIcon: "push.xpm"
*searchPB.compoundName: "push_Button"
*searchPB.x: 40
*searchPB.y: 588
*searchPB.background: "CadetBlue"
*searchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*searchPB.height: 36
*searchPB.shadowThickness: 4
*searchPB.labelString: "SEARCH"
*searchPB.width: 108
*searchPB.activateCallback.source: public
*searchPB.activateCallback: photoOrder_searchCb

*label63.class: label
*label63.static: true
*label63.name: label63
*label63.parent: form3
*label63.isCompound: "true"
*label63.compoundIcon: "label.xpm"
*label63.compoundName: "label_"
*label63.x: 20
*label63.y: 112
*label63.background: "#9ac0cd"
*label63.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label63.labelString: "User ID"
*label63.height: 24

*userIdSW1.class: scrolledWindow
*userIdSW1.static: true
*userIdSW1.name: userIdSW1
*userIdSW1.parent: form3
*userIdSW1.scrollingPolicy: "automatic"
*userIdSW1.visualPolicy: "constant"
*userIdSW1.scrollBarDisplayPolicy: "as_needed"
*userIdSW1.shadowThickness: 0
*userIdSW1.isCompound: "true"
*userIdSW1.compoundIcon: "scrllist.xpm"
*userIdSW1.compoundName: "scrolled_List"
*userIdSW1.x: 12
*userIdSW1.y: 136
*userIdSW1.background: "LightSkyBlue3"
*userIdSW1.height: 440
*userIdSW1.width: 110

*userIdSL1.class: scrolledList
*userIdSL1.static: true
*userIdSL1.name: userIdSL1
*userIdSL1.parent: userIdSW1
*userIdSL1.width: 110
*userIdSL1.height: 440
*userIdSL1.background: "LightSkyBlue3"
*userIdSL1.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*userIdSL1.listSizePolicy: "constant"
*userIdSL1.selectionPolicy: "extended_select"
*userIdSL1.x: 0
*userIdSL1.y: 136
*userIdSL1.listSpacing: 1
*userIdSL1.extendedSelectionCallback.source: public
*userIdSL1.extendedSelectionCallback: photoOrder_queueLists_selectionCb
*userIdSL1.extendedSelectionCallbackClientData: (XtPointer) 1
*userIdSL1.visibleItemCount: 24

*clearPB.class: pushButton
*clearPB.static: true
*clearPB.name: clearPB
*clearPB.parent: form3
*clearPB.isCompound: "true"
*clearPB.compoundIcon: "push.xpm"
*clearPB.compoundName: "push_Button"
*clearPB.x: 336
*clearPB.y: 588
*clearPB.background: "CadetBlue"
*clearPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*clearPB.height: 36
*clearPB.shadowThickness: 4
*clearPB.labelString: "CLEAR"
*clearPB.width: 108
*clearPB.sensitive: "true"
*clearPB.activateCallback.source: public
*clearPB.activateCallback: photoOrder_clearCb

*separator10.class: separator
*separator10.static: true
*separator10.name: separator10
*separator10.parent: form3
*separator10.width: 484
*separator10.height: 5
*separator10.isCompound: "true"
*separator10.compoundIcon: "sep.xpm"
*separator10.compoundName: "separator_"
*separator10.x: 0
*separator10.y: 52
*separator10.background: "#9ac0cd"

*label66.class: label
*label66.static: true
*label66.name: label66
*label66.parent: form3
*label66.isCompound: "true"
*label66.compoundIcon: "label.xpm"
*label66.compoundName: "label_"
*label66.x: 89
*label66.y: 76
*label66.background: "#9ac0cd"
*label66.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label66.labelString: "Total Items:"

*totalItemsTF.class: textField
*totalItemsTF.static: true
*totalItemsTF.name: totalItemsTF
*totalItemsTF.parent: form3
*totalItemsTF.width: 112
*totalItemsTF.isCompound: "true"
*totalItemsTF.compoundIcon: "textfield.xpm"
*totalItemsTF.compoundName: "text_Field"
*totalItemsTF.x: 196
*totalItemsTF.y: 68
*totalItemsTF.background: "LightSkyBlue3"
*totalItemsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*totalItemsTF.editable: "false"
*totalItemsTF.cursorPositionVisible: "false"

*frame3.class: frame
*frame3.static: true
*frame3.name: frame3
*frame3.parent: photoOrder
*frame3.width: 448
*frame3.height: 640
*frame3.isCompound: "true"
*frame3.compoundIcon: "frame.xpm"
*frame3.compoundName: "frame_"
*frame3.x: 580
*frame3.y: 74
*frame3.background: "#9ac0cd"
*frame3.shadowThickness: 4
*frame3.shadowType: "shadow_etched_out"

*form4.class: form
*form4.static: true
*form4.name: form4
*form4.parent: frame3
*form4.width: 200
*form4.height: 200
*form4.resizePolicy: "resize_none"
*form4.isCompound: "true"
*form4.compoundIcon: "form.xpm"
*form4.compoundName: "form_"
*form4.x: -88
*form4.y: 0
*form4.background: "#9ac0cd"

*label21.class: label
*label21.static: true
*label21.name: label21
*label21.parent: form4
*label21.isCompound: "true"
*label21.compoundIcon: "label.xpm"
*label21.compoundName: "label_"
*label21.x: 32
*label21.y: 148
*label21.background: "#9ac0cd"
*label21.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label21.labelString: "Order ID"

*orderIdSW2.class: scrolledWindow
*orderIdSW2.static: true
*orderIdSW2.name: orderIdSW2
*orderIdSW2.parent: form4
*orderIdSW2.scrollingPolicy: "automatic"
*orderIdSW2.visualPolicy: "constant"
*orderIdSW2.scrollBarDisplayPolicy: "as_needed"
*orderIdSW2.shadowThickness: 0
*orderIdSW2.isCompound: "true"
*orderIdSW2.compoundIcon: "scrllist.xpm"
*orderIdSW2.compoundName: "scrolled_List"
*orderIdSW2.x: 28
*orderIdSW2.y: 172
*orderIdSW2.background: "LightSkyBlue3"
*orderIdSW2.height: 406
*orderIdSW2.width: 140

*orderIdSL2.class: scrolledList
*orderIdSL2.static: true
*orderIdSL2.name: orderIdSL2
*orderIdSL2.parent: orderIdSW2
*orderIdSL2.width: 140
*orderIdSL2.height: 406
*orderIdSL2.background: "LightSkyBlue3"
*orderIdSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*orderIdSL2.listSizePolicy: "constant"
*orderIdSL2.selectionPolicy: "extended_select"
*orderIdSL2.x: 0
*orderIdSL2.y: 172
*orderIdSL2.visibleItemCount: 22
*orderIdSL2.itemCount: 0
*orderIdSL2.listSpacing: 1
*orderIdSL2.extendedSelectionCallbackClientData: (XtPointer) 1
*orderIdSL2.extendedSelectionCallback.source: public
*orderIdSL2.extendedSelectionCallback: photoOrder_jobLists_selectionCb

*label25.class: label
*label25.static: true
*label25.name: label25
*label25.parent: form4
*label25.isCompound: "true"
*label25.compoundIcon: "label.xpm"
*label25.compoundName: "label_"
*label25.x: 200
*label25.y: 60
*label25.background: "#9ac0cd"
*label25.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label25.labelString: "Order Date:"

*orderDateTF.class: textField
*orderDateTF.static: true
*orderDateTF.name: orderDateTF
*orderDateTF.parent: form4
*orderDateTF.width: 116
*orderDateTF.isCompound: "true"
*orderDateTF.compoundIcon: "textfield.xpm"
*orderDateTF.compoundName: "text_Field"
*orderDateTF.x: 312
*orderDateTF.y: 52
*orderDateTF.background: "LightSkyBlue3"
*orderDateTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*orderDateTF.editable: "false"
*orderDateTF.cursorPositionVisible: "false"
*orderDateTF.text: ""
*orderDateTF.marginWidth: 1

*label24.class: label
*label24.static: true
*label24.name: label24
*label24.parent: form4
*label24.isCompound: "true"
*label24.compoundIcon: "label.xpm"
*label24.compoundName: "label_"
*label24.x: 8
*label24.y: 60
*label24.background: "#9ac0cd"
*label24.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label24.labelString: "Work Order:"

*workOrderTF.class: textField
*workOrderTF.static: true
*workOrderTF.name: workOrderTF
*workOrderTF.parent: form4
*workOrderTF.width: 75
*workOrderTF.isCompound: "true"
*workOrderTF.compoundIcon: "textfield.xpm"
*workOrderTF.compoundName: "text_Field"
*workOrderTF.x: 116
*workOrderTF.y: 52
*workOrderTF.background: "LightSkyBlue3"
*workOrderTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*workOrderTF.editable: "false"
*workOrderTF.cursorPositionVisible: "false"

*label23.class: label
*label23.static: true
*label23.name: label23
*label23.parent: form4
*label23.isCompound: "true"
*label23.compoundIcon: "label.xpm"
*label23.compoundName: "label_"
*label23.x: 68
*label23.y: 16
*label23.background: "#9ac0cd"
*label23.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label23.labelString: "Photo Job ID:"

*jobIdTF.class: textField
*jobIdTF.static: true
*jobIdTF.name: jobIdTF
*jobIdTF.parent: form4
*jobIdTF.width: 152
*jobIdTF.isCompound: "true"
*jobIdTF.compoundIcon: "textfield.xpm"
*jobIdTF.compoundName: "text_Field"
*jobIdTF.x: 196
*jobIdTF.y: 8
*jobIdTF.background: "LightSkyBlue3"
*jobIdTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*jobIdTF.editable: "false"
*jobIdTF.cursorPositionVisible: "false"

*label31.class: label
*label31.static: true
*label31.name: label31
*label31.parent: form4
*label31.isCompound: "true"
*label31.compoundIcon: "label.xpm"
*label31.compoundName: "label_"
*label31.x: 8
*label31.y: 100
*label31.background: "#9ac0cd"
*label31.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label31.labelString: "Total Prints:"

*label32.class: label
*label32.static: true
*label32.name: label32
*label32.parent: form4
*label32.isCompound: "true"
*label32.compoundIcon: "label.xpm"
*label32.compoundName: "label_"
*label32.x: 200
*label32.y: 100
*label32.background: "#9ac0cd"
*label32.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label32.labelString: "Total Cost($):"

*totalPrintsTF.class: textField
*totalPrintsTF.static: true
*totalPrintsTF.name: totalPrintsTF
*totalPrintsTF.parent: form4
*totalPrintsTF.width: 75
*totalPrintsTF.isCompound: "true"
*totalPrintsTF.compoundIcon: "textfield.xpm"
*totalPrintsTF.compoundName: "text_Field"
*totalPrintsTF.x: 116
*totalPrintsTF.y: 92
*totalPrintsTF.background: "LightSkyBlue3"
*totalPrintsTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*totalPrintsTF.editable: "false"
*totalPrintsTF.cursorPositionVisible: "false"

*totalCostTF.class: textField
*totalCostTF.static: true
*totalCostTF.name: totalCostTF
*totalCostTF.parent: form4
*totalCostTF.width: 116
*totalCostTF.isCompound: "true"
*totalCostTF.compoundIcon: "textfield.xpm"
*totalCostTF.compoundName: "text_Field"
*totalCostTF.x: 312
*totalCostTF.y: 92
*totalCostTF.background: "LightSkyBlue3"
*totalCostTF.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*totalCostTF.editable: "false"
*totalCostTF.cursorPositionVisible: "false"

*label45.class: label
*label45.static: true
*label45.name: label45
*label45.parent: form4
*label45.isCompound: "true"
*label45.compoundIcon: "label.xpm"
*label45.compoundName: "label_"
*label45.x: 168
*label45.y: 148
*label45.background: "#9ac0cd"
*label45.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label45.labelString: "Item"

*label46.class: label
*label46.static: true
*label46.name: label46
*label46.parent: form4
*label46.isCompound: "true"
*label46.compoundIcon: "label.xpm"
*label46.compoundName: "label_"
*label46.x: 224
*label46.y: 148
*label46.background: "#9ac0cd"
*label46.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label46.labelString: "Product ID"

*label47.class: label
*label47.static: true
*label47.name: label47
*label47.parent: form4
*label47.isCompound: "true"
*label47.compoundIcon: "label.xpm"
*label47.compoundName: "label_"
*label47.x: 360
*label47.y: 148
*label47.background: "#9ac0cd"
*label47.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label47.labelString: "Qty"
*label47.width: 32

*itemIdSW2.class: scrolledWindow
*itemIdSW2.static: true
*itemIdSW2.name: itemIdSW2
*itemIdSW2.parent: form4
*itemIdSW2.scrollingPolicy: "automatic"
*itemIdSW2.visualPolicy: "constant"
*itemIdSW2.scrollBarDisplayPolicy: "as_needed"
*itemIdSW2.shadowThickness: 0
*itemIdSW2.isCompound: "true"
*itemIdSW2.compoundIcon: "scrllist.xpm"
*itemIdSW2.compoundName: "scrolled_List"
*itemIdSW2.x: 172
*itemIdSW2.y: 172
*itemIdSW2.background: "LightSkyBlue3"
*itemIdSW2.height: 406
*itemIdSW2.width: 40

*itemIdSL2.class: scrolledList
*itemIdSL2.static: true
*itemIdSL2.name: itemIdSL2
*itemIdSL2.parent: itemIdSW2
*itemIdSL2.width: 40
*itemIdSL2.height: 406
*itemIdSL2.background: "LightSkyBlue3"
*itemIdSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*itemIdSL2.listSizePolicy: "constant"
*itemIdSL2.selectionPolicy: "extended_select"
*itemIdSL2.x: 0
*itemIdSL2.y: 172
*itemIdSL2.visibleItemCount: 22
*itemIdSL2.extendedSelectionCallbackClientData: (XtPointer) 2
*itemIdSL2.extendedSelectionCallback.source: public
*itemIdSL2.extendedSelectionCallback: photoOrder_jobLists_selectionCb
*itemIdSL2.listSpacing: 1

*productIdSW2.class: scrolledWindow
*productIdSW2.static: true
*productIdSW2.name: productIdSW2
*productIdSW2.parent: form4
*productIdSW2.scrollingPolicy: "automatic"
*productIdSW2.visualPolicy: "constant"
*productIdSW2.scrollBarDisplayPolicy: "as_needed"
*productIdSW2.shadowThickness: 0
*productIdSW2.isCompound: "true"
*productIdSW2.compoundIcon: "scrllist.xpm"
*productIdSW2.compoundName: "scrolled_List"
*productIdSW2.x: 216
*productIdSW2.y: 172
*productIdSW2.background: "LightSkyBlue3"
*productIdSW2.height: 406
*productIdSW2.width: 140

*productIdSL2.class: scrolledList
*productIdSL2.static: true
*productIdSL2.name: productIdSL2
*productIdSL2.parent: productIdSW2
*productIdSL2.width: 140
*productIdSL2.height: 406
*productIdSL2.background: "LightSkyBlue3"
*productIdSL2.listSizePolicy: "constant"
*productIdSL2.selectionPolicy: "extended_select"
*productIdSL2.x: 0
*productIdSL2.y: 172
*productIdSL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*productIdSL2.visibleItemCount: 22
*productIdSL2.extendedSelectionCallbackClientData: (XtPointer) 3
*productIdSL2.extendedSelectionCallback.source: public
*productIdSL2.extendedSelectionCallback: photoOrder_jobLists_selectionCb
*productIdSL2.itemCount: 0
*productIdSL2.listSpacing: 1

*qtySW2.class: scrolledWindow
*qtySW2.static: true
*qtySW2.name: qtySW2
*qtySW2.parent: form4
*qtySW2.scrollingPolicy: "automatic"
*qtySW2.visualPolicy: "constant"
*qtySW2.scrollBarDisplayPolicy: "as_needed"
*qtySW2.shadowThickness: 0
*qtySW2.isCompound: "true"
*qtySW2.compoundIcon: "scrllist.xpm"
*qtySW2.compoundName: "scrolled_List"
*qtySW2.x: 360
*qtySW2.y: 172
*qtySW2.background: "LightSkyBlue3"
*qtySW2.height: 406
*qtySW2.width: 40

*qtySL2.class: scrolledList
*qtySL2.static: true
*qtySL2.name: qtySL2
*qtySL2.parent: qtySW2
*qtySL2.width: 40
*qtySL2.height: 406
*qtySL2.background: "LightSkyBlue3"
*qtySL2.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*qtySL2.listSizePolicy: "constant"
*qtySL2.selectionPolicy: "extended_select"
*qtySL2.x: 0
*qtySL2.y: 172
*qtySL2.visibleItemCount: 22
*qtySL2.extendedSelectionCallbackClientData: (XtPointer) 4
*qtySL2.extendedSelectionCallback.source: public
*qtySL2.extendedSelectionCallback: photoOrder_jobLists_selectionCb
*qtySL2.listSpacing: 1

*dummySW2.class: scrolledWindow
*dummySW2.static: true
*dummySW2.name: dummySW2
*dummySW2.parent: form4
*dummySW2.scrollingPolicy: "application_defined"
*dummySW2.visualPolicy: "variable"
*dummySW2.scrollBarDisplayPolicy: "static"
*dummySW2.shadowThickness: 0
*dummySW2.isCompound: "true"
*dummySW2.compoundIcon: "scrllist.xpm"
*dummySW2.compoundName: "scrolled_List"
*dummySW2.x: 400
*dummySW2.y: 172
*dummySW2.width: 15
*dummySW2.background: "LightSkyBlue3"
*dummySW2.height: 400

*dummySL2.class: scrolledList
*dummySL2.static: true
*dummySL2.name: dummySL2
*dummySL2.parent: dummySW2
*dummySL2.width: 2
*dummySL2.height: 380
*dummySL2.background: "LightSkyBlue3"
*dummySL2.listSizePolicy: "variable"
*dummySL2.scrollBarDisplayPolicy: "static"
*dummySL2.selectionPolicy: "extended_select"
*dummySL2.x: 0
*dummySL2.y: 172
*dummySL2.visibleItemCount: 22

*createPB.class: pushButton
*createPB.static: true
*createPB.name: createPB
*createPB.parent: form4
*createPB.isCompound: "true"
*createPB.compoundIcon: "push.xpm"
*createPB.compoundName: "push_Button"
*createPB.x: 28
*createPB.y: 588
*createPB.background: "CadetBlue"
*createPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*createPB.height: 36
*createPB.shadowThickness: 4
*createPB.labelString: "CREATE"
*createPB.width: 108
*createPB.sensitive: "false"
*createPB.activateCallback.source: public
*createPB.activateCallback: photoOrder_createCb

*separator6.class: separator
*separator6.static: true
*separator6.name: separator6
*separator6.parent: form4
*separator6.width: 440
*separator6.height: 8
*separator6.isCompound: "true"
*separator6.compoundIcon: "sep.xpm"
*separator6.compoundName: "separator_"
*separator6.x: 0
*separator6.y: 132
*separator6.background: "#9ac0cd"

*cancelPB.class: pushButton
*cancelPB.static: true
*cancelPB.name: cancelPB
*cancelPB.parent: form4
*cancelPB.isCompound: "true"
*cancelPB.compoundIcon: "push.xpm"
*cancelPB.compoundName: "push_Button"
*cancelPB.x: 308
*cancelPB.y: 588
*cancelPB.background: "CadetBlue"
*cancelPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*cancelPB.height: 36
*cancelPB.shadowThickness: 4
*cancelPB.labelString: "CANCEL"
*cancelPB.width: 108
*cancelPB.sensitive: "false"
*cancelPB.activateCallback.source: public
*cancelPB.activateCallback: photoOrder_cancelCb

*deletePB.class: pushButton
*deletePB.static: true
*deletePB.name: deletePB
*deletePB.parent: form4
*deletePB.isCompound: "true"
*deletePB.compoundIcon: "push.xpm"
*deletePB.compoundName: "push_Button"
*deletePB.x: 168
*deletePB.y: 588
*deletePB.background: "CadetBlue"
*deletePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*deletePB.height: 36
*deletePB.shadowThickness: 4
*deletePB.labelString: "DELETE"
*deletePB.width: 108
*deletePB.sensitive: "false"
*deletePB.activateCallback.source: public
*deletePB.activateCallback: photoOrder_deleteCb

*separator4.class: separator
*separator4.static: true
*separator4.name: separator4
*separator4.parent: photoOrder
*separator4.width: 1040
*separator4.height: 4
*separator4.isCompound: "true"
*separator4.compoundIcon: "sep.xpm"
*separator4.compoundName: "separator_"
*separator4.x: 0
*separator4.y: 724
*separator4.background: "#9ac0cd"

*menuBar2.class: rowColumn
*menuBar2.static: true
*menuBar2.name: menuBar2
*menuBar2.parent: photoOrder
*menuBar2.rowColumnType: "menu_bar"
*menuBar2.isCompound: "true"
*menuBar2.compoundIcon: "pulldownM.xpm"
*menuBar2.compoundName: "menu_Bar"
*menuBar2.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*menuBar2.x: 229
*menuBar2.y: 0
*menuBar2.background: "CadetBlue"
*menuBar2.height: 36
*menuBar2.menuAccelerator: "<KeyUp>F10"
*menuBar2.menuHelpWidget: "menuBar1_top_b4"
*menuBar2.rightAttachment: "attach_form"
*menuBar2.leftAttachment: "attach_form"

*menuBar_p2.class: rowColumn
*menuBar_p2.static: true
*menuBar_p2.name: menuBar_p2
*menuBar_p2.parent: menuBar2
*menuBar_p2.rowColumnType: "menu_pulldown"

*welcomeMPB.class: pushButton
*welcomeMPB.static: true
*welcomeMPB.name: welcomeMPB
*welcomeMPB.parent: menuBar_p2
*welcomeMPB.labelString: "Welcome Screen"
*welcomeMPB.background: "CadetBlue"
*welcomeMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*welcomeMPB.mnemonic: "W"
*welcomeMPB.activateCallback.source: public
*welcomeMPB.activateCallback: goto_welcomeScreen

*menuBar_p1_b1.class: separator
*menuBar_p1_b1.static: true
*menuBar_p1_b1.name: menuBar_p1_b1
*menuBar_p1_b1.parent: menuBar_p2

*menuBar_p2_b4.class: pushButton
*menuBar_p2_b4.static: true
*menuBar_p2_b4.name: menuBar_p2_b4
*menuBar_p2_b4.parent: menuBar_p2
*menuBar_p2_b4.labelString: "Complete Photo Jobs Screen"
*menuBar_p2_b4.mnemonic: "P"
*menuBar_p2_b4.activateCallback.source: public
*menuBar_p2_b4.activateCallback: goto_photoJobScreen
*menuBar_p2_b4.background: "cadetBlue"
*menuBar_p2_b4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*menuBar_p2_b5.class: separator
*menuBar_p2_b5.static: true
*menuBar_p2_b5.name: menuBar_p2_b5
*menuBar_p2_b5.parent: menuBar_p2

*menuBar_p1_b18.class: pushButton
*menuBar_p1_b18.static: true
*menuBar_p1_b18.name: menuBar_p1_b18
*menuBar_p1_b18.parent: menuBar_p2
*menuBar_p1_b18.labelString: "Close  Screen"
*menuBar_p1_b18.mnemonic: "C"
*menuBar_p1_b18.background: "cadetBlue"
*menuBar_p1_b18.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_p1_b18.activateCallback.source: public
*menuBar_p1_b18.activateCallback: photoOrder_closeCb

*menuBar1_p1.class: rowColumn
*menuBar1_p1.static: true
*menuBar1_p1.name: menuBar1_p1
*menuBar1_p1.parent: menuBar2
*menuBar1_p1.rowColumnType: "menu_pulldown"

*printMPB.class: pushButton
*printMPB.static: true
*printMPB.name: printMPB
*printMPB.parent: menuBar1_p1
*printMPB.labelString: "Print  Screen"
*printMPB.mnemonic: "P"
*printMPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*printMPB.background: "CadetBlue"
*printMPB.activateCallback.source: public
*printMPB.activateCallback: photoOrder_printCb

*menuBar1_p4.class: rowColumn
*menuBar1_p4.static: true
*menuBar1_p4.name: menuBar1_p4
*menuBar1_p4.parent: menuBar2
*menuBar1_p4.rowColumnType: "menu_pulldown"

*menuBar1_p3_b2.class: pushButton
*menuBar1_p3_b2.static: true
*menuBar1_p3_b2.name: menuBar1_p3_b2
*menuBar1_p3_b2.parent: menuBar1_p4
*menuBar1_p3_b2.labelString: "No Help Available"
*menuBar1_p3_b2.activateCallback.source: public
*menuBar1_p3_b2.activateCallback: 
*menuBar1_p3_b2.background: "CadetBlue"

*menuBar_top_b2.class: cascadeButton
*menuBar_top_b2.static: true
*menuBar_top_b2.name: menuBar_top_b2
*menuBar_top_b2.parent: menuBar2
*menuBar_top_b2.labelString: "Go To"
*menuBar_top_b2.subMenuId: "menuBar_p2"
*menuBar_top_b2.mnemonic: "G"
*menuBar_top_b2.background: "CadetBlue"
*menuBar_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar_top_b2.marginWidth: 10
*menuBar_top_b2.x: 0
*menuBar_top_b2.y: 0

*menuBar1_top_b3.class: cascadeButtonGadget
*menuBar1_top_b3.static: true
*menuBar1_top_b3.name: menuBar1_top_b3
*menuBar1_top_b3.parent: menuBar2
*menuBar1_top_b3.labelString: "Screen Functions"
*menuBar1_top_b3.mnemonic: "S"
*menuBar1_top_b3.subMenuId: "menuBar1_p1"
*menuBar1_top_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b3.marginWidth: 20
*menuBar1_top_b3.x: 0
*menuBar1_top_b3.y: 0

*menuBar1_top_b4.class: cascadeButton
*menuBar1_top_b4.static: true
*menuBar1_top_b4.name: menuBar1_top_b4
*menuBar1_top_b4.parent: menuBar2
*menuBar1_top_b4.labelString: "Help"
*menuBar1_top_b4.mnemonic: "H"
*menuBar1_top_b4.subMenuId: "menuBar1_p4"
*menuBar1_top_b4.background: "CadetBlue"
*menuBar1_top_b4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*menuBar1_top_b4.x: 0
*menuBar1_top_b4.y: 0

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: photoOrder
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 64
*printPB.y: 740
*printPB.background: "CadetBlue"
*printPB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*printPB.height: 36
*printPB.shadowThickness: 4
*printPB.labelString: "PRINT    SCREEN"
*printPB.width: 180
*printPB.activateCallback.source: public
*printPB.activateCallback: photoOrder_printCb

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: photoOrder
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 796
*closePB.y: 740
*closePB.background: "CadetBlue"
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.height: 36
*closePB.shadowThickness: 4
*closePB.labelString: "CLOSE    SCREEN"
*closePB.width: 180
*closePB.activateCallback.source: public
*closePB.activateCallback: photoOrder_closeCb

*addPB.class: pushButton
*addPB.static: true
*addPB.name: addPB
*addPB.parent: photoOrder
*addPB.isCompound: "true"
*addPB.compoundIcon: "push.xpm"
*addPB.compoundName: "push_Button"
*addPB.x: 500
*addPB.y: 312
*addPB.background: "cadetBlue"
*addPB.labelPixmap: "/local/imsdads/app-defaults/pixmaps/arrow"
*addPB.labelType: "pixmap"
*addPB.shadowThickness: 4
*addPB.activateCallback.source: public
*addPB.activateCallback: photoOrder_addCb
*addPB.resizable: "false"
*addPB.sensitive: "false"
*addPB.leftAttachment: "attach_widget"
*addPB.leftWidget: "frame2"
*addPB.rightAttachment: "attach_widget"
*addPB.rightWidget: "frame3"
*addPB.bottomAttachment: "attach_none"
*addPB.bottomOffset: 0
*addPB.recomputeSize: "false"
*addPB.height: 81
*addPB.width: 81
*addPB.labelInsensitivePixmap: "/local/imsdads/app-defaults/pixmaps/hobbes.xbm"

