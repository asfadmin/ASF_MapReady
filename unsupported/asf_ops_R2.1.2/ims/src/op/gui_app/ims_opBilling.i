! UIMX ascii 2.9 key: 4943                                                      

*billing.class: form
*billing.classinc:
*billing.classspec:
*billing.classmembers:
*billing.classconstructor:
*billing.classdestructor:
*billing.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*billing.ispecdecl:
*billing.funcdecl: swidget create_billing(swidget UxParent)
*billing.funcname: create_billing
*billing.funcdef: "swidget", "<create_billing>(%)"
*billing.argdecl: swidget UxParent;
*billing.arglist: UxParent
*billing.arglist.UxParent: "swidget", "%UxParent%"
*billing.icode:
*billing.fcode: return(rtrn);\

*billing.auxdecl:
*billing.static: true
*billing.name: billing
*billing.parent: NO_PARENT
*billing.parentExpression: UxParent
*billing.defaultShell: transientShell
*billing.resizePolicy: "resize_none"
*billing.isCompound: "true"
*billing.compoundIcon: "form.xpm"
*billing.compoundName: "form_"
*billing.x: 91
*billing.y: 40
*billing.unitType: "pixels"
*billing.allowShellResize: "false"
*billing.background: "#9ac0cd"
*billing.width: 784
*billing.height: 815

*label94.class: label
*label94.static: true
*label94.name: label94
*label94.parent: billing
*label94.isCompound: "true"
*label94.compoundIcon: "label.xpm"
*label94.compoundName: "label_"
*label94.x: 288
*label94.y: 20
*label94.background: "#9ac0cd"
*label94.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label94.labelString: "Billing             Screen"
*label94.height: 36
*label94.width: 264

*frame11.class: frame
*frame11.static: true
*frame11.name: frame11
*frame11.parent: billing
*frame11.width: 744
*frame11.height: 728
*frame11.isCompound: "true"
*frame11.compoundIcon: "frame.xpm"
*frame11.compoundName: "frame_"
*frame11.x: 22
*frame11.y: 72
*frame11.background: "LightSkyBlue3"
*frame11.shadowThickness: 5
*frame11.shadowType: "shadow_etched_out"

*form12.class: form
*form12.static: true
*form12.name: form12
*form12.parent: frame11
*form12.width: 327
*form12.height: 612
*form12.resizePolicy: "resize_none"
*form12.isCompound: "true"
*form12.compoundIcon: "form.xpm"
*form12.compoundName: "form_"
*form12.x: 0
*form12.y: -8
*form12.background: "#9ac0cd"

*separator31.class: separator
*separator31.static: true
*separator31.name: separator31
*separator31.parent: form12
*separator31.width: 736
*separator31.height: 12
*separator31.isCompound: "true"
*separator31.compoundIcon: "sep.xpm"
*separator31.compoundName: "separator_"
*separator31.x: 0
*separator31.y: 656
*separator31.background: "#9ac0cd"

*okPB.class: pushButton
*okPB.static: true
*okPB.name: okPB
*okPB.parent: form12
*okPB.isCompound: "true"
*okPB.compoundIcon: "push.xpm"
*okPB.compoundName: "push_Button"
*okPB.x: 56
*okPB.y: 672
*okPB.background: "cadetBlue"
*okPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*okPB.labelString: "OK"
*okPB.width: 144
*okPB.shadowThickness: 4
*okPB.height: 36
*okPB.activateCallback.source: public
*okPB.activateCallback: billing_printFormCb
*okPB.activateCallbackClientData: (XtPointer) 1

*cancelPB.class: pushButton
*cancelPB.static: true
*cancelPB.name: cancelPB
*cancelPB.parent: form12
*cancelPB.isCompound: "true"
*cancelPB.compoundIcon: "push.xpm"
*cancelPB.compoundName: "push_Button"
*cancelPB.x: 544
*cancelPB.y: 672
*cancelPB.background: "cadetBlue"
*cancelPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*cancelPB.labelString: "Cancel"
*cancelPB.width: 144
*cancelPB.shadowThickness: 4
*cancelPB.height: 36
*cancelPB.activateCallback.source: public
*cancelPB.activateCallback: billing_cancelFormCb

*label102.class: label
*label102.static: true
*label102.name: label102
*label102.parent: form12
*label102.isCompound: "true"
*label102.compoundIcon: "label.xpm"
*label102.compoundName: "label_"
*label102.x: 22
*label102.y: 18
*label102.background: "SlateGray4"
*label102.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label102.labelString: " Bill To: "
*label102.height: 24
*label102.width: 90
*label102.alignment: "alignment_beginning"
*label102.foreground: "white"

*label103.class: label
*label103.static: true
*label103.name: label103
*label103.parent: form12
*label103.isCompound: "true"
*label103.compoundIcon: "label.xpm"
*label103.compoundName: "label_"
*label103.x: 28
*label103.y: 258
*label103.background: "#9ac0cd"
*label103.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label103.labelString: "Order Date"
*label103.height: 24
*label103.width: 95
*label103.alignment: "alignment_beginning"
*label103.foreground: "black"

*orderDateTF.class: textField
*orderDateTF.static: true
*orderDateTF.name: orderDateTF
*orderDateTF.parent: form12
*orderDateTF.width: 144
*orderDateTF.isCompound: "true"
*orderDateTF.compoundIcon: "textfield.xpm"
*orderDateTF.compoundName: "text_Field"
*orderDateTF.x: 176
*orderDateTF.y: 256
*orderDateTF.background: "LightSkyBlue3"
*orderDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*orderDateTF.marginHeight: 3
*orderDateTF.cursorPositionVisible: "false"
*orderDateTF.editable: "false"

*label105.class: label
*label105.static: true
*label105.name: label105
*label105.parent: form12
*label105.isCompound: "true"
*label105.compoundIcon: "label.xpm"
*label105.compoundName: "label_"
*label105.x: 10
*label105.y: 336
*label105.background: "#9ac0cd"
*label105.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label105.labelString: "Item"
*label105.height: 20
*label105.width: 48
*label105.alignment: "alignment_beginning"
*label105.foreground: "black"

*label119.class: label
*label119.static: true
*label119.name: label119
*label119.parent: form12
*label119.isCompound: "true"
*label119.compoundIcon: "label.xpm"
*label119.compoundName: "label_"
*label119.x: 238
*label119.y: 336
*label119.background: "#9ac0cd"
*label119.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label119.labelString: "Name"
*label119.height: 20
*label119.width: 100
*label119.alignment: "alignment_beginning"
*label119.foreground: "black"

*label120.class: label
*label120.static: true
*label120.name: label120
*label120.parent: form12
*label120.isCompound: "true"
*label120.compoundIcon: "label.xpm"
*label120.compoundName: "label_"
*label120.x: 400
*label120.y: 336
*label120.background: "#9ac0cd"
*label120.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label120.labelString: "Media"
*label120.height: 20
*label120.width: 52
*label120.alignment: "alignment_beginning"
*label120.foreground: "black"

*label121.class: label
*label121.static: true
*label121.name: label121
*label121.parent: form12
*label121.isCompound: "true"
*label121.compoundIcon: "label.xpm"
*label121.compoundName: "label_"
*label121.x: 606
*label121.y: 336
*label121.background: "#9ac0cd"
*label121.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label121.labelString: "Qty"
*label121.height: 20
*label121.width: 32
*label121.alignment: "alignment_beginning"
*label121.foreground: "black"

*label122.class: label
*label122.static: true
*label122.name: label122
*label122.parent: form12
*label122.isCompound: "true"
*label122.compoundIcon: "label.xpm"
*label122.compoundName: "label_"
*label122.x: 654
*label122.y: 336
*label122.background: "#9ac0cd"
*label122.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label122.labelString: "Cost"
*label122.height: 20
*label122.width: 40
*label122.alignment: "alignment_beginning"
*label122.foreground: "black"

*label106.class: label
*label106.static: true
*label106.name: label106
*label106.parent: form12
*label106.isCompound: "true"
*label106.compoundIcon: "label.xpm"
*label106.compoundName: "label_"
*label106.x: 28
*label106.y: 187
*label106.background: "#9ac0cd"
*label106.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label106.labelString: "Account ID"
*label106.height: 24
*label106.width: 96
*label106.alignment: "alignment_beginning"
*label106.foreground: "black"

*label123.class: label
*label123.static: true
*label123.name: label123
*label123.parent: form12
*label123.isCompound: "true"
*label123.compoundIcon: "label.xpm"
*label123.compoundName: "label_"
*label123.x: 28
*label123.y: 150
*label123.background: "#9ac0cd"
*label123.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label123.labelString: "Invoice ID"
*label123.height: 24
*label123.width: 92
*label123.alignment: "alignment_beginning"
*label123.foreground: "black"

*label124.class: label
*label124.static: true
*label124.name: label124
*label124.parent: form12
*label124.isCompound: "true"
*label124.compoundIcon: "label.xpm"
*label124.compoundName: "label_"
*label124.x: 408
*label124.y: 150
*label124.background: "#9ac0cd"
*label124.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label124.labelString: "Invoice Amount"
*label124.height: 24
*label124.width: 144
*label124.alignment: "alignment_beginning"
*label124.foreground: "black"

*label125.class: label
*label125.static: true
*label125.name: label125
*label125.parent: form12
*label125.isCompound: "true"
*label125.compoundIcon: "label.xpm"
*label125.compoundName: "label_"
*label125.x: 408
*label125.y: 187
*label125.background: "#9ac0cd"
*label125.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label125.labelString: "Resource Type"
*label125.height: 24
*label125.width: 128
*label125.alignment: "alignment_beginning"
*label125.foreground: "black"

*label126.class: label
*label126.static: true
*label126.name: label126
*label126.parent: form12
*label126.isCompound: "true"
*label126.compoundIcon: "label.xpm"
*label126.compoundName: "label_"
*label126.x: 408
*label126.y: 258
*label126.background: "#9ac0cd"
*label126.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label126.labelString: "Invoice Date"
*label126.height: 24
*label126.width: 112
*label126.alignment: "alignment_beginning"
*label126.foreground: "black"

*label104.class: label
*label104.static: true
*label104.name: label104
*label104.parent: form12
*label104.isCompound: "true"
*label104.compoundIcon: "label.xpm"
*label104.compoundName: "label_"
*label104.x: 408
*label104.y: 223
*label104.background: "#9ac0cd"
*label104.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label104.labelString: "Order ID"
*label104.height: 24
*label104.width: 84
*label104.alignment: "alignment_beginning"
*label104.foreground: "black"

*label131.class: label
*label131.static: true
*label131.name: label131
*label131.parent: form12
*label131.isCompound: "true"
*label131.compoundIcon: "label.xpm"
*label131.compoundName: "label_"
*label131.x: 62
*label131.y: 336
*label131.background: "#9ac0cd"
*label131.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label131.labelString: "Description"
*label131.height: 20
*label131.width: 100
*label131.alignment: "alignment_beginning"
*label131.foreground: "black"

*commentPB.class: pushButton
*commentPB.static: true
*commentPB.name: commentPB
*commentPB.parent: form12
*commentPB.isCompound: "true"
*commentPB.compoundIcon: "push.xpm"
*commentPB.compoundName: "push_Button"
*commentPB.x: 300
*commentPB.y: 672
*commentPB.background: "cadetBlue"
*commentPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentPB.labelString: "Edit Comments"
*commentPB.width: 144
*commentPB.shadowThickness: 4
*commentPB.height: 36
*commentPB.activateCallback.source: public
*commentPB.activateCallback: billing_editCommentsCb

*billItemSW.class: scrolledWindow
*billItemSW.static: true
*billItemSW.name: billItemSW
*billItemSW.parent: form12
*billItemSW.scrollingPolicy: "application_defined"
*billItemSW.visualPolicy: "variable"
*billItemSW.scrollBarDisplayPolicy: "static"
*billItemSW.shadowThickness: 0
*billItemSW.isCompound: "true"
*billItemSW.compoundIcon: "scrllist.xpm"
*billItemSW.compoundName: "scrolled_List"
*billItemSW.x: 8
*billItemSW.y: 360
*billItemSW.background: "#9ac0cd"
*billItemSW.width: 580
*billItemSW.height: 290
*billItemSW.rightAttachment: "attach_form"
*billItemSW.rightOffset: 10
*billItemSW.leftAttachment: "attach_form"
*billItemSW.leftOffset: 10

*billItemSL.class: scrolledList
*billItemSL.static: true
*billItemSL.name: billItemSL
*billItemSL.parent: billItemSW
*billItemSL.width: 520
*billItemSL.height: 290
*billItemSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*billItemSL.background: "LightSkyBlue3"
*billItemSL.scrollBarDisplayPolicy: "static"
*billItemSL.itemCount: 0
*billItemSL.listSizePolicy: "variable"

*invoiceIdTF.class: textField
*invoiceIdTF.static: true
*invoiceIdTF.name: invoiceIdTF
*invoiceIdTF.parent: form12
*invoiceIdTF.width: 145
*invoiceIdTF.isCompound: "true"
*invoiceIdTF.compoundIcon: "textfield.xpm"
*invoiceIdTF.compoundName: "text_Field"
*invoiceIdTF.x: 176
*invoiceIdTF.y: 147
*invoiceIdTF.background: "LightSkyBlue3"
*invoiceIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*invoiceIdTF.text: ""
*invoiceIdTF.marginHeight: 3
*invoiceIdTF.height: 30
*invoiceIdTF.cursorPositionVisible: "false"
*invoiceIdTF.editable: "false"

*accountIdTF.class: textField
*accountIdTF.static: true
*accountIdTF.name: accountIdTF
*accountIdTF.parent: form12
*accountIdTF.width: 145
*accountIdTF.isCompound: "true"
*accountIdTF.compoundIcon: "textfield.xpm"
*accountIdTF.compoundName: "text_Field"
*accountIdTF.x: 176
*accountIdTF.y: 184
*accountIdTF.background: "LightSkyBlue3"
*accountIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*accountIdTF.marginHeight: 3
*accountIdTF.height: 30
*accountIdTF.cursorPositionVisible: "false"
*accountIdTF.editable: "false"

*amountTF.class: textField
*amountTF.static: true
*amountTF.name: amountTF
*amountTF.parent: form12
*amountTF.width: 145
*amountTF.isCompound: "true"
*amountTF.compoundIcon: "textfield.xpm"
*amountTF.compoundName: "text_Field"
*amountTF.x: 556
*amountTF.y: 147
*amountTF.background: "LightSkyBlue3"
*amountTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*amountTF.text: ""
*amountTF.marginHeight: 3
*amountTF.height: 30
*amountTF.cursorPositionVisible: "false"
*amountTF.editable: "false"

*orderIdTF.class: textField
*orderIdTF.static: true
*orderIdTF.name: orderIdTF
*orderIdTF.parent: form12
*orderIdTF.width: 145
*orderIdTF.isCompound: "true"
*orderIdTF.compoundIcon: "textfield.xpm"
*orderIdTF.compoundName: "text_Field"
*orderIdTF.x: 556
*orderIdTF.y: 220
*orderIdTF.background: "LightSkyBlue3"
*orderIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*orderIdTF.text: ""
*orderIdTF.marginHeight: 3
*orderIdTF.height: 30
*orderIdTF.cursorPositionVisible: "false"
*orderIdTF.editable: "false"

*balanceTF.class: textField
*balanceTF.static: true
*balanceTF.name: balanceTF
*balanceTF.parent: form12
*balanceTF.width: 145
*balanceTF.isCompound: "true"
*balanceTF.compoundIcon: "textfield.xpm"
*balanceTF.compoundName: "text_Field"
*balanceTF.x: 176
*balanceTF.y: 220
*balanceTF.background: "LightSkyBlue3"
*balanceTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*balanceTF.marginHeight: 3
*balanceTF.height: 30
*balanceTF.cursorPositionVisible: "false"
*balanceTF.editable: "false"

*billDateTF.class: textField
*billDateTF.static: true
*billDateTF.name: billDateTF
*billDateTF.parent: form12
*billDateTF.width: 145
*billDateTF.isCompound: "true"
*billDateTF.compoundIcon: "textfield.xpm"
*billDateTF.compoundName: "text_Field"
*billDateTF.x: 556
*billDateTF.y: 255
*billDateTF.background: "LightSkyBlue3"
*billDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*billDateTF.marginHeight: 3
*billDateTF.height: 30
*billDateTF.cursorPositionVisible: "false"
*billDateTF.editable: "false"

*billToSW.class: scrolledWindow
*billToSW.static: true
*billToSW.name: billToSW
*billToSW.parent: form12
*billToSW.scrollingPolicy: "application_defined"
*billToSW.visualPolicy: "variable"
*billToSW.scrollBarDisplayPolicy: "static"
*billToSW.isCompound: "true"
*billToSW.compoundIcon: "scrltext.xpm"
*billToSW.compoundName: "scrolled_Text"
*billToSW.x: 176
*billToSW.y: 12
*billToSW.background: "LightSkyBlue3"
*billToSW.width: 520
*billToSW.height: 115

*billToST.class: scrolledText
*billToST.static: true
*billToST.name: billToST
*billToST.parent: billToSW
*billToST.width: 424
*billToST.background: "LightSkyBlue3"
*billToST.scrollHorizontal: "true"
*billToST.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*billToST.editMode: "multi_line_edit"
*billToST.editable: "false"
*billToST.cursorPositionVisible: "false"
*billToST.height: 120

*label203.class: label
*label203.static: true
*label203.name: label203
*label203.parent: form12
*label203.isCompound: "true"
*label203.compoundIcon: "label.xpm"
*label203.compoundName: "label_"
*label203.x: 524
*label203.y: 336
*label203.background: "#9ac0cd"
*label203.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label203.labelString: "Status"
*label203.height: 20
*label203.width: 52
*label203.alignment: "alignment_beginning"
*label203.foreground: "black"

*label204.class: label
*label204.static: true
*label204.name: label204
*label204.parent: form12
*label204.isCompound: "true"
*label204.compoundIcon: "label.xpm"
*label204.compoundName: "label_"
*label204.x: 28
*label204.y: 223
*label204.background: "#9ac0cd"
*label204.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label204.labelString: "Account Balance"
*label204.height: 24
*label204.width: 140
*label204.alignment: "alignment_beginning"
*label204.foreground: "black"

*resourceTF.class: textField
*resourceTF.static: true
*resourceTF.name: resourceTF
*resourceTF.parent: form12
*resourceTF.width: 145
*resourceTF.isCompound: "true"
*resourceTF.compoundIcon: "textfield.xpm"
*resourceTF.compoundName: "text_Field"
*resourceTF.x: 556
*resourceTF.y: 184
*resourceTF.background: "LightSkyBlue3"
*resourceTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*resourceTF.marginHeight: 3
*resourceTF.height: 30
*resourceTF.cursorPositionVisible: "false"
*resourceTF.editable: "false"

*label205.class: label
*label205.static: true
*label205.name: label205
*label205.parent: form12
*label205.isCompound: "true"
*label205.compoundIcon: "label.xpm"
*label205.compoundName: "label_"
*label205.x: 28
*label205.y: 292
*label205.background: "#9ac0cd"
*label205.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label205.labelString: "User Name"
*label205.height: 24
*label205.width: 95
*label205.alignment: "alignment_beginning"
*label205.foreground: "black"

*userNameTF.class: textField
*userNameTF.static: true
*userNameTF.name: userNameTF
*userNameTF.parent: form12
*userNameTF.width: 525
*userNameTF.isCompound: "true"
*userNameTF.compoundIcon: "textfield.xpm"
*userNameTF.compoundName: "text_Field"
*userNameTF.x: 176
*userNameTF.y: 292
*userNameTF.background: "LightSkyBlue3"
*userNameTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*userNameTF.marginHeight: 3
*userNameTF.cursorPositionVisible: "false"
*userNameTF.editable: "false"

*label100.class: label
*label100.static: true
*label100.name: label100
*label100.parent: billing
*label100.isCompound: "true"
*label100.compoundIcon: "label.xpm"
*label100.compoundName: "label_"
*label100.x: 88
*label100.y: 4
*label100.background: "#9ac0cd"
*label100.labelPixmap: "/local/imsdads/app-defaults/pixmaps/calc.xpm"
*label100.labelType: "pixmap"
*label100.width: 56
*label100.height: 52

