! UIMX ascii 2.9 key: 5396                                                      

*shipping.class: form
*shipping.classinc:
*shipping.classspec:
*shipping.classmembers:
*shipping.classconstructor:
*shipping.classdestructor:
*shipping.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*shipping.ispecdecl:
*shipping.funcdecl: swidget create_shipping(swidget UxParent)
*shipping.funcname: create_shipping
*shipping.funcdef: "swidget", "<create_shipping>(%)"
*shipping.argdecl: swidget UxParent;
*shipping.arglist: UxParent
*shipping.arglist.UxParent: "swidget", "%UxParent%"
*shipping.icode:
*shipping.fcode: return(rtrn);\

*shipping.auxdecl:
*shipping.static: true
*shipping.name: shipping
*shipping.parent: NO_PARENT
*shipping.parentExpression: UxParent
*shipping.defaultShell: transientShell
*shipping.resizePolicy: "resize_none"
*shipping.isCompound: "true"
*shipping.compoundIcon: "form.xpm"
*shipping.compoundName: "form_"
*shipping.unitType: "pixels"
*shipping.allowShellResize: "false"
*shipping.background: "#9ac0cd"
*shipping.height: 810
*shipping.width: 784
*shipping.defaultPosition: "true"
*shipping.x: 415
*shipping.y: 33

*frame10.class: frame
*frame10.static: true
*frame10.name: frame10
*frame10.parent: shipping
*frame10.width: 743
*frame10.height: 728
*frame10.isCompound: "true"
*frame10.compoundIcon: "frame.xpm"
*frame10.compoundName: "frame_"
*frame10.x: 19
*frame10.y: 62
*frame10.background: "LightSkyBlue3"
*frame10.shadowThickness: 5
*frame10.shadowType: "shadow_etched_out"

*form11.class: form
*form11.static: true
*form11.name: form11
*form11.parent: frame10
*form11.width: 557
*form11.height: 671
*form11.resizePolicy: "resize_none"
*form11.isCompound: "true"
*form11.compoundIcon: "form.xpm"
*form11.compoundName: "form_"
*form11.x: 5
*form11.y: 5
*form11.background: "#9ac0cd"

*label108.class: label
*label108.static: true
*label108.name: label108
*label108.parent: form11
*label108.isCompound: "true"
*label108.compoundIcon: "label.xpm"
*label108.compoundName: "label_"
*label108.x: 36
*label108.y: 16
*label108.background: "SlateGray4"
*label108.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label108.labelString: " Ship To: "
*label108.height: 24
*label108.width: 94
*label108.alignment: "alignment_beginning"
*label108.foreground: "white"

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: form11
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 44
*printPB.y: 668
*printPB.background: "cadetBlue"
*printPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*printPB.labelString: "OK"
*printPB.width: 144
*printPB.shadowThickness: 4
*printPB.height: 36
*printPB.activateCallback.source: public
*printPB.activateCallback: shipping_printFormCb
*printPB.activateCallbackClientData: (XtPointer) 1

*cancelPB.class: pushButton
*cancelPB.static: true
*cancelPB.name: cancelPB
*cancelPB.parent: form11
*cancelPB.isCompound: "true"
*cancelPB.compoundIcon: "push.xpm"
*cancelPB.compoundName: "push_Button"
*cancelPB.x: 548
*cancelPB.y: 668
*cancelPB.background: "cadetBlue"
*cancelPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*cancelPB.labelString: "Cancel"
*cancelPB.width: 144
*cancelPB.shadowThickness: 4
*cancelPB.height: 36
*cancelPB.activateCallback.source: public
*cancelPB.activateCallback: shipping_cancelFormCb

*separator.class: separator
*separator.static: true
*separator.name: separator
*separator.parent: form11
*separator.width: 736
*separator.height: 10
*separator.isCompound: "true"
*separator.compoundIcon: "sep.xpm"
*separator.compoundName: "separator_"
*separator.x: 0
*separator.y: 648
*separator.background: "#9ac0cd"

*label109.class: label
*label109.static: true
*label109.name: label109
*label109.parent: form11
*label109.isCompound: "true"
*label109.compoundIcon: "label.xpm"
*label109.compoundName: "label_"
*label109.x: 12
*label109.y: 316
*label109.background: "#9ac0cd"
*label109.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label109.labelString: "Item"
*label109.height: 20
*label109.width: 48
*label109.alignment: "alignment_beginning"
*label109.foreground: "black"

*label110.class: label
*label110.static: true
*label110.name: label110
*label110.parent: form11
*label110.isCompound: "true"
*label110.compoundIcon: "label.xpm"
*label110.compoundName: "label_"
*label110.x: 428
*label110.y: 157
*label110.background: "#9ac0cd"
*label110.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label110.labelString: "Order ID"
*label110.height: 24
*label110.width: 84
*label110.alignment: "alignment_beginning"
*label110.foreground: "black"

*orderIdTF.class: textField
*orderIdTF.static: true
*orderIdTF.name: orderIdTF
*orderIdTF.parent: form11
*orderIdTF.width: 145
*orderIdTF.isCompound: "true"
*orderIdTF.compoundIcon: "textfield.xpm"
*orderIdTF.compoundName: "text_Field"
*orderIdTF.x: 548
*orderIdTF.y: 155
*orderIdTF.background: "LightSkyBlue3"
*orderIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*orderIdTF.text: ""
*orderIdTF.marginHeight: 3
*orderIdTF.height: 30
*orderIdTF.cursorPositionVisible: "false"
*orderIdTF.editable: "false"

*label111.class: label
*label111.static: true
*label111.name: label111
*label111.parent: form11
*label111.isCompound: "true"
*label111.compoundIcon: "label.xpm"
*label111.compoundName: "label_"
*label111.x: 240
*label111.y: 316
*label111.background: "#9ac0cd"
*label111.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label111.labelString: "Name"
*label111.height: 20
*label111.width: 56
*label111.alignment: "alignment_beginning"
*label111.foreground: "black"

*label112.class: label
*label112.static: true
*label112.name: label112
*label112.parent: form11
*label112.isCompound: "true"
*label112.compoundIcon: "label.xpm"
*label112.compoundName: "label_"
*label112.x: 404
*label112.y: 316
*label112.background: "#9ac0cd"
*label112.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label112.labelString: "Media"
*label112.height: 20
*label112.width: 52
*label112.alignment: "alignment_beginning"
*label112.foreground: "black"

*label113.class: label
*label113.static: true
*label113.name: label113
*label113.parent: form11
*label113.isCompound: "true"
*label113.compoundIcon: "label.xpm"
*label113.compoundName: "label_"
*label113.x: 606
*label113.y: 316
*label113.background: "#9ac0cd"
*label113.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label113.labelString: "Qty"
*label113.height: 20
*label113.width: 32
*label113.alignment: "alignment_beginning"
*label113.foreground: "black"

*label114.class: label
*label114.static: true
*label114.name: label114
*label114.parent: form11
*label114.isCompound: "true"
*label114.compoundIcon: "label.xpm"
*label114.compoundName: "label_"
*label114.x: 648
*label114.y: 316
*label114.background: "#9ac0cd"
*label114.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label114.labelString: "Cost"
*label114.height: 20
*label114.width: 40
*label114.alignment: "alignment_beginning"
*label114.foreground: "black"

*label115.class: label
*label115.static: true
*label115.name: label115
*label115.parent: form11
*label115.isCompound: "true"
*label115.compoundIcon: "label.xpm"
*label115.compoundName: "label_"
*label115.x: 40
*label115.y: 196
*label115.background: "#9ac0cd"
*label115.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label115.labelString: "Account ID"
*label115.height: 24
*label115.width: 96
*label115.alignment: "alignment_beginning"
*label115.foreground: "black"

*shippingIdTF.class: textField
*shippingIdTF.static: true
*shippingIdTF.name: shippingIdTF
*shippingIdTF.parent: form11
*shippingIdTF.width: 145
*shippingIdTF.isCompound: "true"
*shippingIdTF.compoundIcon: "textfield.xpm"
*shippingIdTF.compoundName: "text_Field"
*shippingIdTF.x: 168
*shippingIdTF.y: 156
*shippingIdTF.background: "LightSkyBlue3"
*shippingIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shippingIdTF.text: ""
*shippingIdTF.marginHeight: 3
*shippingIdTF.height: 30
*shippingIdTF.cursorPositionVisible: "false"
*shippingIdTF.editable: "false"

*accountIdTF.class: textField
*accountIdTF.static: true
*accountIdTF.name: accountIdTF
*accountIdTF.parent: form11
*accountIdTF.width: 145
*accountIdTF.isCompound: "true"
*accountIdTF.compoundIcon: "textfield.xpm"
*accountIdTF.compoundName: "text_Field"
*accountIdTF.x: 168
*accountIdTF.y: 193
*accountIdTF.background: "LightSkyBlue3"
*accountIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*accountIdTF.marginHeight: 3
*accountIdTF.height: 30
*accountIdTF.cursorPositionVisible: "false"
*accountIdTF.editable: "false"

*carrierTF.class: textField
*carrierTF.static: true
*carrierTF.name: carrierTF
*carrierTF.parent: form11
*carrierTF.width: 145
*carrierTF.isCompound: "true"
*carrierTF.compoundIcon: "textfield.xpm"
*carrierTF.compoundName: "text_Field"
*carrierTF.x: 548
*carrierTF.y: 192
*carrierTF.background: "LightSkyBlue3"
*carrierTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*carrierTF.height: 30
*carrierTF.marginHeight: 3
*carrierTF.cursorPositionVisible: "false"
*carrierTF.editable: "false"

*label127.class: label
*label127.static: true
*label127.name: label127
*label127.parent: form11
*label127.isCompound: "true"
*label127.compoundIcon: "label.xpm"
*label127.compoundName: "label_"
*label127.x: 428
*label127.y: 230
*label127.background: "#9ac0cd"
*label127.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label127.labelString: "Order Date"
*label127.height: 24
*label127.width: 100
*label127.alignment: "alignment_beginning"
*label127.foreground: "black"

*shipDateTF.class: textField
*shipDateTF.static: true
*shipDateTF.name: shipDateTF
*shipDateTF.parent: form11
*shipDateTF.width: 145
*shipDateTF.isCompound: "true"
*shipDateTF.compoundIcon: "textfield.xpm"
*shipDateTF.compoundName: "text_Field"
*shipDateTF.x: 168
*shipDateTF.y: 228
*shipDateTF.background: "LightSkyBlue3"
*shipDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shipDateTF.text: ""
*shipDateTF.marginHeight: 3
*shipDateTF.height: 30
*shipDateTF.cursorPositionVisible: "false"
*shipDateTF.editable: "false"

*totalQtyTF.class: textField
*totalQtyTF.static: true
*totalQtyTF.name: totalQtyTF
*totalQtyTF.parent: form11
*totalQtyTF.width: 145
*totalQtyTF.isCompound: "true"
*totalQtyTF.compoundIcon: "textfield.xpm"
*totalQtyTF.compoundName: "text_Field"
*totalQtyTF.x: 168
*totalQtyTF.y: 266
*totalQtyTF.background: "LightSkyBlue3"
*totalQtyTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*totalQtyTF.text: ""
*totalQtyTF.marginHeight: 3
*totalQtyTF.height: 30
*totalQtyTF.cursorPositionVisible: "false"
*totalQtyTF.editable: "false"

*label128.class: label
*label128.static: true
*label128.name: label128
*label128.parent: form11
*label128.isCompound: "true"
*label128.compoundIcon: "label.xpm"
*label128.compoundName: "label_"
*label128.x: 428
*label128.y: 195
*label128.background: "#9ac0cd"
*label128.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label128.labelString: "Ship Via"
*label128.height: 24
*label128.width: 85
*label128.alignment: "alignment_beginning"
*label128.foreground: "black"

*orderDateTF.class: textField
*orderDateTF.static: true
*orderDateTF.name: orderDateTF
*orderDateTF.parent: form11
*orderDateTF.width: 145
*orderDateTF.isCompound: "true"
*orderDateTF.compoundIcon: "textfield.xpm"
*orderDateTF.compoundName: "text_Field"
*orderDateTF.x: 548
*orderDateTF.y: 227
*orderDateTF.background: "LightSkyBlue3"
*orderDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*orderDateTF.text: ""
*orderDateTF.marginHeight: 3
*orderDateTF.height: 30
*orderDateTF.cursorPositionVisible: "false"
*orderDateTF.editable: "false"

*totalCostTF.class: textField
*totalCostTF.static: true
*totalCostTF.name: totalCostTF
*totalCostTF.parent: form11
*totalCostTF.width: 145
*totalCostTF.isCompound: "true"
*totalCostTF.compoundIcon: "textfield.xpm"
*totalCostTF.compoundName: "text_Field"
*totalCostTF.x: 548
*totalCostTF.y: 265
*totalCostTF.background: "LightSkyBlue3"
*totalCostTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*totalCostTF.text: ""
*totalCostTF.marginHeight: 3
*totalCostTF.height: 30
*totalCostTF.cursorPositionVisible: "false"
*totalCostTF.editable: "false"

*shipToSW.class: scrolledWindow
*shipToSW.static: true
*shipToSW.name: shipToSW
*shipToSW.parent: form11
*shipToSW.scrollingPolicy: "application_defined"
*shipToSW.visualPolicy: "variable"
*shipToSW.scrollBarDisplayPolicy: "static"
*shipToSW.isCompound: "true"
*shipToSW.compoundIcon: "scrltext.xpm"
*shipToSW.compoundName: "scrolled_Text"
*shipToSW.x: 168
*shipToSW.y: 12
*shipToSW.background: "LightSkyBlue3"
*shipToSW.width: 520
*shipToSW.height: 115

*shipToST.class: scrolledText
*shipToST.static: true
*shipToST.name: shipToST
*shipToST.parent: shipToSW
*shipToST.width: 424
*shipToST.background: "LightSkyBlue3"
*shipToST.scrollHorizontal: "true"
*shipToST.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shipToST.editMode: "multi_line_edit"
*shipToST.editable: "false"
*shipToST.cursorPositionVisible: "false"
*shipToST.height: 120

*label129.class: label
*label129.static: true
*label129.name: label129
*label129.parent: form11
*label129.isCompound: "true"
*label129.compoundIcon: "label.xpm"
*label129.compoundName: "label_"
*label129.x: 40
*label129.y: 231
*label129.background: "#9ac0cd"
*label129.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label129.labelString: "Ship Date"
*label129.height: 24
*label129.width: 85
*label129.alignment: "alignment_beginning"
*label129.foreground: "black"

*label130.class: label
*label130.static: true
*label130.name: label130
*label130.parent: form11
*label130.isCompound: "true"
*label130.compoundIcon: "label.xpm"
*label130.compoundName: "label_"
*label130.x: 40
*label130.y: 160
*label130.background: "#9ac0cd"
*label130.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label130.labelString: "Shipping ID"
*label130.height: 24
*label130.width: 104
*label130.alignment: "alignment_beginning"
*label130.foreground: "black"

*label132.class: label
*label132.static: true
*label132.name: label132
*label132.parent: form11
*label132.isCompound: "true"
*label132.compoundIcon: "label.xpm"
*label132.compoundName: "label_"
*label132.x: 64
*label132.y: 316
*label132.background: "#9ac0cd"
*label132.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label132.labelString: "Description"
*label132.height: 20
*label132.width: 100
*label132.alignment: "alignment_beginning"
*label132.foreground: "black"

*shipItemSW.class: scrolledWindow
*shipItemSW.static: true
*shipItemSW.name: shipItemSW
*shipItemSW.parent: form11
*shipItemSW.scrollingPolicy: "application_defined"
*shipItemSW.visualPolicy: "variable"
*shipItemSW.scrollBarDisplayPolicy: "static"
*shipItemSW.shadowThickness: 0
*shipItemSW.isCompound: "true"
*shipItemSW.compoundIcon: "scrllist.xpm"
*shipItemSW.compoundName: "scrolled_List"
*shipItemSW.x: 12
*shipItemSW.y: 336
*shipItemSW.background: "#9ac0cd"
*shipItemSW.width: 580
*shipItemSW.height: 300
*shipItemSW.resizable: "false"
*shipItemSW.rightAttachment: "attach_form"
*shipItemSW.rightOffset: 8
*shipItemSW.leftAttachment: "attach_form"
*shipItemSW.leftOffset: 10
*shipItemSW.bottomAttachment: "attach_form"
*shipItemSW.bottomOffset: 80

*shipItemSL.class: scrolledList
*shipItemSL.static: true
*shipItemSL.name: shipItemSL
*shipItemSL.parent: shipItemSW
*shipItemSL.width: 520
*shipItemSL.height: 290
*shipItemSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shipItemSL.background: "LightSkyBlue3"
*shipItemSL.scrollBarDisplayPolicy: "static"
*shipItemSL.itemCount: 0
*shipItemSL.listSizePolicy: "variable"

*editCommentPB.class: pushButton
*editCommentPB.static: true
*editCommentPB.name: editCommentPB
*editCommentPB.parent: form11
*editCommentPB.isCompound: "true"
*editCommentPB.compoundIcon: "push.xpm"
*editCommentPB.compoundName: "push_Button"
*editCommentPB.x: 292
*editCommentPB.y: 668
*editCommentPB.background: "cadetBlue"
*editCommentPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*editCommentPB.labelString: "Edit Comments"
*editCommentPB.width: 144
*editCommentPB.shadowThickness: 4
*editCommentPB.height: 36
*editCommentPB.activateCallback.source: public
*editCommentPB.activateCallback: shipping_editCommentsCb

*label116.class: label
*label116.static: true
*label116.name: label116
*label116.parent: form11
*label116.isCompound: "true"
*label116.compoundIcon: "label.xpm"
*label116.compoundName: "label_"
*label116.x: 40
*label116.y: 269
*label116.background: "#9ac0cd"
*label116.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label116.labelString: "Total Qty"
*label116.height: 24
*label116.width: 83
*label116.alignment: "alignment_beginning"
*label116.foreground: "black"

*label117.class: label
*label117.static: true
*label117.name: label117
*label117.parent: form11
*label117.isCompound: "true"
*label117.compoundIcon: "label.xpm"
*label117.compoundName: "label_"
*label117.x: 428
*label117.y: 268
*label117.background: "#9ac0cd"
*label117.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label117.labelString: "Total Cost"
*label117.height: 24
*label117.width: 89
*label117.alignment: "alignment_beginning"
*label117.foreground: "black"

*label209.class: label
*label209.static: true
*label209.name: label209
*label209.parent: form11
*label209.isCompound: "true"
*label209.compoundIcon: "label.xpm"
*label209.compoundName: "label_"
*label209.x: 520
*label209.y: 316
*label209.background: "#9ac0cd"
*label209.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label209.labelString: "Status"
*label209.height: 20
*label209.width: 52
*label209.alignment: "alignment_beginning"
*label209.foreground: "black"

*label107.class: label
*label107.static: true
*label107.name: label107
*label107.parent: shipping
*label107.isCompound: "true"
*label107.compoundIcon: "label.xpm"
*label107.compoundName: "label_"
*label107.x: 308
*label107.y: 20
*label107.background: "#9ac0cd"
*label107.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label107.labelString: "Shipping            Screen"
*label107.height: 28
*label107.width: 224

*label118.class: label
*label118.static: true
*label118.name: label118
*label118.parent: shipping
*label118.isCompound: "true"
*label118.compoundIcon: "label.xpm"
*label118.compoundName: "label_"
*label118.x: 20
*label118.y: 4
*label118.background: "#9ac0cd"
*label118.labelType: "pixmap"
*label118.height: 48
*label118.width: 52
*label118.labelPixmap: "/local/imsdads/app-defaults/pixmaps/post.xpm"

