! UIMX ascii 2.9 key: 5628                                                      

*shipView.class: form
*shipView.classinc:
*shipView.classspec:
*shipView.classmembers:
*shipView.classconstructor:
*shipView.classdestructor:
*shipView.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*shipView.ispecdecl:
*shipView.funcdecl: swidget create_shipView(swidget UxParent)
*shipView.funcname: create_shipView
*shipView.funcdef: "swidget", "<create_shipView>(%)"
*shipView.argdecl: swidget UxParent;
*shipView.arglist: UxParent
*shipView.arglist.UxParent: "swidget", "%UxParent%"
*shipView.icode:
*shipView.fcode: return(rtrn);\

*shipView.auxdecl:
*shipView.static: true
*shipView.name: shipView
*shipView.parent: NO_PARENT
*shipView.parentExpression: UxParent
*shipView.defaultShell: transientShell
*shipView.width: 784
*shipView.height: 800
*shipView.resizePolicy: "resize_none"
*shipView.isCompound: "true"
*shipView.compoundIcon: "form.xpm"
*shipView.compoundName: "form_"
*shipView.x: 436
*shipView.y: 72
*shipView.unitType: "pixels"
*shipView.allowShellResize: "false"
*shipView.background: "#9ac0cd"

*shipViewLB.class: label
*shipViewLB.static: true
*shipViewLB.name: shipViewLB
*shipViewLB.parent: shipView
*shipViewLB.isCompound: "true"
*shipViewLB.compoundIcon: "label.xpm"
*shipViewLB.compoundName: "label_"
*shipViewLB.x: 184
*shipViewLB.y: 8
*shipViewLB.background: "#9ac0cd"
*shipViewLB.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*shipViewLB.labelString: ""
*shipViewLB.height: 28
*shipViewLB.width: 376
*shipViewLB.leftOffset: 100
*shipViewLB.rightAttachment: "attach_form"
*shipViewLB.rightOffset: 100
*shipViewLB.leftAttachment: "attach_form"

*shipIdSW.class: scrolledWindow
*shipIdSW.static: true
*shipIdSW.name: shipIdSW
*shipIdSW.parent: shipView
*shipIdSW.scrollingPolicy: "application_defined"
*shipIdSW.visualPolicy: "variable"
*shipIdSW.scrollBarDisplayPolicy: "static"
*shipIdSW.shadowThickness: 0
*shipIdSW.isCompound: "true"
*shipIdSW.compoundIcon: "scrllist.xpm"
*shipIdSW.compoundName: "scrolled_List"
*shipIdSW.x: 192
*shipIdSW.y: 76
*shipIdSW.width: 390
*shipIdSW.height: 85
*shipIdSW.background: "LightSkyBlue3"
*shipIdSW.resizable: "false"

*shipIdSL.class: scrolledList
*shipIdSL.static: true
*shipIdSL.name: shipIdSL
*shipIdSL.parent: shipIdSW
*shipIdSL.width: 363
*shipIdSL.height: 200
*shipIdSL.background: "LightSkyBlue3"
*shipIdSL.scrollBarDisplayPolicy: "as_needed"
*shipIdSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shipIdSL.listSizePolicy: "constant"
*shipIdSL.defaultActionCallback.source: public
*shipIdSL.defaultActionCallback: shipView_viewCb
*shipIdSL.selectionPolicy: "browse_select"
*shipIdSL.browseSelectionCallback.source: public
*shipIdSL.browseSelectionCallback: shipView_browseSelectCb

*frame12.class: frame
*frame12.static: true
*frame12.name: frame12
*frame12.parent: shipView
*frame12.width: 748
*frame12.height: 548
*frame12.isCompound: "true"
*frame12.compoundIcon: "frame.xpm"
*frame12.compoundName: "frame_"
*frame12.x: 16
*frame12.y: 172
*frame12.background: "LightSkyBlue3"
*frame12.shadowThickness: 4
*frame12.shadowType: "shadow_etched_out"

*form13.class: form
*form13.static: true
*form13.name: form13
*form13.parent: frame12
*form13.width: 578
*form13.height: 580
*form13.resizePolicy: "resize_none"
*form13.isCompound: "true"
*form13.compoundIcon: "form.xpm"
*form13.compoundName: "form_"
*form13.x: 2
*form13.y: 2
*form13.background: "#9ac0cd"
*form13.noResize: "true"

*label133.class: label
*label133.static: true
*label133.name: label133
*label133.parent: form13
*label133.isCompound: "true"
*label133.compoundIcon: "label.xpm"
*label133.compoundName: "label_"
*label133.x: 36
*label133.y: 16
*label133.background: "SlateGray4"
*label133.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label133.labelString: " Ship To: "
*label133.height: 24
*label133.width: 94
*label133.alignment: "alignment_beginning"
*label133.foreground: "white"

*label134.class: label
*label134.static: true
*label134.name: label134
*label134.parent: form13
*label134.isCompound: "true"
*label134.compoundIcon: "label.xpm"
*label134.compoundName: "label_"
*label134.x: 16
*label134.y: 304
*label134.background: "#9ac0cd"
*label134.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label134.labelString: "Item"
*label134.height: 20
*label134.width: 48
*label134.alignment: "alignment_beginning"
*label134.foreground: "black"

*label136.class: label
*label136.static: true
*label136.name: label136
*label136.parent: form13
*label136.isCompound: "true"
*label136.compoundIcon: "label.xpm"
*label136.compoundName: "label_"
*label136.x: 424
*label136.y: 143
*label136.background: "#9ac0cd"
*label136.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label136.labelString: "Order ID"
*label136.height: 24
*label136.width: 84
*label136.alignment: "alignment_beginning"
*label136.foreground: "black"

*orderIdTF.class: textField
*orderIdTF.static: true
*orderIdTF.name: orderIdTF
*orderIdTF.parent: form13
*orderIdTF.width: 145
*orderIdTF.isCompound: "true"
*orderIdTF.compoundIcon: "textfield.xpm"
*orderIdTF.compoundName: "text_Field"
*orderIdTF.x: 548
*orderIdTF.y: 140
*orderIdTF.background: "LightSkyBlue3"
*orderIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*orderIdTF.text: ""
*orderIdTF.marginHeight: 3
*orderIdTF.height: 30
*orderIdTF.cursorPositionVisible: "false"
*orderIdTF.editable: "false"

*label137.class: label
*label137.static: true
*label137.name: label137
*label137.parent: form13
*label137.isCompound: "true"
*label137.compoundIcon: "label.xpm"
*label137.compoundName: "label_"
*label137.x: 244
*label137.y: 304
*label137.background: "#9ac0cd"
*label137.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label137.labelString: "Name"
*label137.height: 20
*label137.width: 56
*label137.alignment: "alignment_beginning"
*label137.foreground: "black"

*label138.class: label
*label138.static: true
*label138.name: label138
*label138.parent: form13
*label138.isCompound: "true"
*label138.compoundIcon: "label.xpm"
*label138.compoundName: "label_"
*label138.x: 408
*label138.y: 304
*label138.background: "#9ac0cd"
*label138.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label138.labelString: "Media"
*label138.height: 20
*label138.width: 52
*label138.alignment: "alignment_beginning"
*label138.foreground: "black"

*label139.class: label
*label139.static: true
*label139.name: label139
*label139.parent: form13
*label139.isCompound: "true"
*label139.compoundIcon: "label.xpm"
*label139.compoundName: "label_"
*label139.x: 606
*label139.y: 304
*label139.background: "#9ac0cd"
*label139.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label139.labelString: "Qty"
*label139.height: 20
*label139.width: 32
*label139.alignment: "alignment_beginning"
*label139.foreground: "black"

*label145.class: label
*label145.static: true
*label145.name: label145
*label145.parent: form13
*label145.isCompound: "true"
*label145.compoundIcon: "label.xpm"
*label145.compoundName: "label_"
*label145.x: 648
*label145.y: 304
*label145.background: "#9ac0cd"
*label145.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label145.labelString: "Cost"
*label145.height: 20
*label145.width: 40
*label145.alignment: "alignment_beginning"
*label145.foreground: "black"

*label146.class: label
*label146.static: true
*label146.name: label146
*label146.parent: form13
*label146.isCompound: "true"
*label146.compoundIcon: "label.xpm"
*label146.compoundName: "label_"
*label146.x: 40
*label146.y: 179
*label146.background: "#9ac0cd"
*label146.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label146.labelString: "Account ID"
*label146.height: 24
*label146.width: 96
*label146.alignment: "alignment_beginning"
*label146.foreground: "black"

*accountIdTF.class: textField
*accountIdTF.static: true
*accountIdTF.name: accountIdTF
*accountIdTF.parent: form13
*accountIdTF.width: 145
*accountIdTF.isCompound: "true"
*accountIdTF.compoundIcon: "textfield.xpm"
*accountIdTF.compoundName: "text_Field"
*accountIdTF.x: 168
*accountIdTF.y: 176
*accountIdTF.background: "LightSkyBlue3"
*accountIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*accountIdTF.marginHeight: 3
*accountIdTF.height: 30
*accountIdTF.cursorPositionVisible: "false"
*accountIdTF.editable: "false"

*carrierTF.class: textField
*carrierTF.static: true
*carrierTF.name: carrierTF
*carrierTF.parent: form13
*carrierTF.width: 145
*carrierTF.isCompound: "true"
*carrierTF.compoundIcon: "textfield.xpm"
*carrierTF.compoundName: "text_Field"
*carrierTF.x: 548
*carrierTF.y: 176
*carrierTF.background: "LightSkyBlue3"
*carrierTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*carrierTF.height: 30
*carrierTF.marginHeight: 3
*carrierTF.cursorPositionVisible: "false"
*carrierTF.editable: "false"

*label147.class: label
*label147.static: true
*label147.name: label147
*label147.parent: form13
*label147.isCompound: "true"
*label147.compoundIcon: "label.xpm"
*label147.compoundName: "label_"
*label147.x: 424
*label147.y: 215
*label147.background: "#9ac0cd"
*label147.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label147.labelString: "Order Date"
*label147.height: 24
*label147.width: 100
*label147.alignment: "alignment_beginning"
*label147.foreground: "black"

*totalQtyTF.class: textField
*totalQtyTF.static: true
*totalQtyTF.name: totalQtyTF
*totalQtyTF.parent: form13
*totalQtyTF.width: 145
*totalQtyTF.isCompound: "true"
*totalQtyTF.compoundIcon: "textfield.xpm"
*totalQtyTF.compoundName: "text_Field"
*totalQtyTF.x: 168
*totalQtyTF.y: 248
*totalQtyTF.background: "LightSkyBlue3"
*totalQtyTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*totalQtyTF.text: ""
*totalQtyTF.marginHeight: 3
*totalQtyTF.height: 30
*totalQtyTF.cursorPositionVisible: "false"
*totalQtyTF.editable: "false"

*label188.class: label
*label188.static: true
*label188.name: label188
*label188.parent: form13
*label188.isCompound: "true"
*label188.compoundIcon: "label.xpm"
*label188.compoundName: "label_"
*label188.x: 424
*label188.y: 179
*label188.background: "#9ac0cd"
*label188.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label188.labelString: "Ship Via"
*label188.height: 24
*label188.width: 85
*label188.alignment: "alignment_beginning"
*label188.foreground: "black"

*orderDateTF.class: textField
*orderDateTF.static: true
*orderDateTF.name: orderDateTF
*orderDateTF.parent: form13
*orderDateTF.width: 145
*orderDateTF.isCompound: "true"
*orderDateTF.compoundIcon: "textfield.xpm"
*orderDateTF.compoundName: "text_Field"
*orderDateTF.x: 548
*orderDateTF.y: 212
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
*totalCostTF.parent: form13
*totalCostTF.width: 145
*totalCostTF.isCompound: "true"
*totalCostTF.compoundIcon: "textfield.xpm"
*totalCostTF.compoundName: "text_Field"
*totalCostTF.x: 548
*totalCostTF.y: 248
*totalCostTF.background: "LightSkyBlue3"
*totalCostTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*totalCostTF.text: ""
*totalCostTF.marginHeight: 3
*totalCostTF.height: 30
*totalCostTF.cursorPositionVisible: "false"
*totalCostTF.editable: "false"

*label199.class: label
*label199.static: true
*label199.name: label199
*label199.parent: form13
*label199.isCompound: "true"
*label199.compoundIcon: "label.xpm"
*label199.compoundName: "label_"
*label199.x: 68
*label199.y: 304
*label199.background: "#9ac0cd"
*label199.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label199.labelString: "Description"
*label199.height: 20
*label199.width: 100
*label199.alignment: "alignment_beginning"
*label199.foreground: "black"

*shipItemSW.class: scrolledWindow
*shipItemSW.static: true
*shipItemSW.name: shipItemSW
*shipItemSW.parent: form13
*shipItemSW.scrollingPolicy: "application_defined"
*shipItemSW.visualPolicy: "variable"
*shipItemSW.scrollBarDisplayPolicy: "static"
*shipItemSW.shadowThickness: 0
*shipItemSW.isCompound: "true"
*shipItemSW.compoundIcon: "scrllist.xpm"
*shipItemSW.compoundName: "scrolled_List"
*shipItemSW.x: 16
*shipItemSW.y: 324
*shipItemSW.background: "#9ac0cd"
*shipItemSW.width: 580
*shipItemSW.height: 200
*shipItemSW.rightAttachment: "attach_form"
*shipItemSW.rightOffset: 16
*shipItemSW.leftAttachment: "attach_form"
*shipItemSW.leftOffset: 16
*shipItemSW.bottomAttachment: "attach_none"
*shipItemSW.bottomOffset: 0
*shipItemSW.resizable: "false"

*shipItemSL.class: scrolledList
*shipItemSL.static: true
*shipItemSL.name: shipItemSL
*shipItemSL.parent: shipItemSW
*shipItemSL.width: 553
*shipItemSL.height: 290
*shipItemSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shipItemSL.background: "LightSkyBlue3"
*shipItemSL.scrollBarDisplayPolicy: "static"
*shipItemSL.itemCount: 0
*shipItemSL.listSizePolicy: "variable"

*label200.class: label
*label200.static: true
*label200.name: label200
*label200.parent: form13
*label200.isCompound: "true"
*label200.compoundIcon: "label.xpm"
*label200.compoundName: "label_"
*label200.x: 40
*label200.y: 251
*label200.background: "#9ac0cd"
*label200.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label200.labelString: "Total Qty"
*label200.height: 24
*label200.width: 83
*label200.alignment: "alignment_beginning"
*label200.foreground: "black"

*label201.class: label
*label201.static: true
*label201.name: label201
*label201.parent: form13
*label201.isCompound: "true"
*label201.compoundIcon: "label.xpm"
*label201.compoundName: "label_"
*label201.x: 424
*label201.y: 251
*label201.background: "#9ac0cd"
*label201.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label201.labelString: "Total Cost"
*label201.height: 24
*label201.width: 89
*label201.alignment: "alignment_beginning"
*label201.foreground: "black"

*label140.class: label
*label140.static: true
*label140.name: label140
*label140.parent: form13
*label140.isCompound: "true"
*label140.compoundIcon: "label.xpm"
*label140.compoundName: "label_"
*label140.x: 40
*label140.y: 143
*label140.background: "#9ac0cd"
*label140.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label140.labelString: "Shipping ID"
*label140.height: 24
*label140.width: 104
*label140.alignment: "alignment_beginning"
*label140.foreground: "black"

*shippingIdTF.class: textField
*shippingIdTF.static: true
*shippingIdTF.name: shippingIdTF
*shippingIdTF.parent: form13
*shippingIdTF.width: 145
*shippingIdTF.isCompound: "true"
*shippingIdTF.compoundIcon: "textfield.xpm"
*shippingIdTF.compoundName: "text_Field"
*shippingIdTF.x: 168
*shippingIdTF.y: 140
*shippingIdTF.background: "LightSkyBlue3"
*shippingIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shippingIdTF.text: ""
*shippingIdTF.marginHeight: 3
*shippingIdTF.height: 30
*shippingIdTF.cursorPositionVisible: "false"
*shippingIdTF.editable: "false"

*label142.class: label
*label142.static: true
*label142.name: label142
*label142.parent: form13
*label142.isCompound: "true"
*label142.compoundIcon: "label.xpm"
*label142.compoundName: "label_"
*label142.x: 40
*label142.y: 215
*label142.background: "#9ac0cd"
*label142.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label142.labelString: "Ship Date"
*label142.height: 24
*label142.width: 85
*label142.alignment: "alignment_beginning"
*label142.foreground: "black"

*shipDateTF.class: textField
*shipDateTF.static: true
*shipDateTF.name: shipDateTF
*shipDateTF.parent: form13
*shipDateTF.width: 145
*shipDateTF.isCompound: "true"
*shipDateTF.compoundIcon: "textfield.xpm"
*shipDateTF.compoundName: "text_Field"
*shipDateTF.x: 168
*shipDateTF.y: 212
*shipDateTF.background: "LightSkyBlue3"
*shipDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*shipDateTF.text: ""
*shipDateTF.marginHeight: 3
*shipDateTF.height: 30
*shipDateTF.cursorPositionVisible: "false"
*shipDateTF.editable: "false"

*shipToSW.class: scrolledWindow
*shipToSW.static: true
*shipToSW.name: shipToSW
*shipToSW.parent: form13
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
*shipToSW.height: 100

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

*label210.class: label
*label210.static: true
*label210.name: label210
*label210.parent: form13
*label210.isCompound: "true"
*label210.compoundIcon: "label.xpm"
*label210.compoundName: "label_"
*label210.x: 528
*label210.y: 304
*label210.background: "#9ac0cd"
*label210.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label210.labelString: "Status"
*label210.height: 20
*label210.width: 52
*label210.alignment: "alignment_beginning"
*label210.foreground: "black"

*label148.class: label
*label148.static: true
*label148.name: label148
*label148.parent: shipView
*label148.isCompound: "true"
*label148.compoundIcon: "label.xpm"
*label148.compoundName: "label_"
*label148.x: 200
*label148.y: 52
*label148.background: "#9ac0cd"
*label148.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label148.labelString: "Shipping ID"
*label148.height: 24
*label148.width: 104
*label148.alignment: "alignment_beginning"
*label148.foreground: "black"

*label149.class: label
*label149.static: true
*label149.name: label149
*label149.parent: shipView
*label149.isCompound: "true"
*label149.compoundIcon: "label.xpm"
*label149.compoundName: "label_"
*label149.x: 420
*label149.y: 52
*label149.background: "#9ac0cd"
*label149.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label149.labelString: "Ship Date"
*label149.height: 24
*label149.width: 85
*label149.alignment: "alignment_beginning"
*label149.foreground: "black"

*separator29.class: separator
*separator29.static: true
*separator29.name: separator29
*separator29.parent: shipView
*separator29.width: 788
*separator29.height: 12
*separator29.isCompound: "true"
*separator29.compoundIcon: "sep.xpm"
*separator29.compoundName: "separator_"
*separator29.x: 0
*separator29.y: 732
*separator29.background: "#9ac0cd"

*viewPB.class: pushButton
*viewPB.static: true
*viewPB.name: viewPB
*viewPB.parent: shipView
*viewPB.isCompound: "true"
*viewPB.compoundIcon: "push.xpm"
*viewPB.compoundName: "push_Button"
*viewPB.x: 56
*viewPB.y: 76
*viewPB.background: "cadetBlue"
*viewPB.labelPixmap: "/local/imsdads/app-defaults/pixmaps/arrowD"
*viewPB.labelType: "pixmap"
*viewPB.shadowThickness: 4
*viewPB.sensitive: "true"
*viewPB.recomputeSize: "false"
*viewPB.height: 81
*viewPB.width: 81
*viewPB.highlightThickness: 2
*viewPB.activateCallback.source: public
*viewPB.activateCallback: shipView_viewCb

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: shipView
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 628
*printPB.y: 76
*printPB.background: "cadetBlue"
*printPB.labelPixmap: "/local/imsdads/app-defaults/pixmaps/printD"
*printPB.labelType: "pixmap"
*printPB.shadowThickness: 4
*printPB.sensitive: "false"
*printPB.recomputeSize: "false"
*printPB.height: 81
*printPB.width: 81
*printPB.highlightThickness: 2
*printPB.labelString: ""
*printPB.activateCallback.source: public
*printPB.activateCallback: shipping_printFormCb
*printPB.activateCallbackClientData: (XtPointer) 0
*printPB.labelInsensitivePixmap: "/local/imsdads/app-defaults/pixmaps/printer.xpm"
*printPB.marginWidth: 0
*printPB.marginHeight: 0

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: shipView
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 316
*closePB.y: 748
*closePB.background: "cadetBlue"
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.labelString: "CLOSE"
*closePB.width: 158
*closePB.shadowThickness: 4
*closePB.height: 36
*closePB.activateCallback.source: public
*closePB.activateCallback: shipView_closeCb

