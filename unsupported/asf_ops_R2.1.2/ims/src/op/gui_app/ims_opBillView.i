! UIMX ascii 2.9 key: 4461                                                      

*billView.class: form
*billView.classinc:
*billView.classspec:
*billView.classmembers:
*billView.classconstructor:
*billView.classdestructor:
*billView.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*billView.ispecdecl:
*billView.funcdecl: swidget create_billView(swidget UxParent)
*billView.funcname: create_billView
*billView.funcdef: "swidget", "<create_billView>(%)"
*billView.argdecl: swidget UxParent;
*billView.arglist: UxParent
*billView.arglist.UxParent: "swidget", "%UxParent%"
*billView.icode:
*billView.fcode: return(rtrn);\

*billView.auxdecl:
*billView.static: true
*billView.name: billView
*billView.parent: NO_PARENT
*billView.parentExpression: UxParent
*billView.defaultShell: transientShell
*billView.width: 784
*billView.height: 800
*billView.resizePolicy: "resize_none"
*billView.isCompound: "true"
*billView.compoundIcon: "form.xpm"
*billView.compoundName: "form_"
*billView.x: 828
*billView.y: 16
*billView.unitType: "pixels"
*billView.allowShellResize: "false"
*billView.background: "#9ac0cd"

*frame13.class: frame
*frame13.static: true
*frame13.name: frame13
*frame13.parent: billView
*frame13.width: 748
*frame13.height: 560
*frame13.isCompound: "true"
*frame13.compoundIcon: "frame.xpm"
*frame13.compoundName: "frame_"
*frame13.x: 16
*frame13.y: 172
*frame13.background: "LightSkyBlue3"
*frame13.shadowThickness: 4
*frame13.shadowType: "shadow_etched_out"

*form15.class: form
*form15.static: true
*form15.name: form15
*form15.parent: frame13
*form15.width: 578
*form15.height: 580
*form15.resizePolicy: "resize_none"
*form15.isCompound: "true"
*form15.compoundIcon: "form.xpm"
*form15.compoundName: "form_"
*form15.x: 2
*form15.y: 2
*form15.background: "#9ac0cd"

*label135.class: label
*label135.static: true
*label135.name: label135
*label135.parent: form15
*label135.isCompound: "true"
*label135.compoundIcon: "label.xpm"
*label135.compoundName: "label_"
*label135.x: 32
*label135.y: 16
*label135.background: "SlateGray4"
*label135.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label135.labelString: " Bill To: "
*label135.height: 24
*label135.width: 90
*label135.alignment: "alignment_beginning"
*label135.foreground: "white"

*billItemSW.class: scrolledWindow
*billItemSW.static: true
*billItemSW.name: billItemSW
*billItemSW.parent: form15
*billItemSW.scrollingPolicy: "application_defined"
*billItemSW.visualPolicy: "variable"
*billItemSW.scrollBarDisplayPolicy: "static"
*billItemSW.shadowThickness: 0
*billItemSW.isCompound: "true"
*billItemSW.compoundIcon: "scrllist.xpm"
*billItemSW.compoundName: "scrolled_List"
*billItemSW.x: 8
*billItemSW.y: 340
*billItemSW.background: "#9ac0cd"
*billItemSW.width: 580
*billItemSW.height: 200
*billItemSW.leftOffset: 10
*billItemSW.rightAttachment: "attach_form"
*billItemSW.rightOffset: 10
*billItemSW.leftAttachment: "attach_form"

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

*billToSW.class: scrolledWindow
*billToSW.static: true
*billToSW.name: billToSW
*billToSW.parent: form15
*billToSW.scrollingPolicy: "application_defined"
*billToSW.visualPolicy: "variable"
*billToSW.scrollBarDisplayPolicy: "static"
*billToSW.isCompound: "true"
*billToSW.compoundIcon: "scrltext.xpm"
*billToSW.compoundName: "scrolled_Text"
*billToSW.x: 183
*billToSW.y: 12
*billToSW.background: "LightSkyBlue3"
*billToSW.width: 522
*billToSW.height: 100

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

*label141.class: label
*label141.static: true
*label141.name: label141
*label141.parent: form15
*label141.isCompound: "true"
*label141.compoundIcon: "label.xpm"
*label141.compoundName: "label_"
*label141.x: 28
*label141.y: 239
*label141.background: "#9ac0cd"
*label141.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label141.labelString: "Order Date"
*label141.height: 24
*label141.width: 95
*label141.alignment: "alignment_beginning"
*label141.foreground: "black"

*orderDateTF.class: textField
*orderDateTF.static: true
*orderDateTF.name: orderDateTF
*orderDateTF.parent: form15
*orderDateTF.width: 145
*orderDateTF.isCompound: "true"
*orderDateTF.compoundIcon: "textfield.xpm"
*orderDateTF.compoundName: "text_Field"
*orderDateTF.x: 180
*orderDateTF.y: 236
*orderDateTF.background: "LightSkyBlue3"
*orderDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*orderDateTF.marginHeight: 3
*orderDateTF.cursorPositionVisible: "false"
*orderDateTF.editable: "false"

*label143.class: label
*label143.static: true
*label143.name: label143
*label143.parent: form15
*label143.isCompound: "true"
*label143.compoundIcon: "label.xpm"
*label143.compoundName: "label_"
*label143.x: 28
*label143.y: 176
*label143.background: "#9ac0cd"
*label143.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label143.labelString: "Account ID"
*label143.height: 24
*label143.width: 96
*label143.alignment: "alignment_beginning"
*label143.foreground: "black"

*label144.class: label
*label144.static: true
*label144.name: label144
*label144.parent: form15
*label144.isCompound: "true"
*label144.compoundIcon: "label.xpm"
*label144.compoundName: "label_"
*label144.x: 28
*label144.y: 144
*label144.background: "#9ac0cd"
*label144.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label144.labelString: "Invoice ID"
*label144.height: 24
*label144.width: 92
*label144.alignment: "alignment_beginning"
*label144.foreground: "black"

*label158.class: label
*label158.static: true
*label158.name: label158
*label158.parent: form15
*label158.isCompound: "true"
*label158.compoundIcon: "label.xpm"
*label158.compoundName: "label_"
*label158.x: 416
*label158.y: 143
*label158.background: "#9ac0cd"
*label158.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label158.labelString: "Invoice Amount"
*label158.height: 24
*label158.width: 144
*label158.alignment: "alignment_beginning"
*label158.foreground: "black"

*label159.class: label
*label159.static: true
*label159.name: label159
*label159.parent: form15
*label159.isCompound: "true"
*label159.compoundIcon: "label.xpm"
*label159.compoundName: "label_"
*label159.x: 416
*label159.y: 175
*label159.background: "#9ac0cd"
*label159.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label159.labelString: "Resource Type"
*label159.height: 24
*label159.width: 128
*label159.alignment: "alignment_beginning"
*label159.foreground: "black"

*label165.class: label
*label165.static: true
*label165.name: label165
*label165.parent: form15
*label165.isCompound: "true"
*label165.compoundIcon: "label.xpm"
*label165.compoundName: "label_"
*label165.x: 416
*label165.y: 238
*label165.background: "#9ac0cd"
*label165.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label165.labelString: "Invoice Date"
*label165.height: 24
*label165.width: 112
*label165.alignment: "alignment_beginning"
*label165.foreground: "black"

*label166.class: label
*label166.static: true
*label166.name: label166
*label166.parent: form15
*label166.isCompound: "true"
*label166.compoundIcon: "label.xpm"
*label166.compoundName: "label_"
*label166.x: 416
*label166.y: 207
*label166.background: "#9ac0cd"
*label166.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label166.labelString: "Order ID"
*label166.height: 24
*label166.width: 84
*label166.alignment: "alignment_beginning"
*label166.foreground: "black"

*invoiceIdTF.class: textField
*invoiceIdTF.static: true
*invoiceIdTF.name: invoiceIdTF
*invoiceIdTF.parent: form15
*invoiceIdTF.width: 145
*invoiceIdTF.isCompound: "true"
*invoiceIdTF.compoundIcon: "textfield.xpm"
*invoiceIdTF.compoundName: "text_Field"
*invoiceIdTF.x: 180
*invoiceIdTF.y: 140
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
*accountIdTF.parent: form15
*accountIdTF.width: 145
*accountIdTF.isCompound: "true"
*accountIdTF.compoundIcon: "textfield.xpm"
*accountIdTF.compoundName: "text_Field"
*accountIdTF.x: 180
*accountIdTF.y: 172
*accountIdTF.background: "LightSkyBlue3"
*accountIdTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*accountIdTF.marginHeight: 3
*accountIdTF.height: 30
*accountIdTF.cursorPositionVisible: "false"
*accountIdTF.editable: "false"

*amountTF.class: textField
*amountTF.static: true
*amountTF.name: amountTF
*amountTF.parent: form15
*amountTF.width: 145
*amountTF.isCompound: "true"
*amountTF.compoundIcon: "textfield.xpm"
*amountTF.compoundName: "text_Field"
*amountTF.x: 564
*amountTF.y: 140
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
*orderIdTF.parent: form15
*orderIdTF.width: 145
*orderIdTF.isCompound: "true"
*orderIdTF.compoundIcon: "textfield.xpm"
*orderIdTF.compoundName: "text_Field"
*orderIdTF.x: 564
*orderIdTF.y: 204
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
*balanceTF.parent: form15
*balanceTF.width: 145
*balanceTF.isCompound: "true"
*balanceTF.compoundIcon: "textfield.xpm"
*balanceTF.compoundName: "text_Field"
*balanceTF.x: 180
*balanceTF.y: 204
*balanceTF.background: "LightSkyBlue3"
*balanceTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*balanceTF.marginHeight: 3
*balanceTF.height: 30
*balanceTF.cursorPositionVisible: "false"
*balanceTF.editable: "false"

*billDateTF.class: textField
*billDateTF.static: true
*billDateTF.name: billDateTF
*billDateTF.parent: form15
*billDateTF.width: 145
*billDateTF.isCompound: "true"
*billDateTF.compoundIcon: "textfield.xpm"
*billDateTF.compoundName: "text_Field"
*billDateTF.x: 564
*billDateTF.y: 235
*billDateTF.background: "LightSkyBlue3"
*billDateTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*billDateTF.marginHeight: 3
*billDateTF.height: 30
*billDateTF.cursorPositionVisible: "false"
*billDateTF.editable: "false"

*label207.class: label
*label207.static: true
*label207.name: label207
*label207.parent: form15
*label207.isCompound: "true"
*label207.compoundIcon: "label.xpm"
*label207.compoundName: "label_"
*label207.x: 28
*label207.y: 208
*label207.background: "#9ac0cd"
*label207.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label207.labelString: "Account Balance"
*label207.height: 24
*label207.width: 140
*label207.alignment: "alignment_beginning"
*label207.foreground: "black"

*resourceTF.class: textField
*resourceTF.static: true
*resourceTF.name: resourceTF
*resourceTF.parent: form15
*resourceTF.width: 145
*resourceTF.isCompound: "true"
*resourceTF.compoundIcon: "textfield.xpm"
*resourceTF.compoundName: "text_Field"
*resourceTF.x: 564
*resourceTF.y: 172
*resourceTF.background: "LightSkyBlue3"
*resourceTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*resourceTF.marginHeight: 3
*resourceTF.height: 30
*resourceTF.cursorPositionVisible: "false"
*resourceTF.editable: "false"

*label208.class: label
*label208.static: true
*label208.name: label208
*label208.parent: form15
*label208.isCompound: "true"
*label208.compoundIcon: "label.xpm"
*label208.compoundName: "label_"
*label208.x: 28
*label208.y: 271
*label208.background: "#9ac0cd"
*label208.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label208.labelString: "User Name"
*label208.height: 24
*label208.width: 95
*label208.alignment: "alignment_beginning"
*label208.foreground: "black"

*userNameTF.class: textField
*userNameTF.static: true
*userNameTF.name: userNameTF
*userNameTF.parent: form15
*userNameTF.width: 531
*userNameTF.isCompound: "true"
*userNameTF.compoundIcon: "textfield.xpm"
*userNameTF.compoundName: "text_Field"
*userNameTF.x: 180
*userNameTF.y: 268
*userNameTF.background: "LightSkyBlue3"
*userNameTF.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*userNameTF.marginHeight: 3
*userNameTF.cursorPositionVisible: "false"
*userNameTF.editable: "false"

*label172.class: label
*label172.static: true
*label172.name: label172
*label172.parent: form15
*label172.isCompound: "true"
*label172.compoundIcon: "label.xpm"
*label172.compoundName: "label_"
*label172.x: 10
*label172.y: 316
*label172.background: "#9ac0cd"
*label172.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label172.labelString: "Item"
*label172.height: 20
*label172.width: 48
*label172.alignment: "alignment_beginning"
*label172.foreground: "black"

*label173.class: label
*label173.static: true
*label173.name: label173
*label173.parent: form15
*label173.isCompound: "true"
*label173.compoundIcon: "label.xpm"
*label173.compoundName: "label_"
*label173.x: 62
*label173.y: 316
*label173.background: "#9ac0cd"
*label173.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label173.labelString: "Description"
*label173.height: 20
*label173.width: 100
*label173.alignment: "alignment_beginning"
*label173.foreground: "black"

*label179.class: label
*label179.static: true
*label179.name: label179
*label179.parent: form15
*label179.isCompound: "true"
*label179.compoundIcon: "label.xpm"
*label179.compoundName: "label_"
*label179.x: 240
*label179.y: 316
*label179.background: "#9ac0cd"
*label179.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label179.labelString: "Name"
*label179.height: 20
*label179.width: 98
*label179.alignment: "alignment_beginning"
*label179.foreground: "black"

*label187.class: label
*label187.static: true
*label187.name: label187
*label187.parent: form15
*label187.isCompound: "true"
*label187.compoundIcon: "label.xpm"
*label187.compoundName: "label_"
*label187.x: 408
*label187.y: 316
*label187.background: "#9ac0cd"
*label187.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label187.labelString: "Media"
*label187.height: 20
*label187.width: 50
*label187.alignment: "alignment_beginning"
*label187.foreground: "black"

*label197.class: label
*label197.static: true
*label197.name: label197
*label197.parent: form15
*label197.isCompound: "true"
*label197.compoundIcon: "label.xpm"
*label197.compoundName: "label_"
*label197.x: 528
*label197.y: 316
*label197.background: "#9ac0cd"
*label197.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label197.labelString: "Status"
*label197.height: 20
*label197.width: 52
*label197.alignment: "alignment_beginning"
*label197.foreground: "black"

*label198.class: label
*label198.static: true
*label198.name: label198
*label198.parent: form15
*label198.isCompound: "true"
*label198.compoundIcon: "label.xpm"
*label198.compoundName: "label_"
*label198.x: 606
*label198.y: 316
*label198.background: "#9ac0cd"
*label198.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label198.labelString: "Qty"
*label198.height: 20
*label198.width: 32
*label198.alignment: "alignment_beginning"
*label198.foreground: "black"

*label202.class: label
*label202.static: true
*label202.name: label202
*label202.parent: form15
*label202.isCompound: "true"
*label202.compoundIcon: "label.xpm"
*label202.compoundName: "label_"
*label202.x: 654
*label202.y: 316
*label202.background: "#9ac0cd"
*label202.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label202.labelString: "Cost"
*label202.height: 20
*label202.width: 40
*label202.alignment: "alignment_beginning"
*label202.foreground: "black"

*label180.class: label
*label180.static: true
*label180.name: label180
*label180.parent: billView
*label180.isCompound: "true"
*label180.compoundIcon: "label.xpm"
*label180.compoundName: "label_"
*label180.x: 208
*label180.y: 52
*label180.background: "#9ac0cd"
*label180.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label180.labelString: "Invoice ID"
*label180.height: 24
*label180.width: 104
*label180.alignment: "alignment_beginning"
*label180.foreground: "black"

*label186.class: label
*label186.static: true
*label186.name: label186
*label186.parent: billView
*label186.isCompound: "true"
*label186.compoundIcon: "label.xpm"
*label186.compoundName: "label_"
*label186.x: 424
*label186.y: 52
*label186.background: "#9ac0cd"
*label186.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label186.labelString: "Bill Date"
*label186.height: 24
*label186.width: 85
*label186.alignment: "alignment_beginning"
*label186.foreground: "black"

*viewPB.class: pushButton
*viewPB.static: true
*viewPB.name: viewPB
*viewPB.parent: billView
*viewPB.isCompound: "true"
*viewPB.compoundIcon: "push.xpm"
*viewPB.compoundName: "push_Button"
*viewPB.x: 60
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
*viewPB.activateCallback: billView_viewCb

*printPB.class: pushButton
*printPB.static: true
*printPB.name: printPB
*printPB.parent: billView
*printPB.isCompound: "true"
*printPB.compoundIcon: "push.xpm"
*printPB.compoundName: "push_Button"
*printPB.x: 644
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
*printPB.activateCallback: billing_printFormCb
*printPB.activateCallbackClientData: (XtPointer) 0
*printPB.labelInsensitivePixmap: "/local/imsdads/app-defaults/pixmaps/printer.xpm"
*printPB.marginHeight: 0
*printPB.marginWidth: 0

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: billView
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 308
*closePB.y: 752
*closePB.background: "cadetBlue"
*closePB.fontList: "-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1"
*closePB.labelString: "CLOSE"
*closePB.width: 158
*closePB.shadowThickness: 4
*closePB.height: 36
*closePB.activateCallback.source: public
*closePB.activateCallback: billView_closeCb

*billViewLB.class: label
*billViewLB.static: true
*billViewLB.name: billViewLB
*billViewLB.parent: billView
*billViewLB.isCompound: "true"
*billViewLB.compoundIcon: "label.xpm"
*billViewLB.compoundName: "label_"
*billViewLB.x: 232
*billViewLB.y: 8
*billViewLB.background: "#9ac0cd"
*billViewLB.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*billViewLB.labelString: ""
*billViewLB.height: 28
*billViewLB.width: 376
*billViewLB.leftOffset: 100
*billViewLB.rightAttachment: "attach_form"
*billViewLB.rightOffset: 100
*billViewLB.leftAttachment: "attach_form"

*separator33.class: separator
*separator33.static: true
*separator33.name: separator33
*separator33.parent: billView
*separator33.width: 788
*separator33.height: 12
*separator33.isCompound: "true"
*separator33.compoundIcon: "sep.xpm"
*separator33.compoundName: "separator_"
*separator33.x: 0
*separator33.y: 736
*separator33.background: "#9ac0cd"

*billIdSW.class: scrolledWindow
*billIdSW.static: true
*billIdSW.name: billIdSW
*billIdSW.parent: billView
*billIdSW.scrollingPolicy: "application_defined"
*billIdSW.visualPolicy: "variable"
*billIdSW.scrollBarDisplayPolicy: "static"
*billIdSW.shadowThickness: 0
*billIdSW.isCompound: "true"
*billIdSW.compoundIcon: "scrllist.xpm"
*billIdSW.compoundName: "scrolled_List"
*billIdSW.x: 204
*billIdSW.y: 76
*billIdSW.width: 390
*billIdSW.height: 85
*billIdSW.background: "LightSkyBlue3"

*billIdSL.class: scrolledList
*billIdSL.static: true
*billIdSL.name: billIdSL
*billIdSL.parent: billIdSW
*billIdSL.width: 363
*billIdSL.height: 200
*billIdSL.background: "LightSkyBlue3"
*billIdSL.scrollBarDisplayPolicy: "as_needed"
*billIdSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*billIdSL.listSizePolicy: "constant"
*billIdSL.defaultActionCallback.source: public
*billIdSL.defaultActionCallback: billView_viewCb
*billIdSL.selectionPolicy: "browse_select"
*billIdSL.browseSelectionCallback.source: public
*billIdSL.browseSelectionCallback: billView_browseSelectCb

