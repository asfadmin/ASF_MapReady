! UIMX ascii 2.9 key: 5216                                                      

*browseDlg.class: formDialog
*browseDlg.classinc:
*browseDlg.classspec:
*browseDlg.classmembers:
*browseDlg.classconstructor:
*browseDlg.classdestructor:
*browseDlg.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*browseDlg.ispecdecl:
*browseDlg.funcdecl: swidget create_browseDlg(swidget UxParent)
*browseDlg.funcname: create_browseDlg
*browseDlg.funcdef: "swidget", "<create_browseDlg>(%)"
*browseDlg.argdecl: swidget UxParent;
*browseDlg.arglist: UxParent
*browseDlg.arglist.UxParent: "swidget", "%UxParent%"
*browseDlg.icode:
*browseDlg.fcode: return(rtrn);\

*browseDlg.auxdecl:
*browseDlg.static: true
*browseDlg.name: browseDlg
*browseDlg.parent: NO_PARENT
*browseDlg.parentExpression: UxParent
*browseDlg.defaultShell: transientShell
*browseDlg.width: 600
*browseDlg.height: 510
*browseDlg.isCompound: "true"
*browseDlg.compoundIcon: "formD.xpm"
*browseDlg.compoundName: "form_Dialog"
*browseDlg.x: 298
*browseDlg.y: 283
*browseDlg.unitType: "pixels"
*browseDlg.allowShellResize: "false"
*browseDlg.background: "#9ac0cd"

*browseSW.class: scrolledWindow
*browseSW.static: true
*browseSW.name: browseSW
*browseSW.parent: browseDlg
*browseSW.scrollingPolicy: "application_defined"
*browseSW.visualPolicy: "variable"
*browseSW.scrollBarDisplayPolicy: "static"
*browseSW.isCompound: "true"
*browseSW.compoundIcon: "scrltext.xpm"
*browseSW.compoundName: "scrolled_Text"
*browseSW.x: 8
*browseSW.y: 48
*browseSW.background: "LightSkyBlue3"
*browseSW.leftOffset: 10
*browseSW.rightAttachment: "attach_form"
*browseSW.rightOffset: 10
*browseSW.leftAttachment: "attach_form"
*browseSW.height: 390
*browseSW.width: 450

*browseST.class: scrolledText
*browseST.static: true
*browseST.name: browseST
*browseST.parent: browseSW
*browseST.height: 352
*browseST.background: "LightSkyBlue3"
*browseST.scrollHorizontal: "true"
*browseST.scrollVertical: "true"
*browseST.editMode: "multi_line_edit"
*browseST.editable: "false"
*browseST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*browseST.cursorPositionVisible: "false"
*browseST.text: ""

*browseDonePB.class: pushButton
*browseDonePB.static: true
*browseDonePB.name: browseDonePB
*browseDonePB.parent: browseDlg
*browseDonePB.isCompound: "true"
*browseDonePB.compoundIcon: "push.xpm"
*browseDonePB.compoundName: "push_Button"
*browseDonePB.x: 232
*browseDonePB.y: 456
*browseDonePB.width: 118
*browseDonePB.height: 35
*browseDonePB.background: "cadetBlue"
*browseDonePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*browseDonePB.labelString: "Done"
*browseDonePB.shadowThickness: 3
*browseDonePB.activateCallback.source: public
*browseDonePB.activateCallback: browseDlg_okCb

*browseDlgLB.class: label
*browseDlgLB.static: true
*browseDlgLB.name: browseDlgLB
*browseDlgLB.parent: browseDlg
*browseDlgLB.isCompound: "true"
*browseDlgLB.compoundIcon: "label.xpm"
*browseDlgLB.compoundName: "label_"
*browseDlgLB.x: 180
*browseDlgLB.y: 12
*browseDlgLB.background: "#9ac0cd"
*browseDlgLB.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*browseDlgLB.rightAttachment: "attach_form"
*browseDlgLB.leftAttachment: "attach_form"
*browseDlgLB.leftOffset: 70
*browseDlgLB.rightOffset: 70

