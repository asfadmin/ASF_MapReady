! UIMX ascii 2.9 key: 6184                                                      

*selectionBoxDlg.class: selectionBoxDialog
*selectionBoxDlg.classinc:
*selectionBoxDlg.classspec:
*selectionBoxDlg.classmembers:
*selectionBoxDlg.classconstructor:
*selectionBoxDlg.classdestructor:
*selectionBoxDlg.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*selectionBoxDlg.ispecdecl:
*selectionBoxDlg.funcdecl: swidget create_selectionBoxDlg(swidget UxParent)
*selectionBoxDlg.funcname: create_selectionBoxDlg
*selectionBoxDlg.funcdef: "swidget", "<create_selectionBoxDlg>(%)"
*selectionBoxDlg.argdecl: swidget UxParent;
*selectionBoxDlg.arglist: UxParent
*selectionBoxDlg.arglist.UxParent: "swidget", "%UxParent%"
*selectionBoxDlg.icode:
*selectionBoxDlg.fcode: return(rtrn);\

*selectionBoxDlg.auxdecl:
*selectionBoxDlg.static: true
*selectionBoxDlg.name: selectionBoxDlg
*selectionBoxDlg.parent: NO_PARENT
*selectionBoxDlg.parentExpression: UxParent
*selectionBoxDlg.defaultShell: transientShell
*selectionBoxDlg.width: 324
*selectionBoxDlg.height: 380
*selectionBoxDlg.dialogType: "dialog_selection"
*selectionBoxDlg.isCompound: "true"
*selectionBoxDlg.compoundIcon: "selectD.xpm"
*selectionBoxDlg.compoundName: "selBox_Dialog"
*selectionBoxDlg.x: 414
*selectionBoxDlg.y: 216
*selectionBoxDlg.unitType: "pixels"
*selectionBoxDlg.allowShellResize: "true"
*selectionBoxDlg.background: "#9ac0cd"
*selectionBoxDlg.buttonFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*selectionBoxDlg.labelFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*selectionBoxDlg.textFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*selectionBoxDlg.shadowThickness: 2
*selectionBoxDlg.applyLabelString: "Apply"
*selectionBoxDlg.childPlacement: "place_below_selection"

*commentFormW.class: form
*commentFormW.static: true
*commentFormW.name: commentFormW
*commentFormW.parent: selectionBoxDlg
*commentFormW.width: 300
*commentFormW.height: 112
*commentFormW.resizePolicy: "resize_none"
*commentFormW.isCompound: "true"
*commentFormW.compoundIcon: "form.xpm"
*commentFormW.compoundName: "form_"
*commentFormW.x: 12
*commentFormW.y: 196
*commentFormW.background: "#9ac0cd"

*label37.class: label
*label37.static: true
*label37.name: label37
*label37.parent: commentFormW
*label37.isCompound: "true"
*label37.compoundIcon: "label.xpm"
*label37.compoundName: "label_"
*label37.x: 0
*label37.y: 4
*label37.background: "#9ac0cd"
*label37.labelString: "Edit Comments"

*selection_commentSW.class: scrolledWindow
*selection_commentSW.static: true
*selection_commentSW.name: selection_commentSW
*selection_commentSW.parent: commentFormW
*selection_commentSW.scrollingPolicy: "application_defined"
*selection_commentSW.visualPolicy: "variable"
*selection_commentSW.scrollBarDisplayPolicy: "static"
*selection_commentSW.isCompound: "true"
*selection_commentSW.compoundIcon: "scrltext.xpm"
*selection_commentSW.compoundName: "scrolled_Text"
*selection_commentSW.x: 0
*selection_commentSW.y: 28
*selection_commentSW.background: "#9ac0cd"
*selection_commentSW.width: 300
*selection_commentSW.height: 80

*selection_commentST.class: scrolledText
*selection_commentST.static: true
*selection_commentST.name: selection_commentST
*selection_commentST.parent: selection_commentSW
*selection_commentST.width: 281
*selection_commentST.height: 76
*selection_commentST.background: "#9ac0cd"
*selection_commentST.editMode: "multi_line_edit"
*selection_commentST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*selection_commentST.maxLength: 250
*selection_commentST.wordWrap: "true"
*selection_commentST.scrollHorizontal: "false"
*selection_commentST.x: 12
*selection_commentST.y: 0

