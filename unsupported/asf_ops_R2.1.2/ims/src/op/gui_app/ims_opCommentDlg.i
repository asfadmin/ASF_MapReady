! UIMX ascii 2.9 key: 4797                                                      

*commentDlg.class: formDialog
*commentDlg.classinc:
*commentDlg.classspec:
*commentDlg.classmembers:
*commentDlg.classconstructor:
*commentDlg.classdestructor:
*commentDlg.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*commentDlg.ispecdecl:
*commentDlg.funcdecl: swidget create_commentDlg(swidget UxParent)
*commentDlg.funcname: create_commentDlg
*commentDlg.funcdef: "swidget", "<create_commentDlg>(%)"
*commentDlg.argdecl: swidget UxParent;
*commentDlg.arglist: UxParent
*commentDlg.arglist.UxParent: "swidget", "%UxParent%"
*commentDlg.icode:
*commentDlg.fcode: return(rtrn);\

*commentDlg.auxdecl:
*commentDlg.static: true
*commentDlg.name: commentDlg
*commentDlg.parent: NO_PARENT
*commentDlg.parentExpression: UxParent
*commentDlg.defaultShell: transientShell
*commentDlg.width: 493
*commentDlg.height: 470
*commentDlg.isCompound: "true"
*commentDlg.compoundIcon: "formD.xpm"
*commentDlg.compoundName: "form_Dialog"
*commentDlg.x: 326
*commentDlg.y: 200
*commentDlg.unitType: "pixels"
*commentDlg.allowShellResize: "false"
*commentDlg.background: "#9ac0cd"
*commentDlg.dialogStyle: "dialog_full_application_modal"
*commentDlg.buttonFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentDlg.cancelButton: "commentCancelPB"
*commentDlg.defaultButton: "commentUpdatePB"

*commentUpdatePB.class: pushButton
*commentUpdatePB.static: true
*commentUpdatePB.name: commentUpdatePB
*commentUpdatePB.parent: commentDlg
*commentUpdatePB.isCompound: "true"
*commentUpdatePB.compoundIcon: "push.xpm"
*commentUpdatePB.compoundName: "push_Button"
*commentUpdatePB.x: 49
*commentUpdatePB.y: 376
*commentUpdatePB.background: "cadetBlue"
*commentUpdatePB.height: 35
*commentUpdatePB.width: 108
*commentUpdatePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentUpdatePB.labelString: "Update"
*commentUpdatePB.shadowThickness: 3
*commentUpdatePB.leftOffset: 40
*commentUpdatePB.bottomAttachment: "attach_form"
*commentUpdatePB.bottomOffset: 12
*commentUpdatePB.activateCallback.source: public
*commentUpdatePB.activateCallback: commentDlg_updateCb

*commentCancelPB.class: pushButton
*commentCancelPB.static: true
*commentCancelPB.name: commentCancelPB
*commentCancelPB.parent: commentDlg
*commentCancelPB.isCompound: "true"
*commentCancelPB.compoundIcon: "push.xpm"
*commentCancelPB.compoundName: "push_Button"
*commentCancelPB.x: 332
*commentCancelPB.y: 376
*commentCancelPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentCancelPB.height: 35
*commentCancelPB.labelString: "Cancel"
*commentCancelPB.rightAttachment: "attach_form"
*commentCancelPB.rightOffset: 40
*commentCancelPB.shadowThickness: 3
*commentCancelPB.width: 108
*commentCancelPB.background: "cadetBlue"
*commentCancelPB.bottomAttachment: "attach_form"
*commentCancelPB.bottomOffset: 12
*commentCancelPB.activateCallback.source: public
*commentCancelPB.activateCallback: commentDlg_cancelCb

*commentSW.class: scrolledWindow
*commentSW.static: true
*commentSW.name: commentSW
*commentSW.parent: commentDlg
*commentSW.scrollingPolicy: "application_defined"
*commentSW.visualPolicy: "variable"
*commentSW.scrollBarDisplayPolicy: "static"
*commentSW.isCompound: "true"
*commentSW.compoundIcon: "scrltext.xpm"
*commentSW.compoundName: "scrolled_Text"
*commentSW.x: 12
*commentSW.y: 280
*commentSW.background: "LightSkyBlue3"
*commentSW.width: 470

*commentST.class: scrolledText
*commentST.static: true
*commentST.name: commentST
*commentST.parent: commentSW
*commentST.width: 451
*commentST.height: 116
*commentST.background: "LightSkyBlue3"
*commentST.editMode: "multi_line_edit"
*commentST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentST.maxLength: 250
*commentST.wordWrap: "true"
*commentST.scrollHorizontal: "false"
*commentST.x: 12
*commentST.y: 0

*commentLB.class: label
*commentLB.static: true
*commentLB.name: commentLB
*commentLB.parent: commentDlg
*commentLB.isCompound: "true"
*commentLB.compoundIcon: "label.xpm"
*commentLB.compoundName: "label_"
*commentLB.x: 8
*commentLB.y: 44
*commentLB.background: "#9ac0cd"
*commentLB.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*commentLB.leftOffset: 10
*commentLB.rightAttachment: "attach_form"
*commentLB.rightOffset: 10
*commentLB.leftAttachment: "attach_form"
*commentLB.labelString: "commentLB"

*commentOldSW.class: scrolledWindow
*commentOldSW.static: true
*commentOldSW.name: commentOldSW
*commentOldSW.parent: commentDlg
*commentOldSW.scrollingPolicy: "application_defined"
*commentOldSW.visualPolicy: "variable"
*commentOldSW.scrollBarDisplayPolicy: "static"
*commentOldSW.isCompound: "true"
*commentOldSW.compoundIcon: "scrltext.xpm"
*commentOldSW.compoundName: "scrolled_Text"
*commentOldSW.x: 12
*commentOldSW.y: 116
*commentOldSW.background: "LightSkyBlue3"
*commentOldSW.width: 470
*commentOldSW.height: 116

*commentOldST.class: scrolledText
*commentOldST.static: true
*commentOldST.name: commentOldST
*commentOldST.parent: commentOldSW
*commentOldST.width: 451
*commentOldST.height: 76
*commentOldST.background: "LightSkyBlue3"
*commentOldST.editMode: "multi_line_edit"
*commentOldST.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentOldST.maxLength: 250
*commentOldST.wordWrap: "true"
*commentOldST.scrollHorizontal: "false"
*commentOldST.x: 12
*commentOldST.y: 0
*commentOldST.cursorPositionVisible: "false"
*commentOldST.editable: "false"

*commentOldLB.class: label
*commentOldLB.static: true
*commentOldLB.name: commentOldLB
*commentOldLB.parent: commentDlg
*commentOldLB.isCompound: "true"
*commentOldLB.compoundIcon: "label.xpm"
*commentOldLB.compoundName: "label_"
*commentOldLB.x: 12
*commentOldLB.y: 92
*commentOldLB.background: "#9ac0cd"
*commentOldLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentOldLB.labelString: "Existing Comment:"

*commentNewLB.class: label
*commentNewLB.static: true
*commentNewLB.name: commentNewLB
*commentNewLB.parent: commentDlg
*commentNewLB.isCompound: "true"
*commentNewLB.compoundIcon: "label.xpm"
*commentNewLB.compoundName: "label_"
*commentNewLB.x: 12
*commentNewLB.y: 256
*commentNewLB.background: "#9ac0cd"
*commentNewLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*commentNewLB.labelString: "Enter New Comment:"

*commentLB1.class: label
*commentLB1.static: true
*commentLB1.name: commentLB1
*commentLB1.parent: commentDlg
*commentLB1.isCompound: "true"
*commentLB1.compoundIcon: "label.xpm"
*commentLB1.compoundName: "label_"
*commentLB1.x: 184
*commentLB1.y: 12
*commentLB1.background: "#9ac0cd"
*commentLB1.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*commentLB1.labelString: "Edit   Comment"

