! UIMX ascii 2.9 key: 6745                                                      

*srchSelDlg.class: selectionBoxDialog
*srchSelDlg.classinc:
*srchSelDlg.classspec:
*srchSelDlg.classmembers:
*srchSelDlg.classconstructor:
*srchSelDlg.classdestructor:
*srchSelDlg.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*srchSelDlg.ispecdecl:
*srchSelDlg.funcdecl: swidget create_srchSelDlg(swidget UxParent)
*srchSelDlg.funcname: create_srchSelDlg
*srchSelDlg.funcdef: "swidget", "<create_srchSelDlg>(%)"
*srchSelDlg.argdecl: swidget UxParent;
*srchSelDlg.arglist: UxParent
*srchSelDlg.arglist.UxParent: "swidget", "%UxParent%"
*srchSelDlg.icode:
*srchSelDlg.fcode: return(rtrn);\

*srchSelDlg.auxdecl:
*srchSelDlg.static: true
*srchSelDlg.name: srchSelDlg
*srchSelDlg.parent: NO_PARENT
*srchSelDlg.parentExpression: UxParent
*srchSelDlg.defaultShell: transientShell
*srchSelDlg.width: 350
*srchSelDlg.height: 520
*srchSelDlg.dialogType: "dialog_selection"
*srchSelDlg.isCompound: "true"
*srchSelDlg.compoundIcon: "selectD.xpm"
*srchSelDlg.compoundName: "selBox_Dialog"
*srchSelDlg.x: 401
*srchSelDlg.y: 196
*srchSelDlg.unitType: "pixels"
*srchSelDlg.allowShellResize: "false"
*srchSelDlg.background: "#9ac0cd"
*srchSelDlg.buttonFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchSelDlg.dialogStyle: "dialog_full_application_modal"
*srchSelDlg.labelFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchSelDlg.textFontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*srchSelDlg.applyLabelString: "Filter"
*srchSelDlg.childPlacement: "place_below_selection"
*srchSelDlg.mustMatch: "true"
*srchSelDlg.initialFocus: "filterTF"
*srchSelDlg.applyCallback.source: public
*srchSelDlg.applyCallback: srchSelDlg_filterCb
*srchSelDlg.okCallback.source: public
*srchSelDlg.okCallback: srchSelDlg_okCb

*filterForm.class: form
*filterForm.static: true
*filterForm.name: filterForm
*filterForm.parent: srchSelDlg
*filterForm.width: 328
*filterForm.height: 85
*filterForm.resizePolicy: "resize_none"
*filterForm.isCompound: "true"
*filterForm.compoundIcon: "form.xpm"
*filterForm.compoundName: "form_"
*filterForm.x: 11
*filterForm.y: 227
*filterForm.background: "#9ac0cd"

*filterLB.class: label
*filterLB.static: true
*filterLB.name: filterLB
*filterLB.parent: filterForm
*filterLB.isCompound: "true"
*filterLB.compoundIcon: "label.xpm"
*filterLB.compoundName: "label_"
*filterLB.x: 4
*filterLB.y: 16
*filterLB.background: "#9ac0cd"
*filterLB.labelString: "Name Filter: [A-Z*]"

*filterTF.class: textField
*filterTF.static: true
*filterTF.name: filterTF
*filterTF.parent: filterForm
*filterTF.width: 328
*filterTF.isCompound: "true"
*filterTF.compoundIcon: "textfield.xpm"
*filterTF.compoundName: "text_Field"
*filterTF.x: 0
*filterTF.y: 36
*filterTF.background: "#9ac0cd"

