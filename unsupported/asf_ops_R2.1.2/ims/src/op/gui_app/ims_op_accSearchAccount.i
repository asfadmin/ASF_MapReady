! UIMX ascii 2.9 key: 9795                                                      

*search_accounts.class: form
*search_accounts.classinc:
*search_accounts.classspec:
*search_accounts.classmembers:
*search_accounts.classconstructor:
*search_accounts.classdestructor:
*search_accounts.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>\

*search_accounts.ispecdecl:
*search_accounts.funcdecl: swidget create_search_accounts(swidget UxParent)
*search_accounts.funcname: create_search_accounts
*search_accounts.funcdef: "swidget", "<create_search_accounts>(%)"
*search_accounts.argdecl: swidget UxParent;
*search_accounts.arglist: UxParent
*search_accounts.arglist.UxParent: "swidget", "%UxParent%"
*search_accounts.icode:
*search_accounts.fcode: return(rtrn);\

*search_accounts.auxdecl:
*search_accounts.name.source: public
*search_accounts.static: false
*search_accounts.name: search_accounts
*search_accounts.parent: NO_PARENT
*search_accounts.parentExpression: UxParent
*search_accounts.defaultShell: transientShell
*search_accounts.width: 585
*search_accounts.height: 612
*search_accounts.resizePolicy: "resize_none"
*search_accounts.isCompound: "true"
*search_accounts.compoundIcon: "form.xpm"
*search_accounts.compoundName: "form_"
*search_accounts.x: 378
*search_accounts.y: 130
*search_accounts.unitType: "pixels"
*search_accounts.background: "#9ac0cd"
*search_accounts.createCallback: {\
\
}
*search_accounts.allowShellResize: "false"

*scrolledWindow2.class: scrolledWindow
*scrolledWindow2.static: true
*scrolledWindow2.name: scrolledWindow2
*scrolledWindow2.parent: search_accounts
*scrolledWindow2.scrollingPolicy: "automatic"
*scrolledWindow2.width: 531
*scrolledWindow2.height: 486
*scrolledWindow2.isCompound: "true"
*scrolledWindow2.compoundIcon: "scrlwnd.xpm"
*scrolledWindow2.compoundName: "scrolled_Window"
*scrolledWindow2.x: 27
*scrolledWindow2.y: 60
*scrolledWindow2.background: "#9ac0cd"
*scrolledWindow2.createCallback: {\
Widget sb ;\
\
/*XtVaGetValues(UxWidget,XmNverticalScrollBar, &sb,NULL ) ;\
XtVaSetValues (sb,XmNbackground,"#9ac0cd",NULL ) ;*/\
}

*form5.class: form
*form5.static: true
*form5.name: form5
*form5.parent: scrolledWindow2
*form5.width: 527
*form5.height: 482
*form5.resizePolicy: "resize_none"
*form5.isCompound: "true"
*form5.compoundIcon: "form.xpm"
*form5.compoundName: "form_"
*form5.x: 2
*form5.y: -180
*form5.background: "#9ac0cd"

*typeTF.class: textField
*typeTF.static: true
*typeTF.name: typeTF
*typeTF.parent: form5
*typeTF.width: 220
*typeTF.isCompound: "true"
*typeTF.compoundIcon: "textfield.xpm"
*typeTF.compoundName: "text_Field"
*typeTF.x: 133
*typeTF.y: 57
*typeTF.background: "LightSkyBlue3"
*typeTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*typeTF.maxLength: 15
*typeTF.columns: 15
*typeTF.height: 35
*typeTF.activateCallback.source: public
*typeTF.activateCallback: XmProcessTraversal
*typeTF.activateCallbackClientData: (XtPointer)XmTRAVERSE_NEXT_TAB_GROUP
*typeTF.losingFocusCallback.source: public
*typeTF.losingFocusCallback: search_accounts_losing_focusCb
*typeTF.losingFocusCallbackClientData: (XtPointer) 2
*typeTF.editable: "false"
*typeTF.valueChangedCallback.source: public
*typeTF.valueChangedCallback: search_accounts_losing_focusCb
*typeTF.valueChangedCallbackClientData: (XtPointer) 2

*label27.class: label
*label27.static: true
*label27.name: label27
*label27.parent: form5
*label27.isCompound: "true"
*label27.compoundIcon: "label.xpm"
*label27.compoundName: "label_"
*label27.x: 18
*label27.y: 65
*label27.background: "#9ac0cd"
*label27.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label27.labelString: "Type:"

*label35.class: label
*label35.static: true
*label35.name: label35
*label35.parent: form5
*label35.isCompound: "true"
*label35.compoundIcon: "label.xpm"
*label35.compoundName: "label_"
*label35.x: 18
*label35.y: 25
*label35.background: "#9ac0cd"
*label35.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label35.labelString: "ID:"

*resourceTF.class: textField
*resourceTF.static: true
*resourceTF.name: resourceTF
*resourceTF.parent: form5
*resourceTF.width: 220
*resourceTF.isCompound: "true"
*resourceTF.compoundIcon: "textfield.xpm"
*resourceTF.compoundName: "text_Field"
*resourceTF.x: 133
*resourceTF.y: 99
*resourceTF.background: "LightSkyBlue3"
*resourceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*resourceTF.maxLength: 20
*resourceTF.columns: 20
*resourceTF.height: 35
*resourceTF.editable: "false"
*resourceTF.activateCallback.source: public
*resourceTF.activateCallback: XmProcessTraversal
*resourceTF.activateCallbackClientData: (XtPointer)XmTRAVERSE_NEXT_TAB_GROUP
*resourceTF.losingFocusCallback.source: public
*resourceTF.losingFocusCallback: search_accounts_losing_focusCb
*resourceTF.losingFocusCallbackClientData: (XtPointer) 3
*resourceTF.valueChangedCallback.source: public
*resourceTF.valueChangedCallback: search_accounts_losing_focusCb
*resourceTF.valueChangedCallbackClientData: (XtPointer) 3

*option_menu_resource.class: rowColumn
*option_menu_resource.static: true
*option_menu_resource.name: option_menu_resource
*option_menu_resource.parent: form5
*option_menu_resource.rowColumnType: "menu_option"
*option_menu_resource.subMenuId: "option_menu_pane_resource"
*option_menu_resource.isCompound: "true"
*option_menu_resource.compoundIcon: "optionM.xpm"
*option_menu_resource.compoundName: "option_Menu"
*option_menu_resource.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*option_menu_resource.x: 364
*option_menu_resource.y: 98
*option_menu_resource.background: "#9ac0cd"
*option_menu_resource.labelString: " "
*option_menu_resource.entryAlignment: "alignment_beginning"
*option_menu_resource.resizeWidth: "true"
*option_menu_resource.width: 138

*option_menu_pane_resource.class: rowColumn
*option_menu_pane_resource.static: true
*option_menu_pane_resource.name: option_menu_pane_resource
*option_menu_pane_resource.parent: option_menu_resource
*option_menu_pane_resource.rowColumnType: "menu_pulldown"
*option_menu_pane_resource.background: "#9ac0cd"
*option_menu_pane_resource.width: 90
*option_menu_pane_resource.resizeWidth: "false"

*option_menu_resourcePB.class: pushButton
*option_menu_resourcePB.static: true
*option_menu_resourcePB.name: option_menu_resourcePB
*option_menu_resourcePB.parent: option_menu_pane_resource
*option_menu_resourcePB.labelString: "None"
*option_menu_resourcePB.background: "#9ac0cd"
*option_menu_resourcePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*option_menu_resourcePB.activateCallback.source: public
*option_menu_resourcePB.activateCallback: search_accounts_option_resourcesCb
*option_menu_resourcePB.width: 90
*option_menu_resourcePB.activateCallbackClientData: (XtPointer) 0
*option_menu_resourcePB.createCallback.source: public
*option_menu_resourcePB.createCallback: search_accounts_option_resourcesCb
*option_menu_resourcePB.createCallbackClientData: (XtPointer) 1

*scrolledWindowText2.class: scrolledWindow
*scrolledWindowText2.static: true
*scrolledWindowText2.name: scrolledWindowText2
*scrolledWindowText2.parent: form5
*scrolledWindowText2.scrollingPolicy: "automatic"
*scrolledWindowText2.visualPolicy: "constant"
*scrolledWindowText2.scrollBarDisplayPolicy: "as_needed"
*scrolledWindowText2.isCompound: "true"
*scrolledWindowText2.compoundIcon: "scrltext.xpm"
*scrolledWindowText2.compoundName: "scrolled_Text"
*scrolledWindowText2.x: 137
*scrolledWindowText2.y: 405
*scrolledWindowText2.background: "#9ac0cd"
*scrolledWindowText2.width: 366
*scrolledWindowText2.height: 64
*scrolledWindowText2.shadowThickness: 1

*commentsST.class: scrolledText
*commentsST.static: true
*commentsST.name: commentsST
*commentsST.parent: scrolledWindowText2
*commentsST.width: 340
*commentsST.height: 35
*commentsST.editMode: "multi_line_edit"
*commentsST.maxLength: 255
*commentsST.wordWrap: "false"
*commentsST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*commentsST.columns: 29
*commentsST.rows: 2
*commentsST.scrollHorizontal: "false"
*commentsST.resizeHeight: "false"
*commentsST.resizeWidth: "false"
*commentsST.scrollVertical: "true"
*commentsST.background: "LightSkyBlue3"
*commentsST.losingFocusCallback.source: public
*commentsST.losingFocusCallback: search_accounts_losing_focusCb
*commentsST.activateCallback.source: public
*commentsST.activateCallback: XmProcessTraversal
*commentsST.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*commentsST.losingFocusCallbackClientData: (XtPointer) 10

*label9.class: label
*label9.static: true
*label9.name: label9
*label9.parent: form5
*label9.isCompound: "true"
*label9.compoundIcon: "label.xpm"
*label9.compoundName: "label_"
*label9.x: 18
*label9.y: 105
*label9.background: "#9ac0cd"
*label9.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label9.labelString: "Resource:"

*label10.class: label
*label10.static: true
*label10.name: label10
*label10.parent: form5
*label10.isCompound: "true"
*label10.compoundIcon: "label.xpm"
*label10.compoundName: "label_"
*label10.x: 169
*label10.y: 358
*label10.background: "#9ac0cd"
*label10.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label10.labelString: "Begin:"

*label12.class: label
*label12.static: true
*label12.name: label12
*label12.parent: form5
*label12.isCompound: "true"
*label12.compoundIcon: "label.xpm"
*label12.compoundName: "label_"
*label12.x: 18
*label12.y: 411
*label12.background: "#9ac0cd"
*label12.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label12.labelString: "Comments:"

*label13.class: label
*label13.static: true
*label13.name: label13
*label13.parent: form5
*label13.isCompound: "true"
*label13.compoundIcon: "label.xpm"
*label13.compoundName: "label_"
*label13.x: 18
*label13.y: 356
*label13.background: "#9ac0cd"
*label13.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label13.labelString: "Current Balance:"

*label15.class: label
*label15.static: true
*label15.name: label15
*label15.parent: form5
*label15.isCompound: "true"
*label15.compoundIcon: "label.xpm"
*label15.compoundName: "label_"
*label15.x: 18
*label15.y: 300
*label15.background: "#9ac0cd"
*label15.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label15.labelString: "Expiration Date:"

*idTF.class: textField
*idTF.static: true
*idTF.name: idTF
*idTF.parent: form5
*idTF.width: 220
*idTF.isCompound: "true"
*idTF.compoundIcon: "textfield.xpm"
*idTF.compoundName: "text_Field"
*idTF.x: 133
*idTF.y: 15
*idTF.background: "LightSkyBlue3"
*idTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*idTF.maxLength: 20
*idTF.height: 35
*idTF.activateCallback.source: public
*idTF.activateCallback: XmProcessTraversal
*idTF.activateCallbackClientData: (XtPointer)XmTRAVERSE_NEXT_TAB_GROUP
*idTF.losingFocusCallback.source: public
*idTF.losingFocusCallback: search_accounts_losing_focusCb
*idTF.losingFocusCallbackClientData: (XtPointer) 1

*label78.class: label
*label78.static: true
*label78.name: label78
*label78.parent: form5
*label78.isCompound: "true"
*label78.compoundIcon: "label.xpm"
*label78.compoundName: "label_"
*label78.x: 343
*label78.y: 359
*label78.background: "#9ac0cd"
*label78.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label78.labelString: "End:"

*label79.class: label
*label79.static: true
*label79.name: label79
*label79.parent: form5
*label79.isCompound: "true"
*label79.compoundIcon: "label.xpm"
*label79.compoundName: "label_"
*label79.x: 18
*label79.y: 259
*label79.background: "#9ac0cd"
*label79.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label79.labelString: "Creation Date:"
*label79.alignment: "alignment_beginning"

*option_menu_type.class: rowColumn
*option_menu_type.static: true
*option_menu_type.name: option_menu_type
*option_menu_type.parent: form5
*option_menu_type.rowColumnType: "menu_option"
*option_menu_type.subMenuId: "option_menu_pane_type"
*option_menu_type.isCompound: "true"
*option_menu_type.compoundIcon: "optionM.xpm"
*option_menu_type.compoundName: "option_Menu"
*option_menu_type.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*option_menu_type.x: 364
*option_menu_type.y: 56
*option_menu_type.background: "#9ac0cd"
*option_menu_type.labelString: " "

*option_menu_pane_type.class: rowColumn
*option_menu_pane_type.static: true
*option_menu_pane_type.name: option_menu_pane_type
*option_menu_pane_type.parent: option_menu_type
*option_menu_pane_type.rowColumnType: "menu_pulldown"
*option_menu_pane_type.background: "#9ac0cd"
*option_menu_pane_type.resizeWidth: "false"
*option_menu_pane_type.width: 90

*option_menu_typePB.class: pushButton
*option_menu_typePB.static: true
*option_menu_typePB.name: option_menu_typePB
*option_menu_typePB.parent: option_menu_pane_type
*option_menu_typePB.labelString: "None"
*option_menu_typePB.background: "#9ac0cd"
*option_menu_typePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*option_menu_typePB.activateCallback.source: public
*option_menu_typePB.activateCallback: search_accounts_option_typeCb
*option_menu_typePB.activateCallbackClientData: (XtPointer) 0
*option_menu_typePB.createCallback.source: public
*option_menu_typePB.createCallback: search_accounts_option_typeCb
*option_menu_typePB.createCallbackClientData: (XtPointer) 1

*label80.class: label
*label80.static: true
*label80.name: label80
*label80.parent: form5
*label80.isCompound: "true"
*label80.compoundIcon: "label.xpm"
*label80.compoundName: "label_"
*label80.x: 169
*label80.y: 259
*label80.background: "#9ac0cd"
*label80.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label80.labelString: "Start:"

*label81.class: label
*label81.static: true
*label81.name: label81
*label81.parent: form5
*label81.isCompound: "true"
*label81.compoundIcon: "label.xpm"
*label81.compoundName: "label_"
*label81.x: 343
*label81.y: 260
*label81.background: "#9ac0cd"
*label81.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label81.labelString: "End:"

*label82.class: label
*label82.static: true
*label82.name: label82
*label82.parent: form5
*label82.isCompound: "true"
*label82.compoundIcon: "label.xpm"
*label82.compoundName: "label_"
*label82.x: 169
*label82.y: 300
*label82.background: "#9ac0cd"
*label82.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label82.labelString: "Start:"

*label83.class: label
*label83.static: true
*label83.name: label83
*label83.parent: form5
*label83.isCompound: "true"
*label83.compoundIcon: "label.xpm"
*label83.compoundName: "label_"
*label83.x: 343
*label83.y: 301
*label83.background: "#9ac0cd"
*label83.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label83.labelString: "End:"

*start_creationTF.class: text
*start_creationTF.static: true
*start_creationTF.name: start_creationTF
*start_creationTF.parent: form5
*start_creationTF.width: 114
*start_creationTF.isCompound: "true"
*start_creationTF.compoundIcon: "text.xpm"
*start_creationTF.compoundName: "text_"
*start_creationTF.x: 223
*start_creationTF.y: 255
*start_creationTF.activateCallback.source: public
*start_creationTF.activateCallback: XmProcessTraversal
*start_creationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*start_creationTF.background: "LightSkyBlue3"
*start_creationTF.losingFocusCallback.source: public
*start_creationTF.losingFocusCallback: search_accounts_losing_focusCb
*start_creationTF.losingFocusCallbackClientData: (XtPointer) 4
*start_creationTF.modifyVerifyCallback.source: public
*start_creationTF.modifyVerifyCallback: search_accounts_check_dateCb
*start_creationTF.motionVerifyCallback.source: public
*start_creationTF.motionVerifyCallback: search_accounts_check_dateCb
*start_creationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*end_creationTF.class: text
*end_creationTF.static: true
*end_creationTF.name: end_creationTF
*end_creationTF.parent: form5
*end_creationTF.width: 114
*end_creationTF.isCompound: "true"
*end_creationTF.compoundIcon: "text.xpm"
*end_creationTF.compoundName: "text_"
*end_creationTF.x: 389
*end_creationTF.y: 255
*end_creationTF.activateCallback.source: public
*end_creationTF.activateCallback: XmProcessTraversal
*end_creationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*end_creationTF.background: "LightSkyBlue3"
*end_creationTF.losingFocusCallback.source: public
*end_creationTF.losingFocusCallback: search_accounts_losing_focusCb
*end_creationTF.losingFocusCallbackClientData: (XtPointer) 5
*end_creationTF.modifyVerifyCallback.source: public
*end_creationTF.modifyVerifyCallback: search_accounts_check_dateCb
*end_creationTF.motionVerifyCallback.source: public
*end_creationTF.motionVerifyCallback: search_accounts_check_dateCb
*end_creationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*start_expirationTF.class: text
*start_expirationTF.static: true
*start_expirationTF.name: start_expirationTF
*start_expirationTF.parent: form5
*start_expirationTF.width: 114
*start_expirationTF.isCompound: "true"
*start_expirationTF.compoundIcon: "text.xpm"
*start_expirationTF.compoundName: "text_"
*start_expirationTF.x: 223
*start_expirationTF.y: 296
*start_expirationTF.activateCallback.source: public
*start_expirationTF.activateCallback: XmProcessTraversal
*start_expirationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*start_expirationTF.background: "LightSkyBlue3"
*start_expirationTF.losingFocusCallback.source: public
*start_expirationTF.losingFocusCallback: search_accounts_losing_focusCb
*start_expirationTF.losingFocusCallbackClientData: (XtPointer) 6
*start_expirationTF.modifyVerifyCallback.source: public
*start_expirationTF.modifyVerifyCallback: search_accounts_check_dateCb
*start_expirationTF.motionVerifyCallback.source: public
*start_expirationTF.motionVerifyCallback: search_accounts_check_dateCb
*start_expirationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*end_expirationTF.class: text
*end_expirationTF.static: true
*end_expirationTF.name: end_expirationTF
*end_expirationTF.parent: form5
*end_expirationTF.width: 114
*end_expirationTF.isCompound: "true"
*end_expirationTF.compoundIcon: "text.xpm"
*end_expirationTF.compoundName: "text_"
*end_expirationTF.x: 389
*end_expirationTF.y: 296
*end_expirationTF.activateCallback.source: public
*end_expirationTF.activateCallback: XmProcessTraversal
*end_expirationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*end_expirationTF.background: "LightSkyBlue3"
*end_expirationTF.losingFocusCallback.source: public
*end_expirationTF.losingFocusCallback: search_accounts_losing_focusCb
*end_expirationTF.losingFocusCallbackClientData: (XtPointer) 7
*end_expirationTF.modifyVerifyCallback.source: public
*end_expirationTF.modifyVerifyCallback: search_accounts_check_dateCb
*end_expirationTF.motionVerifyCallback.source: public
*end_expirationTF.motionVerifyCallback: search_accounts_check_dateCb
*end_expirationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*start_balanceTF.class: text
*start_balanceTF.static: true
*start_balanceTF.name: start_balanceTF
*start_balanceTF.parent: form5
*start_balanceTF.width: 114
*start_balanceTF.isCompound: "true"
*start_balanceTF.compoundIcon: "text.xpm"
*start_balanceTF.compoundName: "text_"
*start_balanceTF.x: 223
*start_balanceTF.y: 354
*start_balanceTF.activateCallback.source: public
*start_balanceTF.activateCallback: XmProcessTraversal
*start_balanceTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*start_balanceTF.background: "LightSkyBlue3"
*start_balanceTF.losingFocusCallback.source: public
*start_balanceTF.losingFocusCallback: search_accounts_losing_focusCb
*start_balanceTF.losingFocusCallbackClientData: (XtPointer) 8
*start_balanceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*start_balanceTF.modifyVerifyCallback.source: public
*start_balanceTF.modifyVerifyCallback: search_accounts_check_balanceCb
*start_balanceTF.motionVerifyCallback.source: public
*start_balanceTF.motionVerifyCallback: search_accounts_check_balanceCb

*end_balanceTF.class: text
*end_balanceTF.static: true
*end_balanceTF.name: end_balanceTF
*end_balanceTF.parent: form5
*end_balanceTF.width: 114
*end_balanceTF.isCompound: "true"
*end_balanceTF.compoundIcon: "text.xpm"
*end_balanceTF.compoundName: "text_"
*end_balanceTF.x: 389
*end_balanceTF.y: 354
*end_balanceTF.activateCallback.source: public
*end_balanceTF.activateCallback: XmProcessTraversal
*end_balanceTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*end_balanceTF.background: "LightSkyBlue3"
*end_balanceTF.losingFocusCallback.source: public
*end_balanceTF.losingFocusCallback: search_accounts_losing_focusCb
*end_balanceTF.losingFocusCallbackClientData: (XtPointer) 9
*end_balanceTF.modifyVerifyCallback.source: public
*end_balanceTF.modifyVerifyCallback: search_accounts_check_balanceCb
*end_balanceTF.motionVerifyCallback.source: public
*end_balanceTF.motionVerifyCallback: search_accounts_check_balanceCb
*end_balanceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"

*frame8.class: frame
*frame8.static: true
*frame8.name: frame8
*frame8.parent: form5
*frame8.width: 278
*frame8.height: 38
*frame8.isCompound: "true"
*frame8.compoundIcon: "frame.xpm"
*frame8.compoundName: "frame_"
*frame8.x: 223
*frame8.y: 206
*frame8.background: "#9ac0cd"

*form9.class: form
*form9.static: true
*form9.name: form9
*form9.parent: frame8
*form9.width: 200
*form9.height: 200
*form9.resizePolicy: "resize_none"
*form9.isCompound: "true"
*form9.compoundIcon: "form.xpm"
*form9.compoundName: "form_"
*form9.x: 56
*form9.y: 6
*form9.background: "#9ac0cd"

*label5.class: label
*label5.static: true
*label5.name: label5
*label5.parent: form9
*label5.isCompound: "true"
*label5.compoundIcon: "label.xpm"
*label5.compoundName: "label_"
*label5.x: 8
*label5.y: 0
*label5.background: "#9ac0cd"
*label5.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label5.labelString: "Date Format : YYYY-MM-DD"
*label5.height: 32
*label5.width: 258

*label152.class: label
*label152.static: true
*label152.name: label152
*label152.parent: form5
*label152.isCompound: "true"
*label152.compoundIcon: "label.xpm"
*label152.compoundName: "label_"
*label152.x: 18
*label152.y: 147
*label152.background: "#9ac0cd"
*label152.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label152.labelString: "Manager:"
*label152.width: 79
*label152.height: 23

*managerTF.class: textField
*managerTF.static: true
*managerTF.name: managerTF
*managerTF.parent: form5
*managerTF.width: 220
*managerTF.isCompound: "true"
*managerTF.compoundIcon: "textfield.xpm"
*managerTF.compoundName: "text_Field"
*managerTF.x: 133
*managerTF.y: 142
*managerTF.background: "LightSkyBlue3"
*managerTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*managerTF.maxLength: 20
*managerTF.height: 35
*managerTF.activateCallback.source: public
*managerTF.activateCallback: XmProcessTraversal
*managerTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*managerTF.losingFocusCallbackClientData: (XtPointer) 11
*managerTF.editable: "true"
*managerTF.losingFocusCallback.source: public
*managerTF.losingFocusCallback: search_accounts_losing_focusCb
*managerTF.valueChangedCallback.source: public
*managerTF.valueChangedCallback: search_accounts_losing_focusCb
*managerTF.valueChangedCallbackClientData: (XtPointer) 11

*pushButton2.class: pushButton
*pushButton2.static: true
*pushButton2.name: pushButton2
*pushButton2.parent: form5
*pushButton2.isCompound: "true"
*pushButton2.compoundIcon: "push.xpm"
*pushButton2.compoundName: "push_Button"
*pushButton2.x: 380
*pushButton2.y: 144
*pushButton2.background: "#9ac0cd"
*pushButton2.height: 32
*pushButton2.labelString: "List..."
*pushButton2.width: 115
*pushButton2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*pushButton2.activateCallback.source: public
*pushButton2.activateCallback: account_manager_select_dlg_popupCb
*pushButton2.activateCallbackClientData: (XtPointer) 1

*label41.class: label
*label41.static: true
*label41.name: label41
*label41.parent: search_accounts
*label41.isCompound: "true"
*label41.compoundIcon: "label.xpm"
*label41.compoundName: "label_"
*label41.x: 146
*label41.y: 23
*label41.background: "#9ac0cd"
*label41.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label41.labelString: "Search   Accounts   Parameters"

*closeButton.class: pushButton
*closeButton.static: true
*closeButton.name: closeButton
*closeButton.parent: search_accounts
*closeButton.isCompound: "true"
*closeButton.compoundIcon: "push.xpm"
*closeButton.compoundName: "push_Button"
*closeButton.x: 401
*closeButton.y: 563
*closeButton.width: 138
*closeButton.height: 34
*closeButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closeButton.labelString: "Close"
*closeButton.shadowThickness: 4
*closeButton.background: "CadetBlue"
*closeButton.activateCallback.source: public
*closeButton.activateCallback: search_accounts_closeCb

*pushButton3.class: pushButton
*pushButton3.static: true
*pushButton3.name: pushButton3
*pushButton3.parent: search_accounts
*pushButton3.isCompound: "true"
*pushButton3.compoundIcon: "push.xpm"
*pushButton3.compoundName: "push_Button"
*pushButton3.x: 48
*pushButton3.y: 563
*pushButton3.width: 138
*pushButton3.height: 34
*pushButton3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*pushButton3.labelString: "Search"
*pushButton3.shadowThickness: 4
*pushButton3.background: "CadetBlue"
*pushButton3.activateCallback.source: public
*pushButton3.activateCallback: search_accounts_start_searchCb

