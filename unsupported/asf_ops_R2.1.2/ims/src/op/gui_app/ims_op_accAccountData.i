! UIMX ascii 2.9 key: 2546                                                      

*account_data.class: form
*account_data.classinc:
*account_data.classspec:
*account_data.classmembers:
*account_data.classconstructor:
*account_data.classdestructor:
*account_data.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>\
/*typedef enum account_user_data { CREATE = 1, DATA = 2 } ACCOUNT_USER_DATA ;*/\
\

*account_data.ispecdecl:
*account_data.funcdecl: /* create = DATA => an existing account will be accessed, otherwise */\
/* a new account will be created                                    */\
swidget create_account_data(swidget UxParent, /*ACCOUNT_USER_DATA*/ int create)
*account_data.funcname: create_account_data
*account_data.funcdef: "swidget", "<create_account_data>(%)"
*account_data.argdecl: swidget UxParent;\
int create;
*account_data.arglist: UxParent, create
*account_data.arglist.UxParent: "swidget", "%UxParent%"
*account_data.arglist.create: "int", "%create%"
*account_data.icode:
*account_data.fcode: return(rtrn);\

*account_data.auxdecl:
*account_data.name.source: public
*account_data.static: false
*account_data.name: account_data
*account_data.parent: NO_PARENT
*account_data.parentExpression: UxParent
*account_data.defaultShell: transientShell
*account_data.width: 585
*account_data.height: 740
*account_data.resizePolicy: "resize_none"
*account_data.isCompound: "true"
*account_data.compoundIcon: "form.xpm"
*account_data.compoundName: "form_"
*account_data.x: 484
*account_data.y: 30
*account_data.unitType: "pixels"
*account_data.background: "#9ac0cd"
*account_data.allowShellResize: "false"

*scrolledWindow2.class: scrolledWindow
*scrolledWindow2.static: true
*scrolledWindow2.name: scrolledWindow2
*scrolledWindow2.parent: account_data
*scrolledWindow2.scrollingPolicy: "automatic"
*scrolledWindow2.width: 531
*scrolledWindow2.height: 619
*scrolledWindow2.isCompound: "true"
*scrolledWindow2.compoundIcon: "scrlwnd.xpm"
*scrolledWindow2.compoundName: "scrolled_Window"
*scrolledWindow2.x: 28
*scrolledWindow2.y: 48
*scrolledWindow2.background: "#9ac0cd"

*form5.class: form
*form5.static: true
*form5.name: form5
*form5.parent: scrolledWindow2
*form5.width: 527
*form5.height: 615
*form5.resizePolicy: "resize_none"
*form5.isCompound: "true"
*form5.compoundIcon: "form.xpm"
*form5.compoundName: "form_"
*form5.x: 2
*form5.y: 2
*form5.background: "#9ac0cd"

*typeTF.class: textField
*typeTF.static: true
*typeTF.name: typeTF
*typeTF.parent: form5
*typeTF.width: 220
*typeTF.isCompound: "true"
*typeTF.compoundIcon: "textfield.xpm"
*typeTF.compoundName: "text_Field"
*typeTF.x: 150
*typeTF.y: 50
*typeTF.background: "LightSkyBlue3"
*typeTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*typeTF.maxLength: 15
*typeTF.columns: 15
*typeTF.height: 35
*typeTF.editable: "false"
*typeTF.activateCallback.source: public
*typeTF.activateCallback: XmProcessTraversal
*typeTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*typeTF.losingFocusCallbackClientData: (XtPointer) 2

*label27.class: label
*label27.static: true
*label27.name: label27
*label27.parent: form5
*label27.isCompound: "true"
*label27.compoundIcon: "label.xpm"
*label27.compoundName: "label_"
*label27.x: 7
*label27.y: 59
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
*label35.x: 7
*label35.y: 19
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
*resourceTF.x: 150
*resourceTF.y: 92
*resourceTF.background: "LightSkyBlue3"
*resourceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*resourceTF.maxLength: 20
*resourceTF.columns: 20
*resourceTF.height: 35
*resourceTF.editable: "false"
*resourceTF.activateCallback.source: public
*resourceTF.activateCallback: XmProcessTraversal
*resourceTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*resourceTF.losingFocusCallbackClientData: (XtPointer) 3

*procTF.class: textField
*procTF.static: true
*procTF.name: procTF
*procTF.parent: form5
*procTF.width: 220
*procTF.isCompound: "true"
*procTF.compoundIcon: "textfield.xpm"
*procTF.compoundName: "text_Field"
*procTF.x: 150
*procTF.y: 496
*procTF.background: "LightSkyBlue3"
*procTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*procTF.maxLength: 20
*procTF.height: 35
*procTF.activateCallback.source: public
*procTF.activateCallback: XmProcessTraversal
*procTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*procTF.losingFocusCallbackClientData: (XtPointer) 1
*procTF.editable: "false"

*optionMenu2.class: rowColumn
*optionMenu2.static: true
*optionMenu2.name: optionMenu2
*optionMenu2.parent: form5
*optionMenu2.rowColumnType: "menu_option"
*optionMenu2.subMenuId: "optionMenu_p2"
*optionMenu2.isCompound: "true"
*optionMenu2.compoundIcon: "optionM.xpm"
*optionMenu2.compoundName: "option_Menu"
*optionMenu2.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu2.x: 374
*optionMenu2.y: 494
*optionMenu2.background: "#9ac0cd"
*optionMenu2.labelString: " "

*optionMenu_p2.class: rowColumn
*optionMenu_p2.static: true
*optionMenu_p2.name: optionMenu_p2
*optionMenu_p2.parent: optionMenu2
*optionMenu_p2.rowColumnType: "menu_pulldown"
*optionMenu_p2.background: "#9ac0cd"
*optionMenu_p2.width: 94
*optionMenu_p2.resizeWidth: "false"

*optionMenu_p_b2.class: pushButton
*optionMenu_p_b2.static: true
*optionMenu_p_b2.name: optionMenu_p_b2
*optionMenu_p_b2.parent: optionMenu_p2
*optionMenu_p_b2.labelString: "None"
*optionMenu_p_b2.background: "#9ac0cd"
*optionMenu_p_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*optionMenu_p_b2.activateCallback.source: public
*optionMenu_p_b2.activateCallback: account_data_option_special_procCb
*optionMenu_p_b2.createCallback.source: public
*optionMenu_p_b2.createCallback: account_data_option_special_procCb
*optionMenu_p_b2.width: 90

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
*option_menu_type.x: 370
*option_menu_type.y: 48
*option_menu_type.background: "#9ac0cd"
*option_menu_type.labelString: " "

*option_menu_pane_type.class: rowColumn
*option_menu_pane_type.static: true
*option_menu_pane_type.name: option_menu_pane_type
*option_menu_pane_type.parent: option_menu_type
*option_menu_pane_type.rowColumnType: "menu_pulldown"
*option_menu_pane_type.background: "#9ac0cd"
*option_menu_pane_type.resizeHeight: "true"
*option_menu_pane_type.width: 94
*option_menu_pane_type.resizeWidth: "false"

*option_menu_typePB.class: pushButton
*option_menu_typePB.static: true
*option_menu_typePB.name: option_menu_typePB
*option_menu_typePB.parent: option_menu_pane_type
*option_menu_typePB.labelString: "None"
*option_menu_typePB.background: "#9ac0cd"
*option_menu_typePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*option_menu_typePB.activateCallback.source: public
*option_menu_typePB.activateCallback: account_data_option_typeCb
*option_menu_typePB.createCallback.source: public
*option_menu_typePB.createCallback: account_data_option_typeCb
*option_menu_typePB.width: 90
*option_menu_typePB.activateCallbackClientData: (XtPointer) 0
*option_menu_typePB.createCallbackClientData: (XtPointer) 1

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
*option_menu_resource.x: 370
*option_menu_resource.y: 90
*option_menu_resource.background: "#9ac0cd"
*option_menu_resource.labelString: " "
*option_menu_resource.width: 123

*option_menu_pane_resource.class: rowColumn
*option_menu_pane_resource.static: true
*option_menu_pane_resource.name: option_menu_pane_resource
*option_menu_pane_resource.parent: option_menu_resource
*option_menu_pane_resource.rowColumnType: "menu_pulldown"
*option_menu_pane_resource.background: "#9ac0cd"
*option_menu_pane_resource.width: 94
*option_menu_pane_resource.packing: "pack_tight"
*option_menu_pane_resource.orientation: "vertical"
*option_menu_pane_resource.resizeWidth: "false"

*option_menu_resourcePB.class: pushButton
*option_menu_resourcePB.static: true
*option_menu_resourcePB.name: option_menu_resourcePB
*option_menu_resourcePB.parent: option_menu_pane_resource
*option_menu_resourcePB.labelString: "None"
*option_menu_resourcePB.background: "#9ac0cd"
*option_menu_resourcePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*option_menu_resourcePB.activateCallback.source: public
*option_menu_resourcePB.activateCallback: account_data_option_resourcesCb
*option_menu_resourcePB.createCallback.source: public
*option_menu_resourcePB.createCallback: account_data_option_resourcesCb
*option_menu_resourcePB.width: 90
*option_menu_resourcePB.activateCallbackClientData: (XtPointer)0
*option_menu_resourcePB.createCallbackClientData: (XtPointer)1

*label42.class: label
*label42.static: true
*label42.name: label42
*label42.parent: form5
*label42.isCompound: "true"
*label42.compoundIcon: "label.xpm"
*label42.compoundName: "label_"
*label42.x: 7
*label42.y: 183
*label42.background: "#9ac0cd"
*label42.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label42.labelString: "Current Balance:"

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
*scrolledWindowText2.x: 150
*scrolledWindowText2.y: 538
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
*commentsST.losingFocusCallbackClientData: (XtPointer) 11

*label9.class: label
*label9.static: true
*label9.name: label9
*label9.parent: form5
*label9.isCompound: "true"
*label9.compoundIcon: "label.xpm"
*label9.compoundName: "label_"
*label9.x: 7
*label9.y: 97
*label9.background: "#9ac0cd"
*label9.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label9.labelString: "Resource:"

*creationTF.class: textField
*creationTF.static: true
*creationTF.name: creationTF
*creationTF.parent: form5
*creationTF.width: 220
*creationTF.isCompound: "true"
*creationTF.compoundIcon: "textfield.xpm"
*creationTF.compoundName: "text_Field"
*creationTF.x: 150
*creationTF.y: 264
*creationTF.background: "LightSkyBlue3"
*creationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*creationTF.maxLength: 50
*creationTF.columns: 50
*creationTF.height: 35
*creationTF.activateCallback.source: public
*creationTF.activateCallback: XmProcessTraversal
*creationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*creationTF.losingFocusCallbackClientData: (XtPointer)7
*creationTF.focusCallback.source: public
*creationTF.focusCallback: account_data_creation_focusCb
*creationTF.editable: "false"

*label11.class: label
*label11.static: true
*label11.name: label11
*label11.parent: form5
*label11.isCompound: "true"
*label11.compoundIcon: "label.xpm"
*label11.compoundName: "label_"
*label11.x: 7
*label11.y: 137
*label11.background: "#9ac0cd"
*label11.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label11.labelString: "Begin Balance:"

*label12.class: label
*label12.static: true
*label12.name: label12
*label12.parent: form5
*label12.isCompound: "true"
*label12.compoundIcon: "label.xpm"
*label12.compoundName: "label_"
*label12.x: 7
*label12.y: 552
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
*label13.x: 7
*label13.y: 359
*label13.background: "#9ac0cd"
*label13.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label13.labelString: "Rate Multiplier:"

*label14.class: label
*label14.static: true
*label14.name: label14
*label14.parent: form5
*label14.isCompound: "true"
*label14.compoundIcon: "label.xpm"
*label14.compoundName: "label_"
*label14.x: 7
*label14.y: 229
*label14.background: "#9ac0cd"
*label14.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label14.labelString: "On Hold:"

*label15.class: label
*label15.static: true
*label15.name: label15
*label15.parent: form5
*label15.isCompound: "true"
*label15.compoundIcon: "label.xpm"
*label15.compoundName: "label_"
*label15.x: 7
*label15.y: 317
*label15.background: "#9ac0cd"
*label15.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label15.labelString: "Expiration Time:"

*label16.class: label
*label16.static: true
*label16.name: label16
*label16.parent: form5
*label16.isCompound: "true"
*label16.compoundIcon: "label.xpm"
*label16.compoundName: "label_"
*label16.x: 7
*label16.y: 494
*label16.background: "#9ac0cd"
*label16.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label16.labelString: "Special\nProcessing:"
*label16.alignment: "alignment_beginning"
*label16.width: 93

*label18.class: label
*label18.static: true
*label18.name: label18
*label18.parent: form5
*label18.isCompound: "true"
*label18.compoundIcon: "label.xpm"
*label18.compoundName: "label_"
*label18.x: 7
*label18.y: 454
*label18.background: "#9ac0cd"
*label18.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label18.labelString: "Manual Validate:"

*frame3.class: frame
*frame3.static: true
*frame3.name: frame3
*frame3.parent: form5
*frame3.width: 122
*frame3.height: 44
*frame3.isCompound: "true"
*frame3.compoundIcon: "frame.xpm"
*frame3.compoundName: "frame_"
*frame3.x: 152
*frame3.y: 442
*frame3.background: "#9ac0cd"

*rowColumn1.class: rowColumn
*rowColumn1.static: true
*rowColumn1.name: rowColumn1
*rowColumn1.parent: frame3
*rowColumn1.width: 115
*rowColumn1.height: 40
*rowColumn1.isCompound: "true"
*rowColumn1.compoundIcon: "row.xpm"
*rowColumn1.compoundName: "row_Column"
*rowColumn1.x: 140
*rowColumn1.y: 2
*rowColumn1.orientation: "horizontal"
*rowColumn1.radioBehavior: "true"
*rowColumn1.shadowThickness: 0
*rowColumn1.background: "#9ac0cd"
*rowColumn1.radioAlwaysOne: "true"
*rowColumn1.whichButton: 2

*yesTB.class: toggleButton
*yesTB.static: true
*yesTB.name: yesTB
*yesTB.parent: rowColumn1
*yesTB.isCompound: "true"
*yesTB.compoundIcon: "toggle.xpm"
*yesTB.compoundName: "toggle_Button"
*yesTB.x: 140
*yesTB.y: 3
*yesTB.background: "#9ac0cd"
*yesTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*yesTB.labelString: "Yes"
*yesTB.createCallback: 
*yesTB.set: "true"
*yesTB.valueChangedCallback.source: public
*yesTB.valueChangedCallback: account_data_validationCb
*yesTB.valueChangedCallbackClientData: (XtPointer) 0x1
*yesTB.width: 52
*yesTB.height: 34

*noTB.class: toggleButton
*noTB.static: true
*noTB.name: noTB
*noTB.parent: rowColumn1
*noTB.isCompound: "true"
*noTB.compoundIcon: "toggle.xpm"
*noTB.compoundName: "toggle_Button"
*noTB.x: 140
*noTB.y: 3
*noTB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*noTB.labelString: "No"
*noTB.background: "#9ac0cd"
*noTB.set: "false"
*noTB.valueChangedCallback.source: public
*noTB.valueChangedCallback: account_data_validationCb
*noTB.valueChangedCallbackClientData: (XtPointer) 0x2
*noTB.width: 52
*noTB.height: 34

*idTF.class: textField
*idTF.static: true
*idTF.name: idTF
*idTF.parent: form5
*idTF.width: 220
*idTF.isCompound: "true"
*idTF.compoundIcon: "textfield.xpm"
*idTF.compoundName: "text_Field"
*idTF.x: 150
*idTF.y: 8
*idTF.background: "LightSkyBlue3"
*idTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*idTF.maxLength: 20
*idTF.height: 35
*idTF.activateCallback.source: public
*idTF.activateCallback: XmProcessTraversal
*idTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*idTF.losingFocusCallbackClientData: (XtPointer) 1
*idTF.createCallback.source: public
*idTF.createCallback: account_data_account_idCb
*idTF.createCallbackClientData: (XtPointer) 1

*label84.class: label
*label84.static: true
*label84.name: label84
*label84.parent: form5
*label84.isCompound: "true"
*label84.compoundIcon: "label.xpm"
*label84.compoundName: "label_"
*label84.x: 7
*label84.y: 273
*label84.background: "#9ac0cd"
*label84.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label84.labelString: "Creation Time:"

*frame6.class: frame
*frame6.static: true
*frame6.name: frame6
*frame6.parent: form5
*frame6.width: 118
*frame6.height: 74
*frame6.isCompound: "true"
*frame6.compoundIcon: "frame.xpm"
*frame6.compoundName: "frame_"
*frame6.x: 387
*frame6.y: 267
*frame6.background: "#9ac0cd"

*form8.class: form
*form8.static: true
*form8.name: form8
*form8.parent: frame6
*form8.width: 200
*form8.height: 200
*form8.resizePolicy: "resize_none"
*form8.isCompound: "true"
*form8.compoundIcon: "form.xpm"
*form8.compoundName: "form_"
*form8.x: 56
*form8.y: 6
*form8.background: "#9ac0cd"

*label85.class: label
*label85.static: true
*label85.name: label85
*label85.parent: form8
*label85.isCompound: "true"
*label85.compoundIcon: "label.xpm"
*label85.compoundName: "label_"
*label85.x: -1
*label85.y: -2
*label85.background: "#9ac0cd"
*label85.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label85.labelString: "YYYY-MM-DD\nHH:MM:SS"
*label85.height: 74

*expirationTF.class: text
*expirationTF.static: true
*expirationTF.name: expirationTF
*expirationTF.parent: form5
*expirationTF.width: 220
*expirationTF.isCompound: "true"
*expirationTF.compoundIcon: "text.xpm"
*expirationTF.compoundName: "text_"
*expirationTF.x: 150
*expirationTF.y: 308
*expirationTF.background: "LightSkyBlue3"
*expirationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*expirationTF.height: 35
*expirationTF.activateCallback.source: public
*expirationTF.activateCallback: XmProcessTraversal
*expirationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*expirationTF.modifyVerifyCallback.source: public
*expirationTF.modifyVerifyCallback: account_data_check_dateCb
*expirationTF.modifyVerifyCallbackClientData: (XtPointer) 0
*expirationTF.motionVerifyCallback.source: public
*expirationTF.motionVerifyCallback: account_data_check_dateCb
*expirationTF.maxLength: 25

*rateTF.class: text
*rateTF.static: true
*rateTF.name: rateTF
*rateTF.parent: form5
*rateTF.width: 220
*rateTF.isCompound: "true"
*rateTF.compoundIcon: "text.xpm"
*rateTF.compoundName: "text_"
*rateTF.x: 150
*rateTF.y: 350
*rateTF.background: "LightSkyBlue3"
*rateTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*rateTF.height: 35
*rateTF.activateCallback.source: public
*rateTF.activateCallback: XmProcessTraversal
*rateTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*rateTF.modifyVerifyCallback.source: public
*rateTF.modifyVerifyCallback: search_accounts_check_balanceCb
*rateTF.modifyVerifyCallbackClientData: (XtPointer) 0
*rateTF.motionVerifyCallback.source: public
*rateTF.motionVerifyCallback: search_accounts_check_balanceCb
*rateTF.maxLength: 25
*rateTF.focusCallback.source: public
*rateTF.focusCallback: account_data_rate_focusCb

*current_balanceTF.class: text
*current_balanceTF.static: true
*current_balanceTF.name: current_balanceTF
*current_balanceTF.parent: form5
*current_balanceTF.width: 220
*current_balanceTF.isCompound: "true"
*current_balanceTF.compoundIcon: "text.xpm"
*current_balanceTF.compoundName: "text_"
*current_balanceTF.x: 150
*current_balanceTF.y: 176
*current_balanceTF.background: "LightSkyBlue3"
*current_balanceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*current_balanceTF.height: 35
*current_balanceTF.activateCallback.source: public
*current_balanceTF.activateCallback: XmProcessTraversal
*current_balanceTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*current_balanceTF.modifyVerifyCallback.source: public
*current_balanceTF.modifyVerifyCallback: search_accounts_check_balanceCb
*current_balanceTF.modifyVerifyCallbackClientData: (XtPointer) 0
*current_balanceTF.motionVerifyCallback.source: public
*current_balanceTF.motionVerifyCallback: search_accounts_check_balanceCb
*current_balanceTF.createCallback.source: public
*current_balanceTF.createCallback: account_data_current_balanceCb
*current_balanceTF.createCallbackClientData: (XtPointer) 1
*current_balanceTF.maxLength: 25
*current_balanceTF.focusCallback.source: public
*current_balanceTF.focusCallback: account_data_current_balance_focusCb

*on_holdTF.class: text
*on_holdTF.static: true
*on_holdTF.name: on_holdTF
*on_holdTF.parent: form5
*on_holdTF.width: 220
*on_holdTF.isCompound: "true"
*on_holdTF.compoundIcon: "text.xpm"
*on_holdTF.compoundName: "text_"
*on_holdTF.x: 150
*on_holdTF.y: 220
*on_holdTF.background: "LightSkyBlue3"
*on_holdTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*on_holdTF.height: 35
*on_holdTF.activateCallback.source: public
*on_holdTF.activateCallback: XmProcessTraversal
*on_holdTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*on_holdTF.modifyVerifyCallback.source: public
*on_holdTF.modifyVerifyCallback: search_accounts_check_balanceCb
*on_holdTF.modifyVerifyCallbackClientData: (XtPointer) 0
*on_holdTF.motionVerifyCallback.source: public
*on_holdTF.motionVerifyCallback: search_accounts_check_balanceCb
*on_holdTF.createCallback.source: public
*on_holdTF.createCallback: account_data_on_holdCb
*on_holdTF.createCallbackClientData: (XtPointer) 1
*on_holdTF.maxLength: 25
*on_holdTF.focusCallback.source: public
*on_holdTF.focusCallback: account_data_on_hold_focusCb

*begin_balanceTF.class: text
*begin_balanceTF.static: true
*begin_balanceTF.name: begin_balanceTF
*begin_balanceTF.parent: form5
*begin_balanceTF.width: 220
*begin_balanceTF.isCompound: "true"
*begin_balanceTF.compoundIcon: "text.xpm"
*begin_balanceTF.compoundName: "text_"
*begin_balanceTF.x: 150
*begin_balanceTF.y: 134
*begin_balanceTF.background: "LightSkyBlue3"
*begin_balanceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*begin_balanceTF.height: 35
*begin_balanceTF.activateCallback.source: public
*begin_balanceTF.activateCallback: XmProcessTraversal
*begin_balanceTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*begin_balanceTF.modifyVerifyCallbackClientData: (XtPointer) 0
*begin_balanceTF.createCallback.source: public
*begin_balanceTF.createCallback: account_data_begin_balanceCb
*begin_balanceTF.createCallbackClientData: (XtPointer) 1
*begin_balanceTF.text: ""
*begin_balanceTF.modifyVerifyCallback.source: public
*begin_balanceTF.modifyVerifyCallback: search_accounts_check_balanceCb
*begin_balanceTF.motionVerifyCallback.source: public
*begin_balanceTF.motionVerifyCallback: search_accounts_check_balanceCb
*begin_balanceTF.maxLength: 25

*label151.class: label
*label151.static: true
*label151.name: label151
*label151.parent: form5
*label151.isCompound: "true"
*label151.compoundIcon: "label.xpm"
*label151.compoundName: "label_"
*label151.x: 7
*label151.y: 400
*label151.background: "#9ac0cd"
*label151.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label151.labelString: "Manager:"
*label151.width: 79
*label151.height: 23

*managerTF.class: textField
*managerTF.static: true
*managerTF.name: managerTF
*managerTF.parent: form5
*managerTF.width: 220
*managerTF.isCompound: "true"
*managerTF.compoundIcon: "textfield.xpm"
*managerTF.compoundName: "text_Field"
*managerTF.x: 150
*managerTF.y: 394
*managerTF.background: "LightSkyBlue3"
*managerTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*managerTF.maxLength: 20
*managerTF.height: 35
*managerTF.activateCallback.source: public
*managerTF.activateCallback: XmProcessTraversal
*managerTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*managerTF.losingFocusCallbackClientData: (XtPointer) 1
*managerTF.editable: "false"

*pushButton1.class: pushButton
*pushButton1.static: true
*pushButton1.name: pushButton1
*pushButton1.parent: form5
*pushButton1.isCompound: "true"
*pushButton1.compoundIcon: "push.xpm"
*pushButton1.compoundName: "push_Button"
*pushButton1.x: 390
*pushButton1.y: 394
*pushButton1.background: "#9ac0cd"
*pushButton1.height: 35
*pushButton1.labelString: "List..."
*pushButton1.width: 120
*pushButton1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*pushButton1.activateCallback.source: public
*pushButton1.activateCallback: account_manager_select_dlg_popupCb
*pushButton1.activateCallbackClientData: (XtPointer) 0

*account_dataLB1.class: label
*account_dataLB1.name.source: public
*account_dataLB1.static: false
*account_dataLB1.name: account_dataLB1
*account_dataLB1.parent: account_data
*account_dataLB1.isCompound: "true"
*account_dataLB1.compoundIcon: "label.xpm"
*account_dataLB1.compoundName: "label_"
*account_dataLB1.x: 210
*account_dataLB1.y: 13
*account_dataLB1.background: "#9ac0cd"
*account_dataLB1.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*account_dataLB1.labelString: "Create   Account"
*account_dataLB1.width: 168
*account_dataLB1.createCallback.source: public
*account_dataLB1.createCallback: account_data_labelCb

*savePB.class: pushButton
*savePB.static: true
*savePB.name: savePB
*savePB.parent: account_data
*savePB.isCompound: "true"
*savePB.compoundIcon: "push.xpm"
*savePB.compoundName: "push_Button"
*savePB.x: 14
*savePB.y: 685
*savePB.width: 138
*savePB.height: 34
*savePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*savePB.labelString: "Save"
*savePB.shadowThickness: 4
*savePB.background: "CadetBlue"
*savePB.activateCallback.source: public
*savePB.activateCallback: account_data_createCb
*savePB.createCallback.source: public
*savePB.createCallback: account_data_createCb
*savePB.createCallbackClientData: (XtPointer) 1
*savePB.recomputeSize: "false"
*savePB.activateCallbackClientData: (XtPointer) 0

*update_balancePB.class: pushButton
*update_balancePB.static: true
*update_balancePB.name: update_balancePB
*update_balancePB.parent: account_data
*update_balancePB.isCompound: "true"
*update_balancePB.compoundIcon: "push.xpm"
*update_balancePB.compoundName: "push_Button"
*update_balancePB.x: 220
*update_balancePB.y: 685
*update_balancePB.width: 138
*update_balancePB.height: 34
*update_balancePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*update_balancePB.labelString: "Update Balance"
*update_balancePB.shadowThickness: 4
*update_balancePB.background: "CadetBlue"
*update_balancePB.sensitive: "true"
*update_balancePB.activateCallback.source: public
*update_balancePB.activateCallback: account_data_update_balanceCb
*update_balancePB.createCallback.source: public
*update_balancePB.createCallback: account_data_update_balanceCb
*update_balancePB.createCallbackClientData: (XtPointer) 0x1

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: account_data
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 428
*closePB.y: 685
*closePB.width: 138
*closePB.height: 34
*closePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closePB.labelString: "Close"
*closePB.shadowThickness: 4
*closePB.background: "CadetBlue"
*closePB.activateCallback.source: public
*closePB.activateCallback: account_data_closeCb

