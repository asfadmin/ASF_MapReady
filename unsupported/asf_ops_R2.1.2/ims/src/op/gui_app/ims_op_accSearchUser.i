! UIMX ascii 2.9 key: 5418                                                      

*search_users.class: form
*search_users.classinc:
*search_users.classspec:
*search_users.classmembers:
*search_users.classconstructor:
*search_users.classdestructor:
*search_users.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>\

*search_users.ispecdecl:
*search_users.funcdecl: /* if create = CREATE => create user otherwise show user data */\
swidget create_search_users(swidget UxParent, /*ACCOUNT_USER_DATA*/ int  create)
*search_users.funcname: create_search_users
*search_users.funcdef: "swidget", "<create_search_users>(%)"
*search_users.argdecl: swidget UxParent;\
int create;
*search_users.arglist: UxParent, create
*search_users.arglist.UxParent: "swidget", "%UxParent%"
*search_users.arglist.create: "int", "%create%"
*search_users.icode:
*search_users.fcode: return(rtrn);\

*search_users.auxdecl:
*search_users.name.source: public
*search_users.static: false
*search_users.name: search_users
*search_users.parent: NO_PARENT
*search_users.parentExpression: UxParent
*search_users.defaultShell: transientShell
*search_users.width: 559
*search_users.height: 671
*search_users.resizePolicy: "resize_none"
*search_users.isCompound: "true"
*search_users.compoundIcon: "form.xpm"
*search_users.compoundName: "form_"
*search_users.x: 538
*search_users.y: 48
*search_users.unitType: "pixels"
*search_users.background: "#9ac0cd"

*scrolledWindow3.class: scrolledWindow
*scrolledWindow3.static: true
*scrolledWindow3.name: scrolledWindow3
*scrolledWindow3.parent: search_users
*scrolledWindow3.scrollingPolicy: "automatic"
*scrolledWindow3.width: 530
*scrolledWindow3.height: 562
*scrolledWindow3.isCompound: "true"
*scrolledWindow3.compoundIcon: "scrlwnd.xpm"
*scrolledWindow3.compoundName: "scrolled_Window"
*scrolledWindow3.x: 16
*scrolledWindow3.y: 52
*scrolledWindow3.background: "#9ac0cd"

*form10.class: form
*form10.static: true
*form10.name: form10
*form10.parent: scrolledWindow3
*form10.width: 496
*form10.height: 625
*form10.resizePolicy: "resize_none"
*form10.isCompound: "true"
*form10.compoundIcon: "form.xpm"
*form10.compoundName: "form_"
*form10.x: 2
*form10.y: 0
*form10.background: "#9ac0cd"

*label87.class: label
*label87.static: true
*label87.name: label87
*label87.parent: form10
*label87.isCompound: "true"
*label87.compoundIcon: "label.xpm"
*label87.compoundName: "label_"
*label87.x: 8
*label87.y: 90
*label87.background: "#9ac0cd"
*label87.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label87.labelString: "Last Name:"

*label88.class: label
*label88.static: true
*label88.name: label88
*label88.parent: form10
*label88.isCompound: "true"
*label88.compoundIcon: "label.xpm"
*label88.compoundName: "label_"
*label88.x: 352
*label88.y: 50
*label88.background: "#9ac0cd"
*label88.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label88.labelString: "M.I.:"

*idTF.class: textField
*idTF.static: true
*idTF.name: idTF
*idTF.parent: form10
*idTF.width: 220
*idTF.isCompound: "true"
*idTF.compoundIcon: "textfield.xpm"
*idTF.compoundName: "text_Field"
*idTF.x: 114
*idTF.y: 6
*idTF.background: "LightSkyBlue3"
*idTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*idTF.maxLength: 15
*idTF.columns: 15
*idTF.activateCallback.source: public
*idTF.activateCallback: XmProcessTraversal
*idTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*idTF.losingFocusCallback.source: public
*idTF.losingFocusCallback: search_users_losing_focusCb
*idTF.losingFocusCallbackClientData: (XtPointer) 0x1

*m_iTF.class: textField
*m_iTF.static: true
*m_iTF.name: m_iTF
*m_iTF.parent: form10
*m_iTF.width: 44
*m_iTF.isCompound: "true"
*m_iTF.compoundIcon: "textfield.xpm"
*m_iTF.compoundName: "text_Field"
*m_iTF.x: 402
*m_iTF.y: 44
*m_iTF.background: "LightSkyBlue3"
*m_iTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*m_iTF.columns: 3
*m_iTF.maxLength: 3
*m_iTF.activateCallback.source: public
*m_iTF.activateCallback: XmProcessTraversal
*m_iTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*m_iTF.losingFocusCallback.source: public
*m_iTF.losingFocusCallback: search_users_losing_focusCb
*m_iTF.losingFocusCallbackClientData: (XtPointer) 3

*label89.class: label
*label89.static: true
*label89.name: label89
*label89.parent: form10
*label89.isCompound: "true"
*label89.compoundIcon: "label.xpm"
*label89.compoundName: "label_"
*label89.x: 8
*label89.y: 132
*label89.background: "#9ac0cd"
*label89.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label89.labelString: "Type:"

*label90.class: label
*label90.static: true
*label90.name: label90
*label90.parent: form10
*label90.isCompound: "true"
*label90.compoundIcon: "label.xpm"
*label90.compoundName: "label_"
*label90.x: 8
*label90.y: 174
*label90.background: "#9ac0cd"
*label90.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label90.labelString: "Title:"

*label91.class: label
*label91.static: true
*label91.name: label91
*label91.parent: form10
*label91.isCompound: "true"
*label91.compoundIcon: "label.xpm"
*label91.compoundName: "label_"
*label91.x: 8
*label91.y: 246
*label91.background: "#9ac0cd"
*label91.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label91.labelString: "City:"

*label92.class: label
*label92.static: true
*label92.name: label92
*label92.parent: form10
*label92.isCompound: "true"
*label92.compoundIcon: "label.xpm"
*label92.compoundName: "label_"
*label92.x: 8
*label92.y: 288
*label92.background: "#9ac0cd"
*label92.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label92.labelString: "State:"
*label92.height: 28

*label93.class: label
*label93.static: true
*label93.name: label93
*label93.parent: form10
*label93.isCompound: "true"
*label93.compoundIcon: "label.xpm"
*label93.compoundName: "label_"
*label93.x: 8
*label93.y: 370
*label93.background: "#9ac0cd"
*label93.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label93.labelString: "Zip:"

*label94.class: label
*label94.static: true
*label94.name: label94
*label94.parent: form10
*label94.isCompound: "true"
*label94.compoundIcon: "label.xpm"
*label94.compoundName: "label_"
*label94.x: 8
*label94.y: 330
*label94.background: "#9ac0cd"
*label94.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label94.labelString: "Country:"

*label95.class: label
*label95.static: true
*label95.name: label95
*label95.parent: form10
*label95.isCompound: "true"
*label95.compoundIcon: "label.xpm"
*label95.compoundName: "label_"
*label95.x: 8
*label95.y: 408
*label95.background: "#9ac0cd"
*label95.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label95.labelString: "Phone:"
*label95.width: 52

*label96.class: label
*label96.static: true
*label96.name: label96
*label96.parent: form10
*label96.isCompound: "true"
*label96.compoundIcon: "label.xpm"
*label96.compoundName: "label_"
*label96.x: 8
*label96.y: 12
*label96.background: "#9ac0cd"
*label96.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label96.labelString: "ID:"

*label97.class: label
*label97.static: true
*label97.name: label97
*label97.parent: form10
*label97.isCompound: "true"
*label97.compoundIcon: "label.xpm"
*label97.compoundName: "label_"
*label97.x: 8
*label97.y: 46
*label97.background: "#9ac0cd"
*label97.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label97.labelString: "First Name:"

*first_nameTF.class: textField
*first_nameTF.static: true
*first_nameTF.name: first_nameTF
*first_nameTF.parent: form10
*first_nameTF.width: 220
*first_nameTF.isCompound: "true"
*first_nameTF.compoundIcon: "textfield.xpm"
*first_nameTF.compoundName: "text_Field"
*first_nameTF.x: 114
*first_nameTF.y: 44
*first_nameTF.background: "LightSkyBlue3"
*first_nameTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*first_nameTF.maxLength: 20
*first_nameTF.activateCallback.source: public
*first_nameTF.activateCallback: XmProcessTraversal
*first_nameTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*first_nameTF.losingFocusCallback.source: public
*first_nameTF.losingFocusCallback: search_users_losing_focusCb
*first_nameTF.losingFocusCallbackClientData: (XtPointer) 0x2

*last_nameTF.class: textField
*last_nameTF.static: true
*last_nameTF.name: last_nameTF
*last_nameTF.parent: form10
*last_nameTF.width: 220
*last_nameTF.isCompound: "true"
*last_nameTF.compoundIcon: "textfield.xpm"
*last_nameTF.compoundName: "text_Field"
*last_nameTF.x: 114
*last_nameTF.y: 84
*last_nameTF.background: "LightSkyBlue3"
*last_nameTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*last_nameTF.maxLength: 20
*last_nameTF.text: ""
*last_nameTF.activateCallback.source: public
*last_nameTF.activateCallback: XmProcessTraversal
*last_nameTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*last_nameTF.losingFocusCallback.source: public
*last_nameTF.losingFocusCallback: search_users_losing_focusCb
*last_nameTF.losingFocusCallbackClientData: (XtPointer) 4

*typeTF.class: textField
*typeTF.static: true
*typeTF.name: typeTF
*typeTF.parent: form10
*typeTF.width: 220
*typeTF.isCompound: "true"
*typeTF.compoundIcon: "textfield.xpm"
*typeTF.compoundName: "text_Field"
*typeTF.x: 114
*typeTF.y: 126
*typeTF.background: "LightSkyBlue3"
*typeTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*typeTF.maxLength: 20
*typeTF.editable: "false"
*typeTF.height: 35
*typeTF.activateCallback.source: public
*typeTF.activateCallback: XmProcessTraversal
*typeTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*typeTF.losingFocusCallback.source: public
*typeTF.losingFocusCallback: search_users_losing_focusCb
*typeTF.losingFocusCallbackClientData: (XtPointer)5
*typeTF.valueChangedCallback.source: public
*typeTF.valueChangedCallback: search_users_losing_focusCb
*typeTF.valueChangedCallbackClientData: (XtPointer) 5

*label98.class: label
*label98.static: true
*label98.name: label98
*label98.parent: form10
*label98.isCompound: "true"
*label98.compoundIcon: "label.xpm"
*label98.compoundName: "label_"
*label98.x: 8
*label98.y: 212
*label98.background: "#9ac0cd"
*label98.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label98.labelString: "Organization:"

*organizationTF.class: textField
*organizationTF.static: true
*organizationTF.name: organizationTF
*organizationTF.parent: form10
*organizationTF.width: 342
*organizationTF.isCompound: "true"
*organizationTF.compoundIcon: "textfield.xpm"
*organizationTF.compoundName: "text_Field"
*organizationTF.x: 114
*organizationTF.y: 208
*organizationTF.background: "LightSkyBlue3"
*organizationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*organizationTF.maxLength: 32
*organizationTF.columns: 32
*organizationTF.text: ""
*organizationTF.activateCallback.source: public
*organizationTF.activateCallback: XmProcessTraversal
*organizationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*organizationTF.losingFocusCallback.source: public
*organizationTF.losingFocusCallback: search_users_losing_focusCb
*organizationTF.losingFocusCallbackClientData: (XtPointer)7

*titleTF.class: textField
*titleTF.static: true
*titleTF.name: titleTF
*titleTF.parent: form10
*titleTF.width: 220
*titleTF.isCompound: "true"
*titleTF.compoundIcon: "textfield.xpm"
*titleTF.compoundName: "text_Field"
*titleTF.x: 114
*titleTF.y: 168
*titleTF.background: "LightSkyBlue3"
*titleTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*titleTF.maxLength: 10
*titleTF.columns: 10
*titleTF.activateCallback.source: public
*titleTF.activateCallback: XmProcessTraversal
*titleTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*titleTF.losingFocusCallback.source: public
*titleTF.losingFocusCallback: search_users_losing_focusCb
*titleTF.losingFocusCallbackClientData: (XtPointer) 6

*cityTF.class: textField
*cityTF.static: true
*cityTF.name: cityTF
*cityTF.parent: form10
*cityTF.width: 342
*cityTF.isCompound: "true"
*cityTF.compoundIcon: "textfield.xpm"
*cityTF.compoundName: "text_Field"
*cityTF.x: 114
*cityTF.y: 244
*cityTF.background: "LightSkyBlue3"
*cityTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*cityTF.maxLength: 30
*cityTF.columns: 30
*cityTF.activateCallback.source: public
*cityTF.activateCallback: XmProcessTraversal
*cityTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*cityTF.losingFocusCallback.source: public
*cityTF.losingFocusCallback: search_users_losing_focusCb
*cityTF.losingFocusCallbackClientData: (XtPointer)8

*stateTF.class: textField
*stateTF.static: true
*stateTF.name: stateTF
*stateTF.parent: form10
*stateTF.width: 342
*stateTF.isCompound: "true"
*stateTF.compoundIcon: "textfield.xpm"
*stateTF.compoundName: "text_Field"
*stateTF.x: 114
*stateTF.y: 286
*stateTF.background: "LightSkyBlue3"
*stateTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*stateTF.maxLength: 20
*stateTF.columns: 20
*stateTF.activateCallback.source: public
*stateTF.activateCallback: XmProcessTraversal
*stateTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*stateTF.losingFocusCallback.source: public
*stateTF.losingFocusCallback: search_users_losing_focusCb
*stateTF.losingFocusCallbackClientData: (XtPointer) 9

*countryTF.class: textField
*countryTF.static: true
*countryTF.name: countryTF
*countryTF.parent: form10
*countryTF.width: 342
*countryTF.isCompound: "true"
*countryTF.compoundIcon: "textfield.xpm"
*countryTF.compoundName: "text_Field"
*countryTF.x: 114
*countryTF.y: 326
*countryTF.background: "LightSkyBlue3"
*countryTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*countryTF.maxLength: 20
*countryTF.columns: 20
*countryTF.activateCallback.source: public
*countryTF.activateCallback: XmProcessTraversal
*countryTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*countryTF.losingFocusCallback.source: public
*countryTF.losingFocusCallback: search_users_losing_focusCb
*countryTF.losingFocusCallbackClientData: (XtPointer)10

*zipTF.class: textField
*zipTF.static: true
*zipTF.name: zipTF
*zipTF.parent: form10
*zipTF.width: 342
*zipTF.isCompound: "true"
*zipTF.compoundIcon: "textfield.xpm"
*zipTF.compoundName: "text_Field"
*zipTF.x: 114
*zipTF.y: 366
*zipTF.background: "LightSkyBlue3"
*zipTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*zipTF.maxLength: 10
*zipTF.columns: 10
*zipTF.activateCallback.source: public
*zipTF.activateCallback: XmProcessTraversal
*zipTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*zipTF.losingFocusCallback.source: public
*zipTF.losingFocusCallback: search_users_losing_focusCb
*zipTF.losingFocusCallbackClientData: (XtPointer)11

*phoneTF.class: textField
*phoneTF.static: true
*phoneTF.name: phoneTF
*phoneTF.parent: form10
*phoneTF.width: 342
*phoneTF.isCompound: "true"
*phoneTF.compoundIcon: "textfield.xpm"
*phoneTF.compoundName: "text_Field"
*phoneTF.x: 114
*phoneTF.y: 406
*phoneTF.background: "LightSkyBlue3"
*phoneTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*phoneTF.maxLength: 21
*phoneTF.columns: 21
*phoneTF.activateCallback.source: public
*phoneTF.activateCallback: XmProcessTraversal
*phoneTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*phoneTF.losingFocusCallback.source: public
*phoneTF.losingFocusCallback: search_users_losing_focusCb
*phoneTF.losingFocusCallbackClientData: (XtPointer) 12

*faxTF.class: textField
*faxTF.static: true
*faxTF.name: faxTF
*faxTF.parent: form10
*faxTF.width: 342
*faxTF.isCompound: "true"
*faxTF.compoundIcon: "textfield.xpm"
*faxTF.compoundName: "text_Field"
*faxTF.x: 114
*faxTF.y: 448
*faxTF.background: "LightSkyBlue3"
*faxTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*faxTF.maxLength: 21
*faxTF.columns: 21
*faxTF.activateCallback.source: public
*faxTF.activateCallback: XmProcessTraversal
*faxTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*faxTF.losingFocusCallback.source: public
*faxTF.losingFocusCallback: search_users_losing_focusCb
*faxTF.losingFocusCallbackClientData: (XtPointer)13

*emailTF.class: textField
*emailTF.static: true
*emailTF.name: emailTF
*emailTF.parent: form10
*emailTF.width: 342
*emailTF.isCompound: "true"
*emailTF.compoundIcon: "textfield.xpm"
*emailTF.compoundName: "text_Field"
*emailTF.x: 114
*emailTF.y: 488
*emailTF.background: "LightSkyBlue3"
*emailTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*emailTF.maxLength: 128
*emailTF.columns: 128
*emailTF.activateCallback.source: public
*emailTF.activateCallback: XmProcessTraversal
*emailTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*emailTF.losingFocusCallback.source: public
*emailTF.losingFocusCallback: search_users_losing_focusCb
*emailTF.losingFocusCallbackClientData: (XtPointer)14

*label99.class: label
*label99.static: true
*label99.name: label99
*label99.parent: form10
*label99.isCompound: "true"
*label99.compoundIcon: "label.xpm"
*label99.compoundName: "label_"
*label99.x: 0
*label99.y: 450
*label99.background: "#9ac0cd"
*label99.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label99.labelString: "Fax:"
*label99.width: 52

*label100.class: label
*label100.static: true
*label100.name: label100
*label100.parent: form10
*label100.isCompound: "true"
*label100.compoundIcon: "label.xpm"
*label100.compoundName: "label_"
*label100.x: 4
*label100.y: 542
*label100.background: "#9ac0cd"
*label100.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label100.labelString: "FTP Dir:"
*label100.width: 82

*option_menu_type.class: rowColumn
*option_menu_type.static: true
*option_menu_type.name: option_menu_type
*option_menu_type.parent: form10
*option_menu_type.rowColumnType: "menu_option"
*option_menu_type.subMenuId: "option_menu_pane_type"
*option_menu_type.isCompound: "true"
*option_menu_type.compoundIcon: "optionM.xpm"
*option_menu_type.compoundName: "option_Menu"
*option_menu_type.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*option_menu_type.x: 336
*option_menu_type.y: 124
*option_menu_type.background: "#9ac0cd"
*option_menu_type.labelString: " "
*option_menu_type.width: 90
*option_menu_type.resizeWidth: "true"

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
*option_menu_typePB.activateCallback: search_users_option_typeCb
*option_menu_typePB.width: 79
*option_menu_typePB.createCallbackClientData: (XtPointer) 1
*option_menu_typePB.activateCallbackClientData: (XtPointer) 0
*option_menu_typePB.createCallback.source: public
*option_menu_typePB.createCallback: search_users_option_typeCb

*label101.class: label
*label101.static: true
*label101.name: label101
*label101.parent: form10
*label101.isCompound: "true"
*label101.compoundIcon: "label.xpm"
*label101.compoundName: "label_"
*label101.x: 8
*label101.y: 488
*label101.background: "#9ac0cd"
*label101.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label101.labelString: "Email:"
*label101.width: 52

*ftp_dirSW.class: scrolledWindow
*ftp_dirSW.static: true
*ftp_dirSW.name: ftp_dirSW
*ftp_dirSW.parent: form10
*ftp_dirSW.scrollingPolicy: "application_defined"
*ftp_dirSW.visualPolicy: "variable"
*ftp_dirSW.scrollBarDisplayPolicy: "static"
*ftp_dirSW.isCompound: "true"
*ftp_dirSW.compoundIcon: "scrltext.xpm"
*ftp_dirSW.compoundName: "scrolled_Text"
*ftp_dirSW.x: 114
*ftp_dirSW.y: 535
*ftp_dirSW.background: "#9ac0cd"
*ftp_dirSW.width: 362
*ftp_dirSW.height: 35
*ftp_dirSW.shadowThickness: 1

*ftp_dirST.class: scrolledText
*ftp_dirST.static: true
*ftp_dirST.name: ftp_dirST
*ftp_dirST.parent: ftp_dirSW
*ftp_dirST.width: 340
*ftp_dirST.height: 35
*ftp_dirST.editMode: "multi_line_edit"
*ftp_dirST.maxLength: 255
*ftp_dirST.wordWrap: "true"
*ftp_dirST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*ftp_dirST.columns: 29
*ftp_dirST.rows: 2
*ftp_dirST.scrollHorizontal: "false"
*ftp_dirST.resizeHeight: "false"
*ftp_dirST.resizeWidth: "false"
*ftp_dirST.scrollVertical: "true"
*ftp_dirST.background: "LightSkyBlue3"
*ftp_dirST.activateCallback.source: public
*ftp_dirST.activateCallback: XmProcessTraversal
*ftp_dirST.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*ftp_dirST.losingFocusCallback.source: public
*ftp_dirST.losingFocusCallback: search_users_losing_focusCb
*ftp_dirST.losingFocusCallbackClientData: (XtPointer) 15

*label154.class: label
*label154.static: true
*label154.name: label154
*label154.parent: form10
*label154.isCompound: "true"
*label154.compoundIcon: "label.xpm"
*label154.compoundName: "label_"
*label154.x: 7
*label154.y: 585
*label154.background: "#9ac0cd"
*label154.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label154.labelString: "Access Dir:"
*label154.width: 96

*access_dirSW.class: scrolledWindow
*access_dirSW.static: true
*access_dirSW.name: access_dirSW
*access_dirSW.parent: form10
*access_dirSW.scrollingPolicy: "application_defined"
*access_dirSW.visualPolicy: "variable"
*access_dirSW.scrollBarDisplayPolicy: "static"
*access_dirSW.isCompound: "true"
*access_dirSW.compoundIcon: "scrltext.xpm"
*access_dirSW.compoundName: "scrolled_Text"
*access_dirSW.x: 114
*access_dirSW.y: 579
*access_dirSW.background: "#9ac0cd"
*access_dirSW.width: 362
*access_dirSW.height: 35
*access_dirSW.shadowThickness: 1

*access_dirST.class: scrolledText
*access_dirST.static: true
*access_dirST.name: access_dirST
*access_dirST.parent: access_dirSW
*access_dirST.width: 340
*access_dirST.height: 35
*access_dirST.editMode: "multi_line_edit"
*access_dirST.maxLength: 255
*access_dirST.wordWrap: "true"
*access_dirST.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*access_dirST.columns: 29
*access_dirST.rows: 2
*access_dirST.scrollHorizontal: "false"
*access_dirST.resizeHeight: "false"
*access_dirST.resizeWidth: "false"
*access_dirST.scrollVertical: "true"
*access_dirST.background: "LightSkyBlue3"
*access_dirST.activateCallback.source: public
*access_dirST.activateCallback: XmProcessTraversal
*access_dirST.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*access_dirST.losingFocusCallback.source: public
*access_dirST.losingFocusCallback: search_users_losing_focusCb
*access_dirST.losingFocusCallbackClientData: (XtPointer) 16

*label102.class: label
*label102.static: true
*label102.name: label102
*label102.parent: search_users
*label102.isCompound: "true"
*label102.compoundIcon: "label.xpm"
*label102.compoundName: "label_"
*label102.x: 172
*label102.y: 20
*label102.background: "#9ac0cd"
*label102.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label102.labelString: "Search Users Parameters"

*closeButton.class: pushButton
*closeButton.static: true
*closeButton.name: closeButton
*closeButton.parent: search_users
*closeButton.isCompound: "true"
*closeButton.compoundIcon: "push.xpm"
*closeButton.compoundName: "push_Button"
*closeButton.x: 402
*closeButton.y: 624
*closeButton.width: 138
*closeButton.height: 34
*closeButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closeButton.labelString: "Close"
*closeButton.shadowThickness: 4
*closeButton.background: "CadetBlue"
*closeButton.activateCallback.source: public
*closeButton.activateCallback: search_users_closeCb

*start_searchPB.class: pushButton
*start_searchPB.static: true
*start_searchPB.name: start_searchPB
*start_searchPB.parent: search_users
*start_searchPB.isCompound: "true"
*start_searchPB.compoundIcon: "push.xpm"
*start_searchPB.compoundName: "push_Button"
*start_searchPB.x: 12
*start_searchPB.y: 624
*start_searchPB.width: 138
*start_searchPB.height: 34
*start_searchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*start_searchPB.labelString: "Start Search"
*start_searchPB.shadowThickness: 4
*start_searchPB.background: "CadetBlue"
*start_searchPB.activateCallback.source: public
*start_searchPB.activateCallback: search_users_start_searchCb

