! UIMX ascii 2.9 key: 9238                                                      

*user_data.class: form
*user_data.classinc:
*user_data.classspec:
*user_data.classmembers:
*user_data.classconstructor:
*user_data.classdestructor:
*user_data.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>\

*user_data.ispecdecl:
*user_data.funcdecl: /* if create = CREATE => create user otherwise show user data */\
swidget create_user_data(swidget UxParent, /*ACCOUNT_USER_DATA*/ int  create)
*user_data.funcname: create_user_data
*user_data.funcdef: "swidget", "<create_user_data>(%)"
*user_data.argdecl: swidget UxParent;\
int create;
*user_data.arglist: UxParent, create
*user_data.arglist.UxParent: "swidget", "%UxParent%"
*user_data.arglist.create: "int", "%create%"
*user_data.icode:
*user_data.fcode: return(rtrn);\

*user_data.auxdecl:
*user_data.name.source: public
*user_data.static: false
*user_data.name: user_data
*user_data.parent: NO_PARENT
*user_data.parentExpression: UxParent
*user_data.defaultShell: transientShell
*user_data.width: 559
*user_data.height: 671
*user_data.resizePolicy: "resize_none"
*user_data.isCompound: "true"
*user_data.compoundIcon: "form.xpm"
*user_data.compoundName: "form_"
*user_data.x: 538
*user_data.y: 48
*user_data.unitType: "pixels"
*user_data.background: "#9ac0cd"

*scrolledWindow2.class: scrolledWindow
*scrolledWindow2.static: true
*scrolledWindow2.name: scrolledWindow2
*scrolledWindow2.parent: user_data
*scrolledWindow2.scrollingPolicy: "automatic"
*scrolledWindow2.width: 530
*scrolledWindow2.height: 566
*scrolledWindow2.isCompound: "true"
*scrolledWindow2.compoundIcon: "scrlwnd.xpm"
*scrolledWindow2.compoundName: "scrolled_Window"
*scrolledWindow2.x: 16
*scrolledWindow2.y: 52
*scrolledWindow2.background: "#9ac0cd"

*form5.class: form
*form5.static: true
*form5.name: form5
*form5.parent: scrolledWindow2
*form5.width: 496
*form5.height: 758
*form5.resizePolicy: "resize_none"
*form5.isCompound: "true"
*form5.compoundIcon: "form.xpm"
*form5.compoundName: "form_"
*form5.x: 2
*form5.y: -2
*form5.background: "#9ac0cd"

*label25.class: label
*label25.static: true
*label25.name: label25
*label25.parent: form5
*label25.isCompound: "true"
*label25.compoundIcon: "label.xpm"
*label25.compoundName: "label_"
*label25.x: 8
*label25.y: 90
*label25.background: "#9ac0cd"
*label25.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label25.labelString: "Last Name:"

*label26.class: label
*label26.static: true
*label26.name: label26
*label26.parent: form5
*label26.isCompound: "true"
*label26.compoundIcon: "label.xpm"
*label26.compoundName: "label_"
*label26.x: 352
*label26.y: 50
*label26.background: "#9ac0cd"
*label26.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label26.labelString: "M.I.:"

*idTF.class: textField
*idTF.static: true
*idTF.name: idTF
*idTF.parent: form5
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
*idTF.losingFocusCallbackClientData: (XtPointer) 1
*idTF.valueChangedCallbackClientData: (XtPointer) 1
*idTF.createCallback.source: public
*idTF.createCallback: user_data_user_idCb
*idTF.createCallbackClientData: (XtPointer) 1

*m_iTF.class: textField
*m_iTF.static: true
*m_iTF.name: m_iTF
*m_iTF.parent: form5
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
*m_iTF.losingFocusCallbackClientData: (XtPointer)3
*m_iTF.activateCallback.source: public
*m_iTF.activateCallback: XmProcessTraversal
*m_iTF.activateCallbackClientData: (XtPointer)XmTRAVERSE_NEXT_TAB_GROUP
*m_iTF.valueChangedCallbackClientData: (XtPointer) 3

*label27.class: label
*label27.static: true
*label27.name: label27
*label27.parent: form5
*label27.isCompound: "true"
*label27.compoundIcon: "label.xpm"
*label27.compoundName: "label_"
*label27.x: 8
*label27.y: 132
*label27.background: "#9ac0cd"
*label27.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label27.labelString: "Type:"

*label28.class: label
*label28.static: true
*label28.name: label28
*label28.parent: form5
*label28.isCompound: "true"
*label28.compoundIcon: "label.xpm"
*label28.compoundName: "label_"
*label28.x: 10
*label28.y: 254
*label28.background: "#9ac0cd"
*label28.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label28.labelString: "Title:"

*label29.class: label
*label29.static: true
*label29.name: label29
*label29.parent: form5
*label29.isCompound: "true"
*label29.compoundIcon: "label.xpm"
*label29.compoundName: "label_"
*label29.x: 8
*label29.y: 372
*label29.background: "#9ac0cd"
*label29.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label29.labelString: "City:"

*label30.class: label
*label30.static: true
*label30.name: label30
*label30.parent: form5
*label30.isCompound: "true"
*label30.compoundIcon: "label.xpm"
*label30.compoundName: "label_"
*label30.x: 8
*label30.y: 414
*label30.background: "#9ac0cd"
*label30.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label30.labelString: "State:"
*label30.height: 28

*label31.class: label
*label31.static: true
*label31.name: label31
*label31.parent: form5
*label31.isCompound: "true"
*label31.compoundIcon: "label.xpm"
*label31.compoundName: "label_"
*label31.x: 8
*label31.y: 336
*label31.background: "#9ac0cd"
*label31.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label31.labelString: "Address:"

*label32.class: label
*label32.static: true
*label32.name: label32
*label32.parent: form5
*label32.isCompound: "true"
*label32.compoundIcon: "label.xpm"
*label32.compoundName: "label_"
*label32.x: 8
*label32.y: 492
*label32.background: "#9ac0cd"
*label32.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label32.labelString: "Zip:"

*label33.class: label
*label33.static: true
*label33.name: label33
*label33.parent: form5
*label33.isCompound: "true"
*label33.compoundIcon: "label.xpm"
*label33.compoundName: "label_"
*label33.x: 8
*label33.y: 456
*label33.background: "#9ac0cd"
*label33.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label33.labelString: "Country:"

*label34.class: label
*label34.static: true
*label34.name: label34
*label34.parent: form5
*label34.isCompound: "true"
*label34.compoundIcon: "label.xpm"
*label34.compoundName: "label_"
*label34.x: 8
*label34.y: 534
*label34.background: "#9ac0cd"
*label34.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label34.labelString: "Phone:"
*label34.width: 52

*IDLB.class: label
*IDLB.static: true
*IDLB.name: IDLB
*IDLB.parent: form5
*IDLB.isCompound: "true"
*IDLB.compoundIcon: "label.xpm"
*IDLB.compoundName: "label_"
*IDLB.x: 8
*IDLB.y: 12
*IDLB.background: "#9ac0cd"
*IDLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*IDLB.labelString: "ID:"

*first_nameLB.class: label
*first_nameLB.static: true
*first_nameLB.name: first_nameLB
*first_nameLB.parent: form5
*first_nameLB.isCompound: "true"
*first_nameLB.compoundIcon: "label.xpm"
*first_nameLB.compoundName: "label_"
*first_nameLB.x: 8
*first_nameLB.y: 50
*first_nameLB.background: "#9ac0cd"
*first_nameLB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*first_nameLB.labelString: "First Name:"

*first_nameTF.class: textField
*first_nameTF.static: true
*first_nameTF.name: first_nameTF
*first_nameTF.parent: form5
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
*first_nameTF.losingFocusCallbackClientData: (XtPointer)2
*first_nameTF.valueChangedCallbackClientData: (XtPointer) 2

*last_nameTF.class: textField
*last_nameTF.static: true
*last_nameTF.name: last_nameTF
*last_nameTF.parent: form5
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
*last_nameTF.losingFocusCallbackClientData: (XtPointer)4
*last_nameTF.valueChangedCallbackClientData: (XtPointer) 4

*label37.class: label
*label37.static: true
*label37.name: label37
*label37.parent: form5
*label37.isCompound: "true"
*label37.compoundIcon: "label.xpm"
*label37.compoundName: "label_"
*label37.x: 8
*label37.y: 294
*label37.background: "#9ac0cd"
*label37.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label37.labelString: "Organization:"

*organizationTF.class: textField
*organizationTF.static: true
*organizationTF.name: organizationTF
*organizationTF.parent: form5
*organizationTF.width: 342
*organizationTF.isCompound: "true"
*organizationTF.compoundIcon: "textfield.xpm"
*organizationTF.compoundName: "text_Field"
*organizationTF.x: 114
*organizationTF.y: 286
*organizationTF.background: "LightSkyBlue3"
*organizationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*organizationTF.maxLength: 35
*organizationTF.columns: 35
*organizationTF.text: ""
*organizationTF.height: 35
*organizationTF.activateCallback.source: public
*organizationTF.activateCallback: XmProcessTraversal
*organizationTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*organizationTF.losingFocusCallbackClientData: (XtPointer)9
*organizationTF.valueChangedCallbackClientData: (XtPointer) 9

*titleTF.class: textField
*titleTF.static: true
*titleTF.name: titleTF
*titleTF.parent: form5
*titleTF.width: 220
*titleTF.isCompound: "true"
*titleTF.compoundIcon: "textfield.xpm"
*titleTF.compoundName: "text_Field"
*titleTF.x: 114
*titleTF.y: 244
*titleTF.background: "LightSkyBlue3"
*titleTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*titleTF.maxLength: 10
*titleTF.columns: 10
*titleTF.height: 35
*titleTF.activateCallback.source: public
*titleTF.activateCallback: XmProcessTraversal
*titleTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*titleTF.losingFocusCallbackClientData: (XtPointer)8
*titleTF.valueChangedCallbackClientData: (XtPointer) 8

*addressTF.class: textField
*addressTF.static: true
*addressTF.name: addressTF
*addressTF.parent: form5
*addressTF.width: 342
*addressTF.isCompound: "true"
*addressTF.compoundIcon: "textfield.xpm"
*addressTF.compoundName: "text_Field"
*addressTF.x: 114
*addressTF.y: 326
*addressTF.background: "LightSkyBlue3"
*addressTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*addressTF.maxLength: 128
*addressTF.columns: 128
*addressTF.height: 35
*addressTF.activateCallback.source: public
*addressTF.activateCallback: XmProcessTraversal
*addressTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*addressTF.losingFocusCallbackClientData: (XtPointer)10
*addressTF.valueChangedCallbackClientData: (XtPointer) 10

*cityTF.class: textField
*cityTF.static: true
*cityTF.name: cityTF
*cityTF.parent: form5
*cityTF.width: 342
*cityTF.isCompound: "true"
*cityTF.compoundIcon: "textfield.xpm"
*cityTF.compoundName: "text_Field"
*cityTF.x: 114
*cityTF.y: 364
*cityTF.background: "LightSkyBlue3"
*cityTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*cityTF.maxLength: 30
*cityTF.columns: 30
*cityTF.height: 35
*cityTF.activateCallback.source: public
*cityTF.activateCallback: XmProcessTraversal
*cityTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*cityTF.losingFocusCallbackClientData: (XtPointer) 11
*cityTF.valueChangedCallbackClientData: (XtPointer) 11

*stateTF.class: textField
*stateTF.static: true
*stateTF.name: stateTF
*stateTF.parent: form5
*stateTF.width: 342
*stateTF.isCompound: "true"
*stateTF.compoundIcon: "textfield.xpm"
*stateTF.compoundName: "text_Field"
*stateTF.x: 114
*stateTF.y: 406
*stateTF.background: "LightSkyBlue3"
*stateTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*stateTF.maxLength: 20
*stateTF.columns: 20
*stateTF.height: 35
*stateTF.activateCallback.source: public
*stateTF.activateCallback: XmProcessTraversal
*stateTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*stateTF.losingFocusCallbackClientData: (XtPointer)12
*stateTF.valueChangedCallbackClientData: (XtPointer) 12

*countryTF.class: textField
*countryTF.static: true
*countryTF.name: countryTF
*countryTF.parent: form5
*countryTF.width: 342
*countryTF.isCompound: "true"
*countryTF.compoundIcon: "textfield.xpm"
*countryTF.compoundName: "text_Field"
*countryTF.x: 114
*countryTF.y: 446
*countryTF.background: "LightSkyBlue3"
*countryTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*countryTF.maxLength: 20
*countryTF.columns: 20
*countryTF.height: 35
*countryTF.activateCallback.source: public
*countryTF.activateCallback: XmProcessTraversal
*countryTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*countryTF.losingFocusCallbackClientData: (XtPointer) 13
*countryTF.valueChangedCallbackClientData: (XtPointer) 13

*zipTF.class: textField
*zipTF.static: true
*zipTF.name: zipTF
*zipTF.parent: form5
*zipTF.width: 342
*zipTF.isCompound: "true"
*zipTF.compoundIcon: "textfield.xpm"
*zipTF.compoundName: "text_Field"
*zipTF.x: 114
*zipTF.y: 486
*zipTF.background: "LightSkyBlue3"
*zipTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*zipTF.maxLength: 10
*zipTF.columns: 10
*zipTF.height: 35
*zipTF.activateCallback.source: public
*zipTF.activateCallback: XmProcessTraversal
*zipTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*zipTF.losingFocusCallbackClientData: (XtPointer)14
*zipTF.valueChangedCallbackClientData: (XtPointer) 14

*phoneTF.class: textField
*phoneTF.static: true
*phoneTF.name: phoneTF
*phoneTF.parent: form5
*phoneTF.width: 342
*phoneTF.isCompound: "true"
*phoneTF.compoundIcon: "textfield.xpm"
*phoneTF.compoundName: "text_Field"
*phoneTF.x: 114
*phoneTF.y: 526
*phoneTF.background: "LightSkyBlue3"
*phoneTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*phoneTF.maxLength: 25
*phoneTF.columns: 25
*phoneTF.height: 35
*phoneTF.activateCallback.source: public
*phoneTF.activateCallback: XmProcessTraversal
*phoneTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*phoneTF.losingFocusCallbackClientData: (XtPointer)15
*phoneTF.valueChangedCallbackClientData: (XtPointer) 15

*faxTF.class: textField
*faxTF.static: true
*faxTF.name: faxTF
*faxTF.parent: form5
*faxTF.width: 342
*faxTF.isCompound: "true"
*faxTF.compoundIcon: "textfield.xpm"
*faxTF.compoundName: "text_Field"
*faxTF.x: 114
*faxTF.y: 568
*faxTF.background: "LightSkyBlue3"
*faxTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*faxTF.maxLength: 25
*faxTF.columns: 25
*faxTF.height: 35
*faxTF.activateCallback.source: public
*faxTF.activateCallback: XmProcessTraversal
*faxTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*faxTF.losingFocusCallbackClientData: (XtPointer)16
*faxTF.valueChangedCallbackClientData: (XtPointer) 16

*emailTF.class: textField
*emailTF.static: true
*emailTF.name: emailTF
*emailTF.parent: form5
*emailTF.width: 342
*emailTF.isCompound: "true"
*emailTF.compoundIcon: "textfield.xpm"
*emailTF.compoundName: "text_Field"
*emailTF.x: 114
*emailTF.y: 608
*emailTF.background: "LightSkyBlue3"
*emailTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*emailTF.maxLength: 128
*emailTF.columns: 128
*emailTF.height: 35
*emailTF.activateCallback.source: public
*emailTF.activateCallback: XmProcessTraversal
*emailTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*emailTF.losingFocusCallbackClientData: (XtPointer)17
*emailTF.valueChangedCallbackClientData: (XtPointer) 17

*label38.class: label
*label38.static: true
*label38.name: label38
*label38.parent: form5
*label38.isCompound: "true"
*label38.compoundIcon: "label.xpm"
*label38.compoundName: "label_"
*label38.x: 2
*label38.y: 576
*label38.background: "#9ac0cd"
*label38.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label38.labelString: "Fax:"
*label38.width: 52

*label40.class: label
*label40.static: true
*label40.name: label40
*label40.parent: form5
*label40.isCompound: "true"
*label40.compoundIcon: "label.xpm"
*label40.compoundName: "label_"
*label40.x: -1
*label40.y: 658
*label40.background: "#9ac0cd"
*label40.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label40.labelString: "FTP Dir:"
*label40.width: 91

*priorityTF.class: textField
*priorityTF.static: true
*priorityTF.name: priorityTF
*priorityTF.parent: form5
*priorityTF.width: 220
*priorityTF.isCompound: "true"
*priorityTF.compoundIcon: "textfield.xpm"
*priorityTF.compoundName: "text_Field"
*priorityTF.x: 114
*priorityTF.y: 166
*priorityTF.background: "LightSkyBlue3"
*priorityTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*priorityTF.maxLength: 20
*priorityTF.columns: 20
*priorityTF.height: 35
*priorityTF.editable: "false"
*priorityTF.activateCallback.source: public
*priorityTF.activateCallback: XmProcessTraversal
*priorityTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*priorityTF.losingFocusCallbackClientData: (XtPointer)6
*priorityTF.valueChangedCallbackClientData: (XtPointer) 6

*typeTF.class: textField
*typeTF.static: true
*typeTF.name: typeTF
*typeTF.parent: form5
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
*typeTF.activateCallback.source: public
*typeTF.activateCallback: XmProcessTraversal
*typeTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*typeTF.losingFocusCallbackClientData: (XtPointer)5
*typeTF.valueChangedCallbackClientData: (XtPointer) 5

*label42.class: label
*label42.static: true
*label42.name: label42
*label42.parent: form5
*label42.isCompound: "true"
*label42.compoundIcon: "label.xpm"
*label42.compoundName: "label_"
*label42.x: 8
*label42.y: 210
*label42.background: "#9ac0cd"
*label42.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label42.labelString: "Password:"

*passwordTF.class: textField
*passwordTF.static: true
*passwordTF.name: passwordTF
*passwordTF.parent: form5
*passwordTF.width: 220
*passwordTF.isCompound: "true"
*passwordTF.compoundIcon: "textfield.xpm"
*passwordTF.compoundName: "text_Field"
*passwordTF.x: 114
*passwordTF.y: 206
*passwordTF.background: "LightSkyBlue3"
*passwordTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*passwordTF.maxLength: 20
*passwordTF.columns: 20
*passwordTF.height: 35
*passwordTF.activateCallback.source: public
*passwordTF.activateCallback: XmProcessTraversal
*passwordTF.activateCallbackClientData: (XtPointer) XmTRAVERSE_NEXT_TAB_GROUP
*passwordTF.losingFocusCallbackClientData: (XtPointer)7
*passwordTF.valueChangedCallbackClientData: (XtPointer) 7

*label43.class: label
*label43.static: true
*label43.name: label43
*label43.parent: form5
*label43.isCompound: "true"
*label43.compoundIcon: "label.xpm"
*label43.compoundName: "label_"
*label43.x: 8
*label43.y: 614
*label43.background: "#9ac0cd"
*label43.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label43.labelString: "Email:"
*label43.width: 52

*ftp_dirSW.class: scrolledWindow
*ftp_dirSW.static: true
*ftp_dirSW.name: ftp_dirSW
*ftp_dirSW.parent: form5
*ftp_dirSW.scrollingPolicy: "application_defined"
*ftp_dirSW.visualPolicy: "variable"
*ftp_dirSW.scrollBarDisplayPolicy: "static"
*ftp_dirSW.isCompound: "true"
*ftp_dirSW.compoundIcon: "scrltext.xpm"
*ftp_dirSW.compoundName: "scrolled_Text"
*ftp_dirSW.x: 115
*ftp_dirSW.y: 653
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
*ftp_dirST.losingFocusCallbackClientData: (XtPointer)18
*ftp_dirST.valueChangedCallbackClientData: (XtPointer) 18

*label55.class: label
*label55.static: true
*label55.name: label55
*label55.parent: form5
*label55.isCompound: "true"
*label55.compoundIcon: "label.xpm"
*label55.compoundName: "label_"
*label55.x: 8
*label55.y: 170
*label55.background: "#9ac0cd"
*label55.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label55.labelString: "Priority:"

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
*option_menu_type.x: 336
*option_menu_type.y: 124
*option_menu_type.background: "#9ac0cd"
*option_menu_type.labelString: " "
*option_menu_type.width: 133

*option_menu_pane_type.class: rowColumn
*option_menu_pane_type.static: true
*option_menu_pane_type.name: option_menu_pane_type
*option_menu_pane_type.parent: option_menu_type
*option_menu_pane_type.rowColumnType: "menu_pulldown"
*option_menu_pane_type.background: "#9ac0cd"
*option_menu_pane_type.width: 90
*option_menu_pane_type.resizeWidth: "false"

*option_menu_typePB.class: pushButton
*option_menu_typePB.static: true
*option_menu_typePB.name: option_menu_typePB
*option_menu_typePB.parent: option_menu_pane_type
*option_menu_typePB.labelString: "NONE"
*option_menu_typePB.background: "#9ac0cd"
*option_menu_typePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*option_menu_typePB.activateCallback.source: public
*option_menu_typePB.activateCallback: user_data_option_typeCb
*option_menu_typePB.createCallback.source: public
*option_menu_typePB.createCallback: user_data_option_typeCb
*option_menu_typePB.createCallbackClientData: (XtPointer) 1
*option_menu_typePB.activateCallbackClientData: (XtPointer) 0

*option_menu_priority.class: rowColumn
*option_menu_priority.static: true
*option_menu_priority.name: option_menu_priority
*option_menu_priority.parent: form5
*option_menu_priority.rowColumnType: "menu_option"
*option_menu_priority.subMenuId: "option_menu_pane_priority"
*option_menu_priority.isCompound: "true"
*option_menu_priority.compoundIcon: "optionM.xpm"
*option_menu_priority.compoundName: "option_Menu"
*option_menu_priority.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*option_menu_priority.x: 336
*option_menu_priority.y: 164
*option_menu_priority.background: "#9ac0cd"
*option_menu_priority.labelString: " "
*option_menu_priority.width: 133

*option_menu_pane_priority.class: rowColumn
*option_menu_pane_priority.static: true
*option_menu_pane_priority.name: option_menu_pane_priority
*option_menu_pane_priority.parent: option_menu_priority
*option_menu_pane_priority.rowColumnType: "menu_pulldown"
*option_menu_pane_priority.background: "#9ac0cd"
*option_menu_pane_priority.width: 90
*option_menu_pane_priority.resizeWidth: "false"

*option_menu_priorityPB.class: pushButton
*option_menu_priorityPB.static: true
*option_menu_priorityPB.name: option_menu_priorityPB
*option_menu_priorityPB.parent: option_menu_pane_priority
*option_menu_priorityPB.labelString: "NONE"
*option_menu_priorityPB.background: "#9ac0cd"
*option_menu_priorityPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*option_menu_priorityPB.activateCallback.source: public
*option_menu_priorityPB.activateCallback: user_data_option_priorityCb
*option_menu_priorityPB.createCallback.source: public
*option_menu_priorityPB.createCallback: user_data_option_priorityCb
*option_menu_priorityPB.createCallbackClientData: (XtPointer) 1
*option_menu_priorityPB.activateCallbackClientData: (XtPointer) 0

*access_dirSW.class: scrolledWindow
*access_dirSW.static: true
*access_dirSW.name: access_dirSW
*access_dirSW.parent: form5
*access_dirSW.scrollingPolicy: "application_defined"
*access_dirSW.visualPolicy: "variable"
*access_dirSW.scrollBarDisplayPolicy: "static"
*access_dirSW.isCompound: "true"
*access_dirSW.compoundIcon: "scrltext.xpm"
*access_dirSW.compoundName: "scrolled_Text"
*access_dirSW.x: 115
*access_dirSW.y: 697
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
*access_dirST.losingFocusCallbackClientData: (XtPointer)18
*access_dirST.valueChangedCallbackClientData: (XtPointer) 18

*label153.class: label
*label153.static: true
*label153.name: label153
*label153.parent: form5
*label153.isCompound: "true"
*label153.compoundIcon: "label.xpm"
*label153.compoundName: "label_"
*label153.x: 2
*label153.y: 703
*label153.background: "#9ac0cd"
*label153.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label153.labelString: "Access Dir:"
*label153.width: 102

*user_dataLB.class: label
*user_dataLB.name.source: public
*user_dataLB.static: false
*user_dataLB.name: user_dataLB
*user_dataLB.parent: user_data
*user_dataLB.isCompound: "true"
*user_dataLB.compoundIcon: "label.xpm"
*user_dataLB.compoundName: "label_"
*user_dataLB.x: 188
*user_dataLB.y: 22
*user_dataLB.background: "#9ac0cd"
*user_dataLB.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*user_dataLB.labelString: "User Data"
*user_dataLB.width: 182
*user_dataLB.createCallback.source: public
*user_dataLB.createCallback: user_data_labelCb

*createPB.class: pushButton
*createPB.static: true
*createPB.name: createPB
*createPB.parent: user_data
*createPB.isCompound: "true"
*createPB.compoundIcon: "push.xpm"
*createPB.compoundName: "push_Button"
*createPB.x: 18
*createPB.y: 628
*createPB.width: 138
*createPB.height: 34
*createPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*createPB.labelString: "Create"
*createPB.shadowThickness: 4
*createPB.background: "CadetBlue"
*createPB.activateCallback.source: public
*createPB.activateCallback: user_data_createCb
*createPB.createCallback.source: public
*createPB.createCallback: user_data_createCb
*createPB.createCallbackClientData: (XtPointer) 1
*createPB.recomputeSize: "false"
*createPB.activateCallbackClientData: (XtPointer)0

*closePB.class: pushButton
*closePB.static: true
*closePB.name: closePB
*closePB.parent: user_data
*closePB.isCompound: "true"
*closePB.compoundIcon: "push.xpm"
*closePB.compoundName: "push_Button"
*closePB.x: 404
*closePB.y: 628
*closePB.width: 138
*closePB.height: 34
*closePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closePB.labelString: "Close"
*closePB.shadowThickness: 4
*closePB.background: "CadetBlue"
*closePB.activateCallback.source: public
*closePB.activateCallback: user_data_closeCb

