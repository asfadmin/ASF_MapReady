! UIMX ascii 2.9 key: 2199                                                      

*accounts_users.class: form
*accounts_users.classinc:
*accounts_users.classspec:
*accounts_users.classmembers:
*accounts_users.classconstructor:
*accounts_users.classdestructor:
*accounts_users.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>\
\
\

*accounts_users.ispecdecl:
*accounts_users.funcdecl: swidget create_accounts_users(swidget UxParent)
*accounts_users.funcname: create_accounts_users
*accounts_users.funcdef: "swidget", "<create_accounts_users>(%)"
*accounts_users.argdecl: swidget UxParent;
*accounts_users.arglist: UxParent
*accounts_users.arglist.UxParent: "swidget", "%UxParent%"
*accounts_users.icode:
*accounts_users.fcode: return(rtrn);\

*accounts_users.auxdecl:
*accounts_users.static: true
*accounts_users.name: accounts_users
*accounts_users.parent: NO_PARENT
*accounts_users.parentExpression: UxParent
*accounts_users.defaultShell: transientShell
*accounts_users.width: 1023
*accounts_users.height: 770
*accounts_users.resizePolicy: "resize_none"
*accounts_users.isCompound: "true"
*accounts_users.compoundIcon: "form.xpm"
*accounts_users.compoundName: "form_"
*accounts_users.x: 100
*accounts_users.y: 5
*accounts_users.unitType: "pixels"
*accounts_users.background: "#9ac0cd"
*accounts_users.dialogTitle: "Users and Accounts Management"
*accounts_users.sensitive: "true"
*accounts_users.allowShellResize: "true"
*accounts_users.createManaged: "true"
*accounts_users.autoUnmanage: "true"

*acc_usrMB.class: rowColumn
*acc_usrMB.static: true
*acc_usrMB.name: acc_usrMB
*acc_usrMB.parent: accounts_users
*acc_usrMB.rowColumnType: "menu_bar"
*acc_usrMB.isCompound: "true"
*acc_usrMB.compoundIcon: "pulldownM.xpm"
*acc_usrMB.compoundName: "menu_Bar"
*acc_usrMB.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*acc_usrMB.x: 0
*acc_usrMB.y: 0
*acc_usrMB.background: "CadetBlue"
*acc_usrMB.shadowThickness: 3
*acc_usrMB.menuAccelerator: "<KeyUp>F10"
*acc_usrMB.menuHelpWidget: "acc_usrMB_top_b3"
*acc_usrMB.rightAttachment: "attach_form"
*acc_usrMB.leftAttachment: "attach_form"

*acc_usrMB_p1.class: rowColumn
*acc_usrMB_p1.static: true
*acc_usrMB_p1.name: acc_usrMB_p1
*acc_usrMB_p1.parent: acc_usrMB
*acc_usrMB_p1.rowColumnType: "menu_pulldown"
*acc_usrMB_p1.background: "CadetBlue"
*acc_usrMB_p1.marginWidth: 12

*acc_usrMB_p1_b1.class: pushButton
*acc_usrMB_p1_b1.static: true
*acc_usrMB_p1_b1.name: acc_usrMB_p1_b1
*acc_usrMB_p1_b1.parent: acc_usrMB_p1
*acc_usrMB_p1_b1.labelString: "Welcome"
*acc_usrMB_p1_b1.background: "CadetBlue"
*acc_usrMB_p1_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p1_b1.activateCallback.source: public
*acc_usrMB_p1_b1.activateCallback: accounts_users_goto_welcomeCb

*acc_usrMB_p3.class: rowColumn
*acc_usrMB_p3.static: true
*acc_usrMB_p3.name: acc_usrMB_p3
*acc_usrMB_p3.parent: acc_usrMB
*acc_usrMB_p3.rowColumnType: "menu_pulldown"

*acc_usrMB_p3_b2.class: pushButton
*acc_usrMB_p3_b2.static: true
*acc_usrMB_p3_b2.name: acc_usrMB_p3_b2
*acc_usrMB_p3_b2.parent: acc_usrMB_p3
*acc_usrMB_p3_b2.labelString: "Help"
*acc_usrMB_p3_b2.background: "Cadet Blue"
*acc_usrMB_p3_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p3_b2.activateCallback.source: public
*acc_usrMB_p3_b2.activateCallback: 

*acc_usrMB_p10.class: rowColumn
*acc_usrMB_p10.static: true
*acc_usrMB_p10.name: acc_usrMB_p10
*acc_usrMB_p10.parent: acc_usrMB
*acc_usrMB_p10.rowColumnType: "menu_pulldown"
*acc_usrMB_p10.background: "CadetBlue"

*acc_usrMB_p10_b1.class: pushButtonGadget
*acc_usrMB_p10_b1.static: true
*acc_usrMB_p10_b1.name: acc_usrMB_p10_b1
*acc_usrMB_p10_b1.parent: acc_usrMB_p10
*acc_usrMB_p10_b1.labelString: "Search"
*acc_usrMB_p10_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p10_b1.activateCallback.source: public
*acc_usrMB_p10_b1.activateCallback: accounts_users_search_usersCb

*acc_usrMB_p10_b3.class: separatorGadget
*acc_usrMB_p10_b3.static: true
*acc_usrMB_p10_b3.name: acc_usrMB_p10_b3
*acc_usrMB_p10_b3.parent: acc_usrMB_p10

*acc_usrMB_p10_b4.class: pushButtonGadget
*acc_usrMB_p10_b4.static: true
*acc_usrMB_p10_b4.name: acc_usrMB_p10_b4
*acc_usrMB_p10_b4.parent: acc_usrMB_p10
*acc_usrMB_p10_b4.labelString: "User Data"
*acc_usrMB_p10_b4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p10_b4.activateCallback.source: public
*acc_usrMB_p10_b4.activateCallback: accounts_users_create_userCb
*acc_usrMB_p10_b4.activateCallbackClientData: (XtPointer) 0

*acc_usrMB_p10_b7.class: separatorGadget
*acc_usrMB_p10_b7.static: true
*acc_usrMB_p10_b7.name: acc_usrMB_p10_b7
*acc_usrMB_p10_b7.parent: acc_usrMB_p10

*acc_usrMB_p10_b8.class: pushButtonGadget
*acc_usrMB_p10_b8.static: true
*acc_usrMB_p10_b8.name: acc_usrMB_p10_b8
*acc_usrMB_p10_b8.parent: acc_usrMB_p10
*acc_usrMB_p10_b8.labelString: "Create"
*acc_usrMB_p10_b8.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p10_b8.activateCallback.source: public
*acc_usrMB_p10_b8.activateCallback: accounts_users_create_userCb
*acc_usrMB_p10_b8.activateCallbackClientData: (XtPointer) 1

*acc_usrMB_p10_b9.class: pushButtonGadget
*acc_usrMB_p10_b9.static: true
*acc_usrMB_p10_b9.name: acc_usrMB_p10_b9
*acc_usrMB_p10_b9.parent: acc_usrMB_p10
*acc_usrMB_p10_b9.labelString: "Delete"
*acc_usrMB_p10_b9.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p10_b9.activateCallback.source: public
*acc_usrMB_p10_b9.activateCallback: accounts_users_delete_userCb

*acc_usrMB_p4.class: rowColumn
*acc_usrMB_p4.static: true
*acc_usrMB_p4.name: acc_usrMB_p4
*acc_usrMB_p4.parent: acc_usrMB
*acc_usrMB_p4.rowColumnType: "menu_pulldown"
*acc_usrMB_p4.background: "CadetBlue"

*acc_usrMB_p4_b2.class: pushButtonGadget
*acc_usrMB_p4_b2.static: true
*acc_usrMB_p4_b2.name: acc_usrMB_p4_b2
*acc_usrMB_p4_b2.parent: acc_usrMB_p4
*acc_usrMB_p4_b2.labelString: "Search"
*acc_usrMB_p4_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p4_b2.activateCallback.source: public
*acc_usrMB_p4_b2.activateCallback: accounts_users_search_accountsCb

*acc_usrMB_p4_b3.class: separatorGadget
*acc_usrMB_p4_b3.static: true
*acc_usrMB_p4_b3.name: acc_usrMB_p4_b3
*acc_usrMB_p4_b3.parent: acc_usrMB_p4

*acc_usrMB_p4_b4.class: pushButtonGadget
*acc_usrMB_p4_b4.static: true
*acc_usrMB_p4_b4.name: acc_usrMB_p4_b4
*acc_usrMB_p4_b4.parent: acc_usrMB_p4
*acc_usrMB_p4_b4.labelString: "Account Data"
*acc_usrMB_p4_b4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p4_b4.activateCallback.source: public
*acc_usrMB_p4_b4.activateCallback: accounts_users_create_accountCb
*acc_usrMB_p4_b4.activateCallbackClientData: (XtPointer) 0

*acc_usrMB_p4_b5.class: separatorGadget
*acc_usrMB_p4_b5.static: true
*acc_usrMB_p4_b5.name: acc_usrMB_p4_b5
*acc_usrMB_p4_b5.parent: acc_usrMB_p4

*acc_usrMB_p4_b10.class: pushButtonGadget
*acc_usrMB_p4_b10.static: true
*acc_usrMB_p4_b10.name: acc_usrMB_p4_b10
*acc_usrMB_p4_b10.parent: acc_usrMB_p4
*acc_usrMB_p4_b10.labelString: "Assign DataSet"
*acc_usrMB_p4_b10.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p4_b10.activateCallback.source: public
*acc_usrMB_p4_b10.activateCallback: accounts_users_assign_datasetsCb

*acc_usrMB_p4_b6.class: pushButtonGadget
*acc_usrMB_p4_b6.static: true
*acc_usrMB_p4_b6.name: acc_usrMB_p4_b6
*acc_usrMB_p4_b6.parent: acc_usrMB_p4
*acc_usrMB_p4_b6.labelString: "Assign Users"
*acc_usrMB_p4_b6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p4_b6.activateCallback.source: public
*acc_usrMB_p4_b6.activateCallback: accounts_users_assign_usersCb

*acc_usrMB_p4_b7.class: separatorGadget
*acc_usrMB_p4_b7.static: true
*acc_usrMB_p4_b7.name: acc_usrMB_p4_b7
*acc_usrMB_p4_b7.parent: acc_usrMB_p4

*acc_usrMB_p4_b8.class: pushButtonGadget
*acc_usrMB_p4_b8.static: true
*acc_usrMB_p4_b8.name: acc_usrMB_p4_b8
*acc_usrMB_p4_b8.parent: acc_usrMB_p4
*acc_usrMB_p4_b8.labelString: "Create"
*acc_usrMB_p4_b8.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p4_b8.activateCallback.source: public
*acc_usrMB_p4_b8.activateCallback: accounts_users_create_accountCb
*acc_usrMB_p4_b8.activateCallbackClientData: (XtPointer) 1

*acc_usrMB_p4_b9.class: pushButtonGadget
*acc_usrMB_p4_b9.static: true
*acc_usrMB_p4_b9.name: acc_usrMB_p4_b9
*acc_usrMB_p4_b9.parent: acc_usrMB_p4
*acc_usrMB_p4_b9.labelString: "Delete"
*acc_usrMB_p4_b9.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_p4_b9.activateCallback.source: public
*acc_usrMB_p4_b9.activateCallback: accounts_users_delete_accountCb

*acc_usrMB_top_b1.class: cascadeButton
*acc_usrMB_top_b1.static: true
*acc_usrMB_top_b1.name: acc_usrMB_top_b1
*acc_usrMB_top_b1.parent: acc_usrMB
*acc_usrMB_top_b1.labelString: "Go To"
*acc_usrMB_top_b1.subMenuId: "acc_usrMB_p1"
*acc_usrMB_top_b1.mnemonic: "G"
*acc_usrMB_top_b1.background: "CadetBlue"
*acc_usrMB_top_b1.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_top_b1.marginWidth: 12

*acc_usrMB_top_b3.class: cascadeButton
*acc_usrMB_top_b3.static: true
*acc_usrMB_top_b3.name: acc_usrMB_top_b3
*acc_usrMB_top_b3.parent: acc_usrMB
*acc_usrMB_top_b3.labelString: "Help"
*acc_usrMB_top_b3.mnemonic: "H"
*acc_usrMB_top_b3.subMenuId: "acc_usrMB_p3"
*acc_usrMB_top_b3.background: "CadetBlue"
*acc_usrMB_top_b3.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*acc_usrMB_top_b3.marginWidth: 12

*acc_usrMB_top_b8.class: cascadeButton
*acc_usrMB_top_b8.static: true
*acc_usrMB_top_b8.name: acc_usrMB_top_b8
*acc_usrMB_top_b8.parent: acc_usrMB
*acc_usrMB_top_b8.labelString: "Users"
*acc_usrMB_top_b8.mnemonic: "U"
*acc_usrMB_top_b8.subMenuId: "acc_usrMB_p10"
*acc_usrMB_top_b8.background: "CadetBlue"
*acc_usrMB_top_b8.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*acc_usrMB_top_b2.class: cascadeButton
*acc_usrMB_top_b2.static: true
*acc_usrMB_top_b2.name: acc_usrMB_top_b2
*acc_usrMB_top_b2.parent: acc_usrMB
*acc_usrMB_top_b2.labelString: "Accounts"
*acc_usrMB_top_b2.mnemonic: "A"
*acc_usrMB_top_b2.subMenuId: "acc_usrMB_p4"
*acc_usrMB_top_b2.background: "CadetBlue"
*acc_usrMB_top_b2.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: accounts_users
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 672
*label1.y: 76
*label1.width: 152
*label1.background: "#9ac0cd"
*label1.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label1.labelString: "ACCOUNTS"

*label3.class: label
*label3.static: true
*label3.name: label3
*label3.parent: accounts_users
*label3.isCompound: "true"
*label3.compoundIcon: "label.xpm"
*label3.compoundName: "label_"
*label3.x: 224
*label3.y: 42
*label3.width: 604
*label3.background: "#9ac0cd"
*label3.fontList: "-adobe-new century schoolbook-bold-r-normal--25-180-100-100-p-149-iso8859-1"
*label3.labelString: "USERS AND ACCOUNTS MANAGEMENT"

*frame1.class: frame
*frame1.static: true
*frame1.name: frame1
*frame1.parent: accounts_users
*frame1.width: 483
*frame1.height: 616
*frame1.isCompound: "true"
*frame1.compoundIcon: "frame.xpm"
*frame1.compoundName: "frame_"
*frame1.x: 516
*frame1.y: 112
*frame1.shadowThickness: 4
*frame1.background: "#9ac0cd"

*form1.class: form
*form1.static: true
*form1.name: form1
*form1.parent: frame1
*form1.width: 475
*form1.height: 628
*form1.resizePolicy: "resize_none"
*form1.isCompound: "true"
*form1.compoundIcon: "form.xpm"
*form1.compoundName: "form_"
*form1.x: 4
*form1.y: 4
*form1.background: "#9ac0cd"

*label6.class: label
*label6.static: true
*label6.name: label6
*label6.parent: form1
*label6.isCompound: "true"
*label6.compoundIcon: "label.xpm"
*label6.compoundName: "label_"
*label6.x: 30
*label6.y: 22
*label6.background: "#9ac0cd"
*label6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label6.labelString: "Account ID"

*label7.class: label
*label7.static: true
*label7.name: label7
*label7.parent: form1
*label7.isCompound: "true"
*label7.compoundIcon: "label.xpm"
*label7.compoundName: "label_"
*label7.x: 190
*label7.y: 22
*label7.background: "#9ac0cd"
*label7.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label7.labelString: "Account Type"

*label24.class: label
*label24.static: true
*label24.name: label24
*label24.parent: form1
*label24.isCompound: "true"
*label24.compoundIcon: "label.xpm"
*label24.compoundName: "label_"
*label24.x: 328
*label24.y: 22
*label24.background: "#9ac0cd"
*label24.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label24.labelString: "Resource"

*accountsSW.class: scrolledWindow
*accountsSW.static: true
*accountsSW.name: accountsSW
*accountsSW.parent: form1
*accountsSW.scrollingPolicy: "automatic"
*accountsSW.visualPolicy: "constant"
*accountsSW.scrollBarDisplayPolicy: "as_needed"
*accountsSW.shadowThickness: 1
*accountsSW.isCompound: "true"
*accountsSW.compoundIcon: "scrllist.xpm"
*accountsSW.compoundName: "scrolled_List"
*accountsSW.x: 18
*accountsSW.y: 52
*accountsSW.width: 416
*accountsSW.height: 461
*accountsSW.background: "#9ac0cd"
*accountsSW.borderWidth: 0

*accountsSL.class: scrolledList
*accountsSL.static: true
*accountsSL.name: accountsSL
*accountsSL.parent: accountsSW
*accountsSL.width: 414
*accountsSL.height: 458
*accountsSL.background: "LightSkyBlue3"
*accountsSL.listSizePolicy: "constant"
*accountsSL.itemCount: 0
*accountsSL.selectionPolicy: "single_select"
*accountsSL.defaultActionCallback.source: public
*accountsSL.defaultActionCallback: accounts_users_create_accountCb
*accountsSL.singleSelectionCallback.source: public
*accountsSL.singleSelectionCallback: accounts_users_accounts_listCb
*accountsSL.scrollBarDisplayPolicy: "as_needed"
*accountsSL.shadowThickness: 2
*accountsSL.highlightColor: "black"
*accountsSL.foreground: "black"
*accountsSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*accountsSL.defaultActionCallbackClientData: (XtPointer) 0

*accounts_sbSW.class: scrolledWindow
*accounts_sbSW.name.source: public
*accounts_sbSW.static: false
*accounts_sbSW.name: accounts_sbSW
*accounts_sbSW.parent: form1
*accounts_sbSW.scrollingPolicy: "application_defined"
*accounts_sbSW.visualPolicy: "variable"
*accounts_sbSW.scrollBarDisplayPolicy: "static"
*accounts_sbSW.shadowThickness: 0
*accounts_sbSW.isCompound: "true"
*accounts_sbSW.compoundIcon: "scrllist.xpm"
*accounts_sbSW.compoundName: "scrolled_List"
*accounts_sbSW.x: 440
*accounts_sbSW.y: 52
*accounts_sbSW.height: 461
*accounts_sbSW.width: 16
*accounts_sbSW.background: "#9ac0cd"

*accounts_sbSL.class: scrolledList
*accounts_sbSL.static: true
*accounts_sbSL.name: accounts_sbSL
*accounts_sbSL.parent: accounts_sbSW
*accounts_sbSL.width: 2
*accounts_sbSL.height: 441
*accounts_sbSL.scrollBarDisplayPolicy: "static"
*accounts_sbSL.listSizePolicy: "variable"
*accounts_sbSL.background: "LightSkyBlue3"
*accounts_sbSL.createCallback.source: public
*accounts_sbSL.createCallback: accounts_users_accounts_sbCb
*accounts_sbSL.createCallbackClientData: (XtPointer) 1
*accounts_sbSL.visibleItemCount: 25

*create_accountPB.class: pushButton
*create_accountPB.static: true
*create_accountPB.name: create_accountPB
*create_accountPB.parent: form1
*create_accountPB.isCompound: "true"
*create_accountPB.compoundIcon: "push.xpm"
*create_accountPB.compoundName: "push_Button"
*create_accountPB.x: 18
*create_accountPB.y: 564
*create_accountPB.width: 138
*create_accountPB.height: 34
*create_accountPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*create_accountPB.labelString: "Create"
*create_accountPB.shadowThickness: 4
*create_accountPB.background: "CadetBlue"
*create_accountPB.activateCallback.source: public
*create_accountPB.activateCallback: accounts_users_create_accountCb
*create_accountPB.createCallback: 
*create_accountPB.activateCallbackClientData: (XtPointer) 1

*account_dataPB.class: pushButton
*account_dataPB.static: true
*account_dataPB.name: account_dataPB
*account_dataPB.parent: form1
*account_dataPB.isCompound: "true"
*account_dataPB.compoundIcon: "push.xpm"
*account_dataPB.compoundName: "push_Button"
*account_dataPB.x: 318
*account_dataPB.y: 526
*account_dataPB.width: 138
*account_dataPB.height: 34
*account_dataPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*account_dataPB.labelString: "Account Data"
*account_dataPB.shadowThickness: 4
*account_dataPB.background: "CadetBlue"
*account_dataPB.activateCallback.source: public
*account_dataPB.activateCallback: accounts_users_create_accountCb
*account_dataPB.activateCallbackClientData: (XtPointer) 0

*delete_accountPB.class: pushButton
*delete_accountPB.static: true
*delete_accountPB.name: delete_accountPB
*delete_accountPB.parent: form1
*delete_accountPB.isCompound: "true"
*delete_accountPB.compoundIcon: "push.xpm"
*delete_accountPB.compoundName: "push_Button"
*delete_accountPB.x: 320
*delete_accountPB.y: 562
*delete_accountPB.width: 138
*delete_accountPB.height: 34
*delete_accountPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*delete_accountPB.labelString: "Delete"
*delete_accountPB.shadowThickness: 4
*delete_accountPB.background: "CadetBlue"
*delete_accountPB.activateCallback.source: public
*delete_accountPB.activateCallback: accounts_users_delete_accountCb

*searchPB.class: pushButton
*searchPB.static: true
*searchPB.name: searchPB
*searchPB.parent: form1
*searchPB.isCompound: "true"
*searchPB.compoundIcon: "push.xpm"
*searchPB.compoundName: "push_Button"
*searchPB.x: 18
*searchPB.y: 526
*searchPB.width: 138
*searchPB.height: 34
*searchPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*searchPB.labelString: "Search"
*searchPB.shadowThickness: 4
*searchPB.background: "CadetBlue"
*searchPB.activateCallback.source: public
*searchPB.activateCallback: accounts_users_search_accountsCb

*assign_usersButton.class: pushButton
*assign_usersButton.static: true
*assign_usersButton.name: assign_usersButton
*assign_usersButton.parent: form1
*assign_usersButton.isCompound: "true"
*assign_usersButton.compoundIcon: "push.xpm"
*assign_usersButton.compoundName: "push_Button"
*assign_usersButton.x: 172
*assign_usersButton.y: 562
*assign_usersButton.width: 138
*assign_usersButton.height: 34
*assign_usersButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*assign_usersButton.labelString: "Assign Users"
*assign_usersButton.shadowThickness: 4
*assign_usersButton.background: "CadetBlue"
*assign_usersButton.activateCallback.source: public
*assign_usersButton.activateCallback: accounts_users_assign_usersCb

*assign_datasetsButton.class: pushButton
*assign_datasetsButton.static: true
*assign_datasetsButton.name: assign_datasetsButton
*assign_datasetsButton.parent: form1
*assign_datasetsButton.isCompound: "true"
*assign_datasetsButton.compoundIcon: "push.xpm"
*assign_datasetsButton.compoundName: "push_Button"
*assign_datasetsButton.x: 172
*assign_datasetsButton.y: 526
*assign_datasetsButton.width: 138
*assign_datasetsButton.height: 34
*assign_datasetsButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*assign_datasetsButton.labelString: "Assign Dataset"
*assign_datasetsButton.shadowThickness: 4
*assign_datasetsButton.background: "CadetBlue"
*assign_datasetsButton.activateCallback.source: public
*assign_datasetsButton.activateCallback: accounts_users_assign_datasetsCb

*frame2.class: frame
*frame2.static: true
*frame2.name: frame2
*frame2.parent: accounts_users
*frame2.width: 483
*frame2.height: 616
*frame2.isCompound: "true"
*frame2.compoundIcon: "frame.xpm"
*frame2.compoundName: "frame_"
*frame2.x: 21
*frame2.y: 112
*frame2.shadowThickness: 4
*frame2.background: "#9ac0cd"
*frame2.borderWidth: 0

*form2.class: form
*form2.static: true
*form2.name: form2
*form2.parent: frame2
*form2.width: 477
*form2.height: 501
*form2.resizePolicy: "resize_none"
*form2.isCompound: "true"
*form2.compoundIcon: "form.xpm"
*form2.compoundName: "form_"
*form2.x: 4
*form2.y: 3
*form2.background: "#9ac0cd"

*label4.class: label
*label4.static: true
*label4.name: label4
*label4.parent: form2
*label4.isCompound: "true"
*label4.compoundIcon: "label.xpm"
*label4.compoundName: "label_"
*label4.x: 32
*label4.y: 22
*label4.background: "#9ac0cd"
*label4.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label4.labelString: "User ID"

*usersSW.class: scrolledWindow
*usersSW.static: true
*usersSW.name: usersSW
*usersSW.parent: form2
*usersSW.scrollingPolicy: "automatic"
*usersSW.visualPolicy: "constant"
*usersSW.scrollBarDisplayPolicy: "as_needed"
*usersSW.shadowThickness: 1
*usersSW.isCompound: "true"
*usersSW.compoundIcon: "scrllist.xpm"
*usersSW.compoundName: "scrolled_List"
*usersSW.x: 18
*usersSW.y: 52
*usersSW.width: 416
*usersSW.height: 461
*usersSW.background: "#9ac0cd"
*usersSW.borderWidth: 0

*usersSL.class: scrolledList
*usersSL.static: true
*usersSL.name: usersSL
*usersSL.parent: usersSW
*usersSL.width: 414
*usersSL.height: 458
*usersSL.background: "LightSkyBlue3"
*usersSL.listSizePolicy: "constant"
*usersSL.itemCount: 0
*usersSL.selectionPolicy: "single_select"
*usersSL.defaultActionCallback.source: public
*usersSL.defaultActionCallback: accounts_users_create_userCb
*usersSL.singleSelectionCallback.source: public
*usersSL.singleSelectionCallback: accounts_users_users_listCb
*usersSL.scrollBarDisplayPolicy: "as_needed"
*usersSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"
*usersSL.defaultActionCallbackClientData: (XtPointer) 0

*create_userPB.class: pushButton
*create_userPB.static: true
*create_userPB.name: create_userPB
*create_userPB.parent: form2
*create_userPB.isCompound: "true"
*create_userPB.compoundIcon: "push.xpm"
*create_userPB.compoundName: "push_Button"
*create_userPB.x: 18
*create_userPB.y: 562
*create_userPB.width: 138
*create_userPB.height: 34
*create_userPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*create_userPB.labelString: "Create"
*create_userPB.shadowThickness: 4
*create_userPB.background: "CadetBlue"
*create_userPB.activateCallback.source: public
*create_userPB.activateCallback: accounts_users_create_userCb
*create_userPB.activateCallbackClientData: (XtPointer) 1

*user_dataPB.class: pushButton
*user_dataPB.static: true
*user_dataPB.name: user_dataPB
*user_dataPB.parent: form2
*user_dataPB.isCompound: "true"
*user_dataPB.compoundIcon: "push.xpm"
*user_dataPB.compoundName: "push_Button"
*user_dataPB.x: 318
*user_dataPB.y: 526
*user_dataPB.width: 138
*user_dataPB.height: 34
*user_dataPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*user_dataPB.labelString: "User Data"
*user_dataPB.shadowThickness: 4
*user_dataPB.background: "CadetBlue"
*user_dataPB.activateCallback.source: public
*user_dataPB.activateCallback: accounts_users_create_userCb
*user_dataPB.activateCallbackClientData: (XtPointer) 0

*delete_userPB.class: pushButton
*delete_userPB.static: true
*delete_userPB.name: delete_userPB
*delete_userPB.parent: form2
*delete_userPB.isCompound: "true"
*delete_userPB.compoundIcon: "push.xpm"
*delete_userPB.compoundName: "push_Button"
*delete_userPB.x: 318
*delete_userPB.y: 564
*delete_userPB.width: 138
*delete_userPB.height: 34
*delete_userPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*delete_userPB.labelString: "Delete"
*delete_userPB.shadowThickness: 4
*delete_userPB.background: "CadetBlue"
*delete_userPB.activateCallback.source: public
*delete_userPB.activateCallback: accounts_users_delete_userCb

*search_usersButton.class: pushButton
*search_usersButton.static: true
*search_usersButton.name: search_usersButton
*search_usersButton.parent: form2
*search_usersButton.isCompound: "true"
*search_usersButton.compoundIcon: "push.xpm"
*search_usersButton.compoundName: "push_Button"
*search_usersButton.x: 18
*search_usersButton.y: 526
*search_usersButton.width: 138
*search_usersButton.height: 34
*search_usersButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*search_usersButton.labelString: "Search"
*search_usersButton.shadowThickness: 4
*search_usersButton.background: "CadetBlue"
*search_usersButton.activateCallback.source: public
*search_usersButton.activateCallback: accounts_users_search_usersCb

*label8.class: label
*label8.static: true
*label8.name: label8
*label8.parent: form2
*label8.isCompound: "true"
*label8.compoundIcon: "label.xpm"
*label8.compoundName: "label_"
*label8.x: 174
*label8.y: 22
*label8.background: "#9ac0cd"
*label8.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label8.labelString: "User Type"

*users_sbSW.class: scrolledWindow
*users_sbSW.name.source: public
*users_sbSW.static: false
*users_sbSW.name: users_sbSW
*users_sbSW.parent: form2
*users_sbSW.scrollingPolicy: "application_defined"
*users_sbSW.visualPolicy: "variable"
*users_sbSW.scrollBarDisplayPolicy: "static"
*users_sbSW.shadowThickness: 0
*users_sbSW.isCompound: "true"
*users_sbSW.compoundIcon: "scrllist.xpm"
*users_sbSW.compoundName: "scrolled_List"
*users_sbSW.x: 440
*users_sbSW.y: 52
*users_sbSW.height: 461
*users_sbSW.width: 16
*users_sbSW.background: "#9ac0cd"

*users_sbSL.class: scrolledList
*users_sbSL.static: true
*users_sbSL.name: users_sbSL
*users_sbSL.parent: users_sbSW
*users_sbSL.width: 2
*users_sbSL.height: 441
*users_sbSL.scrollBarDisplayPolicy: "static"
*users_sbSL.listSizePolicy: "variable"
*users_sbSL.background: "LightSkyBlue3"
*users_sbSL.createCallback.source: public
*users_sbSL.createCallback: accounts_users_users_sbCb
*users_sbSL.createCallbackClientData: (XtPointer) 1
*users_sbSL.visibleItemCount: 25

*label86.class: label
*label86.static: true
*label86.name: label86
*label86.parent: form2
*label86.isCompound: "true"
*label86.compoundIcon: "label.xpm"
*label86.compoundName: "label_"
*label86.x: 292
*label86.y: 22
*label86.background: "#9ac0cd"
*label86.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label86.labelString: "User Name"

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: accounts_users
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: 192
*label2.y: 76
*label2.width: 152
*label2.background: "#9ac0cd"
*label2.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label2.labelString: "USERS"

*pushButton5.class: pushButton
*pushButton5.static: true
*pushButton5.name: pushButton5
*pushButton5.parent: accounts_users
*pushButton5.isCompound: "true"
*pushButton5.compoundIcon: "push.xpm"
*pushButton5.compoundName: "push_Button"
*pushButton5.x: 22
*pushButton5.y: 732
*pushButton5.width: 138
*pushButton5.height: 34
*pushButton5.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*pushButton5.labelString: "Help"
*pushButton5.shadowThickness: 4
*pushButton5.background: "CadetBlue"
*pushButton5.activateCallback: {\
\
}

*closeButton.class: pushButton
*closeButton.static: true
*closeButton.name: closeButton
*closeButton.parent: accounts_users
*closeButton.isCompound: "true"
*closeButton.compoundIcon: "push.xpm"
*closeButton.compoundName: "push_Button"
*closeButton.x: 858
*closeButton.y: 732
*closeButton.width: 138
*closeButton.height: 34
*closeButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closeButton.labelString: "Close"
*closeButton.shadowThickness: 4
*closeButton.background: "CadetBlue"
*closeButton.activateCallback.source: public
*closeButton.activateCallback: accounts_users_closeCb

