! UIMX ascii 2.9 key: 1802                                                      

*assign_users.class: form
*assign_users.classinc:
*assign_users.classspec:
*assign_users.classmembers:
*assign_users.classconstructor:
*assign_users.classdestructor:
*assign_users.gbldecl: #include <stdio.h>\
#include <ims_op_accCb.h>
*assign_users.ispecdecl:
*assign_users.funcdecl: swidget create_assign_users(swidget UxParent)
*assign_users.funcname: create_assign_users
*assign_users.funcdef: "swidget", "<create_assign_users>(%)"
*assign_users.argdecl: swidget UxParent;
*assign_users.arglist: UxParent
*assign_users.arglist.UxParent: "swidget", "%UxParent%"
*assign_users.icode:
*assign_users.fcode: return(rtrn);\

*assign_users.auxdecl:
*assign_users.name.source: public
*assign_users.static: false
*assign_users.name: assign_users
*assign_users.parent: NO_PARENT
*assign_users.parentExpression: UxParent
*assign_users.defaultShell: transientShell
*assign_users.width: 1036
*assign_users.height: 770
*assign_users.resizePolicy: "resize_none"
*assign_users.isCompound: "true"
*assign_users.compoundIcon: "form.xpm"
*assign_users.compoundName: "form_"
*assign_users.x: 9
*assign_users.y: 82
*assign_users.unitType: "pixels"
*assign_users.background: "#9ac0cd"
*assign_users.dialogTitle: "Assign Users"
*assign_users.sensitive: "true"
*assign_users.allowShellResize: "true"
*assign_users.createManaged: "true"
*assign_users.autoUnmanage: "true"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: assign_users
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 382
*label1.y: 18
*label1.width: 256
*label1.background: "#9ac0cd"
*label1.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label1.labelString: "Assign Users to Account"

*frame1.class: frame
*frame1.static: true
*frame1.name: frame1
*frame1.parent: assign_users
*frame1.width: 481
*frame1.height: 512
*frame1.isCompound: "true"
*frame1.compoundIcon: "frame.xpm"
*frame1.compoundName: "frame_"
*frame1.x: 531
*frame1.y: 204
*frame1.shadowThickness: 3
*frame1.background: "#9ac0cd"

*form1.class: form
*form1.static: true
*form1.name: form1
*form1.parent: frame1
*form1.width: 476
*form1.height: 509
*form1.resizePolicy: "resize_none"
*form1.isCompound: "true"
*form1.compoundIcon: "form.xpm"
*form1.compoundName: "form_"
*form1.x: 3
*form1.y: 4
*form1.background: "#9ac0cd"

*label6.class: label
*label6.static: true
*label6.name: label6
*label6.parent: form1
*label6.isCompound: "true"
*label6.compoundIcon: "label.xpm"
*label6.compoundName: "label_"
*label6.x: 72
*label6.y: 14
*label6.background: "#9ac0cd"
*label6.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label6.labelString: "User ID"

*label7.class: label
*label7.static: true
*label7.name: label7
*label7.parent: form1
*label7.isCompound: "true"
*label7.compoundIcon: "label.xpm"
*label7.compoundName: "label_"
*label7.x: 234
*label7.y: 14
*label7.background: "#9ac0cd"
*label7.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label7.labelString: "User Name"

*deletePB.class: pushButton
*deletePB.static: true
*deletePB.name: deletePB
*deletePB.parent: form1
*deletePB.isCompound: "true"
*deletePB.compoundIcon: "push.xpm"
*deletePB.compoundName: "push_Button"
*deletePB.x: 178
*deletePB.y: 464
*deletePB.width: 138
*deletePB.height: 34
*deletePB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*deletePB.labelString: "Delete"
*deletePB.shadowThickness: 4
*deletePB.background: "CadetBlue"
*deletePB.activateCallback.source: public
*deletePB.activateCallback: assign_users_delete_usersCb

*assigned_usersSW.class: scrolledWindow
*assigned_usersSW.static: true
*assigned_usersSW.name: assigned_usersSW
*assigned_usersSW.parent: form1
*assigned_usersSW.scrollingPolicy: "automatic"
*assigned_usersSW.visualPolicy: "constant"
*assigned_usersSW.scrollBarDisplayPolicy: "as_needed"
*assigned_usersSW.shadowThickness: 1
*assigned_usersSW.isCompound: "true"
*assigned_usersSW.compoundIcon: "scrllist.xpm"
*assigned_usersSW.compoundName: "scrolled_List"
*assigned_usersSW.x: 18
*assigned_usersSW.y: 40
*assigned_usersSW.width: 422
*assigned_usersSW.height: 416
*assigned_usersSW.background: "#9ac0cd"
*assigned_usersSW.borderWidth: 0

*assigned_usersSL.class: scrolledList
*assigned_usersSL.static: true
*assigned_usersSL.name: assigned_usersSL
*assigned_usersSL.parent: assigned_usersSW
*assigned_usersSL.width: 422
*assigned_usersSL.height: 386
*assigned_usersSL.background: "LightSkyBlue3"
*assigned_usersSL.listSizePolicy: "constant"
*assigned_usersSL.itemCount: 0
*assigned_usersSL.selectionPolicy: "extended_select"
*assigned_usersSL.scrollBarDisplayPolicy: "as_needed"
*assigned_usersSL.defaultActionCallback.source: public
*assigned_usersSL.defaultActionCallback: assign_users_assigned_listCb
*assigned_usersSL.extendedSelectionCallback.source: public
*assigned_usersSL.extendedSelectionCallback: assign_users_assigned_listCb
*assigned_usersSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"

*assigned_users_sbSW.class: scrolledWindow
*assigned_users_sbSW.static: true
*assigned_users_sbSW.name: assigned_users_sbSW
*assigned_users_sbSW.parent: form1
*assigned_users_sbSW.scrollingPolicy: "application_defined"
*assigned_users_sbSW.visualPolicy: "variable"
*assigned_users_sbSW.scrollBarDisplayPolicy: "static"
*assigned_users_sbSW.shadowThickness: 1
*assigned_users_sbSW.isCompound: "true"
*assigned_users_sbSW.compoundIcon: "scrllist.xpm"
*assigned_users_sbSW.compoundName: "scrolled_List"
*assigned_users_sbSW.x: 448
*assigned_users_sbSW.y: 40
*assigned_users_sbSW.width: 15
*assigned_users_sbSW.height: 392
*assigned_users_sbSW.background: "#9ac0cd"
*assigned_users_sbSW.borderWidth: 0

*assigned_users_sbSL.class: scrolledList
*assigned_users_sbSL.static: true
*assigned_users_sbSL.name: assigned_users_sbSL
*assigned_users_sbSL.parent: assigned_users_sbSW
*assigned_users_sbSL.width: 2
*assigned_users_sbSL.height: 378
*assigned_users_sbSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*assigned_users_sbSL.background: "LightSkyBlue3"
*assigned_users_sbSL.listSizePolicy: "variable"
*assigned_users_sbSL.itemCount: 0
*assigned_users_sbSL.selectionPolicy: "extended_select"
*assigned_users_sbSL.scrollBarDisplayPolicy: "static"
*assigned_users_sbSL.createCallback.source: public
*assigned_users_sbSL.createCallback: assign_users_assigned_sbCb
*assigned_users_sbSL.createCallbackClientData: (XtPointer) 1

*closeButton.class: pushButton
*closeButton.static: true
*closeButton.name: closeButton
*closeButton.parent: assign_users
*closeButton.isCompound: "true"
*closeButton.compoundIcon: "push.xpm"
*closeButton.compoundName: "push_Button"
*closeButton.x: 870
*closeButton.y: 728
*closeButton.width: 138
*closeButton.height: 34
*closeButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*closeButton.labelString: "Cancel"
*closeButton.shadowThickness: 4
*closeButton.background: "CadetBlue"
*closeButton.activateCallback.source: public
*closeButton.activateCallback: assign_users_closeCb
*closeButton.leftOffset: 870
*closeButton.topOffset: 728

*frame4.class: frame
*frame4.static: true
*frame4.name: frame4
*frame4.parent: assign_users
*frame4.width: 988
*frame4.height: 104
*frame4.isCompound: "true"
*frame4.compoundIcon: "frame.xpm"
*frame4.compoundName: "frame_"
*frame4.x: 24
*frame4.y: 58
*frame4.background: "#9ac0cd"
*frame4.shadowThickness: 3

*form6.class: form
*form6.static: true
*form6.name: form6
*form6.parent: frame4
*form6.width: 200
*form6.height: 200
*form6.resizePolicy: "resize_none"
*form6.isCompound: "true"
*form6.compoundIcon: "form.xpm"
*form6.compoundName: "form_"
*form6.x: 0
*form6.y: 0
*form6.background: "#9ac0cd"

*label44.class: label
*label44.static: true
*label44.name: label44
*label44.parent: form6
*label44.isCompound: "true"
*label44.compoundIcon: "label.xpm"
*label44.compoundName: "label_"
*label44.x: 78
*label44.y: 18
*label44.background: "#9ac0cd"
*label44.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label44.labelString: "Account ID:"

*account_idTF.class: textField
*account_idTF.static: true
*account_idTF.name: account_idTF
*account_idTF.parent: form6
*account_idTF.width: 220
*account_idTF.isCompound: "true"
*account_idTF.compoundIcon: "textfield.xpm"
*account_idTF.compoundName: "text_Field"
*account_idTF.x: 192
*account_idTF.y: 12
*account_idTF.background: "LightSkyBlue3"
*account_idTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*account_idTF.maxLength: 20
*account_idTF.height: 35
*account_idTF.editable: "false"
*account_idTF.cursorPositionVisible: "false"

*label56.class: label
*label56.static: true
*label56.name: label56
*label56.parent: form6
*label56.isCompound: "true"
*label56.compoundIcon: "label.xpm"
*label56.compoundName: "label_"
*label56.x: 532
*label56.y: 14
*label56.background: "#9ac0cd"
*label56.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label56.labelString: "Current Balance:"

*current_balanceTF.class: textField
*current_balanceTF.static: true
*current_balanceTF.name: current_balanceTF
*current_balanceTF.parent: form6
*current_balanceTF.width: 220
*current_balanceTF.isCompound: "true"
*current_balanceTF.compoundIcon: "textfield.xpm"
*current_balanceTF.compoundName: "text_Field"
*current_balanceTF.x: 672
*current_balanceTF.y: 10
*current_balanceTF.background: "LightSkyBlue3"
*current_balanceTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*current_balanceTF.maxLength: 50
*current_balanceTF.columns: 50
*current_balanceTF.height: 35
*current_balanceTF.cursorPositionVisible: "false"
*current_balanceTF.editable: "false"

*label59.class: label
*label59.static: true
*label59.name: label59
*label59.parent: form6
*label59.isCompound: "true"
*label59.compoundIcon: "label.xpm"
*label59.compoundName: "label_"
*label59.x: 78
*label59.y: 56
*label59.background: "#9ac0cd"
*label59.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label59.labelString: "Creation:"

*creationTF.class: textField
*creationTF.static: true
*creationTF.name: creationTF
*creationTF.parent: form6
*creationTF.width: 220
*creationTF.isCompound: "true"
*creationTF.compoundIcon: "textfield.xpm"
*creationTF.compoundName: "text_Field"
*creationTF.x: 192
*creationTF.y: 52
*creationTF.background: "LightSkyBlue3"
*creationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*creationTF.maxLength: 50
*creationTF.columns: 50
*creationTF.height: 35
*creationTF.cursorPositionVisible: "false"
*creationTF.editable: "false"

*expirationTF.class: textField
*expirationTF.static: true
*expirationTF.name: expirationTF
*expirationTF.parent: form6
*expirationTF.width: 220
*expirationTF.isCompound: "true"
*expirationTF.compoundIcon: "textfield.xpm"
*expirationTF.compoundName: "text_Field"
*expirationTF.x: 672
*expirationTF.y: 52
*expirationTF.background: "LightSkyBlue3"
*expirationTF.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*expirationTF.maxLength: 50
*expirationTF.columns: 50
*expirationTF.height: 35
*expirationTF.cursorPositionVisible: "false"
*expirationTF.editable: "false"

*label60.class: label
*label60.static: true
*label60.name: label60
*label60.parent: form6
*label60.isCompound: "true"
*label60.compoundIcon: "label.xpm"
*label60.compoundName: "label_"
*label60.x: 532
*label60.y: 58
*label60.background: "#9ac0cd"
*label60.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label60.labelString: "Expiration:"

*frame5.class: frame
*frame5.static: true
*frame5.name: frame5
*frame5.parent: assign_users
*frame5.width: 483
*frame5.height: 512
*frame5.isCompound: "true"
*frame5.compoundIcon: "frame.xpm"
*frame5.compoundName: "frame_"
*frame5.x: 26
*frame5.y: 204
*frame5.shadowThickness: 3
*frame5.background: "#9ac0cd"

*form4.class: form
*form4.static: true
*form4.name: form4
*form4.parent: frame5
*form4.width: 475
*form4.height: 535
*form4.resizePolicy: "resize_none"
*form4.isCompound: "true"
*form4.compoundIcon: "form.xpm"
*form4.compoundName: "form_"
*form4.x: 4
*form4.y: 4
*form4.background: "#9ac0cd"

*label57.class: label
*label57.static: true
*label57.name: label57
*label57.parent: form4
*label57.isCompound: "true"
*label57.compoundIcon: "label.xpm"
*label57.compoundName: "label_"
*label57.x: 72
*label57.y: 14
*label57.background: "#9ac0cd"
*label57.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label57.labelString: "User ID"

*label58.class: label
*label58.static: true
*label58.name: label58
*label58.parent: form4
*label58.isCompound: "true"
*label58.compoundIcon: "label.xpm"
*label58.compoundName: "label_"
*label58.x: 234
*label58.y: 14
*label58.background: "#9ac0cd"
*label58.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*label58.labelString: "User Name"

*assign_usersSW.class: scrolledWindow
*assign_usersSW.static: true
*assign_usersSW.name: assign_usersSW
*assign_usersSW.parent: form4
*assign_usersSW.scrollingPolicy: "automatic"
*assign_usersSW.visualPolicy: "constant"
*assign_usersSW.scrollBarDisplayPolicy: "as_needed"
*assign_usersSW.shadowThickness: 1
*assign_usersSW.isCompound: "true"
*assign_usersSW.compoundIcon: "scrllist.xpm"
*assign_usersSW.compoundName: "scrolled_List"
*assign_usersSW.x: 17
*assign_usersSW.y: 39
*assign_usersSW.width: 422
*assign_usersSW.height: 416
*assign_usersSW.background: "#9ac0cd"
*assign_usersSW.borderWidth: 0

*assign_usersSL.class: scrolledList
*assign_usersSL.static: true
*assign_usersSL.name: assign_usersSL
*assign_usersSL.parent: assign_usersSW
*assign_usersSL.width: 422
*assign_usersSL.height: 386
*assign_usersSL.background: "LightSkyBlue3"
*assign_usersSL.listSizePolicy: "constant"
*assign_usersSL.itemCount: 0
*assign_usersSL.selectionPolicy: "extended_select"
*assign_usersSL.scrollBarDisplayPolicy: "as_needed"
*assign_usersSL.defaultActionCallback.source: public
*assign_usersSL.defaultActionCallback: assign_users_users_listCb
*assign_usersSL.extendedSelectionCallback.source: public
*assign_usersSL.extendedSelectionCallback: assign_users_users_listCb
*assign_usersSL.fontList: "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1"

*addPB.class: pushButton
*addPB.static: true
*addPB.name: addPB
*addPB.parent: form4
*addPB.isCompound: "true"
*addPB.compoundIcon: "push.xpm"
*addPB.compoundName: "push_Button"
*addPB.x: 168
*addPB.y: 464
*addPB.width: 138
*addPB.height: 34
*addPB.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*addPB.labelString: "Add"
*addPB.shadowThickness: 4
*addPB.background: "CadetBlue"
*addPB.activateCallback.source: public
*addPB.activateCallback: assign_users_add_usersCb

*assign_users_sbSW.class: scrolledWindow
*assign_users_sbSW.static: true
*assign_users_sbSW.name: assign_users_sbSW
*assign_users_sbSW.parent: form4
*assign_users_sbSW.scrollingPolicy: "application_defined"
*assign_users_sbSW.visualPolicy: "variable"
*assign_users_sbSW.scrollBarDisplayPolicy: "static"
*assign_users_sbSW.isCompound: "true"
*assign_users_sbSW.compoundIcon: "scrllist.xpm"
*assign_users_sbSW.compoundName: "scrolled_List"
*assign_users_sbSW.x: 448
*assign_users_sbSW.y: 40
*assign_users_sbSW.width: 15
*assign_users_sbSW.height: 392
*assign_users_sbSW.background: "#9ac0cd"
*assign_users_sbSW.borderWidth: 0

*assign_users_sbSL.class: scrolledList
*assign_users_sbSL.static: true
*assign_users_sbSL.name: assign_users_sbSL
*assign_users_sbSL.parent: assign_users_sbSW
*assign_users_sbSL.width: 2
*assign_users_sbSL.height: 397
*assign_users_sbSL.fontList: "-adobe-times-bold-r-normal--14-100-100-100-p-76-iso8859-1"
*assign_users_sbSL.background: "LightSkyBlue3"
*assign_users_sbSL.listSizePolicy: "variable"
*assign_users_sbSL.scrollBarDisplayPolicy: "static"
*assign_users_sbSL.createCallback.source: public
*assign_users_sbSL.createCallback: assign_users_users_sbCb
*assign_users_sbSL.createCallbackClientData: (XtPointer) 1
*assign_users_sbSL.selectionPolicy: "extended_select"

*label61.class: label
*label61.static: true
*label61.name: label61
*label61.parent: assign_users
*label61.isCompound: "true"
*label61.compoundIcon: "label.xpm"
*label61.compoundName: "label_"
*label61.x: 232
*label61.y: 174
*label61.background: "#9ac0cd"
*label61.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label61.labelString: "Users"

*label62.class: label
*label62.static: true
*label62.name: label62
*label62.parent: assign_users
*label62.isCompound: "true"
*label62.compoundIcon: "label.xpm"
*label62.compoundName: "label_"
*label62.x: 652
*label62.y: 174
*label62.background: "#9ac0cd"
*label62.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label62.labelString: "Users Assigned to Account"

*updateButton.class: pushButton
*updateButton.static: true
*updateButton.name: updateButton
*updateButton.parent: assign_users
*updateButton.isCompound: "true"
*updateButton.compoundIcon: "push.xpm"
*updateButton.compoundName: "push_Button"
*updateButton.x: 26
*updateButton.y: 728
*updateButton.width: 138
*updateButton.height: 34
*updateButton.fontList: "-adobe-new century schoolbook-bold-r-normal--14-140-75-75-p-87-iso8859-1"
*updateButton.labelString: "Update"
*updateButton.shadowThickness: 4
*updateButton.background: "CadetBlue"
*updateButton.activateCallback.source: public
*updateButton.activateCallback: assign_users_updateCb
*updateButton.topOffset: 728

