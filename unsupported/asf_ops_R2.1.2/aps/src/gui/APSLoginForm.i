! UIMX ascii 2.9 key: 3559                                                      

*APSLoginForm.class: form
*APSLoginForm.classinc:
*APSLoginForm.classspec:
*APSLoginForm.classmembers:
*APSLoginForm.classconstructor:
*APSLoginForm.classdestructor:
*APSLoginForm.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
U.S. Government Sponsorship acknowledged.\
#endif\
 \
/*==============================================================================\
Filename:\
 \
Description:\
 \
External Functions Defined:\
 \
File Scope Functions:\
 \
External Variables Defined:\
 \
File Scope Variables:\
 \
Notes:\
 \
==============================================================================*/\
#pragma ident   "@(#)APSLoginForm.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.APSLoginForm.i"\
\
#include "cb_apslogin.h"\
#include "gui_utils.h"\

*APSLoginForm.ispecdecl:
*APSLoginForm.funcdecl: swidget create_APSLoginForm(swidget UxParent)
*APSLoginForm.funcname: create_APSLoginForm
*APSLoginForm.funcdef: "swidget", "<create_APSLoginForm>(%)"
*APSLoginForm.argdecl: swidget UxParent;
*APSLoginForm.arglist: UxParent
*APSLoginForm.arglist.UxParent: "swidget", "%UxParent%"
*APSLoginForm.icode:
*APSLoginForm.fcode: XtVaSetValues( gui_GetShellWidget( rtrn ),\
	XmNdeleteResponse, XmDESTROY,\
	NULL ) ;\
\
cb_map_login();\
\
return(rtrn);\

*APSLoginForm.auxdecl:
*APSLoginForm.name.source: public
*APSLoginForm.static: false
*APSLoginForm.name: APSLoginForm
*APSLoginForm.parent: NO_PARENT
*APSLoginForm.defaultShell: topLevelShell
*APSLoginForm.width: 530
*APSLoginForm.height: 362
*APSLoginForm.resizePolicy: "resize_none"
*APSLoginForm.isCompound: "true"
*APSLoginForm.compoundIcon: "form.xpm"
*APSLoginForm.compoundName: "form_"
*APSLoginForm.x: 0
*APSLoginForm.y: 20
*APSLoginForm.unitType: "pixels"
*APSLoginForm.destroyCallback.source: public
*APSLoginForm.destroyCallback: cb_exit_login
*APSLoginForm.initialFocus: "PasswordTextField"

*APSLoginLabel1.class: label
*APSLoginLabel1.static: true
*APSLoginLabel1.name: APSLoginLabel1
*APSLoginLabel1.parent: APSLoginForm
*APSLoginLabel1.isCompound: "true"
*APSLoginLabel1.compoundIcon: "label.xpm"
*APSLoginLabel1.compoundName: "label_"
*APSLoginLabel1.x: 181
*APSLoginLabel1.y: 25
*APSLoginLabel1.width: 74
*APSLoginLabel1.height: 50
*APSLoginLabel1.fontList: "-adobe-new century schoolbook-bold-i-normal--34-240-100-100-p-193-iso8859-1"
*APSLoginLabel1.labelString: "APS"

*APSLoginLabel2.class: label
*APSLoginLabel2.static: true
*APSLoginLabel2.name: APSLoginLabel2
*APSLoginLabel2.parent: APSLoginForm
*APSLoginLabel2.isCompound: "true"
*APSLoginLabel2.compoundIcon: "label.xpm"
*APSLoginLabel2.compoundName: "label_"
*APSLoginLabel2.x: 265
*APSLoginLabel2.y: 25
*APSLoginLabel2.width: 91
*APSLoginLabel2.height: 50
*APSLoginLabel2.fontList: "-b&h-lucidabright-demibold-i-normal--34-240-100-100-p-203-iso8859-1"
*APSLoginLabel2.labelString: "login"

*LoginLabel.class: label
*LoginLabel.static: true
*LoginLabel.name: LoginLabel
*LoginLabel.parent: APSLoginForm
*LoginLabel.isCompound: "true"
*LoginLabel.compoundIcon: "label.xpm"
*LoginLabel.compoundName: "label_"
*LoginLabel.x: 72
*LoginLabel.y: 97
*LoginLabel.width: 60
*LoginLabel.height: 25
*LoginLabel.fontList: "-adobe-new century schoolbook-medium-r-normal--17-120-100-100-p-91-iso8859-1"
*LoginLabel.labelString: "Login:"

*PasswordLabel.class: label
*PasswordLabel.static: true
*PasswordLabel.name: PasswordLabel
*PasswordLabel.parent: APSLoginForm
*PasswordLabel.isCompound: "true"
*PasswordLabel.compoundIcon: "label.xpm"
*PasswordLabel.compoundName: "label_"
*PasswordLabel.x: 42
*PasswordLabel.y: 147
*PasswordLabel.width: 90
*PasswordLabel.height: 25
*PasswordLabel.fontList: "-adobe-new century schoolbook-medium-r-normal--17-120-100-100-p-91-iso8859-1"
*PasswordLabel.labelString: "Password:"

*DatabaseLabel.class: label
*DatabaseLabel.static: true
*DatabaseLabel.name: DatabaseLabel
*DatabaseLabel.parent: APSLoginForm
*DatabaseLabel.isCompound: "true"
*DatabaseLabel.compoundIcon: "label.xpm"
*DatabaseLabel.compoundName: "label_"
*DatabaseLabel.x: 47
*DatabaseLabel.y: 197
*DatabaseLabel.width: 85
*DatabaseLabel.height: 25
*DatabaseLabel.fontList: "-adobe-new century schoolbook-medium-r-normal--17-120-100-100-p-91-iso8859-1"
*DatabaseLabel.labelString: "Database:"

*LoginTextField.class: textField
*LoginTextField.name.source: public
*LoginTextField.static: false
*LoginTextField.name: LoginTextField
*LoginTextField.parent: APSLoginForm
*LoginTextField.width: 310
*LoginTextField.isCompound: "true"
*LoginTextField.compoundIcon: "textfield.xpm"
*LoginTextField.compoundName: "text_Field"
*LoginTextField.x: 132
*LoginTextField.y: 92
*LoginTextField.height: 35
*LoginTextField.fontList: "-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1"
*LoginTextField.createCallback.source: public
*LoginTextField.createCallback: cb_init_userid
*LoginTextField.activateCallback.source: public
*LoginTextField.activateCallback: cb_verify_login

*PasswordTextField.class: textField
*PasswordTextField.name.source: public
*PasswordTextField.static: false
*PasswordTextField.name: PasswordTextField
*PasswordTextField.parent: APSLoginForm
*PasswordTextField.width: 310
*PasswordTextField.isCompound: "true"
*PasswordTextField.compoundIcon: "textfield.xpm"
*PasswordTextField.compoundName: "text_Field"
*PasswordTextField.x: 132
*PasswordTextField.y: 142
*PasswordTextField.height: 35
*PasswordTextField.fontList: "-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1"
*PasswordTextField.modifyVerifyCallback.source: public
*PasswordTextField.modifyVerifyCallback: cb_save_password
*PasswordTextField.createCallback.source: public
*PasswordTextField.createCallback: cb_init_password
*PasswordTextField.activateCallback.source: public
*PasswordTextField.activateCallback: cb_verify_login

*DatabaseTextField.class: textField
*DatabaseTextField.name.source: public
*DatabaseTextField.static: false
*DatabaseTextField.name: DatabaseTextField
*DatabaseTextField.parent: APSLoginForm
*DatabaseTextField.width: 310
*DatabaseTextField.isCompound: "true"
*DatabaseTextField.compoundIcon: "textfield.xpm"
*DatabaseTextField.compoundName: "text_Field"
*DatabaseTextField.x: 132
*DatabaseTextField.y: 192
*DatabaseTextField.height: 35
*DatabaseTextField.fontList: "-adobe-new century schoolbook-medium-r-normal--14-100-100-100-p-82-iso8859-1"
*DatabaseTextField.createCallback.source: public
*DatabaseTextField.createCallback: cb_init_database
*DatabaseTextField.activateCallback.source: public
*DatabaseTextField.activateCallback: cb_verify_login

*Ok_PushButton.class: pushButton
*Ok_PushButton.static: true
*Ok_PushButton.name: Ok_PushButton
*Ok_PushButton.parent: APSLoginForm
*Ok_PushButton.isCompound: "true"
*Ok_PushButton.compoundIcon: "push.xpm"
*Ok_PushButton.compoundName: "push_Button"
*Ok_PushButton.x: 112
*Ok_PushButton.y: 253
*Ok_PushButton.width: 65
*Ok_PushButton.height: 35
*Ok_PushButton.labelString: "OK"
*Ok_PushButton.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-14-140-72-72-m-90-iso8859-1"
*Ok_PushButton.activateCallback.source: public
*Ok_PushButton.activateCallback: cb_verify_login

*Reset_PushButton.class: pushButton
*Reset_PushButton.static: true
*Reset_PushButton.name: Reset_PushButton
*Reset_PushButton.parent: APSLoginForm
*Reset_PushButton.isCompound: "true"
*Reset_PushButton.compoundIcon: "push.xpm"
*Reset_PushButton.compoundName: "push_Button"
*Reset_PushButton.x: 245
*Reset_PushButton.y: 253
*Reset_PushButton.width: 65
*Reset_PushButton.height: 35
*Reset_PushButton.labelString: "RESET"
*Reset_PushButton.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-14-140-72-72-m-90-iso8859-1"
*Reset_PushButton.activateCallback.source: public
*Reset_PushButton.activateCallback: cb_reset_login

*Exit_PushButton.class: pushButton
*Exit_PushButton.static: true
*Exit_PushButton.name: Exit_PushButton
*Exit_PushButton.parent: APSLoginForm
*Exit_PushButton.isCompound: "true"
*Exit_PushButton.compoundIcon: "push.xpm"
*Exit_PushButton.compoundName: "push_Button"
*Exit_PushButton.x: 377
*Exit_PushButton.y: 253
*Exit_PushButton.width: 65
*Exit_PushButton.height: 35
*Exit_PushButton.labelString: "EXIT"
*Exit_PushButton.fontList: "-b&h-lucida sans typewriter-bold-r-normal-sans-14-140-72-72-m-90-iso8859-1"
*Exit_PushButton.activateCallback.source: public
*Exit_PushButton.activateCallback: cb_exit_login

*APSLoginAsterisk.class: label
*APSLoginAsterisk.static: true
*APSLoginAsterisk.name: APSLoginAsterisk
*APSLoginAsterisk.parent: APSLoginForm
*APSLoginAsterisk.isCompound: "true"
*APSLoginAsterisk.compoundIcon: "label.xpm"
*APSLoginAsterisk.compoundName: "label_"
*APSLoginAsterisk.x: 255
*APSLoginAsterisk.y: 32
*APSLoginAsterisk.width: 10
*APSLoginAsterisk.height: 15
*APSLoginAsterisk.fontList: "-adobe-new century schoolbook-bold-i-normal--20-140-100-100-p-111-iso8859-1"
*APSLoginAsterisk.labelString: "*"

*labelCopyright_line1.class: label
*labelCopyright_line1.static: true
*labelCopyright_line1.name: labelCopyright_line1
*labelCopyright_line1.parent: APSLoginForm
*labelCopyright_line1.isCompound: "true"
*labelCopyright_line1.compoundIcon: "label.xpm"
*labelCopyright_line1.compoundName: "label_"
*labelCopyright_line1.x: 56
*labelCopyright_line1.y: 323
*labelCopyright_line1.width: 386
*labelCopyright_line1.height: 28
*labelCopyright_line1.labelString: "*Copyright (c)1996, California Institute of Technology.\nALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged."

