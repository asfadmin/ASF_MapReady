! UIMX ascii 2.9 key: 4955                                                      

*login.class: form
*login.classinc:
*login.classspec:
*login.classmembers:
*login.classconstructor:
*login.classdestructor:
*login.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"\

*login.ispecdecl:
*login.funcdecl: swidget create_login(swidget UxParent)
*login.funcname: create_login
*login.funcdef: "swidget", "<create_login>(%)"
*login.argdecl: swidget UxParent;
*login.arglist: UxParent
*login.arglist.UxParent: "swidget", "%UxParent%"
*login.icode:
*login.fcode: return(rtrn);\

*login.auxdecl:
*login.static: true
*login.name: login
*login.parent: NO_PARENT
*login.parentExpression: UxParent
*login.defaultShell: transientShell
*login.width: 440
*login.height: 450
*login.resizePolicy: "resize_none"
*login.isCompound: "true"
*login.compoundIcon: "form.xpm"
*login.compoundName: "form_"
*login.x: 299
*login.y: 217
*login.unitType: "pixels"
*login.allowShellResize: "false"
*login.background: "#9ac0cd"
*login.noResize: "true"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: login
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 65
*label1.y: 285
*label1.background: "#9ac0cd"
*label1.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label1.labelString: "User  ID :"

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: login
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: 57
*label2.y: 337
*label2.background: "#9ac0cd"
*label2.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"
*label2.labelString: "Password :"

*userIdText.class: text
*userIdText.static: true
*userIdText.name: userIdText
*userIdText.parent: login
*userIdText.width: 204
*userIdText.isCompound: "true"
*userIdText.compoundIcon: "text.xpm"
*userIdText.compoundName: "text_"
*userIdText.x: 169
*userIdText.y: 277
*userIdText.background: "LightSkyBlue3"
*userIdText.height: 36
*userIdText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*userIdText.activateCallback.source: public
*userIdText.activateCallback: login_moveFocusCb

*passwdText.class: text
*passwdText.static: true
*passwdText.name: passwdText
*passwdText.parent: login
*passwdText.width: 204
*passwdText.isCompound: "true"
*passwdText.compoundIcon: "text.xpm"
*passwdText.compoundName: "text_"
*passwdText.x: 169
*passwdText.y: 325
*passwdText.background: "LightSkyBlue3"
*passwdText.height: 36
*passwdText.fontList: "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1"
*passwdText.modifyVerifyCallback.source: public
*passwdText.modifyVerifyCallback: login_passwdCb

*label3.class: label
*label3.static: true
*label3.name: label3
*label3.parent: login
*label3.isCompound: "true"
*label3.compoundIcon: "label.xpm"
*label3.compoundName: "label_"
*label3.x: 68
*label3.y: 14
*label3.background: "#9ac0cd"
*label3.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label3.labelString: "ASF  IMS/DADS  OPERATIONS"
*label3.height: 36

*label206.class: label
*label206.static: true
*label206.name: label206
*label206.parent: login
*label206.isCompound: "true"
*label206.compoundIcon: "label.xpm"
*label206.compoundName: "label_"
*label206.x: 104
*label206.y: 56
*label206.background: "#9ac0cd"
*label206.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label206.height: 36
*label206.labelType: "pixmap"
*label206.labelPixmap: "/local/imsdads/app-defaults/pixmaps/rsat_small.xpm"

*separator34.class: separator
*separator34.static: true
*separator34.name: separator34
*separator34.parent: login
*separator34.width: 440
*separator34.height: 16
*separator34.isCompound: "true"
*separator34.compoundIcon: "sep.xpm"
*separator34.compoundName: "separator_"
*separator34.x: -1
*separator34.y: 375
*separator34.background: "#9ac0cd"

*okPB.class: pushButton
*okPB.static: true
*okPB.name: okPB
*okPB.parent: login
*okPB.isCompound: "true"
*okPB.compoundIcon: "push.xpm"
*okPB.compoundName: "push_Button"
*okPB.x: 35
*okPB.y: 395
*okPB.width: 120
*okPB.height: 37
*okPB.background: "cadetBlue"
*okPB.labelString: "OK"
*okPB.shadowThickness: 4
*okPB.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"

*cancelPB.class: pushButton
*cancelPB.static: true
*cancelPB.name: cancelPB
*cancelPB.parent: login
*cancelPB.isCompound: "true"
*cancelPB.compoundIcon: "push.xpm"
*cancelPB.compoundName: "push_Button"
*cancelPB.x: 279
*cancelPB.y: 395
*cancelPB.width: 120
*cancelPB.height: 37
*cancelPB.background: "cadetBlue"
*cancelPB.labelString: "Cancel"
*cancelPB.shadowThickness: 4
*cancelPB.fontList: "-adobe-new century schoolbook-bold-r-normal--17-120-100-100-p-99-iso8859-1"

*label150.class: label
*label150.static: true
*label150.name: label150
*label150.parent: login
*label150.isCompound: "true"
*label150.compoundIcon: "label.xpm"
*label150.compoundName: "label_"
*label150.x: 66
*label150.y: 229
*label150.background: "#9ac0cd"
*label150.fontList: "-adobe-times-medium-r-normal--11-80-100-100-p-54-iso8859-1"
*label150.labelString: "Copyright (C) 1996, California Institute of Technology.  U.S. Government\n"\
"Sponsorship under NASA Contract NAS7-1260 is acknowledged."
*label150.height: 36

