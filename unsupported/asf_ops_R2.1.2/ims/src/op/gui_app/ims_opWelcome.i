! UIMX ascii 2.9 key: 4498                                                      

*welcome.class: form
*welcome.classinc:
*welcome.classspec:
*welcome.classmembers:
*welcome.classconstructor:
*welcome.classdestructor:
*welcome.gbldecl: #include <stdio.h>\
#include "ims_opCb.h"
*welcome.ispecdecl:
*welcome.funcdecl: swidget create_welcome(swidget UxParent)
*welcome.funcname: create_welcome
*welcome.funcdef: "swidget", "<create_welcome>(%)"
*welcome.argdecl: swidget UxParent;
*welcome.arglist: UxParent
*welcome.arglist.UxParent: "swidget", "%UxParent%"
*welcome.icode:
*welcome.fcode: return(rtrn);\

*welcome.auxdecl:
*welcome.static: true
*welcome.name: welcome
*welcome.parent: NO_PARENT
*welcome.parentExpression: UxParent
*welcome.defaultShell: transientShell
*welcome.width: 877
*welcome.height: 760
*welcome.resizePolicy: "resize_none"
*welcome.isCompound: "true"
*welcome.compoundIcon: "form.xpm"
*welcome.compoundName: "form_"
*welcome.x: 130
*welcome.y: 82
*welcome.unitType: "pixels"
*welcome.allowShellResize: "true"
*welcome.createCallback.source: public
*welcome.createCallback: welcome_create_op_interfaces
*welcome.background: "CadetBlue"
*welcome.borderPixmap: "/usr/openwin/include/X11/bitmaps/cross_weave"
*welcome.borderWidth: 10
*welcome.borderColor: "#9ac0cd"

*welcomeTitleLB1.class: label
*welcomeTitleLB1.static: true
*welcomeTitleLB1.name: welcomeTitleLB1
*welcomeTitleLB1.parent: welcome
*welcomeTitleLB1.isCompound: "true"
*welcomeTitleLB1.compoundIcon: "label.xpm"
*welcomeTitleLB1.compoundName: "label_"
*welcomeTitleLB1.x: 232
*welcomeTitleLB1.y: 44
*welcomeTitleLB1.fontList: "-adobe-new century schoolbook-bold-r-normal--34-240-100-100-p-193-iso8859-1"
*welcomeTitleLB1.labelString: "ALASKA  SAR  FACILITY"
*welcomeTitleLB1.background: "CadetBlue"
*welcomeTitleLB1.leftOffset: 224
*welcomeTitleLB1.rightAttachment: "attach_form"
*welcomeTitleLB1.rightOffset: 224

*welcomeTitleLB2.class: label
*welcomeTitleLB2.static: true
*welcomeTitleLB2.name: welcomeTitleLB2
*welcomeTitleLB2.parent: welcome
*welcomeTitleLB2.isCompound: "true"
*welcomeTitleLB2.compoundIcon: "label.xpm"
*welcomeTitleLB2.compoundName: "label_"
*welcomeTitleLB2.x: 110
*welcomeTitleLB2.y: 96
*welcomeTitleLB2.fontList: "-adobe-new century schoolbook-bold-r-normal--25-180-100-100-p-149-iso8859-1"
*welcomeTitleLB2.labelString: "GEOPHYSICAL INSTITUTE,  FAIRBANKS,  ALASKA "
*welcomeTitleLB2.background: "CadetBlue"
*welcomeTitleLB2.shadowThickness: 0
*welcomeTitleLB2.leftOffset: 97
*welcomeTitleLB2.rightAttachment: "attach_none"
*welcomeTitleLB2.rightOffset: 0

*welcomeTitleLB3.class: label
*welcomeTitleLB3.static: true
*welcomeTitleLB3.name: welcomeTitleLB3
*welcomeTitleLB3.parent: welcome
*welcomeTitleLB3.isCompound: "true"
*welcomeTitleLB3.compoundIcon: "label.xpm"
*welcomeTitleLB3.compoundName: "label_"
*welcomeTitleLB3.x: 272
*welcomeTitleLB3.y: 480
*welcomeTitleLB3.fontList: "-adobe-new century schoolbook-bold-r-normal--25-180-100-100-p-149-iso8859-1"
*welcomeTitleLB3.labelString: "IMS/DADS  OPERATIONS"
*welcomeTitleLB3.background: "CadetBlue"
*welcomeTitleLB3.width: 337
*welcomeTitleLB3.leftOffset: 270
*welcomeTitleLB3.rightAttachment: "attach_form"
*welcomeTitleLB3.rightOffset: 270

*alaska1LBL.class: label
*alaska1LBL.static: true
*alaska1LBL.name: alaska1LBL
*alaska1LBL.parent: welcome
*alaska1LBL.isCompound: "true"
*alaska1LBL.compoundIcon: "label.xpm"
*alaska1LBL.compoundName: "label_"
*alaska1LBL.x: 188
*alaska1LBL.y: 132
*alaska1LBL.labelPixmap: "/local/imsdads/app-defaults/pixmaps/alaska1.xpm"
*alaska1LBL.labelType: "pixmap"
*alaska1LBL.background: "CadetBlue"
*alaska1LBL.shadowThickness: 0
*alaska1LBL.width: 569
*alaska1LBL.height: 292
*alaska1LBL.leftOffset: 154
*alaska1LBL.rightAttachment: "attach_none"
*alaska1LBL.alignment: "alignment_beginning"
*alaska1LBL.marginWidth: 0

*photoPB.class: pushButton
*photoPB.static: true
*photoPB.name: photoPB
*photoPB.parent: welcome
*photoPB.isCompound: "true"
*photoPB.compoundIcon: "push.xpm"
*photoPB.compoundName: "push_Button"
*photoPB.x: 532
*photoPB.y: 600
*photoPB.width: 204
*photoPB.height: 40
*photoPB.labelString: "PHOTO     MGMT"
*photoPB.fontList: "-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1"
*photoPB.background: "#9ac0cd"
*photoPB.borderWidth: 0
*photoPB.highlightThickness: 2
*photoPB.shadowThickness: 5
*photoPB.activateCallback.source: public
*photoPB.activateCallback: welcome_photoCb

*accountPB.class: pushButton
*accountPB.static: true
*accountPB.name: accountPB
*accountPB.parent: welcome
*accountPB.isCompound: "true"
*accountPB.compoundIcon: "push.xpm"
*accountPB.compoundName: "push_Button"
*accountPB.x: 532
*accountPB.y: 536
*accountPB.width: 204
*accountPB.height: 40
*accountPB.labelString: "ACCOUNT  MGMT"
*accountPB.fontList: "-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1"
*accountPB.background: "#9ac0cd"
*accountPB.shadowThickness: 5
*accountPB.activateCallback.source: public
*accountPB.activateCallback: welcome_accountCb

*orderPB.class: pushButton
*orderPB.static: true
*orderPB.name: orderPB
*orderPB.parent: welcome
*orderPB.isCompound: "true"
*orderPB.compoundIcon: "push.xpm"
*orderPB.compoundName: "push_Button"
*orderPB.x: 160
*orderPB.y: 536
*orderPB.width: 204
*orderPB.height: 40
*orderPB.labelString: "ORDER    MGMT"
*orderPB.fontList: "-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1"
*orderPB.activateCallback.source: public
*orderPB.activateCallback: welcome_orderCb
*orderPB.background: "#9ac0cd"
*orderPB.shadowThickness: 5

*reportsPB.class: pushButton
*reportsPB.static: true
*reportsPB.name: reportsPB
*reportsPB.parent: welcome
*reportsPB.isCompound: "true"
*reportsPB.compoundIcon: "push.xpm"
*reportsPB.compoundName: "push_Button"
*reportsPB.x: 160
*reportsPB.y: 600
*reportsPB.width: 204
*reportsPB.height: 40
*reportsPB.labelString: "FILM     MGMT"
*reportsPB.fontList: "-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1"
*reportsPB.background: "#9ac0cd"
*reportsPB.shadowThickness: 5
*reportsPB.activateCallback.source: public
*reportsPB.activateCallback: welcome_filmCb

*exitPB.class: pushButton
*exitPB.static: true
*exitPB.name: exitPB
*exitPB.parent: welcome
*exitPB.isCompound: "true"
*exitPB.compoundIcon: "push.xpm"
*exitPB.compoundName: "push_Button"
*exitPB.x: 532
*exitPB.y: 666
*exitPB.width: 204
*exitPB.height: 40
*exitPB.labelString: "EXIT"
*exitPB.fontList: "-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1"
*exitPB.background: "#9ac0cd"
*exitPB.shadowThickness: 5
*exitPB.activateCallback.source: public
*exitPB.activateCallback: welcome_exitCb

*alaska2LBL.class: label
*alaska2LBL.static: true
*alaska2LBL.name: alaska2LBL
*alaska2LBL.parent: welcome
*alaska2LBL.isCompound: "true"
*alaska2LBL.compoundIcon: "label.xpm"
*alaska2LBL.compoundName: "label_"
*alaska2LBL.x: 440
*alaska2LBL.y: 132
*alaska2LBL.labelPixmap: "/local/imsdads/app-defaults/pixmaps/alaska2.xpm"
*alaska2LBL.labelType: "pixmap"
*alaska2LBL.background: "CadetBlue"
*alaska2LBL.shadowThickness: 0
*alaska2LBL.width: 569
*alaska2LBL.height: 292
*alaska2LBL.alignment: "alignment_beginning"
*alaska2LBL.marginWidth: 0

*dl2dtkPB.class: pushButton
*dl2dtkPB.name.source: public
*dl2dtkPB.static: false
*dl2dtkPB.name: dl2dtkPB
*dl2dtkPB.parent: welcome
*dl2dtkPB.isCompound: "true"
*dl2dtkPB.compoundIcon: "push.xpm"
*dl2dtkPB.compoundName: "push_Button"
*dl2dtkPB.x: 160
*dl2dtkPB.y: 666
*dl2dtkPB.width: 204
*dl2dtkPB.height: 40
*dl2dtkPB.labelString: "DOWNLINK MGMT"
*dl2dtkPB.fontList: "-adobe-new century schoolbook-bold-r-normal--18-180-75-75-p-113-iso8859-1"
*dl2dtkPB.activateCallback.source: public
*dl2dtkPB.activateCallback: welcome_downlinkCb
*dl2dtkPB.background: "#9ac0cd"
*dl2dtkPB.shadowThickness: 5

