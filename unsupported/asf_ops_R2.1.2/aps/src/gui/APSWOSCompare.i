! UIMX ascii 2.9 key: 4                                                         

*APSWOSCompare.class: form
*APSWOSCompare.classinc:
*APSWOSCompare.classspec:
*APSWOSCompare.classmembers:
*APSWOSCompare.classconstructor:
*APSWOSCompare.classdestructor:
*APSWOSCompare.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.\
#endif\
 \
/*==============================================================================\
Filename:	vc_permstatus.c\
\
Description:	the gui builder code for reporting the status of the\
		multi-user permissions.\
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
#pragma ident   "@(#)APSWOSCompare.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.APSWOSCompare.i"\
\
#include "cb_apswoscompare.h"\
#include "apsfiledef.h"\
#include "gui_utils.h"\

*APSWOSCompare.ispecdecl:
*APSWOSCompare.funcdecl: swidget create_APSWOSCompare(swidget UxParent)
*APSWOSCompare.funcname: create_APSWOSCompare
*APSWOSCompare.funcdef: "swidget", "<create_APSWOSCompare>(%)"
*APSWOSCompare.argdecl: swidget UxParent;
*APSWOSCompare.arglist: UxParent
*APSWOSCompare.arglist.UxParent: "swidget", "%UxParent%"
*APSWOSCompare.icode:
*APSWOSCompare.fcode: return(rtrn);\

*APSWOSCompare.auxdecl:
*APSWOSCompare.name.source: public
*APSWOSCompare.static: false
*APSWOSCompare.name: APSWOSCompare
*APSWOSCompare.parent: NO_PARENT
*APSWOSCompare.parentExpression: UxParent
*APSWOSCompare.defaultShell: topLevelShell
*APSWOSCompare.width: 680
*APSWOSCompare.height: 515
*APSWOSCompare.resizePolicy: "resize_none"
*APSWOSCompare.isCompound: "true"
*APSWOSCompare.compoundIcon: "form.xpm"
*APSWOSCompare.compoundName: "form_"
*APSWOSCompare.x: 360
*APSWOSCompare.y: 450
*APSWOSCompare.unitType: "pixels"
*APSWOSCompare.dialogTitle: "APS WOS Compare"

*label73.class: label
*label73.static: true
*label73.name: label73
*label73.parent: APSWOSCompare
*label73.isCompound: "true"
*label73.compoundIcon: "label.xpm"
*label73.compoundName: "label_"
*label73.x: 42
*label73.y: 62
*label73.labelString: "Enter WOS File Name for Comparison with The APS DB:"

*textField1.class: textField
*textField1.static: true
*textField1.name: textField1
*textField1.parent: APSWOSCompare
*textField1.width: 595
*textField1.isCompound: "true"
*textField1.compoundIcon: "textfield.xpm"
*textField1.compoundName: "text_Field"
*textField1.x: 42
*textField1.y: 84
*textField1.leftOffset: 165
*textField1.createCallback.source: public
*textField1.createCallback: cb_mwos_filename
*textField1.activateCallback.source: public
*textField1.activateCallback: cb_aps_wos_compare

*label76.class: label
*label76.static: true
*label76.name: label76
*label76.parent: APSWOSCompare
*label76.isCompound: "true"
*label76.compoundIcon: "label.xpm"
*label76.compoundName: "label_"
*label76.x: 120
*label76.y: 12
*label76.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label76.height: 45
*label76.labelString: "McMURDO  WOS  COMPARISON  TOOL"
*label76.leftOffset: 237
*label76.leftAttachment: "attach_none"
*label76.width: 456
*label76.alignment: "alignment_center"

*pushButton_OK.class: pushButton
*pushButton_OK.static: true
*pushButton_OK.name: pushButton_OK
*pushButton_OK.parent: APSWOSCompare
*pushButton_OK.isCompound: "true"
*pushButton_OK.compoundIcon: "push.xpm"
*pushButton_OK.compoundName: "push_Button"
*pushButton_OK.x: 43
*pushButton_OK.y: 138
*pushButton_OK.labelString: "OK"
*pushButton_OK.fontList: "rockwell-bold"
*pushButton_OK.height: 40
*pushButton_OK.width: 105
*pushButton_OK.leftOffset: 157
*pushButton_OK.activateCallback.source: public
*pushButton_OK.activateCallback: cb_aps_wos_compare

*pushButton_QUIT.class: pushButton
*pushButton_QUIT.static: true
*pushButton_QUIT.name: pushButton_QUIT
*pushButton_QUIT.parent: APSWOSCompare
*pushButton_QUIT.isCompound: "true"
*pushButton_QUIT.compoundIcon: "push.xpm"
*pushButton_QUIT.compoundName: "push_Button"
*pushButton_QUIT.x: 533
*pushButton_QUIT.y: 138
*pushButton_QUIT.labelString: "QUIT"
*pushButton_QUIT.fontList: "rockwell-bold"
*pushButton_QUIT.height: 40
*pushButton_QUIT.width: 102
*pushButton_QUIT.rightOffset: 157
*pushButton_QUIT.leftOffset: 418
*pushButton_QUIT.activateCallback: {\
extern Widget apswoscompare_form;\
XtPopdown(XtParent(apswoscompare_form)) ;\
}

*scrolledWindowText10.class: scrolledWindow
*scrolledWindowText10.static: true
*scrolledWindowText10.name: scrolledWindowText10
*scrolledWindowText10.parent: APSWOSCompare
*scrolledWindowText10.scrollingPolicy: "application_defined"
*scrolledWindowText10.visualPolicy: "variable"
*scrolledWindowText10.scrollBarDisplayPolicy: "static"
*scrolledWindowText10.isCompound: "true"
*scrolledWindowText10.compoundIcon: "scrltext.xpm"
*scrolledWindowText10.compoundName: "scrolled_Text"
*scrolledWindowText10.x: 40
*scrolledWindowText10.y: 241
*scrolledWindowText10.leftOffset: 40
*scrolledWindowText10.rightOffset: 40
*scrolledWindowText10.width: 600

*scrolledText_woscompare.class: scrolledText
*scrolledText_woscompare.static: true
*scrolledText_woscompare.name: scrolledText_woscompare
*scrolledText_woscompare.parent: scrolledWindowText10
*scrolledText_woscompare.width: 590
*scrolledText_woscompare.height: 229
*scrolledText_woscompare.editMode: "multi_line_edit"
*scrolledText_woscompare.editable: "false"

*separator14.class: separator
*separator14.static: true
*separator14.name: separator14
*separator14.parent: APSWOSCompare
*separator14.width: 679
*separator14.height: 20
*separator14.isCompound: "true"
*separator14.compoundIcon: "sep.xpm"
*separator14.compoundName: "separator_"
*separator14.x: 1
*separator14.y: 192

*label74.class: label
*label74.static: true
*label74.name: label74
*label74.parent: APSWOSCompare
*label74.isCompound: "true"
*label74.compoundIcon: "label.xpm"
*label74.compoundName: "label_"
*label74.x: 46
*label74.y: 218
*label74.fontList: "-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1"
*label74.height: 20
*label74.labelString: "MESSAGES"
*label74.width: 590

