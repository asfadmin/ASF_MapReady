! UIMX ascii 2.9 key: 1363                                                      

*APSMsgBoxDlg.class: messageBoxDialog
*APSMsgBoxDlg.classinc:
*APSMsgBoxDlg.classspec:
*APSMsgBoxDlg.classmembers:
*APSMsgBoxDlg.classconstructor:
*APSMsgBoxDlg.classdestructor:
*APSMsgBoxDlg.gbldecl: #include <stdio.h>\
#ifdef COPYRIGHT\
Copyright (c)1996, California Institute of Technology.\
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.\
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
#pragma ident	"@(#)APSMsgBoxDlg.i	5.2 98/01/08 APS/ASF"\
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.APSMsgBoxDlg.i"\
\
#include "gui_utils.h"\
\
extern Widget mainIface ;\
\
void popup_message() ;\

*APSMsgBoxDlg.ispecdecl:
*APSMsgBoxDlg.funcdecl: swidget create_APSMsgBoxDlg(UxParent)\
swidget UxParent;
*APSMsgBoxDlg.funcname: create_APSMsgBoxDlg
*APSMsgBoxDlg.funcdef: "swidget", "<create_APSMsgBoxDlg>(%)"
*APSMsgBoxDlg.argdecl: swidget UxParent;
*APSMsgBoxDlg.arglist: UxParent
*APSMsgBoxDlg.arglist.UxParent: "swidget", "%UxParent%"
*APSMsgBoxDlg.icode:
*APSMsgBoxDlg.fcode: XtUnmanageChild(XmMessageBoxGetChild(rtrn, XmDIALOG_CANCEL_BUTTON)) ;\
XtUnmanageChild(XmMessageBoxGetChild(rtrn, XmDIALOG_HELP_BUTTON)) ;\
return(rtrn);\

*APSMsgBoxDlg.auxdecl: /*==============================================================================\
Function:       popup_message\
 \
Description:    creates and displays messages in a MessageDialog widget\
 \
Parameters:     dialog_type	(5/96: Unused) specifies the symbol type\
                           	to display in the widget\
\
                dialog_title	title to use for the widget\
\
                msg        	message to be displayed\
\
		grab_kind  	XtPopup grab_kind\
 \
Returns:\
 \
Creator:        Unknown\
 \
Creation Date:\
 \
Notes:\
                5/96:	changed the message area to be a scrolled Text\
                     	widget to handle long and mega-line messages.\
                5/96:	got rid of using the dialog_type, but kept the\
                     	parameter for historical purposes.\
==============================================================================*/\
void\
popup_message(dialog_type, dialog_title, msg, grab_kind)\
    int dialog_type ;\
    char *dialog_title ;\
    char *msg ;\
    XtGrabKind grab_kind ;\
{\
    Widget msg_box ;\
    XmString XmString_dialog_title ;\
 \
    msg_box = (Widget) create_APSMsgBoxDlg(XtParent(mainIface)) ;\
    XmString_dialog_title = XmStringCreateSimple(dialog_title) ;\
\
    XtVaSetValues(msg_box,\
        XmNdialogType, XmDIALOG_TEMPLATE,\
        XmNdialogTitle, XmString_dialog_title,\
        NULL) ;\
\
    XtVaSetValues(scrolledText_msgBox,\
        XmNvalue, msg,\
        NULL) ;\
\
    XmStringFree(XmString_dialog_title) ;\
 \
    XtManageChild(msg_box) ;\
    XtPopup(XtParent(msg_box), grab_kind) ;\
\
    XFlush( XtDisplay( msg_box ) ) ;\
\
    DisplayXCursor(True) ;\
\
    return ;\
}\
\
\
/* ARGSUSED2 */\
void\
destroy_message_box(widget_UNUSED, client_data, cbs)\
    Widget widget_UNUSED ;\
    XtPointer client_data, cbs ;\
{\
	DisplayXCursor(False) ;\
    XtDestroyWidget((Widget) client_data) ;\
}\
\

*APSMsgBoxDlg.static: true
*APSMsgBoxDlg.name: APSMsgBoxDlg
*APSMsgBoxDlg.parent: NO_PARENT
*APSMsgBoxDlg.parentExpression: UxParent
*APSMsgBoxDlg.defaultShell: topLevelShell
*APSMsgBoxDlg.msgDialogType: "dialog_message"
*APSMsgBoxDlg.isCompound: "true"
*APSMsgBoxDlg.compoundIcon: "messageD.xpm"
*APSMsgBoxDlg.compoundName: "msgBox_Dialog"
*APSMsgBoxDlg.unitType: "pixels"
*APSMsgBoxDlg.okCallback: {\
UxPopdownInterface(UxThisWidget) ;\
XtDestroyWidget(UxWidget) ;\
DisplayXCursor(False) ;\
\
}
*APSMsgBoxDlg.buttonFontList: "rockwell-bold"
*APSMsgBoxDlg.allowShellResize: "false"
*APSMsgBoxDlg.noResize: "false"
*APSMsgBoxDlg.createManaged: "false"
*APSMsgBoxDlg.dialogStyle: "dialog_full_application_modal"
*APSMsgBoxDlg.defaultPosition: "false"
*APSMsgBoxDlg.x: 25
*APSMsgBoxDlg.y: 401
*APSMsgBoxDlg.height: 287
*APSMsgBoxDlg.width: 532

*scrolledWindowText_msgBox.class: scrolledWindow
*scrolledWindowText_msgBox.static: true
*scrolledWindowText_msgBox.name: scrolledWindowText_msgBox
*scrolledWindowText_msgBox.parent: APSMsgBoxDlg
*scrolledWindowText_msgBox.scrollingPolicy: "application_defined"
*scrolledWindowText_msgBox.visualPolicy: "variable"
*scrolledWindowText_msgBox.scrollBarDisplayPolicy: "static"
*scrolledWindowText_msgBox.isCompound: "true"
*scrolledWindowText_msgBox.compoundIcon: "scrltext.xpm"
*scrolledWindowText_msgBox.compoundName: "scrolled_Text"
*scrolledWindowText_msgBox.x: 11
*scrolledWindowText_msgBox.y: 25

*scrolledText_msgBox.class: scrolledText
*scrolledText_msgBox.static: true
*scrolledText_msgBox.name: scrolledText_msgBox
*scrolledText_msgBox.parent: scrolledWindowText_msgBox
*scrolledText_msgBox.width: 476
*scrolledText_msgBox.height: 168
*scrolledText_msgBox.cursorPositionVisible: "false"
*scrolledText_msgBox.editable: "false"
*scrolledText_msgBox.editMode: "multi_line_edit"

