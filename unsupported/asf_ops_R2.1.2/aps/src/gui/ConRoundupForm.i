! UIMX ascii 2.9 key: 1877                                                      

*ConRoundupForm.class: form
*ConRoundupForm.classinc:
*ConRoundupForm.classspec:
*ConRoundupForm.classmembers:
*ConRoundupForm.classconstructor:
*ConRoundupForm.classdestructor:
*ConRoundupForm.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)ConRoundupForm.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.ConRoundupForm.i"\
\
#include "gui_utils.h"\
#include "satmenus.h"\
#include "cb_datetime.h"\
#include "cb_conrndup.h"\

*ConRoundupForm.ispecdecl:
*ConRoundupForm.funcdecl: swidget create_ConRoundupForm(swidget UxParent)
*ConRoundupForm.funcname: create_ConRoundupForm
*ConRoundupForm.funcdef: "swidget", "<create_ConRoundupForm>(%)"
*ConRoundupForm.argdecl: swidget UxParent;
*ConRoundupForm.arglist: UxParent
*ConRoundupForm.arglist.UxParent: "swidget", "%UxParent%"
*ConRoundupForm.icode:
*ConRoundupForm.fcode: return(rtrn);\

*ConRoundupForm.auxdecl:
*ConRoundupForm.static: true
*ConRoundupForm.name: ConRoundupForm
*ConRoundupForm.parent: NO_PARENT
*ConRoundupForm.defaultShell: topLevelShell
*ConRoundupForm.width: 338
*ConRoundupForm.height: 278
*ConRoundupForm.resizePolicy: "resize_none"
*ConRoundupForm.isCompound: "true"
*ConRoundupForm.compoundIcon: "form.xpm"
*ConRoundupForm.compoundName: "form_"
*ConRoundupForm.x: 220
*ConRoundupForm.y: 240
*ConRoundupForm.unitType: "pixels"
*ConRoundupForm.mappedWhenManaged: "true"

*label36.class: label
*label36.static: true
*label36.name: label36
*label36.parent: ConRoundupForm
*label36.isCompound: "true"
*label36.compoundIcon: "label.xpm"
*label36.compoundName: "label_"
*label36.x: 67
*label36.y: 19
*label36.width: 199
*label36.height: 35
*label36.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"
*label36.labelString: "CON ROUNDUP"
*label36.mappedWhenManaged: "true"

*label58.class: label
*label58.static: true
*label58.name: label58
*label58.parent: ConRoundupForm
*label58.isCompound: "true"
*label58.compoundIcon: "label.xpm"
*label58.compoundName: "label_"
*label58.x: 23
*label58.y: 106
*label58.height: 30
*label58.labelString: "START TIME:"
*label58.alignment: "alignment_end"

*label61.class: label
*label61.static: true
*label61.name: label61
*label61.parent: ConRoundupForm
*label61.isCompound: "true"
*label61.compoundIcon: "label.xpm"
*label61.compoundName: "label_"
*label61.x: 29
*label61.y: 153
*label61.height: 30
*label61.labelString: "STOP TIME:"
*label61.alignment: "alignment_end"

*TF_CON_RND_STRTTIME.class: textField
*TF_CON_RND_STRTTIME.static: true
*TF_CON_RND_STRTTIME.name: TF_CON_RND_STRTTIME
*TF_CON_RND_STRTTIME.parent: ConRoundupForm
*TF_CON_RND_STRTTIME.isCompound: "true"
*TF_CON_RND_STRTTIME.compoundIcon: "textfield.xpm"
*TF_CON_RND_STRTTIME.compoundName: "text_Field"
*TF_CON_RND_STRTTIME.x: 94
*TF_CON_RND_STRTTIME.y: 105
*TF_CON_RND_STRTTIME.height: 32
*TF_CON_RND_STRTTIME.columns: 21
*TF_CON_RND_STRTTIME.cursorPositionVisible: "false"
*TF_CON_RND_STRTTIME.editable: "true"
*TF_CON_RND_STRTTIME.resizeWidth: "false"
*TF_CON_RND_STRTTIME.text: ""
*TF_CON_RND_STRTTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_CON_RND_STRTTIME.maxLength: 21
*TF_CON_RND_STRTTIME.activateCallback.source: public
*TF_CON_RND_STRTTIME.activateCallback: cb_validate_ASF_datetime
*TF_CON_RND_STRTTIME.activateCallbackClientData: (XtPointer) "CON Roundup Start Time"
*TF_CON_RND_STRTTIME.focusCallback.source: public
*TF_CON_RND_STRTTIME.focusCallback: cb_toggle_cursor
*TF_CON_RND_STRTTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_CON_RND_STRTTIME.losingFocusCallback.source: public
*TF_CON_RND_STRTTIME.losingFocusCallback: cb_toggle_cursor
*TF_CON_RND_STRTTIME.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_CON_RND_STRTTIME.sensitive: "true"
*TF_CON_RND_STRTTIME.modifyVerifyCallback.source: public
*TF_CON_RND_STRTTIME.modifyVerifyCallback: cb_filter_text
*TF_CON_RND_STRTTIME.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*TF_CON_RND_STRTTIME.width: 185

*TF_CON_RND_STOPTIME.class: textField
*TF_CON_RND_STOPTIME.static: true
*TF_CON_RND_STOPTIME.name: TF_CON_RND_STOPTIME
*TF_CON_RND_STOPTIME.parent: ConRoundupForm
*TF_CON_RND_STOPTIME.isCompound: "true"
*TF_CON_RND_STOPTIME.compoundIcon: "textfield.xpm"
*TF_CON_RND_STOPTIME.compoundName: "text_Field"
*TF_CON_RND_STOPTIME.x: 94
*TF_CON_RND_STOPTIME.y: 152
*TF_CON_RND_STOPTIME.height: 32
*TF_CON_RND_STOPTIME.columns: 21
*TF_CON_RND_STOPTIME.cursorPositionVisible: "false"
*TF_CON_RND_STOPTIME.editable: "true"
*TF_CON_RND_STOPTIME.resizeWidth: "false"
*TF_CON_RND_STOPTIME.text: ""
*TF_CON_RND_STOPTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_CON_RND_STOPTIME.maxLength: 21
*TF_CON_RND_STOPTIME.activateCallback.source: public
*TF_CON_RND_STOPTIME.activateCallback: cb_validate_ASF_datetime
*TF_CON_RND_STOPTIME.activateCallbackClientData: (XtPointer) "CON Roundup Stop Time"
*TF_CON_RND_STOPTIME.focusCallback.source: public
*TF_CON_RND_STOPTIME.focusCallback: cb_toggle_cursor
*TF_CON_RND_STOPTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_CON_RND_STOPTIME.losingFocusCallback.source: public
*TF_CON_RND_STOPTIME.losingFocusCallback: cb_toggle_cursor
*TF_CON_RND_STOPTIME.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_CON_RND_STOPTIME.sensitive: "true"
*TF_CON_RND_STOPTIME.modifyVerifyCallback.source: public
*TF_CON_RND_STOPTIME.modifyVerifyCallback: cb_filter_text
*TF_CON_RND_STOPTIME.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*TF_CON_RND_STOPTIME.width: 185

*optionMenu_conRnd_stnid.class: rowColumn
*optionMenu_conRnd_stnid.static: true
*optionMenu_conRnd_stnid.name: optionMenu_conRnd_stnid
*optionMenu_conRnd_stnid.parent: ConRoundupForm
*optionMenu_conRnd_stnid.rowColumnType: "menu_option"
*optionMenu_conRnd_stnid.subMenuId: "subMenu_conRnd_stnid"
*optionMenu_conRnd_stnid.isCompound: "true"
*optionMenu_conRnd_stnid.compoundIcon: "optionM.xpm"
*optionMenu_conRnd_stnid.compoundName: "option_Menu"
*optionMenu_conRnd_stnid.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_conRnd_stnid.x: 19
*optionMenu_conRnd_stnid.y: 61
*optionMenu_conRnd_stnid.width: 215
*optionMenu_conRnd_stnid.height: 35
*optionMenu_conRnd_stnid.labelString: "STATION ID:"
*optionMenu_conRnd_stnid.sensitive: "true"

*subMenu_conRnd_stnid.class: rowColumn
*subMenu_conRnd_stnid.static: true
*subMenu_conRnd_stnid.name: subMenu_conRnd_stnid
*subMenu_conRnd_stnid.parent: optionMenu_conRnd_stnid
*subMenu_conRnd_stnid.rowColumnType: "menu_pulldown"
*subMenu_conRnd_stnid.labelString: ""
*subMenu_conRnd_stnid.height: 48
*subMenu_conRnd_stnid.resizeHeight: "false"
*subMenu_conRnd_stnid.x: 0
*subMenu_conRnd_stnid.y: 315
*subMenu_conRnd_stnid.sensitive: "true"
*subMenu_conRnd_stnid.mappedWhenManaged: "true"

*subMenu_conRnd_stnid_asf.class: pushButton
*subMenu_conRnd_stnid_asf.static: true
*subMenu_conRnd_stnid_asf.name: subMenu_conRnd_stnid_asf
*subMenu_conRnd_stnid_asf.parent: subMenu_conRnd_stnid
*subMenu_conRnd_stnid_asf.labelString: "ASF"
*subMenu_conRnd_stnid_asf.fontList: "rockwell-bold"
*subMenu_conRnd_stnid_asf.x: 2
*subMenu_conRnd_stnid_asf.y: 328
*subMenu_conRnd_stnid_asf.createCallback.source: public
*subMenu_conRnd_stnid_asf.createCallback: cb_build_station_option_menu

*pb_ConRnd_OK.class: pushButton
*pb_ConRnd_OK.static: true
*pb_ConRnd_OK.name: pb_ConRnd_OK
*pb_ConRnd_OK.parent: ConRoundupForm
*pb_ConRnd_OK.isCompound: "true"
*pb_ConRnd_OK.compoundIcon: "push.xpm"
*pb_ConRnd_OK.compoundName: "push_Button"
*pb_ConRnd_OK.x: 20
*pb_ConRnd_OK.y: 214
*pb_ConRnd_OK.width: 90
*pb_ConRnd_OK.height: 40
*pb_ConRnd_OK.labelString: "OK"
*pb_ConRnd_OK.fontList: "rockwell-bold"
*pb_ConRnd_OK.sensitive: "true"
*pb_ConRnd_OK.activateCallback.source: public
*pb_ConRnd_OK.activateCallback: cb_startConRoundup
*pb_ConRnd_OK.multiClick: "multiclick_discard"

*pb_ConRnd_QUIT.class: pushButton
*pb_ConRnd_QUIT.static: true
*pb_ConRnd_QUIT.name: pb_ConRnd_QUIT
*pb_ConRnd_QUIT.parent: ConRoundupForm
*pb_ConRnd_QUIT.isCompound: "true"
*pb_ConRnd_QUIT.compoundIcon: "push.xpm"
*pb_ConRnd_QUIT.compoundName: "push_Button"
*pb_ConRnd_QUIT.x: 237
*pb_ConRnd_QUIT.y: 214
*pb_ConRnd_QUIT.width: 90
*pb_ConRnd_QUIT.height: 40
*pb_ConRnd_QUIT.labelString: "QUIT"
*pb_ConRnd_QUIT.fontList: "rockwell-bold"
*pb_ConRnd_QUIT.activateCallback: {\
XtPopdown( gui_GetShellWidget( ConRoundupForm ) ) ;\
}
*pb_ConRnd_QUIT.multiClick: "multiclick_discard"

*pb_ConRnd_CLEAR.class: pushButton
*pb_ConRnd_CLEAR.static: true
*pb_ConRnd_CLEAR.name: pb_ConRnd_CLEAR
*pb_ConRnd_CLEAR.parent: ConRoundupForm
*pb_ConRnd_CLEAR.isCompound: "true"
*pb_ConRnd_CLEAR.compoundIcon: "push.xpm"
*pb_ConRnd_CLEAR.compoundName: "push_Button"
*pb_ConRnd_CLEAR.x: 131
*pb_ConRnd_CLEAR.y: 214
*pb_ConRnd_CLEAR.width: 90
*pb_ConRnd_CLEAR.height: 40
*pb_ConRnd_CLEAR.labelString: "CLEAR"
*pb_ConRnd_CLEAR.fontList: "rockwell-bold"
*pb_ConRnd_CLEAR.activateCallback.source: public
*pb_ConRnd_CLEAR.activateCallback: cb_clearConRoundupForm
*pb_ConRnd_CLEAR.multiClick: "multiclick_discard"

