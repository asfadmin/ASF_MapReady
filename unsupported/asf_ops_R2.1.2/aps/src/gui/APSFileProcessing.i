! UIMX ascii 2.9 key: 7529                                                      

*APSFileProcessing.class: form
*APSFileProcessing.classinc:
*APSFileProcessing.classspec:
*APSFileProcessing.classmembers:
*APSFileProcessing.classconstructor:
*APSFileProcessing.classdestructor:
*APSFileProcessing.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)APSFileProcessing.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.APSFileProcessing.i"\
\
#include "apsfiledef.h"\
#include "gui_utils.h"\
#include "cb_prcapsfile.h"\

*APSFileProcessing.ispecdecl:
*APSFileProcessing.funcdecl: swidget create_APSFileProcessing(swidget UxParent)
*APSFileProcessing.funcname: create_APSFileProcessing
*APSFileProcessing.funcdef: "swidget", "<create_APSFileProcessing>(%)"
*APSFileProcessing.argdecl: swidget UxParent;
*APSFileProcessing.arglist: UxParent
*APSFileProcessing.arglist.UxParent: "swidget", "%UxParent%"
*APSFileProcessing.icode: XmFontList	fontlist ;\
XmString	pb_string ;\
Widget		temp ;\
char		*typePtr ;\
int			i ;
*APSFileProcessing.fcode: /*\
-- Set up the pulldown menu for Get List,\
-- including callbacks\
*/\
\
/* get the resources from the Get List button */\
XtVaGetValues( getList_menuBar_top_b,\
	XmNfontList, &fontlist,\
	NULL ) ;\
\
/* set the first (& selected) menu button: INBOUND_ALL_TYPES */\
typePtr = INBOUND_ALL_TYPES ;\
pb_string = XmStringCreateLocalized( typePtr ) ;\
temp = XtVaCreateWidget( typePtr,\
	xmPushButtonWidgetClass,\
	getList_menuBar_p,\
	XmNlabelString, pb_string,\
	XmNfontList, fontlist,\
	NULL) ;\
XtManageChild( temp ) ;\
XtAddCallback( temp, XmNactivateCallback,\
	(XtCallbackProc) cb_fill_fileproc_list,\
	(XtPointer) typePtr ) ;\
XtVaSetValues( getList_menuBar,\
	XmNmenuHistory, temp,\
	NULL) ;\
\
for (i = 0 ; (typePtr = reports_inbound[i].type) != NULL ; i++ )\
{\
	pb_string = XmStringCreateLocalized( typePtr ) ;\
	temp = XtVaCreateWidget( typePtr,\
		xmPushButtonWidgetClass,\
		getList_menuBar_p,\
		XmNlabelString, pb_string,\
		XmNfontList, fontlist,\
		NULL) ;\
	XtManageChild( temp ) ;\
	XtAddCallback( temp, XmNactivateCallback,\
		(XtCallbackProc) cb_fill_fileproc_list,\
		(XtPointer) typePtr ) ;\
}\
\
return(rtrn);\

*APSFileProcessing.auxdecl:
*APSFileProcessing.name.source: public
*APSFileProcessing.static: false
*APSFileProcessing.name: APSFileProcessing
*APSFileProcessing.parent: NO_PARENT
*APSFileProcessing.parentExpression: UxParent
*APSFileProcessing.defaultShell: topLevelShell
*APSFileProcessing.width: 700
*APSFileProcessing.height: 596
*APSFileProcessing.resizePolicy: "resize_none"
*APSFileProcessing.isCompound: "true"
*APSFileProcessing.compoundIcon: "form.xpm"
*APSFileProcessing.compoundName: "form_"
*APSFileProcessing.x: 189
*APSFileProcessing.y: 77
*APSFileProcessing.unitType: "pixels"
*APSFileProcessing.dialogTitle: "APS File Processing"

*label161.class: label
*label161.static: true
*label161.name: label161
*label161.parent: APSFileProcessing
*label161.isCompound: "true"
*label161.compoundIcon: "label.xpm"
*label161.compoundName: "label_"
*label161.x: 223
*label161.y: 5
*label161.width: 256
*label161.height: 45
*label161.labelString: "APS  FILE  PROCESSING"
*label161.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindowList8.class: scrolledWindow
*scrolledWindowList8.static: true
*scrolledWindowList8.name: scrolledWindowList8
*scrolledWindowList8.parent: APSFileProcessing
*scrolledWindowList8.scrollingPolicy: "application_defined"
*scrolledWindowList8.visualPolicy: "variable"
*scrolledWindowList8.scrollBarDisplayPolicy: "static"
*scrolledWindowList8.shadowThickness: 0
*scrolledWindowList8.isCompound: "true"
*scrolledWindowList8.compoundIcon: "scrllist.xpm"
*scrolledWindowList8.compoundName: "scrolled_List"
*scrolledWindowList8.x: 18
*scrolledWindowList8.y: 81
*scrolledWindowList8.width: 630
*scrolledWindowList8.height: 160
*scrolledWindowList8.rightAttachment: "attach_none"
*scrolledWindowList8.leftAttachment: "attach_none"
*scrolledWindowList8.topAttachment: "attach_none"

*scrolledList_reports_inbound.class: scrolledList
*scrolledList_reports_inbound.static: true
*scrolledList_reports_inbound.name: scrolledList_reports_inbound
*scrolledList_reports_inbound.parent: scrolledWindowList8
*scrolledList_reports_inbound.width: 613
*scrolledList_reports_inbound.height: 160
*scrolledList_reports_inbound.scrollBarDisplayPolicy: "as_needed"
*scrolledList_reports_inbound.listSizePolicy: "constant"
*scrolledList_reports_inbound.visibleItemCount: 10
*scrolledList_reports_inbound.automaticSelection: "true"
*scrolledList_reports_inbound.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_reports_inbound.selectionPolicy: "browse_select"
*scrolledList_reports_inbound.itemCount: 0
*scrolledList_reports_inbound.listMarginHeight: 0
*scrolledList_reports_inbound.isSelectable: "false"
*scrolledList_reports_inbound.browseSelectionCallback: {\
int		itemPos ;\
\
itemPos = ((XmListCallbackStruct *) UxCallbackArg)->item_position ;\
XmListDeselectPos( UxWidget, itemPos ) ;\
}

*pushButton_APSFileProcQuit.class: pushButton
*pushButton_APSFileProcQuit.static: true
*pushButton_APSFileProcQuit.name: pushButton_APSFileProcQuit
*pushButton_APSFileProcQuit.parent: APSFileProcessing
*pushButton_APSFileProcQuit.isCompound: "true"
*pushButton_APSFileProcQuit.compoundIcon: "push.xpm"
*pushButton_APSFileProcQuit.compoundName: "push_Button"
*pushButton_APSFileProcQuit.x: 508
*pushButton_APSFileProcQuit.y: 272
*pushButton_APSFileProcQuit.width: 105
*pushButton_APSFileProcQuit.height: 40
*pushButton_APSFileProcQuit.labelString: "QUIT"
*pushButton_APSFileProcQuit.sensitive: "true"
*pushButton_APSFileProcQuit.fontList: "rockwell-bold"
*pushButton_APSFileProcQuit.activateCallback.source: public
*pushButton_APSFileProcQuit.activateCallback: cb_quit_file_processing

*label160.class: label
*label160.static: true
*label160.name: label160
*label160.parent: APSFileProcessing
*label160.isCompound: "true"
*label160.compoundIcon: "label.xpm"
*label160.compoundName: "label_"
*label160.x: 26
*label160.y: 60
*label160.width: 79
*label160.height: 20
*label160.labelString: "File Name"
*label160.fontList: "7x13bold"
*label160.alignment: "alignment_beginning"

*pushButton_process.class: pushButton
*pushButton_process.static: true
*pushButton_process.name: pushButton_process
*pushButton_process.parent: APSFileProcessing
*pushButton_process.isCompound: "true"
*pushButton_process.compoundIcon: "push.xpm"
*pushButton_process.compoundName: "push_Button"
*pushButton_process.x: 214
*pushButton_process.y: 272
*pushButton_process.width: 100
*pushButton_process.height: 40
*pushButton_process.labelString: "PROCESS"
*pushButton_process.sensitive: "false"
*pushButton_process.fontList: "rockwell-bold"
*pushButton_process.activateCallback.source: public
*pushButton_process.activateCallback: cb_process_all_files

*scrolledWindowText9.class: scrolledWindow
*scrolledWindowText9.static: true
*scrolledWindowText9.name: scrolledWindowText9
*scrolledWindowText9.parent: APSFileProcessing
*scrolledWindowText9.scrollingPolicy: "application_defined"
*scrolledWindowText9.visualPolicy: "variable"
*scrolledWindowText9.scrollBarDisplayPolicy: "static"
*scrolledWindowText9.isCompound: "true"
*scrolledWindowText9.compoundIcon: "scrltext.xpm"
*scrolledWindowText9.compoundName: "scrolled_Text"
*scrolledWindowText9.x: 50
*scrolledWindowText9.y: 376
*scrolledWindowText9.width: 590
*scrolledWindowText9.height: 200
*scrolledWindowText9.mappedWhenManaged: "true"

*scrolledText_procfile.class: scrolledText
*scrolledText_procfile.static: true
*scrolledText_procfile.name: scrolledText_procfile
*scrolledText_procfile.parent: scrolledWindowText9
*scrolledText_procfile.width: 669
*scrolledText_procfile.height: 141
*scrolledText_procfile.editMode: "multi_line_edit"
*scrolledText_procfile.editable: "false"
*scrolledText_procfile.mappedWhenManaged: "true"

*separator11.class: separator
*separator11.static: true
*separator11.name: separator11
*separator11.parent: APSFileProcessing
*separator11.width: 704
*separator11.height: 10
*separator11.isCompound: "true"
*separator11.compoundIcon: "sep.xpm"
*separator11.compoundName: "separator_"
*separator11.x: -2
*separator11.y: 331

*label166.class: label
*label166.static: true
*label166.name: label166
*label166.parent: APSFileProcessing
*label166.isCompound: "true"
*label166.compoundIcon: "label.xpm"
*label166.compoundName: "label_"
*label166.x: 45
*label166.y: 355
*label166.width: 590
*label166.height: 20
*label166.labelString: "MESSAGES"
*label166.fontList: "-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1"

*label33.class: label
*label33.static: true
*label33.name: label33
*label33.parent: APSFileProcessing
*label33.isCompound: "true"
*label33.compoundIcon: "label.xpm"
*label33.compoundName: "label_"
*label33.x: 239
*label33.y: 60
*label33.width: 120
*label33.height: 20
*label33.labelString: "Directory Name"
*label33.fontList: "7x13bold"
*label33.alignment: "alignment_beginning"

*pushButton_stop.class: pushButton
*pushButton_stop.static: true
*pushButton_stop.name: pushButton_stop
*pushButton_stop.parent: APSFileProcessing
*pushButton_stop.isCompound: "true"
*pushButton_stop.compoundIcon: "push.xpm"
*pushButton_stop.compoundName: "push_Button"
*pushButton_stop.x: 364
*pushButton_stop.y: 272
*pushButton_stop.width: 100
*pushButton_stop.height: 40
*pushButton_stop.labelString: "STOP"
*pushButton_stop.sensitive: "false"
*pushButton_stop.fontList: "rockwell-bold"
*pushButton_stop.activateCallback.source: public
*pushButton_stop.activateCallback: cb_stop_file_processing

*getList_menuBar.class: rowColumn
*getList_menuBar.static: true
*getList_menuBar.name: getList_menuBar
*getList_menuBar.parent: APSFileProcessing
*getList_menuBar.rowColumnType: "menu_bar"
*getList_menuBar.isCompound: "true"
*getList_menuBar.compoundIcon: "pulldownM.xpm"
*getList_menuBar.compoundName: "menu_Bar"
*getList_menuBar.x: 61
*getList_menuBar.y: 272
*getList_menuBar.width: 96
*getList_menuBar.height: 38
*getList_menuBar.menuAccelerator: "<KeyUp>F10"

*getList_menuBar_p.class: rowColumn
*getList_menuBar_p.static: true
*getList_menuBar_p.name: getList_menuBar_p
*getList_menuBar_p.parent: getList_menuBar
*getList_menuBar_p.rowColumnType: "menu_pulldown"
*getList_menuBar_p.x: 0
*getList_menuBar_p.y: 284

*getList_menuBar_top_b.class: cascadeButton
*getList_menuBar_top_b.static: true
*getList_menuBar_top_b.name: getList_menuBar_top_b
*getList_menuBar_top_b.parent: getList_menuBar
*getList_menuBar_top_b.labelString: "GET LIST"
*getList_menuBar_top_b.subMenuId: "getList_menuBar_p"
*getList_menuBar_top_b.marginHeight: 7
*getList_menuBar_top_b.marginWidth: 16
*getList_menuBar_top_b.fontList: "rockwell-bold"
*getList_menuBar_top_b.x: 5
*getList_menuBar_top_b.y: 279

