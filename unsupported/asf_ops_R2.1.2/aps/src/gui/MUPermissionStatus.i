! UIMX ascii 2.9 key: 7896                                                      

*MUPermissionStatus.class: form
*MUPermissionStatus.classinc:
*MUPermissionStatus.classspec:
*MUPermissionStatus.classmembers:
*MUPermissionStatus.classconstructor:
*MUPermissionStatus.classdestructor:
*MUPermissionStatus.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)MUPermissionStatus.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.MUPermissionStatus.i"\
\
#include "cb_permstatus.h"\

*MUPermissionStatus.ispecdecl:
*MUPermissionStatus.funcdecl: swidget create_MUPermissionStatus(swidget UxParent)
*MUPermissionStatus.funcname: create_MUPermissionStatus
*MUPermissionStatus.funcdef: "swidget", "<create_MUPermissionStatus>(%)"
*MUPermissionStatus.argdecl: swidget UxParent;
*MUPermissionStatus.arglist: UxParent
*MUPermissionStatus.arglist.UxParent: "swidget", "%UxParent%"
*MUPermissionStatus.icode:
*MUPermissionStatus.fcode: XtAddCallback( XtParent( rtrn ), XtNpopupCallback,\
		cb_popup_permstatus, NULL ) ;\
\
return(rtrn);\

*MUPermissionStatus.auxdecl:
*MUPermissionStatus.name.source: public
*MUPermissionStatus.static: false
*MUPermissionStatus.name: MUPermissionStatus
*MUPermissionStatus.parent: NO_PARENT
*MUPermissionStatus.parentExpression: UxParent
*MUPermissionStatus.defaultShell: topLevelShell
*MUPermissionStatus.width: 767
*MUPermissionStatus.height: 225
*MUPermissionStatus.resizePolicy: "resize_none"
*MUPermissionStatus.isCompound: "true"
*MUPermissionStatus.compoundIcon: "form.xpm"
*MUPermissionStatus.compoundName: "form_"
*MUPermissionStatus.x: 0
*MUPermissionStatus.y: 565
*MUPermissionStatus.unitType: "pixels"

*AutoPoll_tb.class: toggleButton
*AutoPoll_tb.name.source: public
*AutoPoll_tb.static: false
*AutoPoll_tb.name: AutoPoll_tb
*AutoPoll_tb.parent: MUPermissionStatus
*AutoPoll_tb.isCompound: "true"
*AutoPoll_tb.compoundIcon: "toggle.xpm"
*AutoPoll_tb.compoundName: "toggle_Button"
*AutoPoll_tb.x: 34
*AutoPoll_tb.y: 179
*AutoPoll_tb.width: 220
*AutoPoll_tb.height: 20
*AutoPoll_tb.indicatorOn: "true"
*AutoPoll_tb.labelString: "Auto-update every 10 seconds"
*AutoPoll_tb.set: "true"
*AutoPoll_tb.sensitive: "true"
*AutoPoll_tb.valueChangedCallback.source: public
*AutoPoll_tb.valueChangedCallback: cb_autoUpdate_toggled
*AutoPoll_tb.alignment: "alignment_beginning"

*MU_perm_scrolledWindow.class: scrolledWindow
*MU_perm_scrolledWindow.static: true
*MU_perm_scrolledWindow.name: MU_perm_scrolledWindow
*MU_perm_scrolledWindow.parent: MUPermissionStatus
*MU_perm_scrolledWindow.scrollingPolicy: "application_defined"
*MU_perm_scrolledWindow.visualPolicy: "variable"
*MU_perm_scrolledWindow.scrollBarDisplayPolicy: "static"
*MU_perm_scrolledWindow.shadowThickness: 0
*MU_perm_scrolledWindow.isCompound: "true"
*MU_perm_scrolledWindow.compoundIcon: "scrllist.xpm"
*MU_perm_scrolledWindow.compoundName: "scrolled_List"
*MU_perm_scrolledWindow.x: 36
*MU_perm_scrolledWindow.y: 39
*MU_perm_scrolledWindow.height: 130

*MU_perm_scrolledList.class: scrolledList
*MU_perm_scrolledList.name.source: public
*MU_perm_scrolledList.static: false
*MU_perm_scrolledList.name: MU_perm_scrolledList
*MU_perm_scrolledList.parent: MU_perm_scrolledWindow
*MU_perm_scrolledList.width: 691
*MU_perm_scrolledList.height: 130
*MU_perm_scrolledList.visibleItemCount: 10
*MU_perm_scrolledList.fontList: "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8"
*MU_perm_scrolledList.listSizePolicy: "constant"
*MU_perm_scrolledList.itemCount: 0

*MU_permStatusRefresh_pb.class: pushButton
*MU_permStatusRefresh_pb.static: true
*MU_permStatusRefresh_pb.name: MU_permStatusRefresh_pb
*MU_permStatusRefresh_pb.parent: MUPermissionStatus
*MU_permStatusRefresh_pb.x: 3
*MU_permStatusRefresh_pb.y: 39
*MU_permStatusRefresh_pb.width: 30
*MU_permStatusRefresh_pb.height: 130
*MU_permStatusRefresh_pb.fontList: "rockwell-bold"
*MU_permStatusRefresh_pb.labelString: "R\nE\nF\nR\nE\nS\nH"
*MU_permStatusRefresh_pb.activateCallback.source: public
*MU_permStatusRefresh_pb.activateCallback: cb_refresh_perms

*MU_Quit_pb.class: pushButton
*MU_Quit_pb.static: true
*MU_Quit_pb.name: MU_Quit_pb
*MU_Quit_pb.parent: MUPermissionStatus
*MU_Quit_pb.x: 635
*MU_Quit_pb.y: 179
*MU_Quit_pb.width: 90
*MU_Quit_pb.height: 40
*MU_Quit_pb.fontList: "rockwell-bold"
*MU_Quit_pb.labelString: "QUIT"
*MU_Quit_pb.leftOffset: 455
*MU_Quit_pb.topOffset: 471
*MU_Quit_pb.activateCallback.source: public
*MU_Quit_pb.activateCallback: cb_quit_permstatus

*label74.class: label
*label74.static: true
*label74.name: label74
*label74.parent: MUPermissionStatus
*label74.x: 37
*label74.y: 8
*label74.width: 691
*label74.height: 31
*label74.alignment: "alignment_beginning"
*label74.labelString: "User     Node       Command/Activity                       Darid/     Start Time            Stop Time            \n                                                           Station"

*PollTime_lbl.class: label
*PollTime_lbl.name.source: public
*PollTime_lbl.static: false
*PollTime_lbl.name: PollTime_lbl
*PollTime_lbl.parent: MUPermissionStatus
*PollTime_lbl.x: 126
*PollTime_lbl.y: 205
*PollTime_lbl.width: 144
*PollTime_lbl.height: 14
*PollTime_lbl.labelString: "                     )"
*PollTime_lbl.alignment: "alignment_beginning"

*label73.class: label
*label73.static: true
*label73.name: label73
*label73.parent: MUPermissionStatus
*label73.isCompound: "true"
*label73.compoundIcon: "label.xpm"
*label73.compoundName: "label_"
*label73.x: 34
*label73.y: 205
*label73.width: 95
*label73.height: 14
*label73.labelString: "(Last Updated:"

