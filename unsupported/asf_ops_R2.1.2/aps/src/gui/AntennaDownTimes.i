! UIMX ascii 2.9 key: 7947                                                      

*AntennaDownTimeManager.class: form
*AntennaDownTimeManager.classinc:
*AntennaDownTimeManager.classspec:
*AntennaDownTimeManager.classmembers:
*AntennaDownTimeManager.classconstructor:
*AntennaDownTimeManager.classdestructor:
*AntennaDownTimeManager.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)AntennaDownTimes.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.AntennaDownTimes.i"\
\
#include <stdlib.h>\
\
#include <Xm/DialogS.h>\
\
#include "db_sybint.h"\
#include "aps_db_table.h"\
\
#include "satmenus.h"\
#include "cb_sortform.h"\
#include "cb_searchform.h"\
#include "cb_antdntimes.h"\
#include "cb_datetime.h"\
\
extern Widget AntennaDownTime_manager;\
extern Widget filebox ;\

*AntennaDownTimeManager.ispecdecl:
*AntennaDownTimeManager.funcdecl: swidget create_AntennaDownTimeManager(swidget UxParent)
*AntennaDownTimeManager.funcname: create_AntennaDownTimeManager
*AntennaDownTimeManager.funcdef: "swidget", "<create_AntennaDownTimeManager>(%)"
*AntennaDownTimeManager.argdecl: swidget UxParent;
*AntennaDownTimeManager.arglist: UxParent
*AntennaDownTimeManager.arglist.UxParent: "swidget", "%UxParent%"
*AntennaDownTimeManager.icode: SORT_INFO *sortinfo ;\
SEARCH_INFO *searchinfo ;\
PERIOD_WIDGETS *down_times ;\
ANTENNA_CLIENT_DATA *antenna_menu ;
*AntennaDownTimeManager.fcode: sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;\
searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;\
antenna_menu =\
	(ANTENNA_CLIENT_DATA *) malloc(sizeof(ANTENNA_CLIENT_DATA));\
\
sortinfo->table_name = APS_TABLE(ANTENNA_DOWN_TIMES) ;\
sortinfo->field_to_update = (Widget) TF_DownTime_sortclause ;\
\
searchinfo->table_name = APS_TABLE(ANTENNA_DOWN_TIMES) ;\
searchinfo->field_to_update  = (Widget) TF_DownTime_searchclause ;\
\
down_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
down_times->start = (Widget) TF_ANT_DN_TIMES_STRTTIME ;\
down_times->stop = (Widget) TF_ANT_DN_TIMES_STOPTIME ;\
\
antenna_menu->noAntenna_flag = False ;\
antenna_menu->menuWidgets.optionmenu = (Widget) optionMenu_antenna ;\
antenna_menu->menuWidgets.submenu = (Widget) subMenu_antenna_down_antenna ;\
\
XtAddCallback( subMenu_antenna_down_stn_id, XmNentryCallback,\
	(XtCallbackProc) cb_set_antenna_menus,\
	(XtPointer ) antenna_menu );\
\
XtAddCallback( pushButton_SortDownTime, XmNactivateCallback,\
    (XtCallbackProc) cb_edit_sort_columns,\
    (XtPointer) sortinfo );\
 \
XtAddCallback( pushButton_SearchDownTime, XmNactivateCallback,\
    (XtCallbackProc) cb_edit_search_columns,\
    (XtPointer) searchinfo );\
\
XtAddCallback(TF_DownTime_total_days, XmNactivateCallback,\
	(XtCallbackProc) cb_adjust_ASF_datetimes,\
	(XtPointer) down_times) ;\
\
XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
	cb_show_antdntime_records, (XtPointer *) scrolledList_DownTimes) ;\
\
return(rtrn);\

*AntennaDownTimeManager.auxdecl:
*AntennaDownTimeManager.static: true
*AntennaDownTimeManager.name: AntennaDownTimeManager
*AntennaDownTimeManager.parent: NO_PARENT
*AntennaDownTimeManager.parentExpression: UxParent
*AntennaDownTimeManager.defaultShell: topLevelShell
*AntennaDownTimeManager.height: 639
*AntennaDownTimeManager.resizePolicy: "resize_none"
*AntennaDownTimeManager.isCompound: "true"
*AntennaDownTimeManager.compoundIcon: "form.xpm"
*AntennaDownTimeManager.compoundName: "form_"
*AntennaDownTimeManager.unitType: "pixels"
*AntennaDownTimeManager.dialogTitle: "APS:ANTENNA Down Time"
*AntennaDownTimeManager.width: 732
*AntennaDownTimeManager.y: 5
*AntennaDownTimeManager.x: 400

*label156.class: label
*label156.static: true
*label156.name: label156
*label156.parent: AntennaDownTimeManager
*label156.isCompound: "true"
*label156.compoundIcon: "label.xpm"
*label156.compoundName: "label_"
*label156.x: 232
*label156.y: 14
*label156.width: 268
*label156.height: 35
*label156.labelString: "ANTENNA  DOWN  TIMES"
*label156.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindowList5.class: scrolledWindow
*scrolledWindowList5.static: true
*scrolledWindowList5.name: scrolledWindowList5
*scrolledWindowList5.parent: AntennaDownTimeManager
*scrolledWindowList5.scrollingPolicy: "application_defined"
*scrolledWindowList5.visualPolicy: "variable"
*scrolledWindowList5.scrollBarDisplayPolicy: "static"
*scrolledWindowList5.shadowThickness: 0
*scrolledWindowList5.isCompound: "true"
*scrolledWindowList5.compoundIcon: "scrllist.xpm"
*scrolledWindowList5.compoundName: "scrolled_List"
*scrolledWindowList5.x: 60
*scrolledWindowList5.y: 90
*scrolledWindowList5.width: 645

*scrolledList_DownTimes.class: scrolledList
*scrolledList_DownTimes.static: true
*scrolledList_DownTimes.name: scrolledList_DownTimes
*scrolledList_DownTimes.parent: scrolledWindowList5
*scrolledList_DownTimes.width: 640
*scrolledList_DownTimes.height: 150
*scrolledList_DownTimes.itemCount: 1
*scrolledList_DownTimes.items: "ASF      1       1994:266:11:22:33    1994:109:11:22:33"
*scrolledList_DownTimes.browseSelectionCallback.source: public
*scrolledList_DownTimes.browseSelectionCallback: cb_update_antdntime_form
*scrolledList_DownTimes.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_DownTimes.selectionPolicy: "browse_select"
*scrolledList_DownTimes.scrollBarDisplayPolicy: "static"
*scrolledList_DownTimes.listSizePolicy: "constant"
*scrolledList_DownTimes.visibleItemCount: 9
*scrolledList_DownTimes.x: 0
*scrolledList_DownTimes.y: 90

*pushButton_QuitDownTime.class: pushButton
*pushButton_QuitDownTime.static: true
*pushButton_QuitDownTime.name: pushButton_QuitDownTime
*pushButton_QuitDownTime.parent: AntennaDownTimeManager
*pushButton_QuitDownTime.isCompound: "true"
*pushButton_QuitDownTime.compoundIcon: "push.xpm"
*pushButton_QuitDownTime.compoundName: "push_Button"
*pushButton_QuitDownTime.x: 617
*pushButton_QuitDownTime.y: 573
*pushButton_QuitDownTime.width: 90
*pushButton_QuitDownTime.height: 40
*pushButton_QuitDownTime.labelString: "QUIT"
*pushButton_QuitDownTime.fontList: "rockwell-bold"
*pushButton_QuitDownTime.activateCallback: {\
XtPopdown(XtParent(AntennaDownTime_manager)) ;\
}

*pushButton_SortDownTime.class: pushButton
*pushButton_SortDownTime.static: true
*pushButton_SortDownTime.name: pushButton_SortDownTime
*pushButton_SortDownTime.parent: AntennaDownTimeManager
*pushButton_SortDownTime.isCompound: "true"
*pushButton_SortDownTime.compoundIcon: "push.xpm"
*pushButton_SortDownTime.compoundName: "push_Button"
*pushButton_SortDownTime.x: 326
*pushButton_SortDownTime.labelString: "SORT BY"
*pushButton_SortDownTime.fontList: "rockwell-bold"
*pushButton_SortDownTime.y: 275
*pushButton_SortDownTime.height: 38
*pushButton_SortDownTime.width: 80

*label151.class: label
*label151.static: true
*label151.name: label151
*label151.parent: AntennaDownTimeManager
*label151.isCompound: "true"
*label151.compoundIcon: "label.xpm"
*label151.compoundName: "label_"
*label151.x: 57
*label151.y: 59
*label151.labelString: " STN   ANTENNA\n ID      ID          START TIME           STOP TIME"
*label151.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label151.alignment: "alignment_beginning"
*label151.recomputeSize: "true"

*separator9.class: separator
*separator9.static: true
*separator9.name: separator9
*separator9.parent: AntennaDownTimeManager
*separator9.width: 730
*separator9.height: 10
*separator9.isCompound: "true"
*separator9.compoundIcon: "sep.xpm"
*separator9.compoundName: "separator_"
*separator9.x: 1
*separator9.y: 328

*label153.class: label
*label153.static: true
*label153.name: label153
*label153.parent: AntennaDownTimeManager
*label153.isCompound: "true"
*label153.compoundIcon: "label.xpm"
*label153.compoundName: "label_"
*label153.x: 36
*label153.y: 398
*label153.height: 30
*label153.labelString: "START TIME:"
*label153.alignment: "alignment_end"

*label154.class: label
*label154.static: true
*label154.name: label154
*label154.parent: AntennaDownTimeManager
*label154.isCompound: "true"
*label154.compoundIcon: "label.xpm"
*label154.compoundName: "label_"
*label154.x: 42
*label154.y: 453
*label154.height: 30
*label154.labelString: "STOP TIME:"
*label154.alignment: "alignment_end"

*pushButton_EditDownTime.class: pushButton
*pushButton_EditDownTime.static: true
*pushButton_EditDownTime.name: pushButton_EditDownTime
*pushButton_EditDownTime.parent: AntennaDownTimeManager
*pushButton_EditDownTime.isCompound: "true"
*pushButton_EditDownTime.compoundIcon: "push.xpm"
*pushButton_EditDownTime.compoundName: "push_Button"
*pushButton_EditDownTime.x: 27
*pushButton_EditDownTime.y: 573
*pushButton_EditDownTime.width: 90
*pushButton_EditDownTime.height: 40
*pushButton_EditDownTime.labelString: "EDIT"
*pushButton_EditDownTime.fontList: "rockwell-bold"
*pushButton_EditDownTime.sensitive: "false"
*pushButton_EditDownTime.activateCallback.source: public
*pushButton_EditDownTime.activateCallback: cb_set_antdntimes_editability
*pushButton_EditDownTime.activateCallbackClientData: (XtPointer) True

*pushButton_SearchDownTime.class: pushButton
*pushButton_SearchDownTime.static: true
*pushButton_SearchDownTime.name: pushButton_SearchDownTime
*pushButton_SearchDownTime.parent: AntennaDownTimeManager
*pushButton_SearchDownTime.isCompound: "true"
*pushButton_SearchDownTime.compoundIcon: "push.xpm"
*pushButton_SearchDownTime.compoundName: "push_Button"
*pushButton_SearchDownTime.x: 56
*pushButton_SearchDownTime.y: 275
*pushButton_SearchDownTime.labelString: "SEARCH"
*pushButton_SearchDownTime.fontList: "rockwell-bold"
*pushButton_SearchDownTime.height: 38
*pushButton_SearchDownTime.width: 80

*pushButton_DeleteDownTime.class: pushButton
*pushButton_DeleteDownTime.static: true
*pushButton_DeleteDownTime.name: pushButton_DeleteDownTime
*pushButton_DeleteDownTime.parent: AntennaDownTimeManager
*pushButton_DeleteDownTime.isCompound: "true"
*pushButton_DeleteDownTime.compoundIcon: "push.xpm"
*pushButton_DeleteDownTime.compoundName: "push_Button"
*pushButton_DeleteDownTime.x: 142
*pushButton_DeleteDownTime.y: 573
*pushButton_DeleteDownTime.width: 90
*pushButton_DeleteDownTime.height: 40
*pushButton_DeleteDownTime.labelString: "DELETE\nDOWN TIME"
*pushButton_DeleteDownTime.fontList: "rockwell-bold"
*pushButton_DeleteDownTime.sensitive: "false"
*pushButton_DeleteDownTime.activateCallback.source: public
*pushButton_DeleteDownTime.activateCallback: cb_delete_antdntime_record

*TF_DownTime_sortclause.class: textField
*TF_DownTime_sortclause.static: true
*TF_DownTime_sortclause.name: TF_DownTime_sortclause
*TF_DownTime_sortclause.parent: AntennaDownTimeManager
*TF_DownTime_sortclause.isCompound: "true"
*TF_DownTime_sortclause.compoundIcon: "textfield.xpm"
*TF_DownTime_sortclause.compoundName: "text_Field"
*TF_DownTime_sortclause.x: 406
*TF_DownTime_sortclause.y: 278
*TF_DownTime_sortclause.height: 32
*TF_DownTime_sortclause.cursorPositionVisible: "false"
*TF_DownTime_sortclause.editable: "false"
*TF_DownTime_sortclause.sensitive: "false"
*TF_DownTime_sortclause.text: "station_id asc, antenna_id asc, strttime asc"
*TF_DownTime_sortclause.columns: 256
*TF_DownTime_sortclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_sortclause.resizeWidth: "false"
*TF_DownTime_sortclause.width: 180
*TF_DownTime_sortclause.valueChangedCallback.source: public
*TF_DownTime_sortclause.valueChangedCallback: cb_show_antdntime_records
*TF_DownTime_sortclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DownTimes
*TF_DownTime_sortclause.traversalOn: "false"
*TF_DownTime_sortclause.valueWcs: "station_id asc, antenna_id asc, strttime asc"

*TF_DownTime_recordcount.class: textField
*TF_DownTime_recordcount.static: true
*TF_DownTime_recordcount.name: TF_DownTime_recordcount
*TF_DownTime_recordcount.parent: AntennaDownTimeManager
*TF_DownTime_recordcount.isCompound: "true"
*TF_DownTime_recordcount.compoundIcon: "textfield.xpm"
*TF_DownTime_recordcount.compoundName: "text_Field"
*TF_DownTime_recordcount.x: 631
*TF_DownTime_recordcount.y: 279
*TF_DownTime_recordcount.height: 32
*TF_DownTime_recordcount.columns: 5
*TF_DownTime_recordcount.cursorPositionVisible: "false"
*TF_DownTime_recordcount.editable: "false"
*TF_DownTime_recordcount.sensitive: "false"
*TF_DownTime_recordcount.resizeWidth: "false"
*TF_DownTime_recordcount.text: "1"
*TF_DownTime_recordcount.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_recordcount.traversalOn: "false"

*label152.class: label
*label152.static: true
*label152.name: label152
*label152.parent: AntennaDownTimeManager
*label152.isCompound: "true"
*label152.compoundIcon: "label.xpm"
*label152.compoundName: "label_"
*label152.x: 591
*label152.y: 279
*label152.height: 30
*label152.labelString: "RECORD\nCOUNT:"
*label152.alignment: "alignment_end"

*pushButton_SaveDownTimeChanges.class: pushButton
*pushButton_SaveDownTimeChanges.static: true
*pushButton_SaveDownTimeChanges.name: pushButton_SaveDownTimeChanges
*pushButton_SaveDownTimeChanges.parent: AntennaDownTimeManager
*pushButton_SaveDownTimeChanges.isCompound: "true"
*pushButton_SaveDownTimeChanges.compoundIcon: "push.xpm"
*pushButton_SaveDownTimeChanges.compoundName: "push_Button"
*pushButton_SaveDownTimeChanges.x: 387
*pushButton_SaveDownTimeChanges.y: 573
*pushButton_SaveDownTimeChanges.width: 90
*pushButton_SaveDownTimeChanges.height: 40
*pushButton_SaveDownTimeChanges.labelString: "SAVE\nCHANGES"
*pushButton_SaveDownTimeChanges.fontList: "rockwell-bold"
*pushButton_SaveDownTimeChanges.sensitive: "true"
*pushButton_SaveDownTimeChanges.activateCallback.source: public
*pushButton_SaveDownTimeChanges.activateCallback: cb_save_antdntime_changes
*pushButton_SaveDownTimeChanges.activateCallbackClientData: (XtPointer) False
*pushButton_SaveDownTimeChanges.createManaged: "false"

*pushButton_CancelDownTimeChanges.class: pushButton
*pushButton_CancelDownTimeChanges.static: true
*pushButton_CancelDownTimeChanges.name: pushButton_CancelDownTimeChanges
*pushButton_CancelDownTimeChanges.parent: AntennaDownTimeManager
*pushButton_CancelDownTimeChanges.isCompound: "true"
*pushButton_CancelDownTimeChanges.compoundIcon: "push.xpm"
*pushButton_CancelDownTimeChanges.compoundName: "push_Button"
*pushButton_CancelDownTimeChanges.x: 507
*pushButton_CancelDownTimeChanges.y: 573
*pushButton_CancelDownTimeChanges.width: 90
*pushButton_CancelDownTimeChanges.height: 40
*pushButton_CancelDownTimeChanges.labelString: "CANCEL\nCHANGES"
*pushButton_CancelDownTimeChanges.fontList: "rockwell-bold"
*pushButton_CancelDownTimeChanges.sensitive: "true"
*pushButton_CancelDownTimeChanges.activateCallback.source: public
*pushButton_CancelDownTimeChanges.activateCallback: cb_set_antdntimes_editability
*pushButton_CancelDownTimeChanges.activateCallbackClientData: (XtPointer) False
*pushButton_CancelDownTimeChanges.createManaged: "false"

*TF_DownTime_searchclause.class: textField
*TF_DownTime_searchclause.static: true
*TF_DownTime_searchclause.name: TF_DownTime_searchclause
*TF_DownTime_searchclause.parent: AntennaDownTimeManager
*TF_DownTime_searchclause.isCompound: "true"
*TF_DownTime_searchclause.compoundIcon: "textfield.xpm"
*TF_DownTime_searchclause.compoundName: "text_Field"
*TF_DownTime_searchclause.x: 136
*TF_DownTime_searchclause.y: 278
*TF_DownTime_searchclause.height: 32
*TF_DownTime_searchclause.cursorPositionVisible: "false"
*TF_DownTime_searchclause.editable: "false"
*TF_DownTime_searchclause.sensitive: "false"
*TF_DownTime_searchclause.text: "where strtime > today"
*TF_DownTime_searchclause.columns: 256
*TF_DownTime_searchclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_searchclause.resizeWidth: "false"
*TF_DownTime_searchclause.width: 180
*TF_DownTime_searchclause.valueChangedCallback.source: public
*TF_DownTime_searchclause.valueChangedCallback: cb_show_antdntime_records
*TF_DownTime_searchclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DownTimes
*TF_DownTime_searchclause.traversalOn: "false"
*TF_DownTime_searchclause.valueWcs: "where strttime > ' '"

*pushButton_ANTDNTIME_refresh.class: pushButton
*pushButton_ANTDNTIME_refresh.static: true
*pushButton_ANTDNTIME_refresh.name: pushButton_ANTDNTIME_refresh
*pushButton_ANTDNTIME_refresh.parent: AntennaDownTimeManager
*pushButton_ANTDNTIME_refresh.x: 22
*pushButton_ANTDNTIME_refresh.y: 100
*pushButton_ANTDNTIME_refresh.width: 30
*pushButton_ANTDNTIME_refresh.height: 130
*pushButton_ANTDNTIME_refresh.fontList: "rockwell-bold"
*pushButton_ANTDNTIME_refresh.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton_ANTDNTIME_refresh.activateCallback.source: public
*pushButton_ANTDNTIME_refresh.activateCallback: cb_show_antdntime_records
*pushButton_ANTDNTIME_refresh.activateCallbackClientData: (XtPointer) scrolledList_DownTimes

*pushButton_CreateDownTime.class: pushButton
*pushButton_CreateDownTime.static: true
*pushButton_CreateDownTime.name: pushButton_CreateDownTime
*pushButton_CreateDownTime.parent: AntennaDownTimeManager
*pushButton_CreateDownTime.isCompound: "true"
*pushButton_CreateDownTime.compoundIcon: "push.xpm"
*pushButton_CreateDownTime.compoundName: "push_Button"
*pushButton_CreateDownTime.x: 252
*pushButton_CreateDownTime.y: 573
*pushButton_CreateDownTime.width: 90
*pushButton_CreateDownTime.height: 40
*pushButton_CreateDownTime.labelString: "CREATE"
*pushButton_CreateDownTime.fontList: "rockwell-bold"
*pushButton_CreateDownTime.sensitive: "true"
*pushButton_CreateDownTime.activateCallback.source: public
*pushButton_CreateDownTime.activateCallback: cb_create_new_antdntime_record

*label155.class: label
*label155.static: true
*label155.name: label155
*label155.parent: AntennaDownTimeManager
*label155.isCompound: "true"
*label155.compoundIcon: "label.xpm"
*label155.compoundName: "label_"
*label155.x: 36
*label155.y: 508
*label155.height: 30
*label155.labelString: "TOTAL DAYS:"
*label155.alignment: "alignment_end"

*TF_ANT_DN_TIMES_STRTTIME.class: textField
*TF_ANT_DN_TIMES_STRTTIME.static: true
*TF_ANT_DN_TIMES_STRTTIME.name: TF_ANT_DN_TIMES_STRTTIME
*TF_ANT_DN_TIMES_STRTTIME.parent: AntennaDownTimeManager
*TF_ANT_DN_TIMES_STRTTIME.isCompound: "true"
*TF_ANT_DN_TIMES_STRTTIME.compoundIcon: "textfield.xpm"
*TF_ANT_DN_TIMES_STRTTIME.compoundName: "text_Field"
*TF_ANT_DN_TIMES_STRTTIME.x: 111
*TF_ANT_DN_TIMES_STRTTIME.y: 397
*TF_ANT_DN_TIMES_STRTTIME.height: 32
*TF_ANT_DN_TIMES_STRTTIME.columns: 21
*TF_ANT_DN_TIMES_STRTTIME.cursorPositionVisible: "false"
*TF_ANT_DN_TIMES_STRTTIME.editable: "false"
*TF_ANT_DN_TIMES_STRTTIME.sensitive: "false"
*TF_ANT_DN_TIMES_STRTTIME.resizeWidth: "false"
*TF_ANT_DN_TIMES_STRTTIME.text: ""
*TF_ANT_DN_TIMES_STRTTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_ANT_DN_TIMES_STRTTIME.maxLength: 21
*TF_ANT_DN_TIMES_STRTTIME.activateCallback.source: public
*TF_ANT_DN_TIMES_STRTTIME.activateCallback: cb_validate_ASF_datetime
*TF_ANT_DN_TIMES_STRTTIME.activateCallbackClientData: (XtPointer) "Antenna Down Start Time"
*TF_ANT_DN_TIMES_STRTTIME.focusCallback.source: public
*TF_ANT_DN_TIMES_STRTTIME.focusCallback: cb_toggle_cursor
*TF_ANT_DN_TIMES_STRTTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_ANT_DN_TIMES_STRTTIME.losingFocusCallback.source: public
*TF_ANT_DN_TIMES_STRTTIME.losingFocusCallback: cb_toggle_cursor
*TF_ANT_DN_TIMES_STRTTIME.losingFocusCallbackClientData: (XtPointer) FALSE

*TF_ANT_DN_TIMES_STOPTIME.class: textField
*TF_ANT_DN_TIMES_STOPTIME.static: true
*TF_ANT_DN_TIMES_STOPTIME.name: TF_ANT_DN_TIMES_STOPTIME
*TF_ANT_DN_TIMES_STOPTIME.parent: AntennaDownTimeManager
*TF_ANT_DN_TIMES_STOPTIME.isCompound: "true"
*TF_ANT_DN_TIMES_STOPTIME.compoundIcon: "textfield.xpm"
*TF_ANT_DN_TIMES_STOPTIME.compoundName: "text_Field"
*TF_ANT_DN_TIMES_STOPTIME.x: 111
*TF_ANT_DN_TIMES_STOPTIME.y: 452
*TF_ANT_DN_TIMES_STOPTIME.height: 32
*TF_ANT_DN_TIMES_STOPTIME.columns: 21
*TF_ANT_DN_TIMES_STOPTIME.cursorPositionVisible: "false"
*TF_ANT_DN_TIMES_STOPTIME.editable: "false"
*TF_ANT_DN_TIMES_STOPTIME.sensitive: "false"
*TF_ANT_DN_TIMES_STOPTIME.resizeWidth: "false"
*TF_ANT_DN_TIMES_STOPTIME.text: ""
*TF_ANT_DN_TIMES_STOPTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_ANT_DN_TIMES_STOPTIME.maxLength: 21
*TF_ANT_DN_TIMES_STOPTIME.activateCallback.source: public
*TF_ANT_DN_TIMES_STOPTIME.activateCallback: cb_validate_ASF_datetime
*TF_ANT_DN_TIMES_STOPTIME.activateCallbackClientData: (XtPointer) "Antenna Down Stop Time"
*TF_ANT_DN_TIMES_STOPTIME.focusCallback.source: public
*TF_ANT_DN_TIMES_STOPTIME.focusCallback: cb_toggle_cursor
*TF_ANT_DN_TIMES_STOPTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_ANT_DN_TIMES_STOPTIME.losingFocusCallback.source: public
*TF_ANT_DN_TIMES_STOPTIME.losingFocusCallback: cb_toggle_cursor
*TF_ANT_DN_TIMES_STOPTIME.losingFocusCallbackClientData: (XtPointer) FALSE

*TF_DownTime_total_days.class: textField
*TF_DownTime_total_days.static: true
*TF_DownTime_total_days.name: TF_DownTime_total_days
*TF_DownTime_total_days.parent: AntennaDownTimeManager
*TF_DownTime_total_days.x: 111
*TF_DownTime_total_days.y: 507
*TF_DownTime_total_days.height: 32
*TF_DownTime_total_days.columns: 8
*TF_DownTime_total_days.resizeWidth: "false"
*TF_DownTime_total_days.text: ""
*TF_DownTime_total_days.cursorPositionVisible: "false"
*TF_DownTime_total_days.editable: "false"
*TF_DownTime_total_days.sensitive: "false"
*TF_DownTime_total_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_total_days.maxLength: 8
*TF_DownTime_total_days.width: 83
*TF_DownTime_total_days.modifyVerifyCallback.source: public
*TF_DownTime_total_days.modifyVerifyCallback: cb_filter_text
*TF_DownTime_total_days.modifyVerifyCallbackClientData: (XtPointer) valid_float_chars
*TF_DownTime_total_days.focusCallback.source: public
*TF_DownTime_total_days.focusCallback: cb_toggle_cursor
*TF_DownTime_total_days.focusCallbackClientData: (XtPointer) TRUE
*TF_DownTime_total_days.losingFocusCallback.source: public
*TF_DownTime_total_days.losingFocusCallback: cb_toggle_cursor
*TF_DownTime_total_days.losingFocusCallbackClientData: (XtPointer) FALSE

*label122.class: label
*label122.static: true
*label122.name: label122
*label122.parent: AntennaDownTimeManager
*label122.x: 111
*label122.y: 428
*label122.width: 185
*label122.height: 15
*label122.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label122.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*label123.class: label
*label123.static: true
*label123.name: label123
*label123.parent: AntennaDownTimeManager
*label123.x: 111
*label123.y: 483
*label123.width: 185
*label123.height: 15
*label123.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label123.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*T_ANT_DN_TIMES_COMMENTS.class: text
*T_ANT_DN_TIMES_COMMENTS.static: true
*T_ANT_DN_TIMES_COMMENTS.name: T_ANT_DN_TIMES_COMMENTS
*T_ANT_DN_TIMES_COMMENTS.parent: AntennaDownTimeManager
*T_ANT_DN_TIMES_COMMENTS.isCompound: "true"
*T_ANT_DN_TIMES_COMMENTS.compoundIcon: "text.xpm"
*T_ANT_DN_TIMES_COMMENTS.compoundName: "text_"
*T_ANT_DN_TIMES_COMMENTS.x: 391
*T_ANT_DN_TIMES_COMMENTS.y: 399
*T_ANT_DN_TIMES_COMMENTS.editMode: "multi_line_edit"
*T_ANT_DN_TIMES_COMMENTS.maxLength: 60
*T_ANT_DN_TIMES_COMMENTS.rows: 3
*T_ANT_DN_TIMES_COMMENTS.resizeHeight: "false"
*T_ANT_DN_TIMES_COMMENTS.resizeWidth: "false"
*T_ANT_DN_TIMES_COMMENTS.columns: 30
*T_ANT_DN_TIMES_COMMENTS.wordWrap: "true"
*T_ANT_DN_TIMES_COMMENTS.cursorPositionVisible: "false"
*T_ANT_DN_TIMES_COMMENTS.editable: "false"
*T_ANT_DN_TIMES_COMMENTS.sensitive: "false"
*T_ANT_DN_TIMES_COMMENTS.focusCallback.source: public
*T_ANT_DN_TIMES_COMMENTS.focusCallback: cb_toggle_cursor
*T_ANT_DN_TIMES_COMMENTS.focusCallbackClientData: (XtPointer) True
*T_ANT_DN_TIMES_COMMENTS.losingFocusCallback.source: public
*T_ANT_DN_TIMES_COMMENTS.losingFocusCallback: cb_toggle_cursor
*T_ANT_DN_TIMES_COMMENTS.losingFocusCallbackClientData: (XtPointer) False
*T_ANT_DN_TIMES_COMMENTS.height: 79
*T_ANT_DN_TIMES_COMMENTS.width: 207

*label124.class: label
*label124.static: true
*label124.name: label124
*label124.parent: AntennaDownTimeManager
*label124.isCompound: "true"
*label124.compoundIcon: "label.xpm"
*label124.compoundName: "label_"
*label124.x: 329
*label124.y: 398
*label124.height: 30
*label124.labelString: "COMMENTS:"
*label124.alignment: "alignment_end"
*label124.width: 62

*optionMenu_station_id.class: rowColumn
*optionMenu_station_id.static: true
*optionMenu_station_id.name: optionMenu_station_id
*optionMenu_station_id.parent: AntennaDownTimeManager
*optionMenu_station_id.rowColumnType: "menu_option"
*optionMenu_station_id.subMenuId: "subMenu_antenna_down_stn_id"
*optionMenu_station_id.isCompound: "true"
*optionMenu_station_id.compoundIcon: "optionM.xpm"
*optionMenu_station_id.compoundName: "option_Menu"
*optionMenu_station_id.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_station_id.x: 40
*optionMenu_station_id.y: 348
*optionMenu_station_id.width: 215
*optionMenu_station_id.height: 35
*optionMenu_station_id.labelString: "STATION ID\n/ANTENNA:"
*optionMenu_station_id.sensitive: "false"

*subMenu_antenna_down_stn_id.class: rowColumn
*subMenu_antenna_down_stn_id.static: true
*subMenu_antenna_down_stn_id.name: subMenu_antenna_down_stn_id
*subMenu_antenna_down_stn_id.parent: optionMenu_station_id
*subMenu_antenna_down_stn_id.rowColumnType: "menu_pulldown"
*subMenu_antenna_down_stn_id.labelString: ""
*subMenu_antenna_down_stn_id.height: 48
*subMenu_antenna_down_stn_id.resizeHeight: "false"
*subMenu_antenna_down_stn_id.x: 0
*subMenu_antenna_down_stn_id.y: 349
*subMenu_antenna_down_stn_id.sensitive: "true"
*subMenu_antenna_down_stn_id.mappedWhenManaged: "true"

*subMenu_antenna_down_stn_id_pb.class: pushButton
*subMenu_antenna_down_stn_id_pb.static: true
*subMenu_antenna_down_stn_id_pb.name: subMenu_antenna_down_stn_id_pb
*subMenu_antenna_down_stn_id_pb.parent: subMenu_antenna_down_stn_id
*subMenu_antenna_down_stn_id_pb.labelString: "ASF"
*subMenu_antenna_down_stn_id_pb.fontList: "rockwell-bold"
*subMenu_antenna_down_stn_id_pb.x: 2
*subMenu_antenna_down_stn_id_pb.y: 362
*subMenu_antenna_down_stn_id_pb.createCallback.source: public
*subMenu_antenna_down_stn_id_pb.createCallback: cb_build_station_option_menu

*optionMenu_antenna.class: rowColumn
*optionMenu_antenna.static: true
*optionMenu_antenna.name: optionMenu_antenna
*optionMenu_antenna.parent: AntennaDownTimeManager
*optionMenu_antenna.rowColumnType: "menu_option"
*optionMenu_antenna.subMenuId: "subMenu_antenna_down_antenna"
*optionMenu_antenna.isCompound: "true"
*optionMenu_antenna.compoundIcon: "optionM.xpm"
*optionMenu_antenna.compoundName: "option_Menu"
*optionMenu_antenna.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_antenna.x: 166
*optionMenu_antenna.y: 347
*optionMenu_antenna.width: 215
*optionMenu_antenna.height: 35
*optionMenu_antenna.labelString: "/"
*optionMenu_antenna.sensitive: "false"

*subMenu_antenna_down_antenna.class: rowColumn
*subMenu_antenna_down_antenna.static: true
*subMenu_antenna_down_antenna.name: subMenu_antenna_down_antenna
*subMenu_antenna_down_antenna.parent: optionMenu_antenna
*subMenu_antenna_down_antenna.rowColumnType: "menu_pulldown"
*subMenu_antenna_down_antenna.labelString: ""
*subMenu_antenna_down_antenna.sensitive: "true"
*subMenu_antenna_down_antenna.x: 0
*subMenu_antenna_down_antenna.y: 360
*subMenu_antenna_down_antenna.mappedWhenManaged: "true"

*subMenu_antenna_down_antenna_pb.class: pushButton
*subMenu_antenna_down_antenna_pb.static: true
*subMenu_antenna_down_antenna_pb.name: subMenu_antenna_down_antenna_pb
*subMenu_antenna_down_antenna_pb.parent: subMenu_antenna_down_antenna
*subMenu_antenna_down_antenna_pb.labelString: "1"
*subMenu_antenna_down_antenna_pb.fontList: "rockwell-bold"
*subMenu_antenna_down_antenna_pb.x: 2
*subMenu_antenna_down_antenna_pb.y: 362
*subMenu_antenna_down_antenna_pb.createCallback.source: public
*subMenu_antenna_down_antenna_pb.createCallback: cb_build_antenna_option_menu
*subMenu_antenna_down_antenna_pb.createCallbackClientData: (XtPointer) False

