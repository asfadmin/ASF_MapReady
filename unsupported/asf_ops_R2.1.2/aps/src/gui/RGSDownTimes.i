! UIMX ascii 2.9 key: 5980                                                      

*RGSDownTimeManager.class: form
*RGSDownTimeManager.classinc:
*RGSDownTimeManager.classspec:
*RGSDownTimeManager.classmembers:
*RGSDownTimeManager.classconstructor:
*RGSDownTimeManager.classdestructor:
*RGSDownTimeManager.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)RGSDownTimes.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.RGSDownTimes.i"\
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
#include "cb_rgsdntimes.h"\
#include "cb_datetime.h"\
\
extern Widget DownTime_manager;\
extern Widget filebox ;\

*RGSDownTimeManager.ispecdecl:
*RGSDownTimeManager.funcdecl: swidget create_DownTimeManager(swidget UxParent)
*RGSDownTimeManager.funcname: create_DownTimeManager
*RGSDownTimeManager.funcdef: "swidget", "<create_DownTimeManager>(%)"
*RGSDownTimeManager.argdecl: swidget UxParent;
*RGSDownTimeManager.arglist: UxParent
*RGSDownTimeManager.arglist.UxParent: "swidget", "%UxParent%"
*RGSDownTimeManager.icode: SORT_INFO *sortinfo ;\
SEARCH_INFO *searchinfo ;\
PERIOD_WIDGETS *down_times ;
*RGSDownTimeManager.fcode: sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;\
searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;\
\
sortinfo->table_name = APS_TABLE(RGS_DOWN_TIMES) ;\
sortinfo->field_to_update = (Widget) TF_DownTime_sortclause ;\
\
searchinfo->table_name = APS_TABLE(RGS_DOWN_TIMES) ;\
searchinfo->field_to_update  = (Widget) TF_DownTime_searchclause ;\
\
down_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
down_times->start = (Widget) TF_ASF_DN_TIMES_STRTTIME ;\
down_times->stop = (Widget) TF_ASF_DN_TIMES_STOPTIME ;\
\
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
	cb_show_asfdntime_records, (XtPointer *) scrolledList_DownTimes) ;\
\
return(rtrn);\

*RGSDownTimeManager.auxdecl:
*RGSDownTimeManager.static: true
*RGSDownTimeManager.name: RGSDownTimeManager
*RGSDownTimeManager.parent: NO_PARENT
*RGSDownTimeManager.parentExpression: UxParent
*RGSDownTimeManager.defaultShell: topLevelShell
*RGSDownTimeManager.height: 679
*RGSDownTimeManager.resizePolicy: "resize_none"
*RGSDownTimeManager.isCompound: "true"
*RGSDownTimeManager.compoundIcon: "form.xpm"
*RGSDownTimeManager.compoundName: "form_"
*RGSDownTimeManager.unitType: "pixels"
*RGSDownTimeManager.dialogTitle: "APS:RGS Down Time"
*RGSDownTimeManager.width: 741
*RGSDownTimeManager.y: -12
*RGSDownTimeManager.x: 395

*label156.class: label
*label156.static: true
*label156.name: label156
*label156.parent: RGSDownTimeManager
*label156.isCompound: "true"
*label156.compoundIcon: "label.xpm"
*label156.compoundName: "label_"
*label156.x: 268
*label156.y: 15
*label156.width: 208
*label156.height: 35
*label156.labelString: "RGS  DOWN  TIMES"
*label156.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindowList5.class: scrolledWindow
*scrolledWindowList5.static: true
*scrolledWindowList5.name: scrolledWindowList5
*scrolledWindowList5.parent: RGSDownTimeManager
*scrolledWindowList5.scrollingPolicy: "application_defined"
*scrolledWindowList5.visualPolicy: "variable"
*scrolledWindowList5.scrollBarDisplayPolicy: "static"
*scrolledWindowList5.shadowThickness: 0
*scrolledWindowList5.isCompound: "true"
*scrolledWindowList5.compoundIcon: "scrllist.xpm"
*scrolledWindowList5.compoundName: "scrolled_List"
*scrolledWindowList5.x: 65
*scrolledWindowList5.y: 90
*scrolledWindowList5.width: 645
*scrolledWindowList5.height: 145
*scrolledWindowList5.resizable: "false"

*scrolledList_DownTimes.class: scrolledList
*scrolledList_DownTimes.static: true
*scrolledList_DownTimes.name: scrolledList_DownTimes
*scrolledList_DownTimes.parent: scrolledWindowList5
*scrolledList_DownTimes.width: 640
*scrolledList_DownTimes.height: 160
*scrolledList_DownTimes.itemCount: 1
*scrolledList_DownTimes.items: "ASF 1994:266:11:22:33  1994:109:11:22:33  UNPLANNED MAINTENANCE   CANCEL  Y"
*scrolledList_DownTimes.browseSelectionCallback.source: public
*scrolledList_DownTimes.browseSelectionCallback: cb_update_asfdntime_form
*scrolledList_DownTimes.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_DownTimes.selectionPolicy: "browse_select"
*scrolledList_DownTimes.scrollBarDisplayPolicy: "static"
*scrolledList_DownTimes.visibleItemCount: 8

*pushButton_QuitDownTime.class: pushButton
*pushButton_QuitDownTime.static: true
*pushButton_QuitDownTime.name: pushButton_QuitDownTime
*pushButton_QuitDownTime.parent: RGSDownTimeManager
*pushButton_QuitDownTime.isCompound: "true"
*pushButton_QuitDownTime.compoundIcon: "push.xpm"
*pushButton_QuitDownTime.compoundName: "push_Button"
*pushButton_QuitDownTime.x: 615
*pushButton_QuitDownTime.y: 605
*pushButton_QuitDownTime.width: 90
*pushButton_QuitDownTime.height: 40
*pushButton_QuitDownTime.labelString: "QUIT"
*pushButton_QuitDownTime.fontList: "rockwell-bold"
*pushButton_QuitDownTime.activateCallback.source: public
*pushButton_QuitDownTime.activateCallback: cb_popdown_asfdntime_form

*pushButton_SortDownTime.class: pushButton
*pushButton_SortDownTime.static: true
*pushButton_SortDownTime.name: pushButton_SortDownTime
*pushButton_SortDownTime.parent: RGSDownTimeManager
*pushButton_SortDownTime.isCompound: "true"
*pushButton_SortDownTime.compoundIcon: "push.xpm"
*pushButton_SortDownTime.compoundName: "push_Button"
*pushButton_SortDownTime.x: 331
*pushButton_SortDownTime.labelString: "SORT BY"
*pushButton_SortDownTime.fontList: "rockwell-bold"
*pushButton_SortDownTime.y: 255
*pushButton_SortDownTime.height: 38
*pushButton_SortDownTime.width: 80

*label151.class: label
*label151.static: true
*label151.name: label151
*label151.parent: RGSDownTimeManager
*label151.isCompound: "true"
*label151.compoundIcon: "label.xpm"
*label151.compoundName: "label_"
*label151.x: 55
*label151.y: 60
*label151.labelString: " STN                                                                         FA\n ID   START TIME         STOP TIME          TYPE      REASON       STATUS  REPORT"
*label151.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label151.alignment: "alignment_beginning"
*label151.recomputeSize: "true"
*label151.height: 30

*separator9.class: separator
*separator9.static: true
*separator9.name: separator9
*separator9.parent: RGSDownTimeManager
*separator9.width: 739
*separator9.height: 9
*separator9.isCompound: "true"
*separator9.compoundIcon: "sep.xpm"
*separator9.compoundName: "separator_"
*separator9.x: 1
*separator9.y: 310

*label153.class: label
*label153.static: true
*label153.name: label153
*label153.parent: RGSDownTimeManager
*label153.isCompound: "true"
*label153.compoundIcon: "label.xpm"
*label153.compoundName: "label_"
*label153.x: 35
*label153.y: 405
*label153.height: 30
*label153.labelString: "START TIME:"
*label153.alignment: "alignment_end"

*label154.class: label
*label154.static: true
*label154.name: label154
*label154.parent: RGSDownTimeManager
*label154.isCompound: "true"
*label154.compoundIcon: "label.xpm"
*label154.compoundName: "label_"
*label154.x: 41
*label154.y: 460
*label154.height: 30
*label154.labelString: "STOP TIME:"
*label154.alignment: "alignment_end"

*pushButton_EditDownTime.class: pushButton
*pushButton_EditDownTime.static: true
*pushButton_EditDownTime.name: pushButton_EditDownTime
*pushButton_EditDownTime.parent: RGSDownTimeManager
*pushButton_EditDownTime.isCompound: "true"
*pushButton_EditDownTime.compoundIcon: "push.xpm"
*pushButton_EditDownTime.compoundName: "push_Button"
*pushButton_EditDownTime.x: 25
*pushButton_EditDownTime.y: 605
*pushButton_EditDownTime.width: 90
*pushButton_EditDownTime.height: 40
*pushButton_EditDownTime.labelString: "EDIT"
*pushButton_EditDownTime.fontList: "rockwell-bold"
*pushButton_EditDownTime.sensitive: "false"
*pushButton_EditDownTime.activateCallback.source: public
*pushButton_EditDownTime.activateCallback: cb_set_asfdntimes_editability
*pushButton_EditDownTime.activateCallbackClientData: (XtPointer) True

*pushButton_SearchDownTime.class: pushButton
*pushButton_SearchDownTime.static: true
*pushButton_SearchDownTime.name: pushButton_SearchDownTime
*pushButton_SearchDownTime.parent: RGSDownTimeManager
*pushButton_SearchDownTime.isCompound: "true"
*pushButton_SearchDownTime.compoundIcon: "push.xpm"
*pushButton_SearchDownTime.compoundName: "push_Button"
*pushButton_SearchDownTime.x: 61
*pushButton_SearchDownTime.y: 255
*pushButton_SearchDownTime.labelString: "SEARCH"
*pushButton_SearchDownTime.fontList: "rockwell-bold"
*pushButton_SearchDownTime.height: 38
*pushButton_SearchDownTime.width: 80

*pushButton_DeleteDownTime.class: pushButton
*pushButton_DeleteDownTime.static: true
*pushButton_DeleteDownTime.name: pushButton_DeleteDownTime
*pushButton_DeleteDownTime.parent: RGSDownTimeManager
*pushButton_DeleteDownTime.isCompound: "true"
*pushButton_DeleteDownTime.compoundIcon: "push.xpm"
*pushButton_DeleteDownTime.compoundName: "push_Button"
*pushButton_DeleteDownTime.x: 140
*pushButton_DeleteDownTime.y: 605
*pushButton_DeleteDownTime.width: 90
*pushButton_DeleteDownTime.height: 40
*pushButton_DeleteDownTime.labelString: "CANCEL\nDOWN TIME"
*pushButton_DeleteDownTime.fontList: "rockwell-bold"
*pushButton_DeleteDownTime.sensitive: "false"
*pushButton_DeleteDownTime.activateCallback.source: public
*pushButton_DeleteDownTime.activateCallback: cb_delete_asfdntime_record

*TF_DownTime_sortclause.class: textField
*TF_DownTime_sortclause.static: true
*TF_DownTime_sortclause.name: TF_DownTime_sortclause
*TF_DownTime_sortclause.parent: RGSDownTimeManager
*TF_DownTime_sortclause.isCompound: "true"
*TF_DownTime_sortclause.compoundIcon: "textfield.xpm"
*TF_DownTime_sortclause.compoundName: "text_Field"
*TF_DownTime_sortclause.x: 411
*TF_DownTime_sortclause.y: 258
*TF_DownTime_sortclause.height: 32
*TF_DownTime_sortclause.cursorPositionVisible: "false"
*TF_DownTime_sortclause.editable: "false"
*TF_DownTime_sortclause.text: "disposition desc, station_id asc, strttime asc"
*TF_DownTime_sortclause.columns: 256
*TF_DownTime_sortclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_sortclause.resizeWidth: "false"
*TF_DownTime_sortclause.width: 180
*TF_DownTime_sortclause.valueChangedCallback.source: public
*TF_DownTime_sortclause.valueChangedCallback: cb_show_asfdntime_records
*TF_DownTime_sortclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DownTimes
*TF_DownTime_sortclause.traversalOn: "false"
*TF_DownTime_sortclause.valueWcs: "disposition desc, station_id asc, strttime asc"

*TF_DownTime_recordcount.class: textField
*TF_DownTime_recordcount.static: true
*TF_DownTime_recordcount.name: TF_DownTime_recordcount
*TF_DownTime_recordcount.parent: RGSDownTimeManager
*TF_DownTime_recordcount.isCompound: "true"
*TF_DownTime_recordcount.compoundIcon: "textfield.xpm"
*TF_DownTime_recordcount.compoundName: "text_Field"
*TF_DownTime_recordcount.x: 636
*TF_DownTime_recordcount.y: 258
*TF_DownTime_recordcount.height: 32
*TF_DownTime_recordcount.columns: 5
*TF_DownTime_recordcount.cursorPositionVisible: "false"
*TF_DownTime_recordcount.editable: "false"
*TF_DownTime_recordcount.resizeWidth: "false"
*TF_DownTime_recordcount.text: "0"
*TF_DownTime_recordcount.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_recordcount.traversalOn: "false"

*label152.class: label
*label152.static: true
*label152.name: label152
*label152.parent: RGSDownTimeManager
*label152.isCompound: "true"
*label152.compoundIcon: "label.xpm"
*label152.compoundName: "label_"
*label152.x: 596
*label152.y: 259
*label152.height: 30
*label152.labelString: "RECORD\nCOUNT:"
*label152.alignment: "alignment_end"

*pushButton_SaveDownTimeChanges.class: pushButton
*pushButton_SaveDownTimeChanges.static: true
*pushButton_SaveDownTimeChanges.name: pushButton_SaveDownTimeChanges
*pushButton_SaveDownTimeChanges.parent: RGSDownTimeManager
*pushButton_SaveDownTimeChanges.isCompound: "true"
*pushButton_SaveDownTimeChanges.compoundIcon: "push.xpm"
*pushButton_SaveDownTimeChanges.compoundName: "push_Button"
*pushButton_SaveDownTimeChanges.x: 385
*pushButton_SaveDownTimeChanges.y: 605
*pushButton_SaveDownTimeChanges.width: 90
*pushButton_SaveDownTimeChanges.height: 40
*pushButton_SaveDownTimeChanges.labelString: "SAVE\nCHANGES"
*pushButton_SaveDownTimeChanges.fontList: "rockwell-bold"
*pushButton_SaveDownTimeChanges.sensitive: "true"
*pushButton_SaveDownTimeChanges.activateCallback.source: public
*pushButton_SaveDownTimeChanges.activateCallback: cb_save_asfdntime_changes
*pushButton_SaveDownTimeChanges.activateCallbackClientData: (XtPointer) False
*pushButton_SaveDownTimeChanges.createManaged: "false"

*pushButton_CancelDownTimeChanges.class: pushButton
*pushButton_CancelDownTimeChanges.static: true
*pushButton_CancelDownTimeChanges.name: pushButton_CancelDownTimeChanges
*pushButton_CancelDownTimeChanges.parent: RGSDownTimeManager
*pushButton_CancelDownTimeChanges.isCompound: "true"
*pushButton_CancelDownTimeChanges.compoundIcon: "push.xpm"
*pushButton_CancelDownTimeChanges.compoundName: "push_Button"
*pushButton_CancelDownTimeChanges.x: 505
*pushButton_CancelDownTimeChanges.y: 605
*pushButton_CancelDownTimeChanges.width: 90
*pushButton_CancelDownTimeChanges.height: 40
*pushButton_CancelDownTimeChanges.labelString: "CANCEL\nCHANGES"
*pushButton_CancelDownTimeChanges.fontList: "rockwell-bold"
*pushButton_CancelDownTimeChanges.sensitive: "true"
*pushButton_CancelDownTimeChanges.activateCallback.source: public
*pushButton_CancelDownTimeChanges.activateCallback: cb_set_asfdntimes_editability
*pushButton_CancelDownTimeChanges.activateCallbackClientData: (XtPointer) False
*pushButton_CancelDownTimeChanges.createManaged: "false"

*TF_DownTime_searchclause.class: textField
*TF_DownTime_searchclause.static: true
*TF_DownTime_searchclause.name: TF_DownTime_searchclause
*TF_DownTime_searchclause.parent: RGSDownTimeManager
*TF_DownTime_searchclause.isCompound: "true"
*TF_DownTime_searchclause.compoundIcon: "textfield.xpm"
*TF_DownTime_searchclause.compoundName: "text_Field"
*TF_DownTime_searchclause.x: 141
*TF_DownTime_searchclause.y: 258
*TF_DownTime_searchclause.height: 32
*TF_DownTime_searchclause.cursorPositionVisible: "false"
*TF_DownTime_searchclause.editable: "false"
*TF_DownTime_searchclause.text: "where strtime > today"
*TF_DownTime_searchclause.columns: 256
*TF_DownTime_searchclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DownTime_searchclause.resizeWidth: "false"
*TF_DownTime_searchclause.width: 180
*TF_DownTime_searchclause.valueChangedCallback.source: public
*TF_DownTime_searchclause.valueChangedCallback: cb_show_asfdntime_records
*TF_DownTime_searchclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DownTimes
*TF_DownTime_searchclause.traversalOn: "false"
*TF_DownTime_searchclause.valueWcs: "where strttime > ' '"

*pushButton_ASFDNTIME_refresh.class: pushButton
*pushButton_ASFDNTIME_refresh.static: true
*pushButton_ASFDNTIME_refresh.name: pushButton_ASFDNTIME_refresh
*pushButton_ASFDNTIME_refresh.parent: RGSDownTimeManager
*pushButton_ASFDNTIME_refresh.x: 20
*pushButton_ASFDNTIME_refresh.y: 100
*pushButton_ASFDNTIME_refresh.width: 30
*pushButton_ASFDNTIME_refresh.height: 130
*pushButton_ASFDNTIME_refresh.fontList: "rockwell-bold"
*pushButton_ASFDNTIME_refresh.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton_ASFDNTIME_refresh.activateCallback.source: public
*pushButton_ASFDNTIME_refresh.activateCallback: cb_show_asfdntime_records
*pushButton_ASFDNTIME_refresh.activateCallbackClientData: (XtPointer) scrolledList_DownTimes

*optionMenu_ASFdown_type.class: rowColumn
*optionMenu_ASFdown_type.static: true
*optionMenu_ASFdown_type.name: optionMenu_ASFdown_type
*optionMenu_ASFdown_type.parent: RGSDownTimeManager
*optionMenu_ASFdown_type.rowColumnType: "menu_option"
*optionMenu_ASFdown_type.subMenuId: "subMenu_ASFdown_type"
*optionMenu_ASFdown_type.isCompound: "true"
*optionMenu_ASFdown_type.compoundIcon: "optionM.xpm"
*optionMenu_ASFdown_type.compoundName: "option_Menu"
*optionMenu_ASFdown_type.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_ASFdown_type.x: 350
*optionMenu_ASFdown_type.y: 400
*optionMenu_ASFdown_type.width: 215
*optionMenu_ASFdown_type.height: 35
*optionMenu_ASFdown_type.labelString: "TYPE:"
*optionMenu_ASFdown_type.sensitive: "false"

*subMenu_ASFdown_type.class: rowColumn
*subMenu_ASFdown_type.static: true
*subMenu_ASFdown_type.name: subMenu_ASFdown_type
*subMenu_ASFdown_type.parent: optionMenu_ASFdown_type
*subMenu_ASFdown_type.rowColumnType: "menu_pulldown"
*subMenu_ASFdown_type.labelString: ""
*subMenu_ASFdown_type.sensitive: "true"
*subMenu_ASFdown_type.x: 0
*subMenu_ASFdown_type.y: 335
*subMenu_ASFdown_type.mappedWhenManaged: "true"
*subMenu_ASFdown_type.menuPost: ""

*ASFdown_type_PLANNED.class: pushButton
*ASFdown_type_PLANNED.static: true
*ASFdown_type_PLANNED.name: ASFdown_type_PLANNED
*ASFdown_type_PLANNED.parent: subMenu_ASFdown_type
*ASFdown_type_PLANNED.labelString: "PLANNED"
*ASFdown_type_PLANNED.fontList: "rockwell-bold"
*ASFdown_type_PLANNED.x: 2
*ASFdown_type_PLANNED.y: 335

*ASFdown_type_UNPLANNED.class: pushButton
*ASFdown_type_UNPLANNED.static: true
*ASFdown_type_UNPLANNED.name: ASFdown_type_UNPLANNED
*ASFdown_type_UNPLANNED.parent: subMenu_ASFdown_type
*ASFdown_type_UNPLANNED.labelString: "UNPLANNED"
*ASFdown_type_UNPLANNED.fontList: "rockwell-bold"

*optionMenu_ASFdown_reason.class: rowColumn
*optionMenu_ASFdown_reason.static: true
*optionMenu_ASFdown_reason.name: optionMenu_ASFdown_reason
*optionMenu_ASFdown_reason.parent: RGSDownTimeManager
*optionMenu_ASFdown_reason.rowColumnType: "menu_option"
*optionMenu_ASFdown_reason.subMenuId: "subMenu_ASFdown_reason"
*optionMenu_ASFdown_reason.isCompound: "true"
*optionMenu_ASFdown_reason.compoundIcon: "optionM.xpm"
*optionMenu_ASFdown_reason.compoundName: "option_Menu"
*optionMenu_ASFdown_reason.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_ASFdown_reason.x: 510
*optionMenu_ASFdown_reason.y: 400
*optionMenu_ASFdown_reason.width: 215
*optionMenu_ASFdown_reason.height: 35
*optionMenu_ASFdown_reason.labelString: "REASON"
*optionMenu_ASFdown_reason.sensitive: "false"

*subMenu_ASFdown_reason.class: rowColumn
*subMenu_ASFdown_reason.static: true
*subMenu_ASFdown_reason.name: subMenu_ASFdown_reason
*subMenu_ASFdown_reason.parent: optionMenu_ASFdown_reason
*subMenu_ASFdown_reason.rowColumnType: "menu_pulldown"
*subMenu_ASFdown_reason.labelString: ""
*subMenu_ASFdown_reason.sensitive: "true"
*subMenu_ASFdown_reason.x: 0
*subMenu_ASFdown_reason.y: 335
*subMenu_ASFdown_reason.mappedWhenManaged: "true"

*ASFdown_reason_CONFLICT.class: pushButton
*ASFdown_reason_CONFLICT.static: true
*ASFdown_reason_CONFLICT.name: ASFdown_reason_CONFLICT
*ASFdown_reason_CONFLICT.parent: subMenu_ASFdown_reason
*ASFdown_reason_CONFLICT.labelString: "CONFLICT"
*ASFdown_reason_CONFLICT.fontList: "rockwell-bold"
*ASFdown_reason_CONFLICT.x: 2
*ASFdown_reason_CONFLICT.y: 335

*ASFdown_reason_MAINTENANCE.class: pushButton
*ASFdown_reason_MAINTENANCE.static: true
*ASFdown_reason_MAINTENANCE.name: ASFdown_reason_MAINTENANCE
*ASFdown_reason_MAINTENANCE.parent: subMenu_ASFdown_reason
*ASFdown_reason_MAINTENANCE.labelString: "MAINTENANCE"
*ASFdown_reason_MAINTENANCE.fontList: "rockwell-bold"

*ASFdown_reason_REPAIR.class: pushButton
*ASFdown_reason_REPAIR.static: true
*ASFdown_reason_REPAIR.name: ASFdown_reason_REPAIR
*ASFdown_reason_REPAIR.parent: subMenu_ASFdown_reason
*ASFdown_reason_REPAIR.labelString: "REPAIR"
*ASFdown_reason_REPAIR.fontList: "rockwell-bold"

*ASFdown_reason_UPGRADE.class: pushButton
*ASFdown_reason_UPGRADE.static: true
*ASFdown_reason_UPGRADE.name: ASFdown_reason_UPGRADE
*ASFdown_reason_UPGRADE.parent: subMenu_ASFdown_reason
*ASFdown_reason_UPGRADE.labelString: "UPGRADE"
*ASFdown_reason_UPGRADE.fontList: "rockwell-bold"

*pushButton_CreateDownTime.class: pushButton
*pushButton_CreateDownTime.static: true
*pushButton_CreateDownTime.name: pushButton_CreateDownTime
*pushButton_CreateDownTime.parent: RGSDownTimeManager
*pushButton_CreateDownTime.isCompound: "true"
*pushButton_CreateDownTime.compoundIcon: "push.xpm"
*pushButton_CreateDownTime.compoundName: "push_Button"
*pushButton_CreateDownTime.x: 250
*pushButton_CreateDownTime.y: 605
*pushButton_CreateDownTime.width: 90
*pushButton_CreateDownTime.height: 40
*pushButton_CreateDownTime.labelString: "CREATE"
*pushButton_CreateDownTime.fontList: "rockwell-bold"
*pushButton_CreateDownTime.sensitive: "true"
*pushButton_CreateDownTime.activateCallback.source: public
*pushButton_CreateDownTime.activateCallback: cb_create_new_down_time

*label155.class: label
*label155.static: true
*label155.name: label155
*label155.parent: RGSDownTimeManager
*label155.isCompound: "true"
*label155.compoundIcon: "label.xpm"
*label155.compoundName: "label_"
*label155.x: 35
*label155.y: 525
*label155.height: 30
*label155.labelString: "TOTAL DAYS:"
*label155.alignment: "alignment_end"

*TF_ASF_DN_TIMES_STRTTIME.class: textField
*TF_ASF_DN_TIMES_STRTTIME.static: true
*TF_ASF_DN_TIMES_STRTTIME.name: TF_ASF_DN_TIMES_STRTTIME
*TF_ASF_DN_TIMES_STRTTIME.parent: RGSDownTimeManager
*TF_ASF_DN_TIMES_STRTTIME.isCompound: "true"
*TF_ASF_DN_TIMES_STRTTIME.compoundIcon: "textfield.xpm"
*TF_ASF_DN_TIMES_STRTTIME.compoundName: "text_Field"
*TF_ASF_DN_TIMES_STRTTIME.x: 110
*TF_ASF_DN_TIMES_STRTTIME.y: 405
*TF_ASF_DN_TIMES_STRTTIME.height: 30
*TF_ASF_DN_TIMES_STRTTIME.columns: 21
*TF_ASF_DN_TIMES_STRTTIME.cursorPositionVisible: "false"
*TF_ASF_DN_TIMES_STRTTIME.editable: "false"
*TF_ASF_DN_TIMES_STRTTIME.sensitive: "false"
*TF_ASF_DN_TIMES_STRTTIME.resizeWidth: "false"
*TF_ASF_DN_TIMES_STRTTIME.text: ""
*TF_ASF_DN_TIMES_STRTTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_ASF_DN_TIMES_STRTTIME.maxLength: 21
*TF_ASF_DN_TIMES_STRTTIME.activateCallback.source: public
*TF_ASF_DN_TIMES_STRTTIME.activateCallback: cb_validate_ASF_datetime
*TF_ASF_DN_TIMES_STRTTIME.activateCallbackClientData: (XtPointer) "RGS Down Start Time"
*TF_ASF_DN_TIMES_STRTTIME.focusCallback.source: public
*TF_ASF_DN_TIMES_STRTTIME.focusCallback: cb_toggle_cursor
*TF_ASF_DN_TIMES_STRTTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_ASF_DN_TIMES_STRTTIME.losingFocusCallback.source: public
*TF_ASF_DN_TIMES_STRTTIME.losingFocusCallback: cb_toggle_cursor
*TF_ASF_DN_TIMES_STRTTIME.losingFocusCallbackClientData: (XtPointer) FALSE

*TF_ASF_DN_TIMES_STOPTIME.class: textField
*TF_ASF_DN_TIMES_STOPTIME.static: true
*TF_ASF_DN_TIMES_STOPTIME.name: TF_ASF_DN_TIMES_STOPTIME
*TF_ASF_DN_TIMES_STOPTIME.parent: RGSDownTimeManager
*TF_ASF_DN_TIMES_STOPTIME.isCompound: "true"
*TF_ASF_DN_TIMES_STOPTIME.compoundIcon: "textfield.xpm"
*TF_ASF_DN_TIMES_STOPTIME.compoundName: "text_Field"
*TF_ASF_DN_TIMES_STOPTIME.x: 110
*TF_ASF_DN_TIMES_STOPTIME.y: 455
*TF_ASF_DN_TIMES_STOPTIME.height: 30
*TF_ASF_DN_TIMES_STOPTIME.columns: 21
*TF_ASF_DN_TIMES_STOPTIME.cursorPositionVisible: "true"
*TF_ASF_DN_TIMES_STOPTIME.editable: "false"
*TF_ASF_DN_TIMES_STOPTIME.sensitive: "false"
*TF_ASF_DN_TIMES_STOPTIME.resizeWidth: "false"
*TF_ASF_DN_TIMES_STOPTIME.text: ""
*TF_ASF_DN_TIMES_STOPTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_ASF_DN_TIMES_STOPTIME.maxLength: 21
*TF_ASF_DN_TIMES_STOPTIME.activateCallback.source: public
*TF_ASF_DN_TIMES_STOPTIME.activateCallback: cb_validate_ASF_datetime
*TF_ASF_DN_TIMES_STOPTIME.activateCallbackClientData: (XtPointer) "RGS Down Stop Time"
*TF_ASF_DN_TIMES_STOPTIME.focusCallback.source: public
*TF_ASF_DN_TIMES_STOPTIME.focusCallback: cb_toggle_cursor
*TF_ASF_DN_TIMES_STOPTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_ASF_DN_TIMES_STOPTIME.losingFocusCallback.source: public
*TF_ASF_DN_TIMES_STOPTIME.losingFocusCallback: cb_toggle_cursor
*TF_ASF_DN_TIMES_STOPTIME.losingFocusCallbackClientData: (XtPointer) FALSE

*TF_DownTime_total_days.class: textField
*TF_DownTime_total_days.static: true
*TF_DownTime_total_days.name: TF_DownTime_total_days
*TF_DownTime_total_days.parent: RGSDownTimeManager
*TF_DownTime_total_days.x: 110
*TF_DownTime_total_days.y: 524
*TF_DownTime_total_days.height: 31
*TF_DownTime_total_days.columns: 8
*TF_DownTime_total_days.resizeWidth: "false"
*TF_DownTime_total_days.text: ""
*TF_DownTime_total_days.cursorPositionVisible: "true"
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
*label122.parent: RGSDownTimeManager
*label122.x: 110
*label122.y: 435
*label122.width: 185
*label122.height: 15
*label122.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label122.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*label123.class: label
*label123.static: true
*label123.name: label123
*label123.parent: RGSDownTimeManager
*label123.x: 110
*label123.y: 490
*label123.width: 185
*label123.height: 15
*label123.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label123.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*T_ASF_DN_TIMES_REMARKS.class: text
*T_ASF_DN_TIMES_REMARKS.static: true
*T_ASF_DN_TIMES_REMARKS.name: T_ASF_DN_TIMES_REMARKS
*T_ASF_DN_TIMES_REMARKS.parent: RGSDownTimeManager
*T_ASF_DN_TIMES_REMARKS.isCompound: "true"
*T_ASF_DN_TIMES_REMARKS.compoundIcon: "text.xpm"
*T_ASF_DN_TIMES_REMARKS.compoundName: "text_"
*T_ASF_DN_TIMES_REMARKS.x: 390
*T_ASF_DN_TIMES_REMARKS.y: 455
*T_ASF_DN_TIMES_REMARKS.editMode: "multi_line_edit"
*T_ASF_DN_TIMES_REMARKS.maxLength: 60
*T_ASF_DN_TIMES_REMARKS.rows: 3
*T_ASF_DN_TIMES_REMARKS.resizeHeight: "false"
*T_ASF_DN_TIMES_REMARKS.resizeWidth: "false"
*T_ASF_DN_TIMES_REMARKS.columns: 30
*T_ASF_DN_TIMES_REMARKS.wordWrap: "true"
*T_ASF_DN_TIMES_REMARKS.cursorPositionVisible: "false"
*T_ASF_DN_TIMES_REMARKS.editable: "false"
*T_ASF_DN_TIMES_REMARKS.sensitive: "false"
*T_ASF_DN_TIMES_REMARKS.focusCallback.source: public
*T_ASF_DN_TIMES_REMARKS.focusCallback: cb_toggle_cursor
*T_ASF_DN_TIMES_REMARKS.focusCallbackClientData: (XtPointer) True
*T_ASF_DN_TIMES_REMARKS.losingFocusCallback.source: public
*T_ASF_DN_TIMES_REMARKS.losingFocusCallback: cb_toggle_cursor
*T_ASF_DN_TIMES_REMARKS.losingFocusCallbackClientData: (XtPointer) False

*label124.class: label
*label124.static: true
*label124.name: label124
*label124.parent: RGSDownTimeManager
*label124.isCompound: "true"
*label124.compoundIcon: "label.xpm"
*label124.compoundName: "label_"
*label124.x: 335
*label124.y: 455
*label124.height: 30
*label124.labelString: "REMARKS:"
*label124.alignment: "alignment_end"

*rowColumn4.class: rowColumn
*rowColumn4.static: true
*rowColumn4.name: rowColumn4
*rowColumn4.parent: RGSDownTimeManager
*rowColumn4.width: 290
*rowColumn4.height: 37
*rowColumn4.isCompound: "true"
*rowColumn4.compoundIcon: "row.xpm"
*rowColumn4.compoundName: "row_Column"
*rowColumn4.x: 390
*rowColumn4.y: 355
*rowColumn4.orientation: "horizontal"
*rowColumn4.radioBehavior: "true"
*rowColumn4.labelString: ""
*rowColumn4.sensitive: "false"

*toggleButton_DownTimeScheduled.class: toggleButton
*toggleButton_DownTimeScheduled.static: true
*toggleButton_DownTimeScheduled.name: toggleButton_DownTimeScheduled
*toggleButton_DownTimeScheduled.parent: rowColumn4
*toggleButton_DownTimeScheduled.isCompound: "true"
*toggleButton_DownTimeScheduled.compoundIcon: "toggle.xpm"
*toggleButton_DownTimeScheduled.compoundName: "toggle_Button"
*toggleButton_DownTimeScheduled.x: 3
*toggleButton_DownTimeScheduled.y: 355
*toggleButton_DownTimeScheduled.indicatorSize: 20
*toggleButton_DownTimeScheduled.labelString: "ACTIVE"
*toggleButton_DownTimeScheduled.fontList: "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8"
*toggleButton_DownTimeScheduled.set: "true"

*toggleButton_DownTimeCancelled.class: toggleButton
*toggleButton_DownTimeCancelled.static: true
*toggleButton_DownTimeCancelled.name: toggleButton_DownTimeCancelled
*toggleButton_DownTimeCancelled.parent: rowColumn4
*toggleButton_DownTimeCancelled.isCompound: "true"
*toggleButton_DownTimeCancelled.compoundIcon: "toggle.xpm"
*toggleButton_DownTimeCancelled.compoundName: "toggle_Button"
*toggleButton_DownTimeCancelled.x: 92
*toggleButton_DownTimeCancelled.y: 355
*toggleButton_DownTimeCancelled.indicatorSize: 20
*toggleButton_DownTimeCancelled.labelString: "CANCELLED"
*toggleButton_DownTimeCancelled.fontList: "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8"

*optionMenu_station_id.class: rowColumn
*optionMenu_station_id.static: true
*optionMenu_station_id.name: optionMenu_station_id
*optionMenu_station_id.parent: RGSDownTimeManager
*optionMenu_station_id.rowColumnType: "menu_option"
*optionMenu_station_id.subMenuId: "subMenu_rgs_down_stn_id"
*optionMenu_station_id.isCompound: "true"
*optionMenu_station_id.compoundIcon: "optionM.xpm"
*optionMenu_station_id.compoundName: "option_Menu"
*optionMenu_station_id.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_station_id.x: 60
*optionMenu_station_id.y: 355
*optionMenu_station_id.width: 215
*optionMenu_station_id.height: 35
*optionMenu_station_id.labelString: "STATION\nID:"
*optionMenu_station_id.sensitive: "false"

*subMenu_rgs_down_stn_id.class: rowColumn
*subMenu_rgs_down_stn_id.static: true
*subMenu_rgs_down_stn_id.name: subMenu_rgs_down_stn_id
*subMenu_rgs_down_stn_id.parent: optionMenu_station_id
*subMenu_rgs_down_stn_id.rowColumnType: "menu_pulldown"
*subMenu_rgs_down_stn_id.labelString: ""
*subMenu_rgs_down_stn_id.height: 48
*subMenu_rgs_down_stn_id.resizeHeight: "false"
*subMenu_rgs_down_stn_id.x: 0
*subMenu_rgs_down_stn_id.y: 355
*subMenu_rgs_down_stn_id.sensitive: "true"
*subMenu_rgs_down_stn_id.mappedWhenManaged: "true"

*subMenu_rgs_down_stn_id_pb.class: pushButton
*subMenu_rgs_down_stn_id_pb.static: true
*subMenu_rgs_down_stn_id_pb.name: subMenu_rgs_down_stn_id_pb
*subMenu_rgs_down_stn_id_pb.parent: subMenu_rgs_down_stn_id
*subMenu_rgs_down_stn_id_pb.labelString: "ASF"
*subMenu_rgs_down_stn_id_pb.fontList: "rockwell-bold"
*subMenu_rgs_down_stn_id_pb.x: 2
*subMenu_rgs_down_stn_id_pb.y: 355
*subMenu_rgs_down_stn_id_pb.createCallback.source: public
*subMenu_rgs_down_stn_id_pb.createCallback: cb_build_station_option_menu

*label22.class: label
*label22.static: true
*label22.name: label22
*label22.parent: RGSDownTimeManager
*label22.isCompound: "true"
*label22.compoundIcon: "label.xpm"
*label22.compoundName: "label_"
*label22.x: 345
*label22.y: 355
*label22.height: 30
*label22.labelString: "STATUS:"
*label22.alignment: "alignment_end"

*label27.class: label
*label27.static: true
*label27.name: label27
*label27.parent: RGSDownTimeManager
*label27.isCompound: "true"
*label27.compoundIcon: "label.xpm"
*label27.compoundName: "label_"
*label27.x: 285
*label27.y: 525
*label27.height: 30
*label27.labelString: "FA Unavailibilty\nReport Created:"
*label27.alignment: "alignment_end"

*TF_FA_Notification.class: textField
*TF_FA_Notification.static: true
*TF_FA_Notification.name: TF_FA_Notification
*TF_FA_Notification.parent: RGSDownTimeManager
*TF_FA_Notification.x: 390
*TF_FA_Notification.y: 525
*TF_FA_Notification.height: 31
*TF_FA_Notification.columns: 3
*TF_FA_Notification.resizeWidth: "false"
*TF_FA_Notification.text: "NO"
*TF_FA_Notification.cursorPositionVisible: "false"
*TF_FA_Notification.editable: "false"
*TF_FA_Notification.sensitive: "false"

