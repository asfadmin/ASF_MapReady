! UIMX ascii 2.9 key: 82                                                        

*DARManager.class: form
*DARManager.classinc:
*DARManager.classspec:
*DARManager.classmembers:
*DARManager.classconstructor:
*DARManager.classdestructor:
*DARManager.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)DARManager.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.DARManager.i"\
\
#include <stdlib.h>\
\
#include <Xm/FileSB.h>\
#include <Xm/DialogS.h>\
\
#include "db_sybint.h"\
#include "aps_db_table.h"\
#include "DARconversions.h"\
\
#include "gui_utils.h"\
#include "cb_sortform.h"\
#include "cb_searchform.h"\
#include "cb_datetime.h"\
#include "cb_darmanager.h"\
\
extern Widget DAR_manager;\
extern Widget filebox ;\

*DARManager.ispecdecl:
*DARManager.funcdecl: swidget create_DARManager(swidget UxParent)
*DARManager.funcname: create_DARManager
*DARManager.funcdef: "swidget", "<create_DARManager>(%)"
*DARManager.argdecl: swidget UxParent;
*DARManager.arglist: UxParent
*DARManager.arglist.UxParent: "swidget", "%UxParent%"
*DARManager.icode: Position x, y ;\
SORT_INFO *sortinfo ;\
SEARCH_INFO *searchinfo ;\
PERIOD_WIDGETS *DAR_times ;\

*DARManager.fcode: sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;\
searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;\
\
sortinfo->table_name =  (char *) APS_TABLE(DAR) ;\
sortinfo->field_to_update = (Widget) TF_DAR_sortclause ;\
\
searchinfo->table_name = APS_TABLE(DAR) ;\
searchinfo->field_to_update  = (Widget) TF_DAR_searchclause ;\
\
DAR_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
DAR_times->start = (Widget) TF_DAR_STRTTIME ;\
DAR_times->stop = (Widget) TF_DAR_ENDTIME ;\
\
XtAddCallback( pushButton_SortDAR, XmNactivateCallback,\
    (XtCallbackProc) cb_edit_sort_columns,\
    (XtPointer) sortinfo );\
 \
XtAddCallback( pushButton_SearchDAR, XmNactivateCallback,\
    (XtCallbackProc) cb_edit_search_columns,\
    (XtPointer) searchinfo );\
 \
XtAddCallback(TF_DAR_total_days, XmNactivateCallback,\
    (XtCallbackProc) cb_adjust_ASF_datetimes,\
    (XtPointer) DAR_times) ;\
\
 /*\
    -- the form_quad is the default shape form\
    -- at start up get its (x,y) position for use\
    -- by the form_circle\
    --\
    -- the circle form is constructed to the right\
    -- of the quad form using VC.  It is not placed\
    -- directly on top because may think its a form\
    -- within a form (form_quad --> form_circle)\
    */\
    XtVaGetValues(form_DARquad,\
        XmNx, &x,\
        XmNy, &y,\
        NULL) ;\
\
     XtVaSetValues(form_DARcircle,\
        XmNx, x,\
        XmNy, y,\
        NULL) ;\
\
	XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
		cb_show_dar_records, (XtPointer *) scrolledList_DARS) ;\
 \
return(rtrn);\

*DARManager.auxdecl:
*DARManager.static: true
*DARManager.name: DARManager
*DARManager.parent: NO_PARENT
*DARManager.parentExpression: UxParent
*DARManager.defaultShell: topLevelShell
*DARManager.width: 755
*DARManager.height: 846
*DARManager.resizePolicy: "resize_none"
*DARManager.isCompound: "true"
*DARManager.compoundIcon: "form.xpm"
*DARManager.compoundName: "form_"
*DARManager.unitType: "pixels"
*DARManager.dialogTitle: "APS:DAR Manager"
*DARManager.x: 129
*DARManager.y: 6

*label78.class: label
*label78.static: true
*label78.name: label78
*label78.parent: DARManager
*label78.isCompound: "true"
*label78.compoundIcon: "label.xpm"
*label78.compoundName: "label_"
*label78.x: 15
*label78.y: 15
*label78.width: 730
*label78.height: 35
*label78.labelString: "DAR  MANAGER"
*label78.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindow_DARS.class: scrolledWindow
*scrolledWindow_DARS.static: true
*scrolledWindow_DARS.name: scrolledWindow_DARS
*scrolledWindow_DARS.parent: DARManager
*scrolledWindow_DARS.scrollingPolicy: "application_defined"
*scrolledWindow_DARS.visualPolicy: "variable"
*scrolledWindow_DARS.scrollBarDisplayPolicy: "static"
*scrolledWindow_DARS.shadowThickness: 0
*scrolledWindow_DARS.isCompound: "true"
*scrolledWindow_DARS.compoundIcon: "scrllist.xpm"
*scrolledWindow_DARS.compoundName: "scrolled_List"
*scrolledWindow_DARS.x: 65
*scrolledWindow_DARS.y: 90
*scrolledWindow_DARS.width: 660

*scrolledList_DARS.class: scrolledList
*scrolledList_DARS.static: true
*scrolledList_DARS.name: scrolledList_DARS
*scrolledList_DARS.parent: scrolledWindow_DARS
*scrolledList_DARS.width: 660
*scrolledList_DARS.height: 140
*scrolledList_DARS.itemCount: 1
*scrolledList_DARS.items: "1234567 QUE RADARSAT/SAR 1990:001:00:00:00 1991:001:00:00:00 COLDEST PLACE ON EARTH"
*scrolledList_DARS.browseSelectionCallback.source: public
*scrolledList_DARS.browseSelectionCallback: cb_update_darmanager_form
*scrolledList_DARS.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_DARS.selectionPolicy: "browse_select"
*scrolledList_DARS.listSizePolicy: "constant"
*scrolledList_DARS.visibleItemCount: 8
*scrolledList_DARS.y: 1
*scrolledList_DARS.x: 1

*TF_DAR_DARID.class: textField
*TF_DAR_DARID.static: true
*TF_DAR_DARID.name: TF_DAR_DARID
*TF_DAR_DARID.parent: DARManager
*TF_DAR_DARID.isCompound: "true"
*TF_DAR_DARID.compoundIcon: "textfield.xpm"
*TF_DAR_DARID.compoundName: "text_Field"
*TF_DAR_DARID.x: 101
*TF_DAR_DARID.y: 333
*TF_DAR_DARID.height: 32
*TF_DAR_DARID.cursorPositionVisible: "false"
*TF_DAR_DARID.editable: "false"
*TF_DAR_DARID.text: ""
*TF_DAR_DARID.columns: 5
*TF_DAR_DARID.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_DARID.resizeWidth: "false"
*TF_DAR_DARID.traversalOn: "false"

*pushButton_QuitDAR.class: pushButton
*pushButton_QuitDAR.static: true
*pushButton_QuitDAR.name: pushButton_QuitDAR
*pushButton_QuitDAR.parent: DARManager
*pushButton_QuitDAR.isCompound: "true"
*pushButton_QuitDAR.compoundIcon: "push.xpm"
*pushButton_QuitDAR.compoundName: "push_Button"
*pushButton_QuitDAR.x: 629
*pushButton_QuitDAR.y: 775
*pushButton_QuitDAR.width: 90
*pushButton_QuitDAR.height: 40
*pushButton_QuitDAR.labelString: "QUIT"
*pushButton_QuitDAR.fontList: "rockwell-bold"
*pushButton_QuitDAR.activateCallback.source: public
*pushButton_QuitDAR.activateCallback: cb_quit_darmanager

*pushButton_SortDAR.class: pushButton
*pushButton_SortDAR.static: true
*pushButton_SortDAR.name: pushButton_SortDAR
*pushButton_SortDAR.parent: DARManager
*pushButton_SortDAR.isCompound: "true"
*pushButton_SortDAR.compoundIcon: "push.xpm"
*pushButton_SortDAR.compoundName: "push_Button"
*pushButton_SortDAR.x: 375
*pushButton_SortDAR.labelString: "SORT BY"
*pushButton_SortDAR.fontList: "rockwell-bold"
*pushButton_SortDAR.y: 265
*pushButton_SortDAR.height: 38
*pushButton_SortDAR.width: 80

*label133.class: label
*label133.static: true
*label133.name: label133
*label133.parent: DARManager
*label133.isCompound: "true"
*label133.compoundIcon: "label.xpm"
*label133.compoundName: "label_"
*label133.x: 65
*label133.y: 60
*label133.height: 20
*label133.labelString: "  DARID STATUS SAT/SENSOR    START TIME        STOP TIME          SITE NAME" 
*label133.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label133.alignment: "alignment_beginning"
*label133.width: 675

*form_DARcircle.class: form
*form_DARcircle.static: true
*form_DARcircle.name: form_DARcircle
*form_DARcircle.parent: DARManager
*form_DARcircle.resizePolicy: "resize_none"
*form_DARcircle.x: 775
*form_DARcircle.y: 445
*form_DARcircle.width: 420
*form_DARcircle.height: 80
*form_DARcircle.createManaged: "false"

*TF_DAR_center_lon.class: textField
*TF_DAR_center_lon.static: true
*TF_DAR_center_lon.name: TF_DAR_center_lon
*TF_DAR_center_lon.parent: form_DARcircle
*TF_DAR_center_lon.isCompound: "true"
*TF_DAR_center_lon.compoundIcon: "textfield.xpm"
*TF_DAR_center_lon.compoundName: "text_Field"
*TF_DAR_center_lon.x: 140
*TF_DAR_center_lon.y: 25
*TF_DAR_center_lon.height: 30
*TF_DAR_center_lon.cursorPositionVisible: "false"
*TF_DAR_center_lon.editable: "false"
*TF_DAR_center_lon.text: ""
*TF_DAR_center_lon.columns: 7
*TF_DAR_center_lon.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_center_lon.resizeWidth: "false"
*TF_DAR_center_lon.createManaged: "true"

*TF_DAR_center_lat.class: textField
*TF_DAR_center_lat.static: true
*TF_DAR_center_lat.name: TF_DAR_center_lat
*TF_DAR_center_lat.parent: form_DARcircle
*TF_DAR_center_lat.isCompound: "true"
*TF_DAR_center_lat.compoundIcon: "textfield.xpm"
*TF_DAR_center_lat.compoundName: "text_Field"
*TF_DAR_center_lat.x: 50
*TF_DAR_center_lat.y: 25
*TF_DAR_center_lat.height: 30
*TF_DAR_center_lat.cursorPositionVisible: "false"
*TF_DAR_center_lat.editable: "false"
*TF_DAR_center_lat.text: ""
*TF_DAR_center_lat.columns: 7
*TF_DAR_center_lat.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_center_lat.resizeWidth: "false"
*TF_DAR_center_lat.createManaged: "true"

*label92.class: label
*label92.static: true
*label92.name: label92
*label92.parent: form_DARcircle
*label92.isCompound: "true"
*label92.compoundIcon: "label.xpm"
*label92.compoundName: "label_"
*label92.x: 125
*label92.y: 30
*label92.width: 15
*label92.height: 20
*label92.labelString: "/"
*label92.createManaged: "true"

*label93.class: label
*label93.static: true
*label93.name: label93
*label93.parent: form_DARcircle
*label93.isCompound: "true"
*label93.compoundIcon: "label.xpm"
*label93.compoundName: "label_"
*label93.x: 5
*label93.y: 25
*label93.width: 46
*label93.height: 30
*label93.labelString: "CENTER:"
*label93.alignment: "alignment_end"
*label93.createManaged: "true"

*label94.class: label
*label94.static: true
*label94.name: label94
*label94.parent: form_DARcircle
*label94.isCompound: "true"
*label94.compoundIcon: "label.xpm"
*label94.compoundName: "label_"
*label94.x: 220
*label94.y: 25
*label94.width: 51
*label94.height: 30
*label94.labelString: "RADIUS:"
*label94.alignment: "alignment_end"
*label94.createManaged: "true"

*TF_RADIUS.class: textField
*TF_RADIUS.static: true
*TF_RADIUS.name: TF_RADIUS
*TF_RADIUS.parent: form_DARcircle
*TF_RADIUS.isCompound: "true"
*TF_RADIUS.compoundIcon: "textfield.xpm"
*TF_RADIUS.compoundName: "text_Field"
*TF_RADIUS.x: 270
*TF_RADIUS.y: 25
*TF_RADIUS.height: 30
*TF_RADIUS.cursorPositionVisible: "false"
*TF_RADIUS.editable: "false"
*TF_RADIUS.text: ""
*TF_RADIUS.columns: 7
*TF_RADIUS.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_RADIUS.resizeWidth: "false"
*TF_RADIUS.createManaged: "true"

*label95.class: label
*label95.static: true
*label95.name: label95
*label95.parent: form_DARcircle
*label95.isCompound: "true"
*label95.compoundIcon: "label.xpm"
*label95.compoundName: "label_"
*label95.x: 345
*label95.y: 25
*label95.width: 21
*label95.height: 30
*label95.labelString: "km"
*label95.alignment: "alignment_end"
*label95.createManaged: "true"

*TF_DAR_STRTTIME.class: textField
*TF_DAR_STRTTIME.static: true
*TF_DAR_STRTTIME.name: TF_DAR_STRTTIME
*TF_DAR_STRTTIME.parent: DARManager
*TF_DAR_STRTTIME.isCompound: "true"
*TF_DAR_STRTTIME.compoundIcon: "textfield.xpm"
*TF_DAR_STRTTIME.compoundName: "text_Field"
*TF_DAR_STRTTIME.x: 101
*TF_DAR_STRTTIME.y: 405
*TF_DAR_STRTTIME.height: 32
*TF_DAR_STRTTIME.columns: 21
*TF_DAR_STRTTIME.cursorPositionVisible: "false"
*TF_DAR_STRTTIME.editable: "false"
*TF_DAR_STRTTIME.resizeWidth: "false"
*TF_DAR_STRTTIME.text: "1990:001:00:00:00.000"
*TF_DAR_STRTTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_STRTTIME.maxLength: 21
*TF_DAR_STRTTIME.activateCallback.source: public
*TF_DAR_STRTTIME.activateCallback: cb_validate_ASF_datetime
*TF_DAR_STRTTIME.activateCallbackClientData: (XtPointer) "DAR Start Time"
*TF_DAR_STRTTIME.focusCallback.source: public
*TF_DAR_STRTTIME.focusCallback: cb_toggle_cursor
*TF_DAR_STRTTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_DAR_STRTTIME.losingFocusCallback.source: public
*TF_DAR_STRTTIME.losingFocusCallback: cb_toggle_cursor
*TF_DAR_STRTTIME.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_DAR_STRTTIME.valueWcs: ""
*TF_DAR_STRTTIME.traversalOn: "false"

*TF_DAR_ENDTIME.class: textField
*TF_DAR_ENDTIME.static: true
*TF_DAR_ENDTIME.name: TF_DAR_ENDTIME
*TF_DAR_ENDTIME.parent: DARManager
*TF_DAR_ENDTIME.isCompound: "true"
*TF_DAR_ENDTIME.compoundIcon: "textfield.xpm"
*TF_DAR_ENDTIME.compoundName: "text_Field"
*TF_DAR_ENDTIME.x: 101
*TF_DAR_ENDTIME.y: 442
*TF_DAR_ENDTIME.height: 32
*TF_DAR_ENDTIME.columns: 21
*TF_DAR_ENDTIME.cursorPositionVisible: "false"
*TF_DAR_ENDTIME.editable: "false"
*TF_DAR_ENDTIME.resizeWidth: "false"
*TF_DAR_ENDTIME.text: "1999:001:00:00:00.000"
*TF_DAR_ENDTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_ENDTIME.maxLength: 21
*TF_DAR_ENDTIME.activateCallback.source: public
*TF_DAR_ENDTIME.activateCallback: cb_validate_ASF_datetime
*TF_DAR_ENDTIME.activateCallbackClientData: (XtPointer) "DAR Stop Time"
*TF_DAR_ENDTIME.focusCallback.source: public
*TF_DAR_ENDTIME.focusCallback: cb_toggle_cursor
*TF_DAR_ENDTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_DAR_ENDTIME.losingFocusCallback.source: public
*TF_DAR_ENDTIME.losingFocusCallback: cb_toggle_cursor
*TF_DAR_ENDTIME.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_DAR_ENDTIME.valueWcs: ""
*TF_DAR_ENDTIME.traversalOn: "false"

*TF_DAR_total_days.class: textField
*TF_DAR_total_days.static: true
*TF_DAR_total_days.name: TF_DAR_total_days
*TF_DAR_total_days.parent: DARManager
*TF_DAR_total_days.x: 101
*TF_DAR_total_days.y: 484
*TF_DAR_total_days.height: 32
*TF_DAR_total_days.columns: 8
*TF_DAR_total_days.resizeWidth: "false"
*TF_DAR_total_days.text: ""
*TF_DAR_total_days.cursorPositionVisible: "false"
*TF_DAR_total_days.editable: "false"
*TF_DAR_total_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_total_days.maxLength: 8
*TF_DAR_total_days.width: 83
*TF_DAR_total_days.modifyVerifyCallback.source: public
*TF_DAR_total_days.modifyVerifyCallback: cb_filter_text
*TF_DAR_total_days.modifyVerifyCallbackClientData: (XtPointer) valid_float_chars
*TF_DAR_total_days.focusCallback.source: public
*TF_DAR_total_days.focusCallback: cb_toggle_cursor
*TF_DAR_total_days.focusCallbackClientData: (XtPointer) TRUE
*TF_DAR_total_days.losingFocusCallback.source: public
*TF_DAR_total_days.losingFocusCallback: cb_toggle_cursor
*TF_DAR_total_days.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_DAR_total_days.traversalOn: "false"

*label99.class: label
*label99.static: true
*label99.name: label99
*label99.parent: DARManager
*label99.isCompound: "true"
*label99.compoundIcon: "label.xpm"
*label99.compoundName: "label_"
*label99.x: 26
*label99.y: 484
*label99.height: 30
*label99.labelString: "TOTAL DAYS:"
*label99.alignment: "alignment_end"

*form_DARquad.class: form
*form_DARquad.static: true
*form_DARquad.name: form_DARquad
*form_DARquad.parent: DARManager
*form_DARquad.resizePolicy: "resize_none"
*form_DARquad.width: 401
*form_DARquad.height: 74
*form_DARquad.x: 346
*form_DARquad.y: 484

*TF_NWLON.class: textField
*TF_NWLON.static: true
*TF_NWLON.name: TF_NWLON
*TF_NWLON.parent: form_DARquad
*TF_NWLON.isCompound: "true"
*TF_NWLON.compoundIcon: "textfield.xpm"
*TF_NWLON.compoundName: "text_Field"
*TF_NWLON.x: 120
*TF_NWLON.y: 5
*TF_NWLON.height: 30
*TF_NWLON.cursorPositionVisible: "false"
*TF_NWLON.editable: "false"
*TF_NWLON.text: ""
*TF_NWLON.columns: 7
*TF_NWLON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NWLON.resizeWidth: "false"
*TF_NWLON.traversalOn: "false"

*label134.class: label
*label134.static: true
*label134.name: label134
*label134.parent: form_DARquad
*label134.isCompound: "true"
*label134.compoundIcon: "label.xpm"
*label134.compoundName: "label_"
*label134.x: 105
*label134.y: 10
*label134.width: 15
*label134.height: 20
*label134.labelString: "/"

*TF_NELON.class: textField
*TF_NELON.static: true
*TF_NELON.name: TF_NELON
*TF_NELON.parent: form_DARquad
*TF_NELON.isCompound: "true"
*TF_NELON.compoundIcon: "textfield.xpm"
*TF_NELON.compoundName: "text_Field"
*TF_NELON.x: 325
*TF_NELON.y: 5
*TF_NELON.height: 30
*TF_NELON.cursorPositionVisible: "false"
*TF_NELON.editable: "false"
*TF_NELON.text: ""
*TF_NELON.columns: 7
*TF_NELON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NELON.resizeWidth: "false"
*TF_NELON.traversalOn: "false"

*label131.class: label
*label131.static: true
*label131.name: label131
*label131.parent: form_DARquad
*label131.isCompound: "true"
*label131.compoundIcon: "label.xpm"
*label131.compoundName: "label_"
*label131.x: 310
*label131.y: 10
*label131.width: 15
*label131.height: 20
*label131.labelString: "/"

*label_NE1.class: label
*label_NE1.static: true
*label_NE1.name: label_NE1
*label_NE1.parent: form_DARquad
*label_NE1.isCompound: "true"
*label_NE1.compoundIcon: "label.xpm"
*label_NE1.compoundName: "label_"
*label_NE1.x: 210
*label_NE1.y: 5
*label_NE1.width: 26
*label_NE1.height: 30
*label_NE1.labelString: "NE:"
*label_NE1.alignment: "alignment_end"

*TF_SWLON.class: textField
*TF_SWLON.static: true
*TF_SWLON.name: TF_SWLON
*TF_SWLON.parent: form_DARquad
*TF_SWLON.isCompound: "true"
*TF_SWLON.compoundIcon: "textfield.xpm"
*TF_SWLON.compoundName: "text_Field"
*TF_SWLON.x: 120
*TF_SWLON.y: 40
*TF_SWLON.height: 30
*TF_SWLON.cursorPositionVisible: "false"
*TF_SWLON.editable: "false"
*TF_SWLON.text: ""
*TF_SWLON.columns: 7
*TF_SWLON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SWLON.resizeWidth: "false"
*TF_SWLON.traversalOn: "false"

*label130.class: label
*label130.static: true
*label130.name: label130
*label130.parent: form_DARquad
*label130.isCompound: "true"
*label130.compoundIcon: "label.xpm"
*label130.compoundName: "label_"
*label130.x: 105
*label130.y: 45
*label130.width: 15
*label130.height: 20
*label130.labelString: "/"

*label89.class: label
*label89.static: true
*label89.name: label89
*label89.parent: form_DARquad
*label89.isCompound: "true"
*label89.compoundIcon: "label.xpm"
*label89.compoundName: "label_"
*label89.x: 2
*label89.y: 40
*label89.width: 26
*label89.height: 30
*label89.labelString: "SW:"
*label89.alignment: "alignment_end"

*TF_SELAT.class: textField
*TF_SELAT.static: true
*TF_SELAT.name: TF_SELAT
*TF_SELAT.parent: form_DARquad
*TF_SELAT.isCompound: "true"
*TF_SELAT.compoundIcon: "textfield.xpm"
*TF_SELAT.compoundName: "text_Field"
*TF_SELAT.x: 235
*TF_SELAT.y: 40
*TF_SELAT.height: 30
*TF_SELAT.cursorPositionVisible: "false"
*TF_SELAT.editable: "false"
*TF_SELAT.text: ""
*TF_SELAT.columns: 7
*TF_SELAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SELAT.resizeWidth: "false"
*TF_SELAT.traversalOn: "false"

*TF_SELON.class: textField
*TF_SELON.static: true
*TF_SELON.name: TF_SELON
*TF_SELON.parent: form_DARquad
*TF_SELON.isCompound: "true"
*TF_SELON.compoundIcon: "textfield.xpm"
*TF_SELON.compoundName: "text_Field"
*TF_SELON.x: 325
*TF_SELON.y: 40
*TF_SELON.height: 30
*TF_SELON.cursorPositionVisible: "false"
*TF_SELON.editable: "false"
*TF_SELON.text: ""
*TF_SELON.columns: 7
*TF_SELON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SELON.resizeWidth: "false"
*TF_SELON.traversalOn: "false"

*label132.class: label
*label132.static: true
*label132.name: label132
*label132.parent: form_DARquad
*label132.isCompound: "true"
*label132.compoundIcon: "label.xpm"
*label132.compoundName: "label_"
*label132.x: 310
*label132.y: 45
*label132.width: 15
*label132.height: 20
*label132.labelString: "/"

*label91.class: label
*label91.static: true
*label91.name: label91
*label91.parent: form_DARquad
*label91.isCompound: "true"
*label91.compoundIcon: "label.xpm"
*label91.compoundName: "label_"
*label91.x: 210
*label91.y: 40
*label91.width: 26
*label91.height: 30
*label91.labelString: "SE:"
*label91.alignment: "alignment_end"

*label_NW1.class: label
*label_NW1.static: true
*label_NW1.name: label_NW1
*label_NW1.parent: form_DARquad
*label_NW1.isCompound: "true"
*label_NW1.compoundIcon: "label.xpm"
*label_NW1.compoundName: "label_"
*label_NW1.x: 2
*label_NW1.y: 5
*label_NW1.width: 26
*label_NW1.height: 30
*label_NW1.labelString: "NW:"
*label_NW1.alignment: "alignment_end"

*TF_NELAT.class: textField
*TF_NELAT.static: true
*TF_NELAT.name: TF_NELAT
*TF_NELAT.parent: form_DARquad
*TF_NELAT.isCompound: "true"
*TF_NELAT.compoundIcon: "textfield.xpm"
*TF_NELAT.compoundName: "text_Field"
*TF_NELAT.x: 235
*TF_NELAT.y: 5
*TF_NELAT.height: 30
*TF_NELAT.cursorPositionVisible: "false"
*TF_NELAT.editable: "false"
*TF_NELAT.text: ""
*TF_NELAT.columns: 7
*TF_NELAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NELAT.resizeWidth: "false"
*TF_NELAT.traversalOn: "false"

*TF_NWLAT.class: textField
*TF_NWLAT.static: true
*TF_NWLAT.name: TF_NWLAT
*TF_NWLAT.parent: form_DARquad
*TF_NWLAT.isCompound: "true"
*TF_NWLAT.compoundIcon: "textfield.xpm"
*TF_NWLAT.compoundName: "text_Field"
*TF_NWLAT.x: 29
*TF_NWLAT.y: 5
*TF_NWLAT.height: 30
*TF_NWLAT.cursorPositionVisible: "false"
*TF_NWLAT.editable: "false"
*TF_NWLAT.text: ""
*TF_NWLAT.columns: 7
*TF_NWLAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NWLAT.resizeWidth: "false"
*TF_NWLAT.traversalOn: "false"

*TF_SWLAT.class: textField
*TF_SWLAT.static: true
*TF_SWLAT.name: TF_SWLAT
*TF_SWLAT.parent: form_DARquad
*TF_SWLAT.isCompound: "true"
*TF_SWLAT.compoundIcon: "textfield.xpm"
*TF_SWLAT.compoundName: "text_Field"
*TF_SWLAT.x: 29
*TF_SWLAT.y: 40
*TF_SWLAT.height: 30
*TF_SWLAT.cursorPositionVisible: "false"
*TF_SWLAT.editable: "false"
*TF_SWLAT.text: ""
*TF_SWLAT.columns: 7
*TF_SWLAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SWLAT.resizeWidth: "false"
*TF_SWLAT.traversalOn: "false"

*TF_DAR_SITENAME.class: textField
*TF_DAR_SITENAME.static: true
*TF_DAR_SITENAME.name: TF_DAR_SITENAME
*TF_DAR_SITENAME.parent: DARManager
*TF_DAR_SITENAME.isCompound: "true"
*TF_DAR_SITENAME.compoundIcon: "textfield.xpm"
*TF_DAR_SITENAME.compoundName: "text_Field"
*TF_DAR_SITENAME.x: 473
*TF_DAR_SITENAME.y: 442
*TF_DAR_SITENAME.height: 32
*TF_DAR_SITENAME.cursorPositionVisible: "false"
*TF_DAR_SITENAME.editable: "false"
*TF_DAR_SITENAME.text: ""
*TF_DAR_SITENAME.columns: 32
*TF_DAR_SITENAME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_SITENAME.resizeWidth: "false"
*TF_DAR_SITENAME.traversalOn: "false"

*label80.class: label
*label80.static: true
*label80.name: label80
*label80.parent: DARManager
*label80.isCompound: "true"
*label80.compoundIcon: "label.xpm"
*label80.compoundName: "label_"
*label80.x: 440
*label80.y: 441
*label80.width: 36
*label80.height: 30
*label80.labelString: "SITE \nNAME:"
*label80.alignment: "alignment_end"

*TF_DAR_SHAPE.class: textField
*TF_DAR_SHAPE.static: true
*TF_DAR_SHAPE.name: TF_DAR_SHAPE
*TF_DAR_SHAPE.parent: DARManager
*TF_DAR_SHAPE.isCompound: "true"
*TF_DAR_SHAPE.compoundIcon: "textfield.xpm"
*TF_DAR_SHAPE.compoundName: "text_Field"
*TF_DAR_SHAPE.x: 375
*TF_DAR_SHAPE.y: 442
*TF_DAR_SHAPE.height: 32
*TF_DAR_SHAPE.cursorPositionVisible: "false"
*TF_DAR_SHAPE.editable: "false"
*TF_DAR_SHAPE.text: ""
*TF_DAR_SHAPE.columns: 6
*TF_DAR_SHAPE.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_SHAPE.resizeWidth: "false"
*TF_DAR_SHAPE.traversalOn: "false"

*label81.class: label
*label81.static: true
*label81.name: label81
*label81.parent: DARManager
*label81.isCompound: "true"
*label81.compoundIcon: "label.xpm"
*label81.compoundName: "label_"
*label81.x: 334
*label81.y: 443
*label81.width: 40
*label81.height: 30
*label81.labelString: "SHAPE:"
*label81.alignment: "alignment_end"

*TF_USERID.class: textField
*TF_USERID.static: true
*TF_USERID.name: TF_USERID
*TF_USERID.parent: DARManager
*TF_USERID.isCompound: "true"
*TF_USERID.compoundIcon: "textfield.xpm"
*TF_USERID.compoundName: "text_Field"
*TF_USERID.x: 625
*TF_USERID.y: 333
*TF_USERID.height: 32
*TF_USERID.cursorPositionVisible: "false"
*TF_USERID.editable: "false"
*TF_USERID.text: ""
*TF_USERID.columns: 13
*TF_USERID.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_USERID.resizeWidth: "false"
*TF_USERID.maxLength: 13
*TF_USERID.traversalOn: "false"

*TF_SAT.class: textField
*TF_SAT.static: true
*TF_SAT.name: TF_SAT
*TF_SAT.parent: DARManager
*TF_SAT.isCompound: "true"
*TF_SAT.compoundIcon: "textfield.xpm"
*TF_SAT.compoundName: "text_Field"
*TF_SAT.x: 375
*TF_SAT.y: 369
*TF_SAT.height: 32
*TF_SAT.cursorPositionVisible: "false"
*TF_SAT.editable: "false"
*TF_SAT.text: ""
*TF_SAT.columns: 9
*TF_SAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SAT.resizeWidth: "false"
*TF_SAT.traversalOn: "false"

*label101.class: label
*label101.static: true
*label101.name: label101
*label101.parent: DARManager
*label101.isCompound: "true"
*label101.compoundIcon: "label.xpm"
*label101.compoundName: "label_"
*label101.x: 323
*label101.y: 370
*label101.width: 51
*label101.height: 30
*label101.labelString: "SAT/\nSENSOR:"
*label101.alignment: "alignment_end"

*TF_SENSOR.class: textField
*TF_SENSOR.static: true
*TF_SENSOR.name: TF_SENSOR
*TF_SENSOR.parent: DARManager
*TF_SENSOR.isCompound: "true"
*TF_SENSOR.compoundIcon: "textfield.xpm"
*TF_SENSOR.compoundName: "text_Field"
*TF_SENSOR.x: 475
*TF_SENSOR.y: 369
*TF_SENSOR.height: 32
*TF_SENSOR.cursorPositionVisible: "false"
*TF_SENSOR.editable: "false"
*TF_SENSOR.text: ""
*TF_SENSOR.columns: 6
*TF_SENSOR.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SENSOR.resizeWidth: "false"
*TF_SENSOR.traversalOn: "false"

*label102.class: label
*label102.static: true
*label102.name: label102
*label102.parent: DARManager
*label102.isCompound: "true"
*label102.compoundIcon: "label.xpm"
*label102.compoundName: "label_"
*label102.x: 314
*label102.y: 334
*label102.width: 60
*label102.height: 30
*label102.labelString: "REQUESTED"
*label102.alignment: "alignment_end"

*TF_REQTIME.class: textField
*TF_REQTIME.static: true
*TF_REQTIME.name: TF_REQTIME
*TF_REQTIME.parent: DARManager
*TF_REQTIME.isCompound: "true"
*TF_REQTIME.compoundIcon: "textfield.xpm"
*TF_REQTIME.compoundName: "text_Field"
*TF_REQTIME.x: 375
*TF_REQTIME.y: 333
*TF_REQTIME.height: 32
*TF_REQTIME.columns: 21
*TF_REQTIME.cursorPositionVisible: "false"
*TF_REQTIME.editable: "false"
*TF_REQTIME.resizeWidth: "false"
*TF_REQTIME.text: ""
*TF_REQTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_REQTIME.traversalOn: "false"

*separator13.class: separator
*separator13.static: true
*separator13.name: separator13
*separator13.parent: DARManager
*separator13.width: 744
*separator13.height: 15
*separator13.isCompound: "true"
*separator13.compoundIcon: "sep.xpm"
*separator13.compoundName: "separator_"
*separator13.x: 10
*separator13.y: 310

*TF_DAR_REV.class: textField
*TF_DAR_REV.static: true
*TF_DAR_REV.name: TF_DAR_REV
*TF_DAR_REV.parent: DARManager
*TF_DAR_REV.isCompound: "true"
*TF_DAR_REV.compoundIcon: "textfield.xpm"
*TF_DAR_REV.compoundName: "text_Field"
*TF_DAR_REV.x: 216
*TF_DAR_REV.y: 484
*TF_DAR_REV.height: 32
*TF_DAR_REV.columns: 6
*TF_DAR_REV.cursorPositionVisible: "false"
*TF_DAR_REV.editable: "false"
*TF_DAR_REV.resizeWidth: "false"
*TF_DAR_REV.text: ""
*TF_DAR_REV.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_REV.traversalOn: "false"

*label128.class: label
*label128.static: true
*label128.name: label128
*label128.parent: DARManager
*label128.isCompound: "true"
*label128.compoundIcon: "label.xpm"
*label128.compoundName: "label_"
*label128.x: 186
*label128.y: 484
*label128.width: 30
*label128.height: 30
*label128.labelString: "REV:"
*label128.alignment: "alignment_end"

*label100.class: label
*label100.static: true
*label100.name: label100
*label100.parent: DARManager
*label100.isCompound: "true"
*label100.compoundIcon: "label.xpm"
*label100.compoundName: "label_"
*label100.x: 26
*label100.y: 406
*label100.height: 30
*label100.labelString: "START TIME:"
*label100.alignment: "alignment_end"

*label104.class: label
*label104.static: true
*label104.name: label104
*label104.parent: DARManager
*label104.isCompound: "true"
*label104.compoundIcon: "label.xpm"
*label104.compoundName: "label_"
*label104.x: 32
*label104.y: 443
*label104.height: 30
*label104.labelString: "STOP TIME:"
*label104.alignment: "alignment_end"

*T_PLNRCMNT.class: text
*T_PLNRCMNT.static: true
*T_PLNRCMNT.name: T_PLNRCMNT
*T_PLNRCMNT.parent: DARManager
*T_PLNRCMNT.x: 375
*T_PLNRCMNT.y: 656
*T_PLNRCMNT.width: 300
*T_PLNRCMNT.columns: 200
*T_PLNRCMNT.editMode: "multi_line_edit"
*T_PLNRCMNT.maxLength: 255
*T_PLNRCMNT.rows: 4
*T_PLNRCMNT.text: ""
*T_PLNRCMNT.wordWrap: "true"
*T_PLNRCMNT.editable: "false"
*T_PLNRCMNT.height: 112
*T_PLNRCMNT.valueWcs: ""

*TF_NOBS.class: textField
*TF_NOBS.static: true
*TF_NOBS.name: TF_NOBS
*TF_NOBS.parent: DARManager
*TF_NOBS.x: 101
*TF_NOBS.y: 524
*TF_NOBS.height: 32
*TF_NOBS.columns: 2
*TF_NOBS.resizeWidth: "false"
*TF_NOBS.text: ""
*TF_NOBS.cursorPositionVisible: "false"
*TF_NOBS.editable: "false"
*TF_NOBS.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NOBS.maxLength: 8
*TF_NOBS.traversalOn: "false"

*label97.class: label
*label97.static: true
*label97.name: label97
*label97.parent: DARManager
*label97.isCompound: "true"
*label97.compoundIcon: "label.xpm"
*label97.compoundName: "label_"
*label97.x: 16
*label97.y: 524
*label97.width: 80
*label97.height: 30
*label97.labelString: "OBSERVATIONS/\nFREQUENCY:"
*label97.alignment: "alignment_end"

*label105.class: label
*label105.static: true
*label105.name: label105
*label105.parent: DARManager
*label105.isCompound: "true"
*label105.compoundIcon: "label.xpm"
*label105.compoundName: "label_"
*label105.x: 133
*label105.y: 524
*label105.width: 15
*label105.height: 30
*label105.labelString: "/"
*label105.alignment: "alignment_end"

*TF_ASCDSC.class: textField
*TF_ASCDSC.static: true
*TF_ASCDSC.name: TF_ASCDSC
*TF_ASCDSC.parent: DARManager
*TF_ASCDSC.isCompound: "true"
*TF_ASCDSC.compoundIcon: "textfield.xpm"
*TF_ASCDSC.compoundName: "text_Field"
*TF_ASCDSC.x: 375
*TF_ASCDSC.y: 405
*TF_ASCDSC.height: 32
*TF_ASCDSC.cursorPositionVisible: "false"
*TF_ASCDSC.editable: "false"
*TF_ASCDSC.text: ""
*TF_ASCDSC.columns: 2
*TF_ASCDSC.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_ASCDSC.resizeWidth: "false"
*TF_ASCDSC.maxLength: 2
*TF_ASCDSC.traversalOn: "false"

*label84.class: label
*label84.static: true
*label84.name: label84
*label84.parent: DARManager
*label84.isCompound: "true"
*label84.compoundIcon: "label.xpm"
*label84.compoundName: "label_"
*label84.x: 346
*label84.y: 406
*label84.height: 30
*label84.labelString: "ASC/\nDSC:"
*label84.alignment: "alignment_end"

*label96.class: label
*label96.static: true
*label96.name: label96
*label96.parent: DARManager
*label96.isCompound: "true"
*label96.compoundIcon: "label.xpm"
*label96.compoundName: "label_"
*label96.x: 47
*label96.y: 630
*label96.height: 25
*label96.labelString: " USER COMMENTS:"
*label96.alignment: "alignment_end"

*T_USERCMNT.class: text
*T_USERCMNT.static: true
*T_USERCMNT.name: T_USERCMNT
*T_USERCMNT.parent: DARManager
*T_USERCMNT.x: 50
*T_USERCMNT.y: 656
*T_USERCMNT.width: 300
*T_USERCMNT.columns: 200
*T_USERCMNT.editMode: "multi_line_edit"
*T_USERCMNT.maxLength: 255
*T_USERCMNT.rows: 4
*T_USERCMNT.text: ""
*T_USERCMNT.wordWrap: "true"
*T_USERCMNT.traversalOn: "false"
*T_USERCMNT.height: 112
*T_USERCMNT.editable: "false"
*T_USERCMNT.valueWcs: ""
*T_USERCMNT.fontList: "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-8"
*T_USERCMNT.cursorPositionVisible: "false"

*TF_REQSTAT.class: textField
*TF_REQSTAT.static: true
*TF_REQSTAT.name: TF_REQSTAT
*TF_REQSTAT.parent: DARManager
*TF_REQSTAT.isCompound: "true"
*TF_REQSTAT.compoundIcon: "textfield.xpm"
*TF_REQSTAT.compoundName: "text_Field"
*TF_REQSTAT.x: 166
*TF_REQSTAT.y: 333
*TF_REQSTAT.height: 32
*TF_REQSTAT.cursorPositionVisible: "false"
*TF_REQSTAT.editable: "false"
*TF_REQSTAT.text: ""
*TF_REQSTAT.columns: 4
*TF_REQSTAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_REQSTAT.resizeWidth: "false"
*TF_REQSTAT.maxLength: 4
*TF_REQSTAT.marginWidth: 4
*TF_REQSTAT.traversalOn: "false"

*DAR_menu_status.class: rowColumn
*DAR_menu_status.static: true
*DAR_menu_status.name: DAR_menu_status
*DAR_menu_status.parent: DARManager
*DAR_menu_status.rowColumnType: "menu_option"
*DAR_menu_status.subMenuId: "DAR_status_options"
*DAR_menu_status.isCompound: "true"
*DAR_menu_status.compoundIcon: "optionM.xpm"
*DAR_menu_status.compoundName: "option_Menu"
*DAR_menu_status.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*DAR_menu_status.x: 374
*DAR_menu_status.y: 623
*DAR_menu_status.width: 215
*DAR_menu_status.height: 35
*DAR_menu_status.labelString: "PLANNER COMMENTS/STATUS:"

*DAR_status_options.class: rowColumn
*DAR_status_options.static: true
*DAR_status_options.name: DAR_status_options
*DAR_status_options.parent: DAR_menu_status
*DAR_status_options.rowColumnType: "menu_pulldown"
*DAR_status_options.labelString: ""
*DAR_status_options.x: 0
*DAR_status_options.y: 545
*DAR_status_options.mappedWhenManaged: "true"

*DAR_status_QUE.class: pushButton
*DAR_status_QUE.static: true
*DAR_status_QUE.name: DAR_status_QUE
*DAR_status_QUE.parent: DAR_status_options
*DAR_status_QUE.labelString: "QUE"
*DAR_status_QUE.fontList: "rockwell-bold"
*DAR_status_QUE.x: 2
*DAR_status_QUE.y: 545

*DAR_status_PLN.class: pushButton
*DAR_status_PLN.static: true
*DAR_status_PLN.name: DAR_status_PLN
*DAR_status_PLN.parent: DAR_status_options
*DAR_status_PLN.labelString: "PLN"
*DAR_status_PLN.fontList: "rockwell-bold"

*pushButton_EditDAR.class: pushButton
*pushButton_EditDAR.static: true
*pushButton_EditDAR.name: pushButton_EditDAR
*pushButton_EditDAR.parent: DARManager
*pushButton_EditDAR.isCompound: "true"
*pushButton_EditDAR.compoundIcon: "push.xpm"
*pushButton_EditDAR.compoundName: "push_Button"
*pushButton_EditDAR.x: 29
*pushButton_EditDAR.y: 775
*pushButton_EditDAR.width: 90
*pushButton_EditDAR.height: 40
*pushButton_EditDAR.labelString: "EDIT"
*pushButton_EditDAR.fontList: "rockwell-bold"
*pushButton_EditDAR.activateCallback.source: public
*pushButton_EditDAR.activateCallback: cb_set_darmanager_editability
*pushButton_EditDAR.activateCallbackClientData: (XtPointer) True

*pushButton_SearchDAR.class: pushButton
*pushButton_SearchDAR.static: true
*pushButton_SearchDAR.name: pushButton_SearchDAR
*pushButton_SearchDAR.parent: DARManager
*pushButton_SearchDAR.isCompound: "true"
*pushButton_SearchDAR.compoundIcon: "push.xpm"
*pushButton_SearchDAR.compoundName: "push_Button"
*pushButton_SearchDAR.x: 105
*pushButton_SearchDAR.y: 265
*pushButton_SearchDAR.labelString: "SEARCH"
*pushButton_SearchDAR.fontList: "rockwell-bold"
*pushButton_SearchDAR.height: 38
*pushButton_SearchDAR.width: 80

*menuBar_DELETE.class: rowColumn
*menuBar_DELETE.static: true
*menuBar_DELETE.name: menuBar_DELETE
*menuBar_DELETE.parent: DARManager
*menuBar_DELETE.rowColumnType: "menu_bar"
*menuBar_DELETE.isCompound: "true"
*menuBar_DELETE.compoundIcon: "pulldownM.xpm"
*menuBar_DELETE.compoundName: "menu_Bar"
*menuBar_DELETE.x: 133
*menuBar_DELETE.y: 775
*menuBar_DELETE.width: 90
*menuBar_DELETE.height: 40
*menuBar_DELETE.resizable: "true"
*menuBar_DELETE.marginHeight: 5
*menuBar_DELETE.marginWidth: 8
*menuBar_DELETE.menuAccelerator: "<KeyUp>F10"

*menuBar_p1.class: rowColumn
*menuBar_p1.static: true
*menuBar_p1.name: menuBar_p1
*menuBar_p1.parent: menuBar_DELETE
*menuBar_p1.rowColumnType: "menu_pulldown"
*menuBar_p1.height: 40
*menuBar_p1.width: 90
*menuBar_p1.x: 0
*menuBar_p1.y: 704

*menuBar_p_b_REJECTED.class: pushButton
*menuBar_p_b_REJECTED.static: true
*menuBar_p_b_REJECTED.name: menuBar_p_b_REJECTED
*menuBar_p_b_REJECTED.parent: menuBar_p1
*menuBar_p_b_REJECTED.labelString: "REJECTED"
*menuBar_p_b_REJECTED.x: 2
*menuBar_p_b_REJECTED.y: 729
*menuBar_p_b_REJECTED.activateCallback.source: public
*menuBar_p_b_REJECTED.activateCallback: cb_delete_dar_record
*menuBar_p_b_REJECTED.activateCallbackClientData: (XtPointer) APS_REJECTED_STATUS

*menuBar_p_b_COMPLETED.class: pushButton
*menuBar_p_b_COMPLETED.static: true
*menuBar_p_b_COMPLETED.name: menuBar_p_b_COMPLETED
*menuBar_p_b_COMPLETED.parent: menuBar_p1
*menuBar_p_b_COMPLETED.labelString: "COMPLETED"
*menuBar_p_b_COMPLETED.activateCallback.source: public
*menuBar_p_b_COMPLETED.activateCallback: cb_delete_dar_record
*menuBar_p_b_COMPLETED.activateCallbackClientData: (XtPointer) APS_COMPLETED_STATUS
*menuBar_p_b_COMPLETED.x: 2
*menuBar_p_b_COMPLETED.y: 729

*menuBar_top_b1.class: cascadeButton
*menuBar_top_b1.static: true
*menuBar_top_b1.name: menuBar_top_b1
*menuBar_top_b1.parent: menuBar_DELETE
*menuBar_top_b1.labelString: "DELETE"
*menuBar_top_b1.subMenuId: "menuBar_p1"
*menuBar_top_b1.fontList: "rockwell-bold"
*menuBar_top_b1.height: 40
*menuBar_top_b1.width: 90
*menuBar_top_b1.x: 10
*menuBar_top_b1.y: 728
*menuBar_top_b1.marginHeight: 5

*TF_DAR_sortclause.class: textField
*TF_DAR_sortclause.static: true
*TF_DAR_sortclause.name: TF_DAR_sortclause
*TF_DAR_sortclause.parent: DARManager
*TF_DAR_sortclause.isCompound: "true"
*TF_DAR_sortclause.compoundIcon: "textfield.xpm"
*TF_DAR_sortclause.compoundName: "text_Field"
*TF_DAR_sortclause.x: 455
*TF_DAR_sortclause.y: 268
*TF_DAR_sortclause.height: 32
*TF_DAR_sortclause.cursorPositionVisible: "false"
*TF_DAR_sortclause.editable: "false"
*TF_DAR_sortclause.text: "reqtime desc"
*TF_DAR_sortclause.columns: 256
*TF_DAR_sortclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_sortclause.resizeWidth: "false"
*TF_DAR_sortclause.width: 180
*TF_DAR_sortclause.valueChangedCallback.source: public
*TF_DAR_sortclause.valueChangedCallback: cb_show_dar_records
*TF_DAR_sortclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DARS
*TF_DAR_sortclause.traversalOn: "false"

*TF_DAR_recordcount.class: textField
*TF_DAR_recordcount.static: true
*TF_DAR_recordcount.name: TF_DAR_recordcount
*TF_DAR_recordcount.parent: DARManager
*TF_DAR_recordcount.isCompound: "true"
*TF_DAR_recordcount.compoundIcon: "textfield.xpm"
*TF_DAR_recordcount.compoundName: "text_Field"
*TF_DAR_recordcount.x: 680
*TF_DAR_recordcount.y: 269
*TF_DAR_recordcount.height: 30
*TF_DAR_recordcount.columns: 5
*TF_DAR_recordcount.cursorPositionVisible: "false"
*TF_DAR_recordcount.editable: "false"
*TF_DAR_recordcount.resizeWidth: "false"
*TF_DAR_recordcount.text: "0"
*TF_DAR_recordcount.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_recordcount.traversalOn: "false"

*label108.class: label
*label108.static: true
*label108.name: label108
*label108.parent: DARManager
*label108.isCompound: "true"
*label108.compoundIcon: "label.xpm"
*label108.compoundName: "label_"
*label108.x: 640
*label108.y: 269
*label108.height: 30
*label108.labelString: "RECORD\nCOUNT:"
*label108.alignment: "alignment_end"

*pushButton_SaveDARChanges.class: pushButton
*pushButton_SaveDARChanges.static: true
*pushButton_SaveDARChanges.name: pushButton_SaveDARChanges
*pushButton_SaveDARChanges.parent: DARManager
*pushButton_SaveDARChanges.isCompound: "true"
*pushButton_SaveDARChanges.compoundIcon: "push.xpm"
*pushButton_SaveDARChanges.compoundName: "push_Button"
*pushButton_SaveDARChanges.x: 369
*pushButton_SaveDARChanges.y: 775
*pushButton_SaveDARChanges.width: 90
*pushButton_SaveDARChanges.height: 40
*pushButton_SaveDARChanges.labelString: "SAVE\nCHANGES"
*pushButton_SaveDARChanges.fontList: "rockwell-bold"
*pushButton_SaveDARChanges.activateCallback.source: public
*pushButton_SaveDARChanges.activateCallback: cb_save_dar_changes
*pushButton_SaveDARChanges.activateCallbackClientData: (XtPointer) False
*pushButton_SaveDARChanges.createManaged: "false"

*menuBar_LOAD_DAR.class: rowColumn
*menuBar_LOAD_DAR.static: true
*menuBar_LOAD_DAR.name: menuBar_LOAD_DAR
*menuBar_LOAD_DAR.parent: DARManager
*menuBar_LOAD_DAR.rowColumnType: "menu_bar"
*menuBar_LOAD_DAR.isCompound: "true"
*menuBar_LOAD_DAR.compoundIcon: "pulldownM.xpm"
*menuBar_LOAD_DAR.compoundName: "menu_Bar"
*menuBar_LOAD_DAR.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*menuBar_LOAD_DAR.x: 12
*menuBar_LOAD_DAR.y: 267
*menuBar_LOAD_DAR.menuAccelerator: "<KeyUp>F10"
*menuBar_LOAD_DAR.packing: "pack_tight"
*menuBar_LOAD_DAR.resizeHeight: "false"
*menuBar_LOAD_DAR.resizeWidth: "false"
*menuBar_LOAD_DAR.entryAlignment: "alignment_center"
*menuBar_LOAD_DAR.marginHeight: 2
*menuBar_LOAD_DAR.marginWidth: 0
*menuBar_LOAD_DAR.width: 85
*menuBar_LOAD_DAR.height: 35

*menuBar_FILE.class: rowColumn
*menuBar_FILE.static: true
*menuBar_FILE.name: menuBar_FILE
*menuBar_FILE.parent: menuBar_LOAD_DAR
*menuBar_FILE.rowColumnType: "menu_pulldown"
*menuBar_FILE.packing: "pack_column"
*menuBar_FILE.width: 200
*menuBar_FILE.entryAlignment: "alignment_center"
*menuBar_FILE.x: 0
*menuBar_FILE.y: 249

*pushButton_LOAD_DARS.class: pushButton
*pushButton_LOAD_DARS.static: true
*pushButton_LOAD_DARS.name: pushButton_LOAD_DARS
*pushButton_LOAD_DARS.parent: menuBar_FILE
*pushButton_LOAD_DARS.labelString: "LOAD IMS DARS"
*pushButton_LOAD_DARS.activateCallback.source: public
*pushButton_LOAD_DARS.activateCallback: cb_load_dars_from_ims
*pushButton_LOAD_DARS.activateCallbackClientData: (XtPointer) LOAD_DARS_APPEND
*pushButton_LOAD_DARS.fontList: "rockwell-bold"
*pushButton_LOAD_DARS.x: 2
*pushButton_LOAD_DARS.y: 273

*cascadeButton_SAVE_DAR_RPT.class: cascadeButton
*cascadeButton_SAVE_DAR_RPT.static: true
*cascadeButton_SAVE_DAR_RPT.name: cascadeButton_SAVE_DAR_RPT
*cascadeButton_SAVE_DAR_RPT.parent: menuBar_FILE
*cascadeButton_SAVE_DAR_RPT.labelString: "SAVE DAR REPORT"
*cascadeButton_SAVE_DAR_RPT.subMenuId: "PANE_SAVE_DAR_RPT"
*cascadeButton_SAVE_DAR_RPT.fontList: "rockwell-bold"
*cascadeButton_SAVE_DAR_RPT.x: 2
*cascadeButton_SAVE_DAR_RPT.y: 273

*cascadeButton_PRINT_DAR_RPT.class: cascadeButton
*cascadeButton_PRINT_DAR_RPT.static: true
*cascadeButton_PRINT_DAR_RPT.name: cascadeButton_PRINT_DAR_RPT
*cascadeButton_PRINT_DAR_RPT.parent: menuBar_FILE
*cascadeButton_PRINT_DAR_RPT.labelString: "PRINT DAR REPORT"
*cascadeButton_PRINT_DAR_RPT.subMenuId: "PANE_PRINT_DAR_RPT"
*cascadeButton_PRINT_DAR_RPT.fontList: "rockwell-bold"
*cascadeButton_PRINT_DAR_RPT.x: 2
*cascadeButton_PRINT_DAR_RPT.y: 273

*PANE_PRINT_DAR_RPT.class: rowColumn
*PANE_PRINT_DAR_RPT.static: true
*PANE_PRINT_DAR_RPT.name: PANE_PRINT_DAR_RPT
*PANE_PRINT_DAR_RPT.parent: menuBar_FILE
*PANE_PRINT_DAR_RPT.rowColumnType: "menu_pulldown"
*PANE_PRINT_DAR_RPT.x: 0
*PANE_PRINT_DAR_RPT.y: 250

*PANE_PRINT_DARS_b4.class: separatorGadget
*PANE_PRINT_DARS_b4.static: true
*PANE_PRINT_DARS_b4.name: PANE_PRINT_DARS_b4
*PANE_PRINT_DARS_b4.parent: PANE_PRINT_DAR_RPT
*PANE_PRINT_DARS_b4.x: 2
*PANE_PRINT_DARS_b4.y: 283

*PRINT_SELECTED_DAR.class: pushButton
*PRINT_SELECTED_DAR.static: true
*PRINT_SELECTED_DAR.name: PRINT_SELECTED_DAR
*PRINT_SELECTED_DAR.parent: PANE_PRINT_DAR_RPT
*PRINT_SELECTED_DAR.labelString: "SELECTED DAR"
*PRINT_SELECTED_DAR.activateCallback.source: public
*PRINT_SELECTED_DAR.activateCallback: cb_print_dars
*PRINT_SELECTED_DAR.x: 2
*PRINT_SELECTED_DAR.y: 274
*PRINT_SELECTED_DAR.activateCallbackClientData: (XtPointer) PRINT_DARS_SELECTED

*PRINT_CURRENT_DARS.class: pushButton
*PRINT_CURRENT_DARS.static: true
*PRINT_CURRENT_DARS.name: PRINT_CURRENT_DARS
*PRINT_CURRENT_DARS.parent: PANE_PRINT_DAR_RPT
*PRINT_CURRENT_DARS.labelString: "CURRENT DARS"
*PRINT_CURRENT_DARS.activateCallback.source: public
*PRINT_CURRENT_DARS.activateCallback: cb_print_dars
*PRINT_CURRENT_DARS.x: 2
*PRINT_CURRENT_DARS.y: 274
*PRINT_CURRENT_DARS.activateCallbackClientData: (XtPointer) PRINT_DARS_CURRENT

*PRINT_ALL_DARS.class: pushButton
*PRINT_ALL_DARS.static: true
*PRINT_ALL_DARS.name: PRINT_ALL_DARS
*PRINT_ALL_DARS.parent: PANE_PRINT_DAR_RPT
*PRINT_ALL_DARS.labelString: "ALL DARS"
*PRINT_ALL_DARS.activateCallback.source: public
*PRINT_ALL_DARS.activateCallback: cb_print_dars
*PRINT_ALL_DARS.x: 2
*PRINT_ALL_DARS.y: 274
*PRINT_ALL_DARS.activateCallbackClientData: (XtPointer) PRINT_DARS_ALL

*PANE_SAVE_DAR_RPT.class: rowColumn
*PANE_SAVE_DAR_RPT.static: true
*PANE_SAVE_DAR_RPT.name: PANE_SAVE_DAR_RPT
*PANE_SAVE_DAR_RPT.parent: menuBar_FILE
*PANE_SAVE_DAR_RPT.rowColumnType: "menu_pulldown"
*PANE_SAVE_DAR_RPT.x: 0
*PANE_SAVE_DAR_RPT.y: 249

*RPT_SELECTED_DAR.class: pushButton
*RPT_SELECTED_DAR.static: true
*RPT_SELECTED_DAR.name: RPT_SELECTED_DAR
*RPT_SELECTED_DAR.parent: PANE_SAVE_DAR_RPT
*RPT_SELECTED_DAR.labelString: "SELECTED DAR"
*RPT_SELECTED_DAR.fontList: "rockwell-bold"
*RPT_SELECTED_DAR.activateCallback.source: public
*RPT_SELECTED_DAR.activateCallback: cb_set_print_dars_to_file_cb
*RPT_SELECTED_DAR.activateCallbackClientData: (XtPointer) PRINT_DARS_SELECTED_TO_FILE
*RPT_SELECTED_DAR.x: 2
*RPT_SELECTED_DAR.y: 273

*RPT_CURRENT_DARS.class: pushButton
*RPT_CURRENT_DARS.static: true
*RPT_CURRENT_DARS.name: RPT_CURRENT_DARS
*RPT_CURRENT_DARS.parent: PANE_SAVE_DAR_RPT
*RPT_CURRENT_DARS.labelString: "CURRENT DARS"
*RPT_CURRENT_DARS.fontList: "rockwell-bold"
*RPT_CURRENT_DARS.activateCallback.source: public
*RPT_CURRENT_DARS.activateCallback: cb_set_print_dars_to_file_cb
*RPT_CURRENT_DARS.activateCallbackClientData: (XtPointer) PRINT_DARS_CURRENT_TO_FILE
*RPT_CURRENT_DARS.x: 2
*RPT_CURRENT_DARS.y: 273

*RPT_ALL_DARS.class: pushButton
*RPT_ALL_DARS.static: true
*RPT_ALL_DARS.name: RPT_ALL_DARS
*RPT_ALL_DARS.parent: PANE_SAVE_DAR_RPT
*RPT_ALL_DARS.labelString: "ALL DARS"
*RPT_ALL_DARS.fontList: "rockwell-bold"
*RPT_ALL_DARS.activateCallback.source: public
*RPT_ALL_DARS.activateCallback: cb_set_print_dars_to_file_cb
*RPT_ALL_DARS.activateCallbackClientData: (XtPointer) PRINT_DARS_ALL_TO_FILE
*RPT_ALL_DARS.x: 2
*RPT_ALL_DARS.y: 273

*menuBar_top_b4.class: cascadeButton
*menuBar_top_b4.static: true
*menuBar_top_b4.name: menuBar_top_b4
*menuBar_top_b4.parent: menuBar_LOAD_DAR
*menuBar_top_b4.labelString: "FILE"
*menuBar_top_b4.subMenuId: "menuBar_FILE"
*menuBar_top_b4.fontList: "rockwell-bold"
*menuBar_top_b4.x: 2
*menuBar_top_b4.y: 270
*menuBar_top_b4.alignment: "alignment_center"
*menuBar_top_b4.recomputeSize: "false"
*menuBar_top_b4.width: 80
*menuBar_top_b4.marginWidth: 0
*menuBar_top_b4.marginHeight: 0
*menuBar_top_b4.height: 25

*pushButton_CancelDARChanges.class: pushButton
*pushButton_CancelDARChanges.static: true
*pushButton_CancelDARChanges.name: pushButton_CancelDARChanges
*pushButton_CancelDARChanges.parent: DARManager
*pushButton_CancelDARChanges.isCompound: "true"
*pushButton_CancelDARChanges.compoundIcon: "push.xpm"
*pushButton_CancelDARChanges.compoundName: "push_Button"
*pushButton_CancelDARChanges.x: 499
*pushButton_CancelDARChanges.y: 775
*pushButton_CancelDARChanges.width: 90
*pushButton_CancelDARChanges.height: 40
*pushButton_CancelDARChanges.labelString: "CANCEL\nCHANGES"
*pushButton_CancelDARChanges.fontList: "rockwell-bold"
*pushButton_CancelDARChanges.activateCallback.source: public
*pushButton_CancelDARChanges.activateCallback: cb_save_dar_changes
*pushButton_CancelDARChanges.activateCallbackClientData: (XtPointer) True
*pushButton_CancelDARChanges.createManaged: "false"

*TF_DAR_searchclause.class: textField
*TF_DAR_searchclause.static: true
*TF_DAR_searchclause.name: TF_DAR_searchclause
*TF_DAR_searchclause.parent: DARManager
*TF_DAR_searchclause.isCompound: "true"
*TF_DAR_searchclause.compoundIcon: "textfield.xpm"
*TF_DAR_searchclause.compoundName: "text_Field"
*TF_DAR_searchclause.x: 185
*TF_DAR_searchclause.y: 268
*TF_DAR_searchclause.height: 32
*TF_DAR_searchclause.cursorPositionVisible: "false"
*TF_DAR_searchclause.editable: "false"
*TF_DAR_searchclause.text: "where darid > 0"
*TF_DAR_searchclause.columns: 256
*TF_DAR_searchclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DAR_searchclause.resizeWidth: "false"
*TF_DAR_searchclause.width: 180
*TF_DAR_searchclause.valueChangedCallback.source: public
*TF_DAR_searchclause.valueChangedCallback: cb_show_dar_records
*TF_DAR_searchclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DARS
*TF_DAR_searchclause.traversalOn: "false"

*scrolledWindowText7.class: scrolledWindow
*scrolledWindowText7.static: true
*scrolledWindowText7.name: scrolledWindowText7
*scrolledWindowText7.parent: DARManager
*scrolledWindowText7.scrollingPolicy: "application_defined"
*scrolledWindowText7.visualPolicy: "variable"
*scrolledWindowText7.scrollBarDisplayPolicy: "static"
*scrolledWindowText7.isCompound: "true"
*scrolledWindowText7.compoundIcon: "scrltext.xpm"
*scrolledWindowText7.compoundName: "scrolled_Text"
*scrolledWindowText7.x: 770
*scrolledWindowText7.y: 35

*scrolledWindowText8.class: scrolledWindow
*scrolledWindowText8.static: true
*scrolledWindowText8.name: scrolledWindowText8
*scrolledWindowText8.parent: DARManager
*scrolledWindowText8.scrollingPolicy: "application_defined"
*scrolledWindowText8.visualPolicy: "variable"
*scrolledWindowText8.scrollBarDisplayPolicy: "static"
*scrolledWindowText8.isCompound: "true"
*scrolledWindowText8.compoundIcon: "scrltext.xpm"
*scrolledWindowText8.compoundName: "scrolled_Text"
*scrolledWindowText8.x: 770
*scrolledWindowText8.y: 125

*pushButton2.class: pushButton
*pushButton2.static: true
*pushButton2.name: pushButton2
*pushButton2.parent: DARManager
*pushButton2.x: 20
*pushButton2.y: 100
*pushButton2.width: 30
*pushButton2.height: 130
*pushButton2.fontList: "rockwell-bold"
*pushButton2.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton2.activateCallback.source: public
*pushButton2.activateCallback: cb_show_dar_records
*pushButton2.activateCallbackClientData: (XtPointer) scrolledList_DARS

*label125.class: label
*label125.static: true
*label125.name: label125
*label125.parent: DARManager
*label125.isCompound: "true"
*label125.compoundIcon: "label.xpm"
*label125.compoundName: "label_"
*label125.x: 50
*label125.y: 334
*label125.height: 30
*label125.labelString: "DAR ID/\nSTATUS:"
*label125.alignment: "alignment_end"

*label126.class: label
*label126.static: true
*label126.name: label126
*label126.parent: DARManager
*label126.isCompound: "true"
*label126.compoundIcon: "label.xpm"
*label126.compoundName: "label_"
*label126.x: 592
*label126.y: 334
*label126.width: 30
*label126.height: 30
*label126.labelString: "USER\n ID "
*label126.alignment: "alignment_end"

*label49.class: label
*label49.static: true
*label49.name: label49
*label49.parent: DARManager
*label49.isCompound: "true"
*label49.compoundIcon: "label.xpm"
*label49.compoundName: "label_"
*label49.x: 31
*label49.y: 365
*label49.height: 30
*label49.labelString: "QUICKLOOK:"
*label49.alignment: "alignment_end"
*label49.width: 65

*LABEL_QUICKLOOK.class: label
*LABEL_QUICKLOOK.static: true
*LABEL_QUICKLOOK.name: LABEL_QUICKLOOK
*LABEL_QUICKLOOK.parent: DARManager
*LABEL_QUICKLOOK.isCompound: "true"
*LABEL_QUICKLOOK.compoundIcon: "label.xpm"
*LABEL_QUICKLOOK.compoundName: "label_"
*LABEL_QUICKLOOK.x: 97
*LABEL_QUICKLOOK.y: 370
*LABEL_QUICKLOOK.height: 20
*LABEL_QUICKLOOK.labelString: "No"
*LABEL_QUICKLOOK.alignment: "alignment_end"
*LABEL_QUICKLOOK.width: 30
*LABEL_QUICKLOOK.recomputeSize: "false"

*T_FOBS.class: text
*T_FOBS.static: true
*T_FOBS.name: T_FOBS
*T_FOBS.parent: DARManager
*T_FOBS.width: 168
*T_FOBS.isCompound: "true"
*T_FOBS.compoundIcon: "text.xpm"
*T_FOBS.compoundName: "text_"
*T_FOBS.x: 149
*T_FOBS.y: 524
*T_FOBS.height: 45
*T_FOBS.cursorPositionVisible: "false"
*T_FOBS.editMode: "multi_line_edit"
*T_FOBS.editable: "false"
*T_FOBS.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*T_FOBS.text: ""
*T_FOBS.traversalOn: "false"
*T_FOBS.wordWrap: "true"
*T_FOBS.valueWcs: ""
*T_FOBS.navigationType: "tab_group"

*label79.class: label
*label79.static: true
*label79.name: label79
*label79.isCompound: "true"
*label79.compoundIcon: "label.xpm"
*label79.compoundName: "label_"
*label79.x: 4
*label79.y: 573
*label79.width: 91
*label79.height: 47
*label79.labelString: "J1 OBSERVATION\nFREQUENCY\n(IN DAYS):"
*label79.alignment: "alignment_end"
*label79.parent: DARManager

*TF_J1OBS.class: textField
*TF_J1OBS.static: true
*TF_J1OBS.name: TF_J1OBS
*TF_J1OBS.x: 100
*TF_J1OBS.y: 580
*TF_J1OBS.height: 32
*TF_J1OBS.columns: 8
*TF_J1OBS.resizeWidth: "false"
*TF_J1OBS.text: ""
*TF_J1OBS.cursorPositionVisible: "true"
*TF_J1OBS.editable: "false"
*TF_J1OBS.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_J1OBS.maxLength: 8
*TF_J1OBS.traversalOn: "true"
*TF_J1OBS.parent: DARManager
*TF_J1OBS.width: 83

