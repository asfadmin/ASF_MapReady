! UIMX ascii 2.9 key: 1683                                                      

*DTKManager.class: form
*DTKManager.classinc:
*DTKManager.classspec:
*DTKManager.classmembers:
*DTKManager.classconstructor:
*DTKManager.classdestructor:
*DTKManager.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)DTKManager.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.DTKManager.i"\
\
#include <stdlib.h>\
\
#include <Xm/FileSB.h>\
#include <Xm/DialogS.h>\
\
#include "db_sybint.h"\
#include "aps_db_table.h"\
\
#include "gui_utils.h"\
#include "satmenus.h"\
#include "cb_datetime.h"\
#include "cb_sortform.h"\
#include "cb_searchform.h"\
#include "cb_dtkmanager.h"\
\
extern void cb_format_float() ;\
\
extern Widget DTK_manager;\
extern Widget filebox ;\

*DTKManager.ispecdecl:
*DTKManager.funcdecl: swidget create_DTKManager(swidget UxParent)
*DTKManager.funcname: create_DTKManager
*DTKManager.funcdef: "swidget", "<create_DTKManager>(%)"
*DTKManager.argdecl: swidget UxParent;
*DTKManager.arglist: UxParent
*DTKManager.arglist.UxParent: "swidget", "%UxParent%"
*DTKManager.icode: SORT_INFO *sortinfo ;\
SEARCH_INFO *searchinfo ;\
OPTION_MENU_WIDGETS *sensor_menu ;\
ANTENNA_CLIENT_DATA *antenna_menu ;\

*DTKManager.fcode: sortinfo = (SORT_INFO *) malloc(sizeof(SORT_INFO)) ;\
searchinfo = (SEARCH_INFO *) malloc(sizeof(SEARCH_INFO)) ;\
sensor_menu = (OPTION_MENU_WIDGETS *) malloc(sizeof(OPTION_MENU_WIDGETS)) ;\
antenna_menu = (ANTENNA_CLIENT_DATA *) malloc(sizeof(ANTENNA_CLIENT_DATA)) ;\
\
sortinfo->table_name = APS_TABLE(DTK) ;\
sortinfo->field_to_update = (Widget) TF_DTK_sortclause ;\
\
searchinfo->table_name = APS_TABLE(DTK) ;\
searchinfo->field_to_update  = (Widget) TF_DTK_searchclause ;\
sensor_menu->optionmenu = (Widget) optionMenu_sensor ;\
sensor_menu->submenu = (Widget) subMenu_sensor ;\
antenna_menu->noAntenna_flag = True ;\
antenna_menu->menuWidgets.optionmenu = (Widget) optionMenu_antenna ;\
antenna_menu->menuWidgets.submenu = (Widget) subMenu_antenna ;\
\
XtAddCallback( pushButton_SortDTK, XmNactivateCallback,\
    (XtCallbackProc) cb_edit_sort_columns,\
    (XtPointer) sortinfo );\
 \
XtAddCallback( pushButton_SearchDTK, XmNactivateCallback,\
    (XtCallbackProc) cb_edit_search_columns,\
    (XtPointer) searchinfo );\
\
XtAddCallback( subMenu_sat, XmNentryCallback,\
	(XtCallbackProc) cb_set_sensor_menus,\
	(XtPointer ) sensor_menu );\
\
XtAddCallback( subMenu_sat, XmNentryCallback,\
	(XtCallbackProc) cb_set_dtk_status_menus,\
	(XtPointer ) NULL );\
\
XtAddCallback( subMenu_dtkm_stnid, XmNentryCallback,\
	(XtCallbackProc) cb_set_antenna_menus,\
	(XtPointer ) antenna_menu );\
\
XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
	cb_show_dtk_records, (XtPointer *) scrolledList_DTKS) ;\
\
cb_init_dtk_search_clause( TF_DTK_searchclause, NULL, NULL ) ;\
\
XtCallActionProc( XtNameToWidget(subMenu_sat, "ERS-1" ),\
        "ArmAndActivate", NULL, NULL, 0 );\
\
return(rtrn);\

*DTKManager.auxdecl:
*DTKManager.name.source: public
*DTKManager.static: false
*DTKManager.name: DTKManager
*DTKManager.parent: NO_PARENT
*DTKManager.parentExpression: UxParent
*DTKManager.defaultShell: topLevelShell
*DTKManager.height: 752
*DTKManager.resizePolicy: "resize_none"
*DTKManager.isCompound: "true"
*DTKManager.compoundIcon: "form.xpm"
*DTKManager.compoundName: "form_"
*DTKManager.unitType: "pixels"
*DTKManager.dialogTitle: "APS:DTK Manager"
*DTKManager.width: 887
*DTKManager.y: 94
*DTKManager.x: 246

*label148.class: label
*label148.static: true
*label148.name: label148
*label148.parent: DTKManager
*label148.isCompound: "true"
*label148.compoundIcon: "label.xpm"
*label148.compoundName: "label_"
*label148.x: 17
*label148.y: 9
*label148.width: 828
*label148.height: 35
*label148.labelString: "DOWN-LINK  and  DATA-TAKE  MANAGER"
*label148.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindowList_DTKS.class: scrolledWindow
*scrolledWindowList_DTKS.static: true
*scrolledWindowList_DTKS.name: scrolledWindowList_DTKS
*scrolledWindowList_DTKS.parent: DTKManager
*scrolledWindowList_DTKS.scrollingPolicy: "application_defined"
*scrolledWindowList_DTKS.visualPolicy: "variable"
*scrolledWindowList_DTKS.scrollBarDisplayPolicy: "static"
*scrolledWindowList_DTKS.shadowThickness: 0
*scrolledWindowList_DTKS.isCompound: "true"
*scrolledWindowList_DTKS.compoundIcon: "scrllist.xpm"
*scrolledWindowList_DTKS.compoundName: "scrolled_List"
*scrolledWindowList_DTKS.x: 73
*scrolledWindowList_DTKS.y: 79
*scrolledWindowList_DTKS.width: 770

*scrolledList_DTKS.class: scrolledList
*scrolledList_DTKS.static: true
*scrolledList_DTKS.name: scrolledList_DTKS
*scrolledList_DTKS.parent: scrolledWindowList_DTKS
*scrolledList_DTKS.width: 770
*scrolledList_DTKS.height: 144
*scrolledList_DTKS.itemCount: 1
*scrolledList_DTKS.items: "RADARSAT/SAR  4500  1 1994:161  1994:266:11:22:33  1994:109:11:22:33  RAG   078106/018/000001"
*scrolledList_DTKS.browseSelectionCallback.source: public
*scrolledList_DTKS.browseSelectionCallback: cb_update_dtkmanager_form
*scrolledList_DTKS.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_DTKS.selectionPolicy: "browse_select"
*scrolledList_DTKS.listSizePolicy: "constant"
*scrolledList_DTKS.x: 0
*scrolledList_DTKS.y: 89
*scrolledList_DTKS.visibleItemCount: 9

*TF_DTKID.class: textField
*TF_DTKID.static: true
*TF_DTKID.name: TF_DTKID
*TF_DTKID.parent: DTKManager
*TF_DTKID.isCompound: "true"
*TF_DTKID.compoundIcon: "textfield.xpm"
*TF_DTKID.compoundName: "text_Field"
*TF_DTKID.x: 528
*TF_DTKID.y: 324
*TF_DTKID.height: 30
*TF_DTKID.cursorPositionVisible: "false"
*TF_DTKID.editable: "false"
*TF_DTKID.sensitive: "false"
*TF_DTKID.text: "0"
*TF_DTKID.columns: 3
*TF_DTKID.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTKID.resizeWidth: "false"
*TF_DTKID.maxLength: 3

*pushButton_SearchDTK.class: pushButton
*pushButton_SearchDTK.static: true
*pushButton_SearchDTK.name: pushButton_SearchDTK
*pushButton_SearchDTK.parent: DTKManager
*pushButton_SearchDTK.isCompound: "true"
*pushButton_SearchDTK.compoundIcon: "push.xpm"
*pushButton_SearchDTK.compoundName: "push_Button"
*pushButton_SearchDTK.x: 144
*pushButton_SearchDTK.y: 260
*pushButton_SearchDTK.labelString: "SEARCH"
*pushButton_SearchDTK.fontList: "rockwell-bold"
*pushButton_SearchDTK.height: 38
*pushButton_SearchDTK.width: 80

*TF_DTK_searchclause.class: textField
*TF_DTK_searchclause.static: true
*TF_DTK_searchclause.name: TF_DTK_searchclause
*TF_DTK_searchclause.parent: DTKManager
*TF_DTK_searchclause.isCompound: "true"
*TF_DTK_searchclause.compoundIcon: "textfield.xpm"
*TF_DTK_searchclause.compoundName: "text_Field"
*TF_DTK_searchclause.x: 224
*TF_DTK_searchclause.y: 263
*TF_DTK_searchclause.height: 32
*TF_DTK_searchclause.cursorPositionVisible: "false"
*TF_DTK_searchclause.editable: "false"
*TF_DTK_searchclause.text: "where dtkid > 0"
*TF_DTK_searchclause.columns: 256
*TF_DTK_searchclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_searchclause.resizeWidth: "false"
*TF_DTK_searchclause.width: 180
*TF_DTK_searchclause.valueChangedCallback.source: public
*TF_DTK_searchclause.valueChangedCallback: cb_show_dtk_records
*TF_DTK_searchclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DTKS
*TF_DTK_searchclause.traversalOn: "false"

*pushButton_SortDTK.class: pushButton
*pushButton_SortDTK.static: true
*pushButton_SortDTK.name: pushButton_SortDTK
*pushButton_SortDTK.parent: DTKManager
*pushButton_SortDTK.isCompound: "true"
*pushButton_SortDTK.compoundIcon: "push.xpm"
*pushButton_SortDTK.compoundName: "push_Button"
*pushButton_SortDTK.x: 414
*pushButton_SortDTK.labelString: "SORT BY"
*pushButton_SortDTK.fontList: "rockwell-bold"
*pushButton_SortDTK.y: 260
*pushButton_SortDTK.height: 38
*pushButton_SortDTK.width: 80

*TF_DTK_sortclause.class: textField
*TF_DTK_sortclause.static: true
*TF_DTK_sortclause.name: TF_DTK_sortclause
*TF_DTK_sortclause.parent: DTKManager
*TF_DTK_sortclause.isCompound: "true"
*TF_DTK_sortclause.compoundIcon: "textfield.xpm"
*TF_DTK_sortclause.compoundName: "text_Field"
*TF_DTK_sortclause.x: 494
*TF_DTK_sortclause.y: 263
*TF_DTK_sortclause.height: 32
*TF_DTK_sortclause.cursorPositionVisible: "false"
*TF_DTK_sortclause.editable: "false"
*TF_DTK_sortclause.text: "strttime"
*TF_DTK_sortclause.columns: 256
*TF_DTK_sortclause.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_sortclause.resizeWidth: "false"
*TF_DTK_sortclause.width: 180
*TF_DTK_sortclause.valueChangedCallback.source: public
*TF_DTK_sortclause.valueChangedCallback: cb_show_dtk_records
*TF_DTK_sortclause.valueChangedCallbackClientData: (XtPointer) scrolledList_DTKS
*TF_DTK_sortclause.traversalOn: "false"

*label136.class: label
*label136.static: true
*label136.name: label136
*label136.parent: DTKManager
*label136.isCompound: "true"
*label136.compoundIcon: "label.xpm"
*label136.compoundName: "label_"
*label136.x: 73
*label136.y: 49
*label136.height: 20
*label136.labelString: "   SAT/SENSOR   REV ID LAST MOD     START TIME         STOP TIME    STATUS  FA DTK ID"
*label136.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label136.alignment: "alignment_beginning"
*label136.width: 710

*form_DTKquad.class: form
*form_DTKquad.static: true
*form_DTKquad.name: form_DTKquad
*form_DTKquad.parent: DTKManager
*form_DTKquad.resizePolicy: "resize_none"
*form_DTKquad.x: 429
*form_DTKquad.y: 608
*form_DTKquad.width: 445
*form_DTKquad.height: 80

*TF_NRLON1.class: textField
*TF_NRLON1.static: true
*TF_NRLON1.name: TF_NRLON1
*TF_NRLON1.parent: form_DTKquad
*TF_NRLON1.isCompound: "true"
*TF_NRLON1.compoundIcon: "textfield.xpm"
*TF_NRLON1.compoundName: "text_Field"
*TF_NRLON1.x: 135
*TF_NRLON1.y: 5
*TF_NRLON1.height: 30
*TF_NRLON1.cursorPositionVisible: "false"
*TF_NRLON1.editable: "false"
*TF_NRLON1.sensitive: "false"
*TF_NRLON1.text: ""
*TF_NRLON1.columns: 8
*TF_NRLON1.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NRLON1.resizeWidth: "false"
*TF_NRLON1.maxLength: 8

*label143.class: label
*label143.static: true
*label143.name: label143
*label143.parent: form_DTKquad
*label143.isCompound: "true"
*label143.compoundIcon: "label.xpm"
*label143.compoundName: "label_"
*label143.x: 125
*label143.y: 10
*label143.width: 15
*label143.height: 20
*label143.labelString: "/"

*label144.class: label
*label144.static: true
*label144.name: label144
*label144.parent: form_DTKquad
*label144.isCompound: "true"
*label144.compoundIcon: "label.xpm"
*label144.compoundName: "label_"
*label144.x: 235
*label144.y: 5
*label144.height: 30
*label144.labelString: "FAR  \nPT 1:"
*label144.alignment: "alignment_end"

*TF_NRLON2.class: textField
*TF_NRLON2.static: true
*TF_NRLON2.name: TF_NRLON2
*TF_NRLON2.parent: form_DTKquad
*TF_NRLON2.isCompound: "true"
*TF_NRLON2.compoundIcon: "textfield.xpm"
*TF_NRLON2.compoundName: "text_Field"
*TF_NRLON2.x: 135
*TF_NRLON2.y: 40
*TF_NRLON2.height: 30
*TF_NRLON2.cursorPositionVisible: "false"
*TF_NRLON2.editable: "false"
*TF_NRLON2.sensitive: "false"
*TF_NRLON2.text: ""
*TF_NRLON2.columns: 8
*TF_NRLON2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NRLON2.resizeWidth: "false"
*TF_NRLON2.maxLength: 8

*label142.class: label
*label142.static: true
*label142.name: label142
*label142.parent: form_DTKquad
*label142.isCompound: "true"
*label142.compoundIcon: "label.xpm"
*label142.compoundName: "label_"
*label142.x: 125
*label142.y: 45
*label142.width: 15
*label142.height: 20
*label142.labelString: "/"

*label147.class: label
*label147.static: true
*label147.name: label147
*label147.parent: form_DTKquad
*label147.isCompound: "true"
*label147.compoundIcon: "label.xpm"
*label147.compoundName: "label_"
*label147.x: 10
*label147.y: 5
*label147.height: 30
*label147.labelString: "NEAR \nPT 1:"
*label147.alignment: "alignment_end"

*TF_NRLAT1.class: textField
*TF_NRLAT1.static: true
*TF_NRLAT1.name: TF_NRLAT1
*TF_NRLAT1.parent: form_DTKquad
*TF_NRLAT1.isCompound: "true"
*TF_NRLAT1.compoundIcon: "textfield.xpm"
*TF_NRLAT1.compoundName: "text_Field"
*TF_NRLAT1.x: 45
*TF_NRLAT1.y: 5
*TF_NRLAT1.height: 30
*TF_NRLAT1.cursorPositionVisible: "false"
*TF_NRLAT1.editable: "false"
*TF_NRLAT1.sensitive: "false"
*TF_NRLAT1.text: ""
*TF_NRLAT1.columns: 8
*TF_NRLAT1.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NRLAT1.resizeWidth: "false"
*TF_NRLAT1.maxLength: 8

*TF_NRLAT2.class: textField
*TF_NRLAT2.static: true
*TF_NRLAT2.name: TF_NRLAT2
*TF_NRLAT2.parent: form_DTKquad
*TF_NRLAT2.isCompound: "true"
*TF_NRLAT2.compoundIcon: "textfield.xpm"
*TF_NRLAT2.compoundName: "text_Field"
*TF_NRLAT2.x: 45
*TF_NRLAT2.y: 40
*TF_NRLAT2.height: 30
*TF_NRLAT2.cursorPositionVisible: "false"
*TF_NRLAT2.editable: "false"
*TF_NRLAT2.sensitive: "false"
*TF_NRLAT2.text: ""
*TF_NRLAT2.columns: 8
*TF_NRLAT2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NRLAT2.resizeWidth: "false"
*TF_NRLAT2.maxLength: 8

*label_NW2.class: label
*label_NW2.static: true
*label_NW2.name: label_NW2
*label_NW2.parent: form_DTKquad
*label_NW2.isCompound: "true"
*label_NW2.compoundIcon: "label.xpm"
*label_NW2.compoundName: "label_"
*label_NW2.x: 10
*label_NW2.y: 40
*label_NW2.height: 30
*label_NW2.labelString: "NEAR \nPT 2:"
*label_NW2.alignment: "alignment_end"

*label145.class: label
*label145.static: true
*label145.name: label145
*label145.parent: form_DTKquad
*label145.isCompound: "true"
*label145.compoundIcon: "label.xpm"
*label145.compoundName: "label_"
*label145.x: 235
*label145.y: 40
*label145.height: 30
*label145.labelString: "FAR  \nPT 2:"
*label145.alignment: "alignment_end"

*TF_FARLON1.class: textField
*TF_FARLON1.static: true
*TF_FARLON1.name: TF_FARLON1
*TF_FARLON1.parent: form_DTKquad
*TF_FARLON1.isCompound: "true"
*TF_FARLON1.compoundIcon: "textfield.xpm"
*TF_FARLON1.compoundName: "text_Field"
*TF_FARLON1.x: 360
*TF_FARLON1.y: 5
*TF_FARLON1.height: 30
*TF_FARLON1.cursorPositionVisible: "false"
*TF_FARLON1.editable: "false"
*TF_FARLON1.sensitive: "false"
*TF_FARLON1.text: ""
*TF_FARLON1.columns: 8
*TF_FARLON1.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_FARLON1.resizeWidth: "false"
*TF_FARLON1.maxLength: 8

*label87.class: label
*label87.static: true
*label87.name: label87
*label87.parent: form_DTKquad
*label87.isCompound: "true"
*label87.compoundIcon: "label.xpm"
*label87.compoundName: "label_"
*label87.x: 350
*label87.y: 10
*label87.width: 15
*label87.height: 20
*label87.labelString: "/"

*TF_FARLAT2.class: textField
*TF_FARLAT2.static: true
*TF_FARLAT2.name: TF_FARLAT2
*TF_FARLAT2.parent: form_DTKquad
*TF_FARLAT2.isCompound: "true"
*TF_FARLAT2.compoundIcon: "textfield.xpm"
*TF_FARLAT2.compoundName: "text_Field"
*TF_FARLAT2.x: 270
*TF_FARLAT2.y: 40
*TF_FARLAT2.height: 30
*TF_FARLAT2.cursorPositionVisible: "false"
*TF_FARLAT2.editable: "false"
*TF_FARLAT2.sensitive: "false"
*TF_FARLAT2.text: ""
*TF_FARLAT2.columns: 8
*TF_FARLAT2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_FARLAT2.resizeWidth: "false"
*TF_FARLAT2.maxLength: 8

*TF_FARLON2.class: textField
*TF_FARLON2.static: true
*TF_FARLON2.name: TF_FARLON2
*TF_FARLON2.parent: form_DTKquad
*TF_FARLON2.isCompound: "true"
*TF_FARLON2.compoundIcon: "textfield.xpm"
*TF_FARLON2.compoundName: "text_Field"
*TF_FARLON2.x: 360
*TF_FARLON2.y: 40
*TF_FARLON2.height: 30
*TF_FARLON2.cursorPositionVisible: "false"
*TF_FARLON2.editable: "false"
*TF_FARLON2.sensitive: "false"
*TF_FARLON2.text: ""
*TF_FARLON2.columns: 8
*TF_FARLON2.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_FARLON2.resizeWidth: "false"
*TF_FARLON2.maxLength: 8

*label141.class: label
*label141.static: true
*label141.name: label141
*label141.parent: form_DTKquad
*label141.isCompound: "true"
*label141.compoundIcon: "label.xpm"
*label141.compoundName: "label_"
*label141.x: 350
*label141.y: 45
*label141.width: 15
*label141.height: 20
*label141.labelString: "/"

*TF_FARLAT1.class: textField
*TF_FARLAT1.static: true
*TF_FARLAT1.name: TF_FARLAT1
*TF_FARLAT1.parent: form_DTKquad
*TF_FARLAT1.isCompound: "true"
*TF_FARLAT1.compoundIcon: "textfield.xpm"
*TF_FARLAT1.compoundName: "text_Field"
*TF_FARLAT1.x: 270
*TF_FARLAT1.y: 5
*TF_FARLAT1.height: 30
*TF_FARLAT1.cursorPositionVisible: "false"
*TF_FARLAT1.editable: "false"
*TF_FARLAT1.sensitive: "false"
*TF_FARLAT1.text: ""
*TF_FARLAT1.columns: 8
*TF_FARLAT1.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_FARLAT1.resizeWidth: "false"
*TF_FARLAT1.maxLength: 8

*TF_DTK_SITENAME.class: textField
*TF_DTK_SITENAME.static: true
*TF_DTK_SITENAME.name: TF_DTK_SITENAME
*TF_DTK_SITENAME.parent: DTKManager
*TF_DTK_SITENAME.isCompound: "true"
*TF_DTK_SITENAME.compoundIcon: "textfield.xpm"
*TF_DTK_SITENAME.compoundName: "text_Field"
*TF_DTK_SITENAME.x: 362
*TF_DTK_SITENAME.y: 521
*TF_DTK_SITENAME.height: 32
*TF_DTK_SITENAME.cursorPositionVisible: "false"
*TF_DTK_SITENAME.editable: "false"
*TF_DTK_SITENAME.sensitive: "false"
*TF_DTK_SITENAME.text: ""
*TF_DTK_SITENAME.columns: 32
*TF_DTK_SITENAME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_SITENAME.resizeWidth: "false"
*TF_DTK_SITENAME.maxLength: 32

*label140.class: label
*label140.static: true
*label140.name: label140
*label140.parent: DTKManager
*label140.isCompound: "true"
*label140.compoundIcon: "label.xpm"
*label140.compoundName: "label_"
*label140.x: 322
*label140.y: 522
*label140.width: 35
*label140.height: 30
*label140.labelString: "SITE \nNAME:"
*label140.alignment: "alignment_end"

*label83.class: label
*label83.static: true
*label83.name: label83
*label83.parent: DTKManager
*label83.isCompound: "true"
*label83.compoundIcon: "label.xpm"
*label83.compoundName: "label_"
*label83.x: 616
*label83.y: 441
*label83.height: 30
*label83.labelString: "FA DTK ID:"
*label83.alignment: "alignment_end"

*TF_FADTKID.class: textField
*TF_FADTKID.static: true
*TF_FADTKID.name: TF_FADTKID
*TF_FADTKID.parent: DTKManager
*TF_FADTKID.isCompound: "true"
*TF_FADTKID.compoundIcon: "textfield.xpm"
*TF_FADTKID.compoundName: "text_Field"
*TF_FADTKID.x: 683
*TF_FADTKID.y: 440
*TF_FADTKID.height: 32
*TF_FADTKID.columns: 20
*TF_FADTKID.cursorPositionVisible: "false"
*TF_FADTKID.editable: "false"
*TF_FADTKID.sensitive: "false"
*TF_FADTKID.resizeWidth: "false"
*TF_FADTKID.text: ""
*TF_FADTKID.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_FADTKID.maxLength: 20
*TF_FADTKID.width: 186

*separator12.class: separator
*separator12.static: true
*separator12.name: separator12
*separator12.parent: DTKManager
*separator12.width: 888
*separator12.height: 15
*separator12.isCompound: "true"
*separator12.compoundIcon: "sep.xpm"
*separator12.compoundName: "separator_"
*separator12.x: -1
*separator12.y: 305

*TF_REV.class: textField
*TF_REV.static: true
*TF_REV.name: TF_REV
*TF_REV.parent: DTKManager
*TF_REV.isCompound: "true"
*TF_REV.compoundIcon: "textfield.xpm"
*TF_REV.compoundName: "text_Field"
*TF_REV.x: 362
*TF_REV.y: 324
*TF_REV.height: 30
*TF_REV.columns: 6
*TF_REV.cursorPositionVisible: "false"
*TF_REV.editable: "false"
*TF_REV.sensitive: "false"
*TF_REV.resizeWidth: "false"
*TF_REV.text: "0"
*TF_REV.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"

*label103.class: label
*label103.static: true
*label103.name: label103
*label103.parent: DTKManager
*label103.isCompound: "true"
*label103.compoundIcon: "label.xpm"
*label103.compoundName: "label_"
*label103.x: 333
*label103.y: 324
*label103.width: 30
*label103.height: 30
*label103.labelString: "REV:"
*label103.alignment: "alignment_end"

*label139.class: label
*label139.static: true
*label139.name: label139
*label139.parent: DTKManager
*label139.isCompound: "true"
*label139.compoundIcon: "label.xpm"
*label139.compoundName: "label_"
*label139.x: 36
*label139.y: 415
*label139.height: 30
*label139.labelString: "START TIME:"
*label139.alignment: "alignment_end"

*label138.class: label
*label138.static: true
*label138.name: label138
*label138.parent: DTKManager
*label138.isCompound: "true"
*label138.compoundIcon: "label.xpm"
*label138.compoundName: "label_"
*label138.x: 42
*label138.y: 459
*label138.height: 30
*label138.labelString: "STOP TIME:"
*label138.alignment: "alignment_end"

*TF_DTK_DARID.class: textField
*TF_DTK_DARID.static: true
*TF_DTK_DARID.name: TF_DTK_DARID
*TF_DTK_DARID.parent: DTKManager
*TF_DTK_DARID.isCompound: "true"
*TF_DTK_DARID.compoundIcon: "textfield.xpm"
*TF_DTK_DARID.compoundName: "text_Field"
*TF_DTK_DARID.x: 811
*TF_DTK_DARID.y: 324
*TF_DTK_DARID.height: 30
*TF_DTK_DARID.cursorPositionVisible: "false"
*TF_DTK_DARID.editable: "false"
*TF_DTK_DARID.sensitive: "false"
*TF_DTK_DARID.text: ""
*TF_DTK_DARID.columns: 5
*TF_DTK_DARID.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_DARID.resizeWidth: "false"

*label106.class: label
*label106.static: true
*label106.name: label106
*label106.parent: DTKManager
*label106.isCompound: "true"
*label106.compoundIcon: "label.xpm"
*label106.compoundName: "label_"
*label106.x: 774
*label106.y: 324
*label106.height: 30
*label106.labelString: "DARID:"
*label106.alignment: "alignment_end"
*label106.width: 38

*optionMenu_dtk_status.class: rowColumn
*optionMenu_dtk_status.static: true
*optionMenu_dtk_status.name: optionMenu_dtk_status
*optionMenu_dtk_status.parent: DTKManager
*optionMenu_dtk_status.rowColumnType: "menu_option"
*optionMenu_dtk_status.subMenuId: "subMenu_dtk_status"
*optionMenu_dtk_status.isCompound: "true"
*optionMenu_dtk_status.compoundIcon: "optionM.xpm"
*optionMenu_dtk_status.compoundName: "option_Menu"
*optionMenu_dtk_status.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_dtk_status.x: 57
*optionMenu_dtk_status.y: 364
*optionMenu_dtk_status.width: 215
*optionMenu_dtk_status.height: 35
*optionMenu_dtk_status.labelString: "STATUS:"
*optionMenu_dtk_status.sensitive: "false"

*subMenu_dtk_status.class: rowColumn
*subMenu_dtk_status.static: true
*subMenu_dtk_status.name: subMenu_dtk_status
*subMenu_dtk_status.parent: optionMenu_dtk_status
*subMenu_dtk_status.rowColumnType: "menu_pulldown"
*subMenu_dtk_status.labelString: ""
*subMenu_dtk_status.x: 796
*subMenu_dtk_status.y: 0
*subMenu_dtk_status.mappedWhenManaged: "true"

*subMenu_dtk_status_PLN.class: pushButton
*subMenu_dtk_status_PLN.static: true
*subMenu_dtk_status_PLN.name: subMenu_dtk_status_PLN
*subMenu_dtk_status_PLN.parent: subMenu_dtk_status
*subMenu_dtk_status_PLN.labelString: "PLN"
*subMenu_dtk_status_PLN.fontList: "rockwell-bold"
*subMenu_dtk_status_PLN.x: 800
*subMenu_dtk_status_PLN.y: 24

*subMenu_dtk_status_QUE.class: pushButton
*subMenu_dtk_status_QUE.static: true
*subMenu_dtk_status_QUE.name: subMenu_dtk_status_QUE
*subMenu_dtk_status_QUE.parent: subMenu_dtk_status
*subMenu_dtk_status_QUE.labelString: "QUE"
*subMenu_dtk_status_QUE.fontList: "rockwell-bold"
*subMenu_dtk_status_QUE.x: 800
*subMenu_dtk_status_QUE.y: 2

*subMenu_dtk_status_SUB.class: pushButton
*subMenu_dtk_status_SUB.static: true
*subMenu_dtk_status_SUB.name: subMenu_dtk_status_SUB
*subMenu_dtk_status_SUB.parent: subMenu_dtk_status
*subMenu_dtk_status_SUB.labelString: "SUB"
*subMenu_dtk_status_SUB.fontList: "rockwell-bold"
*subMenu_dtk_status_SUB.x: 800
*subMenu_dtk_status_SUB.y: 112

*subMenu_dtk_status_SCH.class: pushButton
*subMenu_dtk_status_SCH.static: true
*subMenu_dtk_status_SCH.name: subMenu_dtk_status_SCH
*subMenu_dtk_status_SCH.parent: subMenu_dtk_status
*subMenu_dtk_status_SCH.labelString: "SCH"
*subMenu_dtk_status_SCH.fontList: "rockwell-bold"
*subMenu_dtk_status_SCH.x: 800
*subMenu_dtk_status_SCH.y: 90

*subMenu_dtk_status_CON.class: pushButton
*subMenu_dtk_status_CON.static: true
*subMenu_dtk_status_CON.name: subMenu_dtk_status_CON
*subMenu_dtk_status_CON.parent: subMenu_dtk_status
*subMenu_dtk_status_CON.labelString: "CON"
*subMenu_dtk_status_CON.sensitive: "false"
*subMenu_dtk_status_CON.fontList: "rockwell-bold"

*subMenu_dtk_status_REJ.class: pushButton
*subMenu_dtk_status_REJ.static: true
*subMenu_dtk_status_REJ.name: subMenu_dtk_status_REJ
*subMenu_dtk_status_REJ.parent: subMenu_dtk_status
*subMenu_dtk_status_REJ.labelString: "REJ"
*subMenu_dtk_status_REJ.fontList: "rockwell-bold"
*subMenu_dtk_status_REJ.x: 800
*subMenu_dtk_status_REJ.y: 46

*subMenu_dtk_status_DEL.class: pushButton
*subMenu_dtk_status_DEL.static: true
*subMenu_dtk_status_DEL.name: subMenu_dtk_status_DEL
*subMenu_dtk_status_DEL.parent: subMenu_dtk_status
*subMenu_dtk_status_DEL.labelString: "DEL"
*subMenu_dtk_status_DEL.fontList: "rockwell-bold"
*subMenu_dtk_status_DEL.x: 800
*subMenu_dtk_status_DEL.y: 68

*subMenu_dtk_status_INV.class: pushButton
*subMenu_dtk_status_INV.static: true
*subMenu_dtk_status_INV.name: subMenu_dtk_status_INV
*subMenu_dtk_status_INV.parent: subMenu_dtk_status
*subMenu_dtk_status_INV.labelString: "INV"
*subMenu_dtk_status_INV.fontList: "rockwell-bold"

*pushButton_EditDTK.class: pushButton
*pushButton_EditDTK.static: true
*pushButton_EditDTK.name: pushButton_EditDTK
*pushButton_EditDTK.parent: DTKManager
*pushButton_EditDTK.isCompound: "true"
*pushButton_EditDTK.compoundIcon: "push.xpm"
*pushButton_EditDTK.compoundName: "push_Button"
*pushButton_EditDTK.x: 38
*pushButton_EditDTK.y: 697
*pushButton_EditDTK.width: 90
*pushButton_EditDTK.height: 40
*pushButton_EditDTK.labelString: "EDIT"
*pushButton_EditDTK.fontList: "rockwell-bold"
*pushButton_EditDTK.sensitive: "false"
*pushButton_EditDTK.activateCallback.source: public
*pushButton_EditDTK.activateCallback: cb_set_dtkmanager_editability
*pushButton_EditDTK.activateCallbackClientData: (XtPointer) DTK_EDIT

*pushButton_DeleteDTK.class: pushButton
*pushButton_DeleteDTK.static: true
*pushButton_DeleteDTK.name: pushButton_DeleteDTK
*pushButton_DeleteDTK.parent: DTKManager
*pushButton_DeleteDTK.isCompound: "true"
*pushButton_DeleteDTK.compoundIcon: "push.xpm"
*pushButton_DeleteDTK.compoundName: "push_Button"
*pushButton_DeleteDTK.x: 149
*pushButton_DeleteDTK.y: 697
*pushButton_DeleteDTK.width: 90
*pushButton_DeleteDTK.height: 40
*pushButton_DeleteDTK.labelString: "DELETE"
*pushButton_DeleteDTK.fontList: "rockwell-bold"
*pushButton_DeleteDTK.sensitive: "false"
*pushButton_DeleteDTK.activateCallback.source: public
*pushButton_DeleteDTK.activateCallback: cb_delete_dtk_record

*pushButton_QuitDTK.class: pushButton
*pushButton_QuitDTK.static: true
*pushButton_QuitDTK.name: pushButton_QuitDTK
*pushButton_QuitDTK.parent: DTKManager
*pushButton_QuitDTK.isCompound: "true"
*pushButton_QuitDTK.compoundIcon: "push.xpm"
*pushButton_QuitDTK.compoundName: "push_Button"
*pushButton_QuitDTK.x: 749
*pushButton_QuitDTK.y: 697
*pushButton_QuitDTK.width: 90
*pushButton_QuitDTK.height: 40
*pushButton_QuitDTK.labelString: "QUIT"
*pushButton_QuitDTK.fontList: "rockwell-bold"
*pushButton_QuitDTK.activateCallback: {\
XtPopdown(XtParent(DTK_manager)) ;\
}

*pushButton_ClearDTKForm.class: pushButton
*pushButton_ClearDTKForm.static: true
*pushButton_ClearDTKForm.name: pushButton_ClearDTKForm
*pushButton_ClearDTKForm.parent: DTKManager
*pushButton_ClearDTKForm.isCompound: "true"
*pushButton_ClearDTKForm.compoundIcon: "push.xpm"
*pushButton_ClearDTKForm.compoundName: "push_Button"
*pushButton_ClearDTKForm.x: 399
*pushButton_ClearDTKForm.y: 697
*pushButton_ClearDTKForm.width: 90
*pushButton_ClearDTKForm.height: 40
*pushButton_ClearDTKForm.labelString: "CLEAR"
*pushButton_ClearDTKForm.fontList: "rockwell-bold"
*pushButton_ClearDTKForm.activateCallback.source: public
*pushButton_ClearDTKForm.activateCallback: cb_clear_dtkmanager_form
*pushButton_ClearDTKForm.createManaged: "false"

*pushButton_SaveDTKChanges.class: pushButton
*pushButton_SaveDTKChanges.static: true
*pushButton_SaveDTKChanges.name: pushButton_SaveDTKChanges
*pushButton_SaveDTKChanges.parent: DTKManager
*pushButton_SaveDTKChanges.isCompound: "true"
*pushButton_SaveDTKChanges.compoundIcon: "push.xpm"
*pushButton_SaveDTKChanges.compoundName: "push_Button"
*pushButton_SaveDTKChanges.x: 508
*pushButton_SaveDTKChanges.y: 697
*pushButton_SaveDTKChanges.width: 90
*pushButton_SaveDTKChanges.height: 40
*pushButton_SaveDTKChanges.labelString: "SAVE\nCHANGES"
*pushButton_SaveDTKChanges.fontList: "rockwell-bold"
*pushButton_SaveDTKChanges.activateCallback.source: public
*pushButton_SaveDTKChanges.activateCallback: cb_save_dtk_changes
*pushButton_SaveDTKChanges.activateCallbackClientData: (XtPointer) False
*pushButton_SaveDTKChanges.createManaged: "false"

*pushButton_CancelDTKChanges.class: pushButton
*pushButton_CancelDTKChanges.name.source: public
*pushButton_CancelDTKChanges.static: false
*pushButton_CancelDTKChanges.name: pushButton_CancelDTKChanges
*pushButton_CancelDTKChanges.parent: DTKManager
*pushButton_CancelDTKChanges.isCompound: "true"
*pushButton_CancelDTKChanges.compoundIcon: "push.xpm"
*pushButton_CancelDTKChanges.compoundName: "push_Button"
*pushButton_CancelDTKChanges.x: 617
*pushButton_CancelDTKChanges.y: 697
*pushButton_CancelDTKChanges.width: 90
*pushButton_CancelDTKChanges.height: 40
*pushButton_CancelDTKChanges.labelString: "CANCEL\nEDIT"
*pushButton_CancelDTKChanges.fontList: "rockwell-bold"
*pushButton_CancelDTKChanges.activateCallback.source: public
*pushButton_CancelDTKChanges.activateCallback: cb_set_dtkmanager_editability
*pushButton_CancelDTKChanges.activateCallbackClientData: (XtPointer) DTK_RESET
*pushButton_CancelDTKChanges.createManaged: "false"

*TF_DTK_recordcount.class: textField
*TF_DTK_recordcount.static: true
*TF_DTK_recordcount.name: TF_DTK_recordcount
*TF_DTK_recordcount.parent: DTKManager
*TF_DTK_recordcount.isCompound: "true"
*TF_DTK_recordcount.compoundIcon: "textfield.xpm"
*TF_DTK_recordcount.compoundName: "text_Field"
*TF_DTK_recordcount.x: 719
*TF_DTK_recordcount.y: 263
*TF_DTK_recordcount.height: 32
*TF_DTK_recordcount.columns: 5
*TF_DTK_recordcount.cursorPositionVisible: "false"
*TF_DTK_recordcount.editable: "false"
*TF_DTK_recordcount.resizeWidth: "false"
*TF_DTK_recordcount.text: "0"
*TF_DTK_recordcount.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_recordcount.traversalOn: "false"

*label137.class: label
*label137.static: true
*label137.name: label137
*label137.parent: DTKManager
*label137.isCompound: "true"
*label137.compoundIcon: "label.xpm"
*label137.compoundName: "label_"
*label137.x: 679
*label137.y: 264
*label137.height: 30
*label137.labelString: "RECORD\nCOUNT:"
*label137.alignment: "alignment_end"

*menuBar_DTK_FILE.class: rowColumn
*menuBar_DTK_FILE.static: true
*menuBar_DTK_FILE.name: menuBar_DTK_FILE
*menuBar_DTK_FILE.parent: DTKManager
*menuBar_DTK_FILE.rowColumnType: "menu_bar"
*menuBar_DTK_FILE.isCompound: "true"
*menuBar_DTK_FILE.compoundIcon: "pulldownM.xpm"
*menuBar_DTK_FILE.compoundName: "menu_Bar"
*menuBar_DTK_FILE.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 1, 0);\
}
*menuBar_DTK_FILE.x: 49
*menuBar_DTK_FILE.y: 262
*menuBar_DTK_FILE.menuAccelerator: "<KeyUp>F10"
*menuBar_DTK_FILE.packing: "pack_column"
*menuBar_DTK_FILE.resizeHeight: "false"
*menuBar_DTK_FILE.resizeWidth: "false"
*menuBar_DTK_FILE.entryAlignment: "alignment_center"
*menuBar_DTK_FILE.marginHeight: 2
*menuBar_DTK_FILE.marginWidth: 0
*menuBar_DTK_FILE.width: 80
*menuBar_DTK_FILE.height: 34

*menuBar_DTK_FILE_pane.class: rowColumn
*menuBar_DTK_FILE_pane.static: true
*menuBar_DTK_FILE_pane.name: menuBar_DTK_FILE_pane
*menuBar_DTK_FILE_pane.parent: menuBar_DTK_FILE
*menuBar_DTK_FILE_pane.rowColumnType: "menu_pulldown"
*menuBar_DTK_FILE_pane.packing: "pack_column"
*menuBar_DTK_FILE_pane.width: 200
*menuBar_DTK_FILE_pane.entryAlignment: "alignment_center"
*menuBar_DTK_FILE_pane.x: 0
*menuBar_DTK_FILE_pane.y: 244

*cascadeButton_DTK_SAVE.class: cascadeButton
*cascadeButton_DTK_SAVE.static: true
*cascadeButton_DTK_SAVE.name: cascadeButton_DTK_SAVE
*cascadeButton_DTK_SAVE.parent: menuBar_DTK_FILE_pane
*cascadeButton_DTK_SAVE.labelString: "SAVE DTK REPORT"
*cascadeButton_DTK_SAVE.subMenuId: "PANE_SAVE_DTK_RPT"
*cascadeButton_DTK_SAVE.fontList: "rockwell-bold"
*cascadeButton_DTK_SAVE.x: 2
*cascadeButton_DTK_SAVE.y: 268

*cascadeButton_DTK_PRINT.class: cascadeButton
*cascadeButton_DTK_PRINT.static: true
*cascadeButton_DTK_PRINT.name: cascadeButton_DTK_PRINT
*cascadeButton_DTK_PRINT.parent: menuBar_DTK_FILE_pane
*cascadeButton_DTK_PRINT.labelString: "PRINT DTK REPORT"
*cascadeButton_DTK_PRINT.subMenuId: "PANE_PRINT_DTK_RPT"
*cascadeButton_DTK_PRINT.fontList: "rockwell-bold"
*cascadeButton_DTK_PRINT.x: 2
*cascadeButton_DTK_PRINT.y: 268

*PANE_SAVE_DTK_RPT.class: rowColumn
*PANE_SAVE_DTK_RPT.static: true
*PANE_SAVE_DTK_RPT.name: PANE_SAVE_DTK_RPT
*PANE_SAVE_DTK_RPT.parent: menuBar_DTK_FILE_pane
*PANE_SAVE_DTK_RPT.rowColumnType: "menu_pulldown"
*PANE_SAVE_DTK_RPT.x: 0
*PANE_SAVE_DTK_RPT.y: 244

*RPT_SELECTED_DTK.class: pushButton
*RPT_SELECTED_DTK.static: true
*RPT_SELECTED_DTK.name: RPT_SELECTED_DTK
*RPT_SELECTED_DTK.parent: PANE_SAVE_DTK_RPT
*RPT_SELECTED_DTK.labelString: "SELECTED DTK"
*RPT_SELECTED_DTK.fontList: "rockwell-bold"
*RPT_SELECTED_DTK.alignment: "alignment_center"
*RPT_SELECTED_DTK.activateCallback.source: public
*RPT_SELECTED_DTK.activateCallback: cb_set_print_dtks_to_file_cb
*RPT_SELECTED_DTK.activateCallbackClientData: (XtPointer) PRINT_DTKS_SELECTED_TO_FILE
*RPT_SELECTED_DTK.x: 2
*RPT_SELECTED_DTK.y: 268

*RPT_CURRENT_DTKS.class: pushButton
*RPT_CURRENT_DTKS.static: true
*RPT_CURRENT_DTKS.name: RPT_CURRENT_DTKS
*RPT_CURRENT_DTKS.parent: PANE_SAVE_DTK_RPT
*RPT_CURRENT_DTKS.labelString: "CURRENT DTKS"
*RPT_CURRENT_DTKS.fontList: "rockwell-bold"
*RPT_CURRENT_DTKS.alignment: "alignment_center"
*RPT_CURRENT_DTKS.activateCallback.source: public
*RPT_CURRENT_DTKS.activateCallback: cb_set_print_dtks_to_file_cb
*RPT_CURRENT_DTKS.activateCallbackClientData: (XtPointer) PRINT_DTKS_CURRENT_TO_FILE
*RPT_CURRENT_DTKS.x: 2
*RPT_CURRENT_DTKS.y: 268

*RPT_ALL_DTKS.class: pushButton
*RPT_ALL_DTKS.static: true
*RPT_ALL_DTKS.name: RPT_ALL_DTKS
*RPT_ALL_DTKS.parent: PANE_SAVE_DTK_RPT
*RPT_ALL_DTKS.labelString: "ALL DTKS"
*RPT_ALL_DTKS.fontList: "rockwell-bold"
*RPT_ALL_DTKS.alignment: "alignment_center"
*RPT_ALL_DTKS.activateCallback.source: public
*RPT_ALL_DTKS.activateCallback: cb_set_print_dtks_to_file_cb
*RPT_ALL_DTKS.activateCallbackClientData: (XtPointer) PRINT_DTKS_ALL_TO_FILE
*RPT_ALL_DTKS.x: 2
*RPT_ALL_DTKS.y: 268

*PANE_PRINT_DTK_RPT.class: rowColumn
*PANE_PRINT_DTK_RPT.static: true
*PANE_PRINT_DTK_RPT.name: PANE_PRINT_DTK_RPT
*PANE_PRINT_DTK_RPT.parent: menuBar_DTK_FILE_pane
*PANE_PRINT_DTK_RPT.rowColumnType: "menu_pulldown"
*PANE_PRINT_DTK_RPT.x: 0
*PANE_PRINT_DTK_RPT.y: 246

*PRINT_SELECTED_DTK.class: pushButton
*PRINT_SELECTED_DTK.static: true
*PRINT_SELECTED_DTK.name: PRINT_SELECTED_DTK
*PRINT_SELECTED_DTK.parent: PANE_PRINT_DTK_RPT
*PRINT_SELECTED_DTK.labelString: "SELECTED DTK"
*PRINT_SELECTED_DTK.activateCallback.source: public
*PRINT_SELECTED_DTK.activateCallback: cb_print_dtks
*PRINT_SELECTED_DTK.activateCallbackClientData: (XtPointer) PRINT_DTKS_SELECTED
*PRINT_SELECTED_DTK.x: 2
*PRINT_SELECTED_DTK.y: 269

*PRINT_CURRENT_DTKS.class: pushButton
*PRINT_CURRENT_DTKS.static: true
*PRINT_CURRENT_DTKS.name: PRINT_CURRENT_DTKS
*PRINT_CURRENT_DTKS.parent: PANE_PRINT_DTK_RPT
*PRINT_CURRENT_DTKS.labelString: "CURRENT DTKS"
*PRINT_CURRENT_DTKS.activateCallback.source: public
*PRINT_CURRENT_DTKS.activateCallback: cb_print_dtks
*PRINT_CURRENT_DTKS.activateCallbackClientData: (XtPointer) PRINT_DTKS_CURRENT
*PRINT_CURRENT_DTKS.x: 2
*PRINT_CURRENT_DTKS.y: 269

*PRINT_ALL_DTKS.class: pushButton
*PRINT_ALL_DTKS.static: true
*PRINT_ALL_DTKS.name: PRINT_ALL_DTKS
*PRINT_ALL_DTKS.parent: PANE_PRINT_DTK_RPT
*PRINT_ALL_DTKS.labelString: "ALL DTKS"
*PRINT_ALL_DTKS.activateCallback.source: public
*PRINT_ALL_DTKS.activateCallback: cb_print_dtks
*PRINT_ALL_DTKS.activateCallbackClientData: (XtPointer) PRINT_DTKS_ALL
*PRINT_ALL_DTKS.x: 2
*PRINT_ALL_DTKS.y: 269

*menuBar_cascade_button_DTK_FILE.class: cascadeButton
*menuBar_cascade_button_DTK_FILE.static: true
*menuBar_cascade_button_DTK_FILE.name: menuBar_cascade_button_DTK_FILE
*menuBar_cascade_button_DTK_FILE.parent: menuBar_DTK_FILE
*menuBar_cascade_button_DTK_FILE.labelString: "FILE"
*menuBar_cascade_button_DTK_FILE.subMenuId: "menuBar_DTK_FILE_pane"
*menuBar_cascade_button_DTK_FILE.fontList: "rockwell-bold"
*menuBar_cascade_button_DTK_FILE.x: 2
*menuBar_cascade_button_DTK_FILE.y: 266
*menuBar_cascade_button_DTK_FILE.alignment: "alignment_center"
*menuBar_cascade_button_DTK_FILE.recomputeSize: "false"
*menuBar_cascade_button_DTK_FILE.width: 76
*menuBar_cascade_button_DTK_FILE.marginWidth: 0
*menuBar_cascade_button_DTK_FILE.marginHeight: 0
*menuBar_cascade_button_DTK_FILE.height: 32

*pushButton_DTK_refresh.class: pushButton
*pushButton_DTK_refresh.static: true
*pushButton_DTK_refresh.name: pushButton_DTK_refresh
*pushButton_DTK_refresh.parent: DTKManager
*pushButton_DTK_refresh.x: 26
*pushButton_DTK_refresh.y: 79
*pushButton_DTK_refresh.width: 30
*pushButton_DTK_refresh.height: 130
*pushButton_DTK_refresh.fontList: "rockwell-bold"
*pushButton_DTK_refresh.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton_DTK_refresh.activateCallback.source: public
*pushButton_DTK_refresh.activateCallback: cb_show_dtk_records
*pushButton_DTK_refresh.activateCallbackClientData: (XtPointer) scrolledList_DTKS

*TF_STOPLAT.class: textField
*TF_STOPLAT.static: true
*TF_STOPLAT.name: TF_STOPLAT
*TF_STOPLAT.parent: DTKManager
*TF_STOPLAT.isCompound: "true"
*TF_STOPLAT.compoundIcon: "textfield.xpm"
*TF_STOPLAT.compoundName: "text_Field"
*TF_STOPLAT.x: 344
*TF_STOPLAT.y: 648
*TF_STOPLAT.height: 30
*TF_STOPLAT.cursorPositionVisible: "false"
*TF_STOPLAT.editable: "false"
*TF_STOPLAT.sensitive: "false"
*TF_STOPLAT.text: ""
*TF_STOPLAT.columns: 8
*TF_STOPLAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_STOPLAT.resizeWidth: "false"
*TF_STOPLAT.createManaged: "true"
*TF_STOPLAT.maxLength: 8

*TF_STRTLAT.class: textField
*TF_STRTLAT.static: true
*TF_STRTLAT.name: TF_STRTLAT
*TF_STRTLAT.parent: DTKManager
*TF_STRTLAT.isCompound: "true"
*TF_STRTLAT.compoundIcon: "textfield.xpm"
*TF_STRTLAT.compoundName: "text_Field"
*TF_STRTLAT.x: 344
*TF_STRTLAT.y: 613
*TF_STRTLAT.height: 30
*TF_STRTLAT.cursorPositionVisible: "false"
*TF_STRTLAT.editable: "false"
*TF_STRTLAT.sensitive: "false"
*TF_STRTLAT.text: ""
*TF_STRTLAT.columns: 8
*TF_STRTLAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_STRTLAT.resizeWidth: "false"
*TF_STRTLAT.createManaged: "true"
*TF_STRTLAT.maxLength: 8

*label146.class: label
*label146.static: true
*label146.name: label146
*label146.parent: DTKManager
*label146.isCompound: "true"
*label146.compoundIcon: "label.xpm"
*label146.compoundName: "label_"
*label146.x: 279
*label146.y: 613
*label146.height: 30
*label146.labelString: "START LAT:"
*label146.alignment: "alignment_end"
*label146.createManaged: "true"

*label112.class: label
*label112.static: true
*label112.name: label112
*label112.parent: DTKManager
*label112.isCompound: "true"
*label112.compoundIcon: "label.xpm"
*label112.compoundName: "label_"
*label112.x: 279
*label112.y: 648
*label112.height: 30
*label112.labelString: " STOP LAT:"
*label112.alignment: "alignment_end"
*label112.createManaged: "true"

*TF_NOTES.class: textField
*TF_NOTES.static: true
*TF_NOTES.name: TF_NOTES
*TF_NOTES.parent: DTKManager
*TF_NOTES.isCompound: "true"
*TF_NOTES.compoundIcon: "textfield.xpm"
*TF_NOTES.compoundName: "text_Field"
*TF_NOTES.x: 362
*TF_NOTES.y: 561
*TF_NOTES.height: 32
*TF_NOTES.cursorPositionVisible: "false"
*TF_NOTES.editable: "false"
*TF_NOTES.sensitive: "false"
*TF_NOTES.text: ""
*TF_NOTES.columns: 40
*TF_NOTES.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NOTES.resizeWidth: "false"
*TF_NOTES.maxLength: 40

*label113.class: label
*label113.static: true
*label113.name: label113
*label113.parent: DTKManager
*label113.isCompound: "true"
*label113.compoundIcon: "label.xpm"
*label113.compoundName: "label_"
*label113.x: 317
*label113.y: 562
*label113.height: 30
*label113.labelString: "NOTES:"
*label113.alignment: "alignment_end"
*label113.width: 40

*optionMenu_sat.class: rowColumn
*optionMenu_sat.static: true
*optionMenu_sat.name: optionMenu_sat
*optionMenu_sat.parent: DTKManager
*optionMenu_sat.rowColumnType: "menu_option"
*optionMenu_sat.subMenuId: "subMenu_sat"
*optionMenu_sat.isCompound: "true"
*optionMenu_sat.compoundIcon: "optionM.xpm"
*optionMenu_sat.compoundName: "option_Menu"
*optionMenu_sat.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_sat.x: 46
*optionMenu_sat.y: 321
*optionMenu_sat.width: 215
*optionMenu_sat.height: 35
*optionMenu_sat.labelString: "SATELLITE\n/SENSOR:"
*optionMenu_sat.sensitive: "false"

*subMenu_sat.class: rowColumn
*subMenu_sat.static: true
*subMenu_sat.name: subMenu_sat
*subMenu_sat.parent: optionMenu_sat
*subMenu_sat.rowColumnType: "menu_pulldown"
*subMenu_sat.labelString: ""
*subMenu_sat.x: 0
*subMenu_sat.y: 326
*subMenu_sat.mappedWhenManaged: "true"
*subMenu_sat.menuPost: ""

*subMenu_sat_ERS.class: pushButton
*subMenu_sat_ERS.static: true
*subMenu_sat_ERS.name: subMenu_sat_ERS
*subMenu_sat_ERS.parent: subMenu_sat
*subMenu_sat_ERS.labelString: "RADARSAT"
*subMenu_sat_ERS.fontList: "rockwell-bold"
*subMenu_sat_ERS.x: 2
*subMenu_sat_ERS.y: 328
*subMenu_sat_ERS.createCallback.source: public
*subMenu_sat_ERS.createCallback: cb_build_satellite_option_menu

*optionMenu_sensor.class: rowColumn
*optionMenu_sensor.static: true
*optionMenu_sensor.name: optionMenu_sensor
*optionMenu_sensor.parent: DTKManager
*optionMenu_sensor.rowColumnType: "menu_option"
*optionMenu_sensor.subMenuId: "subMenu_sensor"
*optionMenu_sensor.isCompound: "true"
*optionMenu_sensor.compoundIcon: "optionM.xpm"
*optionMenu_sensor.compoundName: "option_Menu"
*optionMenu_sensor.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_sensor.x: 216
*optionMenu_sensor.y: 321
*optionMenu_sensor.width: 215
*optionMenu_sensor.height: 35
*optionMenu_sensor.labelString: "/"
*optionMenu_sensor.sensitive: "false"

*subMenu_sensor.class: rowColumn
*subMenu_sensor.static: true
*subMenu_sensor.name: subMenu_sensor
*subMenu_sensor.parent: optionMenu_sensor
*subMenu_sensor.rowColumnType: "menu_pulldown"
*subMenu_sensor.labelString: ""
*subMenu_sensor.x: 0
*subMenu_sensor.y: 326
*subMenu_sensor.mappedWhenManaged: "true"

*subMenu_sensor_SAR.class: pushButton
*subMenu_sensor_SAR.static: true
*subMenu_sensor_SAR.name: subMenu_sensor_SAR
*subMenu_sensor_SAR.parent: subMenu_sensor
*subMenu_sensor_SAR.labelString: "SAR"
*subMenu_sensor_SAR.fontList: "rockwell-bold"
*subMenu_sensor_SAR.x: 2
*subMenu_sensor_SAR.y: 328
*subMenu_sensor_SAR.createCallback.source: public
*subMenu_sensor_SAR.createCallback: cb_build_sensor_option_menu

*optionMenu_dtk_direction.class: rowColumn
*optionMenu_dtk_direction.static: true
*optionMenu_dtk_direction.name: optionMenu_dtk_direction
*optionMenu_dtk_direction.parent: DTKManager
*optionMenu_dtk_direction.rowColumnType: "menu_option"
*optionMenu_dtk_direction.subMenuId: "subMenu_dtk_direction"
*optionMenu_dtk_direction.isCompound: "true"
*optionMenu_dtk_direction.compoundIcon: "optionM.xpm"
*optionMenu_dtk_direction.compoundName: "option_Menu"
*optionMenu_dtk_direction.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_dtk_direction.x: 129
*optionMenu_dtk_direction.y: 613
*optionMenu_dtk_direction.width: 215
*optionMenu_dtk_direction.height: 35
*optionMenu_dtk_direction.labelString: "DIRECTION:"
*optionMenu_dtk_direction.sensitive: "false"

*subMenu_dtk_direction.class: rowColumn
*subMenu_dtk_direction.static: true
*subMenu_dtk_direction.name: subMenu_dtk_direction
*subMenu_dtk_direction.parent: optionMenu_dtk_direction
*subMenu_dtk_direction.rowColumnType: "menu_pulldown"
*subMenu_dtk_direction.labelString: ""
*subMenu_dtk_direction.sensitive: "false"
*subMenu_dtk_direction.x: 0
*subMenu_dtk_direction.y: 335
*subMenu_dtk_direction.mappedWhenManaged: "true"

*subMenu_dtk_direction_ascend.class: pushButton
*subMenu_dtk_direction_ascend.static: true
*subMenu_dtk_direction_ascend.name: subMenu_dtk_direction_ascend
*subMenu_dtk_direction_ascend.parent: subMenu_dtk_direction
*subMenu_dtk_direction_ascend.labelString: "ASC"
*subMenu_dtk_direction_ascend.fontList: "rockwell-bold"
*subMenu_dtk_direction_ascend.x: 2
*subMenu_dtk_direction_ascend.y: 335

*subMenu_dtk_direction_descend.class: pushButton
*subMenu_dtk_direction_descend.static: true
*subMenu_dtk_direction_descend.name: subMenu_dtk_direction_descend
*subMenu_dtk_direction_descend.parent: subMenu_dtk_direction
*subMenu_dtk_direction_descend.labelString: "DSC"
*subMenu_dtk_direction_descend.fontList: "rockwell-bold"

*subMenu_dtk_direction_cvrgNotAllowed.class: pushButton
*subMenu_dtk_direction_cvrgNotAllowed.static: true
*subMenu_dtk_direction_cvrgNotAllowed.name: subMenu_dtk_direction_cvrgNotAllowed
*subMenu_dtk_direction_cvrgNotAllowed.parent: subMenu_dtk_direction
*subMenu_dtk_direction_cvrgNotAllowed.labelString: "N/A"
*subMenu_dtk_direction_cvrgNotAllowed.fontList: "rockwell-bold"

*pushButton_CreateDTK.class: pushButton
*pushButton_CreateDTK.static: true
*pushButton_CreateDTK.name: pushButton_CreateDTK
*pushButton_CreateDTK.parent: DTKManager
*pushButton_CreateDTK.isCompound: "true"
*pushButton_CreateDTK.compoundIcon: "push.xpm"
*pushButton_CreateDTK.compoundName: "push_Button"
*pushButton_CreateDTK.x: 262
*pushButton_CreateDTK.y: 697
*pushButton_CreateDTK.width: 90
*pushButton_CreateDTK.height: 40
*pushButton_CreateDTK.labelString: "CREATE"
*pushButton_CreateDTK.fontList: "rockwell-bold"
*pushButton_CreateDTK.activateCallback.source: public
*pushButton_CreateDTK.activateCallback: cb_create_dtk

*TF_DTKDATE.class: textField
*TF_DTKDATE.static: true
*TF_DTKDATE.name: TF_DTKDATE
*TF_DTKDATE.parent: DTKManager
*TF_DTKDATE.x: 107
*TF_DTKDATE.y: 561
*TF_DTKDATE.height: 32
*TF_DTKDATE.columns: 21
*TF_DTKDATE.resizeWidth: "false"
*TF_DTKDATE.text: ""
*TF_DTKDATE.cursorPositionVisible: "false"
*TF_DTKDATE.editable: "false"
*TF_DTKDATE.sensitive: "false"
*TF_DTKDATE.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTKDATE.maxLength: 21
*TF_DTKDATE.traversalOn: "false"
*TF_DTKDATE.width: 185
*TF_DTKDATE.leftOffset: 107

*label116.class: label
*label116.static: true
*label116.name: label116
*label116.parent: DTKManager
*label116.isCompound: "true"
*label116.compoundIcon: "label.xpm"
*label116.compoundName: "label_"
*label116.x: 46
*label116.y: 562
*label116.height: 30
*label116.labelString: "  LAST  \nMODIFIED:"
*label116.alignment: "alignment_end"
*label116.width: 60

*TF_DTK_STRTTIME.class: textField
*TF_DTK_STRTTIME.static: true
*TF_DTK_STRTTIME.name: TF_DTK_STRTTIME
*TF_DTK_STRTTIME.parent: DTKManager
*TF_DTK_STRTTIME.isCompound: "true"
*TF_DTK_STRTTIME.compoundIcon: "textfield.xpm"
*TF_DTK_STRTTIME.compoundName: "text_Field"
*TF_DTK_STRTTIME.x: 107
*TF_DTK_STRTTIME.y: 414
*TF_DTK_STRTTIME.height: 32
*TF_DTK_STRTTIME.columns: 21
*TF_DTK_STRTTIME.cursorPositionVisible: "false"
*TF_DTK_STRTTIME.editable: "false"
*TF_DTK_STRTTIME.sensitive: "false"
*TF_DTK_STRTTIME.resizeWidth: "false"
*TF_DTK_STRTTIME.text: ""
*TF_DTK_STRTTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_STRTTIME.maxLength: 21
*TF_DTK_STRTTIME.activateCallback.source: public
*TF_DTK_STRTTIME.activateCallback: cb_validate_ASF_datetime
*TF_DTK_STRTTIME.activateCallbackClientData: (XtPointer) "DTK Start Time"
*TF_DTK_STRTTIME.focusCallback.source: public
*TF_DTK_STRTTIME.focusCallback: cb_toggle_cursor
*TF_DTK_STRTTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_DTK_STRTTIME.losingFocusCallback.source: public
*TF_DTK_STRTTIME.losingFocusCallback: cb_toggle_cursor
*TF_DTK_STRTTIME.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_DTK_STRTTIME.modifyVerifyCallback.source: public
*TF_DTK_STRTTIME.modifyVerifyCallback: cb_filter_text
*TF_DTK_STRTTIME.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*TF_DTK_STRTTIME.width: 185

*TF_DTK_STOPTIME.class: textField
*TF_DTK_STOPTIME.static: true
*TF_DTK_STOPTIME.name: TF_DTK_STOPTIME
*TF_DTK_STOPTIME.parent: DTKManager
*TF_DTK_STOPTIME.isCompound: "true"
*TF_DTK_STOPTIME.compoundIcon: "textfield.xpm"
*TF_DTK_STOPTIME.compoundName: "text_Field"
*TF_DTK_STOPTIME.x: 107
*TF_DTK_STOPTIME.y: 458
*TF_DTK_STOPTIME.height: 32
*TF_DTK_STOPTIME.columns: 21
*TF_DTK_STOPTIME.cursorPositionVisible: "false"
*TF_DTK_STOPTIME.editable: "false"
*TF_DTK_STOPTIME.sensitive: "false"
*TF_DTK_STOPTIME.resizeWidth: "false"
*TF_DTK_STOPTIME.text: ""
*TF_DTK_STOPTIME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DTK_STOPTIME.maxLength: 21
*TF_DTK_STOPTIME.activateCallback.source: public
*TF_DTK_STOPTIME.activateCallback: cb_validate_ASF_datetime
*TF_DTK_STOPTIME.activateCallbackClientData: (XtPointer) "DTK Stop Time"
*TF_DTK_STOPTIME.focusCallback.source: public
*TF_DTK_STOPTIME.focusCallback: cb_toggle_cursor
*TF_DTK_STOPTIME.focusCallbackClientData: (XtPointer) TRUE
*TF_DTK_STOPTIME.losingFocusCallback.source: public
*TF_DTK_STOPTIME.losingFocusCallback: cb_toggle_cursor
*TF_DTK_STOPTIME.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_DTK_STOPTIME.modifyVerifyCallback.source: public
*TF_DTK_STOPTIME.modifyVerifyCallback: cb_filter_text
*TF_DTK_STOPTIME.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*TF_DTK_STOPTIME.width: 185

*scrolledWindow_FATapes.class: scrolledWindow
*scrolledWindow_FATapes.static: true
*scrolledWindow_FATapes.name: scrolledWindow_FATapes
*scrolledWindow_FATapes.parent: DTKManager
*scrolledWindow_FATapes.scrollingPolicy: "automatic"
*scrolledWindow_FATapes.width: 100
*scrolledWindow_FATapes.height: 145
*scrolledWindow_FATapes.isCompound: "true"
*scrolledWindow_FATapes.compoundIcon: "scrlwnd.xpm"
*scrolledWindow_FATapes.compoundName: "scrolled_Window"
*scrolledWindow_FATapes.x: 467
*scrolledWindow_FATapes.y: 367
*scrolledWindow_FATapes.sensitive: "false"

*rowColumn2.class: rowColumn
*rowColumn2.static: true
*rowColumn2.name: rowColumn2
*rowColumn2.parent: scrolledWindow_FATapes
*rowColumn2.width: 170
*rowColumn2.height: 65
*rowColumn2.isCompound: "true"
*rowColumn2.compoundIcon: "row.xpm"
*rowColumn2.compoundName: "row_Column"
*rowColumn2.x: 18
*rowColumn2.y: -12
*rowColumn2.orientation: "vertical"

*FATape_ASF.class: toggleButton
*FATape_ASF.static: true
*FATape_ASF.name: FATape_ASF
*FATape_ASF.parent: rowColumn2
*FATape_ASF.isCompound: "true"
*FATape_ASF.compoundIcon: "toggle.xpm"
*FATape_ASF.compoundName: "toggle_Button"
*FATape_ASF.x: 30
*FATape_ASF.y: 10
*FATape_ASF.indicatorSize: 20
*FATape_ASF.labelString: "ASF"

*FATape_ESA.class: toggleButton
*FATape_ESA.static: true
*FATape_ESA.name: FATape_ESA
*FATape_ESA.parent: rowColumn2
*FATape_ESA.isCompound: "true"
*FATape_ESA.compoundIcon: "toggle.xpm"
*FATape_ESA.compoundName: "toggle_Button"
*FATape_ESA.x: 35
*FATape_ESA.y: 10
*FATape_ESA.indicatorSize: 20
*FATape_ESA.labelString: "ESA"

*FATape_NASDA.class: toggleButton
*FATape_NASDA.static: true
*FATape_NASDA.name: FATape_NASDA
*FATape_NASDA.parent: rowColumn2
*FATape_NASDA.isCompound: "true"
*FATape_NASDA.compoundIcon: "toggle.xpm"
*FATape_NASDA.compoundName: "toggle_Button"
*FATape_NASDA.x: 45
*FATape_NASDA.y: 40
*FATape_NASDA.indicatorSize: 20
*FATape_NASDA.labelString: "NASDA"

*FATape_CSA.class: toggleButton
*FATape_CSA.static: true
*FATape_CSA.name: FATape_CSA
*FATape_CSA.parent: rowColumn2
*FATape_CSA.isCompound: "true"
*FATape_CSA.compoundIcon: "toggle.xpm"
*FATape_CSA.compoundName: "toggle_Button"
*FATape_CSA.x: 13
*FATape_CSA.y: 47
*FATape_CSA.indicatorSize: 20
*FATape_CSA.labelString: "CSA"

*label25.class: label
*label25.static: true
*label25.name: label25
*label25.parent: DTKManager
*label25.isCompound: "true"
*label25.compoundIcon: "label.xpm"
*label25.compoundName: "label_"
*label25.x: 299
*label25.y: 365
*label25.labelString: "ACTIVITY/\nFA TAPES:"
*label25.alignment: "alignment_end"

*frameActivity.class: frame
*frameActivity.static: true
*frameActivity.name: frameActivity
*frameActivity.parent: DTKManager
*frameActivity.width: 100
*frameActivity.height: 145
*frameActivity.isCompound: "true"
*frameActivity.compoundIcon: "frame.xpm"
*frameActivity.compoundName: "frame_"
*frameActivity.x: 364
*frameActivity.y: 367
*frameActivity.shadowType: "shadow_in"
*frameActivity.sensitive: "false"

*rowColumn1.class: rowColumn
*rowColumn1.static: true
*rowColumn1.name: rowColumn1
*rowColumn1.parent: frameActivity
*rowColumn1.width: 170
*rowColumn1.height: 130
*rowColumn1.isCompound: "true"
*rowColumn1.compoundIcon: "row.xpm"
*rowColumn1.compoundName: "row_Column"
*rowColumn1.x: 400
*rowColumn1.y: 2
*rowColumn1.orientation: "vertical"
*rowColumn1.radioBehavior: "true"
*rowColumn1.spacing: 0
*rowColumn1.packing: "pack_column"
*rowColumn1.resizeHeight: "true"

*Activity_Downlink.class: toggleButton
*Activity_Downlink.static: true
*Activity_Downlink.name: Activity_Downlink
*Activity_Downlink.parent: rowColumn1
*Activity_Downlink.isCompound: "true"
*Activity_Downlink.compoundIcon: "toggle.xpm"
*Activity_Downlink.compoundName: "toggle_Button"
*Activity_Downlink.x: 400
*Activity_Downlink.y: 3
*Activity_Downlink.indicatorSize: 20
*Activity_Downlink.labelString: "RT DOWNLK"
*Activity_Downlink.set: "true"

*Activity_Observe.class: toggleButton
*Activity_Observe.static: true
*Activity_Observe.name: Activity_Observe
*Activity_Observe.parent: rowColumn1
*Activity_Observe.isCompound: "true"
*Activity_Observe.compoundIcon: "toggle.xpm"
*Activity_Observe.compoundName: "toggle_Button"
*Activity_Observe.x: 400
*Activity_Observe.y: 34
*Activity_Observe.indicatorSize: 20
*Activity_Observe.labelString: "RT OBSERV"
*Activity_Observe.valueChangedCallback.source: public
*Activity_Observe.valueChangedCallback: cb_rtobservation_toggle

*Activity_Dump.class: toggleButton
*Activity_Dump.static: true
*Activity_Dump.name: Activity_Dump
*Activity_Dump.parent: rowColumn1
*Activity_Dump.isCompound: "true"
*Activity_Dump.compoundIcon: "toggle.xpm"
*Activity_Dump.compoundName: "toggle_Button"
*Activity_Dump.x: 400
*Activity_Dump.y: 65
*Activity_Dump.indicatorSize: 20
*Activity_Dump.labelString: "DUMP"

*Activity_Record.class: toggleButton
*Activity_Record.static: true
*Activity_Record.name: Activity_Record
*Activity_Record.parent: rowColumn1
*Activity_Record.isCompound: "true"
*Activity_Record.compoundIcon: "toggle.xpm"
*Activity_Record.compoundName: "toggle_Button"
*Activity_Record.x: 400
*Activity_Record.y: 95
*Activity_Record.indicatorSize: 20
*Activity_Record.labelString: "TAPE REC"

*optionMenu_J1_DLinkChannel.class: rowColumn
*optionMenu_J1_DLinkChannel.static: true
*optionMenu_J1_DLinkChannel.name: optionMenu_J1_DLinkChannel
*optionMenu_J1_DLinkChannel.parent: DTKManager
*optionMenu_J1_DLinkChannel.rowColumnType: "menu_option"
*optionMenu_J1_DLinkChannel.subMenuId: "subMenu_J1_DLinkChannel"
*optionMenu_J1_DLinkChannel.isCompound: "true"
*optionMenu_J1_DLinkChannel.compoundIcon: "optionM.xpm"
*optionMenu_J1_DLinkChannel.compoundName: "option_Menu"
*optionMenu_J1_DLinkChannel.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_J1_DLinkChannel.x: 591
*optionMenu_J1_DLinkChannel.y: 363
*optionMenu_J1_DLinkChannel.width: 215
*optionMenu_J1_DLinkChannel.height: 35
*optionMenu_J1_DLinkChannel.labelString: "DOWNLINK\nCHANNEL:"
*optionMenu_J1_DLinkChannel.sensitive: "false"

*subMenu_J1_DLinkChannel.class: rowColumn
*subMenu_J1_DLinkChannel.static: true
*subMenu_J1_DLinkChannel.name: subMenu_J1_DLinkChannel
*subMenu_J1_DLinkChannel.parent: optionMenu_J1_DLinkChannel
*subMenu_J1_DLinkChannel.rowColumnType: "menu_pulldown"
*subMenu_J1_DLinkChannel.labelString: ""
*subMenu_J1_DLinkChannel.x: 661
*subMenu_J1_DLinkChannel.y: 0
*subMenu_J1_DLinkChannel.mappedWhenManaged: "true"

*subMenu_J1_DLink_00.class: pushButton
*subMenu_J1_DLink_00.static: true
*subMenu_J1_DLink_00.name: subMenu_J1_DLink_00
*subMenu_J1_DLink_00.parent: subMenu_J1_DLinkChannel
*subMenu_J1_DLink_00.labelString: "00"
*subMenu_J1_DLink_00.fontList: "rockwell-bold"
*subMenu_J1_DLink_00.x: 665
*subMenu_J1_DLink_00.y: 2

*subMenu_J1_DLink_F1.class: pushButton
*subMenu_J1_DLink_F1.static: true
*subMenu_J1_DLink_F1.name: subMenu_J1_DLink_F1
*subMenu_J1_DLink_F1.parent: subMenu_J1_DLinkChannel
*subMenu_J1_DLink_F1.labelString: "F1"
*subMenu_J1_DLink_F1.fontList: "rockwell-bold"
*subMenu_J1_DLink_F1.x: 665
*subMenu_J1_DLink_F1.y: 24

*subMenu_J1_DLink_F2.class: pushButton
*subMenu_J1_DLink_F2.static: true
*subMenu_J1_DLink_F2.name: subMenu_J1_DLink_F2
*subMenu_J1_DLink_F2.parent: subMenu_J1_DLinkChannel
*subMenu_J1_DLink_F2.labelString: "F2"
*subMenu_J1_DLink_F2.fontList: "rockwell-bold"
*subMenu_J1_DLink_F2.x: 665
*subMenu_J1_DLink_F2.y: 46

*subMenu_J1_DLink_CB.class: pushButton
*subMenu_J1_DLink_CB.static: true
*subMenu_J1_DLink_CB.name: subMenu_J1_DLink_CB
*subMenu_J1_DLink_CB.parent: subMenu_J1_DLinkChannel
*subMenu_J1_DLink_CB.labelString: "CB"
*subMenu_J1_DLink_CB.fontList: "rockwell-bold"
*subMenu_J1_DLink_CB.x: 665
*subMenu_J1_DLink_CB.y: 68

*subMenu_R1_DLink_F3.class: pushButton
*subMenu_R1_DLink_F3.static: true
*subMenu_R1_DLink_F3.name: subMenu_R1_DLink_F3
*subMenu_R1_DLink_F3.parent: subMenu_J1_DLinkChannel
*subMenu_R1_DLink_F3.labelString: "F3"
*subMenu_R1_DLink_F3.fontList: "rockwell-bold"
*subMenu_R1_DLink_F3.x: 665
*subMenu_R1_DLink_F3.y: 90

*subMenu_R1_DLink_F4.class: pushButton
*subMenu_R1_DLink_F4.static: true
*subMenu_R1_DLink_F4.name: subMenu_R1_DLink_F4
*subMenu_R1_DLink_F4.parent: subMenu_J1_DLinkChannel
*subMenu_R1_DLink_F4.labelString: "F4"
*subMenu_R1_DLink_F4.fontList: "rockwell-bold"
*subMenu_R1_DLink_F4.x: 665
*subMenu_R1_DLink_F4.y: 112

*subMenu_A1_DLink_F5.class: pushButton
*subMenu_A1_DLink_F5.static: true
*subMenu_A1_DLink_F5.name: subMenu_A1_DLink_F5
*subMenu_A1_DLink_F5.parent: subMenu_J1_DLinkChannel
*subMenu_A1_DLink_F5.labelString: "F5"
*subMenu_A1_DLink_F5.fontList: "rockwell-bold"
*subMenu_A1_DLink_F5.x: 665
*subMenu_A1_DLink_F5.y: 134

*subMenu_A1_DLink_F6.class: pushButton
*subMenu_A1_DLink_F6.static: true
*subMenu_A1_DLink_F6.name: subMenu_A1_DLink_F6
*subMenu_A1_DLink_F6.parent: subMenu_J1_DLinkChannel
*subMenu_A1_DLink_F6.labelString: "F6"
*subMenu_A1_DLink_F6.fontList: "rockwell-bold"
*subMenu_A1_DLink_F6.x: 665
*subMenu_A1_DLink_F6.y: 156

*subMenu_A1_DLink_F7.class: pushButton
*subMenu_A1_DLink_F7.static: true
*subMenu_A1_DLink_F7.name: subMenu_A1_DLink_F7
*subMenu_A1_DLink_F7.parent: subMenu_J1_DLinkChannel
*subMenu_A1_DLink_F7.labelString: "F7"
*subMenu_A1_DLink_F7.fontList: "rockwell-bold"
*subMenu_A1_DLink_F7.x: 665
*subMenu_A1_DLink_F7.y: 178

*pushButton_GetCvrgPoints.class: pushButton
*pushButton_GetCvrgPoints.static: true
*pushButton_GetCvrgPoints.name: pushButton_GetCvrgPoints
*pushButton_GetCvrgPoints.parent: DTKManager
*pushButton_GetCvrgPoints.isCompound: "true"
*pushButton_GetCvrgPoints.compoundIcon: "push.xpm"
*pushButton_GetCvrgPoints.compoundName: "push_Button"
*pushButton_GetCvrgPoints.x: 63
*pushButton_GetCvrgPoints.y: 608
*pushButton_GetCvrgPoints.width: 65
*pushButton_GetCvrgPoints.height: 70
*pushButton_GetCvrgPoints.labelString: "GET\nCVRG\nPOINTS"
*pushButton_GetCvrgPoints.fontList: "rockwell-bold"
*pushButton_GetCvrgPoints.activateCallback.source: public
*pushButton_GetCvrgPoints.activateCallback: cb_get_cvrg_points
*pushButton_GetCvrgPoints.sensitive: "false"

*optionMenu_dtkm_station_id.class: rowColumn
*optionMenu_dtkm_station_id.static: true
*optionMenu_dtkm_station_id.name: optionMenu_dtkm_station_id
*optionMenu_dtkm_station_id.parent: DTKManager
*optionMenu_dtkm_station_id.rowColumnType: "menu_option"
*optionMenu_dtkm_station_id.subMenuId: "subMenu_dtkm_stnid"
*optionMenu_dtkm_station_id.isCompound: "true"
*optionMenu_dtkm_station_id.compoundIcon: "optionM.xpm"
*optionMenu_dtkm_station_id.compoundName: "option_Menu"
*optionMenu_dtkm_station_id.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_dtkm_station_id.x: 583
*optionMenu_dtkm_station_id.y: 318
*optionMenu_dtkm_station_id.width: 215
*optionMenu_dtkm_station_id.height: 35
*optionMenu_dtkm_station_id.labelString: "STATION ID\n/ANTENNA:"
*optionMenu_dtkm_station_id.sensitive: "false"

*subMenu_dtkm_stnid.class: rowColumn
*subMenu_dtkm_stnid.static: true
*subMenu_dtkm_stnid.name: subMenu_dtkm_stnid
*subMenu_dtkm_stnid.parent: optionMenu_dtkm_station_id
*subMenu_dtkm_stnid.rowColumnType: "menu_pulldown"
*subMenu_dtkm_stnid.labelString: ""
*subMenu_dtkm_stnid.height: 48
*subMenu_dtkm_stnid.resizeHeight: "false"
*subMenu_dtkm_stnid.x: 0
*subMenu_dtkm_stnid.y: 315
*subMenu_dtkm_stnid.mappedWhenManaged: "true"

*subMenu_dtkm_stnid_asf.class: pushButton
*subMenu_dtkm_stnid_asf.static: true
*subMenu_dtkm_stnid_asf.name: subMenu_dtkm_stnid_asf
*subMenu_dtkm_stnid_asf.parent: subMenu_dtkm_stnid
*subMenu_dtkm_stnid_asf.labelString: "ASF"
*subMenu_dtkm_stnid_asf.fontList: "rockwell-bold"
*subMenu_dtkm_stnid_asf.x: 2
*subMenu_dtkm_stnid_asf.y: 328
*subMenu_dtkm_stnid_asf.createCallback.source: public
*subMenu_dtkm_stnid_asf.createCallback: cb_build_station_option_menu

*label34.class: label
*label34.static: true
*label34.name: label34
*label34.parent: DTKManager
*label34.isCompound: "true"
*label34.compoundIcon: "label.xpm"
*label34.compoundName: "label_"
*label34.x: 728
*label34.y: 363
*label34.height: 30
*label34.labelString: "SCIENCE QUICKLOOK:"
*label34.alignment: "alignment_end"
*label34.width: 114

*label_SciQuicklook.class: label
*label_SciQuicklook.static: true
*label_SciQuicklook.name: label_SciQuicklook
*label_SciQuicklook.parent: DTKManager
*label_SciQuicklook.isCompound: "true"
*label_SciQuicklook.compoundIcon: "label.xpm"
*label_SciQuicklook.compoundName: "label_"
*label_SciQuicklook.x: 843
*label_SciQuicklook.y: 368
*label_SciQuicklook.height: 20
*label_SciQuicklook.labelString: "No"
*label_SciQuicklook.alignment: "alignment_end"
*label_SciQuicklook.width: 25

*optionMenu_antenna.class: rowColumn
*optionMenu_antenna.static: true
*optionMenu_antenna.name: optionMenu_antenna
*optionMenu_antenna.parent: DTKManager
*optionMenu_antenna.rowColumnType: "menu_option"
*optionMenu_antenna.subMenuId: "subMenu_antenna"
*optionMenu_antenna.isCompound: "true"
*optionMenu_antenna.compoundIcon: "optionM.xpm"
*optionMenu_antenna.compoundName: "option_Menu"
*optionMenu_antenna.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_antenna.x: 705
*optionMenu_antenna.y: 318
*optionMenu_antenna.width: 215
*optionMenu_antenna.height: 35
*optionMenu_antenna.labelString: "/"
*optionMenu_antenna.sensitive: "false"

*subMenu_antenna.class: rowColumn
*subMenu_antenna.static: true
*subMenu_antenna.name: subMenu_antenna
*subMenu_antenna.parent: optionMenu_antenna
*subMenu_antenna.rowColumnType: "menu_pulldown"
*subMenu_antenna.labelString: ""
*subMenu_antenna.x: 0
*subMenu_antenna.y: 326
*subMenu_antenna.mappedWhenManaged: "true"

*subMenu_antenna_1.class: pushButton
*subMenu_antenna_1.static: true
*subMenu_antenna_1.name: subMenu_antenna_1
*subMenu_antenna_1.parent: subMenu_antenna
*subMenu_antenna_1.labelString: "1"
*subMenu_antenna_1.fontList: "rockwell-bold"
*subMenu_antenna_1.x: 2
*subMenu_antenna_1.y: 328
*subMenu_antenna_1.createCallback.source: public
*subMenu_antenna_1.createCallback: cb_build_antenna_option_menu
*subMenu_antenna_1.createCallbackClientData: (XtPointer) True

*label_FA_Schedule_Link.class: label
*label_FA_Schedule_Link.static: true
*label_FA_Schedule_Link.name: label_FA_Schedule_Link
*label_FA_Schedule_Link.parent: DTKManager
*label_FA_Schedule_Link.isCompound: "true"
*label_FA_Schedule_Link.compoundIcon: "label.xpm"
*label_FA_Schedule_Link.compoundName: "label_"
*label_FA_Schedule_Link.x: 574
*label_FA_Schedule_Link.y: 481
*label_FA_Schedule_Link.height: 30
*label_FA_Schedule_Link.labelString: "FA SCHEDULE LINK:"
*label_FA_Schedule_Link.alignment: "alignment_end"

*TF_FA_SCHEDULE_LINK.class: textField
*TF_FA_SCHEDULE_LINK.static: true
*TF_FA_SCHEDULE_LINK.name: TF_FA_SCHEDULE_LINK
*TF_FA_SCHEDULE_LINK.parent: DTKManager
*TF_FA_SCHEDULE_LINK.width: 186
*TF_FA_SCHEDULE_LINK.isCompound: "true"
*TF_FA_SCHEDULE_LINK.compoundIcon: "textfield.xpm"
*TF_FA_SCHEDULE_LINK.compoundName: "text_Field"
*TF_FA_SCHEDULE_LINK.x: 683
*TF_FA_SCHEDULE_LINK.y: 480
*TF_FA_SCHEDULE_LINK.height: 32
*TF_FA_SCHEDULE_LINK.maxLength: 20
*TF_FA_SCHEDULE_LINK.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_FA_SCHEDULE_LINK.editable: "false"
*TF_FA_SCHEDULE_LINK.sensitive: "false"

*separator8.class: separator
*separator8.static: true
*separator8.name: separator8
*separator8.parent: DTKManager
*separator8.width: 886
*separator8.height: 12
*separator8.isCompound: "true"
*separator8.compoundIcon: "sep.xpm"
*separator8.compoundName: "separator_"
*separator8.x: 0
*separator8.y: 594

*label63.class: label
*label63.static: true
*label63.name: label63
*label63.parent: DTKManager
*label63.isCompound: "true"
*label63.compoundIcon: "label.xpm"
*label63.compoundName: "label_"
*label63.height: 25
*label63.labelString: "DTK ID:"
*label63.x: 472
*label63.y: 326
*label63.topOffset: 324

*optionMenu_PlanQuicklook.class: rowColumn
*optionMenu_PlanQuicklook.static: true
*optionMenu_PlanQuicklook.name: optionMenu_PlanQuicklook
*optionMenu_PlanQuicklook.rowColumnType: "menu_option"
*optionMenu_PlanQuicklook.subMenuId: "subMenu_PlanQuicklook"
*optionMenu_PlanQuicklook.isCompound: "true"
*optionMenu_PlanQuicklook.compoundIcon: "optionM.xpm"
*optionMenu_PlanQuicklook.compoundName: "option_Menu"
*optionMenu_PlanQuicklook.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_PlanQuicklook.x: 701
*optionMenu_PlanQuicklook.y: 398
*optionMenu_PlanQuicklook.width: 215
*optionMenu_PlanQuicklook.height: 35
*optionMenu_PlanQuicklook.labelString: "PLANNER QUICKLOOK:"
*optionMenu_PlanQuicklook.sensitive: "false"
*optionMenu_PlanQuicklook.parent: DTKManager

*subMenu_PlanQuicklook.class: rowColumn
*subMenu_PlanQuicklook.static: true
*subMenu_PlanQuicklook.name: subMenu_PlanQuicklook
*subMenu_PlanQuicklook.rowColumnType: "menu_pulldown"
*subMenu_PlanQuicklook.labelString: ""
*subMenu_PlanQuicklook.x: 661
*subMenu_PlanQuicklook.y: 0
*subMenu_PlanQuicklook.mappedWhenManaged: "true"
*subMenu_PlanQuicklook.parent: optionMenu_PlanQuicklook

*subMenu_PlanQuicklook_no.class: pushButton
*subMenu_PlanQuicklook_no.static: true
*subMenu_PlanQuicklook_no.name: subMenu_PlanQuicklook_no
*subMenu_PlanQuicklook_no.labelString: "NO"
*subMenu_PlanQuicklook_no.fontList: "rockwell-bold"
*subMenu_PlanQuicklook_no.x: 665
*subMenu_PlanQuicklook_no.y: 24
*subMenu_PlanQuicklook_no.parent: subMenu_PlanQuicklook
*subMenu_PlanQuicklook_no.activateCallback.source: public
*subMenu_PlanQuicklook_no.activateCallback: 

*subMenu_PlanQuicklook_yes.class: pushButton
*subMenu_PlanQuicklook_yes.static: true
*subMenu_PlanQuicklook_yes.name: subMenu_PlanQuicklook_yes
*subMenu_PlanQuicklook_yes.labelString: "YES"
*subMenu_PlanQuicklook_yes.fontList: "rockwell-bold"
*subMenu_PlanQuicklook_yes.x: 665
*subMenu_PlanQuicklook_yes.y: 2
*subMenu_PlanQuicklook_yes.parent: subMenu_PlanQuicklook

