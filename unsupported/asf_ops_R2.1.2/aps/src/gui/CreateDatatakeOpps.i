! UIMX ascii 2.9 key: 2054                                                      

*CreateDatatakeOpps.class: form
*CreateDatatakeOpps.classinc:
*CreateDatatakeOpps.classspec:
*CreateDatatakeOpps.classmembers:
*CreateDatatakeOpps.classconstructor:
*CreateDatatakeOpps.classdestructor:
*CreateDatatakeOpps.gbldecl: #include <stdio.h>\
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
	**************************************************************************\
	* Create Specific Site Coverage USED TO BE Create Datatake Opportunities *\
	* so some of the names are still relics from the old title               *\
	**************************************************************************\
 \
==============================================================================*/\
#pragma ident   "@(#)CreateDatatakeOpps.i	5.2 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.CreateDatatakeOpps.i"\
\
#include <stdlib.h>\
\
#include "cb_cdtakeopps.h"\
#include "cb_datetime.h"\
#include "satmenus.h"\
\
extern XtCallbackProc check ;\
extern void cb_set_sensor_menu();\
extern Widget cdtakeopps_form ;\

*CreateDatatakeOpps.ispecdecl:
*CreateDatatakeOpps.funcdecl: swidget create_CreateDatatakeOpps(swidget UxParent)
*CreateDatatakeOpps.funcname: create_CreateDatatakeOpps
*CreateDatatakeOpps.funcdef: "swidget", "<create_CreateDatatakeOpps>(%)"
*CreateDatatakeOpps.argdecl: swidget UxParent;
*CreateDatatakeOpps.arglist: UxParent
*CreateDatatakeOpps.arglist.UxParent: "swidget", "%UxParent%"
*CreateDatatakeOpps.icode: Position x, y ;\
OPTION_MENU_WIDGETS *sensor_menu ;\
PERIOD_WIDGETS *dtkopps_times ;\
PERIOD_WIDGETS *dtkopps_revs ;
*CreateDatatakeOpps.fcode: sensor_menu =\
	(OPTION_MENU_WIDGETS *) malloc(sizeof(OPTION_MENU_WIDGETS)) ;\
sensor_menu->optionmenu = (Widget) optionMenu_cdtk_sensor ;\
sensor_menu->submenu = (Widget) subMenu_cdtk_sensor ;\
\
dtkopps_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
dtkopps_times->start = (Widget) TF_dtkopps_start ;\
dtkopps_times->stop = (Widget) TF_dtkopps_end ;\
\
dtkopps_revs = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
dtkopps_revs->start = (Widget) TF_start_rev ;\
dtkopps_revs->stop = (Widget) TF_stop_rev ;\
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
    XtVaGetValues(form_quad,\
        XmNx, &x,\
        XmNy, &y,\
        NULL) ;\
\
     XtVaSetValues(form_circle,\
        XmNx, x,\
        XmNy, y,\
        NULL) ;\
\
XtAddCallback( subMenu_cdtk_sat, XmNentryCallback,\
    (XtCallbackProc) cb_set_cvrg_allowed_sensor_menus,\
    (XtPointer ) sensor_menu );\
\
XtAddCallback(TF_dtkopps_total_days, XmNactivateCallback,\
	(XtCallbackProc) cb_adjust_ASF_datetimes,\
	(XtPointer) dtkopps_times) ;\
\
\
XtAddCallback(TF_total_revs, XmNactivateCallback,\
	(XtCallbackProc) cb_adjust_revs,\
	(XtPointer) dtkopps_revs) ;\
\
\
XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
	cb_show_dar_relations, (XtPointer *) scrolledList_sites) ;\
\
return(rtrn);\

*CreateDatatakeOpps.auxdecl:
*CreateDatatakeOpps.static: true
*CreateDatatakeOpps.name: CreateDatatakeOpps
*CreateDatatakeOpps.parent: NO_PARENT
*CreateDatatakeOpps.parentExpression: UxParent
*CreateDatatakeOpps.defaultShell: topLevelShell
*CreateDatatakeOpps.width: 856
*CreateDatatakeOpps.height: 828
*CreateDatatakeOpps.resizePolicy: "resize_none"
*CreateDatatakeOpps.isCompound: "true"
*CreateDatatakeOpps.compoundIcon: "form.xpm"
*CreateDatatakeOpps.compoundName: "form_"
*CreateDatatakeOpps.x: 286
*CreateDatatakeOpps.unitType: "pixels"
*CreateDatatakeOpps.dialogTitle: "APS:Create Specific Site Coverage"
*CreateDatatakeOpps.y: -14

*label37.class: label
*label37.static: true
*label37.name: label37
*label37.parent: CreateDatatakeOpps
*label37.isCompound: "true"
*label37.compoundIcon: "label.xpm"
*label37.compoundName: "label_"
*label37.x: 69
*label37.y: 12
*label37.width: 700
*label37.height: 40
*label37.labelString: "CREATE  SPECIFIC  SITE  COVERAGE"
*label37.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindowList3.class: scrolledWindow
*scrolledWindowList3.static: true
*scrolledWindowList3.name: scrolledWindowList3
*scrolledWindowList3.parent: CreateDatatakeOpps
*scrolledWindowList3.scrollingPolicy: "application_defined"
*scrolledWindowList3.visualPolicy: "variable"
*scrolledWindowList3.scrollBarDisplayPolicy: "static"
*scrolledWindowList3.shadowThickness: 0
*scrolledWindowList3.isCompound: "true"
*scrolledWindowList3.compoundIcon: "scrllist.xpm"
*scrolledWindowList3.compoundName: "scrolled_List"
*scrolledWindowList3.x: 75
*scrolledWindowList3.y: 100
*scrolledWindowList3.width: 275

*scrolledList_sites.class: scrolledList
*scrolledList_sites.static: true
*scrolledList_sites.name: scrolledList_sites
*scrolledList_sites.parent: scrolledWindowList3
*scrolledList_sites.width: 310
*scrolledList_sites.itemCount: 1
*scrolledList_sites.items: "12345 12345678901234567890123456789012"
*scrolledList_sites.defaultActionCallback.source: public
*scrolledList_sites.defaultActionCallback: cb_update_cdtakeopps_form
*scrolledList_sites.browseSelectionCallback.source: public
*scrolledList_sites.browseSelectionCallback: cb_update_cdtakeopps_form
*scrolledList_sites.visibleItemCount: 13
*scrolledList_sites.listSizePolicy: "resize_if_possible"

*TF_COMMENTS.class: textField
*TF_COMMENTS.static: true
*TF_COMMENTS.name: TF_COMMENTS
*TF_COMMENTS.parent: CreateDatatakeOpps
*TF_COMMENTS.isCompound: "true"
*TF_COMMENTS.compoundIcon: "textfield.xpm"
*TF_COMMENTS.compoundName: "text_Field"
*TF_COMMENTS.x: 419
*TF_COMMENTS.y: 163
*TF_COMMENTS.height: 32
*TF_COMMENTS.cursorPositionVisible: "false"
*TF_COMMENTS.editable: "false"
*TF_COMMENTS.sensitive: "false"
*TF_COMMENTS.text: ""
*TF_COMMENTS.columns: 40
*TF_COMMENTS.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_COMMENTS.resizeWidth: "false"
*TF_COMMENTS.width: 365

*TF_DARID.class: textField
*TF_DARID.static: true
*TF_DARID.name: TF_DARID
*TF_DARID.parent: CreateDatatakeOpps
*TF_DARID.isCompound: "true"
*TF_DARID.compoundIcon: "textfield.xpm"
*TF_DARID.compoundName: "text_Field"
*TF_DARID.x: 419
*TF_DARID.y: 76
*TF_DARID.height: 32
*TF_DARID.cursorPositionVisible: "false"
*TF_DARID.editable: "false"
*TF_DARID.sensitive: "false"
*TF_DARID.text: ""
*TF_DARID.columns: 5
*TF_DARID.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_DARID.resizeWidth: "false"
*TF_DARID.maxLength: 5
*TF_DARID.traversalOn: "false"

*label53.class: label
*label53.static: true
*label53.name: label53
*label53.parent: CreateDatatakeOpps
*label53.isCompound: "true"
*label53.compoundIcon: "label.xpm"
*label53.compoundName: "label_"
*label53.x: 390
*label53.y: 77
*label53.width: 30
*label53.height: 30
*label53.labelString: "DAR\nID:"
*label53.alignment: "alignment_end"

*TF_QUICKLOOK.class: label
*TF_QUICKLOOK.static: true
*TF_QUICKLOOK.name: TF_QUICKLOOK
*TF_QUICKLOOK.parent: CreateDatatakeOpps
*TF_QUICKLOOK.isCompound: "true"
*TF_QUICKLOOK.compoundIcon: "label.xpm"
*TF_QUICKLOOK.compoundName: "label_"
*TF_QUICKLOOK.x: 560
*TF_QUICKLOOK.y: 82
*TF_QUICKLOOK.height: 20
*TF_QUICKLOOK.labelString: "No"
*TF_QUICKLOOK.alignment: "alignment_end"
*TF_QUICKLOOK.width: 25

*label5.class: label
*label5.static: true
*label5.name: label5
*label5.parent: CreateDatatakeOpps
*label5.isCompound: "true"
*label5.compoundIcon: "label.xpm"
*label5.compoundName: "label_"
*label5.x: 494
*label5.y: 77
*label5.height: 30
*label5.labelString: "QUICKLOOK:"
*label5.alignment: "alignment_end"
*label5.width: 65

*TF_SITENAME.class: textField
*TF_SITENAME.static: true
*TF_SITENAME.name: TF_SITENAME
*TF_SITENAME.parent: CreateDatatakeOpps
*TF_SITENAME.isCompound: "true"
*TF_SITENAME.compoundIcon: "textfield.xpm"
*TF_SITENAME.compoundName: "text_Field"
*TF_SITENAME.x: 419
*TF_SITENAME.y: 117
*TF_SITENAME.height: 32
*TF_SITENAME.cursorPositionVisible: "false"
*TF_SITENAME.editable: "false"
*TF_SITENAME.sensitive: "false"
*TF_SITENAME.text: ""
*TF_SITENAME.columns: 32
*TF_SITENAME.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SITENAME.resizeWidth: "false"
*TF_SITENAME.maxLength: 32

*label54.class: label
*label54.static: true
*label54.name: label54
*label54.parent: CreateDatatakeOpps
*label54.isCompound: "true"
*label54.compoundIcon: "label.xpm"
*label54.compoundName: "label_"
*label54.x: 384
*label54.y: 118
*label54.width: 35
*label54.height: 30
*label54.labelString: "SITE \nNAME:"
*label54.alignment: "alignment_end"

*TF_SHAPE.class: textField
*TF_SHAPE.static: true
*TF_SHAPE.name: TF_SHAPE
*TF_SHAPE.parent: CreateDatatakeOpps
*TF_SHAPE.isCompound: "true"
*TF_SHAPE.compoundIcon: "textfield.xpm"
*TF_SHAPE.compoundName: "text_Field"
*TF_SHAPE.x: 419
*TF_SHAPE.y: 204
*TF_SHAPE.height: 32
*TF_SHAPE.cursorPositionVisible: "false"
*TF_SHAPE.editable: "false"
*TF_SHAPE.sensitive: "false"
*TF_SHAPE.text: ""
*TF_SHAPE.columns: 6
*TF_SHAPE.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SHAPE.resizeWidth: "false"

*label55.class: label
*label55.static: true
*label55.name: label55
*label55.parent: CreateDatatakeOpps
*label55.isCompound: "true"
*label55.compoundIcon: "label.xpm"
*label55.compoundName: "label_"
*label55.x: 378
*label55.y: 204
*label55.width: 40
*label55.height: 30
*label55.labelString: "SHAPE:"
*label55.alignment: "alignment_end"

*label56.class: label
*label56.static: true
*label56.name: label56
*label56.parent: CreateDatatakeOpps
*label56.isCompound: "true"
*label56.compoundIcon: "label.xpm"
*label56.compoundName: "label_"
*label56.x: 359
*label56.y: 164
*label56.width: 60
*label56.height: 30
*label56.labelString: "COMMENTS:"
*label56.alignment: "alignment_end"

*rowColumn3.class: rowColumn
*rowColumn3.static: true
*rowColumn3.name: rowColumn3
*rowColumn3.parent: CreateDatatakeOpps
*rowColumn3.width: 137
*rowColumn3.height: 20
*rowColumn3.isCompound: "true"
*rowColumn3.compoundIcon: "row.xpm"
*rowColumn3.compoundName: "row_Column"
*rowColumn3.x: 134
*rowColumn3.y: 320
*rowColumn3.orientation: "horizontal"
*rowColumn3.radioBehavior: "true"
*rowColumn3.labelString: ""
*rowColumn3.numColumns: 1
*rowColumn3.packing: "pack_tight"
*rowColumn3.whichButton: 1

*toggleButton_DARSites.class: toggleButton
*toggleButton_DARSites.static: true
*toggleButton_DARSites.name: toggleButton_DARSites
*toggleButton_DARSites.parent: rowColumn3
*toggleButton_DARSites.isCompound: "true"
*toggleButton_DARSites.compoundIcon: "toggle.xpm"
*toggleButton_DARSites.compoundName: "toggle_Button"
*toggleButton_DARSites.x: 3
*toggleButton_DARSites.y: 282
*toggleButton_DARSites.width: 67
*toggleButton_DARSites.height: 12
*toggleButton_DARSites.labelString: "DAR"
*toggleButton_DARSites.set: "true"
*toggleButton_DARSites.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_DARSites.indicatorSize: 20
*toggleButton_DARSites.valueChangedCallback.source: public
*toggleButton_DARSites.valueChangedCallback: cb_display_dar_sites

*toggleButton_HypoSites.class: toggleButton
*toggleButton_HypoSites.static: true
*toggleButton_HypoSites.name: toggleButton_HypoSites
*toggleButton_HypoSites.parent: rowColumn3
*toggleButton_HypoSites.isCompound: "true"
*toggleButton_HypoSites.compoundIcon: "toggle.xpm"
*toggleButton_HypoSites.compoundName: "toggle_Button"
*toggleButton_HypoSites.x: 56
*toggleButton_HypoSites.y: 282
*toggleButton_HypoSites.width: 61
*toggleButton_HypoSites.height: 17
*toggleButton_HypoSites.labelString: "Hypothetical"
*toggleButton_HypoSites.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_HypoSites.indicatorSize: 20
*toggleButton_HypoSites.valueChangedCallback.source: public
*toggleButton_HypoSites.valueChangedCallback: cb_display_hypo_sites

*label60.class: label
*label60.static: true
*label60.name: label60
*label60.parent: CreateDatatakeOpps
*label60.isCompound: "true"
*label60.compoundIcon: "label.xpm"
*label60.compoundName: "label_"
*label60.x: 80
*label60.y: 327
*label60.width: 49
*label60.height: 30
*label60.labelString: "DISPLAY\nSITES:"
*label60.alignment: "alignment_end"

*scrolledWindowText2.class: scrolledWindow
*scrolledWindowText2.static: true
*scrolledWindowText2.name: scrolledWindowText2
*scrolledWindowText2.parent: CreateDatatakeOpps
*scrolledWindowText2.scrollingPolicy: "application_defined"
*scrolledWindowText2.visualPolicy: "variable"
*scrolledWindowText2.scrollBarDisplayPolicy: "static"
*scrolledWindowText2.isCompound: "true"
*scrolledWindowText2.compoundIcon: "scrltext.xpm"
*scrolledWindowText2.compoundName: "scrolled_Text"
*scrolledWindowText2.x: 27
*scrolledWindowText2.y: 645
*scrolledWindowText2.width: 800
*scrolledWindowText2.height: 160

*scrolledText_create_dtkopps.class: scrolledText
*scrolledText_create_dtkopps.static: true
*scrolledText_create_dtkopps.name: scrolledText_create_dtkopps
*scrolledText_create_dtkopps.parent: scrolledWindowText2
*scrolledText_create_dtkopps.height: 141
*scrolledText_create_dtkopps.editMode: "multi_line_edit"
*scrolledText_create_dtkopps.editable: "false"
*scrolledText_create_dtkopps.cursorPositionVisible: "true"

*separator5.class: separator
*separator5.static: true
*separator5.name: separator5
*separator5.parent: CreateDatatakeOpps
*separator5.width: 855
*separator5.height: 15
*separator5.isCompound: "true"
*separator5.compoundIcon: "sep.xpm"
*separator5.compoundName: "separator_"
*separator5.x: 0
*separator5.y: 604

*label52.class: label
*label52.static: true
*label52.name: label52
*label52.parent: CreateDatatakeOpps
*label52.isCompound: "true"
*label52.compoundIcon: "label.xpm"
*label52.compoundName: "label_"
*label52.x: 134
*label52.y: 623
*label52.width: 590
*label52.height: 20
*label52.labelString: "MESSAGES"
*label52.fontList: "-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1"

*pushButton_cdtakeopps_quit.class: pushButton
*pushButton_cdtakeopps_quit.static: true
*pushButton_cdtakeopps_quit.name: pushButton_cdtakeopps_quit
*pushButton_cdtakeopps_quit.parent: CreateDatatakeOpps
*pushButton_cdtakeopps_quit.isCompound: "true"
*pushButton_cdtakeopps_quit.compoundIcon: "push.xpm"
*pushButton_cdtakeopps_quit.compoundName: "push_Button"
*pushButton_cdtakeopps_quit.x: 630
*pushButton_cdtakeopps_quit.y: 559
*pushButton_cdtakeopps_quit.width: 120
*pushButton_cdtakeopps_quit.height: 40
*pushButton_cdtakeopps_quit.labelString: "QUIT"
*pushButton_cdtakeopps_quit.fontList: "rockwell-bold"
*pushButton_cdtakeopps_quit.activateCallback: {\
XtPopdown(XtParent(cdtakeopps_form)) ;\
}

*pushButton_create_dtkopps.class: pushButton
*pushButton_create_dtkopps.static: true
*pushButton_create_dtkopps.name: pushButton_create_dtkopps
*pushButton_create_dtkopps.parent: CreateDatatakeOpps
*pushButton_create_dtkopps.isCompound: "true"
*pushButton_create_dtkopps.compoundIcon: "push.xpm"
*pushButton_create_dtkopps.compoundName: "push_Button"
*pushButton_create_dtkopps.width: 130
*pushButton_create_dtkopps.height: 40
*pushButton_create_dtkopps.labelString: "CREATE\nSITE  COVERAGE"
*pushButton_create_dtkopps.fontList: "rockwell-bold"
*pushButton_create_dtkopps.activateCallback.source: public
*pushButton_create_dtkopps.activateCallback: cb_do_create_dtk_opps
*pushButton_create_dtkopps.sensitive: "true"
*pushButton_create_dtkopps.x: 100
*pushButton_create_dtkopps.y: 559

*label67.class: label
*label67.static: true
*label67.name: label67
*label67.parent: CreateDatatakeOpps
*label67.isCompound: "true"
*label67.compoundIcon: "label.xpm"
*label67.compoundName: "label_"
*label67.x: 75
*label67.y: 70
*label67.width: 270
*label67.height: 20
*label67.labelString: "DAR ID      SITE NAME"
*label67.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label67.alignment: "alignment_beginning"

*form_quad.class: form
*form_quad.static: true
*form_quad.name: form_quad
*form_quad.parent: CreateDatatakeOpps
*form_quad.resizePolicy: "resize_none"
*form_quad.x: 370
*form_quad.y: 238
*form_quad.width: 420
*form_quad.height: 80

*TF_NW_LON.class: textField
*TF_NW_LON.static: true
*TF_NW_LON.name: TF_NW_LON
*TF_NW_LON.parent: form_quad
*TF_NW_LON.isCompound: "true"
*TF_NW_LON.compoundIcon: "textfield.xpm"
*TF_NW_LON.compoundName: "text_Field"
*TF_NW_LON.x: 140
*TF_NW_LON.y: 10
*TF_NW_LON.height: 30
*TF_NW_LON.cursorPositionVisible: "false"
*TF_NW_LON.editable: "false"
*TF_NW_LON.sensitive: "false"
*TF_NW_LON.text: ""
*TF_NW_LON.columns: 7
*TF_NW_LON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NW_LON.resizeWidth: "false"

*label38.class: label
*label38.static: true
*label38.name: label38
*label38.parent: form_quad
*label38.isCompound: "true"
*label38.compoundIcon: "label.xpm"
*label38.compoundName: "label_"
*label38.x: 125
*label38.y: 15
*label38.width: 15
*label38.height: 20
*label38.labelString: "/"

*TF_NE_LON.class: textField
*TF_NE_LON.static: true
*TF_NE_LON.name: TF_NE_LON
*TF_NE_LON.parent: form_quad
*TF_NE_LON.isCompound: "true"
*TF_NE_LON.compoundIcon: "textfield.xpm"
*TF_NE_LON.compoundName: "text_Field"
*TF_NE_LON.x: 345
*TF_NE_LON.y: 10
*TF_NE_LON.height: 30
*TF_NE_LON.cursorPositionVisible: "false"
*TF_NE_LON.editable: "false"
*TF_NE_LON.sensitive: "false"
*TF_NE_LON.text: ""
*TF_NE_LON.columns: 7
*TF_NE_LON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NE_LON.resizeWidth: "false"

*label45.class: label
*label45.static: true
*label45.name: label45
*label45.parent: form_quad
*label45.isCompound: "true"
*label45.compoundIcon: "label.xpm"
*label45.compoundName: "label_"
*label45.x: 330
*label45.y: 15
*label45.width: 15
*label45.height: 20
*label45.labelString: "/"

*label_NE.class: label
*label_NE.static: true
*label_NE.name: label_NE
*label_NE.parent: form_quad
*label_NE.isCompound: "true"
*label_NE.compoundIcon: "label.xpm"
*label_NE.compoundName: "label_"
*label_NE.x: 220
*label_NE.y: 10
*label_NE.width: 36
*label_NE.height: 30
*label_NE.labelString: "NE:"
*label_NE.alignment: "alignment_end"

*TF_SW_LON.class: textField
*TF_SW_LON.static: true
*TF_SW_LON.name: TF_SW_LON
*TF_SW_LON.parent: form_quad
*TF_SW_LON.isCompound: "true"
*TF_SW_LON.compoundIcon: "textfield.xpm"
*TF_SW_LON.compoundName: "text_Field"
*TF_SW_LON.x: 140
*TF_SW_LON.y: 45
*TF_SW_LON.height: 30
*TF_SW_LON.cursorPositionVisible: "false"
*TF_SW_LON.editable: "false"
*TF_SW_LON.sensitive: "false"
*TF_SW_LON.text: ""
*TF_SW_LON.columns: 7
*TF_SW_LON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SW_LON.resizeWidth: "false"

*label47.class: label
*label47.static: true
*label47.name: label47
*label47.parent: form_quad
*label47.isCompound: "true"
*label47.compoundIcon: "label.xpm"
*label47.compoundName: "label_"
*label47.x: 125
*label47.y: 50
*label47.width: 15
*label47.height: 20
*label47.labelString: "/"

*label48.class: label
*label48.static: true
*label48.name: label48
*label48.parent: form_quad
*label48.isCompound: "true"
*label48.compoundIcon: "label.xpm"
*label48.compoundName: "label_"
*label48.x: 20
*label48.y: 45
*label48.width: 26
*label48.height: 30
*label48.labelString: "SW:"
*label48.alignment: "alignment_end"

*TF_SE_LAT.class: textField
*TF_SE_LAT.static: true
*TF_SE_LAT.name: TF_SE_LAT
*TF_SE_LAT.parent: form_quad
*TF_SE_LAT.isCompound: "true"
*TF_SE_LAT.compoundIcon: "textfield.xpm"
*TF_SE_LAT.compoundName: "text_Field"
*TF_SE_LAT.x: 255
*TF_SE_LAT.y: 45
*TF_SE_LAT.height: 30
*TF_SE_LAT.cursorPositionVisible: "false"
*TF_SE_LAT.editable: "false"
*TF_SE_LAT.sensitive: "false"
*TF_SE_LAT.text: ""
*TF_SE_LAT.columns: 7
*TF_SE_LAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SE_LAT.resizeWidth: "false"

*TF_SE_LON.class: textField
*TF_SE_LON.static: true
*TF_SE_LON.name: TF_SE_LON
*TF_SE_LON.parent: form_quad
*TF_SE_LON.isCompound: "true"
*TF_SE_LON.compoundIcon: "textfield.xpm"
*TF_SE_LON.compoundName: "text_Field"
*TF_SE_LON.x: 345
*TF_SE_LON.y: 45
*TF_SE_LON.height: 30
*TF_SE_LON.cursorPositionVisible: "false"
*TF_SE_LON.editable: "false"
*TF_SE_LON.sensitive: "false"
*TF_SE_LON.text: ""
*TF_SE_LON.columns: 7
*TF_SE_LON.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SE_LON.resizeWidth: "false"

*label50.class: label
*label50.static: true
*label50.name: label50
*label50.parent: form_quad
*label50.isCompound: "true"
*label50.compoundIcon: "label.xpm"
*label50.compoundName: "label_"
*label50.x: 330
*label50.y: 50
*label50.width: 15
*label50.height: 20
*label50.labelString: "/"

*label51.class: label
*label51.static: true
*label51.name: label51
*label51.parent: form_quad
*label51.isCompound: "true"
*label51.compoundIcon: "label.xpm"
*label51.compoundName: "label_"
*label51.x: 230
*label51.y: 45
*label51.width: 26
*label51.height: 30
*label51.labelString: "SE:"
*label51.alignment: "alignment_end"

*label_NW.class: label
*label_NW.static: true
*label_NW.name: label_NW
*label_NW.parent: form_quad
*label_NW.isCompound: "true"
*label_NW.compoundIcon: "label.xpm"
*label_NW.compoundName: "label_"
*label_NW.x: 10
*label_NW.y: 10
*label_NW.width: 36
*label_NW.height: 30
*label_NW.labelString: "NW:"
*label_NW.alignment: "alignment_end"

*TF_NE_LAT.class: textField
*TF_NE_LAT.static: true
*TF_NE_LAT.name: TF_NE_LAT
*TF_NE_LAT.parent: form_quad
*TF_NE_LAT.isCompound: "true"
*TF_NE_LAT.compoundIcon: "textfield.xpm"
*TF_NE_LAT.compoundName: "text_Field"
*TF_NE_LAT.x: 255
*TF_NE_LAT.y: 10
*TF_NE_LAT.height: 30
*TF_NE_LAT.cursorPositionVisible: "false"
*TF_NE_LAT.editable: "false"
*TF_NE_LAT.sensitive: "false"
*TF_NE_LAT.text: ""
*TF_NE_LAT.columns: 7
*TF_NE_LAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NE_LAT.resizeWidth: "false"

*TF_NW_LAT.class: textField
*TF_NW_LAT.static: true
*TF_NW_LAT.name: TF_NW_LAT
*TF_NW_LAT.parent: form_quad
*TF_NW_LAT.isCompound: "true"
*TF_NW_LAT.compoundIcon: "textfield.xpm"
*TF_NW_LAT.compoundName: "text_Field"
*TF_NW_LAT.x: 50
*TF_NW_LAT.y: 10
*TF_NW_LAT.height: 30
*TF_NW_LAT.cursorPositionVisible: "false"
*TF_NW_LAT.editable: "false"
*TF_NW_LAT.sensitive: "false"
*TF_NW_LAT.text: ""
*TF_NW_LAT.columns: 7
*TF_NW_LAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_NW_LAT.resizeWidth: "false"

*TF_SW_LAT.class: textField
*TF_SW_LAT.static: true
*TF_SW_LAT.name: TF_SW_LAT
*TF_SW_LAT.parent: form_quad
*TF_SW_LAT.isCompound: "true"
*TF_SW_LAT.compoundIcon: "textfield.xpm"
*TF_SW_LAT.compoundName: "text_Field"
*TF_SW_LAT.x: 50
*TF_SW_LAT.y: 45
*TF_SW_LAT.height: 30
*TF_SW_LAT.cursorPositionVisible: "false"
*TF_SW_LAT.editable: "false"
*TF_SW_LAT.sensitive: "false"
*TF_SW_LAT.text: ""
*TF_SW_LAT.columns: 7
*TF_SW_LAT.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_SW_LAT.resizeWidth: "false"

*form_circle.class: form
*form_circle.static: true
*form_circle.name: form_circle
*form_circle.parent: CreateDatatakeOpps
*form_circle.resizePolicy: "resize_none"
*form_circle.x: 371
*form_circle.y: 238
*form_circle.width: 423
*form_circle.height: 80
*form_circle.createManaged: "false"

*TF_center_lon.class: textField
*TF_center_lon.static: true
*TF_center_lon.name: TF_center_lon
*TF_center_lon.parent: form_circle
*TF_center_lon.isCompound: "true"
*TF_center_lon.compoundIcon: "textfield.xpm"
*TF_center_lon.compoundName: "text_Field"
*TF_center_lon.x: 143
*TF_center_lon.y: 25
*TF_center_lon.height: 30
*TF_center_lon.cursorPositionVisible: "false"
*TF_center_lon.editable: "false"
*TF_center_lon.sensitive: "false"
*TF_center_lon.text: ""
*TF_center_lon.columns: 7
*TF_center_lon.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_center_lon.resizeWidth: "false"
*TF_center_lon.createManaged: "true"
*TF_center_lon.width: 71

*TF_center_lat.class: textField
*TF_center_lat.static: true
*TF_center_lat.name: TF_center_lat
*TF_center_lat.parent: form_circle
*TF_center_lat.isCompound: "true"
*TF_center_lat.compoundIcon: "textfield.xpm"
*TF_center_lat.compoundName: "text_Field"
*TF_center_lat.x: 50
*TF_center_lat.y: 25
*TF_center_lat.height: 30
*TF_center_lat.cursorPositionVisible: "false"
*TF_center_lat.editable: "false"
*TF_center_lat.sensitive: "false"
*TF_center_lat.text: ""
*TF_center_lat.columns: 7
*TF_center_lat.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_center_lat.resizeWidth: "false"
*TF_center_lat.createManaged: "true"

*label1.class: label
*label1.static: true
*label1.name: label1
*label1.parent: form_circle
*label1.isCompound: "true"
*label1.compoundIcon: "label.xpm"
*label1.compoundName: "label_"
*label1.x: 125
*label1.y: 30
*label1.width: 15
*label1.height: 20
*label1.labelString: "/"
*label1.createManaged: "true"

*label2.class: label
*label2.static: true
*label2.name: label2
*label2.parent: form_circle
*label2.isCompound: "true"
*label2.compoundIcon: "label.xpm"
*label2.compoundName: "label_"
*label2.x: 5
*label2.y: 25
*label2.width: 46
*label2.height: 30
*label2.labelString: "CENTER:"
*label2.alignment: "alignment_end"
*label2.createManaged: "true"

*label3.class: label
*label3.static: true
*label3.name: label3
*label3.parent: form_circle
*label3.isCompound: "true"
*label3.compoundIcon: "label.xpm"
*label3.compoundName: "label_"
*label3.x: 222
*label3.y: 28
*label3.width: 49
*label3.height: 25
*label3.labelString: "RADIUS:"
*label3.alignment: "alignment_end"
*label3.createManaged: "true"

*textField_radius.class: textField
*textField_radius.static: true
*textField_radius.name: textField_radius
*textField_radius.parent: form_circle
*textField_radius.isCompound: "true"
*textField_radius.compoundIcon: "textfield.xpm"
*textField_radius.compoundName: "text_Field"
*textField_radius.x: 268
*textField_radius.y: 25
*textField_radius.height: 30
*textField_radius.cursorPositionVisible: "false"
*textField_radius.editable: "false"
*textField_radius.sensitive: "false"
*textField_radius.text: ""
*textField_radius.columns: 7
*textField_radius.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_radius.resizeWidth: "false"
*textField_radius.createManaged: "true"

*label4.class: label
*label4.static: true
*label4.name: label4
*label4.parent: form_circle
*label4.isCompound: "true"
*label4.compoundIcon: "label.xpm"
*label4.compoundName: "label_"
*label4.x: 345
*label4.y: 25
*label4.width: 21
*label4.height: 30
*label4.labelString: "km"
*label4.alignment: "alignment_end"
*label4.createManaged: "true"

*menuCreateHypoSite.class: rowColumn
*menuCreateHypoSite.static: true
*menuCreateHypoSite.name: menuCreateHypoSite
*menuCreateHypoSite.parent: CreateDatatakeOpps
*menuCreateHypoSite.rowColumnType: "menu_bar"
*menuCreateHypoSite.x: 420
*menuCreateHypoSite.y: 324
*menuCreateHypoSite.height: 28
*menuCreateHypoSite.packing: "pack_tight"
*menuCreateHypoSite.resizeHeight: "false"

*menu1_p1.class: rowColumn
*menu1_p1.static: true
*menu1_p1.name: menu1_p1
*menu1_p1.parent: menuCreateHypoSite
*menu1_p1.rowColumnType: "menu_pulldown"
*menu1_p1.x: 0
*menu1_p1.y: 260

*menuCreateHypoSite_Circle.class: pushButton
*menuCreateHypoSite_Circle.static: true
*menuCreateHypoSite_Circle.name: menuCreateHypoSite_Circle
*menuCreateHypoSite_Circle.parent: menu1_p1
*menuCreateHypoSite_Circle.labelString: "Circular"
*menuCreateHypoSite_Circle.x: 2
*menuCreateHypoSite_Circle.y: 286
*menuCreateHypoSite_Circle.fontList: "rockwell-bold"
*menuCreateHypoSite_Circle.activateCallback.source: public
*menuCreateHypoSite_Circle.activateCallback: cb_edit_new_HypoSite
*menuCreateHypoSite_Circle.activateCallbackClientData: "P"

*menuCreateHypoSite_Quad.class: pushButton
*menuCreateHypoSite_Quad.static: true
*menuCreateHypoSite_Quad.name: menuCreateHypoSite_Quad
*menuCreateHypoSite_Quad.parent: menu1_p1
*menuCreateHypoSite_Quad.labelString: "Quadrilateral"
*menuCreateHypoSite_Quad.x: 2
*menuCreateHypoSite_Quad.y: 286
*menuCreateHypoSite_Quad.fontList: "rockwell-bold"
*menuCreateHypoSite_Quad.activateCallback.source: public
*menuCreateHypoSite_Quad.activateCallback: cb_edit_new_HypoSite
*menuCreateHypoSite_Quad.activateCallbackClientData: "Q"

*menu1_top_b1.class: cascadeButton
*menu1_top_b1.static: true
*menu1_top_b1.name: menu1_top_b1
*menu1_top_b1.parent: menuCreateHypoSite
*menu1_top_b1.labelString: "CREATE SITE"
*menu1_top_b1.subMenuId: "menu1_p1"
*menu1_top_b1.fontList: "rockwell-bold"
*menu1_top_b1.x: 5
*menu1_top_b1.y: 290

*form_dtk_info.class: form
*form_dtk_info.static: true
*form_dtk_info.name: form_dtk_info
*form_dtk_info.parent: CreateDatatakeOpps
*form_dtk_info.resizePolicy: "resize_none"
*form_dtk_info.x: 90
*form_dtk_info.y: 369
*form_dtk_info.width: 680
*form_dtk_info.height: 190

*label57.class: label
*label57.static: true
*label57.name: label57
*label57.parent: form_dtk_info
*label57.isCompound: "true"
*label57.compoundIcon: "label.xpm"
*label57.compoundName: "label_"
*label57.x: 0
*label57.y: 5
*label57.width: 85
*label57.height: 35
*label57.labelString: "SAT/SENSOR\n COVERAGE:"
*label57.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label57.alignment: "alignment_beginning"

*label59.class: label
*label59.static: true
*label59.name: label59
*label59.parent: form_dtk_info
*label59.isCompound: "true"
*label59.compoundIcon: "label.xpm"
*label59.compoundName: "label_"
*label59.x: 420
*label59.y: 10
*label59.height: 30
*label59.labelString: "DIRECTION:"
*label59.alignment: "alignment_end"
*label59.createManaged: "true"

*optionMenu_cdtk_sensor.class: rowColumn
*optionMenu_cdtk_sensor.static: true
*optionMenu_cdtk_sensor.name: optionMenu_cdtk_sensor
*optionMenu_cdtk_sensor.parent: form_dtk_info
*optionMenu_cdtk_sensor.rowColumnType: "menu_option"
*optionMenu_cdtk_sensor.subMenuId: "subMenu_cdtk_sensor"
*optionMenu_cdtk_sensor.isCompound: "true"
*optionMenu_cdtk_sensor.compoundIcon: "optionM.xpm"
*optionMenu_cdtk_sensor.compoundName: "option_Menu"
*optionMenu_cdtk_sensor.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_cdtk_sensor.x: 265
*optionMenu_cdtk_sensor.y: 5
*optionMenu_cdtk_sensor.width: 215
*optionMenu_cdtk_sensor.height: 35
*optionMenu_cdtk_sensor.labelString: "SENSOR:"
*optionMenu_cdtk_sensor.sensitive: "true"

*subMenu_cdtk_sensor.class: rowColumn
*subMenu_cdtk_sensor.static: true
*subMenu_cdtk_sensor.name: subMenu_cdtk_sensor
*subMenu_cdtk_sensor.parent: optionMenu_cdtk_sensor
*subMenu_cdtk_sensor.rowColumnType: "menu_pulldown"
*subMenu_cdtk_sensor.labelString: ""
*subMenu_cdtk_sensor.sensitive: "true"
*subMenu_cdtk_sensor.x: 0
*subMenu_cdtk_sensor.y: 335
*subMenu_cdtk_sensor.mappedWhenManaged: "true"

*subMenu_cdtk_ensor_SAR.class: pushButton
*subMenu_cdtk_ensor_SAR.static: true
*subMenu_cdtk_ensor_SAR.name: subMenu_cdtk_ensor_SAR
*subMenu_cdtk_ensor_SAR.parent: subMenu_cdtk_sensor
*subMenu_cdtk_ensor_SAR.labelString: "SAR"
*subMenu_cdtk_ensor_SAR.fontList: "rockwell-bold"
*subMenu_cdtk_ensor_SAR.x: 2
*subMenu_cdtk_ensor_SAR.y: 335
*subMenu_cdtk_ensor_SAR.createCallback.source: public
*subMenu_cdtk_ensor_SAR.createCallback: cb_build_cvrg_allowed_sensor_option_menu

*rc_CoverageType1.class: rowColumn
*rc_CoverageType1.static: true
*rc_CoverageType1.name: rc_CoverageType1
*rc_CoverageType1.parent: form_dtk_info
*rc_CoverageType1.width: 137
*rc_CoverageType1.height: 20
*rc_CoverageType1.isCompound: "true"
*rc_CoverageType1.compoundIcon: "row.xpm"
*rc_CoverageType1.compoundName: "row_Column"
*rc_CoverageType1.x: 486
*rc_CoverageType1.y: 4
*rc_CoverageType1.orientation: "horizontal"
*rc_CoverageType1.radioBehavior: "false"
*rc_CoverageType1.labelString: ""
*rc_CoverageType1.numColumns: 1
*rc_CoverageType1.packing: "pack_tight"
*rc_CoverageType1.whichButton: 1
*rc_CoverageType1.sensitive: "true"
*rc_CoverageType1.radioAlwaysOne: "false"

*toggleButton_Ascending.class: toggleButton
*toggleButton_Ascending.static: true
*toggleButton_Ascending.name: toggleButton_Ascending
*toggleButton_Ascending.parent: rc_CoverageType1
*toggleButton_Ascending.isCompound: "true"
*toggleButton_Ascending.compoundIcon: "toggle.xpm"
*toggleButton_Ascending.compoundName: "toggle_Button"
*toggleButton_Ascending.x: 0
*toggleButton_Ascending.y: 0
*toggleButton_Ascending.width: 67
*toggleButton_Ascending.height: 12
*toggleButton_Ascending.labelString: "ASCENDING"
*toggleButton_Ascending.set: "true"
*toggleButton_Ascending.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_Ascending.indicatorSize: 20

*toggleButton_Descending.class: toggleButton
*toggleButton_Descending.static: true
*toggleButton_Descending.name: toggleButton_Descending
*toggleButton_Descending.parent: rc_CoverageType1
*toggleButton_Descending.isCompound: "true"
*toggleButton_Descending.compoundIcon: "toggle.xpm"
*toggleButton_Descending.compoundName: "toggle_Button"
*toggleButton_Descending.x: 73
*toggleButton_Descending.y: 3
*toggleButton_Descending.width: 61
*toggleButton_Descending.height: 12
*toggleButton_Descending.labelString: "DESCENDING"
*toggleButton_Descending.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_Descending.indicatorSize: 20

*TF_start_rev.class: textField
*TF_start_rev.static: true
*TF_start_rev.name: TF_start_rev
*TF_start_rev.parent: form_dtk_info
*TF_start_rev.isCompound: "true"
*TF_start_rev.compoundIcon: "textfield.xpm"
*TF_start_rev.compoundName: "text_Field"
*TF_start_rev.x: 490
*TF_start_rev.y: 55
*TF_start_rev.height: 30
*TF_start_rev.columns: 6
*TF_start_rev.cursorPositionVisible: "false"
*TF_start_rev.editable: "true"
*TF_start_rev.resizeWidth: "false"
*TF_start_rev.text: "0"
*TF_start_rev.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_start_rev.focusCallback.source: public
*TF_start_rev.focusCallback: cb_toggle_cursor
*TF_start_rev.focusCallbackClientData: (XtPointer) True
*TF_start_rev.losingFocusCallback.source: public
*TF_start_rev.losingFocusCallback: cb_toggle_cursor
*TF_start_rev.losingFocusCallbackClientData: (XtPointer) False
*TF_start_rev.modifyVerifyCallback.source: public
*TF_start_rev.modifyVerifyCallback: cb_filter_text
*TF_start_rev.modifyVerifyCallbackClientData: (XtPointer) valid_numeric_chars

*TF_total_revs.class: textField
*TF_total_revs.static: true
*TF_total_revs.name: TF_total_revs
*TF_total_revs.parent: form_dtk_info
*TF_total_revs.x: 490
*TF_total_revs.y: 145
*TF_total_revs.height: 31
*TF_total_revs.columns: 6
*TF_total_revs.resizeWidth: "false"
*TF_total_revs.text: "0"
*TF_total_revs.cursorPositionVisible: "false"
*TF_total_revs.editable: "true"
*TF_total_revs.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_total_revs.maxLength: 6
*TF_total_revs.focusCallback.source: public
*TF_total_revs.focusCallback: cb_toggle_cursor
*TF_total_revs.focusCallbackClientData: (XtPointer) True
*TF_total_revs.losingFocusCallback.source: public
*TF_total_revs.losingFocusCallback: cb_toggle_cursor
*TF_total_revs.losingFocusCallbackClientData: (XtPointer) False
*TF_total_revs.modifyVerifyCallback.source: public
*TF_total_revs.modifyVerifyCallback: cb_filter_text
*TF_total_revs.modifyVerifyCallbackClientData: (XtPointer) valid_numeric_chars

*label65.class: label
*label65.static: true
*label65.name: label65
*label65.parent: form_dtk_info
*label65.isCompound: "true"
*label65.compoundIcon: "label.xpm"
*label65.compoundName: "label_"
*label65.x: 415
*label65.y: 145
*label65.width: 70
*label65.height: 30
*label65.labelString: "TOTAL REVS:"
*label65.alignment: "alignment_end"

*label68.class: label
*label68.static: true
*label68.name: label68
*label68.parent: form_dtk_info
*label68.isCompound: "true"
*label68.compoundIcon: "label.xpm"
*label68.compoundName: "label_"
*label68.x: 10
*label68.y: 60
*label68.width: 65
*label68.height: 35
*label68.labelString: "ANALYSIS\n PERIOD:"
*label68.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label68.alignment: "alignment_beginning"

*optionMenu_cdtk_sat.class: rowColumn
*optionMenu_cdtk_sat.static: true
*optionMenu_cdtk_sat.name: optionMenu_cdtk_sat
*optionMenu_cdtk_sat.parent: form_dtk_info
*optionMenu_cdtk_sat.rowColumnType: "menu_option"
*optionMenu_cdtk_sat.subMenuId: "subMenu_cdtk_sat"
*optionMenu_cdtk_sat.isCompound: "true"
*optionMenu_cdtk_sat.compoundIcon: "optionM.xpm"
*optionMenu_cdtk_sat.compoundName: "option_Menu"
*optionMenu_cdtk_sat.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_cdtk_sat.x: 90
*optionMenu_cdtk_sat.y: 5
*optionMenu_cdtk_sat.width: 215
*optionMenu_cdtk_sat.height: 35
*optionMenu_cdtk_sat.labelString: "SATELLITE:"
*optionMenu_cdtk_sat.sensitive: "true"

*subMenu_cdtk_sat.class: rowColumn
*subMenu_cdtk_sat.static: true
*subMenu_cdtk_sat.name: subMenu_cdtk_sat
*subMenu_cdtk_sat.parent: optionMenu_cdtk_sat
*subMenu_cdtk_sat.rowColumnType: "menu_pulldown"
*subMenu_cdtk_sat.labelString: ""
*subMenu_cdtk_sat.sensitive: "true"
*subMenu_cdtk_sat.x: 0
*subMenu_cdtk_sat.y: 335
*subMenu_cdtk_sat.mappedWhenManaged: "true"

*subMenu_cdtk_sat_ERS.class: pushButton
*subMenu_cdtk_sat_ERS.static: true
*subMenu_cdtk_sat_ERS.name: subMenu_cdtk_sat_ERS
*subMenu_cdtk_sat_ERS.parent: subMenu_cdtk_sat
*subMenu_cdtk_sat_ERS.labelString: "RADARSAT"
*subMenu_cdtk_sat_ERS.fontList: "rockwell-bold"
*subMenu_cdtk_sat_ERS.x: 2
*subMenu_cdtk_sat_ERS.y: 335
*subMenu_cdtk_sat_ERS.activateCallback: {\
(void) printf("ok\n") ;\
}
*subMenu_cdtk_sat_ERS.createCallback.source: public
*subMenu_cdtk_sat_ERS.createCallback: cb_build_cvrg_allowed_satellite_option_menu

*label117.class: label
*label117.static: true
*label117.name: label117
*label117.parent: form_dtk_info
*label117.isCompound: "true"
*label117.compoundIcon: "label.xpm"
*label117.compoundName: "label_"
*label117.x: 83
*label117.y: 146
*label117.width: 70
*label117.height: 30
*label117.labelString: "TOTAL DAYS:"
*label117.alignment: "alignment_end"

*TF_dtkopps_total_days.class: textField
*TF_dtkopps_total_days.static: true
*TF_dtkopps_total_days.name: TF_dtkopps_total_days
*TF_dtkopps_total_days.parent: form_dtk_info
*TF_dtkopps_total_days.x: 160
*TF_dtkopps_total_days.y: 145
*TF_dtkopps_total_days.height: 31
*TF_dtkopps_total_days.columns: 8
*TF_dtkopps_total_days.resizeWidth: "false"
*TF_dtkopps_total_days.text: "00000.00"
*TF_dtkopps_total_days.cursorPositionVisible: "false"
*TF_dtkopps_total_days.editable: "true"
*TF_dtkopps_total_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_dtkopps_total_days.maxLength: 8
*TF_dtkopps_total_days.width: 83
*TF_dtkopps_total_days.modifyVerifyCallback.source: public
*TF_dtkopps_total_days.modifyVerifyCallback: cb_filter_text
*TF_dtkopps_total_days.modifyVerifyCallbackClientData: (XtPointer) valid_float_chars
*TF_dtkopps_total_days.focusCallback.source: public
*TF_dtkopps_total_days.focusCallback: cb_toggle_cursor
*TF_dtkopps_total_days.focusCallbackClientData: (XtPointer) TRUE
*TF_dtkopps_total_days.losingFocusCallback.source: public
*TF_dtkopps_total_days.losingFocusCallback: cb_toggle_cursor
*TF_dtkopps_total_days.losingFocusCallbackClientData: (XtPointer) FALSE

*label119.class: label
*label119.static: true
*label119.name: label119
*label119.parent: form_dtk_info
*label119.x: 158
*label119.y: 114
*label119.width: 185
*label119.height: 15
*label119.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label119.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*TF_dtkopps_end.class: textField
*TF_dtkopps_end.static: true
*TF_dtkopps_end.name: TF_dtkopps_end
*TF_dtkopps_end.parent: form_dtk_info
*TF_dtkopps_end.isCompound: "true"
*TF_dtkopps_end.compoundIcon: "textfield.xpm"
*TF_dtkopps_end.compoundName: "text_Field"
*TF_dtkopps_end.x: 160
*TF_dtkopps_end.y: 85
*TF_dtkopps_end.height: 30
*TF_dtkopps_end.columns: 21
*TF_dtkopps_end.cursorPositionVisible: "false"
*TF_dtkopps_end.editable: "true"
*TF_dtkopps_end.resizeWidth: "false"
*TF_dtkopps_end.text: ""
*TF_dtkopps_end.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_dtkopps_end.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*TF_dtkopps_end.modifyVerifyCallback.source: public
*TF_dtkopps_end.modifyVerifyCallback: cb_filter_text
*TF_dtkopps_end.maxLength: 21
*TF_dtkopps_end.activateCallback.source: public
*TF_dtkopps_end.activateCallback: cb_validate_ASF_datetime
*TF_dtkopps_end.activateCallbackClientData: (XtPointer) "Site Coverage Stop Time"
*TF_dtkopps_end.focusCallback.source: public
*TF_dtkopps_end.focusCallback: cb_toggle_cursor
*TF_dtkopps_end.focusCallbackClientData: (XtPointer) TRUE
*TF_dtkopps_end.losingFocusCallback.source: public
*TF_dtkopps_end.losingFocusCallback: cb_toggle_cursor
*TF_dtkopps_end.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_dtkopps_end.sensitive: "true"

*label121.class: label
*label121.static: true
*label121.name: label121
*label121.parent: form_dtk_info
*label121.isCompound: "true"
*label121.compoundIcon: "label.xpm"
*label121.compoundName: "label_"
*label121.x: 88
*label121.y: 84
*label121.width: 70
*label121.height: 30
*label121.labelString: " STOP TIME:"
*label121.alignment: "alignment_end"

*label120.class: label
*label120.static: true
*label120.name: label120
*label120.parent: form_dtk_info
*label120.isCompound: "true"
*label120.compoundIcon: "label.xpm"
*label120.compoundName: "label_"
*label120.x: 88
*label120.y: 54
*label120.width: 70
*label120.height: 30
*label120.labelString: "START TIME:"
*label120.alignment: "alignment_end"

*TF_dtkopps_start.class: textField
*TF_dtkopps_start.static: true
*TF_dtkopps_start.name: TF_dtkopps_start
*TF_dtkopps_start.parent: form_dtk_info
*TF_dtkopps_start.isCompound: "true"
*TF_dtkopps_start.compoundIcon: "textfield.xpm"
*TF_dtkopps_start.compoundName: "text_Field"
*TF_dtkopps_start.x: 160
*TF_dtkopps_start.y: 55
*TF_dtkopps_start.height: 30
*TF_dtkopps_start.columns: 21
*TF_dtkopps_start.cursorPositionVisible: "false"
*TF_dtkopps_start.editable: "true"
*TF_dtkopps_start.resizeWidth: "false"
*TF_dtkopps_start.text: ""
*TF_dtkopps_start.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_dtkopps_start.modifyVerifyCallbackClientData: (XtPointer) valid_ASF_datetime_chars
*TF_dtkopps_start.modifyVerifyCallback.source: public
*TF_dtkopps_start.modifyVerifyCallback: cb_filter_text
*TF_dtkopps_start.maxLength: 21
*TF_dtkopps_start.activateCallback.source: public
*TF_dtkopps_start.activateCallback: cb_validate_ASF_datetime
*TF_dtkopps_start.activateCallbackClientData: (XtPointer) "Site Coverage Start Time"
*TF_dtkopps_start.focusCallback.source: public
*TF_dtkopps_start.focusCallback: cb_toggle_cursor
*TF_dtkopps_start.focusCallbackClientData: (XtPointer) TRUE
*TF_dtkopps_start.losingFocusCallback.source: public
*TF_dtkopps_start.losingFocusCallback: cb_toggle_cursor
*TF_dtkopps_start.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_dtkopps_start.sensitive: "true"

*label66.class: label
*label66.static: true
*label66.name: label66
*label66.parent: form_dtk_info
*label66.isCompound: "true"
*label66.compoundIcon: "label.xpm"
*label66.compoundName: "label_"
*label66.x: 420
*label66.y: 90
*label66.height: 30
*label66.labelString: " STOP REV:"
*label66.alignment: "alignment_end"

*label118.class: label
*label118.static: true
*label118.name: label118
*label118.parent: form_dtk_info
*label118.isCompound: "true"
*label118.compoundIcon: "label.xpm"
*label118.compoundName: "label_"
*label118.x: 420
*label118.y: 55
*label118.height: 30
*label118.labelString: "START REV:"
*label118.alignment: "alignment_end"

*TF_stop_rev.class: textField
*TF_stop_rev.static: true
*TF_stop_rev.name: TF_stop_rev
*TF_stop_rev.parent: form_dtk_info
*TF_stop_rev.isCompound: "true"
*TF_stop_rev.compoundIcon: "textfield.xpm"
*TF_stop_rev.compoundName: "text_Field"
*TF_stop_rev.x: 490
*TF_stop_rev.y: 85
*TF_stop_rev.height: 30
*TF_stop_rev.columns: 6
*TF_stop_rev.cursorPositionVisible: "false"
*TF_stop_rev.editable: "true"
*TF_stop_rev.resizeWidth: "false"
*TF_stop_rev.text: "0"
*TF_stop_rev.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_stop_rev.focusCallback.source: public
*TF_stop_rev.focusCallback: cb_toggle_cursor
*TF_stop_rev.focusCallbackClientData: (XtPointer) True
*TF_stop_rev.losingFocusCallback.source: public
*TF_stop_rev.losingFocusCallback: cb_toggle_cursor
*TF_stop_rev.losingFocusCallbackClientData: (XtPointer) False
*TF_stop_rev.modifyVerifyCallback.source: public
*TF_stop_rev.modifyVerifyCallback: cb_filter_text
*TF_stop_rev.modifyVerifyCallbackClientData: (XtPointer) valid_numeric_chars

*pushButton_cancel_create_site.class: pushButton
*pushButton_cancel_create_site.static: true
*pushButton_cancel_create_site.name: pushButton_cancel_create_site
*pushButton_cancel_create_site.parent: CreateDatatakeOpps
*pushButton_cancel_create_site.x: 680
*pushButton_cancel_create_site.y: 322
*pushButton_cancel_create_site.width: 106
*pushButton_cancel_create_site.height: 30
*pushButton_cancel_create_site.labelString: "CANCEL"
*pushButton_cancel_create_site.fontList: "rockwell-bold"
*pushButton_cancel_create_site.createManaged: "false"
*pushButton_cancel_create_site.activateCallback.source: public
*pushButton_cancel_create_site.activateCallback: cb_cancel_edit_new_HypoSite

*pushButton_delete_site.class: pushButton
*pushButton_delete_site.static: true
*pushButton_delete_site.name: pushButton_delete_site
*pushButton_delete_site.parent: CreateDatatakeOpps
*pushButton_delete_site.x: 550
*pushButton_delete_site.y: 324
*pushButton_delete_site.width: 106
*pushButton_delete_site.height: 30
*pushButton_delete_site.labelString: "DELETE SITE"
*pushButton_delete_site.fontList: "rockwell-bold"
*pushButton_delete_site.createManaged: "true"
*pushButton_delete_site.activateCallback.source: public
*pushButton_delete_site.activateCallback: cb_delete_HypoSite
*pushButton_delete_site.sensitive: "true"

*pushButton_done_create_site.class: pushButton
*pushButton_done_create_site.static: true
*pushButton_done_create_site.name: pushButton_done_create_site
*pushButton_done_create_site.parent: CreateDatatakeOpps
*pushButton_done_create_site.x: 550
*pushButton_done_create_site.y: 324
*pushButton_done_create_site.width: 106
*pushButton_done_create_site.height: 30
*pushButton_done_create_site.labelString: "DONE"
*pushButton_done_create_site.fontList: "rockwell-bold"
*pushButton_done_create_site.createManaged: "false"
*pushButton_done_create_site.activateCallback.source: public
*pushButton_done_create_site.activateCallback: cb_add_HypoSite

*pushButton_Refresh.class: pushButton
*pushButton_Refresh.static: true
*pushButton_Refresh.name: pushButton_Refresh
*pushButton_Refresh.parent: CreateDatatakeOpps
*pushButton_Refresh.x: 33
*pushButton_Refresh.y: 133
*pushButton_Refresh.width: 30
*pushButton_Refresh.height: 132
*pushButton_Refresh.fontList: "rockwell-bold"
*pushButton_Refresh.labelString: "R\nE\nF\nR\nE\nS\nH"
*pushButton_Refresh.activateCallback.source: public
*pushButton_Refresh.activateCallback: cb_show_dar_relations
*pushButton_Refresh.activateCallbackClientData: (XtPointer) scrolledList_sites

*separator10.class: separator
*separator10.static: true
*separator10.name: separator10
*separator10.parent: CreateDatatakeOpps
*separator10.width: 855
*separator10.height: 15
*separator10.isCompound: "true"
*separator10.compoundIcon: "sep.xpm"
*separator10.compoundName: "separator_"
*separator10.x: 0
*separator10.y: 359

