! UIMX ascii 2.9 key: 8028                                                      

*APSFileGeneration.class: form
*APSFileGeneration.classinc:
*APSFileGeneration.classspec:
*APSFileGeneration.classmembers:
*APSFileGeneration.classconstructor:
*APSFileGeneration.classdestructor:
*APSFileGeneration.gbldecl: #include <stdio.h>\
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
#pragma ident   "%W% %E% APS/ASF"\
#pragma ident   "%Z% %P%"\
#include <stdlib.h>\
\
#include "cb_datetime.h"\
#include "cb_crtapsfile.h"\
#include "cb_fileviewer.h"\
\
extern Widget	apsfilegen_form ;\

*APSFileGeneration.ispecdecl:
*APSFileGeneration.funcdecl: swidget create_APSFileGeneration(swidget UxParent)
*APSFileGeneration.funcname: create_APSFileGeneration
*APSFileGeneration.funcdef: "swidget", "<create_APSFileGeneration>(%)"
*APSFileGeneration.argdecl: swidget UxParent;
*APSFileGeneration.arglist: UxParent
*APSFileGeneration.arglist.UxParent: "swidget", "%UxParent%"
*APSFileGeneration.icode: PERIOD_WIDGETS *rptgen_times ;
*APSFileGeneration.fcode: rptgen_times = (PERIOD_WIDGETS *) malloc(sizeof(PERIOD_WIDGETS)) ; ;\
rptgen_times->start = (Widget) TF_report_start ;\
rptgen_times->stop = (Widget) TF_report_stop ;\
\
XtAddCallback(TF_report_total_days, XmNactivateCallback,\
    (XtCallbackProc) cb_adjust_ASF_datetimes,\
    (XtPointer) rptgen_times) ;\
\
XtAddCallback(XtParent(rtrn), XtNpopupCallback,\
	cb_show_reports, (XtPointer *) scrolledList_reports) ;\
 \
cb_init_extended_times( ExtendTimes_tb, NULL, NULL ) ;\
\
return(rtrn);\

*APSFileGeneration.auxdecl:
*APSFileGeneration.static: true
*APSFileGeneration.name: APSFileGeneration
*APSFileGeneration.parent: NO_PARENT
*APSFileGeneration.parentExpression: UxParent
*APSFileGeneration.defaultShell: topLevelShell
*APSFileGeneration.width: 623
*APSFileGeneration.height: 720
*APSFileGeneration.resizePolicy: "resize_none"
*APSFileGeneration.isCompound: "true"
*APSFileGeneration.compoundIcon: "form.xpm"
*APSFileGeneration.compoundName: "form_"
*APSFileGeneration.x: 464
*APSFileGeneration.y: 20
*APSFileGeneration.unitType: "pixels"
*APSFileGeneration.dialogTitle: "APS:File Generation"

*label46.class: label
*label46.static: true
*label46.name: label46
*label46.parent: APSFileGeneration
*label46.isCompound: "true"
*label46.compoundIcon: "label.xpm"
*label46.compoundName: "label_"
*label46.x: 10
*label46.y: 5
*label46.width: 590
*label46.height: 35
*label46.labelString: "APS  FILE  GENERATION"
*label46.fontList: "-adobe-new century schoolbook-bold-r-normal--20-140-100-100-p-113-iso8859-1"

*scrolledWindowList4.class: scrolledWindow
*scrolledWindowList4.static: true
*scrolledWindowList4.name: scrolledWindowList4
*scrolledWindowList4.parent: APSFileGeneration
*scrolledWindowList4.scrollingPolicy: "application_defined"
*scrolledWindowList4.visualPolicy: "variable"
*scrolledWindowList4.scrollBarDisplayPolicy: "static"
*scrolledWindowList4.shadowThickness: 0
*scrolledWindowList4.isCompound: "true"
*scrolledWindowList4.compoundIcon: "scrllist.xpm"
*scrolledWindowList4.compoundName: "scrolled_List"
*scrolledWindowList4.x: 61
*scrolledWindowList4.y: 64
*scrolledWindowList4.width: 500
*scrolledWindowList4.height: 190

*scrolledList_reports.class: scrolledList
*scrolledList_reports.static: true
*scrolledList_reports.name: scrolledList_reports
*scrolledList_reports.parent: scrolledWindowList4
*scrolledList_reports.width: 484
*scrolledList_reports.height: 190
*scrolledList_reports.scrollBarDisplayPolicy: "as_needed"
*scrolledList_reports.listSizePolicy: "constant"
*scrolledList_reports.automaticSelection: "true"
*scrolledList_reports.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"
*scrolledList_reports.selectionPolicy: "browse_select"
*scrolledList_reports.itemCount: 16
*scrolledList_reports.items: "AWOS           ASF Weekly Operation Schedule,MWOS           McMurdo Weekly Operation Schedule,MWOS           McMurdo Weekly Operation Schedule,MWO3           McMurdo Weekly Operation Schedule,MWO4           McMurdo Weekly Operation Schedule,MWO5           McMurdo Weekly Operation Schedule,MWO6           McMurdo Weekly Operation Schedule,MWO7           McMurdo Weekly Operation Schedule,MWO8           McMurdo Weekly Operation Schedule,MWO9           McMurdo Weekly Operation Schedule,MW10           McMurdo Weekly Operation Schedule,MW11           McMurdo Weekly Operation Schedule,MW12           McMurdo Weekly Operation Schedule,MW13           McMurdo Weekly Operation Schedule,MW14           McMurdo Weekly Operation Schedule,MW15           McMurdo Weekly Operation Schedule"
*scrolledList_reports.listMarginHeight: 0
*scrolledList_reports.browseSelectionCallback.source: public
*scrolledList_reports.browseSelectionCallback: cb_update_capsfile_form

*separator6.class: separator
*separator6.static: true
*separator6.name: separator6
*separator6.parent: APSFileGeneration
*separator6.width: 619
*separator6.height: 10
*separator6.isCompound: "true"
*separator6.compoundIcon: "sep.xpm"
*separator6.compoundName: "separator_"
*separator6.x: 2
*separator6.y: 265

*label64.class: label
*label64.static: true
*label64.name: label64
*label64.parent: APSFileGeneration
*label64.isCompound: "true"
*label64.compoundIcon: "label.xpm"
*label64.compoundName: "label_"
*label64.x: 75
*label64.y: 323
*label64.height: 25
*label64.labelString: "START TIME:"

*label69.class: label
*label69.static: true
*label69.name: label69
*label69.parent: APSFileGeneration
*label69.isCompound: "true"
*label69.compoundIcon: "label.xpm"
*label69.compoundName: "label_"
*label69.x: 75
*label69.y: 373
*label69.height: 25
*label69.labelString: " STOP TIME:"

*pushButton_CreateReportFile.class: pushButton
*pushButton_CreateReportFile.static: true
*pushButton_CreateReportFile.name: pushButton_CreateReportFile
*pushButton_CreateReportFile.parent: APSFileGeneration
*pushButton_CreateReportFile.isCompound: "true"
*pushButton_CreateReportFile.compoundIcon: "push.xpm"
*pushButton_CreateReportFile.compoundName: "push_Button"
*pushButton_CreateReportFile.x: 44
*pushButton_CreateReportFile.y: 471
*pushButton_CreateReportFile.width: 120
*pushButton_CreateReportFile.height: 40
*pushButton_CreateReportFile.labelString: "CREATE"
*pushButton_CreateReportFile.sensitive: "false"
*pushButton_CreateReportFile.fontList: "rockwell-bold"
*pushButton_CreateReportFile.activateCallback.source: public
*pushButton_CreateReportFile.activateCallback: cb_create_report

*pushButton_APSFileGenQuit.class: pushButton
*pushButton_APSFileGenQuit.static: true
*pushButton_APSFileGenQuit.name: pushButton_APSFileGenQuit
*pushButton_APSFileGenQuit.parent: APSFileGeneration
*pushButton_APSFileGenQuit.isCompound: "true"
*pushButton_APSFileGenQuit.compoundIcon: "push.xpm"
*pushButton_APSFileGenQuit.compoundName: "push_Button"
*pushButton_APSFileGenQuit.x: 455
*pushButton_APSFileGenQuit.y: 471
*pushButton_APSFileGenQuit.width: 120
*pushButton_APSFileGenQuit.height: 40
*pushButton_APSFileGenQuit.labelString: "QUIT"
*pushButton_APSFileGenQuit.sensitive: "true"
*pushButton_APSFileGenQuit.fontList: "rockwell-bold"
*pushButton_APSFileGenQuit.activateCallback.source: public
*pushButton_APSFileGenQuit.activateCallback: cb_quit_filegen

*scrolledWindowText4.class: scrolledWindow
*scrolledWindowText4.static: true
*scrolledWindowText4.name: scrolledWindowText4
*scrolledWindowText4.parent: APSFileGeneration
*scrolledWindowText4.scrollingPolicy: "application_defined"
*scrolledWindowText4.visualPolicy: "variable"
*scrolledWindowText4.scrollBarDisplayPolicy: "static"
*scrolledWindowText4.isCompound: "true"
*scrolledWindowText4.compoundIcon: "scrltext.xpm"
*scrolledWindowText4.compoundName: "scrolled_Text"
*scrolledWindowText4.x: 11
*scrolledWindowText4.y: 551
*scrolledWindowText4.width: 590
*scrolledWindowText4.height: 160
*scrolledWindowText4.mappedWhenManaged: "true"

*scrolledText_create_report.class: scrolledText
*scrolledText_create_report.name.source: public
*scrolledText_create_report.static: false
*scrolledText_create_report.name: scrolledText_create_report
*scrolledText_create_report.parent: scrolledWindowText4
*scrolledText_create_report.width: 485
*scrolledText_create_report.height: 141
*scrolledText_create_report.editMode: "multi_line_edit"
*scrolledText_create_report.editable: "false"
*scrolledText_create_report.mappedWhenManaged: "true"

*label75.class: label
*label75.static: true
*label75.name: label75
*label75.parent: APSFileGeneration
*label75.isCompound: "true"
*label75.compoundIcon: "label.xpm"
*label75.compoundName: "label_"
*label75.x: 75
*label75.y: 423
*label75.height: 25
*label75.labelString: "TOTAL DAYS:"

*textField_reportname.class: textField
*textField_reportname.static: true
*textField_reportname.name: textField_reportname
*textField_reportname.parent: APSFileGeneration
*textField_reportname.isCompound: "true"
*textField_reportname.compoundIcon: "textfield.xpm"
*textField_reportname.compoundName: "text_Field"
*textField_reportname.x: 150
*textField_reportname.y: 281
*textField_reportname.height: 32
*textField_reportname.cursorPositionVisible: "true"
*textField_reportname.editable: "true"
*textField_reportname.text: ""
*textField_reportname.columns: 30
*textField_reportname.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_reportname.resizeWidth: "false"
*textField_reportname.maxLength: 1000
*textField_reportname.width: 410

*label77.class: label
*label77.static: true
*label77.name: label77
*label77.parent: APSFileGeneration
*label77.isCompound: "true"
*label77.compoundIcon: "label.xpm"
*label77.compoundName: "label_"
*label77.x: 64
*label77.y: 46
*label77.width: 505
*label77.height: 16
*label77.labelString: "REPORT ID      REPORT NAME"
*label77.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*label77.alignment: "alignment_beginning"

*TF_report_total_days.class: textField
*TF_report_total_days.static: true
*TF_report_total_days.name: TF_report_total_days
*TF_report_total_days.parent: APSFileGeneration
*TF_report_total_days.x: 150
*TF_report_total_days.y: 421
*TF_report_total_days.height: 31
*TF_report_total_days.columns: 8
*TF_report_total_days.resizeWidth: "false"
*TF_report_total_days.text: "00000.00"
*TF_report_total_days.cursorPositionVisible: "true"
*TF_report_total_days.editable: "true"
*TF_report_total_days.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_report_total_days.maxLength: 8
*TF_report_total_days.width: 83
*TF_report_total_days.modifyVerifyCallback.source: public
*TF_report_total_days.modifyVerifyCallback: cb_filter_text
*TF_report_total_days.modifyVerifyCallbackClientData: (XtPointer) valid_float_chars
*TF_report_total_days.focusCallback.source: public
*TF_report_total_days.focusCallback: cb_toggle_cursor
*TF_report_total_days.focusCallbackClientData: (XtPointer) TRUE
*TF_report_total_days.losingFocusCallback.source: public
*TF_report_total_days.losingFocusCallback: cb_toggle_cursor
*TF_report_total_days.losingFocusCallbackClientData: (XtPointer) FALSE

*label71.class: label
*label71.static: true
*label71.name: label71
*label71.parent: APSFileGeneration
*label71.x: 150
*label71.y: 401
*label71.width: 185
*label71.height: 15
*label71.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label71.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*TF_report_stop.class: textField
*TF_report_stop.static: true
*TF_report_stop.name: TF_report_stop
*TF_report_stop.parent: APSFileGeneration
*TF_report_stop.isCompound: "true"
*TF_report_stop.compoundIcon: "textfield.xpm"
*TF_report_stop.compoundName: "text_Field"
*TF_report_stop.x: 150
*TF_report_stop.y: 371
*TF_report_stop.height: 30
*TF_report_stop.columns: 21
*TF_report_stop.cursorPositionVisible: "true"
*TF_report_stop.editable: "true"
*TF_report_stop.resizeWidth: "false"
*TF_report_stop.text: "1999:001:00:00:00.000"
*TF_report_stop.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_report_stop.maxLength: 21
*TF_report_stop.activateCallback.source: public
*TF_report_stop.activateCallback: cb_validate_ASF_datetime
*TF_report_stop.activateCallbackClientData: (XtPointer) "Create File Stop Time"
*TF_report_stop.focusCallback.source: public
*TF_report_stop.focusCallback: cb_toggle_cursor
*TF_report_stop.focusCallbackClientData: (XtPointer) TRUE
*TF_report_stop.losingFocusCallback.source: public
*TF_report_stop.losingFocusCallback: cb_toggle_cursor
*TF_report_stop.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_report_stop.sensitive: "true"

*label70.class: label
*label70.static: true
*label70.name: label70
*label70.parent: APSFileGeneration
*label70.x: 150
*label70.y: 351
*label70.width: 185
*label70.height: 15
*label70.labelString: "yyyy:ddd:hh:mm:ss.ccc"
*label70.fontList: "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-8"

*TF_report_start.class: textField
*TF_report_start.static: true
*TF_report_start.name: TF_report_start
*TF_report_start.parent: APSFileGeneration
*TF_report_start.isCompound: "true"
*TF_report_start.compoundIcon: "textfield.xpm"
*TF_report_start.compoundName: "text_Field"
*TF_report_start.x: 150
*TF_report_start.y: 321
*TF_report_start.height: 30
*TF_report_start.columns: 21
*TF_report_start.cursorPositionVisible: "true"
*TF_report_start.editable: "true"
*TF_report_start.resizeWidth: "false"
*TF_report_start.text: "1990:001:00:00:00.000"
*TF_report_start.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_report_start.maxLength: 21
*TF_report_start.activateCallback.source: public
*TF_report_start.activateCallback: cb_validate_ASF_datetime
*TF_report_start.activateCallbackClientData: (XtPointer) "Create File Start Time"
*TF_report_start.focusCallback.source: public
*TF_report_start.focusCallback: cb_toggle_cursor
*TF_report_start.focusCallbackClientData: (XtPointer) TRUE
*TF_report_start.losingFocusCallback.source: public
*TF_report_start.losingFocusCallback: cb_toggle_cursor
*TF_report_start.losingFocusCallbackClientData: (XtPointer) FALSE
*TF_report_start.sensitive: "true"

*pushButton_input_file1.class: pushButton
*pushButton_input_file1.static: true
*pushButton_input_file1.name: pushButton_input_file1
*pushButton_input_file1.parent: APSFileGeneration
*pushButton_input_file1.isCompound: "true"
*pushButton_input_file1.compoundIcon: "push.xpm"
*pushButton_input_file1.compoundName: "push_Button"
*pushButton_input_file1.x: 52
*pushButton_input_file1.y: 282
*pushButton_input_file1.width: 93
*pushButton_input_file1.height: 30
*pushButton_input_file1.labelString: "SELECT FILE"
*pushButton_input_file1.sensitive: "true"
*pushButton_input_file1.fontList: "rockwell-bold"
*pushButton_input_file1.activateCallback.source: public
*pushButton_input_file1.activateCallback: cb_capsfile_select_input
*pushButton_input_file1.activateCallbackClientData: (XtPointer) scrolledList_reports

*pushButton_view.class: pushButton
*pushButton_view.static: true
*pushButton_view.name: pushButton_view
*pushButton_view.parent: APSFileGeneration
*pushButton_view.x: 189
*pushButton_view.y: 471
*pushButton_view.width: 100
*pushButton_view.height: 40
*pushButton_view.activateCallback.source: public
*pushButton_view.activateCallback: cb_FileViewer_popup
*pushButton_view.activateCallbackClientData: (XtPointer) textField_reportname
*pushButton_view.labelString: "VIEW"
*pushButton_view.fontList: "rockwell-bold"

*pushButton_transfer.class: pushButton
*pushButton_transfer.static: true
*pushButton_transfer.name: pushButton_transfer
*pushButton_transfer.parent: APSFileGeneration
*pushButton_transfer.isCompound: "true"
*pushButton_transfer.compoundIcon: "push.xpm"
*pushButton_transfer.compoundName: "push_Button"
*pushButton_transfer.x: 311
*pushButton_transfer.y: 471
*pushButton_transfer.width: 120
*pushButton_transfer.height: 40
*pushButton_transfer.labelString: "TRANSFER"
*pushButton_transfer.sensitive: "false"
*pushButton_transfer.fontList: "rockwell-bold"
*pushButton_transfer.highlightOnEnter: "true"

*separator7.class: separator
*separator7.static: true
*separator7.name: separator7
*separator7.parent: APSFileGeneration
*separator7.width: 622
*separator7.height: 9
*separator7.isCompound: "true"
*separator7.compoundIcon: "sep.xpm"
*separator7.compoundName: "separator_"
*separator7.x: 1
*separator7.y: 516

*label6.class: label
*label6.static: true
*label6.name: label6
*label6.parent: APSFileGeneration
*label6.isCompound: "true"
*label6.compoundIcon: "label.xpm"
*label6.compoundName: "label_"
*label6.x: 11
*label6.y: 526
*label6.width: 590
*label6.height: 20
*label6.labelString: "MESSAGES"
*label6.fontList: "-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso8859-1"

*ExtendTimes_tb.class: toggleButton
*ExtendTimes_tb.name.source: public
*ExtendTimes_tb.static: false
*ExtendTimes_tb.name: ExtendTimes_tb
*ExtendTimes_tb.parent: APSFileGeneration
*ExtendTimes_tb.isCompound: "true"
*ExtendTimes_tb.compoundIcon: "toggle.xpm"
*ExtendTimes_tb.compoundName: "toggle_Button"
*ExtendTimes_tb.width: 180
*ExtendTimes_tb.height: 32
*ExtendTimes_tb.indicatorSize: 20
*ExtendTimes_tb.indicatorType: "n_of_many"
*ExtendTimes_tb.labelString: "Extend Start/Stop Times\n(to day boundaries)"
*ExtendTimes_tb.marginLeft: 2
*ExtendTimes_tb.set: "true"
*ExtendTimes_tb.spacing: 4
*ExtendTimes_tb.createManaged: "false"
*ExtendTimes_tb.alignment: "alignment_beginning"
*ExtendTimes_tb.x: 359
*ExtendTimes_tb.y: 321
*ExtendTimes_tb.indicatorOn: "true"

