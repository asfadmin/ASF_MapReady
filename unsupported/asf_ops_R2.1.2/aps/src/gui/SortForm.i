! UIMX ascii 2.9 key: 7808                                                      

*SortForm.class: form
*SortForm.classinc:
*SortForm.classspec:
*SortForm.classmembers:
*SortForm.classconstructor:
*SortForm.classdestructor:
*SortForm.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)SortForm.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.SortForm.i"\
\
#include "cb_sortform.h"\

*SortForm.ispecdecl:
*SortForm.funcdecl: swidget create_SortForm(swidget UxParent)
*SortForm.funcname: create_SortForm
*SortForm.funcdef: "swidget", "<create_SortForm>(%)"
*SortForm.argdecl: swidget UxParent;
*SortForm.arglist: UxParent
*SortForm.arglist.UxParent: "swidget", "%UxParent%"
*SortForm.icode:
*SortForm.fcode: return(rtrn);\

*SortForm.auxdecl:
*SortForm.static: true
*SortForm.name: SortForm
*SortForm.parent: NO_PARENT
*SortForm.parentExpression: UxParent
*SortForm.defaultShell: topLevelShell
*SortForm.width: 334
*SortForm.height: 670
*SortForm.resizePolicy: "resize_none"
*SortForm.isCompound: "true"
*SortForm.compoundIcon: "form.xpm"
*SortForm.compoundName: "form_"
*SortForm.x: 408
*SortForm.y: 95
*SortForm.unitType: "pixels"
*SortForm.dialogTitle: "Sort Order"

*scrolledWindowList6.class: scrolledWindow
*scrolledWindowList6.static: true
*scrolledWindowList6.name: scrolledWindowList6
*scrolledWindowList6.parent: SortForm
*scrolledWindowList6.scrollingPolicy: "application_defined"
*scrolledWindowList6.visualPolicy: "variable"
*scrolledWindowList6.scrollBarDisplayPolicy: "static"
*scrolledWindowList6.shadowThickness: 0
*scrolledWindowList6.isCompound: "true"
*scrolledWindowList6.compoundIcon: "scrllist.xpm"
*scrolledWindowList6.compoundName: "scrolled_List"
*scrolledWindowList6.x: 35
*scrolledWindowList6.y: 315
*scrolledWindowList6.width: 245
*scrolledWindowList6.height: 255
*scrolledWindowList6.resizable: "false"

*scrolledList_column_names.class: scrolledList
*scrolledList_column_names.static: true
*scrolledList_column_names.name: scrolledList_column_names
*scrolledList_column_names.parent: scrolledWindowList6
*scrolledList_column_names.width: 245
*scrolledList_column_names.visibleItemCount: 10
*scrolledList_column_names.listSizePolicy: "variable"
*scrolledList_column_names.height: 245
*scrolledList_column_names.defaultActionCallback.source: public
*scrolledList_column_names.defaultActionCallback: cb_add_sort_column
*scrolledList_column_names.browseSelectionCallback.source: public
*scrolledList_column_names.browseSelectionCallback: cb_add_sort_column

*scrolledWindowText5.class: scrolledWindow
*scrolledWindowText5.static: true
*scrolledWindowText5.name: scrolledWindowText5
*scrolledWindowText5.parent: SortForm
*scrolledWindowText5.scrollingPolicy: "application_defined"
*scrolledWindowText5.visualPolicy: "variable"
*scrolledWindowText5.scrollBarDisplayPolicy: "static"
*scrolledWindowText5.isCompound: "true"
*scrolledWindowText5.compoundIcon: "scrltext.xpm"
*scrolledWindowText5.compoundName: "scrolled_Text"
*scrolledWindowText5.x: 35
*scrolledWindowText5.y: 85
*scrolledWindowText5.width: 270
*scrolledWindowText5.height: 130

*scrolledText_SortClause.class: scrolledText
*scrolledText_SortClause.static: true
*scrolledText_SortClause.name: scrolledText_SortClause
*scrolledText_SortClause.parent: scrolledWindowText5
*scrolledText_SortClause.width: 265
*scrolledText_SortClause.height: 115
*scrolledText_SortClause.editable: "false"
*scrolledText_SortClause.wordWrap: "true"
*scrolledText_SortClause.editMode: "multi_line_edit"
*scrolledText_SortClause.scrollHorizontal: "false"

*label82.class: label
*label82.static: true
*label82.name: label82
*label82.parent: SortForm
*label82.isCompound: "true"
*label82.compoundIcon: "label.xpm"
*label82.compoundName: "label_"
*label82.x: 10
*label82.y: 25
*label82.width: 60
*label82.height: 20
*label82.labelString: "TABLE:"

*textField_table_name.class: textField
*textField_table_name.static: true
*textField_table_name.name: textField_table_name
*textField_table_name.parent: SortForm
*textField_table_name.isCompound: "true"
*textField_table_name.compoundIcon: "textfield.xpm"
*textField_table_name.compoundName: "text_Field"
*textField_table_name.x: 70
*textField_table_name.y: 20
*textField_table_name.height: 32
*textField_table_name.cursorPositionVisible: "false"
*textField_table_name.editable: "false"
*textField_table_name.text: ""
*textField_table_name.columns: 12
*textField_table_name.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_table_name.resizeWidth: "false"
*textField_table_name.maxLength: 12

*label98.class: label
*label98.static: true
*label98.name: label98
*label98.parent: SortForm
*label98.isCompound: "true"
*label98.compoundIcon: "label.xpm"
*label98.compoundName: "label_"
*label98.x: 20
*label98.y: 290
*label98.height: 20
*label98.labelString: "COLUMN NAMES:"

*pushButton_SortOrder_OK.class: pushButton
*pushButton_SortOrder_OK.static: true
*pushButton_SortOrder_OK.name: pushButton_SortOrder_OK
*pushButton_SortOrder_OK.parent: SortForm
*pushButton_SortOrder_OK.isCompound: "true"
*pushButton_SortOrder_OK.compoundIcon: "push.xpm"
*pushButton_SortOrder_OK.compoundName: "push_Button"
*pushButton_SortOrder_OK.x: 20
*pushButton_SortOrder_OK.y: 600
*pushButton_SortOrder_OK.width: 90
*pushButton_SortOrder_OK.height: 40
*pushButton_SortOrder_OK.labelString: "OK"
*pushButton_SortOrder_OK.fontList: "rockwell-bold"
*pushButton_SortOrder_OK.activateCallback.source: public
*pushButton_SortOrder_OK.activateCallback: cb_update_sort_order_field

*pushButton_SortOrder_Cancel.class: pushButton
*pushButton_SortOrder_Cancel.static: true
*pushButton_SortOrder_Cancel.name: pushButton_SortOrder_Cancel
*pushButton_SortOrder_Cancel.parent: SortForm
*pushButton_SortOrder_Cancel.isCompound: "true"
*pushButton_SortOrder_Cancel.compoundIcon: "push.xpm"
*pushButton_SortOrder_Cancel.compoundName: "push_Button"
*pushButton_SortOrder_Cancel.x: 230
*pushButton_SortOrder_Cancel.y: 600
*pushButton_SortOrder_Cancel.width: 90
*pushButton_SortOrder_Cancel.height: 40
*pushButton_SortOrder_Cancel.labelString: "QUIT"
*pushButton_SortOrder_Cancel.fontList: "rockwell-bold"
*pushButton_SortOrder_Cancel.activateCallback: {\
XtPopdown(XtParent(XtParent(UxWidget))) ;\
}

*pushButton_SortOrder_Clear.class: pushButton
*pushButton_SortOrder_Clear.static: true
*pushButton_SortOrder_Clear.name: pushButton_SortOrder_Clear
*pushButton_SortOrder_Clear.parent: SortForm
*pushButton_SortOrder_Clear.isCompound: "true"
*pushButton_SortOrder_Clear.compoundIcon: "push.xpm"
*pushButton_SortOrder_Clear.compoundName: "push_Button"
*pushButton_SortOrder_Clear.x: 125
*pushButton_SortOrder_Clear.y: 600
*pushButton_SortOrder_Clear.width: 90
*pushButton_SortOrder_Clear.height: 40
*pushButton_SortOrder_Clear.labelString: "CLEAR"
*pushButton_SortOrder_Clear.fontList: "rockwell-bold"
*pushButton_SortOrder_Clear.activateCallback.source: public
*pushButton_SortOrder_Clear.activateCallback: cb_clear_sort_order_form

*label107.class: label
*label107.static: true
*label107.name: label107
*label107.parent: SortForm
*label107.isCompound: "true"
*label107.compoundIcon: "label.xpm"
*label107.compoundName: "label_"
*label107.x: 20
*label107.y: 60
*label107.height: 20
*label107.labelString: "ORDER BY"

*rc_SortOrder.class: rowColumn
*rc_SortOrder.static: true
*rc_SortOrder.name: rc_SortOrder
*rc_SortOrder.parent: SortForm
*rc_SortOrder.width: 137
*rc_SortOrder.height: 20
*rc_SortOrder.isCompound: "true"
*rc_SortOrder.compoundIcon: "row.xpm"
*rc_SortOrder.compoundName: "row_Column"
*rc_SortOrder.x: 117
*rc_SortOrder.y: 236
*rc_SortOrder.orientation: "horizontal"
*rc_SortOrder.radioBehavior: "true"
*rc_SortOrder.labelString: ""
*rc_SortOrder.numColumns: 1
*rc_SortOrder.packing: "pack_tight"
*rc_SortOrder.whichButton: 1
*rc_SortOrder.sensitive: "true"

*toggleButton_ASCENDING.class: toggleButton
*toggleButton_ASCENDING.static: true
*toggleButton_ASCENDING.name: toggleButton_ASCENDING
*toggleButton_ASCENDING.parent: rc_SortOrder
*toggleButton_ASCENDING.isCompound: "true"
*toggleButton_ASCENDING.compoundIcon: "toggle.xpm"
*toggleButton_ASCENDING.compoundName: "toggle_Button"
*toggleButton_ASCENDING.x: 3
*toggleButton_ASCENDING.y: 531
*toggleButton_ASCENDING.width: 67
*toggleButton_ASCENDING.height: 12
*toggleButton_ASCENDING.labelString: "ASCENDING"
*toggleButton_ASCENDING.set: "true"
*toggleButton_ASCENDING.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_ASCENDING.indicatorSize: 20

*toggleButton_DESCENDING.class: toggleButton
*toggleButton_DESCENDING.static: true
*toggleButton_DESCENDING.name: toggleButton_DESCENDING
*toggleButton_DESCENDING.parent: rc_SortOrder
*toggleButton_DESCENDING.isCompound: "true"
*toggleButton_DESCENDING.compoundIcon: "toggle.xpm"
*toggleButton_DESCENDING.compoundName: "toggle_Button"
*toggleButton_DESCENDING.x: 92
*toggleButton_DESCENDING.y: 531
*toggleButton_DESCENDING.width: 61
*toggleButton_DESCENDING.height: 12
*toggleButton_DESCENDING.labelString: "DESCENDING"
*toggleButton_DESCENDING.fontList: "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1"
*toggleButton_DESCENDING.indicatorSize: 20

*label115.class: label
*label115.static: true
*label115.name: label115
*label115.parent: SortForm
*label115.isCompound: "true"
*label115.compoundIcon: "label.xpm"
*label115.compoundName: "label_"
*label115.x: 32
*label115.y: 246
*label115.height: 20
*label115.labelString: "COLUMN ORDER:"

