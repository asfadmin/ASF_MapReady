! UIMX ascii 2.9 key: 5040                                                      

*SearchForm.class: form
*SearchForm.classinc:
*SearchForm.classspec:
*SearchForm.classmembers:
*SearchForm.classconstructor:
*SearchForm.classdestructor:
*SearchForm.gbldecl: #include <stdio.h>\
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
#pragma ident   "@(#)SearchForm.i	5.1 98/01/08 APS/ASF"\
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.SearchForm.i"\
\
#include "cb_searchform.h"\
\
extern Widget searchform ;\

*SearchForm.ispecdecl:
*SearchForm.funcdecl: swidget create_SearchForm(swidget UxParent)
*SearchForm.funcname: create_SearchForm
*SearchForm.funcdef: "swidget", "<create_SearchForm>(%)"
*SearchForm.argdecl: swidget UxParent;
*SearchForm.arglist: UxParent
*SearchForm.arglist.UxParent: "swidget", "%UxParent%"
*SearchForm.icode:
*SearchForm.fcode: return(rtrn);\

*SearchForm.auxdecl:
*SearchForm.static: true
*SearchForm.name: SearchForm
*SearchForm.parent: NO_PARENT
*SearchForm.parentExpression: UxParent
*SearchForm.defaultShell: topLevelShell
*SearchForm.width: 670
*SearchForm.height: 458
*SearchForm.resizePolicy: "resize_none"
*SearchForm.isCompound: "true"
*SearchForm.compoundIcon: "form.xpm"
*SearchForm.compoundName: "form_"
*SearchForm.x: 162
*SearchForm.y: 191
*SearchForm.unitType: "pixels"
*SearchForm.dialogTitle: "Search Form"

*scrolledWindowText6.class: scrolledWindow
*scrolledWindowText6.static: true
*scrolledWindowText6.name: scrolledWindowText6
*scrolledWindowText6.parent: SearchForm
*scrolledWindowText6.scrollingPolicy: "application_defined"
*scrolledWindowText6.visualPolicy: "variable"
*scrolledWindowText6.scrollBarDisplayPolicy: "static"
*scrolledWindowText6.isCompound: "true"
*scrolledWindowText6.compoundIcon: "scrltext.xpm"
*scrolledWindowText6.compoundName: "scrolled_Text"
*scrolledWindowText6.x: 45
*scrolledWindowText6.y: 80
*scrolledWindowText6.width: 545

*scrolledText_SearchClause.class: scrolledText
*scrolledText_SearchClause.static: true
*scrolledText_SearchClause.name: scrolledText_SearchClause
*scrolledText_SearchClause.parent: scrolledWindowText6
*scrolledText_SearchClause.width: 565
*scrolledText_SearchClause.height: 105
*scrolledText_SearchClause.editMode: "multi_line_edit"
*scrolledText_SearchClause.text: "where "
*scrolledText_SearchClause.scrollHorizontal: "false"
*scrolledText_SearchClause.wordWrap: "true"

*scrolledWindowList7.class: scrolledWindow
*scrolledWindowList7.static: true
*scrolledWindowList7.name: scrolledWindowList7
*scrolledWindowList7.parent: SearchForm
*scrolledWindowList7.scrollingPolicy: "application_defined"
*scrolledWindowList7.visualPolicy: "variable"
*scrolledWindowList7.scrollBarDisplayPolicy: "static"
*scrolledWindowList7.shadowThickness: 0
*scrolledWindowList7.isCompound: "true"
*scrolledWindowList7.compoundIcon: "scrllist.xpm"
*scrolledWindowList7.compoundName: "scrolled_List"
*scrolledWindowList7.x: 140
*scrolledWindowList7.y: 260
*scrolledWindowList7.height: 180
*scrolledWindowList7.resizable: "false"
*scrolledWindowList7.width: 120

*scrolledList_searchColumnNames.class: scrolledList
*scrolledList_searchColumnNames.static: true
*scrolledList_searchColumnNames.name: scrolledList_searchColumnNames
*scrolledList_searchColumnNames.parent: scrolledWindowList7
*scrolledList_searchColumnNames.height: 240
*scrolledList_searchColumnNames.browseSelectionCallback.source: public
*scrolledList_searchColumnNames.browseSelectionCallback: cb_add_search_column
*scrolledList_searchColumnNames.defaultActionCallback.source: public
*scrolledList_searchColumnNames.defaultActionCallback: cb_add_search_column
*scrolledList_searchColumnNames.x: 140
*scrolledList_searchColumnNames.y: 0

*textField_search_column.class: textField
*textField_search_column.static: true
*textField_search_column.name: textField_search_column
*textField_search_column.parent: SearchForm
*textField_search_column.isCompound: "true"
*textField_search_column.compoundIcon: "textfield.xpm"
*textField_search_column.compoundName: "text_Field"
*textField_search_column.x: 140
*textField_search_column.y: 218
*textField_search_column.height: 32
*textField_search_column.cursorPositionVisible: "false"
*textField_search_column.editable: "false"
*textField_search_column.text: ""
*textField_search_column.columns: 12
*textField_search_column.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_search_column.resizeWidth: "false"
*textField_search_column.maxLength: 32
*textField_search_column.width: 120

*optionMenu_operator.class: rowColumn
*optionMenu_operator.static: true
*optionMenu_operator.name: optionMenu_operator
*optionMenu_operator.parent: SearchForm
*optionMenu_operator.rowColumnType: "menu_option"
*optionMenu_operator.subMenuId: "optionMenu_p2"
*optionMenu_operator.isCompound: "true"
*optionMenu_operator.compoundIcon: "optionM.xpm"
*optionMenu_operator.compoundName: "option_Menu"
*optionMenu_operator.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_operator.x: 260
*optionMenu_operator.y: 209
*optionMenu_operator.width: 90
*optionMenu_operator.height: 25
*optionMenu_operator.labelString: " "
*optionMenu_operator.menuHelpWidget: ""
*optionMenu_operator.entryAlignment: "alignment_center"

*optionMenu_p2.class: rowColumn
*optionMenu_p2.static: true
*optionMenu_p2.name: optionMenu_p2
*optionMenu_p2.parent: optionMenu_operator
*optionMenu_p2.rowColumnType: "menu_pulldown"
*optionMenu_p2.x: 0
*optionMenu_p2.y: 95

*pb_equal.class: pushButton
*pb_equal.static: true
*pb_equal.name: pb_equal
*pb_equal.parent: optionMenu_p2
*pb_equal.labelString: "(=) equal\n"
*pb_equal.fontList: "rockwell-bold"
*pb_equal.x: 2
*pb_equal.y: 223
*pb_equal.alignment: "alignment_center"

*pb_not_equal.class: pushButton
*pb_not_equal.static: true
*pb_not_equal.name: pb_not_equal
*pb_not_equal.parent: optionMenu_p2
*pb_not_equal.labelString: "(!=) not equal\n"
*pb_not_equal.fontList: "rockwell-bold"
*pb_not_equal.x: 2
*pb_not_equal.y: 223

*pb_lessthan.class: pushButton
*pb_lessthan.static: true
*pb_lessthan.name: pb_lessthan
*pb_lessthan.parent: optionMenu_p2
*pb_lessthan.labelString: "(<) less than\n"
*pb_lessthan.fontList: "rockwell-bold"
*pb_lessthan.x: 2
*pb_lessthan.y: 223
*pb_lessthan.alignment: "alignment_center"

*pb_greaterthan.class: pushButton
*pb_greaterthan.static: true
*pb_greaterthan.name: pb_greaterthan
*pb_greaterthan.parent: optionMenu_p2
*pb_greaterthan.labelString: "(>) greater than\n"
*pb_greaterthan.fontList: "rockwell-bold"
*pb_greaterthan.x: 2
*pb_greaterthan.y: 223
*pb_greaterthan.alignment: "alignment_center"

*pb_lessequal.class: pushButton
*pb_lessequal.static: true
*pb_lessequal.name: pb_lessequal
*pb_lessequal.parent: optionMenu_p2
*pb_lessequal.labelString: "(<=) less than\nor equal to"
*pb_lessequal.fontList: "rockwell-bold"
*pb_lessequal.x: 2
*pb_lessequal.y: 223
*pb_lessequal.alignment: "alignment_center"

*pb_greaterequal.class: pushButton
*pb_greaterequal.static: true
*pb_greaterequal.name: pb_greaterequal
*pb_greaterequal.parent: optionMenu_p2
*pb_greaterequal.labelString: "(>=) greater than\nor equal to"
*pb_greaterequal.fontList: "rockwell-bold"
*pb_greaterequal.x: 2
*pb_greaterequal.y: 223
*pb_greaterequal.alignment: "alignment_center"

*pb_like.class: pushButton
*pb_like.static: true
*pb_like.name: pb_like
*pb_like.parent: optionMenu_p2
*pb_like.labelString: "like\n"
*pb_like.fontList: "rockwell-bold"
*pb_like.x: 2
*pb_like.y: 223
*pb_like.alignment: "alignment_center"

*pb_notlike.class: pushButton
*pb_notlike.static: true
*pb_notlike.name: pb_notlike
*pb_notlike.parent: optionMenu_p2
*pb_notlike.labelString: "not like\n"
*pb_notlike.fontList: "rockwell-bold"
*pb_notlike.x: 2
*pb_notlike.y: 223

*textField_search_value.class: textField
*textField_search_value.static: true
*textField_search_value.name: textField_search_value
*textField_search_value.parent: SearchForm
*textField_search_value.isCompound: "true"
*textField_search_value.compoundIcon: "textfield.xpm"
*textField_search_value.compoundName: "text_Field"
*textField_search_value.x: 430
*textField_search_value.y: 218
*textField_search_value.height: 32
*textField_search_value.cursorPositionVisible: "true"
*textField_search_value.editable: "true"
*textField_search_value.text: ""
*textField_search_value.columns: 25
*textField_search_value.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*textField_search_value.resizeWidth: "false"
*textField_search_value.maxLength: 200

*optionMenu_connector.class: rowColumn
*optionMenu_connector.static: true
*optionMenu_connector.name: optionMenu_connector
*optionMenu_connector.parent: SearchForm
*optionMenu_connector.rowColumnType: "menu_option"
*optionMenu_connector.subMenuId: "optionMenu_p3"
*optionMenu_connector.isCompound: "true"
*optionMenu_connector.compoundIcon: "optionM.xpm"
*optionMenu_connector.compoundName: "option_Menu"
*optionMenu_connector.compoundEditor: {\
extern swidget UxGUIMePopup UXPROTO((swidget, swidget, int, int));\
UxGUIMePopup(UxThisWidget, NULL, 2, 0);\
}
*optionMenu_connector.x: 25
*optionMenu_connector.y: 216
*optionMenu_connector.width: 90
*optionMenu_connector.height: 25
*optionMenu_connector.labelString: " "
*optionMenu_connector.menuHelpWidget: ""
*optionMenu_connector.sensitive: "false"

*optionMenu_p3.class: rowColumn
*optionMenu_p3.static: true
*optionMenu_p3.name: optionMenu_p3
*optionMenu_p3.parent: optionMenu_connector
*optionMenu_p3.rowColumnType: "menu_pulldown"
*optionMenu_p3.x: 0
*optionMenu_p3.y: 217

*pb_and.class: pushButton
*pb_and.static: true
*pb_and.name: pb_and
*pb_and.parent: optionMenu_p3
*pb_and.labelString: "AND"
*pb_and.fontList: "rockwell-bold"
*pb_and.x: 2
*pb_and.y: 230

*pb_or.class: pushButton
*pb_or.static: true
*pb_or.name: pb_or
*pb_or.parent: optionMenu_p3
*pb_or.labelString: "OR"
*pb_or.fontList: "rockwell-bold"
*pb_or.x: 2
*pb_or.y: 230

*label109.class: label
*label109.static: true
*label109.name: label109
*label109.parent: SearchForm
*label109.isCompound: "true"
*label109.compoundIcon: "label.xpm"
*label109.compoundName: "label_"
*label109.x: 5
*label109.y: 25
*label109.height: 20
*label109.labelString: "TABLE:"

*TF_search_table_name.class: textField
*TF_search_table_name.static: true
*TF_search_table_name.name: TF_search_table_name
*TF_search_table_name.parent: SearchForm
*TF_search_table_name.isCompound: "true"
*TF_search_table_name.compoundIcon: "textfield.xpm"
*TF_search_table_name.compoundName: "text_Field"
*TF_search_table_name.x: 45
*TF_search_table_name.y: 20
*TF_search_table_name.height: 32
*TF_search_table_name.cursorPositionVisible: "false"
*TF_search_table_name.editable: "false"
*TF_search_table_name.text: ""
*TF_search_table_name.columns: 12
*TF_search_table_name.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"
*TF_search_table_name.resizeWidth: "false"
*TF_search_table_name.maxLength: 12

*label110.class: label
*label110.static: true
*label110.name: label110
*label110.parent: SearchForm
*label110.isCompound: "true"
*label110.compoundIcon: "label.xpm"
*label110.compoundName: "label_"
*label110.x: 44
*label110.y: 59
*label110.height: 20
*label110.labelString: "SEARCH CRITERIA:"

*pushButton_QuitSearchForm.class: pushButton
*pushButton_QuitSearchForm.static: true
*pushButton_QuitSearchForm.name: pushButton_QuitSearchForm
*pushButton_QuitSearchForm.parent: SearchForm
*pushButton_QuitSearchForm.isCompound: "true"
*pushButton_QuitSearchForm.compoundIcon: "push.xpm"
*pushButton_QuitSearchForm.compoundName: "push_Button"
*pushButton_QuitSearchForm.x: 510
*pushButton_QuitSearchForm.y: 400
*pushButton_QuitSearchForm.width: 90
*pushButton_QuitSearchForm.height: 40
*pushButton_QuitSearchForm.labelString: "QUIT"
*pushButton_QuitSearchForm.fontList: "rockwell-bold"
*pushButton_QuitSearchForm.activateCallback: {\
XtPopdown(XtParent(searchform)) ;\
}

*pushButton_AddPhrase.class: pushButton
*pushButton_AddPhrase.static: true
*pushButton_AddPhrase.name: pushButton_AddPhrase
*pushButton_AddPhrase.parent: SearchForm
*pushButton_AddPhrase.isCompound: "true"
*pushButton_AddPhrase.compoundIcon: "push.xpm"
*pushButton_AddPhrase.compoundName: "push_Button"
*pushButton_AddPhrase.x: 275
*pushButton_AddPhrase.y: 295
*pushButton_AddPhrase.width: 90
*pushButton_AddPhrase.height: 40
*pushButton_AddPhrase.labelString: "ADD\nPHRASE"
*pushButton_AddPhrase.fontList: "rockwell-bold"
*pushButton_AddPhrase.sensitive: "true"
*pushButton_AddPhrase.activateCallback.source: public
*pushButton_AddPhrase.activateCallback: cb_add_search_phrase
*pushButton_AddPhrase.createManaged: "true"

*pushButton_ApplySearch.class: pushButton
*pushButton_ApplySearch.static: true
*pushButton_ApplySearch.name: pushButton_ApplySearch
*pushButton_ApplySearch.parent: SearchForm
*pushButton_ApplySearch.isCompound: "true"
*pushButton_ApplySearch.compoundIcon: "push.xpm"
*pushButton_ApplySearch.compoundName: "push_Button"
*pushButton_ApplySearch.x: 275
*pushButton_ApplySearch.y: 400
*pushButton_ApplySearch.width: 90
*pushButton_ApplySearch.height: 40
*pushButton_ApplySearch.labelString: "APPLY\nSEARCH"
*pushButton_ApplySearch.fontList: "rockwell-bold"
*pushButton_ApplySearch.sensitive: "true"
*pushButton_ApplySearch.activateCallback.source: public
*pushButton_ApplySearch.activateCallback: cb_update_search_field
*pushButton_ApplySearch.createManaged: "true"

*textField_matched_records.class: textField
*textField_matched_records.static: true
*textField_matched_records.name: textField_matched_records
*textField_matched_records.parent: SearchForm
*textField_matched_records.isCompound: "true"
*textField_matched_records.compoundIcon: "textfield.xpm"
*textField_matched_records.compoundName: "text_Field"
*textField_matched_records.x: 455
*textField_matched_records.y: 300
*textField_matched_records.height: 30
*textField_matched_records.columns: 5
*textField_matched_records.cursorPositionVisible: "false"
*textField_matched_records.editable: "false"
*textField_matched_records.resizeWidth: "false"
*textField_matched_records.text: "0"
*textField_matched_records.fontList: "-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1"

*label111.class: label
*label111.static: true
*label111.name: label111
*label111.parent: SearchForm
*label111.isCompound: "true"
*label111.compoundIcon: "label.xpm"
*label111.compoundName: "label_"
*label111.x: 400
*label111.y: 300
*label111.height: 30
*label111.labelString: "MATCHING\nRECORDS:"
*label111.alignment: "alignment_end"

*pushButton_CancelSearch.class: pushButton
*pushButton_CancelSearch.static: true
*pushButton_CancelSearch.name: pushButton_CancelSearch
*pushButton_CancelSearch.parent: SearchForm
*pushButton_CancelSearch.isCompound: "true"
*pushButton_CancelSearch.compoundIcon: "push.xpm"
*pushButton_CancelSearch.compoundName: "push_Button"
*pushButton_CancelSearch.x: 395
*pushButton_CancelSearch.y: 400
*pushButton_CancelSearch.width: 90
*pushButton_CancelSearch.height: 40
*pushButton_CancelSearch.labelString: "CLEAR\nSEARCH"
*pushButton_CancelSearch.fontList: "rockwell-bold"
*pushButton_CancelSearch.sensitive: "true"
*pushButton_CancelSearch.activateCallback.source: public
*pushButton_CancelSearch.activateCallback: cb_clear_search_form
*pushButton_CancelSearch.createManaged: "true"

