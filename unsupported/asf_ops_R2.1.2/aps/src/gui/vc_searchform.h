
/*******************************************************************************
       vc_searchform.h
       This header file is included by vc_searchform.c

*******************************************************************************/

#ifndef	_VC_SEARCHFORM_INCLUDED
#define	_VC_SEARCHFORM_INCLUDED

#include <stdio.h>

#ifdef MOTIF
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <X11/Shell.h>
#include <Xm/MenuShell.h>
#endif /* MOTIF */

#include "UxXt.h"

#ifndef XKLOADDS
#define XKLOADDS
#endif /* XKLOADDS */

/*******************************************************************************
       The definition of the context structure:
       If you create multiple copies of your interface, the context
       structure ensures that your callbacks use the variables for the
       correct copy.

       For each swidget in the interface, each argument to the Interface
       function, and each variable in the Interface Specific section of the
       Declarations Editor, there is an entry in the context structure
       and a #define.  The #define makes the variable name refer to the
       corresponding entry in the context structure.
*******************************************************************************/

typedef	struct
{
	Widget	UxSearchForm;
	Widget	UxscrolledWindowText6;
	Widget	UxscrolledText_SearchClause;
	Widget	UxscrolledWindowList7;
	Widget	UxscrolledList_searchColumnNames;
	Widget	UxtextField_search_column;
	Widget	UxoptionMenu_p2;
	Widget	Uxpb_equal;
	Widget	Uxpb_not_equal;
	Widget	Uxpb_lessthan;
	Widget	Uxpb_greaterthan;
	Widget	Uxpb_lessequal;
	Widget	Uxpb_greaterequal;
	Widget	Uxpb_like;
	Widget	Uxpb_notlike;
	Widget	UxoptionMenu_operator;
	Widget	UxtextField_search_value;
	Widget	UxoptionMenu_p3;
	Widget	Uxpb_and;
	Widget	Uxpb_or;
	Widget	UxoptionMenu_connector;
	Widget	Uxlabel109;
	Widget	UxTF_search_table_name;
	Widget	Uxlabel110;
	Widget	UxpushButton_QuitSearchForm;
	Widget	UxpushButton_AddPhrase;
	Widget	UxpushButton_ApplySearch;
	Widget	UxtextField_matched_records;
	Widget	Uxlabel111;
	Widget	UxpushButton_CancelSearch;
	swidget	UxUxParent;
} _UxCSearchForm;

#ifdef CONTEXT_MACRO_ACCESS
static _UxCSearchForm          *UxSearchFormContext;
#define SearchForm              UxSearchFormContext->UxSearchForm
#define scrolledWindowText6     UxSearchFormContext->UxscrolledWindowText6
#define scrolledText_SearchClause UxSearchFormContext->UxscrolledText_SearchClause
#define scrolledWindowList7     UxSearchFormContext->UxscrolledWindowList7
#define scrolledList_searchColumnNames UxSearchFormContext->UxscrolledList_searchColumnNames
#define textField_search_column UxSearchFormContext->UxtextField_search_column
#define optionMenu_p2           UxSearchFormContext->UxoptionMenu_p2
#define pb_equal                UxSearchFormContext->Uxpb_equal
#define pb_not_equal            UxSearchFormContext->Uxpb_not_equal
#define pb_lessthan             UxSearchFormContext->Uxpb_lessthan
#define pb_greaterthan          UxSearchFormContext->Uxpb_greaterthan
#define pb_lessequal            UxSearchFormContext->Uxpb_lessequal
#define pb_greaterequal         UxSearchFormContext->Uxpb_greaterequal
#define pb_like                 UxSearchFormContext->Uxpb_like
#define pb_notlike              UxSearchFormContext->Uxpb_notlike
#define optionMenu_operator     UxSearchFormContext->UxoptionMenu_operator
#define textField_search_value  UxSearchFormContext->UxtextField_search_value
#define optionMenu_p3           UxSearchFormContext->UxoptionMenu_p3
#define pb_and                  UxSearchFormContext->Uxpb_and
#define pb_or                   UxSearchFormContext->Uxpb_or
#define optionMenu_connector    UxSearchFormContext->UxoptionMenu_connector
#define label109                UxSearchFormContext->Uxlabel109
#define TF_search_table_name    UxSearchFormContext->UxTF_search_table_name
#define label110                UxSearchFormContext->Uxlabel110
#define pushButton_QuitSearchForm UxSearchFormContext->UxpushButton_QuitSearchForm
#define pushButton_AddPhrase    UxSearchFormContext->UxpushButton_AddPhrase
#define pushButton_ApplySearch  UxSearchFormContext->UxpushButton_ApplySearch
#define textField_matched_records UxSearchFormContext->UxtextField_matched_records
#define label111                UxSearchFormContext->Uxlabel111
#define pushButton_CancelSearch UxSearchFormContext->UxpushButton_CancelSearch
#define UxParent                UxSearchFormContext->UxUxParent

#endif /* CONTEXT_MACRO_ACCESS */


/*******************************************************************************
       Declarations of global functions.
*******************************************************************************/

Widget	create_SearchForm( swidget _UxUxParent );

#endif	/* _VC_SEARCHFORM_INCLUDED */
