#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_searchform.c

Description:	

External Functions Defined:
				void cb_edit_search_columns
				void cb_add_search_column
				void cb_add_search_phrase
				void cb_update_search_field
				void cb_clear_search_form
	
File Scope Functions:
				void update_searchform
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_searchform.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_searchform.c"

#include <stdio.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#define CONTEXT_MACRO_ACCESS 1
#include "vc_searchform.h"
#undef CONTEXT_MACRO_ACCESS

#include "dapps_defs.h"
#include "db_sybint.h"

#include "aps_db_table.h"
#include "cb_searchform.h"
#include "aps_extern.h"

#include "gui_utils.h"

extern void		popup_message() ;
extern char		*get_satellite_string() ;

extern char		display_string[] ;

Widget searchform ;

static char blank[] = " " ;	
static llist *columns  = NULL ;
static DB_RECORD **column ;


#define BEGIN_CONTEXT( widget ) \
	_UxCSearchForm            *UxSaveCtx; \
	UxSaveCtx = UxSearchFormContext; \
	UxSearchFormContext = \
		(_UxCSearchForm *) UxGetContext( widget ) ; \
	{

#define END_CONTEXT \
    } \
	UxSearchFormContext = UxSaveCtx;

/*==============================================================================
Function:		update_searchform

Description:
	Update the SearchForm with the column names of the table relation
	The table relation name is passes as part of the XmNuserData resource 
	of the SearchForm widget.

Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	09/01/1994
Notes:		
==============================================================================*/
static void
update_searchform(Widget searchform)
{
BEGIN_CONTEXT( searchform )

	XmStringTable str_list ;
	XmString default_str ;

	SEARCH_INFO *search ;

	cursor ptr ;

	int i ;

	XtVaGetValues(searchform,
		XmNuserData, &search,
		NULL) ;

	if (columns)
		DEL_LIST( columns ) ;

	if ( !(columns = db_get_column_data( APS_dbproc, search->table_name )))
	{
		(void) sprintf( display_string,
			"DB Error in table %s:\n Can't get the names\n of the search columns\nFix the DB and retry",
			search->table_name ) ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
			display_string, XtGrabNone ) ;
		return ;
	}

	XmTextFieldSetString(TF_search_table_name, search->table_name) ;

	str_list = (XmStringTable) 
		XtMalloc(NUMELTS(columns) * sizeof(XmString)) ;

	for (i = 0, column = FIRST( columns, ptr ) ; column
		; i++, column = NEXT( columns, ptr ))
	{
		(void) sprintf(display_string, "%s", (char *) column[SYBCOL_NAME]) ;
		str_list[i] = XmStringCreateLocalized(display_string) ;
	}

	XtVaSetValues(scrolledList_searchColumnNames,
		 XmNitems, str_list,
		 XmNitemCount, NUMELTS(columns),
		 NULL) ;

	for (i = 0; i < NUMELTS(columns) ; i++)
		XmStringFree(str_list[i]) ;
	XtFree((char*) str_list) ;

	/* use the first column as the selected item */
	column = FIRST(columns, ptr) ;
	default_str = XmStringCreateLocalized((char *) column[0]) ;
	XmListSelectItem(scrolledList_searchColumnNames, default_str, True ) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_edit_search_columns
Description:	
	
Parameters:     Standard X Callback parameters

Returns:     	None

Creator:		Ron Green
Creation Date:	09/27/1994
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_edit_search_columns(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( searchform )

	SEARCH_INFO	*search ;
	char		*search_string ;
	static char	*lastTableName = NULL ;

	search = (SEARCH_INFO *) client_data ;

	/* pass the client data (SEARCH INFO: field to update and table name) */
	XtVaSetValues(searchform,
		XmNuserData, search,
		NULL) ;

	/*
	-- If the table is not the same as the last one, clear the form.
	-- Get the columns for the table
	*/
	if (!lastTableName || (strcmp( search->table_name, lastTableName ) != 0))
	{
		cb_clear_search_form( widget, NULL, NULL );

		lastTableName = realloc( lastTableName, strlen(search->table_name) + 1);
		(void) strcpy( lastTableName, search->table_name ) ;
	}
	update_searchform(searchform) ;

	/* get the current search phrase listed in the field to update */
	search_string = XmTextFieldGetString(search->field_to_update) ;

	/* TO DO ... add to search text window */

	XtFree(search_string) ;
	XtPopup(XtParent(searchform), XtGrabNone) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_add_search_column
Description:	
	Default Action Callback for the XmScrollList Search Columns.  Once a
column is selected it is added the the search clause text field and removed
from the scroll list.

Parameters:     Standard X Callback parameters

Returns:     	None

Creator:		Ron Green
Creation Date:	09/07/1994
Notes:		
==============================================================================*/
void
cb_add_search_column(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs)
{
BEGIN_CONTEXT( widget )

	char *column_name ;

	int *pos_list, pos_cnt ;
	int value_type ;

	/* get the name of the column */
	XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &column_name) ;

	/* now place it in the search colmumn name field */
	XmTextFieldSetString(textField_search_column, column_name) ;

	XmListGetSelectedPos(
		scrolledList_searchColumnNames,	&pos_list, &pos_cnt) ;

	column = db_nth_record(columns, pos_list[0]) ;
	value_type = CAST_SYBCOL_TYPE column[SYBCOL_TYPE] ;

	/* turn off/on the valid operators */

	switch(value_type) 
	{
		case CHAR : 	/* NCHAR same type as char */
		case VARCHAR :  /* NVARCHAR same type as varchar */
			XtSetSensitive(pb_like, TRUE) ;
			XtSetSensitive(pb_notlike, TRUE) ;
			break ;

		default : /* all others */
			XtSetSensitive(pb_like, FALSE) ;
			XtSetSensitive(pb_notlike, FALSE) ;
			break ;

	} /* end switch(value type) */

END_CONTEXT
}



/*==============================================================================
Function:		cb_add_search_phrase

Description:	

Parameters:     Standard X Callback parameters

Returns:     	None

Creator:		Ron Green

Creation Date:	09/28/1994

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_add_search_phrase(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	Widget operator_button ;
	Widget conjunction_button ;

	String String_operator ;
	String String_conjunction ;

	char *conjunction ;
	char *column_name ;
	char *operator ;
	char *str_operator ;
	char *column_value ;

	char *ptr ;

	char *where_string ;
	char *table_name ;
	int count ;

	int *pos_list, pos_cnt ;
	int value_type ;
	char current_phrase[512] ;

	/* 
	-- get the CONJUNCTION if needed 
	-- in other words a phrase already exists
	*/
	if (!XtIsSensitive(optionMenu_connector))
		conjunction = blank ;
	else
	{
		/* get the currently selected conjunction option button */
		XtVaGetValues(optionMenu_connector,
			XmNmenuHistory, &conjunction_button,
			NULL) ;

		/* now get the label string from the selected option button */
		XtVaGetValues(conjunction_button,
			XmNlabelString, &String_conjunction,
			NULL) ;

		/* now convert to a C string */
		XmStringGetLtoR((XmString) String_conjunction,
			XmFONTLIST_DEFAULT_TAG, &conjunction) ;

		XtFree(String_conjunction) ;
	}

	/* get the COLUMN NAME */
	column_name = XmTextFieldGetString(textField_search_column) ;

	/* 
	-- use the selected position (column name) as an index
	-- into the columns llist.  This info will provide the 
	-- value type 
	*/

	XmListGetSelectedPos(
		scrolledList_searchColumnNames,	&pos_list, &pos_cnt) ;

	column = db_nth_record(columns, pos_list[0]) ;
	value_type = CAST_SYBCOL_TYPE column[SYBCOL_TYPE] ;
	XtFree((char*) pos_list) ;

	/* get the OPERATOR VALUE */
	/* get the currently selected operator option button */
	XtVaGetValues(optionMenu_operator,
		XmNmenuHistory, &operator_button,
		NULL) ;

	/* now get the label string from the selected option button */
	XtVaGetValues(operator_button,
		XmNlabelString, &String_operator,
		NULL) ;

	/* now convert to a C string */
	XmStringGetLtoR((XmString) String_operator, XmFONTLIST_DEFAULT_TAG,
		&str_operator) ;
	XtFree(String_operator) ;

	if ((strcmp(str_operator, "like") != 0)  /* if regular operator */
	&&  (strcmp(str_operator, "not like") != 0)) 
	{
		/* the first paren is the start of the operator e.g. (<=) */
		operator = strchr(str_operator, '(') ;
		operator++ ;  /* move one char past that */

		/* now search for the end parentheses */
		ptr = strchr(operator, ')') ;
		*ptr = NULL ;
	}
	else operator = str_operator ;

	/* get the COLUMN VALUE */
	column_value = XmTextFieldGetString(textField_search_value) ;

	if (strlen(column_value) == 0)
	{
		(void) sprintf(display_string,
			"No value given for\n   column name: %s", column_name) ;

		popup_message(
			XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone) ;
		XtFree(str_operator) ;
		XtFree(column_value) ;
		return ;
	}
	
#define SINGLE_QUOTE '\''
#define DOUBLE_QUOTE '"'	
	/*
	-- if the value type is a string type
	-- quotes will have to surround the value 
	*/
	switch(value_type) 
	{
		case CHAR : 	/* NCHAR same type as char */
		case VARCHAR :  /* NVARCHAR same type as varchar */

			/* if no quotes in the string  add them */
			if (!strchr(column_value, SINGLE_QUOTE)
			&&  !strchr(column_value, DOUBLE_QUOTE))
			{
				(void) sprintf(current_phrase, "%s %s '%s' ", 
					column_name, operator, column_value) ;
				break ;
			}

			/* quotes are in the string */
			if (!(	/* if NOT surrounded by single quotes OR double quotes */
				(column_value[0] == SINGLE_QUOTE
				&&  column_value[strlen(column_value) - 1] == SINGLE_QUOTE) 
				|| (column_value[0] == DOUBLE_QUOTE
				&&  column_value[strlen(column_value) - 1] == DOUBLE_QUOTE)
				))
			{
				ptr = strchr(column_value, SINGLE_QUOTE) ;
				while (ptr)
				{
					if (*ptr != *(ptr+1))  /* if not 2 single quotes in a row */
					{
						(void) sprintf(display_string, 
							"Error with (') in Value Field\nSingle Quotes\nmust be specified ('')") ;
						popup_message(
							XmDIALOG_ERROR, "APS:DB ERROR", display_string, XtGrabNone) ;
						XtFree(str_operator) ;
						XtFree(column_value) ;
						return ;
					}
					/* skip past the two quotes */
					ptr++ ; 
					ptr++ ; 
					ptr = strchr(ptr, SINGLE_QUOTE) ;
				}

				/*
				--  all sinqle quotes matched up,
				--  so add the surrounding quotes
				*/
				(void) sprintf(current_phrase, "%s %s '%s' ", 
					column_name, operator, column_value) ;
				break ;
			}

			/* FALLTHROUGH */

		default : /* all others */
			(void) sprintf(current_phrase, "%s %s %s ", 
				column_name, operator, column_value) ;
	} /* end switch(value type) */

	/* check the only the phrase for accurracy */
	table_name = XmTextFieldGetString(TF_search_table_name) ;
	(void) sprintf(display_string, "where %s", current_phrase) ;

	count = db_num_records(APS_dbproc, table_name, display_string) ;
	if (count == -1)
	{
		(void) sprintf(display_string, 
			"Error in current Search Phrase\nPlease Respecify") ;
		popup_message(
			XmDIALOG_ERROR, "APS:DB ERROR", display_string, XtGrabNone) ;
		XtFree(str_operator) ;
		XtFree(column_value) ;
		XtFree(table_name) ;
		return ;
	}

	/* now complete the phrase conjunction and all */
	(void) sprintf(display_string, "%s %s", conjunction, current_phrase) ;
	gui_display_message_widget(scrolledText_SearchClause, display_string) ;

	/* 
	-- turn on conjunctions if this is the first
	-- phrase also turn on the Apply Button
	*/
	if (strcmp(conjunction, blank) == 0) 
		XtSetSensitive(optionMenu_connector, TRUE) ;
	else /* free the space taken by reading the conjunction */
		XtFree(conjunction) ;  

	/* now check how many records match the current phrase */
	where_string = XmTextGetString(scrolledText_SearchClause) ;

	/* get and display the number of matching records */
	count = db_num_records(APS_dbproc, table_name, where_string) ;
	(void) sprintf(display_string, "%d", count) ;
	XmTextFieldSetString(textField_matched_records, display_string) ;

	/* free allocated memory */
	if (str_operator)
		XtFree(str_operator) ;
	XtFree(column_value) ;
	XtFree(table_name) ;
	XtFree(where_string) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_update_search_order_field
Description:	
	The SearchForm OK activate Callback.  Updates the search field on the
activating form (caller) with the results in the search clause window and
pops down the Search Form.

Parameters:     Standard X Callback parameters

Returns:     	None

Creator:		Ron Green
Creation Date:	09/28/1994
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_update_search_field(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	SEARCH_INFO *search ;
	char *searchstr ;

	XtVaGetValues(searchform,
		XmNuserData, &search,
		NULL) ;
	searchstr = gui_TF_string(scrolledText_SearchClause) ;
	if (strcmp(searchstr, "where") == 0)  /* no search clause given */
	{
		(void) sprintf(display_string, "No Search criteria was entered") ;
		popup_message(XmDIALOG_INFORMATION, 
			"APS: Search", display_string, XtGrabNone) ;
	}
	else
		XmTextSetString(search->field_to_update, searchstr) ;

	XtPopdown(XtParent(searchform)) ;
	XtFree(searchstr) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_clear_search_form

Description:	

Parameters:     Standard X Callback parameters

Returns:     	None

Creator:		Ron Green

Creation Date:	09/28/1994

Notes:		
==============================================================================*/
/* ARGSUSED0 */
void
cb_clear_search_form(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( searchform )

	XmTextSetString(scrolledText_SearchClause, "where") ;
	XmTextFieldSetString(textField_search_value, EMPTY_STR) ;
	XmTextFieldSetString(textField_matched_records, "0") ;

	/* turn off the connector button */
	XtSetSensitive(optionMenu_connector, FALSE) ;

END_CONTEXT
}
