#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_sortform.c

Description:	

External Functions Defined:
				void cb_edit_sort_columns
				void cb_add_sort_column
				void cb_update_sort_order_field
				void cb_clear_sort_order_form
	
File Scope Functions:
				void update_sortform
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_sortform.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_sortform.c"

#include <stdio.h>
#include <string.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#define CONTEXT_MACRO_ACCESS 1
#include "vc_sortform.h"
#undef CONTEXT_MACRO_ACCESS

#include "dapps_defs.h"
#include "aps_extern.h"
#include "db_sybint.h"
#include "aps_db_table.h"

#include "cb_sortform.h"

#include "gui_utils.h"

extern void		popup_message() ;

extern char		display_string[] ;

Widget sortform ;

#define MAX_SORT_COLUMNS 16

#define BEGIN_CONTEXT( widget ) \
	_UxCSortForm            *UxSaveCtx; \
	UxSaveCtx = UxSortFormContext; \
	UxSortFormContext = \
		(_UxCSortForm *) UxGetContext( widget ) ; \
	{

#define END_CONTEXT \
    } \
	UxSortFormContext = UxSaveCtx;

/*==============================================================================
Function:		update_sortform

Description:
	Update the SortForm with the column names of the table relation
	The table relation name is passes as part of the XmNuserData resource 
	of the SortForm widget.

Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	09/01/1994
Notes:		
==============================================================================*/
static void
update_sortform(Widget sortform)
{
BEGIN_CONTEXT( sortform )

	XmStringTable str_list ;

	SORT_INFO *sort ;

	cursor ptr ;
	llist *columns ;
	DB_RECORD **column ;
	int i ;

	XtVaGetValues(sortform,
		XmNuserData, &sort,
		NULL) ;

	XmTextFieldSetString(textField_table_name, sort->table_name) ;

	if (!(columns = db_get_column_data( APS_dbproc, sort->table_name )))
	{
		(void) sprintf( display_string,
			"DB Error in table %s:\n Can't get the names\n of the sort columns\nFix the DB and retry",
			sort->table_name ) ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
			display_string, XtGrabNone ) ;
		return ;
	}

	str_list = (XmStringTable) 
		XtMalloc(NUMELTS(columns) * sizeof(XmString)) ;

	for (i = 0, column = FIRST( columns, ptr ) ; column
		; i++, column = NEXT( columns, ptr ))
	{
		(void) sprintf(display_string, "%s", (char *) column[0]) ;
		str_list[i] = XmStringCreateLocalized(display_string) ;
	}

	XtVaSetValues(scrolledList_column_names,
		XmNitems, str_list,
		XmNitemCount, NUMELTS(columns),
		NULL) ;

	for (i = 0; i < NUMELTS(columns) ; i++)
		XmStringFree(str_list[i]) ;
	XtFree((char *) str_list) ;

	DEL_LIST(columns) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_edit_sort_columns
Description:	
	
Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	09/06/1994
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_edit_sort_columns(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( sortform )

	XmString	XmString_column_name ;
	SORT_INFO	*sort ;
	char		*sort_string ;
	char		*column_name ;
	static char	*lastTableName = NULL ;

	sort = (SORT_INFO *) client_data ;

	/* pass the client data (SORT INFO: field to update and table name) */
	XtVaSetValues(sortform,
		XmNuserData, sort,
		NULL) ;

	if (!lastTableName || (strcmp( sort->table_name, lastTableName ) != 0))
	{
		/* Note: clear function also does an update_sortform() */
		cb_clear_sort_order_form( widget, NULL, NULL ) ; 

		lastTableName = realloc( lastTableName, strlen(sort->table_name) + 1 );
		(void) strcpy( lastTableName, sort->table_name ) ;
	}
	else
		update_sortform(sortform) ;

	/* get the current sort columns listed in the field to update */
	sort_string = XmTextFieldGetString(sort->field_to_update) ;

	/* add the text to the text window */
	XmTextSetString(scrolledText_SortClause, sort_string) ;

	/* 
	-- if a previous sort order exists, 
	-- remove those column names from the list 
	*/
	if (strlen(sort_string))
	{
		column_name = strtok(sort_string, ", ") ;
		while (column_name)
		{
			if (strcmp(column_name, "asc") != 0
			&&  strcmp(column_name, "desc") != 0)
			{
				/* convert to XmString for use by XmListDelete function */
				XmString_column_name = XmStringCreateLocalized(column_name);
				XmListDeleteItem(scrolledList_column_names, 
					XmString_column_name) ;
				XmStringFree(XmString_column_name) ;
			}
			column_name = strtok(NULL, ", ") ;
		}
	}

	XtFree(sort_string) ;
	XtPopup(XtParent(sortform), XtGrabNone) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_add_sort_column
Description:	
	Default Action Callback for the XmScrollList Sort Columns.  Once a
column is selected it is added the the sort clause text field and removed
from the scroll list.

Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	09/07/1994
Notes:		
==============================================================================*/
void
cb_add_sort_column(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs)
{
BEGIN_CONTEXT( widget )

	/* add a space prior so the colname text will be separated */
	static char asc[] = " asc" ;
	static char desc[] = " desc" ;

	int count ;

	char *column_name ;
	char *sort_order ;
	char *current_sort_string ;
	char *ptr ;

	/* get the name of the column */
	XmStringGetLtoR(cbs->item, XmFONTLIST_DEFAULT_TAG, &column_name) ;

	if (XmToggleButtonGetState(toggleButton_ASCENDING))
		sort_order = asc ;
	else
		sort_order = desc ;

	current_sort_string = gui_TF_string(scrolledText_SortClause) ;

	/* count the number of commas to determine how many coluns we have */
	count = 1 ;
	ptr = strchr(current_sort_string, ',') ;
	while (ptr != NULL)
	{
		count++ ; 
		ptr++ ; /* advance past the comma */
		ptr = strchr(ptr, ',') ;
	}	
	XtFree(current_sort_string) ;

	if (count == MAX_SORT_COLUMNS)
	{
		(void) sprintf(display_string, 
			"Number of Sort Columns\nMay not exceed %d\n",
			 MAX_SORT_COLUMNS) ;
		popup_message(XmDIALOG_ERROR, "APS: DB ERROR", 
			display_string, XtGrabNone) ;
		return ;
	}	

	/* add it to our sort list, prefix with a comma if necessary */
	
	if (XmTextGetLastPosition(scrolledText_SortClause) != 0)
		gui_display_message_widget(scrolledText_SortClause, ", ") ;

	gui_display_message_widget(scrolledText_SortClause, column_name) ;
	gui_display_message_widget(scrolledText_SortClause, sort_order) ;

	/* now remove from the list */
	XmListDeletePos(scrolledList_column_names, cbs->item_position) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_update_sort_order_field
Description:	
	The SortForm OK activate Callback.  Updates the sort field on the
activating form (caller) with the results in the sort clause window and
pops down the Sort Fom.

Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	09/07/1994
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_update_sort_order_field(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	SORT_INFO *sort ;
	char *sortstr ;

	XtVaGetValues(sortform,
		XmNuserData, &sort,
		NULL) ;

	sortstr = gui_TF_string(scrolledText_SortClause) ;
	XmTextSetString(sort->field_to_update, sortstr) ;

	XtPopdown(XtParent(sortform)) ;
	XtFree( sortstr ) ;

END_CONTEXT
}


/* ARGSUSED0 */
void
cb_clear_sort_order_form(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( sortform )

	XmTextSetString(scrolledText_SortClause, EMPTY_STR) ;

	update_sortform(sortform) ;

END_CONTEXT
}
