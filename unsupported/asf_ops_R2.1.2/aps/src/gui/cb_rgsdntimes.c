#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_rgsdntimes.c

Description:	

External Functions Defined:
				cb_popup_downtime_form
				cb_show_asfdntime_records
				cb_update_asfdntime_form
				cb_set_asfdntimes_editability
				cb_save_asfdntime_changes
				cb_create_new_down_time
				cb_delete_asfdntime_record
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			5/30/96: file renamed from cb_asfdntimes.c to cb_rgsdntimes.c.
				some variables and widgets still have "ASF" or "asf" instead
				of "RGS" or "rgs" in their names.

==============================================================================*/
#pragma ident	"@(#)cb_rgsdntimes.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_rgsdntimes.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#define CONTEXT_MACRO_ACCESS 1
#include "vc_rgsdntimes.h"
#undef CONTEXT_MACRO_ACCESS

#include "UxXt.h"

#include "dapps_defs.h"
#include "db_sybint.h"

#include "aps_db_table.h"
#include "db_rgs_down_times.h"

#include "aps_extern.h"
#include "nmalloc.h"
#include "satmenus.h"
#include "gui_utils.h"
#include "gui_mu_utils.h"
#include "mu_utilities.h"
#include "subprocess.h"

#include "cb_rgsdntimes.h"
#include "cb_datetime.h"
#include "timeconv.h"

#define ASF_DN_TIMES_EDIT    1 
#define ASF_DN_TIMES_CREATE  2 

#define BEGIN_CONTEXT(widget) \
    _UxCRGSDownTimeManager          *UxSaveCtx; \
    UxSaveCtx = UxRGSDownTimeManagerContext; \
    UxRGSDownTimeManagerContext =  \
            (_UxCRGSDownTimeManager *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
    } \
	UxRGSDownTimeManagerContext = UxSaveCtx;


static int 	asfdntime_perm_id = 0 ; /* permission id for MU support */

extern void		popup_message() ;
extern int		quote_doubler() ;

extern char		display_string[] ;
extern Widget	filebox ;
extern Widget 	DownTime_manager;

typedef
	struct _TBLE
	{
		char id ;
		char *text ;
	} TABLE ;

static TABLE types[] =
{
	{'P', "PLANNED  "},
	{'U', "UNPLANNED"},
	{' ', NULL}
} ;

static TABLE statuses[] =
{
	{'N', "ACTIVE"},
	{'C', "CANCEL"},
	{' ', NULL}
} ;

static TABLE reasons[] =
{
	{'C', "CONFLICT   "},
	{'M', "MAINTENANCE"},
	{'R', "REPAIR     "},
	{'U', "UPGRADE    "},
	{' ', NULL}
} ;
	
static llist *down_times ;


/*==============================================================================
Function:		cb_popup_asfdntime_form

Description:	
	This function pops up the RGS Down Times interface after it checks for 
	permission.  If it cannot get permission, it pops up an error dialog 
	informing the user.

Parameters:		Standard X Callback parameters

Returns:		None

Creator:		Philip Yurchuk

Creation Date:	12/17/96

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void 
cb_popup_asfdntime_form(Widget widget, XtPointer client_data, XtPointer cbs)
{

BEGIN_CONTEXT ( widget )

	int		ret_code = 0 ;

	/* First check to see if the existing permission is valid */

	if (asfdntime_perm_id > 0)
		ret_code = mu_permission_validate(asfdntime_perm_id,
										 MU_STATION_DOWN_TIMES,
										 MU_SINGLE_ACTIVITY_TYPE);

	/* 
	-- If the permission was invalid, or if we haven't gotten it yet, get 
	-- the permission id. 
	*/

	if ((ret_code <= 0) || (asfdntime_perm_id <= 0))
	{

	  	ret_code = gui_get_single_permission(MU_STATION_DOWN_TIMES) ;

		/* 
		-- If the return code was <= 0, a dialog box explaining why the user
		-- was not granted permission to proceed has already popped up.  If
		-- we successfully obtained permission, set the new perm id and pop
		-- up the interface.
		*/
		
		if (ret_code > 0)
		{
		  	asfdntime_perm_id = ret_code;
			XtPopup(XtParent(DownTime_manager), XtGrabNone) ;
		}
		
	}

	/*
	-- If the permission id was valid, set the new perm id and pop up the 
	-- interface.
	*/

	else
    {
	  	asfdntime_perm_id = ret_code;
		XtPopup(XtParent(DownTime_manager), XtGrabNone) ;
	}

END_CONTEXT

}


/*==============================================================================
Function:		cb_popdown_asfdntime_form

Description:	
	This function pops down the RGS Down Times interface after it terminates its 
	permission.  If it cannot terminate the permission, it pops up an error 
	dialog informing the user.

Parameters:		Standard X Callback parameters

Returns:		None

Creator:		Philip Yurchuk

Creation Date:	12/17/96

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void 
cb_popdown_asfdntime_form(Widget widget, XtPointer client_data, XtPointer cbs)
{
	int ret_code = 0 ;

BEGIN_CONTEXT ( widget )
  
  	ret_code = gui_free_permission(asfdntime_perm_id, 
								   MU_STATION_DOWN_TIMES,
								   MU_SINGLE_ACTIVITY_TYPE) ;

	asfdntime_perm_id = 0 ;

	/* If there was an error, a dialog box has popped up to inform the user */
	XtPopdown(XtParent(DownTime_manager)) ;

END_CONTEXT

}


/*==============================================================================
Function:		cb_show_asfdntime_records

Description:	
	This function retrieves DownTime records from the database and displays
	them on the screen

Parameters:		Standard X Callback parameters

Returns:		None

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_asfdntime_records(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
    int i, j ;
	char *order_columns ;
	char *search_str ;
    cursor ptr ;
    DB_RECORD **asfdntime_rec ;

	char *type, *reason, *status ;
 
    XmStringTable str_list ;

    Widget list_widget = (Widget) client_data ;
 
BEGIN_CONTEXT( list_widget )

	/*
	-- get the available down_times from the db table
	-- and allocate a String Table to hold them
	*/

	if (down_times)
	{
		DEL_LIST(down_times) ;
		down_times = NULL ;
	}

	order_columns = XmTextFieldGetString(TF_DownTime_sortclause) ;
	search_str = XmTextFieldGetString(TF_DownTime_searchclause) ;

	(void) sprintf(orderby_cols, "%s", order_columns) ;
	(void) sprintf(where_clause, "%s", search_str) ;

	if (strcmp(where_clause, "where") == 0)
		where_clause[0] = NULL ;

	XtFree(order_columns) ;
	XtFree(search_str) ;

	down_times = db_get_records(APS_dbproc, APS_TABLE(RGS_DOWN_TIMES), 
		where_clause, orderby_cols, APS_CDEFS(RGS_DOWN_TIMES), ALL_COLS) ;

	if (!down_times)
	{
		(void) sprintf(display_string, 
			"No Down Times found\nPossible Error with APS DB") ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR" , 
			display_string, XtGrabNone) ;

		/* since we had a db problem ensure that no items are in the list */
		XtVaSetValues(list_widget,
			XmNitemCount, 0,
			NULL) ;

		return ;
	}

	str_list = 
		(XmStringTable) XtMalloc(NUMELTS(down_times) * sizeof(XmString)) ;

	/* format each downtime for the display window */ 

	for (i = 0, asfdntime_rec = (DB_RECORD **) FIRST(down_times, ptr)
		; asfdntime_rec
		; i++, asfdntime_rec = (DB_RECORD **) NEXT(down_times, ptr))
	{
		/* get the corresponding type */
		for (j = 0 ; types[j].text ; j++)
		{
			if (types[j].id == CAST_RGS_DOWN_TIMES_UTYPE
				asfdntime_rec[RGS_DOWN_TIMES_UTYPE])
			{
				type = types[j].text ;
				break ;	
			}	
		}

		/* get the corresponding reason */
		for (j = 0 ; reasons[j].text ; j++)
		{
			if (reasons[j].id == CAST_RGS_DOWN_TIMES_UREASON
				asfdntime_rec[RGS_DOWN_TIMES_UREASON])
			{
				reason = reasons[j].text ;
				break ;	
			}	
		}

		/* get the down time status */
		for (j = 0 ; statuses[j].text ; j++)
		{
			if (statuses[j].id == CAST_RGS_DOWN_TIMES_DISPOSITION
				asfdntime_rec[RGS_DOWN_TIMES_DISPOSITION])
			{
				status = statuses[j].text ;
				break ;	
			}	
		}
		
		(void) sprintf( display_string,
			"%-4s%.17s%*s%.17s%*s%s %s %s   %c ",
			CAST_RGS_DOWN_TIMES_STATION_ID
				asfdntime_rec[RGS_DOWN_TIMES_STATION_ID],
			CAST_RGS_DOWN_TIMES_STRTTIME
				asfdntime_rec[RGS_DOWN_TIMES_STRTTIME],
			2, blank_str,
			CAST_RGS_DOWN_TIMES_STOPTIME
				asfdntime_rec[RGS_DOWN_TIMES_STOPTIME],
			1, blank_str,
			type, reason, status, 
			CAST_RGS_DOWN_TIMES_FA_NOTIFICATION
				asfdntime_rec[RGS_DOWN_TIMES_FA_NOTIFICATION] ) ;

		str_list[i] = XmStringCreateLocalized(display_string) ;
	}

	XtVaSetValues(list_widget,
		XmNitems, str_list,
		XmNitemCount, NUMELTS(down_times),
		NULL) ;

	(void) sprintf( display_string, "%5d", NUMELTS(down_times) ) ;
	XmTextFieldSetString( TF_DownTime_recordcount, display_string ) ;

	for (i = 0; i < NUMELTS(down_times) ;i++)
		XmStringFree(str_list[i]) ;
	XtFree((char *) str_list) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_update_asfdntime_form

Description:	

	This is the BrowseSelection Callback for the Down Time list.  As each 
Down Time is browsed in the DownTime window, this callback is executed

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	09/dd/1994
Notes:		
==============================================================================*/
void
cb_update_asfdntime_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs)
{
BEGIN_CONTEXT( widget )

	Widget		menu_status ;
	DB_RECORD	**asfdntime_rec ;
	double		et_total_days ;
	char		disposition ;

	asfdntime_rec = 
		(DB_RECORD **) db_nth_record(down_times, cbs->item_position) ;

#define SHOW_FIELD(column, cast, textField) \
	(void) sprintf(display_string, \
		APS_PFMT(RGS_DOWN_TIMES, (column)), cast asfdntime_rec[(column)]) ; \
	XmTextFieldSetString((textField), display_string) ; 


	SHOW_FIELD(RGS_DOWN_TIMES_STRTTIME, CAST_RGS_DOWN_TIMES_STRTTIME,
		TF_ASF_DN_TIMES_STRTTIME)
	SHOW_FIELD(RGS_DOWN_TIMES_STOPTIME, CAST_RGS_DOWN_TIMES_STOPTIME,
		TF_ASF_DN_TIMES_STOPTIME)
#undef SHOW_FIELD

	/* show the total days of down time */
	tc_et_ASF_datetime_diff(
		(char *) asfdntime_rec[RGS_DOWN_TIMES_STRTTIME],
		(char *) asfdntime_rec[RGS_DOWN_TIMES_STOPTIME], 
		&et_total_days) ;

	(void) sprintf(display_string, "%7.2f", et_total_days) ;
	XmTextFieldSetString(TF_DownTime_total_days, display_string) ;

	XmTextSetString(T_ASF_DN_TIMES_REMARKS,
		(char *) asfdntime_rec[RGS_DOWN_TIMES_REMARKS]) ; 


	/* update the station id option menu */
	set_stationid_menu( optionMenu_station_id, subMenu_rgs_down_stn_id,
		CAST_RGS_DOWN_TIMES_STATION_ID
		asfdntime_rec[RGS_DOWN_TIMES_STATION_ID] ) ;

	/* update the type option menu */
	if (CAST_RGS_DOWN_TIMES_UTYPE asfdntime_rec[RGS_DOWN_TIMES_UTYPE] ==  'P')
		menu_status = ASFdown_type_PLANNED ;
	else if
		(CAST_RGS_DOWN_TIMES_UTYPE asfdntime_rec[RGS_DOWN_TIMES_UTYPE] ==  'U')
	{
		menu_status = ASFdown_type_UNPLANNED ;
	}
	else
	{
		(void) sprintf(display_string,
			"Internal Error\nInvalid Down Time\nType Code: '%c'\nretrieved from the database\nUsing Planned",
			CAST_RGS_DOWN_TIMES_UTYPE asfdntime_rec[RGS_DOWN_TIMES_UTYPE]) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		menu_status = ASFdown_type_PLANNED ;
	}

	XtVaSetValues(optionMenu_ASFdown_type,
		XmNmenuHistory, menu_status,
		NULL) ;


	/* update the reason option menu */
	switch (CAST_RGS_DOWN_TIMES_UREASON asfdntime_rec[RGS_DOWN_TIMES_UREASON])
	{
	case 'C' :  /* conflict other than E1, E2, J1, RS */
		menu_status = ASFdown_reason_CONFLICT ;
		break ;

	case 'M' :  /* mainetenance */
		menu_status = ASFdown_reason_MAINTENANCE ;
		break ;

	case 'R' :  /* repair */
		menu_status = ASFdown_reason_REPAIR ;
		break ;

	case 'U' :  /* upgrade */
		menu_status = ASFdown_reason_UPGRADE ;
		break ;

	default : 
		(void) sprintf(display_string,
			"Internal Error\nInvalid Down Time\nReason Code: '%c'\nretrieved from the database\nUsing Maintenance",
			CAST_RGS_DOWN_TIMES_UREASON asfdntime_rec[RGS_DOWN_TIMES_UREASON]) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		menu_status = ASFdown_reason_MAINTENANCE ;
	}

	XtVaSetValues(optionMenu_ASFdown_reason,
		XmNmenuHistory, menu_status,
		NULL) ;

	/* update the fa notification text field */
	switch (CAST_RGS_DOWN_TIMES_FA_NOTIFICATION
		asfdntime_rec[RGS_DOWN_TIMES_FA_NOTIFICATION])
	{
	case 'Y' :  
		(void) sprintf(display_string, "YES") ;
		break ;

	case 'N' : 
		(void) sprintf(display_string, "NO") ;
		break ;

	default : 
		(void) sprintf(display_string,
			"Internal Error\nInvalid FA Notification\nCode: '%c'\nretrieved from the database\nUsing NO",
			CAST_RGS_DOWN_TIMES_FA_NOTIFICATION
			asfdntime_rec[RGS_DOWN_TIMES_FA_NOTIFICATION]) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		(void) sprintf(display_string, "NO") ;
	}
	XmTextFieldSetString(TF_FA_Notification, display_string) ;

	/* 
	-- set the status toggle of this down time as being either
	-- (N)ew (a.k.a scheduled) or (C)ancel  any other value is incorrect
	*/
	disposition = CAST_RGS_DOWN_TIMES_DISPOSITION
		asfdntime_rec[RGS_DOWN_TIMES_DISPOSITION] ;

	if (disposition == 'N')
		XmToggleButtonSetState(toggleButton_DownTimeScheduled, True, True) ;
	else if (disposition == 'C')
		XmToggleButtonSetState(toggleButton_DownTimeCancelled, True, True) ;
	else
	{
		(void) sprintf(display_string,
			"Internal Error\nInvalid disposition\nCode: '%c'\nretrieved from the database\nUsing New",
			CAST_RGS_DOWN_TIMES_DISPOSITION
			asfdntime_rec[RGS_DOWN_TIMES_DISPOSITION]) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		XmToggleButtonSetState(toggleButton_DownTimeScheduled, True, True) ;
	}

	/* turn on the edit and delete buttons */
	XtSetSensitive(pushButton_EditDownTime, True) ;
	XtSetSensitive(pushButton_DeleteDownTime, True) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_set_asfdntimes_editability

Description:	
	Set the asfdntimes form's editability so that items that can
	be modified are set so that they can be
Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	11/30/1994

Notes:		
==============================================================================*/
void
cb_set_asfdntimes_editability(
	Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	Boolean editability ;
	int *pos_list ;
	int pos_cnt ;
	int index ;
	DB_RECORD **asfdntime_rec ;

	editability = (Boolean) client_data ;

	/*
	-- check if a widget is activating this cb if it is
	-- the cbs is not null... therefore we just need this
	-- function to set the fields as editable
	*/
	if (cbs)
	{
		if (editability)  /* ready to make changes */
		{
			/* 
			-- check if selected down time has already been cancelled
			-- if so don't allow changes, force the entry of a new
			-- record since each record is uniquely identified
			*/
			index = gui_XmList_selected_item(scrolledList_DownTimes) ;
			if (index == 0)
			{
				(void) sprintf(display_string, "Select a Down Time first") ;
				popup_message(XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone) ;
				return ;
			}	

			asfdntime_rec = db_nth_record(down_times, index) ;

			if (CAST_RGS_DOWN_TIMES_DISPOSITION
				asfdntime_rec[RGS_DOWN_TIMES_DISPOSITION] == 'C') /* cancelled*/
			{
				(void) sprintf(display_string,
					"Down Time is already Cancelled\nModifications not allowed"
					) ;
				popup_message(XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone) ;
				return ;
			}

			if (CAST_RGS_DOWN_TIMES_FA_NOTIFICATION
				asfdntime_rec[RGS_DOWN_TIMES_FA_NOTIFICATION] == 'Y')
			{
				(void) sprintf(question, 
					"FA Reports for this Down Time have been already generated.\nSome FAs may not receive modifications.\nYou may have to create a new Down Time.\n\nContinue Editing?") ;
				if (AskUser(widget, question, NO) == NO)
					return ;
			}


			/* remove any currently installed cb's for the SaveButton */
			XtRemoveAllCallbacks(
				pushButton_SaveDownTimeChanges, XmNactivateCallback) ;

			/* 
			-- now install the cb_save_asfdntime cb
			-- passing the ASF_DN_TIMES_EDIT flag
			*/
			XtAddCallback(pushButton_SaveDownTimeChanges, XmNactivateCallback, 
				cb_save_asfdntime_changes, (XtPointer) ASF_DN_TIMES_EDIT) ;
		}
		else /* canceling changes */
		{
			/* 
			-- the cb is a result of a cancel of DownTime changes 
			-- make sure the original values are replaced via
			-- the XmListSelect cb
			*/
			if (XmListGetSelectedPos(scrolledList_DownTimes, &pos_list,
				&pos_cnt))
			{
				XmListSelectPos(scrolledList_DownTimes, pos_list[0], True) ;
				XtFree((char *) pos_list) ;
			}
		}
		/*
		-- whether we're cancelling or editing a down time
		-- we want the station_id to be inactive, since we can
		-- only select it when we are creating down times
		*/
		XtSetSensitive(optionMenu_station_id, False) ;
	}

	/* turn off/on items that can be edited */
	gui_setEditable( TF_ASF_DN_TIMES_STRTTIME, AG_TEXTFIELD, editability ) ;
	gui_setEditable( TF_ASF_DN_TIMES_STOPTIME, AG_TEXTFIELD, editability ) ;
	gui_setEditable( TF_DownTime_total_days,   AG_TEXTFIELD, editability ) ;
	gui_setEditable( T_ASF_DN_TIMES_REMARKS,   AG_TEXT,      editability ) ;
	XtSetSensitive(optionMenu_ASFdown_type, editability) ;
	XtSetSensitive(optionMenu_ASFdown_reason, editability) ;

	/* turn off/on the other buttons */
	XtSetSensitive(pushButton_SearchDownTime, !editability) ;
	XtSetSensitive(pushButton_SortDownTime, !editability) ;
	XtSetSensitive(pushButton_EditDownTime, !editability) ;
	XtSetSensitive(pushButton_DeleteDownTime, !editability) ;
	XtSetSensitive(scrolledList_DownTimes, !editability) ;
	XtSetSensitive(pushButton_CreateDownTime, !editability) ;

	if (editability)
	{
		XtManageChild(pushButton_SaveDownTimeChanges) ;
		XtManageChild(pushButton_CancelDownTimeChanges) ;
	}
	else
	{
		XtUnmanageChild(pushButton_SaveDownTimeChanges) ;
		XtUnmanageChild(pushButton_CancelDownTimeChanges) ;
	}

END_CONTEXT
}



/*==============================================================================
Function:		cb_save_asfdntime_changes

Description:	
	The activate callback for the SAVE pushbutton

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_save_asfdntime_changes(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	int *pos_list ;
	int pos_cnt = 0 ;	

	int asfdntime_savemode ;
	int index ;

	llist *new_down_times ;
	DB_RECORD 
		**nasfdntime_rec,
		**asfdntime_rec ;

	int nrecs ;

	char
		*station_id,
		*strttime, *stoptime,
		*type, *reason,
		*remarks ;

	char	*db_remarks = NULL;
	int		db_remarks_size;

	asfdntime_savemode = (Boolean) client_data ;

	index = gui_XmList_selected_item(scrolledList_DownTimes) ;

	/* verify the changes want to be made */

	strttime = gui_TF_string(TF_ASF_DN_TIMES_STRTTIME) ;
	stoptime = gui_TF_string(TF_ASF_DN_TIMES_STOPTIME) ;

	if ((strttime[0] == STREND) || (stoptime[0] == STREND) )
	{
		(void) sprintf(display_string, "Enter new start and stop times") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		XtFree(strttime) ; XtFree(stoptime) ;
		return ;
	}

	/* verify start/stop time is valid */

	/* activate the start/stop time fields for error validation */
	XtCallActionProc(TF_ASF_DN_TIMES_STRTTIME, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
	{
		XtFree(strttime) ; XtFree(stoptime) ;
		return ;
	}

	XtCallActionProc(TF_ASF_DN_TIMES_STOPTIME, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
	{
		XtFree(strttime) ; XtFree(stoptime) ;
		return ;
	}

	if (time_range_error(strttime, stoptime))
	{
		XtFree(strttime) ; XtFree(stoptime) ;
		return ;
	}

	/* 
	-- create a where clause that describes this Down Time
	-- make the where clause match the appropiate
	-- operation: create or edit
	*/

	station_id = gui_optionMenu_string(optionMenu_station_id) ;
	type = gui_optionMenu_string(optionMenu_ASFdown_type) ;
	reason = gui_optionMenu_string(optionMenu_ASFdown_reason) ;
	remarks = gui_TF_string(T_ASF_DN_TIMES_REMARKS) ;

	if (asfdntime_savemode == ASF_DN_TIMES_EDIT)
	{
		/* allocate storage for remarks w/ single quote correction */
		db_remarks_size =
			APS_SIZE( RGS_DOWN_TIMES, RGS_DOWN_TIMES_REMARKS );
		db_remarks = (char *) malloc( db_remarks_size );
		(void) quote_doubler( remarks, db_remarks, db_remarks_size );
		XtFree( remarks );

		asfdntime_rec = db_nth_record(down_times, index) ;
	
		if (!asfdntime_rec)
		{
			(void) sprintf(display_string, "Select a Down Time first") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;

			/* free all the pointers */
			XtFree(station_id) ;
			XtFree(strttime) ; XtFree(stoptime) ;
			XtFree(type) ;
			XtFree(reason) ;
			free( db_remarks );
			return;
		}
		(void) sprintf(question, 
			"Save Down Time changes for\n\nRGS: %s\nSTART: %s\n STOP: %s?",
			
			CAST_RGS_DOWN_TIMES_STATION_ID
				asfdntime_rec[RGS_DOWN_TIMES_STATION_ID],
			CAST_RGS_DOWN_TIMES_STRTTIME asfdntime_rec[RGS_DOWN_TIMES_STRTTIME],
			CAST_RGS_DOWN_TIMES_STOPTIME
				asfdntime_rec[RGS_DOWN_TIMES_STOPTIME]) ;
	}
	else /* ASF_DN_TIMES_CREATE mode */
	{
		/* allocate storage to duplicate remarks (no quote correction) */
		db_remarks_size = strlen( remarks ) + 1 ;
		db_remarks = (char *) malloc( db_remarks_size );
		(void) strcpy( db_remarks, remarks ) ;
		XtFree(remarks) ;

		(void) sprintf(where_clause, 
			"\nwhere %s = '%s' and %s = '%c'\nand %s = '%s'\n and %s = '%s'", 
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STATION_ID), station_id,
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_DISPOSITION), 'N',
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME), strttime,
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STOPTIME),  stoptime) ;

		/* check that the new Down Time doesn't already exist */
		nrecs = db_num_records(APS_dbproc, APS_TABLE(RGS_DOWN_TIMES), where_clause) ;
		if (nrecs > 0)
		{
			(void) sprintf(display_string, 
				"A Down Time already exists for\n\nSTATION: %s\nSTART:   %s\n STOP:   %s",
				station_id, strttime, stoptime) ;

			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;

			XtFree(station_id) ;
			XtFree(strttime) ; XtFree(stoptime) ;
			XtFree(type) ;
			XtFree(reason) ;
			free( db_remarks );

			return ;
		}
		(void) sprintf(question, 
			"Save new Down Time?\n\n START: %s\n  STOP: %s\n  TYPE: %s\nREASON: %s",
			strttime, stoptime, type, reason) ;
	}

	if (AskUser(widget, question, NO) == NO)
	{
		XtFree(station_id) ;
		XtFree(strttime) ;
		XtFree(stoptime) ;
		XtFree(type) ;
		XtFree(reason) ;
		free( db_remarks );

		return ;
	}

	/* continue with the save/update */

	if (asfdntime_savemode == ASF_DN_TIMES_EDIT)
	{
		(void) sprintf(fields_to_set, 
			"%s = '%s', \n%s = '%s',\
			\n%s = '%c', \n%s = '%c',\n%s = '%s'",

			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME), strttime,
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STOPTIME), stoptime,
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_UTYPE),    type[0],
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_UREASON),  reason[0], 
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_REMARKS),  db_remarks) ;

		(void) sprintf(where_clause, 
			"\nwhere %s = '%s' and %s = '%s'\n", 
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME), 
			CAST_RGS_DOWN_TIMES_STRTTIME asfdntime_rec[RGS_DOWN_TIMES_STRTTIME],

			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STOPTIME), 
			CAST_RGS_DOWN_TIMES_STOPTIME
				asfdntime_rec[RGS_DOWN_TIMES_STOPTIME]) ;

		nrecs = db_update_records(APS_dbproc, 
			APS_TABLE(RGS_DOWN_TIMES), fields_to_set, where_clause) ;

		if (!nrecs)
		{
			(void) sprintf(display_string, 
				"Unable to save changes to table") ;
			popup_message(XmDIALOG_ERROR, "APS:DB ERROR", 
				display_string, XtGrabNone) ;
			XtFree(station_id) ;
			XtFree(strttime) ;
			XtFree(stoptime) ;
			XtFree(type) ;
			XtFree(reason) ;
			free( db_remarks );
			return ;
		}
	}
	else /* ASF_DN_TIMES_CREATE mode */
	{
		/* make a ASF_DN_TIMES db record to copy the new values into */
		nasfdntime_rec =  new_table_record(APS_CDEFS(RGS_DOWN_TIMES)) ;

		(void) strcpy(
			(char *) nasfdntime_rec[RGS_DOWN_TIMES_STATION_ID], station_id) ;

		(void) strcpy( (char *) nasfdntime_rec[RGS_DOWN_TIMES_STRTTIME],
				strttime) ;
		(void) strcpy( (char *) nasfdntime_rec[RGS_DOWN_TIMES_STOPTIME],
				stoptime) ;

		/* set disposition to new record */
		CAST_RGS_DOWN_TIMES_DISPOSITION 
			nasfdntime_rec[RGS_DOWN_TIMES_DISPOSITION] = 'N' ;

		/* set FA Notification to No  */
		CAST_RGS_DOWN_TIMES_FA_NOTIFICATION 
			nasfdntime_rec[RGS_DOWN_TIMES_FA_NOTIFICATION] = 'N' ;

		CAST_RGS_DOWN_TIMES_UTYPE 
			nasfdntime_rec[RGS_DOWN_TIMES_UTYPE] = type[0] ;

		CAST_RGS_DOWN_TIMES_UREASON 
			nasfdntime_rec[RGS_DOWN_TIMES_UREASON] = reason[0] ;

		CAST_RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER 
			nasfdntime_rec[RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER] = 0 ;

		(void) strcpy( (char *) nasfdntime_rec[RGS_DOWN_TIMES_REMARKS] ,
			db_remarks);
		/* 
		-- place the down time in the llist 
		-- so we can add it to the down time table 
		*/
		new_down_times = create_dyn_llist() ;
		APPEND(new_down_times, nasfdntime_rec, free_db_record,
			nasfdntime_rec) ;
#ifdef DEBUG
		db_print_record(nasfdntime_rec, APS_CDEFS(RGS_DOWN_TIMES)) ;
#endif

		nrecs = db_insert_records(APS_dbproc, new_down_times, 
			APS_TABLE(RGS_DOWN_TIMES), APS_CDEFS(RGS_DOWN_TIMES)) ;

		DEL_LIST(new_down_times) ;
		if (!nrecs)
		{
			(void) sprintf(display_string, 
				"Unable to add new \n Down Time to table") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;
			XtFree(station_id) ;
			XtFree(strttime) ;
			XtFree(stoptime) ;
			XtFree(type) ;
			XtFree(reason) ;
			free( db_remarks );
			return ;
		}
	}

	/* if we are editing  get the currently selected DownTime */
	if (asfdntime_savemode == ASF_DN_TIMES_EDIT)
		XmListGetSelectedPos(scrolledList_DownTimes, &pos_list, &pos_cnt) ;

	/* refresh the Down Times list */
	cb_show_asfdntime_records(
		widget, (XtPointer) scrolledList_DownTimes, NULL) ;

	/* 
	-- reselect the item in the list if one was previously selected, force 
	-- the selection cb so as to update the fields on the Down Time Mg form 
	--
	-- NOTE: the record should be in the same position in the list
	-- However, this may not be true if Down Times were added while 
	-- this update is being done.
	*/
	if (pos_cnt)
	{
		XmListSelectPos(scrolledList_DownTimes, pos_list[0], True) ;
		XtFree((char *) pos_list) ;
	}	
	else /* select the first one */
		XmListSelectPos(scrolledList_DownTimes, 1, True) ;
	cb_set_asfdntimes_editability(widget, False, NULL) ;

	/* free all the pointers */
	XtFree(station_id) ;
	XtFree(strttime) ;
	XtFree(stoptime) ;
	XtFree(type) ;
	XtFree(reason) ;
	free( db_remarks );

END_CONTEXT
}



/*==============================================================================
Function:		cb_create_new_down_time

Description:	
	The activate callback for the CREATE pushbutton

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	10/31/1994

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_create_new_down_time(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	/* 
	-- use the set_asfdntimes_editablility cb to turn on the fields 
	-- for edit pass null as the cbs structure so it knows this isn't 
	-- called by a widget
	*/
	cb_set_asfdntimes_editability(widget, (XtPointer) TRUE, NULL) ;


	/*
	-- turn on the station id menu for creation of a new down time
	-- we only allow the station id to be set upon creation
	-- not on editing
	*/
	XtSetSensitive(optionMenu_station_id, True) ;

	/* remove any currently installed callbacks for the SaveButton */
	 XtRemoveAllCallbacks(pushButton_SaveDownTimeChanges, XmNactivateCallback) ;

	/* 
	-- now install the cb_save_down_time cb
	-- passing the ASF_DN_TIMES_CREATE flag
	*/
	XtAddCallback(pushButton_SaveDownTimeChanges, XmNactivateCallback, 
		cb_save_asfdntime_changes, (XtPointer) ASF_DN_TIMES_CREATE) ;

	/* set the status to default to scheduled */
	XmToggleButtonSetState(toggleButton_DownTimeScheduled, True, True) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_delete_asfdntime_record

Description:	
	The activate callback for the DELETE button.

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_delete_asfdntime_record(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	int *pos_list ;
	int pos_cnt ;	

	char *strttime ;
	char *stoptime ;
	char *station_id ;

	int nrecs ;

	station_id = gui_optionMenu_string(optionMenu_station_id) ;
	strttime   = gui_TF_string(TF_ASF_DN_TIMES_STRTTIME) ;
	stoptime   = gui_TF_string(TF_ASF_DN_TIMES_STOPTIME) ;

	if ((strttime[0] == STREND) || (stoptime[0] == STREND) )
	{
		(void) sprintf(display_string, "Select a Down Time first") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		XtFree(station_id) ; XtFree(strttime) ; XtFree(stoptime) ;
		return;
	}

	(void) sprintf(question, 
		"Cancel Down Time scheduled for\n\nSTATION: %s\n  START: %s\n   STOP: %s?",
		station_id, strttime, stoptime) ;

	if (AskUser(widget, question, NO) == YES)
	{
		/* 
		-- set the disposition of this downtime from new to cancel 
		-- and also set the fa_notification field to 'N' so that 
		-- a CRAR file will get generated later.  
		*/
		(void) sprintf(fields_to_set, "%s = '%s', %s = '%s'",
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_DISPOSITION), "C",
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_FA_NOTIFICATION), "N") ;

		(void) sprintf(where_clause,
			"where %s = '%s' and %s = '%s'\nand %s = '%s'",
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STATION_ID), station_id,
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STRTTIME), strttime,
			APS_COL(RGS_DOWN_TIMES, RGS_DOWN_TIMES_STOPTIME), stoptime) ;

		nrecs = db_update_records(APS_dbproc, APS_TABLE(RGS_DOWN_TIMES), 
			fields_to_set, where_clause) ;
		if (!nrecs)
		{
			(void) sprintf(display_string, 
				"Unable to cancel\n Down Time in table") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;
			XtFree(station_id) ;
			XtFree(strttime) ;
			XtFree(stoptime) ;
			return ;
		}

		XmListGetSelectedPos(scrolledList_DownTimes, &pos_list, &pos_cnt) ;

		/* refresh the ASF_DN_TIMES list */
		cb_show_asfdntime_records(
			widget, (XtPointer) scrolledList_DownTimes, NULL) ;

		/* 
		-- reselect the item in the list if one was previously selected, 
		-- force the selection cb so as to update the fields on the Down 
		-- Time Mg form 
		--
		-- NOTE: the record should be in the same position in the list
		-- However, this may not be true if Down Times were added while 
		-- this update is being done.
		*/
		if (pos_cnt)
		{
			XmListSelectPos(scrolledList_DownTimes, pos_list[0], True) ;
			XtFree((char *) pos_list) ;
		}	
		else /* select the first one */
			XmListSelectPos(scrolledList_DownTimes, 1, True) ;

		cb_set_asfdntimes_editability(widget, False, NULL) ;
	}

	XtFree(station_id) ;
	XtFree(strttime) ;
	XtFree(stoptime) ;

END_CONTEXT
}
