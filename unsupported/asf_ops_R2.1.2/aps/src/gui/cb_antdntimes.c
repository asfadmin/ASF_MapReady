#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_antdntimes.c

Description:	

External Functions Defined:
				cb_update_antdntime_form
				cb_show_antdntime_records
				cb_set_antdntimes_editability
				cb_create_new_antdntime_record
				cb_save_antdntime_changes
				cb_delete_antdntime_record

File Scope Functions:
				do_ant_roundup
				ant_roundup_done
				clear_text_areas
	
External Variables Defined:
	
File Scope Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)cb_antdntimes.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_antdntimes.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#define CONTEXT_MACRO_ACCESS 1
#include "vc_antdntimes.h"
#undef CONTEXT_MACRO_ACCESS

#include "UxXt.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_exe_names.h"

#include "db_sybint.h"

#include "aps_db_table.h"
#include "db_antenna_down_times.h"

#include "aps_extern.h"
#include "nmalloc.h"
#include "satmenus.h"
#include "gui_utils.h"
#include "gui_mu_utils.h"
#include "mu_utilities.h"
#include "subprocess.h"

#include "cb_antdntimes.h"
#include "cb_datetime.h"
#include "timeconv.h"

#define ANT_DN_TIMES_EDIT    1 
#define ANT_DN_TIMES_CREATE  2 

#define BEGIN_CONTEXT( widget ) \
    _UxCAntennaDownTimeManager          *UxSaveCtx; \
    UxSaveCtx = UxAntennaDownTimeManagerContext; \
    UxAntennaDownTimeManagerContext = \
            (_UxCAntennaDownTimeManager *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
	} \
   UxAntennaDownTimeManagerContext = UxSaveCtx;

/* 
-- Thu Sep 19 19:15:37 PDT 1996
-- change to remove the antenna_id argument from the command 
-- aps_ant_roundup
-- other source lines were changed to support this, mainly by 
-- removing references to antenna_id when supporting the command 
-- line call and reporting status.  
*/
typedef struct ANT_ROUNDUP
	{
		char	*stationid;
		char	begintime[ASF_TIME_STR_LENGTH+1];
		char	endtime[ASF_TIME_STR_LENGTH+1];
	} ANT_ROUNDUP;


extern void		popup_message();
extern int		quote_doubler();

extern char		display_string[] ;

static llist	*down_times ;
static int      antdntime_perm_id = 0 ; /* permission id for MU support */


/*==============================================================================
Function:		ant_roundup_done

Description:	Popups a status message when the process has completed

Parameters:		Standard X Callback parameters

Returns:		None

Creator:		Teresa McKillop

Creation Date:	01/02/96

Notes:		
==============================================================================*/
static void
ant_roundup_done( PROCESS_INFO *process, ANT_ROUNDUP *ant_roundup )
{
	switch (process->exit_status)
	{
	case APS_EXIT_OK :
		(void) sprintf( display_string,
				"Antenna roundup:\n\n Completed Successfully for\n\n \
STATION: %s\n   START: %s\n    STOP: %s",
		ant_roundup->stationid, 
		ant_roundup->begintime, ant_roundup->endtime );
		popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
			display_string, XtGrabNone );
#ifdef DEBUG
		(void) printf( "%s", display_string );
#endif
		break ;
	case APS_EXIT_ERROR :
		(void) sprintf( display_string,
				"Antenna roundup:\n\n UNSUCCESSFUL Run\n\n \
see %s log file for:\n STATION: %s\n   START: %s\n    STOP: %s",
		ANT_ROUNDUP_CMD, ant_roundup->stationid, 
		ant_roundup->begintime, ant_roundup->endtime );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
#ifdef DEBUG
		(void) printf( "%s", display_string );
#endif
		break ;
	case APSGUI_EXIT_COREDUMP : /* process core dumped */
		(void) sprintf( display_string,
				"Antenna roundup:\n\n Signal Caught CORE DUMPED\n\n \
see %s log file for:\n STATION: %s\n   START: %s\n    STOP: %s",
		ANT_ROUNDUP_CMD, ant_roundup->stationid, 
		ant_roundup->begintime, ant_roundup->endtime );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
#ifdef DEBUG
		(void) printf( "%s", display_string );
#endif
		break ;
	default :	/* process caught a signal, but no core dump */
		(void) sprintf( display_string,
				"Antenna roundup:\n\n SIGNAL Caught (signal = %d)\n\n \
see %s log file for:\n STATION: %s\n   START: %s\n    STOP: %s",
		-(process->exit_status), ANT_ROUNDUP_CMD,
		ant_roundup->stationid, 
		ant_roundup->begintime, ant_roundup->endtime );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
#ifdef DEBUG
		(void) printf( "%s", display_string );
#endif
		break ;
	}

	TimeoutCursors( False, False, NULL ) ;

	free( ant_roundup->stationid );
	free( ant_roundup );

	/* The process has ended, so free the permission */
	(void) gui_free_permission(antdntime_perm_id,
						MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
						MU_PLANNING_ACTIVITY_TYPE) ;
	return;
}


/*==============================================================================
Function:		do_ant_roundup

Description:	sets up and starts the process for doing the antenna roundup

Parameters:		

Returns:		TRUE  - if antenna roundup successfully started
				FALSE - if antenna roundup was not started

Creator:		Teresa McKillop

Creation Date:	01/02/96

Notes:		
==============================================================================*/
static int
do_ant_roundup( char *stationid, 
	char *begintime, char *endtime )
{
	ANT_ROUNDUP		*ant_roundup;
	PROCESS_INFO	*ant_roundup_process;

	int				retval = TRUE;

	TimeoutCursors( True, True, NULL ) ;

	ant_roundup            = (ANT_ROUNDUP *) malloc( sizeof(ANT_ROUNDUP) );
	ant_roundup->stationid = (char *) malloc(
		APS_SIZE( ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STATION_ID ) );

	(void) strcpy( ant_roundup->stationid, stationid );
	(void) strcpy( ant_roundup->begintime, begintime );
	(void) strcpy( ant_roundup->endtime, endtime );	
	
	(void)sprintf( display_string, "%s -U %s -P %s -s %s -b %s -e %s -p %d",
		ANT_ROUNDUP_CMD, userid, password,
		stationid, begintime, endtime, antdntime_perm_id );
	ant_roundup_process = create_process( display_string, NULL, TRUE, NULL,
		NULL, NULL, ant_roundup_done, ant_roundup );
	if (ant_roundup_process == NULL)
	{
		(void) sprintf( display_string,
			"Can't start the ant roundup process.\n Too many processes running?"
			) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		retval = FALSE;

		/* There has been an error, so free the permission */
		(void) gui_free_permission(antdntime_perm_id,
							MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
							MU_PLANNING_ACTIVITY_TYPE) ;
	}
	else if (start_process( ant_roundup_process ))
	{
		 /* can't get pipes, error message already popped up; cleanup */
		destroy_process( ant_roundup_process );
		retval = FALSE;

		/* There has been an error, so free the permission */
		(void) gui_free_permission(antdntime_perm_id,
							MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
							MU_PLANNING_ACTIVITY_TYPE) ;
	}

	if ( retval == FALSE )
		TimeoutCursors( False, False, NULL ) ;

	return (retval);
}


/*==============================================================================
Function:		clear_text_areas

Description:	clears out the text areas (text & textfield widgets)

Parameters:		None

Returns:		None

Creator:		Teresa McKillop

Creation Date:	07/11/96

Notes:		
==============================================================================*/
static void
clear_text_areas()
{
	XmTextFieldSetString( TF_ANT_DN_TIMES_STRTTIME, EMPTY_STR ) ; 
	XmTextFieldSetString( TF_ANT_DN_TIMES_STOPTIME, EMPTY_STR ) ; 
	XmTextFieldSetString(TF_DownTime_total_days, EMPTY_STR) ;
	XmTextSetString( T_ANT_DN_TIMES_COMMENTS, EMPTY_STR ) ;
	XmTextSetString( T_ANT_DN_TIMES_COMMENTS, EMPTY_STR ) ;

	return ;
}


/*==============================================================================
Function:		cb_show_antdntime_records

Description:	
	This function retrieves DownTime records from the database and displays
	them on the screen

Parameters:		Standard X Callback parameters

Returns:		None

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
				Teresa McKillop		12/29/95	modified from rgs to antenna
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_antdntime_records( Widget widget, XtPointer client_data, XtPointer cbs )
{
    int i ;
	char *order_columns ;
	char *search_str ;
    cursor ptr ;
    DB_RECORD **antdntime_rec ;

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

	down_times = db_get_records( APS_dbproc, APS_TABLE( ANTENNA_DOWN_TIMES ), 
		where_clause, orderby_cols, APS_CDEFS( ANTENNA_DOWN_TIMES ), ALL_COLS );

	if (!down_times)
	{
		/* there is a db problem */
		(void) sprintf(display_string,
				"Error with the APS DB\nNeed to fix the DB") ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR", 
				display_string, XtGrabNone) ;

		/* ensure that no items are in the list */
		XtVaSetValues(list_widget,
			XmNitemCount, 0,
			NULL) ;

		return ;
	}

	str_list = 
		(XmStringTable) XtMalloc(NUMELTS(down_times) * sizeof(XmString)) ;

	for (i = 0, antdntime_rec = (DB_RECORD **) FIRST( down_times, ptr )
		; antdntime_rec
		; i++, antdntime_rec = (DB_RECORD **) NEXT(down_times, ptr))
	{
		(void) sprintf( display_string, 
			"%-4s%6d%*s%17.17s%*s%17.17s",
			CAST_ANTENNA_DOWN_TIMES_STATION_ID
				antdntime_rec[ANTENNA_DOWN_TIMES_STATION_ID],
			CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID
				antdntime_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID],
			7, blank_str,
			CAST_ANTENNA_DOWN_TIMES_STRTTIME
				antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME],
			4, blank_str,
			CAST_ANTENNA_DOWN_TIMES_STOPTIME
				antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME] );

		str_list[i] = XmStringCreateLocalized(display_string) ;
	}

	XtVaSetValues(list_widget,
		XmNitems, str_list,
		XmNitemCount, NUMELTS(down_times),
		NULL) ;

	(void) sprintf(display_string, "%5d", NUMELTS(down_times)) ;
	XmTextFieldSetString(TF_DownTime_recordcount, display_string) ;

	for (i = 0; i < NUMELTS(down_times) ;i++)
		XmStringFree(str_list[i]) ;
	XtFree((char *) str_list) ;

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_update_antdntime_form

Description:	
				This is the BrowseSelection Callback for the Down Time list.
				As each Down Time is browsed in the DownTime window, this
				callback is executed

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
				Teresa McKillop		12/29/95	modified from rgs to antenna
==============================================================================*/
void
cb_update_antdntime_form(
	Widget widget, XtPointer client_data, XmListCallbackStruct *cbs )
{
BEGIN_CONTEXT( widget )

	DB_RECORD	**antdntime_rec ;
	Widget		stn_button ;
	char		antenna[APS_MAX_ANTENNA_LEN+1];
	double		et_total_days ;

	antdntime_rec = 
		(DB_RECORD **) db_nth_record(down_times, cbs->item_position) ;

	/* update the station and antenna id option menu */
	stn_button = XtNameToWidget( subMenu_antenna_down_stn_id,
		CAST_ANTENNA_DOWN_TIMES_STATION_ID
		antdntime_rec[ANTENNA_DOWN_TIMES_STATION_ID] );
	XtCallActionProc(stn_button, "ArmAndActivate", NULL, NULL, 0);
	set_stationid_menu( optionMenu_station_id, subMenu_antenna_down_stn_id,
		CAST_ANTENNA_DOWN_TIMES_STATION_ID
		antdntime_rec[ANTENNA_DOWN_TIMES_STATION_ID] );
	(void)sprintf( antenna, "%d",
		CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID
		antdntime_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID] );
	set_antenna_menu( optionMenu_antenna, subMenu_antenna_down_antenna,
		antenna );

#define SHOW_FIELD(column, cast, textField) \
	(void) sprintf(display_string, \
		APS_PFMT(ANTENNA_DOWN_TIMES, (column)), cast antdntime_rec[(column)]) ;\
	XmTextFieldSetString((textField), display_string) ; 

	SHOW_FIELD(ANTENNA_DOWN_TIMES_STRTTIME, CAST_ANTENNA_DOWN_TIMES_STRTTIME,
		TF_ANT_DN_TIMES_STRTTIME)
	SHOW_FIELD(ANTENNA_DOWN_TIMES_STOPTIME, CAST_ANTENNA_DOWN_TIMES_STOPTIME,
		TF_ANT_DN_TIMES_STOPTIME)
#undef SHOW_FIELD

	/* show the total days of down time */
	if (tc_et_ASF_datetime_diff(
		(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME],
		(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME], 
		&et_total_days) == FALSE)
	{
		et_total_days = 0.0;
	}

	(void) sprintf(display_string, "%7.2f", et_total_days) ;
	XmTextFieldSetString(TF_DownTime_total_days, display_string) ;

	(void) sprintf( display_string,
		APS_PFMT( ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_COMMENTS ),
		CAST_ANTENNA_DOWN_TIMES_COMMENTS
			antdntime_rec[ANTENNA_DOWN_TIMES_COMMENTS] ) ;
	XmTextSetString( T_ANT_DN_TIMES_COMMENTS, display_string ) ;

	/* turn on the edit and delete buttons */
	XtSetSensitive(pushButton_EditDownTime, True) ;
	XtSetSensitive(pushButton_DeleteDownTime, True) ;

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_set_antdntimes_editability

Description:	
				Set the antdntimes form's editability so that items that can
				be modified are set so that they can be

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	11/30/1994

Notes:		
				Teresa McKillop		12/29/95	modified from rgs to antenna
==============================================================================*/
void
cb_set_antdntimes_editability(
	Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

	Boolean		editability ;
	int			*pos_list ;
	int			pos_cnt ;
	int			index ;

	editability = (Boolean) client_data ;

	/*
	-- check if a widget is activating this cb if it is
	-- the cbs is not null... therefore we just need this
	-- function to set the fields as editable
	*/
	if (cbs)
	{
		if (editability)  /* editing an existing record */
		{
			/* 
			-- see if a down time record has been selected
			*/
			index = gui_XmList_selected_item(scrolledList_DownTimes) ;
			if (index == 0)
			{
				(void) sprintf(display_string, "Select a Down Time First") ;
				popup_message(XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone) ;
				return ;
			}	

			/* remove any currently installed cb's for the SaveButton */
			XtRemoveAllCallbacks( pushButton_SaveDownTimeChanges,
				XmNactivateCallback) ;

			/* 
			-- now install the cb_save_antdntime cb
			-- passing the ANT_DN_TIMES_EDIT flag
			*/
			XtAddCallback( pushButton_SaveDownTimeChanges, XmNactivateCallback, 
				cb_save_antdntime_changes, (XtPointer) ANT_DN_TIMES_EDIT ) ;
		}
		else /* canceling changes */
		{
			/* 
			-- the cb is a result of a cancel of DownTime changes 
			-- make sure the original values are replaced via
			-- the XmListSelect cb
			*/
			if (XmListGetSelectedPos( scrolledList_DownTimes,
				&pos_list, &pos_cnt ))
			{
				XmListSelectPos( scrolledList_DownTimes, pos_list[0], True ) ;
				XtFree( (char *) pos_list ) ;
			}
		}
		/*
		-- whether we're cancelling or editing a down time we want
		-- the station_id and antenna_id to be inactive, since we can
		-- only select it when we are creating down times
		*/
		XtSetSensitive( optionMenu_station_id, False ) ;
		XtSetSensitive( optionMenu_antenna, False ) ;
	}

	/* turn on/off items that can be edited */
	gui_setEditable( TF_ANT_DN_TIMES_STRTTIME, AG_TEXTFIELD, editability ) ;
	gui_setEditable( TF_ANT_DN_TIMES_STOPTIME, AG_TEXTFIELD, editability ) ;
	gui_setEditable( TF_DownTime_total_days,   AG_TEXTFIELD, editability ) ;
	gui_setEditable( T_ANT_DN_TIMES_COMMENTS,  AG_TEXT,      editability ) ;

	/* turn off/on the other buttons */
	XtSetSensitive( pushButton_SearchDownTime, !editability ) ;
	XtSetSensitive( pushButton_SortDownTime,   !editability ) ;
	XtSetSensitive( pushButton_EditDownTime,   !editability ) ;
	XtSetSensitive( pushButton_DeleteDownTime, !editability ) ;
	XtSetSensitive( pushButton_CreateDownTime, !editability ) ;
	XtSetSensitive( scrolledList_DownTimes,    !editability ) ;

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

	return;
}


/*==============================================================================
Function:		cb_save_antdntime_changes

Description:	
				The activate callback for the SAVE pushbutton

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
				Teresa McKillop		12/29/95	modified from rgs to antenna
==============================================================================*/
/* ARGSUSED2 */
void
cb_save_antdntime_changes( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

	int				*pos_list ;
	int				pos_cnt = 0 ;	

	int				antdntime_savemode ;
	int				index ;

	DB_RECORD		**antdntime_rec ;

	int				nrecs,
					iantenna_id ;

	char			*station_id, *antenna_id,
					*strttime, *stoptime,
					*remarks ;

	char			*db_remarks;
	int				db_remarks_size;

	antdntime_savemode = (Boolean) client_data ;

	index = gui_XmList_selected_item( scrolledList_DownTimes ) ;

	/* verify the changes want to be made */

	strttime = gui_TF_string(TF_ANT_DN_TIMES_STRTTIME) ;
	stoptime = gui_TF_string(TF_ANT_DN_TIMES_STOPTIME) ;

	if ((strttime[0] == STREND) || (stoptime[0] == STREND) )
	{
		(void) sprintf(display_string, "Enter missing start/stop times") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		XtFree(strttime) ; XtFree(stoptime) ;
		return ;
	}

	/* verify start/stop time is valid */

	/* activate the start/stop time fields for error validation */
	XtCallActionProc(TF_ANT_DN_TIMES_STRTTIME, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
	{
		XtFree(strttime) ; XtFree(stoptime) ;
		return ;
	}

	XtCallActionProc(TF_ANT_DN_TIMES_STOPTIME, "activate", NULL, NULL, 0) ;
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
	-- operation create or edit
	*/

	station_id  = gui_optionMenu_string(optionMenu_station_id) ;
	antenna_id  = gui_optionMenu_string(optionMenu_antenna) ;
	iantenna_id = atoi( antenna_id );
	remarks     = gui_TF_string(T_ANT_DN_TIMES_COMMENTS) ;

	if (antdntime_savemode == ANT_DN_TIMES_EDIT)
	{
		/* allocate storage for remarks w/ single quote correction*/
		db_remarks_size =
			APS_SIZE( ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_COMMENTS );
		db_remarks = (char *) malloc( db_remarks_size );
		(void) quote_doubler( remarks, db_remarks, db_remarks_size );
		XtFree(remarks) ;

		antdntime_rec = db_nth_record(down_times, index) ;
	
		if (!antdntime_rec)
		{
			(void) sprintf(display_string, "Select a Down Time record") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;

			/* free all the pointers */
			XtFree(station_id) ; XtFree(antenna_id) ;
			XtFree(strttime) ; XtFree(stoptime) ;
			free( db_remarks );
			return;
		}

		(void) sprintf(question,
			"Save Down Time changes for\n\nSTATION: %s\nANTENNA: %d\n  START: %s\n   STOP: %s?",
			(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STATION_ID],
			CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID
				antdntime_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID],
			(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME],
			(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME]) ;
	}
	else /* ANT_DN_TIMES_CREATE mode */
	{
		/* allocate storage to duplicate remarks (no quote correction) */
		db_remarks_size = strlen( remarks ) + 1 ;
		db_remarks = (char *) malloc( db_remarks_size );
		(void) strcpy( db_remarks, remarks ) ;
		XtFree(remarks) ;

		(void) sprintf(where_clause, 
			"\nwhere %s = '%s' and %s = %d\nand %s = '%s'\nand %s = '%s'", 
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STATION_ID),
				station_id,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_ANTENNA_ID),
				iantenna_id,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STRTTIME), strttime,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STOPTIME), stoptime);

		/* check that the new Down Time doesn't already exist */
		nrecs = db_num_records(APS_dbproc, APS_TABLE(ANTENNA_DOWN_TIMES),
			where_clause) ;
		if (nrecs > 0)
		{
			(void) sprintf(display_string,
				"A Down Time already exists for\n\nSTATION: %s\nANTENNA: %s\n  START: %s\n   STOP: %s",
				station_id, antenna_id, strttime, stoptime) ;

			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;

			/* free all the pointers */
			XtFree(station_id) ; XtFree(antenna_id) ;
			XtFree(strttime) ; XtFree(stoptime) ;
			free( db_remarks );
			return;
		}
		(void) sprintf(question, 
			"Save new Down Time?\n\nSTATION: %s\nANTENNA: %s\n  START: %s\n   STOP: %s?",
			station_id, antenna_id, strttime, stoptime) ;
	}

	if (AskUser(widget, question, NO) == NO)
	{
		/* free all the pointers */
		XtFree(station_id) ; XtFree(antenna_id) ;
		XtFree(strttime) ; XtFree(stoptime) ;
		free( db_remarks );
		return;
	}

	/* continue with the save/update */

	/* Get permission before proceeding */

	antdntime_perm_id = gui_get_planning_permission(MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES, strttime, stoptime, station_id );

	/*
	-- If we did not receive permission, an error dialog has already popped up,
	-- so we return.  Otherwise continue.
	*/
	if (antdntime_perm_id <= 0)
	{
	  	/* free all the pointers */
	  	XtFree(station_id) ; XtFree(antenna_id) ;
		XtFree(strttime) ; XtFree(stoptime) ;
		free( db_remarks );
		return ;
	}

	if (antdntime_savemode == ANT_DN_TIMES_EDIT)
	{
		(void) sprintf(fields_to_set, 
			"%s = '%s', \n%s = '%s',\n%s = '%s'",
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STRTTIME), strttime,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STOPTIME), stoptime,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_COMMENTS),
				db_remarks);

		(void) sprintf( where_clause, 
			"\nwhere %s = '%s' and %s = %d\nand %s = '%s' and %s = '%s'\n", 
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STATION_ID),
				(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STATION_ID],
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_ANTENNA_ID),
			CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID
				antdntime_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID],
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STRTTIME), 
				(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME],
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STOPTIME), 
				(char *) antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME] ) ;

		nrecs = db_update_records(APS_dbproc, 
			APS_TABLE(ANTENNA_DOWN_TIMES), fields_to_set, where_clause) ;

		if (!nrecs)
		{
			(void) sprintf(display_string,
				"Unable to change \n Down Time in table") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;
			/* free all the pointers */
			XtFree(station_id) ; XtFree(antenna_id) ;
			XtFree(strttime) ; XtFree(stoptime) ;
			free( db_remarks );

			/* There has been an error, so free the permission */
		  	(void) gui_free_permission(antdntime_perm_id,
								MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
								MU_PLANNING_ACTIVITY_TYPE) ;
			return ;
		}
	}
	else /* ANT_DN_TIMES_CREATE mode */
	{
		/* make a ANT_DN_TIMES db record to copy the new values into */
		antdntime_rec =  new_table_record(APS_CDEFS(ANTENNA_DOWN_TIMES)) ;

		(void) strcpy( (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STATION_ID],
			station_id ) ;
		CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID
			antdntime_rec[ANTENNA_DOWN_TIMES_ANTENNA_ID] = iantenna_id;
		(void) strcpy( (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME],
			strttime ) ;
		(void) strcpy( (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME],
			stoptime ) ;
		(void) strcpy( (char *) antdntime_rec[ANTENNA_DOWN_TIMES_COMMENTS],
			db_remarks );

#ifdef DEBUG
		db_print_record(antdntime_rec, APS_CDEFS(ANTENNA_DOWN_TIMES)) ;
#endif

		nrecs = db_insert_single_record(APS_dbproc, antdntime_rec, 
			APS_TABLE(ANTENNA_DOWN_TIMES), APS_CDEFS(ANTENNA_DOWN_TIMES)) ;

		if (!nrecs)
		{
			(void) sprintf(display_string,
				"Unable to add new \n Down Time to table") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;
			/* free all the pointers */
			XtFree(station_id) ; XtFree(antenna_id) ;
			XtFree(strttime) ; XtFree(stoptime) ;
			free( db_remarks );

			/* There has been an error, so free the permission */
		  	(void) gui_free_permission(antdntime_perm_id,
								MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
								MU_PLANNING_ACTIVITY_TYPE) ;
			return ;
		}
	}

	/*
	-- Successfully changed the db: start the ant(enna) roundup to update
	-- the DTK database
	*/

	(void) do_ant_roundup( station_id, 
		(strcmp( (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME],
			strttime ) > 0)
			? (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STRTTIME] : strttime,
		(strcmp( (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME],
			stoptime ) > 0)
			? (char *) antdntime_rec[ANTENNA_DOWN_TIMES_STOPTIME] : stoptime );

	/* if we are editing, get the currently selected DownTime, if any */
	if (antdntime_savemode == ANT_DN_TIMES_EDIT)
		XmListGetSelectedPos(scrolledList_DownTimes, &pos_list, &pos_cnt) ;

	/* refresh the Down Times list */
	cb_show_antdntime_records(widget, (XtPointer) scrolledList_DownTimes, NULL);

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
	cb_set_antdntimes_editability(widget, False, NULL) ;

	/* free all the pointers */
	XtFree(station_id) ; XtFree(antenna_id) ;
	XtFree(strttime) ; XtFree(stoptime) ;
	free( db_remarks );

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_create_new_antdntime_record

Description:	
				The activate callback for the CREATE pushbutton

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	10/31/1994

Notes:		
				Teresa McKillop		12/29/95	modified from rgs to antenna
==============================================================================*/
/* ARGSUSED1 */
void
cb_create_new_antdntime_record(
	Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

	/* 
	-- use the set_antdntimes_editablility cb to turn on the fields 
	-- for edit; pass null as the cbs structure so it knows this isn't 
	-- called by a widget
	*/
	cb_set_antdntimes_editability(widget, (XtPointer) TRUE, NULL) ;

	/*
	-- turn on the station and antenna id menus; we only allow
	-- the station id to be set upon creation not on editing
	*/
	XtSetSensitive(optionMenu_station_id, True) ;
	XtSetSensitive(optionMenu_antenna, True) ;

	/* remove any currently installed callbacks for the SaveButton */
	 XtRemoveAllCallbacks(pushButton_SaveDownTimeChanges, XmNactivateCallback) ;

	/* 
	-- now install the cb_save_down_time cb
	-- passing the ANT_DN_TIMES_CREATE flag
	*/
	XtAddCallback(pushButton_SaveDownTimeChanges, XmNactivateCallback, 
		cb_save_antdntime_changes, (XtPointer) ANT_DN_TIMES_CREATE) ;

END_CONTEXT

	return;
}


/*==============================================================================
Function:		cb_delete_antdntime_record

Description:	
				The activate callback for the DELETE button.

Parameters:		Standard X Callback parameters

Returns:		None	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
				Teresa McKillop		12/29/95	modified from rgs to antenna
==============================================================================*/
/* ARGSUSED1 */
void
cb_delete_antdntime_record(
	Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

	char	*strttime ;
    char	*stoptime ;
    char	*station_id ;
	char	*antenna_id ;

	int		iantenna_id ;
	int		index ;

	/* 
	-- see if a down time record has been selected
	*/
	index = gui_XmList_selected_item(scrolledList_DownTimes) ;
	if (index == 0)
	{
		(void) sprintf(display_string, "Select a Down Time First") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		return ;
	}	
	
	/* get the values from the screen */
	strttime    = gui_TF_string(TF_ANT_DN_TIMES_STRTTIME) ;
    stoptime    = gui_TF_string(TF_ANT_DN_TIMES_STOPTIME) ;
	station_id  = gui_optionMenu_string(optionMenu_station_id) ;
	antenna_id  = gui_optionMenu_string(optionMenu_antenna) ;
	iantenna_id = atoi( antenna_id );

	(void) sprintf(question, "Are you sure you want to\nDELETE Down Time scheduled for\n\nSTATION: %s\nANTENNA: %d\n  START: %s\n   STOP: %s?",
		station_id, iantenna_id, strttime, stoptime) ;

	if (AskUser(widget, question, NO) == YES)
	{
		/* Get permission before proceeding */

	  	antdntime_perm_id = gui_get_planning_permission(MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES, strttime, stoptime, station_id );

		/*
		-- If we did not receive permission, an error dialog has already popped
		-- up, so we return.  Otherwise continue.
		*/
		if (antdntime_perm_id <= 0)
		  {
				XtFree(station_id) ;
				XtFree(antenna_id) ;
				XtFree(strttime) ;
				XtFree(stoptime) ;
				return ;
		  }

		(void) sprintf(where_clause,
			"where %s = '%s' and %s = %d and %s = '%s'\nand %s = '%s'",
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STATION_ID),
				station_id,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_ANTENNA_ID),
				iantenna_id,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STRTTIME), strttime,
			APS_COL(ANTENNA_DOWN_TIMES, ANTENNA_DOWN_TIMES_STOPTIME), stoptime);

		if (db_delete_records( APS_dbproc, APS_TABLE(ANTENNA_DOWN_TIMES), 
			where_clause ) == -1)
		{
			(void) sprintf( display_string,
				"APS DB Error:\n can't delete antenna down time\n Fix DB and retry" ) ;
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone ) ;

			/* There has been an error, so free the permission */
		  	(void) gui_free_permission(antdntime_perm_id,
								MU_CREATE_EDIT_OR_DELETE_ANTENNA_DOWN_TIMES,
								MU_PLANNING_ACTIVITY_TYPE) ;
		}
		else
		{
			(void) do_ant_roundup( station_id, strttime, stoptime );

			/* refresh the ANT_DN_TIMES list */
			cb_show_antdntime_records(
					widget, (XtPointer) scrolledList_DownTimes, NULL) ;
			if (!down_times || !NUMELTS( down_times ))
				clear_text_areas() ;

			XmListSelectPos(scrolledList_DownTimes, 1, True) ;
		}

		cb_set_antdntimes_editability(widget, False, NULL) ;
	}

	XtFree(station_id) ;
	XtFree(antenna_id) ;
	XtFree(strttime) ;
	XtFree(stoptime) ;

END_CONTEXT

	return;
}
