#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_permstatus.c

Description:	the callback functions for the APS gui permission
				status display.

External Functions Defined:
				cb_init_permstatus
				cb_popup_permstatus
				cb_quit_permstatus
				cb_refresh_perms
				cb_autoUpdate_toggled
				cb_interval_popup_OK
				cb_undo_interval_popup
	
File Scope Functions:
				refresh_perms
				t_cb_do_autoUpdate
				modify_autoTimeout
				interval_is_valid
				set_interval_message
				set_last_updated_message
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)cb_permstatus.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_permstatus.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#include "dapps_defs.h"
#include "db_sybint.h"
#include "timeconv.h"

#include "aps_defs.h"
#include "aps_extern.h"
#include "aps_db_table.h"	/* includes aps_db_tablenums.h */

#include "db_active_dar_activities.h"
#include "db_active_planning_activities.h"
#include "db_active_single_activities.h"
#include "mu_utilities.h"

#include "gui_defs.h"
#include "gui_utils.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_permstatus.h"
#undef CONTEXT_MACRO_ACCESS
#include "cb_permstatus.h"


/*==============================================================================
	Global Variables
==============================================================================*/
extern void			popup_message();

extern XtAppContext	UxAppContext;
extern Widget		MUIntervalDialog ;
extern Widget		Interval_tf ;

extern char			display_string[] ;


/*==============================================================================
	Static Globals
==============================================================================*/
static void			modify_autoTimeout( int interval ) ;
static Boolean		interval_is_valid( int interval ) ;
static void			set_interval_message( Boolean action ) ;
static void			set_last_updated_message() ;


#define MU_TIMEOUT_OFF_ID		0
#define MU_TIMEOUT_OFF_VALUE	0	/* also used for check for valid interval */

#define	MU_ON					True
#define	MU_OFF					False

#define BEGIN_CONTEXT( widget ) \
    _UxCMUPermissionStatus          *UxSaveCtx; \
    UxSaveCtx = UxMUPermissionStatusContext; \
    UxMUPermissionStatusContext = \
            (_UxCMUPermissionStatus *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
	} \
	UxMUPermissionStatusContext = UxSaveCtx;


static int			defaultInterval ;
static int			savedInterval = MU_TIMEOUT_OFF_VALUE ;



/*==============================================================================
Function:       refresh_perms

Description:    Gets and displays the active permissions
				and updates the time-of-last-update.

Parameters:     None

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/20/96

Notes:		
==============================================================================*/
static void
refresh_perms()
{
	static XmFontList	origFontList = NULL;
	static XmFontList	bigFontList = NULL;	/* the origFontList + the big font*/

	static char		plnFmtStr[BUFSIZ] ;	/* in-pln displayed line format string*/
	static char		*darFmtStr = NULL ;	/* dar displayed line format string */
	static char		*singleFmtStr ;		/* single displayed line format string*/

	/* db record variables */
	DB_RECORD		**permRec ;
	cursor			permPtr ;

	/* permission status list variables */
	llist			*inPlnList ;
	llist			*DARList ;
	llist			*singleList ;

	int				numPerms ;			/* no. of active permission rec's */
	XmStringTable	permStrTbl ;

	int				retStat ;
	int				index ;
	int				i ;
	char			*strPtr ;
	char			tmpBuf[BUFSIZ] ;

	/* initialize/save the original fontList */
	if (origFontList == NULL)
	{
		XtVaGetValues( MU_perm_scrolledList,
			XmNfontList, &origFontList,
			NULL ) ;

		/* a larger BOLD font */
		bigFontList = UxConvertFontList(
					"-misc-fixed-bold-r-normal--13-120-75-75-c-80-iso8859-1" ) ;
	}

	/* create the format for displaying the lines (done ONCE) */
	if (darFmtStr == NULL)
	{
		/* User */
		strPtr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES,
				ACTIVE_PLANNING_ACTIVITIES_USERID, TRUE ) ;
		(void) strcpy( plnFmtStr, strPtr ) ;
		free( strPtr ) ;
		/* Node */
		strPtr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES,
				ACTIVE_PLANNING_ACTIVITIES_NODE, TRUE ) ;
		(void) sprintf( plnFmtStr, "%s %s", plnFmtStr, strPtr ) ;
		free( strPtr ) ;
		/* Command/Activity */
		(void) sprintf(  plnFmtStr, "%s %%-%ds", plnFmtStr,
				APS_SIZE( ACTIVE_PLANNING_ACTIVITIES,
					ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME ) - 1 +
					APS_SIZE( ACTIVE_PLANNING_ACTIVITIES,
					ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID ) ) ;

		singleFmtStr = (char *) malloc( strlen( plnFmtStr ) + 1 ) ;
		(void) strcpy( singleFmtStr, plnFmtStr ) ;

		/* DARID | Stn (darid field is larger than station, so use it's width)*/
		i = aps_max_fw( ACTIVE_DAR_ACTIVITIES,
				ACTIVE_DAR_ACTIVITIES_DARID ) ;
		/* finish the DAR line format */
		strPtr = aps_set_fw_pfmt( ACTIVE_DAR_ACTIVITIES,
				ACTIVE_DAR_ACTIVITIES_DARID, i, TRUE ) ;
		(void) sprintf( plnFmtStr, "%s %s", plnFmtStr, strPtr ) ;
		free( strPtr ) ;
		darFmtStr = (char *) malloc( strlen( plnFmtStr ) + 1 ) ;
		(void) strcpy( darFmtStr, plnFmtStr ) ;
		/* now back out DARID, and finish the in-planning format */
		(void) strcpy( plnFmtStr, singleFmtStr ) ;
		strPtr = aps_set_fw_pfmt( ACTIVE_PLANNING_ACTIVITIES,
				ACTIVE_PLANNING_ACTIVITIES_STATION_ID, i, TRUE ) ;
		(void) sprintf( plnFmtStr, "%s %s", plnFmtStr, strPtr ) ;
		free( strPtr ) ;

		/* Start Time */
		strPtr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES,
				ACTIVE_PLANNING_ACTIVITIES_STRTTIME, TRUE ) ;
		(void) sprintf( plnFmtStr, "%s %s", plnFmtStr, strPtr ) ;
		free( strPtr ) ;
		/* Stop Time */
		strPtr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES,
				ACTIVE_PLANNING_ACTIVITIES_STOPTIME, TRUE ) ;
		(void) sprintf( plnFmtStr, "%s %s", plnFmtStr, strPtr ) ;
		free( strPtr ) ;
	}

	/*
	 *	Get the current permissions (ie, the status)
	 */

	inPlnList	= create_dyn_llist() ;
	DARList		= create_dyn_llist() ;
	singleList	= create_dyn_llist() ;

	retStat = mu_permission_retrieve( APS_dbproc,
			inPlnList, DARList, singleList ) ;
	if (retStat < 0)
	{
		(void) sprintf( display_string,
				"Error getting list of active permissions:\n    %s\n\n%s",
				MU_ERROR_MESSAGE( retStat ),
				"** turning auto-update OFF **" ) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;

		/* turn off auto-update */
		XmToggleButtonSetState( AutoPoll_tb, False, True ) ;

		DEL_LIST( inPlnList ) ;
		DEL_LIST( DARList ) ;
		DEL_LIST( singleList ) ;

		return ;
	}

	numPerms = NUMELTS( inPlnList ) + NUMELTS( DARList ) + 
			NUMELTS( singleList ) ; 
	if (!numPerms)	/* no permissions */
	{
		/* display "NO perms" msg in scrolled list, update time of last */
		index = 2 ;	/* total no. lines in "NO perms" scrolled list */
		permStrTbl	= (XmStringTable) XtMalloc( index * sizeof(XmString) ) ;
		permStrTbl[0] = XmStringCreateLocalized( EMPTY_STR ) ;	/* empty line */
		(void) strcpy( display_string,
				"        *** Currently there are NO active permissions ***" ) ;
		permStrTbl[1] = XmStringCreateLocalized( display_string ) ;

		XtVaSetValues( MU_perm_scrolledList,
			XmNitems, permStrTbl,
			XmNitemCount, index,
			XmNfontList, bigFontList,
			NULL ) ;

		set_last_updated_message() ;

		for (i = 0 ; i < index ; i++)
			XmStringFree( permStrTbl[i] ) ;
		XtFree( (char *) permStrTbl ) ;
		DEL_LIST( inPlnList ) ;
		DEL_LIST( DARList ) ;
		DEL_LIST( singleList ) ;

		return ;
	}

	permStrTbl	= (XmStringTable) XtMalloc( numPerms * sizeof(XmString) ) ;
	index		= 0 ;

	/*
	 *	Display the statuses
	 */

	/* in-planning permissions status */
	for (permRec = (DB_RECORD **) FIRST( inPlnList, permPtr )
		; permRec ; permRec =  (DB_RECORD **) NEXT( inPlnList, permPtr ))
	{
		/* Concatenate cmd/activity */
		(void) strcpy( tmpBuf, gui_trimstring(
				CAST_ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME
				permRec[ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME] ) ) ;
		(void) sprintf( tmpBuf, "%s/%s", tmpBuf, gui_trimstring(
				CAST_ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID
				permRec[ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID] ) ) ;

		/* Create the display line */
		(void) sprintf( display_string, plnFmtStr,
				CAST_ACTIVE_PLANNING_ACTIVITIES_USERID
					permRec[ACTIVE_PLANNING_ACTIVITIES_USERID],
				CAST_ACTIVE_PLANNING_ACTIVITIES_NODE
					permRec[ACTIVE_PLANNING_ACTIVITIES_NODE],
				tmpBuf,
				CAST_ACTIVE_PLANNING_ACTIVITIES_STATION_ID
					permRec[ACTIVE_PLANNING_ACTIVITIES_STATION_ID],
				CAST_ACTIVE_PLANNING_ACTIVITIES_STRTTIME
					permRec[ACTIVE_PLANNING_ACTIVITIES_STRTTIME],
				CAST_ACTIVE_PLANNING_ACTIVITIES_STOPTIME
					permRec[ACTIVE_PLANNING_ACTIVITIES_STOPTIME] ) ;
		permStrTbl[index++] = XmStringCreateLocalized( display_string ) ;
	}

	/* dar permissions status */
	for (permRec = (DB_RECORD **) FIRST( DARList, permPtr )
		; permRec ; permRec =  (DB_RECORD **) NEXT( DARList, permPtr ))
	{
		/* Concatenate cmd/activity */
		(void) strcpy( tmpBuf, gui_trimstring(
				CAST_ACTIVE_DAR_ACTIVITIES_COMMAND_NAME
				permRec[ACTIVE_DAR_ACTIVITIES_COMMAND_NAME] ) ) ;
		(void) sprintf( tmpBuf, "%s/%s", tmpBuf, gui_trimstring(
				CAST_ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID
				permRec[ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID] ) ) ;

		/* Create the display line */
		(void) sprintf( display_string, darFmtStr,
				CAST_ACTIVE_DAR_ACTIVITIES_USERID
					permRec[ACTIVE_DAR_ACTIVITIES_USERID],
				CAST_ACTIVE_DAR_ACTIVITIES_NODE
					permRec[ACTIVE_DAR_ACTIVITIES_NODE],
				tmpBuf,
				CAST_ACTIVE_DAR_ACTIVITIES_DARID
					permRec[ACTIVE_DAR_ACTIVITIES_DARID] ) ;
		permStrTbl[index++] = XmStringCreateLocalized( display_string ) ;
	}

	/* single activity permissions status */
	for (permRec = (DB_RECORD **) FIRST( singleList, permPtr )
		; permRec ; permRec =  (DB_RECORD **) NEXT( singleList, permPtr ))
	{
		/* Concatenate cmd/activity */
		(void) strcpy( tmpBuf, gui_trimstring(
				CAST_ACTIVE_SINGLE_ACTIVITIES_COMMAND_NAME
				permRec[ACTIVE_SINGLE_ACTIVITIES_COMMAND_NAME] ) ) ;
		(void) sprintf( tmpBuf, "%s/%s", tmpBuf, gui_trimstring(
				CAST_ACTIVE_SINGLE_ACTIVITIES_MU_ACTIVITY_ID
				permRec[ACTIVE_SINGLE_ACTIVITIES_MU_ACTIVITY_ID] ) ) ;

		/* Create the display line */
		(void) sprintf( display_string, singleFmtStr,
				CAST_ACTIVE_SINGLE_ACTIVITIES_USERID
					permRec[ACTIVE_SINGLE_ACTIVITIES_USERID],
				CAST_ACTIVE_SINGLE_ACTIVITIES_NODE
					permRec[ACTIVE_SINGLE_ACTIVITIES_NODE],
				tmpBuf ) ;
		permStrTbl[index++] = XmStringCreateLocalized( display_string ) ;
	}

	XtVaSetValues( MU_perm_scrolledList,
		XmNitems, permStrTbl,
		XmNitemCount, index,
		XmNfontList, origFontList,	/* use the original font */
		NULL ) ;

		XtVaSetValues( MU_perm_scrolledList,
			NULL ) ;

	/* set the time of last update */
	set_last_updated_message() ;

	for (i = 0 ; i < index ; i++)
		XmStringFree( permStrTbl[i] ) ;
	XtFree( (char *) permStrTbl ) ;
	DEL_LIST( inPlnList ) ;
	DEL_LIST( DARList ) ;
	DEL_LIST( singleList ) ;

	return ;
}


/*==============================================================================
Function:       cb_init_permstatus

Description:    Function to initialize the permission status processing.

Parameters:     standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/05/96

Notes:			Needs to be called AFTER the interval dialog popup is created
				in order to set the interval TextField on the dialog.
==============================================================================*/
/* ARGSUSED1 */
void
cb_init_permstatus( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

	char		*tmpStr ;

	/*
	-- set the MU default interval: from the env var, if any; else
	--	from the constant.  If this is not a valid value, set it
	--	to the "off" value
	*/

	if ((tmpStr = getenv( APS_MU_INT_ENVVAR )))
		defaultInterval = atoi( tmpStr ) ;
	else	/* use the constant */
		defaultInterval = MU_DEFAULT_INTERVAL ;

	if (interval_is_valid( defaultInterval ) == False)
		defaultInterval = MU_TIMEOUT_OFF_VALUE ;

	/*
	-- init. the interval TextField and saved interval
	*/

	(void) sprintf( display_string, "%d", defaultInterval ) ;
	XmTextFieldSetString( Interval_tf, display_string ) ;
	savedInterval = defaultInterval ;

END_CONTEXT
	return ;
}


/*==============================================================================
Function:       cb_popup_permstatus

Description:    Callback to initialize the permission status window
				when it is being popped up.

Parameters:     standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/05/96

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_popup_permstatus( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

	/* display the current permission status */
	refresh_perms() ;

	/* if auto-updating was in progress during the last popdown, turn it on */
	if (interval_is_valid( savedInterval ) == True)
	{
		/* set the interval to the saved interval */
		(void) sprintf( display_string, "%d", savedInterval ) ;
		XmTextFieldSetString( Interval_tf, display_string ) ;

		modify_autoTimeout( savedInterval ) ;
		XmToggleButtonSetState( AutoPoll_tb, True, False ) ;
		set_interval_message( MU_ON ) ;
	}

END_CONTEXT
	return ;
}


/*==============================================================================
Function:       cb_quit_permstatus

Description:    Callback when the MU permission status QUIT button
				is activated.  Pops down the window, so turns off
				auto-updating during the time the window is popped down.

Parameters:     standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/25/96

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_quit_permstatus( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

	/* if auto-updating, turn off while popped down */
	if (XmToggleButtonGetState( AutoPoll_tb ) == True)
	{
		modify_autoTimeout( MU_TIMEOUT_OFF_VALUE ) ;
		XmToggleButtonSetState( AutoPoll_tb, False, False ) ;
		set_interval_message( MU_OFF ) ;
	}

	XtPopdown( gui_GetShellWidget( widget ) ) ;

END_CONTEXT
	return ;
}


/*==============================================================================
Function:       cb_refresh_perms

Description:    The callback to get and display the active permissions

Parameters:     standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/20/96

Notes:		
==============================================================================*/
/* ARGSUSED0 */
void
cb_refresh_perms( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

		/* display the current perms */
		refresh_perms() ;

		/* update the autoTimeout */
		if (XmToggleButtonGetState( AutoPoll_tb ) == True)
		{
			modify_autoTimeout( savedInterval ) ;
		}

END_CONTEXT
	return ;
}


/*==============================================================================
Function:       cb_autoUpdate_toggled

Description:    Callback when the auto-updating value is changed.

Parameters:     standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/25/96

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_autoUpdate_toggled( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

	if (XmToggleButtonGetState( AutoPoll_tb ) == True)	/* set, get interval */
	{
		char	*intervalStr ;	/* have to "capture" this, so can be free'ed */

		/* popup the interval dialog */
		intervalStr		= XmTextFieldGetString( Interval_tf ) ;
		savedInterval	= atoi( intervalStr ) ;	/* save for reset/cancel */
		XtFree( intervalStr ) ;
		if (interval_is_valid( savedInterval ) == False)
		{
			savedInterval = defaultInterval ;
			(void) sprintf( display_string, "%d", savedInterval ) ;
			XmTextFieldSetString( Interval_tf, display_string ) ;
		}
		/* popup/manage the dialog, if ok, will start auto-updating */
		XtManageChild( MUIntervalDialog ) ;
	}
	else	/* not set, turn off auto-update */
	{
		modify_autoTimeout( MU_TIMEOUT_OFF_VALUE ) ;
		savedInterval = MU_TIMEOUT_OFF_VALUE ;
		set_interval_message( MU_OFF ) ;
	}

END_CONTEXT
	return ;
}


/*==============================================================================
Function:       cb_interval_popup_OK

Description:    Callback when the OK button is activated on the interval
				dialog popup.  Processes the interval value, and when
				valid, turns on auto-updating.

Parameters:     standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/05/96

Notes:			
==============================================================================*/
/* ARGSUSED1 */
void
cb_interval_popup_OK( Widget widget, XtPointer client_data_UNUSED,
		XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

	int		interval ;
	char	*intervalStr ;	/* have to "capture" this, so can be free'ed */

	/* if interval is valid, turn on auto-updating */
	intervalStr	= XmTextFieldGetString( Interval_tf ) ;
	interval	= atoi( intervalStr ) ;
	XtFree( intervalStr ) ;
	if (interval_is_valid( interval ) == True)
	{
		/* this is a new interval, update the saved value */
		savedInterval = interval ;

		/* get the scrolled list of active perms */
		refresh_perms() ;

		/* turn on auto-updating */
		modify_autoTimeout( interval ) ;
		set_interval_message( MU_ON ) ;	/* just in case interval changed */

		/* popdown/unmanage this dialog */
		XtUnmanageChild( gui_GetShellWidget( widget ) ) ;

	}
	else	/* invalid interval, popup error message */
	{
		(void) sprintf( display_string,
				"Interval for auto-updating must be greater than %d",
				MU_TIMEOUT_OFF_VALUE ) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}

END_CONTEXT
	return ;
}


/*==============================================================================
Function:       cb_undo_interval_popup

Description:    Callback for undoing any changes on the interval dialog
				popup.  (The initiating action is either reset or cancel).

Parameters:     standard X Callback parameters
				client_data	- the initiating action:
							  MU_INTERVAL_RESET or MU_INTERVAL_CANCEL

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/03/96

Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_undo_interval_popup( Widget widget, XtPointer client_data,
		XtPointer cbs_UNUSED )
{
	int		action = (int) client_data ;

	/* reset the interval to the saved interval */
	(void) sprintf( display_string, "%d", savedInterval ) ;
	XmTextFieldSetString( Interval_tf, display_string ) ;

	if (action == MU_INTERVAL_CANCEL)	/* cancelling turning on auto-update */
	{
		/* since auto-update is already off, just need to reset values */
		XmToggleButtonSetState( AutoPoll_tb, False, False ) ;
		savedInterval = MU_TIMEOUT_OFF_VALUE ;
		XtUnmanageChild( gui_GetShellWidget( widget ) ) ;
	}
	
	return ;
}


/*==============================================================================
Function:       t_cb_do_autoUpdate

Description:    Callback when the auto-update timeout expires:
				it's time to do the auto-update and reset the timeout.

Parameters:     (XtTimerCallbackProc parameters)
                client_data - interval for the next timeout

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/05/96

Notes:			
==============================================================================*/
/* ARGSUSED2 */
static void
t_cb_do_autoUpdate( XtPointer client_data, XtPointer cbs_UNUSED )
{
	int		interval = (int) client_data ;

	/* get the scrolled list of active perms */
	refresh_perms() ;

	/* if auto-update wasn't turned off when getting the scrolled list */
	if (XmToggleButtonGetState( AutoPoll_tb ) == True)
	{
		/* set up the next timeout */
		modify_autoTimeout( interval ) ;
	}

	return ;
}


/*==============================================================================
Function:       modify_autoTimeout

Description:    sets/resets the timeout for auto-updating of the active
				permissions.

Parameters:     interval	- the number of seconds to use for the
							  timeout (if it equals MU_TIMEOUT_OFF_VALUE,
							  reset the timeout)

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/25/96

Notes:		
==============================================================================*/
static void
modify_autoTimeout( int interval )
{
	static XtIntervalId		lastTimeoutId = MU_TIMEOUT_OFF_ID ;

	/* first, remove any registered timeout */
	if (lastTimeoutId != MU_TIMEOUT_OFF_ID)
	{
		XtRemoveTimeOut( lastTimeoutId ) ;
	}

	if (interval != MU_TIMEOUT_OFF_VALUE)
	{
		lastTimeoutId = XtAppAddTimeOut( UxAppContext,
				(unsigned long) interval * SECOND_TO_MILLISECS,
				(XtTimerCallbackProc) t_cb_do_autoUpdate,
				(XtPointer) interval ) ;
	}
	else
		lastTimeoutId = MU_TIMEOUT_OFF_ID ;

	return ;
}


/*==============================================================================
Function:       interval_is_valid

Description:    verifies that the interval is a positive number.

Parameters:     interval	- the interval to be validated.

Returns:        True, if the interval is valid.
				False, if the interval is not valid.

Creator:        Teresa McKillop

Creation Date:  12/03/96

Notes:		
==============================================================================*/
static Boolean
interval_is_valid( int interval )
{
	if (interval > MU_TIMEOUT_OFF_VALUE)
		return (True) ;
	else
		return (False) ;
}


/*==============================================================================
Function:       set_interval_message

Description:    Sets auto-poll's interval message.

Parameters:     action	- if MU_ON, sets the value in the interval message
						  to the current interval
						  if MU_OFF, sets the value to the auto-poll off msg

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/03/96

Notes:		
==============================================================================*/
static void
set_interval_message( Boolean action )
{
	char			*intervalStr ;
	XmString		tmpXmStr ;

	if (action == MU_ON)
	{
		intervalStr = gui_trimstring( XmTextFieldGetString( Interval_tf ) ) ;
		switch( atoi( intervalStr ) )
		{
		case 0:
			(void) strcpy( display_string, "Auto-update" ) ;
			break;
		case 1:
			(void) sprintf( display_string, "Auto-update every second" ) ;
			break;
		default:
			(void) sprintf( display_string,
					"Auto-update every %s seconds", intervalStr ) ;
			break;
		}
		XtFree( intervalStr ) ;
	}
	else
		(void) strcpy( display_string, "Auto-update" ) ;

	tmpXmStr = XmStringCreateLocalized( display_string ) ;
	XtVaSetValues( AutoPoll_tb,
		XmNlabelString, tmpXmStr,
		NULL ) ;
	XmStringFree( tmpXmStr ) ;

	return ;
}


/*==============================================================================
Function:       set_last_updated_message

Description:    sets the last-update time to the current time
				(in ASF time format: yyyy:doy:hh:mm:ss:uuu)

Parameters:     None

Returns:        None

Creator:        Teresa McKillop

Creation Date:  11/25/96

Notes:		
==============================================================================*/
static void
set_last_updated_message()
{
	XmString		tmpXmStr ;

	(void) tc_systime2asf( display_string ) ;	/* always returns TRUE */
	(void) strcat( display_string, ")" ) ;	/* catenate with closing paren. */

	tmpXmStr = XmStringCreateLocalized( display_string ) ;
	XtVaSetValues( PollTime_lbl,
		XmNlabelString, tmpXmStr,
		NULL ) ;

	XmStringFree( tmpXmStr ) ;

	return ;
}
