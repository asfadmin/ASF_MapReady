#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		gui_mu_utils.c

Description:	Utility functions for the multi-user capability
				of APS.

External Functions Defined:
				gui_get_planning_permission
				gui_get_dar_permission
				gui_free_permission
				gui_get_single_permission
	
File Scope Functions:
				gui_mu_show_pln_blocked
				gui_mu_show_dar_blocked
				gui_mu_show_sgl_blocked
				gui_mu_perror
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)gui_mu_utils.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.gui_mu_utils.c"

/*==============================================================================
    Include Files
==============================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <Xm/Xm.h>

#include "dapps_defs.h"
#include "aps_extern.h"
#include "aps_db_table.h"
#include "aps_db_tablenums.h"
#include "db_active_planning_activities.h"
#include "db_active_dar_activities.h"
#include "mu_utilities.h"

#include "gui_mu_utils.h"


/*==============================================================================
    Constants/Macros
==============================================================================*/
#define GUI_BLOCKED_MSG	"The following activities prevent this one:\n"

#define BLOCK_MSG_INTRO	"BLOCKing permissions are:"


/*==============================================================================
    Function Prototypes (extern or static)
==============================================================================*/
extern void		popup_message() ;

static void		gui_mu_show_pln_blocked( char *, llist * ) ;
static void		gui_mu_show_dar_blocked( char *, llist * ) ;
static void		gui_mu_show_sgl_blocked( char *, llist * ) ;
static void		gui_mu_perror( char *, int ) ;


/*==============================================================================
    Global Variables
==============================================================================*/
extern char		display_string[] ;

static char		*blockedMsg = NULL ;/* malloc'ed storage for blocked messages */
static int		blockedMsgLen = 0 ;	/* amount of storage currently malloc'ed */



/*==============================================================================
Function:       gui_get_planning_permission

Description:    Tries to get the in-planning permission with the input
				parameters.  If the mu request fails to grant the permission
				or it returns an error code, this routine formats and displays
				an appropriate message.

Parameters:     muActivityId	- mu activity that needs the permission
				startTime		- dtk start time
				stopTime		- dtk stop time
				stationId		- station id

Returns:        the return value from the call to the permission request
				function:
					> 0, the permission id
					= 0, permission not granted; is blocked
					< 0, an mu library error occurred

Creator:        Teresa McKillop

Creation Date:  12/09/96

Notes:		
==============================================================================*/
int
gui_get_planning_permission(
		char	*muActivityId,
		char	*startTime,
		char	*stopTime,
		char	*stationId )
{
	llist		*mu_block_list ;
	int			muRetval ;	/* return value from permission request */
	char		msgStr[BUFSIZ] ;

	mu_block_list = create_dyn_llist() ;

	/*
	--	Request the in-planning permission
	*/

	muRetval = mu_permission_request( APS_dbproc, MU_EMPTY_INT_PARAM,
			muActivityId, MU_PLANNING_ACTIVITY_TYPE, mu_block_list,
			startTime, stopTime, stationId, MU_EMPTY_INT_PARAM ) ;

	/*
	--	If couldn't get the permission, inform the user
	*/

	if (muRetval < 0 )	/* error occurred */
	{
		(void) sprintf( msgStr, "For %s\n    strt time: %s\n    stop time: %s\n    stn: %s\n\n%s\n    ",
				muActivityId, startTime, stopTime, stationId,
				"This permission request ERROR occurred" ) ;
		gui_mu_perror( msgStr, muRetval ) ;
	}
	else if (muRetval == 0)	/* permission not granted, ie, "blocked" */
	{
		(void) sprintf( msgStr, "Could not get %s permission for %s\n    strt time: %s\n    stop time: %s\n    stn: %s\n",
				MU_PLANNING_ACTIVITY_TYPE,
				muActivityId, startTime, stopTime, stationId ) ;
		gui_mu_show_pln_blocked( msgStr, mu_block_list ) ;
	}
	
	DEL_LIST( mu_block_list ) ;

	return (muRetval) ;
}


/*==============================================================================
Function:       gui_get_dar_permission

Description:    Tries to get the dar activity permission with the input
				parameters.  If the mu request fails to grant the permission
				or it returns an error code, this routine formats and displays
				an appropriate message.

Parameters:     muActivityId	- mu activity that needs the permission
				dar_id			- the id of the DAR being modified/created

Returns:        the return value from the call to the permission request
				function:
					> 0, the permission id
					= 0, permission not granted; is blocked
					< 0, an mu library error occurred

Creator:        Philip Yurchuk

Creation Date:  1/31/97

Notes:		
==============================================================================*/
int
gui_get_dar_permission(
		char	*muActivityId,
		int		dar_id )
{
	llist		*mu_block_list ;
	int			muRetval ;	/* return value from permission request */
	char		msgStr[BUFSIZ] ;

	mu_block_list = create_dyn_llist() ;

	/*
	--	Request the DAR activity permission
	*/

	muRetval = mu_permission_request( APS_dbproc, MU_EMPTY_INT_PARAM,
			muActivityId, MU_DAR_ACTIVITY_TYPE, mu_block_list,
			NULL, NULL, NULL, dar_id ) ;

	/*
	--	If couldn't get the permission, inform the user
	*/

	if (muRetval < 0 )	/* error occurred */
	{
		(void) sprintf( msgStr, "For %s\n    DAR id: %d\n\n%s\n    ",
				muActivityId, dar_id,
				"This permission request ERROR occurred" ) ;
		gui_mu_perror( msgStr, muRetval ) ;
	}
	else if (muRetval == 0)	/* permission not granted, ie, "blocked" */
	{
		(void) sprintf(msgStr, "Could not get %s permission for %s\n    DAR id: %d\n",
					   MU_DAR_ACTIVITY_TYPE, muActivityId, dar_id ) ;
		gui_mu_show_dar_blocked( msgStr, mu_block_list ) ;
	}
	
	DEL_LIST( mu_block_list ) ;

	return (muRetval) ;
}


/*==============================================================================
Function:       gui_get_single_permission

Description:    Tries to get the single activity permission with the input
				parameters.  If the mu request fails to grant the permission
				or it returns an error code, this routine formats and displays
				an appropriate message.

Parameters:     muActivityId	- mu activity that needs the permission

Returns:        the return value from the call to the permission request
				function:
					> 0, the permission id
					= 0, permission not granted; is blocked
					< 0, an mu library error occurred

Creator:        Teresa McKillop

Creation Date:  01/09/96

Notes:		
==============================================================================*/
int
gui_get_single_permission( char	*muActivityId )
{
	llist		*mu_block_list ;
	int			muRetval ;	/* return value from permission request */
	char		msgStr[BUFSIZ] ;

	mu_block_list = create_dyn_llist() ;

	/*
	--	Request the single activity permission
	*/

	muRetval = mu_permission_request( APS_dbproc, MU_EMPTY_INT_PARAM,
			muActivityId, MU_SINGLE_ACTIVITY_TYPE, mu_block_list,
			NULL, NULL, NULL, MU_EMPTY_INT_PARAM ) ;

	/*
	--	If couldn't get the permission, inform the user
	*/

	if (muRetval < 0 )	/* error occurred */
	{
		(void) sprintf( msgStr,
				"This %s permission request ERROR occurred for %s\n    ",
				MU_SINGLE_ACTIVITY_TYPE, muActivityId ) ;
		gui_mu_perror( msgStr, muRetval ) ;
	}
	else if (muRetval == 0)	/* permission not granted, ie, "blocked" */
	{
		(void) sprintf( msgStr, "Could not get %s permission for %s\n",
				MU_SINGLE_ACTIVITY_TYPE, muActivityId ) ;
		gui_mu_show_sgl_blocked( msgStr, mu_block_list ) ;
	}
	
	DEL_LIST( mu_block_list ) ;

	return (muRetval) ;
}


/*==============================================================================
Function:       gui_free_permission

Description:    Releases the muActivityType permission with the
				input id.

Parameters:     permId			- id of the permission to release
				muActivityId	- mu activity that has the permission
				muActivityType	- mu permission type

Returns:        the return value from the call to the permission
				terminate function:
					> 0, the permission id, the permission was released
					= 0, the permission doesn't exist
					< 0, an mu library error occurred

Creator:        Teresa McKillop

Creation Date:  12/12/96

Notes:		
==============================================================================*/
int
gui_free_permission( int permId, char *muActivityId, char *muActivityType )
{
	char		msgStr[BUFSIZ] ;
	int			muRetval ;

	if ((muRetval = mu_permission_terminate( APS_dbproc,
			permId, muActivityId, muActivityType )) < 0)
	{
		(void) sprintf( display_string,
				"Error releasing %s permission (permission id: %d)\nfor %s\n",
				muActivityType, permId, muActivityId ) ;
		gui_mu_perror( msgStr, muRetval ) ;
	}

	return (muRetval) ;
}


/*==============================================================================
Function:       gui_mu_perror

Description:    Formats and pops up an error message when an mu library
				error has occurred.

				First prints the "str", then a colon and a blank, then the
				message based on "mu_errno" and a newline.  If "str" is NULL,
				prints just the message based on "mu_errno" and a newline.

Parameters:     str			- caller supplied error string
				mu_errno	- the mu library error number

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/11/96

Notes:		
==============================================================================*/
static void
gui_mu_perror( char *str, int mu_errno )
{
	if (str != NULL)
		(void) sprintf( display_string, "%s: ", str ) ;
	else
		display_string[0] = STREND ;

	(void) sprintf( display_string, "%s%s\n",
			display_string, MU_ERROR_MESSAGE( mu_errno ) ) ;

	popup_message( XmDIALOG_ERROR, "APS:MU ERROR" ,
            display_string, XtGrabNone ) ;

	return ;
}


/*==============================================================================
Function:       gui_mu_show_pln_blocked

Description:    Formats and pops up a in-planning permission not granted
				message, which includes a list of the permissions blocking
				the requested permission.

				First prints the "str", then a newline, then the
				BLOCK_MSG_INTRO and 2 newlines, then a columns-hdr line
				and a formatted list of blocking permissions.  If "str"
				is NULL, just prints the BLOCK_MSG_INTRO, 2 newlines,
				columns-hdr, and the formatted list of blocking permissions.

Parameters:     str				- caller supplied blocked permission message
				blocked_list	- llist of permissions blocking the request

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/11/96

Notes:		
==============================================================================*/
static void
gui_mu_show_pln_blocked( char *str, llist *blocked_list )
{
	static char	*fmtStr = NULL ;	/* fmt string for list of blocking perm.s */
	static int	fmtLineLen ;	/* length of a blocking perm. formatted line */
	char		*tmpStr ;

	int			currMsgLen = 0 ;	/* length of entire formatted message */

	/* MU permission-blocked "llist" variables */
	DB_RECORD	**permRec ;
	cursor		permPtr ;

	if (!fmtStr)
	{
		/*
		--	make the format string:
		--		userid, node, pid, cmd_name, acty_id, stn, strt, stop
		--	(allocate more than is needed and free up the unused bytes later)
		*/

		if ((fmtStr = malloc( BUFSIZ )) == NULL)
		{
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					"Can't alloc space for the \"permission BLOCKed\" format",
					XtGrabNone ) ;
			return ;
		}

		/* will append each formatted line via sprintf to the end of the
		-- message, so need "%s    " for the initial message portion
		*/
		(void) strcpy( fmtStr, "%s    " ) ;

		/* USER */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_USERID, TRUE ) ;
		(void) sprintf( fmtStr, "%s%s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* NODE */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_NODE, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* PID */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* invoking CMD */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* ACTIVITY */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* STATION */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_STATION_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* START Time */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_STRTTIME, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* STOP Time */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_STOPTIME, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* newline */
		(void) sprintf( fmtStr, "%s%c", fmtStr, NEWLINE ) ;

		/* free up unused bytes, should never fail since are giving up bytes */
		fmtStr = realloc( fmtStr, strlen( fmtStr ) ) ;

		/* Calculate the formatted line length */
		fmtLineLen = aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_USERID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_NODE ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_STATION_ID )
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_STRTTIME ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_STOPTIME ) 
				+ 5 /* 4 blanks at beg of line and newline at the end */ ;
	}

	/*
	--	Make sure there is enough storage space for the message
	*/

	if (str != NULL)
		currMsgLen = strlen( str ) + 1 ;		/* str plus newline */
	currMsgLen += strlen( BLOCK_MSG_INTRO ) + 2	/* 2 newlines */
			+ fmtLineLen /* line of column headers */
			+ NUMELTS( blocked_list ) * fmtLineLen
			+ 1		/* string terminator */ ;

	if (currMsgLen > blockedMsgLen)
	{
		char	*tmpPtr ;

		if ((tmpPtr = realloc( blockedMsg, currMsgLen )) == NULL)
		{
			(void) sprintf( display_string, "Can't alloc enough space for \"permission BLOCKed\" message\n(%s)",
					str ) ;
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone ) ;

			/* NOTE: blockedMsg is unchanged, so storage/size are still valid */
			return ;
		}
		blockedMsg = tmpPtr ;
		blockedMsgLen	= currMsgLen ;
	}

	/*
	--	format the message
	*/

	/* caller's individual message */
	if (str != NULL)
		(void) sprintf( blockedMsg, "%s\n", str ) ;
	else
		blockedMsg[0] = STREND ;

	/* append the standard message and 2 newlines */
	(void) sprintf( blockedMsg, "%s%s\n\n", blockedMsg, BLOCK_MSG_INTRO ) ;

	/* append the columns-hdr line */
	(void) sprintf( blockedMsg, fmtStr, blockedMsg,
			"(user", "node", "pid", "command",
			"activity", "stn", "start time", "stop time)" ) ;

	/* format each permission in the list, and append this formatted line */
	for (permRec = FIRST( blocked_list, permPtr )
			; permRec != (DB_RECORD **) NULL
			; permRec = (DB_RECORD **) NEXT( blocked_list, permPtr ))
	{
		(void) sprintf( blockedMsg, fmtStr, blockedMsg,
				CAST_ACTIVE_PLANNING_ACTIVITIES_USERID
					permRec[ACTIVE_PLANNING_ACTIVITIES_USERID],
                CAST_ACTIVE_PLANNING_ACTIVITIES_NODE
                    permRec[ACTIVE_PLANNING_ACTIVITIES_NODE],
				CAST_ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID
                    permRec[ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID],
				CAST_ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME
					permRec[ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME],
				CAST_ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID
					permRec[ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID],
                CAST_ACTIVE_PLANNING_ACTIVITIES_STATION_ID
                    permRec[ACTIVE_PLANNING_ACTIVITIES_STATION_ID],
                CAST_ACTIVE_PLANNING_ACTIVITIES_STRTTIME
                    permRec[ACTIVE_PLANNING_ACTIVITIES_STRTTIME],
                CAST_ACTIVE_PLANNING_ACTIVITIES_STOPTIME
                    permRec[ACTIVE_PLANNING_ACTIVITIES_STOPTIME] ) ;
	}

	/*
	--	Display the message
	*/
	
	popup_message( XmDIALOG_ERROR, "APS:ERROR",
			blockedMsg, XtGrabNone ) ;

	return ;
}


/*==============================================================================
Function:       gui_mu_show_dar_blocked

Description:    Formats and pops up a in-planning permission not granted
				message, which includes a list of the permissions blocking
				the requested permission.

				First prints the "str", then a newline, then the
				BLOCK_MSG_INTRO and 2 newlines, then a columns-hdr line
				and a formatted list of blocking permissions.  If "str"
				is NULL, just prints the BLOCK_MSG_INTRO, 2 newlines,
				columns-hdr, and the formatted list of blocking permissions.

Parameters:     str				- caller supplied blocked permission message
				blocked_list	- llist of permissions blocking the request

Returns:        None

Creator:        Philip Yurchuk

Creation Date:  1/30/97

Notes:		
==============================================================================*/
static void
gui_mu_show_dar_blocked( char *str, llist *blocked_list )
{
	static char	*fmtStr = NULL ;	/* fmt string for list of blocking perm.s */
	static char	*hdrfmtStr = NULL ;	/* fmt string for list header */
	static int	fmtLineLen ;	/* length of a blocking perm. formatted line */
	char		*tmpStr ;

	int			currMsgLen = 0 ;	/* length of entire formatted message */
	int			index ; 

	/* MU permission-blocked "llist" variables */
	DB_RECORD	**permRec ;
	cursor		permPtr ;

	if (!fmtStr)
	{
		/*
		--	make the format string:
		--		userid, node, pid, cmd_name, acty_id, dar_id
		--	(allocate more than is needed and free up the unused bytes later)
		*/

		if ((fmtStr = malloc( BUFSIZ )) == NULL)
		{
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					"Can't alloc space for the \"permission BLOCKed\" format",
					XtGrabNone ) ;
			return ;
		}

		if ((hdrfmtStr = malloc( BUFSIZ )) == NULL)
		{
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					"Can't alloc space for the \"permission BLOCKed\" format",
					XtGrabNone ) ;
			return ;
		}

		/* will append each formatted line via sprintf to the end of the
		-- message, so need "%s    " for the initial message portion
		*/
		(void) strcpy( fmtStr, "%s    " ) ;
		(void) strcpy( hdrfmtStr, "%s    " ) ;

		/* USER */
		tmpStr = aps_max_pfmt( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_USERID, TRUE ) ;
		(void) sprintf( fmtStr, "%s%s", fmtStr, tmpStr ); free( tmpStr ) ;
		(void) sprintf( hdrfmtStr, "%s%s", hdrfmtStr, tmpStr ); free( tmpStr ) ;

		/* NODE */
		tmpStr = aps_max_pfmt( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_NODE, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;
		(void) sprintf( hdrfmtStr, "%s  %s", hdrfmtStr, tmpStr ); free( tmpStr ) ;

		/* PID */
		tmpStr = aps_max_pfmt( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_PROCESS_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;
		(void) sprintf( hdrfmtStr, "%s  %s", hdrfmtStr, tmpStr ); free( tmpStr ) ;

		/* invoking CMD */
		tmpStr = aps_max_pfmt( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_COMMAND_NAME, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;
		(void) sprintf( hdrfmtStr, "%s  %s", hdrfmtStr, tmpStr ); free( tmpStr ) ;

		/* ACTIVITY */
		tmpStr = aps_max_pfmt( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;
		(void) sprintf( hdrfmtStr, "%s  %s", hdrfmtStr, tmpStr ); free( tmpStr ) ;
		
		/* DAR id */
		tmpStr = aps_max_pfmt( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_DARID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;
		(void) sprintf( hdrfmtStr, "%s  %s", hdrfmtStr, tmpStr ); free( tmpStr ) ;

		/*
		-- Ok, DAR id in the header is a string, but it's an integer in the 
		-- actual column, so we have to make an adjustment.
		*/
		index = strlen(hdrfmtStr) - 1 ;
		hdrfmtStr[index] = 's' ;

		/* newline */
		(void) sprintf( fmtStr, "%s%c", fmtStr, NEWLINE ) ;
		(void) sprintf( hdrfmtStr, "%s%c", hdrfmtStr, NEWLINE ) ;

		/* free up unused bytes, should never fail since are giving up bytes */
		fmtStr = realloc( fmtStr, strlen( fmtStr ) ) ;
		hdrfmtStr = realloc( hdrfmtStr, strlen( hdrfmtStr ) ) ;

		/* Calculate the formatted line length */
		fmtLineLen = aps_max_fw( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_USERID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_NODE ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_PROCESS_ID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_COMMAND_NAME ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_DAR_ACTIVITIES, 
					ACTIVE_DAR_ACTIVITIES_DARID ) 
				+ 5 /* 4 blanks at beg of line and newline at the end */ ;
	}

	/*
	--	Make sure there is enough storage space for the message
	*/

	if (str != NULL)
		currMsgLen = strlen( str ) + 1 ;		/* str plus newline */
	currMsgLen += strlen( BLOCK_MSG_INTRO ) + 2	/* 2 newlines */
			+ fmtLineLen /* line of column headers */
			+ NUMELTS( blocked_list ) * fmtLineLen
			+ 1		/* string terminator */ ;

	if (currMsgLen > blockedMsgLen)
	{
		char	*tmpPtr ;

		if ((tmpPtr = realloc( blockedMsg, currMsgLen )) == NULL)
		{
			(void) sprintf( display_string, "Can't alloc enough space for \"permission BLOCKed\" message\n(%s)",
					str ) ;
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone ) ;

			/* NOTE: blockedMsg is unchanged, so storage/size are still valid */
			return ;
		}
		blockedMsg = tmpPtr ;
		blockedMsgLen	= currMsgLen ;
	}

	/*
	--	format the message
	*/

	/* caller's individual message */
	if (str != NULL)
		(void) sprintf( blockedMsg, "%s\n", str ) ;
	else
		blockedMsg[0] = STREND ;

	/* append the standard message and 2 newlines */
	(void) sprintf( blockedMsg, "%s%s\n\n", blockedMsg, BLOCK_MSG_INTRO ) ;

	/* append the columns-hdr line */
	(void) sprintf( blockedMsg, hdrfmtStr, blockedMsg,
			"(user", "node", "pid", "command",
			"activity", "dar id)" ) ;

	/* format each permission in the list, and append this formatted line */
	for (permRec = FIRST( blocked_list, permPtr )
			; permRec != (DB_RECORD **) NULL
			; permRec = (DB_RECORD **) NEXT( blocked_list, permPtr ))
	{
		(void) sprintf( blockedMsg, fmtStr, blockedMsg,
				CAST_ACTIVE_DAR_ACTIVITIES_USERID
					permRec[ACTIVE_DAR_ACTIVITIES_USERID],
                CAST_ACTIVE_DAR_ACTIVITIES_NODE
                    permRec[ACTIVE_DAR_ACTIVITIES_NODE],
				CAST_ACTIVE_DAR_ACTIVITIES_PROCESS_ID
                    permRec[ACTIVE_DAR_ACTIVITIES_PROCESS_ID],
				CAST_ACTIVE_DAR_ACTIVITIES_COMMAND_NAME
					permRec[ACTIVE_DAR_ACTIVITIES_COMMAND_NAME],
				CAST_ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID
					permRec[ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID],
                CAST_ACTIVE_DAR_ACTIVITIES_DARID
                    permRec[ACTIVE_DAR_ACTIVITIES_DARID] ) ;
	}

	/*
	--	Display the message
	*/
	
	popup_message( XmDIALOG_ERROR, "APS:ERROR",
			blockedMsg, XtGrabNone ) ;

	return ;
}


/*==============================================================================
Function:       gui_mu_show_sgl_blocked

Description:    Formats and pops up a "single activity" permission not granted
				message, which includes a list of the permissions blocking
				the requested one.

				First prints the "str", then a newline, then the
				BLOCK_MSG_INTRO and 2 newlines, then a columns-hdr line
				and a formatted list of blocking permissions.  If "str"
				is NULL, just prints the BLOCK_MSG_INTRO, 2 newlines,
				columns-hdr, and the formatted list of blocking permissions.

Parameters:     str				- caller supplied blocked permission message
				blocked_list	- llist of permissions blocking the request

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/11/96

Notes:		
==============================================================================*/
static void
gui_mu_show_sgl_blocked( char *str, llist *blocked_list )
{
	static char	*fmtStr = NULL ;	/* fmt string for list of blocking perm.s */
	static int	fmtLineLen ;	/* length of a blocking perm. formatted line */
	char		*tmpStr ;

	int			currMsgLen = 0 ;	/* length of entire formatted message */

	/* MU permission-blocked "llist" variables */
	DB_RECORD	**permRec ;
	cursor		permPtr ;

	if (!fmtStr)
	{
		/*
		--	make the format string:
		--		userid, node, pid, cmd_name, acty_id
		--	(allocate more than is needed and free up the unused bytes later)
		*/

		if ((fmtStr = malloc( BUFSIZ )) == NULL)
		{
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					"Can't alloc space for the \"permission BLOCKed\" format",
					XtGrabNone ) ;
			return ;
		}

		/* will append each formatted line via sprintf to the end of the
		-- message, so need "%s    " for the initial message portion
		*/
		(void) strcpy( fmtStr, "%s    " ) ;

		/* USER */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_USERID, TRUE ) ;
		(void) sprintf( fmtStr, "%s%s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* NODE */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_NODE, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* PID */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* invoking CMD */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* ACTIVITY */
		tmpStr = aps_max_pfmt( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID, TRUE ) ;
		(void) sprintf( fmtStr, "%s  %s", fmtStr, tmpStr ); free( tmpStr ) ;

		/* newline */
		(void) sprintf( fmtStr, "%s%c", fmtStr, NEWLINE ) ;

		/* free up unused bytes, should never fail since are giving up bytes */
		fmtStr = realloc( fmtStr, strlen( fmtStr ) ) ;

		/* Calculate the formatted line length */
		fmtLineLen = aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_USERID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_NODE ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME ) 
				+ 2	/* # blanks between items */
				+ aps_max_fw( ACTIVE_PLANNING_ACTIVITIES, 
					ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID ) 
				+ 5 /* 4 blanks at beg of line and newline at the end */ ;
	}

	/*
	--	Make sure there is enough storage space for the message
	*/

	if (str != NULL)
		currMsgLen = strlen( str ) + 1 ;		/* str plus newline */
	currMsgLen += strlen( BLOCK_MSG_INTRO ) + 2	/* 2 newlines */
			+ fmtLineLen /* line of column headers */
			+ NUMELTS( blocked_list ) * fmtLineLen
			+ 1		/* string terminator */ ;

	if (currMsgLen > blockedMsgLen)
	{
		char	*tmpPtr ;

		if ((tmpPtr = realloc( blockedMsg, currMsgLen )) == NULL)
		{
			(void) sprintf( display_string, "Can't alloc enough space for \"permission BLOCKed\" message\n(%s)",
					str ) ;
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone ) ;

			/* NOTE: blockedMsg is unchanged, so storage/size are still valid */
			return ;
		}
		blockedMsg = tmpPtr ;
		blockedMsgLen	= currMsgLen ;
	}

	/*
	--	format the message
	*/

	/* caller's individual message */
	if (str != NULL)
		(void) sprintf( blockedMsg, "%s\n", str ) ;
	else
		blockedMsg[0] = STREND ;

	/* append the standard message and 2 newlines */
	(void) sprintf( blockedMsg, "%s%s\n\n", blockedMsg, BLOCK_MSG_INTRO ) ;

	/* append the columns-hdr line */
	(void) sprintf( blockedMsg, fmtStr, blockedMsg,
			"(user", "node", "pid", "command",
			"activity)" ) ;

	/* format each permission in the list, and append this formatted line */
	for (permRec = FIRST( blocked_list, permPtr )
			; permRec != (DB_RECORD **) NULL
			; permRec = (DB_RECORD **) NEXT( blocked_list, permPtr ))
	{
		(void) sprintf( blockedMsg, fmtStr, blockedMsg,
				CAST_ACTIVE_PLANNING_ACTIVITIES_USERID
					permRec[ACTIVE_PLANNING_ACTIVITIES_USERID],
                CAST_ACTIVE_PLANNING_ACTIVITIES_NODE
                    permRec[ACTIVE_PLANNING_ACTIVITIES_NODE],
				CAST_ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID
                    permRec[ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID],
				CAST_ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME
					permRec[ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME],
				CAST_ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID
					permRec[ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID] ) ;
	}

	/*
	--	Display the message
	*/
	
	popup_message( XmDIALOG_ERROR, "APS:ERROR",
			blockedMsg, XtGrabNone ) ;

	return ;
}
