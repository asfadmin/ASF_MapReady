#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		cb_conrdnup.c

Description:	contains the callbacks for the CON roundup window

External Functions Defined:
				cb_clearConRoundupForm
				cb_startConRoundup
	
File Scope Functions:
				conRoundupDoneFunc
				free_con_roundup_args_struct
	
External Variables Defined:
	
File Scope Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)cb_conrndup.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_conrndup.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <Xm/Text.h>
#include <Xm/TextF.h>

#define CONTEXT_MACRO_ACCESS 1
#include "vc_conrndup.h"
#undef CONTEXT_MACRO_ACCESS
#include "UxXt.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_exe_names.h"
#include "aps_extern.h"
#include "aps_db_table.h"
#include "db_dtk.h"

#include "gui_utils.h"
#include "subprocess.h"
#include "cb_datetime.h"

/******************************************************************************
**	Macros/Constants
******************************************************************************/

#define STN_ID_OPT			"-s"		/* cmd line option for station id */
#define START_TIME_OPT		"-b"		/* cmd line option for start time */
#define STOP_TIME_OPT		"-e"		/* cmd line option for stop time */

#define BEGIN_CONTEXT(widget) \
    _UxCConRoundupForm          *UxSaveCtx; \
    UxSaveCtx = UxConRoundupFormContext; \
    UxConRoundupFormContext = \
            (_UxCConRoundupForm *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
	} \
	UxConRoundupFormContext = UxSaveCtx;

/******************************************************************************
**	Global Functions
******************************************************************************/
extern void		popup_message() ;

/******************************************************************************
**	Global Variables
******************************************************************************/

typedef struct CON_ROUNDUP
{
	char *stationid ;
	char *starttime ;
	char *stoptime ;
} CON_ROUNDUP ;			/* struct of con roundup cmd option values */

extern char		display_string[] ;


/*==============================================================================
Function:       free_con_roundup_args_struct

Description:    Frees the memory allocated to a con roundup structure

Parameters:     con_roundup_args - the con roundup structure

Returns:        N/A

Creator:        Teresa McKillop

Creation Date:  02/09/96

Notes:			con_roundup_args still points into heap memory, on return.
==============================================================================*/
static void
free_con_roundup_args_struct( CON_ROUNDUP *con_roundup_args )
{
	free( con_roundup_args->stationid ) ;
	free( con_roundup_args->starttime ) ;
	free( con_roundup_args->stoptime ) ;

	free( con_roundup_args ) ;

	return;
}


/*==============================================================================
Function:       conRoundupDoneFunc

Description:    Popups a status message when the process has completed

Parameters:     process - the process struct from the subprocess.
				NULL	- not used, necessary for generic subprocess control

Returns:        

Creator:        Teresa McKillop

Creation Date:  02/09/96

Notes:		
==============================================================================*/
static void
conRoundupDoneFunc( PROCESS_INFO *process, CON_ROUNDUP *con_roundup_args )
{
	switch (process->exit_status)
	{
	case APS_EXIT_OK :
		(void) sprintf( display_string,
				"DTK CON roundup:\n\n Completed Successfully for\n\n \
STN:   %s\n START: %s\n STOP:  %s",
				con_roundup_args->stationid,
				con_roundup_args->starttime, con_roundup_args->stoptime );
		popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
				display_string, XtGrabNone );
		break ;
	case APS_EXIT_ERROR :
		(void) sprintf( display_string,
				"DTK CON roundup:\n\n UNSUCCESSFUL Run\n\n \
see %s log file for:\n STN:   %s\n START: %s\n STOP:  %s",
				CON_ROUNDUP_CMD, con_roundup_args->stationid,
				con_roundup_args->starttime, con_roundup_args->stoptime );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		break ;
	case APSGUI_EXIT_COREDUMP : /* process core dumped */
		(void) sprintf( display_string,
				"DTK CON roundup:\n\n Signal Caught CORE DUMPED\n\n \
see %s log file for:\n STN:   %s\n START: %s\n STOP:  %s",
				CON_ROUNDUP_CMD, con_roundup_args->stationid,
				con_roundup_args->starttime, con_roundup_args->stoptime );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		break ;
	default :	/* process caught a signal, but no core dump */
		(void) sprintf( display_string,
				"DTK CON roundup:\n\n SIGNAL Caught (signal = %d)\n\n \
see %s log file for:\n STN:   %s\n START: %s\n STOP:  %s",
				-(process->exit_status),
				CON_ROUNDUP_CMD, con_roundup_args->stationid,
				con_roundup_args->starttime, con_roundup_args->stoptime );
		popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		break ;
	}

	free_con_roundup_args_struct( con_roundup_args ) ;

	return;
}


/*==============================================================================
Function:       cb_clearConRoundupForm

Description:    Clears out/blanks the CON roundup form

Parameters:     N/A

Returns:        

Creator:        Ron Green

Creation Date:  03/15/1995

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_clearConRoundupForm( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

	XmTextFieldSetString( TF_CON_RND_STRTTIME, EMPTY_STR ) ;
	XmTextFieldSetString( TF_CON_RND_STOPTIME, EMPTY_STR ) ;

END_CONTEXT

	return;
}


/*==============================================================================
Function:       cb_startConRoundup

Description:    validates the inputs from the CON roundup screen and
				executes the CON roundup process.

Parameters:     Standard X Callback parameters

Returns:        N/A

Creator:        Teresa McKillop

Creation Date:  02/08/96

Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_startConRoundup( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )

	CON_ROUNDUP		*con_roundup_args;
	char			*stationid;
	char			*starttime;
	char			*stoptime;

	PROCESS_INFO	*conRoundupProcess;

	/* display the "hourglass" to indicate: wait */
	TimeoutCursors( True, True , NULL) ;

	/* get the screen's values */
	stationid	= gui_optionMenu_string( optionMenu_conRnd_stnid ) ;
	starttime	= gui_TF_string( TF_CON_RND_STRTTIME ) ;
	stoptime	= gui_TF_string( TF_CON_RND_STOPTIME ) ;

	/*
	-- validate the times
	*/

	/* make sure both time values have been entered */
	if (starttime[0] == STREND || stoptime[0] == STREND)
	{
		(void) sprintf(display_string, "Enter missing start/stop time") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		XtFree( starttime ) ; XtFree( stoptime ) ; XtFree( stationid ) ;
		TimeoutCursors( False, False , NULL) ;	/* turn off the "hourglass" */
		return ;
	}

	/* trigger the "activate" callback to validate each time */
	XtCallActionProc(TF_CON_RND_STRTTIME, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
	{
		XtFree( starttime ) ; XtFree( stoptime ) ; XtFree( stationid ) ;
		TimeoutCursors( False, False , NULL) ;	/* turn off the "hourglass" */
		return ;
	}
	XtCallActionProc(TF_CON_RND_STOPTIME, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
	{
		XtFree( starttime ) ; XtFree( stoptime ) ; XtFree( stationid ) ;
		TimeoutCursors( False, False , NULL) ;	/* turn off the "hourglass" */
		return ;
	}

	/* verify that the stop time comes AFTER the start time */
	if (time_range_error(starttime, stoptime))
	{
		XtFree( starttime ) ; XtFree( stoptime ) ; XtFree( stationid ) ;
		TimeoutCursors( False, False , NULL) ;	/* turn off the "hourglass" */
		return ;
	}

	/*
	-- now execute the CON roundup (Yee Haw!!!)
	*/

	/* set up the args structure for "done" processing */
	con_roundup_args = (CON_ROUNDUP *) malloc( sizeof (CON_ROUNDUP) );
	con_roundup_args->stationid = (char *) malloc(
		APS_SIZE( DTK, DTK_STATION_ID ) );
	(void) strcpy( con_roundup_args->stationid, stationid ) ;
	con_roundup_args->starttime = (char *) malloc( ASF_TIME_STR_LENGTH + 1 );
	(void) strcpy( con_roundup_args->starttime, starttime ) ;
	con_roundup_args->stoptime = (char *) malloc( ASF_TIME_STR_LENGTH + 1 );
	(void) strcpy( con_roundup_args->stoptime, stoptime ) ;

	(void) sprintf( display_string, "%s -U %s -P %s %s %s %s %s %s %s",
		CON_ROUNDUP_CMD, userid, password,
		STN_ID_OPT, stationid,
		START_TIME_OPT, starttime,
		STOP_TIME_OPT, stoptime );

	XtFree( starttime ) ; XtFree( stoptime ) ; XtFree( stationid ) ;

	conRoundupProcess = create_process( display_string, NULL, TRUE, NULL,
		NULL, NULL, conRoundupDoneFunc, con_roundup_args );
	if (conRoundupProcess == NULL)
	{
		/* error msg already popped up */
		free_con_roundup_args_struct( con_roundup_args );
		TimeoutCursors( False, False , NULL) ;	/* turn off the "hourglass" */
		return;
	}

	if (start_process( conRoundupProcess ))
	{
		/* can't get pipes, error msg already popped up */
		destroy_process( conRoundupProcess );
		free_con_roundup_args_struct( con_roundup_args );
	}

	TimeoutCursors( False, False , NULL) ;	/* turn off the "hourglass" */

END_CONTEXT

	return;
}
