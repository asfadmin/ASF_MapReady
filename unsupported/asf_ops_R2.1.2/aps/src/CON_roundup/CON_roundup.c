#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		CON_roundup.c

Description:	source file for the antenna down times roundup,

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)CON_roundup.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/CON_roundup/SCCS/s.CON_roundup.c"

#include "CON_roundup.h"     /* for define macros                   */

/*==============================================================================
Function:       CON_roundup_error_message[]

Description:    strings that correspond to CON_roundup error codes.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan  3 16:09:38 PST 1996

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
char    *CON_roundup_error_message[] =
{
	"zero is not an error code",
	"CON_ROUNDUP_ERROR_MOVING_QUES",                                /*   -1 */
	"CON_ROUNDUP_ERROR_MOVING_SUBS",                                /*   -2 */
	"CON_ROUNDUP_ERROR_MOVING_PLNS",                                /*   -3 */
	"CON_ROUNDUP_ERROR_MOVING_SCHS",                                /*   -4 */
	"CON_ROUNDUP_ERROR_IN_STATION_ID",                              /*   -5 */
	"CON_ROUNDUP_ERROR_IN_STRTTIME",                                /*   -6 */
	"CON_ROUNDUP_ERROR_IN_STOPTIME",                                /*   -7 */
	"CON_ROUNDUP_ERROR_INITIALIZING_STOICFILE",                     /*   -8 */
	"CON_ROUNDUP_ERROR_NULL_REPORT_FILE_POINTER",                   /*   -9 */
	" -10 unknown error code.   ",                                  /*  -10 */
	" -11 unknown error code.   ",                                  /*  -11 */
	" -12 unknown error code.   ",                                  /*  -12 */
	" end of list               "
} ;


/*==============================================================================
Function:       ant_check_station_antenna()
 
Description:    checks station_id and antenna_id for existence.
 
Creator:        Lawrence Stevens
 
Creation Date:  Tue Jan  2 18:05:15 PST 1996
 
Notes:
    This routine was created using 4-character tabs.  If you don't have
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.
 
==============================================================================*/
int CON_check_station_id( char    *station_id )
{
    int     return_code ;
 
    sprintf(where_clause, "where %s = '%s' ",
        APS_COL(ANTENNA, ANTENNA_STATION_ID), station_id ) ;
 
    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC,
            APS_TABLE(ANTENNA), where_clause ) ;
 
    if ( return_code >= 1 )
        return TRUE ;
    else
        return FALSE ;
}


/*==============================================================================
Function:       CON_roundup()

Description:	routine for the CON data-takes roundup, in 
				which, for a time bracket, all CON dtks within the 
				time bracket are retrieved and re-submitted to the dtk proposal 
				list processor.  
				This may be done when time has been freed on an antenna
				and maybe some data-takes that were conflicted or bumped 
				can get back onto the schedule or into the plans.  

Creator:        Lawrence Stevens

Creation Date:  Mon Jan  8 15:06:25 PST 1996 

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

int CON_roundup(
	DBPROCESS	*APS_dbproc,
	char		*station_id,
	char		*strttime,
	char		*stoptime,
	FILE		*report_fp ) 
{
	int			return_code ;

	llist		*list_check = NULL ;

	llist		*rounded_up_CON_dtks = NULL ;

	llist		*sorted_dtks = NULL ;

    llist       *accepted_dtks; /* output list of accepted data-takes       */
    llist       *rejected_dtks; /* output list of rejected data-takes       */
    llist       *CON_dtks;      /* output list of CON data-takes            */
    llist       *deleted_dtks;  /* output list data-takes deleted on request. */
    llist       *error_dtks;    /* output list data-takes with errors.      */
    llist       *omission_dtks; /* db dtks omitted by FA schedule           */
    llist       *other_sat_dtks;/* other satellite db dtks affected         */
    llist       *same_sat_dtks; /* same sat, other db dtks affected         */
    llist       *dtk_updates;   /* complete list of final updates.          */

	if ( ! CON_check_station_id( station_id ) )
        return CON_ROUNDUP_ERROR_IN_STATION_ID ;

    if ( tc_validate_asf_datetime(strttime) < 0 )
		return CON_ROUNDUP_ERROR_IN_STRTTIME ;
    if ( tc_validate_asf_datetime(stoptime) < 0 )
		return CON_ROUNDUP_ERROR_IN_STOPTIME ;
	if ( report_fp == NULL )
		return CON_ROUNDUP_ERROR_NULL_REPORT_FILE_POINTER ;

	/* 
	-- initialize the vector library here, to 
	-- put unwanted output at the start.  
	*/
	if ( init_vec_lib() )
		return CON_ROUNDUP_ERROR_INITIALIZING_STOICFILE ;

	fprintf(report_fp, "\n\n\t\t*\t*\t*\t*\t*\t*\t*\n\n\n");
	fprintf(report_fp, "\t\tCON DATA-TAKES PROCESSING.\n");
	fprintf(report_fp, "\n\t\tSTATION:  %s\n",
		station_id ) ;
	fprintf(report_fp, "\n\t\tTime Bracket:  \n\t\t\t\t%s\n\t\t\t\t%s\n\n", 
		strttime, stoptime ) ;

	fprintf(report_fp, 
	"\tAll CON data-takes in the time bracket will be re-submitted \n" ) ;
	fprintf(report_fp, 
	"\tfor processing.  The final status for each data-take, if no\n") ;
	fprintf(report_fp, 
	"\tconflicts, will be the originally proposed status.  The data-takes\n") ;
	fprintf(report_fp, 
	"\twill be processed in QUE, SUB, PLN, SCH order. \n\n");
		 
	/* 
	-- get all of the CON data-takes within 
	-- the time bracket.  
	-- note the comparison of start with stop times 
	-- to get the overlapping data-takes with CON status.  
	*/
	sprintf( where_clause, 
		"where %s = '%s' and %s = '%s' and %s < '%s' and %s > '%s' ",
		APS_COL(DTK, DTK_DTKSTAT), "CON",
		APS_COL(DTK, DTK_STATION_ID), station_id,
		APS_COL(DTK, DTK_STRTTIME), stoptime,
		APS_COL(DTK, DTK_STOPTIME), strttime ) ;

#ifdef DEBUG
	printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, 
		where_clause ) ;
#endif

	rounded_up_CON_dtks = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
		APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
	if ( rounded_up_CON_dtks == NULL )
		return DTKM_ERROR_DB_QUERY_FAILED ;

#ifdef DEBUG
	printf("%s(%d):  rounded_up_CON_dtks = %d\n", __FILE__, __LINE__, 
		NUMELTS( rounded_up_CON_dtks ));
	dtkm_print_list( stdout, rounded_up_CON_dtks ) ;
#endif

	/* 
	-- now set up a sorted list so that it is sorted 
	-- by the dtk.proposed_dtkstat value,  
	-- and that the order is:  QUE, SUB, PLN, SCH.  
	*/
	sorted_dtks = create_dyn_llist() ;

	/*
	-- move the QUE data-takes to sorted_dtks:
	*/
	list_check = move_db_record_matches2llist(
		APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "QUE", sorted_dtks, 
		rounded_up_CON_dtks ) ;
	if ( list_check != sorted_dtks )
	{
		DEL_LIST( rounded_up_CON_dtks ) ;
		DEL_LIST( sorted_dtks ) ;
		return CON_ROUNDUP_ERROR_MOVING_QUES ;
	}

	/*
	-- move the SUB data-takes to sorted_dtks:
	*/
	list_check = move_db_record_matches2llist(
		APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "SUB", sorted_dtks, 
		rounded_up_CON_dtks ) ;
	if ( list_check != sorted_dtks )
	{
		DEL_LIST( rounded_up_CON_dtks ) ;
		DEL_LIST( sorted_dtks ) ;
		return CON_ROUNDUP_ERROR_MOVING_SUBS ;
	}

	/*
	-- move the PLN data-takes to sorted_dtks:
	*/
	list_check = move_db_record_matches2llist(
		APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "PLN", sorted_dtks, 
		rounded_up_CON_dtks ) ;
	if ( list_check != sorted_dtks )
	{
		DEL_LIST( rounded_up_CON_dtks ) ;
		DEL_LIST( sorted_dtks ) ;
		return CON_ROUNDUP_ERROR_MOVING_PLNS ;
	}

	/*
	-- move the SCH data-takes to sorted_dtks:
	*/
	list_check = move_db_record_matches2llist(
		APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "SCH", sorted_dtks, 
		rounded_up_CON_dtks ) ;
	if ( list_check != sorted_dtks )
	{
		DEL_LIST( rounded_up_CON_dtks ) ;
		DEL_LIST( sorted_dtks ) ;
		return CON_ROUNDUP_ERROR_MOVING_SCHS ;
	}

	DEL_LIST( rounded_up_CON_dtks ) ;

#ifdef DEBUG
	printf("%s(%d):  sorted_dtks = %d\n", __FILE__, __LINE__, 
		NUMELTS( sorted_dtks ));
	dtkm_print_list( stdout, sorted_dtks ) ;
#endif

	if ( NUMELTS( sorted_dtks ) <= 0 )
	{
		/* there are no data-takes to process.  */
		DEL_LIST( sorted_dtks ) ;
		fprintf( report_fp, "There are no data-takes that need processing.\n");
		return CON_ROUNDUP_OK ;
	}

	/* 
	-- the sorted_dtks list is now ready.  
	*/

    accepted_dtks = create_dyn_llist() ;
    rejected_dtks = create_dyn_llist() ;
    CON_dtks = create_dyn_llist() ;
    deleted_dtks = create_dyn_llist() ;
    error_dtks = create_dyn_llist() ;
    omission_dtks = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks = create_dyn_llist() ;
    dtk_updates = create_dyn_llist() ;

	return_code = dtkm_process_dtk_proposal_list( APS_dbproc, sorted_dtks,     
		accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks,error_dtks,    
		omission_dtks, other_sat_dtks, same_sat_dtks, dtk_updates, 
		report_fp ) ;

	fprintf(report_fp, 
		"\n\nEnd of CON data-takes processing.\n");
	fprintf(report_fp, "\n\n\t\t*\t*\t*\t*\t*\t*\t*\n\n\n");

    DEL_LIST( sorted_dtks ) ;
    DEL_LIST( accepted_dtks ) ;
    DEL_LIST( rejected_dtks ) ;
    DEL_LIST( CON_dtks ) ;
    DEL_LIST( deleted_dtks ) ;
    DEL_LIST( error_dtks ) ;
    DEL_LIST( omission_dtks ) ;
    DEL_LIST( other_sat_dtks ) ;
    DEL_LIST( same_sat_dtks ) ;
    DEL_LIST( dtk_updates ) ;

	if ( return_code < 0 )
		return return_code ;

	return CON_ROUNDUP_OK ;

}
