#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   ant_roundup.c

Description:    source file for the antenna down times roundup,

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)ant_roundup.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/ant_roundup/SCCS/s.ant_roundup.c"

#include "ant_roundup.h"     /* for define macros                   */

/*==============================================================================
Function:       ant_roundup_error_message[]

Description:    strings that correspond to ant_roundup error codes.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan  3 16:09:38 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
char    *ant_roundup_error_message[] =
{
    "zero is not an error code",
    "ANT_ROUNDUP_ERROR_SETTING_CON_VALUE",                          /*   -1 */
    "ANT_ROUNDUP_ERROR_MOVING_CONS",                                /*   -2 */
    "ANT_ROUNDUP_ERROR_MOVING_QUES",                                /*   -3 */
    "ANT_ROUNDUP_ERROR_MOVING_SUBS",                                /*   -4 */
    "ANT_ROUNDUP_ERROR_MOVING_PLNS",                                /*   -5 */
    "ANT_ROUNDUP_ERROR_MOVING_SCHS",                                /*   -6 */
    "ANT_ROUNDUP_ERROR_IN_STATION_ID",                              /*   -7 */
    "ANT_ROUNDUP_ERROR_IN_STRTTIME",                                /*   -8 */
    "ANT_ROUNDUP_ERROR_IN_STOPTIME",                                /*   -9 */
    "ANT_ROUNDUP_ERROR_SETTING_QUE_VALUE",                          /*  -10 */
    "ANT_ROUNDUP_ERROR_SETTING_SUB_VALUE",                          /*  -11 */
    "ANT_ROUNDUP_ERROR_SETTING_PLN_VALUE",                          /*  -12 */
    "ANT_ROUNDUP_ERROR_SETTING_SCH_VALUE",                          /*  -13 */
    "ANT_ROUNDUP_ERROR_INITIALIZING_STOICFILE",                     /*  -14 */
    "ANT_ROUNDUP_ERROR_NULL_REPORT_FILE_POINTER",                   /*  -15 */
    "ANT_ROUNDUP_ERROR_SETTING_ZERO_VALUE",                         /*  -16 */
    " -17 unknown error code.   ",                                  /*  -17 */
    " -18 unknown error code.   ",                                  /*  -18 */
    " -19 unknown error code.   ",                                  /*  -19 */
    " -20 unknown error code.   ",                                  /*  -20 */
    " -21 unknown error code.   ",                                  /*  -21 */
    " -22 unknown error code.   ",                                  /*  -22 */
    " -23 unknown error code.   ",                                  /*  -23 */
    " -24 unknown error code.   ",                                  /*  -24 */
    " -25 unknown error code.   ",                                  /*  -25 */
    " -26 unknown error code.   ",                                  /*  -26 */
    " -27 unknown error code.   ",                                  /*  -27 */
    " -28 unknown error code.   ",                                  /*  -28 */
    " -29 unknown error code.   ",                                  /*  -29 */
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
int ant_check_station_antenna( 
    char    *station_id, 
    int     antenna_id ) 
{
    int     return_code ;

    sprintf(where_clause, "where %s = '%s' and %s = %d ",
        APS_COL(ANTENNA, ANTENNA_STATION_ID), station_id, 
        APS_COL(ANTENNA, ANTENNA_ANTENNA_ID), antenna_id ) ;

    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC,
            APS_TABLE(ANTENNA), where_clause ) ;

    if ( return_code == 1 )
        return TRUE ;
    else
        return FALSE ;
}


/*==============================================================================
Function:       ant_roundup()

Description:    routine for the antenna down times roundup, in 
                which, for a station, antenna, and time bracket, 
                all CON dtks within the time bracket and all QUE, SUB, 
                PLN, SCH dtks for the antenna within the time bracket 
                are retrieved and re-submitted to the dtk proposal 
                list processor.  this is done so that the change 
                in the antenna status can have the desired effect 
                on the appropriate data-takes.  


Creator:        Lawrence Stevens

Creation Date:  Tue Jan  2 17:47:32 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

int ant_roundup(
    DBPROCESS   *APS_dbproc,
    char        *station_id,
    char        *strttime,
    char        *stoptime,
    FILE        *report_fp ) 
{
    int         return_code ;
    DBSMALLINT  dbsmallint_0 = 0 ;  /* used to set a value; pointer required. */
                                    /* cannot be done using a literal.        */

    llist       *list_check = NULL ;

    llist       *ant_dtks = NULL ;
    cursor      ant_dtks_ptr ;
    DB_RECORD   **ant_dtk_rec ;

    llist       *sorted_dtks = NULL ;

    llist       *accepted_dtks; /* output list of accepted data-takes       */
    llist       *rejected_dtks; /* output list of rejected data-takes       */
    llist       *CON_dtks;      /* output list of CON data-takes            */
    llist       *deleted_dtks;  /* output list data-takes deleted on request. */
    llist       *error_dtks;    /* output list data-takes with errors.      */
    llist       *omission_dtks; /* db dtks omitted by FA schedule           */
    llist       *other_sat_dtks;/* other satellite db dtks affected         */
    llist       *same_sat_dtks; /* same sat, other db dtks affected         */
    llist       *dtk_updates;   /* complete list of final updates.          */

    /*
    -- check station_id
    */
    if ( ! ant_check_station_antenna( station_id, 1 ) ) 
        return ANT_ROUNDUP_ERROR_IN_STATION_ID ;

    if ( tc_validate_asf_datetime(strttime) < 0 )
        return ANT_ROUNDUP_ERROR_IN_STRTTIME ;
    if ( tc_validate_asf_datetime(stoptime) < 0 )
        return ANT_ROUNDUP_ERROR_IN_STOPTIME ;
    if ( report_fp == NULL )
        return ANT_ROUNDUP_ERROR_NULL_REPORT_FILE_POINTER ;

    /* 
    -- initialize the vector library here, to 
    -- put unwanted output at the start.  
    */
    if ( init_vec_lib() )
        return ANT_ROUNDUP_ERROR_INITIALIZING_STOICFILE ;

    fprintf(report_fp, "\n\n\t\t*\t*\t*\t*\t*\t*\t*\n\n\n");
    fprintf(report_fp, "\t\tANTENNA DOWN TIMES CHANGE POST-PROCESSING.\n");
    fprintf(report_fp, "\n\t\tSTATION:  %s\n", station_id ) ;
    fprintf(report_fp, "\n\t\tTime Bracket:  \n\t\t\t\t%s\n\t\t\t\t%s\n", 
        strttime, stoptime ) ;
    fprintf(report_fp, 
    "\n\n\tThe antenna status of the above station during the time bracket\n") ;
    fprintf(report_fp, 
    "\thas recently changed; data-takes may need to be moved to another\n" ) ;
    fprintf(report_fp, 
    "\tantenna if it has come down, or re-instated if it has come up.\n" ) ;
    fprintf(report_fp, 
    "\tAll CON, QUE, SUB, PLN, and SCH downlink data-takes in the time\n" ) ;
    fprintf(report_fp, 
	"\tbracket will be re-submitted for processing.  The final status for\n") ;
    fprintf(report_fp, 
	"\teach data-take, if accepted, will be the originally proposed\n" ) ;
    fprintf(report_fp, 
	"\tstatus.  The data-takes will be processed in QUE, SUB, PLN, SCH\n" ) ;
    fprintf(report_fp, 
	"\torder. \n\n\n");

#ifdef PRINT_DIAG
    printf("%s(%d):  START:  station_id = %s\n", 
        __FILE__, __LINE__, station_id ) ;
    printf("      strttime = %s, stoptime = %s\n", strttime, stoptime ) ;
#endif

    /* 
    -- get all of the CON, QUE, SUB, PLN, and SCH 
	-- data-takes within the time bracket, for this stationid.  
    -- note the interesting comparison of start with stop times 
    -- to get the overlapping data-takes with CON status.  
	-- NOTE:  later, using an existing utility, we MUST remove 
	-- the non-downlinking data-takes.  
    */
    sprintf( where_clause, 
"where %s < '%s' and %s > '%s' and %s = '%s' and ( %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' ) ",
        APS_COL(DTK, DTK_STRTTIME), stoptime,
        APS_COL(DTK, DTK_STOPTIME), strttime,
        APS_COL(DTK, DTK_STATION_ID), station_id,
        APS_COL(DTK, DTK_DTKSTAT), "CON",
        APS_COL(DTK, DTK_DTKSTAT), "QUE",
        APS_COL(DTK, DTK_DTKSTAT), "SUB",
        APS_COL(DTK, DTK_DTKSTAT), "PLN",
        APS_COL(DTK, DTK_DTKSTAT), "SCH" ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  where_clause = \n%s\n", __FILE__, __LINE__, 
        where_clause ) ;
#endif

    ant_dtks = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if (ant_dtks == NULL)
        return DTKM_ERROR_DB_QUERY_FAILED ;

#ifdef PRINT_DIAG
    printf("%s(%d):  ant_dtks = %d\n", __FILE__, __LINE__, NUMELTS( ant_dtks ));
    dtkm_print_list( stdout, ant_dtks ) ;
#endif

	/*
	-- Filter out recording data-takes.  Remove all of the 
	-- non-downlink data-takes.  
	*/
    for (
        ant_dtk_rec = (DB_RECORD **) FIRST(ant_dtks, ant_dtks_ptr) ;
        ant_dtk_rec ;
        ant_dtk_rec = (DB_RECORD **) NEXT(ant_dtks, ant_dtks_ptr) 
        )
    {
		/* 
		-- if NOT a downlink data-take, remove 
		-- it from the list.  
		*/
		if( dtkm_is_a_downlink(ant_dtk_rec) != TRUE )
			DEL_AT_CURSOR( ant_dtks, ant_dtks_ptr ) ;
	}

#ifdef PRINT_DIAG
    printf("%s(%d):  Non-downlinks REMOVED:  ant_dtks = %d\n", 
		__FILE__, __LINE__, NUMELTS( ant_dtks ));
    dtkm_print_list( stdout, ant_dtks ) ;
#endif

    /*
    -- for each QUE, SUB, PLN, and SCH data-take, copy 
	-- the DTK_DTKSTAT value to DTK_PROPOSED_DTKSTAT to 
	-- save it for use during processing.  This is a 
	-- safety measure.
    */
    for (
        ant_dtk_rec = (DB_RECORD **) FIRST(ant_dtks, ant_dtks_ptr) ;
        ant_dtk_rec ;
        ant_dtk_rec = (DB_RECORD **) NEXT(ant_dtks, ant_dtks_ptr) 
        )
    {
        if ( strcmp( CAST_DTK_DTKSTAT ant_dtk_rec[DTK_DTKSTAT], "QUE" ) == 0 
        ||   strcmp( CAST_DTK_DTKSTAT ant_dtk_rec[DTK_DTKSTAT], "SUB" ) == 0 
        ||   strcmp( CAST_DTK_DTKSTAT ant_dtk_rec[DTK_DTKSTAT], "PLN" ) == 0 
        ||   strcmp( CAST_DTK_DTKSTAT ant_dtk_rec[DTK_DTKSTAT], "SCH" ) == 0 )
            strcpy( CAST_DTK_PROPOSED_DTKSTAT ant_dtk_rec[DTK_PROPOSED_DTKSTAT],
                    CAST_DTK_DTKSTAT ant_dtk_rec[DTK_DTKSTAT] ) ;
    }

    /* 
    -- now set up a sorted list so that it is sorted 
    -- by the dtk.proposed_dtkstat value,  
    -- and that the order is:  QUE, SUB, PLN, SCH.  
    -- this is just to mimmick the usual order of processing.  
    -- SCH and PLN data-takes ignore QUE data-takes, and 
	-- can therefore bump them.  But we don't want a QUE 
	-- data-take to bump a SCH data-take.  Therefore, we 
	-- run the SCH data-takes last and maintain the normal 
	-- planning order. 
    */
    sorted_dtks = create_dyn_llist() ;

    /*
    -- move the QUE data-takes to sorted_dtks:
    */
    list_check = move_db_record_matches2llist(
        APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "QUE", sorted_dtks, ant_dtks ) ;
    if ( list_check != sorted_dtks )
    {
        DEL_LIST( ant_dtks ) ;
        DEL_LIST( sorted_dtks ) ;
        return ANT_ROUNDUP_ERROR_MOVING_QUES ;
    }

    /*
    -- move the SUB data-takes to sorted_dtks:
    */
    list_check = move_db_record_matches2llist(
        APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "SUB", sorted_dtks, ant_dtks ) ;
    if ( list_check != sorted_dtks )
    {
        DEL_LIST( ant_dtks ) ;
        DEL_LIST( sorted_dtks ) ;
        return ANT_ROUNDUP_ERROR_MOVING_SUBS ;
    }

    /*
    -- move the PLN data-takes to sorted_dtks:
    */
    list_check = move_db_record_matches2llist(
        APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "PLN", sorted_dtks, ant_dtks ) ;
    if ( list_check != sorted_dtks )
    {
        DEL_LIST( ant_dtks ) ;
        DEL_LIST( sorted_dtks ) ;
        return ANT_ROUNDUP_ERROR_MOVING_PLNS ;
    }

    /*
    -- move the SCH data-takes to sorted_dtks:
    */
    list_check = move_db_record_matches2llist(
        APS_CDEFS(DTK), DTK_PROPOSED_DTKSTAT, "SCH", sorted_dtks, ant_dtks ) ;
    if ( list_check != sorted_dtks )
    {
        DEL_LIST( ant_dtks ) ;
        DEL_LIST( sorted_dtks ) ;
        return ANT_ROUNDUP_ERROR_MOVING_SCHS ;
    }

    DEL_LIST( ant_dtks ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):  sorted_dtks = %d\n", __FILE__, __LINE__, 
        NUMELTS( sorted_dtks ));
    dtkm_print_list( stdout, sorted_dtks ) ;
#endif

    if ( NUMELTS( sorted_dtks ) <= 0 )
    {
        /* there are no data-takes to process.  */
        DEL_LIST( sorted_dtks ) ;
        fprintf( report_fp, "There are no data-takes that need processing.\n");
        return ANT_ROUNDUP_OK ;
    }

    /*
    -- change all of the dtk.dtkstat values in the 
    -- sorted_dtks list to "CON" to prepare for the 
    -- submission of the list to dtkm_process_dtk_proposal_list()
    -- as a CON roundup.  Every data-take will be re-submitted 
    -- using the status in DTK_PROPOSED_DTKSTAT.  
    */
    list_check = set_db_record_values( APS_CDEFS(DTK), DTK_DTKSTAT, "CON", 
        sorted_dtks ) ;
    if ( list_check != sorted_dtks )
    {
        DEL_LIST( sorted_dtks ) ;
        return ANT_ROUNDUP_ERROR_SETTING_CON_VALUE ;
    }

    /*
    -- change all of the dtk.antenna_id values in the 
    -- sorted_dtks list to 0 so that the antenna preferences 
    -- will determine the desired antenna_id for each proposal.  
    -- Every data-take will be re-submitted using antenna_id = 0.
    -- Note:  DTK_ANTENNA_ID is a DBSMALLINT type storage.  
    */
    list_check = set_db_record_values( APS_CDEFS(DTK), DTK_ANTENNA_ID, 
        &dbsmallint_0, sorted_dtks ) ;
    if ( list_check != sorted_dtks )
    {
        DEL_LIST( sorted_dtks ) ;
        return ANT_ROUNDUP_ERROR_SETTING_ZERO_VALUE ;
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
        "\n\nEnd of Antenna Down Times Change Post-processing.\n");
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

    return ANT_ROUNDUP_OK ;

}
