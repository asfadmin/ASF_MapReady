#undef PRINT_DIAG_LIST_BEFORE_SORT
#undef PRINT_DIAG
#undef PRINT_DIAG_EXTRA

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_process_dtk_proposal_list.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_process_dtk_proposal_list.c	5.2 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_process_dtk_proposal_list.c"


#include "dtkm.h"            

/* FOR DATABASE TABLES        */
#include "db_dtk.h"            /* for dtk table             */
#include "db_sat_inclusion_period.h"

#include <string.h>     /* for strcmp strcpy        */



/*==============================================================================
Function:       dtkm_remove_deleted_dtks()

Description:    Look for DEL dtks in dtk_updates.  If any are found 
                there, then delete them from omission_dtks if they 
                are found there, too.  
                We don't want to REJ any data-takes that have already 
                been DELeted earlier in the run!  

Creator:        Lawrence Stevens

Creation Date:  Thu Apr 17 21:08:01 PDT 1997

Notes:          
==============================================================================*/
int dtkm_remove_deleted_dtks( 
    llist   *dtk_updates,    /* read list of updates to find deleted dtks.   */
    llist   *omission_dtks ) /* if any are found, then remove them from here */
{

    int     return_code ;
    llist   *llist_check ; 
    llist   *DEL_dtk_list = NULL ;

    /* 
    -- first, find all the dtks with a DEL status 
    -- in the dtk_updates list.  Copy them to the DEL_list:
    */
    DEL_dtk_list = create_dyn_llist() ;
    llist_check = copy_db_record_matches2llist( APS_CDEFS(DTK), 
        DTK_DTKSTAT, "DEL", 
        DEL_dtk_list, dtk_updates ) ;
    if( llist_check != DEL_dtk_list )
        return DTKM_ERROR_COPYING_DTK_RECORDS_TO_LLIST ;

#ifdef PRINT_DIAG
    printf("%s(%d):  elements in DEL_dtk_list: %d\n", __FILE__, __LINE__, 
        NUMELTS( DEL_dtk_list ) );
    return_code = dtkm_print_list(stdout, DEL_dtk_list ) ;
#endif

    return_code = dtkm_remove_dtks_from_list( DEL_dtk_list, omission_dtks ) ;

    DEL_LIST( DEL_dtk_list ) ;

    return return_code ;

}

/*==============================================================================
Function:       dtkm_remove_realtime_obs_overlaps()

Description:    remove time-overlapping, same-rev Realtime 
                Observations from the remove_dtks list

Creator:        Lawrence Stevens

Creation Date:  Fri Apr  4 16:38:04 PST 1997

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
static int dtkm_remove_realtime_obs_overlaps( 
    DB_RECORD   **dtk_proposal, 
    llist       *remove_dtks )
{

    cursor      dtk_list_ptr ;
    llist       *same_rev_list ; 
    llist       *llist_check ; 
    DB_RECORD   **dtk_rec ;

    /*  
    -- duplicate same-rev dtks to same_rev_list.  This call 
    -- allocates new storage for the duplicated data-takes:  
    -- Why?  we need to delete records from one list, 
    -- and it is tricky when we are looping through that 
    -- same list.  (try it yourself, but be sure you debug it very 
    --              carefully with a variety of test cases.  )  
    -- But now, we will loop through same_rev_list 
    -- and delete from remove_dtks, which will not be tricky.  
    -- Also, the rev test is the first filter.  
    */
    same_rev_list = create_dyn_llist() ;
    llist_check = copy_db_record_matches2llist( APS_CDEFS(DTK), DTK_REV, 
        dtk_proposal[DTK_REV], same_rev_list, remove_dtks ) ;
    if( llist_check != same_rev_list )
    {
        DEL_LIST( same_rev_list ) ;
        return DTKM_ERROR_COPYING_DTK_RECORDS_TO_LLIST ;
    }
    if( NUMELTS( same_rev_list ) <= 0 )
    {
        DEL_LIST( same_rev_list ) ;
        return TRUE ;
    }

    /* there is at least 1 data-take to check.  */
    for (   dtk_rec = (DB_RECORD **) FIRST(same_rev_list, dtk_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(same_rev_list, dtk_list_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        if( dtkm_is_a_realtime_observation( dtk_rec ) != TRUE )
            continue ;

        /* 
        -- this data-take is a realtime observation.  
        -- check if it time-overlaps the input dtk:  
        -- Although the comparisons are between start and stop, 
        -- they will determine if there is a time overlap.  
        -- it is assumed that starttime is < stoptime, as this 
        -- is always checked when a data-take enters the APS 
        */
        if( strcmp( CAST_DTK_STRTTIME      dtk_rec[DTK_STRTTIME], 
                    CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) < 0 
        &&  strcmp( CAST_DTK_STOPTIME      dtk_rec[DTK_STOPTIME], 
                    CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) > 0 )
        {
            /* 
            -- this is a time overlapping, same-rev realtime 
            -- observation.  It is in the input remove_dtks list.  
            -- Now remove dtk_rec from that list:  
            */
            dtkm_delete_dtk_from_list( dtk_rec, remove_dtks ) ;
        }
    }

    /* clean up storage.  */
    DEL_LIST( same_rev_list ) ;
    return TRUE ;

}

static void print_design(FILE *report_fp)
{
    char        design[] = "     -    " ;  /* printed 8 times at start
                                              and end of the report.    */
    int         j ;

    (void) fprintf( report_fp, "\n" ) ;
    for (j = 0 ; j < 8 ; j ++ )
        (void) fprintf( report_fp, "%s", design ) ; 
    (void) fprintf( report_fp, "\n" ) ;

}

/*==============================================================================
Function:       dtkm_process_dtk_proposal_list

Description:  processes a list of data-takes in the form of a 
linked list of DB_RECORD's.  The processing output includes a report 
in a file and a list of updated and rejected data-takes.  

OMISSIONS:
The processing also includes "rejection by schedule omission" for 
SCH and PLN single-satellite lists.  
This means the rejection of PLN and SCH data-takes not connected and 
confirmed by any data-takes in the input data-take list.  
This is rejection by schedule omission; the data-take is rejected because 
it is omitted from the schedule.  Any db data-take never appearing in any 
dtk_concurs list or any dtk_similars list pertaining to any input data-take 
is rejected.  
IMPORTANT NOTE:  The reason this is done for dtk lists with 
a single satellite all with status = SCH or PLN is the certainty that the list 
comes from a flight agency schedule, such as the ESA SHAQ or the NASDA OPLN.  
These files contain EVERY data-take between the earliest one in the file 
and the latest one in the file.  Any data-take in the database without a 
corresponding, confirming record in the schedule must be removed (REJ) from 
the schedule.  
If it is not on the schedule, it must be rejected.  Note that there are 
no other conditions that justify "rejection by schedule omission".  

Rejection by schedule omission is done only for flight agency files 
that are expected to contain all activities in a time bracket.  

See also dtkm_process_dtk_proposal(), which processes a single data-take 
proposal and is called by this routine for each dtk in the input list.  

The expected status of each proposed dtk is: 

REP, which indicates an FA reply (it will have a PLN status when processed), 
REQ, which indicates an FA request (it will have a PLN status when processed), 
PLN, which indicates a data-take from an FA file if satellite plans
SCH, which indicates a data-take from an FA schedule.  
QUE, which indicates a not-yet-requested data-take from the planner.

Returns:        
    int
    >= 0 : The number of rejected data-takes

    < 0 ERROR:  

Creator:        Lawrence Stevens

Creation Date:  Wed Nov 15 18:20:37 PST 1995

Notes:      
use create_dyn_list() to initialize the lists in the arguments.  


==============================================================================*/
int dtkm_process_dtk_proposal_list(
    DBPROCESS   *APS_dbproc,    /* Sybase db process                        */
    llist       *input_dtks,    /* list of input data-take records          */

    /* 
    -- the following 5 output dtk lists tell what happened 
    -- to input data-take proposals.  
    */
    llist       *accepted_dtks, /* accepted data-takes               */
    llist       *rejected_dtks, /* data-takes rejected by APS        */
            /*
            -- data-takes rejected by APS occur because there 
            -- is a conflict with same-satellite equipment.  
            -- for this reason these data-take proposals are 
            -- rejected by the APS and not placed into the 
            -- database.  They can never take place and are rejected
            */

    llist       *CON_dtks,      /* CON data-takes                    */
            /*
            -- CON data-takes could take place except for only 
            -- ONE PROBLEM:  there is/are other satellite(s) 
            -- that are on the antenna(s) and they have higher 
            -- priority.  CON data-takes go into the database 
            -- with a CON status.  
            -- Later, maybe one or more of the other satellite 
            -- data-takes could get cancelled.  If this happens, 
            -- a CON data-take could then go onto an antenna.  
            -- and be put into the schedule.  
            */
    llist       *deleted_dtks,  /* data-takes DEL or REJ on request. */
            /*
            -- This list contains data-takes rejected because 
            -- they were input data-take proposals with a status 
            -- of REJ or DEL status.  
            -- They are essentially asking this program to 
            -- please delete or change the dtk db record.  
            */

    llist       *error_dtks,    /* data-takes with errors.           */

    /* 
    -- the following 3 output dtk lists tell what happened to 
    -- data-takes already in the database, not input 
    -- data-take proposals. 
    */
    llist       *omission_dtks, /* db dtks omitted by FA schedule           */
                                /* if = DTKM_DO_NOT_REJ_BY_OMISSION, then   */
                                /* rejection by omission is disabled.       */

    llist       *other_sat_dtks,/* other satellite db dtks affected         */
    llist       *same_sat_dtks, /* same sat, other db dtks affected         */

    llist       *dtk_updates,   /* final version of every db dtk changed.   */
    FILE        *report_fp )    /* pointer for output report file.          */
{

    int         return_code ;
    char        buf_80[80] ;
    int         dtk_darid_update_code ;

    /* lists to use when processing a single data-take proposal.  */
    llist       *dtk_sat_down_times = NULL ;
    llist       *antenna_down_times = NULL ;
    llist       *dtk_conflicts = NULL ;  
    llist       *dtk_similars = NULL ;
    llist       *dtk_concurs = NULL ;
    llist       *single_dtk_dtk_updates = NULL ;

    cursor      input_dtks_ptr ;
    DB_RECORD   **proposed_dtk ;
    DB_RECORD   **result_dtk ;
    DB_RECORD   **parent_dtk ;  /* this is a downlink dtk for an observation */

    cursor      temp_fetch_list_ptr ;
    DB_RECORD   **temp_fetch_rec = NULL ; /* a temporary data-take  */
    llist       *temp_fetch_list = NULL ; /* a temporary list       */

    llist       *list_check = NULL ;   

    cursor      omission_dtks_ptr ;
    int         omissions_flag = FALSE ; 
                /* = TRUE activates rejection by schedule omission */ 
    int         ESA_Special_Checkoff_flag = FALSE ; 
                /* = TRUE activates ESA Special Check-off during 
                          rejection by schedule omission.          */ 

    llist       *temp_dtks_list = NULL ;

    char        omissions_sat[4] = "   " ; 
    int         different_sat_count  = 0 ;
    char        omissions_station_id[3] = "   " ;
    int         different_station_id_count  = 0 ;
    int         non_SCH_count  = 0 ;
    int         non_PLN_count  = 0 ;
    int         non_CON_count  = 0 ;
    int         ESA_sat_count = 0 ;
    int         ROB_count = 0 ;
    int         min_rev = MAXINT ;   /* HIGH init value for min rev.  */
    int         max_rev = - MAXINT ;  /* there is no MININT           */
    int         record_number = 0 ;
    int         omitted_dtk_number = 0 ;
    int         inclusion_seconds ;

    /* error checking  */

    /*
    -- check input dtk list  
    */

    if ( input_dtks == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( input_dtks ) <= 0 )
        return DTKM_ERROR_INPUT_DTK_LIST_EMPTY ;

    if ( accepted_dtks == NULL )
        return DTKM_ERROR_OUTPUT_ACCEPTED_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( accepted_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_ACCEPTED_DTK_LIST_IS_NOT_EMPTY ;

    if ( rejected_dtks == NULL )
        return DTKM_ERROR_OUTPUT_REJECTED_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( rejected_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_REJECTED_DTK_LIST_IS_NOT_EMPTY ;

    if ( CON_dtks == NULL )
        return DTKM_ERROR_OUTPUT_CON_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( CON_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_CON_DTK_LIST_IS_NOT_EMPTY ;

    if ( deleted_dtks == NULL )
        return DTKM_ERROR_OUTPUT_DELETED_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( deleted_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_DELETED_DTK_LIST_IS_NOT_EMPTY ;

    if ( error_dtks == NULL )
        return DTKM_ERROR_OUTPUT_ERROR_DTK_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( error_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_ERROR_DTK_LIST_IS_NOT_EMPTY ;

    if ( omission_dtks == NULL )
        return DTKM_ERROR_OUTPUT_OMISSION_DTK_LIST_NOT_INITIALIZED ;
    if( omission_dtks != DTKM_DO_NOT_REJ_BY_OMISSION )
    {
        /* this list is active.  check it:  */
        if ( NUMELTS( omission_dtks ) != 0 )
            return DTKM_ERROR_OUTPUT_OMISSION_DTK_LIST_IS_NOT_EMPTY ;
    }

    if ( other_sat_dtks == NULL )
        return DTKM_ERROR_OUTPUT_OTHER_SAT_DTKS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( other_sat_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_OTHER_SAT_DTKS_LIST_IS_NOT_EMPTY ;

    if ( same_sat_dtks == NULL )
        return DTKM_ERROR_OUTPUT_SAME_SAT_DTKS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( same_sat_dtks ) != 0 )
        return DTKM_ERROR_OUTPUT_SAME_SAT_DTKS_LIST_IS_NOT_EMPTY ;

    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_updates ) != 0 )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY ;


    /**********************************************************************/
    /*                                                                    */
    /*           RE-ORDER THE INPUT DATA-TAKE LIST.                       */
    /*                                                                    */
    /*           OBSERVATIONS FIRST, THEN DOWNLINKS.                      */
    /*           LASTLY, DATA-TAKES THAT ARE NEITHER (ERRORS)             */
    /*                                                                    */
    /**********************************************************************/

#ifdef PRINT_DIAG_LIST_BEFORE_SORT
    if ( report_fp )
    {
        print_design(report_fp ) ;
        (void) fprintf( report_fp,
            "\nORIGINAL, NON-SORTED INPUT LIST OF DATA-TAKES TO PROCESS:  %d\n",
            NUMELTS( input_dtks ) ) ;
        dtkm_print_list( report_fp, input_dtks ) ;
        (void) fprintf( report_fp, "END OF INPUT LIST\n\n" ) ;
    }
#endif

    /*
    -- the input to the function has been validated.  Now re-order the input 
    -- list of data-takes.  (Tue Mar 11 15:21:50 PST 1997)
    -- we want to put all of the downlinks at the end of the 
    -- list of data-takes and the observations first.  
    -- This will help the processing of downlinks - their observations 
    -- will already be in the database when processing takes place.  
    -- 
    -- We will first move the Real-time downlinks out of the 
    -- list and them move all of them back into the list, which 
    -- places them at the end.  Then we will do the tape dumps.  
    -- 
    */

    /*
    -- create the temp_fetch_list.
    */
    temp_fetch_list = create_dyn_llist() ;

    /* 
    -- move all of the realtime downlinks into 
    -- temp_fetch_list.  
    */
    list_check = move_db_record_matches2llist( 
        APS_CDEFS(DTK), DTK_SENSOR, DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
        temp_fetch_list, input_dtks ) ;
    if ( list_check != temp_fetch_list )
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
    /* 
    -- move all of the realtime downlinks back into 
    -- input_dtks, and the end.  
    */
    list_check = move_db_record_matches2llist( 
        APS_CDEFS(DTK), DTK_SENSOR, DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
        input_dtks, temp_fetch_list ) ;
    if ( list_check != input_dtks )
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;

    /* 
    -- move all of the tape dump downlinks into 
    -- temp_fetch_list.  
    */
    list_check = move_db_record_matches2llist( 
        APS_CDEFS(DTK), DTK_SENSOR, DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
        temp_fetch_list, input_dtks ) ;
    if ( list_check != temp_fetch_list )
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
    /* 
    -- move all of the tape dump downlinks back into 
    -- input_dtks, at the end of the list.  
    */
    list_check = move_db_record_matches2llist( 
        APS_CDEFS(DTK), DTK_SENSOR, DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
        input_dtks, temp_fetch_list ) ;
    if ( list_check != input_dtks )
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;

    if ( report_fp )
    {
        print_design(report_fp ) ;
        (void) fprintf( report_fp,
            "\nSORTED INPUT LIST OF DATA-TAKES TO PROCESS: %d\n",
            NUMELTS( input_dtks ) ) ;
        (void) fprintf( report_fp, 
            "    (observations first, then downlinks)\n" ) ;  
        dtkm_print_list( report_fp, input_dtks ) ;
        (void) fprintf( report_fp, "END OF INPUT LIST\n\n" ) ;
    }

    /*
    -- Now delete the temp_fetch_list to clean up.  
    */
    DEL_LIST( temp_fetch_list ) ;

    /* 
    -- determine whether or not we will perform the 
    -- "rejection by schedule omission"
    -- procedure.  this is done only if each of the input data-takes are 
    -- SCH, or if each of them is PLN.   
    -- AND IF ALL of the data-takes are of the same satellite.  
    --
    -- For ESA SHAQP:  
    -- And something else:  In the case of a SHAQ file, which is 
    -- composed of only E1 SCH realtime downlinks or of only E2 SCH realtime 
    -- downlinks, we don't want any overlapping Realtime Observation dtks 
    -- in the database to be rejected.  This is because this particular 
    -- file, the ESA SHAQ file, contains only downlinks.  So, in this 
    -- next loop in which we analyse the data-takes in the list, we will 
    -- determine if we have a SHAQ file here.  If so, we will alter the 
    -- method of rejection by schedule omission:  when a realtime downlink 
    -- is processed later, any overlapping realtime observations will 
    -- ALSO be removed (checked off) from the omissions list.  So that 
    -- at the end of processing, these RealTime Observations in the 
    -- database will not be rejected.  
    -- However, RealTime Observations will still be rejected if not 
    -- confirmed by any overlapping Real Time Downlink in the SHAQ file.  
    */

    /**********************************************************************/
    /*                                                                    */
    /*           START OF FIRST LOOP ON THE INPUT DATA-TAKES.             */
    /*                                                                    */
    /*           CHECK FOR NON_SCH AND NON_PLN DATA-TAKES                 */
    /*           DETERMINE MIN AND MAX REV NUMBER IN INPUT LIST           */
    /*                                                                    */
    /**********************************************************************/


    /* initialize the loop parameters */
    proposed_dtk = NULL ;
    non_SCH_count  = 0 ;
    non_PLN_count  = 0 ;
    different_sat_count  = 0 ;
    different_station_id_count  = 0 ;
    ESA_sat_count = 0 ;
    ROB_count = 0 ;

    /* set up start of loop.  */
    proposed_dtk = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr) ;
    /* set up the satellite value to compare with.  */
    (void) strcpy(omissions_sat, CAST_DTK_SAT proposed_dtk[DTK_SAT]) ;
    (void) strcpy(omissions_station_id, 
        CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID]) ;
    /* 
    -- check every data-take to see if this is a CON roundup or a 
    -- FA schedule file.  
    */
    for (
        proposed_dtk = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr) ;
        proposed_dtk ;
        proposed_dtk = (DB_RECORD **) NEXT(input_dtks, input_dtks_ptr) 
        )
    {
        /*
        -- check the "proposed_dtk" from the input list.  
        */

        /* check for an ESA-sat; an ESA satellite in the group:  */
        if ( strncmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "E", 1 ) == 0 )
            ESA_sat_count ++ ;

        /* check for a Realtime Observation    */
        if( dtkm_is_a_realtime_observation( proposed_dtk ) == TRUE )
            ROB_count++ ;

        /* check for different-sat; a different satellite in the group:  */
        if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], omissions_sat ) != 0 )
            different_sat_count ++ ;

        /* check for different-station_id; a different station in the group:  */
        if ( strcmp( CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID], 
            omissions_station_id ) != 0 )
            different_station_id_count ++ ;

        /* check for non-SCH status:  */
        if ( strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "SCH" ) != 0 )
            non_SCH_count ++ ;

        /* check for non-PLN status:  */
        if ( strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "PLN" ) != 0 )
            non_PLN_count ++ ;

        /* check for non-CON status:  */
        if ( strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "CON" ) != 0 )
            non_CON_count ++ ;

        /* 
        -- only if a downlink, check for min and max rev:  
        */
        if ( dtkm_is_a_downlink( proposed_dtk ) == TRUE )
        {
            max_rev = MAX( max_rev, CAST_DTK_REV proposed_dtk[DTK_REV] ) ;
            min_rev = MIN( min_rev, CAST_DTK_REV proposed_dtk[DTK_REV] ) ;
        }
    }

    /*
    -- note:  using the value of DTKM_DO_NOT_REJ_BY_OMISSION
    --        in the parameter omission_dtks is an indication 
    --        not to reject anything by omission.  
    */
    if ( ( non_SCH_count == 0 || non_PLN_count == 0 )
    &&   different_sat_count == 0 
    &&   different_station_id_count == 0  
    &&   omission_dtks != DTKM_DO_NOT_REJ_BY_OMISSION   )  
    {
        /* 
        -- the whole file is SCH or the whole file is PLN. 
        -- AND the file has only one satellite, AND only one 
        -- station_id  
        */

        /* 
        -- This list is therefore presumed to be a Flight 
        -- Agency Schedule or a file of Flight Agency Plans.  
        */

        /*  
        -- Now activiate the rejections by 
        -- schedule omission function.  
        */
        omissions_flag = TRUE ;

#ifdef PRINT_DIAG
        printf("%s(%d):  rejection by plan/schedule omission is activated.\n",
            __FILE__, __LINE__ );
        printf(
            "     sat = %s,  station_id = %s    [min, max rev]: [%d, %d]\n",
            omissions_sat, omissions_station_id, min_rev, max_rev   ) ;
#endif

        /*
        -- check to see if this is an ESA file AND 
        -- if there are no Realtime Observations in the 
        -- file.  If so, we activate ESA_Special_Checkoff, in which 
        -- overlapping realtime observations are checked off 
        -- when processing a realtime downlink.  
        */
        if( ESA_sat_count != 0  && ROB_count == 0 )
        {
            ESA_Special_Checkoff_flag = TRUE ;
#ifdef PRINT_DIAG
            printf("%s(%d):  ESA_Special_Checkoff is activated.\n",
                __FILE__, __LINE__ );
            printf( "Overlapping Realtime observations are checked off\n" ) ;
            printf( "processing a realtime downlink.  \n" ) ;
#endif
        }
    }
    else
    {
        /* 
        -- check for CON roundup processing:  
        */
        /* 
        -- Note:  the antenna roundup function appears 
        --        here as a CON roundup, so that it 
        --        can never be confused with a FA schedule.
        */

        if ( non_CON_count == 0  && NUMELTS( input_dtks ) > 0 )
        {
#ifdef PRINT_DIAG
            printf( "%s(%d):  CON roundup processing is activated.\n",
                    __FILE__, __LINE__ );
#endif
            /* 
            -- a CON roundup list.  
            -- these are CON data-takes that want to be re-instated.
            -- re-set their dtkstat status from their
            -- dtk.proposed_dtkstat field.  
            */
            for (
                proposed_dtk = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr);
                proposed_dtk ;
                proposed_dtk = (DB_RECORD **) NEXT(input_dtks, input_dtks_ptr) 
                )
            {
                (void) strcpy( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], 
                    CAST_DTK_PROPOSED_DTKSTAT 
                        proposed_dtk[DTK_PROPOSED_DTKSTAT] ) ;
            }
        }
    }

    if ( omissions_flag )
    {
        /*  will do the rejections by schedule omission function.  */

        /****************************************************************/
        /*                                                              */
        /*     CREATE DTK LIST FROM DATABASE TO DETECT REJECTIONS       */
        /*     BY OMISSION.                                             */
        /*                                                              */
        /****************************************************************/

        /* 
        -- retrieve with same satellite, within rev bracket, 
        -- and SCH or PLN status.  
        -- retrieve ONLY downlink data-takes to reject.  
        */
        (void) sprintf(where_clause, 
"where %s = '%s' and ( %s = '%s' or %s = '%s' ) and %s >= %d and %s <= %d and %s = '%s' and ( %s = '%s' or %s = '%s' ) ",
            APS_COL(DTK, DTK_SAT), omissions_sat, 
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
            APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
            APS_COL(DTK, DTK_REV), min_rev,
            APS_COL(DTK, DTK_REV), max_rev,
            APS_COL(DTK, DTK_STATION_ID), omissions_station_id, 
            APS_COL(DTK, DTK_DTKSTAT), "SCH",
            APS_COL(DTK, DTK_DTKSTAT), "PLN") ;

        (void) sprintf(orderby_cols, "%s, %s",
            APS_COL(DTK, DTK_REV),
            APS_COL(DTK, DTK_STRTTIME) ) ;

#ifdef PRINT_DIAG
        printf( "%s(%d):  rejection by schedule omission where_clause:\n%s\n",
            __FILE__, __LINE__, where_clause );
#endif

        /* retrieve data-takes that might be rejected by schedule omission. */
        temp_dtks_list = db_get_records(APS_dbproc, APS_TABLE(DTK),
            where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

        /* 
        -- move all of the records to the parameter omission_dtk 
        -- list.  this maintains the consistent use of the lists 
        -- in the parameters of this subroutine.  
        */
        list_check = db_record_llist_move( omission_dtks, temp_dtks_list ) ;
        if ( list_check != omission_dtks )
            return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;

        DEL_LIST( temp_dtks_list ) ;

#ifdef PRINT_DIAG
        printf("%s(%d): no. of relevant omission db dtks retrieved: %d\n",
            __FILE__, __LINE__, NUMELTS( omission_dtks ) ) ;
        return_code = dtkm_print_list(stdout, omission_dtks ) ;
#endif

    }


    /* set up storage to store the result data-take.  */
    result_dtk = new_table_record(APS_CDEFS(DTK)) ;
    /* 
    -- if the proposal is an observation dtk, then its 
    -- parent data-take, in this routine, is the downlink 
    -- data-take that downlinks it.  
    -- this rec must be freed later.  
    */
    parent_dtk = new_table_record(APS_CDEFS(DTK)) ;

    /**********************************************************************/
    /*                                                                    */
    /*           START OF LOOP ON THE INPUT DATA-TAKES.                   */
    /*                                                                    */
    /**********************************************************************/
    for (
        proposed_dtk = (DB_RECORD **) FIRST(input_dtks, input_dtks_ptr) ;
        proposed_dtk ;
        proposed_dtk = (DB_RECORD **) NEXT(input_dtks, input_dtks_ptr)
        )
    {

        /*
        -- the input dtk record was retrieved; now processing 
        -- the "proposed_dtk" from the input list.  
        */

        /* set up lists and storage to process the data-take.  */

        dtk_sat_down_times  = create_dyn_llist() ;
        antenna_down_times  = create_dyn_llist() ;
        dtk_conflicts  = create_dyn_llist() ;
        dtk_similars = create_dyn_llist() ;
        dtk_concurs  = create_dyn_llist() ;
        single_dtk_dtk_updates  = create_dyn_llist() ;

        /* print the current data-take  */
        if ( report_fp )
        {
            (void) fprintf( report_fp, "Processing data-take   #%3d:  \n",
                ++record_number ) ;
            dtkm_print( report_fp, proposed_dtk ) ;
        }


        /*
        -- Update a zero-value "proposed_dtk" darid value 
        -- with information from the database.
        -- If the darid is updated, make a note of it later, in the report.
        -- 
        -- This is not a redundant step, even thought the darid value may
        -- also be re-populated later.  This step allows us to have a 
        -- darid value in the case where the "proposed_dtk" does not go through
        -- the dtkm_combine_dtks() routine.
        */
        dtk_darid_update_code = 
            dtkm_update_dtk_darid(proposed_dtk) ;
        if (dtk_darid_update_code < 0)
        {
            free_db_record( parent_dtk ) ;
            DEL_LIST ( dtk_conflicts ) ;
            DEL_LIST ( dtk_similars ) ;
            DEL_LIST ( dtk_concurs ) ;
            DEL_LIST ( single_dtk_dtk_updates ) ;
            DEL_LIST( dtk_sat_down_times ) ;
            DEL_LIST( antenna_down_times ) ;
            return (dtk_darid_update_code) ;
        }
            

        /* now process this data-take from the list.  */
        return_code = dtkm_process_dtk_proposal(APS_dbproc, 
            proposed_dtk, result_dtk, parent_dtk,
            dtk_sat_down_times, antenna_down_times, dtk_concurs, dtk_similars, 
            dtk_conflicts, single_dtk_dtk_updates ) ;
        if ( return_code < 0 ) /* serious error.  */
        {
            /* Special handling of this one error code:  */
            if (return_code == DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD)
            {
                /* 
                -- special case, per FA request: 
                -- show the inclusion period length 
                */
                if ( report_fp )
                {
                    (void) sprintf(where_clause,
                        "where %s = '%s'", 
                        APS_COL(SAT_INCLUSION_PERIOD,SAT_INCLUSION_PERIOD_SAT), 
                        CAST_DTK_SAT result_dtk[DTK_SAT]);

                    temp_fetch_list = db_get_records(
                        DB_SYBINT_USE_APS_READER_DBPROC, 
                        APS_TABLE(SAT_INCLUSION_PERIOD), where_clause, NULL, 
                        APS_CDEFS(SAT_INCLUSION_PERIOD), ALL_COLS) ;

                    if (temp_fetch_list == NULL)
                    {
                        free_db_record( parent_dtk ) ;
                        DEL_LIST ( dtk_conflicts ) ;
                        DEL_LIST ( dtk_similars ) ;
                        DEL_LIST ( dtk_concurs ) ;
                        DEL_LIST ( single_dtk_dtk_updates ) ;
                        DEL_LIST( dtk_sat_down_times ) ;
                        DEL_LIST( antenna_down_times ) ;
                        return_code =
                                DTKM_ERROR_NO_INCLUSION_PERIOD_FOR_SATELLITE ;
                    }
                 
                    /* check for no records.    */
                    temp_fetch_rec = (DB_RECORD **) 
                        FIRST(temp_fetch_list, temp_fetch_list_ptr) ;
                    if (temp_fetch_rec == NULL)
                    {
                        free_db_record( parent_dtk ) ;
                        DEL_LIST ( dtk_conflicts ) ;
                        DEL_LIST ( dtk_similars ) ;
                        DEL_LIST ( dtk_concurs ) ;
                        DEL_LIST ( single_dtk_dtk_updates ) ;
                        DEL_LIST( dtk_sat_down_times ) ;
                        DEL_LIST( antenna_down_times ) ;
                        return_code =
                                DTKM_ERROR_NO_INCLUSION_PERIOD_FOR_SATELLITE ;
                    }

                    inclusion_seconds =  
                        CAST_SAT_INCLUSION_PERIOD_LENGTH 
                        temp_fetch_rec[SAT_INCLUSION_PERIOD_LENGTH] ;
                }
            /* return_code may have changed values, herein */
            }

            if ( report_fp )
            {
                if (return_code ==
                        DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD)
                {

                    (void) fprintf(report_fp, 
"ERROR, datatake shorter than %d secs. See sat_inclusion_period table.\n",
                        inclusion_seconds);
                }
                else
                {
                    (void) fprintf(report_fp, 
                        "ERROR:  %s\n", DTKM_ERROR_MESSAGE(return_code) ) ;
                }
            }

            /* 
            -- add data-take to ERROR list 
            */
            list_check = dtkm_duplicate_dtk_into_list( proposed_dtk, 
                error_dtks ) ;
            if ( list_check != error_dtks )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST ( dtk_conflicts ) ;
                DEL_LIST ( dtk_similars ) ;
                DEL_LIST ( dtk_concurs ) ;
                DEL_LIST ( single_dtk_dtk_updates ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
            }
            /*
            -- complete processing this data-take; continue with 
            -- the rejections by schedule omission, if indicated.  
            */
        }
        else
        {
#ifdef PRINT_DIAG_EXTRA
            printf("%s(%d) ------------------------------------------\n",
                __FILE__, __LINE__ ) ;
            printf("dtkm_process_dtk_proposal() return_code = %d\n", 
                return_code ) ;

            printf("result_dtk:\n" ) ;
            dtkm_print(stdout, result_dtk ) ;

            printf("dtk_sat_down_times:\n" ) ;
            dtkm_print_list(stdout, dtk_sat_down_times ) ;

            printf("antenna_down_times:\n" ) ;
            dtkm_print_antenna_down_times_list( stdout, antenna_down_times ) ;

            printf("dtk_concurs:\n" ) ;
            dtkm_print_list(stdout, dtk_concurs ) ;

            printf("dtk_similars:\n" ) ;
            dtkm_print_list(stdout, dtk_similars ) ;

            printf("dtk_conflicts:\n" ) ;
            dtkm_print_list(stdout, dtk_conflicts ) ;

            printf("single_dtk_dtk_updates:\n" ) ;
            dtkm_print_list(stdout, single_dtk_dtk_updates ) ;

            printf("%s(%d) ------------------------------------------\n",
                __FILE__, __LINE__ ) ;
#endif
            /* 
            --  OK.  normal return code.  
            */
            switch ( return_code )
            {

            case DTKM_DTK_ACCEPTED_INHERITANCE :
                if ( report_fp )
                {
                    /* 
                    -- the proposal is an observation dtk.  
                    -- its parent data-take, in this routine, is 
                    -- the downlink data-take that downlinks it.  
                    */
                    (void) fprintf(report_fp, 
"ACCEPTED, BUT INHERITED %s STATUS FROM CORRESPONDING DOWNLINK %s/%s/%ld.%02d\n"
                        ,CAST_DTK_DTKSTAT parent_dtk[DTK_DTKSTAT]
                        ,CAST_DTK_SAT     parent_dtk[DTK_SAT]
                        ,CAST_DTK_SENSOR  parent_dtk[DTK_SENSOR]
                        ,CAST_DTK_REV     parent_dtk[DTK_REV]
                        ,CAST_DTK_DTKID   parent_dtk[DTK_DTKID]) ;
                }
                /* 
                -- add the RESULT data-take to the accepted list 
                */
                list_check = dtkm_duplicate_dtk_into_list( result_dtk, 
                    accepted_dtks ) ;
                if ( list_check != accepted_dtks )
                {
                    free_db_record( parent_dtk ) ;
                    DEL_LIST ( dtk_conflicts ) ;
                    DEL_LIST ( dtk_similars ) ;
                    DEL_LIST ( dtk_concurs ) ;
                    DEL_LIST ( single_dtk_dtk_updates ) ;
                    DEL_LIST( dtk_sat_down_times ) ;
                    DEL_LIST( antenna_down_times ) ;
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                }

                break ;
            case DTKM_DTK_ACCEPTED :
                if ( report_fp )
                {
                    /* print the result data-take  */
                    (void) fprintf(report_fp, "ACCEPTED \n") ;
                }
                /* 
                -- add the RESULT data-take to the accepted list 
                */
                list_check = dtkm_duplicate_dtk_into_list( result_dtk, 
                    accepted_dtks ) ;
                if ( list_check != accepted_dtks )
                {
                    free_db_record( parent_dtk ) ;
                    DEL_LIST ( dtk_conflicts ) ;
                    DEL_LIST ( dtk_similars ) ;
                    DEL_LIST ( dtk_concurs ) ;
                    DEL_LIST ( single_dtk_dtk_updates ) ;
                    DEL_LIST( dtk_sat_down_times ) ;
                    DEL_LIST( antenna_down_times ) ;
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                }

                break ;

            case DTKM_DTK_PROPOSAL_REJ :
                if ( report_fp )
                {
                    (void) fprintf(report_fp, 
"REJECTED due to same-satellite conflicts or satellite equipment down:\n");
                    dtkm_print_list( report_fp, dtk_sat_down_times );
                    dtkm_print_list( report_fp, dtk_conflicts );
                    (void) fprintf(report_fp, "NO CHANGE to database.\n");
                }

                /* add to rejected list */
                list_check = dtkm_duplicate_dtk_into_list( proposed_dtk, 
                    rejected_dtks ) ;
                if ( list_check != rejected_dtks )
                {
                    free_db_record( parent_dtk ) ;
                    DEL_LIST ( dtk_conflicts ) ;
                    DEL_LIST ( dtk_similars ) ;
                    DEL_LIST ( dtk_concurs ) ;
                    DEL_LIST ( single_dtk_dtk_updates ) ;
                    DEL_LIST( dtk_sat_down_times ) ;
                    DEL_LIST( antenna_down_times ) ;
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                }

                break ;

            case DTKM_DTK_PROPOSAL_CONFLICT :
                if ( report_fp )
                {
                    (void) fprintf(report_fp, 
"CONFLICT due to antenna conflicts or antennas down:\n");
                    dtkm_print_list( report_fp, dtk_conflicts ) ;
                    dtkm_print_antenna_down_times_list( stdout, 
                        antenna_down_times ) ;
                }

                /* 
                -- add the RESULT data-take to the CON_dtks list 
                */
                list_check = dtkm_duplicate_dtk_into_list( result_dtk, 
                    CON_dtks ) ;
                if ( list_check != CON_dtks )
                {
                    free_db_record( parent_dtk ) ;
                    DEL_LIST ( dtk_conflicts ) ;
                    DEL_LIST ( dtk_similars ) ;
                    DEL_LIST ( dtk_concurs ) ;
                    DEL_LIST ( single_dtk_dtk_updates ) ;
                    DEL_LIST( dtk_sat_down_times ) ;
                    DEL_LIST( antenna_down_times ) ;
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                }

                break ;

            case DTKM_DTK_DELETED_OK :
                if ( report_fp )
                {
                    (void) fprintf(report_fp,
                        "REJECTED/DELETED as requested.\n");
                }
                /* 
                -- add the RESULT data-take to the deleted_dtks list 
                */
                list_check = dtkm_duplicate_dtk_into_list( result_dtk, 
                    deleted_dtks ) ;
                if ( list_check != deleted_dtks )
                {
                    free_db_record( parent_dtk ) ;
                    DEL_LIST ( dtk_conflicts ) ;
                    DEL_LIST ( dtk_similars ) ;
                    DEL_LIST ( dtk_concurs ) ;
                    DEL_LIST ( single_dtk_dtk_updates ) ;
                    DEL_LIST( dtk_sat_down_times ) ;
                    DEL_LIST( antenna_down_times ) ;
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                }

                break ;

            default :
                if ( report_fp )
                {
                    (void) fprintf(report_fp, 
"ERROR:  unknown return code from the function dtkm_process_dtk_proposal.\n");
                    (void) fprintf(report_fp, 
"Programmers must check recent modifications to functions \n" ) ;
                    (void) fprintf(report_fp, 
"dtkm_process_dtk_proposal and dtkm_process_dtk_proposal_list.\n");
                    (void) fprintf(report_fp,
                        "Skipping to the next dtk record.\n");
                }
                /* 
                -- add data-take to ERROR list 
                */
                list_check = dtkm_duplicate_dtk_into_list( proposed_dtk, 
                    error_dtks ) ;
                if ( list_check != error_dtks )
                {
                    free_db_record( parent_dtk ) ;
                    DEL_LIST ( dtk_conflicts ) ;
                    DEL_LIST ( dtk_similars ) ;
                    DEL_LIST ( dtk_concurs ) ;
                    DEL_LIST ( single_dtk_dtk_updates ) ;
                    DEL_LIST( dtk_sat_down_times ) ;
                    DEL_LIST( antenna_down_times ) ;
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                }
                break ;

            } /* end of switch ( return_code ) ... */

            /* 
            -- normal ( > 0 ) return code (continued 1).  
            */
            if ( report_fp )
            {
                /* print changed db dtks.  */
                if ( NUMELTS ( single_dtk_dtk_updates ) > 0 )
                {
                    (void) fprintf(report_fp, 
                        "Data-take(s) inserted or updated:\n");
                    if ( strcmp ( CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR],
                                  CAST_DTK_SENSOR result_dtk[DTK_SENSOR] ) != 0)
                    {
                        (void) fprintf(report_fp,
                            "DTK proposal sensor was updated\n");
                    }
                
                    if (dtk_darid_update_code == TRUE)
                    {
                        (void) fprintf(report_fp,
                            "DTK proposal darid  was updated\n");
                    }
                    dtkm_print_list( report_fp, single_dtk_dtk_updates );
                }
            }
            /* 
            -- normal ( > 0 ) return code (continued 2).  
            */

            /*
            -- look for REJ, CON, or DEL data-takes in the 
            -- single_dtk_dtk_updates list.  If any of these 
            -- data-takes are in the accepted_dtks list, 
            -- they should be removed now.  Data-takes like 
            -- that were accepted earlier, but got REJ 
            -- just now.  
            */
            return_code = dtkm_reduce_accepted_dtks(
                accepted_dtks, single_dtk_dtk_updates ) ;

            /*
            -- accumulate the other_sat_dtks list and the 
            -- same_sat_dtks.  
            -- other_sat_dtks holds changed db dtks for other satellites.
            -- same_sat_dtks holds db dtks for the same satellite that were 
            -- affected, in addition to the dtk_proposal.  
            */
            return_code = dtkm_extract_other_sat_dtks( result_dtk, 
                single_dtk_dtk_updates, other_sat_dtks ) ;
            if ( return_code < 0 )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST ( dtk_conflicts ) ;
                DEL_LIST ( dtk_similars ) ;
                DEL_LIST ( dtk_concurs ) ;
                DEL_LIST ( single_dtk_dtk_updates ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                return return_code ; 
            }

            return_code = dtkm_extract_same_sat_dtks( result_dtk, 
                single_dtk_dtk_updates, same_sat_dtks ) ;
            if ( return_code < 0 )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST ( dtk_conflicts ) ;
                DEL_LIST ( dtk_similars ) ;
                DEL_LIST ( dtk_concurs ) ;
                DEL_LIST ( single_dtk_dtk_updates ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                return return_code ; 
            }

            /* 
            -- normal ( > 0 ) return code (continued 3).  
            */
            /*
            -- put the single_dtk_dtk_updates for this dtk record
            -- into the master (dtk_updates) list.  
            -- note:  it is possible that a data-take was earlier 
            -- accepted, and now bumped by the current data-take.  
            -- for this reason, we remove from the dtk_updates 
            -- list any data-takes updated just now.  then 
            -- we add the updated data-takes to the list.  
            -- in this way, we never have a data-take mentioned 
            -- twice in dtk_updates when we return.  
            */
            return_code = dtkm_remove_dtks_from_list( single_dtk_dtk_updates,
                dtk_updates ) ;
            if ( return_code < 0 )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST ( dtk_conflicts ) ;
                DEL_LIST ( dtk_similars ) ;
                DEL_LIST ( dtk_concurs ) ;
                DEL_LIST ( single_dtk_dtk_updates ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                return return_code ;
            }
            list_check = db_record_llist_move( dtk_updates, 
                single_dtk_dtk_updates ) ;
            if ( list_check != dtk_updates ) 
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST ( dtk_conflicts ) ;
                DEL_LIST ( dtk_similars ) ;
                DEL_LIST ( dtk_concurs ) ;
                DEL_LIST ( single_dtk_dtk_updates ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
            }
            /* 
            -- normal ( > 0 ) return code (completed).  
            */

        }  /* end of else for if (return_code...   */

        /* all dtks processing comes here.  */

        if ( omissions_flag )
        {
            /*********************************************************/
            /*                                                       */
            /*   REMOVE DTKS FROM OMISSIONS LIST IF IN CONCURS LIST  */
            /*                                                       */
            /*********************************************************/

            if ( NUMELTS( dtk_concurs ) > 0 )
            {

#ifdef PRINT_DIAG
                printf("dtkm_process_list:  elements in dtk_concurs: %d\n",
                    NUMELTS( dtk_concurs ) );
                return_code = dtkm_print_list(stdout, dtk_concurs ) ;
#endif
                return_code = dtkm_remove_dtks_from_list( 
                    dtk_concurs, omission_dtks ) ;
            }

            /*********************************************************/
            /*                                                       */
            /*   REMOVE DTKS FROM OMISSIONS LIST IF IN SIMILARS LIST */
            /*                                                       */
            /*********************************************************/

            if ( NUMELTS( dtk_similars ) > 0 )
            {
#ifdef PRINT_DIAG
                printf("dtkm_process_list:  elements in dtk_similars: %d\n",
                    NUMELTS( dtk_similars ) );
                return_code = dtkm_print_list(stdout, dtk_similars ) ;
#endif
                return_code = dtkm_remove_dtks_from_list( 
                    dtk_similars, omission_dtks ) ;
            }
            /*********************************************************/
            /*                                                       */
            /*          ESA SPECIAL CHECK-OFF                        */
            /*                                                       */
            /*********************************************************/
            if( ESA_Special_Checkoff_flag )
            {
                /* 
                -- compare with dtk proposal (result_dtk) and 
                -- remove time-overlapping, same-rev Realtime 
                -- Observations from the omission_dtks list:  
                */
                return_code = dtkm_remove_realtime_obs_overlaps( 
                    result_dtk, omission_dtks ) ;
            }
        }

        /* stick in a blank line before the next one.  */
        if ( report_fp )
            (void) fprintf(report_fp, "\n");

        /* free all of the lists created for this last record:  */
        DEL_LIST ( dtk_conflicts ) ;
        DEL_LIST ( dtk_similars ) ;
        DEL_LIST ( dtk_concurs ) ;
        DEL_LIST ( single_dtk_dtk_updates ) ;
        DEL_LIST( dtk_sat_down_times ) ;
        DEL_LIST( antenna_down_times ) ;

    } 
    /**********************************************************************/
    /*                                                                    */
    /*             END OF LOOP ON THE INPUT DATA-TAKES.                   */
    /*                                                                    */
    /**********************************************************************/

    /*
    -- post processing of lists.  
    */
    if ( omissions_flag )
    {

        /*********************************************************/
        /*                                                       */
        /*   REMOVE DTKS FROM OMISSIONS LIST IF ALREADY DELETED  */
        /*                                                       */
        /*********************************************************/
        /*
        -- We don't want to REJ any data-takes that have 
        -- already been DELeted earlier in the run!  
        */
        return_code = dtkm_remove_deleted_dtks( dtk_updates, omission_dtks ) ;
        if( return_code < 0 )
        {
            free_db_record( parent_dtk ) ;
            return return_code ;
        }

        if ( report_fp )
        {
            if( NUMELTS(omission_dtks) > 0 )
            {
                (void) fprintf(report_fp, 
"\nDATABASE DATA-TAKES TO BE REJECTED BECAUSE THEY WEREN'T IN THE FA FILE:%3d\n",
                    NUMELTS(omission_dtks) ) ;
            }
            else
            {
                (void) fprintf(report_fp, 
"\nEach database data-take was confirmed by the schedule.\n");
            }
        }

        /*     
        -- For each dtk in omissions list, 
        -- update DTK_DTKSTAT field to "REJ"
        -- insert FA-CANCELLED into DTK_NOTES field which indicates 
        -- that this dtk was cancelled by a Flight Agency.  
        -- use dtkm_process_dtk_proposal()
        -- this will allow other, deserving 
        -- satellite data-takes to 
        -- take the place of the rejected data-take.  
        */
        for (proposed_dtk = (DB_RECORD **) FIRST(omission_dtks, 
                                                 omission_dtks_ptr);
             proposed_dtk ;
             proposed_dtk = (DB_RECORD **) NEXT(omission_dtks, 
                                                 omission_dtks_ptr)  
            )
        {
            /*
            -- the omitted dtk record was retrieved; now process 
            -- it as a "proposed_dtk", with a REJ status, by 
            -- changing the status:
            */
            strcpy( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "REJ" ) ;

            /* put in note, for use by APS statistics:  */
            strcpy( buf_80,  "FA-CANCELLED" ) ;
            strcat( buf_80,  CAST_DTK_NOTES proposed_dtk[DTK_NOTES] ) ;
            /* terminate string if too long.  max len = 40 */
            buf_80[40] = '\0' ;
            strcpy( CAST_DTK_NOTES proposed_dtk[DTK_NOTES], buf_80 ) ;

            /* set up lists and storage to process this data-take.  */

            dtk_sat_down_times  = create_dyn_llist() ;
            antenna_down_times  = create_dyn_llist() ;
            dtk_conflicts  = create_dyn_llist() ;
            dtk_similars = create_dyn_llist() ;
            dtk_concurs  = create_dyn_llist() ;
            single_dtk_dtk_updates  = create_dyn_llist() ;

            /* print the current data-take  */
            if ( report_fp )
            {
                (void) fprintf( report_fp, "\nProcessing rejection  #%3d:  \n",
                    ++omitted_dtk_number ) ;
                dtkm_print( report_fp, proposed_dtk ) ;
            }

            /* now process this omitted data-take.  */
            return_code = dtkm_process_dtk_proposal(APS_dbproc, 
                proposed_dtk, result_dtk, parent_dtk,
                dtk_sat_down_times, antenna_down_times, dtk_concurs, 
                dtk_similars, dtk_conflicts, single_dtk_dtk_updates ) ;
            if ( return_code < 0 ) /* serious error.  */
            {
                if ( report_fp )
                    (void) fprintf(report_fp, 
                        "ERROR:  %s\n", DTKM_ERROR_MESSAGE(return_code) ) ;
                /* 
                -- add data-take to ERROR list 
                */
                list_check = dtkm_duplicate_dtk_into_list( proposed_dtk, 
                    error_dtks ) ;
                if ( list_check != error_dtks )
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;

                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                DEL_LIST( dtk_conflicts ) ;
                DEL_LIST( dtk_similars ) ;
                DEL_LIST( dtk_concurs ) ;
                DEL_LIST( single_dtk_dtk_updates ) ;

                /*
                -- completed processing this data-take; continue with next.
                */
                continue ;
            }

            /* 
            --  OK.  normal, not error return code < 0.  
            */
            if( return_code != DTKM_DTK_DELETED_OK )
            {
                if ( report_fp )
                {
                    (void) fprintf(report_fp, 
"%s(%d):  ERROR:  unexpected return code from dtkm_process_dtk_proposal().\n",
                        __FILE__, __LINE__ );
                    (void) fprintf(report_fp, 
"Programmers must check recent modifications to functions \n" ) ;
                    (void) fprintf(report_fp, 
"dtkm_process_dtk_proposal() and dtkm_process_dtk_proposal_list().\n");
                    (void) fprintf(report_fp,
                        "Skipping to the next dtk record.\n");
                }
                /* 
                -- add data-take to ERROR list 
                */
                list_check = dtkm_duplicate_dtk_into_list( proposed_dtk, 
                    error_dtks ) ;
                if ( list_check != error_dtks )
                    return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                /* skip to next record:  */
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                DEL_LIST( dtk_conflicts ) ;
                DEL_LIST( dtk_similars ) ;
                DEL_LIST( dtk_concurs ) ;
                DEL_LIST( single_dtk_dtk_updates ) ;
                continue ;
            }

            /* return_code == DTKM_DTK_DELETED_OK   */
            if ( report_fp )
            {
                (void) fprintf(report_fp,
                "REJECTED \n");
            }
            /* 
            -- do not add the RESULT data-take to the deleted_dtks list 
            -- the omissions list takes care of all these REJ data-takes.  
            */
            if ( report_fp )
            {
                /* print changed db dtks.  */
                if ( NUMELTS ( single_dtk_dtk_updates ) > 0 )
                {
                    (void) fprintf(report_fp, 
                        "Data-take(s) inserted or updated:\n");
                    dtkm_print_list( report_fp, single_dtk_dtk_updates );
                }
            }
            /* 
            -- normal ( > 0 ) return code for REJ (continued 2).  
            */
            /*
            -- accumulate the other_sat_dtks list and the 
            -- same_sat_dtks.  
            -- other_sat_dtks holds changed db dtks for other satellites.
            -- same_sat_dtks holds db dtks for the same satellite that were 
            -- affected, in addition to the dtk_proposal.  
            */
            return_code = dtkm_extract_other_sat_dtks( result_dtk, 
                single_dtk_dtk_updates, other_sat_dtks ) ;
            if ( return_code < 0 )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                DEL_LIST( dtk_conflicts ) ;
                DEL_LIST( dtk_similars ) ;
                DEL_LIST( dtk_concurs ) ;
                DEL_LIST( single_dtk_dtk_updates ) ;
                return return_code ; 
            }

            return_code = dtkm_extract_same_sat_dtks( result_dtk, 
                single_dtk_dtk_updates, same_sat_dtks ) ;
            if ( return_code < 0 )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                DEL_LIST( dtk_conflicts ) ;
                DEL_LIST( dtk_similars ) ;
                DEL_LIST( dtk_concurs ) ;
                DEL_LIST( single_dtk_dtk_updates ) ;
                return return_code ; 
            }

            /* 
            -- normal ( > 0 ) return code (continued 3).  
            */
            /*
            -- put the single_dtk_dtk_updates for this dtk record
            -- into the master (dtk_updates) list.  
            -- note:  it is possible that a data-take was earlier 
            -- accepted, and now bumped by the current data-take.  
            -- for this reason, we remove from the dtk_updates 
            -- list any data-takes updated just now.  then 
            -- we add the updated data-takes to the list.  
            -- in this way, we never have a data-take mentioned 
            -- twice in dtk_updates when we return.  
            */
            return_code = dtkm_remove_dtks_from_list( single_dtk_dtk_updates,
                dtk_updates ) ;
            if ( return_code < 0 )
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                DEL_LIST( dtk_conflicts ) ;
                DEL_LIST( dtk_similars ) ;
                DEL_LIST( dtk_concurs ) ;
                DEL_LIST( single_dtk_dtk_updates ) ;
                return return_code ;
            }
            list_check = db_record_llist_move( dtk_updates, 
                single_dtk_dtk_updates ) ;
            if ( list_check != dtk_updates ) 
            {
                free_db_record( parent_dtk ) ;
                DEL_LIST( dtk_sat_down_times ) ;
                DEL_LIST( antenna_down_times ) ;
                DEL_LIST( dtk_conflicts ) ;
                DEL_LIST( dtk_similars ) ;
                DEL_LIST( dtk_concurs ) ;
                DEL_LIST( single_dtk_dtk_updates ) ;
                return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
            }

            /* 
            -- normal ( > 0 ) return code (completed).  
            */
            DEL_LIST( dtk_sat_down_times ) ;
            DEL_LIST( antenna_down_times ) ;
            DEL_LIST( dtk_conflicts ) ;
            DEL_LIST( dtk_similars ) ;
            DEL_LIST( dtk_concurs ) ;
            DEL_LIST( single_dtk_dtk_updates ) ;
        } /* end of for loop on omission_dtks  */
    } /* end if omissions flag.  for rejection by schedule omission  */

    /* clean up; free the memory for result_dtk */
    free_db_record (result_dtk) ;
    free_db_record( parent_dtk ) ;

    if ( report_fp ) 
    {
        (void) fprintf(report_fp, "\nSUMMARY OF ACTIVITY\n") ;
        (void) fprintf(report_fp,   "===================\n") ;
        (void) fprintf(report_fp,
            "TOTAL INPUT RECORDS: %d\n", NUMELTS(input_dtks) ) ;
        (void) fprintf(report_fp, "ACCEPTED DATA-TAKE RECORDS: %d\n",
            NUMELTS( accepted_dtks ) ) ;
        (void) fprintf(report_fp, "DATA-TAKE RECORDS REJECTED BY APS: %d\n",
            NUMELTS( rejected_dtks ) ) ;
        (void) fprintf(report_fp, "DATA-TAKE RECORDS WITH CONFLICTS: %d\n",
            NUMELTS( CON_dtks ) ) ;
        (void) fprintf(report_fp, 
            "DATA-TAKE RECORDS REJECTED/DELETED AS REQUESTED: %d\n",
            NUMELTS( deleted_dtks ) ) ;
        (void) fprintf(report_fp, "ERROR DATA-TAKE RECORDS: %d\n",
            NUMELTS( error_dtks ) ) ;

        (void) fprintf(report_fp, "\nOTHER CHANGES\n") ;
        (void) fprintf(report_fp,   "-------------\n") ;
        (void) fprintf(report_fp,
            "OTHER-SATELLITE DATA-TAKE RECORDS CHANGED: %d\n",
            NUMELTS( other_sat_dtks ) ) ;
        (void) fprintf(report_fp, 
            "SAME SATELLITE OTHER DATA-TAKE RECORDS CHANGED: %d\n",
            NUMELTS( same_sat_dtks ) ) ;
        if ( omissions_flag )
            (void) fprintf(report_fp, 
"\nDATABASE RECORDS REJECTED BECAUSE THEY WEREN'T IN THE FA FILE: %d\n",
                NUMELTS( omission_dtks ) ) ;

        (void) fprintf(report_fp, "\n\nDATA-TAKES LISTED BY RESULT\n") ;
        (void) fprintf(report_fp,     "=================================\n") ;
        (void) fprintf(report_fp, "\nACCEPTED DATA-TAKE RECORDS: %d\n",
            NUMELTS( accepted_dtks ) ) ;
        dtkm_print_list( report_fp, accepted_dtks ) ;
        (void) fprintf(report_fp, "\nDATA-TAKE RECORDS REJECTED BY APS: %d\n",
            NUMELTS( rejected_dtks ) ) ;
        dtkm_print_list( report_fp, rejected_dtks ) ;
        (void) fprintf(report_fp, "\nDATA-TAKE RECORDS WITH CONFLICTS: %d\n",
            NUMELTS( CON_dtks ) ) ;
        dtkm_print_list( report_fp, CON_dtks ) ;
        (void) fprintf(report_fp, 
            "\nDATA-TAKE RECORDS REJECTED/DELETED AS REQUESTED: %d\n",
            NUMELTS( deleted_dtks ) ) ;
        dtkm_print_list( report_fp, deleted_dtks ) ;
        (void) fprintf(report_fp, "\nERROR DATA-TAKE RECORDS: %d\n",
            NUMELTS( error_dtks ) ) ;
        dtkm_print_list( report_fp, error_dtks ) ;

        (void) fprintf(report_fp, "\nOTHER CHANGES\n") ;
        (void) fprintf(report_fp,   "-------------\n") ;
        (void) fprintf(report_fp,
            "\nOTHER-SATELLITE DATA-TAKE RECORDS CHANGED: %d\n",
            NUMELTS( other_sat_dtks ) ) ;
        dtkm_print_list( report_fp, other_sat_dtks ) ;
        (void) fprintf(report_fp, 
            "\nSAME-SATELLITE OTHER DATA-TAKE RECORDS CHANGED: %d\n",
            NUMELTS( same_sat_dtks ) ) ;
        dtkm_print_list( report_fp, same_sat_dtks ) ;
        if ( omissions_flag )
        {
            (void) fprintf(report_fp, 
"\nDATABASE RECORDS REJECTED BECAUSE THEY WEREN'T IN THE FA FILE: %d\n",
                NUMELTS( omission_dtks ) ) ;
            dtkm_print_list( report_fp, omission_dtks ) ;
        }

        (void) fprintf(report_fp, "\nCOMPLETE LIST OF FINAL UPDATES: %d\n",
            NUMELTS( dtk_updates ) ) ;
        dtkm_print_list( report_fp, dtk_updates ) ;

        print_design( report_fp ) ;
    }
    return TRUE ;

}
