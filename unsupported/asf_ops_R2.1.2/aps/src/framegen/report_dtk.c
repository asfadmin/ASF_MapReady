#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       report_dtk.c

Description:    reports a data-take to the frame generator interface with IMS.

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)report_dtk.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.report_dtk.c"


/*==============================================================================
Function:       report_dtk()

Description:    report the data-take to the frame generator IMS interface.  
                Subject to various criteria.  Some data may need to be 
                gathered also.  

Creator:        Lawrence Stevens

Creation Date:  Fri Mar 22 15:00:23 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "aps_framegen.h"

int report_dtk(
    char        *progname,
    FILE        *logfp,
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_rec,
    int         *non_fatal_error_count  ) 
{
    int         return_code ;
    char        msg[MSG_LEN] ;
    int         nrec ; 
    int         nrecs_deleted ;

    llist       *dtk_cmb_list ;
    DB_RECORD   **dtk_cmb_rec ;
    cursor      dtk_cmb_list_ptr ;

    DB_RECORD   **framegen_calls_rec ;

    DB_RECORD   **framegen_calls_delete_rec = NULL ;
    llist       *framegen_calls_delete_list ;
    cursor      framegen_calls_delete_ptr ;

    int         time_pairs_count = 0 ;
    TimePair    *time_pairs = NULL ;

    /* quick error checking. */
    if ( dtk_rec == NULL )
        return APS_FRAMEGEN_ERROR_DTK_REC_IS_NULL ;
    if ( strlen( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] ) != 3 )
        return APS_FRAMEGEN_ERROR_IN_DTKSTAT_VALUE ;
    if ( strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "DEL" ) 
    &&   strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "REJ" )
    &&   strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "PLN" ) 
    &&   strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "SCH" ) )
    {
        (void)sprintf(msg,
"%s(%d):  %s/%s/%5.5ld.%2.2d dtkstat value is neither DEL, REJ, PLN, nor SCH.",
            __FILE__, __LINE__,
            CAST_DTK_SAT dtk_rec[DTK_SAT], CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
            CAST_DTK_REV dtk_rec[DTK_REV], CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_IN_DTKSTAT_VALUE ;
    }
 
    /* 
    -- the routine will mark records for deletion 
    -- early.  then it will delete them later.  
    -- here, set up the list of records to delete.  */
    framegen_calls_delete_list = create_dyn_llist() ;
    framegen_calls_delete_rec = new_table_record( APS_CDEFS(FRAMEGEN_CALLS) ) ;

    /*
    -- [Note:  "previously reported" means that there is a record in the 
    --  framegen_calls relation for the data-take in question. ] 
    -- 
    -- 1.  Examine various conditions regarding the current data-take:
    */
    /*
    --  Check to see if dtk_was_reported:
    */
    framegen_calls_rec = new_table_record( APS_CDEFS(FRAMEGEN_CALLS) ) ;
    return_code = dtk_was_reported( progname, logfp, dtk_rec, 
        framegen_calls_rec ) ;
    if ( return_code < 0 )
        return return_code ;

    if ( return_code == TRUE )
    {
        /* 
        -- data-take was previously reported; check the status of 
        -- this data-take:
        */
        if ( strcmp( "REJ", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] ) 
        &&   strcmp( "DEL", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] )  )
        {
            /*
            -- the status is neither REJ nor DEL; now compare the dtkdate 
            -- field with the previous call's date.
            -- If they are the same, do not report this data-take; return 
            -- to the calling routine.  This is because there probably was an 
            -- earlier run in the same day that did not complete and this 
            -- data-take was reported OK already.  
            -- no need to report it again.  
            */
            if ( strcmp(
CAST_FRAMEGEN_CALLS_DTKDATE framegen_calls_rec[FRAMEGEN_CALLS_DTKDATE],
                CAST_DTK_DTKDATE dtk_rec[DTK_DTKDATE] ) == 0 ) 
            {
                /*
                -- The dates are the same; do not report this data-take; 
                -- return to the calling routine.  
                */
#ifdef DEBUG
                printf(
"%s(%d):  data-take already reported for this day, no need to \n",
                    __FILE__, __LINE__ ) ;
                printf("        re-report:  skipping\n" ) ;
#endif

                DEL_LIST( framegen_calls_delete_list ) ;
                free_db_record( framegen_calls_delete_rec ) ;
                free_db_record( framegen_calls_rec ) ;
                return APS_FRAMEGEN_OK ;
            }

            /*
            -- the update dates are not the same.  an edit was done.  
            -- check to see if any data has changed.  
            -- if not, then no call to framegen.  
            -- compare all 5 fields:  
            -- sensor, strttime, stoptime, dtkstat, station_id.  
            -- if anything has changed, then report.  
            */
            if( strcmp( CAST_FRAMEGEN_CALLS_SENSOR 
                        framegen_calls_rec[FRAMEGEN_CALLS_SENSOR], 
                        CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) != 0
            ||  strcmp( CAST_FRAMEGEN_CALLS_STRTTIME 
                        framegen_calls_rec[FRAMEGEN_CALLS_STRTTIME], 
                        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) != 0
            ||  strcmp( CAST_FRAMEGEN_CALLS_STOPTIME 
                        framegen_calls_rec[FRAMEGEN_CALLS_STOPTIME], 
                        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) != 0
            ||  strcmp( CAST_FRAMEGEN_CALLS_DTKSTAT 
                        framegen_calls_rec[FRAMEGEN_CALLS_DTKSTAT], 
                        CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] ) != 0
            ||  strcmp( CAST_FRAMEGEN_CALLS_STATION_ID 
                        framegen_calls_rec[FRAMEGEN_CALLS_STATION_ID], 
                        CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) != 0  )
            {
                /* something changed.  report.  */
                /* set up previous times:  */
                time_pairs_count = 1 ;
                time_pairs = malloc( sizeof(TimePair) ) ;
                tc_asf2odl( CAST_FRAMEGEN_CALLS_STRTTIME 
                            framegen_calls_rec[FRAMEGEN_CALLS_STRTTIME],
                            time_pairs[0].start_time );
                tc_asf2odl( CAST_FRAMEGEN_CALLS_STOPTIME 
                            framegen_calls_rec[FRAMEGEN_CALLS_STOPTIME],
                            time_pairs[0].end_time ) ;
            }
            else
            {
                /* 
                -- nothing changed.  
                -- The data is all the same; do not report this data-take; 
                -- return to the calling routine.  
                */
#ifdef DEBUG
                printf(
"%s(%d):  data-take already reported for this day, no need to \n",
                    __FILE__, __LINE__ ) ;
                printf("        re-report:  skipping\n" ) ;
#endif

                DEL_LIST( framegen_calls_delete_list ) ;
                free_db_record( framegen_calls_delete_rec ) ;
                free_db_record( framegen_calls_rec ) ;
                return APS_FRAMEGEN_OK ;
            }
        }
    } /*    [endif dtk was previously reported (continued) ] */
    else 
    {
        /*
        -- This data-take was NOT previously reported:
        */
        if ( !strcmp( "REJ", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] ) 
        ||   !strcmp( "DEL", CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] )  )
        {
            /*
            -- if the status of the data-take is REJ or DEL, skip it.
            -- this is because this data-take has not previously 
            -- been reported to frame generator.  
            */
            DEL_LIST( framegen_calls_delete_list ) ;
            free_db_record( framegen_calls_delete_rec ) ;
            free_db_record( framegen_calls_rec ) ;
            if ( time_pairs_count )
                free( time_pairs ) ;
            return APS_FRAMEGEN_OK ;
        }
        /* at this time, there are no time-pairs:  */
        time_pairs_count = 0 ;
        time_pairs = NULL ;
    }

    if ( strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "SCH" ) == 0 )
    {

        /*  
        -- data-take status is SCH
        -- Retrieve CMB data-takes, with yesterday's date stamp, from
        -- the dtk relation; they have been combined with this data-take.
        -- Note:  due to the change in dtkdate from 8 to 21 characters, 
        -- we use the "like" keyword and the '%' wildcard to implement 
        -- this action.  Thu Aug  7 19:03:14 PDT 1997
        */
        (void)sprintf( where_clause,
"where %s = '%s' and %s = '%s' and %s = %ld and %s like '%8.8s%%' and %s = '%s' and %s like '%%%s%2.2d%%' ",
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL(DTK, DTK_SENSOR), CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV dtk_rec[DTK_REV],
            APS_COL(DTK, DTK_DTKDATE), CAST_DTK_DTKDATE dtk_rec[DTK_DTKDATE],
            APS_COL(DTK, DTK_DTKSTAT), "CMB",
            APS_COL(DTK, DTK_NOTES), "CMB->",
                    (int) CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;
        (void)sprintf( orderby_cols, "%s ",
            APS_COL(DTK, DTK_DTKID) ) ;
        dtk_cmb_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
            APS_TABLE(DTK),
            where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;
        if ( dtk_cmb_list == NULL )
        {
            (void)sprintf(msg,
                "%s(%d):  Error in Sybase query on data-takes %s order by %s",
                    __FILE__, __LINE__, where_clause, orderby_cols ) ;
            (void)fprintf(logfp, "%s\n", msg ) ;
            aps_log_msg("", APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
                free_db_record( framegen_calls_delete_rec ) ;
            DEL_LIST( framegen_calls_delete_list ) ;
            free_db_record( framegen_calls_delete_rec ) ;
            free_db_record( framegen_calls_rec ) ;
            if ( time_pairs_count )
                free( time_pairs ) ;
            return APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY ;
        }
        /*  [if dtk status == SCH (continued) ] */
        if ( NUMELTS(dtk_cmb_list) > 0 )
        {
            /*
            --  for each CMB record NOT previously reported, report the
            --  CMB data-take separately [make a special call to the
            --  frame generator at this point] with a SCH status and
            --  the times of the CMB data-take and provide no previous
            --  time-pairs with this call.  If there was an ERROR in
            --  the frame generator call, exit the program.  If no
            --  error, then record this special call in the
            --  framegen_calls relation.
            --  append each CMB time-pair to the SCH time-pairs list.
            --  Mark each CMB data-take's framegen_calls record for later
            --  deletion in Step 2.
            */

            /* we introduce time pairs:  */
            /*
            --  time_pairs_count
            */
            time_pairs_count = NUMELTS(dtk_cmb_list) ;
 
            /*
            --  time_pairs
            --  Allocate space for a time pair for every CMB data-take.  
            */
            time_pairs = malloc( NUMELTS(dtk_cmb_list) * sizeof(TimePair) ) ;
 
#ifdef DEBUG
            printf("%s(%d):  dtks retrieved:  %d\n", __FILE__, __LINE__,
                NUMELTS( dtk_cmb_list ) ) ;
            dtkm_print_list( stdout, dtk_cmb_list ) ;
#endif
 
            nrec = 0 ;
            for ( dtk_cmb_rec = (DB_RECORD **)
                            FIRST(dtk_cmb_list, dtk_cmb_list_ptr);
                  dtk_cmb_rec != NULL ;
                  dtk_cmb_rec = (DB_RECORD **)
                            NEXT(dtk_cmb_list, dtk_cmb_list_ptr)
                )
            {

                /* was this CMB data-take previously reported?  */
                return_code = dtk_was_reported( progname, logfp, dtk_cmb_rec, 
                    framegen_calls_delete_rec ) ;
                if ( return_code < 0 )
                {
                    DEL_LIST( dtk_cmb_list ) ;
                    DEL_LIST( framegen_calls_delete_list ) ;
                    free_db_record( framegen_calls_rec ) ;
                    free_db_record( framegen_calls_delete_rec ) ;
                    if ( time_pairs_count )
                        free( time_pairs ) ;
                    return return_code ;
                }
                if ( return_code != TRUE )
                {
                    /*
                    -- CMB record ws NOT previously reported: 
                    -- report the CMB data-take separately [make a special 
                    -- call to the frame generator at this point] with a 
                    -- SCH status and the times of the CMB data-take 
                    -- If there was an ERROR in the frame generator call, 
                    -- exit the program.  
                    */
                    return_code = call_framegen( progname, logfp, APS_dbproc, 
                        dtk_cmb_rec, "SCH", 0, NULL, 
                        framegen_calls_delete_rec ) ;
                    /* If there was a fatal ERROR, exit the program.   */
                    if ( return_code < 0 )
                    {
                        DEL_LIST( dtk_cmb_list ) ;
                        DEL_LIST( framegen_calls_delete_list ) ;
                        free_db_record( framegen_calls_rec ) ;
                        free_db_record( framegen_calls_delete_rec ) ;
                        if ( time_pairs_count )
                            free( time_pairs ) ;
                        return return_code ;
                    }
                    if ( return_code == APS_FRAMEGEN_FG_INPUT_ERROR 
                    ||   return_code == APS_FRAMEGEN_FG_ERROR       )
                    {
                        /* this is >= 0 and therefore a non-fatal error.  */
                        (*non_fatal_error_count)++ ;
                    }

                } /* endif CMB record not previously reported.  */

                /*  
                -- append the reported time-pair to the 
                -- SCH time-pairs list.  
                -- in the process, convert to ODL format.  
                */
                tc_asf2odl( CAST_DTK_STRTTIME dtk_cmb_rec[DTK_STRTTIME], 
                    time_pairs[nrec].start_time ) ;

                tc_asf2odl( CAST_DTK_STOPTIME dtk_cmb_rec[DTK_STOPTIME], 
                    time_pairs[nrec].end_time ) ;

                nrec++ ;

                /*
                --  append the CMB data-take's framegen_calls 
                --  record to the list for later deletion in Step 2.  
                */
                APPEND( framegen_calls_delete_list, 
                        framegen_calls_delete_rec, free_db_record, 
                        framegen_calls_delete_rec) ;
                /* make a new record for later use:  */
                framegen_calls_delete_rec = new_table_record( 
                        APS_CDEFS(FRAMEGEN_CALLS) ) ;

            } /* end for each CMB data-take.  */

        } /* endif for > 0 CMB data-takes.  */ 
        DEL_LIST( dtk_cmb_list ) ;

    }  /*  endif dtk status == SCH         */

    /*
    --  Report to the frame generator the above-accumulated data.  
    --  [make the call to the frame generator at this point]
    */
    return_code = call_framegen( progname, logfp, APS_dbproc, dtk_rec, 
        CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], time_pairs_count, time_pairs, 
        framegen_calls_delete_rec ) ;
    /* if ( return_code < 0 )  NOTE:  we clean up first, then check this... */

    /* free the time pairs, if allocated. */
    if ( time_pairs_count )
        free( time_pairs ) ;

    /* free the uneeded records:  */
    free_db_record( framegen_calls_delete_rec ) ;
    free_db_record( framegen_calls_rec ) ;

    /*
    -- 2.  After the frame generator call for the current data-take is made, 
    -- check the return status.  IF ERROR, then exit the program.  
    */
    if ( return_code < 0 )
    {
        DEL_LIST( framegen_calls_delete_list ) ;
        return return_code ;
    }

    if ( return_code == APS_FRAMEGEN_FG_INPUT_ERROR 
    ||   return_code == APS_FRAMEGEN_FG_ERROR         )
    {
        /* this is >= 0 and a non-fatal error.  */
        (*non_fatal_error_count)++ ;
    }

    /*
    -- If no FATAL error, update the framegen_calls relation according to the 
    -- data-take status:  
    -- 
    -- Delete the framegen_calls records that were noted for deletion earlier, 
    -- in Step 1.  
    */

    /* 
    -- note that if there are no recs in this list, no 
    -- iterations will be run.  first free the DB_RECORD we 
    -- have maintained for use:
    */
    for ( framegen_calls_delete_rec = (DB_RECORD **) 
                FIRST(framegen_calls_delete_list, framegen_calls_delete_ptr) ;
          framegen_calls_delete_rec ;
          framegen_calls_delete_rec = (DB_RECORD **) 
                NEXT(framegen_calls_delete_list, framegen_calls_delete_ptr)
        )
    {
        /* delete this rec from the database:  */
        (void)sprintf( where_clause, 
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SAT), 
            CAST_FRAMEGEN_CALLS_SAT 
                framegen_calls_delete_rec[FRAMEGEN_CALLS_SAT],
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SENSOR), 
            CAST_FRAMEGEN_CALLS_SENSOR 
                framegen_calls_delete_rec[FRAMEGEN_CALLS_SENSOR],
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_REV),
            CAST_FRAMEGEN_CALLS_REV 
                framegen_calls_delete_rec[FRAMEGEN_CALLS_REV],
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_DTKID),
            CAST_FRAMEGEN_CALLS_DTKID 
                framegen_calls_delete_rec[FRAMEGEN_CALLS_DTKID] ) ;

        nrecs_deleted = db_delete_records(APS_dbproc,
                APS_TABLE(FRAMEGEN_CALLS), where_clause ) ;
        if ( nrecs_deleted != 1 )
        {
            DEL_LIST( framegen_calls_delete_list ) ;
            (void)sprintf(msg,
"%s(%d):  Error in Sybase deletion on framegen_calls relation %s",
                __FILE__, __LINE__, where_clause ) ;
            (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG,
                DO_PRINT);
            return APS_FRAMEGEN_ERROR_DELETING_REC_IN_DB ;
        }
    }

    /* now clean up and return.  */
    DEL_LIST( framegen_calls_delete_list ) ;
    return APS_FRAMEGEN_OK ;

}
