#undef PRINT_DIAG
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_framegen.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)aps_framegen.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.aps_framegen.c"

#include "db_sybint.h"      /* for APS sybase interface routines.   */
#include "aps_db_table.h"   /* for DTK - accessing dtk table.       */
#include "aps_defs.h"       /* for APSDB_ENVVAR and APS_USERID_ENVVAR   */
#include "db_dtk.h"         /* for APS db relation dtk.             */
#include "DARconversions.h" /* for table_lookupAPS2IMS()            */
#include "IMSconversions.h" /* for fa_table_lookupAPS2()            */
#include "ODLconversions.h" /* for WOS_satellite, etc.              */
#include "dtkm_utilities.h" /* for dtkm_print() and dtkm_print_list()   */


#include "aps_framegen.h"   
#include "timeconv.h"   

#include <string.h>         /* for strdup()   */


/*==============================================================================
Function:       aps_framegen()

Description:    function that does frame generation.  It obtains the data 
                from the APS DB, then writes the stats to the IMS DB.  

Creator:        Lawrence Stevens

Creation Date:  Tue Feb 27 11:57:43 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int 
aps_framegen( 
    char        *progname,   
    FILE        *logfp,   
    DBPROCESS   *APS_dbproc, 
    char        *today_dtkdate ) 
{

    int         return_code ;
    char        msg[MSG_LEN];

    char        today_asftime[]     = "yyyy:ddd:00:00:00.000";
    char        yesterday_asftime[] = "yyyy:ddd:hh:mm:ss.sss";
    char        yesterday_dtkdate[] = "yyyy:ddd";

    DB_RECORD   **dtk_rec ;
    llist       *dtk_list = NULL ;
    cursor      dtk_list_ptr ;

    DB_RECORD   **framegen_calls_rec ;
    DB_RECORD   **prev_dtk_rec ;

    /* 
    -- initialize the non-fatal error count for the run to 0.  
    -- this count, if there are no FAIL errors, is returned in 
    -- the return code.  
    */
    int         non_fatal_error_count = 0 ;

    /*
    -- 1.   Retrieve all SAR sensor (recording or real-time) data-takes with
    --      yesterday's date stamp with a status of PLN, SCH, DEL, or REJ, and 
    --      with satellite != A1 (ADEOS)
    --
    -- yesterday's data-takes: all those with 
    -- dtkdate = yyyy:ddd corresponding to yesterday.  
    -- start with the argument todays dtkdate, then develop 
    -- yesterday's dtkdate.  
    */
    (void)strncpy( today_asftime, today_dtkdate, 8 ) ;
    return_code = tc_asf_add_ndays( today_asftime, -1.0, yesterday_asftime ) ;
    (void)strncpy( yesterday_dtkdate, yesterday_asftime, 8 ) ;

    /* 
    -- dtkdate changed to full 21-char field.  
    -- Thu Aug  7 18:53:03 PDT 1997
    -- to implement the query, we now use the keyword 'like' 
    -- and the wild card character '%'.  
    */
    (void)sprintf( where_clause, 
"where %s != 'A1' and %s like 'S%%' and %s like '%s%%' and (  %s = '%s' or %s = '%s' or %s = '%s' or %s = '%s' )", 
        APS_COL(DTK, DTK_SAT), 
        APS_COL(DTK, DTK_SENSOR), 
        APS_COL(DTK, DTK_DTKDATE), yesterday_dtkdate,
        APS_COL(DTK, DTK_DTKSTAT), "DEL",
        APS_COL(DTK, DTK_DTKSTAT), "REJ",
        APS_COL(DTK, DTK_DTKSTAT), "PLN",
        APS_COL(DTK, DTK_DTKSTAT), "SCH" ) ;

    /* 
    -- order by dtkdate, the time of the update.  
    -- so that the most recent frame status is 
    -- reported last.  
    */
    (void)sprintf( orderby_cols, "%s ", APS_COL(DTK, DTK_DTKDATE) ) ;

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        (void)sprintf(msg, 
            "%s(%d):  Error in Sybase query on data-takes %s order by %s", 
            __FILE__, __LINE__, where_clause, orderby_cols ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY ;
    }
    if ( NUMELTS(dtk_list) <= 0 )
    {
        (void)sprintf(msg, 
            "%s(%d):  No data-takes found to process %s", 
            __FILE__, __LINE__, where_clause ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
        /* 
        -- continue processing; there is still more 
        -- processing to do later.   
        */
    }

#ifdef PRINT_DIAG
    (void) printf("%s(%d):  dtks retrieved:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_list ) ) ;
    dtkm_print_list( stdout, dtk_list ) ;
#endif

    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
        )
    {
#ifdef PRINT_DIAG
        (void) printf("\n%s(%d):  now processing:\n", __FILE__, __LINE__ ) ;
        (void) printf(
        "------------------------------------------------------------------\n");
        dtkm_print( stdout, dtk_rec ) ;
#endif

        /* 
        -- If sat = R1 AND there was a sensor mode change, 
        -- AND if the dtk was previously reported, then 
        -- there is special processing.  
        */
        if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1" ) == 0 )
        {
            /* sat = R1.   */
            framegen_calls_rec = new_table_record(APS_CDEFS(FRAMEGEN_CALLS)) ;
            return_code = dtk_was_reported( progname, logfp, dtk_rec, 
                framegen_calls_rec ) ;
            if ( return_code < 0 )
            {
                free_db_record( framegen_calls_rec ) ;
                return return_code ;
            }
            if ( return_code != TRUE )
                free_db_record( framegen_calls_rec ) ;
            else
            {
                /* 
                -- R1 dtk was reported before.  
                -- Check for sensor mode change.  
                */
                if( strcmp( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                            CAST_FRAMEGEN_CALLS_SENSOR 
                            framegen_calls_rec[FRAMEGEN_CALLS_SENSOR] ) != 0 )
                {
                    /* 
                    -- there was a sensor mode change.  
                    -- report the previous call as REJ/DEL 
                    -- and then delete the framegen_calls rec
                    -- from the database.  
                    -- then continue with the current data-take.  
                    -- NOTE:  we are capturing the case where 
                    -- the sensor = SAR and used to be something else.  
                    -- normally, the sensor = SAR would not be handled 
                    -- at all, so this is why this sensor mode change case 
                    -- is handled here.  
                    */
                    /* 
                    -- set up prev_dtk_rec to be the dtk_rec being 
                    -- rejected.  
                    */  
                    prev_dtk_rec = new_table_record(APS_CDEFS(DTK)) ;
                    (void) db_copy_record ( APS_CDEFS(DTK), 
                        prev_dtk_rec, dtk_rec ) ;
                    /* 
                    -- set sensor to old value and status to 
                    -- REJ for the reporting.  
                    */
                    (void)strcpy( CAST_DTK_SENSOR prev_dtk_rec[DTK_SENSOR], 
                            CAST_FRAMEGEN_CALLS_SENSOR 
                            framegen_calls_rec[FRAMEGEN_CALLS_SENSOR] ) ;
                    (void)strcpy( CAST_DTK_DTKSTAT prev_dtk_rec[DTK_DTKSTAT], "REJ");

                    /* reject the previous sensor data-take call.   */
                    return_code = report_dtk( progname, logfp, APS_dbproc, 
                        prev_dtk_rec, &non_fatal_error_count) ;
                    if ( return_code < 0 )
                        return return_code ;

                    /*
                    -- done.  now free the memory that was allocated.  
                    */
                    free_db_record( prev_dtk_rec ) ;
                    free_db_record( framegen_calls_rec ) ;

                    /* 
                    -- now proceed normally with the new dtk_rec.  
                    -- conditions are now as if the old dtk never 
                    -- existed.  
                    */
                }  /* endif sensor mode change.  */

            } /* endif dtk was previously reported.  */

        }

        /*
        --  3.  If the data-take is DEL or REJ:
        --      o   If the data-take has been previously reported, then report
        --          this data-take as described above.
        --      o   CONTINUE with the next data-take.
        */
        if ( !strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "DEL" )
        ||   !strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "REJ" )  )
        {
            return_code = dtk_was_reported( progname, logfp, dtk_rec, NULL ) ;
            if ( return_code < 0 )
                return return_code ;
            if ( return_code == TRUE )
            {
                /* data-take has been previously reported  */
                return_code = report_dtk( progname, logfp, APS_dbproc, dtk_rec,
                    &non_fatal_error_count) ;
                if ( return_code < 0 )
                    return return_code ;
                continue ;
            }
#ifdef PRINT_DIAG
                (void) printf(
"%s(%d):  data-take is unreported, no need to report REJ/DEL:  skipping\n", 
                    __FILE__, __LINE__ ) ;
#endif
            continue ;
        }

        /*
        --  4.  If DARID != 0:
        --      o   If the satellite == R1 and the sensor == SAR, then this 
        --          is an ERROR; the sar mode is unresolved.  Report this 
        --          error to the APS syslog; CONTINUE to step 2 with the 
        --          next data-take.
        --      o   report the data-take 
        --      o   CONTINUE to step 2 with the next data-take.
        */
        if ( CAST_DTK_DARID dtk_rec[DTK_DARID] != 0 )
        {
            if ( !strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1" ) 
            &&   !strcmp( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], "SAR" ) )
            {
                /* error.  sat = R1, sensor = SAR:  mode is unresolved */
                (void)sprintf(msg,
"%s(%d):  data-take:  %s/%s/%5.5ld.%2.2d  sensor mode is unresolved",
                    __FILE__, __LINE__,
                    CAST_DTK_SAT    dtk_rec[DTK_SAT],
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                    CAST_DTK_REV    dtk_rec[DTK_REV],
                    CAST_DTK_DTKID  dtk_rec[DTK_DTKID] ) ;
                (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
                aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);

                /* continue on with the next data-take, anyway:  */
                continue ;
            }
            return_code = report_dtk( progname, logfp, APS_dbproc, dtk_rec,
                    &non_fatal_error_count ) ;
            if ( return_code < 0 )
                return return_code ;
            continue ;
        }

        /*
        --  5.  If DARID == 0:
        --      o   if the satellite = R1 (Radarsat) CONTINUE to step 2 with the
        --          next data-take.
        --      o   if the status == SCH, report the data-take 
        --      o   if the sat == E2 or E2, report the data-take 
        --      o   CONTINUE to step 2 with the next data-take.
        */
        else
        {
            if ( !strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1" ) )
            {
#ifdef PRINT_DIAG
                (void) printf("%s(%d):  darid = 0 and sat = R1:  skipping\n", 
                    __FILE__, __LINE__ ) ;
#endif
                continue ;
            }
            if ( !strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "SCH" ) )
            {
                /* SCH:  report it.  */
                return_code = report_dtk( progname, logfp, APS_dbproc, dtk_rec,
                    &non_fatal_error_count ) ;
                if ( return_code < 0 )
                    return return_code ;
                continue ;
            }
            if ( ( !strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E1" ) 
                 ||   !strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "E2" ) )
            &&   !strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "PLN" )  )
            {
                /* sat == E1 or E2.  Also, status == PLN:  report it.  */
                return_code = report_dtk( progname, logfp, APS_dbproc, dtk_rec,
                    &non_fatal_error_count ) ;
                if ( return_code < 0 )
                    return return_code ;
                continue ;
            }
#ifdef PRINT_DIAG
            (void) printf("%s(%d):  sat = %s, darid=0 and status != SCH:  skipping\n", 
                __FILE__, __LINE__, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
#endif
            continue ;
        }

    }  /* for each data-take   */

    /* list is not needed now.  */
    DEL_LIST( dtk_list ) ;

    /*
    -- 6.   Report the expired data-takes:
    --      Retrieve all data-takes from the framegen_calls relation with 
    --      stoptime before the current date.  For each data-take, check 
    --      its current status in the dtk relation.
    --      If the status != SCH, report this data-take as rejected.
    */
#ifdef PRINT_DIAG
    (void) printf( "\n%s(%d):  now report as REJ all expired non-SCH data-takes\n",
        __FILE__, __LINE__ ) ;
    (void) printf( "         that may still have frames.\n" ) ;
    (void) printf(
        "------------------------------------------------------------------\n");
#endif

    return_code = report_expired_dtks( progname, logfp, APS_dbproc, 
        today_dtkdate, &non_fatal_error_count ) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    -- 7.   Clean up by deleting records from the framegen_calls relation 
    --      with stoptime before the current date.
    */
#ifdef PRINT_DIAG
    (void) printf( "\n%s(%d):  now clean up framegen_calls with stoptime before %s\n",
        __FILE__, __LINE__, today_dtkdate ) ;
    (void) printf(
        "------------------------------------------------------------------\n");
#endif

    return_code = clean_up_framegen_calls(progname, logfp, APS_dbproc, 
        today_dtkdate) ;
    if ( return_code < 0 )
        return return_code ;

    /* check for non-fatal errors in this run.  */
    if ( non_fatal_error_count > 0 )
        return non_fatal_error_count ;

    /* 
    -- a COMPLETELY clean run:   
    -- APS_FRAMEGEN_FG_OK MUST be == 0  
    */
    return APS_FRAMEGEN_OK ;

} /* aps_framegen()   */
