#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_report_all_aos_los.c

Description:    go through list, report all AOS/LOS in it.  

External Functions Defined:  
    int stats_report_all_aos_los()
    
File Scope Functions:  
    static int stats_report_aos()
    
==============================================================================*/
#pragma ident   "@(#)stats_report_all_aos_los.c	1.2 98/03/19 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_report_all_aos_los.c"

#include <aps_db_table.h>      /* for STATS_CALLS                   */
#include <string.h>            /* for strcmp(), strcpy()            */
#include <stdlib.h>            /* for malloc()                      */
#include "aps_Statistics.h"
#include <dapps_defs.h>        /* for ASF_TIME_STR_LENGTH           */
#include <timeconv.h>          /* for tc_systime2asf() etc.         */
#include <db_stats_calls.h>    /* for CAST_STATS_CALLS_SAT etc.     */
#include <db_dtk.h>            /* for CAST_DTK_ANTENNA_ID etc.      */
#include <dtkm_utilities.h>    /* for dtkm_print()                  */


/*==============================================================================
Function:       stats_report_aos()

Description:    Calls IMS interfaces with to report a aos los time info
                of a 11 meter pass. 
                Returns TRUE if successfully reported
                Returns FALSE if already reported.  in this case, there 
                    won't be any reporting  - not an error.  
                Returns < 0 if there was an error.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 12:11:56 PST 1998

==============================================================================*/
static int stats_report_aos(
    DBPROCESS       *APS_dbproc,
    char            *sat,
    int             rev,
    int             antenna_id,
    char            *asftime_aos,
    char            *asftime_los,
    char            *asftime_original_aos,
    char            *asftime_original_los,
    char            *condition,         /* SCHEDULED, CANCELLED, or REDUCED */
    APS_STATS_IMS_INFO   *aps_stats_ims_info ) 
{

    int         return_code ;
    int         nrecs_inserted ;
    char        asftime_now[ASF_TIME_STR_LENGTH+1] ;
    DB_RECORD   **temp_stats_calls_rec ;

    APS_STATS_PMF_VALUES *pmf_values ;

    (void) tc_systime2asf( asftime_now ) ;

    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n----------------------------------------------------------------" ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  AOS-LOS to report:  \n", __FILE__, __LINE__ ) ;

    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "     %s/%d ASF %d    %s\n", 
        sat, rev, antenna_id, condition ) ;

    if( strcmp(condition, "CANCELLED") != 0 )
    {
        /* only cancelled does not have these times:  */
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "     AOS-LOS  %s  %s\n", asftime_aos, asftime_los ) ;
    }
    if( strcmp(condition, "SCHEDULED") != 0 )
    {
        /* only scheduled does not have original times:  */
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "     original AOS-LOS  %s  %s\n", 
            asftime_original_aos,
            asftime_original_los ) ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "     asftime_now       %s\n", asftime_now ) ;

    /* 
    -- make a new empty rec:  
    -- [must use free_db_record() later, to free the memory ]
    -- create the fields for the record, use 
    -- it to check for a previous call and 
    -- to register the call:  
    */
    temp_stats_calls_rec =  new_table_record(APS_CDEFS(STATS_CALLS)) ;

    (void) strcpy( CAST_STATS_CALLS_TYPE temp_stats_calls_rec[STATS_CALLS_TYPE],
        "AOS" ) ;

    /* only copy the first 3 chars to the stats_calls.condition field:  */
    (void) strcpy( 
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        "   " ) ;
    (void) strncpy( 
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        condition, 3 ) ;

    (void) strcpy( CAST_STATS_CALLS_SAT temp_stats_calls_rec[STATS_CALLS_SAT], 
        sat ) ;

    CAST_STATS_CALLS_REV temp_stats_calls_rec[STATS_CALLS_REV] = rev ;

    CAST_STATS_CALLS_DTKID temp_stats_calls_rec[STATS_CALLS_DTKID] = 0 ;

    if( strcmp(condition, "CANCELLED") == 0 )
    {
        /* cancelled has only the original times.    */
        (void) strcpy( CAST_STATS_CALLS_STRTTIME 
            temp_stats_calls_rec[STATS_CALLS_STRTTIME], asftime_original_aos ) ;

        (void) strcpy( CAST_STATS_CALLS_STOPTIME 
            temp_stats_calls_rec[STATS_CALLS_STOPTIME], asftime_original_los ) ;
    }
    else
    {
        (void) strcpy( CAST_STATS_CALLS_STRTTIME 
            temp_stats_calls_rec[STATS_CALLS_STRTTIME], asftime_aos ) ;

        (void) strcpy( CAST_STATS_CALLS_STOPTIME 
            temp_stats_calls_rec[STATS_CALLS_STOPTIME], asftime_los ) ;
    }

    CAST_STATS_CALLS_ANTENNA_ID temp_stats_calls_rec[STATS_CALLS_ANTENNA_ID] 
        = antenna_id ;

    (void) strcpy( CAST_STATS_CALLS_DTKSTAT 
        temp_stats_calls_rec[STATS_CALLS_DTKSTAT], "" ) ;

    (void) strcpy( CAST_STATS_CALLS_CALL_TIME 
        temp_stats_calls_rec[STATS_CALLS_CALL_TIME], asftime_now ) ;

    return_code = stats_call_reported( temp_stats_calls_rec ) ;
    if( return_code < 0 )
    {
        /* error.  */
        free_db_record(temp_stats_calls_rec) ;
        return return_code ;
    }

    if( return_code == TRUE )
    {
        /* stats call was already reported.  */
        (void) fprintf(aps_stats_ims_info->logfile_ptr, 
            "\nPMF named:  stats.%s.%s.%s.%ld.%d.S  was previously reported.\n",
            CAST_STATS_CALLS_TYPE temp_stats_calls_rec[STATS_CALLS_TYPE],
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
            CAST_STATS_CALLS_SAT temp_stats_calls_rec[STATS_CALLS_SAT], 
            CAST_STATS_CALLS_REV temp_stats_calls_rec[STATS_CALLS_REV],
            CAST_STATS_CALLS_DTKID temp_stats_calls_rec[STATS_CALLS_DTKID] ) ;
        free_db_record(temp_stats_calls_rec) ;
        return FALSE ;   /* no error.  already reported.  */
    }

    /* 
    -- stats call not yet reported.  
    -- report to IMS now:  
    */

    /*********************************************************************
    *                                                                    *
    *  Interface with IMS.                                               *
    *                                                                    *
    *  1.  Load pmf_values structure                                     *
    *  2.  Call IMS_interface() to do IMS interface.                     *
    *                                                                    *
    *********************************************************************/
    /* 
    -- fill structure with values for PMF.  
    -- 6 or 8 values:  
    */

    pmf_values = malloc( sizeof(APS_STATS_PMF_VALUES) ) ;
    if( pmf_values == NULL )
    {
        free_db_record(temp_stats_calls_rec) ;
        return STATS_ERROR_MALLOC_FAILED ;
    }
    (void) stats_init_pmf_values( pmf_values ) ;

    (void) strcpy( pmf_values->type, 
        CAST_STATS_CALLS_TYPE temp_stats_calls_rec[STATS_CALLS_TYPE] ) ;
    (void) strcpy( pmf_values->condition, 
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION]);

    (void) strcpy( pmf_values->sat, sat ) ;
    pmf_values->rev = rev ;
    pmf_values->antenna_id = antenna_id ;
    (void) strcpy( pmf_values->asftime_now, asftime_now ) ;
    if( strcmp(condition, "REDUCED") == 0 
    ||  strcmp(condition, "CANCELLED") == 0 )
    {
        (void) strcpy( pmf_values->asftime_aos_fa, asftime_original_aos ) ;
        (void) strcpy( pmf_values->asftime_los_fa, asftime_original_los ) ;
    }

    if( strcmp(condition, "CANCELLED") != 0 )
    {
        /* condition was NOT CANCELLED    */
        (void) strcpy( pmf_values->asftime_aos, asftime_aos ) ;
        (void) strcpy( pmf_values->asftime_los, asftime_los ) ;
    }

    /*********************************************************************
    *                                                                    *
    *  Make call to IMS to report the statistics.                        *
    *                                                                    *
    *********************************************************************/
    return_code = IMS_interface( aps_stats_ims_info, pmf_values ) ;
    free(pmf_values) ;
    if( return_code < 0 )
    {
        free_db_record(temp_stats_calls_rec) ;
        return return_code ;    /* error in call.  */
    }

    /*
    -- record the successful call to IMS.  
    */
    nrecs_inserted = db_insert_single_record(APS_dbproc,
        temp_stats_calls_rec, APS_TABLE(STATS_CALLS), APS_CDEFS(STATS_CALLS) ) ;
    free_db_record(temp_stats_calls_rec) ;
    if( nrecs_inserted < 0 )
        return STATS_ERROR_DB_ERROR_INSERTING_STATS_CALLS_REC ;
    else if( nrecs_inserted != 1 )
        return STATS_ERROR_STATS_CALLS_REC_NOT_INSERTED ;

    /* successful reporting.  */
    return TRUE ;

}


/*==============================================================================
Function:       stats_report_all_aos_los

Description:    report aos and los times to IMS for statistics. 
                for passes with 11-meter antenna.  For each pass (sat/rev)

    Definition of AOS/LOS times for a pass at ASF:
    ----------------------------------------------
        Consider all downlinks in the pass and determine the earliest
        start time and the latest stop time.
        (There may be more than one downlink in the pass.)
        Then pad the start time by the "pre dtk track padding" and the
        pad the stop time by the "post dtk track padding".

        Wed Jan 21 1998:   For the 11 meter antenna, these times 
        are 60 and 30 seconds, respectively.  

        The resulting times are the AOS/LOS times for the pass.


Creator:        Lawrence Stevens

Creation Date:  Mon Jan 19 16:18:36 PST 1998

==============================================================================*/
int stats_report_all_aos_los(
    DBPROCESS   *APS_dbproc,
    llist       *dtk_list,   /* ASF downlink dtks sorted by sat, rev          */
    char        *condition,  /* SCHEDULED, CANCELLED, or REDUCED              */
                   /* SCHEDULED:  report aos-los for scheduled dtks.          */
                   /* CANCELLED:  report original aos-los for cancelled dtks. */
                   /* REDUCED:  report original and reduced aos-los           */
    APS_STATS_IMS_INFO   *aps_stats_ims_info,
    int         *call_count ) 
{

    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr ;

    char    asftime_aos[ASF_TIME_STR_LENGTH+1] ;
    char    asftime_los[ASF_TIME_STR_LENGTH+1] ;
    char    asftime_original_aos[ASF_TIME_STR_LENGTH+1] ;
    char    asftime_original_los[ASF_TIME_STR_LENGTH+1] ;

    char        previous_sat[3] = "xx";
    int         previous_rev    = -1 ;
    int         return_code ;

    *call_count = 0 ;

    /* error checking.  */
    if( strcmp( condition, "SCHEDULED" ) != 0 
    &&  strcmp( condition, "CANCELLED" ) != 0  
    &&  strcmp( condition, "REDUCED" ) != 0  )
        return STATS_ERROR_CONDITION_ARG ;

    /*
    -- find unique sat, rev combinations, to report mask 
    -- in and out.  
    -- Input list is sorted by sat, rev.  
    */
    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
        )
    {
        if( CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] != 2 )
            continue ; /* exclude dtk:  not 11-meter antenna   */

        if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], previous_sat ) == 0 
        &&   CAST_DTK_REV dtk_rec[DTK_REV] == previous_rev )
             continue ; /* exclude dtk:  already included this same pass */

        /* process the current dtk_rec right here.  */
        if( strcmp( condition, "SCHEDULED" ) == 0 )
        {
            /* collect only SCH passes.     */
            if( stats_dtk_is_sch( dtk_rec ) != TRUE )
                continue ; /* exclude dtk:  not SCH   */

            /*
            -- Compute scheduled AOS/LOS times  
            -- see stats_compute_aos_los() for details.  
            */
            return_code = stats_compute_aos_los( dtk_rec, condition, 
                asftime_aos, asftime_los ) ;
            if( return_code < 0 )
                return return_code ;

        }
        else if( strcmp( condition, "CANCELLED" ) == 0 )
        {
            /* 
            -- report only passes that were scheduled, then 
            -- CANCELLED.   
            -- This means that proposed_dtkstat = SCH   
            -- and dtkstat = REJ, DEL, or CON 
            */
            if( stats_dtk_is_cancelled( dtk_rec ) != TRUE )
                continue ; /* exclude:  not cancelled   */
            /*
            -- Compute actual AOS/LOS times for cancelled dtks
            -- see stats_compute_aos_los() for details.  
            */
            return_code = stats_compute_aos_los( dtk_rec, condition, 
                asftime_original_aos, asftime_original_los ) ;
            if( return_code < 0 )
                return return_code ;
        }
        else
        {
            /* 
            -- report only passes that were scheduled, then 
            -- REDUCED.   
            -- This means that dtk.dtkstat = SCH and 
            -- dtk.asf_reduction_min > 0.0
            */
            if( stats_dtk_is_reduced( dtk_rec ) != TRUE )
                continue ; /* exclude:  not reduced   */

            /*
            -- Compute reduced AOS/LOS times  
            -- see stats_compute_aos_los() for details.  
            */
            return_code = stats_compute_aos_los( dtk_rec, condition, 
                asftime_aos, asftime_los ) ;
            if( return_code < 0 )
                return return_code ;
            /*
            -- Also compute original AOS/LOS times:  
            -- see stats_compute_aos_los() for details.  
            */
            return_code = stats_compute_aos_los( dtk_rec, "ORIGINAL", 
                asftime_original_aos, asftime_original_los ) ;
            if( return_code < 0 )
                return return_code ;
        }

        /* 
        -- this pass made it through.  
        -- report the aos/los times for this sat, rev:  
        -- save the pass identifier, don't report twice.  
        */
        (void) strcpy( previous_sat, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
        previous_rev = CAST_DTK_REV dtk_rec[DTK_REV] ; 

        /*
        -- Report this aos_los pass to the IMS:
        -- ( the '2' in the argument list means antenna 2, 11 meter. )
        */
        return_code = stats_report_aos( APS_dbproc, 
            previous_sat, previous_rev, 2, 
            asftime_aos,          asftime_los,
            asftime_original_aos, asftime_original_los,
            condition,  aps_stats_ims_info ) ;
        if( return_code < 0 )
            return return_code ;

        /* return_code = 1 if reported, 0 if not:  */
        (*call_count) += return_code ;
    }

    return TRUE ;

}
