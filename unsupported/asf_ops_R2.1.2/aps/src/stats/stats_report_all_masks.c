#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_report_all_masks.c

Description:    report all Mask entry and Mask exits that satisfy condition.
                10-meter antenna only.  

External Functions Defined:
    int stats_report_all_masks()
    
File Scope Functions:
    static int stats_report_msk()   report a single mask entry/exit.
    
==============================================================================*/
#pragma ident   "@(#)stats_report_all_masks.c	1.3 98/03/19 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_report_all_masks.c"


#include <string.h>            /* for strcmp() strcpy()            */
#include <stdlib.h>            /* for malloc() and free()          */
#include <dapps_defs.h>       /* for ASF_TIME_STR_LENGTH           */
#include <timeconv.h>          /* for tc_systime2asf() etc.         */
#include <db_dtk.h>           /* for CAST_DTK_ANTENNA_ID etc.      */
#include <dtkm_utilities.h>   /* for dtkm_get_mask_times()         */
#include "aps_Statistics.h"

/*==============================================================================
Function:       stats_report_msk()

Description:    Calls IMS interface to report mask in and out times 
                of a single 10 meter pass. 

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 12:11:56 PST 1998

==============================================================================*/
static int stats_report_msk(
    DBPROCESS       *APS_dbproc,
    char            *sat,
    int             rev,
    int             antenna_id,
    char            *asftime_mask_entry,
    char            *asftime_mask_exit,
    char            *condition,         /* SCHEDULED or CANCELLED    */
    APS_STATS_IMS_INFO   *aps_stats_ims_info ) 
{
    int         return_code ;
    int         nrecs_inserted ;
    char        asftime_now[ASF_TIME_STR_LENGTH+1] ;
    DB_RECORD   **temp_stats_calls_rec ;

    APS_STATS_PMF_VALUES *pmf_values ;

    (void) tc_systime2asf(asftime_now) ;

    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n----------------------------------------------------------------" ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  Mask Entry-Exit to report:  \n", __FILE__, __LINE__ ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "     %s/%d ASF %d    %s  %s  %s\n",
        sat, rev, antenna_id, asftime_mask_entry, asftime_mask_exit, condition);
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "     asftime_now       %s\n", asftime_now ) ;
 
    /*
    -- make a new empty rec:
    -- create the fields for the record, use
    -- it to check for a previous call and
    -- to register the call:
    */
    temp_stats_calls_rec =  new_table_record(APS_CDEFS(STATS_CALLS)) ;
 
    /*   create the fields for the record:  */
    (void) strcpy( CAST_STATS_CALLS_TYPE temp_stats_calls_rec[STATS_CALLS_TYPE],
        "MSK" ) ;

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
 
    (void) strcpy( CAST_STATS_CALLS_STRTTIME
        temp_stats_calls_rec[STATS_CALLS_STRTTIME], asftime_mask_entry ) ;
    (void) strcpy( CAST_STATS_CALLS_STOPTIME
        temp_stats_calls_rec[STATS_CALLS_STOPTIME], asftime_mask_exit ) ;
    CAST_STATS_CALLS_ANTENNA_ID temp_stats_calls_rec[STATS_CALLS_ANTENNA_ID]
        = antenna_id ;
    (void) strcpy( CAST_STATS_CALLS_DTKSTAT
        temp_stats_calls_rec[STATS_CALLS_DTKSTAT], "" ) ;
    (void) strcpy( CAST_STATS_CALLS_CALL_TIME
        temp_stats_calls_rec[STATS_CALLS_CALL_TIME], asftime_now ) ;
 
    return_code = stats_call_reported( temp_stats_calls_rec ) ;
    if( return_code < 0 )
    {
        /*  error  */
        free_db_record(temp_stats_calls_rec) ;
        return return_code ;
    }
    if( return_code == TRUE )
    {
        /* already reported.  */
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

    /* skip pmf_values->sequence      */
    /* skip pmf_values->sensor        */

    pmf_values->antenna_id = antenna_id ;

    /* skip pmf_values->activity_id        */

    (void) strcpy( pmf_values->asftime_now, asftime_now ) ;

    /* skip pmf_values->asftime_aos        */
    /* skip pmf_values->asftime_los        */
    /* skip pmf_values->asftime_aos_fa     */
    /* skip pmf_values->asftime_los_fa     */

    (void) strcpy( pmf_values->asftime_mask_entry, asftime_mask_entry ) ;
    (void) strcpy( pmf_values->asftime_mask_exit, asftime_mask_exit ) ;

    /* skip pmf_values->asftime_time_on       */
    /* skip pmf_values->asftime_time_off      */
    /* skip pmf_values->asftime_time_on_fa    */
    /* skip pmf_values->asftime_time_off_fa   */
    /* skip pmf_values->asftime_planned       */
    /* skip pmf_values->downlink_rev_char     */
    /* skip pmf_values->downlink_sequence     */
    /* skip pmf_values->downlink_status       */

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
Function:       stats_report_all_masks

Description:    Finds all mask entry and exit times that satisfy 
                condition, and reports.  for passes with 10-meter antenna.

Creator:        Lawrence Stevens

Creation Date:  Mon Jan 19 16:18:36 PST 1998

==============================================================================*/
int stats_report_all_masks(
    DBPROCESS   *APS_dbproc,
    llist       *dtk_list,   /* ASF downlink dtks sorted by sat, rev        */
    char        *condition,  /* SCHEDULED or CANCELLED                      */
                   /* SCHEDULED:  report masks for scheduled downlinks.     */
                   /* CANCELLED:  report masks for cancelled downlinks.     */
    APS_STATS_IMS_INFO   *aps_stats_ims_info,
    int         *call_count ) 
{

    DB_RECORD    **dtk_rec ;
    cursor      dtk_list_ptr ;

    char        asftime_mask_entry[ASF_TIME_STR_LENGTH+1] ;
    char        asftime_mask_exit[ASF_TIME_STR_LENGTH+1] ;
    double      et_mask_entry, et_mask_exit ;  /* ephemeris times.  */

    char        previous_sat[3] = "xx";
    int         previous_rev    = -1 ;
    int         return_code ;

    *call_count = 0 ;

    /* error checking.  */
    if( strcmp( condition, "SCHEDULED" ) != 0 
    &&  strcmp( condition, "CANCELLED" ) != 0  )
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
        if( CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] != 1 )
            continue ; /* exclude dtk:  not 10-meter antenna   */

        if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], previous_sat ) == 0 
        &&   CAST_DTK_REV dtk_rec[DTK_REV] == previous_rev )
             continue ; /* exclude dtk:  already included this same pass */

        /* process the current dtk_rec right here.  */
        if( strcmp( condition, "SCHEDULED" ) == 0 )
        {
            /* collect only SCH passes.     */
            if( stats_dtk_is_sch( dtk_rec ) != TRUE )
                continue ; /* exclude dtk:  not SCH   */
        }
        else
        {
            /* 
            -- report only passes that were scheduled, then 
            -- CANCELLED.   
            -- This means that proposed_dtkstat = SCH   
            -- and dtkstat = REJ, DEL, or CON 
            */
            if( stats_dtk_is_cancelled( dtk_rec ) != TRUE )
                continue ; /* exclude dtk:  not cancelled   */
        }

        /* 
        -- this pass made it through.  
        -- report the mask in and out times for 
        -- this sat, rev:  
        -- save the pass identifier, don't report twice.  
        */
        (void) strcpy( previous_sat, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
        previous_rev = CAST_DTK_REV dtk_rec[DTK_REV] ; 
        return_code = dtkm_get_mask_times( previous_sat, previous_rev, "ASF", 
            asftime_mask_entry,  asftime_mask_exit, 
            &et_mask_entry, &et_mask_exit ) ;
        if( return_code < 0 )
        {
            (void) fprintf(aps_stats_ims_info->logfile_ptr,
                "%s(%d):  %s\n    sat = %s, rev = %d, station_id:  ASF\n", 
                __FILE__, __LINE__, DTKM_ERROR_MESSAGE(return_code),
                previous_sat, previous_rev ) ;
            return STATS_ERROR_GETTING_MASK_TIMES ;
        }

        /*
        -- Report this pass to the IMS:
        -- ( the '1' in the argument list means antenna 1, 10 meter. )
        */
        return_code = stats_report_msk( APS_dbproc, 
            previous_sat, previous_rev, 1, 
            asftime_mask_entry,  asftime_mask_exit,
            condition,  aps_stats_ims_info ) ;
        if( return_code < 0 )
            return return_code ;
        /* return_code = 1(TRUE) if reported, 0(FALSE) if not:  */
        (*call_count) += return_code ;

    }

    return TRUE ;

}
