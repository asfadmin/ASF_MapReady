#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_report_all_downlinks.c

Description:    go through list, report all downlinks which satisfy condition.  

External Functions Defined:
    int stats_report_all_downlinks()
    
File Scope Functions:
    static int stats_report_sar_past(
    
==============================================================================*/
#pragma ident   "@(#)stats_report_all_downlinks.c	1.2 98/03/19 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_report_all_downlinks.c"


#include <string.h>          /* for strcmp(), strcpy()            */
#include <stdlib.h>            /* for malloc()                      */
#include <dapps_defs.h>      /* for ASF_TIME_STR_LENGTH           */
#include <timeconv.h>        /* for tc_systime2asf() etc.         */
#include <db_dtk.h>          /* for CAST_DTK_FA_STRTTIME etc.     */
#include <dtkm_utilities.h>  /* for dtkm_print(), dtkm_dl2obs()   */

#include "aps_Statistics.h"


/*==============================================================================
Function:       stats_report_sar_past()

Description:    Calls IMS interfaces to report past SAR dtk info; the 
                SAR imaging was planned for the past and either 
                took place, or was canceled.  A link to the 
                downlink is provided in the report.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 28 12:17:12 PST 1998

==============================================================================*/
static int stats_report_sar_past(
    DBPROCESS       *APS_dbproc,
    DB_RECORD       **dl_rec,           /* downlink                */
    DB_RECORD       **sar_dtk_rec,      /* sar observation         */
    char            *condition,         /* REDUCED or CANCELLED    */
    APS_STATS_IMS_INFO   *aps_stats_ims_info ) 
{
    int         return_code ;
    int         nrecs_inserted ;
    char        asftime_now[ASF_TIME_STR_LENGTH+1] ;
    DB_RECORD   **temp_stats_calls_rec ;

    APS_STATS_PMF_VALUES *pmf_values ;

    (void) tc_systime2asf(asftime_now) ;


    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  SAR activity on above downlink to report:  \n", 
        __FILE__, __LINE__ ) ;
    dtkm_print( aps_stats_ims_info->logfile_ptr, sar_dtk_rec ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "original SAR times:           %s %s\n",
            CAST_DTK_FA_STRTTIME sar_dtk_rec[DTK_FA_STRTTIME],
            CAST_DTK_FA_STOPTIME sar_dtk_rec[DTK_FA_STOPTIME] ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "    condition   = %s\n", condition ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "    asftime_now  %s\n", asftime_now ) ;

    /*
    -- make a new empty rec:
    -- create the fields for the record, use
    -- it to check for a previous call and
    -- to register the call:
    */
    temp_stats_calls_rec =  new_table_record(APS_CDEFS(STATS_CALLS)) ;

    /*   create the fields for the record:  */
    (void) strcpy( CAST_STATS_CALLS_TYPE temp_stats_calls_rec[STATS_CALLS_TYPE],
        "SAR" ) ;
 
    /* only copy the first 3 chars to the stats_calls.condition field:  */
    (void) strcpy(
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        "   " ) ;
    (void) strncpy(
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        condition, 3 ) ;
 
    (void) strcpy( CAST_STATS_CALLS_SAT temp_stats_calls_rec[STATS_CALLS_SAT],
        CAST_DTK_SAT sar_dtk_rec[DTK_SAT] ) ;
 
    CAST_STATS_CALLS_REV temp_stats_calls_rec[STATS_CALLS_REV]
        = CAST_DTK_REV sar_dtk_rec[DTK_REV] ;
 
    CAST_STATS_CALLS_DTKID temp_stats_calls_rec[STATS_CALLS_DTKID]
        = CAST_DTK_DTKID sar_dtk_rec[DTK_DTKID] ;
 
    /* use original FA times:  */
    (void) strcpy(
        CAST_STATS_CALLS_STRTTIME temp_stats_calls_rec[STATS_CALLS_STRTTIME],
        CAST_DTK_FA_STRTTIME sar_dtk_rec[DTK_FA_STRTTIME] ) ;
    (void) strcpy(
        CAST_STATS_CALLS_STOPTIME temp_stats_calls_rec[STATS_CALLS_STOPTIME],
        CAST_DTK_FA_STOPTIME sar_dtk_rec[DTK_FA_STOPTIME] ) ;
 
    CAST_STATS_CALLS_ANTENNA_ID temp_stats_calls_rec[STATS_CALLS_ANTENNA_ID]
        = 0 ;
 
    (void) strcpy(
        CAST_STATS_CALLS_DTKSTAT temp_stats_calls_rec[STATS_CALLS_DTKSTAT],
        CAST_DTK_DTKSTAT sar_dtk_rec[DTK_DTKSTAT] ) ;
 
    (void) strcpy( CAST_STATS_CALLS_CALL_TIME
        temp_stats_calls_rec[STATS_CALLS_CALL_TIME], asftime_now ) ;
 
    return_code = stats_call_reported( temp_stats_calls_rec ) ;
    if( return_code < 0 )
    {
        /*   error.   */
        free_db_record(temp_stats_calls_rec) ;
        return return_code ;
    }
    if( return_code == TRUE )
    {
        /* stats call already reported.  */
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

    /* not yet reported.  */

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

    (void) strcpy( pmf_values->sat, CAST_DTK_SAT sar_dtk_rec[DTK_SAT] ) ;

    pmf_values->rev  = CAST_DTK_REV sar_dtk_rec[DTK_REV] ;

    pmf_values->dtkid = CAST_DTK_DTKID sar_dtk_rec[DTK_DTKID] ;

    (void) strcpy( pmf_values->sensor, 
        CAST_DTK_SENSOR sar_dtk_rec[DTK_SENSOR] ) ;

    /* skip pmf_values->antenna_id_char    not a downlink           */
    /* skip pmf_values->activity_id        not a downlink           */
    /* skip pmf_values->status             downlink status only     */

    (void) strcpy( pmf_values->asftime_now, asftime_now ) ;

    /* skip pmf_values->asftime_aos                                 */
    /* skip pmf_values->asftime_los                                 */

    /* 
    -- use the original FA times for SAR 
    -- activities on downlinks   
    */
    (void) strcpy( pmf_values->asftime_on_fa, 
        CAST_DTK_FA_STRTTIME sar_dtk_rec[DTK_FA_STRTTIME] ) ;
    (void) strcpy( pmf_values->asftime_off_fa, 
        CAST_DTK_FA_STOPTIME sar_dtk_rec[DTK_FA_STOPTIME] ) ;

    /* skip pmf_values->asftime_mask_entry                          */
    /* skip pmf_values->asftime_mask_exit                           */
    /* skip pmf_values->asftime_aos                                 */
    /* skip pmf_values->asftime_los                                 */
    /* skip pmf_values->asftime_planned                             */

    pmf_values->downlink_rev = CAST_DTK_REV dl_rec[DTK_REV] ;

    pmf_values->downlink_dtkid = CAST_DTK_DTKID dl_rec[DTK_DTKID] ;

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
Function:       stats_report_dlk()

Description:    Calls IMS interface to report downlink times of a pass.  
                also report each SAR sensor activity that is each 
                the downlink.  
                Returns TRUE if successfully reported
                Returns FALSE if already reported.  in this case, there
                    won't be any reporting  - not an error.
                Returns < 0 if there was an error.

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 12:11:56 PST 1998

==============================================================================*/
static int stats_report_dlk(
    DBPROCESS       *APS_dbproc,
    DB_RECORD       **dtk_rec,
    char            *condition,         /* REDUCED or CANCELLED    */
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
        "\n%s(%d):  DOWNLINK to report:  \n", __FILE__, __LINE__ ) ;
    dtkm_print( aps_stats_ims_info->logfile_ptr, dtk_rec ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "original downlink times:      %s %s\n",
        CAST_DTK_FA_STRTTIME dtk_rec[DTK_FA_STRTTIME], 
        CAST_DTK_FA_STOPTIME dtk_rec[DTK_FA_STOPTIME] ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "    condition   = %s\n", condition ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "    asftime_now = %s\n", asftime_now) ;

    /*
    -- make a new empty rec:
    -- create the fields for the record, use
    -- it to check for a previous call and
    -- to register the call:
    */
    temp_stats_calls_rec =  new_table_record(APS_CDEFS(STATS_CALLS)) ;
 
    /*   create the fields for the record:  */
    (void) strcpy( CAST_STATS_CALLS_TYPE temp_stats_calls_rec[STATS_CALLS_TYPE],
        "DLK" ) ;

        /* only copy the first 3 chars to the stats_calls.condition field:  */
    (void) strcpy(
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        "   " ) ;
    (void) strncpy(
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        condition, 3 ) ;

    (void) strcpy( CAST_STATS_CALLS_SAT temp_stats_calls_rec[STATS_CALLS_SAT],
        CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;

    CAST_STATS_CALLS_REV temp_stats_calls_rec[STATS_CALLS_REV] 
        = CAST_DTK_REV dtk_rec[DTK_REV] ; 

    CAST_STATS_CALLS_DTKID temp_stats_calls_rec[STATS_CALLS_DTKID] 
        = CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;

    if( strcmp(condition, "CANCELLED") == 0 )
    {
        /* 
        -- on a cancellation, we use the 
        -- original FA times.  
        */
        (void) strcpy( CAST_STATS_CALLS_STRTTIME 
            temp_stats_calls_rec[STATS_CALLS_STRTTIME], 
            CAST_DTK_FA_STRTTIME dtk_rec[DTK_FA_STRTTIME] ) ;
        (void) strcpy( CAST_STATS_CALLS_STOPTIME 
            temp_stats_calls_rec[STATS_CALLS_STOPTIME], 
            CAST_DTK_FA_STOPTIME dtk_rec[DTK_FA_STOPTIME] ) ;
    }

    if( strcmp(condition, "REDUCED") == 0 )
    {
        (void) strcpy( CAST_STATS_CALLS_STRTTIME 
            temp_stats_calls_rec[STATS_CALLS_STRTTIME], 
            CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;

        (void) strcpy( CAST_STATS_CALLS_STOPTIME 
            temp_stats_calls_rec[STATS_CALLS_STOPTIME], 
            CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
    }

    CAST_STATS_CALLS_ANTENNA_ID temp_stats_calls_rec[STATS_CALLS_ANTENNA_ID]
        = CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;

    (void) strcpy( 
        CAST_STATS_CALLS_DTKSTAT temp_stats_calls_rec[STATS_CALLS_DTKSTAT], 
        CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] ) ;

    (void) strcpy( CAST_STATS_CALLS_CALL_TIME
        temp_stats_calls_rec[STATS_CALLS_CALL_TIME], asftime_now ) ;
 
    return_code = stats_call_reported( temp_stats_calls_rec ) ;
    if( return_code < 0 )
    {
        /*   error.   */
        free_db_record(temp_stats_calls_rec) ;
        return return_code ;
    }
    if( return_code == TRUE )
    {
        /* stats call already reported.  */
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
 
    /* not yet reported.  */

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

    (void) strcpy( pmf_values->sat, CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;

    pmf_values->rev = CAST_DTK_REV dtk_rec[DTK_REV] ;

    pmf_values->dtkid = CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;

    /* skip pmf_values->sensor                                 */

    pmf_values->antenna_id = CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;

    /* first 3 chars only:    */
    (void) sprintf(pmf_values->activity_id, "%3.3s", 
        CAST_DTK_ACTID dtk_rec[DTK_ACTID] ) ;

    (void) strcpy( pmf_values->asftime_now, asftime_now ) ;

    /* skip pmf_values->asftime_aos                                 */
    /* skip pmf_values->asftime_los                                 */

    /* provide original FA times if cancelled or reduced:  */
    (void) strcpy( pmf_values->asftime_on_fa, 
        CAST_DTK_FA_STRTTIME dtk_rec[DTK_FA_STRTTIME] ) ;
    (void) strcpy( pmf_values->asftime_off_fa, 
        CAST_DTK_FA_STOPTIME dtk_rec[DTK_FA_STOPTIME] ) ;

    /* skip pmf_values->asftime_mask_entry                          */
    /* skip pmf_values->asftime_mask_exit                           */

    if( strcmp( condition, "REDUCED" ) == 0 )
    {
        /* provide actual FA times if reduced:  */
        (void) strcpy( pmf_values->asftime_on, 
            CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;

        (void) strcpy( pmf_values->asftime_off, 
            CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
    }

    /* skip pmf_values->asftime_planned                             */
    /* skip pmf_values->downlink_rev_char                           */
    /* skip pmf_values->downlink_sequence                           */
    /* skip pmf_values->downlink_status                             */

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
Function:       stats_report_all_downlinks()

Description:    report times of all downlinks that satisfy condition.  

            Times for ASF-cancelled downlinks.
            Retrieve dtks
            where stop time is within the previous day
                and ( dtk.sensor = RDL or dtk.sensor = DMP )
                and dtk.proposed_dtkstat = SCH
                and (   dtk.dtkstat == REJ
                     or dtk.dtkstat == DEL
                     or dtk.dtkstat == CON )
                and dtk.station_id = ASF

            Original & reduced downlink times for ASF-reduced downlinks.
            Retrieve dtks
            where stop time is within the previous day
                and ( dtk.sensor RDL = or dtk.sensor = DMP )
                and dtk.dtkstat = SCH
                and dtk.station_id = ASF
                and asf_reduction_min > .1


Creator:        Lawrence Stevens

Creation Date:  Tue Jan 20 16:33:30 PST 1998

==============================================================================*/
int stats_report_all_downlinks(
    DBPROCESS       *APS_dbproc,
    llist           *dtk_list,    /* ASF downlink list, sorted by sat, rev.  */
    char            *condition,   /* CANCELLED or REDUCED    */
    APS_STATS_IMS_INFO   *aps_stats_ims_info,
    int             *call_count ) 
{

    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr ;

    int         return_code ;

    llist       *obs_dtk_list ;
    DB_RECORD   **obs_dtk_rec ;
    cursor      obs_dtk_list_ptr ;


    *call_count = 0 ;

    /* error checking.  */
    if( strcmp( condition, "REDUCED" ) != 0
    &&  strcmp( condition, "CANCELLED" ) != 0  )
        return STATS_ERROR_CONDITION_ARG ;
 
    /*
    -- find downlinks to report.  
    --
    -- REDUCED:  
    --     find downlinks whose times have been reduced from 
    --     the times given by Flight Agencies.  
    --     indicated by dtk.dtkstat = SCH and dtk.asf_reduction_min > 0.0
    -- 
    -- CANCELLED:  
    --     find downlinks sheduled by FA but cancelled by ASF.  
    --     indicated by dtk.proposed_dtkstat = SCH and 
    --     ( dtk.dtkstat = REJ or dtk.dtkstat = DEL or dtk.dtkstat = CON  )
    */

    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)
        )
    {
        /* process the current dtk_rec right here.  */
        if( strcmp( condition, "REDUCED" ) == 0 )
        {
            /*
            --     find downlinks whose times have been reduced from 
            --     the times given by Flight Agencies.  
            --     indicated by dtk.dtkstat = SCH and 
            --     dtk.asf_reduction_min > 0.0
            --     Skip any other downlinks.  
            */
            if( stats_dtk_is_reduced( dtk_rec ) != TRUE )
                continue ; /* skip dtk:  not reduced.    */
        }
        else
        {
            /*
            --     find downlinks sheduled by FA but CANCELLED by ASF.  
            --     indicated by dtk.proposed_dtkstat = SCH and 
            --     ( dtk.dtkstat = REJ or dtk.dtkstat = DEL 
            --       or dtk.dtkstat = CON  )
            --     Skip any other downlinks.  
            */
            if( stats_dtk_is_cancelled( dtk_rec ) != TRUE )
                continue ; /* skip dtk:  not cancelled   */
        }

        /*
        -- this downlink made it through.
        -- report the data for this downlink.  
        */

        return_code = stats_report_dlk( APS_dbproc,
            dtk_rec, condition,  aps_stats_ims_info ) ;
        if( return_code < 0 )
            return return_code ;
        /* return_code = 1 if reported, 0 if not:  */
        (*call_count) += return_code ;

        /********************************************************************
        *                                                                   *
        *    REPORT EACH SAR SENSOR OBSERVATION FOR THIS DOWNLINK           *
        *                                                                   *
        ********************************************************************/
        obs_dtk_list = create_dyn_llist() ;

        /* get the observations on this downlink:  */
        return_code = dtkm_dl2obs( dtk_rec, obs_dtk_list ) ;
        if( return_code < 0 )
        {
            DEL_LIST( obs_dtk_list ) ;
            return return_code ;
        }

        if( NUMELTS( obs_dtk_list ) <= 0 )
        {
            DEL_LIST( obs_dtk_list ) ;
            continue ;   /* no observations at all, skip to next downlink.  */
        }

        for(obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, obs_dtk_list_ptr);
            obs_dtk_rec ;
            obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, obs_dtk_list_ptr)  
           )
        {
            /* process the current obs_dtk_rec right here.  */
            if( *(CAST_DTK_SENSOR obs_dtk_rec[DTK_SENSOR]) != 'S' )
                continue ;   /* not a SAR sensor.  */ 

            /* this SAR data-take was for the past.  */
            return_code = stats_report_sar_past( APS_dbproc,
                dtk_rec, obs_dtk_rec, condition,  aps_stats_ims_info ) ;
            if( return_code < 0 )
            {
                DEL_LIST( obs_dtk_list ) ;
                return return_code ;
            }
        }
        DEL_LIST( obs_dtk_list ) ;

    }

    return TRUE ;

}
