#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_report_sar_pln.c

Description:    Calls IMS interface to report a planned SAR data-take.  

==============================================================================*/
#pragma ident   "@(#)stats_report_sar_pln.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_report_sar_pln.c"


/*==============================================================================
Function:       stats_report_sar_pln()

Description:    Calls IMS interface to report a planned SAR data-take.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 12:11:56 PST 1998

==============================================================================*/
#include <string.h>            /* for strcpy()                      */
#include <stdlib.h>            /* for malloc() and free()           */
#include <timeconv.h>          /* for tc_systime2asf() etc.         */
#include <dapps_defs.h>      /* for ASF_TIME_STR_LENGTH           */
#include "aps_Statistics.h"
#include <dtkm_utilities.h>    /* for dtkm_print()   */
int stats_report_sar_pln( 
    DBPROCESS       *APS_dbproc, 
    DB_RECORD       **dtk_rec,
    APS_STATS_IMS_INFO   *aps_stats_ims_info ) 
{
    int         return_code ;
    int         nrecs_inserted ;
    char        asftime_now[ASF_TIME_STR_LENGTH+1] ;
    DB_RECORD   **temp_stats_calls_rec ;

    APS_STATS_PMF_VALUES   *pmf_values ;

    (void) tc_systime2asf(asftime_now) ;

    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n----------------------------------------------------------------" ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  SAR Activity to report:  \n", __FILE__, __LINE__ ) ;
    dtkm_print( aps_stats_ims_info->logfile_ptr, dtk_rec ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "    condition   = PLANNED\n" ) ;
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
        "SAR" ) ;
 
    (void) strcpy(
        CAST_STATS_CALLS_CONDITION temp_stats_calls_rec[STATS_CALLS_CONDITION],
        "PLN" ) ;
 
    (void) strcpy( CAST_STATS_CALLS_SAT temp_stats_calls_rec[STATS_CALLS_SAT],
        CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
 
    CAST_STATS_CALLS_REV temp_stats_calls_rec[STATS_CALLS_REV]
        = CAST_DTK_REV dtk_rec[DTK_REV] ;
 
    CAST_STATS_CALLS_DTKID temp_stats_calls_rec[STATS_CALLS_DTKID]
        = CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;
 
    (void) strcpy(
        CAST_STATS_CALLS_STRTTIME temp_stats_calls_rec[STATS_CALLS_STRTTIME],
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;
 
    (void) strcpy(
        CAST_STATS_CALLS_STOPTIME temp_stats_calls_rec[STATS_CALLS_STOPTIME],
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
 
    CAST_STATS_CALLS_ANTENNA_ID temp_stats_calls_rec[STATS_CALLS_ANTENNA_ID]
        = 0 ;
 
    (void) strcpy(
        CAST_STATS_CALLS_DTKSTAT temp_stats_calls_rec[STATS_CALLS_DTKSTAT],
        CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT] ) ;
 
    (void) strcpy( CAST_STATS_CALLS_CALL_TIME
        temp_stats_calls_rec[STATS_CALLS_CALL_TIME], asftime_now ) ;
 
    return_code = stats_call_reported( temp_stats_calls_rec ) ;
    if( return_code < 0 )
    {
        /* error  */
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
 
    /* not yet reported.  report now:  */

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
        free_db_record(temp_stats_calls_rec ) ;
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
 
    (void) strcpy( pmf_values->sensor, CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) ;
 
    /*  skip  pmf_values->antenna_id_char             */
    /*  skip  pmf_values->activity_id                 */

    (void) strcpy( pmf_values->asftime_now, asftime_now ) ;

    /*  skip  pmf_values->asftime_aos         */
    /*  skip  pmf_values->asftime_los         */
    /*  skip  pmf_values->asftime_aos_fa      */
    /*  skip  pmf_values->asftime_los_fa      */
    /*  skip  pmf_values->asftime_mask_entry  */
    /*  skip  pmf_values->asftime_mask_exit   */

    (void) strcpy( pmf_values->asftime_on, 
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;
    (void) strcpy( pmf_values->asftime_off, 
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;

    /*  skip  pmf_values->asftime_on_fa       */
    /*  skip  pmf_values->asftime_off_fa      */
    /* skip pmf_values->asftime_planned       */
    /*  skip  pmf_values->downlink_rev_char   */
    /*  skip  pmf_values->downlink_sequence   */
    /*  skip  pmf_values->downlink_status     */

    (void) strcpy( pmf_values->asftime_planned, 
        CAST_DTK_DTKDATE dtk_rec[DTK_DTKDATE] ) ;

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
 
    /* record the successful call.  */
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
