#undef RUN_FROM_GUI

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_Statistics.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          This file was written with a 4-character tab setting.
                If you don't use 4-character tabs, it will look funny.
                Use set tabstop=4 in vi to browse.  

==============================================================================*/
#pragma ident   "@(#)aps_Statistics.c	1.3 98/03/19 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.aps_Statistics.c"

#include <stdlib.h>         /* for getenv()                         */
#include <string.h>         /* for strncpy() etc.                   */
#include <db_sybint.h>      /* for APS sybase interface routines.   */
#include "aps_db_table.h"   /* for DTK - accessing dtk table.       */
#include "aps_defs.h"       /* for APSDB_ENVVAR and APS_USERID_ENVVAR   */
#include "db_dtk.h"         /* for APS db relation dtk.             */
#include "timeconv.h"      /* for tc_systime2asf() etc.             */
#include "dtkm_utilities.h" /* for dtkm_print() etc.             */
#include "DARconversions.h" /* for table_lookupAPS2IMS()            */

#include "aps_log_msg.h"    /* for aps_log_msg() stuff and MSG_LEN   */
#include "aps_Statistics.h" /* for application function prototypes   */



/*==============================================================================
Function:       aps_Statistics()

Description:    function that does APS statistics.  It obtains the stats 
                from the APS DB, then calls a function to report the 
                stats to the IMS.  

    The statistics are to help track dish usage and data quantities 
    received.  In addition, dish usage and data losses due to 
    conflicts are also to tracked.  

    SCH      :   dtk was scheduled by ASF and was to take place.  
    REDUCED  :   SCH dtk was reduced by ASF due to conflict or other reason.
    CANCELLED:   FA-scheduled dtk was cancelled by ASF.  

    What to report:                     SCH   CANCELLED    REDUCED B&A   
                                               BY ASF
    mask entry/exit times 10 meter       x       x               
    AOS/LOS times 11 meter               x       x             x
    dtk start/stop times                         x             x

    B&A:  Before and After:  this means to report the Before reduction 
          and After reduction times.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 18 17:46:13 PST 1997

==============================================================================*/
int 
aps_Statistics(
    DBPROCESS   *APS_dbproc,
    APS_STATS_IMS_INFO *aps_stats_ims_info,
    int         keep_flag,       /* TRUE:  keep all old stats_calls recs */
    char        *today_dtkdate,  /* input "yyyy:ddd" use as today.       */
    int         n_lookback_days, /* no. of days to look back for dtks.   */
    int         *IMS_error_count, /* output count of IMS errors.          */
    int         *IMS_fatal_count) /* output count of IMS fatal erros.     */
{

    int         return_code ;
    int         nrecs_deleted ;

    char        msg[MSG_LEN];

    char        today_asftime[ASF_TIME_STR_LENGTH+1] = "yyyy:ddd:12:00:00.000";
    char        yesterday_asftime[ASF_TIME_STR_LENGTH+1] 
                    = "yyyy:ddd:hh:mm:ss.sss";
    char        yesterday_dtkdate[] = "yyyy:ddd";
    char        look_back_asftime[ASF_TIME_STR_LENGTH+1] 
                    = "yyyy:ddd:hh:mm:ss.sss";
    char        look_back_dtkdate[] = "yyyy:ddd";
    char        asftime_delete[ASF_TIME_STR_LENGTH+1] ;

    llist       *dtk_list = NULL ;
    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr ;

    int         ERROR_count = 0 ;
    int         count_SCH_mask = 0 ;
    int         count_CAN_mask = 0 ;
    int         count_SCH_aos = 0 ;
    int         count_CAN_aos = 0 ;
    int         count_RED_aos = 0 ;
    int         count_CAN_dlk = 0 ;
    int         count_RED_dlk = 0 ;
    int         count_PLN_sar = 0 ;


    *IMS_error_count = 0 ;
    *IMS_fatal_count = 0 ;

    /*******************************************************************
    *                                                                  *
    *    PAST DOWNLINKS.                                               *
    *    Retrieve ASF downlinks, completed yesterday or before,        *
    *    back to and including the look-back day, into linked          *
    *    list.  Work from linked list to avoid multiple searches       *
    *    thru entire dtk relation.                                     *
    *                                                                  *
    *******************************************************************/

    /*
    -- retrieve downlinks; all those with stoptime within 
    -- 00:00:00.000 on the look-back day up to the end of 
    -- yesterday.  these data-takes are to be checked.  
    */

    /* 
    -- to obtain yesterday's date, concatenate today_dtkdate 
    -- with a 12 noon time, and subtract 1 day.  
    */
    (void) strncpy( today_asftime, today_dtkdate, strlen(today_dtkdate) ) ;
    return_code = tc_validate_asf_datetime( today_asftime ) ;
    if ( return_code != TRUE )
    {
        /* pop up window message */
        (void) sprintf(msg, "Illegal input dtkdate:  %s", today_dtkdate ) ;
#ifdef  RUN_FROM_GUI
        (void) fprintf(stderr, "%s:\n\n%s\n", aps_stats_ims_info->programName, 
            msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(aps_stats_ims_info->programName, APS_CRITICAL, msg, 
            DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

    /* 
    -- the date is valid.  get the previous date  
    -- by subtracting 1 day.  
    */
    return_code = tc_asf_add_ndays( today_asftime, -1.0, yesterday_asftime ) ;
    (void) strncpy( yesterday_dtkdate, yesterday_asftime, 8 ) ;


    /* 
    -- to obtain look-back date, subtract n_lookback_days 
    -- from today_asftime.  
    */
    return_code = tc_asf_add_ndays( today_asftime, - n_lookback_days, 
        look_back_asftime ) ;
    (void) strncpy( look_back_dtkdate, look_back_asftime, 8 ) ;

    /* 
    -- retrieve downlinks:  
    -- Retrieve dtks where stop time is <= yesterday----:23:59:59.999 
    --               and   stop time is >= look-back-day:00:00:00.000
    --               and   station_id = ASF
    --               and   ( sensor = RDL or sensor = DMP)
    --               and   notes not like '%FA-CANCELLED%'
    -- If a dtk was cancelled by FA, then do not look at it.  
    */
    (void) sprintf( where_clause, 
"where %s <= '%s:23:59:59.999' and %s >= '%s:00:00:00.000' and %s = '%s' and ( %s = '%s' or %s = '%s' ) and %s not like '%%FA-CANCELLED%%'", 
        APS_COL(DTK, DTK_STOPTIME),   yesterday_dtkdate,
        APS_COL(DTK, DTK_STOPTIME),   look_back_dtkdate,
        APS_COL(DTK, DTK_STATION_ID), "ASF", 
        APS_COL(DTK, DTK_SENSOR),     DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
        APS_COL(DTK, DTK_SENSOR),     DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,
        APS_COL(DTK, DTK_NOTES)     ) ;

    (void) sprintf( orderby_cols, "%s, %s", 
        APS_COL(DTK, DTK_SAT), 
        APS_COL(DTK, DTK_REV) ) ;

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        (void) sprintf(msg, 
            "%s(%d):  Error in Sybase query on data-takes %s order by %s", 
            __FILE__, __LINE__, where_clause, orderby_cols ) ;
#ifdef  RUN_FROM_GUI
        (void) fprintf(stderr, "%s:\n\n%s\n", aps_stats_ims_info->programName, 
            msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(aps_stats_ims_info->programName, APS_CRITICAL, msg, 
            DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

#ifdef DEBUG
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "%s(%d):  downlinks retrieved:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_list ) ) ;
    dtkm_print_list( stdout, dtk_list ) ;
#endif

    /*
    --  1.  Report mask entry end exit times for SCH passes at ASF 10 meter.
    --      Retrieve UNIQUE sat/rev from dtk
    --      where stop time is within the previous day
    --          and ( dtk.sensor = RDL or dtk.sensor = DMP )
    --          and dtk.dtkstat = SCH
    --          and dtk.station_id = ASF
    --          and dtk.antenna_id = 1
    */
    return_code = stats_report_all_masks( APS_dbproc, dtk_list, "SCHEDULED",
                    aps_stats_ims_info, &count_SCH_mask ) ;
    if( return_code < 0 )
    {
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;

        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;

        ERROR_count ++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
        "\n================> No. of SCHEDULED mask in/out times reported:  %d\n", 
        count_SCH_mask ) ;

    /*
    --  2.  Report mask entry end exit times for SCH passes with cancelled 
    --      dtk at ASF 10 meter.
    --      Retrieve UNIQUE sat/rev from dtk
    --      where stop time is within the previous day
    --          and ( dtk.sensor = RDL or dtk.sensor = DMP )
    --          and dtk.station_id = ASF
    --          and dtk.antenna_id = 1
    --          and (   dtk.dtkstat = REJ
    --               or dtk.dtkstat = DEL
    --               or dtk.dtkstat = CON )
    --          and dtk.proposed_dtkstat = SCH
    */
    return_code = stats_report_all_masks( APS_dbproc, dtk_list, "CANCELLED",
                    aps_stats_ims_info, &count_CAN_mask ) ;
    if( return_code < 0 )
    {
        ERROR_count ++ ;
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;
        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
        "\n================> No. of CANCELLED mask in/out times reported:  %d\n", 
        count_CAN_mask ) ;

    /*
    --  3.  Report AOS/LOS times for SCH passes at ASF 11 meter.
    --      Retrieve UNIQUE sat/rev from dtk
    --      where stop time is within the previous day
    --          and ( dtk.sensor = RDL or dtk.sensor = DMP )
    --          and dtk.dtkstat = SCH
    --          and dtk.station_id = ASF
    --          and dtk.antenna_id = 2
    */
    return_code = stats_report_all_aos_los( APS_dbproc, dtk_list, "SCHEDULED",
                    aps_stats_ims_info, &count_SCH_aos ) ;
    if( return_code < 0 )
    {
        ERROR_count ++ ;
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;
        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
"\n================> No. of SCHEDULED AOS-LOS on 11 meter antenna reported:  %d\n", 
        count_SCH_aos ) ;
 
    /*
    --  4.  Report AOS/LOS times for SCH passes cancelled at ASF 11 meter.
    --      Retrieve UNIQUE sat/rev from dtk
    --      where stop time is within the previous day
    --          and ( dtk.sensor = RDL or dtk.sensor = DMP )
    --          and dtk.proposed_dtkstat = SCH
    --          and (   dtk.dtkstat = REJ
    --               or dtk.dtkstat = DEL
    --               or dtk.dtkstat = CON  )
    --          and dtk.station_id = ASF
    --          and dtk.antenna_id = 2
    */
    return_code = stats_report_all_aos_los( APS_dbproc, dtk_list, "CANCELLED",
                    aps_stats_ims_info, &count_CAN_aos ) ;
    if( return_code < 0 )
    {
        ERROR_count ++ ;
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;
        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
"\n================> No. of CANCELLED AOS-LOS on 11 meter antenna reported:  %d\n", 
        count_CAN_aos ) ;

    /*
    --  5.  Report original & reduced AOS/LOS times for reduced passes
    --      at ASF 11 meter.
    --      Retrieve UNIQUE sat/rev from dtk
    --      where stop time is within the previous day
    --          and ( dtk.sensor = RDL or dtk.sensor = DMP )
    --          and dtk.dtkstat = SCH
    --          and dtk.station_id = ASF
    --          and dtk.antenna_id = 2
    --          and asf_reduction_min > .1
    */
    return_code = stats_report_all_aos_los( APS_dbproc, dtk_list, "REDUCED",
                    aps_stats_ims_info, &count_RED_aos ) ;
    if( return_code < 0 )
    {
        ERROR_count ++ ;
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;
        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
"\n================> No. of REDUCED AOS-LOS on 11 meter antenna reported:  %d\n", 
        count_RED_aos ) ;

    /*
    --  6.  Times for ASF-cancelled downlinks.
    --      Retrieve dtks
    --      where stop time is within the previous day
    --          and ( dtk.sensor = RDL or dtk.sensor = DMP )
    --          and dtk.proposed_dtkstat = SCH
    --          and (   dtk.dtkstat == REJ
    --               or dtk.dtkstat == DEL
    --               or dtk.dtkstat == CON )
    --          and dtk.station_id = ASF
    */
    return_code = stats_report_all_downlinks( APS_dbproc, 
        dtk_list, "CANCELLED", aps_stats_ims_info, &count_CAN_dlk ) ;
    if( return_code < 0 )
    {
        ERROR_count ++ ;
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;
        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
        "\n================> No. of CANCELLED downlinks reported:  %d\n", 
        count_CAN_dlk ) ;

    /*
    --  7.  Original & reduced downlink times for ASF-reduced downlinks.
    --      Retrieve dtks
    --      where stop time is within the previous day
    --          and ( dtk.sensor RDL = or dtk.sensor = DMP )
    --          and dtk.dtkstat = SCH
    --          and dtk.station_id = ASF
    --          and asf_reduction_min > .1
    */
    return_code = stats_report_all_downlinks( APS_dbproc, dtk_list, "REDUCED",
                    aps_stats_ims_info, &count_RED_dlk ) ;
    if( return_code < 0 )
    {
        ERROR_count ++ ;
        (void) fprintf(aps_stats_ims_info->logfile_ptr,
            "%s(%d):  %s\n", __FILE__, __LINE__, 
            STATS_ERROR_MESSAGE(return_code) ) ;
        if( return_code == STATS_ERROR_IMS_FATAL )
            (*IMS_fatal_count)++ ;

        if( return_code == STATS_ERROR_IMS_ERROR 
        ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
            (*IMS_error_count)++ ;
    }

    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
        "\n================> No. of REDUCED downlinks reported:  %d\n", 
        count_RED_dlk ) ;

    /* the list is done being used; free it.  */
    DEL_LIST( dtk_list ) ;

    /*********************************************************************
    *                                                                    *
    *      FUTURE SAR DATA-TAKES (observations)                          *
    *                                                                    *
    *********************************************************************/

    /*
    --      Data-take times & planning time stamp for SAR PLN dtk recs.
    --      Retrieve dtks
    --      where dtk.dtkdate is within the previous day or the look-back 
    --      day or in between.  
    --          and dtk.dtkstat = PLN
    --          and dtk.sensor like S%%
    */
    (void) sprintf( where_clause, 
"where %s <= '%s:23:59:59.999' and %s >= '%s:00:00:00.000' and %s = 'PLN' and %s like 'S%%%%' ", 
        APS_COL(DTK, DTK_DTKDATE),   yesterday_dtkdate,
        APS_COL(DTK, DTK_DTKDATE),   look_back_dtkdate,
        APS_COL(DTK, DTK_DTKSTAT), 
        APS_COL(DTK, DTK_SENSOR)  ) ;

    /* 
    -- sort to collect same-sensor mode 
    -- data-takes together.  
    */
    (void) sprintf( orderby_cols, "%s, %s, %s", 
        APS_COL(DTK, DTK_SAT), 
        APS_COL(DTK, DTK_SENSOR),
        APS_COL(DTK, DTK_REV) ) ;

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;
    if ( dtk_list == NULL )
    {
        (void) sprintf(msg, 
            "%s(%d):  Error in Sybase query on data-takes %s order by %s", 
            __FILE__, __LINE__, where_clause, orderby_cols ) ;
#ifdef  RUN_FROM_GUI
        (void) fprintf(stderr, "%s:\n\n%s\n", aps_stats_ims_info->programName, 
            msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(aps_stats_ims_info->programName, APS_CRITICAL, msg, 
            DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

#ifdef DEBUG
    (void) fprintf(aps_stats_ims_info->logfile_ptr,
        "%s(%d):  data-takes retrieved:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_list ) ) ;
    dtkm_print_list( stdout, dtk_list ) ;
#endif

    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)
        )
    {
        /* process the current dtk_rec right here.  */
        return_code = stats_report_sar_pln( APS_dbproc,
            dtk_rec, aps_stats_ims_info ) ;
        if( return_code < 0 )
        {
            if( return_code == STATS_ERROR_IMS_FATAL )
                (*IMS_fatal_count)++ ;

            if( return_code == STATS_ERROR_IMS_ERROR 
            ||  return_code == STATS_ERROR_IMS_UNKNOWN_RETURN_CODE )
                (*IMS_error_count)++ ;

            DEL_LIST( dtk_list ) ;
            return return_code ;
        }
        if( return_code == TRUE ) 
            count_PLN_sar ++ ;
    }
    DEL_LIST( dtk_list ) ;
    (void) fprintf(aps_stats_ims_info->logfile_ptr, 
        "\n================> No. of PLANNED SAR activities reported:  %d\n", 
        count_PLN_sar ) ;

    if( keep_flag == TRUE )
    {
        (void) fprintf( aps_stats_ims_info->logfile_ptr,
            "\n%s(%d):  keep_flag == TRUE:\n", __FILE__, __LINE__ ) ;  
        (void) fprintf( aps_stats_ims_info->logfile_ptr,
            "Keeping ALL old stats_calls records.\n" ) ;
        (void) fprintf( aps_stats_ims_info->logfile_ptr,
            "returning APS_EXIT_OK\n" ) ;
        return APS_EXIT_OK ;
    }

    /*******************************************************************
    *                                                                  *
    *   DELETE OLD RECORDS FROM THE stats_calls RELATION               *
    *                                                                  *
    *******************************************************************/
    /* 
    -- old stats_calls are no longer needed to detect previous reporting.  
    -- Delete all records with stoptime more than 30 days before the 
    -- lookback time.  Use look_back_asftime - 30 days. 
    */
    return_code = tc_asf_add_ndays( look_back_asftime, -30.0, asftime_delete ) ;
    (void) sprintf( where_clause, "where %s < '%s' ", 
        APS_COL(STATS_CALLS, STATS_CALLS_STOPTIME),   asftime_delete ) ;
    nrecs_deleted = db_delete_records(APS_dbproc, APS_TABLE(STATS_CALLS), 
        where_clause ) ;
    if( nrecs_deleted < 0 )
    {
        (void) sprintf(msg, 
            "%s(%d):  Error in Sybase deletion on stats_calls %s", 
            __FILE__, __LINE__, where_clause ) ;
#ifdef  RUN_FROM_GUI
        (void) fprintf(stderr, "%s:\n\n%s\n", aps_stats_ims_info->programName, 
            msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(aps_stats_ims_info->programName, APS_CRITICAL, msg, 
            DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  Deleted  %d old stats_calls records %s\n", 
        __FILE__, __LINE__, nrecs_deleted, where_clause ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  New activities reported:  %d\n", __FILE__, __LINE__,
        (count_SCH_mask 
        + count_CAN_mask 
        + count_SCH_aos 
        + count_CAN_aos 
        + count_RED_aos 
        + count_CAN_dlk 
        + count_RED_dlk 
        + count_PLN_sar )  ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Scheduled Mask times   :  %d\n", count_SCH_mask ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Cancelled Mask times   :  %d\n", count_CAN_mask ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Scheduled AOS-LOS times:  %d\n", count_SCH_aos ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Cancelled AOS-LOS times:  %d\n", count_CAN_aos ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Reduced AOS-LOS times  :  %d\n", count_RED_aos ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Cancelled Downlinks    :  %d\n", count_CAN_dlk ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Reduced Downlinks      :  %d\n", count_RED_dlk ) ;

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "Planned SAR activities :  %d\n", count_PLN_sar ) ;

    if( ERROR_count > 0 )
    {
        (void) fprintf( aps_stats_ims_info->logfile_ptr,
            "\n  ****  Number of ERRORS       :  %d    ****\n", ERROR_count ) ;
        (void) fprintf( aps_stats_ims_info->logfile_ptr,
            "  NOTE:  CHECK LOG FILE ABOVE FOR ERRORS\n" ) ;
        (void) fprintf( aps_stats_ims_info->logfile_ptr,
            "\n%s(%d):  returning APS_EXIT_ERROR\n", __FILE__, __LINE__ ) ;

        return ERROR_count ;
    }

    (void) fprintf( aps_stats_ims_info->logfile_ptr,
        "\n%s(%d):  returning APS_EXIT_OK\n", __FILE__, __LINE__ ) ;

    return APS_EXIT_OK ;

} /* main */
