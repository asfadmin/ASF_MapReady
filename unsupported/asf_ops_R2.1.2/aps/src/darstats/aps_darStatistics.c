#undef RUN_FROM_GUI

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_darStatistics.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          This file was written with a 4-character tab setting.
                If you don't use 4-character tabs, it will look funny.
                Use set tabstop=4 in vi to browse.  

==============================================================================*/
#pragma ident   "@(#)aps_darStatistics.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/darstats/SCCS/s.aps_darStatistics.c"

#include "db_sybint.h"      /* for APS sybase interface routines.   */
#include "aps_db_table.h"   /* for DTK - accessing dtk table.       */
#include "aps_defs.h"       /* for APSDB_ENVVAR and APS_USERID_ENVVAR   */
#include "db_dtk.h"         /* for APS db relation dtk.             */
#include "timeconv.h"      /* for tc_systime2asf() etc.             */
#include "dtkm_utilities.h" /* for dtkm_print() etc.             */
#include "DARconversions.h" /* for table_lookupAPS2IMS()            */

#include "aps_log_msg.h"    /* for aps_log_msg() stuff and MSG_LEN   */

#include "aps_darStatistics.h"   /* for IMS_MSG_STRUCT  etc.   */



/*==============================================================================
Function:       aps_darStatistics()

Description:    function that does DAR statistics.  It obtains the stats 
                from the APS DB, then writes the stats to the IMS DB.  

Creator:        Lawrence Stevens

Creation Date:  Wed Feb 21 17:44:33 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int 
aps_darStatistics(
    char        *progname,
    char        *ims_username,
    char        *ims_password,
    char        *today_dtkdate  )  /* input.  look at previous date's dtks */
{

    int         return_code ;
    int         ret_value;
    char        msg[MSG_LEN];
    short       item_id = 1;         /*  a constant.  always 1.   */
    char*       IMS_dtk_status ;

    char        dar_timeStamp[]     = "yyyy:ddd:hh:mm:ss.sss";
    char        today_asftime[]     = "yyyy:ddd:12:00:00.000";
    char        yesterday_asftime[] = "yyyy:ddd:hh:mm:ss.sss";
    char        yesterday_dtkdate[] = "yyyy:ddd";

    IMS_MSG_STRUCT *msgDesc;

    int         ERROR_dtks_count = 0 ;
    int         OK_queries_count = 0 ;
    int         ERROR_queries_count = 0 ;

    double      timediff_days ;
    int         current_darid ;
    char        current_darid_dtkstat[4] ;

    int         previous_darid ;
    char        previous_darid_dtkstat[4] ;
    int         previous_darid_seconds ;

    DB_RECORD   **dtk_rec ;
    llist       *dtk_list = NULL ;
    cursor      dtk_list_ptr ;

    IMS_XLAT_TBL dar_statistics_status_table[] =  
    {
        { "SUBMITTED", "SUB" },
        { "PLANNED",   "PLN" },
        { NULL,        NULL  }
    } ;

    /*
    -- Allocate message facility structure.
    */
    msgDesc = ims_msgStructAlloc ();
    if ( msgDesc == (IMS_MSG_STRUCT *) NULL )
    {
        /* pop up window message */
        sprintf(msg, 
            "IMS message structure could not be allocated." ) ;
#ifdef  RUN_FROM_GUI
        fprintf(stderr, "%s:\n\n%s\n", progname, msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

    /*
    -- retrieve yesterday's data-takes; all those with 
    -- dtkdate = yyyy:ddd corresponding to yesterday.  
    */
    /* for the call, concatenate today_dtkdate with a 12 noon time.  */
    strncpy( today_asftime, today_dtkdate, strlen(today_dtkdate) ) ;
    return_code = tc_validate_asf_datetime( today_asftime ) ;
    if ( return_code != TRUE )
    {
        /* pop up window message */
        sprintf(msg, "Illegal input dtkdate:  %s", today_dtkdate ) ;
#ifdef  RUN_FROM_GUI
        fprintf(stderr, "%s:\n\n%s\n", progname, msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

    return_code = tc_asf_add_ndays( today_asftime, -1.0, yesterday_asftime ) ;

    strncpy( yesterday_dtkdate, yesterday_asftime, 8 ) ;

    /* retrieve non-downlinks, only sensing activities:  */
    /* 
    -- note modification to db:  dtk.dtkdate is 21 char asf time now, 
    -- instead of 8.  The where_clause now looks for a match 
    -- at the start of the dtkdate field, using the "like" SQL keyword 
    -- with the "%" wildcard at the end of the string.  
    -- Thu Aug  7 12:04:47 PDT 1997
    */
    sprintf( where_clause, 
"where %s != '%s' and %s != '%s' and %s != '%s' and %s like '%s%%' and (  %s = '%s' or %s = '%s' )", 
        APS_COL(DTK, DTK_SAT), "A1",
        APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
        APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,
        APS_COL(DTK, DTK_DTKDATE), yesterday_dtkdate,
        APS_COL(DTK, DTK_DTKSTAT), "SUB",
        APS_COL(DTK, DTK_DTKSTAT), "PLN" ) ;

    sprintf( orderby_cols, "%s, %s", 
        APS_COL(DTK, DTK_DARID), 
        APS_COL(DTK, DTK_DTKSTAT) ) ;

    /*
    -- order the records by darid.  
    */
    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS) ;

    /* time stamp this moment of the retrieve and use it later:  */
    tc_systime2asf( dar_timeStamp );

    if ( dtk_list == NULL )
    {
        sprintf(msg, 
            "%s(%d):  Error in Sybase query on data-takes %s order by %s", 
            __FILE__, __LINE__, where_clause, orderby_cols ) ;
#ifdef  RUN_FROM_GUI
        fprintf(stderr, "%s:\n\n%s\n", progname, msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }
    if ( NUMELTS(dtk_list) <= 0 )
    {
        sprintf(msg, 
            "No data-takes found for date %s; no data-takes to process.",
            yesterday_dtkdate ) ;
#ifdef  RUN_FROM_GUI
        printf("%s:\n\n%s\n", progname, msg ) ;
#endif  /*  RUN_FROM_GUI   */
        aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
		DEL_ALL (dtk_list) ;
        return APS_EXIT_OK ;
    }

#ifdef DEBUG
    printf("%s(%d):  dtks retrieved:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_list ) ) ;
    dtkm_print_list( stdout, dtk_list ) ;
#endif

    /* set up for dtk group processing. */
    previous_darid = -1 ;
    strcpy(previous_darid_dtkstat, "" ) ;
    previous_darid_seconds = 0 ;

    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
        )
    {
#ifdef DEBUG
        printf("\n%s(%d):  now processing:\n", __FILE__, __LINE__ ) ;
        dtkm_print( stdout, dtk_rec ) ;
#endif
        /* process the current dtk_rec right here.  */
        current_darid = CAST_DTK_DARID dtk_rec[DTK_DARID] ;
        strcpy( current_darid_dtkstat, CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT]) ;

        return_code = tc_et_ASF_datetime_diff(
            CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
            CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
            &timediff_days ) ;

#ifdef DEBUG
        printf("%s(%d):  darid = %d, dtkstat = %s, seconds = %d\n",
            __FILE__, __LINE__, current_darid, current_darid_dtkstat, 
            (int) (0.5 + timediff_days * 24 * 3600 ) ) ;
#endif

        if ( !return_code )
        {
            ERROR_dtks_count ++ ;
            /* error in strttime or stoptime.  */
            sprintf( msg,
"skipping dtk record %s/%s/%5.5ld.%2.2d %s   due to illegal strttime '%s' or stoptime '%s'", 
                CAST_DTK_SAT dtk_rec[DTK_SAT],
                CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                CAST_DTK_REV dtk_rec[DTK_REV],
                CAST_DTK_DTKID dtk_rec[DTK_DTKID],
                CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID],
                CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
                CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;

#ifdef      RUN_FROM_GUI
            printf("%s:  %s\n", progname, msg ) ;
#endif      /*  RUN_FROM_GUI   */

            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            continue ;
        }

        if ( current_darid == previous_darid 
        &&   strcmp(previous_darid_dtkstat, current_darid_dtkstat) == 0 )
        {
            /* 
            -- same darid, dtkstat.  continue to 
            -- accumulate time duration for the 
            -- darid, dtkstat.  
            */
            /* 
            -- accumulate time duration in seconds, 
            -- rounding off:  
            */
            previous_darid_seconds += 0.5 + timediff_days * 24.0 * 60 * 60 ;

        }
        else
        {
            /* 
            -- different darid or dtkstat.  
            -- call write_ims_darStatistics() with the data for 
            -- previous_darid and dtkstat.  then start new accumulation 
            -- for current_darid and dtkstat.  
            */
            if ( previous_darid > 0 )
            {

                IMS_dtk_status = table_lookupAPS2IMS( 
                    dar_statistics_status_table, 
                    previous_darid_dtkstat, NULL ) ;
                if ( IMS_dtk_status == NULL )
                {
                    ERROR_dtks_count ++ ;
                    /* could not translate value for dtkstat. */
                    sprintf( msg,
"skipping dtk recs with darid: %d and untranslatable status: %s", 
                        previous_darid, previous_darid_dtkstat ) ;

#ifdef              RUN_FROM_GUI
                    printf("%s:  %s\n", progname, msg ) ;
#endif              /*  RUN_FROM_GUI   */

                    aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
                }
                else
                {
#ifdef DEBUG
                        printf( 
                        "\n%s(%d):\nCALLING write_ims_darStatistics() with \n",
                            __FILE__, __LINE__ ) ;
                        printf( 
                        "TIME:  %s, DARID = %d, STATUS = %s, SECONDS = %d\n",
                            dar_timeStamp, previous_darid, IMS_dtk_status, 
                            previous_darid_seconds ) ;
#endif
                    ret_value = write_ims_darStatistics(previous_darid, item_id, 
                        IMS_dtk_status, dar_timeStamp, previous_darid_seconds,
                        ims_username, ims_password, progname, msgDesc ) ;

                    if (ret_value != APS_EXIT_OK)
                    { 
                        ERROR_queries_count ++ ;
                        sprintf(msg,
                        "IMS DAR query failed for darid %d  dtkstatus '%s'",
                            previous_darid, previous_darid_dtkstat );

#ifdef                  RUN_FROM_GUI
                        fprintf(stderr, "%s:  %s\n", progname, msg ) ;
#endif                  /*  RUN_FROM_GUI   */

                        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, 
                            DO_PRINT);
                    }
                    else
                    {
                        OK_queries_count ++ ;
#ifdef DEBUG
                        printf( "%s(%d):\nDAR query OK for\n", 
                            __FILE__, __LINE__ ) ;
                        printf( 
                           "TIME = %s, DARID = %d, STATUS = %s, SECONDS = %d\n",
                            dar_timeStamp, previous_darid, IMS_dtk_status, 
                            previous_darid_seconds ) ;
#endif
                    }
                }
            }
            /* 
            -- previous_darid, dtkstat is done.  now start 
            -- accumulations with current_darid, dtkstat.
            */
            previous_darid_seconds = 0.5 + timediff_days * 24.0 * 60 * 60 ;
            previous_darid = current_darid ;
            strcpy( previous_darid_dtkstat, current_darid_dtkstat ) ;
        }
    }

    /* 
    -- no more data-takes.  
    -- call write_ims_darStatistics() with the data for 
    -- previous_darid.  then we are done.  
    */
    if ( previous_darid > 0 )
    {
        IMS_dtk_status = table_lookupAPS2IMS( dar_statistics_status_table, 
            previous_darid_dtkstat, NULL ) ;
        if ( IMS_dtk_status == NULL )
        {
            ERROR_dtks_count ++ ;
            /* could not translate value for dtkstat. */
            sprintf( msg,
"skipping dtk recs with darid: %d and untranslatable status: %s", 
                previous_darid, previous_darid_dtkstat ) ;

#ifdef      RUN_FROM_GUI
            printf("%s:  %s\n", progname, msg ) ;
#endif      /*  RUN_FROM_GUI   */

            aps_log_msg( progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        }
        else
        {
#ifdef DEBUG
            printf( "\n%s(%d):\nCALLING write_ims_darStatistics() with \n",
                __FILE__, __LINE__ ) ;
            printf( "TIME:  %s, DARID = %d, STATUS = %s, SECONDS = %d\n",
                dar_timeStamp, previous_darid, IMS_dtk_status, 
                previous_darid_seconds ) ;
#endif
            ret_value = write_ims_darStatistics(previous_darid, item_id, 
                IMS_dtk_status, dar_timeStamp, previous_darid_seconds,
                ims_username, ims_password, progname, msgDesc ) ;

            if (ret_value != APS_EXIT_OK)
            { 
                ERROR_queries_count ++ ;
                sprintf(msg,"IMS DAR query failed for darid %d  dtkstatus '%s'",
                    previous_darid, previous_darid_dtkstat );

#ifdef          RUN_FROM_GUI
                printf("%s:  %s\n", progname, msg ) ;
#endif          /*  RUN_FROM_GUI   */

                aps_log_msg( progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            }
            else
            {
                OK_queries_count ++ ;
#ifdef DEBUG
                        printf( "%s(%d):\nDAR query OK for\n", 
                            __FILE__, __LINE__ ) ;
                        printf( 
                           "TIME = %s, DARID = %d, STATUS = %s, SECONDS = %d\n",
                            dar_timeStamp, previous_darid, IMS_dtk_status, 
                            previous_darid_seconds ) ;
#endif
            }
        }
    }

    /* 
    -- previous_darid is done.  
    -- now clean up and terminate.  
    */

    ims_msgStructFree (msgDesc);
	DEL_ALL (dtk_list) ;

    if ( ERROR_dtks_count || ERROR_queries_count )
    {
        aps_log_msg(progname, APS_INFO,  
"Program will terminate abnormally due to previously noted errors in data-takes or IMS DAR queries.", 
            DO_SYSLOG, DO_PRINT);
        return APS_EXIT_ERROR ;
    }

    return APS_EXIT_OK ;

} /* main */
