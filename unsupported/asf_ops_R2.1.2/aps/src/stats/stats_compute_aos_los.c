#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_compute_aos_los.c

Description:    

==============================================================================*/
#pragma ident   "@(#)stats_compute_aos_los.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_compute_aos_los.c"


/*==============================================================================
Function:       stats_compute_aos_los()

Description:    Compute AOS time and LOS time for antenna.  
                AOS/LOS is the time that the ASF dish is capable 
                of receiving data from the satellite, possibly 
                longer than the downlink(s) on/of time bracket, 
                possibly shorter than the tracking time.  

                AOS/LOS, for the ASF purpose, is the downlink(s) 
                time bracket padded by the data-take track padding 
                times found in the antenna relation.  

                How to:  
                1.  Collect the relevant downlinks.  
                2.  Get the earliest start time.  
                3.  Get the latest start time.  
                4.  Pad this time bracket:  

                    10 meter antenna:  pad with 0 seconds on both ends. 
                    11 meter antenna:  pad with 60 seconds before and 
                                       30 seconds after.  
                    [pad times are from the antenna relation 
                     Wed Feb  4 16:29:16 PST 1998 ]


    The "condition" input parameter indicates what downlinks and 
    what downlink times are to be considered.  

    input condition   action
    ---------------   ------

    SCHEDULED         consider only SCH downlinks.  

    REDUCED           downlinks have been reduced.  consider only SCH 
                      downlinks and use the post-reduction times.  (Same 
                      as SCHEDULED).  

    ORIGINAL          downlinks have been reduced.  consider only SCH 
                      downlinks but use only the original 
                      FA times:  dtk.fa_strttime and dtk.fa_stoptime
                      There will be another call (REDUCED) that 
                      uses dtk.strttime and dtk.stoptime.  

    CANCELLED         consider SCH and REJ/DEL/CON (cancelled) downlinks.  
                      For REJ/DEL/CON (cancelled) downlinks, use the 
                      original FA times:  dtk.fa_strttime and dtk.fa_stoptime.


Creator:        Lawrence Stevens

Creation Date:  Wed Feb  4 11:14:41 PST 1998

Notes:          
==============================================================================*/
#include <string.h>             /* for strcmp()                 */
#include <db_antenna.h>         /* for ANTENNA_STATION_ID etc.  */
#include "aps_Statistics.h"  

int stats_compute_aos_los(
    DB_RECORD   **dl_dtk_rec,  /* input downlink, giving sat/rev, etc.        */
    char        *condition,    /* SCHEDULED, CANCELLED, REDUCED, or ORIGINAL  */
    char        *asftime_aos,  /* output AOS time                             */
    char        *asftime_los ) /* output LOS time                             */
{
    char        dtkstat_where_clause[256] ;
    llist       *dtk_list ;
    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr ;

    llist       *antenna_list ;
    DB_RECORD   **antenna_rec ;
    cursor      antenna_list_ptr ;

    char        dtk_strttime[ASF_TIME_STR_LENGTH+1] ;
    char        dtk_stoptime[ASF_TIME_STR_LENGTH+1] ;

    /* initialize to help find earliest start and latest stop times.  */
    char        pass_strttime[ASF_TIME_STR_LENGTH+1] = "3000:001:01:02:03.444" ;
    char        pass_stoptime[ASF_TIME_STR_LENGTH+1] = "1000:001:01:02:03.444" ;

    int         pad_start_seconds ;
    int         pad_stop_seconds ;

    /*
    -- retrieve all the relevant downlinks from the 
    -- dtk relation:  
    */

    if( strcmp( condition, "ORIGINAL" )  == 0 
    ||  strcmp( condition, "SCHEDULED" ) == 0 
    ||  strcmp( condition, "REDUCED" )   == 0  )
    {
        /* retrieve only SCH downlinks.  */
        (void)sprintf( dtkstat_where_clause, "%s = 'SCH'", 
            APS_COL(DTK, DTK_DTKSTAT) ) ;
    }
    else if( strcmp( condition, "CANCELLED" ) == 0 )
    {
        /* 
        -- retrieve SCH, DEL, REJ, and DEL downlinks.  
        -- don't look at any dtks cancelled by FA.  
        */
        (void)sprintf( dtkstat_where_clause, 
"( %s = 'SCH' or ( ( %s = 'DEL' or %s = 'REJ' or %s = 'DEL' ) and %s not like '%%FA-CANCELLED%%' ) ) ", 
            APS_COL(DTK, DTK_DTKSTAT) ,
            APS_COL(DTK, DTK_DTKSTAT) ,
            APS_COL(DTK, DTK_DTKSTAT) ,
            APS_COL(DTK, DTK_DTKSTAT) ,
            APS_COL(DTK, DTK_NOTES)   ) ;
    }
    else
        return STATS_ERROR_CONDITION_ARG ;

    (void) sprintf(where_clause, "where %s = '%s' and %s = %ld and %s = '%s' and ( %s = '%s' or %s = '%s' ) and %s = %d and %s ",
        APS_COL(DTK, DTK_SAT),  CAST_DTK_SAT dl_dtk_rec[DTK_SAT],
        APS_COL(DTK, DTK_REV),  CAST_DTK_REV dl_dtk_rec[DTK_REV],
        APS_COL(DTK, DTK_STATION_ID), 
                                CAST_DTK_STATION_ID dl_dtk_rec[DTK_STATION_ID],
        APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
        APS_COL(DTK, DTK_SENSOR), DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,
        APS_COL(DTK, DTK_ANTENNA_ID), 
                                CAST_DTK_ANTENNA_ID dl_dtk_rec[DTK_ANTENNA_ID],
        dtkstat_where_clause   ) ;

    dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, APS_TABLE(DTK),
        where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if (dtk_list == NULL)
        return STATS_ERROR_DB_ERROR_IN_APSDB_QUERY ;
    if( NUMELTS(dtk_list) == 0 )
    {
        DEL_LIST( dtk_list ) ;
        return STATS_ERROR_NO_DTK_RECS_FOUND_IN_RETRIEVE ;
    }

    /* 
    -- some dtk records were found in the retrieve.  
    -- compute the AOS/LOS.  
    */

    /* 
    -- loop thru all, to get earliest start time and 
    -- latest stop time.  
    */
    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        if( strcmp(condition, "ORIGINAL") == 0 
        ||( strcmp(condition, "CANCELLED") == 0 
            && (   strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],"REJ")
                   == 0
                || strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],"DEL")
                   == 0
                || strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],"CON")
                   == 0     )     )     )
        {
            /* 
            -- the condition is "ORIGINAL"
            -- or:  (the condition is "CANCELLED" and the dtk was cancelled)
            -- therefore, use the original times from the flight agency:  
            */
            (void) strcpy(dtk_strttime,
                CAST_DTK_FA_STRTTIME dtk_rec[DTK_FA_STRTTIME]) ;
            (void) strcpy(dtk_stoptime,
                CAST_DTK_FA_STOPTIME dtk_rec[DTK_FA_STOPTIME]) ;
        }
        else
        {
            /* use the operational times:  */
            (void)strcpy(dtk_strttime, CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME]);
            (void)strcpy(dtk_stoptime, CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME]);
        }

        /* check for earliest start time:  */
        if(         strcmp(pass_strttime, dtk_strttime) > 0 )
            (void) strcpy( pass_strttime, dtk_strttime ) ;

        /* check for latest start time:  */
        if(        strcmp(pass_stoptime, dtk_stoptime) < 0 )
            (void) strcpy(pass_stoptime, dtk_stoptime ) ;
 
    }
    /* we are done with this linked list.  */
    DEL_LIST(dtk_list) ;

    /* 
    -- done.  Now we have the pass start and 
    -- stop times.  need to pad these times for AOS/LOS
    -- find the pad times by checking the antenna relation.  
    */
    (void) sprintf(where_clause, "where %s = '%s' and %s = %d ",
        APS_COL(ANTENNA, ANTENNA_STATION_ID), 
            CAST_DTK_STATION_ID dl_dtk_rec[DTK_STATION_ID],
        APS_COL(ANTENNA, ANTENNA_ANTENNA_ID), 
            CAST_DTK_ANTENNA_ID dl_dtk_rec[DTK_ANTENNA_ID] ) ;

    antenna_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(ANTENNA),
        where_clause, NULL, APS_CDEFS(ANTENNA), ALL_COLS) ;
    if (antenna_list == NULL)
        return STATS_ERROR_DB_ERROR_IN_APSDB_QUERY ;
    if( NUMELTS(antenna_list) == 0 )
    {
        DEL_LIST( antenna_list ) ;
        return STATS_ERROR_NO_ANTENNA_RECS_FOUND_IN_QUERY ;
    }
    if( NUMELTS(antenna_list) > 1 )
    {
        DEL_LIST( antenna_list ) ;
        return STATS_ERROR_GT_1_ANTENNA_RECS_FOUND_IN_QUERY ;
    }

    /* 
    -- some antenna records were found in the retrieve.  
    -- get the padding times and use then to pad.  
    */

    antenna_rec = (DB_RECORD **) FIRST(antenna_list, antenna_list_ptr);
    pad_start_seconds = CAST_ANTENNA_PRE_DTK_TRACK_PAD_SEC 
                 antenna_rec[ANTENNA_PRE_DTK_TRACK_PAD_SEC] ;
    pad_stop_seconds  = CAST_ANTENNA_POST_DTK_TRACK_PAD_SEC 
                 antenna_rec[ANTENNA_POST_DTK_TRACK_PAD_SEC] ;

    /* when passing the pad time, convert to days.  */
    (void) tc_asf_add_ndays( pass_strttime, 
        (-1.0 * pad_start_seconds/60.0/60.0/24.0 ), asftime_aos ) ;
    (void) tc_asf_add_ndays( pass_stoptime, 
        (       pad_stop_seconds/60.0/60.0/24.0 ), asftime_los ) ;

    DEL_LIST(antenna_list) ;

    return TRUE ;

}
