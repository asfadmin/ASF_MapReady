#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_check_values.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_values.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_values.c"

#include <string.h>       /* for strcmp, strncmp argument checks  */
#include <math.h>         /* for fabs, absolute value...          */
#include <stdio.h>        /* for fprintf etc...                   */

#include "dtkm.h"
#include "timeconv.h"   /* for tc_validate_asf_datetime()  */

/* FOR DATABASE TABLES        */
#include "db_activ_conf.h"              /* for activ_conf table      */
#include "db_activities.h"              /* for activities table      */
#include "db_antenna.h"                 /* for db table         */
#include "db_antenna_pref.h"            /* for db table         */
#include "db_dtk.h"                     /* for dtk table             */
#include "db_sat_inclusion_period.h"    /* for db table         */
#include "db_satsensor.h"               /* for db table         */
#include "db_station.h"                 /* for db table         */

#include "phase_utilities.h"   /* for check_rev_asftimes    */

/* FOR LAT/LON FIELDS        */
#include "check_lat_lon.h"     /* for check_latitude() and check_longitude() */



/*==============================================================================
Function:       dtkm_check_dtkid()

Description:    check dtkid.  if > 0, it MUST exist.  

Creator:        Lawrence Stevens

Creation Date:  Thu May  8 18:57:51 PDT 1997

Notes:          
==============================================================================*/
static int dtkm_check_dtkid( DB_RECORD **dtk_rec )
{
    int return_code ;

    if ( CAST_DTK_DTKID dtk_rec[DTK_DTKID] > APS_MAX_DTKID )
        return DTKM_ERROR_DTKID_TOO_BIG ;
    if ( CAST_DTK_DTKID dtk_rec[DTK_DTKID] < 0 )
        return DTKM_ERROR_DTKID_LT_ZERO ;

    if ( CAST_DTK_DTKID dtk_rec[DTK_DTKID] == 0 )
        return TRUE ;

    /* 
    -- dtkid is > 0 ; dtk it is supposed to already 
    -- exist in the database.  
    */

    /* 
    -- This dtk proposal MUST match an existing 
    -- dtk in the db, on all 3 fields!  
    -- primary key( Fri Oct 17 11:57:53 PDT 1997) 
    -- is now sat/rev/dtkid; sensor can be changed.  
    */
    sprintf(where_clause, 
        "where %s = '%s' and %s = %ld and %s = %d",
        APS_COL(DTK, DTK_SAT), CAST_DTK_SAT dtk_rec[DTK_SAT],
        APS_COL(DTK, DTK_REV), CAST_DTK_REV dtk_rec[DTK_REV],
        APS_COL(DTK, DTK_DTKID), CAST_DTK_DTKID dtk_rec[DTK_DTKID]);

    /* 
    -- the where_clause is set up.  now 
    -- count records  
    */
    return_code = db_num_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause ) ;
    if( return_code < 0 )
        return return_code ;

    if( return_code == 0 )
        return DTKM_ERROR_DTK_PROPOSAL_DTKID_GT_ZERO_AND_NOT_IN_DB ;

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_check_dtkstat()

Description:    check the dtk.dtkstat value.  


Creator:        Lawrence Stevens

Creation Date:  Tue Jun 11 12:09:26 PDT 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int
dtkm_check_dtkstat( DB_RECORD   **proposed_dtk )
{

    /*
    -- 18. dtkstat
    */
#ifdef PRINT_DIAG
    PRINT_DIAG(" %s", CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT] ) ;
#endif
    if ( strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "CON" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "REJ" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "DEL" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "QUE" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "SUB" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "PLN" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "SCH" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "REQ" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "REP" ) != 0 )
        return DTKM_ERROR_BAD_DTKSTAT_VALUE ;

    if (*(CAST_DTK_SAT proposed_dtk[DTK_SAT]) == 'A') 
    {
        /* 
        -- special case for ADEOS 
        -- QUE and SUB are not acceptable.  
        */
        if (strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "QUE" ) == 0 
        ||  strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "SUB" ) == 0 )
            return DTKM_ERROR_BAD_DTKSTAT_VALUE;

    }

    if ( dtkm_is_a_tape_dump( proposed_dtk ) == TRUE )
    {
        /* 
        -- this is a tape dump.  ASF does not send out requests 
        -- for tape dumps.  Therefore, QUE and SUB status values 
        -- are never used for tape dumps.  
        */
        if (strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "QUE") == 0 
        ||  strcmp( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], "SUB") == 0)
            return DTKM_ERROR_BAD_DTKSTAT_VALUE;
    }
    return TRUE ;
}



/*==============================================================================
Function:       dtkm_check_agency()

Description:    checks legal values for dtk.actid, the last 3 characters. 

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 17 17:56:56 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int
dtkm_check_agency( DB_RECORD   **proposed_dtk )
{

    if ( (int) strlen( CAST_DTK_ACTID proposed_dtk[DTK_ACTID] ) != 6 )
        return DTKM_ERROR_BAD_ACTID_VALUE ;

    /* 
    -- check for E1 or E2 first.  
    */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "E1" ) == 0  
    ||   strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "E2" ) == 0 )
    {
        if ( strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "ASF" ) == 0 
        ||   strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "ESA" ) == 0 
        ||   strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "ESF" ) == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_ACTID_AGENCY_VALUE ;
    }

    /* 
    -- check for J1 and ADEOS
    */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "J1" ) == 0 
    ||   strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "A1" ) == 0 )
    {
        if ( strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "ASF" ) == 0 
        ||   strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "NAS" ) == 0 
        ||   strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "NSF" ) == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_ACTID_AGENCY_VALUE ;
    }

    /* 
    -- check for R1 
    */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "R1" ) == 0 )
    {
        if ( strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "ASF" ) == 0 
        ||   strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "CSA" ) == 0 
        ||   strcmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID]+3, "CEF" ) == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_ACTID_AGENCY_VALUE ;
    }

    return DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_agency ;

}

/*==============================================================================
Function:       dtkm_check_transid()

Description:    checks legal values for dtk.transid, looking at 
                satellite and activity id.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 28 20:32:27 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
/*   must be GLOBAL, not static:   */
int
dtkm_check_transid( DB_RECORD   **proposed_dtk )
{

    if ( (int) strlen( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID] ) != 2 )
        return DTKM_ERROR_BAD_TRANSID_VALUE ;

    /* 
    -- check for E1 or E2 first.  
    -- transid = "00"  
    */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "E1" ) == 0  
    ||   strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "E2" ) == 0 )
    {
        if ( strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID], "00" ) == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_TRANSID_VALUE ;
    }

    /*  satellite is neither E1 or E2.  */

    /* 
    -- check if an observation, regardless of satellite:  
    -- should have transid = "00"  
    */
    if( dtkm_is_an_observation( proposed_dtk ) == TRUE )
    {
        if ( strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"00") == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_TRANSID_VALUE ;
    }

    /*  
    --  satellite is neither E1 or E2.  
    --  satellite is not an observation.
    --  it is a real-time or a playback (dump)
    */
    /* check J1  */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "J1" ) == 0 )
    {
        if ( strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F1") == 0 
        ||   strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F2") == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_TRANSID_VALUE ;
    }

    /*  
    --  satellite is neither E1 or E2.  
    --  satellite is not an observation.
    --  it is a real-time or a playback (dump)
    --  satellite is not J1
    */

    /* check R1  */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "R1" ) == 0 )
    {
        if ( strncmp( CAST_DTK_ACTID proposed_dtk[DTK_ACTID], "DMP", 3 ) == 0 )
        {
            /* playback must be F4.  */
            if ( strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F4") == 0 )
                return DTKM_FIELD_OK ;
            else
                return DTKM_ERROR_BAD_TRANSID_VALUE ;
        }
        else
        {
            /* real-time must be F3.  */
            if ( strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F3") == 0 )
                return DTKM_FIELD_OK ;
            else
                return DTKM_ERROR_BAD_TRANSID_VALUE ;
        }
    }
    /*  
    --  satellite is neither E1 or E2.  
    --  satellite is not an observation.
    --  it is a real-time or a playback (dump)
    --  satellite is not J1
    --  satellite is not R1
    */

    /* check for A1  */
    if ( strcmp( CAST_DTK_SAT proposed_dtk[DTK_SAT], "A1" ) == 0 )
        if ( strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F5") == 0
        ||   strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F6") == 0 
        ||   strcmp( CAST_DTK_TRANSID proposed_dtk[DTK_TRANSID],"F7") == 0 )
            return DTKM_FIELD_OK ;
        else
            return DTKM_ERROR_BAD_TRANSID_VALUE ;

    /*  
    --  satellite is neither E1 or E2.  
    --  satellite is not an observation.
    --  it is a real-time or a playback (dump)
    --  satellite is not J1
    --  satellite is not R1
    --  satellite is not A1
    */

    return DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_transid ;

}


/*==============================================================================
Function:       dtkm_check_sat_inclusion_times_ok

Description:    checks to see if the the start and stop time interval
                is at least as large as the satellite inclusion period.
                The satellite inclusion period is a time duration.  
                If a data-take is shorter than this time duration, 
                it should not be included into the APS schedule or plans.
                This time period depends on the satellite value, and 
                is at the discretion of ASF mission planning.  

Returns:        
                > 0
                    DTKM_REC_OK

                < 0
                    DTKM_ERROR_COMPUTING_TIME_DIFF
                    DTKM_ERROR_DB_QUERY_FAILED
                    DTKM_ERROR_NO_INCLUSION_PERIOD_FOR_SATELLITE
                    DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD

Creator:        Miguel Siu (adopted by tlm)

Creation Date:  Wed Apr 10 14:03:53 PDT 1996

Notes:          ASSUMES the times in the dtk record are valid asf times.
==============================================================================*/
static int
dtkm_check_sat_inclusion_times_ok ( DB_RECORD   **dtk_rec )
{
    static int      got_inclusion_seconds = FALSE ;

    static cursor   sat_period_list_ptr ;
    static llist    *sat_period_list = NULL;/* a list of sat_inclusion_periods*/

    DB_RECORD   **sat_period_rec = NULL ; /* a sat_inclusion_period record  */

    int         inclusion_seconds ;
    double      delta_days ;    /*days & fraction thereof in the time interval*/
    int         delta_secs ;

    /* 
    -- get the inclusion period, in seconds, for all available satellites.
    -- This value is satellite-dependent and may vary for each
    -- datatake proposal being processed.
    -- We therefore need values for all available satellites,
    -- and we keep the values in memory, for more efficient access.
    */
    if (got_inclusion_seconds == FALSE)
    {
        sat_period_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(SAT_INCLUSION_PERIOD), NULL /*where_clause*/,  NULL, 
            APS_CDEFS(SAT_INCLUSION_PERIOD), ALL_COLS ) ;

        if (sat_period_list == NULL)
            return DTKM_ERROR_DB_QUERY_FAILED ;

        /*
        -- We have the information regarding satellite inclusion times
        -- in the list sat_period_list.  Do not search the database again.
        */
        got_inclusion_seconds = TRUE ;
    }

    /*
    -- calculate the time interval, in ephemeris time
    */
    if (!tc_et_ASF_datetime_diff(CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
                                CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME], 
                                &delta_days) )
    {
        return DTKM_ERROR_COMPUTING_TIME_DIFF ;
    }

    /*  
    --  convert the days into number of seconds
    --  (adding ".5" corrects for the truncation error)
    */
    delta_secs = (delta_days * 24.0 * 60.0 * 60.0) + .5;

    /*
    -- Now, retrieve the inclusion period for this satellite, from
    -- the list sat_period_list which was populated earlier.
    */
    for (
        sat_period_rec = (DB_RECORD **)
            FIRST(sat_period_list, sat_period_list_ptr ) ;
        sat_period_rec ;
        sat_period_rec = (DB_RECORD **) 
            NEXT(sat_period_list, sat_period_list_ptr )
        )
    {
        /* 
        -- seek a match on satellite_id 
        */
        if ( strcmp(CAST_SAT_INCLUSION_PERIOD_SAT 
                        sat_period_rec[SAT_INCLUSION_PERIOD_SAT] ,
                    CAST_SAT_INCLUSION_PERIOD_SAT
                        dtk_rec[SAT_INCLUSION_PERIOD_SAT] ) == 0
           )
        {
            /* a match on SATELLITE_ID */
            inclusion_seconds =  CAST_SAT_INCLUSION_PERIOD_LENGTH 
                        sat_period_rec[SAT_INCLUSION_PERIOD_LENGTH] ;
            /* break out of the loop  */
            break ;
        }
    }

    if (!sat_period_rec)    /* didn't find a match */
        return DTKM_ERROR_NO_INCLUSION_PERIOD_FOR_SATELLITE ;

    if (delta_secs < inclusion_seconds) 
        return DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD ;

    return DTKM_REC_OK ;
}


/*==============================================================================
Function:       dtkm_check_rev_times_overlap

Description:    checks to see if the start time is within the 
                rev, and if the dtk is too long.  Don't check if 
                the stop time is withink the rev.  

Returns:        
    > 0:   TRUE  if the strttime is within the input rev and dtk not too long.
    = 0:   FALSE if not or if some error occurred.  

Creator:        Lawrence Stevens

Creation Date:  Wed Apr 10 14:03:53 PDT 1996

Notes:      
==============================================================================*/
int
dtkm_check_rev_times_overlap ( 
    DB_RECORD   **dtk_rec,
    int         *rev_number_for_strttime,   /* output rev for strttime   */
    char        *strttime_for_rev,          /* output strttime for rev   */
    char        *stoptime_for_rev  )        /* output stoptime for rev   */
{
    int     return_code ;

    /* 
    -- extract values from the dtk_rec to 
    -- the argument list in the call:
    */
    return_code = check_rev_times_overlap( 
        CAST_DTK_SAT dtk_rec[DTK_SAT],
        CAST_DTK_REV dtk_rec[DTK_REV],
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
        rev_number_for_strttime, strttime_for_rev, stoptime_for_rev ) ;

    if ( return_code == PHASE_INPUT_TIME_BETWEEN_PHASES )
        return DTKM_FIELD_OK ;

    return return_code ;
}


/*==============================================================================
Function:       dtkm_check_rev_asftimes

Description:    checks to see if the entire data-take (start and stop times) 
                are within the rev.  

Parameters:     

Returns:        
    > 0:   TRUE  if the strttime and stoptime are both within the input rev.
    = 0:   FALSE if the strttime and stoptime are not.  
    or if some error occurred.  

Creator:        Lawrence Stevens

Creation Date:  Tue Jun 27 13:37:18 PDT 1995

Notes:      
==============================================================================*/
int
dtkm_check_rev_asftimes ( 
    DB_RECORD   **dtk_rec,
    int         *rev_number_for_strttime,   /* output rev for strttime   */
    char        *strttime_for_rev,          /* output strttime for rev   */
    char        *stoptime_for_rev  )        /* output stoptime for rev   */
{
    int     return_code ;

    /* 
    -- extract values from the dtk_rec to 
    -- the argument list in the call:
    */
    return_code = check_rev_asftimes( 
        CAST_DTK_SAT dtk_rec[DTK_SAT],
        CAST_DTK_REV dtk_rec[DTK_REV],
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
        rev_number_for_strttime, strttime_for_rev, stoptime_for_rev ) ;

    if ( return_code == PHASE_INPUT_TIME_BETWEEN_PHASES )
        return DTKM_FIELD_OK ;

    return return_code ;
}


/*==============================================================================
Function:   dtkm_check_station_id_antenna_id

Description:    checks the value of the station_id, sat combination 
against the list of approved values in the station relation.  
Also checks the antenna_id if not zero.  

Parameters:     
    char        *station_id
    char        *sat

Returns:        
    > 0
        DTKM_FIELD_OK

    < 0
        DTKM_ERROR_BAD_SAT_STATION_ID_VALUE
        DTKM_ERROR_DB_QUERY_FAILED

Creator:        Lawrence Stevens

Creation Date:  05/02/1995

Notes:      
==============================================================================*/

int
dtkm_check_station_id_antenna_id ( 
    DB_RECORD   **dtk_rec ) 
{

    int         return_code;

    (void) sprintf(where_clause, "where %s = '%s' and %s = '%s' ",
        APS_COL(STATION, STATION_STATIONID), 
            CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID], 
        APS_COL(STATION, STATION_SAT),
            CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;

#ifdef PRINT_DIAG
    printf("dtkm_check_station_id:  where_clause = \n%s\n", where_clause ) ;
#endif

    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(STATION), where_clause ) ;

    if ( return_code < 0 ) 
        return DTKM_ERROR_DB_QUERY_FAILED ;

    if ( return_code == 0 )
        return DTKM_ERROR_BAD_SAT_STATION_ID_VALUE ;

    /* 
    -- this station id value was seen; it is 
    -- valid:  
    */
    /* a zero value for DTK_ANTENNA_ID right now is OK.  */
    if ( CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] == 0 )
        return TRUE ;

    (void) sprintf(where_clause, "where %s = '%s' and %s = '%s' and %s = %d ",
        APS_COL(ANTENNA_PREF, ANTENNA_PREF_SAT), 
            CAST_DTK_SAT dtk_rec[DTK_SAT], 
        APS_COL(ANTENNA_PREF, ANTENNA_PREF_STATION_ID), 
            CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID], 
        APS_COL(ANTENNA_PREF, ANTENNA_PREF_ANTENNA_ID), 
            CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ) ;

#ifdef PRINT_DIAG
    printf("dtkm_check_station_id:  where_clause = \n%s\n", where_clause ) ;
#endif

    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(ANTENNA_PREF), where_clause ) ;

    if ( return_code < 0 ) 
        return DTKM_ERROR_DB_QUERY_FAILED ;

    if ( return_code == 0 )
        return DTKM_ERROR_BAD_SAT_STATION_ID_ANTENNA_ID_COMBINATION ;

    return TRUE ;
#undef PRINT_DIAG
}

/*==============================================================================
Function:   dtkm_check_satsensor

Description:    validates the satellite and sensor values against the 
satsensor relation.  

Returns:        
    = TRUE   values are OK.  

    < 0
        DTKM_ERROR_BAD_SAT_SENSOR_VALUES
        DTKM_ERROR_DB_QUERY_FAILED

Creator:        Lawrence Stevens

Creation Date:  03/06/1995

Notes:      
==============================================================================*/

static int
dtkm_check_satsensor ( 
    char        *sat,
    char        *sensor ) 
{
    int         return_code;

    (void) sprintf(where_clause, "where %s = '%s' and %s = '%s' ",
        APS_COL(SATSENSOR, SATSENSOR_SAT),     sat, 
        APS_COL(SATSENSOR, SATSENSOR_SENSOR),  sensor ) ;

#ifdef PRINT_DIAG
    printf("dtkm_check_satsensor:  where_clause = \n%s\n", where_clause ) ;
#endif

    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(SATSENSOR), where_clause ) ;

    if ( return_code < 0 ) 
        return DTKM_ERROR_DB_QUERY_FAILED ;

    if ( return_code == 0 )
        return DTKM_ERROR_BAD_SAT_SENSOR_VALUES ;

    /* the values are OK.  */
    return TRUE ;

}


/*==============================================================================
Function:       dtkm_check_values

Description:    minimally checks values in a dtk DBRECORD.  
checks required fields to make sure that something is there.  the 
assumption is that the dtk record is ready to go into the database.  

NOTE:  if the lat/lon location values appear to be unset, the routine 
will fill them by reading from the cvrg relation, written by the 
create_nominal-coverage program.  

NOTE:  the input_dtk is unchanged.  all results go to the result_dtk.  
It is OK if you want to call this routine with both parameters equal to the 
same DB_RECORD; the record will then be checked and updated.  



fields checked:  
---------------
 1. sat
 2. sensor
 3. rev
 3.5 dtkid     [0-APS_MAX_DTKID] OK  !=0:  dtk MUST exist.  
 4. actid
 5. ascdsc
 6. strttime
 7. stoptime
 8. strtlat
 9. stoplat
10. nrlat1
11. nrlon1
12. farlat1
13. farlon1
14. nrlat2
15. nrlon2
16. farlat2
17. farlon2
18. dtkstat     a value of REQ is OK, for checking FA dtk records.  
19. transid
20. station_id 
21. schedule_id 

fields not checked:  
-------------------
 2. fadtkid     can be blank.  
 3. darid       can be zero
 4. lookangl    usually zero
 5. sitename    can be blank
 6. notes       can be blank
 7. dtkdate     the date is set by other functions.  
 8. e1          not used.
 9. j1          not used.
10. r1          not used.

It returns on finding the first invalid field.  

Parameters:
    DB_RECORD   **proposed_dtk ,    proposed data-take      
    DB_RECORD   **result_dtk )      result data-take      

Returns:
    int
    = 0 DTKM_REC_OK

    < 0 ERROR:
        DTKM_ERROR_BAD_SAT_SENSOR_VALUE 
        DTKM_ERROR_BAD_REV_VALUE 
        DTKM_ERROR_BAD_ACTID_VALUE 
        DTKM_ERROR_BAD_ASCDSC_VALUE 
        DTKM_ERROR_SENSOR_AND_ACTID_DONT_MATCH
        DTKM_ERROR_BAD_STRTTIME_VALUE 
        DTKM_ERROR_BAD_STOPTIME_VALUE 
        DTKM_ERROR_BAD_STRTLAT_VALUE 
        DTKM_ERROR_BAD_STOPLAT_VALUE
        DTKM_ERROR_BAD_NRLAT1_VALUE
        DTKM_ERROR_BAD_NRLAT2_VALU
        DTKM_ERROR_BAD_NRLON1_VALUE
        DTKM_ERROR_BAD_NRLON2_VALUE
        DTKM_ERROR_BAD_FARLAT1_VALUE 
        DTKM_ERROR_BAD_FARLAT2_VALUE 
        DTKM_ERROR_BAD_FARLON1_VALUE 
        DTKM_ERROR_BAD_FARLON2_VALUE 
        DTKM_ERROR_STRTLAT_AND_STOPLAT_EQUAL_ZERO 
        DTKM_ERROR_NRLAT1_AND_NRLAT2_EQUAL_ZERO 
        DTKM_ERROR_NRLON1_AND_NRLON2_EQUAL_ZERO 
        DTKM_ERROR_FARLAT1_AND_FARLAT2_EQUAL_ZERO 
        DTKM_ERROR_FARLON1_AND_FARLON2_EQUAL_ZERO 
        DTKM_ERROR_BAD_TRANSID_VALUE 
        DTKM_ERROR_BAD_SCHEDULE_ID_FOR_R1_DTK 
        DTKM_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE

Creator:    Lawrence Stevens

Creation Date:  03/23/1995

Notes:

==============================================================================*/
int
dtkm_check_values( 
    DB_RECORD   **proposed_dtk , /* proposed data-take  */
    DB_RECORD   **result_dtk )   /* result data-take    */
{
    int return_code ;
    int cvrg_points_errors ;     /* used to help check the lat/lon points */
    int     rev_number_for_strttime ;
    char    strttime_for_rev[ASF_TIME_STR_LENGTH+1] ;
    char    stoptime_for_rev[ASF_TIME_STR_LENGTH+1] ;
    char    strttime_for_mask[ASF_TIME_STR_LENGTH+1] ;
    char    stoptime_for_mask[ASF_TIME_STR_LENGTH+1] ;

    /* 
    -- ERROR checking
    */
    if ( proposed_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    /* 
    -- copy the proposed data-take to the result data-take.  
    -- if any values are added, such as the coverage points, 
    -- they will be in the result_dtk, not the proposed_dtk.  
    */
    return_code = db_copy_record ( APS_CDEFS(DTK), result_dtk, proposed_dtk ) ;

    /*
    --  fields 1 and 2. sat and sensor
    */
#ifdef PRINT_DIAG
    PRINT_DIAG("dtkm_check_values:  \n%s", 
        CAST_DTK_SAT result_dtk[DTK_SAT] ) ;
    PRINT_DIAG("/%s", 
        CAST_DTK_SENSOR result_dtk[DTK_SENSOR] ) ;
#endif

    return_code =  dtkm_check_satsensor( 
        CAST_DTK_SAT    result_dtk[DTK_SAT] ,
        CAST_DTK_SENSOR result_dtk[DTK_SENSOR] )  ;

    /* check for an error  */
    if ( return_code < 0 )
        return return_code ;

    /*
    --  3. rev
    */
#ifdef PRINT_DIAG
    PRINT_DIAG("/%05d", CAST_DTK_REV result_dtk[DTK_REV] ) ;
#endif
    if ( CAST_DTK_REV result_dtk[DTK_REV] <= 0 )
        return DTKM_ERROR_BAD_REV_VALUE ;

    /*
    -- 3.5 dtkid.  
    */
    /*
    -- NOTE:    a value of 0 is not valid in the database, 
    --          but it is valid here; the software, when it 
    --          encounters the zero value, will create a valid dtkid
    --          value when the dtk relation is updated.  
    */
    return_code = dtkm_check_dtkid(result_dtk) ;
    if( return_code < 0 )
        return return_code ;

    /*
    --  4. actid
    */
#ifdef PRINT_DIAG
    PRINT_DIAG("/%s", 
        CAST_DTK_ACTID result_dtk[DTK_ACTID] ) ;
#endif
    if ( (int) strlen( CAST_DTK_ACTID result_dtk[DTK_ACTID] ) != 6 )
        return DTKM_ERROR_BAD_ACTID_VALUE ;

    /* should be non-blanks in chars 4-6 of field.  */
    if ( *(CAST_DTK_ACTID result_dtk[DTK_ACTID]+3) == ' ' 
    ||   *(CAST_DTK_ACTID result_dtk[DTK_ACTID]+4) == ' ' 
    ||   *(CAST_DTK_ACTID result_dtk[DTK_ACTID]+5) == ' '  )
        return DTKM_ERROR_BAD_ACTID_VALUE ;

    /* check for sensor and actid matching if DMP or RDL  */
    if ( strncmp(CAST_DTK_ACTID result_dtk[DTK_ACTID], 
                 DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE, 
                 strlen(DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE) ) == 0 )
    {
        if ( strcmp(CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                    DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE) != 0 )
            return DTKM_ERROR_SENSOR_AND_ACTID_DONT_MATCH ;
    }
    else if ( strncmp(CAST_DTK_ACTID result_dtk[DTK_ACTID], 
                 DTKM_SENSOR_REALTIME_DOWNLINK_CODE, 
                 strlen(DTKM_SENSOR_REALTIME_DOWNLINK_CODE) ) == 0 )
    {
        if ( strcmp(CAST_DTK_SENSOR result_dtk[DTK_SENSOR], 
                    DTKM_SENSOR_REALTIME_DOWNLINK_CODE) != 0 )
            return DTKM_ERROR_SENSOR_AND_ACTID_DONT_MATCH ;
    }

    /*
    --  if this is a recording or a tape dump, check to see if
    --  the satellite has a recorder on board.  
    --  and take a look at the antenna id, too.  
    */
    if ( dtkm_is_a_tape_dump( result_dtk ) == TRUE
    ||   dtkm_is_a_recording( result_dtk ) == TRUE )
    {
        if( !(dtkm_sat_has_recorder(result_dtk) == TRUE) )
            return DTKM_ERROR_SAT_DOES_NOT_HAVE_A_RECORDER ;
    }

    if ( dtkm_is_an_observation( result_dtk ) == TRUE )
    {
        /* this is not a downlink; look at the antenna id; should be 0:  */
        if( CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] != 0 )
            return DTKM_ERROR_OBSERVATION_DTK_HAS_ANTENNA_ID_NE_ZERO ;
    }

    return_code = dtkm_check_agency( result_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    --  6. strttime
    */
    return_code = tc_validate_asf_datetime(
        CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME]) ;
#ifdef PRINT_DIAG
    PRINT_DIAG("/%s", CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) ;
#endif
    if ( return_code < 0 ) 
        return DTKM_ERROR_BAD_STRTTIME_VALUE ;

    /*
    --  7. stoptime
    */
    return_code = tc_validate_asf_datetime( 
        CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) ;
#ifdef PRINT_DIAG
    PRINT_DIAG("/%s", 
        CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) ;
#endif
    if ( return_code < 0 )  
        return DTKM_ERROR_BAD_STOPTIME_VALUE ;

    /*
    -- Fields 6., 7.:  check times and satellite inclusion period.  
    */
    if (strcmp( CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME],
        CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) <= 0)
        return DTKM_ERROR_STOPTIME_LE_STRTTIME ;

    return_code = dtkm_check_sat_inclusion_times_ok( result_dtk ) ;
    if (return_code < 0)
        return return_code ;

    /*
    -- Fields 3., 6., 7.:  check rev and times.  
    */
    if( dtkm_sat_sensor_is_low_bit_rate( CAST_DTK_SAT result_dtk[DTK_SAT],
            CAST_DTK_SENSOR result_dtk[DTK_SENSOR] ) == TRUE )
    {
        /*
        -- We do not need to do any checking of the start or stop time
        -- for low bit-rate sensors, since very long 
        -- observations are sometimes encountered.
        -- Note that we do not reconcile the starttime with the rev,
        -- since it was this very same starttime that was used to 
        -- calculate the rev.
        */
        /* 
        -- put in a dummy statement so that lint 
        -- won't print a warning OK.  
        */
        return_code = 0 ;
    }
    else
    {
        /* 
        -- insure that start and stop times are 
        -- both within the rev.  
        */
        return_code = dtkm_check_rev_asftimes( 
            result_dtk, &rev_number_for_strttime, 
            strttime_for_rev, stoptime_for_rev ) ;
        if ( return_code != TRUE )
            return DTKM_ERROR_TIMES_NOT_WITHIN_REV ;
    }

    /*
    --  FIELDS 5, 8-17
    */

    /* 
    -- check the coverage points.  they will be checked and 
    -- errors are counted.  
    */
    cvrg_points_errors = 0 ;

    /*
    --  5. ascdsc
    */
#ifdef PRINT_DIAG
    PRINT_DIAG("/%c\n", 
        CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] ) ;
#endif
    if ( CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] != 'A' 
    &&   CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] != 'D' 
    &&   CAST_DTK_ASCDSC result_dtk[DTK_ASCDSC] != '-' )
        cvrg_points_errors ++ ;

#ifdef PRINT_DIAG
    PRINT_DIAG("%s(%d):  after check 5. ASCDSC:  cvrg_points_errors = %d\n",
        __FILE__, __LINE__, cvrg_points_errors ) ;
#endif

    /*
    --  8. strtlat
    --  9. stoplat
    */
    if ( check_latitude(CAST_DTK_STRTLAT result_dtk[DTK_STRTLAT]) != TRUE )
        cvrg_points_errors ++ ;

    if ( check_latitude(CAST_DTK_STOPLAT result_dtk[DTK_STOPLAT]) != TRUE )
        cvrg_points_errors ++ ;

    /* 
    -- report an error if both start and stop lats 
    -- are 0, indicating no value was set.  
    */
    if (fabs(CAST_DTK_STRTLAT result_dtk[DTK_STRTLAT]) < 0.00001 
    &&  fabs(CAST_DTK_STOPLAT result_dtk[DTK_STOPLAT]) < 0.00001 )
        cvrg_points_errors ++ ;

    /*
    -- 10. nrlat1
    -- 14. nrlat2
    */
    if ( check_latitude(CAST_DTK_NRLAT1 result_dtk[DTK_NRLAT1]) != TRUE )
        cvrg_points_errors ++ ;
    if ( check_latitude(CAST_DTK_NRLAT2 result_dtk[DTK_NRLAT2]) != TRUE )
        cvrg_points_errors ++ ;
    /* 
    -- report an error if both lats 
    -- are 0, indicating no value was set.  
    */
    if (fabs(CAST_DTK_NRLAT1 result_dtk[DTK_NRLAT1]) < 0.00001 
    &&  fabs(CAST_DTK_NRLAT2 result_dtk[DTK_NRLAT2]) < 0.00001 )
        cvrg_points_errors ++ ;

    /*
    -- 11. nrlon1
    -- 15. nrlon2
    */
    if ( check_longitude(CAST_DTK_NRLON1 result_dtk[DTK_NRLON1]) != TRUE )
        cvrg_points_errors ++ ;
    if ( check_longitude(CAST_DTK_NRLON2 result_dtk[DTK_NRLON2]) != TRUE )
        cvrg_points_errors ++ ;
    /* 
    -- report an error if both lons 
    -- are 0, indicating no value was set.  
    */
    if (fabs(CAST_DTK_NRLON1 result_dtk[DTK_NRLON1]) < 0.00001 
    &&  fabs(CAST_DTK_NRLON2 result_dtk[DTK_NRLON2]) < 0.00001 )
        cvrg_points_errors ++ ;

    /*
    -- 12. farlat1
    -- 16. farlat2
    */
    if ( check_latitude(CAST_DTK_FARLAT1 result_dtk[DTK_FARLAT1]) != TRUE )
        cvrg_points_errors ++ ;
#ifdef PRINT_DIAG
    PRINT_DIAG("%s(%d):  cvrg_points_errors = %d\n",
        __FILE__, __LINE__, cvrg_points_errors ) ;
#endif
    if ( check_latitude(CAST_DTK_FARLAT2 result_dtk[DTK_FARLAT2]) != TRUE )
        cvrg_points_errors ++ ;
    /* 
    -- report an error if both lats 
    -- are 0, indicating no value was set.  
    */
    if (fabs(CAST_DTK_FARLAT1 result_dtk[DTK_FARLAT1]) < 0.00001 
    &&  fabs(CAST_DTK_FARLAT2 result_dtk[DTK_FARLAT2]) < 0.00001 )
        cvrg_points_errors ++ ;

    /*
    -- 13. nrlon1
    -- 17. nrlon2
    */
    if ( check_longitude(CAST_DTK_FARLON1 result_dtk[DTK_FARLON1]) != TRUE )
        cvrg_points_errors ++ ;
    if ( check_longitude(CAST_DTK_FARLON2 result_dtk[DTK_FARLON2]) != TRUE )
        cvrg_points_errors ++ ;
    /* 
    -- report an error if both lons 
    -- are 0, indicating no value was set.  
    */
    if (fabs(CAST_DTK_FARLON1 result_dtk[DTK_FARLON1]) < 0.00001 
    &&  fabs(CAST_DTK_FARLON2 result_dtk[DTK_FARLON2]) < 0.00001 )
        cvrg_points_errors ++ ;

    /*
    -- if the error count for the coverage points was non-zero, 
    -- then put them in.  the ascdsc field is included.  
    */

    if ( cvrg_points_errors != 0 )
    {
        return_code = dtkm_get_cvrg_points(DB_SYBINT_USE_APS_READER_DBPROC, 
            result_dtk, result_dtk);
        if ( return_code < 0 )
            return return_code ;
    }

    /*
    -- 18. dtkstat
    */
#ifdef PRINT_DIAG
    PRINT_DIAG(" %s", CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT] ) ;
#endif
    return_code = dtkm_check_dtkstat( result_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /*
    -- 19. transid
    */
#ifdef PRINT_DIAG
    PRINT_DIAG(" %s", CAST_DTK_TRANSID result_dtk[DTK_TRANSID] ) ;
#endif
    return_code = dtkm_check_transid( result_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* 
    -- sat, sensor, actid and transid are OK. 
    -- confirm valid activity_id.  
    */
    return_code = dtkm_activity_id( proposed_dtk );
    if (return_code < 0)
        return return_code; ;

    /*
    -- 20. station_id    
    */
#ifdef PRINT_DIAG
    PRINT_DIAG(" %s\n", CAST_DTK_STATION_ID result_dtk[DTK_STATION_ID] ) ;
#endif
    return_code =  dtkm_check_station_id_antenna_id( result_dtk ) ;

    /* check for an error  */
    if ( return_code < 0 )
        return return_code ;

    /*
    -- 21.  submit time for valid ASF format.   
    */
    return_code = tc_validate_asf_datetime(
        CAST_DTK_SUBMIT_TIME result_dtk[DTK_SUBMIT_TIME]) ;
#ifdef PRINT_DIAG
    PRINT_DIAG(" %s\n", CAST_DTK_SUBMIT_TIME result_dtk[DTK_SUBMIT_TIME] ) ;
#endif
    if ( return_code < 0 ) 
        return DTKM_ERROR_BAD_SUBMIT_TIME_VALUE ;

    /*
    -- 22.  check for downlink data-takes within the station
    --      mask if QUE or SUB.  
    */
    if ( dtkm_is_a_downlink( result_dtk ) )
        if ( strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "QUE" ) == 0 
        ||   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SUB" ) == 0 )
        {
            return_code = dtkm_in_station_mask(
                CAST_DTK_STATION_ID result_dtk[DTK_STATION_ID], result_dtk, 0, 
                strttime_for_mask, stoptime_for_mask ) ;
            if ( return_code < 0 )
                return return_code ;
            else if ( return_code == DTKM_DTK_HAS_NO_TIME_IN_MASK )
                return DTKM_ERROR_DOWNLINK_DTK_HAS_NO_TIME_IN_MASK ;
            else if ( return_code == DTKM_DTK_HAS_TIME_IN_MASK )
            {
                if ( strcmp( strttime_for_mask, 
                        CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME] ) > 0 
                ||   strcmp( stoptime_for_mask, 
                        CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME] ) < 0  )
                    return 
                    DTKM_ERROR_DOWNLINK_DTK_NOT_ENTIRELY_WITHIN_STATION_PASS ;
            }
            else
                return 
                DTKM_ERROR_UNEXPECTED_RETURN_CODE_FROM_dtkm_in_station_mask ;
        }

    return DTKM_REC_OK ;

}

