#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:   ODLconversions.c
 
Description:    source file for conversion routines and Conversion tables.
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
    ODL_VALUEDEFS wos_file_valuedefs[] =
    ODL_CREATE_VALUEDEFS odl_addm_valuedefs[] =
    ODL_CREATE_VALUEDEFS odl_mddm_valuedefs[] =
 
File Scope Variables:
 
NOTE:  The file definitions are processed by
       .../src/ODL_dtkf_c/ODL_dtkf_creator.c
 
==============================================================================*/
#pragma ident   "@(#)ODLconversions.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_interface/SCCS/s.ODLconversions.c"
 
#include <string.h>         /* for strcmp, strncmp argument checks  */
#include <math.h>           /* for fabs, absolute value...          */
#include <stdio.h>          /* for fprintf etc...                   */
#include <errno.h>          /* for errno definitions...             */
#include <stdlib.h>         /* for getopt                           */

#include <sys/types.h>      /* for read                 */
#include <sys/uio.h>        /* for read                 */
#include <sys/unistd.h>     /* for read                 */
#include <timeconv.h>          /* for tc_asf2odl() etc.    */
#include <mu_utilities.h>   /* for permissions          */

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "aps_db_table.h"   /* for APS DB table  definitions        */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "dapps_defs.h"     /* for APS basic definitions            */
#include "apsfiledef.h"     /* for APS error definitions            */

#include "fa_defs.h"        /* for APS file types:  _KEYWORD_VALUE_DEFS  */
#include "ODL_defs.h"       /* for APS file types:  ODL_FILEDEF  */
#include "dtkm_utilities.h" 

/* for syslogging     */
#include "file_utilities.h"
#include "aps_log_msg.h"

/* FOR DATABASE TABLES        */
#include "db_dtk.h"         /* for dtk table             */
#include "db_antenna.h"         /* for dtk table             */
#include "db_maskinout.h"   /* for maskinout table      */

/* FOR APS FULLPATH DEFINITIONS */
#include "apspath.h"   

/* for debugging purposes */
#define   PRINT_DIAG 1 
#undef    PRINT_DIAG

/* FOR ODL_CREATOR */
#include "GENconversions.h"  /* for fa_* globals, gen_string2str() etc.   */
#include "ODLconversions.h"

/* 
--                   global values for 
--                   ODL file creation:  
*/
DB_RECORD   **odl_downlink_dtk_rec ;
llist       *odl_obs_dtk_list ;
DB_RECORD   **odl_obs_dtk_rec ;

/*==============================================================================
Function:       ODL_rev2trackstart()

Description:    calculate the track_start time in ODL format

Creator:        Lawrence Stevens

Creation Date:  Fri Apr 11 16:28:44 PDT 1997

==============================================================================*/
static int 
ODL_rev2trackstart(
    void        *unused_pointer,
    int         *rev,
    char        *ODL_track_start_time )
{
    int     return_code ;
    char    asf_track_start_time[ASF_TIME_STR_LENGTH+1] ;

    /* 
    -- get the time in ASF time format:  
    */
    return_code = gen_rev2trackstart( unused_pointer, rev, 
        asf_track_start_time ) ;
    if( return_code != TRUE )
        return return_code ;

    return ( tc_asf2odl( asf_track_start_time, ODL_track_start_time ) ) ;

}

/*==============================================================================
Function:       ODL_rev2trackend()

Description:    calculate the track_end time in ODL format

Creator:        Lawrence Stevens

Creation Date:  Fri Apr 11 16:28:44 PDT 1997

==============================================================================*/
static int 
ODL_rev2trackend(
    void        *unused_pointer,
    int         *rev,
    char        *ODL_track_end_time )
{

    int     return_code ;
    char    asf_track_end_time[ASF_TIME_STR_LENGTH+1] ;

    /* 
    -- get the time in ASF time format:  
    */
    return_code = gen_rev2trackend( unused_pointer, rev, 
        asf_track_end_time ) ;
    if( return_code != TRUE )
        return return_code ;

    return ( tc_asf2odl( asf_track_end_time, ODL_track_end_time ) ) ;

}

/*==============================================================================
Function:       get_aos_los_start_stop()

Description:    Get values for the keywords TIME_AOS, TIME_LOS.

                *** For ASF 10-Meter  
                Given current rev, determine the start/end of ASF mask 
                and assign this value to AOS/LOS.  (Populate AOS and 
                additionally, place AOS/LOS in  global variables.
                (FOR 10 METER ANTENNA ONLY)

                If the current downlink start time is earlier than AOS,
                shift the AOS/LOS span to match this start time.
                If the current downlink stop time is later than LOS, 
                shift the LOS to match this stop time, and notify the user
                that we have exceeded the original AOS/LOS span.
                (FOR 10 METER ANTENNA ONLY)

                *** For antennas other than the ASF 10 meter, we use the
                routines ODL_rev2trackstart and ODL_rev2trackend.

Creator:        Miguel Siu

Creation Date:  Tue May  6 15:33:01 PDT 1997

NOTES:
    This routine populates the TIME_AOS argument, but it also places AOS/LOS
    values in global variables fa_aos_track_start and fa_los_track_end.

==============================================================================*/
int get_aos_los_start_stop(
    void        *unused_pointer,
    DB_RECORD   ***dtk_rec_ptr,      /* read values from this DB_RECORD   */
    char        *result_string )    
{
    DB_RECORD   **dtk_rec ;
    DB_RECORD   **maskinout_rec ;
    llist       *maskinout_list = NULL ;
    cursor      maskinout_list_ptr ;

    DB_RECORD   **antenna_rec ;
    llist       *antenna_list = NULL ;
    cursor      antenna_list_ptr ;

    double      et_dtk_strttime ; /* time of dtk strttime     */
    double      et_dtk_stoptime ; /* time of dtk stoptime     */
    double      final_et_start ;
    double      final_et_stop;
    double      et_mask_span ;    /* this is pretty much a constant */
    int         return_code ;
    int         rev_int ;
    int         MASK_EXTENSIONS = 0 ;

    static int      id_10_meter_antenna = 0 ;
    static int      last_rev = 0 ;
    static char     last_sat[3] = "XX" ;
    static double   et_inmask = 0.0 ;   /* time  IN from maskinout table */
    static double   et_outmask = 0.0 ;  /* time OUT from maskinout table */
    double          et_shift_inmask ;
    double          et_shift_outmask ;


    /* 
    -- the APS code always uses a DB_RECORD, 
    -- so make this assignment to make the code 
    -- look like everywhere else.  Use dtk_rec after 
    -- this.  
    */
    dtk_rec = *dtk_rec_ptr ;

    /*
    -- Determine if we are processing ASF 10 Meter antenna.
    -- First, get a few facts... like antenna_id for ASF 10 Meter.
    */
    if (id_10_meter_antenna == 0)
    {
        sprintf(where_clause, "where %s like '%s%%'",
            APS_COL(ANTENNA, ANTENNA_COMMENT), "ASF 10 Meter antenna");
        antenna_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
            APS_TABLE(ANTENNA),
            where_clause, NULL, APS_CDEFS(ANTENNA), ALL_COLS ) ;
        if ( antenna_list == NULL )
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "get_aos_los_start_stop():\
DTKM_ERROR_DB_QUERY_FAILED. Check ANTENNA relation.\n",
                DO_SYSLOG, DO_PRINT);
            return FALSE ;
        }
     
        if ( NUMELTS( antenna_list ) == 0 )
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "get_aos_los_start_stop():\
Zero records found in ANTENNA relation for 10 Meter antenna.\n",
                DO_SYSLOG, DO_PRINT);
            DEL_LIST( antenna_list ) ;
            return FALSE ;
        }
     
        if ( NUMELTS( antenna_list ) != 1 )
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "get_aos_los_start_stop():\
Too many records found in ANTENNA relation for 10 Meter antenna.\n",
                DO_SYSLOG, DO_PRINT);
            DEL_LIST( antenna_list ) ;
            return FALSE ;
        }
        antenna_rec = (DB_RECORD **) FIRST(antenna_list, antenna_list_ptr) ;
        id_10_meter_antenna = 
                    CAST_ANTENNA_ANTENNA_ID antenna_rec[ANTENNA_ANTENNA_ID] ;
        DEL_LIST (antenna_list) ;
    }


    /*
    -- If 10 Meter antenna not being used,
    -- use ODL_rev2trackstart and ODL_rev2trackend and exit
    */
    if (strcmp(CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID], "ASF") != 0
    ||  CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] != id_10_meter_antenna )
    {
        rev_int = CAST_DTK_REV dtk_rec[DTK_REV] ;
        if (gen_rev2trackstart(NULL, &rev_int, 
                               fa_aos_track_start) != TRUE)
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "get_aos_los_start_stop():gen_rev2trackstart failed.\n",
                DO_SYSLOG, DO_PRINT);
            return FALSE ;
        }

        if (gen_rev2trackend  (NULL, &rev_int,
                               fa_los_track_end) != TRUE)
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "get_aos_los_start_stop():gen_rev2trackend failed.\n",
                DO_SYSLOG, DO_PRINT);
            return FALSE ;
        }

        tc_asf2odl(fa_aos_track_start,result_string) ;
        return TRUE;
    }


    /*
    -- We're using the ASF 10 meter antenna, continue.
    */
    /*
    -- convert the dtk_rec strttime/stoptime into ephemeris time,
    -- for comparisons later in program.
    */
    return_code = tc_asf2et(CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
        &et_dtk_strttime ) ;
    if ( !return_code )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
        "get_aos_los_start_stop():DTKM_ERROR_BAD_STRTTIME_VALUE\n",
            DO_SYSLOG, DO_PRINT);
        return FALSE ;
    }
    return_code = tc_asf2et(CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
        &et_dtk_stoptime ) ;
    if ( !return_code )
    {
        aps_log_msg(file_util_progname, APS_ERROR, 
        "get_aos_los_start_stop():DTKM_ERROR_BAD_STOPTIME_VALUE\n",
            DO_SYSLOG, DO_PRINT);
        return FALSE ;
    }   

    /*
    -- lookup the ASF mask-in value, if needed.
    -- (There's a chance that we already have the values we need from the
    -- last lookup)
    */
    if (strcmp(CAST_DTK_SAT dtk_rec[DTK_SAT], last_sat) != 0
    || CAST_DTK_REV dtk_rec[DTK_REV] != last_rev )
    {
        strcpy(last_sat, CAST_DTK_SAT dtk_rec[DTK_SAT]) ;
        last_rev = CAST_DTK_REV dtk_rec[DTK_REV] ;

        sprintf(where_clause, "where %s = '%s' and %s = '%s' and %s = %ld",
            APS_COL(MASKINOUT, MASKINOUT_STATIONID), "ASF",
            APS_COL(MASKINOUT, MASKINOUT_SAT), CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL(MASKINOUT, MASKINOUT_REV), CAST_DTK_REV dtk_rec[DTK_REV]);

        maskinout_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
            APS_TABLE(MASKINOUT),
            where_clause, NULL, APS_CDEFS(MASKINOUT), ALL_COLS ) ;
     
        if ( maskinout_list == NULL )
        {
            aps_log_msg(file_util_progname, APS_ERROR, 
            "get_aos_los_start_stop():ERROR_DB_QUERY_FAILED\n",
                DO_SYSLOG, DO_PRINT);
            return FALSE ;
        }
     
        if ( NUMELTS( maskinout_list ) == 0 )
        {
            DEL_LIST( maskinout_list ) ;
            sprintf(file_util_msg,
            "get_aos_los_start_stop():\
No records in MASKINOUT relation for station='ASF' sat='%s' rev=%ld",
            CAST_DTK_SAT dtk_rec[DTK_SAT], CAST_DTK_REV dtk_rec[DTK_REV]);
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
                DO_SYSLOG, DO_PRINT);
            return FALSE ;
        }
     
        if ( NUMELTS( maskinout_list ) != 2 )
        {
            DEL_LIST( maskinout_list ) ;
            sprintf(file_util_msg,
            "get_aos_los_start_stop():\
%d recs found, should have been 2 recs in MASKINOUT relation for station='ASF' sat='%s' rev=%ld",
            NUMELTS( maskinout_list ), CAST_DTK_SAT dtk_rec[DTK_SAT], 
            CAST_DTK_REV dtk_rec[DTK_REV]);
            aps_log_msg(file_util_progname, APS_ERROR, file_util_msg,
                DO_SYSLOG, DO_PRINT);
            return FALSE ;
        }

        for (
            maskinout_rec =
                (DB_RECORD **) FIRST(maskinout_list, maskinout_list_ptr) ;
            maskinout_rec ;
            maskinout_rec = 
                (DB_RECORD **)NEXT(maskinout_list, maskinout_list_ptr)
            )
        {
            if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT],
            "IN", 2 ) == 0 )
                et_inmask = 
                CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] ;
            else if ( strncmp ( CAST_MASKINOUT_INOUT maskinout_rec[MASKINOUT_INOUT],
            "OUT", 3 ) == 0 )
                et_outmask = 
                CAST_MASKINOUT_EJDATE maskinout_rec[MASKINOUT_EJDATE] ;
            else
            {
                aps_log_msg(file_util_progname, APS_ERROR, 
                    "get_aos_los_start_stop():\
    DTKM_ERROR_REC_NEITHER_IN_NOR_OUT_IN_MASKINOUT\n",
                    DO_SYSLOG, DO_PRINT);
                return FALSE ;
            }
        }
        DEL_LIST (maskinout_list) ;
    }


    et_mask_span    = et_outmask - et_inmask ;
    et_shift_inmask = et_inmask ;
    et_shift_outmask = et_outmask ;

    /*
    -- Now, compare the dtk_rec strttime/stoptime to the maskinout times,
    -- and assign values to ASF_aos_start, ASF_aos_stop.
    -- 
    -- 1) Compare et_inmask, et_dtk_strttime; place earliest in 
    --    global variable fa_aos_track_start.
    -- 2) If et_dtk_strttime was used, reduce et_outmask by the overlap 
    --    between et_inmask, et_dtk_strttime.
    -- 3) Compare (reduced) et_outmask, et_dtk_stoptime; place latest in 
    --    global variable fa_los_track_end.
    -- 4) If et_dtk_stoptime was used, do some further checking;
    --    the range between original et_inmask and original et_outmask 
    --    may have been exceeded.  Print a warning message.
    --
    -- NOTE: We place the AOS value in result_string to satisfy the action
    -- of the calling program.
    */
    if (et_dtk_strttime > et_shift_inmask)
        final_et_start = et_shift_inmask ;
    else
    {
        final_et_start = et_dtk_strttime ;

        /* shift the end-of-mask */
        et_shift_outmask = 
                et_shift_outmask - (et_shift_inmask - et_dtk_strttime) ;
        MASK_EXTENSIONS++ ;
    } 


    if (et_dtk_stoptime < et_shift_outmask)
        final_et_stop = et_shift_outmask ;
    else
    {
        final_et_stop = et_dtk_stoptime ;

        /* 
        -- shift the start-of-mask.
        */
        if (++MASK_EXTENSIONS == 2)
            /* 
            -- If this is our second shift, just continue to the final action
            */ ;
        else
        {
            et_shift_inmask = 
                    et_shift_inmask + (et_dtk_stoptime - et_shift_outmask) ;

            /*
            -- One last check of the start-of-mask.
            */
            if (et_dtk_strttime < et_shift_inmask)
                final_et_start = et_dtk_strttime ;
            else
                final_et_start = et_shift_inmask ;
        }
    }


    /* final action */

    if (MASK_EXTENSIONS == 2
    ||  et_mask_span < (final_et_stop - final_et_start) )
    {
        sprintf(file_util_msg,
"The following downlink has exceeded station mask=>sat:%s, sensor:%s rev: %ld, dtkid:%d, strttime: %s , stoptime: %s \n",
        CAST_DTK_SAT dtk_rec[DTK_SAT],
        CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
        CAST_DTK_REV dtk_rec[DTK_REV],
        CAST_DTK_DTKID dtk_rec[DTK_DTKID],
        CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
        CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME]) ;
        aps_log_msg(file_util_progname, APS_INFO, file_util_msg,
            DO_SYSLOG, DO_PRINT);
    }

    if (MASK_EXTENSIONS == 2)
    {
        /* 
        -- assign the downlink start/stop to the TIME_AOS, TIME_LOS keywords
        */
        tc_et2asf(et_dtk_strttime, fa_aos_track_start);
        tc_et2asf(et_dtk_stoptime, fa_los_track_end);
    }
    else
    {
        /* 
        -- assign the downlink start/stop to the TIME_AOS, TIME_LOS keywords
        */
        tc_et2asf(final_et_start, fa_aos_track_start);
        tc_et2asf(final_et_stop , fa_los_track_end);
    }

    /* 
    -- populate TIME_AOS value, to satisfy the calling program 
    */
    tc_asf2odl(fa_aos_track_start, result_string) ;

    return TRUE ;
}

/*==============================================================================
Function:       get_obs_dtks()

Description:    Get obs_dtks into the global llist and put number of 
                records into the result_string

Creator:        Lawrence Stevens

Creation Date:  Wed Apr  9 16:52:39 PDT 1997

==============================================================================*/
static int 
get_obs_dtks(
    void        *unused_pointer,
    DB_RECORD   ***dtk_rec_ptr,      /* read values from this DB_RECORD   */
    char        *result_string )
{
    int         return_code ;
    DB_RECORD   **dtk_rec ;
    cursor      dtk_list_ptr ;
    /* 
    -- the APS code always uses a DB_RECORD, 
    -- so make this assignment to make the code 
    -- look like everywhere else.  Use dtk_rec after 
    -- this.  
    */
    dtk_rec = *dtk_rec_ptr ;

    /* 
    -- odl_obs_dtk_list is a global *llist  
    -- if not yet created, then create the list 
    -- and allocate memory.  
    */
    if( odl_obs_dtk_list == NULL )
        odl_obs_dtk_list = create_dyn_llist() ;

    /* 
    -- if there are any members in this list, then 
    -- delete them and free memory.  
    */
    DEL_ALL( odl_obs_dtk_list ) ;

    /* 
    -- the llist odl_obs_dtk_list now exists 
    -- and is empty, as required by dtkm_dl2obs(), 
    -- which reads the downlink record (dtk_rec) and then 
    -- finds its observations and puts them into odl_obs_dtk_list:
    */
    return_code = dtkm_dl2obs( dtk_rec, odl_obs_dtk_list ) ;
    if( return_code < 0 )
    {
        /* print dtkm error message.  */
        fprintf(stderr, "%s(%d):  Error in get_obs_dtks()\n", 
            __FILE__, __LINE__ ) ;
        fprintf(stderr, 
            "Error code returned from dtkm_dl2obs()\n" ) ;
        fprintf(stderr, "%s\n", DTKM_ERROR_MESSAGE( return_code ) ) ;
        (void)dtkm_print( stderr, dtk_rec ) ;
        /* 
        -- NOTE:  we retain odl_obs_dtk_list as an empty list 
        -- when exiting this routine, so that it can be used 
        -- to process the dtk records in it.  
        */
        return FALSE ;
    }

    /* 
    -- print the count; this does go 
    -- into the report:  
    */
    sprintf(result_string, "%d", NUMELTS( odl_obs_dtk_list ) ) ;

    /* 
    -- NOTE:  we retain odl_obs_dtk_list with the records in 
    -- it when exiting this routine, so that the list can 
    -- be used to process the dtk records in it.  
    */

    /*
    -- Another note:  CSA does not give us the value of the 
    -- sensor mode in their files.  The APS must input the 
    -- value of SAR as a default for dtk.sensor.  
    -- However, in the downlink-to-data-take mapping file,
    -- another default value is preferred.  This 
    -- value is set here:
    -- By setting the value here, this is the only place 
    -- that it need be set.  The code after here will 
    -- never take the value and try to find something in 
    -- the database using that value.  
    */
    for (   dtk_rec = (DB_RECORD **) FIRST(odl_obs_dtk_list, dtk_list_ptr);
            dtk_rec != NULL ;
            dtk_rec = (DB_RECORD **) NEXT(odl_obs_dtk_list, dtk_list_ptr)  
        )
    {
        if( strcmp( "R1",  CAST_DTK_SAT    dtk_rec[DTK_SAT] ) == 0 
        &&  strcmp( "SAR", CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0  )
        {
            /* 
            --  sat/sensor is R1/SAR; set the 
            --  sensor value to the approved default value:
            */
            strcpy( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                ODL_DEFAULT_R1_SENSOR ) ;
        }
    }

    return TRUE ;

}

/*==============================================================================
Function:       ODL_set_frame_mode()

Description:    Check to see if the dtk is in Antarctic Mode.  This 
                is the value for FRAME_MODE =

Creator:        Lawrence Stevens

Creation Date:  Tue Apr  8 17:09:45 PDT 1997

==============================================================================*/
static int 
ODL_set_frame_mode(
    void        *unused_pointer,
    DB_RECORD   ***dtk_rec_ptr,        /* read values from DB_RECORD   */
    char        *result_string )
{
    int     return_code ;
    DB_RECORD   **dtk_rec ;

    dtk_rec = *dtk_rec_ptr ;

    return_code = dtkm_is_in_antarctic_mode( dtk_rec ) ;
    if( return_code < 0 )
    {
        /* print dtkm error message.  */
        fprintf(stderr, "%s(%d):  Error in ODL_set_frame_mode()\n", 
            __FILE__, __LINE__ ) ;
        fprintf(stderr, 
            "Error code returned from dtkm_is_in_antarctic_mode()\n" ) ;
        fprintf(stderr, "%s\n", DTKM_ERROR_MESSAGE( return_code ) ) ;
        (void)dtkm_print( stderr, dtk_rec ) ;
        return FALSE ;
    }

    /* the value is single-quoted:  */
    if( return_code == TRUE )
        strcpy( result_string, "'ANTARCTIC'" ) ;
    else
        strcpy( result_string, "'ARCTIC'" ) ;

    return TRUE ;

}

/*==============================================================================
Function:       ODL_set_quicklook_flag()

Description:    Determine and put in the QUICKLOOK_FLAG for a DTK in the DDM.

Creator:        Lawrence Stevens

Creation Date:  Tue Apr  8 17:09:45 PDT 1997

==============================================================================*/
static int 
ODL_set_quicklook_flag(
    void        *unused_pointer,
    DB_RECORD   ***dtk_rec_ptr,        /* read values from DB_RECORD   */
    char        *result_string )
{
    DB_RECORD   **dtk_rec ;

    /* set up to make code look like usual APS code:  */
    dtk_rec = *dtk_rec_ptr ;

    /*
    -- if either the planner or the science user 
    -- says yes, the flag is 'YES'
    */
    if ( CAST_DTK_SCIENCE_QUICKLOOK dtk_rec[DTK_SCIENCE_QUICKLOOK] == 'Y' 
    ||   CAST_DTK_PLANNER_QUICKLOOK dtk_rec[DTK_PLANNER_QUICKLOOK] == 'Y' )
        strcpy( result_string, "'YES'" ) ;
    else
        strcpy( result_string, "'NO'" ) ;

    return TRUE ;
}

/*==============================================================================
Function:       ODL_set_auth_flag()

Description:    
        Determine and put in the PROCESS_AUTH_FLAG for a DTK in the DDM.
        The value (single-quoted YES or NO) is determined from other 
        fields in the dtk relation and is as follows:

        Value is 'YES', but there are 2 exceptions:

            value is 'NO' if:
                (sat = R1 AND darid = 0
                AND stoptime is < 6 months before current system time)
            value is 'NO' if:
                sat = A1

        Note:  using 183 days for the 6 months.  

Creator:        Lawrence Stevens

Creation Date:  Tue Apr  8 17:09:45 PDT 1997

==============================================================================*/
static int 
ODL_set_auth_flag(
    void        *unused_pointer,
    DB_RECORD   ***dtk_rec_ptr,        /* read values from DB_RECORD   */
    char        *result_string )
{
    DB_RECORD   **dtk_rec ;

    char    nowtime[ASF_TIME_STR_LENGTH+1] ;
    char    oldtime[ASF_TIME_STR_LENGTH+1] ;

    /* 
    -- set up to make code look like it usually 
    -- does in the APS with dtk_rec
    */
    dtk_rec = *dtk_rec_ptr ;
    strcpy(result_string, "'YES'") ;

    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "A1" ) == 0 )
    {
        /* 
        -- sat = A1
        */
        strcpy(result_string, "'NO'") ;
        return TRUE ;
    }

    /* sat == J1:  YES   */

    if ( strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], "R1" ) == 0 
    &&   CAST_DTK_DARID dtk_rec[DTK_DARID] == 0 )
    {
        /* 
        -- sat = R1 AND darid = 0.  
        -- now check if stoptime is < 183 days 
        -- before current system time
        */
        if( !tc_systime2asf(nowtime) )
            return FALSE ;
        if( !tc_asf_add_ndays( nowtime, (double)-183.0, oldtime ) )
            return FALSE ;
        if( strcmp( CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME], oldtime ) > 0 )
        {
            /* 
            -- stoptime is after oldtime.  
            -- stoptime is too recent, < 183 days 
            -- before current system time
            -- sat = R1 AND darid = 0.  
            */
            strcpy(result_string, "'NO'") ;
            return TRUE ;
        }
    }

    return TRUE ;
}

/*==============================================================================
Function:       ODL_set_quicklook()

Description:    compares the dtk.science_quicklook flag with the 
                global variable fa_dtk_quicklook_flag and then
                translates the result.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 14 21:58:07 PST 1995

==============================================================================*/
static int 
ODL_set_quicklook(
    EQUIV_TABLE *conversion_table,
    char        *science_quicklook,   /* a single character.  */
    char        *result_string )
{

    /* 
    -- logical OR the science_quicklook flag with the 
    -- planner_quicklook flag, saved into fa_dtk_quicklook_flag:
    */
    if ( *science_quicklook == 'Y' )
        fa_dtk_quicklook_flag = 'Y' ;

    /*
    -- translate the single character value to the 
    -- file value.  
    */ 
    return ( table_lookupAPS2FAquoted( conversion_table, 
        &fa_dtk_quicklook_flag, result_string ) )  ;
}

/*==============================================================================
Function:       ODL_planner_quicklook()

Description:    saves the dtk.planner_quicklook flag into the 
                global variable fa_dtk_quicklook_flag for later
                comparison.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 14 21:58:07 PST 1995

==============================================================================*/
static int 
ODL_planner_quicklook(
    void        *unused_pointer,
    char        *planner_quicklook,   /* a single character.  */
    char        *result_string )
{

    /* save string.  */
    fa_dtk_quicklook_flag = *planner_quicklook ; 

    /*
    -- must suppress any printing
    */
    strcpy( result_string, "NONE" ) ;

    return TRUE ;
}

/*==============================================================================
Function:       ODL_save_sat_value()

Description:    saves the satellite value to fa_sat_id.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 14 20:08:03 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int 
ODL_save_sat_value(
    void        *unused_pointer,
    char        *sat_source_string,
    char        *result_string )
{

    /*
    -- need to save the sat value into 
    -- the fa_sat_id[] string 
    */
    strcpy( fa_sat_id, sat_source_string ) ;

    /* 
    -- make the result_string be NONE to prevent 
    -- printing anything.  
    */
    strcpy( result_string, "NONE" ) ;

    return TRUE ;
}

/*==============================================================================
Function:       get_ODL_transid()

Description:    accepts the transid value, calls a table lookup routine, 
                then, if the satellite is an ERS-2, must tweak the 
                values.  

Creator:        Lawrence Stevens

Creation Date:  Thu Dec 14 20:08:03 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int 
get_ODL_transid(
    EQUIV_TABLE *equiv_table,
    char        *transid_source_string,
    char        *result_string )
{

    if (!table_lookupAPS2FAquoted(equiv_table, transid_source_string, 
        result_string ))
    {
        return FALSE ;
    }


    /* may have to change ERS-1_8140 to ERS-2_8140  */
    if ( strcmp( fa_sat_id, "E2" ) == 0 )
        result_string[5] = '2' ;

    return TRUE ;
}

/*==============================================================================
Function:       ODL_antenna_id()

Description:    accepts the antenna_id value, prints to ANTENNA_1 etc.  

Creator:        Lawrence Stevens

Creation Date:  Fri Jan 12 21:46:51 PST 1996 

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int 
ODL_antenna_id(
    void         *unused_pointer,
    short int   *antenna_id,
    char        *result_string )
{
    /* 
    -- sprintf() returns < 0 if there 
    -- is an error:  
    */
    if ( sprintf( result_string, "'ANTENNA_%hd'", *antenna_id )    <    0 )
        return FALSE ;

    return TRUE ;
}


/*==============================================================================
Function:       ODL_CREATE_VALUEDEFS odl_wos_valuedefs[] =

Description:    description of ODL WOS


Creator:        Lawrence Stevens

Creation Date:  Thu Dec 14 22:24:38 PST 1995

NOTE:  this file definition is processed by 
       .../src/ODL_dtkf_c/ODL_dtkf_creator.c
==============================================================================*/
ODL_CREATE_VALUEDEFS odl_wos_valuedefs[] =  
{

#ifdef TEMPLATE
    {source_code,   destination_code, 
        keyword,                conversion(),           conversion_table,
                                                                    source},
#endif

    /*
    --  FILE_HEADER destination definitions:
    */
    /*  0 outgoing WOS/AREQ */
    {ODL_DEFAULT, ODL_FILE_HEADER,
        "object = ",  table_lookupAPS2FA,       WOS_object, (int) fa_file_type},

    {ODL_DEFAULT, ODL_FILE_HEADER,
        "object = common_header",   NULL,       NULL,       NULL },

    {ODL_CONTROL, ODL_FILE_HEADER,
        "    TIME = ",  gen_asftime2odl,        NULL, (int) &fa_creation_date },

    {ODL_DEFAULT, ODL_FILE_HEADER, 
        "    MSG_TYPE = ", table_lookupAPS2FAquoted, WOS_msg_type, 
                                                        (int) fa_file_type    },

    {ODL_DEFAULT, ODL_FILE_HEADER,   
        "    DESTINATION = ",table_lookupAPS2FAquoted, WOS_destination,
                                                        (int) fa_station_id   },

    /*  5 outgoing WOS/AREQ */
    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "    SOURCE = ",    gen_string2str,     NULL, (int) "'APS'" },

    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "    FIRST_AOS = ", gen_asftime2odl,    NULL, 
                                                (int) fa_first_track_start},

    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "    LAST_LOS = ",  gen_asftime2odl,    NULL, 
                                                (int) fa_last_track_end},
    /*  8 outgoing WOS/AREQ */
    {ODL_CONTROL, ODL_FILE_HEADER,
        "    NUMBER_OF_RECORDS = ", gen_int2str,NULL,
                                                (int) &fa_number_of_records },
    {ODL_DEFAULT, ODL_FILE_HEADER,
        "end_object = common_header",   NULL,   NULL,       NULL },


    /* 10 outgoing WOS/AREQ */
    {ODL_DEFAULT, ODL_FILE_TRAILER,
        "end_object = ",  table_lookupAPS2FA,   WOS_object, (int) fa_file_type},
    /* 11 outgoing WOS/AREQ */
    {ODL_DEFAULT, ODL_FILE_TRAILER,
        "end",      NULL,   NULL,       NULL },


    /*
    --  FILE_RECORD definitions:
    */
    {ODL_DEFAULT, ODL_FILE_RECORD,  
        "object = ",  table_lookupAPS2FA,   WOS_record, (int) fa_file_type},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,   
        "    SAVE SAT VALUE ",  ODL_save_sat_value, NULL,   DTK_SAT},
    {ODL_DTK_RECORD, ODL_FILE_RECORD,   
        "    PLATFORM = ",  table_lookupAPS2FAquoted, WOS_satellite,DTK_SAT},

    /* 15 outgoing WOS/AREQ */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,   
        "    SENSOR = ",  table_lookupAPS2FAquoted, WOS_sensor,   DTK_SENSOR},

    /*
    -- NOTE:  there MUST be a blank before and after MODE here:
    -- There is a value-based patch involving this keyword in
    -- ODL_dtkf_creator.c.  Search on patch to find it there.
    -- This is so that some other keyword with MODE embedded
    -- in it will not cause the patch problems.
    */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    MODE = ",      table_lookupAPS2FAquoted, WOS_mode, DTK_SENSOR},

    /* 17 outgoing WOS/AREQ */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,   
        "    REVOLUTION = ",gen_int2str,        NULL,       DTK_REV},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    SEQUENCE = ",gen_tinyint2str,      NULL,       DTK_DTKID},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    ACTIVITY_ID = ",table_lookupAPS2FAquoted, WOS_activity, DTK_ACTID},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    AGENCY = ",    table_lookupAPS2FAquoted, WOS_agency,   DTK_ACTID},

    /* 21 outgoing WOS/AREQ */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    TIME_ON  = ",      gen_asftime2odl, NULL,      DTK_STRTTIME},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    TIME_OFF = ",      gen_asftime2odl, NULL,      DTK_STOPTIME},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    TRANSMITTER_ID = ",get_ODL_transid, WOS_transid,   DTK_TRANSID},

    /* 24 outgoing WOS/AREQ */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    ANTENNA_ID = ",    ODL_antenna_id,     NULL,   DTK_ANTENNA_ID},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    SITE_NAME = ",      gen_string2qstr, NULL,     DTK_SITENAME},

    /*
    -- The following instructions for TIME_AOS and TIME_LOS require the 
    -- initialization of global variables fa_sat_id, fa_station_id, 
    -- fa_antenna_id.  This is done by the calling routine, before 
    -- the individual odl dtk record is processed.
    */
    {ODL_CONTROL, ODL_FILE_RECORD,   
        "  TIME_AOS = ",        get_aos_los_start_stop,     NULL,    
                                           (int) &odl_downlink_dtk_rec },
    {ODL_CONTROL, ODL_FILE_RECORD,   
        "  TIME_LOS = ",        gen_asftime2odl,            NULL,    
                                           (int) fa_los_track_end },

    /* 
    --  2 fields, dtk.planner_quicklook and 
    -- dtk.science_quicklook, 
    -- must be logical OR'ed and the 
    -- value converted.  to compare, the first value 
    -- is saved into fa_dtk_quicklook_flag, a 
    -- global variable.  
    */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    SAVE PLANNER QUICKLOOK = ",ODL_planner_quicklook, NULL,
                                                        DTK_PLANNER_QUICKLOOK},

    /* compare flags, use WOS_quicklook to convert values.  */
    /* 29 outgoing WOS/AREQ */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    QUICKLOOK_FLAG = ",ODL_set_quicklook, WOS_quicklook,
                                                        DTK_SCIENCE_QUICKLOOK},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "    FA_SCHEDULE_LINK = ",gen_string2qstr, NULL, DTK_FA_SCHEDULE_LINK},


    /* 31 outgoing WOS/AREQ */
    {ODL_DEFAULT, ODL_FILE_RECORD,  
        "end_object = ",  table_lookupAPS2FA,   WOS_record, (int) fa_file_type},


    { 0, 0, NULL, NULL, NULL}
    /*
    --     NOTE:  this file definition is processed by 
    --     .../src/ODL_dtkf_c/ODL_dtkf_creator.c
    */

} ; 


/*==============================================================================
Function:       ODL_CREATE_VALUEDEFS odl_dmap_valuedefs[] =

Description:    description of ODL DMAP file.  

Creator:        Lawrence Stevens

Creation Date:  Mon Apr  7 18:27:46 PDT 1997

NOTE:  this file definition is processed by 
       .../src/ODL_dtkf_c/ODL_dtkf_creator.c
==============================================================================*/
ODL_CREATE_VALUEDEFS odl_dmap_valuedefs[] =  
{

#ifdef TEMPLATE
    {source_code,   destination_code, 
        keyword,                conversion(),           conversion_table,
                                                                    source},
#endif

    /*
    --  FILE_HEADER destination definitions:
    */
    /*  0 outgoing DMAP */
    {ODL_DEFAULT, ODL_FILE_HEADER,
        "OBJECT = ",  table_lookupAPS2FA,       WOS_object, (int) fa_file_type},

    {ODL_DEFAULT, ODL_FILE_HEADER,
        "OBJECT = COMMON_HEADER",   NULL,       NULL,       NULL },

    {ODL_DEFAULT, ODL_FILE_HEADER, 
        "  MSG_TYPE = ", table_lookupAPS2FAquoted, WOS_msg_type, 
                                                        (int) fa_file_type    },
    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "  SOURCE = 'APS'",       NULL,       NULL,       NULL },

    {ODL_DEFAULT, ODL_FILE_HEADER,   
        "  DESTINATION = 'IMS'",  NULL,       NULL,       NULL },
    /*  5 outgoing DMAP */
    {ODL_CONTROL, ODL_FILE_HEADER,
        "  FILE_CREATION_TIME = ",  gen_asftime2odl,  NULL, 
                                                   (int) &fa_creation_date },
    {ODL_CONTROL, ODL_FILE_HEADER,
        "  NUMBER_OF_RECORDS = ", gen_intplus1_2str,NULL,
                                                (int) &fa_number_of_records },
    {ODL_DEFAULT, ODL_FILE_HEADER,
        "END_OBJECT = COMMON_HEADER",   NULL,   NULL,       NULL },

    /*  8 outgoing DMAP */
    {ODL_DEFAULT, ODL_FILE_HEADER,
        "OBJECT = CATALOG_METADATA",   NULL,       NULL,       NULL },

    {ODL_CONTROL, ODL_FILE_HEADER,
        "  FILE_CREATION_TIME = ",  gen_asftime2odl,  NULL, 
                                                   (int) &fa_creation_date },
    /* 10 outgoing DMAP  */
    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "  FILE_NAME = ", gen_string2qstr,    NULL, (int) &fa_file_type },

    {ODL_CONTROL, ODL_FILE_HEADER,
        "  VALID_START_TIME = ",  gen_asftime2odl,  NULL, 
                                            (int) &fa_argument_start_time },
    {ODL_CONTROL, ODL_FILE_HEADER,
        "  VALID_END_TIME = ",    gen_asftime2odl,  NULL, 
                                            (int) &fa_argument_stop_time },
    /* 13 outgoing DMAP  */
    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "  FIRST_AOS = ", gen_asftime2odl,    NULL, 
                                                (int) fa_first_track_start},
    {ODL_DEFAULT, ODL_FILE_HEADER,  
        "  LAST_LOS = ",  gen_asftime2odl,    NULL, 
                                                (int) fa_last_track_end},
    {ODL_DEFAULT, ODL_FILE_HEADER,
        "END_OBJECT = CATALOG_METADATA",   NULL,   NULL,       NULL },

    /*
    --  FILE_RECORD definitions:
    */
    /* 16 outgoing DMAP  */
    {ODL_DEFAULT, ODL_FILE_RECORD,  
        "OBJECT = DL_TO_DTKS_ENTRY",  NULL, NULL, NULL },

    {ODL_DTK_RECORD, ODL_FILE_RECORD,   
        "  PLATFORM = ", table_lookupAPS2FAquoted, WOS_satellite, DTK_SAT},

    /* 18 outgoing DMAP  */
    {ODL_DEFAULT, ODL_FILE_RECORD,  
        "  SENSOR = 'Z'",  NULL, NULL, NULL },

    {ODL_DTK_RECORD, ODL_FILE_RECORD,   
        "  REVOLUTION = ",gen_int2str,        NULL,       DTK_REV},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  SEQUENCE = ",gen_tinyint2str,      NULL,       DTK_DTKID},

    /* 23 outgoing DMAP  */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  TIME_ON  = ",      gen_asftime2odl, NULL,      DTK_STRTTIME},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  TIME_OFF = ",      gen_asftime2odl, NULL,      DTK_STOPTIME},

    /* 25 outgoing DMAP  */
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  ACTIVITY_ID = ",table_lookupAPS2FAquoted, WOS_activity, DTK_ACTID},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  STATION_ID = ", table_lookupAPS2FAquoted, ODL_dmap_station_id, 
                                                                DTK_STATION_ID},
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  ANTENNA_ID = ",    ODL_antenna_id,     NULL,   DTK_ANTENNA_ID},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  TRANSMITTER_ID = ",get_ODL_transid, WOS_transid,   DTK_TRANSID},

    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  FA_SCHEDULE_LINK = ",gen_string2qstr, NULL, DTK_FA_SCHEDULE_LINK},

    /*
    -- The following instructions for TIME_AOS and TIME_LOS require the 
    -- initialization of global variables fa_sat_id, fa_station_id, 
    -- fa_antenna_id.  This is done by the calling routine, before 
    -- the individual odl dtk record is processed.
    */
    /* 30 outgoing DMAP
    -- The following instruction will populate the TIME_AOS and TIME_LOS
    -- (the latter is accomplished using global variable fa_los_track_end)
    */
    {ODL_CONTROL, ODL_FILE_RECORD,   
        "  TIME_AOS = ",        get_aos_los_start_stop,     NULL,    
                                           (int) &odl_downlink_dtk_rec },
    {ODL_CONTROL, ODL_FILE_RECORD,   
        "  TIME_LOS = ",        gen_asftime2odl,            NULL,    
                                           (int) fa_los_track_end },
    {ODL_DTK_RECORD, ODL_FILE_RECORD,
        "  DOWNLINK_STATUS = ", table_lookupAPS2FAquoted, ODL_dmap_dtkstat,
                                                                 DTK_DTKSTAT},

    {ODL_CONTROL, ODL_FILE_RECORD,   
        "  NUMBER_OF_DTK_ENTRY = ",      get_obs_dtks,           NULL,    
                                           (int) &odl_downlink_dtk_rec },
    /* 34 outgoing DMAP  */
    {ODL_CONTROL, ODL_FILE_RECORD,   
        "  WRITE OBSERVATION DTKS ",     write_dmap_obs_dtks,    NULL,    
                                                     (int) &odl_obs_dtk_list },
        /*
        -- Here is where there are several entries 
        -- for subrecords for the downlink dtk record.  
        */
        {ODL_DEFAULT, ODL_FILE_SUBRECORD,  
            "  OBJECT = DTK_ENTRY",  NULL, NULL, NULL },

        /* 36 outgoing DMAP  */
        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,
            "    SENSOR = ",      table_lookupAPS2FAquoted, ODL_dmap_sensor, 
                                                                DTK_SENSOR },
        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,   
            "    REVOLUTION = ",gen_int2str,     NULL,          DTK_REV },

        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,
            "    SEQUENCE = ",gen_tinyint2str,   NULL,          DTK_DTKID },

        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,
            "    TIME_ON  = ",  gen_asftime2odl, NULL,          DTK_STRTTIME },

        /* 40 outgoing DMAP  */
        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,
            "    TIME_OFF = ",  gen_asftime2odl, NULL,          DTK_STOPTIME },

        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,
            "    SITE_NAME = ", gen_string2qstr, NULL,          DTK_SITENAME },

        {ODL_DTK_RECORD, ODL_FILE_SUBRECORD,
            "    MODE = ",      table_lookupAPS2FAquoted, ODL_dmap_mode, 
                                                                DTK_SENSOR},

        /* 
        --  2 fields, dtk.planner_quicklook and 
        -- dtk.science_quicklook, 
        -- must be logical OR'ed and the 
        -- value converted.  to compare, the first value 
        -- is saved into fa_dtk_quicklook_flag, a 
        -- global variable.  
        */
        {ODL_CONTROL, ODL_FILE_SUBRECORD,
            "    QUICKLOOK_FLAG = ",ODL_set_quicklook_flag, NULL,
                                                      (int) &odl_obs_dtk_rec},
        /* 44 outgoing DMAP  */
        {ODL_CONTROL,  ODL_FILE_SUBRECORD,   
            "    PROCESS_AUTH_FLAG = ",  ODL_set_auth_flag, NULL, 
                                                       (int) &odl_obs_dtk_rec},
        {ODL_CONTROL,  ODL_FILE_SUBRECORD,   
            "    FRAME_MODE = ",         ODL_set_frame_mode, NULL, 
                                                       (int) &odl_obs_dtk_rec},
        /* 46 outgoing DMAP  */
        {ODL_DEFAULT, ODL_FILE_SUBRECORD,  
            "  END_OBJECT = DTK_ENTRY",  NULL, NULL, NULL },

    {ODL_DEFAULT, ODL_FILE_RECORD,  
        "END_OBJECT = DL_TO_DTKS_ENTRY",  NULL, NULL, NULL },

    {ODL_DEFAULT, ODL_FILE_TRAILER,
        "END_OBJECT = ",  table_lookupAPS2FA,   WOS_object, (int) fa_file_type},

    /* 49 outgoing DMAP  */
    {ODL_DEFAULT, ODL_FILE_TRAILER,
        "END",      NULL,   NULL,       NULL },

    { 0, 0, NULL, NULL, NULL}
    /*
    --     NOTE:  this table definition is processed by 
    --     .../src/ODL_dtkf_c/ODL_dtkf_creator.c
    */

} ; 


/*==============================================================================
Function:       odl_create_file_info[]

Description:    provides, for each file type, information to help 
                create the file.  
Creator:        Lawrence Stevens

Creation Date:  Wed Apr  9 14:12:48 PDT 1997

==============================================================================*/
ODL_CREATE_FILENAME odl_create_file_info[] = 
{
/*  { "filetype", valuedef_pointer },  */
    { "AWOS", odl_wos_valuedefs  },
    { "MWOS", odl_wos_valuedefs  },
    { "AREQ", odl_wos_valuedefs  },
    { "ADDM", odl_dmap_valuedefs },  /* downlink to datatake map file */
    { "MDDM", odl_dmap_valuedefs },  /* downlink to datatake map file */
    { NULL, NULL }

} ;


/*==============================================================================
Function:       write_dmap_obs_dtks()

Description:    Write the observation dtks into the output file.  
                Make sure that result_string = "NONE" to prevent 
                further printing upon return.  

Creator:        Lawrence Stevens

Creation Date:  Wed Apr  9 16:52:39 PDT 1997

==============================================================================*/
int write_dmap_obs_dtks(
    void        *unused_pointer,
    llist       **dtk_llist_ptr,      /* read values from this llist   */
    char        *result_string )
{
    int         return_code ;
    llist       *obs_dtk_list ;
    cursor      obs_dtk_list_ptr ;
    DB_RECORD   **obs_dtk_rec ;

    /*
    -- Make sure that result_string = "NONE" to prevent 
    -- any printing by the handler upon return.  
    -- print only the subrecords.  
    */
    strcpy( result_string, "NONE" ) ;

    /* 
    -- set up this pointer for code use that will 
    -- look like other APS linked list code.  
    */
    obs_dtk_list = *dtk_llist_ptr ;

    for (   obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, obs_dtk_list_ptr);
            obs_dtk_rec != NULL ;
            obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, obs_dtk_list_ptr)  
        )
    {
        /*
        -- WRITE THE DTK RECORD INFO
        -- This will be a recursive call, since odl_dtkfile_create_record()
        -- is above the current routine in the call stack.  
        -- odl_dtkfile_create_record() is located in 
        -- ../src/ODL_dtkf_c/ODL_dtkf_creator.c
        */

        /* set up global pointer for access:  */
        odl_obs_dtk_rec = obs_dtk_rec ;

        return_code = odl_dtkfile_create_record( ODL_FILE_SUBRECORD, 
            odl_dmap_valuedefs, obs_dtk_rec, fa_output_file_fp ) ;
        if ( return_code < 0 )
            return FALSE ;
        /*
        -- DTK SUBRECORD IS COMPLETE
        */

    }

    /*
    -- DTK SUBRECORD IS COMPLETE
    */

    return TRUE ;
}
