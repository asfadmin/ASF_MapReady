#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       crt_dar_dtk.c
Description:    
External Functions Defined:
File Scope Functions:
External Variables Defined:
File Scope Variables:
==============================================================================*/
#pragma ident   "@(#)crt_dar_dtk.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/DAR2DTK/SCCS/s.crt_dar_dtk.c"


#include <stdlib.h>         /* for exit() etc.                            */
#include <string.h>         /* for strcmp() etc.                          */
#include <db_sybint.h>      /* for DBPROCESS                              */
#include <timeconv.h>       /* for tc_validate_asf_datetime() etc.        */
#include <phase_utilities.h>/* for asftime2rev(), asftime_2_phase() etc.  */
#include <aps_db_table.h>   /* for DAR, DTK                               */
#include <db_dar.h>         /* for CAST_DAR_SAT etc.                      */
#include <db_dtk.h>         /* for CAST_DTK_SAT etc.                      */
#include <db_phase.h>       /* for CAST_PHASE_ANTARCTIC_MODE etc.         */
#include "crt_dar_dtk.h"    /* for CRT_DAR_DTK_ERROR_MESSAGE() etc.       */
#include <aps_log_msg.h>    /* for APS_CRITICAL, etc.                     */
#include <mu_utilities.h>   /* for MU_DAR_ACTIVITY_TYPE, etc.             */
#include <dtkm_utilities.h> /* for DTKM_ACTID_RECORDING_OBSERVATION_CODE  */

/*
-- when we have more time: Tue Jul 15 18:42:03 PDT 1997
-- This routine was COPIED from 
-- dtkm_check_values.c, where it is a static 
-- routine, and not available to call.  
-- But it is used here.  Later, we can 
-- make it an available library routine.  
*/
#include <db_satsensor.h>   /* for SATSENSOR_SAT etc.                      */
static int
check_satsensor (
    char        *sat,
    char        *sensor )
{
    int         return_code;
 
    (void) sprintf(where_clause, "where %s = '%s' and %s = '%s' ",
        APS_COL(SATSENSOR, SATSENSOR_SAT),     sat,
        APS_COL(SATSENSOR, SATSENSOR_SENSOR),  sensor ) ;
 
    return_code = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(SATSENSOR), where_clause ) ;
 
    if ( return_code < 0 )
        return FALSE ;
 
    if ( return_code == 0 )
        return FALSE ;
 
    /* the values are OK.  */
    return TRUE ;
 
}



/*==============================================================================
Function:       crt_dar_dtk()

Description:    uses the data in the dar relation record to create a 
                dtk proposal that is then submitted 
                Satellite must be R1, during a Phase.  

Returns:        1 if dtk was created.  
                0 if no dtk was created - no errors in processing.  
                < 0 if an error was encountered.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jun 18 18:20:37 PDT 1997

==============================================================================*/
int crt_dar_dtk( 
    DBPROCESS       *APS_dbproc,
    char            *progname,
    int             permission_id, /* if != 0, DAR & planning permission id. */
    int             n_seconds_retry, /* seconds between Multi-User retries  */
    int             n_retries,       /* no. retries for Multi-User.         */
    int             darid,
    char            *dtkstat,    /* desired status of new dtk: SUB/PLN/SCH  */
    char            planner_quicklook, /* desired planner quicklook: Y | N  */
 
    char            ASFmaskinout_flag, /* Y means to use maskinout.         */
                                       /* if N: no realtime dtks at ASF.    */
 
    char            MCMmaskinout_flag, /* Y means to use maskinout.         */
                                       /* if N: no realtime dtks at MCM.    */
 
    int             max_minutes, /* Maximum allowed time duration.          */
    FILE            *rptfp  )    /* if != NULL, report file pointer.        */
{

    /* 
    -- NOTE:  care must be taken to always clean up before 
    --        returning.  Permissions may need to be terminated,
    --        memory may need to be freed.  
    --        Be really careful about this.  
    */
    int         return_code ;
    int         mu_return_code ;

    char   original_strttime[ASF_TIME_STR_LENGTH+1];
    char   original_stoptime[ASF_TIME_STR_LENGTH+1];
    char   reduced_strttime[ASF_TIME_STR_LENGTH+1];
    char   reduced_stoptime[ASF_TIME_STR_LENGTH+1];
    char   station_id[4] ;

    char        msg[256] ;

    DB_RECORD    **phase_rec ;
    DB_RECORD    **dar_rec ;
    llist       *dar_list ;
    cursor      dar_list_ptr ;

    DB_RECORD    **proposed_dtk ;
    DB_RECORD    **downlink_dtk ;
    llist        *dtk_list ;

    llist        *dtk_check_list ;
    DB_RECORD    **dtk_rec ;
    cursor       dtk_list_ptr ;

    double       delta_time_days ;
    double       delta_time_seconds ;

    int     provided_permission_id ;

    /* 
    -- the 3 characters must be pointers;  
    -- all are optional parameters for getting permissions.
    */
    char        *min_strttime = NULL ;
    char        *max_stoptime = NULL ;
    char        *perm_station_id = NULL ;
    int         permission_darid = 0 ;

    double      deltatime_days, deltatime_minutes ;
    int         revstrt, revend ;

    llist       *accepted_dtks ;
    llist       *rejected_dtks ;
    llist       *CON_dtks ;
    llist       *deleted_dtks ;
    llist       *error_dtks ;
    llist       *omission_dtks ;
    llist       *other_sat_dtks ;
    llist       *same_sat_dtks ;
    llist       *dtk_updates ;
    int         num_accepted_dtks ;

    /* 
    -- save the provided permission_id as a flag to indicate 
    -- whether permissions were obtained in this routine and 
    -- therefore need to be terminated in this routine.  
    */
    provided_permission_id = permission_id ;

    if( rptfp )
        (void) fprintf( rptfp, 
            "\n\nREPORT FILE FOR CREATING DTK FROM DAR %d\n\n", 
            darid ) ;

    /* error checking.  */
    if( planner_quicklook != 'N' 
    &&  planner_quicklook != 'Y' )
    {
        if( rptfp )
            (void) fprintf(rptfp, "ERROR:  %s \ninput planner quicklook = %c\n",
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_PLANNER_QUICKLOOK_NOT_Y_OR_N),
                    planner_quicklook ) ;
        return CRT_DAR_DTK_ERROR_PLANNER_QUICKLOOK_NOT_Y_OR_N ;
    }
    if( ASFmaskinout_flag != 'N' 
    &&  ASFmaskinout_flag != 'Y' )
    {
        if( rptfp )
            (void) fprintf(rptfp, "ERROR:  %s \ninput ASFmaskinout_flag = %c\n",
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_MASKINOUT_FLAG_NOT_Y_OR_N),
                    ASFmaskinout_flag ) ;
        return CRT_DAR_DTK_ERROR_MASKINOUT_FLAG_NOT_Y_OR_N ;
    }
    if( MCMmaskinout_flag != 'N' 
    &&  MCMmaskinout_flag != 'Y' )
    {
        if( rptfp )
            (void) fprintf(rptfp, "ERROR:  %s \ninput MCMmaskinout_flag = %c\n",
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_MASKINOUT_FLAG_NOT_Y_OR_N),
                    MCMmaskinout_flag ) ;
        return CRT_DAR_DTK_ERROR_MASKINOUT_FLAG_NOT_Y_OR_N ;
    }
    if( strcmp( dtkstat, "PLN" ) != 0 
    &&  strcmp( dtkstat, "SUB" ) != 0 
    &&  strcmp( dtkstat, "SCH" ) != 0  )
    {
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s \ninput dtk status = %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_DTKSTAT_NOT_PLN_SUB_OR_SCH),
                    dtkstat ) ;
        return CRT_DAR_DTK_ERROR_DTKSTAT_NOT_PLN_SUB_OR_SCH ;
    }

    if( darid <= 0 )
    {
        if( rptfp )
            (void) fprintf( rptfp, 
                "ERROR:  darid %d <= 0.  Terminating processing of DAR.\n", 
                darid  ) ;
        return CRT_DAR_DTK_ERROR_DARID_LE_ZERO ;
    }

    if( max_minutes <= 0 )
    {
        if( rptfp )
            (void) fprintf( rptfp, 
            "ERROR:  max_minutes %d <= 0.  Terminating processing of DAR.\n", 
                max_minutes  ) ;
        return CRT_DAR_DTK_ERROR_MAX_MINUTES_LE_ZERO ;
    }

    /* -- check this DAR.  */
    (void) sprintf(where_clause, "where %s = %d ",
        APS_COL(DAR, DAR_DARID), darid ) ;
 
    dar_list = db_get_records(APS_dbproc, APS_TABLE(DAR),
        where_clause, NULL, APS_CDEFS(DAR), ALL_COLS) ;
    if (dar_list == NULL)
    {
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_DB_QUERY_ERROR),
                where_clause ) ;
        return CRT_DAR_DTK_ERROR_DB_QUERY_ERROR ;
    }
    if( NUMELTS( dar_list ) <= 0 )
    {
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_DAR_NOT_FOUND),
                where_clause ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_DAR_NOT_FOUND ;
    }
    if( NUMELTS( dar_list ) > 1 )
    {
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_GT_ONE_DAR_FOUND),
                where_clause ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_GT_ONE_DAR_FOUND ;
    }
 
    /*
    -- one and only one dar was found for
    -- our input darid.  get this record.
    */
    dar_rec = FIRST(dar_list, dar_list_ptr) ;

    /*
    -- now that we have this dar, go over it
    -- and make sure it is inside a phase, not in a gap.  
    -- there is a lot of set-up first...
    */
    if( rptfp )
    {
        (void) fprintf( rptfp, "DARID = %d\n", darid ) ;
        (void) fprintf( rptfp, "               Requested dtk status = '%s'\n", 
            dtkstat ) ;
        (void) fprintf( rptfp, "               Requested quicklook = '%c'\n", 
            planner_quicklook ) ;
        (void) fprintf( rptfp, 
            "               Max allowed dtk time = %d minutes\n", 
            max_minutes ) ;
        if( ASFmaskinout_flag == 'Y' )
        {
            (void) fprintf( rptfp,
"               ASF Nominal mask entry/exit times checked to possibly \n" ) ;
            (void) fprintf( rptfp,
"               create a real-time data-take.\n" ) ;
        }
        else
        {
            (void) fprintf( rptfp,
"               ASF Nominal mask entry/exits not checked; no ASF realtimes.\n");
        }

        if( MCMmaskinout_flag == 'Y' )
        {
            (void) fprintf( rptfp,
"               MCM Nominal mask entry/exit times checked to possibly \n" ) ;
            (void) fprintf( rptfp,
"               create a real-time data-take and downlink.\n" ) ;
        }
        else
        {
            (void) fprintf( rptfp,
"               MCM Nominal mask entry/exits not checked; no MCM realtimes.\n");
        }

        (void) fprintf( rptfp, "\nVALUES IN THE INPUT DAR %d RECORD:\n", darid);
        db_fprint_record( rptfp, dar_rec, APS_CDEFS(DAR) ) ;
        (void) fprintf( rptfp, "\n" ) ;
    }
 
    /* check satellite   */
    if( strcmp( CAST_DAR_SAT dar_rec[DAR_SAT], "R1" ) != 0 )
    {
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s \nsat = %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_SAT_NOT_R1),
                CAST_DAR_SAT dar_rec[DAR_SAT] ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_SAT_NOT_R1 ;
    }
 
    /* check sat/sensor   */
    return_code = check_satsensor( CAST_DAR_SAT dar_rec[DAR_SAT], 
        CAST_DAR_SENSOR dar_rec[DAR_SENSOR] ) ;
    if( return_code != TRUE )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \nsensor = %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_SENSOR_NOT_USABLE),
                CAST_DAR_SENSOR dar_rec[DAR_SENSOR] ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_SENSOR_NOT_USABLE ;
    }
 
    /* check start time  */
    if ( tc_validate_asf_datetime( CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] )
         != TRUE )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \nstrttime = %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_BAD_STRTTIME ),
                CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_BAD_STRTTIME ;
    }
 
    /* check stop time   */
    if ( tc_validate_asf_datetime( CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] )
         != TRUE )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \nendtime = %s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( CRT_DAR_DTK_ERROR_BAD_ENDTIME  ),
                CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_BAD_ENDTIME ;
    }

    /* check time bracket length   */
    return_code = tc_et_ASF_datetime_diff(
        CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME],
        CAST_DAR_ENDTIME  dar_rec[DAR_ENDTIME],
        &deltatime_days ) ;
 
    deltatime_minutes = deltatime_days * 24 * 60 ;
    if( deltatime_minutes <= 0 )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \ntime delta = %f minutes\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_TIME_DURATION_LE_ZERO),
                deltatime_minutes ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_TIME_DURATION_LE_ZERO ;
    }
    /* 
    -- check to see if the time exceeds the max 
    -- allowed time duration:  
    */
    if( deltatime_minutes > (double) max_minutes )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, 
                "ERROR:  %s\ntime delta = %f minutes;  max allowed = %d\n",
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_TIME_DURATION_TOO_LONG_FOR_AMM),
                deltatime_minutes, max_minutes ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_TIME_DURATION_TOO_LONG_FOR_AMM ;
    }

    /*
    -- check to make sure that the start and end times are 
    -- in the same rev.  
    */
    (void) asftime2rev( CAST_DAR_SAT dar_rec[DAR_SAT], 
        CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME], &revstrt  ) ;
    (void) asftime2rev( CAST_DAR_SAT dar_rec[DAR_SAT], 
        CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME], &revend  ) ;
    if( revstrt == 0 || revend == 0 )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \ntimes = %s  %s \n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_ANY_PHASE),
                CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME],
                CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME]   ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_ANY_PHASE ;
    }

    if( revstrt != revend )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \nrevs = %d  %d \n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_SAME_REV),
                revstrt, revend ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_SAME_REV ;
    }

    /*
    -- verify that the rev is within 
    -- a phase.  
    */
    phase_rec = NULL ;
    /* 
    -- this function allocates memory, the dbrec phase_rec,
    -- which needs to be freed later:  
    */
    return_code = asftime_2_phase( 
        CAST_DAR_SAT dar_rec[DAR_SAT],
        CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME],
        &phase_rec ) ;
    if( phase_rec == NULL )
    {
        /* error.   */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR:  %s   \nrev = %d\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_AMM_PHASE),
                revstrt ) ;
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_TIMES_NOT_WITHIN_AMM_PHASE ;

    }

    /* 
    -- no errors found.  looks OK.  
    -- create a dtk db record and then submit it.  
    */
    /* clean up.  */
    free_db_record(phase_rec) ;

    /* 
    -- However, get permissions first.  
    -- 1.  get DAR activity permission
    -- 2.  get planning activity permission
    --     if not obtained, then release DAR activity permission
    --     and then return.  
    -- Do work.  
    -- release permissions.  
    */

    /*
    -- At this point, we need to either validate a permission
    -- or to request a new permission.
    -- mu_get_permission() takes care of
    -- the logic and the syslog printing.  for
    -- example, it will determine to request a new
    -- permission or to validate an old permission.
    -- it is all taken care of in mu_get_permission()
    */

    /*
    -- prepare parameters based 
    -- on input parameter (provided) permission_id
    */
    if( provided_permission_id <= 0 )
    {
        /* 
        -- must get permission and supply various values 
        -- to permission routines.  
        */
        permission_darid = darid ;
        min_strttime = CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] ;
        max_stoptime = CAST_DAR_ENDTIME  dar_rec[DAR_ENDTIME] ;
        perm_station_id = "ALL" ;
    }
    else
    {
        /* 
        -- Permission already obtained.  must 
        -- validating permissions this time.  
        -- no retries:  
        */
        n_seconds_retry = 0 ;
        n_retries = 0 ;
    }

    mu_return_code = mu_get_permission( progname, APS_dbproc,
        permission_id, MU_DAR_ACTIVITY_TYPE, MU_CREATE_DATATAKES_FROM_DAR,
        NULL, NULL, NULL, /* parameters not used for DAR permission.       */
        permission_darid,    /* value must be 0 if permission_id given in arg */
        n_retries, n_seconds_retry ) ;
    if ( mu_return_code < 0 )
    {
        /* permission not granted/validated.  */
        /* error.  */
        if( rptfp )
        {
            (void) fprintf( rptfp, "ERROR:  %s\ndarid = %d\n", 
                MU_ERROR_MESSAGE(mu_return_code), darid ) ;
            (void) fprintf( rptfp, "%s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_DAR_PERMISSION_NOT_GRANTED)  ) ;
        }
        DEL_LIST( dar_list ) ;
        return CRT_DAR_DTK_ERROR_DAR_PERMISSION_NOT_GRANTED ;
    }
    /* permission obtained/validated; set id.  */
    permission_id = mu_return_code ;
 
    /*
    -- now obtain the planning activity.  Note that the
    -- permission_id value already obtained for the
    -- DAR activity will also be used for the planning
    -- activity.  At this point, the permission_id will
    -- always be != 0
    */
 
    /*
    -- At this point, we need to either validate a permission
    -- or to request a new permission.
    -- mu_get_permission() takes care of
    -- the logic and the syslog printing.  for
    -- example, it will determine to request a new
    -- permission or to validate an old permission.
    -- it is all taken care of in mu_get_permission()
    */
    mu_return_code = mu_get_permission( progname, APS_dbproc,
        permission_id, MU_PLANNING_ACTIVITY_TYPE, MU_CREATE_DATATAKES_FROM_DAR,
        min_strttime, /* value will be NULL if permission_id given in arg */
        max_stoptime, /* value will be NULL if permission_id given in arg */
        perm_station_id, /* value will be NULL if permission_id given in arg */
        0,       /* darid not used in planning permission.  */
        n_retries, n_seconds_retry ) ;
    if ( mu_return_code < 0 )
    {
        /* 
        -- permission not granted/validated. 
        -- terminate DAR permission and then return.  
        */
        if( rptfp )
        {
            (void) fprintf( rptfp, "ERROR:  %s\ndarid = %d\n", 
                MU_ERROR_MESSAGE(mu_return_code), darid);
            if( min_strttime && max_stoptime && perm_station_id )
                (void) fprintf( rptfp, 
                    "times = %s  %s\nperm_station_id = %s\n\n", 
                    min_strttime, max_stoptime, perm_station_id ) ;
            else
                (void) fprintf(rptfp,
                    "Planning Permission id being validated = %d\n\n",
                    permission_id ) ;
            (void) fprintf( rptfp, "%s\n", 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_PLANNING_PERMISSION_NOT_GRANTED) );
        }

        /* cleanup:  */
        /* must terminate the permissions that were obtained here.  */
        if( provided_permission_id <= 0 )
            (void) mu_permission_terminate( APS_dbproc, permission_id, 
                MU_CREATE_DATATAKES_FROM_DAR, MU_DAR_ACTIVITY_TYPE ) ;
        DEL_LIST( dar_list ) ;

        return CRT_DAR_DTK_ERROR_PLANNING_PERMISSION_NOT_GRANTED ;
    }

    /* 
    -- permission obtained; set id.  
    -- terminate permissions later.  
    */
    permission_id = mu_return_code ;

    /* 
    -- create a DB_RECORD structure and memory, then 
    -- load blank or zero values into the fields:
    */
    proposed_dtk =  new_table_record(APS_CDEFS(DTK)) ;
    (void) dtkm_blank_values( proposed_dtk, proposed_dtk ) ;

    /* 
    -- now load all the fields we can.  
    -- the other fields will be filled in by dtkm_default_values() 
    */

    /*   1 sat                   SatType          2  */
    (void)strcpy( CAST_DTK_SAT proposed_dtk[DTK_SAT], 
        CAST_DAR_SAT dar_rec[DAR_SAT]);

    /*   2 sensor                SensorType       3  */
    (void)strcpy( CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR], 
        CAST_DAR_SENSOR dar_rec[DAR_SENSOR] ) ;

    /*   3 rev                   RevType          4  */
    CAST_DTK_REV proposed_dtk[DTK_REV] = revstrt ;

    /*   4 dtkid                 DtkidType        1  */
    /*   5 fadtkid               char            20  */
    /*   6 darid                 DaridType        4  */
    CAST_DTK_DARID proposed_dtk[DTK_DARID] = darid ;

    /*   7 actid                 ActidType        6  */
    (void)strcpy( CAST_DTK_ACTID proposed_dtk[DTK_ACTID], 
        DTKM_ACTID_RECORDING_OBSERVATION_CODE ) ;
    /* the final 3 characters will be filled in by dtkm_default_values()  */

    /*   8 ascdsc                AscdscType       1  */
    CAST_DTK_ASCDSC proposed_dtk[DTK_ASCDSC] 
        = CAST_DAR_ASCDSC dar_rec[DAR_ASCDSC] ;

    /*   9 strttime              AsftimeType     21  */
    (void)strcpy( CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME], 
        CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] ) ;

    /*  10 stoptime              AsftimeType     21  */
    (void)strcpy( CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME], 
        CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] ) ;

    /*  11 strtlat               Latitude4Type    4  */
    /*  12 stoplat               Latitude4Type    4  */
    /*  13 nrlat1                Latitude4Type    4  */
    /*  14 nrlon1                Longitude4Type   4  */
    /*  15 farlat1               Latitude4Type    4  */
    /*  16 farlon1               Longitude4Type   4  */
    /*  17 nrlat2                Latitude4Type    4  */
    /*  18 nrlon2                Longitude4Type   4  */
    /*  19 farlat2               Latitude4Type    4  */
    /*  20 farlon2               Longitude4Type   4  */
    /*  21 lookangl              real             4  */

    /*  22 dtkstat               DtkstatType      3  */
    (void)strcpy( CAST_DTK_DTKSTAT proposed_dtk[DTK_DTKSTAT], dtkstat ) ;

    /*  23 proposed_dtkstat      DtkstatType      3  */
    (void)strcpy( CAST_DTK_PROPOSED_DTKSTAT proposed_dtk[DTK_PROPOSED_DTKSTAT], 
        dtkstat ) ;

    /*  24 transid               TransidType      2  */
    /*  25 sitename              SitenameType    32  */
    (void)strcpy( CAST_DTK_SITENAME proposed_dtk[DTK_SITENAME], 
        CAST_DAR_SITENAME dar_rec[DAR_SITENAME]  ) ;

    /*  26 notes                 varchar         40  */
    (void)strcpy( CAST_DTK_NOTES proposed_dtk[DTK_NOTES], "From AMM DAR" ) ;

    /*  27 dtkdate               AsfdateType      8  */
    /*  28 station_id            StationidType    3  */
    (void)strcpy( CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID], "ASF" ) ;

    /*  29 fa_schedule_link      char            20  */
    (void)strcpy( CAST_DTK_FA_SCHEDULE_LINK proposed_dtk[DTK_FA_SCHEDULE_LINK],
                    "" ) ;

    /*  30 planner_quicklook     YesNoType        1  */
    CAST_DTK_PLANNER_QUICKLOOK proposed_dtk[DTK_PLANNER_QUICKLOOK] 
        = planner_quicklook ;

    /*  31 science_quicklook     YesNoType        1  */
    CAST_DTK_SCIENCE_QUICKLOOK proposed_dtk[DTK_SCIENCE_QUICKLOOK] = 
        CAST_DAR_QUICKLOOK dar_rec[DAR_QUICKLOOK] ;

    /*  32 submit_time           AsftimeType     21  */
    /*  33 antenna_id            Antenna_idType   2  */

#ifdef PRINT_DIAG
    if( rptfp )
    {
        (void) fprintf(rptfp, "\nPRELIMINARY DTK VALUES, OBTAINED FROM DAR:\n");
        db_fprint_record( rptfp, proposed_dtk, APS_CDEFS(DTK) ) ;
        (void) fprintf( rptfp, "\n" ) ;
    }
#endif

    return_code = dtkm_default_values( proposed_dtk, proposed_dtk ) ;
    if( return_code < 0 )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR from dtkm_default_values():  %s \n", 
                DTKM_ERROR_MESSAGE( return_code ) ) ;

        /* cleanup:  */
        /* terminate permissions here!!   */
        if( provided_permission_id <= 0 )
        {
            /* must terminate the permissions that were obtained here.  */
            (void) mu_permission_terminate( APS_dbproc, permission_id, 
                MU_CREATE_DATATAKES_FROM_DAR, MU_DAR_ACTIVITY_TYPE ) ;
            (void) mu_permission_terminate( APS_dbproc, permission_id, 
                MU_CREATE_DATATAKES_FROM_DAR, MU_PLANNING_ACTIVITY_TYPE ) ;
        }
        DEL_LIST( dar_list ) ;
        free_db_record( proposed_dtk ) ;

        return CRT_DAR_DTK_ERROR_GETTING_DEFAULT_VALUES ;
    }


    /* 
    -- so far, OK.  
    -- it is best for us to put it into a list 
    -- then call dtkm_process_dtk_proposal_list()
    -- since there is a lot of error handling on the 
    -- return from the call for a single proposal.  
    -- here is the list:  
    */
    dtk_list = create_dyn_llist() ;

    if( ASFmaskinout_flag == 'Y' 
    ||  MCMmaskinout_flag == 'Y' )
    {
        /* special processing; realtime dtks are possible.  */
        /*
        -- check to see if the dtk is within the station masks
        -- if so, then put the station_id into the dtk record
        -- and make it a REALTIME observation if the 
        -- correct maskinout_flag is set.  
        -- if not, then leave it alone, a recording.  
        */
        int     minimum_seconds = 0 ;

        return_code = dtkm_determine_station_mask( proposed_dtk,
            minimum_seconds, reduced_strttime, reduced_stoptime, station_id ) ;
        if ( return_code < 0 )
        {
            /* ERROR.  */
            if ( rptfp )
            {
                (void) fprintf(rptfp,
                    "ERROR from dtkm_determine_station_mask():  %s\n",
                     DTKM_ERROR_MESSAGE( return_code )  ) ;
            }

            /*
            -- cleanup:
            -- terminate permissions here!!
            */
            if( provided_permission_id <= 0 )
            {
                /* must terminate the permissions that were obtained here.  */
                (void) mu_permission_terminate( APS_dbproc, permission_id, 
                    MU_CREATE_DATATAKES_FROM_DAR, MU_DAR_ACTIVITY_TYPE ) ;
                (void) mu_permission_terminate( APS_dbproc, permission_id, 
                    MU_CREATE_DATATAKES_FROM_DAR, MU_PLANNING_ACTIVITY_TYPE ) ;
            }
            DEL_LIST( dar_list ) ;
            DEL_LIST( dtk_list ) ;
            free_db_record( proposed_dtk ) ;
            return CRT_DAR_DTK_ERROR_DETERMINING_MASK ;

        }
     
        /* check to see if the dtk is within
        -- a mask with a Y indicating that 
        -- realtimes are desired in that mask.  
        -- 
        -- Using successive, non-indented if-statements, 
        -- to try to make the condition more clear.  
        -- if there is no time in mask, skip everything.  
        */
        if(  return_code == DTKM_DTK_HAS_TIME_IN_MASK )
        if(  (strcmp( station_id, "ASF" ) == 0  &&  ASFmaskinout_flag == 'Y')
        ||   (strcmp( station_id, "MCM" ) == 0  &&  MCMmaskinout_flag == 'Y')  )
        {
            /* 
            -- This dtk has time in a mask.  
            -- Also, the mask that it is in, station_id, is 
            -- a mask that has a Y value to indicate a desire 
            -- for a realtime downlink there.  
            -- Therefore, we create a realtime observation.  
            */
         
#ifdef PRINT_DIAG
            printf("%s(%d):  observation is in mask.\n", 
                __FILE__, __LINE__ );
#endif

            if ( rptfp )
            {
                (void) fprintf(rptfp,
"\nNOTE:  data-take being created as a Realtime data-take.\n\n");
            }

            (void) strncpy(CAST_DTK_ACTID proposed_dtk[DTK_ACTID],
                    DTKM_ACTID_REALTIME_OBSERVATION_CODE, 
                    strlen( DTKM_ACTID_REALTIME_OBSERVATION_CODE)  ) ;

            (void) strcpy(CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID], 
                    station_id);

#ifdef CREATE_DOWNLINK_RECORD
            /*
            -- this data-take is a real-time observation; 
            -- we will later create the downlinking data-take, too. 
            -- The reason is to do conflicts.
            -- but here we will put in a value for fa_schedule_link,
            -- and the same value in the downlink, too.  This will
            -- link them up; the APS will know that one dtk downlinks
            -- the other.
            */
            (void) sprintf( CAST_DTK_FA_SCHEDULE_LINK 
                proposed_dtk[DTK_FA_SCHEDULE_LINK],
                "%ld_%sdar%d",
                CAST_DTK_REV proposed_dtk[DTK_REV],
                CAST_DTK_STATION_ID proposed_dtk[DTK_STATION_ID],
                darid ) ;

            /*
            -- now create the downlink record.
            -- set up NEW storage to store the record
            -- copy all of the values to the new record.  
            -- must free the memory later:
            */
            downlink_dtk = db_duplicate_record( APS_CDEFS(DTK), proposed_dtk ) ;

            /*
            -- transform this new copy into a
            -- real-time DOWNLINK record.
            */
            (void) strcpy(CAST_DTK_SENSOR downlink_dtk[DTK_SENSOR],
                   DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ;
 
            (void) strncpy(CAST_DTK_ACTID downlink_dtk[DTK_ACTID],
                    DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
                    strlen(DTKM_SENSOR_REALTIME_DOWNLINK_CODE) ) ;
            /*
            -- set the TRANSID to "" to be later 
            -- assigned by dtkm_default values()
            */
            (void) strcpy( CAST_DTK_TRANSID downlink_dtk[DTK_TRANSID], "" ) ;
 
            return_code = dtkm_default_values( downlink_dtk, downlink_dtk ) ;
            if ( return_code < 0 )
            {
                if ( rptfp )
                {
                    (void) fprintf(rptfp,
                        "ERROR from dtkm_default_values():  %s\n",
                        DTKM_ERROR_MESSAGE( return_code )  ) ;
                    (void) fprintf(rptfp,
"Real-time downlink data-take NOT ADDED to the list for processing:\n") ;
                    dtkm_print( rptfp, downlink_dtk ) ;
                }

                /*
                -- cleanup:
                -- terminate permissions here!!
                */
                if( provided_permission_id <= 0 )
                {
                    /* 
                    -- must terminate the permissions that 
                    -- were obtained in this routine.  
                    */
                    (void) mu_permission_terminate( APS_dbproc, permission_id, 
                        MU_CREATE_DATATAKES_FROM_DAR, 
                        MU_DAR_ACTIVITY_TYPE ) ;
                    (void) mu_permission_terminate( APS_dbproc, permission_id, 
                        MU_CREATE_DATATAKES_FROM_DAR, 
                        MU_PLANNING_ACTIVITY_TYPE ) ;
                }
                DEL_LIST( dar_list ) ;
                DEL_LIST( dtk_list ) ;
                free_db_record( proposed_dtk ) ;
                free_db_record( downlink_dtk ) ;
                return CRT_DAR_DTK_ERROR_GETTING_DEFAULT_VALUES ;
            }
            /*
            -- add this downlink data-take to the list to be processed;
            */
            APPEND( dtk_list, downlink_dtk, free_db_record, downlink_dtk ) ;
            if( rptfp )
            {
                (void) fprintf( rptfp, "REALTIME DOWNLINK DTK VALUES:\n");
                db_fprint_record( rptfp, downlink_dtk, APS_CDEFS(DTK) ) ;
                (void) fprintf( rptfp, "\n" ) ;
            }

#endif    /*   if def  CREATE_DOWNLINK_RECORD     */


        } /* endif time in desired mask.  */

    }  /* endif at least one maskinout_flag == 'Y'  */


    return_code = dtkm_check_values( proposed_dtk, proposed_dtk ) ;
    if( return_code < 0 )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, "ERROR from dtkm_check_values():  %s \n", 
                DTKM_ERROR_MESSAGE( return_code ) ) ;
        /*
        -- cleanup:
        -- terminate permissions here!!
        */
        if( provided_permission_id <= 0 )
        {
            /* must terminate the permissions that were obtained here.  */
            (void) mu_permission_terminate( APS_dbproc, permission_id, 
                MU_CREATE_DATATAKES_FROM_DAR, MU_DAR_ACTIVITY_TYPE ) ;
            (void) mu_permission_terminate( APS_dbproc, permission_id, 
                MU_CREATE_DATATAKES_FROM_DAR, MU_PLANNING_ACTIVITY_TYPE ) ;
        }
        DEL_LIST( dar_list ) ;
        DEL_LIST( dtk_list ) ;   /* this will also delete downlink_dtk */
        free_db_record( proposed_dtk ) ;
        return CRT_DAR_DTK_ERROR_FROM_DTKM_CHECK_VALUES ;
    }

    /*
    -- If there is an existing, time-overlapping observation 
    -- dtk with same sat/sensor/rev/actid[1-3]
    -- with status of PLN, then alter the time bracket 
    -- of proposed_dtk to include both/all of the 
    -- data-takes.  
    */

    (void) sprintf( where_clause, 
"where %s = '%s' and %s = '%s' and %s = %ld and %s like '%3.3s%%' and %s <= '%s' and %s >= '%s' ",
        APS_COL(DTK, DTK_SAT),      CAST_DTK_SAT proposed_dtk[DTK_SAT],
        APS_COL(DTK, DTK_SENSOR),   CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR],
        APS_COL(DTK, DTK_REV),      CAST_DTK_REV proposed_dtk[DTK_REV],
        APS_COL(DTK, DTK_ACTID),    CAST_DTK_ACTID proposed_dtk[DTK_ACTID],
    APS_COL(DTK, DTK_STRTTIME), CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
    APS_COL(DTK, DTK_STOPTIME), CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME] ) ;

    dtk_check_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
    if (dtk_check_list == NULL)
    {
        /* error.  */
        (void) sprintf (msg, " %s:(%d):\n     %s\nwhere_clause = \n%s", __FILE__, __LINE__, 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_DB_QUERY_ERROR),
                where_clause ) ;

        if( rptfp )
            (void) fprintf( rptfp,  "\n%s:  ERROR:  %s\n\n", progname, msg ) ;
        (void) fprintf(     stderr, "\n%s:  ERROR:  %s\n\n", progname, msg ) ;

        aps_log_msg( progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT ) ;

        return CRT_DAR_DTK_ERROR_DB_QUERY_ERROR ;
    }

    /* 
    -- a (possibly empty) list of dtks that time-overlap the proposed_dtk 
    -- has been retrieved from the DB.  as/if/when necessary, expand the 
    -- proposed_dtk time bracket to include these data-takes.  
    */
    for (   dtk_rec = (DB_RECORD **) FIRST(dtk_check_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dtk_check_list, dtk_list_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        if( strcmp( CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
                    CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) 
            > 0 ) 
        {
            /* 
            --  update the start time, making the 
            --  proposed_dtk LONGER.  
            */
            (void) strcpy( CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
                    CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME] ) ;
            if( rptfp )
                (void) fprintf( rptfp, 
"\nNOTE:  start time expanded to include existing dtk %s/%s/%5.5ld/%2.2d\n", 
                    CAST_DTK_SAT    dtk_rec[DTK_SAT],
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                    CAST_DTK_REV    dtk_rec[DTK_REV],
                    CAST_DTK_DTKID  dtk_rec[DTK_DTKID]  ) ;
        }

        if( strcmp( CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
                    CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) 
            < 0 ) 
        {
            /* 
            --  update the stop time, making 
            --  the proposed_dtk LONGER.  
            */
            (void) strcpy( CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
                    CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME] ) ;
            if( rptfp )
                (void) fprintf( rptfp, 
"\nNOTE:  stop time expanded to include existing dtk %s/%s/%5.5ld/%2.2d\n", 
                    CAST_DTK_SAT    dtk_rec[DTK_SAT],
                    CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                    CAST_DTK_REV    dtk_rec[DTK_REV],
                    CAST_DTK_DTKID  dtk_rec[DTK_DTKID]  ) ;
        }
    }
    /* clean up the list.  */
    DEL_LIST( dtk_check_list ) ;

    /*
    -- NOTE:  if the (combined) datatake is less than 30 seconds, 
    -- print a warning.  
    */
    return_code = tc_et_ASF_datetime_diff( 
        CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
        CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME], 
        &delta_time_days ) ;
    if( return_code != TRUE )
    {
        /* error.  */
        (void) sprintf (msg, " %s:(%d):\n     %s\n\ttimes=%s %s", __FILE__, __LINE__, 
                CRT_DAR_DTK_ERROR_MESSAGE( 
                    CRT_DAR_DTK_ERROR_DETERMINING_DELTA_TIME),
                CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
                CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME] ) ;

        if( rptfp )
            (void) fprintf( rptfp,  "\n%s:  ERROR:  %s\n\n", progname, msg ) ;
        (void) fprintf(     stderr, "\n%s:  ERROR:  %s\n\n", progname, msg ) ;

        aps_log_msg( progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT ) ;

        return CRT_DAR_DTK_ERROR_DETERMINING_DELTA_TIME ;

    }

    delta_time_seconds = delta_time_days * 24.0 * 3600.0 ;
    if( delta_time_seconds < 30.0 )
    {
        (void) sprintf (msg, 
" Proposed data-take %s/%s/%5.5ld/%3.3s for DAR %d is only %.1f seconds long.", 
            CAST_DTK_SAT    proposed_dtk[DTK_SAT],
            CAST_DTK_SENSOR proposed_dtk[DTK_SENSOR],
            CAST_DTK_REV    proposed_dtk[DTK_REV],
            CAST_DTK_ACTID  proposed_dtk[DTK_ACTID],
            darid, delta_time_seconds ) ;
        if( rptfp )
            (void) fprintf( rptfp, "\n%s:  WARNING:  %s\n\n", progname, msg ) ;
        (void) fprintf(     stderr,"\n%s:  WARNING:  %s\n\n", progname, msg ) ;

        aps_log_msg( progname, APS_WARNING, msg, DO_SYSLOG, DO_PRINT ) ;
    }

    /*
    -- add the observation data-take to the list to be processed;
    */
    APPEND( dtk_list, proposed_dtk, free_db_record, proposed_dtk ) ;

    if( rptfp )
    {
        (void) fprintf( rptfp, 
            "\nAPPROVED OBSERVATION DTK VALUES, WITH ADDED DEFAULTS:\n");
        db_fprint_record( rptfp, proposed_dtk, APS_CDEFS(DTK) ) ;
        (void) fprintf( rptfp, "\n" ) ;
        (void) fprintf( rptfp, 
            "This data-take proposal will now be submitted\n" ) ;
        (void) fprintf( rptfp, 
            "and processed like an FA file with %d dtk(s).\n\n", 
            NUMELTS( dtk_list )  ) ;
    }


    /*
    -- PROCESS THE DTK LIST with the 1 or 2 dtks in it.  
    */
    accepted_dtks = create_dyn_llist();
    rejected_dtks = create_dyn_llist();
    CON_dtks = create_dyn_llist() ;
    deleted_dtks = create_dyn_llist() ;
    error_dtks    = create_dyn_llist();
    omission_dtks = create_dyn_llist() ;
    other_sat_dtks = create_dyn_llist() ;
    same_sat_dtks = create_dyn_llist() ;
    dtk_updates = create_dyn_llist() ;
 
    return_code = dtkm_process_dtk_proposal_list (APS_dbproc, 
        dtk_list,
        accepted_dtks, rejected_dtks, CON_dtks, deleted_dtks,
        error_dtks, DTKM_DO_NOT_REJ_BY_OMISSION, 
        other_sat_dtks, same_sat_dtks, dtk_updates,
        rptfp );
    /*
    -- since the work is all done now, we 
    -- terminate permissions right here:  
    */
    if( provided_permission_id <= 0 )
    {
        /* must terminate the permissions that were obtained here.  */
        (void) mu_permission_terminate( APS_dbproc, permission_id, 
            MU_CREATE_DATATAKES_FROM_DAR, MU_DAR_ACTIVITY_TYPE ) ;
        (void) mu_permission_terminate( APS_dbproc, permission_id, 
            MU_CREATE_DATATAKES_FROM_DAR, MU_PLANNING_ACTIVITY_TYPE ) ;
    }
    /* partial clean up now.  */
    DEL_LIST( rejected_dtks ) ;
    DEL_LIST( CON_dtks ) ;
    DEL_LIST( deleted_dtks ) ;
    DEL_LIST( omission_dtks ) ;
    DEL_LIST( other_sat_dtks ) ;
    DEL_LIST( same_sat_dtks ) ;
    DEL_LIST( dtk_updates ) ;
    DEL_LIST( dar_list ) ;
    DEL_LIST( dtk_list ) ;

    if( return_code < 0 )
    {
        /* error.  */
        if( rptfp )
            (void) fprintf( rptfp, 
                "ERROR from dtkm_process_dtk_proposal_list():  %s \n", 
                DTKM_ERROR_MESSAGE(return_code) ) ;
        DEL_LIST( accepted_dtks ) ;
        DEL_LIST( error_dtks    ) ;

        return CRT_DAR_DTK_ERROR_IN_PROCESSING_DTK_PROPOSAL ;
    }
    if ( NUMELTS (error_dtks) > 0 )
    {
        (void) sprintf (msg, "ERROR in %d data-takes", NUMELTS (error_dtks)) ;
        aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
        (void) sprintf (msg,
            "PLANNER MUST REVIEW DAR DTK REPORT FILE ERROR MESSAGES.\n") ;
        aps_log_msg(progname, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
 
        (void) fprintf (rptfp, "\nPLANNER MUST REVIEW ABOVE ERROR MESSAGES.\n");

        DEL_LIST( accepted_dtks ) ;
        DEL_LIST( error_dtks    ) ;

        return CRT_DAR_DTK_ERROR_IN_PROCESSING_DTK_PROPOSAL ;
    }
    /* done with error_dtks llist  */
    DEL_LIST( error_dtks    ) ;

    num_accepted_dtks = NUMELTS( accepted_dtks ) ;
    DEL_LIST( accepted_dtks ) ;

    if( rptfp  )
        (void) fprintf(rptfp, "\nPROCESSING OF DAR %d IS COMPLETE.\n\n", darid);

    return num_accepted_dtks ;

}
