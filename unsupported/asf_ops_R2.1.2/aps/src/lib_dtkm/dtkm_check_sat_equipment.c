#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_check_sat_equipment.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_sat_equipment.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_sat_equipment.c"

#include "dtkm.h"
#include "db_dtk.h"
#include "db_j1_dn_times.h"
#include "timeconv.h"   /* for tc_et_ASF_datetime_diff()  */

#include <string.h>     /* for strcpy strcmp  */

static char     TIME_CODE[] = "*TIME*" ;
static char     DOWN_CODE[] = "*DOWN*" ;


/*==============================================================================
Function:   dtkm_j1_equipment_status_check

Description:    This is a big coding fragment from the routine 
dtkm_j1_equipment_status that needs to be repeated 6 times.  It is a 
collection of different steps put together here only for that reason.

It processes an individual down time activity gleaned from a 
j1_dn_times record.  A single j1_dn_times record can have anywhere 
from 0 to six activities implied by it - the various 
different equipments being down that would prevent the 
proposed data-take from taking place.  

Parameters:     
    int         proposed_dtk_activity_id, 
    DB_RECORD   **temp_dtk_rec,           this is a place holder for 
                                          down times info.  it is used to 
                                          look up in the activities relation
                                          to get the id for the down condition.
    llist       *dtk_sat_down_times,      linked list to hold the down time.
    DB_RECORD   **j1_dn_times_rec,        the current down time record. 

Returns:        
    int normal status:
            DTKM_ACTIVITIES_DO_NOT_CONFLICT 
            DTKM_SAT_EQUIPMENT_NOT_OK 

        errors:
            DTK_ERROR_ACTIVITY_ILLEGAL
            DTK_ERROR_IN_CODE_dtkm_j1_equipment_status_check 

Notes:

==============================================================================*/
static int dtkm_j1_equipment_status_check( 
    int         proposed_dtk_activity_id, 
    DB_RECORD   **temp_dtk_rec,   /* used to get the down activity id */
    llist       *dtk_sat_down_times, 
    DB_RECORD   **j1_dn_times_rec ) 
{
    int     conflict_status ;
    int     down_activity_id ; 
    DB_RECORD   **dtk_sat_down_times_rec = NULL ;

    /* use the temp_dtk_rec to obtain the down activity id.  */
    down_activity_id = dtkm_activity_id(temp_dtk_rec) ; 
    if ( down_activity_id <= 0 )
    {
        return DTKM_ERROR_ACTIVITY_ILLEGAL ;
    }

    conflict_status = activities_conflict( proposed_dtk_activity_id, 
        down_activity_id ) ;

#ifdef PRINT_DIAG
    PRINT_DIAG( "dtkm_j1_equipment_status_check.c:  conflict_status = %d\n", 
        conflict_status ) ;
#endif
    if ( conflict_status == DTKM_ACTIVITIES_DO_NOT_CONFLICT )
        return DTKM_ACTIVITIES_DO_NOT_CONFLICT ;

    /* 
    -- the status was not DTKM_ACTIVITIES_DO_NOT_CONFLICT.  
    -- in other words, the proposed dtk CANNOT take place.  
    -- Now save the J1 equipment down time info to the dtk_sat_down_times 
    -- list, clean up, then return the proper code.
    */
    dtk_sat_down_times_rec =  new_table_record(APS_CDEFS(DTK)) ;
    db_copy_record( APS_CDEFS(DTK), dtk_sat_down_times_rec, temp_dtk_rec ) ;

    strcpy(CAST_DTK_STRTTIME dtk_sat_down_times_rec[DTK_STRTTIME],   
        CAST_J1_DN_TIMES_STRTTIME   j1_dn_times_rec[J1_DN_TIMES_STRTTIME] ) ;
    strcpy(CAST_DTK_STOPTIME dtk_sat_down_times_rec[DTK_STOPTIME],   
        CAST_J1_DN_TIMES_STOPTIME   j1_dn_times_rec[J1_DN_TIMES_STOPTIME] ) ;

    /* set the antenna id to 0 for printing.  */
    CAST_DTK_ANTENNA_ID dtk_sat_down_times_rec[DTK_ANTENNA_ID] = 0 ;

    /* 
    -- the dtk_sat_down_times_rec now goes into the dtk_sat_down_times list.  
    -- make more clear use of the fields in order to 
    -- improve the reporting of this down time activity that caused the 
    -- proposed dtk to be cancelled.  
    */
    if ( strcmp(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], "SDN") == 0 )
    {
        strcpy(CAST_DTK_SENSOR dtk_sat_down_times_rec[DTK_SENSOR], "SAR") ;
        strcpy(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], DOWN_CODE) ;
    }
    else if ( strcmp(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], "ODN") == 0 )
    {
        strcpy(CAST_DTK_SENSOR dtk_sat_down_times_rec[DTK_SENSOR], "OPS") ;
        strcpy(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], DOWN_CODE) ;
    }
    else if ( strcmp(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], "VDN") == 0 )
    {
        strcpy(CAST_DTK_SENSOR dtk_sat_down_times_rec[DTK_SENSOR], "VNR") ;
        strcpy(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], DOWN_CODE) ;
    }
    else if ( strcmp(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], "RDN") == 0 )
    {
        strcpy(CAST_DTK_SENSOR dtk_sat_down_times_rec[DTK_SENSOR], "MDR") ;
        strcpy(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], DOWN_CODE) ;
    }
    else if ( strcmp(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], "TDN") == 0 )
    {
        strcpy(CAST_DTK_SENSOR dtk_sat_down_times_rec[DTK_SENSOR], "MDT") ;
        strcpy(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], DOWN_CODE) ;
    }

    APPEND( dtk_sat_down_times, dtk_sat_down_times_rec, free_db_record, 
            dtk_sat_down_times_rec ) ;

    /* use the proper return code according to the conflict status:  */
    switch(conflict_status)
    {

        case DTKM_ACTIVITIES_CONFLICT :
            return DTKM_SAT_EQUIPMENT_NOT_OK ;
            break ;

        case DTKM_ACTIVITIES_DO_NOT_CONFLICT :
            printf(" dtkm_j1_equipment_status.c:  ERROR in code.\n");
            printf("     this code was changed. \n");
            return DTKM_ERROR_IN_CODE_dtkm_j1_equipment_status_check ;
            break ;

        case DTKM_ACTIVITIES_SHOULD_BE_COMBINED :
            /* this should never happen with a J1 down time activity.  */
        case DTKM_ACTIVITIES_IDENTICAL :
            /* this should never happen with a J1 down time activity.  */
        default :
            /* this should never happen.  */
            return DTKM_ERROR_IN_CODE_dtkm_activities_conflict ;
            break ;

    } /* end of switch */

    /* 
    -- all cases return; this place in the code is unreachable.  
    -- there should be no return here, since 
    -- it just creates a warning from the C compiler.  
    */

}

/*==============================================================================
Function:   dtkm_j1_equipment_status

Description:    Check to see if there is j1 equipment out of order 
that would prevent the proposed data-take from taking place.  

Parameters:     
    DB_RECORD   **proposed_dtk    check to see if this data-take uses 
                                  any satellite equipment that is down 
                                  during the time bracket of the data-take.  
    llist       *dtk_sat_down_times    holds satellite down time indicator.  

Returns:        
    int 
    = DTKM_SAT_EQUIPMENT_OK
           the J1 satellite is in order.  

    = DTKM_SAT_EQUIPMENT_NOT_OK
           the data-take cannot take place; some required equipment on 
           the J1 satellite is not in order.  

    < 0    an error was encountered.
           DTKM_ERROR_ACTIVITY_NOT_FOUND
           DTKM_ERROR_DB_QUERY_FAILED
           DTKM_ERROR_ACTIVITY_ILLEGAL
           DTKM_ERROR_IN_CODE_dtkm_j1_equipment_status_check


Creater:        Lawrence Stevens

Creation Date:  02/01/1995

Notes:      
==============================================================================*/

static int dtkm_j1_equipment_status(
    DB_RECORD   **proposed_dtk,   /* check to see if this data-take uses 
                                  any satellite equipment that is down 
                                  during the time bracket of the data-take.  */
    llist      *dtk_sat_down_times ) /* holds satellite down time indicators */
{

    llist       *j1_dn_times_list = NULL ;
    cursor      *j1_dn_times_list_ptr ;
    DB_RECORD   **j1_dn_times_rec = NULL ;
    DB_RECORD   **temp_dtk_rec = NULL ;
    DB_RECORD   **dtk_sat_down_times_rec = NULL ;

    double      proposed_dtk_duration_secs ;
    double      duration_days ;

    int         proposed_dtk_activity_id ;
    int         return_code ;

#ifdef PRINT_DIAG
    PRINT_DIAG ("dtkm_j1_equipment_status.c:  input dtk = \n");
    dtkm_print(stdout,proposed_dtk) ;
#endif

    if ( proposed_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_sat_down_times == NULL )
        return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_sat_down_times ) != 0 )
        return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY ;

        /*
        -- if the proposed dtk is not J1, then we have an error.  
        */

    if ( strcmp(CAST_DTK_SAT proposed_dtk[DTK_SAT], "J1" ) != 0 )
        return DTKM_ERROR_SAT_NOT_J1 ;


    /* obtain the activity id of the proposed data-take  */
    proposed_dtk_activity_id = dtkm_activity_id(proposed_dtk);
    if ( proposed_dtk_activity_id <= 0)
        return proposed_dtk_activity_id ;

#ifdef PRINT_DIAG
    PRINT_DIAG ("dtkm_j1_equipment_status.c:  proposed dtk activ.  id =%d\n",
        proposed_dtk_activity_id ) ;
#endif

    /* get duration of proposed data-take in days:  */
    return_code = tc_et_ASF_datetime_diff (
        CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME],
        CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
        &duration_days ) ;

    /* convert duration to seconds.  */
    proposed_dtk_duration_secs = duration_days * 24.0 * 60.0 * 60.0 ; 

#ifdef PRINT_DIAG
    PRINT_DIAG( "dtkm_j1_equipment_status.c:  duration: %lf\n", 
        proposed_dtk_duration_secs ) ;
#endif

        /*
        -- put the time bracket for the retrieve
        -- into the where clause.  
        -- trying to retrieve records with time brackets
        -- that overlap the 'retrieve' time bracket.
        -- the following statement may seem backwards, at first, since it
        -- compares the start times with the stop times and vice-versa,
        -- but this is intentional and does the job.
        */

    sprintf(where_clause, "where %s < '%s' and %s > '%s' ",
        APS_COL(J1_DN_TIMES, J1_DN_TIMES_STRTTIME), 
                CAST_DTK_STOPTIME proposed_dtk[DTK_STOPTIME],
        APS_COL(J1_DN_TIMES, J1_DN_TIMES_STOPTIME), 
                CAST_DTK_STRTTIME proposed_dtk[DTK_STRTTIME]) ;

    sprintf(orderby_cols, "%s",
        APS_COL(J1_DN_TIMES, J1_DN_TIMES_STRTTIME) ) ;

    j1_dn_times_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(J1_DN_TIMES),
        where_clause, orderby_cols, APS_CDEFS(J1_DN_TIMES), ALL_COLS) ;

    if ( j1_dn_times_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

    /* now see if there are any down times that overlap:  */
    if ( NUMELTS( j1_dn_times_list ) == 0 )
    {
        DEL_LIST( j1_dn_times_list ) ;
        return DTKM_SAT_EQUIPMENT_OK ;
    }

    /* 
    -- there are some j1 down time records that overlap.  
    */


    /* 
    -- set up a temp DB_RECORD to use when establishing the 
    -- activity id's of the down time activies.  
    -- If there is a conflict, it will be added to the 
    -- dtk_conflict list and can be printed out to offer 
    -- conflict info.  
    */
    temp_dtk_rec =  new_table_record(APS_CDEFS(DTK)) ;
    strcpy(CAST_DTK_SAT temp_dtk_rec[DTK_SAT], "J1" ) ;
    CAST_DTK_REV temp_dtk_rec[DTK_REV] = 0 ;
    CAST_DTK_DTKID temp_dtk_rec[DTK_DTKID] = 0 ;


#ifdef PRINT_DIAG
    PRINT_DIAG ("dtkm_j1_equipment_status.c:  process j1_dn_times recs.\n");
#endif
    /*
    -- need to determine what equipment is down for each record.  
    -- there are 6 possible down time configurations.  0 to 
    -- 6 of them might exist.  we need to check for each of the 
    -- 6 possible conditions and then test against the proposed 
    -- data-take to see if it stops it from taking place.  
    -- if one of these conditions does stop the data-take, 
    -- we append the condition info to the dtk_sat_down_times list
    */
    for (
        j1_dn_times_rec = (DB_RECORD **) FIRST( 
                                    j1_dn_times_list, j1_dn_times_list_ptr ) ;
        j1_dn_times_rec ;
        j1_dn_times_rec = (DB_RECORD **) NEXT( 
                                    j1_dn_times_list, j1_dn_times_list_ptr )
        )
    {
#ifdef PRINT_DIAG
    PRINT_DIAG ("dtkm_j1_equipment_status.c:  j1_dn_times rec:\n");
    PRINT_DIAG ("    %s %s %c %c %c %d %c\n", 
        CAST_J1_DN_TIMES_STRTTIME   j1_dn_times_rec[J1_DN_TIMES_STRTTIME],
        CAST_J1_DN_TIMES_STOPTIME  j1_dn_times_rec[J1_DN_TIMES_STOPTIME],
        CAST_J1_DN_TIMES_SAR_STATUS  j1_dn_times_rec[J1_DN_TIMES_SAR_STATUS],
        CAST_J1_DN_TIMES_OPS_STATUS  j1_dn_times_rec[J1_DN_TIMES_OPS_STATUS],
        CAST_J1_DN_TIMES_MDR_STATUS  j1_dn_times_rec[J1_DN_TIMES_MDR_STATUS],
        CAST_J1_DN_TIMES_MDR_OP_TIME  j1_dn_times_rec[J1_DN_TIMES_MDR_OP_TIME],
        CAST_J1_DN_TIMES_MDT_STATUS  j1_dn_times_rec[J1_DN_TIMES_MDT_STATUS] ) ;
#endif

        /*
        -- 1. check for SAR sensor down.  
        */
        if ( CAST_J1_DN_TIMES_SAR_STATUS j1_dn_times_rec[J1_DN_TIMES_SAR_STATUS]
           != 'Y' )
        {
            /* SAR is down.  */
            /* 
            -- put data into the temp_dtk_rec to be used by 
            -- dtk_j1_equipment_status_check
            */
            strcpy(CAST_DTK_ACTID   temp_dtk_rec[DTK_ACTID],   "SDN" ) ;
            strcpy(CAST_DTK_TRANSID temp_dtk_rec[DTK_TRANSID], "00" ) ;
#ifdef PRINT_DIAG
            dtkm_print(stdout,temp_dtk_rec);
#endif
            /*
            -- THE FOLLOWING CODE IS REPEATED IN THE NEXT 5 PARAGRAPHS,
            -- FOR EACH OF THE POSSIBLE DOWN EQUIPMENT CONDITIONS:
            */
            /*
            -- now check the conflict status.  see if this down time
            -- stops the proposed data-take.  
            */
            return_code = dtkm_j1_equipment_status_check( 
                proposed_dtk_activity_id, 
                temp_dtk_rec, dtk_sat_down_times, j1_dn_times_rec ) ;

            if ( return_code < 0 )
                return return_code ;
        }

        /*
        -- 2. check for OPS down.  value can be Y, S, V, N.  only Y is OK.
        */
        if ( CAST_J1_DN_TIMES_OPS_STATUS j1_dn_times_rec[J1_DN_TIMES_OPS_STATUS]
           != 'Y' )
        {
            /* OPS is down.  */
            strcpy(CAST_DTK_ACTID   temp_dtk_rec[DTK_ACTID],   "ODN" ) ;
            strcpy(CAST_DTK_TRANSID temp_dtk_rec[DTK_TRANSID], "00" ) ;
#ifdef PRINT_DIAG
            dtkm_print(stdout,temp_dtk_rec);
#endif
            return_code = dtkm_j1_equipment_status_check( 
                proposed_dtk_activity_id, 
                temp_dtk_rec, dtk_sat_down_times, j1_dn_times_rec ) ;

            if ( return_code < 0 )
                return return_code ;

        }


        /*
        -- 3. check for VNIR (or VNR) down.  (Visual Near Infra Red)
        --    value can be Y, S, V, N. 
        --    S and N are not OK for this sensor; Y and V mean is is up.  
        */
        if ( CAST_J1_DN_TIMES_OPS_STATUS j1_dn_times_rec[J1_DN_TIMES_OPS_STATUS]
           == 'N' 
        ||   CAST_J1_DN_TIMES_OPS_STATUS j1_dn_times_rec[J1_DN_TIMES_OPS_STATUS]
           == 'S' )
        {
            /* VNR is down.  */
            strcpy(CAST_DTK_ACTID   temp_dtk_rec[DTK_ACTID],   "VDN" ) ;
            strcpy(CAST_DTK_TRANSID temp_dtk_rec[DTK_TRANSID], "00" ) ;
#ifdef PRINT_DIAG
            dtkm_print(stdout,temp_dtk_rec);
#endif
            return_code = dtkm_j1_equipment_status_check( 
                proposed_dtk_activity_id, 
                temp_dtk_rec, dtk_sat_down_times, j1_dn_times_rec ) ;

            if ( return_code < 0 )
                return return_code ;
        }

        /*
        -- 4. check for Mission Data Recorder (MDR) down.  
        */
        if ( CAST_J1_DN_TIMES_MDR_STATUS j1_dn_times_rec[J1_DN_TIMES_MDR_STATUS]
           != 'Y' )
        {
            /* MDR is down.  */
            strcpy(CAST_DTK_ACTID   temp_dtk_rec[DTK_ACTID],   "RDN" ) ;
            strcpy(CAST_DTK_TRANSID temp_dtk_rec[DTK_TRANSID], "00" ) ;
#ifdef PRINT_DIAG
            dtkm_print(stdout,temp_dtk_rec);
#endif
            return_code = dtkm_j1_equipment_status_check( 
                proposed_dtk_activity_id, 
                temp_dtk_rec, dtk_sat_down_times, j1_dn_times_rec ) ;

            if ( return_code < 0 )
                return return_code ;
        }

        /*
        -- 5. check for Mission Data Transmitter (MDT) frequency 1 (F1) 
        --    down.  N and 2 are not OK.
        */
        if ( CAST_J1_DN_TIMES_MDT_STATUS j1_dn_times_rec[J1_DN_TIMES_MDT_STATUS]
           == 'N' 
        ||   CAST_J1_DN_TIMES_MDT_STATUS j1_dn_times_rec[J1_DN_TIMES_MDT_STATUS]
           == '2' )
        {
            /* MDT F1 is down.  */
            strcpy(CAST_DTK_ACTID   temp_dtk_rec[DTK_ACTID],   "TDN" ) ;
            strcpy(CAST_DTK_TRANSID temp_dtk_rec[DTK_TRANSID], "F1" ) ;
#ifdef PRINT_DIAG
            dtkm_print(stdout,temp_dtk_rec);
#endif
            return_code = dtkm_j1_equipment_status_check( 
                proposed_dtk_activity_id, 
                temp_dtk_rec, dtk_sat_down_times, j1_dn_times_rec ) ;

            if ( return_code < 0 )
                return return_code ;
        }

        /*
        -- 6. check for Mission Data Transmitter (MDT) frequency 2 (F2) 
        --    down.  N and 1 are not OK.
        */
        if ( CAST_J1_DN_TIMES_MDT_STATUS j1_dn_times_rec[J1_DN_TIMES_MDT_STATUS]
           == 'N' 
        ||   CAST_J1_DN_TIMES_MDT_STATUS j1_dn_times_rec[J1_DN_TIMES_MDT_STATUS]
           == '1' )
        {
            /* MDT F2 is down.  */
            strcpy(CAST_DTK_ACTID   temp_dtk_rec[DTK_ACTID],   "TDN" ) ;
            strcpy(CAST_DTK_TRANSID temp_dtk_rec[DTK_TRANSID], "F2" ) ;
#ifdef PRINT_DIAG
            dtkm_print(stdout,temp_dtk_rec);
#endif
            return_code = dtkm_j1_equipment_status_check( 
                proposed_dtk_activity_id, 
                temp_dtk_rec, dtk_sat_down_times, j1_dn_times_rec ) ;

            if ( return_code < 0 )
                return return_code ;
        }

        /* done checking for equipment down for this j1_dn_times record.  */

        /* 
        -- if the proposed data-take is a recording, then check time 
        -- duration in seconds vs recorder operation time.  
        */
#ifdef PRINT_DIAG
        PRINT_DIAG( "dtkm_j1_equipment_status.c:  check for recording time.\n");
#endif
        if ( !dtkm_is_a_downlink(proposed_dtk) )
        {
            /* the proposed data-take is a recording */

#ifdef PRINT_DIAG
            PRINT_DIAG( "dtkm_j1_equipment_status.c:  a recording op.\n");
#endif

            /* check duration of recording against operation time.  */
            if ( proposed_dtk_duration_secs > 
                 (double) CAST_J1_DN_TIMES_MDR_OP_TIME  
                                 j1_dn_times_rec[J1_DN_TIMES_MDR_OP_TIME] )
            {
                /* 
                -- WARNING:  
                -- WARNING:  
                -- This data is appended to the "dtk_sat_down_times" list. 
                -- It isn't really a data-take, but fields are set and then
                -- placed into a record which is placed into the 
                -- dtk_sat_down_times list as diagnostic information.  It allows
                -- the planner to tell which activity caused a conflict.
                -- in this case, it is the Mission Data Recorder (MDR) 
                -- Operations Time (opT) from the J1 satellite which 
                -- prevents the data-take from taking place.  It is 
                -- longer than the capacity of the on-board MDR.  
                --
                -- We could use a better way to somehow indicate this... 
                */

                /*
                --  send back a message.  MDR TIME_CODE designates the problem 
                --  as described in the previous paragraph:
                */
                dtk_sat_down_times_rec =  new_table_record(APS_CDEFS(DTK)) ;
                db_copy_record( APS_CDEFS(DTK), dtk_sat_down_times_rec, 
                    temp_dtk_rec ) ;

                strcpy(CAST_DTK_SENSOR dtk_sat_down_times_rec[DTK_SENSOR], 
                    "MDR" ) ;
                strcpy(CAST_DTK_ACTID dtk_sat_down_times_rec[DTK_ACTID], 
                    TIME_CODE) ;
                strcpy(CAST_DTK_TRANSID dtk_sat_down_times_rec[DTK_TRANSID], 
                    "00" ) ;
                strcpy(CAST_DTK_STRTTIME dtk_sat_down_times_rec[DTK_STRTTIME],
                    CAST_J1_DN_TIMES_STRTTIME 
                    j1_dn_times_rec[J1_DN_TIMES_STRTTIME] ) ;
                strcpy(CAST_DTK_STOPTIME dtk_sat_down_times_rec[DTK_STOPTIME],
                    CAST_J1_DN_TIMES_STOPTIME   
                    j1_dn_times_rec[J1_DN_TIMES_STOPTIME] ) ;

                APPEND( dtk_sat_down_times, dtk_sat_down_times_rec, 
                    free_db_record, dtk_sat_down_times_rec ) ;

            }
        }

    }  /* end of j1_dn_times_rec  while loop */

    /* clean up and finish.  */
    free_db_record (temp_dtk_rec) ;
    DEL_LIST( j1_dn_times_list ) ;

    /*
    --  Count the sat down times to determine if the equipment is OK.  
    */
    if ( NUMELTS( dtk_sat_down_times ) > 0 )
        return DTKM_SAT_EQUIPMENT_NOT_OK ;
    else
        return DTKM_SAT_EQUIPMENT_OK ;

}



/*==============================================================================
Function:   dtkm_check_sat_equipment

Description:    Check to see if there is satellite equipment out of order 
that would prevent the proposed data-take from taking place.  

Parameters:     
    DB_RECORD   **proposed_dtk    check to see if this data-take uses 
                                  any satellite equipment that is down 
                                  during the time bracket of the data-take.  
    llist       *dtk_sat_down_times    holds the first satellite equipment down 
                                  indicator.

Returns:        
    int 
    = DTKM_SAT_EQUIPMENT_OK    
           the data-take can take place; the required equipment on 
           the satellite is in order.  
    = other 
           see dtkm_j1_equipment_status()

    < 0    an error was encountered.

Creator:        Lawrence Stevens

Creation Date:  02/01/1995

Notes:      
==============================================================================*/
int dtkm_check_sat_equipment(
    DB_RECORD   **dtk_proposal,   /* check to see if this data-take uses 
                                  any satellite equipment that is down 
                                  during the time bracket of the data-take.  */
    llist      *dtk_sat_down_times )/* holds satellite down time indicators */
{

    if ( dtk_proposal == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_sat_down_times == NULL )
        return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_sat_down_times ) != 0 )
        return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY ;

        /*
        -- if the proposed dtk is not J1, then we have no data to check.
        */

    if ( strcmp(CAST_DTK_SAT dtk_proposal[DTK_SAT], "J1" ) != 0 )
        return DTKM_SAT_EQUIPMENT_OK ;

    return ( dtkm_j1_equipment_status( dtk_proposal, dtk_sat_down_times ) ) ;

}



