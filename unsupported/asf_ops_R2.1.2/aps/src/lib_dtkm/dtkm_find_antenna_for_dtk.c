#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_find_antenna_for_dtk.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_find_antenna_for_dtk.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_find_antenna_for_dtk.c"



/*==============================================================================
Function:       dtkm_find_antenna_for_dtk()

Description:    make all attempts to find an antenna for the 
                dtk_proposal.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sat Nov 11 20:41:04 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"
#include "timeconv.h"

extern void free(void *); /* conflict with <stdlib.h>, <macros.h> over abs() */

int dtkm_find_antenna_for_dtk(
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **dtk_proposal,
    llist       *dtk_similars, 
    llist       *dtk_parallels, 
    llist       *dtk_same_pass, 
    llist       *antenna_down_times,
    llist       *dtk_conflicts,   /* conflicts against the data-take.  */
    DB_RECORD   **result_dtk )
{

    int     return_code ;
    int     *list_of_antennas ;

    llist   *list_check ;
    llist   *current_antenna_down_times = NULL ;

    char    antenna_strttime[ASF_TIME_STR_LENGTH+1] ;
    char    antenna_stoptime[ASF_TIME_STR_LENGTH+1] ; 
    llist   *antenna_dtk_conflicts = NULL ; /* conflicts for the antenna.  */

    struct Best_Antenna best_antenna ;
    int     next_antenna_id ;

    int     antenna_dtk_padding_time_sec  = 0 ;

    /* the same sat time bracket:  */
    char    same_sat_strttime[ASF_TIME_STR_LENGTH+1] ;
    char    same_sat_stoptime[ASF_TIME_STR_LENGTH+1] ;

    /* quick error checking.  */
    if ( dtk_proposal == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_similars == NULL )
        return DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED ;

    if ( dtk_parallels == NULL )
        return DTKM_ERROR_PARALLELS_LIST_NOT_INITIALIZED ;

    if ( dtk_same_pass == NULL )
        return DTKM_ERROR_SAME_PASS_LIST_NOT_INITIALIZED ;

    if ( antenna_down_times == NULL )
        return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( antenna_down_times ) != 0 )
        return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY ;

    if ( dtk_conflicts == NULL )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_conflicts ) != 0 )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY ;

    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    /* 
    -- initialize the best antenna structure.  
    -- Note:  originally, the software checked more 
    -- than one suitable antenna and picked the best.  
    -- Now, Wed Nov  6 10:58:45 PST 1996, the software 
    -- picks the first possible antenna to use.  
    -- the best_antenna variables and its use was retained, 
    -- in case the search logic comes back.  Also, the 
    -- variables hold the data for use just before returning.  
    */
    best_antenna.dtk_conflicts = create_dyn_llist() ;
    best_antenna.antenna_id = 0 ;
    best_antenna.preference = 0 ;

    /* initialize the conflicts list for a single antenna.  */
    antenna_dtk_conflicts = create_dyn_llist() ;
    current_antenna_down_times = create_dyn_llist() ;

    /* copy the input record.  use and change the copy.  */
    db_copy_record( APS_CDEFS(DTK), result_dtk, dtk_proposal ) ;

    /*
    --   STEP 5. 
    --   If the dtk proposal information does not yet include antenna, select
    --   the preferred antenna using the Planner-provided preferences from
    --   Section 2.2.  Set up a list of antennas.  Whenever an antenna has
    --   been considered, check it on this list.  Whenever an untried antenna
    --   is needed, obtain it from this list.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 5.  setup antenna list, select antenna.\n", 
        __FILE__, __LINE__ ) ;
#endif
    /* list_of_antennas is allocated here;  free() it later.  */
    return_code = dtkm_create_antenna_list( APS_dbproc, result_dtk, 
        &list_of_antennas ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( antenna_dtk_conflicts ) ;
        DEL_LIST( current_antenna_down_times ) ;
        DEL_LIST( best_antenna.dtk_conflicts ) ;
        return return_code ;
    }

    if ( CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] == 0 )
    {
        /* antenna is not yet selected.  */
        return_code = dtkm_select_next_untried_antenna( list_of_antennas,
            &next_antenna_id ) ;
        if (return_code < 0 )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return return_code ;
        }
        if ( return_code == DTKM_NO_MORE_UNTRIED_ANTENNAS )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return DTKM_NO_AVAILABLE_ANTENNA ;
        }

        CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] = next_antenna_id ;
    }
    else
    {
        /* 
        - initial antenna was already 
        -- selected.  
        */
        next_antenna_id = CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] ;
    }

    /* the first antenna to try has been selected.  */

    /*
    --  STEP 6.
    --  Create the "same-sat time bracket", which includes the time brackets of
    --  all of the downlinking similar, parallel, and same-pass data-takes.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 6. Create the same-sat time bracket\n", 
        __FILE__, __LINE__ ) ;
#endif

    return_code = dtkm_create_same_sat_time_bracket( result_dtk, 
            dtk_similars, dtk_parallels, dtk_same_pass, 
            same_sat_strttime, same_sat_stoptime )  ;
    if ( return_code < 0 )
    {
        DEL_LIST( antenna_dtk_conflicts ) ;
        DEL_LIST( current_antenna_down_times ) ;
        DEL_LIST( best_antenna.dtk_conflicts ) ;
        free( list_of_antennas ) ;
        return DTKM_ERROR_IN_CREATING_SAME_SAT_TIME_BRACKET ;
    }

    /* LOOP to try antennas.  */
    while ( next_antenna_id > 0 )
    {
#ifdef PRINT_DIAG
    printf("%s(%d):  trying antenna = %d\n", 
        __FILE__, __LINE__, CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] ) ;
#endif
        /* 
        -- starting with a new antenna to check.  
        -- remove antenna_dtk_conflicts members, if any
        */
        DEL_ALL( antenna_dtk_conflicts ) ;
        DEL_ALL( current_antenna_down_times ) ;

        /*
        --  we have the same_sat time bracket.  
        -- 
        --  Pad this time bracket with the sum of the various times 
        --  required for the antenna to set up, get ready, and to 
        --  stand down.  
        --  This antenna time bracket is what is used when checking 
        --  other-satellite data-takes for conflicts.
        -- 
        --  This time bracket is formed only for the purpose of 
        --  comparisons with data-takes from DIFFERENT SATELLITES.  
        --  The dtk proposal retains its original 
        --  time bracket.
        */
        return_code = dtkm_get_antenna_dtk_padding_time( APS_dbproc, result_dtk,
            &antenna_dtk_padding_time_sec ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return return_code ;
        }
        /*
        -- convert to minutes and pad the same_sat time bracket to
        -- get the antenna time bracket.
        -- tc_time_pad() needs a padding in terms of minutes.  
        */
        if ( !tc_time_pad( same_sat_strttime, same_sat_stoptime,
            (float) ( antenna_dtk_padding_time_sec/60.0),
            antenna_strttime, antenna_stoptime ) )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return DTKM_ERROR_PADDING_SAME_SAT_TIME_BRACKET ;
        }

        /*
        --  STEP 7. 
        --  The APS checks for antenna-based conflicts.  This is where we loop
        --  to choose the antenna for the dtk proposal.
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 7. check for antenna-based conflicts\n", 
            __FILE__, __LINE__ ) ;
#endif

        /*
        --  STEP 7.1
        --  check RGS equipment status for the antenna.
        --  The db relation searched is antenna_down_times.
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 7.1. check RGS equipment status\n", 
            __FILE__, __LINE__ ) ;
#endif
        /* 
        -- start with a fresh list for this antenna.  
        -- delete all members.  
        */
        return_code = dtkm_check_antenna_down_times( APS_dbproc, result_dtk,
            antenna_strttime, antenna_stoptime, current_antenna_down_times ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return return_code ;
        }
        /*
        --  STEP 7.2
        -- 
        --  check for conflicts for each other satellite.  
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 7.2. check for antenna conflicts\n", 
            __FILE__, __LINE__ ) ;
#endif
        return_code = dtkm_check_antenna_conflicts( APS_dbproc, result_dtk, 
            antenna_strttime, antenna_stoptime, antenna_dtk_conflicts ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return return_code ;
        }

        /*
        --  STEP 7.3     
        --  Mark the current antenna on the antenna list as being checked.
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 7.3. Mark the current antenna\n", 
            __FILE__, __LINE__ ) ;
#endif
        return_code = dtkm_check_off_antenna( 
            CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID], list_of_antennas ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return return_code ;
        }

        /*
        --  STEP 8. 
        --  Check the down time / conflict status of the 
        --  dtk proposal from Step 7.
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 8. Check the status from Step 7\n", 
            __FILE__, __LINE__ ) ;
#endif
        if ( NUMELTS( current_antenna_down_times ) > 0 )
        {
            /* 
            --  STEP 8.1
            --  RGS EQUIPMENT DOWN  
            --  Change the dtk proposal to the most preferred
            --  untried antenna on the list then go to Step 7.
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 8.1  rgs equipment down\n", 
                __FILE__, __LINE__ ) ;
#endif
            /*
            -- accumulate down times in the output antenna_down_times
            -- list for later use if necessary.  
            -- if no antenna is ever acceptable, all of these 
            -- down times may be reported.   
            -- they will all have contributed to the 
            -- data-take being kept off the schedule.  
            */
            list_check = db_record_llist_move( antenna_down_times, 
                current_antenna_down_times);
            if ( list_check != antenna_down_times )
            {
                DEL_LIST( antenna_dtk_conflicts ) ;
                DEL_LIST( current_antenna_down_times ) ;
                DEL_LIST( best_antenna.dtk_conflicts ) ;
                free( list_of_antennas ) ;
                return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
            }

        }
        else if ( NUMELTS( antenna_dtk_conflicts ) > 0 )
        {
            /* 
            -- STEP 8.2  
            -- OTHER SATELLITE CONFLICTS  
            --  this means that one or more other satellite(s) use
            --  the antenna designated in the dtk proposal.
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 8.2  other satellite conflicts\n", 
                __FILE__, __LINE__ ) ;
#endif
            /* 
            -- check for bumpability on this antenna.
            */
            return_code = dtkm_check_bumpability( APS_dbproc, result_dtk, 
                antenna_dtk_conflicts ) ;
            if ( return_code < 0 )
            {
                DEL_LIST( antenna_dtk_conflicts ) ;
                DEL_LIST( current_antenna_down_times ) ;
                DEL_LIST( best_antenna.dtk_conflicts ) ;
                free( list_of_antennas ) ;
                return return_code ;
            }
            if ( return_code == DTKM_CAN_BUMP_DTKS )
            {
                /*
                -- 8.2.3.  If the data-take can bump the other data-takes,
                --         this antenna is selected here.  This is because
                --         this is the desired antenna and the other data-takes
                --         can be bumped.
                --         Re-set the best-antenna data.  Continue to Step 9.
                */
    #ifdef PRINT_DIAG
                printf("%s(%d):  STEP 8.2.3 DTK CAN BUMP\n", 
                    __FILE__, __LINE__ ) ;
    #endif
                return_code = dtkm_update_best_antenna( APS_dbproc, 
                    result_dtk, antenna_dtk_conflicts, &best_antenna ) ;
                if ( return_code < 0 )
                {
                    DEL_LIST( antenna_dtk_conflicts ) ;
                    DEL_LIST( current_antenna_down_times ) ;
                    DEL_LIST( best_antenna.dtk_conflicts ) ;
                    free( list_of_antennas ) ;
                    return return_code ;
                }
                /* success.  continue with step 9.  */
                break ;
            }
            else if ( return_code == DTKM_CANNOT_BUMP_DTKS )
            {
                /*
                -- accumulate conflicts in the output conflict
                -- list for later use if necessary.  
                -- if no antenna is ever acceptable, all of these 
                -- conflicts with the dtk_proposal will be reported.   
                -- they will all have contributed to the 
                -- data-take being kept off the schedule.  
                */
                list_check = db_record_llist_move( dtk_conflicts, 
                    antenna_dtk_conflicts);
                if ( list_check != dtk_conflicts )
                {
                    DEL_LIST( antenna_dtk_conflicts ) ;
                    DEL_LIST( current_antenna_down_times ) ;
                    DEL_LIST( best_antenna.dtk_conflicts ) ;
                    free( list_of_antennas ) ;
                    return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;
                }
            }
            else
            {
                /* error.  */
                DEL_LIST( antenna_dtk_conflicts ) ;
                DEL_LIST( current_antenna_down_times ) ;
                DEL_LIST( best_antenna.dtk_conflicts ) ;
                free( list_of_antennas ) ;
                return 
                    DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_check_bumpability ;
            }

            /* 
            -- there were conflicts; the info has been handled.  
            -- now select another antenna to keep trying to 
            -- find a better antenna.  
            */
        }
        else if ( NUMELTS( antenna_dtk_conflicts ) == 0 )
        {
            /*
            -- STEP 8.3
            -- NO CONFLICTS
            -- We will use the current antenna.  This is 
            -- because there will never be a "better" antenna, as 
            -- we try each antenna in preferred order:
            -- Re-set the best-antenna data to indicate the 
            -- choice.  Continue to Step 9.
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 8.3 NO CONFLICTS\n", __FILE__, __LINE__ ) ;
#endif
            return_code = dtkm_update_best_antenna( APS_dbproc, 
                result_dtk, antenna_dtk_conflicts, &best_antenna ) ;
            if ( return_code < 0 )
            {
                DEL_LIST( antenna_dtk_conflicts ) ;
                DEL_LIST( current_antenna_down_times ) ;
                DEL_LIST( best_antenna.dtk_conflicts ) ;
                free( list_of_antennas ) ;
                return return_code ;
            }
            /* success.  continue with step 9.  */
            break ;
        }
        else 
        {
            /* 
            -- we already checked conflicts == 0 and > 0.  
            */
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return DTKM_ERROR_LT_ZERO_ELEMENTS_IN_LIST ;
        }

        /*
        -- STEP 8.4
        -- the current antenna has been examined; 
        -- it may not be the best antenna:
        -- 
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 8.4. try another antenna\n", 
            __FILE__, __LINE__ ) ;
#endif

        /*
        -- STEP 8.4.1.  
        -- Change the dtk proposal to the most preferred 
        -- untried antenna on the list
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 8.4.1. \n", __FILE__, __LINE__ ) ;
#endif
        return_code = dtkm_select_next_untried_antenna( list_of_antennas,
            &next_antenna_id ) ;
        if ( return_code < 0 )
        {
            DEL_LIST( antenna_dtk_conflicts ) ;
            DEL_LIST( current_antenna_down_times ) ;
            DEL_LIST( best_antenna.dtk_conflicts ) ;
            free( list_of_antennas ) ;
            return return_code ;
        }
        if ( return_code == DTKM_NO_MORE_UNTRIED_ANTENNAS )
        {
            /*
            -- STEP 8.4.2.  
            --  If there are no more antennas to try:
            --     if there is a best antenna, set the value 
            --          in the dtk and continue with Step 9
            --     if there is no best antenna, the dtk proposal 
            --          is cannot be scheduled.  it will 
            --          be placed in the dtk relation with 
            --          dtkstat = CON for possible scheduling 
            --          later, if the conflicts go away.  
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 8.4.2 no more antennas to try\n", 
                __FILE__, __LINE__ ) ;
#endif
            if ( best_antenna.antenna_id > 0 )
            {
                /* 
                -- YES.  there is a best antenna.  
                -- set the value to force this antenna 
                -- as the choice.  
                */
                CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] = 
                    best_antenna.antenna_id ;
                break ;   /* success.  Continue to Step 9.  */
            }
            else 
            {
                /* 
                -- NO.  there is no acceptable antenna 
                -- at all.  process is finished for our data-take.  
                */
                DEL_LIST( antenna_dtk_conflicts ) ;
                DEL_LIST( current_antenna_down_times ) ;
                DEL_LIST( best_antenna.dtk_conflicts ) ;
                free( list_of_antennas ) ;
                return DTKM_NO_AVAILABLE_ANTENNA ;
            }
        }

        /* 
        -- Step 8.4.3   There is an antenna to try; 
        -- use this antenna and continue to Step 7.
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 8.4.3 new antenna to try\n", 
            __FILE__, __LINE__ ) ;
#endif
        CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] = next_antenna_id ;
        /* 
        -- now continue the loop with the next antenna.  
        */

    } /* end of loop:    while ( next_antenna_id > 0 )   */

    /* 
    -- STEP 9.
    -- The dtk proposal was accepted; it has been assigned to an antenna
    -- unless it is a recording.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 9", __FILE__, __LINE__ ) ;
    printf("   antenna accepted = %d\n", 
        CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID] ) ;
#endif

    /* 
    -- remove all current members of antenna_down_times and 
    -- dtk_conflicts.  not relevant now that the dtk proposal 
    -- was accepted.  
    */
    DEL_ALL( antenna_down_times ) ;
    DEL_ALL( dtk_conflicts ) ;
    /*
    -- then move the best antenna conflicts to the dtk_conflicts list.
    -- these are the only relevant ones to report.  
    -- these conflicts, if any, will be bumped.  
    -- also free memory 
    */
    DEL_LIST( current_antenna_down_times ) ;
    DEL_LIST( antenna_dtk_conflicts ) ;
    free( list_of_antennas ) ;

    list_check = db_record_llist_move( dtk_conflicts, 
        best_antenna.dtk_conflicts ) ;
    DEL_LIST( best_antenna.dtk_conflicts ) ;
    if ( list_check != dtk_conflicts )
        return DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST ;

    return DTKM_ANTENNA_FOUND ;

}
