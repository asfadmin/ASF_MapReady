#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_check_same_sat_conflicts.c


External Functions Defined:
    
File Scope Functions:
    dtkm_extract_concurs()
    dtkm_extract_similars()
    dtkm_extract_same_pass()
    dtkm_extract_conflicts_and_parallels()
    
External Variables Defined:
    
File Scope Variables:
    
Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_check_same_sat_conflicts.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_check_same_sat_conflicts.c"

#include "dtkm.h"
#include "timeconv.h"
#include "db_dtk.h"

#include <string.h>     /* for strcmp strncmp strcat strcpy   */


/*==============================================================================
Function:       dtkm_set_antenna_id_from_list()

Description:    goes through a list and sets the antenna id 
                of the input dtk to match it.  starts the 
                search for a good antenna where the other 
                data-takes already are planned. 
                if the data-take is not a downlink, forget it.  
                if the data-take already has a value set, forget it.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  2 19:57:04 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_set_antenna_id_from_list( 
    DB_RECORD   **input_dtk, 
    llist       *dtk_list )
{
    cursor      dtk_list_ptr ;
    DB_RECORD   **dtk_rec ;

    if ( !dtkm_is_a_downlink( input_dtk ) )
        return TRUE ;

    if ( CAST_DTK_ANTENNA_ID input_dtk[DTK_ANTENNA_ID] != 0 )
        return TRUE ;

    /* check each record in the list.  */
    for (   
        dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
        dtk_rec != NULL ;
        dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
        )
    {
        if ( dtkm_is_a_downlink( dtk_rec ) )
            if ( CAST_DTK_ANTENNA_ID input_dtk[DTK_ANTENNA_ID] == 0 )
            {
                CAST_DTK_ANTENNA_ID input_dtk[DTK_ANTENNA_ID] = 
                    CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] ;
                return TRUE ;
            }
    }

    return TRUE ;
}


/*==============================================================================
Function:       dtkm_extract_concurs()

Description:    extract the concurring datatakes from the list and put 
                them into the dtk_concurs list.  also update the 
                antenna_id of the dtk_proposal if necessary.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 19:35:16 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_extract_concurs(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_retrievals,
    llist       *dtk_concurs )
{
    cursor      dtk_retrievals_ptr ;
    DB_RECORD   **dtk_rec = NULL ;
    DB_RECORD   **next_dtk_rec = NULL ;
    cursor      next_dtk_retrievals_ptr ;
    llist       *list_check = NULL ;

    llist       *dtk_list = NULL ;
    cursor      dtk_list_ptr ; 
    int         return_code ;

    /* 
    -- check each record in the dtk_retrievals list, move it to 
    -- dtk_concurs when appropriate.  
    */
    dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, dtk_retrievals_ptr);

    /*
    -- this loop is a bit TRICKY. 
    -- trying to loop through each data-take, 
    -- but also, when we find the desired record, 
    -- we want to take it out of the list 
    -- and put it in another list.  
    -- for that reason, the usual simple 
    -- loop construction is not done here.  
    */
    while ( dtk_rec )
    {
        return_code = dtkm_2dtks_concur( dtk_proposal, dtk_rec ) ;
        if ( return_code < 0 )
            return return_code ;
        if ( return_code == TRUE )
        {
            /* 
            -- this data-take is a CONCURRING data-take, established
            -- by same sat, sensor, rev, and dtkid 
            --    (or fadtkid if dtk_proposal.dtkid == 0 ) 
            */
            /*
            -- if necessary, set the dtkid 
            */
            if ( CAST_DTK_DTKID dtk_proposal[DTK_DTKID] == 0 )
                CAST_DTK_DTKID dtk_proposal[DTK_DTKID] = 
                    CAST_DTK_DTKID dtk_rec[DTK_DTKID] ;

            /* 
            -- capture/save the next record from the 
            -- list for processing.  
            */
            next_dtk_retrievals_ptr = dtk_retrievals_ptr ;
            next_dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                    next_dtk_retrievals_ptr);

            /* 
            -- move the dtk_rec to the dtk_concurs list.  
            */
            list_check = move_db_record2new_llist( dtk_concurs, dtk_rec, 
                dtk_retrievals, dtk_retrievals_ptr ) ;
            if ( list_check != dtk_concurs )
                return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;

            /* now set up for the next iteration.  */
            dtk_rec = next_dtk_rec ;
            dtk_retrievals_ptr = next_dtk_retrievals_ptr ;

        }
        else
            dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, dtk_retrievals_ptr);
    }

    if ( NUMELTS( dtk_concurs ) == 0 )
    {
        /* 
        -- not found in the original retrieve.  
        -- try again to find it in the database;
        -- identified by sat/sensor/rev/dtkid.
        -- this time, retrieve any dtkstat value.  
        */

        sprintf(where_clause, 
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d ", 
            APS_COL(DTK, DTK_SAT),    CAST_DTK_SAT    dtk_proposal[DTK_SAT],
            APS_COL(DTK, DTK_SENSOR), CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR],
            APS_COL(DTK, DTK_REV),    CAST_DTK_REV    dtk_proposal[DTK_REV],
            APS_COL(DTK, DTK_DTKID),  
                (int) CAST_DTK_DTKID  dtk_proposal[DTK_DTKID] );

        dtk_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(DTK), where_clause, NULL, APS_CDEFS(DTK), ALL_COLS ) ;
        if ( dtk_list == NULL )
            return DTKM_ERROR_DB_QUERY_FAILED ;

        if ( NUMELTS( dtk_list ) == 1 )
        {
            /* 
            -- move the rec out of the dtk_list into the 
            -- dtk_concurs list. 
            */
            dtk_rec = FIRST( dtk_list, dtk_list_ptr ) ;
            list_check = move_db_record2new_llist( dtk_concurs, 
                dtk_rec, dtk_list, dtk_list_ptr ) ;
            if ( list_check != dtk_concurs ) 
            {
                DEL_LIST( dtk_list ) ;
                return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;
            }
        }
        else if ( NUMELTS( dtk_list ) > 1 )
        {
            DEL_LIST( dtk_list ) ;
            return DTKM_ERROR_GT_1_DTK_REC_WITH_SAME_SAT_SENSOR_REV_DTKID ;
        }

        DEL_LIST( dtk_list ) ;
    }

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_extract_similars()

Description:    extract the similar datatakes from the list and put 
                them into the dtk_similars list.  also update the 
                antenna_id of the dtk_proposal if necessary.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 19:35:16 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_extract_similars(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_retrievals,
    llist       *dtk_similars )
{
    cursor      dtk_retrievals_ptr ;
    DB_RECORD   **dtk_rec = NULL ;
    DB_RECORD   **next_dtk_rec = NULL ;
    cursor      next_dtk_retrievals_ptr ;
    llist       *list_check = NULL ;

    int         return_code ;

    /* 
    -- check each record in the dtk_retrievals list, move it to 
    -- dtk_similars when appropriate.  
    */
    dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, dtk_retrievals_ptr);

    /*
    -- this loop is a bit TRICKY. 
    -- trying to loop through each data-take, 
    -- but also, when we find the desired record, 
    -- we want to take it out of the list 
    -- and put it in another list.  
    -- for that reason, the usual simple 
    -- loop construction is not done here.  
    */
    while ( dtk_rec )
    {
        /* 
        -- test the dtk_rec vs dtk_proposal for a 
        -- similar.  must check for a time overlap, among 
        -- other tests for equality.  note the strange comparison 
        -- between strttime and stoptime between the two records.  
        -- this is how you check for a time overlaps.  
        --
        -- check for satellite, rev, dtkids !=, actid[1-3], transid.  
        -- 
        -- NOTE:  the sensor field is not compared.  this is because 
        --        the first character in the sensor field is also 
        --        seen in the 3rd character of DTK_ACTID.  
        --        Requiring identical sensors would work for every satellite 
        --        except for Radarsat, when a SAR sensor is "similar" to 
        --        every other sensor.  So even after this check, we need 
        --        to rule out a SS1 vs SS2 comparison, which would not
        --        be similar.  This is all caused by the RADARSAT files 
        --        not providing the actual sensor and mode.  
        */
        if 
        (   strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], 
                    CAST_DTK_SAT dtk_proposal[DTK_SAT] ) == 0 
        &&  CAST_DTK_REV dtk_rec[DTK_REV] == CAST_DTK_REV dtk_proposal[DTK_REV] 
        &&  CAST_DTK_DTKID dtk_rec[DTK_DTKID] 
                != CAST_DTK_DTKID dtk_proposal[DTK_DTKID] 
        &&  strncmp(CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
                    CAST_DTK_ACTID dtk_proposal[DTK_ACTID], 3 ) == 0 
        &&  strcmp( CAST_DTK_TRANSID dtk_rec[DTK_TRANSID], 
                    CAST_DTK_TRANSID dtk_proposal[DTK_TRANSID] ) == 0 
        &&  strcmp( CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME], 
                    CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) < 0 
        &&  strcmp( CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME], 
                    CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) > 0 
        )
        {
            /*
            -- one last check for a similar.  we want to 
            -- exclude a concurring data-take.  
            */
            return_code = dtkm_2dtks_concur( dtk_proposal, dtk_rec ) ;
            if ( return_code < 0 )
                return return_code ;
            if ( return_code == TRUE )
            {
                /*  
                -- this is a CONCURRING data-take 
                */
                /* NOT a similar; skip to the next record.  */
                dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                            dtk_retrievals_ptr);
                continue ;
            }

            /*
            -- NOTE:  
            -- SAR is similar to any sensor starting with S.  
            -- but SS1 is similar only to SS1.  
            -- any non-sar sensor is only similar when sensors 
            -- match.  
            */
            if ( strcmp( CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], "SAR" ) != 0 
            &&   strcmp( CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR], "SAR" ) != 0 
            &&   strcmp( CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR], 
                         CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) != 0    )
            {
                /* 
                -- neither dtk is SAR; they differ; this is 
                -- NOT a similar; skip to the next record. 
                */
                dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                            dtk_retrievals_ptr);
                continue ;
            }

            /* 
            -- this data-take is a SIMILAR data-take, established
            -- by same sat, sensor, rev, actid[0-2], transid and 
            -- overlapping time brackets.  
            */

            /* 
            -- capture/save the next record from the 
            -- list for processing.  
            */
            next_dtk_retrievals_ptr = dtk_retrievals_ptr ;
            next_dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                    next_dtk_retrievals_ptr);

            /* 
            -- move the dtk_rec to the dtk_similars list.  
            */
            list_check = move_db_record2new_llist( dtk_similars, dtk_rec, 
                dtk_retrievals, dtk_retrievals_ptr ) ;
            if ( list_check != dtk_similars )
                return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;

            /* now set up for the next iteration.  */
            dtk_rec = next_dtk_rec ;
            dtk_retrievals_ptr = next_dtk_retrievals_ptr ;

        }
        else
            dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, dtk_retrievals_ptr);
    }

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_extract_same_pass()

Description:    extract the same_pass datatakes from the list and put 
                them into the dtk_same_pass list.  also update the 
                antenna_id of the dtk_proposal if necessary.  
                with a same-pass, there is no time overlap.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 19:35:16 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_extract_same_pass(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_retrievals,
    llist       *dtk_same_pass )
{
    cursor      dtk_retrievals_ptr ;
    DB_RECORD   **dtk_rec = NULL ;
    DB_RECORD   **next_dtk_rec = NULL ;
    cursor      next_dtk_retrievals_ptr ;
    llist       *list_check = NULL ;

    /* 
    -- the data-take must be a downlink.  
    -- in that case, return; there will be 
    -- no dtk_same_pass data-takes.  
    */
    if ( !dtkm_is_a_downlink( dtk_proposal ) )
        return TRUE ;

    /* 
    -- check each record in the dtk_retrievals list, move it to 
    -- dtk_same_pass when appropriate.  
    */
    /*
    -- this loop is a bit TRICKY. 
    -- trying to loop through each data-take, 
    -- but also, when we find the desired record, 
    -- we want to take it out of the list 
    -- and put it in another list.  
    -- for that reason, the usual simple 
    -- loop construction is not done here.  
    */
    dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, dtk_retrievals_ptr ) ;
    while ( dtk_rec )
    {
        /*
        -- test the dtk_rec vs dtk_proposal for a 
        -- same_pass.  must not have a time overlap, among 
        -- other tests for equality.  note the strange comparison 
        -- between strttime and stoptime between the two records.  
        -- this is how you AVOID a time overlap.  
        */
        if 
        (   dtkm_is_a_downlink( dtk_rec ) 
        &&  strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], 
                    CAST_DTK_SAT dtk_proposal[DTK_SAT] ) == 0 
        &&  strcmp( CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID], 
                    CAST_DTK_STATION_ID dtk_proposal[DTK_STATION_ID] ) == 0 
        &&  CAST_DTK_REV dtk_rec[DTK_REV] == CAST_DTK_REV dtk_proposal[DTK_REV] 
        &&  (   strcmp( CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME], 
                    CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) > 0 
            ||  strcmp( CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME], 
                    CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) < 0 
            )
        )
        {
            /* 
            -- both the dtk_rec AND dtk_proposal records 
            -- are downlinks after here.  
            */

            /*
            -- one last check for a same_pass.  we want to 
            -- exclude a concurring data-take.  check for 
            -- sensors equal 
            -- and ( dtkid's equal 
            --       or ( dtk_proposal.dtkid == 0 and fadtkid equality ) )
            */
            if 
            ( strcmp(CAST_DTK_SENSOR dtk_proposal[DTK_SENSOR],
                        CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] ) == 0
            &&   ( CAST_DTK_DTKID dtk_proposal[DTK_DTKID] == 
                   CAST_DTK_DTKID dtk_rec[DTK_DTKID]
                 || (  CAST_DTK_DTKID dtk_proposal[DTK_DTKID] == 0 
                    && strcmp( CAST_DTK_FADTKID dtk_proposal[DTK_FADTKID],
                               CAST_DTK_FADTKID dtk_rec[DTK_FADTKID] ) == 0 
                    )
                 )
            )
            {
                /*  
                -- this is a concurring data-take, matched 
                -- by the fadtkid when the dtk_proposal.dtkid == 0.
                */
                /* NOT a same_pass; skip to the next record.  */
                dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                            dtk_retrievals_ptr);
                continue ;
            }

            /* 
            -- this data-take is a SAME_PASS data-take, established
            -- by same sat, rev, station_id, and non-overlapping time 
            -- brackets.  
            */

            /* 
            -- capture/save the next record from the 
            -- list for later processing.  
            */
            next_dtk_retrievals_ptr = dtk_retrievals_ptr ;
            next_dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                    next_dtk_retrievals_ptr);

            /*
            -- this is a SAME_PASS data-take.  
            -- move this record into dtk_same_pass list
            */

            /* 
            -- move the dtk_rec to the dtk_same_pass list.  
            */
            list_check = move_db_record2new_llist( dtk_same_pass, dtk_rec, 
                dtk_retrievals, dtk_retrievals_ptr ) ;
            if ( list_check != dtk_same_pass )
                return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;

            /* now set up for the next iteration.  */
            dtk_rec = next_dtk_rec ;
            dtk_retrievals_ptr = next_dtk_retrievals_ptr ;

        }
        else
            dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, dtk_retrievals_ptr);
    }

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_extract_conflicts_and_parallels()

Description:    extract the conflicting and parallel datatakes from the 
                list and move them into the corresponding list.  also update 
                the antenna_id of the dtk_proposal if necessary.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 19:35:16 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_extract_conflicts_and_parallels(
    DB_RECORD   **dtk_proposal,
    llist       *dtk_retrievals,
    llist       *dtk_conflicts, 
    llist       *dtk_parallels )
{
    cursor      dtk_retrievals_ptr ;
    DB_RECORD   **dtk_rec = NULL ;
    DB_RECORD   **next_dtk_rec = NULL ;
    cursor      next_dtk_retrievals_ptr ;
    llist       *list_check = NULL ;
    int         conflict_status ;
    int         return_code ;

    /* 
    -- check each record in the dtk_retrievals list, move it to 
    -- dtk_conflicts or dtk_parallels when appropriate.  
    */
    dtk_rec = (DB_RECORD **) FIRST(dtk_retrievals, dtk_retrievals_ptr);
    /*
    -- this loop is a bit TRICKY. 
    -- trying to loop through each data-take, 
    -- but also, when we find the desired record, 
    -- we want to take it out of the list 
    -- and put it in another list.  
    -- for that reason, the usual simple 
    -- loop construction is not done here.  
    */
    while ( dtk_rec )
    {
        /* 
        -- check same sat, rev, time overlapping data-takes to see if they 
        -- conflict with the dtk_proposal.  
        -- note the correct but strange comparison between strttimes 
        -- and stoptimes to effectively identify time-overlapping 
        -- data-takes.  
        */
        if 
        ( 
            strcmp( CAST_DTK_SAT dtk_rec[DTK_SAT], 
                    CAST_DTK_SAT dtk_proposal[DTK_SAT] ) == 0 
        &&  CAST_DTK_REV dtk_rec[DTK_REV] == CAST_DTK_REV dtk_proposal[DTK_REV] 
        &&  strcmp( CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME], 
                    CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) <= 0 
        &&  strcmp( CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME], 
                    CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) >= 0 
        )
        {
            /* 
            -- the dtk_rec and dtk_proposal are same sat, rev, and 
            -- time overlapping.  
            */
            return_code = dtkm_activities_conflict( dtk_proposal, dtk_rec ) ;
            if ( return_code < 0 )
                return return_code ;
            conflict_status = return_code ;

            if ( conflict_status == DTKS_CONCUR 
            ||   conflict_status == DTKS_SIMILAR )
            {
                /* skip to the next record.  */
                dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                            dtk_retrievals_ptr);
                continue ;
            }
            else if (  conflict_status == DTKS_PARALLEL 
                    || conflict_status == DTKS_SHOULD_BE_COMBINED 
                    || conflict_status == DTKS_CONFLICT  )
            {
                /* we will move this record to another list.  */

                /* 
                -- capture/save the next record from the 
                -- list for processing.  
                */
                next_dtk_retrievals_ptr = dtk_retrievals_ptr ;
                next_dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, 
                        next_dtk_retrievals_ptr);

                /* 
                -- move the dtk_rec to the appropriate list.  
                */
                if ( conflict_status == DTKS_SHOULD_BE_COMBINED 
                ||   conflict_status == DTKS_CONFLICT          )
                {
                    list_check = move_db_record2new_llist( dtk_conflicts, 
                        dtk_rec, dtk_retrievals, dtk_retrievals_ptr ) ;
                    if ( list_check != dtk_conflicts )
                        return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;
                }
                else if ( conflict_status == DTKS_PARALLEL )
                {
                    list_check = move_db_record2new_llist( dtk_parallels, 
                        dtk_rec, dtk_retrievals, dtk_retrievals_ptr ) ;
                    if ( list_check != dtk_parallels )
                        return DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST ;
                }
                else
                    return 
                    DTKM_ERROR_IN_CODE_dtkm_extract_conflicts_and_parallels_if ;

                /* now set up for the next iteration.  */
                dtk_rec = next_dtk_rec ;
                dtk_retrievals_ptr = next_dtk_retrievals_ptr ;
                continue ;

            }
            else
                return DTKM_ERROR_IN_CODE_dtkm_extract_conflicts_and_parallels_else ;

        }
        else
            dtk_rec = (DB_RECORD **) NEXT(dtk_retrievals, dtk_retrievals_ptr);
    }

    return TRUE ;

}

/*==============================================================================
Function:       dtkm_check_same_sat_conflicts()

Description:    check for conflicts with the same satellite data-takes.

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Fri Nov 10 11:49:44 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_check_same_sat_conflicts( 
    DB_RECORD       **dtk_proposal,
    llist           *dtk_concurs, 
    llist           *dtk_similars, 
    llist           *dtk_conflicts, 
    llist           *dtk_parallels, 
    llist           *dtk_same_pass,
    DB_RECORD       **result_dtk )    /* dtk_proposal might change.  */
{
    char    retrieve_strttime[ASF_TIME_STR_LENGTH+1] ;
    char    retrieve_stoptime[ASF_TIME_STR_LENGTH+1] ;

    llist   *dtk_retrievals = NULL ;
    char    dtkstat_where_clause[100] ;
    int     return_code ;

    /* quick error checking.  */
    if ( dtk_proposal == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    if (dtk_concurs == NULL)
        return DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_concurs) != 0 )
        return DTKM_ERROR_CONCURS_LIST_NOT_EMPTY ;
 
    if (dtk_similars == NULL)
        return DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_similars) != 0 )
        return DTKM_ERROR_SIMILARS_LIST_NOT_EMPTY ;
 
    if (dtk_conflicts == NULL)
        return DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_conflicts) != 0 )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY ;
 
    if (dtk_parallels == NULL)
        return DTKM_ERROR_PARALLELS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_parallels) != 0 )
        return DTKM_ERROR_PARALLELS_LIST_NOT_EMPTY ;
 
    if (dtk_same_pass == NULL)
        return DTKM_ERROR_SAME_PASS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS(dtk_same_pass) != 0 )
        return DTKM_ERROR_SAME_PASS_LIST_NOT_EMPTY ;

    db_copy_record( APS_CDEFS(DTK), result_dtk, dtk_proposal ) ;
    /*
    -- retrieve the relevant data-takes from the database.  
    -- then separate them into the output dtk lists.
    -- return TRUE if everything went OK.  
    -- if not, return the appropriate error code < 0 .
    */
    /*
    -- start by retrieving all nearby same sat data-takes 
    -- in the same rev.  
    */
    /*
    -- nearby means in the same pass.  about 15 minutes is the 
    -- maximum pass.  use 20 minutes for safety.  
    */

    /* subtract 20 minutes from strttime; convert 20 minutes to days. */
    if ( !tc_asf_add_ndays( CAST_DTK_STRTTIME result_dtk[DTK_STRTTIME],
                            -(20.0/60.0/24.0), retrieve_strttime )    )
        return DTKM_ERROR_IN_MAKING_RETRIEVE_TIME_BRACKET ;

    /* add 20 minutes to stoptime; convert 20 minutes to days. */
    if ( !tc_asf_add_ndays( CAST_DTK_STOPTIME result_dtk[DTK_STOPTIME],
                            (20.0/60.0/24.0), retrieve_stoptime )  )
        return DTKM_ERROR_IN_MAKING_RETRIEVE_TIME_BRACKET ;

    sprintf(where_clause,
        "where %s = '%s' and %s = %ld and %s = '%s' and %s < '%s' and %s > '%s' ",
        APS_COL(DTK, DTK_SAT),        CAST_DTK_SAT result_dtk[DTK_SAT],
        APS_COL(DTK, DTK_REV),        CAST_DTK_REV result_dtk[DTK_REV],
        APS_COL(DTK, DTK_STATION_ID),
                CAST_DTK_STATION_ID result_dtk[DTK_STATION_ID],
        APS_COL(DTK, DTK_STRTTIME),   retrieve_stoptime, 
        APS_COL(DTK, DTK_STOPTIME),   retrieve_strttime ) ;

    /* 
    --  according to the status if the proposal, adjust the 
    --  DTKSTAT qualifiers in the retrieve from the database.  
    --
    --  When checking the dtk relation, the status of the dtk proposal
    --  limits which records are retrieved:
    --
    --  dtk proposal    check only dtks
    --  status          with status of:
    --  ------------    ---------------
    --  QUE             SCH, SUB, PLN, QUE
    --  PLN             SCH, SUB, PLN
    --  REQ             SCH, SUB, PLN
    --  SCH             SCH, PLN
    --
    --  Thus when processing a dtk proposal with status = SCH, all records
    --  in the dtk relation with a status other than SCH and PLN are
    --  completely ignored.
    */
 
    return_code = dtkm_get_dtkstat_where_clause(dtk_proposal, 
            dtkstat_where_clause ) ;
    if ( return_code < 0 )
        return return_code ; ;
    strcat(where_clause, dtkstat_where_clause ) ;

    strcpy(orderby_cols, APS_COL(DTK, DTK_STRTTIME) ) ;

#ifdef PRINT_DIAG
    printf("%s(%d):   where_clause = \n%s\n   orderby_cols = %s \n", 
        __FILE__, __LINE__, where_clause, orderby_cols ) ;
#endif
    dtk_retrievals = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(DTK), where_clause, orderby_cols, APS_CDEFS(DTK), ALL_COLS ) ;
    if ( dtk_retrievals == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ;

#ifdef PRINT_DIAG
    printf("%s(%d):  dtk_proposal = \n", __FILE__, __LINE__ ) ;
    dtkm_print( stdout, dtk_proposal ) ;
    printf("%s(%d):   dtk_retrievals:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_retrievals ) ) ;
    dtkm_print_list(stdout, dtk_retrievals ) ;
#endif

    /*
    -- STEP 2.2  in the multi-antenna algorithm.  
    */

    /* 
    -- NOTE:  
    -- the antenna_id in the result_dtk will get updated 
    -- if the dtks encountered have a different antenna_id.
    -- every data-take in the same pass should use the same antenna.  
    -- we put in a check to detect if more than one antenna 
    -- is used in the same pass.  this would be a database error.  
    */

    /* 
    -- concurring dtk:  match of sat, sensor, rev, and (dtkid or fadtkid) 
    -- regardless of status.  
    -- NOTE:  this routine removes the extracted dtk DB_RECORD out of 
    -- the dtk_retrievals list and into the dtk_concurs list.  
    */
    return_code = dtkm_extract_concurs( result_dtk, dtk_retrievals, 
        dtk_concurs ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( dtk_retrievals ) ;
        return return_code ;
    }

    /* 
    -- similar dtk:  match of sat, sensor, rev, and NOT (dtkid or fadtkid) 
    -- NOTE:  this routine removes the extracted dtk DB_RECORD out of 
    -- the dtk_retrievals list and into the dtk_similars list.  
    */
    /*
    -- NOTE:  currently, with the Radarsat not reporting sensor/mode 
    -- in their files sent to us, dtkm_extract_similars() needs 
    -- to be run before dtkm_extract_conflicts_and_parallels().  
    */ 
    return_code = dtkm_extract_similars( result_dtk, dtk_retrievals, 
        dtk_similars ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( dtk_retrievals ) ;
        return return_code ;
    }

    /* 
    -- "same pass" dtk:  downlink data-takes in the same rev 
    -- and the same station that do not overlap in time with 
    -- the dtk proposal.  
    -- if the result_dtk and the dtk_retrievals dtk are not both downlinks, 
    -- then destroy the dtk_retrievals and don't return it in the 
    -- dtk_same_pass list.  
    -- they must be moved to a new antenna if the dtk_proposal, being 
    -- a downlink as well, has to go on a different antenna.  
    --
    -- NOTE:  this routine removes the extracted dtk DB_RECORD out of 
    -- the dtk_retrievals list and into the dtk_same_pass list.  
    */
    return_code = dtkm_extract_same_pass( result_dtk, dtk_retrievals, 
        dtk_same_pass ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( dtk_retrievals ) ;
        return return_code ;
    }
    /*
    -- at this point, the only dtks left will be 
    -- time-overlapping conflict possibilities.
    -- NOTE:  this routine removes the extracted dtk DB_RECORD out of 
    -- the dtk_retrievals list and into the dtk_conflicts and 
    -- dtk_retrievals lists.  
    */
    /*
    -- NOTE:  currently, with the Radarsat not reporting sensor/mode 
    -- in their files sent to us, 
    -- dtkm_extract_conflicts_and_parallels() needs 
    -- to be run AFTER dtkm_extract_similars().  
    */ 
    return_code = dtkm_extract_conflicts_and_parallels( 
        result_dtk, dtk_retrievals, 
        dtk_conflicts, dtk_parallels ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( dtk_retrievals ) ;
        return return_code ;
    }

#ifdef PRINT_DIAG

    printf("%s(%d):   dtk_concurs:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_concurs ) ) ;
    dtkm_print_list( stdout, dtk_concurs ) ;

    printf("%s(%d):   dtk_similars:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_similars ) ) ;
    dtkm_print_list( stdout, dtk_similars ) ;

    printf("%s(%d):   dtk_conflicts:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_conflicts ) ) ;
    dtkm_print_list( stdout, dtk_conflicts ) ;

    printf("%s(%d):   dtk_parallels:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_parallels ) ) ;
    dtkm_print_list( stdout, dtk_parallels ) ;

    printf("%s(%d):   dtk_same_pass:  %d\n", __FILE__, __LINE__, 
        NUMELTS( dtk_same_pass ) ) ;
    dtkm_print_list( stdout, dtk_same_pass ) ;

#endif

    /* 
    -- we wish a 0 value in the proposed 
    -- DTK_ANTENNA_ID to indicate that 
    -- the planner's antena preferences in the DB 
    -- will operate to select the desired 
    -- antenna.  
    */

    if ( dtkm_is_an_observation( result_dtk ) == TRUE )
    {
        /* 
        -- if the data-take is an observation - no downlink - the 
        -- members in the two lists dtk_parallels and dtk_same_pass 
        -- are no longer needed.  however, it was OK to remove 
        -- them from the dtk_retrievals list at the time.  
        */
        DEL_ALL( dtk_same_pass ) ;
        DEL_ALL( dtk_parallels ) ;
    }

    /*
    -- at this point, the dtk_retrievals list 
    -- is no longer needed.  
    */
    DEL_LIST( dtk_retrievals ) ;

    return TRUE ;

}
