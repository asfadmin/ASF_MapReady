#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_process_dtk.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_process_dtk.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_process_dtk.c"



/*==============================================================================
Function:       dtkm_process_dtk

Description:    performs the part of the multi-antenna algorithm that 
                processes a single data-take.  

The expected status of the input dtk proposal is: 

DEL, which indicates a deletion by either the planner or an FA
REJ, which indicates a rejection by either the planner or an FA (handled as DEL)
REQ, which indicates an FA request (it will have a PLN status when processed), 
PLN, which indicates a data-take from an FA plan or an FA 
     confirmation of an ASF request, or 
SCH, which indicates a data-take from an FA schedule.  
QUE, which indicates a not-yet-requested data-take from the planner.  


Returns:        
    int
    >= 0 normal:
        DTKM_DTK_DELETED_OK 
        DTKM_DTK_PROPOSAL_REJ 
        DTKM_DTK_PROPOSAL_CONFLICT 
        DTKM_DTK_ACCEPTED 
        DTKM_DTK_ACCEPTED_INHERITANCE

    < 0 ERROR:  

Creator:        Lawrence Stevens

Creation Date:  Fri Oct 27 22:13:04 PDT 1995

Notes:      
See etc/notes/Multi-antenna.txt for the algorithm.  

==============================================================================*/
#include "dtkm.h"  /* each dtkm source file will have this include file. */
#include "db_dtk.h"

#include <string.h>     /* for strcpy() strcmp()   */

int dtkm_process_dtk(
    DBPROCESS   *APS_dbproc,     /* Sybase db process                       */
    DB_RECORD   **input_dtk_rec, /* data-take proposal.                     */
    DB_RECORD   **result_dtk,    /* data-take with changed info             */
    DB_RECORD   **downlink_dtk,  /* downlink datatake associated 
                                    with observation datatake proposal */
    llist       *dtk_sat_down_times, /* list of conflicting sat down times  */
    llist       *antenna_down_times,/* list of antenna down times encountered */
    llist       *dtk_concurs,    /* data-takes that match the proposal       */
    llist       *dtk_similars,   /* list of data-takes that the proposed     */
                                 /* data-take is similar to                  */
    llist       *dtk_conflicts,  /* list of conflicting data-takes           */
    llist       *dtk_parallels,  /* list of parallel data-takes              */
    llist       *dtk_same_pass,  /* list of same_pass data-takes             */
    llist       *dtk_updates,    /* list of updated data-takes, not proposal */ 
    llist       *dtk_bumps )     /* list of bumped data-takes to process.    */ 
{
    int     return_code ;

    int     inheritance_flag = 0 ;    /* if 1:  obs dtkstat copied from DL   */
    char    observation_dtkstat[4] ;  /* saves obs dtkstat for comparison.   */

    llist   *llist_check ;             /* only used for checking a return.   */
    DB_RECORD   **changed_dtk = NULL ; /* a data-take with changed info      */
    DBSMALLINT  dbsmallint_0 = 0 ;  /* used to set a value; pointer required.*/
                                    /* cannot be done using a literal.       */
    /* 
    -- NOTE:  
    -- the input_dtk_rec is the input data-take record.  we don't want 
    -- to write on it.  
    -- we will copy the input record input_dtk_rec to db 
    -- record called dtk_result and edit it when necessary.  
    */

    /* 
    -- parameter error checking  
    */
    if ( input_dtk_rec == NULL ) 
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( dtk_sat_down_times == NULL )
        return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_sat_down_times ) != 0 )
        return DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY ;

    if ( antenna_down_times == NULL )
        return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( antenna_down_times ) != 0 )
        return DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY ;

    if ( dtk_concurs == NULL )
        return DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_concurs ) != 0 )
        return DTKM_ERROR_CONCURS_LIST_NOT_EMPTY ;

    if ( dtk_similars == NULL )
        return DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_similars ) != 0 )
        return DTKM_ERROR_SIMILARS_LIST_NOT_EMPTY ;

    if ( dtk_conflicts == NULL )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_conflicts ) != 0 )
        return DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY ;

    if ( dtk_parallels == NULL )
        return DTKM_ERROR_PARALLELS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_parallels ) != 0 )
        return DTKM_ERROR_PARALLELS_LIST_NOT_EMPTY ;

    if ( dtk_same_pass == NULL )
        return DTKM_ERROR_SAME_PASS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_same_pass ) != 0 )
        return DTKM_ERROR_SAME_PASS_LIST_NOT_EMPTY ;

    if ( result_dtk == NULL ) 
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    if ( NUMELTS( dtk_updates ) != 0 )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY ;

    if ( dtk_bumps == NULL )
        return DTKM_ERROR_BUMPS_LIST_NOT_INITIALIZED ;
    if ( NUMELTS( dtk_bumps ) != 0 )
        return DTKM_ERROR_BUMPS_LIST_NOT_EMPTY ;

    /* 
    -- check values; add coverage location points to the rec if needed.  
    -- the input_dtk_rec is not changed; results are placed in the 
    -- second dtk parameter, in this case result_dtk. We will 
    -- work with the result_dtk record from now on, never changing 
    -- the input_dtk_rec record.  
    */
    return_code = dtkm_check_values( input_dtk_rec, result_dtk ) ;
    if ( return_code < 0 ) 
        return return_code ;

    /* 
    -- FROM HERE ON, we work with result_dtk    -     -     -     -
    */

    /* check status of data-take  */
    if ( strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "DEL" ) != 0
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "REJ" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "INV" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "QUE" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SUB" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "REP" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "REQ" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH" ) != 0 )
        return 
        DTKM_ERROR_DTK_SHOULD_BE_DEL_REJ_INV_QUE_SUB_REQ_PLN_OR_SCH_STATUS ;

    /* adjust the status of the result_dtk as needed.  */
    if ( strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "REP" ) == 0 
    ||   strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "REQ" ) == 0 )
        (void) strcpy(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN" ) ;

    /*
    -- Now update dtk.proposed_dtkstat with the proposed 
    -- dtk.dtkstat value:
    */
    return_code = dtkm_set_proposed_dtkstat( result_dtk, result_dtk ) ;
    if ( return_code < 0 )
        return return_code ;

    /* 
    -- the following allocated data-take record is used later whenever 
    -- a routine outputs the changed version of the result_dtk we use.
    */
    changed_dtk = new_table_record(APS_CDEFS(DTK)) ;

    /*
    -- handle the REJ and DEL cases here:
    */
    if ( strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "REJ" ) == 0 
    ||   strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "DEL" ) == 0 )
    {
        /*
        -- If the status of the data-take is DEL or REJ, the data-take is
        -- processed as follows:
        -- 1.1  The status value of the data-take in the database is changed;
        -- 1.2  Other-satellite data-takes that could have conflicted with
        --      this data-take are retrieved.  These data-takes have a status
        --      of either CON, SCH, PLN, QUE, or SUB.  They have a time bracket
        --      that overlaps the antenna time-padded REJ/DEL data-take.
        --      For each other-satellite found:
        --      o   set the status to BMP and update the dtk record.
        --      o   place the data-take at the end of the list of next-dtk
        --          proposals.
        -- 1.3  PROCESS STARTS AGAIN WITH NEXT DTK PROPOSAL IN THE LIST.
        */

        /* 
        --  Step 1.1:  dtkm_update_dtk() will update the database in the 
        --  correct way.  
        --  it will also update the dtk_updates list with 
        --  a duplicate of the update.  
        */
        return_code = dtkm_update_dtk( APS_dbproc, result_dtk, changed_dtk,
            dtk_updates ) ;
        if ( return_code < 0 )
        {
            db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
            free_db_record( changed_dtk ) ;
            return return_code ;
        }

        if ( dtkm_is_an_observation( result_dtk ) == TRUE 
        &&   strcmp( CAST_DTK_DTKSTAT changed_dtk[DTK_DTKSTAT],
                     CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT] ) != 0 )
        {
            /* 
            -- the dtkstat of the result_dtk got changed.  
            -- this would have been handled in a call to dtkm_update_dtk().  
            -- this was due to the downlink data-take 
            -- having a different dtkstat, which was inherited by 
            -- the observation:
            */
            inheritance_flag = 1 ;

            /* 
            -- also, need to get the downlink data-take for this obs, 
            -- for informational purposes, to return to calling routine: 
            -- this call will fill the downlink_dtk rec.  
            */
            return_code = dtkm_obs2dl( changed_dtk, downlink_dtk ) ;

            /* observations return here.  */
            db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
            free_db_record( changed_dtk ) ;

            if (inheritance_flag)
                return DTKM_DTK_ACCEPTED_INHERITANCE ;
            else
                return DTKM_DTK_DELETED_OK ;

        }

        db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
        free_db_record( changed_dtk ) ;

        /* 
        -- if the REJ or DEL data-take is a downlink, 
        -- propagate its observations (recordings or 
        -- real time observations) 
        -- to the same status; update the data-takes in the DB:
        -- NOTE:  This was just done by dtkm_update_dtk()
        */

        /*
        -- if the current data-take is NOT a downlink 
        -- data-take, then we return here; no more 
        -- processing.  
        */
        return_code = dtkm_is_a_downlink(result_dtk) ;
        if ( return_code < 0 )
            return return_code ;
        if ( return_code != TRUE )
            return DTKM_DTK_DELETED_OK ;

        /* the data-take IS a downlink because return_code = TRUE  */

        /*
        -- 1.2  Other-satellite data-takes that could have conflicted with
        --      this data-take are retrieved.  These data-takes have a status
        --      of either CON, SCH, PLN, QUE, or SUB.  They have a time bracket
        --      that overlaps the antenna time-padded REJ/DEL data-take.
        */
        return_code = dtkm_get_rej_conflicts( result_dtk, dtk_conflicts ) ;
        if ( return_code < 0 )
            return return_code ;

        if ( NUMELTS( dtk_conflicts ) > 0 )
        {
            /*
            --      For each other-satellite other-antenna data-take found:
            --      o   set the status to BMP and update the dtk record.
            --      o   place the data-take at the end of the list of next-dtk
            --          proposals.
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 1.2  put REJ/DEL conflicts into bumps list\n",
                __FILE__, __LINE__ ) ;
#endif
            /*
            -- change all of the dtk.antenna_id values in the
            -- dtk_conflicts list to 0 so that the antenna preferences
            -- will determine the desired antenna_id, later.
            -- Each of these data-takes will be re-submitted using 
            -- antenna_id = 0.
            -- Note:  DTK_ANTENNA_ID is a DBSMALLINT type storage.
            */
            llist_check = set_db_record_values( APS_CDEFS(DTK), DTK_ANTENNA_ID,
                &dbsmallint_0, dtk_conflicts ) ;
            if ( llist_check != dtk_conflicts )
                return DTKM_ERROR_SETTING_ZERO_VALUE ;

            /* 
            -- update status of dtk_conflicts data-takes to BMP.  
            -- AND copy them to the list of bumped data-takes.  
            -- dtkm_update_dtks_field() will update all of the 
            -- fields in the record, if the DTK_DTKSTAT is not "BMP",
            -- and I know each record will get updated.  
            */
            return_code = dtkm_update_dtks_field(APS_dbproc, dtk_conflicts, 
                DTK_DTKSTAT, "BMP", dtk_bumps ) ;
            if ( return_code < 0 )
                return return_code ;

            /*
            -- now we must remove every member from dtk_conflicts 
            -- because they are in the dtk_bumps list now.  
            */
            DEL_ALL( dtk_conflicts ) ;
        }

        return DTKM_DTK_DELETED_OK ;
    }

    /*
    -- handle the INV case here; a kluge for ADEOS.  
    -- the Realtime downlinks can contain more than 
    -- one observation on a channel.  
    */
    if ( strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "INV" ) == 0 )
    {
        return_code = dtkm_update_dtk( APS_dbproc, result_dtk, changed_dtk,
            dtk_updates ) ;
        db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
        free_db_record( changed_dtk ) ;

        if ( return_code < 0 )
            return return_code ;
        else
            return DTKM_DTK_ACCEPTED ;
    }


    /*
    -- STEP 2.1
    -- check satellite equipment status
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 2.1 check satellite equipment status\n", 
        __FILE__, __LINE__ ) ;
    dtkm_print(stdout, result_dtk ) ;
#endif
    return_code = dtkm_check_sat_equipment( result_dtk, dtk_sat_down_times  ) ;
    if ( return_code < 0 )
    {
        free_db_record( changed_dtk ) ;
        return return_code ;
    }

    /*
    -- STEP 2.2
    -- check for conflicts with the same satellite data-takes.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 2.2 check for same satellite conflicts\n", 
        __FILE__, __LINE__ ) ;
#endif
    return_code = dtkm_check_same_sat_conflicts( result_dtk, 
        dtk_concurs, dtk_similars, dtk_conflicts,
        dtk_parallels, dtk_same_pass, result_dtk ) ;
    if ( return_code < 0 )
    {
        free_db_record( changed_dtk ) ;
        return return_code ;
    }


    /*
    -- STEP 3. Check Condition from Step 2
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 3 Check Condition from Step 2\n", 
        __FILE__, __LINE__ ) ;
#endif

    if ( NUMELTS(dtk_conflicts) > 0 )
    {
        /*
        -- STEP 3.1
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 3.1 same-satellite conflicts\n", 
            __FILE__, __LINE__ ) ;
#endif

        /* SAME SATELLITE CONFLICT.  */
        if ( strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN") == 0
        ||   strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH") == 0 )
        {
            /*
            -- special case:  the dtk proposal is a PLN or SCH.  
            -- This means that the flight agency as approved of 
            -- this data-take.  If the flight agency
            -- has approved of this data-take, then it has passed
            -- all necessary conflict analysis for that satellite.  
            -- However, we have conflicting same-satellite data-takes
            -- for this satellite in our databases.  
            -- Therefore, we need to change the conflicting dtks 
            -- in our database to make them DEL.  
            -- And then allow our data-take proposal to continue with 
            -- the acceptance process.
            -- We are relying on the flight agency to do their own
            -- conflict analysis for their own satellite if the 
            -- data-take has been approved by it.  
            */
            return_code = dtkm_update_dtks_field( APS_dbproc, dtk_conflicts,
                DTK_DTKSTAT, "DEL", dtk_updates ) ;
 
            /* 
            -- now delete all the members of the conflict list, which 
            -- are all of this same satellite at this time:  
            */
            DEL_ALL( dtk_conflicts ) ;
            /* 
            -- the data-take has just DEL all of the 
            -- same-satellite conflicts.  
            -- continue processing.  
            */
            
        }
        else
        {
            /* 
            -- the dtk proposal conflicts with other same-satellite 
            -- activities.  
            -- it's status is neither PLN nor SCH; 
            -- we must reject it due to same-satellite conflicts
            -- and end processing:
            -- (it must be either QUE or SUB, so would not be a 
            -- tape dump activity.  ASF never 
            -- creates a tape dump activity as a request.  )
            */
            free_db_record( changed_dtk ) ;
            return DTKM_DTK_PROPOSAL_REJ ;
        }
    }

    if ( NUMELTS(dtk_sat_down_times) > 0 )
    {
        /*
        -- STEP 3.2
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 3.2   satellite equipment is down\n", 
            __FILE__, __LINE__ ) ;
#endif
        /* SATELLITE EQUIPMENT DOWN.  */
        if ( strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN") == 0
        ||   strcmp(CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH") == 0 )
        {
            /*
            -- special case:  the dtk proposal is a PLN or SCH.  
            -- This means that the flight agency as approved of 
            -- this data-take.  If the flight agency
            -- has approved of this data-take, then it has passed
            -- all necessary equipment checks for that satellite.  
            -- Therefore, we need to allow this data-take to continue 
            -- with the acceptance process.
            -- We are relying on the flight agency to do their own
            -- equipment checks for their satellite if the data-take 
            -- has been approved by it.  
            -- now delete all the members of the dtk_sat_down_times 
            -- list:  
            */
            DEL_ALL( dtk_sat_down_times ) ;
            /* 
            -- continue processing.  
            */
            
        }
        else
        {
            /* 
            -- status is neither PLN nor SCH; it cannot be a 
            -- tape dump.  equipment check by APS failed:  
            -- (it must be either QUE or SUB, so would not be a 
            -- tape dump activity.  ASF never 
            -- creates a tape dump activity as a request.  )
            */
            free_db_record( changed_dtk ) ;
            return DTKM_DTK_PROPOSAL_REJ ;
        }
    }

    /*
    -- STEP 3.3
    -- NO CONFLICTS
    -- The process continues with Step 4.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 3.3   no same-satellite conflicts\n", 
        __FILE__, __LINE__ ) ;
#endif

    /*
    -- STEP 4.  If the data-take is an observation (recording or 
    --          real-time observation, it is accepted; skip to step 9.  
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 4 If an observation, skip to step 9\n", 
        __FILE__, __LINE__ ) ;
#endif

    if ( dtkm_is_a_downlink( result_dtk ) == TRUE ) 
    {
#ifdef PRINT_DIAG
        printf("%s(%d):  data-take is a downlink.\n", 
            __FILE__, __LINE__ ) ;
#endif

        /* 
        -- this data-take is a downlink, not an
        -- observation.  therefore, retain only the 
        -- downlink dtks in the dtk_parallels list.  
        -- the observations were needed earlier when checking for 
        -- same-satellite conflicts, but now that we 
        -- are checking just for antenna conflicts 
        -- and usage, delete them from this list.  
        */
        return_code = dtkm_remove_observations( dtk_parallels ) ;

        /*
        -- Steps 5, 6, 7, 8.
        */
        return_code = dtkm_find_antenna_for_dtk( APS_dbproc, result_dtk, 
            dtk_similars, dtk_parallels, dtk_same_pass, 
            antenna_down_times, dtk_conflicts, changed_dtk ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
        db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;

        if ( return_code == DTKM_NO_AVAILABLE_ANTENNA )
        {
#ifdef PRINT_DIAG
            printf("%s(%d):  NO_AVAILABLE_ANTENNA\n", __FILE__, __LINE__ ) ;
#endif
            /*
            -- There was no antenna to use.  Place the dtk proposal into 
            -- the database with a status of CON.  It will await 
            -- the possibility that a conflicting data-take 
            -- gets rejected.  first save the originally proposed status.  
            */
            return_code = dtkm_set_proposed_dtkstat( result_dtk, result_dtk ) ;
            if ( return_code < 0 )
            {
                free_db_record( changed_dtk ) ;
                return return_code ;
            }

            (void) strcpy( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "CON" ) ;

            /* 
            --  dtkm_update_dtk() will update the downlink in the 
            --  correct way.  
            --  it will also save a duplicate of the change into 
            --  dtk_updates.  And propagate dtkstat, too.  
            */
            return_code = dtkm_update_dtk(APS_dbproc, 
                result_dtk, changed_dtk, dtk_updates ) ; 
            db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
            if ( return_code < 0 )
            {
                free_db_record( changed_dtk ) ;
                return return_code ;
            }

            /* return now. */
            free_db_record( changed_dtk ) ;
            return DTKM_DTK_PROPOSAL_CONFLICT ;
        }
    }

    /* THE DATA-TAKE PROPOSAL WAS ACCEPTED.  */
    /*
    -- STEP 9.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 9.  The data-take proposal was ACCEPTED\n", 
        __FILE__, __LINE__ ) ;
    dtkm_print(stdout, result_dtk ) ;
#endif

    /*
    -- STEP 9.0 Not in the Multi-antenna.txt document yet.
    --          Downlink data-takes will pass their DTK_DTKSTAT to any
    --          of their observations (recordings or realtime observations).  
    --          Observations inherit this value from their corresponding 
    --          downlinks.
    */
    /*
    -- One helper:  
    -- now that the dtk proposal was accepted: 
    -- if the fa_schedule_link in our dtk record 
    -- has no value, AND the dtkid value is 0 in 
    -- our data-take, we will try to obtain an fa_schedule_link 
    -- value from a dtk in the similars list.  
    -- because the dtk proposal winds up updating the similar, 
    -- rather than inserting a new dtk record, 
    -- we want to obtain the fa_schedule_link to use 
    -- in order to either obtain or update the status 
    -- of the associated downlink or observations.  
    -- If a downlink, the proposed status will update 
    -- the observations.  If the proposal is an observation, 
    -- we need to use the fa_schedule_link to obtain the 
    -- status of the downlink, and use that status.    
    */
    if( strlen( CAST_DTK_FA_SCHEDULE_LINK result_dtk[DTK_FA_SCHEDULE_LINK] )
        == 0 
    &&  CAST_DTK_DTKID result_dtk[DTK_DTKID] == 0 )
    {
        return_code = dtkm_set_fa_schedule_link_from_list( 
            dtk_similars, result_dtk ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
    }

    if ( dtkm_is_an_observation( result_dtk ) == TRUE )
    {
        /* 
        -- the observation data-take proposal was accepted.  
        -- however, it depends on a downlink data-take to 
        -- downlink it.  for that reason, this observation 
        -- must accept the status of that tape dump or real time 
        -- downlink (if it exists right  now in the dtk 
        -- relation); we must find it and set the dtkstat field value 
        -- from it in our result_dtk.  
        -- we do this only when our data-take is being accepted:
        -- save the value of dtkstat to compare later.  We need 
        -- to set inheritance_flag to 1 if there was a change 
        -- due to the downlink dtkstat being different.  
        */
        strcpy( observation_dtkstat, CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]);
    }
    else
        strcpy( observation_dtkstat, "" ) ;

    /*
    -- Now update the 
    -- database with its info.  
    */

    if ( NUMELTS( dtk_conflicts ) > 0 )
    {
        /*
        -- STEP 9.1
        */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 9.1  put conflicts into bumps list\n", 
        __FILE__, __LINE__ ) ;
#endif
        /* 
        -- there were conflicts.  but the result_dtk was 
        -- accepted.  this means that the conflicts were bumped.  
        -- update their status to BMP.  
        -- and copy the conflicts to the list of bumped data-takes.  
        */
        return_code = dtkm_update_dtks_field(APS_dbproc, dtk_conflicts, 
            DTK_DTKSTAT, "BMP", dtk_bumps ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
        /*
        -- the changed data-takes were copied to dtk_bumps.  
        -- every data-take was updated; none of them had a 
        -- status of "BMP".  
        -- now we must remove every member from dtk_conflicts 
        -- because they are not a reason for the dtk_proposal 
        -- not to be placed in the schedule.
        -- the dtk_proposal was accepted.  
        */
        DEL_ALL( dtk_conflicts ) ;
    }

    /* THE DATA-TAKE PROPOSAL WAS ACCEPTED (CONTINUED).  */
    /* 
    -- NOTE:  
    -- at this time, Sat Dec  2 21:04:16 PST 1995, the 
    -- sensor/mode for Radarsat is not in the schedules 
    -- they send to us.  the data-take would then have 
    -- R1 SAR for the satellite and sensor values.  
    -- Since this code processes the  
    -- schedules, we would like to take the chance to 
    -- put in the correct sensor/mode in the data-take proposal.  
    -- so if the values are R1 SAR for the dtk proposal, and 
    -- if there is a concurring or similar data-take, 
    -- and if the sensor value is different, then we update 
    -- the sensor field in the data-take proposal.  
    -- also, if there is a SAR in the dtk_similars list and if 
    -- the dtk proposal is not SAR, then we update the 
    -- record in the dtk_similars list.  
    */
    if ( strcmp( CAST_DTK_SAT result_dtk[DTK_SAT], "R1" ) == 0 
    &&   ( NUMELTS( dtk_similars ) > 0 || NUMELTS( dtk_concurs ) > 0 )  )
    {
        return_code = dtkm_r1_update_sensor( APS_dbproc, 
            result_dtk, result_dtk, 
            dtk_concurs, dtk_similars, dtk_updates ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
    }

    /* 
    -- STEP 9.2
    -- Process the dtk proposal, the concurs, and the similars:
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 9.2 Process dtk proposal, concurs, and similars\n", 
        __FILE__, __LINE__ ) ;
#endif
    if ( ( strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "SCH" ) == 0 
         ||strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], "PLN" ) == 0 )
    &&   NUMELTS( dtk_similars ) > 0   )
    {
        /*
        --  If the dtk proposal is SCH, AND if there are similar data-takes,
        --  STEP 9.2.1
        --      combine the dtk proposal, the similars, and the concurring
        --      dtk, if any, into one data-take using the time bracket and
        --      antenna from the dtk proposal.
        */
#ifdef PRINT_DIAG
        printf(
            "%s(%d):  STEP 9.2.1 combine dtk proposal, similars, and concur\n",
            __FILE__, __LINE__ ) ;
#endif
        return_code = dtkm_combine_dtks( APS_dbproc, result_dtk, 
            dtk_concurs, dtk_similars, changed_dtk, dtk_updates ) ;
        db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
    }
    else 
    {
        /*
        --  otherwise:    ( #similars = 0 OR (dtkstat not SCH and not PLN) )
        --
        --  STEP 9.2.2   
        --      if there is a concurring data-take, update it with the dtk
        --      proposal, if not, insert the dtk proposal into the db.
        --  STEP 9.2.3   
        --      if there are any similars, update all of them with the
        --      antenna from the dtk proposal.
        */

        if ( NUMELTS( dtk_concurs ) > 0 )
        {
            /*
            -- STEP 9.2.2
            -- if there is a concurring data-take, update it with 
            -- the dtk proposal
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 9.2.2 update the concurring dtk\n", 
                __FILE__, __LINE__ ) ;
#endif
            /* 
            -- UPDATE the concurring data-take with the result_dtk 
            -- info.  
            -- a duplicated of the change is saved into dtk_updates 
            */
            return_code = dtkm_update_dtk( APS_dbproc, 
                result_dtk, changed_dtk, dtk_updates ) ;
            db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
            if ( return_code < 0 )
            {
                free_db_record( changed_dtk ) ;
                return return_code ;
            }
        }
        else
        {
            /*
            -- STEP 9.2.3
            -- if there is no concurring data-take, insert the
            -- dtk proposal into the db.
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 9.2.3 insert dtk proposal into the db\n", 
                __FILE__, __LINE__ ) ;
#endif
            /* 
            -- must now INSERT the data-take proposal to the db.   
            -- it will add to dtk_updates list.  
            -- this is handled by dtkm_update_dtk() which will 
            -- note that the result_dtk needs to be insert 
            -- due to dtkid = 0.  
            */
            return_code = dtkm_update_dtk( APS_dbproc, 
                result_dtk, changed_dtk, dtk_updates ) ;
            db_copy_record( APS_CDEFS(DTK), result_dtk, changed_dtk ) ;
            if ( return_code < 0 )
            {
                free_db_record( changed_dtk ) ;
                return return_code ;
            }
        }

        /*   #similars = 0 OR (dtkstat neither SCH nor PLN)   */
        if ( NUMELTS( dtk_similars ) > 0 
        &&   dtkm_is_a_downlink( result_dtk )  == TRUE )
        {
            /*   
            -- dtkstat is neither SCH nor PLN   
            -- there are similars, and this is a downlink.  
            */
            /*
            -- STEP 9.2.4
            -- if there are any similars, update all of them with the
            -- antenna from the dtk proposal.
            */
#ifdef PRINT_DIAG
            printf("%s(%d):  STEP 9.2.4  update antenna for each similar dtk\n",
                __FILE__, __LINE__ ) ;
#endif
            /* update the antenna_id of the dtk_similars.  */
            return_code = dtkm_update_dtks_field( APS_dbproc, dtk_similars, 
                DTK_ANTENNA_ID,
                &(CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID]), 
                dtk_updates ) ;
            if ( return_code < 0 )
            {
                free_db_record( changed_dtk ) ;
                return return_code ;
            }
        }
    }

    /*
    -- end of STEP 9.2
    -- endif  for statement that handles the dtk_proposal, the 
    -- dtk_concurs, and the dtk_similars 
    */

    if ( dtkm_is_an_observation( result_dtk ) == TRUE 
    &&   strcmp( observation_dtkstat, 
                 CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT] ) != 0 )
    {
        /* between the time we saved the status, and now, 
        -- the dtkstat of the result_dtk got changed.  
        -- this would have been handled in a call to dtkm_update_dtk().  
        -- this was due to the downlink data-take 
        -- having a different dtkstat, which was inherited by 
        -- the observation:
        */
        inheritance_flag = 1 ;

        /* 
        -- also, need to get the downlink data-take for this obs, 
        -- for informational purposes, to return to calling routine: 
        -- this call will fill the downlink_dtk rec.  
        */
        return_code = dtkm_obs2dl( result_dtk, downlink_dtk ) ;

    }

    /*  
    -- One last thing.  For a realtime downlink, hunt down 
    -- its real time observations, and either trim them 
    -- or DEL them.  If it overlaps the downlink, then 
    -- trim it to fit.  If it does not time-overlap the 
    -- downlink, then it will be DEL.  
    -- Do this only where satsensor.low_bit_rate_flag = N.  
    -- For a normal bit rate - SAR sensor, the realtime 
    -- sensing and downlinking is simultaneious and not 
    -- compressed.  
    */
    if ( dtkm_is_a_realtime_downlink( result_dtk ) == TRUE )
    {
        return_code = dtkm_update_obs_times_from_rdl( APS_dbproc, 
            result_dtk, dtk_updates ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
    }

    /* now handle the rest of the lists:  */

    /* THE DATA-TAKE PROPOSAL WAS ACCEPTED (CONTINUED).  */

    if ( NUMELTS( dtk_parallels ) > 0 
    &&   dtkm_is_a_downlink( result_dtk )  == TRUE )
    {
        /*
        -- STEP 9.3  
        --  If there are one or more parallel data-takes, change each of
        --  their antennas to match that of the dtk proposal.
        --  note the prior removal of observations from this 
        --  list when the dtk_proposal was cleared of conflicts.  
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 9.3 update antenna for each parallel dtk\n", 
            __FILE__, __LINE__ ) ;
#endif
        return_code = dtkm_update_dtks_field( APS_dbproc, dtk_parallels, 
            DTK_ANTENNA_ID,
            &(CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID]), dtk_updates ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
    }

    if ( NUMELTS( dtk_same_pass ) > 0 
    &&   dtkm_is_a_downlink( result_dtk )  == TRUE )
    {
        /*
        -- STEP 9.4  
        -- If there are one or more same-pass data-takes, change each of
        -- their antennas to match that of the dtk proposal.
        -- this list is defined as a downlink list.  
        */
#ifdef PRINT_DIAG
        printf("%s(%d):  STEP 9.4 update antenna for each same-pass dtk\n", 
            __FILE__, __LINE__ ) ;
#endif
        return_code = dtkm_update_dtks_field( APS_dbproc, dtk_same_pass, 
            DTK_ANTENNA_ID,
            &(CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID]), dtk_updates ) ;
        if ( return_code < 0 )
        {
            free_db_record( changed_dtk ) ;
            return return_code ;
        }
    }

    /*
    -- STEP 9.5
    -- Process starts again with next dtk proposal, if any, in the list.
    */
#ifdef PRINT_DIAG
    printf("%s(%d):  STEP 9.5 \n", __FILE__, __LINE__ ) ;
#endif
    free_db_record( changed_dtk ) ;

    if (inheritance_flag)
        return DTKM_DTK_ACCEPTED_INHERITANCE ;
    else
        return DTKM_DTK_ACCEPTED ;
}
