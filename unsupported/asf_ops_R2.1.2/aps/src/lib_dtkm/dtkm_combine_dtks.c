#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif
   
/*==============================================================================
Filename:   dtkm_combine_dtks.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_combine_dtks.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_combine_dtks.c"

#include "dtkm.h"
#include <db_dtk.h>
#include <db_dl2obs.h>
#include <timeconv.h>

#include <string.h>     /* for strcpy strlen strncat strcmp    */


/*==============================================================================
Function:       dtkm_combine_darid_values()
Description:    set the dtk.darid value from 
                the values of the dtks.  look for a nonzero value.  
Creator:        Lawrence Stevens
Creation Date:  Wed Jan 22 15:13:16 PST 1997
==============================================================================*/
static int dtkm_combine_darid_values( 
    DB_RECORD   **dtk_proposal, 
    llist       *dtk_concurs, 
    llist       *dtk_similars, 
    DB_RECORD   **combine_dtk )
{
    DB_RECORD   **dtk_rec = NULL ;
    cursor      dtk_list_ptr ;
    llist       *dtk_list ;

    int         j ;

    /* 
    -- Find the first nonzero darid and put it 
    -- into combine_dtk.  
    */
    if ( CAST_DTK_DARID combine_dtk[DTK_DARID] == 0 
    &&   CAST_DTK_DARID dtk_proposal[DTK_DARID] != 0 )
    {
        /* 
        -- the combine_dtk darid == 0 and  
        -- the dtk_proposal darid != 0.  
        -- set the combine_dtk darid value and then 
        -- return.  
        */
        CAST_DTK_DARID  combine_dtk[DTK_DARID] 
                = CAST_DTK_DARID dtk_proposal[DTK_DARID] ;
         /* done.  */
         return TRUE ;
    }

    for ( j = 0 ; j < 2 ; j++ )
    {
        /*
        -- process first the dtk_concurs and then 
        -- the dtk_similars list:
        */
        if ( j == 0 )
            dtk_list = dtk_concurs ;
        else if ( j == 1 )
            dtk_list = dtk_similars ;
        else 
            dtk_list = NULL ;

        for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
                dtk_rec ; 
                dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
            )
        {
            /* process the current dtk_rec right here.  */
            if ( CAST_DTK_DARID combine_dtk[DTK_DARID] == 0 
            &&   CAST_DTK_DARID dtk_rec[DTK_DARID]     != 0 )
            {
                /* 
                -- the combine_dtk darid == 0 and  
                -- the dtk_rec darid != 0.  
                -- set the combine_dtk darid value and then 
                -- return.  
                */
                 CAST_DTK_DARID  combine_dtk[DTK_DARID] 
                        = CAST_DTK_DARID dtk_rec[DTK_DARID] ;
                /* done.  */
                return TRUE ;
            }
        }
    }
    return TRUE ;
}


/*==============================================================================
Function:       dtkm_combine_quicklook_values()

Description:    set the dtk.science_quicklook and the 
                dtk.planner_quicklook values from a logical 
                OR from all of the values:  

Creator:        Lawrence Stevens

Creation Date:  Sun Dec 10 15:42:41 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_combine_quicklook_values( 
    DB_RECORD   **dtk_proposal, 
    llist       *dtk_concurs, 
    llist       *dtk_similars, 
    DB_RECORD   **combine_dtk )
{
    DB_RECORD   **dtk_rec = NULL ;
    cursor      dtk_list_ptr ;
    llist       *dtk_list ;

    int         j ;

    /* 
    -- logical OR with dtk_proposal.  if either is 
    -- Y change to Y.  
    */
    if ( CAST_DTK_SCIENCE_QUICKLOOK dtk_proposal[DTK_SCIENCE_QUICKLOOK] == 'Y' )
         CAST_DTK_SCIENCE_QUICKLOOK  combine_dtk[DTK_SCIENCE_QUICKLOOK] =  'Y' ;
    if ( CAST_DTK_PLANNER_QUICKLOOK dtk_proposal[DTK_PLANNER_QUICKLOOK] == 'Y' )
         CAST_DTK_PLANNER_QUICKLOOK  combine_dtk[DTK_PLANNER_QUICKLOOK] =  'Y' ;

    for ( j = 0 ; j < 2 ; j++ )
    {
        /*
        -- process first the dtk_concurs and then 
        -- the dtk_similars list:
        */
        if ( j == 0 )
            dtk_list = dtk_concurs ;
        else if ( j == 1 )
            dtk_list = dtk_similars ;
        else 
            dtk_list = NULL ;

        for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
                dtk_rec ; dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
            )
        {
            /* process the current dtk_rec right here.  */
            if ( CAST_DTK_SCIENCE_QUICKLOOK dtk_rec[DTK_SCIENCE_QUICKLOOK] 
                    == 'Y' )
                 CAST_DTK_SCIENCE_QUICKLOOK  combine_dtk[DTK_SCIENCE_QUICKLOOK] 
                    =  'Y' ;
            if ( CAST_DTK_PLANNER_QUICKLOOK dtk_rec[DTK_PLANNER_QUICKLOOK] 
                    == 'Y' )
                 CAST_DTK_PLANNER_QUICKLOOK  combine_dtk[DTK_PLANNER_QUICKLOOK] 
                    =  'Y' ;
        }
    }
    return TRUE ;
}


/*==============================================================================
Function:       dtkm_combine_submit_time_values()

Description:    set the dtk.submit_time in the combine_dtk as the 
                earliest of all of the values.  

Creator:        Lawrence Stevens

Creation Date:  Sun Dec 10 15:43:15 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_combine_submit_time_values( 
    DB_RECORD   **dtk_proposal, 
    llist       *dtk_concurs, 
    llist       *dtk_similars, 
    DB_RECORD   **combine_dtk )
{
    DB_RECORD   **dtk_rec = NULL ;
    cursor      dtk_list_ptr ;
    llist       *dtk_list ;

    int         j ;

    /* start with the current time.  */
    tc_systime2asf(CAST_DTK_SUBMIT_TIME combine_dtk[DTK_SUBMIT_TIME]) ;

    /* 
    -- compare with dtk_proposal; update combine_dtk with the 
    -- sooner time.  
    */
    if ( strcmp(CAST_DTK_SUBMIT_TIME combine_dtk[DTK_SUBMIT_TIME],
                CAST_DTK_SUBMIT_TIME dtk_proposal[DTK_SUBMIT_TIME] )
        > 0 )
    {
        /* new earliest time, if the time was valid.  */
        if ( tc_validate_asf_datetime(
                CAST_DTK_SUBMIT_TIME dtk_proposal[DTK_SUBMIT_TIME]) == TRUE )
            strcpy(CAST_DTK_SUBMIT_TIME combine_dtk[DTK_SUBMIT_TIME],
                CAST_DTK_SUBMIT_TIME dtk_proposal[DTK_SUBMIT_TIME] ) ;
    }

    for ( j = 0 ; j < 2 ; j++ )
    {
        /*
        -- process first the dtk_concurs and then 
        -- the dtk_similars list:
        */
        if ( j == 0 )
            dtk_list = dtk_concurs ;
        else if ( j == 1 )
            dtk_list = dtk_similars ;
        else 
            dtk_list = NULL ;

        for (   dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr);
                dtk_rec ;
                dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr)  
            )
        {
            /* process the current dtk_rec right here.  */
            /* 
            -- compare with combine_dtk ; update combine_dtk with the 
            -- sooner time if valid.  
            */
            if ( strcmp(CAST_DTK_SUBMIT_TIME combine_dtk[DTK_SUBMIT_TIME],
                        CAST_DTK_SUBMIT_TIME dtk_rec[DTK_SUBMIT_TIME] )
                > 0 )
            {
                /* new earliest time.  */
                if ( tc_validate_asf_datetime( 
                        CAST_DTK_SUBMIT_TIME dtk_rec[DTK_SUBMIT_TIME]) == TRUE)
                    strcpy(CAST_DTK_SUBMIT_TIME combine_dtk[DTK_SUBMIT_TIME],
                            CAST_DTK_SUBMIT_TIME dtk_rec[DTK_SUBMIT_TIME] ) ;
            }
        }
    }
    return TRUE ;
}

/*==============================================================================
Function:       dtkm_combine_dtk_record()

Description:    combine one data-take into another data-take.  
                this means change the status of a data-take to 
                CMB, mark it in dtk.notes that it is combined into 
                the other data-take.  
                Then update the data-take in the database.  
                Also, find its entry in the dl2obs relation and 
                delete it.  That is, remove the link so that there 
                cannot be a dtk.dtkstat flow from a downlink to this 
                observation.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 14 17:50:37 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
static int dtkm_combine_dtk_record(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_rec,          /* this record stays in the schedule.   */
    DB_RECORD   **dtk_2b_combined,  /* update THIS record to CMB in the db  */
    DB_RECORD   **result_CMB_dtk,   /* contains record values after changes */
    llist       *dtk_updates )   /* add the update to this list.  */
{
    int     return_code ;
    int     nrecs ;
    int     length = 0 ;
    char    comment[] = " CMB->dd" ;

    /* quick error checking.  */
    if ( dtk_rec == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    if ( dtk_2b_combined == NULL )
        return DTKM_ERROR_NULL_DTK_2B_COMBINED ;

    if ( result_CMB_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    db_copy_record( APS_CDEFS(DTK), result_CMB_dtk, dtk_2b_combined ) ;

    /* set status.  */
    strcpy(CAST_DTK_DTKSTAT result_CMB_dtk[DTK_DTKSTAT], "CMB" ) ;

    /*
    -- if the dtk_rec and the dtk_2b_combined have the same 
    -- dtkid, then skip.  Looks like this combine is a special 
    -- case where a single record is updated.  
    */
    if (CAST_DTK_DTKID dtk_rec[DTK_DTKID] == 
        CAST_DTK_DTKID dtk_2b_combined[DTK_DTKID] )
        return TRUE ;

    /* 
    -- we want to concatenate the comment in this routine to 
    -- the dtk.notes field without overrunning the storage.  
    -- therefore we use APS_SIZE to check.  
    -- use length to be the number of bytes we can safely 
    -- concatenate into that field, never more than the 
    -- length of the comment.  
    */
    length = MIN(   
        ( (int) APS_SIZE(DTK, DTK_NOTES) 
              -  (int) strlen( CAST_DTK_NOTES result_CMB_dtk[DTK_NOTES] ) ), 
          (int) strlen(comment) ) ;

    /* 
    -- put the dtkid into the comment.  
    -- the concatenated comment will look like this:  " CMB->03"
    */
    sprintf( comment+6, "%2.2d", CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;

    /* 
    -- concatenate the comment onto the dtk.notes field, as much 
    -- as is safe.  
    */
    strncat( (CAST_DTK_NOTES result_CMB_dtk[DTK_NOTES]), comment, length ) ;

    /*********************************************************************
    *                                                                    *
    *    Create or update the dl2obs entry for the dtk that remains      *
    *    from the several dtks that are getting a CMB status.            *
    *                                                                    *
    *********************************************************************/

    /*
    -- the data-take result_CMB_dtk is being combined into the 
    -- the data-take dtk_rec.  
    -- therefore, whatever old links between observation and downlink 
    -- that existed to or from result_CMB_dtk must now be 
    -- deleted and re-created, if necessary, for the 
    -- data-take dtk_rec, which will stay in the schedule.
    */

    /*
    -- first, make sure that the surviving data-take, dtk_rec, 
    -- has the downlink-observation links in the dl2obs relation 
    -- that are now possessed by result_CMB_dtk.  
    -- Then, delete the old downlink-observation links from the dl2obs 
    -- relation possed by result_CMB_dtk.
    */

    if( dtkm_is_a_downlink( result_CMB_dtk ) == TRUE )
    {
        /* 
        -- update the dtkid of any possible entries 
        -- that exist for the dtkid of result_CMB_dtk.  
        -- update this dtkid to the values of dtk_rec.  
        */
        sprintf( fields_to_set, "%s = %d", 
            APS_COL(DL2OBS, DL2OBS_DTKID_DL), 
                CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;
        sprintf( where_clause, 
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT), 
                CAST_DTK_SAT            dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_DL), 
                CAST_DTK_REV            dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_DL), 
                CAST_DTK_DTKID          result_CMB_dtk[DTK_DTKID] ) ;
        nrecs = db_update_records( APS_dbproc, APS_TABLE( DL2OBS ),
            fields_to_set, where_clause ) ;
        if( nrecs < 0 )
            return DTKM_ERROR_DB_UPDATE_FAILED_ON_DL2OBS_RELATION ;
    }
    if( dtkm_is_an_observation( result_CMB_dtk ) == TRUE )
    {
        /* 
        -- need to remove any dl2obs references to the CMB data-take.  
        -- also need to put in a dl2obs entry for the surviving data-take.  
        --
        -- if possible, use the info from the entry to be deleted.  
        --
        -- update the dtkid of any possible entries 
        -- that exist for the dtkid with result_CMB_dtk.  
        -- update these values to match the values of dtk_rec.  
        -- (in this case, change the dtkid of the found entries.  )
        -- will find either 1 or 0 entries.  
        */
        /* 
        -- first check to see if a dl2obs record with our intended 
        -- unique primary key ALREADY exists.  
        -- that is, see if there is ALREADY an entry for the 
        -- observation dtk_rec.  
        -- If there is already an entry, just DELETE the CMB dtk dl2obs rec.  
        -- If there is no entry,         just UPDATE the CMB dtk dl2obs rec.  
        */
        /* where_clause identifies the expected new entry.    */
        sprintf( where_clause, 
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT), 
                                            CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_OBS), 
                                            CAST_DTK_REV dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_OBS), 
                                            CAST_DTK_DTKID dtk_rec[DTK_DTKID]);
        nrecs = db_num_records( APS_dbproc, APS_TABLE(DL2OBS), where_clause ) ;
        if( nrecs < 0 )
            return DTKM_ERROR_DB_QUERY_FAILED ;
        /* dtkm_is_an_observation( result_CMB_dtk ) == TRUE */
        if( nrecs == 0 )
        {
            /* 
            -- a dl2obs record with our intended primary unique key 
            -- does not already exist.  
            -- update the CMB entries to refer to dtk_rec instead:  
            */
            return_code = dtkm_update_dl2obs_4dtk( APS_dbproc, 
                result_CMB_dtk, dtk_rec ) ;
            if( return_code < 0 )
                return return_code ;
        }
        else
        {
            /* dtkm_is_an_observation( result_CMB_dtk ) == TRUE */
            /* 
            -- a dl2obs record with our intended primary unique key 
            -- does already exist.  
            -- need only to DELETE the CMB entry.  
            */
            return_code = dtkm_delete_dl2obs_recs( APS_dbproc, result_CMB_dtk,
                dtk_updates ) ;
            if( return_code < 0 )
                return return_code ;
        }
    }

    /* 
    -- finally, lastly, update this record in the database.  it 
    -- might already exist but it might not.  
    -- dtkm_update_dtk() will figure out whether to 
    -- insert or update.  
    -- the result data-take is duplicated into the dtk_updates list.  
    */
    return_code = dtkm_update_dtk( APS_dbproc, result_CMB_dtk, result_CMB_dtk, 
        dtk_updates ) ;
    if ( return_code < 0 )
        return return_code ;

    return TRUE ;
}

/*==============================================================================
Function:       dtkm_combine_dtks()

Description:    according to various rules, combines data-takes for the 
                purpose of making the schedule like the FA schedule and 
                providing tracking of dar-based dtks into the FA scheduled 
                data-takes.  

Returns:        

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 14 11:15:26 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/

int dtkm_combine_dtks(
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **dtk_proposal,
    llist       *dtk_concurs,    /* data-takes that match the proposal       */
    llist       *dtk_similars,   /* list of data-takes that the proposed     */
                                 /* data-take is similar to                  */
    DB_RECORD   **result_dtk,   /* the resulting dtk_proposal  */
    llist       *dtk_updates )  /* this routine adds the changed dtks to list */
{
    DB_RECORD   **new_dtk_rec = NULL ;
    DB_RECORD   **combine_dtk_rec = NULL ;
    DB_RECORD   **dtk_similars_rec = NULL ;
    cursor      dtk_similars_ptr ;
    int         return_code ;
    int         previous_dtk_darid ;
    int         dtk_darid_update_code ;

    /* quick error checking.  */
    if ( dtk_proposal == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if ( strcmp( CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "SCH" ) != 0 
    &&   strcmp( CAST_DTK_DTKSTAT dtk_proposal[DTK_DTKSTAT], "PLN" ) != 0 )
        return DTKM_ERROR_STATUS_IS_NEITHER_SCH_NOR_PLN_WHEN_COMBINING_DTKS ;

    if ( dtk_concurs == NULL )
        return DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED ;
    /* it is OK for the dtk_concurs list to have 0 or 1 members.  */
    if ( NUMELTS( dtk_concurs ) != 0  &&  NUMELTS( dtk_concurs ) != 1 )
        return DTKM_ERROR_DTK_CONCURS_LIST_HAS_NEITHER_0_NOR_1_MEMBERS ;

    if ( dtk_similars == NULL )
        return DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED ;

    if ( NUMELTS( dtk_similars ) <= 0 )
        return DTKM_ERROR_SIMILARS_LIST_IS_EMPTY ;

    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;

    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    /* the dtk_updates list can have members in it at this point.  */

    /* set up the output record in case it is changed.  */
    db_copy_record(APS_CDEFS(DTK), result_dtk, dtk_proposal ) ;

    /*
    --      o   combine the dtk proposal, the similars, and the concurring
    --          dtk, if any, into a new data-take using the time bracket and
    --          antenna from the dtk proposal.  
    --      o   ALSO:  the earliest submit times 
    */

    /* 
    -- create the combine_dtk_rec record; we do need to 
    -- free_db_record() the combine_dtk_rec record 
    -- later in this routine.  
    -- put the first similar data-take into this record.  
    */
    combine_dtk_rec = new_table_record( APS_CDEFS(DTK) ) ;
    dtk_similars_rec = (DB_RECORD **) FIRST(dtk_similars, dtk_similars_ptr) ;
    if ( dtk_similars_rec == NULL )
    {
        free_db_record( combine_dtk_rec ) ;
        return DTKM_ERROR_SIMILARS_LIST_IS_EMPTY ;
    }
    return_code = db_copy_record ( APS_CDEFS(DTK), combine_dtk_rec, 
        dtk_similars_rec ) ;
    if ( return_code < 0 )
    {
        free_db_record( combine_dtk_rec ) ;
        return return_code ;
    }
    /* 
    -- if we are combining more than 1 dtk, then make 
    -- a brand-new record.  if we are "combining" a 
    -- a single record, use the previous data-take id.
    */
    if ( (NUMELTS(dtk_concurs) + NUMELTS(dtk_similars)) > 1 )
    {
        /* 
        -- make this combined data-take to be new
        */
        CAST_DTK_DTKID combine_dtk_rec[DTK_DTKID] = 0 ;
        strcpy( CAST_DTK_FADTKID combine_dtk_rec[DTK_FADTKID], "" ) ;
    }

    /* 
    -- the DB_RECORD was created with data from one dtk.  must 
    -- now update it with the appropriate info from the 
    -- dtk_proposal.  
    */
    /* 
    -- set the status, the antenna, the times, and the 
    -- fa_schedule_link from the dtk proposal.  
    */
    strcpy( CAST_DTK_DTKSTAT combine_dtk_rec[DTK_DTKSTAT], 
            CAST_DTK_DTKSTAT    dtk_proposal[DTK_DTKSTAT] ) ;
    strcpy( CAST_DTK_STRTTIME combine_dtk_rec[DTK_STRTTIME], 
            CAST_DTK_STRTTIME dtk_proposal[DTK_STRTTIME] ) ;
    strcpy( CAST_DTK_STOPTIME combine_dtk_rec[DTK_STOPTIME], 
            CAST_DTK_STOPTIME dtk_proposal[DTK_STOPTIME] ) ;
    strcpy( CAST_DTK_FA_SCHEDULE_LINK combine_dtk_rec[DTK_FA_SCHEDULE_LINK], 
            CAST_DTK_FA_SCHEDULE_LINK dtk_proposal[DTK_FA_SCHEDULE_LINK] ) ;
    CAST_DTK_ANTENNA_ID combine_dtk_rec[DTK_ANTENNA_ID] 
        = CAST_DTK_ANTENNA_ID dtk_proposal[DTK_ANTENNA_ID] ;

    /*
    -- update the "combine_dtk_rec" darid value with information from database.
    -- The darid value is dependent on the satellite and start/stop times.
    -- It does not depend on sensor mode, for instance.
    -- It does depend on which observation dtk in the database has the greater
    -- overlap with the "combine_dtk_rec"
    --
    -- If the dtk_proposal has an initial non-zero darid value, let's keep it.
    -- (We don't want to overwrite a value that the user has entered)
    --
    -- NOTE: This step no longer uses dtkm_combine_darid_values() which used
    -- to populate a zero-valued darid with the first non-zero value it could
    -- find in the list of dtk_similars or dtk_concurs.
    */
    if (CAST_DTK_DARID dtk_proposal[DTK_DARID] == 0 )
    {
        /*
        -- There is no initial value for darid in "dtk_proposal"
        -- so use the "combine_dtk_rec" darid value 
        -- (zero or non-zero does not matter, as the value will be updated).
        --
        --  1) save the current "combine_dtk_rec" darid value
        --  2) set the "combine_dtk_rec" darid value to 0, 
        --     to trigger the dtkm_update_dtk_darid()
        */
        previous_dtk_darid = CAST_DTK_DARID combine_dtk_rec[DTK_DARID] ;
        CAST_DTK_DARID combine_dtk_rec[DTK_DARID] = 0 ;

        dtk_darid_update_code =
            dtkm_update_dtk_darid(combine_dtk_rec) ;
        if (dtk_darid_update_code < 0)
            return (dtk_darid_update_code) ;

        /* see if the darid value has changed */
        if (CAST_DTK_DARID combine_dtk_rec[DTK_DARID] == previous_dtk_darid)
            dtk_darid_update_code = FALSE ;
        else
            dtk_darid_update_code = TRUE ;
    }
    else
    {
        CAST_DTK_DARID combine_dtk_rec[DTK_DARID] = 
            CAST_DTK_DARID dtk_proposal[DTK_DARID] ;
        dtk_darid_update_code = FALSE ;
    }
        



    /* 
    -- ALSO:  set the dtk.science_quicklook and the 
    --  dtk.planner_quicklook values from a logical 
    -- OR from all of the values:  
    */
    return_code = dtkm_combine_quicklook_values( dtk_proposal, dtk_concurs, 
        dtk_similars, combine_dtk_rec ) ;

    /* 
    -- ALSO:  set the dtk.submit_time as the 
    -- EARLIEST of all of the values.  
    */
    return_code = dtkm_combine_submit_time_values( dtk_proposal, dtk_concurs, 
        dtk_similars, combine_dtk_rec ) ;

    /* 
    -- now update the old record or possibly insert the new record 
    -- into the database with this data.  
    */
    return_code = dtkm_update_dtk(APS_dbproc, combine_dtk_rec, 
        combine_dtk_rec, dtk_updates ) ;
    if ( return_code < 0 )
    {
        free_db_record( combine_dtk_rec ) ;
        return return_code ;
    }

    /* 
    -- now make a new DB_RECORD to receive updated values.  
    -- this is just to satisfy calling parameters for the next 
    -- few routines.  
    */
    new_dtk_rec = new_table_record( APS_CDEFS(DTK) ) ;

    /* 
    -- now combine the dtk_proposal if it is a modification, 
    -- into the combine_dtk_rec.  
    -- don't combine if the dtk_proposal has no dtkid; dtkid = 0.  
    -- we don't need to input a new dtk for this dtk proposal since 
    -- there is a combined data-take for this purpose.  
    */
    if ( CAST_DTK_DTKID result_dtk[DTK_DTKID] != 0 )
    {
        /* update this record; */
        return_code = dtkm_combine_dtk_record( APS_dbproc, 
            combine_dtk_rec, result_dtk, new_dtk_rec, dtk_updates ) ;
        if ( return_code < 0 )
        {
            free_db_record( combine_dtk_rec ) ;
            free_db_record( new_dtk_rec ) ;
            return return_code ;
        }
    }

    /* 
    -- no need to combine a dtk_concur record; the dtk_proposal has 
    -- already been updated.  If there is a dtk_concur, by definition, 
    -- it has the same DTK_DTKID.  
    */

    /* 
    -- now combine each record in the dtk_similars list into the 
    -- combine_dtk_rec.  
    */
    for (
        dtk_similars_rec = (DB_RECORD **) FIRST(dtk_similars, dtk_similars_ptr);
        dtk_similars_rec ;
        dtk_similars_rec = (DB_RECORD **) NEXT(dtk_similars, dtk_similars_ptr)
        )
    {
        /* 
        -- combine the dtk_similars_rec into the combine_dtk_rec; 
        */
        return_code = dtkm_combine_dtk_record( APS_dbproc, 
            combine_dtk_rec, dtk_similars_rec, new_dtk_rec, dtk_updates ) ;
        if ( return_code < 0 )
        {
            free_db_record( combine_dtk_rec ) ;
            free_db_record( new_dtk_rec ) ;
            return return_code ;
        }
    }

    /* 
    -- the resulting values for the dtk proposal are 
    -- that in the combine_dtk_rec.  Those values are to 
    -- be returned.  
    */
    db_copy_record( APS_CDEFS(DTK), result_dtk, combine_dtk_rec ) ;

    free_db_record( new_dtk_rec ) ;
    free_db_record( combine_dtk_rec ) ;
    return TRUE ;

}
