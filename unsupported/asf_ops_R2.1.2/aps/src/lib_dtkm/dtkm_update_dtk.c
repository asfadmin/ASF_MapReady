#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_update_dtk.c

    
Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dtk.c	5.2 98/03/12 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dtk.c"



/*==============================================================================
Function:       dtkm_update_dtk

Description:    updates the dtk relation with the input DB_RECORD.  
                this routine will figure out wether or not it is an 
                update or insert and handle either case.  
                an update means that there is an existing rec to modify.  

                In addition, it handles the flowing of dtkstat and 
                quicklook values between observation and downlink data-takes 
                that are linked via dtk.fa_schedule_link and/or the
                dl2obs relation.  

Creator:        Lawrence Stevens

Creation Date:  Tue Oct 31 18:00:41 PST 1995

Notes:      
    if the record is new, it is created via dtkm_insert_dtk().  
    if the record has a dtkid or fadtkid field value, it is probable 
    that an existing dtk record (dtkid) will be identified.  


HELPFUL NOTES ON THE CODE STRATEGY/CONCEPTS IN THIS FILE:  

Inserting:
    Possibly, there will be an insert on dl2obs, too, to link observation 
    and downlink toghether.  dtk.fa_schedule_link is used to link them.  
    That is, a recording looks for a matching tape dump, later in time, 
    with the same value of dtk.fa_schedule_link.  A tape dump, however, 
    looks for recordings previous in time.  

Updating:
    Normally, there will be an existing dl2obs record already.  
    For downlinks, we use dtk.fa_schedule_link to generate one list 
    of linked observations, and use dl2obs to generate another list 
    for comparision.  The dtk.fa_schedule_link list is considered 
    to be correct, which provides the ability to correct the dl2obs 
    relation if the lists differ.  

Input is a downlink:
    Need to look at observation science quicklook values to determine the 
    correct science quicklook value.  
    Need to look at observation planner quicklook values and change them if 
    they disagree with the downlink value.  
    Need to look at linked observation status (dtk.dtkstat) values and change 
    them if they disagree.  

Strategy in code:
    Due to the differences in processing between observation/downlink and 
    insert/update, one might be tempted to make 4 routines to eliminate 
    some "if" statements.  However, this might result in a lot of duplicated 
    code.  In addition, they all have the same basic steps to follow, which 
    are noted and repeated in the comments below.  

Strategy in lists, DB_RECORDs:    [ This code has been tested with purify ]
    They are initialized to NULL, and then used as flags to see if they 
    are in use.  That is, there may or may not be any linked observations 
    to a downlink.  If there are linked obs, the list pointer is not 
    NULL.  On the other hand, if there are no linked obs found, the 
    pointer is kept to NULL.  In this case, fa_schedule_link_obs_list 
    and/or dl2obs_obs_list may be NULL, and you will see "if" statements 
    based on this:

    if( fa_schedule_link_obs_list )
    {
        /. process the list of observations found for this downlink.  ./
        ...
    }

    For this reason, when you see a DEL_LIST( fa_schedule_link_obs_list ) 
    to free the memory, you will either see an IMMEDIATE return 
    to the calling program, or you will see the pointer set to NULL:
    if the program continues processing:  

    You will see this:

            /. ... error handling  ...   ./
            DEL_LIST( fa_schedule_link_obs_list )
            return return_code ;
        }

    or:
            DEL_LIST( fa_schedule_link_obs_list )
            fa_schedule_link_obs_list = NULL ;
            /.  ... more processing.     ./
        }

    At the end of processing, you will see code like this:  

        if( dl2obs_obs_list ) 
            DEL_LIST( dl2obs_obs_list ) ;

    Here, the code is using dl2obs_obs_list as a flag, and if 
    it == NULL, the DEL_LIST() is NOT executed.  If DEL_LIST() executes 
    with a NULL argument, it core dumps.  

    You will see the same strategy used with DB_RECORDs:

        if( return_code < 0 )
        {
            free_db_record( fa_schedule_link_dl_dtk_rec ) ;
            return return_code ;
        }

        ...

        free_db_record( fa_schedule_link_dl_dtk_rec ) ;
        fa_schedule_link_dl_dtk_rec = NULL ;

        ...

        if( fa_schedule_link_dl_dtk_rec )
            free_db_record( fa_schedule_link_dl_dtk_rec ) ;


==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"       /* for CAST_DTK_REV, etc.          */
#include "db_dl2obs.h"    /* for CAST_DL2OBS_REV_OBS  etc.   */
#include "timeconv.h"    /* for tc_systime2asf()    etc.   */

#include <string.h>     /*  for strlen()    */

int dtkm_update_dtk( 
    DBPROCESS   *APS_dbproc, 
    DB_RECORD   **input_dtk,    /* DB_RECORD to update dtk relation */
    DB_RECORD   **result_dtk,   /* resulting updated data-take      */
    llist       *dtk_updates )  /* update duplicated into list      */
{
 
    int             nrecs ;
    int             return_code ;

    int             downlink_flag ;         /* TRUE if input dtk is a DL */
    int             insert_flag ;           /* TRUE if input dtk is an insert */

    int             obs_dtk_in_list_flag ;  /* TRUE if input obs dtk is found 
                                               in the dtk list being searched */
 
    llist           *list_check = NULL ;

    /* dtk relation lists/recs/ptrs   */
    llist           *existing_dtk_list = NULL ;
    cursor          existing_dtk_list_ptr ;
    DB_RECORD       **existing_dtk = NULL ; /* existing data-take    */
 
    cursor          dtk_list_ptr ;
    llist           *fa_schedule_link_obs_list = NULL ;
    llist           *dl2obs_obs_list = NULL ;

    cursor          obs_dtk_list_ptr ;
    llist           *obs_dtk_list = NULL ;
    DB_RECORD       **obs_dtk_rec = NULL ;

    DB_RECORD       **fa_schedule_link_dl_dtk_rec = NULL ;
    DB_RECORD       **dl2obs_dl_dtk_rec = NULL ;
    DB_RECORD       **linked_dl_dtk_rec = NULL ;

    char            obs_planner_quicklook = '0' ;  /* init to non-(Y/N)   */
    char            obs_science_quicklook = '0' ;  /* init to non-(Y/N)   */
    char            dl_dtkstat[4] = "000" ;

    char            now_asftime[ASF_TIME_STR_LENGTH+1] ;

    /* error checking  */
    if ( input_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;
 
    /* 
    -- check for dtkid > 0:  it must indicate 
    -- an existing dtk.  
    */
    if ( CAST_DTK_DTKID input_dtk[DTK_DTKID] > 0 )
    {
        /* the input record has a > 0 DTK_DTKID in it.  */
        /* 
        -- check to see if it exists.  
        -- if not, this is an error.  
        */
        sprintf(where_clause, 
            "where %s = '%s' and %s = %ld and %s = %d", 
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT input_dtk[DTK_SAT],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV input_dtk[DTK_REV],
            APS_COL(DTK,DTK_DTKID), (int) CAST_DTK_DTKID input_dtk[DTK_DTKID]);

        nrecs = db_num_records(APS_dbproc, APS_TABLE(DTK), 
            where_clause ) ;
        if ( nrecs < 0 )
            return DTKM_ERROR_DB_QUERY_FAILED ;

        if ( nrecs == 0 )
        {
            /* dtk does not exist.  */
            return DTKM_ERROR_ATTEMPT_TO_UPDATE_NONEXISTING_DTK ;
        }
    }

    /*
    -- set up the result data-take;
    */
    (void) db_copy_record(APS_CDEFS(DTK), result_dtk, input_dtk ) ;

    /* 
    --if input dtkid = 0, then try to use fadtkid, if it 
    -- has a value, to obtain the dtkid of the dtk rec 
    -- that it indicates.  
    */
    if ( CAST_DTK_DTKID result_dtk[DTK_DTKID] == 0 
    &&   (int) strlen(CAST_DTK_FADTKID result_dtk[DTK_FADTKID]) > 0 )
    {
        /*
        -- DTK_DTKID == 0  but there is something in FADTKID 
        --                 which might identify the existing dtk in db.  
        --
        -- we expect to identify an existing record in the 
        -- dtk relation using the fadtkid field.  
        -- we try find out its dtkid and complete the primary key,
        -- by doing a retrieve.  
        */
        sprintf(where_clause, 
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = '%s'", 
            APS_COL(DTK, DTK_SAT), CAST_DTK_SAT result_dtk[DTK_SAT],
            APS_COL(DTK, DTK_SENSOR), CAST_DTK_SENSOR result_dtk[DTK_SENSOR],
            APS_COL(DTK, DTK_REV), CAST_DTK_REV result_dtk[DTK_REV],
            APS_COL(DTK,DTK_FADTKID), 
                                CAST_DTK_FADTKID result_dtk[DTK_FADTKID]);

        /* must remember to clean up existing_dtk_list later.  */
        existing_dtk_list = db_get_records(APS_dbproc, APS_TABLE(DTK),
            where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
        if (existing_dtk_list == NULL)
            return DTKM_ERROR_DB_QUERY_FAILED ;

        nrecs = NUMELTS( existing_dtk_list ) ;
        if ( nrecs == 1 )
        {
            /* 
            -- now we can obtain the correct dtkid to use.   
            -- copy the dtkid to complete the primary key of 
            -- the record we are updating.  
            */
            existing_dtk = 
                (DB_RECORD **) FIRST( existing_dtk_list, existing_dtk_list_ptr);
            CAST_DTK_DTKID result_dtk[DTK_DTKID] = 
                CAST_DTK_DTKID existing_dtk[DTK_DTKID] ; 

            /*
            -- Special case.  If this is a REJ/DEL data-take, 
            -- and if the dtk proposal has an antenna id of 0, 
            -- see if there is a non-zero dtkid in the existing_dtk.  
            -- If so, then this is a special case.  The existing non-zero 
            -- dtkid value must be used, after dtkm_update_dtk() returns, 
            -- to find downlinks that could have previously conflicted 
            -- with this soon-to-be REJ/DEL data-take.  
            -- These previously conflicted downlinks (PCD's),
            -- have a chance to be brought back into the schedule, 
            -- or placed on a more favorable antenna.  
            */
            if( 
            (
                strcmp("DEL", CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) == 0 
                || strcmp("REJ", CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) == 0
            )
            &&  (int) CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID]    == 0
            &&  (int) CAST_DTK_ANTENNA_ID existing_dtk[DTK_ANTENNA_ID]  != 0  )
            {
                /* 
                -- special case.  this ia a REJ/DEL result_dtk, a downlink, 
                -- and we should copy the existing non-zero antenna id.  
                */
                CAST_DTK_ANTENNA_ID result_dtk[DTK_ANTENNA_ID]
                    = CAST_DTK_ANTENNA_ID existing_dtk[DTK_ANTENNA_ID] ;
            }
        }
        else if ( nrecs == 0 )
        {
            /* 
            -- there is no record that matches this record; just 
            -- insert this new record later.  
            -- at the moment, just do nothing:  
            */
            /* add harmless null statement to remove lint warning.  */
            return_code = 0 ;
        }
        else
        {
            /* 
            -- there is more than one record that matches this record; 
            -- we cannot know the corredt dtkid.  
            -- perhaps the planner put some new characters in this field.  
            -- do nothing right now.  
            */
            /* add harmless null statement to remove lint warning.  */
            return_code = 0 ;
        }
        /* clean up everything here.  */
        DEL_LIST( existing_dtk_list ) ;
        existing_dtk_list = NULL ;
    }

    /*
    -- dtkm_update_dtk.c,  the current routine, maintains the 
    -- observation-downlink links via the dl2obs relation.  It 
    -- also maintains the flow of the dtk.dtkstat value from downlink 
    -- to observation, and it also maintains the flow 
    -- of the quicklook values.  
    -- 
    -- That is a tall order, and under different conditions, 
    -- as you will see, we must do different things.  
    -- what to do is based on whether or not the dtk 
    -- is an observation/downlink or an insert/update.  
    -- so set up flags for use in this routine:
    */
    if( dtkm_is_a_downlink( result_dtk ) == TRUE )
        downlink_flag = TRUE ;
    else
        if( dtkm_is_an_observation( result_dtk ) == TRUE )
            downlink_flag = FALSE ;
        else
            return DTKM_ERROR_DTK_IS_NEITHER_OBSERVATION_NOR_DOWNLINK ;

    if ( CAST_DTK_DTKID result_dtk[DTK_DTKID] == 0 )
        insert_flag = TRUE ;
    else
        if ( CAST_DTK_DTKID result_dtk[DTK_DTKID] > 0 )
            insert_flag = FALSE ;
        else
            return DTKM_ERROR_DTKID_LT_ZERO ;

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    --
    --  LIST OF STEPS:  
    -- 
    --  -> 1.  (if insert or downlink) obtain links via fa_schedule_link 
    --     2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --     3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --     4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --     5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s)
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --     6.  if set, flow the QUICKLOOK values (obs_planner_quicklook,
    --         obs_science_quicklook) to downlink
    --
    --     All done with the above steps regarding data flow and links.  
    */

    /* 
    -- possibly obtain the fa_schedule_link 
    -- links:  the only possible links for inserts 
    -- ( there are no dl2obs links for dtks that don't yet exist. ) 
    -- and downlinks ( may need to update/change/delete dl2obs 
    -- links later. )
    */
    if( insert_flag || downlink_flag )
    {
        /* 
        -- obtain the fa_schedule_link links:  
        */

        if( downlink_flag )
        {
            /* dtk proposal is a downlink.  */

            /* 
            -- cleanup later required for 
            -- fa_schedule_link_obs_list  
            */
            fa_schedule_link_obs_list = create_dyn_llist() ;

            /* dtkm_link_obs_dtks() uses dtk.fa_schedule_link.  */
            return_code = dtkm_link_obs_dtks( result_dtk, 
                fa_schedule_link_obs_list ) ;
            if( return_code < 0 )
            {
                DEL_LIST( fa_schedule_link_obs_list ) ;
                return return_code ;
            }
            if( NUMELTS( fa_schedule_link_obs_list ) <= 0 )
            {
                /* 
                -- no elements.  free and NULL the pointer, 
                -- the NULL value is always used as a flag 
                -- to indicate that there are no elements or 
                -- info to work with.  
                */
                DEL_LIST( fa_schedule_link_obs_list ) ;
                fa_schedule_link_obs_list = NULL ;
            }
        }
        else
        {
            /* dtk proposal is an observation.  */

            /* 
            -- cleanup later required for 
            -- fa_schedule_link_dl_dtk_rec  
            -- set a 0 value for REV to check later.  
            */
            fa_schedule_link_dl_dtk_rec = new_table_record( APS_CDEFS(DTK)) ;
            CAST_DTK_REV fa_schedule_link_dl_dtk_rec[DTK_REV] = 0 ;

            /* dtkm_link_dl_dtk() uses dtk.fa_schedule_link.  */
            return_code = dtkm_link_dl_dtk( result_dtk, 
                fa_schedule_link_dl_dtk_rec ) ;
            if( return_code < 0 )
            {
                free_db_record( fa_schedule_link_dl_dtk_rec ) ;
                return return_code ;
            }
            if( CAST_DTK_REV fa_schedule_link_dl_dtk_rec[DTK_REV] <= 0 )
            {
                /* 
                -- no downlink found.  free memory and NULL 
                -- the pointer to indicate this.  
                */
                free_db_record( fa_schedule_link_dl_dtk_rec ) ;
                fa_schedule_link_dl_dtk_rec = NULL ;
            }

        }   /* end  else dtk proposal is an observation  */

    }  /*  endif   if( insert_flag || downlink_flag )   */

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    -- 
    --
    --  LIST OF STEPS:  
    -- 
    --     1.  (if insert or downlink) obtain links via fa_schedule_link 
    --  -> 2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --     3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --     4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --     5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s)
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --     6.  if set, flow the QUICKLOOK values (obs_planner_quicklook,
    --         obs_science_quicklook) to downlink
    --
    --     All done with the above steps regarding data flow and links.  
    */

    /* 
    -- possibly obtain the dl2obs links:   
    -- updates only. ( inserts have no dl2obs links yet.)
    */
    if( insert_flag == FALSE )
    {
        /* 
        -- the input dtk is an update, not an insert.  
        -- obtain the dl2obs -linked dtks here:  
        */

        if( downlink_flag )
        {
            /* dtk proposal is a downlink.  */

            /* cleanup later required for dl2obs_obs_list  */
            dl2obs_obs_list = create_dyn_llist() ;

            /* dtkm_get_obs_dtks() uses dl2obs.  */
            return_code = dtkm_get_obs_dtks( result_dtk, dl2obs_obs_list ) ;
            if( return_code < 0 )
            {
                if( fa_schedule_link_obs_list ) 
                    DEL_LIST( fa_schedule_link_obs_list ) ;
                DEL_LIST( dl2obs_obs_list ) ;
                return return_code ;
            }
            if( NUMELTS( dl2obs_obs_list ) <= 0 )
            {
                /* no links found.  free the memory and NULL the ptr.  */
                DEL_LIST( dl2obs_obs_list ) ;
                dl2obs_obs_list = NULL ;
            }

            /* 
            -- this is an update of a downlink.  at this time, 
            -- we review the 2 lists, 
            -- fa_schedule_link_obs_list and dl2obs_obs_list
            -- and we treat the fa_schedule_link_obs_list as the 
            -- valid set of observations linked to our downlink, 
            -- and we use it to evaluate the dl2obs_obs_list.  
            -- we make corrections to the dl2obs relation as needed 
            -- so that it holds the correct links as indicated 
            -- by the fa_schedule_link_obs_list.  
            -- this is where we fix/repair/maintain the dl2obs relation.
            --
            -- after this function call, the dl2obs_obs_list is no 
            -- longer needed, and cleaned up now.  
            --
            -- note:  either or both llist pointers could be NULL.   
            */
            return_code = dtkm_fix_dl2obs( APS_dbproc, 
                result_dtk, fa_schedule_link_obs_list, dl2obs_obs_list ) ;
            if( dl2obs_obs_list ) 
            {
                DEL_LIST( dl2obs_obs_list ) ;
                dl2obs_obs_list = NULL ;
            }
            if( return_code < 0 )
            {
                if( fa_schedule_link_obs_list ) 
                    DEL_LIST( fa_schedule_link_obs_list ) ;
                return return_code ;
            }
        }
        else
        {
            /* 
            -- dtk proposal is an observation.  use dl2obs to 
            -- get its downlink.  
            */

            /* 
            -- cleanup later required for dl2obs_dl_dtk_rec  
            -- set a 0 value for REV to check later.  
            */
            dl2obs_dl_dtk_rec = new_table_record( APS_CDEFS(DTK) ) ;
            CAST_DTK_REV dl2obs_dl_dtk_rec[DTK_REV] = 0 ;

            /* dtkm_get_dl_dtk() uses dl2obs.  */
            return_code = dtkm_get_dl_dtk( result_dtk, dl2obs_dl_dtk_rec ) ;
            if( return_code < 0 )
            {
                free_db_record( dl2obs_dl_dtk_rec ) ;
                return return_code ;
            }
            if( CAST_DTK_REV dl2obs_dl_dtk_rec[DTK_REV] <= 0 )
            {
                /* 
                -- no downlink found.  free memory and NULL 
                -- the pointer to indicate this.  
                */
                free_db_record( dl2obs_dl_dtk_rec ) ;
                dl2obs_dl_dtk_rec = NULL ;
            }

        }

    }  /* endif( insert_flag == FALSE )    */

    /*
    -- if the input dtk was an update of a downlink, 
    -- then the dl2obs records for it were corrected, 
    -- if necessary, above.  They are now OK.  
    */

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    -- 
    --
    --  LIST OF STEPS:  
    -- 
    --     1.  (if insert or downlink) obtain links via fa_schedule_link 
    --     2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --  -> 3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --     4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --     5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s)
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --     6.  if set, flow the QUICKLOOK values (obs_planner_quicklook,
    --         obs_science_quicklook) to downlink
    --
    --     All done with the above steps regarding data flow and links.  
    */


    /********************************************************************
     *                                                                  *
     *       obtain QUICKLOOK values from observations for downlink,    *
     *       whether the input dtk is an observation or a downlink.     *
     *                                                                  *
     *       if the input dtk is an observation, there could be a       *
     *       change to a quicklook value which could result             *
     *       in a change in the quicklook value in its downlink.        *
     *       to know this, we must check all OTHER observations,        *
     *       linked to this downlink.                                   *   
     *                                                                  *
     ********************************************************************/
    if( downlink_flag )
    {
        /* 
        -- input dtk is a downlink.  if observations 
        -- are already present, use fa_schedule_link_obs_list 
        -- to get quicklook values.  
        */
        if( fa_schedule_link_obs_list )
        {
            return_code = dtkm_get_quicklook_values( fa_schedule_link_obs_list,
                &obs_planner_quicklook, &obs_science_quicklook ) ;
            if( return_code < 0 )
            {
                DEL_LIST( fa_schedule_link_obs_list ) ;
                return return_code ;
            }

            /* 
            -- quicklook values obtained 
            -- in the case of the input dtk being a downlink.  
            */
        }
    }
    else
    {
        /* 
        -- input dtk is an observation.  
        -- if there is a known downlink for this observation, then get all 
        -- of its observations.  
        -- this is where we need to know all of the observation 
        -- quicklook values, even though only the one observation 
        -- is being created/updated.  The insert or update to our 
        -- observation could affect its linked downlink.  the only 
        -- way to know this is to look at all of the observations 
        -- for that linked downlink at the same time.  
        */
        if( fa_schedule_link_dl_dtk_rec )
            linked_dl_dtk_rec = fa_schedule_link_dl_dtk_rec ;
        else if ( dl2obs_dl_dtk_rec )
            linked_dl_dtk_rec = dl2obs_dl_dtk_rec ;

        if( linked_dl_dtk_rec )
        {
            obs_dtk_list = create_dyn_llist() ;
            return_code = dtkm_dl2obs( linked_dl_dtk_rec, obs_dtk_list ) ;
            if( return_code < 0 )
            {
                DEL_LIST( obs_dtk_list ) ;
                if( fa_schedule_link_dl_dtk_rec )
                    free_db_record( fa_schedule_link_dl_dtk_rec ) ;
                if( dl2obs_dl_dtk_rec )
                    free_db_record( dl2obs_dl_dtk_rec ) ;
                return return_code ;
            }

            /*
            -- if present, copy the input observation data 
            -- into its entry in the list of observations, 
            -- so that the observation list will be correct,
            -- and so that the computed quicklook value will be correct.  
            -- if the input observation is not in the list of 
            -- observations, then append the input observation dtk 
            -- rec to the list.  
            */
            obs_dtk_in_list_flag = FALSE ;
            for (   obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, 
                                                        obs_dtk_list_ptr);
                    obs_dtk_rec ;
                    obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, 
                                                        obs_dtk_list_ptr)  
                )
            {
                /* process the current obs_dtk_rec right here.  */
                if( strcmp( CAST_DTK_SAT obs_dtk_rec[DTK_SAT], 
                            CAST_DTK_SAT  result_dtk[DTK_SAT] ) == 0
                &&     CAST_DTK_REV obs_dtk_rec[DTK_REV] 
                    == CAST_DTK_REV  result_dtk[DTK_REV]
                &&     CAST_DTK_DTKID obs_dtk_rec[DTK_DTKID] 
                    == CAST_DTK_DTKID  result_dtk[DTK_DTKID]  )
                {
                    obs_dtk_in_list_flag = TRUE ;
                    if( strcmp("DEL",
                            CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) == 0  )
                    {
                        /* 
                        -- input dtk observation will be DEL; 
                        -- this record therefore must not count, 
                        -- in determining downlink quicklook value. 
                        */
                        DEL_AT_CURSOR( obs_dtk_list, obs_dtk_list_ptr ) ;
                    }
                    else
                    {
                        /* 
                        -- match on sat/rev/dtkid, the primary keys.  
                        -- the input dtk rec was in this list.  
                        -- copy all of the input dtk rec values 
                        -- into the dtk rec in the list, to reflect the 
                        -- new/latest values of the input dtk rec.  
                        */
                        (void) db_copy_record( APS_CDEFS(DTK), 
                            obs_dtk_rec, result_dtk ) ;
                    }

                    /* don't process any more of the list.  */
                    break ;
                }
            }  /* end of for loop.  */

            if( obs_dtk_in_list_flag == FALSE )
            {
                if( strcmp("DEL",CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) )
                {
                    /* input dtk is not DEL.  OK:   */
                    /* 
                    -- the input dtk observation was not in 
                    -- the list.  duplicate the input observation, 
                    -- (allocating new memory) then APPEND() to list.  
                    -- Why not just APPEND() result_dtk?  
                    -- because if DEL_LIST(obs_dtk_list), we would 
                    -- free the memory associated with result_dtk, 
                    -- which is allocated and managed elsewhere.  
                    -- result_dtk is an argument to this routine.  
                    */
                    obs_dtk_rec = db_duplicate_record( APS_CDEFS(DTK), 
                        result_dtk);
                    APPEND( obs_dtk_list, obs_dtk_rec, free_db_record, 
                        obs_dtk_rec);
                }
            }

            return_code = dtkm_get_quicklook_values( obs_dtk_list,
                &obs_planner_quicklook, &obs_science_quicklook ) ;
            /* obs_dtk_list not used any more:  */
            DEL_LIST( obs_dtk_list ) ;
            obs_dtk_list = NULL ;
            if( return_code < 0 )
            {
                if( fa_schedule_link_dl_dtk_rec )
                    free_db_record( fa_schedule_link_dl_dtk_rec ) ;
                if( dl2obs_dl_dtk_rec )
                    free_db_record( dl2obs_dl_dtk_rec ) ;
                return return_code ;
            }
        }
        /* 
        -- quicklook values obtained 
        -- in the case of the input dtk being an observation.  
        */
    }

    /* 
    -- quicklook values obtained 
    */

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    -- 
    --
    --  LIST OF STEPS:  
    -- 
    --     1.  (if insert or downlink) obtain links via fa_schedule_link 
    --     2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --     3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --  -> 4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --     5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s)
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --     6.  if set, flow the QUICKLOOK values (obs_planner_quicklook,
    --         obs_science_quicklook) to downlink
    --
    --     All done with the above steps regarding data flow and links.  
    */

    /********************************************************************
     *                                                                  *
     *       obtain dtkstat value from downlink for observations,       *
     *       whether the input dtk is an observation or a downlink.     *
     *                                                                  *
     *       if the input dtk is a downlink, there could be a           *
     *       change to a dtkstat value which must flow to its           *
     *       observations.                                              *
     *                                                                  *
     ********************************************************************/
    if( downlink_flag )
        strcpy( dl_dtkstat, CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT] ) ;
    else
    {
        /* 
        -- input dtk is an observation.  
        -- if there is a known downlink for this observation, 
        -- then get its status.  
        */
        if( fa_schedule_link_dl_dtk_rec )
            strcpy( dl_dtkstat, 
                CAST_DTK_DTKSTAT fa_schedule_link_dl_dtk_rec[DTK_DTKSTAT] ) ;
        else if ( dl2obs_dl_dtk_rec )
            strcpy( dl_dtkstat, 
                CAST_DTK_DTKSTAT dl2obs_dl_dtk_rec[DTK_DTKSTAT] ) ;
        /* 
        -- downlink dtkstat value obtained 
        -- in the case of the input dtk being an observation.  
        */
    }

    /* 
    -- downlink dtkstat value obtained 
    */

    /*
    -- at this point, the dtkstat value is known and 
    -- the quicklook values are known.  
    -- And the input dtk has not been inserted/updated.  
    -- we now flow the values between downlink and 
    -- observation.  If a value flows to the input 
    -- dtk record, we just update the value in the 
    -- **DB_RECORD.  But if a value flows to a linked 
    -- dtk record, we update the value in the database.  
    -- 
    -- That is, if the input is a downlink, and the dtkstat 
    -- flows to linked observations, we update the 
    -- observations in the database.  But the quicklook values 
    -- which flow into the downlink we just put in the input 
    -- into the **DB_RECORD.  
    */

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    -- 
    --
    --  LIST OF STEPS:  
    -- 
    --     1.  (if insert or downlink) obtain links via fa_schedule_link 
    --     2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --     3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --     4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --  -> 5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s).
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --     6.  if set, flow the QUICKLOOK values (obs_planner_quicklook, 
    --         obs_science_quicklook) to downlink  
    --
    --     All done with the above steps regarding data flow and links.  
    */
    /**********************************************************************
     *                                                                    *
     *  If set, flow the dtkstat value from downlink to observation.      *
     *                                                                    *
     *********************************************************************/

    if( strcmp( dl_dtkstat, "QUE" ) == 0
    ||  strcmp( dl_dtkstat, "SUB" ) == 0
    ||  strcmp( dl_dtkstat, "PLN" ) == 0
    ||  strcmp( dl_dtkstat, "SCH" ) == 0
    ||  strcmp( dl_dtkstat, "REJ" ) == 0
    ||  strcmp( dl_dtkstat, "DEL" ) == 0
    ||  strcmp( dl_dtkstat, "CON" ) == 0  )
    {
        /* 
        -- the value of the downlink dtkstat 
        -- indicates that it should be flowed to
        -- the observation(s)
        */
        if( downlink_flag == FALSE )
        {
            /* 
            -- input dtk is an Observation; 
            -- update DB_RECORD from Downlink dtkstat, 
            -- except if input obs is DEL:  
            */
            if( strcmp("DEL", CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT]) != 0 )
                strcpy( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], dl_dtkstat ) ;

        }
        else if( fa_schedule_link_obs_list ) 
        {
            /* 
            -- input dtk is a downlink.  
            -- AND there are observations linked.  Must flow the 
            -- dtkstat value to the observations in the database.  
            -- Also flow the planner quicklook value to the observations 
            -- in the database.  
            -- use fa_schedule_link_obs_list as being correct/latest.  
            -- set up fields_to_set, the same for all obs:
            */
            (void) tc_systime2asf(now_asftime) ;
            sprintf( fields_to_set, "%s = '%c', %s = '%s', %s = '%s'",
                APS_COL(DTK, DTK_PLANNER_QUICKLOOK), 
                  CAST_DTK_PLANNER_QUICKLOOK result_dtk[DTK_PLANNER_QUICKLOOK],
                APS_COL(DTK, DTK_DTKSTAT), dl_dtkstat,
                APS_COL(DTK, DTK_DTKDATE), now_asftime ) ;

            for (   obs_dtk_rec = (DB_RECORD **) FIRST(
                                    fa_schedule_link_obs_list, dtk_list_ptr);
                    obs_dtk_rec ;
                    obs_dtk_rec = (DB_RECORD **) NEXT(
                                    fa_schedule_link_obs_list, dtk_list_ptr)  
                )
            {
                /* process the current obs_dtk_rec right here.  */
                if( strcmp( CAST_DTK_DTKSTAT obs_dtk_rec[DTK_DTKSTAT], 
                    dl_dtkstat) != 0 
                ||  CAST_DTK_PLANNER_QUICKLOOK 
                        obs_dtk_rec[DTK_PLANNER_QUICKLOOK] 
                    != CAST_DTK_PLANNER_QUICKLOOK 
                        result_dtk[DTK_PLANNER_QUICKLOOK]  )
                {
                    /* must update the obs rec in db.  */
                    /* ALSO update DB_RECORD for dtk_updates list  */

                    strcpy( CAST_DTK_DTKSTAT obs_dtk_rec[DTK_DTKSTAT], 
                        dl_dtkstat) ;

                    CAST_DTK_PLANNER_QUICKLOOK 
                      obs_dtk_rec[DTK_PLANNER_QUICKLOOK] 
                        = CAST_DTK_PLANNER_QUICKLOOK 
                              result_dtk[DTK_PLANNER_QUICKLOOK]  ;

                    sprintf( where_clause,
                        "where %s = '%s' and %s = %ld and %s = %d",
                        APS_COL(DTK, DTK_SAT), 
                                CAST_DTK_SAT   obs_dtk_rec[DTK_SAT],
                        APS_COL(DTK, DTK_REV),
                                CAST_DTK_REV   obs_dtk_rec[DTK_REV],
                        APS_COL(DTK, DTK_DTKID),
                                CAST_DTK_DTKID obs_dtk_rec[DTK_DTKID]);

                    nrecs = db_update_records( APS_dbproc, APS_TABLE( DTK ),
                        fields_to_set, where_clause ) ;
                    if( nrecs < 0 )
                    {
                        DEL_LIST( fa_schedule_link_obs_list ) ;
                        return DTKM_ERROR_DTK_NOT_UPDATED ;
                    }

                    /* important:  report change into dtk_updates list.  */
                    list_check = dtkm_duplicate_dtk_into_list( 
                        obs_dtk_rec, dtk_updates ) ;
                    if ( list_check != dtk_updates )
                    {
                        DEL_LIST( fa_schedule_link_obs_list ) ;
                        return DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST ;
                    }

                } /* endif:  change to dtkstat.  */
             
            }  /* end of for loop on observations.  */

        } /* end if/else on downlink_flag  */

    } /* endif on dtkstat changes.  */

    /*
    -- dtkstat flow has been completed.  
    */

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    -- 
    --
    --  LIST OF STEPS:  
    -- 
    --     1.  (if insert or downlink) obtain links via fa_schedule_link 
    --     2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --     3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --     4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --     5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s)
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --  -> 6.  if set, flow the QUICKLOOK values (obs_planner_quicklook, 
    --         obs_science_quicklook) to downlink  
    --
    --     All done with the above steps regarding data flow and links.  
    */

    /**********************************************************************
     *                                                                    *
     *  If set, flow the QUICKLOOK values between downlink                *
     *  and observations.                                                 *
     *                                                                    *
     *********************************************************************/
    if( downlink_flag )
    {
        /* 
        -- the science quicklook flag is set by science users 
        -- which flows to the sensing activities.  Now flow 
        -- this value into result_dtk.  
        */

        if( obs_science_quicklook == 'N' ||  obs_science_quicklook == 'Y' )
        {
            CAST_DTK_SCIENCE_QUICKLOOK result_dtk[DTK_SCIENCE_QUICKLOOK] 
                = obs_science_quicklook ;
        }
    }
    else
    {
        /* -- input dtk is an observation, update downlink in database.  */

        /* set up DB_RECORD for downlink.  */
        linked_dl_dtk_rec = NULL ;

        if( insert_flag )
        {
            /* 
            -- input dtk is an insert observation; use 
            -- fa_schedule_link_dl_dtk_rec.  
            */
            if( fa_schedule_link_dl_dtk_rec )
                linked_dl_dtk_rec = fa_schedule_link_dl_dtk_rec ;
        }
        else
        {
            /* 
            -- input dtk is an update observation; use dl2obs_dl_dtk_rec.  
            */
            if( dl2obs_dl_dtk_rec )
                linked_dl_dtk_rec = dl2obs_dl_dtk_rec ; 
        }

        if( linked_dl_dtk_rec )
        {
            /* 
            -- there is a linked DL DB_RECORD; now update 
            -- the quicklook values of this 
            -- downlink in the database.  
            */

            return_code = dtkm_update_dl_quicklook( APS_dbproc, 
                obs_planner_quicklook, obs_science_quicklook, 
                linked_dl_dtk_rec, dtk_updates ) ;
            if( return_code < 0 )
            {
                if( fa_schedule_link_dl_dtk_rec )
                    free_db_record( fa_schedule_link_dl_dtk_rec ) ;
                if( dl2obs_dl_dtk_rec )
                    free_db_record( dl2obs_dl_dtk_rec ) ;
                return return_code ;
            }

        } /* endif (linked_dl_dtk_rec)    */

    }  /* endif   else dtk is an observation.   */

    /*
    --  First, we take care of the data flow and links between the input 
    --  dtk DB_RECORD and its linked dtk recs in the database.  
    --  The input dtk DB_RECORD could be:  
    --  insert or update, observation or downlink.  
    -- 
    --
    --  LIST OF STEPS:  
    -- 
    --     1.  (if insert or downlink) obtain links via fa_schedule_link 
    --     2.  (if update) obtain the links via the dl2obs relation.  If a
    --         downlink is being updated, fix/repair/maintain the 
    --         dl2obs links as needed; the fa_schedule_link links are 
    --         considered to be correct.  
    --     3.  obtain QUICKLOOK value (obs_science_quicklook) from 
    --         observation(s) by logical 'OR' for use by downlink.
    --     4.  obtain dtkstat value (dl_dtkstat) from downlink for use by 
    --         observation(s)
    --     5.  if set, flow the dtkstat value (dl_dtkstat) to observations(s)
    --         If proposal is a downlink, flow planner_quicklook to 
    --         observation(s), too.  
    --     6.  if set, flow the QUICKLOOK values (obs_planner_quicklook, 
    --         obs_science_quicklook) to downlink  
    --
    --  -> All done with the above steps regarding data flow and links.  
    */

    /********************************************************************
     *                                                                  *
     *   All of the linked dtk records have been handled.  The data     *
     *   has already flowed between downlink and observation.           *
     *   Now it is time to update the input dtk record and possibly     *
     *   insert the new dl2obs record.                                  *
     *                                                                  *
     ********************************************************************/

    /* 
    -- In case of an insert dtk record: use dtkm_insert_dtk_record()
    -- In case of an update dtk record: use dtkm_update_dtk_record()
    */

    if ( insert_flag != TRUE )
    {
        /* update.  */
        return_code = dtkm_update_dtk_record(APS_dbproc,
            result_dtk, result_dtk, dtk_updates ) ;
        if ( return_code < 0 )
        {
            if( fa_schedule_link_dl_dtk_rec )
                free_db_record( fa_schedule_link_dl_dtk_rec ) ;
            if( dl2obs_dl_dtk_rec )
                free_db_record( dl2obs_dl_dtk_rec ) ;
            return return_code ;
        }

        /* 
        -- updating of dl2obs links is 
        -- already done.  we can return here.  
        */
        /* 
        -- CLEAN UP.  
        -- ALL UPDATES TERMINATE HERE
        */
        if( fa_schedule_link_obs_list ) 
            DEL_LIST( fa_schedule_link_obs_list ) ;
        if( dl2obs_obs_list ) 
            DEL_LIST( dl2obs_obs_list ) ;
        if( fa_schedule_link_dl_dtk_rec )
            free_db_record( fa_schedule_link_dl_dtk_rec ) ;
        if( dl2obs_dl_dtk_rec )
            free_db_record( dl2obs_dl_dtk_rec ) ;
        return DTKM_DTK_UPDATED_OK ;

    }  /* endif update dtk.   */

    /* 
    -- below here, we are only 
    -- processing INSERT data-takes.  
    */

    /* 
    -- processing INSERT data-takes:  
    -- AT THIS POINT:  dtkid is assigned, and is no longer == 0 
    */
    return_code = dtkm_insert_dtk_record(APS_dbproc,
        result_dtk, result_dtk, dtk_updates ) ;
    if ( return_code < 0 )
    {
        if( fa_schedule_link_obs_list ) 
            DEL_LIST( fa_schedule_link_obs_list ) ;
        if( dl2obs_obs_list ) 
            DEL_LIST( dl2obs_obs_list ) ;
        if( fa_schedule_link_dl_dtk_rec )
            free_db_record( fa_schedule_link_dl_dtk_rec ) ;
        if( dl2obs_dl_dtk_rec )
            free_db_record( dl2obs_dl_dtk_rec ) ;
        return return_code ;
    }

    /* note:  dtk.dtkid for the input dtk (result_dtk) is now > 0   */

    /* 
    -- processing INSERT data-takes:  
    -- there is one possible circumstance to handle here: 
    -- if the input dtk is an insert of a downlink:  
    --     Due to fa_schedule_link values, it is possible that one or more
    --     linked observations already have a record in dl2obs.  
    --     And when we go to insert the new dl2obs record, there will 
    --     be a Sybase error, inserting duplicate rec with same primary keys.
    --     To prevent this, we first delete an existing dl2obs rec if 
    --     it exists.
    */

    /*
    -- processing INSERT data-takes:  
    -- no more worries here.  now insert the new dl2obs records.  
    -- they all should insert OK.  
    */
    /* note:  dtk.dtkid for the input dtk (result_dtk) is now > 0   */
    if( downlink_flag == FALSE )
    {
        /* input dtk is an observation.  1 record to insert.  */
        /* note:  dtk.dtkid for the input dtk (result_dtk) is now > 0   */
        if( fa_schedule_link_dl_dtk_rec )
        {
            /* 
            -- there is a known downlink.   create a dl2obs linke 
            -- between input dtk and the downlink:
            */
            return_code = dtkm_insert_dl2obs_rec( APS_dbproc, 
                result_dtk, fa_schedule_link_dl_dtk_rec ) ;
            if ( return_code < 0 )
            {
                free_db_record( fa_schedule_link_dl_dtk_rec ) ;
                if( fa_schedule_link_obs_list ) 
                    DEL_LIST( fa_schedule_link_obs_list ) ;
                if( dl2obs_obs_list ) 
                    DEL_LIST( dl2obs_obs_list ) ;
                if( dl2obs_dl_dtk_rec )
                    free_db_record( dl2obs_dl_dtk_rec ) ;
                return return_code ;
            }
            free_db_record(fa_schedule_link_dl_dtk_rec ) ;
            fa_schedule_link_dl_dtk_rec = NULL ;

        }  /* end if fa_schedule_link_dl_dtk_rec  */

    }
    else
    {
        /* input (insert) dtk is a downlink.  multiple records to insert.  */
        /* note:  dtk.dtkid for the input dtk (result_dtk) is now > 0   */
        if( fa_schedule_link_obs_list )
        {
            /* there is a list.  for each entry, insert its link.  */

            for (   obs_dtk_rec = (DB_RECORD **) 
                                FIRST(fa_schedule_link_obs_list, dtk_list_ptr);
                    obs_dtk_rec ;
                    obs_dtk_rec = (DB_RECORD **) 
                                NEXT(fa_schedule_link_obs_list, dtk_list_ptr)  
                )
            {
                /* process the current obs_dtk_rec right here.  */
                /* 
                -- create a dl2obs link between obs_dtk_rec dtk and 
                -- the input downlink (result_dtk):
                -- note:  this routine checks for an existing 
                -- dl2obs and deletes it first, if it exists.  
                */
                return_code = dtkm_insert_dl2obs_rec( APS_dbproc, 
                    obs_dtk_rec, result_dtk ) ;
                if ( return_code < 0 )
                {
                    DEL_LIST( fa_schedule_link_obs_list ) ;
                    return return_code ;
                }

            } /* end for-loop on fa_schedule_link_obs_list of dtks.   */

            /* free the storage.  */
            DEL_LIST( fa_schedule_link_obs_list ) ;
            fa_schedule_link_obs_list = NULL ;

        } /* endif fa_schedule_link_obs_list exists     */

        if( dl2obs_obs_list ) 
        {
            DEL_LIST( dl2obs_obs_list ) ;
            dl2obs_obs_list = NULL ;
        }

    }  /* endif - else - input (insert) dtk is a downlink.   */

    if( fa_schedule_link_dl_dtk_rec )
        free_db_record( fa_schedule_link_dl_dtk_rec ) ;
    if( dl2obs_dl_dtk_rec )
        free_db_record( dl2obs_dl_dtk_rec ) ;

    return DTKM_DTK_UPDATED_OK ;

}
