#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_delete_dl2obs_recs.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_delete_dl2obs_recs.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_delete_dl2obs_recs.c"



/*==============================================================================
Function:       dtkm_delete_dl2obs_recs()

Description:    deletes any entries in the dl2obs relation that pertain 
                to the input data-take, which could be a downlink 
                or an observation activity.  
                ALSO:  
                if an observation:
                    Re-do the downlink quicklook values.  The logical OR 
                    may be different due to the deletion of the observation.  
                    This is necessitated by the deletion of dl2obs links.  
                if downlinke:
                    Update the dtk.dtkstat value to DEL for each 
                    linked observation.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  2 23:22:58 PST 1995

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"       /* for CAST_DTK_SAT etc.  */
#include "db_dl2obs.h"    /* for DL2OBS_SAT etc.    */
#include "timeconv.h"    /* for tc_systime2asf()  etc.    */

int dtkm_delete_dl2obs_recs(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_record,  /* delete dl2obs associated with this dtk.    */
    llist       *dtk_updates ) /* if dtk updates are necessitated, put here  */
{

    int         return_code ;
    int         nrecs ;
    int         downlink_flag = FALSE ;

    char        now_asftime[ASF_TIME_STR_LENGTH+1] ;
    char        dl_planner_quicklook ;
    char        dl_science_quicklook ;

    /* 
    -- lists/rec pointers initialized to NULL 
    -- some values are compared with NULL later, as a flag,  
    -- to determine the presence of data.  
    */
    DB_RECORD   **dl2obs_rec = NULL ;
    llist       *dl2obs_list = NULL ;
    cursor       dl2obs_list_ptr ;

    DB_RECORD   **downlink_rec = NULL ;
    llist       *downlink_dtk_list = NULL ;
    cursor      downlink_dtk_list_ptr ;

    DB_RECORD   **obs_dtk_rec = NULL ;
    llist       *obs_dtk_list = NULL ;
    cursor      obs_dtk_list_ptr ;

    /* quick error checking. */
    if ( dtk_record == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    if( dtkm_is_a_downlink( dtk_record ) == TRUE )
    {
        downlink_flag = TRUE ;
        /* where_clause for a downlink:  */
        sprintf( where_clause,
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT),
                CAST_DTK_SAT dtk_record[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_DL),
                CAST_DTK_REV dtk_record[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_DL),
                CAST_DTK_DTKID dtk_record[DTK_DTKID] ) ;
        /* 
        -- must later find all of the observations linked to 
        -- the downlink and then change their dtk.dtkstat 
        -- value to DEL.  
        */
    }
    else
    {
        /* where_clause for an observation:  */
        sprintf( where_clause,
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT),
                CAST_DTK_SAT dtk_record[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_OBS),
                CAST_DTK_REV dtk_record[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_OBS),
                CAST_DTK_DTKID dtk_record[DTK_DTKID] ) ;

        /* 
        -- must re-do the quicklook values in the associated 
        -- downlink, after the current dl2obs link is deleted.  
        -- For example, if there are planner quicklook values 
        -- of Y for one observation and N for another observation, 
        -- there would be a Y in the downlink planner quicklook value.  
        -- 
        -- But deleting the observation with the Y value requires 
        -- that the downlink value get changed from Y to N.  
        */

    }

    /* 
    -- now save the dl2obs record(s), to be deleted, 
    -- for later use in quicklook value changes or 
    -- dtk.dtkstat changes:  
    */
    dl2obs_list = db_get_records( APS_dbproc, APS_TABLE(DL2OBS), 
        where_clause, NULL, APS_CDEFS(DL2OBS), ALL_COLS ) ;
    if( dl2obs_list == NULL )
        return DTKM_ERROR_DB_QUERY_FAILED ; 

    nrecs = db_delete_records(APS_dbproc,
        APS_TABLE(DL2OBS), where_clause ) ;
    if ( nrecs < 0 )
    {
        DEL_LIST( dl2obs_list ) ;
        return DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION ;
    }

    /* 
    -- Note the following two 'if' statements:
    -- This code construction prevents a core dump,  
    -- preventing the use of dl2obs_list by NUMELTS() if dl2obs_list == NULL.  
    -- The lack of indentation for the second 'if' is to 
    -- promote readability.  
    */
    if( dl2obs_list )
    if( NUMELTS( dl2obs_list ) > 0 )
    {
        /* get the time for dtk.dtkdate value.  */
        (void) tc_systime2asf( now_asftime ) ;

        if( downlink_flag == TRUE ) 
        {
            /* 
            -- 1 or more observations linked to the downlink, and 
            -- each must be set to DEL dtk.dtkstat value.  
            -- use the info in dl2obs_list to update each observations.  
            -- note:  the fields to set are the same for each observation:
            */
            sprintf( fields_to_set, "%s = '%s', %s = '%s'",
                APS_COL(DTK, DTK_DTKSTAT), "DEL",
                APS_COL(DTK, DTK_DTKDATE), now_asftime ) ;

            for (   dl2obs_rec = (DB_RECORD **) FIRST(dl2obs_list, 
                                                            dl2obs_list_ptr);
                    dl2obs_rec ;
                    dl2obs_rec = (DB_RECORD **) NEXT(dl2obs_list, 
                                                            dl2obs_list_ptr)  
                )
            {
                /* process the current obs_dtk_rec right here.  */

                /* update this dtk observation; use keys from dl2obs rec:  */
                sprintf(where_clause, 
                    "where %s = '%s' and %s = %ld and %s = %d",
                    APS_COL(DTK, DTK_SAT), 
                        CAST_DL2OBS_SAT dl2obs_rec[DL2OBS_SAT],
                    APS_COL(DTK, DTK_REV), 
                        CAST_DL2OBS_REV_OBS dl2obs_rec[DL2OBS_REV_OBS],
                    APS_COL(DTK, DTK_DTKID), 
                        CAST_DL2OBS_DTKID_OBS dl2obs_rec[DL2OBS_DTKID_OBS]) ;

                nrecs = db_update_records(APS_dbproc,
                    APS_TABLE(DTK), fields_to_set, where_clause) ;
                if ( nrecs < 0 )
                {
                    DEL_LIST( dl2obs_list ) ;
                    return DTKM_ERROR_DB_UPDATE_FAILED_ON_DTK_RELATION ;
                }

                /* 
                -- important:  report change into dtk_updates list.  
                --             must retrieve the record changed.  
                --             use the same where_clause. 
                */
                obs_dtk_list = db_get_records( APS_dbproc, APS_TABLE(DTK), 
                    where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
                if (obs_dtk_list == NULL)
                {
                    DEL_LIST( dl2obs_list ) ;
                    return DTKM_ERROR_DB_QUERY_FAILED ; 
                }
                if( NUMELTS( obs_dtk_list ) == 1 )
                {
                    /* OK.  Report:  */
                    obs_dtk_rec = (DB_RECORD **) 
                                    FIRST(obs_dtk_list, obs_dtk_list_ptr) ;
                    (void) dtkm_duplicate_dtk_into_list( obs_dtk_rec, 
                        dtk_updates ) ;
                }
                DEL_LIST( obs_dtk_list ) ;
                obs_dtk_list = NULL ;

            } /* end   for obs_dtk_list  */

        }  /* endif  downlink_flag == TRUE   */

        if( downlink_flag == FALSE ) 
        {
            /* 
            -- an observation, with a single downlink.   
            -- a single rec in dl2obs_list.  
            */
            dl2obs_rec = (DB_RECORD **) FIRST(dl2obs_list, dl2obs_list_ptr) ;

            /* 
            -- There existed a downlink that was linked to the observation.  
            -- That downlink may need to have its quicklook values 
            -- changed if there are any remaining observations 
            -- linked to it.  
            -- get the downlink record by using dl2obs info:  
            -- dtk field = dl2obs values.  
            */
            sprintf(where_clause, "where %s = '%s' and %s = %ld and %s = %d",
                APS_COL(DTK, DTK_SAT),      
                            CAST_DL2OBS_SAT      dl2obs_rec[DL2OBS_SAT],
                APS_COL(DTK, DTK_REV),      
                            CAST_DL2OBS_REV_DL   dl2obs_rec[DL2OBS_REV_DL],
                APS_COL(DTK, DTK_DTKID),    
                            CAST_DL2OBS_DTKID_DL dl2obs_rec[DL2OBS_DTKID_DL] ) ;
            downlink_dtk_list = db_get_records( APS_dbproc, APS_TABLE(DTK), 
                where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
            if( downlink_dtk_list == NULL )
            {
                DEL_LIST( dl2obs_list ) ;
                return DTKM_ERROR_DB_QUERY_FAILED ;
            }

            if( NUMELTS( downlink_dtk_list ) <= 0 )
            {
                /* 
                -- error.   this downlink does not exist.  
                -- delete any dl2obs record(s) still associated with 
                -- this downink.  This is a clean up.  then 
                -- proceed normally.  
                */
                sprintf(where_clause,"where %s = '%s' and %s = %ld and %s = %d",
                    APS_COL(DL2OBS, DL2OBS_SAT),      
                        CAST_DL2OBS_SAT      dl2obs_rec[DL2OBS_SAT],
                    APS_COL(DL2OBS, DL2OBS_REV_DL),      
                        CAST_DL2OBS_REV_DL   dl2obs_rec[DL2OBS_REV_DL],
                    APS_COL(DL2OBS, DL2OBS_DTKID_DL),    
                        CAST_DL2OBS_DTKID_DL dl2obs_rec[DL2OBS_DTKID_DL] ) ;
                nrecs = db_delete_records(APS_dbproc,
                    APS_TABLE(DL2OBS), where_clause ) ;
                if ( nrecs < 0 )
                {
                    DEL_LIST( dl2obs_list ) ;
                    DEL_LIST( downlink_dtk_list ) ;
                    return DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION ;
                }
            }
            else
            {
                /* 
                -- downlink exists in list.  get observations linked 
                -- by dl2obs for possible updating of downlink's 
                -- quicklook values.  
                */
                downlink_rec = (DB_RECORD **) FIRST( downlink_dtk_list, 
                    downlink_dtk_list_ptr) ;

                /*
                -- use dl2obs relation to obtain the observations 
                -- for the downlink:  
                */
                obs_dtk_list = create_dyn_llist() ;
                return_code = dtkm_get_obs_dtks( downlink_rec, obs_dtk_list ) ;
                if( return_code < 0 )
                {
                    DEL_LIST( obs_dtk_list ) ;
                    DEL_LIST( downlink_dtk_list ) ;
                    DEL_LIST( dl2obs_list ) ;
                    return return_code ;
                }
                if( NUMELTS( obs_dtk_list ) > 0 )
                {
                    /* 
                    -- get quicklook values for downlink.  
                    -- input dtk is a downlink.  all observations
                    -- are already present; use obs_dtk_list.
                    */
                    return_code = dtkm_get_quicklook_values( obs_dtk_list, 
                        &dl_planner_quicklook, &dl_science_quicklook ) ;
                    if( return_code < 0 )
                    {
                        DEL_LIST( obs_dtk_list ) ;
                        DEL_LIST( downlink_dtk_list ) ;
                        DEL_LIST( dl2obs_list ) ;
                        return return_code ;
                    }

                    /* 
                    -- if necessary, update database quicklook values in 
                    -- the downlink.  
                    */
                    return_code = dtkm_update_dl_quicklook( APS_dbproc, 
                        dl_planner_quicklook, dl_science_quicklook, 
                        downlink_rec, dtk_updates ) ;
                    if( return_code < 0 )
                    {
                        DEL_LIST( obs_dtk_list ) ;
                        DEL_LIST( downlink_dtk_list ) ;
                        DEL_LIST( dl2obs_list ) ;
                        return return_code ;
                    }

                }  /* endif  if( NUMELTS( obs_dtk_list ) > 0    */
                DEL_LIST( obs_dtk_list ) ;
                obs_dtk_list = NULL ;

            }  /* endif  NUMELTS( downlink_dtk_list ) > 0   */
            DEL_LIST( downlink_dtk_list  ) ;
            downlink_dtk_list = NULL ;

        }  /* endif   NUMELTS( dl2obs_list ) > 0   */

        DEL_LIST( dl2obs_list ) ;
        dl2obs_list = NULL ;

    } /* endif  dl2obs_list   */
    if( dl2obs_list )
        DEL_LIST( dl2obs_list ) ;

    return TRUE ;

}
