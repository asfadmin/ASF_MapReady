#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_fix_dl2obs.c

Description:    update, if necessary, dl2obs records in the database.  


==============================================================================*/
#pragma ident   "@(#)dtkm_fix_dl2obs.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_fix_dl2obs.c"


#include "dtkm.h"
#include <db_dtk.h>       /*  for CAST_DTK_REV, etc.        */
#include <db_dl2obs.h>    /*  for DL2OBS_DTKID_OBS, etc.    */
    /* 
    -- set up a structure which is to function as 
    -- a check list for each data-take observation:
    */
#define CHECK_LIST_SIZE 100

typedef
    struct _CHECK_LIST_ENTRY 
    {
        DB_RECORD   **obs_dtk_rec ;  
        int         correct_flag ; /* TRUE if it appears in correct_obs_list  */
        int         dl2obs_flag ;  /* TRUE if it appears in dl2obs_obs_list   */
    } CHECK_LIST_ENTRY ;



/*==============================================================================
Function:       dtkm_insert_dtk2checklist()

Description:    return the index on the list of the input dtk.  
                If the dtk is not yet in the list, put it, then 
                return the index on the list of the input dtk. 

Creator:        Lawrence Stevens

Creation Date:  Wed Aug 13 17:37:11 PDT 1997

==============================================================================*/
static int dtkm_insert_dtk2checklist( 
    DB_RECORD           **dtk_rec,
    CHECK_LIST_ENTRY    *dtk_check_list )
{
    int     index ;

    /* try to locate the input dtk.  */
    for ( 
            index = 0 ;
            dtk_check_list[index].obs_dtk_rec ;
            index ++ 
        )
    {
        /* looking for a match on (sat), rev, dtkid) :  */
        if(     CAST_DTK_REV (dtk_check_list[index].obs_dtk_rec)[DTK_REV]
            ==  CAST_DTK_REV dtk_rec[DTK_REV] 
        &&      CAST_DTK_DTKID (dtk_check_list[index].obs_dtk_rec)[DTK_DTKID]
            ==  CAST_DTK_DTKID dtk_rec[DTK_DTKID]    
          )
            return index ;
    }

    if( index >= CHECK_LIST_SIZE )
    {
        /* 
        -- over flow the array.  
        -- this should NEVER happen, more than 100 dtks.   
        -- typical is 1-3.  Maybe there could be 5 max 
        -- as far as we know now ( Wed Aug 13 17:55:41 PDT 1997 )
        */
        fprintf( stderr, "%s(%d):  %s\n", __FILE__, __LINE__, 
            DTKM_ERROR_MESSAGE( 
                DTKM_ERROR_INCREASE_CHECK_LIST_SIZE_in_dtkm_fix_dl2obs)  ) ;
        fprintf( stderr, "Need to re-compile and re-deliver.\n" ) ;

        return DTKM_ERROR_INCREASE_CHECK_LIST_SIZE_in_dtkm_fix_dl2obs ;
    }

    /* 
    -- dtk is not in the check list.  add it.  
    -- use index, which points to the next unused entry.  
    */
    dtk_check_list[index].obs_dtk_rec = dtk_rec ;
    return index ;

}



/*==============================================================================
Function:       dtkm_fix_dl2obs()

Description:    fix/update/repair/maintain, if necessary, dl2obs 
                records in the database

Creator:        Lawrence Stevens

Creation Date:  Wed Aug 13 11:36:51 PDT 1997

==============================================================================*/
int dtkm_fix_dl2obs( 
    DBPROCESS       *APS_dbproc,
    DB_RECORD       **downlink_dtk_rec,    /* input downlink dtk.    */

            /* 
            -- note:  either or both llist pointers could be NULL, 
            -- with the meaning (here) that there are no observations 
            -- in the input list.  
            */
    llist           *correct_obs_list, /* input correct obs list */
    llist           *dl2obs_obs_list ) /* input obs list according to dl2obs */
{
    int             return_code ;

    DB_RECORD       **dtk_rec = NULL ;
    cursor          dtk_list_ptr ;

    int             nrecs ;

    /* 
    -- make a check list with CHECK_LIST_SIZE rows, 
    -- and initialize every member of 
    -- every row to 0:
    -- ( A Book on C, Ch 9.5, p. 373 )
    */
    CHECK_LIST_ENTRY    dtk_check_list[CHECK_LIST_SIZE] = {0} ;
    int                 dtk_check_list_index = 0 ;

    /* quick error checking.  */

    if ( downlink_dtk_rec == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;

    if( dtkm_is_a_downlink( downlink_dtk_rec ) != TRUE )
        return DTKM_ERROR_DTK_IS_NOT_A_DOWNLINK ;

    /* 
    -- note:  in this routine, a NULL input list is 
    --        interpreted as a list with no members.  
    --        this is due to the routine dtkm_update_dtk(), 
    --        which calls this routine.  
    */
    if( correct_obs_list == NULL && dl2obs_obs_list == NULL )
    {
        /* both lists agree.  nothing to fix.  */
        return TRUE ;
    }

    /*
    -- populate the check list:
    */
    if( correct_obs_list )
    for (   dtk_rec = (DB_RECORD **) FIRST(correct_obs_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(correct_obs_list, dtk_list_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        return_code = dtkm_insert_dtk2checklist( dtk_rec, dtk_check_list ) ;
        if( return_code < 0 )
            return return_code ;
        dtk_check_list_index = return_code ;

        /* set flag.  */
        dtk_check_list[dtk_check_list_index].correct_flag = TRUE ;
    }
    if( dl2obs_obs_list )
    for (   dtk_rec = (DB_RECORD **) FIRST(dl2obs_obs_list, dtk_list_ptr);
            dtk_rec ;
            dtk_rec = (DB_RECORD **) NEXT(dl2obs_obs_list, dtk_list_ptr)  
        )
    {
        /* process the current dtk_rec right here.  */
        return_code = dtkm_insert_dtk2checklist( dtk_rec, dtk_check_list ) ;
        if( return_code < 0 )
            return return_code ;
        dtk_check_list_index = return_code ;

        /* set flag.  */
        dtk_check_list[dtk_check_list_index].dl2obs_flag = TRUE ;
    }

    /* 
    -- go through each entry in the check list, 
    -- which represents every dtk appearing on 
    -- one or both lists.  
    -- process each entry according to the flags
    --
    -- If the dtk is both lists, do nothing
    --
    -- If the dtk is in the correct list only, 
    -- then insert a new dl2obs record.  
    --
    -- if the dtk is in the dl2obs list only, 
    -- then delete the dl2obs record.  
    */
    for( 
        dtk_check_list_index = 0 ;
        dtk_check_list[dtk_check_list_index].obs_dtk_rec ;
        dtk_check_list_index ++ 
        )
    {
        if( dtk_check_list[dtk_check_list_index].correct_flag 
        &&  dtk_check_list[dtk_check_list_index].dl2obs_flag )
            continue ;

        if( dtk_check_list[dtk_check_list_index].correct_flag 
        &&  dtk_check_list[dtk_check_list_index].dl2obs_flag != TRUE )
        {
            /* 
            -- INSERT:
            -- obs_dtk_rec is IN the correct list, but not in dl2obs list, 
            -- INSERT new rec into dl2obs relation, linking 
            -- obs_dtk_rec to the downlink.  
            */
            return_code = dtkm_insert_dl2obs_rec( APS_dbproc,
                dtk_check_list[dtk_check_list_index].obs_dtk_rec,
                downlink_dtk_rec ) ;
            if ( return_code < 0 )
                return return_code ;

            continue ;
        }

        if( dtk_check_list[dtk_check_list_index].correct_flag != TRUE
        &&  dtk_check_list[dtk_check_list_index].dl2obs_flag )
        {
            /* 
            -- DELETE:
            -- rec is NOT in correct list, but is in dl2obs list, 
            -- delete from dl2obs relation.  
            -- use the sat value from the input downlink 
            -- and the others from the checklist
            -- to identify the dl2obs record to delete:
            */
            sprintf( where_clause, "where %s = '%s' and %s = %ld and %s = %d",
                APS_COL(DL2OBS, DL2OBS_SAT),       
                         CAST_DTK_SAT   downlink_dtk_rec[DTK_SAT],
                APS_COL(DL2OBS, DL2OBS_REV_OBS),   
                         CAST_DTK_REV 
                         (dtk_check_list[dtk_check_list_index].obs_dtk_rec)
                         [DTK_REV],
                APS_COL(DL2OBS, DL2OBS_DTKID_OBS), 
                         CAST_DTK_DTKID 
                         (dtk_check_list[dtk_check_list_index].obs_dtk_rec)
                         [DTK_DTKID]  ) ;
            nrecs = db_delete_records( APS_dbproc,
                APS_TABLE(DL2OBS), where_clause ) ;
            if( nrecs < 0 )
                return DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION ;
        }

    }  /* end of for-loop on check list.   */

    return TRUE ;

}
