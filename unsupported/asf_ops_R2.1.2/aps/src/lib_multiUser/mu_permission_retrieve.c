#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_permission_retrieve.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_permission_retrieve.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_permission_retrieve.c"


/*==============================================================================
Function:       mu_permission_retrieve()

Description:    For each activity_type, read and validate every record 
                using the Sybase pseudo relation sysprocesses.  Then 
                return the valid records to the calling module.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 22:38:01 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#include <string.h>           /* for strlen(), strcmp()   */
#include "mu.h"

#include <db_multi_user_wait.h>

int mu_permission_retrieve(
    DBPROCESS   *dbproc,       /*  input sybase session pointer              */
    llist       *planning_activity_list, 
                                /* output linked list for planning activity 
                                   permissions. used only if valid planning 
                                   permissions exist.  
                                   calling routine must allocate the list 
                                   and it must have no members at input.    */

    llist       *dar_activity_list, 
                                /* output linked list for DAR activity 
                                   permissions. used only if valid DAR 
                                   permissions exist.  
                                   calling routine must allocate the list 
                                   and it must have no members at input.    */

    llist       *single_activity_list ) 
                                /* output linked list for single activity 
                                   permissions. used only if valid single 
                                   permissions exist.  
                                   calling routine must allocate the list 
                                   and it must have no members at input.    */

{
    llist       *planning_perm_list = NULL ;
    llist       *dar_perm_list = NULL ;
    llist       *single_perm_list = NULL ;

    llist       *list_check = NULL ;

    int         total_count = 0 ;
    int         return_code ;
    int         rollback_return_code ;
    int         nrecs ;

    /* brief error checking   */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;

    if( planning_activity_list == NULL )
        return MU_ERROR_INPUT_PLANNING_ACTIVITY_LIST_IS_NULL_PTR ;
    if( NUMELTS( planning_activity_list ) != 0 )
        return MU_ERROR_INPUT_PLANNING_ACTIVITY_LIST_IS_NOT_EMPTY ;

    if( dar_activity_list == NULL )
        return MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NULL_PTR ;
    if( NUMELTS( dar_activity_list ) != 0 )
        return MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NOT_EMPTY ;

    if( single_activity_list == NULL )
        return MU_ERROR_INPUT_SINGLE_ACTIVITY_LIST_IS_NULL_PTR ;
    if( NUMELTS( single_activity_list ) != 0 )
        return MU_ERROR_INPUT_SINGLE_ACTIVITY_LIST_IS_NOT_EMPTY ;

    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 0.1.  begin transaction                              *
    *                                                               *
    *                                                               *
    ****************************************************************/
    return_code = db_begin_tran( dbproc ) ;
    if( return_code != TRUE )
        return MU_ERROR_DURING_BEGIN_TRANSACTION ;
 
    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 0.2.  update the multi_user_wait table.              *
    *                                                               *
    *                                                               *
    ****************************************************************/
    /*
    --  This will cause the  Sybase server to create an exclusive
    --  lock on this table for the current process.  When this
    --  step is completed, the exclusive lock is
    --  maintained until the commit transaction step below.
    --  NOTE:  Sybase will queue up the permission requests at
    --  this point on a first-come-first-serve basis.
    */
    (void) sprintf( fields_to_set, "%s = '%s'",
        APS_COL( MULTI_USER_WAIT, MULTI_USER_WAIT_STATUS ), "IN_PROGRESS" ) ;
    nrecs = db_update_records( dbproc, APS_TABLE(MULTI_USER_WAIT),
        fields_to_set, NULL ) ;
    if( nrecs != 1 )
    {
        /*
        -- ERROR.   rollback transaction now.  
        --          alow nrecs to determine actual return code.  
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;

        if( nrecs < 0 )
             return MU_DB_ERROR_UPDATING_MULTI_USER_WAIT ;
        if( nrecs == 0 )
            return MU_ERROR_NO_MULTI_USER_WAIT_REC_UPDATED ;
        if( nrecs > 1 )
             return MU_ERROR_MORE_THAN_ONE_MULTI_USER_WAIT_REC_UPDATED ;
    }

    /**************************************************************************
    *                                                                         *
    *    Permission Retrieval (and validation) Algorithm:                     *
    *                                                                         *
    *    1. Retrieve records from the active_planning_activities table.       *
    *                                                                         *
    *    2. Retrieve records from the active_dar_activities table.            *
    *                                                                         *
    *    3. Retrieve records from the active_single_activities table.         *
    *                                                                         *
    *    4. If any permission records are found from Steps (1-3),             *
    *       verify that each record's requesting process is still active      *
    *       as follows:                                                       *
    *                                                                         *
    *       Count records from the sysprocesses table which have the          *
    *       same node, process_id, kpid, and command_name as the              *
    *       permission record being validated.                                *
    *                                                                         *
    *       If any sysprocesses record is found, then the requesting          *
    *       process is still active and the permission record is valid.       *
    *                                                                         *
    *       If not, the requesting process is dead and the permission         *
    *       is not valid.                                                     *
    *                                                                         *
    *   5.  Steps (1-3) records that failed validation in Step (4) are        *
    *       deleted from the database and also removed from the list of       *
    *       records to return.                                                *
    *                                                                         *
    *   6.  The remaining records are considered valid and are returned       *
    *       to the calling module with a count of the total number            *
    *       records.  They will be returned via three pointers to linked      *
    *       lists, one linked list for each table.                            *
    *                                                                         *
    **************************************************************************/
         

    /**************************************************************************
    *                                                                         *
    *     STEP 1. Retrieve records from active_planning_activities table.     *
    *                                                                         *
    **************************************************************************/
    planning_perm_list = db_get_records( dbproc, 
        APS_TABLE(ACTIVE_PLANNING_ACTIVITIES),
        NULL, NULL, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES), ALL_COLS) ;
    if( planning_perm_list == NULL )
    {
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_PLANNING_ACTIVITIES_TABLE ;
    }

    /**************************************************************************
    *                                                                         *
    *     STEP 2. Retrieve records from active_dar_activities table.          *
    *                                                                         *
    **************************************************************************/
    dar_perm_list = db_get_records( dbproc, 
        APS_TABLE(ACTIVE_DAR_ACTIVITIES),
        NULL, NULL, APS_CDEFS(ACTIVE_DAR_ACTIVITIES), ALL_COLS) ;
    if( dar_perm_list == NULL )
    {
        DEL_LIST( planning_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_DAR_ACTIVITIES_TABLE ;
    }

    /**************************************************************************
    *                                                                         *
    *     STEP 3. Retrieve records from active_single_activities table.       *
    *                                                                         *
    **************************************************************************/
    single_perm_list = db_get_records( dbproc, 
        APS_TABLE(ACTIVE_SINGLE_ACTIVITIES),
        NULL, NULL, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES), ALL_COLS) ;
    if( single_perm_list == NULL )
    {
        DEL_LIST( planning_perm_list ) ;
        DEL_LIST( dar_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_SINGLE_ACTIVITIES_TABLE ;
    }

    /**************************************************************************
    *                                                                         *
    *     STEP 4. If any permission records are found from Steps (1-3),       *
    *             verify that each record's requesting process is still       *
    *             active.                                                     *
    *                                                                         *
    **************************************************************************/
    /**************************************************************************
    *                                                                         *
    *     STEP 5. Steps (1-3) records that failed verification in Step (4)    *
    *             are deleted from the database and also removed from the     *
    *             list of records to return.                                  *
    *                                                                         *
    **************************************************************************/
    /*
    -- both steps 4 and 5 are done with
    -- mu_verify_planning_perms(), etc:
    */
    return_code = mu_verify_planning_perms( dbproc, planning_perm_list ) ;
    if( return_code < 0 )
    {
        DEL_LIST( planning_perm_list ) ;
        DEL_LIST( dar_perm_list ) ;
        DEL_LIST( single_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return return_code ;
    }

    return_code = mu_verify_dar_perms( dbproc, dar_perm_list ) ;
    if( return_code < 0 )
    {
        DEL_LIST( planning_perm_list ) ;
        DEL_LIST( dar_perm_list ) ;
        DEL_LIST( single_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return return_code ;
    }

    return_code = mu_verify_single_perms( dbproc, single_perm_list ) ;
    if( return_code < 0 )
    {
        DEL_LIST( planning_perm_list ) ;
        DEL_LIST( dar_perm_list ) ;
        DEL_LIST( single_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return return_code ;
    }

    /**************************************************************************
    *                                                                         *
    *     STEP 6. The remaining records are considered valid and are          *
    *             returned to the calling module with a count of the total    *
    *             number of records.                                          *
    *                                                                         *
    **************************************************************************/
    /*
    -- move the permissions from planning_perm_list
    -- to the output planning_activity_list. 
    */
    list_check = db_record_llist_move( planning_activity_list, 
        planning_perm_list ) ;
    if( list_check != planning_activity_list )
    {
        DEL_LIST( planning_perm_list ) ;
        DEL_LIST( dar_perm_list ) ;
        DEL_LIST( single_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return MU_ERROR_IN_MOVING_RECS_TO_LLIST ;
    }
    DEL_LIST( planning_perm_list ) ;

    /*
    -- move the permissions from dar_perm_list
    -- to the output dar_activity_list. 
    */
    list_check = db_record_llist_move( dar_activity_list, 
        dar_perm_list ) ;
    if( list_check != dar_activity_list )
    {
        DEL_LIST( dar_perm_list ) ;
        DEL_LIST( single_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return MU_ERROR_IN_MOVING_RECS_TO_LLIST ;
    }

    DEL_LIST( dar_perm_list ) ;

    /*
    -- move the permissions from single_perm_list
    -- to the output single_activity_list. 
    */
    list_check = db_record_llist_move( single_activity_list, 
        single_perm_list ) ;
    if( list_check != single_activity_list )
    {
        DEL_LIST( single_perm_list ) ;
        /*
        -- ERROR.  rollback transaction and return.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
 
        return MU_ERROR_IN_MOVING_RECS_TO_LLIST ;
    }

    DEL_LIST( single_perm_list ) ;

    total_count = NUMELTS( planning_activity_list ) 
                + NUMELTS( dar_activity_list ) 
                + NUMELTS( single_activity_list ) ;

    /*
    -- no error.  commit transaction.
    */
    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP Z.  Commit Transaction.                            *
    *                                                               *
    *                                                               *
    ****************************************************************/
    /*
    -- Step Z. commit transaction.  This will cause the
    --         Sybase server to release the exclusive lock held
    --         since Step 0.2 and allow the next action in the Sybase
    --         queue to proceed.
    */
    return_code = db_commit_tran( dbproc ) ;
    if( return_code != TRUE )
        return MU_ERROR_DURING_COMMIT_TRANSACTION ;

    return total_count ;

}
