#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_permission_terminate.c

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_permission_terminate.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_permission_terminate.c"


/*==============================================================================
Function:       mu_permission_terminate()

Description:    terminate a supposedly existing permission for a task 
                or activity to proceed.  
                only a permission belonging to the current process 
                can be terminated.  

Returns:        permission_id or 0 or < 0 if error.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 22:38:01 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include <string.h>           /* for strlen(), strcmp()   */
#include "mu.h"

#include "db_multi_user_wait.h"
#include "db_active_dar_activities.h"
#include "db_active_planning_activities.h"
#include "db_active_single_activities.h"

int mu_permission_terminate(
    DBPROCESS   *dbproc,        /* input sybase session pointer             */
    int         permission_id,  /* input permission id to be terminated     */
    char        *mu_activity_id,/* input activity for which permission is  
                                   terminated.                              */
    char        *activity_type) /* input activity type for which permission is  
                                   terminated.  values = "planning", "DAR",
                                   or "single"                              */
{
    int     return_code ;
    int     rollback_return_code ;
    int     n_deleted_recs = 0 ;
    int     nrecs ;

    int     kpid;            /* current kernel process id in Sybase      */
    char    hostname[12] ;   /* current nodename.  allow 11 chars */
    char    progname[18] ;   /* current programe name.  allow 17 char*/
    char    process_id[10] ; /* current process id, 9 chars       */

    /* brief error checking   */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;
 
    if( permission_id < 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO ;
    if( permission_id == 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_ZERO ;

    return_code = mu_validate_activity_id( activity_type, mu_activity_id ) ;
    if( return_code < 0 )
        return return_code ;

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
        -- ERROR.  rollback transaction.  
        --         allow the nrecs value 
        --         to determine the actual exit code 
        --         in the following statements.  
        --         do not exit here.  
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

    /*
    -- get the info from sysprocesses that will 
    -- uniquely identify our process.  This is part 
    -- of making sure that we only delete our own 
    -- permissions:
    */
    return_code = mu_get_sysprocesses_khph( dbproc,
        &kpid, hostname, progname, process_id ) ;
    if( return_code < 0 )
    {
        /*
        -- ERROR.  rollback transaction.  
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
        return return_code ;
    }

    if(strcmp(activity_type, MU_PLANNING_ACTIVITY_TYPE ) == 0 )
    {
        (void) sprintf( where_clause, 
"where %s = %d and %s = '%s' and %s = %d and %s = '%s' and %s = '%s' and %s = '%s' ",

            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_PERMISSION_ID),
            permission_id, 

            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID),
            mu_activity_id,

            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_KPID),
            kpid,

            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_NODE),
            hostname,

            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME),
            progname,

            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID),
            process_id ) ;

        n_deleted_recs = db_delete_records( dbproc,
            APS_TABLE(ACTIVE_PLANNING_ACTIVITIES), where_clause ) ;
        if( n_deleted_recs < 0 || n_deleted_recs > 1 )
        {
            /*
            -- ERROR.  rollback transaction.  
            --         allow the n_deleted_recs value 
            --         to determine the actual exit code
            --         in the following statements.  
            --         do not exit here.  
            */
            rollback_return_code = db_rollback_tran( dbproc ) ;
            if( rollback_return_code != TRUE )
                return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
        }
        if( n_deleted_recs < 0 )
            return MU_DB_ERROR_DELETING_FROM_ACTIVE_PLANNING_ACTIVITIES ;
        if( n_deleted_recs > 1 )
            return MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_PLANNING_ACTIVITIES ;

    }

    if(strcmp(activity_type, MU_DAR_ACTIVITY_TYPE ) == 0 )
    {
        (void) sprintf( where_clause,
"where %s = %d and %s = '%s' and %s = %d and %s = '%s' and %s = '%s' and %s = '%s' ",
            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_PERMISSION_ID),
            permission_id, 
            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID),
            mu_activity_id,

            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_KPID),
            kpid,

            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_NODE),
            hostname,

            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_COMMAND_NAME),
            progname,

            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_PROCESS_ID),
            process_id ) ;

        n_deleted_recs = db_delete_records( dbproc,
            APS_TABLE(ACTIVE_DAR_ACTIVITIES), where_clause ) ;
        if( n_deleted_recs < 0 || n_deleted_recs > 1 )
        {
            /*
            -- ERROR.  rollback transaction.  
            --         allow the n_deleted_recs value 
            --         to determine the actual exit code
            --         in the following statements.  
            --         do not exit here.  
            */
            rollback_return_code = db_rollback_tran( dbproc ) ;
            if( rollback_return_code != TRUE )
                return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
        }
        if( n_deleted_recs < 0 )
            return MU_DB_ERROR_DELETING_FROM_ACTIVE_DAR_ACTIVITIES ;
        if( n_deleted_recs > 1 )
            return MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_DAR_ACTIVITIES ;

    }
 
    if(strcmp(activity_type, MU_SINGLE_ACTIVITY_TYPE ) == 0 )
    {
        (void) sprintf( where_clause, 
"where %s = %d and %s = '%s' and %s = %d and %s = '%s' and %s = '%s' and %s = '%s' ",
            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_PERMISSION_ID),
            permission_id, 
            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_MU_ACTIVITY_ID),
            mu_activity_id,

            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_KPID),
            kpid,

            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_NODE),
            hostname,

            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_COMMAND_NAME),
            progname,

            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_PROCESS_ID),
            process_id ) ;

        n_deleted_recs = db_delete_records( dbproc,
            APS_TABLE(ACTIVE_SINGLE_ACTIVITIES), where_clause ) ;
        if( n_deleted_recs < 0 || n_deleted_recs > 1 )
        {
            /*
            -- ERROR.  rollback transaction.  
            --         allow the n_deleted_recs value 
            --         to determine the actual exit code
            --         in the following statements.  
            --         do not exit here.  
            */
            rollback_return_code = db_rollback_tran( dbproc ) ;
            if( rollback_return_code != TRUE )
                return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;
        }
        if( n_deleted_recs < 0 )
            return MU_DB_ERROR_DELETING_FROM_ACTIVE_SINGLE_ACTIVITIES ;
        if( n_deleted_recs > 1 )
            return MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_SINGLE_ACTIVITIES ;
    }

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
    --         since Step 0.2 and allow the next request in the Sybase
    --         queue to proceed.
    */
    return_code = db_commit_tran( dbproc ) ;
    if( return_code != TRUE )
        return MU_ERROR_DURING_COMMIT_TRANSACTION ;

    if( n_deleted_recs == 1 )
    {
        /* 
        -- returning with the permission 
        -- id value to indicate success.  
        */
        return permission_id ;
    }
    if( n_deleted_recs == 0 )
    {
        /* 
        -- returning with zero to
        -- indicate that no records were deleeted.  
        */
        return 0 ;
    }

    /*
    -- this should never happen; n_deleted_recs is 
    -- either 1 or zero at this point.
    -- other cases were handled above.  
    */
    (void) printf(
        "%s(%d):  error in code; review recent modifications of APS.  \n",
        __FILE__, __LINE__ ) ;
    return MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE ;

}
