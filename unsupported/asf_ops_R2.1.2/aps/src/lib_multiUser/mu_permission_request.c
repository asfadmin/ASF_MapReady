#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_permission_request.c

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
#pragma ident   "@(#)mu_permission_request.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_permission_request.c"

#include <string.h>           /* for strlen(), strcmp()   */

#include "mu.h"

/* multi-user database tables:  */
#include <db_multi_user_wait.h>
#include <db_active_dar_activities.h>
#include <db_active_single_activities.h>
#include <db_active_planning_activities.h>
#include <db_permission_counter.h>


/*==============================================================================
Function:       mu_permission_request()

Description:    asks for permission for a task or activity to proceed.  
                Part of Multi-user Capability.

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 22:38:01 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int mu_permission_request(
    DBPROCESS   *dbproc,       /*  input sybase session pointer              */
    int         permission_id, /*  input permission id, usually = 0          */
    char        *mu_activity_id,/* input activity for which permission is  
                                   requested.                                */
    char        *activity_type,/*  input activity type for which permission is  
                                   requested.  values = "planning", "DAR",
                                   or "single"                               */
    llist       *blocking_permission_list, 
                                /* output linked list for blocking permissions. 
                                   used only if permission is denied.  calling
                                   routine must allocate the list and it must 
                                   have no members at input.                 */

    /* Other parameters.   use NULL, "", or a 0 value if not used.  */
    char        *strttime,  /* for planning activities, input start time ASF  */
    char        *stoptime,  /* for planning activities, input stop time ASF   */
    char        *station_id,/* for planning activities, input station ID, "ASF",
                                  "MCM", or "ALL"                            */
    int         darid )     /* for DAR activities, the darid of the DAR.     */
{

    int         rollback_return_code ;
    int         return_code ;
    int         nrecs ;

    int         return_permission_code ;


    /* brief error checking   */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;

    if( permission_id < 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO ;

    /* 
    -- validate both activity_type and 
    -- activity_id 
    */
    return_code = mu_validate_activity_id( activity_type, mu_activity_id ) ;
    if( return_code < 0 )
        return return_code ;

    if( blocking_permission_list == NULL )
        return MU_ERROR_INPUT_BLOCKING_PERMISSION_LIST_IS_NULL_PTR ;
    if( NUMELTS( blocking_permission_list ) != 0 )
        return MU_ERROR_INPUT_BLOCKING_PERMISSION_LIST_IS_NOT_EMPTY ;


    /* **********************************************************************
    --  
    --  INPUT ARGUMENTS:
    --  1.  APS_dbproc  pointer 
    --  This is a pointer to the current Sybase database session information.  
    --  The calling program obtains this pointer when it opens a database 
    --  session on the APS database.  
    --
    --  2.  permission_id   int 
    --  If = 0, this indicates that a new permission_id will be created if 
    --  permission is granted.  
    --  If != 0, this indicates an existing permission_id.  If permission is 
    --  granted, this permission_id will be used for the new permission; a 
    --  new permission_id will not be created.  
    --
    --  3.  mu_activity_id  char    
    --  values:  a string which identifies the APS activity for which 
    --  permission is being requested.  This APS activity is about to begin 
    --  and will proceed if the permission is granted.  
    --  If the Permission Request module is called by the APS GUI on 
    --  behalf of an APS activity, mu_activity_id identifies the activity, not 
    --  the APS GUI.   
    --
    --  4.  activity_type   char    
    --  Three possible values:  "planning", "DAR", or "single"
    --
    --  5.  Other parameters.  
    --  Depending on the activity_type, other parameters are required 
    --  as follows:
    --
    --  If activity_type = planning
    --  a.  strttime    char    start time of the planning time bracket.  
    --  b.  stoptime    char    stop time of the planning time bracket.  
    --      Note:  The Permission Request module will pad this strttime/
    --      stoptime time bracket with the value computed as described in 
    --      Section 3.1.3.1 Planning Activities Data, page 11.  This 
    --      value, in minutes, will be subtracted from strttime and also 
    --      added to stoptime to enlarge the time bracket.  This 
    --      "enlarged time bracket" for the input activity is used 
    --      below by the Permission Request module.  
    --  c.  station_id  char    
    --      value = "ASF" for planning at ASF, "MCM" for planning at 
    --      McMurdo, and "ALL" for planning at both ground stations.  
    --
    --  If activity_type = DAR
    --  a.  darid   int DAR id of the DAR for which work is being done.  
    --      If activity_type = single, then no extra parameters are required.  
    --  
    ***********************************************************************/


    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 1.  begin transaction                              *
    *                                                               *
    *                                                               *
    ****************************************************************/
    return_code = db_begin_tran( dbproc ) ;
    if( return_code != TRUE )
        return MU_ERROR_DURING_BEGIN_TRANSACTION ;

    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 2.  update the multi_user_wait table.              *
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

    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEPS 3-7.  Perform the activity type related steps.    *
    *                                                               *
    *                                                               *
    ****************************************************************/

    if(strcmp(activity_type, MU_PLANNING_ACTIVITY_TYPE ) == 0 )
    {
        return_permission_code = mu_planning_activity_request( dbproc, 
            permission_id, mu_activity_id, strttime, stoptime, station_id,
            blocking_permission_list ) ;
    }
    else if(strcmp(activity_type, MU_DAR_ACTIVITY_TYPE ) == 0 )
    {
        return_permission_code = mu_dar_activity_request( dbproc, 
            permission_id, mu_activity_id, darid,
            blocking_permission_list ) ;
    }
    else if(strcmp(activity_type, MU_SINGLE_ACTIVITY_TYPE ) == 0 )
    {
        return_permission_code = mu_single_activity_request(dbproc, 
            permission_id, mu_activity_id, 
            blocking_permission_list );
    }
    else
    {
        /*
        -- all cases should have been handled above.
        */
        (void) printf(
            "%s(%d):  error in code; review recent modifications of APS.\n",
            __FILE__, __LINE__ ) ;
        /*
        -- ERROR.  must rollback transaction.
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;

        return MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE ;
    }
    /* error check.  */
    if( return_permission_code < 0 )
    {
        /* 
        -- ERROR.  rollback transaction and exit. 
        */
        rollback_return_code = db_rollback_tran( dbproc ) ;
        if( rollback_return_code != TRUE )
            return MU_ERROR_DURING_ROLLBACK_TRANSACTION ;

        /* now return the retained return code.  */
        return return_permission_code ;
    }

    /* 
    -- no error.  commit transaction.  
    -- use the return_permission_code from the above 
    -- call in Step 9.  
    */

    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 8.  Commit Transaction.                            *
    *                                                               *
    *                                                               *
    ****************************************************************/

    /*
    -- Step 8. commit transaction.  This will cause the 
    --         Sybase server to release the exclusive lock held 
    --         since Step 2 and allow the next request in the Sybase 
    --         queue to proceed.  
    */
    return_code = db_commit_tran( dbproc ) ;
    if( return_code != TRUE )
        return MU_ERROR_DURING_COMMIT_TRANSACTION ;

    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 9.  Return to the calling program.                 *
    *                                                               *
    *                                                               *
    ****************************************************************/

    /*
    -- Step 9. Return to the calling program as follows:
    --         If permission_id = 0, indicating rejection, 
    --             return the blocking permission records, with 
    --             return code = 0.  
    --         If permission_id != 0, indicating approval, 
    --             return with return_code = permission_id.  
    */

    /* 
    -- use the return_permission_code from above.  
    -- the blocking permission records, if any, are in the 
    -- linked list.  Just return with return_permission_code.  
    */
    return return_permission_code ;

}
