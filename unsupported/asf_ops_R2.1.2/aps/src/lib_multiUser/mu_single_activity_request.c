#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_single_activity_request.c

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
#pragma ident   "@(#)mu_single_activity_request.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_single_activity_request.c"


/*==============================================================================
Function:       mu_single_activity_request()

Description:    asks for permission for a task or activity to proceed.  
                Part of Multi-user Capability.

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 22:38:01 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include <string.h>           /* for strlen(), strcmp()   */
#include <stdio.h>            /* for cuserid()            */

#include "mu.h"

/* multi-user database tables:  */
#include <db_active_single_activities.h>
#include <db_permission_counter.h>


int mu_single_activity_request(
    DBPROCESS   *dbproc,       /*  input sybase session pointer              */
    int         permission_id, /*  input permission id, usually = 0          */
    char        *mu_activity_id,/* input mu activity for which permission is  
                                   requested.                                */
    llist       *blocking_permission_list ) 
                               /*  output linked list for blocking permissions. 
                                   used only if permission is denied.  calling
                                   routine must allocate the list and it must 
                                   have no members at input.                 */
{

    int         return_code ;
    int         result_permission_id ;

    /* permission storage.  */
    llist           *perm_list = NULL ;
    llist           *list_check = NULL ;


    /* brief error checking   */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;

    if( permission_id < 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO ;

    /* 
    -- validate mu_activity_id 
    */
    return_code = mu_validate_activity_id( MU_SINGLE_ACTIVITY_TYPE, 
        mu_activity_id ) ;
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
    ***********************************************************************/


    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 3.  According to activity_type, retrieve the       *
    *                appropriate blocking permission records.       *
    *                                                               *
    *                                                               *
    ****************************************************************/
    /*
    --  NOTE:   Retain a list of these blocking permission records for 
    --          use in steps below.     
    */

    /*
    --  activity_type = single:
    --      Retrieve records from the active_single_activities table
    --      which have the same mu_activity_id as the input parameter.
    */
    /*
    -- retrieve only single activities with the
    -- same mu_activity_id.
    */
    (void) sprintf(where_clause, "where %s = '%s' ",
        APS_COL(ACTIVE_SINGLE_ACTIVITIES,
            ACTIVE_SINGLE_ACTIVITIES_MU_ACTIVITY_ID), mu_activity_id ) ;
    perm_list = db_get_records( dbproc, APS_TABLE(ACTIVE_SINGLE_ACTIVITIES),
        where_clause, NULL, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES),
        ALL_COLS) ;
    if( perm_list == NULL )
        return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_SINGLE_ACTIVITIES_TABLE ;

#ifdef PRINT_DIAG
    (void) printf("\n%s(%d):  perm_list:  \n", __FILE__, __LINE__ ) ;
    db_print_list( perm_list, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES) ) ;
    (void) printf("\n" ) ;
#endif /* PRINT_DIAG  */
    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 4.  If any blocking permission records are         *
    *                found from Step (3), verify each record's      *
    *                requesting process is still active.            *
    *                                                               *
    *                                                               *
    ****************************************************************/

    /*
    -- If any blocking permission records are found from Step (3), 
    -- verify that each record's requesting process is still active, 
    -- as follows:  
    -- Retrieve records from the sysprocesses table which have the 
    -- same node, process_id, kpid, and command_name as the blocking 
    -- permission record being validated.  
    -- If any sysprocesses record is found, then the requesting process 
    -- is still active.  
    -- If not, the requesting process is dead and the blocking permission 
    -- is not valid.  
    */

    /****************************************************************
    *                                                               *
    *                                                               *
    *       STEP 5.  Step (4) records that are not valid are        *
    *                deleted from the database and also removed     *
    *                from the list of blocking permission records   *
    *                from Step (3).                                 *
    *                                                               *
    *                                                               *
    ****************************************************************/

    /* 
    -- both steps 4 and 5 are done with 
    -- mu_verify_single_perms():  
    */
    return_code = mu_verify_single_perms( dbproc, perm_list ) ;
    if( return_code < 0 )
    {
        DEL_LIST( perm_list ) ;
        return return_code ;
    }

    if( NUMELTS( perm_list ) > 0 )
    {
        /****************************************************************
        *                                                               *
        *                                                               *
        *       STEP 6.  If any blocking permission records remain,     *
        *                then set permission_id = 0                     *
        *                                                               *
        *                                                               *
        ****************************************************************/
        /* 
        -- permission denied due to blocking activities.  
        */
        result_permission_id = 0 ;

        /* 
        -- move the blocking permissions from perm_list 
        -- to the output permissions list (blocking_permission_list).  
        */
        list_check = db_record_llist_move( blocking_permission_list, 
            perm_list ) ;
        if( list_check != blocking_permission_list )
        {
            DEL_LIST( perm_list ) ;
            return MU_ERROR_IN_MOVING_RECS_TO_LLIST ;
        }
        DEL_LIST( perm_list ) ;
    }
    else
    {
        /****************************************************************
        *                                                               *
        *                                                               *
        *       STEP 7.  If no blocking permission records remain,      *
        *                permission is granted.                         *
        *                there are several steps here...                *
        *                                                               *
        ****************************************************************/
        DEL_LIST( perm_list ) ;
        /* permission is granted.  */
        if( permission_id > 0 )
            /*
            -- a.  if the input permission_id != 0, set the 
            --      permission_id to that value.  
            */
            result_permission_id = permission_id ;
        else
        {
            /*
            --  b.  if the input permission_id = 0, then set the 
            --      permission_id using the permission_counter 
            --      relation as follows:  Increment 
            --      permission_counter.permission_id, then retrieve 
            --      permission_counter.permission_id and use that value 
            --      for permission_id.  
            */
            return_code = mu_get_new_permission_id( dbproc ) ;
            if( return_code < 0 )
                return return_code ;
            result_permission_id = return_code ;
        }
        /*
        -- c.   append a new record to the database table, corresponding to 
        --      activity_type, that was read in Step (3).  
        --      Use the mu_activity_id  from the input parameters, use the 
        --      permission_id value from the previous step; 
        --      use the node, command_name, process_id, and kpid 
        --      values obtained from the Sybase pseudo-relation sysprocesses; 
        --      obtain the userid as described in Step (3).   
        */
        return_code = mu_insert_single_perm( dbproc,
            mu_activity_id, result_permission_id ) ;
        if( return_code < 0 )
            return return_code ;
    }

    return result_permission_id ;

}
