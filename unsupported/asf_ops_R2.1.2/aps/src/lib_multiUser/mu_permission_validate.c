#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_permission_validate.c

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_permission_validate.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_permission_validate.c"


/*==============================================================================
Function:       mu_permission_validate()

Description:    validates a supposedly existing permission for a task 
                or activity to proceed.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 22:38:01 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

This module is only invoked by an APS module that has received 
a permission_id, via the command line, from the APS GUI.  The 
APS module must validate it by calling the Permission validation 
module.  This validation is mainly a retrieval to see if the 
permission record exists.  

Input data for this module:
    Note:  Since this module only reads from the database, it will 
    use the "aps_reader" account; no pointer to a Sybase session 
    needs to be passed as input data.  

1.  permission_id   int     The permission_id to be validated.    

2.  mu_activity_id  char    values:  a string which identifies the 
                            APS activity for which permission is 
                            being validated.  This APS activity is 
                            about to begin and will proceed if the 
                            permission is validated.    

3.  activity_type   char    values:  planning, DAR, or single

==============================================================================*/
#include <string.h>           /* for strlen(), strcmp()   */
#include "mu.h"               /* for most mu_ routines.  */

#include "db_active_dar_activities.h"
#include "db_active_planning_activities.h"
#include "db_active_single_activities.h"


int mu_permission_validate(
    int     permission_id,   /*  input permission id to be validated       */
    char    *mu_activity_id, /*  input activity for which permission is  
                                 validated.                                */
    char    *activity_type)  /*  input activity type for which permission is  
                                 validated.  values = "planning", "DAR",
                                 or "single"                               */
{

    int     return_code ;

    /* 
    -- for counting the number of sysprocesses records
    */
    int     syscount = 0 ;

    DB_RECORD   **perm_rec ;
    llist       *perm_list ;
    cursor      perm_list_ptr ;

    /* 
    -- values to match a sysprocesses record.  
    */
    char    node[50] ;
    char    process_id[50] ;
    int     kpid ;
    char    command_name[50] ;

    /* brief error checking   */
    if( permission_id < 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO ;
    if( permission_id == 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_ZERO ;

    return_code = mu_validate_activity_id( activity_type, mu_activity_id ) ;
    if( return_code < 0 )
        return return_code ;
 
    /**********************************************************************
    *                                                                     *
    *     STEP 1:  According to activity_type, retrieve the appropriate   *
    *              data.                                                  *
    *                                                                     *
    **********************************************************************/
    if(strcmp(activity_type, MU_PLANNING_ACTIVITY_TYPE ) == 0 )
    {
        /*
        --  If activity_type = planning:  
        --      Retrieve records from the active_planning_activities table 
        --      which have the same permission_id value and mu_activity_id 
        --      value.  
        */
        (void) sprintf( where_clause, "where %s = %d and %s = '%s' ",
            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_PERMISSION_ID),
            permission_id,
            APS_COL(ACTIVE_PLANNING_ACTIVITIES,
                ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID),
            mu_activity_id ) ;

        perm_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(ACTIVE_PLANNING_ACTIVITIES),
            where_clause, NULL, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES),
            ALL_COLS) ;
        if( perm_list == NULL )
            return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_PLANNING_ACTIVITIES_TABLE;
        if( NUMELTS(perm_list) == 1 )
        {
            /* collect 4 values for later use.  */
            perm_rec = (DB_RECORD **) FIRST( perm_list, perm_list_ptr ) ;
            (void) strcpy( node, 
                CAST_ACTIVE_PLANNING_ACTIVITIES_NODE 
                    perm_rec[ACTIVE_PLANNING_ACTIVITIES_NODE] ) ;
            (void) strcpy( process_id, 
                CAST_ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID 
                    perm_rec[ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID] ) ;
            (void) strcpy( command_name, 
                CAST_ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME 
                    perm_rec[ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME] ) ;
            kpid = CAST_ACTIVE_PLANNING_ACTIVITIES_KPID 
                    perm_rec[ACTIVE_PLANNING_ACTIVITIES_KPID] ;
        }
 
    }  /*   MU_PLANNING_ACTIVITY_TYPE    */
    else if(strcmp(activity_type, MU_DAR_ACTIVITY_TYPE ) == 0 )
    {
        /*
        --  If activity_type = DAR:
        --      Retrieve records from the active_dar_activities table which 
        --      have the same permission_id value and mu_activity_id value.  
        */
        (void) sprintf( where_clause, "where %s = %d and %s = '%s' ",
            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_PERMISSION_ID),
            permission_id,
            APS_COL(ACTIVE_DAR_ACTIVITIES,
                ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID),
            mu_activity_id ) ;

        perm_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(ACTIVE_DAR_ACTIVITIES),
            where_clause, NULL, APS_CDEFS(ACTIVE_DAR_ACTIVITIES),
            ALL_COLS) ;
        if( perm_list == NULL )
            return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_DAR_ACTIVITIES_TABLE ;
        if( NUMELTS(perm_list) == 1 )
        {
            /* collect 4 values for later use.  */
            perm_rec = (DB_RECORD **) FIRST( perm_list, perm_list_ptr ) ;
            (void) strcpy( node, 
                CAST_ACTIVE_DAR_ACTIVITIES_NODE 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_NODE] ) ;
            (void) strcpy( process_id, 
                CAST_ACTIVE_DAR_ACTIVITIES_PROCESS_ID 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_PROCESS_ID] ) ;
            (void) strcpy( command_name, 
                CAST_ACTIVE_DAR_ACTIVITIES_COMMAND_NAME 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_COMMAND_NAME] ) ;
            kpid = CAST_ACTIVE_DAR_ACTIVITIES_KPID 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_KPID] ;
        }
 
    }  /* MU_DAR_ACTIVITY_TYPE    */
    else if(strcmp(activity_type, MU_SINGLE_ACTIVITY_TYPE ) == 0 )
    {
        /*
        --  If activity_type = single: 
        --      Retrieve records from the active_single_activities table 
        --      which have the same permission_id value and mu_activity_id 
        --      value.  
        */
        (void) sprintf( where_clause, "where %s = %d and %s = '%s' ",
            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_PERMISSION_ID),
            permission_id,
            APS_COL(ACTIVE_SINGLE_ACTIVITIES,
                ACTIVE_SINGLE_ACTIVITIES_MU_ACTIVITY_ID),
            mu_activity_id ) ;

        perm_list = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(ACTIVE_SINGLE_ACTIVITIES),
            where_clause, NULL, APS_CDEFS(ACTIVE_SINGLE_ACTIVITIES),
            ALL_COLS) ;
        if( perm_list == NULL )
            return MU_DB_ERROR_RETRIEVING_FROM_ACTIVE_SINGLE_ACTIVITIES_TABLE ;
        if( NUMELTS(perm_list) == 1 )
        {
            /* collect 4 values for later use.  */
            perm_rec = (DB_RECORD **) FIRST( perm_list, perm_list_ptr ) ;
            (void) strcpy( node, 
                CAST_ACTIVE_SINGLE_ACTIVITIES_NODE 
                    perm_rec[ACTIVE_SINGLE_ACTIVITIES_NODE] ) ;
            (void) strcpy( process_id, 
                CAST_ACTIVE_SINGLE_ACTIVITIES_PROCESS_ID 
                    perm_rec[ACTIVE_SINGLE_ACTIVITIES_PROCESS_ID] ) ;
            (void) strcpy( command_name, 
                CAST_ACTIVE_SINGLE_ACTIVITIES_COMMAND_NAME 
                    perm_rec[ACTIVE_SINGLE_ACTIVITIES_COMMAND_NAME] ) ;
            kpid = CAST_ACTIVE_SINGLE_ACTIVITIES_KPID 
                    perm_rec[ACTIVE_SINGLE_ACTIVITIES_KPID] ;
        }
    }  /* MU_SINGLE_ACTIVITY_TYPE    */
    else
    {
        /* 
        -- all cases should have been handled above.  
        */
        (void) printf("%s(%d):  error in code; review recent modifications of APS.\n",
            __FILE__, __LINE__ ) ;
        return MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE ;

    }

    if( NUMELTS(perm_list) == 1 )
    {
        /**********************************************************************
        *                                                                     *
        *     STEP 2:  If one record is found from Step (1), verify that its  *
        *              requesting process is still active.                    *
        *                                                                     *
        **********************************************************************/
        /*
        --  Count records from the sysprocesses table which have the 
        --  same node, process_id, kpid, and command_name as the permission 
        --  record being verified.  
        --  These 4 values were collected earlier for use here.  
        */
        syscount = mu_count_sysprocesses_khph(DB_SYBINT_USE_APS_READER_DBPROC,
            kpid, node, command_name, process_id ) ;
        if( syscount < 0 )
        {
            DEL_LIST( perm_list ) ;
            return syscount ;  /* this is an error code, not a count.  */
        }

        /*
        --  If any sysprocesses record is found, the permission is considered 
        --  to be valid; return the permission_id value to indicate its 
        --  validity.  
        */
        DEL_LIST( perm_list ) ;
        if( syscount > 0 )
            return permission_id ; /* indicates VALID  */
        else
            return 0 ;             /* indicates NOT VALID  */
    }

    if( NUMELTS(perm_list) == 0 )
    {
        DEL_LIST( perm_list ) ;
        /**********************************************************************
        *                                                                     *
        *     STEP 3:  If no records are found from Step (1), the permission  *
        *              is not considered to be valid; return a 0 value to     *
        *              indicate that the permission_id is not valid.          *
        *                                                                     *
        **********************************************************************/
        return 0 ;             /* indicates NOT VALID  */
    }

    if( NUMELTS(perm_list) >  1 )
    {
        /**********************************************************************
        *                                                                     *
        *     STEP 4:  If more than one record is found from Step (1), this   *
        *              is an error.                                           *
        *                                                                     *
        **********************************************************************/
        /*
        --  There should never be two or more records satisfying the conditions 
        --  in Step (1).  
        --  The APS multi-user data is corrupted; return with a -1 value to 
        --  indicate this error condition.  
        */
        DEL_LIST( perm_list ) ;
        return MU_ERROR_DATA_CORRUPTION_DURING_VALIDATION ; 

    }

    /* 
    -- all cases should have been handled above.  
    */
    DEL_LIST( perm_list ) ;
    (void) printf("%s(%d):  error in code; review recent modifications of APS.  \n",
        __FILE__, __LINE__ ) ;
    return MU_ERROR_IN_MULTI_USER_SOFTWARE_CODE ;

}
