#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_verify_dar_perms.c

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
#pragma ident   "@(#)mu_verify_dar_perms.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_verify_dar_perms.c"


/*==============================================================================
Function:       mu_verify_dar_perms()

Description:    verify that the permissions in the input list are 
                still active, using Sybase master..sysprocesses.  

Creator:        Lawrence Stevens

Creation Date:  Tue Dec  3 17:32:15 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "mu.h"
#include "db_active_dar_activities.h"

int mu_verify_dar_perms( 
    DBPROCESS   *dbproc,        /* Sybase session pointer.                */
    llist       *perm_list )    /* list of dar activity permissions.  */
{

    DB_RECORD       **perm_rec ;
    DB_RECORD       **perm_unlinked_rec ;
    cursor          perm_list_ptr ;

    int             sys_count ;         
    int             n_deleted_recs ;

    /* brief paramter checking.  */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;

    if( perm_list == NULL )
        return MU_ERROR_INPUT_DAR_ACTIVITY_LIST_IS_NULL_PTR ;

    /* 
    -- go through each dar permission on the list, 
    -- and verify that the activity is active.  
    -- if not active:
    --  1.  remove the record from the database relation: 
    --      active_dar_activities.
    --  2.  remove the record from perm_list.  
    */

    /*
    -- a permission or activity is active if there exists 
    -- a corresponding record in the Sybase pseudo-table sysprocesses.  
    */


    for (   perm_rec = (DB_RECORD **) FIRST(perm_list, perm_list_ptr);
            perm_rec != NULL ;
            perm_rec = (DB_RECORD **) NEXT(perm_list, perm_list_ptr)  
        )
    {
        /* process the current perm_rec right here.  */

        /* is this permission/process active? */

        sys_count = mu_count_sysprocesses_khph( dbproc,
            CAST_ACTIVE_DAR_ACTIVITIES_KPID 
                perm_rec[ACTIVE_DAR_ACTIVITIES_KPID],
            CAST_ACTIVE_DAR_ACTIVITIES_NODE 
                perm_rec[ACTIVE_DAR_ACTIVITIES_NODE],
            CAST_ACTIVE_DAR_ACTIVITIES_COMMAND_NAME 
                perm_rec[ACTIVE_DAR_ACTIVITIES_COMMAND_NAME],
            CAST_ACTIVE_DAR_ACTIVITIES_PROCESS_ID 
                perm_rec[ACTIVE_DAR_ACTIVITIES_PROCESS_ID] ) ;
        if( sys_count < 0 )
            return sys_count ;   /* this is an error code, not a count.  */

        if( sys_count < 1 )
        {
            /* 
            -- the activity/perm is not active.  
            -- delete this dar activity from the database 
            -- and the perm_list.  
            -- make SURE that this is the ONLY record 
            -- we are deleting, by including enough fields 
            -- in the where_clause:  
            */
            (void) sprintf( where_clause, 
"where %s = %ld and %s = '%s' and %s = '%s' and %s = '%s' and %s = %ld and %s = '%s' ",
                APS_COL(ACTIVE_DAR_ACTIVITIES, 
                    ACTIVE_DAR_ACTIVITIES_KPID),
                CAST_ACTIVE_DAR_ACTIVITIES_KPID 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_KPID],

                APS_COL(ACTIVE_DAR_ACTIVITIES, 
                    ACTIVE_DAR_ACTIVITIES_NODE),
                CAST_ACTIVE_DAR_ACTIVITIES_NODE 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_NODE],

                APS_COL(ACTIVE_DAR_ACTIVITIES, 
                    ACTIVE_DAR_ACTIVITIES_COMMAND_NAME),
                CAST_ACTIVE_DAR_ACTIVITIES_COMMAND_NAME 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_COMMAND_NAME],

                APS_COL(ACTIVE_DAR_ACTIVITIES, 
                    ACTIVE_DAR_ACTIVITIES_PROCESS_ID),
                CAST_ACTIVE_DAR_ACTIVITIES_PROCESS_ID 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_PROCESS_ID],

                APS_COL(ACTIVE_DAR_ACTIVITIES, 
                    ACTIVE_DAR_ACTIVITIES_PERMISSION_ID),
                CAST_ACTIVE_DAR_ACTIVITIES_PERMISSION_ID 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_PERMISSION_ID],

                APS_COL(ACTIVE_DAR_ACTIVITIES, 
                    ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID),
                CAST_ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID 
                    perm_rec[ACTIVE_DAR_ACTIVITIES_MU_ACTIVITY_ID] ) ;

            n_deleted_recs = db_delete_records( dbproc, 
                APS_TABLE(ACTIVE_DAR_ACTIVITIES), where_clause ) ;
            if( n_deleted_recs < 0 )
                return MU_DB_ERROR_DELETING_FROM_ACTIVE_DAR_ACTIVITIES ;
            if( n_deleted_recs == 0 )
                return MU_ERROR_DELETED_NO_RECS_FROM_ACTIVE_DAR_ACTIVITIES ;
            if( n_deleted_recs > 1 )
                return MU_ERROR_DEL_MORE_THAN_ONE_REC_FROM_DAR_ACTIVITIES ;

            /* 
            -- rec deleted from db OK; now remove 
            -- from perm_list  
            */
            perm_unlinked_rec = 
                (DB_RECORD **) UNLINK_AT_CURSOR( perm_list, perm_list_ptr ) ;
            if( perm_unlinked_rec != perm_rec )
                return MU_ERROR_UNLINKING_PERM_RECORD ;

            free_db_record( perm_unlinked_rec ) ;

        }   /* end if( sys_count < 1 )   */


    }   /* end of loop on the perm_list.  */

    return TRUE ;

}
