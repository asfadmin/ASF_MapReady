#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_insert_planning_perm.c

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
#pragma ident   "@(#)mu_insert_planning_perm.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_insert_planning_perm.c"


/*==============================================================================
Function:       mu_insert_planning_perm()

Description:    inserts a planning activity permission into the db.  

Creator:        Lawrence Stevens

Creation Date:  Wed Dec  4 12:59:15 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "mu.h"
#include "db_active_planning_activities.h"

int mu_insert_planning_perm(
    DBPROCESS   *dbproc,         /*  input Sybase session pointer.        */
    char        *mu_activity_id, /*  input planning-type activity id      */
    int         permission_id ,  /*  input:  the permission id to use.    */
    char        *padded_strttime,/*  input:  the enlarged time bracket    */
    char        *padded_stoptime,/*  input:  the enlarged time bracket    */
    char        *station_id )    /*  input:  the station_id value         */
{

    DB_RECORD   **perm_rec = NULL ;

    int         kpid ;           /*  current Sybase kernel process id         */
    char        hostname[12] ;   /*  current nodename.  allow 11 chars        */
    char        progname[18] ;   /*  current programe name.  allow 17 char    */
    char        hostprocess_id[10] ;/*  current process id, allow 9 chars  */

                /* login name for current process.  */
    char        mu_unix_userid[MU_UNIX_USERID_STRLEN+1] ;

    int         return_code ;
    int         nrecs_inserted ;

    /* brief error checking   */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;

    /*
    -- validate mu_activity_id
    */
    return_code = mu_validate_activity_id( MU_PLANNING_ACTIVITY_TYPE,
        mu_activity_id ) ;
    if( return_code < 0 )
        return return_code ;

    if( permission_id == 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_ZERO ;
    if( permission_id < 0 )
        return MU_ERROR_INPUT_PERMISSION_ID_IS_LESS_THAN_ZERO ;

    /*
    --      append a new record to the database table, corresponding to
    --      activity_type, that was read in Step (3).
    --      Use the mu_activity_id  from the input parameters, use the
    --      permission_id value from the previous step;
    --      use the node, command_name, process_id, and kpid
    --      values obtained from the Sybase pseudo-relation sysprocesses;
    --      obtain the userid as described in Step (3).
    */

    /*
    -- obtain the node, command_name, process_id, and kpid 
    -- from sysprocesses:
    */
    return_code = mu_get_sysprocesses_khph( dbproc, 
        &kpid, 
        hostname, progname, hostprocess_id ) ;
    if( return_code < 0 )
        return return_code ;

    /*
    -- get the current userid, the first MU_UNIX_USERID_STRLEN characters only:
    */
    return_code = mu_get_unix_userid( mu_unix_userid ) ;
    if( return_code != TRUE )
        return MU_ERROR_COULD_NOT_GET_USERID ;

    /*
    -- now create a new db record, put values in 
    -- each field, then insert.  
    */
    perm_rec =  new_table_record(APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES)) ;

    /*
    -- NODE               
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_NODE 
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_NODE], 
        hostname ) ;

    /*
    -- PROCESS_ID         
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_PROCESS_ID], 
        hostprocess_id ) ;

    /*
    -- KPID               
    */
    CAST_ACTIVE_PLANNING_ACTIVITIES_KPID 
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_KPID]
        = kpid ;

    /*
    -- COMMAND_NAME       
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_COMMAND_NAME], 
        progname ) ;

    /*
    -- PERMISSION_ID      
    */
    CAST_ACTIVE_PLANNING_ACTIVITIES_PERMISSION_ID 
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_PERMISSION_ID]
        = permission_id ;

    /*
    -- MU_ACTIVITY_ID     
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_MU_ACTIVITY_ID],
        mu_activity_id ) ;

    /*
    -- USERID             
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_USERID
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_USERID],
        mu_unix_userid ) ;

    /*
    -- STRTTIME     
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_STRTTIME
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_STRTTIME],
        padded_strttime ) ;

    /*
    -- STOPTIME     
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_STOPTIME
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_STOPTIME],
        padded_stoptime ) ;

    /*
    -- STATION_ID     
    */
    (void) strcpy( CAST_ACTIVE_PLANNING_ACTIVITIES_STATION_ID
        perm_rec[ACTIVE_PLANNING_ACTIVITIES_STATION_ID],
        station_id ) ;

    /*
    -- all the fields are filled in.  
    */

#ifdef PRINT_MU_INSERT
    (void) printf("\n%s(%d):  planning activity to insert:\n", 
        __FILE__, __LINE__ ) ;
    db_print_record(perm_rec, APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;
    (void) printf("\n");
#endif /*   PRINT_MU_INSERT    */

    /* insert one record into db:  */
    nrecs_inserted = db_insert_single_record(dbproc, perm_rec, 
        APS_TABLE(ACTIVE_PLANNING_ACTIVITIES), 
        APS_CDEFS(ACTIVE_PLANNING_ACTIVITIES) ) ;

    free_db_record(perm_rec) ;

    if( nrecs_inserted != 1 )
        return MU_DB_ERROR_INSERTING_PLANNING_ACTIVITY ;

    return TRUE ;

}
