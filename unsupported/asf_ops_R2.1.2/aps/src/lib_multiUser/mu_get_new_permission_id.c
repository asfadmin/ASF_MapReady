#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_get_new_permission_id.c

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
#pragma ident   "@(#)mu_get_new_permission_id.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_get_new_permission_id.c"



/*==============================================================================
Function:       mu_get_new_permission_id()

Description:    increments the counter in the database, then 
                retrieves the latest counter value.  
                permission_counter.permission_id  is the database value.  


Creator:        Lawrence Stevens

Creation Date:  Tue Dec  3 16:48:16 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "mu.h"
#include "db_permission_counter.h"

int mu_get_new_permission_id(
    DBPROCESS   *dbproc )       /* Pointer to Sybase structure for session. */
{

    int             nrecs = 0 ;
    int             new_permission_id ;
    DB_RECORD       **counter_rec ;
    llist           *counter_list = NULL ;
    cursor          counter_list_ptr ;

    /* brief error checking   */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
        return MU_ERROR_INPUT_DBPROC_INDICATES_APS_READER_ACCOUNT ;

    counter_list = db_get_records( dbproc, APS_TABLE(PERMISSION_COUNTER), 
        NULL, NULL, APS_CDEFS(PERMISSION_COUNTER), ALL_COLS ) ;
    if( counter_list == NULL )
        return MU_DB_ERROR_RETRIEVING_FROM_PERMISSION_COUNTER_TABLE ;

    if( NUMELTS(counter_list) == 0 )
    {
        DEL_LIST( counter_list ) ;
        return MU_ERROR_NO_PERMISSION_COUNTER_RECS_FOUND_IN_TABLE ;
    }

    if( NUMELTS(counter_list) > 1 )
    {
        DEL_LIST( counter_list ) ;
        return MU_ERROR_MORE_THAN_ONE_PERMISSION_COUNTER_REC_FOUND_IN_TABLE ;
    }

    counter_rec = (DB_RECORD **) FIRST(counter_list, counter_list_ptr) ;

    new_permission_id = 1 + 
        CAST_PERMISSION_COUNTER_PERMISSION_ID 
        counter_rec[PERMISSION_COUNTER_PERMISSION_ID] ;

    if( new_permission_id >= 100000 )
        new_permission_id = 1 ;

    (void) sprintf( fields_to_set, 
        "%s = %d",
        APS_COL(PERMISSION_COUNTER, PERMISSION_COUNTER_PERMISSION_ID),
        new_permission_id ) ;

    nrecs = db_update_records(dbproc,
                APS_TABLE(PERMISSION_COUNTER), fields_to_set, NULL) ;
    DEL_LIST( counter_list ) ;
    if( nrecs < 0 )
        return MU_DB_ERROR_UPDATING_PERM_COUNTER_REC ;
    if( nrecs != 1 )
        return MU_ERROR_NOT_ONLY_ONE_PERM_COUNTER_REC_UPDATED ;


    return new_permission_id ;

}
