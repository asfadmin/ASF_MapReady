#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_delete_dtk_record.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_delete_dtk_record.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_delete_dtk_record.c"



/*==============================================================================
Function:       dtkm_delete_dtk_record()

Description:    deletes a data-take from the dtk relation, based on 
                sat/sensor/rev/dtkid.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  2 23:22:58 PST 1995

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"

int dtkm_delete_dtk_record(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dtk_record ) 
{
    int     return_code ;
    int     nrecs_deleted ;
    llist   *dtk_updates ;

    /* quick error checking. */
    if ( dtk_record == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    sprintf(where_clause,
        "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
        APS_COL(DTK, DTK_SAT),      CAST_DTK_SAT dtk_record[DTK_SAT],
        APS_COL(DTK, DTK_SENSOR),   CAST_DTK_SENSOR dtk_record[DTK_SENSOR],
        APS_COL(DTK, DTK_REV),      CAST_DTK_REV dtk_record[DTK_REV],
        APS_COL(DTK, DTK_DTKID),    CAST_DTK_DTKID dtk_record[DTK_DTKID] ) ;
 
    nrecs_deleted = db_delete_records(APS_dbproc,
        APS_TABLE(DTK), where_clause ) ;
 
    if ( nrecs_deleted == 0 )
    {
        /*
        -- ERROR:
        -- this record did not exist in the db; it should have.
        */
        return DTKM_ERROR_ATTEMPT_TO_DELETE_NONEXISTING_DTK ;
    }
    else if ( nrecs_deleted < 0 )
        return DTKM_ERROR_ATTEMPTING_TO_DELETE_DTK ;
    else if ( nrecs_deleted > 1 )  /* this should never happen:  */
        return DTKM_ERROR_GT_1_DTK_DELETED ;

    /* 
    -- check to see if there is an entry 
    -- in the dl2obs relation to delete, too.  
    */
    /* 
    -- accommodate to the calling sequence of this routine, 
    -- which normally runs during a file processor, with 
    -- an log file indicating all updates.  
    */
    dtk_updates = create_dyn_llist() ;
    return_code = dtkm_delete_dl2obs_recs( APS_dbproc, dtk_record, 
        dtk_updates ) ;
    DEL_LIST( dtk_updates ) ;
    if( return_code < 0 )
        return return_code ;

    return TRUE ;

}
