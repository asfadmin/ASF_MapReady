#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_insert_dl2obs_rec.c

Description:    Given an observation and downlink, create a dl2obs 
                record linking them together.  If necessary, delete 
                the old dl2obs record for the observation.  

==============================================================================*/
#pragma ident   "@(#)dtkm_insert_dl2obs_rec.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_insert_dl2obs_rec.c"


/*==============================================================================
Function:       dtkm_insert_dl2obs_rec()

Description:    Given an observation and downlink, create a dl2obs 
                record linking them together.  If necessary, delete 
                the old dl2obs record for the observation.  

Creator:        Lawrence Stevens

Creation Date:  Sat Aug 16 21:13:58 PDT 1997

==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>
#include <db_dl2obs.h>

int dtkm_insert_dl2obs_rec(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **obs_dtk_rec,       /* input REC or ROBS observation dtk    */
    DB_RECORD   **downlink_dtk_rec ) /* input downlnk data-take              */
{

    int         nrecs ;
    DB_RECORD   **dl2obs_rec ;  

    /* allocate new storage.  free it later:  */
    dl2obs_rec = new_table_record(APS_CDEFS(DL2OBS)) ;

    /*
    -- we wish to (before inserting) delete any dl2obs
    -- rec for this obs.  (just in case the rec already 
    -- exists.  
    -- the fields to search on are sat, rev_obs, and dtkid_obs 
    -- in the dl2obs relation.  
    -- if the rec to delete doesn't exist, this is
    -- considered to be OK in this situation.
    */
    sprintf(where_clause,
        "where %s = '%s' and %s = %ld and %s = %d",
        APS_COL(DL2OBS, DL2OBS_SAT),
                    CAST_DTK_SAT obs_dtk_rec[DTK_SAT],
        APS_COL(DL2OBS, DL2OBS_REV_OBS),
                    CAST_DTK_REV obs_dtk_rec[DTK_REV],
        APS_COL(DL2OBS,DL2OBS_DTKID_OBS),
                    (int) CAST_DTK_DTKID obs_dtk_rec[DTK_DTKID]);
    nrecs = db_delete_records(APS_dbproc,
            APS_TABLE(DL2OBS), where_clause ) ;
    if( nrecs < 0 )
    {
        free_db_record( dl2obs_rec ) ;
        return DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION ;
    }
    /* dl2obs.sat    */
    strcpy( CAST_DL2OBS_SAT dl2obs_rec[DL2OBS_SAT],
            CAST_DTK_SAT    obs_dtk_rec[DTK_SAT] ) ;

    /* dl2obs rev/dtkid for observation:    */
    CAST_DL2OBS_REV_OBS dl2obs_rec[DL2OBS_REV_OBS]   =
        CAST_DTK_REV      obs_dtk_rec[DTK_REV]  ;
    CAST_DL2OBS_DTKID_OBS dl2obs_rec[DL2OBS_DTKID_OBS] =
        CAST_DTK_DTKID      obs_dtk_rec[DTK_DTKID]  ;

    /* dl2obs rev/dtkid for downlink:    */
    CAST_DL2OBS_REV_DL  dl2obs_rec[DL2OBS_REV_DL]    =
        CAST_DTK_REV downlink_dtk_rec[DTK_REV]  ;
    CAST_DL2OBS_DTKID_DL dl2obs_rec[DL2OBS_DTKID_DL]  =
        CAST_DTK_DTKID    downlink_dtk_rec[DTK_DTKID]  ;

    nrecs = db_insert_single_record(APS_dbproc,
        dl2obs_rec, APS_TABLE(DL2OBS), APS_CDEFS(DL2OBS) ) ;
    if ( nrecs != 1 )
    {
        free_db_record( dl2obs_rec ) ;
        return DTKM_ERROR_dl2obs_rec_NOT_INSERTED ;
    }

    free_db_record( dl2obs_rec ) ;
    return TRUE ;

}
