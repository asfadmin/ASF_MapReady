#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_update_dl2obs_4dtk.c

==============================================================================*/
#pragma ident   "@(#)dtkm_update_dl2obs_4dtk.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_dl2obs_4dtk.c"

/*==============================================================================
Function:       dtkm_update_dl2obs_4dtk()

Description:    given a 'before' and 'after' data-take, find the 
                entries that link to the 'before' dtk and change 
                those links so that they link to the 'after' dtk.  
                This is done when a sat/sensor/rev/dtkid changes, 
                and we want the links in the dl2obs relation to keep up.  

Creator:        Lawrence Stevens

Creation Date:  Thu May  1 20:58:03 PDT 1997

==============================================================================*/

#include <dtkm_utilities.h>
#include <aps_db_table.h>     /* for DL2OBS                */
#include <db_dtk.h>           /* for CAST_DTK_SENSOR etc.  */
#include <db_dl2obs.h>        /* for DL2OBS_SAT etc.       */

int dtkm_update_dl2obs_4dtk(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **before_dtk_rec,    /*   dtk before it was changed    */
    DB_RECORD   **after_dtk_rec  )   /*   dtk after it was changed.    */
{
    int nrecs ;

    if( before_dtk_rec == NULL )
        return DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE ;

    if( after_dtk_rec == NULL )
        return DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE ;

    if( dtkm_is_a_downlink( before_dtk_rec ) == TRUE )
    {
        /*
        -- update the values, if any, of any possible entries
        -- in dl2obs realtion that exist for before_dtk_rec
        -- update these values to match the values of after_dtk_rec.
        */
        sprintf( fields_to_set, " %s = '%s', %s = %ld, %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT),
                CAST_DTK_SAT after_dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_DL),
                CAST_DTK_REV after_dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_DL),
                CAST_DTK_DTKID after_dtk_rec[DTK_DTKID] ) ;
        sprintf( where_clause,
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT),
                CAST_DTK_SAT before_dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_DL),
                CAST_DTK_REV before_dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_DL),
                CAST_DTK_DTKID before_dtk_rec[DTK_DTKID] ) ;
        nrecs = db_update_records( APS_dbproc, APS_TABLE( DL2OBS ),
            fields_to_set, where_clause ) ;
        if( nrecs < 0 )
            return DTKM_ERROR_DB_UPDATE_FAILED_ON_DL2OBS_RELATION ;
    }
    if( dtkm_is_an_observation( before_dtk_rec ) == TRUE )
    {
        /*
        -- update the values, if any, of any possible entries
        -- in dl2obs realtion that exist for before_dtk_rec
        -- update these values to match the values of after_dtk_rec.
        */
        sprintf( fields_to_set, " %s = '%s', %s = %ld, %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT),
                CAST_DTK_SAT after_dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_OBS),
                CAST_DTK_REV after_dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_OBS),
                CAST_DTK_DTKID after_dtk_rec[DTK_DTKID] ) ;
        sprintf( where_clause,
            "where %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DL2OBS, DL2OBS_SAT),
                CAST_DTK_SAT before_dtk_rec[DTK_SAT],
            APS_COL(DL2OBS, DL2OBS_REV_OBS),
                CAST_DTK_REV before_dtk_rec[DTK_REV],
            APS_COL(DL2OBS, DL2OBS_DTKID_OBS),
                CAST_DTK_DTKID before_dtk_rec[DTK_DTKID] ) ;
        nrecs = db_update_records( APS_dbproc, APS_TABLE( DL2OBS ),
            fields_to_set, where_clause ) ;
        if( nrecs < 0 )
            return DTKM_ERROR_DB_UPDATE_FAILED_ON_DL2OBS_RELATION ;
    }

    return TRUE ;

}
