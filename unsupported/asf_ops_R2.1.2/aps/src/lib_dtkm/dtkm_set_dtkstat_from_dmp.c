THIS SOURCE FILE HAS BEEN REPLACED BY dtkm_set_dtkstat_from_dl.c
Sun Mar  9 11:59:00 PST 1997

#ifdef COPYRIGHT 
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_set_dtkstat_from_dmp.c

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_set_dtkstat_from_dmp.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_set_dtkstat_from_dmp.c"


/*==============================================================================
Function:       dtkm_set_dtkstat_from_dmp()

Description:    given a recording data-take, this routine finds the 
                tape dump data-take that downlinks it.  Then it copies 
                the dtk.dtkstat field from that dtk to the recording 
                data-take proposal.  
                i.e., if a recording wants to be SCH, but the tape dump 
                is CON, then the recording will result in CON status.  

Creator:        Lawrence Stevens

Creation Date:  Thu Mar 21 15:59:40 PST 1996

Notes:      
    There is no update of the relation that links recording datatakes to
    tape_dump datatakes (dtk_obs_dmp).
    This is because at the time we call this routine, we do not know if
    the recording datatake has been inserted into the database or not, and
    therefore do not know if the recording datatake has been assigned a
    valid dtkid value.

    The (dtk_obs_dmp) relation will be updated at a later time, after the
    recording datatake has been successfully inserted into the database.

    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>     /* for dtk relation.  */

int dtkm_set_dtkstat_from_dmp(
                                /* it is OK for the pointers to be equal:  */
                                /* obs_dtk == result_dtk                   */
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **obs_dtk,      /* input (observation) recording data-take  */
    DB_RECORD   **result_dtk,   /* result data-take.                        */
    DB_RECORD   **parent_dtk )  /* store the downlink data-take associated
                                   with a recording data-take proposal      */
{
    DB_RECORD   **dmp_dtk;

    int         return_code ;

    /* quick error checking.  */

    if ( obs_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    if ( parent_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_DOWNLINK_RECORD ;
 
    /* NOTE:  we will check for an observation in obs_dtk in dtkm_obs2dmp() */

    /* 
    -- we readonly the input data-take and write to the 
    -- previosly allocated output data-take.  copy now 
    -- and work with the result:
    */
    return_code = db_copy_record ( APS_CDEFS(DTK), result_dtk, obs_dtk ) ;

    /* 
    -- find the corresponding DMP data-take 
    -- for this observation.  In the process, the dtk_obs_dmp relation 
    -- could get updated.  
    -- NOTE: the update to dtk_obs_dmp is NO LONGER TRUE. See notes in header.
    */ 
    /* initialize the output record:  */
    dmp_dtk = new_table_record(APS_CDEFS(DTK)) ;

    return_code = dtkm_obs2dmp( APS_dbproc, result_dtk, dmp_dtk ) ;
    if ( return_code < 0 )
    {
        free_db_record(dmp_dtk) ;
        return return_code ;
    }

    if ( return_code == DTKM_DMP_REC_FOUND )
    {
        /* update the DTK_DTKSTAT value in the result record.  */
        strcpy( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], 
                CAST_DTK_DTKSTAT dmp_dtk[DTK_DTKSTAT] ) ;

        /* store a copy of the downlink (parent) record for later use */
        return_code = db_copy_record ( APS_CDEFS(DTK), parent_dtk, dmp_dtk ) ;

        /* the work is done.  */
        free_db_record(dmp_dtk) ;
        return TRUE ;
    }
    else
    {
        /* 
        -- There was no error, but since no update took place,
        -- return FALSE.
        */
        free_db_record(dmp_dtk) ;
        return FALSE ;
    }

}
