#ifdef COPYRIGHT 
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_set_dtkstat_from_dl.c

Notes:          

==============================================================================*/
#pragma ident   "@(#)dtkm_set_dtkstat_from_dl.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_set_dtkstat_from_dl.c"


/*==============================================================================
Function:       dtkm_set_dtkstat_from_dl()

Description:    given a recording or real time sensing data-take, this 
                routine finds the downlink data-take that downlinks it.  
                Then it copies the dtk.dtkstat field from that dtk to 
                the recording or real time sensing data-take proposal.  
                i.e., if an observation wants to be SCH, but the downlink 
                is CON, then the observation must result in CON status.  

                This routine now returns the downlink_dtk whether or not 
                the dtkstat value needed changing.  Earlier, if the 
                dtkstat values were the same, the downlink dtk was 
                not stored.  

Creator:        Lawrence Stevens

Creation Date:  Thu Mar 21 15:59:40 PST 1996

Notes:      
    There is no update of the relation that links observation datatakes to
    downlink datatakes (dl2obs).
    This is because at the time we call this routine, we do not know if
    the observation datatake has been inserted into the database or not, and
    therefore do not know if the observation datatake has been assigned a
    valid dtkid value.

    The (dl2obs) relation will be updated at a later time, after the
    observation datatake has been successfully inserted into the database.

    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>     /* for dtk relation.  */

int dtkm_set_dtkstat_from_dl(   /* it is OK for the pointers to be equal:  */
                                /* obs_dtk == result_dtk                   */
    DB_RECORD   **obs_dtk,      /* input (observation) recording data-take   */
    DB_RECORD   **result_dtk,   /* resulting (observation) data-take         */
    DB_RECORD   **downlink_dtk )  /* store the downlink data-take associated
                                   with an observation data-take proposal    */
{
    DB_RECORD   **dl_dtk;

    int         return_code ;

    /* quick error checking.  */

    if ( obs_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( result_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_RESULT_RECORD ;
 
    if ( downlink_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_DOWNLINK_RECORD ;
 
    /* NOTE:  we will check for an observation in obs_dtk in dtkm_obs2dl() */

    /* 
    -- we readonly the input data-take and write to the 
    -- previously allocated output data-take.  copy now 
    -- and work with the result:
    */
    return_code = db_copy_record ( APS_CDEFS(DTK), result_dtk, obs_dtk ) ;

    /* 
    -- find the corresponding DMP data-take 
    -- for this observation.  
    */ 
    /* initialize the output record:  */
    dl_dtk = new_table_record(APS_CDEFS(DTK)) ;

    return_code = dtkm_obs2dl( result_dtk, dl_dtk ) ;
    if ( return_code < 0 )
    {
        free_db_record(dl_dtk) ;
        return return_code ;
    }

    if ( return_code == DTKM_DOWNLINK_REC_FOUND )
    {
        /*
        -- store a copy of the downlink record for later use.       
        -- this is part of the output for this routine:  
        -- Note:  this store now takes place no matter what 
        -- the comparison was for the dtkstat value.  
        */
        return_code = db_copy_record ( APS_CDEFS(DTK), downlink_dtk, dl_dtk ) ;

        if(  strcmp( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], 
                 CAST_DTK_DTKSTAT dl_dtk[DTK_DTKSTAT] )     )
        {
            /* 
            -- the 2 statuses are different.  Therefore, must update 
            -- the DTK_DTKSTAT value in the result record.  
            */
            strcpy( CAST_DTK_DTKSTAT result_dtk[DTK_DTKSTAT], 
                    CAST_DTK_DTKSTAT dl_dtk[DTK_DTKSTAT] ) ;

            /* the work is done.  */
            free_db_record(dl_dtk) ;

            /* 
            -- TRUE means that the status given to the data-take 
            -- was DIFFERENT from that proposed, due to being 
            -- overridden by the downlink data-take.  
            -- This is UNEXPECTED by the planner and might cause 
            -- confusion.  Therefore, we return TRUE, which 
            -- will trigger a print statement in the report file.  
            */
            return TRUE ;
        } /* endif:  statuses different  */
        else
        {
            /* 
            -- There was no update.   
            -- return FALSE.
            */
            free_db_record(dl_dtk) ;
            /* 
            -- FALSE means that the status given to the data-take 
            -- was THE SAME as that proposed.  
            -- This is EXPECTED by the planner and might cause 
            -- confusion if we print a message explaining it.  
            -- Therefore, we return FALSE, which 
            -- will not trigger a print statement in the report file.  
            */
            return FALSE ;
        }
    }
    else
    {
        /* 
        -- downlink was not found.  
        -- return FALSE.
        */
        free_db_record(dl_dtk) ;
        /* 
        -- FALSE means that the status given to the data-take 
        -- was THE SAME as that proposed.  
        -- This is EXPECTED by the planner and might cause 
        -- confusion if we print a message explaining it.  
        -- Therefore, we return FALSE, which 
        -- will not trigger a print statement in the report file.  
        */
        return FALSE ;
    }

}
