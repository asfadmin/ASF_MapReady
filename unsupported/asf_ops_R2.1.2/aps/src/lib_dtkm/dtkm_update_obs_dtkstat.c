#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_update_obs_dtkstat.c

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_update_obs_dtkstat.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_update_obs_dtkstat.c"


/*==============================================================================
Function:       dtkm_update_obs_dtkstat()

Description:    given a downlink data-take, this routine finds the 
                observations (recordings or realtime observations) that 
                are downlinked by it and updates their status to match 
                the downlink.  

Creator:        Lawrence Stevens

Creation Date:  Thu Mar 21 13:34:09 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>

int dtkm_update_obs_dtkstat(
    DBPROCESS   *APS_dbproc,
    DB_RECORD   **dl_dtk,  
    llist       *dtk_updates ) 
{
    llist       *obs_dtks;
    int         return_code ;

    /* quick error checking.  */
    if ( dl_dtk == NULL )
        return DTKM_ERROR_NULL_DTK_PROPOSAL ;
 
    if ( dtk_updates == NULL )
        return DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED ;

    /* NOTE:  we will check for a downlink in dl_dtk in dtkm_dl2obs()  */

    /* initialize linked list to obtain data-takes.  */
    obs_dtks = create_dyn_llist() ;

    /* 
    -- find the corresponding observation data-takes 
    -- for this downlink: 
    */ 
    return_code = dtkm_dl2obs( dl_dtk, obs_dtks ) ;
    if ( return_code < 0 )
    {
        DEL_LIST( obs_dtks ) ;
        return return_code ;
    }

    if ( NUMELTS( obs_dtks ) == 0 )
    {
        /* no observations found; this result is OK.  */
        DEL_LIST( obs_dtks ) ;
        return TRUE ;
    }

    /* 
    -- now update the data-takes in the 
    -- llist obs_dtks by changing DTK_DTKSTAT
    -- to the value in dl_dtk.
    */
    return_code = dtkm_update_dtks_field( APS_dbproc, obs_dtks, DTK_DTKSTAT, 
        CAST_DTK_DTKSTAT dl_dtk[DTK_DTKSTAT], dtk_updates ) ;

    if ( return_code < 0 )
    {
        DEL_LIST( obs_dtks ) ;
        return return_code ;
    }

    DEL_LIST( obs_dtks ) ;
    return TRUE ;

}
