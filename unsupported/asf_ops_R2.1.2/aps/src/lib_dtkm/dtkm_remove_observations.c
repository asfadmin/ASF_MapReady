#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_remove_observations.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_remove_observations.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_remove_observations.c"

#include "dtkm.h"


/*==============================================================================
Function:       dtkm_remove_observations

Description:    remove observation data-takes from a list, retaining
                only downlink data-takes.  

Returns:        TRUE, FALSE

Creator:        Lawrence Stevens

Creation Date:  Sat Nov 11 15:44:05 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int dtkm_remove_observations( llist *dtk_list )
{
    DB_RECORD   **dtk_rec = NULL ;
    cursor      dtk_list_ptr ;
    DB_RECORD   **next_dtk_rec = NULL ;
    cursor      next_dtk_list_ptr ;

    if ( dtk_list == NULL ) 
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
    while ( dtk_rec )
    {
        if ( dtkm_is_an_observation( dtk_rec ) )
        {
            /* 
            -- this is an observation (non-downlink) data-take; destroy 
            -- this record from the list. 
            */

            /* obtain and save the next dtk record to process:  */
            next_dtk_list_ptr = dtk_list_ptr ;
            next_dtk_rec = (DB_RECORD **) NEXT(dtk_list, next_dtk_list_ptr) ;

            /* destroy the dtk_rec from the list */
            DEL_AT_CURSOR( dtk_list, dtk_list_ptr ) ;

            /* set up next iteration.  */
            dtk_rec = next_dtk_rec ;
            dtk_list_ptr = next_dtk_list_ptr ;
            continue ;
        }
        else
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr) ;
    }

    return TRUE ;

}
