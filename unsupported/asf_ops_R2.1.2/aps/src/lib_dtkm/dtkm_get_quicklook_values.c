#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_get_quicklook_values.c

Description:    look through a list of obs dtks and get 
                the correct value of planner and science quicklook flags.  

    
==============================================================================*/
#pragma ident   "@(#)dtkm_get_quicklook_values.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_get_quicklook_values.c"


/*==============================================================================
Function:       dtkm_get_quicklook_values()

Description:    look through a list of obs dtks and get 
                the correct value of planner and science quicklook flags.  

Creator:        Lawrence Stevens

Creation Date:  Tue Aug 12 20:45:07 PDT 1997

==============================================================================*/
#include "dtkm.h"     /* for db_sybint.c, dapps_defs.h,  etc.  */
#include "db_dtk.h"   /* for CAST_DTK_PLANNER_QUICKLOOK  etc.  */

int dtkm_get_quicklook_values( 
    llist   *obs_dtk_list,          /* input list of observations            */
    char    *dl_planner_quicklook,  /* output 1-char planner QL for downlink */
    char    *dl_science_quicklook ) /* output 1-char science QL for downlink */
{

    DB_RECORD   **obs_dtk_rec ;
    cursor      obs_dtk_list_ptr ;

    /*
    -- quick error checking.  
    */
    if( obs_dtk_list == NULL )
        return DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED ;

    /* 
    -- the correct QL value is a logical OR of the observation values.  
    -- this means that if there is at least one Y value, the 
    -- downlink value is Y.  if all are N, the downlink value is N.
    */
    *dl_planner_quicklook = 'N' ;
    *dl_science_quicklook = 'N' ;

    for (   obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, obs_dtk_list_ptr);
            obs_dtk_rec ;
            obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, obs_dtk_list_ptr)  
        )
    {
        /* process the current obs_dtk_rec right here.  */
        if( CAST_DTK_PLANNER_QUICKLOOK obs_dtk_rec[DTK_PLANNER_QUICKLOOK] 
            == 'Y' )
        {
            *dl_planner_quicklook = 'Y' ;
            break ;
        }

    }

    for (   obs_dtk_rec = (DB_RECORD **) FIRST(obs_dtk_list, obs_dtk_list_ptr);
            obs_dtk_rec ;
            obs_dtk_rec = (DB_RECORD **) NEXT(obs_dtk_list, obs_dtk_list_ptr)  
        )
    {
        /* process the current obs_dtk_rec right here.  */
        if( CAST_DTK_SCIENCE_QUICKLOOK obs_dtk_rec[DTK_SCIENCE_QUICKLOOK] 
            == 'Y' )
        {
            *dl_science_quicklook = 'Y' ;
            break ;
        }

    }

    return TRUE ;


}
