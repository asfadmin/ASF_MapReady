#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_is_an_observation.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_is_an_observation.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_is_an_observation.c"



/*==============================================================================
Function:       dtkm_is_an_observation()

Description:    returns TRUE if data-take is an observation.  
                FALSE if not, and < 0 if an error occurred.  

Creator:        Lawrence Stevens

Creation Date:  Sun Mar  9 15:20:52 PST 1997

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include <aps_defs.h>  /* for DTKM_ACTID_RECORDING_OBSERVATION_CODE etc.  */
#include <db_dtk.h>    /* for dtk record stuff                           */
#include <string.h>    /* for strncmp()                                  */

int dtkm_is_an_observation( DB_RECORD **dtk_rec )
{
    if ( dtk_rec == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    /* check of RECording or Realtime OBservation:  */
    if ( strncmp(CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
             DTKM_ACTID_REALTIME_OBSERVATION_CODE, 
             strlen(DTKM_ACTID_REALTIME_OBSERVATION_CODE)  ) == 0 
    ||   strncmp(CAST_DTK_ACTID dtk_rec[DTK_ACTID], 
             DTKM_ACTID_RECORDING_OBSERVATION_CODE, 
             strlen( DTKM_ACTID_RECORDING_OBSERVATION_CODE )  ) == 0   )
    {
        return TRUE ;
    }
    else
    {
        return FALSE ;
    }
}
