#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   dtkm_is_a_downlink.c

Notes:
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_is_a_downlink.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_is_a_downlink.c"



/*==============================================================================
Function:       dtkm_is_a_downlink()

Description:    returns TRUE if data-take is a downlink, if it must use an 
                antenna, FALSE if not, 
                and < 0 if an error occurred.  

Creator:        Lawrence Stevens

Creation Date:  Mon Oct 30 12:53:23 PST 1995

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include <db_dtk.h>    /* for dtk record stuff                    */
#include <aps_defs.h>  /* for DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE   */
#include <string.h>    /* for strncmp()                           */

int dtkm_is_a_downlink( DB_RECORD **dtk_rec )
{
    if ( dtk_rec == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    if ( strcmp(CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0 
    ||   strcmp(CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) == 0  )
        return TRUE ;
    else
        return FALSE ;
}
