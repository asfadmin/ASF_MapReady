#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_is_a_tape_dump.c

Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_is_a_tape_dump.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_is_a_tape_dump.c"


/*==============================================================================
Function:       dtkm_is_a_tape_dump()

Description:    returns TRUE if data-take is a tape_dump, FALSE if not, 
                and < 0 if an error occurred.  

Creator:        Lawrence Stevens

Creation Date:  Thu Mar 21 13:29:09 PST 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"    /* for dtk record stuff  */
#include <string.h>    /* for strncmp()         */
#include <aps_defs.h>  /* for DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE  */

int dtkm_is_a_tape_dump( DB_RECORD **dtk_rec )
{
    if ( dtk_rec == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    if ( strcmp(CAST_DTK_SENSOR dtk_rec[DTK_SENSOR], 
                DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0 )
        return TRUE ;
    else
        return FALSE ;
}
