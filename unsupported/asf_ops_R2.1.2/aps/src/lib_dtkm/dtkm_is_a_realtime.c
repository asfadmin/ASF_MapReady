THIS SOURCE FILE IS OBSOLETE.  
Sun Mar  9 15:17:39 PST 1997




#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtkm_is_a_realtime.c
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)dtkm_is_a_realtime.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_dtkm/SCCS/s.dtkm_is_a_realtime.c"


/*==============================================================================
Function:       dtkm_is_a_realtime()

Description:    returns TRUE if data-take is a realtime, FALSE if not, 
                and < 0 if an error occurred.  

Creator:        Lawrence Stevens

Creation Date:  Fri May 17 15:57:37 PDT 1996

Notes:      
    This routine was created using 4-character tabs.  If you don't have 
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
#include "dtkm.h"
#include "db_dtk.h"    /* for dtk record stuff  */
#include <string.h>    /* for strncmp()         */

int dtkm_is_a_realtime( DB_RECORD **dtk_rec )
{
    if ( dtk_rec == NULL )
        return DTKM_ERROR_NULL_RECORD ;

    if ( strncmp(CAST_DTK_ACTID dtk_rec[DTK_ACTID], "RT",  2 ) == 0  )
        return TRUE ;
    else
        return FALSE ;
}
