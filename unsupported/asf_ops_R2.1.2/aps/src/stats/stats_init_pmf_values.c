#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_init_pmf_values.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)stats_init_pmf_values.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_init_pmf_values.c"


/*==============================================================================
Function:       stats_init_pmf_values()

Description:    initialize APS_STATS_PMF_VALUES structure values with 
                null strings or "0" for convenience of use in 
                the statistics feature.  

Creator:        Lawrence Stevens

Creation Date:  Fri Feb  6 16:48:13 PST 1998

Notes:          
==============================================================================*/
#include <string.h>            /* for strcpy()                  */
#include "aps_Statistics.h"    /* for APS_STATS_PMF_VALUES      */
int stats_init_pmf_values( APS_STATS_PMF_VALUES *pmf_values )
{
    if( pmf_values == NULL )
    {
        (void)fprintf(stderr, "#########################################\n") ;
        (void)fprintf(stderr, "#########################################\n") ;
        (void)fprintf(stderr, "#########################################\n") ;
        (void)fprintf(stderr, "%s(%d):  NULL pointer in argument.\n", 
            __FILE__, __LINE__ ) ;
        (void)fprintf(stderr, "#########################################\n") ;
        (void)fprintf(stderr, "#########################################\n") ;
        (void)fprintf(stderr, "#########################################\n") ;
        return STATS_ERROR_NULL_POINTER_IN_ARGUMENT ;
    }

    /* 
    -- initialize to "0":  string values 
    -- of numerics; no value indicated by 0.   
    */

    pmf_values->dtkid = 0 ;
    pmf_values->antenna_id = 0 ;
    pmf_values->downlink_rev = 0 ;
    pmf_values->downlink_dtkid = 0 ;

    /* 
    -- initialize to ""; these are string variables, 
    -- sometimes with no value.  
    -- place a null character onto 0 offset for a null 
    -- string value.  
    */
    pmf_values->sensor[0] = '\0' ;
    pmf_values->activity_id[0] = '\0' ;
    pmf_values->asftime_now[0] = '\0' ;
    pmf_values->asftime_aos[0] = '\0' ;
    pmf_values->asftime_los[0] = '\0' ;
    pmf_values->asftime_aos_fa[0] = '\0' ;
    pmf_values->asftime_los_fa[0] = '\0' ;
    pmf_values->asftime_mask_entry[0] = '\0' ;
    pmf_values->asftime_mask_exit[0] = '\0' ;
    pmf_values->asftime_on[0] = '\0' ;
    pmf_values->asftime_off[0] = '\0' ;
    pmf_values->asftime_on_fa[0] = '\0' ;
    pmf_values->asftime_off_fa[0] = '\0' ;
    pmf_values->asftime_planned[0] = '\0' ;

    return TRUE ;

}
