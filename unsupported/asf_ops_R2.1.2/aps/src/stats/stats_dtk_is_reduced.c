#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_dtk_is_reduced.c

Description:    

==============================================================================*/
#pragma ident   "@(#)stats_dtk_is_reduced.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_dtk_is_reduced.c"


/*==============================================================================
Function:       stats_dtk_is_reduced()

Description:    return TRUE if dtk is REDUCED.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 11:22:57 PST 1998

==============================================================================*/
#include <string.h>   /*  for strcmp()     */
#include <db_sybint.h>  /* for DB_RECORD etc.      */
#include <dapps_defs.h> /*  for TRUE, FALSE  */
#include <db_dtk.h>   /* for CAST_DTK_DTKSTAT etc.      */
int stats_dtk_is_reduced( DB_RECORD **dtk_rec )
{

    if( strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "SCH" ) != 0 )
        return FALSE ; /* not SCH   */

    /* dtk is SCH, now check if reduced.  */
    if( CAST_DTK_ASF_REDUCTION_MIN dtk_rec[DTK_ASF_REDUCTION_MIN] <= 0.0 )
        return FALSE ; /* dtk not reduced.    */

    return TRUE ;

}
