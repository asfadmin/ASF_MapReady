#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_dtk_is_sch.c

Description:    

==============================================================================*/
#pragma ident   "@(#)stats_dtk_is_sch.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_dtk_is_sch.c"


/*==============================================================================
Function:       stats_dtk_is_sch()

Description:    return TRUE if dtk is SCHEDULED.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 11:22:57 PST 1998

==============================================================================*/
#include <string.h>   /*  for strcmp()     */
#include <db_sybint.h>  /* for DB_RECORD etc.      */
#include <dapps_defs.h> /*  for TRUE, FALSE  */
#include <db_dtk.h>   /* for CAST_DTK_DTKSTAT etc.      */
int stats_dtk_is_sch( DB_RECORD **dtk_rec )
{

    if( strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "SCH" ) == 0 )
        return TRUE ; /* SCH   */

    return FALSE ;

}
