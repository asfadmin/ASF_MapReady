#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_dtk_is_cancelled.c

Description:    

==============================================================================*/
#pragma ident   "@(#)stats_dtk_is_cancelled.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_dtk_is_cancelled.c"


/*==============================================================================
Function:       stats_dtk_is_cancelled()

Description:    return TRUE if dtk is CANCELLED by ASF.  

Creator:        Lawrence Stevens

Creation Date:  Wed Jan 21 11:22:57 PST 1998

==============================================================================*/
#include <string.h>   /*  for strcmp(), strstr()   */
#include <db_sybint.h>  /* for DB_RECORD etc.      */
#include <dapps_defs.h> /*  for TRUE, FALSE        */
#include <db_dtk.h>   /* for CAST_DTK_DTKSTAT etc. */
int stats_dtk_is_cancelled( DB_RECORD   **dtk_rec )
{

    if( strcmp(CAST_DTK_PROPOSED_DTKSTAT dtk_rec[DTK_PROPOSED_DTKSTAT], "SCH"))
        return FALSE ; /* dtk.proposed_dtkstat != SCH   */

    /* dtk was proposed as SCH.   */

    /* 
    -- if FA-CANCELLED is within dtk.notes, this 
    -- dtk was not cancelled by ASF, but by Flight Agency.  
    */
    if( strstr(CAST_DTK_NOTES dtk_rec[DTK_NOTES], "FA-CANCELLED") != NULL )
        return FALSE ; /* dtk was cancelled by FA, not by ASF.   */

    if( strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "REJ") == 0
    ||  strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "DEL") == 0
    ||  strcmp(CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "CON") == 0  )
        return TRUE ;   /* dtk was cancelled.   */

    /* dtk was not cancelled.   */
    return FALSE ;

}
