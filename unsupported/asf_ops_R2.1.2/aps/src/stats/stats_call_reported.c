#undef PRINT_DIAG

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       stats_call_reported.c

Description:    


==============================================================================*/
#pragma ident   "@(#)stats_call_reported.c	1.1 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.stats_call_reported.c"


/*==============================================================================
Function:       stats_call_reported()

Description:    return TRUE if the stats report call has already 
                been reported.  Therefore, no need to report it 
                again.  This is part of error handling.  
                Look in the stats_calls relation to find out.  
                The parameters in this routine uniquely identify 
                what is to be reported.  

Creator:        Lawrence Stevens

Creation Date:  Tue Feb  3 14:31:25 PST 1998

Notes:          
==============================================================================*/
#include "aps_Statistics.h"
#include <db_sybint.h>         /* for db_num_records() etc.  */
#include <db_stats_calls.h>    /* for STATS_CALLS_TYPE etc.  */
#include <aps_db_table.h>      /* for STATS_CALLS            */

int stats_call_reported( DB_RECORD **stats_calls_rec ) 
{
    int nrecs ;

#ifdef PRINT_DIAG
    /*
    -- print input record:  
    */
    (void) printf( "\n%s(%d):  INPUT RECORD:\n", __FILE__, __LINE__ ) ;
    db_print_record(stats_calls_rec, APS_CDEFS(STATS_CALLS) ) ;
#endif /*  PRINT_DIAG   */

    /* 
    --  These values are obtained from the input record:  
    --  char        *type             /x "AOS", "MSK", "SAR", "DLK"        x/
    --  char        *condition        /x "SCH", "RED", "CAN", "PLN"        x/
    --  char        *sat 
    --  int     rev 
    --  char    dtkid  (1-byte int)   /x will be 0 if type = "AOS", "MSK"  x/
    */
    (void) sprintf(where_clause, 
        "where %s = '%s' and %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
        APS_COL(STATS_CALLS, STATS_CALLS_TYPE), 
            CAST_STATS_CALLS_TYPE stats_calls_rec[STATS_CALLS_TYPE],
        APS_COL(STATS_CALLS, STATS_CALLS_CONDITION), 
            CAST_STATS_CALLS_CONDITION stats_calls_rec[STATS_CALLS_CONDITION],
        APS_COL(STATS_CALLS, STATS_CALLS_SAT), 
            CAST_STATS_CALLS_SAT stats_calls_rec[STATS_CALLS_SAT],
        APS_COL(STATS_CALLS, STATS_CALLS_REV), 
            CAST_STATS_CALLS_REV stats_calls_rec[STATS_CALLS_REV],
        APS_COL(STATS_CALLS, STATS_CALLS_DTKID), 
            CAST_STATS_CALLS_DTKID stats_calls_rec[STATS_CALLS_DTKID]  ) ;
    nrecs = db_num_records(DB_SYBINT_USE_APS_READER_DBPROC, 
        APS_TABLE(STATS_CALLS), where_clause ) ;
    if( nrecs < 0 )
        return STATS_ERROR_DB_ERROR_COUNTING_STATS_CALLS_RECS ;
    if( nrecs > 0 )
        return TRUE ;  /* this stats_call has already been reported.    */

    return FALSE ;     /* this stats_call has not been reported.    */

}
