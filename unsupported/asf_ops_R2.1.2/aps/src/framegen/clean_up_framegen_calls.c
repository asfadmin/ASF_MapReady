#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       clean_up_framegen_calls.c

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
#pragma ident   "@(#)clean_up_framegen_calls.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.clean_up_framegen_calls.c"


/*==============================================================================
Function:       clean_up_framegen_calls()

Description:    Clean up by deleting all records from the framegen_calls 
                relation with stoptime before the current date.

Creator:        Lawrence Stevens

Creation Date:  Tue Mar 26 18:30:37 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "aps_framegen.h"

int clean_up_framegen_calls(
    char        *progname,
    FILE        *logfp,                 /* log file pointer.                */
    DBPROCESS   *APS_dbproc,
    char        *today_dtkdate ) 
{

    char        msg[MSG_LEN] ;

    DB_RECORD    **framegen_calls_delete_rec ;
    llist       *framegen_calls_delete_list ;
    cursor      framegen_calls_delete_ptr ;

    int         nrecs_deleted ;
    int         total_recs = 0 ;

    /*
    -- Retrieve all data-takes from the framegen_calls relation 
    -- with stoptime before the current date.  
    */
    (void)sprintf( where_clause, "where %s < '%s'",
        APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_STOPTIME), today_dtkdate ) ;

    /* order by STRTTIME  */
    framegen_calls_delete_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(FRAMEGEN_CALLS),
        where_clause, APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_STRTTIME), 
        APS_CDEFS(FRAMEGEN_CALLS), ALL_COLS) ;
    if (framegen_calls_delete_list == NULL)
    {
        (void)sprintf(msg,
            "%s(%d):  Error in Sybase query on framegen_calls relation %s",
            __FILE__, __LINE__, where_clause ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY ;
    }
    if ( NUMELTS( framegen_calls_delete_list ) == 0 )
    {
        DEL_LIST( framegen_calls_delete_list ) ;
#ifdef DEBUG
        printf( "%s(%d):  No records to clean up; no records found %s\n",
            __FILE__, __LINE__, where_clause ) ;
#endif
        return APS_FRAMEGEN_OK ;
    }

    for ( framegen_calls_delete_rec = (DB_RECORD **)
                FIRST(framegen_calls_delete_list, framegen_calls_delete_ptr) ;
          framegen_calls_delete_rec ;
          framegen_calls_delete_rec = (DB_RECORD **)
                NEXT(framegen_calls_delete_list, framegen_calls_delete_ptr)
        )
    {
        /* delete this rec from the database:  */
        (void)sprintf( where_clause,
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SAT),
            CAST_FRAMEGEN_CALLS_SAT
                framegen_calls_delete_rec[FRAMEGEN_CALLS_SAT],
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SENSOR),
            CAST_FRAMEGEN_CALLS_SENSOR
                framegen_calls_delete_rec[FRAMEGEN_CALLS_SENSOR],
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_REV),
            CAST_FRAMEGEN_CALLS_REV
                framegen_calls_delete_rec[FRAMEGEN_CALLS_REV],
            APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_DTKID),
            CAST_FRAMEGEN_CALLS_DTKID
                framegen_calls_delete_rec[FRAMEGEN_CALLS_DTKID] ) ;
 
        nrecs_deleted = db_delete_records(APS_dbproc,
                APS_TABLE(FRAMEGEN_CALLS), where_clause ) ;
        if ( nrecs_deleted != 1 )
        {
            (void)sprintf(msg,
"%s(%d):  Error in Sybase deletion on framegen_calls relation %s",
                __FILE__, __LINE__, where_clause ) ;
            (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            return APS_FRAMEGEN_ERROR_DELETING_REC_IN_DB ;
        }
        total_recs ++ ;
    }

    DEL_LIST( framegen_calls_delete_list ) ;

#ifdef DEBUG
    printf( "%s(%d):  cleaned up %d old, unneeded records from \n",
        __FILE__, __LINE__, total_recs ) ;
    printf( "    framegen_calls relation.\n" ) ;
#endif

    return APS_FRAMEGEN_OK ;

}
