#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       report_expired_dtks.c

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
#pragma ident   "@(#)report_expired_dtks.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.report_expired_dtks.c"


/*==============================================================================
Function:       report_expired_dtks()

Description:    Report the expired data-takes to frame generator.  
                Retrieve all data-takes from the framegen_calls relation 
                with stoptime before the current date.  For each data-take, 
                check its current status in the dtk relation.  If the 
                status != SCH, report this data-take as rejected.

Creator:        Lawrence Stevens

Creation Date:  Tue Mar 26 18:30:37 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "aps_framegen.h"

int report_expired_dtks(
    char        *progname,
    FILE         *logfp,
    DBPROCESS   *APS_dbproc,
    char        *today_dtkdate,
    int         *non_fatal_error_count  )
{
    char        msg[MSG_LEN] ;

    DB_RECORD    **framegen_calls_rec ;
    llist       *framegen_calls_list ;
    cursor      framegen_calls_ptr ;

    DB_RECORD    **dtk_rec ;
    llist       *dtk_list ;
    cursor      dtk_list_ptr ;

    int         reported_recs = 0 ;
    int         return_code ;

    (void)fprintf(logfp, "\n%s(%d):  Now reporting expired data-takes...\n", 
        __FILE__, __LINE__ ) ;
    /*
    -- Retrieve all data-takes from the framegen_calls relation 
    -- with stoptime before the current date.  
    */
    (void)sprintf( where_clause, "where %s < '%s'",
        APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_STOPTIME), today_dtkdate ) ;

    /* order by STRTTIME  */
    framegen_calls_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(FRAMEGEN_CALLS),
        where_clause, APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_STRTTIME), 
        APS_CDEFS(FRAMEGEN_CALLS), ALL_COLS) ;
    if (framegen_calls_list == NULL)
    {
        (void)sprintf(msg,
            "%s(%d):  Error in Sybase query on framegen_calls relation %s",
            __FILE__, __LINE__, where_clause ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY ;

    }
    if ( NUMELTS( framegen_calls_list ) == 0 )
    {
        DEL_LIST( framegen_calls_list ) ;
        (void)fprintf(logfp, "%s(%d):  No expired records to report when checking %s\n",
            __FILE__, __LINE__, where_clause ) ;
        return APS_FRAMEGEN_OK ;
    }
    /*
    -- For each data-take: 
    -- check its current status in the dtk relation.  If the 
    -- status != SCH, report this data-take as rejected.
    */
    for ( framegen_calls_rec = (DB_RECORD **)
                FIRST(framegen_calls_list, framegen_calls_ptr) ;
          framegen_calls_rec ;
          framegen_calls_rec = (DB_RECORD **)
                NEXT(framegen_calls_list, framegen_calls_ptr)
        )
    {
        /* retrieve the corresponding dtk rec from the database:  */
        (void)sprintf( where_clause,
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
            APS_COL(DTK, DTK_SAT),
            CAST_FRAMEGEN_CALLS_SAT
                framegen_calls_rec[FRAMEGEN_CALLS_SAT],
            APS_COL(DTK, DTK_SENSOR),
            CAST_FRAMEGEN_CALLS_SENSOR
                framegen_calls_rec[FRAMEGEN_CALLS_SENSOR],
            APS_COL(DTK, DTK_REV),
            CAST_FRAMEGEN_CALLS_REV
                framegen_calls_rec[FRAMEGEN_CALLS_REV],
            APS_COL(DTK, DTK_DTKID),
            CAST_FRAMEGEN_CALLS_DTKID
                framegen_calls_rec[FRAMEGEN_CALLS_DTKID] ) ;
 
        dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
            APS_TABLE(DTK),
            where_clause, NULL, APS_CDEFS(DTK), ALL_COLS) ;
        if ( dtk_list == NULL )
        {
            DEL_LIST( framegen_calls_list ) ;
            (void)sprintf(msg,
                "%s(%d):  Error in Sybase query on framegen_calls relation %s",
                __FILE__, __LINE__, where_clause ) ;
            (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
            aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
            return APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY ;
        }
        if ( NUMELTS( dtk_list ) == 0 )
        {
            DEL_LIST( dtk_list ) ; 
            continue ;
        }
        if ( NUMELTS( dtk_list ) != 1 )
        {
            DEL_LIST( framegen_calls_list ) ;
            DEL_LIST( dtk_list ) ; 
            (void)sprintf(msg, "%s(%d):  Error: nrecs > 1 in Sybase query %s",
                __FILE__, __LINE__, where_clause ) ;
            (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
            aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
            return APS_FRAMEGEN_ERROR_IN_FRAMEGEN_CALLS_RELATION ;
        }

        /* check the single data-take retrieved:  */
        dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;
        if ( strcmp( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "SCH" ) )
        {
            /* 
            -- data-take is not SCH.  
            -- previously reported data-take did not actually 
            -- downlink and never will; it was not scheduled.  
            -- we must report it as REJ:  
            */
            (void)strcpy( CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT], "REJ" ) ;
            return_code = report_dtk( progname, logfp, APS_dbproc, dtk_rec, 
                non_fatal_error_count  ) ;
            /* If there was an ERROR, exit the program.   */
            if ( return_code < 0 )
            {
                DEL_LIST( framegen_calls_list ) ;
                DEL_LIST( dtk_list ) ; 
                return return_code ;
            }
            reported_recs ++ ;
        }
        DEL_LIST( dtk_list ) ;

    } /* end for each framegen_calls record  */

    DEL_LIST( framegen_calls_list ) ;
    (void)fprintf(logfp, "%s(%d):  Number of expired data-takes reported:  %d\n",
            __FILE__, __LINE__, reported_recs ) ;

    return APS_FRAMEGEN_OK ;
}
