#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       dtk_was_reported.c

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
#pragma ident   "@(#)dtk_was_reported.c	5.2 98/03/04 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/framegen/SCCS/s.dtk_was_reported.c"


/*==============================================================================
Function:       dtk_was_reported()

Description:    returns TRUE if a data-take was previously reported.  
                also returns frame gen calls record if desired.  
                returns FALSE if not previously reported, < 0 if error.  

Creator:        Lawrence Stevens

Creation Date:  Fri Mar 22 17:35:36 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "aps_framegen.h"  /* local include file for this executable.  */

int dtk_was_reported( 
    char            *progname,
    FILE            *logfp,                /* log file pointer.    */
    DB_RECORD       **dtk_rec,             /* input data-take.   */
    DB_RECORD       **framegen_calls_rec ) /* if NULL, then don't return data */
{

    char            msg[MSG_LEN] ;

    DB_RECORD       **framegen_calls_rec_temp ;
    cursor          framegen_calls_ptr ;
    llist           *framegen_calls_list ;

    /*
    -- [Note:  "previously reported" means that there is a record in the 
    --  framegen_calls relation for the data-take in question. ] 
    -- 
    -- Check the framegen_calls relation to see if this data-take 
    --     has been previously reported.  
    */

    if ( dtk_rec == NULL )
        return APS_FRAMEGEN_ERROR_DTK_REC_IS_NULL ;

    (void)sprintf( where_clause, 
        "where %s = '%s' and %s = %ld and %s = %d", 
        APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_SAT), 
            CAST_DTK_SAT dtk_rec[DTK_SAT],
        APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_REV), 
            CAST_DTK_REV dtk_rec[DTK_REV],
        APS_COL(FRAMEGEN_CALLS, FRAMEGEN_CALLS_DTKID), 
            CAST_DTK_DTKID dtk_rec[DTK_DTKID]  ) ;

    framegen_calls_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(FRAMEGEN_CALLS), where_clause, NULL, 
        APS_CDEFS(FRAMEGEN_CALLS), ALL_COLS) ;
    if ( framegen_calls_list == NULL )
    {
        (void)sprintf(msg, 
            "%s(%d):  Error in Sybase query on framegen_calls relation %s",
            __FILE__, __LINE__, where_clause ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_CRITICAL, msg, DO_SYSLOG, DO_PRINT);
        return APS_FRAMEGEN_ERROR_IN_SYBASE_QUERY ;
    }

    if ( NUMELTS( framegen_calls_list ) > 1 
    ||   NUMELTS( framegen_calls_list ) < 0  )
    {
        (void)sprintf(msg, 
"%s(%d):  Error: nrecs != 1 and != 0 in Sybase query on framegen_calls %s",
            __FILE__, __LINE__, where_clause ) ;
        (void)fprintf(logfp, "%s:\n\n%s\n", progname, msg ) ;
        aps_log_msg(progname, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        DEL_LIST( framegen_calls_list ) ;
        return APS_FRAMEGEN_ERROR_IN_FRAMEGEN_CALLS_RELATION ;
    }

    if ( NUMELTS( framegen_calls_list ) == 0 )
    {
        DEL_LIST( framegen_calls_list ) ;
        return FALSE ;
    }

    /*  NUMELTS( framegen_calls_list ) == 1   */

    /* 
    -- return the FRAMEGEN_CALLS rec data if requested
    -- by the calling routine setting the input argument 
    -- framegen_calls_rec != NULL.  
    */
    if ( framegen_calls_rec ) 
    {
        framegen_calls_rec_temp = (DB_RECORD **) FIRST(framegen_calls_list, 
            framegen_calls_ptr ) ;
        db_copy_record ( APS_CDEFS(FRAMEGEN_CALLS), framegen_calls_rec, 
            framegen_calls_rec_temp ) ;
    }

    /* don't need to free framegen_calls_rec_temp */
    DEL_LIST( framegen_calls_list ) ;

    return TRUE ;
}
