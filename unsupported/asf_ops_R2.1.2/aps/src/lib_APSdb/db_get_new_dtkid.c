#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       db_get_new_dtkid.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

This function is implemented and supported at several levels:

Unix command line                        aps_get_sequence_number
C function with userid/password/dbname   aps_get_sequence_number()
C function with dbproc                   db_get_new_dtkid()
SQL stored procedure                     aps_sp_new_dtkid

==============================================================================*/
#pragma ident   "@(#)db_get_new_dtkid.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_APSdb/SCCS/s.db_get_new_dtkid.c"


/*==============================================================================
Function:       db_get_new_dtkid()

Description:    execute the stored procedure to get a new dtkid 
                for the input sat/rev number.  Returns the new dtkid.  

Returns:        > 0 :  the new dtkid

                < 0 :  an error message decoded by APS_NEW_DTKID_ERROR_MESSAGE
                       usage is like this:
                printf("%s\n", APS_NEW_DTKID_ERROR_MESSAGE(return_code) );

Creator:        Lawrence Stevens

Creation Date:  Mon May  5 15:48:08 PDT 1997

Notes:          
==============================================================================*/
#include <string.h>
#include <stdlib.h>
#include <db_sybint.h>
#include <nmalloc.h>
#include "aps_get_sequence_number.h"

int db_get_new_dtkid(
    DBPROCESS   *APS_dbproc, /* input sybase process pointer.                */
    char        *sat,        /* input 2-char satellite (platform): E2/R1 etc.*/
    int         rev )        /* input rev number.  > 0                       */
{
    int     return_code ;
    int     proc_return_status = 0 ;
    int     new_dtkid = -1 ;

    /* allocate enough bytes for clause:  */
    char    *phase_where_clause = "where sat = 'E1'  " ;

    /* initialize each time:  */
    char    *sql_cmd = "aps_sp_new_dtkid   ,        " ;
                    /*  aps_sp_new_dtkid ss, rrrrrrr  */
                    /*  012345678901234567890123456  */
    /* 
    -- within sql_cmd, the sat value goes at offset 17, the 
    -- rev at offset 20 
    */
    #define SAT_OFFSET 17
    #define REV_OFFSET 21


    if( APS_dbproc == NULL )
        return APS_NEW_DTKID_ERROR_NULL_DBPROC ;

    if( sat == NULL )
        return APS_NEW_DTKID_ERROR_NULL_SAT ;

    if( rev <= 0 )
        return APS_NEW_DTKID_ERROR_INPUT_REV_LE_ZERO ;

    if( rev >= 9999999 )
        return APS_NEW_DTKID_ERROR_REV_GE_9999999 ;

    if( strlen(sat) != 2 )
        return APS_NEW_DTKID_ERROR_SAT_STRLEN_NE_2 ;


    /* 
    -- validate the satellite by counting recs 
    -- for it in the phase table:  
    */
    (void) sprintf(phase_where_clause, "where sat = '%s'", sat ) ;
    return_code = db_num_records( APS_dbproc, "phase", phase_where_clause) ;
    if( return_code < 1 )
        return APS_NEW_DTKID_ERROR_INVALID_SAT_VALUE ;

    /* 
    -- execute the stored procedure 
    -- aps_sp_new_dtkid <sat>, <rev>
    */

    /* 
    -- complete the command with sat and 
    -- rev values:  
    */
    (void) strncpy( sql_cmd+SAT_OFFSET, sat, 2 ) ;
    (void) sprintf( sql_cmd+REV_OFFSET, "%d", rev ) ;

    dbcmd( APS_dbproc, sql_cmd) ;
    dbsqlexec(APS_dbproc) ;

    while ((return_code = dbresults(APS_dbproc)) != NO_MORE_RESULTS)
    {
        if (return_code == SUCCEED)
        {
            /* 
            -- check proc_return_status:  
            -- if it is bad, we still have to 
            -- complete the loops to please sybase.  
            */
            proc_return_status = dbretstatus( APS_dbproc ) ;
            if( proc_return_status == 0 )
            {
                /* set up binding:  */
                dbbind(APS_dbproc, 1, INTBIND, (DBINT) 0, 
                    (BYTE *) &new_dtkid ) ;

                /* get return value.  */
                dbnextrow(APS_dbproc) ;

                /* value is obtained:  */
                return(new_dtkid) ;
            }
        }
    }

    /* no ROWS.  */
    if( proc_return_status < 0 )
        return proc_return_status ;
 
    return APS_NEW_DTKID_ERROR_NO_RETURNED_VALUE_FROM_SP_CALL ;

}
