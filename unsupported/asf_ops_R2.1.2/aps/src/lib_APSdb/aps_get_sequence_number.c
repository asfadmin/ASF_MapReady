#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       aps_get_sequence_number.c

Notes:          
This function is implemented and supported at several levels:
 
Unix command line                        aps_get_sequence_number
C function with userid/password/dbname   aps_get_sequence_number()
C function with dbproc                   db_get_new_dtkid()
SQL stored procedure                     aps_sp_new_dtkid


==============================================================================*/
#pragma ident   "@(#)aps_get_sequence_number.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_APSdb/SCCS/s.aps_get_sequence_number.c"


/*==============================================================================
Function:       aps_get_sequence_number()

Description:    opens the db and obtains a valid, new dtkid for the 
                input sat, rev number.  The dtkid is returned.  


Creator:        Lawrence Stevens

Creation Date:  Mon May  5 15:23:05 PDT 1997

Notes:          
==============================================================================*/
#include "aps_get_sequence_number.h"
#include <stdlib.h>

int aps_get_sequence_number(
    char    *progname,        /* input progname needed by sybase             */
    char    *sybase_userid,   /* input userid                                */
    char    *sybase_password, /* input password                              */
    char    *dbname,          /* input db name                               */
    char    *sat,             /* input 2-char satellite or platform          */
    int     rev,              /* input rev number                            */
    char    *sybase_server )  /* input sybase server name: NULL uses DSQUERY */
{

    DBPROCESS   *APS_dbproc = NULL ;

    int     return_code ;

    /* quick error checking  */
    if( sybase_userid == NULL )
        return APS_NEW_DTKID_ERROR_NULL_SYBASE_USERID ;

    if( sybase_password == NULL )
        return APS_NEW_DTKID_ERROR_NULL_SYBASE_PASSWORD ;

    if( dbname == NULL )
        return APS_NEW_DTKID_ERROR_NULL_DBNAME ;

    /*
    -- open the database
    -- do not install any message handlers.  
    -- the calling program does this.  
    -- this is convenient for the applications 
    -- using this routine.  
    */

    APS_dbproc = db_open_server( dbname, progname, sybase_userid, 
        sybase_password, NULL, NULL, &return_code, 
        sybase_server );
    if(return_code != DB_OPEN_OK)
        return APS_NEW_DTKID_ERROR_DB_NOT_OPENED_OK ;

    /*
    -- call the stored procedure:
    */
    return_code = db_get_new_dtkid(APS_dbproc, sat, rev ) ;

    /*
    --  Don't forget to close the db, like 
    --  I originally forgot to:
    */
    db_close( APS_dbproc ) ;

    return return_code ;
}
