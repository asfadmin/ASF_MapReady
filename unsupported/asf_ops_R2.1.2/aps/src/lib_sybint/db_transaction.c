#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:        db_transaction.c

External Functions Defined:
    db_begin_tran.c
    db_commit_tran.c
    db_rollback_tran.c
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:            
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)db_transaction.c	5.1 98/01/08 APS/ASF"
#pragma ident    "@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_transaction.c"


/*==============================================================================
Function:       db_begin_tran()

Description:    begin transaction - Sybase ISQL

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 19:29:56 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

#include "db_sybint.h"      /* for sybase prototypes like dbcmd()   */
#include "dapps_defs.h"     /* for TRUE & FALSE                     */

int db_begin_tran( DBPROCESS   *dbproc )
{

    RETCODE     return_code ;

    dbcmd(dbproc, "begin transaction" ) ;
    return_code = dbsqlexec(dbproc) ;

    if( return_code == SUCCEED )
        return TRUE ;
    else
        return FALSE ;

}


/*==============================================================================
Function:       db_commit_tran()

Description:    commit transaction - Sybase SQL

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 19:39:28 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int db_commit_tran( DBPROCESS   *dbproc )
{

    RETCODE     return_code ;

    dbcmd(dbproc, "commit transaction" ) ;
    return_code = dbsqlexec(dbproc) ;

    if( return_code == SUCCEED )
        return TRUE ;
    else
        return FALSE ;

}


/*==============================================================================
Function:       db_rollback_tran()

Description:    rollback transaction - Sybase SQL

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 19:39:28 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
int db_rollback_tran( DBPROCESS   *dbproc )
{

    RETCODE     return_code ;

    dbcmd(dbproc, "rollback transaction" ) ;
    return_code = dbsqlexec(dbproc) ;

    if( return_code == SUCCEED )
        return TRUE ;
    else
        return FALSE ;

}
