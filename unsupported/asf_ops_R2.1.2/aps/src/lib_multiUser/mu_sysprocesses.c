#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       mu_sysprocesses.c

External Functions Defined:
mu_get_sysprocesses_khph()
mu_count_sysprocesses_khph()
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

==============================================================================*/
#pragma ident   "@(#)mu_sysprocesses.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_multiUser/SCCS/s.mu_sysprocesses.c"


#include <string.h>
#include "mu.h"

/* 
-- this is info about the sybase pseudo-table 
-- sysproceses.  
-- There are many more fields, but these fields are the only 
-- ones we use.  
-- These macros are created to be just like what you
-- see in the db_<tablename>.h include files.  They
-- make it easy to use db_get_records() and other db_routines in
-- lib_sybint.  sysprocesses is accessed ONLY in this file.  
*/
#define SYSPROCESSES_KPID                      0
#define SYSPROCESSES_HOSTNAME                  1
#define SYSPROCESSES_PROGRAM_NAME              2
#define SYSPROCESSES_HOSTPROCESS               3
#define NUM_SYSPROCESSES_COLS                  4
 
#define CAST_SYSPROCESSES_KPID         *(DBINT *)
#define CAST_SYSPROCESSES_HOSTNAME       (char *)
#define CAST_SYSPROCESSES_PROGRAM_NAME   (char *)
#define CAST_SYSPROCESSES_HOSTPROCESS    (char *)

/*
-- This code is analogous to the code in lib_APSdb/aps_db_table.c
-- It makes it possible to use db_get_records() and other 
-- routines in lib_sybint for sysprocesses.  sysprocesses 
-- is accessed ONLY in this file.  
*/
static COLUMN_DEFS sysprocesses_columns[NUM_SYSPROCESSES_COLS+1] =
{
    {"master..sysprocesses.kpid",           4,    INTBIND,       "%d"},
    {"master..sysprocesses.hostname",      10+1,  NTBSTRINGBIND, "%s"},
    {"master..sysprocesses.program_name",  16+1,  NTBSTRINGBIND, "%s"},
    {"master..sysprocesses.hostprocess",    8+1,  NTBSTRINGBIND, "%s"},
    {NULL, 0, 0, NULL}
} ;


/*==============================================================================
Function:       mu_get_sysprocesses_khph()

Description:    A utility routine which gets the values of 
                Kpid, Hostname, Programe name, and Host process id, 
                from the Sybase pseudo-table sysprocesses, 
                for the current process, returned in parameters.

Returs:         TRUE if ok, < 0 if there is an error.  

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 20:29:32 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/
#include "db_sybint.h"      /* for sybase prototypes like dbcmd()   */
#include "dapps_defs.h"     /* for TRUE & FALSE                     */

int mu_get_sysprocesses_khph( 
    DBPROCESS   *dbproc,      /*  input sybase session pointer               */
    int         *kpid,        /*  output current Sybase kernel process id    */
    char        *hostname,    /*  output current nodename.  allow 11 chars   */
    char        *progname,    /*  output current programe name.  allow 17 char*/
    char        *hostprocess_id ) /*  output current process id, char string 
                                    allow 9 characters    */
{

    DB_RECORD   **sysprocesses_rec ;
    llist       *sysprocesses_list = NULL ;
    cursor      sysprocesses_list_ptr ;

    char        *where_clause_current = "where spid = @@spid" ;

    /* some error checking  */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;

    sysprocesses_list = db_get_records(dbproc, 
        "master..sysprocesses", where_clause_current, NULL, 
        sysprocesses_columns, ALL_COLS) ;
    if (sysprocesses_list == NULL)
        return MU_DB_ERROR_DURING_SYSPROCESSES_RETRIEVE ;

    if ( NUMELTS(sysprocesses_list) <= 0 )
    {
        DEL_LIST(sysprocesses_list) ;
        return MU_ERROR_COULD_NOT_FIND_SYSPROCESSES_INFO_FOR_CURRENT_PROCESS ;
    }

    #ifdef PRINT_DIAG
    db_print_list(sysprocesses_list, sysprocesses_columns ) ;
    #endif

    sysprocesses_rec = (DB_RECORD **) 
                            FIRST(sysprocesses_list, sysprocesses_list_ptr) ;

    (void) strcpy( hostname, 
        CAST_SYSPROCESSES_HOSTNAME 
        sysprocesses_rec[SYSPROCESSES_HOSTNAME] ) ;

    (void) strcpy( hostprocess_id, 
        CAST_SYSPROCESSES_HOSTPROCESS 
        sysprocesses_rec[SYSPROCESSES_HOSTPROCESS] ) ;

    if( strlen( CAST_SYSPROCESSES_PROGRAM_NAME 
                sysprocesses_rec[SYSPROCESSES_PROGRAM_NAME]) == 0 )
    {
        return 
        MU_ERROR_NO_SYSPROCESSES_PROG_NAME_CHECK_DBOPEN_CALL_2ND_ARGUMENT ;
    }

    (void) strcpy( progname, 
        CAST_SYSPROCESSES_PROGRAM_NAME 
        sysprocesses_rec[SYSPROCESSES_PROGRAM_NAME]);

    *kpid = CAST_SYSPROCESSES_KPID sysprocesses_rec[SYSPROCESSES_KPID] ;

    DEL_LIST(sysprocesses_list) ;

    return TRUE ;

}


/*==============================================================================
Function:       mu_count_sysprocesses_khph()

Description:    A utility routine which counts records in the
                Sybase pseudo-table sysprocesses, which have 
                the values of the input parameters:
                Kpid, Hostname, Program name, and Hostprocess id, 

Creator:        Lawrence Stevens

Creation Date:  Tue Nov 19 20:29:32 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
==============================================================================*/

int mu_count_sysprocesses_khph( 
    DBPROCESS   *dbproc,          /* input sybase session pointer          */
    int         kpid,             /* input kernel process id in Sybase     */
    char        *hostname,        /* input nodename                        */
    char        *progname,        /* input programe name                   */
    char        *hostprocess_id ) /* input process id, character string    */
{

    int         nrecs ;

    /* 
    -- some error checking for implementation help  
    */
    if( dbproc == NULL )
        return MU_ERROR_INPUT_DBPROC_IS_NULL ;
    if( kpid == 0 )
        return MU_ERROR_INPUT_KPID_IS_ZERO ;
    if( hostname == NULL )
        return MU_ERROR_INPUT_HOSTNAME_POINTER_IS_NULL ;
    if( progname == NULL )
        return MU_ERROR_INPUT_PROGNAME_POINTER_IS_NULL ;
    if( hostprocess_id == NULL )
        return MU_ERROR_INPUT_HOSTPROCESS_POINTER_IS_NULL ;
    if( strlen(hostname) <= 0 )
        return MU_ERROR_INPUT_HOSTNAME_STRING_ZERO_LENGTH ;
    if( strlen(progname) <= 0 )
        return MU_ERROR_INPUT_PROGNAME_STRING_ZERO_LENGTH ;
    if( strlen(hostprocess_id) <= 0 )
        return MU_ERROR_INPUT_HOSTPROCESS_STRING_ZERO_LENGTH ;

    (void) sprintf( where_clause, 
        " where kpid = %d and hostname = '%s' and program_name = '%s' and hostprocess = '%s' ",
        kpid, hostname, progname, hostprocess_id ) ;

    nrecs = db_num_records(dbproc, "master..sysprocesses", where_clause ) ;

    if( nrecs < 0 )
        return MU_DB_ERROR_DURING_SYSPROCESSES_COUNTING ;

    return nrecs ;

}
