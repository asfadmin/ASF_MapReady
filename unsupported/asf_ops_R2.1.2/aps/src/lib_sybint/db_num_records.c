#define REDUCED_PRINT

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       db_num_records.c

Description:    counts records in a table that satisfy a rev clause.  

Notes:          

==============================================================================*/
#pragma ident   "@(#)db_num_records.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_num_records.c"


/*==============================================================================
Function:       db_num_records()

Description:    counts records in a table that satisfy a rev clause.  

Parameters:
    DBPROCESS *dbproc
    char *table
    char *where_clause
 
Returns:        int
    Number of rows
    -1 if error

Creator:        Lawrence Stevens

Creation Date:  Tue May  6 15:37:26 PDT 1997

Notes:          
    SAMPLE: db_num_records(dbproc, "table name", "where colname = 10") ;
==============================================================================*/
#include <db_sybint.h>   /* for sybase stuff.     */
#include <stdlib.h>      /* for malloc()          */
#include <string.h>      /* for strlen()          */
int db_num_records(DBPROCESS *dbproc, char *table, char *where_clause)
{
    /* memory cache for readonly session:  */
    static DBPROCESS        *APS_READER_dbproc = NULL ;

    char                    *blank = " " ;
    RETCODE                 return_code ;
 
    int numrows ;
 
    static char *select_count_phrase = "select count(*) from" ;
    char *stmt ;
 
    if ( dbproc == DB_SYBINT_USE_APS_READER_DBPROC )
    {
        /*
        -- the calling routine wants to use the
        -- aps_reader account to read the database.
        */
        /*
        -- if not already opened, open an
        -- aps_reader session.
        */
        if ( APS_READER_dbproc == NULL )
        {
            /* open the database for the aps_reader session.  */
            if ( !db_open_APS_readonly( &APS_READER_dbproc ) )
                return NULL ;
        }
        /*
        -- switch to the APS_READER session.
        */
        /*
        -- this new value is not passed back to the calling
        -- routine.
        */
        dbproc = APS_READER_dbproc ;
    }
 
    if (!where_clause)
        where_clause = blank ;
    else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
        where_clause = blank ;
 
    /*
    -- allocating memory; free it later.
    */
    stmt = (char *)malloc(
        strlen(select_count_phrase)
        + strlen(table)
        + strlen(where_clause)
        + 5) ;  /* extra for spaces and null */
 
    sprintf(stmt, "%s %s %s",
        select_count_phrase, table, where_clause) ;
 
#ifndef REDUCED_PRINT
    printf("COUNT QUERY: %s\n\n", stmt) ;
#endif
 
    dbcmd(dbproc, stmt) ;
    dbsqlexec(dbproc) ;
 
    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if (return_code == SUCCEED)
        {
            dbbind(dbproc, 1, INTBIND, (DBINT) 0, (BYTE *) &numrows) ;
            dbnextrow(dbproc) ;
 
#ifndef REDUCED_PRINT
            printf("RECORD COUNT: %d\n\n", numrows) ;
#endif
            free( stmt ) ;
            return(numrows) ;
        }
    }
    free( stmt ) ;
    return(-1) ;
}

