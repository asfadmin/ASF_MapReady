#define REDUCED_PRINT

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       db_get_min_value.c

Description:    get min value from the db, given a where clause.  

==============================================================================*/
#pragma ident   "@(#)db_get_min_value.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_get_min_value.c"


/*==============================================================================
Function:       db_get_min_value()

Description:    given a table, a field, and a where_clause, 
                determine the min value.  

                Example of the SQL query:
    select min (dtk.sat) from dtk 
    where dtk.sat = "A1" and dtk.sensor = "SAR"

Creator:        Lawrence Stevens

Creation Date:  Mon Apr 28 14:53:18 PDT 1997

==============================================================================*/
#include <string.h>
#include <stdlib.h>
#include <db_sybint.h>
#include <nmalloc.h>

static DBPROCESS    *APS_READER_dbproc ;

int db_get_min_value(
    DBPROCESS   *dbproc, 
    char        *table,         /* APS_TABLE(DTK) or "dtk"                  */
    int         col_index,      /* DTK_REV        or 2                      */
    char        *where_clause,  /* "where dtk.sat = 'A1'"                   */
    COLUMN_DEFS *columns,       /* APS_CDEFS(DTK),                          */
    void        *min_value_ptr )/* result goes here.  caller provides space */
{
    RETCODE return_code ;
 
    static char *select_min_phrase = "select min(" ;
    static char *from_phrase = ") from " ;
    char        *stmt ;
    char        *blank = " " ;
 
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
    -- string example is:  
    -- "select min (dtk.sat) from dtk 
    --  where dtk.sat = 'A1' and dtk.sensor = 'SAR'"
    */
    stmt = NEW(
        strlen(select_min_phrase)
        + strlen( columns[col_index].name )
        + strlen(from_phrase)
        + strlen(table)
        + strlen(where_clause)
        + 5) ;  /* extra for the between spaces and for the null */

    sprintf(stmt, "%s %s %s %s %s",
        select_min_phrase, 
        columns[col_index].name, 
        from_phrase,
        table, 
        where_clause) ;
 
#ifndef REDUCED_PRINT
    printf("MIN QUERY: %s\n\n", stmt) ;
#endif
 
    dbcmd(dbproc, stmt) ;
    dbsqlexec(dbproc) ;
 
    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if (return_code == SUCCEED)
        {
            dbbind(dbproc, 1, columns[col_index].bindtype, (DBINT) 0, 
                (BYTE *) min_value_ptr) ;
            dbnextrow(dbproc) ;
 
#ifndef REDUCED_PRINT
            switch( columns[col_index].bindtype )
            {
            case NTBSTRINGBIND:
                printf("MIN NTBSTRINGBIND:  %s\n", (char *)min_value_ptr ) ;
                break ;
            case INTBIND:
                printf("MIN INTBIND:   %d\n", *(int *) min_value_ptr ) ;
                break ;
            case TINYBIND:
                printf("MIN TINYBIND:  %d\n", (int) (*(char *) min_value_ptr) );
                break ;
            case CHARBIND:
                printf("MIN CHARBIND:  %c\n", *(char *) min_value_ptr ) ;
                break ;
            case REALBIND:
                printf("MIN REALBIND:  %f\n", *(float *) min_value_ptr ) ;
                break ;
            case SMALLBIND:
                printf("MIN SMALLBIND:  %d\n", *(short *) min_value_ptr ) ;
                break ;
            case FLT8BIND:
                printf("MIN FLT8BIND:  %f\n", *(double *) min_value_ptr ) ;
                break ;
            default :
                printf("Unknown bindtype for column.\n" ) ;
                break ;
            } 
#endif
            free( stmt ) ;
            return(TRUE) ;
        }
    }
    /* no ROWS.  */
    free( stmt ) ;
    return FALSE  ;
}
