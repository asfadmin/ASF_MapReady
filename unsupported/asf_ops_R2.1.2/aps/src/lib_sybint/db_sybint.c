#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#define REDUCED_PRINT

/*==============================================================================
Filename:   db_sybint.c

Description:

    This file contains functions that interface with Sybase via the
Open Client DB-Library.  Data to be stored/retrieved from the database
makes use of a linked list structure with the data records being the
links.  See the dapp_list library for more on the linked list.

External Functions:

    db_quote_doubler
    free_db_record
    new_db_record
    new_table_record
    db_count_distinct_vals
    db_make_colnames_string
    db_make_format_string   -- unused.  --
    db_get_column_data
    db_get_records
    db_insert_values
    db_print_record
    db_fprint_record
    db_print_list
    db_fprint_list
    db_insert_records
    db_insert_single_record
    db_delete_records
    db_update_records
    db_nth_record
    db_read_records_from_ingres_file
    db_ftn_first_record
    db_ftn_next_record
    db_copy_record
    db_duplicate_record
    db_record_llist_move
    db_record_llist_copy
    move_db_record2new_llist
    db_get_min_record
    db_get_max_record
    move_db_record_matches2llist
    copy_db_record_matches2llist

Static Functions:
    trimstring
    static int read_col_from_file(int fd, char *buf)

External Variables Defined:

    char where_clause[1024] ;
    char orderby_cols[1024] ;
    char fields_to_set[1024] ;

    char format_string[1024] ;
    char col_names[1024] ;
    char col_values[1024] ;
    char tables[256] ;


File Scope Static Variables:

Notes:

Function:
==============================================================================*/
#pragma ident   "@(#)db_sybint.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_sybint.c"


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <varargs.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>

#include "db_sybint.h"
#include "aps.h"        /* for aps_internal_error(), other src/aps routines  */
#include "aps_db_table.h"   /* for APS DB table  definitions                */

#include "dapps_list.h"
#include "nmalloc.h"

#define COLNAMES_PER_LINE 4

static char *blank = " " ;

/*
-- this permanently holds the APS_READER session info
-- pointer, to be used only by the routines in this file.
*/
static DBPROCESS    *APS_READER_dbproc = NULL ;

char where_clause[1024] ;
char orderby_cols[1024] ;
char fields_to_set[1024] ;

char format_string[1024] ;
char col_names[1024] ;
char col_values[1024] ;
char tables[256] ;

char aps_message[1024] ;

int llist_errno = 0 ;

/* information for sybase specific data */
char sys_tables[] = "syscolumns c, sysobjects o" ;

COLUMN_DEFS sybsys_columns[] =
{
    {"c.name", 30, STRINGBIND, "%30s"},
    {"c.type", sizeof(DBINT), INTBIND, "%d"},
    {NULL, 0, 0, NULL}
} ;


/*==============================================================================
Function:       trimstring

Description:    Used to trim leading and trailing characters from strings

Parameters:     char * - string to be trimmed

Returns:        char * - pointer to modified string

Creator:        ???

Creation Date:  11/03/1994

Notes:
        Originally called  IC_trimString
        Borrowed from code on an IMS/DADs system
==============================================================================*/
static char * trimstring(char *Str)
{
   int i = 0, j = 0, Blank = 0;

   while (Str[i] != NULL && isspace(Str[i]))
     i++;

   while (Str[i] != NULL)
    {
     Blank = 0;
     while (Str[i] != NULL && !isspace(Str[i]))
       Str[j++] = Str[i++];
     if (Str[i] != NULL && isspace(Str[i]))
      {
       Str[j++] = ' ';
       i++;
       Blank = 1;
      }
     while (Str[i] != NULL && isspace(Str[i]))
       i++;
    }

   if (Blank)
     Str[j-1] = '\0';
   else
     Str[j] = '\0';
    return(Str) ;
}

/*==============================================================================
Function:       free_db_record

Description:
    Free the memory associated with a db record.  This function is primarily
passed as the delete function for the link list.

Parameters:     DB_RECORD **

Returns:        void

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
==============================================================================*/
void free_db_record(DB_RECORD **recordptr)
{
    int i ;

    /* free each column in the database record */
    for (i = 0 ; i < MAXITEMS ; i++)
    {
        if (recordptr[i])
            FREE(recordptr[i]) ;
    }

    /* now free the pointer to the columns */
    FREE(recordptr) ;
}



/*==============================================================================
Function:       new_db_record

Description:
    Allocates and initializes memory to hold data for a DB_RECORD
    DB_RECORD is an array of void pointers which are initialized to NULL

Parameters:     None

Returns:        DB_RECORD **

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
==============================================================================*/
DB_RECORD** new_db_record()
{
    DB_RECORD **recordptr ;
    int i ;

    recordptr = (DB_RECORD **) NEW(sizeof(DB_RECORD)) ;
    for (i = 0 ; i < MAXITEMS; i++)
        recordptr[i] = NULL ;
    return(recordptr) ;
}



/*==============================================================================
Function:       new_table_record

Description:    creates a DB_RECORD with space allocated for each column of the table

Parameters:     COLUMN_DEFS *columns   column table definition

Returns:        DB_RECORD **

Creator:        Ron Green

Creation Date:  11/03/1994

Notes:
==============================================================================*/
DB_RECORD**  new_table_record(COLUMN_DEFS *columns)
{
    DB_RECORD **record ;
    int i = 0 ;

    record = new_db_record() ;
    while (columns[i].size)
    {
        record[i] = (void *) ZNEW(columns[i].size) ;
        i++ ;
    }

    return(record) ;
}


/*==============================================================================
Function:       db_count_distinct_vals()

Description:    count the distinct (unique) values in the field of the 
                indicated db relation.  

Returns:        -1 if error, >= 0 if there is a count.  

Creator:        Lawrence Stevens

Creation Date:  Sat Dec  7 23:32:08 PST 1996

Notes:      
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  
EXAMPLE OF USE:
    ( count the number of different satellites in the phase relation: )
    n_sats = db_count_distinct_vals( DB_SYBINT_USE_APS_READER_DBPROC,
        APS_TABLE(PHASE), PHASE_SAT, APS_CDEFS(PHASE), NULL ) ;
==============================================================================*/ 
int db_count_distinct_vals(
    DBPROCESS   *dbproc,
    char        *table,
    int         column_index,
    COLUMN_DEFS *columns,
    char        *where_clause )
{
 
    int                 n_distinct_values ;
 
    RETCODE             return_code ;
 
    static char         *select_count_phrase = "select count( distinct" ;
    static char         *from = ") from" ;
    char                *stmt ;
 
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
    else /* a real where clause, trim any excess tabs, control chars, etc.. */
        where_clause = trimstring(where_clause) ;
 
    /*
    -- allocate memory; must FREE() this memory later:
    -- form of SQL statement will be:
    -- "select count ( distinct <column_name> ) from <table> <where_clause>"
    --  ^^^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^ ^^^^^^ ^^^^^^^ ^^^^^^^^^^^^^^
    --  <---  select_count_phrase    ------> <-  column ->   from   table   where_clause
    */
    stmt = (char *) NEW(
        strlen(select_count_phrase)
        + strlen(columns[column_index].name)
        + strlen(from)
        + strlen(table)
        + strlen(where_clause)
        + 5) ; /* extra chars for added blanks between arguments and null */
 
    sprintf(stmt, "%s %s %s %s\n%s",
        select_count_phrase, columns[column_index].name, from, table,
        where_clause ) ;
 
#ifdef PRINT_DIAG
    printf("QUERY: %s\n", stmt) ;
#endif
 
    /*
    -- Now get the count from the database
    */
 
    dbcmd(dbproc, stmt) ;
    if (dbsqlexec(dbproc) == FAIL)
    {
        FREE( stmt ) ;
        return(-1) ;
    }
 
    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if (return_code == SUCCEED)
        {
            dbbind(dbproc, 1, INTBIND, (DBINT) 0, (BYTE *) &n_distinct_values) ;
            dbnextrow(dbproc) ;
 
#ifndef REDUCED_PRINT
            printf("DISTINCT VALUE COUNT: %d\n\n", n_distinct_values) ;
#endif
            free( stmt ) ;
            return(n_distinct_values) ;
        }
    }
    free( stmt ) ;
    return(-1) ;
}



#ifdef DONTUSE

/*==============================================================================
Function:       db_min_max_values

Description:
    Returns the minimum and maximum values of a column in a table

Parameters:
    DBPROCESS *dbproc
    char *table
    char *column_name

Returns:        llist *

Creator:        Ron Green

Creation Date:  10/05/1994

Notes:
    SAMPLE: db_min_max_values(dbproc, "table name", "colname") ;
==============================================================================*/
int db_min_max_values(DBPROCESS *dbproc,
    char *table, char *colname, char * where_clause)
{
    RETCODE return_code ;

    int numrows ;

    static char *select = "select" ;
    static char *min = "min" ;
    static char *max = "max" ;
    static char *from = "from" ;

    char *stmt ;

    if (!where_clause)
        where_clause = blank ;
    else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
        where_clause = blank ;

    stmt = NEW(
        strlen(select)
        + strlen(min) + 2  /* add two for parens */
        + strlen(max) + 2  /* add two for parens */
        + strlen(colname) * 2  /* use the column name twice */
        + strlen(table)
        + strlen(where_clause)
        + 7) ;  /* extra for spaces and null */

    sprintf(stmt, "%s %s %s",
        select, min, colname, max, colname, from, table, where_clause) ;

    printf("MIN/MAX QUERY: %s\n\n", stmt) ;

    dbcmd(dbproc, stmt) ;
    dbsqlexec(dbproc) ;

    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if (return_code == SUCCEED)
        {
            dbbind(dbproc, 1, INTBIND, (DBINT) 0, &numrows) ;
            dbnextrow(dbproc) ;
            return(numrows) ;
        }
    }
    return(-1) ;
}
#endif



/*==============================================================================
Function:       db_make_colnames_string

Description:    Creates a string of the column names of a given table
    Used mainly for queries to specify the ordering of columns to be returned

Parameters:
    char *table            table to make the column names for
    COLUMN_DEFS *columns   column table definition
    char *col_names        allocated string to contain the column names

Returns:

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
    This function should be modified to read the names out of the database,
rather than the static global COLUMN_DEF table.
==============================================================================*/
void db_make_colnames_string( COLUMN_DEFS *columns, char *col_names)
{
    int index = 0 ;
    char *cptr ;

    sprintf(col_names, "\n") ;
    while (columns[index].name)
    {
        sprintf(col_names, "%s%s, ", col_names, columns[index].name)  ;
        index++ ;

        /* for readability add a newline after so many column names */
        if (index % COLNAMES_PER_LINE == 0)
            sprintf(col_names, "%s\n", col_names) ;
    }

    cptr = strrchr(col_names, ',') ;
    *cptr = NULL ; /* place a NULL at the last comma */
}




/*==============================================================================
Function:       db_make_format_string

Description:
    Make a format descriptor string for columns in a table.  The string
can then be used to print the values of the data stored in a DB_RECORD

Parameters:
    char *table            table to make the column names for
    COLUMN_DEFS *columns   column table definition
    char *format_string    allocated string to contain the format specification

Returns:

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
==============================================================================*/
void db_make_format_string( COLUMN_DEFS *columns, char *format_string)
{
    int index = 0 ;
    char *cptr ;

    sprintf(format_string, "%s", blank) ;
    while (columns[index].name)
    {
        switch( columns[index].bindtype )
        {
        case CHARBIND :
            sprintf(format_string, "%s'%%c', ", format_string) ;
            break ;

        case STRINGBIND :
        case NTBSTRINGBIND :
            sprintf(format_string, "%s'%s', ",
                format_string, columns[index].format) ;
            break ;

        default :
            sprintf(format_string, "%s%s, ",
                 format_string, columns[index].format)  ;
        }
        index++ ;
    }
    cptr = strrchr(format_string, ',') ;
    *cptr = NULL ; /* place a NULL at the last comma */
}






/*==============================================================================
Function:       db_get_column_data

Description:
    This function is used to get specific information about columns
in a sybase table.

Parameters:
    char *table_name

Returns:        llist*  of the records retrieved
    The records contain information contained in table sybinfo
    name and type

Creator:        Ron Green

Creation Date:  09/30/1994

Notes:
==============================================================================*/
llist *db_get_column_data(DBPROCESS *dbproc, char *table_name)
{
    llist *sybdata_records ;

    sprintf(where_clause, "where o.name = '%s' and c.id = o.id", table_name) ;

    sybdata_records = db_get_records(dbproc, sys_tables,
        where_clause, NULL, sybsys_columns, ALL_COLS) ;

    return(sybdata_records) ;
}


/*==============================================================================
Function:       db_get_records

Description:
    Retrieves records from the tables and stores them as a link list

Parameters:
    DBPROCESS *dbproc
    char *table            primary table to get records from
    char *where_clause     where clause to limit records
    char *fields_to_order  fields the records should be sorted on
    COLUMN_DEFS *columns   column definitions for table
    ...)                   variable list of columns to be retrieved, these are
                           indices into the column definition table
        The variable list must be terminated with END_COLS
        The define ALL_COLS will cause all columns of the table to be returned


Returns:        llist*  of the records retrieved

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
    SAMPLE: db_get_records(dbproc,
        "table name", "where colname = 10", "col1, col2", tabledef, ALL_COLS) ;
==============================================================================*/
llist * db_get_records(DBPROCESS *dbproc,
    char *table,
    char *where_clause,
    char *fields_to_order,
    COLUMN_DEFS *columns,
    ...)
{

    va_list             vlist_p ;
    RETCODE             return_code ;

    DB_RECORD           tmp_record ;
    DB_RECORD           **recordptr;
    llist               *llrecords ;

    int                 i ;
    int                 col_id ;
    int                 colnames_len ;

    static char         *select = "select" ;
    static char         *from = "from" ;
    static char         *order_by_phrase = "order by" ;
    char                *order_by ;
    char                *stmt ;
    char                *colnames ;
    char                *cptr ;

    /* first get space to hold all field names if necessary */
    i = 0 ;

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

    /*
    -- changed due to memory access error.  colnames starts out with
    -- a newline character; we must allow for this by initializing
    -- colnames_len to 1, not 0 as before.
    */
    colnames_len = 1 ;
    while (columns[i].name)
    {
        colnames_len =
            colnames_len + strlen(columns[i].name) + 2 ;  /* add 2 for ', ' */
        i++ ;

        if (i % COLNAMES_PER_LINE == 0)
            colnames_len = colnames_len + 1 ;  /* add for newline */
    }

    /*
    -- must FREE() this later:
    */
    colnames = (char *) NEW(colnames_len+1) ; /* add for NULL */

    /* get the name of each field in the query */

    va_start(vlist_p) ;
    if ((col_id = va_arg(vlist_p, int)) != END_COLS)
    {
        sprintf(colnames, "%s, ", columns[col_id].name) ;
        col_id = va_arg(vlist_p, int) ;

        i = 0 ;
        while (col_id != END_COLS)
        {
            sprintf(colnames, "%s%s, ", colnames, columns[col_id].name) ;
            col_id = va_arg(vlist_p, int) ;

            i++ ;
            if (i % COLNAMES_PER_LINE == 0)
                sprintf(colnames, "%s\n", colnames) ;
        }
        cptr = strrchr(colnames, ',')   ;
        *cptr = NULL ; /* place the NULL at the last comma */
    }
    else /* get all field names */
    {
        db_make_colnames_string( columns, colnames) ;
    }
    va_end(vlist_p) ;


    if (!where_clause)
        where_clause = blank ;
    else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
        where_clause = blank ;
    else /* a real where clause, trim any excess tabs, control chars, etc.. */
        where_clause = trimstring(where_clause) ;

    if (!fields_to_order)
    {
        order_by = blank ;
        fields_to_order = blank ;
    }
    else if (strlen(fields_to_order) == 0) /* for FORTRAN NULL strings */
    {
        order_by = blank ;
        fields_to_order = blank ;
    }
    else
        order_by = order_by_phrase ;

    /*
    -- must FREE() this memory later:
    */
    stmt = (char *) NEW(
        strlen(select)
        + colnames_len + 1
        + strlen(from) + 1
        + strlen(table) + 1
        + strlen(where_clause) + 1
        + strlen(order_by) + 1
        + strlen(fields_to_order) + 1
        + 7) ; /* extra for added blanks between arguments */

    sprintf(stmt, "%s %s\n%s %s\n%s\n%s %s",
        select, colnames, from, table,
        where_clause, order_by, fields_to_order) ;

#ifndef REDUCED_PRINT
    printf("QUERY: %s\n", stmt) ;
#endif

    /*
    -- Now get the records from the database
    -- in case of error, use DEL_LIST() to free it.
    */
    llrecords = create_dyn_llist() ;

    dbcmd(dbproc, stmt) ;
    if (dbsqlexec(dbproc) == FAIL)
    {
        DEL_LIST( llrecords ) ;
        FREE( colnames ) ;
        FREE( stmt ) ;
        return(NULL) ;
    }

    /*
    -- now allocate temporary memory and bind the results of the query
    -- to the storage... the size is stored in the table definition
    */
    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if (return_code == SUCCEED)
        {
            va_start(vlist_p) ;

            /* allocate and bind specific columns */
            if ((col_id = va_arg(vlist_p, int)) != END_COLS)
            {
                i = 0 ;
                while (col_id != END_COLS)
                {
#ifdef DEBUG
                    printf("NAME: %s %d %d\n",
                        columns[col_id].name, col_id, columns[col_id].size) ;
#endif
                    tmp_record[col_id] = (void *) NEW(columns[col_id].size) ;
                    dbbind(dbproc, i+1,
                        columns[col_id].bindtype, (DBINT) 0,
                        (BYTE *) tmp_record[col_id]) ;
                    col_id = va_arg(vlist_p, int) ;
                    i++ ;
                }
            }
            else /* allocate and bind all columns */
            {
                i = 0 ;
                while (columns[i].size)
                {
#ifdef DEBUG
                    printf("NAME: %s %d %d\n",
                        columns[i].name, i, columns[i].size) ;
#endif
                    tmp_record[i] = (void *) NEW(columns[i].size) ;
                    dbbind(dbproc, i+1,
                        columns[i].bindtype, (DBINT) 0,
                        (BYTE *) tmp_record[i]);
                    i++ ;
                }
            }
            va_end(vlist_p) ;

            while (dbnextrow(dbproc) != NO_MORE_ROWS)
            {
                /* allocate space and copy the results for this row */
                recordptr = new_db_record() ;

                va_start(vlist_p) ;
                /* copy specific columns */
                if ((col_id = va_arg(vlist_p, int)) != END_COLS)
                {
                    while (col_id != END_COLS)
                    {
                        recordptr[col_id] = (void *) NEW(columns[col_id].size) ;
                        memcpy(recordptr[col_id], tmp_record[col_id],
                            columns[col_id].size) ;
                        col_id = va_arg(vlist_p, int) ;
                    }
                }
                else /* copy all columns */
                {
                    i = 0 ;
                    while (columns[i].size)
                    {
                        recordptr[i] = (void *) NEW(columns[i].size) ;
                        memcpy(recordptr[i], tmp_record[i], columns[i].size) ;
                        i++ ;
                    }
                }
                va_end(vlist_p) ;

                /* add the record to the link list */
                APPEND(llrecords, recordptr, free_db_record, recordptr) ;
            }


            /*
            -- Now that were done free up the
            -- pointers in the temporary record
            */
            va_start(vlist_p) ;
            if ((col_id = va_arg(vlist_p, int)) != END_COLS)
            {
                while (col_id != END_COLS)
                {
                    FREE(tmp_record[col_id]) ;
                    col_id = va_arg(vlist_p, int) ;
                }
            }
            else /* free all fields */
            {
                i = 0 ; /* use the field size to as a counter; check all cols */
                while (columns[i].size)
                {
                    FREE(tmp_record[i]) ;
                    i++ ;
                }
            }
            va_end(vlist_p) ;
        }
    }

    FREE(stmt) ;
    FREE(colnames) ;
#ifndef REDUCED_PRINT
    printf("NUMBER OF RECORDS: %d\n\n", NUMELTS(llrecords)) ;
#endif
    return(llrecords) ;
}



/*==============================================================================
Function:       db_insert_values

Description:
    Adds one record to a table... the column names and values are
passed in char format

Parameters:
    DBPROCESS *dbproc
    char *table         - the table to add the values to
    char *column_name   - a list of comma separated names
    char *column_values - a list of comma separated values

Returns:        int
     1 record added
    -1 record not added

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
    SAMPLE: db_insert_values(dbproc,
        "table name", "colname1, colname2,...", "val1, val2,...") ;
==============================================================================*/
int db_insert_values(DBPROCESS *dbproc, char *table, char *column_names, char *column_values)
{
    int nrecs ;

    char *stmt = NULL ;
    static char *insert = "insert into" ;
    static char *values = "values" ;

    stmt = (char *) NEW(
        strlen(insert)
        + strlen(table)
        + strlen(values)
        + strlen(column_names)
        + strlen(column_values)
        + 9) ; /* extra for added blanks (3) and parentheses (4) */

    sprintf(stmt, "%s %s (%s) %s (%s)",
        insert, table, column_names, values, column_values) ;

#ifndef REDUCED_PRINT
    printf("INSERT STMT:\n %s\n", stmt) ;
#endif

    dbcmd(dbproc, stmt) ;
    dbsqlexec(dbproc) ;
    dbresults(dbproc) ;

    FREE(stmt) ;

    nrecs = DBCOUNT(dbproc) ;
    return(nrecs) ;
}



/*==============================================================================
Function:       db_print_record

Description:
    Print  the contents of a DB_RECORD

Parameters:
    DBPROCESS *dbproc
    DB_RECORD **record record to print
    COLUMN_DEFS    - column definitions for the table

Returns:        void

Creator:        Ron Green

Creation Date:  11/03/1994

Notes:
    SAMPLE: db_print_record(record, columntable) ;
==============================================================================*/
void db_print_record(DB_RECORD **record, COLUMN_DEFS *columns)
{
    int index = 0 ;

        while(columns[index].name)
        {
            printf("%s = ", columns[index].name) ;

            /* only write the data if not null */
            if (record[index] == NULL)
            {
                printf("***** NULL VALUE *****\n") ;
                index++ ;
                continue ;
            }

            switch(columns[index].bindtype)
            {
            case CHARBIND :
                printf("'%c'\n", *(DBCHAR *) record[index]) ;
                break ;

            case STRINGBIND :   /* string pointers don't need cast */
            case NTBSTRINGBIND :
                printf("'%s'\n", (char *) record[index]) ;
                break ;

            case TINYBIND :
                printf("%d\n", *(DBTINYINT *) record[index]) ;
                break ;

            case SMALLBIND :
                printf("%d\n", *(DBSMALLINT *) record[index]) ;
                break ;

            case INTBIND :
                printf("%ld\n", *(DBINT *) record[index]) ;
                break ;

            case REALBIND :
                printf("%f\n", *(DBREAL *) record[index]) ;
                break ;

            case FLT8BIND :
                printf("%.9f\n", *(DBFLT8 *) record[index]) ;
                break ;

            default : ;
                sprintf(aps_message,
                    "NO BINDTYPE; Using string format: '%s'\n",
                    (char *) record[index]) ;
                aps_internal_error(stdout, __FILE__, __LINE__, aps_message ) ;

            } /* switch */
            index++ ;
        }
    printf("END OF RECORD\n") ;
}


/*==============================================================================
Function:       db_fprint_record

Description:
    Print  the contents of a DB_RECORD to a file

Parameters:
    DBPROCESS *dbproc
    DB_RECORD **record record to print
    COLUMN_DEFS    - column definitions for the table

Returns:        void

Creator:        Miguel A. Siu

Creation Date:  Sun Dec 15 10:42:50 PST 1996

Notes:
    SAMPLE: db_fprint_record(stdout, record, columntable) ;
==============================================================================*/
void db_fprint_record(FILE *dest, DB_RECORD **record, COLUMN_DEFS *columns)
{
    int index = 0 ;

        while(columns[index].name)
        {
            fprintf(dest,"%s = ", columns[index].name) ;

            /* only write the data if not null */
            if (record[index] == NULL)
            {
                fprintf(dest,"***** NULL VALUE *****\n") ;
                index++ ;
                continue ;
            }

            switch(columns[index].bindtype)
            {
            case CHARBIND :
                fprintf(dest,"'%c'\n", *(DBCHAR *) record[index]) ;
                break ;

            case STRINGBIND :   /* string pointers don't need cast */
            case NTBSTRINGBIND :
                fprintf(dest,"'%s'\n", (char *) record[index]) ;
                break ;

            case TINYBIND :
                fprintf(dest,"%d\n", *(DBTINYINT *) record[index]) ;
                break ;

            case SMALLBIND :
                fprintf(dest,"%d\n", *(DBSMALLINT *) record[index]) ;
                break ;

            case INTBIND :
                fprintf(dest,"%ld\n", *(DBINT *) record[index]) ;
                break ;

            case REALBIND :
                fprintf(dest,"%f\n", *(DBREAL *) record[index]) ;
                break ;

            case FLT8BIND :
                fprintf(dest,"%.9f\n", *(DBFLT8 *) record[index]) ;
                break ;

            default : ;
                sprintf(aps_message,
                    "NO BINDTYPE; Using string format: '%s'\n",
                    (char *) record[index]) ;
                aps_internal_error(stdout, __FILE__, __LINE__, aps_message ) ;

            } /* switch */
            index++ ;
        }
    fprintf(dest,"END OF RECORD\n") ;
}


/*==============================================================================
Function:       db_print_list()

Description:    prints a list of DB_RECORDS

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Sat Nov 11 17:02:43 PST 1995

Notes:
    This routine was created using 4-character tabs.  If you don't have
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
void db_print_list(llist *list, COLUMN_DEFS *columns)
{
    DB_RECORD   **record = NULL ;
    cursor      list_ptr ;
    int         j = 1 ;

    for (
        record = (DB_RECORD **) FIRST(list, list_ptr) ;
        record ;
        record = (DB_RECORD **) NEXT(list, list_ptr)
        )
    {
        /*
        -- print a record number and line to
        -- help identify separate records.
        */
        printf("RECORD %d  ---------------------------------------\n", j++ ) ;
        db_print_record(record, columns ) ;
    }
}


/*==============================================================================
Function:       db_fprint_list()

Description:    prints a list of DB_RECORDS to a file

Returns:        void

Creator:        Miguel A. Siu

Creation Date:  Sun Dec 15 10:41:17 PST 1996

Notes:
    This routine was created using 4-character tabs.  If you don't have
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
void db_fprint_list(FILE *dest, llist *list, COLUMN_DEFS *columns)
{
    DB_RECORD   **record = NULL ;
    cursor      list_ptr ;
    int         j = 1 ;

    for (
        record = (DB_RECORD **) FIRST(list, list_ptr) ;
        record ;
        record = (DB_RECORD **) NEXT(list, list_ptr)
        )
    {
        /*
        -- print a record number and line to
        -- help identify separate records.
        */
        fprintf(dest,
            "RECORD %d  ---------------------------------------\n", j++ ) ;
        db_fprint_record(dest, record, columns ) ;
    }
}


/*==============================================================================
Function:       db_insert_records

Description:
    Insert records stored as a link list into a table

Parameters:
    DBPROCESS *dbproc
    llist *records - records to be added to the table
    char *table    - table to add the records to
    COLUMN_DEFS    - column definitions for the table

Returns:        int  number of records actually added

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
    SAMPLE: db_insert_records(dbproc, llist, "table name", columntable") ;
==============================================================================*/
int db_insert_records(DBPROCESS *dbproc, llist *records,
    char *table, COLUMN_DEFS *columns)
{
    DB_RECORD** record ;
    cursor ptr ;

    int added = 0 ;

    /*
    -- go through each record in the list.
    */
    for ( record = FIRST(records, ptr) ; record != NULL ;
            record = NEXT(records, ptr) )
    {
        /* insert the record into the database and increment if added.  */
        if ( db_insert_single_record(dbproc, record, table, columns ) )
            added++ ;
    }

#ifndef REDUCED_PRINT
    printf("RECORDS ADDED: %d\n\n", added) ;
#endif

    return(added) ;
}



/*==============================================================================
Function:       db_delete_records

Description:
    Deletes records from a table based on a where clause

Parameters:
    DBPROCESS *dbproc
    char *table     - table from which to delete records
    char *where     - where clause specifying which records to delete

Returns:        int
     n  - number of records deleted
    -1  - error deleting records

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
    SAMPLE: db_delete_records(dbproc, "table name", "where colname = values") ;
==============================================================================*/
int db_delete_records(DBPROCESS *dbproc, char *table, char *where_clause)
{
    int nrecs ;
    char *stmt = NULL ;
    static char *delete = "delete" ;

    if (!where_clause)
        where_clause = blank ;

    stmt = (char *) NEW(
        strlen(delete)
        + strlen(table)
        + strlen(where_clause)
        + 5) ; /* extra for added blanks */

    sprintf(stmt, "%s %s %s", delete, table, where_clause) ;

#ifndef REDUCED_PRINT
    printf("DELETE STMT:\n %s\n", stmt) ;
#endif
    dbcmd(dbproc, stmt) ;
    dbsqlexec(dbproc) ;
    dbresults(dbproc) ;

    FREE(stmt) ;

    nrecs = DBCOUNT(dbproc) ;
    return(nrecs) ;
}



/*==============================================================================
Function:       db_update_records

Description:
    Updates values of columns in existing records in a table

Parameters:
    DBPROCESS *dbproc
    char *table         table to update
    char *fields_to_set comma separated 'columnname = value' to update
    char *where_clause  clause specifying which records to update

Returns:        int
     n - Number of records updated
    -1 - error updating records

Creator:        Ron Green

Creation Date:  07/dd/1994

Notes:
    SAMPLE: db_update_records(dbproc,
        "table name", "col1 = 'text', col2 = 5", "where colname = 'update'") ;
==============================================================================*/
int db_update_records(DBPROCESS *dbproc, char *table, char *fields_to_set, char *where_clause)
{
    char *stmt = NULL ;

    int nrecs ;

    static char *update = "update" ;
    static char *set = "set" ;

    if (!where_clause)
        where_clause = blank ;

    stmt = (char *) NEW(
        strlen(update)
        + strlen(table)
        + strlen(set)
        + strlen(fields_to_set)
        + strlen(where_clause)
        + 7) ; /* extra for added blanks between arguments */

    sprintf(stmt, "%s %s %s\n%s\n%s ",
        update, table, set,  fields_to_set, where_clause) ;

#ifndef REDUCED_PRINT
    printf("UPDATE STMT:\n %s\n", stmt) ;
#endif

    dbcmd(dbproc, stmt) ;
    dbsqlexec(dbproc) ;
    dbresults(dbproc) ;

    FREE(stmt) ;

    nrecs = DBCOUNT(dbproc) ;
    return(nrecs) ;
}



/*==============================================================================
Function:       db_nth_record

Description:    Gets the nth record of a link list

Parameters:
    llist *dbase  the link list to retrieve the record from
    int n         the nth record to be retrieved

Returns:        DB_RECORD ** pointer to the link list record

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
==============================================================================*/
DB_RECORD **db_nth_record(llist *records, int n)
{
    int i ;
    cursor ptr ;
    DB_RECORD **record ;

    record = (DB_RECORD **) FIRST(records, ptr) ;
    for (i = 0 ; i < n - 1 ; i++)
        record = (DB_RECORD **) NEXT(records, ptr) ;
    return(record) ;
}




/*==============================================================================
Function:       db_read_records_from_ingres_file

Description:
    Reads binary records from ingres type files

Parameters:

Returns:        llist* link list of records

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
    This function assumes that the records read are the same order in both
the ingres and sybase tables.  It also assumes that the field describing the
length of the data is 5 bytes (should be parameterized).
==============================================================================*/
#define SIZE_INGRES_FIELD_LEN 5

static int read_col_from_file(int fd, char *buf)
{
    int len, nbytes ;

    /* read the next five bytes for the field length */
    nbytes = read(fd, buf, SIZE_INGRES_FIELD_LEN) ;

    if (nbytes < SIZE_INGRES_FIELD_LEN)
        return(-1) ;

    /* convert length of field, just read.  */
    buf[SIZE_INGRES_FIELD_LEN] = '\0' ;
    len = atoi(buf) ;

    if (len > 0)
    {
        /* read the next len bytes into buf */
        if ((nbytes = read(fd, buf, (unsigned) len)) < len)
            return(-1) ;
        buf[nbytes] = NULL ;
    }
    else if (len == 0)
        return(0) ;
    else    /* EOF */
        return(-1) ;

    return(len) ;
}


llist* db_read_records_from_ingres_file(
    COLUMN_DEFS *columns, char *file, int *status)
{
    llist *file_records ;
    DB_RECORD **recordptr ;

    int fd ;
    int index ;

    char buf[2048] ;

    static char char_format[]   = "%c" ;
    static char int_format[]    = "%d" ;
    static char real_format[]   = "%f" ;

    if ((fd = open(file, O_RDONLY)) == -1)
    {
        *status = DB_FILE_ERROR ;
        return(NULL) ;
    }

    /*
    -- use the information in the DBCOL table to
    -- read the data in the db record
    */
    file_records = create_dyn_llist() ;
    recordptr = new_db_record() ;

    index = 0 ;
    while (read_col_from_file(fd, buf) != -1)
    {
        recordptr[index] = (void *) NEW(columns[index].size) ;

        switch (columns[index].bindtype)
        {
        case CHARBIND :
            sscanf(buf, char_format, recordptr[index]) ;
            break ;

        case STRINGBIND :
        case NTBSTRINGBIND :
            strcpy((char *) recordptr[index], buf) ;
            break ;

        case INTBIND :
            sscanf(buf, int_format, recordptr[index]) ;
            break ;

        case REALBIND :
        case FLT8BIND :
            sscanf(buf, real_format, recordptr[index]) ;
            break ;

        default :
            printf("SCAN FORMAT for item %s not given...\n",
                columns[index].name) ;
            exit(-1) ;
        }

        index++ ;
        if (!columns[index].name)
        {
            /* add the current record to the link list */
            APPEND(file_records, recordptr, free_db_record, recordptr) ;

            /* get space for a new record */
            recordptr = new_db_record() ;

            /* reset the index for COLUMN defs */
            index = 0 ;
        }
    }
    FREE(recordptr) ;  /* free the last record initialized */
    close(fd) ;

    *status = DB_FILE_OK ;
    return(file_records) ;
}




/*==============================================================================
Function:       db_ftn_first_record
Function:       db_ftn_next_record

Description:
    FORTRAN support functions that mimic the FIRST and NEXT link list
macros.  These functions can be used by FORTRAN subroutines to traverse
records retrieved with db_get_records

Parameters:

Returns:

Creator:        Ron Green

Creation Date:  07/dd/1994

Notes:
==============================================================================*/
void *db_ftn_first_record(llist * list, cursor ptr)
{
    DB_RECORD **data_record ;

    data_record = FIRST(list, *ptr) ;

    return(data_record) ;
}


void *db_ftn_next_record(llist * list, cursor ptr)
{
    DB_RECORD **data_record ;

    data_record = NEXT(list, *ptr) ;

    return(data_record) ;
}

void db_ftn_free_llist(llist * list)
{
    DEL_LIST(list) ;
}



/*==============================================================================
Function:   db_copy_record

Description:    copies a the field values from one DB_RECORD into another
DB_RECORD.  The source DB_RECORD is unchanged.

Parameters:
    COLUMN_DEFS *columns
    DB_RECORD   **destination_rec
    DB_RECORD   **source_rec

Returns:
    int
    = 1    success
            DB_COPY_RECORD_OK

    < 0    Error:
            DB_RECORD_NULL

Creator:        Lawrence Stevens

Creation Date:  03/08/1995

Notes:
==============================================================================*/
int db_copy_record(
    COLUMN_DEFS *columns,
    DB_RECORD   **destination_rec,
    DB_RECORD   **source_rec )
{

    int j = 0 ;

    if ( destination_rec == NULL )
        return DB_RECORD_NULL ;

    if ( source_rec == NULL )
        return DB_RECORD_NULL ;

    /*
    -- copy (memory to memory) each column, according to
    -- the column size in bytes.
    */
    while (columns[j].size)
    {
        /*
        -- note:  memmove is the same as memcpy except that it works
        --        even if the objects overlap.  the results of
        --        memcpy would be undefined in this case.  we take the
        --        safer, more robust choice.
        --
        --        although the word "move" is in the name, it really is
        --        a copy action.
        --        memmove does not affect the source memory
        --        except in the above mentioned special case.
        */
        memmove( destination_rec[j], source_rec[j],
            (size_t) columns[j].size ) ;
        j++ ;
    }

    return DB_COPY_RECORD_OK ;

}

/*==============================================================================
Function:   db_duplicate_record

Description:    duplicates one DB_RECORD into another DB_RECORD.  
                The source DB_RECORD is unchanged.  Memory is 
                allocated for the new DB_RECORD.  

Parameters:
    COLUMN_DEFS *columns
    DB_RECORD   **destination_rec
    DB_RECORD   **source_rec

Returns:
    int
    = 1    success
            DB_COPY_RECORD_OK

    < 0    Error:
            DB_RECORD_NULL

Creator:        Lawrence Stevens

Creation Date:  04/04/1997

Notes:
==============================================================================*/
DB_RECORD **db_duplicate_record( COLUMN_DEFS *columns, DB_RECORD **source_rec )
{

    int         return_code ;
    DB_RECORD   **destination_rec = NULL ;

    if ( source_rec == NULL )
        return NULL ;

    /* 
    -- allocate memory for the new DB_RECORD  
    -- and all of its fields:
    */
    destination_rec = new_table_record(columns) ;
    if( destination_rec == NULL )
        return NULL ;

    /* copy the input record to the destination.  */
    return_code = db_copy_record( columns, destination_rec, source_rec ) ;
    if( return_code != TRUE )
    {
        free_db_record( destination_rec ) ;
        return NULL ;
    }

    return destination_rec ;

}


/*==============================================================================
Function:       db_quote_doubler

Description:
    Copies a character string, doubling any single quote marks (').

Parameters:
    char *source            - source string.
    char **destination      - destination string pointer returned to
                                the calling program, WHICH MUST free() IT.

Returns:     int    length of the destination string.
                    -1 if the malloc() falied.

Creator:    Lawrence Stevens

Creation Date:  Thu Oct  5 11:00:03 PDT 1995

Notes:
    SAMPLE: db_quote_doubler("string ' with a quote", &destination_str ) ;

    Calling program MUST FREE any non-NULL returned destination_str

==============================================================================*/
int
db_quote_doubler(
    char *source,
    char **returned_destination_str )
{

    int     source_offset ;     /* counter/offset for source bytes          */
    int     output_str_offset ; /* counter/offset for output_str bytes      */
    int     output_str_maxsize ;/* max size for output_str bytes    */
    char    *output_str ;       /* local output string.  used for readability.*/

    /*
    -- compute the largest possible output_str string
    -- assuming every character is a single quote.
    */
    output_str_maxsize = 1 + 2 * (int) strlen(source) ;
    output_str = malloc(output_str_maxsize) ;

    /* assign value to return variable to calling program.  */
    *returned_destination_str = output_str ;
    if ( output_str == NULL )
        return -1 ;

    /*
    -- strategy:  copy each byte into output_str, but if
    -- a byte was a single quote, copy it again.
    -- and be sure to add in the null byte to end the string.
    */

    /*
    -- for each character in source until a NULL:
    */
    /*
    -- source_offset is an offset onto source;
    -- output_str_offset is an offset onto output_str.
    */
    output_str_offset = 0 ;
    for ( source_offset = 0 ;
          *(source+source_offset) != NULL ;
          source_offset++ )
    {
        /*
        -- copy the character, incrementing
        -- output_str offset
        */
        *(output_str+output_str_offset++) = *(source+source_offset) ;

        /* if this is a quote, copy it again, just like before. */
        if ( *(source+source_offset) == '\'' )
        {
            *(output_str+output_str_offset++) = *(source+source_offset) ;
        }
    }

    /*
    -- terminate the string; we have already allowed space
    -- for this null char; don't increment the offset/counter
    -- output_str_offset; it is used for the length of the string.
    */
    *(output_str+output_str_offset) = '\0' ;

    /* return the number of bytes in the string, not counting the NULL.  */
    return( output_str_offset ) ;

}

/*==============================================================================
Function:       db_insert_single_record

Description:
    Insert a DB_RECORD record into a table

Parameters:
    DBPROCESS *dbproc
    DB_RECORD **record - record to be added to the table
    char *table    - table to add the record to
    COLUMN_DEFS    - column definitions for the table

Returns:        int  number of records actually added.  should be 1.

Creator:        Lawrence Stevens

Creation Date:  Mon Oct  2 12:00:29 PDT 1995

Notes:
    SAMPLE: db_insert_single_record(dbproc, db_record, "table name", columntable ) ;

==============================================================================*/
int db_insert_single_record(DBPROCESS *dbproc,
    DB_RECORD **record, char *table, COLUMN_DEFS *columns )
{

    int return_code;
    int index ;
    int added = 0 ;

    char *quote_buf = NULL ;
    char *stmt = NULL ;
    char *cptr ;
    char valuestr[4096] ;

    static char *insert = "insert into" ;
    static char *values = "values" ;

    /* form the colnames for which to write the values with */
    db_make_colnames_string( columns, col_names) ;

    index = 0 ;
    sprintf(valuestr, blank) ;
    while(columns[index].name)
    {
        /* only write the data if not null */
        if (record[index] == NULL)
        {
            index++ ;
            continue ;
        }

        switch(columns[index].bindtype)
        {
            case CHARBIND :

                /*
                -- check for values of ' and \0 for special cases:
                */
                switch(  *(DBCHAR *) record[index]  )
                {

                    case '\'' :
                        sprintf(valuestr, "%s'''', ", valuestr ) ;
                        break ;

                    case '\0' :
                        sprintf(valuestr, "%s'', ", valuestr ) ;
                        break ;

                    default   :
                        sprintf(valuestr, "%s'%c', ", valuestr,
                            *(DBCHAR *) record[index]) ;
                        break ;

                }

                break ;

            case STRINGBIND :
            case NTBSTRINGBIND :
                /*
                -- check for the existence of at least one '
                -- single quote character:
                */
                if( strchr( (char *) record[index], '\'') )
                {
                    /* there is a quote.  */
                    /*
                    -- double each quote, to accommodate the syntax we
                    -- use, which delimits character fields by single quotes.(')
                    -- the negative return code indicates an extremely unlikely
                    -- falure to malloc on the output buffer.
                    -- REMEMBER TO FREE THE quote_buf MEMORY.
                    */
                    return_code = db_quote_doubler( (char *) record[index],
                        &quote_buf ) ;
                    if ( return_code < 0 )
                    {
                        sprintf(aps_message,
"Could not malloc() when doubling quotes.  :  Problem field:  %s =  %s\nLEAVING OUT THE VALUE\n",
                        columns[index].name, (char *) record[index] ) ;
                        aps_internal_error(stdout, __FILE__, __LINE__,
                            aps_message ) ;
                    }
                    else
                    {
                        sprintf(valuestr, "%s'%s', ", valuestr, quote_buf ) ;
                        free( quote_buf ) ;
                    }
                }
                else
                {
                    /* there is no quote.  */
                    sprintf(valuestr, "%s'%s', ", valuestr,
                        (char *) record[index]) ;
                }
                break ;

            case TINYBIND :
                sprintf(valuestr, "%s%d, ", valuestr,
                    *(DBTINYINT *) record[index]) ;
                break ;

            case SMALLBIND :
                sprintf(valuestr, "%s%d, ", valuestr,
                    *(DBSMALLINT *) record[index]) ;
                break ;

            case INTBIND :
                sprintf(valuestr, "%s%ld, ", valuestr,
                    *(DBINT *) record[index]) ;
                break ;

            case REALBIND :
                /*
                -- single precision.  make sure that enough precision
                -- for our application goes in to the database.
                -- the main driver would be the latitude/lons.
                -- give extra precision just in case; it can't hurt.
                */
                sprintf(valuestr, "%s%.10f, ", valuestr,
                    *(DBREAL *) record[index]) ;
                break ;

            case FLT8BIND :
                /*
                -- double precision.  make sure that enough precision
                -- for our application goes in to the database.
                -- the main driver is the ephemeris time field.
                -- give extra precision just in case; it can't hurt.
                */
                sprintf(valuestr, "%s%.10f, ", valuestr,
                    *(DBFLT8 *) record[index]) ;
                break ;

            default : ;
                sprintf(aps_message,
                    "BINDTYPE for item %s not given...\nUsing string format",
                    columns[index].name) ;
                aps_internal_error(stdout, __FILE__, __LINE__, aps_message ) ;
                sprintf(valuestr, "%s'%s', ",
                    valuestr, (char *) record[index]) ;

        } /* end of switch */
        index++ ;
    }  /* end of while.  */

    cptr = strrchr(valuestr, ',') ;
    *cptr = NULL ; /* place a NULL at the last comma */

    stmt = (char *) NEW(
            strlen(insert)
            + strlen(table)
            + strlen(values)
            + strlen(col_names)
            + strlen(valuestr)
            + 9) ; /* extra for added blanks (3) and parentheses (4) */

    sprintf(stmt, "%s %s (%s) %s (%s)",
            insert, table, col_names, values, valuestr) ;

#ifndef REDUCED_PRINT
    printf("INSERT STMT:\n %s\n", stmt) ;
#endif

    return_code = dbcmd(dbproc, stmt) ;
    if( return_code != SUCCEED ) 
    {
        FREE(stmt) ;
        return -1 ;
    }
    return_code = dbsqlexec(dbproc) ;
    if( return_code != SUCCEED ) 
    {
        FREE(stmt) ;
        return -1 ;
    }
    return_code = dbresults(dbproc) ;
    if( return_code != SUCCEED ) 
    {
        FREE(stmt) ;
        return -1 ;
    }

    FREE(stmt) ;
    if (DBCOUNT(dbproc) > 0)
            added++ ;

#ifndef REDUCED_PRINT
    if ( added == 1 )
        printf("RECORD WAS ADDED\n\n" ) ;
    else
        printf("RECORD WAS NOT ADDED\n\n" ) ;
#endif

    return(added) ;
}


/*==============================================================================
Function:       db_record_llist_move()

Description:    move every element from one DB_RECORD llist into another
                DB_RECORD llist.  The source llist will then be empty.

Creator:        Lawrence Stevens

Creation Date:  Sat Oct 28 19:39:43 PDT 1995

Notes:
    This routine was created using 4-character tabs.  If you don't have
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
llist *db_record_llist_move( llist *destination, llist *source )
{
    DB_RECORD   **db_record ;
    DB_RECORD   **db_unlinked_record ;
    cursor      source_ptr ;

    if ( source == NULL)
        return NULL ;

    if ( destination == NULL)
        return NULL ;

    for (
        db_record = (DB_RECORD **) FIRST( source, source_ptr ) ;
        db_record ;
        db_record = (DB_RECORD **) FIRST( source, source_ptr )
        )
    {
        /* remove (UNLINK) the db_record from the source list:  */
        db_unlinked_record = UNLINK_AT_CURSOR( source, source_ptr ) ;
        if( db_unlinked_record != db_record )
            return NULL ;

        /* APPEND to the destination list.  */
        APPEND( destination, db_record, free_db_record, db_record ) ;
    }

    return destination ;

}


/*==============================================================================
Function:       db_record_llist_copy()

Description:    copy every element from one DB_RECORD llist into another
                DB_RECORD llist.

Creator:        Miguel Siu

Creation Date:  Thu Mar 20 18:53:28 PST 1997

Returns:
        DB_RECORD_NULL      an error was encountered.
        otherwise,          address of destination list.    

Notes:
    This routine will allocate the memory in the destination list, in
    which the copied data will be deposited.

    This routine was created using 4-character tabs.  If you don't have
    4-character tabs, this routine will look funny.  use set tabstop=4 in vi.
==============================================================================*/
llist *db_record_llist_copy(
    COLUMN_DEFS *columns, 
    llist *destination, 
    llist *source )
{
    DB_RECORD   **db_record ;
    DB_RECORD   **db_record_copy ;
    cursor      source_ptr ;

    if ( source == NULL)
        return NULL ;

    if ( destination == NULL)
        return NULL ;

    for (
        db_record = (DB_RECORD **) FIRST( source, source_ptr ) ;
        db_record ;
        db_record = (DB_RECORD **) NEXT( source, source_ptr )
        )
    {
        /* 
        -- allocate memory for the new record in which the copy will reside
        */
        db_record_copy =  new_table_record(columns) ;

        if (db_copy_record( columns, db_record_copy, db_record) 
                != DB_COPY_RECORD_OK)
        return NULL ;

        /* APPEND to the destination list.  */
        APPEND( destination, db_record_copy, free_db_record, db_record_copy ) ;
    }

    return destination ;

}


/*==============================================================================
Function:       move_db_record2new_llist

Description:    move a record out of one llist and into another.

Parameters:
    llist      *destination_list,    append the DB_RECORD to this list
    DB_RECORD  **record,             DB_RECORD to move
    llist      *source_list,         remove record from this list
    cursor     *source_list_ptr,     pointer to the record to remove

Returns:
    >= 0  :  no errors
        pointer to destination_list

    < 0   :  error
        NULL

Creator:        Lawrence Stevens

Creation Date:  Mon Nov  6 12:19:47 PST 1995

Notes:
    ALL four parameters are input parameters; the calling routine
    supplies values for them.

==============================================================================*/

llist *move_db_record2new_llist(    /* ALL parameters are input parameters */
    llist       *destination_list,  /* append the record to this list      */
    DB_RECORD   **db_record,        /* record to move                      */
    llist       *source_list,       /* remove record from this list        */
    cursor      source_list_ptr )   /* pointer to the record to remove     */
{

    DB_RECORD **db_unlinked_record ;

    /*
    -- quick error checking:
    -- the source_list_ptr MUST point to
    -- the input db_record.
    */
    db_unlinked_record = (DB_RECORD **) GET( source_list, source_list_ptr ) ;
    if ( db_unlinked_record != db_record )
        return NULL ;

    db_unlinked_record = (DB_RECORD **) UNLINK_AT_CURSOR( source_list,
        source_list_ptr ) ;

    if( db_unlinked_record != db_record )
        return NULL ;

    APPEND( destination_list, db_record, free_db_record, db_record ) ;

    return destination_list ;

}


/*==============================================================================
Function:       move_db_record_matches2llist

Description:    move a record out of one llist and into another,
                based on a field equalling a value.

Returns:
    >= 0  :  no errors
        pointer to destination_list

    < 0   :  error
        NULL

Creator:        Lawrence Stevens

Creation Date:  Tue Jan  2 15:51:39 PST 1996

Notes:
    ALL four parameters are input parameters; the calling routine
    supplies values for them.
Example:

    list_check = move_db_record_matches2llist( APS_CDEFS(DTK), DTKSTAT,
        "SCH", source_list, destination_list ) ;

==============================================================================*/

llist *move_db_record_matches2llist(
    COLUMN_DEFS *columns,           /* the colum defs for the DB_RECORD.   */
    int         column_index,       /* column number to compare.           */
    void        *value_pointer,     /* pointer to the value to compare     */
    llist       *destination_list,  /* append the record to this list      */
    llist       *source_list )      /* remove record from this list        */
{

    DB_RECORD   **db_source_record ;
    DB_RECORD   **next_db_source_record ;
    llist       *llist_check = NULL ;
    int         equality_flag = 0 ;
    cursor      source_list_ptr ;
    cursor      next_source_list_ptr ;

    /*
    -- quick error checking:
    */
    if ( source_list == NULL )
        return NULL ;

    if ( destination_list == NULL )
        return NULL ;

    if ( NUMELTS( source_list )  == 0 )
        return destination_list ;

    db_source_record = (DB_RECORD **) FIRST(source_list, source_list_ptr ) ;
    while ( db_source_record )
    {
        /*
        -- set up the NEXT record to process; the
        -- current one might get removed from source_list
        */
        next_source_list_ptr = source_list_ptr ;
        next_db_source_record = (DB_RECORD **) NEXT(source_list,
            next_source_list_ptr ) ;

        /*
        -- if this rec has a value that matches, move it to the
        -- destination list.
        */
        equality_flag = 0 ;
        switch( columns[column_index].bindtype )
        {
        case STRINGBIND :
        case NTBSTRINGBIND :
            if ( strcmp( (char *) db_source_record[column_index],
                value_pointer ) == 0 )
                equality_flag = 1 ;
            break ;
        default :
            if ( memcmp( db_source_record[column_index], value_pointer,
                 columns[column_index].size ) == 0 )
                equality_flag = 1 ;
            break ;
        }

        if ( equality_flag )
        {
            /* the value does match.  */
            llist_check = move_db_record2new_llist ( destination_list,
                db_source_record, source_list, source_list_ptr ) ;
            if ( llist_check != destination_list )
                return NULL ;
        }

        /* set up for processing the next record.  */
        db_source_record = next_db_source_record ;
        source_list_ptr = next_source_list_ptr ;
    }

    return destination_list ;

}

/*==============================================================================
Function:       set_db_record_values

Description:    set a field value into each DB_RECORD in a llist.

Returns:
    >= 0  :  no errors
        pointer to destination_list

    < 0   :  error
        NULL

Creator:        Lawrence Stevens

Creation Date:  Wed Jan  3 12:43:37 PST 1996

Notes:
    ALL four parameters are input parameters; the calling routine
    supplies values for them.
Example:

    list_check = set_db_record_values( APS_CDEFS(DTK), DTKSTAT,
        "SCH", source_list ) ;

==============================================================================*/

llist *set_db_record_values(
    COLUMN_DEFS *columns,           /* the colum defs for the DB_RECORD.   */
    int         column_index,       /* column number to set.               */
    void        *value_pointer,     /* pointer to the value to set         */
    llist       *source_list )      /* update records in this list         */
{

    DB_RECORD   **db_source_record ;
    cursor      source_list_ptr ;

    /*
    -- quick error checking:
    */
    if ( source_list == NULL )
        return NULL ;

    if ( NUMELTS( source_list )  == 0 )
        return source_list ;

    if ( columns[column_index].size <= 0 )
        return NULL ;

    for (
        db_source_record = (DB_RECORD **) FIRST(source_list, source_list_ptr ) ;
        db_source_record ;
        db_source_record = (DB_RECORD **) NEXT(source_list, source_list_ptr )
        )
    {
        memcpy( db_source_record[column_index], value_pointer,
                 columns[column_index].size ) ;
    }

    return source_list ;

}

/*==============================================================================
Function:       db_get_min_record()

Description:    get the min value for the DB_RECORD field indicated
                from the given input list.
                Populate a pointer to a copy of the DB_RECORD containing
                the min value.

Creator:        Miguel Siu

Creation Date:  Sun Dec 31 10:16:45 PST 1995

Notes:      Function returns    DB_MIN_RECORD_OK for success.
                                DB_MIN_RECORD_FAIL for error.

==============================================================================*/
int db_get_min_record(
    llist       *db_rec_list,
    COLUMN_DEFS *columns,
    int         min_search_index,
    DB_RECORD   ***record_containing_min)
{
    DB_RECORD       **db_rec = NULL ;
    DB_RECORD       **min_rec = NULL ;
    cursor          list_ptr ;

    /* quick error checking.  */
    if ( db_rec_list == NULL )
        return DB_MIN_RECORD_FAIL ;

    if ( NUMELTS( db_rec_list ) <= 0 )
        return DB_MIN_RECORD_FAIL ;

    /*
    -- Set up the comparison structures.  We will initialize min_rec to
    -- contain the information in the FIRST element in db_rec_list
    */
    min_rec =  new_table_record(APS_CDEFS(DTK)) ;
    db_rec = (DB_RECORD **) FIRST (db_rec_list, list_ptr);
    if (db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
        return DB_MIN_RECORD_FAIL ;


    for
    ( db_rec = (DB_RECORD **) NEXT (db_rec_list, list_ptr);
      db_rec ;
      db_rec = (DB_RECORD **) NEXT (db_rec_list, list_ptr)
    )
    {
        switch(columns[min_search_index].bindtype)
        {
        case CHARBIND :
            if( *(DBCHAR *) db_rec[min_search_index]
               <*(DBCHAR *) min_rec[min_search_index]
            && db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        case STRINGBIND :
        case NTBSTRINGBIND :
            if (strcmp( (char *)min_rec[min_search_index],
                        (char *)db_rec[min_search_index] ) > 0
            &&  db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        case TINYBIND :
            if( *(DBTINYINT *) db_rec[min_search_index]
               <*(DBTINYINT *) min_rec[min_search_index]
            && db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        case SMALLBIND :
            if( *(DBSMALLINT *) db_rec[min_search_index]
               <*(DBSMALLINT *) min_rec[min_search_index]
            && db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        case INTBIND :
            if( *(DBINT *) db_rec[min_search_index]
               <*(DBINT *) min_rec[min_search_index]
            && db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        case REALBIND :
            if( *(DBREAL *) db_rec[min_search_index]
               <*(DBREAL *) min_rec[min_search_index]
            && db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        case FLT8BIND :
            if( *(DBFLT8 *) db_rec[min_search_index]
               <*(DBFLT8 *) min_rec[min_search_index]
            && db_copy_record(columns, min_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MIN_RECORD_FAIL);

            break ;

        default :
            sprintf(aps_message,
                "BINDTYPE for item %s not given...\n Stop search for MIN value",
                columns[min_search_index].name) ;
                aps_internal_error(stdout, __FILE__, __LINE__, aps_message ) ;
                return (DB_MIN_RECORD_FAIL);

        } /* switch */

    } /* for() loop */

    *record_containing_min = min_rec ;
    return DB_MIN_RECORD_OK ;
}


/*==============================================================================
Function:       db_get_max_record()

Description:    get the max value for the DB_RECORD field indicated
                from the given input list.
                Populate a pointer to a copy of the DB_RECORD containing
                the max value.

Creator:        Miguel Siu

Creation Date:  Sun Dec 31 10:16:45 PST 1995

Notes:      Function returns    DB_MAX_RECORD_OK for success.
                                DB_MAX_RECORD_FAIL for error.

==============================================================================*/
int db_get_max_record(
    llist       *db_rec_list,
    COLUMN_DEFS *columns,
    int         max_search_index,
    DB_RECORD   ***record_containing_max)
{
    DB_RECORD       **db_rec = NULL ;
    DB_RECORD       **max_rec = NULL ;
    cursor          list_ptr ;

    /* quick error checking.  */
    if ( db_rec_list == NULL )
        return DB_MAX_RECORD_FAIL ;

    if ( NUMELTS( db_rec_list ) <= 0 )
        return DB_MAX_RECORD_FAIL ;

    /*
    -- Set up the comparison structures.  We will initialize max_rec to
    -- contain the information in the FIRST element in db_rec_list
    */
    max_rec =  new_table_record(APS_CDEFS(DTK)) ;
    db_rec = (DB_RECORD **) FIRST (db_rec_list, list_ptr);
    if (db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
        return DB_MAX_RECORD_FAIL ;


    for
    ( db_rec = (DB_RECORD **) NEXT (db_rec_list, list_ptr);
      db_rec ;
      db_rec = (DB_RECORD **) NEXT (db_rec_list, list_ptr)
    )
    {
        switch(columns[max_search_index].bindtype)
        {
        case CHARBIND :
            if( *(DBCHAR *) db_rec[max_search_index]
               >*(DBCHAR *) max_rec[max_search_index]
            && db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        case STRINGBIND :
        case NTBSTRINGBIND :
            if (strcmp( (char *)max_rec[max_search_index],
                        (char *)db_rec[max_search_index]) < 0
            &&  db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        case TINYBIND :
            if( *(DBTINYINT *) db_rec[max_search_index]
               >*(DBTINYINT *) max_rec[max_search_index]
            && db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        case SMALLBIND :
            if( *(DBSMALLINT *) db_rec[max_search_index]
               >*(DBSMALLINT *) max_rec[max_search_index]
            && db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        case INTBIND :
            if( *(DBINT *) db_rec[max_search_index]
               >*(DBINT *) max_rec[max_search_index]
            && db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        case REALBIND :
            if( *(DBREAL *) db_rec[max_search_index]
               >*(DBREAL *) max_rec[max_search_index]
            && db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        case FLT8BIND :
            if( *(DBFLT8 *) db_rec[max_search_index]
               >*(DBFLT8 *) max_rec[max_search_index]
            && db_copy_record(columns, max_rec, db_rec) != DB_COPY_RECORD_OK)
                return (DB_MAX_RECORD_FAIL);

            break ;

        default :
            sprintf(aps_message,
                "BINDTYPE for item %s not given...\n Stop search for MAX value",
                columns[max_search_index].name) ;
                aps_internal_error(stdout, __FILE__, __LINE__, aps_message ) ;
                return (DB_MAX_RECORD_FAIL);

        } /* switch */

    } /* for() loop */

    *record_containing_max = max_rec ;
    return DB_MAX_RECORD_OK ;
}


/*==============================================================================
Function:       copy_db_record_matches2llist

Description:    copy a record from one llist into another,
                based on a field equalling a value.
                allocate storage for the new DB_RECORD.  

Returns:
    >= 0  :  no errors
        pointer to destination_list

    < 0   :  error
        NULL

Creator:        Lawrence Stevens

Creation Date:  Tue Jan  2 15:51:39 PST 1996

Notes:
    ALL four parameters are input parameters; the calling routine
    supplies values for them.
Example:

    list_check = copy_db_record_matches2llist( APS_CDEFS(DTK), DTKSTAT,
        "SCH", source_list, destination_list ) ;

==============================================================================*/

llist *copy_db_record_matches2llist(
    COLUMN_DEFS *columns,           /* the colum defs for the DB_RECORD.   */
    int         column_index,       /* column number to compare.           */
    void        *value_pointer,     /* pointer to the value to compare     */
    llist       *destination_list,  /* append the record to this list      */
    llist       *source_list )      /* read the records in this list       */
{

    DB_RECORD   **duplicate_source_record ;
    DB_RECORD   **db_source_record ;
    int         equality_flag = 0 ;
    cursor      source_list_ptr ;

    /*
    -- quick error checking:
    */
    if ( source_list == NULL )
        return NULL ;

    if ( destination_list == NULL )
        return NULL ;

    if ( NUMELTS( source_list )  == 0 )
        return destination_list ;

    for( 
        db_source_record = (DB_RECORD **) FIRST(source_list, source_list_ptr ) ;
        db_source_record ;
        db_source_record = (DB_RECORD **) NEXT(source_list, source_list_ptr ) 
        )
    {
        /*
        -- if this rec has a value that matches, COPY it to the
        -- destination list.
        */
        equality_flag = 0 ;
        switch( columns[column_index].bindtype )
        {
        case STRINGBIND :
        case NTBSTRINGBIND :
            if ( strcmp( (char *) db_source_record[column_index],
                value_pointer ) == 0 )
                equality_flag = 1 ;
            break ;
        default :
            if ( memcmp( db_source_record[column_index], value_pointer,
                 columns[column_index].size ) == 0 )
                equality_flag = 1 ;
            break ;
        }

        if ( equality_flag )
        {
            /* 
            -- the value does match.  
            -- copy the record by duplicating it (allocating storage) 
            -- then appending to the destination list.  
            */
            duplicate_source_record = db_duplicate_record( columns, 
                db_source_record ) ;
            if( duplicate_source_record == NULL )
                return NULL ;

            APPEND( destination_list, duplicate_source_record, free_db_record, 
                duplicate_source_record ) ;
        }
    }

    return destination_list ;

}
