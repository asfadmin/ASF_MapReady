#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:   db_sybint.h

Description:
    Function Declarations for accessing data contained in Sybase tables
    The function definitions make use of the Open Client-Library functions
    to access the data

Creator:    Ron Green
Notes:
==============================================================================*/
#pragma ident   "@(#)db_sybint.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/global/SCCS/s.db_sybint.h"

#ifndef _DB_SYBINT_H_
#define _DB_SYBINT_H_

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>

#include <dapps_list.h>

/* globals referenced for db use */
extern char where_clause[] ;
extern char fields_to_set[] ;
extern char orderby_cols[] ;
extern char format_string[] ;
extern char tables[] ;


/* define ERROR CODES returned from db_open */

#define DB_OPEN_OK          0
#define DB_OPEN_ERR_DBNAME  1
#define DB_OPEN_ERR_PROG    2
#define DB_OPEN_ERR_USER    3
#define DB_OPEN_ERR_PASSWD  4
#define DB_OPEN_ERR_DBINIT  5
#define DB_OPEN_ERR_DBLOGIN 6
#define DB_OPEN_ERR_DBOPEN  7
#define DB_OPEN_ERR_DBUSE   8

/* define return codes from db_copy_record */
#define DB_RECORD_NULL              -1
#define DB_COPY_RECORD_OK            1

/* define return codes for db_get_min_record, db_get_max_record */
#define DB_MIN_RECORD_FAIL          -1
#define DB_MIN_RECORD_OK             1
#define DB_MAX_RECORD_FAIL          -1
#define DB_MAX_RECORD_OK             1

#define MAXITEMS 60
#define END_COLS -1
#define ALL_COLS -1

#define DB_FILE_ERROR 0
#define DB_FILE_OK    1

/* defines for sybsys colums */
#define SYBCOL_NAME 0
#define SYBCOL_TYPE 1

#define CAST_SYBCOL_NAME /* strings don't need cast types */
#define CAST_SYBCOL_TYPE *( DBINT *)

/*
-- this is a macro to use in db_get_records() when you
-- want to use the aps_reader account instead of an existing
-- Sybase session.  instead of
-- this:     dtk_llist = db_get_records( APS_dbproc, ... ) ;
-- do this:  dtk_llist = db_get_records( DB_SYBINT_USE_APS_READER_DBPROC, ... );
--
-- The main benifit is for software that does not write into the
-- database, but only reads data from the database.  In this case
-- the software does not need to open a session; db_get_records()
-- will do this, once only, for the process.
*/
#define DB_SYBINT_USE_APS_READER_DBPROC (DBPROCESS *) 1

/*
-- the following are type values that are
-- store with each column in the tables
-- these values are useful when determining
-- the type of value a column is...
--
-- these values are gotten by the sql statement:
--   select type, name from systypes
--
-- to get the type of data for a particular column use
--    select c.name, c.type from syscolumns c, sysobjects o where
--    o.name = 'table name' and c.name = 'colname' and c.id = o.id
*/

#define  BINARY                           45
#define  BIT                              50
#define  CHAR                             47
#define  DATETIME                         61
#define  DATETIMN                        111
#define  DECIMAL                          55
#define  DECIMALN                        106
#define  FLOAT                            62
#define  FLOATN                          109
#define  IMAGE                            34
#define  INT                              56
#define  INTN                             38
#define  MONEY                            60
#define  MONEYN                          110
#define  NCHAR                            47
#define  NUMERIC                          63
#define  NUMERICN                        108
#define  NVARCHAR                         39
#define  REAL                             59
#define  SMALLDATETIME                    58
#define  SMALLINT                         52
#define  SMALLMONEY                      122
#ifdef   SYSNAME
#undef   SYSNAME
#endif
#define  SYSNAME                          39
#define  TEXT                             35
#define  TIMESTAMP                        37
#define  TINYINT                          48
#define  VARBINARY                        37
#define  VARCHAR                          39

typedef
    char *DB_RECORD[MAXITEMS] ;

typedef
    struct _DBCOLS
    {
        char *name ;
        int  size ;
        int  bindtype ;
        char *format ;
    } DBCOLS ;

typedef
    struct _COLUMN_DEFS
    {
        char *name ;
        int  size ; /* # bytes needed to store this item in "C" */
        int  bindtype ;
        char *format ;
    } COLUMN_DEFS ;

typedef
    struct _DB_TABLES
    {
        char *name ;
        COLUMN_DEFS *col_defs ;
    } DB_TABLES ;



/* FUNCTION PROTOTYPES */
/*
-- EXAMPLE for max/min values:
-- return_code = db_get_max_value( APS_dbproc, APS_TABLE(DTK), DTK_REV,
--                          where_clause, APS_CDEFS(DTK), &rev_int ) ;
-- returns TRUE if OK, 
--         FALSE if error.
-- You allocate space for the answer, in this case:  int  rev_int ;
*/
int db_get_max_value(
    DBPROCESS   *dbproc,
    char        *table,         /* APS_TABLE(DTK) or "dtk"                  */
    int         col_index,      /* DTK_REV        or 2                      */
    char        *where_clause,  /* "where dtk.sat = 'A1'"                   */
    COLUMN_DEFS *columns,       /* APS_CDEFS(DTK),                          */
    void        *max_value_ptr);/* result goes here.  caller provides space */
int db_get_min_value(
    DBPROCESS   *dbproc,
    char        *table,         /* APS_TABLE(DTK) or "dtk"                  */
    int         col_index,      /* DTK_REV        or 2                      */
    char        *where_clause,  /* "where dtk.sat = 'A1'"                   */
    COLUMN_DEFS *columns,       /* APS_CDEFS(DTK),                          */
    void        *min_value_ptr);/* result goes here.  caller provides space */

int db_quote_doubler( char *source, char **returned_destination_str ) ;

void db_fprint_record(FILE *dest, DB_RECORD **record, COLUMN_DEFS *columns) ;

int db_copy_record( COLUMN_DEFS *columns, DB_RECORD **destination_rec,
    DB_RECORD **source_rec ) ;

DB_RECORD **db_duplicate_record( COLUMN_DEFS *columns, DB_RECORD **source_rec) ;



/* in db_open.c:  */
void db_close( DBPROCESS *dbproc );

/* default handlers:  */
int db_default_message_handler() ;
int db_default_error_handler() ;

DBPROCESS* db_open( char *dbname, char *program, char *sybase_userid,
    char *password, int (*msg_handler)(), int (*err_handler)(),
    int *error_code ) ;   

DBPROCESS* db_open_server( char *dbname, char *program, char *sybase_userid,
    char *password, int (*msg_handler)(), int (*err_handler)(),
    int *error_code, char *sybase_server ) ;   

void db_open_errs( int error_code, char *dbname, char *sybase_userid) ;

/*
-- use this error handler in db_open to force a process
--  to exit when an error occurs
*/
int error_handler_exit() ;




void free_db_record(DB_RECORD **recordptr) ;

DB_RECORD **new_db_record() ;

DB_RECORD **new_table_record(COLUMN_DEFS *columns) ;

int db_num_records( DBPROCESS *dbproc, char *table, char *where_clause) ;

int db_count_distinct_vals(
    DBPROCESS   *dbproc,
    char        *table,
    int         column_index,
    COLUMN_DEFS *columns,
    char        *where_clause ) ;

void db_make_colnames_string( COLUMN_DEFS *columns, char *col_names) ;

void db_make_format_string( COLUMN_DEFS *columns, char *col_names) ;

llist* db_get_column_data(DBPROCESS *dbproc, char *table_name) ;

llist* db_get_records(DBPROCESS *dbproc, char *table,
    char *where_clause, char *fields_to_order, COLUMN_DEFS *col_defs, ...) ;

int db_insert_values(DBPROCESS *dbproc,
    char *table, char *column_names, char *column_values) ;

int db_install_message_handler(
    int     (*new_message_handler)() ) ;  /* pointer to the function  */

int db_install_error_handler(
    int     (*new_error_handler)() ) ;  /* pointer to the function  */

void db_print_record(DB_RECORD **record, COLUMN_DEFS *columns) ;

void db_print_list(llist *records, COLUMN_DEFS *columns) ;

void db_fprint_list(FILE *destination, llist *records, COLUMN_DEFS *columns) ;

int db_insert_records(DBPROCESS *dbproc,
    llist *records, char *table, COLUMN_DEFS *columns) ;

int db_insert_single_record(DBPROCESS *dbproc,
    DB_RECORD **record, char *table, COLUMN_DEFS *columns) ;

int db_delete_records(DBPROCESS *dbproc, char *table, char *where_clause) ;

int db_update_records(DBPROCESS *dbproc,
    char *table, char *fields_to_set, char *where_clause) ;

DB_RECORD** db_nth_record(llist *records, int n) ;

llist* db_read_records_from_ingres_file(
    COLUMN_DEFS *columns, char *file, int *status) ;

llist *db_record_llist_move( llist *destination, llist *source ) ;

llist *db_record_llist_copy( 
    COLUMN_DEFS *columns, llist *destination, llist *source ) ;

llist *move_db_record2new_llist( llist *destination_list,
    DB_RECORD **db_record, llist *source_list, cursor source_list_ptr );

llist *move_db_record_matches2llist( COLUMN_DEFS *columns,
    int column_index, void *value_pointer, llist *destination_list,
    llist *source_list ) ;

llist *copy_db_record_matches2llist( COLUMN_DEFS *columns,
    int column_index, void *value_pointer, llist *destination_list,
    llist *source_list ) ;

llist *set_db_record_values( COLUMN_DEFS *columns, int column_index,
    void *value_pointer, llist *source_list ) ;

int db_get_min_record( llist *db_rec_list, COLUMN_DEFS *columns,
    int min_search_index, DB_RECORD ***record_containing_min);

int db_get_max_record( llist *db_rec_list, COLUMN_DEFS *columns,
    int max_search_index, DB_RECORD ***record_containing_max);

/* 
-- these utilities are called by the Multi-user library:  
-- they return TRUE on success, FALSE on any other condition.  
*/
int db_begin_tran( DBPROCESS   *dbproc ) ;
int db_commit_tran( DBPROCESS   *dbproc ) ;
int db_rollback_tran( DBPROCESS   *dbproc ) ;

#endif  /* _DB_SYBINT_H_ */
