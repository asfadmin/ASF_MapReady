/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	db_sybint.h

Description:	
	Function Declarations for accessing data contained in Sybase tables
	The function definitions make use of the Open Client-Library functions
	to access the data

Creator:	Ron Green
==============================================================================*/

#ifndef _DB_SYBINT_H_
#define _DB_SYBINT_H_

#pragma ident "@(#)db_sybint.h	1.1  11/21/96"

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

#define DB_OPEN_OK			0
#define DB_OPEN_ERR_DBNAME	1
#define DB_OPEN_ERR_PROG	2
#define DB_OPEN_ERR_USER	3
#define DB_OPEN_ERR_PASSWD	4
#define DB_OPEN_ERR_DBINIT	5
#define DB_OPEN_ERR_DBLOGIN	6
#define DB_OPEN_ERR_DBOPEN	7
#define DB_OPEN_ERR_DBUSE	8
#define DB_OPEN_ERR_OPEN_COMMIT	9

/* define return codes from db_copy_record */
#define DB_RECORD_NULL              -1
#define DB_COPY_RECORD_OK            1

#define MAXITEMS 60
#define END_COLS -1
#define ALL_COLS -1

#define DB_FILE_ERROR 0
#define DB_FILE_OK    1

/* defines for sybsys colums */
#define SYBCOL_NAME	0
#define SYBCOL_TYPE	1

#define CAST_SYBCOL_NAME /* strings don't need cast types */
#define CAST_SYBCOL_TYPE *( DBINT *)

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
		int  size ;
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

int db_copy_record(
    COLUMN_DEFS *columns,
    DB_RECORD   **destination_rec,
    DB_RECORD   **source_rec ) ;

DBPROCESS* db_open(
    char    *dbname,
    char    *program,
    char    *sybase_userid,
    char    *password,
    char    *server_name,
    int     (*msg_handler)(),
    int     (*err_handler)(),
    int     *error_code,
    LOGINREC **login) ;

int db_open_errs(int error_code, char *dbname, char *sybase_userid) ;

/* 
-- use this error handler in db_open to force a process 
-- 	to exit when an error occurs
*/
int error_handler_exit() ;

void free_db_record(DB_RECORD **recordptr) ;

DB_RECORD** new_db_record() ;

DB_RECORD**  new_table_record(COLUMN_DEFS *columns) ;

int db_num_records(
	DBPROCESS *dbproc, char *table, char *where_clause) ;

void db_make_colnames_string(
	char *table, COLUMN_DEFS *columns, char *col_names) ;

void db_make_format_string(char *table, COLUMN_DEFS *columns, char *col_names) ;

llist* db_get_column_data(DBPROCESS *dbproc, char *table_name) ;

llist* db_get_records(DBPROCESS *dbproc, 
	char *table, char *where_clause, char *fields_to_order, 
	COLUMN_DEFS *col_defs, ...) ;

int db_insert_values(DBPROCESS *dbproc, 
	char *table, char *column_names, char *column_values) ;

void db_print_record(DB_RECORD **record, COLUMN_DEFS *columns) ;

int db_insert_records(DBPROCESS *dbproc, 
	llist *records, char *table, COLUMN_DEFS *columns) ;

int db_delete_records(DBPROCESS *dbproc, 
	char *table, char *where_clause) ;

int db_update_records(DBPROCESS *dbproc, 
	char *table, char *fields_to_set, char *where_clause) ;

DB_RECORD** db_nth_record(llist *records, int n) ;

llist* db_read_records_from_ingres_file(
    char *table, COLUMN_DEFS *columns, char *file, int *status) ;

#endif
