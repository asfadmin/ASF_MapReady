/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _DB_UTIL_INCLUDED
#define _DB_UTIL_INCLUDED

#include <sybfront.h>
#include <sybdb.h>

#pragma ident   "@(#)db_util.h	1.6  02/21/97"

#define MAXLINELEN  132

/* structure containing Sybase login info */
typedef struct {
    char    username[DBMAXNAME+1];
    char    password[DBMAXNAME+1];
    char    servername[DBMAXNAME+1];
} PPS_LOGIN_STRUCT;

/* structures used for storing result of data queries */
 
typedef struct {
        CS_DATAFMT      datafmt;
        CS_BYTE         *colValue;
} PPS_COLUMN;
 
typedef struct {
        /*---------------------------------------------------------------*/
        /* pointer to Sybase opaque structure used to manage             */
        /* client/server communication                                   */
        /*---------------------------------------------------------------*/
        CS_COMMAND      *cs_command;

        /*---------------------------------------------------------------*/
        /* current state of the query - tells the user what action       */
        /* to take next                                                  */
        /*  CS_ROW_RESULT:      the user needs to call db_fetch          */
        /*  CS_STATUS_RESULT :  procRetCode contains the stored procedure*/
        /*                             return status.                    */
        /*  CS_END_DATA:        all rows have been fetched - no further  */
        /*                             action needed                     */
        /*---------------------------------------------------------------*/
        CS_INT          queryState;

        /*---------------------------------------------------------------*/
        /* information about columns and column data returned from       */
        /* Sybase server as a result of "select" statement.              */
        /*---------------------------------------------------------------*/
        int             colCount;
        PPS_COLUMN      *columns;
 
        /*---------------------------------------------------------------*/
        /* returned status code from stored procedure                    */
        /* (valid only when queryState = CS_STATUS_RESULT)               */
        /*---------------------------------------------------------------*/
        int             procRetCode;

} PPS_QUERY_RESULT;

/*---------------------------------------------------------------*/
/* Structure which provides the list of names and                */
/* datatype format for columns of a given database table         */
/*---------------------------------------------------------------*/
 
typedef struct {
        char            colName[DBMAXNAME+1];
        CS_INT          colType;
        CS_TINYINT      colLength;
        CS_INT          colRuleId;

} PPS_COLUMN_STRUCT;

/*---------------------------------------------------------------*/
/* Structure which provides the list of dataserver names         */
/* specified in $SYBASE/interfaces file                          */
/*---------------------------------------------------------------*/

typedef char ServerName[DBMAXNAME+1];
typedef ServerName *DATA_SERVER_LIST; 

/*---------------------------------------------------------------*/
/* login to Sybase server                                        */
/*---------------------------------------------------------------*/
CS_RETCODE db_connect(PPS_LOGIN_STRUCT *loginStruct,   /* in */
              CS_CONNECTION    **connection);  /* out */
 
/*---------------------------------------------------------------*/
/* logout the connection with Sybase server                      */
/* and all memory associated with the connection structure.      */
/*---------------------------------------------------------------*/
CS_RETCODE db_disconnect(CS_CONNECTION *connection);

/*---------------------------------------------------------------*/
/* change the current database to the named database passed-in   */ 
/* for the connection                                            */ 
/*---------------------------------------------------------------*/
CS_RETCODE db_set_database(CS_CONNECTION **connection,  /* in */
							char          *dbName);     /* in */
 
/*---------------------------------------------------------------*/
/* db_exec sends the sql statement *cmdString to                 */
/* Sybase server and allocates buffer space to receive data returned */
/* from Sybase server.                                           */
/* db_exec returns:                                              */
/*  CS_SUCCEED - a result set is available to be fetched.        */
/*  CS_FAIL    - routine failed.                                 */
/*---------------------------------------------------------------*/
CS_RETCODE db_exec (CS_CONNECTION     **connection,     /* in */
                    char              *cmdString,       /* in */
                    int               rowsPerFetch,     /* in */
                    PPS_QUERY_RESULT  **queryResult);   /* out */


/*-----------------------------------------------------------------*/
/* fetches the next set of rows from the result set and returns to */
/* the caller in queryResult.                                      */
/* db_fetch returns :                                              */
/*  CS_SUCCEED - a row is successfully fetched and placed in *queryResult. */
/*  CS_END_DATA - all rows have been fetched.                      */
/*  CS_FAIL    - failure to fetch a row - nonrecoverable error.    */
/*                                                                 */
/* The caller should call db_fetch until CS_END_DATA or CS_FAIL    */
/*-----------------------------------------------------------------*/
CS_RETCODE db_fetch (PPS_QUERY_RESULT   *queryResult,   /* in out */
                     int                *rowsFetched);  /* out */

/*-----------------------------------------------------------------*/
/* freeQueryResult frees the memory used by queryResult            */
/*-----------------------------------------------------------------*/
 
void freeQueryResult (PPS_QUERY_RESULT *queryResult);

/*-----------------------------------------------------------------*/
/* Routine which provides the list of names and                    */
/* datatype format for columns of a given database table           */
/*-----------------------------------------------------------------*/

CS_RETCODE getTableColumnDesc  (CS_CONNECTION     **connection,      /* in */
                                char              *tableName,        /* in */
                                PPS_COLUMN_STRUCT **columnDescList,  /* out */
                                int               *colCount);        /* out */

void freeTableColumnDesc (PPS_COLUMN_STRUCT *columnDescList);  /* in */

/******************************************************************************
** Routine which returns an array of the data servers specified in the 
** $SYBASE/interfaces file.
******************************************************************************/

CS_RETCODE getServerList (DATA_SERVER_LIST  *serverListPtr,     /* out */
                  int           *serverCount);      /* out */

#ifdef FOR_SALLY
 
CS_RETCODE getColumnOptions (CS_CONNECTION     **connection,      /* in */
                             char              *tableName,        /* in */
                             char              *colName,          /* in */
                             PPS_QUERY_RESULT  **colOptionList);  /* out */
#endif

#endif /*_DB_UTIL_INCLUDED*/
