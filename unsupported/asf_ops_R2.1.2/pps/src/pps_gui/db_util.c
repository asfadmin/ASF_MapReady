/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static const char rcsid[] = "@(#)db_util.c	1.6  02/21/97";

#include <stdio.h>
#include <ctpublic.h>
#include <cstypes.h>
#include "db_util.h"
 
static CS_CONTEXT       *context;
static int          context_init = 0;
static int      error_severity;


/****************************************************************************/

CS_RETCODE CS_PUBLIC
clientmsg_cb(context, connection, errmsg)
CS_CONTEXT      *context;
CS_CONNECTION   *connection;
CS_CLIENTMSG    *errmsg;
{
    /* disregard informational message */
    if (CS_SEVERITY(errmsg->msgnumber) == CS_SV_INFORM)
        return (CS_SUCCEED); 

    /* log the error */
        fprintf(stderr, "\nOpen Client Message:\n");
        fprintf(stderr, "Message number: LAYER = (%ld) ORIGIN = (%ld) ",
                CS_LAYER(errmsg->msgnumber), CS_ORIGIN(errmsg->msgnumber));
        fprintf(stderr, "SEVERITY = (%ld) NUMBER = (%ld)\n",
                CS_SEVERITY(errmsg->msgnumber), CS_NUMBER(errmsg->msgnumber));
        fprintf(stderr, "Message String: %s\n", errmsg->msgstring);
        if (errmsg->osstringlen > 0)
        {
                fprintf(stderr, "Operating System Error: %s\n",
                        errmsg->osstring);
        }
        fflush(stderr);

    /* determine the severity of the error 
    if (CS_SEVERITY(errmsg->msgnumber) < CS_SV_RESOURCE_FAIL)
        error_severity = ER_DB_NONFATAL;
    else
        error_severity = ER_DB_FATAL;
    */
 
        return (CS_SUCCEED);
}

/*****************************************************************************/

CS_RETCODE CS_PUBLIC
servermsg_cb(context, connection, srvmsg)
CS_CONTEXT      *context;
CS_CONNECTION   *connection;
CS_SERVERMSG    *srvmsg;
{
    /* disregard informational message */
    if (srvmsg->severity == 10)
        return (CS_SUCCEED);

    /* log the error */
        fprintf(stderr, "\nServer message:\n");
        fprintf(stderr, "Message number: %ld, Severity %ld, ",
                srvmsg->msgnumber, srvmsg->severity);
        fprintf(stderr, "State %ld, Line %ld\n",
                srvmsg->state, srvmsg->line);
 
        if (srvmsg->svrnlen > 0)
        {
                fprintf(stderr, "Server '%s'\n", srvmsg->svrname);
        }
 
        if (srvmsg->proclen > 0)
        {
                fprintf(stderr, " Procedure '%s'\n", srvmsg->proc);
        }
 
        fprintf(stderr, "Message String: %s\n", srvmsg->text);
        fflush(stderr);

    /*  error_severity = ER_DB_FATAL; */
    
        return (CS_SUCCEED);
}

/*****************************************************************************/

CS_RETCODE db_init_context ()
{
    CS_RETCODE  retcode;

        retcode = cs_ctx_alloc(CS_VERSION_100, &context);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "cs_ctx_alloc() failed with %d\n",
                        retcode);
                return (retcode); 
        }
 
        /* init ctlib */
        retcode = ct_init(context, CS_VERSION_100);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_init() failed with %d\n", retcode);
                return(retcode);
        }
 
        /* install message callback routines */
        retcode = ct_callback (context, NULL, CS_SET, CS_CLIENTMSG_CB,
                        (CS_VOID *)clientmsg_cb);
        if (retcode != CS_SUCCEED)
        {
                fprintf(stderr, "ct_callback(clientmsg) failed with %d\n", retcode);
                return(retcode);
        }
        retcode = ct_callback (context, NULL, CS_SET, CS_SERVERMSG_CB,
                        (CS_VOID *)servermsg_cb);
        if (retcode != CS_SUCCEED)
        {
                fprintf(stderr, "ct_callback(servermsg) failed with %d\n", retcode);
                return(retcode);
        }
 
    return (CS_SUCCEED);    
}

/*****************************************************************************/

CS_RETCODE db_connect (PPS_LOGIN_STRUCT *loginStruct,   /* in */
               CS_CONNECTION    **connection)  /* out */
{
    CS_RETCODE  retcode;

    /* only need to initialize database context once */
    if (context_init == 0)
    {   
        if ((db_init_context()) != CS_SUCCEED)
            return (CS_FAIL);
        else
            context_init = 1;
    }
        
        /* allocate connection context */
        retcode = ct_con_alloc(context, connection);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_alloc() failed with %d\n", retcode);
                return(CS_FAIL);
        }
 
        /* establish connection properties */
        retcode = ct_con_props(*connection, CS_SET, CS_USERNAME,
                  loginStruct->username, CS_NULLTERM, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_props(CS_USERNAME) failed with %d\n", retcode);
                return(CS_FAIL);
        }

        retcode = ct_con_props(*connection, CS_SET, CS_PASSWORD,
                  loginStruct->password, CS_NULLTERM, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_props(CS_PASSWORD) failed with %d\n", retcode);
                return(CS_FAIL);
        }

        retcode = ct_con_props(*connection, CS_SET, CS_APPNAME,
                        "PPS_GUI", CS_NULLTERM, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_props(CS_APPNAME) failed with %d\n", retcode);
        }

        /* connect to the server */
        retcode = ct_connect(*connection, loginStruct->servername, 
                CS_NULLTERM);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_connect() failed with %d\n", retcode);
                return(CS_FAIL);
        }
    return (CS_SUCCEED);

}

/*****************************************************************************/
 
CS_RETCODE check_connection_status (CS_CONNECTION **connection)
{
        CS_RETCODE retcode;
        CS_INT     connection_status;
 
        /* check if the passed-in connection is still good */
        retcode = ct_con_props(*connection, CS_GET, CS_CON_STATUS,
                                &connection_status, CS_UNUSED, NULL);
        if (retcode != CS_SUCCEED)
        {
                fprintf(stderr, "Failed to check connection status\n");
                return (retcode);
        }
    /* if this connection is dead, drop it, and allocate a new one */
        if (connection_status == CS_CONSTAT_DEAD)
        {
        (void)ct_con_drop(*connection); 
    /*  retcode = db_connect(connection);  */
        if (retcode != CS_SUCCEED)
        {
            fprintf(stderr, "Failed to make a new connection\n");
            return (retcode);
        }
        }
        return (CS_SUCCEED);
}

/*****************************************************************************/
 
static CS_RETCODE
get_return_status (PPS_QUERY_RESULT *queryResult)
{
    CS_RETCODE  retcode;
    CS_DATAFMT  datafmt;
    CS_INT      rows_read;

        /* bind the return status to an int */
        datafmt.datatype = CS_INT_TYPE;
        datafmt.format = CS_FMT_UNUSED;
        datafmt.scale = CS_SRC_VALUE;
        datafmt.precision = CS_SRC_VALUE;
        datafmt.count = 1;
        datafmt.locale = NULL;

    if ((ct_bind(queryResult->cs_command, 1, &datafmt,
        &(queryResult->procRetCode), NULL, NULL)) != CS_SUCCEED) 
    {
        fprintf(stderr, "ct_bind() failed in get_return_status\n");
                return(CS_FAIL);
    }

    /* fetch the return status */   
        while (((retcode = ct_fetch(queryResult->cs_command, CS_UNUSED, 
        CS_UNUSED, CS_UNUSED, &rows_read)) == CS_SUCCEED) ||
                (retcode == CS_ROW_FAIL))
        {
        /* 
        ** CT-Lib is putting the data in queryResult->procRetCode 
        */
        queryResult->queryState = CS_STATUS_RESULT;
    }
        switch (retcode)
        {
                case CS_END_DATA:
                case CS_CANCELED:
            return (CS_SUCCEED);
                        break;
                case CS_FAIL:
        default:
                        return (CS_FAIL);
                        break;
        }
}

/*****************************************************************************/

CS_RETCODE db_fetch (PPS_QUERY_RESULT   *queryResult,   /* in out */
                     int                *rowsFetched)   /* out */
{
    CS_RETCODE  retcode;
    int     i, result_type;

    /* make sure the user calls this function at the appropriate time */
    if (queryResult->queryState != CS_ROW_RESULT)
    {
        fprintf(stderr,"No rows to be fetched\n");
        return (CS_FAIL);
    }
        /* check for valid command structure */
        if (queryResult->cs_command == (CS_COMMAND *) NULL)
    {
        fprintf(stderr,"Invalid command structure passed to db_fetch\n");
        return (CS_FAIL);
    }

    for (i = 0; i < queryResult->colCount; i++) 
    {
                retcode = ct_bind(queryResult->cs_command, i+1,
                        &(queryResult->columns[i].datafmt),
                        queryResult->columns[i].colValue, NULL, NULL);
                if (retcode != CS_SUCCEED) {
                        fprintf(stderr, "ct_bind() failed with %d in \
                                db_fetch\n", retcode);
                        return(CS_FAIL);
                }
    }

    retcode = ct_fetch(queryResult->cs_command, CS_UNUSED, CS_UNUSED,
                       CS_UNUSED, rowsFetched);
    /*
    ** CS_SUCCEED retcode indicates that ct_fetch succeeded - but
    ** still more rows to be fetched - return this retcode to the
    ** caller who should call db_fetch again
    */ 
    if (retcode == CS_SUCCEED)
        return (CS_SUCCEED);

    /* 
    ** any retcode other than CS_END_DATA indicates something
    ** went wrong - return CS_FAIL to the caller 
    */
    if (retcode != CS_END_DATA)
        return (CS_FAIL);

    /* 
    ** retcode = CS_END_DATA -> this means all rows have been fetched.
    ** Since we do not support multiple result sets, cancel any
    ** pending results to get a clean command structure.  
    ** The only exception is stored procedure returning status code.
    */
    while ((retcode = ct_results(queryResult->cs_command, 
                    &result_type)) == CS_SUCCEED) 
    {
                switch (result_type) 
        {
            case CS_CMD_DONE:
            case CS_CMD_SUCCEED:
            case CS_CMD_FAIL:
                /*
                ** This means no rows were returned, stay in
                ** the ct_results loop
                */
                queryResult->queryState = CS_END_DATA;
                break;
            case CS_STATUS_RESULT:
                /* stored procedure returning status */
                if (get_return_status(queryResult) !=
                    CS_SUCCEED)
                {
                    fprintf(stderr,"Failed to get_return_"
                        "status in db_fetch\n");
                }
                break;  
            case CS_ROW_RESULT:
            case CS_PARAM_RESULT:
            case CS_COMPUTE_RESULT:
                /*  
                ** This means the command buffer contains
                ** multiple commands - we cannot handle
                ** multiple result sets, so cancel them all
                */
                fprintf(stderr,"\nMultiple result sets not"
                        " supported\n");
                ct_cancel(NULL,queryResult->cs_command,
                      CS_CANCEL_ALL);
                break;      
            
            default:
                            fprintf(stderr, "unexpected result_type (%d) "
                        "in db_fetch\n", result_type);
                ct_cancel(NULL,queryResult->cs_command,
                                          CS_CANCEL_ALL);
                        break;
                }
    }
    if (retcode != CS_END_RESULTS)
    {
        fprintf(stderr,"unexpected retcode (%d) for ct_results "
                   "in db_fetch\n", retcode);
    }
    return (CS_END_DATA);

}

/*****************************************************************************/

static CS_RETCODE 
setup_result_buffer
    (PPS_QUERY_RESULT    **queryResult,
     int             rowsPerFetch) 
{
        CS_RETCODE      retcode;
    CS_COMMAND  *command = (*queryResult)->cs_command;
    int     i, j;

        /*
    ** get the number of columns returned from the query 
    */
        retcode = ct_res_info(command, CS_NUMDATA,
                 &((*queryResult)->colCount), CS_UNUSED, NULL);
        if (retcode != CS_SUCCEED) 
    {
                fprintf(stderr, "ct_res_info() failed with %d in get_rows\n", 
            retcode);
                return(CS_FAIL);
        }
#if 0
printf("colCount = %d\n\n\n\n",(*queryResult)->colCount);
#endif
    /*
    ** allocate space in queryResult to hold data returned from the query
    */

    if (((*queryResult)->columns = 
                 (PPS_COLUMN *)malloc ((unsigned)(*queryResult)->colCount *
                 (unsigned)sizeof (PPS_COLUMN))) == (PPS_COLUMN *) NULL)
    {
        fprintf(stderr,"Could not allocate memory for \
            queryResult->columns in setup_result_buffer\n");
        return (CS_FAIL);
    }

    /* 
    ** get data format of columns returned from the query to do the binds.
    */
        for (i = 0; i < (*queryResult)->colCount; i++) 
    {
                retcode = ct_describe(command,i+1,
                          &((*queryResult)->columns[i].datafmt));
                if (retcode != CS_SUCCEED) 
        {
                        fprintf(stderr, "ct_describe() failed with %d\n",
                                retcode);
                        return(CS_FAIL);
                }
        /*
        ** modify the datafmt structure to do the bind 
        */
        if ((*queryResult)->columns[i].datafmt.datatype == CS_CHAR_TYPE)
        {
            (*queryResult)->columns[i].datafmt.format = 
                        CS_FMT_NULLTERM;
            /* allow for null character */
            (*queryResult)->columns[i].datafmt.maxlength++;
        }
        else
        {
            (*queryResult)->columns[i].datafmt.format =
                        CS_FMT_UNUSED;
        }           
        (*queryResult)->columns[i].datafmt.count = rowsPerFetch;

            if (((*queryResult)->columns[i].colValue = (CS_BYTE *) malloc
                        (rowsPerFetch * 
             (*queryResult)->columns[i].datafmt.maxlength)) 
                == (CS_BYTE *) NULL)
        {
                        fprintf(stderr,"Could not allocate memory for data in \
                                 setup_result_buffer\n");
                        return (CS_FAIL);
        }
        }

        return (CS_SUCCEED);

}


/*****************************************************************************/

CS_RETCODE db_exec(
CS_CONNECTION     **connection,     /* in */
char              *cmdString,       /* in */
int               rowsPerFetch,     /* in */
PPS_QUERY_RESULT  **queryResult)    /* out */
{
    CS_RETCODE      retcode;
    CS_COMMAND  *command;
    int     result_type;
    int     exec_succeed = 1;
    CS_BOOL     expose_key = CS_TRUE;
    CS_BOOL     expose_key_prop = CS_FALSE;

    *queryResult = (PPS_QUERY_RESULT *) NULL;
    
        /* check if the passed-in connection is still good, if not,
           drop it and allocate a new one which will get passed back
           to the caller */
    if ( check_connection_status(connection) != CS_SUCCEED)
    {
        fprintf(stderr,"db_exec failed to check_connection_status\n");
        return (CS_FAIL);
    }


    /* allocate command structure */
        retcode = ct_cmd_alloc(*connection, &command);
    if (retcode != CS_SUCCEED)
    {
                fprintf(stderr, "ct_cmd_alloc() failed with %d in db_exec\n", 
            retcode);
        return (CS_FAIL);
        }

        /* fill in command string to be sent to Sybase server */
        retcode = ct_command(command, CS_LANG_CMD, 
                 cmdString, CS_NULLTERM, CS_UNUSED);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_command() failed with %d in db_exec\n", 
            retcode);
        ct_cmd_drop (command);
        return (CS_FAIL);
        }

        /* allocate queryResult */
        if ((*queryResult = (PPS_QUERY_RESULT *)
                malloc ((unsigned)sizeof (PPS_QUERY_RESULT))) ==
                (PPS_QUERY_RESULT *) NULL)
        {
                fprintf(stderr,"Could not allocate memory for queryResult \
                        in db_exec\n");
                return (CS_FAIL);
        }
 
        /* send command */
        retcode = ct_send(command);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_send() failed with %d in db_exec\n", 
                retcode);
        ct_cmd_drop (command);
        return (CS_FAIL);
        }

    /* save the command structure */
    (*queryResult)->cs_command = command;
    
    /* initialize the queryState */
    (*queryResult)->queryState = CS_CMD_DONE;

        /* process results */
        while ((retcode = ct_results(command, &result_type)) == CS_SUCCEED) {
                switch (result_type) {

                case CS_ROW_RESULT:
                        if ((setup_result_buffer(queryResult, rowsPerFetch)) 
                    != CS_SUCCEED)
            {
                fprintf(stderr,"Failed to setup_result_buffer"
                    " for db_exec\n");
                return (CS_FAIL);
            }
            /* return to caller who will initiate db_fetch */
            (*queryResult)->queryState = CS_ROW_RESULT;
            return (CS_SUCCEED);
         
                case CS_STATUS_RESULT:
            /* the command is to execute a stored procedure which 
            ** is returning a status code */ 
            if ((get_return_status(*queryResult)) != CS_SUCCEED)
            {
                fprintf(stderr,"Failed to get_return_status"
                    " for db_exec\n");
                (*queryResult)->procRetCode = 0;
                return (CS_FAIL);
            }
            (*queryResult)->queryState = CS_STATUS_RESULT;
            break;

                case CS_CMD_FAIL:
                        fprintf(stderr, "failure in \"%s\"\n", cmdString);
                        exec_succeed = 0;
                        break;
                case CS_CMD_DONE:
                case CS_CMD_SUCCEED:
                        break;
                default:
                        fprintf(stderr, "unexpected result_type %d in db_exec \n", result_type);
                        exec_succeed = 0;
                        break;
                }
        }
        switch (retcode) 
    {
            case CS_END_RESULTS:
            case CS_CANCELED:
                    break;
            case CS_FAIL:
                    exec_succeed = 0;
                    break;
            default:
                    fprintf(stderr, "unexpected final status %d\n", retcode);
                    exec_succeed = 0;
                    break;
        }
        if (exec_succeed == 0)
    {
        ct_cmd_drop (command);
        return (CS_FAIL);
    }
        else
    {
                return (CS_SUCCEED);
    }
}

/****************************************************************************/

CS_RETCODE db_disconnect (CS_CONNECTION *connection)
{
        (void)ct_close(connection,CS_UNUSED);
        (void)ct_con_drop(connection);
        (void)ct_exit(context, CS_FORCE_EXIT);
        (void)cs_ctx_drop(context);
    context_init = 0;
}

/*****************************************************************************/
 
CS_RETCODE db_set_database (CS_CONNECTION **connection,
                            char          *dbName)
{
        CS_RETCODE          retcode;
        PPS_QUERY_RESULT        *queryResult;
        char                cmdString[35];
 
        sprintf(cmdString, "use %s", dbName);
 
        /* call db_exec to execute the sql statement */
        if (db_exec (connection, cmdString, 0, &queryResult) == CS_FAIL)
        {
                (void)fprintf(stderr, "db_set_database failed to change\
                                to '%s' database\n", dbName);
        freeQueryResult(queryResult);
                return (CS_FAIL);
        }
    freeQueryResult(queryResult);
    return (CS_SUCCEED);
}

/******************************************************************************
** This function converts the server datatype to the CT-lib datatype. 
******************************************************************************/

static
CS_INT convertCStype (CS_TINYINT server_datatype)
{
    switch (server_datatype)
    {
                case SYBINT4:
                case SYBINTN:
                        return (CS_INT_TYPE);
                        break;
                case SYBCHAR:
                case SYBVARCHAR:
                        return (CS_CHAR_TYPE);
                        break;
                case SYBFLT8:
                case SYBFLTN:
                        return (CS_FLOAT_TYPE);
                        break;
                case SYBINT1:
                        return (CS_TINYINT_TYPE);
                        break;
                case SYBINT2:
                        return (CS_SMALLINT_TYPE);
                        break;
        case SYBBINARY:
        case SYBVARBINARY:
            return (CS_BINARY_TYPE);
            break;
        case SYBBIT:
            return (CS_BIT_TYPE);
            break;
        case SYBDATETIME4:
            return (CS_DATETIME4_TYPE);
            break;
        case SYBDATETIME:
        case SYBDATETIMN:
            return (CS_DATETIME_TYPE);
            break;
        case SYBDECIMAL:
            return (CS_DECIMAL_TYPE);
            break;
        case SYBREAL:
            return (CS_REAL_TYPE);
            break;
        case SYBIMAGE:
            return (CS_IMAGE_TYPE);
            break;
        case SYBMONEY4:
        case SYBMONEYN:
            return (CS_MONEY4_TYPE);
            break;
        case SYBMONEY:
            return (CS_MONEY_TYPE);
            break;
        case SYBNUMERIC:
            return (CS_NUMERIC_TYPE);
            break;
        case SYBTEXT:
            return (CS_TEXT_TYPE);
            break;
        default:
            return (CS_ILLEGAL_TYPE);
            break;
    }
}

/******************************************************************************
** This function returns the description of all columns for the
** specified database table.  
******************************************************************************/

CS_RETCODE getTableColumnDesc (
CS_CONNECTION     **connection,      /* in */
char              *tableName,        /* in */
PPS_COLUMN_STRUCT **columnDescList,  /* out */
int               *colCount)         /* out */
{
    PPS_QUERY_RESULT    *queryResult;
    CS_RETCODE      retcode;
    char            cmdString[200];
    int         rowsRead, i, col_len;
    int         listSize = 0;
    static int      rowsPerFetch = 30;
    CS_TINYINT      syb_datatype;

    *colCount = 0;

    /* 
    ** populate the command buffer with the sql statement to
    ** get info about the columns in table specified *tableName
    ** from syscolumns system table 
    */
    (void)sprintf(cmdString,"select name, type, length, domain from \
            syscolumns where id = object_id('%s') order by name", 
            tableName);

    /* call db_exec to execute the sql statement */
    if (db_exec (connection, cmdString, rowsPerFetch, &queryResult)
             == CS_FAIL)
    {
        (void)fprintf(stderr, "getColumnDesc failed to retrieve info "
                               "from syscolumns system table\n");
        *columnDescList = (PPS_COLUMN_STRUCT *) NULL;
        return (CS_FAIL);
    }

    /*
    ** allocate space for ColumnDescList -
    ** initial size = rowsPerFetch
    */ 
    listSize = rowsPerFetch;
    if ((*columnDescList = (PPS_COLUMN_STRUCT *)
                malloc (sizeof (PPS_COLUMN_STRUCT) * listSize)) ==
                (PPS_COLUMN_STRUCT *) NULL)
    {
                fprintf(stderr, "Could not allocate memory for columnDescList \
                                in getColumnDesc\n");
                *columnDescList = (PPS_COLUMN_STRUCT *) NULL;
        freeQueryResult(queryResult);
                return (CS_FAIL);
    }

    /*
    ** call db_fetch to get the data and populate the ColumnDescList with 
    ** data returned in queryResult.  
    */
    while ((retcode = db_fetch (queryResult, &rowsRead)) == CS_SUCCEED) 
    {
        /*
        ** allocate more space in ColumnDescList if not enough left 
        ** to hold the data returned from database server 
        */
        if (((*colCount) + rowsRead) > listSize)
        {
            listSize += rowsPerFetch;
            if ((*columnDescList = (PPS_COLUMN_STRUCT *)
                   realloc(*columnDescList, 
                   sizeof(PPS_COLUMN_STRUCT) * listSize)) == 
                       (PPS_COLUMN_STRUCT *) NULL)
                {
                        fprintf(stderr, "Could not reallocate memory for"
                    " columnDescList in getColumnDesc\n");
                        *columnDescList = (PPS_COLUMN_STRUCT *) NULL;
                freeQueryResult(queryResult);
                        return (CS_FAIL);
                }

        }
            for (i = 0; i < rowsRead; i++)
            {
                    /* column name */
                    col_len = queryResult->columns[0].datafmt.maxlength;
                    (void) memcpy ((*columnDescList)[*colCount].colName,
                           queryResult->columns[0].colValue + i * col_len, 
                col_len);
                    (*columnDescList)[*colCount].colName[col_len] = '\0';
 
                    /* column data type */
                    col_len = queryResult->columns[1].datafmt.maxlength;
                    (void) memcpy (&syb_datatype,
                           queryResult->columns[1].colValue +i*col_len, 
                   col_len);
                    (*columnDescList)[*colCount].colType = 
                convertCStype(syb_datatype);
 
                    /* column maximum length */
                    col_len = queryResult->columns[2].datafmt.maxlength;
                    (void) memcpy (&((*columnDescList)[*colCount].colLength),
                         queryResult->columns[2].colValue + i*col_len, 
                 col_len);

            /* column rule id */
            col_len = queryResult->columns[3].datafmt.maxlength;
            (void)memcpy(&((*columnDescList)[*colCount].colRuleId),
                                 queryResult->columns[3].colValue + i*col_len,
                                 col_len);

            (*colCount)++;
            }
    }
    /* 
    ** free the memory used by queryResult 
    */
    freeQueryResult (queryResult);

    /*
    ** all done
    */
    return (CS_SUCCEED);
    
}

/******************************************************************************
** This function frees the memory used by queryResult
******************************************************************************/
 
void freeQueryResult (PPS_QUERY_RESULT *queryResult)
{
    int i;

    if (queryResult == (PPS_QUERY_RESULT *) NULL)
        return;

    /* free the memory associated with the sybase command structure */
    if (queryResult->cs_command != (CS_COMMAND *) NULL)
    {
        ct_cmd_drop (queryResult->cs_command);
    }

    /* free the data buffer */
    for (i = 0; i < queryResult->colCount; i++)
    {
        if (queryResult->columns[i].colValue != (CS_BYTE *) NULL)
        {
            free (queryResult->columns[i].colValue);
        }       
    }
    free (queryResult);
}


/******************************************************************************
** This function frees the list of column description
** (generated by getTableColumnDesc function)
******************************************************************************/

void freeTableColumnDesc (PPS_COLUMN_STRUCT *columnDescList)
{
    if (columnDescList != (PPS_COLUMN_STRUCT *) NULL)
    {
        free (columnDescList);
    }
}

#ifdef FOR_SALLY
/******************************************************************************
** (Will delete this function once Sally puts it in her code).
** This function returns the list of option values for the specified 
** specified table column.  
******************************************************************************/

CS_RETCODE getColumnOptions (
CS_CONNECTION     **connection,      /* in */
char              *tableName,        /* in */
char              *colName,          /* in */
PPS_QUERY_RESULT  **colOptionList)   /* out */
{
    CS_RETCODE      retcode;
    char            cmdString[100];
    int         rowsRead, i, col_len, datalen;
    int         listSize = 0;
    static int      rowsPerFetch = 30;
    char            optionValue[256];

    /* 
    ** populate the command buffer with the sql statement to
    ** execute the stored procedure "sp_getColumnOptions" to
    ** retrieve the option values for the specified column 
    ** from syscomments system table 
    */
    (void)sprintf(cmdString,"sp_getColumnOptions '%s', '%s'", 
              colName, tableName);

    /* call db_exec to execute the sql statement */
    if (db_exec (connection, cmdString, rowsPerFetch, colOptionList)
             == CS_FAIL)
    {
        (void)fprintf(stderr, "getColumnOptions failed to retrieve \
                info from system table\n");
        return (CS_FAIL);
    }

    /*
    ** call db_fetch to get the data and print the 
    ** data returned in queryResult.  
    */
    while ((retcode = db_fetch (*colOptionList, &rowsRead)) == CS_SUCCEED)  
    {
        col_len = (*colOptionList)->columns[0].datafmt.maxlength;
            for (i = 0; i < rowsRead; i++)
            {
            col_len = (*colOptionList)->columns[0].datafmt.maxlength;
                    (void) memcpy (optionValue,
                       (*colOptionList)->columns[0].colValue + i * col_len, 
                col_len);
            printf("optionValue = '%s'", optionValue);
 
            }
    }
    /*
    ** all done
    */
    return (CS_SUCCEED);
    
}
#endif

/******************************************************************************
** Routine which returns an array of the data servers specified in the
** $SYBASE/interfaces file.
******************************************************************************/
 
CS_RETCODE getServerList (DATA_SERVER_LIST  *serverListPtr,     /* out */
                          int                   *serverCount)       /* out */
{
    char    *SybaseDir; 
    char    *interfaceFileName;
    char    *fileNameFormat = "%s/interfaces";
    char    *ptr;
    char    lineBuf[MAXLINELEN+1];
    char    name[DBMAXNAME+1];
    FILE    *interfaceFileDesc;
    int     ListSize = 0;
    static int sizePerMalloc = 15;

    /*
    ** Make sure we can open the interfaces file for "read".
    ** The interfaces file is in the $SYBASE directory.
    */
    if ((SybaseDir = (char *)getenv("SYBASE")) == (char *) NULL)
    {
        fprintf (stderr,"SYBASE env variable must be set\n");
        return (CS_FAIL);
    }
    /*
    ** Allocate space for the interface file name
    */
    if ((interfaceFileName = (char *) malloc (strlen(SybaseDir) +
        strlen(fileNameFormat) +1)) == (char *) NULL)
    {
        fprintf (stderr,"Could not allocate space for interface "
                "file name\n");
        return (CS_FAIL);
    }
    sprintf(interfaceFileName, fileNameFormat, SybaseDir);
    /*
    ** Now try to open the interfaces file
    */
    if ((interfaceFileDesc = fopen (interfaceFileName, "r")) 
        == (FILE *) NULL)
    {
        fprintf(stderr,"Could not open interface file %s\n",
            interfaceFileName);
        free (interfaceFileName);
        return (CS_FAIL);
    }

    free(interfaceFileName);    
    /*
    ** malloc space for the array with size "serverListSize" to start
    ** with.  If this is not enough, realloc will be called inside the
    ** reading loop as often as needed
    */
        if ((*serverListPtr = (ServerName *) 
        malloc (sizePerMalloc * sizeof(ServerName)))
                        == (ServerName *) NULL)
        {
                fprintf (stderr, "Could not allocate memory for server list\n");
                fclose (interfaceFileDesc);
                return (CS_FAIL);
        }
    *serverCount = 0;
    ListSize = sizePerMalloc;
    while ((ptr = fgets(lineBuf, MAXLINELEN, interfaceFileDesc)) 
            != (char *) NULL)
    {
        /* 
        ** if this is not a server name, read the next line
        */
        if (isspace(*ptr) || (*ptr == '#')) continue;

        /*
        ** if there is not enough space in the array, 
        ** realloc() the array 
        */
        if (*serverCount == ListSize-1)
        {
            ListSize += sizePerMalloc;
            if ((*serverListPtr = (ServerName *)
                        realloc (*serverListPtr, 
                    ListSize * sizeof(ServerName)))
                                == (ServerName *) NULL)
            {
                fprintf(stderr,"Could not reallocate space "
                    "for server list\n");
                fclose(interfaceFileDesc);
                return (CS_FAIL);
            }
        }
        /*
        ** copy the server name into the returned list
        */
        if (sscanf(lineBuf, "%s", name) != 1)
        {
            fprintf(stderr,"Could not recognize interfaces file"
                    " format\n");
            fclose(interfaceFileDesc);              
            return (CS_FAIL);
        }
        (void)strcpy(*serverListPtr+(*serverCount),name);
        (*serverCount)++;

    }
    
    fclose(interfaceFileDesc);
    return (CS_SUCCEED);

}

