/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/* pps db library - jtg */

static char SccsFileId[] = "@(#)pps_db.c	1.4    10/31/97";

#include <stdio.h>
#include <ctpublic.h>
#include "pps_db.h"
#include "defs.h"
#include "PPSerr.h"
#include "PPSdefs.h"
 
#define RETURN_STATUS \
{ \
	return_status = error_severity; \
	ct_cmd_drop(command); \
	error_severity = ER_NO_ERROR; \
	if (return_status == ER_NO_ERROR) \
		return (ER_DB_NONFATAL); \
	else \
		return (return_status); \
}
 
extern PPSConfigStruct PPSConfigs[];

static CS_CONTEXT      *context;
static int		error_severity;

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

	/* determine the severity of the error */
	if (CS_SEVERITY(errmsg->msgnumber) < CS_SV_RESOURCE_FAIL)
		error_severity = ER_DB_NONFATAL;
	else
		error_severity = ER_DB_FATAL;
 
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

	error_severity = ER_DB_FATAL;
	
        return (CS_SUCCEED);
}

/*****************************************************************************/
CS_RETCODE db_set_database (CS_COMMAND *command)
{
	CS_RETCODE	retcode;
	int		exec_succeed = 1;
	char		usedb_cmd[35];
	CS_INT		result_type;

	sprintf(usedb_cmd, "use %s", 
		(const char*)PPSConfigs[PPS_DBNAME].value);
	
        /* fill in usedb command */
        retcode = ct_command(command, CS_LANG_CMD, usedb_cmd, CS_NULLTERM,
                CS_UNUSED);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_command() failed with %d\n", retcode);
                return(retcode);
        }
 
        /* send command */
        retcode = ct_send(command);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_send() failed with %d\n", retcode);
                return(retcode);
        }
 
        /* process results */
        while ((retcode = ct_results(command, &result_type)) == CS_SUCCEED) {
                switch (result_type) {
                case CS_CMD_FAIL:
                        fprintf(stderr, "failure in \"%s\"\n", usedb_cmd);
			exec_succeed = 0;
                        break;
                case CS_CMD_DONE:
                case CS_CMD_SUCCEED:
                        break;
                default:
                        fprintf(stderr, "unknown result_type %d in set_db \n",
                                result_type);
			exec_succeed = 0;
                        break;
                }
        }
        switch (retcode) {
	        case CS_END_RESULTS:
        	case CS_CANCELED:
               		 break;
	        case CS_FAIL:
			fprintf(stderr, "usedb fail\n");
			exec_succeed = 0;
       			break;
        	default:
               		fprintf(stderr, "unexpected final status %d\n", retcode);
			exec_succeed = 0;
       			break;
        }
	if (exec_succeed == 0)
                return (CS_FAIL);
        else
                return (CS_SUCCEED);
}

/*****************************************************************************/

CS_RETCODE db_init_context ()
{
	CS_RETCODE	retcode;

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

CS_RETCODE db_connect (CS_CONNECTION **connection)
{
	CS_RETCODE	retcode;
	static int      context_init = 0;

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
                return(retcode);
        }
 
        /* establish connection properties */
        retcode = ct_con_props(*connection, CS_SET, CS_USERNAME,
                 PPSConfigs[PPS_USERID].value, CS_NULLTERM, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_props(CS_USERNAME) failed with %d\n", retcode);
                return(retcode);
        }

        retcode = ct_con_props(*connection, CS_SET, CS_PASSWORD,
                PPSConfigs[PPS_PASSWD].value, CS_NULLTERM, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_props(CS_PASSWORD) failed with %d\n", retcode);
                return(retcode);
        }

        retcode = ct_con_props(*connection, CS_SET, CS_APPNAME,
                		"PPS_GUI", CS_NULLTERM, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_con_props(CS_APPNAME) failed with %d\n", retcode);
        }

        /* connect to the server */
        retcode = ct_connect(*connection, PPSConfigs[PPS_SERVER].value, 
				CS_NULLTERM);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_connect() failed with %d\n", retcode);
                return(retcode);
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
		retcode = db_connect(connection); 
		if (retcode != CS_SUCCEED)
		{
			fprintf(stderr, "Failed to make a new connection\n");
			return (retcode);
		}
        }
        return (CS_SUCCEED);
}

/***************************************************************************/

CS_RETCODE fetch_row (CS_COMMAND *command,
                      struct pps_db_exec_dcl *db_query,
					  int	 exec_callback)
{
	CS_RETCODE 	retcode;
	int		exec_succeed = 1;
	CS_INT		rows_read;
	CS_INT		row_count = 0;

        /* fetch the row(s) returning from SQL server */
	while (((retcode = ct_fetch(command, CS_UNUSED, CS_UNUSED,
		CS_UNUSED, &rows_read)) == CS_SUCCEED) ||
		(retcode == CS_ROW_FAIL))
	{
		row_count = row_count + rows_read;

		if (retcode == CS_SUCCEED)
		{
			if (db_query->callback && exec_callback)
				db_query->callback();
		}
		else
		{
			fprintf(stderr, "Error fetching row %d\n", row_count);
			exec_succeed = 0;
		}
	}
       
	switch (retcode) 
	{
		case CS_END_DATA:
			break;
		case CS_CANCELED:
			break;
		case CS_FAIL:
			exec_succeed = 0;
			break;
	}

	if (exec_succeed == 0)
		return (retcode);
	else
		return (CS_SUCCEED);
}

/******************************************************************************/


CS_RETCODE get_rows(CS_COMMAND *command, 
                    struct pps_db_exec_dcl *db_query)
{
        CS_RETCODE      retcode;
        CS_DATAFMT      datafmt[MAX_PPS_DB_ITEMS];
        CS_DATAFMT      myfmt;
        int             numcols, i;
 
        /* get the number of columns in a row */
        retcode = ct_res_info(command, CS_NUMDATA, &numcols, CS_UNUSED, NULL);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_res_info() failed with %d\n", retcode);
                return(retcode);
        }
 
        /* check for correct number of columns */
        if (numcols != db_query->num_items) {
                fprintf(stderr, "expected %d columns, got %d\n",
                        db_query->num_items, numcols);
                return(CS_FAIL);
        }
 
        /* get the column data types */
        for (i = 0; i < numcols; i++) {
                retcode = ct_describe(command, i+1, &datafmt[i]);
                if (retcode != CS_SUCCEED) {
                        fprintf(stderr, "ct_describe() failed with %d\n",
                                retcode);
                        return(retcode);
                }
        }
        /* verify that the types match what you expect */
        for (i = 0; i < numcols; i++) {
                if (datafmt[i].datatype !=
                                db_query->item[i].datafmt.datatype) {
                        fprintf(stderr, "data type mismatch on column %d ", i);
                        fprintf(stderr, "expected %d, got %d\n",
                                db_query->item[i].datafmt.datatype,
                                datafmt[i].datatype);
                        return(CS_FAIL);
                }
        }
 
        /* bind the columns to variables */
        for (i = 0; i < numcols; i++) {
                retcode = ct_bind(command, i+1,
                        &db_query->item[i].datafmt,
                        db_query->item[i].val, NULL,
                        &db_query->item[i].indicator);
                if (retcode != CS_SUCCEED) {
                        fprintf(stderr, "ct_bind() failed with %d\n", retcode);
                        return(retcode);
                }
        }
        retcode = fetch_row(command,db_query,TRUE);
        if (retcode != CS_SUCCEED)
                fprintf(stderr, "fetch_row failed in get_rows\n",retcode); 
 
        return (retcode);

}

/****************************************************************************/

CS_RETCODE get_return_status (CS_COMMAND *command,
			      struct pps_db_exec_dcl *db_query,
			      int	 *return_status)  /* output */
{
	CS_RETCODE	retcode;
        int             numcols;
        CS_DATAFMT      exec_result_datafmt;
	

	/* get the number of columns in a row */
	retcode = ct_res_info(command, CS_NUMDATA, &numcols, CS_UNUSED, NULL);
	if (retcode != CS_SUCCEED) 
	{
		fprintf(stderr, "ct_res_info() failed with %d\n", retcode);
		return(retcode);
	}
 
	/* check for correct number of columns */
	if (numcols != 1) 
	{
		fprintf(stderr, "expected 1 column, got %d\n", numcols);
		return(CS_FAIL);
	}
 
       	/* bind the column to an int */
	exec_result_datafmt.datatype = CS_INT_TYPE;
	exec_result_datafmt.format = CS_FMT_UNUSED;
	exec_result_datafmt.scale = CS_SRC_VALUE;
	exec_result_datafmt.precision = CS_SRC_VALUE;
	exec_result_datafmt.count = 1;
	exec_result_datafmt.locale = NULL;
        retcode = ct_bind(command, 1, &exec_result_datafmt,
			  &return_status, NULL, NULL);
	if (retcode != CS_SUCCEED) 
	{
		fprintf(stderr, "ct_bind() failed with %d\n", retcode);
		return(retcode);
	}
	retcode = fetch_row(command,db_query,FALSE);
	if (retcode != CS_SUCCEED)
	{
		fprintf(stderr, "fetch_row failed with retcode = %d in get_return_status\n", retcode);
	}

	return (retcode);
}

/******************************************************************************/

int db_exec(CS_CONNECTION          **connection,
	    char	           *cmd_string,
	    struct pps_db_exec_dcl *db_query)
{
        CS_RETCODE      retcode;
	CS_COMMAND 	*command;
	CS_INT		result_type;
	int		exec_result;
	int		exec_succeed = 1;
	int		return_status;

        /* check if the passed-in connection is still good, if not,
           drop it and allocate a new one which will get passed back
           to the caller */
	retcode = check_connection_status(connection);
	if (retcode != CS_SUCCEED)
		return (ER_DB_FATAL);

	/* allocate command structure */
        retcode = ct_cmd_alloc(*connection, &command);
	if (retcode != CS_SUCCEED)
	{
                fprintf(stderr, "ct_cmd_alloc() failed with %d\n", retcode);
		return (ER_DB_NONFATAL);
        }

	/* set database */
	if ((db_set_database(command)) != CS_SUCCEED)
		RETURN_STATUS;

        /* fill in command string to be sent to Sybase server */
        retcode = ct_command(command, CS_LANG_CMD, cmd_string, CS_NULLTERM,
                CS_UNUSED);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_command() failed with %d\n", retcode);
		RETURN_STATUS;			
        }
 
        /* send command */
        retcode = ct_send(command);
        if (retcode != CS_SUCCEED) {
                fprintf(stderr, "ct_send() failed with %d\n", retcode);
		RETURN_STATUS;
        }

        /* process results */
        while ((retcode = ct_results(command, &result_type)) == CS_SUCCEED) {
                switch (result_type) {

                case CS_ROW_RESULT:
                        if ((get_rows(command, db_query)) != CS_SUCCEED)
				exec_succeed = 0;
                        break;
		 
                case CS_STATUS_RESULT:
			/* the command is to execute a stored procedure which is
			   returning a status code */ 
			if ((get_return_status(command, db_query, &exec_result)) != CS_SUCCEED)
				exec_succeed = 0;
			break;

                case CS_CMD_FAIL:
                        fprintf(stderr, "failure in \"%s\"\n", cmd_string);
                        exec_succeed = 0;
                        break;
                case CS_CMD_DONE:
                        break;
                case CS_CMD_SUCCEED:
                        break;
                default:
                        fprintf(stderr, "unexpected result_type %d in db_exec end\n", result_type);
                        exec_succeed = 0;
                        break;
                }
        }
        switch (retcode) {
        case CS_END_RESULTS:
                break;
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
		RETURN_STATUS;
	}
        else
	{
		ct_cmd_drop(command);
                return (ER_NO_ERROR);
	}
}

/****************************************************************************/

CS_RETCODE db_disconnect (CS_CONNECTION *connection)
{
        (void)ct_close(connection,CS_FORCE_CLOSE);
        (void)ct_con_drop(connection);
        (void)ct_exit(context, CS_FORCE_EXIT);
        (void)cs_ctx_drop(context);
}

/*****************************************************************************/

/* append an int item to a pps_db_exec_dcl struct */
void pps_db_bind_int(struct pps_db_exec_dcl *db_query, 
                     int *p_int)
{
	db_query->item[db_query->num_items].datafmt.datatype = CS_INT_TYPE;
	db_query->item[db_query->num_items].datafmt.format = CS_FMT_UNUSED;
	db_query->item[db_query->num_items].datafmt.scale = CS_SRC_VALUE;
	db_query->item[db_query->num_items].datafmt.precision = CS_SRC_VALUE;
	db_query->item[db_query->num_items].datafmt.count = 1;
	db_query->item[db_query->num_items].datafmt.locale = NULL;
	db_query->item[db_query->num_items].val = p_int;
	db_query->num_items++;
}

/*****************************************************************************/

/* append a string item to a pps_db_exec_dcl struct */
void pps_db_bind_char(struct pps_db_exec_dcl *db_query, 
                      char *p_char, int maxchars)
{
	db_query->item[db_query->num_items].datafmt.datatype = CS_CHAR_TYPE;
	db_query->item[db_query->num_items].datafmt.format = CS_FMT_NULLTERM;
	db_query->item[db_query->num_items].datafmt.maxlength = maxchars;
	db_query->item[db_query->num_items].datafmt.count = 1;
	db_query->item[db_query->num_items].datafmt.locale = NULL;
	db_query->item[db_query->num_items].val = p_char;
	db_query->num_items++;
}

/*****************************************************************************/

/* append a date item to a pps_db_exec_dcl struct */
void pps_db_bind_date(struct pps_db_exec_dcl *db_query, CS_DATETIME *p_date)
{
	db_query->item[db_query->num_items].datafmt.datatype = CS_DATETIME_TYPE;
	db_query->item[db_query->num_items].datafmt.format = CS_FMT_UNUSED;
	db_query->item[db_query->num_items].datafmt.count = 1;
	db_query->item[db_query->num_items].datafmt.locale = NULL;
	db_query->item[db_query->num_items].val = p_date;
	db_query->num_items++;
}

/*****************************************************************************/

/* append a float item to a pps_db_exec_dcl struct */
void pps_db_bind_float(struct pps_db_exec_dcl *db_query, float *p_float)
{
	db_query->item[db_query->num_items].datafmt.datatype = CS_REAL_TYPE;
	db_query->item[db_query->num_items].datafmt.format = CS_FMT_UNUSED;
	db_query->item[db_query->num_items].datafmt.scale = CS_SRC_VALUE;
	db_query->item[db_query->num_items].datafmt.precision = CS_SRC_VALUE;
	db_query->item[db_query->num_items].datafmt.count = 1;
	db_query->item[db_query->num_items].datafmt.locale = NULL;
	db_query->item[db_query->num_items].val = p_float;
	db_query->num_items++;
}


