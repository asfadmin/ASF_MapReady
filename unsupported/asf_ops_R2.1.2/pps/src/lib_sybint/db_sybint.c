/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
/*==============================================================================
Filename:	db_sybint.c

Description:	

	This file contains functions that interface with Sybase via the
Open Client DB-Library.  Data to be stored/retrieved from the database
makes use of a linked list structure with the data records being the 
links.  See the dapp_list library for more on the linked list.

External Functions:

	free_db_record
	new_db_record
	new_table_record
	db_num_records
	db_make_colnames_string
	db_make_format_string
	db_get_column_data
	db_get_records
	db_insert_values
	db_insert_records
	db_print_record
	db_delete_records
	db_update_records
	db_nth_record
	db_read_records_from_ingres_file
	db_ftn_first_record
	db_ftn_next_record
	db_copy_record

	
Static Functions:
	int read_col_from_file(int fd, char *buf) 
	
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

static char SccsFileId[] = "@(#)db_sybint.c	1.5  10/31/97";

#include <varargs.h> 
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syberror.h>

#include "db_sybint.h"

#include "dapps_list.h"
#include "nmalloc.h"

#include "defs.h"
#include "PPSextern.h"

#include "messages.h"

#define	COLNAMES_PER_LINE 4
#define DEADLOCK_ERROR	  1205

/* 11/8/95 Nadia Adhami  Making this file thread safe
 *                       remove all global and static variables
 */

/*			
char where_clause[1024] ;
char orderby_cols[1024] ;
char fields_to_set[1024] ;

char format_string[1024] ;
char col_names[1024] ;
char col_values[1024] ;
char tables[256] ;

char aps_message[1024] ;

*/

/* leave this one global, have to use mutex lock when accessing it */
int llist_errno = 0 ;

/* information for sybase specific data */
/*
char sys_tables[] = "syscolumns c, sysobjects o" ;

COLUMN_DEFS sybsys_columns[] =
{
	{"c.name", 30, STRINGBIND, "%30s"},
	{"c.type", sizeof(DBINT), INTBIND, "%d"},
	{NULL, 0, 0, NULL}
} ;
*/

void db_print_command_buffer( DBPROCESS *dbproc ) ;


/*==============================================================================
Function:		trimstring

Description:	Used to trim leading and trailing characters from strings

Parameters:		char * - string to be trimmed

Returns:     	char * - pointer to modified string

Creator:		??? 

Creation Date:	11/03/1994

Notes:		
		Originally called  IC_trimString
		Borrowed from code on an IMS/DADs system
==============================================================================*/
char * trimstring(char *Str)
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
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
int db_default_message_handler(
	DBPROCESS   *dbproc,
	DBINT       msgno,
	int         msgstate,
	int         severity,
	char        *msgtext,
	char        *srvname,
	char        *procname,
	DBUSMALLINT line)
{
	char    logmsg[MAX_SYSLOG_MSGLEN+1];
	char	buf[MAX_SYSLOG_MSGLEN+1];

	/* Note: The message handler must return a value of 0 to DB-Lib 
         * why? that's the way DB-Lib works! */

        /* Only report errors with severity level greater than 10
         * (anything 10 and below is not an error) */
        
	if (severity <= 10) {
 		return (0);
	}

	(void)sprintf(logmsg, "Error message from SQL Server %s [Msg %ld, Level %d, State %d", srvname, msgno, severity, msgstate);

	if ( (int) strlen(procname) > 0)
	{
		(void)sprintf (buf,", Procedure '%s'", procname);
		strcat(logmsg, buf);
	}
	if (line > 0)
	{
		(void)sprintf(buf,", Line %d", line);
		strcat(logmsg,buf);
	}
 
	(void)sprintf(buf,"]: %s", msgtext);
 	strcat(logmsg,buf);
	pps_logMsg(ProgName, PPS_ERROR, logmsg);

	db_print_command_buffer(dbproc);

	/* If this is a deadlock error, set the deadlock indicator 
		for the application to resubmit command */
	if (msgno == DEADLOCK_ERROR)
	{
		*((DBBOOL *) dbgetuserdata(dbproc)) = TRUE;
	}
	return (0);
}

 

/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
INT_CANCEL will return FAIL from the DB-Library function that caused the error.
INT_EXIT will cause the program to terminate.


==============================================================================*/
int db_default_error_handler(
	DBPROCESS       *dbproc,
	int             severity,
	int             dberr,
	int             oserr,
	char            *dberrstr,
	char            *oserrstr       )
{
	char		logmsg[MAX_SYSLOG_MSGLEN+1];
	unsigned32      status;	

	pps_logMsg(ProgName,PPS_WARNING,"sybase_error_handler () invoked");

	if (dbproc == (DBPROCESS *) NULL || DBDEAD(dbproc))
	{
		pps_logMsg(ProgName,PPS_CRITICAL,"Connection to Sybase is dead");
		return (INT_CANCEL);
	}

        /* Only report errors that are not SQL Server's
         * errors because the Message Handler has already reported 
         * the SQL Server errors */
        if (dberr != SYBESMSG)
        {
		(void)sprintf(logmsg,"DB-Lib error (severity: %d, no: %d): %s", severity, dberr, dberrstr);
                 pps_logMsg(ProgName,PPS_ERROR,logmsg);
 
                /* If Sybase has encountered a system error, report it */
                if (oserr != DBNOERR)
                {
                       (void)sprintf(logmsg,"Sybase OS error (severity: %d, no: %d): %s", severity, oserr, oserrstr);
                       pps_logMsg(ProgName,PPS_ERROR,logmsg);
                }
	        db_print_command_buffer(dbproc);
	}

	/*
	-- INT_CANCEL will return FAIL from the 
	-- DB-Library function that caused the eror 
	*/
	return(INT_CANCEL) ;
}



/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
void db_print_command_buffer( DBPROCESS *dbproc )
{
	int		j, len;
	char		line[81];
	RETCODE         return_code;
	char		logmsg[MAX_SYSLOG_MSGLEN+1];

	if (dbproc == (DBPROCESS *) NULL)
	{
		pps_logMsg(ProgName,PPS_WARNING,"db_print_command_buffer invoked with NULL dbproc");
	}
	len = dbstrlen(dbproc);
	if(len <= 0)
		strcpy(logmsg,"SQL Server command buffer was empty");
	else
	{
		strcpy(logmsg,"SQL Server command buffer contents: ");
		j = 0;
		while ( j*80 < len && j*80 < MAX_SYSLOG_MSGLEN - 40)
		{
			return_code = dbstrcpy(dbproc, 80*j++, 80, line);
			strcat(logmsg,line);
		}
	}
	pps_logMsg(ProgName, PPS_DEBUG, logmsg);
	return;
}


/**********************************************************************
*  Name: db_open_errs.c
*
*  Module Type:	Subroutine	Language: C 
*
*  ......./bld/mps/lib/src/opendb_errs.c  
*
*  Purpose:  prints message explaining error code from opendb.c
*
*  Input Parameters:
*  Name				Type	Description
*  error_code		int		error code from opendb.c
*
*  Output Parameters:
*  Name				Type		Description
*
*  Return Parameter:
*  Type			Description
*
******************************************************************************
*  Usage example:  	
*
*		#include "db_sybint.h"
*		.
*		.
*		.
*		extern	DBPROCESS* db_open(char*, char*, char*, char*,
*			int (*msg_handler)(), int (*err_handler)(), int*)
*		DBPROCESS	*dbproc;
*		int			err_code;
*		int			m_handler();
*		int			e_handler();
*
*		dbproc = db_open("oprmpsdb", "create_nominal_coverage",
*				"quasimodo", "e$miralda", m_handler, e_handler, &err_code);
*		if(err_code != DB_OPEN_OK)
*		{
*			/* db_open failed */											/*
*			db_open_errs(errcode, "oprmpsdb", "quasimodo", err_code);
*			.
*			.
*			.
*		}
******************************************************************************
*
*  Modification History:
*  Author	Revision	Date
*
****************************************************************************/
int db_open_errs(int error_code, char *dbname, char *sybase_userid)
{
	char	logmsg[MAX_SYSLOG_MSGLEN+1] ;	

	switch(error_code)
	{
		case DB_OPEN_OK:
			pps_logMsg(ProgName, PPS_INFO,"DB WAS SUCCESSFULLY OPENED.");
			break;

		case DB_OPEN_ERR_DBNAME:
			pps_logMsg(ProgName, PPS_ERROR, "Database name not given. Check any database environment variables");
			break;

		case DB_OPEN_ERR_PROG:
			pps_logMsg(ProgName, PPS_ERROR,"Program name not given.  Programmer should check for a NULL string.");
			break;

		case DB_OPEN_ERR_USER:
			pps_logMsg(ProgName, PPS_ERROR, "Sybase userid not given-- Check PPS_USERID environment variable.");
			break;

		case DB_OPEN_ERR_PASSWD:
                        pps_logMsg(ProgName, PPS_ERROR, "Sybase password not given --  Check PPS_PASSWD environment variable.");
                        break;

		case DB_OPEN_ERR_DBINIT:
			pps_logMsg(ProgName, PPS_ERROR,"ERROR in initializing Sybase session.  Sybase server might be down. ");
			break;

		case DB_OPEN_ERR_DBLOGIN:
			pps_logMsg(ProgName, PPS_ERROR, "ERROR in Sybase session login. Sybase server might be down. Possibly the server is down.");
			break;

		case DB_OPEN_ERR_DBOPEN:
			pps_logMsg(ProgName, PPS_ERROR,	"ERROR in DB-Library dbopen call. Probably userid or password is incorrect. Possibly the server is down");
			if (sybase_userid != NULL)
			{
				(void)sprintf(logmsg,"Userid used = %s", sybase_userid) ;
				pps_logMsg(ProgName,PPS_ERROR,logmsg);
			}
			break;

		case DB_OPEN_ERR_DBUSE:
			pps_logMsg(ProgName, PPS_ERROR,"ERROR in accessing database. Probably database does not exist.") ;

			if (dbname != NULL)
			{
				(void)sprintf(logmsg,"Database name used = %s", dbname) ;
				pps_logMsg(ProgName,PPS_ERROR,logmsg);
			}
			break ;

		case DB_OPEN_ERR_OPEN_COMMIT:
			pps_logMsg(ProgName,PPS_ERROR,"ERROR in DB-Library open_commit call.");
			break ;

		default :
			(void)sprintf(logmsg,"UNKNOWN ERROR in opening database.  Code %d occurred when opening the database.", error_code) ;
			pps_logMsg(ProgName,PPS_ERROR,logmsg);
	}
}


/**********************************************************************
*  Name: OpenDB
*
*  Module Type:	Subroutine	Language: C with calls to Sybase DBLIB
*
*  AOS_BLD/bld/mps/lib/src/opendb.c  
*
*  Purpose: 
*  Starts a Sybase session via login.  Also opens the database 
*  identified by an environment variable.   if there is any problem, 
*  this routine stops the run.  
*
*  Input Parameters:
*  Name				Type	Description
*  dbname			*char	database name to open.  
*  program			*char	name of the calling program.
*  sybase_userid	*char	Sybase-approved userid.
*  password			*char	password for userid.  
*  msg_handler		int(*function)()	address of message handling function,
*										or NULL to use default from this source
*										file.  
*  err_handler		int(*function)()	address of error handling function,
*										or NULL to use default from this source
*										file.  
*
*  Output Parameters:
*  Name				Type		Description
*  error_code		*int
*					these are the error codes returned from dbopen.c
*					DB_OPEN_OK	0
*					ERR_DBNAME	1
*					ERR_PROG	2
*					ERR_USER	3
*					ERR_PASSWD	4
*					ERR_DBINIT	5
*									error from calling dbinit(); probably the 
*									server is down.  
*					ERR_DBLOGIN	6
*									error from calling dblogin(); probably the 
*									server is down.  
*					ERR_DBOPEN	7
*									probably a bad userid or password. 	
*					ERR_DBUSE	8
*									probably a bad dbname
*
*
*  Return Parameter:
*  Type			Description
*  DBPROCESS*	if no errors (error_code = DB_OPEN_OK):
*				the return value is the pointer to the DBPROCESS structure
*				that provides the connection for the process.  
*				this pointer is used in all subsequent db activity for 
*				this db session.
*				if there were any errors (error_code != DB_OPEN_OK), NULL 
*				is returned.  
*
******************************************************************************
*  Usage example:  	
*
*		#include "opendb.h"
*		.
*		.
*		.
*		extern	DBPROCESS* opendb( char*, char*, char*, char*,
*			int (*msg_handler)(), int (*err_handler)(), int*)
*		DBPROCESS	*dbproc;
*		int			err_code;
*		int			m_handler();
*		int			e_handler();
*
*		dbproc = opendb("oprmpsdb", "create_nominal_coverage",
*				"quasimodo", "e$miralda", m_handler, e_handler, &err_code);
*		if(err_code != DB_OPEN_OK)
*		{
*			/* opendb failed */											/*
*			.
*			.
*			.
*		}
******************************************************************************
*
*  Modification History:
*  Author	Revision	Date
*
****************************************************************************/

db_open_commit(
	LOGINREC	*login,
	char		*server_name,
	int		*error_code,
	DBPROCESS       **dbproc_commit_ptr
	)

{
	DBPROCESS	*dbproc_commit;

	/* open connection with the commit service */ 
	/* dbproc_commit is used in subsequent calls to commit service */
	dbproc_commit = open_commit(login, server_name) ;
 
	if (dbproc_commit == NULL)
	{
		*error_code = DB_OPEN_ERR_OPEN_COMMIT ;
		return NULL ;
	}

	/* everything is ok, return the new dbproc */
	*dbproc_commit_ptr = dbproc_commit;

}
	
DBPROCESS* db_open(
	char	*dbname,
	char	*program,
	char	*sybase_userid,
	char	*password,
	char	*server_name,
	int		(*msg_handler)(),
	int		(*err_handler)(),
	int		*error_code,
	LOGINREC	**loginrec
	)
	
{
	DBPROCESS	*dbproc;
	LOGINREC	*login;
	RETCODE		return_code ;
	int		rcode ;
	char		program_substring[30];
	char		*str=0;

	*error_code = 0 ;

	if (dbname == NULL || strlen(dbname) == 0)
	{
		*error_code = DB_OPEN_ERR_DBNAME ;
		return NULL ;
	}

	if (program == NULL || strlen(program) == 0)
	{
		*error_code = DB_OPEN_ERR_PROG ;
		return NULL ;
	}

	/*--------------------------------------------*/
	/* strip the path, and truncate the program   */
	/* name to 30 chars                           */
	/*--------------------------------------------*/
	if ((str=strrchr(program, '/')) == NULL)
		str = program;
	else
		str++;
	(void)strncpy(program_substring, str, 29);
	program_substring[29] = '\0';

	if (sybase_userid == NULL || strlen(sybase_userid) == 0)
	{
		*error_code = DB_OPEN_ERR_USER ;
		return NULL ;
	}

	if (password == NULL || strlen(password) == 0)
	{
		*error_code = DB_OPEN_ERR_PASSWD ;
		return NULL ;
	}

	if (dbname == NULL || strlen(dbname) == 0)
	{
		*error_code = DB_OPEN_ERR_DBNAME ;
		return NULL ;
	}

	if (dbname == NULL || strlen(dbname) == 0)
	{
		*error_code = DB_OPEN_ERR_DBNAME ;
		return NULL ;
	}

	if (dbinit() == FAIL)
	{
		return_code = DB_OPEN_ERR_DBINIT ;
		return NULL ;
	}

	/* Install the MESSAGE HANDLER */
	if (msg_handler == NULL)
		dbmsghandle((MHANDLEFUNC)db_default_message_handler) ;  /* default */
	else
		dbmsghandle((MHANDLEFUNC)msg_handler) ; /* caller-supplied */
 
	/* Install the ERROR HANDLER */
	if (err_handler == NULL)
		dberrhandle((EHANDLEFUNC)db_default_error_handler) ;  /* default */
	else
		dberrhandle((EHANDLEFUNC)err_handler) ; /* caller-supplied */
 
	login = dblogin();
	if (login == NULL)
	{
		*error_code = DB_OPEN_ERR_DBLOGIN ;
		return NULL ;
	}

	*loginrec = login;
 
	DBSETLAPP(login, program_substring) ;
	DBSETLPWD(login, password) ;
	DBSETLUSER(login, sybase_userid) ;

	/* open connection with the server */ 
	/* one dbopen() call per server */
	dbproc = dbopen(login, server_name) ;

	if (dbproc == NULL)
	{
		*error_code = DB_OPEN_ERR_DBOPEN ;
		return NULL ;
	}

	return_code = dbuse(dbproc, dbname) ;
	if (return_code == FAIL)
	{
		*error_code = DB_OPEN_ERR_DBUSE ;
		return NULL ;
	}

	/* Allocate a pointer to a DBBOOL variable which will be  set by
		the message handler when deadlock occurs */ 
	dbsetuserdata (dbproc, malloc(sizeof(DBBOOL)));

	return (dbproc) ;
}



/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Larry Stevens

Creation Date:	11/dd/1994

Notes:		
==============================================================================*/
/**********************************************************************
*  Name: error_handler_exit
*
*  Module Type:	Subroutine	Language: C with calls to Sybase DBLIB
*
*  Purpose: 
*  Prints out Sybase error messages, the SQL server command buffer, a
*  banner ERROR, and then EXITS the process.  
**********************************************************************/
int error_handler_exit(
	DBPROCESS       *dbproc,
	int             severity,
	int             dberr,
	int             oserr,
	char            *dberrstr,
	char            *oserrstr       )
{
	int rcode ;
	char separater_str[80] ;
	int separater_len = 80 ;

 
	printf("\n\nSybase error handler:\n") ;
	memset(separater_str, '#', separater_len - 1) ;
	printf("%s\n", separater_str) ;

	if ((dbproc == NULL) || (DBDEAD(dbproc)))
		printf("The database process (DBPROC) is dead.\n");
	else
	{
		printf("DB-Library error:\n\t%s\n", dberrstr);
		if (oserr != DBNOERR)
			printf("Operating-system error:\n\t%s\n", oserrstr);
		else
		{
			db_print_command_buffer(dbproc);
			memset(separater_str, '-', separater_len - 1) ;
			printf("%s\n\n", separater_str) ;
		}
	}

	printf("Terminating the run due to database errors.\n");

	fflush(stdout);

	rcode = system("date;banner ERROR");

	/* this will cause the SQL server to print a message and abort the	*/
	/* program with an error condition and without a core file.  		*/
	return(INT_EXIT) ;
}



/*==============================================================================
Function:		free_db_record

Description:	
	Free the memory associated with a db record.  This function is primarily 
passed as the delete function for the link list.

Parameters:		DB_RECORD **

Returns:     	void

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
void free_db_record(DB_RECORD **recordptr)
{
	int i ;

	/* free each column in the databse record */
	for (i = 0 ; i < MAXITEMS ; i++)
	{
		if (recordptr[i])
			FREE(recordptr[i]) ;
	}

	/* now free the pointer to the columns */
	FREE(recordptr) ;
}



/*==============================================================================
Function:		new_db_record

Description:	
	Allocates and initializes memory to hold data for a DB_RECORD
	DB_RECORD is an array of void pointers which are initialized to NULL

Parameters:		None

Returns:     	DB_RECORD **

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
DB_RECORD** new_db_record() 
{
	DB_RECORD **recordptr ;
	int i ;
	
	recordptr = (DB_RECORD **) ZNEW(sizeof(DB_RECORD)) ;
	return(recordptr) ;
}



/*==============================================================================
Function:		new_table_record

Description:	creates a DB_RECORD with space allocated for each column of the table

Parameters:		COLUMN_DEFS *columns   column table definition

Returns:     	DB_RECORD **

Creator:		Ron Green

Creation Date:	11/03/1994

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
Function:		db_num_records

Description:	
	Returns the number of records that match a given where_clause

Parameters:		
	DBPROCESS *dbproc    
	char *table
	char *where_clause

Returns:     	int 
	Number of rows
	-1 if error 

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
	SAMPLE: db_num_records(dbproc, "table name", "where colname = 10") ;
==============================================================================*/
int db_num_records(DBPROCESS *dbproc, char *table, char *where_clause) 
{
	RETCODE return_code ;

	int numrows ;

	char *select_count_phrase = "select count(*) from" ;
	char *stmt ;

	if (!where_clause)
		where_clause = " " ;
    else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
		where_clause = " " ;

	stmt = ZNEW(
		strlen(select_count_phrase)
		+ strlen(table)
		+ strlen(where_clause)
		+ 5) ;  /* extra for spaces and null */

	sprintf(stmt, "%s %s %s", 
		select_count_phrase, table, where_clause) ;

#ifdef DEBUG
	printf("COUNT QUERY: %s\n\n", stmt) ;
#endif

	dbcmd(dbproc, stmt) ;
	dbsqlexec(dbproc) ;

	while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
	{
		if (return_code == SUCCEED)
		{
			dbbind(dbproc, 1, INTBIND, (DBINT) 0, (BYTE *)&numrows) ;
			dbnextrow(dbproc) ;

			printf("RECORD COUNT: %d\n\n", numrows) ;
			return(numrows) ;
		}
	}
	return(-1) ;
}



/*==============================================================================
Function:		db_max_value, db_min_value

Description:	
	Returns the minimum and maximum values of a column in a table

Parameters:		
	DBPROCESS *dbproc    
	char *table
	char *column_name

Returns:
		1	successful return
		-1 	unsuccessful return

Creator:	Nadia Adhami

Creation Date:	7/15/1995

Notes:		
	SAMPLE: db_max_value(dbproc, "table name", "colname", 
			where_clause, &max_value) ;
==============================================================================*/
int db_max_value(DBPROCESS *dbproc, 
	char *table, char *colname, char *where_clause, int *max_value) 
{
	RETCODE return_code ;

	char *select_clause = "select" ;	
	char *max = "max" ;
	char *from = "from" ;
	
	char *stmt ;

	if (!where_clause)
		where_clause = " " ;
	else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
		where_clause = " " ;

	stmt = ZNEW(
		strlen(select_clause)
		+ strlen(max) + 2  /* add two for parens */
		+ strlen(colname) * 2  /* use the column name twice */
		+ strlen(from) + 2  /* add two for parens */
		+ strlen(table)
		+ strlen(where_clause)
		+ 8) ;  /* extra for spaces and null */

	sprintf(stmt, "%s %s(%s) %s %s %s", 
		select_clause, max, colname, from, table, where_clause) ;

	dbcmd(dbproc, stmt) ;
	dbsqlexec(dbproc) ;

	while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
	{
		if (return_code == SUCCEED)
		{
			dbbind(dbproc, 1, INTBIND, (DBINT) 0, (BYTE *)max_value) ;
			dbnextrow(dbproc) ;
			return(1) ;
		}
	}
	FREE (stmt);
	return(-1) ;
}

int db_min_value(DBPROCESS *dbproc, 
	char *table, char *colname, char *where_clause, int *min_value) 
{
	RETCODE return_code ;

	char *select_clause = "select" ;	
	char *min = "min" ;
	char *from = "from" ;
	
	char *stmt ;

	if (!where_clause)
		where_clause = " " ;
	else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
		where_clause = " " ;

	stmt = ZNEW(
		strlen(select_clause)
		+ strlen(min) + 2  /* add two for parens */
		+ strlen(colname) * 2  /* use the column name twice */
		+ strlen(from) + 2  /* add two for parens */
		+ strlen(table)
		+ strlen(where_clause)
		+ 8) ;  /* extra for spaces and null */

	sprintf(stmt, "%s %s(%s) %s %s %s", 
		select_clause, min, colname, from, table, where_clause) ;

	dbcmd(dbproc, stmt) ;
	dbsqlexec(dbproc) ;

	while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
	{
		if (return_code == SUCCEED)
		{
			dbbind(dbproc, 1, INTBIND, (DBINT) 0, (BYTE *)min_value) ;
			dbnextrow(dbproc) ;
			return(1) ;
		}
	}
	FREE (stmt);
	return(-1) ;
}


/*==============================================================================
Function:		db_make_colnames_string

Description:	Creates a string of the column names of a given table
	Used mainly for queries to specify the ordering of columns to be returned

Parameters:		
	char *table            table to make the column names for
	COLUMN_DEFS *columns   column table definition
	char *col_names        allocated string to contain the column names

Returns:     	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
	This function should be modified to read the names out of the database,
rather than the static global COLUMN_DEF table.
==============================================================================*/
void db_make_colnames_string(char *table, COLUMN_DEFS *columns, char *col_names)
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
Function:		db_make_format_string

Description:	
	Make a format descriptor string for columns in a table.  The string
can then be used to print the values of the data stored in a DB_RECORD

Parameters:		
	char *table            table to make the column names for
	COLUMN_DEFS *columns   column table definition
	char *format_string    allocated string to contain the format specification

Returns:     	

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
==============================================================================*/
void db_make_format_string(
	char *table, COLUMN_DEFS *columns, char *format_string)
{
	int index = 0 ;
	char *cptr ;

	sprintf(format_string, "%s", " ") ;
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
Function:		db_get_column_data

Description:	
	This function is used to get specific information about columns
in a sybase table. 

Parameters:		
	char *table_name 

Returns:     	llist*  of the records retrieved
	The records contain information contained in table sybinfo
	name and type

Creator:		Ron Green

Creation Date:	09/30/1994

Notes:		
==============================================================================*/
llist *db_get_column_data(DBPROCESS *dbproc, char *table_name) 
{
	llist *sybdata_records ;
	char where_clause[1024] ;
	char sys_tables[] = "syscolumns c, sysobjects o" ;
	COLUMN_DEFS sybsys_columns[] =
	{
		{"c.name", 30, STRINGBIND, "%30s"},
		{"c.type", sizeof(DBINT), INTBIND, "%d"},
		{NULL, 0, 0, NULL}
	} ;

	sprintf(where_clause, "where o.name = '%s' and c.id = o.id", table_name) ;

	sybdata_records = db_get_records(dbproc, sys_tables, 
		where_clause, NULL, sybsys_columns, ALL_COLS) ;
 
	return(sybdata_records) ;
}


/*==============================================================================
Function:		db_get_records

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


Returns:     	llist*  of the records retrieved

Creator:		Ron Green

Creation Date:	09/dd/1994

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
	va_list vlist_p ;
	RETCODE return_code ;

	DB_RECORD tmp_record ;
	DB_RECORD **recordptr;
	llist *llrecords ;
	cursor ptr ;

	int i ;
	int col_id ;
	int colnames_len ;

	char *select_clause = "select" ;
	char *from = "from" ;
	char *order_by_phrase = "order by" ;
	char *order_by ;
	char *stmt ;
	char *colnames ;
	char *cptr ;
	char logmsg[MAX_SYSLOG_MSGLEN+1];
	char tmpmsg[5000];

	if (dbproc == (DBPROCESS *) NULL)
	{
		pps_logMsg(ProgName,PPS_ERROR,"db_get_records() invoked with NULL dbproc");
		return NULL;
	}
	
        /* cancel all pending results if any (unless dbproc is dead) */
        if (DBDEAD(dbproc) == FALSE)
        {
                if (dbcancel(dbproc) == FAIL)
                {
                        pps_logMsg(ProgName,PPS_ERROR,"dbcancel() failed in db_get_records -- The PPS connection to Sybase may be dead");
                        return NULL;
                }
        }
	else
	{
		pps_logMsg(ProgName,PPS_ERROR,"db_get_records() failed because the PPS connection to Sybase is dead");
		return NULL;
	}
		

	/* first get space to hold all field names if necessary */
	i = 0 ;
	colnames_len = 1 ;
	while (columns[i].name)
	{
		colnames_len =  
			colnames_len + strlen(columns[i].name) + 2 ;  /* add 2 for ', ' */
		i++ ;
		
		if (i % COLNAMES_PER_LINE == 0)
			colnames_len = colnames_len + 1 ;  /* add for newline */
	}

	colnames = (char *) ZNEW(colnames_len+1) ; /* add for NULL */

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
		db_make_colnames_string(table, columns, colnames) ;
	}
	va_end(vlist_p) ;

	
	if (!where_clause)
		where_clause = " " ;
	else if (strlen(where_clause) == 0) /* for FORTRAN NULL strings */
		where_clause = " " ;
	else /* a real where clause, trim any excess tabs, control chars, etc.. */
		where_clause = trimstring(where_clause) ;

	if (!fields_to_order)
	{
		order_by = " " ;
		fields_to_order = " " ;
	}
	else if (strlen(fields_to_order) == 0) /* for FORTRAN NULL strings */
	{
		order_by = " " ;
		fields_to_order = " " ;
	}
	else
		order_by = order_by_phrase ;

	stmt = (char *) ZNEW(
		strlen(select_clause) 
		+ colnames_len + 1
		+ strlen(from) + 1
		+ strlen(table) + 1
		+ strlen(where_clause) + 1
		+ strlen(order_by) + 1
		+ strlen(fields_to_order) + 1
		+ 7) ; /* extra for added blanks between arguments */

	sprintf(stmt, "%s %s\n%s %s\n%s\n%s %s", 
		select_clause, colnames, from, table, 
		where_clause, order_by, fields_to_order) ;


#ifdef DEBUG
	(void)fprintf(stderr,"QUERY: %s", stmt) ;
#endif

	/* Now get the records from the database */
	llrecords = create_dyn_llist() ;

	dbcmd(dbproc, stmt) ;

	/* Reset deadlock indicator */
	*((DBBOOL *) dbgetuserdata(dbproc)) = FALSE;

	if (dbsqlexec(dbproc) == FAIL)
	{
		pps_logMsg(ProgName,PPS_ERROR,"dbsqlexec() failed in db_get_records");
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
#ifdef DEBUG2	
					(void)sprintf(logmsg,"NAME: %s %d %d",
				        	columns[col_id].name, col_id, 
						columns[col_id].size) ;
					pps_logMsg(ProgName, PPS_DEBUG, logmsg);
#endif
					tmp_record[col_id] = (void *) ZNEW(columns[col_id].size) ;
					dbbind(dbproc, i+1, columns[col_id].bindtype, 
							(DBINT) 0, (BYTE *)tmp_record[col_id]) ;
					col_id = va_arg(vlist_p, int) ;
					i++ ;
				}
			}
			else /* allocate and bind all columns */ 
			{
				i = 0 ;
				while (columns[i].size)
				{
#ifdef DEBUG2
                                        (void)sprintf(logmsg,"NAME: %s %d %d",
                                                columns[col_id].name, col_id, 
                                                columns[col_id].size) ;
                                        pps_logMsg(ProgName, PPS_DEBUG, logmsg);
#endif
					tmp_record[i] = (void *) ZNEW(columns[i].size) ;
					dbbind(dbproc, i+1, columns[i].bindtype, 
						(DBINT) 0, (BYTE *)tmp_record[i]) ;
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
					 	recordptr[col_id] = (void *)ZNEW(columns[col_id].size);
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
					 	recordptr[i] = (void *) ZNEW(columns[i].size) ; 
						memcpy(recordptr[i], tmp_record[i], columns[i].size) ;
#ifdef DEBUG
						switch(columns[i].bindtype)
						{
							case INTBIND:
							{
								int temp1, temp2;
								memcpy((void*)&temp1, tmp_record[i], sizeof(int));
								memcpy((void*)&temp2, recordptr[i], sizeof(int));
								printf("db_get_records: %s from [%d] to [%d]\n",
								columns[i].name, temp1, temp2);
								break;
							}
							case REALBIND:
							{
								float temp1, temp2;
								memcpy((void*)&temp1, tmp_record[i], sizeof(float));
								memcpy((void*)&temp2, recordptr[i], sizeof(float));
								printf("db_get_records: %s from [%f] to [%f]\n",
								columns[i].name, temp1, temp2);
								break;
							}
							case STRINGBIND:
								printf("db_get_records: %s from [%s] to [%s]\n",
								columns[i].name, tmp_record[i], recordptr[i]);
								break;
							default:
								break;
						}
#endif
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
				i = 0 ;	/* use the field size to as a counter; check all cols */
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
#ifdef DEBUG2
	(void)sprintf(logmsg,"NUMBER OF RECORDS: %d", NUMELTS(llrecords)) ;
	pps_logMsg(ProgName,PPS_DEBUG,logmsg);
#endif
	return(llrecords) ;
}



/*==============================================================================
Function:		db_insert_values

Description:	
	Adds one record to a table... the column names and values are
passed in char format

Parameters:		
	DBPROCESS *dbproc    
	char *table         - the table to add the values to
	char *column_name   - a list of comma separated names
	char *column_values - a list of comma separated values

Returns:     	int 
	 1 record added
	-1 record not added

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
	SAMPLE: db_insert_values(dbproc, 
		"table name", "colname1, colname2,...", "val1, val2,...") ;
==============================================================================*/
int db_insert_values(DBPROCESS *dbproc, char *table, char *column_names, char *column_values)
{
	int nrecs ;

	char *stmt = NULL ;
	char *insert = "insert into" ;
	char *values = "values" ;

	stmt = (char *) ZNEW(
		strlen(insert) 
		+ strlen(table) 
		+ strlen(values) 
		+ strlen(column_names) 
		+ strlen(column_values) 
		+ 9) ; /* extra for added blanks (3) and parentheses (4) */

	sprintf(stmt, "%s %s (%s) %s (%s)", 
		insert, table, column_names, values, column_values) ;

	printf("INSERT STMT:\n %s\n", stmt) ;

	dbcmd(dbproc, stmt) ;
	dbsqlexec(dbproc) ;
	dbresults(dbproc) ;

	FREE(stmt) ;

	nrecs = DBCOUNT(dbproc) ;
	return(nrecs) ;
}

/*==============================================================================
Function:	db_exec_cmd

Description:	Execute a single cmd and get the return value (if any) 
		and number of records affected by the operation (if any).

Parameters:		
	DBPROCESS 	*dbproc    
	char		*cmdbuf

Returns:     		none 

Creator:		Nadia Adhami

Creation Date:		7/15/95

Notes:		
	SAMPLE: ret = db_exec_cmd(dbproc, cmdbuf, ret, nrecs)
==============================================================================*/
void db_exec_cmd(DBPROCESS *dbproc, char *cmdbuf, int *ret_value, int *nrecs)
{
	char logmsg[MAX_SYSLOG_MSGLEN+1];
	RETCODE	ret;

	if (dbproc == (DBPROCESS *) NULL)
	{
		pps_logMsg(ProgName,PPS_ERROR,"db_exec_cmd() invoked with NULL dbproc");
		return;
	}

	if (!cmdbuf || (cmdbuf[0] == '\0'))
	{
		pps_logMsg(ProgName,PPS_ERROR, "db_exec_cmd() cmdbuf is null");
		return ;
	}
#ifdef DEBUG
	(void)sprintf(logmsg,"db_exec_cmd( %s )", cmdbuf);
	pps_logMsg(ProgName,PPS_DEBUG,logmsg);
#endif

	/* cancel all pending results if any (unless dbproc is dead) */
	if (DBDEAD(dbproc) == FALSE)
	{
		if (dbcancel(dbproc) == FAIL)
		{
			pps_logMsg(ProgName,PPS_ERROR,"dbcancel() failed in db_exec_cmd -- The PPS connection to Sybase may be dead");
			return;
		}
	}
	else
	{
		pps_logMsg(ProgName,PPS_ERROR,"db_exec_cmd() failed because the PPS connection to Sybase is dead");
		return;
	}

	if (dbcmd(dbproc, cmdbuf) == FAIL)
	{
		pps_logMsg(ProgName,PPS_ERROR,"Failed to dbcmd()");
		return;
	}

	/* Initialize return values */
	*ret_value = -100;
	*nrecs = 0;

        /* Initialize the deadlock indicator */
        *((DBBOOL *) dbgetuserdata(dbproc)) = FALSE; 

	if (dbsqlexec(dbproc) == FAIL)
	{
		pps_logMsg(ProgName,PPS_ERROR,"Failed to dbsqlexec()");
		return;
	}
	if (dbresults(dbproc) == FAIL)
	{
		pps_logMsg(ProgName,PPS_ERROR,"Failed to get dbresults()\n");
                return;
	}
	/*
        ** If there is a return status from a Sybase stored procedure, 
	** save the value in ret_value. 
        ** For operations such as insert, delete, update,
	** save the number of records affected in nrecs. 
        */
        if (dbhasretstat (dbproc))
                *ret_value = (int) dbretstatus (dbproc);
	else
                *ret_value = 0;
	*nrecs = DBCOUNT(dbproc);
}



/*==============================================================================
Function:		db_print_record

Description:	
	Print  the contents of a DB_RECORD 

Parameters:		
	DBPROCESS *dbproc    
	DB_RECORD **record record to print 
	COLUMN_DEFS    - column definitions for the table 

Returns:     	void

Creator:		Ron Green

Creation Date:	11/03/1994

Notes:		
	SAMPLE: db_print_record(record, columntable) ;
==============================================================================*/
void db_print_record(DB_RECORD **record, COLUMN_DEFS *columns)
{
	int index = 0 ;
	char aps_message[1024] ;

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
				printf("%d\n", *(DBINT *) record[index]) ;
				break ;

			case REALBIND :
				printf("%f\n", *(DBREAL *) record[index]) ;
				break ;

			case FLT8BIND :
				printf("%f\n", *(DBFLT8 *) record[index]) ;
				break ;

			default :
				sprintf(aps_message, 
					"NO BINDTYPE; Using string format: '%s'\n", record[index]) ;
				printf(aps_message);
				break ;

			} /* switch */
			index++ ;
		}
	printf("END OF RECORD\n") ;
}



/*==============================================================================
Function:		db_insert_records

Description:	
	Insert records stored as a link list into a table

Parameters:		
	DBPROCESS *dbproc    
	llist *records - records to be added to the table
	char *table    - table to add the records to
	COLUMN_DEFS    - column definitions for the table 

Returns:     	int  number of records actually added

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
	SAMPLE: db_insert_records(dbproc, llist, "table name", columntable") ;
==============================================================================*/
int db_insert_records(DBPROCESS *dbproc, llist *records, char *table, COLUMN_DEFS *columns)
{
	DB_RECORD** record ;
	cursor ptr ;

	int index ;
	int added = 0 ;

	char *stmt = NULL ;
	char *cptr ;
	char valuestr[4096] ;
	char col_names[1024] ;

	char *insert = "insert into" ;
	char *values = "values" ;
	char aps_message[1024] ;

	/* form the colnames for which to write the values with */
	db_make_colnames_string(table, columns, col_names) ;

	record = FIRST(records, ptr) ;
	while (record)
	{
		index = 0 ;
		sprintf(valuestr, " ") ;
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
				sprintf(valuestr, "%s'%c', ", valuestr, 
					*(DBCHAR *) record[index]) ;
				break ;

			case STRINGBIND :   /* string pointers don't need cast */
			case NTBSTRINGBIND :
				sprintf(valuestr, "%s'%s', ", valuestr, 
					(char *) record[index]) ;
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
				sprintf(valuestr, "%s%d, ", valuestr, 
					*(DBINT *) record[index]) ;
				break ;

			case REALBIND :
				sprintf(valuestr, "%s%f, ", valuestr, 
					*(DBREAL *) record[index]) ;
				break ;

			case FLT8BIND :
				sprintf(valuestr, "%s%f, ", valuestr, 
					*(DBFLT8 *) record[index]) ;
				break ;

			default : ;
				sprintf(aps_message, 
					"BINDTYPE for item %s not given...\nUsing string format",
					columns[index].name) ;
				printf(aps_message);
				sprintf(valuestr, "%s'%s', ", valuestr, record[index]) ;

			} /* switch */
			index++ ;
		}

		cptr = strrchr(valuestr, ',') ;
		*cptr = NULL ; /* place a NULL at the last comma */
	
		stmt = (char *) ZNEW(
			strlen(insert) 
			+ strlen(table) 
			+ strlen(values) 
			+ strlen(col_names) 
			+ strlen(valuestr) 
			+ 9) ; /* extra for added blanks (3) and parentheses (4) */

		sprintf(stmt, "%s %s (%s) %s (%s)", 
			insert, table, col_names, values, valuestr) ;

		printf("INSERT STMT:\n %s\n", stmt) ;

		dbcmd(dbproc, stmt) ;
		dbsqlexec(dbproc) ;
		dbresults(dbproc) ;

		FREE(stmt) ;
		if (DBCOUNT(dbproc) > 0) 
			added++ ;

		record = NEXT(records, ptr) ;
	}

	printf("RECORDS ADDED: %d\n\n", added) ;

	return(added) ;	
}



/*==============================================================================
Function:		db_delete_records

Description:	
	Deletes records from a table based on a where clause

Parameters:		
	DBPROCESS *dbproc    
	char *table     - table from which to delete records
	char *where     - where clause specifying which records to delete

Returns:     	int 
	 n  - number of records deleted
	-1  - error deleting records

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
	SAMPLE: db_delete_records(dbproc, "table name", "where colname = values") ;
==============================================================================*/
int db_delete_records(DBPROCESS *dbproc, char *table, char *where_clause)
{
	int nrecs ;
	char *stmt = NULL ;
	char *delete = "delete" ;

	if (!where_clause)
		where_clause = " " ;

	stmt = (char *) ZNEW(
		strlen(delete) 
		+ strlen(table) 
		+ strlen(where_clause) 
		+ 5) ; /* extra for added blanks */

	sprintf(stmt, "%s %s %s", delete, table, where_clause) ;

	printf("DELETE STMT:\n %s\n", stmt) ;
	dbcmd(dbproc, stmt) ;
	dbsqlexec(dbproc) ;
	dbresults(dbproc) ;

	FREE(stmt) ;

	nrecs = DBCOUNT(dbproc) ;
	return(nrecs) ;
}



/*==============================================================================
Function:		db_update_records

Description:	
	Updates values of columns in existing records in a table

Parameters:		
	DBPROCESS *dbproc    
	char *table         table to update
	char *fields_to_set comma separated 'columnname = value' to update
	char *where_clause  clause specifying which records to update

Returns:     	int 
	 n - Number of records updated
	-1 - error updating records

Creator:		Ron Green

Creation Date:	07/dd/1994

Notes:		
	SAMPLE: db_update_records(dbproc,
		"table name", "col1 = 'text', col2 = 5", "where colname = 'update'") ;
==============================================================================*/
int db_update_records(DBPROCESS *dbproc, char *table, char *fields_to_set, char *where_clause)
{
	RETCODE return_code ;
	char *stmt = NULL ;

	int nrecs ;

	char *update = "update" ;
	char *set = "set" ;

	if (!where_clause)
		where_clause = " " ;

	stmt = (char *) ZNEW(
		strlen(update) 
		+ strlen(table) 
		+ strlen(set) 
		+ strlen(fields_to_set) 
		+ strlen(where_clause)
		+ 7) ; /* extra for added blanks between arguments */

	sprintf(stmt, "%s %s %s\n%s\n%s ", 
		update, table, set,  fields_to_set, where_clause) ;

	printf("UPDATE STMT:\n %s\n", stmt) ;

	dbcmd(dbproc, stmt) ;
	dbsqlexec(dbproc) ;
	dbresults(dbproc) ;

	FREE(stmt) ;

	nrecs = DBCOUNT(dbproc) ;
	return(nrecs) ;
}



/*==============================================================================
Function:		db_nth_record

Description:	Gets the nth record of a link list

Parameters:		
	llist *dbase  the link list to retrieve the record from 
	int n         the nth record to be retrieved

Returns:     	DB_RECORD ** pointer to the link list record

Creator:		Ron Green

Creation Date:	09/dd/1994

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
Function:		db_read_records_from_ingres_file

Description:	
	Reads binary records from ingres type files 

Parameters:		

Returns:     	llist* link list of records

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:		
	This function assumes that the records read are the same order in both
the ingres and sybase tables.  It also assumes that the field describing the
length of the data is 5 bytes (should be parameterized).
==============================================================================*/
#define SIZE_INGRES_FIELD_LEN 5

int read_col_from_file(int fd, char *buf) 
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
		if ((nbytes = read(fd, buf, len)) < len)
			return(-1) ;
		buf[nbytes] = NULL ;
	}
	else if (len == 0)
        return(0) ;
    else 	/* EOF */
		return(-1) ;
    
	return(len) ;
}


llist* db_read_records_from_ingres_file(
	char *table, COLUMN_DEFS *columns, char *file, int *status)
{
	llist *file_records ;	
	DB_RECORD **recordptr ;
	cursor ptr ;

	int fd ;
	int index ;

	char *cptr ;
	char buf[2048] ;

	char *format ;
	char new_format[10]  ;
	char char_format[]   = "%c" ;
	char string_format[] = "%s" ;
	char int_format[]    = "%d" ;
	char real_format[]   = "%f" ;

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
		recordptr[index] = (void *) ZNEW(columns[index].size) ;

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

		default : ;
			sscanf(buf, format, recordptr[index]) ;
			printf("SCAN FORMAT for item %s not given...\n", 
				columns[index].name) ;
			exit ;
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
Function:		db_ftn_first_record
Function:		db_ftn_next_record

Description:	
	FORTRAN support functions that mimic the FIRST and NEXT link list
macros.  These functions can be used by FORTRAN subroutines to traverse
records retrieved with db_get_records

Parameters:		

Returns:     	

Creator:		Ron Green

Creation Date:	07/dd/1994

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
	DEL_ALL(list) ;
}



/*==============================================================================
Function:	db_copy_record

Description:	copies a the field values from one DB_RECORD into another 
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

Creator:		Lawrence Stevens

Creation Date:	03/08/1995

Notes:		
==============================================================================*/
int db_copy_record(
	COLUMN_DEFS *columns,
	DB_RECORD   **destination_rec,
	DB_RECORD   **source_rec )
{

	int	j = 0 ;

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
