#define REDUCED_PRINT

#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       db_open.c

Description:    

External Functions Defined:
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          
    This file was written with a 4-character tab setting.  If you don't use 
    4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
    browse.  

External Functions:

    db_close
    db_default_error_handler
    db_default_message_handler
    db_open
    db_open_server
    db_open_errs
    db_print_command_buffer
    error_handler_exit

Static Functions:

External Variables Defined:

File Scope Static Variables:

Notes:

Function:
==============================================================================*/
#pragma ident   "@(#)db_open.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_sybint/SCCS/s.db_open.c"

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

#include "nmalloc.h"



/*==============================================================================
Function:   db_print_command_buffer()
 
Description:  print comand buffer.  
 
Creator:        Ron Green
 
Creation Date:  09/dd/1994
 
Notes:
==============================================================================*/
void db_print_command_buffer( DBPROCESS *dbproc )
{
    int     j, len;
    char        line[81];
 
    printf("Server command buffer: \n");
 
    len = dbstrlen(dbproc);
    if(len > 0)
        printf("ISQL Server command buffer contents: \n") ;
    else
    {
        printf("ISQL Server command buffer was empty. \n") ;
        return ;
    }
 
    j = 0;
    while ( j*80 < len)
    {
        (void) dbstrcpy(dbproc, 80*j++, 80, line);
        printf("%s\n",line);
    }
}
/**********************************************************************
*  Name: error_handler_exit
*
*  Module Type: Subroutine  Language: C with calls to Sybase DBLIB
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
#define  SEPARATER_LEN   80

    char separater_str[SEPARATER_LEN+1] ;
 
    printf("\n\nAPS Sybase error handler with exit:\n") ;

    memset(separater_str, '#', SEPARATER_LEN ) ;
    separater_str[SEPARATER_LEN] = '\0' ;

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
            printf("severity = %d; definitions in syberror.h\n", severity ) ;
            printf("dberr    = %d; definitions in sybdb.h\n", dberr ) ;
            db_print_command_buffer(dbproc);

            memset(separater_str, '-', SEPARATER_LEN ) ;
            separater_str[SEPARATER_LEN] = '\0' ;

            printf("%s\n\n", separater_str) ;
        }
    }
 
    printf("Terminating the run due to database errors.\n");
 
    fflush(stdout);
 
    (void) system("date;banner ERROR");
 
    /* this will cause the SQL server to print a message and abort the  */
    /* program with an error condition and without a core file.         */
    return(INT_EXIT) ;
}


/*==============================================================================
Function:       db_close()

Description:    closes the database.  although it is just about the same
                as the Sybase dbclose(), its use will isolate needed changes.

Parameters:     DBPROCESS   *dbproc

Returns:        void

Creator:        Lawrence Stevens

Creation Date:  Mon Jul 10 15:14:54 PDT 1995

Notes:
Copyright (c)1995, California Institute of Technology. U.S. Government
Sponsorship acknowledged.
==============================================================================*/
void db_close( DBPROCESS   *dbproc  )
{
    dbclose(dbproc) ;
    return ;
}


/*==============================================================================
Function:   db_default_message_handler()

Description:    offered to the calling program for db_open() as a 
                message handler, if none provided.  

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
==============================================================================*/
int db_default_message_handler(
    DBPROCESS   *dbproc,
    DBINT       msgno,    /* message number, documented in sysmessages table */
    int         msgstate, /* error state number, for Sybase tech support     */
    int         severity, /* info class/error severity, see sys admin guide  */
                            /*
                            -- From Sybase SQL server 
                            -- System Administration Guide:
                            -- page 11-7 on severity levels:
                            -- Level 10:  Status information
                            -- They are not errors at all; they 
                            -- provide additional info after 
                            -- certain commands.  
                            */

    char        *msgtext, 
    char        *srvname, 
    char        *procname, 
    int         line)       /* changed from DBUSMALLINT on new Sybase version
                               to avoid warnings.  Now matches
                               /ua/aps/sybase/sol/include/sybdb.h(line 2720)  */
{
    if (severity == 0)
        return (0);

    printf("\n\nAPS Default Sybase message handler:\n");
    printf("########################################");
    printf("#######################################\n");

    printf("Msg %ld, Level %d, State %d\n", msgno, severity, msgstate);

    if ( (int) strlen(srvname) > 0)
        printf ("Server '%s', ", srvname);
    if ( (int) strlen(procname) > 0)
        printf ("Procedure '%s', ", procname);
    if (line > 0)
        printf ("Line %d", line);

    printf("\n\t%s\n", msgtext);

    db_print_command_buffer(dbproc);

    printf("----------------------------------------");
    printf("---------------------------------------\n\n");

    return (0);
}



/*==============================================================================
Function:   db_default_error_handler()

Description:   offered to the caller of db_open() as an error handler.  

Parameters:     argument list is dictated by dberrhandle() and can't be
                changed

Returns:

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:
INT_CANCEL will return FAIL from the DB-Library function that caused the error


==============================================================================*/
int db_default_error_handler(
    DBPROCESS       *dbproc,
    int             severity,
    int             dberr,
    int             oserr,
    char            *dberrstr,
    char            *oserrstr       )
{
    printf("\n\nAPS Default Sybase error handler:\n") ;
    printf("###############################################################################\n") ;

    if ((dbproc != NULL) && !(DBDEAD(dbproc)))
    {
        printf("DB-Library error:\n\t%s\n", dberrstr);
        if (oserr != DBNOERR)
            printf("Operating-system error:\n\t%s\n", oserrstr) ;
        else
        {
            printf("severity = %d; definitions in syberror.h\n", severity ) ;
            printf("dberr    = %d; definitions in sybdb.h\n", dberr ) ;
            db_print_command_buffer(dbproc) ;
            printf("-------------------------------------------------------------------------------\n\n");

        }
    }

    /*
    -- INT_CANCEL will return FAIL from the
    -- DB-Library function that caused the eror
    */
    return(INT_CANCEL) ;
}




/**********************************************************************
*  Name: db_open_errs.c
*
*  Module Type: Subroutine  Language: C
*
*  ......./bld/mps/lib/src/opendb_errs.c
*
*  Purpose:  prints message explaining error code from opendb.c
*
*  Input Parameters:
*  Name             Type    Description
*  error_code       int     error code from opendb.c
*
*  Output Parameters:
*  Name             Type        Description
*
*  Return Parameter:
*  Type         Description
*
******************************************************************************
*  Usage example:
*
*       #include "db_sybint.h"
*       .
*       .
*       .
*       extern  DBPROCESS* db_open(char*, char*, char*, char*,
*           int (*msg_handler)(), int (*err_handler)(), int*)
*       DBPROCESS   *dbproc;
*       int         err_code;
*       int         m_handler();
*       int         e_handler();
*
*       dbproc = db_open("oprmpsdb", "create_nominal_coverage",
*               "quasimodo", "e$miralda", m_handler, e_handler, &err_code);
*       if(err_code != DB_OPEN_OK)
*       {
*           /. db_open failed ./                                            
*           db_open_errs(errcode, "oprmpsdb", "quasimodo", err_code);
*           .
*           .
*           .
*       }
******************************************************************************
*
*  Modification History:
*  Author   Revision    Date
*
****************************************************************************/
void db_open_errs(int error_code, char *dbname, char *sybase_userid)
{
    printf("Error (%d) occurred when opening the database\n", error_code);

    switch(error_code)
    {
        case DB_OPEN_OK:
            printf("DB WAS SUCCESSFULLY OPENED.\n");
            break;

        case DB_OPEN_ERR_DBNAME:
            printf("ERROR; database name not given.\n");
            printf("Check any database environment variables\n");
            break;

        case DB_OPEN_ERR_PROG:
            printf("ERROR; program name not given.  \n");
            printf("Programmer should check for a NULL string.\n");
            break;

        case DB_OPEN_ERR_USER:
            printf("ERROR; sybase userid not given.\n");
            printf("Check APS_SYBASE_USERID environment variable.\n");
            break;

        case DB_OPEN_ERR_PASSWD:
            printf("ERROR; Sybase password not given.\n");
            break;

        case DB_OPEN_ERR_DBINIT:
            printf("ERROR in initializing Sybase session.  \n");
            printf("Sybase server might be down.  \n");
            break;

        case DB_OPEN_ERR_DBLOGIN:
            printf("ERROR in Sybase session login.  \n");
            printf("Sybase server might be down.  \n");
            printf("Possibly the server is down.\n");
            break;

        case DB_OPEN_ERR_DBOPEN:
            printf("ERROR in DB-Library dbopen call.  \n");
            printf("Probably userid or password is incorrect.\n");
            printf("Possibly the server is down.\n");
            if (sybase_userid != NULL)
                printf("Userid used = %s\n", sybase_userid) ;
            break;

        case DB_OPEN_ERR_DBUSE:
            printf("ERROR in accessing database.\n") ;
            printf("Probably database does not exist.\n") ;
            if (dbname != NULL)
                printf("Database name used = %s\n", dbname) ;
            break ;

        default :
            printf("UNKNOWN ERROR in opening database.\n") ;
            printf("Code %d occurred when opening the database.\n", error_code) ;
    }
}

/**********************************************************************
*  Name: db_open()
*
*  Module Type: Subroutine  Language: C with calls to Sybase DBLIB
*
*  Purpose:
*  Starts a Sybase session via login.  Also opens the database
*  identified by an environment variable.   if there is any problem,
*  this routine stops the run.
*
*  Input Parameters:
*  Name             Type    Description
*  dbname           *char   database name to open.
*  program          *char   name of the calling program.
*  sybase_userid    *char   Sybase-approved userid.
*  password         *char   password for userid.
*  msg_handler      int(*function)()    address of message handling function,
*                                       or NULL to use default from this source
*                                       file.
*  err_handler      int(*function)()    address of error handling function,
*                                       or NULL to use default from this source
*                                       file.
*
*  Output Parameters:
*  Name             Type        Description
*  error_code       *int
*                   these are the error codes returned from dbopen.c
*                   DB_OPEN_OK  0
*                   ERR_DBNAME  1
*                   ERR_PROG    2
*                   ERR_USER    3
*                   ERR_PASSWD  4
*                   ERR_DBINIT  5
*                                   error from calling dbinit(); probably the
*                                   server is down.
*                   ERR_DBLOGIN 6
*                                   error from calling dblogin(); probably the
*                                   server is down.
*                   ERR_DBOPEN  7
*                                   probably a bad userid or password.
*                   ERR_DBUSE   8
*                                   probably a bad dbname
*
*
*  Return Parameter:
*  Type         Description
*  DBPROCESS*   if no errors (error_code = DB_OPEN_OK):
*               the return value is the pointer to the DBPROCESS structure
*               that provides the connection for the process.
*               this pointer is used in all subsequent db activity for
*               this db session.
*               if there were any errors (error_code != DB_OPEN_OK), NULL
*               is returned.
*
******************************************************************************
*  Usage example:
*
*       #include "opendb.h"
*       .
*       .
*       .
*       extern  DBPROCESS* opendb( char*, char*, char*, char*,
*           int (*msg_handler)(), int (*err_handler)(), int*)
*       DBPROCESS   *dbproc;
*       int         err_code;
*       int         m_handler();
*       int         e_handler();
*
*       dbproc = o()pendb("oprmpsdb", "create_nominal_coverage",
*               "userid", "password", m_handler, e_handler, &err_code);
*       if(err_code != DB_OPEN_OK)
*       {
*           /. opendb failed ./                                         
*           .
*           .
*           .
*       }
****************************************************************************/

DBPROCESS* db_open(
    char    *dbname,
    char    *program,
    char    *sybase_userid,
    char    *password,
    int     (*msg_handler)(),
    int     (*err_handler)(),
    int     *error_code )
{
    /* if NULL, pass the default MESSAGE HANDLER */
    if (msg_handler == NULL)
        msg_handler = db_default_message_handler ;  /* default */

    /* if NULL, pass the default ERROR HANDLER */
    if (err_handler == NULL)
        err_handler = db_default_error_handler ;  /* default */

    /* 
    -- call db_open_server() with NULL as 
    -- SQL server name.    
    */
    return db_open_server( dbname, program, sybase_userid, password, 
        msg_handler, err_handler, error_code, NULL ) ;
}

/**********************************************************************
*  Name: db_open_server()
*
*  Module Type: Subroutine  Language: C with calls to Sybase DBLIB
*
*  Purpose:
*  Starts a Sybase session via login.  Also opens the database
*  identified by an environment variable.   if there is any problem,
*  this routine stops the run. 
*
*
*  Output Parameters:
*  Name             Type        Description
*  error_code       *int
*                   these are the error codes returned from dbopen.c
*                   DB_OPEN_OK  0
*                   ERR_DBNAME  1
*                   ERR_PROG    2
*                   ERR_USER    3
*                   ERR_PASSWD  4
*                   ERR_DBINIT  5
*                                   error from calling dbinit(); probably the
*                                   server is down.
*                   ERR_DBLOGIN 6
*                                   error from calling dblogin(); probably the
*                                   server is down.
*                   ERR_DBOPEN  7
*                                   probably a bad userid or password.
*                   ERR_DBUSE   8
*                                   probably a bad dbname
*
*
*  Return Parameter:
*  Type         Description
*  DBPROCESS*   if no errors (error_code = DB_OPEN_OK):
*               the return value is the pointer to the DBPROCESS structure
*               that provides the connection for the process.
*               this pointer is used in all subsequent db activity for
*               this db session.
*               if there were any errors (error_code != DB_OPEN_OK), NULL
*               is returned.
*
******************************************************************************
*  Usage example:
*
*       #include "opendb.h"
*       .
*       .
*       .
*       extern  DBPROCESS* opendb( char*, char*, char*, char*,
*           int (*msg_handler)(), int (*err_handler)(), int*)
*       DBPROCESS   *dbproc;
*       int         err_code;
*       int         m_handler();
*       int         e_handler();
*
*       dbproc = db_open_server("oprmpsdb", "create_nominal_coverage",
*               "userid", "password", m_handler, e_handler, &err_code,
*               "DAPPSDEV" );
*       if(err_code != DB_OPEN_OK)
*       {
*           /. opendb failed ./                                         
*           .
*           .
*           .
*       }
*
****************************************************************************/
/* */
DBPROCESS *db_open_server(
    char    *dbname,
    char    *program,
    char    *sybase_userid,
    char    *password,
    int     (*msg_handler)(),
    int     (*err_handler)(),
    int     *error_code ,
    char    *sybase_server )
{
    DBPROCESS   *dbproc ;
    LOGINREC    *login ;
    RETCODE     return_code ;

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

    /* 
    -- if  sybase_server == NULL, 
    -- this is OK.  Sybase picks up the
    -- value through DSQUERY.  
    */
    if (dbinit() == FAIL)
    {
        return_code = DB_OPEN_ERR_DBINIT ;
        return NULL ;
    }

    /* Install the MESSAGE HANDLER */
    if (msg_handler != NULL)
        dbmsghandle(msg_handler) ; /* caller-supplied */

    login = dblogin();
    if (login == NULL)
    {
        *error_code = DB_OPEN_ERR_DBLOGIN ;
        return NULL ;
    }

    DBSETLAPP(login, program) ;
    DBSETLPWD(login, password) ;
    DBSETLUSER(login, sybase_userid) ;

    dbproc = dbopen(login, sybase_server) ;

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

    /* Install the ERROR HANDLER */
    if (err_handler != NULL)
        dberrhandle(err_handler) ; /* caller-supplied */

    /* free the login record area.  */
    dbloginfree(login) ;

    return(dbproc) ;
}

