/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	init_dbprocs.c

Description:
	This module contains the routines used for 
	- initilizing a pool of database logins (implemented as a table)
	- getting a free login from the pool
	- puting back a login into the pool when not needed

External Functions:
	free_PPS_dbproc
	get_PPS_dbproc
	init_PPS_dbprocs
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)init_dbprocs.c	1.3    12/16/96";

#include <pthread.h>
#include "defs.h"
#include "PPShdr.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

extern DBPROCESS        *open_commit();
extern pthread_mutex_t  g_mutex_for_PPS_dbproc_table;
extern PPSConfigStruct	PPSConfigs[];
static int		num_threads;

/*******************************************************************
**
** checkConnection ()
**
** Check for valid database server connection.  If connection is dead,
** will try to make a connection.
**
** Return ER_NO_ERROR if connection is valid or a new connection is
**                    successfully established.
**        ER_DBLOGIN_FAIL if failed to reconnect. 
**
******************************************************************** */
 
static int checkConnection (int table_index)
{
	DBPROCESS       *dbproc_server;
	LOGINREC        *login;
	int		status;

	/* if connection is still good, return good status code to caller */
	/* if it's dead, dispose it */
	if (g_PPS_dbproc_table[table_index].dbproc_server != (DBPROCESS *) NULL)
	{
		if (DBDEAD(g_PPS_dbproc_table[table_index].dbproc_server)
			== FALSE)
		{
			return (ER_NO_ERROR);
		}
		else
		{
			dbclose(g_PPS_dbproc_table[table_index].dbproc_server);
			dbloginfree(g_PPS_dbproc_table[table_index].dblogin);
		}
	}	

	/* try to make a new connection */
	pps_logMsg(ProgName, PPS_WARNING,
	   "PPS connection to Sybase is dead -- Attempting to reconnect");

	g_PPS_dbproc_table[table_index].dbproc_server = (DBPROCESS *) NULL;
	g_PPS_dbproc_table[table_index].dblogin = (LOGINREC *) NULL;

	dbproc_server = db_open(
                /* db name   */         PPSConfigs[PPS_DBNAME].value,
                /* prog name */         ProgName,
                /* db user   */         PPSConfigs[PPS_USERID].value,
                /* db passwd */         PPSConfigs[PPS_PASSWD].value,
                /* server    */         PPSConfigs[PPS_SERVER].value,
                /* msg handl */         NULL,
                /* err handl */         NULL,
                /* db status */         &status,
                /* db login  */         &login
                ) ;
	if (status != DB_OPEN_OK)
        {
		db_open_errs(status, PPSConfigs[PPS_DBNAME].value,
                                PPSConfigs[PPS_DBNAME].value) ;
		pps_logMsg(ProgName, PPS_CRITICAL,
                "Attempt to reconnect to PPS SQL Server failed");
		return (ER_DBLOGIN_FAIL);
	}
 
	g_PPS_dbproc_table[table_index].free = TRUE;
	g_PPS_dbproc_table[table_index].dblogin = login;
	g_PPS_dbproc_table[table_index].dbproc_server = dbproc_server;
		
	return(ER_NO_ERROR);	
	
}

/*==============================================================================
Function:	
	init_PPS_dbprocs()

Description:	
	This function initializes a pool of database logins.
	It creates a login for every thread.

	Two stages:
	1. Obtain login connections with PPS database
	2. Create and initialize structures to be used for IMS queries

Parameters:	None
Returns:	OK, ERROR
Creator:	Nadia Adhami	
Creation Date:	5/1/1995

Notes:		
==============================================================================*/

int init_PPS_dbprocs()
{
	LOGINREC 	*login;
	DBPROCESS 	*dbproc_server;
        int 		i = 1, status ;
	IMS_MSG_STRUCT 	*msgDesc;
	IMS_CMN_QUERY   *aux_query;
	char 		logmsg[180];

 
	/* allocate table to store database login handles */
	sscanf(PPSConfigs[NUM_THREADS].value, "%d", &num_threads);
	g_PPS_dbproc_table = (DB_Proc *) 
		malloc(num_threads * (sizeof (DB_Proc)));
	if ( ! g_PPS_dbproc_table)
	{
                sprintf (logmsg,
                       	"Memory allocation for PPS dbproc table failed.");
                fprintf (stderr, "%s\n", logmsg);
                pps_logMsg(ProgName, PPS_ERROR, logmsg);
                return (ER_PPS_MALLOC);
	}

	for (i = 0 ; i < num_threads ; i++)
	{
        	/* logon to the PPS database */
        	dbproc_server = db_open(
                /* db name   */ 	PPSConfigs[PPS_DBNAME].value,
                /* prog name */ 	ProgName,
                /* db user   */ 	PPSConfigs[PPS_USERID].value,
                /* db passwd */ 	PPSConfigs[PPS_PASSWD].value,
                /* server    */ 	PPSConfigs[PPS_SERVER].value,
                /* msg handl */ 	NULL,
                /* err handl */ 	NULL,
                /* db status */ 	&status,
                /* db login  */ 	&login
		) ;
 
        	if (status != DB_OPEN_OK)
        	{
                	db_open_errs(status, PPSConfigs[PPS_DBNAME].value, 
				PPSConfigs[PPS_DBNAME].value) ;
                        pps_logMsg(ProgName, PPS_CRITICAL,
                	        "Could not login to database.");
                	return (ER_DBLOGIN_FAIL);
        	}

		g_PPS_dbproc_table[i].free = TRUE;

		g_PPS_dbproc_table[i].dblogin = login;
		g_PPS_dbproc_table[i].dbproc_server = dbproc_server;
		g_PPS_dbproc_table[i].dbproc_commit = NULL;

		/* Establish connection with IMS Server */
		status = ims_db_connect(&(g_PPS_dbproc_table[i].ims_aux_query));
		if (status != ER_NO_ERROR)
		{
                	fprintf (stderr, pps_err_msgs[status]);
                        pps_logMsg(ProgName, PPS_ERROR, pps_err_msgs[status]);
                	return (ER_IMS_OPEN_DB_CONNECT);
		}
	}

        return(ER_NO_ERROR) ;

} /* init_PPS_dbprocs */





/*==============================================================================
Function:	
	get_PPS_dbproc()

Description:	
	This function returns an index to an entry to a table which 
	contains free database login structures.

Parameters: dbproc_server	free login
Returns:	OK, ERROR
Creator:	Nadia Adhami	
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
int get_PPS_dbproc( table_index )
	int	*table_index;
{
	int		i;
	int		found = FALSE;

	/* lock the mutex lock for dbproc table */
	pthread_mutex_lock(& g_mutex_for_PPS_dbproc_table);
	
	/* find the next available dbproc */
	for (i = 0 ; (i < num_threads) && (!found) ; i++)
	{
		if (g_PPS_dbproc_table[i].free == TRUE)
		{
			/* check if dbproc is still good */
			if (checkConnection(i) != ER_NO_ERROR)
			{
				/* unlock the mutex lock */
				pthread_mutex_unlock(& g_mutex_for_PPS_dbproc_table);
				(void)cleanup();
				exit(1);
			}

			found = TRUE;

			/* mark the entry in the table as used */
			g_PPS_dbproc_table[i].free = FALSE;

			/* assign the return value */
			*table_index = i;
		}
	}

	/* unlock the mutex lock */
	pthread_mutex_unlock(& g_mutex_for_PPS_dbproc_table);

	if (found == TRUE)
		return (OK);
	else
	{
		*table_index = -1;
		return(ERROR);
	}

} /* get_PPS_dbproc */





/*==============================================================================
Function:	free_PPS_dbproc()
Description:	
	This function puts database login back into the pool of database logins 
once the user is done with it.

Parameters: 	table_index	index of the table
Returns:	none
Creator:	Nadia Adhami	
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
void free_PPS_dbproc(table_index)
	int table_index;
{
	/* use mutex lock to lock the table */
	pthread_mutex_lock( &g_mutex_for_PPS_dbproc_table);

	g_PPS_dbproc_table[table_index].free = TRUE;

	/* close the connection to the commit server */
	if (g_PPS_dbproc_table[table_index].dbproc_commit)
		close_commit(g_PPS_dbproc_table[table_index].dbproc_commit);

	/* unlock the mutex */
	pthread_mutex_unlock( &g_mutex_for_PPS_dbproc_table );

} /* free_PPS_dbproc */





/*==============================================================================
Function:	void free_PPS_dbprocs_table(void)
Description:	Deallocate dbprocs in dbprocs table	
Parameters:	None
Returns:	None	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:04:30 PDT 1995
Notes:		
==============================================================================*/
void free_PPS_dbprocs_table()
{
	int	i;
	IMS_CMN_QUERY   *aux_query;

	for (i = 0 ; i < num_threads ; i++)
	{
		aux_query = g_PPS_dbproc_table[i].ims_aux_query;
		ims_msgStructFree (aux_query->msgDesc);
		free (g_PPS_dbproc_table[i].ims_aux_query);
	}

} /* free_PPS_dbprocs_table */

/* End of File */
