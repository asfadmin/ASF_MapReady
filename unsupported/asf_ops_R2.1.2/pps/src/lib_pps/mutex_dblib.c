/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	mutex_dblib.c

Description:
	This module contains a mutex protected wrapper for functions
	which access the db open client library.

External Functions:
	db_exec_cmd_mx
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)mutex_dblib.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include <sybfront.h>
#include <sybdb.h>
#include "db_sybint.h"
#include "db_jobs.h"
#include "PPSdefs.h"
#include "PPShdr.h"

extern pthread_mutex_t  g_mutex_for_dblib;

/*==============================================================================
Function:	db_exec_cmd_mx
Description:	Wrap the db_exec_cmd() in a mutex.
Parameters: 	dbproc_server, cmdbuf, ret, nrecs
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  db_exec_cmd_mx(DBPROCESS *dbproc,char *cmdbuf,int *ret_value, int *nrecs)
#else
void  db_exec_cmd_mx(dbproc, cmdbuf, ret_value, nrecs)
	DBPROCESS *dbproc;
	char *cmdbuf;
	int *ret_value;
	int *nrecs;
#endif
{

#ifdef DEBUG
	printf("db_exec_cmd_mx(%s)\n",cmdbuf);
#endif
        /* lock the mutex lock for call to dblib */
        pthread_mutex_lock(& g_mutex_for_dblib);
#ifdef DEBUG
	printf("LOCK g_mutex_for_dblib\n");
#endif

        db_exec_cmd(dbproc, cmdbuf, ret_value, nrecs);

        /* unlock the mutex lock */
        pthread_mutex_unlock(& g_mutex_for_dblib);
#ifdef DEBUG
	printf("UNLOCK g_mutex_for_dblib\n");
#endif

} /* db_exec_cmd_mx */

/* End of File */
