/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	get_jobstate.c

Description:
	This module contains the function used for determining
	the job state of a job in the jobs table.

External Functions:
	get_jobstate
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)get_jobstate.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sybfront.h>
#include <sybdb.h>
#include "db_sybint.h"
#include "db_jobs.h"
#include "PPSdefs.h"
#include "PPShdr.h"

extern COLUMN_DEFS	jobs_columns_[];

/*==============================================================================
Function:	get_jobstate
Description:	Find the corresponding job state from the jobs table
Parameters: 	dbproc_server, job_id, jobs_state
Returns:	
	OK		successful completion
	ERROR		error
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int  get_jobstate(DBPROCESS *dbproc_server, int job_id, char *job_state)
#else
int  get_jobstate(dbproc_server, job_id, job_state)
	DBPROCESS *dbproc_server;
	int 	job_id;
	char	*job_state;
#endif
{
	char	where_clause[100];
        cursor 	ptr ;
	llist	*llistptr = NULL;
        DB_RECORD 	**db_rec ;
	int	found = FALSE;

	/* initialize job_state */
	job_state[0] = '\0';

	/* form the query string */
	sprintf(where_clause, "where job_id = %d", job_id);

	/* find the corresponding record in the jos table */
	llistptr = db_get_records(dbproc_server, JOBS_TABLENAME, 
		where_clause, NULL, jobs_columns_, ALL_COLS);
	if (llistptr)
	{
		db_rec = (DB_RECORD **) FIRST( llistptr, ptr );
		if (db_rec)
		{
			strcpy(job_state, (char *) db_rec[JOBS_JOB_STATE]);
			found = TRUE;
		}
		DEL_LIST( llistptr );
	}

	if (found) 
		return( OK );
	else
		return( ERROR );

} /* get_jobstate */

/* End of File */
