/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	start_xact.c

Description:
	This module contains the routines used for 
	- starting a database transaction
	- commiting a database transaction
	- rolling back a database transaction

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)start_xact.c	1.2    12/16/96";

#include "PPSdefs.h"
#include "PPShdr.h"
#include "db_sybint.h"



/*==============================================================================
Function:	
	abortall()

Description:	
	This function aborts the distributed transaction

Parameters:
	in	pps_dbproc_server	
	in	dbproc_commit
	out	commid

Returns:

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/

void abortall( dbproc_server, dbproc_commit, commid )
	DBPROCESS 	*dbproc_server;
	DBPROCESS 	*dbproc_commit;
	int		commid;
{
	char		cmdbuf[256];
	int		ret;

#ifdef DEBUG
        printf("abortall(dbproc_server=0x%x\n",
		dbproc_server);
#endif

#ifdef TWOPHASE_COMMIT
	if (! dbproc_commit)
		return;

        /* some part of the transaction failed     *
	 * inform the commit server of the failure */
        abort_xact(dbproc_commit, commid);
#endif
 
        /* roll back the transaction  */
        sprintf(cmdbuf, "rollback transaction");

        dbcmd(dbproc_server, cmdbuf);
	ret = dbsqlexec(dbproc_server);
#ifdef TWOPHASE_COMMIT
        if ( ret != FAIL)
		remove_xact(dbproc_commit, commid, 1);
#endif

}

/*==============================================================================
Function:	
	pps_start_xact()

Description:	
	This function starts a transaction

Parameters:
	in	pps_dbproc_server	
	in	dbproc_commit
	out	commid

Return:

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/

void pps_start_xact( dbproc_server, dbproc_commit, commid, xact_name )
	DBPROCESS 	*dbproc_server;
	DBPROCESS 	*dbproc_commit;
	int		*commid;
	char		*xact_name;
{
	char		xact_string[128];
	char		cmdbuf[256];

#ifdef DEBUG
        printf("start_xact(server=0x%x xact_name=%s)\n",
		dbproc_server, xact_name);
#endif

        /* build the first command buffer */
        sprintf(cmdbuf, "begin transaction");
 
        /* begin transaction on the server */
        dbcmd(dbproc_server, cmdbuf);
        dbsqlexec(dbproc_server);

	return; 
}


/*==============================================================================
Function:	
	pps_commit_xact()

Description:	
	This function commits a transaction

Parameters:
	in	pps_dbproc_server	
	in	dbproc_commit
	in	commid

Returns:

Creator:	Nadia Adhami	

Creation Date:	5/1/1995

Notes:		
==============================================================================*/

void pps_commit_xact( dbproc_server, dbproc_commit, commid )
	DBPROCESS 	*dbproc_server;
	DBPROCESS 	*dbproc_commit;
	int		commid;
{
	char		cmdbuf[256];
	int		ret;

#ifdef DEBUG
        printf("commit_xact(dbproc_server=0x%x)\n",
		dbproc_server);
#endif

	/* The transaction has successfully committed, *
	 * inform the server                           */
	sprintf(cmdbuf, "commit transaction");
        dbcmd(dbproc_server, cmdbuf);
	ret = dbsqlexec(dbproc_server) ;

	return ;
}


