/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	msg_handler.c

Description:	Error handlers for DB library used by PPS	

External Functions:
	msg_handler
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)msg_handler.c	1.1    11/21/96";

#include <sybfront.h>
#include <sybdb.h>

extern int strlen();
 

/*==============================================================================
Function:	int msg_handler(dbproc, msgno, msgstate, severity, msgtext,
                                srvname, procname, line)
Description:	TODO	
Parameters:	TODO
Returns:	TODO	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:16:39 PDT 1995
Notes:		
==============================================================================*/
int msg_handler(dbproc, msgno, msgstate, severity, msgtext,
        srvname, procname, line)
 
DBPROCESS       *dbproc;
DBINT           msgno;
int             msgstate;
int             severity;
char            *msgtext;
char            *procname;
DBUSMALLINT     line;
{
        printf("Msg %ld, level %d, State %d\n",
                msgno, severity, msgstate);
        if (strlen(srvname) > 0)
                printf("Server %s, ", srvname);
        if (strlen(procname) > 0)
                printf("Procedure %s, ", procname);
        if (line > 0)
                printf("Line %d, ", line);
        printf( "\n\t%s\n", msgtext);
	return(0);

} /* msg_handler */

/* End of File */
