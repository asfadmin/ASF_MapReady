/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	pps_sigtrap.c

Description:
	This module contains the function used by the server for traping signals

External Functions:
	pps_sigtrap
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)sigtrap.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "messages.h"

/*==============================================================================
Function:	void  pps_sigtrap()

Description:
	Call pps_rpc_kill() to gracefuly terminate

Parameters: 	None
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  pps_sigtrap (int signal)
#else
void  pps_sigtrap (signal)
	int signal;
#endif
{
	error_status_t status;

	printf("\nPPS Server: signal %d trapped\n", signal);
	pps_rpc_kill (&status);
}

/*==============================================================================
Function:	void  set_pps_sigtrap()

Description:
	Trap the following signals:
		Control C
		Modem Hangup
		kill command with no parameters

Parameters: 	None
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  set_pps_sigtrap ()
#else
void  set_pps_sigtrap ()
#endif
{
	signal (SIGHUP, pps_sigtrap);
	signal (SIGINT, pps_sigtrap);
	signal (SIGQUIT, pps_sigtrap);
	signal (SIGTERM, pps_sigtrap);
}

/* End of File */
