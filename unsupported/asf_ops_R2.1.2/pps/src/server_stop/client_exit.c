/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	client_exit.c

Description:	
	This client sends an exit message to PPS server.

External Functions:
	None
	
Static Functions:
	None

External Variables Defined:
	None
	
File Scope Static Variables:
	files
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)client_exit.c	1.1  11/21/96";

#include <stdio.h>
#include <unistd.h>
#include "messages.h"
#include "check_status.h"

int main(int argc, char *argv[])
{
        error_status_t status = rpc_s_ok ;
 
	printf("Send an exit message, wait...\n");
        pps_rpc_exit(&status);

} /* main */

