/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       pps_rpc_kill.c
 
Description:
        This module contains the remote procedure calls used to kill
	the PPS server
 
External Functions:
	None

Static Functions:
	None
 
External Variables Defined:
	None
 
File Scope Static Variables:
	None

Notes:
==============================================================================*/

#include <stdio.h>
#include "messages.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "ppsmx.h"

static char SccsFileId[] = "@(#)pps_rpc_kill.c	1.1    11/21/96";

extern MX_VAR 			g_dying, g_numrpcs;

/*==============================================================================
Function:	short int pps_rpc_kill(error_status_t *status)
Description:
	This function shuts down the pps server by 
		- Informing the server to block RPC calls
		- Wait for all threads to complete
		- Call cleanup to close all database connections and DCE

Parameters:	DCE RPC error status
Returns:	Status of kill RPC	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 15:09:40 PDT 1995
Notes:		
==============================================================================*/
short int pps_rpc_kill(error_status_t *status)
{
	/*---------------------------------------------------*/
	/* should have used rpc_mgmt_stop_server_listening() */
	/* here, but it hangs...                             */
	/*---------------------------------------------------*/

        /* block new rpcs from starting */
        inc_mx_var(& g_dying);
 
        /* wait until no active rpcs */
        while (test_mx_var(& g_numrpcs))
	{
		printf("waiting for active rpcs to complete ...\n");
                sleep(1);
	}

        /*---------------------------------------------------*/
        /* cleanup DCE and database connections              */ 
        /*---------------------------------------------------*/

	cleanup();

	*status = 0;

	exit(0);
 
} /* pps_rpc_kill */

/* End of File */
