/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/


/*==============================================================================
Function:	short int pps_rpc_exit()
Description:
	This function causes the server to exit

Parameters:	
Returns:	
Creator:	Nadia Adhami
Creation Date:	Wed Jan 17 15:40:42 PST 1996
Notes:		
==============================================================================*/
#include "stdio.h"
#include "messages.h"

static char SccsFileId[] = "@(#)pps_rpc_exit.c	1.1    11/21/96";

void pps_rpc_exit( error_status_t *status )
{

	(void)pps_rpc_kill(status);
}/* pps_rpc_exit */
