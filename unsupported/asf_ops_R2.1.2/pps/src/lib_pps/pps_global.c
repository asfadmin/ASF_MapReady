/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	pps_global.c

Description:
	This module contains the global variables used by the PPS server/client.

External Functions:
	None
	
Static Functions:
	None
	
External Variables Defined:
	configValues
	g_num_threads
	*g_PPS_dbproc_table
	g_mutex_for_PPS_dbproc_table
	g_mutex_for_odllib
	g_dying
	g_numrpcs
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)pps_global.c	1.2    12/16/96";

#include <stdio.h>
#include <pthread.h>
#include "db_sybint.h"
#include "PPSdefs.h"
#include "ppsmx.h"

/* structure used to store config params */

/* total number of threads allowed to run */

int			g_num_threads = 0;

/* pool of dbprocs , one per thread */

DB_Proc 		*g_PPS_dbproc_table = NULL;

/* mutex needed to lock access */

pthread_mutex_t		g_mutex_for_PPS_dbproc_table;
pthread_mutex_t		g_mutex_for_odllib;

/* needed for gracefull termination */

MX_VAR           	g_dying;   /*dying flag */
MX_VAR           	g_numrpcs; /*number of rpcs in progress*/
 
/* End of File */
