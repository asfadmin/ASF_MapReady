/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	init_sybase_mx.c

Description:
	This module contains the function used by the server for 
	initializing mutex variables used to access sybase tables.

External Functions:
	init_sybase_mx
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)init_sybase_mx.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <PPSdefs.h>
#include <defs.h>
#include <PPSextern.h>

extern pthread_mutex_t  g_mutex_for_PPS_dbproc_table;
extern pthread_mutex_t  g_mutex_for_odllib;

int  init_sybase_mx() ;



/*==============================================================================
Function:	init_sybase_mx
Description:	Initialize mutex variables used to lock access to tables.
Parameters: 	None	
Returns:	
		OK	successful completion
		ERROR	unsuccessful completion
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int  init_sybase_mx()
#else
int  init_sybase_mx()
#endif
{
	int ret;

        /* mutex used to lock the dbproc table */
        ret = pthread_mutex_init(
                &g_mutex_for_PPS_dbproc_table, pthread_mutexattr_default);
        if (ret != 0)
        {
                pps_logMsg(ProgName, PPS_ERROR,
                           "Unable to get a lock for g_PPS_dbproc_table.");
                return(ERROR);
        }

        /* mutex used to lock the calls to ODL lib */
        ret = pthread_mutex_init(
                &g_mutex_for_odllib, pthread_mutexattr_default);
        if (ret != 0)
        {
                pps_logMsg(ProgName, PPS_ERROR,
                           "Unable to get a lock for calls to ODL lib");
                return(ERROR);
        }

	return(OK);

} /* init_sybase_mx */


/* End of File */
