/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	cleanup.c

Description:
	This module contains the function used by the server for cleaning 
	up prior to exiting.

External Functions:
	cleanup
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)cleanup.c	1.2    12/16/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sybfront.h>
#include <sybdb.h>
#include <pthread.h>
#include "PPSextern.h"
#include "messages.h"
#include "check_status.h"

extern rpc_binding_vector_t     *binding_vector;
extern unsigned_char_t 	*entry_name;
extern int            	DCE_init;
extern pthread_mutex_t  g_mutex_for_PPS_dbproc_table;
extern pthread_mutex_t  g_mutex_for_odllib;
extern PPSConfigStruct	PPSConfigs[];


/*==============================================================================
Function:	void  cleanup()

Description:	
	Clean-up task performed by the server prior to exiting

Parameters: 	None
Returns:	
	OK		successful completion
	ERROR		error
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void  cleanup()
#else
void  cleanup()
#endif
{
	int		i;
 	error_status_t	status;
	int		num_threads = 0;

        /*------------------------------------------------------*/
        /* log the last shutdown in syslog and close the syslog */
        /*------------------------------------------------------*/
        pps_logMsg(ProgName, PPS_CRITICAL, "Shutting down RPC services");
 
        fprintf(stdout, "Server stops listening.\n"); 
 
        /*------------------------------------------------------*/
        /* cleanup the internal database login                  */
        /*------------------------------------------------------*/
	
	fprintf(stdout, "Cleanup database connections...\n"); fflush(stdout);
        /* delete all mutex */
        pthread_mutex_destroy( &g_mutex_for_PPS_dbproc_table );
        pthread_mutex_destroy( &g_mutex_for_odllib );

	/* close all database connections (if still alive) */ 
	sscanf(PPSConfigs[NUM_THREADS].value, "%d", &num_threads);
        for (i = 0 ; i < num_threads ; i++)
        {
		if (g_PPS_dbproc_table[i].dbproc_server != NULL) 
			dbclose (g_PPS_dbproc_table[i].dbproc_server);
		if (g_PPS_dbproc_table[i].ims_aux_query != NULL)
			ims_closeQueryConnection (g_PPS_dbproc_table[i].ims_aux_query);
	}	

        /*------------------------------------------------------*/     
        /* only perform DCE cleanup if DCE has been initialized */     
        /*------------------------------------------------------*/     

        if (DCE_init)
        {
                fprintf(stdout, "Server unregisters endpoint.\n");
                rpc_ep_unregister(
                        messages_v1_0_s_ifspec,
                        binding_vector,
                        NULL,
                        &status
                );
                CHECK_STATUS(status, "rpc_ep_unregister failed\n", RESUME);
 
                /* Unexport entry to name service database */
                fprintf(stdout, "Server unexports binding.\n");
                rpc_ns_binding_unexport(
                        rpc_c_ns_syntax_default,
                        entry_name,
                        messages_v1_0_s_ifspec,
                        NULL,
                        &status
                );
                CHECK_STATUS(status,
                        "Can't unexport to name service database\n", RESUME);
 
                rpc_binding_vector_free (
                        &binding_vector,
                        &status
                );
                CHECK_STATUS(status, "rpc_binding_vector_free failed\n", RESUME
);
        }

        /*------------------------------------------------------*/
        /* log the last shutdown in syslog and close the syslog */
        /*------------------------------------------------------*/
        pps_logMsg(ProgName, PPS_CRITICAL, "Shutting down the PPS server");
        closelog();
 
        fprintf(stdout, "PPS server exiting...\n");fflush(stdout);

}

/* End of File */
