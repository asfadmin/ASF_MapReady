/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	server.c

Description:	

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

static char SccsFileId[] = "@(#)server.c	1.3    02/19/97";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "messages.h"
#include "check_status.h"
#include "syslog.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "ppsmx.h"
#include "ppscount.h"

/* global program name for reporting to syslog */
char ProgName[MAX_FILENAME_LEN];

/* flag indicating whether DCE has been initialized */
int  DCE_init;

extern PPSConfigStruct  PPSConfigs[];

/* from $(DAPPS_SRCPATH)/include/defs.h */

#define  SYSLOG_PPS	(LOG_LOCAL2)

rpc_binding_vector_t 	*binding_vector;
unsigned_char_t 	*entry_name;

/*==============================================================================
Function:       static void pps_exit(int retcode)
 
Description:
        This is the function that is called every time the server has
to exit.
 
Parameters:
        retcode - the return code
 
Returns:        None
Creator:        Nadia Adhami
Creation Date:  10/10/1995
Notes:
==============================================================================*/
#ifdef __STDC__
void pps_exit(int retcode)
#else
void pps_exit(retcode)
   int retcode;
#endif
{
	/* maintain statistics */
	/* there has to be one PPS_SHUTDOWN for every PPS_START */
	pps_count(PPS_SHUTDOWN);

	exit( retcode );
}

/*==============================================================================
Function:       static void usage(char *progname)
 
Description:
        This is the program usage banner that gets executed when
the program is executed with missing or invalid options.
 
         Usage:
            pps_server [-c config_file_name] [-t number_of_threads]
 
Parameters:
        progname - the name of the executable program
 
Returns:        None
Creator:        Nadia Adhami
Creation Date:  10/10/1995
Notes:
==============================================================================*/
#ifdef __STDC__
static void
usage(char *progname)
#else
static void
usage(progname)
   char *progname ;
#endif
{
   printf("Usage:\n") ;
   printf("	%s [-c <config_file_name>] [-t <number_of_threads>]\n\n", 
	progname) ;
} /* usage */

boolean32
authFunc(
rpc_binding_handle_t	client_binding,
unsigned long           requested_mgmt_operation,
unsigned long           *status)
{
	return(TRUE);

}/* authFunc */
 

main (int argc, char *argv[])
{
	int 	ret;
	int	opt;
	unsigned32 		status;
	int	num_threads 	= 0;
	char 	logmsg[MAX_SYSLOG_MSGLEN];
	char 	*optlist 	= "c:t:" ;
	char 	*configfile = NULL;
	extern 	MX_VAR		g_dying;   /*dying flag */
	extern 	MX_VAR		g_numrpcs; /*number of rpcs in progress*/
	extern 	char		*optarg;
	char 	pps_rootpath[MAXLINE] ;
	char	*ptr;
	int	i;

	(void)strcpy(ProgName, argv[0]);

	DCE_init = 0;
 
        /* Open error log */
        openlog("PPS:", LOG_PID|LOG_CONS|LOG_NDELAY, SYSLOG_PPS) ;
	pps_logMsg(ProgName, PPS_INFO, "Starting PPS server");

	fprintf(stdout, "Starting PPS Server\n");

	for (i = strlen(argv[0]) - 1, ptr = argv[0] + strlen(argv[0]);
			i >= 0; i--, ptr--)
	{
		if (*ptr == '/')
		{
			(void)strcpy(ProgName, ptr+1);
			break;
		}
	}

	while ((opt = getopt(argc, argv, optlist)) != EOF)
	switch (opt)
	{
		case 'c' :
			configfile = (char *) strdup(optarg) ;
			break ;
		case 't' :
			sscanf (optarg, "%d", &num_threads) ;
			
			break ;
		default :
			usage(argv[0]) ;
			exit(1) ;
	} /* endswitch */

	if ((ptr = (char *)getenv(PPS_ENV_LOCAL)) == NULL)
	{
		fprintf(stderr, 
			"Environment variable %s not set, exiting...\n",
					PPS_ENV_LOCAL);
		pps_exit(1);
	   
	}
	else
	{
		/* pps_rootpath = $LOCAL/pps */
		(void)sprintf(pps_rootpath, "%s/pps", ptr);
	}


	/* Read configuration file (PRIOR to db login) */

	if (configfile == (char *)NULL)
	{
		/* Try using the default config file */
		configfile = 
		   (char *)util_do_malloc(sizeof(char)*(strlen(pps_rootpath)+1+
			strlen(PPS_DEFAULT_CONFIG)+10)) ;
		strcpy(configfile, pps_rootpath) ;
		strcat(configfile, "/config/") ;
		strcat(configfile, PPS_DEFAULT_CONFIG) ;
	}
	ret = read_config_file(configfile);
        if (ret == ER_NO_ERROR)
	{
                fprintf(stderr, "Using configuration file %s\n", configfile);
		free (configfile);
	}
	else
	{
		exit(1);
	}

        /* set the number of threads if specified on command line 
		override the value in the config file */
        if (num_threads > 0)
	{
		if (num_threads > MAX_NUM_THREADS)
		{
			printf("Maximum number of threads is %d\n",
				MAX_NUM_THREADS);
			num_threads = MAX_NUM_THREADS;
		}
		/* store it in the config structure */
		sprintf(PPSConfigs[NUM_THREADS].value,"%d", num_threads);
	}
        else
	{
		/* take it from the config file */
		sscanf(PPSConfigs[NUM_THREADS].value, "%d", &num_threads);
	}

	sprintf(logmsg,"NUM_THREADS is set to %s\n",
			PPSConfigs[NUM_THREADS].value);
	pps_logMsg(ProgName, PPS_INFO, logmsg);
       	fprintf(stdout,logmsg);

	/* prior to opening the database connections,	*
	 * register the signal trap handler		*/
	set_pps_sigtrap();

	/* Initialize the mutex used for gathering statistics 
	*/
        ret = init_pps_count();
        if (ret) 
	{
                fprintf(stderr, "ERROR: init_pps_count() failed\n");
                fprintf(stderr, 
			"Check write permission in the current directory.");
                exit(1);
        }

	/* Maintain statistics 
	*/
	pps_count(PPS_START);

	/* Initialize a pool of dbprocs 
	*/
	ret = init_PPS_dbprocs();
	if (ret)
	{
		fprintf(stderr, "ERROR: init_PPS_dbprocs() failed\n");
		pps_exit(1);
	}

	/* Initialize mutex variables used for sybase access 
	*/
	ret = init_sybase_mx();
	if (ret == ERROR)
	{
		fprintf(stderr, "ERROR : Unable to create mutex variables\n");
		pps_exit (1);
	}

	/* Mutex used as dying flag 
	*/
        ret = init_mx_var(&g_dying, 0);
        if (ret) 
	{
                fprintf(stderr, "ERROR: init_mx_var() failed ret=%d\n", ret);
                pps_exit(1);
        }

	/* Mutex used to count number of rpcs in progress 
	*/
        ret = init_mx_var(&g_numrpcs, 0);
        if (ret) 
	{
                fprintf(stderr, "ERROR: init_mx_var() failed ret=%d\n", ret);
                pps_exit(1);
        }

	/* set I/O buffering to no buffering */
	setbuf (stdout, NULL);

	/**********************************
	 ***  DCE Server Initialization ***
	 **********************************/

	/* Register interface with the RPC runtime 
	*/
	rpc_server_register_if (
		messages_v1_0_s_ifspec,
		NULL,
		NULL,
		&status
	);
	CHECK_STATUS(status, "Can't register interface\n", ABORT);

	/* Create binding information -- only accept TCP/IP */
	rpc_server_use_protseq(
		(unsigned_char_t *)("ncacn_ip_tcp"),
		rpc_c_protseq_max_reqs_default,
		&status);
	
	/*	rpc_server_use_all_protseqs(
		rpc_c_protseq_max_reqs_default,
		&status
	);
	*/

	CHECK_STATUS(status, "Can't create binding information\n", ABORT);

	/* Obtain this server's binding information 
	*/
	rpc_server_inq_bindings(
		&binding_vector,
		&status
	);
	CHECK_STATUS(status, "Can't get binding information\n", ABORT);

	/* Get entry name from environment 
	*/
	entry_name = (unsigned_char_t *)getenv("RPC_DEFAULT_ENTRY");
	if (entry_name == NULL) {
		fprintf(stderr, 
		    "Environment variable RPC_DEFAULT_ENTRY must be set.\n");
		pps_exit(1);
	}
	else
                fprintf(stderr, "RPC_DEFAULT_ENTRY set to %s\n", entry_name);

	/* Register endpoints in local endpoint map 
	*/
	rpc_ep_register(
		messages_v1_0_s_ifspec,
		binding_vector,
		NULL,
		NULL,
		&status
	);
	CHECK_STATUS(status, "Can't add address to the endpoint map\n", ABORT);

	/* Export entry to name service database 
	*/
	rpc_ns_binding_export(
		rpc_c_ns_syntax_default,
		entry_name,
		messages_v1_0_s_ifspec,
		binding_vector,
		NULL,
		&status
	);
	CHECK_STATUS(status, "Can't export to name service database\n", ABORT);

	sprintf(logmsg, "number of threads set to %d.", num_threads);

        system("date");
	puts(logmsg);
	puts("Listening for remote procedure calls...");

#if 0
	/*------------------------------------------------*/
	/* allow user's RPC to stop server from listening */
	/*------------------------------------------------*/
	rpc_mgmt_set_authorization_fn(authFunc, &status);
	CHECK_STATUS(status, "Can't authorize stop listening.\n", ABORT);
#endif

	DCE_init = 1;

	/* listening */
	rpc_server_listen ( 
		num_threads, 
		&status
	);
	CHECK_STATUS(status, "rpc listen failed\n", ABORT);

	fprintf(stdout, "Server stops listening.\n"); fflush(stdout);

	(void)pps_rpc_kill(&status);

	return(0);

} /* main */

/* End of File */
