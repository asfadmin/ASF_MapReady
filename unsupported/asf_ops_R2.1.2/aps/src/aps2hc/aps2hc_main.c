#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps2hc_main.c

Description:	This is the driver for the DCE client program to send
				WOS and SV file message to HC

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

*** WARNING ****** WARNING ****** WARNING ****** WARNING ****** WARNING ***
*** WARNING ****** WARNING ****** WARNING ****** WARNING ****** WARNING ***
*** WARNING ****** WARNING ****** WARNING ****** WARNING ****** WARNING ***
 
The test procedure outlined below can result in great harm to the
operational system at asf.  IT COULD BRING OPERATIONS AT ASF TO A *HALT* !!!

The test should not be carried out without first notifying ASF personnel, 
and obtaining their approval. See below.
 
*** WARNING ****** WARNING ****** WARNING ****** WARNING ****** WARNING ***
*** WARNING ****** WARNING ****** WARNING ****** WARNING ****** WARNING ***
*** WARNING ****** WARNING ****** WARNING ****** WARNING ****** WARNING ***

To test the function of this executable, we use a test server at ASF, and
transmit a WOS file to it.  This test server is under ASF control, so you
must first call Phil Muntz at (907)474-6973 or (907)474-7850 
or Norm Cushing at (907)474-7176.

Tell them that you wish to test the aps2hc_xmitClient by sending a WOS to
the test server.  They can verify that the server is up, or they can bring
it up for you.  They can also help you to monitor the test.

You must make sure that your environment settings point to the test server:
(These settings may vary, consult Phil Muntz or Norm Cushing)
	setenv HC_BACKUP_DCE_SERVER adak.asf.alaska.edu
	setenv HC_MAIN_DCE_SERVER   adak.asf.alaska.edu
	setenv HC_DCE_PROTSEQ		ncadg_ip_udp

Now, you can transfer your test WOS file. You will need both a WOS file and
its metadata file (ie: WOS.M)

Here is a test that was successfully run, using files WOStest and WOStest.M :

aps_aps2hc_xmitClient -t AWOS -d adak.asf.alaska.edu -p ncadg_ip_udp -f WOStest

Finally, notify the ASF personnel that your test is over, so that they may
monitor the system for any effects of the test.  

*** THANK U ****** THANK U ****** THANK U ****** THANK U ****** THANK U ***
*** THANK U ****** THANK U ****** THANK U ****** THANK U ****** THANK U ***
*** THANK U ****** THANK U ****** THANK U ****** THANK U ****** THANK U ***
==============================================================================*/

#pragma ident   "@(#)aps2hc_main.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.aps2hc_main.c"

#include "aps2hc.h"

/* External functions */

int aps2hc_init(char *, handle_t *, char *, unsigned_char_t *, int);
int aps2hc_send(CLIENT_INPUTS *, char *, char *, int);


/* Local Function */

static void usage(char*);

/* *************************************************************
**
** main ()
**
**
**************************************************************** */

int
main (int argc, char *argv[])
{
	extern int  optind ;
	extern char *optarg ;

	int				hostFlag = 1 ;	/* 1 if there are untried hosts, else 0 */
	int				i,j,opt, opt_flag ;
	char			*optlist   = "t:f:d:p:" ;
	char			*filename  = NULL;
	char			*filetype  = NULL;
	char			*host_name = NULL;
	unsigned_char_t	*prot_seq  = NULL;
	char			*progName ;
	char			msg[MSG_LEN];
	handle_t		binding_h;
#if 0	/***********************************/
	char *rpc_hostname[] = {"adak.asf.alaska.edu", "attu.asf.alaska.edu"}; 
#endif	/***********************************/
	char			*rpc_hostname[2];
	CLIENT_INPUTS	*client_inputs;

	setbuf( stdout, NULL ) ;
	setbuf( stderr, NULL ) ;

	progName = (char *) strdup(argv[0]) ;
	aps_open_syslog();
	(void)sprintf(msg, "Program started with options: " ) ;
	for( j = 1; j < argc; j++ )
	{
		(void) strcat(msg, " " ) ;
		(void) strcat(msg, argv[j] ) ;
	}
	aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

	opt_flag = 0;
	while((opt = getopt(argc, argv, optlist)) != EOF)
	{
		opt_flag++;
		switch(opt)
		{
			case 'f' :
				filename = (char *) strdup(optarg) ;
				break ;

			case 't' :
				filetype = (char *) strdup(optarg) ;
				break ;

			case 'd' :
				host_name = (char *) strdup(optarg) ;
				break ;

			case 'p' :
				prot_seq = (unsigned_char_t *) strdup(optarg) ;
				break ;

			default :
				(void)sprintf(msg, "Invalid option (%s) on command line.",
					argv[optind-1]);
				aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
				usage(progName) ;
				break ;
		}
	}
	if (opt_flag < 2)
	{
		usage(progName) ;
		exit(APS_EXIT_ERROR) ;
	}

	if (!filetype)
	{
		(void)sprintf(msg, "Missing file type.");
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		usage(progName) ;
	}
	else if (
		(strcmp(filetype , "AE1E") !=0) && 
		(strcmp(filetype , "AE2E") !=0) && 
		(strcmp(filetype , "AJ1E") !=0) && 
		(strcmp(filetype , "AA1E") !=0) && 
		(strcmp(filetype , "AR1E") !=0) && 
		(strcmp(filetype , "AWOS") !=0))
	{
		(void)sprintf(msg, "Invalid file type (%s).", filetype);
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		usage(progName) ;
	} 

	if (!filename)
	{
		(void)sprintf(msg, "Missing file name.");
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		usage(progName) ;
	}

	if (host_name == NULL)
	{
		if((rpc_hostname[0] = getenv("HC_MAIN_DCE_SERVER")) == NULL)
		{
			(void)sprintf(msg, "HC_MAIN_DCE_SERVER not defined !");
			aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
			(void)sprintf(msg, "Program terminated abnormally.");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			exit(APS_EXIT_ERROR) ;
		}
	}
	else
	{
		rpc_hostname[0] = (char *) strdup( host_name ) ;
	}

	if((rpc_hostname[1] = getenv("HC_BACKUP_DCE_SERVER")) == NULL)
	{
		(void)sprintf(msg, "HC_BACKUP_DCE_SERVER not set !");
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		(void)sprintf(msg, "Program terminated abnormally.");
		aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
		exit(APS_EXIT_ERROR) ;
	}

	if (prot_seq == NULL)
	{
		if((prot_seq = (unsigned_char_t *) getenv("HC_DCE_PROTSEQ")) == NULL)
		{
			(void)sprintf(msg, "HC_DCE_PROTSEQ not set !");
			aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
			(void)sprintf(msg, "Program terminated abnormally.");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			exit(APS_EXIT_ERROR) ;
		}
	}
								

	client_inputs = (CLIENT_INPUTS *) malloc( sizeof( CLIENT_INPUTS ) );
	(void) strcpy((char *)client_inputs->source, progName);

	if (access(filename, F_OK) == 0)
	{	 
		(void) strcpy((char *)client_inputs->data_file_path[0], filename);
	}
	else 
	{
		perror(filename);
		exit(APS_EXIT_ERROR);
	} 

	client_inputs->count=FILE_COUNT;
	for (i=0; i < HOSTNAME_COUNT; i++)
	{
		if(i == HOSTNAME_COUNT - 1)	/* if this is the last host */
			hostFlag = 0;
		if (host_name != NULL)
			(void) free( host_name ) ;	/* storage was allocated */
		host_name = (char *) strdup (rpc_hostname[i]); 

		if (aps2hc_init(host_name, &binding_h, progName, prot_seq, hostFlag))
		{
			client_inputs->handle = binding_h;
			switch (aps2hc_send(client_inputs,filetype,progName, hostFlag))
			{
				case 1 :	/* successful */
					exit(APS_EXIT_OK);
					break ;
				case -1 :	/* had errors, server ok; diff. server won't help */
					exit(APS_EXIT_ERROR);
					break ;
				default :	/* had server problems; try diff. server? */ 
					break ;
			}
		}
	} /* for loop */

	if (i >= HOSTNAME_COUNT)
		return (APS_EXIT_ERROR);

	return (APS_EXIT_OK) ;
} /* main */

/***************************************************************
**
** usage ()
**
**************************************************************** */

void
usage(char *progName)
{

	(void)printf("Usage: %s\n    ", progName); 
	(void)printf("-f <filename> -t <filetype> [-d <server hostname>] [-p <prot_seq>]");
	(void)printf("\nwhere....\n") ;
	(void)printf("\t-f <filename> -- file name with valid path name\n");
	(void)printf("\t-t <filetype> -- AE1E | AE2E | AJ1E | AA1E | AR1E | AWOS\n");
	(void)printf("\t-d <server hostname> -- adak.asf.alaska.edu | attu.asf.alaska.edu\n");
	(void)printf("\t-p <prot_seq> -- ncacn_ip_tcp | ncadg_ip_udp\n");
	exit(APS_EXIT_ERROR) ;
} /* usage */

