static char *sccs = "@(#)ims_v0Srv.c	5.8  04/16/97";

/**************************************************************************
**
**   ims_v0Srv  -  Server program to support ASF v0_client connection and
**                 communications.  
**
**   Creator:   Hoshyar Sayah, Julie Wang
** 
**   Date       :  April 28 1994
**   
**   Note, this code is based on NSIDC ims_v0Srv code, developed originally 
**   by Ruth ???, Gale Tate, etc.  
**
** Modifications:
**
**   07/24/96    jwang   Added release and copyright info
**
**   06/06/96    jwang   Check environment variable after finish command
**                       checking
**
**   05/24/96    jwang   PR 770
**
**   12/14/95    jwang   added error detection on environment veriables
**                       SYBASE, IMS_SERVER, IMS_DB
**
**   06/05/95    jwang   IMS front-end order processing
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
**
**   04/31/95    jwang   add syslog capability   
**
**   10/01/94    jwang   IMS/DADS Release 0.
**
***************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <IK_Network.h>
#include <IK_Syslog.h> 
#include <odlinter.h>

#include <ims_query.h>
#include <ims_cmd.h>
#include <ims_hash.h>
#include <ims_v0.h>
#include <sys/utsname.h>
#include <syslog.h>

/* 
** the following is global data shared by the handle_sigchild procedure and 
** ims_listener__initialize function.  It is used to maintain an inventory
** of child processes so that in the case of a termination of the process the
** child processes will be properly terminated.
*/

#define DEF_PORT_NUMBER    12300     /* Default port number */
#define DEF_MAX_CHILDREN   10       /* Default maximum children */
/*
#define DEF_LOGIN          "IMS" */    /* Default database login */
/*
#define DEF_PASSWORD       "IMS"  */  /* Default database password */
#define DEF_DIRECTORY      "./"     /* Default log directory */

/*
** global variables
*/
char *glbLogin; 
char *glbPassword;
char *glbLogDir;
char *glbArchiveDir;

/*
IMS_HASH_STRUCT *v0_package_HashPtr;
AGGREGATE pkg_root_tree;
*/

/*
** Local function declarations 
*/
static void sigchild_handler (int);
static void gracefull (int);
static int runDown (int);
static void usage ();
static int  processCmdLine ();
static void processMessages ();

/*
** External varaiable declarations 
*/
extern int errno; 

/*
** local variables
*/
static char *program;
static char buff[IMS_INPUT_BUF_LEN+1];
static int  port;             /* server port number */
static int  maxChild;         /* server max children number */
static int  sigterm_flag = 0;     /* sigterm flag inherited from the server */

/*
** variable declarations 
*/
IK_Errno IK_ImsErrno;

#ifndef NDEBUG
int DebugLevel = 0;
#endif


/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *port;
	char *maxChild;
	char *login;
	char *password;
	char *logDir;
	char *archiveDir;
	char *help;
	char *nopackage;
	char *commandFile;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm[] =
{
	{"-N",          &commands.port},
	{"+portN",      &commands.port},
	{"-M",          &commands.maxChild},
	{"+maxChild",   &commands.maxChild},
	{"-L",          &commands.login},
	{"+login",      &commands.login},
	{"-P",          &commands.password},
	{"+password",   &commands.password},
	{"-D",          &commands.logDir},
	{"+logDir",  	&commands.logDir},
        {"-A",          &commands.archiveDir}, /* enable archiving client msgs */
        {"+archiveDir", &commands.archiveDir},
	{"-C",          &commands.commandFile},
	{"+commandFile",&commands.commandFile},
	{"-k",          &commands.nopackage},  /* disable Dynamic Packaging */
	{"+noPackage",  &commands.nopackage},
	{"-h",          &commands.help},
	{"+help",       &commands.help},
};

static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm[] =
{
	{"portN",      &commands.port},
	{"maxChild",   &commands.maxChild},
	{"login",      &commands.login},
	{"password",   &commands.password},
	{"logDir",     &commands.logDir},
	{"archiveDir", &commands.archiveDir},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/************************************************************************
**
** Main program
**
************************************************************************/

main(argc, argv)
int argc;
char *argv[];
{
	int	 server;        /* server created by IK_InitConnection */
 	int  message_id;    /* message identifier for a client socket */
 	int  child_id;      /* id of a forked child process        */
 	int  sigchild_flag;  /* indicates arrival of a sigchild signal */
 	char errbuf[IK_MAXBUFLEN];  /* contains an error message */
	char logName[IMS_COL128_LEN+1];
	char IK_logName[IMS_COL128_LEN+1];
	char logFile[IMS_COL255_LEN+1];
	char IK_logFile[IMS_COL255_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char hostName[IMS_HOST_LEN+1];
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	int	i, status, statusp;
	IMS_MSG_STRUCT *msgDesc;
	char env_string[IMS_COL60_LEN+1];
	char *timeStamp;
	V0_CAT_STRUCT catReq;
	struct stat     st;

	/* Initialize variables */
	errno = 0;
	port = DEF_PORT_NUMBER;
	v0_package_HashPtr = (IMS_HASH_STRUCT *)NULL;

	/*
	** print the release and copyright information 
	*/
	(void) ims_printVersion (stderr);

	/*
	** extractFileName ALWAYS returns a pointer to a string.
	*/
	program = (char *)ims_extractFileName (argv[0]);
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';

	/* set umask */
	(void) umask (S_IRWXG|S_IRWXO);

	/*
	** establish a signal handler for the sigchild signal (child term.).
	** procedure will remove a child id from the (global) list of children.
	*/
	(void) signal(SIGCHLD, SIG_IGN);
	(void) signal (SIGTERM, gracefull);
	(void) signal (SIGINT,  gracefull);
	(void) signal (SIGUSR1, gracefull);

	/*
	** Initialize message facility and options. 
	*/

	/* 
	** Allocate msg structure 
	*/
	if ((msgDesc = (IMS_MSG_STRUCT *)ims_msgStructAlloc ())
					== (IMS_MSG_STRUCT *)NULL)
	{
		fprintf (stderr, "IMS_MSG_STRUCT allocation error.");
		exit (-1);
	}

	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, program);
	(void) sprintf (banner, "%s::%s", hostName, program);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

#ifdef MDEBUG
	(void) ims_msgSetSyslogSeverity (msgDesc, IMS_INFO);
#else
	(void) ims_msgSetSyslogSeverity (msgDesc, IMS_ERROR);
#endif 

	(void) ims_msgStderrFlag (msgDesc, IMS_ON);
	(void) ims_msgQueueFlag (msgDesc, IMS_OFF);

	if ((status = ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, "Could not turn on syslog.");
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);

	}


	/*
	** Initialize the signal handler.
	*/
	
	if (ims_setWrapup (runDown) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		     "Failed to initialize signal handler.");
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (1);
	}

	/*
	** get command line args.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		(int) cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (IMS_INFO);
	}

	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the commandline, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if (ims_getFileParms (commands.commandFile, cmdFileElm,
			cmdFileElmCount, msgDesc) < IMS_OK)
		{
			ims_msgStructFree(msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;
			exit (-1);
		}

		/*
		** get command line args again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv, cmdLineElm, 
			(int) cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			ims_msgStructFree(msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;
			exit (-1);
		}
	}

	/*
	** Update the request structure with information from the command
	** line or from a command file.
	*/
	if ((status = processCmdLine (msgDesc)) < IMS_OK)
	{
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (status);
	}

	/*
	** define the path to store log files.   
	*/
	env_string[0]='\0';
	sprintf (env_string,"USERDATA_DIR=%s",glbLogDir);
	(void) putenv (env_string);

	timeStamp = (char *)ims_timeStamp();
	/*
	(void) sprintf (logName, "ims_v0Srv_%s.log", 
		(char *)ims_timeStamp ());
	*/
	(void) sprintf (logName, "ims_v0Srv_%s.log", timeStamp);
	free (timeStamp);
	(void) ims_concatFilePath (logFile, glbLogDir, logName);

	(void) sprintf (IK_logName, "IK_%s", logName);
	(void) ims_concatFilePath (IK_logFile, glbLogDir, IK_logName);


	if ((status = ims_msgLogFileName (msgDesc, logFile) ) < IMS_OK )
	{
		(void) ims_msg (msgDesc, status, "Could not turn on log file");
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	if ( (status = IK_NameSyslog (IK_logFile)) < 0)
	{
		(void) ims_msg (msgDesc, status, "Could not open IK syslog file");
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	/*
	** change log file permission
	*/
	if ((chmod (logFile, 0644)) == -1)
	{
		(void) ims_msg (msgDesc, status, 
			"Could not change permission for log file %s", logFile);
		IK_CloseSyslog ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	if ((chmod (IK_logFile, 0644)) == -1)
	{
		(void) ims_msg (msgDesc, status, 
			"Could not change permission for IK syslog file %s", IK_logFile);
		IK_CloseSyslog ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	/* 
	** Initialize IK server connection 
	*/
	server = 0;
	if ((server = IK_InitConnection(port)) <= 0)
	{
		errbuf[0] ='\0';
		(void)sprintf(errbuf, "IK_InitConnection failed for port '%d'.\n", port);
		(void) ims_msg (msgDesc, IMS_FATAL, errbuf);
		IK_CloseSyslog ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit(-1);
	}

	/*
	** check if all required environment variables, i.e. IMS_SERVER, IMS_DB
	** SYBASE have been defined
	*/
	if (getenv("IMS_SERVER") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable IMS_SERVER has not been defined.");
		IK_CloseSyslog ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	if (getenv("IMS_DB") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable IMS_DB has not been defined.");
		IK_CloseSyslog ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	if (getenv("SYBASE") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable SYBASE has not been defined.");
		IK_CloseSyslog ();
		ims_msgStructFree(msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		exit (-1);
	}

	/*
	** If client messages archiving is enabled, make sure the specified
	** archive directory is valid 
	*/
	if (glbArchiveDir != (char *) NULL)
	{
		if (stat(glbArchiveDir, &st) < 0 || !(S_ISDIR(st.st_mode))) 
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Invalid path specified for V0 Client Message archiving.");
			IK_CloseSyslog ();
			ims_msgStructFree(msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;
			exit (-1);
		}
		(void) ims_msg (msgDesc, IMS_INFO,
			"***** V0 Client Message Archiving Enabled.");

	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"***** Client Message Archiving Not Enabled.");
	}

	/*
	** If dynamic packaging is specified (default), create a hash table and 
	** load package infomation. 
	**
	** This is done only once when the server is started.  The server has to 
	** be restarted whenever the package information is updated.
	*/

	if (commands.nopackage != (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_INFO, 
					"***** Dynamic Packaging disabled.");
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO, 
					"***** Begin collecting Dynamic Packaging Information.");

		/*
		** initialize a catReq structure
		*/
		catReq.qDesc = (IMS_QI_DESC_OBJ *)NULL;
		catReq.msgDesc = msgDesc;
		catReq.userSpec.dbUserName = glbLogin;
		catReq.userSpec.dbPassword = glbPassword;
		catReq.userSpec.server = V0_SERVER;
		catReq.userSpec.dbName = V0_DBNAME;
		catReq.userSpec.program= V0_PROGRAM;
	
		if ( v0_cat(&catReq, V0_OPENCONNECTION) <IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, "Database connection failed.");
			IK_CloseSyslog ();
			ims_msgStructFree(msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;
			exit (-1);
		}

		status = v0_package__build_packageHash
				(msgDesc, glbLogin, glbPassword, &catReq, 1);

		(void) v0_cat(&catReq, V0_CLOSECONNECTION);
/* jlw testing*/
/*
printf ("status is %d\n End testing.\n", status);
status = -3;
*/

		if (status < IMS_OK)
		{

			(void) ims_msg (msgDesc, status, 
				">>> Unable to build Package Info Hash Table. "); 
			if (v0_package_HashPtr != (IMS_HASH_STRUCT *)NULL)
			{
				(void) ims_hashDestroy (v0_package_HashPtr, msgDesc);
			}
			IK_CloseSyslog ();
			ims_msgStructFree(msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;
			exit (-1);
		}

		(void) ims_msg (msgDesc, IMS_INFO, 
			"***** Dynamic Packaging Info collection completed.\n");

	}

	(void) ims_msg (msgDesc, IMS_INFO, "***** Successful server connection.\n");

	/* loop and fork a child process to handle client connection requests */
	while (1)
	{
		/*
		** break, if a sigterm signal has been set.
		*/
		if (sigterm_flag)
		{
			errbuf[0] = '\0';
			sprintf(errbuf,"Break: sigterm flag is set.\n");
			(void) ims_msg (msgDesc, IMS_INFO, errbuf);

			break;
		}

		errno = 0;
		/* Loop and accept a client message */
		while ((message_id = IK_Accept(server)) < 0) 
		{
			if (errno == EWOULDBLOCK)
			{
				errno = 0;
				continue;  /* No message received */
			}
			else  /* some error has occurred */
			{
				IK_Shutdown(message_id);

				errbuf[0] = '\0';
				sprintf(errbuf,"***** IK_Accept failed with errno = %d \n", errno);
				(void) ims_msg (msgDesc, IMS_FATAL, errbuf);
				(void) processMessages (msgDesc);
				IK_CloseSyslog ();
				ims_msgStructFree(msgDesc);
				msgDesc = (IMS_MSG_STRUCT *)NULL;
				exit(-1);
			}  /* end loop */
		}

		if ((child_id = fork()) == 0)
		{ 
			/* Child process */

			/*
			sleep (30); 
			*/

			/* 
			** Close server socket connection 
			*/
			IK_Close (server);

			errno = 0;
			(void) v0_handler (msgDesc, message_id, glbLogin, glbPassword, glbArchiveDir);

			/* 
			** Process messages and place in log file 
			*/
			(void) processMessages (msgDesc);

			errno = 0;

			/* 
			** Shutdown sockect connection. No longer needed. 
			*/
			if (IK_Shutdown (message_id) < 0)
			{
				errbuf[0] = '\0';
				(void)sprintf(errbuf, "IK_Shutdown failed.\n");
				(void) ims_msg (msgDesc, IMS_FATAL, errbuf);

				(void) processMessages (msgDesc);
				IK_CloseSyslog ();
				ims_msgStructFree(msgDesc);
				msgDesc = (IMS_MSG_STRUCT *)NULL;
				exit(-1);
			}
			else  
			{
				(void) processMessages (msgDesc);
				IK_CloseSyslog ();
				ims_msgStructFree(msgDesc);
				msgDesc = (IMS_MSG_STRUCT *)NULL;
				exit(0);   
			}

		}
		else 
		{
			/* Parent process */
			/* Close connectiion to msg-socket handled by the child,
			** reports errors or adds child_id to the list of children.
			*/
			errno = 0;
			IK_Close(message_id);

			if (child_id < 0)
			{
				/* Forking child process failed. */
				errbuf[0] = '\0';
				(void)sprintf(errbuf, "Fork child process failed.\n");
				(void) ims_msg(msgDesc, IMS_FATAL, errbuf);
			}

			/* wait with WNOHANG option for zombie child process .
			** OK for now.  Must create a more elaborate scheme for
			** cleaning all zombie child processes.
			*/
			(void) waitpid (-1, &statusp, WNOHANG);

			errno = 0;
		}

	} /* end while loop */

	(void) processMessages (msgDesc);
	ims_msgStructFree(msgDesc);
	msgDesc = (IMS_MSG_STRUCT *)NULL;

	exit (0);

} /* end main program */



/*********************************************************************
**
** gracefull -  
**
**  this function handles the SIGUSR1 signal for a gracefull shutdown
**
** Notes:  
** (1) you could use this as the signal handler if you wanted to 
** now how childs time was spent.
**
** 1/10/95 - S. Hardman
** Removed call to getdtablesize() which was used to close open file
** descriptors.  There is no substitution for this call and besides
** exit() will close all open file descriptors for us.
**
***********************************************************************/
static void gracefull (sig)
int sig;			/* the signal that caused the interupt */
{
	/* 
	** add whatever else you need here
	*/

	kill (0, SIGUSR1);		/* kill all of my kids */
	sigterm_flag = 1;
}

/**************************************************************************
**
**  sigchild_handler - 
*
* Purpose:
* This procedure catches a sigcld signal and deletes terminated subprocess 
* from the list of children.
*
* Method:
* The procedure is called when a signal is generated by a terminating child.
* The child's id is fetched and it is removed from the list of children,
* which is a global variable.                                                
**************************************************************************/
static void sigchild_handler (sig)
int sig;
{
	fprintf (stdout, "sigchild_hanlder is called.\n");
	fflush (stdout);

	errno = 0;
}


/***************************************************************************
**
** runDown -  Cleanup and exit from program.
**
***************************************************************************/
static runDown (int sig)
{
	(void) printf ("\n\nTermination of %s due to signal: %s (%d) \n\n",
	          program, ims_sigMsg(sig), sig);     
	(void)v0_msgTree__destroy (pkg_root_tree);

	return (sig);
}

/***************************************************************************
**
** usage ()
**
** Print command line argument switches.
**
***************************************************************************/

static void usage (void)
{
	int i;

	(void) fprintf (stderr, "\nims_v0Srv usage:\n\n");
	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}
	(void) fprintf (stderr,"\n\n");
}

/**************************************************************************
**
** processCmdLine - 
**
** Update the request structure and prompt for needed 
** information not provided either in the commandLine or
** commandFile.
**
***************************************************************************/

static int processCmdLine (msgDesc)
IMS_MSG_STRUCT *msgDesc;
{
	int invalid, i, number;
	char *Msg;

	/* Initialize */
	glbLogin = (char *)NULL;
	glbPassword = (char *)NULL;
	glbArchiveDir = (char *)NULL;
	invalid = 1;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/
	
	/* Define port number if specified by user*/
	if ( (commands.port != (char *) NULL) &&
		  (strlen (commands.port) != 0) )
	{
		if ( (!ims_isInteger (commands.port)) || 
		     ( (port = (int) atoi (commands.port)) < 2000) )

		{
			(void) ims_msg (msgDesc, IMS_WARNING, 
				"Port number is not a valid integer, or is less than '2000'.");
			do
			{
				if ((char *)ims_getString(IMS_TRUE, buff, sizeof (buff), 
					"Port :  ") == (char *)NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"Error detected while reading input string.");
					return (IMS_FATAL);
				}
				invalid = 0;
				for (i=0; buff[i]; i++)
				{
					if (! isdigit (buff[i]))
					{
						(void) fprintf (stderr, 
							"Expecting numerical input.  Try again.\n");
						fflush (stderr);
						invalid = 1;
						break;
					}
				}

				if (!invalid && 
						((number = (int) atoi(buff)) < 2000))
				{
					invalid = 1;
					(void) fprintf (stderr, 
						"Port number is less than '2000'.  Try again.\n");
					fflush (stderr);
				}
			}while (invalid);

			port = number;
		}
	}
	else
	{
		port = DEF_PORT_NUMBER;
	}


	invalid = 1;

	/* Define maxChild number */
	if ( (commands.maxChild != (char *) NULL) &&
		  (strlen (commands.maxChild) != 0))
	{
		if ( (!ims_isInteger (commands.maxChild)) || 
		     ( (maxChild = (int) atoi (commands.maxChild)) < 0) ||
			  ( maxChild > 256) )
		{
			(void) ims_msg (msgDesc, IMS_WARNING, 
				"maxChild is not a valid integer, or not in range of 0 < maxChild < 256.");
			do
			{
				if ((char *)ims_getString(IMS_TRUE, buff, sizeof (buff), 
					"MaxChild :  ") == (char *)NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"Error detected while reading input string.");
					return (IMS_FATAL);
				}
				invalid = 0;
				for (i=0; buff[i]; i++)
				{
					if (! isdigit (buff[i]))
					{
						(void) fprintf (stderr,  
							"Expecting numerical input.  Try again.\n");
						fflush (stderr);
						invalid = 1;
						break;
					}
				}
				if (!invalid && 
						(((number = (int) atoi(buff)) < 0) || (number > 256)))
				{
					invalid = 1;
					(void) fprintf (stderr,
						"MaxChild not in range: 0 < maxChild < 256.  Try again.\n");
					fflush (stderr);
				}
			}while (invalid);
			maxChild = number;
		}
	}
	else
	{
		maxChild = DEF_MAX_CHILDREN;
	}

	/* define the log directory path */
	if ( (commands.logDir != (char *) NULL) &&
		  (strlen (commands.logDir) != 0 ))
	{
		glbLogDir = commands.logDir;
	}
	else
	{
		glbLogDir = DEF_DIRECTORY;
	}

	invalid = 1;

	/* check the archive directory path */
	if (commands.archiveDir != (char *) NULL)
	{
		/* use the current directory path if not provided */
		if (strlen(commands.archiveDir) == 0)
		{
			glbArchiveDir = malloc(IMS_COL255_LEN+1);	
			(void) getcwd(glbArchiveDir,IMS_COL255_LEN);
		}
		else
		{
			glbArchiveDir = commands.archiveDir;
		}
	}

	invalid = 1;

	/* Check login information */
	if (commands.login != (char *) NULL)
	{
		glbLogin = commands.login;
	}
	else
	{
		do
		{
			if  ((char *)ims_getString(IMS_FALSE, buff, sizeof(buff),
				"login : ") == (char *)NULL) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}
      
			ims_truncStr (buff);

			if (strlen(buff) != 0)
			{
				invalid = 0;
			}

		} while (invalid);

		 glbLogin = malloc (strlen (buff) + 1);
		(void) strcpy (glbLogin, buff);

	}

	invalid = 1;

	if (commands.password != (char *) NULL)
	{
		glbPassword = commands.password;

	}
	else
	{
		if ((glbLogin != (char *)NULL) && (strlen(glbLogin) != 0))
		{
			do
			{
				if (ims_getPassword (buff) == NULL)
				{
					return (IMS_FATAL);
				}

				ims_truncStr (buff);

				if (strlen (buff) != 0)
				{
					invalid = 0;
				}

			} while (invalid);

			glbPassword = malloc (strlen (buff) + 1);
			(void) strcpy (glbPassword, buff);


		}
	}

	return (IMS_OK);
}

/************************************************************************
**
** processMessages - Log messages with severity level < IMS_ERROR. 
**                   Note that message queue is cleaned up.
**
************************************************************************/
static void processMessages (IMS_MSG_STRUCT *msgDesc)
{
	IMS_MSG_QUEUE *msgQueue;

	msgQueue = (IMS_MSG_QUEUE *)NULL;

	while ((msgQueue = (IMS_MSG_QUEUE *)
		ims_msgQueueExtract (msgDesc)) != (IMS_MSG_QUEUE *)NULL)
	{
		/* Only log msgs with ERROR or FATAL severity level */
		if (msgQueue->severity <= IMS_ERROR)
		{
			errno = 0;
			IK_Syslog (LOG_ERR, msgQueue->msg);
		}
		(void) ims_msgQueueFree (msgQueue);
		msgQueue = (IMS_MSG_QUEUE *)NULL;
	}

	return;
}
