static char *sccs = "@(#)ims_disp.c	5.6  25 Jul 1996";

/**************************************************************************
**
**   ims_disp - Process to dispatch various types of orders 
**
**   Creator:   Julie Wang
** 
**   Date       : June 9 1995 
**   
**   Description: This is a process runs as a background job to collect
**                and forward ASF product orders from IMS catalog
**                to appropriate subsystems for order processing or
**                product generation based on the order type.
**                
**                List of capabilities:
** 
**                1. Accepts parameters from both command line and
**                   command file.
**                2. Supports syslog functions and generates run-time log.
**                3. Notify subsystems upon arrival of new orders.
**                4. Updates IMS catalog with latest status. 
**  Modifications:
**       
**   07/24/96   jwang  Added release and copyright info.
**
**   06/06/96   jwang  Check environment variables after finish command 
**                     checking.
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
#include <sys/signal.h>

#include <ims_query.h>
#include <ims_cmd.h>
#include <syslog.h>
#include <IK_Network.h>
#include <sys/utsname.h>
#include <IK_Syslog.h>

#define DEF_SLEEPTIME      300      /* Default delay time is 5 minutes */
#define DEF_DIRECTORY      "./"     /* Default log directory */

/*
** global variables
*/
char *glbLogin; 
char *glbPassword;
char *glbDirectory;   /* location for log file */

/*
** Local function declarations 
*/
static void gracefull (int sig);
static void runDown (int status);
static void usage ();
static int processCmdLine ();
static void processMessages ();


/*
** local variables
*/
static char *program;
static char buff[IMS_INPUT_BUF_LEN+1];
static int sleepTime;        /* delay time */
static int  sigterm_flag=0;

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
	char *sleepTime;
	char *login;
	char *password;
	char *directory;
	char *help;
	char *commandFile;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm[] =
{
	{"-S",          &commands.sleepTime},
	{"+sleepTime",  &commands.sleepTime},
	{"-L",          &commands.login},
	{"+login",      &commands.login},
	{"-P",          &commands.password},
	{"+password",   &commands.password},
	{"-D",          &commands.directory},
	{"+directory",  &commands.directory},
	{"-C",          &commands.commandFile},
	{"+commandFile",&commands.commandFile},
	{"-h",          &commands.help},
	{"+help",       &commands.help},
};

static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm[] =
{
	{"sleepTime",  &commands.sleepTime},
	{"login",      &commands.login},
	{"password",   &commands.password},
	{"directory",  &commands.directory},
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
	char logName[IMS_COL128_LEN+1];
	char IK_logName[IMS_COL128_LEN+1];
	char logFile[IMS_COL255_LEN+1];
	char IK_logFile[IMS_COL255_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char hostName[IMS_HOST_LEN+1];
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	int	i, status, statusp;
	char env_string[IMS_COL60_LEN+1];
 	IMS_MSG_STRUCT *msgDesc;
	char msgbuf[IMS_COL60_LEN+1];
	char *timestamp;

	int count =0;

	/*
	** print release and copyright information 
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
	** establish a signal handler 
	*/
	(void) signal (SIGCHLD, SIG_IGN);
	(void) signal (SIGTERM, gracefull);
	(void) signal (SIGINT,  gracefull);
	(void) signal (SIGUSR1, gracefull);

	/*
	** Initialize message facility.
	*/

	if ((msgDesc = (IMS_MSG_STRUCT *)ims_msgStructAlloc ())
					== (IMS_MSG_STRUCT *)NULL)
	{
		fprintf (stderr, "IMS_MSG_STRUCT allocation error.");
		runDown (-1);
	}


	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, program);
	(void) sprintf (banner, "%s::%s", hostName, program);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

#ifdef DEBUG
	(void) ims_msgSetSyslogSeverity (msgDesc, IMS_INFO);
#else
	(void) ims_msgSetSyslogSeverity (msgDesc, IMS_ERROR);
#endif 

	(void) ims_msgStderrFlag (msgDesc, IMS_ON);
	(void) ims_msgQueueFlag (msgDesc, IMS_OFF);

	if ((status = ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, "Could not turn on syslog.");

		(void) processMessages (msgDesc);
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;

		runDown (-1);

	}

#ifdef DCEON 
	if (getenv("RPC_DEFAULT_ENTRY") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable RPC_DEFAULT_ENTRY has not been defined.");
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		runDown (-1);
	}
#endif 

	/*
	** Initialize the signal handler.
	**
	** if (ims_setWrapup (runDown) < IMS_OK)
	** 	runDown(ims_syserr (IMS_FATAL));
	*/

	/*
	** get command line args.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		(int) cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, "failed to get command line arguments");

		(void) processMessages (msgDesc);
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;

		runDown (-1);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();

		(void) processMessages (msgDesc);
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;

		runDown (IMS_INFO);
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
			(void)ims_msg (msgDesc, status, "failed to get command file arguments");

			(void) processMessages (msgDesc);
			ims_msgStructFree  (msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;

			runDown (-1);
		}

		/*
		** get command line args again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv, cmdLineElm, 
			(int) cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void)ims_msg (msgDesc, status, "failed to get command line arguments on the 2nd time");

			(void) processMessages (msgDesc);
			ims_msgStructFree  (msgDesc);
			msgDesc = (IMS_MSG_STRUCT *)NULL;

			runDown (-1);
		}
	}

	/*
	** Update the request structure with information from the command
	** line or from a command file.
	*/
	if ((status = processCmdLine (msgDesc)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, "failed to process arguments");

		free (glbLogin);
		free (glbPassword);
		(void) processMessages (msgDesc);
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;

		runDown (status);
	}

	if (sleepTime < DEF_SLEEPTIME)
	{
		(void)ims_msg (msgDesc, IMS_ERROR, 
			"sleepTime is too short.  Minimum 300 seconds");

		free (glbLogin);
		free (glbPassword);
		(void) processMessages (msgDesc);
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;

		runDown (status);
	}

	/*
	** define the path to store log files.   
	*/

	timestamp = (char *)ims_timeStamp();
	(void) sprintf (logName, "ims_disp_%s.log", timestamp);
	free(timestamp);
	(void) ims_concatFilePath (logFile, glbDirectory, logName);
 
	(void) sprintf (IK_logName, "IK_%s", logName);
	(void) ims_concatFilePath (IK_logFile, glbDirectory, IK_logName);


	if ((status = ims_msgLogFileName (msgDesc, logFile) ) < IMS_OK )
	{
		(void)ims_msg (msgDesc, status, "Could not turn on log file");

		free (glbLogin);
		free (glbPassword);
		(void) processMessages (msgDesc);
		(void) ims_msgStructFree (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;

		runDown (-1);
	}

	if ( (status = IK_NameSyslog (IK_logFile)) < 0)
	{
		free (glbLogin);
		free (glbPassword);
		(void) ims_msg (msgDesc, status, "Could not open IK syslog file");
		(void) processMessages (msgDesc);
		(void) ims_msgStructFree (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
								 
		runDown (-1);
										 
	}
 
	/*
	** change log file permission
	*/
	if ((chmod (logFile, 0644)) == -1)
	{
		(void) ims_msg (msgDesc, status,
						 "Could not change permission for log file %s", logFile);
		(void) ims_msgStructFree (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		IK_CloseSyslog ();
		unlink (IK_logFile);
		runDown (-1);
	}
											 
	if ((chmod (IK_logFile, 0644)) == -1)
	{
		(void) ims_msg (msgDesc, status,
						 "Could not change permission for log file %s", IK_logFile);
		(void) ims_msgStructFree (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		IK_CloseSyslog ();
		unlink (IK_logFile);
		runDown (-1);
	}
											 

	/*
	** check if all required environment variables, i.e. IMS_SERVER, IMS_DB
	** SYBASE have been defined
	*/
	if (getenv("IMS_SERVER") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable IMS_SERVER has not been defined.");
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		runDown (-1);
	}

	if (getenv("IMS_DB") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable IMS_DB has not been defined.");
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		runDown (-1);
	}

	if (getenv("SYBASE") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "Environment variable SYBASE has not been defined.");
		ims_msgStructFree  (msgDesc);
		msgDesc = (IMS_MSG_STRUCT *)NULL;
		runDown (-1);
	}

	while (1)
	{

		if (sigterm_flag)
		{
#ifdef DEBUG 
			(void) ims_msg (msgDesc, IMS_INFO, "Break: sigterm flag is set.");
#endif

			break;
		}

		/* for debugging only
if (count == 0)
{
count++;
sleep (30);
}
*/


		(void) ims_dispHandler(msgDesc, glbLogin, glbPassword);
		(void) processMessages (msgDesc);

		sleep (sleepTime);

	} /* end while loop */


	free (glbLogin);
	free (glbPassword);

	(void) processMessages (msgDesc);

	/*
	** send critical IK messages to IMS syslog
	*/
	(void)ims_msgIKSyslog (msgDesc, IK_logFile); 
	(void) IK_CloseSyslog();
	(void) ims_msgStructFree (msgDesc);
	unlink (IK_logFile);
	msgDesc = (IMS_MSG_STRUCT *)NULL;

	exit (0);

} /* end main program */



/*********************************************************************
**
** gracefull -  
**
**  this function handles signals
**
***********************************************************************/
static void gracefull ( int sig)
{
	/* 
	** add whatever else you need here
	*/

	kill (0, SIGUSR1);
	sigterm_flag = 1;

} /* end of gracefull */



/***************************************************************************
**
** runDown ()
**
** Cleanup and exit from program.
**
***************************************************************************/
static void runDown (int status)
{
	/*
	** Doesn't do a lot.
	*/

	exit (status);
} /* end of runDown */

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

	(void) fprintf (stderr, "\nims_disp usage:\n\n");
	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}
	(void) fprintf (stderr,"\n\n");
} /* end of usage */

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
	int i, number, invalid;
	char *Msg;

	/* Initialize */
	glbLogin = (char *)NULL;
	glbPassword = (char *)NULL;
	invalid = 1;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/
	
	/* Define port number if specified by user*/
	if ( (commands.sleepTime != (char *) NULL) &&
		  (strlen (commands.sleepTime) != 0) )
	{
		sleepTime = atoi (commands.sleepTime);
	}
	else
	{
		sleepTime = DEF_SLEEPTIME;
	}

	/* define the log directory path */
	if ( (commands.directory != (char *) NULL) &&
		  (strlen (commands.directory) != 0 ))
	{
		glbDirectory = commands.directory;
	}
	else
	{
		glbDirectory = DEF_DIRECTORY;
	}

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

} /* end of processCmdLine */

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

	while ((msgQueue = (IMS_MSG_QUEUE *) ims_msgQueueExtract (msgDesc)) 
				 != (IMS_MSG_QUEUE *)NULL)
	{
		/* Only log msgs with ERROR or FATAL severity level */
		if (msgQueue->severity <= IMS_ERROR)
		{
			ims_msg (msgDesc, msgQueue->severity, msgQueue->msg);
		}

		(void) ims_msgQueueFree (msgQueue);
		msgQueue = (IMS_MSG_QUEUE *)NULL;
	}

	return;

} /* end of processMessages */
