static char *sccs = "@(#)ims_svQueryEx.c	5.2  01/07/97";
/******************************************************************************
**
** File:        ims_svQueryEx.c
**
** Function:    Perform queries on state vector data as requested
**              by the caller.
**
** Author:      Dan Crichton
**
** Date:        5/1/95
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_query.h>
#include <ims_cmd.h>
#include <ims_version.h>
#include <ims_signal.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, IMS_CMN_QUERY *);
static int checkQueryStatus (IMS_MSG_STRUCT *, IMS_CMN_QUERY *, int);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *platform;
	char *precision;
	char *startTime;
	char *endTime;
	char *listResultsFlag;
	char *commandFile;
	char *server;
	char *database;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",                  &commands.username},
	{"+username",           &commands.username},
	{"-P",                  &commands.password},
	{"+password",           &commands.password},
	{"-L",                  &commands.platform},
	{"+platform",           &commands.platform},
	{"-S",                  &commands.startTime},
	{"+startTime",          &commands.startTime},
	{"-E",                  &commands.endTime},
	{"+endTime",            &commands.endTime},
	{"-R",                  &commands.precision},
	{"+precision",          &commands.precision},
	{"-F",                  &commands.listResultsFlag},
	{"+listResultsFlag",    &commands.listResultsFlag},
	{"-C",                  &commands.commandFile},
	{"+commandFile",        &commands.commandFile},
	{"-X",                  &commands.server},
	{"+server",             &commands.server},
	{"-Y",                  &commands.database},
	{"+database",           &commands.database},
	{"-h",                  &commands.help},
	{"+help",               &commands.help},
	{"-r",                  &commands.release},
	{"+release",            &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",        &commands.username},
	{"password",        &commands.password},
	{"platform",        &commands.platform},
	{"startTime",       &commands.startTime},
	{"endTime",         &commands.endTime},
	{"precision",       &commands.precision},
	{"listResultsFlag", &commands.listResultsFlag},
	{"server",          &commands.server},
	{"database",        &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *listResultsString;
static char *platform;
static char precision;
static char *startTime;
static char *endTime;
static int listResultsFlag;

/******************************************************************************
**
** main ()
**
** This is the driver program for the ims_svQuery() function.
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	IMS_MSG_QUEUE *msgQueue;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	IMS_CMN_QUERY *query;
	IMS_SV_STRUCT retbuf;
	IMS_SV_STRUCT *pnt_retbuf;
	IMS_SV_STRUCT *pnt_retbuf2;

	/*
	** Get the program name and the node name.
	*/
	programName = ims_extractFileName (argv[0]);
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

	/*
	** Allocate message facility structure.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (1);
	}

	/*
	** Initialize the message facility options.
	*/
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgStderrFlag (msgDesc, IMS_OFF);
	(void) ims_msgQueueFlag (msgDesc, IMS_ON);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/*
	** Initialize the signal handler.
	*/
	if (ims_setWrapup (runDown) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Initialization of the signal handler failed. %s.",
			strerror (errno));
		goto ERROR;
	}

	/*
	** Get the command line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occurred parsing the command-line.");
		goto ERROR;
	}

	/*
	** Check to see if we got everything off of the command line.
	*/
	if (status < argc)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
		"Only %d out of the %d command line arguments were processed.",
			status, argc);
	}

	/*
	** If release was specified, print it out.
	*/
	if (commands.release != (char *) NULL)
	{
		(void) ims_printVersion (stderr);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the commandline, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if ((status = ims_getFileParms (commands.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-file.");
			goto ERROR;
		}

		/*
		** Now, get command line arguments again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-line.");
			goto ERROR;
		}
	}

	/*
	** Allocate space for the IMS_CMN_QUERY structure.
	*/
	if ((query = (IMS_CMN_QUERY *) malloc (
		(size_t) sizeof (IMS_CMN_QUERY))) ==
		(IMS_CMN_QUERY *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for the IMS_CMN_QUERY structure.");
		goto ERROR;
	}

	/*
	** Initialize common query structure members.
	*/
	(void) strcpy (query->program, programName);
	query->msgDesc = msgDesc;

	/*
	** Process the information from command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc, query)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Set the precision value.
	*/
	if ((listResultsString[0] == 'y') ||
		(listResultsString[0] == 'Y'))
	{
		listResultsFlag = IMS_TRUE;
	}
	else
	{
		listResultsFlag = IMS_FALSE;
	}

	/*
	** Open the database server connection.
	*/
	if ((status = ims_openQueryConnection (query)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not open a database server connection.");
		goto ERROR;
	}

	/*
	** Perform the query.
	*/
	query->retPtr = (char *) &retbuf;
	if ((status = ims_svQuery (query, platform, precision,
		startTime, endTime, listResultsFlag)) < IMS_OK)
	{
		(void) checkQueryStatus (msgDesc, query, status);
		goto ERROR;
	}

	/*
	** Close the database server connection.
	*/
	if ((status = ims_closeQueryConnection (query)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not close database server connection.");
		goto ERROR;
	}

	/*
	** Print out the results.
	*/
	pnt_retbuf = &retbuf;
	while (pnt_retbuf != (IMS_SV_STRUCT *) NULL)
	{
		(void) printf ("\nQuery Results\n");
		(void) printf ("rev = %s\n", pnt_retbuf->rev);
		(void) printf ("date/time = %s\n", pnt_retbuf->date);
		(void) printf ("coord sys = %s\n", pnt_retbuf->coord_sys);
		(void) printf ("x position = %s\n", pnt_retbuf->x_pos);
		(void) printf ("y position = %s\n", pnt_retbuf->y_pos);
		(void) printf ("z position = %s\n", pnt_retbuf->z_pos);
		(void) printf ("x velocity = %s\n", pnt_retbuf->x_vel);
		(void) printf ("y velocity = %s\n", pnt_retbuf->y_vel);
		(void) printf ("z velocity = %s\n\n", pnt_retbuf->z_vel);
		pnt_retbuf = pnt_retbuf->next;
	}

	/*
	** Free the results.
	*/
	pnt_retbuf = &retbuf;
	while (pnt_retbuf != (IMS_SV_STRUCT *) NULL)
	{
		pnt_retbuf2 = pnt_retbuf->next;
		free (pnt_retbuf);
		pnt_retbuf = pnt_retbuf2;
	}

	if (query != (IMS_CMN_QUERY *) NULL)
	{
		free (query);
	}

	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	if (query != (IMS_CMN_QUERY *) NULL)
	{
		if (query->qDesc != (IMS_QI_DESC_OBJ *) NULL)
		{
			(void) ims_closeQueryConnection (query);
		}
		free (query);
	}

	(void) ims_msg (msgDesc, status,
		"Could not obtain State Vector data.");

	/*
	** Extract the queued messages.
	*/
	while ((msgQueue = ims_msgQueueExtract (msgDesc)) !=
		(IMS_MSG_QUEUE *) NULL)
	{
		(void) fprintf (stderr, "%s\n", msgQueue->msg);
		(void) ims_msgQueueFree (msgQueue);
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);

	exit (1);
}   /*  main   */

/******************************************************************************
**
** runDown ()
**
** Cleanup and exit from program.
**
******************************************************************************/

static int runDown (
	int sig)
{
	/* Print out the signal caught. */
	(void) fprintf (stderr,
		"\n\nTermination of %s due to signal: %s (%d)\n\n",
		programName, ims_sigMsg (sig), sig);

	return (sig);
}   /*  runDown  */

/******************************************************************************
**
** usage ()
**
** Print command line argument switches.
**
******************************************************************************/

static void usage (void)
{
	int i;

	(void) fprintf (stderr,
		"\n%s command-line arguments:\n\n", programName);

	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}

	(void) fprintf (stderr, "\n\n");
}   /*  usage  */

/******************************************************************************
**
** getArgInput ()
**
** Process command-line and command-file arguments.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc,
	IMS_CMN_QUERY *query)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	long  i,k;


	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		(void) strcpy (query->username, commands.username);
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Username: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (query->username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		(void) strcpy (query->password, commands.password);
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (query->password, inputBuffer);
	}

	/* platform */
	if (commands.platform != (char *) NULL)
	{
		platform = commands.platform;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Platform: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		platform = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (platform, inputBuffer);
	}

	/* precison */
	if (commands.precision != (char *) NULL)
	{
		precision = commands.precision[0];
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Precision: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		precision = inputBuffer[0];
	}

	/* startTime */
	if (commands.startTime != (char *) NULL)
	{
		startTime = commands.startTime;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Start Time: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		startTime = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (startTime, inputBuffer);
	}

	/* endTime */
	if (commands.endTime != (char *) NULL)
	{
		endTime = commands.endTime;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"End Time: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		endTime = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (endTime, inputBuffer);
	}

	/* listResultsFlag */
	if (commands.listResultsFlag != (char *) NULL)
	{
		listResultsString = commands.listResultsFlag;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"List Results Flag (y|n): ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		listResultsString = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (listResultsString, inputBuffer);
		
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		(void) strcpy (query->server, commands.server);
	}

	/* database */
	if (commands.database != (char *) NULL)
	{
		(void) strcpy (query->database, commands.database);
	}

	return (IMS_OK);
}   /*  getArgInput */

/******************************************************************************
**
** checkQueryStatus ()
**
******************************************************************************/

static int checkQueryStatus (
	IMS_MSG_STRUCT *msgDesc,
	IMS_CMN_QUERY *query,
	int status)
{
	switch (query->retStatus)
	{
		case IMS_OK:
			(void) ims_msg (msgDesc, status,
				"Error is not query related.");
			break;

		case IMS_NOCONNECT:
			(void) ims_msg (msgDesc, status,
				"Error due to connection failure.");
			break;

		case IMS_DEADLOCK:
			(void) ims_msg (msgDesc, status,
				"Error due to query deadlock.");
			break;

		case IMS_NOROWS:
			(void) ims_msg (msgDesc, status,
				"Error due to no rows returned.");
			break;

		case IMS_QUERYFAIL:
			(void) ims_msg (msgDesc, status,
				"Error due to query failure.");
			break;

		default:
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Unknown query return status.");
			return (IMS_ERROR);
	}

	return (IMS_OK);
}   /*  checkQueryStatus   */
