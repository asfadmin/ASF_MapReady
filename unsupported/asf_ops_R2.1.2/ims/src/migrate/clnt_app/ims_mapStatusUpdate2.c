char *sccs = "@(#)ims_mapStatusUpdate2.c	1.1 07/17/97";
/******************************************************************************
**
** File:        ims_mapStatusUpdate2
**
** Function:    Checks downlink map and updates status for data migration
**
** Author:      Dan Crichton	
**
** Date:        5/1/97
** 
** Modified:	Dan Crichton	
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>

#define ZERO 0

/*
** Log File Information structure definition.
*/
typedef struct logInfo
{
	int logFlag;
	char *path;
	char *fileName;
} LOG_SPEC;

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, LOG_SPEC *);
static int openConnection (IMS_MSG_STRUCT *);
static int findDownlinkInfo(IMS_MSG_STRUCT *, char *, int, char *, char *,
int *, int *, char *, char *);
static int checkDownlink(IMS_MSG_STRUCT *, char *, char *, int, int, int *);
static int getGranulesTable (IMS_MSG_STRUCT *, char *);

/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
	char *username;
	char *password;
	char *dataset;
	char *startIdx;
	char *endIdx;
	char *logFilePath;
	char *logFileName;
	char *commandFile;
	char *server;
	char *database;
	char *logFlag;
	char *help;
	char *release;
	char *historyName;
} commands;

/*
** "Itemlist" for getting the above commands from the command-line.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",              &commands.username},
	{"+username",       &commands.username},
	{"-P",              &commands.password},
	{"+password",       &commands.password},
	{"-D",              &commands.dataset},
	{"+dataset",        &commands.dataset},
	{"-S",              &commands.startIdx},
	{"+startIdx",       &commands.startIdx},
	{"-E",              &commands.endIdx},
	{"+endIdx",         &commands.endIdx},
	{"-L",              &commands.logFilePath},
	{"+logFilePath",    &commands.logFilePath},
	{"-N",              &commands.logFileName},
	{"+logFileName",    &commands.logFileName},
	{"-C",              &commands.commandFile},
	{"+commandFile",    &commands.commandFile},
	{"-X",              &commands.server},
	{"+server",         &commands.server},
	{"-Y",              &commands.database},
	{"+database",       &commands.database},
	{"-l",              &commands.logFlag},
	{"+logFlag",        &commands.logFlag},
	{"-h",              &commands.help},
	{"+help",           &commands.help},
	{"-r",              &commands.release},
	{"+release",        &commands.release},
	{"-o",      		&commands.historyName},
	{"-outputFile",		&commands.historyName}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from the command-file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",        &commands.username},
	{"password",        &commands.password},
	{"dataset",         &commands.dataset},
	{"startIdx",        &commands.startIdx},
	{"endIdx",          &commands.endIdx},
	{"logFilePath",     &commands.logFilePath},
	{"logFileName",     &commands.logFileName},
	{"server",          &commands.server},
	{"database",        &commands.database},
	{"logFlag",         &commands.logFlag},
	{"outputFile",   	&commands.historyName},

};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
IMS_QI_DESC_OBJ *qDesc;
static char *programName;
static char *username;
static char *password;
static char *dataset;
static int  startIdx;
static int  endIdx;
static char *server;
static char *database;
static int  updateFlag;
static char cmdBuf[255];
static char *historyName;

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	LOG_SPEC logInfo;
	char logFileSpec[IMS_PATH_LEN+1];
	char writeBuffer[100];
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int fd;
	char granules_table[IMS_COL30_LEN+1];
	int granule_idx;
	char platform[IMS_COL15_LEN+1];
	char sensor[IMS_COL10_LEN+1];
	char mode[IMS_COL10_LEN+1];
	char activity_id[IMS_COL10_LEN+1];
	int rev, seq;
	FILE *fileDesc;
	int count;
	char log_msg[IMS_COL255_LEN+1];

	server = database = username = password = NULL;
	memset((void *) &commands, 0 , sizeof(commands));


	/*
	** Initialize variables.
	*/
	qDesc = (IMS_QI_DESC_OBJ *) NULL;
	(void) memset (&logInfo, 0, (size_t) sizeof (LOG_SPEC));

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
	(void) ims_msgProgramName (msgDesc, programName);
	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
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
	** Get the command-line arguments. The variable status will actually
	** contain the number of command-line arguments processed upon
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
	** Check to see if we got everything off of the command-line.
	*/
	if (status < argc)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Only %d out of the %d command-line arguments were processed.",
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
	** If there is a command-file present, then get any commands from
	** this file, then overlay all commands from the command-line, except
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
		** Now, get command-line arguments again to overlay file arguments.
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
	** Process the information from command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc, &logInfo)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Set-up the log file if requested.
	*/
	if (logInfo.logFlag == IMS_TRUE)
	{
		/*
		** Assemble the log file specification.
		*/
		ims_concatFilePath (logFileSpec, logInfo.path, logInfo.fileName);

		/*
		** Open the log file.
		*/
		if ((fd = open (logFileSpec, O_WRONLY|O_CREAT|O_APPEND)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not open log file '%s'. %s",
				logFileSpec, strerror (errno));
			goto ERROR;
		}

		/*
		** Change the mode of the log file.
		*/
		if (chmod (logFileSpec, 0644) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not change the mode for log file '%s'. %s",
				logFileSpec, strerror (errno));
			goto ERROR;
		}

		/*
		** Enable message writting to the log file.
		** Disable message writting to stderr.
		*/
		if ((status = ims_msgLogFileDesc (msgDesc, fd)) < IMS_OK)
		{
			goto ERROR;
		}
		(void) ims_msgStderrFlag (msgDesc, IMS_OFF);

		/*
		** Write a delimiting message to the log file.
		*/
		(void) sprintf (writeBuffer,
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  ims_mapStatusUpdate2"
			"<<<<<<<<<<<<<<<<<<<<<<\n\n");

		if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not write to log file '%s'. %s",
				logFileSpec, strerror (errno));
			goto ERROR;
		}
	}
	
	/*
	** Open a connection to the database server.
	*/
	if ((status = openConnection (msgDesc)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Check to see if the dataset passed in resembles a granules
	** table name.  If so use it, otherwise use the dataset name
	** to get the granules table name.
	*/
	if ((strncmp (dataset, "granules_", (size_t) 9) == 0) &&
		((int) strlen (dataset) <= IMS_COL30_LEN))
	{
		(void) strcpy (granules_table, dataset);
		(void) strcpy (dataset, "UNKNOWN");
	}
	else /* Query for the granules table name. */
	{
		if ((status = getGranulesTable (msgDesc, granules_table)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not get granule table name for dataset '%s'.",
				dataset);
			goto ERROR;
		}
	}

	(void) strcat (granules_table, "_old");

	/*
	** Open the history file.
	*/

	if ((fileDesc = fopen(historyName, "a")) == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open history file '%s'", historyName);
		goto ERROR;
	}

	/*
	** Display some informational messages.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"Starting check of downlink map  for granule_idx %d "
		"in granules table '%s' for dataset '%s'.",
		startIdx, granules_table, dataset);

	for (granule_idx = startIdx; granule_idx < endIdx+1; granule_idx++)
	{
		if ((status = findDownlinkInfo(msgDesc, granules_table,
			granule_idx, platform, sensor, &rev, &seq, mode, activity_id)) 
			< IMS_WARNING)
		{
			(void) ims_msg (msgDesc, status,
				"Error occurred getting information for granule_idx %d"
				" granules table '%s' for dataset '%s'.",
				granule_idx, granules_table, dataset);
			goto ERROR;
		}

		if (status == IMS_WARNING)
		{
			(void) ims_msg (msgDesc, status,
				"A granule record did not exist for index '%d' in "
				"granules table '%s' for dataset '%s'.",
				granule_idx, granules_table, dataset);
		}
		else
		{
			if (checkDownlink(msgDesc,
				platform, sensor, rev, seq, &count) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Error occurred in accessing downlink info for "
					"granule_idx %d granule table %s dataset %s",
					granule_idx, granules_table, dataset);
				goto ERROR;
			}

			if (count == 0)
			{
				(void) sprintf(log_msg,
					"%s %s %s %d %d %s\n", platform, sensor,
					mode, rev, seq, activity_id);
				fwrite(log_msg, 1, strlen(log_msg), fileDesc);
			}
		}
	}

	fclose(fileDesc);

	(void) ims_msg (msgDesc, IMS_INFO,
		"Ending the ims_mapStatusUpdate2 at index '%d' "
		"in granules table '%s' for dataset '%s'.",
		endIdx, granules_table, dataset);

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"ims_mapStatusUpdate2 processing completed successfully.");

	(void) ims_qiFreeDesc(qDesc);
	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_mapStatusUpdate2 failed.");

	if (qDesc != (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_qiFreeDesc (qDesc);
	}
	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (1);
}   /* main */

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
	LOG_SPEC *logInfo)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	int number;
	int invalid;
	int i;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		username = commands.username;
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

		username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (password, inputBuffer);
	}

	/* dataset */
	if (commands.dataset != (char *) NULL)
	{
		dataset = commands.dataset;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Dataset: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		dataset = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (dataset, inputBuffer);
	}

	/* startIdx */
	number = 0;
	if (commands.startIdx == (char *) NULL)
	{
		/* We expect a number. */
		do
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Starting Index: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			invalid = 0;
			for (i=0; inputBuffer[i]; i++)
			{
				if ((i == 0) && ((inputBuffer[i] == '-') ||
					(inputBuffer[i] == '+')))
				{
					continue;
				}
		
				if (! isdigit (inputBuffer[i]))
				{
					(void) printf ( 
						"Expecting numerical input. Try again.\n");
					invalid = 1;
					break;
				}
			}

			number = (int) atoi (inputBuffer);
			if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
				((number < ZERO) || (number > IMS_MAX_INT)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '%d' to '%d'. "
					"Try again.\n",
					ZERO, IMS_MAX_INT);
			}
		}while (invalid);

		startIdx = (int) number;
	}
	else if (ims_isInteger (commands.startIdx) == IMS_TRUE)
	{
		number = (int) atoi (commands.startIdx);
		if ((number < ZERO) || (number > IMS_MAX_INT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'startIdx' has a value of '%d', "
				"which is not in the range of '%d' to '%d'.",
				number, ZERO, IMS_MAX_INT);
			return (IMS_ERROR);
		}
		else
		{
			startIdx = (int) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'startIdx' must have a valid integer value.");
		return (IMS_ERROR);
	}

	/* endIdx */
	number = 0;
	if (commands.endIdx == (char *) NULL)
	{
		/* We expect a number. */
		do
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Ending Index: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			invalid = 0;
			for (i=0; inputBuffer[i]; i++)
			{
				if ((i == 0) && ((inputBuffer[i] == '-') ||
					(inputBuffer[i] == '+')))
				{
					continue;
				}
		
				if (! isdigit (inputBuffer[i]))
				{
					(void) printf ( 
						"Expecting numerical input. Try again.\n");
					invalid = 1;
					break;
				}
			}

			number = (int) atoi (inputBuffer);
			if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
				((number < ZERO) || (number > IMS_MAX_INT)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '%d' to '%d'. "
					"Try again.\n",
					ZERO, IMS_MAX_INT);
			}

			if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
				(number < startIdx))
			{
				invalid = 1;
				(void) printf (
					"Numerical input must be greater than or equal to the\n"
					"starting index of '%d'. Try again.\n",
					startIdx);
			}

		}while (invalid);

		endIdx = (int) number;
	}
	else if (ims_isInteger (commands.endIdx) == IMS_TRUE)
	{
		number = (int) atoi (commands.endIdx);
		if ((number < ZERO) || (number > IMS_MAX_INT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'endIdx' has a value of '%d', "
				"which is not in the range of '%d' to '%d'.",
				number, ZERO, IMS_MAX_INT);
			return (IMS_ERROR);
		}
		else if (number < startIdx)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'endIdx' has a value of '%d', "
				"which is not greater than or equal to the "
				"starting index of '%d'.",
				number, startIdx);
			return (IMS_ERROR);
		}
		else
		{
			endIdx = (int) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'endIdx' must have a valid integer value.");
		return (IMS_ERROR);
	}

	/* logFilePath */
	if (commands.logFilePath != (char *) NULL)
	{
		logInfo->path = commands.logFilePath;
	}
	else
	{
		logInfo->path = ".";
	}

	/* logFileName */
	if (commands.logFileName != (char *) NULL)
	{
		logInfo->fileName = commands.logFileName;
	}
	else
	{
		logInfo->fileName = malloc ((size_t) 9 + IMS_PROGRAM_LEN + 10 + 1);
		(void) sprintf (logInfo->fileName, "%s_%s.log",
			programName, ims_timeStamp ());
	}

	/* server */
	server = getenv("IMS_SERVER");
	if (commands.server != (char *) NULL)
	{
		server = commands.server;
	}

	/* database */
	database = getenv("IMS_DB");
	if (commands.database != (char *) NULL)
	{
		database = commands.database;
	}

	/* logFlag */
	if (commands.logFlag != (char *) NULL)
	{
		logInfo->logFlag = IMS_TRUE;
	}
	else
	{
		logInfo->logFlag = IMS_FALSE;
	}

	/* historyName */
	if (commands.historyName != (char *) NULL)
	{
		historyName = commands.historyName;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Output File: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		historyName = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (historyName, inputBuffer);
	}


	return (IMS_OK);
}   /* getArgInput */

/******************************************************************************
**
** openConnection ()
**
** Open a connection to the database server.
**
******************************************************************************/

static int openConnection (
	IMS_MSG_STRUCT *msgDesc)

{
	int status;

	/*
	** Allocate a query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return (IMS_FATAL);
	}

	/*
	** Setup the descriptor with necessary information about this
	** process.
	*/
	IMS_SETUSER (qDesc, username);

	IMS_SETPSWD (qDesc, password);

	IMS_SETPROG (qDesc, programName);

	if (server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, server);
	}

	if (database != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, database);
	}

	IMS_SET_VERBOSE (qDesc, 10);

	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not login to the database server.");
		return (status);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	return (IMS_OK);
}



/******************************************************************************
**
** findDownlinkInfo
**
** Find the downlink_entry information for the given downlink message.
**
******************************************************************************/

static int findDownlinkInfo(
  IMS_MSG_STRUCT *msgDesc,
	char *granule_name,
	int granule_idx,
	char *platform,
	char *sensor,
	int *rev,
	int *seq,
	char *mode,
	char *activity_id)
{
	int status;
	int rowCount;
	short int short_seq;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}


	(void) sprintf (cmdBuf,
    "select PLATFORM, SENSOR, REVOLUTION, SEQUENCE, MODE, ACTIVITY_ID\
		from %s where granule_idx = %d",
		granule_name, granule_idx);

	qDesc->cmd = cmdBuf;
	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of %s table.", granule_name);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		/*
		** Get platform, sensor, rev, seq...
		*/

		(void) memcpy((char *) platform, qDesc->valAddr[0],
				qDesc->valLength[0]);
		platform[qDesc->valLength[0]] = '\0';
		(void) memcpy((char *) sensor, qDesc->valAddr[1],
				qDesc->valLength[1]);
		sensor[qDesc->valLength[1]] = '\0';
		(void) memcpy((char *) rev, qDesc->valAddr[2],
				qDesc->valLength[2]);
		(void) memcpy((char *) &short_seq, qDesc->valAddr[3],
				qDesc->valLength[3]);
		(void) memcpy((char *) mode, qDesc->valAddr[4],
				qDesc->valLength[4]);
		mode[qDesc->valLength[4]] = '\0';
		(void) memcpy((char *) activity_id, qDesc->valAddr[5],
				qDesc->valLength[5]);
		activity_id[qDesc->valLength[5]] = '\0';

	}

	*seq = short_seq;

	if (rowCount == 0)
	{
		/*
		** Return an error since it was not found.
		*/
		return(IMS_WARNING);
	}

	return(IMS_OK);
}

/******************************************************************************
**
** checkDownlink
**
** Find the downlink in the downlink_entry table.  If it exists, then
** update the status of the downlink to ACQUIRED.  If it does not exist,
** then return and error.
**
******************************************************************************/

static int checkDownlink(
	IMS_MSG_STRUCT *msgDesc,
	char *platform,
	char *sensor,
	int rev,
	int seq,
	int *count)
{
	int status;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Set up appropriate command buffer.
	*/
	if (strcmp (sensor, "Z") == 0)
	{
		(void) sprintf (cmdBuf,
			"update downlink_entry set DOWNLINK_STATUS = 'ACQUIRED' where "
			"PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d and "
			"SEQUENCE = %d",
			platform, sensor, rev, seq);
	}
	else /* Any other sensor value. */
	{
		(void) sprintf (cmdBuf,
			"update downlink_entry set DOWNLINK_STATUS = 'ACQUIRED' "
			"from datatake_entry dt, downlink_entry dl "
			"where dt.PLATFORM = dl.PLATFORM "
			"and dt.SENSOR = dl.SENSOR "
			"and dt.REVOLUTION = dl.REVOLUTION "
			"and dt.SEQUENCE = dl.SEQUENCE "
			"and DT_PLATFORM = '%s' "
			"and DT_SENSOR = '%s' "
			"and DT_REVOLUTION = %d "
			"and DT_SEQUENCE = %d",
			platform, sensor, rev, seq);
	}

	qDesc->cmd = cmdBuf;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of downlink_entry table.");
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	*count = IMS_AFFECTED (qDesc);

	(void) ims_msg (msgDesc, IMS_INFO,
		"Updated %d rows in table downlink_entry: %s",
		IMS_AFFECTED (qDesc), cmdBuf);

	return(IMS_OK);
}

/******************************************************************************
**
** getGranulesTable ()
**
** Query the database server for the granule table name.
**
******************************************************************************/

static int getGranulesTable (
	IMS_MSG_STRUCT *msgDesc,
	char *granules_table)
{
	int status;
	short spatial_type;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select granules_table "
		"from dataset_policy d, dataset_relation r "
		"where r.dataset = '%s' and r.dataset_idx = d.dataset_idx",
		dataset);

	/*
	** Process the result row for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	/*
	** See if we got one row back.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		return (IMS_ERROR);
	}

	/*
	** Get the returned data.
	*/

	/* granules_table */
	(void) memcpy (granules_table,
		qDesc->valAddr[0], qDesc->valLength[0]);
	granules_table[qDesc->valLength[0]] = '\0';

	return (IMS_OK);
}

