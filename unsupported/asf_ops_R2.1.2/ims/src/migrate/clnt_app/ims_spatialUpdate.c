static char *sccs = "@(#)ims_spatialUpdate.c	1.1  21 Oct 1996";
/******************************************************************************
**
** File:        ims_spatialUpdate.c
**
** Function:    This program will update the spatial metadata query values
**              for an existing record in a granules table.  The dataset
**              associated with the granules table must have a spatial
**              type of quadrilateral.
**
** Author:      Sean Hardman
**
** Date:        10/3/96
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
#include <ims_signal.h>
#include <ims_childHandler.h>

/*
** Local Definitions.
*/
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
static int getGranulesTable (IMS_MSG_STRUCT *, char *);
static int updateGranuleRecord (IMS_MSG_STRUCT *, char *, int);
static int setTransState (IMS_MSG_STRUCT *, char *);

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
	char *updateFlag;
	char *help;
	char *release;
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
	{"-u",              &commands.updateFlag},
	{"+updateFlag",     &commands.updateFlag},
	{"-h",              &commands.help},
	{"+help",           &commands.help},
	{"-r",              &commands.release},
	{"+release",        &commands.release}
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
	{"updateFlag",      &commands.updateFlag}

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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  Spatial Update Startup  "
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

	/*
	** Display some informational messages.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"Starting the spatial metadata update at index '%d' "
		"in granules table '%s' for dataset '%s'.",
		startIdx, granules_table, dataset);

	if (updateFlag == IMS_FALSE)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"The record update flag has been turned off so "
			"instead of updating the records the new spatial "
			"metadata will be displayed.");
	}

	/*
	** Calculate and update the spatial metadata for each
	** granule in the range.
	*/
	for (granule_idx = startIdx; granule_idx < endIdx+1; granule_idx++)
	{
		if ((status = updateGranuleRecord (msgDesc, granules_table,
			granule_idx)) < IMS_WARNING)
		{
			(void) ims_msg (msgDesc, status,
				"Could not update the granule record for index '%d' "
				"in granules table '%s' for dataset '%s'.",
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
	}

	(void) ims_msg (msgDesc, IMS_INFO,
		"Ending the spatial metadata update at index '%d' "
		"in granules table '%s' for dataset '%s'.",
		endIdx, granules_table, dataset);

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"Spatial update processing completed successfully.");

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"Spatial update processing failed.");

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
	if (commands.server != (char *) NULL)
	{
		server = commands.server;
	}

	/* database */
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

	/* updateFlag */
	if (commands.updateFlag != (char *) NULL)
	{
		updateFlag = IMS_FALSE;
	}
	else
	{
		updateFlag = IMS_TRUE;
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
		"select granules_table, spatial_type "
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

	/* spatial_type */
	(void) memcpy (&spatial_type,
		qDesc->valAddr[1], qDesc->valLength[1]);

	/*
	** Make sure the spatial type is quadrilateral.
	*/
	if (spatial_type != (short) 4)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The dataset '%s' does not have a spatial type "
			"of Quadrilateral.", dataset);
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** updateGranuleRecord ()
**
** Update the spatial query metadata.
**
******************************************************************************/

static int updateGranuleRecord (
	IMS_MSG_STRUCT *msgDesc,
	char *granules_table,
	int granule_idx)
{
	char asc_desc[3];
	float near_start_lat, near_start_lon, near_end_lat, near_end_lon;
	float far_start_lat, far_start_lon, far_end_lat, far_end_lon;
	float lat_array[4];
	float lon_array[4];
	float min_lat, max_lat;
	float min_lon, max_lon;
	char pole_included;
	int status;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (IMS_ERROR);
	}

	/*
	** Begin the transaction.
	*/
	if ((status = setTransState (msgDesc, "begin")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not begin the transaction.");
		return (IMS_ERROR);
	}

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select ASC_DESC, "
		"NEAR_START_LAT, NEAR_START_LON, NEAR_END_LAT, NEAR_END_LON, "
		"FAR_START_LAT, FAR_START_LON, FAR_END_LAT, FAR_END_LON "
		"from %s "
		"where granule_idx = %d",
		granules_table, granule_idx);

	/*
	** Process the result row for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) setTransState (msgDesc, "rollback");
			return (IMS_ERROR);
		}
	}

	/*
	** See if we got one row back.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) setTransState (msgDesc, "rollback");
		return (IMS_WARNING);
	}

	/*
	** Get the returned data.
	*/

	/* asc_desc */
	(void) memcpy (asc_desc,
		qDesc->valAddr[0], qDesc->valLength[0]);
	asc_desc[qDesc->valLength[0]] = '\0';

	/* near_start_lat */
	(void) memcpy (&near_start_lat,
		qDesc->valAddr[1], qDesc->valLength[1]);

	/* near_start_lon */
	(void) memcpy (&near_start_lon,
		qDesc->valAddr[2], qDesc->valLength[2]);

	/* near_end_lat */
	(void) memcpy (&near_end_lat,
		qDesc->valAddr[3], qDesc->valLength[3]);

	/* near_end_lon */
	(void) memcpy (&near_end_lon,
		qDesc->valAddr[4], qDesc->valLength[4]);

	/* far_start_lat */
	(void) memcpy (&far_start_lat,
		qDesc->valAddr[5], qDesc->valLength[5]);

	/* far_start_lon */
	(void) memcpy (&far_start_lon,
		qDesc->valAddr[6], qDesc->valLength[6]);

	/* far_end_lat */
	(void) memcpy (&far_end_lat,
		qDesc->valAddr[7], qDesc->valLength[7]);

	/* far_end_lon */
	(void) memcpy (&far_end_lon,
		qDesc->valAddr[8], qDesc->valLength[8]);

	/*
	** Setup the lat and lon arrays.
	*/
	if (strncmp (asc_desc, "A", (size_t) 1) == 0)
	{
		lat_array[0] = near_end_lat;
		lat_array[1] = far_end_lat;
		lat_array[2] = far_start_lat;
		lat_array[3] = near_start_lat;

		lon_array[0] = near_end_lon;
		lon_array[1] = far_end_lon;
		lon_array[2] = far_start_lon;
		lon_array[3] = near_start_lon;
	}
	else if (strncmp (asc_desc, "D", (size_t) 1) == 0)
	{
		lat_array[0] = far_start_lat;
		lat_array[1] = near_start_lat;
		lat_array[2] = near_end_lat;
		lat_array[3] = far_end_lat;

		lon_array[0] = far_start_lon;
		lon_array[1] = near_start_lon;
		lon_array[2] = near_end_lon;
		lon_array[3] = far_end_lon;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Unknown ASC_DESC value. '%s'.", asc_desc);
		(void) setTransState (msgDesc, "rollback");
		return (IMS_ERROR);
	}

	/*
	** Perform spatial metadata calculation.
	*/
	if ((status = v0_spatial__v0_calcb (&lat_array[0], &lon_array[0],
		&min_lat, &max_lat, &min_lon, &max_lon, &pole_included)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occurred calculating the spatial metadata.");
		(void) setTransState (msgDesc, "rollback");
		return (IMS_ERROR);
	}

	/*
	** Check the update flag to see if we should update the record.
	*/
	if (updateFlag == IMS_TRUE)
	{
		/*
		** Reset the query descriptor.
		*/
		if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not reset the query descriptor.");
			(void) setTransState (msgDesc, "rollback");
			return (IMS_ERROR);
		}

		/*
		** Populate the command buffer with the SQL statement.
		*/
		(void) sprintf (cmdBuf,
			"update %s set "
			"north_lat = %f, south_lat = %f, "
			"west_lon = %f, east_lon = %f, pole_included = '%c' "
			"where granule_idx = %d",
			granules_table, max_lat, min_lat, min_lon, max_lon,
			pole_included, granule_idx);
	
		/*
		** Process the row for this update.
		*/
		while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"An error occurred updating the spatial metadata.");
				(void) setTransState (msgDesc, "rollback");
				return (IMS_ERROR);
			}
		}

		/*
		** See if we updated one row.
		*/
		if (IMS_AFFECTED (qDesc) < 1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The row was not updated.");
			(void) setTransState (msgDesc, "rollback");
			return (IMS_ERROR);
		}
	}
	else
	{
		/*
		** If we are not updating let's print out the new
		** spatial metadata values.
		*/
		(void) fprintf (stderr,
			"\nMetadata values for index '%d' in granules table '%s':"
			"\n\tnorth_lat = '%f'\n\tsouth_lat = '%f'"
			"\n\twest_lon = '%f'\n\teast_lon = '%f'\n\tpole_included = '%c'\n",
			granule_idx, granules_table,
			max_lat, min_lat, min_lon, max_lon, pole_included);
	}

	/*
	** Commit the transaction.
	*/
	if ((status = setTransState (msgDesc, "commit")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not commit the transaction.");
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** setTransState ()
**
** Set the state of the transaction.
**
******************************************************************************/

static int setTransState (
	IMS_MSG_STRUCT *msgDesc,
	char *transType)
{
	int status;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf, "%s transaction", transType);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Execute the command.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	return (IMS_OK);
}
