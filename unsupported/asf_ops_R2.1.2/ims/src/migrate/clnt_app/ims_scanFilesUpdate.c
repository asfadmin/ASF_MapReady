static char *sccs = "@(#)ims_scanFilesUpdate.c	1.2  06/27/97";
/******************************************************************************
**
** File:        ims_scanFilesUpdate.c
**
** Function:
**
** Author:      Sean Hardman
**
** Date:        5/20/97
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

/*
** Local Definitions.
*/
#define LINE_SIZE 255

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
** File specification structure definition
*/
typedef struct fileSpec
{
	char name[IMS_COL30_LEN+1];
	char path[IMS_COL255_LEN+1];
	struct fileSpec *next;
} FILE_SPEC;

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, LOG_SPEC *);
static int updateFile (IMS_MSG_STRUCT *, char *, char *, int);
static int getFileSpec (IMS_MSG_STRUCT *, char *, char *, char *, int, 
	int, char *, char *);
static int getGranulesTable (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *,
	char *);
static void freeFileSpec (FILE_SPEC *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *inputFile;
	char *fileName;
	char *sequence;
	char *logFilePath;
	char *logFileName;
	char *commandFile;
	char *server;
	char *database;
	char *logFlag;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-I",           &commands.inputFile},
	{"+inputFile",   &commands.inputFile},
	{"-F",           &commands.fileName},
	{"+fileName",    &commands.fileName},
	{"-S",           &commands.sequence},
	{"+sequence",    &commands.sequence},
	{"-L",           &commands.logFilePath},
	{"+logFilePath", &commands.logFilePath},
	{"-N",           &commands.logFileName},
	{"+logFileName", &commands.logFileName},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-l",           &commands.logFlag},
	{"+logFlag",     &commands.logFlag},
	{"-h",           &commands.help},
	{"+help",        &commands.help},
	{"-r",           &commands.release},
	{"+release",     &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"inputFile",   &commands.inputFile},
	{"fileName",    &commands.fileName},
	{"sequence",    &commands.sequence},
	{"logFilePath", &commands.logFilePath},
	{"logFileName", &commands.logFileName},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"logFlag",     &commands.logFlag}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Platform map definition.
*/
static struct platform_map
{
	char *acry;
	char *name;
} IMS_PLATFORM_MAP[] =
{
	{"A1", "ADEOS-1"},
	{"E1", "ERS-1"},
	{"E2", "ERS-2"},
	{"J1", "JERS-1"},
	{"R1", "RADARSAT-1"}
};

/*
** Global Variables
*/
static char *programName;
static char *username;
static char *password;
static char *inputFile;
static char *fileName;
static int sequence;
static char *server;
static char *database;
FILE_SPEC *fileSpec;

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
	FILE_SPEC *currPtr;
	char logFileSpec[IMS_PATH_LEN+1];
	char writeBuffer[100];
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int status;
	int fd;
	FILE *inputDesc;
	char nameWithExt[IMS_COL30_LEN+1];
	char platform_name[IMS_COL30_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	char platform[IMS_COL10_LEN+1];
	char sensor[IMS_COL10_LEN+1];
	char new_sensor[IMS_COL10_LEN+1];
	char mode[IMS_COL10_LEN+1];
	char new_mode[IMS_COL10_LEN+1];
	char activity_id[IMS_COL10_LEN+1];
	int rev;
	int sequence;
	int new_seq_dk;
	int i;

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
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc, &logInfo)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not process argument input.");
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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  ims_scanFilesUpdate  "
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
	** Check to see if we passed a file name in on the command line
	** for testing purposes.
	*/
	if (fileName != (char *) NULL)
	{
		/*
		** Update the input file.
		*/
		if ((status = updateFile (msgDesc, fileName, ".", sequence)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not update the file '%s'.",
				fileName);
			goto ERROR;
		}
	}
	else
	{
		/*
		** Open the input file.
		*/
		if ((inputDesc = fopen (inputFile, "r")) == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not open input file '%s'.", inputFile);
			status = IMS_ERROR;
			goto ERROR;
		}

		/*
		** Read the first line of the file.
		*/
		(void) fscanf (inputDesc, "%s %s %s %d %d %s %s %s %d",
			platform, sensor, mode, &rev, &sequence, activity_id, new_sensor,
			new_mode, &new_seq_dk);

		while (!feof (inputDesc))
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Updating Scan files based on platform '%s', sensor '%s', "
				"rev '%d', and new sequence '%d'.",
				platform, sensor, rev, new_seq_dk);
			
			/*
			** Determine the platform name.
			*/
			for (i = 0; i < 5; i++)
			{
				if (strncmp (platform, IMS_PLATFORM_MAP[i].acry, 2) == 0)
				{
					(void) strcpy (platform_name, IMS_PLATFORM_MAP[i].name);
				}
			}

			/*
			** Determine dataset name.
			*/
			(void) sprintf (dataset, "%s SCAN RESULTS FILE", platform_name);

			/*
			** Set the pointer to NULL for the fun of it.
			*/
			fileSpec = (FILE_SPEC *) NULL;

			/*
			** Get the file specifications.
			** This will populate the fileSpec structure.
			*/
			if ((status = getFileSpec (msgDesc, platform, sensor, dataset,
				rev, new_seq_dk, mode, activity_id)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, status,
					"Could not obtain the file specifications for the Scan Results "
					"record matching platform '%s', sensor '%s', rev '%d' and "
					"sequence '%d'.", platform, sensor, rev, new_seq_dk);
				(void) fclose (inputDesc);
				goto ERROR;
			}

			/*
			** Update all files returned.
			*/
			currPtr = fileSpec;
			while (currPtr != (FILE_SPEC *) NULL)
			{
				/*
				** Update the metadata file.
				*/
				(void) sprintf (nameWithExt, "%s.M", currPtr->name);

				if ((status = updateFile (msgDesc, nameWithExt, currPtr->path,
					new_seq_dk)) < IMS_OK)
				{
					(void) ims_msg (msgDesc, status,
						"Could not update the metadata file '%s'.",
						nameWithExt);
					freeFileSpec (fileSpec);
					(void) fclose (inputDesc);
					goto ERROR;
				}

				(void) ims_msg (msgDesc, IMS_INFO,
					"Updated metadata file '%s'.", nameWithExt);

				/*
				** Update the data file.
				*/
				(void) sprintf (nameWithExt, "%s.D", currPtr->name);

				if ((status = updateFile (msgDesc, nameWithExt, currPtr->path,
					new_seq_dk)) < IMS_OK)
				{
					(void) ims_msg (msgDesc, status,
						"Could not update the data file '%s'.",
						nameWithExt);
					freeFileSpec (fileSpec);
					(void) fclose (inputDesc);
					goto ERROR;
				}

				(void) ims_msg (msgDesc, IMS_INFO,
					"Updated data file '%s'.", nameWithExt);

				currPtr = currPtr->next;
			}

			freeFileSpec (fileSpec);

			/*
			** Read again.
			*/
			(void) fscanf (inputDesc, "%s %s %s %d %d %s %s %s %d",
				platform, sensor, mode, &rev, &sequence, activity_id, new_sensor,
				new_mode, &new_seq_dk);
		}
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"The file(s) were successfully updated.");

	(void) fclose (inputDesc);
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, status,
		"Updating the file(s) was not successful.");

	(void) ims_msgStructFree (msgDesc);
					  
	exit (1);
}

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
	(void) fprintf (stderr, "\n\nTermination of %s due to signal: %s (%d)\n\n",
		programName, ims_sigMsg (sig), sig);

	return (sig);
}

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

	return;
}

/******************************************************************************
**
** getArgInput ()
**
** Prompt for needed information not provided in the command-line
** and command-file.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc,
	LOG_SPEC *logInfo)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	int invalid;
	int i;
	int number;

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

	/* inputFile */
	if (commands.inputFile != (char *) NULL)
	{
		inputFile = commands.inputFile;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Input File: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		inputFile = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputFile, inputBuffer);
	}

	/* fileName */
	if (commands.fileName != (char *) NULL)
	{
		fileName = commands.fileName;
	}

	/* sequence */
	number = 0;
	if (commands.sequence != (char *) NULL)
	{
		if (ims_isInteger (commands.sequence) == IMS_TRUE)
		{
			number = (int) atoi (commands.sequence);
			if ((number < 1 ) || (number > 99))
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The parameter 'sequence' has a value of '%d', which "
					"is not in the range of '1' to '99'.",
					number);
				return (IMS_ERROR);
			}
			else
			{
				sequence = (int) number;
			}
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'sequence' must contain a valid integer "
				"value.");
			return (IMS_ERROR);
		}
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

	return (IMS_OK);
}

/******************************************************************************
**
** getFileSpec
**
******************************************************************************/

static int getFileSpec (
	IMS_MSG_STRUCT *msgDesc, 
	char *platform,
	char *sensor,
	char *dataset,
	int rev, 
	int new_seq,
	char *mode,
	char *activity_id)

{
	IMS_QI_DESC_OBJ *qDesc;
	FILE_SPEC *currPtr;
	FILE_SPEC *prevPtr;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	char granules_table[IMS_COL30_LEN+1];
	int rowCount;

	prevPtr = (FILE_SPEC *) NULL;

	/*         
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return (IMS_ERROR);
	}	
	
	IMS_SETUSER (qDesc, username);
	IMS_SETPSWD (qDesc, password);
	IMS_SETPROG (qDesc, programName);
	if (server != (char *) NULL)
		IMS_SETSERVER (qDesc, server);
	if (database != (char *) NULL)
		IMS_SETDBNAME (qDesc, database);
	IMS_SET_VERBOSE (qDesc, 10);

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database.");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	IMS_SET_USERDATA(qDesc);

	if (getGranulesTable (msgDesc, qDesc, dataset, granules_table) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			   "Could not get granule table name.");
		(void) ims_qiFreeDesc (qDesc);
		return(IMS_ERROR);
	}

	/*
	** Set up the command buffer.
	*/
	sprintf(qbuf,
		"select name, path from %s g, dataset_path_policy d "
		"where PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d and "
		"SEQUENCE = %d and MODE = '%s' and  ACTIVITY_ID = '%s' and "
		"g.dataset_idx = d.dataset_idx",
		granules_table,
		platform, sensor, rev, new_seq, mode, activity_id);

	qDesc->cmd = qbuf;
	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the FILE_SPEC structure.
		*/
		if ((currPtr = (FILE_SPEC *) malloc
			((size_t) sizeof (FILE_SPEC))) ==
			(FILE_SPEC *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FILE_SPEC structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			(void) ims_qiFreeDesc (qDesc);
			return (IMS_FATAL);
		}

		/*
		** fileSpec points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			fileSpec = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FILE_SPEC *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* name */
		(void) memcpy (currPtr->name, IMS_VALUE (qDesc, 0),
			IMS_VALUELENGTH (qDesc, 0));
		currPtr->name[IMS_VALUELENGTH (qDesc, 0)] = '\0';
		(void) ims_truncStr (currPtr->name);

		/* path */
		(void) memcpy (currPtr->path, IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc, 1));
		currPtr->path[IMS_VALUELENGTH (qDesc, 1)] = '\0';
		(void) ims_truncStr (currPtr->path);

		prevPtr = currPtr;
	}

	/*
	** Check to see if we got any rows.
	*/
	if (IMS_AFFECTED(qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Could not obtain any rows matching the criteria.");
	}
			
	(void) ims_qiFreeDesc (qDesc);

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
	IMS_QI_DESC_OBJ *qDesc, 
	char *dataset,
	char *granules_table)
{
	char qBuf[IMS_COL512_LEN+1];
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

	qDesc->cmd = qBuf;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (qBuf,
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
		if (status == IMS_ENDOFQUERY)
		{
			continue;
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

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** updateFile ()
**
******************************************************************************/

static int updateFile (
	IMS_MSG_STRUCT *msgDesc,
	char *fileName,
	char *path,
	int sequence)
{
	FILE *fpIn;
	FILE *fpOut;
	char fileSpec[IMS_PATH_LEN+1];
	char tempFileSpec[IMS_PATH_LEN+1];
	char lineBuf[LINE_SIZE+1];
	char *fileBuffer;
	char *bufPtr;
	char *ptr;
	char *seqPtr;
	char *equalPtr;
	char *tempPtr;
	char seqStr[10];
	int seqStrLen;
	int valueLen;
	int bufSize;
	int fileSize;
	int i;
	struct stat statBuf;

	/*
	** Build the file specification.
	*/
	ims_concatFilePath (fileSpec, path, fileName);

	/*
	** Open the input file.
	*/
	if ((fpIn = fopen (fileSpec, "r")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open the original file '%s'. %s",
			fileSpec, strerror (errno));
		return (IMS_ERROR);
	}

	/*
	** Determine the size of the file.
	*/
	if (stat (fileSpec, &statBuf))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the file size for file '%s'.",
			fileSpec);
		(void) fclose (fpIn);
		return (IMS_ERROR);
	}
	
	fileSize = statBuf.st_size;

	/*
	** Allocate space for the file buffer.
	*/
	if ((fileBuffer = (char *) malloc (fileSize+1)) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for the file buffer.");
		(void) fclose (fpIn);
		return (IMS_FATAL);
	}

	/*
	** Read the file into the buffer.
	*/
	bufPtr = fileBuffer;
	while ((ptr = fgets (lineBuf, LINE_SIZE, fpIn)) != (char *) NULL)
	{
		while (*ptr != '\0')
		{
			*bufPtr = *ptr;
			ptr++;
			bufPtr++;
		}
		*bufPtr = '\0';
	}

	/*
	** Close the file and determine the total size.
	*/
	(void) fclose (fpIn);
	bufSize = strlen (fileBuffer);

	if (bufSize != fileSize)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The file size does not match the number of bytes read from the file.");
		free (fileBuffer);
		return (IMS_ERROR);
	}

	/*
	** Search to the SEQUENCE string.
	*/
	if ((seqPtr = strstr (fileBuffer, "SEQUENCE")) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not find the 'SEQUENCE' string in the file buffer.");
		free (fileBuffer);
		return (IMS_ERROR);
	}

	/*
	** Search to the = string.
	*/
	if ((equalPtr = strstr (seqPtr, "=")) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not find the '=' string in the file buffer.");
		free (fileBuffer);
		return (IMS_ERROR);
	}

	/*
	** Determine the new value length.
	*/
	(void) sprintf (seqStr, "%d", sequence);
	seqStrLen = strlen (seqStr);

	/*
	** Determine the old value length.
	*/
	valueLen = 0;
	tempPtr = equalPtr+1;
	while (*tempPtr != '\n')
	{
		valueLen++;
		tempPtr++;
	}

	/*
	** Substitute the old value for the new value in the buffer.
	*/
	if (valueLen < seqStrLen)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The length of the new sequence value is too long for "
			"the available space.");
		free (fileBuffer);
		return (IMS_ERROR);
	}
	else if (valueLen > seqStrLen)
	{
		tempPtr = equalPtr+1;
		for (i = valueLen - seqStrLen; i > 0; i--)
		{
			*tempPtr = ' ';
			tempPtr++;
		}
		for (i = 0; i < seqStrLen; i++)
		{
			*tempPtr = seqStr[i];
			tempPtr++;
		}
	}
	else
	{
		tempPtr = equalPtr+1;
		for (i = 0; i < seqStrLen; i++)
		{
			*tempPtr = seqStr[i];
			tempPtr++;
		}
	}

	/*
	** Rename the old file.
	*/
	(void) sprintf (tempFileSpec, "%s_PMF_TEMP", fileSpec);
	if (rename (fileSpec, tempFileSpec) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not rename the old file '%s'. %s",
			fileSpec, strerror (errno));
		free (fileBuffer);
		return (IMS_ERROR);
	}

	/*
	** Open the new file.
	*/
	if ((fpOut = fopen (fileSpec, "w")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open the new file '%s'. %s",
			fileSpec, strerror (errno));
		free (fileBuffer);
		(void) rename (tempFileSpec, fileSpec);
		return (IMS_ERROR);
	}

	/*
	** Write the new file.
	*/
	if (fputs (fileBuffer, fpOut) != bufSize)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the new file '%s'.",
			fileSpec);
		(void) rename (tempFileSpec, fileSpec);
		(void) fclose (fpOut);
		return (IMS_ERROR);
	}

	/*
	** Change the mode of the file.
	*/
	if (chmod (fileSpec, 0755) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not change the mode of new file '%s'.",
			fileSpec);
		(void) rename (tempFileSpec, fileSpec);
		(void) fclose (fpOut);
		return (IMS_ERROR);
	}

	(void) fclose (fpOut);
	free (fileBuffer);

	return (IMS_OK);
}

/******************************************************************************
**
** freeFileSpec ()
**
** Free the FILE_SPEC structure.
**
******************************************************************************/

static void freeFileSpec (
	FILE_SPEC *currPtr)
{
	FILE_SPEC *nextPtr;

	while (currPtr != (FILE_SPEC *) NULL)
	{
		nextPtr = currPtr->next;
		(void) free ((char *) currPtr);
		currPtr = nextPtr;
	}
}
