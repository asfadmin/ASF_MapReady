char *sccs = "@(#)ims_apsChangeUpdate.c	1.2 06/09/97";
/******************************************************************************
**
** File:        ims_apsChangeUpdate
**
** Function:    Update the sequence # mappings for the APS files.
**
** Author:      Dan Crichton	
**
** Date:        5/14/97
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
#include <ims_cmnQuery.h>
#include <ims_timeConv.h>


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
static int getGranulesTable (IMS_MSG_STRUCT *, char *, char *);
static int findRawSignal(IMS_MSG_STRUCT *, char *, char *, int, int, char *, char *, char *, char *);

/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
	char *username;
	char *password;
	char *logFilePath;
	char *logFileName;
	char *commandFile;
	char *server;
	char *database;
	char *logFlag;
	char *help;
	char *release;
	char *inputName;
	char *scanName;
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
	{"-i",      		&commands.inputName},
	{"+inputFile",		&commands.inputName},
	{"-s",      		&commands.scanName},
	{"+scanFile",		&commands.scanName}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from the command-file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",        &commands.username},
	{"password",        &commands.password},
	{"logFilePath",     &commands.logFilePath},
	{"logFileName",     &commands.logFileName},
	{"server",          &commands.server},
	{"database",        &commands.database},
	{"logFlag",         &commands.logFlag},
	{"inputFile",     	&commands.inputName},
	{"scanFile",      	&commands.scanName}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);


static struct platform_map
{
	char *acry;
	char *name;
} IMS_PLATFORM_MAP[] =
{
	{"A1", 	"ADEOS-1"},
	{"E1",	"ERS-1"},
	{"E2",	"ERS-2"},
	{"J1",	"JERS-1"},
	{"R1",	"RADARSAT-1"}
};

/*
** Global Variables
*/
IMS_QI_DESC_OBJ *qDesc;
static char *programName;
static char *username;
static char *password;
static int  startIdx;
static int  endIdx;
static char *server;
static char *database;
static int  updateFlag;
static char cmdBuf[255];
static char *inputName;
static char *scanName;

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
	char map_platform[IMS_COL15_LEN+1];
	char platform[IMS_COL15_LEN+1];
	char sensor[IMS_COL10_LEN+1];
	char new_sensor[IMS_COL10_LEN+1];
	char mode[IMS_COL10_LEN+1];
	char new_mode[IMS_COL10_LEN+1];
	char activity_id[IMS_COL10_LEN+1];
	char dataset[IMS_COL64_LEN+1];
	char granules_table[IMS_COL64_LEN+1];
	int i, rev, seq,  new_seq_dk;
	FILE *inpDesc;
	FILE *scanDesc;
	int updateFlag;

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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  ims_mapStatusUpdate"
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
	** Open the input file.
	*/
	if ((inpDesc = fopen(inputName, "r")) == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open input file '%s'", inputName);
		goto ERROR;
	}

	/*
	** Open the scan history file.
	*/
	if ((scanDesc = fopen(scanName, "a")) == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open scan file '%s'", scanName);
		goto ERROR;
	}

	/*         
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		goto ERROR;
	}	
	
	/*
	** Set-up server variables.
	*/
	IMS_SETUSER (qDesc, username);
	IMS_SETPSWD (qDesc, password);
	IMS_SETPROG (qDesc, programName);
	IMS_SETSERVER (qDesc, server);
	IMS_SETDBNAME (qDesc, database);
	IMS_SET_VERBOSE (qDesc, 10);

	/*
	** Login to the database server.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database.");
		goto ERROR;
	}

	/*
	** Set-up the message and error handlers.
	*/
	IMS_SET_USERDATA(qDesc);

	/*
	** Go through the entries in the input file and create the
	** map and update the appropriate tables.
	*/

	fscanf(inpDesc, "%s %s %s %d %d %s %s %s %d",
		platform, sensor, mode, &rev, &seq, activity_id, new_sensor,
		new_mode, &new_seq_dk);


	while (!feof(inpDesc))
	{

		/*
		** Map platform to full-name
		*/


		for (i = 0; i < 5; i++)
		{
			if (strncmp(platform, IMS_PLATFORM_MAP[i].acry, 2) == 0)
			{
				strcpy(map_platform, IMS_PLATFORM_MAP[i].name);		
			}
		}


		/*
		** See if the sequence # exists in the downlink table
		*/
		sprintf(dataset, "%s RAW SIGNAL SEGMENT", map_platform);

		if ((status = findRawSignal(msgDesc, platform, sensor, rev, seq,
			mode, activity_id, dataset, granules_table))
			 < IMS_WARNING)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not check %s for existence of record",
				dataset);
			goto ERROR;
		}

		/*
		** If the downlink did not exist ...
		*/

		if (status != IMS_WARNING)
		{
			/*
			** Skip because we didn't find a record!
			*/

			(void) ims_msg(msgDesc, IMS_WARNING,
				"Downlink does not exist for p=%s, s= %s, r = %d, seq = %d",
				platform, sensor, rev, seq);

			fscanf(inpDesc, "%s %s %s %d %d %s %s %s %d",
				platform, sensor, mode, &rev, &seq, activity_id, new_sensor,
				new_mode, &new_seq_dk);
			continue;

		}

		(void) ims_msg(msgDesc, IMS_INFO,
			"Updating tables with new sequence number %d \n p=%s s=%s r=%d s=%d",
			new_seq_dk, platform, sensor, rev, seq);


		/*
		** Update IMS tables.
		*/

		if (strcmp(platform, "A1"))
		{
			/*
			** Update Frames
			*/

			dataset[0] = '\0';
			for (i = 0; i < IMS_MAXPLATFORMMODES; i++)
			{
				if (strcmp (mode, IMS_PLATFORM_MODES[i].mode) == 0)
				{
					(void) strcpy (dataset, map_platform);
					(void) strcat (dataset, IMS_PLATFORM_MODES[i].dataset);
					(void) strcat (dataset, " FRAMES");
					break;
				}
			}

			status = updateDataset(msgDesc, 
				platform, sensor, dataset, rev, seq, new_seq_dk, mode, activity_id,
				&updateFlag);

			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, status,
					"Error updating sequence in dataset '%s'", dataset);
				goto ERROR;
			}

			/*
			** Update Scan Results File Metadata.
			*/

			sprintf(dataset, "%s SCAN RESULTS FILE", map_platform);

			status = updateDataset(msgDesc, 
				platform, sensor, dataset, rev, seq, new_seq_dk, mode, activity_id,
				&updateFlag);

			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, status,
					"Error updating sequence in dataset '%s'", dataset);
				goto ERROR;
			}

			/*
			** Write out the Scan updates to a history file.
			*/
			if (updateFlag == IMS_TRUE)
			{
				(void) fprintf (scanDesc, "%s %s %s %d %d %s %s %s %d\n",
					platform, sensor, mode, rev, seq, activity_id, sensor,
					mode, new_seq_dk);
			}
		}

		/*
		** Update Raw Signal Segment
		*/

		sprintf(dataset, "%s RAW SIGNAL SEGMENT", map_platform);
		strcpy(granules_table, dataset);


		status = updateDataset(msgDesc, 
			platform, sensor, dataset, rev, seq, new_seq_dk, mode, activity_id,
			&updateFlag);

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, status,
				"Error updating sequence in dataset '%s'", dataset);
			goto ERROR;
		}


		/*
		** Get next entry in input file.
		*/

		fscanf(inpDesc, "%s %s %s %d %d %s %s %s %d",
			platform, sensor, mode, &rev, &seq, activity_id, new_sensor,
			new_mode, &new_seq_dk);
	}



	/*
	** Clean-up.
	*/
	fclose(inpDesc);
	fclose(scanDesc);

	(void) ims_msg (msgDesc, IMS_INFO,
		"ims_apsChangeUpdate processing completed successfully.");

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (0);

ERROR:
	/*
	** Clean-up.
	*/
	fclose(inpDesc);
	fclose(scanDesc);

	(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_apsChangeUpdate failed.");

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

	server = getenv("IMS_SERVER");
	database = getenv("IMS_DB");

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

	/* Input File Name */
	if (commands.inputName != (char *) NULL)
	{
		inputName = commands.inputName;
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

		inputName = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputName, inputBuffer);
	}

	/* Scan File Name */
	if (commands.scanName != (char *) NULL)
	{
		scanName = commands.scanName;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Scan File: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		scanName = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (scanName, inputBuffer);
	}

	return (IMS_OK);
}   /* getArgInput */


/******************************************************************************
**
** updateDataset 
**
******************************************************************************/
int updateDataset(
	IMS_MSG_STRUCT *msgDesc, 
	char *platform,
	char *sensor,
	char *dataset,
	int rev, 
	int seq, 
	int new_seq,
	char *mode,
	char *activity_id,
	int *updateFlag)

{
	int status;
	int rowCount;
	char granules_table[IMS_COL64_LEN+1];

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	if (getGranulesTable (msgDesc, dataset, granules_table) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			   "Could not get granule table name.");
		return(IMS_ERROR);
	}

	/*
	** Update dataset to new sequence number
	*/

	sprintf(cmdBuf,
		"update %s set SEQUENCE = %d where "
		"PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d and "
		"SEQUENCE = %d and "
		"MODE = '%s' and  ACTIVITY_ID = '%s'", granules_table, new_seq,
		platform, sensor, rev, seq, mode, activity_id);
	qDesc->cmd = cmdBuf;
	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not update %s table.", granules_table);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

	}
	(void) ims_msg(msgDesc, IMS_INFO,
			"Updated %d Rows for dataset %s: %s", IMS_AFFECTED(qDesc), dataset, 
			cmdBuf);

	if (IMS_AFFECTED(qDesc) > 0)
	{
		*updateFlag = IMS_TRUE;
	}
	else
	{
		*updateFlag = IMS_FALSE;
	}

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
	char *dataset,
	char *granules_table)
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

	qDesc->cmd = cmdBuf;

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
** findRawSignal
**
******************************************************************************/

static int findRawSignal(
  IMS_MSG_STRUCT *msgDesc,
  char *platform,
  char *sensor,
  int rev,
  int seq,
	char *mode,
	char *activity_id,
	char *dataset,
  char *granules_table)
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

	if (getGranulesTable (msgDesc, dataset, granules_table) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			   "Could not get granule table name.");
		return(IMS_ERROR);
	}

	qDesc->cmd = cmdBuf;

	(void) sprintf (cmdBuf,
		"select name from %s where "
		"PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d and SEQUENCE = %d "
		"and MODE = '%s' and ACTIVITY_ID = '%s'",
		granules_table, platform, sensor, rev, seq, mode, activity_id);

	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not query granules table '%s'", granules_table);
			return(IMS_ERROR);
		}
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;
	}

	if (rowCount > 0)
	{
		/*
		** Found a duplicate...
		*/

		return(IMS_WARNING);	
	}

	return(IMS_OK);
}
