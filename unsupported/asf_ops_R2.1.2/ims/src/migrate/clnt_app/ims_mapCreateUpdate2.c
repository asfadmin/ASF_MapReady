static char *sccs = "@(#)ims_mapCreateUpdate2.c	1.1 07/17/97";
/******************************************************************************
**
** File:        ims_mapCreateUpdate2
**
** Function:    Create the data entry mappings
**
** Author:      Dan Crichton	
**
** Date:        5/5/97: Cinco de Mayo
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
static int createMapEntry(IMS_MSG_STRUCT *, char *, char *, int, int, int, int, char *, char *, char *);

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
  char *updateFlag;
	char *help;
	char *release;
	char *historyName;
	char *inputName;
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
  {"-u",              &commands.updateFlag},
	{"+updateFlag",     &commands.updateFlag},
	{"-h",              &commands.help},
	{"+help",           &commands.help},
	{"-r",              &commands.release},
	{"+release",        &commands.release},
	{"-o",      		&commands.historyName},
	{"+outputFile",		&commands.historyName},
	{"-i",      		&commands.inputName},
	{"+inputFile",		&commands.inputName}
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
  {"updateFlag",      &commands.updateFlag},
	{"outputFile",   	&commands.historyName},
	{"inputFile",   	&commands.inputName}
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
static char *historyName;
static char *inputName;

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
	char mode[IMS_COL10_LEN+1];
	char activity_id[IMS_COL10_LEN+1];
	char dataset[IMS_COL64_LEN+1];
	char granules_table[IMS_COL64_LEN+1];
	int i, rev, seq, new_seq_dt, new_seq_dk;
	FILE *fileDesc, *inpDesc;

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
	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  ims_mapCreateUpdate2  "
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
	** Open the history file.
	*/
	if ((fileDesc = fopen(historyName, "a")) == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open history file '%s'", historyName);
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
	** Informational message regarding insert status.
	*/
	if (updateFlag == IMS_FALSE)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Inserting rows has been disabled.");
	}

	/*
	** Go through the entries in the input file and create the
	** map and update the appropriate tables.
	*/

	fscanf(inpDesc, "%s %s %s %d %d %s",
		platform, sensor, mode, &rev, &seq, activity_id);

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
		** Make sure record actually exists before going forward.
		*/


		sprintf(dataset, "%s RAW SIGNAL SEGMENT", map_platform);

		if ((status = findRawSignal(msgDesc, platform, sensor, rev, seq,
			mode, activity_id, dataset, granules_table)) < IMS_WARNING)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not check dataset %s for existence of record",
				dataset);
			goto ERROR;
		}

		if (status != IMS_WARNING)
		{
			/*
			** Skip because we didn't find a record!
			*/

			(void) ims_msg(msgDesc, IMS_WARNING,
				"RAW SIGNAL ENTRY does not exist for p=%s, s= %s, r = %d, seq = %d",
				platform, sensor, rev, seq);

			fscanf(inpDesc, "%s %s %s %d %d %s",
				platform, sensor, mode, &rev, &seq, activity_id);
			continue;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Processing entry: platform: %s, sensor: %s, revolution: %d, "
			"sequence: %d", platform, sensor, rev, seq);

		sprintf(dataset, "%s RAW SIGNAL SEGMENT", map_platform);
		strcpy(granules_table, dataset);

		status = createMapEntry(msgDesc, 
			platform, sensor, rev, seq, seq, seq,  
			mode, activity_id, granules_table);

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, status,
				"Error creating map entry in table '%s'", granules_table);
			goto ERROR;
		}

		/*
		** Write out history to output file based on APS.
		*/
		fprintf(fileDesc, "%s %s %s %d %d %s %s %s %d\n",
			platform, sensor, mode, rev, seq, activity_id, sensor,
			mode, seq);

		/*
		** Get next entry in input file.
		*/
		fscanf(inpDesc, "%s %s %s %d %d %s",
			platform, sensor, mode, &rev, &seq, activity_id);
	}

	/*
	** Clean-up.
	*/
	fclose(inpDesc);
	fclose(fileDesc);

	(void) ims_msg (msgDesc, IMS_INFO,
		"ims_mapCreateUpdate2 processing completed successfully.");

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);

	(void) close (fd);
	exit (0);

ERROR:
	/*
	** Clean-up.
	*/
	fclose(inpDesc);
	fclose(fileDesc);

	(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_mapCreateUpdate2 failed.");

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

	/* updateFlag */
	if (commands.updateFlag != (char *) NULL)
	{
		updateFlag = IMS_TRUE;
	}
	else
	{
		updateFlag = IMS_FALSE;
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
** createMapEntry
**
** Create a map entry in the downlink_entry table.
**
******************************************************************************/

static int createMapEntry(
  IMS_MSG_STRUCT *msgDesc,
	char *platform,
	char *sensor,
	int rev,
	int seq,
	int new_seq_dk,
	int new_seq_dt,
	char *mode,
	char *activity_id,
	char *dataset)
{

	int status;
	int rowCount;
	char station_id[IMS_COL15_LEN+1];
	char antenna_id[IMS_COL15_LEN+1];
	char transmitter_id[IMS_COL15_LEN+1];
	char fa_schedule_link[IMS_COL15_LEN+1];
	char schedule_link[IMS_COL15_LEN+1];
	char time_on[IMS_COL30_LEN+1];
	char time_off[IMS_COL30_LEN+1];
	char time_aos[IMS_COL30_LEN+1];
	char time_los[IMS_COL30_LEN+1];
	char quick_look[5];
	char granules_table[IMS_COL64_LEN+1];
	char process_auth[4];
	int days, msecs;
	int ins_seq_dk = 0;
	int ins_seq_dt = 0;
	char ins_sensor_dt[IMS_COL2_LEN+1];

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
	** Check to see if this already exists.
	*/
	(void) sprintf (cmdBuf,
		"select DOWNLINK_STATUS from downlink_entry "
		"where PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d "
		"and SEQUENCE = %d",
		platform, sensor, rev, seq);

	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get the downlink information for "
				"platform = '%s' rev = '%d' seq = '%d'",
				platform, rev, seq);
			return(IMS_ERROR);
		}
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	if (IMS_AFFECTED (qDesc) >= 1)
	{
		(void) ims_msg(msgDesc, IMS_INFO,
			"The downlink map entry already exists: platform = '%s' sensor = '%s' "
			"rev = '%d' seq = '%d'",
			platform, sensor, rev, seq);
		return (IMS_OK);
	}

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
			   "Could not get granule table name for dataset '%s'.", dataset);
		return(IMS_ERROR);
	}

	(void) strcat (granules_table, "_old");

	qDesc->cmd = cmdBuf;

	/*
	** Get the granule information
	*/
	(void) sprintf (cmdBuf,
		"select 'FA', ANTENNA_ID, TRANSMITTER_ID, FA_SCHEDULE_LINK, "
		"TIME_ON, TIME_OFF, TIME_AOS, TIME_LOS, QUICKLOOK_FLAG from "    
		"%s where PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d "
		"and SEQUENCE = %d and MODE = '%s' and ACTIVITY_ID = '%s'",
		granules_table, platform, sensor, rev, seq, mode, activity_id);
	

	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get the granule table information for "
				"platform = '%s' rev = '%d' seq = '%d'",
				platform, rev, seq);
			return(IMS_ERROR);
		}
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		/*
		** Get the returned columns for each row...
		*/

		/*
		** STATION_ID
		*/

		(void) memcpy (station_id,
			qDesc->valAddr[0], qDesc->valLength[0]);
		station_id[qDesc->valLength[0]] = '\0';			
		ims_trim(station_id);

		/*
		** ANTENNA_ID
		*/

		(void) memcpy (antenna_id,
			qDesc->valAddr[1], qDesc->valLength[1]);
		antenna_id[qDesc->valLength[1]] = '\0';			
		ims_trim(antenna_id);

		/*
		** TRANSMITTER_ID
		*/ 

		(void) memcpy (transmitter_id,
			qDesc->valAddr[2], qDesc->valLength[2]);
		transmitter_id[qDesc->valLength[2]] = '\0';			
		ims_trim(transmitter_id);

		/*
		** FA_SCHEDULE_LINK
		*/
		(void) memcpy (fa_schedule_link,
			qDesc->valAddr[3], qDesc->valLength[3]);
		fa_schedule_link[qDesc->valLength[3]] = '\0';			
		ims_trim(fa_schedule_link);


		/*
		** TIME_ON
		*/
		(void) memcpy (time_on,
			qDesc->valAddr[4], qDesc->valLength[4]);
		time_on[qDesc->valLength[4]] = '\0';			
		ims_trim(time_on);

		/*
		** TIME_OFF
		*/
		(void) memcpy (time_off,
			qDesc->valAddr[5], qDesc->valLength[5]);
		time_off[qDesc->valLength[5]] = '\0';			
		ims_trim(time_off);

		/*
		** TIME_AOS
		*/
		(void) memcpy (time_aos,
			qDesc->valAddr[6], qDesc->valLength[6]);
		time_aos[qDesc->valLength[6]] = '\0';			
		ims_trim(time_aos);

		/*
		** TIME_LOS
		*/
		(void) memcpy (time_los,
			qDesc->valAddr[7], qDesc->valLength[7]);
		time_los[qDesc->valLength[7]] = '\0';			
		ims_trim(time_los);


		/*
		** QUICKLOOK_FLAG
		*/
		(void) memcpy (quick_look,
			qDesc->valAddr[8], qDesc->valLength[8]);
		quick_look[qDesc->valLength[8]] = '\0';			
		ims_trim(quick_look);
	}

	if (rowCount == 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not find granule information for "
			"platform = '%s' rev = '%d' seq = '%d'",
			platform, rev, seq);
		return(IMS_ERROR);

	}

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
	** Determine whether the sequence number should be used for the 
	** downlink or the datatake.
	*/
	if ((strcmp (platform, "E1") == 0) || (strcmp (platform, "E2") == 0))
	{
		ins_seq_dk = seq;
		ins_seq_dt = seq - 1;
	}
	
	if (((strcmp (platform, "R1") == 0) || (strcmp (platform, "J1") == 0)) &&
		(strcmp (activity_id, "RLT") == 0))
	{
		ins_seq_dk = seq;
		ins_seq_dt = seq - 1;
	}

	if (((strcmp (platform, "R1") == 0) || (strcmp (platform, "J1") == 0)) &&
		(strcmp (activity_id, "DMP") == 0))
	{
		ins_seq_dk = seq;
		ins_seq_dt = seq + 1;
	}

	if (strcmp (platform, "A1") == 0)
	{
		ins_seq_dk = seq;
		ins_seq_dt = seq + 1;
	}

	if ((ins_seq_dk == 0) || (ins_seq_dt == 0))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"One of the sequence values is zero: DL: %d, DT: %d",
			ins_seq_dk, ins_seq_dt);
		return (IMS_ERROR);
	}

	/*
	** Set-up the command buffer.
	*/
	(void) sprintf (cmdBuf,
		"insert into downlink_entry (PLATFORM, SENSOR, REVOLUTION, SEQUENCE,"
		"DOWNLINK_STATUS, ACTIVITY_ID, STATION_ID, ANTENNA_ID, TRANSMITTER_ID,"
		"FA_SCHEDULE_LINK, TIME_ON, TIME_OFF, TIME_AOS, TIME_LOS,"
		"NUMBER_OF_DTK_ENTRY, received_time) values ('%s', 'Z', %d, %d,"
		"'ACQUIRED','%s','%s','%s','%s','%s','%s','%s','%s','%s',1, getdate())",
		platform, rev, ins_seq_dk, activity_id, station_id,
		antenna_id, transmitter_id, fa_schedule_link, time_on, time_off,
		time_aos, time_los);

	rowCount = 0;

	if (updateFlag == IMS_TRUE)
	{
		while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not update downlink_entry");
				return(IMS_ERROR);
			}
			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}

			rowCount ++;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Inserted %d rows of downlink information accordingly: %s",
			IMS_AFFECTED(qDesc), cmdBuf);
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Would have updated downlink information accordingly: %s",
			cmdBuf);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	rowCount = 0;

	/*
	** Setup Process Auth Flag.
	*/

	if (platform[0] == 'A')
	{
		strcpy(process_auth, "NO");
	}
	else
	{
		strcpy(process_auth, "YES");
	}

	(void) sprintf (cmdBuf,
		"insert into datatake_entry (PLATFORM, SENSOR, REVOLUTION, SEQUENCE,"
		"DT_SENSOR, DT_REVOLUTION, DT_SEQUENCE, DT_PLATFORM,"
		"QUICKLOOK_FLAG, PROCESS_AUTH_FLAG, MODE, FRAME_MODE," 
		"TIME_ON, TIME_OFF, SITE_NAME, updated_time) "
		"values ('%s', 'Z', %d, %d, 'S', %d, %d, '%s', "
		"'%s','%s','%s','%s','%s','%s','%s',getdate()) ",
		platform, rev, ins_seq_dk, rev, ins_seq_dt, platform,
		quick_look, process_auth, mode, "ARCTIC", time_on, time_off, "");

	if (updateFlag == IMS_TRUE)
	{
		while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not update datatake_entry");
				return(IMS_ERROR);
			}
			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}

			rowCount ++;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Inserted %d rows of datatake information accordingly: %s",
			IMS_AFFECTED(qDesc), cmdBuf);
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Would have updated datatake information accordingly: %s",
			cmdBuf);
	}

	return(IMS_OK);
}

/******************************************************************************
**
** findRawSignal
**
** 
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

	(void) strcat (granules_table, "_old");

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
