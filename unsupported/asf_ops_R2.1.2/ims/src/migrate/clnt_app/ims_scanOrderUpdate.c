static char *sccs = "@(#)ims_scanOrderUpdate.c	1.1  06/27/97";
/******************************************************************************
**
** File:        ims_scanOrderUpdate.c
**
** Function:
**
** Author:      Sean Hardman
**
** Date:        6/12/97
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
** Log File Information structure definition.
*/
typedef struct logInfo
{
	int logFlag;
	char *path;
	char *fileName;
} LOG_SPEC;

/*
** Order specification structure definition.
*/
typedef struct orderSpec
{
	int order_id;
	int item_id;
	int dataset_idx;
	int granule_idx;
	char granule_name[IMS_COL30_LEN+1];
	struct orderSpec *next;
} ORDER_SPEC;

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, LOG_SPEC *);
static int updateFile (IMS_MSG_STRUCT *, char *, char *, int);
static int getOrderSpec (IMS_MSG_STRUCT *);
static int insertScanRecord (IMS_MSG_STRUCT *, ORDER_SPEC *);
static void freeOrderSpec (ORDER_SPEC *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
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
	char *insertFlag;
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
	{"-i",           &commands.insertFlag},
	{"+insertFlag",  &commands.insertFlag},
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
	{"logFilePath", &commands.logFilePath},
	{"logFileName", &commands.logFileName},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"logFlag",     &commands.logFlag},
	{"insertFlag",  &commands.insertFlag}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static IMS_QI_DESC_OBJ *qDesc;
char cmdBuf[IMS_COL512_LEN+1];
static char *programName;
static char *username;
static char *password;
static char *server;
static char *database;
static int insertFlag;
ORDER_SPEC *orderSpec;

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
	ORDER_SPEC *currPtr;
	char logFileSpec[IMS_PATH_LEN+1];
	char writeBuffer[100];
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int status;
	int fd;
	int i;

	/*
	** Initialize variables.
	*/
	qDesc = (IMS_QI_DESC_OBJ *) NULL;
	orderSpec = (ORDER_SPEC *) NULL;
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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  ims_scanOrderUpdate  "
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
	** Get the order item specifications.
	** This will populate the orderSpec structure.
	*/
	if ((status = getOrderSpec (msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not obtain the order item specifications.");
		goto ERROR;
	}

	/*
	** Informational message regarding insert status.
	*/
	if (insertFlag == IMS_FALSE)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Insertion of rows into the scan table has been disabled.");
	}

	/*
	** Process each order.
	*/
	currPtr = orderSpec;
	while (currPtr != (ORDER_SPEC *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Processing item '%d' of order '%d'.",
			currPtr->item_id, currPtr->order_id);

		/*
		** Insert a row into the scan table.
		*/
		if ((status = insertScanRecord (msgDesc, currPtr)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not insert a row into the scan table.");
			goto ERROR;
		}

		currPtr = currPtr->next;
	}

	/*
	** Clean up.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"The %s application completed successfully.", programName);

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);
	freeOrderSpec (orderSpec);

	exit (0);

ERROR:
	/*
	** Clean up.
	*/
	(void) ims_msg (msgDesc, status,
		"The %s application was not successful.", programName);

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);
	freeOrderSpec (orderSpec);
					  
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

	/* server */
	server = getenv ("IMS_SERVER");
	if (commands.server != (char *) NULL)
	{
		server = commands.server;
	}

	/* database */
	database = getenv ("IMS_DB");
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

	/* insertFlag */
	if (commands.insertFlag != (char *) NULL)
	{
		insertFlag = IMS_TRUE;
	}
	else
	{
		insertFlag = IMS_FALSE;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getOrderSpec ()
**
******************************************************************************/

static int getOrderSpec (
	IMS_MSG_STRUCT *msgDesc)

{
	ORDER_SPEC *currPtr;
	ORDER_SPEC *prevPtr;
	int status;
	int rowCount;
	short item_id_temp;
	short dataset_idx_temp;

	prevPtr = (ORDER_SPEC *) NULL;

	/*
	** Set up the command buffer.
	*/
	(void) sprintf (cmdBuf,
		"select order_id, item_id, dataset_idx, granule_idx, granule_name "
		"from order_item "
		"where status in (1,2) and order_item_type = 6 "
		"order by order_id, item_id");

	qDesc->cmd = cmdBuf;
	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the ORDER_SPEC structure.
		*/
		if ((currPtr = (ORDER_SPEC *) malloc
			((size_t) sizeof (ORDER_SPEC))) ==
			(ORDER_SPEC *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for ORDER_SPEC structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** orderSpec points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			orderSpec = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (ORDER_SPEC *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* order_id */
		(void) memcpy (&(currPtr->order_id), IMS_VALUE (qDesc, 0),
			IMS_VALUELENGTH (qDesc, 0));

		/* item_id */
		(void) memcpy (&(item_id_temp), IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc, 1));
		currPtr->item_id = (int) item_id_temp;

		/* dataset_idx */
		(void) memcpy (&(dataset_idx_temp), IMS_VALUE (qDesc, 2),
			IMS_VALUELENGTH (qDesc, 2));
		currPtr->dataset_idx = (int) dataset_idx_temp;

		/* granule_idx */
		(void) memcpy (&(currPtr->granule_idx), IMS_VALUE (qDesc, 3),
			IMS_VALUELENGTH (qDesc, 3));

		/* granule_name */
		(void) memcpy (currPtr->granule_name, IMS_VALUE (qDesc, 4),
			IMS_VALUELENGTH (qDesc, 4));
		currPtr->granule_name[IMS_VALUELENGTH (qDesc, 4)] = '\0';
		(void) ims_truncStr (currPtr->granule_name);

		prevPtr = currPtr;
	}

	/*
	** Check to see if we got any rows.
	*/
	if (IMS_AFFECTED(qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Could not obtain any Scan orders that were NEW or VALIDATED.");
	}
			
	return (IMS_OK);
}

/******************************************************************************
**
** insertScanRecord ()
**
** Query the database server for the granule table name.
**
******************************************************************************/

static int insertScanRecord (
	IMS_MSG_STRUCT *msgDesc,
	ORDER_SPEC *orderSpec)
{
	char granules_table[IMS_COL30_LEN+1];
	char dt_platform[IMS_COL2_LEN+1];
	char dt_sensor[IMS_COL1_LEN+1];
	int  dt_revolution;
	int  dt_sequence;
	char quicklook_flag[IMS_COL3_LEN+1];
	char mode[IMS_COL5_LEN+1];
	char frame_mode[IMS_COL10_LEN+1];
	char activity_id[IMS_COL10_LEN+1];
	char station_id[IMS_COL4_LEN+1];
	char time_on[IMS_DATETIME_LEN+1];
	char time_off[IMS_DATETIME_LEN+1];
	char site_name[IMS_COL30_LEN+1];
	short dt_sequence_temp;
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
	** Determine the name of the granules table.
	*/
	(void) sprintf (granules_table, "granules_%d", orderSpec->dataset_idx);

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select dt.DT_PLATFORM, dt.DT_SENSOR, dt.DT_REVOLUTION, dt.DT_SEQUENCE, "
		"dt.QUICKLOOK_FLAG, dt.MODE, dt.FRAME_MODE, dl.ACTIVITY_ID, "
		"dl.STATION_ID, dt.TIME_ON, dt.TIME_OFF, dt.SITE_NAME "
		"from %s g, datatake_entry dt, downlink_entry dl "
		"where g.granule_idx = %d "
		"and g.PLATFORM = dt.DT_PLATFORM "
		"and g.REVOLUTION = dt.DT_REVOLUTION "
		"and g.SEQUENCE = dt.DT_SEQUENCE "
		"and dt.PLATFORM = dl.PLATFORM "
		"and dt.SENSOR = dl.SENSOR "
		"and dt.REVOLUTION = dl.REVOLUTION "
		"and dt.SEQUENCE = dl.SEQUENCE",
		granules_table, orderSpec->granule_idx);

	qDesc->cmd = cmdBuf;

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
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Could not obtain matching granule information for item '%d' "
			"of order '%d': '%s'",
			orderSpec->item_id, orderSpec->order_id, cmdBuf);
		return (IMS_OK);
	}

	/*
	** See if we got more than one row back.
	*/
	if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Obtained more than one row of matching granule information for "
			"item '%d' of order '%d': '%s'",
			orderSpec->item_id, orderSpec->order_id, cmdBuf);
		return (IMS_OK);
	}

	/*
	** Get the returned data.
	*/

	/* DT_PLATFORM */
	(void) memcpy (dt_platform, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	dt_platform[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (dt_platform);

	/* DT_SENSOR */
	(void) memcpy (dt_sensor, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	dt_sensor[IMS_VALUELENGTH (qDesc, 1)] = '\0';
	(void) ims_truncStr (dt_sensor);

	/* DT_REVOLUTION */
	(void) memcpy (&(dt_revolution), IMS_VALUE (qDesc, 2),
		IMS_VALUELENGTH (qDesc, 2));

	/* DT_SEQUENCE */
	(void) memcpy (&(dt_sequence_temp), IMS_VALUE (qDesc, 3),
		IMS_VALUELENGTH (qDesc, 3));
	dt_sequence = (int) dt_sequence_temp;

	/* QUICKLOOK_FLAG */
	(void) memcpy (quicklook_flag, IMS_VALUE (qDesc, 4),
		IMS_VALUELENGTH (qDesc, 4));
	quicklook_flag[IMS_VALUELENGTH (qDesc, 4)] = '\0';
	(void) ims_truncStr (quicklook_flag);

	/* MODE */
	(void) memcpy (mode, IMS_VALUE (qDesc, 5),
		IMS_VALUELENGTH (qDesc, 5));
	mode[IMS_VALUELENGTH (qDesc, 5)] = '\0';
	(void) ims_truncStr (mode);

	/* FRAME_MODE */
	(void) memcpy (frame_mode, IMS_VALUE (qDesc, 6),
		IMS_VALUELENGTH (qDesc, 6));
	frame_mode[IMS_VALUELENGTH (qDesc, 6)] = '\0';
	(void) ims_truncStr (frame_mode);

	/* ACTIVITY_ID */
	(void) memcpy (activity_id, IMS_VALUE (qDesc, 7),
		IMS_VALUELENGTH (qDesc, 7));
	activity_id[IMS_VALUELENGTH (qDesc, 7)] = '\0';
	(void) ims_truncStr (activity_id);

	/* STATION_ID */
	(void) memcpy (station_id, IMS_VALUE (qDesc, 8),
		IMS_VALUELENGTH (qDesc, 8));
	station_id[IMS_VALUELENGTH (qDesc, 8)] = '\0';
	(void) ims_truncStr (station_id);

	/* TIME_ON */
	(void) memcpy (time_on, IMS_VALUE (qDesc, 9),
		IMS_VALUELENGTH (qDesc, 9));
	time_on[IMS_VALUELENGTH (qDesc, 9)] = '\0';
	(void) ims_truncStr (time_on);

	/* TIME_OFF */
	(void) memcpy (time_off, IMS_VALUE (qDesc, 10),
		IMS_VALUELENGTH (qDesc, 10));
	time_off[IMS_VALUELENGTH (qDesc, 10)] = '\0';
	(void) ims_truncStr (time_off);

	/* SITE_NAME */
	(void) memcpy (site_name, IMS_VALUE (qDesc, 11),
		IMS_VALUELENGTH (qDesc, 11));
	site_name[IMS_VALUELENGTH (qDesc, 11)] = '\0';
	(void) ims_truncStr (site_name);

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
		"select * from scan "
		"where order_id = %d and item_id = %d",
		orderSpec->order_id, orderSpec->item_id);

	qDesc->cmd = cmdBuf;

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
	** See if we inserted one row.
	*/
	if (IMS_AFFECTED (qDesc) >= 1)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"An entry already exists in the scan table for item '%d' "
			"of order '%d': '%s'",
			orderSpec->item_id, orderSpec->order_id, cmdBuf);
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

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"insert into scan "
		"(order_id, item_id, DT_PLATFORM, DT_SENSOR, DT_REVOLUTION, DT_SEQUENCE, "
		"QUICKLOOK_FLAG, MODE, FRAME_MODE, ACTIVITY_ID, "
		"STATION_ID, TIME_ON, TIME_OFF, SITE_NAME) "
		"values (%d, %d, '%s', '%s', %d, %d, "
		"'%s', '%s', '%s', '%s', "
		"'%s', '%s', '%s', '%s')",
		orderSpec->order_id, orderSpec->item_id,
		dt_platform, dt_sensor, dt_revolution, dt_sequence,
		quicklook_flag, mode, frame_mode, activity_id,
		station_id, time_on, time_off, site_name);

	qDesc->cmd = cmdBuf;

	/*
	** Only insert the row if the flag is set.
	*/
	if (insertFlag == IMS_TRUE)
	{
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
		** See if we inserted one row.
		*/
		if (IMS_AFFECTED (qDesc) != 1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not insert granule information for item '%d' "
				"of order '%d': '%s'",
				orderSpec->item_id, orderSpec->order_id, cmdBuf);
			return (IMS_ERROR);
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Successfully inserted granule information for item '%d' "
				"of order '%d': '%s'",
				orderSpec->item_id, orderSpec->order_id, cmdBuf);
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Would have inserted granule information for item '%d' "
			"of order '%d': '%s'",
			orderSpec->item_id, orderSpec->order_id, cmdBuf);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** freeOrderSpec ()
**
** Free the ORDER_SPEC structure.
**
******************************************************************************/

static void freeOrderSpec (
	ORDER_SPEC *currPtr)
{
	ORDER_SPEC *nextPtr;

	while (currPtr != (ORDER_SPEC *) NULL)
	{
		nextPtr = currPtr->next;
		(void) free ((char *) currPtr);
		currPtr = nextPtr;
	}
}
