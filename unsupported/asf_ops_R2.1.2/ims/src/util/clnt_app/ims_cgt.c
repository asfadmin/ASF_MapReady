static char *sccs = "@(#)ims_cgt.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_cgt.c
**
** Function:    Creates the granules table for a specified dataset.
**
** Author:      K.Tembekjian
**
** Date:        5/29/90
**
** Modified:    5/4/93 - S. Hardman - V19.0
**              FR S1690 - Modified the getKeywordList() function to get the
**              length column from the keywordPolicy table in the catalog.
**              Modified the genTableCmd() function to default to a
**              length of 255 when zero is the length.
**
**              2/17/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Replaced a call to stripName() with cdb_extractFileName().
**              Replaced the password extraction calls with cdb_getPassword().
**              Added the include file ctype.h. Replaced the
**              include files sybfront.h and sybdb.h with cdb_dbms.h.
**
**              4/20/95 - D. Crichton - R1B
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              5/17/95 - D. Crichton - R1B
**              Update granules table to new ASF schema v3.00.
**
**              8/9/95 - S. Hardman - R1B
**              Update granules table to new ASF schema v3.20.
**              Also cleaned up some of the queries.
**
**				2/20/96 - D. Crichton - R1B'
**              Updated to include indexes for building the granules
**				tables.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <syslog.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <errno.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_signal.h>
#include <ims_version.h>

/*
** Definition of the dataset list structure.
*/
typedef struct datasetList 
{
   DBSMALLINT dataset_idx;
   DBCHAR dataset[IMS_COL80_LEN+1];
   DBCHAR granules_table[IMS_COL30_LEN+1];
   struct datasetList *next;
} DATASET_LIST;

/*
** Definitions of the keyword list structure.
*/
typedef struct keywordList
{
   DBCHAR keyword[IMS_COL30_LEN+1];
   DBSMALLINT data_type;
   DBSMALLINT max_len;
   DBSMALLINT significance;
   struct keywordList *next;
} KEYWORD_LIST;

/*
** Local Functions
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static DATASET_LIST *getDatasetList (IMS_QI_DESC_OBJ *);
static KEYWORD_LIST *getKeywordList (IMS_QI_DESC_OBJ *, DBSMALLINT, int *);
static int genTableCmd (char *, KEYWORD_LIST *);
static char *centerString (char *, int);
static IMS_QI_DESC_OBJ *openConnection (IMS_MSG_STRUCT *);
static int checkGranuleTable (IMS_QI_DESC_OBJ *, char *);
static int execCmd (IMS_QI_DESC_OBJ *);
static void freeKeywordList (KEYWORD_LIST *);
static void freeDatasetList (DATASET_LIST *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *platform;
	char *sensor;
	char *dataset;
	char *tableCheck;
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
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-L",           &commands.platform},
	{"+platform",    &commands.platform},
	{"-S",           &commands.sensor},
	{"+sensor",      &commands.sensor},
	{"-D",           &commands.dataset},
	{"+dataset",     &commands.dataset},
	{"-T",           &commands.tableCheck},
	{"+tableCheck",  &commands.tableCheck},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
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
	{"platform",    &commands.platform},
	{"sensor",      &commands.sensor},
	{"dataset",     &commands.dataset},
	{"tableCheck",  &commands.tableCheck},
	{"server",      &commands.server},
	{"database",    &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Conversion structure for converting keyword datatypes to 
** Sybase datatypes.  This is based on ASF schema.
*/
static char *kwdSybDatTypes[] =
{
   "",
   "tinyint",
   "smallint",
   "int",
   "real",
   "float",
   "char(1)",
   "char",
   "varchar",
   "datetime",
   "varchar"  /* DOY Time */
};

/*
** Global Variables
*/
static char cmdBuf[4096];  /* Command buffer used for all queries. */
static char *programName;
static char *username;
static char *password;
static char *platform = (char *) NULL;
static char *sensor = (char *) NULL;
static char *dataset = (char *) NULL;
static char *server = (char *) NULL;
static char *database = (char *) NULL;

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (
	int argc,
	char *argv[])

{
	DATASET_LIST *datasetList;
	DATASET_LIST *datasetPtr;
	KEYWORD_LIST *keywordList;
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN];	
	int status;
	time_t clock;
	struct utsname uname_info;    /* Structure for uname() */
	int noRowsFlag;
	int tableCheck;
	
	/*
	** Init variables
	*/
	qDesc = (IMS_QI_DESC_OBJ *) NULL;
	datasetList = (DATASET_LIST *) NULL;
	status = IMS_OK;

	/*
	** Get program and node names.
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
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Get the command line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
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
	** this file, then overlay all commands from the command-line, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if ((status = ims_getFileParms (commands.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}

		/*
		** Now, get command line arguments again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}
	}

	/*
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** See if we are only supposed to check.
	*/
	if (commands.tableCheck != (char *) NULL)
	{
		tableCheck = IMS_TRUE;
	}
	else
	{
		tableCheck = IMS_FALSE;
	}

	/*
	** Initialize the query descriptor and open the connection.
	*/
	if ((qDesc = openConnection (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open a connection to the database server.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Populate the dataset list.
	*/
	if ((datasetList = getDatasetList (qDesc)) == (DATASET_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain Dataset Information for '%s, %s, %s'.", 
			platform?platform:"<ALL>",
			sensor?sensor:"<ALL>",
			dataset?dataset:"<ALL>");
		(void) ims_qiFreeDesc (qDesc);
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** For each dataset try to create the associated granule table.
	*/
	datasetPtr = datasetList;
	while (datasetPtr != (DATASET_LIST *) NULL)
	{
		/* Check if granule table exists. */
		if ((checkGranuleTable (qDesc,
			datasetPtr->granules_table)) == IMS_TRUE)
		{
			/*
			** If we are just checking, generate a slightly different
			** message and proceed as normal.
			*/
			if (tableCheck == IMS_TRUE)
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"Granule table '%s' for dataset '%s' exists.",
					datasetPtr->granules_table, datasetPtr->dataset);
			}
			else  /* Generate normal message and continue. */
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"Could not create table '%s' for dataset '%s'. The table already exists.",
					datasetPtr->granules_table, datasetPtr->dataset);
			}
			datasetPtr = datasetPtr->next;
			continue;
		}
		else
		{
			/*
			** If we are just checking, generate a message and
			** continue to the next dataset in the list.
			*/
			if (tableCheck == IMS_TRUE)
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"The granule table for dataset '%s' does not exist.",
					datasetPtr->dataset);
				datasetPtr = datasetPtr->next;
				continue;
			}
		}

		/*
		** Get keyword info for this dataset_idx.
		*/
		noRowsFlag = IMS_FALSE;
		if (((keywordList = getKeywordList (qDesc, datasetPtr->dataset_idx,
			&noRowsFlag)) == (KEYWORD_LIST *) NULL) &&
			(noRowsFlag == IMS_FALSE))
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Could not create table '%s' for dataset '%s'. Could not obtain keyword list.",
				datasetPtr->granules_table, datasetPtr->dataset);
			datasetPtr = datasetPtr->next;
			status = IMS_ERROR;
			continue;
		}

		/*
		** Generate the table creation sql statement.
		*/
		if ((status = genTableCmd (datasetPtr->granules_table,
			keywordList)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Could not create table '%s' for dataset '%s'. Could not generate SQL command for table.",
				datasetPtr->granules_table, datasetPtr->dataset);
			freeKeywordList (keywordList);
			datasetPtr = datasetPtr->next;
			status = IMS_ERROR;
			continue;
		}

		/*
		** Execute the table generating sql command.
		*/
		if ((status = execCmd (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Could not create table '%s' for dataset '%s'. Could not execute SQL command for table.",
				datasetPtr->granules_table, datasetPtr->dataset);
			freeKeywordList (keywordList);
			datasetPtr = datasetPtr->next;
			status = IMS_ERROR;
			continue;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Created table '%s' for dataset '%s'.",
				datasetPtr->granules_table, datasetPtr->dataset);
		}

		freeKeywordList (keywordList);
		datasetPtr = datasetPtr->next;
	}

	/*
	** Terminate database server connection and free the 
	** query descriptor.
	*/
	freeDatasetList (datasetList);
	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred during creation of the granule tables.");
		exit (1);
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Granule table creation completed successfully.");
		exit (0);
	}
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
	IMS_MSG_STRUCT *msgDesc)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	int status;
	size_t argLength;

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

	/* platform */
	if (commands.platform != (char *) NULL)
	{
		/* Allow for a null entry. */
		if ((argLength = strlen (commands.platform)) != 0)
		{
			platform = commands.platform;
		}
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Platform: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		/* Allow for a null entry. */
		if ((argLength = strlen (inputBuffer)) != 0)
		{
			platform = malloc (argLength + 1);
			(void) strcpy (platform, inputBuffer);
		}
	}

	/* sensor */
	if (commands.sensor != (char *) NULL)
	{
		/* Allow for a null entry. */
		if ((argLength = strlen (commands.sensor)) != 0)
		{
			sensor = commands.sensor;
		}
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Sensor: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		/* Allow for a null entry. */
		if ((argLength = strlen (inputBuffer)) != 0)
		{
			sensor = malloc (argLength + 1);
			(void) strcpy (sensor, inputBuffer);
		}
	}

	/* dataset */
	if (commands.dataset != (char *) NULL)
	{
		/* Allow for a null entry. */
		if ((argLength = strlen (commands.dataset)) != 0)
		{
			dataset = commands.dataset;
		}
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Dataset: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		/* Allow for a null entry. */
		if ((argLength = strlen (inputBuffer)) != 0)
		{
			dataset = malloc (argLength + 1);
			(void) strcpy (dataset, inputBuffer);
		}
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

	return (IMS_OK);
}

/******************************************************************************
**
** getDatasetList ()
**
** This function takes the platform, sensor and an optional dataset
** and return the list of dataset info for the specified dataset.
** If dataset was supplied, then it returns info about that dataset.
**
******************************************************************************/

static DATASET_LIST *getDatasetList (
	IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	DATASET_LIST *datasetList;
	DATASET_LIST *currPtr;
	DATASET_LIST *prevPtr;
	char tempDataset[IMS_COL80_LEN+1];
	int status;
	int rowCount;
	
	/*
	** Variable initialization.
	*/
	msgDesc = qDesc->msgDesc;
	currPtr = prevPtr = (DATASET_LIST *) NULL;

	/*
	** Build the query in the command buffer.
	** We need two separate queries because the sensor value in
	** the catalog can be NULL.
	*/
	if (sensor != (char *) NULL)
	{
		(void) sprintf (cmdBuf,
			"select p.dataset_idx, dataset, granules_table \
			from dataset_policy p, dataset_relation m \
			where p.dataset_idx = m.dataset_idx \
			and m.platform like '%s' \
			and m.sensor = '%s' \
			and dataset like '%s' \
			order by dataset",
			platform ? platform : "%",
			sensor,
			dataset ? dataset : "%");
	}
	else
	{
		(void) sprintf (cmdBuf,
			"select p.dataset_idx, dataset, granules_table \
			from dataset_policy p, dataset_relation m \
			where p.dataset_idx = m.dataset_idx \
			and m.platform like '%s' \
			and dataset like '%s' \
			order by dataset",
			platform ? platform : "%",
			dataset ? dataset : "%");
	}

	/*
	** Process the result rows for this query.
	*/
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return ((DATASET_LIST *) NULL);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the DATASET_LIST structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (DATASET_LIST *) malloc
			((size_t) sizeof (DATASET_LIST))) ==
			(DATASET_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for DATASET_LIST structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return ((DATASET_LIST *) NULL);
		}

		/*
		** datasetList points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			datasetList = currPtr;
		}
		else
		{
			/*
			** lint: warning: variable may be used before set: prevList
			** We don't use this variable until the second time through, and
			** by that time it has been set.
			*/
			prevPtr->next = currPtr;
		}

		currPtr->next = (DATASET_LIST *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* dataset_idx */
		(void) memcpy (&(currPtr->dataset_idx), IMS_VALUE (qDesc, 0),
			IMS_VALUELENGTH (qDesc, 0));

		/* dataset */
		(void) memcpy (tempDataset, IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc,1));
		tempDataset[IMS_VALUELENGTH (qDesc,1)] = '\0';
		(void) strcpy (currPtr->dataset, ims_truncStr (tempDataset));

		/* granules_table */
		(void) memcpy (currPtr->granules_table, IMS_VALUE (qDesc, 2),
			IMS_VALUELENGTH (qDesc, 2));
		currPtr->granules_table[IMS_VALUELENGTH (qDesc, 2)] = '\0';

		prevPtr = currPtr;
	}

	/*
	** Check to see if any datasets were returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		return ((DATASET_LIST *) NULL);
	}

	return (datasetList);
}

/******************************************************************************
**
** getKeywordList ()
**
** Returns the keyword list and information for specified dataset index.
** It's OK to not have any keywords associated with the dataset.  We
** set the noRowsFlag to be TRUE in this case.
**
** FR S1690 - Get the length column from the keywordPolicy table in
** the catalog database.
**
******************************************************************************/

static KEYWORD_LIST *getKeywordList (
	IMS_QI_DESC_OBJ *qDesc,
	DBSMALLINT dataset_idx,
	int *noRowsFlag)
{
	IMS_MSG_STRUCT *msgDesc;
	KEYWORD_LIST *keywordList;
	KEYWORD_LIST *currPtr;
	KEYWORD_LIST *prevPtr;
	char tempKeyword[IMS_COL30_LEN+1];
	int status;
	int rowCount;
	
	/*
	** Variable initialization.
	*/
	msgDesc = qDesc->msgDesc;
	currPtr = prevPtr = (KEYWORD_LIST *) NULL;

	/*
	** Set up the command buffer with the query.
	*/
	(void) sprintf (cmdBuf,
		"select keyword, data_type, max_len, significance \
		from keyword_set s, keyword_policy p \
		where s.keyword_idx = p.keyword_idx \
		and dataset_idx = %d \
		order by position",
		dataset_idx);
	
	/*
	** Process the result rows for this query.
	*/
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return ((KEYWORD_LIST *) NULL);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the KEYWORD_LIST structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (KEYWORD_LIST *) malloc
			((size_t) sizeof (KEYWORD_LIST))) ==
			(KEYWORD_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for KEYWORD_LIST structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return ((KEYWORD_LIST *) NULL);
		}

		/*
		** datasetList points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			keywordList = currPtr;
		}
		else
		{
			/*
			** lint: warning: variable may be used before set: prevList
			** We don't use this variable until the second time through, and
			** by that time it has been set.
			*/
			prevPtr->next = currPtr;
		}

		currPtr->next = (KEYWORD_LIST *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* keyword */
		(void) memcpy (tempKeyword, IMS_VALUE (qDesc, 0),
			IMS_VALUELENGTH (qDesc, 0));
		tempKeyword[IMS_VALUELENGTH (qDesc, 0)] = '\0';
		(void) strcpy (currPtr->keyword, ims_truncStr (tempKeyword));

		/* data_type */
		(void) memcpy (&(currPtr->data_type), IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc, 1));

		/* max_len */
		(void) memcpy (&(currPtr->max_len), IMS_VALUE (qDesc, 2),
			IMS_VALUELENGTH (qDesc, 2));

		/* significance */
		(void) memcpy (&(currPtr->significance), IMS_VALUE (qDesc, 3),
			IMS_VALUELENGTH (qDesc, 3));

		prevPtr = currPtr;
	}

	/*
	** Check to see if any datasets were returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		*noRowsFlag = IMS_TRUE;
		return ((KEYWORD_LIST *) NULL);
	}

	return (keywordList);
}

/******************************************************************************
**
** genTableCmd ()
**
** Generates the sql statement to create the named keyword table
** based on the supplied keyword information list.
**
** FR S1690 - Default to a length of 255 when the length is zero.
**
******************************************************************************/

static int genTableCmd (
	char *table,
	KEYWORD_LIST *keywordList)
{
	static char cmd[5096];
	static char indexCmd[512];
	static char buf[BUFSIZ];
	KEYWORD_LIST *kwdListPtr1;
	int indexCount = 0;


	memset(indexCmd, 0, sizeof(indexCmd));
	
	/*
	** Generate the beginning of the command with the static columns.
	*/
	(void) sprintf (cmd, " create table %s (\
		granule_idx int,\
		dataset_idx smallint,\
		status smallint,\
		received_time datetime,\
		contributor varchar(30) null,\
		o_gdr tinyint,\
		format char(10),\
		name varchar(30),\
		name_stamp varchar(15) null,\
		version smallint null,\
		data_kbytes int null,\
		metadata_kbytes int null,\
		ims_visible_p char(1) default 'Y',\
		north_lat real null,\
		south_lat real null,\
		west_lon real null,\
		east_lon real null,\
		pole_included char(1) default ' ',\
		start_time datetime null,\
		end_time datetime null,", table);
	
	/*
	** Put a column specification in the command for each keyword.
	** If the keyword list is null, skip ahead to the indexes.
	*/
	if (keywordList != (KEYWORD_LIST *) NULL)
	{
		for (kwdListPtr1 = keywordList; kwdListPtr1; kwdListPtr1 = 
			kwdListPtr1->next)
		{
			/*
			** Add the column specification.
			*/
			(void) sprintf (buf, " %s %s", kwdListPtr1->keyword, 
				kwdSybDatTypes[kwdListPtr1->data_type]);
			(void) strcat (cmd, buf);
		
			/*
			** If its a character data type, include the length in the
			** column specification.  Do this for DOY datatype as well. 
			*/
			if ((kwdListPtr1->data_type == 7) || (kwdListPtr1->data_type == 8) ||
				(kwdListPtr1->data_type == 10))
			{
				/*
				** If the length is zero, default to 255.
				*/
	
				if (kwdListPtr1->max_len == 0)
				{
					(void) sprintf (buf, " (%d)", 255);
				}
				else
				{
					(void) sprintf (buf, " (%d)",
						kwdListPtr1->max_len);
				}
				(void) strcat (cmd, buf);
			}
	
			/*
			** Allow all keywords to have null.
			*/
			(void) strcat (cmd, " null,");

			if ((kwdListPtr1->significance != 2) && 
				(kwdListPtr1->significance != 3))
			{
				continue;

			}

			indexCount ++;

			/*
			** Add indexes.  Don't append comma to end of index list.
			** We will use up to 6.
			*/

			if (indexCount < 7) 
			{
				strcat(indexCmd,  kwdListPtr1->keyword);
				strcat(indexCmd,  ",");
			}

		}
	}

	if (strlen(indexCmd))
	{
		indexCmd[strlen(indexCmd) - 1] = ' ';
	}

	cmd[strlen (cmd) - 1] = ' ';
	(void) strcat (cmd, ")");

	/*
	** Add the index creation statements for the table.
	*/
	/* prim_key */
	(void) sprintf (buf,
		" create unique clustered index prim_key on %s (granule_idx)",
		table);
	(void) strcat (cmd, buf);

	/* key1 */
	(void) sprintf (buf,
		" create index key1 on %s (name)",
		table);
	(void) strcat (cmd, buf);

	/* key2 */
	(void) sprintf (buf,
		" create index key2 on %s (north_lat, south_lat, west_lon, east_lon)",
		table);
	(void) strcat (cmd, buf);

	/* key3 */
	(void) sprintf (buf,
		" create index key3 on %s (start_time, end_time)",
		table);
	(void) strcat (cmd, buf);

	/* key4 */
	(void) sprintf (buf,
		" create index key4 on %s (pole_included)",
		table);
	(void) strcat (cmd, buf);

	/* key5 */
	(void) sprintf (buf,
		" create index key5 on %s (status)",
		table);
	(void) strcat (cmd, buf);

	/* key6 */
	(void) sprintf (buf,
		" create index key6 on %s (ims_visible_p)",
		table);
	(void) strcat (cmd, buf);

	/* key7 */
	(void) sprintf (buf,
		" create index key7 on %s (ims_visible_p, north_lat, south_lat, west_lon, east_lon, start_time, end_time, pole_included, status)",
		table);
	(void) strcat (cmd, buf);

	/* Key8 - Keyword Index */

	if (indexCount > 0)
	{
		(void) sprintf (buf,
			" create index key8 on %s (%s)",
			table, indexCmd);
		(void) strcat (cmd, buf);
	}



	/*
	** Add the grant statement for the table.
	*/
	(void) sprintf (buf, " grant select on %s to public", table);
	(void) strcat (cmd, buf);

	/*
	** Copy the command to the global buffer.
	*/
	(void) strcpy (cmdBuf, cmd);

	return (IMS_OK);
}

/******************************************************************************
**
** openConnection ()
**
** Initializes the global query descriptor.
**
******************************************************************************/

static IMS_QI_DESC_OBJ *openConnection (
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;

	/*
	** Allocate the query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain a query descriptor.");
		return ((IMS_QI_DESC_OBJ *) NULL);
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

	IMS_SETCMD (qDesc, cmdBuf);

	IMS_SET_VERBOSE (qDesc, 10);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not login to the database server.");
		return ((IMS_QI_DESC_OBJ *) NULL);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	return (qDesc);
}

/******************************************************************************
**
** checkGranuleTable ()
**
** Checks if the table exists in current database.
**
******************************************************************************/

static int checkGranuleTable (
	IMS_QI_DESC_OBJ *qDesc,
	char *granules_table)
{
	int status;
	int foundFlag;
	
	/*
	** Set up the command buffer with the query.
	*/
	(void) sprintf (cmdBuf,
		"select name from sysobjects \
		where type = 'U' and name = '%s'",
		granules_table);
	
	/*
	** Process the result rows for this query.
	*/
	foundFlag = IMS_FALSE;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (IMS_FALSE);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/* Row returned means we found it. */
		foundFlag = IMS_TRUE;
	}
	
	return (foundFlag);
}

/******************************************************************************
**
** execCmd ()
**
** Executes the sql statement that returns no rows.
**
******************************************************************************/

static int execCmd (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** centerString ()
**
** A utility function which takes a string prepends it with spaces
** enough to center it within a given width. It's the caller's
** responsibility to free the allocated centered string.
**
******************************************************************************/

static char *centerString (
	char *str,
	int width)
{
	char *cp = str;
	int strLength = strlen (str);
	int pad = 0;

	if (strLength < width)
	{
		pad = (width - strLength) / 2;
		
		if (!(cp = (char *) malloc (sizeof (char) * (pad + strLength + 1))))
		{
			return ((char *) NULL);
		}
		else
		{
			(void) memset (cp, ' ', pad);
			(void) strncpy (cp + pad, str, strLength);
			*(cp + pad + strLength) = 0;
		}
	}

	return (cp);
}

/******************************************************************************
**
** freeKeywordList ()
**
** Free the KEYWORD_LIST structure.
**
******************************************************************************/
 
static void freeKeywordList (
	KEYWORD_LIST *currPtr)
{
	KEYWORD_LIST *nextPtr;
		  
	while (currPtr != (KEYWORD_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}

	return;
}

/******************************************************************************
**
** freeDatasetList ()
**
** Free the DATASET_LIST structure.
**
******************************************************************************/
 
static void freeDatasetList (
	DATASET_LIST *currPtr)
{
	DATASET_LIST *nextPtr;
		  
	while (currPtr != (DATASET_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}

	return;
}
