static char *sccs = "@(#)ims_cleaner.c	5.1  03/17/96";

/******************************************************************************
**
** File:        ims_cleaner.c
**
** Function:    This application will search through the dataset and granules
**              tables to determine all the products which are marked for 
**              delete.  The product files will be removed and the status
**              will be changed to deleted.
**
** Author:      Dan Crichton
**
** Date:        1/3/96
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
#include <ims_childHandler.h>

/*
** Local Definitions.
*/
#define DIR_NAME_LEN (size_t)30

/*
** Items Table Information - We may want to get this from the 
** catalog at some point.
*/


#define MFD_VALUE 		7
#define DELETE_VALUE 	8
#define L0_PRODUCT		0
#define L1_PRODUCT		1
#define L2_PRODUCT		2
#define L3_PRODUCT		2

#define PMF_FILE_TYPE	1 

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
** User Information structure definition.
*/
typedef struct userSpec
{
	char *username;
	char *password;
	char *server;
	char *database;
	char *program;
} USER_SPEC;

/*
** Path List Information Structure Definition.
*/

typedef struct Path_List
{
	char path[IMS_COL255_LEN+1];
	int start_granule_idx;
	int end_granule_idx;
	struct Path_List *next;
} PATH_LIST;

/*
** Ext List Information Structure Definition.
*/

typedef struct Ext_List
{
	char format[IMS_COL10_LEN+1];
	short int type;
	char extension[IMS_COL10_LEN+1];
    struct Ext_List *next;
} EXT_LIST;

/*
** Dataset List Information Structure Definition.
*/

typedef struct Dataset_List
{
	short int dataset_idx;
	char granules_table[IMS_COL30_LEN+1];
	char version_p;
	short int process_level;
	PATH_LIST *pathList;
	EXT_LIST *extList;
	struct Dataset_List *next;
} DATASET_LIST;


/*
** Granule list Information Structure Definition.
*/
typedef struct granule_list
{
	int granule_idx;
	struct granule_list *next;
} GRANULE_LIST;


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
	{"logFilePath",     &commands.logFilePath},
	{"logFileName",     &commands.logFileName},
	{"server",          &commands.server},
	{"database",        &commands.database},
	{"logFlag",         &commands.logFlag}

};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;


/*
** Local Functions
*/

static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, USER_SPEC *, LOG_SPEC *);
static int cacheDatasetInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, DATASET_LIST **);
static int closeConnection (IMS_QI_DESC_OBJ *);
static int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **, USER_SPEC *);
static int updateGranuleStatus(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, DATASET_LIST *, GRANULE_LIST *);
static int removeProductFiles(IMS_MSG_STRUCT *, DATASET_LIST *, char *, char *, int, short int);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int removeMFDFiles(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, DATASET_LIST *);
static int freeCacheData(IMS_MSG_STRUCT *, DATASET_LIST *);

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
	USER_SPEC userSpec;
	char logFileSpec[IMS_PATH_LEN+1];
	char writeBuffer[100];
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int fd;
	IMS_QI_DESC_OBJ *qDesc;
	DATASET_LIST *datasetInfo;

	/*
	** Initialize variables.
	*/
	(void) memset (&logInfo, 0, (size_t) sizeof (LOG_SPEC));
	(void) memset (&userSpec, 0, (size_t) sizeof (USER_SPEC));

	/*
	** Get the program name and the node name.
	*/
	programName = ims_extractFileName (argv[0]);
	userSpec.program = programName;
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
	if ((status = getArgInput (msgDesc, &userSpec, &logInfo)) < IMS_OK)
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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  IMS CLEANER STARTUP  <<<<<<<<<<<<<<<<<<<<<<\n\n");

		if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not write to log file '%s'. %s",
				logFileSpec, strerror (errno));
			goto ERROR;
		}
	}
	
	/*
	** Okay, go do the work...
	*/

	if (openConnection(msgDesc, &qDesc, &userSpec) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Cache the policy dataset, path, and file information.
	*/

	if (cacheDatasetInfo(msgDesc, qDesc, &datasetInfo) < IMS_OK)
	{
		closeConnection(qDesc);
		goto ERROR;
	}

	if (removeMFDFiles(msgDesc, qDesc, datasetInfo) < IMS_OK)
	{
		closeConnection(qDesc);

		/*
		** Dump the dataset information.
		*/

		(void) freeCacheData(msgDesc, datasetInfo);

		goto ERROR;
	}

	/*
	** Dump the dataset information.
	*/	

	(void) freeCacheData(msgDesc, datasetInfo);

	closeConnection(qDesc);



	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"The ims_cleaner processing is complete.");

	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"The ims_cleaner processing failed.");

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
	USER_SPEC *userSpec,
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
		userSpec->username = commands.username;
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

		userSpec->username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (userSpec->username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		userSpec->password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		userSpec->password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (userSpec->password, inputBuffer);
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
		(void) sprintf (logInfo->fileName, "errorlog_%s%s",
			programName, ims_timeStamp ());
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		userSpec->server = commands.server;
	}

	/* database */
	if (commands.database != (char *) NULL)
	{
		userSpec->database = commands.database;
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
}   /* getArgInput */


/******************************************************************************
**
** cacheDatasetInfo
**
** Load the dataset policy information into a linked list.
**
** Dataset 
** |  |  | 
** |  |  -----> Path List ---> Path List ---> Path List...
** |  |
** |  --------> Ext List ---> Ext List ---> Ext List ....
** |
** Dataset
** |  |  | 
** |  |  -----> Path List ---> Path List ---> Path List...
** |  |
** |  --------> Ext List ---> Ext List ---> Ext List ....
** |
** Dataset
** |  |  | 
** |  |  -----> Path List ---> Path List ---> Path List...
** |  |
** |  --------> Ext List ---> Ext List ---> Ext List ....
** Dataset 
** ...
******************************************************************************/

static int cacheDatasetInfo(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	DATASET_LIST **dataset)
{
	DATASET_LIST *ptr;	
	PATH_LIST *ptrPath;
	EXT_LIST *ptrExt;
	char qbuf[IMS_COL512_LEN+1];
	char *saveBuf;
	int status;
	int rowCount;
	int pathCount;
	int extCount;
	int i;


	/*
	** First build the list of datasets
	*/

	saveBuf = qDesc->cmd;
	qDesc->cmd = (char *) &qbuf[0];
	sprintf(qbuf, "select r.dataset_idx, p.granules_table, version_p, \
		process_level from \
		dataset_relation r, dataset_policy p where \
		r.dataset_idx = p.dataset_idx");
	
	ptr = NULL;
	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Can't cache dataset information. Query Failed.");
			qDesc->cmd = saveBuf;
			return(IMS_ERROR);
		}

		/*
		** Check for End of Query.
		*/

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount++;

		if (rowCount == 1)
		{
			ptr = (void *) malloc(sizeof(DATASET_LIST));

			if (ptr == NULL)
			{
				(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate memory to cache dataset info");

				qDesc->cmd = saveBuf;

				return(IMS_FATAL);
			}
			*dataset = ptr;
		}

		else
		{
			ptr->next = (void *) malloc(sizeof(DATASET_LIST));

			if (ptr->next == NULL)
			{
				(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate memory to cache dataset info.");
				qDesc->cmd = saveBuf;
				return(IMS_FATAL);
			}

			ptr = ptr->next;
		}

		ptr->next = NULL;


		/*
		** Get Dataset Index
		*/

		memcpy((char *) &(ptr->dataset_idx), qDesc->valAddr[0],
			qDesc->valLength[0]);

		/*
		** Get Granule Table Name
		*/

		memset((char *) ptr->granules_table, 0, sizeof(ptr->granules_table));
		memcpy((char *) ptr->granules_table, qDesc->valAddr[1], 
					qDesc->valLength[1]);
		ims_trim(ptr->granules_table);

		/*
		** Version Information
		*/

		memcpy((char *) &(ptr->version_p), qDesc->valAddr[2],
			qDesc->valLength[2]);

		/*
		** Process Level Information
		*/

		memcpy((char *) &(ptr->process_level), qDesc->valAddr[3],
			qDesc->valLength[3]);

	}


	/*
	** Reset Query Descriptor
	*/

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not reset the query descriptor.");
		qDesc->cmd = saveBuf;
		return(IMS_FATAL);
	}

	/*
	** Now, get the path information for each dataset.
	*/

	ptr = *dataset;

	for (i = 0; i < rowCount; i++)
	{
		sprintf(qDesc->cmd, "select path, start_granule_idx, end_granule_idx \
			from dataset_path_policy where dataset_idx = %d",
			ptr->dataset_idx);
		
		pathCount = 0;
		ptr->pathList = NULL;
		
		while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
		{

			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Can't cache dataset information. Query Failed.");
				qDesc->cmd = saveBuf;
				return(IMS_ERROR);
			}

			/*
			** Check for End of Query.
			*/

			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}
	
			pathCount++;

			if (pathCount == 1)
			{
				ptrPath = (void *) malloc(sizeof(PATH_LIST));
	
				if (ptrPath == NULL)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not allocate memory to cache dataset info");
					qDesc->cmd = saveBuf;
					return(IMS_FATAL);
				}
				ptr->pathList = ptrPath;
			}

			else
			{
				ptrPath->next = (void *) malloc(sizeof(PATH_LIST));

				if (ptrPath->next == NULL)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not allocate memory to cache dataset info.");
					qDesc->cmd = saveBuf;
					return(IMS_FATAL);
				}

				ptrPath = ptrPath->next;
			}

			ptrPath->next = NULL;


			/*
			** Get Dataset Path Specification
			*/

			memset(ptrPath->path, 0, sizeof(ptrPath->path));

			memcpy(ptrPath->path, qDesc->valAddr[0], qDesc->valLength[0]);
			ims_trim(ptrPath->path);

			/*
			** Get Start Granule Index
			*/

			memcpy((char *) &(ptrPath->start_granule_idx), qDesc->valAddr[1], 
					qDesc->valLength[1]);

			if (qDesc->valLength[1] == 0)
				ptrPath->start_granule_idx = -1;

			/*
			** Get End Granule Index
			*/

			memcpy((char *) &(ptrPath->end_granule_idx), qDesc->valAddr[2], 
					qDesc->valLength[2]);

			if (qDesc->valLength[2] == 0)
				ptrPath->end_granule_idx = -1;


		}
		ptr = ptr->next;	

		/*
		** Reset Query Descriptor
		*/

		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not reset the query descriptor.");
			qDesc->cmd = saveBuf;
			return(IMS_FATAL);
		}
	}


	/*
	** Now, get the file policy information for each dataset.
	*/

	ptr = *dataset;

	for (i = 0; i < rowCount; i++)
	{
		sprintf(qDesc->cmd, "select format, extension, type \
			from dataset_file_policy where dataset_idx = %d",
			ptr->dataset_idx);
		
		extCount = 0;
		ptr->extList = NULL;
		
		while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
		{

			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Can't cache dataset information. Query Failed.");
				qDesc->cmd = saveBuf;
				return(IMS_ERROR);
			}

			/*
			** Check for End of Query.
			*/

			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}
	
			extCount++;

			if (extCount == 1)
			{
				ptrExt = (void *) malloc(sizeof(PATH_LIST));
	
				if (ptrExt == NULL)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not allocate memory to cache dataset info");
					qDesc->cmd = saveBuf;
					return(IMS_FATAL);
				}
				ptr->extList = ptrExt;
			}

			else
			{
				ptrExt->next = (void *) malloc(sizeof(PATH_LIST));

				if (ptrExt->next == NULL)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not allocate memory to cache dataset info.");
					qDesc->cmd = saveBuf;
					return(IMS_FATAL);
				}

				ptrExt = ptrExt->next;
			}

			ptrExt->next = NULL;


			/*
			** Get File Extension Format
			*/

			memset(ptrExt->format, 0, sizeof(ptrExt->format));

			memcpy(ptrExt->format, qDesc->valAddr[0], qDesc->valLength[0]);
			ims_trim(ptrExt->format);

			/*
			** Get Extension
			*/

			memset(ptrExt->extension, 0, sizeof(ptrExt->extension));

			memcpy(ptrExt->extension, qDesc->valAddr[1], qDesc->valLength[1]);
			ims_trim(ptrExt->extension);

			/*
			** Get the file type
			*/

			memcpy(&(ptrExt->type), qDesc->valAddr[2], 
					qDesc->valLength[2]);


		}
		ptr = ptr->next;	

		/*
		** Reset Query Descriptor
		*/

		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not reset the query descriptor.");
			qDesc->cmd = saveBuf;
			return(IMS_FATAL);
		}
	}

	qDesc->cmd = saveBuf;
	return(IMS_OK);

}

/******************************************************************************
**
** freeCacheData()
**
** This function will free the associated data structures for the cached
** dataset data.
**
******************************************************************************/

static int freeCacheData(
	IMS_MSG_STRUCT *msgDesc,
	DATASET_LIST *dataset)
{
	DATASET_LIST *ptr;
	EXT_LIST *ptrExt;
	PATH_LIST *ptrPath;

	ptr = dataset; 
	while (ptr != NULL)
	{
		ptrExt = ptr->extList; 

		while (ptrExt != NULL)
		{
			ptrExt = ptrExt->next;
			free(ptr->extList);
			ptr->extList = ptrExt;
		}

		ptrPath = ptr->pathList;

		while (ptrPath != NULL)
		{
			ptrPath = ptrPath->next;
			free(ptr->pathList);
			ptr->pathList = ptrPath;
		}

		ptr = ptr->next;
		free(dataset);
		dataset = ptr;
	}

	return(IMS_OK);

}


/******************************************************************************
**
** openConnection ()
**
** This function will open a connection to the SQL server.
** 
**
******************************************************************************/

static int openConnection (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ **qDescPass,
	USER_SPEC *userSpec)
{
	IMS_QI_DESC_OBJ *qDesc;   
	int status;


	/*
	** Allocate a query descriptor
	*/

	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate a query descriptor.");
		return(IMS_ERROR);
	}
	
	qDesc->cmd = (char *) malloc(IMS_COL512_LEN);

    if ((char *) qDesc->cmd == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the command area for the query descriptor.");
				 
		 free(qDesc->cmd);
		 (void) ims_qiFreeDesc(qDesc);
		 return(IMS_ERROR);
	}
														  
	IMS_SETUSER (qDesc, userSpec->username);
	IMS_SETPSWD (qDesc, userSpec->password);

	if (userSpec->program != NULL)
		IMS_SETPROG(qDesc, userSpec->program);

	if (userSpec->server != NULL)
		IMS_SETSERVER(qDesc, userSpec->server);

	if (userSpec->database != NULL)
		IMS_SETDBNAME(qDesc, userSpec->database);

	/*
	** Attempt to logon to database
	*/

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	IMS_SET_USERDATA(qDesc);

	*qDescPass = qDesc;  /* Set return query descriptor */
	return(IMS_OK);
}

/******************************************************************************
**
** closeConnection ()
**
** This function will close a connection to the SQL server.
** 
**
******************************************************************************/

static int closeConnection (
	IMS_QI_DESC_OBJ *qDesc)
{

	free(qDesc->cmd);
	(void) ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}


/******************************************************************************
**
** removeMFDFiles ()
**
** This function will remove the products which are marked for delete
** by updating the status in the granules table to DELETED and removing
** the files from the repository.
**
******************************************************************************/

static int removeMFDFiles(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	DATASET_LIST *datasetInfo)
{
	char qbuf[IMS_COL512_LEN+1];
	char *saveBuf;
	char format[IMS_COL10_LEN+1];
	char name[IMS_COL30_LEN|1];
	int granule_idx;
	int status;
	int rowCount;
	short int version;
	GRANULE_LIST *ptr, *glist;

	saveBuf = qDesc->cmd;
	qDesc->cmd = (char *) &qbuf[0];
	
	while (datasetInfo != NULL)
	{
		/*
		** Retrieve the products with status = MFD
		*/

		sprintf(qbuf, "select name, format, granule_idx, version from %s where \
			status = %d",
			datasetInfo->granules_table, MFD_VALUE); 

		rowCount = 0;
		ptr = NULL;
		glist = NULL;

		while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
		{

			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Can't determine granule information. Query Failed.");
				qDesc->cmd = saveBuf;
				return(IMS_ERROR);
			}

			/*
			** Check for End of Query.
			*/

			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}

			rowCount ++;

			/*
			** Get product name
			*/

			memset(name, 0, sizeof(name));
			memcpy(name, qDesc->valAddr[0], qDesc->valLength[0]);
			ims_trim(name);

			/*
			** Get Format
			*/

			memset(format, 0, sizeof(format));
			memcpy(format, qDesc->valAddr[1], qDesc->valLength[1]);
			ims_trim(format);

			/*
			** Get Granule Index 
			*/

			memcpy(&granule_idx, qDesc->valAddr[2], qDesc->valLength[2]);

			/*
			** Get Version
			*/

			memcpy(&version, qDesc->valAddr[3], qDesc->valLength[3]);


			/*
			** Remove Product Files
			*/

			if (removeProductFiles(msgDesc, datasetInfo, name, format, 
					granule_idx, version) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not remove files for granule %s, index %d",
					datasetInfo->granules_table, granule_idx);

				granule_idx = -1;

				/*
				** Allow it to continue...
				*/
			}

			if (rowCount == 1) 
			{
				
				ptr = (void *) malloc(sizeof(struct granule_list));

				if (ptr == NULL)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not allocate memory for granule list");
					qDesc->cmd = saveBuf;
					return(IMS_FATAL);
				}

				glist = ptr;
			}
			else
			{
				ptr->next = (void *) malloc(sizeof(struct granule_list));

				if (ptr->next == NULL)
				{
					(void) ims_msg(msgDesc, IMS_FATAL,
						"Could not allocate memory for granule list");
					qDesc->cmd = saveBuf;
					return(IMS_FATAL);
				}

				ptr = ptr->next;

			}

			ptr->next = NULL;
			ptr->granule_idx = granule_idx;

		}

		/*
		** Reset Query Descriptor
		*/

		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not reset the query descriptor.");
			qDesc->cmd = saveBuf;
			return(IMS_FATAL);
		}

		/*
		** Update all the records for the current dataset.
		*/

		if (updateGranuleStatus(msgDesc, qDesc, datasetInfo, glist) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not update granule status for table %s",
				datasetInfo->granules_table);
		}

		/*
		** Free the granule list 
		*/

		while (glist != NULL)
		{
			ptr = glist;
			glist = glist->next;
			free(ptr);
		}


		datasetInfo = datasetInfo->next;
	}
	
	qDesc->cmd = saveBuf;
	return(IMS_OK);
}

/******************************************************************************
**
** updateGranuleStatus
**
** Update the granule status to DELETE.
**
******************************************************************************/

static int updateGranuleStatus(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	DATASET_LIST *datasetInfo,
	GRANULE_LIST *glist)
{
	char qbuf[IMS_COL512_LEN+1];
	char *saveBuf;


	saveBuf = qDesc->cmd;
	qDesc->cmd = &qbuf[0];

	/*
	** Reset Query Descriptor
	*/

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not reset the query descriptor.");
		qDesc->cmd = saveBuf;
		return(IMS_FATAL);
	}

	while (glist != NULL)
	{
		
		if (glist->granule_idx == -1)
		{
			glist = glist->next;
			continue;
		}

		sprintf(qbuf, "update %s set status = %d where granule_idx = %d",
			datasetInfo->granules_table, DELETE_VALUE, glist->granule_idx);

		if (execCmd(msgDesc, qDesc) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not update granule status for index %d", 
				glist->granule_idx);
			qDesc->cmd = saveBuf;
			return(IMS_ERROR);
		}

		glist = glist->next;
	}

	qDesc->cmd = saveBuf;
	return(IMS_OK);

}


/******************************************************************************
**
** removeProductFiles
**
** Remove the product files from the repository
**
******************************************************************************/

static int removeProductFiles(
	IMS_MSG_STRUCT *msgDesc,
	DATASET_LIST *datasetInfo,
	char *name,
	char *format,
	int granule_idx, 
	short int version)
{
	PATH_LIST *ptrPath;
	EXT_LIST *ptrExt;
	char fullPathName[IMS_PATH_LEN+1];
	

	
	/*
	** Determine appropriate path
	*/

	for (ptrPath = datasetInfo->pathList; ptrPath != NULL; 
			ptrPath = ptrPath->next)
	{
		
		if ((ptrPath->start_granule_idx  == -1) &&
		   (ptrPath->end_granule_idx == -1))
		{
			break;
		}

		if ((ptrPath->start_granule_idx <= granule_idx) &&
			(ptrPath->end_granule_idx == -1))
 		{
			break;
		}

		if ((ptrPath->start_granule_idx <= granule_idx) &&
			(granule_idx <= ptrPath->end_granule_idx))
		{
			break;
		}
	}

	if (ptrPath == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"No path was found for granule index %d.", granule_idx);
		return(IMS_ERROR);
	}

	ptrExt = datasetInfo->extList;

	(void) ims_msg(msgDesc, IMS_INFO, "Removing Product File %s for granule table %s.", 
			name, datasetInfo->granules_table);

	while (ptrExt != NULL)
	{
		/*
		** If this extension corresponds to the products format type then
		** we found something to delete.
		*/
		
		if (strcmp(ptrExt->format, format) == 0)
		{
			if (datasetInfo->version_p != 'Y')
				sprintf(fullPathName, "%s/%s.%s",
					ptrPath->path, name, ptrExt->extension);
			else
				sprintf(fullPathName, "%s/%s.%s.%d",
					ptrPath->path, name, ptrExt->extension, version);

			/*
			** Level-1 Products should leave the PMF file around...
			*/

			if ((datasetInfo->process_level == L1_PRODUCT) &&
				(ptrExt->type == PMF_FILE_TYPE))
			{
				ptrExt = ptrExt->next;
				continue;
			}


			if (remove(fullPathName) < 0)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not remove product file %s", fullPathName);
				return(IMS_ERROR);
			}

		}

		ptrExt = ptrExt->next;
	}
	
	return(IMS_OK);

}


/******************************************************************************
**
** execCmd ()
**
******************************************************************************/

static int execCmd (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	int severity;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	if (qDesc->msgNo != 0)
	{
		return (ims_msgGetSeverity (msgDesc));
	}


	/*
	** Reset Query Descriptor
	*/

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not reset the query descriptor.");
		return(IMS_FATAL);
	}

	return (IMS_OK);
}

