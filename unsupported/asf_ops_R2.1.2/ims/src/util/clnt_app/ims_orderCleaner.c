static char *sccs = "@(#)ims_orderCleaner.c	5.7 04/22/97";

/******************************************************************************
**
** File:        ims_orderCleaner.c
**
** Function:    This program will search through the orders and determine
**				if there are any completed orders.  If an order is determined
**				to be completed or cancelled, then it will be deleted.
**
** Author:      Dan Crichton
**
** Date:        1/12/96
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
#include <ims_archive.h>

/*
** Local Definitions.
*/
#define DIR_NAME_LEN (size_t)30

/*
** Items Table Information - We may want to get this from the 
** catalog at some point.
*/


#define L0_PRODUCT		0
#define L1_PRODUCT		1
#define L2_PRODUCT		2
#define L3_PRODUCT		2


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
** Dataset List Information Structure Definition.
*/

typedef struct Dataset_List
{
	short int dataset_idx;
	char platform[IMS_COL30_LEN+1];
	char sensor[IMS_COL30_LEN+1];
	char dataset_name[IMS_COL80_LEN+1];
	char granules_table[IMS_COL30_LEN+1];
	char version_p;
	short int process_level;
	struct Dataset_List *next;
} DATASET_LIST;


/*
** Order Item List
*/

typedef struct order_item_list
{
	DATASET_LIST *dataset;
	char product_name[IMS_COL30_LEN+1];
	short int status;
	int order_id;
	int granule_idx;
	struct order_item_list *next;
} ORDER_ITEM_LIST;



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
int glbl_delete_count = 0;


/*
** Local Functions
*/

static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, USER_SPEC *, LOG_SPEC *);
static int cacheDatasetInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, DATASET_LIST **);
static int loadOrderInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, 
ORDER_ITEM_LIST **, DATASET_LIST *);
static int closeConnection (IMS_QI_DESC_OBJ *);
static int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **, USER_SPEC *);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, int);
static int freeCacheData(IMS_MSG_STRUCT *, DATASET_LIST *);
static int deleteOrderProduct(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, ORDER_ITEM_LIST *);

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
	ORDER_ITEM_LIST *orderItems, *ptr;
	int count = 0;

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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  IMS ORDER CLEANER STARTUP  <<<<<<<<<<<<<<<<<<<<<<\n\n");

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

	if (loadOrderInfo(msgDesc, qDesc, &orderItems, datasetInfo) < IMS_OK)
	{
		(void) freeCacheData(msgDesc, datasetInfo);
		closeConnection(qDesc);
		goto ERROR;
	}


	while (orderItems != NULL)
	{
		
		if (deleteOrderProduct(msgDesc, qDesc, orderItems) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not remove product %s for order_id %d.", 
				orderItems->product_name, orderItems->order_id);
		}
		ptr = orderItems;
		orderItems = orderItems->next;
		free(ptr);
		count ++;
	}


	/*
	** Dump the dataset information.
	*/	


	(void) freeCacheData(msgDesc, datasetInfo);

	closeConnection(qDesc);



	/*
	** Shutdown the message facility.
	*/

	(void) ims_msg(msgDesc, IMS_INFO,
		"%d orders were candidates for deleting.  %d were deleted.",
		count, glbl_delete_count);


	(void) ims_msg (msgDesc, IMS_INFO,
		"The ims_orderCleaner processing is complete.");

	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"The ims_orderCleaner processing failed.");

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
******************************************************************************/

static int cacheDatasetInfo(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	DATASET_LIST **dataset)
{
	DATASET_LIST *ptr;	
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
	sprintf(qbuf, "select r.dataset_idx, p.granules_table, p.version_p, \
		process_level, r.sensor, r.dataset, r.platform from \
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

		/*
		** Sensor
		*/

		memset(ptr->sensor, 0, sizeof(ptr->sensor));

		memcpy(ptr->sensor, qDesc->valAddr[4],
			qDesc->valLength[4]);
		ims_trim(ptr->sensor);

		/*
		** Dataset Name
		*/

		memset(ptr->dataset_name, 0, sizeof(ptr->dataset_name));

		memcpy(ptr->dataset_name, qDesc->valAddr[5],
			qDesc->valLength[5]);
		ims_trim(ptr->dataset_name);

		/*
		** Platform
		*/

		ims_trim(ptr->platform);
		memset(ptr->platform, 0, sizeof(ptr->platform));

		memcpy(ptr->platform, qDesc->valAddr[6],
			qDesc->valLength[6]);
		ims_trim(ptr->platform);

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

	qDesc->cmd = saveBuf;
	return(IMS_OK);

}

/******************************************************************************
**
** loadOrderInfo
**
** This function will load the order information which is ready to be 
** purged into the database.
**
******************************************************************************/

static int loadOrderInfo(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	ORDER_ITEM_LIST **orderItems,
	DATASET_LIST *dataset)
{
	char qbuf[IMS_COL512_LEN+1];
	char *saveBuf;
	int status;
	int rowCount;
	int i;
	ORDER_ITEM_LIST *ptr;
	short int dataset_idx;
	DATASET_LIST *dataset_ptr;

	/*
	** First build the list of datasets
	*/

	*orderItems = NULL;


	saveBuf = qDesc->cmd;
	qDesc->cmd = (char *) &qbuf[0];

	/*
	** ?? Think about changing 10 and 12 here to use the order item
	** table.
	*/

	sprintf(qbuf, "select o.p_dataset_idx, o.p_granule_name, o.status, \
		o.order_id, o.p_granule_idx from order_item o, dataset_policy p  where \
		o.p_dataset_idx = p.dataset_idx and (o.status = 10 or o.status = 12) \
		and o.deleted_p <> 'Y' and p.process_level = 1");

	
	ptr = NULL;
	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Can't cache order information. Query Failed.");
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

		if (ptr == NULL)
		{
			/*
			** First item
			*/

			ptr = (void *) malloc(sizeof(ORDER_ITEM_LIST));
			*orderItems = ptr;

		}
		else
		{
			ptr->next = (void *) malloc(sizeof(ORDER_ITEM_LIST));
			ptr = ptr->next;
		}

		if (ptr == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not allocate memory for order list");
			return(IMS_ERROR);
		}

		/*
		** dataset_idx - we will set up a link into the dataset cache table
		*/

		dataset_idx = -1;
		memcpy((char *) &dataset_idx, qDesc->valAddr[0], qDesc->valLength[0]); 

		ptr->dataset = NULL;

		dataset_ptr = dataset;

		while (dataset_ptr != NULL)
		{
			if (dataset_ptr->dataset_idx == dataset_idx)
			{
				ptr->dataset = dataset_ptr;
				break;
			}
			dataset_ptr = dataset_ptr->next;
		}


		/*
		** Granule Name (Name of the product not granules_xx)
		*/

		memset(ptr->product_name, 0, sizeof(ptr->product_name));
		memcpy(ptr->product_name, qDesc->valAddr[1], qDesc->valLength[1]);
		ims_trim(ptr->product_name);

		/*
		** status
		*/

		memcpy((char *) &(ptr->status), qDesc->valAddr[2], qDesc->valLength[2]);

		/*
		** order_id
		*/

		memcpy((char *) &(ptr->order_id), qDesc->valAddr[3], qDesc->valLength[3]);

		/*
		** granule_idx
		*/

		memcpy((char *) &(ptr->granule_idx), qDesc->valAddr[4], qDesc->valLength[4]);

		ptr->next = NULL;

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

	ptr = dataset; 
	while (ptr != NULL)
	{
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

	/*
	** I am not going to free the command buffer.  Since this is opened
	** once and closed once we will let the O/S clean up.  This seems
	** better since there was a case where the command buffer was modified
	** and not restored.
	**
	** free(qDesc->cmd);
	*/

	(void) ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}



/******************************************************************************
**
** execCmd ()
**
******************************************************************************/

static int execCmd (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	int resetFlag)
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

	if (!resetFlag)
		return(IMS_OK);


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


/******************************************************************************
**
** deleteOrderProduct()
**
******************************************************************************/

static int deleteOrderProduct(
	IMS_MSG_STRUCT *msgDesc, 
	IMS_QI_DESC_OBJ *qDesc, 
	ORDER_ITEM_LIST *orderItems) 
{
	int status;
	char qbuf[IMS_COL512_LEN+1];
	IMS_CLNT_EVENT request;
	char *saveBuf;
	short int read_count;



	/*
	** Check read count on granule.
	*/

	saveBuf = qDesc->cmd;

	qDesc->cmd = &qbuf[0];

	sprintf(qDesc->cmd, 
		"get_read_count %d, %d",
		orderItems->dataset->dataset_idx,
		orderItems->granule_idx);
		


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get order read count information.");
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

		memcpy(&read_count, qDesc->valAddr[0], qDesc->valLength[0]);

	}

	if (IMS_AFFECTED(qDesc) < 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get read count information: %s",
			qDesc->cmd);

   		qDesc->cmd = saveBuf;
		return(IMS_ERROR);

	}

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
   		qDesc->cmd = saveBuf;
		(void) ims_msg(msgDesc, IMS_FATAL,
			"Could not reset the query descriptor.");
		return(IMS_FATAL);
	}

	if (read_count >  0)
	{
   		qDesc->cmd = saveBuf;

		/*
		(void) ims_msg(msgDesc, IMS_WARNING,
			"Can't remove order %d.  Currently being read by another user.", 
			orderItems->order_id);
		*/

		return(IMS_OK);
	}

	/*
	** Verify that this is an product we really want to delete.
	*/
		
	if ((orderItems->product_name[0] == '\0') && 
		(orderItems->dataset == NULL) &&
		(orderItems->status == 12))
	{
		/*
		** Skip this since the info is just not available for cancelled 
		** orders.
		*/

		goto update_table;

	}



	/*
	** First call ims_archive() and delete product
	*/

	memset((char *) &request,0, sizeof(request));

	request.username = "ims_clnt";
	request.password = "ims_clnt";
	request.accountId = "ACCT_IMS";
	request.sensor = orderItems->dataset->sensor;
	request.platform = orderItems->dataset->platform;
	request.dataset = orderItems->dataset->dataset_name;
	request.name = orderItems->product_name;
	request.version = -1;
	request.programName = "ims_orderCleaner";
	request.msgDesc = msgDesc;
	request.requestType = IMS_DELETE;

	if (ims_archive(&request) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not remove product from server.");
   		qDesc->cmd = saveBuf;
		return(IMS_ERROR);
	}

	/*
	** Update order_item table to reflect that item was deleted.
	*/
		

update_table:

	qDesc->cmd = &qbuf[0];

	sprintf(qDesc->cmd, 
		"update order_item set deleted_p = 'Y' where  \
			p_dataset_idx = %d and p_granule_name = '%s'",
		orderItems->dataset->dataset_idx, orderItems->product_name);

	if (execCmd(msgDesc, qDesc, 1) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not update deleted_p flag in order table");
		qDesc->cmd = saveBuf;
		return(IMS_ERROR);
	}

	qDesc->cmd = saveBuf;
	glbl_delete_count ++;
	return(IMS_OK);
}
