static char *sccs = "@(#)ims_ftp.c	5.5  11/10/96";
/******************************************************************************
**
** File:        ims_ftp.c
**
** Function:    This application will search the given target path for
**              for distributed order directories and then transfer the
**              files from those directories to the given site using ftp.
**
** Author:      Sean Hardman
**
** Date:        12/2/95
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
#include <ims_media.h>
#include <ims_tar.h>
#include <ims_childHandler.h>

/*
** Local Definitions.
*/
#define DIR_NAME_LEN (size_t)30

/*
** FTP Information structure definition.
*/
typedef struct ftpInfo
{
	char *username;
	char *password;
	char *host;
	char *path;
} FTP_SPEC;

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
** Path List structure definition.
*/
typedef struct pathList
{
	char path[IMS_PATH_LEN+1];
	struct pathList *next;
} PATH_LIST;

/*
** File List structure definition.
*/
typedef struct fileList
{
	char fileName[IMS_NAME_LEN+1];
	struct fileList *next;
} FILE_LIST;

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, MEDIA_USER_SPEC *, FTP_SPEC *,
	LOG_SPEC *);
static int processTargetPath (IMS_MSG_STRUCT *, MEDIA_USER_SPEC *, FTP_SPEC *,
	char *);
static int checkOrderDirName (IMS_MSG_STRUCT *, char *, int *, int *);
static int getOrderStatus (IMS_MSG_STRUCT *, MEDIA_USER_SPEC *, int, int,
	DBSMALLINT *);
static int createCommandFile (IMS_MSG_STRUCT *, FTP_SPEC *, FILE_LIST *,
	char *, char *);
static int createShellFile (IMS_MSG_STRUCT *, FTP_SPEC *, char *, char *,
	char *);
static void freeFileList (FILE_LIST *);
static void freePathList (PATH_LIST *);

/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
	char *username;
	char *password;
	char *targetPath;
	char *ftpUser;
	char *ftpPassword;
	char *ftpHost;
	char *ftpPath;
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
	{"-T",              &commands.targetPath},
	{"+targetPath",     &commands.targetPath},
	{"-A",              &commands.ftpUser},
	{"+ftpUser",        &commands.ftpUser},
	{"-B",              &commands.ftpPassword},
	{"+ftpPassword",    &commands.ftpPassword},
	{"-H",              &commands.ftpHost},
	{"+ftpHost",        &commands.ftpHost},
	{"-D",              &commands.ftpPath},
	{"+ftpPath",        &commands.ftpPath},
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
	{"targetPath",      &commands.targetPath},
	{"ftpUser",         &commands.ftpUser},
	{"ftpPassword",     &commands.ftpPassword},
	{"ftpHost",         &commands.ftpHost},
	{"ftpPath",         &commands.ftpPath},
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
static char *targetPath;

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
	FTP_SPEC ftpInfo;
	LOG_SPEC logInfo;
	MEDIA_USER_SPEC userSpec;
	char logFileSpec[IMS_PATH_LEN+1];
	char writeBuffer[100];
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int fd;

	/*
	** Initialize variables.
	*/
	(void) memset (&ftpInfo, 0, (size_t) sizeof (FTP_SPEC));
	(void) memset (&logInfo, 0, (size_t) sizeof (LOG_SPEC));
	(void) memset (&userSpec, 0, (size_t) sizeof (MEDIA_USER_SPEC));

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
	if ((status = getArgInput (msgDesc, &userSpec, &ftpInfo,
		&logInfo)) < IMS_OK)
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
			"\n\n>>>>>>>>>>>>>>>>>>>>>>  FTP Startup  "
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
	** Process the order directories found in the target path.
	*/
	if ((status = processTargetPath (msgDesc, &userSpec, &ftpInfo,
		targetPath)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"Processing completed successfully.");

	(void) ims_msgStructFree (msgDesc);
	(void) close (fd);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"Processing failed.");

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
	MEDIA_USER_SPEC *userSpec,
	FTP_SPEC *ftpInfo,
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

	/* targetPath */
	if (commands.targetPath != (char *) NULL)
	{
		targetPath = commands.targetPath;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Target Path: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		targetPath = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (targetPath, inputBuffer);
	}

	/* ftpUser */
	if (commands.ftpUser != (char *) NULL)
	{
		ftpInfo->username = commands.ftpUser;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"FTP User: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		ftpInfo->username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (ftpInfo->username, inputBuffer);
	}

	/* ftpPassword */
	if (commands.ftpPassword != (char *) NULL)
	{
		ftpInfo->password = commands.ftpPassword;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"FTP Password: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		ftpInfo->password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (ftpInfo->password, inputBuffer);
	}

	/* ftpHost */
	if (commands.ftpHost != (char *) NULL)
	{
		ftpInfo->host = commands.ftpHost;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"FTP Host: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		ftpInfo->host = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (ftpInfo->host, inputBuffer);
	}


	/* ftpPath */
	if (commands.ftpPath != (char *) NULL)
	{
		ftpInfo->path = commands.ftpPath;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"FTP Path: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		ftpInfo->path = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (ftpInfo->path, inputBuffer);
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
** processTargetPath ()
**
******************************************************************************/

static int processTargetPath (
	IMS_MSG_STRUCT *msgDesc,
	MEDIA_USER_SPEC *userSpec,
	FTP_SPEC *ftpInfo,
	char *targetPath)
{
	DIR *dp;
	PATH_LIST *pathList;
	PATH_LIST *currPath;
	PATH_LIST *prevPath;
	FILE_LIST *fileList;
	FILE_LIST *currFile;
	FILE_LIST *prevFile;
	struct dirent *dirp;
	struct stat statbuf;
	char homePath[IMS_PATH_LEN+1];
	char commandFileSpec[IMS_PATH_LEN+IMS_NAME_LEN+1];
	char shellFileSpec[IMS_PATH_LEN+IMS_NAME_LEN+1];
	DBSMALLINT orderStatus;
	int orderId;
	int itemId;
	int status;
	int pathCount;
	int fileCount;
	int deleteFlag;
	pid_t pid;

	/*
	** Initialize variables.
	*/
	pathCount = 0;
	deleteFlag = IMS_ON;
	pathList = (PATH_LIST *) NULL;
	prevPath = (PATH_LIST *) NULL;
	prevFile = (FILE_LIST *) NULL;

	/*
	** Get the current working directory.
	*/
	if (getcwd (homePath, IMS_PATH_LEN) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not get the current working directory. %s",
			strerror (errno));
		return (IMS_ERROR);
	}

	/*
	** Open the target path directory file.
	*/
	if ((dp = opendir (targetPath)) == (DIR *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open target path '%s'. %s",
			targetPath, strerror (errno));
		return (IMS_ERROR);
	}

	/*
	** Change directories to our target path.
	*/
	if (chdir (targetPath) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not change directory to target path '%s'. %s",
			targetPath, strerror (errno));
		return (IMS_ERROR);
	}

	/*
	** Get the directory contents.
	*/
	while ((dirp = readdir (dp)) != NULL)
	{
		/*
		** Ignore . and ..
		*/
		if (dirp->d_name[0] == '.')
		{
			continue;
		}

		/*
		** Search for the directory entries and place them in a list.
		*/
		stat (dirp->d_name, &statbuf);
		if (S_ISDIR (statbuf.st_mode))
		{
			/*
			** Allocate space for the PATH_LIST structure.
			*/
			if ((currPath = (PATH_LIST *) malloc
				((size_t) sizeof (PATH_LIST))) ==
				(PATH_LIST *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for PATH_LIST structure.");
				return (IMS_FATAL);
			}

			/*
			** pathList points to the first element of the list.
			*/
			if (++pathCount == 1)
			{
				pathList = currPath;
			}
			else
			{
				prevPath->next = currPath;
			}

			(void) strcpy (currPath->path, dirp->d_name);
			currPath->next = (PATH_LIST *) NULL;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_WARNING,
				"A stray file '%s', not associated with an order, "
				"was found in directory '%s'.",
				dirp->d_name, targetPath);
		}

		prevPath = currPath;
	}

	/*
	** Close the target path.
	*/
	if (closedir (dp) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not close target path '%s'. %s",
			targetPath, strerror (errno));
		freePathList (pathList);
		return (IMS_ERROR);
	}

	/*
	** Determine whether order directories were found.
	*/
	if (pathList == (PATH_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"No order directories were found in target path '%s'.",
			targetPath);
	}

	/*
	** Process each of the order directories in the path list.
	*/
	currPath = pathList;
	while (currPath != (PATH_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Investigating order directory '%s'.",
			currPath->path);

		/*
		** Check the format of the order directory name.
		*/
		if ((status = checkOrderDirName (msgDesc, currPath->path,
			&orderId, &itemId)) < IMS_OK)
		{
			/* Skip this directory. */
			(void) ims_msg (msgDesc, status,
				"The order directory '%s' was not processed due to an "
				"invalid name format.",
				currPath->path);
			currPath = currPath->next;
			continue;
		}

		/*
		** Query the database server for the order status.
		*/
		if ((status = getOrderStatus (msgDesc, userSpec, orderId, itemId,
			&orderStatus)) < IMS_WARNING)
		{
			(void) ims_msg (msgDesc, status,
				"An error occurred connecting to, or retrieving data from, "
				"the database server.");
			freePathList (pathList);
			return (status);
		}

		/*
		** Check the status.
		*/
		if ((orderStatus != MEDIA_GENERATED) &&
			(orderStatus != MEDIA_COMPLETE))
		{
			/* Skip this directory. */
			(void) ims_msg (msgDesc, status,
				"The order directory '%s' was not processed due to a "
				"non-GENERATED status.",
				currPath->path);
			currPath = currPath->next;
			continue;
		}

		/*
		** Open the order directory file.
		*/
		if ((dp = opendir (currPath->path)) == (DIR *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not open order directory '%s'. %s",
				currPath->path, strerror (errno));
			freePathList (pathList);
			return (IMS_ERROR);
		}
	
		/*
		** Change directories to our order directory.
		*/
		if (chdir (currPath->path) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not change to order directory '%s'. %s",
				currPath->path, strerror (errno));
			freePathList (pathList);
			return (IMS_ERROR);
		}

		/*
		** Get the directory contents.
		*/
		fileCount = 0;
		fileList = (FILE_LIST *) NULL;			

		while ((dirp = readdir (dp)) != NULL)
		{
			/*
			** Ignore . and ..
			*/
			if (dirp->d_name[0] == '.')
			{
				continue;
			}

			/*
			** Search for files in the order directory and
			** place them in a list.
			*/
			stat (dirp->d_name, &statbuf);
			if (S_ISDIR (statbuf.st_mode))
			{
				(void) ims_msg (msgDesc, IMS_WARNING,
					"A stray directory '%s', was found inside "
					"order directory '%s'.",
					dirp->d_name, currPath->path);
			}
			else
			{
				/*
				** Allocate space for the FILE_LIST structure.
				*/
				if ((currFile = (FILE_LIST *) malloc
					((size_t) sizeof (FILE_LIST))) ==
					(FILE_LIST *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Could not allocate memory for "
						"FILE_LIST structure.");
					return (IMS_FATAL);
				}

				/*
				** fileList points to the first element of the list.
				*/
				if (++fileCount == 1)
				{
					fileList = currFile;
				}
				else
				{
					prevFile->next = currFile;
				}

				(void) strcpy (currFile->fileName, dirp->d_name);
				currFile->next = (FILE_LIST *) NULL;

#ifdef DEBUG
				(void) ims_msg (msgDesc, IMS_INFO,
					"Found order file '%s'.",
					dirp->d_name);
#endif  /* DEBUG */

				prevFile = currFile;
			}
		}
	
		/*
		** If we found files, FTP them to their destination.
		*/
		if (fileList != (FILE_LIST *) NULL)
		{
			/*
			** Create the FTP command file.
			*/
			if ((status = createCommandFile (msgDesc, ftpInfo, fileList,
				homePath, commandFileSpec)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, status,
					"Could not create FTP command file.");
				freeFileList (fileList);
				freePathList (pathList);
				return (status);
			}

			/*
			** Create the FTP C shell file.
			*/
			if ((status = createShellFile (msgDesc, ftpInfo, commandFileSpec,
				homePath, shellFileSpec)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, status,
					"Could not create FTP C shell file.");
				(void) remove (commandFileSpec);
				freeFileList (fileList);
				freePathList (pathList);
				return (status);
			}

			/*
			** Start the FTP process.
			*/
			if ((pid = ims_startChild (msgDesc, shellFileSpec, shellFileSpec,
				(char *) 0)) == -1)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not start the FTP process.");
				(void) remove (commandFileSpec);
				(void) remove (shellFileSpec);
				freeFileList (fileList);
				freePathList (pathList);
				return (IMS_ERROR);
			}

			/*
			** Wait for the FTP process to finish.
			*/
			if ((status = ims_waitForChild (msgDesc, pid)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, status,
					"The FTP process failed.");
				(void) remove (commandFileSpec);
				(void) remove (shellFileSpec);
				freeFileList (fileList);
				freePathList (pathList);
				return (status);
			}

			/*
			** Send success message.
			*/
			(void) ims_msg (msgDesc, IMS_INFO,
				"Successfully transferred the files from order "
				"directory '%s' to directory '%s' on host '%s'.",
				currPath->path, ftpInfo->path, ftpInfo->host);

			/*
			** Remove the FTP command file.
			*/
			if (remove (commandFileSpec) == -1)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not remove the FTP command file '%s'. %s",
					commandFileSpec, strerror (errno));
				(void) remove (shellFileSpec);
				freeFileList (fileList);
				freePathList (pathList);
				return (IMS_ERROR);
			}

			/*
			** Remove the FTP C shell file.
			*/
			if (remove (shellFileSpec) == -1)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not remove the FTP C shell file '%s'. %s",
					shellFileSpec, strerror (errno));
				freeFileList (fileList);
				freePathList (pathList);
				return (IMS_ERROR);
			}

			/*
			** Free the file list.
			*/
			freeFileList (fileList);
		}
		else /* No files found in order directory. */
		{
			deleteFlag = IMS_OFF;
			(void) ims_msg (msgDesc, IMS_WARNING,
				"There were no files found in order directory '%s'.",
				currPath->path);
		}

		/*
		** Close the order directory.
		*/
		if (closedir (dp) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not close order directory '%s'. %s",
				currPath->path, strerror (errno));
			freePathList (pathList);
			return (IMS_ERROR);
		}

		/*
		** Change directories back to the target path.
		*/
		if (chdir (targetPath) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not change directory back to target "
				"path '%s'. %s",
				targetPath, strerror (errno));
			freePathList (pathList);
			return (IMS_ERROR);
		}

		/*
		** Remove the order directory and all associated files.
		*/
		if (deleteFlag == IMS_ON)
		{
			if ((status = ims_removeStageDir (msgDesc,
				currPath->path)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not remove order directory '%s'.",
					currPath->path);
				freePathList (pathList);
				return (IMS_ERROR);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"Removed order directory '%s'.");
			}
		}

		currPath = currPath->next;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** checkOrderDirName ()
**
** Parse the order directory name to determine orderId and itemId.
** The normal format will be "<order_id><item_id>" where item id
** will always be 3 characters long.
**
******************************************************************************/

static int checkOrderDirName (
	IMS_MSG_STRUCT *msgDesc,
	char *orderDirName,
	int *orderId,
	int *itemId)
{
	size_t length;
	char orderStr[DIR_NAME_LEN+1];
	char itemStr[4];

	/*
	** Initialize variables.
	*/
	*orderId = 0;
	*itemId = 0;

	/*
	** Check for valid integers.
	*/
	if (ims_isInteger (orderDirName) == IMS_FALSE)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"The order directory name contains non-integer characters.");
		return (IMS_WARNING);
	}

	/*
	** See if the name is long enough to parse but not longer than the max.
	*/
	if ((length = strlen (orderDirName)) < (size_t) 4)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"The length of the order directory name is less than the minimum length of '4'.");
		return (IMS_WARNING);
	}
	else if (length > DIR_NAME_LEN)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"The length of the order directory name exceeds the maximum length of '%d'.",
			DIR_NAME_LEN);
		return (IMS_WARNING);
	}

	/*
	** Extract the order id and item id from the path.
	*/
	(void) strncpy (orderStr, orderDirName, length-3); 
	orderStr[length-3] = '\0';
	(void) strncpy (itemStr, &orderDirName[length-3], 3);
	itemStr[3] = '\0';

	*orderId = atoi (orderStr);
	*itemId = atoi (itemStr);

	return (IMS_OK);
}

/******************************************************************************
**
** getOrderStatus ()
**
** Query the database server for the status of the given order item.
**
******************************************************************************/

static int getOrderStatus (
	IMS_MSG_STRUCT *msgDesc,
	MEDIA_USER_SPEC *userSpec,
	int orderId,
	int itemId,
	DBSMALLINT *orderStatus)
{
	IMS_QI_DESC_OBJ *qDesc;
	char cmdBuf[256];
	int status;

	/*
	** Initialize variables.
	*/
	*orderStatus = MEDIA_NO_STATUS;

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
	IMS_SETUSER (qDesc, userSpec->username);

	IMS_SETPSWD (qDesc, userSpec->password);
	
	IMS_SETPROG (qDesc, userSpec->program);

	if (userSpec->server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, userSpec->server);
	}

	if (userSpec->database != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, userSpec->database);
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
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_ERROR);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select status from order_item \
		where order_id = %d and item_id = %d",
		orderId, itemId);

	/*
	** Process the result row for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The database query failed.");
			(void) ims_qiFreeDesc (qDesc);
			return (IMS_ERROR);
		}
	}

	/*
	** Make sure we got one row and only one row back.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Could not obtain the status for item '%d' of order '%d'.",
			itemId, orderId);
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_WARNING);
	}

	if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"More than one status was returned for item '%d' of order '%d'.",
			itemId, orderId);
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_WARNING);
	}

	/*
	** Save the status value.
	*/
	(void) memcpy (&(*orderStatus), IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));

	/*
	** Close the connection and free the query descriptor.
	*/
	(void) ims_qiFreeDesc (qDesc);

	return (IMS_INFO);
}

/******************************************************************************
**
** createCommandFile ()
**
******************************************************************************/

static int createCommandFile (
	IMS_MSG_STRUCT *msgDesc,
	FTP_SPEC *ftpInfo,
	FILE_LIST *fileList,
	char *homePath,
	char *commandFileSpec)
{
	FILE_LIST *currFile;
	char commandFileName[IMS_NAME_LEN+1];
	char *writeBuffer;
	int fd;

	/*
	** Allocate space for the writeBuffer.
	*/
	if ((writeBuffer = malloc (128)) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate space the command file buffer.");
		return (IMS_FATAL);
	}

	/*
	** Assemble the file specification.
	*/
	(void) strcpy (commandFileName, "ftp.cmd");
	ims_concatFilePath (commandFileSpec, homePath, commandFileName);

	/*
	** Create the FTP command file.
	*/
	if ((fd = open (commandFileSpec, O_RDWR|O_CREAT|O_EXCL)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open FTP command file '%s'. %s",
			commandFileSpec, strerror (errno));
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Change the mode of the file.
	*/
	if (chmod (commandFileSpec, 0600) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not change the mode for FTP command file '%s'. %s",
			commandFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (commandFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Write the user and password information.
	*/
	(void) sprintf (writeBuffer, "user %s %s\n",
		ftpInfo->username, ftpInfo->password);

	if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the user information to FTP "
			"command file '%s'. %s",
			commandFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (commandFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Write the target directory information.
	*/
	(void) sprintf (writeBuffer, "cd %s\n",
		ftpInfo->path);

	if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the path information to FTP "
			"command file '%s'. %s",
			commandFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (commandFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Write the binary statement.
	*/
	(void) sprintf (writeBuffer, "binary\n");

	if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the 'binary' statement to FTP "
			"command file '%s'. %s",
			commandFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (commandFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Write the file list information.
	*/
	currFile = fileList;
	while (currFile != (FILE_LIST *) NULL)
	{
		(void) sprintf (writeBuffer, "put ");
		(void) strcat (writeBuffer, currFile->fileName);
		(void) strcat (writeBuffer, "\n");

		if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not write the file list to FTP "
				"command file '%s'. %s",
				commandFileSpec, strerror (errno));
			(void) close (fd);
			(void) remove (commandFileSpec);
			free (writeBuffer);
			return (IMS_ERROR);
		}

		currFile = currFile->next;
	}

	/*
	** Write the bye statement.
	*/
	(void) sprintf (writeBuffer, "bye\n");

	if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the 'bye' statement to FTP "
			"command file '%s'. %s",
			commandFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (commandFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	(void) close (fd);
	free (writeBuffer);

	return (IMS_OK);
}

/******************************************************************************
**
** createShellFile ()
**
******************************************************************************/

static int createShellFile (
	IMS_MSG_STRUCT *msgDesc,
	FTP_SPEC *ftpInfo,
	char *commandFileSpec,
	char *homePath,
	char *shellFileSpec)
{
	char shellFileName[IMS_NAME_LEN+1];
	char *writeBuffer;
	int fd;

	/*
	** Allocate space for the writeBuffer.
	*/
	if ((writeBuffer = malloc (128)) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate space the shell file buffer.");
		return (IMS_FATAL);
	}

	/*
	** Assemble the file specification.
	*/
	(void) strcpy (shellFileName, "ftp.csh");
	ims_concatFilePath (shellFileSpec, homePath, shellFileName);

	/*
	** Create the FTP C shell file.
	*/
	if ((fd = open (shellFileSpec, O_RDWR|O_CREAT|O_EXCL)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open FTP C shell file '%s'. %s",
			shellFileSpec, strerror (errno));
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Change the mode of the file.
	*/
	if (chmod (shellFileSpec, 0700) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not change the mode for FTP C shell file '%s'. %s",
			shellFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (shellFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Write the C shell flag line.
	*/
	(void) sprintf (writeBuffer, "#!/bin/csh -f\n");

	if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the C shell flag to FTP C shell file '%s'. %s",
			shellFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (shellFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	/*
	** Write the ftp command and argument information.
	*/
	(void) sprintf (writeBuffer, "/bin/ftp -i -n %s < %s\n",
		ftpInfo->host, commandFileSpec);

	if ((int) write (fd, writeBuffer, strlen (writeBuffer)) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not write the 'ftp' command to FTP C shell file '%s'. %s",
			commandFileSpec, strerror (errno));
		(void) close (fd);
		(void) remove (shellFileSpec);
		free (writeBuffer);
		return (IMS_ERROR);
	}

	(void) close (fd);
	free (writeBuffer);

	return (IMS_OK);
}

/******************************************************************************
**
** freeFileList ()
**
** Free the FILE_LIST structure.
**
******************************************************************************/
 
static void freeFileList (
	FILE_LIST *currPtr)
{
	FILE_LIST *nextPtr;
		  
	while (currPtr != (FILE_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}

	return;
}

/******************************************************************************
**
** freePathList ()
**
** Free the PATH_LIST structure.
**
******************************************************************************/
 
static void freePathList (
	PATH_LIST *currPtr)
{
	PATH_LIST *nextPtr;
		  
	while (currPtr != (PATH_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}

	return;
}
