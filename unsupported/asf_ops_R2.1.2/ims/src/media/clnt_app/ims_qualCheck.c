static char *sccs = "@(#)ims_qualCheck.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_qualCheck.c
**
** Function:    Quality check driver program.  This program calls the 
**              ims_qc() function which verifies CEOS products and then
**              generates a report.
**
** Author:      Sean Hardman
**
** Date:        8/17/95
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_media.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static int checkTypes (IMS_MSG_STRUCT *);
static void freeDeviceInfo (DEVICE_INFO *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *mediaType;
	char *mediaId;
	char *reportType;
	char *targetPath;
	char *archiveCheck;
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
	{"-M",           &commands.mediaType},
	{"+mediaType",   &commands.mediaType},
	{"-I",           &commands.mediaId},
	{"+mediaId",     &commands.mediaId},
	{"-R",           &commands.reportType},
	{"+reportType",  &commands.reportType},
	{"-T",           &commands.targetPath},
	{"+targetPath",  &commands.targetPath},
	{"-A",           &commands.archiveCheck},
	{"+archiveCheck",&commands.archiveCheck},
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
	{"mediaType",   &commands.mediaType},
	{"mediaId",     &commands.mediaId},
	{"reportType",  &commands.reportType},
	{"targetPath",  &commands.targetPath},
	{"archiveCheck",&commands.archiveCheck},
	{"server",      &commands.server},
	{"database",    &commands.database},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *glb_programName;
static char *mediaTypeStr;
static DBSMALLINT mediaType;
static char *mediaId = "";
static char *reportTypeStr;
static DBSMALLINT reportType;
static char *targetPath = (char *) NULL;
static char *archiveCheck = (char *) NULL;
static MEDIA_USER_SPEC userSpec;

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
	DEVICE_INFO *deviceInfo;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char qcPath[IMS_PATH_LEN+1];
	int tryCount;
	int mountStatus;
	int qcFlag;

	deviceInfo = (DEVICE_INFO *) NULL;

	/*
	** Get the program name and the node name.
	*/ 
	glb_programName = ims_extractFileName (argv[0]);
	userSpec.program = glb_programName;
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
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
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
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occurred parsing the command-line.");
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
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-file.");
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}

		/*
		** Now, get command line arguments again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-line.");
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
	** Allocate space for the DEVICE_INFO structure.
	**
	** lint: pointer cast may result in improper alignment
	** No problem, malloc() aligns on worst case boundary.
	*/
	if ((deviceInfo = (DEVICE_INFO *) malloc
		((size_t) sizeof (DEVICE_INFO))) ==
		(DEVICE_INFO *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for the DEVICE_INFO structure.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** If we are quality checking products on tape we either
	** use the given path specification or allocate a tape device
	** from the catalog.
	*/
	if (mediaType != IMS_DISK)
	{
		if (targetPath == (char *) NULL)
		{
			/*
			** Allocate a tape device based on mediaType requested.
			*/
			if ((status = ims_deviceAlloc (msgDesc, (char *) &userSpec, mediaType,
				NULL_ORDER, deviceInfo)) < IMS_OK)
			{
				freeDeviceInfo (deviceInfo);
				(void) ims_msgStructFree (msgDesc);
				exit (1);
			}

			/*
			** Grab the device path.
			*/
			(void) strcpy (qcPath, deviceInfo->path);

			/*
			** Prompt for tape to be mounted.
			*/
			tryCount = 0;
			do
			{
				(void) fprintf (stdout,
					"\nPlease mount tape in '%s', then type <Return>",
					deviceInfo->name);
				(void) gets (inputBuffer);

				mountStatus = ims_deviceTapeCheck (msgDesc, deviceInfo);
				tryCount += 1;
			} while ((mountStatus == IMS_WARNING) && (tryCount != IMS_MAXTRIES));

			if (mountStatus < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not mount tape in '%s'.",
					deviceInfo->name);
				(void) ims_deviceFree (msgDesc, (char *) &userSpec,
					deviceInfo->device_id);
				freeDeviceInfo (deviceInfo);
				(void) ims_msgStructFree (msgDesc);
				exit (1);
			}
		}
		else  /* A tape device path was given. */
		{
			(void) strcpy (qcPath, targetPath);
		}
	}
	else  /* The quality check will be run on disk files. */
	{
		if (targetPath != (char *) NULL)
		{
			(void) strcpy (qcPath, targetPath);
		}
		else  /* No path was provided for checking disk files. */
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"A path must be provided for quality checking disk files.");
			goto ERROR;
		}
	}

	/*
	** The archiveCheck argument determines whether the quality
	** check is performed on the files in the local archive or
	** whether it is performed on the target media.
	*/
	if (archiveCheck == (char *) NULL)
	{
		qcFlag = IMS_FALSE;
	}
	else
	{
		if ((archiveCheck[0] == 'y') || (archiveCheck[0] == 'Y'))
		{
			qcFlag = IMS_TRUE;
		}
		else
		{
			qcFlag = IMS_FALSE;
		}
	}

	/*
	** Perform the quality check.
	*/
	status = ims_qc (msgDesc, (char *) &userSpec, reportType,
		qcPath, mediaId, qcFlag);

	/*
	** Free an allocated tape device.
	*/
	if ((mediaType != IMS_DISK) && (targetPath == (char *) NULL))
	{
		(void) ims_deviceFree (msgDesc, (char *) &userSpec,
			deviceInfo->device_id);
	}

	/*
	** Check the status of the media distribution.
	*/
	if (status < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Quality check failed.");
		goto ERROR;
	}

	/*
	** Shutdown the message facility.
	*/
	freeDeviceInfo (deviceInfo);
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	freeDeviceInfo (deviceInfo);
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
		glb_programName, ims_sigMsg (sig), sig);

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
		"\n%s command-line arguments:\n\n", glb_programName);

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

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		userSpec.username = commands.username;
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

		userSpec.username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (userSpec.username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		userSpec.password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		userSpec.password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (userSpec.password, inputBuffer);
	}

	/* mediaType */
	if (commands.mediaType != (char *) NULL)
	{
		mediaTypeStr = commands.mediaType;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Media Type: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		mediaTypeStr = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (mediaTypeStr, inputBuffer);
	}

	/* reportType */
	if (commands.reportType != (char *) NULL)
	{
		reportTypeStr = commands.reportType;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Report Type: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		reportTypeStr = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (reportTypeStr, inputBuffer);
	}

	/* mediaId */
	if (commands.mediaId != (char *) NULL)
	{
		mediaId = commands.mediaId;
	}

	/* targetPath */
	if (commands.targetPath != (char *) NULL)
	{
		targetPath = commands.targetPath;
	}

	/* archiveCheck */
	if (commands.archiveCheck != (char *) NULL)
	{
		archiveCheck = commands.archiveCheck;
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		userSpec.server = commands.server;
	}

	/* database */
	if (commands.database != (char *) NULL)
	{
		userSpec.database = commands.database;
	}

	/*
	** Check the type arguments.
	*/
	status = checkTypes (msgDesc);

	if (status < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** checkTypes ()
**
** Check the media type and report type and convert it to its integer form.
**
******************************************************************************/

static int checkTypes (
	IMS_MSG_STRUCT *msgDesc)
{
	int status = IMS_OK;

	/*
	** Check the media type.
	*/
	(void) ims_toUpper (mediaTypeStr);

	if (strcmp (mediaTypeStr, "4-MM") == 0)
	{
		mediaType = IMS_4_MM;
	}
	else if (strcmp (mediaTypeStr, "4-MM HD") == 0)
	{
		mediaType = IMS_4_MM_HD;
	}
	else if (strcmp (mediaTypeStr, "8-MM") == 0)
	{
		mediaType = IMS_8_MM;
	}
	else if (strcmp (mediaTypeStr, "8-MM HD") == 0)
	{
		mediaType = IMS_8_MM_HD;
	}
	else if (strcmp (mediaTypeStr, "9-TRACK") == 0)
	{
		mediaType = IMS_9_TRACK;
	}
	else if (strcmp (mediaTypeStr, "9-TRACK HD") == 0)
	{
		mediaType = IMS_9_TRACK_HD;
	}
	else if (strcmp (mediaTypeStr, "DISK") == 0)
	{
		mediaType = IMS_DISK;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Media type '%s' is invalid.", mediaTypeStr);
		status = IMS_ERROR;
	}

	/*
	** Check the report type.
	*/
	(void) ims_toUpper (reportTypeStr);

	if (strcmp (reportTypeStr, "FULL") == 0)
	{
		reportType = FULL_REPORT;
	}
	else if (strcmp (reportTypeStr, "BRIEF") == 0)
	{
		reportType = BRIEF_REPORT;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Report type '%s' is invalid.", reportTypeStr);
		status = IMS_ERROR;
	}

	if (status < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** freeDeviceInfo ()
**
** Free the DEVICE_INFO structure.
**
******************************************************************************/
 
static void freeDeviceInfo (
	DEVICE_INFO *currPtr)
{
	DEVICE_INFO *nextPtr;
		  
	while (currPtr != (DEVICE_INFO *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}

	return;
}
