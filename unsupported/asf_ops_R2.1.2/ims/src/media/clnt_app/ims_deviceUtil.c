static char *sccs = "@(#)ims_deviceUtil.c	5.2  07/11/96";
/******************************************************************************
**
** File:        ims_deviceUtil.c
**
** Function:    A utility program that uses the device functions.
**
** Author:      Sean Hardman
**
** Date:        6/20/95
**
** Modified:    8/7/95 - C. Porter - R1B
**              Tape check case added.
**
**              9/20/95 - D. Pass - R1B'
**              Eject case added.
**
**              9/21/95 - S. Hardman - R1B'
**              Info case added.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_query.h>
#include <ims_media.h>
#include <ims_cmd.h>
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
	char *event;
	char *username;
	char *password;
	char *mediaType;
	char *mediaFormat;
	char *deviceId;
	char *orderId;
	char *status;
	char *opComment;
	char *targetPath;
	char *commandFile;
	char *server;
	char *database;
	char *queryFlag;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-E",           &commands.event},
	{"+event",       &commands.event},
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-M",           &commands.mediaType},
	{"+mediaType",   &commands.mediaType},
	{"-F",           &commands.mediaFormat},
	{"+mediaFormat", &commands.mediaFormat},
	{"-D",           &commands.deviceId},
	{"+deviceId",    &commands.deviceId},
	{"-O",           &commands.orderId},
	{"+orderId",     &commands.orderId},
	{"-S",           &commands.status},
	{"+status",      &commands.status},
	{"-Z",           &commands.opComment},
	{"+opComment",   &commands.opComment},
	{"-T",           &commands.targetPath},
	{"+targetPath",  &commands.targetPath},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-q",           &commands.queryFlag},
	{"+queryFlag",   &commands.queryFlag},
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
	{"event",       &commands.event},
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"mediaType",   &commands.mediaType},
	{"mediaFormat", &commands.mediaFormat},
	{"deviceId",    &commands.deviceId},
	{"orderId",     &commands.orderId},
	{"status",      &commands.status},
	{"opComment",   &commands.opComment},
	{"targetPath",  &commands.targetPath},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"queryFlag",   &commands.queryFlag},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *mediaTypeString = "NO-TYPE";
static char *mediaFmtString = "NO-TYPE";
static char *statusString = "NO-STATUS";
static DBSMALLINT mediaType;
static DBSMALLINT mediaFormat;
static DBSMALLINT deviceId;
static DBINT orderId;
static DBSMALLINT deviceStatus;
static DBCHAR *opComment;
static char *targetPath;
static MEDIA_USER_SPEC userSpec;
static int queryFlag = IMS_TRUE;
static char deviceEvent[IMS_COL10_LEN+1];

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
	DEVICE_INFO *infoPtr;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char mediaId[IMS_COL15_LEN+1];

	deviceInfo = (DEVICE_INFO *) NULL;

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
	** Get the command line arguments. The variable status will actually
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
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the commandline, except
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
		** Now, get command line arguments again to overlay file arguments.
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
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
	 	goto ERROR;
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
		goto ERROR;
	}

	/*
	** Perform the function specified by the event parameter.
	*/
	switch (commands.event[0])
	{
	case 'a':
		/*
		** Allocate a device based on mediaType requested.
		*/
		if ((status = ims_deviceAlloc (msgDesc, (char *) &userSpec, mediaType,
			orderId, deviceInfo)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not allocate a device for media type '%s'.",
				ims_mediaDesc (mediaType));
			goto ERROR;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"'%s' is allocated.", deviceInfo->name);
		}
		break;

	case 'c':
		/*
		** Change the status of the given device.
		*/
		if ((status = ims_deviceStatusChange (msgDesc, (char *) &userSpec,
			deviceId, deviceStatus, opComment)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not change the status of device '%d'.",
				deviceId);
			goto ERROR;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Device '%d' status is changed to '%s'.",
				deviceId, ims_statusDesc (deviceStatus));
		}
		break;

	case 'f':
		/*
		** Free an allocated device.
		*/
		if ((status = ims_deviceFree (msgDesc, (char *) &userSpec,
			deviceId)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not release device '%d'", deviceId);
			goto ERROR;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Device '%d' is freed.", deviceId);
		}
		break;

	case 'l':
		/*
		** Check the statuses of all devices.
		*/
		if ((status = ims_deviceStatusList (msgDesc, (char *) &userSpec,
			mediaType, deviceInfo)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not get the device status list.");
			goto ERROR;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"The device list for media type '%s' on host '%s' follows:",
				ims_mediaDesc (mediaType), hostName);
		}

		/*
		** Print out the device info list.
		*/
		infoPtr = deviceInfo;
		while (infoPtr != (DEVICE_INFO *) NULL)
		{
			(void) fprintf (stdout, "\ndevice_id: %d\n",
				infoPtr->device_id);
									 
			(void) fprintf (stdout, "name: %s\n",
				infoPtr->name);

			(void) fprintf (stdout, "host: %s\n",
				infoPtr->host);

			(void) fprintf (stdout, "path: %s\n",
				infoPtr->path);

			(void) fprintf (stdout, "path_extension: %s\n",
				infoPtr->path_extension);

			(void) fprintf (stdout, "description: %s\n",
				infoPtr->description);

			(void) fprintf (stdout, "status: %d\n",
				infoPtr->status);
																			   
			(void) fprintf (stdout, "order_id: %ld\n",
				infoPtr->order_id);

			(void) fprintf (stdout, "last_requested: %s\n",
				infoPtr->last_requested);

			(void) fprintf (stdout, "op_comment: %s\n",
				infoPtr->op_comment);

			(void) fprintf (stdout, "status_conflict: %d\n",
				infoPtr->status_conflict);
																			   
			infoPtr = infoPtr->next;
		}
		(void) fprintf (stdout, "\n");
		break;

	case 't':
		/*
		** Check the given device for a tape.
		*/
		deviceInfo->device_id = 0;
		(void) strcpy (deviceInfo->name, "Device 0");
		(void) strcpy (deviceInfo->path, targetPath);

		if ((status = ims_deviceTapeCheck (msgDesc, deviceInfo)) < IMS_OK)
		{
			goto ERROR;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO, "Device is ready.");
		}
		break;
		
	case 'e':
		/*
		** Rewind and eject the tape in the given device.
		*/
		deviceInfo->device_id = 0;
		(void) strcpy (deviceInfo->name, "Device 0");
		(void) strcpy (deviceInfo->path, targetPath);

		if ((status = ims_deviceTapeEject (msgDesc, deviceInfo)) < IMS_OK)
		{
			goto ERROR;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"The tape was rewound and ejected.");
		}
		break;
		
	case 'i':
		/*
		** Display configuration and status information for the device.
		*/
		deviceInfo->device_id = 0;
		(void) strcpy (deviceInfo->name, "Device 0");
		(void) strcpy (deviceInfo->path, targetPath);

		if ((status = ims_deviceTapeInfo (msgDesc, deviceInfo)) < IMS_OK)
		{
			goto ERROR;
		}
		break;
		
	case 'm':
		/*
		** Obtain the next media identifier for the given media type.
		*/
		if ((status = ims_deviceMediaId (msgDesc, (char *) &userSpec,
			mediaType, mediaFormat, mediaId, queryFlag)) < IMS_OK)
		{
			goto ERROR;
		}
		else
		{
			if (queryFlag == IMS_TRUE)
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"The next media identifier for media type '%s' will be '%s'.",
					ims_mediaDesc (mediaType), mediaId);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"The next media identifier for media type '%s' is '%s'.",
					ims_mediaDesc (mediaType), mediaId);
			}
		}
		break;
		
	default:
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Invalid event '%s'.", commands.event);
		goto ERROR;
	}


	/*
	** Free the device info structure.
	** Shutdown the message facility.
	*/
	freeDeviceInfo (deviceInfo);
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	/*
	** Free the device info structure.
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
	int eventFlag;
	int invalid;
	int i;
	int number;
	int status;
	size_t length;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* event */
	if (commands.event == (char *) NULL)
	{
		eventFlag = 0;
		do
		{
			if (ims_getString (IMS_TRUE, deviceEvent, IMS_COL10_LEN,
				"Event (alloc, change, free, list, tape, eject, info, media): ") 
				== (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			if (((strlen (deviceEvent) == 1) && ((deviceEvent[0] == 'a') ||
				(deviceEvent[0] == 'c') || (deviceEvent[0] == 'f') ||
				(deviceEvent[0] == 'l') || (deviceEvent[0] == 't') ||
				(deviceEvent[0] == 'e') || (deviceEvent[0] == 'i') ||
				(deviceEvent[0] == 'm'))) ||
				(strcmp (deviceEvent, "alloc") == 0) || 
				(strcmp (deviceEvent, "change") == 0) ||
				(strcmp (deviceEvent, "free") == 0) || 
				(strcmp (deviceEvent, "list") == 0) ||
				(strcmp (deviceEvent, "tape") == 0) ||
				(strcmp (deviceEvent, "eject") == 0) ||
				(strcmp (deviceEvent, "info") == 0) ||
				(strcmp (deviceEvent, "media") == 0))
			{
				eventFlag = 1;
			}
			else
			{
				(void) printf ("Event '%s' not supported. Try again.\n",
					deviceEvent);
			}
		} while (eventFlag == 0);

		commands.event = deviceEvent;
	}

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
		/* Allow for a null entry. */
		if ((length = strlen (commands.mediaType)) != 0)
		{
			mediaTypeString = commands.mediaType;
		}
	}
	else
	{
		if ((commands.event[0] == 'a') || (commands.event[0] == 'm'))
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Media Type: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			mediaTypeString = malloc (strlen (inputBuffer) + 1);
			(void) strcpy (mediaTypeString, inputBuffer);
		}
		else if (commands.event[0] == 'l')
		{
			if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Media Type: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			/* Allow for a null entry. */
			if ((length = strlen (inputBuffer)) != 0)
			{
				mediaTypeString = malloc (length + 1);
				(void) strcpy (mediaTypeString, inputBuffer);
			}
		}
	}

	/* mediaFormat */
	if (commands.mediaFormat != (char *) NULL)
	{
		mediaFmtString = commands.mediaFormat;
	}
	else
	{
		if (commands.event[0] == 'm')
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Media Format: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			mediaFmtString = malloc (strlen (inputBuffer) + 1);
			(void) strcpy (mediaFmtString, inputBuffer);
		}
	}

	/* deviceId */
	number = 0;
	if (commands.deviceId == (char *) NULL)
	{
		if ((commands.event[0] == 'c') || (commands.event[0] == 'f'))
		{
			/* We expect a number. */
			do
			{
				if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
					"Device Id: ") == (char *) NULL)
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
					((number < NULL_ORDER) || (number > IMS_MAX_SINT)))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is not in the range of '%d' to '%d'. Try again.\n",
						NULL_ORDER, IMS_MAX_SINT);
				}
			}while (invalid);
		}

		deviceId = (DBSMALLINT) number;
	}
	else if (ims_isInteger (commands.deviceId) == IMS_TRUE)
	{
		number = (int) atoi (commands.deviceId);
		if ((number < NULL_ORDER) || (number > IMS_MAX_SINT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'deviceId' has a value of '%d', which is not in the range of '%d' to '%d'.",
				number, NULL_ORDER, IMS_MAX_SINT);
			return (IMS_ERROR);
		}
		else
		{
			deviceId = (DBSMALLINT) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'deviceId' must have a valid integer value.");
		return (IMS_ERROR);
	}

	/* orderId */
	number = 0;
	if (commands.orderId == (char *) NULL)
	{
		if (commands.event[0] == 'a')
		{
			/* We expect a number. */
			do
			{
				if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
					"Order Id: ") == (char *) NULL)
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
					((number < NULL_ORDER) || (number > IMS_MAX_INT)))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is not in the range of '%d' to '%d'. Try again.\n",
						NULL_ORDER, IMS_MAX_INT);
				}
			}while (invalid);
		}

		orderId = (DBINT) number;
	}
	else if (ims_isInteger (commands.orderId) == IMS_TRUE)
	{
		number = (int) atoi (commands.orderId);
		if ((number < NULL_ORDER) || (number > IMS_MAX_INT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'orderId' has a value of '%d', which is not in the range of '%d' to '%d'.",
				number, NULL_ORDER, IMS_MAX_INT);
			return (IMS_ERROR);
		}
		else
		{
			orderId = (DBINT) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'orderId' must have a valid integer value.");
		return (IMS_ERROR);
	}

	/* status */
	if (commands.status != (char *) NULL)
	{
		statusString = commands.status;
	}
	else
	{
		if (commands.event[0] == 'c')
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Status: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			statusString = malloc (strlen (inputBuffer) + 1);
			(void) strcpy (statusString, inputBuffer);
		}
	}

	/* opComment */
	if (commands.opComment != (char *) NULL)
	{
		/* Remove quotations if present. */
		opComment = ims_removeQuotes (commands.opComment);
	}
	else
	{
		if (commands.event[0] == 'c')
		{
			if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Operator Comment: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			opComment = malloc (strlen (inputBuffer) + 1);
			(void) strcpy (opComment, inputBuffer);
		}
	}

	/* targetPath */
	if (commands.targetPath != (char *) NULL)
	{
		targetPath = commands.targetPath;
	}
	else
	{
		if ((commands.event[0] == 't') || (commands.event[0] == 'e') ||
			(commands.event[0] == 'i'))
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

	/* queryFlag */
	if (commands.queryFlag != (char *) NULL)
	{
		queryFlag = IMS_FALSE;
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
** Check the media type and status and convert them to their integer form.
**
******************************************************************************/

static int checkTypes (
	IMS_MSG_STRUCT *msgDesc)
{
	int status = IMS_OK;

	/*
	** Check the media type.
	*/
	(void) ims_toUpper (mediaTypeString);

	if (strcmp (mediaTypeString, "NO-TYPE") == 0)
	{
		mediaType = IMS_NO_MEDIA_TYPE;
	}
	else if (strcmp (mediaTypeString, "4-MM") == 0)
	{
		mediaType = IMS_4_MM;
	}
	else if (strcmp (mediaTypeString, "4-MM HD") == 0)
	{
		mediaType = IMS_4_MM_HD;
	}
	else if (strcmp (mediaTypeString, "8-MM") == 0)
	{
		mediaType = IMS_8_MM;
	}
	else if (strcmp (mediaTypeString, "8-MM HD") == 0)
	{
		mediaType = IMS_8_MM_HD;
	}
	else if (strcmp (mediaTypeString, "9-TRACK") == 0)
	{
		mediaType = IMS_9_TRACK;
	}
	else if (strcmp (mediaTypeString, "9-TRACK HD") == 0)
	{
		mediaType = IMS_9_TRACK_HD;
	}
	else if (strcmp (mediaTypeString, "DISK") == 0)
	{
		mediaType = IMS_DISK;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Media type '%s' is invalid.", mediaTypeString);
		status = IMS_ERROR;
	}

	/*
	** Check the media format.
	*/
	(void) ims_toUpper (mediaFmtString);

	if (strcmp (mediaFmtString, "NO-TYPE") == 0)
	{
		mediaFormat = IMS_NO_MEDIA_FORMAT;
	}
	else if (strcmp (mediaFmtString, "TAR") == 0)
	{
		mediaFormat = IMS_TAR;
	}
	else if (strcmp (mediaFmtString, "CEOS") == 0)
	{
		mediaFormat = IMS_CEOS;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Media format '%s' is invalid.", mediaFmtString);
		status = IMS_ERROR;
	}

	/*
	** Check the status.
	*/
	(void) ims_toUpper (statusString);

	if (strcmp (statusString, "NO-STATUS") == 0)
	{
		deviceStatus = DEVICE_NO_STATUS;
	}
	else if (strcmp (statusString, "AVAILABLE") == 0)
	{
		deviceStatus = DEVICE_AVAILABLE;
	}
	else if (strcmp (statusString, "IN-USE") == 0)
	{
		deviceStatus = DEVICE_INUSE;
	}
	else if (strcmp (statusString, "OFF-LINE") == 0)
	{
		deviceStatus = DEVICE_OFFLINE;
	}
	else if (strcmp (statusString, "IN-SERVICE") == 0)
	{
		deviceStatus = DEVICE_INSERVICE;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Status '%s' is invalid.", statusString);
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
