static char *sccs = "@(#)ims_ftsClnt.c	5.2  03/13/97";
/******************************************************************************
**
** File:        ims_ftsClnt.c
**
** Function:    Main program for the client interface of the IMS File
**              Transfer System (FTS).
**
** Author:      J. Jacobson
**
** Date:        4/30/90
**
** Modified:    10/90 - H. Sayah - V15.0
**              Modified command arguments and options interface.
**
**              2/16/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files stdlib.h, ctype.h and string.h. 
**              Removed the include files sfoc.h and strings.h.
**              Replaced the calls to strdup() with calls to malloc()
**              and strcpy().
**
**              8/17/94 - H. Sayah, S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-IMS.
**
**              1/25/95 - S. Hardman - R1B
**              Removed the call to ims_clntInit(). We now make the message
**              facility calls to set up message queueing and Sybase message
**              handling.
**
**              2/20/95 - D. Crichton - R1B 
**              Added Account ID and the associated processing to the request
**              structure.
**
**              8/11/95 - S. Hardman - R1B
**              Changed the sensor prompt so that input is not required.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_archive.h>
#include <ims_version.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_signal.h>

/*
** Local Functions
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static int parseExtensions (IMS_MSG_STRUCT *);

/*
** Global Variables
*/
static char *programName;

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *event;
	char *username;
	char *password;
	char *accountId;
	char *platform;
	char *sensor;
	char *dataset;
	char *name;
	char *format;
	char *version;
	char *fileCount;
	char *extensions;
	char *sourceDir;
	char *localArchive;
	char *commandFile;
	char *catSrvName;
	char *catDbName;
	char *ftsSrvName;
	char *help;
	char *releaseNo;
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
	{"-A",           &commands.accountId},
	{"+accountId",   &commands.accountId},
	{"-L",           &commands.platform},
	{"+platform",    &commands.platform},
	{"-S",           &commands.sensor},
	{"+sensor",      &commands.sensor},
	{"-D",           &commands.dataset},
	{"+dataset",     &commands.dataset},
	{"-N",           &commands.name},
	{"+name",        &commands.name},
	{"-F",           &commands.format},
	{"+format",      &commands.format},
	{"-V",           &commands.version},
	{"+version",     &commands.version},
	{"-I",           &commands.fileCount},
	{"+fileCount",   &commands.fileCount},
	{"-T",           &commands.extensions},
	{"+extensions",  &commands.extensions},
	{"-O",           &commands.sourceDir},
	{"+sourceDir",   &commands.sourceDir},
	{"-R",           &commands.localArchive},
	{"+localArchive",&commands.localArchive},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.catSrvName},
	{"+catSrvName",  &commands.catSrvName},
	{"-Y",           &commands.catDbName},
	{"+catDbName",   &commands.catDbName},
	{"-Z",           &commands.ftsSrvName},
	{"+ftsSrvName",  &commands.ftsSrvName},
	{"-h",           &commands.help},
	{"+help",        &commands.help},
	{"-r",           &commands.releaseNo},
	{"+release",     &commands.releaseNo}
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
	{"accountId",   &commands.accountId},
	{"platform",    &commands.platform},
	{"sensor",      &commands.sensor},
	{"dataset",     &commands.dataset},
	{"name",        &commands.name},
	{"format",      &commands.format},
	{"version",     &commands.version},
	{"fileCount",   &commands.fileCount},
	{"extensions",  &commands.extensions},
	{"sourceDir",   &commands.sourceDir},
	{"localArchive",&commands.localArchive},
	{"catSrvName",  &commands.catSrvName},
	{"catDbName",   &commands.catDbName},
	{"ftsSrvName",  &commands.ftsSrvName}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Our request structure for granule events.
*/
static IMS_CLNT_EVENT request;

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
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	struct utsname uname_info;    /* Structure for uname() */
	char hostName[IMS_HOST_LEN+1];
	int status;

	/*
	** Get the program name and host name.
	*/
	programName = ims_extractFileName (argv[0]);
	request.programName = programName;
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Null-terminate just in case */

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
	** Assign the IMS_CLNT_EVENT message descriptor.
	*/
	request.msgDesc = msgDesc;

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
	** If releaseNo was specified, print it out.
	*/
	if (commands.releaseNo != (char *) NULL)
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
	** Update the request structure with information from the command
	** line or from a command file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
	 	goto ERROR;
	}

	/*
	** Perform the client's requested event.
	*/
	if ((status = ims_archive (&request)) < IMS_OK)
	{
	 	goto ERROR;
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);

	exit (0);


ERROR:
	
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);

	exit(1);

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
}

/******************************************************************************
**
** getArgInput ()
**
** Update the request structure and prompt for needed 
** information not provided in the commandLine and
** commandFile.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char event[IMS_COL10_LEN+1];
	int invalid;
	int i;
	int number;
	int status;
	int eventFlag;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/
	if (commands.event == (char *) NULL)
	{
		eventFlag = 0;
		do
		{
			if (ims_getString (IMS_TRUE, event, IMS_COL10_LEN,
				"Event (add, get, latest, delete, replace, who): ") 
				== (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			if (((strlen (event) == 1) && ((event[0] == 'a') ||
				(event[0] == 'g') || (event[0] == 'l') ||
				(event[0] == 'd') || (event[0] == 'r'))) ||
				(strcmp (event, "add") == 0) || 
				(strcmp (event, "get") == 0) ||
				(strcmp (event, "latest") == 0) || 
				(strcmp (event, "delete") == 0) ||
				(strcmp (event, "replace") == 0) ||
				(strcmp (event, "who") == 0))
			{
				eventFlag = 1;
			}
			else
			{
				(void) printf ("Event '%s' not supported. Try again.\n",
					event);
			}
		} while (eventFlag == 0);

		commands.event = event;
	}
	
	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		request.username = commands.username;
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

		request.username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		request.password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		request.password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.password, inputBuffer);
	}

	/* accountId */
	if (commands.accountId != (char *) NULL)
	{
		request.accountId = commands.accountId;
	}
	else 
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Account Id: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		request.accountId = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.accountId, inputBuffer);
	}

	/* platform */
	if (commands.platform != (char *) NULL)
	{
		request.platform = commands.platform;
	}
	else if (commands.event[0] != 'w')
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Platform: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		request.platform = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.platform, inputBuffer);
	}

	/* sensor */
	if (commands.sensor != (char *) NULL)
	{
		request.sensor = commands.sensor;
	}
	else if (commands.event[0] != 'w')
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Sensor: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		request.sensor = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.sensor, inputBuffer);
	}

	/* dataset */
	if (commands.dataset != (char *) NULL)
	{
		request.dataset = commands.dataset;
	}
	else if (commands.event[0] != 'w')
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Dataset: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		request.dataset = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.dataset, inputBuffer);
	}

	/* name */
	if (commands.name != (char *) NULL)
	{
		request.name = commands.name;
	}
	else
	{
		if ((commands.event[0] != 'l') &&
			(commands.event[0] != 'w'))
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Granule name: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			request.name = malloc (strlen (inputBuffer) + 1);
			(void) strcpy (request.name, inputBuffer);
		}
		else
		{
			/* Granule name is ignored for the latest event. */
			request.name = malloc (2);
			(void) strcpy (request.name, " ");
		}
	}

	/* format */
	if (commands.format != (char *) NULL)
	{
		request.format = commands.format;
	}
	else
	{
		if ((commands.event[0] != 'd') &&
			(commands.event[0] != 'g') &&
			(commands.event[0] != 'l') &&
			(commands.event[0]  != 'w'))
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Format: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			request.format = malloc (strlen (inputBuffer) + 1);
			(void) strcpy (request.format, inputBuffer);
		}
		else
		{
			/* Granule format is ignored for the delete event. */
			request.format = malloc (2);
			(void) strcpy (request.format, " ");
		}
	}

	/* version */
	number = -1;
	if (commands.version == (char *) NULL)
	{
		/* A value of -1 means versions are not supported. */
		request.version = (short) number;
	}
	else if (ims_isInteger (commands.version) == IMS_TRUE)
	{
		if ((number = abs ((int) atoi (commands.version))) > IMS_MAX_SINT)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'version' has a value of '%d' which is greater than the maximum value of '%d'.",
				number, IMS_MAX_SINT);
			return (IMS_ERROR);
		}
		else
		{
			request.version = (short) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'version' must have a valid integer value.");
		return (IMS_ERROR);
	}

	/* fileCount */
	number = 0;
	if (commands.fileCount == (char *) NULL)
	{
		if ((commands.event[0] == 'a') || (commands.event[0] == 'r'))
		{
			/* We expect a number. */
			do
			{
				if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
					"File count: ") == (char *) NULL)
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

				if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
					((number = abs ((int) atoi (inputBuffer)))
					> IMS_MAX_TINT))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is larger than '%d'. Try again.\n",
						IMS_MAX_TINT);
				}
			}while (invalid);
		}

		request.fileCount = (short) number;
	}
	else if (ims_isInteger (commands.fileCount) == IMS_TRUE)
	{
		if ((number = abs ((int) atoi (commands.fileCount))) > IMS_MAX_TINT)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'fileCount' has a value of '%d' which is greater than the maximum value of '%d'.",
				number, IMS_MAX_TINT);
			return (IMS_ERROR);
		}
		else
		{
			request.fileCount = (short) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'fileCount' must have a valid integer value.");
		return (IMS_ERROR);
	}

	/* extensions */
	if (commands.extensions != (char *) NULL)
	{
		if ((status = parseExtensions (msgDesc)) < IMS_OK)
		{
			return (status);
		}
	}
	else if ((commands.event[0] == 'a') || (commands.event[0] == 'r'))
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"File extensions (XXX, YYY, ...): ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		commands.extensions = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (commands.extensions, inputBuffer);

		if ((status = parseExtensions (msgDesc)) < IMS_OK)
		{
			return (status);
		}
	}

	/* sourceDir */
	if (commands.sourceDir != (char *) NULL)
	{
		request.sourceDir = commands.sourceDir;
	}
	else if ((commands.event[0] != 'd') &&
			 (commands.event[0] != 'w'))
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Directory path: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		request.sourceDir = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (request.sourceDir, inputBuffer);
	}

	/* localArchive */
	if (commands.localArchive == (char *) NULL)
	{
		request.localArchiveFlag = 'N';
	}
	else if ((commands.localArchive[0] == 'Y') ||
			 (commands.localArchive[0] == 'N'))
	{
		request.localArchiveFlag = commands.localArchive[0];
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'localArchive' must have a value of 'Y' or 'N'.");
		return (IMS_ERROR);
	}

	/* catSrvName */
	if (commands.catSrvName != (char *) NULL)
	{
		request.catSrvName = commands.catSrvName;
	}

	/* catDbName */
	if (commands.catDbName != (char *) NULL)
	{
		request.catDbName = commands.catDbName;
	}

	/* ftsSrvName */
	if (commands.ftsSrvName != (char *) NULL)
	{
		request.ftsSrvName = commands.ftsSrvName;
	}
	else if (commands.event[0] == 'w')
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"The FTS server name must be provided with the WHO event.");
		return(IMS_ERROR);
	}

	/*
	** Event switch, setup the request type.
	*/
	switch (commands.event[0])
	{
	case 'a':
		request.requestType = IMS_ADD;
		break;

	case 'g':
		request.requestType = IMS_GET;
		break;

	case 'd':
		request.requestType = IMS_DELETE;
		break;

	case 'l':
		request.requestType = IMS_GET_LATEST;
		break;

	case 'm':
		request.requestType = IMS_GET_MAX;
		break;

	case 'r':
		request.requestType = IMS_REPLACE;
		break;

	case 'w':
		request.requestType = IMS_WHO;
		break;

	default:
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Invalid event specified '%s'.", commands.event);
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** parseExtensions ()
**
** Parse the set of file extensions from the commands structure and put
** the resulting extensions into an array.
**
** The format for the commands.extensions string should be:
**	(MTA, LDR, DAT, TRL)
**
** Note: This function is not very robust at this time, meaning that
** any input varying significantly from the above example could cause
** a core dump. (S. Hardman 8/26/94)
**
******************************************************************************/

static int parseExtensions (
	IMS_MSG_STRUCT *msgDesc)
{
	char extTemp[IMS_COL255_LEN+1];   /* Temporary area for the extension. */
	char *p;                          /* Temporary pointers. */
	char *q;
	char *r;
	char *s;
	int extCount;                     /* Extensions processed counter. */
	int lastToken;                    /* Last token reached flag. */

	/*
	** Allocate memory for the extension pointer array.
	**
	** lint: pointer cast may result in improper alignment
	** No problem, calloc() aligns on worst case boundary.
	*/
	if ((request.extensions = (char **) calloc ((size_t) request.fileCount,
		(size_t) sizeof (char *))) == (char **) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Memory allocation for the extension pointer array failed.");
		return (IMS_FATAL);
	}

	s = commands.extensions;
	extCount = 0;
	lastToken = 0;

	/*
	** Parse the extensions until we reach the closing parenthesis.
	*/
	while (lastToken != 1)
	{
		extCount++;

		if ((p = strchr (s, ',')) == (char *) NULL)
		{
			if ((p = strchr (s, ')')) == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Syntax Error parsing file extensions: Token ')' not found.");
				return (IMS_ERROR);
			}
			else
			{
				lastToken = 1;
			}
		}
		q = s;

		while (isspace (*q))
			q++;

		if (*q == '(')
			q++;

		while (isspace (*q))
			q++;

		if (q >= p)
		{
			if (lastToken == 1)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Syntax Error parsing file extensions: Extra ',' found.");
				return (IMS_ERROR);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Syntax Error parsing file extensions: Developer oversight.");
				return (IMS_FATAL);
			}
		}

		r = extTemp;
		while (q < p)
		{
			*(r++) = *(q++);
			if (isspace (*q))
			{
				break;
			}
		}
		*(r) = '\0';
		
		if (lastToken == 0)
		{
			p++;
			q++;
			s = p;
		}
		else
		{
			/* If the counts don't match print out an error. */
			if (extCount < (int) request.fileCount)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Only %d extensions found when %d were expected.",
					extCount, request.fileCount);
				return (IMS_ERROR);
			}
			else if (extCount > (int) request.fileCount)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"%d extensions were found when %d were expected.",
					extCount, request.fileCount);
				return (IMS_ERROR);
			}
		}

		/*
		** Allocate space and copy the extension into the request structure.
		*/
		request.extensions[extCount-1] = (char *) malloc (strlen (extTemp)+1);
		(void) strcpy (request.extensions[extCount-1], extTemp);

		/* Clear out the temporary area. */
		extTemp[0] = '\0';
	}

	return (IMS_OK);
}
