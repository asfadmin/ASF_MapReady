static char *sccs = "@(#)ims_filmStatusRet.c	5.1  03/17/96";
/* *************************************************************
**
** File:        ims_filmStatusRet.c
**
** Function:    An client program that uses the ims_filmStatus
**              generation routine.
**
** Author:      Sean Hardman and David Pass
**
** Date:        9/27/95
**
**************************************************************** */

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
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>
#include <ims_media.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static int checkTypes (IMS_MSG_STRUCT *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *itemId;
	char *orderId;
	char *inStatus;
	char *opComment;
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
	{"-I",           &commands.itemId},
	{"+itemId",      &commands.itemId},
	{"-O",           &commands.orderId},
	{"+orderId",     &commands.orderId},
	{"-S",           &commands.inStatus},
	{"+status",      &commands.inStatus},
	{"-Z",           &commands.opComment},
	{"+opComment",   &commands.opComment},
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

static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"itemId",      &commands.itemId},
	{"orderId",     &commands.orderId},
	{"status",      &commands.inStatus},
	{"opComment",   &commands.opComment},
	{"server",      &commands.server},
	{"database",    &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *statusString;
static DBINT orderId;
static DBSMALLINT itemId;
static DBSMALLINT inStatus;
static DBCHAR *opComment = (DBCHAR *) NULL;
static MEDIA_USER_SPEC userSpec;

/* *************************************************************
**
** main ()
**
** This is the driver for the status update after the
** Things-to-do-list (TTDL) for FPS has been generated.  It uses
** ims_filmStatus() function to do this.
**
**************************************************************** */

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */

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
		** Now, get command line arguments again to overlay file args.
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
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Update the order item's status.
	*/
	if ((status = ims_filmStatus (msgDesc, (char *) &userSpec,
		orderId, itemId, inStatus, opComment)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"The film status update was successful.");
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"An error occurred updating the film status.");
	(void) ims_msgStructFree (msgDesc);

	exit (1);
}   /*  main   */


/* *************************************************************
**
** runDown ()
**
** Cleanup and exit from program.
**
**************************************************************** */

static int runDown (
	int sig)
{
	/* Print out the signal caught. */
	(void) fprintf (stderr,
		"\n\nTermination of %s due to signal: %s (%d)\n\n",
		programName, ims_sigMsg (sig), sig);

	return (sig);
}   /*  runDown  */


/* *************************************************************
**
** usage ()
**
** Print command line argument switches.
**
**************************************************************** */

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


/* *************************************************************
**
** getArgInput ()
**
** Process command-line and command-file arguments.
**
**************************************************************** */

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	int status;
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

	/* orderId */
	number = 0;
	if (commands.orderId == (char *) NULL)
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
				((number < 1) || (number > IMS_MAX_INT)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '1' to '%d'. Try again.\n",
					IMS_MAX_INT);
			}
		}while (invalid);

		orderId = (DBINT) number;
	}
	else if (ims_isInteger (commands.orderId ) == IMS_TRUE)
	{
		number = (int) atoi (commands.orderId);
		if ((number < 1) || (number > IMS_MAX_INT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'orderId' has a value of '%d', which is not in the range of '1' to '%d'.",
				number, IMS_MAX_INT);
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
			"The parameter 'orderId' must contain a valid integer value.");
		return (IMS_ERROR);
	}

	/* itemId */
	number = 0;
	if (commands.itemId == (char *) NULL)
	{
		/* We expect a number. */
		do
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Item Id: ") == (char *) NULL)
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
				((number < 1) || (number > IMS_MAX_SINT)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '1' to '%d'. Try again.\n",
					IMS_MAX_SINT);
			}
		}while (invalid);

		itemId = (DBSMALLINT) number;
	}
	else if (ims_isInteger (commands.itemId) == IMS_TRUE)
	{
		number = (int) atoi (commands.itemId);
		if ((number < 1) || (number > IMS_MAX_SINT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'itemId' has a value of '%d', which is not in the range of '1' to '%d'.",
				number, IMS_MAX_SINT);
			return (IMS_ERROR);
		}
		else
		{
			itemId = (DBSMALLINT) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		"The parameter 'itemId' must contain a valid integer value.");
		return (IMS_ERROR);
	}

	/* inStatus */
	if (commands.inStatus != (char *) NULL)
	{
		statusString = commands.inStatus;
	}
	else
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

	/* opComment */
	if (commands.opComment != (char *) NULL)
	{
		/* Remove quotations if present. */
		opComment = ims_removeQuotes (commands.opComment);
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
}   /*  getArgInput */

/******************************************************************************
**
** checkTypes ()
**
** Check the status and convert it to its integer form.
**
******************************************************************************/

static int checkTypes (
	IMS_MSG_STRUCT *msgDesc)
{
	int status = IMS_OK;

	/*
	** Check the status.
	*/
	(void) ims_toUpper (statusString);

	if ((strcmp (statusString, "GENERATED") == 0) ||
		(strcmp (statusString, "GEN") == 0))
	{
		inStatus = FILM_GENERATED;
	}
	else if ((strcmp (statusString, "ERROR") == 0) ||
		(strcmp (statusString, "ERR") == 0))
	{
		inStatus = FILM_ERROR;
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
