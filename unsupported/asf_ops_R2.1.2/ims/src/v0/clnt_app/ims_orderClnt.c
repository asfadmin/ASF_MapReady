static char *sccs = "@(#)ims_orderClnt.c	5.2  05/13/96";
/******************************************************************************
**
** File:        ims_orderClnt.c
**
** Function:
**
** Author:      Sean Hardman
**
** Date:        8/21/95
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>

#include <IK_Syslog.h>
#include <odldef.h>
#include <ims_order.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *fileSpec;
	char *syslogPath;
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
	{"-F",           &commands.fileSpec},
	{"+fileSpec",    &commands.fileSpec},
	{"-S",           &commands.syslogPath},
	{"+syslogPath",  &commands.syslogPath},
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
	{"fileSpec",    &commands.fileSpec},
	{"syslogPath",  &commands.syslogPath},
	{"server",      &commands.server},
	{"database",    &commands.database},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *fileSpec;
static char *server;
static char *database;
static char syslogPath[IMS_COL255_LEN+1];
static char syslogFile[IMS_COL30_LEN+1];

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
	AGGREGATE TxTree;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	FILE *filePtr;
	int orderId;

	TxTree = (AGGREGATE) NULL;
	orderId = 0;

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
			exit (1);
		}

		/*
		** Now, get command line arguments again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			exit (1);
		}
	}

	/*
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not obtain argument input.");
	 	exit (1);
	}

	/*
	** Open the IK syslog file.
	*/
	if (IK_NameSyslog (syslogPath) < 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Opening IK syslog '%s' failed.", syslogPath);
		exit (1);
	}

	/*
	** Open the input file.
	*/
	if ((filePtr = fopen (fileSpec, "r")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open the order file '%s'. %s",
			fileSpec, strerror (errno));
		goto ERROR;
	}

	/*
	** Allocate the structure for the root node of the ODL tree.
	*/
	if ((TxTree = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
		(AGGREGATE) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Unable to allocate the structure for the root node of the ODL tree.");
		(void) fclose (filePtr);
		goto ERROR;
	}

	/*
	** Read and parse the ODL formatted file into our allocated tree.
	*/
	if ((ReadLabel (filePtr, TxTree)) == 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occured while parsing the order file '%s'.",
			fileSpec);
		(void) fclose (filePtr);
		goto ERROR;
	}

	/*
	** Process the order.
	*/
	if ((status = ims_order (msgDesc, TxTree, &orderId)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not process the order.");
		(void) fclose (filePtr);
		goto ERROR;
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"Order '%d' was successfully submitted.", orderId);

	IK_CloseSyslog ();
	(void) unlink (syslogPath);
	(void) RemoveAggregate (TxTree);
	(void) ims_msgStructFree (msgDesc);
	(void) fclose (filePtr);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	IK_CloseSyslog ();
	(void) ims_msgIKSyslog (msgDesc, syslogPath);

	(void) ims_msg (msgDesc, IMS_ERROR,
		"Order submittal failed.");

	(void) unlink (syslogPath);

	if (TxTree != (AGGREGATE) NULL)
	{
		(void) RemoveAggregate (TxTree);
	}

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

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* fileSpec */
	if (commands.fileSpec != (char *) NULL)
	{
		fileSpec = commands.fileSpec;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"File Specification: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		fileSpec = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (fileSpec, inputBuffer);
	}

	/* syslogPath */
	(void) sprintf (syslogFile, "syslog_%s", programName);
	if (commands.syslogPath != (char *) NULL)
	{
		ims_concatFilePath (syslogPath, commands.syslogPath, syslogFile);
	}
	else
	{
		(void) sprintf (syslogPath, "./%s", syslogFile);
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
