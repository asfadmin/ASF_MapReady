static char *sccs = "@(#)ims_oagdr.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_oagdr.c
**
** Function:    This program will determine the integer value
**              for the selected permissions.
**
** Author:      Sean Hardman
**
** Date:        2/16/96
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_const.h>
#include <ims_msg.h>
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

/*
** Structure for getting arguments from the command-line, and also
** for getting them from the command-file.
*/
static struct commands
{
	char *order;
	char *add;
	char *get;
	char *delete;
	char *replace;
	char *commandFile;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the command-line.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-O",              &commands.order},
	{"+order",          &commands.order},
	{"-A",              &commands.add},
	{"+add",            &commands.add},
	{"-G",              &commands.get},
	{"+get",            &commands.get},
	{"-D",              &commands.delete},
	{"+delete",         &commands.delete},
	{"-R",              &commands.replace},
	{"+replace",        &commands.replace},
	{"-C",              &commands.commandFile},
	{"+commandFile",    &commands.commandFile},
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
	{"order",           &commands.order},
	{"add",             &commands.add},
	{"get",             &commands.get},
	{"delete",          &commands.delete},
	{"replace",         &commands.replace}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char order;
static char add;
static char get;
static char delete;
static char replace;

/*
** Bit Field structure definition.
*/
typedef struct
{
	unsigned unused1:  1;
	unsigned unused2:  1;
	unsigned unused3:  1;
	unsigned order:    1;
	unsigned add:      1;
	unsigned get:      1;
	unsigned delete:   1;
	unsigned replace:  1;
} OAGDR_BITS;

/*
** Union definition for mapping bits to the unsigned char.
*/
typedef union
{
	OAGDR_BITS oagdr_bits;
	unsigned char oagdr_uchar;
} OAGDR_UNION;


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
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	OAGDR_UNION oagdr_union;

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
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Determine the required permissions.
	*/
	oagdr_union.oagdr_bits.unused1 = 0;
	oagdr_union.oagdr_bits.unused2 = 0;
	oagdr_union.oagdr_bits.unused3 = 0;

	/* order */
	if ((order == 'y') || (order == '\0'))
	{
		oagdr_union.oagdr_bits.order = 1;
	}
	else
	{
		oagdr_union.oagdr_bits.order = 0;
	}

	/* add */
	if ((add == 'y') || (add == '\0'))
	{
		oagdr_union.oagdr_bits.add = 1;
	}
	else
	{
		oagdr_union.oagdr_bits.add = 0;
	}

	/* get */
	if ((get == 'y') || (get == '\0'))
	{
		oagdr_union.oagdr_bits.get = 1;
	}
	else
	{
		oagdr_union.oagdr_bits.get = 0;
	}

	/* delete */
	if ((delete == 'y') || (delete == '\0'))
	{
		oagdr_union.oagdr_bits.delete = 1;
	}
	else
	{
		oagdr_union.oagdr_bits.delete = 0;
	}

	/* replace */
	if ((replace == 'y') || (replace == '\0'))
	{
		oagdr_union.oagdr_bits.replace = 1;
	}
	else
	{
		oagdr_union.oagdr_bits.replace = 0;
	}

	/*
	** Print out the unsigned char permission value.
	*/
	(void) fprintf (stdout, "\noagdr value: '%u'\n\n",
		oagdr_union.oagdr_uchar);

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"Could not determine the oagdr integer permission value.");
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
	(void) fprintf (stderr,
		"\n\nTermination of %s due to signal: %s (%d)\n\n",
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
** Process command-line and command-file arguments.
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

	/* order */
	if (commands.order != (char *) NULL)
	{
		order = commands.order[0];
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Order (y|n) {y}: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		order = inputBuffer[0];
	}

	/* add */
	if (commands.add != (char *) NULL)
	{
		add = commands.add[0];
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Add (y|n) {y}: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		add = inputBuffer[0];
	}

	/* get */
	if (commands.get != (char *) NULL)
	{
		get = commands.get[0];
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Get (y|n) {y}: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		get = inputBuffer[0];
	}

	/* delete */
	if (commands.delete != (char *) NULL)
	{
		delete = commands.delete[0];
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Delete (y|n) {y}: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		delete = inputBuffer[0];
	}

	/* replace */
	if (commands.replace != (char *) NULL)
	{
		replace = commands.replace[0];
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Replace (y|n) {y}: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		replace = inputBuffer[0];
	}

	return (IMS_OK);
}
