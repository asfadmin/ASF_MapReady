static char *sccs = "@(#)ims_parseODLEx.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_parseODLEx.c
**
** Function:
**
** Author:      Sean Hardman
**
** Date:        1/5/96
**
** The following command will make the executible on SunOS Release 5.x:
**
** cc -I/asf/include/imsdads -L/asf/lib ims_parseODLEx.c \
** -lims -lodl_new -o ims_parseODLEx
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
#include <ims_timeConv.h>
#include <ims_keyword.h>

#include <IK_Syslog.h>
#include <odldef.h>
#include <ims_odl.h>

/*
** Local Definitions.
*/
#define BUFFER_SIZE 1024
#define LINE_SIZE 255

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
	char *targetObject;
	char *syslogPath;
	char *commandFile;
	char *server;
	char *database;
	char *bufferFlag;
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
	{"-T",           &commands.targetObject},
	{"+targetObject",&commands.targetObject},
	{"-S",           &commands.syslogPath},
	{"+syslogPath",  &commands.syslogPath},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-b",           &commands.bufferFlag},
	{"+bufferFlag",  &commands.bufferFlag},
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
	{"targetObject",&commands.targetObject},
	{"syslogPath",  &commands.syslogPath},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"bufferFlag",  &commands.bufferFlag}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *fileSpec;
static char *targetObject;
static char *server;
static char *database;
static char syslogPath[IMS_COL255_LEN+1];
static char syslogFile[IMS_COL30_LEN+1];
static int bufferFlag;

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
	IMS_KEYWORD_LIST *keywordList;
	IMS_KEYWORD_LIST *currList;
	IMS_NUMERIC_DATE dateStruct;
	FILE *fp;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char lineBuf[LINE_SIZE+1];
	char *odlBuffer;
	char *bufPtr;
	char *ptr;
	unsigned long index;
	int status;
	int bufSize;
	int currSize;

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
		exit (1);
	}

	/*
	** Get the command-line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occurred parsing the command-line.");
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
			exit (1);
		}

		/*
		** Now, get command-line arguments again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-line.");
			exit (1);
		}
	}

	/*
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not process argument input.");
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
	** Use the buffer function is requested.
	*/
	if (bufferFlag == IMS_TRUE)
	{
		/*
		** Open the input file.
		*/
		if ((fp = fopen (fileSpec, "r")) == (FILE *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not open the ODL file '%s'. %s",
				fileSpec, strerror (errno));
			status = IMS_ERROR;
			goto ERROR;
		}

		/*
		** Allocate space for the ODL buffer.
		*/
		if ((odlBuffer = (char *) malloc (BUFFER_SIZE+1)) == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for the ODL buffer.");
			status = IMS_FATAL;
			goto ERROR;
		}

		/*
		** Read the file into the buffer.
		*/
		bufPtr = odlBuffer;
		bufSize = BUFFER_SIZE;
		while ((ptr = fgets (lineBuf, LINE_SIZE, fp)) != (char *) NULL)
		{
			/* 
			** Place the Line just extracted into the buffer. 
			** If more storage space is required then allocate the
			** space.
			*/
			currSize = 0;
			while (*ptr != '\0')
			{
				currSize++;
				if (currSize >= bufSize)
				{
					/* Need more space. */
					index = bufPtr - odlBuffer;
					if ((odlBuffer = (char *) realloc (odlBuffer, BUFFER_SIZE)) ==
						(char *) NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL,
							"Could not reallocate memory for ODL buffer.");
						status = IMS_FATAL;
						goto ERROR;
					}

					bufSize += BUFFER_SIZE;
					bufPtr = odlBuffer + index;
				}
				*bufPtr = *ptr;
				ptr++;
				bufPtr++;
			}
			*bufPtr = '\0';
		}

		/*
		** Read and parse the ODL formatted buffer.
		*/
		if ((status = ims_parseODLBuffer (msgDesc, odlBuffer, targetObject,
			&keywordList)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"An error occured while parsing the ODL buffer.");
			goto ERROR;
		}
	}
	else
	{
		/*
		** Read and parse the ODL formatted file.
		*/
		if ((status = ims_parseODLFile (msgDesc, fileSpec, targetObject,
			&keywordList)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"An error occured while parsing the ODL file '%s'.",
				fileSpec);
			goto ERROR;
		}
	}

	/*
	** Print out the keyword list.
	*/
	currList = keywordList;
	while (currList != (IMS_KEYWORD_LIST *) NULL)
	{
		if (currList->item_type == IMS_OBJECT)
		{
			(void) printf ("\n%s\n", currList->keyword);
		}
		else /* Print out the associated keyword value. */
		{
			(void) printf ("\n%s = ", currList->keyword);

			switch (currList->data_type)
			{
				case IMS_INT4_TYPE:
					(void) printf ("%d\n", currList->value_integer);
					break;

				case IMS_FLOAT8_TYPE:
					(void) printf ("%f\n", currList->value_real);
					break;

				case IMS_DATETIME_TYPE:
					if ((status = ims_timeToNumericDate (msgDesc,
						currList->value_string, &dateStruct)) < IMS_OK)
					{
						(void) ims_msg (msgDesc, status,
							"The keyword '%s' has an invalid date/time value of '%s'.",
							currList->keyword, currList->value_string);
						break;
					}
	
				case IMS_SYMBOL_TYPE:
				case IMS_STRING_TYPE:
					(void) printf ("%s\n", currList->value_string);
					break;

				default:
					(void) ims_msg (msgDesc, IMS_ERROR,
						"Keyword '%s' has an invalid data type.",
						currList->keyword);
			}
		}

		currList = currList->next;
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"The ODL file '%s' was successfully parsed.",
		fileSpec);

	IK_CloseSyslog ();
	(void) remove (syslogPath);
	(void) ims_msgStructFree (msgDesc);
	(void) fclose (fp);
	ims_freeKeywordList (keywordList);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	IK_CloseSyslog ();
	(void) ims_msgIKSyslog (msgDesc, syslogPath);

	(void) ims_msg (msgDesc, status,
		"Parsing the ODL file '%s' was not successful.",
		fileSpec);

	(void) remove (syslogPath);
	(void) ims_msgStructFree (msgDesc);
	ims_freeKeywordList (keywordList);
					  
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

	/* targetObject */
	if (commands.targetObject != (char *) NULL)
	{
		targetObject = commands.targetObject;
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

	/* bufferFlag */
	if (commands.bufferFlag != (char *) NULL)
	{
		bufferFlag = IMS_TRUE;
	}
	else
	{
		bufferFlag = IMS_FALSE;
	}

	return (IMS_OK);
}
