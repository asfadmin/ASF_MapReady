static char *sccs = "@(#)ims_dbq.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_dbq.c
**
** Function:    Execute an SQL command that contains one or more queries
**              and write the output buffer to stdout.
**
** Author:      J. Rector
**
** Date:        2/21/90
**
** Modified:    4/27/93 - M. Sarrel
**              Taken from the original AMMOS-CDB dbq application.
**              Modifications for the MDMS environment. Added the
**              ability to supress headers and the ability to choose
**              the format of the output.
**
**              9/28/94 - S. Hardman - R0
**              ASF adaptation of code inherited from MDMS.
**              Modified for the ASF IMS Query Interface and Message
**              Facility libraries. Added the help and release
**              arguments and cleaned up the argument validation code.
**
** Notes:
**
** 1. This program allows a specific dataserver userName and password to be
**    used, or uses the default of 'ims_clnt'.
**
** 2. The syntax for using this program is:
**
**    ims_dbq [-U <username>] [-P <password>]
**            [-S <cat_srv_name>] [-D <cat_db_name>]
**            [-H {y|n}] [-F {table|list|export}] [-B <buffer_size>]
**            [-O <output_file>] [-C <command_file>] [-h] [-r] <command> 
**
**    The -H argument defaults to 'y' and the -F flag defaults to 'table'.
**
** For a description, see man page ims_dbq(1).
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
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_version.h>
#include <ims_util.h>
#include <ims_signal.h>

/*
** Local Functions
*/
static int runDown (int);
static void usage (void);
static int checkArguments (IMS_MSG_STRUCT *msgDesc);
static int initQiDesc (char *, IMS_QI_DESC_OBJ *);
static int processCommandFile (IMS_QI_DESC_OBJ *, FILE *);
static int qiPrintResults (IMS_QI_DESC_OBJ *, FILE *);
static void displayAttrHdr (IMS_QI_DESC_OBJ *, FILE *);
static int displayRowsExport (IMS_QI_DESC_OBJ *, FILE *);
static int displayRows (IMS_QI_DESC_OBJ *, FILE *);
static int displayRowsAsList (IMS_QI_DESC_OBJ *, FILE *);
static int resultsStatus (IMS_QI_DESC_OBJ *, FILE *);
static int printRets (IMS_QI_DESC_OBJ *, FILE *);

/*
** Misc. defines.
*/
#define CMD_BUF_EXT 1024

/*
** Common structure for command line args.
*/
typedef struct cmdLine
{
	char *userName;
	char *password;
	char *server;
	char *dbname;
	char *buffers;
	char *headers;
	char *format;
	char *commandFile;
	char *outputFile;
	char *help;
	char *releaseNo;
} IMS_CMDLINE_ARGS;
static IMS_CMDLINE_ARGS cmdLineArg;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm[] =
{
	{"-U",           &cmdLineArg.userName},
	{"+username",    &cmdLineArg.userName},
	{"-P",           &cmdLineArg.password},
	{"+password",    &cmdLineArg.password},
	{"-S",           &cmdLineArg.server},
	{"+catSrvName",  &cmdLineArg.server},
	{"-D",           &cmdLineArg.dbname},
	{"+catDbName",   &cmdLineArg.dbname},
	{"-B",           &cmdLineArg.buffers},
	{"+buffers",     &cmdLineArg.buffers},
	{"-H",           &cmdLineArg.headers},
	{"+headers",     &cmdLineArg.headers},
	{"-F",           &cmdLineArg.format},
	{"+format",      &cmdLineArg.format},
	{"-C",           &cmdLineArg.commandFile},
	{"+commandFile", &cmdLineArg.commandFile},
	{"-O",           &cmdLineArg.outputFile},
	{"+outputFile",  &cmdLineArg.outputFile},
	{"-h",           &cmdLineArg.help},
	{"+help",        &cmdLineArg.help},
	{"-r",           &cmdLineArg.releaseNo},
	{"+release",     &cmdLineArg.releaseNo},
};
static int cmdLineCnt = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** Enumeration for output format.
*/
enum outputFormat
{
	NO_FORMAT,
	TABLE,
	LIST,
	EXPORT
};
static enum outputFormat format = TABLE;

/*
** Static Variables
*/
static char *glb_programName;

/******************************************************************************
**
** main ()
**
** Form the procedure, with its arguments, from the program command buffer.
** Initialize query parameters.
** Execute the procedure and have the results printed to stdout.
**
******************************************************************************/

void main (
	int argc,
	char **argv)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	struct utsname uname_info;    /* Structure for uname() */
	char hostName[IMS_HOST_LEN+1];
	FILE *out_fp;
	int i;
	int count;
	int bufSize;
	static char *command = (char *) NULL;

	/*
	** Get the program name and host name.
	*/
	glb_programName = ims_extractFileName (argv[0]);
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
		goto ERROR;
	}

	/*
	** Parse the command-line arguments.
	*/
	if ((count = ims_getCmdLine (argc, argv, cmdLineElm, cmdLineCnt,
		msgDesc)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** If releaseNo was specified, print it out.
	*/
	if (cmdLineArg.releaseNo != (char *) NULL)
	{
		(void) ims_printVersion (stderr);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (cmdLineArg.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** Validate the command-line arguments.
	*/
	if (checkArguments (msgDesc) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Set up the output file.
	*/
	if (cmdLineArg.outputFile == (char *) NULL)
	{
		out_fp = stdout;
	}
	else if ((out_fp = fopen (cmdLineArg.outputFile, "w")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Unable to open output file specified by the -O argument.");
		goto ERROR;
	}

	/*
	** Check for remaining stuff on the command-line.
	*/
	if (count < argc)
	{
		bufSize = 0;
		for (i = count; i < argc; i++)
		{
			bufSize += strlen (argv[i]) + 1;
		}
		bufSize += 1;	/* Null terminator. */

		if ((command = (char *) malloc (bufSize)) == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Malloc error on command buffer of size %d.",
				bufSize);
			goto ERROR;
		}
		command[0] = '\0';

		for (i = count; i < argc; i++)
		{
			(void) strcat (command, argv[i]);

			if (i != argc-1)
			{
				(void) strcat (command, " ");
			}
		}
	}

	/*
	** There should be either a command file or a command specified
	** on the command-line.
	*/
	if ((command == (char *) NULL) &&
		(cmdLineArg.commandFile == (char *) NULL))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No command or command-file specified.");
		goto ERROR;
	}

	if ((command != (char *) NULL) &&
		(cmdLineArg.commandFile != (char *) NULL))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Use either a command or a command-file.");
		goto ERROR;
	}

	/*
	** Get a query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		goto ERROR;
	}

	/*
	** Initialize the query descriptor structure.
	*/
	if (initQiDesc (command, qDesc) < IMS_OK)
	{
		(void) ims_qiFreeDesc (qDesc);
		goto ERROR;
	}

	/*
	* Execute the command file, otherwise, just execute the single command.
	*/
	if (cmdLineArg.commandFile != (char *) NULL)
	{
		/*
		** Read the command file, execute all of its commands.  Commands
		** are delimited by a "go".
		*/
		if (processCommandFile (qDesc, out_fp) < IMS_OK)
		{
			(void) ims_qiExit ();
			goto ERROR;
		}
	}
	else
	{
		/*
		** Execute the procedure, returning column headers and data rows for
		** each query in the procedure.
		*/
		if (qiPrintResults (qDesc, out_fp) < IMS_OK)
		{
			(void) ims_qiExit ();
			goto ERROR;
		}
	}

	/*
	** Cancel all queries, free the query descriptor and free the
	** message descriptor.
	*/
	(void) ims_qiExit ();
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
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

	for (i = 0; i < cmdLineCnt; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}

	(void) fprintf (stderr, "\n\n");
}

/******************************************************************************
**
** checkArguments ()
**
** Check the command-line arguments for validity.
**
******************************************************************************/

static int checkArguments (
	IMS_MSG_STRUCT *msgDesc)
{
	int status;
	int i;

	status = IMS_OK;

	/* username and password */
	if ((cmdLineArg.userName == (char *) NULL) &&
		(cmdLineArg.password == (char *) NULL))
	{
		cmdLineArg.userName = "ims_clnt";
		cmdLineArg.password = "ims_clnt";
	}
	else if ((cmdLineArg.userName == (char *) NULL) &&
		(cmdLineArg.password != (char *) NULL))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Must specify a value for username if a value for password is specified.");
		status = IMS_ERROR;
	}

	/* catSrvName */
	if ((cmdLineArg.server != (char *) NULL) &&
		(strcmp (cmdLineArg.server, "") == 0))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Must specify a value for server.");
		status = IMS_ERROR;
	}

	/* catDbName */
	if ((cmdLineArg.dbname != (char *) NULL) &&
		(strcmp (cmdLineArg.dbname, "") == 0))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Must specify a value for database.");
		status = IMS_ERROR;
	}

	/* headers */
	if (cmdLineArg.headers != (char *) NULL)
	{
		for (i = 0; i < (int) strlen (cmdLineArg.headers); i++)
		{
			cmdLineArg.headers[i] = tolower (cmdLineArg.headers[i]);
		}

		if ((strcmp (cmdLineArg.headers, "y") != 0) &&
			(strcmp (cmdLineArg.headers, "n") != 0))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Must specify \"y\" or \"n\" for headers.");
			status = IMS_ERROR;
		}
	}

	/* format */
	if (cmdLineArg.format != (char *) NULL)
	{
		for (i = 0; i < (int) strlen (cmdLineArg.format); i++)
		{
			cmdLineArg.format[i] = tolower (cmdLineArg.format[i]);
		}

		if (strcmp (cmdLineArg.format, "table") == 0)
		{
			format = TABLE;
		}
		else if (strcmp (cmdLineArg.format, "list") == 0)
		{
			format = LIST;
		}
		else if (strcmp (cmdLineArg.format, "export") == 0)
		{
			format = EXPORT;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Must specify \"table\", \"list\" or \"export\" for format.");
			status = IMS_ERROR;
		}
	}

	/* buffers */
	if ((cmdLineArg.buffers != (char *) NULL) &&
		(strcmp (cmdLineArg.buffers, "") == 0))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Must specify a value for buffers.");
		status = IMS_ERROR;
	}

	/* Check status. */
	if (status != IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** initQiDesc ()
**
** Initialize values in the query descriptor.
**
******************************************************************************/

static int initQiDesc (
	char *command,
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;

	IMS_SETUSER (qDesc, cmdLineArg.userName);

	if (cmdLineArg.password == (char *) NULL)
	{
		IMS_SETPSWD (qDesc, "");
	}
	else
	{
		IMS_SETPSWD (qDesc, cmdLineArg.password);
	}

	IMS_SETPROG (qDesc, glb_programName);

	IMS_SETCMD (qDesc, command);

	/*
	** Set row buffering count if specified on command line.
	*/
	if (cmdLineArg.buffers != (char *) NULL)
	{
		IMS_SETBUFCOUNT (qDesc, cmdLineArg.buffers);
	}
   
	/*
	** Set server if supplied on command line.
	*/
	if (cmdLineArg.server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, cmdLineArg.server);
	}

	/*
	** Set database if supplied on command line.
	*/
	if (cmdLineArg.dbname != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, cmdLineArg.dbname);
	}

	/*
	** Make sure we get all of the messages back.
	*/
	IMS_SET_VERBOSE (qDesc, 10);

	/*
	** Make sure the data returned is by ims_qiNextRow is ALL null
	** terminated.
	*/
	IMS_SETATTRSNTS (qDesc);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	return (IMS_OK);
}

/******************************************************************************
**
** processCommandFile ()
**
******************************************************************************/

static int processCommandFile (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	IMS_MSG_STRUCT *msgDesc;
	FILE *fp;
	int cmdBufSize;
	int bytesUsed;           /* Always includes space for '\0'. */
	char *p;
	char *cmdBuffer;
	int inLineLen;
	int lineCount;
	char inBuf [2048];

	/* Assign values. */
	msgDesc = qDesc->msgDesc;
	cmdBuffer = (char *) NULL;
	lineCount = 0;

	if (strcmp (cmdLineArg.commandFile, "stdin") == 0)
	{
		fp = stdin;
	}
	else if ((fp = fopen (cmdLineArg.commandFile, "r")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not open command file %s. %s",
			cmdLineArg.commandFile, strerror (errno));
		return (IMS_ERROR);
	}

	cmdBufSize = CMD_BUF_EXT * 2;
	if ((cmdBuffer = (char *) malloc (cmdBufSize)) == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate command buffer.");
		(void) fclose (fp);
		return (IMS_FATAL);
	}

	p = cmdBuffer;
	*p = '\0';
	bytesUsed = 1;

	/*
	** Now, start reading the file and append each line to the command
	** we are accumulating.  Ship when we see a go.  Note: We always
	** want to see the line numbers.  Just don't show the input if
	** it comes from stdin, since usually, this will be interactive.
	*/
	(void) fprintf (stderr, "%d> ", ++lineCount);
	(void) fflush (stderr);

	while (fgets (inBuf, sizeof (inBuf), fp) != (char *) NULL)
	{
		if (strcmp (inBuf, "go\n") == 0 ||
			(strncmp (inBuf, "go", 2) == 0 &&
			isspace (inBuf[2])))
		{
			if (fp != stdin)
			{
				(void) fprintf (stderr, "%d> ", ++lineCount);
				(void) fprintf (stderr, "%s", inBuf);
			}

			IMS_SETCMD (qDesc, cmdBuffer);

			/*
			** If its just an error or warning, just continue on.
			*/
			if (qiPrintResults (qDesc, out_fp) == IMS_FATAL)
			{
				(void) fclose (fp);
				(void) free (cmdBuffer);
				return (ims_msgGetSeverity (msgDesc));
			}

			/*
			** Now that the command has been shipped, reset our pointers, etc.
			*/
			p = cmdBuffer;
			*p = '\0';
			bytesUsed = 1;
			lineCount = 0;

			if (fp != stdin)
			{
				continue;
			}
		}
		else if (strcmp (inBuf, "exit\n") == 0 ||
			strcmp (inBuf, "quit\n") == 0)
		{
			break;
		}
		else
		{
			inLineLen = strlen (inBuf);
			if (bytesUsed + inLineLen >= cmdBufSize)
			{
				cmdBufSize = cmdBufSize + inLineLen + CMD_BUF_EXT;
				if ((cmdBuffer = (char *) realloc ((void *) cmdBuffer,
					cmdBufSize)) == (char *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Could not re-allocate command buffer.");
					(void) fclose (fp);
					return (IMS_FATAL);
				}
			}

			p = cmdBuffer + bytesUsed - 1;
			(void) strcpy (p, inBuf);
			bytesUsed += inLineLen;
		}

		(void) fprintf (stderr, "%d> ", ++lineCount);

		if (fp != stdin)
		{
			(void) fprintf (stderr, "%s", inBuf);
		}
		(void) fflush (stderr);
	}
	(void) free (cmdBuffer);
	(void) fclose (fp);

	return (IMS_OK);
}

/******************************************************************************
**
** qiPrintResults ()
**
******************************************************************************/

static int qiPrintResults (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	int status;
	IMS_MSG_STRUCT *msgDesc = qDesc->msgDesc;

	/*
	** Get the description of the returned attributes and display all of
	** the data until ENDOFTRANSACTION is reached.
	*/

	/*
	** Get Attribute Description for next query until END OF TRANSACTION
	** is reached ... then return
	*/
	while ((status = ims_qiTblDesc (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/*
		** Make sure we got the Description without error.
		*/
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}

		/*
		** If the the status is IMS_ENDOFQUERY, then we have no rows or
		** results to check, so let's go back up and see if there are any
		** results for yet another query within the command.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			/*
			** Print any returned parameters to the screen.
			*/
			(void) printRets (qDesc, out_fp);
			continue;
		}

		/*
		** Check to make sure that rows were actually returned.	If so
		** format and display the Header and rows returned by the query.
		*/
		if (IMS_ATTRCOUNT (qDesc) > (short) 0)
		{
			switch (format)
			{
			case LIST:
				if ((status = displayRowsAsList (qDesc, out_fp))
					< IMS_ROWRETURNED)
				{
					return (ims_msgGetSeverity (msgDesc));
				}
				break;

			case EXPORT:
				if ((status = displayRowsExport (qDesc, out_fp))
					< IMS_ROWRETURNED)
				{
					return (ims_msgGetSeverity (msgDesc));
				}
				break;

			case TABLE:
				if ((cmdLineArg.headers == (char *) NULL) ||
					(strcmp (cmdLineArg.headers, "yes") == 0))
				{
					displayAttrHdr (qDesc, out_fp);
				}

				if ((status = displayRows (qDesc, out_fp)) < IMS_ROWRETURNED)
				{
					return (ims_msgGetSeverity (msgDesc));
				}
				break;

			default:
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Invalid format type.");
				return (IMS_FATAL);
			}
		}
      
		/*
		** Print the query results to the screen.
		*/
		if (((cmdLineArg.headers == (char *) NULL) ||
			(strcmp (cmdLineArg.headers, "yes") == 0)) && format != EXPORT)
		{
			if (resultsStatus (qDesc, out_fp) < IMS_OK)
			{
				return (ims_msgGetSeverity (msgDesc));
			}
		}

		/*
		** Check the message number returned by SYBASE to make sure the
		** call was successful.
		*/
		if (IMS_MSGNO (qDesc) != 0)
		{
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	return (status);
}

/******************************************************************************
**
** displayAttrHdr ()
**
** From the Attribute information stored in qDesc, printout the Attribute
** Header for the query.
**
******************************************************************************/

static void displayAttrHdr (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	char fmt[15];
	long maxAttrLength;
	int i, j;

	/*
	** For each attribute returned by the query, find out the maximum
	** length between the Attribute Name and the maximum length of the
	** Attribute data. Use this maximum length to generate a format
	** statement which left justifies the output string (IMS_ATTRNAME)
	** and pads any remaining difference from the maximum length with
	** spaces.
	*/
	(void) fprintf (out_fp, "\n\n");

	for (i = 0; i < (int) IMS_ATTRCOUNT (qDesc); i++)
	{
		maxAttrLength =
			IMS_MAX (IMS_ATTRLENGTH (qDesc, i), IMS_ATTRNAMELENGTH (qDesc, i));
		(void) sprintf (fmt, "%%-%ds ", maxAttrLength);
		(void) fprintf (out_fp, fmt, IMS_ATTRNAME (qDesc, i));
	}

	/*
	** Now that all column headers have been printed, start a new line
	** for the line separator.
	*/
	(void) fprintf (out_fp, "\n");

	/*
	** Now print out a separator line of dashes for each Attribute with
	** the maximum Attribute length
	*/
	for (i = 0; i < (int) IMS_ATTRCOUNT (qDesc); i++)
	{
		maxAttrLength =
			IMS_MAX (IMS_ATTRLENGTH (qDesc, i), IMS_ATTRNAMELENGTH (qDesc, i));

		for (j = 0; j < maxAttrLength; j++)
		{
			(void) fprintf (out_fp, "%c", '-');
		}
		(void) fprintf (out_fp, "%c", ' ');
	}

	/*
	** Now that the line seperator has been printed, start a new line
	** for the following rows of data.
	*/
	(void) fprintf (out_fp, "\n");

	return;
}

/******************************************************************************
**
** displayRowsExport ()
**
** Display each row returned by the query in export format.
**
******************************************************************************/

static int displayRowsExport (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	char *attrValue;
	int i;
	int status;

	while ((status = ims_qiNextRow (qDesc)) == IMS_ROWRETURNED)
	{
		/*
		** For each attribute of the returned row, find out the maximum
		** length. Use the maximum length (between the Attribute Name and
		** max Value length) to generate a format statement which left
		** justifies the output string (IMS_ATTRNAME) and pads any
		** remaining difference from the maximum length with spaces.
		*/
		for (i = 0; i < (int) IMS_ATTRCOUNT (qDesc); i++)
		{
			if (strcmp ((attrValue = IMS_VALUE (qDesc, i)), "") == 0)
			{
				attrValue = "NULL";
			}
			(void) fprintf (out_fp, "%s\t", attrValue);
		}

		/*
		** Now that the row has been printed, start a new line for the
		** next row.
		*/
		(void) fprintf (out_fp, "\n");
	}

   return (status);
}

/******************************************************************************
**
** displayRows ()
**
** Display each row returned by the query.
**
******************************************************************************/

static int displayRows (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	char fmt[15];
	char *attrValue;
	int i;
	long maxAttrLength;
	int status;

	while ((status = ims_qiNextRow (qDesc)) == IMS_ROWRETURNED)
	{
		/*
		** For each attribute of the returned row, find out the maximum
		** length Use the maximum length (between the Attribute Name and
		** max Value length) to generate a format statement which left
		** justifies the output string (IMS_ATTRNAME) and pads any
		** remaining difference from the maximum length with spaces.
		*/

		for (i = 0; i < (int) IMS_ATTRCOUNT (qDesc); i++)
		{
			if (strcmp ((attrValue = IMS_VALUE (qDesc, i)), "") == 0)
			{
				attrValue = "NULL";
			}

			maxAttrLength =
				IMS_MAX (IMS_ATTRLENGTH (qDesc, i), IMS_ATTRNAMELENGTH (qDesc, i));
			(void) sprintf (fmt, "%%-%ds ", maxAttrLength);
			(void) fprintf (out_fp, fmt, attrValue);
		}

		/*
		** Now that the row has been printed, start a new line for the
		** next row.
		*/
		(void) fprintf (out_fp, "\n");
	}

	return (status);
}

/******************************************************************************
**
** displayRowAsList ()
**
** Display each row returned by the query.
**
******************************************************************************/

static int displayRowsAsList (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	char *attrValue;
	int i;
	int rowCount = 0;
	int status;

	while ((status = ims_qiNextRow (qDesc)) == IMS_ROWRETURNED)
	{
		/*
		** Print out name = value\n format of the row.
		*/
		(void) fprintf (out_fp, "\nRow %d>\n", ++rowCount);

		for (i = 0; i < (int) IMS_ATTRCOUNT (qDesc); i++)
		{
			if (strcmp ((attrValue = IMS_VALUE (qDesc, i)), "") == 0)
			{
				attrValue = "NULL";
			}

			(void) fprintf (out_fp, "\t%s = %s\n",
			IMS_ATTRNAME (qDesc, i), attrValue);
		}

		/*
		** Now that the row has been printed, start a new line for the
		** next row.
		*/
		(void) fprintf (out_fp, "\n");
	}

	return (status);
}

/******************************************************************************
**
** printRets ()
**
** Converts all stored procedure OUTPUT returned parmeters to character
** strings, and outputs them to the screen.
**
******************************************************************************/

static int printRets (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	int numRets = IMS_RETVALUECOUNT (qDesc);
	int i;
	BYTE buf[512];

	for (i = 0; i < numRets; i++)
	{
		IMS_RETVALUENTS (qDesc, i, buf, (DBINT) - 1);
		(void) fprintf (out_fp, "\nReturn Value %d:\t`%s'.", i + 1, buf);
	}

	(void) fprintf (out_fp, "\n");

	return (IMS_OK);
}

/******************************************************************************
**
** resultsStatus ()
**
** Prints a single line that:
** 1. shows the number of rows returned if this is a query.
** 2. shows the Sybase procedure return code if a status is returned.
** 3. shows both 1 and 2.
**
******************************************************************************/

static int resultsStatus (
	IMS_QI_DESC_OBJ *qDesc,
	FILE *out_fp)
{
	/*
	** If there is a return status from a Sybase procedure, check the
	** count. If there were no rows returned, we get back a value of -1.
	** 
	** If the count is not -1, print both the count and the status.
	** Otherwise just print the count.
	*/
	if ((IMS_HASRETSTAT (qDesc)) == IMS_TRUE)
	{
		if ((IMS_AFFECTED (qDesc)) != -1)
		{
			(void) fprintf (out_fp,
				"\n(%ld row(s) affected, return status = %ld)\n\n",
				IMS_AFFECTED (qDesc), IMS_PROCRETURN (qDesc));
		}
		else
		{
			(void) fprintf (out_fp,
				"\n(return status = %ld)\n\n",
				IMS_PROCRETURN (qDesc));
		}
	}
	/*
	** If there is not return status, see if the count is something
	** other than -1. If it is, return the count.
	*/
	else if ((IMS_AFFECTED (qDesc)) != -1)
	{
		(void) fprintf (out_fp,
			"\n(%ld row(s) affected)\n\n",
			IMS_AFFECTED (qDesc));
	}
	/*
	** If we failed to get anything at all, just print a couple of <LF>.
	*/
	else
	{
		(void) fprintf (out_fp, "\n\n");
	}

	return (IMS_OK);
}
