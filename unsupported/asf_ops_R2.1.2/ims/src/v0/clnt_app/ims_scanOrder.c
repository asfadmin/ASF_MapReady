static char *sccs = "@(#)ims_scanOrder.c	1.3  08/11/97";
/******************************************************************************
**
** File:        ims_scanOrder.c
**
** Function:    This application allows the user to initiate a Scan Job
**              Order without performing all of the usual checks based
**              on scanning criteria.
**
** Author:      Sean Hardman
**
** Date:        5/7/97
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <unistd.h>
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

#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>
#include <IK_Syslog.h>
#include <ims_order.h>

/*
** Input argument structure specification.
*/
typedef struct inputArg
{
	char *username;
	char *password;
	char *accountId;
	char *platform;
	char *granuleName;
	int  odlFileFlag;
} INPUT_ARG;

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, INPUT_ARG *);
static int openConnection (IMS_MSG_STRUCT *, INPUT_ARG *);
static int getGranuleTable (IMS_MSG_STRUCT *, char *, char *);
static int getUserInfo (IMS_MSG_STRUCT *, INPUT_ARG *, char *, char *, char *);
static int getQuickLookFlag (IMS_MSG_STRUCT *, INPUT_ARG *, char *, char *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *accountId;
	char *platform;
	char *granuleName;
	char *odlFileFlag;
	char *commandFile;
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
	{"-A",           &commands.accountId},
	{"+accountId",   &commands.accountId},
	{"-L",           &commands.platform},
	{"+platform",    &commands.platform},
	{"-G",           &commands.granuleName},
	{"+granuleName", &commands.granuleName},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-f",           &commands.odlFileFlag},
	{"+odlFileFlag", &commands.odlFileFlag},
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
	{"accountId",   &commands.accountId},
	{"platform",    &commands.platform},
	{"granuleName", &commands.granuleName},
	{"odlFileFlag", &commands.odlFileFlag}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;
static char cmdBuf[1024];

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
	INPUT_ARG inputArg;
	AGGREGATE root;
	IMS_ODL_TREE *scan_request;
	IMS_ODL_TREE *user_info;
	IMS_ODL_TREE *scan_item;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char dataset[IMS_COL80_LEN+1];
	char granuleTable[IMS_COL30_LEN+1];
	char firstName[IMS_COL20_LEN+1];
	char lastName[IMS_COL20_LEN+1];
	char authenticator[IMS_COL20_LEN+1];
	char quickLookFlag[IMS_COL3_LEN+1];
	char fileName[IMS_PATH_LEN+1];
	int newOrderId;

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
			"Only %d out of the %d command line arguments were "
			"processed.", status, argc);
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
	** this file, then overlay all commands from the command-line,
	** except password, which will be gone by this point.
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
		** Now, get command line args again to overlay file arguments.
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
	** Process the information from the command-line and/or file.
	*/
	if ((status = getArgInput (msgDesc, &inputArg)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Open a database server connection.
	*/
	if ((status = openConnection (msgDesc, &inputArg)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred opening a database server connection.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Get the RAW SIGNAL SEGMENT granule table name.
	*/
	(void) strcpy (dataset, inputArg.platform);
	(void) strcat (dataset, " RAW SIGNAL SEGMENT");

	if ((status = getGranuleTable (msgDesc, dataset,
		granuleTable)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not obtain granule table name for dataset '%s'.",
			dataset);
		goto ERROR;
	}

	/*
	** Query for authorization information.
	*/
	if ((status = getUserInfo (msgDesc, &inputArg, firstName, lastName,
		authenticator)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred obtaining the user information.");
		goto ERROR;
	}

	/*
	** Query for quick look information.
	*/
	if ((status = getQuickLookFlag (msgDesc, &inputArg, granuleTable,
		quickLookFlag)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred obtaining the quick look information.");
		goto ERROR;
	}

	/*
	** Build the SCAN_REQUEST object.
	*/
	(void) ims_addODLObject(msgDesc, NULL, &scan_request, 
		"SCAN_REQUEST", FALSE, IMS_OBJECT);

	/*
	** Build the USER_IMFO object.
	*/
	(void) ims_addODLObject(msgDesc, scan_request,  &user_info,
		"USER_INFO", TRUE, IMS_OBJECT);

	(void) ims_addODLKeyword(msgDesc, user_info, "FIRST_NAME",
		TV_STRING, firstName);

	(void) ims_addODLKeyword(msgDesc, user_info, "LAST_NAME",
		TV_STRING, lastName);

	(void) ims_addODLKeyword(msgDesc, user_info, "AUTHENTICATOR",
		TV_STRING, authenticator);

	(void) ims_addODLKeyword(msgDesc, user_info, "BILLING_ID",
		TV_STRING, inputArg.accountId);

	/*
	** Build the SCAN_ITEM object.
	*/
	(void) ims_addODLObject(msgDesc, scan_request, &scan_item, 
		"SCAN_ITEM", TRUE, IMS_OBJECT);

	(void) ims_addODLKeyword(msgDesc, scan_item, "PLATFORM",
		TV_STRING, inputArg.platform);

	(void) ims_addODLKeyword(msgDesc, scan_item, "DATASET",
		TV_STRING, dataset);
		
	(void) ims_addODLKeyword(msgDesc, scan_item, "FILENAME",
		TV_STRING, inputArg.granuleName);

	(void) ims_addODLKeyword(msgDesc, scan_item, "QUICK_LOOK",
		TV_STRING, quickLookFlag);

	/*
	** If the flag is set, generate an ODL file, otherwise
	** place an order.
	*/
	if (inputArg.odlFileFlag == IMS_TRUE)
	{
		(void) sprintf (fileName, "%s_%d.odl", programName, (int) getpid());

		if ((status = ims_buildPMF (msgDesc, scan_request,
			fileName, NULL)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not write out the ODL file '%s'.", fileName);
			goto ERROR;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Successfully generated ODL file '%s'.", fileName);
	}
	else
	{
		if ((status = ims_buildAggregate (msgDesc, scan_request,
			&root)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not build the aggregate structure.");
			goto ERROR;
		}

		if ((status = ims_order (msgDesc, root, &newOrderId)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not submit the order.");
			goto ERROR;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Successfully submitted order '%d'.", newOrderId);
	}

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	(void) ims_qiFreeDesc (qDesc);
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
	(void) fprintf (stderr, "\n\nTermination of %s due to signal: "
		"%s (%d)\n\n", programName, ims_sigMsg (sig), sig);

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
	IMS_MSG_STRUCT *msgDesc,
	INPUT_ARG *inputArg)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char prompt[20];

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		inputArg->username = commands.username;
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

		inputArg->username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputArg->username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		inputArg->password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		inputArg->password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputArg->password, inputBuffer);
	}

	/* accountId */
	if (commands.accountId != (char *) NULL)
	{
		inputArg->accountId = commands.accountId;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Account ID: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		inputArg->accountId = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputArg->accountId, inputBuffer);
	}

	/* platform */
	if (commands.platform != (char *) NULL)
	{
		inputArg->platform = commands.platform;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Platform: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		inputArg->platform = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputArg->platform, inputBuffer);
	}

	/* granuleName */
	if (commands.granuleName != (char *) NULL)
	{
		inputArg->granuleName = commands.granuleName;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Granule Name: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		inputArg->granuleName = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (inputArg->granuleName, inputBuffer);
	}

	/* odlFileFlag */
	if (commands.odlFileFlag != (char *) NULL)
	{
		inputArg->odlFileFlag = IMS_TRUE;
	}
	else
	{
		inputArg->odlFileFlag = IMS_FALSE;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** openConnection ()
**
******************************************************************************/

static int openConnection (
	IMS_MSG_STRUCT *msgDesc,
	INPUT_ARG *inputArg)
{
	int status;

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
	IMS_SETUSER (qDesc, inputArg->username);

	IMS_SETPSWD (qDesc, inputArg->password);

	IMS_SETPROG (qDesc, programName);

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
** getGranuleTable ()
**
******************************************************************************/

static int getGranuleTable (
	IMS_MSG_STRUCT *msgDesc,
	char *dataset,
	char *granuleTable)
{
	int status;

	/*
	** Set up the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select granules_table "
		"from dataset_policy p, dataset_relation r "
		"where dataset = '%s' "
		"and r.dataset_idx = p.dataset_idx",
		dataset);

	/*
	** Process the result rows for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	/*
	** Check to see if a row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain information for dataset '%s'.",
			dataset);
		return (IMS_ERROR);
	}

	/*
	** Check to see if more than one row was returned.
	*/
	if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row returned for dataset '%s'.",
			dataset);
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* granules_table */
	(void) memcpy (granuleTable, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	granuleTable[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (granuleTable);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getUserInfo ()
**
******************************************************************************/

static int getUserInfo (
	IMS_MSG_STRUCT *msgDesc,
	INPUT_ARG *inputArg,
	char *firstName,
	char *lastName,
	char *authenticator)
{
	int status;

	/*
	** Set up the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select first_name, last_name, auth_key "
		"from user_profile "
		"where user_id = '%s'",
		inputArg->username);

	/*
	** Process the result rows for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	/*
	** Check to see if a row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain information for user '%s'.",
			inputArg->username);
		return (IMS_ERROR);
	}

	/*
	** Check to see if more than one row was returned.
	*/
	if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row returned for user '%s'.",
			inputArg->username);
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* first_name */
	(void) memcpy (firstName, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	firstName[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (firstName);

	/* last_name */
	(void) memcpy (lastName, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	lastName[IMS_VALUELENGTH (qDesc, 1)] = '\0';
	(void) ims_truncStr (lastName);

	/* auth_key */
	(void) memcpy (authenticator, IMS_VALUE (qDesc, 2),
		IMS_VALUELENGTH (qDesc, 2));
	authenticator[IMS_VALUELENGTH (qDesc, 2)] = '\0';
	(void) ims_truncStr (authenticator);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getQuickLookFlag ()
**
******************************************************************************/

static int getQuickLookFlag (
	IMS_MSG_STRUCT *msgDesc,
	INPUT_ARG *inputArg,
	char *granuleTable,
	char *quickLookFlag)
{
	int status;

	/*
	** Default the quick look flag value to N.
	*/
	(void) strcpy (quickLookFlag, "N");

	/*
	** Set up the command buffer with the SQL statement for
	** searching for an existing downlink.
	*/
	(void) sprintf (cmdBuf,
		"select QUICKLOOK_FLAG "
		"from %s g, datatake_entry d "
		"where name = '%s' and "
		"((g.PLATFORM = d.PLATFORM "
		"and g.SENSOR = d.SENSOR "
		"and g.REVOLUTION = d.REVOLUTION "
		"and g.SEQUENCE = d.SEQUENCE) or "
		"(g.PLATFORM = d.DT_PLATFORM "
		"and g.SENSOR = d.DT_SENSOR "
		"and g.REVOLUTION = d.DT_REVOLUTION "
		"and g.SEQUENCE = d.DT_SEQUENCE))",
		granuleTable, inputArg->granuleName);

	/*
	** Process the result rows for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/* QUICKLOOK_FLAG */

		/*
		** If one of the rows returned has a value of YES than
		** set the flag to Y.
		*/
		if (strncmp (IMS_VALUE (qDesc, 0), "Y", 1) == 0)
		{
			(void) strcpy (quickLookFlag, "Y");
		}
	}

	/*
	** Check to see if a row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain information for granule '%s'.",
			inputArg->granuleName);
		return (IMS_ERROR);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	return (IMS_OK);
}

