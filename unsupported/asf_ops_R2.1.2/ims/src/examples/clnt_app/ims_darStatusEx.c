static char *sccs = "@(#)ims_darStatusEx.c	5.1  03/17/96";
/* *************************************************************
**
** File:        ims_darStatusEx.c
**
** Function:    An example program for the ims_darStatus routine.
**
** Author:      Alin Tilden
**
** Date:        12/15/95
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
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *, IMS_CMN_QUERY *);
static int checkQueryStatus (IMS_MSG_STRUCT *, IMS_CMN_QUERY *, int);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *orderId;
	char *itemId;
	char *status;
    char *plannerComment;
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
	{"-U",              &commands.username},
	{"+username",       &commands.username},
	{"-P",              &commands.password},
	{"+password",       &commands.password},
	{"-O",              &commands.orderId},
	{"+orderId",        &commands.orderId},
	{"-I",              &commands.itemId},
	{"+itemId",         &commands.itemId},
	{"-S",              &commands.status},
	{"+status",         &commands.status},
    {"-T",              &commands.plannerComment},
    {"+plannerComment", &commands.plannerComment},
	{"-C",              &commands.commandFile},
	{"+commandFile",    &commands.commandFile},
	{"-X",              &commands.server},
	{"+server",         &commands.server},
	{"-Y",              &commands.database},
	{"+database",       &commands.database},
	{"-h",              &commands.help},
	{"+help",           &commands.help},
	{"-r",              &commands.release},
	{"+release",        &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",        &commands.username},
	{"password",        &commands.password},
	{"orderId",         &commands.orderId},
	{"itemId",          &commands.itemId},
	{"status",          &commands.status},
	{"plannerComment",  &commands.plannerComment},
	{"server",          &commands.server},
	{"database",        &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *programName;
static char *orderId;
static char *itemId;
static char *new_status;
static char *plannerComment;

/* *************************************************************
**
** main ()
**
** This is the driver for the ims_darStatus routine.
**
**************************************************************** */

void main (
	int argc,
	char *argv[])
{
    int             status;
    char            hostName[IMS_HOST_LEN+1];
    struct utsname  uname_info;          /* Structure for uname() */
    char            banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    IMS_MSG_STRUCT *msgDesc;
    IMS_CMN_QUERY  *query;
    IMS_DAR_STATUS *darStatus;


    /*
    ** Initialize variables.
    */

    commands.username = NULL;
    commands.password = NULL;
    commands.orderId = NULL;
    commands.itemId = NULL;
    commands.status = NULL;
    commands.plannerComment = NULL;
    commands.commandFile = NULL;
    commands.server = NULL;
    commands.database = NULL;
    commands.help = NULL;
    commands.release = NULL;

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
    (void) ims_msgStderrFlag (msgDesc, IMS_ON);
    (void) ims_msgQueueFlag (msgDesc, IMS_ON);
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
    ** Allocate space for the IMS_CMN_QUERY structure.
    */
    if ((query = (IMS_CMN_QUERY *) malloc (
        (size_t) sizeof (IMS_CMN_QUERY))) ==
        (IMS_CMN_QUERY *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate memory for the IMS_CMN_QUERY structure.");
        goto ERROR;
    }
 
    /*
    ** Initialize common query structure members.
    */
    query->qDesc = NULL;
    query->username[0] = '\0';
    query->password[0] = '\0';
    query->server[0] = '\0';
    query->database[0] = '\0';
    query->qDesc = NULL;
    query->retStatus = IMS_OK;
    query->retPtr = NULL;
    (void) strcpy (query->program, programName);
    query->msgDesc = msgDesc;
 

    /*
    ** Process the information from command-line and/or command-file.
    */
    if ((status = getArgInput (msgDesc, query)) < IMS_OK)
    {
        goto ERROR;
    }

    /*
    ** Open the database server connection.
    */
    if ((status = ims_openQueryConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not open a database server connection.");
        goto ERROR;
    }
 
    /*
    ** Allocate memory for the darStatus structure
    */
    if ((darStatus = (IMS_DAR_STATUS *) malloc (sizeof (IMS_DAR_STATUS)))
	== (IMS_DAR_STATUS *) NULL)
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate memory for the IMS_DAR_STATUS structure.");
	goto ERROR;
    }

    /*
    ** Initialize darStatus structure members.
    */
    darStatus->order_id = (int) atoi (orderId);
    darStatus->item_id = (int) atoi (itemId);
    strcpy (darStatus->status, new_status);
    strcpy (darStatus->planner_comment, plannerComment);

    /*
    ** Report dar status.
    */
    if ((status = ims_darStatus (query, darStatus)) < IMS_OK)
    {
        (void) checkQueryStatus (msgDesc, query, status);
        goto ERROR;
    }

    (void) ims_msg (msgDesc, IMS_INFO,
        "The ims_darStatus was successful.");

    /*
    ** Close the database server connection.
    */
    if ((status = ims_closeQueryConnection (query)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not close database server connection.");
        goto ERROR;
    }

    /*
    ** Shutdown the message facility.
    */
    (void) ims_msgStructFree (msgDesc);

    exit (0);

ERROR:
    if (query != (IMS_CMN_QUERY *) NULL)
    {
        if (query->qDesc != (IMS_QI_DESC_OBJ *) NULL)
        {
            (void) ims_closeQueryConnection (query);
        }
        free (query);
    }

    if (darStatus != (IMS_DAR_STATUS *) NULL)
    {
	free (darStatus);
    }

    (void) ims_msg (msgDesc, status, "Could not update dar status.");
 
    (void) ims_msgStructFree (msgDesc);

    exit (1);

}   /*  main   */

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
	IMS_CMN_QUERY *query)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	long i;
        int read_success;
        int invalid;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		(void) strcpy (query->username, commands.username);
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

		(void) strcpy (query->username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		(void) strcpy (query->password, commands.password);
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (query->password, inputBuffer);
	}

	/* order id */
	if (commands.orderId != (char *) NULL)
	{
	    orderId = commands.orderId;
	}
	else
	{
	    read_success = IMS_FALSE;
	    while (read_success != IMS_TRUE)
	    {
		invalid = IMS_FALSE;
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Order ID: ") == (char *) NULL)
		{
		    (void) ims_msg (msgDesc, IMS_FATAL,
			"Error detected while reading input string.");
		    return (IMS_FATAL);
		}

		for (i=0; inputBuffer[i]; i++)
		{
		    if ((i == 0) && ((inputBuffer[i] == '-') ||
				     (inputBuffer[i] == '+')))
		    {
			continue;
		    }
		    
		    if (!isdigit (inputBuffer[i]))
		    {
			(void) ims_msg( msgDesc, IMS_INFO,
			    "Expecting numerical input. Enter Order ID again.");
			invalid = IMS_TRUE;
			break;
		    }
		}
		if (invalid != IMS_TRUE)
		    read_success = IMS_TRUE;
	    }

	    orderId = malloc (strlen (inputBuffer) + 1);
	    (void) strcpy (orderId, inputBuffer);
	}

	/* item id */
	if (commands.itemId != (char *) NULL)
	{
		itemId = commands.itemId;
	}
	else
	{
	    read_success = IMS_FALSE;
	    while (read_success != IMS_TRUE)
	    {
		invalid = IMS_FALSE;
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Item ID: ") == (char *) NULL)
		{
		    (void) ims_msg (msgDesc, IMS_FATAL,
			"Error detected while reading input string.");
		    return (IMS_FATAL);
		}

		for (i=0; inputBuffer[i]; i++)
		{
		    if ((i == 0) && ((inputBuffer[i] == '-') ||
				     (inputBuffer[i] == '+')))
		    {
			continue;
		    }
		    
		    if (!isdigit (inputBuffer[i]))
		    {
			(void) ims_msg( msgDesc, IMS_INFO,
			    "Expecting numerical input. Enter Item ID again.");
			invalid = IMS_TRUE;
			break;
		    }
		}
		if (invalid != IMS_TRUE)
		    read_success = IMS_TRUE;
	    }

	    itemId = malloc (strlen (inputBuffer) + 1);
	    (void) strcpy (itemId, inputBuffer);

	}

	/* status */
	if (commands.status != (char *) NULL)
	{
		new_status = commands.status;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Dar Status: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		new_status = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (new_status, inputBuffer);
	}

	/* plannerComment */
	if (commands.plannerComment != (char *) NULL)
	{
		plannerComment = commands.plannerComment;
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Planner Comment: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		plannerComment = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (plannerComment, inputBuffer);
		
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		(void) strcpy (query->server, commands.server);
	}

	/* database */
	if (commands.database != (char *) NULL)
	{
		(void) strcpy (query->database, commands.database);
	}

	return (IMS_OK);
}   /*  getArgInput */


/******************************************************************************
**
** checkQueryStatus ()
**
******************************************************************************/

static int checkQueryStatus (
	IMS_MSG_STRUCT *msgDesc,
	IMS_CMN_QUERY *query,
	int status)
{
	switch (query->retStatus)
	{
		case IMS_OK:
			(void) ims_msg (msgDesc, status,
				"Error is not query related.");
			break;

		case IMS_NOCONNECT:
			(void) ims_msg (msgDesc, status,
				"Error due to connection failure.");
			break;

		case IMS_DEADLOCK:
			(void) ims_msg (msgDesc, status,
				"Error due to query deadlock.");
			break;

		case IMS_NOROWS:
			(void) ims_msg (msgDesc, status,
				"Error due to no rows returned.");
			break;

		case IMS_QUERYFAIL:
			(void) ims_msg (msgDesc, status,
				"Error due to query failure.");
			break;

		default:
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Unknown query return status.");
			return (IMS_ERROR);
	}

	return (IMS_OK);
}   /*  checkQueryStatus   */


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

