static char *sccs = "@(#)ims_msgEx.c	5.2  11/08/96";
/******************************************************************************
**
** File:        ims_msgEx.c
**
** Function:    Test the capabilities of the IMS Message Facility.
**
** Author:      J. Rector
**
** Date:        2/20/89
**
** Modified:    1/27/95 - S. Hardman
**              Converted from the MDMS message facility to the IMS
**              message facility.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <syslog.h>

#include <ims_query.h>

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (void)
{
	IMS_MSG_STRUCT *msgDesc;
	IMS_MSG_QUEUE *msgQueue;
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char fileName[IMS_PATH_LEN];
	char buf[512];

	/* Database access information variables. */
	static char userName[IMS_NAME_LEN + 1];
	static char password[IMS_NAME_LEN + 1];
	static char server[IMS_NAME_LEN + 1];
	static char dbName[IMS_NAME_LEN + 1];

	char *progName = "ims_msgEx";
	char *selectCommand = "select * from junk";

	/*
	** Initialize the IMS Message Facility which will allow messages to
	** be retrieved from the message queue.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (-1);
	}

	/* Initialize the message facility options. */
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, progName);
	(void) ims_msgBanner (msgDesc, progName, IMS_MSG_ALLBANNER);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/* Obtain database access information. */
	(void) fprintf (stdout,
		"\nObtain login information for Sybase error handling test.\n\n");
	(void) fprintf (stdout, "Enter db user name  : ");
	(void) gets (userName);
	(void) fprintf (stdout, "Enter password      : ");
	(void) gets (password);
	(void) fprintf (stdout, "Enter server name   : ");
	(void) gets (server);
	(void) fprintf (stdout, "Enter database name : ");
	(void) gets (dbName);

	/*
	** Messages can be sent to the syslog facility by doing the following.
	*/
	if ((status = ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5))
		< IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not turn on syslog capability.");
		exit (-1);
	}

	(void) ims_msgSetSyslogSeverity (msgDesc, IMS_ERROR);

	(void) ims_msg (msgDesc, IMS_ERROR, "Test the syslog facility.");

	(void) ims_msg (msgDesc, IMS_INFO,
		"This message should not show up in the syslog.");

	(void) ims_msgCloseSyslog (msgDesc);

	/*
	** Message can be sent to a log file using the following technique.
	*/
	(void) sprintf (fileName, "%s.%d.log", progName, (int) getpid());

	if ((status = ims_msgLogFileName (msgDesc, fileName)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not turn on log file capability.");
		exit (-1);
	}

	/*
	** Put messages into the queue to be extracted at the
	** end of the program. In this example we will also display
	** the messages to the screen as well as write them to the
	** log file now.
	*/
	(void) ims_msgQueueFlag (msgDesc, IMS_ON);

	(void) ims_msg (msgDesc, IMS_INFO, "First queued message.");
	(void) ims_msg (msgDesc, IMS_INFO, "Second queued message.");
	(void) ims_msg (msgDesc, IMS_INFO, "Third queued message.");

	(void) ims_msgQueueFlag (msgDesc, IMS_OFF);

	/*
	** Display messages with different banner configurations.
	*/
	(void) ims_msg (msgDesc, IMS_INFO, "Full Banner.");

	(void) ims_msgTimeFlag (msgDesc, IMS_OFF);
	(void) ims_msg (msgDesc, IMS_INFO, "Banner without Time and Date.");

	(void) ims_msgBanner (msgDesc, " ", IMS_MSG_ALLBANNER);
	(void) ims_msg (msgDesc, IMS_INFO,
		"Banner without Time, Date and Banner string.");

	(void) ims_msgSeverityFlag (msgDesc, IMS_OFF);
	(void) ims_msg (msgDesc, IMS_INFO,
		"Banner without Time, Date, Banner string and Severity.");

	(void) ims_msgBanner (msgDesc, " ", IMS_MSG_NOBANNER);
	(void) ims_msg (msgDesc, IMS_INFO, "No Banner.");
	
	/* Reset banner options. */
	(void) ims_msgBanner (msgDesc, progName, IMS_MSG_ALLBANNER);
	(void) ims_msgTimeFlag (msgDesc, IMS_ON);
	(void) ims_msgSeverityFlag (msgDesc, IMS_ON);

	/*
	** The message facility always reports a message severity when issuing a
	** message.  The following function calls check issue message using each
	** of the severity levels.  The severity levels are defined in ims_msg.h.
	*/
	(void) ims_msg (msgDesc, IMS_OK, "Severity level check (IMS_OK).");
	(void) ims_msg (msgDesc, IMS_INFO, "Severity level check (IMS_INFO).");
	(void) ims_msg (msgDesc, IMS_WARNING,
		"Severity level check (IMS_WARNING).");
	(void) ims_msg (msgDesc, IMS_ERROR, "Severity level check (IMS_ERROR).");
	(void) ims_msg (msgDesc, IMS_FATAL, "Severity level check (IMS_FATAL).");

	/*
	** Each time we send a message, the new message severity is saved.
	** We can recall it or test it, using the routine ims_msgGetSeverity().
	** We can also set the message severity level with the function
	** ims_msgSeverity().
	**
	** The current message severity is IMS_FATAL, set by the last ims_msg()
	** call above.  Check the value of the message severity. Then set it to
	** a value of IMS_OK and check it again.
	*/
	if (ims_msgGetSeverity (msgDesc) != IMS_FATAL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Incorrect severity level detected.");
	}

	(void) ims_msg (msgDesc, ims_msgGetSeverity (msgDesc), 
			"Reporting current severity level (IMS_FATAL).");

	(void) ims_msgSeverity (msgDesc, IMS_OK);

	if (ims_msgGetSeverity (msgDesc) != IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Incorrect severity level detected.");
	}

	(void) ims_msg (msgDesc, ims_msgGetSeverity (msgDesc), 
			"Reporting current severity level (IMS_OK).");

	/*
	** A message from a program has two parts, the severity level and the
	** message text. The message text is composed of a format and the
	** corresponding values.
	**
	** This relationship between the format and values is identical to that
	** of the fprintf function. 
	**
	** Note: At this time the message log is turned on.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"Log file %s is open.", fileName);

	(void) ims_msg (msgDesc, IMS_INFO,
		"Log file %s is open for program %s.", fileName, progName);

	(void) ims_msg (msgDesc, IMS_INFO, 
		"Current number of errors reported: %d.", 0);

	(void) ims_msg (msgDesc, IMS_INFO, 
		"Log file %s is open and the current number of errors is %d.",
		fileName, 0);

	/*
	** Supress sending messages to the terminal.
	*/
	(void) ims_msgStderrFlag (msgDesc, IMS_OFF);

	(void) ims_msg (msgDesc, IMS_INFO, 
		"This message should not appear on the terminal.");

	(void) ims_msgStderrFlag (msgDesc, IMS_ON);

	/*
	** If you would like to display system messages, you can place
	** a call to strerror() in your format string.
	**
	** We shall try to read from a stream for which there is not a valid
	** file descriptor.
	*/
	if (read (-1, buf, 512) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Read error. %s.", strerror (errno));
	}

	/*
	** Now we will use the IMS Query Interface to perform a database
	** server login and submit an invalid query to test the Sybase
	** error and message handlers.
	*/

	(void) ims_msg (msgDesc, IMS_INFO,
		"Sybase error and message handling is turned on.");

	/* Allocate a query descriptor */
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		exit (-1);
	}

	/* Initialize the query descriptor for database server access. */
	IMS_SETPROG (qDesc, progName);
	IMS_SETUSER (qDesc, userName);
	IMS_SETPSWD (qDesc, password);
	IMS_SETSERVER (qDesc, server);
	IMS_SETDBNAME (qDesc, dbName);

	/* Login to the specified server. */
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Database server login failed.");
	}
	else
	{
		/*
		** Saves the message descriptor for use by
		** Sybase error and message handling.
		*/
		IMS_SET_USERDATA (qDesc);

		/* Set the command in the query descriptor. */
		IMS_SETCMD (qDesc, selectCommand);

		/* Perform the command and just check for status. */
		if ((status = ims_qiNextRow (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Select statement failed.");
		}
		else
		{
			(void) ims_qiCancel (qDesc);
		}

		/* Check the last Sybase error/message numbers and severity levels. */
		(void) ims_msg (msgDesc, IMS_INFO,
			"Sybase error number: %d", ims_msgGetSybErrNo (msgDesc));
		(void) ims_msg (msgDesc, IMS_INFO,
			"Sybase error severity: %d", ims_msgGetSybErrSeverity (msgDesc));
		(void) ims_msg (msgDesc, IMS_INFO,
			"Sybase message number: %d", ims_msgGetSybMsgNo (msgDesc));
		(void) ims_msg (msgDesc, IMS_INFO,
			"Sybase message severity: %d", ims_msgGetSybMsgSeverity (msgDesc));

		/* Turn off Sybase error and message handling. */
		(void) ims_msgSybErrHndlFlag (msgDesc, IMS_OFF);
		(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_OFF);

		(void) ims_msg (msgDesc, IMS_INFO,
			"Sybase error and message handling is turned off.");

		/* Reset the query descriptor. */
		(void) ims_qiResetDesc (qDesc);

		/* Set the command in the query descriptor. */
		IMS_SETCMD (qDesc, selectCommand);

		/* Perform the command and just check for status. */
		if ((status = ims_qiNextRow (qDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Select statement failed.");
		}
		else
		{
			(void) ims_qiCancel (qDesc);
		}
	}

	/* Turn on Sybase error and message handling. */
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/* Logoff from the database server and free the query descriptor. */
	(void) ims_qiFreeDesc (qDesc);

	/*
	** Extract the messages that we placed in the queue earlier.
	*/
	(void) ims_msgQueueFlag (msgDesc, IMS_ON);

	while ((msgQueue = ims_msgQueueExtract (msgDesc)) !=
		(IMS_MSG_QUEUE *) NULL)
	{
		(void) fprintf (stdout, "%s\n", msgQueue->msg);
		(void) ims_msgQueueFree (msgQueue);
	}

	(void) ims_msgQueueFlag (msgDesc, IMS_OFF);

	(void) ims_msg (msgDesc, IMS_OK, "Normal program termination.");

	/* Close the log file and free the message descriptor structure. */
	(void) ims_msgStructFree (msgDesc);

	exit (0);
}
