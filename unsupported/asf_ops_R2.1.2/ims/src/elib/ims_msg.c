static char *sccs = "@(#)ims_msg.c	5.4  11/14/96";
/******************************************************************************
**
** File:        ims_msg.c
**
** Function:    Message facility.
**
** Author:      H. Sayah
**
** Date:        1/13/92
**
** Modified:    1/28/95 - S. Hardman - R1B
**              Cleaned up the file a little and substituted IMS_ON for
**              IMS_TRUE and IMS_OFF for IMS_FALSE for consistency.
**
**              3/8/95 - S. Hardman - R1B
**              Added the syslog capability which includes three new
**              functions: ims_msgOpenSyslog(), ims_msgCloseSyslog(),
**              ims_msgSetSyslogSeverity() and ims_msgProgramName().
**
**              8/22/95 - S. Hardman - R1B
**              Added the ims_msgIKSyslog() function.
**
**              9/22/95 - S. Hardman - R1B'
**              Added the ims_msgStructInit() function.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <errno.h>
#include <syslog.h>
#include <time.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>

/*
** Local Functions.
*/
int ims_reportSybMessage (DBPROCESS *, DBINT, int, int, char *, char *,
	char *, int);
int ims_reportSybError (DBPROCESS *, int, int, int, char *, char *);
static int insertMsgQueue (IMS_MSG_STRUCT *, IMS_MSG_QUEUE *);

/*
** External Fucntions.
**
** Extracted from time.h to avoid REENTRANT definition hassles
** on various platforms.
*/
#ifndef __alpha
	extern char *ctime_r (const time_t *, char *, int);
#else
	extern int ctime_r (const time_t *, char *, int);
#endif /* !__alpha */

/*
** Global variable definitions.
*/
static IMS_MSG_STRUCT glbMsgStruct =
{
	IMS_OK,                 /* last severity level */
	"IMS",                  /* sub-system name */
	"General Msg Handler",  /* banner */
	IMS_MSG_ALLBANNER,      /* banner option */
	IMS_ON,                 /* stderr flag */
	IMS_OFF,                /* queue flag */
	IMS_ON,                 /* severity flag */
	IMS_OFF,                /* log file flag */
	(char *)NULL,           /* log file name */
	-1,                     /* log file desc */
	(FILE *)NULL,           /* log file pointer */
	0,                      /* log file desc is open */
	0,                      /* log file ponter is open */
	IMS_ON,                 /* time-stamp flag */
	IMS_OFF,                /* additional info flag */
	10,                     /* Sybase message supress level */
	0,                      /* last sybase error severity */
	0,                      /* last sybase error number */
	0,                      /* last sybase msg severity */
	0,                      /* last sybase msg number */
	IMS_ON,                 /* sybase error handler flag */
	IMS_ON,                 /* syabse msg handler flag */
	0,                      /* sig event counter */
	IMS_OFF,                /* syslog flag */
	IMS_INFO,               /* severity level written to syslog */
	"Unknown",              /* program name */
	(IMS_MSG_QUEUE *)NULL,  /* first queued msgs */
	(IMS_MSG_QUEUE *)NULL   /* last queued msg */
};

/******************************************************************************
**
** ims_msgStructAlloc ()
**
** Allocate and initialize the IMS_MSG_STRUCT structure.
**
******************************************************************************/

IMS_MSG_STRUCT *ims_msgStructAlloc (void)
{
	IMS_MSG_STRUCT *msgStruct;

	/*
	** Allocate the structure.
	**
	** lint: pointer cast may result in improper alignment
	** No problem, malloc() aligns on worst case boundary.
	*/
	if ((msgStruct = (IMS_MSG_STRUCT *) 
		malloc ((unsigned) sizeof (IMS_MSG_STRUCT)))
		== (IMS_MSG_STRUCT *) NULL)
	{
		return (msgStruct);
	}

	/*
	** Structure is allocated. Initialize the members.
	*/
	msgStruct->lastSeverity = IMS_OK;
	msgStruct->bannerOption = IMS_MSG_ALLBANNER;
	msgStruct->banner[0] = '\0';
	(void) strcpy (msgStruct->subSystem, "IMS");
	msgStruct->stderrFlag = IMS_ON;
	msgStruct->queueFlag = IMS_OFF;
	msgStruct->logFileFlag = IMS_OFF;
	msgStruct->logFileName = (char *)NULL;
	msgStruct->logFileDesc = -1;
	msgStruct->logFilePtr = (FILE *)NULL;
	msgStruct->logFDOpen = IMS_OFF;
	msgStruct->logFPOpen = IMS_OFF;
	msgStruct->timeStampFlag = IMS_ON;
	msgStruct->addInfoFlag = IMS_OFF;
	msgStruct->severityFlag = IMS_ON;
	msgStruct->sybMsgLevel = -1;
	msgStruct->lastSybErrSeverity = 0;
	msgStruct->lastSybErrNo = 0;
	msgStruct->lastSybMsgSeverity = 0;
	msgStruct->lastSybMsgNo = 0;
	msgStruct->sybErrHndlFlag = IMS_OFF;
	msgStruct->sybMsgHndlFlag = IMS_OFF;
	msgStruct->sigEventCounter = 0;
	msgStruct->syslogFlag = IMS_OFF;
	msgStruct->syslogSeverity = IMS_INFO;
	(void) strcpy (msgStruct->programName, "Unknown");
	msgStruct->first = (IMS_MSG_QUEUE *) NULL;
	msgStruct->last = (IMS_MSG_QUEUE *) NULL;

	return (msgStruct);
}

/******************************************************************************
**
** ims_msgStructInit ()
**
** Initialize a previously allocated IMS_MSG_STRUCT structure.
**
******************************************************************************/

int ims_msgStructInit (
	IMS_MSG_STRUCT *msgStruct)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	/*
	** Initialize the members to their default values. 
	*/
	msgStruct->lastSeverity = IMS_OK;
	msgStruct->bannerOption = IMS_MSG_ALLBANNER;
	msgStruct->banner[0] = '\0';
	(void) strcpy (msgStruct->subSystem, "IMS");
	msgStruct->stderrFlag = IMS_ON;
	msgStruct->queueFlag = IMS_OFF;
	msgStruct->logFileFlag = IMS_OFF;
	msgStruct->logFileName = (char *)NULL;
	msgStruct->logFileDesc = -1;
	msgStruct->logFilePtr = (FILE *)NULL;
	msgStruct->logFDOpen = IMS_OFF;
	msgStruct->logFPOpen = IMS_OFF;
	msgStruct->timeStampFlag = IMS_ON;
	msgStruct->addInfoFlag = IMS_OFF;
	msgStruct->severityFlag = IMS_ON;
	msgStruct->sybMsgLevel = -1;
	msgStruct->lastSybErrSeverity = 0;
	msgStruct->lastSybErrNo = 0;
	msgStruct->lastSybMsgSeverity = 0;
	msgStruct->lastSybMsgNo = 0;
	msgStruct->sybErrHndlFlag = IMS_OFF;
	msgStruct->sybMsgHndlFlag = IMS_OFF;
	msgStruct->sigEventCounter = 0;
	msgStruct->syslogFlag = IMS_OFF;
	msgStruct->syslogSeverity = IMS_INFO;
	(void) strcpy (msgStruct->programName, "Unknown");
	msgStruct->first = (IMS_MSG_QUEUE *) NULL;
	msgStruct->last = (IMS_MSG_QUEUE *) NULL;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgStructFree ()
**
** Free the allocated IMS_MSG_STRUCT structure and
** all associated queued messages.
**
******************************************************************************/

int ims_msgStructFree (
	IMS_MSG_STRUCT *msgStruct)
{
	IMS_MSG_QUEUE *currMsg, *nextMsg;

	if (msgStruct != (IMS_MSG_STRUCT *) NULL)
	{
		currMsg = msgStruct->first;
		while (currMsg != (IMS_MSG_QUEUE *) NULL)
		{
			nextMsg = currMsg->next;
			(void) ims_msgQueueFree (currMsg);
			currMsg = nextMsg;
		}

		/*
		** Close the logFile if open.
		*/
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFDOpen == IMS_ON)
			{
				(void) close (msgStruct->logFileDesc);
			}
			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
			}
		}

		/*
		** Close the syslog if open.
		*/
		if (msgStruct->syslogFlag == IMS_ON)
		{
			closelog ();
		}

		(void) free (msgStruct);
		msgStruct = (IMS_MSG_STRUCT *) NULL;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgQueueFree ()
**
** Free the message queue structure and associated allocated spaces.
**
******************************************************************************/

int ims_msgQueueFree (
	IMS_MSG_QUEUE *msgQueue)
{
	if (msgQueue != (IMS_MSG_QUEUE *) NULL)
	{
		if (msgQueue->msg != (char *) NULL)  
		{
			(void) free (msgQueue->msg);
		}

		(void) free (msgQueue);
		msgQueue = (IMS_MSG_QUEUE *) NULL;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgFree ()
**
** Free all associated queued messages.
**
******************************************************************************/

int ims_msgFree (
	IMS_MSG_STRUCT *msgStruct)
{
	IMS_MSG_QUEUE *currMsg, *nextMsg;

	if (msgStruct != (IMS_MSG_STRUCT *) NULL)
	{
		currMsg = msgStruct->first;
		while (currMsg != (IMS_MSG_QUEUE *) NULL)
		{
			nextMsg = currMsg->next;
			(void) ims_msgQueueFree (currMsg);
			currMsg = nextMsg;
		}

		/*
		** Close the logFile if open.
		*/
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFDOpen == IMS_ON)
			{
				(void) close (msgStruct->logFileDesc);
			}
			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
			}
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msg ()
**
** Generate and direct the message into the selected IO streams
** and/or the message queue.
**
******************************************************************************/

int ims_msg (
	IMS_MSG_STRUCT *msgStruct,
	int severity,
	char *fmt,
	...)
{
	va_list args;
	int msgLen, msgSize, rawMsgLen, tempLen;
	char timeBuf[IMS_MSG_TIME_LEN];
	char *timePtr, *bannerPtr, *tempPtr;
	char *emptyStr, *blankPtr;
	char *msgPtr, *completeMsg;
	char asciiSeverity[IMS_COL60_LEN+1];
	IMS_MSG_QUEUE *msgQueue;
	int seperator;
	FILE *fp;
	int syslogLevel;
	char syslogAsciiLevel[IMS_COL15_LEN];
	size_t syslogMessageLength;
	size_t syslogHeaderLength;
	time_t clock;

	/* Initialize */
	msgLen = rawMsgLen = 0;
	asciiSeverity[0] = '\0';
	emptyStr = "";
	blankPtr = emptyStr;
	seperator = IMS_OFF;

	/*
	** Continue if msgStruct is not null.
	*/
	if (msgStruct != (IMS_MSG_STRUCT *) NULL)
	{
		/* Record the new severity level */
		msgStruct->lastSeverity = severity;

		if (msgStruct->timeStampFlag == IMS_ON)
		{
			clock = time ((time_t *) 0);
			(void) ctime_r (&clock, timeBuf, IMS_MSG_TIME_LEN);
			timePtr = timeBuf;
			blankPtr = "  ";
			tempPtr = timePtr + (strlen(timePtr)-1);
			if (*tempPtr == '\n') *tempPtr = '\0';
			seperator = IMS_ON;
		}
		else
		{
			timePtr = emptyStr;
		}

		tempLen = strlen (timePtr) + strlen (blankPtr);
		msgLen += tempLen; 
		rawMsgLen += tempLen;

		if (msgStruct->severityFlag == IMS_ON)
		{
			seperator = IMS_ON;
			/* Convert integer severity to ascii severity */
			switch (severity)
			{
				case IMS_OK:
					(void) sprintf (asciiSeverity,
						"%%%s_OK%%  ", msgStruct->subSystem);
					break;

				case IMS_INFO:
					(void) sprintf (asciiSeverity,
						"%%%s_INFORMATION%%  ", msgStruct->subSystem);
					break;

				case IMS_WARNING:
					(void) sprintf (asciiSeverity,
						"%%%s_WARNING%%  ", msgStruct->subSystem);
					break;

				case IMS_ERROR:
					(void) sprintf (asciiSeverity,
						"%%%s_ERROR%%  ", msgStruct->subSystem);
					break;

				case IMS_FATAL:
					(void) sprintf (asciiSeverity,
						"%%%s_FATAL%%  ", msgStruct->subSystem);
					break;

				default:
					(void) sprintf (asciiSeverity,
						"%%%s_NOSEVERITY%%  ", msgStruct->subSystem);
			}
			tempLen = strlen (asciiSeverity);
			msgLen += tempLen; 
			rawMsgLen += tempLen;
		}

		/* Setup the banner. */
		switch (msgStruct->bannerOption)
		{
			case IMS_MSG_NOBANNER:
				bannerPtr = emptyStr;
				break;

			case IMS_MSG_ALLBANNER:
				seperator = IMS_ON;
				bannerPtr = msgStruct->banner;
				break;

			case IMS_MSG_ONEBANNER:
				if (msgStruct->first == (IMS_MSG_QUEUE *) NULL)
				{
					seperator = IMS_ON;
					bannerPtr = msgStruct->banner;
				}
				else
				{
					bannerPtr = emptyStr;
				}
				break;

			default:
				return (IMS_FATAL);
		}
		tempLen = strlen (bannerPtr); 
		msgLen += tempLen;
		rawMsgLen += tempLen;
		
		/*
		** Open file pointer to /dev/null to determine message length.
		*/
		if ((fp = fopen ("/dev/null","w")) == (FILE *) NULL)
		{
			if (msgStruct->stderrFlag == IMS_ON) 
			{
				(void) fputs ("ims_msg(): fopen(/dev/null) failed.", 
				stderr);
			}

			if (msgStruct->logFileFlag == IMS_ON) 
			{
				(void) fputs ("ims_msg(): fopen(/dev/null) failed.", 
				msgStruct->logFilePtr);
			}
			return (IMS_FATAL);
		}

		/*
		** Start variable argument processing.
		*/
		va_start (args, fmt);
		msgLen += vfprintf (fp, fmt, args) + 1;
		msgLen += 10; /* additional space for newline characters */
		(void) fclose (fp);

		msgSize = msgLen/32;
		msgSize = (msgSize + 1)*32;

		/* Allocate space for the message. */
		if ((completeMsg = 
			(char *) malloc ((unsigned) msgSize)) == (char *) NULL)
		{
			if (msgStruct->stderrFlag == IMS_ON) 
			{
				(void) fputs ("ims_msg(): Memory alloaction failed.", 
				stderr);
			}

			if (msgStruct->logFileFlag == IMS_ON) 
			{
				(void) fputs ("ims_msg(): Memory alloaction failed.", 
				msgStruct->logFilePtr);
			}

			/* End variable argument  processing. */
			va_end (args);
			return (IMS_FATAL);
		}

		/*
		** Build the complete message.
		*/
		if (seperator == IMS_ON)
		{
			msgPtr = completeMsg;
			(void) sprintf (msgPtr, "\n%s%s%s%s\n",
				asciiSeverity, bannerPtr, blankPtr, timePtr);
			msgPtr += strlen (msgPtr);
			(void) vsprintf (msgPtr, fmt, args);

			/* Added two newline characters. */
			rawMsgLen += 2;
		}
		else
		{
			msgPtr = completeMsg;
			(void) sprintf (msgPtr, "\n");
			msgPtr += strlen (msgPtr);
			(void) vsprintf (msgPtr, fmt, args);

			/* Added one newline character. */
			rawMsgLen += 1;
		}

		/* End variable argument processing. */
		va_end (args);

		/*
		** Send the message to stderr, if defined.
		*/
		if (msgStruct->stderrFlag == IMS_ON)
		{
			(void) fputs (completeMsg, stderr);
			(void) fputs ("\n", stderr);
			(void) fflush (stderr);
		}

		/*
		** Send the message to the log file, if defined.
		*/
		if (msgStruct->logFileFlag == IMS_ON)
		{
			(void) fputs (completeMsg, msgStruct->logFilePtr);
			(void) fputs ("\n", msgStruct->logFilePtr);
			(void) fflush (msgStruct->logFilePtr);
		}

		/*
		** Send the message to the syslog facility, if defined.
		*/
		if ((msgStruct->syslogFlag == IMS_ON) &&
			(msgStruct->syslogSeverity >= severity))
		{
			/* Convert our severity to a syslog level. */
			switch (severity)
			{
				case IMS_OK:
				case IMS_INFO:
					syslogLevel = LOG_INFO;
					(void) strcpy (syslogAsciiLevel, "INFORMATION");
					break;

				case IMS_WARNING:
					syslogLevel = LOG_WARNING;
					(void) strcpy (syslogAsciiLevel, "WARNING");
					break;

				case IMS_ERROR:
					syslogLevel = LOG_ERR;
					(void) strcpy (syslogAsciiLevel, "ERROR");
					break;

				case IMS_FATAL:
					syslogLevel = LOG_CRIT;
					(void) strcpy (syslogAsciiLevel, "CRITICAL");
					break;

				default:
					/*
					** All messages should have a severity but we still
					** want to send them off if for some reason they don't.
					*/
					syslogLevel = LOG_DEBUG;
					(void) strcpy (syslogAsciiLevel, "NO_SEVERITY");
			}

			/* Determine message and header lengths. */
			syslogMessageLength = strlen (completeMsg + rawMsgLen);
			syslogHeaderLength = (strlen (msgStruct->programName) +
				strlen (syslogAsciiLevel) + 5);

			/* Send the message to the syslog facility. */
			if (syslogMessageLength > (IMS_MAX_SYSLOG_MSG - syslogHeaderLength))
			{
				syslog (syslogLevel, "(%s) %s: Original message was not sent"
					" because it exceeded the maximum syslog message length.",
					msgStruct->programName, syslogAsciiLevel);
			}
			else
			{
				syslog (syslogLevel, "(%s) %s: %s", msgStruct->programName,
					syslogAsciiLevel, completeMsg + rawMsgLen);
			}
		}

		/*
		** Send the message to the message queue, if defined.
		*/
		if (msgStruct->queueFlag == IMS_ON)
		{
			/*
			** Allocate space for the message queue.
			**
			** lint: pointer cast may result in improper alignment
			** No problem, malloc() aligns on worst case boundary.
			*/
			if ((msgQueue = (IMS_MSG_QUEUE *) 
				malloc ((unsigned) sizeof (IMS_MSG_QUEUE))) 
				== (IMS_MSG_QUEUE *) NULL)
			{
				(void) free (completeMsg);
				if (msgStruct->stderrFlag == IMS_ON) 
				{
					(void) fputs ("ims_msg(): Memory allocation failed.", 
					stderr);
				}
				if (msgStruct->logFileFlag == IMS_ON) 
				{
					(void) fputs ("ims_msg(): Memory allocation failed.", 
					msgStruct->logFilePtr);
				}
				return (IMS_FATAL);
			}

			msgQueue->msg = completeMsg;
			msgQueue->rawMsg = completeMsg + rawMsgLen;
			msgQueue->msgLength = strlen (completeMsg);
			msgQueue->severity = severity;
			msgQueue->type = 0;
			msgQueue->number = 0;
			msgQueue->class = 0;
			msgQueue->state = 0;
			msgQueue->sigEvent = 0;
			msgQueue->next = (IMS_MSG_QUEUE *)NULL;

			/* Insert msgQueue in the message queue. */
			(void) insertMsgQueue (msgStruct, msgQueue); 
		}
		else
		{
			/*
			** If the queueFlag is set to IMS_OFF we can go ahead
			** and free the message buffer because we no longer
			** need it.
			*/
			(void) free (completeMsg);
		}

		return (IMS_OK);
	}

	return (IMS_ERROR);
}

/******************************************************************************
**
** ims_msgQueueExtract ()
**
** Extract the first queued message from the message queue.
**
******************************************************************************/

IMS_MSG_QUEUE *ims_msgQueueExtract (
	IMS_MSG_STRUCT *msgStruct)
{
	IMS_MSG_QUEUE *msgQueue, *msgPtr;

	msgQueue = (IMS_MSG_QUEUE *)NULL;

	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (msgQueue);
	}

	if (msgStruct->queueFlag == IMS_ON)
	{
		/* Extract the first queued message if any. */
		if (msgStruct->last != (IMS_MSG_QUEUE *)NULL)
		{
			msgQueue = msgStruct->last;
			if (msgStruct->first == msgStruct->last)
			{
				/* This is the last message in the queue. */
				msgStruct->last = msgStruct->first = (IMS_MSG_QUEUE *)NULL;
			}
			else
			{
				msgPtr = msgStruct->first;
				while (msgPtr->next->next != (IMS_MSG_QUEUE *) NULL)
				{
					msgPtr = msgPtr->next;
				}
				msgPtr->next = (IMS_MSG_QUEUE *) NULL;
				msgStruct->last = msgPtr;
			}
			msgQueue->next = (IMS_MSG_QUEUE *) NULL;

			if (msgQueue->sigEvent)
			{
				msgStruct->sigEventCounter -= 1;
			}
		}
	}

	return (msgQueue);
}

/******************************************************************************
**
** ims_msgStderrFlag ()
**
** Set the stderr flag in the message structure.
** When TRUE, messages are sent to the stderr stream.
**
******************************************************************************/

int ims_msgStderrFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON) 
	{ 
		msgStruct->stderrFlag = IMS_ON;
	}
	else
	{
		msgStruct->stderrFlag = IMS_OFF;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgQueueFlag ()
**
** Set the message queue flag in the messsage structure.
** When TRUE, messages are inserted in the queue.
**
******************************************************************************/

int ims_msgQueueFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON) 
	{ 
		msgStruct->queueFlag = IMS_ON;
	}
	else
	{
		msgStruct->queueFlag = IMS_OFF;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSeverityFlag ()
**
** Set the message severity flag in the message structure.
** When TRUE, severity is included in message header.
**
******************************************************************************/

int ims_msgSeverityFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON) 
	{ 
		msgStruct->severityFlag = IMS_ON;
	}
	else
	{
		msgStruct->severityFlag = IMS_OFF;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSeverity ()
**
** Set the message severity to the specified value.
**
******************************************************************************/

int ims_msgSeverity (
	IMS_MSG_STRUCT *msgStruct,
	int severity)
{
	msgStruct->lastSeverity = severity;

	return (msgStruct->lastSeverity);
}

/******************************************************************************
**
** ims_msgLogFileName ()
**
** Set log file name, log file descriptor, and log
** file pointer in the message structure and open the 
** file for write. Note, only the previously openned log 
** file by the message facility is closed.
** When set, messages are stored in the log file.
**
******************************************************************************/

int ims_msgLogFileName (
	IMS_MSG_STRUCT *msgStruct,
	char *fileName)
{
	int fd;
	FILE *fp;

	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if ((fileName != (char *) NULL) && ((int) strlen (fileName) > 0))
	{
		/* Open the file for write */
		if ((fd = open (fileName, O_CREAT|O_EXCL|O_WRONLY, 00644)) < 0)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not open log file '%s'. %s",
				fileName, strerror (errno));
			return (IMS_FATAL);
		}

		if ((fp = fdopen (fd, "w")) == (FILE *) NULL)
		{
			(void) close (fd);
			(void) ims_msg (msgStruct, IMS_FATAL, 
				"Could not open log file. %s",
				strerror (errno));
			return (IMS_FATAL);
		}
				
		/* Close the previously openned log file. */
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFDOpen == IMS_ON) 
			{
				(void) close (msgStruct->logFileDesc);
			}

			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
			}
		}

		/* Assign new log file */
		msgStruct->logFileDesc = fd;
		msgStruct->logFDOpen = IMS_ON;
		msgStruct->logFilePtr = fp;
		msgStruct->logFPOpen = IMS_ON;
		msgStruct->logFileFlag = IMS_ON;
		msgStruct->logFileName = fileName;
	}
	else
	{
		/* Close previously openned log file. */
		if (msgStruct->logFileFlag)
		{
			if (msgStruct->logFDOpen == IMS_ON)
			{
				(void) close (msgStruct->logFileDesc);
				msgStruct->logFDOpen = IMS_OFF;
				msgStruct->logFileDesc = -1;
			}

			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
				msgStruct->logFPOpen = IMS_OFF;
				msgStruct->logFilePtr = (FILE *)NULL;
			}
			msgStruct->logFileFlag = IMS_OFF;
			msgStruct->logFileName = (char *)NULL;
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgLogFileDesc ()
**
** Set log file descriptor and log file pointer in 
** the message structure. Log file must already be open
** for write. Note, only previously openned log file 
** by the message facility is closed.
** When set, messages are stored in the log file.
**
******************************************************************************/

int ims_msgLogFileDesc (
	IMS_MSG_STRUCT *msgStruct,
	int fd)
{
	FILE *fp;

	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (fd >= 0)
	{
		/* Make sure input fd is not the same as old fd. */
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFileDesc == fd) 
			{
				(void) ims_msg (msgStruct, IMS_ERROR, 
					"ims_msgLogFileDesc(): A log file is already open for this file desccriptor.");
				return (IMS_ERROR);
			}
		}

		/*
		** The file must already be open for write.
		** Assign fd to a file pointer.
		*/
		if ((fp = fdopen (fd, "w")) == (FILE *) NULL)
		{
			(void) ims_msg (msgStruct, IMS_FATAL, 
				"Could not associate a file pointer to the file descriptor. %s",
				strerror (errno));
			return (IMS_FATAL);
		}

		/* Close previously openned log file. */
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFDOpen == IMS_ON) 
			{
				(void) close (msgStruct->logFileDesc);
			}

			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
			}
		}

		/* Assign new log file */
		msgStruct->logFileDesc = fd;
		msgStruct->logFDOpen = IMS_OFF;
		msgStruct->logFilePtr = fp;
		msgStruct->logFPOpen = IMS_ON;
		msgStruct->logFileFlag = IMS_ON;
		msgStruct->logFileName = (char *)NULL;
	}
	else
	{
		/* Close previously openned log file. */
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFDOpen == IMS_ON) 
			{
				(void) close (msgStruct->logFileDesc);
				msgStruct->logFDOpen = IMS_OFF;
				msgStruct->logFileDesc = -1;
			}

			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
				msgStruct->logFPOpen = IMS_OFF;
				msgStruct->logFilePtr = (FILE *)NULL;
			}
			msgStruct->logFileFlag = IMS_OFF;
			msgStruct->logFileName = (char *)NULL;
		}
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgLogFilePtr ()
**
** Set log file pointer in the message structure.  File 
** must already be open for write. Note, only previously
** openned log file by the message facility is closed.
** When set, messages are stored in the log file.
**
******************************************************************************/

int ims_msgLogFilePtr (
	IMS_MSG_STRUCT *msgStruct,
	FILE *fp)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	/*
	** The file must already be open for write.
	*/
	if (fp != (FILE *) NULL)
	{
		/* Close previously openned log file. */
		if (msgStruct->logFileFlag == IMS_ON)
		{
			if (msgStruct->logFDOpen == IMS_ON) 
			{
				(void) close (msgStruct->logFileDesc);
			}

			if (msgStruct->logFPOpen == IMS_ON)
			{
				(void) fclose (msgStruct->logFilePtr);
			}
		}

		/* Assign new log file */
		msgStruct->logFileDesc = -1;
		msgStruct->logFDOpen = IMS_OFF;
		msgStruct->logFilePtr = fp;
		msgStruct->logFPOpen = IMS_OFF;
		msgStruct->logFileFlag = IMS_ON;
		msgStruct->logFileName = (char *)NULL;
	}
	else
	{
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgBanner ()
**
** Set banner and banner option in the message structure.
**
******************************************************************************/

int ims_msgBanner (
	IMS_MSG_STRUCT *msgStruct,
	char *banner,
	int option)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if ((banner == (char *) NULL) ||
		((int) strlen (banner) > IMS_COL255_LEN) ||
		(option < IMS_MSG_NOBANNER) || 
		(option > IMS_MSG_ONEBANNER)) 
	{
		(void) ims_msg (msgStruct, IMS_ERROR, 
			"ims_msgBanner(): Invalid input parameter.");
		return (IMS_ERROR);
	}
	
	(void) strcpy (msgStruct->banner, banner);
	msgStruct->bannerOption = option;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSubSystem ()
**
** Set the subsystem name in the message structure.
**
******************************************************************************/

int ims_msgSubSystem (
	IMS_MSG_STRUCT *msgStruct,
	char *subSystem)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if ((subSystem == (char *) NULL) ||
		 (strlen (subSystem) >= sizeof (msgStruct->subSystem)))
	{
		(void) ims_msg (msgStruct, IMS_ERROR, 
			"An invalid sub-system identifier was provided to the message facility.");
		return (IMS_ERROR);
	}

	(void) strcpy (msgStruct->subSystem, subSystem);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgProgramName ()
**
** Set the program name in the message structure.
**
******************************************************************************/

int ims_msgProgramName (
	IMS_MSG_STRUCT *msgStruct,
	char *programName)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if ((programName == (char *) NULL) ||
		 (strlen (programName) >= sizeof (msgStruct->programName)))
	{
		(void) ims_msg (msgStruct, IMS_ERROR, 
			"An invalid program name was provided to the message facility.");
		return (IMS_ERROR);
	}

	(void) strcpy (msgStruct->programName, programName);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgTimeFlag ()
**
** Set the time stamp flag in the message structure.
**
******************************************************************************/

int ims_msgTimeFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON)
	{
		msgStruct->timeStampFlag = IMS_ON;
	}
	else
	{
		msgStruct->timeStampFlag = IMS_OFF;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgAddInfoFlag ()
**
** Set the additional information flag in the message structure.
**
******************************************************************************/

int ims_msgAddInfoFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON)
	{
		msgStruct->addInfoFlag = IMS_ON;
	}
	else
	{
		msgStruct->addInfoFlag = IMS_OFF;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgInitSybMsg ()
**
** Initialize last Sybase error/message number and severity.
**
******************************************************************************/

int ims_msgInitSybMsg (
	IMS_MSG_STRUCT *msgStruct)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	msgStruct->lastSybErrNo = 0;
	msgStruct->lastSybErrSeverity = 0;
	msgStruct->lastSybMsgNo = 0;
	msgStruct->lastSybMsgSeverity = 0;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSybLevel ()
**
** Set Sybase message suppress level.
**
******************************************************************************/

int ims_msgSybLevel (
	IMS_MSG_STRUCT *msgStruct,
	int level)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	msgStruct->sybMsgLevel = level;
	
	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSybErrHndlFlag ()
**
** Set the Sybase error handle flag to on and install the Sybase client
** error handler.
**
******************************************************************************/

int ims_msgSybErrHndlFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *)NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON)
	{
		msgStruct->sybErrHndlFlag = IMS_ON;
		(void) dberrhandle (ims_reportSybError);
	}
	else
	{
		msgStruct->sybErrHndlFlag = IMS_OFF;
		(void) dberrhandle ((EHANDLEFUNC) NULL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSybMsgHndlFlag ()
**
** Set the Sybase message handle flag to on and install the Sybase client
** message handler.
**
******************************************************************************/

int ims_msgSybMsgHndlFlag (
	IMS_MSG_STRUCT *msgStruct,
	int flag)
{
	if (msgStruct == (IMS_MSG_STRUCT *)NULL)
	{
		return (IMS_ERROR);
	}

	if (flag == IMS_ON)
	{
		msgStruct->sybMsgHndlFlag = IMS_ON;
		(void) dbmsghandle (ims_reportSybMessage);
	}
	else
	{
		msgStruct->sybMsgHndlFlag = IMS_OFF;
		(void) dbmsghandle ((MHANDLEFUNC) NULL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgGetSeverity ()
**
** Return the last severity number.
**
******************************************************************************/

int ims_msgGetSeverity (
	IMS_MSG_STRUCT *msgStruct)
{
	return (msgStruct->lastSeverity);
}

/************************************************************************
**
** ims_msgGetSybLevel - return the setting for sybase message 
**                      suppress level.
**
************************************************************************/
int ims_msgGetSybLevel (msgStruct)
IMS_MSG_STRUCT *msgStruct;
{
	return (msgStruct->sybMsgLevel);
}

/******************************************************************************
**
** ims_msgGetSybErrNo ()
**
** Return the last Sybase error number.
**
******************************************************************************/

int ims_msgGetSybErrNo (
	IMS_MSG_STRUCT *msgStruct)
{
	return (msgStruct->lastSybErrNo);
}

/******************************************************************************
**
** ims_msgGetSybErrSeverity ()
**
** Return the last Sybase error severity.
**
******************************************************************************/

int ims_msgGetSybErrSeverity (
	IMS_MSG_STRUCT *msgStruct)
{
	return (msgStruct->lastSybErrSeverity);
}

/******************************************************************************
**
** ims_msgGetSybMsgNo ()
**
** Return the last Sybase message number.
**
******************************************************************************/

int ims_msgGetSybMsgNo (
	IMS_MSG_STRUCT *msgStruct)
{
	return (msgStruct->lastSybMsgNo);
}

/******************************************************************************
**
** ims_msgGetSybMsgSeverity ()
**
** Return the last Sybase message severity.
**
******************************************************************************/

int ims_msgGetSybMsgSeverity (
	IMS_MSG_STRUCT *msgStruct)
{
	return (msgStruct->lastSybMsgSeverity);
}

/******************************************************************************
**
** ims_msgOpenSyslog ()
**
** Open the syslog file for the provided facility.
**
******************************************************************************/

int ims_msgOpenSyslog (
	IMS_MSG_STRUCT *msgStruct,
	char *ident,
	int facility)
{
	if ((msgStruct == (IMS_MSG_STRUCT *) NULL) ||
		(ident == (char *) NULL))
	{
		return (IMS_ERROR);
	}

	/*
	** If the syslogFlag is already set to IMS_ON close the syslog
	** and set the syslogFlag to IMS_OFF. This will allow the user
	** to open a different syslog facility.
	*/
	if (msgStruct->syslogFlag == IMS_ON)
	{
		closelog ();
		msgStruct->syslogFlag = IMS_OFF;
	}

	/*
	** Open the syslog with the specified facility and identifier.
	*/
	openlog (ident, LOG_PID|LOG_CONS|LOG_NDELAY, facility);

	msgStruct->syslogFlag = IMS_ON;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgCloseSyslog ()
**
** Close the syslog facility log file.
**
******************************************************************************/

int ims_msgCloseSyslog (
	IMS_MSG_STRUCT *msgStruct)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	if (msgStruct->syslogFlag == IMS_ON)
	{
		closelog();
		msgStruct->syslogFlag = IMS_OFF;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgSetSyslogSeverity ()
**
** Any message with a severity value at or below the specified
** severity will be sent to the syslog facility.
**
******************************************************************************/

int ims_msgSetSyslogSeverity (
	IMS_MSG_STRUCT *msgStruct,
	int severity)
{
	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	msgStruct->syslogSeverity = severity;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_msgIKSyslog ()
**
** Extract any messages from the IK syslog file pertaining to ODL
** parsing errors and warnings.  The IK syslog file
** is created by the IK_NameSyslog() function and is closed by the
** IK_CloseSyslog() function.
**
******************************************************************************/

int ims_msgIKSyslog (
	IMS_MSG_STRUCT *msgDesc,
	char *syslogPath)
{
	FILE *filePtr;
	char syslogMsg[IMS_COL1024_LEN];
	char *syslogPtr;
	size_t msgLen;
	int eofFlag;

	if (msgDesc == (IMS_MSG_STRUCT *) NULL)
	{
		return (IMS_ERROR);
	}

	/*
	** Open the IK syslog file.
	*/
	if ((filePtr = fopen (syslogPath, "r")) == (FILE *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Unable to open IK syslog file '%s' for message extraction. %s",
			syslogPath, strerror (errno));
		return (IMS_ERROR);
	}

	/*
	** Process the messages in the IK syslog file and send
	** it to our message facility. We only care about the ODL
	** parsing messages which contain the string 'ODL'.
	*/
	eofFlag = IMS_FALSE;
	while (eofFlag != IMS_TRUE)
	{
		(void) fgets (syslogMsg, IMS_COL1024_LEN, filePtr);

		if ((syslogMsg == (char *) NULL) ||
			((msgLen = strlen (syslogMsg)) == 0))
		{
			eofFlag = IMS_TRUE;
		}
		else
		{
			/* Remove the return value from the message. */
			syslogMsg[msgLen-1] = '\0';

			syslogPtr = strstr (syslogMsg, "ODL");

			/* Ignore messages that are missing the 'ODL' string. */
			if (syslogPtr != (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, syslogPtr);
			}
		}

		syslogMsg[0] = '\0';
	}

	(void) fclose (filePtr);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_reportSybError ()
**
** Report an error generated by a Sybase DB-Library function.
**
** This call is made by DB-Library routines. The return codes are those
** recongnized by DB-Library.
**
******************************************************************************/

int ims_reportSybError (
	DBPROCESS *dbproc,
	int dbSeverity,
	int dbErrNo,
	int osErrNo, 
	char *dbErrStr,
	char *osErrStr)
{
	IMS_MSG_STRUCT *msgStruct;
	int currSev;

	/*
	** Get the message descriptor from the dbproc.
	**
	** lint: pointer cast may result in improper alignment
	** ???? Review this.
	*/
	if ((msgStruct = (IMS_MSG_STRUCT *) dbgetuserdata (dbproc)) == 
		(IMS_MSG_STRUCT *) NULL)
	{
		/* Assign a message structure with default values. */
		msgStruct = &glbMsgStruct;
	}

	/*
	** Set the severity level based on the dbSeverity level.
	** 10 and below get severity of IMS_INFO; these are not Sybase errors.
	** For levels (Sybase serverity that is) in the range 11 - 16 set
	** severity to IMS_ERROR.  Anything higher, set the severity
	** level to IMS_FATAL.
	** Between 11 and 16 usually means a programmer error, like SQL
	** statement syntax error.  Most errors that are higher than that
	** mean that Sybase has a severe error that should be noticed and
	** acted upon; the dataserver may have a corruption, for example.
	*/
	if (dbSeverity <= 10)
	{
		currSev = IMS_INFO;
	}
	else if (dbSeverity <= 16)
	{
		currSev = IMS_ERROR;
	}
	else
	{
		currSev = IMS_FATAL;
	}

	/*
	** Send the error message if required.  One example of where it is not
	** required is when the message just tells the user to look at the
	** messages from the sqlServer (dbErrNo == 20018).
	*/
	if (msgStruct->sybErrHndlFlag && dbErrNo != 20018)
	{
		/*
		** Record the error number and severity.
		*/
		msgStruct->lastSybErrNo = dbErrNo;
		msgStruct->lastSybErrSeverity = dbSeverity;

		/*
		** Add the DBLIB message to a message header and send it out.
		*/
		(void) ims_msg (msgStruct, currSev, 
			"Sybase error (Svr %d, No %d): %s", 
			dbSeverity, dbErrNo, dbErrStr);

		/*
		** If Sybase has encountered a system error, report it.
		*/
		if (osErrNo != DBNOERR)
		{
			(void) ims_msg (msgStruct, currSev, 
				"Sybase OS error (Svr %d, No %d): %s", 
				dbSeverity, osErrNo, osErrStr);
		}
	}

	return (INT_CANCEL);
}

/******************************************************************************
**
** ims_reportSybMessage ()
**
** Report a message sent from the SQL Server to a Sybase DB-Library routine.
**
** This routine is called by the DB-Library. Return parameters are those
** expected by DB-Library.  In particular, do not have DB-Library save
** its messages.
**
** Note:  There is a weird effect here:  If during login, a login user has
**        a dafault database, a message during login will indicate the
**        change from master to the default db, but it will be proceded
**        by a "dead or not enabled" library message.  I hope to be able
**        to suppress the "dead or not enabled" message in this case.
**
******************************************************************************/

int ims_reportSybMessage (
	DBPROCESS *dbproc,
	DBINT msgNo,
	int msgState,
	int dbSeverity,
	char *msgText,
	char *srvName,
	char *procName,
	int line)
{
	IMS_MSG_STRUCT *msgStruct;
	int currSev;

	/*
	** Get the message descriptor from the dbproc.
	**
	** lint: pointer cast may result in improper alignment
	** ???? Review this.
	*/
	if ((msgStruct = (IMS_MSG_STRUCT *) dbgetuserdata (dbproc)) == 
		(IMS_MSG_STRUCT *) NULL)
	{
		/* Assign a message structure with default values. */
		msgStruct = &glbMsgStruct;
	}

	/*
	** If the db severity level is below or at the suppress level, ignore
	** the message and return with things normal.
	*/
	if (dbSeverity <= msgStruct->sybMsgLevel)
	{
		return (DBNOSAVE);
	}

	/*
	** Set the severity level based on the dbSeverity level.
	** 10 and below get severity of IMS_INFO; these are not Sybase errors.
	** For levels (Sybase serverity that is) in the range 11 - 16 set
	** severity to IMS_ERROR.  Anything higher, set the severity
	** level to IMS_FATAL.
	** Between 11 and 16 usually means a programmer error, like SQL
	** statement syntax error.  Most errors that are higher than that
	** mean that Sybase has a severe error that should be noticed and
	** acted upon; the dataserver may have a corruption, for example.
	**
	** For messages that originated from our stored procedures we have
	** special message numbers that represent our severity.
	*/
	switch (msgNo)
	{
	case 20101:
		currSev = IMS_WARNING;
		break;

	case 20102:
		currSev = IMS_ERROR;
		break;

	case 20103:
		currSev = IMS_FATAL;
		break;

	default:
		if (dbSeverity <= 10)
		{
			currSev = IMS_INFO;
		}
		else if (dbSeverity <= 16)
		{
			currSev = IMS_ERROR;
		}
		else
		{
			currSev = IMS_FATAL;
		}
	}

	/*
	** Send the error message if required.
	*/
	if (msgStruct->sybMsgHndlFlag == IMS_ON)
	{
		/*
		** Record the error number and severity.
		*/
		msgStruct->lastSybMsgNo = msgNo;
		msgStruct->lastSybMsgSeverity = dbSeverity;

		if (strlen (procName) == 0)
		{
			(void) ims_msg (msgStruct, currSev,
				"Sybase message (Svr %d, No %d, Srv %s, Db %s): %s",
				dbSeverity, msgNo, srvName, dbname(dbproc), msgText);
		}
		else
		{
			(void) ims_msg (msgStruct, currSev,
				"Sybase message (Svr %d, No %d, Srv %s, Db %s, Proc %s): %s",
				dbSeverity, msgNo, srvName, dbname(dbproc), procName,
				msgText);
		}
	}

	return (INT_CANCEL);
}

/******************************************************************************
**
** insertMsgQueue ()
**
** Insert a message into the message queue.
**
******************************************************************************/

static int insertMsgQueue (
	IMS_MSG_STRUCT *msgStruct,
	IMS_MSG_QUEUE *msgQueue)
{
	if ((msgStruct == (IMS_MSG_STRUCT *) NULL) || 
		(msgQueue == (IMS_MSG_QUEUE *) NULL))
	{
		return (IMS_FATAL);
	}

	if ((msgQueue->next = msgStruct->first) == (IMS_MSG_QUEUE *) NULL)
	{
		/* This is the first queued message. */
		msgStruct->last = msgQueue;
	}

	/* Increment the sigEventCounter in the msg structure */
	if (msgQueue->sigEvent)
	{
		msgStruct->sigEventCounter += 1;
	}

	msgStruct->first = msgQueue;

	return (IMS_OK);
}
