static char *sccs = "@(#)ims_srvShipMsgs.c	5.3  03/13/97";
/******************************************************************************
**
** File:        ims_srvShipMsgs.c
**
** Function:    Function call to ship all ims_msg() queued messages
**              to the client.
**
** Author:      Jeff Jacobson
**
** Date:        9/22/90
**
** Modified:    8/19/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/9/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
******************************************************************************/

#include <stdio.h>
#include <sys/types.h>

#include <ims_dbms.h>
#include <ospublic.h>
#include <oserror.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_hash.h>
#include <ims_archive.h>
#include <ims_ftsSrvCat.h>
#include <ims_ftsSrv.h>
#include <ims_util.h>
/* #include <ims_krb.h> */

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_ftsSrv.h.
** They are listed here for reference.
**
**	int ims_srvShipMsgs (register SRV_PROC *);
**	int ims_srvLogMsgs (register SRV_PROC *);
*/

/******************************************************************************
**
** ims_srvShipMsgs ()
**
** De-queue all CDB messages and relay them to the client
** associated with the srvproc.
**
******************************************************************************/

int ims_srvShipMsgs (
	register SRV_PROC *srvproc)
{
	IMS_MSG_QUEUE *msgQueue;
	FTS_PROC_DESC *procDesc;
	FTS_CAT_STATESPEC *stateSpec;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];
	int counter;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvShipMsgs.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg,
			CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		srvMsg.severity = SRV_FATAL_PROCESS;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvShipMsgs.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg,
			CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		srvMsg.severity = SRV_FATAL_PROCESS;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	stateSpec = &(procDesc->catReq.stateSpec);


	/*
	** While there are messages, then ship them to the client.
	*/
	counter = 0;
	while ((msgQueue = ims_msgQueueExtract (procDesc->msgStruct)) 
		!= (IMS_MSG_QUEUE *) NULL)
	{
		counter++;

		/*
		** First, decode the IMS_SEVERITY, and map it to some
		** SRV severity.
		*/
		switch (msgQueue->severity)
		{
			case IMS_INFO:
				srvMsg.severity = SRV_INFO;
				break;

			case IMS_WARNING:
				srvMsg.severity = SRV_FATAL_PROCESS;
				break;

			case IMS_ERROR:
				srvMsg.severity = SRV_FATAL_PROCESS;
				break;

			case IMS_FATAL:
				srvMsg.severity = SRV_FATAL_SERVER;
				break;

			default:
				srvMsg.severity = SRV_FATAL_SERVER;
		}

		/*
		** Now, send this message to the client.
		*/

		/*
		** Place the message in log file if severity is IMS_FATAL.
		*/
		if ((procDesc->recordMsgs) || 
			(msgQueue->severity <= IMS_FATAL))
		{
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msgQueue->rawMsg, 
				CS_NULLTERM);
			(void) sprintf (msg, "\n");
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, msg, 
				CS_NULLTERM);
		}

		if (msgQueue->sigEvent)
		{
			/*
			** Format the string to support single quotes.
			*/
			(void) ims_formatQuotedString (msgQueue->rawMsg,
				msg);

			/*
			** Truncate the message for the sigEvents database.
			*/
			msg[255] = '\0';

			/*
			** Generated messages by ims_srvCat() are placed in
			** the queue.
			*/
			stateSpec->msg = msg;
			stateSpec->eventType = "ERROR";
			stateSpec->eventNumber = msgQueue->number;
			(void) ims_srvCat (srvproc, FTS_ADD_SIG_EVENT); 
		}


		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_QUEUED_MSG;
		(void) strcpy (srvMsg.text, msgQueue->rawMsg);
		srvMsg.textlen = (CS_INT) strlen (msgQueue->rawMsg);

		if (srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED) == CS_FAIL)
		{
			(void) ims_msgQueueFree (msgQueue);
			return (IMS_FATAL);
		}

		(void) ims_msgQueueFree (msgQueue);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_srvLogMsgs ()
**
** De-queue all CDB messages and log them in the log file.
**
******************************************************************************/

int ims_srvLogMsgs (
	register SRV_PROC *srvproc)
{
	IMS_MSG_QUEUE *msgQueue;
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_srvShipMsgs.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_srvShipMsgs.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** While there are messages, then ship them to the client.
	*/
	while ((msgQueue = ims_msgQueueExtract (procDesc->msgStruct)) 
		!= (IMS_MSG_QUEUE *) NULL)
	{
		(void) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msgQueue->rawMsg, 
			CS_NULLTERM);
		(void) sprintf (msg, "\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, msg, 
			CS_NULLTERM);
		(void) ims_msgQueueFree (msgQueue);
	}

	return (IMS_OK);
}
