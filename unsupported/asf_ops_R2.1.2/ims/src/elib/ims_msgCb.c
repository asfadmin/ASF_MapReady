static char *sccs = "@(#)ims_msgCb.c	1.2 08/11/97";
/******************************************************************************
**
** File:        ims_msgCb.c
**
** Function:    Message facility callback for Sybase.
**
** Author:      S. Hardman
**
** Date:        8/30/96
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ctpublic.h>
#include <ims_const.h>
#include <ims_msg.h>

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
** ims_msgClientCb ()
**
******************************************************************************/

CS_RETCODE CS_PUBLIC ims_msgClientCb (
	CS_CONTEXT *context,
	CS_CONNECTION *connection,
	CS_CLIENTMSG *errmsg)
{
	IMS_MSG_STRUCT *msgStruct;
	CS_INT retCode;
	int imsSeverity;
	int sybSeverity;
	int sybErrno;

	/*
	** Initialize variables.
	*/
	msgStruct = (IMS_MSG_STRUCT *) NULL;
	sybSeverity = CS_SEVERITY(errmsg->msgnumber);
	sybErrno = CS_NUMBER(errmsg->msgnumber);

	/*
	** Get the message descriptor from the connection structure.
	*/
	if ((retCode = cs_config (context, CS_GET, CS_USERDATA,
		(CS_VOID *) msgStruct, (CS_INT) sizeof (msgStruct),
		(CS_INT *) NULL)) != CS_SUCCEED)
	{
		/* Assign a message structure with default values. */
		msgStruct = &glbMsgStruct;
	}

	if (msgStruct == (IMS_MSG_STRUCT *) NULL)
	{
		msgStruct = &glbMsgStruct;
	}

	/*
	** Consider suppressing messages with a severity that falls
	** below the message facility suppress level.
	*/

	/*
	** Set the IMS severity level based on the Sybase severity level.
	*/
	if (sybSeverity <= 10)
	{
		imsSeverity = IMS_INFO;
	}
	else if (sybSeverity <= 16)
	{
		imsSeverity = IMS_ERROR;
	}
	else
	{
		imsSeverity = IMS_FATAL;
	}

	/*
	** Record the error number and severity.
	*/
	msgStruct->lastSybErrNo = sybErrno;
	msgStruct->lastSybErrSeverity = sybSeverity;

	/*
	** Generate the client message.
	*/
	(void) ims_msg (msgStruct, imsSeverity, 
		"Sybase Client (Severity '%ld', Number '%ld'): %s", 
		sybSeverity, sybErrno, errmsg->msgstring);

	/*
	** Generate an operating system error if encountered.
	*/
	if (errmsg->osstringlen > 0)
	{
		(void) ims_msg (msgStruct, imsSeverity, 
			"Sybase OS Error: %s", errmsg->osstring);
	}

	return (CS_SUCCEED);
}

/******************************************************************************
**
** ims_msgServerCb ()
**
******************************************************************************/

CS_RETCODE CS_PUBLIC ims_msgServerCb (
	CS_CONTEXT *context,
	CS_CONNECTION *connection,
	CS_SERVERMSG *srvmsg)
{
	IMS_MSG_STRUCT *msgStruct;
	CS_INT retCode;
	int imsSeverity;

	/*
	** Ignore change of database message.
	*/
	if (srvmsg->msgnumber == 5701)
	{
		return (CS_SUCCEED);
	}

	/*
	** Get the message descriptor from the connection structure.
	*/
	if ((retCode = cs_config (context, CS_GET, CS_USERDATA,
		(CS_VOID *) &msgStruct, (CS_INT) sizeof (IMS_MSG_STRUCT),
		(CS_INT *) NULL)) != CS_SUCCEED)
	{
		/* Assign a message structure with default values. */
		msgStruct = &glbMsgStruct;
	}

	/*
	** Consider suppressing messages with a severity that falls
	** below the message facility suppress level.
	*/

	/*
	** Set the IMS severity level based on the Sybase severity level.
	**
	** For messages that originated from our stored procedures we have
	** special message numbers that represent our severity.
	*/
	switch (srvmsg->msgnumber)
	{
	case 20101:
		imsSeverity = IMS_WARNING;
		break;

	case 20102:
		imsSeverity = IMS_ERROR;
		break;

	case 20103:
		imsSeverity = IMS_FATAL;
		break;

	default:
		if (srvmsg->severity <= 10)
		{
			imsSeverity = IMS_INFO;
		}
		else if (srvmsg->severity <= 16)
		{
			imsSeverity = IMS_ERROR;
		}
		else
		{
			imsSeverity = IMS_FATAL;
		}
	}

	/*
	** Record the error number and severity.
	*/
	msgStruct->lastSybMsgNo = srvmsg->msgnumber;
	msgStruct->lastSybMsgSeverity = srvmsg->severity;

	/*
	** Generate the server message.
	*/
	(void) ims_msg (msgStruct, imsSeverity, 
		"Sybase Server (Severity '%ld', Number '%ld', Server '%s', "
		"Proc '%s'): %s", 
		srvmsg->severity, srvmsg->msgnumber, srvmsg->svrname,
		srvmsg->proc, srvmsg->text);

	return (CS_SUCCEED);
}

