static char *sccs = "@(#)ims_archiveEx.c	5.2  03/13/97";
/******************************************************************************
**
** File:        ims_archiveEx.c
**
** Function:    IMS example program for the FTS client event interface.
**
** Author:      S. Hardman
**
** Date:        1/23/95
**
** Notes:
**
** The following command will make the executable on on SunOS Release 5.x:
**
** cc -I/$ASF/include/imsdads -I$SYBASE/include \
** -L/$ASF/lib/sparc -L$SYBASE/lib \
** ims_archiveEx.c -lims -lct -lcs -ltcl -lnsl -lm -o ims_archiveEx
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include <ims_query.h>
#include <ims_archive.h>

/* Local Functions. */
static void whichEvent (void);

/* Static Variables. */
static IMS_CLNT_EVENT request;
static IMS_MSG_STRUCT *msgDesc;
static IMS_MSG_QUEUE *msgQueue;

/* Event information variables. */
static char event[IMS_NAME_LEN +1];
static char server[IMS_NAME_LEN + 1];
static char database[IMS_NAME_LEN + 1];

/* Static values for a specific granule. */
static char *username = "ims_clnt";
static char *password = "ims_clnt";
static char *accountId = "ACCT_ALL";
static char *platform = "ERS-1";
static char *sensor = "SAR";
static char *dataset = "ERS-1 ICE CLASSIFICATIONS";
static char *name = "DEMO2";
static char *format = "HDF";
static short version = -1;
static short fileCount = 2;
static char *extensions[] = {"M", "D"};
static char *sourceDir = "../../src/examples/files";
static char localArchiveFlag = 'N';
static char *programName = "ims_archiveEx";

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (void)
{
	int status;

	/*
	** Allocate and initialize the message facility.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (1);
	}

	/* Inititialize the message facility options. */
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgBanner (msgDesc, programName, IMS_MSG_ALLBANNER);
	(void) ims_msgStderrFlag (msgDesc, IMS_OFF);
	(void) ims_msgQueueFlag (msgDesc, IMS_ON);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/* Specify the parameters for event. */
	(void) fprintf (stdout, "Enter event         : ");
	(void) gets (event);
	(void) fprintf (stdout, "Enter server name   : ");
	(void) gets (server);
	(void) fprintf (stdout, "Enter database name : ");
	(void) gets (database);

	/* Populate the event request structure. */
	whichEvent ();
	request.username = username;
	request.password = password;
	request.accountId = accountId;
	request.platform = platform;
	request.sensor = sensor;
	request.dataset = dataset;
	request.name = name;
	request.format = format;
	request.version = version;
	request.fileCount = fileCount;
	request.extensions = extensions;
	request.sourceDir = sourceDir;
	request.localArchiveFlag = localArchiveFlag;
	request.programName = programName;
	request.catSrvName = server;
	request.catDbName = database;
	request.msgDesc = msgDesc;

	/*
	** Call library function to handle the request.
	*/
	status = ims_archive (&request);

	/* Extract messages from the message queue. */
	while ((msgQueue = ims_msgQueueExtract (msgDesc)) !=
		(IMS_MSG_QUEUE *) NULL)
	{
		(void) fprintf (stdout, "%s\n", msgQueue->msg);
		(void) ims_msgQueueFree (msgQueue);
	}

	/* Free the message descriptor. */
	(void) ims_msgStructFree (msgDesc);

	if (status < IMS_OK)
	{
		exit (1);
	}
	else
	{
		exit (0);
	}
}

/******************************************************************************
**
** whichEvent ()
**
******************************************************************************/

static void whichEvent (void)
{
	/* Event switch, setup the request type. */
	switch (event[0])
	{
	case 'a':
		request.requestType = IMS_ADD;
		break;

	case 'g':
		request.requestType = IMS_GET;
		break;

	case 'd':
		request.requestType = IMS_DELETE;
		break;

	case 'r':
		request.requestType = IMS_REPLACE;
		break;

	default:
		request.requestType = IMS_NO_REQUEST;
	}
}

