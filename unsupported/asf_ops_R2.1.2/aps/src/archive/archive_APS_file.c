#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		archive_APS_file.c

Description:	Library function to invoke ims_archive() to archive APS file 
				and the associated PMF into IMS

External Functions:
				ims_archive()
	
Static Functions:
	
External Variables Defined:

File Scope Static Variables:

Notes:
==============================================================================*/
#pragma ident   "@(#)archive_APS_file.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/archive/SCCS/s.archive_APS_file.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <aps_log_msg.h>
#include "aps_defs.h"
#include <sys/utsname.h>

/*
#include "faifdefs.h"
#include "ims_archive_faif.h"
*/

#include <unistd.h>
#include <syslog.h> /* Output goes to /opt/test/log/FAIFsyslog.log */
#include <sys/wait.h>
#include <sys/types.h>

#include <ims_query.h>
#include <ims_archive.h>
 

/* local function */

void display_error(char *,char *);


int archive_APS_file (char *programName,
		      char *sourceDir,
		      char *file_name,
		      char *platform,
		      char *dataset,
			  char *format,
			  char **extensions,
			  char *userid,
			  char *password,
			  int  fileCount,
			  int  delFlag)
{

/* Static Variables. */
static IMS_CLNT_EVENT request;
static IMS_MSG_STRUCT *msgDesc;
static IMS_MSG_QUEUE *msgQueue;

char *server   = NULL;
char *database = NULL;

/* Values for a specific granule. */

char *accountId = NULL;
char *sensor = NULL; /* Eventually NULL will be ok here. */
short version = -1;
char localArchiveFlag = 'N';
char msg[MSG_LEN];
int status;
char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
char hostName[IMS_HOST_LEN+1];
struct utsname uname_info;     /*Structure for uname() */

	request.username = userid;
	free(&userid);

	/* Get environmental definitions from configuration file.*/
    if ((server = (char *)getenv("IMS_SERVER")) == NULL)
	{
	   display_error(programName,"IMS_SERVER");
	}

    if ((database  = (char *)getenv("IMS_DB")) == NULL)
	{
	   display_error(programName,"IMS_DB");
	}

    if ((accountId = (char *)getenv("IMS_ACCOUNT_ID")) == NULL)
	{
	   display_error(programName,"IMS_ACCOUNT_ID");
	}

	/*
	** Allocate and initialize the message facility.
	*/

	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		sprintf (msg,
		    "Memory allocation for IMS_MSG_STRUCT structure failed.");
		aps_log_msg(programName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		return (1);
	}

	/* Inititialize the message facility options. */
	(void) ims_msgSubSystem (msgDesc, APS_SUBSYS_NAME);
	(void) ims_msgProgramName (msgDesc, programName);

	/*
	** Get the node name.
	*/
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';

	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);

    (void) sprintf (msg, "%s:", APS_SUBSYS_NAME);
    (void) ims_msgOpenSyslog (msgDesc, msg, LOG_LOCAL1);
	(void) ims_msgStderrFlag (msgDesc, IMS_OFF);
	(void) ims_msgQueueFlag (msgDesc, IMS_ON);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/* Populate the event request structure. */

	request.password = password;
	request.accountId = accountId;
	request.platform = platform;
	request.sensor = sensor;
	request.dataset = dataset;
	request.name = file_name;
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
	** Not needed. Will be looked up automatically by IMS.
	request.ftsSrvName = "FTS_TEST";
	*/

	/*
	** Call library function to remove file from IMS.
	*/
	if (delFlag)
	{
		request.requestType = IMS_DELETE;
    }
	else
	{
		request.requestType = IMS_REPLACE;
	}

	/*
	** Call library function to handle the request.
	*/

	status = ims_archive (&request);

	/* Extract queued messages */ 
	if (status != IMS_OK)
	{
    	while ((msgQueue = ims_msgQueueExtract(msgDesc)) != 
												(IMS_MSG_QUEUE *) NULL)
		{
			fprintf(stderr, "%s\n", msgQueue->msg);
			(void) ims_msgQueueFree (msgQueue);
		}
	}
    else
	{	
		while ((msgQueue = ims_msgQueueExtract(msgDesc)) != 
												(IMS_MSG_QUEUE *) NULL)
		{
			fprintf(stdout, "%s\n", msgQueue->msg);
			(void) ims_msgQueueFree (msgQueue);
		}
	}

	(void) ims_msgQueueFlag (msgDesc, IMS_OFF);

	/* Close the log file and free the message descriptor structure. */
    (void) ims_msgStructFree (msgDesc);

	if (status < IMS_OK)
	{
        return (1);
	}
	else
	{
		return (0);
	}
}


/*==============================================================================

display_error()

==============================================================================*/
void display_error(char *programName,char * env_name)
{
char msg[MSG_LEN];
	sprintf(msg,"Enviorment name = '%s' was not specified!", env_name);
	aps_log_msg(programName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
	exit(APS_EXIT_ERROR);
}
