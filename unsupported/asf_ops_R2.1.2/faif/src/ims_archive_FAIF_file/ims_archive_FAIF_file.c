/*==============================================================================
Filename:	ims_archive_FAIF_file.c

Description:	
	Library function for ims_archive of a FAIF file and associated PMF

External Functions:
	ims_archive
	
Static Functions:
	
External Variables Defined:

File Scope Static Variables:

Creator:        Rich Norman (richard@orca.jpl.nasa.gov) Shameless plagarization
                from Sean Hardman.

Notes:
==============================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include "faifdefs.h"

#include <unistd.h>
#include <syslog.h> /* Output goes to /opt/test/log/FAIFsyslog.log */
#include <sys/wait.h>
#include <sys/types.h>

#include <ims_query.h>
#include <ims_archive.h>
#include "ims_archive_faif.h"

int ims_archive_FAIF_file (char *programName, char *configfile,
                           char *recept_dir, char *file_name,
                           char *platform, char *dataset)

{

/* Static Variables. */
static IMS_CLNT_EVENT request;
static IMS_MSG_STRUCT *msgDesc;
static IMS_MSG_QUEUE *msgQueue;

/* Event information variables. */
char event[IMS_NAME_LEN +1];
char *server   = NULL;
char *database = NULL;
char *sourceDir = NULL;

/* Values for a specific granule. */
char *username = NULL;
char *password = NULL;
char *accountId = NULL;
char *sensor = NULL;
char *format = "FA";
short version = -1;
short fileCount = 2;
char *extensions[] = {"D", "M"};
char localArchiveFlag = 'N';
char logmsg[MAX_SYSLOG_MSGLEN + 1];
int status;
char *last_dot;
char *internal_file_name;

/* Get environmental definitions from configuration file.*/
        sourceDir = (char *)strdup( recept_dir) ;
        server    = (char *)get_config_val(configfile, IMS_DEV_EV);
        database  = (char *)get_config_val(configfile, IMS_DB_EV);
        username  = (char *)get_config_val(configfile, IMS_USERNAME_EV);
        accountId = (char *)get_config_val(configfile, IMS_ACCT_ID_EV);
        password  = (char *)get_config_val(configfile, IMS_PASSWD_EV);

	/*
	** Allocate and initialize the message facility.
	*/

	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
           sprintf (logmsg,
            "Memory allocation for IMS_MSG_STRUCT structure failed.");
           syslog(LOG_ERR, logmsg);
           return (1);
	}

	/* Inititialize the message facility options. */
	(void) ims_msgSubSystem (msgDesc, "IMS");
/*        (void) ims_msgProgramName (msgDesc, programName); */
	(void) ims_msgBanner (msgDesc, programName, IMS_MSG_ALLBANNER);
	(void) ims_msgStderrFlag (msgDesc, IMS_OFF);
	(void) ims_msgQueueFlag (msgDesc, IMS_ON);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/* Populate the event request structure. */

	request.username = username;
	request.password = password;
	request.accountId = accountId;
	request.platform = platform;
	request.sensor = sensor;
	request.dataset = dataset;
/*	request.name = file_name; */

        internal_file_name = (char *)strdup(file_name);
        last_dot = (char *) strrchr(internal_file_name, '.');
        strcpy(last_dot, "\0");
        request.name = internal_file_name;

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
        request.requestType = IMS_REPLACE;
        status = ims_archive (&request);

        free(internal_file_name);

#ifdef TEST
	/*
	** Call library function to remove file from IMS.
	*/
        request.requestType = IMS_DELETE;
	status = ims_archive (&request);
#endif

/* Extract queued messages and put to syslog */

        while ((msgQueue = ims_msgQueueExtract(msgDesc))
         != (IMS_MSG_QUEUE *) NULL)
        {
           syslog(LOG_ERR, msgQueue->rawMsg);
           (void) ims_msgQueueFree (msgQueue);
        }

        (void) ims_msgQueueFlag (msgDesc, IMS_OFF);

/* Close the log file and free the message descriptor structure. */
        (void) ims_msgStructFree (msgDesc);

	if (status < IMS_OK)
	{
           sprintf (logmsg,
            "ims_archive failed. Returned status = %d", status);
           syslog(LOG_ERR, logmsg);
           return (1);
	}
	else
	{
           return (0);
	}
}
