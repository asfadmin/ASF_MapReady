#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps2ims_archive.c

Description:	

External Functions Defined:
				aps2ims_archive
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)aps2ims_archive.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/stoic/SCCS/s.aps2ims_archive.c"

/******************************************************************************
**
** File:        Function aps2ims_archive.c
**
** Function:    .
**
** Author:     Lisa Nguyen 
**
** Date:        10 Feb 96
**
**
** The following command will make the executable on SunOS Release 5.x:
**
** cc -I/asf/include/imsdads -I$SYBASE/include -L/asf/lib -L$SYBASE/lib \
** aps2ims_archive.c /asf/lib/sparc/libims.a -lsybdb -lnsl -lm -o \ 
## aps2ims_archive.exe
**
** Parameter need to pass in subroutine
** static char *dataset = "ASF STOIC"; or "ASF GREENWICH HOUR ANGLE"
** static char *name = ""; <base_file_name> withouout extention
** static char *sourceDir = getenv("stoicfile_dir"); or getenv("gha_dir"); 
**
22 Apr 96  VAH  added code to read IMS params from config file 
                (environment var GHA_CONFIG)
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ims_query.h> 
#include <ims_archive.h> 

static int read_init_file( IMS_CLNT_EVENT *ims_event);

/* Static Variables. */
static IMS_CLNT_EVENT request;
static IMS_MSG_STRUCT *msgDesc;
static IMS_MSG_QUEUE *msgQueue;

#if 0
/* Next 5 variables read from config file ! */

/* Event information variables. */
static char *server = "IMSDBSRV";
static char *database = "ims_catalog";

/* Static values for a specific granule. */
static char *username = "faif_clnt";
static char *password = "faif_clnt";
static char *accountId = "ACCT_FAIF";
#endif

static char *platform = "ASF";
static char *sensor = "";
static char *format = "FA";
static short version = -1;
static short fileCount = 2;
static char *extensions[] = {"M", "D"};
static char localArchiveFlag = 'N';
static char *programName = "aps2ims_arch";

/******************************************************************************
**
** int aps2ims_archive()
**
******************************************************************************/
int aps2ims_archive(char *dataset, char *name, char *sourceDir)
{
   int status;
/*
** Allocate and initialize the message facility.
*/
if ((msgDesc = ims_msgStructAlloc ()) == NULL)
{
   fprintf (stderr, "Mem allocation for IMS_MSG_STRUCT struct failed");
   exit (1);
}

/* Inititialize the message facility options. */
ims_msgSubSystem (msgDesc, "IMS");
ims_msgBanner (msgDesc, programName, IMS_MSG_ALLBANNER);
ims_msgStderrFlag (msgDesc, IMS_OFF);
ims_msgQueueFlag (msgDesc, IMS_ON);
ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

/* Populate the event request structure. */
if (read_init_file( &request) < 0)
{
   fprintf (stderr, "Error reading init file \"GHA_CONFIG\"\n");
   exit (1);
} 
#if 0
else {
   fprintf (stderr,"DBG: Server = %s\n",request.catSrvName);
   fprintf (stderr,"DBG: Database = %s\n",request.catDbName);
   fprintf (stderr,"DBG: User = %s\n",request.username);
   fprintf (stderr,"DBG: Password = %s\n",request.password);
   fprintf (stderr,"DBG: account = %s\n",request.accountId);
}
#endif
/* 
request.username = username;
request.password = password;
request.accountId = accountId;
request.catSrvName = server;
request.catDbName = database;
*/

request.requestType = IMS_ADD;

/* Optional */
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
request.msgDesc = msgDesc;

/*
** Call library function to handle the request.
*/

status = ims_archive (&request);

/* Extract messages from the message queue. */
while ((msgQueue = ims_msgQueueExtract (msgDesc)) != NULL)
{
   fprintf (stdout, "%s\n", msgQueue->msg);
   ims_msgQueueFree (msgQueue);
}

/* Free the message descriptor. */
ims_msgStructFree (msgDesc);

if (status < IMS_OK)
   exit (1);
else
   exit (0);
}


static int read_init_file(
   IMS_CLNT_EVENT *ims_event
)
{
static char ims_user[100], ims_pwd[100], ims_acnt[100], 
   ims_server[100], ims_db[100];
char configfn[100], *tmpptr;
FILE *fp;
char linein[80], key[80], val[80];

   tmpptr = getenv("GHA_CONFIG");

   if (tmpptr == NULL)
      return(-1);
   
   strcpy(configfn,tmpptr);

   if ((fp = fopen(configfn, "r")) == NULL)
   {
      sprintf (linein, "Error opening config file %s",configfn);
      perror (linein);
      return(-1);
   }

   ims_event->username = NULL;
   ims_event->password = NULL;
   ims_event->accountId = NULL;
   ims_event->catDbName = NULL;
   ims_event->catSrvName = NULL;

   while (fgets(linein,80,fp) !=NULL)
   {
#if 0
fprintf (stderr, "DBG: read %s\n",linein);
#endif
      tmpptr = strchr(linein,'#');         /* remove comment */
      if (tmpptr != NULL) *tmpptr = '\0';

      tmpptr = strchr(linein,'\n');        /* remove trailing newline */
      if (tmpptr != NULL) *tmpptr = '\0';

      tmpptr = strchr(linein,'=');         /* need <key>=<value> */
      if (tmpptr == NULL) continue;
      *tmpptr = (char)' ';

      sscanf(linein,"%s %s",key,val);

      if (strcmp(key,"GHA_USER") == 0)
      {
         strcpy(ims_user,val);
         ims_event->username = ims_user;
      }

      if (strcmp(key,"GHA_PASSWORD") == 0)
      {
         strcpy(ims_pwd,val);
         ims_event->password = ims_pwd;
      }

      if (strcmp(key,"GHA_ACCOUNT") == 0)
      {
         strcpy(ims_acnt,val);
         ims_event->accountId = ims_acnt;
      }

      if (strcmp(key,"GHA_SERVER") == 0)
      {
         strcpy(ims_server,val);
         ims_event->catSrvName = ims_server;
      }
      if (strcmp(key,"GHA_DB") == 0)
      {
         strcpy(ims_db,val);
         ims_event->catDbName = ims_db;
      }
   }
   fclose(fp);
   return(0);
}
