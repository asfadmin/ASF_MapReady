/******************************************************************************
**
** File:        ims_msg.h
**
** Function:    Header file for message facility structures and prototypes.
**
** Modified:    11/28/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
******************************************************************************/

#ifndef _IMS_MSG_H
#define _IMS_MSG_H

static char *sccsMsg = "@(#)ims_msg.h	5.1  03/16/96";

/*
** Message facility return severities.
*/
#define IMS_HELP       3
#define IMS_INFO       2
#define IMS_OK         1
#define IMS_MSGFAIL    0
#define IMS_WARNING   -1
#define IMS_ERROR     -2
#define IMS_FATAL     -3

/*
** Banner types.
*/
#define IMS_MSG_NOBANNER  0
#define IMS_MSG_ALLBANNER 1
#define IMS_MSG_ONEBANNER 2

/*
** Syslog Definitions.
*/
#define IMS_MAX_SYSLOG_MSG 1024

/*
** Message queue structure.
*/
typedef struct ims_msg_queue
{
	int severity;                   /* msg severity */
	int type;                       /* msg type */
	int number;                     /* msg number */
	int class;                      /* msg class */ 
	int state;                      /* msg state */
	int sigEvent;                   /* msg significant event flag */
	int msgLength;                  /* msg length */
	char *msg;                      /* complete msg */
	char *rawMsg;                   /* raw part of the msg. no banners */
	struct ims_msg_queue *next;     /* pointer to next msg in queue */
} IMS_MSG_QUEUE;

/*
** Message descriptor structure.
*/
typedef struct ims_msg_struct
{
	int lastSeverity;                /* last severity level */
	char subSystem[IMS_COL30_LEN+1]; /* Sub-system name to use */
	char banner[IMS_COL255_LEN+1];   /* banner character string */
	int bannerOption;                /* banner option */
	int stderrFlag;                  /* standard error flag */
	int queueFlag;                   /* queue flag */
	int severityFlag;                /* severity inclusion flag */
	int logFileFlag;                 /* log file flag */
	char *logFileName;               /* log file name */
	int logFileDesc;                 /* log file descriptor */
	FILE *logFilePtr;                /* log file stream pointer */
	int logFDOpen;                   /* log file desc openned by msg flag */
	int logFPOpen;                   /* log file ptr openned by msg flag */
	int timeStampFlag;               /* time stamp flag */
	int addInfoFlag;                 /* additional information flag */
	int sybMsgLevel;                 /* sybase msg supress level */
	int lastSybErrSeverity;          /* last sybase err(dblib) severity */
	int lastSybErrNo;                /* last sybase err(dblib) number */
	int lastSybMsgSeverity;          /* last sybase msg(server) severity */
	int lastSybMsgNo;                /* last sybase msg(server) number */
	int sybErrHndlFlag;              /* sybase error hanlder flag */
	int sybMsgHndlFlag;              /* sybase message hanlder flag */
	int sigEventCounter;             /* significant event flag */
	int syslogFlag;                  /* syslog flag */
	int syslogSeverity;              /* severity level written to syslog */
	char programName[IMS_COL30_LEN+1]; /* program name */
	IMS_MSG_QUEUE *first;            /* pointer to first queued msg */
	IMS_MSG_QUEUE *last;             /* pointer to last queued msg */
} IMS_MSG_STRUCT;

/*
** Function definitions.
*/
IMS_MSG_STRUCT *ims_msgStructAlloc (void);
int ims_msgStructInit (IMS_MSG_STRUCT *);
int ims_msgStructFree (IMS_MSG_STRUCT *);
int ims_msgQueueFree (IMS_MSG_QUEUE *);
int ims_msgFree (IMS_MSG_STRUCT *);
int ims_msg (IMS_MSG_STRUCT *, int, char *, ...);
IMS_MSG_QUEUE *ims_msgQueueExtract (IMS_MSG_STRUCT *);
int ims_msgStderrFlag (IMS_MSG_STRUCT *, int);
int ims_msgQueueFlag (IMS_MSG_STRUCT *, int);
int ims_msgSeverityFlag (IMS_MSG_STRUCT *, int);
int ims_msgSeverity (IMS_MSG_STRUCT *, int);
int ims_msgLogFileName (IMS_MSG_STRUCT *, char *);
int ims_msgLogFileDesc (IMS_MSG_STRUCT *, int);
int ims_msgLogFilePtr (IMS_MSG_STRUCT *, FILE *);
int ims_msgBanner (IMS_MSG_STRUCT *, char *, int);
int ims_msgSubSystem (IMS_MSG_STRUCT *, char *);
int ims_msgProgramName (IMS_MSG_STRUCT *, char *);
int ims_msgTimeFlag (IMS_MSG_STRUCT *, int);
int ims_msgAddInfoFlag (IMS_MSG_STRUCT *, int);
int ims_msgInitSybMsg (IMS_MSG_STRUCT *);
int ims_msgSybLevel (IMS_MSG_STRUCT *, int);
int ims_msgSybErrHndlFlag (IMS_MSG_STRUCT *, int);
int ims_msgSybMsgHndlFlag (IMS_MSG_STRUCT *, int);
int ims_msgGetSeverity (IMS_MSG_STRUCT *);
int ims_msgGetSybLevel (IMS_MSG_STRUCT *);
int ims_msgGetSybErrNo (IMS_MSG_STRUCT *);
int ims_msgGetSybErrSeverity (IMS_MSG_STRUCT *);
int ims_msgGetSybMsgNo (IMS_MSG_STRUCT *);
int ims_msgGetSybMsgSeverity (IMS_MSG_STRUCT *);
int ims_msgOpenSyslog (IMS_MSG_STRUCT *, char *, int);
int ims_msgCloseSyslog (IMS_MSG_STRUCT *);
int ims_msgSetSyslogSeverity (IMS_MSG_STRUCT *, int);
int ims_msgIKSyslog (IMS_MSG_STRUCT *, char *);

#endif   /* _IMS_MSG_H */
