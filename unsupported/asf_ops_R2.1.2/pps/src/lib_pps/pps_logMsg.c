/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

static char SccsFileId[] = "@(#)pps_logMsg.c	1.2  11 Apr 1996";

#include <stdio.h>
#include <syslog.h>

#include "defs.h"
#include "PPSextern.h"

#ifdef __STDC__
void
pps_logMsg(
char*			progName,	/* program name */
PPS_SYSLOG_LEVEL_E	level,		/* log priority */
char*			msg)		/* msg body */
#else
void
pps_logMsg(progName, level, msg)
char*			progName;	/* program name */
PPS_SYSLOG_LEVEL_E	level;		/* log priority */
char*			msg;		/* msg body */
#endif
{
	char logString[255];
	/* syslogd takes maximum of 1024 chars, so we adjust it to
		allow some room */
	char msgString[800];
	char printString[800];
	int  log_len;
 	int  start_print_index;
	int  print_len;
	int  len_to_print;
	int  msgType;

#ifdef DEBUG
	printf("%s\n", msg);
#endif
 
	switch(level)
	{
		case PPS_CRITICAL:
			sprintf(logString, "(%s) CRITICAL: ", progName);
			msgType = LOG_CRIT;
			break;
		case PPS_ERROR:
			sprintf(logString, "(%s) ERROR: ", progName);
			msgType = LOG_ERR;
			break;
		case PPS_WARNING:
			sprintf(logString, "(%s) WARNING: ", progName);
			msgType = LOG_WARNING;
			break;
		case PPS_INFO:
			sprintf(logString, "(%s) INFO: ", progName);
			msgType = LOG_INFO;
			break;
		case PPS_DEBUG:
			sprintf(logString, "(%s) DEBUG: ", progName);
			msgType = LOG_DEBUG;
			break;
	}/*switch*/

	log_len = strlen(logString);

	/* still have this much to send to syslog */
	len_to_print = strlen(msg) + log_len;

	/* start print msg from here */
 	start_print_index = 0;

	/* can only send to syslog this much of msg each time */
	print_len = 500 - log_len; 	

	while (len_to_print > 0)
	{
		if (len_to_print < 500) 
		{
			sprintf(msgString, "%s%s\n", logString, 
			 	msg + start_print_index);	
		}
		else
		{
			strncpy(printString,msg+start_print_index, print_len);
			printString[print_len] = '\0';
			sprintf(msgString, "%s%s\n", logString, printString); 
		}
		syslog(msgType, msgString);

		/* still have this much to send to syslog */
		len_to_print = len_to_print - print_len;

		/* start print msg from here */
		start_print_index = start_print_index + print_len;
	}

}/* pps_logMsg */
