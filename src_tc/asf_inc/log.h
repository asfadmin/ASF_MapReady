/* All the goodies for managing log files */
#ifndef __LOGFILE_H
#define __LOGFILE_H

#include <stdio.h>

FILE *fLog;		/* file pointer for log file */
int logflag;		/* flag for log file output */
char logFile[255];	/* log file name */
int quietflag;		/* flag for short or long output */
char logbuf[255];	/* buffer for log file output */

void printLog(char *msg);

#endif
