/* All the goodies for managing log files */
#ifndef __LOGFILE_H
#define __LOGFILE_H

#include <stdio.h>

extern FILE *fLog;		/* file pointer for log file */
extern int logflag;		/* flag for log file output */
extern char logFile[255];	/* log file name */
extern int quietflag;		/* flag for short or long output */
extern char logbuf[255];	/* buffer for log file output */

void printLog(char *msg);

#endif
