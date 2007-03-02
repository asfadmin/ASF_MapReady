/* All the goodies for managing log files */
#ifndef _LOGFILE_H_
#define _LOGFILE_H_

#include <stdio.h>
#include <stdarg.h>

extern int quietflag;       /* flag for little or lots of output */
extern int logflag;         /* flag for log file output */
extern char logFile[256];   /* log file name */
extern char logbuf[4096];   /* buffer for log file output */
extern FILE *fLog;          /* file pointer for log file */

void printLog(const char *msg);

#endif
