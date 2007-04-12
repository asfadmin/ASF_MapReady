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

/* Stuff related to the "status" file -- used by the GUI to update the
   "status" column during processing. */
void set_status_file(const char *status_file);
void update_status(const char *format, ...);
void clear_status_file(void);

#endif
