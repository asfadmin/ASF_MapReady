/************************************
Log file utilities
************************************/

#include "log.h"

FILE *fLog;         /* log file stream pointer */
char logFile[256];  /* log file name */
char logbuf[4096];  /* buffer for log file output */
int logflag;        /* flag for log file output */
int quietflag;      /* flag for short or long output */

void printLog(char *msg)
{
  fprintf(fLog, "%s", msg);
}

void printAndLog(char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  if (!quietflag)
    vprintf(format, ap);
  if (logflag)
    vfprintf(fLog, format, ap);
  va_end(ap);
}
