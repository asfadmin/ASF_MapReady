/************************************
Log file utilities
************************************/

#include "log.h"

FILE *fLog;          /* log file stream pointer */
char logFile[256];   /* log file name */
char logbuf[4096];   /* buffer for log file output */
int logflag=FALSE;   /* flag for log file output */
int quietflag=FALSE; /* flag for short or long output */

void printLog(char *msg)
{
  fprintf(fLog, "%s", msg);
}
