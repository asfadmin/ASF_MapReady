/************************************
Log file utilities
************************************/

#include "log.h"

FILE *fLog;
int logflag;		/* flag for log file output */
char logFile[256];	/* log file name */
int quietflag;		/* flag for short or long output */
char logbuf[4096];	/* buffer for log file output */

void printLog(char *msg)
{
  fprintf(fLog, "%s", msg);
}
