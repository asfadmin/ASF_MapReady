/************************************
Log file utilities
************************************/
#include "asf.h"
#include "log.h"

FILE *fLog;          /* log file stream pointer */
char logFile[256];   /* log file name */
char logbuf[4096];   /* buffer for log file output */
int logflag=FALSE;   /* flag for log file output */
int quietflag=FALSE; /* flag for short or long output */

void printLog(const char *msg)
{
  fprintf(fLog, "%s", msg);
}

/* Stuff related to the "status" file -- used by the GUI to update the
   "status" column during processing. */

char *g_status_file=NULL;   /* name of a "status" file, if being used */

void set_status_file(const char *status_file)
{
  if (status_file && strlen(status_file) > 0)
    g_status_file = STRDUP(status_file);
  else
    clear_status_file();
}

void update_status(const char *format, ...)
{
  if (g_status_file && strlen(g_status_file) > 0) {
    FILE *fStat = fopen(g_status_file, "w");
    if (fStat) {
      va_list ap;
      va_start(ap, format);
      vfprintf(fStat, format, ap);
      va_end(ap);
      fclose(fStat);
    }
  }
}

void clear_status_file()
{
  if (g_status_file) {
    FREE(g_status_file);
    g_status_file = NULL;
  }
}
