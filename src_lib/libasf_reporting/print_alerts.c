/******************************************************************************
NAME:
 print_alerts.c

DESCRIPTION:
 Wrappers for consistant reporting to the terminal & log file
******************************************************************************/
#include "asf.h"
#include "log.h"


/* Do not print to the terminal, only report to the log file */
void asf_print_to_log_only(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  if (logflag)
    vfprintf(fLog, format, ap);
  va_end(ap);
}

/* Basically a printf that pays attention to quiet & log flags */
void asfPrintStatus(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  if (!quietflag)
    vprintf(format, ap);
  if (logflag)
    vfprintf(fLog, format, ap);
  va_end(ap);
}

/* Basically a printf that pays attention to log flag but NOT the quiet flag */
void asfForcePrintStatus(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  vprintf(format, ap);
  if (logflag)
    vfprintf(fLog, format, ap);
  va_end(ap);
}

/* Report warning to user & log file, then continue the program  */
void asfPrintWarning(const char *format, ...)
{
  va_list ap;
  char warningBegin[64];
  char warningEnd[64];

  sprintf(warningBegin,"\n** Warning: ********\n");
  sprintf(warningEnd,"** End of warning **\n\n");

  printf(warningBegin);
  if (logflag) fprintf(fLog, warningBegin);

  va_start(ap, format);
  vprintf(format, ap);
  if (logflag)
    vfprintf(fLog, format, ap);
  va_end(ap);

  printf(warningEnd);
  if (logflag) fprintf(fLog, warningEnd);
}


/* Report to user & logfile, then die  */
void asfPrintError(const char *format, ...)
{
  va_list ap;
  char errorBegin[64];
  char errorEnd[64];

  sprintf(errorBegin,"\n** Error: ********\n");
  sprintf(errorEnd,"** End of error **\n\n");

  printf(errorBegin);
  if (logflag) fprintf(fLog, errorBegin);

  va_start(ap, format);
  vprintf(format, ap);
  if (logflag)
    vfprintf(fLog, format, ap);
  va_end(ap);

  printf(errorEnd);
  if (logflag) fprintf(fLog, errorEnd);

  FCLOSE(fLog);
  exit(EXIT_FAILURE);
}
