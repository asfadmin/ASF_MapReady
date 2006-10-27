/******************************************************************************
NAME:
 print_alerts.c

DESCRIPTION:
 Wrappers for consistant reporting to the terminal & log file
******************************************************************************/
#include "asf.h"

static const int SEVERITY_DEBUG = 0;
static const int SEVERITY_STATUS = 1;
static const int SEVERITY_INFO = 2;
static const int SEVERITY_WARNING = 3;
static const int SEVERITY_ERROR = 4;

typedef void callback_t(int severity, const char *str);
static callback_t *callback_fn = NULL;
void register_asfPrint_callback(callback_t c)
{
    callback_fn = c;
}

static void asfPrintImpl(int severity, const char *format, ...)
{
  char buf[1280];

  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);

  if (severity > SEVERITY_STATUS || !quietflag) {
    printf("%s", buf);
    fflush (stdout);
  }

  if (logflag) {
    fprintf(fLog, "%s", buf);
    fflush (fLog);
  }

  if (callback_fn) {
    callback_fn(severity, buf);
  }
}

/* Do not print to the terminal, only report to the log file */
void asf_print_to_log_only(const char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  if (logflag) {
    vfprintf(fLog, format, ap);
    fflush (fLog);
  }
  va_end(ap);
}

/* Basically a printf that pays attention to quiet & log flags */
void asfPrintStatus(const char *format, ...)
{
  char buf[1024];

  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);

  asfPrintImpl(SEVERITY_STATUS, "%s", buf);
}

/* Basically a printf that pays attention to log flag but NOT the quiet flag */
void asfForcePrintStatus(const char *format, ...)
{
  char buf[1024];

  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);

  asfPrintImpl(SEVERITY_INFO, "%s", buf);
}

/* Report warning to user & log file, then continue the program  */
void asfPrintWarning(const char *format, ...)
{
  char buf[1024];

  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);

  static const char *warningBegin = "\n** Warning: ********\n";
  static const char *warningEnd = "** End of warning **\n\n";

  asfPrintImpl(SEVERITY_WARNING, "%s%s%s", warningBegin, buf, warningEnd);
}


/* Report to user & logfile, then die  */
void asfPrintError(const char *format, ...)
{
  char buf[1024];

  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof(buf), format, ap);
  va_end(ap);

  static const char *errorBegin = "\n** Error: ********\n";
  static const char *errorEnd = "** End of error **\n\n";

  asfPrintImpl(SEVERITY_ERROR, "%s%s%s", errorBegin, buf, errorEnd);

  if (logflag) {
    FCLOSE(fLog);
  }

  // Note that we don't have to worry about flushing output in this
  // function since its fatal and the operating system will therefore
  // presumably do it for us.

  exit(EXIT_FAILURE);
}
