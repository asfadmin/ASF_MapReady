/******************************************************************************
NAME:
 print_alerts.c

DESCRIPTION:
 Wrappers for consistant reporting to the terminal & log file
******************************************************************************/
#include "asf.h"

report_level_t g_report_level=WARNING;

static void check_stop()
{
    snprintf(logbuf, sizeof(logbuf), "%s/stop.txt", get_asf_tmp_dir());
    if (fileExists(logbuf)) {
        remove(logbuf);
        asfPrintError("Interrupted by user.\n");
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
  va_list ap;
  if (!quietflag) {
    va_start(ap, format);
    vprintf(format, ap);
    va_end(ap);
  
    fflush (stdout);
  }
  if (logflag) {
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);

    fflush (fLog);
  }

  check_stop();
}

/* Basically a printf that pays attention to log flag but NOT the quiet flag */
void asfForcePrintStatus(const char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  vprintf(format, ap);
  va_end(ap);

  if (logflag) {
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);

    fflush (fLog);
  }
}

/* Report warning to user & log file, then continue the program  */
void asfPrintWarning(const char *format, ...)
{
  va_list ap;
  const char *warningBegin = "\n** Warning: ********\n";
  const char *warningEnd = "** End of warning **\n\n";

  printf("%s", warningBegin);
  if (logflag) {
    fprintf(fLog, "%s", warningBegin);
  }

  va_start(ap, format);
  vprintf(format, ap);
  va_end(ap);

  if (logflag) {
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);
  }

  printf(warningEnd);
  fflush (stdout);
  if (logflag) {
    fprintf(fLog, "%s", warningEnd);
    fflush (fLog);
  }
}


/* Report to user & logfile, then die  */
void asfPrintError(const char *format, ...)
{
  va_list ap;
  const char *errorBegin = "\n** Error: ********\n";
  const char *errorEnd = "** End of error **\n\n";

  printf("%s", errorBegin);
  if (logflag) {
    fprintf(fLog, "%s", errorBegin);
  }

  va_start(ap, format);
  vprintf(format, ap);
  va_end(ap);

  if (logflag) {
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);
  }

  printf(errorEnd);

  if (logflag) {
    fprintf(fLog, "%s", errorEnd);
    FCLOSE(fLog);
  }

  update_status("Error");
  clear_status_file();

  // Note that we don't have to worry about flushing output in this
  // function since its fatal and the operating system will therefore
  // presumably do it for us.

  exit(EXIT_FAILURE);
}

void asfPrintErrorMaybe(const char *format, ...)
{
    // ignoring errors?
    if (g_report_level == NOREPORT) return;

    // normal asfPrintError
    char buffer[4096];
    va_list ap;
    va_start(ap, format);
    vsprintf(buffer, format, ap);
    va_end(ap);

    asfPrintError(buffer);
}

// Report with the appriate level
void asfReport(report_level_t level, const char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  vsprintf(logbuf, format, ap);
  va_end(ap);

  if (level == LOG)
    asf_print_to_log_only("%s", logbuf);
  else if (level == STATUS)
    asfPrintStatus("%s", logbuf);
  else if (level == WARNING)
    asfPrintWarning("%s", logbuf);
  else if (level == ERROR)
    asfPrintError("%s", logbuf);
}

/******************************************************************************
 * Report the number of lines processed out of the total number of lines */
void asfLineMeter(int currentLine, int totalLines)
{
  char *null="", *newline="\n", *endline;
  char *present="ing", *past="ed ", *tense;
  int blather;

  /* Since C is 0 indexed and totalLines is 1 indexed, add 1 to currentLine*/
  currentLine++;

  /* Flag to report every 128 lines */
  blather = currentLine%128==0 || currentLine==1 || currentLine==totalLines;

  /* Leave if we're not going to blather at the user (or log) */
  if (!blather) return;

  /* Concoct status message */
  endline = (currentLine!=totalLines) ? null : newline;
  tense   = (currentLine!=totalLines) ? present : past;
  sprintf(logbuf,"Process%s %5d of %5d lines.%s",
          tense, currentLine, totalLines, endline);

  /* Report to terminal */
  if (!quietflag) {
    printf("%c%s",'\r',logbuf);
    fflush(NULL);
  }

  /* Report to the log as well */
  /* Only on the last line */
  if (logflag && currentLine==totalLines) {
    sprintf(logbuf,"%s%c",logbuf,'\n');
    printLog(logbuf);
  }

  check_stop();
}

/******************************************************************************
 * Print the percent thats been completed to stdout */
void asfPercentMeter(double inPercent)
{
  char *null="", *newline="\n", *endline;
  static int oldPercent=-1;
  int newPercent;
  int blather;

  /* Get inPercent to integer form */
  newPercent = (int)(inPercent * 100.0);

  /* Flag to report every 1% */
  blather = newPercent-oldPercent == 1;
  oldPercent = newPercent;

  /* Quit now if we're not going to blather at the user */
  if (!blather) return;

  /* Figure status message in correct format & tense */
  endline = (newPercent!=100) ? null : newline;
  sprintf(logbuf,"Processed %3d%%%s", newPercent, endline);

  /* Report to terminal */
  if (!quietflag) {
    printf("%c%s",'\r',logbuf);
    fflush(NULL);
    inPercent++;
  }
  /* Report to the log as well */
  /* Only on the last line */
  if (logflag && newPercent==100) {
    sprintf(logbuf,"%s%c",logbuf,'\n');
    printLog(logbuf);
  }

  check_stop();
}
