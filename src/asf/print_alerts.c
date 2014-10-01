/******************************************************************************
NAME:
 print_alerts.c

DESCRIPTION:
 Wrappers for consistant reporting to the terminal & log file
******************************************************************************/
#include "asf.h"
#include <sys/time.h>

report_level_t g_report_level=REPORT_LEVEL_WARNING;

static void check_stop()
{
    snprintf(logbuf, sizeof(logbuf), "%s/stop.txt", get_asf_tmp_dir());
    if (fileExists(logbuf)) {
        remove(logbuf);
        asfPrintError("Interrupted by user.\n");
    }
}

/* Do not print to the terminal, only report to the log file */
void asfPrintToLogOnly(const char *format, ...)
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
    if (!fLog) {
      logflag = FALSE;
      asfPrintWarning("Error writing to log file: invalid file pointer\n");
    }
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
    if (!fLog) {
      logflag = FALSE;
      asfPrintWarning("Error writing to log file: invalid file pointer\n");
    }
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);

    fflush (fLog);
  }
}

/* Report warning to user & log file, then continue the program  */
void asfPrintWarning(const char *format, ...)
{
  const char *warningBegin = "\n** Warning: ********\n";
  const char *warningEnd = "** End of warning **\n\n";

  if (logflag && !fLog) {
    // re-entry here, but it's ok since we change the flag...
    logflag = FALSE;
    asfPrintWarning("Error writing to log file: invalid file pointer\n");
  }

  va_list ap;

  if (quietflag < 2)
    printf("%s", warningBegin);
  if (logflag)
    fprintf(fLog, "%s", warningBegin);

  if (quietflag < 2) {
    va_start(ap, format);
    vprintf(format, ap);
    va_end(ap);
  }
  if (logflag) {
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);
  }
  
  if (quietflag < 2) {
    printf("%s", warningEnd);
    fflush (stdout);
  }
  if (logflag) {
    fprintf(fLog, "%s", warningEnd);
    fflush (fLog);
  }
}


/* Report to user & logfile, then die  */
void asfPrintError(const char *format, ...)
{
  const char *errorBegin = "\n** Error: ********\n";
  const char *errorEnd = "** End of error **\n\n";

  if (logflag && !fLog) {
    logflag = FALSE;
    asfPrintWarning("Error writing to log file: invalid file pointer\n");
  }

  va_list ap;

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

  printf("%s", errorEnd);

  if (logflag) {
    fprintf(fLog, "%s", errorEnd);
    if (fLog) FCLOSE(fLog);
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
    if (g_report_level == REPORT_LEVEL_NONE) return;

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

  if (level == REPORT_LEVEL_LOG)
    asfPrintToLogOnly("%s", logbuf);
  else if (level == REPORT_LEVEL_STATUS)
    asfPrintStatus("%s", logbuf);
  else if (level == REPORT_LEVEL_WARNING)
    asfPrintWarning("%s", logbuf);
  else if (level == REPORT_LEVEL_ERROR)
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
    strcat(logbuf, "\n");
    printLog(logbuf);
  }

  /* Check if we should abort every once in a while */
  if (currentLine%640==0 || currentLine==totalLines)
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

  /* Flag to report every 1% or more */
  blather = newPercent-oldPercent >= 1;
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
    strcat(logbuf, "\n");
    printLog(logbuf);
  }

  /* Check if we should abort, 10 times during processing */
  if (newPercent%10==0)
    check_stop();
}

// Cute watch dog indicator ...just something on the screen
// (looks like a rotating bar) to let the user know something
// is processing.  WARNING: Unfortunately there are 2 issues:
// a) the cursor-hiding escape sequence (esc[?25l) won't work
// if the user is using a strange terminal (almost all linux
// terminals work), and b) gettimeofday() is slow ...but I 
// don't know of another way to get fractional seconds of time.
// Processing with the watch dog indicator running does take
// longer than without.
//
static struct timeval last_time = {0,0};
static int watch_dog_idx = 0;
void asfRunWatchDog(double delay)
{
  char ESC = 27;
  unsigned char flipper[4] = {'|', '/', '-', '\\'};
  struct timeval new_time;
  double start, stop;

  gettimeofday(&new_time, 0x0);
  start = last_time.tv_sec + last_time.tv_usec/1000000.0;
  stop = new_time.tv_sec + new_time.tv_usec/1000000.0;

  if (stop - start > delay) {
    printf("%c[?25l%c%c", ESC, '\r', flipper[watch_dog_idx]);
    fflush(0x0);
    watch_dog_idx = (watch_dog_idx + 1) % 4; 
    last_time = new_time;
  }
}

void asfStopWatchDog()
{
  char ESC = 27;
  printf("%c %c[?25h\n", '\r', ESC);
}



