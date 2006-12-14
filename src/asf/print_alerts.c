/******************************************************************************
NAME:
 print_alerts.c

DESCRIPTION:
 Wrappers for consistant reporting to the terminal & log file
******************************************************************************/
#include "asf.h"

static void check_stop()
{
    char stop_file[255];
    sprintf(stop_file, "%s/stop.txt", get_asf_tmp_dir());
    if (fileExists(stop_file)) {
        remove(stop_file);
        asfPrintError("Processing stopped by user!\n");
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
  char warningBegin[64];
  char warningEnd[64];

  sprintf(warningBegin,"\n** Warning: ********\n");
  sprintf(warningEnd,"** End of warning **\n\n");

  printf(warningBegin);
  if (logflag) {
    fprintf(fLog, warningBegin);
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
    fprintf(fLog, warningEnd);
    fflush (fLog);
  }
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
  va_end(ap);

  if (logflag) {
    va_start(ap, format);
    vfprintf(fLog, format, ap);
    va_end(ap);
  }

  printf(errorEnd);

  if (logflag) {
    fprintf(fLog, errorEnd);
    FCLOSE(fLog);
  }

  // Note that we don't have to worry about flushing output in this
  // function since its fatal and the operating system will therefore
  // presumably do it for us.

  exit(EXIT_FAILURE);
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
  blather = ((currentLine%128==0)||(currentLine==1)||(currentLine==totalLines))
            ? TRUE : FALSE;

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
  blather = (newPercent-oldPercent == 1) ? TRUE : FALSE;
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
