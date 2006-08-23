#include "asf.h"

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
}
