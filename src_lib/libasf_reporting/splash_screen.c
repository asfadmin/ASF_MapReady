#include "asf.h"
#include "log.h"
#include "asf_reporting.h"

/* Default splash screen
   This function should be called as soon as the log & quiet flags have been
   set the way the user wants them */
void asfSplashScreen(int argc, char* argv[])
{
  int ii;

  sprintf(logbuf, "\nCommand line:\n");
  for (ii = 0; ii < argc; ii++) {
    sprintf(logbuf, "%s %s",logbuf, argv[ii]);
  }
  asfPrintStatus("%s\n"
                "\n"
                "Date: %s\n"
                "PID:  %i\n"
                "\n",
                logbuf, date_time_stamp(), (int)getpid());
}

