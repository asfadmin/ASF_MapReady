#include "asf.h"

/* this is over in license.c */
const char *version_string(const char *program_name);

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
  char *date_time = date_time_stamp ();
  asfPrintStatus("%s\n"
                "\n"
                "Date: %s\n"
                "PID:  %i\n"
                "Version: %s\n"
                "\n",
                 logbuf, date_time, (int)getpid(),
                 version_string(argv[0]));
}
