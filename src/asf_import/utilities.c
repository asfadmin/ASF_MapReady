#include "asf.h"
#include "asf_endian.h"
#include "asf_import.h"
#include "asf_reporting.h"
#include <ctype.h>

/* Helpful functions */
int firstRecordLen(char *ceosName)
{
  FILE *f;
  struct HEADER h;

  f=FOPEN(ceosName,"rb");    /*Open file.*/
  FREAD(&h,1,12,f);          /*Read first CEOS header.*/
  FCLOSE(f);                 /*Close file*/
  return bigInt32(h.recsiz); /*put recsiz in proper endian format & return*/
}


/* Default splash screen, the same for all the tools
   This function should be called first in the "we're good enough" part of command line parsing */
void print_splash_screen(int argc, char* argv[])
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


/* Check to see if an option was supplied or not
   If it was found, return its argument number
   Otherwise, return FLAG_NOT_SET */
int checkForOption(char* key, int argc, char* argv[])
{
  int ii = 0;
  while(ii < argc)
  {
    if(strmatch(key, argv[ii]))
      return(ii);
    ++ii;
  }
  return(FLAG_NOT_SET);
}

void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName)
{
  if (*flag_count==0)
    strcat(flags_used, flagName);
  else if (*flag_count==1)
    strcat(strcat(flags_used, " and "), flagName);
  else if (*flag_count>1)
    strcat(strcat(flags_used, ", and "), flagName);
  else
    asfPrintError("Programmer error dealing with the %s flag.\n", flagName);
  (*flag_count)++;
}
