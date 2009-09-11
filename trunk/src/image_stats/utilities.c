#include "asf.h"
#include "asf_endian.h"
#include "image_stats.h"
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
  char temp1[255];
  char temp2[255];
  int ii;
  sprintf(temp1, "\nCommand line:");
  for (ii = 0; ii < argc; ii++)
  {
    sprintf(temp2, " %s",argv[ii]);
    strcat(temp1, temp2);
  }
  printf("%s\n", temp1);
  printf("Date: %s\n",date_time_stamp());
  printf("PID: %i\n\n", (int)getpid());
}


void print_progress(int current_line, int total_lines)
{
  current_line++;

  if ((current_line%256==0) || (current_line==total_lines)) {
    printf("\rWrote %5d of %5d lines.", current_line, total_lines);
    fflush(NULL);
    if (current_line == total_lines) {
      printf("\n");
      if (logflag) {
        sprintf(logbuf,"Wrote %5d of %5d lines.\n", current_line, total_lines);
        printLog(logbuf);
      }
    }
  }
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

/* Print an error message. This is just here for circumventing check_return.
   Also, it makes it possible to reformat all the error messages at once. */
void print_error(char *msg)
{
  char tmp[256];
  /* I made "ERROR:" red...Yay! :D */
  printf("\n   \033[31;1mERROR:\033[0m %s\n\n", msg);
  sprintf(tmp, "\n   ERROR: %s\n\n", msg);
  printLog(tmp);
  exit(EXIT_FAILURE);
}

/* Check the return value of a function and display an error message if it's a bad return */
void check_return(int ret, char *msg)
{
  if (ret != 0)
    print_error(msg);
}

void pixel_type_flag_looker(int *flag_count, char *flags_used, char *flagName)
{
  if (*flag_count==0)
    strcat(flags_used, flagName);
  else if (*flag_count==1)
    strcat(strcat(flags_used, " and "), flagName);
  else if (*flag_count>1)
    strcat(strcat(flags_used, ", and "), flagName);
  else {
    char msg[256];
    sprintf(msg, "Programmer error dealing with the %s flag.", flagName);
    print_error(msg);
  }
  (*flag_count)++;
}

