#include "asf.h"
#include "asf_endian.h"
#include "asf_import.h"
#include "asf_reporting.h"
#include <ctype.h>
#include <string.h>

#ifdef linux
char *strdup(char *);
#endif

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

/* Check to see if an option with (or without) an argument
   was supplied.  If it was found, return its argument
   number.  otherwise return FLAG_NOT_SET.

   The argument is assumed to be of the form "<key>[=value]"
   The [=value] part is optional. */
int checkForOptionWithArg(char *key, int argc, char *argv[])
{
  int ii = 0;
  while (ii < argc)
  {
    /* make a copy of the arg, and remove anything past the "=" */
    char *arg = strdup(argv[ii]);
    char *eq = strchr(arg, '=');
    if (eq) *eq = '\0';

    /* now check for a match in the usual way */
    int match = strmatch(key, arg);    
    free(arg);
    if (match) {
      return ii;
    }
    ++ii;
  }
  return FLAG_NOT_SET;
}

double getDoubleOptionArgWithDefault(char *arg, double def)
{
  double val = def;
  char *arg_cpy = strdup(arg);
  char *eq = strchr(arg_cpy, '=');
  if (eq) {
    ++eq;
    char *endptr;
    double d = strtod(eq, &endptr);
    if (endptr != eq) val = d;
  }
  free(arg_cpy);

  return val;
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
