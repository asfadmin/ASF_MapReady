/*
assert type functions for reporting to programmer with a little bit of
niftiness to make it possible to explain things to the user
*/

#include <stdlib.h>

#include "asf.h"


void require_function (const char *file, int line, int condition,
                       const char *conditionString, const char *formatString,
		       ...)
{
  if ( !condition ) {
    va_list ap;       /* Variadic arguments pointer.  */
    char temp[4096];  /* Combination of condition string & formatString */

    sprintf (temp, "%s:%d: failed assertion `%s'\n%s",
             file, line, conditionString, formatString);
    asfPrintError (temp, ap);
  }
}

void die_function (const char *file, int line, const char *formatString, ...)
{
  va_list ap;       /* Variadic arguments pointer.  */
  char temp[4096];  /* Combination of condition string & message */

  sprintf (temp, "At source file %s line %d: %s", file, line, formatString);
  asfPrintError (temp, ap);
  /* Reassure compiler that this noreturn fctn doesn't return (without
     having to declare asfPrintError to be `noreturn'.  */
  exit (EXIT_FAILURE); 
}
