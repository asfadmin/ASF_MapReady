#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

/* Print an error on stderr using printf style formatting, then exit.  */
void err_die(const char *error_message, ...)
{
  va_list ap;

  fprintf(stderr, "%s: ", progname);
  va_start(ap, error_message);
  vfprintf(stderr, error_message, ap);
  va_end(ap);
  
  exit(EXIT_FAILURE);
}
