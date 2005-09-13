/****************************************************************
FUNCTION NAME: Alert

SYNTAX: 
       void Alert(char *c, ...)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    fmt         char *          pointer to error message

DESCRIPTION:
   Print a warning message to stderr. Alert uses a variable length argument
   list so that variable messages may be passed. This is ANSI C dependent. 

RETURN VALUE:
   none.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - Mike Shindle - Original Development.
****************************************************************/

#include "asf.h"
#include <stdarg.h>

void Alert(char *fmt, ...) {
   va_list args;

   va_start(args, fmt);
   fprintf(stderr,"\nWarning:\n");
   vfprintf(stderr,fmt,args);
   fprintf(stderr,"\n");
   return;
}
