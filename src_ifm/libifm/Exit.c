/****************************************************************
FUNCTION NAME: Exit

SYNTAX: 
       void Exit(char *fmt, ...)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    fmt           char *          pointer to error message

DESCRIPTION:
   Exit from program while printing an error message. Exit() allows multiple
   parameters in order to print error message with variables values.

RETURN VALUE:
   none.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - Mike Shindle - original devlopment.
****************************************************************/
#include "asf.h"

#include <stdarg.h>

void Exit(char *fmt, ...) {
   va_list args;

   va_start(args,fmt);
   fprintf(stderr,"\nExit function:\n");
   vfprintf(stderr,fmt,args);
   fprintf(stderr,"\n\n");
   exit(1);
}
