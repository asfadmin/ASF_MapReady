/****************************************************************
FUNCTION NAME: nrerror

SYNTAX: 
       void nrerror(char *fmt, ...)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    fmt           char *          pointer to error message

DESCRIPTION:
   Exit from program while printing an error message. nrerror() allows 
   multiple parameters in order to print error message with variables 
   values. This program was written in order to provide better error support
   for those functions used in Numerical recipies for C.

RETURN VALUE:
   none.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - Mike Shindle - original devlopment.
****************************************************************/

#include "asf.h"

#include <stdarg.h>

void nrerror(char *fmt, ...) {
   va_list args;

   va_start(args,fmt);
   fprintf(stderr,"\nnrerror function: run-time error...\n");
   vfprintf(stderr,fmt,args);
   fprintf(stderr,"\n\n");
   exit(1);
}
