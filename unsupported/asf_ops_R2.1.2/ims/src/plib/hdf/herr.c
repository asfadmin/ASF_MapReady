/***************************************************************************
*
*
*                         NCSA HDF version 3.3r1
*                            September 20, 1993
*
* NCSA HDF Version 3.3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
*****************************************************************************/

/*+ herr.c
*** error routines
+*/

#define _H_ERR_MASTER_

#include "hdf.h"
#include "herr.h"

/*
** Include files for variable argument processing for HEreport
*/
#if defined PROTOTYPE
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* We use a stack to hold the errors plus we keep track of the function,
   file and line where the error occurs.*/

/* the strcuture of the error stack element */

typedef struct error_t {
    int16 error_code;          /* Error number */
    char *function_name;       /* function where error occur */
    char *file_name;           /* file where error occur */
    intn line;                 /* line in file where error occurs */
    intn system;               /* bool for system or HDF error */
    char *desc;                /* optional supplied description */
} error_t;

/* error_stack is the error stack.  error_top is the stack top pointer, and points to
   the next available slot on the stack */

#ifndef ERR_STACK_SZ
#   define ERR_STACK_SZ 10
#endif

/* max size of a stored error description */
#ifndef ERR_STRING_SIZE
#   define ERR_STRING_SIZE 512
#endif

/* pointer to the structure to hold error messages */
PRIVATE error_t *error_stack = NULL;


/* always points to the next available slot; the last error record is in slot (top-1) */
int32 error_top = 0;

#ifndef DEFAULT_MESG
#   define DEFAULT_MESG "Unknown error"
#endif

/* size of error message table */

#define ERRMESG_SZ (sizeof(error_messages) / sizeof(error_messages[0]))

/*- HEstring
*** returns the error message associated with error_code, uses a
*** linear search but efficiency should not be a problem here
-*/
#ifdef PROTOTYPE
char _HUGE *HEstring(int16 error_code)
#else
char _HUGE *HEstring(error_code)
    int16 error_code;
#endif
{
    int i;                     /* temp int index */

    /* look for the error_code in error message table */

    for (i = 0; i < ERRMESG_SZ; i++)
       if (error_messages[i].error_code == error_code)
           return error_messages[i].str;

    /* otherwise, return default message */

    return DEFAULT_MESG;
}

/*- HEclear
*** clears the error stack
-*/
#ifdef PROTOTYPE
VOID HEclear(void)
#else
VOID HEclear()
#endif
{

    if(!error_top) return;

    /* error_top == 0 means no error in stack */    
    /* clean out old descriptions if they exist */
    for (error_top; error_top; error_top--) {
        if(error_stack[error_top - 1].desc) {
            HDfreespace(error_stack[error_top - 1].desc);
            error_stack[error_top - 1].desc = NULL;
        }
    }
}

/*- HEpush
*** push a new error onto stack.  If stack is full, error is ignored.
*** assumes that the character strings (function_name and file_name) referred
***  are in some semi-permanent storage, so it just saves the pointer to
***  the strings.
*** blank out the description field so that a description is reported
***  only if REreport is called
***
-*/
#ifdef PROTOTYPE
VOID HEpush(int16 error_code, char *function_name, char *file_name, int line)
#else
VOID HEpush(error_code, function_name, file_name, line)
    int16  error_code;           /* internal number of the error */
    char   *function_name;       /* name of function that error occurred */
    char   *file_name;           /* name of file that error occurred */
    int    line;                 /* line number in file that error occurred */
#endif
{
    int i;

    /* if the stack is not allocated, then do it */

    if (!error_stack) {
       error_stack =(error_t *)HDgetspace((uint32)sizeof(error_t)*ERR_STACK_SZ);
       if (!error_stack) {
           puts("HEpush cannot allocate space.  Unable to continue!!");
           exit(8);
       }
       for(i = 0; i < ERR_STACK_SZ; i++)
        error_stack[i].desc = NULL;
    }

    /* if stack is full, discard error */
    /* otherwise, push error details onto stack */

    if (error_top < ERR_STACK_SZ)  {
       error_stack[error_top].function_name = function_name;
       error_stack[error_top].file_name = file_name;
       error_stack[error_top].line = line;
       error_stack[error_top].error_code = error_code;
       if(error_stack[error_top].desc) {
           HDfreespace(error_stack[error_top].desc);
           error_stack[error_top].desc = NULL;
       }
       error_top++;
    }
}


/* ====================================================================== */
/* Write a nicely formatted string to a log file */
#if defined PROTOTYPE
VOID HEreport(char *format, ...) {
  va_list arg_ptr;
  char *tmp;
  char *FUNC="HEreport";   /* name of function if HIalloc fails */

  va_start(arg_ptr, format);

  if((error_top < ERR_STACK_SZ+1) && (error_top > 0)){
    tmp = (char *) HDgetspace(ERR_STRING_SIZE);
    if (!tmp) {
      HERROR(DFE_NOSPACE);
      return;
    }
    vsprintf(tmp, format, arg_ptr);
    if(error_stack[error_top - 1].desc)
        HDfreespace(error_stack[error_top - 1].desc);
    error_stack[error_top - 1].desc = tmp;
  }
  
  va_end(arg_ptr);
  return;
}
#else
VOID HEreport(va_alist)
va_dcl
{
  char *FUNC="HEreport";   /* name of function if HIalloc fails */
  char *tmp;
  char * format;
  va_list arg_ptr;
  
  va_start(arg_ptr);

  format = va_arg(arg_ptr, char *);

  if((error_top < ERR_STACK_SZ+1) && (error_top > 0)){
    tmp = (char *) HDgetspace(ERR_STRING_SIZE);
    if (!tmp) {
      HERROR(DFE_NOSPACE);
      return;
    }


    vsprintf(tmp, format, arg_ptr);

/* can't do this w/o stdC <stdio.h>
*
* For example, on xongmao a sun4, stdio.h declares:
*
* extern char *sprintf();
*
*    count = vsprintf(tmp, format, arg_ptr);
*
*    if(count > ERR_STRING_SIZE) {
*           printf("HEreport overwrote array. %d Unsafe to continue!!", count);
*           exit(8);     
*    }
*/
    if(error_stack[error_top - 1].desc)
        HDfreespace(error_stack[error_top - 1].desc);
    error_stack[error_top - 1].desc = tmp;

  }
  
  va_end(arg_ptr);
  return;
}
#endif /* PROTOTYPE */


/*- HEprint
*** print a number of error, starting from the error_top of the stack
-*/
#ifdef PROTOTYPE
VOID HEprint(FILE *stream, int32 print_levels)
#else
VOID HEprint(stream, print_levels)
     FILE *stream;             /* stream to print to */
     int32 print_levels;         /* levels to print */
#endif
{
    if (print_levels == 0 || print_levels > error_top)
        /* print all errors */
        print_levels = error_top;
    
    /* print the errors starting from most recent */
    
    for (print_levels--; print_levels >= 0; print_levels--) {
        fprintf(stream, "HDF error: <%s>\n\tDetected in %s() [%s line %d]\n",
                HEstring(error_stack[print_levels].error_code),
                error_stack[print_levels].function_name,
                error_stack[print_levels].file_name,
                error_stack[print_levels].line);
        if(error_stack[print_levels].desc) {
            fprintf(stream, "\t%s\n", error_stack[print_levels].desc);
        }
    }
}

/*- HEvalue
*** return the nth most recent error
-*/
#ifdef PROTOTYPE
int16 HEvalue(int32 level)
#else
int16 HEvalue(level)
    int32 level;                 /* level of error code to return */
#endif
{
    if (level > 0 && level <= error_top)
       return error_stack[error_top - level].error_code;

    else return DFE_NONE;
}
