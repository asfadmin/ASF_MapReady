#ifndef _ASF_REPORTING_H_
#define _ASF_REPORTING_H_
/******************************************************************************
NAME:
 asf_reoporting.h

DESCRIPTION:
 C header with definitions & prototypes for libasf_reporting library

******************************************************************************/

#include <stdarg.h>
/* Logging should eventually be moved from the asf.a library to this library */
#include "log.h"


/* Prototypes from progress_meters.c *****************************************/
void asfLineMeter(int currentLine, int totalLines);
void asfPercentMeter(double inPercent);

/* Prototypes from print_alerts.c ********************************************/
/* Do not print to the terminal, only report to the log file */
void asfPrintToLogOnly(const char *format, ...);
/* Basically a printf that pays attention to quiet & log flags */
void asfPrintStatus(const char *format, ...);
/* Basically a printf that pays attention to log flag but NOT the quiet flag */
void asfForcePrintStatus(const char *format, ...);
/* Report warning to user & log file, then continue the program  */
void asfPrintWarning(const char *format, ...);
/* Report to user & logfile, then die  */
void asfPrintError(const char *format, ...);

/* Prototype from splash_screen.c ********************************************/
/* Print the commandline captured, date, and PID to screen & logfile */
void asfSplashScreen(int argc, char **argv);

/* Prototypes from diagnostics.c *********************************************/
/* Given a condition, and an optional printf()-style message string (possibly
   with additional variadic arguments), this macro will print an assert style
   message and the optional message to the terminal & log file iff the
   condition is false.  */
#define asfRequire(COND,...) \
        (require_function (__FILE__, __LINE__, COND, #COND, __VA_ARGS__))
/* This routine is onlyused by the require macro, never use it elsewhere!  */
void require_function (const char *file, int line, int condition,
                       const char *condSrt, const char *message, ...)
#ifdef __GNUC__
 __attribute__ ((format (printf, 5, 6)))
#endif
; /* <--- semicolon for require_function */

/* Given a printf()-style message string and additional variadic
   arguments for the message string, in that order, this macro will
   print message to terminal, and log message prefixed with source file
   and line information to the log file. */
#define asfDie(...) (die_function (__FILE__, __LINE__, __VA_ARGS__))
/* This routine is used by the die macro.  It should never be used
   anywhere else.  */
void die_function (const char *file, int line, const char *message, ...)
#ifdef __GNUC__
 __attribute__ ((format (printf, 3, 4), noreturn))
#endif
; /* <--- semicolon for die_function */


#endif
