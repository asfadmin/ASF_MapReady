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


#endif
