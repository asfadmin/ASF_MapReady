/*
 * @(#)logUtils.c	2.7 96/04/18 10:54:49
 *
 * Utility routines for working with syslog files and messages.
 *
 * Definitions
 *     GLOBAL_asfnName		Subsystem identifier (e.g., RDS-1)
 *     GLOBAL_asfnDispName	Subsystem identifier (obsolete?)
 *     GLOBAL_identBuffer	Subsystem label for syslog messages
 *     GLOBAL_procName		Name of process as invoked (e.g., rds)
 *     syslog_tag		Process (application) label for syslog messages
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>

#ifdef	NOSTDARG
#include <varargs.h>
#else
#include <stdarg.h>
#endif	/* !NOSTDARG */

#include "logUtils.h"
#include "asf_syslog.h"

static char sccsid_logutils_c[] =
	"@(#)logUtils.c	2.7 96/04/18 10:54:49";

#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif

#ifndef MAXARGS
#define MAXARGS	100
#endif

/* GLOBAL variables for this file */

/* unused by anyone?
FILE *GLOBAL_fp;
char *GLOBAL_logFileName = NULL;
*/

char *GLOBAL_asfnName = NULL; 
char *GLOBAL_asfnDispName = NULL; 
char *GLOBAL_identBuffer; 
char *GLOBAL_procName; 

/* end GLOBAL variables for this file */

char *syslog_tag; 
void *doMalloc(int size);

/* unused by anyone? */
#define DEFAULT_FILE_NAME "ASFdefaultLOG"
#define ASF_LOG_ENV       "ASF_LOG_ENV"
#define ASF_SYSLOG       "SYSLOG"
#define LOG_EXT           ".log"
#define SYSLOG_IDENT      "" 

#ifndef	LOG_PERROR
#define	LOG_PERROR	LOG_ERR
#endif

/*
 * Name:  initASFlog
 *
 * Arguments
 *     argc		Application argument count
 *     argv		Application argument vector
 *     identString	Generic application name
 *
 *
 * Description
 *
 * initASFlog parses the command line and sets the name of the application
 * and opens the syslog files depending on the values specified on the command
 * line.  The values parsed from the command line are
 *     -asfn:		Subsystem identifier (assumed to be unique)
 *     -asflogloc:	Logging behavior.
 *			Allowed values are
 *			    SYSLOG:  Send messages to syslog() only
 *			    SYSLOG_AND_TERM:  Send messages to both syslog()
 *				and stderr
 *
 * Based on the values specified on the command line, initASFlog sets the
 * values of the following variables:
 *	GLOBAL_asfnName
 *	GLOBAL_asfnDispName
 *	GLOBAL_identBuffer
 *	GLOBAL_procName
 *	syslog_tag
 *
 * Neither -asfn nor -asflogloc is mandatory; if the application fails to
 * specify a name through identString and -asfn is omitted from the command
 * line, though, initASFlog will return an error.
 *
 * GLOBAL_procName is equivalent to the name of the application as invoked,
 * i.e., argv[0].  syslog_tag is equivalent to GLOBAL_procName with any
 * leading directories stripped off, enclosed in parentheses and followed
 * by a space.
 *
 * Diagnostics
 * initASFlog returns -1 if any of the following situations are true:
 * 1.  The -asfn or -asflogloc flag is specified without a corresponding value.
 * 2.  The specified value of -asflogloc is invalid.
 * 3.  The name of the application cannot be obtained from either the
 *     command line or the routine that called initASFlog.
 *
 * Notes
 *
 * The calling routine can specify the name of the application via the
 * identString argument, but this value will be overridden by the name
 * specified by the -asfn flag.
 *
 * Legal values to command-line arguments may not start with a hyphen ("-").
 *
 * strncasecmp(3) is considered to be part of the 4.3BSD library by SGI.
 */

#ifdef	_NO_PROTO
int initASFlog(argc, argv, identString)
int argc;
char **argv;
char* identString;

#else
int initASFlog(int argc, char **argv, char *identString)
#endif
{
    int i, len;
    LoggingType logging_type = SYSLOG;	/* Type of logging to be performed */
    char *p;				/* Scratch pointer */
    int destFlag = LOG_SPS;		/* Log to SPS syslogs by default */
    int parse_error = FALSE;		/* TRUE for bad command-line syntax */

    /*
     * Open the syslog files temporarily so that the application can write
     * error messages to it before the application's name is parsed from
     * the command line.  Write these messages to the CP syslog files,
     * where it is more likely that they will be noticed by the operator.
     */
     
    openlog(argv[0], LOG_PID|LOG_CONS|LOG_NDELAY, LOG_CP);

    /*
     * Capture the name of the process.
     */

    GLOBAL_procName = doMalloc(strlen(argv[0]) + 1);
    strcpy(GLOBAL_procName, argv[0]);

    /*
     * Set p to exclude any leading directory names from the name of the
     * process passed to syslog().
     */
    if ((p = (char *) strrchr(GLOBAL_procName, '/') ) != NULL) {
	p++;
	if (p == (char *) NULL) {
	    /*
	     * The last character of the process name was a '/' (which it
	     * shouldn't have been, but it is legal to invoke a process
	     * this way).  Set p to the entire pathname in the hope that
	     * someone will notice it in the log messages.
	     */
	    p = GLOBAL_procName;
	}
    } else
	p = GLOBAL_procName;

    len = strlen(p) + strlen("() ") + 1;
    if ((syslog_tag = doMalloc(len)) == (char *) NULL) {
	printfLLog(LOG_ERR, "initASFlog():  malloc syslog_tag");
	return -1;
    }
    sprintf(syslog_tag, "(%s) ", p);
    syslog_tag[len-1] = '\0';

    /*
     * Search for the -asfn and -asflogloc command-line options.
     * In either case, print an error message and exit if the flag is
     * present but its argument is missing or invalid.
     */

    for (i = 1; i < argc; i++) {
	if (strcmp(argv[i], "-asfn") == 0) {

	    /* Check that another argument exists on the command line */
	    if ((i + 1) == argc) {
		printfLLog(LOG_ERR,
		    "Missing argument to -asfn flag\n");
		parse_error = TRUE;
		break;
	    }

	    /* Check that the next argument isn't another flag */
	    if (*argv[++i] == '-') {
		printfLLog(LOG_ERR,
		    "Argument to -asfn flag cannot start with a \"-\"\n");
		parse_error = TRUE;
		break;
	    }

	    GLOBAL_asfnName = doMalloc(strlen(argv[i]) + 1);
	    GLOBAL_asfnDispName = doMalloc(strlen(argv[i]) + 1);
	    strcpy(GLOBAL_asfnName, argv[i]);
	    strcpy(GLOBAL_asfnDispName, argv[i]);

	} else if (strcmp(argv[i], "-asflogloc") == 0) {

	    /* Check that another argument exists on the command line */
	    if ((i + 1) == argc) {
		printfLLog(LOG_ERR,
		    "Missing argument to -asflogloc flag\n");
		parse_error = TRUE;
		break;
	    }

	    /* Check that the next argument isn't another flag */
	    if (*argv[++i] == '-') {
		printfLLog(LOG_ERR,
		    "Argument to -asflogloc flag cannot start with a \"-\"\n");
		parse_error = TRUE;
		break;
	    }

	    /* Check that the argument is one of the allowed values */
	    if (! (strcmp(argv[i], "SYSLOG") == 0 ||
		   strcmp(argv[i], "SYSLOG_AND_TERM") == 0) ) {
		printfLLog(LOG_ERR,
		    "Illegal argument %s to -asflogloc flag:\n", argv[i]);
		printfLLog(LOG_ERR,
		    "    Allowed values are SYSLOG and SYSLOG_AND_TERM\n");
		parse_error = TRUE;
		break;
	    } else {
		if (strcmp(argv[i], "SYSLOG") == 0) {
		    logging_type = SYSLOG;
		} else {
		    logging_type = SYSLOG_AND_TERM;
		}
	    }
	}
    }

    if (parse_error == TRUE) {
	return -1;
    }

    /*
     * If -asfn wasn't specified on the command line and the user didn't
     * specify the application name (through the identString argument to
     * this function), print an error message and exit.
     */
    if (GLOBAL_asfnName == NULL && identString == NULL) {
	printfLLog(LOG_ERR, "Name of application is unspecified\n");
	return -1;
    }

    /*
     * If the application's name was specified on the command line, use
     * that name to build the syslog messages.  Otherwise, use whatever
     * was passed in by the application through identString.
     */

    if (GLOBAL_asfnName != (char *) NULL) {
	GLOBAL_identBuffer = doMalloc(strlen(GLOBAL_asfnName) + 1);
	strcpy(GLOBAL_identBuffer, GLOBAL_asfnName);
    } else {
	GLOBAL_identBuffer = doMalloc(strlen(identString) + 1);
	strcpy(GLOBAL_identBuffer, identString);
    }

    /*
     * If the application is the CP, GPR, or QC application, use the CP
     * syslog files.  Otherwise, use the SPS syslog files (default).
     */
/****
    if (strncmp(GLOBAL_identBuffer, "CP", strlen("CP")) == 0 ||
	strncasecmp(GLOBAL_identBuffer, "GPR", strlen("GPR")) == 0 ||
	strncasecmp(GLOBAL_identBuffer, "QC", strlen("QC")) == 0) {
	    destFlag = LOG_CP;
    }
*****/

    if (strncmp(GLOBAL_identBuffer, "CP", strlen("CP")) == 0 ) {
       doFree(GLOBAL_identBuffer);
       GLOBAL_identBuffer = doMalloc(strlen("CP") + 1);
       strcpy(GLOBAL_identBuffer , "CP");
       destFlag = LOG_CP;
    }
    else if (strncasecmp(GLOBAL_identBuffer, "GPR", strlen("GPR")) == 0 ||
        strncasecmp(GLOBAL_identBuffer, "QC", strlen("QC")) == 0) {
            destFlag = LOG_CP;
    }

    /*
     * Close the syslog files and immediately reopen them with the new
     * application name.
     * Specify logging to be either sent or not sent to stderr depending
     * on the value of -asflogloc specified by the user.
     */

    closelog();
    if (logging_type == SYSLOG) {
	openlog(GLOBAL_identBuffer, LOG_PID|LOG_CONS|LOG_NDELAY, destFlag);
    } else { /* SYSLOG_AND_TERM */
	openlog(GLOBAL_identBuffer, LOG_PID|LOG_CONS|LOG_NDELAY|LOG_PERROR,
	    destFlag);
    }

    return 0;
}


/*----------------------------------------------------------
 * NAME:
 *  printfLog
 *
 * DESCRIPTION:
 *  provide a thread safe means of accessing a potenially
 *  thread unsage implementations of syslog as well as a 
 *  consistent interface for the creation and access of 
 *  loging information during development
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
#ifdef	NOSTDARG
int printfLog (va_alist)
va_dcl
{
    va_list args;
    int  status;
    char *bufPtr;
    char *format;

    va_start(args);
    format = va_arg(args, char*);

#else
int printfLog(char *format, ...)
{
    va_list args;
    int  status;
    char *bufPtr;

    va_start(args, format);
#endif

    if (format == (char *) NULL) {
	syslog(LOG_ERR, "printfLog:  No message specified\n");
	return -1;
    }

#if defined(__sgi) || defined(__sun)
    vsyslog(LOG_DEBUG, format, args);
#else
    /* arbitrarily allocate MAX_MESSAGE_LEN more bytes for arguments */
    bufPtr = (char *) doMalloc(strlen(format) + MAX_MESSAGE_LEN);
    status = (int) vsprintf(bufPtr, format, args);
    syslog(LOG_DEBUG, "%s", bufPtr);
    doFree(bufPtr);
#endif

    va_end(args);
    return 0;
}


/*----------------------------------------------------------
 * NAME:
 *  printfLLog
 *
 * DESCRIPTION:
 *  same as above execept that it allows the user to specify
 *  a level as the first parameter
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
#ifdef	NOSTDARG
int printfLLog(va_alist)
va_dcl
{
    va_list args;
    int status;
    int level;
    int len;
    char *bufPtr;
    char *format;
    char *newformat;

    va_start(args);
    level  = va_arg(args, int);
    format = va_arg(args, char*);

#else
int printfLLog(int level, char *format, ...)
{
    va_list args;
    int status;
    int len;
    char *bufPtr;
    char *newformat;

    va_start(args, format);
#endif /* ! NOSTDARG */

    if (syslog_tag == (char *) NULL) {
	syslog(LOG_ERR,
	    "Global syslog tag not set -- setting label to UNKNOWN\n");
	syslog_tag = "(UNKNOWN) ";
    }

    if (format == (char *) NULL) {
	syslog(LOG_ERR, "printfLLog:  No message specified\n");
	return -1;
    }

    len = strlen(syslog_tag) + strlen(format) + 1;
    newformat = doMalloc(len);
    sprintf(newformat, "%s%s", syslog_tag, format);
    newformat[len-1] = '\0';

#if defined(__sgi) || defined(__sun)
    vsyslog(level, newformat, args);
#else

    /*
     * Assuming that the remaining arguments are less than MAX_MESSAGE_LEN
     * bytes in length total is probably safe, but it is not the most
     * robust way to handle this situation.  Unfortunately, both varargs(3)
     * and stdarg(3) require the calling routine to specify the number of
     * arguments passed in.  A better method of handling the parameters
     * would be to either (1) parse the second parameter, which is nominally
     * the format string, to determine the number of parameters that follow;
     * or (2) require the calling routine to pass in NULL as the last
     * parameter.  Neither method is foolproof, though.
     */

    bufPtr = (char *) doMalloc(strlen(newformat) + MAX_MESSAGE_LEN);
    status = (int) vsprintf(bufPtr, newformat, args);
    syslog(level, "%s", bufPtr);

    doFree(bufPtr);
#endif

    doFree(newformat);
    va_end(args);
    return 0;
}
