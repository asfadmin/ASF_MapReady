/*
 * @(#)logUtils.h	2.7 96/04/18 10:56:11
 *
 * Declarations needed by the functions in logUtils.c
 */

#ifndef _LOGUTILS_H
#define _LOGUTILS_H

static char sccsid_logutils_h[] =
	"@(#)logUtils.h	2.7 96/04/18 10:56:11";

int initASFlog(int argc, char **argv, char *identString);

#ifdef	NOSTDARG
int printfLog();
int printfLLog();
#else
int printfLog(char *format, ...);
int printfLLog(int level, char *format, ...);
#endif

typedef enum {
    SYSLOG = 0,
    SYSLOG_AND_TERM
} LoggingType;

/* MAX_MESSAGE_LEN used by printfLLog */
#define MAX_MESSAGE_LEN	800

#endif /* !_LOGUTILS_H */
