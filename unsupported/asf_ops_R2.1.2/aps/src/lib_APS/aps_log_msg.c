#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#pragma ident	"@(#)aps_log_msg.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.aps_log_msg.c"
/*******************************************************************
**
** File:        aps_log_msg.c
**
** Description: 
**				Define functions that will print error message to 
**				UNIX stderr and syslog according to the project specified
**				standard.
**
**				contains the following functions:
**				aps_log_msg()
**              aps_open_syslog()
**              aps_close_syslog()
**
**				aps_fprintf()
**				aps_syslog()
**
** Author:		Q. Sun
**
** Date:        01/23/96
**
** Modified: 
**
**************************************************************** */
#include "aps_log_msg.h"
#include <time.h>
#include <sys/utsname.h>
#include <stdio.h>

/*
==========================================================================
Function:       aps_getCurrentTimeStamp(char* curr_time)

Description: 	Fill in the input buffer with the current time stamp.

Parameters:
Returns:
Creator:        Q. Sun
Creation Date:  01/25/1996
Revision History:
Notes:
==========================================================================
*/
void aps_getCurrentTimeStamp(char* curr_time)
{
time_t sec_since_1970 ;
struct tm *time_ptr ;
char *month[] =
{
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
} ;
char *wday[] =
{
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
} ;

	/*  Get the current time, seconds since 1 Jan 1970. */
	time(&sec_since_1970) ;
				  
	/* compute time fields from sec_1970 */
	time_ptr = localtime (&sec_since_1970) ;
						   
	/*
	-- Store time fields into buffer..
	-- Use asftime format
	*/
	sprintf(curr_time, "%s %s %2.2d %2.2d:%2.2d:%2.2d %d",
			wday[time_ptr->tm_wday], 
			month[time_ptr->tm_mon], 
			time_ptr->tm_mday, 
			time_ptr->tm_hour,
			time_ptr->tm_min,
			time_ptr->tm_sec,
			1900 + time_ptr->tm_year);
}																							
/*
==========================================================================
Function:       aps_close_syslog()
Description:	Close the UNIX syslog connection.
Parameters:
Returns:
Creator:        Q. Sun
Creation Date:  01/25/1996
Revision History:
Notes:
==========================================================================
*/
void aps_close_syslog()
{
	closelog();
}

/*
==========================================================================
Function:       aps_open_syslog()
Description:	Open the UNIX syslog with the project specified standard.
Parameters:
Returns:
Creator:        Q. Sun
Creation Date:  01/25/1996
Revision History:
Notes:
==========================================================================
*/
void aps_open_syslog()
{
	openlog("APS:", LOG_PID|LOG_CONS|LOG_NDELAY, LOG_LOCAL1);
}

/*
==========================================================================
Function:       aps_fprintf()
Description:	Perform the UNIX fprintf() function to the specified 
				output file desciptor (stdout or stderr) after formatting
				the message (msg). 
				In addition to the message string to be printed, 
				it also takes as input a program name and a message 
				level ID that indicate the severity of the message.
Parameters:
Returns:
Creator:        Q. Sun
Creation Date:  01/25/1996
Revision History:
Notes:
==========================================================================
*/
void aps_fprintf(FILE* filePtr, char* progName, int level, char* msg)
{
char hostName[HOSTNAME_LEN+1];
char timeStamp[TIME_LEN*2];
struct utsname uname_info;     /*Structure for uname() */
char msgType [MSG_TYPE_LEN]; 

	aps_getCurrentTimeStamp(timeStamp);
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, HOSTNAME_LEN);
	hostName[HOSTNAME_LEN] = '\0';

	switch (level)
	{
		case APS_INFO:
			sprintf(msgType, "APS_INFORMATION");
			break;
		case APS_ERROR:
			sprintf(msgType, "APS_ERROR");
			break;
		case APS_WARNING:
			sprintf(msgType, "APS_WARNING");
			break;
		case APS_DEBUG:
			sprintf(msgType, "APS_DEBUG");
			break;
		case APS_CRITICAL:
			sprintf(msgType, "APS_CRITICAL");
			break;
		default:
			sprintf(msgType, "INVALID_MSG_LEVEL");
			break;
	}

	fprintf(filePtr, "\n%%%s%%  %s::%s  %s\n", 
			msgType, hostName, progName, timeStamp); 
	fprintf(filePtr, "%s\n", msg);
}

/*
==========================================================================
Function:       
Description:	Perform the UNIX syslog() function after formatting
				the input message according to the project specified
				standard.
				In addition to the message string to be printed, 
				it also takes as input a program name and a message 
				level ID that indicate the severity of the message.
Description:
Parameters:
Returns:
Creator:        Q. Sun
Creation Date:  01/25/1996
Revision History:
Notes:
==========================================================================
*/
void aps_syslog(char* progname, int level, char* msg)
{
char logString[MSG_LEN];

	switch(level)
	{
		case APS_INFO:
			sprintf(logString, "(%s) INFO: %s\n", progname, msg);
			syslog(LOG_INFO, logString);
			break;
		case APS_ERROR:
			sprintf(logString, "(%s) ERROR: %s\n", progname, msg);
			syslog(LOG_ERR, logString);
			break;
		case APS_WARNING:
			sprintf(logString, "(%s) WARNING: %s\n", progname, msg);
			syslog(LOG_WARNING, logString);
			break;
		case APS_DEBUG:
			sprintf(logString, "(%s) DEBUG: %s\n", progname, msg);
			syslog(LOG_DEBUG, logString);
			break;
		case APS_CRITICAL:
			sprintf(logString, "(%s) CRITICAL: %s\n", progname, msg);
			syslog(LOG_CRIT, logString);
			break;
		default:
			sprintf(logString, "(%s) INVALID_MSG_LEVEL: %s\n", 
					progname, msg);
			syslog(LOG_ERR, logString);
			break;
	}
}

/*
==========================================================================
Function:       aps_log_msg()

Description:	Perform the UNIX syslog() and/or fprintf()function 
				depending on the flag set in the input parameter list.
Parameters:
				progname: the caller program name
				level: the severity level of the message; valide values are:
					APS_INFO
					APS_ERROR
					APS_WARNING
					APS_CRITICAL
					APS_DEBUG
				msg: the message to be printed
				syslogFlag: toggle flag; 
							if set to TRUE (or DO_SYSLOG),
							will invoke the aps_syslog() function 
				printFlag: toggle flag; if set to TRUE (or DO_PRINT),
							will invoke the aps_fprintf() function.

include file: 	aps_log_msg.h

Returns:
Creator:        Q. Sun
Creation Date:  01/25/1996
Revision History:
Notes:
==========================================================================
*/
void aps_log_msg(char* progname, int level, char* msg, 
				int syslogFlag, int printFlag)
{
	if (printFlag == TRUE)	
	{
		if (level == APS_INFO)
			aps_fprintf(stdout, progname, level, msg); 
		else
			aps_fprintf(stderr, progname, level, msg); 
	}
	if (syslogFlag == TRUE)
		aps_syslog(progname, level, msg);
}

