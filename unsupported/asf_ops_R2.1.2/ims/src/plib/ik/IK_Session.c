/*
 * Name: IK_Session.c
 *
 * Description: This file contains code which gathers and processes
 * session-related data, such as the session start date and time, the
 * end date and time, and the session ID.
 *
 * Notes:
 *
 *-------------------------------------------------------------------------
 *
 * RCS information:
 *
 * $RCSfile$
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0  1995/11/06  13:55:25  ims
 * COPIED FROM 4.5.1.4 FOR GUI_PROMO_5_0_951106.
 *
 * Revision 4.5.1.4  1995/10/17  14:36:53  winter
 * Changed to use strftime() instead of cftime(), for portability.
 *
 * Revision 4.5.1.4  1995/10/17  13:29:00  winter
 * Replaced calls to cftime() with strftime() for portability.
 *
 * Revision 4.5.1.3  1995/10/12  19:12:36  winter
 * Added code for IK_GetSessionStartTime() and IK_GetSessionID().
 *
 * Revision 4.5.1.2  1995/10/12  18:40:25  winter
 * Fleshed out the first version of IK_InitSessionData().
 *
 * Revision 4.5.1.1  1995/10/12  18:00:32  winter
 * Placeholder.
 *
 * Revision 4.5  1995/10/12  17:58:29  winter
 * Initial checkin.
 *
 * Revision 4.5  1995/10/12  17:58:29  winter
 * Initial checkin.
 *
 * */

/*****************************************************************************/

/* Define the RCS identifier string. */
static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* Standard headers */
#include <assert.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

/* Third-party library headers */

/* IMS headers */
#include "IK_Session.h"

/*****************************************************************************/

/* Local #define directives */

/* This is the maximum number of digits possible in a process ID. The
   maximum value of a PID is found in the constant MAX_PID in
   <limits.h>, and is typically 30000. */
#define MAX_PID_DIGITS (5)

/* These are the patterns to use for date and time fields in the
   session ID. They are used as templates only, for calculating the
   length of the session ID. */
#define SESSION_ID_DATE_TEMPLATE "YYYYMMDD"
#define SESSION_ID_TIME_TEMPLATE "HHMMSS"

/* These are the patterns to use for formatting date and time fields
   in the session ID by calling cftime(). */
#define SESSION_ID_DATE_FORMAT "%Y%m%d"
#define SESSION_ID_TIME_FORMAT "%H%M%S"

/* This is the maximum allowable length of a session ID. */
#define MAX_SESSION_ID_LENGTH (MAXHOSTNAMELEN + 1 \
			       + MAX_PID_DIGITS + 1 + \
			       sizeof(SESSION_ID_DATE_TEMPLATE) + 1 + \
			       sizeof(SESSION_ID_TIME_TEMPLATE))

/* This is the character to use to delimit the sub-parts of the
   session ID string. */
#define SESSION_ID_PART_DELIMITER ':'

/*****************************************************************************/

/* Local data type definitions */

/*****************************************************************************/

/* Local variable definitions */

/* This variable stores the session start time in UNIX seconds. This
   time can then be converted to a formatted date-time string using
   cftime() or a similar function. */
static time_t time_sessionStart;

/* This is the process ID of this client process. */
static pid_t pid_client;

/* This is the FQDN (fully-qualified domain name) of the machine
   running the client. */
static char ac_clientHostFQDN[MAXHOSTNAMELEN + 1];

/* ac_dateString[] and ac_timeString[] hold the YYYYMMDD and HHMMSS
   formatted date and time, respectively. */
static char ac_dateString[sizeof(SESSION_ID_DATE_TEMPLATE) + 1];
static char ac_timeString[sizeof(SESSION_ID_TIME_TEMPLATE) + 1];

/* This is the session ID string for this session. */
static char ac_sessionID[MAX_SESSION_ID_LENGTH + 1];

/*****************************************************************************/

/* External function prototypes */

/*****************************************************************************/

/* Local function prototypes */

/*****************************************************************************/

/* PUBLIC ENTRY POINTS:
 *
 * IK_InitSessionData()
 * IK_GetSessionStartTime()
 * IK_GetSessionID()
 *
*/

/*****************************************************************************/

/*
 * Name: IK_InitSessionData()
 *
 * Description: This function gathers and initializes all
 * session-related data at the start of the session.
 *
 * Parameters:
 * None
 *
 * Return Values:
 * int 0 - If all goes well
 * int -1 - If an error occurs
 *
 * Warnings:
 *
 * Global Variables Used:
 * None
 *
 * Notes:
 *
 * (1) This function should be called as soon as possible after the
 * program is invoked.
 *
 * Revision History:
 *
 * Thursday 12 October 1995 (Eric Winter) - Started initial version.
 *
 * Tuesday 17 October 1995 (Eric Winter) - Changed to use strftime().
 * */

int
IK_InitSessionData(void)
{

    /* phostent_client points to the hostent structure for the host
       running the client. */
    struct hostent *phostent_client;

    /* ptm points to the return value from localtime(). */
    struct tm *ptm = NULL;

    /* tm_clock holds the struct tm version of the time. */
    struct tm tm_clock;

    /* i_len is the length of the formatted time string. */
    int i_len = 0;

    /*------------------------------------------------------------------------*/

    /* Get the current time to use as the session start time. */
    time_sessionStart = time(NULL);
    assert(time_sessionStart != (time_t) -1);

    /*------------------------------------------------------------------------*/

    /* Get the process ID of this client. */
    pid_client = getpid();
    assert(pid_client != (pid_t) -1);

    /*------------------------------------------------------------------------*/

    /* Get the name of the host on which the client is running,
       storing it (temporarily) using the space allocated for the host
       FQDN. N.B. The prototype for gethostname() is _supposed_ to be
       in <unistd.h>, but at least under IRIX 4.1, it is _not_ present
       in any header file that I can find (ELW 11 Jul 94). */
    if (gethostname(ac_clientHostFQDN, sizeof(ac_clientHostFQDN)) == -1) {
	return(-1);
    }

    /* Map the host name to a hostent structure. */
    phostent_client = gethostbyname(ac_clientHostFQDN);
    if (phostent_client == NULL) {
	return(-1);
    }

    /* Replace the gethostname() name with the FQDN from the
       hostent. */
    assert(phostent_client->h_name != NULL);
    (void) strcpy(ac_clientHostFQDN, phostent_client->h_name);

    /*------------------------------------------------------------------------*/

    /* Convert the time to a tm structure. */
    ptm = localtime(&time_sessionStart);
    assert(ptm != NULL);
    tm_clock = *ptm;

    /* Format the date in YYYYMMDD format. */
    i_len = strftime(ac_dateString, sizeof(ac_dateString),
		     SESSION_ID_DATE_FORMAT, &tm_clock);
    assert(i_len > 0);

    /* Format the time in HHMMSS format. */
    i_len = strftime(ac_timeString, sizeof(ac_timeString),
		     SESSION_ID_TIME_FORMAT, &tm_clock);
    assert(i_len > 0);

    /*------------------------------------------------------------------------*/

    /* Construct the session ID string. It is composed of the FQDN,
       followed by the PID, then the date and the time. The parts are
       separated with the character specified by the
       SESSION_ID_PART_DELIMITER constant. */
    (void) sprintf(ac_sessionID, "%s%c%d%c%s%c%s",
		   ac_clientHostFQDN,
		   SESSION_ID_PART_DELIMITER,
		   pid_client,
		   SESSION_ID_PART_DELIMITER,
		   ac_dateString,
		   SESSION_ID_PART_DELIMITER,
		   ac_timeString);

    /*------------------------------------------------------------------------*/

    /* Return normally. */
    return(0);

}

/*****************************************************************************/

/*
 * Name: IK_GetSessionStartTime()
 *
 * Description: This function returns the session start time in UNIX
 * seconds.
 *
 * Parameters:
 * None
 *
 * Return Values:
 * time_t time_sessionStart - Start time of session
 * (time_t) -1 - If an error occurs
 *
 * Warnings:
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions
 *
 * Notes:
 *
 * Revision History:
 *
 * Thursday 12 October 1995 (Eric Winter) - Initial version.
 * */

time_t
IK_GetSessionStartTime(void)
{

    /* Return the session start time. */
    return(time_sessionStart);

}

/*****************************************************************************/

/*
 * Name: IK_GetSessionID()
 *
 * Description: This function returns a pointer to the (static)
 * session ID string.
 *
 * Parameters:
 * None
 *
 * Return Values:
 * char *pc_sessionID - Pointer to static session ID string
 * NULL - If an error occurs
 *
 * Warnings:
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions
 *
 * Notes:
 *
 * Revision History:
 *
 * Thursday 12 October 1995 (Eric Winter) - Initial version.
 * */

char *
IK_GetSessionID(void)
{

    /* Return the session ID string. */
    return(ac_sessionID);

}
