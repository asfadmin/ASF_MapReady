/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	datetime.c

Description:	
	Check if valid date/time string routine(s)

	This module contains the definition of the function(s) used for
	validating a character string as a date/time string.  
	Ex. 1995-001-00:00:00.000
	Ex. 1995-001T00:00:00.000	ODL format

External Functions:
	is_datetime 
	
Static Functions:
	None

External Variables Defined:
	None

File Scope Static Variables:
	None

Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)datetime.c	1.1    11/21/96";

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "defs.h"
#include "PPShdr.h"

#ifdef __STDC__ 
int is_datetime(char *);      
#else
int is_datetime();      
#endif 

#define LEAP_YEAR_DAYS   366
#define LAST_LEAP_FEB_DAY 29

#define LEAPYEAR(y) (( y%400 == 0 ) || ((y%4 == 0) && (y%100 != 0)) )





/*==============================================================================
Function:	int is_datetime(char *string)

Description:	
	Parse and validate string as a valid CSA date/time string

	This function checks if string is a valid DATE/TIME value.  The
string is first checked for proper format which means that digits as
well as -, :, . are found in correct locations in the string.  Once the
syntax is validated, the date obtained is checked if it is a valid
date.  This function returns ACCEPT if string is a valid DATE/TIME
value, REJECT if string is not in the DATE/TIME format, and ERROR if
string is in the DATE/TIME format but is an invalid DATE/TIME value.

	The string validation is performed by implementing a state
machine representing the valid date/time format.  Basically, the string
is parsed left to right starting state at state 0.  As succeeding
characters are read, changes in state may be triggered until the final
accepting state is reached unless an error occurs, in which case
parsing is halted and REJECT is returned.  The state ACCEPT is returned
unless upon checking the date/time values, they are found invalid, in
which case ERROR is returned.

Parameters:
	char * string - input string to validate as date/time 

Returns:
	ACCEPT - string is a valid date/time string
	REJECT - string is not a valid date/time string 
	ERROR - string has correct format but invalid value

Creator:	Norbert Piega	
Creation Date:	03/03/1994

Notes:		
==============================================================================*/
#ifdef __STDC__
int 
is_datetime(char *string)
#else
int 
is_datetime(string)
   char * string;
#endif
{
   double second = 0.0;
   char *location_of_lastdash, 
	*location_of_colon, 
	*p = string,
	*ptr = NULL;
   LOGICAL accept = FALSE, 
	   reject = FALSE ;
   int year = 0, 
       day = 0, 
       hour = 0, 
       minute = 0 ;
   int state, 
       day_limit ;

   /* If input is NULL, REJECT
   */
   if (string == (char *)NULL)
      return(REJECT) ;

   /* if ODL format, convert to timerec format */
   CONVERT_ODL_TO_TIMESTR(string, ptr);

   /* Start state is state 0 
   */
   state = 0;

   /* Use state machine to check if the string is in correct syntax.
   -- For DATE which is YYYY-DDD see state 0-10.
   -- For TIME which is HH:MM[SS.[S[S]+] see state 1,2,14-21.
   -- The accepting state is : 21
   --
   -- The following are the valid inputs :
   --	 digit, - (dash), : (colon), . (period/dot)
   */
   while (!accept && !reject)
   {
      switch (state)
      {
         /* States which expect a digit for next input
         -- advancing to the next immediate state.
         -- Move 0-4 is YYYY, 5-6-7-10 for DDD.
         -- Move 11-13 HH, 14-16 MM, 17-19 SS,
         --    20-21+ fraction of seconds.
         */
         case 0 :   case 1 :   case 2 :	  case 3 :
         case 5:    case 6 :   case 7 :   case 9 :
         case 10 :  case 12 :  case 13 :  case 15 :
         case 16 :  case 18 :
            /* Go to next state.  Advance input string pointer. 
	    */
            if (isdigit(*p))
            {
               p++;
               state = state + 1;
            }
            else
               reject = TRUE;
            break ;

         case 4 :
            if (*p == '-')
            {
               year = atoi(string);
               p++;
               state = 5;
            }
            else
               reject = TRUE;
            break;

         case 8 :
            if (*p == '-')
            {
               day = atoi(string+5);
               p++;
               state = 9 ;
            }
            else
               reject = TRUE;
            break;

         case 11 :
            if (*p == ':')
            {
               location_of_lastdash = strrchr(string, '-');
               hour = atoi(location_of_lastdash+1);
               p++;
               state = 12;
            }
            else
               reject = TRUE;
            break;

         case 14 :
            if (*p == ':')
            {
               location_of_colon = strchr(string, ':');
               minute = atoi(location_of_colon+1);
               p++;
               state = 15 ;
            }
            else
               reject = TRUE;
            break;

         case 17 :
            if (*p == '.')
            {
               location_of_colon = strchr(location_of_colon+1, ':');
               second = (double) atof(location_of_colon+1);
               p++;
               state = 18 ;
            }
            else
               reject = TRUE;
            break;

         case 19 :
            if (*p == NULL)
               accept = TRUE;
            else if (isdigit(*p))
               p++;
            else
               reject = TRUE;
            break;
      }
   }

   /* REJECT : string is not in date/time format 
   */
   if (reject)
      return(REJECT);

   /* Set day_limit and last day of February 
   */
   if (LEAPYEAR(year))
      day_limit = LEAP_YEAR_DAYS;
   else
      day_limit = LEAP_YEAR_DAYS-1;

   /* Year YYYY must equal 0001 - 9999 
   */
   if (year == 0)
       return(ERROR);

   /* Day of year (no month) format, check day 
   */
   else if ((day == 0L) || (day > day_limit))
      return(ERROR);

   /* Check HH (HOUR), MM (MINUTE), SS.SSS... (SECOND) 
   */
   if (hour > 23)
      return(ERROR);

   if (minute > 59)
      return(ERROR);

   if (second > 61.0)
      return(ERROR);

   return(ACCEPT) ;

} /* is_datetime */

#ifdef __STDC__
char
odl_timestring_to_seconds(
char*		odl_time_string,
time_t		*seconds,		/* return */
unsigned short	*milliseconds)		/* return */
#else
char
odl_timestring_to_seconds(odl_time_string, seconds, milliseconds)
char*		odl_time_string;
time_t		*seconds;
unsigned short	*milliseconds;
#endif
{
	char time_string[TIME_STRING_LEN + 1];
	int		rc;
	struct tm	tm_time;

	(void)strcpy(time_string, odl_time_string);

	/* time string length must == TIME_STRING_LEN */
	if (strlen(time_string) != TIME_STRING_LEN)
		return(0);

	/* parse the milli second part */
	rc = sscanf(time_string, "%*17c.%hd", milliseconds);
	if (rc != 1)
		return(0);

	/* put the rest in a struct tm */
	time_string[17] = '\0';
	if (strptime(time_string, "%Y-%jT%H:%M:%S", &tm_time) == NULL)
		return(0);

	/* convert tm to seconds */
	*seconds = mktime(&tm_time);
	if (*seconds == (time_t) -1)
		return(0);
	else
	{
		*seconds -= timezone;
	}

	return(1);
	
}/*odl_timestring_to_seconds*/

#ifdef __STDC__
char
seconds_to_odl_timestring(
char*		odl_time_string,	/* return, user provides space */
time_t		seconds,
unsigned short	milliseconds)
#else
char
seconds_to_odl_timestring(odl_time_string, seconds, milliseconds)
char*		odl_time_string;	/* return, user provides space */
time_t		seconds;
unsigned short	milliseconds;
#endif
{
	struct tm tm_time;
	
	if (gmtime_r(&seconds, &tm_time) == 0)
		return(0);

	(void) sprintf(odl_time_string, "%04d-%03dT%02d:%02d:%02d.%03d",
		tm_time.tm_year + 1900, tm_time.tm_yday + 1,
		tm_time.tm_hour, tm_time.tm_min, tm_time.tm_sec,
		milliseconds);

	return (1);

}/*seconds_to_odl_timestring*/
