/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	timerec.c

Description:	
	This module contains the function for allocating a time record.
The time record contains the time string as well as the converted time
components of the time string.

External Functions:
	alloc_time_record
	convert_date_string
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)timerec.c	1.1    11/21/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "defs.h"
#include "timerec.h"  /* time record header file */

extern void *util_do_malloc() ;
extern int is_datetime() ;



/*==============================================================================
Function:	Time_Record *alloc_time_record(void)
Description:	
	Allocate and initialize a Time record data structure

	This function allocates a time record and then initializes its
fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to time record or NULL (if unable to allocate) 
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
Time_Record *
alloc_time_record(void)
#else
Time_Record *
alloc_time_record()
#endif
{
   Time_Record *timerec = NULL ;

   timerec = (Time_Record *)util_do_malloc(sizeof(Time_Record)) ;
   if (timerec != (Time_Record *)NULL)
   {
      *timerec->time_string = NULL ;
      timerec->year = timerec->day = timerec->hour = timerec->minute = 0 ;
      timerec->seconds = 0.0 ;
   }

   return(timerec) ;

} /* alloc_time_record */
 





/*==============================================================================
Function:	int convert_date_string(char *string, Time_Record *gentime)

Description:
	Convert a date string into its component values and store in a
time record

	This function calls is_datetime to validate the input string as
a date/time.  If the string is accepted as a valid date/time string,
the time components are picked and their numerical equivalents are
assigned to respective fields in the time record.  ACCEPT is returned
after completing the conversions and assignments.  The return status of
the is_datetime function is returned on default.

Parameters:
	char *string - input date string to convert
	Time_Record *gentime - time record where converted time 
components are stored

Returns:	
	As received from is_datetime:
	ACCEPT - string is a valid date/time and successfully converted 
	REJECT - string is not a valid date/time; cannot be converted
	ERROR - error encountered while parsing string

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
convert_date_string(char *string, Time_Record *gentime)
#else
int
convert_date_string(string, gentime)
   char *string ;
   Time_Record *gentime ;
#endif
{
   int status = REJECT ;
   char *ptr ;
 
   if (gentime == (Time_Record *)NULL)
      return(status) ;

   if ((status = is_datetime(string)) == ACCEPT)
   {
      gentime->year = atoi(string) ;
      ptr = strchr(string, '-') ;
      gentime->day = atoi(ptr+1) ;
      ptr = strrchr(string, '-') ;
      gentime->hour = atoi(ptr+1) ;
      ptr = strchr(string, ':') ;
      gentime->minute = atoi(ptr+1) ;
      ptr = strrchr(string, ':') ;
      gentime->seconds = (double) atof(ptr+1) ;

      sprintf(gentime->time_string, "%d-%03dT%02d:%02d:%06.03f",
         gentime->year, gentime->day, gentime->hour,
         gentime->minute, gentime->seconds);

   }
 
   return(status) ;

} /* convert_date_string */




/*==============================================================================
Function:	void timerec_cmp(Time_Record *timerec1, Time_Record *timerec2,
				int *res)
Description:	Compares 2 time values	
Parameters:	Two records containing time values to be compared and the result
Returns:	none
Creator:	Norbert Piega
Creation Date:	Wed Dec  6 14:32:16 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void timerec_cmp(Time_Record *timerec1, 
		Time_Record *timerec2, Time_Compare_Result *res)
#else
void timerec_cmp(timerec1, timerec2, res)
   Time_Record *timerec1 ;
   Time_Record *timerec2 ;
   Time_Compare_Result *res ;
#endif
{
   if (timerec1->year > timerec2->year)
      *res = greaterthan ;
   if (timerec1->year < timerec2->year)
      *res = lessthan ;

   if (timerec1->day > timerec2->day)
      *res = greaterthan ;
   if (timerec1->day < timerec2->day)
      *res = lessthan ;

   if (timerec1->hour > timerec2->hour)
      *res = greaterthan ;
   if (timerec1->hour < timerec2->hour)
      *res = lessthan ;

   if (timerec1->minute > timerec2->minute)
      *res = greaterthan ;
   if (timerec1->minute < timerec2->minute)
      *res = lessthan ;

   if (timerec1->seconds > timerec2->seconds)
      *res = greaterthan ;
   if (timerec1->seconds < timerec2->seconds)
      *res = lessthan ;

   *res = equal ;


} /* timerec_cmp */





/*==============================================================================
Function:	void timerec_subtract(Time_Record *timerec, int units, 
					Time_Unit unittype)
Description:	Provides the time value units before the input time
Parameters:	Input time record, number or units to subtract, unit type
Returns:	none
Creator:	Norbert Piega
Creation Date:	Wed Dec  6 14:32:16 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void timerec_subtract(Time_Record *timerec, int units, Time_Unit unittype)
#else
void timerec_subtract(Time_Record *timerec, int units, Time_Unit unittype)
   Time_Record *timerec ;
   int units ;
   Time_Unit unittype ;
#endif
{
   int is_leap_year ;
   float secdiff ;
   int hourdiff, mindiff, daydiff, yeardiff ;
   int yrdays ;

   is_leap_year = IS_LEAP_YEAR(timerec->year) ;
   if (is_leap_year)
      yrdays = 364 ;
   else
      yrdays = 365 ;

   if (unittype == unit_seconds)
   {
      secdiff = timerec->seconds - units ;
      if (secdiff < 0)
      {
	 timerec->seconds = 60 + secdiff ;
	 unittype = unit_minute ;
	 units = 1 ;
      }
   }
   if (unittype == unit_minute)
   {
      mindiff = timerec->minute - units ;
      if (mindiff < 0)
      {
	 timerec->minute = 60 + mindiff ;
	 unittype = unit_hour ;
	 units = 1 ;
      }
   }
   if (unittype == unit_hour)
   {
      hourdiff = timerec->hour - units ;
      if (hourdiff < 0)
      {
	 timerec->hour = 24 + hourdiff ;
	 unittype = unit_day ;
	 units = 1 ;
      }
   }
   if (unittype == unit_day)
   {
      daydiff = timerec->day - units ;
      if (daydiff < 0)
      {
	 timerec->day = yrdays + secdiff ;
	 unittype = unit_year ;
	 units = 1 ;
      }
   }
   if (unittype == unit_year)
   {
      yeardiff = timerec->year - units ;
      if (yeardiff < 0)
      {
	 timerec->seconds = 60 + secdiff ;
      }
   }


} /* timerec_subtract */





/*==============================================================================
Function:	void timerec_range(Time_Record *timerec, 
				  Time_Record *timerec_min, 
				  Time_Record *timerec_max,
				  int exclusive, int &res)
Description:	Checks if time is within the given range
Parameters:	Time records containing time to check and time ranges,
		Flag if exclusive boundary check, variable to pass back value
Returns:	none
Creator:	Norbert Piega
Creation Date:	Wed Dec  6 14:32:16 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void timerec_range(Time_Record *timerec, Time_Record *timerec_min, 
   Time_Record *timerec_max, int exclusive, int *res)
#else
void timerec_range(timerec, timerec_min, timerec_max, exclusive, res)
   Time_Record *timerec ;
   Time_Record *timerec_min ;
   Time_Record *timerec_max ;
   int exclusive ;
   int *res ;
#endif
{
   Time_Compare_Result mincheck, maxcheck ;

   timerec_cmp(timerec, timerec_min, &mincheck) ;
   timerec_cmp(timerec, timerec_max, &maxcheck) ;

   /* Case tmin < t < tmax */
   if (mincheck == greaterthan && maxcheck == lessthan && exclusive)
   {
      *res = TRUE ;
      return;
   }

   /* Case tmin =< t =< tmax */
   if ((mincheck == greaterthan || mincheck == equal) &&
       (maxcheck == lessthan || maxcheck == equal) && !exclusive)
   {
      *res = TRUE;
      return;
   }

   *res = FALSE ;

} /* timerec_range */

/* End of file */
