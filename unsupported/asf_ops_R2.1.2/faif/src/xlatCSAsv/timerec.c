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

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "faifdefs.h"
#include "timerec.h"  /* time record header file */


#ifdef __STDC__
Time_Record *alloc_time_record(void) ;
int convert_date_string(char *, Time_Record *) ;
#else
Time_Record *alloc_time_record() ;
int convert_date_string() ;
#endif

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
   }
 
   return(status) ;

} /* convert_date_string */


/* End of file */
