/*==============================================================================
Filename:	WALPSfname.c

Description:	

   Identify file type by validating the file name related functions.
This module contains the function(s) used for determining if a filename 
is a WALLOPS conformant filename.  The Wallops file naming convention
is described in Alaska SAR Facility (ASF) to Wallops Flight Facility
(WFF) Interface Specification Document (ISD), JPL-D11871.

External Functions:
	parse_WALPS_filename
	
Static Functions:
	None
	
External Variables Defined:
	WALPS_File_Id_Table
	
File Scope Static Variables:
	None
	
Notes:
1.  October '96 - RH
    Update to ISD Version 2.3
2.  July '97 - R. Hoffman
    Correct comments for parse_WALPS_filename() on filename format
==============================================================================*/

static char SccsFile[] = "WALPSfname.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)WALPSfname.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include "WALPS.h"

#ifdef __STDC__
int parse_WALPS_filename(char *) ;
#else
int parse_WALPS_filename() ;
#endif

extern void *util_do_malloc() ;

#define LEAP_YEAR_DAYS   366
#define LAST_LEAP_FEB_DAY 29

#define WALPS_ID_STRLEN          4

/* yyyydddhhmmssfff */
#define WFF_DATETIME_STRLEN     16
 
#define LEAPYEAR(y) (( y%400 == 0 ) || ((y%4 == 0) && (y%100 != 0)) )

File_Identifier WALPS_File_Id_Table[] =
{
   { WALPS_REQ,        WALPS_REQ_PREFIX }, 
   { WALPS_RES,        WALPS_RES_PREFIX }, 
   { WALPS_WOS,        WALPS_WOS_PREFIX },
   { WALPS_EPH,        WALPS_EPH_PREFIX },
   { WALPS_DNL,        WALPS_DNL_PREFIX },
   { WALPS_MSH,        WALPS_MSH_PREFIX }, 
   { WALPS_TPR,        WALPS_TPR_PREFIX },
   { SENTINEL,         NULL             }
} ;



/*==============================================================================
Function:	int parse_WALPS_filename(filename)

Description:
	Parse and validate the input string filename as a Wallops
filename.  This is an initial means of determining the file type of a
specific flight agency file.  If filename conforms to the Wallops file
naming convention, then it is further validated as an Wallops file.
The table Wallops_File_Id_Table is consulted to validate file id
information that are obtained from the filename.  The return status of
this function is TRUE, FALSE (Wallops file or not) or an error code if
an error occurred.

Parameters:
	char *filename - file path of input WALLOPS file to parse

Returns:
	ERROR - Unable to recognize filename as valid Wallops filename
	TYPE_MATCHED - file id code for the type of file determined from
the filename

Creator:	Norbert Piega	
Creation Date:	07/29/1994
Notes:		
	The Wallops file naming convention is described in Alaska 
SAR Facility (ASF) to Wallops Flight Facility (WFF) Interface 
Specification Document (ISD), JPL-D11871.  The following examples 
illustrate Wallops file names:

WOS_1996123010000000 (for a WOS file)
DNL_1996123010000000 (for a DOWNLINK file)

The form is XXX_YYYYDDDHHMMSSUUU
   where XXX may be WOS, DNL, EPH, REQ, RES, MSH, OR TPR 
      YYYYDDD is a valid date string; HHMMSSUUU (HH:MM:SS.UUU) is the time.
==============================================================================*/
#ifdef __STDC__
int
parse_WALPS_filename(char *filename)
#else
int
parse_WALPS_filename(filename)
   char *filename ;
#endif
{
   int i, type_matched = ERROR ;
   int dt_status ;
   char file_id[WALPS_ID_STRLEN+1] ;
   char datestr[WFF_DATETIME_STRLEN+1] ;
   char *start, *fname, *datep ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Skip past directory path to filename portion only
   */
   if ((start = strrchr(filename, '/')) != NULL)
   {
      fname = (char *)util_do_malloc(sizeof(char)*(strlen(start+1)+1)) ;
      strcpy(fname, start+1) ;
   }
   else
      fname = filename ;

   /* Filename length OK?
   */
   if ((int)strlen(fname) > WALPS_ID_STRLEN+WFF_DATETIME_STRLEN)
   {
      sprintf(logmsg, "ERROR, %s has invalid Wallops file name length\n", 
	 fname) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Collect filename prefix which serves as identifier
   -- Prefix is delimited by WALPS_FN_DELIM
   */
   for (i=0; fname[i] != WALPS_FN_DELIM; i++)
   {
      if (!isalpha(fname[i]))
      {
         sprintf(logmsg, 
	    "ERROR, %s is an invalid Wallops file name\n", fname) ;
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
      }
      file_id[i] = toupper(fname[i]) ;
   }
   file_id[i] = '\0' ;

   /* Make sure the remainder of the filename (the time part) is valid.
   -- Report and return an error if not
   */
   datep = &fname[i] + 1 ;
   if (strlen(datep) != WFF_DATETIME_STRLEN)
   {
      syslog(LOG_ERR, "ERROR, Invalid Wallops file name length\n") ;
      return(ERROR) ;
   }
   strcpy(datestr, datep) ;

   /* Call is_wff_date to validate time string
   */
   if ((dt_status = is_wff_date(datestr)) == REJECT ||
	dt_status == ERROR)
   {
      sprintf(logmsg,
	 "ERROR, The date string portion of Wallops filename %s is invalid\n",
	  fname) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Search table if prefix matches identifier in table
   */
   i = 0 ;
   while (WALPS_File_Id_Table[i].file_id_number != SENTINEL)
      if (strcmp(WALPS_File_Id_Table[i].file_identifier, file_id) == 0)
      {
         type_matched = WALPS_File_Id_Table[i].file_id_number ;
         break ; 
      }
      else
         i++ ;

   return(type_matched) ;

} /* parse_WALPS_filename */



/*==============================================================================
Function:       int is_wff_date(char *string)
 
Description:
        Parse and validate string as a valid WALPS date string YYYYDDD

Parameters:
        char * string - input string to validate as date/time 
 
Returns:
        ACCEPT - string is a valid date/time string
        REJECT - string is not a valid date/time string 
        ERROR - string has correct format but invalid value
 
Creator:        Rodney Hoffman
Creation Date:  10/16/1996
 
Notes:
==============================================================================*/
#ifdef __STDC__
int 
is_wff_date(char *string)
#else
int 
is_wff_date(string)
   char * string;
#endif
{
   LOGICAL accept = FALSE, 
           reject = FALSE ;
   int     year = 0, 
           day = 0, 
           day_limit,
           hours, minutes, seconds ;
 
   /* If input is NULL, REJECT
   */
   if (string == (char *)NULL)
      return(REJECT) ;
 
   sscanf (string, "%4d%3d%2d%2d%2d", &year, &day, &hours, &minutes, &seconds);

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
 
   if ((hours < 0) || (hours > 23)) 
      return (ERROR);

   if ((minutes < 0) || (minutes > 59))
      return (ERROR);

   if ((seconds < 0) || (seconds > 59))
      return (ERROR);

   return(ACCEPT) ;
 
} /* is_wff_date */

/* End of File */
