
/*==============================================================================
Filename:	WALPSar.c

Description:
	WALLOPS availability response file parser function.  This module
contains the function for parsing parts of a WALLOPS avail-resp file.  The
specification of the WALLOPS file format is described in the Alaska SAR
Facility (ASF) to Wallops Flight Facility (WFF) Interface Specification
Document (ISD), JPL-D11871.

External Functions:
	alloc_WALPS_AR_record
	extract_WALPS_AR_record

Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  Nov. '96 - RH
    Updated for ISD Version 2.3
==============================================================================*/

static char SccsFile[] = "WALPSar.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)WALPSar.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <syslog.h>
#include "odldef.h"
#include "odlinter.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include "WALPS.h"
#include "WALPShdr.h"
#include "WALPSar.h"

#ifdef __STDC__
WALPS_AR_Record *alloc_WALPS_AR_record(void) ;
int              extract_WALPS_AR_record
                      (FILE *, WALPS_AR_Record *, AGGREGATE, int) ;
#else
WALPS_AR_Record *alloc_WALPS_AR_record() ;
int              extract_WALPS_AR_record() ;
#endif

extern void *util_do_malloc() ;
extern int   extract_param_value() ;


/*==============================================================================
Function:	WALPS_AR_Record *alloc_WALPS_AR_record(void)

Description:	
	Allocate space for a WALPS avail-resp record.  The record contains 
various fields storing information obtained from a WALPS file avail-resp
ODL object.  Once the avail-resp record is allocated successfully, the field
values are initialized and a pointer to the record is returned.  If the
allocation did not succeed, NULL is returned.

Parameters: 	None
Returns:	pointer to newly allocated AR record or NULL 
Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
WALPS_AR_Record *
alloc_WALPS_AR_record(void)
#else
WALPS_AR_Record *
alloc_WALPS_AR_record()
#endif
{
   WALPS_AR_Record *ar_record = NULL ;

   ar_record = (WALPS_AR_Record *)util_do_malloc(sizeof(WALPS_AR_Record)) ;
   if (ar_record != NULL)
   {
      *ar_record->file_name = NULL ;
      *ar_record->sensor = NULL ;

      /* start and end times are struct ODLDate types,
      -- they are not initialized
      --   ar_record->time_on
      --   ar_record->time_off
      */
   }

   return(ar_record) ;

} /* alloc_WALPS_AR_record */




/*==============================================================================
Function:	int extract_WALPS_AR_record(FILE *WALPS_fp, 
		   WALPS_AR_Record *WALPS_AR_rec, AGGREGATE top, int num_recs)

Description:	
	This function parses the input file represented by the file
pointer WALPS_fp to obtain and store avail-resp record information.  The 
table WALPS_Hdr_Keyword_Table is consulted during the parsing to identify 
valid WALPS identifiers and expected symbols (ex.  start of a comment line).
The avail-resp information obtained are stored in the passed avail-resp
record WALPS_AR_rec.  If the avail-resp record was filled successfully,
ACCEPT is returned, otherwise, ERROR is returned.

Parameters:
	FILE *WALPS_fp - pointer to WALPS input file stream
	WALPS_AR_Record *WALPS_AR_rec - WALPS avail-resp record where 
	   extracted avail-resp record information will be stored
        AGGREGATE top - topmost aggregate object for input file
        int num_recs - number of AR records in RES file

Returns:	
	ACCEPT - all avail-resp record information obtained successfully
	ERROR - memory allocation errors, unable to complete avail-resp record

Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_WALPS_AR_record(FILE *WALPS_fp, WALPS_AR_Record *WALPS_AR_rec, 
			AGGREGATE top, int num_recs)
#else
int
extract_WALPS_AR_record(WALPS_fp, WALPS_AR_rec, top, num_recs)
   FILE *WALPS_fp ;
   WALPS_AR_Record *WALPS_AR_rec ;
   AGGREGATE top ;
   int num_recs
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   if (top == NULL)
   {
      syslog(LOG_DEBUG, "ERROR, ODL top node is NULL\n") ;
      return(ERROR) ;
   }

   if ((object = FindObject(top, WALPS_ARRECORD_NAME, NULL)) == NULL)
   {
      sprintf(logmsg, 
         "ERROR, unable to find avail-resp record for %s\n", 
	  WALPS_AR_rec->file_name) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   if (get_ar_range(WALPS_AR_rec, top, num_recs) == ERROR)
   {
      syslog(LOG_ERR, "ERROR.  Unable to determine time range of RES file.\n");
      return(ERROR) ;
   }

   /*  Look for SENSOR inside the AVAIL-RESP_RECORD aggregation.  
       We'll take the first one we find.  */
   if (extract_param_value(object, WALPS_AR_rec->sensor, SENSOR_KEYWD) 
       == ERROR)
      return(ERROR) ;

   return(ACCEPT) ;

} /* extract_WALPS_AR_record */




/*==============================================================================
Function: int get_ar_range(WALPS_AR_Record *WALPS_AR_rec, AGGREGATE top,
                           int num_recs)

Description:
        This function extracts the TIME_ON and TIME_OFF values 
        from each Availability Response record in top, the ODL tree, to 
        find the earliest TIME_ON and latest TIME_OFF values.
        These are placed in time_on, time_off of WALPS_AR_rec.

Parameters:
	WALPS_AR_Record *WALPS_AR_rec - WALPS avail-resp record where 
	   extracted avail-resp record information will be stored
        AGGREGATE top - topmost aggregate object for input file
        int num_recs - Number of AR records in the RES file

Returns: 
	ACCEPT - all information obtained successfully
	ERROR  - any problems 

Creator:         Rodney Hoffman
Creation Date:   November 1996
Notes:
==============================================================================*/
#ifdef __STDC__
int get_ar_range(WALPS_AR_Record *WALPS_AR_rec, AGGREGATE top, int num_recs)
#else
int get_ar_time_range(WALPS_AR_rec, top, num_recs)
   WALPS_AR_Record *WALPS_AR_rec ;
   AGGREGATE top ;
   int num_recs
#endif
{
   AGGREGATE       ar_record ;
   struct ODLDate  min_time, max_time, start, stop;
   int             i;

   /* initialize min_time and max_time */
   min_time.year = 2099;
   min_time.doy = 1;
   min_time.hours = 0;
   min_time.minutes = 0;
   min_time.seconds = 0;
   min_time.nanoseconds = 0;
   max_time.year = 1900;
   max_time.doy = 1;
   max_time.hours = 0;
   max_time.minutes = 0;
   max_time.seconds = 0;
   max_time.nanoseconds = 0;

   /* Start with the first AR record */
   ar_record = FindObject(top, "AVAILABILITY_RESPONSE_RECORD", NULL);

   for (i = 0; i < num_recs; i++)
   {
      if ((ar_record == NULL) || (ar_record == top)) return (ERROR);
      extract_param_value(ar_record, &start, "TIME_ON");
      extract_param_value(ar_record, &stop, "TIME_OFF");
      if (compare_ODLDates (&start, &min_time) < 0)
          min_time = start;
      if (compare_ODLDates (&stop, &max_time) > 0)
          max_time = stop;
      ar_record = ar_record->right_sibling;
   }
   
   WALPS_AR_rec->time_on  = min_time;
   WALPS_AR_rec->time_off = max_time;
   return (OK);
}   /* get_ar_range */



/*==============================================================================
Function: int compare_ODLDates(struct ODLDate *d1, struct ODLDate *d2)

Description:
        This function compares two ODLDates.

Parameters:
        struct ODLDate   *d1
        struct ODLDate   *d2

Returns: 
        -1     - d1 is before d2
         0     - d1 = d2
         1     - d1 is after d2

Creator:         Rodney Hoffman
Creation Date:   November 1996
Notes:
==============================================================================*/
#ifdef __STDC__
int compare_ODLDates (struct ODLDate *d1, struct ODLDate *d2)
#else
int compare_ODLDates(d1, d2)
   struct ODLDate *d1;
   struct ODLDate *d2;
#endif
{
    char   dstr1[17], dstr2[17];    /* yyyydddhhmmsssss */

    sprintf (dstr1, "%04d%03d%02d%02d%02d%03d", 
              d1->year, d1->doy, d1->hours, d1->minutes, d1->seconds, 
              d1->nanoseconds/1000000);

    sprintf (dstr2, "%04d%03d%02d%02d%02d%03d", 
              d2->year, d2->doy, d2->hours, d2->minutes, d2->seconds, 
              d2->nanoseconds/1000000);

    return (strcmp(dstr1, dstr2));

}  /* compare_ODLDates */

/* End of file */
