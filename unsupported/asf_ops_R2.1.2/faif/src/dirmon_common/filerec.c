/*==============================================================================
Filename:	filerec.c

Description:
	This module contains the general state vector data type
manipulation routines.  It includes the allocation of a file record
and the initialization and resetting of its field values.

External Functions:
	alloc_file_record
	assign_filerec_ftime 
	assign_filerec_ftype 
	assign_filerec_rtime 
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  March '96 - R. Hoffman
    Fix off-by-one bug in our use of tm_yday in assign_filerec_ftime() and
    assign_filerec_rtime().
==============================================================================*/

static char SccsFile[] = "filerec.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "18 Mar 1996";
static char SccsLastChanger[] = "@(#)filerec.c	1.2";
static char SccsState[] = "1.2";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <syslog.h>
#include "filerec.h"
#include "dapps_list.h"
#include "nmalloc.h"


#ifdef __STDC__
File_Record *alloc_file_record(void) ;
int          assign_filerec_ftime(File_Record *) ;
int          assign_filerec_rtime(File_Record *) ;
#else
File_Record *alloc_file_record() ;
int          assign_filerec_ftime() ;
int          assign_filerec_rtime() ;
#endif


extern void *util_do_malloc() ;


/*==============================================================================
Function:	int alloc_file_record(File_Record *)

Description:
	Allocate and Initialize a file record data structure.  This
function allocates a file record structure and initializes its fields.
If the allocation fails, NULL is returned, otherwise a pointer to the
allocated structure is returned.

Parameters:	None
Returns:	pointer to newly allocated structure or NULL if failed
Creator:	Norbert L. Piega / JPL
Creation Date:	08/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
File_Record *
alloc_file_record(void)
#else
File_Record *
alloc_file_record()
#endif
{
   File_Record *filerec = NULL ;

   filerec = (File_Record *) NEW(sizeof(File_Record)) ;
   if (filerec != (File_Record *)NULL)
   {
      *filerec->file_name = NULL ;
      filerec->file_type_status = -1 ;
      *filerec->file_dest = NULL ;
      filerec->routed_flag = FALSE ;
      filerec->logged_flag = FALSE ;
      *filerec->time_received = NULL ;
      *filerec->time_forwarded = NULL ;
      filerec->translated_flag = FALSE ;
      filerec->flight_agency = NULL ;
      filerec->next_filerec = NULL ;
   }

   return(filerec) ;

} /* init_file_record */
 




/*==============================================================================
Function:	int assign_filerec_ftime(File_Record *filerecp) 

Description:	
	Assign the forwarded time field of a file record.  Use the current
time as the value of the forwarded time.

Parameters:
	filerecp - pointer to the file record to update

Returns:
	OK - assignment succeeded
	ERROR - error assigning forwarded time

Creator:	Norbert Piega	
Creation Date:	08/08/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
assign_filerec_ftime(File_Record *filerecp) 
#else
int
assign_filerec_ftime(filerecp) 
   File_Record *filerecp ; 
#endif
{
   time_t clockt ;
   struct tm result ;
   char time_str[MAXLINE] ;

   time(&clockt) ;
   localtime_r(&clockt, &result) ;

   /* Handle years beyond 2000
   */
   /* Note that tm_yday is days *since* Jan. 1.  Add 1 to it. */
   if (result.tm_year < 100)
      sprintf(time_str, "19%02d:%03d:%02d:%02d:%02d.000", 
         result.tm_year, result.tm_yday+1, result.tm_hour,
         result.tm_min, result.tm_sec) ;
   else
      sprintf(time_str, "2%03d:%03d:%02d:%02d:%02d.000", 
         result.tm_year-100, result.tm_yday+1, result.tm_hour,
         result.tm_min, result.tm_sec) ;
   strcpy(filerecp->time_forwarded, time_str) ; 

   return(OK) ;

} /* assign_filerec_ftime */






/*==============================================================================
Function:	int assign_filerec_rtime(File_Record *filerecp)

Description:	
	This function assigns the received time field of a file record.
The time assigned is the accessed time of the file from its stat
structure.  The stat structure is obtained by calling the stat system
call.

Parameters:
	filerecp - pointer to the file record to be modified

Returns:	
	OK - assignment succeeded
	ERROR - error assigning forwarded time

Creator:	Norbert Piega	
Creation Date:	08/08/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
assign_filerec_rtime(File_Record *filerecp)
#else
int
assign_filerec_rtime(filerecp)
   File_Record *filerecp ;
#endif
{
   struct stat statbuf ;
   struct tm result ;
   char time_str[MAXLINE] ;

   /* Obtain file stats
   */
   stat(filerecp->file_name, &statbuf) ;

   /* Obtain struct tm containing statbuf time
   */
   localtime_r(&(statbuf.st_mtime), &result) ;

   /* Create time string
   -- Handle years 2000 and above
   */
   /* Note that tm_yday is days *since* Jan. 1.  Add 1 to it. */
   if (result.tm_year < 100)
      sprintf(time_str, "19%02d:%03d:%02d:%02d:%02d.000",
         result.tm_year, result.tm_yday+1, result.tm_hour,
         result.tm_min, result.tm_sec) ;
   else
      sprintf(time_str, "2%03d:%03d:%02d:%02d:%02d.000",
         result.tm_year-100, result.tm_yday+1, result.tm_hour,
         result.tm_min, result.tm_sec) ;

   /* Copy time string to time received field
   */
   strcpy(filerecp->time_received, time_str) ;

   return(OK) ;

} /* assign_filerec_rtime */

/* End of file */
