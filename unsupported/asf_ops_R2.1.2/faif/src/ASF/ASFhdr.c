/*==============================================================================
Filename:	ASFhdr.c

Description:	ASF file header record creation/operation functions

	This module contains the ASF file header data type manipulation
routines.  It includes allocation of data structures, initialization
and writing to an output file for creation of an ASF file.

External Functions:
	alloc_ASF_hdr_record
	write_ASF_hdr_record
	extract_ASF_hdr_record
	assign_ASF_hdr_values

Static Functions:
	None

External Variables Defined:
	None

File Scope Static Variables:
	None
	
Notes:
1.  March '96 - R. Hoffman
    Fix off-by-one bug in our use of tm_yday in assign_ASF_hdr_values()
==============================================================================*/

static char SccsFile[] = "ASFhdr.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "18 Mar 1996";
static char SccsLastChanger[] = "@(#)ASFhdr.c	1.2";
static char SccsState[] = "1.2";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include "faifdefs.h"
#include "ASFhdr.h"


#ifdef __STDC__
ASF_Header_Record *alloc_ASF_hdr_record(void) ;
int                write_ASF_hdr_record(FILE *, ASF_Header_Record *) ;
int                extract_ASF_hdr_record(FILE *, ASF_Header_Record *) ;
int                assign_ASF_hdr_values(ASF_Header_Record *, char *,
		      char *, char *, char *) ;
#else
ASF_Header_Record *alloc_ASF_hdr_record() ;
int                write_ASF_hdr_record() ;
int                extract_ASF_hdr_record() ;
int                assign_ASF_hdr_values() ;
#endif

extern void *util_do_malloc() ;


/*==============================================================================
Function:	ASF_Header_Record *alloc_ASF_hdr_record(void)

Description:	Allocate and initialize an ASF header record

	This function allocates an ASF header record and then
initializes its fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to the allocated ASF header record or NULL
Creator:	Norbert Piega	
Creation Date:	07/26/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
ASF_Header_Record *
alloc_ASF_hdr_record(void)
#else
ASF_Header_Record *
alloc_ASF_hdr_record()
#endif
{
   ASF_Header_Record *hdr = NULL ;

   hdr = (ASF_Header_Record *) util_do_malloc(sizeof(ASF_Header_Record)) ;
   if (hdr != (ASF_Header_Record *) NULL)
   {
      *hdr->year = NULL ;
      *hdr->time = NULL ;
      *hdr->msg_type = NULL ;
      *hdr->dest = NULL ;
      *hdr->origin = NULL ;
      *hdr->spare = NULL ;
   }

   return(hdr) ;

} /* alloc_ASF_hdr_record */





/*==============================================================================
Function:	int write_ASF_hdr_record(FILE *outfp, ASF_Header_Record *hdr)

Description:	
	Write out the contents of the ASF header record to the output file
stream outfp.

Parameters:
	FILE *outfp - pointer to output file stream
	ASF_Header_Record *hdr - pointer to header record to be written

Returns:	
	ACCEPT - header was written successfully
	REJECT - NULL header record fields 
	ERROR - NULL header record or NULL file pointer

Creator:	Norbert Piega	
Creation Date:	08/01/1994
Notes:		
	The format of the ASF header record is documented in the ASF 
Software Interface Specification, JPL-D5267.
==============================================================================*/
#ifdef __STDC__
int 
write_ASF_hdr_record(FILE *outfp, ASF_Header_Record *hdr)
#else
int 
write_ASF_hdr_record(outfp, hdr)
   FILE *outfp ;
   ASF_Header_Record *hdr ;
#endif
{
   int i ;

   if (hdr == (ASF_Header_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL ASF header input in ASF header write\n");
      return(ERROR) ;
   }

   if (outfp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL file input writing ASF header\n");
      return(ERROR) ;
   }

   if (*(hdr->year) != (char)NULL)
   {
      fprintf(outfp, "%s", hdr->year) ;
      for (i=1; i<=ASFHDR_YEAR+1-(int)strlen(hdr->year); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   if (*hdr->time != (char)NULL)
   {
      fprintf(outfp, "%s", hdr->time) ;
      for (i=1; i<=ASFHDR_TIME+1-(int)strlen(hdr->time); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   if (*hdr->msg_type != (char)NULL)
   {
      fprintf(outfp, "%s", hdr->msg_type) ;
      for (i=1; i<=ASFHDR_MSGT+1-(int)strlen(hdr->msg_type); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   if (*hdr->dest != (char)NULL)
   {
      fprintf(outfp, "%s", hdr->dest) ;
      for (i=1; i<=ASFHDR_DEST+1-(int)strlen(hdr->dest); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   if (*hdr->origin != (char)NULL)
   {
      fprintf(outfp, "%s", hdr->origin) ;
      for (i=1; i<=ASFHDR_ORGN+1-(int)strlen(hdr->origin); i++)
	 fprintf(outfp, " ") ;
   }
   else
      return(REJECT) ;

   /* Write spare bytes also
   */
   for (i=1; i<=ASFHDR_SPARE; i++)
	 fprintf(outfp, " ") ;

   return(ACCEPT) ;

} /* write_ASF_hdr_record */
 




/*==============================================================================
Function:	int extract_ASF_hdr_record(FILE *ASF_fp,
					   ASF_Header_Record *ASF_hdr)
Description:	
	This function parses an input ASF file and obtains the data in the
common header portion of the file in the ASF header record ASF_hdr.

Parameters:
	FILE *ASF_fp - pointer to input ASF file stream
	ASF_Header_Record *ASF_hdr - pointer to ASF header record where
header information parsed from the input file are stored

Returns:	
	ACCEPT - able to extract header info and store in record
	REJECT - data read failed
	ERROR - error, bad input

Creator:	Norbert Piega	
Creation Date:	10/24/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_ASF_hdr_record(FILE *ASF_fp, ASF_Header_Record *ASF_hdr)
#else
int
extract_ASF_hdr_record(ASF_fp, ASF_hdr)
   FILE *ASF_fp ;
   ASF_Header_Record *ASF_hdr ;
#endif
{
   char *hdrstr ;

   if (ASF_fp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL input file stream in extract_ASF_hdr_record\n") ;
      return(ERROR) ;
   }

   hdrstr = (char *)util_do_malloc(sizeof(char)*(ASFHDR_LEN)) ;
   if (fgets(hdrstr, (size_t)ASFHDR_LEN+1, ASF_fp) == NULL) 
      return(REJECT) ;

   strncpy(ASF_hdr->year, hdrstr, ASFHDR_YEAR) ;
   ASF_hdr->year[ASFHDR_YEAR] = '\0' ;
   hdrstr += (ASFHDR_YEAR+1) ;

   strncpy(ASF_hdr->time, hdrstr, ASFHDR_TIME) ;
   ASF_hdr->time[ASFHDR_TIME] = '\0' ;
   hdrstr += (ASFHDR_TIME+1) ;

   strncpy(ASF_hdr->msg_type, hdrstr, ASFHDR_MSGT) ;
   ASF_hdr->msg_type[ASFHDR_MSGT] = '\0' ;
   hdrstr += (ASFHDR_MSGT+1) ;

   strncpy(ASF_hdr->dest, hdrstr, ASFHDR_DEST) ;
   ASF_hdr->dest[ASFHDR_DEST] = '\0' ;
   hdrstr += (ASFHDR_DEST+1) ;

   strncpy(ASF_hdr->origin, hdrstr, ASFHDR_ORGN) ;
   ASF_hdr->origin[ASFHDR_ORGN] = '\0' ;
   hdrstr += (ASFHDR_ORGN+1) ;

   strncpy(ASF_hdr->spare, hdrstr, ASFHDR_SPARE) ;
   ASF_hdr->spare[ASFHDR_SPARE] = '\0' ;
   hdrstr += (ASFHDR_SPARE+1) ;

   return(ACCEPT) ;

} /* extract_ASF_hdr_record */






/*==============================================================================
Function:	int assign_ASF_hdr_values(ASF_Header_Record *hdr,
		   char *ftype, char *source, char *dest, char *spare)

Description:	
	Assign the field values for the ASF header record specified.
The header record field values assigned are the year, time, message
type, destination, origin, and spare field.  The year and time values
are derived from the current time while the other values are set
from values passed from the parameters.

Parameters:
	ASF_Header_Record *hdr - pointer to the header record whose
field values will be assigned
	char *ftype - type of file/message
	char *source - file source id
	char *dest - file destination id

Returns:	
	ERROR - if hdr is NULL
	OK - upon assignment of values

Creator:	Norbert Piega	
Creation Date:	08/01/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
assign_ASF_hdr_values(ASF_Header_Record *hdr, char *ftype, 
                      char *source, char *dest, char *spare)
#else
int
assign_ASF_hdr_values(hdr, ftype, source, dest, spare)
   ASF_Header_Record *hdr ; 
   char *ftype ;
   char *source ;
   char *dest ;
   char *spare ;
#endif
{
   time_t clockt ;
   struct tm result ;
   char time_str[100] ;

   if (hdr == (ASF_Header_Record *)NULL)
      return(ERROR) ;

   time(&clockt) ;
   (struct tm *)localtime_r(&clockt, &result) ;

   /* Handle years from beyond 2000
   */
   if (result.tm_year < 100)
      sprintf(time_str, "19%02d", result.tm_year) ;
   else
      sprintf(time_str, "2%03d", result.tm_year - 100) ;
   strcpy(hdr->year, time_str) ;

   /* Note that tm_yday is days *since* Jan. 1.  Need to add 1 to it.  */
   sprintf(time_str, "%03d:%02d:%02d:%02d.000", 
         result.tm_yday+1, result.tm_hour,
         result.tm_min, result.tm_sec) ;
   strcpy(hdr->time, time_str) ; 

   if (ftype != (char *)NULL)
      strcpy(hdr->msg_type, ftype) ;
   else
      return(ERROR) ;

   if (source != (char *)NULL)
      strcpy(hdr->origin, source) ;
   else
      return(ERROR) ;

   if (dest != (char *)NULL)
      strcpy(hdr->dest, dest) ;
   else
      return(ERROR) ;

   if (spare != (char *)NULL)
      strcpy(hdr->spare, spare) ;
   else
      return(ERROR) ;

   return(OK) ;

} /* assign_ASF_hdr_values */


/* End of file */
