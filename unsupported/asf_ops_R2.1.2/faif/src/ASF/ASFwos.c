/*==============================================================================
Filename:	ASFwos.c

Description:	ASF WOS (Weekly Operations Schedule) file manipulation 
		functions

	This module contains the functions that operate on WOS records
found in a ASF WOS file.  It includes allocation of data structures, 
initialization and extraction from the input ASF file.

External Functions:
	alloc_ASF_WOS_record
	reset_ASF_WOS_record
	extract_ASF_WOS_record

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
#include <syslog.h>
#include "faifdefs.h"
#include "ASF.h"
#include "ASFwos.h"   /* ASF WOS record typedefs, etc */


#ifdef __STDC__
ASF_WOS_Record *alloc_ASF_WOS_record(void) ;
int             reset_ASF_WOS_record(ASF_WOS_Record *) ;
int             extract_ASF_WOS_record(FILE *, ASF_WOS_Record *) ;
#else
ASF_WOS_Record *alloc_ASF_WOS_record() ;
int             reset_ASF_WOS_record() ;
int             extract_ASF_WOS_record() ;
#endif

extern void *util_do_malloc() ;


/*==============================================================================
Function:	ASF_WOS_Record *alloc_ASF_WOS_record(void)

Description:	Allocate and initialize an ASF WOS record

	This function allocates an ASF WOS record and then
initializes its fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to the allocated ASF WOS record or NULL
Creator:	Norbert Piega	
Creation Date:	10/24/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
ASF_WOS_Record *
alloc_ASF_WOS_record(void)
#else
ASF_WOS_Record *
alloc_ASF_WOS_record()
#endif
{
   ASF_WOS_Record *wos = NULL ;

   wos = (ASF_WOS_Record *) util_do_malloc(sizeof(ASF_WOS_Record)) ;
   if (wos != (ASF_WOS_Record *) NULL)
   {
      *wos->datatake_id = NULL ;
      *wos->activity_id = NULL ;
      *wos->on_year = NULL ;
      *wos->off_year = NULL ;
      *wos->on_time = NULL ;
      *wos->off_time = NULL ;
      *wos->trans_id = NULL ;
      *wos->site_name = NULL ;
      *wos->mode = NULL ;
      *wos->spare = NULL ;
   }

   return(wos) ;

} /* alloc_ASF_WOS_record */






/*==============================================================================
Function:	int reset_ASF_WOS_record(ASF_WOS_Record *wos)

Description:	
	This function resets the values of the string fields in the ASF 
WOS record structure to NULL.

Parameters:	ASF_WOS_Record *wos - pointer to ASF WOS record to reset
Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	10/27/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
reset_ASF_WOS_record(ASF_WOS_Record *wos)
#else
int
reset_ASF_WOS_record(wos)
   ASF_WOS_Record *wos ;
#endif
{
   if (wos == (ASF_WOS_Record *)NULL)
      return(ERROR) ;

   *wos->datatake_id = NULL ;
   *wos->activity_id = NULL ;
   *wos->on_year = NULL ;
   *wos->off_year = NULL ;
   *wos->on_time = NULL ;
   *wos->off_time = NULL ;
   *wos->trans_id = NULL ;
   *wos->site_name = NULL ;
   *wos->mode = NULL ;
   *wos->spare = NULL ;

   return(OK) ;

} /* reset_ASF_WOS_record */





/*==============================================================================
Function:	int extract_ASF_WOS_record(FILE *ASF_fp,
					   ASF_WOS_Record *ASF_wos)
Description:	
	This function parses an input ASF file and obtains the data in the
common header portion of the file in the ASF header record ASF_wos.

Parameters:
	FILE *ASF_fp - pointer to input ASF file stream
	ASF_WOS_Record *ASF_wos - pointer to ASF header record where
header information parsed from the input file are stored

Returns:	
	ACCEPT - able to extract data and store in record
	REJECT - data read failed
	ERROR - error, bad input

Creator:	Norbert Piega	
Creation Date:	10/24/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_ASF_WOS_record(FILE *ASF_fp, ASF_WOS_Record *ASF_wos)
#else
int
extract_ASF_WOS_record(ASF_fp, ASF_wos)
   FILE *ASF_fp ;
   ASF_WOS_Record *ASF_wos ;
#endif
{
   char *wosstr ;
   char *newline = NULL ;

   if (ASF_fp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL input file stream in extract_ASF_WOS_record\n") ;
      return(ERROR) ;
   }

   wosstr = (char *)util_do_malloc(sizeof(char)*(ASFWOS_RECLEN)) ;
   if (fgets(wosstr, (size_t)ASFWOS_RECLEN+1, ASF_fp) == NULL)
      return(REJECT) ;

   if ((newline = strchr(wosstr, '\n')) != (char *)NULL)
   {
      syslog(LOG_WARNING, 
        "WARNING, Unexpected newline encountered prior to WOS record. \
        \nIgnoring newline\n") ;
      if ((int)strlen(wosstr) < ASFWOS_RECLEN)
         if (fgets(wosstr, (size_t)ASFWOS_RECLEN+1, ASF_fp) == NULL)
            return(REJECT) ;
   }

   strncpy(ASF_wos->datatake_id, wosstr, ASFWOS_DATATAKEID) ;
   ASF_wos->datatake_id[ASFWOS_DATATAKEID] = '\0' ;
   wosstr += (ASFWOS_DATATAKEID+1) ;

   strncpy(ASF_wos->activity_id, wosstr, ASFWOS_ACTID) ;
   ASF_wos->activity_id[ASFWOS_ACTID] = '\0' ;
   wosstr += (ASFWOS_ACTID+1) ;

   strncpy(ASF_wos->on_year, wosstr, ASFHDR_YEAR) ;
   ASF_wos->on_year[ASFHDR_YEAR] = '\0' ;
   wosstr += (ASFHDR_YEAR+1) ;

   strncpy(ASF_wos->on_time, wosstr, ASFHDR_TIME) ;
   ASF_wos->on_time[ASFHDR_TIME] = '\0' ;
   wosstr += (ASFHDR_TIME+1) ;

   strncpy(ASF_wos->off_year, wosstr, ASFHDR_YEAR) ;
   ASF_wos->off_year[ASFHDR_YEAR] = '\0' ;
   wosstr += (ASFHDR_YEAR+1) ;

   strncpy(ASF_wos->off_time, wosstr, ASFHDR_TIME) ;
   ASF_wos->off_time[ASFHDR_TIME] = '\0' ;
   wosstr += (ASFHDR_TIME+1) ;

   strncpy(ASF_wos->trans_id, wosstr, ASFWOS_TRANSID) ;
   ASF_wos->trans_id[ASFWOS_TRANSID] = '\0' ;
   wosstr += (ASFWOS_TRANSID+1) ;

   strncpy(ASF_wos->site_name, wosstr, ASFWOS_SITENAME) ;
   ASF_wos->site_name[ASFWOS_SITENAME] = '\0' ;
   wosstr += (ASFWOS_SITENAME+1) ;

   strncpy(ASF_wos->mode, wosstr, ASFWOS_MODE) ;
   ASF_wos->mode[ASFWOS_MODE] = '\0' ;
   wosstr += (ASFWOS_MODE) ;

   strncpy(ASF_wos->spare, wosstr, ASFWOS_SPARE) ;
   ASF_wos->spare[ASFWOS_SPARE] = '\0' ;

   return(ACCEPT) ;

} /* extract_ASF_WOS_record */


/* End of file */
