/*==============================================================================
Filename:	ASFdt.c

Description:	ASF DT (Datatake Message) file manipulation functions

	This module contains the functions that operate on datatake
message records found in a ASF datatake message file.  It includes 
allocation of data structures, initialization and extraction from the 
input ASF file.

External Functions:
	alloc_ASF_DT_record
	reset_ASF_DT_record
	extract_ASF_DT_record

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
#include "ASFhdr.h"   /* ASF header record typedefs, etc */
#include "ASFdt.h"   /* ASF DT record typedefs, etc */


#ifdef __STDC__
ASF_DT_Record *alloc_ASF_DT_record(void) ;
int            reset_ASF_DT_record(ASF_DT_Record *) ;
int            extract_ASF_DT_record(FILE *, ASF_DT_Record *) ;
ASF_DT_Record *check_DT_msg_file(char *, char *, LOGICAL *) ;
#else
ASF_DT_Record *alloc_ASF_DT_record() ;
int            reset_ASF_DT_record() ;
int            extract_ASF_DT_record() ;
ASF_DT_Record *check_DT_msg_file() ;
#endif

extern void *util_do_malloc() ;


/*==============================================================================
Function:	ASF_DT_Record *alloc_ASF_DT_record(void)

Description:	Allocate and initialize an ASF DT record

	This function allocates an ASF DT record and then
initializes its fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to the allocated ASF DT record or NULL
Creator:	Norbert Piega	
Creation Date:	12/01/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
ASF_DT_Record *
alloc_ASF_DT_record(void)
#else
ASF_DT_Record *
alloc_ASF_DT_record()
#endif
{
   ASF_DT_Record *dt = NULL ;

   dt = (ASF_DT_Record *) util_do_malloc(sizeof(ASF_DT_Record)) ;
   if (dt != (ASF_DT_Record *) NULL)
   {
      *dt->datatake_id = NULL ;
      *dt->activity_id = NULL ;
      *dt->taken_flag = NULL ;
      *dt->media_id = NULL ;
      *dt->start_tape = NULL ;
      *dt->end_tape = NULL ;
      *dt->on_year = NULL ;
      *dt->off_year = NULL ;
      *dt->on_time = NULL ;
      *dt->off_time = NULL ;
      *dt->recorder_id = NULL ;
      *dt->fst_addr = NULL ;
      *dt->fen_addr = NULL ;
   }

   return(dt) ;

} /* alloc_ASF_DT_record */






/*==============================================================================
Function:	int reset_ASF_DT_record(ASF_DT_Record *dt)

Description:	
	This function resets the values of the string fields in the ASF 
DT record structure to NULL.

Parameters:	ASF_DT_Record *dt - pointer to ASF DT record to reset
Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	12/01/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
reset_ASF_DT_record(ASF_DT_Record *dt)
#else
int
reset_ASF_DT_record(dt)
   ASF_DT_Record *dt ;
#endif
{
   if (dt == (ASF_DT_Record *)NULL)
      return(ERROR) ;

   *dt->datatake_id = NULL ;
   *dt->activity_id = NULL ;
   *dt->taken_flag = NULL ;
   *dt->media_id = NULL ;
   *dt->start_tape = NULL ;
   *dt->end_tape = NULL ;
   *dt->on_year = NULL ;
   *dt->off_year = NULL ;
   *dt->on_time = NULL ;
   *dt->off_time = NULL ;
   *dt->recorder_id = NULL ;
   *dt->fst_addr = NULL ;
   *dt->fen_addr = NULL ;

   return(OK) ;

} /* reset_ASF_DT_record */





/*==============================================================================
Function:	int extract_ASF_DT_record(FILE *ASF_fp, ASF_DT_Record *ASF_dt)

Description:	
	This function parses an input ASF file and obtains the data in a
datatake message record portion of the file and stores it in the ASF header
record ASF_dt.

Parameters:
	FILE *ASF_fp - pointer to input ASF file stream
	ASF_DT_Record *ASF_dt - pointer to ASF datatake message record where
datatake record information parsed from the input file are stored

Returns:	
	ACCEPT - able to extract data and store in record
	REJECT - data read failed
	ERROR - error, bad input

Creator:	Norbert Piega	
Creation Date:	12/01/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_ASF_DT_record(FILE *ASF_fp, ASF_DT_Record *ASF_dt)
#else
int
extract_ASF_DT_record(ASF_fp, ASF_dt)
   FILE *ASF_fp ;
   ASF_DT_Record *ASF_dt ;
#endif
{
   char *dtstr ;
   char *newline = NULL ;

   if (ASF_fp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL input file stream in extract_ASF_DT_record\n") ;
      return(ERROR) ;
   }

   dtstr = (char *)util_do_malloc(sizeof(char)*(ASFDT_RECLEN)) ;
   if (fgets(dtstr, (int)(ASFDT_RECLEN+1), ASF_fp) == NULL)
      return(REJECT) ;

   if ((newline = strchr(dtstr, '\n')) != (char *)NULL)
   {
      syslog(LOG_WARNING, 
        "WARNING, Unexpected newline encountered prior to DT record. \
        \nIgnoring newline\n") ;
      if ((int)strlen(dtstr) < ASFDT_RECLEN)
         if (fgets(dtstr, (size_t)ASFDT_RECLEN+1, ASF_fp) == NULL)
            return(REJECT) ;
   }

   strncpy(ASF_dt->datatake_id, dtstr, ASFDT_DATATAKEID) ;
   ASF_dt->datatake_id[ASFDT_DATATAKEID] = '\0' ;
   dtstr += (ASFDT_DATATAKEID+1) ;

   strncpy(ASF_dt->activity_id, dtstr, ASFDT_ACTID) ;
   ASF_dt->activity_id[ASFDT_ACTID] = '\0' ;
   dtstr += (ASFDT_ACTID+1) ;

   strncpy(ASF_dt->taken_flag, dtstr, ASFDT_TAKENFLAG) ;
   ASF_dt->taken_flag[ASFDT_TAKENFLAG] = '\0' ;
   dtstr += (ASFDT_TAKENFLAG+1) ;

   strncpy(ASF_dt->media_id, dtstr, ASFDT_MEDIAID) ;
   ASF_dt->media_id[ASFDT_MEDIAID] = '\0' ;
   dtstr += (ASFDT_MEDIAID+1) ;

   strncpy(ASF_dt->start_tape, dtstr, ASFDT_STARTENDTAPE) ;
   ASF_dt->start_tape[ASFDT_STARTENDTAPE] = '\0' ;
   dtstr += (ASFDT_STARTENDTAPE+1) ;

   strncpy(ASF_dt->end_tape, dtstr, ASFDT_STARTENDTAPE) ;
   ASF_dt->end_tape[ASFDT_STARTENDTAPE] = '\0' ;
   dtstr += (ASFDT_STARTENDTAPE+1) ;

   strncpy(ASF_dt->on_year, dtstr, ASFHDR_YEAR) ;
   ASF_dt->on_year[ASFHDR_YEAR] = '\0' ;
   dtstr += (ASFHDR_YEAR+1) ;

   strncpy(ASF_dt->on_time, dtstr, ASFHDR_TIME) ;
   ASF_dt->on_time[ASFHDR_TIME] = '\0' ;
   dtstr += (ASFHDR_TIME+1) ;

   strncpy(ASF_dt->off_year, dtstr, ASFHDR_YEAR) ;
   ASF_dt->off_year[ASFHDR_YEAR] = '\0' ;
   dtstr += (ASFHDR_YEAR+1) ;

   strncpy(ASF_dt->off_time, dtstr, ASFHDR_TIME) ;
   ASF_dt->off_time[ASFHDR_TIME] = '\0' ;
   dtstr += (ASFHDR_TIME+1) ;

   strncpy(ASF_dt->recorder_id, dtstr, ASFDT_RECORDID) ;
   ASF_dt->recorder_id[ASFDT_RECORDID] = '\0' ;
   dtstr += (ASFDT_RECORDID+1) ;

   strncpy(ASF_dt->fst_addr, dtstr, ASFDT_FSTENADDR) ;
   ASF_dt->fst_addr[ASFDT_FSTENADDR] = '\0' ;
   dtstr += (ASFDT_FSTENADDR+1) ;

   strncpy(ASF_dt->fen_addr, dtstr, ASFDT_FSTENADDR) ;
   ASF_dt->fen_addr[ASFDT_FSTENADDR] = '\0' ;

   return(ACCEPT) ;

} /* extract_ASF_DT_record */





/*==============================================================================
Function:	ASF_DT_Record *check_DT_msg_file(char *DTmsgfile, char *FA,
		   LOGICAL *matched)

Description:	
	This function checks if a file is of a specified type, ie.
if a CSA, ESA or NASDA Datatake message.

Parameters:
	char *DTmsgfile - name of Datatake message file to check
	char *FA - flight agency for which DT message is checked for
	LOGICAL *matched - flag that signals if DT file is from FA

Returns:	pointer to generated DT record or NULL if error
Creator:	Norbert Piega	
Creation Date:	01/11/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
ASF_DT_Record *
check_DT_msg_file(char *DTmsgfile, char *FA, LOGICAL *matched)
#else
ASF_DT_Record *
check_DT_msg_file(DTmsgfile, FA, matched)
   char *DTmsgfile ;
   char *FA ;
   LOGICAL *matched ;
#endif
{
   FILE *infp ;
   ASF_Header_Record *asf_hdr = NULL ;
   ASF_DT_Record *asf_dt = NULL ;

   if ((infp = fopen(DTmsgfile, "r")) == (FILE *)NULL)
   {
      fprintf(stderr, "Can't open DT message file %s\n", DTmsgfile) ;
      return(NULL) ;
   }

   asf_hdr = (ASF_Header_Record *)alloc_ASF_hdr_record() ;
   extract_ASF_hdr_record(infp, asf_hdr) ;

   asf_dt = (ASF_DT_Record *)alloc_ASF_DT_record() ;
   extract_ASF_DT_record(infp, asf_dt) ;

   *matched = FALSE ;
   if (strcmp(FA, CSA_STR) == 0)
   {
      if (strncmp(ASF_SATNAME_RS, asf_dt->datatake_id, 
		 strlen(ASF_SATNAME_RS)) == 0)
         *matched = TRUE ;
   }
   else if (strcmp(FA, ESA_STR) == 0)
   {
      if ((strncmp(ASF_SATNAME_E1, asf_dt->datatake_id,
		 strlen(ASF_SATNAME_E1)) == 0) ||
         (strncmp(ASF_SATNAME_E2, asf_dt->datatake_id,
		 strlen(ASF_SATNAME_E2)) == 0))
         *matched = TRUE ;
   }

   else if (strcmp(FA, NASDA_STR) == 0)
      if (strncmp(ASF_SATNAME_J1, asf_dt->datatake_id,
		 strlen(ASF_SATNAME_J1)) == 0)
         *matched = TRUE ;

   return(asf_dt) ;

} /* check_DT_msg_file */

/* End of file */
