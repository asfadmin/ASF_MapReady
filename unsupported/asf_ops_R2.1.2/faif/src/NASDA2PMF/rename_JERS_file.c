/*==============================================================================
Filename:	rename_JERS_file.c

Description:
	This module contains the function used to append creation time 
        to a JERS-1 filename.

External Functions:
        rename_JERS_file 
	
Static Functions:
	None
	
External Variables Defined:
        ascii_file

File Scope Static Variables:
	None
	
Notes:
        rename_JERS_file was extracted from NASDA2PMF.c so it could be used
        before gen_NASDA_PMF().
==============================================================================*/

static char SccsFile[] = "rename_JERS_file.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "19 Jan 1997";
static char SccsLastChanger[] = "@(#)rename_JERS_file.c	1.0";
static char SccsState[] = "1.0";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>
#include <time.h>
#include "GENconversions.h"
#include "faifdefs.h"
#include "NASDA.h"
#include "NASDA2PMF.h"

#define  FA_ASCII_REC_PROCESS_OK		 0
#define  FA_ASCII_REC_INGESTION_OK		 0

#ifdef __STDC__
void rename_JERS_file (char *, char *, char *);
#else
void rename_JERS_file();
#endif

/* Global variables */
char          file_creation_time[] =  "yyyy-dddThh:mm:ss.uuu";
char          fa_create_time[] =      "hh:mm:ss";
char          fa_create_date[] =      "yyyymmdd";
char          asf_creation_time[] =   "yyyy:ddd:hh:mm:ss.uuu";
char          fa_file_type_temp[] =   "ffff";

   VALUE_DEFS ascii_file[] =  
   {
#ifdef TEMPLATE
     {FILE_HEADER,	REPORT_CONTROL,	 0, 0, function, equiv_table, dest},
     {FILE_HEADER,	REPORT_HEADER,	 0, 0, function, equiv_table, dest},
     {FILE_HEADER,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},

     {FILE_RECORD,	REPORT_HEADER,	 0, 0, function, equiv_table, dest},
     {FILE_RECORD,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
     {FA_DEFAULT,	REPORT_RECORD,	 0, 0, function, equiv_table, dest},
#endif
     {FILE_HEADER, REPORT_RECORD, 0, 4, 
        gen_string2str, NULL, (int) fa_file_type_temp},
     {FILE_HEADER, REPORT_HEADER, 20, 8, 
        gen_string2str, NULL, (int) fa_create_date},
     {FILE_HEADER, REPORT_HEADER, 28, 8, 
        gen_string2str, NULL, (int) fa_create_time},
     {0, 0, -1, -1, NULL, NULL, NULL} 
   };                                                           

extern void *util_do_malloc() ;



/*==============================================================================
Function:	int rename_JERS_file(char * orig_filename, char * new_filename);
Description:	Append creation time to filename
Parameters:     char  *path           of the file
                char  *orig_filename  the NASDA-supplied 4-letter filename
                char  *new_filename   the new filename
Returns:	(none)
Creator:	Rodney Hoffman
Creation Date:	June 1997
Notes:          Extracted from NASDA2PMF.c
==============================================================================*/

void rename_JERS_file (char *path, char *orig_filename, char *new_filename)
{

   char          fullpathname[MAXLINE];
   char          filedate[22];
   char          new_fullpathname[MAXLINE];
   FILE          *ascii_file_ptr ;
   int           status_rename;

   sprintf(fullpathname, "%s/%s", path, orig_filename) ;

   /* Parse the header of the NASDA file */
   if (fa_ascii_rec_ingestion(fullpathname, 
				   ascii_file, 
				   HEADER_RECORD_SIZE, 
				   DATA_RECORD_SIZE, 
				   &ascii_file_ptr) != 0)
   {
      syslog(LOG_ERR, "ERROR: Unable to parse header (rename_JERS_file()).\n");
      return;
   }

   fclose(ascii_file_ptr);

   /* Time conversions */
   tc_yyyymmdd_hhmmss2asf(fa_create_date, fa_create_time,asf_creation_time);
   tc_asf2odl (asf_creation_time, file_creation_time);

   /* If the orig_filename is still the one NASDA supplied, it's four characters
      long.  In that case, append the file creation date to the name. */
   if (strlen(orig_filename) == 4)
   {
      strncpy (filedate, file_creation_time, 21);
      filedate[21] = '\0';
      sprintf (new_filename, "%s_%s", orig_filename, filedate);
      sprintf (new_fullpathname, "%s_%s", fullpathname, filedate);
      status_rename = rename (fullpathname, new_fullpathname);
      if (status_rename != 0)
      {
         syslog (LOG_ERR, "WARNING: Unable to rename JERS-1 file to: %s.\n", 
                  new_fullpathname);
         return;
      }
   }
}

/* End of File */
