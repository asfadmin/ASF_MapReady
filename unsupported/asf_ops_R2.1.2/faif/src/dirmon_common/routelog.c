/*==============================================================================
Filename:	routelog.c

Description:
	This module contains the update_log function which is used by
the routing routine to add new log records to the a log file.

External Functions:
	check_if_logged
	update_log
	
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

#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>
#include "filerec.h"
#include "dapps_list.h"
#include "nmalloc.h"
#include "ESA.h"
#include "NASDA.h"
#include "CSA.h"
#include "WALPS.h"
#include "ESAftypestr.h"
#include "NASDAftypestr.h"
#include "CSAftypestr.h"
#include "WALPSftypestr.h"
#include "ADEOSftypestr.h"


#ifdef __STDC__
int update_log(char *, File_Record *) ;
int check_if_logged(llist *, char *) ;
#else
int update_log() ;
int check_if_logged() ;
#endif

/* Tables of file type strings to be written to the log file
*/
extern File_Identifier ESA_FileTypeStr_Table[] ;
extern File_Identifier NASDA_FileTypeStr_Table[] ;
extern File_Identifier CSA_FileTypeStr_Table[] ;
extern File_Identifier WALPS_FileTypeStr_Table[] ;
extern File_Identifier ADEOS_FileTypeStr_Table[] ;



/*==============================================================================
Function:	int check_if_logged(llist *filerec_llist, char *logfile)

Description:	
	This function checks if each file in the file record is already
listed in the log file.  If a file is already listed, the logged_flag
field of the corresponding file record is marked TRUE.

Parameters:
	llist *filerec_llist - file record list to check
	char *logfile - name of log file to be check for filenames

Returns:	status of the check
Creator:	Norbert Piega	
Creation Date:	08/09/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int check_if_logged(llist *filerec_llist, char *logfile)
#else
int check_if_logged(filerec_llist, logfile)
   llist *filerec_llist;
   char *logfile ;
#endif
{
   FILE *logfp ;
   int count ;
   File_Record *filerecp ;
   cursor ptr ;
   char inline[MAXLINE+1] ;
   char *start ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* First, check if a log file already exists
   -- If it does, open it for reading
   */
   if (access(logfile, F_OK) == 0)
   {
      if ((logfp = fopen(logfile, "r")) == (FILE *)NULL)
      {
	 sprintf(logmsg, "WARNING, Unable to read config file %s\n", logfile) ;
	 syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
      }
   }

   /* If no log file, file records logged flag are FALSE by default
   -- Just return OK
   */
   else
      return(OK) ;

   /* Check through files in file list
   */
   count = 0 ;
   filerecp = FIRST(filerec_llist, ptr) ; 
   while (filerecp) 
   {
      count++ ;

      /* Check if file is listed in the log file
      */
      while (fgets(inline, MAXLINE, logfp) != NULL)
      {
         /* Skip blanks 
         */
         start = inline ;
         while (isspace(*start))
            start++ ;
			    
	 /* Skip BLANK & COMMENT line 
	 */
	 if (*start == '\0' || *start == '#')
            continue ;

	 /* Find filerecp->file_name in inline 
	 */
	 if (strstr(start, filerecp->file_name) != (char *)NULL)
	 {
            filerecp->logged_flag = TRUE ;
            sprintf(logmsg, 
	       "WARNING, File %s is already listed in the log file %s\n",
                filerecp->file_name, 
	        logfile) ;
/*	    syslog(LOG_WARNING, logmsg) ; */
         }

      } /* endwhile */

      rewind(logfp) ;

      filerecp = NEXT(filerec_llist, ptr)  ;

   } /* endwhile not end of linked list */

   fclose(logfp) ;

   return(OK) ;

} /* check_if_logged */





/*==============================================================================
Function:	update_log(log_file, filerec)

Description:	
	Open the log file using fopen
   
	Read the log file using fread
	If filename is listed in log_file (file is already logged)
	   Continue
	Else
	   Add line in log_file for filename using fwrite
	Endif
	    
	Close the log_file using fclose
		       
	Return ERROR if an error was encountered, otherwise return OK.

Parameters:
	filename - name of file (part of file info stored 
in a file record structure)

	log_file - file path for the log file.  A log of all
files that are detected in the incoming directory
is generated and updated as necessary.
	The log is a simple flat file listing the names of the 
incoming files as they are found by the monitor.  Other related 
information may be added to this log such as file type, time
created, flight agency, etc.

Returns:	
	OK - update log succeeded
	ERROR - error encountered

Creator:	Norbert Piega	
Creation Date:	08/04/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
update_log(char *log_file, File_Record *filerec)
#else
int
update_log(log_file, filerec)
   char *log_file ;
   File_Record *filerec ;
#endif
{
   FILE *logfp ;
   char logmsg[2*MAX_SYSLOG_MSGLEN+1] ;
   char *file_id_str = NULL ;
   int i = SENTINEL ;

   if (access(log_file, F_OK) == 0)
   {
      if (filerec->logged_flag == TRUE)
      {
         sprintf(logmsg,
	    "WARNING, %s is already listed in log file. \
	    \nAdding new entry\n", filerec->file_name) ;
/*         syslog(LOG_WARNING, logmsg) ; */
      }

      if ((logfp = fopen(log_file, "a")) == (FILE *)NULL)
      {
         sprintf(logmsg, "WARNING, Unable to open log file %s\n", log_file) ;
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
      }
   }

   else
   {
      syslog(LOG_NOTICE, 
	 "NOTICE: Log file does not already exist. Creating one.\n") ;
      if ((logfp = fopen(log_file, "w")) == (FILE *)NULL)
      {
         sprintf(logmsg, "WARNING, Unable to open log file %s\n", log_file) ;
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
      }
   }
   sprintf(logmsg, "NOTICE: Adding entry in log file %s.\n", log_file);
/*   syslog(LOG_NOTICE, logmsg) ; */

   
   /* Add entry in log
   */
   fprintf(logfp, "%s %s %s %s ",
	 filerec->file_name,
	 filerec->time_received,
	 filerec->time_forwarded,
	 filerec->flight_agency) ;

   if (strcmp(filerec->flight_agency, ESA_STR) == 0)
   {
      for (i=0; ESA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
         if (ESA_FileTypeStr_Table[i].file_id_number == 
	     filerec->file_type_status)
         {
	    fprintf(logfp, "%s", ESA_FileTypeStr_Table[i].file_identifier) ;
	    file_id_str = ESA_FileTypeStr_Table[i].file_identifier ;
            break ;
         }
   }

   else if (strcmp(filerec->flight_agency, NASDA_STR) == 0)
   {
      for (i=0; NASDA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
         if (NASDA_FileTypeStr_Table[i].file_id_number == 
	     filerec->file_type_status)
         {
	    fprintf(logfp, "%s", NASDA_FileTypeStr_Table[i].file_identifier) ;
	    file_id_str = NASDA_FileTypeStr_Table[i].file_identifier ;
            break ;
         }
   }

   else if (strcmp(filerec->flight_agency, CSA_STR) == 0)
   {
      for (i=0; CSA_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
         if (CSA_FileTypeStr_Table[i].file_id_number == 
	     filerec->file_type_status)
         {
	    fprintf(logfp, "%s", CSA_FileTypeStr_Table[i].file_identifier) ;
	    file_id_str = CSA_FileTypeStr_Table[i].file_identifier ;
            break ;
         }
   }

   else if (strcmp(filerec->flight_agency, WALPS_STR) == 0)
   {
      for (i=0; WALPS_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
         if (WALPS_FileTypeStr_Table[i].file_id_number == 
	     filerec->file_type_status)
         {
	    fprintf(logfp, "%s", WALPS_FileTypeStr_Table[i].file_identifier) ;
	    file_id_str = WALPS_FileTypeStr_Table[i].file_identifier ;
            break ;
         }
   }

   else if (strcmp(filerec->flight_agency, ADEOS_STR) == 0)
   {
      for (i=0; ADEOS_FileTypeStr_Table[i].file_id_number != SENTINEL; i++)
         if (ADEOS_FileTypeStr_Table[i].file_id_number == 
	     filerec->file_type_status)
         {
	    fprintf(logfp, "%s", ADEOS_FileTypeStr_Table[i].file_identifier) ;
	    file_id_str = ADEOS_FileTypeStr_Table[i].file_identifier ;
            break ;
         }
   }

   if (i != SENTINEL)
   {
      fprintf(logfp, "\n") ;
      sprintf(logmsg, 
	 "NOTICE: Appended the ff. to log file %s: %s %s %s %s %s\n",
         log_file,
	 filerec->file_name,
	 filerec->time_received,
	 filerec->time_forwarded,
	 filerec->flight_agency,
	 file_id_str) ;
/*      syslog(LOG_NOTICE, logmsg) ;*/
   }
   else
   {
      fprintf(logfp, "UNRECOGNIZED\n") ;
      sprintf(logmsg, 
	 "NOTICE: Appended the ff. to log file %s: %s %s %s %s UNRECOGNIZED\n",
         log_file,
	 filerec->file_name,
	 filerec->time_received,
	 filerec->time_forwarded,
	 filerec->flight_agency) ;
/*      syslog(LOG_NOTICE, logmsg) ;*/
   }


   fclose(logfp) ;
   return(OK) ;

} /* update_log */

/* End of File */
