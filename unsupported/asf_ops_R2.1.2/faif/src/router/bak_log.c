/*==============================================================================
Filename:	bak_log.c

Description:
	This module contains the update_bak_log function which is used by
        the routing routine to add new log records to the a log file.

External Functions:
	check_bak_logged
	update_bak_log
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  July '97 - R. Hoffman
    removed unneeded logged_flag parameter from check_bak_logged()
==============================================================================*/

static char SccsFile[] = "bak_log.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "11/13/96";
static char SccsLastChanger[] = "@(#)bak_log.c	1.1";
static char SccsState[] = "1.1";

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

#ifdef __STDC__
int update_bak_log(char *, char *, char *, char *) ;
int check_bak_logged(char *, char *) ;
#else
int update_bak_log() ;
int check_bak_logged() ;
#endif

/*==============================================================================
Function:	int check_bak_logged(char *file_name, char *logfile)

Description:	
	This function checks if each file in the file record is already
        listed in the log file. 

Returns:	status of the check, either ERROR, TRUE, or FALSE
Creator:	Rich Norman
Creation Date:	96/11/02

==============================================================================*/
#ifdef __STDC__
int check_bak_logged(char *file_name, char *logfile)
#else
int check_bak_logged(file_name, logfile)
   char *file_name;
   char *logfile ;
#endif
{
   FILE *logfp ;
   cursor ptr ;
   char inline[MAXLINE+1] ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int   logged_flag;

   /* Check if log file already exists. If it does, open it for reading. */

   if (access(logfile, F_OK) == 0)
   {
      if ((logfp = fopen(logfile, "r")) == (FILE *)NULL)
      {
	 sprintf(logmsg, "WARNING, Unable to read log file %s\n", logfile) ;
	 syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
      }
   }

   /* If no log file, file records logged flag are FALSE by default */
   else
   {
      return(FALSE) ;
   }

   /* Check if file is listed in the log file. */

   logged_flag = FALSE;

   while (fgets(inline, MAXLINE, logfp) != NULL)

   {
      if (strstr(inline, file_name) != (char *)NULL)
      {
         logged_flag = TRUE ;
         break;
      }

   } /* endwhile */

   fclose(logfp) ;

   return(logged_flag) ;

} /* check_bak_logged */

/*==============================================================================

Function:    update_bak_log(log_file, file_name)

Description: Add file names to log as they are copied to destination directory
                
Returns:	
	OK - update log succeeded
	ERROR - error encountered

Creator:	Rich Norman
Creation Date:	96/11/02

==============================================================================*/

#ifdef __STDC__

int
update_bak_log(char *log_file, char *file_name,
                char *time_received, char *time_forwarded)
#else
int
update_bak_log(log_file, file_name)
   char *log_file ;
   char *file_name;
   char *time_received;
   char *time_forwarded;
#endif
{
   FILE *logfp ;
   char logmsg[2*MAX_SYSLOG_MSGLEN+1] ;

   if (access(log_file, F_OK) == 0)
   {

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
   fprintf(logfp, "%s %s %s\n", file_name, time_received, time_forwarded);

   fclose(logfp) ;

   return(OK) ;

} /* update_bak_log */

/* End of File */
