/*==============================================================================
Filename:	NFSsend.c

Description:	
	This module contains the server end functions for sending local
files to external systems via NFS.

External Functions:
	send_file_via_nfs
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None

Notes:
1.  May '96 - R. Hoffman
    Added the chmod on the destination file in send_file_via_nfs()
==============================================================================*/

static char SccsFile[] = "NFSsend.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "01 May 1996";
static char SccsLastChanger[] = "@(#)NFSsend.c	1.2";
static char SccsState[] = "1.2";

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <syslog.h>
#include "faifdefs.h"
#include "sendfile.h"


#ifdef __STDC__
int                   send_file_via_nfs(File_Transfer_Params *) ;
#else
int                   send_file_via_nfs() ;
#endif


extern void *util_do_malloc() ;
extern int   process_exit() ;



/*==============================================================================
Function:	int send_file_via_nfs()

Description:
	Perform file send via NFS - copy then remove the source file.
This function calls the Unix system function exec to execute a cp and
then a rm of filename into dest_dir.  Note the source and destination
directories are specified via config variables.  The send status is
returned upon send completion.

Parameters:
	sendprm - pointer to File_Transfer_Params record containing
file transfer parameters

Returns:
	ACCEPT - nfs transfer succeeded
	ERROR - transfer failed

Creator:	Norbert Piega	
Creation Date:  06/13/1994	
Notes:		
==============================================================================*/
#ifdef __STDC__
int
send_file_via_nfs(File_Transfer_Params *sendprm)
#else
int
send_file_via_nfs(sendprm)
   File_Transfer_Params *sendprm ;
#endif
{
   pid_t pid;
   int status, statloc, mstatus ;
   char *outname ;
   char *outputfile ;
   char current_dir[MAX_DIRNAME_LEN+1] ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   char fn[MAX_DIRNAME_LEN + MAX_FILENAME_LEN + 1];
   mode_t all_rw;
 
   if (sendprm == (File_Transfer_Params *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL NFS send parameters\n") ;
      return(ERROR) ;
   }
   if (sendprm->src_dir == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL NFS source directory\n") ;
      return(ERROR) ;
   }
   if (sendprm->dest_dir == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL NFS destination directory\n") ;
      return(ERROR) ;
   }
   if (sendprm->out_file == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL NFS send file\n") ;
      return(ERROR) ;
   }

   if (access(sendprm->dest_dir, W_OK) != 0)
   {
      sprintf(logmsg, 
         "WARNING, No write permission on destination directory %s\n",
	  sendprm->dest_dir);
      syslog(LOG_ERR, logmsg) ; 
      return(ERROR) ;
   }

   if (access(sendprm->src_dir, R_OK) != 0)
   {
      sprintf(logmsg, 
        "WARNING, No read permission on source directory %s\n",
         sendprm->src_dir);
      syslog(LOG_ERR, logmsg) ; 
      return(ERROR) ;
   }

   outputfile = 
      (char *)util_do_malloc(sizeof(char)*
                             (strlen(sendprm->src_dir)+
			      strlen(sendprm->out_file)+1+1)) ;
   strcpy(outputfile, sendprm->src_dir) ;
   strcat(outputfile, "/") ;
   strcat(outputfile, sendprm->out_file) ;
   if (access(outputfile, R_OK) != 0)
   {
      sprintf(logmsg, 
        "WARNING, No read permission on source file %s\n", outputfile);
      syslog(LOG_ERR, logmsg) ; 
      return(ERROR) ;
   }

   if ((pid = fork()) < 0)
   {
       syslog(LOG_DEBUG, "WARNING, Error forking NFS transfer\n") ;
       return(ERROR) ;
   }
 
   else if (pid == 0)
   {                    /* child */
      getcwd(current_dir, MAX_DIRNAME_LEN) ;
      chdir(sendprm->src_dir) ;

      outname = (char *)util_do_malloc(sizeof(char)*
				       (strlen(sendprm->dest_dir)+
				        strlen("/.")+1)) ;
      strcpy(outname, sendprm->dest_dir) ;
      strcat(outname, "/.") ;

      if (execl(NFS_SEND_CMD_PATH, NFS_SEND_CMD, 
		sendprm->out_file, outname, (char *) 0) < 0)
      {
         syslog(LOG_DEBUG, "WARNING, Execl NFS transfer error\n");
         free(outname) ;
         chdir(current_dir) ;
         exit(ERROR) ;
      }
      free(outname) ;
      chdir(current_dir) ;
   }
 
   if ((status = waitpid(pid, &statloc, 0)) < 0)  /* parent */
   {
      syslog(LOG_DEBUG, "WARNING, waitpid error\n");
      return(ERROR) ;
   }
 
   process_exit(statloc) ;

   if (WIFEXITED(statloc))
   {
      /* Do chmod on destination file */
      sprintf (fn, "%s/%s", sendprm->dest_dir, sendprm->out_file);
      /* all_rw = 666 = Read/Write by all */
      all_rw = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
      if ((mstatus = chmod(fn, all_rw)) == -1)
      {
        sprintf (logmsg, "WARNING, NSFsend/chmod: %s; File: %s\n", 
                 strerror(errno), fn);
        syslog (LOG_ERR, logmsg);
        return (ERROR);
      }
   }
   else
      printf ("Bad statloc: %d\n", statloc); 

   return(status) ;

} /* send_file_via_nfs */
 

/* End of file */
