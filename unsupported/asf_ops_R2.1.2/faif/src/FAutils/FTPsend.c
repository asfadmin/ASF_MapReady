/*==============================================================================
Filename:	FTPsend.c

Description:	
	This module contains the server end functions for sending local
files to external systems via FTP.

External Functions:
	send_file_via_ftp
	get_file_via_ftp
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None

Notes:
==============================================================================*/

static char SccsFile[] = "FTPsend.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "04 Sep 1996";
static char SccsLastChanger[] = "@(#)FTPsend.c	1.3";
static char SccsState[] = "1.3";

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <syslog.h>
#include "faifdefs.h"
#include "sendfile.h"
#include "getupass.h"


#ifdef __STDC__
int                   send_file_via_ftp(File_Transfer_Params *) ;
int                   get_file_via_ftp(File_Transfer_Params *) ;
#else
int                   send_file_via_ftp() ;
int                   get_file_via_ftp() ;
#endif


extern void *util_do_malloc() ;
extern int   process_exit() ;
extern char *get_FA_upass() ;



/*==============================================================================
Function:	int send_file_via_ftp(File_Transfer_Params *sendprm)

Description:
	Perform file send via FTP.  This function calls the Unix system
function exec to execute a specified FTP script.  Note the FTP parameters
are obtained from the function parameter sendprm.  The FTP script used is
searched in the FAIF binpath directory which must be specified via an
environment variable.  A password file that has to be decrypted is also
searched in a directory designated for password files.  Note that the
password file directory along with other FAIF directories is located
under the FAIF root path (the FAIF root directory).

Parameters:
	sendprm - pointer to File_Transfer_Params record containing
file transfer parameters

Returns:
	ACCEPT - file transfer succeeded
	REJECT - file transfer failed
	ERROR - error in input or execution of transfer

Creator:	Norbert Piega	
Creation Date:  06/13/1994	
Notes:		
==============================================================================*/
#ifdef __STDC__
int
send_file_via_ftp(File_Transfer_Params *sendprm)
#else
int
send_file_via_ftp(sendprm)
   File_Transfer_Params *sendprm ;
#endif
{
   pid_t pid, status ;
   int statloc ;
   char *srcpath = NULL ;
   char *scriptpath = NULL ;
   char current_dir[MAX_DIRNAME_LEN+1] ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   char *comma, *ups ;
   char *upassfile = NULL ;
   char *username, *password ;


   if (sendprm == (File_Transfer_Params *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTPsend parameters\n") ;
      return(ERROR) ;
   }
   if (sendprm->dest_host == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTPsend destination host\n") ;
      return(ERROR) ;
   }
   if (sendprm->dest_dir == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTPsend destination directory\n") ;
      return(ERROR) ;
   }
   if (sendprm->out_file == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTPsend out file\n") ;
      return(ERROR) ;
   }
   if (sendprm->mode == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTPsend transfer mode\n") ;
      return(ERROR) ;
   }
   if (sendprm->send_cmd == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTPsend script name\n") ;
      return(ERROR) ;
   }
   if (sendprm->rootpath == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL rootpath name in FTPsend script call\n") ;
      return(ERROR) ;
   }
   if (sendprm->user == (char *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL password filepath in FTPsend script call\n") ;
      return(ERROR) ;
   }

   srcpath = getenv(FAIF_BINPATH_EV) ;
   if (srcpath == (char *)NULL)
   {
      sprintf(logmsg, 
         "WARNING, environment variable %s not set.  Using default %s\n",
	  FAIF_BINPATH_EV, FAIF_BINPATH_DEF) ;
      syslog(LOG_WARNING, logmsg) ;

      srcpath = (char *)util_do_malloc(sizeof(char)*
         (strlen(FAIF_BINPATH_DEF)+1)) ;
      strcpy(srcpath, FAIF_BINPATH_DEF) ;
   }

   if (((int)strlen((char *)FTPSEND_SCRIPT_SUBPATH)) > 0)
   {
      scriptpath = (char *)util_do_malloc(sizeof(char)*
         (strlen(srcpath) + 
	  strlen((char *)FTPSEND_SCRIPT_SUBPATH) +
	  strlen(sendprm->send_cmd) + 3)) ;
      strcpy(scriptpath, srcpath) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, FTPSEND_SCRIPT_SUBPATH) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, sendprm->send_cmd) ;
   }
   else
   {
      scriptpath = (char *)util_do_malloc(sizeof(char)*
	 (strlen(srcpath) + strlen(sendprm->send_cmd) + 2)) ;
      strcpy(scriptpath, srcpath) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, sendprm->send_cmd) ;
   }
   if (access(scriptpath, F_OK) != 0)
   {
      sprintf(logmsg, 
         "WARNING, FTP send script %s does not exist\n", scriptpath);
      syslog(LOG_ERR, logmsg) ; 

      free(scriptpath) ;
      return(ERROR) ;
   }
   if (access(scriptpath, X_OK) != 0)
   {
      sprintf(logmsg, 
         "WARNING, No execute permission for FTP send script %s\n",
	  scriptpath);
      syslog(LOG_ERR, logmsg) ; 

      free(scriptpath) ;
      return(ERROR) ;
   }

   upassfile = (char *)util_do_malloc(sizeof(char)*
      (strlen(sendprm->rootpath)+1 + strlen(FAIF_FTPUPW_DIR)+1 +
       strlen(sendprm->user) + 1)) ;
   strcpy(upassfile, sendprm->rootpath) ;
   strcat(upassfile, "/") ;
   strcat(upassfile, FAIF_FTPUPW_DIR) ;
   strcat(upassfile, "/") ;
   strcat(upassfile, sendprm->user) ;
   if (access(upassfile, R_OK) != 0)
   {
      sprintf(logmsg, 
         "WARNING, No read permission for FTPsend password file %s\n",
	  upassfile);
      syslog(LOG_ERR, logmsg) ; 

      free(upassfile) ;
      return(ERROR) ;
   }

   if ((ups = get_FA_upass(upassfile)) == (char *)NULL)
   {
      sprintf(logmsg, 
         "WARNING, Unable to decrypt FTPsend password file %s\n",
	  upassfile);
      syslog(LOG_ERR, logmsg) ; 

      free(upassfile) ;
      return(ERROR) ;
   }

   comma = strchr(ups, ',') ;
   if (comma == (char *)NULL)
   {
      sprintf(logmsg, 
         "WARNING, Invalid decrypted data from FTPsend password file %s\n",
	  upassfile);
      syslog(LOG_ERR, logmsg) ; 

      free(upassfile) ;
      free(ups) ;
      return(ERROR) ;
   }
   *comma = '\0' ;
   username = ups ;
   password = comma+1 ;

   /* Send file via ftp script to approp. out dir.
   */
   if ((pid = fork()) < 0)
   {
       syslog(LOG_DEBUG, "WARNING, Error forking FTPsend\n") ;
       return(ERROR) ;
   }

   else if (pid == 0)
   {                    /* child */
      getcwd(current_dir, MAX_DIRNAME_LEN) ;
      chdir(sendprm->src_dir) ;
 
      if (execl(scriptpath, sendprm->send_cmd, 
		sendprm->dest_host,
		sendprm->dest_dir, 
		sendprm->out_file, 
		sendprm->mode, 
		sendprm->rootpath, 
		username, 
		password, (char *) 0) < 0)
      {
         syslog(LOG_DEBUG, "WARNING, FTPsend Execl error\n");
	 chdir(current_dir) ;
         exit(ERROR) ;
      }
      chdir(current_dir) ;
   }
   free(scriptpath) ;
   free(upassfile) ;
   free(ups);
 
   if ((status = waitpid(pid, &statloc, 0)) < 0)  /* parent */
   {
      syslog(LOG_DEBUG, "WARNING, Waitpid FTPsend error\n");
      return(ERROR) ;
   }

   process_exit(statloc) ;
   if (WIFEXITED(statloc))
	 statloc = WEXITSTATUS(statloc) ;

   switch (statloc)
   {
      case 0:
	 sprintf(logmsg, "NOTICE: Sucessfully transferred %s to %s\n",
	    sendprm->out_file, sendprm->dest_host);
         syslog(LOG_NOTICE, logmsg) ;
         return(ACCEPT) ;
	 break ;
      case 1:
	 /* This case is for usage error.  Occurs in interactive mode only 
	 */
         syslog(LOG_ERR, "CRITICAL, Unexpected script usage error\n") ;
	 return(ERROR) ;
	 break ;
      case 2:
	 sprintf(logmsg, "CRITICAL, Unable to login to remote host %s\n",
	    sendprm->dest_host);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      case 3:
	 sprintf(logmsg,
	    "CRITICAL, Unable to locate remote directory or Vax logical %s\n",
	    sendprm->dest_dir);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      case 4:
	 sprintf(logmsg, "CRITICAL, %s transferred incorrectly to %s\n",
	    sendprm->out_file, sendprm->dest_host);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      case 5:
	 sprintf(logmsg,
	    "WARNING, Error while checking transfer. Unable to pull %s from %s\n",
	    sendprm->out_file, sendprm->dest_host) ;
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      default:
         syslog(LOG_ERR, 
	    "CRITICAL, Unexpected error status from FTP send script\n") ;
	 return(ERROR) ;
   }

} /* send_file_via_ftp */





/*==============================================================================
Function:	int get_file_via_ftp(File_Transfer_Params *sendprm)

Description:
	Perform file get via FTP.  This function calls the Unix system
function exec to execute a specified FTP script.  The FTP parameters
are obtained from the function parameter sendprm.  The FTP script used
is searched in the FAIF binpath directory which must be specified via an
environment variable.  A password file that has to be decrypted is also
searched in a directory designated for password files.  Note that the
password file directory along with other FAIF directories is located
under the FAIF root path (the FAIF root directory).

Parameters:
	sendprm - pointer to File_Transfer_Params record containing
file transfer parameters

Returns:
	ACCEPT - file transfer succeeded
	REJECT - file transfer failed
	ERROR - error in input or execution of transfer

Creator:	Norbert Piega	
Creation Date:  06/13/1994	
Notes:		
==============================================================================*/
#ifdef __STDC__
int
get_file_via_ftp(File_Transfer_Params *sendprm)
#else
int
get_file_via_ftp(sendprm)
   File_Transfer_Params *sendprm ;
#endif
{
   pid_t pid;
   int status, statloc , temp_int;
   char *srcpath = NULL ;
   char *scriptpath = NULL ;
   char current_dir[MAX_DIRNAME_LEN+1] ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   char *comma, *ups ;
   char *upassfile = NULL ;
   char *username, *password ;

   if (sendprm == (File_Transfer_Params *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get parameters\n") ;
      return(ERROR) ;
   }
   if (sendprm->src_host == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get source host\n") ;
      return(ERROR) ;
   }
   if (sendprm->src_dir == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get source directory\n") ;
      return(ERROR) ;
   }
   if (sendprm->dest_dir == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get local destination directory\n") ;
      return(ERROR) ;
   }
   if (sendprm->out_file == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get file\n") ;
      return(ERROR) ;
   }
   if (sendprm->mode == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get transfer mode\n") ;
      return(ERROR) ;
   }
   if (sendprm->send_cmd == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL FTP get script name\n") ;
      return(ERROR) ;
   }
   if (sendprm->rootpath == (char *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL root path in FTP get script call\n") ;
      return(ERROR) ;
   }
   if (sendprm->user == (char *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL password filepath in FTP get script call\n") ;
      return(ERROR) ;
   }

   srcpath = getenv(FAIF_BINPATH_EV) ;
   if (srcpath == (char *)NULL)
   {
      sprintf(logmsg, 
         "WARNING, environment variable %s not set.  Using default %s\n",
	  FAIF_BINPATH_EV, FAIF_BINPATH_DEF) ;
      syslog(LOG_WARNING, logmsg) ;

      srcpath = (char *)util_do_malloc(sizeof(char)*
         (strlen(FAIF_BINPATH_DEF)+1)) ;
      strcpy(srcpath, FAIF_BINPATH_DEF) ;
   }

   if (((int)strlen((char *)FTPGET_SCRIPT_SUBPATH)) > 0)
   {
      scriptpath = (char *)util_do_malloc(sizeof(char)*
         (strlen(srcpath) + 
          strlen((char *)FTPGET_SCRIPT_SUBPATH) +
	  strlen(sendprm->send_cmd) + 3)) ;
      strcpy(scriptpath, srcpath) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, FTPGET_SCRIPT_SUBPATH) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, sendprm->send_cmd) ;
   } 
   else
   {
      scriptpath = (char *)util_do_malloc(sizeof(char)*
         (strlen(srcpath) + strlen(sendprm->send_cmd) + 2)) ;
      strcpy(scriptpath, srcpath) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, sendprm->send_cmd) ;
   }

   if (access(scriptpath, X_OK) != 0)
   {
      sprintf(logmsg, 
         "WARNING, No execute permission for FTP get script %s\n",
	  scriptpath);
      syslog(LOG_ERR, logmsg) ; 
      free(scriptpath) ;
      return(ERROR) ;
   }

   upassfile = (char *)util_do_malloc(sizeof(char)*
      (strlen(sendprm->rootpath)+1 + strlen(FAIF_FTPUPW_DIR)+1 +
       strlen(sendprm->user) + 1)) ;
   strcpy(upassfile, sendprm->rootpath) ;
   strcat(upassfile, "/") ;
   strcat(upassfile, FAIF_FTPUPW_DIR) ;
   strcat(upassfile, "/") ;
   strcat(upassfile, sendprm->user) ;
   if (access(upassfile, R_OK) != 0)
   {
      sprintf(logmsg, 
         "WARNING, No read permission for FTPsend password file %s\n",
	  upassfile);
      syslog(LOG_ERR, logmsg) ; 

      free(upassfile) ;
      return(ERROR) ;
   }

   if ((ups = get_FA_upass(upassfile)) == (char *)NULL) 
   {
      sprintf(logmsg, 
         "WARNING, Unable to decrypt FTPsend password file %s\n",
	  upassfile);
      syslog(LOG_ERR, logmsg) ; 

      free(upassfile) ;
      return(ERROR) ;
   }

   comma = strchr(ups, ',') ;
   if (comma == (char *)NULL)
   {
      sprintf(logmsg, 
         "WARNING, Invalid decrypted data from FTPsend password file %s\n",
	  upassfile);
      syslog(LOG_ERR, logmsg) ; 

      free(upassfile) ;
      return(ERROR) ;
   }
   *comma = '\0' ;
   username = ups ;
   password = comma+1 ;

   /* Get file via ftp script to approp. out dir.
   */
   if ((pid = fork()) < 0)
   {
       syslog(LOG_DEBUG, "WARNING, Error forking FTP get\n") ;
       return(ERROR) ;
   }

   else if (pid == 0)
   {                    /* child */
      getcwd(current_dir, MAX_DIRNAME_LEN) ;
      chdir(sendprm->dest_dir) ;

      if (execl(scriptpath, sendprm->send_cmd, 
		sendprm->src_host,
		sendprm->src_dir, 
		sendprm->dest_dir, 
		sendprm->out_file, 
		sendprm->mode, 
		sendprm->rootpath,
		username,
		password,
		(char *) 0) < 0)
      {
         syslog(LOG_DEBUG, "WARNING, FTP Execl error\n");
	 chdir(current_dir) ;
         exit(ERROR) ;
      }
      chdir(current_dir) ;
   }
   free(upassfile) ;
   free(scriptpath) ;
 
   if ((status = waitpid(pid, &statloc, 0)) < 0)  /* parent */
   {
      syslog(LOG_DEBUG, "WARNING, Waitpid FTP error\n");
      return(ERROR) ;
   }

   process_exit(statloc) ;
   if (WIFEXITED(statloc)) statloc = WEXITSTATUS(statloc) ;

   switch (statloc)
   {
      case 0:
	 sprintf(logmsg,
	    "NOTICE: Normal termination obtaining or checking for %s file types from %s\n",
	    sendprm->out_file, sendprm->src_host);
         syslog(LOG_NOTICE, logmsg) ;
         return(ACCEPT) ;
	 break ;
      case 1:
	 /* This case is for usage error.  Occurs in interactive mode only 
	 */
         syslog(LOG_ERR, "CRITICAL, Unexpected script usage error\n") ;
         return(ERROR) ;
	 break ;
      case 2:
	 sprintf(logmsg, "CRITICAL, Unable to login to remote host %s\n",
	    sendprm->src_host);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      case 3:
	 sprintf(logmsg, "CRITICAL, Unable to locate remote directory %s\n",
	    sendprm->src_dir);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      case 4:
	 sprintf(logmsg,
	    "WARNING, Unable to obtain remote host %s file listing\n",
	    sendprm->src_host);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      case 5:
	 sprintf(logmsg,
	    "WARNING, Unable to obtain files from remote host %s\n",
	    sendprm->src_host);
         syslog(LOG_ERR, logmsg) ;
         return(ERROR) ;
	 break ;
      default:
         syslog(LOG_ERR, 
	    "CRITICAL, Unexpected error status from FTP get script\n") ;
	 return(ERROR) ;
   }

} /* get_file_via_ftp */


/* End of file */
