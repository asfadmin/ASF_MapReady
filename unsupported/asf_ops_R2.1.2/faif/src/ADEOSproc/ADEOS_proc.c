/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:       ADEOS_proc.c

Description:    Contains the functions to process ADEOS mail exchanges.

External Functions:
	get_ADEOS_file
	create_ADEOS_msg
	parse_ADEOS_msg
	check_ADEOS_msg
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  May '96 - R. Hoffman
    a.  Corrected printed status for "Sender site problem" in check_ADEOS_msg()
    b.  Added filename_only and relpath in check_ADEOS_msg()
    c.  Added full_out_dir and subject_re in check_ADEOS_msg()
2.  Sept. 96 - R. Hoffman
    a. Added chmod on temp mail files
    b. Cleaned up ROOTPATH errors and syslog messages
==============================================================================*/

static char SccsFile[] = "ADEOS_proc.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "20 May 1996";
static char SccsLastChanger[] = "@(#)ADEOS_proc.c	1.3";
static char SccsState[] = "1.3";

#include <unistd.h>
#include <stdio.h>
#include <syslog.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "faifdefs.h"
#include "sendfile.h"
#include "ADEOS.h"
#include "ADEOS_msg.h"
#include "ADEOSroute_upw.h"
#include "filerec.h"
#include "dapps_list.h"

extern llist *check_dir() ;
extern char  *get_config_val() ;
extern int    get_file_via_ftp() ;
extern void  *util_do_malloc() ;
extern void   process_exit() ;

char *get_ADEOS_file(char *, char *) ;
int create_ADEOS_msg(MESSAGEPTR, char *, char *, int, char *, char) ;
int check_ADEOS_msg(MESSAGEPTR, char *, char *) ;
MESSAGEPTR parse_ADEOS_msg(char *) ;
MESSAGEPTR alloc_ADEOS_msg_rec(void) ;





/*==============================================================================
Function:	alloc_ADEOS_msg_rec
Description:	Allocate and initialize a new MESSAGE record 
Parameters:	None
Returns:	Pointer to newly allocated record	
Creator:	Norbert Piega
Creation Date:	Sun Oct 29 16:47:26 PST 1995
Notes:		
==============================================================================*/
MESSAGEPTR alloc_ADEOS_msg_rec(void)
{
   MESSAGEPTR msg ;

   msg = (MESSAGEPTR)util_do_malloc(sizeof(MESSAGE)) ;
   *msg->msg_type = NULL ;
   *msg->filename = NULL ;
   *msg->file_size = NULL ;
   *msg->computerid = NULL ;
   *msg->spare = NULL ;
   msg->next = NULL ;

   return(msg) ;

} /* alloc_ADEOS_msg_rec */




/*==============================================================================
Function:       get_ADEOS_file
Description:    Takes the linked list of messages, and for each one, creates a 
                parameter structure to get passed to get_file_via_ftp.  
Parameters:     MESSAGEPTR head - head of linked list
Returns:        "0" for success or "2" for error
Creator:        Philip Yurchuk
Creation Date:  05/30/1995
Notes:
==============================================================================*/
char *get_ADEOS_file(char *filename, char *config_file)
{
  File_Transfer_Params params ;
  File_Transfer_Params *sendprm ;
  llist *destdir_flist ;
  File_Record * rfilerecp ;
  cursor rptr ;
  char realdir[MAXLINE] ;
  char *last_slash = NULL ;
  int status ;
  int adeos_ftype ;

  /* Check first if file is needed
  -- If not, spare field must be set to "1" for "Not Used"
  -- This will be put in the body of the email response to ADEOS
  */

/* Not needed at this time... following code skips "some" types of input files.
   adeos_ftype = parse_ADEOS_filename(filename) ; 
  if (adeos_ftype == ADEOS_RPLN || 
      adeos_ftype == ADEOS_ORST || 
      adeos_ftype == ADEOS_TMDF) 
     return("1") ;
 */

  /* Otherwise, file is needed and should be FTPed in
  */
  sendprm = &params ;
  sendprm->src_host = (char *)get_config_val(config_file, ADEOS_SRCHOST_EV) ;
  sendprm->src_dir = (char *)get_config_val(config_file, ADEOS_SRCDIR_EV) ;
  sendprm->dest_dir = (char *)get_config_val(config_file, ADEOS_RECEPTDIR_EV) ; 
  sendprm->out_file = filename ;
  sendprm->mode = ASCII ;
  sendprm->send_cmd = (char *)get_config_val(config_file, ADEOS_TRANSPROTO_EV) ;
  sendprm->rootpath = (char *) getenv ("FAIF_ROOTPATH");
  sendprm->user = ADEOSROUTE_UPW_FILE ;

  if (sendprm->rootpath == (char *)NULL)
  {
     syslog(LOG_ERR, "ERROR, FAIF_ROOTPATH unknown. Exiting.\n") ;
     return("2") ;
  }

  /* Get files from ADEOS host.  Deposit files obtained in the
  -- reception directory, sendprm->dest_dir
  */
  status = get_file_via_ftp(sendprm) ;
  if (status == ERROR)
  {
     syslog(LOG_ERR,
            "ERROR, Error encountered while getting files from ADEOS.\n") ;
     return("2") ;
  }

  /* Check through files in reception directory
  -- Did we get the file we were supposed to get? (sendprm->out_file)
  */
  last_slash = (char *)strrchr(sendprm->out_file, '/') ;
  if (last_slash != (char *)NULL)
     last_slash++ ;
  else
     last_slash = sendprm->out_file ;

  destdir_flist = (llist *) check_dir(sendprm->dest_dir) ;
  if (destdir_flist != (llist *)NULL)
  {
     rfilerecp = FIRST(destdir_flist, rptr) ;
     while (rfilerecp)
     {
        if (strcmp(last_slash, rfilerecp->file_name) == 0)
           return("0") ;
        rfilerecp = NEXT(destdir_flist, rptr) ;
     }
  }

  return("2") ;

} /* get_ADEOS_file */




/*==============================================================================
Function:       create_ADEOS_msg
Description:    Creates the receipt message and sends it to the FA.
Parameters:     MESSAGEPTR head - head of linked list
                char *subject - subject of the message
		status[] - an integer array of the status
Returns:        ERROR or ACCEPT
Creator:        Philip Yurchuk
Creation Date:  05/30/1995
Notes:
==============================================================================*/
int create_ADEOS_msg(MESSAGEPTR head, char *address, char *subject, 
		     int get_flag, char *configfile, char subject_mtype)
{
  MESSAGEPTR temp ;
  pid_t pid;
  FILE *fp ;
  char *arguments[10] ;
  char *tmpfile = NULL ;
  char *rootpath = NULL ;
  char *script = NULL ;
  char *scriptpath = NULL ;
  char *srcpath = NULL ;
  char logmsg[MAX_SYSLOG_MSGLEN+1] ;
  int status, mstatus ;
  int statloc;
  mode_t all_rw;

  /* Get rootpath and name of mail script from config file 
  */
  rootpath = (char *) getenv ("FAIF_ROOTPATH");
  if (rootpath == (char *)NULL)
  {
     syslog(LOG_ERR, "ERROR, FAIF_ROOTPATH unknown.  Exiting.\n") ;
     return (ERROR);
  }

  script = (char *)get_config_val(configfile, ADEOS_MAILSCRIPT_EV) ;
  if (script == (char *)NULL)
  {
     syslog(LOG_WARNING, 
	"WARNING, Mail script file name unknown.  Using default %s\n", 
	ADEOS_MAILSCRIPT_DEF) ;
     script =
	(char *)util_do_malloc(sizeof(char)*(strlen(ADEOS_MAILSCRIPT_DEF)+1)) ;
     strcpy(script, ADEOS_MAILSCRIPT_DEF) ;
  }

  /* Temporary mail file is in $FAIF_ROOTPATH/...
  */
  tmpfile = (char *)util_do_malloc(sizeof(char)*(strlen(rootpath)+1 +
						 strlen(ADEOS_TMPMAIL_DIR)+1 +
						 strlen(ADEOS_MSG_TMPFILE)+1)) ;
  strcpy(tmpfile, rootpath) ;
  strcat(tmpfile, "/") ;
  strcat(tmpfile, ADEOS_TMPMAIL_DIR) ;
  strcat(tmpfile, "/") ;
  strcat(tmpfile, ADEOS_MSG_TMPFILE) ;

  if ((fp = fopen(tmpfile, "w")) == (FILE *)NULL)
  {
      syslog(LOG_ERR, "WARNING, Unable to open temp file %s\n", tmpfile) ;
      free(tmpfile) ;
/*      free(rootpath) ; */
      free(script) ;
      return(ERROR) ;
  }

  /* Do chmod on the temp mail file */
  /* all_rw = 666 = Read/Write by all */
  all_rw = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  if ((mstatus = chmod(tmpfile, all_rw)) == -1)
  {
    sprintf (logmsg, "WARNING, create_ADEOS_msg/chmod: %s; File: %s\n", 
             strerror(errno), tmpfile);
    syslog (LOG_ERR, logmsg);
  }

  fprintf(fp, "<BOM>\n") ;

  /* For each file in mail msg., get via ftp
  */
  temp = head ;
  while (temp)
  {
     if (get_flag == 1)
     {
        strcpy(temp->spare, get_ADEOS_file(temp->filename, configfile)) ;

        fprintf(fp, "%s>%s>%s>%s>%s>\n", 
             temp->msg_type, 
	     temp->filename, 
	     temp->file_size, 
	     temp->computerid, 
	     temp->spare) ;
     }
     else
        fprintf(fp, "%s>%s>%s>%s>>\n", 
             temp->msg_type, 
	     temp->filename, 
	     temp->file_size, 
	     temp->computerid) ; 

     temp = temp->next ;
  }
  fprintf(fp, "<EOM>\n") ;
  fclose(fp) ;

  /* Get FAIF bin path from environment
  */
  srcpath = (char *)getenv(FAIF_BINPATH_EV) ;
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
 
  /* Form name of mail script path
  */
  scriptpath = (char *)util_do_malloc(sizeof(char)*
     (strlen(srcpath) + 1 + strlen(script) + 1)) ;
  strcpy(scriptpath, srcpath) ;
  strcat(scriptpath, "/") ;
  strcat(scriptpath, script) ;
 
  /* Check script permissions
  */
  if (access(scriptpath, F_OK) != 0)
  {
     sprintf(logmsg, 
         "ERROR, Mail script %s does not exist.  Exiting.\n", scriptpath);
     syslog(LOG_ERR, logmsg) ; 
 
     free(tmpfile) ;
/*     free(rootpath) ; */
     free(script) ;
     free(scriptpath) ;
     return(ERROR) ;
  }
  if (access(scriptpath, X_OK) != 0)
  {
     sprintf(logmsg, 
        "ERROR, No execute permission for mail script %s.  Exiting.\n",
          scriptpath);
     syslog(LOG_ERR, logmsg) ; 
 
     free(tmpfile) ;
/*     free(rootpath) ; */
     free(script) ;
     free(scriptpath) ;
     return(ERROR) ;
  }

  /* Mail subject message type should be "Receipt Acknowledgement"
  */
  subject[5] = subject_mtype ;

  /* Spawn mail script process
  */
  if ((pid = fork()) < 0)
    {
      syslog(LOG_ERR, "ERROR, Error forking NFS transfer.  Exiting.\n") ;
      free(tmpfile) ;
/*      free(rootpath) ; */
      free(script) ;
      free(scriptpath) ;
      return(ERROR) ;
    }
  
  else if (pid == 0)
    {                    /* child */
      execl(scriptpath, script, subject, address, tmpfile, rootpath, NULL) ;
    }

  if ((status = waitpid(pid, &statloc, 0)) < 0) 
    {
      syslog(LOG_ERR, "ERROR, Waitpid FTPsend error.  Exiting.\n");
      free(tmpfile) ;
/*      free(rootpath) ; */
      free(script) ;
      free(scriptpath) ;
      return(ERROR) ;
    }
 
  if (remove(tmpfile) < 0)
      syslog(LOG_WARNING, 
	 "WARNING, Unable to remove temporary mail file %s\n", tmpfile) ;

  process_exit(statloc) ;
  if (WIFEXITED(statloc))
    statloc = WEXITSTATUS(statloc) ;
  
  switch (statloc)
    {
    case 0:
      if (subject_mtype == 'I')
         syslog(LOG_NOTICE,"NOTICE: Successfully sent Data Notification mail to ADEOS\n") ;
      else if (subject_mtype == 'R')
         syslog(LOG_NOTICE,"NOTICE: Successfully sent Receipt Reply mail to ADEOS\n") ;
      free(tmpfile) ;
/*      free(rootpath) ; */
      free(script) ;
      free(scriptpath) ;
      return(ACCEPT) ;
      break ;
    case 1:
      syslog(LOG_ERR, "CRITICAL:  Error in Reply to ADEOS\n") ;
      free(tmpfile) ;
/*      free(rootpath) ; */
      free(script) ;
      free(scriptpath) ;
      return(ERROR) ;
      break ;
    default:
      syslog(LOG_ERR, "Unexpected exit status from %s\n", scriptpath);
      free(tmpfile) ;
/*      free(rootpath) ; */
      free(script) ;
      free(scriptpath) ;
      return(ERROR) ;
      break;

    }  /* End switch */

} /* create_ADEOS_msg */




/*==============================================================================
Function:       parse_ADEOS_msg
Description:    Parses the ADEOS message into a message node, that will be put
                into a linked list.
Parameters:     char msg[] - the ADEOS message string
Returns:        MESSAGEPTR temp - a pointer to a message node
Creator:        Philip Yurchuk
Creation Date:  05/30/1995
Notes:
==============================================================================*/
MESSAGEPTR parse_ADEOS_msg(char msg[])
{
  MESSAGEPTR temp;
  char *tempstr ;

/* use strtok to pull out the parameters from the message, and put them into
-- a node
*/
  if (!msg)
    return NULL ;
  
  temp = (MESSAGEPTR)util_do_malloc(sizeof(struct msg_node)) ;

  if (tempstr = (char *)strtok(msg, ">"))
    strcpy(temp->msg_type, tempstr) ;
  if (tempstr = (char *)strtok(NULL, ">"))
    strcpy(temp->filename, tempstr) ;
  if (tempstr = (char *)strtok(NULL, ">"))
    strcpy(temp->file_size, tempstr) ;
  if (tempstr = (char *)strtok(NULL, ">"))
    strcpy(temp->computerid, tempstr) ;
  if (tempstr = (char *)strtok(NULL, ">"))
    strcpy(temp->spare, tempstr) ;     /* most likely will be NULL */
  temp->next = NULL ;

  return temp ;

} /* parse_ADEOS_msg */




/*==============================================================================
Function:       check_ADEOS_msg
Description:    Checks to see which files ADEOS got.
Parameters:     MESSAGEPTR head - head of linked list
                char *subject - subject of the message
		status[] - an integer array of the status
Returns:        
Creator:        Cameron Cooper
Creation Date:  08/15/95
Notes:
==============================================================================*/
int check_ADEOS_msg(MESSAGEPTR head, char *subject, char *configfile)
{
  FILE *fp ;
  int status ;
  MESSAGEPTR temp ;
  pid_t pid;
  char * address ;
  char *tmpfile = NULL ;
  char *remfile = NULL ;
  char *filename_only = NULL;
  char *last_slash = NULL;
  char relpath[MAX_DIRNAME_LEN];
  char *rootpath = NULL ;
  char *out_dir = NULL;
  char full_out_dir[MAX_DIRNAME_LEN];
  char *script = NULL ;
  char *scriptpath = NULL ;
  char *srcpath = NULL ;
  char logmsg[MAX_SYSLOG_MSGLEN+1] ;
  int statloc, mstatus;
  mode_t all_rw;
  int errors;
  llist *outdir_flist ;
  File_Record * rfilerecp ;
  cursor rptr ;
  char spare_stat_msg[MAXLINE];
  char subject_re[MAXLINE];
  char in_dir[10] ;

  syslog(LOG_DEBUG, "In function check_ADEOS_msg");

  /* Get rootpath, name of mail script, out directory and operator 
     from config file. 
  */
  rootpath = (char *) getenv ("FAIF_ROOTPATH");
  script = (char *)get_config_val(configfile, ADEOS_MAILSCRIPT_EV) ;
  out_dir = (char *)get_config_val(configfile, ADEOS_OUTDIR_EV) ;
  address =  (char *)get_config_val(configfile, ADEOS_OP_USER_EV) ;

  if (rootpath == (char *)NULL)
  {
     syslog(LOG_ERR, "ERROR, FAIF_ROOTPATH unknown.  Exiting.\n") ;
     return (ERROR);
  }

  /* Temporary mail file is in $FAIF_ROOTPATH/...
  */
  tmpfile = (char *)util_do_malloc(sizeof(char)*(strlen(rootpath)+1 +
						 strlen(ADEOS_TMPMAIL_DIR)+1 +
						 strlen(ADEOS_RCPT_TMPFILE)+1)) ;
  strcpy(tmpfile, rootpath) ;
  strcat(tmpfile, "/") ;
  strcat(tmpfile, ADEOS_TMPMAIL_DIR) ;
  strcat(tmpfile, "/") ;
  strcat(tmpfile, ADEOS_RCPT_TMPFILE) ;

  if ((fp = fopen(tmpfile, "w")) == (FILE *)NULL)
  {
      syslog(LOG_ERR, "ERROR, Unable to open temp file %s.  Exiting.\n", 
             tmpfile) ;
      return(ERROR) ;
  }

  /* Do chmod on the temp mail file */
  /* all_rw = 666 = Read/Write by all */
  all_rw = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
  if ((mstatus = chmod(tmpfile, all_rw)) == -1)
  {
    sprintf (logmsg, "WARNING, check_ADEOS_msg/chmod: %s; File: %s\n", 
             strerror(errno), tmpfile);
    syslog (LOG_ERR, logmsg);
  }

  fprintf(fp, "Error:  ADEOS could not FTP the following file(s):\n");
  fprintf(fp, "File(s) should be located in OUT_DIR (%s)\n\n",out_dir);
  fprintf(fp, "     File                                               In OUT_DIR? (yes/no)\n");
  fprintf(fp, "=============================================================================\n");
  /* For each file in mail msg.
  */
  errors = 0 ;
  temp = head ;
  while (temp)
    {

      /* parse temp->filename: separate path (if any) from filename */
      filename_only = (char *)strrchr(temp->filename, '/') ;
      if (filename_only != (char *)NULL)
        filename_only++ ;
      else
        filename_only = temp->filename ;
      relpath[0] = '\0';
      full_out_dir[0] = '\0';
      strcpy (relpath, temp->filename);
      strcpy (full_out_dir, out_dir);
      last_slash = (char *)strrchr(relpath, '/') ;
      if (last_slash != (char *)NULL) 
      {
         *last_slash = '\0';
         strcat(full_out_dir, "/"); 
         strcat(full_out_dir, relpath);
      }
      else
         relpath[0] = '\0';

      if (strcmp (temp->spare,"0") != 0)
	{
          if (strcmp (temp->spare,"1") == 0)
	     sprintf(spare_stat_msg,
	        "(ADEOS spare status = 1, Target File not needed)") ;
          else if (strcmp (temp->spare,"2") == 0)
	     sprintf(spare_stat_msg,
	        "(ADEOS spare status = 2, FTP error, Receiver site problem)") ;
          else if (strcmp (temp->spare,"3") == 0)
	     sprintf(spare_stat_msg,
	        "(ADEOS spare status = 3, FTP error, Sender site problem)") ;
          else 
	     sprintf(spare_stat_msg,
	        "(ADEOS spare status = %s, Undefined in current interface)",
		temp->spare) ;

          strcpy (in_dir, "no");
	  outdir_flist = (llist *) check_dir(full_out_dir) ;
	  if (outdir_flist != (llist *)NULL)
	    {
	      rfilerecp = FIRST(outdir_flist, rptr) ;
	      while (rfilerecp)
		{
		  if (strcmp(filename_only, rfilerecp->file_name) == 0)
		    strcpy(in_dir, "yes");
		  rfilerecp = NEXT(outdir_flist, rptr) ;
		}
	    }  
      
	  fprintf(fp, "%3d. %-60s %s %s\n", 
             errors+1, temp->filename, in_dir, spare_stat_msg) ;
	  ++errors ;
	  syslog(LOG_ERR, "ERROR:  ADEOS could not FTP %s\n", temp->filename) ;
	}
      else
	{
	  remfile = (char *)util_do_malloc(
                                 sizeof(char)*(strlen(full_out_dir)+1 +
					       strlen(filename_only)+1)) ;
	  strcpy(remfile, full_out_dir) ;
	  strcat(remfile, "/") ;
	  strcat(remfile, filename_only) ;
	  if (remove(remfile))
	    syslog(LOG_ERR, "WARNING, Unable to remove %s\n", remfile);
	}
      temp = temp->next ;
    }
  fclose(fp) ;

  /* Get FAIF bin path from environment
  */
  if (errors != 0)
    {
      syslog(LOG_DEBUG, "Got error status from ADEOS email message.\n");

      srcpath = (char *)getenv(FAIF_BINPATH_EV) ;
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
      
      /* Form name of mail script path
      */
      scriptpath = (char *)util_do_malloc(sizeof(char)*
			   (strlen(srcpath) + 1 + strlen(script) + 1)) ;
      strcpy(scriptpath, srcpath) ;
      strcat(scriptpath, "/") ;
      strcat(scriptpath, script) ;
      
      /* Check script permissions
      */
      if (access(scriptpath, F_OK) != 0)
	{
	  sprintf(logmsg, 
		  "ERROR, Mail script %s does not exist.  Exiting.\n", 
                  scriptpath);
	  syslog(LOG_ERR, logmsg) ; 
 
	  free(scriptpath) ;
	  return(ERROR) ;
	}
      if (access(scriptpath, X_OK) != 0)
	{
	  sprintf(logmsg, 
		"ERROR, No execute permission for mail script %s.  Exiting.\n",
		scriptpath);
	  syslog(LOG_ERR, logmsg) ; 
	  
	  free(scriptpath) ;
	  return(ERROR) ;
	}
      
      if ((pid = fork()) < 0)
	{
	  syslog(LOG_ERR, "ERROR, Error forking NFS transfer.  Exiting.\n") ;
	  return(ERROR) ;
	}
      
      else if (pid == 0)
	{                    /* child */
          sprintf (subject_re, "RE: %s", subject);
	  execl(scriptpath, script, subject_re, address, tmpfile, 
                rootpath, NULL) ;
	}
      
      if ((status = waitpid(pid, &statloc, 0)) < 0) 
	{
	  syslog(LOG_ERR, "ERROR, Waitpid FTPsend error.  Exiting.\n");
	  return(ERROR) ;
	}
      
      process_exit(statloc) ;
      if (WIFEXITED(statloc))
	statloc = WEXITSTATUS(statloc) ;
      
      switch (statloc)
	{
	case 0:
	  syslog(LOG_NOTICE,
                 "NOTICE: Mailed info about ADEOS error to FAIF operator.\n") ;
	  return(ACCEPT) ;
	  break ;
	case 1:
	  syslog(LOG_ERR, "CRITICAL:  Error in Reply to ADEOS.\n") ;
	  return(ERROR) ;
	  break ;
	default:
	  syslog(LOG_ERR, "ERROR: Unexpected exit status from %s\n", 
                 scriptpath);
	  return(ERROR) ;
	  break;
	  
	}  /* End switch */
    }
    syslog(LOG_NOTICE, "NOTICE: No errors from ADEOS email message.\n");

} /* check_ADEOS_msg */

/* End of File */
