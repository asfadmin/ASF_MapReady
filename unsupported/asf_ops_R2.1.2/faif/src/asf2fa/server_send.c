/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	server_send.c

Description:	
	This module contains the main send driver function send_file_to_FA.

External Functions:
	send_file_to_FA
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	FA_TransferMode_Table
	FA_Send_Func_Table

Notes:
1.  June '96 - R. Hoffman
    Call expand_path_val() to handle environment variables in path strings
    within the config file.
==============================================================================*/

static char SccsFile[] = "server_send.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "17 Sep 1996";
static char SccsLastChanger[] = "@(#)server_send.c	1.1";
static char SccsState[] = "1.1";

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <syslog.h>
#include "faifdefs.h"
#include "sendfile.h"
#include "ESAftypestr.h"
#include "NASDAftypestr.h"
#include "CSAftypestr.h"
#include "WALPSftypestr.h"
#include "ADEOSftypestr.h"
#include "ADEOS_msg.h"

#ifdef __STDC__
int send_file_to_FA(char *, char *, char *) ;
int send_ADEOS_msg(File_Transfer_Params *) ;
#else
int send_file_to_FA() ;
int send_ADEOS_msg() ;
#endif

extern void                 *util_do_malloc() ;
extern File_Transfer_Params *alloc_sendprms_record() ;
extern int                   send_file_via_ftp() ;
extern int                   send_file_via_nfs() ;
extern int                   send_file_via_ftam() ;
extern MESSAGEPTR            alloc_ADEOS_msg_rec() ;
extern char                 *expand_path_val();

extern char *FAxmitserver_configfile ;
extern File_Identifier ESA_FileTypeStr_Table[] ;
extern File_Identifier CSA_FileTypeStr_Table[] ;
extern File_Identifier NASDA_FileTypeStr_Table[] ;
extern File_Identifier WALPS_FileTypeStr_Table[] ;

extern Names_Table_Entry Xmit_HostVar_Table[] ;
extern Names_Table_Entry ESA_ExtDir_Table[] ;
extern Names_Table_Entry NASDA_ExtDir_Table[] ;
extern Names_Table_Entry CSA_ExtDir_Table[] ;
extern Names_Table_Entry WALPS_ExtDir_Table[] ;
extern Names_Table_Entry ADEOS_ExtDir_Table[] ;
extern Names_Table_Entry FA_TransferCmd_Table[] ;

extern Names_Table_Entry FA_PassFile_Table[] ; 


/* Table of file transfer modes used when 
-- transferrring files for a FA.  Note that the same
-- mode is used for all files per FA, ie. if ESA mode
-- is BINARY, all ESA files are transferred as BINARY
*/
static Names_Table_Entry FA_TransferMode_Table[] =
{
   { ESA,      ESA_STR,    BINARY },
   { NASDA,    NASDA_STR,  BINARY },
   { CSA,      CSA_STR,    ASCII  },
   { WALPS,    WALPS_STR,  ASCII  },
   { ADEOS,    ADEOS_STR,  ASCII  },
   { SENTINEL, NULL,       NULL   }
} ;

/* Table of send protocol activating
-- routines used by the FAs
*/
static Function_Table_Entry FA_Send_Func_Table[] =
{
   { ESA,      ESA_STR,   send_file_via_ftp },
   { NASDA,    NASDA_STR, send_file_via_ftp },
   { CSA,      CSA_STR,   send_file_via_ftp },
   { WALPS,    WALPS_STR, send_file_via_ftp },
   { ADEOS,    ADEOS_STR, send_ADEOS_msg    },
   { SENTINEL, NULL,      NULL              }
} ;




/*==============================================================================
Function:       int send_file_to_FA(char *outbound_FA_file, char *FA,
				    char *Ftype_str)

Description:
	The send file to FA function invokes the appropriate file
transfer protocol when it is called.  The file outbound_FA_file is 
sent to the flight agency described by FA.  This function refers to 
the Xmit_HostVar_Table, ESA_ExtDir_Table, NASDA_ExtDir_Table, 
CSA_ExtDir_Table and WALPS_ExtDir_Table to determine the variables 
to resolve to obtain the remote destination host name and directory.
Note that the indices to the tables mentioned have to be determined
based on the parameters FA and Ftype_str.  The transfer protocol used 
(the send_file function used) is also determined using a table, 
FA_Send_Func_Table, which points to the assigned function per flight 
agency.  If an error status is returned by the send function called
the ASF standard error routine is invoked to report the error and an 
error status is returned to the caller.

Parameters:
	outbound_FA_file - filename of file to send
	FA - destination flight agency may be "ESA", "NASDA", "CSA",
		"WALPS" or "ADEOS"
	Ftype_str - type of file to send (determines destination dir)

Returns:	status returned from send operations or ERROR
Creator:	Norbert Piega	
Creation Date:  06/13/1994
		09/18/1995 Added ADEOS support
Notes:		
	If Ftype_str is NULL, the outbound file is stored in the
default destination directory for the specified FA.
===========================================================================*/
#ifdef __STDC__
int
send_file_to_FA(char *outbound_FA_file, char *FA, char *Ftype_str)
#else
int
send_file_to_FA(outbound_FA_file, FA, Ftype_str)
   char *outbound_FA_file ;
   char *FA ;
   char *Ftype_str ;
#endif
{
   int status ;                                  /* status returned */
   int index ;                                   /* index to tables */
   int h_index = SENTINEL ;         /* index to table for host name */
   int d_index = SENTINEL ;          /* index to table for dest dir */
   int t_index = SENTINEL ;      /* index to table for transfer cmd */
   int FA_index = SENTINEL ;            /* index to table for FA id */
   int FA_id = SENTINEL ;                                  /* FA id */
   int Ftype_id = SENTINEL ;                      /* FileTypeStr id */ 
   FILE *configfp ;                 /* file pointer for config file */
   char inline[MAXLINE+1] ;                      /* input file line */
   char *start ;
   char *substr1, *substr2 ;           /* sub-strings in input line */
   char *expanded_substr2;
   File_Transfer_Params *sendprm ;    /* send function param struct */
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;          /* syslog msg string */
   Names_Table_Entry *ExtDir_Table ;          /* External dir table */
   File_Identifier *FileTypeStr_Table ;   /* File type string table */
   char *tmpstr = NULL ; 

   /* Make sure specified file to transmit is not NULL
   */
   if (outbound_FA_file != (char *)NULL)
   {
      /* Allocate file send parameters record
      */
      sendprm = (File_Transfer_Params *)alloc_sendprms_record() ;
      if (sendprm != (File_Transfer_Params *)NULL)
      {
         sendprm->out_file = (char *)
            util_do_malloc(sizeof(char)*(strlen(outbound_FA_file)+1)) ;
         strcpy(sendprm->out_file, outbound_FA_file) ;
      }
      else
      {
         syslog(LOG_ERR, 
	    "ERROR, Unable to allocate transfer params record\n") ;
         return(ERROR) ;
      }
   }
   else
   {
      syslog(LOG_ERR, "ERROR, NULL input filename\n") ;
      return(ERROR) ;
   }

   /* Determine variables for destination host, dir, transfer cmd
   -- and flight agency id.  Determine which indices in tables to use.
   */

   /* First, determine FA id from FA string. Use the function table 
   -- FA_Send_Func_Table.  Note that the FA_Send_Func_Table is used 
   -- to store FA name strings through the func_name field.  Also,
   -- store the index obtained; it will be used to refer to the send 
   -- function to call later. 
   */
   for (index=0; FA_Send_Func_Table[index].func_id != SENTINEL; index++)
      if (strcmp(FA, FA_Send_Func_Table[index].func_name) == 0)
      {
	 FA_id = FA_Send_Func_Table[index].func_id ;
	 FA_index = index ;
	 break ;
      }
   if (FA_Send_Func_Table[index].func_id == SENTINEL)
   {
      syslog(LOG_ERR, "ERROR, Unrecognized FA string '%s'\n", FA);
      free(sendprm->out_file) ;
      free(sendprm) ;
      return(ERROR) ;
   }

   /* Get index to variable for host address
   */
   for (index=0; Xmit_HostVar_Table[index].name_id != SENTINEL; index++)
      if (Xmit_HostVar_Table[index].name_id == FA_id)
      {
         h_index = index ; 
	 break ;
      }
   if (Xmit_HostVar_Table[index].name_id == SENTINEL)
   {
      syslog(LOG_ERR, "ERROR, Unable to determine host destination from FA string %s\n", FA) ;
      free(sendprm->out_file) ;
      free(sendprm) ;
      return(ERROR) ;
   }

   /* Get index to variable for transfer cmd (script name for FTP)
   */
   for (index=0; FA_TransferCmd_Table[index].name_id != SENTINEL; index++)
      if (FA_TransferCmd_Table[index].name_id == FA_id)
      {
         t_index = index ; 
	 break ;
      }
   if (FA_TransferCmd_Table[index].name_id == SENTINEL)
   {
      syslog(LOG_ERR, "ERROR, Unable to determine transfer command from FA string %s\n", FA);
      free(sendprm->out_file) ;
      free(sendprm) ;
      return(ERROR) ;
   }

   /* Based on FA, assign which file type string and
   -- external directory table to use.
   */
   if (FA_id == ESA)
   {
      ExtDir_Table = ESA_ExtDir_Table ;
      FileTypeStr_Table = ESA_FileTypeStr_Table ;
   }
   else if (FA_id == NASDA)
   {
      ExtDir_Table = NASDA_ExtDir_Table ;
      FileTypeStr_Table = NASDA_FileTypeStr_Table ;
   }
   else if (FA_id == CSA)
   {
      ExtDir_Table = CSA_ExtDir_Table ;
      FileTypeStr_Table = CSA_FileTypeStr_Table ;
   }
   else if (FA_id == WALPS)
   {
      ExtDir_Table = WALPS_ExtDir_Table ;
      FileTypeStr_Table = WALPS_FileTypeStr_Table ;
   }
   else if (FA_id == ADEOS)
   {
      ExtDir_Table = ADEOS_ExtDir_Table ;
      FileTypeStr_Table = ADEOS_FileTypeStr_Table ;
   }

   /* Determine FA file type id from file type string
   */
   if (Ftype_str != (char *)NULL)
   {
      for (index=0; FileTypeStr_Table[index].file_id_number != SENTINEL; 
	   index++)
         if (strcmp(Ftype_str, FileTypeStr_Table[index].file_identifier) == 0)
         {
	    Ftype_id = FileTypeStr_Table[index].file_id_number ;
	    break ;
         }
      if (FileTypeStr_Table[index].file_id_number == SENTINEL)
      {
         syslog(LOG_ERR, "ERROR, Unrecognized file type string %s\n", Ftype_str) ;
         free(sendprm->out_file) ;
         free(sendprm) ;
         return(ERROR) ;
      }
      /* Get index to variable for External (destination) directory name
      */
      for (index=0; ExtDir_Table[index].name_id != SENTINEL; index++)
         if (ExtDir_Table[index].name_id == Ftype_id)
         {
            d_index = index ; 
	    break ;
         }
      if (ExtDir_Table[index].name_id == SENTINEL)
      {
         syslog(LOG_ERR, "ERROR, Unable to determine external directory name for %s\n", Ftype_str);
         free(sendprm->out_file) ;
         free(sendprm) ;
         return(ERROR) ;
      }
   }
   else
   {
      /* File type string specified is NULL. Use default for the FA.
      -- Issue a warning on this.
      */
      syslog(LOG_WARNING, "ERROR, File type specified is NULL.\n");
      return(ERROR) ;
   }

   /* If config file was specified
   */
   if (FAxmitserver_configfile != (char *)NULL)
   {
      /* Open config file
      */
      if ((configfp = fopen(FAxmitserver_configfile, "r")) == (FILE *)NULL)
      {
	 syslog(LOG_ERR, "ERROR, Error opening config file %s\n", FAxmitserver_configfile) ;
         free(sendprm->out_file) ;
         free(sendprm) ;
	 return(ERROR) ;
      }
      else
      {
         /* Parse config file
         -- Obtain value for config variables: 
	 --    1 destination host address
	 --    2 destination directory
         */
         while (fgets(inline, MAXLINE, configfp) != NULL)
         {
	    /* Skip blanks
	    */
	    start = inline ;
	    while (isspace(*start))
	       start++ ;

            /* Skip BLANK and COMMENT line
	    */
	    if (*start == '\0' || *start == '#')
	       continue ;

            /* Get first 2 substrings in input line
            */
            substr1 = strtok(start, " ") ;
            substr2 = strtok(NULL, " \n") ;
 
            if (strchr(substr2, '$') != NULL)
            {
               expanded_substr2 = expand_path_val(substr2);
               substr2 = expanded_substr2; 
            }

	    /* Test if 1st substring matches config variable name
            -- If matched, assign substring 2 to the value of the variable.
            */

	    /* Remote Host Address
	    */
            if (strcmp(substr1, 
	               Xmit_HostVar_Table[h_index].name_identifier) == 0)
            {
               sendprm->dest_host = 
		  (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
               strcpy(sendprm->dest_host, substr2) ;
            }

	    /* Transfer cmd / FTP script name
            */
            else if (strcmp(substr1, 
	               FA_TransferCmd_Table[t_index].name_identifier) == 0)
            {
               sendprm->send_cmd = 
		  (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
               strcpy(sendprm->send_cmd, substr2) ;
            }

            /* External/Destination directory name
            */
            else if (strcmp(substr1, 
			    ExtDir_Table[d_index].name_identifier) == 0)
            {
               sendprm->dest_dir = 
		  (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
               strcpy(sendprm->dest_dir, substr2) ;
            }

            /* FAIF rootpath
            */
            else if (strcmp(substr1, FAIF_ROOTPATH_EV) == 0) 
            {
               sendprm->rootpath = 
		  (char *)util_do_malloc(sizeof(char)*(strlen(substr2)+1)) ;
               strcpy(sendprm->rootpath, substr2) ;
            }

          } /* Endwhile checking config file */ ;

	  fclose(configfp) ;

      } /* EndElse */
   } /* Endif config not NULL */

   /* If no config file or not in config file, use environment variables
   */

   if (sendprm->dest_host == (char *)NULL)
   {
      /* Get environment variable value for destination host
      */
      tmpstr = 
	 (char *) getenv(Xmit_HostVar_Table[h_index].name_identifier) ;
      if (tmpstr != (char *)NULL)
      {
         sendprm->dest_host = 
	    (char *) util_do_malloc(sizeof(char)*(strlen(tmpstr)+1)) ;
         strcpy(sendprm->dest_host, tmpstr) ;
      }
      else
      {
         sendprm->dest_host = 
	    (char *) util_do_malloc(sizeof(char)*
		     (strlen(Xmit_HostVar_Table[h_index].default_value)+1)) ;
         strcpy(sendprm->dest_host, 
		Xmit_HostVar_Table[h_index].default_value) ;
      }
   }

   if (sendprm->send_cmd == (char *)NULL)
   {
      /* Get environment variable value for transfer cmd / script name
      */
      tmpstr = 
	 (char *) getenv(FA_TransferCmd_Table[t_index].name_identifier) ;
      if (tmpstr != (char *)NULL)
      {
         sendprm->send_cmd = 
	    (char *) util_do_malloc(sizeof(char)*(strlen(tmpstr)+1)) ;
         strcpy(sendprm->send_cmd, tmpstr) ;
      }
      else
      {
         sendprm->send_cmd = 
	    (char *) util_do_malloc(sizeof(char)*
		     (strlen(FA_TransferCmd_Table[t_index].default_value)+1)) ;
         strcpy(sendprm->send_cmd, 
		FA_TransferCmd_Table[t_index].default_value) ;
      }
   }

   if (sendprm->dest_dir == (char *)NULL)
   {
      /* Get environment variable value for destination directory
      */
      tmpstr = 
	 (char *) getenv(ExtDir_Table[d_index].name_identifier) ;
      if (tmpstr != (char *)NULL)
      {
         sendprm->dest_dir = 
            (char *) util_do_malloc(sizeof(char)*(strlen(tmpstr)+1)) ;
         strcpy(sendprm->dest_dir, tmpstr) ;
      }
      else
      {
         sendprm->dest_dir = 
            (char *) util_do_malloc(sizeof(char)*
		     (strlen(ExtDir_Table[d_index].default_value)+1)) ;
         strcpy(sendprm->dest_dir, ExtDir_Table[d_index].default_value) ;
      }
   }

   if (sendprm->rootpath == (char *)NULL)
   {
      /* Get environment variable value for FAIF rootpath
      */
      tmpstr = (char *) getenv(FAIF_ROOTPATH_EV) ;
      if (tmpstr != (char *)NULL)
      {
         sendprm->rootpath = 
            (char *) util_do_malloc(sizeof(char)*(strlen(tmpstr)+1)) ;
         strcpy(sendprm->rootpath, tmpstr) ;
      }
      else
      {
         sendprm->rootpath = (char *) util_do_malloc(sizeof(char)*
		                (strlen(FAIF_ROOTPATH_DEF)+1)) ;
         strcpy(sendprm->rootpath, FAIF_ROOTPATH_DEF) ;
      }
   }

   /* Assign file transfer mode used specific to a FA
   */
   for (index=0; FA_TransferMode_Table[index].name_id != SENTINEL; index++)
      if (strcmp(FA, FA_TransferMode_Table[index].name_identifier) == 0)
      {
	 sendprm->mode = FA_TransferMode_Table[index].default_value ;
	 break ;
      }
   if (FA_TransferMode_Table[index].name_id == SENTINEL)
   {
      syslog(LOG_ERR, "ERROR, Unable to obtain file transfer mode for %s. \n", FA) ;
      return(ERROR) ;
   }

   /* Assign Password file used specific to a FA
   */
   for (index=0; FA_PassFile_Table[index].name_id != SENTINEL; index++)
      if (strcmp(FA, FA_PassFile_Table[index].name_identifier) == 0)
      {
	 /* If CSA file, check name id as well, not just FA string
	 */
	 if (strcmp(FA, CSA_STR) == 0)
	 {
	    if (FA_PassFile_Table[index].name_id == Ftype_id)
            {
	       sendprm->user = FA_PassFile_Table[index].default_value ;
	       break ;
	    }
	 }

	 /* If ADEOS file, don't need password file; just send email 
	 */
	 else if (strcmp(FA, ADEOS_STR) == 0)
	    break ;

	 else
	 {
	    sendprm->user = FA_PassFile_Table[index].default_value ;
	    break ;
         }
      }
   if ((strcmp(FA, ADEOS_STR) != 0) &&
       (FA_PassFile_Table[index].name_id == SENTINEL))
   {
      syslog(LOG_ERR,"ERROR, Unable to obtain password file for %s. \n", FA) ;
      return(ERROR) ;
   }

#ifdef VERBOSE
   printf("outfile %s\n", sendprm->out_file) ;
   printf("host %s\n", sendprm->dest_host) ;
   printf("destdir %s\n", sendprm->dest_dir) ;
   printf("sendcmd %s\n", sendprm->send_cmd) ;
   printf("rootpath %s\n", sendprm->rootpath) ;
#endif

   /* Call the appropriate function table entry 
   */
   status = (*FA_Send_Func_Table[FA_index].func_entry)(sendprm) ;
   free(sendprm->out_file) ;
   free(sendprm->dest_host) ;
   free(sendprm->dest_dir) ;
   free(sendprm->send_cmd) ;
   free(sendprm->rootpath) ;
   free(sendprm) ;

   return(status);

} /* send_file_to_FA */




/*==============================================================================
Function:	send_ADEOS_msg(File_Transfer_Params *sendprm)
Description:	send data notification email to ADEOS
Parameters:
	File_Transfer_Params *sendprm - the file parameters structure is
overloaded to contain email parameter information

Returns:	status of email notification action	
Creator:	Norbert Piega
Creation Date:	Fri Aug 11 14:26:27 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int
send_ADEOS_msg(File_Transfer_Params *sendprm)
#else
int
send_ADEOS_msg(sendprm)
   File_Transfer_Params *sendprm ;
#endif
{
   char *mtype = "CON" ;        /* Msg type in email body is always "CON"     */
   char *comp_id = "HEOC_0101" ; /* Comp Id in email body is 
				 -- always "HEOC_0101" 
				 */
   char *subj_str = 
      "F-NR-I-ASF_0101-HEOC_0101" ;  /* Subject format for data Notification 
				     -- from ASF is always the same 
				     */
   char *subpath = "asf/sen/" ; /* File subpath ADEOS wants in email body */
   MESSAGEPTR msg ;
   struct stat statbuf ;
   char *address = NULL ;
   char subject[MAXLINE] ;
   char *orig_outfile = NULL ;
   char *slash = NULL ;
   int status = OK ;

   /* Source directory name must be derived from out filename
   */
   orig_outfile = 
      (char *)util_do_malloc(sizeof(char)*(strlen(sendprm->out_file)+1)) ;
   if (orig_outfile == (char *)NULL)
   {
      syslog(LOG_ERR, 
	 "ERROR, Memory allocation error in send_ADEOS_msg function\n") ;
      return(ERROR) ;
   }
   strcpy(orig_outfile, sendprm->out_file) ;

   sendprm->src_dir = 
      (char *)util_do_malloc(sizeof(char)*(strlen(sendprm->out_file)+1)) ;
   if (sendprm->src_dir == (char *)NULL)
   {
      syslog(LOG_ERR, 
	 "ERROR, Memory allocation error in send_ADEOS_msg function\n") ;
      return(ERROR) ;
   }

   strcpy(sendprm->src_dir, sendprm->out_file) ;
   slash = (char *)strrchr(sendprm->src_dir, '/') ;
   *slash = NULL ;
   slash++ ;

   strcpy(sendprm->out_file, slash) ;

   syslog(LOG_NOTICE, 
	 "Moving temp file %s/%s into outbound ADEOS directory %s\n", 
	 sendprm->src_dir, sendprm->out_file, sendprm->dest_dir) ;

   /* First copy temporary file into the directory where
   -- ADEOS is supposed to get the file from.
   */
   status = send_file_via_nfs(sendprm) ;
   if (status == ERROR)
   {
      syslog(LOG_ERR, 
	 "ERROR storing %s into outbound ADEOS directory %s\n", 
	 orig_outfile, sendprm->dest_dir) ;
      free(orig_outfile) ;
      return(ERROR) ;
   }

   msg = (MESSAGEPTR)alloc_ADEOS_msg_rec() ;
   if (msg == (MESSAGEPTR)NULL)
   {
      syslog(LOG_ERR, 
	 "ERROR, Memory allocation error in send_ADEOS_msg function\n") ;
      free(orig_outfile) ;
      return(ERROR) ;
   }
   strcpy(msg->msg_type, mtype) ;

   /* Email body must say CON>asf/sen/filename>fsize>HEOC_0101>>
   -- Next 3 lines build "asf/sen/" + filename (w/c starts at slash)
   */
   strcpy(msg->filename, subpath) ;
   strcat(msg->filename, slash) ;

   stat(orig_outfile, &statbuf) ;
   if (statbuf.st_size <= 0)
   {
      syslog(LOG_ERR, 
	 "ERROR, Unexpected file size for ADEOS file %s\n", orig_outfile) ;
      free(orig_outfile) ;
      free(msg) ;
      return(ERROR) ;
   }
   sprintf(msg->file_size, "%d", statbuf.st_size) ;
   strcpy(msg->computerid, comp_id) ;
   strcpy(msg->spare, "") ;

   address = (char *)get_config_val(FAxmitserver_configfile, ADEOS_OP_USER_EV) ;
   if (address == (char *)NULL)
   {
      syslog(LOG_WARNING,
	 "ERROR, ADEOS account name unknown.  %s value not set.", ADEOS_OP_USER_EV) ;
      free(orig_outfile) ;
      free(msg) ;
      return(ERROR) ;
   }
   sprintf(subject, subj_str) ;

   create_ADEOS_msg(msg, address, subject, 0, FAxmitserver_configfile, 'I') ;
   free(orig_outfile) ;
   free(msg) ;

   return(status) ;

} /* send_ADEOS_msg */


/* End of file */
