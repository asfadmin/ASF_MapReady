/*==============================================================================
Filename:	router.c

Description:
	This module contains the check_sendlist function.  The check_sendlist
routine contains the calls that execute the routing of each file found in a
file record list (the send list).

External Functions:
	check_sendlist
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  R. Hoffman - Feb. '96
    Added NULL checks before each use of FIRST. 
    (See additional comments at the call to gen_ESA_PMF.)
2.  R. Hoffman - Sept. 96
    Added SHAQP
    Changed ims_section.inc to ims_section.c
3.  R. Hoffman - June '97
    Re-structuring
4.  R. Hoffman - Aug. '97
    Try next file (instead of quitting) if gen_xxx_PMF() fails.
==============================================================================*/

static char SccsFile[] = "router.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "25 Mar 1996";
static char SccsLastChanger[] = "@(#)router.c	1.4";
static char SccsState[] = "1.4";

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include "configrec.h"
#include "filerec.h"
#include "sendfile.h"
#include "dapps_list.h"
#include "nmalloc.h"

#ifdef ESA_DM
#include "ESA.h"
#include "ESArouter.h"

#elif NASDA_DM
#include "NASDA.h"
#include "NASDArouter.h"

#elif CSA_DM
#include "CSA.h"
#include "CSArouter.h"

#elif WALPS_DM
#include "WALPS.h"
#include "WALPSrouter.h"

#elif ADEOS_DM
#include "ADEOS.h"
#include "ADEOSrouter.h"
#endif


#ifdef __STDC__
int check_sendlist(llist *, Config_Record *) ;
int route_file_to_bak_dir(File_Record *, Config_Record *) ;
#else
int check_sendlist() ;
int route_file_to_bak_dir() ;
#endif

extern File_Transfer_Params *alloc_sendprms_record() ;
extern int                   assign_filerec_ftime() ;
extern int                   update_log() ;
extern int                   update_incoming_dir() ;
extern int                   check_bak_logged();
extern int                   update_bak_log();
extern void                  rename_JERS_file();
extern int                   send_to_ims();

#ifdef ESA_DM
extern Names_Table_Entry ESA_ReceptDir_Table[] ;
llist *gen_ESA_PMF() ;

#elif NASDA_DM
extern Names_Table_Entry NASDA_ReceptDir_Table[] ;
llist *gen_NASDA_PMF() ;
void   rename_JERS_file();

#elif CSA_DM
extern Names_Table_Entry CSA_ReceptDir_Table[] ;
llist *gen_CSA_PMF() ;

#elif WALPS_DM
extern Names_Table_Entry WALPS_ReceptDir_Table[] ;
llist *gen_WALPS_PMF() ;

#elif ADEOS_DM
extern Names_Table_Entry ADEOS_ReceptDir_Table[] ;
llist *gen_ADEOS_PMF() ;

#endif
 


/*==============================================================================
Function:	int route_file_to_bak_dir(filerecp, config) ;
Description:	Copy file to specified backup directory area
Parameters:	file record, config record
Returns:	OK, ERROR	
Creator:	Norbert Piega
Creation Date:	Sun Oct 29 18:12:14 PST 1995
Notes:		If ddir == null, don't attempt to copy
==============================================================================*/
#ifdef __STDC__
int route_file_to_bak_dir(File_Record *filerecp, Config_Record *config)
#else
int route_file_to_bak_dir(filerecp, config)
   File_Record *filerecp ;
   Config_Record *config ;
#endif
{
   File_Transfer_Params *sendprm ;
   int status = OK ;

   /* Perform routing of file in list. */
   sendprm = alloc_sendprms_record() ;
   if (sendprm != (File_Transfer_Params *)NULL)
   {
      /* Assign parameter values for file send operation. */
      sendprm->out_file =
         (char *)util_do_malloc(sizeof(char)*strlen(filerecp->file_name)+1) ;
      strcpy(sendprm->out_file, filerecp->file_name) ;
 
      sendprm->src_dir =
         (char *)util_do_malloc(sizeof(char)*strlen(config->FA_receptdir)+1) ;
      strcpy(sendprm->src_dir, config->FA_receptdir) ;
 
      if (config->FA_desthost != (char *)NULL)
      {
         sendprm->dest_host =
            (char *)util_do_malloc(sizeof(char)*strlen(config->FA_desthost)+1) ;
         strcpy(sendprm->dest_host, config->FA_desthost) ;
      }

      if (config->FA_destspec == (char *)NULL) return(status) ;
      sendprm->dest_dir =
         (char *)util_do_malloc(sizeof(char)*(strlen(config->FA_destspec)+1)) ;
      strcpy(sendprm->dest_dir, config->FA_destspec) ;
 
      sendprm->rootpath =
         (char *)util_do_malloc(sizeof(char)*(strlen(config->FA_rootpath)+1)) ;
      strcpy(sendprm->rootpath, config->FA_rootpath) ;
 
      if (config->FA_transfercmd != (char *)NULL)
      {
         sendprm->send_cmd = (char *)util_do_malloc(sizeof(char)*
                                   (strlen(config->FA_transfercmd)+1)) ;
         strcpy(sendprm->send_cmd, config->FA_transfercmd) ;
      }

      sendprm->mode = (char *)util_do_malloc(sizeof(char)*(strlen(ASCII)+1)) ;
      strcpy(sendprm->mode, ASCII) ;
 
      status = send_file_via_nfs(sendprm) ;
      
      /* Free sendprm fields and sendprm itself. */
      free(sendprm->out_file) ;
      free(sendprm->src_dir) ;
      if (sendprm->dest_host != (char *)NULL)
         free(sendprm->dest_host) ;
      free(sendprm->dest_dir) ;
      free(sendprm->rootpath) ;
      free(sendprm->mode) ;
      if (sendprm->send_cmd != (char *)NULL)
         free(sendprm->send_cmd) ;
      if (sendprm->user != (char *)NULL)
         free(sendprm->user) ;
      free(sendprm) ;

      if (status == ERROR)
         syslog(LOG_ERR,
                "ERROR: Error copying file to destination directory\n") ;

   } /* end sendprm not NULL */

   else
   {
     syslog(LOG_DEBUG,
        "ERROR: Unable to allocate sendprm in func. route_file_to_bak_dir\n") ;
     status = ERROR ;
   }

   return(status) ;

} /* route_file_to_bak_dir */




/*==============================================================================
Function:	int check_sendlist(filerec_llist, config)

Description:	
	For each file in filerec_llist:
                Route to DDIR if called for.
		Call the corresponding function to create the PMF for the file
                   and to create any needed translation files.
		Call the IMS archive function to send the file into the IMS DB.
		Assign the forwarded/routed time of the file using the
		   function assign_filerec_ftime.
		List the file in the log file to indicate that it has been
		   routed successfully using the function update_log.
		Remove the file from the incoming directory by calling the 
		   update_incoming_dir function.
	Endloop

	Return check_sendlist status.

Parameters:
	filerec_llist - Pointer to list of file records representing
                        the files obtained from the monitored directory.  
                        If this is NULL, it means no files were found from the 
                        directory.
	config        - Record structure containing configuration info.

Returns:
	OK    - sendlist check completed normally
	ERROR - error encountered during check

Creator:	Norbert Piega	
Creation Date:	08/04/1994
Notes:
1.  June '97 - R. Hoffman
    Re-structured:  Make route_to_bak_dir the first action.  Rename
    JERS files sooner. Separate send_to_ims more cleanly.  Omit switch 
    on filetype.  Minimize conditional compilation pieces.
==============================================================================*/
#ifdef __STDC__
int
check_sendlist(llist *filerec_llist, Config_Record *config)
#else
int
check_sendlist(filerec_llist, config)
   llist *filerec_llist ; 
   Config_Record *config ;
#endif
{
   int status;                                /* status returned */
   int status_ims;                               /* archived OK? */
   int status_l;                                  /* bak_log OK? */
   int index ;                            /* index to dir tables */
   File_Record * filerecp ;      /* file record from linked list */
   cursor ptr ;                            /* linked list cursor */
   File_Transfer_Params *sendprm ;   /* send parameter structure */
   llist *file_pmf_list ;
   Names_Table_Entry *ReceptDir_Table ;
   int  logged_flag;
   char bak_log_file[MAXLINE];
   char new_JERS_filename[MAXLINE];

#ifdef ESA_DM
   ReceptDir_Table = ESA_ReceptDir_Table;
#elif NASDA_DM
   ReceptDir_Table = NASDA_ReceptDir_Table;
#elif CSA_DM
   ReceptDir_Table = CSA_ReceptDir_Table;
#elif WALPS_DM
   ReceptDir_Table = WALPS_ReceptDir_Table;
#elif ADEOS_DM
   ReceptDir_Table = ADEOS_ReceptDir_Table;
#endif

   /* Define the full pathname for the bak.log file */
   strcpy(bak_log_file, config->FA_receptdir);
   strcat(bak_log_file, "/log/bak.log");

   /* Check through files in the file list */
   if (filerec_llist == (llist *) NULL) return (ERROR);
   filerecp = FIRST(filerec_llist, ptr) ;
   while (filerecp)
   {
      /* Check in ReceptDir_Table if expected file type */
      for (index=0; ReceptDir_Table[index].name_id != SENTINEL; index++)
	 if (ReceptDir_Table[index].name_id == filerecp->file_type_status)
	    break ;
      if (ReceptDir_Table[index].name_id == SENTINEL)
      {
         syslog(LOG_ERR, 
                "ERROR: %s is an unknown file type.\n", filerecp->file_name) ;
         status = ERROR ;
         break ; 
      }

      if (filerecp->file_type_status != config->FA_filetype)
      {
         filerecp = NEXT(filerec_llist, ptr) ;
	 continue ;
      }

      /* Assign forwarded time for routed file */
      if ((status = assign_filerec_ftime(filerecp)) == ERROR)
      {
         syslog(LOG_ERR, 
               "ERROR: Error in file forwarded time assignment operation\n") ;
         break ;
      }

#ifdef NASDA_DM
      /* Parse the header of a NASDA file, and rename the file if necessary. */
      rename_JERS_file(config->FA_receptdir, filerecp->file_name, 
                       new_JERS_filename);
      if (strlen(filerecp->file_name) == 4)
         strcpy(filerecp->file_name, new_JERS_filename) ;
#endif

      syslog(LOG_NOTICE, "NOTICE: Routing file %s\n", 
             filerecp->file_name) ;

      logged_flag = check_bak_logged(filerecp->file_name, bak_log_file) ;

      /* CSA files may repeat the same name.  Always log and route them. */
      if (strcmp(filerecp->flight_agency, CSA_STR) == 0) logged_flag = FALSE;

      if (logged_flag != TRUE)
      {
         /* Copy file to destination directory */
         if ((status_l = route_file_to_bak_dir(filerecp, config)) == ERROR)
         {
            syslog(LOG_ERR, 
                   "ERROR: Error in copy to back-up dirs operation.\n") ;
         }
         else
	 {
            /* Add file to bak.log */
            if ((status_l = update_bak_log(bak_log_file, filerecp->file_name,
                 filerecp->time_received, filerecp->time_forwarded)) == ERROR)
            {
               syslog(LOG_ERR, "ERROR: Error in bak_log operation.\n") ;
            }
	 }
      }

      /* Create PMF and any needed translation files */
#ifdef ESA_DM
      file_pmf_list = (llist *)gen_ESA_PMF(config->FA_receptdir,
             filerecp->file_name, config->FA_configfile) ;
#elif NASDA_DM
      file_pmf_list = (llist *)gen_NASDA_PMF(config->FA_receptdir,
             filerecp->file_name, config->FA_configfile) ;
#elif WALPS_DM
      file_pmf_list = (llist *)gen_WALPS_PMF(config->FA_receptdir,
             filerecp->file_name, filerecp->file_type_status) ; 
#elif CSA_DM
      file_pmf_list = (llist *)gen_CSA_PMF(config->FA_receptdir,
             filerecp->file_name, filerecp->file_type_status) ; 
#elif ADEOS_DM
      file_pmf_list = (llist *)gen_ADEOS_PMF(config->FA_receptdir,
             filerecp->file_name, config->FA_configfile) ;
#endif

      /*  Cannot send a NULL file_pmf_list to send_to_ims()  */
      if (file_pmf_list == (llist *) NULL) 
      {
         filerecp = NEXT(filerec_llist, ptr) ;
	 continue ;
      }
      status_ims = send_to_ims(file_pmf_list, filerecp, config);

      /* Free structures */
      DEL_ALL(file_pmf_list) ;

      if (status_ims)         /* Add name of archived file in log. */
      {
         /* Update log to reflect newly routed file */
         if ((status = update_log(config->FA_logfile, filerecp)) == ERROR)
         {
            syslog(LOG_ERR, "ERROR: Error in update_log operation\n") ;
	    break ;
         }
      }
      else
      {
         syslog(LOG_ERR, "ERROR: Unable to archive file %s to IMS.  Keeping file in reception area.\n",
	    filerecp->file_name) ;
	 status = ERROR ;
      }

      filerecp->routed_flag = TRUE ;
      syslog(LOG_NOTICE, "NOTICE: Routed %s\n", filerecp->file_name) ;

      filerecp = NEXT(filerec_llist, ptr) ;

   } /* endWhile not end of linked list */

   /* Free structures */
   DEL_ALL(filerec_llist) ;

   return(status) ;

} /* check_sendlist */

/* End of File */
