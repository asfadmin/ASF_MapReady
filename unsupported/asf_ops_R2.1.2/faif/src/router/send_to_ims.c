/*==============================================================================
Filename:	send_to_ims.c

Description:
	This module contains the send_to_ims function which is used by
        the routing routine to archive files in IMS.

External Functions:
	send_to_ims
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  R. Hoffman - Aug. '97
    Improved one error message.
==============================================================================*/

static char SccsFile[] = "send_to_ims.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "06/05/97";
static char SccsLastChanger[] = "@(#)send_to_ims.c	1.1";
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
#include "configrec.h"
#include "dapps_list.h"

#ifdef __STDC__
int send_to_ims(llist *, File_Record *, Config_Record *) ;
#else
int send_to_ims() ;
#endif


/*==============================================================================
Function:	int send_to_ims(file_pmf_list, filerecp, config) ;
Description:	Send specified file(s) to IMS
Parameters:     filerecp         record for the incoming file being routed	
                file_pmf_list    linked list of file(s) generated from that one
                config           configuration record
Returns:	(none)
Creator:	Norbert Piega, Rich Norman, Rodney Hoffman
Creation Date:	Oct 29 1995
Notes:
==============================================================================*/


int send_to_ims(llist *file_pmf_list, File_Record *filerecp, 
                 Config_Record *config)
{

   char platform[15];
   char programName[MAXLINE] ;
   cursor pfptr ;
   PMF_FILE_LIST *fpmfpair ;
   char temp_old_rename[MAXLINE] ;
   char temp_new_rename[MAXLINE] ;
   char temp_old_rename_m[MAXLINE] ;
   char temp_new_rename_m[MAXLINE] ;
   char *dot_D ;
   char remove_file_name[MAXLINE] ;
   char remove_file_dir[MAXLINE] ;
   char ims_directory[MAXLINE] ;
   int  status_all, status_ims, status_u, statur; 
   char dataset[MAXLINE] ;
   char *first_space;
   int  platform_len;
   char sat[3] ;

     status_all = TRUE;     /* No errors yet. */

     /* Define the directory from which ims will obtain the files */
     strcpy(ims_directory, config->FA_receptdir) ;
     strcat(ims_directory, "/") ;
     strcat(ims_directory, "PMF/") ;

     /* file_pmf_list is checked for NULL before we get here */
     fpmfpair = FIRST(file_pmf_list, pfptr) ;
     while (pfptr)
     {

        /* Set programName and determine platform */
#ifdef ESA_DM
        strcpy(programName, "ESAdirmon  ") ;
	if (fpmfpair->orig_not_tran == TRUE)
	{
           strncpy(sat,
		   &fpmfpair->file_name[strlen(fpmfpair->file_name)-2], 2);
           sat[2] = '\0';
	}
        else
	{
           strncpy(sat, &fpmfpair->file_name[0], 2);
           sat[2] = '\0';
	}

        if (strcmp(sat,"e1") == 0 || strcmp(sat, "E1") == 0)
           strcpy(platform, "ERS-1") ;
        else if (strcmp(sat,"e2") == 0 || strcmp(sat,"E2") == 0)
           strcpy(platform, "ERS-2") ;
        else
	{
            syslog(LOG_ERR, 
               "ERROR: Filename %s does not start or end with 'E1' or 'E2'.\n",
               fpmfpair->file_name) ;
            status_all = FALSE;
            break;
	}

#elif NASDA_DM
        strcpy(programName, "NASDAdirmon") ;
        strcpy(platform, "JERS-1") ;
 
#elif WALPS_DM
        strcpy(programName, "WALPSdirmon") ;
        if (*fpmfpair->file_name == NULL)
	{
           first_space = (char *)strchr(fpmfpair->dataset_suffix, ' ');
           platform_len = first_space - fpmfpair->dataset_suffix;
           strncpy(platform, fpmfpair->dataset_suffix, platform_len);
           platform[platform_len] = '\0';
	}
        else
	{
           strcpy (platform, "WFF");
	}

#elif CSA_DM
        strcpy(programName, "CSAdirmon  ") ;
        strcpy(platform, "RADARSAT-1") ;

#elif ADEOS_DM
        strcpy(programName, "ADEOSdirmon") ;
        strcpy(platform, "ADEOS-1") ;
 
#endif

        /* Set dataset */
        if (*fpmfpair->file_name != NULL)
        {
           strcpy(dataset, platform) ;
           strcat(dataset, " ") ;
           strcat(dataset, fpmfpair->dataset_suffix) ;
        }
        else
        {
           strcpy(dataset, fpmfpair->dataset_suffix) ;
        }

        /* Rename the data file (not the PMF), by appending .D to its name. */

        if (*fpmfpair->file_name != NULL)
        {
           strcpy(temp_old_rename, config->FA_receptdir) ;
           strcat(temp_old_rename, "/") ;

           if (fpmfpair->orig_not_tran != TRUE)
              strcat(temp_old_rename, "tran/") ;

           strcat(temp_old_rename, fpmfpair->file_name) ;
           strcpy(temp_new_rename, temp_old_rename) ;
           strcat(temp_new_rename, ".D") ;

           statur = rename(temp_old_rename, temp_new_rename) ;
           if (statur != 0) 
           {
              syslog(LOG_ERR, "ERROR: Failure trying to rename file=%s\n", 
                     temp_old_rename);
              syslog(LOG_ERR,
                     "to new name=%s\n", temp_new_rename);
              status_all = FALSE;
	      break ;
           }
           else /* If rename was successful, change internal names also. */
	   {
              strcat(fpmfpair->file_name, ".D") ;
              strcat(filerecp->file_name, ".D") ;
	   }
        }

        if (*fpmfpair->file_name != NULL)
        {
           syslog(LOG_NOTICE,
                  "ims_archive for file = %s, PMF = %s\n",
            fpmfpair->file_name, fpmfpair->PMF_file_name) ;
        }
        else
        {
           syslog(LOG_NOTICE,
                  "ims_archive for file = %s", fpmfpair->PMF_file_name) ;
        }

        syslog(LOG_NOTICE,
               "platform = %s, dataset_suffix= %s\n",
               platform, fpmfpair->dataset_suffix) ;

     /* The PMF and the original file must both be in the same directory.
        Move the "recept" file down to the /PMF directory. */

        if (*fpmfpair->file_name != NULL)
        {
           strcpy(temp_old_rename_m, config->FA_receptdir) ;
           strcat(temp_old_rename_m, "/") ;

           if (fpmfpair->orig_not_tran == FALSE)
               strcat(temp_old_rename_m, "tran/") ;

           strcat(temp_old_rename_m, fpmfpair->file_name) ;

           strcpy(temp_new_rename_m, ims_directory);
           strcat(temp_new_rename_m, fpmfpair->file_name) ;

           statur = rename(temp_old_rename_m, temp_new_rename_m) ;
           if (statur != 0) 
           {
              syslog(LOG_ERR,
                   "ERROR: Failure trying to rename recpt file=%s to PMFdir\n",
                   temp_old_rename);
              status_all = FALSE;
	      break ;
	   }
	}

        /* Send the file(s) to IMS */
        if (*fpmfpair->file_name != NULL)
        {
           status_ims = ims_archive_FAIF_file(programName,
                                          config->FA_configfile, 
                                          ims_directory, fpmfpair->file_name,
                                          platform, dataset) ;
	}
        else
        {
           status_ims = ims_archive_FAIF_PMF(programName,
                                         config->FA_configfile, 
                                         ims_directory, fpmfpair->PMF_file_name,
                                         platform, dataset) ;
	}

        /* Put the recpt file back where it belongs */
        if (*fpmfpair->file_name != NULL)
        {
           statur = rename(temp_new_rename_m, temp_old_rename_m) ;

           dot_D = (char *) strstr(filerecp->file_name, ".D") ;
           if (dot_D != NULL) strcpy(dot_D, "\0\0") ;

           if (statur != 0) 
           {
              syslog(LOG_ERR,
               "ERROR: Failure trying to rename recpt file=%s to receptdir/\n", 
               fpmfpair->file_name);
              status_all = FALSE;
	      break ;
	   }
	}

        dot_D = (char *) strstr(fpmfpair->file_name, ".D") ;
        if (dot_D != NULL) strcpy(dot_D, "\0\0") ;

        rename(temp_new_rename, temp_old_rename) ;

	 if (status_ims != OK)
	 {
            if (*fpmfpair->file_name != NULL)
            {
	       syslog(LOG_ERR, 
		     "ERROR: Unable to archive file %s to IMS.\n",
		     fpmfpair->file_name) ;
               status_all = FALSE;
               break;
            }
            else
            {
               syslog(LOG_ERR, 
		      "ERROR: Unable to archive file %s to IMS.\n",
		      fpmfpair->PMF_file_name) ;
               status_all = FALSE;
               break;
	    }
	 }
	 else
	 {
            if (*fpmfpair->file_name != NULL)   /* It's a file+PMF pair */
            {
	       syslog(LOG_NOTICE, "NOTICE: Archived file %s to IMS.\n",  
		      fpmfpair->file_name) ;

               /* If this file successfully went to IMS,
                  if it's a translated file, OK to delete it,
                  but if it's the original flight agency file, only delete it 
                  if there were no IMS troubles for any file in the set. 
               */
               if ((fpmfpair->orig_not_tran == FALSE) || status_all)
	       {
                   if (fpmfpair->orig_not_tran == TRUE)
                      syslog(LOG_NOTICE, 
                             "NOTICE: Deleting file from reception area.\n");
                   strcpy(remove_file_dir, config->FA_receptdir) ;
                   strcat(remove_file_dir, "/") ;
                   if (fpmfpair->orig_not_tran == FALSE)
                   strcat(remove_file_dir, "tran/") ;

                   status_u = update_incoming_dir(remove_file_dir, 
	  				 fpmfpair->file_name) ;
                   if (status_u == ERROR)
                      syslog(LOG_ERR, "ERROR: Unable to remove file.\n");
                }
	     }
             else
             {
		syslog(LOG_NOTICE, "NOTICE:  Archived file %s to IMS.\n",
                       fpmfpair->PMF_file_name) ;
	     }

             /* delete the PMF file */
             strcpy(remove_file_name, config->FA_receptdir) ;
             strcat(remove_file_name, "/PMF/") ;
             strcat(remove_file_name, fpmfpair->PMF_file_name) ;
             remove(remove_file_name) ;
          }

          fpmfpair = NEXT(file_pmf_list, pfptr) ;

      }   /* End of loop --  while (pfptr)  */
      return (status_all);
}
