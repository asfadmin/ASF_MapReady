/*==============================================================================
Filename:	checkFAdir.c

Description:	
	This module contains the check directory function which inspects
a specified directory for flight agency file entries

External Functions:
	check_FA_dir
	update_incoming_dir
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
1.  Oct. '96 - RH
    Eliminate check_4_DT().  Not needed with wholesale WALPS revisions.
==============================================================================*/

static char SccsFile[] = "checkFAdir.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "02 May 1996";
static char SccsLastChanger[] = "@(#)checkFAdir.c	1.4";
static char SccsState[] = "1.4";

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include "filerec.h"
#include "dapps_list.h"
#include "nmalloc.h"

int     llist_errno ;


#ifdef __STDC__
llist *check_FA_dir(char *, int) ;
int    update_incoming_dir(char *, char *) ;
#else
llist *check_FA_dir() ;
int    update_incoming_dir() ;
#endif

extern File_Record *alloc_file_record() ;
extern File_Record *add_filerec_to_list() ;
extern int          assign_filerec_ftime() ;
extern int          assign_filerec_rtime() ;
extern int          assign_filerec_ftype() ;


/*==============================================================================
Function:	llist * check_FA_dir(char *dir)
Description:

Check the directory dir for incoming files.
   
opendir(directory name)
For each file found in the directory (returned by readdir),
	Perform minimal checks on the entry and report any
	anomalies, ex. check against zero file size.  Also,
	report errors returned by readdir.
			 
	Create a file record by calling alloc_file_record and
	determine and assign its file type and flight agency fields
	using the function assign_filerec_ftype.
					   
	Add each created file record to the file record list through
	add_filerec_to_list.  At the end of this routine, a file
	record list representing the files found in dir is set up
	and is passed back to the calling function.
								   
	If an error occurs during the creation of the list, the ASF
	standard error routine is called and NULL is returned.
	(Therefore, either there is a complete list or there is no
	list at all.)
Endfor
closedir(directory structure for in directory)

Parameters:
	char *dir - name of directory to be checked

Returns:	Linked list of file records representing file entries in dir	
Creator:	Norbert Piega	
Creation Date:	08/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
llist *
check_FA_dir(char *dir, int fa_code)
#else
llist *
check_FA_dir(dir, fa_code)
   char *dir ;
   int fa_code ;
#endif
{
   int status;
   int fcount = 1 ;                   /* count of files found in IN dir */
   DIR *dp ;                           /* structure returned by opendir */
   struct dirent *dirp ;         /* directory entry returned by readdir */
   struct stat statbuf ;              /* structure containing file stat */
   llist *fllist ;                 /* holds linked list of file records */
   File_Record *filerec = NULL ;            /* pointer to a file record */
   char current_dir[MAX_DIRNAME_LEN+1] ;   /* name of current directory */
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;   /* string containing syslog msg */

   /* Open directory file 
   */
   if ((dp = opendir(dir)) == NULL)
   {
      sprintf(logmsg, "WARNING, Error opening directory %s\n", dir) ;
      syslog(LOG_DEBUG, logmsg) ;
      return(NULL) ;
   }

   /* Save current working directory
   */
   getcwd(current_dir, MAX_DIRNAME_LEN) ;

   /* Go to directory to check
   */
   if (chdir(dir) < 0) 
   {
      syslog(LOG_DEBUG, "WARNING, Change directory operation failed\n") ;
      return(NULL) ;
   }

   /* Create linked list of file records
   */
   fllist = create_dyn_llist() ;

   /* Get directory contents 
   */
   while ((dirp = readdir(dp)) != NULL)
   {
      /* Ignore dot and dot dot 
      */
      if (dirp->d_name[0] == '.')
/* if (strcmp(dirp->d_name, ".") == 0 || strcmp(dirp->d_name, "..") == 0) */
         continue ;

/* Check for ADEOS ELM files, which "may" need a "****" string in the
   header to be replaced with "ASF ". */
/*
      if (strcmp(dir + strlen(dir) -5, "ADEOS") == 0)
      {
         if (check_4_ELM(dirp->d_name)) check_ELM_header(dirp->d_name);
      }
*/
      stat(dirp->d_name, &statbuf) ;
      if (S_ISDIR(statbuf.st_mode))
	 continue ;

      /* Allocate new file record structure 
      */
      filerec = (File_Record *) alloc_file_record() ;
      if (filerec != (File_Record *)NULL)
      {
         /* Assign values for file record fields
	 */
         strcpy(filerec->file_name, dirp->d_name) ;

	 /* Check if zero length file
	 */
	 stat(filerec->file_name, &statbuf) ;
	 if ((int)statbuf.st_size == 0)
	 {
	    free(filerec) ;
	    sprintf(logmsg, "WARNING, Skipping zero length file %s\n",
	       dirp->d_name) ;
	    syslog(LOG_WARNING, logmsg) ; 
	    continue ;
	 }

         /* Assign file type for file record
	 */
	 if ((status = assign_filerec_ftype(filerec, fa_code)) == ERROR)
	 {
	    free(filerec) ;
	    sprintf(logmsg, "WARNING, Skipping unrecognized file %s\n",
	       dirp->d_name) ;
	    syslog(LOG_WARNING, logmsg) ; 
	    continue ;
         }

	 /* Assign received time for file record
	 */
	 if ((status = assign_filerec_rtime(filerec)) == ERROR)
         {
	    free(filerec) ;
	    sprintf(logmsg, 
	       "WARNING, Unable to assign received time for file %s\n",
	       dirp->d_name) ;
	    syslog(LOG_ERR, logmsg) ; 
	    continue ;
         }

         /* Init dest to NULL 
	 */
         *filerec->file_dest = NULL ;

         /* Add file record to file list 
	 */
         APPEND(fllist, filerec, free, filerec) ;

         fcount++ ;

      } /* endif filerec NOT NULL */
   } /* endwhile */

   /* Pop back to current working dir
   */
   if (chdir(current_dir) < 0) 
      syslog(LOG_DEBUG, "WARNING, Change directory operation error\n") ;

   /* Close directory file 
   */
   if (closedir(dp) < 0)
   {
      sprintf(logmsg, "Error closing directory %s\n", dir) ; 
      syslog(LOG_DEBUG, logmsg) ; 
   }

   /* Return FILE LIST 
   */
   if (filerec == (File_Record *)NULL)
      return(NULL) ;
   else
      return(fllist) ;

} /* check_FA_dir */





/*==============================================================================
Function:	int update_incoming_dir(dir, filename)

Description:
	This function updates the incoming directory dir by removing
the file filename from dir.  This operation is performed after file
filename has been routed successfully and listed as routed in the log
file.  If this operation is not able to perform its task, ERROR is
returned to the calling routine, otherwise SUCCESS is returned.

Parameters:
	char *dir - name of directory to be updated
	char *filename - name of file to be removed from dir

Returns:
	OK - update operation succeeded
	ERROR - error removing file from incoming directory

Creator:	Norbert Piega	
Creation Date:	08/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
update_incoming_dir(char *dir, char *filename)
#else
int
update_incoming_dir(dir, filename)
   char *dir ;
   char *filename ;
#endif
{
   int status ;
   char current_dir[MAX_DIRNAME_LEN+1] ;

   /* Save current working directory
   */
   getcwd(current_dir, MAX_DIRNAME_LEN) ;

   /* Goto directory to update
   */
   chdir(dir) ;

   /* Make file inaccessible by calling remove
   */
   status = remove(filename) ; 
   if (status == -1)
   {
      syslog(LOG_ERR, "WARNING, Update incoming directory operation failed\n") ;
      return(ERROR) ;
   }

   /* Go back to the current working directory
   */
   chdir(current_dir) ;
 
   return(OK) ;

} /* update_incoming_dir */

/*==============================================================================

Description:
      Check that the input filename exactly matches the form of an ADEOS
      ELM file.

Creator:	Rich Norman
Creation Date:	960430
==============================================================================*/
int check_4_ELM(char * filename)

{
   int status;

   status = 1;

   if (strlen(filename) != 10) 
   {
      status = 0;
      goto done;
   }

   if (strncmp("ELM", filename, 3) !=0)
   {
      status = 0;
      goto done;
   }

   if (filename[3] != 'D' && filename[3] != 'P')
   {
      status = 0;
      goto done;
   }

   done:
   return (status);
}

/*==============================================================================
Filename:	fix_ELM_header.c

Description:	
	This module contains the fix_ELM_header function.

External Functions:
	
Static Functions:
        	
External Variables Defined:
	
File Scope Static Variables:
	
Description:
        Check the header of an ADEOS ELM file. If the "format description"
        filed is "****", change it to "ASF ".

Parameter:      Name of ADEOS ELM file
Creator:	Rich Norman
Creation Date:	960430
==============================================================================*/

int check_ELM_header(char *file_name)

{

   FILE *infp;
   int  status, i, j, k;
   char line[MAXLINE], no_white[MAXLINE];

   status = 1;

/* Open file to read AND write. */
   if ((infp = fopen(file_name, "r+")) == (FILE *)NULL)
   {
      printf("Error in fix_ELM_header... unable to open file = %s\n", file_name) ;
      status = ERROR ;
      goto done;
   }

/* Position to the format part of the header, and obtain the 4 bytes. */
   i = fseek(infp, 23, SEEK_SET);
   if (fgets(line,  5, infp) == NULL)
   {
      syslog(LOG_ERR, "Unable to read header of %s\n", file_name) ;
      status = ERROR;
      goto done;
   }

   if (strncmp("****", line, 4) ==0)
   {
      i = fseek(infp, 23, SEEK_SET);
      if (fputs("ASF ", infp) == NULL)
      {
         syslog(LOG_ERR, "Unable to rewrite header of %s\n", file_name) ;
         status = ERROR;
         goto done;
      }
   }

   fclose (infp);

   done:
   return (status);
}
/* End of file */
