/*==============================================================================
Filename:	checkdir.c

Description:	
	This module contains the check directory function which inspects
a specified directory for file entries

External Functions:
	check_dir
	
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
llist *check_dir(char *) ;
#else
llist *check_dir() ;
#endif

extern File_Record *alloc_file_record() ;
extern File_Record *add_filerec_to_list() ;
extern int          assign_filerec_ftime() ;
extern int          assign_filerec_rtime() ;



/*==============================================================================
Function:	llist * check_dir(char *dir)
Description:

Check the directory dir for incoming files.
   
opendir(directory name)
For each file found in the directory (returned by readdir),
	Perform minimal checks on the entry and report any
	anomalies, ex. check against zero file size.  Also,
	report errors returned by readdir.
			 
	Create a file record by calling alloc_file_record 
					   
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
Creation Date:	11/09/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
llist *
check_dir(char *dir)
#else
llist *
check_dir(dir)
   char *dir ;
#endif
{
   int status ;
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
/*      if (strcmp(dirp->d_name, ".") == 0 || strcmp(dirp->d_name, "..") == 0)*/
         continue ;

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
   return(fllist) ;

} /* check_dir */


/* End of file */
