/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* op_futs.c - This module contains file and directory
   utilities.

*/

#include <procfil.h>
#include <procdec.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>


FILE   *FilePtr;

/* redefine for group access */

#define FMODE 0775

/* op_findfile(filename)------------------------------------
	This routine opens the file "filename" in read mode
	and if it exists, the file size is returned.  If
	the file does not exist, it returns FAIL.
*/

long op_findfile(filename)
char    filename[PATHLEN];
{
    long    size;
    if ((FilePtr = fopen(filename,"rb")) != NULL)  /* File exists */
    {
	fseek(FilePtr,0,SEEK_END);  /* Go to end of file */
        size = ftell(FilePtr);      /* Get size of file  */
	fclose(FilePtr);
        return(size);
    }
    return(FAIL);  /* File does not exist */
}



/* op_mkdir(dirname)---------------------------------------
	This routine makes a directory if it doesn't
	already exist.
*/

int op_mkdir(dirname)
char    dirname[PATHLEN];
{
    char    *getcwd();
    char    cur_dir[PATHLEN];

    umask(FMODE_MASK); /* Set file mode mask in case it's not correct */

    /* Attempt to make directory */
    if (mkdir(dirname,FMODE) == FAIL) /* Unable to make dir */
    {
	if (errno != 17)  /* System error */
	    {
	    perror("op_mkdir ERROR needs expert attention");
            return(ABORT);
	    }
    }

    strcpy(cur_dir,getcwd(cur_dir,PATHLEN));  /* Save current dir */

    /* Change to directory */
    if (chdir(dirname) == FAIL)
	{
	perror("op_mkdir ERROR needs expert attention");
	return(ABORT);   /* Unable to change directories */
	}
    
    chdir(cur_dir);  /* Change back to previous directory */
    return(PASS);
}



/* op_finddir(dirname)--------------------------------------
	This routine verifies that a directory exists.
*/

int op_finddir(dirname)
char    dirname[PATHLEN];
{
    char    cur_dir[PATHLEN];
    char    *getcwd();

    strcpy(cur_dir,getcwd(cur_dir,PATHLEN));  /* Save current dir */

    if (chdir(dirname) == FAIL)
    {
	if (errno != 2)    /* System error */
	{
            perror("op_finddir ERROR needs expert attention");
	    return(ABORT);
        }
        else
	    return(FAIL);  /* Directory does not exist */
    }

    chdir(cur_dir);  /* Change back to old directory */
    return(PASS);
}
