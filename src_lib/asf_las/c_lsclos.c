/*********************   Label Services Subsystem   ***************************
FUNCTION:	lsclos

PURPOSE:	Close an associated file.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
	   04/86    C. Greenhagen	initial development
	   07/86    K. Gacke		newLAS development
	   12/87    B. Ailts		change include directory specifications
					move bridge routines to seperate file
					replace DESC arguments with char strings
					use raw 'C' types
   5.0     04/88    D. Hollaren		changed include spec to las
					changed cerrmsg to c_errmsg and
					passes error code by address
					return values E_SUCC/E_FAIL
  5.1     11/88	   B. Ailts		Replaced las.h with worgen.h so these
					routines can be placed in world
  5.2     11/88    B. Ailts		Now does not create a new version when
					compacting a file
  5.3     07/90    D. VanderZee         Standardized error handling
  5.4     01/91    K. Gacke		Changed i/o to buffered i/o for sun --
					sun has a problem with direct i/o when
					the close takes to int to be executed
  5.5     05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  5.6	  09/92	   T. Mittan		Type cast constant 0 to (char *) to 
					eliminate warnings on compile.
  5.7     03/93    D. Etrheim           Changed pmode to 666 for file open.
					This will allow users to use umask
					appropriately. Execute permision is not
					needed for any files created by LAS.
  6.0	  6/97     O. Lawlor            Changed to standard I/O.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
There are three ways that an associated file can be closed (normal, crunch, and
delete).
Case 0:
    Normal close of the file
    report any errors closing

Case 1:
    Seek to the beginning of the file
    Open a temporary file
    For each record in the assoicated file
	read the header information
	check to see if it has the "DELETED" flag
	Skip any records that are deleted or
	read the data and write it to the temp output file
   If the temp output file is not empty
	rename it to input file name
   else
	delete the associated and temp files

Case 2:
    Delete the associated file
    Report any errors deleting the file.

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"

#include <unistd.h>

#include "vll.h"
#include "worgen.h"

lasErr FUNCTION c_lsclos (FILE **fd, const char *hostname, int *action)
{
FILE *fdtemp;
int	clen, dlen, total_len, empty,  more, nbytes;
int	buf_size = 1024;
char	tempfile[CMLEN];
char	*buffer;
char    dtype[TYPL];
char	header[HDRL];
char	*key=NULL;
char	msgtxt[ERRLEN + 1];
char	*ptr;

switch (*action)
    {
    case 0:					/***  normal close    ***/
	FCLOSE(*fd);
	break;

    case 1:					/***  crunch file     ***/
	FSEEK(*fd, (int)0, 0);

	strcpy(tempfile,hostname);
	strcat(tempfile,"_temp");

	fdtemp = FOPEN (tempfile, "w");

	buffer = MALLOC((unsigned)buf_size);

	empty = 1;
	more = 1;
	while (more)
	    {
 	    nbytes = fread(header,sizeof(char),HDRL,*fd);
	    if (nbytes == 0)
	       break;

	    if (nbytes == -1)
	       {
	       c_errmsg("Error reading header record from label services file",
                         "lsclos-read",NON_FATAL);
	       return(E_FAIL);
	       }

	    clen = dlen = 0;
            if (((ptr = strchr(header,'/')) != NULL ) && ((int)(ptr - header) < LENL))
                sscanf(header,"%d/%d%s",&clen,&dlen,dtype); 
            else
                sscanf(header,"%d%s",&dlen,dtype); 

            key = squeeze(header+LENL+TYPL,strlen(header+LENL+TYPL));
	    total_len = clen + dlen;

	    if (strcmp(key,"DELETED") != 0)		/* copy record	*/
		{					/* to new file	*/
		if (total_len > buf_size)
		    {
		    free(buffer);
		    buf_size = total_len;

		    buffer = MALLOC((unsigned)buf_size);
		    }

		FREAD(buffer,1,total_len,*fd);

		FWRITE(header,1,HDRL,fdtemp);
		FWRITE(buffer,1,nbytes,fdtemp);

		empty = 0;
		}
	    else					/* skip record	*/ 
		{
		if (fseek(*fd, (int)total_len, 1) == -1)
		    {
		    c_errmsg("Error seeking in label services file",
			     "lsclos-seek",NON_FATAL);
		    return(E_FAIL);
		    }
		}
	    }

        free(buffer);

	if (fclose(*fd) != 0)
	    {
	    c_errmsg("Error closing label services file","lsclos-close",
			NON_FATAL);
	    return(E_FAIL);
	    }

	if (fclose(fdtemp) != 0)
	    {
	    sprintf(msgtxt,"Error closing temporary file %s",tempfile);
	    c_errmsg(msgtxt,"lsclos-close",NON_FATAL);
	    return(E_FAIL);
	    }

	 if (unlink(hostname) == -1)
	     {
	     sprintf(msgtxt,"Error deleting label services file %s",
		     hostname);
	     c_errmsg(msgtxt,"lsclos-delete",NON_FATAL);
	     return(E_FAIL);
	     }

	if (!empty)					/***  delete file   ***/
	   {
	   if (rename(tempfile,hostname) != E_SUCC)
	       {
	       sprintf(msgtxt,"Error renaming file %s to %s",
		        tempfile,hostname);
	       c_errmsg(msgtxt,"lsclos-rename",NON_FATAL);
	       return(E_FAIL);
	       }
           }
	else
	   if (unlink(tempfile) == -1)
	      {
	      sprintf(msgtxt,"Error deleting label services file %s",
		     hostname);
	      c_errmsg(msgtxt,"lsclos-delete",NON_FATAL);
	      return(E_FAIL);
	      }

	free(key);
	break;

    case 2:					/***  delete file     ***/
	if (unlink(hostname) == -1)
	    {
	    sprintf(msgtxt,"Error deleting label services file %s",
		    hostname);
	    c_errmsg(msgtxt,"lsclos-delete",NON_FATAL);
	    return(E_FAIL);
	    }
	break;
    }

return(E_SUCC);
}

