/******************************************************************************
FUNCTION:	c_egclse

PURPOSE		closes the group after all processing is done

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	   06/85    T. Butzer		Original implementation
  1.1	   03/86    T. Butzer		dealloc the buffer space used
  1.2	   03/86    K. Johnson		handle multiple file groups
  5.0	   12/87    B. Ailts		change include directory specifications
					change to raw 'C' types
					place bridge routines is seperate file
  5.1	   04/88    D.Hollaren		Replaced newlas.h with las.h
					Replaced cerrmsg with c_errmsg
  5.2      10/88    B. Ailts		Added statements to free dynamic
					memory that was used internal to imageio
  5.3      02/91    B. Ailts		Standardized error messages
  5.4	   10/92    T. Mittan		Added compression option
  5.5	   12/92    T. Mittan		Freed memory that was not being freed.	
	    1/94    K.Gacke		Modified to read/write dal images.  TAE
					images can be read, but all images are
					written as a BSQ DAL image.
  7.0	    4/95    T. Logan (ASF)	Removed ability to process compressed
					or TAE files, removing TAE dependency

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT			LAS

ALGORITHM 
	For each file
	   Close the file
	   If access equals update
	      Free the update and conversion buffers
	   else
	      Free the buffer and conversion buffers
	   Free the file descriptors
	Free the group descriptors
	Return

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include <unistd.h>
#include "las.h"
#include "locinc.h"

lasErr FUNCTION c_egclse(struct GDESC **gdesc)
{
short i;
short j;

/* call xiclse for each file in group */

for (i=0; i<(*gdesc)->nfiles; i++)
{
	close((*gdesc)->fdarray[i]->dalfd);
	c_dkfree((*gdesc)->fdarray[i]->fname);

   /*  Free the update and conversion buffers
   -----------------------------------------*/
   if ((*gdesc)->fdarray[i]->acc == IUPDATE)
      {
      if ((*gdesc)->fdarray[i]->flags & CONVERSION)
         for (j = 0; j < (*gdesc)->fdarray[i]->nbands; j++)
	    free((*gdesc)->fdarray[i]->upbuf[j].conbuf);
      free((*gdesc)->fdarray[i]->upbuf);
      }  /*  if access equals UPDATE  */
   else
      {
      if ((*gdesc)->fdarray[i]->flags & CONVERSION)
         for (j = 0; j < (*gdesc)->fdarray[i]->nbands; j++)
	    free((*gdesc)->fdarray[i]->buf[j].conbuf);
      free((*gdesc)->fdarray[i]->buf);
      }  /*  else access not equals UPDATE  */
	    
   /*  free the file descriptors  */
   free((*gdesc)->fdarray[i]);
   }  /*  for gdesc->nfiles  */

/*  Free the group descriptor  */
free(*gdesc);
return(E_SUCC);
}
