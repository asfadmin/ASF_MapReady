/*******************************************************************************
FUNCTION:	close_tab

PURPOSE: 	Closes Labeled Table files

PROGRAM HISTORY:
  Version  Date   Author       	Request
  -------  ----	  ------    	-------
    1      12/86  K. Gacke      Original development
    2	    3/87  K. Gacke	Modified open_tab to allow directories to be 
				specified for the subfiles.  Replaced "strcat" 
				calls with "strcpy" to make the code more 
				efficient.  Replaced "sscanf" calls with "atol"
				and "atof" to make the code more efficient.
    3       5/87  K. Gacke      Modified "put_vector" to search the link list 
				for each attribute defined.
    4	    6/87  K. Gacke	Allow the LT to be opened for append.  Modified
				"close_tab", "get_vector", "open_tab", and 
				"put_vector".
    5	    7/87  K. Gacke	changed the calls malloc and free to xalloc and
				xree for the sel.  These routines are the C run
				time routines renamed and put into the newlas.a
				library to avoid linking the malloc and free in
				the fortran libraries.
    6	    8/87  K. Gacke	added the DATE and TIME data types.  Modified
				the routines get_vector, get_record, put_record,
				and put_vector.  Created the routines
				get_lt_date, get_lt_time, put_lt_date, and
				put_lt_time.
    7       9/87  K. Gacke	removed mod for "version 5" after installing
				new OS (UTX 2.0)
    8      12/87  B. Ailts	Change include directory structure
    9      05/88  B. Ailts	Standardized err messages
				Split routines into separate files
				Added worgen.h
    9      12/92  J. Hemmer	Freed vector data pointer in free_vec.  Also
				freed vector root node.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
If opened for write access, flush the I/O buffers prior to the close.  
Close the file
Free the dynamic memory used for the I/O buffers and the logical record
linked list.
*******************************************************************************/
#include "asf.h"


#include <fcntl.h>
#include <unistd.h>

#include "worgen.h"
#include "ltable.h"

void close_tab(struct TAB_DEF *tab)	/* Structure defining the Labeled Table files */
{
char msg[ERRLEN];

if ((tab->access == 1) || (tab->access == 2))	/* if opened for write access,*/
   write_tab(tab);				/*   flush the I/O buffers.   */

if (cltab(&tab->fd) == E_FAIL)			/* Close the Label Table files*/
   {
   sprintf(msg,"Error closing Labeled Table file %s.",tab->hname);
   tab_error(tab,TAB_LT_CLOSE,FATAL,msg,"close_tab");
   return;
   }

free(tab->buf.addr);				/* free dynamic I/O buffers   */
free_vec(tab->vector);				/* free dynamic linked list   */
free(tab->vector);
return;
}

/******************************************************************************
FUNCTION:	cltab

ALGORITHM:	Closes the Labeled Table file
******************************************************************************/
lasErr cltab(int *fd)		/* File descriptor (created by "optab")	(input)      */
{
if (close(*fd) == -1)
   return(E_FAIL);		/* Error closing Labeled Table file	*/

return(E_SUCC);
}

/*******************************************************************************
FUNCTION:	free_vec

ALGORITHM:	
Free the dynamic linked list of the LT vector.  
It is called when the LT file is closed.  
NOTE: That this is a recursive
function, which follows the linked list freeing the label
name and file description.  Once the end of the linked list
is reached, the linked list node is freed.
*******************************************************************************/
void free_vec(struct VECTOR *vec)		/* Labeled Table vector			      */
{

if (vec == NULL)
   return;

if (vec->label != NULL)
   free(vec->label);

if (vec->desc != NULL)
   free(vec->desc);

switch (vec->dtype)
   {
   case I1:
      free(vec->ptr.i1);
      break;

   case I2:
      free(vec->ptr.i2);
      break;

   case I4:
      free(vec->ptr.i4);
      break;

   case R4:
      free(vec->ptr.r4);
      break;

   case R8:
      free(vec->ptr.r8);
      break;

   case C:
   case LTDATE:
   case LTTIME:
      free(vec->ptr.c);
      break;
   }

if (vec->next != NULL)
   {
   free_vec(vec->next);
   free(vec->next);
   }

return;
}
