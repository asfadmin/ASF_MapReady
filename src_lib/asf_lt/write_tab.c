/*******************************************************************************
FUNCTIONS:	write_tab		

PURPOSE:	Physical write from I/O buffer into Labeled Table file

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
   10      05/90  B. Ailts	Added c_sigblk and c_sigunblk in order
 				that the interupts produced by the 
				processing message does corrupt the I/O.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
Issues a physical write from the working I/O buffer into the Labeled Table file.
*******************************************************************************/
#include "asf.h"

#include <fcntl.h>
#include <unistd.h>

#include "worgen.h"
#include "ltable.h"

void write_tab(
    struct TAB_DEF *tab)	/*structure defining LT file to be processed. */
{
if (wrtab(&tab->fd,&tab->cur,&tab->buf) != E_SUCC)
   {
   tab_error(tab,TAB_LT_WRITE,FATAL,"","write_tab");
   return;
   }
*(tab->buf.addr) = '\0';
tab->cur = 0;
return;
}

/******************************************************************************
FUNCTION:	wrtab

ALGORITHM:	Writes to the specified Labeled Table file.
******************************************************************************/
lasErr wrtab(
    int *fd,			/* File descriptor array   	(input)      */
    int *nbytes,		/* Number of bytes to write	(input)      */
    struct DESC *buffer)	/* Buffer to write data from	(input)	     */
{
static int curlen   = 0;
static int str_flag = TRUE;
char *bufptr;
char *prev;

/*  Scan the buffer that is to be written to the Labeled Table.  If the
*   number of bytes between new line characters (FIELD_SEP1) is greater
*   than LIN_SIZ, then the previous FIELD_SEP2 is replaced with a new
*   new line character.  */
prev = NULL;
for (bufptr=buffer->addr; bufptr < buffer->addr + *nbytes; bufptr++)
   {
   if (str_flag)
      {
      switch (*bufptr)
         {
         case CHAR_DEL:
	   str_flag = FALSE;
	   break;

         case FIELD_SEP2:
	   curlen = 0;
	   prev = NULL;
	   break;

	 case FIELD_SEP1:
	   prev = bufptr;
         }
      }
   else if (*bufptr == CHAR_DEL)
      str_flag = TRUE;

   curlen++;
   if ((curlen > LIN_SIZ) && (prev != NULL))
      {
      curlen = bufptr - prev;
      *prev = FIELD_SEP2;
      prev = NULL;
      }
   }

if (write(*fd,buffer->addr,*nbytes) != *nbytes)
   return(E_FAIL);

return(E_SUCC);
}
