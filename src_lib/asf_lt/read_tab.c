/*******************************************************************************
FUNCTIONS:	read_tab		

PURPOSE:	Physical read from Labeled Table file into I/O buffer

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
*   READ_TAB issues a physical read from the Labeled Table file into the 
*   working I/O buffer.
*******************************************************************************/

#include "worgen.h"
#include "ltable.h"
#include <unistd.h>
#include <fcntl.h>

void read_tab(struct TAB_DEF *tab)	/*structure defining LT file to be processed */
{
int size = TAB_SIZ;

if ((tab->nbytes = rdtab(&tab->fd,&size,&tab->buf)) == E_FAIL)
   {
   tab_error(tab,TAB_LT_READ,FATAL,"","read_tab");
   return;
   }
tab->cur = 0;
return;
}

/******************************************************************************
FUNCTION:	rdtab

ALGORITHM:	
Reads from the specified Label Table file
The file pointer is left positioned at the beginning of a logical labeled 
table record field
The number of bytes actually read is returned
If an End Of File is encountered 
   Zero number of bytes will be returned.
******************************************************************************/
int rdtab(
    int *fd,			/* File descriptor array (Created by "optab") */
    int *nbytes,		/* Number of bytes to read      (input)	      */
    struct DESC *buffer)	/* Buffer to read data into	(output)      */
{
int bytes_read;
char *bufptr;

if ((bytes_read = read(*fd,buffer->addr,*nbytes)) == -1)
   return(E_FAIL);

if (bytes_read == 0)		/* EOF encountered            		*/
   return(bytes_read);		/* Return zero number of bytes read	*/

/*  Backspace to the beginning of the last record field.  Call lseek to move
*   the file pointer to the correct position and reset the return value of
*   "bytes_read".  */
for (bufptr=buffer->addr+bytes_read-1; 
     (*bufptr!=RECORD_SEP) && (*bufptr!=FIELD_SEP1) && (*bufptr!=FIELD_SEP2); 
     bufptr--)
   {
   if (bufptr < buffer->addr)		/* LT record field is too large       */
      return(E_FAIL);
   }

if (*bufptr == RECORD_SEP)
   bufptr += 2;
else
   bufptr++;

lseek(*fd,bufptr - buffer->addr - bytes_read,1);
bytes_read = bufptr - buffer->addr;

/*  Search the buffer just read for new line characters (FIELD_SEP2) and 
*   replace them with FIELD_SEP1.  If the new line character is within a 
*   character string delimited by quotes, it is unchanged.  */
/**************************************************************************
for (bufptr=buffer->addr; bufptr < buffer->addr + bytes_read; bufptr++)
   {
   if (str_flag)
      {
      switch (*bufptr)
         {
         case CHAR_DEL:
	   str_flag = FALSE;
	   break;

	 case FIELD_SEP2:
	   if (*(bufptr - 1) != RECORD_SEP)
	      *bufptr = FIELD_SEP1;
	   break;
         }
      }
   else if (*bufptr == CHAR_DEL)
      str_flag = TRUE;
   }
****************************************************************************/

return(bytes_read);
}
