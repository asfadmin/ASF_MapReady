/*******************************************************************************
FUNCTIONS: 	put_null_field	

PURPOSE: 	Write a null field into Labeled Table I/O buffer

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

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
PUT_NULL_FIELD writes a null field into the Labeled Table I/O buffer.  
If the buffer is full, a physical write is issued.
*******************************************************************************/
#include "asf.h"


#include "worgen.h"
#include "ltable.h"

void put_null_field(struct TAB_DEF *tab,char sep)
{
int len;
static char temp[10];

if (sep == RECORD_SEP)			/* store value into temporary buffer  */
   sprintf(temp,"%c\n",sep);
else
   sprintf(temp,"%c",sep);
len = strlen(temp);
if (tab->cur + len > TAB_SIZ)		/* if buffer is full, then issue a    */
   write_tab(tab);			/*   physical write command.          */
strcpy(tab->buf.addr+tab->cur,temp);	/* add string to the I/O buffer       */
tab->cur += len;			/* update the current I/O pointer     */
return;
}
