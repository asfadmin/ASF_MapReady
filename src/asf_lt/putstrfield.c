/*******************************************************************************
FUNCTIONS:	put_string_field	

PURPOSE: 	Write string value into Labeled Table I/O buffer

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
   10      03/93  D. Etrheim    Subtracted one from TAB_SIZ so that "\0"
                                can be appended to the end of the string


COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
FUPUT_STRING_FIELD writes a character string into the Labeled Table I/O buffer
If the buffer is full 
   A physical write is issued.
*******************************************************************************/
#include "asf.h"

#include <ctype.h>

#include "worgen.h"
#include "ltable.h"

void put_string_field(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed. */
    char *val,			/*character string  written into I/O buffer   */
    char sep,			/*field or record separator to be written to  */
				/*the I/O buffer after the integer.	      */
    int flag)			/*flag specifying whether or not the string is*/
				/*to be delimited by CHAR_DEL. 	   	      */
				/* 1 --> string is to be delimited by CHAR_DEL*/
				/* 0 --> string not to be delimited   	      */
{
static char temp[MAX_FIELD_LEN];
int len;

if (flag)				/* character delimiters included      */
   sprintf(temp,"%c%s%c%c",CHAR_DEL,val,CHAR_DEL,sep);
else					/* character delimiters not included  */
   sprintf(temp,"%s%c",val,sep);
if (sep == RECORD_SEP)			/* if record separator, then append   */
   strcat(temp,"\n");			/*   the new line character.          */
len = strlen(temp);
if (tab->cur + len > (TAB_SIZ - 1))	/* if buffer is full, then issue a    */
   write_tab(tab);			/*   physical write.                  */
strcpy(tab->buf.addr+tab->cur,temp);	/* add string to the I/O buffer       */
tab->cur += len;			/* update the current I/O pointer     */
return;
}
