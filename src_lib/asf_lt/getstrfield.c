/*******************************************************************************
FUNCTION:	get_string_field

PURPOSE:	Return string value from Labeled Table I/O buffer

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
   10      02/90  B. ailts      Checked for allocation errors

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
Scans the buffer from current position and returns the string value to the 
application program.  

Note that a field or record separator characters within the character
delimiters (CHAR_DEL) are valid.
*******************************************************************************/
#include "asf.h"



#include "worgen.h"
#include "ltable.h"

char *get_string_field(
    struct TAB_DEF *tab,	/* struct of LT being processed.	      */

    int *eor)			/* End of record flag                         */
				/*   TRUE  - field terminated by record sep.  */
				/*   FALSE - field terminated by field sep.   */
{
int count;			/* loop index counter                         */
int len;			/* character length of ASCII string field     */
int cont;			/* boolean used to detect end of integer field*/
int nmrchr;			/* number of characters in the string field   */
int str_flag;			/* boolean detecting character delimiter      */
char *str;			/* pointer to character string to be returned */
char *temp;			/* temporary pointer to a character string    */

/* This loop searches the I/O buffer until a field or record separator is 
*  reached to find the length of the field.  Note that field or record
*  separator characters within character delimiters are ignored.
*/
if (*(tab->buf.addr + tab->cur) == CHAR_DEL)
   str_flag = FALSE;
else
   str_flag = TRUE;

for (cont = TRUE,len = 0; cont; len++)		
   {				
   if (tab->cur+len>tab->nbytes)
      {
      tab_error(tab,TAB_LT_FORMAT,FATAL,"","get_string_field");
      return(NULL);
      }

/* If not within string delimiters, then if current character is field or
*  record separator, then the length of string field has been found.
*/
   if (str_flag)
      {
      if ((*(tab->buf.addr + tab->cur + len) == FIELD_SEP1) ||
          (*(tab->buf.addr + tab->cur + len) == FIELD_SEP2) ||
          (*(tab->buf.addr + tab->cur + len) == RECORD_SEP))
	      cont = FALSE;
      }

/* If within string delimiters, then if current character is character 
*  delimiter, and the next character is a field or record separator, then
*  the end of character delimeter string has been reached.
*/
   else
      {
      if ((*(tab->buf.addr + tab->cur + len) == CHAR_DEL) &&
         ((*(tab->buf.addr + tab->cur + len + 1) == FIELD_SEP1) ||
          (*(tab->buf.addr + tab->cur + len + 1) == FIELD_SEP2) ||
          (*(tab->buf.addr + tab->cur + len + 1) == RECORD_SEP)))
              str_flag = TRUE;
      }
   }

--len;
str = MALLOC(len + 1);
if (*(tab->buf.addr+tab->cur)==CHAR_DEL)/* if character delimiter is first   */
   {					/*   character in field, then the    */
   count = 1;				/*   actual string is between the    */
   nmrchr = len - 1;			/*   two character delimiters.       */
   }
else					/* else the actual string is all     */
   {					/*   characters in the string field  */
   count = 0;
   nmrchr = len;
   }

for (temp=str;count<nmrchr;count++)	/* copy the string field into str    */
   *temp++ = *(tab->buf.addr + tab->cur + count);

*temp = '\0';
tab->cur += len;
switch (*(tab->buf.addr + tab->cur))	/* Update the I/O pointers            */
   {
   case FIELD_SEP1:

   case FIELD_SEP2:
      *eor = FALSE;
      tab->cur++;
      break;

   case RECORD_SEP:
      *eor = TRUE;
      tab->cur += 2;
      break;

   default:
      tab_error(tab,TAB_LT_FORMAT,FATAL,
                "String field not terminated properly.","get_string_field");
      return(NULL);
      break;
   }

if (strlen(str) > MAX_FIELD_LEN)
   {
   tab_error(tab,TAB_LT_FORMAT,FATAL,"Character string field is too long",
             "get_string_field");
   return(NULL);
   }

if (tab->cur >= tab->nbytes)		/* If required issue a physical read */
   read_tab(tab);

return(str);			/* return the string field                    */
}
