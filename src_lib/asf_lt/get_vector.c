/*******************************************************************************
FUNCTION:	get_vector

PURPOSE:	Return label vector as a linked list from I/O buffer

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
  10       02/90  B.Ailts       Checked for allocation errors

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
Assigns the label vector to the Labeled Table structure.  
The label vector is stored in a linked list within the TAB_DEF structure.  
It also allocates the linked list which stores the contents of a Labeled 
Table record.  

GET_VECTOR must be called prior to calling GET_RECORD.
*******************************************************************************/
#include "asf.h"


#include <ctype.h>

#include "worgen.h"
#include "ltable.h"

void get_vector(
    struct TAB_DEF *tab)	/* struct of LT file to be processed          */
{
struct VECTOR *next_vec;	/* pointer to next label vector               */
struct VECTOR *vec=NULL;	/* pointer to current label vector            */
int eor;			/* flag indicating end or record detected     */
int index;			/* loop index counter                         */
int null;			/* flag indicating null value has been read   */
int size;			/* number of entities to allocate             */
char *dtype;			/* data type of the current label name        */
char msg[ERRLEN];		/* error message buffer                       */

++tab->currec;
strcpy(msg,
       "Unexpected end of record encountered while reading the label vector.");

for (index=0,eor=FALSE; eor==FALSE; index++)
  {
  if ((next_vec = (struct VECTOR *) malloc(sizeof(struct VECTOR))) == NULL)
     {
     tab_error(tab,TAB_ALLOC,FATAL," ","get_vector");
     return;
     }
     
  next_vec->label = get_string_field(tab,&eor);	/* get label name of vector   */
  if (eor)
     {
     tab_error(tab,TAB_LT_EOR,FATAL,msg,"get_vector");
     return;
     }

  dtype = get_string_field(tab,&eor);		/* get data type of vector    */
  for (next_vec->dtype=0; next_vec->dtype<NDTYPE; next_vec->dtype++)
     if (strcmp(dtype,DATA_TYPE[next_vec->dtype]) == 0)
        break;

  if (eor)
     {
     tab_error(tab,TAB_LT_EOR,FATAL,msg,"get_vector");
     return;
     }

  next_vec->desc  = get_string_field(tab,&eor);	/* get label desc of vector   */
  if (eor)
     {
     tab_error(tab,TAB_LT_EOR,FATAL,msg,"get_vector");
     return;
     }

  get_i4_field(tab,&next_vec->lt_size.nrow,&null,&eor);  /* get # of rows     */
  if (eor)
     {
     tab_error(tab,TAB_LT_EOR,FATAL,msg,"get_vector");
     return;
     }

  get_i4_field(tab,&next_vec->lt_size.ncol,&null,&eor);  /* get # of columns  */
  next_vec->sep = FIELD_SEP1;				 /* type of separator */
  size = next_vec->lt_size.nrow * next_vec->lt_size.ncol;
  next_vec->flag = FALSE;		/* flag used when opened for append   */

  switch (next_vec->dtype)		/* allocate memory to hold values for */
     {					/*   the record field                 */
     case I1:
        next_vec->ptr.i1 = MALLOC(sizeof(char) * size);
        break;

     case I2:
        next_vec->ptr.i2 = (short *) MALLOC(sizeof(short) * size);
	break;

     case I4:
        next_vec->ptr.i4 = (int *) MALLOC(sizeof(int) * size);
	break;

     case R4:
       next_vec->ptr.r4 = (float *) MALLOC(sizeof(float) * size);
	break;

     case R8:
     next_vec->ptr.r8 = (double *) MALLOC(sizeof(double) * size);
	break;

     case C:

     case LTDATE:

     case LTTIME:
        next_vec->ptr.c = MALLOC(MAX_FIELD_LEN);
	break;

     default:
        tab_error(tab,TAB_LT_DTYPE,FATAL,"","get_vector");
	return;
	break;
     }

  next_vec->next = NULL;
  if (index == 0)				/* assign the current vector  */
     vec = tab->vector = next_vec;		/*   to the end of the linked */
  else						/*   list.                    */
     vec = vec->next = next_vec;
  }

tab->ncol = index;
return;
}
