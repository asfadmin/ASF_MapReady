
/*******************************************************************************
FUNCTION:	get_field_ptr

PURPOSE:	Return pointer to record field of Labeled Table record

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
Searches the Labeled Table label vector to find the specified label name.  
The function returns the pointer to the record structure containing the value 
of the label name.  
If the name is not found, the NULL pointer is returned.  

Function GET_VECTOR and must be called prior to GET_FIELD_PTR.
*******************************************************************************/
#include "asf.h"

#include <ctype.h>

#include "worgen.h"
#include "ltable.h"

struct VECTOR *get_field_ptr(
    struct TAB_DEF *tab,	/* Labeled Table description structure        */
    char *name)			/* Name to search for within label vector     */
{
struct VECTOR *vec;		/* Pointer to follow the column linked list   */

for (vec=tab->vector; vec!=NULL; vec=vec->next)
   if (strcmp(name,vec->label) == 0)
      return(vec);

return(NULL);
}
