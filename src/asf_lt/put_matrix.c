/******************************************************************************
FUNCTION:	put_x_matrix		

PURPOSE: 	Write matrix into Labeled Table I/O buffer

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
PUT_MATRIX copies the matrix from the application programs buffer into the
LT record structure.  The matrix is converted to the correct data type when
copied to the LT record structure.
    Arguments:
 	*vec	VECTOR	  I/O	structure defining the record structure to copy
 				the matrix into
 	idtype	I*4	   I	data type of the input matrix 
 	size	MATRIX	   I	structure defining the size of the input matrix
 	matrix	I*1	   I	input matrix (may be any valid matrix data type)
*******************************************************************************/
#include "asf.h"
#include <ctype.h>

#include "worgen.h"
#include "ltable.h"

void put_matrix(
    struct VECTOR *vec,		/*  VECTOR structuref			  */
    int idtype,		/*  input data type			  */
    struct MATRIX *size,	/*  Matrix structure			  */
    unsigned char *matrix)	/*  Output buffer			  */
{
int in_dtype;
int nentity;
int out_dtype;

switch (idtype)
   {
   case I1: in_dtype = 1;	break;
   case I2: in_dtype = 2;	break;
   case I4: in_dtype = 3;	break;
   case R4: in_dtype = 4;	break;
   case R8: in_dtype = 5;	break;
   default: 
       tab_error((struct TAB_DEF *)NULL,TAB_LT_DTYPE,FATAL,"Invalid data type for a matrix",
                 "put+matrix");
       return;
       break;
   }
switch (vec->dtype)
   {
   case I1: out_dtype = 1;	break;
   case I2: out_dtype = 2;	break;
   case I4: out_dtype = 3;	break;
   case R4: out_dtype = 4;	break;
   case R8: out_dtype = 5;	break;
   default: 
       tab_error((struct TAB_DEF *)NULL,TAB_LT_DTYPE,FATAL,"Invalid data type for a matrix",
                 "put+matrix");
       return;
       break;
   }
nentity = size->nrow * size->ncol;
if (in_dtype == out_dtype)
   c_pxcopy((unsigned char *)matrix,(unsigned char *)vec->ptr.i1,in_dtype,nentity);
else
   c_pxconv(in_dtype,out_dtype,matrix,(unsigned char *)vec->ptr.i1,nentity);
vec->size.nrow = size->nrow;
vec->size.ncol = size->ncol;
return;
}
