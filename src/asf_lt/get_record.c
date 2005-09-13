/*******************************************************************************
FUNCTION:	get_record

PURPOSE:	Read a logical record from Labeled Table file into I/O 
		buffer following the definition of the label vector

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
				Changed TRUE to E_EOF
				Changed FALSE to E_FAIL or E_SUCC

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
Reads a record from the Labeled Table file according to the label vector 
definition until end of the linked list is found.  
Call the correct retrieval routine based upon data type

The function GET_VECTOR must be called prior to GET_RECORD to allocate the 
vector linked list.
*******************************************************************************/
#include "asf.h"


#include <unistd.h>

#include "worgen.h"
#include "ltable.h"

lasErr get_record(struct TAB_DEF *tab)	/* Labeled Table to be processed	      */
{
struct VECTOR *vec;		/* pointer to the current label vector        */
int eor;			/* flag indicating end or record detected     */
char msg[ERRLEN];		/* error message buffer                       */
char *ptr;			/* character string pointer                   */

if (tab->nbytes == 0)		/* if end of file return value of E_EOF       */
   return(E_EOF);

++tab->currec;
for (vec=tab->vector; vec!=NULL; vec=vec->next)
   {
   switch (vec->dtype)
      {
      case I1:
	get_i1_matrix(tab,&vec->lt_size,&vec->size,vec->ptr.i1,&vec->null,&eor);
	break;

      case I2:
	get_i2_matrix(tab,&vec->lt_size,&vec->size,vec->ptr.i2,&vec->null,&eor);
	break;

      case I4:
	get_i4_matrix(tab,&vec->lt_size,&vec->size,vec->ptr.i4,&vec->null,&eor);
	break;

      case R4:
	get_r4_matrix(tab,&vec->lt_size,&vec->size,vec->ptr.r4,&vec->null,&eor);
	break;

      case R8:
	get_r8_matrix(tab,&vec->lt_size,&vec->size,vec->ptr.r8,&vec->null,&eor);
	break;

      case C:

      case LTDATE:

      case LTTIME:
        ptr = get_string_field(tab,&eor);
	strcpy(vec->ptr.c,ptr);
	free(ptr);
	switch (*vec->ptr.c)
	   {
	   case '\0':	vec->null = TRUE;	break;
	   default:	vec->null = FALSE;	break;
	   }
	break;

      default:
	tab_error(tab,TAB_LT_DTYPE,FATAL,"","get_record");
	return(E_FAIL);
  	break;
      }

   if ((eor) && (vec->next != NULL))
      {
      sprintf(msg,"%s %s","Unexpected end of record encountered while",
	      "reading Labeled Table record.");
      tab_error(tab,TAB_LT_EOR,FATAL,msg,"get_record");
      return(E_FAIL);
      }
   }

if (!(eor))
   tab_error(tab,TAB_LT_NOEOR,FATAL,
             "End of record not detected while reading Labeled Table record.",
             "get_record");

return(E_SUCC);
}
/*******************************************************************************
FUNCTION:	get_i1_field

ALGORITHM:
Scans the buffer from current position and returns the byte value to the 
application program.  

Note that the byte value is stored as an ASCII string within the Labeled Table 
I/O buffer.
*******************************************************************************/
void get_i1_field(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    char *val,			/* Byte value read from Labeled Table I/O buff*/

    int *null,			/* Null flag:				      */
				/*    TRUE  - null value returned	      */
				/*    FALSE - integer value returned	      */

    int *eor)			/* End of record flag:*/
				/*    TRUE  - field terminated by record sep. */
				/*    FALSE - field terminated by field sep.  */
{
int cont = TRUE;		/* boolean used to detect end of integer field*/
int len;			/* character length of the integer field      */
int lval;			/* integer value                              */
char sep;			/* type of separator                          */

for (len = 0; cont; )		/* This loop searches the I/O buffer until a  */
   {				/*   field or record separator is reached to  */
   if (tab->cur+len>tab->nbytes)/*   find the length of the field.            */
      {
      tab_error(tab,TAB_LT_FORMAT,FATAL,"","get_record");
      return;
      }

   switch (*(tab->buf.addr + tab->cur + len))
      {
      case FIELD_SEP1:	cont = FALSE;	break;
      case FIELD_SEP2:  cont = FALSE;	break;
      case RECORD_SEP:	cont = FALSE;	break;
      default:		len++;		break;
      }
   }

sep = *(tab->buf.addr + tab->cur + len);
if (len > 0)
   {
   *null = FALSE;
   *(tab->buf.addr + tab->cur + len) = '\0';
   lval = atol(tab->buf.addr + tab->cur);
   *val = (char) lval;
   *(tab->buf.addr + tab->cur + len) = sep;
   }
else
   {
   *null = TRUE;
   *val = 0;
   }

tab->cur += len;
switch (sep)			/* Update the I/O pointers    */
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
                "Integer field not terminated properly.","get_record");
      return;
      break;
   }

if (tab->cur >= tab->nbytes)		/* If required issue a physical read */
   read_tab(tab);

return;
}

/*******************************************************************************
FUNCTION:	get_i1_matrix

ALGORITHM:	
Reads a byte (8-bit) matrix from the Labeled Table and returns it to the 
application program as a row major array.

Note that the matrix is stored as an ASCII string within the Labeled Table I/O 
buffer.
*******************************************************************************/
void get_i1_matrix(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    char   matrix[],		/* byte matrix read from LT I/O buffer	      */

    int   *null,		/* Null flag:				      */
				/*  TRUE  - null values detected in matrix    */
				/*  FALSE - no null values detected in matrix */

    int   *eor)		/* End of record flag:			      */
				/*  TRUE  - matrix terminated by record sep   */
				/*  FALSE - matrix terminated by field sep    */
{
int col_index;
int eor_flag;
int index;
int null_flag;
int row_index;

out->ncol = out->nrow = -1;
*null = FALSE;
for (index = 0, row_index = 1; row_index <= lt_size->nrow; row_index++)
   {
   for (col_index = 1; col_index <= lt_size->ncol; col_index++)
      {
      get_i1_field(tab,matrix+index,&null_flag,&eor_flag);
      if ((eor_flag) && (row_index<lt_size->nrow) && (col_index<lt_size->ncol))
	 {
	 tab_error(tab,TAB_LT_FORMAT,FATAL,
                 "End of record encountered prior to reading full matrix.",
                  "get_record");
	 return;
	 }

      if (null_flag)				/* if null value read, set the*/
         {					/*   output matrix size. The  */
	 *null = TRUE;				/*   index is NOT incremented.*/
         if ((out->ncol < 0) && (row_index == 1))
	    out->ncol = col_index - 1;

         if ((out->nrow < 0) && (col_index == 1))
            out->nrow = row_index - 1;
         }
      else					/* otherwise the index is     */
	 index++;				/*   incremented.             */
      }
   }

if (out->ncol < 0)				/* if the output number of    */
   out->ncol = lt_size->ncol;			/*   rows and columns have not*/

if (out->nrow < 0)				/*   been set, set them equal */
   out->nrow = lt_size->nrow;			/*   to Labeled Table size    */

*eor = eor_flag;
return;
}

/*******************************************************************************
FUNCTION:	get_i2_field

ALGORITHM:	
Scans the buffer from current position and returns the 16-bit integer value to 
the application program.  

Note that the value is stored as an ASCII string within the Labeled Table I/O
buffer.
*******************************************************************************/
void get_i2_field(
    struct TAB_DEF *tab,	/* Labeled table to be processed	      */
    short *val,			/* 16-bit value read from LT I/O buffer       */
    int  *null,		/* Null flag:				      */
				/*   TRUE  - null value returned	      */
				/*   FALSE - integer value returned	      */

    int  *eor)			/* End of record flag:			      */
				/*   TRUE  - field terminated by record sep   */
				/*   FALSE - field terminated by field sep    */
{
int cont = TRUE;		/* boolean used to detect end of integer field*/
int len;			/* character length of the integer field      */
int lval;			/* integer value                              */
char sep;			/* type of separator                          */

for (len = 0; cont; )		/* This loop searches the I/O buffer until a  */
   {				/*   field or record separator is reached to  */
   if (tab->cur+len>tab->nbytes)/*   find the length of the field.            */
      {
      tab_error(tab,TAB_LT_FORMAT,FATAL,"","get_record");
      return;
      }

   switch (*(tab->buf.addr + tab->cur + len))
      {
      case FIELD_SEP1:	cont = FALSE;	break;
      case FIELD_SEP2:  cont = FALSE;	break;
      case RECORD_SEP:	cont = FALSE;	break;
      default:		len++;		break;
      }
   }

sep = *(tab->buf.addr + tab->cur + len);
if (len > 0)
   {
   *null = FALSE;
   *(tab->buf.addr + tab->cur + len) = '\0';
   lval = atol(tab->buf.addr + tab->cur);
   *val = (short) lval;
   *(tab->buf.addr + tab->cur + len) = sep;
   }
else
   {
   *null = TRUE;
   *val = 0;
   }

tab->cur += len;
switch (sep)			/* Update the I/O pointers    */
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
                "Integer field not terminated properly.","get_record");
      return;
      break;
   }

if (tab->cur >= tab->nbytes)		/* If required issue a physical read */
   read_tab(tab);

return;
}

/*******************************************************************************
FUNCTION:	get_i2_matrix

ALGORITHM:	
Reads an integer (16-bit) matrix from the Labeled Table and returns it to the 
application program as a row major array.

Note that the matrix is stored as an ASCII string within the Labeled Table 
I/O buffer.
*******************************************************************************/
void get_i2_matrix(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    short  matrix[],		/* 16-bit integer matrix read from LT I/O buff*/

    int   *null,		/* Null flag:				      */
				/*   TRUE  - null values detected in matrix   */
				/*   FALSE - no null values detected in matrix*/

    int   *eor)		/* End of record flag:			      */
				/*   TRUE  - matrix terminated by record sep. */
				/*   FALSE - matrix terminated by field sep.  */
{
int col_index;
int eor_flag;
int index;
int null_flag;
int row_index;

out->ncol = out->nrow = -1;
*null = FALSE;
for (index = 0, row_index = 1; row_index <= lt_size->nrow; row_index++)
   {
   for (col_index = 1; col_index <= lt_size->ncol; col_index++)
      {
      get_i2_field(tab,matrix+index,&null_flag,&eor_flag);
      if ((eor_flag) && (row_index<lt_size->nrow) && (col_index<lt_size->ncol))
         {
	 tab_error(tab,TAB_LT_FORMAT,FATAL,
                 "End of record encountered prior to reading full matrix.",
                  "get_record");
          return;
          }

      if (null_flag)				/* if null value read, set the*/
         {					/*   output matrix size. The  */
	 *null = TRUE;				/*   index is NOT incremented.*/
         if ((out->ncol < 0) && (row_index == 1))
	    out->ncol = col_index - 1;

         if ((out->nrow < 0) && (col_index == 1))
            out->nrow = row_index - 1;
         }
      else					/* otherwise the index is     */
	 index++;				/*   incremented.             */
      }
   }

if (out->ncol < 0)				/* if the output number of    */
   out->ncol = lt_size->ncol;			/*   rows and columns have not*/

if (out->nrow < 0)				/*   been set, set them equal */
   out->nrow = lt_size->nrow;			/*   to Labeled Table size    */

*eor = eor_flag;
return;
}

/*******************************************************************************
FUNCTION:	get_i4_field

ALGORITHM:	
Scans the buffer from current position and returns the integer value to the 
application program.  

Note that the integer value is stored as an ASCII string within the Labeled 
Table I/O buffer.
*******************************************************************************/
void get_i4_field(
    struct TAB_DEF *tab,	/* Labeled Table to be processed.	      */
    int *val,			/* integer value read from LT I/O buffer      */

    int  *null,		/* Null flag:				      */
				/*   TRUE  - null value returned	      */
				/*   FALSE - integer value returned	      */

    int  *eor)			/* End of record flag:			      */
				/*   TRUE  - field terminated by record sep   */
				/*   FALSE - field terminated by field sep    */
{
int cont = TRUE;		/* boolean used to detect end of integer field*/
int len;			/* character length of the integer field      */
char sep;			/* type of separator                          */

for (len = 0; cont; )		/* This loop searches the I/O buffer until a  */
   {				/*   field or record separator is reached to  */
   if (tab->cur+len>tab->nbytes)/*   find the length of the field.            */
      {
      tab_error(tab,TAB_LT_FORMAT,FATAL,"","get_record");
      return;
      }

   switch (*(tab->buf.addr + tab->cur + len))
      {
      case FIELD_SEP1:	cont = FALSE;	break;
      case FIELD_SEP2:  cont = FALSE;	break;
      case RECORD_SEP:	cont = FALSE;	break;
      default:		len++;		break;
      }
   }

sep = *(tab->buf.addr + tab->cur + len);
if (len > 0)
   {
   *null = FALSE;
   *(tab->buf.addr + tab->cur + len) = '\0';
   *val = atol(tab->buf.addr + tab->cur);
   *(tab->buf.addr + tab->cur + len) = sep;
   }
else
   {
   *null = TRUE;
   *val = 0;
   }

tab->cur += len;
switch (sep)			/* Update the I/O pointers    */
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
                "Integer field not terminated properly.","get_record");
      return;
      break;
   }

if (tab->cur >= tab->nbytes)		/* If required issue a physical read */
   read_tab(tab);

return;
}

/*******************************************************************************
FUNCTION:	get_i4_matrix

ALGORITHM:	
Reads an integer (32-bit) matrix from the Labeled Table and returns it to the 
application program as a row major array.

Note that the matrix is stored as an ASCII string within the Labeled Table 
I/O buffer.
*******************************************************************************/
void get_i4_matrix(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* Number of rows & columns of LT matrix      */
    struct MATRIX *out,	/* Actual num of rows & columns of matrix read*/
    int   matrix[],		/* integer matrix read from LT I/O buffer     */

    int   *null,		/* Null flag:				      */
				/*   TRUE  - null values detected in matrix   */
				/*   FALSE - no null values detected in matrix*/

    int   *eor)		/* End of record flag:			      */
				/*   TRUE  - matrix terminated by record sep. */
				/*   FALSE - matrix terminated by field sep.  */
{
int col_index;
int eor_flag;
int index;
int null_flag;
int row_index;

out->ncol = out->nrow = -1;
*null = FALSE;
for (index = 0, row_index = 1; row_index <= lt_size->nrow; row_index++)
   {
   for (col_index = 1; col_index <= lt_size->ncol; col_index++)
      {
      get_i4_field(tab,matrix+index,&null_flag,&eor_flag);
      if ((eor_flag) && (row_index<lt_size->nrow) && (col_index<lt_size->ncol))
         {
	 tab_error(tab,TAB_LT_FORMAT,FATAL,
                 "End of record encountered prior to reading full matrix.",
                  "get_record");
         return;
         }

      if (null_flag)				/* if null value read, set the*/
         {					/*   output matrix size. The  */
	 *null = TRUE;				/*   index is NOT incremented.*/
         if ((out->ncol < 0) && (row_index == 1))
	    out->ncol = col_index - 1;

         if ((out->nrow < 0) && (col_index == 1))
            out->nrow = row_index - 1;
         }
      else					/* otherwise the index is     */
	 index++;				/*   incremented.             */
      }
   }

if (out->ncol < 0)				/* if the output number of    */
   out->ncol = lt_size->ncol;			/*   rows and columns have not*/

if (out->nrow < 0)				/*   been set, set them equal */
   out->nrow = lt_size->nrow;			/*   to Labeled Table size    */

*eor = eor_flag;
return;
}

/*******************************************************************************
FUNCTION:	get_r4_field

ALGORITHM:	
Scans the buffer from current position and returns the real value to the 
application program.  

Note that the real value is stored as an ASCII string within the Labeled Table 
I/O buffer.
*******************************************************************************/
void get_r4_field(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    float  *val,		/* Real value read from LT I/O buffer	      */

    int   *null,		/* Null flag:				      */
				/*   TRUE  - null value returned	      */
				/*   FALSE - real value returned	      */

    int   *eor)		/* End of record flag:			      */
				/*   TRUE  - field terminated by record sep.  */
				/*   FALSE - field terminated by field sep.   */
{
int cont = TRUE;		/* boolean used to detect end of integer field*/
int len;			/* character length of the integer field      */
char sep;			/* type of separator                          */

for (len = 0; cont; )		/* This loop searches the I/O buffer until a  */
   {				/*   field or record separator is reached to  */
   if (tab->cur+len>tab->nbytes)/*   find the length of the field.            */
      {
      tab_error(tab,TAB_LT_FORMAT,FATAL,"","get_record");
      return;
      }

   switch (*(tab->buf.addr + tab->cur + len))
      {
      case FIELD_SEP1:	cont = FALSE;	break;
      case FIELD_SEP2:  cont = FALSE;	break;
      case RECORD_SEP:	cont = FALSE;	break;
      default:		len++;		break;
      }
   }

sep = *(tab->buf.addr + tab->cur + len);
if (len > 0)
   {
   *null = FALSE;
   *(tab->buf.addr + tab->cur + len) = '\0';
   *val = (float) atof(tab->buf.addr + tab->cur);
   *(tab->buf.addr + tab->cur + len) = sep;
   }
else
   {
   *null = TRUE;
   *val = 0;
   }

tab->cur += len;
switch (sep)			/* Update the I/O pointers    */
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
	 	"Integer field not terminated properly.","get_record");
      return;
      break;
   }

if (tab->cur >= tab->nbytes)		/* If required issue a physical read */
   read_tab(tab);

return;
}

/*******************************************************************************
FUNCTION:	get_r4_matrix

ALGORITHM:	
Reads a real (32-bit) matrix from the Labeled Table and returns it to the 
application program as a row major array.

Note that the matrix is stored as an ASCII string within the Labeled Table 
I/O buffer.
*******************************************************************************/
void get_r4_matrix(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed.        */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    float  matrix[],		/* Real matrix read from LT I/O buffer        */

    int   *null,		/* Null flag:				      */
				/*   TRUE  - null values detected in matrix   */
				/*   FALSE - no null values detected in matrix*/

    int   *eor)		/* End of record flag:			      */
				/*   TRUE  - matrix terminated by record sep. */
				/*   FALSE - matrix terminated by field sep.  */
{
int col_index;
int eor_flag;
int index;
int null_flag;
int row_index;

out->ncol = out->nrow = -1;
*null = FALSE;
for (index = 0, row_index = 1; row_index <= lt_size->nrow; row_index++)
   {
   for (col_index = 1; col_index <= lt_size->ncol; col_index++)
      {
      get_r4_field(tab,matrix+index,&null_flag,&eor_flag);
      if ((eor_flag) && (row_index<lt_size->nrow) && (col_index<lt_size->ncol))
         {
	 tab_error(tab,TAB_LT_FORMAT,FATAL,
                 "End of record encountered prior to reading full matrix.",
                  "get_record");
         return;
         }

      if (null_flag)				/* if null value read, set the*/
         {					/*   output matrix size.  The */
	 *null = TRUE;				/*   index is NOT incremented.*/
         if ((out->ncol < 0) && (row_index == 1))
	    out->ncol = col_index - 1;

         if ((out->nrow < 0) && (col_index == 1))
            out->nrow = row_index - 1;
         }
      else					/* otherwise the index is     */
	 index++;				/*   incremented.             */
      }
   }

if (out->ncol < 0)				/* set the output number of   */
   out->ncol = lt_size->ncol;			/*   rows and columns have not*/

if (out->nrow < 0)				/*   been set, set them equal */
   out->nrow = lt_size->nrow;			/*   to Labeled Table size    */

*eor = eor_flag;
return;
}

/*******************************************************************************
FUNCTION:	GET_R8_FIELD 

ALGORITHM:	
Scans the buffer from current position and returns the double value 
to the application program.  

Note that the double value is stored as an ASCII string within the 
Labeled Table I/O buffer.
*******************************************************************************/
void get_r8_field(
    struct TAB_DEF *tab,	/* Labeled Table to be processed.	      */
    double *val,		/* integer value read from LT I/O buffer      */

    int  *null,	/* Null flag:				      */
				/*   TRUE  - null value returned	      */
				/*   FALSE - integer value returned	      */

    int  *eor)			/* End of record flag:			      */
				/*   TRUE  - field terminated by record sep   */
				/*   FALSE - field terminated by field sep    */
{
int cont = TRUE;		/* boolean used to detect end of integer field*/
int len;			/* character length of the integer field      */
char sep;			/* type of separator                          */

for (len = 0; cont; )		/* This loop searches the I/O buffer until a  */
   {				/*   field or record separator is reached to  */
   if (tab->cur+len>tab->nbytes)/*   find the length of the field.            */
      {
      tab_error(tab,TAB_LT_FORMAT,FATAL,"","get_record");
      return;
      }

   switch (*(tab->buf.addr + tab->cur + len))
      {
      case FIELD_SEP1:	cont = FALSE;	break;
      case FIELD_SEP2:	cont = FALSE;	break;
      case RECORD_SEP:	cont = FALSE;	break;
      default:		len++;		break;
      }
   }

sep = *(tab->buf.addr + tab->cur + len);
if (len > 0)
   {
   *null = FALSE;
   *(tab->buf.addr + tab->cur + len) = '\0';
   *val = atof(tab->buf.addr + tab->cur);
   *(tab->buf.addr + tab->cur + len) = sep;
   }
else
   {
   *null = TRUE;
   *val = 0;
   }

tab->cur += len;
switch (sep)			/* Update the I/O pointers    */
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
	 	"Integer field not terminated properly.","get_record");
      return;
      break;
   }

if (tab->cur >= tab->nbytes)		/* If required issue a physical read */
   read_tab(tab);

return;
}

/*******************************************************************************
FUNCTION:	get_r8_matrix

ALGORITHM:	
Reads a real double (64-bit) matrix from the Labeled Table and returns it to 
the application program as a row major array.  

Note that the matrix is stored as an ASCII string within the Labeled Table 
I/O buffer.
*******************************************************************************/
void get_r8_matrix(
    struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    double matrix[],		/* Double matrix read from LT I/O buffer      */

    int   *null,		/* Null flag:				      */
				/*   TRUE  - null values detected in matrix   */
				/*   FALSE - no null values detected in matrix*/

    int   *eor)		/* End of record flag:			      */
				/*   TRUE  - matrix terminated by record sep. */
				/*   FALSE - matrix terminated by field sep.  */
{
int col_index;
int eor_flag;
int index;
int null_flag;
int row_index;

out->ncol = out->nrow = -1;
*null = FALSE;
for (index = 0, row_index = 1; row_index <= lt_size->nrow; row_index++)
   {
   for (col_index = 1; col_index <= lt_size->ncol; col_index++)
      {
      get_r8_field(tab,matrix+index,&null_flag,&eor_flag);
      if ((eor_flag) && (row_index<lt_size->nrow) && (col_index<lt_size->ncol))
         {
	 tab_error(tab,TAB_LT_FORMAT,FATAL,
                 "End of record encountered prior to reading full matrix.",
                  "get_record");
         return;
         }

      if (null_flag)				/* if null value read, set the*/
         {					/*   output matrix size.  The */
	 *null = TRUE;				/*   index is NOT incremented.*/
         if ((out->ncol < 0) && (row_index == 1))
	    out->ncol = col_index - 1;

         if ((out->nrow < 0) && (col_index == 1))
            out->nrow = row_index - 1;
         }
      else					/* otherwise the index is     */
	 index++;				/*   incremented.             */
      }
   }

if (out->ncol < 0)				/* set the output number of   */
   out->ncol = lt_size->ncol;			/*   rows and columns have not*/

if (out->nrow < 0)				/*   been set, set them equal */
   out->nrow = lt_size->nrow;			/*   to Labeled Table size    */

*eor = eor_flag;
return;
}
