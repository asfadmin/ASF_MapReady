/*******************************************************************************
FUNCTIONS: 	put_record		
PURPOSE: 	Write a logical record to Labeled Table I/O buffer following 
		the definition of the label vector

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
   10      08/91  T. Mittan     Subtracted one from TAB_SIZ so that "\0"
				can be appended to the end of the string 

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	none.

PROJECT:	LAS

ALGORITHM:
PUT_RECORD writes a record to the Labeled Table file according to the label 
vector definition.  

The function PUT_VECTOR must be called prior to PUT_RECORD to allocate the 
vector linked list.
*******************************************************************************/
#include "asf.h"

#include <ctype.h>

#include "worgen.h"
#include "ltable.h"


/*******************************************************************************
FUNCTION:	PUT_I4_FIELD 

ALGORITHM:
Converts an integer to ASCII 
Writes the ASCII integer into the Labeled Table I/O buffer.  
If the buffer is full 
   A physical write is issued.
*******************************************************************************/
void put_i4_field(
    struct TAB_DEF *tab,	/*  structure defining the LT file to be      */
				/*  processed.*/
    int val,			/*  integer value written to the I/O buffer   */
    char sep)			/*  field or record separator to be written   */
				/*  to the I/O buffer after the integer.      */
{
int len;
static char temp[MAX_FIELD_LEN];

if (sep == RECORD_SEP)			/* store value into temporary buffer  */
   sprintf(temp,"%d%c\n",val,sep);
else
   sprintf(temp,"%d%c",val,sep);
len = strlen(temp);
if (tab->cur + len > (TAB_SIZ - 1))	/* if buffer is full, then issue a    */
                                        /* -1 is so "\0" can be appended      */
   write_tab(tab);			/*   physical write command.          */
strcpy(tab->buf.addr+tab->cur,temp);	/* add string to the I/O buffer       */
tab->cur += len;			/* update the current I/O pointer     */
return;
}

/*******************************************************************************
FUNCTION:	put_i1_matrix

ALGORITHM:	Converts a 8-bit integer matrix to an ASCII matrix and 
		writes the ASCII matrix into the Labeled Table I/O buffer.
		If the buffer is full, a physical write is issued.  It
		is assumed the matrix is stored in row major.

		Note that the label vector denotes the size of the Labeled
		Table matrix.  If the matrix to be written has a larger
		number of rows or columns, a fatal error message is outputted.
		If the matrix to be written has a smaller number of row
		or columns, null values are written into the lower right
		region of the matrix.
*******************************************************************************/
void put_i1_matrix(
    struct TAB_DEF *tab,	/* structure of the LT file being processed   */
    struct MATRIX *in,		/* Number of rows & columns of the matrix     */
    struct MATRIX *lt_size,	/* actual 				      */
    char   matrix[],		/* 8-bit integer matrix to be written	      */
    char   sep)			/* field or reord separator to be written to the
				   I/O buffer after the real matrix	      */
{
int col_index;
int row_index;
int index;
char msg[ERRLEN];
/* If matrix to be written is larger than the Labeled Table matrix, output
*  a fatal error message.
*/
if ((in->nrow > lt_size->nrow) || (in->ncol > lt_size->ncol))
   {
   sprintf(msg,"%s  %s %d x %d.  %s %d x %d.",
           "Matrix to be written to the Labeled Table is too large.",
           "Input matrix is",in->nrow,in->ncol,
           "Labeled Table matrix is",lt_size->nrow,lt_size->ncol);
   tab_error(tab,TAB_LT_MATRIX_SIZE,FATAL,msg,"put_record");
   return;
   }

/* If matrix to be written is null, then write the null matrix to the LT
*/
else if ((in->nrow == 0) && (in->ncol == 0))
   put_null_matrix(tab,lt_size,sep);

/* If matrix to be written is the same size as the Labeled Table matrix, write
*  the matrix to the Labeled Table so that the last field of each row is
*  terminated by FIELD_SEP2 (new line character), and the last field of the
*  last row is terminated by input parameter "sep" (field separator or record
*  separator).
*/
else if ((in->nrow == lt_size->nrow) && (in->ncol == lt_size->ncol))
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_i4_field(tab,matrix[index],FIELD_SEP1);
      put_i4_field(tab,matrix[index],FIELD_SEP2);
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_i4_field(tab,matrix[index],FIELD_SEP1);
   put_i4_field(tab,matrix[index],sep);
   }

/* If matrix to be written is smaller than the Labeled Table matrix, write
*  the matrix as a upper left matrix.  The lower right portion of the matrix
*  is null filled.  The last field of each row is terminated by FIELD_SEP2 
*  (new line character), and the last field of the last row is terminated by 
*  input parameter "sep" (field separator or record separator).
----------------------------------------------------------------------------*/
else
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_i4_field(tab,matrix[index],FIELD_SEP1);
      if (col_index == lt_size->ncol)		/* column is correct size    */
         put_i4_field(tab,matrix[index],FIELD_SEP2);
      else					/* null fill extra columns   */
         {
         put_i4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_i4_field(tab,matrix[index],FIELD_SEP1);
   if ((row_index == lt_size->nrow) && (col_index == lt_size->ncol))
      put_i4_field(tab,matrix[index],sep);
   else if ((row_index == lt_size->nrow) && (col_index < lt_size->ncol))
      {
      put_i4_field(tab,matrix[index],FIELD_SEP1);
      for (++col_index; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   else
      {
      if (col_index == lt_size->ncol)			/* finish current row */
         put_i4_field(tab,matrix[index],FIELD_SEP2);
      else
         {
         put_i4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (++row_index; row_index < lt_size->nrow; row_index++) 
	 {				/* write null values for extra rows   */
         for (col_index = 1; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (col_index = 1; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   }
return;
}

/*******************************************************************************
FUNCTION:	PUT_I2_MATRIX 

ALGORITHM:
Converts a 16-bit integer matrix to an ASCII matrix 
Writes the ASCII matrix into the Labeled Table I/O buffer.  
If the buffer is full 
   A physical write is issued.  
Note that the label vector denotes the size of the Labeled Table matrix.
If the matrix to be written has a larger number of rows or columns, 
   A fatal error message is outputted.  
If the matrix to be written has a smaller number of row or columns, 
   Null values are written into the lower right region of the matrix.

It is assumed the matrix is stored in row major.
 
*******************************************************************************/
void put_i2_matrix(
    struct TAB_DEF *tab,	/*  structure defining the LT file to be 
				    processed*/
    struct MATRIX *in,		/*  number of rows & columns of input matrix  */
    struct MATRIX *lt_size, 	/*  number of rows & columns of LT matrix     */
    short  matrix[],		/*8-bit integer matrix to be written 	      */
    char   sep)			/* field or record separator to be written to */
				/* the I/O buffer after the real matrix	      */
{
int col_index;
int row_index;
int index;
char msg[ERRLEN];

/* If matrix to be written is larger than the Labeled Table matrix, output
*  a fatal error message.
*/
if ((in->nrow > lt_size->nrow) || (in->ncol > lt_size->ncol))
   {
   sprintf(msg,"%s  %s %d x %d.  %s %d x %d.",
           "Matrix to be written to the Labeled Table is too large.",
           "Input matrix is",in->nrow,in->ncol,
           "Labeled Table matrix is",lt_size->nrow,lt_size->ncol);
   tab_error(tab,TAB_LT_MATRIX_SIZE,FATAL,msg,"put_record");
   return;
   }

/* If matrix to be written is null, then write the null matrix to the LT
------------------------------------------------------------------------ */
else if ((in->nrow == 0) && (in->ncol == 0))
   put_null_matrix(tab,lt_size,sep);

/* If matrix to be written is the same size as the Labeled Table matrix, write
*  the matrix to the Labeled Table so that the last field of each row is
*  terminated by FIELD_SEP2 (new line character), and the last field of the
*  last row is terminated by input parameter "sep" (field separator or record
*  separator).  */
else if ((in->nrow == lt_size->nrow) && (in->ncol == lt_size->ncol))
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_i4_field(tab,matrix[index],FIELD_SEP1);
      put_i4_field(tab,matrix[index],FIELD_SEP2);
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_i4_field(tab,matrix[index],FIELD_SEP1);
   put_i4_field(tab,matrix[index],sep);
   }

/* If matrix to be written is smaller than the Labeled Table matrix, write
*  the matrix as a upper left matrix.  The lower right portion of the matrix
*  is null filled.  The last field of each row is terminated by FIELD_SEP2 
*  (new line character), and the last field of the last row is terminated by 
*  input parameter "sep" (field separator or record separator).  */
else
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_i4_field(tab,matrix[index],FIELD_SEP1);
      if (col_index == lt_size->ncol)		/* column is correct size    */
         put_i4_field(tab,matrix[index],FIELD_SEP2);
      else					/* null fill extra columns   */
         {
         put_i4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_i4_field(tab,matrix[index],FIELD_SEP1);
   if ((row_index == lt_size->nrow) && (col_index == lt_size->ncol))
      put_i4_field(tab,matrix[index],sep);
   else if ((row_index == lt_size->nrow) && (col_index < lt_size->ncol))
      {
      put_i4_field(tab,matrix[index],FIELD_SEP1);
      for (++col_index; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   else
      {
      if (col_index == lt_size->ncol)			/* finish current row */
         put_i4_field(tab,matrix[index],FIELD_SEP2);
      else
         {
         put_i4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (++row_index; row_index < lt_size->nrow; row_index++) 
	 {				/* write null values for extra rows   */
         for (col_index = 1; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (col_index = 1; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   }
return;
}


/*******************************************************************************
FUNCTION:	PUT_I4_MATRIX 

ALGORITHM:
Converts a 32-bit integer matrix to an ASCII matrix 
Writes the ASCII matrix into the Labeled Table i/O buffer
If the buffer is full 
   a physical write is issued
Note that the label vector denotes the size of the Labeled Table matrix.
If the matrix to be written has a larger number of rows or columns
   A fatal error message is outputted
If the matrix to be written has a smaller number of row or columns 
   null values are written into the lower right region of the matrix.
 
It is assumed the matrix is stored in row major.
*******************************************************************************/
void put_i4_matrix(
    struct TAB_DEF *tab,
    struct MATRIX *in,
    struct MATRIX *lt_size,	/*  number of rows & columns of LT matrix    */
    int   matrix[],		/*  32-bit integer matrix to be written      */
    char   sep)		/*  field or record separator to be written  */
				/*  to the I/O buffer after the real matrix  */
{
int col_index;
int row_index;
int index;
char msg[ERRLEN];

/* If matrix to be written is larger than the Labeled Table matrix, output
*  a fatal error message.  */
if ((in->nrow > lt_size->nrow) || (in->ncol > lt_size->ncol))
   {
   sprintf(msg,"%s  %s %d x %d.  %s %d x %d.",
           "Matrix to be written to the Labeled Table is too large.",
           "Input matrix is",in->nrow,in->ncol,
           "Labeled Table matrix is",lt_size->nrow,lt_size->ncol);
   tab_error(tab,TAB_LT_MATRIX_SIZE,FATAL,msg,"put_record");
   return;
   }

/* If matrix to be written is null, then write the null matrix to the LT
------------------------------------------------------------------------*/
else if ((in->nrow == 0) && (in->ncol == 0))
   put_null_matrix(tab,lt_size,sep);

/* If matrix to be written is the same size as the Labeled Table matrix, write
*  the matrix to the Labeled Table so that the last field of each row is
*  terminated by FIELD_SEP2 (new line character), and the last field of the
*  last row is terminated by input parameter "sep" (field separator or record
*  separator).  */
else if ((in->nrow == lt_size->nrow) && (in->ncol == lt_size->ncol))
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_i4_field(tab,matrix[index],FIELD_SEP1);
      put_i4_field(tab,matrix[index],FIELD_SEP2);
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_i4_field(tab,matrix[index],FIELD_SEP1);
   put_i4_field(tab,matrix[index],sep);
   }

/* If matrix to be written is smaller than the Labeled Table matrix, write
*  the matrix as a upper left matrix.  The lower right portion of the matrix
*  is null filled.  The last field of each row is terminated by FIELD_SEP2 
*  (new line character), and the last field of the last row is terminated by 
*  input parameter "sep" (field separator or record separator).  */
else
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_i4_field(tab,matrix[index],FIELD_SEP1);
      if (col_index == lt_size->ncol)		/* column is correct size    */
         put_i4_field(tab,matrix[index],FIELD_SEP2);
      else					/* null fill extra columns   */
         {
         put_i4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_i4_field(tab,matrix[index],FIELD_SEP1);
   if ((row_index == lt_size->nrow) && (col_index == lt_size->ncol))
      put_i4_field(tab,matrix[index],sep);
   else if ((row_index == lt_size->nrow) && (col_index < lt_size->ncol))
      {
      put_i4_field(tab,matrix[index],FIELD_SEP1);
      for (++col_index; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   else
      {
      if (col_index == lt_size->ncol)			/* finish current row */
         put_i4_field(tab,matrix[index],FIELD_SEP2);
      else
         {
         put_i4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (++row_index; row_index < lt_size->nrow; row_index++) 
	 {				/* write null values for extra rows   */
         for (col_index = 1; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (col_index = 1; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   }
return;
}
/*******************************************************************************
FUNCTION:	PUT_R4_FIELD 

ALGORITHM:
Converts a real to ASCII 
Writes the ASCII real into the Labeled Table I/O buffer  
If the buffer is full 
   A physical write is issued.
*******************************************************************************/
void put_r4_field(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed. */
    float val,			/*real value to be written into the I/O buffer*/
    char sep)			/*field or record separator to be written to  */
				/*the I/O buffer after the real value.        */
{
static int PREC = 10;
static int TOT = 11;
static char temp[MAX_FIELD_LEN];
int len;

if (sep == RECORD_SEP)			/* store value into temporary buffer  */
   sprintf(temp,"%*.*E%c\n",TOT,PREC,val,sep); 
else
   sprintf(temp,"%*.*E%c",TOT,PREC,val,sep); 
len = strlen(temp);
if (tab->cur + len > (TAB_SIZ - 1))	/* if buffer is full, then issue a    */
                                        /* -1 is so "\0" can be appended      */
   write_tab(tab);			/*   physical write command.          */
strcpy(tab->buf.addr+tab->cur,temp);	/* add string to the I/O buffer       */
tab->cur += len;			/* update the current I/O pointer     */
return;
}

/*******************************************************************************
FUNCTION:	PUT_R4_MATRIX 
Converts a real matrix to an ASCII matrix 
Writes the ASCII real matrix into the Labeled Table I/O buffer
If the buffer is full 
   A physical write is issued
Note that the label vector denotes the size of the Labeled Table matrix.
If the matrix to be written has a larger number of rows or columns
   A fatal error message is outputted
If the matrix to be written has a smaller number of row or columns
   Null values are written into the lower right region of the matrix.

It is assumed the matrix is stored in row major.
*******************************************************************************/
void put_r4_matrix(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed  */
    struct MATRIX *in,		/*number of rows & columns of input matrix    */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    float  matrix[],		/*real matrix to be written to the I/O buffer */
    char   sep)			/*field or record separator to be written to  */
				/*the I/O buffer after the real matrix	      */
{
int col_index;
int row_index;
int index;
char msg[ERRLEN];

/* If matrix to be written is larger than the Labeled Table matrix, output
*  a fatal error message.  */
if ((in->nrow > lt_size->nrow) || (in->ncol > lt_size->ncol))
   {
   sprintf(msg,"%s  %s %d x %d.  %s %d x %d.",
           "Matrix to be written to the Labeled Table is too large.",
           "Input matrix is",in->nrow,in->ncol,
           "Labeled Table matrix is",lt_size->nrow,lt_size->ncol);
   tab_error(tab,TAB_LT_MATRIX_SIZE,FATAL,msg,"put_record");
   return;
   }

/* If matrix to be written is null, then write the null matrix to the LT */
else if ((in->nrow == 0) && (in->ncol == 0))
   put_null_matrix(tab,lt_size,sep);

/* If matrix to be written is the same size as the Labeled Table matrix, write
*  the matrix to the Labeled Table so that the last field of each row is
*  terminated by FIELD_SEP2 (new line character), and the last field of the
*  last row is terminated by input parameter "sep" (field separator or record
*  separator).  */
else if ((in->nrow == lt_size->nrow) && (in->ncol == lt_size->ncol))
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_r4_field(tab,matrix[index],FIELD_SEP1);
      put_r4_field(tab,matrix[index],FIELD_SEP2);
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_r4_field(tab,matrix[index],FIELD_SEP1);
   put_r4_field(tab,matrix[index],sep);
   }

/* If matrix to be written is smaller than the Labeled Table matrix, write
*  the matrix as a upper left matrix.  The lower right portion of the matrix
*  is null filled.  The last field of each row is terminated by FIELD_SEP2 
*  (new line character), and the last field of the last row is terminated by 
*  input parameter "sep" (field separator or record separator).  */
else
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_r4_field(tab,matrix[index],FIELD_SEP1);
      if (col_index == lt_size->ncol)		/* column is correct size    */
         put_r4_field(tab,matrix[index],FIELD_SEP2);
      else					/* null fill extra columns   */
         {
         put_r4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_r4_field(tab,matrix[index],FIELD_SEP1);
   if ((row_index == lt_size->nrow) && (col_index == lt_size->ncol))
      put_r4_field(tab,matrix[index],sep);
   else if ((row_index == lt_size->nrow) && (col_index < lt_size->ncol))
      {
      put_r4_field(tab,matrix[index],FIELD_SEP1);
      for (++col_index; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   else
      {
      if (col_index == lt_size->ncol)			/* finish current row */
         put_r4_field(tab,matrix[index],FIELD_SEP2);
      else
         {
         put_r4_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (++row_index; row_index < lt_size->nrow; row_index++) 
	 {				/* write null values for extra rows   */
         for (col_index = 1; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (col_index = 1; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   }
return;
}

/*******************************************************************************
FUNCTION:	PUT_R8_FIELD 

ALGORITHM:
Converts a double to ASCII 
Writes the ASCII double into the Labeled Table I/O buffer
If the buffer is full
   A physical write is issued.
*******************************************************************************/
void put_r8_field(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed  */
    double val,			/*double value to be written to the I/O buffer*/
    char   sep)			/*field or record separator to be written to  */
				/*the I/O buffer after the real value.        */
{
static int PREC = 20;
static int TOT = 21;
static char temp[MAX_FIELD_LEN];
int len;

if (sep == RECORD_SEP)			/* store value into temporary buffer  */
   sprintf(temp,"%*.*E%c\n",TOT,PREC,val,sep); 
else
   sprintf(temp,"%*.*E%c",TOT,PREC,val,sep); 
len = strlen(temp);
if (tab->cur + len > (TAB_SIZ - 1))	/* if buffer is full, then issue a    */
                                        /* -1 is so "\0" can be appended      */
   write_tab(tab);			/*   physical write command.          */
strcpy(tab->buf.addr+tab->cur,temp);	/* add string to the I/O buffer       */
tab->cur += len;			/* update the current I/O pointer     */
return;
}

/*******************************************************************************
FUNCTION:	PUT_R8_MATRIX 

ALGORITHM:
Converts a double matrix to an ASCII matrix 
Writes the ASCII double matrix into the Labeled Table I/O buffer
If the buffer is full
   A physical write is issued
Note that the label vector denotes the size of the Labeled Table matrix.
If the matrix to be written has a larger number of rows or columns
   A fatal error message is outputted
If the matrix to be written has a smaller number of row or columns
   Null values are written into the lower right region of the matrix.

It is assumed the matrix is stored in row major.
*******************************************************************************/
void put_r8_matrix(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed. */
    struct MATRIX *in,		/*number of rows & columns of input matrix    */
    struct MATRIX *lt_size,	/*number of rows & columns of LT matrix       */
    double matrix[],		/*double matrix written into the I/O buffer   */
    char   sep)		/*field or record separator to be written to  */
				/*the I/O buffer after the real matrix        */
{
int col_index;
int row_index;
int index;
char msg[ERRLEN];

/* If matrix to be written is larger than the Labeled Table matrix, output
*  a fatal error message.  */
if ((in->nrow > lt_size->nrow) || (in->ncol > lt_size->ncol))
   {
   sprintf(msg,"%s  %s %d x %d.  %s %d x %d.",
           "Matrix to be written to the Labeled Table is too large.",
           "Input matrix is",in->nrow,in->ncol,
           "Labeled Table matrix is",lt_size->nrow,lt_size->ncol);
   tab_error(tab,TAB_LT_MATRIX_SIZE,FATAL,msg,"put_record");
   return;
   }

/* If matrix to be written is null, then write the null matrix to the LT */
else if ((in->nrow == 0) && (in->ncol == 0))
   put_null_matrix(tab,lt_size,sep);

/* If matrix to be written is the same size as the Labeled Table matrix, write
*  the matrix to the Labeled Table so that the last field of each row is
*  terminated by FIELD_SEP2 (new line character), and the last field of the
*  last row is terminated by input parameter "sep" (field separator or record
*  separator).  */
else if ((in->nrow == lt_size->nrow) && (in->ncol == lt_size->ncol))
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_r8_field(tab,matrix[index],FIELD_SEP1);
      put_r8_field(tab,matrix[index],FIELD_SEP2);
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_r8_field(tab,matrix[index],FIELD_SEP1);
   put_r8_field(tab,matrix[index],sep);
   }

/* If matrix to be written is smaller than the Labeled Table matrix, write
*  the matrix as a upper left matrix.  The lower right portion of the matrix
*  is null filled.  The last field of each row is terminated by FIELD_SEP2 
*  (new line character), and the last field of the last row is terminated by 
*  input parameter "sep" (field separator or record separator).  */
else
   {
   for (index = 0,row_index = 1; row_index < in->nrow; row_index++,index++)
      {
      for (col_index = 1; col_index < in->ncol; col_index++,index++)
         put_r8_field(tab,matrix[index],FIELD_SEP1);
      if (col_index == lt_size->ncol)		/* column is correct size    */
         put_r8_field(tab,matrix[index],FIELD_SEP2);
      else					/* null fill extra columns   */
         {
         put_r8_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      }
   for (col_index = 1; col_index < in->ncol; col_index++,index++)
      put_r8_field(tab,matrix[index],FIELD_SEP1);
   if ((row_index == lt_size->nrow) && (col_index == lt_size->ncol))
      put_r8_field(tab,matrix[index],sep);
   else if ((row_index == lt_size->nrow) && (col_index < lt_size->ncol))
      {
      put_r8_field(tab,matrix[index],FIELD_SEP1);
      for (++col_index; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   else
      {
      if (col_index == lt_size->ncol)			/* finish current row */
         put_r8_field(tab,matrix[index],FIELD_SEP2);
      else
         {
         put_r8_field(tab,matrix[index],FIELD_SEP1);
         for (++col_index; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (++row_index; row_index < lt_size->nrow; row_index++) 
	 {				/* write null values for extra rows   */
         for (col_index = 1; col_index < lt_size->ncol; col_index++)
            put_null_field(tab,FIELD_SEP1);
         put_null_field(tab,FIELD_SEP2);
         }
      for (col_index = 1; col_index < lt_size->ncol; col_index++)
         put_null_field(tab,FIELD_SEP1);
      put_null_field(tab,sep);
      }
   }
return;
}






void put_record(struct TAB_DEF *tab)
{
struct VECTOR *vec;		/* pointer to the current label vector        */
char sep;

++tab->currec;
for (vec=tab->vector; vec!=NULL; vec=vec->next)
   {
   if (vec->next == NULL)
      sep = RECORD_SEP;
   else 
      sep = vec->sep;
   switch (vec->dtype)
      {
      case I1:
	put_i1_matrix(tab,&vec->size,&vec->lt_size,vec->ptr.i1,sep);
	break;
      case I2:
	put_i2_matrix(tab,&vec->size,&vec->lt_size,vec->ptr.i2,sep);
	break;
      case I4:
	put_i4_matrix(tab,&vec->size,&vec->lt_size,vec->ptr.i4,sep);
	break;
      case R4:
	put_r4_matrix(tab,&vec->size,&vec->lt_size,vec->ptr.r4,sep);
	break;
      case R8:
	put_r8_matrix(tab,&vec->size,&vec->lt_size,vec->ptr.r8,sep);
	break;
      case C:
      case LTDATE:
      case LTTIME:
	put_string_field(tab,vec->ptr.c,sep,TRUE);
	break;
      default:
	tab_error(tab,TAB_LT_DTYPE,FATAL,"","put_record");
	return;
  	break;
      }
   }
return;
}
