#ifndef _ITABLE_H_
#define _ITABLE_H_

#define FATAL	 -1		/* fatal error encountered */
#define NONFATAL  1		/* nonfatal error encountered */

#define TAB_LT_OPEN		-1
#define TAB_LT_ACCESS		-2
#define TAB_LT_READ		-3
#define TAB_LT_WRITE		-4
#define TAB_LT_CLOSE		-5
#define TAB_LT_MATRIX_SIZE	-6
#define TAB_LT_FORMAT		-7
#define TAB_LT_DTYPE		-8
#define TAB_LT_EOR		-9
#define TAB_LT_NOEOR		-10
#define TAB_FORTRAN_MAXFLD	-11
#define TAB_ALLOC		-12

#define TAB_LT_SEEK		-13
#define TAB_LT_FTYPE		-14
#define TAB_LT_SUBFIL		-15
#define TAB_LT_PUTVEC		-16
#define TAB_LT_GETDATE		-17
#define TAB_LT_GETTIME		-18

#ifndef min
#define min(a,b)  ((a) < (b) ? (a) : (b))
#endif
#ifndef max
#define max(a,b)  ((a) > (b) ? (a) : (b))
#endif

#ifndef FALSE
#define FALSE     0		/* boolean value for FALSE */
#endif
#ifndef TRUE
#define TRUE	  1		/* boolean value for TRUE  */
#endif

#define MAX_SUB  20		/* maximum number of associated LT files    */
#define TAE_VAL  50		/* max number of TAE values for a parameter */

#define FIELD_SEP1 ','		/* logical record field separator 1 */
#define FIELD_SEP2 '\n'		/* logical record field separator 2 */
#define RECORD_SEP ';'		/* logical record separator */
#define CHAR_DEL   '\"'		/* character delimiter */

#define FSEP1	0		/* field separator 1 for FORTRAN programs */
#define FSEP2	1		/* field separator 2 for FORTRAN programs */
#define RSEP	2		/* record separator for FORTRAN programs  */

#define LIN_SIZ         80      /* number characters without a new line */
#define MAX_FIELD_LEN 1024      /* max number of characters in a record field */
#define TAB_SIZ       8192	/* labeled table physical record size */

#define C	0		/* character data type */
#define I1	1		/* 8-bit integer data type */
#define I2	2		/* 16-bit integer data type */
#define I4	3		/* 32-bit integer data type */
#define R4	4		/* 32-bit real data type */
#define R8	5		/* 64-bit real data type */
#define LTDATE  6		/* Date stored as char string mm/dd/yyyy */
#define LTTIME	7		/* Time stored as char string hh:mm:ss */
#define NDTYPE  8		/* total number of data types */

extern char DATA_TYPE[NDTYPE][7];

#include "desc.h"

struct LT_DATE
   {
   int month;
   int day;
   int year;
   };

struct LT_TIME
   {
   int hour;
   int minute;
   int second;
   };

struct MATRIX			/* Structrue defining the number of rows    */
   {				/*   and columns of a matrix                */
   int nrow;
   int ncol;
   };

struct VECTOR			/* Recursive structure defining the         */
   {				/*   label vectors of the Labeled Table.    */
   char *label;			/* label name                               */
   int dtype;			/* data type of the vector                  */
   char *desc;			/* label description                        */
   union			/* define a union which depends on the data */
      {				/*   type as to which pointer is currently  */
      char   *c;		/*   valid.  There is a separate pointer    */
      char   *i1;		/*   for each of the valid data types.      */
      short  *i2;
      int   *i4;
      float  *r4;
      double *r8;
      } ptr;
   struct MATRIX lt_size;	/* number of rows & columns in labeled table*/
   struct MATRIX size;		/* actual number of rows & columns in data  */
   int null;			/* flag indicating null fields within data  */
   char sep;			/* type of separator                        */
   int flag;			/* boolean flag used for APPEND access      */
   struct VECTOR *next;		/* pointer to next vector                   */
   };

struct TAB_DEF
   {
   char hname[CMLEN];		/* host name of the Labeled Table          */
   char *subfile[MAX_SUB];	/* list of files associated to this LT      */
   int nsubs;			/* number of files associated to this LT    */
   int fd;			/* file descriptor to label table           */
   int access;			/* access the Labeled Table was opened for */
   char *ftype;			/* file type of the labeled table           */
   char *fdesc;			/* file description of the labeled table    */
   struct DESC buf;		/* data buffer                              */
   int ncol;			/* number of columns in the LT              */
   struct VECTOR *vector;	/* column vector                            */
   int nbytes;			/* number of bytes in data buffer           */
   int cur;			/* current byte offset into the data buffer */
   int currec;			/* current logical record being processed   */
   int label_flag;		/* flag indicating data has been written    */
   };

void close_tab(struct TAB_DEF *tab);	/* Structure defining the Labeled Table files */
lasErr cltab(int *fd);
void free_vec(struct VECTOR *vec);

/*Error.c:*/
void tab_error(struct TAB_DEF *tab,
    int err_code,int severity,char msg[],char key[]);

/*Get_record.c:*/
lasErr get_record(struct TAB_DEF *tab);
void get_i1_field(struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    char *val,int *null,int *eor);
void get_i1_matrix(struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    char   matrix[],int   *null,int   *eor);
void get_i2_field(struct TAB_DEF *tab,	/* Labeled table to be processed	      */
    short *val,			/* 16-bit value read from LT I/O buffer       */
    int  *null,int  *eor);
void get_i2_matrix(struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    short  matrix[],int   *null,int   *eor);
void get_i4_field(struct TAB_DEF *tab,	/* Labeled Table to be processed.	      */
    int *val,int  *null,int  *eor);
void get_i4_matrix(struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* Number of rows & columns of LT matrix      */
    struct MATRIX *out,	/* Actual num of rows & columns of matrix read*/
    int   matrix[],int   *null,int   *eor);
void get_r4_field(struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    float  *val,int   *null,int   *eor);
void get_r4_matrix(struct TAB_DEF *tab,	/* Labeled Table file to be processed.        */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    float  matrix[],int   *null,int   *eor);
void get_r8_field(struct TAB_DEF *tab,	/* Labeled Table to be processed.	      */
    double *val,int  *null,int  *eor);
void get_r8_matrix(struct TAB_DEF *tab,	/* Labeled Table file to be processed	      */
    struct MATRIX *lt_size,	/* number of rows & columns of LT matrix      */
    struct MATRIX *out,		/* actual num of rows & columns of matrix read*/
    double matrix[],int   *null,int   *eor);

/*Put_record.c:*/
 void put_i4_field (struct TAB_DEF *tab, int val, char sep); 
 void put_i1_matrix (struct TAB_DEF *tab, struct MATRIX *in, struct MATRIX *lt_size, char *matrix, char sep); 
 void put_i2_matrix (struct TAB_DEF *tab, struct MATRIX *in, struct MATRIX *lt_size, short int *matrix, char sep); 
 void put_i4_matrix (struct TAB_DEF *tab, struct MATRIX *in, struct MATRIX *lt_size, int *matrix, char sep); 
 void put_r4_field (struct TAB_DEF *tab, float val, char sep); 
 void put_r4_matrix (struct TAB_DEF *tab, struct MATRIX *in, struct MATRIX *lt_size, float *matrix, char sep); 
 void put_r8_field (struct TAB_DEF *tab, double val, char sep); 
 void put_r8_matrix (struct TAB_DEF *tab, struct MATRIX *in, struct MATRIX *lt_size, double *matrix, char sep); 


/*various:*/
void get_vector(struct TAB_DEF *tab);

struct VECTOR *get_field_ptr(struct TAB_DEF *tab,char *name);

char *get_string_field(struct TAB_DEF *tab,int *eor);

void open_tab(struct TAB_DEF *tab,char *fname,int access);

void read_tab(struct TAB_DEF *tab);
void write_tab(struct TAB_DEF *tab);

int rdtab(int *fd,int *nbytes,struct DESC *buffer);
lasErr wrtab(int *fd,int *nbytes,struct DESC *buffer);


void put_null_field(struct TAB_DEF *tab,char sep);
void put_null_matrix(struct TAB_DEF *tab,struct MATRIX *size,char  sep);
void put_string_field(struct TAB_DEF *tab,char *val,char sep,int flag);

void put_matrix(
    struct VECTOR *vec,		/*  VECTOR structuref			  */
    int idtype,		/*  input data type			  */
    struct MATRIX *size,	/*  Matrix structure			  */
    unsigned char *matrix);	/*  Output buffer			  */
 
void put_vector(
    struct TAB_DEF *tab,	/*structure defining LT file to be processed. */
    char label[],		/*label name of the vector		      */
    int dtype,			/*data type of the label vector		      */
    char desc[],		/*description of the label vector	      */
    int nrow,			/*number of rows			      */
    int ncol,			/*number of columns			      */
    char sep);			/*separator (FIELD_SEP1,FIELD_SEP2,RECORD_SEP)*/


/*Put_record.c:*/
void put_record(struct TAB_DEF *tab);
#endif
