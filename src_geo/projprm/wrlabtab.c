/*****************************************************************************
NAME:				 WRLABTAB 

PURPOSE:  Opens and writes the parameters to the output labeled table file.

PROGRAM HISTORY:
VERSION		DATE	AUTHOR		CODE/CONT   REASON
-------		----	------		---------   ------
 NEWLAS		9/87	B. Ailts	   CSB	    original development
  5.0		1/89	D. Steinwand	   CSB      retrocode to 5.0
  5.01		8/89	D. Steinwand	   CSB      Fixed problem for SUN 4
					 	    (wrote to ptr->ptr.c
						    instead of ptr->ptr)

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
   PROJPRM must be run under TAE

PROJECT:  LAS

ALGORITHM DESCRIPTION:
	Initialize the variables needed.
	Open output label table file.
	Write the parameter values to the output matrix.
	Write the matrix record to the output labeled table file.
	Close the labeled table file
	Return
*****************************************************************************/
#include "asf.h"
#include "las.h"
#include "ltable.h"

void wrlabtab(hosout,prjkey,prjunits,prjzone,prjtype,prjsph,prjparms)

char *hosout;			/* output host file name */
double *prjparms;		/* projection parameters */
int *prjunits;			/* Projection code */
int *prjzone;			/* projection zone */
int *prjtype;			/* projection type */
int *prjsph;			/* projection sphere */
char *prjkey;			/* projection key */
{
struct MATRIX size;		/* matrix structure of label table */
struct TAB_DEF tab;		/* tabular structure of label table */
struct VECTOR *ptr;		/* pointer to each vector of label table */
char fdescbuf[80];		/* Data buffer */
char ftypebuf[80];		/* Data buffer */

/* Initialize the variables needed.
-----------------------------------*/
tab.nsubs = 0;
tab.subfile[0]=NULL;
strcpy(ftypebuf, "PROJFILE");
tab.ftype = ftypebuf;
strcpy(fdescbuf, "Projection parameter file");
tab.fdesc = fdescbuf;
tab.ncol = 6;

/*  Open output label table file.
---------------------------------*/
open_tab(&tab,hosout,IUPDATE);

put_vector(&tab,"PROJKEY",C,"Uniquely identify the defined projection",1,8,
	   FIELD_SEP2);
put_vector(&tab,"PROJTYPE",I4,"Projection type code",1,1,FIELD_SEP2);
put_vector(&tab,"PROJZONE",I4,"Projection zone code",1,1,FIELD_SEP2);
put_vector(&tab,"PROJUNITS",I4,"Units code",1,1,FIELD_SEP2);
put_vector(&tab,"PROJSPH",I4,"Speroid code",1,1,FIELD_SEP2);
put_vector(&tab,"PROJPARMS",R8,"Projection parameters",1,15,RECORD_SEP);

/*  Write the parameter values to the output matrix.
----------------------------------------------------*/
size.nrow = 1;
size.ncol = 8;
ptr = tab.vector;
strcpy(ptr->ptr.c,prjkey);

size.ncol = 1;
ptr = ptr->next;
put_matrix(ptr,I4,&size,(unsigned char *)prjtype);
ptr = ptr->next;
put_matrix(ptr,I4,&size,(unsigned char *)prjzone);
ptr = ptr->next;
put_matrix(ptr,I4,&size,(unsigned char *)prjunits);
ptr = ptr->next;
put_matrix(ptr,I4,&size,(unsigned char *)prjsph);
size.ncol = 15;
ptr = ptr->next;
put_matrix(ptr,R8,&size,(unsigned char *)prjparms);

/*  Write the matrix record to the output labeled table file.
-------------------------------------------------------------*/
put_record(&tab);

/*  Close the labeled table file
--------------------------------*/
close_tab(&tab);
return;
}
