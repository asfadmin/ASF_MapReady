/*****************************************************************************
NAME			pxconv

PURPOSE		Convert an input array from one data type to another.  The 
		output array will be of the same type as 'totyp'.

PROGRAM HISTORY
	PROGRAMMER	DATE	REASON
	----------	----	------
	D.GORDON	5/7/85	 Original development for NEWLAS
	J.REED		11/12/85 Optimized byte portion of routine.
	D.GORDON	11/20/85 Optimized all routines.
	B.Ailts		 12/87	 Changed include directory specifications
				 Use raw 'C' types
				 Place bridge routines in a seperate file
	B.Ailts		 04/88   replaced newlas.a with las.a
	B.Ailts		 12/88	 When converting to a smaller data type, the
				 output value is set to the max or min of the
				 output data type if the input value is above
				 or below the max or min of the output data
				 type
	O. Lawlor	 8/97  Added function prototypes.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS
		Part of the LAS system.

PROJECT				LAS

ALGORITHM

receive:
	-the input array and its data type
	-the output array and its data type
	-the number of samples of the input array contains.
depending on the type of input array values
	call a function for that particular type of conversion

for each pixel of the input arrays
	each the pixels from each input array are type cast into the output 
	array
return

ALGORITHM REFERENCES		none
*******************************************************************************/
#include "asf.h"
#include "worgen.h"
#include "pixman.h"
/*We can define the easy conversion functions right up front.*/
#define construct_cvt_func(name,fromType,toType) \
void name(fromType *fromBuf,toType *toBuf,int size); \
void name(fromType *fromBuf,toType *toBuf,int size) \
{ \
	while (size--) *toBuf++=(toType)*fromBuf++;\
}
construct_cvt_func(byte_word,unsigned char,short)
construct_cvt_func(byte_long,unsigned char,int)
construct_cvt_func(byte_real,unsigned char,float)
construct_cvt_func(byte_double,unsigned char,double)
construct_cvt_func(word_long,short,int)
construct_cvt_func(word_real,short,float)
construct_cvt_func(word_double,short,double)
construct_cvt_func(long_real,int,float)
construct_cvt_func(long_double,int,double)
construct_cvt_func(real_double,float,double)

void word_byte(short *frombuf,unsigned char *tobuf,int size);
void long_byte(int *frombuf,unsigned char *tobuf,int size);
void long_word(int *frombuf,short *tobuf,int size);
void real_byte(float *frombuf,unsigned char *tobuf,int size);
void real_word(float *frombuf,short *tobuf,int size);
void real_long(float *frombuf,int *tobuf,int size);
void double_byte(double *frombuf,unsigned char *tobuf,int size);
void double_word(double *frombuf,short *tobuf,int size);
void double_long(double *frombuf,int *tobuf,int size);
void double_real(double *frombuf,float *tobuf,int size);

void word_byte(short *frombuf,unsigned char *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > MAXUTINY)
	      *tobuf = MAXUTINY;
	   else if (*frombuf < MINUTINY)
	      *tobuf = MINUTINY;
	   else
	      *tobuf = (unsigned char)*frombuf;
}

void long_byte(int *frombuf,unsigned char *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > MAXUTINY)
	      *tobuf = MAXUTINY;
	   else if (*frombuf < MINUTINY)
	      *tobuf = MINUTINY;
	   else
	      *tobuf = (unsigned char)*frombuf;
}


void long_word(int *frombuf,short *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > ASF_MAXWORD)
	      *tobuf = ASF_MAXWORD;
	   else if (*frombuf < ASF_MINWORD)
	      *tobuf = ASF_MINWORD;
	   else
	      *tobuf = (short)*frombuf;
}


void real_byte(float *frombuf,unsigned char *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > MAXUTINY)
	      *tobuf = MAXUTINY;
	   else if (*frombuf < MINUTINY)
	      *tobuf = MINUTINY;
	   else
	      *tobuf = (unsigned char)*frombuf;
}

void real_word(float *frombuf,short *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > ASF_MAXWORD)
	      *tobuf = ASF_MAXWORD;
	   else if (*frombuf < ASF_MINWORD)
	      *tobuf = ASF_MINWORD;
	   else
	      *tobuf = (short)*frombuf;
}

void real_long(float *frombuf,int *tobuf,int size)
{
	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > ASF_MAXLONG)
	      *tobuf = ASF_MAXLONG;
	   else if (*frombuf < ASF_MINLONG)
	      *tobuf = ASF_MINLONG;
	   else
	      *tobuf = (int)*frombuf;
}

void double_byte(double *frombuf,unsigned char *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > MAXUTINY)
	      *tobuf = MAXUTINY;
	   else if (*frombuf < MINUTINY)
	      *tobuf = MINUTINY;
	   else
	      *tobuf = (unsigned char)*frombuf;
}

void double_word(double *frombuf,short *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > ASF_MAXWORD)
	      *tobuf = ASF_MAXWORD;
	   else if (*frombuf < ASF_MINWORD)
	      *tobuf = ASF_MINWORD;
	   else
	      *tobuf = (short)*frombuf;
}

void double_long(double *frombuf,int *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > ASF_MAXLONG)
	      *tobuf = ASF_MAXLONG;
	   else if (*frombuf < ASF_MINLONG)
	      *tobuf = ASF_MINLONG;
	   else
	      *tobuf = (int)*frombuf;
}

void double_real(double *frombuf,float *tobuf,int size)
{

	for(; size--; tobuf++, frombuf++)
	   if (*frombuf > MAXREAL)
	      *tobuf = MAXREAL;
	   else if (*frombuf < MINREAL)
	      *tobuf = MINREAL;
	   else
	      *tobuf = (float)*frombuf;
}


lasErr c_pxconv(int ftyp,int totyp, const unsigned char *frombuf,unsigned char * tobuf, int size)
{ /* pxconv */

	int status = 0;

	switch (ftyp)
	{
	case EBYTE:
		switch (totyp)
		{
		case EBYTE:
			c_pxcopy((unsigned char *)frombuf, (unsigned char *)tobuf, totyp, size);
			break;
		case EWORD:
			byte_word((unsigned char *)frombuf,(short *)tobuf,size);
			break;
		case ELONG:
			byte_long((unsigned char *)frombuf,(int *)tobuf,size);
			break;
		case EREAL:
			byte_real((unsigned char *)frombuf,(float *)tobuf,size);
			break;
		case EDOUBLE:
			byte_double((unsigned char *)frombuf,(double *)tobuf,size);
			break;
		}
		break;
	case EWORD:
		switch (totyp)
		{
		case EBYTE:
			word_byte((short *)frombuf,(unsigned char *)tobuf,size);
			break;
		case EWORD:
			c_pxcopy((unsigned char *)frombuf, (unsigned char *)tobuf, totyp, size);
			break;
		case ELONG:
			word_long((short *)frombuf,(int *)tobuf,size);
			break;
		case EREAL:
			word_real((short *)frombuf,(float *)tobuf,size);
			break;
		case EDOUBLE:
			word_double((short *)frombuf,(double *)tobuf,size);
			break;
		}
		break;
	case ELONG:
		switch (totyp)
		{
		case EBYTE:
			long_byte((int *)frombuf,(unsigned char *)tobuf,size);
			break;
		case EWORD:
			long_word((int *)frombuf,(short *)tobuf,size);
			break;
		case ELONG:
			c_pxcopy((unsigned char *)frombuf, (unsigned char *)tobuf, totyp, size);
			break;
		case EREAL:
			long_real((int *)frombuf,(float *)tobuf,size);
			break;
		case EDOUBLE:
			long_double((int *)frombuf,(double *)tobuf,size);
			break;
		}
		break;
	case EREAL:
		switch (totyp)
		{
		case EBYTE:
			real_byte((float *)frombuf,(unsigned char *)tobuf,size);
			break;
		case EWORD:
			real_word((float *)frombuf,(short *)tobuf,size);
			break;
		case ELONG:
			real_long((float *)frombuf,(int *)tobuf,size);
			break;
		case EREAL:
			c_pxcopy((unsigned char *)frombuf, (unsigned char *)tobuf, totyp, size);
			break;
		case EDOUBLE:
			real_double((float *)frombuf,(double *)tobuf,size);
			break;
		}
		break;
	case EDOUBLE:
		switch (totyp)
		{
		case EBYTE:
			double_byte((double *)frombuf,(unsigned char *)tobuf,size);
			break;
		case EWORD:
			double_word((double *)frombuf,(short *)tobuf,size);
			break;
		case ELONG:
			double_long((double *)frombuf,(int *)tobuf,size);
			break;
		case EREAL:
			double_real((double *)frombuf,(float *)tobuf,size);
			break;
		case EDOUBLE:
			c_pxcopy((unsigned char *)frombuf, (unsigned char *)tobuf, totyp, size);
			break;
		}
		break;
	}
if (!status)
   status = E_SUCC;
return ((lasErr)status);
}
