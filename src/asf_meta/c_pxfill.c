/*****************************************************************************
NAME			pxfill

PURPOSE		Fill an output array with a specified value.  The 
		output array will be of the same type as 'dtype'.

PROGRAM HISTORY
	PROGRAMMER	DATE	REASON
	----------	----	------
	D.GORDON	5/7/85	 Original development for NEWLAS
	J.REED		11/12/85 Optimized byte portion of routine.
	B.Ailts		 12/87	 Changed include directory specifications
				 Use raw 'C' types
				 Place bridge routines in a seperate file
	B.Ailts		 04/88   replaced newlas.a with las.a

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS
		Part of the LAS system.

PROJECT				NEWLAS

ALGORITHM 

receive:
	-the output array and its data type
	-the number of samples to write to the output array
	-the value to be placed within the output array

Call the routine which corresponds to the input data type

for each sample of the output array
	copy the specified value to the output array
return

ALGORITHM REFERENCES		none
*******************************************************************************/
#include "asf.h"
#include "worgen.h"
#include "pixman.h"

void FUNCTION realfill( float *rtoptr,register int ns,float *fillval);
void FUNCTION bytefill( unsigned char *btoptr,register int ns,unsigned char *fillval);
void FUNCTION wordfill(short *wtoptr,register int ns,short *fillval);
void FUNCTION longfill(int *ltoptr,register int ns,int *fillval);

void FUNCTION c_pxfill(unsigned char *tobuf,int * dtype, int *ns, unsigned char *fillval)

{
	switch (*dtype)
	{
		case EBYTE:
			bytefill(tobuf, *ns, fillval);
			break;
		case EWORD:
			wordfill((short *)tobuf, *ns, (short *)fillval);
			break;
		case ELONG:
			longfill((int *)tobuf, *ns, (int *)fillval);
			break;
		case EREAL:
			realfill((float *)tobuf, *ns, (float *)fillval);
			break;
	}
}

void FUNCTION realfill( float *rtoptr,register int ns,float *fillval)
{
	for(; ns--; *rtoptr++ = *fillval)
		;
}
void FUNCTION bytefill( unsigned char *btoptr,register int ns,unsigned char *fillval)
{
	for(;ns--; *btoptr++ = *fillval)
		;
}
void FUNCTION wordfill(short *wtoptr,register int ns,short *fillval)
{
	for(; ns--; *wtoptr++ = *fillval)
		;
}
void FUNCTION longfill(int *ltoptr,register int ns,int *fillval)
{
	for(; ns--; *ltoptr++ = *fillval)
		;
}
