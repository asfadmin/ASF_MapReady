/*****************************************************************************
NAME			pxcopy

PURPOSE		Copy an input array to an output array.  The 
		output array will be of the same type as the input array.

PROGRAM HISTORY
	PROGRAMMER	DATE	REASON
	----------	----	------
	D.GORDON	5/7/85	 Original development for NEWLAS
	J.REED		11/12/85 Optimized byte portion of routine.
	B.Ailts		 12/87	 Changed include directory specifications
				 Use raw 'C' types
				 Place bridge routines in a seperate file
	B.Ailts		 04/88   replaced newlas.a with las.a
	B.Ailts		 01/88	 Added the VAX memory copy call to help speed up
				 pxcopy

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS
		Part of the LAS system.

PROJECT				NEWLAS

ALGORITHM 

receive:
	-the input array and its data type
	-the output array 
	-the number of samples of the input array contains.

Multiply the number of samples times the number of bytes that the input array's
data type takes up

for each byte of the input arrays
	copy each byte from each input array the output array
return

ALGORITHM REFERENCES		none
*******************************************************************************/
#include "asf.h"

#include "worgen.h"

void FUNCTION c_pxcopy(const unsigned char *frombuf, unsigned char *tobuf, int dtype,int ns)
{
static int datasize[] = {0,1,2,4,4,8};
register int lcnt;

lcnt = ns * datasize[dtype];
memcpy(tobuf,frombuf,lcnt);
}
