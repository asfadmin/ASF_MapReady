/*
These routines interpret an array of bytes as
an array of 4-byte big-endian integers:
This way, machines with larger integer sizes (for example,
8 or 16 byte integers) can read and write 
"standard" (4 byte integer) files.
*/
#include "las.h"

void byte2intArr(unsigned char *inBuf,int *outArr,int nInts)
{
	int i;
	for (i=0;i<nInts;i++)
		outArr[i]=((int)inBuf[i*4+0])<<24|
			((int)inBuf[i*4+1])<<16|
			((int)inBuf[i*4+2])<<8|
			((int)inBuf[i*4+3])<<0;
}

void int2byteArr(int *inArr,unsigned char *outBuf,int nInts)
{
	int i;
	for (i=0;i<nInts;i++)
	{
		outBuf[i*4+0]=0xff&(inArr[i]>>24);
		outBuf[i*4+1]=0xff&(inArr[i]>>16);
		outBuf[i*4+2]=0xff&(inArr[i]>>8);
		outBuf[i*4+3]=0xff&(inArr[i]>>0);
	}	
}
