/*Endian.c:
	Implements the byte-swapping interface defined in
endian.h.  See endian.h for comments.
	Orion Lawlor, ASF-STEP, 6/98. */
#include "asf_endian.h"
/*
Each of these functions exchanges the bytes
in a run of 16, 32, or 64 bits "in" points to.
Though these are fast and efficient for 16 or 32 bits,
since we can't be sure there's a built-in type
for 64 bits we have to swap using a loop.
*/

void swap16(unsigned char *in)
{
	int tmp=in[0];
	in[0]=in[1];
	in[1]=tmp;
}
void swap32(unsigned char *in)
{
	int tmp=in[0];
	in[0]=in[3];
	in[3]=tmp;
	tmp=in[1];
	in[1]=in[2];
	in[2]=tmp;
}
void swap64(unsigned char *in)
{
	int i;
	for (i=0;i<4;i++)
	{
		int tmp=in[i];
		in[i]=in[7-i];
		in[7-i]=tmp;
	}
}

/*Each of these functions reads the given character
array, and parses it as a integer.  This works
despite varying integer sizes and formats.*/
int lilInt16(unsigned char *in)
{
	return (((int)in[1])<<8)|((int)in[0]);
}
int lilInt32(unsigned char *in)
{
	return (((int)in[3])<<24)|(((int)in[2])<<16)|
		(((int)in[1])<<8)|((int)in[0]);
}

int bigInt16(unsigned char *in)
{
	return (((int)in[0])<<8)|((int)in[1]);
}
int bigInt32(unsigned char *in)
{
	return (((int)in[0])<<24)|(((int)in[1])<<16)|
		(((int)in[2])<<8)|((int)in[3]);
}

/*Each of these functions writes the given character
array so it contains the given integer.  This works
despite varying integer sizes and formats.*/

void lilInt16_out(int in,unsigned char *out)
{
	out[1]=0xff&(in>>8);
	out[0]=0xff&(in>>0);
	
}
void lilInt32_out(int in,unsigned char *out)
{
	out[3]=0xff&(in>>24);
	out[2]=0xff&(in>>16);
	out[1]=0xff&(in>>8);
	out[0]=0xff&(in>>0);
	
}
void bigInt16_out(int in,unsigned char *out)
{
	out[0]=0xff&(in>>8);
	out[1]=0xff&(in>>0);
}
void bigInt32_out(int in,unsigned char *out)
{
	out[0]=0xff&(in>>24);
	out[1]=0xff&(in>>16);
	out[2]=0xff&(in>>8);
	out[3]=0xff&(in>>0);
}
