/*
Bit_errors:
	A tiny program to add bit errors to any file.
The bit error rate is given as a parameter.  For speed,
the bit error pattern is stored in a buffer, and repeats
again and again.

	The lowest bit error rate it will really produce
is 1/0x7fff, or 0.00003.  Larger errors will be even multiples
of this base rate (rounded down).

	The program has "stable" error behaviour-- if a bit
is wrong at 0.001 bit error rate, it will still (always) be wrong
at that or any higher rate.  The same rate will always give the
same errors.

	My definition of bit error rate is "at a BER of 0.2,
20% of the bits will be wrong (flipped from their true value),
in a uniform fashion".

*/
#include <stdio.h>

int main(int argc,char *argv[])
{
	char *inName,*outName;
	FILE *inF,*outF;
	int blockNo;
	double errorRate=0;
	int errorThreshold;
#define bytesPerBlock 20000
	int errorBlock[bytesPerBlock];/*Contains the bit error mask for the datatake*/
	int i;
	if (argc!=4) {printf("Usage: bit_errors <in> <desired bit error rate> <out>\n");return 1;}
	inName=argv[1];
	sscanf(argv[2],"%lf",&errorRate);
	outName=argv[3];
	inF=fopen(inName,"rb");
	errorThreshold=(int)(errorRate*0x07fff);
	outF=fopen(outName,"wb");
	printf("Error threshold is %d\n",errorThreshold);
	
	for (i=0;i<bytesPerBlock;i++)
	{
		int outByte=0;
		int bitNo;
		for (bitNo=0;bitNo<8;bitNo++)
		{
			if ((rand()&0x07fff)<errorThreshold)
				outByte|=(1<<bitNo);/*Add a bit error*/
		}
		if (outByte!=0)
			printf("Error block byte %d is %x\n",i,outByte);
		errorBlock[i]=(unsigned char)outByte;
	}
	
	blockNo=0;
	while (1)
	{
		unsigned char buf[bytesPerBlock];
		int nRead=0;
		
		if (0>=(nRead=fread(buf,1,bytesPerBlock,inF)))
			{printf("%d blocks written\n",blockNo);return 0;}
		for (i=0;i<bytesPerBlock;i++)
			buf[i]^=errorBlock[i];
		fwrite(buf,nRead,1,outF);
		blockNo++;
	}
}
