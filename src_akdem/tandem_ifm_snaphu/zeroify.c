/******************************************************************************
NAME:  zeroify

DESCRIPTION:

	Zeroify writes an output image whose pixels are those
of the input image, except where the test image has zeros.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/99   O. Lawlor    Needed for correct dem-guided phase unwrapping.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/

#include "asf.h"
#include "ddr.h"

const float VERSION=1.0;
int main(int argc,char *argv[])
{
	float *buf,*testbuf;
	char *outfile,*infile,*testfile;
	FILE *outF,*inF,*testF;
	struct DDR outDDR,inDDR,testDDR;
	int x,y;
	int nl,ns;
	
/*Parse CLA's.*/
	switch (argc)
	{
	case 4:	
		infile=argv[1];
		testfile=argv[2];
		outfile=argv[3];
		break;
	default:
		printf("Usage:\n"
		"zeroify  <input image> <test image> <output image>\n"
		"      All images are arbitrary one-banded LAS images.\n"
		"      The input and test images must exist.\n"
		"\n"
		"zeroify creates an output image which is identical to\n"
		"the input image, but zero where the test image is zero.\n"
		"This is used in dem-guided phase unwrapping, in tandem_ifm.\n"
		"ASF ISAR Tools, Version %.2f\n",VERSION);
		exit(1);
	}
	
/*Open output files.*/
	inF=fopenImage(infile,"rb");
	testF=fopenImage(testfile,"rb");
	outF=fopenImage(outfile,"wb");

/*Read and copy over DDR.*/
	c_getddr(infile,&inDDR);
	c_getddr(testfile,&testDDR);
	outDDR=inDDR;
	c_putddr(outfile,&outDDR);
	
/*Allocate buffers.*/
	buf=(float *)MALLOC(sizeof(float)*inDDR.ns);
	testbuf=(float *)MALLOC(sizeof(float)*testDDR.ns);
	
/*Copy over each line of input to the output.*/
	nl=inDDR.nl;
	ns=inDDR.ns;
	for (y=0;y<nl;y++)
	{
		getFloatLine(inF,&inDDR,y,buf);
		getFloatLine(testF,&testDDR,y,testbuf);
		for (x=0;x<ns;x++)
			if (testbuf[x]==0)
				buf[x]=0;
		putFloatLine(outF,&outDDR,y,buf);
	}
/*Done!*/
	return 0;
}
