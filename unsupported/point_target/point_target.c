/*****************************************************************************
NAME: point_target

SYNOPSIS: point_target in.img line sample out.img

DESCRIPTION:
	Point_target blows up the brightest target near the
given point in the given image.  A logarithmic, blown-up
output image is created.
    

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
	asf_fft.a: FFT Library

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/99   O. Lawlor    Needed to find ISLR/PSLR for AISP.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*****************************************************************************/
#include "asf.h"
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "point.h"

#define VERSION 1.0

typedef struct {
	float r,i;
} complex;

float cPower(complex x) {return x.r*x.r+x.i*x.i;}

void usage(void);

int main(int argc,char **argv)
{
	int in_mX,in_mY; /*Invariant: 2^mX=ns; 2^mY=nl. (input image)*/
	int out_mX,out_mY; /*Invariant: 2^mX=ns; 2^mY=nl.(zoomed output image)*/
	int nl,ns;/*Size of zoomed image*/

	int chipX,chipY, /*Chip location (top left corner) in image*/
		chipDX,chipDY; /*Chip size in image.*/
	
	int pointX,pointY;/*User's point of interest*/
	int x,y;
	char *inFile=NULL,*outFile=NULL;
	FILE *inF,*outF;
	float *inBuf,*outBuf;
	complex *fftBuf,*zoomBuf,cZero={0.0,0.0};
	struct DDR inDDR,outDDR;
	
/*Process CLA's.*/
	if (argc!=5)
		usage();
	inFile=argv[1];
	pointY=atoi(argv[2]);
	pointX=atoi(argv[3]);
	outFile=argv[4];

/*Open input files.*/
	inF=fopenImage(inFile,"rb");
	outF=fopenImage(outFile,"wb");
	c_getddr(inFile,&inDDR);
	
/*Round to find nearest power of 2 for FFT size.*/
	in_mX=(int)(log(zoomSize)/log(2.0)+0.5);
	in_mY=(int)(log(zoomSize)/log(2.0)+0.5);
	fft2dInit(in_mY, in_mX);
	chipDX=1<<in_mX;
	chipDY=1<<in_mY;
	chipX=pointX-chipDX/2;
	chipY=pointY-chipDY/2;
	printf("\tChip at %dx%d, size=%dx%d\n",chipX,chipY,chipDX,chipDY);
	
	out_mX=(int)(log(zoomSize*zoomFactor)/log(2.0)+0.5);
	out_mY=(int)(log(zoomSize*zoomFactor)/log(2.0)+0.5);
	fft2dInit(out_mY, out_mX);
	ns=1<<out_mX;
	nl=1<<out_mY;
	
	printf("\tInput FFT Size: %d samples by %d lines\n",chipDX,chipDY);
	printf("\tOutput FFT Size: %d samples by %d lines\n",ns,nl);
	

/*Create output DDR*/
	c_intddr(&outDDR);
	outDDR.dtype=4;
	outDDR.nbands=1;
	outDDR.nl=nl;
	outDDR.ns=ns;
	c_putddr(outFile,&outDDR);
	
/*Read input chip*/
	inBuf=(float *)MALLOC(sizeof(float)*inDDR.ns);
	fftBuf=(complex *)MALLOC(sizeof(complex)*(chipDX*chipDY));
	zoomBuf=(complex *)MALLOC(sizeof(complex)*(ns*nl));
	for (y=0;y<chipDY;y++)
	{
		getFloatLine(inF,&inDDR,chipY+y,inBuf);
		for (x=0;x<chipDX;x++)
		{
			fftBuf[y*chipDX+x].r=inBuf[chipX+x];
			fftBuf[y*chipDX+x].i=0.0;
		}
	}
	
/*FFT input chip.*/
	fft2d((float *)fftBuf,in_mY,in_mX);

/*Zero output chip*/
	for (y=0;y<nl;y++)
		for (x=0;x<ns;x++)
			zoomBuf[y*ns+x]=cZero;
	
/*Copy input chip into low frequencies of output chip*/
	for (y=0;y<chipDY/2;y++)
		for (x=0;x<chipDX/2;x++)
		{
			zoomBuf[y*ns+x]=fftBuf[y*chipDX+x];
			zoomBuf[y*ns+(ns-1-x)]=fftBuf[y*chipDX+(chipDX-1-x)];
			zoomBuf[(nl-1-y)*ns+x]=fftBuf[(chipDY-1-y)*chipDX+x];
			zoomBuf[(nl-1-y)*ns+(ns-1-x)]=fftBuf[(chipDY-1-y)*chipDX+(chipDX-1-x)];
		}
	

/*i-FFT output chip*/
	fft2d((float *)zoomBuf,out_mY,out_mX);
	
/*Convert blown-up data to dB*/
	outBuf=(float *)MALLOC(sizeof(float)*ns*nl);
	for (y=0;y<nl;y++)
		for (x=0;x<ns;x++)
		{
			float val=zoomBuf[y*ns+x].r*(chipDX*chipDY)/(nl*ns);
			if (val<=0.1) val=0.1;
			outBuf[y*ns+x]=10*log(val*val);
		}
	
/*Analyze blown-up data*/
	analyze_blowup(outBuf,ns,nl,chipX,chipY);
	
/*Write out blown-up data*/
	for (y=0;y<nl;y++)
		putFloatLine(outF,&outDDR,y,&outBuf[y*ns]);
	
	return (0);
}
void usage(void)
{

	printf("Usage:\n"
	"\tpoint_target in.img line sample out.img\n"
	"\n"
	"\tin.img: any single-banded LAS amplitude image.\n"
	"\tline, sample: location in above image of target of interest\n"
	"\tout.img: a blown-up image of the logarithm of the\n"
	"\t         brightness of the above point in the above image.\n"
	"\t"
	"\tBlow up the given point of the given image, converting\n"
	"blow-up amplitudes to logarithm values.\n"
	"ASF-STEP Tools, Orion Lawlor.  Version %.2f\n",VERSION);
	exit(1);
}

