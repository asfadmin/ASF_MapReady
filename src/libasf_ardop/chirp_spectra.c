/*
Chirp_Spectra:
A tiny program to create a spectrum image of a 
satellite chirp.  The chirp is read from a file,
FFT'd, and its power is written to a LAS float image.

This is only useful for debugging chirp files.

*/

#include "asf.h"
#include "ardop_defs.h"
#include "read_signal.h"

void read_reference(char *fName,complexFloat *ref,int *len)
{
	char buf[255];
	FILE *f=FOPEN(fName,"r");
	float r,i;
	(*len)=0;
	while (NULL!=fgets(buf,255,f))
	{
		if (2==sscanf(buf,"%f%f",&r,&i))
		{/*Two floats sitting on the line-- probably OK.*/
			ref[*len].r=r;
			ref[*len].i=i;
			(*len)++;
		}
	}
}

main(int argc,char **argv)
{
#define fftLen 16
	int x,y,i;

	char tmp[100];	
	complexFloat ref[100000];
	int refLen;
	complexFloat fftBuf[fftLen];
	float outBuf[fftLen];
	FILE *outF;
	if (argc!=3)
		{printf("Usage:chirp_spectra <in chirp> <out spectra>\n"
		"\n"
		"Computes the spectrum of %d-sample chunks of the given\n"
		"reference function.  Writes output to given LAS file.\n"
		"ASF ISAR Tools, 1998\n",fftLen);exit(1);}
	
	read_reference(argv[1],ref,&refLen);
	cfft1d(fftLen,NULL,0);
	
/*Compute reference spectra.*/
	refLen/=fftLen;
	sprintf(tmp,"makeddr %s %d %d float\n",argv[2],refLen,fftLen);
	system(tmp);
	outF=FOPEN(argv[2],"w");
	for (y=0;y<refLen;y++)
	{
		for (x=0;x<fftLen;x++)
			fftBuf[x]=ref[y*fftLen+x];
		cfft1d(fftLen,fftBuf,-1);
		for (i=0;i<fftLen;i++) 
			outBuf[i]=Cabs(fftBuf[i]);
		fwrite(outBuf,sizeof(float),fftLen,outF);
	}
	FCLOSE(outF);
	
	return 0;
}

