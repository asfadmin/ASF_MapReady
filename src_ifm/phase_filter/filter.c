/*****************************************************************************
NAME: phase_filter

SYNOPSIS: phase_filter [-log <file] <in> <strength> <out>

DESCRIPTION:
	phase_filter applies the Goldstein phase filter
to an interferometric phase image.

	An interpolation scheme lifted from Rob Fatland smooths
the edges.


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
	asf_fft.a: FFT Library

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    7/98   O. Lawlor    Filter interferometric phase.
    1.1     7/01   R. Gens	Added log file switch

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*****************************************************************************/
/****************************************************************************
*								            *
*   Filters <in>, a phase image, via the Goldstein filter with strength     *
*           <strength> and writes the result to <out>. 			    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
#include "asf.h"
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"
#include "filter.h"

#define VERSION 1.1

int nl,ns;

void image_filter(FILE *in,meta_parameters *meta,
		FILE *out,float strength);

int main(int argc,char **argv)
{
	int i, optind=1;
	FILE *in,*out;
	char *inFile,*outFile;
	meta_parameters *meta;
	float strength;
	
	if (argc==1 || argc>6) 
	{
		printf("\nUSAGE: phase_filter [-log <file>] <in> <strength> <out>\n"
		"\n\t   <in>     LAS 6.0 sigle-banded image (with extension)\n"
		"\t<strength>  is a real decimal number generally between 1.2"
		"\n\t            and 1.8\n"
		"\t  <out>     LAS 6.0 sigle_banded floating-point image.\n"
		"\t  -log      Allows the output to be written to a log file.\n"     
		"\nFilters <in>, a phase image, via the Goldstein\n"
		"filter with strength <strength> and writes the result to <out>."
		"\nVersion %.1f, ASF SAR TOOLS\n\n",VERSION);
		exit(1);
	}

	logflag=0;
	
/* Check options */
	for (i=1; i<argc; i++) {
	  if (strncmp(argv[i], "-log", 4)==0) {
	    sprintf(logFile, "%s", argv[i+1]);
	    fLog = FOPEN(logFile, "a");
	    logflag=1;
	    i+=1;
	    optind+=2;
	  }
	  else if (strncmp(argv[i], "-", 1)==0) {
	    sprintf(errbuf, "   ERROR: %s is not a valid option!", argv[i]);
	    printErr(errbuf);
	  }
	}

/* Check required input */
	i=optind;
	inFile=argv[i];
	if (1!=sscanf(argv[i+1],"%f",&strength))
	{
		sprintf(errbuf, "   ERROR: '%s' is not floating-point number.\n",argv[i+1]);
		printErr(errbuf);
	}
	outFile=argv[i+2];

	printf("%s\n",date_time_stamp());
	printf("Program: phase_filter\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: phase_filter\n\n");
	}

/*Open input files.*/
	in=fopenImage(inFile,"rb");
	out=fopenImage(outFile,"wb");
	meta = meta_read(inFile);
	
/*Round up to find image size which is an even number of output chunks..*/
	ns=(meta->general->sample_count+ox-1)/ox*ox;
	nl=(meta->general->line_count+oy-1)/oy*oy;
	printf("   Output Size: %d samples by %d lines\n\n",ns,nl);
	if (logflag) {
	  sprintf(logbuf,"   Output Size: %d samples by %d lines\n\n",ns,nl);
	  printLog(logbuf);
	}
	
/*Set up output file.*/
	meta_write(meta, outFile);
	
/*Perform the filtering, write out.*/
	fft2dInit(dMy, dMx);
	image_filter(in,meta,out,strength);

	printf("   Completed 100 percent\n\n");

	return (0);
}

/**************************************************
 read_image: reads the image file given by in & ddr
into the (delX x delY) float array dest.  Reads pixels 
into topleft corner of dest, starting
at (startY , startX) in the input file.
*/
void read_image(FILE *in,meta_parameters *meta, float *dest, 
	int startX,int startY,int delX,int delY)
{
	register int x,y,l;
	int stopY=delY,stopX=delX;
	if ((stopY+startY)>meta->general->line_count)
		stopY = meta->general->line_count - startY;
	if ((stopX+startX)>meta->general->sample_count)
		stopX = meta->general->sample_count - startX;
/*Read portion of input image into topleft of dest array.*/
	for (y=0;y<stopY;y++)
	{
		l=ns*y;
		get_float_line(in,meta,startY+y,&dest[l]);
		for (x=stopX;x<delX;x++)
			dest[l+x]=0.0; /*Fill rest of line with zeros.*/
	}
/*Fill remainder of array (bottom portion) with zero lines.*/
	for (y=stopY;y<delY;y++)
	{
		l=ns*y;
		for (x=0;x<ns;x++)
			dest[l+x]=0.0; /*Fill rest of in2 with zeros.*/
	}
}

/*******************************************
Phase_filter:
	Performs goldstein phase filtering on the given
dx x dy buffer of complex data.

Goldstein phase filtering consists of:
fft; exponentiate amplitude of fft'd data; ifft
Where the exponential applied (strength) is typically
between 1.2 and 1.7.  Larger strengths filter noise
better, but eliminate more good information, too.
Huge scalings, like 2.0 or 3.0, result in very geometric-
looking phase.
*/

void phase_filter(complex *buf,float strength)
{
	register int x,y;
	
/*We must adjust the scaling for two reasons:
	-The complex buffer is not normalized (strength-1)
	-We operate on the square of the amplitude (/2)
Hence,
	fft*=pow(fft.r^2+fft.i^2,adjScale);
is equivalent to
	amp=sqrt(fft.r^2+fft.i^2);fft/=amp;fft*=pow(amp,strength);
*/
	float adjStrength=(strength-1)/2;

/*fft buf*/
	fft2d((float *)buf,dMy,dMx);	
			
/*Manipulate power spectrum.*/
	for (y=0;y<dy;y++) 
	{
		register complex *fft=&buf[y*dx];
		for (x=0;x<dx;x++)
		{
			float mul=pow(fft->r*fft->r+fft->i*fft->i,adjStrength);
			fft->r*=mul;fft->i*=mul;
			fft++;
		}
	}
	
/*ifft buf*/
	ifft2d((float *)buf,dMy,dMx);
}

/************************************************************
image_filter: 
	Applies the goldstein phase filter across an entire
image, and writes the result to another image.

	The goldstein phase filter works best when applied
to little pieces of the image.  But processing the image as
a bunch of little pieces results in a segmented phase image.
Hence we do a bilinear weighting of 4 overlapping filters 
to "feather" the edges.
*/

void image_filter(FILE *in,meta_parameters *meta,
		FILE *out,float strength)
{
	int chunkX,chunkY,nChunkX,nChunkY;
	int i,x,y;
	float *inBuf,*outBuf,*weight, percent=5.0;
	complex **chunks,**last_chunks=NULL;
	
	/*Allocate polar to complex conversion array*/
#define NUM_PHASE 512
#define phase2cpx(ph) p2c[(int)((ph)*polarCvrt)&(NUM_PHASE-1)]
	complex *p2c;
	float polarCvrt=NUM_PHASE/(2*PI);
	p2c=(complex *)MALLOC(sizeof(complex)*NUM_PHASE);
	for (i=0;i<NUM_PHASE;i++)
	{
		float phase=i*2*PI/NUM_PHASE;
		p2c[i].r=cos(phase);
		p2c[i].i=sin(phase);
	}
	
	/*Allocate bilinear weighting array.*/
	weight=(float *)MALLOC(sizeof(float)*ox*oy);
	for (y=0;y<oy;y++)
		for (x=0;x<ox;x++)
			weight[y*ox+x]=(float)x/(ox-1)*(float)y/(oy-1);

	/*Allocate storage arrays.*/
	nChunkX=ns/ox-1;
	nChunkY=nl/oy-1;
	outBuf=inBuf=(float *)MALLOC(sizeof(float)*ns*dy);
#define newChunkArray(name) name=(complex **)MALLOC(sizeof(complex **)*nChunkX); \
			for (chunkX=0;chunkX<nChunkX;chunkX++) \
				name[chunkX]=(complex *)MALLOC(sizeof(complex)*dx*dy);
	newChunkArray(chunks);
	
	/*Loop across each chunk in file.
	printf("Filtering phase in %d x %d blocks...\n",dx,dy);
	printf("Output phase in %d x %d blocks...\n",ox,oy);*/
	for (chunkY=0;chunkY<nChunkY;chunkY++) 
	{

	if ((chunkY*100/nChunkY)>percent) {
	  printf("   Completed %3.0f percent\n",percent);
	  percent+=5.0;
	}

	/*Read next chunk of input.*/
		read_image(in,meta,inBuf,0,chunkY*ox,ns,dy);

	/*Convert polar image to complex chunk.*/
		for (chunkX=0;chunkX<nChunkX;chunkX++)
		{
			register float *in;
			register complex *out;
			for (y=0;y<dy;y++) 
			{
				in=&inBuf[y*ns+chunkX*ox];
				out=&chunks[chunkX][y*dx];
				for (x=0;x<dx;x++)
					*out++=phase2cpx(*in++);
			}
		}
		
	/*Filter each newly-read chunk.*/
		for (chunkX=0;chunkX<nChunkX;chunkX++)
			phase_filter(chunks[chunkX],strength);
	
	/*Blend and write out filtered data.*/
		blendData(chunks,last_chunks,weight,outBuf);
		for (y=0;y<oy;y++) {
			if (chunkY*oy+y<meta->general->line_count)
			  put_float_line(out,meta,chunkY*oy+y,&outBuf[y*ns]);
		}
		
	/*Swap chunks and last_chunks.*/
		{complex **tmp=last_chunks;last_chunks=chunks;chunks=tmp;}
		if (chunks==NULL) { newChunkArray(chunks); }
	}

	/*Write very last line of phase.*/
	blendData(NULL,last_chunks,weight,outBuf);
	for (y=0;y<oy;y++)
		if (chunkY*oy+y<meta->general->line_count)
		  put_float_line(out,meta,chunkY*oy+y,&outBuf[y*ns]);

}
