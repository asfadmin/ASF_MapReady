/*****************************************************************************
NAME: fftMatch

SYNOPSIS: fftMatch [-m <matchFile> [-c <corrImg.ext>]] [-log <file>] [-quiet]
			<img1.ext> <img2.ext>

DESCRIPTION:
	
	fftMatch lines up two images, to slightly better the
	single-pixel precision.  It will work with images of any
	size, but is most efficient when the image dimensions are
	near a power of 2.  The images need not be square.

	The lining-up is performed using image correlation via
	the much-vaunted Fast Fourier Transform (FFT).  The working
	space size is rounded up to the nearest power of 2 (for the FFT).
	The first image is read in completely, and a chip of the second
	image is also read in.  The average brightness of the chip is
	calculated and subtracted from both images.  The images are then
	FFT'd to frequency space, the FFT'd chip is conjugated, and the
	two images are multiplied together.  The product is inverse-FFT'd,
	and the resulting correlation image shows a brightness peak
	where the two images line up best.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
	asf_fft.a: FFT Library

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/98   O. Lawlor    Needed better co-registration tool for
				 interferometry.
    1.5     2/02   P. Denny     Redo commandline parsing

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:	
	The images must have at least 75% overlap.
        fftMatch will occasionally fail to find the
        correct offset (especially with noisy images).

*****************************************************************************/
/****************************************************************************
*								            *
*   fftMatch attempts to estimate the offset between two		    *
*	     images using the Fast Fourier Transform.			    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include <math.h>
#include "fft.h"
#include "fft2d.h"
#include "ddr.h"

#define VERSION 1.5
#define MIN(a,b) ( ((a)<(b)) ? (a) : (b) )

int nl,ns;
int mX,mY;                   /*Invariant: 2^mX=ns; 2^mY=nl.*/
#define modX(x) ((x+ns)%ns)  /*Return x, wrapped to [0..ns-1]*/
#define modY(y) ((y+nl)%nl)  /*Return y, wrapped to [0..nl-1]*/
int chipX, chipY,            /*Chip location (top left corner) in second image*/
    chipDX,chipDY;           /*Chip size in second image.*/
int searchX,searchY;         /*Maximum distance to search for peak*/

/**** PROTOTYPES ****/
void findPeak(float *corrImage,float *dx,float *dy,float *doubt);
void topOffPeak(float *peaks,int i,int j,int maxI,float *dx,float *dy);
void las_readImg(FILE *in,const struct DDR *ddr, 
	int startX,int startY,int delX,int delY,
	float add,float *sum, float *dest);
/*void dump(float *img,char *name);*/
void las_fftProd(FILE *in1F,const struct DDR *inDDR1,
		FILE *in2F,const struct DDR *inDDR2,float **corrImage);
void status(char *message);
void usage(char *name);


int main(int argc,char **argv)
{
	int x,y;
	float bestLocX,bestLocY,doubt;
	float *corrImage=NULL;
	char *corrFile=NULL,*descFile=NULL,*inFile1,*inFile2;
	FILE *corrF=NULL,*descF,*in1F,*in2F;
	struct DDR inDDR1,inDDR2,outDDR;
	extern int optind;            /* argv index of the next argument */
	extern char *optarg;          /* current argv[] */
	int c;                        /* option letter from getopt() */
	extern FILE *fLog;
	extern int logflag,quietflag;

	fLog=NULL;
	logflag=quietflag=0;

	/* process command line */
	/* q has a : because we want to have it be -quiet (: = uiet) */
	while ((c=getopt(argc,argv,"m:c:l:q:")) != EOF)
	{
	   switch (c) {
	     case 'm':
		descFile=optarg;
		break;
	     case 'c':
		if (descFile)
		    corrFile=optarg;
		else
		{
		    fprintf(stderr,"\nMust have -m option before -c option for -c to work!!\n");
		    if (fLog) FCLOSE(fLog);
		    usage(argv[0]);
		}
		break;			
	     case 'l':/* -log <filename>, get logfile; this is sorta hacked */
		if (0==strncmp(optarg,"og",2)) 
		{
			sscanf(argv[optind++], "%s", logFile);
			logflag=1;
			fLog = FOPEN(logFile, "a");
		}
		else usage(argv[0]);
		break;
	     case 'q':/* -quiet flag; this is also hacked */
		if (0==strncmp(optarg,"uiet",4)) 
			quietflag=1;
		else usage(argv[0]);
		break;
	     default:
		if (fLog) FCLOSE(fLog);
 		usage(argv[0]);
		break;	
	   }
	}

	if ((argc-optind) != 2)
	{
		if ((argc-optind) > 2) printf("\nToo many inputs.\n");
		if ((argc-optind) < 2) printf("\nToo few inputs.\n");
		if (fLog) FCLOSE(fLog);
		usage(argv[0]);
	}
	else
	{
		inFile1=argv[optind];
		inFile2=argv[optind+1];
	}

/*	printf("fftMatch'ing '%s' and '%s'.\n",inFile1,inFile2);
Open input files.*/
	in1F=fopenImage(inFile1,"rb");
	in2F=fopenImage(inFile2,"rb");
	c_getddr(inFile1,&inDDR1);
	c_getddr(inFile2,&inDDR2);
	
/*Round to find nearest power of 2 for FFT size.*/
	mX=(int)(log((float)(inDDR1.ns))/log(2.0)+0.5);
	mY=(int)(log((float)(inDDR1.nl))/log(2.0)+0.5);

	/* Keep size of fft's reasonable */	
	if (mX > 13) mX = 13;
	if (mY > 15) mY = 15;
	ns=1<<mX;
	nl=1<<mY;
	fft2dInit(mY, mX);
	
	if (!quietflag) printf("   FFT Size: %d samples by %d lines\n",ns,nl);
	if (!quietflag && (ns*nl*2*sizeof(float)>20*1024*1024))
		printf("   WARNING: These images will take %d megabytes of memory to match.\n"
		"   You may want to try smaller images.\n",ns*nl*2*sizeof(float)/(1024*1024));
	
/*Set up search chip size.*/
	chipDX=MIN(inDDR2.ns,ns)*3/4;
	chipDY=MIN(inDDR2.nl,nl)*3/4;
	chipX=MIN(inDDR2.ns,ns)/8;
	chipY=MIN(inDDR2.nl,nl)/8;
	searchX=MIN(inDDR2.ns,ns)*3/8;
	searchY=MIN(inDDR2.nl,nl)*3/8;
	
	if (!quietflag) printf("\tChip at %dx%d, size=%dx%d\n",chipX,chipY,chipDX,chipDY);
	
/*Optionally open the correlation image file.*/
	if (corrFile)
	{
		outDDR=inDDR1;
		outDDR.dtype=4;
		outDDR.nl=2*searchY;
		outDDR.ns=2*searchX;
		corrF=fopenImage(corrFile,"w");
		c_putddr(corrFile,&outDDR);
	}
	
/*Perform the correlation.*/
	las_fftProd(in1F,&inDDR1,in2F,&inDDR2,&corrImage);

/*Optionally write out correlation image.*/
	if (corrFile)
	{
		int outY=0;
		float *outBuf=(float*)MALLOC(sizeof(float)*outDDR.ns);
		for (y=chipY-searchY;y<chipY+searchY;y++)
		{
			int index=ns*modY(y);
			int outX=0;
			for (x=chipX-searchX;x<chipX+searchX;x++)
				outBuf[outX++]=corrImage[index+modX(x)];
			putFloatLine(corrF,&outDDR,outY++,outBuf);
		}
		FREE(outBuf);
	}

/*Search correlation image for a peak.*/
	findPeak(corrImage,&bestLocX,&bestLocY,&doubt);

	printf("   Offset slave image: dx = %f, dy = %f\n"
		"   Certainty: %f%%\n",bestLocX,bestLocY,100*(1-doubt));
	if (logflag) {
	  sprintf(logbuf,"   Offset slave image: dx = %f, dy = %f\n"
			"   Certainty: %f%%\n",bestLocX,bestLocY,100*(1-doubt));
	  printLog(logbuf);
	}
	if (descFile)
	{
		descF=FOPEN(descFile,"w");
		fprintf(descF,"%f\t%f\t%f\n",bestLocX,bestLocY,100*(1-doubt));
	}
	return (0);
}


void status(char *message)
{
	printf("\tDoing: %s\n",message);
}


/*FindPeak: search correlation image for coherence peak.*/
void findPeak(float *corrImage,float *dx,float *dy,float *doubt)
{
	float biggestNearby=-100000000000.0;
	int delX=15+chipX/8,delY=15+chipY/8;
	int closeX=5+chipX/16,closeY=5+chipY/16;
	
/*Search for the peak with the highest correlation strength*/
	int x,y,bestX,bestY;
	float bestMatch=-100000000000.0;
	float bestLocX=delX,bestLocY=delY;
	if (!quietflag) printf("   Searching for Peak (largest offset=%d lines & %d samples)\n",
		searchY,searchX);
	for (y=chipY-searchY;y<chipY+searchY;y++)
		for (x=chipX-searchX;x<chipX+searchX;x++)
		{
			int index=ns*modY(y)+modX(x);
			if (bestMatch<corrImage[index])
			{
				bestMatch=corrImage[index];
				bestX=x;bestY=y;
			}
		}
	topOffPeak(corrImage,bestX,bestY,ns,&bestLocX,&bestLocY);
	
/*Compute the doubt in our offset guess, by
finding the largest of several pixels around our guess.*/
	for (y=-delY;y<=delY;y++)
		for (x=-delX;x<=delX;x++)
			if ((abs(y)>closeY)||(abs(x)>closeX))
			{
				float cor=corrImage[modY(bestY+y)*ns+modX(bestX+x)];
				if (biggestNearby<cor)
					biggestNearby=cor;
			}
	*doubt=biggestNearby/bestMatch;
	if (*doubt<0) *doubt=0;
	
/*Output our guess:*/
	bestLocX-=chipX;
	bestLocY-=chipY;
	*dx=bestLocX;
	*dy=bestLocY;
}


/*Perform parabolic interpolation on peak.*/
void topOffPeak(float *peaks,int i,int j,int maxI,float *di,float *dj)
{
        float a,b,c,d;
        a=peaks[modY(j)*maxI+modX(i-1)];
        b=peaks[modY(j)*maxI+modX(i)];
        c=peaks[modY(j)*maxI+modX(i+1)];
        d=4*((a+c)/2-b);
        if (d!=0)
                *di=i+(a-c)/d;
        else *di=i;
        a=peaks[modY(j-1)*maxI+modX(i)];
        b=peaks[modY(j)*maxI+modX(i)];
        c=peaks[modY(j+1)*maxI+modX(i)];
        d=4*((a+c)/2-b);
        if (d!=0)
                *dj=j+(a-c)/d;
        else *dj=j;
}


/* las_readImg: reads the image file given by in & ddr
into the (nl x ns) float array dest.  Reads a total of
(delY x delX) pixels into topleft corner of dest, starting
at (startY , startX) in the input file.
*/
void las_readImg(FILE *in,const struct DDR *ddr, 
	int startX,int startY,int delX,int delY,
	float add,float *sum, float *dest)
{
	float *inBuf=(float *)MALLOC(sizeof(float)*(ddr->ns));
	register int x,y,l;
	double tempSum=0;
/*Read portion of input image into topleft of dest array.*/
	for (y=0;y<delY;y++)
	{
		l=ns*y;
		getFloatLine(in,ddr,startY+y,inBuf);
		if (sum==NULL)
			for (x=0;x<delX;x++)
				dest[l+x]=inBuf[startX+x]+add;
		else
			for (x=0;x<delX;x++)
			{
				tempSum+=inBuf[startX+x];
				dest[l+x]=inBuf[startX+x]+add;
			}
		for (x=delX;x<ns;x++)
			dest[l+x]=0.0; /*Fill rest of line with zeros.*/
	}
/*Fill remainder of array (bottom portion) with zero lines.*/
	for (y=delY;y<nl;y++)
	{
		l=ns*y;
		for (x=0;x<ns;x++)
			dest[l+x]=0.0; /*Fill rest of in2 with zeros.*/
	}
	if (sum!=NULL) 
		*sum=(float)tempSum;
	FREE(inBuf);
}


/*Dump: for debugging.
void dump(float *img,char *name)
{
	char bname[100];
	FILE *f;
	int y;
	strcat(strcpy(bname,name),".img");
	f=fopenImage(bname,"wb");
	for (y=0;y<nl;y++)
		fwrite(&img[y*ns],sizeof(float),ns,f);
	fclose(f);
}
*/

/* las_fftProd: reads both given files, and correlates them into the 
created outReal (nl x ns) float array.*/
void las_fftProd(FILE *in1F,const struct DDR *inDDR1,
		FILE *in2F,const struct DDR *inDDR2,float *outReal[])
{
	float scaleFact=1.0/(chipDX*chipDY);
	register float *in1,*in2,*out;
	register int x,y,l;
	float aveChip;
	
	in1=(float *)MALLOC(sizeof(float)*ns*nl);
	in2=(float *)MALLOC(sizeof(float)*ns*nl);
	out=in2;
	*outReal=in2;
	
/*Read image 2 (chip)*/
	if (!quietflag) status("Reading Image 2");
	las_readImg(in2F,inDDR2,
		chipX,chipY,chipDX,chipDY,
		0.0,&aveChip,in2);
/*	dump(in2,"in2_in");*/
/*Compute average brightness of chip.*/
	aveChip/=-(float)chipDY*chipDX;
	
/*Subtract this average off of image 2(chip):*/
	for (y=0;y<chipDY;y++)
	{
		l=ns*y;
		for (x=0;x<chipDX;x++)
			in2[l+x]=(in2[l+x]+aveChip)*scaleFact;
	}
/*	dump(in2,"in2_scale");*/
/*FFT image 2 */
	if (!quietflag) status("FFT Image 2");
	rfft2d(in2,mY,mX);
/*	dump(in2,"in2_fft");*/

/*Read image 1: Much easier, now that we know the average brightness. */
	if (!quietflag) status("Reading Image 1");
	las_readImg(in1F,inDDR1,
		0,0,MIN(inDDR1->ns,ns),MIN(inDDR1->nl,nl),
		aveChip,NULL,in1);
/*	dump(in1,"in1_in");*/

/*FFT Image 1 */
	if (!quietflag) status("FFT Image 1");
	rfft2d(in1,mY,mX);
/*	dump(in1,"in1_fft");*/

/*Conjugate in2.*/
	if (!quietflag) status("Conjugate Image 2");
	for (y=0;y<nl;y++)
	{
		l=ns*y;
		if (y<2) x=1; else x=0;
		for (;x<ns/2;x++)
			in2[l+2*x+1]*=-1.0;
	}
/*Take complex product of in1 and in2 into out.*/
	if (!quietflag) status("Complex Product");
	rspect2dprod(in1,in2,out,nl,ns);
/*	dump(in2,"prod_fft");*/

/*Zero out the low frequencies of the correlation image.*/
	if (!quietflag) status("Zero low frequencies.");
	for (y=0;y<4;y++)
	{
		l=ns*y;
		for (x=0;x<8;x++)
			out[l+x]=0;
		l=ns*(nl-1-y);
		for (x=0;x<8;x++)
			out[l+x]=0;
	}
/*	dump(in2,"prod_zero");*/

/*Inverse-fft the product*/
	if (!quietflag) status("I-FFT");
	rifft2d(out,mY,mX);
/*	dump(in2,"prod_time");*/

	FREE(in1);/*Note: in2 shouldn't be freed, because we return it.*/
}


void usage(char *name)
{
 printf("\nUSAGE:\n"
	"   %s [-m <matchFile> [-c <corrImg.ext>]] [-log <file>] [-quiet]\n"
	"            <img1.ext> <img2.ext>\n"
	"\nOPTIONS:\n"
	"   -m <matchFile>:   an output file, listing how much image 2\n"
	"                       should be shifted to line up with image 1:\n"
	"                       <shift x (pixels)>   <shift y (pixels)>\n"
	"                       <confidence (percent)>\n"
	"   -c <corrImg.ext>: an image of the correlation between the two\n"
	"                       source images.  This is useful for debugging.\n"
	"   -log <file>:      allows the output to be written to a log file\n"
	"   -quiet:           suppresses the output to the essentials\n"
	"\nINPUTS:\n"
	"   <img1.ext>:  any single-banded LAS image.\n"
	"   <img2.ext>:  an image to line up with the first.\n"
	"                  image 2 should not be bigger than image 1\n"
	"\nDESCRIPTION:\n"
	"   fftMatch lines up two images, to slightly better the\n"
	"   single-pixel precision.  It will work with images of any\n"
	"   size, but is most efficient when the image dimensions are\n"
	"   near a power of 2.  The images need not be square.\n"
	"\nVersion %.2f, ASF SAR TOOLS\n\n",name,VERSION);
	exit(1);
}
