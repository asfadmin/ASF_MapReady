/******************************************************************************
*                                                                             *
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
*	Geophysical Institute			http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
/*******************************************************************************
NAME: coregister_fine

SYNOPSIS:
	coregister_fine [<grid resolution>] [-f] [-log <file>] [-quiet]
	                <file1> <file2> <control> <out>

DESCRIPTION:
	Coregister_fine is a program used to perform sub-pixel correlation 
	during the process of interferometry. It only works with complex images.

	The way coregister_fine works is it correlates (that is, match up) two 
	images at various points in the image.  The points it matches up are 
	arranged in a grid.  The number of points in this grid can be set by the 
	command line.

	Coregister_fine needs an gross estimate (to a single pixel) of the 
	offset between the two images; it gets this offset from its control 
	file.  For each grid point, coregister_fine computes the phase coherence 
	of several offsets near the gross offset, and searches for the highest 
	coherence offset.  It then refines this offset by performing a parabolic 
	interpolation on the nearby coherence values.

	Coregister_fine now double-checks its result by running the same 
	calculation with the images reversed.  If the two results agree, the 
	point is "good" and is output.  If the results disagree, the correlation 
	is "bad" and is not output.

	In addition to the offset, coregister_fine also calculates the SNR 
	(Signal-to-Noise Ratio) at each point.  This can be used as a measure of 
	the quality of the correlation.  Correlation points with a low SNR are 
	bad.

	If coregister_fine does not find many good points, you may not get an 
	interferogram when you interfere the two images. This can be the result 
	of several things: your interferometric baseline might be too big, and 
	there will never be an interferogram between the images; the initial 
	offset in the coregister_fine control file is incorrect, and 
	coregister_fine cannot find anything that looks like interferometric 
	phase.

	Increasing the number of grid points slows the program down, but 
	generates more output for fit_line or fit_plane, which then improves the 
	coherence.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     10/95        R. Fatland - Original Development
    1.1     10/95        M. Shindle - Revised & cleaned
			 T. Logan   - Port to Solaris, modified form
    1.2      4/96        M. Shindle - Apply ASF standards
    2.0      8/96        M. Shindle - Implement modifications from Rob Fatland.
    3.0	     2/97	 T. Logan   - Modified to work on float complex output
                                       patches from ASP code
    4.0      5/97	 O. Lawlor  - Modified to allow reverse correlation
					(to check the first correlation) and 
					grid resolution, and do a trilinear peak 
					interpolation.
    5.0      9/97	O. Lawlor   - Optimization-- replaced FFT with
					much faster phase coherence method.
    5.1      5/98	O. Lawlor   - Perform forward and reverse correlation,
                                        delete suspect points.
    5.2      6/00	M. Ayers    - Add Complex FFT matching option to offset
                                        estimation
    5.21     7/01	R. Gens     - Added logfile and quiet switch
    5.5     10/03       P. Denny    - Standardize commandline parsing & order
                                        Use meta v1.1 instead of DDR
    5.6      2/04       P. Denny    - Change license from GPL to ASF. Change
                                        name from fico to coregister_fine.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "ifm.h"

#define borderX 80	/*Distances from edge of image to start correlating.*/
#define borderY 80
#define minSNR 0.30	/*SNR's below this will be deleted.*/
#define maxDisp 1.8	/*Forward and reverse correlations which differ by more than this will be deleted.*/
#define VERSION 5.6

/*Read-only, informational globals:*/
int wid, len;			/*Width and length of source images.*/
int intOffsetX, intOffsetY;	/*Image offset estimates from coregister_coarse.*/
int srcSize=32, trgSize;
float xMEP=4.1,yMEP=6.1;	/*Maximum Error Pixel values.*/
complexFloat cZero;

/*Function declarations */
void usage(char *name);
void readControlFile(char *filename);
void initSourcePts(char *gridRes);

bool getNextPoint(int *x1,int *y1,int *x2,int *y2);
bool outOfBounds(int x1, int y1, int x2, int y2, int srcSize, int trgSize);

void getPeak(int x1,int y1,char *szImg1,int x2,int y2,char *szImg2,float *dx,float *dy, float *snr,int fft_flag);
void topOffPeak(float *peaks,int i, int j, int maxI, int maxJ,float *dx,float *dy);
float getPhaseCoherence(complexFloat *igram,int sizeX,int sizeY);
float getFFTCorrelation(complexFloat *igram,int sizeX,int sizeY);


/* Start of main progam */
int main(int argc, char *argv[])
{
	char szOut[MAXNAME], szCtrl[MAXNAME], szImg1[MAXNAME], szImg2[MAXNAME];
	int fft_flag=0;
	int x1, x2, y1, y2;
	int goodPoints,attemptedPoints;
	FILE *fp_output;
	char gridRes[256];
	meta_parameters *meta;

/* parse command line */
	logflag=quietflag=FALSE;
	while (currArg < (argc-4)) {
	   char *key = argv[currArg++];
	   if (strmatch(key,"-log")) {
	      CHECK_ARG(1);
	      strcpy(logFile,GET_ARG(1));
	      fLog = FOPEN(logFile, "a");
	      logflag=TRUE;
	   }
	   else if (strmatch(key,"-quiet")) {
	      quietflag=TRUE;
	   }
	   else if (strmatch(key,"-f")) {
	      fft_flag=1;
	      printf("   Using Complex FFT instead of coherence for matching\n");
	   }
	   else if (strmatch(key,"-g")) {
	      CHECK_ARG(1);
	      strcpy(gridRes,GET_ARG(1));
	   }
	   else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	create_name(szImg1, argv[currArg++], ".cpx");
	create_name(szImg2, argv[currArg++], ".cpx");
	strcpy(szCtrl,argv[currArg++]);
	strcpy(szOut,argv[currArg]);
	
	system("date");
	printf("Program: coregister_fine\n\n");

	if (!quietflag) printf("   coregister_fine is correlating '%s' to '%s'.\n",szImg2,szImg1);

	if (logflag) {
	  StartWatchLog(fLog);
          printLog("Program: coregister_fine\n\n");
	  if (fft_flag) 
	    printLog("   Using Complex FFT instead of coherence for matching\n");
	}

	readControlFile(szCtrl);

	/* calculate parameters */
	trgSize = 2*srcSize;

	/* determine size of input files */
	meta = meta_read(szImg1);
	wid = meta->general->sample_count;
	len = meta->general->line_count;
	meta_free(meta);

	/* initialize params before looping */
	cZero = Czero();

	/* create output file */
	fp_output=FOPEN(szOut,"w");

	initSourcePts(gridRes);

	/* Loop over grid, performing forward and backward correlations */
	goodPoints=attemptedPoints=0;
	while (getNextPoint(&x1,&y1,&x2,&y2))
	{
		float dx,dy,snr,dxFW,dyFW,snrFW,dxBW,dyBW,snrBW;
		attemptedPoints++;
		/*Check bounds...*/
		if (!(outOfBounds(x1, y1, x2, y2, srcSize, trgSize) ||
		      outOfBounds(x2, y2, x1, y1, srcSize, trgSize)))
		{
			/*...check forward correlation...*/
			getPeak(x1,y1,szImg1,x2,y2,szImg2,&dxFW,&dyFW,&snrFW,fft_flag);
			if (snrFW>minSNR)
			{
				/*...check backward correlation...*/
				getPeak(x2,y2,szImg2,x1,y1,szImg1,&dxBW,&dyBW,&snrBW,fft_flag);
				dxBW*=-1.0;dyBW*=-1.0;
				if ((snrBW>minSNR)&&
					(fabs(dxFW-dxBW)<maxDisp)&&
					(fabs(dyFW-dyBW)<maxDisp))
				{
					goodPoints++;
					dx=(dxFW+dxBW)/2;
					dy=(dyFW+dyBW)/2;
					snr=snrFW*snrBW;
		fprintf(fp_output,"%6d %6d %8.5f %8.5f %4.2f\n",x1,y1,x2+dx,y2+dy,snr);
		fflush(fp_output);
		if (!quietflag && (goodPoints <= 10 || !(goodPoints%100)))
			printf("\t%6d %6d %8.5f %8.5f %4.2f/%4.2f\n",x1,y1,dx,dy,snrFW,snrBW);
				}
			}
		}
	} /* end while(getNextPoint) */
	
	if (goodPoints<20)
	{
		sprintf(errbuf,"   *************** ERROR! ************\n"
			"   **    coregister_fine was only able to find %i points which\n"
			"   ** correlated the same backwards and forwards.  This\n"
			"   ** is not enough for a planar map!\n"
			"   **    Problems with coregister_fine can usually be traced back to coregister_fine's\n"
			"   ** control file, which MUST have a good estimate of the \n"
			"   ** single-pixel offset between the two images.\n"
			"   **      Exiting with error!\n",goodPoints);
		printErr(errbuf);
	} 
	else 
		printf("   coregister_fine attempted %d correlations, %d succeeded.\n\n",attemptedPoints,goodPoints);
		if (logflag) {
		  sprintf(logbuf,"   coregister_fine attempted %d correlations, %d succeeded.\n\n",attemptedPoints,goodPoints);
		  printLog(logbuf);
		}

	exit(EXIT_SUCCESS);
}

bool outOfBounds(int x1, int y1, int x2, int y2, int srcSize, int trgSize)
{
	if (x1 - srcSize/2 + 1 < 0) return TRUE;
	if (y1 - srcSize/2 + 1 < 0) return TRUE;
	if (x2 - trgSize/2 + 1 < 0) return TRUE;
	if (y2 - trgSize/2 + 1 < 0) return TRUE;
	if (x1 + srcSize/2  >= wid) return TRUE;
	if (y1 + srcSize/2  >= len) return TRUE;
	if (x2 + trgSize/2  >= wid) return TRUE;
	if (y2 + trgSize/2  >= len) return TRUE;
	return FALSE;
}

void readControlFile(char *filename)
{
	float ignored;
	/* read control file parameters 
			-----------------------------*/
	FILE *fp = FOPEN(filename,"r"); /* control file: */
	
	/* range offset (loc in img 2 = loc in img 1 - x offset) */
	fscanf(fp, "%d", &intOffsetX);
	/* azimuth offset (loc in img 2 = loc in img 1 - y offset) */
	fscanf(fp, "%d", &intOffsetY);
	fscanf(fp, "%d", &srcSize);
	if (srcSize<8) srcSize=32;
	fscanf(fp, "%f", &ignored);/*IGNORED oversample. This is now always 1.*/
	fscanf(fp, "%f", &xMEP);
	fscanf(fp, "%f", &yMEP);
	
	FCLOSE(fp);
}

int pointNo=0;
int gridResolution=20;
void initSourcePts(char *gridRes)
{
	/*Check to see if the last parameter contains a number, the grid resolution*/
	if (gridRes)
		gridResolution=atoi(gridRes);
	if (gridResolution<2) 
		gridResolution=20;
	if (!quietflag) printf("   Sampling rectangular grid, %ix%i resolution.\n",gridResolution,gridResolution);
}
bool getNextPoint(int *x1,int *y1,int *x2,int *y2)
{
	int unscaledX, unscaledY;
	unscaledX=pointNo%gridResolution;
	unscaledY=pointNo/gridResolution;
	*x1=unscaledX*(wid-2*borderX)/(gridResolution-1)+borderX;
	*y1=unscaledY*(len-2*borderY)/(gridResolution-1)+borderY;
	*x2=*x1-intOffsetX;
	*y2=*y1-intOffsetY;
	if (pointNo>=(gridResolution*gridResolution)) 
		return FALSE;
	pointNo++;
	return TRUE;
}
/*getPeak:
This function computes a correlation peak, with SNR, between
the two given images at the given points.
*/
void getPeak(int x1,int y1,char *szImg1,int x2,int y2,char *szImg2,float *peakX,float *peakY, float *snr,int fft_flag)
{
	static float *peaks;
	static complexFloat *s=NULL, *t, *product; /*Keep working arrays around between calls.*/
	int peakMaxX, peakMaxY, x,y,xOffset,yOffset,count;
	int xOffsetStart, yOffsetStart, xOffsetEnd, yOffsetEnd;
	float dx,dy,accel1 = (float)(trgSize/2 - srcSize/2);
	
	float peakMax, thisMax, peakSum;
	
	/* 
		* Calculate the limits of the time domain correlations...
		*   A coordinate in the target may be set to:
		*   (  (trgSize/2 - srcSize/2), (trgSize/2 - srcSize/2)  ).
		*   If this is the ulh element of the source chip, then
		*     the src chip coincides with the trg precisely, with no offset.
		*/
	xOffsetStart = (trgSize/2 - srcSize/2) - (int)(xMEP);
	xOffsetEnd = (trgSize/2 - srcSize/2) + (int)(xMEP);
	yOffsetStart = (trgSize/2 - srcSize/2) - (int)(yMEP);
	yOffsetEnd = (trgSize/2 - srcSize/2) + (int)(yMEP);
	
	/*Allocate working arrays if we haven't already done so.*/
	if (s==NULL)
	{
		s = (complexFloat *)(MALLOC(srcSize*srcSize*sizeof(complexFloat)));
		t = (complexFloat *)(MALLOC(trgSize*trgSize*sizeof(complexFloat)));
		product = (complexFloat *)(MALLOC(srcSize*srcSize*sizeof(complexFloat)));
		peaks=(float *)MALLOC(sizeof(float)*trgSize*trgSize);
	}
	
	/*At each grid point, read in a chunk of each image...*/
	readMatrix(szImg1,s,FLOAT_COMPLEX,srcSize,srcSize,
	    x1- srcSize/2+1, y1-srcSize/2+1, wid, len,0,0);
	readMatrix(szImg2,t,FLOAT_COMPLEX,trgSize,trgSize, 
	    x2 - trgSize/2 + 1, y2 - trgSize/2 + 1, wid, len,0,0);

	/*Take the complex conjugate of the source chunk (so we only have to do so once).*/
	for(y=0;y<srcSize;y++)
	{
		int srcIndex=y*srcSize;
		for(x=0;x<srcSize;x++)
			s[srcIndex++].imag*=-1;
	}

	/*Now compute the best possible offset between these two images,
		by checking the phase coherence at each possible offset.*/

	peakMax = peakSum = 0.0;
	peakMaxX=peakMaxY=count=0;
	for(yOffset=yOffsetStart;yOffset<=yOffsetEnd;yOffset++)
	{
		for(xOffset=xOffsetStart;xOffset<=xOffsetEnd;xOffset++)
		{
			/* Form an interferogram (multiply by complex conjugate at this offset between the images: */
			for(y=0;y<srcSize;y++)
			{
				int srcIndex=y*srcSize;
				int trgIndex=xOffset+(yOffset+y)*trgSize;
				for(x=0;x<srcSize;x++)
				{
					product[srcIndex] = Cmul(s[srcIndex], t[trgIndex]);
					srcIndex++,trgIndex++;
				}
			}
			
			/*Find the phase coherence for this interferogram*/
			if(fft_flag)
				thisMax=getFFTCorrelation(product,srcSize,srcSize);
			else
				thisMax=getPhaseCoherence(product,srcSize,srcSize);
			
			/*Possibly save this coherence value.*/
			if (thisMax>peakMax)
			{
				peakMax=thisMax;
				peakMaxX=xOffset;
				peakMaxY=yOffset;
			}
			peaks[yOffset*trgSize+xOffset]=thisMax;
			peakSum += thisMax;
			count++;
		}
	}

	/*Now that we've computed the best possible offset (peak)
		between the images, we top this peak off and return it.*/

	/* Optionally print a cool graphic of the actual peak:
	for(y=yOffsetStart;y<=yOffsetEnd;y++) 
	{
		for(x=xOffsetStart;x<=xOffsetEnd;x++)
		{
			char *table="      .,:;>})%#$";
			int off=y*(xOffsetEnd+1)+x;
			peaks[off]=peaks[off]/peakMax;
			printf("%c%c",table[(int)(peaks[off]*15.0)],table[(int)(peaks[off]*15.0)]);
		} 
		printf("\n");
	} */


	/* Calculate the SNR, with a much faster (but weaker) SNR calculation */
	*snr = peakMax / ((peakSum - peakMax) / (float)(count-1))-1.0;

	
	if ((peakMaxX>xOffsetStart)&&(peakMaxY>yOffsetStart)&&(peakMaxX<xOffsetEnd)&&(peakMaxY<yOffsetEnd))
		topOffPeak(peaks,peakMaxX,peakMaxY,trgSize,trgSize,&dx,&dy);
	else 
		dx=dy=0.0;
	
	*peakX=((float)(peakMaxX) + dx - accel1 );
	*peakY=((float)(peakMaxY) + dy - accel1 );
}

/*TopOffPeak:
Given an array of peak values, use trilinear interpolation to determine the exact (i.e. float) top.
This works by finding the peak of a parabola which goes though the highest point, and the three points
surrounding it.
*/
void topOffPeak(float *peaks,int i, int j, int maxI, int maxJ,float *dx,float *dy)
{
	int offset=j*maxI+i;
	float a,b,c,d;
	a=peaks[offset-1];
	b=peaks[offset];
	c=peaks[offset+1];
	d=4*((a+c)/2-b);
	if (d!=0)
		*dx=(a-c)/d;
	else *dx=0;
	a=peaks[offset-maxI];
	b=peaks[offset];
	c=peaks[offset+maxI];
	d=4*((a+c)/2-b);
	if (d!=0)
		*dy=(a-c)/d;
	else *dy=0;
}

void usage(char *name)
{	
 printf("\n"
	"USAGE:\n"
	"   %s [-g <grid res>] [<-f>] [-log <file>] [-quiet]\n"
	"                   <file1> <file2> <control> <out>\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   file1    First raw float complex base file name (.cpx & .meta)\n"
	"   file2    Second raw float complex base file name (.cpx & .meta)\n"
	"   control  Parameter file. This file is created by coregister_coarse.\n"
	"   out      Ouput ASCII file of offset points.\n");
 printf("\n"
	"OPTIONAL ARGUMETNS:\n"
	"   -g <grid_res>   Number of grid points per axis\n"
	"   -f              Use of complex FFT instead of coherence\n"
	"   -log <file>     Allows the output to be written to a log file\n"
	"   -quiet          Suppress the output to the essential\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Used to perform sub-pixel co-registration on two images, to form\n"
	"   an interferogram\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
