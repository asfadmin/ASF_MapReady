/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"offset_test"

#define ASF_USAGE_STRING \
"<file1> <file2> <out>\n"\
"\n"\
"Additional option: -help"

#define ASF_DESCRIPTION_STRING \
"Offset_test is verifying that two images have no offset relative to \n"\
"each other. This is achieved by matching small image chips defined \n"\
"on a regular grid. If no offset can be determined between the two \n"\
"images, it is assumed that changes in the implementation of a \n"\
"particular tool did not affect the geometry or geolocation of the \n"\
"image."

#define ASF_INPUT_STRING \
"<file1> <file2>\n"\
"The two images to be compared."

#define ASF_OUTPUT_STRING \
"<out>\n"\
"Text file reporting the individual correlations."

#define ASF_OPTIONS_STRING \
"None."

#define ASF_EXAMPLES_STRING \
"offset_test test_old test_new test.out"

#define ASF_LIMITATIONS_STRING \
"None known."

#define ASF_SEE_ALSO_STRING \
"image_stats, detect_cr"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"None."

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"
#include "fft.h"
#include "fft2d.h"
#include "ifm.h"
#include "offset_test.h"

/*Read-only, informational globals:*/
int lines, samples;		/* Lines and samples of source images. */
int srcSize=64, trgSize=64;
int pointNo=0, gridResolution=20;
int chipX, chipY,            /*Chip location (top left corner) in second image*/
    chipDX,chipDY;           /*Chip size in second image.*/
int searchX,searchY;         /*Maximum distance to search for peak*/
float xMEP=4.1,yMEP=6.1;     /*Maximum Error Pixel values.*/
complexFloat cZero;

/*Function declarations */
bool getNextPoint(int *x1,int *y1,int *x2,int *y2);

void topOffPeak(float *peaks, int i, int j, int maxI, float *di, float *dj);
void topOffPeakCpx(float *peaks,int i, int j, int maxI, int maxJ,
		   float *dx,float *dy);

bool findPeak(int x1,int y1, char *szImg1, int x2, int y2, char *szImg2,
	      float *peakX, float *peakY, float *snr);
float getFFTCorrelation(complexFloat *igram,int sizeX,int sizeY);
void getPeak(int x1,int y1,char *szImg1,int x2,int y2,char *szImg2,
	     float *peakX,float *peakY, float *snr);
bool outOfBounds(int x1, int y1, int srcSize);

/* usage - enter here on command-line usage error*/
void usage(void)
{
  printf("\n"
         "USAGE:\n"
         ASF_NAME_STRING
         " "
         ASF_USAGE_STRING
         "\n\n");
  exit (EXIT_FAILURE);
}

/* help_page - go here when the -help option is specified */
void help_page()
{
  if(system("echo '"
            "\n\n\n"
            "Tool name:\n" ASF_NAME_STRING "\n\n\n"
            "Usage:\n" ASF_USAGE_STRING "\n\n\n"
            "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
            "Input:\n" ASF_INPUT_STRING "\n\n\n"
            "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
            "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
            "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
            "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
            "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
            "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
            "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
            "' | less") != -1)
    exit(EXIT_SUCCESS);
  
  else if(system("echo '"
                 "\n\n\n"
                 "Tool name:\n" ASF_NAME_STRING "\n\n\n"
                 "Usage:\n" ASF_USAGE_STRING "\n\n\n"
                 "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
                 "Input:\n" ASF_INPUT_STRING "\n\n\n"
                 "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
                 "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
                 "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
                 "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
                 "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
                 "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
                 "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n"
                 "' | more") != -1)
    exit(EXIT_SUCCESS);
  
  else
    printf("\n\n\n"
           "Tool name:\n" ASF_NAME_STRING "\n\n\n"
           "Usage:\n" ASF_USAGE_STRING "\n\n\n"
           "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
           "Input:\n" ASF_INPUT_STRING "\n\n\n"
           "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
           "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
           "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
           "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
           "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
           "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n"
           "Program history:\n" ASF_PROGRAM_HISTORY_STRING "\n\n\n");
  exit(EXIT_SUCCESS);
}


/* Start of main progam */
int main(int argc, char *argv[])
{
  char szOut[255], szImg1[255], szImg2[255];
  int x1, x2, y1, y2, ii;
  int goodPoints=0, attemptedPoints=0;
  FILE *fp_output;
  meta_parameters *masterMeta, *slaveMeta;
  int ampFlag=TRUE;
  
  flag_indices_t flags[NUM_FLAGS];

  /* Set all flags to 'not set' */
  for (ii=0; ii<NUM_FLAGS; ii++) {
    flags[ii] = FLAG_NOT_SET;
  }
  
/**********************BEGIN COMMAND LINE PARSING STUFF**********************/
  /* Check to see if any options were provided */
  if (checkForOption("-help", argc, argv) != -1) /* Most important */
    help_page();

  /* Make sure to set log & quiet flags (for use in our libraries) */
  logflag = (flags[f_LOG]!=FLAG_NOT_SET) ? TRUE : FALSE;
  quietflag = (flags[f_QUIET]!=FLAG_NOT_SET) ? TRUE : FALSE;

  if (flags[f_QUIET] == FLAG_NOT_SET)
    /* display splash screen if not quiet */
    print_splash_screen(argc, argv);
  if (flags[f_LOG] != FLAG_NOT_SET)
    strcpy(logFile, argv[flags[f_LOG] + 1]);
  else
    /* default behavior: log to tmp<pid>.log */
    sprintf(logFile, "tmp%i.log", (int)getpid());
  fLog = FOPEN(logFile, "a");

  /* Fetch required arguments */
  strcpy(szImg1,argv[argc - 3]);
  strcpy(szImg2,argv[argc - 2]);
  strcpy(szOut, argv[argc - 1]);
/***********************END COMMAND LINE PARSING STUFF***********************/

  /* Read metadata */
  masterMeta = meta_read(szImg1);
  slaveMeta = meta_read(szImg2);
  if (masterMeta->general->line_count != slaveMeta->general->line_count ||
      masterMeta->general->sample_count != slaveMeta->general->sample_count) {
    printf("\n   WARNING: Input images have different dimension!\n\n");
  }
  else if (masterMeta->general->data_type != slaveMeta->general->data_type) {
    printf("\n   ERROR: Input image have different data type!\n\n");
    exit(0);
  }
  else if (masterMeta->general->data_type > 5 &&
	   masterMeta->general->data_type != COMPLEX_REAL32) {
    printf("\n   ERROR: Cannot compare raw images for offsets!\n\n");
    exit(0);
  }
  lines = masterMeta->general->line_count;
  samples = masterMeta->general->sample_count;
  if (masterMeta->general->data_type == COMPLEX_REAL32) {
    srcSize = 32;
    ampFlag = FALSE;
    cZero = Czero();
  }

  /* Create output file */
  fp_output=FOPEN(szOut, "w");
  
  /* Loop over grid, performing forward and backward correlations */
  while (getNextPoint(&x1,&y1,&x2,&y2))
      {
	float dx, dy, snr, dxFW, dyFW, snrFW, dxBW, dyBW, snrBW;
	attemptedPoints++;

        /* Check bounds */
	if (!(outOfBounds(x1, y1, srcSize)))
	{
	  /* ...check forward correlation... */
	  if (ampFlag) {
	    if (!(findPeak(x1,y1,szImg1,x2,y2,szImg2,&dxFW,&dyFW,&snrFW))) {
	      attemptedPoints--;
	      continue; /* next point if chip in complete background fill */
	    }
	  }
	  else {
	    getPeak(x1,y1,szImg1,x2,y2,szImg2,&dxFW,&dyFW,&snrFW);
	  }
	  if ((!ampFlag && snrFW>minSNR) || (ampFlag)) {
	    /* ...check backward correlation... */
	    if (ampFlag) {
	      if (!(findPeak(x2,y2,szImg2,x1,y1,szImg1,&dxBW,&dyBW,&snrBW))) {
		attemptedPoints--;
                continue; /* next point if chip in complete background fill */
	    printf("dxFW: %.2f, dyFW: %.2f\n", dxFW, dyFW);
	      }
	    }
	    else {
	      getPeak(x2,y2,szImg2,x1,y1,szImg1,&dxBW,&dyBW,&snrBW);
	    }

	    dxBW*=-1.0;dyBW*=-1.0;
	    if (((!ampFlag && snrFW>minSNR) || (ampFlag)) &&
		(fabs(dxFW-dxBW) < maxDisp) &&
		(fabs(dyFW-dyBW) < maxDisp))
	      {
		dx = (dxFW+dxBW)/2;
		dy = (dyFW+dyBW)/2;
		snr = snrFW*snrBW;
		if (dx < maxDxDy && dy < maxDxDy) goodPoints++;
	      }
	  }
	  fprintf(fp_output,"%6d %6d %8.5f %8.5f %4.2f\n",
		  x1, y1, x2+dx, y2+dy, snr);
	  fflush(fp_output);
	}
      }
  if (goodPoints < attemptedPoints)
    printf("\n   WARNING: %i out of %i points moved by "
	   "more than one pixel!\n\n", 
	   (attemptedPoints-goodPoints), attemptedPoints);
  else 
    printf("\n   There is no difference between the images\n\n");

  FCLOSE(fp_output);
  FREE(masterMeta);
  FREE(slaveMeta);  

  return(0);
}


bool outOfBounds(int x1, int y1, int srcSize)
{
  if (x1 - srcSize/2 + 1 < 0) return TRUE;
  if (y1 - srcSize/2 + 1 < 0) return TRUE;
  if (x1 + srcSize/2  >= samples) return TRUE;
  if (y1 + srcSize/2  >= lines) return TRUE;
  return FALSE;
}


bool getNextPoint(int *x1,int *y1,int *x2,int *y2)
{
  int unscaledX, unscaledY;
  unscaledX=pointNo%gridResolution;
  unscaledY=pointNo/gridResolution;
  *x1=unscaledX*(samples-2*borderX)/(gridResolution-1)+borderX;
  *y1=unscaledY*(lines-2*borderY)/(gridResolution-1)+borderY;
  *x2=*x1;
  *y2=*y1;
  if (pointNo>=(gridResolution*gridResolution)) 
    return FALSE;
  pointNo++;
  return TRUE;
}


/*FindPeak: 
  This function computes a correlation peak, with doubt level, between
  the two amplitude images at the given points.
*/
bool findPeak(int x1, int y1, char *szImg1, int x2, int y2, char *szImg2,
	      float *peakX, float *peakY, float *doubt)
{
  meta_parameters *meta;
  static float *peaks, *s=NULL, *t, *product;
  int x, y, srcIndex;
  int mX,mY; /* Invariant: 2^mX=ns; 2^mY=nl. */
#define modX(x) ((x+srcSize)%srcSize)  /* Return x, wrapped to [0..srcSize-1] */
#define modY(y)	((y+srcSize)%srcSize)  /* Return y, wrapped to [0..srcSize-1] */
  
  float aveChip=0;
  float scaleFact=1.0/(srcSize*srcSize);
  float sum=0, stdDev;
  
  meta = meta_read(szImg1); 

  /* Allocate working arrays */
  if (s == NULL) {
    s = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
    t = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
    product = (float *)(MALLOC(srcSize*srcSize*sizeof(float)));
    peaks=(float *)MALLOC(sizeof(float)*srcSize*srcSize);
  }  

  /* At each grid point, read in a chunk of each image...*/
  readSubset(szImg1, srcSize, srcSize, x1-srcSize/2+1, y1-srcSize/2+1, s);
  readSubset(szImg2, srcSize, srcSize, x2-srcSize/2+1, y2-srcSize/2+1, t);
  
  /* Compute average brightness of chip */
  for(y=0;y<srcSize;y++)
    {
      srcIndex=y*srcSize;
      for(x=0;x<srcSize;x++) {
	sum += s[x+srcIndex];
      }
    }
  aveChip = sum/(float)srcSize*srcSize;
  stdDev = sqrt(sum/(srcSize*srcSize-1));
  if (stdDev < 0.1) return FALSE;

  /* Subtract average brightness from chip 2 */
  for(y=0;y<srcSize;y++)
    {
      srcIndex=y*srcSize;
      for(x=0;x<srcSize;x++)
	t[x+srcIndex]=(t[x+srcIndex]-aveChip)*scaleFact;
    }
  
  /* Add average brightness to chip 1 */
  for(y=0;y<srcSize;y++)
    {
      srcIndex=y*srcSize;
      for(x=0;x<srcSize;x++)
	s[x+srcIndex]=s[x+srcIndex]-aveChip;
    }
  
  /* Do the FFT and back */
  mX = (int)(log(srcSize)/log(2.0)+0.5);
  mY = (int)(log(srcSize)/log(2.0)+0.5);

  fft2dInit(mY,mX); /* Initialization */

  rfft2d(s,mY,mX); /* FFT chip 1 */
  rfft2d(t,mY,mX); /* FFT chip 2 */

  for(y=0;y<srcSize;y++) /* Conjugate chip 2 */    {
      srcIndex=y*srcSize;
      if (y<2) x=1; else x=0;
      for (;x<srcSize/2;x++)
	t[srcIndex+2*x+1]*=-1.0;
    }	
  
  rspect2dprod(s,t,product,srcSize,srcSize); /* Complex product */
  
  for (y=0;y<4;y++) /* Zero out the low frequencies of the correlation chip */
    {
      srcIndex=y*srcSize;
      for (x=0;x<8;x++) product[srcIndex+x]=0;
      srcIndex=srcSize*(srcSize-1-y);
      for (x=0;x<8;x++) product[srcIndex+x]=0;
    }
  
  rifft2d(product,mY,mX); /* Inverse FFT */
  
  /* Set up search chip size. */
  chipDX = srcSize*3/4;
  chipDY = srcSize*3/4;
  chipX = srcSize/8;
  chipY = srcSize/8;
  searchX = srcSize*3/8;
  searchY = srcSize*3/8;

  /* Find peak */
  float biggestNearby=-100000000000.0;
  int delX=15+chipX/8,delY=15+chipY/8;
  int closeX=5+chipX/16,closeY=5+chipY/16;
  
  /* Search for the peak with the highest correlation strength */
  int bestX,bestY;
  float bestMatch=-100000000000.0;
  float bestLocX=delX,bestLocY=delY;

  for (y=chipY-searchY; y<chipY+searchY; y++)
    for (x=chipX-searchX; x<chipX+searchX; x++)
      {
	int index = srcSize * modY(y) + modX(x);
	if (bestMatch < product[index])
	  {
	    bestMatch = product[index];
	    bestX = x;
	    bestY = y;
	  }
      }
  topOffPeak(product,bestX,bestY,srcSize,&bestLocX,&bestLocY);
  
  /* Compute the doubt in our offset guess, by
     finding the largest of several pixels around our guess. */
  for (y=-delY; y<=delY; y++)
    for (x=-delX; x<=delX; x++)
      if ((abs(y)>closeY) || (abs(x)>closeX))
	{
	  float cor = product[modY(bestY+y)*srcSize+modX(bestX+x)];
	  if (biggestNearby < cor)
	    biggestNearby = cor;
	}

  *doubt=biggestNearby/bestMatch;
  if (*doubt<0) *doubt = 0;
  
  /* Clean up */
  meta_free(meta);

  /* Output our guess. */
  *peakX = bestLocX;
  *peakY = bestLocY;

  return TRUE;
}


/*getPeak:
  This function computes a correlation peak, with SNR, between
  the two given images at the given points.
*/
void getPeak(int x1,int y1,char *szImg1,int x2,int y2,char *szImg2,
	     float *peakX,float *peakY, float *snr)
{
  static float *peaks;
  static complexFloat *s=NULL, *t, *product;
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
	     x1- srcSize/2+1, y1-srcSize/2+1, samples, lines,0,0);
  readMatrix(szImg2,t,FLOAT_COMPLEX,trgSize,trgSize, 
	     x2 - trgSize/2+1, y2-trgSize/2+1, samples, lines,0,0);
  
  /*Take the complex conjugate of the source chunk */
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
	  /* Form an interferogram (multiply by complex conjugate 1
	     at this offset between the images: */
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
	  thisMax=getFFTCorrelation(product,srcSize,srcSize);
	  
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
  
  /* Calculate the SNR, with a much faster (but weaker) SNR calculation */
  *snr = peakMax / ((peakSum - peakMax) / (float)(count-1))-1.0;
  
  
  if ((peakMaxX>xOffsetStart)&&(peakMaxY>yOffsetStart)&&
      (peakMaxX<xOffsetEnd)&&(peakMaxY<yOffsetEnd))
    topOffPeakCpx(peaks,peakMaxX,peakMaxY,trgSize,trgSize,&dx,&dy);
  else 
    dx=dy=0.0;
  
  *peakX=((float)(peakMaxX) + dx - accel1 );
  *peakY=((float)(peakMaxY) + dy - accel1 );
}


/* TopOffPeak:
   Given an array of peak values, use trilinear interpolation to determine the 
   exact (i.e. float) top. This works by finding the peak of a parabola which 
   goes though the highest point, and the three points surrounding it.
*/
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


void topOffPeakCpx(float *peaks,int i, int j, int maxI, int maxJ,
		   float *dx,float *dy)
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
