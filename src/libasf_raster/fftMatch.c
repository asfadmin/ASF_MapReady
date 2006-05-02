#include "asf.h"
#include <math.h>
#include "fft.h"
#include "fft2d.h"

#define VERSION 1.5
#define MINI(a,b) ( ((a)<(b)) ? (a) : (b) )

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
void readImage(FILE *in,meta_parameters *meta, 
	       int startX,int startY,int delX,int delY,
	       float add,float *sum, float *dest);
void fftProd(FILE *in1F,meta_parameters *metaMaster,
		FILE *in2F,meta_parameters *metaSlave,float **corrImage);
void status(char *message);


void fftMatch(char *inFile1, char *inFile2, char *corrFile, char *descFile)
{
  int x,y;
  float bestLocX,bestLocY,doubt;
  float *corrImage=NULL;
  FILE *corrF=NULL,*descF,*in1F,*in2F;
  meta_parameters *metaMaster, *metaSlave, *metaOut;
  extern int optind;            /* argv index of the next argument */
  extern char *optarg;          /* current argv[] */
  int c;                        /* option letter from getopt() */
  extern FILE *fLog;
  extern int logflag,quietflag;
  
  in1F = fopenImage(inFile1,"rb");
  in2F = fopenImage(inFile2,"rb");
  metaMaster = meta_read(inFile1);
  metaSlave = meta_read(inFile2);
  
  /*Round to find nearest power of 2 for FFT size.*/
  mX = (int)(log((float)(metaMaster->general->sample_count))/log(2.0)+0.5);
  mY = (int)(log((float)(metaMaster->general->line_count))/log(2.0)+0.5);
  
  /* Keep size of fft's reasonable */	
  if (mX > 13) mX = 13;
  if (mY > 15) mY = 15;
  ns = 1<<mX;
  nl = 1<<mY;
  fft2dInit(mY, mX);
  
  if (!quietflag) printf("   FFT Size: %d samples by %d lines\n",ns,nl);
  if (!quietflag && (ns*nl*2*sizeof(float)>20*1024*1024))
    printf("   WARNING: These images will take %d megabytes of memory to match.\n"
	   "   You may want to try smaller images.\n",ns*nl*2*sizeof(float)/(1024*1024));
  
  /*Set up search chip size.*/
  chipDX=MINI(metaSlave->general->sample_count,ns)*3/4;
  chipDY=MINI(metaSlave->general->line_count,nl)*3/4;
  chipX=MINI(metaSlave->general->sample_count,ns)/8;
  chipY=MINI(metaSlave->general->line_count,nl)/8;
  searchX=MINI(metaSlave->general->sample_count,ns)*3/8;
  searchY=MINI(metaSlave->general->line_count,nl)*3/8;
  
  if (!quietflag) printf("\tChip at %dx%d, size=%dx%d\n",chipX,chipY,chipDX,chipDY);
  
  /*Optionally open the correlation image file.*/
  if (corrFile) {
    metaOut = meta_read(inFile1);
    metaOut->general->data_type= REAL32;
    metaOut->general->line_count = 2*searchY;
    metaOut->general->sample_count = 2*searchX;
    corrF=fopenImage(corrFile,"w");
    meta_write(metaOut, corrFile);
  }
  
  /*Perform the correlation.*/
  fftProd(in1F,metaMaster,in2F,metaSlave,&corrImage);
  
  /*Optionally write out correlation image.*/
  if (corrFile) {
    int outY=0;
    float *outBuf=(float*)MALLOC(sizeof(float)*metaOut->general->sample_count);
    for (y=chipY-searchY;y<chipY+searchY;y++) {
      int index=ns*modY(y);
      int outX=0;
      for (x=chipX-searchX;x<chipX+searchX;x++)
	outBuf[outX++]=corrImage[index+modX(x)];
      put_float_line(corrF,metaOut,outY++,outBuf);
    }
    meta_write(metaOut, corrFile);
    FREE(outBuf);
  }

  /*Search correlation image for a peak.*/
  findPeak(corrImage,&bestLocX,&bestLocY,&doubt);
  
  if (!quietflag)
    printf("   Offset slave image: dx = %f, dy = %f\n"
	   "   Certainty: %f%%\n",bestLocX,bestLocY,100*(1-doubt));
  if (logflag) {
    sprintf(logbuf,"   Offset slave image: dx = %f, dy = %f\n"
	    "   Certainty: %f%%\n",bestLocX,bestLocY,100*(1-doubt));
    printLog(logbuf);
  }
  if (descFile) {
    descF=FOPEN(descFile,"w");
    fprintf(descF,"%f\t%f\t%f\n",bestLocX,bestLocY,100*(1-doubt));
    FCLOSE(descF);
  }
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
  if (!quietflag) 
    printf("   Searching for Peak (largest offset=%d lines & %d samples)\n",
	   searchY,searchX);
  for (y=chipY-searchY;y<chipY+searchY;y++)
    for (x=chipX-searchX;x<chipX+searchX;x++) {
      int index=ns*modY(y)+modX(x);
      if (bestMatch<corrImage[index]) {
	bestMatch=corrImage[index];
	bestX=x;bestY=y;
      }
    }
  topOffPeak(corrImage,bestX,bestY,ns,&bestLocX,&bestLocY);
  
  /*Compute the doubt in our offset guess, by
    finding the largest of several pixels around our guess.*/
  for (y=-delY;y<=delY;y++)
    for (x=-delX;x<=delX;x++)
      if ((abs(y)>closeY)||(abs(x)>closeX)) {
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


/* readImg: reads the image file given by in
   into the (nl x ns) float array dest.  Reads a total of
   (delY x delX) pixels into topleft corner of dest, starting
   at (startY , startX) in the input file.
*/
void readImage(FILE *in,meta_parameters *meta, 
	       int startX,int startY,int delX,int delY,
	       float add,float *sum, float *dest)
{
  float *inBuf=(float *)MALLOC(sizeof(float)*(meta->general->sample_count));
  register int x,y,l;
  double tempSum=0;

  /*Read portion of input image into topleft of dest array.*/
  for (y=0;y<delY;y++)
    {
      l=ns*y;
      get_float_line(in,meta,startY+y,inBuf);
      if (sum==NULL)
	for (x=0;x<delX;x++)
	  dest[l+x]=inBuf[startX+x]+add;
      else
	for (x=0;x<delX;x++) {
	  tempSum+=inBuf[startX+x];
	  dest[l+x]=inBuf[startX+x]+add;
	}
      for (x=delX;x<ns;x++)
	dest[l+x]=0.0; /*Fill rest of line with zeros.*/
    }

  /*Fill remainder of array (bottom portion) with zero lines.*/
  for (y=delY;y<nl;y++) {
    l=ns*y;
    for (x=0;x<ns;x++)
      dest[l+x]=0.0; /*Fill rest of in2 with zeros.*/
  }
  if (sum!=NULL) 
    *sum=(float)tempSum;
  FREE(inBuf);
}


/* las_fftProd: reads both given files, and correlates them into the 
created outReal (nl x ns) float array.*/
void fftProd(FILE *in1F,meta_parameters *metaMaster,
	     FILE *in2F,meta_parameters *metaSlave,float *outReal[])
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
  readImage(in2F,metaSlave,
	    chipX,chipY,chipDX,chipDY,
	    0.0,&aveChip,in2);

  /*Compute average brightness of chip.*/
  aveChip/=-(float)chipDY*chipDX;
	
  /*Subtract this average off of image 2(chip):*/
  for (y=0;y<chipDY;y++) {
    l=ns*y;
    for (x=0;x<chipDX;x++)
      in2[l+x]=(in2[l+x]+aveChip)*scaleFact;
  }

  /*FFT image 2 */
  if (!quietflag) status("FFT Image 2");
  rfft2d(in2,mY,mX);

  /*Read image 1: Much easier, now that we know the average brightness. */
  if (!quietflag) status("Reading Image 1");
  readImage(in1F,metaMaster,
	    0,0,MINI(metaMaster->general->sample_count,ns),
	    MINI(metaMaster->general->line_count,nl),
	    aveChip,NULL,in1);

  /*FFT Image 1 */
  if (!quietflag) status("FFT Image 1");
  rfft2d(in1,mY,mX);

  /*Conjugate in2.*/
  if (!quietflag) status("Conjugate Image 2");
  for (y=0;y<nl;y++) {
    l=ns*y;
    if (y<2) x=1; else x=0;
    for (;x<ns/2;x++)
      in2[l+2*x+1]*=-1.0;
  }

  /*Take complex product of in1 and in2 into out.*/
  if (!quietflag) status("Complex Product");
  rspect2dprod(in1,in2,out,nl,ns);

  /*Zero out the low frequencies of the correlation image.*/
  if (!quietflag) status("Zero low frequencies.");
  for (y=0;y<4;y++) {
    l=ns*y;
    for (x=0;x<8;x++)
      out[l+x]=0;
    l=ns*(nl-1-y);
    for (x=0;x<8;x++)
      out[l+x]=0;
  }

  /*Inverse-fft the product*/
  if (!quietflag) status("I-FFT");
  rifft2d(out,mY,mX);

  FREE(in1);/*Note: in2 shouldn't be freed, because we return it.*/
}
