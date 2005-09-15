/*cproc_hist.c:
	Implements Tcl/Tk-callable histogram routines in C.
These are called by the "routing" routines in tkCommand.c.
*/
#include "main.h"
#include "image.h"

#define SQR(X) ((X)*(X))




void cproc_renderhist(int width,int height,int *minHt,int *maxHt,
			double *minVal,double *maxVal,double *mean,double *rms)
/* TCL asks C to render 
	the image histogram into img_hist.*/
{

#if defined(lil_endian)
	Tk_PhotoImageBlock block={NULL,-1,-1,-1,-1, {2,1,0}};
#elif defined(big_endian)
	Tk_PhotoImageBlock block={NULL,-1,-1,-1,-1, {sizeof(int)-3,sizeof(int)-2,sizeof(int)-1}};
#else
#   error "You must define either lil_endian or big_endian!"
#endif
	Tk_PhotoImageBlock *blockPtr=&block;
	Tk_PhotoHandle hPhoto=Tk_FindPhoto(interp,"img_hist");
	
	int *dest=(int *)MALLOC(sizeof(int)*width*height);

	/*Compute histogram, staying within the current selection*/
	double sum=0.0;
	int denominator=0;
	int x;

	/* get minimum & maximum intensities */
	*minVal = scale_fromByte(0);	/*scale_fromByte is a macro in tk_interface.h*/
	*maxVal = scale_fromByte(255);	/*scale_fromByte is a macro in tk_interface.h*/

	/* Render histogram into "dest" buffer (black & white) & get some statistics */
	renderHist(data_hist,data_hist->min,data_hist->max,minHt,maxHt,mean,0x00000000,0x00FFffFF,dest,width,height);
	
	/* Find Root Mean Squared */
	sum = 0.0;
	for (x=0;x<width;x++)
	{
		sum += SQR(((double)x - data_hist->offset)/data_hist->slope - *mean);
		denominator += data_hist->bins[x];
	}
	*rms = sqrt(sum / (double)denominator);
	
	/*Tell TCL about the block of pixels we'll be feeding it*/
	blockPtr->pixelPtr=(unsigned char *)dest;
	blockPtr->width=width;
	blockPtr->height=height;
	blockPtr->pitch=sizeof(int)*width;
	blockPtr->pixelSize=sizeof(int);

	/*Feed TCL with pixels*/
	Tk_PhotoPutBlock(hPhoto, blockPtr, 0,0, width, height);

	FREE(dest);
}
