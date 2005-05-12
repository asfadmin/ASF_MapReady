/*SARview image.c:
The histogram routines.  Our histograms are divided into "bins",
which store the number of occurences of values for (in this case)
image brightness.

Orion Sky Lawlor, 5/16/99
*/
#include "main.h"
#include <asf_raster.h>
#include <limits.h>

/*Create a histogram structure with the given number of bins*/
histogram *createHist(int nBins,double min,double max)
{
	int i;
	histogram *h = (histogram *)MALLOC(sizeof(histogram));
	h->nBins   = nBins;
	h->min     = min;
	h->max     = max;
	h->slope   = h->nBins / (h->max - h->min);
	h->offset  = -h->min * h->slope;
	h->nPixels = 0;
	h->bins    = (int*)MALLOC(sizeof(int)*h->nBins);
	for (i=0; i<h->nBins; i++)
		h->bins[i] = 0;
	return h;
}

/*Add the given array of pixels to the histogram*/
void addToHist(histogram *h,float *vals,int nVals)
{
	register int i;
	register int *bins=h->bins;
	register int nBins=h->nBins;
	register double slope=h->slope,offset=h->offset;
	for (i=nVals-1;i>=0;i--)
	{
		register int dex;
		if ( FLOAT_EQUIVALENT(0.0, vals[i]) ) continue;
		dex=(int)(slope*vals[i]+offset);
		if (dex<0) dex=0;
		if (dex>=nBins) dex=nBins-1;
		bins[dex]++;
	}
	h->nPixels+=nVals;
}

/*Compute the trimmed max & min values.
  minDex & maxDex should be the image minimum and maximum if we removed
  the trimFraction smallest and trimFraction largest pixels.*/
void trimMaxMin(const histogram *h,double trimFraction,double *Tmin,double *Tmax)
{
	register int sum,nPix,minDex,maxDex;
	register int *bins=h->bins;
	double overshoot;

	nPix=(int)(h->nPixels*trimFraction);/*Compute number of pixels to trim off histogram*/
	sum=0;
	minDex=0;
	while (sum<nPix)
		sum+=bins[minDex++];
	if (minDex-1>=0)
		overshoot=(double)(nPix-sum)/bins[minDex-1];
	else
		overshoot=0;
	*Tmin=bin2val(minDex-overshoot);

	sum=0;
	maxDex=h->nBins-1;
	while (sum<nPix)
		sum+=bins[maxDex--];
	if (maxDex+1<h->nBins)
		overshoot=(double)(nPix-sum)/bins[maxDex+1];
	else
		overshoot=0;
	*Tmax=bin2val(maxDex+1+overshoot);
}

/*Copy the source histogram's bins into the dest histogram's*/
void copyHist(histogram *dest,const histogram *source)
{
	register int i;
	for (i=0;i<source->nBins;i++)
	{
		double val=(i+0.5-source->offset)/source->slope;
		int destBin=(int)(dest->slope*val+dest->offset);
		if (destBin<0) destBin=0;
		if (destBin>=dest->nBins) destBin=dest->nBins-1;
		dest->bins[destBin]+=source->bins[i];
	}
}

/*Render the given histogram into the given (4-byte) pixel buffer.
  This routine draws the number of pixels in each histogram bin,
  returning the number of pixels in the smallest and largest bin
  and the mean pixel value.*/
void renderHist(histogram *src,double min,double max,
		int *lowest,int *highest,double *mean,
		int fg,int bg,
		int *dest,int width,int height)
{
	int x,y;
	histogram *h=createHist(width,min,max);
	int highOccurance=LONG_MIN, lowOccurance=LONG_MAX;
	double sum=0.0;
	int denominator=0;

	copyHist(h,src);

/*Run through data to get lowest and highest value Occurance*/
	for (x=0;x<width;x++)
	{
		if (highOccurance<h->bins[x]) highOccurance=h->bins[x];
		if (lowOccurance >h->bins[x]) lowOccurance =h->bins[x];
		sum += bin2val((double)x) * (double)(h->bins[x]); /*bin2val macro in main.h*/
		denominator += h->bins[x];
	}

/* Figure Mean */
	*mean = sum / (double)denominator;

/*Render data into buffer-- erasing as we go*/
	for (x=0;x<width;x++)
	{
		int barEnd=(int)(height - h->bins[x]/(double)(highOccurance+1)*height+0.5);
		for (y=height-1;y>=barEnd;y--)
			dest[y*width+x]=fg;
		for (;y>=0;y--)
			dest[y*width+x]=bg;
	}

/*Record lowest & highest value*/
	*lowest =lowOccurance;
	*highest=highOccurance;

/*Free temporaries*/
	deleteHist(h);
}

/*Blow away the given histogram record*/
void deleteHist(histogram *h)
{
	FREE(h->bins);h->bins=NULL;
	FREE(h);
}
