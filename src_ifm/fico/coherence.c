/*GetPhaseCoherence: computes the phase coherence of a given interferogram.*/
#include "asf.h"
#include "las.h"
#include "asf_meta.h"
#include "ifm.h"
#include "ddr.h"

#ifndef TWOPI
# define TWOPI (2*PI)
#endif
float getPhaseCoherence(complexFloat *igram,int sizeX,int sizeY);

/*Returns x in the range (-pi, pi] */
double topi(double x);
double topi(double x)
{
	x=fmod(x,TWOPI);
	if (x>PI)
		return x-TWOPI;
	else if (x<=-PI)
		return x+TWOPI;
	return x;
}
float getPhaseCoherence(complexFloat *igram,int sizeX,int sizeY)
{
	register double phaseErr=0;
	register int npixels=0;
#define ml 8
#define boxX 1
#define boxY 8
	int targX,targY,mlSizeY=sizeY/ml;
/*Multilook the interferogram in-place:*/
	for (targY=0;targY<mlSizeY;targY++)
	{
		int targetIndex=targY*sizeX;
		int sourceIndex=targY*ml*sizeX;
		for (targX=0;targX<sizeX;targX++)
		{
			register int y;
			register double sum_imag,sum_real;
			register complexFloat *src=&igram[sourceIndex];
			sum_imag=sum_real=0;
			for (y=0;y<boxY;y++)
			{
				sum_imag+=src->imag;
				sum_real+=src->real;
				src+=sizeX;
			}
			sourceIndex++;
			igram[targetIndex++].real=atan2(sum_real,sum_imag);
		}
	}
/*Now find the deviation from f1 continuity in this multilooked image.*/
	for (targY=0;targY<mlSizeY;targY++)
	{
#define winSize 10
#define halfSize (winSize/2)
#define invWinSize (1/(float)winSize)
		float deltas[500],sumDiff=0;
		int index=targY*sizeX;
	/*Compute the phase difference at each pixel.*/
		for (targX=0;targX<(sizeX-1);targX++)
			deltas[targX]=topi(igram[index+targX+1].real-igram[index+targX].real);
	/*Compute a moving average of the phase difference, and compare
		this to the middle value.*/
		for (targX=0;targX<winSize;targX++)
			sumDiff+=deltas[targX];
		for (targX=halfSize;targX<(sizeX-1-halfSize);targX++)
		{
			phaseErr+=fabs(sumDiff*invWinSize-deltas[targX]);
			sumDiff-=deltas[targX-halfSize];
			sumDiff+=deltas[targX+halfSize];
			npixels++;
		}
	}
	return 1.0/(phaseErr/npixels);
}

