#include "asf.h"
#include "point.h"

/*Examine the given 2D strip, which runs for "len" pixels,
and has a peak at index "peak".
Compute the integrated side-lobe ratio (ratio of 
peak's total power to side-lobes' total power),
peak-to-side-lobe ratio (ratio of peak's highest power
to side-lobes' highest power), and peak width at 
half power (width in pixels of peak, until power
drops by half).  This procedure is called in both range 
and azimuth.*/

void compute_stats(float *data,int peak,int len,double *islr,double *pslr,double *pwhp)
{
	int lefthp,righthp;
	double half_power_dB=-3.0;
	lefthp=peak;
	while (data[lefthp]>half_power_dB)
		lefthp--;
	righthp=peak;
	while (data[righthp]>half_power_dB)
		righthp++;
	*pwhp=righthp-lefthp;
	
	*pslr=*islr=-1.0;
	
}

/*Examine the given 2D data, which starts at startX,startY
in the input image, has been blown up by a factor of
zoomFactor, and now spans wid*ht pixels.  Input in dB*/

void analyze_blowup(float *data,int wid,int ht,int startX,int startY)
{
	float strip[zoomSize*zoomFactor+2];
	int x,y;
	double orig_peakX,orig_peakY;
	double islr,pslr,pwhp;
/*Find image peak*/
	int peakX,peakY;
	double peakVal=-10000000000000.0;
	for (y=0;y<ht;y++)
		for (x=0;x<wid;x++)
		{
			float val=data[y*wid+x];
			if (peakVal<val)
			{
				peakVal=val;
				peakX=x;
				peakY=y;
			}
		}

/*Print out peak value*/
	orig_peakX=startX+(float)peakX/zoomFactor;
	orig_peakY=startY+(float)peakY/zoomFactor;
	printf("Amplitude peak at input line %f sample %f\n",orig_peakY, orig_peakX);
	printf("dB peak at output line %d sample %d (%f dB)\n",peakY,peakX,peakVal);
	
/*Subtract peak height from all other values*/
	for (y=0;y<ht;y++)
		for (x=0;x<wid;x++)
			data[y*wid+x]-=peakVal;
	
/*Compute statistics of peak, in range and azimuth*/
	strip[0]=strip[wid]=-1000;
	for (x=0;x<wid;x++)
		strip[x+1]=data[peakY*wid+x];
	compute_stats(&strip[1],peakX,wid,&islr,&pslr,&pwhp);
	printf("In range, ISLR=%f dB, PSLR=%f dB, half-power width=%f pixels\n",
		islr,pslr,pwhp/zoomFactor);
	for (y=0;y<ht;y++)
		strip[y]=data[y*wid+peakX];
	compute_stats(&strip[1],peakY,ht,&islr,&pslr,&pwhp);
	printf("In azimuth, ISLR=%f dB, PSLR=%f dB, half-power width=%f pixels\n",
		islr,pslr,pwhp/zoomFactor);
}
