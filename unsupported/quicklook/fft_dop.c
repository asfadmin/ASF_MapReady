/*
fft_dop.c:
	A routine to quickly determine the central
doppler frequency of a given piece of SAR signal data.
This is much faster than estdop; but only gives *one*
number (the central doppler freqency), instead of a 
function (quadratic in sample), for the doppler.
*/
#include "asf.h"
#include "aisp_defs.h"

/*Return the maximum value of given array*/
float findPeak(float *arr,int len)
{
	float bestVal=-1e30;
	int bestDex=-1;
	int i;
	for (i=0;i<len;i++)
		if (bestVal<arr[i])
		{
			bestVal=arr[i];
			bestDex=i;
		}
	if (bestDex==0||bestDex==len-1)
		return bestDex;
	else
	{	/*Top off peak.*/
		double a,b,c,d;
		a=arr[bestDex-1];
		b=arr[bestDex];
		c=arr[bestDex+1];
		d=4*((a+c)/2-b);
		if (d!=0)
			return bestDex+(a-c)/d;
		else return bestDex;
	}
}
/*Filters the given float array to the low "keepSamples"
spectral components (low-pass filter).  If keepSamples
is small, the filter is strong (only lets the lowest
freqencies by)*/
void filter_lowPass(float *arr,int fftLen,int keepSamples)
{
	int i;
/*Init.*/
	FCMPLX *fft=(FCMPLX *)MALLOC(sizeof(FCMPLX)*fftLen);
	cfft1d(fftLen,NULL,0);
/*Copy arr into fft buffer.*/
	for (i=0;i<fftLen;i++)
	{
		fft[i].r=arr[i];
		fft[i].i=0;
	}
	cfft1d(fftLen,fft,-1);/*Forward FFT.*/
/*Filter out high frequencies.*/
	for (i=1+keepSamples;i<fftLen-keepSamples;i++)
	{
		fft[i].r=0;
		fft[i].i=0;
	}
	cfft1d(fftLen,fft,1);/*Reverse FFT.*/
/*Copy (smoothed) fft buffer back into arr.*/
	for (i=0;i<fftLen;i++)
		arr[i]=Cabs(fft[i]);
	FREE(fft);
}
/*Return the average doppler centroid (in fractional PRF)
of inFile using fftLen lines starting at startLine*/
double fftEstDop(getRec *inFile,int startLine,int xStride,int fftLen)
{
	int x,y,i,wid=inFile->nSamples;
	float *power,peak;
	FCMPLX *in,*fft;
/*Allocate arrays.*/
	in=(FCMPLX *)MALLOC(sizeof(FCMPLX)*fftLen*wid);
	fft=(FCMPLX *)MALLOC(sizeof(FCMPLX)*fftLen);
	power=(float *)MALLOC(sizeof(float)*fftLen);
/*Set power sum to zero.*/
	for (i=0;i<fftLen;i++)
		power[i]=0;
/*Read in fftLen lines of data.*/
	for (y=0;y<fftLen;y++)
		getSignalLine(inFile,startLine+y,&(in[y*wid]),0,wid);
/*FFT each column, add to power sum array.*/
	cfft1d(fftLen,NULL,0);
	for (x=0;x<wid;x+=xStride)
	{
		for (y=0;y<fftLen;y++)
			fft[y]=in[y*wid+x];
		cfft1d(fftLen,fft,-1);
		for (i=0;i<fftLen;i++)
			power[i]+=fft[i].r*fft[i].r+fft[i].i*fft[i].i;
	}

/*Find peak of azimuth power array-- this is the center of the doppler.*/
	power[0]=0.0;/*Zero out DC component (misleading).*/
	filter_lowPass(power,fftLen,4);/*Do a low-pass on the power array.*/
	peak=findPeak(power,fftLen);

	if (!quietflag) printf("Peak at %f\n",peak);

	FREE(in);
	FREE(fft);
	FREE(power);
	return peak/fftLen;
}
