/*SPECAN SAR Processor Include File
by Orion Lawlor, ASF 11/98.

This is the actual implementation of 
the SPECAN SAR processor.  It's designed to
work exactly the same whether you're 
compressing in range or azimuth-- in fact,
this routine only does one at a time.
*/
#include "asf.h"
#include "aisp_defs.h" /* for FCMPLX, cfft1d */
#include "specan.h"

/*Returns i mapped, mod fs, onto [-fs/2,fs/2)*/
double mapHalf(double fs,double i);
double mapHalf(double fs,double i)
{
	while (i<-fs/2) i+=fs;
	while (i>=fs/2) i-=fs;
	return i;
}

/*Return i modulo len, so i is on [0,len)*/
double modLen(double len,double i);
double modLen(double len,double i)
{
	while (i<0) i+=len;
	while (i>=len) i-=len;
	return i;
}

/*Computes calculated parameters from filled-in parameters.*/
void specan_init(specan_struct *s)
{
	int i;
	int len=s->fftLen;
	double fs;/*Input sampling rate [Hz] (=1/iSamp)*/
	double center,oppCenter;
	double chirpAbs=fabs(s->chirpSlope);
	int chirpSign=s->chirpSlope/chirpAbs;/*1-> up chirp; -1 ->down chirp.*/
	double resampPix;/*First pixel of valid output*/
	double wasted,bandWasted;/*Wasted samples due to circular convolution & bandwidth*/
	double toFreq,fromFreq;/*Convert output index to/from frequency [Hz/pixel, pixel/Hz]*/
/*Calculate the easy parameters:*/
	fs=1.0/s->iSamp;
	toFreq=fs/len;fromFreq=len/fs;
	s->oSamp=toFreq/chirpAbs;

/*Determine wasted samples*/
	wasted=len*len*s->iSamp*s->iSamp*chirpAbs;
	bandWasted=(1-s->bandwidth)*len;
	if (wasted+bandWasted>len)
	{
		sprintf(errbuf,"No valid output would result from using\n"
			"SPECAN with %d fft samples.  Use a smaller fft length.\n",
			len);
		printErr(errbuf);
	}

/*Find the chirp center and the opposite of the chirp center*/
	center=s->chirpCenter*fromFreq;
	/*Find index of the spectral opposite of the chirp's center frequency
	(i.e. center of unused bandwidth).*/
	oppCenter=center+len/2;
	
/*Compute valid bounds in output space, then quantize to indices.*/
	s->oFirst=modLen(len,oppCenter-chirpSign*(bandWasted/2+wasted));
	s->oLast =modLen(len,oppCenter+chirpSign*(bandWasted/2));
	s->oNum=(int)(modLen(len,chirpSign*(s->oFirst-s->oLast)));
	
/*Find these bounds in input space.*/
	s->iFirst=(int)(mapHalf(fs,s->chirpCenter-toFreq*s->oFirst)/s->chirpSlope*fs);
	s->iLast =(int)(mapHalf(fs,s->chirpCenter-toFreq*s->oLast )/s->chirpSlope*fs);
	s->iNum=s->iLast-s->iFirst;

	if (s->chirpSlope<0)/*Down chirp-- starts from oFirst and goes right way*/
		resampPix=s->oFirst;
	else /*Up chirp-- starts at oFirst and goes backwards, so line up end.*/
		resampPix=s->oLast;
	
	s->oCenter=modLen(len,s->powerCenter*fromFreq-s->oFirst);
	

/*Initialize deramping array.*/
	for (i=0;i<len;i++)
	{
		double t=i*s->iSamp;
		double resampSlope=-resampPix/len*fs;
		double phase=2*pi*(t*resampSlope-s->chirpSlope*t*t/2);
		/* t term: Resampling shift*/
		/* t*t term: Quadratic phase => linear FM, conjugated.*/
		s->deramp[i]=Cmplx(cos(phase),sin(phase));
	}
	
}

/*Perform SPECAN SAR Processing.*/
void specan_process(specan_struct *s,FCMPLX *input,FCMPLX *output)
{
	register int i,len=s->fftLen,oNum=s->oNum-1;
	register FCMPLX *deramp=s->deramp;
	FCMPLX xform[SPECAN_MAXFFT];/*Transform array*/

/*Deramp:*/
	for (i=0;i<len;i++)
		xform[i]=Cmul(input[i],deramp[i]);
	
/*FFT:*/
	cfft1d(len,xform,0);
	cfft1d(len,xform,-1);
	
/*Copy out output:*/
	if (s->chirpSlope<0)
	/*Down chirp: Data is in order*/
		for (i=0;i<=oNum;i++)
			output[i]=xform[i];
	else
	/*Up chirp: Data is in reverse order*/
		for (i=0;i<=oNum;i++)
			output[i]=xform[oNum-i];
}
