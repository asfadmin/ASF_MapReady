/*GetFFTCorrelation: computes the maximum FFT value of a given interferogram.*/
#include "fft.h"
#include "ifm.h"
#include <math.h>

float getFFTCorrelation(complexFloat *igram,int sizeX,int sizeY);


float getFFTCorrelation(complexFloat *igram,int sizeX,int sizeY)
{

	int line, samp;
	int fftpowr;
	float ampTmp=0;
	float maxAmp=0;
	complexFloat *fftBuf;
	complexFloat *fftTemp;
	complexFloat *fft;

/*	fftBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*sizeX);*/
	fft=(complexFloat *)MALLOC(sizeof(complexFloat)*sizeX*sizeX);
	fftTemp=(complexFloat *)MALLOC(sizeof(complexFloat)*sizeX*sizeX);

	fftpowr=(log(sizeX)/log(2));
	
	fftInit(fftpowr);


	/* Compute the 2d complex FFT since no libraries exist to do this */

	/* First do the FFT of each line*/
	for(line=0;line<sizeX;line++)
	{
		fftBuf=&igram[line*sizeX];

		ffts((float *)fftBuf,fftpowr,1);
		for(samp=0;samp<sizeX;samp++)
		{
			fftTemp[line*sizeX+samp].real=fftBuf[samp].real;
			fftTemp[line*sizeX+samp].imag=fftBuf[samp].imag;
		}

	}

	/* Now do the FFT of the columns of the FFT'd lines */
	for(samp=0;samp<sizeX;samp++)
        {
		
		/* Fill up the FFT buffer */
		for(line=0;line<sizeX;line++)
		{
		fftBuf[line].real=fftTemp[line*sizeX+samp].real; 
		fftBuf[line].imag=fftTemp[line*sizeX+samp].imag;
		}		                

		/* Do the FFT */
		
		ffts((float *)fftBuf,fftpowr,1);
                for(line=0;line<sizeX;line++)
                {
                        fft[line*sizeX+samp].real=fftBuf[line].real;
                        fft[line*sizeX+samp].imag=fftBuf[line].imag;
                }
         
        }
	free(fftTemp);
	fftFree();

	/* Now we have a two dimension FFT that we can search to find the max value */

	for(line=0;line<sizeX;line++)
	{
		for(samp=0;samp<sizeX;samp++)
		{
			ampTmp=sqrt(fft[line*sizeX+samp].real*fft[line*sizeX+samp].real+fft[line*sizeX+samp].imag*fft[line*sizeX+samp].imag);
			if(ampTmp>maxAmp)
				maxAmp=ampTmp;
		}
	}
	free(fft);
	return maxAmp;
	
}

