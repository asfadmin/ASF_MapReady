/******************************************************************************
NAME: spectra

    SYNOPSIS: spectra <signal data> [ startX startY ]

    DESCRIPTION:

	Spectra computes range and azimuth spectra of the input
complex signal data file.  The spectra are written to the output
files "spectra_range", and "spectra_az", which are 1024-line long
ASCII files containing the (arbitrary-units) power of the signal
data at freqencies between 0 Hz and the sampling frequency.
In range, this is several tens of megahertz (18.96 MHz for ERS);
in azimuth, this is about a thousand hertz (about 1700 Hz for ERS).

	Spectra is useful in checking signal data quality/validity,
but most people have no reason to use it.

	Spectra's output files have the format:
<FFT bin #> <power>
<FFT bin #> <power>
...

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/98   O. Lawlor

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   spectra computes the range and azimuth power spectra of a complexn      *
*	    signal data file.						    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/


#include <math.h>
#include "asf.h"

#include "aisp_defs.h"
#include "read_signal.h"

#define Version 1.1

int main(int argc,char **argv)
{
#define fftLen 1024

	int startX=0,startY=0;
	int x,y,i;
	getRec *r;
	complexFloat *inBuf;
	complexFloat fftBuf[fftLen];
	float outBuf[fftLen];
	FILE *outF;
	if ((argc!=2 && argc!=4)||(argv[1][0]=='-'))
		{printf("\nUSAGE: spectra <raw> [start X start Y]\n"
		"\n"
		"    <raw>:	APD raw complex signal data file.\n"
		"    start X:	sample (Default is 0)\n"
		"    start Y:	line (Default is 0)\n\n"
		"spectra computes the range and azimuth\n"
		"power spectra of a complex signal data file.\n"
		"These spectra are written to files 'spectra_range'\n"
		"and 'spectra_az' and are both %d points long.\n\n"
		"Version %.2f, ASF SAR TOOLS\n\n",fftLen, Version);exit(1);}
	
	r=fillOutGetRec(argv[1]);
	if (argc>3)
	{
		startX=atoi(argv[2]);
		startY=atoi(argv[3]);
	}
	
	inBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*fftLen*fftLen);
	cfft1d(fftLen,NULL,0);
	
/*Read a fftLen x fftLen block of input complex data.*/
	for (y=0;y<fftLen;y++)
		getSignalLine(r,startY+y,&inBuf[y*fftLen],startX,fftLen);
		
/*Compute range spectra.*/
	for (i=0;i<fftLen;i++) outBuf[i]=0;
	outF=FOPEN("input_data","w");
	for (y=0;y<fftLen;y++)
	{
		for (x=0;x<fftLen;x++)
		{
			fftBuf[x]=inBuf[y*fftLen+x];
			fprintf(outF,"%f %f\n",fftBuf[x].real,fftBuf[x].imag);
		}
		cfft1d(fftLen,fftBuf,-1);
		for (i=0;i<fftLen;i++) 
			outBuf[i] += fftBuf[i].real * fftBuf[i].real
                                     + fftBuf[i].imag * fftBuf[i].imag;
	}
	outF=FOPEN("spectra_range","w");
	for (i=0;i<fftLen;i++) 
		fprintf(outF,"%d %f\n",i,outBuf[i]);
	FCLOSE(outF);


/*Compute azimuth spectra.*/
	for (i=0;i<fftLen;i++) outBuf[i]=0;
	for (x=0;x<fftLen;x++)
	{
		for (y=0;y<fftLen;y++)
			fftBuf[y]=inBuf[y*fftLen+x];
		cfft1d(fftLen,fftBuf,-1);
		for (i=0;i<fftLen;i++) 
			outBuf[i] += fftBuf[i].real * fftBuf[i].real
                                     + fftBuf[i].imag * fftBuf[i].imag;
	}
	outF=FOPEN("spectra_az","w");
	printf("Writing the average azimuthal spectra\n");
	for (i=0;i<fftLen;i++) 
		fprintf(outF,"%d %f\n",i,outBuf[i]/fftLen);
	FCLOSE(outF);
	
	return 0;
}

