/******************************************************************************
NAME: spectra

SYNOPSIS: spectra [ startX startY ] <signal data>

DESCRIPTION:

    Spectra computes range and azimuth spectra of the input complex signal data
    file.  The spectra are written to the output files "spectra_range", and
    "spectra_az", which are 1024-line long ASCII files containing the
    (arbitrary-units) power of the signal data at freqencies between 0 Hz and
    the sampling frequency. In range, this is several tens of megahertz (18.96
    MHz for ERS); in azimuth, this is about a thousand hertz (about 1700 Hz for
    ERS).

    Spectra is useful in checking signal data quality/validity, but most people
    have no reason to use it.

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
    1.1     ?/??   ?. ??????
    1.3    12/03   P. Denny     Update command line order & parsing

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   spectra computes the range and azimuth power spectra of a complexn      *
*	    signal data file.						    *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/


#include <math.h>
#include "asf.h"
#include "aisp_defs.h"
#include "read_signal.h"

#undef VERSION
#define VERSION 1.3

#define FFT_LENGTH 1024
#define NUM_ARGS 1

int main(int argc,char **argv)
{
	int startX=0,startY=0;
	int x,y,i;
	getRec *r;
	complexFloat *inBuf;
	complexFloat fftBuf[FFT_LENGTH];
	float outBuf[FFT_LENGTH];
	FILE *outF;
	
/* Parse command line arguments */
	while (currArg < (argc-NUM_ARGS)) {
	   char *key = argv[currArg++];
	   if (strmatch(key,"-x")) {
	      CHECK_ARG(1);
	      startX = atoi(GET_ARG(1));
	   }
	   else if (strmatch(key,"-y")) {
	      CHECK_ARG(1);
	      startY = atoi(GET_ARG(1));
	   }
	   else {printf( "\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < NUM_ARGS) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	r = fillOutGetRec(argv[currArg]);

	inBuf=(complexFloat *)MALLOC(sizeof(complexFloat)*FFT_LENGTH*FFT_LENGTH);
	cfft1d(FFT_LENGTH,NULL,0);
	
/*Read a FFT_LENGTH x FFT_LENGTH block of input complex data.*/
	for (y=0;y<FFT_LENGTH;y++)
		getSignalLine(r,startY+y,&inBuf[y*FFT_LENGTH],startX,FFT_LENGTH);
		
/*Compute range spectra.*/
	for (i=0;i<FFT_LENGTH;i++) outBuf[i]=0;
	outF=FOPEN("input_data","w");
	for (y=0;y<FFT_LENGTH;y++)
	{
		for (x=0;x<FFT_LENGTH;x++)
		{
			fftBuf[x]=inBuf[y*FFT_LENGTH+x];
			fprintf(outF,"%f %f\n",fftBuf[x].real,fftBuf[x].imag);
		}
		cfft1d(FFT_LENGTH,fftBuf,-1);
		for (i=0;i<FFT_LENGTH;i++) 
			outBuf[i] += fftBuf[i].real * fftBuf[i].real
                                     + fftBuf[i].imag * fftBuf[i].imag;
	}
	outF=FOPEN("spectra_range","w");
	for (i=0;i<FFT_LENGTH;i++) 
		fprintf(outF,"%d %f\n",i,outBuf[i]);
	FCLOSE(outF);


/*Compute azimuth spectra.*/
	for (i=0;i<FFT_LENGTH;i++) outBuf[i]=0;
	for (x=0;x<FFT_LENGTH;x++)
	{
		for (y=0;y<FFT_LENGTH;y++)
			fftBuf[y]=inBuf[y*FFT_LENGTH+x];
		cfft1d(FFT_LENGTH,fftBuf,-1);
		for (i=0;i<FFT_LENGTH;i++) 
			outBuf[i] += fftBuf[i].real * fftBuf[i].real
                                     + fftBuf[i].imag * fftBuf[i].imag;
	}
	outF=FOPEN("spectra_az","w");
	printf("Writing the average azimuthal spectra\n");
	for (i=0;i<FFT_LENGTH;i++) 
		fprintf(outF,"%d %f\n",i,outBuf[i]/FFT_LENGTH);
	FCLOSE(outF);
	
	return 0;
}


void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-x <startX>]  [-y <startY>] <raw>\n",name);
 printf("\n"
	"REQUIRED ARGUMENT:\n"
	"   <raw>   Raw complex signal data file.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -x   sample <startX> (Default is 0)\n"
	"   -y   line <startY> (Default is 0)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Computes the range and azimuth power spectra of a complex signal data file.\n"
	"   These spectra are written to files 'spectra_range' and 'spectra_az' and are\n"
	"   both %d points long.\n", FFT_LENGTH);
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}


