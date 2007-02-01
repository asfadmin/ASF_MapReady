/******************************************************************************
NAME:cpx_spectrum- Calculates the azimuthal spectrum of a complex image product

SYNOPSIS:   

  cpx_spectrum <complex image input> <azimuthal spectrum file> <start_line>

        <complex image input>           Input Complex Image
        <azimuthal spectrum file>       ASCII file containing the spectrum
        <start_line>                    Line to begin processing at

DESCRIPTION:

  This program is used to generate an azimuthal spectrum of
  a complex  SAR image  product.   Its  main use is for bandpass filtering
  of complex image data (see cpx_autofilter). It takes a processed complex
  image and calculates an average azimuthal spectrum over all the range
  samples.  It uses a simple one-dimensional FFT to calculate the
  spectrum and outputs the frequency, amplitude, and phase an ASCII
  file for inspection.  The output file appends the extension .spectra
  to the end of the <azimuthal spectrum file> filename.
  The file format is:

  Column #
  1.  Frequency - All positive values.  The FFT has been examined from 0->fs
  where fs is the sampling frequency.  Standard FFT's generate data from
  -fs/2->fs/2, but since this data is complex, there is no intrinsic symmetry
  so 0->fs is perfectly acceptable.  For ERS1-ERS2 fs~1679 Hz or 1 PRF.

  2.  Amplitude - The amplitude of the calculated spectrum.  This is a linear
  representation of the amplitude spectrum.

  3.  Phase - The phase of the calculated spectrum.  This field doesn't really
  appear to have much use, but was included for completeness.

  This file is very easy to view in a program such as Matlab or Excel.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/00   M. Ayers
    1.5     6/03   P. Denny     Standardize usage()
                                  Use meta struct instead of DDR
                                  Use get_*_lines instead of FREAD
                                  Use complexFloat type instead of FCMPLX

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   cpx_spectrum - Calculates the azimuthal spectrum of a complex image	    *
*		   product.						    *
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
#include "asf_meta.h"

#define FFT_LEN 2048
#define VERSION 1.10


void usage (char *name)
{
 printf("\n"
	"USAGE:\n"
 	"   %s <in_cpx> <az_specs> <start_line>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <in_cpx>      Input Complex Image\n"
	"   <az_specs>    ASCII file containing the azimuthal spectrum\n"
	"   <start_line>  Line to begin processing at.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   An Azimuthal Spectrum Calculator for Complex Images\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);	
}



int main(int argc,char **argv)
{
  int lines,samps,start_line;   /* Number of image lines and samples          */
  int x,y,ii;                   /* Counters and the filter frequency indicies */
  char *inName, *outName;       /* Input filename                             */
  char metaName[256];           /* Name of meta file                          */
  complexFloat *inBuf;          /* Input/Output Image Buffers                 */
  complexFloat *fftBuf;         /* FFT Buffer for the image                   */
  float *ampBuf,*phsBuf;        /* Amplitude and Phase Buffers                */
  float df, freq[FFT_LEN], f_s; /* Frequency Vector                           */
  FILE *inF,*outF1;             /* Input and Output file pointers             */
  meta_parameters *meta;        /* Meta-file data pointer                     */

/* Usage is shown if the user doesn't give correct number of arguments */
  if(argc!=4) { usage(argv[0]); }

  StartWatch();

/* Get the filename and filter start and end frequencies from the command line */ 
  inName  = argv[1];
  outName = argv[2];
  start_line = atoi(argv[3]);
  create_name(metaName, inName, ".meta");

/* Get the PRF and number of samples from the meta-file */
  meta = meta_read(metaName);
  lines = FFT_LEN;
  samps = meta->general->sample_count;
  f_s   = 1/(meta->sar->azimuth_time_per_pixel);
  printf("Sampling Frequency is %f\n",f_s);

/* Compute the FFT length based on the number of lines. Must be a power of 2 */
  printf("FFT Length is %d\n",FFT_LEN);
  cfft1d(FFT_LEN,NULL,0);

/* Allocate the memory for all the buffers */
  inBuf  = (complexFloat *)MALLOC(sizeof(complexFloat)*samps*FFT_LEN);
  fftBuf = (complexFloat *)MALLOC(sizeof(complexFloat)*FFT_LEN);
  ampBuf = (float *)MALLOC(sizeof(float)*FFT_LEN);
  phsBuf = (float *)MALLOC(sizeof(float)*FFT_LEN);

  df = f_s/FFT_LEN;
  for(ii=0; ii<FFT_LEN; ii++)   freq[ii] = df*ii;

/* Open the Complex Image File */
  if((inF=FOPEN(inName,"rb"))==NULL) {	
    printf("Complex Image file %s could not be opened\n",inName);
    exit(EXIT_FAILURE);
  }

/* Zero out all the arrays and begin processing data */
  for(ii=0; ii<FFT_LEN; ii++) {
    ampBuf[ii]=0;
    phsBuf[ii]=0;
  }

/* Read in the data chunk */
  printf("Reading Data Starting at Line %d\n",start_line);
  get_complexFloat_lines(inF, meta, start_line, FFT_LEN, inBuf);

/* Process the each column, take the average at the end */
  printf("Performing the FFT\n");
  for(x=0; x<samps; x++) {
    if(x%500 == 0)  printf("Processing Column %d\n",x);

    for(y=0; y<FFT_LEN; y++) {
      fftBuf[y].real=0;
      fftBuf[y].imag=0;
    }

    for(y=0; y<lines; y++) {
      fftBuf[y].real=inBuf[y*samps+x].real;
      fftBuf[y].imag=inBuf[y*samps+x].imag;
    }

    cfft1d(FFT_LEN,fftBuf,-1);

    for (ii=0; ii<FFT_LEN; ii++) {
      ampBuf[ii] += sqrt(fftBuf[ii].real*fftBuf[ii].real
                         + fftBuf[ii].imag*fftBuf[ii].imag);	
      if(fftBuf[ii].imag!=0.0 || fftBuf[ii].real!=0.0)
        phsBuf[ii] += atan2(fftBuf[ii].imag, fftBuf[ii].real);
    }
  }
  printf("Finished the FFT\n");

/* Open and write output file */
  strcat(outName,".spectra"); 
  if((outF1=FOPEN(outName,"w"))==NULL) {
    printf("Unable to write output %s\n",outName);
    exit(EXIT_FAILURE);
  }
  for (ii=0; ii<FFT_LEN; ii++) {
    ampBuf[ii] /= samps;
    phsBuf[ii] /= samps;
    fprintf(outF1,"%f %f %f\n",freq[ii],ampBuf[ii],phsBuf[ii]);
  }

/* Close up & leave */
  meta_free(meta);
  FCLOSE(outF1);
  StopWatch();
  return 0;
}
