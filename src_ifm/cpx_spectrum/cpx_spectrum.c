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
  of complex image data (see cpx_autofilter) .It takes a processed complex
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
    1.0	   5-2000  M. Ayers

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
#include "ddr.h"
#include "asf_meta.h"

#define fftLen 2048
#define VERSION 1.10

main(int argc,char **argv)
{
	
/* Define all the variables that we'll be using */
	
	int lines,samps,start_line;			/* Number of image lines, samples*/
	int x,y,i;					/* Counters and the filter frequency indicies */
	struct DDR inDDR;				/* DDR structures to figure out info about the images */
	char *infile, *outfile;				/* Input filename */
	char *metafile;
	FCMPLX *inBuf;					/* Input/Output Image Buffers */
	FCMPLX *fftBuf;					/* FFT Buffer for the image */
	float *ampBuf,*phsBuf;				/* Amplitude and Phase Buffers */
	float df, freq[fftLen], f_s;				/* Frequency Vector */
	FILE *inF,*outF1;				/* Input and Output file pointers */
	meta_parameters *meta;				/* Meta-file data pointer */

/* Usage is shown if the user doesn't give input and output arguements */
	if(argc!=4)
	{
	  printf("\nUsage:\n");
	  printf("%s <cpx img_in> <azimuthal spectrum file>"
	         " <start_line>\n\n",argv[0]);
	printf( "	     <cpx img_in>     Input Complex Image\n"
		"<azimuthal spectrum file>     ASCII file containing the spectrum\n"
		"	     <start_line>     Line to begin processing at.\n");
	  printf("\ncpx_spectrum - An Azimuthal Spectrum Calculator for Complex Images\n");
	  printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
	  exit(0);	
	}

	StartWatch();
	

/* Get the filename and filter start and end frequencies from the command line */ 
	infile=argv[1];
	outfile=argv[2];
	sscanf(argv[3],"%d",&start_line);
	metafile=appendExt(infile,".meta");	

/* Get the number of lines and samples from the input DDR */
	c_getddr(infile,&inDDR);
	lines=fftLen;
	samps=inDDR.ns;

/* Get the PRF from the meta-file */
	meta=meta_init(metafile);
	f_s=1/(meta->geo->azPixTime);
	printf("Sampling Frequency is %f\n",f_s);

/* Compute the FFT length based on the number of lines. Must be a power of 2 */

	printf("FFT Length is %d\n",fftLen);
	cfft1d(fftLen,NULL,0);

	printf("Allocating Memory\n");

/* Allocate the memory for all the buffers */
          
        inBuf=(FCMPLX *)MALLOC(sizeof(FCMPLX)*samps*fftLen);
	fftBuf=(FCMPLX *)MALLOC(sizeof(FCMPLX)*fftLen);

	ampBuf=(float *)MALLOC(sizeof(float)*fftLen);
        phsBuf=(float *)MALLOC(sizeof(float)*fftLen);
	

	df=f_s/fftLen;
	for(i=0;i<fftLen;i++)
		freq[i]=df*i;

/* Open the Complex Image File */
	
	if((inF=FOPEN(infile,"rb"))==NULL)
	{	
		printf("Complex Image file %s could not be opened\n",infile);
		exit(0);
	}
	
        strcat(outfile,".spectra"); 

	if((outF1=FOPEN(outfile,"w"))==NULL)
        {
                printf("Unable to write output %s\n",outfile);
                exit(0);
        }
	
                
/* Zero out all the arrays and begin processing data */

	for(i=0;i<fftLen;i++)
	{
		ampBuf[i]=0;
		phsBuf[i]=0;
	}
	

/* Read in the data chunk */
	printf("Reading Data Starting at Line %d\n",start_line);
	fseek(inF,sizeof(FCMPLX)*samps*start_line,SEEK_SET);
	fread(inBuf,sizeof(FCMPLX)*samps*fftLen,1,inF);

/* Process the each column, take the average at the end */
	
	printf("Performing the FFT\n");
	for(x=0;x<samps;x++) 
	{
		if(x%500 == 0)  printf("Processing Column %d\n",x);

		for(y=0;y<fftLen;y++)
		{
			fftBuf[y].r=0;
			fftBuf[y].i=0;
		}
		
		for(y=0;y<lines;y++)
		{
			fftBuf[y].r=inBuf[y*samps+x].r;
			fftBuf[y].i=inBuf[y*samps+x].i;
		}

		cfft1d(fftLen,fftBuf,-1);

		for (i=0;i<fftLen;i++) 
		{
			ampBuf[i]+=sqrt(fftBuf[i].r*fftBuf[i].r+fftBuf[i].i*fftBuf[i].i);	

			if(fftBuf[i].i!=0.0 || fftBuf[i].r!=0.0)
				phsBuf[i]+=atan2(fftBuf[i].i,fftBuf[i].r);
			else
				phsBuf[i]+=0;

		}
		
		
	}
	
	printf("Finished the FFT\n");
	

	for (i=0;i<fftLen;i++)
	{
		ampBuf[i]/=samps;
		phsBuf[i]/=samps;
		fprintf(outF1,"%f %f %f\n",freq[i],ampBuf[i],phsBuf[i]);
		
	}


	FCLOSE(outF1);
	StopWatch();
	return 0;
}

