/******************************************************************************
NAME:       gen_filt_params - Generate filter parameters for cpx_filter

SYNOPSIS:

        gen_filt_params <cpx img1> <cpx img2> <out_filt_params>

                *no extensions are required for this program*
                *the program looks for a .spectra file for both images*

                <cpx img1>     Complex Input image 1
                <cpx img2>     Complex Input image 2
                <out_filt_params>  Output file containing filter parameters

DESCRIPTION:
                In order to automate the cpx_filtering routine, a mean needs
        to exist to estimate the stopband, passband, and frequency shift
        applied to the complex image pair automatically.  gen_filt_params
        performs this task.  It takes both complex image files as input and
        using the metadata and the azimuthal spectra, estimates the passband
        and the frequency shift.  The output file has the following format in
        ASCII text:

        line 1. frequency shift (in Hertz)
        line 2. loFreq1 hiFreq1 (in Hertz)
        line 3. loFreq2 hiFreq2 (in Hertz)

                The frequency shift simply specifies the amount both spectra
        should be shifted to return them both to baseband.  The first pair of
        frequency specifications indicates the first stopband, the second pair
        specifies the second stopband (There are potentially two stopbands
        depending on how the spectra wrapped around the 1 PRF sampling
	frequency).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	   5/00   M. Ayers       Original development
    1.5    5/03   P. Denny       Standardize usage()
                                  Update meta struct values

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   gen_filt_params - Generate filter parameters for cpx_filter		    *
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

#define fftLen 2048	/* this is the default FFT length from the cpx_spectrum program */
#define thresh 0.1	/* set the threshold to determine which frequencies we will be zeroing currently 5% of max amplitude */
#define VERSION 1.10


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <cpx img1> <cpx img2> <out_filt_params>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
        "   <cpx img1>          Complex Input image 1 (no extension)\n"
        "   <cpx img2>          Complex Input image 2 (no extension)\n"
        "   <out_filt_params>   Output file containing filter parameters\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program generates filter parameters for cpx_filter");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}


int main(int argc,char **argv)
{
	
/* Define all the variables that we'll be using */
	
	int i,j,k;					/* Program Counter */
	meta_parameters *meta1, *meta2;			/* Input meta files for both images */
	char *infile1, *infile2, *outfile;		/* Input filenames */
	char specFile1[255], specFile2[255];		/* Spectra filenames */
	FILE *inSpectra1, *inSpectra2, *outF1;		/* Input and Output file pointers */
	double *azDop1, *azDop2;			/* Image average dopplers */
	float freq[fftLen], amp1[fftLen], amp2[fftLen];	/* Frequency support and amplitude values */
	float phase1[fftLen], phase2[fftLen];		/* Phase values */
	float max_amp1, max_amp2, min_amp1, min_amp2;	/* Maximum, Minimum spectral amplitudes */
	int index1[fftLen], index2[fftLen];		/* Indices for lo and hi frequencies of filtering */
	int two_bands1, two_bands2;			/* Are there two filter bands in either spectra? */
	float lo_im1_1,lo_im1_2,hi_im1_1,hi_im1_2;	/* Filter frequencies for the first image */
	float lo_im2_1,lo_im2_2,hi_im2_1,hi_im2_2;	/* Filter frequencies for the second image */
	float lo_out_1, lo_out_2, hi_out_1, hi_out_2;	/* Output frequencies */
	float freq_mod, f_s;				/* Output modulation frequency */

/* Usage is shown if the user doesn't give 3 arguments */
	if(argc != 4) { usage(argv[0]); }
	
	StartWatch();

/* Get the filenames from the command line */ 
	infile1=argv[1];
	infile2=argv[2];
	outfile=argv[3];

/* Read the meta files in from the images */
	strcpy(specFile1, infile1);
	strcpy(specFile2, infile2);

	meta1 = meta_read(infile1);
	meta2 = meta_read(infile2);

	azDop1 = meta1->sar->azimuth_doppler_coefficients;
	azDop2 = meta2->sar->azimuth_doppler_coefficients;

	printf("%s Image Doppler is %f\n",infile1,azDop1[0]);
	printf("%s Image Doppler is %f\n\n",infile2,azDop2[0]);
	
	freq_mod=-(azDop1[0]+azDop2[0])/2;
	f_s = 1 / (meta1->sar->azimuth_time_per_pixel);
	printf("The sampling frequency is %f Hz\n",f_s);
	printf("The time domain shift needs to be %f Hz\n\n",freq_mod);

/* Read in the frequency and ampitude information from the .spectra files */
	
	strcat(specFile1,".spectra");
	strcat(specFile2,".spectra");
	

	if((inSpectra1=FOPEN(specFile1,"r"))==NULL)
	{
		printf("Error! Couldn't open %s\n",infile1);
		exit(EXIT_FAILURE);
	}


	if((inSpectra2=FOPEN(specFile2,"r"))==NULL) 
        { 
                printf("Error! Couldn't open %s\n",infile2);
                exit(EXIT_FAILURE);
        }

	for(i=0;i<fftLen;i++)
	{
		freq[i]=0;
		amp1[i]=0;
		amp2[i]=0;
		phase1[i]=0;
		phase2[i]=0;
	}

	for(i=0;i<fftLen;i++)	
	{
		fscanf(inSpectra1,"%f %f %f",&freq[i],&amp1[i],&phase1[i]);
		fscanf(inSpectra2,"%f %f %f",&freq[i],&amp2[i],&phase2[i]);
	}

/* Do the calculations to find the frequencies to zero */

	/* Find the maximum amplitude of each spectrum */
	
	max_amp1=0;
	max_amp2=0;
	min_amp1=10000000; /* Set these values to some arbitrary large number */
	min_amp2=10000000;

	for(i=0;i<fftLen;i++)
	{
		if(amp1[i]>max_amp1)
			max_amp1=amp1[i];
		if(amp2[i]>max_amp2)
			max_amp2=amp2[i];
		if(amp1[i]<min_amp1)
			min_amp1=amp1[i];
		if(amp2[i]<min_amp2)
			min_amp2=amp2[i];

	}

	/* Find the indices that have low spectral content */
	j=0;
	k=0;
	for(i=0;i<fftLen;i++)
        {
		if(amp1[i] < (thresh*max_amp1)+min_amp1)
		{
			index1[j]=i;
			j++;
		}
		if(amp2[i] < (thresh*max_amp2)+min_amp2)
		{
			index2[k]=i;
			k++;
		}
	}
	
	lo_im1_1=freq[index1[0]];
	lo_im1_2=0;
	hi_im1_1=freq[index1[j-1]];
	hi_im1_2=0;
	
	lo_im2_1=freq[index2[0]];
	lo_im2_2=0;
	hi_im2_1=freq[index2[k-1]];
	hi_im2_2=0;

	two_bands1=0;
	two_bands2=0;
	
	/* Let's see if there are two bands */

	for(i=0;i<j-1;i++)
	{
		if((index1[i+1]-index1[i])>100)
		{
			lo_im1_2=freq[index1[i+1]];
			hi_im1_1=freq[index1[i]];
			hi_im1_2=freq[index1[j-1]];
			two_bands1=1;
			break;
		}

	}

	for(i=0;i<k-1;i++)
        {
                if((index2[i+1]-index2[i])>100)
                {
                        lo_im2_2=freq[index2[i+1]];
                        hi_im2_1=freq[index2[i]];  
                        hi_im2_2=freq[index2[k-1]];
			two_bands2=1;
                        break;
                }
         
        }
	
	printf("%s low1: %f high1: %f\n",infile1,lo_im1_1,hi_im1_1);
	printf("%s low2: %f high2: %f\n\n\n",infile1,lo_im1_2,hi_im1_2);	
	
	printf("%s low1: %f high1: %f\n",infile2,lo_im2_1,hi_im2_1);
        printf("%s low2: %f high2: %f\n\n\n",infile2,lo_im2_2,hi_im2_2);
	printf("%s has %d bands and %s has %d bands\n",infile1,two_bands1+1,infile2,two_bands2+1);

	/* Now figure out which frequencies to filter using the calculated values */

	/* This is the case where both spectra have only one band */
	if((two_bands1==0) && (two_bands2==0))
	{
		if(lo_im1_1 < lo_im2_1)
			lo_out_1 = lo_im1_1;
		else
			lo_out_1 = lo_im2_1;
	
		if(hi_im1_1 > hi_im2_1)
			hi_out_1 = hi_im1_1;
		else
			hi_out_1 = hi_im2_1;
	}

	/* This is the case where the first spectra has one band but the second has two */
	if((two_bands1==0) && (two_bands2==1))
	{
		if(lo_im2_2>hi_im1_1)
		{
			if(lo_im2_1<lo_im1_1)
				lo_out_1=lo_im2_1;
			else	
				lo_out_1=lo_im1_1;
			
			if(hi_im2_1>hi_im1_1)
				hi_out_1=hi_im2_1;
			else
				hi_out_1=hi_im1_1;
			
			lo_out_2=lo_im2_2;
			hi_out_2=hi_im2_2;
		}

		if(lo_im2_2<hi_im1_1)
		{
			if(lo_im1_1<lo_im2_2)
				lo_out_2=lo_im1_1;
			else
				lo_out_2=lo_im2_2;
			
			if(hi_im1_1>hi_im2_2)
				hi_out_2=hi_im1_1;
			else
				hi_out_2=hi_im2_2;
		
			lo_out_1=lo_im2_1;
			hi_out_1=hi_im2_1;
		}
	}

	/* This is the case where the first spectra has two bands but the second has one */ 
        if((two_bands1==1) && (two_bands2==0))
        {
                if(lo_im1_2>hi_im2_1)
                {
                        if(lo_im1_1<lo_im2_1)
                                lo_out_1=lo_im1_1;
                        else
                                lo_out_1=lo_im2_1;
                        
                        if(hi_im2_1>hi_im1_1)
                                hi_out_1=hi_im2_1;
                        else
                                hi_out_1=hi_im1_1;
        
                        lo_out_2=lo_im1_2;
                        hi_out_2=hi_im1_2;
                }
                        
                if(lo_im1_2<hi_im2_1)
                {
                        if(lo_im2_1<lo_im1_2)
                                lo_out_2=lo_im2_1;
                        else
                                lo_out_2=lo_im1_2;
         
                        if(hi_im1_2>hi_im2_1)
                                hi_out_2=hi_im1_2;
                        else
                                hi_out_2=hi_im2_1;
        
                        lo_out_1=lo_im1_1;
                        hi_out_1=hi_im1_1;
                }       
        }

	/* The final case is the one where both spectra have two bands */
	if((two_bands1==1) && (two_bands2==1))
	{
		if(hi_im1_1>hi_im2_1)
			hi_out_1=hi_im1_1;
		else
			hi_out_1=hi_im2_1;
		
		if(lo_im1_2<lo_im2_2)
			lo_out_2=lo_im1_2;
		else
			lo_out_2=lo_im2_2;

		lo_out_1=lo_im1_1;	/* Both spectra have a minimum of 0 Hz for this value */
		hi_out_2=hi_im1_2;	/* Both spectra have a maximum value of 1 PRF for this value */
	}

	printf("%s low1: %f high1: %f\n",outfile,lo_out_1,hi_out_1);
        printf("%s low2: %f high2: %f\n\n\n",outfile,lo_out_2,hi_out_2);
	
	if((outF1=FOPEN(outfile,"w"))==NULL)
        {
                printf("Error! Couldn't open %s\n",infile2);
                exit(EXIT_FAILURE);
        }
	
	fprintf(outF1,"%f\n%f\n%f %f\n%f %f",f_s,freq_mod,lo_out_1,hi_out_1,lo_out_2,hi_out_2);
	FCLOSE(outF1);
        return 0;
}
