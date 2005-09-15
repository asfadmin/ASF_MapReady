/*
    NAME: gen_filt_params

    SYNOPSIS: gen_filt_params <cpx image 1> <cpx image 2> <filt_params>

    DESCRIPTION:
	This program figures out what the filter parameters need to be to automate the slc_filtering routine.
	
    AUTHOR:
	M. Ayers 5-2000
*/

#include <math.h>
#include "asf.h"
#include "aisp_defs.h"
#include "ddr.h"
#include "asf_meta.h"

#define fftLen 2048	/* this is the default FFT length from the cpx_spectrum program */
#define thresh 0.1	/* set the threshold to determine which frequencies we will be zeroing currently 5% of max amplitude */
#define VERSION 1.10

main(int argc,char **argv)
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
	if(argc != 4)
	{
		printf("\n%s <cpx image 1> <cpx image 2> <filt_params>\n\n",argv[0]);
		printf("This program uses the SAR metadata and the spectra generated\n");
		printf("by cpx_spectrum to estimate the appropriate filtering frequencies\n");
		printf("for coherence improvement\n");
		printf("Version %.2f ASF IFSAR Tools 2000\n",VERSION);
		exit(0);
	}
	
	StartWatch();

/* Get the filenames from the command line */ 
	infile1=argv[1];
	infile2=argv[2];
	outfile=argv[3];

/* Read the meta files in from the images */
	strcpy(specFile1, infile1);
	strcpy(specFile2, infile2);

	meta1=meta_read(infile1);
	meta2=meta_read(infile2);

	azDop1=meta1->geo->dopAz;
	azDop2=meta2->geo->dopAz;

	printf("%s Image Doppler is %f\n",infile1,azDop1[0]);
	printf("%s Image Doppler is %f\n\n",infile2,azDop2[0]);
	
	freq_mod=-(azDop1[0]+azDop2[0])/2;
	f_s=1/(meta1->geo->azPixTime);
	printf("The sampling frequency is %f Hz\n",f_s);
	printf("The time domain shift needs to be %f Hz\n\n",freq_mod);

/* Read in the frequency and ampitude information from the .spectra files */
	
	strcat(specFile1,".spectra");
	strcat(specFile2,".spectra");
	

	if((inSpectra1=FOPEN(specFile1,"r"))==NULL)
	{
		printf("Error! Couldn't open %s\n",infile1);
		exit(1);
	}


	if((inSpectra2=FOPEN(specFile2,"r"))==NULL) 
        { 
                printf("Error! Couldn't open %s\n",infile2);
                exit(1);
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
                exit(1);
        }
	
	fprintf(outF1,"%f\n%f\n%f %f\n%f %f",f_s,freq_mod,lo_out_1,hi_out_1,lo_out_2,hi_out_2);
	FCLOSE(outF1);

		
	
}
