/*****************************************************************************
NAME: 	dem_diff - creates a difference image for 2 DEM files.

SYNOPSIS:	dem_diff <insar_dem> <ref_dem> <output_diff>

DESCRIPTION:


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/00   T. Logan     Check accuracy of INSAR created DEMs

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   dem_diff.c - creates a difference image of two DEMs                     *
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
#include "asf.h"
#include "cproj.h"
#include "proj.h"                   
#include "asf_meta.h"
#include "libasf_proj.h"                             

#define  VERSION 0.0
#define  DPR    57.2957795 		/* Degrees per Radian */
#define HIST_SIZE       10000		/* Values from -5000 to 4999 */
#define MIN(a,b)        ((a)<(b) ? (a) : (b))
#define MAX(a,b)        ((a)<(b) ? (b) : (a))

short int getRefValue(int rline,int rsamp, int sample_count);

FILE *fpRefIn;

int main(int argc,char *argv[])
{
	meta_parameters *metaSource, *metaTarget;
	char  	inName[256];
	char 	inRefName[256];
	char  	outName[256];
	int   	line, samp;
	FILE  	*fpIn, *fpOut;
	//inverse_transform proj2Latlon[100];/* function array for projection   */            
	//forward_transform LatLon2Proj[100];/* function array for projection   */            
	float   *inBuf;
	float   *outBuf;
	char 	command[256];
	double 	proj_y, proj_x;
	int	i,j,sample_count;

        float   average;                                /* Average Correlation */
        double  hist_sum = 0.0;                         /* Histogram sum */
	int	hist[HIST_SIZE];			/* Histogram of data */
        long long hist_cnt = 0;                         /* Histogram count */
	float   percent_sum = 0.0;
	float   ms_sum = 0.0;				/* Mean-Squared offset sum */

	float  min = 1000000.0;
	float  max = -1000000.0;

	if (argc != 4)
	  {
		printf("Usage: %s insar_dem ref_dem output_diff\n",argv[0]);
		printf("    Inputs:\n");
		printf("\tinsar_dem	Insar created DEM file (geocoded, float)\n");
		printf("\tref_dem	Reference DEM file (geocoded, short int)\n");
		printf("    Outputs:\n");
		printf("\toutput_diff   Difference file, Insar - Ref (float)\n\n");
		printf("    Creates a difference image from two DEM files\n\n");
		printf("Version %.2f, ASF SAR TOOLS\n",VERSION);
		exit(1);
	  }

	StartWatch();

	strcpy(inName,argv[1]);
	strcpy(inRefName,argv[2]);
	strcpy(outName,argv[3]);

	printf("\nDEM Differencing program\n\n");
	printf("Subtracting file %s from %s to create %s\n",inRefName,inName,outName);

	for (i=0; i<HIST_SIZE; i++) hist[i] = 0;

	/* Copy .ddr file for output */
	sprintf(command,"cp %s.ddr %s.ddr",inName,outName);
	system(command);

	/* Read ddr files 
	if (c_getddr(inName,&ddr) != 0)
          {
            printf("Error returned from c_getddr:  unable to read file %s\n",inName);
            exit(1);
          }

	if (c_getddr(inRefName,&ddr2) != 0)
          {
            printf("Error returned from c_getddr:  unable to read file %s\n",inRefName);
            exit(1);
	    }*/
	metaSource = meta_read(inName);
	metaTarget = meta_read(inRefName);
	sample_count = metaSource->general->sample_count;

	/* Initialize transformation package *
	inv_init(ddr.proj_code, ddr.zone_code, ddr.proj_coef, ddr.datum_code,
		NULL, NULL, &iflag, proj2Latlon);

	for_init(ddr2.proj_code, ddr2.zone_code, ddr2.proj_coef, ddr2.datum_code,
	NULL, NULL, &iflag, LatLon2Proj);*/

	inBuf = (float *) MALLOC (sizeof(float) * sample_count);
	outBuf = (float *) MALLOC (sizeof(float) * sample_count);

	strcat(inName,".img");
	strcat(inRefName,".img");
	strcat(outName,".img");
	fpIn = FOPEN(inName,"rb");
	fpRefIn = FOPEN(inRefName,"rb");
	fpOut = FOPEN(outName,"wb");

	/* Loop through each line of the input file */
	proj_y = metaSource->projection->startY;
	for (line = 0; line < metaSource->general->line_count; 
	     line++, proj_y+=metaSource->projection->perY)
	  {
		double		lat, lon, height;
		double		rproj_x, rproj_y, rproj_z;
		int		rline, rsamp;
		short int	refVal;

		proj_x = metaSource->projection->startX;
		FREAD(inBuf, sample_count*sizeof(float),1, fpIn);

		/* For each sample in this line */
		for (samp = 0; samp < sample_count; 
		     samp++,proj_x += metaSource->projection->perX)
		  {
		    if (inBuf[samp] != 0.0)
		      {	
			/* Convert the projection x,y to geographic lat,lon */
			proj_to_latlon(metaSource->projection, 'R', proj_x, proj_y, 0.0, 
				       &lon, &lat, &height); 

			/* Convert the lat,lon to Ref projection */
			latlon_to_proj(metaTarget->projection, 'R', lon, lat, 0.0,
				       &rproj_x, &rproj_y, &rproj_z); 

			/* Determine line,sample in Ref dem */
			rline = (int) ((fabs(metaTarget->projection->startY - rproj_y))/
				       -metaTarget->projection->perY +0.5);
			rsamp = (int) ((fabs(metaTarget->projection->startX - rproj_x))/
				       metaTarget->projection->perX +0.5);

			/* Get the value from the reference DEM */
			refVal = getRefValue(rline, rsamp, metaTarget->general->sample_count);

			/* Set the output pixel value */
			outBuf[samp] = inBuf[samp] - refVal;

			/*
			printf("%i, %i -> %lf, %lf -> ",line,samp,proj_y,proj_x); fflush(NULL);
			printf("%lf, %lf ->",lon*DPR, lat*DPR); fflush(NULL);
			printf("%lf, %lf ->",rproj_y,rproj_x); fflush(NULL);
			printf("%i, %i ",rline,rsamp); fflush(NULL);
			printf(" %f - %i = %f\n",inBuf[samp],refVal,outBuf[samp]); fflush(NULL);
			*/
		      }
		    else outBuf[samp] = 0.0;
		  }

		/* Write modified data to output file */
		FWRITE(outBuf, sample_count*sizeof(float),1, fpOut);

		/* Calculate statistics for this line */
		for (samp = 0; samp < sample_count; samp++)
		  {
			if (outBuf[samp]!=0.0)
			  {
			    int bin;

			    hist_sum += outBuf[samp];
			    hist_cnt++;
			    min = MIN(outBuf[samp],min);
			    max = MAX(outBuf[samp],max);

			    bin = (HIST_SIZE/2) + (int) (outBuf[samp]+0.5);
			    if (bin >= HIST_SIZE)
			      { 
				bin = HIST_SIZE - 1; 
				printf("Value %i out of histogram range, set to %i\n",
					(int) (outBuf[samp]+0.5), bin - (HIST_SIZE/2));
			      }
			    else if (bin < 0)
			     {
				bin = 0;
				printf("Value %i out of histogram range, set to %i\n",
					(int) (outBuf[samp]+0.5), bin - (HIST_SIZE/2));
			     }
			    hist[bin]++;	
			  }
		  }


		asfLineMeter(line, metaSource->general->line_count);
	  }

	FREE(inBuf);
	FREE(outBuf);

	average = (float)hist_sum/(float)hist_cnt;      

	printf("Histogram of Data:\n");

	for (i=0; i<HIST_SIZE; i+=10)
	  {
            int tmp_sum = 0; 	
	    int bin_low, bin_high;
	    float percent;

	    for (j=0; j<10; j++) { tmp_sum += hist[i+j]; }

	    bin_low = i - (HIST_SIZE/2);
	    bin_high = bin_low+j-1;

	    percent = (float) tmp_sum / (float) hist_cnt;
	    percent_sum += (float) 100 * percent;

	    if (tmp_sum) 
	      printf(" %.4i -> %.4i :\t%i\t%2.3f \n",bin_low,bin_high,tmp_sum,100*percent);
	  }


	for (i = 0; i<HIST_SIZE; i++)
	  {
	     if (hist[i])
	      {
	        float ms;
	     	ms = (float) (i-HIST_SIZE/2) - average;
	     	ms_sum += ms*ms*hist[i];
	      }
	  }

	printf("\n");
	printf("Average Offset : %f\n",average);
	printf("RMS Difference : %f\n",sqrt(ms_sum/(float)hist_cnt));
	printf("Minimum Offset : %f\n",min);
	printf("Maximum Offset : %f\n",max);
	printf("\n");

	printf("Program Completed\n");
 	StopWatch();	
	exit(0);
}

#define	bufSize 512

short int getRefValue(int rline,int rsamp, int sample_count)
 {
    	long long   	file_offset;
	short int   	refVal;
	static short int 	*dataValues=NULL;
	static int		start_line = -1;	
	int			line_off;
	
	if (!dataValues) 
	  {
		dataValues = (short int *) MALLOC (sizeof(short int) * sample_count * bufSize); 
 	  }

	/* If we don't have the data in the buffer */
	if (rline < start_line || rline >= start_line+bufSize)
	  {
	    file_offset = (long long) ((rline-bufSize/2)*sample_count*2);
	    if (file_offset < 0)
	      {
		printf("Illegal file offset %lli (line %i, sample %i)\n",file_offset, rline, rsamp);
		exit(1);
	      }
	    FSEEK64(fpRefIn,file_offset,SEEK_SET);
	    FREAD(dataValues,bufSize*sample_count*sizeof(short int),1,fpRefIn);
	    start_line = rline-bufSize/2;
	  }
	line_off = rline - start_line;
	refVal = dataValues[line_off*sample_count+rsamp];
	return(refVal);
 }
