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
*	Alaska SAR Facility			STEP Lab Web Site:	    *	
*	Geophysical Institute			www.images.alaska.edu	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/
#include "asf.h"
#include "ddr.h"
#include "cproj.h"
#include "proj.h"                   
#include "asf_meta.h"                             

#define  VERSION 0.0
#define  DPR    57.2957795 		/* Degrees per Radian */
#define HIST_SIZE       10000		/* Values from -5000 to 4999 */
#define MIN(a,b)        ((a)<(b) ? (a) : (b))
#define MAX(a,b)        ((a)<(b) ? (b) : (a))

short int getRefValue(int rline,int rsamp, struct DDR *refddr);

FILE *fpRefIn;

main(int argc,char *argv[])
{
	struct DDR ddr;
	struct DDR ddr2;
	char  	inName[256];
	char 	inRefName[256];
	char  	outName[256];
	int   	line, samp;
	FILE  	*fpIn, *fpOut;
	inverse_transform proj2Latlon[100];/* function array for projection   */            
	forward_transform LatLon2Proj[100];/* function array for projection   */            
	float   *inBuf;
	float   *outBuf;
	char 	command[256];
	int 	iflag = 0;
	float 	proj_y, proj_x;
	int	i,j;

        float   average;                                /* Average Correlation */
        double  hist_sum = 0.0;                         /* Histogram sum */
	int	hist[HIST_SIZE];			/* Histogram of data */
        long long hist_cnt = 0;                         /* Histogram count */
	float   percent_sum = 0.0;
	float   ms_sum = 0.0;				/* Mean-Squared offset sum */

	float  min = 1000000.0;
	float  max = -1000000.0;

	int	next_print=5;

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

	/* Read ddr files */
	if (c_getddr(inName,&ddr) != 0)
          {
            printf("Error returned from c_getddr:  unable to read file %s\n",inName);
            exit(1);
          }

	if (c_getddr(inRefName,&ddr2) != 0)
          {
            printf("Error returned from c_getddr:  unable to read file %s\n",inRefName);
            exit(1);
          }

	/* Initialize transformation package */
	inv_init(ddr.proj_code, ddr.zone_code, ddr.proj_coef, ddr.datum_code,
		NULL, NULL, &iflag, proj2Latlon);

	for_init(ddr2.proj_code, ddr2.zone_code, ddr2.proj_coef, ddr2.datum_code,
		NULL, NULL, &iflag, LatLon2Proj);

	inBuf = (float *) MALLOC (sizeof(float) * ddr.ns);
	outBuf = (float *) MALLOC (sizeof(float) * ddr.ns);

	strcat(inName,".img");
	strcat(inRefName,".img");
	strcat(outName,".img");
	fpIn = FOPEN(inName,"rb");
	fpRefIn = FOPEN(inRefName,"rb");
	fpOut = FOPEN(outName,"wb");

	/* Loop through each line of the input file */
	proj_y = ddr.upleft[0];
	for (line = 0; line < ddr.nl; line++, proj_y-=ddr.pdist_y)
	  {
		double		lat, lon;
		double		rproj_x, rproj_y;
		int		rline, rsamp;
		short int	refVal;

		proj_x = ddr.upleft[1];
		FREAD(inBuf, ddr.ns*sizeof(float),1, fpIn);

		/* For each sample in this line */
		for (samp = 0; samp < ddr.ns; samp++,proj_x += ddr.pdist_x)
		  {
		    if (inBuf[samp] != 0.0)
		      {	
			/* Convert the projection x,y to geographic lat,lon */
			proj2Latlon[ddr.proj_code](proj_x,proj_y,&lon,&lat); 

			/* Convert the lat,lon to Ref projection */
			LatLon2Proj[ddr2.proj_code](lon,lat,&rproj_x,&rproj_y); 

			/* Determine line,sample in Ref dem */
			rline = (int) ((fabs(ddr2.upleft[0] - rproj_y))/ddr2.pdist_y +0.5);
			rsamp = (int) ((fabs(ddr2.upleft[1] - rproj_x))/ddr2.pdist_x +0.5);

			/* Get the value from the reference DEM */
			refVal = getRefValue(rline, rsamp, &ddr2);

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
		FWRITE(outBuf, ddr.ns*sizeof(float),1, fpOut);

		/* Calculate statistics for this line */
		for (samp = 0; samp < ddr.ns; samp++)
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


		if ((int)(100.0*line/ddr.nl)>next_print) 
		  {
			printf("\tCompleted %.2i percent\n",next_print);
			next_print += 5;
		  }
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

	    if (tmp_sum) printf(" %.4i -> %.4i :\t%i\t%2.3f \n",bin_low,bin_high,tmp_sum,100*percent);
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

short int getRefValue(int rline,int rsamp, struct DDR *refddr)
 {
    	long long   	file_offset;
	short int   	refVal;
	static short int 	*dataValues=NULL;
	static int		start_line = -1;	
	int			line_off;
	
	if (!dataValues) 
	  {
		dataValues = (short int *) MALLOC (sizeof(short int) * refddr->ns * bufSize); 
 	  }

	/* If we don't have the data in the buffer */
	if (rline < start_line || rline >= start_line+bufSize)
	  {
	    file_offset = (long long) ((rline-bufSize/2)*refddr->ns*2);
	    if (file_offset < 0)
	      {
		printf("Illegal file offset %lli (line %i, sample %i)\n",file_offset, rline, rsamp);
		exit(1);
	      }
	    FSEEK64(fpRefIn,file_offset,SEEK_SET);
	    FREAD(dataValues,bufSize*refddr->ns*sizeof(short int),1,fpRefIn);
	    start_line = rline-bufSize/2;
	  }
	line_off = rline - start_line;
	refVal = dataValues[line_off*refddr->ns+rsamp];
	return(refVal);
 }
