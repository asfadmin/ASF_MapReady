/******************************************************************************
NAME: dopTrend.c - Looks for large jumps in doppler centroid along swath

SYNOPSIS:  dopTrend <infile>

DESCRIPTION:

	Examines the input file (assumed to be created by dopPlot)
   for doppler centroid varaitions.  Attempts to identify bad dopplers
   and reports its findings.  

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    4/98   T. Logan 	Doppler Trending for RAMP products
    1.1	    5/98   T. Logan 	Added call to writePPF to create parameter file
    1.2	    6/98   O. Lawlor 	Added dump to e-mail file.
    1.3	    6/98   O. Lawlor 	Moved dump to writePPF.
    1.4	    7/98   O. Lawlor 	Use doppler gradient instead of linear function.

HARDWARE/SOFTWARE LIMITATIONS:
	Detects and corrects ONLY singleton errors!
	Must have at least 3 images in a swath.
	Must have two frames in a row with proper dopplers!

ALGORITHM DESCRIPTION:

 While input remains
   Read input record
   Check current list for duplicate frame
     if duplicate frame
       extract version number of this product 
       extract version number of stored product
       keep this product only if version is newer
     if no duplicates, put at end of list 

 Sort the items in the list by frame number
 Display the sorted list
 Create forward difference of dopppler centroids along swath
   if difference < prf/2 mark frame as ok else mark to be rechecked
 If a forward search doesn't give 2 good frames; do a reverse search
 If less than two frames are considered good, exit with failure message
 Using only good frames, perform linear regression on swath dopplers

 For each frame in list
     check actual reported doppler versus linear model of dopplers
 	if the abs(difference) > prf / 2
  	  calculate doppler offset in integer PRFs.
 	  report bad frame
 	  write processing parameter file

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*                                                                           *
*   dopTrend -- Examines doppler centroids along swath for inconsistencies  *
*   Copyright (C) 1997  ASF STEP LAB                                        *
*                                                                           *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.                                     *
*                                                                           *
*   This program is distributed in the hope that it will be useful,         *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).                                  *
*                                                                           *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software             *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*                                                                           *
*   ASF STEP LAB Contacts:                                                  *
*       Lab Coordinator   - Rick Guritz         rguritz@images.alaska.edu   *
*       Software Engineer - Tom Logan           tlogan@images.alaska.edu    *
*                                                                           *
*       Alaska SAR Facility                     STEP Lab Web Site:          *
*       Geophysical Institute                   www.images.alaska.edu       *
*       University of Alaska Fairbanks                                      *
*       P.O. Box 757320                                                     *
*       Fairbanks, AK 99775-7320                                            *
*                                                                           *
****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Structure for important information from each frame */
struct img_frame
 {
   char    order[256];	/* directory of file          */
   char    fname[256];  /* file name                  */
   int     frame;	/* frame number               */
   float   doppler;	/* doppler centroid           */
   float   prf;		/* pulse repitition frequency */
   float   pvers;	/* processor version number   */
 } data[256];

/* Framenumber-based image comparsion for quicksort. */
int frame_compare(const struct img_frame *a,const struct img_frame *b)
	{ return a->frame-b->frame; }

/*Prototype:*/
void frames2slope_offset(float *num_vec,float *dop_vec,int nFrames,float *slope,float *offset);

main(int argc,char *argv[])
{
   char	fname[256];			/* Input file name                 */
   FILE *fp;				/* Input file pointer              */
   float dDop[256];			/* Doppler Difference Array        */
   int nFrames;				/* Number of frames we're analysing*/
   int j,k;  				/* COUNTERS                        */
   int tot=0;				/* Total number of bad frames      */
   int offset;				/* calculated doppler offset       */
   int tframe;				/* Temporary frame number          */
   float tdop, tpvers, tprf;		/* Temps for doppler, version, prf */
   char tfname[256];			/* Temporary file name             */
   int  duplicate = 0;			/* Flag for duplicate frames       */
   float x_vec[256], y_vec[256], a, b;	/* x,y vectors for y = a*x + b     */
   float y, diff;			/* computed doppler and difference */
   
   printf("\n\n====================================================\n"); 
   printf("  Doppler Trending Analysis for RAMP data products\n");
   printf("====================================================\n\n"); 
   if (argc != 2) { printf("Usage::  %s infile\n",argv[0]); exit(1); }

   strcpy(fname,argv[1]);
   fp = fopen(fname,"rb");
   if (fp == NULL) {printf("Unable to open File %s\n",fname); exit(1); }

   for (j=0; j< 256; j++) { data[j].prf = 0.0; }

   printf("*Reading input file %s; checking for duplicates\n",fname);
   printf("------------------------------------------------------------\n");
   fscanf(fp,"%i %f %f %f %s",&(data[0].frame),&(data[0].doppler),
	&(data[0].pvers),&(data[0].prf),data[0].fname);

   nFrames = 0;
   while(data[nFrames++].prf != 0.0)
    {
     duplicate = 0;
     if (fscanf(fp,"%i %f %f %f %s",&tframe,&tdop,&tpvers,&tprf,tfname)!=5)
	 break;

     /* Check for duplicate frames
      ----------------------------*/
     for (j=0; j<nFrames; j++)
      {
       if (data[j].frame == tframe)
	 {
	   char *cptr;
	   char tstring[4];
	   int  thisVers, storedVers;

	   printf("    Duplicate frame %i encountered -\n",tframe);

	   /* extract version number of this product */
	   cptr = strrchr(tfname,'R');
           if (cptr==NULL) { printf("Unable to get version number\n"); exit(1);}
	   cptr++;
	   strncpy(tstring,cptr,3);
	   thisVers = atoi(tstring);

	   /* extract version number of stored product */
	   cptr = strrchr(data[j].fname,'R');
           if (cptr==NULL) { printf("Unable to get version number\n"); exit(1);}
	   cptr++;
	   strncpy(tstring,cptr,3);
	   storedVers = atoi(tstring);

 	   /* keep this product only if version is newer */	
	   if (storedVers < thisVers)
	     {
		printf("\t replacing %s with %s\n",data[j].fname,tfname);
   		strcpy(data[j].fname, tfname);
   		data[j].doppler = tdop;
		data[j].prf = tprf;
		data[j].pvers = tpvers;
             }
	   else
	     {  printf("\t keeping %s; discarding %s\n",data[j].fname,tfname); }
	   duplicate++;
	 } /* End if DUPLICATE */
      } /* End for j */

     /* if no duplicates, put at end of list */
     if (!duplicate)
       {
 	data[nFrames].frame = tframe;
	strcpy(data[nFrames].fname, tfname);
   	data[nFrames].doppler = tdop;
	data[nFrames].prf = tprf;
	data[nFrames].pvers = tpvers;
       }
     else { nFrames--; }
    } /* End while data loop */

   printf("\n*Sorting the list by frame numbers\n");
   printf("----------------------------------\n\n");

   /* Sort the items in the list by frame number */
   qsort(data,nFrames,sizeof(struct img_frame),
   	(int (*)(const void *,const void *))frame_compare);

   /* Display the sorted list
    -------------------------*/
   printf("\tNumber\tFrame\tDoppler\n");
   printf("\t--------------------------\n");
   for (j=0;j < nFrames;j++)
	printf("\t%i  \t%i  \t%f\n",j,data[j].frame,data[j].doppler);

/*Fit a linear function to the doppler vs. frame number list.*/
   printf("\n\n*Analyzing doppler differences\n");
   printf("------------------------------\n");
   for (j=0;j<nFrames;j++)
     {
	x_vec[j] = data[j].frame;
   	y_vec[j] = data[j].doppler/data[0].prf;
     } 

   if (nFrames<2) {printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n"
   			  "XXXXX CANNOT VERIFY THIS SEQUENCE !!!! XXXXX\n"
   			  "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");exit(1);}
   else
   	frames2slope_offset(x_vec,y_vec,nFrames,&a,&b);

/*Now figure out which frames are bad, using this fit.*/
   printf("\tNumber\tFrame\tDoppler\n");
   printf("\t--------------------------\n");
   for (j=0;j < nFrames;j++)
     { 
	printf("\t%i  \t%i  ",j,data[j].frame);

	/* Compute what doppler should be */
        y = (a*data[j].frame + b)*data[0].prf;

	/* Find difference with real doppler */
	diff = y - data[j].doppler;

 	/* If the difference is to0 much, report as bad frame */
	if (abs(diff) > data[j].prf/2.0)
	  {
	    if (diff > 0.0)
	     {
	      offset = (int) (diff/data[j].prf + 0.5);
	      printf("\ttoo low (reprocess at +%i PRF",offset);
             }
	    else
             {
	      offset = (int) (diff/data[j].prf - 0.5);
 	      printf("\ttoo high (reprocess at %i PRF",offset);
	     }
 	    printf(" = %f)\n", data[j].doppler+(float)offset*data[j].prf);
	    writeDopplerDelta(data[j].fname,(double)offset);
 	    tot++;
	  }
	else printf("\tok\n");
     }
 
  printf("\n\nRESULTS OF ANALYSIS:\n");
  printf("--------------------\n\n");
  printf("A total of %i frames in this swath have bad dopplers\n\n",tot);

  printf("\n======================================================\n");
  printf("  End of Doppler Trending Analysis for RAMP products\n");
  printf("======================================================\n\n"); 
}


/******************************************************
Frames2slope_offset:
	Fits a linear function to the doppler vs. frame
number list.  This fit is relatively insensitive to 1-PRF
jumps.  All dopplers are in PRF.

	The fit found is:
doppler= slope * frame_no + offset;

Inputs:
	num_vec[]; Frame numbers.
	dop_vec[]; Doppler for each frame, in PRFs.
	nFrames; Number of frames.
Outputs:
	*offset;  Expected doppler at frame 0.
	*slope;  Doppler (in PRF) per frame.
*/

void frames2slope_offset(float *num_vec,float *dop_vec,int nFrames,float *slope,float *offset)
{
	int i;
	double slope_sum;
	double offset_sum;
/*First, we average to find the slope.  This is complicated
by the 1-PRF jumps that may be present in the doppler.*/
	slope_sum=0;
	for (i=1;i<nFrames;i++)
	{
	/* slope_term is the doppler slope between sucessive frames.*/
		float slope_term=dop_vec[i]-dop_vec[i-1];
	/* De-ambiguify a_term, removing any 1-PRF jumps therein.*/
		while (slope_term>0.5) slope_term-=1.0;
		while (slope_term<-0.5) slope_term+=1.0;
	/* Now we just add the slope in.*/
		slope_sum+=slope_term/(num_vec[i]-num_vec[i-1]);
	}
	*slope=slope_sum/(nFrames-1);
	
/*Next, we average to find the offset: we ignore 1-PRF jumps.*/
	offset_sum=0;
	for (i=0;i<nFrames;i++)
	{
	/* slope_term is the doppler slope between sucessive frames.*/
		float offset_term=dop_vec[i]-(*slope)*num_vec[i];
		offset_sum+=offset_term;
	}
	*offset=offset_sum/nFrames;
}

