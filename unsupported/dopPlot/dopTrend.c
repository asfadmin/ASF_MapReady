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

 
main(int argc,char *argv[])
{
   char	fname[256];			/* Input file name                 */
   FILE *fp;				/* Input file pointer              */
   float dDop[256];			/* Doppler Difference Array        */
   int i,j,k;				/* COUNTERS                        */
   int tot=0;				/* Total number of bad frames      */
   int offset;				/* calculated doppler offset       */
   int checkFrame[256];			/* Flag for possible bad frame     */
   int badFrame[256];			/* Flag for determined bad frame   */
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

   for (i=0; i< 256; i++) { data[i].prf = 0.0; }

   printf("*Reading input file %s; checking for duplicates\n",fname);
   printf("------------------------------------------------------------\n");
   fscanf(fp,"%i %f %f %f %s",&(data[0].frame),&(data[0].doppler),
	&(data[0].pvers),&(data[0].prf),data[0].fname);

   i = 0;
   while(data[i++].prf != 0.0)
    {
     duplicate = 0;
     if (fscanf(fp,"%i %f %f %f %s",&tframe,&tdop,&tpvers,&tprf,tfname)!=5)
	 break;

     /* Check for duplicate frames
      ----------------------------*/
     for (j=0; j<i; j++)
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
 	data[i].frame = tframe;
	strcpy(data[i].fname, tfname);
   	data[i].doppler = tdop;
	data[i].prf = tprf;
	data[i].pvers = tpvers;
       }
     else { i--; }
    } /* End while data loop */

   printf("\n*Sorting the list by frame numbers\n");
   printf("----------------------------------\n\n");

   /* Sort the items in the list by frame number */
   for (j = 1; j < i; j++)
    {
      if (data[j].frame < data[j-1].frame)
	{
	  tframe = data[j].frame; tdop = data[j].doppler; tpvers=data[j].pvers;
	  strcpy(tfname,data[j].fname); tprf = data[j].prf;

	  k=j-1;
	  while(tframe < data[k].frame)
           {
	     data[k+1].frame = data[k].frame;
	     data[k+1].doppler = data[k].doppler;
	     data[k+1].pvers = data[k].pvers;		 
	     data[k+1].prf = data[k].prf;
	     strcpy(data[k+1].fname,data[k].fname);
	     k--;
	     if (k==0) break;
           }

	  data[k+1].frame = tframe;
	  data[k+1].doppler = tdop;
	  data[k+1].pvers = tpvers;		 
	  data[k+1].prf = tprf;
	  strcpy(data[k+1].fname,tfname);
	}
    }


   /* Display the sorted list
    -------------------------*/
   j = 0;
   printf("\tNumber\tFrame\tDoppler\n");
   printf("\t--------------------------\n");
   while (j < i)
     { 
	printf("\t%i  \t%i  \t%f\n",j,data[j].frame,data[j].doppler);
	j++;
     }

   printf("\n\n*Analyzing doppler differences\n");
   printf("------------------------------\n");
   j = 1;
   k = 0;
   while (j<i)
     {
 	dDop[j] = data[j].doppler - data[j-1].doppler;
	if (fabs(dDop[j]) > data[j].prf/2.0)
	  {
 	     if (j==1) checkFrame[0] = 1;
	     checkFrame[j] = 1;
          }
	else
	  {
	     checkFrame[j] = 0;	
	     if (j==1)
	      {
		x_vec[k] = data[0].frame;
   	        y_vec[k] = data[0].doppler;
		k++;
	      }
  	     x_vec[k] = data[j].frame;
   	     y_vec[k] = data[j].doppler;
             k++;
          }
        j++;
     } 


   /* if a forward search wasn't enough; do a reverse search 
    -------------------------------------------------------*/
   if (k<2)
    {
     j = 0;
     printf("\n*Analyzing reverse doppler differences\n");
     printf("--------------------------------------\n");
     while (j<i-1)
      {
       if (checkFrame[j])
         {
  	   dDop[j] = data[j+1].doppler - data[j].doppler;
           if (fabs(dDop[j]) > data[j].prf/2.0) { badFrame[j] = 1; }
           else {
  	     badFrame[j] = 0;
    	     x_vec[k] = data[j].frame;
     	     y_vec[k] = data[j].doppler;
             k++;
           }
         }  	  	
       j++;
      }
    } /* if k<2 */


   if (k>1) yaxb(x_vec,y_vec,k,&a,&b);
   else { printf("Unable to verify this doppler/frame sequence\n"); exit(1); }

   j = 0;
   printf("\tNumber\tFrame\tDoppler\n");
   printf("\t--------------------------\n");
   while (j < i)
     { 
	printf("\t%i  \t%i  ",j,data[j].frame);

	/* Compute what doppler should be */
        y = a*data[j].frame + b;

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
        j++;
     }
 
  printf("\n\nRESULTS OF ANALYSIS:\n");
  printf("--------------------\n\n");
  printf("A total of %i frames in this swath have bad dopplers\n\n",tot);

  printf("\n======================================================\n");
  printf("  End of Doppler Trending Analysis for RAMP products\n");
  printf("======================================================\n\n"); 
}

