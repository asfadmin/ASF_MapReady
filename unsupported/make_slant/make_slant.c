/****************************************************************
NAME: 		calibrate

SYNOPSIS:       calibrate [-m] insarfile outsarfile

DESCRIPTION:	Performs radiometric calibration of lo-res or
                full-res ASF SAR images using the radiometric
                data record found in the SAR leader file.

EXTERNAL ASSOCIATES:
get_ifiledr.c      Process image file data record; Extracts size of SAR image.
get_raddr.c        Process radiometric data record; Extract radiometric
		    calibration coefficients.	
get_mpdr.c Process map projection data record; Extract geolocation for images.
xform.c    Transformation from geocoded image line,sample to SSM/I east,north.
ssmill.c   Transformation from SSM/I east,north to geographic lat,lon
	
FILE REFERENCES:
		inSAR 	input SAR image
		outSAR	output SAR image

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
      1     June 27, 92  Unixrconv.c Created by Hiro_soft 
      2     May 14, 1993 unixrconv2.c modifications by Shusun Li
      2.1   Sept 1993    unixrconv2.c modifications by Tom Logan 
      1.0   2/28/94      calibrate.c Optimized and Streamlined 
                         Allowed input from command line
      2.0   4/5/95	 Allow calibration of geocoded images using
			 remapping coefficients from map data record
		 	 (T. Logan)
      2.1   6/19/95	 Fixed bug for samples out of image area (T. Logan)
      2.2   8/22/95      Added code to fix bug with image IDs < 7000.
                         Coeff.'s a1 & a2 need to be recalculated.
      2.3   5/96	 Updated to work with new metadata handlers
      2.4   9/96         Updated for RADARSAT era data file handling
      2.41  5/97	 Make tiny change for uncalibrated RADARSAT data
      			 (no longer bail on uncalibrated).
      2.5   7/97	 Fixed bug resulting from 8/22 bug fix (test for
		         era of file) - T. Logan
      3.0   3/98         T. Logan/ O. Lawlor-- big change: allow calibration of
                         Geocoded scanSAR data.
      3.1   4/98         O. Lawlor-- Eliminate fflush(NULL) for SunOS compatibility.
      3.2   5/98         O. Lawlor-- Calibrate AT/CT (georeferenced) ground range ScanSAR.
      3.3   6/98         O. Lawlor-- AT/CT bug fix.
      3.5   6/98         O. Lawlor-- Other Projection bug fixes (lambert still doesn't work right).
      4.0   12/98        O. Lawlor-- Uses asf_meta routines; vastly simplified.

ALGORITHM DESCRIPTION:
	(1) Read and Verify the calibration coefficients and LUT
	(2) Determine size and format of SAR image
	(3) Keep Header Record
	(4) For each line of the inSAR
   	    (4.1) Copy prefix bytes to obuff
   	    (4.2) For each pixel in this image line
       		(4.2.1) Convert ibuff DN value to Sigma-0 (dB)
       		(4.2.2) Convert Sigma-0 to Byte quantity in obuff
   	    (4.3) Write obuff to Output File
		
	Once the original sample is known, the approriate correction
	factor is accessed and used.
****************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.

This software was authored by:

Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu

NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include "asf.h"
#include "ddr.h"
#include "ceos_io.h"
#include "asf_meta.h"

/* constants */
#define VERSION 4.0


int main(int argc, char **argv)
{
	char *inSAR,*outLAS;
	int	*ibuff;
	unsigned char *obuff1,*obuff2,*obuff3;	/* Input and output buffers	*/
	CEOS_FILE   * fpIn;
	FILE *fpOut1,*fpOut2,*fpOut3;		/* input and output file ptrs	*/
	meta_parameters *meta;/*Calibration coefficients, etc.*/
	int	x,y; 	/* loop counters */
	int	ns, nl,ons,onl;		/* number lines and samples in input and output	*/
	struct DDR outddr;
	int step=1;
	
#define MAX_tableRes 512 /*Num. of noise table entries, across one image line.*/
	double noise_table[MAX_tableRes];/*Table of noise vs. x pixel.*/
	int tableRes=MAX_tableRes;

	StartWatch();
	if (argc < 3 ) {
		printf("Usage: %s inSAR out [ -s step ]\n", argv[0]);
		printf("       Creates byte slant/time/doppler images,\n"
			"downsampled by a factor of step x step (default: no downsizing).\n"
			"The iamges are titled out_slant (1 fringe/10km slant)\n"
			"out_time (1 fringe/sec) and out_dop(1 fringe/1000Hz doppler)\n");
		printf("\n"
  		"       Version %.2f,  ASF STEP TOOLS\n", VERSION);
		exit(1);
	}
	
	inSAR=argv[1];
	outLAS=argv[2];
	
	if (argc>4)
		sscanf(argv[4],"%d",&step);

/*Open input files, create output files.*/
	fpIn=fopenCeos(inSAR);
	nl=fpIn->ddr.nl;
	ns=fpIn->ddr.ns;
	meta=meta_init(inSAR);
	
	fpOut1=fopenImage(appendExt(outLAS,"_slant"),"wb");
	fpOut2=fopenImage(appendExt(outLAS,"_time"),"wb");
	fpOut3=fopenImage(appendExt(outLAS,"_dop"),"wb");
	outddr=fpIn->ddr;
	ons=outddr.ns=ns/step;
	onl=outddr.nl=nl/step;
	outddr.dtype=DTYPE_BYTE;
	c_putddr(appendExt(outLAS,"_slant"),&outddr);
	c_putddr(appendExt(outLAS,"_time"),&outddr);
	c_putddr(appendExt(outLAS,"_dop"),&outddr);
	
	printf("Output-Sigma0-FILE : %s\n", outLAS);

	printf("Input nl=%i, ns=%i; output nl=%i, ns=%i\n", nl, ns,onl,ons);

	/* allocate appropriate buffer space */
	ibuff = (int *) MALLOC(ns * sizeof(int));
	obuff1 = (unsigned char *) MALLOC(ons * sizeof(unsigned char));
	obuff2 = (unsigned char *) MALLOC(ons * sizeof(unsigned char));
	obuff3 = (unsigned char *) MALLOC(ons * sizeof(unsigned char));

	/* Read input file, convert, and write to output file  */
	for (y = 0; y < onl; y++) 
	{
		
		/*Read image data.*/
		readCeosLine(ibuff,step*y,fpIn);
		
		/*Convert data.*/
		for (x = 0; x < ons; x++)
		if (ibuff[x*step])
		{
			double time,slant,dop;
			
			meta_get_timeSlantDop(meta,step*y,step*x,&time,&slant,&dop);
			
			obuff1[x]=slant/10000.0*256.0;
			obuff2[x]=time/1.0*256.0;
			obuff3[x]=dop/1000.0*256.0;
		}
		else 
			obuff1[x]=obuff2[x]=obuff3[x]=0;
		
		/*Write it out.*/
		FWRITE(obuff1, ons, 1, fpOut1);
		FWRITE(obuff2, ons, 1, fpOut2);
		FWRITE(obuff3, ons, 1, fpOut3);
		
		if ((y % 100) == 0) 
			printf(" Now Processing Line No = %d \n", y);
	}
	printf("Wrote %i lines of %i samples\n", onl, ons);
	closeCeos(fpIn);
	
	printf("\7\n\n Calibration is complete! \n\n");
	StopWatch();

	return(0);
}

