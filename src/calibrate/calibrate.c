/****************************************************************
NAME: 		calibrate

SYNOPSIS:       calibrate insarfile outsarfile [-m] [-g]
   			  
			  inSARfile 	input SAR image
			  outSARfile	output LAS image

DESCRIPTION:	Performs radiometric calibration of ASF SAR images

EXTERNAL ASSOCIATES:
	NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
	NAME:               USAGE:
    ---------------------------------------------------------------
	
PROGRAM HISTORY:
 VERS:   DATE:  AUTHOR:    PURPOSE:
 --------------------------------------------------------------
   1     6/92   Hiro_soft  Unixrconv.c Created
   2     5/93   Shusun Li  unixrconv2.c modifications
   2.1   9/93   Tom Logan  unixrconv2.c modifications 
   1.0   2/94   T. Logan   calibrate.c Optimized and Streamlined 
                             Allowed input from command line
   2.0   4/95   T. Logan   Allow calibration of geocoded images using remapping 
                             coefficients from map data record
   2.1   6/95   T. Logan   Fixed bug for samples out of image area
   2.2   8/95              Added code to fix bug with image IDs < 7000.
                           Coeff.'s a1 & a2 need to be recalculated.
   2.3   5/96              Updated to work with new metadata handlers
   2.4   9/96              Updated for RADARSAT era data file handling
   2.41  5/97              Made tiny change for uncalibrated RADARSAT data
                            (no longer bail on uncalibrated).
   2.5   7/97   T. Logan   Fixed bug from 8/22 bug fix (test for era of file)
   3.0   3/98   Logan / Lawlor
                           Allow calibration of geocoded scanSAR data
   3.1   4/98   O. Lawlor  Eliminate fflush(NULL) for SunOS compatibility.
   3.2   5/98   O. Lawlor  Calibrate AT/CT ground range ScanSAR.
   3.3   6/98   O. Lawlor  AT/CT bug fix.
   3.5   6/98   O. Lawlor  Other Projection fixes (lambert still isn't right).
   4.0  12/98   O. Lawlor  Uses asf_meta routines; vastly simplified.
   4.1   1/99   T. Logan   Added hardcoded noise vectors for ScanSAR
                             Added gamma-0 calculation 
   4.2   6/01   P. Denny   Added check_cal call to check if image is calibrated
   4.5   2/02   P. Denny   Updated commandline parsing; update to meta v1.1
   5.0  10/02   J. Nicoll  Updated to deal with SWB antenna pattern 16.
   5.1   2/04   P. Denny   Use put_data_lines instead of FWRITE

ALGORITHM DESCRIPTION:

******************************************************************************/

/******************************************************************************
*                                                                             *
*Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks    *
*All rights reserved.                                                         *
*                                                                             *
*Redistribution and use in source and binary forms, with or without           *
*modification, are permitted provided that the following conditions are met:  *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
*THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  *
*AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    *
*IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   *
*ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     *
*LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          *
*CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         *
*SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     *
*INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      *
*CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      *
*ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   *
*POSSIBILITY OF SUCH DAMAGE.                                                  *
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

#include <stdlib.h>

#include "asf.h"
#include "asf_meta.h"
#include "calibrate.h"
#include "ceos_io.h"

/* constants */
#define VERSION 5.1

/* Prototypes */
int get_cal_dn(cal_params*,double,double,int);
int check_cal(char*);
void usage(char*);


#define MAX_tableRes 512 /*Num. of noise table entries, across one image line.*/
int put_data_lines(FILE *file, meta_parameters *meta, int line_number,
                   int num_lines_to_write, const void *buffer, int data_type);

int main(int argc, char **argv)
{
	char 	      *inSAR,	/* Input image name */
		      *outLAS;	/* Output image name */
	int	      *ibuff;
	unsigned char *obuff;	/* Input and output buffers	*/
	CEOS_FILE     *fpIn;
	FILE          *fpOut;	/* output file pointers	*/
	cal_params    *p;	/* Calibration coefficients, etc.*/
	int	      x,y; 	/* loop counters */ 
        int	      ns, nl;	/* num lines and samples in input and output */
	int           override_flag=0,gamma_flag=0;
	double noise_table[MAX_tableRes];/*Table of noise vs. x pixel.*/
	int tableRes=MAX_tableRes;
	double incidence[MAX_tableRes];  /* Table of incidence vs pixel */
	extern int currArg; /* from cla.h which is in asf.h */

	StartWatch();
	/* Parse command line args */
	while (currArg < (argc-2))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-m")) {
			override_flag=1;
		}
		else if (strmatch(key,"-g")) {
			gamma_flag=1;
		}
		else {printf("\n**Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < 2) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	
	inSAR=argv[currArg++];
	outLAS=argv[currArg];
	
	check_cal(inSAR);	/* Make sure file is calibrated */

        /* Open input files, create output files.  */
	fpIn=fopenCeos(inSAR);
	nl = fpIn->meta->general->line_count;
	ns = fpIn->meta->general->sample_count;
	
	fpOut=fopenImage(outLAS,"wb");
	fpIn->meta->general->data_type = BYTE;
	meta_write(fpIn->meta,outLAS);
	
	printf("Output-Sigma0-FILE : %s\n", outLAS);

	p=create_cal_params(inSAR);
	if (NULL==p) bail("Error fetching calibration parameters. Exiting..\n");

	if (override_flag) modify_cal_params(p);
	if (gamma_flag)
	{
		p->output_type=gamma_naught;
		printf("Creating gamma-0 output\n");
	}

        /*  Switch on the image size to determine the appropriate
          length of the noise table-- the trade-off here is between
          speed (few table updates) and quality (accurate noise values).*/

	if (nl<1500) tableRes=128;
	else if (nl<3000) tableRes=256;
	
	printf("Input and output nl=%i, ns=%i\n", nl, ns);

	/* allocate appropriate buffer space */
	ibuff = (int *) MALLOC(ns * sizeof(int));
	obuff = (unsigned char *) MALLOC(ns * sizeof(unsigned char));

	printf("\n");
	/* Read input file, convert, and write to output file  */
	for (y = 0; y < nl; y++ ) 
	  {
		/*# pixels per table entry.*/
		int tablePix=((ns+(tableRes-1))/tableRes);
		
		/*Read image data.*/
		readCeosLine(ibuff,y,fpIn);
		
		/*Allocate noise table entries and/or update if needed.*/
		if (y==0 || (y%(nl/tableRes)==0 && p->noise_type!=by_pixel))
		  	for (x=0;x<tableRes;x++)
				noise_table[x]=get_noise(p,x*tablePix,y);

		/*Allocate incidence table entries or update.*/
	 	if (p->output_type==gamma_naught)
                  {
		    if (y==0 || (y%(nl/tableRes)==0 && p->noise_type!=by_pixel))
		  	for (x=0;x<tableRes;x++)
			        incidence[x]=get_invCosIncAngle(p,x*tablePix,y);
                  }

	        /*Convert data.*/
		for (x = 0; x < ns; x++)
		  if (ibuff[x])
		   {  /*Interpolate noise table to find this pixel's noise.*/
			double index=(float)x/tablePix;
			int    base=(int)index;
			double frac=index-base;
			double noise=noise_table[base]+
			          frac*(noise_table[base+1]-noise_table[base]);
			double incid;
			if (p->output_type==gamma_naught)
				incid=incidence[base]+
			          frac*(incidence[base+1]-incidence[base]);
			obuff[x]=get_cal_dn(p,noise,incid,ibuff[x]);
		   }
		  else obuff[x]=0;
		
		/*Write it out.*/
		put_data_lines(fpOut, fpIn->meta, y, 1, obuff,
		               fpIn->meta->general->data_type);

		if ((y%100)==0) {
			printf(" Now Processing Line: %d\r", y);
			fflush(NULL);
		}
	  }
	printf("Number of lines processed: %d.                \n", y);
	printf("Wrote %i lines of %i samples\n", nl, ns);
	closeCeos(fpIn);
	FCLOSE(fpOut);
	
	printf("\n Calibration is complete!\n\n");
	StopWatch();

	meta_free(fpIn->meta);
	exit(EXIT_SUCCESS);
}

/*----------------------------------------------------------------------
  Get_cal_dn:
	Convert amplitude image data number into
	calibrated image data number, given the current
	noise value.
----------------------------------------------------------------------*/
int get_cal_dn(cal_params *p,double noiseValue,double invCosIncAngle,int inDn)
{
	double scaledPower,sigma0;
	double Dmin=p->Dmin,Dmax=p->Dmax;
	
	/*Convert (amplitude) data number to scaled, noise-removed power*/
	scaledPower=p->a1*((float)inDn*inDn-p->a0*noiseValue) + p->a2;

	/*Apply gamma-0 correction if needed*/
	if (p->output_type==gamma_naught) scaledPower *= invCosIncAngle;

	/*Convert power to sigma-0 */
	if (1.0<=scaledPower)
		return 255;/*Sigma-0 would be greater than 0.*/
	if (scaledPower > 0.0) 
	{
		sigma0=10.0*log10(scaledPower);
		if (sigma0 > Dmin) 
			return (int)((sigma0 - Dmin) /(Dmax - Dmin)*255.0+0.5);
	}
	/*Otherwise, sigma-0 is too small*/
	return 0;/*Otherwise, return 0.*/
}

/*----------------------------------------------------------------------
check_cal
  Checks calibration status field of the data quality summary record.
  If the data is not calibrated, a message is displayed and 0 is returned.
  If the data has inferred calibration, a message is displayed and 1 is
  returned.  Otherwise 1 is returned.
----------------------------------------------------------------------*/
#include "ceos.h"
int check_cal(char *filename)
{
	struct qual_sum_rec    *dqsr;
	char   file[256];
	int    era;

	era = set_era(filename,file,-1);
	if (!era) return(1);
	else
	{
		dqsr=(struct qual_sum_rec*)malloc(sizeof(struct qual_sum_rec));
		if (get_dqsr(file,dqsr) == -1) return(1);

		if (strncmp(dqsr->cal_status,"UNCALIB",7)==0)
		{
			printf("\7\7\n\n**********  UNCALIBRATED DATA  **********  \7\7\n");
			printf("Calibration Comments: %s\n",dqsr->cal_comment);
			free(dqsr);
			return(0);
		}
		else if (strncmp(dqsr->cal_status,"INFERRE",7)==0)
		{
			printf("INFERRED CALIBRATION DATA\n");
			printf("Calibration Comments: %s\n",dqsr->cal_comment);
			free(dqsr);
			return(1);
		}
		else if (strncmp(dqsr->cal_status,"CALIBRA",7)==0)
		{
			printf("Calibration Comments: %s\n",dqsr->cal_comment);
			free(dqsr);
			return(1);
		}
		else 
		{
			printf("\7\7\n\n****** UNABLE TO DETERMINE CALIBRATION OF DATA ******\n"); 
			printf("Calibration Comments: %s\n",dqsr->cal_comment);
			free(dqsr);
			return(0);
		}
	}
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-m] [-g] <inSAR> <out>\n", name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   inSAR   uncalibrated CEOS image (input)\n"
	"   out     calibrated ASF tools ready image (output)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -m   manually overide calibration coefficients\n"
	"   -g   create gamma0 output <default is sigma0>\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Performs radiometric calibration of SAR images to create\n"
	"   output images whose values are in sigma-0 (or gamma-0).\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}
