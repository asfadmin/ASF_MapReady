/******************************************************************************
NAME:		coh - Calculates interferogram coherence 

SYNOPSIS:   coh  [-l linexsamp] [-s linexsample] In1 In2 Out

	-l	set look box line and sample.  Default 15x3
        -s      set step boc line and sample.  Default 5x1
        In1     image 1 complex file
        In2     image 2 complex file
        Out     Output file name (Out.img, Out.ddr)

DESCRIPTION:

     Calculates the coherence for the Igram using the following
     formula:
 
		|         Sum (n pixels) [ a x b* ] 	     |
         rho =  | __________________________________________ |
		|					     |
		|  Sqrt { Sum  [ a x a* ]  Sum  [ b x b* ] } |

 	the 'x' in the equation above indicates multiplication and
	the '*' indicates complex conjugation.
	In this program n = 45 pixels; 15 in azimuth x 3 in range.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/97    T. Logan    To compare our interferogram with H. Zebker's.
    1.1	    6/00    D. Koster	Accept files larger than 2GB
    1.2     7/00    M. Ayers    Fixed so that coherence is calculated correctly.
    1.3	    8/00    D. Koster	Fixed bug in seek position var that caused 
				  it to 'roll over' to 0.
    1.4	    9/00    P. Denny	Default step/win-line/sample read in from
				  metafile.  Now RSAT, JERS compatible
    1.5     9/00    M. Ayers    Repaired bug in code to change the calculation from
				  Sum(|A x B*|) to |Sum(A x B*)| for the igram term
    1.6	    3/01    P. Denny	Allow use of one or both .meta files
				  Display nice histogram
    1.61    10/01   T. Logan    change rP = 0.0 (instead of 1.0) when denomProduct is 0
    1.75     4/02   P. Denny    Update commandline parsing & usage()
                                  fixed histogram values

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   coh: A Correlation Calculator to Estimate Interferogram Quality	    *
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

#include "asf.h"
#include "ddr.h"
#include "asf_meta.h"
#include "ifm.h"

#define VERSION           1.75
#define STEP_LINE         5
#define STEP_SAMPLE       1
#define WIN_LINE          15
#define WIN_SAMPLE        3
#define WINDOW_SIZE       3
#define EBYTE             1
#define MN 	  	  256
#define MIN(a,b) 	((a)<(b) ? (a) : (b))
#define HIST_SIZE	10

void usage(char *name);
void bye(char *msg);

int main(int argc, char *argv[])
{
	int 	 line;					/* Line index counter */
	float    *a1sum, *a2sum;
	FComplex *igram_sum;
	char     a1f[MN], a2f[MN], cohf[MN], ddrf[MN];	/* arg names */
	FILE 	 *fin1, *fin2, *fcoh;			
	FComplex *in1,  *in2;
	float    *rho,  *rP;
	int	 ns = -1, nl = -1;			/* number of samples & number of lines */
	int 	 sl = STEP_LINE, ss = STEP_SAMPLE;
	int 	 wl = WIN_LINE, ws = WIN_SAMPLE;
	
	int	cnt;					/* Histogram counter */ 
	float	bin_high, bin_low;			/* Histogram ranges */
	float	average;				/* Average Correlation */
	double	hist_sum;				/* Histogram sum */
	double	percent, percent_sum;			/* Percent of correlation levels */
	long long hist_val[HIST_SIZE];			/* Histogram value table */
	long long hist_cnt = 0;				/* Histogram count */

	struct DDR ddr,newddr;
	
	/* process cla's*/
	StartWatch();

	for (cnt = 0; cnt<HIST_SIZE; cnt++) hist_val[cnt] = 0;

	currArg=1;
	/* parse command line */
	while (currArg < (argc-3)) {
		char *key = argv[currArg++];
		if (strmatch(key,"-l")) {
			CHECK_ARG(1)
			if (2!=sscanf(GET_ARG(1),"%dx%d",&wl,&ws)) {
				printf("**ERROR: -l '%s' does not look like a line x sample (e.g. '10x2').\n",GET_ARG(1));
				usage(argv[0]);
			}
 		}
		else if (strmatch(key,"-s")) {
			CHECK_ARG(1)
			if (2!=sscanf(GET_ARG(1),"%dx%d",&sl,&ss)) {
				printf("**ERROR: -s '%s' does not look like a line x sample (e.g. '5x1').\n",GET_ARG(1));
				usage(argv[0]);
			}
		}
		else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
	}
	if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}

	create_name(a1f, argv[currArg],".cpx");
	create_name(a2f, argv[currArg+1],".cpx");
	create_name(cohf,argv[currArg+2],".img");
	create_name(ddrf,argv[currArg+2],"");

	/* Read nlooks from meta file if not user specified */
	if (currArg == 1)
	{
		meta_parameters *meta1, *meta2;
		/*Read .meta files if possible*/
		if (extExists(argv[currArg],".meta") && extExists(argv[currArg+1],".meta"))
		{
			meta1 = meta_read(argv[currArg]);
			meta2 = meta_read(argv[currArg+1]);
			if (meta1->ifm->nLooks != meta2->ifm->nLooks)
			{
				char error[256];
				sprintf(error,"Step line values conflict for meta files %s and %s\n",
						argv[currArg],argv[currArg+1]);
				meta_free(meta1);
				meta_free(meta2);
				bye(error);
			}
			sl = meta1->ifm->nLooks;	ss = 1;
			wl = WINDOW_SIZE * sl;		ws = WINDOW_SIZE;
			meta_free(meta1); meta_free(meta2);
			printf("Got nLooks from meta files\n");
		}
		else if (extExists(argv[currArg],".meta"))
		{
			printf("Warning: Unable to either find or open %s.meta\n",argv[currArg+1]);
			printf("\t Using %s.meta for both images.\n",argv[currArg]);
			meta1 = meta_read(argv[currArg]);
			sl = meta1->ifm->nLooks;	ss = 1;
			wl = WINDOW_SIZE * sl;		ws = WINDOW_SIZE;
			meta_free(meta1);
		}
		else if (extExists(argv[currArg+1],".meta"))
		{
			printf("Warning: Unable to either find or open %s.meta\n",argv[currArg]);
			printf("\t Using %s.meta for both images.\n",argv[currArg+1]);
			meta1 = meta_read(argv[currArg+1]);
			sl = meta1->ifm->nLooks;	ss = 1;
			wl = WINDOW_SIZE * sl;		ws = WINDOW_SIZE;
			meta_free(meta1);
		}
		else { bye("ERROR!  Unable to either find or open metaFile."); }
	}

	if (ns == -1 || nl == -1) {
		if (c_getddr(a1f, &ddr) != 0) bye("Unable to get ddr file for igram");
		nl = ddr.nl; 
		ns = ddr.ns;
		newddr=ddr;
		newddr.nl=nl/sl;
		newddr.ns=ns/ss;
		newddr.dtype=4;
		newddr.nbands=1;
                newddr.line_inc *= sl;
                newddr.sample_inc *= ss;
                newddr.pdist_x *= ss;
                newddr.pdist_y *= sl;
		c_putddr(cohf,&newddr);
	}

	printf("\nCalculating interferogram coherence for %s and %s\n",a1f,a2f);

	/* create buffers */
	rho   = (float*)MALLOC(sizeof(float)*ns/ss);
	in1  = (FComplex*)MALLOC(sizeof(FComplex)*ns*wl);
	in2  = (FComplex*)MALLOC(sizeof(FComplex)*ns*wl);
	a1sum = (float*)MALLOC(sizeof(float)*ns);
	a2sum = (float*)MALLOC(sizeof(float)*ns);
	igram_sum = (FComplex*)MALLOC(sizeof(FComplex)*ns);

	/* file open*/
	fin1=fopenImage(a1f,"rb");
	fin2=fopenImage(a2f,"rb");
	fcoh=fopenImage(cohf,"wb");

	printf("\nInput Size    => line: %5d  sample: %5d\n",nl,ns);
	printf("Step Interval => line: %5d  sample: %5d\n",sl,ss);
	printf("Window Size   => line: %5d  sample: %5d\n\n",wl,ws);

	for (line = 0; line < nl; line+=sl)
	{
		register int off,row,col,limitLine;
		register float denXYS1,denXYS2;
		FComplex igram;
		int inCol;
		long long seek_pos;
		limitLine=MIN(wl,nl-line);

		printf("Percent completed %3.0f\r",(float)line/nl*100.0);

		rP = rho;

		/* read in the next lines of data*/
		seek_pos = (long long) line*ns*sizeof(float);

		FSEEK64(fin1,2*seek_pos,0);
		FREAD(in1, sizeof(FComplex),limitLine*ns,fin1);

		FSEEK64(fin2,2*seek_pos,0);
		FREAD(in2, sizeof(FComplex),limitLine*ns,fin2);

#define amp(cpx) sqrt((cpx).real*(cpx).real + (cpx).imag*(cpx).imag)

		/* add the remaining rows into sum vectors*/
		off=ns;
		for (col=0; col<ns; col++)
		{
			off=col;
			denXYS1=0.0;
			denXYS2=0.0;
			igram.real=0.0;
			igram.imag=0.0;

			for (row=0; row<limitLine; row++)
			{
				igram.real += in1[off].real*in2[off].real + in1[off].imag*in2[off].imag;
                                igram.imag += in1[off].imag*in2[off].real - in1[off].real*in2[off].imag; 	
				denXYS1 += amp(in1[off])*amp(in1[off]);
				denXYS2 += amp(in2[off])*amp(in2[off]);
				off+=ns;
			}
			igram_sum[col] = igram;			
			a1sum[col] = denXYS1;
			a2sum[col] = denXYS2;
		}
		
		/* calculate the coherence by adding from sum vectors*/
		for (inCol=0; inCol<ns; inCol+=ss)
		{
			register float  denomProduct;
			register int limitSample=MIN(ws,ns-inCol);
			denXYS1=0.0;
			denXYS2=0.0;
			igram.real=0.0;
			igram.imag=0.0;

			/* step over coherence area; Sum output columns*/
			for (col = 0; col < limitSample; col++)
			{
				igram.real += igram_sum[inCol+col].real;
				igram.imag += igram_sum[inCol+col].imag;				
				denXYS1 += a1sum[inCol+col];
				denXYS2 += a2sum[inCol+col];
			}
			denomProduct=denXYS1*denXYS2;
			
			if (denomProduct == 0.0)
				*rP = 0.0;
			else 
			{
				*rP = (float)Cabs(igram)/sqrt(denomProduct);
				if (*rP>1.0001)
				{ 
					printf("coh = %f -- setting to 1.0\n",*rP);
					printf("You shouldn't have seen this!\n");
					bye("Exiting");
					*rP=1.0;
				}
			}
			rP++;
		} /* End for inCol */
		/*Dump out coherence values for this line.*/
		FWRITE(rho,sizeof(float),ns/ss,fcoh);

		for (cnt = 0; cnt <ns/ss; cnt++)
		 {
			register int tmp;
			tmp = (int) (rho[cnt]*HIST_SIZE); /* Figure out which bin this value is in */
			/* This shouldn't happen */
			if(tmp >= HIST_SIZE)
				tmp = HIST_SIZE-1;
			if(tmp < 0)
				tmp = 0;

			hist_val[tmp]++;		 /* Increment that bin for the histogram */
			hist_sum += rho[cnt];		 /* Add up the values for the sum */
			hist_cnt++;			 /* Keep track of the total number of values */
		 }
	} /* End for line */

	printf("Percent completed %3.0f\n",(float)line/nl*100.0);
	
	/*  free and halt */
	FREE(rho); 
	FREE(in1); 
	FREE(in2);
	FCLOSE(fcoh); 
	FCLOSE(fin1); 
	FCLOSE(fin2);
	StopWatch();

	/* Sum and print the statistics */
	percent_sum = 0.0;
	printf("   Coherence  :  Occurrences  :  Percent\n");
	printf("---------------------------------------\n");
	for (cnt = 0; cnt < HIST_SIZE; cnt++)
	{
		bin_low  = (float)(cnt)/(float)HIST_SIZE;
		bin_high = (float)(cnt+1)/(float)HIST_SIZE;
		percent  = (double)hist_val[cnt]/(double)hist_cnt;
		percent_sum += (float)100*percent;
		printf(" %.2f -> %.2f :   %.8lld       %2.3f \n",
			bin_low,bin_high, (long long) hist_val[cnt],100*percent); 
	}
	average = (float)hist_sum/(float)hist_cnt;
	printf("---------------------------------------\n");
	printf("Average Coherence: %.3f  (%.1f / %lld) %f\n",average,hist_sum,hist_cnt,percent_sum);

	return(0);
}


void bye(char *msg) { 
	fprintf(stderr,"%s\n",msg); 
	exit(1); 
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-l lxs] [-s lxs] <In1> <In2> <Out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   In1   image 1 complex file\n"
	"   In2   coregistered image 2 complex file\n"
	"   Out   floating-point coherence file (.img).\n\n");
 printf("OPTIONAL ARGUMENTS:\n"
	"    -l lxs   change look box (l)ine and (s)ample.\n"
	"               (Read from meta file by default)\n"
	"    -s lxs   change step box (l)ine and (s)ample.\n"
	"               (Read from meta file by default)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   A correlation calculator to estimate interferogram quality\n");
 printf("\n"
	"Version %4.2f, ASF SAR Tools\n"
	"\n",VERSION);
 exit(1);
}
