/******************************************************************************
NAME: coh - Calculates interferogram coherence 

SYNOPSIS:   coh  [-look linexsamp] [-step linexsample] In1 In2 Out

        -look   set look box line and sample.  Default 15x3
        -step   set step boc line and sample.  Default 5x1
        In1     image 1 complex file
        In2     image 2 complex file
        Out     Output file name (Out.img, Out.meta)

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
    1.5     9/00    M. Ayers    Repaired bug in code to change the calculation
                                  from Sum(|A x B*|) to |Sum(A x B*)| for the
                                  igram term
    1.6	    3/01    P. Denny	Allow use of one or both .meta files
				  Display nice histogram
    1.61   10/01    T. Logan    change rP = 0.0 (instead of 1.0) when
                                  denomProduct is 0
    1.75    4/02    P. Denny    Update commandline parsing & usage()
                                  fixed histogram values
    2.0     5/03    P. Denny    Use get_*_line and put_*_line instead of FREAD
                                  and FWRITE. Change FComplex data type to
                                  complexFloat. Use new meta, kill DDR.

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
#include "asf_meta.h"
#include "asf_complex.h"
#include "ifm.h" /* For Cabs() function */

#define VERSION      2.0
#define WINDOW_SIZE  3
#define HIST_SIZE   10

/* Macro to figure amplitude */
#define AMP(cpx) sqrt((cpx).real*(cpx).real + (cpx).imag*(cpx).imag)
/* Macro to figure minimum value */
#define MIN(a,b) (((a)<(b))?(a):(b))



int main(int argc, char *argv[])
{
  int line;                             /* Line index counter                 */
  float *a1sum, *a2sum;
  char inName1[256], inName2[256], outName[256]; /* Command line arg names    */
  FILE *inFile1, *inFile2, *outFile;    /* 2 input and 1 output file pointers */
  int num_samples, num_lines;           /* number of samples, number of lines */
  int stepLine, stepSample;             /* Lines/samples to increment per loop*/
  int lookLine, lookSample;             /* Size (Lines/samples) of look window*/
  int lookFlag=FALSE, stepFlag=FALSE;   /* Did the user define look/step ?    */
  int cnt;                              /* Histogram counter                  */
  float	bin_high, bin_low;              /* Histogram ranges                   */
  float	average;                        /* Average Correlation                */
  double hist_sum;                      /* Histogram sum                      */
  double percent, percent_sum;          /* Percent of correlation levels      */
  long long hist_val[HIST_SIZE];        /* Histogram value table              */
  long long hist_cnt = 0;               /* Histogram count                    */
  meta_parameters *inMeta1,*inMeta2,*outMeta; /* Meta structs for in/out files*/
  complexFloat *inBuf1, *inBuf2;        /* Input data buffers                 */
  complexFloat *igram_sum;
  float *rho, *rP;                      /* Output data buffer & extra pointer */

  /* parse command line */
  while (currArg < (argc-3)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-look")) {
      CHECK_ARG(1)
      if (2!=sscanf(GET_ARG(1),"%dx%d",&lookLine,&lookSample)) {
	printf("**ERROR: -look '%s' does not look like line x sample (e.g. '10x2').\n",GET_ARG(1));
	usage(argv[0]);
      }
      lookFlag=TRUE;
    }
    else if (strmatch(key,"-step")) {
      CHECK_ARG(1)
      if (2!=sscanf(GET_ARG(1),"%dx%d",&stepLine,&stepSample)) {
	printf("**ERROR: -step '%s' does not look like line x sample (e.g. '5x1').\n",GET_ARG(1));
	usage(argv[0]);
      }
      stepFlag=TRUE;
    }
    else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 3) {printf("Insufficient arguments.\n"); usage(argv[0]);}

  StartWatch();

  create_name(inName1, argv[currArg],  ".cpx");
  create_name(inName2, argv[currArg+1],".cpx");
  create_name(outName, argv[currArg+2],".img");

  /* Read input meta files */
  inMeta1 = meta_read(inName1);
  inMeta2 = meta_read(inName2);
  num_lines = inMeta1->general->line_count; 
  num_samples = inMeta1->general->sample_count;
  /* Since we're assuming data is complex, make sure metadata reflects that...
   * particularly so get/put_complexFloat_line() work... needs to be here thanks to DDRs */
  inMeta1->general->data_type = meta_polar2complex(inMeta1->general->data_type);
  inMeta2->general->data_type = meta_polar2complex(inMeta2->general->data_type);

  /* Figure multilooking values if necessary */
  if (!stepFlag) {
    stepLine = inMeta1->sar->look_count;
    stepSample = 1;
  }
  if (!lookFlag) {
    lookLine = WINDOW_SIZE * stepLine;
    lookSample = WINDOW_SIZE;
  }

  /*  Create & write output meta file */
  outMeta = meta_copy(inMeta1);
  outMeta->general->line_count   = num_lines / stepLine;
  outMeta->general->sample_count = num_samples / stepSample;
  outMeta->general->data_type = meta_complex2polar(inMeta1->general->data_type);
  outMeta->general->x_pixel_size *= stepSample;
  outMeta->general->y_pixel_size *= stepLine;
  outMeta->sar->line_increment   *= stepLine;
  outMeta->sar->sample_increment *= stepSample;
  meta_write (outMeta, outName);

  printf("\n");
  printf("Calculating interferogram coherence for %s and %s\n",
         inName1, inName2);

  /* create buffers */
  rho     = (float*)MALLOC(sizeof(float)*num_samples/stepSample);
  inBuf1  = (complexFloat*)MALLOC(sizeof(complexFloat)*num_samples*lookLine);
  inBuf2  = (complexFloat*)MALLOC(sizeof(complexFloat)*num_samples*lookLine);
  a1sum   = (float*)MALLOC(sizeof(float)*num_samples);
  a2sum   = (float*)MALLOC(sizeof(float)*num_samples);
  igram_sum = (complexFloat*)MALLOC(sizeof(complexFloat)*num_samples);

  /* Open files */
  inFile1 = fopenImage(inName1,"rb");
  inFile2 = fopenImage(inName2,"rb");
  outFile = fopenImage(outName,"wb");

  printf("\n");
  printf("Input Size    => line: %5d  sample: %5d\n",num_lines,num_samples);
  printf("Step Interval => line: %5d  sample: %5d\n",stepLine,stepSample);
  printf("Window Size   => line: %5d  sample: %5d\n\n",lookLine,lookSample);

  for (cnt = 0; cnt<HIST_SIZE; cnt++) hist_val[cnt] = 0;

  for (line = 0; line<num_lines; line+=stepLine)
  {
    register int off,row,col,limitLine;
    register float denXYS1,denXYS2;
    complexFloat igram;
    int inCol;
    limitLine=MIN(lookLine,num_lines-line);

    printf("Percent completed %3.0f\r",(float)line/num_lines*100.0);

    rP = rho;

    /* read in the next lines of data*/
    get_complexFloat_lines(inFile1, inMeta1, line, limitLine, inBuf1);
    get_complexFloat_lines(inFile2, inMeta2, line, limitLine, inBuf2);

    /* add the remaining rows into sum vectors*/
    off = num_samples;
    for (col=0; col<num_samples; col++)
    {
      off = col;
      denXYS1 = 0.0;
      denXYS2 = 0.0;
      igram.real = 0.0;
      igram.imag = 0.0;

      for (row=0; row<limitLine; row++)
      {
	igram.real += inBuf1[off].real*inBuf2[off].real + inBuf1[off].imag*inBuf2[off].imag;
        igram.imag += inBuf1[off].imag*inBuf2[off].real - inBuf1[off].real*inBuf2[off].imag; 	
	denXYS1 += AMP(inBuf1[off])*AMP(inBuf1[off]);
	denXYS2 += AMP(inBuf2[off])*AMP(inBuf2[off]);
	off+=num_samples;
      }
      igram_sum[col] = igram;			
      a1sum[col] = denXYS1;
      a2sum[col] = denXYS2;
    }

    /* calculate the coherence by adding from sum vectors*/
    for (inCol=0; inCol<num_samples; inCol+=stepSample)
    {
      register float denomProduct;
      register int limitSample = MIN(lookSample,num_samples-inCol);
      denXYS1 = 0.0;
      denXYS2 = 0.0;
      igram.real = 0.0;
      igram.imag = 0.0;

      /* step over coherence area; Sum output columns*/
      for (col = 0; col<limitSample; col++)
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
	  printf("Exiting.\n");
          exit(EXIT_FAILURE);
	  *rP=1.0;
	}
      }
      rP++;
    } /* End for inCol */

    /*Dump out coherence values for this line.*/
    put_float_line(outFile, outMeta, line/stepLine, rho);

    for (cnt = 0; cnt <num_samples/stepSample; cnt++)
    {
      register int tmp;
      tmp = (int) (rho[cnt]*HIST_SIZE); /* Figure out which bin this value is in */
      /* This shouldn't happen */
      if(tmp >= HIST_SIZE)
	      tmp = HIST_SIZE-1;
      if(tmp < 0)
	      tmp = 0;

      hist_val[tmp]++;        /* Increment that bin for the histogram */
      hist_sum += rho[cnt];   /* Add up the values for the sum */
      hist_cnt++;             /* Keep track of the total number of values */
    }
  } /* End for line */

  printf("Percent completed %3.0f\n",(float)line/num_lines*100.0);

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

  /* Free and halt */
  FREE(rho); 
  FREE(inBuf1); 
  FREE(inBuf2);
  FCLOSE(outFile); 
  FCLOSE(inFile1); 
  FCLOSE(inFile2);
  meta_free(inMeta1);
  meta_free(inMeta2);
  meta_free(outMeta);
  StopWatch();
  return(0);
}


void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-look lxs] [-step lxs] <In1> <In2> <Out>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   In1   image 1 complex file\n"
	"   In2   coregistered image 2 complex file\n"
	"   Out   floating-point coherence file (.img).\n\n");
 printf("OPTIONAL ARGUMENTS:\n"
	"   -look lxs   change look box (l)ine and (s)ample.\n"
	"                 (Read from meta file by default)\n"
	"   -step lxs   change step box (l)ine and (s)ample.\n"
	"                 (Read from meta file by default)\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   A correlation calculator to estimate interferogram quality\n");
 printf("\n"
	"Version %4.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
