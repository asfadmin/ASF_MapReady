/****************************************************************
NAME: DESPIKE -- Performs an adaptive low-pass filter
 
SYNOPSIS:  despike infile outfile
 
DESCRIPTION:
      This algorithm uses kernel filtering to remove high frequency
    information from the input image.  Processing proceeds by placing a 
    3X3 kernel over each point in the image.  The mean and deviation of
    all values x in the kernel such that min <= x <= max are then computed.
    If the pixel being processed is more than nstd deviations from the mean
    AND is more than tol from the mean, it is considered invalid.
    Invalid pixels are replaced based on the invld option given.
      If invld = 1, invalid pixels are replaced with 0.
      If invld = 2, invalid pixels are replaced with the kernel mean.
    Processing continues until all points in the image have been despiked.
    The input file, infile.img, is thus despiked to produce the output file,
    outfile.img.
 
    Parameters:
       infile    input .img byte file
       outfile   output .img byte file
	 
EXTERNAL ASSOCIATES: terrcorr
 
FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    infile.img           Input image to despike  
    outfile.img          Results of despiking  
 
PROGRAM HISTORY:
  Version       Date       Author             Request
  -------       ----       ------             -------
    1           11/84   J. LaVergne  
    2            1/87   K. Zanter     conversion to unix         
    3            3/89   K. Narayanan  conversion to c and 5.0 standards
    4            8/89   J. Marmon     Changes to work on PN9050
    5            7/94   T. Logan      Redesigned without LAS or TAE 
				      depencies for the Cray YMP
    6.0          7/94   T. Logan      Converted to Parallel for Cray-T3D
    6.1		 2/95	T. Logan      Scalable problem size
    7.0		 10/95  T. Logan      Port from T3D to Solaris
    7.1		 8/98   O. Lawlor     Despike non-square images.
 
HARDWARE/SOFTWARE LIMITATIONS:
   
ALGORITHM DESCRIPTION:

    Prepare column sums for start of file 
    For all lines in this section
      For all pixels in the current line
	if (min <= input pixel <= max) include input pixel in mean
	else  don't include input pixel in mean  
	if (((pixel-mean)^2 > nstd^2*variance) && (abs(pixel-mean) > tol))
	  if (invld == 1) output pixel = 0
	  if (invld == 2) output pixel = mean
	else output pixel = input pixel
      Reset column sums for next image line
 
ALGORITHM REFERENCES:
 
BUGS:
*******************************************************************************
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
#include <sys/times.h>
#include "ddr.h"
#include "asf_reporting.h"
#include "asf_nan.h"

#define VERSION 7.1

int is_valid(float val) {
  return !ISNAN(val);
}

int
main (argc,argv)
    int argc; char **argv;
{
double  nstd;                        /* number of standard deviations  */
double  tol;                         /* tolerance value                */
double  kersum;                      /* sum of data in the kernel      */
double  varsum;                      /* sum of squared data in kernel  */
double  mean;                        /* mean of kernel being processed */
double  variance;                    /* variance in kernel             */
double  mydiff;                      /* difference from kernel mean    */
double  *sum;                        /* array of kernel column sums    */ 
double  *sqsum;                      /* array of column square sums    */
short   *vsum;                       /* # valid pixels in kernel column*/
int    divv;			     /* kernel sum divisor	       */
int    i, j, k, l;	             /* temporary storage	       */
int    line;                        /* loop counter                   */
int    del_line, add_line;          /* line to del/add to column sums */
int    invld;                       /* invalid pixel option           */
int    klines, ksamps;              /* no. lines & samples in kernel  */
int    nl, ns;                      /* no. lines & samples in image   */
int    valsum;                      /* no. elements in the kernel     */
int    xhalf, yhalf;                /* half kernel in x & y dimension */
float low;                           /* lower limit for data           */
//float high;                          /* upper limit for data           */
float *in_buf;                       /* input image                    */ 
float *out_buf;                      /* output image                   */
float *temp;		             /* temp pointer for buffer swap   */
float *del_buf;                      /* next line to del from kernel   */
float *add_buf;                      /* next line to add to kernel     */
char    infile[255];	             /* name of input file	       */
char    outfile[255];		     /* name of output file	       */
char    inddr[255];	             /* name of input file ddr	       */
char    outddr[255];		     /* name of output file ddr	       */
char    cmd[255];		     /* buffer for system command      */
FILE    *fpin, *fpout;	             /* input and output file pointers */
int     loop;			     /* counter to determine pass      */
struct  DDR      ddr;                /* ddr structure                  */

StartWatch();

/* Get user entered parameters & echo 
 -----------------------------------*/
if (argc != 3)
 {
    asfPrintStatus("\nUsage: %s infile outfile\n",argv[0]);
    asfPrintStatus("       inputs:  infile     .img (REAL32 image)\n");
    asfPrintStatus("      outputs:  outfile    .img (REAL32 image)\n");
    asfPrintStatus("\n");
    asfPrintStatus("       Version %.2f,  ASF Tools\n\n",VERSION);
    exit(1);
 }
strcat(strcpy(infile,argv[1]),".img");
strcat(strcpy(outfile,argv[2]),".img");
strcat(strcpy(inddr,argv[1]),".meta");
strcat(strcpy(outddr,argv[2]),".meta");
i = c_getddr(argv[1], &ddr);
if (i != 0) { asfPrintError("Unable to get ddr for file %s\n",infile); }
nl = ddr.nl;
ns = ddr.ns;
asfPrintStatus("Processing Parameters:\n");
asfPrintStatus("\t input file %s is size %i*%i\n",infile,nl,ns);
asfPrintStatus("\t output file : %s\n",outfile);
 
/* Read the input file into memory 
 ------------------------------- */
in_buf  = (float *) MALLOC (nl*ns);
out_buf = (float *) MALLOC (nl*ns);

fpin = fopenImage(infile,"rb");
for (i = 0; i < nl; ++i)
  getFloatLine(fpin, &ddr, i, &in_buf[i*ns]);
fclose(fpin);
 
/* get remaining parameters
 -------------------------*/
klines = 3;
ksamps = 3;
xhalf = (ksamps - 1) / 2;
yhalf = (klines - 1) / 2;
 
sum   = (double *) MALLOC (ns*sizeof(double));
sqsum = (double *) MALLOC (ns*sizeof(double));
vsum  = (short *)  MALLOC (ns*sizeof(short));
low   = in_buf[0];

for (loop = 0; loop < 2; loop++)
 {
  /* Set parameters for this loop */
  for (i = 0; i < nl*ns; i++) out_buf[i] = 0.0;
  if (loop == 0)
    {
      /* low = 0; high = 255; */ invld = 1; nstd = 1.5; tol = 50.0;
      /* printf("\t low = %f high = %f\n",low,high); */
      printf("\t invld = %i, nstd = %g, tol = %f\n",invld,nstd,tol);
    }
  else 
    {
      /* low = 1; */ invld = 2;
      /* printf("\t low = %f high = %f\n",low,high); */
      printf("\t invld = %i, nstd = %g, tol = %g\n",invld,nstd,tol);
    }
 
  /* prepare the initial kernel for filtering
   -----------------------------------------*/
  for (j = 0; j < ns; j++)
    {
      sum[j] = 0.0;
      vsum[j] = 0;
      sqsum[j] = 0.0;
    }
  for (line = 0; line <= xhalf ; line++)
    for (k = 0, l = line*ns; k < ns ; k++, l++)
      if (is_valid(in_buf[l]))
        {
	  if (in_buf[l] < low) low = in_buf[l];

          sum[k] += (double) in_buf[l];
          vsum[k]++;
          sqsum[k] += ((double)in_buf[l]*(double)in_buf[l]);
        }
  kersum = 0.0;
  valsum = 0;
  varsum = 0.0;
  for (k = 0; k <= xhalf ; k++)
   {
     kersum += sum[k];
     valsum += vsum[k];
     varsum += sqsum[k];
   }

  /* apply the low pass filter to the image
   -------------------------------------- */
  del_line = 0 - xhalf;
  del_buf  = &in_buf[0];
  add_line = xhalf + 1;
  add_buf  = &in_buf[add_line*ns];
 
  for (line = 0; line < nl; line++)
     {
       for (k = 0, l = line*ns; k < ns; k++,l++)
        {
	  if (ISNAN(in_buf[l]))
	    out_buf[l] = in_buf[l];
	  else {
	    divv = valsum - 1;
	    if (divv  ==  0) mean = 0;
	    else mean = (kersum - (double)in_buf[l]) / divv;
	    if (valsum != 0)
	      variance = (varsum/valsum)-((kersum*kersum)/(valsum*valsum));
	    else variance = 0.0;
	    mydiff = in_buf[l] - mean;
	    if (( mydiff*mydiff > nstd*nstd*variance) && (fabs(mydiff) > tol ))
	    {
	      if (invld == 1) out_buf[l] = NAN;
	      if (invld == 2) out_buf[l] = mean;
	    }
	    else out_buf[l] = in_buf[l];
	  }

         /* reset kernel sum and sum valid values
  	  ------------------------------------- */
         if (k < ns-(xhalf+1))
  	   {
	     kersum += sum[k+xhalf+1];
	     valsum += vsum[k+xhalf+1];
	     varsum += sqsum[k+xhalf+1];
	   }
         if (k >= xhalf) 
	   {
	     kersum -= sum[k-xhalf];
	     valsum -= vsum[k-xhalf];
	     varsum -= sqsum[k-xhalf];
	   }
        } /* End of this line */
 
       if (del_line >= 0)
        {
  	  for (k = 0; k < ns ; k++)
	    if (is_valid(del_buf[k]))
	    {
	      sum[k] -= (double) del_buf[k];
	      vsum[k]--;
	      sqsum[k] -= ((double)del_buf[k]*(double)del_buf[k]);
	    }
	  del_buf += ns;
        }
       del_line++;
       if (add_line < nl)
        {
  	  for (k = 0; k < ns ; k++)
	    if (is_valid(add_buf[k]))
	      {
	        sum[k] += (double) add_buf[k];
	        vsum[k]++; 
	        sqsum[k] +=  ((double)add_buf[k]*(double)add_buf[k]);
	      }
	  add_line++;
	  add_buf += ns;
        }
       varsum = 0.0;
       kersum = 0;
       valsum = 0.0;
       for (k = 0; k <= xhalf ; k++)
        {
  	  kersum += sum[k];
	  valsum += vsum[k];
	  varsum += sqsum[k];
        }
     }   /* End of a pass through the image */

    /* Swap input and output buffers for the next pass
     ------------------------------------------------*/
    temp = in_buf;
    in_buf = out_buf;
    out_buf = temp;  
  }
 
  for (line = 0; line < nl; line++)
    for (k = 0, l = line*ns; k < ns; k++,l++)
      if (ISNAN(in_buf[l])) in_buf[l] = low;

/* open, write, and close output image
 ------------------------------------ */
fpout = fopenImage(outfile,"wb");
for (i = 0; i < nl; ++i)
  putFloatLine(fpout, &ddr, i, &in_buf[i*ns]);
fclose(fpout);

sprintf(cmd,"cp %s %s\n",inddr,outddr);
if (system(cmd) != 0)
  { asfPrintError("Failure copying metadata.\n\n"); }

asfPrintStatus("\nDespike finished, wrote raw image %s\n",outfile);
StopWatch();

free(in_buf);
free(out_buf);
free(sum); 
free(sqsum);
free(vsum);

exit(0);
}

