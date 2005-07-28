/*****************************************************************************
NAME: atdp --  ASF time domain processor

SYNOPSIS:     atdp [options] ifile ofile

*****************************************************************************/
/******************************************************************************
*								              *
*  atdp - ASF time domain processor		                              *
*  Copyright Geophysical Institute, University of Alaska                      *
*  Fairbanks. All rights reserved.                                            *
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
#include "aisp_defs.h"
#include "asf_complex.h"

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, FLOAT_MICRON))
#define SQR(a) (a*a)

/*Usage:*/

void give_usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [options] <ifile> <ofile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"      ifile   input signal data file (.D & .L or .raw & .in)\n"
	"      ofile   output file name\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   Argument     Default  Description\n"
	"   ----------------------------------------------------------------\n"
	"   -l first_line   1     First line to process (from 0)\n"
	"   -p patches      8     Number of patches to process (@ 4K lines)\n"
	"   -v valid_lines  3000  Valid output lines per patch\n"
	"   -s skip_samp    0     range samples to skip (of INVALID samps)\n"
	"   -f first_samp   0     1st range samp to process (of VALID samps)\n"
	"   -e 1            0     remove doppler skew from image (flag).\n"
	"   -n num_samps    META  Number of range samples to process\n"
	"                         (Default is read from metadata)\n"
	"   -r output_res   8.0   Desired output azimuth resolution (m)\n"
	"   -c dfile        NO    Read doppler centroid from dfile\n"
	"   -o off_file     NO    Read resampling coeg.fs from off_file\n"
	"   -hamming 	   NO    Use a Hamming window instead of a rectangular one\n"
	"                         for the azimuth reference function weighting\n"
/*	"   -kaiser	   NO    Use a Kaiser window instead of a rectangular one\n"
 *	"                         for the azimuth reference function weighting\n" */
	"   -m CAL_PARAMS   NO    Read the Elevation Angle and Gain vectors from the\n"
	"			 CAL_PARAMS file to correct for the antenna gain\n"
	"   -debug dbg_flg  1     Debug: for options enter -debug 0\n"
	"   -log logfile	   NO	 Allows output to be written to a log file\n"
	"   -quiet	   NO	 Suppresses the output to the essential\n"
	"   -power	   NO	 Creates a power image\n"
	"   -sigma	   NO	 Creates a sigma image\n"
	"   -gamma	   NO	 Creates a gamma image\n"
	"   -beta	   NO	 Creates a beta image\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program creates a SAR image from SAR signal data.\n\n"
	"   The optional switches provide the ability to\n"
	"   override any or all of:\n"
	"        1) the default values given above,\n"
	"        2) parameters read from metadata (.D & .L),\n"
	"        3) parameters read from a parameter file (.raw and .in)\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

double rect(double range_time, double pulse_duration) 
{
  double ratio = range_time / pulse_duration;
  if (ratio > 0.5) return 0.0;
  else if (FLOAT_EQUIVALENT(ratio, 0.5)) return 0.5;
  else if (ratio < 0.5) return 1.0;
}

main (int argc, char *argv [])
{
  patch *p;
  satellite *s;
  rangeRef *r;
  getRec *signalGetRec;
  file *f;
  int ii, kk;
  int n_az,n_range;/*Region to be processed.*/
  int give_usage_action=0;
  struct AISP_PARAMS params;
  meta_parameters *meta;
  complexFloat *image_in, *impulse_response, *image_out;
  double time, range_time, azimuth_time, beam_center_time, pulse_duration;
  double pulse_envelope, antenna_beam_pattern, wavelength, chirp_slope;
  
  printf("%s\n",date_time_stamp());
  fflush(NULL);
  printf("Program: atdp\n\n");
  
  logflag=quietflag=0;
  
  give_usage_action=parse_cla(argc,argv,&params,&meta);
  if (give_usage_action==0) give_usage(argv[0]);
 
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: aisp\n\n");
  }
 
  /* Read input out of SAR processing parameter file */
  atdp_setup(&params,meta,&n_az,&n_range,&s,&r,&f,&signalGetRec);
  
  /* Arrange for memory */
  image_in = (complexFloat *) MALLOC (sizeof(complexFloat)*n_range);
  impulse_response = (complexFloat *) MALLOC (sizeof(complexFloat)*n_range);
  image_out = (complexFloat *) MALLOC (sizeof(complexFloat)*n_range);

  /* Read metadata */
  meta = meta_read(inFile);

  /* Loop through the image */
  for (ii=0; ii<meta->general->line_count; ii++) {

    /* Read image in line by line */
    get_complexFloat_line(inFile, inMeta, ii, image_in);

    /* Define some constant parameters */
    beam_center_time = n_az * meta->sar->line_count / 2;
    pulse_duration = params->pulsedur;
    wavelength = params->wavl;
    chirp_slope = params->slope;

    for (kk=0; kk<meta->general->sample_count; kk++) {

      /* Determine range and azimuth time */
      range_time = kk * meta->sar->range_time_per_pixel;
      azimuth_time = ii * meta->sar->azimuth_time_per_pixel;

      /* Envelope of transmitted radar pulse */
      slant_range = meta_get_slant(meta, ii, kk);
      time = range_time - 2*slant_range/speedOfLight;
      pulse_envelope = rect(time, pulse_duration);

      /* Antenn beam pattern */
      time = azimuth_time - beam_center_time;
      antenna_beam_pattern = rect(time, pulse_duration);

      /* Real part of impulse response function */
      impulse_response[kk].real = pulse_envelope * antenna_beam_pattern;

      /* Imaginary part of impulse response function */
      impulse_response[kk].imag =
	exp(-4*PI * slant_range * wavelength) *
	exp(PI * chirp_slope * SQR(range_time - 2*slant_range / speedOfLight));

      /* Calculate impulse response function */
      image_out.real = 
	image_in[kk].real * impulse_response[kk].real + 
	image_in[kk].imag * impulse_response[kk].imag;
      image_out.imag = 
	image_in[kk].imag * impulse_response[kk].real -
	image_in[kk].real * impulse_response[kk].imag;
    }
    
    put_complexFloat_line(outFile, inMeta, ii, image_out);

    asfPercentMeter(5.0);
  }
  
  return(0);
}
