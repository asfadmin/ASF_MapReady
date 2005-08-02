/*****************************************************************************
NAME: atdp --  ASF time domain processor

SYNOPSIS:     atdp ifile ofile

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
#include "asf_meta.h"
#include "atdp_defs.h"
#include "asf_complex.h"
#include "read_signal.h"

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
	"   %s <ifile> <ofile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"      ifile   input signal data file (.raw & .in)\n"
	"      ofile   output file name\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program creates a SAR image from SAR signal data.\n\n"
	"   The optional switches provide the ability to\n"
	"   override any or all of:\n"
	"        1) the default values given above,\n"
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
  FILE *fp;
  patch *p;
  satellite *s;
  rangeRef *ref;
  getRec *signalGetRec;
  file *f;
  int ii, kk, ll, mm, n_az, n_range, give_usage_action=0, offset;
  int filter_azimuth, filter_range, filter_size;
  struct AISP_PARAMS params;
  meta_parameters *meta;
  complexFloat *image_in, *image_out, impulse_response, sum;
  double lines, samples;
  double time, range_time, azimuth_time, beam_center_time, pulse_duration;
  double pulse_envelope, antenna_beam_pattern, wavelength, chirp_slope;
  double slant_range, pulse_repetition_frequency, range_sampling_rate;
  double exposure_time, r, theta;
  
  printf("%s\n",date_time_stamp());
  fflush(NULL);
  printf("Program: atdp\n\n");
  
  logflag=0;
  quietflag=1;
  
  give_usage_action=parse_cla(argc,argv,&params,&meta);
  if (give_usage_action==0) give_usage(argv[0]);
 
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: atdp\n\n");
  }
  printf("   Initialization ...\n"); 

  /* Read input out of SAR processing parameter file */
  atdp_setup(&params,meta,&n_az,&n_range,&s,&ref,&f,&signalGetRec);
  
  /* Define some parameters */
  beam_center_time = n_az * lines / 2;
  pulse_duration = params.pulsedur;
  wavelength = params.wavl;
  chirp_slope = params.slope;
  pulse_repetition_frequency = params.prf;
  range_sampling_rate = params.fs;
  exposure_time = 0.64; // fix me
  filter_azimuth = (int)(exposure_time * pulse_repetition_frequency / 2 + 0.5);
  filter_range = (int)(pulse_duration * range_sampling_rate / 2 + 0.5);
  filter_size = filter_azimuth * filter_range * 4;

  /* Write metadata */
  lines = signalGetRec->nLines;
  samples = signalGetRec->nSamples;
  meta->general->line_count = lines - filter_azimuth*2;
  meta->general->sample_count = samples - filter_range*2;
  meta->general->data_type = COMPLEX_REAL32;
  meta->general->image_data_type = COMPLEX_IMAGE;
  meta_write(meta, f->out_cpx);

  /* Arrange for memory */
  image_in = (complexFloat *) MALLOC (sizeof(complexFloat)*samples*lines);
  image_out = (complexFloat *) MALLOC (sizeof(complexFloat)*meta->general->sample_count);

  /* Read raw SAR image */
  for (ii=0; ii<lines; ii++)
    getSignalLine(signalGetRec,ii,&image_in[ii],0,samples);
  
  /* Open output image */
  fp = FOPEN(f->out_cpx, "wb");

  /* Loop through the image */
  printf("   Start SAR processing raw image ...\n");
  printf("   Match filter size: %i lines, %i samples\n", 
	 filter_azimuth*2, filter_range*2);
  for (ii=filter_azimuth; ii<lines-filter_azimuth; ii++) {
    for (kk=filter_range; kk<samples-filter_range; kk++) {

      offset = ii*samples + kk;
      ll=0;mm=0;

      /* Apply match filter */
      for (ll=0; ll<filter_azimuth*2; ll++) {
	for (mm=0; mm<filter_range*2; mm++) {
	  
	  sum.real = 0.0;
	  sum.imag = 0.0;
	  
	  /* Determine range and azimuth time */
	  range_time = (kk+mm) * meta->sar->range_time_per_pixel;
	  azimuth_time = (ii+ll) * meta->sar->azimuth_time_per_pixel;
	  
	  /* Envelope of transmitted radar pulse */
	  slant_range = meta_get_slant(meta, ii+ll, kk+mm);
	  time = range_time - 2*slant_range/speedOfLight;
	  pulse_envelope = rect(time, pulse_duration);
	  
	  /* Antenn beam pattern */
	  time = azimuth_time - beam_center_time;
	  antenna_beam_pattern = rect(time, pulse_duration);
	  
	  /* Impulse response function -
	     Straight out of Ian Cumming's book (4-42), written in polar coordinates.
	     For complex data, we have z = r * exp(i*theta). The real part out of that
	     is r*cos(theta), the imaginary part is r*sin(theta).*/
	  r = pulse_envelope * antenna_beam_pattern;
	  theta = (-4*PI * slant_range * wavelength) +
	    (PI * chirp_slope * SQR(range_time - 2*slant_range / speedOfLight));
	  
	  /* Real and imaginary part of impulse response function */
	  impulse_response.real = r * cos(theta);
	  impulse_response.imag = r * sin(theta);
	  
	  /* Multiplication of raw image with time reversed complex conjugate 
	     impulse response function */
	  sum.real += 
	    image_in[offset + ll*filter_range*2 + mm].real * impulse_response.real + 
	    image_in[offset + ll*filter_range*2 + mm].imag * impulse_response.imag;
	  sum.imag += 
	    image_in[offset + ll*filter_range*2 + mm].imag * impulse_response.real -
	    image_in[offset + ll*filter_range*2 + mm].real * impulse_response.imag;
	}
      }

      image_out[kk].real = sum.real / filter_size;
      image_out[kk].imag = sum.imag / filter_size; 
      //printf("   image: line = %5i, sample = %5i\r", ii, kk);

    }
    put_complexFloat_line(fp, meta, ii, image_out);
    if (ii%200 == 0) 
      printf("   Processed line %5d\n", ii);

  }
  FCLOSE(fp);
  
  return(0);
}
