/*****************************************************************************
NAME: cpx_filter

SYNOPSIS: cpx_filter <complex image input> <complex image output>
                         <frequency parameter file>


DESCRIPTION:
        This program filters the complex image data in an attempt to improve
        coherence in interferograms. Complex data that is processed at a doppler
        different that the optimal doppler will exhibit a decreased coherence in
        interferometry. This coherence can be improved by filtering the data in
        azimuth and retaining only the part of the two dataset's spectra that
        overlap. The two datasets are then time-domain demodulated to baseband
        by the mean of their two doppler centroids. Another program
        'cpx_spectrum' generates an azimuthal spectrum of the data.  
        
        The frequency parameter file indicates which frequencies should be
        filtered. This can be generated using the program gen_filt_params. The
        format of the file is as follows:
          frequency shift (modulation)
          loFreq1 hiFreq1
          loFreq2 hiFreq2

        All specifications are in Hz and each line is ended with a carriage
        return.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0           5/00   M. Ayers       Original development
    1.5    5/03   P. Denny       Standardize usage()
                                  Use meta struct instead of DDR
                                  Use get_*_lines instead of FREAD
                                  Use put_*_lines instead of FWRITE
                                  Replace FCMPLX struct with complexFloat

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*                                                                           *
*   cpx_filter - Bandpass filters complex image data in azimuth             *
*   Copyright (C) 2001  ASF Advanced Product Development                    *
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
*       ASF Advanced Product Development LAB Contacts:                      *
*       APD E-mail:     apd@asf.alaska.edu                                  *
*                                                                           *
*       Alaska SAR Facility                     APD Web Site:               *        
*       Geophysical Institute                   www.asf.alaska.edu/apd      *
*       University of Alaska Fairbanks                                      *
*       P.O. Box 757320                                                     *
*       Fairbanks, AK 99775-7320                                            *
*                                                                           *
****************************************************************************/
#include <math.h>
#include "asf.h"
#include "aisp_defs.h"
#include "asf_meta.h"

#define BLOCK_SIZE 2000
#define TOSS_SIZE BLOCK_SIZE/100
#define VERSION 1.5

int main(int argc,char **argv)
{
  int lines,samps;                   /* Number of image lines and samples     */
  int fftLen,start_line;             /* FFT length, & line to start processing at*/
  int x,y,i,k;                       /* Counters                              */
  int f_lo1,f_lo2,f_hi1,f_hi2;       /* Filter frequency indicies             */
  int chunk_size,chunk_int;          /* Size of current datablock, & temp value*/
  int last_chunk;                    /* Size of the last chunk                */
  int compensate_for_last_chunk=1;   /* If last chunk = 0 dont loop for last chunk*/
  char *inName, *parmName, *outName; /* Input filename                        */
  float filtStart[2], filtEnd[2];    /* Filter start and stop variables       */
  float df, stop;                    /* Delta and counter variables           */
  complexFloat *inBuf,*outBuf;       /* Input/Output Image Buffers            */
  complexFloat *fftBuf;              /* FFT Buffer for the image              */
  float *ampBuf,*phsBuf;             /* Amplitude and Phase Buffers           */
  float *time_vector,A,B,shift;      /* Time vector, & freq modulation shift vars*/
  float chunk_float;                 /* Temporary value                       */
  FILE *inF, *freqF, *outF1;         /* Input and Output file pointers        */
  float cur_time, f_s;               /* Current time to increment the time vector by */
  meta_parameters *inMeta, *outMeta; /* Meta info about the images            */

/* Usage is shown if the user doesn't give 3 arguements */
  if(argc!=4) { usage(argv[0]); }

  StartWatch();
  printf("Program cpx_filter\n");

/* Get the filename and filter start and end frequencies from the command line*/ 
  inName=argv[1];
  if(findExt(inName)==NULL)
          inName = appendExt(argv[1],".cpx");
  outName=argv[2];
  parmName=argv[3];

/* Get input metadata. Make sure data_type is complex */
  inMeta = meta_read(inName);
  if (inMeta->general->data_type < COMPLEX_BYTE) {
   switch (inMeta->general->data_type) {
     case BYTE:      inMeta->general->data_type=COMPLEX_BYTE;      break;
     case INTEGER16: inMeta->general->data_type=COMPLEX_INTEGER16; break;
     case INTEGER32: inMeta->general->data_type=COMPLEX_INTEGER32; break;
     case REAL32:    inMeta->general->data_type=COMPLEX_REAL32;    break;
     case REAL64:    inMeta->general->data_type=COMPLEX_REAL64;    break;
    }
    meta_write (inMeta, inName);
  }

/* Open the frequency parameter file and read the parameters */
  if((freqF=FOPEN(parmName,"r"))==NULL) {
    printf("Frequency Parameter File %s could not be Opened!\n",parmName);
    exit(EXIT_FAILURE);
  }
  fscanf(freqF,"%f\n",&f_s);
  fscanf(freqF,"%f\n",&shift);
  fscanf(freqF,"%f %f\n", &filtStart[0], &filtEnd[0]);
  fscanf(freqF,"%f %f\n", &filtStart[1], &filtEnd[1]);

  printf("\n");
  printf("Input file is %s\n",inName);
  printf("Output file is %s.cpx\n",outName);
  printf("Parameter file is %s\n",parmName);
  printf("Filtering from frequencies %.2f to %.2f Hz and %.2f to %.2f in Azimuth\n",filtStart[0],filtEnd[0],filtStart[1],filtEnd[1]);
  printf("The sampling frequency is %f Hz\n",f_s);
  printf("Shifting the spectrum by %.2f Hz\n",shift);

/* Get the number of lines and samples from the input meta file */
  lines = inMeta->general->line_count;
  samps = inMeta->general->sample_count;

  chunk_size  = BLOCK_SIZE;
  chunk_float = (float)lines/chunk_size;
  chunk_int   = lines/chunk_size;
  last_chunk  = (int)((chunk_float-(float)chunk_int) * (float)BLOCK_SIZE + 0.5);
  if( (2*TOSS_SIZE) > last_chunk)
    compensate_for_last_chunk=0;
  printf("Chunk Size is set to %d, the last chunk is %d lines\n",
         chunk_size, last_chunk);

/* Compute the FFT length based on the number of lines. Must be a power of 2 */
  i      = (log10(chunk_size)/log10(2)) + 1;
  fftLen = pow(2,i);
  printf("FFT Length is %d\n",fftLen);
  cfft1d(fftLen,NULL,0);

  printf("The Input Image has %d lines and %d samples\n",lines,samps);

/* Allocate the memory for all the buffers */
  inBuf  = (complexFloat *)MALLOC(sizeof(complexFloat)*samps*fftLen);
  outBuf = (complexFloat *)MALLOC(sizeof(complexFloat)*samps*fftLen);
  fftBuf = (complexFloat *)MALLOC(sizeof(complexFloat)*fftLen);
  ampBuf = (float *)MALLOC(sizeof(float)*fftLen);
  phsBuf = (float *)MALLOC(sizeof(float)*fftLen);
  time_vector = (float *)MALLOC(sizeof(float)*lines);

/* Open the Complex Image File */
  if((inF=FOPEN(inName,"rb"))==NULL) {        
    printf("Complex Image file %s could not be opened\n",inName);
    exit(EXIT_FAILURE);
  }

  strcat(outName,".cpx"); 
  if((outF1=FOPEN(outName,"wb"))==NULL) {
    printf("Unable to write output %s\n",outName);
    exit(EXIT_FAILURE);
  }

  outMeta = meta_copy(inMeta);
  outMeta->general->line_count = 
          chunk_int*(BLOCK_SIZE-2*TOSS_SIZE)+(last_chunk-2*TOSS_SIZE);
  outMeta->general->sample_count = samps; 
  meta_write(outMeta, outName);


/* Find the filter frequency index values */
  df   = f_s/fftLen;
  stop = 0;
  i    = 0;
  while(stop<filtStart[0]) {
    f_lo1 = i;
    stop  = df*i;
    i++;
  }

  stop = 0;
  i    = 0;
  while(stop<filtStart[1]) {
    f_lo2=i;
    stop=df*i;
    i++;
  }

  i    = fftLen;
  stop = df*i;  
  while(stop>filtEnd[0]) {
    f_hi1 = i;
    stop  = df*i;   
    i--;
  }

  i    = fftLen;
  stop = df*i;
  while(stop>filtEnd[1]) {
    f_hi2 = i;
    stop  = df*i;
    i--;
  }

/* Zero out all the arrays and begin processing data */
  cur_time = 0;
  for(i=0; i<fftLen; i++)
  {
    ampBuf[i] = 0;
    phsBuf[i] = 0;
  }

  for(k=0; k<chunk_int+compensate_for_last_chunk; k++)
  {
    printf("\nProcessing Chunk %d of %d\n",k,lines/chunk_size);

    start_line = (k==0) ? 0 : (k*chunk_size)-(2*TOSS_SIZE);

    if (k==chunk_int)
      chunk_size=last_chunk;

    cur_time=start_line*(1/f_s);

  /* Read in the data chunk & put in proper endian order */
    printf("Reading %d Lines Starting at Line %d\n",chunk_size,start_line);
    get_complexFloat_lines(inF, inMeta, start_line, chunk_size, inBuf);

  /* Process the each column */
    printf("Performing the FFT and Filtering Operations\n");
    for(x=0; x<samps; x++)
    {
      if(x%1000 == 0)  printf("Processing Column %d\n",x);

      for(y=0;y<fftLen;y++)
      {
        fftBuf[y].real = 0;
        fftBuf[y].imag = 0;
      }

      for(y=0;y<chunk_size;y++)
      {
        fftBuf[y].real = inBuf[y*samps+x].real;
        fftBuf[y].imag = inBuf[y*samps+x].imag;
      }

      cfft1d(fftLen,fftBuf,-1);

      for (i=0; i<fftLen; i++) 
      {
        ampBuf[i] = sqrt(fftBuf[i].real*fftBuf[i].real
                    + fftBuf[i].imag*fftBuf[i].imag);        

        if(fftBuf[i].imag!=0.0 || fftBuf[i].real!=0.0)
          phsBuf[i] = atan2(fftBuf[i].imag,fftBuf[i].real);
        else
          phsBuf[i] = 0;

        if(((i>f_lo1)&&(i<f_hi1)) || ((i>f_lo2) && (i<f_hi2)))
        {
          ampBuf[i] = 0;
          phsBuf[i] = 0;
        }

        fftBuf[i].real = ampBuf[i]*cos(phsBuf[i]);
        fftBuf[i].imag = ampBuf[i]*sin(phsBuf[i]);        
      }

      cfft1d(fftLen,fftBuf,1);

      for(i=0;i<chunk_size;i++)
      {        
        outBuf[i*samps+x].real = fftBuf[i].real;
        outBuf[i*samps+x].imag = fftBuf[i].imag;
      }        
    }
    printf("Finished the FFT and Filtering Operations\n");

  /* Perform the time-domain frequency shift */ 
    if(shift != 0.0)
    {
      for(i=0; i<chunk_size; i++)
        time_vector[i] = cur_time+(1/f_s)*i;

      printf("\nPerforming time-domain frequency shift of %.2f Hz\n",shift);
      for(y=0; y<chunk_size; y++)
      {
        for(x=0; x<samps; x++)
        {
          A = outBuf[y*samps+x].real;
          B = outBuf[y*samps+x].imag;
          outBuf[y*samps+x].real = A*cos(2*pi*shift*time_vector[y])
                                   - B*sin(2*pi*shift*time_vector[y]);
          outBuf[y*samps+x].imag = B*cos(2*pi*shift*time_vector[y])
                                   + A*sin(2*pi*shift*time_vector[y]);
        }
      }
    }

  /* Write out the data file in big endian format */
    printf("Writing the output lines %d to %d in file %s\n",
      start_line, start_line+chunk_size-TOSS_SIZE, outName);
    put_complexFloat_lines(outF1, outMeta, start_line,
      samps*(chunk_size-TOSS_SIZE), outBuf+(samps*TOSS_SIZE));
  }

  printf("\n");

  FCLOSE(outF1);
  StopWatch();
  return 0;
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s <.cpx in> <.cpx out> <frequency parameter file>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <.cpx in>   Input Complex Image\n"
	"   <.cpx out>  Output Complex Image\n"
	"   <frequency parameter file>   File containing passband information.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program filters complex image data in azimuth to remove\n"
	"   non-overlapping frequencies.\n");
 printf("\n"
	"Version %.2f, ASF InSAR Tools\n"
	"\n", VERSION);
 exit(EXIT_FAILURE);
}
