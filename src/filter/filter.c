/*********************************************************************
NAME:     filter.c - filters 

SYNOPSIS: filter [ -log <file> ] -resample <size> | -kernel <type> <size> 
                 <infile> <outfile>

DESCRIPTION:

*********************************************************************/

/******************************************************************************
*								              *
*   filter.c - resamples images and optionally subsamples them                *
*	                                                                      *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
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
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "kernel.h"
#include <ctype.h>

#define VERSION 1.0
#define REQUIRED_ARGS 2
#define BROWSE 512

/* usage - enter here on command-line usage error*/
static
void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <file> ] -resample <size> | -kernel <type> <size>\n"
         "   <inFile> <outFile>\n", name);
  printf("\n"
	 "OPTIONAL ARGUMENTS:\n"
 	 "   -log <file> Allows the output to be written to a log file.\n");
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
	 "   -resample <size>      Subsampling with given kernel size.\n"
         "   -kernel <type> <size> Kernel type: average\n"
         "               Size of the kernel\n"
         "   inFile      Basename of the input file.\n"
	 "   outFile     Basename of the output file.\n");
  printf("\n"
	 "DESCRIPTION:\n"
 	 "   %s filters an image with a variety of kernels (only average for the\n"
         "   format). It opitionally subsamples the image to a smaller size\n",name);
  printf("\n"
	 "Version %.2f, ASF SAR TOOLS\n\n", VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
  FILE *fpIn, *fpOut;
  meta_parameters *inMeta, *outMeta;
  char inFile[255], outFile[255], filter_type[25];
  float *inbuf, *outbuf, percent=5.0, base, rate;
  int inLines, inSamples, outLines, outSamples, kernel_size, ii, jj; 
  int resampleFlag=0, browseFlag=0, filter=0, xii=0, yii=0;
  int half, numLines, startLine;
  extern int currArg; /* from cla.h in asf.h */

  logflag = FALSE;
  
  /* Parse command line args */
  while (currArg < (argc-REQUIRED_ARGS)) {
    char *key=argv[currArg++];
    /* check for required options */
    if (strmatch(key,"-kernel")) {
      CHECK_ARG(2);
      strcpy(filter_type,GET_ARG(2));
      kernel_size = strtod(GET_ARG(1),NULL);
    }
    else if (strmatch(key,"-resample")) {
      CHECK_ARG(1);
      kernel_size = strtod(GET_ARG(1),NULL);
      sprintf(filter_type, "average");
      resampleFlag = TRUE;
    }
    else if (strmatch(key,"-browse")) {
      sprintf(filter_type, "average");
      resampleFlag = TRUE;
      browseFlag = TRUE;
    }
    /* check for other options */
    else if (strmatch(key,"-log")) {
      CHECK_ARG(1); /*one string argument: log file */
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile,"a");
      logflag=TRUE;
    }
    else {
      printf("\n   ** Invalid option:  %s\n\n",argv[currArg-1]);
      usage(argv[0]);
    }
  }
  if ((argc-currArg) < REQUIRED_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  /* Read required arguments */
  strcpy(inFile,argv[currArg]);
  strcpy(outFile,argv[currArg+1]);

  /* Lets get started */
  system("date");
  printf("Program: filter\n\n");
  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: filter\n\n");
  }

  /* Assign filter type */
  if (strncmp(uc(filter_type), "AVERAGE", 7)==0) filter = AVERAGE;
  if (strncmp(uc(filter_type), "GAMMA_MAP", 9)==0) filter = GAMMA_MAP;
  /*** other filters to come here ***/
  else {
    sprintf(errbuf, "   ERROR: Unsupported filter type '%s'\n", filter_type);
    printErr(errbuf);
  }

  /* Check kernel size - must be odd number except for average filter */ 
  if (kernel_size%2==0 && filter!=AVERAGE) {
    kernel_size--;
    sprintf(logbuf, 
	    "   WARNING: Kernel must have an odd number of lines and samples!\n"
	    "            Kernel size reduced to %dx%d\n", kernel_size, kernel_size);
    printf(logbuf);
    if (logflag) printLog(logbuf);
  } 

  /* Create metadata */
  inMeta = meta_read(inFile);
  outMeta = meta_read(inFile);
  
  inLines = inMeta->general->line_count;
  inSamples = inMeta->general->sample_count;
  base = 1.0 / (2.0 / kernel_size) - 0.5;
  rate = 1.0 * kernel_size;
  half = (kernel_size - 1) / 2;
  numLines = kernel_size;
  
  /* Open output files */
  fpIn = fopenImage(inFile,"rb");
  fpOut = fopenImage(outFile,"wb");
  
  /* Filtering with resampling */
  if (resampleFlag || browseFlag) {

    /* Determine kernel_size for browse image option */
    if (browseFlag) {
      if (inMeta->general->line_count <= inMeta->general->sample_count)
	kernel_size = inMeta->general->line_count / BROWSE;
      else 
	kernel_size = inMeta->general->sample_count / BROWSE;
      base = 1.0 / (2.0 / kernel_size) - 0.5;
      rate = 1.0 * kernel_size;
      half = (kernel_size - 1) / 2;
      numLines = kernel_size;
    }

    /* Calculate the parameters of the output image */
    sprintf(logbuf, "   Resampling image with average kernel (%dx%d)\n\n",
	    kernel_size, kernel_size);
    printf(logbuf);
    if (logflag) printLog(logbuf);

    outLines = (inLines) / kernel_size;
    outSamples = (inSamples) / kernel_size;
    outMeta->general->line_count = outLines;
    outMeta->general->sample_count = outSamples;
    outMeta->general->x_pixel_size *= kernel_size;
    outMeta->general->y_pixel_size *= kernel_size;

    /* Allocate memory for input and output file */
    inbuf= (float*) MALLOC (kernel_size*inSamples*sizeof(float));
    outbuf = (float*) MALLOC (outSamples*sizeof(float));

    /* Resampling */
    for (ii=0; ii<outLines; ii++) {
      /*** Read next set of lines for kernel ***/
      yii = ii * rate + base + 0.5;
      startLine = yii - half;
      if (startLine<0) startLine = 0;
      if (inLines < (kernel_size+startLine)) numLines = inLines - startLine;
      get_float_lines(fpIn, inMeta, startLine, numLines, inbuf); 
      
      /*** Calculate the output line and write to disk ***/
      for (jj=half; jj<outSamples-half; jj++) {
	xii = jj * rate + base + 0.5;
	outbuf[jj] = kernel(filter,inbuf,numLines,inSamples,xii,kernel_size);
      }
      put_float_line(fpOut, outMeta, ii, outbuf);
      if ((yii*100/inLines)>percent) {
	printf("   Completed %3.0f percent\n",percent);
	percent+=5.0;
      }
    }
    printf("   Completed 100 percent\n\n");
    FCLOSE(fpOut);
    FCLOSE(fpIn);

  }

  /* Filtering without resampling */
  else {

    sprintf(logbuf, "   Kernel type: %s\n   Kernel size: %dx%d\n\n",
	    filter_type, kernel_size, kernel_size);
    printf(logbuf);
    if (logflag) printLog(logbuf);
    
    /* Allocate memory for input and output file */
    inbuf= (float*) MALLOC (kernel_size*inSamples*sizeof(float));
    outbuf = (float*) MALLOC (inSamples*sizeof(float));
  
    /* Set upper margin of image to input pixel values */
    for (ii=0; ii<half; ii++) {
      for (jj=0; jj<inSamples; jj++) outbuf[jj] = inbuf[jj];
      put_float_line(fpOut, outMeta, ii, outbuf);
    }

    /* Filtering the 'regular' lines */
    for (ii=half; ii<inLines-half; ii++) {

      /*** Read next set of lines for kernel ***/
      startLine = ii - half;
      if (startLine<0) startLine = 0;
      if (inLines < (kernel_size+startLine)) numLines = inLines - startLine;
      get_float_lines(fpIn, inMeta, startLine, numLines, inbuf); 

      /*** Calculate the usual output line ***/
      for (jj=0; jj<half; jj++) outbuf[jj] = inbuf[jj];
      for (jj=half; jj<inSamples-half; jj++) {
	outbuf[jj] = kernel(filter,inbuf,numLines,inSamples,jj,kernel_size);
      }
      for (jj=inSamples-half; jj<inSamples; jj++) outbuf[jj] = inbuf[jj];

      /*** Write line to disk ***/
      put_float_line(fpOut, outMeta, ii, outbuf);
      if ((ii*100/inLines)>percent) {
	printf("   Completed %3.0f percent\n",percent);
	percent+=5.0;
      }
    }

    /* Set lower margin of image to input pixel values */
    for (ii=inLines-half; ii<inLines; ii++) {
      for (jj=0; jj<inSamples; jj++) outbuf[jj] = inbuf[jj];
      put_float_line(fpOut, outMeta, ii, outbuf);
    }

    printf("   Completed 100 percent\n\n");
    FCLOSE(fpOut);
    FCLOSE(fpIn);
  }
  
  /* Write metadata */
  outMeta->general->data_type = REAL32;
  meta_write(outMeta,outFile);

  return(0);
}


