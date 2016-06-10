/******************************************************************************
*                                                                             *
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
#include "asf_nan.h"
#include "asf_endian.h"
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_license.h"
#include "dateUtil.h"
#include "time.h"
#include <string.h>
#include <hdf5.h>

#define ASF_NAME_STRING "cleanup_pixel"
#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
	 "USAGE:\n"
	 "   %s [ -log <logFile> ][ -distance <distance> ] <tolerance>\n"
	 "   <oldValue> <newValue> <inFile> <outFile>\n", name);
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
	 "   distance    Number of pixels to the next backgound pixel.\n");
  printf("\n"
	 "REQUIRED ARGUMENTS:\n"
	 "   tolerance   Tolerance between pixel value and reference pixel value.\n"
	 "   oldValue    Old pixel value to be replaced.\n"
	 "   newValue    New pixel value to be used instead.\n"
	 "   inFile      Name of the input file.\n"
	 "   outFile     Name of the output file.\n");
  printf("\n"
	 "DESCRIPTION:\n"
	 "   %s cleans up pixel values.\n",
	 name);
  printf("\n"
	 "Version %.2f, ASF SAR Tools\n"
	 "\n",VERSION);
  exit(EXIT_FAILURE);
}

int strmatches(const char *key, ...)
{
    va_list ap;
    char *arg = NULL;
    int found = FALSE;

    va_start(ap, key);
    do {
        arg = va_arg(ap, char *);
        if (arg) {
            if (strcmp(key, arg) == 0) {
                found = TRUE;
                break;
            }
        }
    } while (arg);

    return found;
}


int main(int argc, char **argv)
{
  char inFile[1024], outFile[1024];
  int ii, jj, kk, ll, mm, distance = 0, currArg = 1;
  int NUM_ARGS = 5;

  if (argc<=1)
      usage(ASF_NAME_STRING);

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-distance","--distance",NULL)) {
      CHECK_ARG(1);
      distance = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else {
      --currArg;
      break;
    }
  }

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  float tolerance = atof(argv[currArg]);
  float oldValue = atof(argv[currArg+1]);
  float newValue = atof(argv[currArg+2]);
  sprintf(inFile, "%s", argv[currArg+3]);
  sprintf(outFile, "%s", argv[currArg+4]);

  asfSplashScreen(argc, argv);

  FILE *fpIn = FOPEN(inFile, "rb");
  FILE *fpOut = FOPEN(outFile, "wb");
  meta_parameters *meta = meta_read(inFile);
  meta_write(meta, outFile);

  int sample_count = meta->general->sample_count;
  int line_count = meta->general->line_count;
  int band_count = meta->general->band_count;
  int size = distance*2 + 1;
  int background = FALSE;
  
  float *inbuf = (float *) MALLOC(sizeof(float)*sample_count*size);
  float *outbuf = (float *) MALLOC(sizeof(float)*sample_count);

  for (kk=0; kk<band_count; kk++) {
    for (ii=0; ii<distance; ii++) {
      get_band_float_line(fpIn, meta, kk, ii, outbuf);
      put_band_float_line(fpOut, meta, kk, ii, outbuf);
    }
    for (ii=distance; ii<line_count-distance; ii++) {
      get_band_float_lines(fpIn, meta, kk, ii-distance, size, inbuf);
      for (jj=0; jj<distance; jj++)
  	    outbuf[jj] = inbuf[jj];
      for (jj=distance; jj<sample_count-distance; jj++) {
        if (distance == 0)
          background = TRUE;
        else
          background = FALSE;
        for (ll=0; ll<size; ll++)
          for (mm=jj-distance; mm<jj+distance; mm++)
            if (FLOAT_EQUIVALENT(inbuf[mm+ll*sample_count], oldValue))
              background = TRUE;
        if (background && 
          fabs(inbuf[jj+distance*sample_count] - oldValue) < tolerance)
  	      outbuf[jj] = newValue;
  	    else
  	      outbuf[jj] = inbuf[jj+distance*sample_count];
      }
      for (jj=sample_count-distance; jj<sample_count; jj++)
  	    outbuf[jj] = inbuf[jj];
      put_band_float_line(fpOut, meta, kk, ii, outbuf);
      asfLineMeter(ii, line_count);
    }
    for (ii=line_count-distance; ii<line_count; ii++) {
      get_band_float_line(fpIn, meta, kk, ii, outbuf);
      put_band_float_line(fpOut, meta, kk, ii, outbuf);
      asfLineMeter(ii, line_count);
    }
  }
  FREE(inbuf);
  FREE(outbuf);
  FCLOSE(fpIn);
  FCLOSE(fpOut);

  return 0;
}

