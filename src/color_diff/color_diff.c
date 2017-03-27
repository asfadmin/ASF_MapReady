/********************************************************************************
 *  Mode 0: Create a color image from two bands of FBD data using a modified
 *  Kellndorfer method:
 *
 *   Use 0 as background
 *   Scale to dB (10*log10)
 *   HH: -30 dB is DN 1, -1 dB is DN 255
 *   HV: -30 dB is DN 1, -10 dB is DN 255
 *   DIV:  HH-HV, scale from -25 to -10
 *
 *  Mode 1: Create color image from three bands of PLR data using 3sigma stretch
 *
 *   Use 0 as background
 *   Scale to dB (10*log10)
 *   scale each band separately by 3-sigma
 *
 *  Written by Tom Logan, March 2014
 *  Part of the RTC Project
 *  Extended by Rudi Gens for Sentinel dual-pol browse images (March 2015)
 *  Update for use of color browse images for hazards (March 2017)
 *
 *******************************************************************************/
#include "asf_nan.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_export.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <asf_license.h>
#include <asf_contact.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
   "USAGE:\n"
   "   color_diff <inFile1> <inFile2> <outFile\n");
  printf("\n"
   "REQUIRED ARGUMENTS:\n"
   "   inFile1   Name of a pre-event floating point RGB image\n"
   "   inFile2   Name of a post-event floating point RGB image\n"
   "   outFile   Name of an RGB difference image\n");
  printf("\n"
   "OPTIONAL ARGUMENTS:\n"
   "   -tmpDir   Directory where intermediate are kept\n");
  printf("\n"
   "DESCRIPTION:\n"
   "   This program generates an RGB difference image for change detection.\n");
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

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=(char *)CALLOC(256, sizeof(char));

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k);
  return value;
}

static char *read_str(char *line, char *param)
{
  static char value[255];
  char *p = strchr(line, '=');

  // skip past the '=' sign, and eat up any whitespace
  ++p;
  while (isspace(*p))
      ++p;

  strcpy(value, p);

  // eat up trailing whitespace, too
  p = value + strlen(value) - 1;
  while (isspace(*p))
      *p-- = '\0';

  return value;
}

static float read_float(char *line, char *param)
{
  char *tmp;
  float value;

  tmp = read_str(line, param);
  sscanf(tmp, "%f", &value);

  return value;
}

static void color_diff(char *inFile1, char *inFile2, char *outFile)
{

  // Calculate the color browse image
  int ii, kk;
  meta_parameters *metaIn = meta_read(inFile1);
  int line_count = metaIn->general->line_count;
  int sample_count = metaIn->general->sample_count;
  float *red = (float *) MALLOC(sizeof(float)*sample_count);
  float *green = (float *) MALLOC(sizeof(float)*sample_count);
  float *blue = (float *) MALLOC(sizeof(float)*sample_count);
  meta_parameters *metaOut = meta_read(inFile1);
  strcpy(metaOut->general->bands, "surface,volume,volume_diff");
  metaOut->general->band_count=3;
  metaOut->general->data_type = REAL32;
  float *preGreen = (float *) MALLOC(sizeof(float)*sample_count);
  float *postRed = (float *) MALLOC(sizeof(float)*sample_count);
  float *postGreen = (float *) MALLOC(sizeof(float)*sample_count);
  float preMask, postMask, mask;

  FILE *fpIn1 = FOPEN(inFile1, "rb");
  FILE *fpIn2 = FOPEN(inFile2, "rb");
  FILE *fpOut = FOPEN(outFile, "wb");
  for (kk=0; kk<line_count; kk++) {
    get_band_float_line(fpIn1, metaIn, 1, kk, preGreen);
    get_band_float_line(fpIn2, metaIn, 0, kk, postRed);
    get_band_float_line(fpIn2, metaIn, 1, kk, postGreen);
    for (ii=0; ii<sample_count; ii++) {
      preMask = (preGreen[ii] > 0.0) ? 1.0 : 0.0;
      postMask = (postGreen[ii] > 0.0) ? 1.0 : 0.0;
      mask = preMask*postMask;
      red[ii] = postRed[ii]*255.0*mask;
      green[ii] = postGreen[ii]*255.0*mask;
      blue[ii] = 5.0*(postGreen[ii] - preGreen[ii])*255.0*mask;
    }
    put_band_float_line(fpOut, metaOut, 0, kk, red);
    put_band_float_line(fpOut, metaOut, 1, kk, green);
    put_band_float_line(fpOut, metaOut, 2, kk, blue);
    asfLineMeter(kk, line_count);
  }
  FCLOSE(fpIn1);
  FCLOSE(fpIn2);
  FCLOSE(fpOut);
  meta_free(metaIn);
  meta_write(metaOut, outFile);
  meta_free(metaOut);
  FREE(preGreen);
  FREE(postRed);
  FREE(postGreen);
  FREE(red);
  FREE(green);
  FREE(blue);
}

int main(int argc,char *argv[])
{
  char inFile1[256], inFile2[256], outFile[256];
  extern int currArg;

  // Parse command line
  if ((argc-currArg)<1) {
    printf("Insufficient arguments.\n");
    usage("");
  }

  while (currArg < (argc-2)) {
    char *key = argv[currArg++];
    if (strmatches(key, "-log", "--log", NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key, "-quiet", "--quiet", "-q", NULL))
      quietflag = TRUE;
    else {
      --currArg;
      break;
    }
  }
  if ((argc-currArg) < 2) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  }

  create_name(inFile1, argv[1], ".img");
  create_name(inFile2, argv[2], ".img");
  create_name(outFile, argv[3], ".img");

  color_diff(inFile1, inFile2, outFile);

  asfPrintStatus("Done.\n");
  exit(EXIT_SUCCESS);
}
