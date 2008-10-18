/****************************************************************************
*                                                                             *
*   Data_qc verifies the validity of a CEOS data set                          *
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

#define ASF_NAME_STRING "asf_gamma_import"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-interferogram <interferogram>]\n"\
"     [-coherence <coherence>] <amplitude> <metadata> <out>\n"

#define ASF_DESCRIPTION_STRING \
"     This program ingests GAMMA data sets into ASF internal data format.\n\n"

#define ASF_INPUT_STRING \
"     The input files is required. The first input is the amplitude data\n"\
"     file and the second input is the GAMMA format parameter file,\n"\
"     containing the accompanying metadata.\n"

#define ASF_OUTPUT_STRING \
"     The output file is required and is the basename of the ASF internal\n"\
"     data file.\n"

#define ASF_OPTIONS_STRING \
"     -interferogram\n"\
"          Stacks an interferogram onto an amplitude image.\n"\
"     -coherence\n"\
"          Stacks an coherence onto an amplitude image.\n"\
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -quiet\n"\
"          Supresses all non-essential output.\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#include <asf.h>
#include <asf_endian.h>
#include <asf_meta.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <gamma.h>

// Print minimalistic usage info & exit
static void usage(const char *name)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  exit(EXIT_FAILURE);
}

// Print the help info & exit
static void print_help(void)
{
  asfPrintStatus(
      "\n"
      "Tool name:\n   " ASF_NAME_STRING "\n\n"
      "Usage:\n" ASF_USAGE_STRING "\n"
      "Description:\n" ASF_DESCRIPTION_STRING "\n"
      "Input:\n" ASF_INPUT_STRING "\n"
      "Output:\n" ASF_OUTPUT_STRING "\n"
      "Options:\n" ASF_OPTIONS_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_SUCCESS);
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

typedef enum {
  ISP,
  MSP
} gamma_type_t;

void asf_gamma_import(char *dataName, char *metaName, char *outBaseName,
		      char *interferogram, char *coherence)
{
  gamma_type_t gamma_type;
  meta_parameters *metaIn, *metaOut;
  FILE *fpIn, *fpOut;
  float *floatBuf, *floatAmpBuf, *floatPhaseBuf, amp;
  complexFloat *floatCpxBuf;
  char *outFile, line[1024], reason[1024]="", tmp[255];
  int ii, kk, bands, status=TRUE;

  // Check interferogram and coherence files. The amplitude of the
  // interferogram contains the coherence, so there is not point of passing in
  // a coherence image as well.
  if (interferogram && coherence) {
    FREE(coherence);
    coherence = NULL;
  }

  // Check the existence of files
  if (!fileExists(dataName)) {
    sprintf(tmp, "Missing amplitude image file (%s)\n", dataName);
    strcat(reason, tmp);
    status = FALSE;
  }
  else 
    bands = 1;
  if (!fileExists(metaName)) {
    sprintf(tmp, "Missing metadata file (%s)\n", metaName);
    strcat(reason, tmp);
    status = FALSE;
  }
  if (interferogram && !fileExists(interferogram)) {
    sprintf(tmp, "Missing interferogram file (%s)\n", interferogram);
    strcat(reason, tmp);
    status = FALSE;
  }
  else if (interferogram)
    bands += 2;
  if (coherence && !fileExists(coherence)) {
    sprintf(tmp, "Missing coherence file (%s)\n", coherence);
    strcat(reason, tmp);
    status = FALSE;
  }
  else if (coherence)
    bands++;

  // Check out which kind of gamma parameter file we are dealing with
  if (status) {
    fpIn = FOPEN(metaName, "r");
    fgets(line, 1024, fpIn);
    if (strncmp_case(line, 
		     "GAMMA INTERFEROMETRIC SAR PROCESSOR (ISP)", 41) == 0)
      gamma_type = ISP;
    else if (strncmp_case(line, "GAMMA MODULAR SAR PROCESSOR (MSP)", 33) == 0)
      gamma_type = MSP;
    else {
      strcat(reason, "Metadata file contains Unknown GAMMA parameter file "
	     "format.\n");
      status = FALSE;
    }
    FCLOSE(fpIn);
  }

  // Do you thing or report errors
  if (status) {
    if (gamma_type == MSP)
      asfPrintError("GAMMA MSP data format implemented but not tested yet!\n");
    else if (gamma_type == ISP) {

      // Read the metadata parameter file. We will update the metadata
      // structure as we go along
      metaIn = meta_read_gamma_isp(metaName, "FLOAT", "IMAGE_LAYER_STACK");
      metaOut = meta_read_gamma_isp(metaName, "FLOAT", "IMAGE_LAYER_STACK");
      metaOut->general->band_count = bands;
      int sample_count = metaOut->general->sample_count;
      int line_count = metaOut->general->line_count;
      int current_band = 0;

      // The first band is always going to be the amplitude image. Otherwise
      // we can't guarantee terrain correction and such.
      floatBuf = (float *) MALLOC(sizeof(float)*sample_count);
      outFile = appendExt(outBaseName, ".img");
      fpIn = FOPEN(dataName, "rb");
      fpOut = FOPEN(outFile, "wb");
      asfPrintStatus("Writing amplitude image ...\n");
      for (ii=0; ii<line_count; ii++) {
	get_float_line(fpIn, metaIn, ii, floatBuf);
	for (kk=0; kk<sample_count; kk++) {
	  amp = sqrt(floatBuf[kk]);
	  floatBuf[kk] = amp;
	}
	put_band_float_line(fpOut, metaOut, current_band, ii, floatBuf);
	asfLineMeter(ii, line_count);
      }
      FCLOSE(fpIn);
      FREE(floatBuf);
      strcpy(metaOut->general->bands, "AMP");

      // Lets add a coherence image. This is the simple case, because it
      // comes as floating point.
      if (coherence) {
	fpIn = FOPEN(coherence, "rb");
	current_band++;
	asfPrintStatus("\nWriting coherence image ...\n");
	floatBuf = (float *) MALLOC(sizeof(float)*sample_count);
	for (ii=0; ii<line_count; ii++) {
	  get_float_line(fpIn, metaIn, ii, floatBuf);
	  put_band_float_line(fpOut, metaOut, current_band, ii, floatBuf);
	  asfLineMeter(ii, line_count);
	}
	FCLOSE(fpIn);
	FREE(floatBuf);
	strcat(metaOut->general->bands, ",COHERENCE");
      }

      // Lets add an interferogram. This is a little trickier, since it comes
      // in complex form, and we need to store it a two bands
      if (interferogram) {
	fpIn = FOPEN(interferogram, "rb");
	current_band++;
	asfPrintStatus("\nWriting coherence image and interferogram ...\n");
	floatCpxBuf = 
	  (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count);
	floatAmpBuf = (float *) MALLOC(sizeof(float)*sample_count);
	floatPhaseBuf = (float *) MALLOC(sizeof(float)*sample_count);
	metaIn->general->data_type = COMPLEX_REAL32;
	for (ii=0; ii<line_count; ii++) {
	  get_complexFloat_line(fpIn, metaIn, ii, floatCpxBuf);
	  for (kk=0; kk<sample_count; kk++) {
	    float re = floatCpxBuf[kk].real;
	    float im = floatCpxBuf[kk].imag;
	    if (re != 0.0 || im != 0.0) {
	      floatAmpBuf[kk] = sqrt(re*re + im*im);
	      floatPhaseBuf[kk] = atan2(im, re);
	    } 
	    else
	      floatAmpBuf[kk] = floatPhaseBuf[kk] = 0.0;
	  }
	  put_band_float_line(fpOut, metaOut, current_band, ii, floatAmpBuf);
	  put_band_float_line(fpOut, metaOut, current_band+1, ii, floatPhaseBuf);
	  asfLineMeter(ii, line_count);
	}
	current_band++; // accounting for second band
	FCLOSE(fpIn);
	FREE(floatCpxBuf);
	FREE(floatAmpBuf);
	FREE(floatPhaseBuf);
	strcat(metaOut->general->bands, 
	       ",COHERENCE,INTERFEROGRAM");
      }
      FCLOSE(fpOut);
      FREE(interferogram);

      // Time to write the metadata file
      metaOut->general->image_data_type = IMAGE_LAYER_STACK;
      meta_write(metaOut, outFile);

      // Clean up time
      meta_free(metaIn);
      meta_free(metaOut);
    }
  }
  else
    asfPrintError("Ingest of GAMMA data failed!\n%s\n", reason);
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *dataFile, *metaFile, *outFile, *interferogram=NULL, *coherence=NULL;
  int currArg = 1;
  int NUM_ARGS = 3;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      
    }
    else if (strmatches(key,"-interferogram","--interferogram","-i",NULL)) {
      CHECK_ARG(1);
      interferogram = (char *) MALLOC(sizeof(char)*512);
      strcpy(interferogram, GET_ARG(1));
    }
    else if (strmatches(key,"-coherence","--coherence","-c",NULL)) {
      CHECK_ARG(1);
      coherence = (char *) MALLOC(sizeof(char)*512);
      strcpy(coherence, GET_ARG(1));
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

  dataFile = argv[currArg];
  metaFile = argv[currArg+1];
  outFile = argv[currArg+2];

  asf_gamma_import(dataFile, metaFile, outFile, interferogram, coherence);
  asfPrintStatus("\nDone.\n");
  
  return EXIT_SUCCESS;
}
