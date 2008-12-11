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

#define ASF_NAME_STRING "asf_airsar_import"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-polarimetry_stack] [-insar_stack] <in> <out>\n"

#define ASF_DESCRIPTION_STRING \
"     This program ingest AirSAR data into ASF internal format with more\n"\
"     with more options than the regular asf_import or MapReady.\n"

#define ASF_INPUT_STRING \
"     The input file is required and is the basename of the AirSAR data.\n"\
"     For interferometric AirSAR data the ingest is limited to a particular\n"\
"     frequency. For polarimetric data it should not include a frequency.\n"

#define ASF_OUTPUT_STRING \
"     The output file is required and is the basename of the ASF internal\n"\
"     data file.\n"

#define ASF_OPTIONS_STRING \
"     -polarimetry_stack\n"\
"          Ingests the amplitude images from all polarimetric images into a "\
"multiband image.\n"\
"     -insar_stack\n"\
"          Ingests all interferometric data sets of one polarization into\n"\
"          a multiband image.\n"\
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
#include <airsar.h>
#include <asf_import.h>

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

static void ingest_insar_data(char *inFile, char *inBaseName, char *band,
			      int size, int line_count, char *outFile, 
			      int create)
{
  airsar_header *header;
  meta_parameters *metaIn = NULL, *metaOut = NULL;
  FILE *fpIn, *fpOut;
  int ii, kk, line_offset;
  float *floatBuf;
  char tmp[10];
  
  fpIn = FOPEN(inFile, "rb");
  append_ext_if_needed(outFile, ".img", NULL);
  if (create)
    fpOut = FOPEN(outFile, "wb");
  else
    fpOut = FOPEN(outFile, "ab");

  if (create) {
    metaOut = import_airsar_meta(inFile, inBaseName, TRUE);
    metaOut->general->data_type = REAL32;
    metaOut->general->band_count = 1;
    sprintf(metaOut->general->bands, "%s", band);
    metaOut->general->image_data_type = IMAGE_LAYER_STACK;
  }
  else {
    metaOut = meta_read(outFile);
    metaOut->general->band_count += 1;
    sprintf(tmp, ",%s", band);
    strcat(metaOut->general->bands, tmp);
  }      

  header = read_airsar_header(inFile);
  metaIn = import_airsar_meta(inFile, inBaseName, TRUE);
  line_offset = header->first_data_offset/metaIn->general->sample_count/size;
  metaIn->general->line_count = line_count + line_offset;
  if (size == 2)
    metaIn->general->data_type = INTEGER16;
  else if (size == 1)
    metaIn->general->data_type = BYTE;
  floatBuf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
  for (ii=0; ii<line_count; ii++) {
    get_float_line(fpIn, metaIn, ii+line_offset, floatBuf);
    if (strcmp_case(band, "DEM") == 0) {
      for (kk=0; kk<metaIn->general->sample_count; kk++)
	floatBuf[kk] = floatBuf[kk]*metaIn->airsar->elevation_increment +
	  metaIn->airsar->elevation_offset;
    }
    put_float_line(fpOut, metaOut, ii, floatBuf);
    asfLineMeter(ii, line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  meta_write(metaOut, outFile);

  if (floatBuf)
    FREE(floatBuf);
  if (metaIn)
    meta_free(metaIn);
  if (metaOut)
    meta_free(metaOut);
}

static void ingest_polarimetry_data(char *inFile, char *inBaseName, 
				    char *outFile, char band, int create)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta = NULL;
  char tmp[10];
  int ii, kk;
  float *power = NULL;
  char *byteBuf = NULL;
  
  fpIn = FOPEN(inFile, "rb");
  append_ext_if_needed(outFile, ".img", NULL);
  if (create)
    fpOut = FOPEN(outFile, "wb");
  else
    fpOut = FOPEN(outFile, "ab");
  
  if (create) {
    meta = import_airsar_meta(inFile, inBaseName, TRUE);
    meta->general->data_type = REAL32;
    meta->general->band_count = 1;
    sprintf(meta->general->bands, "AMP-%c", band);
    meta->general->image_data_type = IMAGE_LAYER_STACK;
  }
  else {
    meta = meta_read(outFile);
    meta->general->band_count += 1;
    sprintf(tmp, ",AMP-%c", band);
    strcat(meta->general->bands, tmp);
  }      

  power = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  byteBuf = (char *) MALLOC(sizeof(char)*10);
  airsar_header *header = read_airsar_header(inFile);
  long offset = header->first_data_offset;
  FSEEK(fpIn, offset, SEEK_SET);
  for (ii=0; ii<meta->general->line_count; ii++) {
    for (kk=0; kk<meta->general->sample_count; kk++) {
      FREAD(byteBuf, sizeof(char), 10, fpIn);
      power[kk] = sqrt(((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]));
    }
    put_float_line(fpOut, meta, ii, power);
    asfLineMeter(ii, meta->general->line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  meta_write(meta, outFile);
  if (power)
    FREE(power);
  if (byteBuf)
    FREE(byteBuf);
  if (meta)
    meta_free(meta);
}

void asf_airsar_import(char *inFile, char *outFile, int insar, int polar)
{
  airsar_header *header = NULL;
  meta_parameters *metaIn = NULL, *metaOut = NULL;
  FILE *fpIn, *fpOut;
  float *floatBuf;
  char dataName[1024], *airsar_basename=NULL;
  int ii, kk, create=TRUE, line_count;
  int dem=FALSE, amp=FALSE, coh=FALSE;
  long line_offset;

  // First check for the existence of the input file. In this case we ingest
  // a single AirSAR file and ignore any other option that is passed in.
  if (fileExists(inFile) && strstr(inFile, "_meta.airsar") == NULL) {

    // Check what kind of data it is
    if (strstr(inFile, ".demi2"))
      dem = TRUE;
    else if (strstr(inFile, ".vvi2"))
      amp = TRUE;
    else if (strstr(inFile, ".corgr"))
      coh = TRUE;
    // FIX ME: Does not deal with polarimetric data yet

    header = read_airsar_header(inFile);
    metaIn = import_airsar_meta(inFile, airsar_basename, TRUE);
    line_offset = header->first_data_offset/metaIn->general->sample_count/2;
    metaIn->general->line_count += line_offset;
    metaOut = import_airsar_meta(inFile, airsar_basename, TRUE);

    // Assign appropriate data type
    if (dem || amp)
      metaIn->general->data_type = INTEGER16;
    else 
      metaIn->general->data_type = BYTE;

    metaOut->general->data_type = REAL32;
    floatBuf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
    fpIn = FOPEN(inFile, "rb");
    append_ext_if_needed(outFile, ".img", NULL);
    fpOut = FOPEN(outFile, "wb");
    for (ii=0; ii<metaOut->general->line_count; ii++) {

      // Interferometric data can be read using get_float_line
      if (dem || amp || coh)
	get_float_line(fpIn, metaIn, ii+line_offset, floatBuf);

      // DEM needs some additional treatment      
      if (dem) {
	for (kk=0; kk<metaIn->general->sample_count; kk++)
	  floatBuf[kk] = floatBuf[kk]*metaIn->airsar->elevation_increment +
	    metaIn->airsar->elevation_offset;
      }

      // Writing away should work the same way
      put_float_line(fpOut, metaOut, ii, floatBuf);
      asfLineMeter(ii, metaOut->general->line_count);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    if (dem) {
      metaOut->general->image_data_type = DEM;
      strcpy(metaOut->general->bands, "DEM");
    }
    else if (amp) {
      metaOut->general->image_data_type = AMPLITUDE_IMAGE;
      strcpy(metaOut->general->bands, "AMPLITUDE");
    }
    else if (coh) {
      metaOut->general->image_data_type = COHERENCE_IMAGE;
      strcpy(metaOut->general->bands, "COHERENCE");
    }
    meta_write(metaOut, outFile);
  }
  // Take care of interferometric layer stack
  else if (insar) {

    // Check for DEM
    sprintf(dataName, "%s.demi2", inFile);
    if (!fileExists(dataName))
      asfPrintError("Could not find DEM (%s)\nDEM is required for reliable"
		    "geometry and geolocation\n", dataName);
    else {
      printf("Ingesting DEM (%s) ...\n", dataName);
      metaIn = import_airsar_meta(dataName, inFile, TRUE);
      line_count = metaIn->general->line_count;
      ingest_insar_data(dataName, inFile, "DEM", sizeof(short), line_count,
			outFile, create);
      create = FALSE;
    }

    // Check for amplitude image
    sprintf(dataName, "%s.vvi2", inFile);
    if (!fileExists(dataName))
      asfPrintWarning("Could not find amplitude image (%s)\n", dataName);
    else {
      printf("Ingesting amplitude image (%s) ...\n", dataName);
      ingest_insar_data(dataName, inFile, "AMPLITUDE", sizeof(short), 
			line_count, outFile, create);
      create = FALSE;
    }

    // Check for coherence image
    sprintf(dataName, "%s.corgr", inFile);
    if (!fileExists(dataName))
      asfPrintWarning("Could not find coherence image (%s)\n", dataName);
    else {
      printf("Ingesting coherence image (%s) ...\n", dataName);
      ingest_insar_data(dataName, inFile, "COHERENCE", sizeof(char), 
			line_count, outFile, create);
      create = FALSE;
    }    
  }
  // Take care of polarimetric layer stack
  // We will just put all available power images into a multiband image
  else if (polar) {

    // Check for C-band data
    sprintf(dataName, "%s_c.datgr", inFile);
    if (!fileExists(dataName))
      sprintf(dataName, "%s_c.dat", inFile);
    if (!fileExists(dataName))
      asfPrintWarning("Could not find polarimetric data set (%s)\n",
		      dataName);
    else {
      printf("Ingesting C band data (%s) ...\n", dataName);
      ingest_polarimetry_data(dataName, inFile, outFile, 'C', create);
      create = FALSE;
    }

    // Check for L-band data
    sprintf(dataName, "%s_l.datgr", inFile);
    if (!fileExists(dataName))
      sprintf(dataName, "%s_l.dat", inFile);
    if (!fileExists(dataName))
      asfPrintWarning("Could not find polarimetric data set (%s)\n",
		      dataName);
    else {
      printf("Ingesting L band data (%s) ...\n", dataName);
      ingest_polarimetry_data(dataName, inFile, outFile, 'L', create);
      create = FALSE;
    }

    // Check for P-band data
    sprintf(dataName, "%s_p.datgr", inFile);
    if (!fileExists(dataName))
      sprintf(dataName, "%s_p.dat", inFile);
    if (!fileExists(dataName))
      asfPrintWarning("Could not find polarimetric data set (%s)\n",
		      dataName);
    else {
      printf("Ingesting P band data (%s) ...\n", dataName);
      ingest_polarimetry_data(dataName, inFile, outFile, 'P', create);
      create = FALSE;
    }    
  }
  // Regular ingest
  else 
    import_airsar(inFile, r_AMP, outFile);

  // Clean up time
  if (metaIn)
    meta_free(metaIn);
  if (metaOut)
    meta_free(metaOut);
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *outFile;
  int currArg = 1;
  int NUM_ARGS = 2;
  int insar=FALSE, polarimetry=FALSE;

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
      quietflag = TRUE;
    }
    else if (strmatches(key,"-insar_stack","--insar_stack","-i",NULL)) {
      insar = TRUE;
    }
    else if (strmatches(key,"-polarimetry_stack","--polarimetry_stack","-p",NULL)) {
      polarimetry = TRUE;
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

  inFile = argv[currArg];
  outFile = argv[currArg+1];

  asf_airsar_import(inFile, outFile, insar, polarimetry);
  asfPrintStatus("\nDone.\n");
  
  return EXIT_SUCCESS;
}
