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

#define ASF_NAME_STRING "asf_polsarpro_import"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <dataName> <format> <polsarName> <out>\n"

#define ASF_DESCRIPTION_STRING \
"     This program ingests PolSAR Pro data into ASF internal data format.\n\n"

#define ASF_INPUT_STRING \
"     The input files are all required. The first input is the data in its\n"\
"     original format in order to extract the correct metadata. The second\n"\
"     input is format of the original data (either CEOS or AIRSAR). The\n"\
"     third input is the basename of the PolSAR Pro file.\n"

#define ASF_OUTPUT_STRING \
"     The output file is required and is the basename of the ASF internal\n"\
"     data file.\n"

#define ASF_OPTIONS_STRING \
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
#include <envi.h>
#include <airsar.h>
#include <asf_import.h>
#include <asf_raster.h>

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

const char *input_format_to_str(int input_format)
{
  switch (input_format)
    {
    case STF: return "STF";
    case CEOS: return "CEOS";
    case GENERIC_GEOTIFF: return "GENERIC GEOTIFF";
    case BIL: return "BIL";
    case GRIDFLOAT: return "GRIDFLOAT";
    case AIRSAR: return "AIRSAR";
    case VP: return "VP";
    case JAXA_L0: return "JAXA_L0";
    case ALOS_MOSAIC: return "ALOS_MOSAIC";
    default: return "UNKNOWN";
    }
}

static void ingest_airsar_polsar_amp(char *inFile, char *outFile,
				     double *p_range_scale,
				     double *p_azimuth_scale)
{
  FILE *fpIn, *fpOut;
  meta_parameters *meta = NULL;
  int ii, kk, do_resample = FALSE;
  float *power = NULL;
  double azimuth_scale, range_scale;
  char *byteBuf = NULL, *inBaseName = NULL, *p = NULL, unscaleBaseName[1024];
  char *airsarFile;

  // Check the scale factors to determine whether we need to resample
  fpIn = FOPEN(inFile, "rb");
  if (p_azimuth_scale && p_range_scale) {
    range_scale = *p_range_scale;
    azimuth_scale = *p_azimuth_scale;
    do_resample = TRUE;
  }
  if (do_resample) {
    sprintf(unscaleBaseName, "%s_unscale", outFile);
    append_ext_if_needed(unscaleBaseName, ".img", NULL);
  }
  else
    append_ext_if_needed(outFile, ".img", NULL);

  // Check whether the main metadata file is available. Only then we can
  // populate the location that is used for the geocoding of old AirSAR data
  // (processor version 3.56).
  airsarFile = STRDUP(inFile);
  inBaseName = (char *) MALLOC(sizeof(char)*1024);
  if (strstr(airsarFile, "_c.dat"))
    p = strstr(airsarFile, "_c.dat");
  else if (strstr(airsarFile, "_l.dat"))
    p = strstr(airsarFile, "_l.dat");
  else if (strstr(airsarFile, "_p.dat"))
    p = strstr(airsarFile, "_p.dat");
  if (p) {
    p[0] = '\0';
    sprintf(inBaseName, "%s", airsarFile);
    strcat(inBaseName, "_meta.airsar");
    if (!fileExists(inBaseName)) {
      FREE(inBaseName);
      inBaseName = NULL;
    }
    else
      sprintf(inBaseName, "%s", airsarFile);
  }
  else {
    FREE(inBaseName);
    inBaseName = NULL;
    asfPrintStatus("Data file (%s) is not polarimetric AirSAR data\n",
		   inFile);
  }
      
  meta = import_airsar_meta(inFile, inBaseName, TRUE);
  meta->general->data_type = REAL32;
  meta->general->band_count = 1;
  strcpy(meta->general->bands, "AMP");
  meta->general->image_data_type = IMAGE_LAYER_STACK;

  power = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  byteBuf = (char *) MALLOC(sizeof(char)*10);
  airsar_header *header = read_airsar_header(inFile);
  long offset = header->first_data_offset;
  if (do_resample)
    fpOut = FOPEN(unscaleBaseName, "wb");
  else
    fpOut = FOPEN(outFile, "wb");
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
  if (do_resample)
    meta_write(meta, unscaleBaseName);
  else
    meta_write(meta, outFile);
  if (power)
    FREE(power);
  if (byteBuf)
    FREE(byteBuf);
  if (meta)
    meta_free(meta);

  if (do_resample) {
    asfPrintStatus("Resampling with scale factors: "
		   "%lf range, %lf azimuth.\n",
		   range_scale, azimuth_scale);

    resample(unscaleBaseName, outFile, range_scale, azimuth_scale);
  }
}

void asf_polsarpro_import(char *ceosName, input_format_t format,
			  char *polsarName, int byteFlag, char *outBaseName)
{
  meta_parameters *metaIn = NULL, *metaOut = NULL;
  envi_header *envi;
  FILE *fpIn, *fpOut;
  float *floatBuf;
  double *p_azimuth_scale = NULL, *p_range_scale = NULL;
  double azimuth_scale, range_scale;
  char enviName[1024], outName[1024];
  int ii, multilook = FALSE;

  // Read the ENVI header first. We need to know the dimensions of the
  // polarimetric data first in order to resample the amplitude data to the
  // correct size.
  sprintf(enviName, "%s.hdr", polsarName);
  envi = read_envi(enviName);
  int line_count = envi->lines;
  int sample_count = envi->samples;
  if (format == CEOS)
    metaOut = meta_read(ceosName);
  else if (format == AIRSAR)
    metaOut = import_airsar_meta(ceosName, ceosName, TRUE);
  else
    asfPrintStatus("Input format (%s) not supported.\n",
		   input_format_to_str(format));

  if (line_count != metaOut->general->line_count ||
      sample_count != metaOut->general->sample_count) {
    azimuth_scale = 1.0 / (metaOut->general->line_count / line_count);
    range_scale = 1.0 / (metaOut->general->sample_count / sample_count);
    p_azimuth_scale = &azimuth_scale;
    p_range_scale = &range_scale;
    if (!FLOAT_EQUIVALENT(azimuth_scale, range_scale))
      multilook = TRUE;
  }
  meta_free(metaOut);

  // Ingest the CEOS/AirSAR data to generate an amplitude image (in case the
  // user wants to terrain correct. Will need to get the metadata anyway
  if (format == CEOS) {
    asfPrintStatus("Ingesting CEOS data ...\n");
    import_ceos(ceosName, outBaseName, "none", NULL, p_range_scale,
		p_azimuth_scale, NULL, 0, 0, -99, -99, NULL, r_AMP, FALSE,
		FALSE, FALSE, TRUE, FALSE);
  }
  else if (format == AIRSAR) {
    asfPrintStatus("Ingesting AirSAR data ...\n");
    ingest_airsar_polsar_amp(ceosName, outBaseName,
			     p_range_scale, p_azimuth_scale);
  }

  // Read the PolSAR Pro data into the layer stack
  sprintf(outName, "%s.img", outBaseName);
  metaIn = envi2meta(envi);
  metaOut = meta_read(outBaseName);
  metaOut->general->band_count = 2;
  strcat(metaOut->general->bands, ",POLSARPRO");
  floatBuf = (float *) MALLOC(sizeof(float)*metaOut->general->sample_count);

  fpIn = FOPEN(polsarName, "rb");
  fpOut = FOPEN(outName, "ab");

  // Let the reading and writing function take care of swapping bytes.
  for (ii=0; ii<metaOut->general->line_count; ii++) {
    get_float_line(fpIn, metaIn, ii, floatBuf);
    put_float_line(fpOut, metaOut, ii, floatBuf);
    asfLineMeter(ii, metaOut->general->line_count);
  }

  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(floatBuf);
  metaOut->sar->multilook = multilook;
  meta_write(metaOut, outBaseName);
  if (metaIn)
    meta_free(metaIn);
  if (metaOut)
    meta_free(metaOut);
}

// Main program body.
int
main (int argc, char *argv[])
{
  input_format_t format;
  char *dataFile, *polsarFile, *outFile, *formatStr;
  int classification = FALSE;
  int currArg = 1;
  int NUM_ARGS = 4;

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
    else if (strmatches(key,"-classification","--classification","-c",NULL)) {
      classification = TRUE;
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
  formatStr = argv[currArg+1];
  polsarFile = argv[currArg+2];
  outFile = argv[currArg+3];

  // There is currently no point in supporting anything else but CEOS and
  // AirSAR. For CEOS is assumed we are talking about PALSAR data.
  if (strncmp_case(formatStr, "CEOS", 4) == 0)
    format = CEOS;
  else if (strncmp_case(formatStr, "AIRSAR", 6) == 0)
    format = AIRSAR;
  else
    asfPrintError("Unsupported format: %s\n", formatStr);

  asf_polsarpro_import(dataFile, format, polsarFile, classification, outFile);
  asfPrintStatus("\nDone.\n");

  return EXIT_SUCCESS;
}
