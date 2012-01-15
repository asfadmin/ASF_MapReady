/****************************************************************************
*                                                                             *
*   bin2asc                                                                   *
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

#define ASF_NAME_STRING "bin2asc"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-format <format str>] [-delimiter <character>] "\
"[-nodata <value>]\n     <in> <out>\n"

#define ASF_DESCRIPTION_STRING \
"     This program converts binary data into an delimited ASCII text file.\n"

#define ASF_INPUT_STRING \
"     The input file is required and is a flat binary image in ASF internal format.\n"

#define ASF_OUTPUT_STRING \
"     The output file is required and is an ASCII text file.\n"

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
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

// Print minimalistic usage info & exit
void usage(const char *name)
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


// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *outFile, delimiterStr[25], format[25], formatStr[50];
  char delimiter, formatEnd[50];
  float nodata;
  int delimiterflag = FALSE, formatflag = FALSE, nodataflag = FALSE;
  int currArg = 1;
  int NUM_ARGS = 2;

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
    else if (strmatches(key,"-delimiter","--delimiter","-d",NULL)) {
      CHECK_ARG(1);
      strcpy(delimiterStr,GET_ARG(1));
      delimiterflag = TRUE;
    }
    else if (strmatches(key,"-format","--format","-f",NULL)) {
      CHECK_ARG(1);
      strcpy(format,GET_ARG(1));
      formatflag = TRUE;
    }
    else if (strmatches(key, "-nodata","--nodata","-nd",NULL)) {
      CHECK_ARG(1);
      nodata = atof(GET_ARG(1));
      nodataflag = TRUE;
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

  inFile = argv[currArg];
  outFile = argv[currArg+1];

  // do some actual work
  if (delimiterflag) {
    if (strcmp_case(delimiterStr, "COMMA") == 0)
      delimiter = ',';
    else if (strcmp_case(delimiterStr, "COLON") == 0)
      delimiter = ':';
    else if (strcmp_case(delimiterStr, "SEMICOLON") == 0)
      delimiter = ';';
    else if (strcmp_case(delimiterStr, "TAB") == 0)
      delimiter = '\t';
    else
      asfPrintStatus("Unsupported delimiter (%s)\n", delimiterStr);
  }
  else
    delimiter = ',';
  if (formatflag) {
    sprintf(formatStr, "%s%c", format, delimiter);
    sprintf(formatEnd, "%s\n", format);
  }
  else {
    sprintf(formatStr, "%s%c", "%.6f", delimiter);
    sprintf(formatEnd, "%s\n", "%.6f");
  }
  FILE *fpIn = FOPEN(inFile, "r");
  FILE *fpOut = FOPEN(outFile, "wt");
  meta_parameters *md = meta_read(inFile);
  int line_count = md->general->line_count;
  int sample_count = md->general->sample_count;
  float *float_value = (float *) MALLOC(sizeof(float)*sample_count);
  int ii, kk;
  for (ii=0; ii<line_count; ii++) {
    get_float_line(fpIn, md, ii, float_value);
    for (kk=0; kk<sample_count; kk++) {
      if (nodataflag && FLOAT_EQUIVALENT(float_value[kk], nodata))
	float_value[kk] = MAGIC_UNSET_DOUBLE;
    }
    for (kk=0; kk<sample_count-1; kk++)
      fprintf(fpOut, formatStr, float_value[kk]);
    fprintf(fpOut, formatEnd, float_value[sample_count-1]);
    asfLineMeter(ii, line_count);
  }
  FREE(float_value);
  meta_free(md);
  FCLOSE(fpIn);
  FCLOSE(fpOut);

  asfPrintStatus("\nDone.\n");
  
  return EXIT_SUCCESS;
}
