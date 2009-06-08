#define ASF_NAME_STRING "write_ppf"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <leader file> <ambiguity>\n"

#define ASF_DESCRIPTION_STRING \
"     This program generates a parameter file for processing Radarsat-1\n"\
"     data with a Doppler ambiguity.\n"

#define ASF_INPUT_STRING \
"     The input file are required, and should be the CEOS leader file and\n"\
"     the ambiguity that should be applied in the processing.\n"

#define ASF_OPTIONS_STRING \
"     -log <log file>\n"\
"          Output will be written to a specified log file.\n"\
"\n"\
"     -license\n"\
"          Print copyright and license for this software then exit.\n"\
"\n"\
"     -version\n"\
"          Print version and copyright then exit.\n"\
"\n"\
"     -help\n"\
"          Print a help page and exit.\n"

#include <stdio.h>
#include "asf_import.h"
#include "asf_meta.h"
#include "ceos.h"
#include "asf_license.h"
#include "asf_contact.h"

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
  FILE *fp;
  struct dataset_sum_rec dssr;
  meta_parameters *meta;
  char *inFile, *outFile, type[5];
  double ambiguity;
  int currArg = 1;
  int NUM_ARGS = 2;
  double zero = 0.0;

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

  // Take care of the input
  inFile = argv[currArg];
  sscanf(argv[currArg+1], "%lf", &ambiguity);

  // Assign output name
  char *in_base = get_basename(inFile);
  outFile = (char *) MALLOC(sizeof(char)*512);
  sprintf(outFile, "%s.prm", in_base);

  // Get metadata
  get_dssr(inFile, &dssr);
  meta = meta_read_only(inFile);

  // Apply Doppler ambiguity
  double doppler = ambiguity * dssr.prf1;
  dssr.alt_dopcen[0] += doppler;
  dssr.crt_dopcen[0] += doppler;

  // Write the parameter file
  fp = FOPEN(outFile, "w");
  fprintf(fp, "OBJECT = PROCESSING_PARAMETERS\n");
  fprintf(fp, "  OBJECT = COMMON_HEADER\n");
  fprintf(fp, "    MSG_TYPE = \"PROCESSING_PARAMTERS\"\n");
  fprintf(fp, "    TIME = 1990-001T00:00:01\n");
  fprintf(fp, "    SOURCE = \"WRITE_PPF\"\n");
  fprintf(fp, "    DESTINATION = \"SPS\"\n");
  fprintf(fp, "    NUMBER_OF_RECORDS = 1\n");
  fprintf(fp, "  END_OBJECT = COMMON_HEADER\n");
  fprintf(fp, "  PLATFORM = \"RADARSAT-1\"\n");
  fprintf(fp, "  REVOLUTION = %d\n", meta->general->orbit);
  fprintf(fp, "  FRAME_ID = %d\n", meta->general->frame);
  strncpy(type, meta->general->mode, 2);
  if (strcmp_case(type, "SW") == 0)
    fprintf(fp, "  PRODUCT_TYPE = \"UNKNOWN IMAGE TYPE U\"\n"); 
  else // FIX ME: does not distinguish standard, quicklook, complex and ramp
    fprintf(fp, "  PRODUCT_TYPE = \"STANDARD\"\n");
  fprintf(fp, "  NO_BEAMS = %d\n", dssr.no_beams);
  fprintf(fp, "  BEAM1 = \"%s\" \n", dssr.beam1);
  fprintf(fp, "  BEAM2 = \"%s\" \n", dssr.beam2);
  fprintf(fp, "  BEAM3 = \"%s\" \n", dssr.beam3);
  fprintf(fp, "  BEAM4 = \"%s\" \n", dssr.beam4);
  fprintf(fp, "  PRF1 = %e\n", dssr.prf1);
  fprintf(fp, "  PRF2 = %e\n", dssr.prf2);
  fprintf(fp, "  PRF3 = %e\n", dssr.prf3);
  fprintf(fp, "  PRF4 = %e\n", dssr.prf4);
  fprintf(fp, "  ALT_DOPCEN     = (%e, %e, %e)\n",
	  dssr.alt_dopcen[0], dssr.alt_dopcen[1], dssr.alt_dopcen[2]);
  fprintf(fp, "  ALT_DOPCEN_DELTA  = (%e, %e, %e)\n", zero, zero, zero);
  fprintf(fp, "  CRT_DOPCEN     = (%e, %e, %e)\n",
	  dssr.crt_dopcen[0], dssr.crt_dopcen[1], dssr.crt_dopcen[2]);
  fprintf(fp, "  CRT_DOPCEN_DELTA  = (%e, %e, %e)\n", doppler, zero, zero);
  fprintf(fp, "  ALT_RATE       = (%e, %e, %e)\n",
	  dssr.alt_rate[0], dssr.alt_rate[1], dssr.alt_rate[2]);
  fprintf(fp, "  CRT_RATE       = (%e, %e, %e)\n",
	  dssr.crt_rate[0], dssr.crt_rate[1], dssr.crt_rate[2]);
  fprintf(fp, "END_OBJECT = PROCESSING_PARAMETERS\n");
  FCLOSE(fp);

  // Clean up
  asfPrintStatus("\nDone.\n");
  free(in_base);
  meta_free(meta);

  return EXIT_SUCCESS;
}
