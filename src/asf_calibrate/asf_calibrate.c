#define ASF_NAME_STRING "asf_calibrate"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-db] [-wh_scale] -sigma | -gamma | -beta <input file> <output file>\n"

#define ASF_DESCRIPTION_STRING \
"   This program applies the radiometric calibration parameter to an\n"\
"   amplitude. The radiometric sigma, gamma or beta values are either\n"\
"   in linear power scale or logarithmic dB.\n"

#define ASF_INPUT_STRING \
"   input file    The input is expected to be an amplitude image in ASF\n"\
"                 internal format.\n"\
"   -sigma        Switch to convert amplitude in sigma values.\n"\
"   -gamma        Switch to convert amplitude in gamma values.\n"\
"   -beta         Switch to convert amplitude in beta values.\n"

#define ASF_OUTPUT_STRING \
"   output file   The output will be radiometric values stored in ASF\n"\
"                 internal format\n"

#define ASF_OPTIONS_STRING \
"   -db           Convert the linear radiometric power scale values into\n"\
"                 logarithmic dB values.\n"\
"   -wh_scale     Scale the output image to byte with a formula developed\n"\
"                 at Woods Hole.\n\n"\
"                 cal DN [byte] = (cal DN + 31) / 0.15 + 1\n\n"\
"                 In this scheme the DN of zero is reserved for the no data\n"\
"                 value.\n"

#include <asf.h>
#include <asf_meta.h>
#include <asf_sar.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <asf_terrcorr.h>
#include <assert.h>
#include "vector.h"

// Print minimalistic usage info & exit
static void calibrate_usage(const char *name)
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
      "Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_SUCCESS);
}

static int strmatches(const char *key, ...)
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

// work around a hard-coded function call in CHECK_ARG macro
#define usage calibrate_usage

// Main program body.
int
main (int argc, char *argv[])
{
  int currArg = 1;
  int NUM_ARGS = 3;
  int dbFlag = FALSE;
  int wh_scaleFlag = FALSE;
  radiometry_t radiometry;
  char *inFile, *outFile, *radio;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc >= 2 && strmatches(argv[1],"-help","--help",NULL))
    print_help();
  if (argc<3)
    calibrate_usage(ASF_NAME_STRING);

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
      print_help(); // doesn't return
    }
    else if (strmatches(key,"-db","--db",NULL)) {
      dbFlag = TRUE;
    }
    else if (strmatches(key,"-wh_scale","--wh_scale",NULL)) {
      wh_scaleFlag = TRUE;
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
    printf("Insufficient arguments.  Expected %d, got %d.\n",
           NUM_ARGS, argc-currArg);
    calibrate_usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    calibrate_usage(argv[0]);
  }

  radio = argv[currArg];
  inFile = argv[currArg+1];
  outFile = argv[currArg+2];

  if (strcmp_case(radio, "-GAMMA") == 0) {
    if (dbFlag)
      radiometry = r_GAMMA_DB;
    else
      radiometry = r_GAMMA;
  }
  else if (strcmp_case(radio, "-BETA") == 0) {
    if (dbFlag)
      radiometry = r_BETA_DB;
    else
      radiometry = r_BETA;
  }
  else if (strcmp_case(radio, "-SIGMA") == 0) {
    if (dbFlag)
      radiometry = r_SIGMA_DB;
    else
      radiometry = r_SIGMA;
  }
  else
    asfPrintError("Unknown radiometry (%s)\n", radio);

  int fail = asf_calibrate(inFile, outFile, radiometry, wh_scaleFlag);
  int ok = !fail;

  asfPrintStatus(ok ? "Done.\n" : "Failed.\n");
  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
