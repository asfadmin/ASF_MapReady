#define ASF_NAME_STRING "clip"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] \n"\
"          <in_base_name> <mask_file> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program clips out the area defined in the mask file out on an\n"\
"     image.\n"

#define ASF_INPUT_STRING \
"     The input files is required, and should be in ASF Internal format.\n"\
"     The clip area in the mask file is assumed to have the pixel value\n"\
"     of 1. Pixel values of 0 are treated as background value.\n"

#define ASF_OUTPUT_STRING \
"     The output file is required, and will have the same dimensions as the\n"\
"     input file. The clipped area will retain the original pixel values.\n"\
"     The remaining area is set to a background value.\n"

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

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

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
      "Output:\n"ASF_OUTPUT_STRING "\n"
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
  char *inFile, *maskFile, *outFile;
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
  maskFile = argv[currArg+1];
  outFile = argv[currArg+2];

  char *in_base = get_basename(inFile);
  char *out_base = get_basename(outFile);

  asfPrintStatus("Clipping image: %s -> %s\n", in_base, out_base);

  clip(inFile, maskFile, outFile);
  asfPrintStatus("Done.\n");

  free(in_base);
  free(out_base);

  return EXIT_SUCCESS;
}
