#define ASF_NAME_STRING "smooth"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-k <kernel size>]\n"\
"          <in_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program smooths the given input image, by averaging pixels\n"\
"     together.\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be in ASF Internal format.\n"\
"     The kernel size is also required.\n"

#define ASF_OUTPUT_STRING \
"     The output file will have the same number of bands as the input file\n"\
"     (each band is smooth separately).  The output will also have the same\n"\
"     size as the input image.  In fact, the metadata for the output image\n"\
"     will be identical to the input metadata.\n"

#define ASF_OPTIONS_STRING \
"     -kernel-size (-ks, -k)\n"\
"          Specifies how many pixels on the side of a square, centered on\n"\
"          each pixel, will be used in calculating the corresponding output\n"\
"          pixel.  For example, a kernel size of 3 will average together 9\n"\
"          pixels to produce each output pixel.\n"\
"\n"\
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

#define ASF_EXAMPLES_STRING \
"     > "ASF_NAME_STRING" -k 3 <input basename> <output basename>\n"

#define ASF_SEE_ALSO_STRING \
"     resample\n"

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
      "Examples:\n" ASF_EXAMPLES_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
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
  char *inFile, *outFile;
  int currArg = 1;
  int kernel_size = -1;
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
    else if (strmatches(key,"-quiet","--quiet","-q",NULL)) {
      quietflag = TRUE;
    }
    else if (strmatches(key,"-kernel-size","--kernel-size","-ks","-k",NULL)) {
      CHECK_ARG(1);
      kernel_size = atoi(GET_ARG(1));
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

  if (kernel_size < 0)
    asfPrintError("No kernel size specified!\n"
                  "Use the option '-kernel-size'.\n");

  char *in_base = get_basename(inFile);
  char *out_base = get_basename(outFile);

  asfPrintStatus("Smoothing image: %s -> %s\n", in_base, out_base);

  smooth(inFile, outFile, kernel_size, EDGE_TRUNCATE);
  asfPrintStatus("Done.\n");

  free(in_base);
  free(out_base);

  return EXIT_SUCCESS;
}
