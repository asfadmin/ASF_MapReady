#define ASF_NAME_STRING "shaded_relief"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-water] <inFile> <outFile>\n"

#define ASF_DESCRIPTION_STRING \
"     This program generates a shaded relief map from a digital elevation\n"\
"     model.\n"

#define ASF_INPUT_STRING \
"     The input file is the digital elevation model with its full file name.\n"

#define ASF_OUTPUT_STRING \
"     The output file is the shaded relief map with its full file name.\n\n"

#define ASF_OPTIONS_STRING \
"     -water\n"\
"          The terrain correction process produces a number of intermediate\n"\
"          files on the way to generating the final product.  Normally, these\n"\
"          temporary files are deleted when the process completes, however\n"\
"          if you wish to keep these files around you may do so with this\n"\
"          option.\n"\
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
#include <stdlib.h>
#include <asf.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

#define NUM_ARGS 2

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
      "Version: 1.0\n\n");
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

int main(int argc, char *argv[])
{
  int addSpeckle = TRUE;
  int water = FALSE;
  char *inFile, *outFile;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<=1)
      usage(ASF_NAME_STRING);
  else if (strmatches(argv[1],"-help","--help",NULL))
      print_help();

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-water","--water",NULL)) {
      water = TRUE;
    }
    else {
      printf( "\n**Invalid option:  %s\n", argv[currArg-1]);
      usage(ASF_NAME_STRING);
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(ASF_NAME_STRING);
  }

  inFile = argv[currArg];
  outFile = argv[currArg+1];

  shaded_relief(inFile, outFile, addSpeckle, water);
  exit(EXIT_SUCCESS);
}
