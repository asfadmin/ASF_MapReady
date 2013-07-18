#define ASF_NAME_STRING "update_state"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [ --num <number of state vectors> ]\n"\
"          [ --spacing <state vector separation> ] <input metadata file name>\n"\
"          <output metadata file name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program updates the given metadata file to use the requested state\n"\
"     vectors, either with the requested number or with the requested spacing.\n"\
"\n"\
"     The first state vector will be at the start of acquisition, and the last\n"\
"     at the end of the acquisition, unless a particular spacing is specified\n"\
"     in which case the last state vector could be after the acquisition stop\n"\
"     time.\n"

#define ASF_INPUT_STRING \
"     The metadata file is required, and should be in ASF Internal format.\n"\
"     Specify either the number of state vectors, or the state vector spacing\n"\
"     (in seconds).\n"

#define ASF_OUTPUT_STRING \
"     The output metadata file is required.  It will only differ from the input\n"\
"     in the number of state vectors in the stateVectors section of the file.\n"

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

#define ASF_EXAMPLES_STRING \
"       > "ASF_NAME_STRING" --num 3 <input metadata> <output metadata>\n"\
"       > "ASF_NAME_STRING" --spacing 1 <input metadata> <output metadata>\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>

void update_state(const char *inFile, const char *outFile, int num, double spacing);

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
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n"  TOOL_SUITE_NAME " " TOOL_SUITE_VERSION_STRING "\n\n");
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
  int NUM_ARGS = 2;

  int num = -1;
  double spacing = -1;

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
    else if (strmatches(key,"-num","--num",NULL)) {
      CHECK_ARG(1);
      num = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"-spacing","--spacing",NULL)) {
      CHECK_ARG(1);
      spacing = atof(GET_ARG(1));
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

  if (num<0 && spacing<0) {
    printf("Specify a positive value for --num or --spacing.\n");
    usage(argv[0]);
  }

  if (num>0 && spacing>0) {
    printf("Specify either --num or --spacing, but not both.\n");
    usage(argv[0]);
  }

  inFile = argv[currArg];
  outFile = argv[currArg+1];

  asfPrintStatus("Processing: %s -> %s\n", inFile, outFile);

  update_state(inFile, outFile, num, spacing);
  asfPrintStatus("Done.\n");

  return EXIT_SUCCESS;
}

void update_state(const char *inFile, const char *outFile, int num, double spacing)
{
  char *in_meta_file = appendExt(inFile, ".meta");
  char *out_meta_file = appendExt(outFile, ".meta");

  meta_parameters *in_meta = meta_read(in_meta_file);
  if (!in_meta) {
   asfPrintError("Could not read metadata file %s\n", in_meta);
  }

  if (!in_meta->sar || !in_meta->state_vectors) {
    asfPrintError("Can't update state vectors in metadata that doesn't have any!\n");
  }

  meta_parameters *out_meta = meta_copy(in_meta);

  int nl = in_meta->general->line_count;
  int ns = in_meta->general->sample_count;
  double t1 = meta_get_time(in_meta, 0, ns/2);
  double t2 = meta_get_time(in_meta, nl, ns/2);
  double t = fabs(t2-t1);

  if (num == 1 && spacing < 0) {
    spacing = t;
  }
  else if (num > 0 && spacing < 0) {
    spacing = t / (num - 1);
  }
  else if (num < 0 && spacing > 0) {
    num = (int)(2+floor(t / spacing));
  }
  else {
    asfPrintError("Invalid values in update_state: num=%d, spacing=%f\n", num, spacing);
  }

  asfPrintStatus("Propagating %d state vectors with interval %g.\n", num, spacing);

  propagate_state(out_meta, num, spacing);

  meta_write(out_meta, out_meta_file);
  meta_free(in_meta);
  meta_free(out_meta);
  FREE(in_meta_file);
  FREE(out_meta_file);
}

