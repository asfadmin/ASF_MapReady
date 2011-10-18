#define ASF_NAME_STRING "make_gr_dem"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <image file> <dem file> <output file>\n"

#define ASF_DESCRIPTION_STRING \
"     This program performs creates a DEM with the geometry of the input file,\n"\
"     using data in the given DEM.\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <asf_terrcorr.h>
#include <assert.h>
#include "vector.h"

// Print minimalistic usage info & exit
static void make_gr_dem_usage(const char *name)
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
#define usage make_gr_dem_usage

// Main program body.
int
main (int argc, char *argv[])
{
  int currArg = 1;
  int NUM_ARGS = 3;
  int pad = 0;
  double tolerance = 0.5;
  
  const int n=1024;
  char mask_filename[n];
  strcpy(mask_filename, "");

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc >= 2 && strmatches(argv[1],"-help","--help",NULL))
    print_help();
  if (argc<3)
    make_gr_dem_usage(ASF_NAME_STRING);

  while (currArg < (argc-NUM_ARGS)) {
    char *key = argv[currArg++];
    if (strmatches(key,"-help","--help",NULL)) {
        print_help(); // doesn't return
    }
    else if (strmatches(key,"-log","--log",NULL)) {
      CHECK_ARG(1);
      strncpy_safe(logFile,GET_ARG(1),n);
      fLog = FOPEN(logFile, "a");
      logflag = TRUE;
    }
    else if (strmatches(key,"-pad","--pad",NULL)) {
      CHECK_ARG(1);
      pad = atoi(GET_ARG(1));
    }
    else if (strmatches(key,"-tolerance","--tolerance",NULL)) {
      CHECK_ARG(1);
      tolerance = atof(GET_ARG(1));
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
    make_gr_dem_usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    make_gr_dem_usage(argv[0]);
  }

  char *input = argv[currArg];
  char *dem = argv[currArg+1];
  char *out_name = argv[currArg+2];

  meta_parameters *meta = meta_read(input);

  char *demImg = appendExt(dem, ".img");
  char *demMeta = appendExt(dem, ".meta");

  asfPrintStatus("Input file: %s\n", input);
  asfPrintStatus("DEM file: %s\n", dem);
  asfPrintStatus("Output file: %s\n", out_name);
  asfPrintStatus("Padding: %d\n\n", pad);

  int fail = make_gr_dem_ext(meta, demImg, demMeta, pad, tolerance, out_name, 1);
  int ok = !fail;

  FREE(demImg);
  FREE(demMeta);
  meta_free(meta);

  asfPrintStatus(ok ? "Done.\n" : "Failed!\n");
  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
