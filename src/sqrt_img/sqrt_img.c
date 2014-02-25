#define ASF_NAME_STRING "sqrt_img"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" <input_file> <output_file>\n"

#define ASF_DESCRIPTION_STRING \
"     This really should be part of raster_calc.  sqrt all values.\n"

#define ASF_INPUT_STRING \
"     An ASF Internal file.\n"\

#define ASF_OUTPUT_STRING \
"     An ASF Internal file, all pixels are the sqrt of the corresponding\n"\
"     pixel in the input image.  Negative-values pixels are set to 0.\n"

#define ASF_OPTIONS_STRING \
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
"     > "ASF_NAME_STRING" in out\n"

#define ASF_SEE_ALSO_STRING \
"     \n"

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
      "Version:\n   %s\n\n",
       version_string(ASF_NAME_STRING));
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
  int currArg = 1;
  int NUM_ARGS = 2;

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);
  asfSplashScreen(argc, argv);

  if (argc<2)
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
    printf("Insufficient arguments.  Expected %d, got %d.\n",
           NUM_ARGS, argc-currArg);
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  char *in_file = argv[currArg];
  char *out_file = argv[currArg+1];

  char *in_img = appendExt(in_file, ".img");
  char *out_img = appendExt(out_file, ".img");
  char *in_meta = appendExt(in_file, ".meta");
  char *out_meta = appendExt(out_file, ".meta");

  meta_parameters *meta = meta_read(in_meta);
  if (!meta) asfPrintError("Failed to read metadata for: %s\n", in_file);

  FILE *ifp = FOPEN(in_img, "r");
  FILE *ofp = FOPEN(out_img, "w");
  float *buf = MALLOC(sizeof(float)*meta->general->sample_count);

  int ii,jj;
  for (ii=0; ii<meta->general->line_count; ++ii) {
    get_float_line(ifp, meta, ii, buf);
    for (jj=0; jj<meta->general->sample_count; ++jj) {
      if (buf[jj] < 0) {
        buf[jj] = 0;
      } else {
        buf[jj] = sqrt(buf[jj]);
      }
    }
    put_float_line(ofp, meta, ii, buf);
    asfLineMeter(ii,meta->general->line_count);
  }

  FCLOSE(ifp);
  FCLOSE(ofp);
  FREE(buf);

  if (meta->stats) { 
    FREE(meta->stats);
    meta->stats = NULL;
  }
 
  meta_write(meta, out_meta);
  meta_free(meta);

  FREE(in_img);
  FREE(out_img);
  FREE(in_meta);
  FREE(out_meta);

  asfPrintStatus("Done.\n");
  return EXIT_SUCCESS;
}
