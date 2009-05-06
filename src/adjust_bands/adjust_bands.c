#define ASF_NAME_STRING "adjust_bands"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-bands <band list>]\n"\
"          <in_base_name #1> ... <in base_name #n> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program allows users to combine, remove or reorder bands\n"\
"     in one or more source files.\n"

#define ASF_INPUT_STRING \
"     At least one input file is required, and should be in ASF Internal\n"\
"     format.  You should just specify the basename of the file(s).\n"

#define ASF_OUTPUT_STRING \
"     The output file will be an ASF Internal file, with the bands\n"\
"     pulled from the input file as specified with the -bands option.\n"\
"     (If -bands was not given, the output contains all of the bands\n"\
"     from all of the input file(s).)\n"

#define ASF_OPTIONS_STRING \
"     -bands <band list>\n"\
"          This option specifies which bands from the input file(s) are\n"\
"          to be put into the output, and in which order.  It is a list\n"\
"          of band names, separated by commas.  The band list can be\n"\
"          quoted (e.g., if band names contain spaces).  For example,\n"\
"          if the input file contains 8 bands (e.g., quad-pol palsar data)\n"\
"          of amplitude and phase data, like this:\n"\
"            AMP_HH,PHASE_HH,AMP_HV,PHASE_HV,AMP_VH,PHASE_VH,AMP_VV,PHASE_VV\n"\
"          and you wanted just the HH and VV amplitudes, the bands string\n"\
"          should be:\n"\
"            AMP_HH,AMP_VV\n"\
"\n"\
"          If the -bands option is not given, all bands of the input files\n"\
"          are put into the output.\n"\
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
"          The input file is: example.img (and example.meta)\n"\
"          The input file contains 8 bands (e.g., quad-pol palsar data)\n"\
"          of amplitude and phase data, like this:\n"\
"            AMP_HH,PHASE_HH,AMP_HV,PHASE_HV,AMP_VH,PHASE_VH,AMP_VV,PHASE_VV\n"\
"          and you wanted just the HH and VV amplitudes:\n"\
"            > adjust_bands -bands AMP_HH,AMP_VV example output\n"\
"\n"\
"          Suppose you have an ampitude image as amp.img (and amp.meta)\n"\
"          and an interferogram image as interf.img (together with\n"\
"          interf.meta) that you wanted to combine together.\n"\
"          In amp.meta the band_names string is AMP, and in interf.meta\n"\
"          the band_names string is INTERFEROGRAM.  Then:\n"\
"            > adjust_bands -bands AMP,INTERFEROGRAM amp interf output\n"\
"\n"\
"          To simple combine two separate files into a single multi-band\n"\
"          file, you can leave out the -bands option (or, specify all the\n"\
"          bands, if you would like to change the order):\n"\
"            > adjust_bands in1 in2 out\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_sar.h>
#include <asf_license.h>
#include <asf_contact.h>
#include <asf_raster.h>

// Print minimalistic usage info & exit
static void usage()
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

static void adjust_bands(char **inFiles, int n, char *outFile, char *bands)
{
  int i,j,b,nl,ns;
  meta_parameters *outMeta=NULL;

  char *outFile_img = appendExt(outFile, ".img");
  char *outFile_meta = appendExt(outFile, ".meta");

  nl=ns=-1;
  if (strcmp_case(bands, "ALL")==0) {
    // made the "ALL" case separate
    asfPrintStatus("Putting all bands from the inputs into the output.\n");

    int out_band = 0;
    FILE *outFp = FOPEN(outFile_img, "wb");

    for (i=0; i<n; ++i) {
      meta_parameters *meta = meta_read(inFiles[i]);
      meta_general *mg = meta->general;
      int nb = mg->band_count;
      
      if (i==0) {
        nl = mg->line_count;
        ns = mg->sample_count;
        outMeta = meta_read(inFiles[i]);
      }
      else {
        if (nl != mg->line_count || ns != mg->sample_count) {
          asfPrintError("Input file '%s' does not have the same size as the\n"
                        "first input file: %dx%d vs %dx%d.  All input files "
                        "must\nbe the same size.\n");
        }

        strcat(outMeta->general->bands, mg->bands);
        outMeta->general->band_count += mg->band_count;
      }
      strcat(outMeta->general->bands, ",");

      asfPrintStatus("\nFile: %s\n", inFiles[i]);
      char *imgFile = appendExt(inFiles[i], ".img");
      float *line = MALLOC(sizeof(float)*ns);
      FILE *inFp = FOPEN(imgFile, "rb");
      for (b=0; b<nb; ++b) {
        asfPrintStatus("Band: %s\n", get_str(mg->bands, b));
        for (j=0; j<nl; ++j) {
          get_band_float_line(inFp, meta, b, j, line);
          put_band_float_line(outFp, outMeta, out_band, j, line);
          asfLineMeter(j,nl);
        }
        ++out_band;
      }
      FCLOSE(inFp);
      FREE(line);
      FREE(imgFile);
    }

    FCLOSE(outFp);

    asfPrintStatus("\nWrote %d band%s to the output.\n", out_band,
                   out_band == 1 ? "" : "s");

    if (outMeta->general->bands[strlen(outMeta->general->bands)-1]==',')
      outMeta->general->bands[strlen(outMeta->general->bands)-1]='\0';

    meta_write(outMeta, outFile_meta);
  }
  else {
    int num_out_bands=0;
    char **out_bands=NULL;
    split_into_array(bands, ',', &num_out_bands, &out_bands);
    
    int *band_nums = MALLOC(sizeof(int)*num_out_bands);
    int *file_nums = MALLOC(sizeof(int)*num_out_bands);
    for (i=0; i<num_out_bands; ++i)
      file_nums[i] = -1;
    
    // open all the inputs, build metadatas, figure out which band numbers
    // in which inputs we want
    FILE **inFps = MALLOC(sizeof(FILE*)*n);
    meta_parameters **metas = MALLOC(sizeof(meta_parameters*)*n);
    for (i=0; i<n; ++i) {
      meta_parameters *meta = meta_read(inFiles[i]);
      meta_general *mg = meta->general;
      int nb = mg->band_count;
      
      if (i==0) {
        nl = mg->line_count;
        ns = mg->sample_count;
        outMeta = meta_read(inFiles[i]);
      }
      else {
        if (nl != mg->line_count || ns != mg->sample_count) {
          asfPrintError("Input file '%s' does not have the same size as the\n"
                        "first input file: %dx%d vs %dx%d.  All input files "
                        "must\nbe the same size.\n");
        }
      }
      
      for (j=0; j<num_out_bands; ++j) {
        // if we haven't already figured out where band #j of the output comes
        // from, see if it comes from this particular file
        if (file_nums[j] < 0) {
          int band_num = get_band_number(mg->bands, nb, out_bands[j]);
          if (band_num >= 0) {
            file_nums[j] = i;
            band_nums[j] = band_num;
          }
        }
      }

      char *imgFile = appendExt(inFiles[i], ".img");
      inFps[i] = FOPEN(imgFile, "rb");

      FREE(imgFile);
      metas[i] = meta;
    }
    
    // verify that we found a match for all requested bands
    int ok=TRUE;
    for (i=0; i<num_out_bands; ++i) {
      if (file_nums[i] < 0) {
        ok=FALSE;
        asfPrintWarning("Band '%s' not found in the input!\n", out_bands[i]);
      }
    }
    if (!ok)
      asfPrintError("Not all requested output bands were found in the input "
                    "files.\n");
    
    // now build the output file
    outMeta->general->band_count = num_out_bands;
    strcpy(outMeta->general->bands, bands);
    meta_write(outMeta, outFile_meta);
    
    float *line = MALLOC(sizeof(float)*ns);
    FILE *outFp = FOPEN(outFile_img, "wb");
    for (i=0; i<num_out_bands; ++i) {
      asfPrintStatus("Working on band #%d: %s (from '%s')\n",
                     i, out_bands[i], inFiles[file_nums[i]]);

      for (j=0; j<nl; ++j) {
        int f = file_nums[i];
        asfRequire(f >= 0 && f < n, "Illegal file_num!");
        int band_num = band_nums[i];
        asfRequire(band_num >= 0, "Illegal band_num!");

        get_band_float_line(inFps[f], metas[f], band_num, j, line);
        put_band_float_line(outFp, outMeta, i, j, line);

        asfLineMeter(j,nl);
      }
    }

    asfPrintStatus("\nWrote %d band%s to the output.\n", num_out_bands,
                   num_out_bands==1 ? "" : "s");

    // clean up
    FCLOSE(outFp);
    FREE(line);

    for (i=0; i<n; ++i) {
      fclose(inFps[i]);
      meta_free(metas[i]);
    }
    FREE(inFps);
    FREE(metas);
    for (i=0; i<num_out_bands; ++i)
      FREE(out_bands[i]);
    FREE(out_bands);
    FREE(band_nums);
    FREE(file_nums);
  }

  meta_free(outMeta);
  FREE(outFile_img);
  FREE(outFile_meta);
}

// Main program body.
int
main (int argc, char *argv[])
{
  int i, currArg = 1;
  int NUM_ARGS = 2;

  char bands[512];
  strcpy(bands, "ALL");

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
    else if (strmatches(key,"-bands","--bands","-band","--band",NULL)) {
      CHECK_ARG(1);
      strcpy(bands,GET_ARG(1));
    }
    else {
      --currArg;
      break;
    }
  }
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage();
  }

  char *outFile = argv[argc-1];

  int n_inputs = argc-1-currArg;
  char **inputs = MALLOC(sizeof(char*)*n_inputs);
  for (i=0; i<n_inputs; ++i)
    inputs[i] = STRDUP(argv[currArg+i]);

  adjust_bands(inputs, n_inputs, outFile, bands);

  for (i=0; i<n_inputs; ++i)
    FREE(inputs[i]);
  FREE(inputs);

  asfPrintStatus("Done.\n");

  return EXIT_SUCCESS;
}
