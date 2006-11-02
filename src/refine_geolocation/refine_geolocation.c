#define ASF_NAME_STRING "refine_geolocation"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-update]\n"\
"          [-mask-file <filename> | -auto-water-mask]\n"\
"          [-mask-height-cutoff <height in meters>] [-other-file <basename>]\n"\
"          <in_base_name> <dem_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program takes two inputs: (1) An unprojected SAR image\n"\
"     in the ASF internal format, and (2) a geocoded digital elevation\n"\
"     model (DEM) encompassing the area in the SAR image, also in the\n"\
"     ASF internal format.  The output is a pair of offsets in slant\n"\
"     and time that improve the geolocation of the image.  These\n"\
"     offsets can either be applied to the input SAR image's metadata\n"\
"     (the -update option), or a new metadata file can be created.\n"

#define ASF_INPUT_STRING \
"     Two input files are required.  Each input file, the SAR image and\n"\
"     the DEM, need to be in the ASF internal format, and you should just\n"\
"     specify the basename (i.e., do not give the file's extension) of each.\n"

#define ASF_OUTPUT_STRING \
"     When using the -update option, no output file should be given, as\n"\
"     the metadata for the input SAR image is updated with the offsets.\n\n"\
"     When not using the -update option, an output basename is required.\n"

#define ASF_OPTIONS_STRING \
"     -update (-u)\n"\
"          With this option, the original metadata is updated with the\n"\
"          calculated offsets.\n\n"\
"          Without this option, a new metadata file is written with the\n"\
"          provided name.  Since the image data is not affected by\n"\
"          geolocation refinement a new image file a not written.  However,\n"\
"          because most ASF tools require that the metadata basename match\n"\
"          the image basename, you will need to either (1) make a copy\n"\
"          of the image file with a new matching basename, or (2) make a\n"\
"          symbolic link, or (3) rename the generated metadata file to\n"\
"          match the image basename, overwriting the original metadata,\n"\
"          presumably after you have verified that the new metadata file\n"\
"          is reasonable.  For example, if your input files are 'sar.img'\n"\
"          and 'sar.meta' and the DEM is 'dem.img' (and dem.meta), and you\n"\
"          use an output name of 'adjusted_sar.meta', these options would\n"\
"          be used as follows:\n\n"\
"            (1) Make a copy of the image file: \n"\
"                  > refine_geolocation sar dem adjusted_sar\n"\
"                  > cp sar.img adjusted_sar.img\n\n"\
"            (2) Make a symbolic link:  (Unix only)\n"\
"                  > refine_geolocation sar dem adjusted_sar\n"\
"                  > ln -s sar.img adjusted_sar.img\n\n"\
"            (3) Overwrite the original: \n"\
"                  > refine_geolocation sar dem adjusted_sar\n"\
"                     (verify that adjusted_sar.meta is ok)\n"\
"                  > mv adjusted_sar.meta sar.meta\n\n"\
"\n"\
"     -mask-file <filename>\n"\
"          Specify a file that contains regions to be omitted from geolocation\n"\
"          refinement.  These regions will also not be considered when\n"\
"          attempting to correlate the SAR and DEM images, as large featureless\n"\
"          regions (such as water, or glaciers) do not correlate well.\n"\
"          You may use this option to mask those regions, confining the\n"\
"          correlation to areas that you expect to provide good matches.\n\n"\
"          The mask file should be 0 in the unmasked regions, and positive in\n"\
"          the masked regions.\n\n"\
"          You cannot use this option together with -auto-water-mask.\n"\
"\n"\
"     -auto-water-mask\n"\
"          A mask file (see the -mask-file option, above) is automatically\n"\
"          produced from the DEM.  Areas where the DEM height is less than 1\n"\
"          meter are masked.  (Though you may change this cutoff with the\n"\
"          option -mask-height-cutoff.)\n\n"\
"          You cannot use this option together with -mask-file.\n"\
"\n"\
"     -mask-height-cutoff <height>\n"\
"          Only applies to automatic water masking.\n\n"\
"          DEM pixels of the given value or below are masked.  The default\n"\
"          is 1 meter.\n"\
"\n"\
"     -other-file <basename>\n"\
"          The correlation that is done during the geolocation refinement\n"\
"          process produces offsets in range and azimuth that, when applied \n"\
"          to the SAR image's metadata, result in a good match with the DEM.\n"\
"          Sometimes, you will want these same offsets applied to other data\n"\
"          files.  Specifying -other-file will update the metadata for this\n"\
"          listed file with the calculated offsets (in addition to the file\n"\
"          already updated, if you have used -update).\n\n"\
"          This option may be specified more than once.\n"\
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
"     Geolocation refinement, updating the original metadata:\n"\
"     > "ASF_NAME_STRING" -update input_image dem_image\n\n"\
"     Geolocation refinement, creating a new metadata file: \n"\
"     > "ASF_NAME_STRING" input_image dem_image updated\n"\
"     This produces an output file 'updated.meta', and no image file.\n"\
"     Additional examples are given in the explanation for\n"\
"     the -update option.\n"

#define ASF_LIMITATIONS_STRING \
"     Can be quite slow when refining geolocation with a mask.\n"

#define ASF_SEE_ALSO_STRING \
"     asf_terrcorr\n"

#include <stdio.h>
#include <asf.h>
#include <asf_terrcorr.h>
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
      "Limitations:\n" ASF_LIMITATIONS_STRING "\n"
      "See also:\n" ASF_SEE_ALSO_STRING "\n"
      "Contact:\n" ASF_CONTACT_STRING "\n"
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n");
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

#define MAX_OTHER 10

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *demFile, *maskFile=NULL, *outFile;
  int currArg = 1;
  int update_flag = FALSE;
  int NUM_ARGS = 2;
  int auto_water_mask = FALSE;
  float mask_height_cutoff = 1.0;
  char *other_files[MAX_OTHER];
  int i,n_other = 0;

  for (i=0; i<MAX_OTHER; ++i)
      other_files[i]=NULL;

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
    else if (strmatches(key,"-update","--update","-u",NULL)) {
      update_flag = TRUE;
    }
    else if (strmatches(key,"-mask-file","--mask-file",NULL)) {
        CHECK_ARG(1);
        maskFile = GET_ARG(1);
    }
    else if (strmatches(key,"-mask-height-cutoff","--mask-height-cutoff",NULL)) {
        CHECK_ARG(1);
        mask_height_cutoff = atof(GET_ARG(1));
    }
    else if (strmatches(key,"-auto-water-mask","--auto-water-mask",NULL)) {
        auto_water_mask = TRUE;
    }
    else if (strmatches(key,"-other-file","--other-file",NULL)) {
        CHECK_ARG(1);
        if (n_other == MAX_OTHER)
            asfPrintError("-other-file option only supported %d times.\n", MAX_OTHER);
        other_files[n_other++] = strdup(GET_ARG(1));
    }
    else {
        --currArg;
        break;
    }
  }
  if (!update_flag) ++NUM_ARGS;
  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  inFile = argv[currArg];
  demFile = argv[currArg+1];
  if (update_flag) {
      outFile = appendToBasename(inFile, "_tmp");      
  } else {
      outFile = argv[currArg+2];
  }

  int ret = refine_geolocation(inFile, demFile, maskFile, outFile, 
                               update_flag, auto_water_mask,
                               mask_height_cutoff, other_files);

  if (update_flag) {
      char *meta = appendExt(outFile, ".meta");
      remove(meta);
      FREE(meta);
      FREE(outFile);
  }

  for (i=0; i<MAX_OTHER; ++i)
      if (other_files[i])
          free(other_files[i]);

  return ret==0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
