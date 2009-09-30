#define ASF_NAME_STRING "farcorr"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-keep] [-single-angle]\n"\
"           [-threshold <threshold angle>  [-sigma | -beta | -gamma ] [-db]\n"\
"           <in_base_name> <out_base_name>\n"

#define ASF_DESCRIPTION_STRING \
"     This program performs Faraday rotation correction to a quad-pol\n"\
"     complex polarimetric image.\n\n"\

#define ASF_INPUT_STRING \
"     The input file is required, and should be in ASF Internal format.\n"\
"     It must have 8 bands, named HH_AMP, HH_PHASE, etc., for each of HH,\n"\
"     HV, VH, and VV.  You should just specify the basename of the file.\n"\
"     The data must not be calibrated, amplitude data is necessary for the\n"\
"     Faraday Rotation calculations.\n"

#define ASF_OUTPUT_STRING \
"     The output file will have 8 bands, like the input, and be corrected\n"\
"     for Faraday rotation.  If calibration options were used, the output\n"\
"     will be calibrated according to the specified options.\n"

#define ASF_OPTIONS_STRING \
"     -keep (-k)\n"\
"          Intermediate files will be saved.\n"\
"\n"\
"          The intermediates file will have two or three bands:\n"\
"             OMEGA: Contains the calculated faraday rotation angles at\n"\
"                    every pixel.\n"\
"             OMEGA_SMOOTHED: Contains the local average of the rotation\n"\
"                    angles at each pixel.  Not present if the -s option\n"\
"                    was used.\n"\
"             RESIDUALS: The difference in the omega values calculated\n"\
"                    in the input & output images.\n"\
"\n"\
"     -single-angle (-s)\n"\
"          Normally, the Faraday Rotation code will use a local average of\n"\
"          the per-pixel rotation angles (this is the smoothed version of\n"\
"          the angle map in the image of itermediates).  A 600x600\n"\
"          averaging kernel is applied to the rotation map to generate the\n"\
"          local average angles.\n"\
"\n"\
"          With this option turned on, a global average rotation angle\n"\
"          is used instead of a local average -- in other words, a single\n"\
"          correction angle is used for all pixels in the image.\n"\
"\n"\
"          Using this option is highly recommended, as the local averaging\n"\
"          process is quite slow, and it isn't clear if it really produces\n"\
"          better results.\n"\
"\n"\
"     -threshold <threshold angle>\n"\
"          The correction is skipped if the calculated average Faraday\n"\
"          Rotation angle is below the given threshold (specified in\n"\
"          degrees).  The output file will be calibrated, if calibration\n"\
"          options are selected, otherwise the output will be the same\n"\
"          as the input.\n"\
"\n"\
"     -ksize <kernel size>\n"\
"          Size of the smoothing kernel.  Must be an odd integer.\n"\
"\n"\
"     -sigma\n"\
"          After Faraday Rotation correction, calibrate the image using\n"\
"          Sigma powerscale values. Use only one of: -sigma, -beta, -gamma.\n"\
"\n"\
"     -beta\n"\
"          After Faraday Rotation correction, calibrate the image using\n"\
"          Beta powerscale values. Use only one of: -sigma, -beta, -gamma.\n"\
"\n"\
"     -gamma\n"\
"          After Faraday Rotation correction, calibrate the image using\n"\
"          Gamma powerscale values. Use only one of: -sigma, -beta, -gamma.\n"\
"\n"\
"     -db\n"\
"          Output calibrated image in decibels.  This can be used with\n"\
"          -sigma, -beta, or -gamma; this option is ignored if none of\n"\
"          those options have been specified.  When perorming statistics\n"\
"          on the imagery you may wish to consider leaving the image in\n"\
"          powerscale (i.e., do not use the -db option)\n"\
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
"     > "ASF_NAME_STRING" <input basename> <output basename>\n"

#define ASF_SEE_ALSO_STRING \
"     asf_calpol\n"

#include <stdio.h>
#include <asf.h>
#include <asf_meta.h>
#include <asf_sar.h>
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
  int keep_flag = FALSE;
  int single_angle_flag = FALSE;
  int NUM_ARGS = 2;
  int sigma_flag=FALSE, beta_flag=FALSE, gamma_flag=FALSE, db_flag=FALSE;
  double threshold = -1;
  int ksize=599;

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
    else if (strmatches(key,"-keep","--keep","-k",NULL)) {
      keep_flag = TRUE;
    }
    else if (strmatches(key,"-single-angle","--single-angle","-s",NULL)) {
      single_angle_flag = TRUE;
    }
    else if (strmatches(key,"-sigma","--sigma",NULL)) {
      sigma_flag = TRUE;
    }
    else if (strmatches(key,"-beta","--beta",NULL)) {
      beta_flag = TRUE;
    }
    else if (strmatches(key,"-gamma","--gamma",NULL)) {
      gamma_flag = TRUE;
    }
    else if (strmatches(key,"-db","--db",NULL)) {
      db_flag = TRUE;
    }
    else if (strmatches(key,"-threshold","--threshold",NULL)) {
      CHECK_ARG(1);
      threshold = atof(GET_ARG(1));
    }
    else if (strmatches(key,"-ksize","--ksize",NULL)) {
      CHECK_ARG(1);
      ksize = atoi(GET_ARG(1));
    }
    else {
        --currArg;
        break;
    }
  }

  if (db_flag && !sigma_flag && !beta_flag && !gamma_flag) {
    asfPrintWarning("-db option ignored.  No calibration radiometry "
                    "(-sigma, -beta, or -gamma) was specified.\n");
    db_flag = FALSE;
  }

  if (sigma_flag + beta_flag + gamma_flag > 1) {
    asfPrintError("More than one calibration option (-sigma, -beta, or "
                  "-gamma) was specified.\n");
  }

  radiometry_t radiometry = r_AMP;
  if (sigma_flag)
    radiometry = db_flag ? r_SIGMA_DB : r_SIGMA;
  else if (beta_flag)
    radiometry = db_flag ? r_BETA_DB : r_BETA;
  else if (gamma_flag)
    radiometry = db_flag ? r_GAMMA_DB : r_GAMMA;

  if ((argc-currArg) < NUM_ARGS) {
    printf("Insufficient arguments.\n");
    usage(argv[0]);
  } else if ((argc-currArg) > NUM_ARGS) {
    printf("Unknown argument: %s\n", argv[currArg]);
    usage(argv[0]);
  }

  inFile = argv[currArg];
  outFile = argv[currArg+1];

  asfPrintStatus("Using kernel size: %d\n", ksize);

  faraday_correct(inFile, outFile, threshold, keep_flag, single_angle_flag,
                  radiometry, ksize);

  asfPrintStatus("Done.\n");

  return EXIT_SUCCESS;
}
