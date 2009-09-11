#define ASF_NAME_STRING "shift_geolocation"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" [-log <logfile>] [-quiet] [-update | -copy-image]\n"\
"          <x-shift> <y-shift> <in_base_name> [<out_base_name>]\n"

#define ASF_DESCRIPTION_STRING \
"     This program adjusts the time_shift and slant_shift values\n"\
"     in the metadata, to produce geolocations shifted by the given\n"\
"     amounts.  The x-shift and y-shift values are in units of\n"\
"     pixels or meters (to specify meters, add an 'm').\n"\
"     If the image is already geocoded, then the startX/Y values\n"\
"     are adjusted by the specified amounts, instead of the time\n"\
"     and slant shift values.\n"

#define ASF_INPUT_STRING \
"     The input file is required, and should be in ASF Internal format.\n"\
"     You should just specify the basename of the file.\n\n"\
"     The shifts are either in units of pixels, or meters.  By default,\n"\
"     a shift amount is taken to be in pixels, unless an 'm' is appended\n"\
"     to the end of the number, e.g.: 100m\n\n"\
"     Positive values shift to the right (x), and down (y).\n"\
"     Negative values shift to the left (x), and up (y).\n"

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
"          the geolocation shift, a new image file is not written, unless\n"\
"          the -copy-image option is also used.  If you don't use -copy-image,\n"\
"          because most ASF tools require that the metadata basename match\n"\
"          the image basename, you will need to either (1) make a copy\n"\
"          of the image file with a new matching basename, or (2) make a\n"\
"          symbolic link, or (3) rename the generated metadata file to\n"\
"          match the image basename, overwriting the original metadata,\n"\
"          presumably after you have verified that the new metadata file\n"\
"          is reasonable.  For example, if your input file is 'sar.img'\n"\
"          (with 'sar.meta') and you wish to apply a shift of 3 pixels in x,\n"\
"          and 7 pixels in y, and you use the output name 'adjusted_sar.meta',\n"\
"          these options would be used as follows:\n\n"\
"            (1) Make a copy of the image file: (also see -copy-image, below)\n"\
"                  > shift_geolocation 3 7 sar adjusted_sar\n"\
"                  > cp sar.img adjusted_sar.img\n\n"\
"            (2) Make a symbolic link:  (Unix only)\n"\
"                  > shift_geolocation 3 7 sar adjusted_sar\n"\
"                  > ln -s sar.img adjusted_sar.img\n\n"\
"            (3) Overwrite the original: \n"\
"                  > shift_geolocation 3 7 sar adjusted_sar\n"\
"                     (verify that adjusted_sar.meta is ok)\n"\
"                  > mv adjusted_sar.meta sar.meta\n\n"\
"\n"\
"     -copy-image (-c)\n"\
"          Makes a copy of the image file to match the new metadata file\n"\
"          this is created.  Cannot be used with the -update option.\n"\
"          It is equivalent to using option #1, from the discussion\n"\
"          above, for what to do when you do not use the -update option.\n"\
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
"     Geolocation shift of 2 pixels in x, 5 pixels in y, updating\n"\
"     the original metadata:\n"\
"       > "ASF_NAME_STRING" -update 2 5 input_image\n\n"\
"     Geolocation shift of 2 meters in x, 5 meters in y, updating\n"\
"     the original metadata:\n"\
"       > "ASF_NAME_STRING" -update 2m 5m input_image\n\n"\
"     Geolocation shift of 2 pixels in x, 5 pixels in y, creating\n"\
"     a new metadata file:\n"\
"       > "ASF_NAME_STRING" input_image updated\n\n"\
"     This produces an output file 'updated.meta', and no image file.\n\n"\
"     Geolocation shift of 2 pixels in x, 5 pixels in y, creating\n"\
"     new metadata and image files (and note that the output image\n"\
"     file is identical to the input image file):\n"\
"       > "ASF_NAME_STRING" -copy-image input_image updated\n\n"\
"     Additional examples are given in the explanation for\n"\
"     the -update option.\n"

#define ASF_SEE_ALSO_STRING \
"     asf_terrcorr, refine_geolocation\n"

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

static int is_in_meters(const char *s)
{
    char c = s[strlen(s)-1];
    return c=='m' || c=='M';
}

// Main program body.
int
main (int argc, char *argv[])
{
  char *inFile, *outFile;
  int currArg = 1;
  int update_flag = FALSE;
  int copy_image_flag = FALSE;
  double x_shift=0, y_shift=0;
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
    else if (strmatches(key,"-update","--update","-u",NULL)) {
      update_flag = TRUE;
    }
    else if (strmatches(key,"-copy-image","--copy-image","-c",NULL)) {
      copy_image_flag = TRUE;
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
  if (copy_image_flag && update_flag) {
    printf("** The -update and -copy-image flags cannot be used together!\n");
    usage(argv[0]);
  }

  inFile = argv[currArg+2];
  char *meta_name = appendExt(inFile, ".meta");
  meta_parameters *meta = meta_read(meta_name);
  int user_gave_meters = is_in_meters(argv[currArg]);

  if (user_gave_meters)
    x_shift = atof(argv[currArg]) / meta->general->x_pixel_size;
  else
    x_shift = atof(argv[currArg]);

  if (is_in_meters(argv[currArg+1]))
    y_shift = atof(argv[currArg+1]) / meta->general->y_pixel_size;
  else
    y_shift = atof(argv[currArg+1]);

  double x_shift_m = x_shift * meta->general->x_pixel_size;
  double y_shift_m = y_shift * meta->general->y_pixel_size;

  if (meta->projection) {
    // when adjusting startX/startY, we have to take into account the
    // fact that startX/Y <-> line 0,0 are in the upper left corner --
    // so increasing sample numbers go to the right (and so we must shift
    // the origin to the *left* to move points *right*), and the reverse
    // for line numbers.  For x/y we need to flip both, since increasing
    // x and y both move up/right, so to get points to move that direction,
    // we must move the origin down/left.

    if (user_gave_meters) {
      asfPrintStatus("Shifting by: %.5f pixels (%.5f meters) in X.\n",
                     x_shift, x_shift_m);
      asfPrintStatus("             %.5f pixels (%.5f meters) in Y.\n\n",
                     -y_shift, y_shift_m);
      asfPrintStatus("Input Metadata: %s\n", meta_name);
      x_shift_m = -x_shift_m;
      y_shift_m = -y_shift_m;
    }
    else {
      asfPrintStatus("Shifting by: %.5f pixels (%.5f meters) in X.\n",
                     x_shift, x_shift_m);
      asfPrintStatus("             %.5f pixels (%.5f meters) in Y.\n\n",
                     y_shift, -y_shift_m);
      asfPrintStatus("Input Metadata: %s\n", meta_name);
      x_shift_m = -x_shift_m;
    }

    // projected data -- update startX/Y
    asfPrintStatus("  StartX: %f -> %f\n"
                   "  StartY: %f -> %f\n",
        meta->projection->startX,
        meta->projection->startX + x_shift_m,
        meta->projection->startY,
        meta->projection->startY + y_shift_m);

    meta->projection->startX += x_shift_m;
    meta->projection->startY += y_shift_m;
  } else {
    asfPrintStatus("Shifting by: %.5f pixels (%.5f meters) in X.\n",
                   x_shift, x_shift_m);
    asfPrintStatus("             %.5f pixels (%.5f meters) in Y.\n\n",
                   y_shift, y_shift_m);
    asfPrintStatus("Input Metadata: %s\n", meta_name);

    // unprojected data -- shifting in time/slant coordinates
    double t_shift, slant_shift;
    refine_offset(x_shift, y_shift, meta, &t_shift, &slant_shift);

    asfPrintStatus("  Time Shift: %f -> %f\n"
                   "  Slant Shift: %f -> %f\n",
          meta->sar->time_shift,
          meta->sar->time_shift + t_shift,
          meta->sar->slant_shift,
          meta->sar->slant_shift + slant_shift);

    meta->sar->time_shift += t_shift;
    meta->sar->slant_shift += slant_shift;
  }

  if (update_flag) {
      asfPrintStatus("Input file's metadata will be updated.\n");
      outFile = appendToBasename(inFile, "_tmp");      
  } else {
      outFile = argv[currArg+3];
  }

  if (copy_image_flag) {
    asfPrintStatus("A copy of the input image will be created to match the "
                   "new metadata file.\n");
  }

  char *out_meta_name = appendExt(outFile, ".meta");
  if (update_flag) {
      asfPrintStatus("Updating %s\n", meta_name);
      meta_write(meta, meta_name);
  } else {
      asfPrintStatus("Writing %s\n", out_meta_name);
      meta_write(meta, out_meta_name);
  }

  free(meta_name);

  if (copy_image_flag) {
      char *img_copy = appendExt(out_meta_name, ".img");
      char *in_img = appendExt(inFile, ".img");
      asfPrintStatus("Copying %s to %s...\n", in_img, img_copy);
      fileCopy(in_img, img_copy);
      FREE(img_copy);
      FREE(in_img);
  }

  free(out_meta_name);
  asfPrintStatus("Done.\n");

  return EXIT_SUCCESS;
}
