/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to
change the following defines. For the short ones (like
ASF_NAME_STRING) this is no big deal. However, for some of the longer
ones, such as ASF_COPYRIGHT_STRING, it can be a daunting task to get
all the newlines in correctly, etc. In order to help you with this
task, there is a tool, edit_man_header. The tool *only* works with
this portion of the code, so fear not. It will scan in defines of the
format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor,
allow you to edit the text in a clean manner, and then automatically
generate these defines, formatted appropriately. The only warning is
that any text between those two markers and not part of one of those
defines will not be preserved, and that all of this auto-generated
code will be at the top of the source file. Save yourself the time and
trouble, and use edit_man_header. :)
*/

#define ASF_NAME_STRING \
"asf_geocode"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" -p <projection name> <<projection parameters>>\n"\
"               [-force] [-resample-method <method>] [-height <height>]\n"\
"               [-datum <datum>] [-pixel-size <pixel size>] [-band <band_id | all>]\n"\
"               [-log <file>] [-write-proj-file <file>] [-read-proj-file <file>]\n"\
"               [-background <val>] [-quiet] [-license] [-version] [-help]\n"\
"               <in_base_name> <out_base_name>\n"\
"\n"\
"   Use the -help option for more projection parameter controls.\n"

#define ASF_DESCRIPTION_STRING \
"     This program takes a map projected or an unprojected (ground\n"\
"     range) image in the ASF internal format and geocodes it,\n"\
"     i.e. swizzles it around into one of the standard projections used\n"\
"     for maps (universal transverse mercator, polar stereo, etc).  The\n"\
"     output is a new image in ASF internal format.\n"

#define ASF_INPUT_STRING \
"     Most of the \"options\" are actually required.  The specification\n"\
"     of a certain projection type implies that all the parameters\n"\
"     required to fully specify a projection of that type be included.\n"\
"\n"\
"     This must be an ASF internal format image base name.\n"

#define ASF_OUTPUT_STRING \
"     The base name of the geocoded image to produce.\n"

#define ASF_OPTIONS_STRING \
"%s"\
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
"     To map project an image with centerpoint at -147 degrees\n"\
"     longitude and average height 466 meters into universal transverse\n"\
"     mercator projection, with one pixel 50 meters on a side:\n"\
"\n"\
"     "ASF_NAME_STRING" -p utm --central-meridian -147.0 --height 466\n"\
"                 input_image output_image\n"\
"\n"\
"     To geocode one band within an image file, you specify the selected band\n"\
"     with the -band option, and the selected band MUST be one of which appears\n"\
"     in the list of available bands as noted in the 'bands' item found in the\n"\
"     'general' (first) block in the metadata file.  For example, if 'bands'\n"\
"     contains \"01,02,03,04\", then you could specify a band_id, e.g. \"-band 02\"\n"\
"     etc on the command line.  The same applies to band lists such as \"HH,HV,VH,VV\"\n"\
"     or just \"03\" etcetera.\n"\
"\n"\
"     "ASF_NAME_STRING" -p utm -band HV file outfile_HV\n"

#define ASF_LIMITATIONS_STRING \
"     May fail badly if bad projection parameters are supplied for the\n"\
"     area in the image.\n"

#define ASF_SEE_ALSO_STRING \
"     asf_import, asf_export\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/
#include <asf_contact.h>
#include <asf_license.h>

// Standard libraries.
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Libraries from packages outside ASF.
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_statistics_double.h>

// Libraries developed at ASF.
#include <asf.h>
#include <asf_nan.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include "float_image.h"
#include <libasf_proj.h>
#include <spheroids.h>
#include <asf_contact.h>

// Headers used by this program.
#include "asf_geocode.h"

// Print minimalistic usage info & exit
static void print_usage(void)
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
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n",
        geocode_projection_options_help());
  exit(EXIT_SUCCESS);
}

// Main routine.
int
main (int argc, char **argv)
{
  int force_flag = FALSE;
  int debug_dump = FALSE;
  char band_id[256]="";
  char *in_base_name, *out_base_name;

  in_base_name = (char *) MALLOC(sizeof(char)*255);
  out_base_name = (char *) MALLOC(sizeof(char)*255);

  // Get the projection parameters from the command line.
  projection_type_t projection_type;
  // Terrain height to assume.  Defaults to 0.
  double average_height;
  // Pixel size to use for output image, in projection coordinate
  // units.  This variable corresponds to a "private"
  // (i.e. undocumented, so users don't fiddle with it) option.
  double pixel_size;
  // Datum to use in the target projection
  datum_type_t datum;
  // Method to use to resample images.
  resample_method_t resample_method;
  // Value to put in the region outside the image
  double background_val = 0.0;

  if (detect_flag_options(argc, argv, "-help", "--help", "-h", NULL)) {
    print_help();
  }

  // Detect & Process logging arguments
  if ((logflag = detect_string_options(argc, argv, logFile,
				      "-log", "--log", NULL))) {
      fLog = fopen (logFile, "a");
      if ( fLog == NULL ) {
	// Couldn't open the log file, so just don't do logging.
	logflag = FALSE;
      }
  }
  quietflag = detect_flag_options(argc, argv, "-quiet", "--quiet", NULL);

  handle_license_and_version_args(argc, argv, ASF_NAME_STRING);

  asfSplashScreen(argc, argv);

  project_parameters_t *pp
    = get_geocode_options (&argc, &argv, &projection_type, &average_height,
			   &pixel_size, &datum, &resample_method,
			   &force_flag, band_id);

  if (!pp) {
      print_usage();
  }

  // The argument at which the filenames start
  int arg_num = 1;

  if (detect_flag_options(argc, argv, "-debug", NULL)) {
    debug_dump=TRUE;
    ++arg_num;
  }

  extract_double_options(&argc, &argv, &background_val, "--background",
                         "-background", NULL);
  if (ISNAN(background_val)) background_val = DEFAULT_NO_DATA_VALUE;

  // Get non-option command line arguments.
  if ( argc != 3 && !debug_dump ) {
    int ii;
    int bad_arg = FALSE;

    for (ii = 0; ii < argc; ++ii) {
      if (argv[ii][0] == '-') {
	bad_arg = TRUE;
	asfPrintStatus("Unrecognized argument: %s\n", argv[ii]);
      }
    }

    if (!bad_arg)
      fprintf (stderr, "Wrong number of arguments\n");

    print_usage ();
  }

  strcpy (in_base_name, argv[arg_num]);
  strcpy (out_base_name, argv[arg_num + 1]);

  // Strip .img and also check to make sure input and output
  // base names are not the same
  char *ext = findExt(in_base_name);
  if (ext && strncmp(ext, ".img", 4) == 0) *ext = '\0';
  ext = findExt(out_base_name);
  if (ext && strncmp(ext, ".img", 4) == 0) *ext = '\0';
  char msg[512];
  sprintf(msg,"Input and output basenames cannot be the same:\n"
      "    Input base name : %s\n"
          "    Output base name: %s\n",
      in_base_name, out_base_name);
  if (strcmp(in_base_name, out_base_name) == 0)
    asfPrintError(msg);

  // Call library function that does the actual work
  asf_geocode(pp, projection_type, force_flag, resample_method, average_height,
	      datum, pixel_size, band_id, in_base_name, out_base_name,
              (float)background_val);

  // Close Log, if needed
  if (logflag)
    FCLOSE (fLog);

  exit (EXIT_SUCCESS);
}
