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
"               [-log <file>] [-write-proj-file <file>] [-read_proj_file <file>]\n"\
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
"     Projection Parameter Options  \n"\
"     ============================\n"\
"\n"\
"     All these options take arguments, unless otherwise noted.  Groups\n"\
"     of options which appear together on a single line seperated by\n"\
"     commas are aliases for the same parameter (they are intended to\n"\
"     aid recognition, since there is much confusion of map projection\n"\
"     terminology).\n"\
"\n"\
"     -p, -projection: Projection\n"\
"          Projection to use.  Argument must be one of the following:\n"\
"               utm    - Universal Transverse Mercator\n"\
"               ps     - Polar stereographic\n"\
"               lamcc  - Lambert conformal conic\n"\
"               lamaz  - Lambert azimuthal equal area\n"\
"               albers - Albers conical equal area\n"\
"\n"\
"       UTM\n"\
"       ---\n"\
"          --zone                      : Zone (optional)\n"\
"\n"\
"          If a zone is not specified, it will be determined from the\n"\
"          scene's metadata.\n\n"\
"\n"\
"       POLAR STEREO\n"\
"       ------------\n"\
"          --first-standard-parallel    : Latitude of True Scale\n"\
"          --central-meridian           : Longitude of Central Meridian\n"\
"          --north-pole                 : Center on North Pole (no argument)\n"\
"          --south-pole                 : Center on South Pole (no argument)\n"\
"          --false-easting              : False Easting (optional)\n"\
"          --false-northing             : False Northing (optional)\n"\
"\n"\
"       LAMBERT CONFORMAL CONIC\n"\
"       -----------------------\n"\
"          --first-standard-parallel   : First Standard Parallel\n"\
"          --second-standard-parallel  : Second Standard Parallel\n"\
"          --latitude-of-origin        : Latitude at projection\"s origin\n"\
"          --central-meridian          : Central Meridian\n"\
"          --false-easting             : False Easting (optional)\n"\
"          --false-northing            : False Northing (optional)\n"\
"\n"\
"          You may omit the origin (the image center will be used as the\n"\
"          origin), however the standard parallels are required.\n"\
"\n"\
"       LAMBERT AZIMUTHAL EQUAL AREA\n"\
"       ----------------------------\n"\
"          --latitude-of-origin        : Latitude at center of projection\n"\
"          --central-meridian          : Longitude at center of projection\n"\
"          --false-easting             : False Easting (optional)\n"\
"          --false-northing            : False Northing (optional)\n"\
"\n"\
"          You may omit the point of tangency (the image center will be\n"\
"          used).\n"\
"\n"\
"       ALBERS CONICAL EQUAL AREA\n"\
"       -------------------------\n"\
"          --first-standard-parallel   : First Standard Parallel\n"\
"          --second-standard-parallel  : Second Standard Parallel\n"\
"          --latitude-of-origin        : Latitude of projection\"s origin\n"\
"          --central-meridian          : Central Meridian\n"\
"          --false-easting             : False Easting (optional)\n"\
"          --false-northing            : False Northing (optional)\n"\
"\n"\
"          You may omit the origin (the image center will be used as the\n"\
"          origin), however the standard parallels are required.\n"\
"\n"\
"     Using a Projection Parameters File\n"\
"     ==================================\n"\
"\n"\
"     -write-proj-file <file>\n"\
"          Save the specified projection information to a file with\n"\
"          the given name.  The file may be used for subsequent projections\n"\
"          with --read-proj-file.\n"\
"\n"\
"     -read-proj-file <file>\n"\
"          Read projection information from the given file.  The format of\n"\
"          the file must match what is used with --write-proj-file.\n"\
"          This option may not be used together with any other\n"\
"          projection options.\n"\
"\n"\
"          You cannot specify a projection (with --projection), and\n"\
"          --read-proj-file at the same time.\n"\
"\n"\
"     Other Options\n"\
"     =============\n"\
"\n"\
"     -height <height> \n"\
"          Assume that terrain in the image is <height> meters above\n"\
"          the reference GEM6 ellipsoid.  Optimal geolocation accuracy\n"\
"          will then be achieved for pixels on terrain at this height.\n"\
"          The geolocation of terrain at other height will be off by\n"\
"          about the height difference between the terrain and\n"\
"          <height>, assuming a satellite look angle 45 degrees from\n"\
"          horizontal.\n"\
"\n"\
"     -resample-method <method>\n"\
"          Specifies which interpolation method to use when resampling\n"\
"          images into projection geometry.  Available choices are:\n"\
"            nearest_neighbor\n"\
"            bilinear\n"\
"            bicubic\n"\
"\n"\
"     -datum <datum> \n"\
"          Specifies the datum that is used when projecting.  The datum\n"\
"          applies to the target coordinate system.  Supported Datums:\n"\
"            NAD27  (North American Datum 1927) (Clarke 1866)\n"\
"            NAD83  (North American Datum 1983) (GRS 1980)\n"\
"            WGS84  (World Geodetic System 1984) (default).\n"\
"          NOTE: When specifying a datum together with a UTM projection\n"\
"          type, note the following valid zone numbers which may be combined\n"\
"          with each datum type:\n"\
"            NAD27  Valid zones are 2 through 22\n"\
"            NAD83  Valid zones are 2 through 23\n"\
"            WGS84  Valid zones are 1 through 60\n"\
"\n"\
"     -pixel_size <pixel spacing>\n"\
"          Specifies the pixel spacing of the geocoded image.  "ASF_NAME_STRING"\n"\
"          by default will preserve the pixel size of the input image.\n"\
"\n"\
"     -band <band_id | all>\n"\
"          If the image file contains multiple bands (channels), then\n"\
"          geocode the band identified by 'band_id' (only) into a single-band\n"\
"          ASF-format file.  If 'all' is specified rather than a band_id, then\n"\
"          geocode all available bands into a single multi-band ASF-format file.\n"\
"          In this case, all bands will be geocoded using the same set of\n"\
"          projection parameters, and the parameters in the resulting metadata\n"\
"          file apply to all.  Default is '-band all'.\n"\
"\n"\
"     -background <background fill value>\n"\
"          Value to use for pixels that fall outside of the scene.  "ASF_NAME_STRING"\n"\
"          by default will fill the outside with zeroes.\n"\
"\n"\
"     -force\n"\
"          Override the built-in projection sanity checks.  "ASF_NAME_STRING"\n"\
"          by default will abort with an error if it detects that a\n"\
"          scene lies in an area where the selected projection is\n"\
"          going to give poor results.  However, you may still wish\n"\
"          to do the projection anyway (such as when you will mosaic\n"\
"          the result a number of other scenes and wish to have them\n"\
"          all in the same projection), the -force option can be used\n"\
"          in these situations.\n"\
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
      "Version:\n   " CONVERT_PACKAGE_VERSION_STRING "\n\n");
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
