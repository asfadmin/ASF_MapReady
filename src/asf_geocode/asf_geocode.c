#include <asf_contact.h>
#include <asf_copyright.h>
#include <asf_license.h>
/*==================BEGIN ASF AUTO-GENERATED DOCUMENTATION==================*/
/*
ABOUT EDITING THIS DOCUMENTATION:
If you wish to edit the documentation for this program, you need to change the
following defines. For the short ones (like ASF_NAME_STRING) this is no big
deal. However, for some of the longer ones, such as ASF_COPYRIGHT_STRING, it
can be a daunting task to get all the newlines in correctly, etc. In order to
help you with this task, there is a tool, edit_man_header.pl. The tool *only*
works with this portion of the code, so fear not. It will scan in defines of
the format #define ASF_<something>_STRING between the two auto-generated
documentation markers, format them for a text editor, run that editor, allow
you to edit the text in a clean manner, and then automatically generate these
defines, formatted appropriately. The only warning is that any text between
those two markers and not part of one of those defines will not be preserved,
and that all of this auto-generated code will be at the top of the source
file. Save yourself the time and trouble, and use edit_man_header.pl. :)
*/

#define ASF_NAME_STRING \
"asf_geocode"

#define ASF_USAGE_STRING \
"   "ASF_NAME_STRING" -p <projection name> <<projection parameters>>\n"\
"               [-force] [-resample-method <method>] [-height <height>]\n"\
"               [-datum <datum>] [-pixel-size <pixel size>] [-log <file>]\n"\
"               [-quiet] [-license] [-version] [-help]\n"\
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
"     UTM\n"\
"     ---\n"\
"          --zone                      : Zone\n"\
"          --central-meridian          : Longitude of Central Meridian\n"\
"          --latitude-of-origin        : Latitude at projection\"s origin\n"\
"\n"\
"	  Either the zone or center_longitude must be specified.  The\n"\
"	  center_longitude may not be an exact multiple of 6 (i.e. may\n"\
"	  not lie on a UTM zone boundry).\n"\
"\n"\
"     POLAR STEREO\n"\
"     ------------\n"\
"          --first-standard-parallel    : Latitude of True Scale\n"\
"          --central-meridian           : Longitude of Central Meridian\n"\
"          --north-pole                 : Center on North Pole (no argument)\n"\
"          --south-pole                 : Center on South Pole (no argument)\n"\
"          --false-easting              : False Easting (optional)\n"\
"          --false-northing             : False Northing (optional)\n"\
"\n"\
"     LAMBERT CONFORMAL CONIC\n"\
"     -----------------------\n"\
"          --first-standard-parallel   : First Standard Parallel\n"\
"          --second-standard-parallel  : Second Standard Parallel\n"\
"          --latitude-of-origin        : Latitude at projection\"s origin\n"\
"          --central-meridian          : Central Meridian\n"\
"          --false-easting             : False Easting (optional)\n"\
"          --false-northing            : False Northing (optional)\n"\
"\n"\
"         You may omit the origin (the image center will be used as the\n"\
"         origin), however the standard parallels are required.\n"\
"\n"\
"     LAMBERT AZIMUTHAL EQUAL AREA\n"\
"     ----------------------------\n"\
"          --latitude-of-origin        : Latitude at center of projection\n"\
"          --central-meridian          : Longitude at center of projection\n"\
"          --false-easting             : False Easting (optional)\n"\
"          --false-northing            : False Northing (optional)\n"\
"\n"\
"         You may omit the point of tangency (the image center will be\n"\
"         used).\n"\
"\n"\
"     ALBERS CONICAL EQUAL AREA\n"\
"     -------------------------\n"\
"          --first-standard-parallel   : First Standard Parallel\n"\
"          --second-standard-parallel  : Second Standard Parallel\n"\
"          --latitude-of-origin        : Latitude of projection\"s origin\n"\
"          --central-meridian          : Central Meridian\n"\
"          --false-easting             : False Easting (optional)\n"\
"          --false-northing            : False Northing (optional)\n"\
"\n"\
"         You may omit the origin (the image center will be used as the\n"\
"         origin), however the standard parallels are required.\n"\
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
"\n"\
"     -pixel_size <pixel spacing>\n"\
"          Specifies the pixel spacing of the geocoded image.  "ASF_NAME_STRING"\n"\
"          by default will preserve the pixel size of the input image.\n"\
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
"   -license\n"\
"        Print copyright and license for this software then exit.\n"\
"\n"\
"   -version\n"\
"        Print version and copyright then exit.\n"\
"\n"\
"   -help\n"\
"        Print a help page and exit.\n"

#define ASF_EXAMPLES_STRING \
"     To map project an image with centerpoint at -147 degrees\n"\
"     longitude and average height 466 meters into universal transverse\n"\
"     mercator projection, with one pixel 50 meters on a side:\n"\
"\n"\
"     "ASF_NAME_STRING" -p utm --central-meridian -147.0 --height 466\n"\
"                 input_image output_image\n"


#define ASF_LIMITATIONS_STRING \
"     May fail badly if bad projection parameters are supplied for the\n"\
"     area in the image.\n"

#define ASF_SEE_ALSO_STRING \
"     asf_import, asf_export\n"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) "ASF_COPYRIGHT_YEAR_STRING", University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
"All rights reserved.\n"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

// Standard libraries.
#include <math.h>
#include <signal.h>
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
#include <asf_meta.h>
#include <asf_raster.h>
#include <asf_reporting.h>
#include "float_image.h"
#include <libasf_proj.h>
#include <spheroids.h>
#include <asf_contact.h>

// Headers used by this program.
#include "geocode_options.h"

// Prototype
void check_parameters(projection_type_t projection_type,
		      project_parameters_t *pp, meta_parameters *meta,
		      int override_checks);

// Print minimalistic usage info & exit
static void print_usage(void)
{
  asfPrintStatus("\n"
      "Usage:\n"
      ASF_USAGE_STRING
      "\n");
  exit(EXIT_FAILURE);
}

// Blurb about what the user can do if projection errors are too
// large.  Called when we detect this condition.
static void print_large_error_blurb(int force_flag)
{
  if (!force_flag) {
    asfPrintStatus(
      "\nLarge projection errors can result if your projection parameters do\n"
      "not accurately represent the scene you are geocoding.  You can either\n"
      "re-run geocode using the '--force' option, or adjust your projection\n"
      "parameters to better reflect the scene.\n");
  }
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

// Print version and copyright & exit
static void print_version(void)
{
  asfPrintStatus(
    ASF_NAME_STRING", version "CONVERT_PACKAGE_VERSION_STRING"\n"
    ASF_COPYRIGHT_STRING);
  exit(EXIT_SUCCESS);
}

// Print our copyright and license notice & exit
static void print_license(int license_id)
{
  asfPrintStatus("\n"ASF_COPYRIGHT_STRING"\n");

  switch (license_id) {
    case ASF_BSD_ID:
      asfPrintStatus(ASF_BSD_LICENSE_STRING"\n");
      break;
    default:
      printf("License not found.\n");
      break;
  }
  exit(EXIT_SUCCESS);
}


// We want to trap segmentation faults (signal number 11,
// i.e. SIGSEGV) and try to produce a backtrace.  Here is a signal
// handler that does that.
static void
sigsegv_handler (int signal_number)
{
  g_assert (signal_number == SIGSEGV);

#define MAX_BACKTRACE_CALL_DEPTH 300

  // void *array[MAX_BACKTRACE_CALL_DEPTH];
  // size_t size = backtrace (array, MAX_BACKTRACE_CALL_DEPTH);

  fprintf (stderr, "SIGSEGV caught.  Backtrace:\n");
  fprintf (stderr, "unfortunately, backtrace functionality doesn't work.\n");
  //  backtrace_symbols_fd (array, size, STDERR_FILENO);

  abort ();
}

// Since our normal approach is to pass the datum from the input image
// on through to the (re)projected output image, reprojecting a pixel
// from a lat long pseudoprojected image requires us to do exactly
// nothing.  But we still need these functions to use when we need to
// use a function pointer to perform a generic operation.
static int 
project_lat_long_pseudo (project_parameters_t *pps, double lat, double lon,
			 double *x, double *y)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps;		

  *x = lat;
  *y = lon;

  return TRUE;
}
static int
project_lat_long_pseudo_inv (project_parameters_t *pps, double x, double y, 
			     double *lat, double *lon)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps;

  *lat = x;
  *lon = y;

  return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
//
// We want to find natural cubic splines to form approximating
// functions X and Y st
//
//      X(x, y) = input image x pixel coordinate of projection coordinates x, y
//      Y(x, y) = input image y pixel coordinate of projection coordinates x, y
//
// The basic steps are:
//
//   1.  Find the extent of the input image in projection coordinate
//       space, i.e. the minimum and maximum x and y projection
//       coordinates of all the pixels in the input image.
//
//   2.  Find the pairs of input image pixel coordinates corresponding
//       to the points of an evenly distributed grid in the output
//       projection space.
//
//   3.  Construct interpolating cubic splines for each column of points
//       in the grid.
//
//   4.  For each row in the output image, construct an interpolating
//       spline over the values which result from evaluating the column
//       splines at that y position.
//
//   5.  Verify that the splines aren't introducing too much error away
//       from the control points by examing the errors in the spline
//       approximation compared to the results of the analytical
//       transformation of a denser grid.
//
// The steps don't necessarily occer in exactly this order in the code
// though.
//
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////

// This is the form of the input data we want to fit splines to.
struct data_to_fit {
  size_t grid_size;		// Size of grid of points, in points on a side.
  size_t n;			// Number of transformed points (grid_size^2).
  double *x_proj;		// Projection x coordinates.
  double *y_proj;		// Projection y coordinates.
  // Input image pixel coordinates put 0, 0 at the top left.
  double *x_pix;		// Input image pixel x coordinate.
  double *y_pix;		// Input image pixel y coordinate.

  // These values are like the above ones, and should form a grid
  // covering the same area, but are considerably more sparse.
  size_t sparse_grid_size;
  size_t sparse_n;
  double *sparse_x_proj;
  double *sparse_y_proj;
  double *sparse_x_pix;
  double *sparse_y_pix;
};

// Reverse map from projection coordinates x, y to input pixel
// coordinate X.  This function looks at the data to fit on the first
// time through, in order to set up vertical splines, after which this
// argument is mostly ignored.  But not entirely, so you can't free or
// change the dtf.  Mapping is efficient only if the y coordinates are
// usually identical between calls, since when y changes a new spline
// between splines has to be created.
static double
reverse_map_x (struct data_to_fit *dtf, double x, double y)
{
  // True iff this is our first time through this routine.
  static gboolean first_time_through = TRUE;
  // Accelerators and interpolators for the all the vertical columns
  // of sample points.  Filled in first time through routine.
  static gsl_interp_accel **y_accel;
  static gsl_spline **y_spline;
  // Current accelerator and interpolator.  Updated when y argument is
  // different between calls.
  static gsl_interp_accel *crnt_accel;
  static gsl_spline *crnt;
  // Value of y for which current interpolator works.
  static double last_y;

  // Convenience aliases.
  size_t sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *xpixs = dtf->sparse_x_pix;

  if ( G_UNLIKELY (first_time_through || y != last_y) ) {
    if ( !first_time_through ) {
      // Free the spline from the last line.
      gsl_interp_accel_free (crnt_accel);
      gsl_spline_free (crnt);
    } else {
      // Its our first time through, so set up the splines for the
      // grid point columns.
      y_accel = g_new (gsl_interp_accel *, sgs);
      y_spline = g_new (gsl_spline *, sgs);
      size_t ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
	gsl_vector *cypv = gsl_vector_alloc (sgs);
	gsl_vector *cxpixv = gsl_vector_alloc (sgs);
	size_t jj;
	for ( jj = 0 ; jj < sgs ; jj++ ) {
	  gsl_vector_set (cypv, jj, yprojs[jj * sgs + ii]);
	  gsl_vector_set (cxpixv, jj, xpixs[jj * sgs + ii]);
	}
	y_accel[ii] = gsl_interp_accel_alloc ();
	y_spline[ii] = gsl_spline_alloc (gsl_interp_cspline, sgs);
	gsl_spline_init (y_spline[ii], cypv->data, cxpixv->data, sgs);
	gsl_vector_free (cxpixv);
	gsl_vector_free (cypv);
      }
      first_time_through = FALSE;
    }
    // Set up the spline that runs horizontally, between the column
    // splines.
    crnt_accel = gsl_interp_accel_alloc ();
    crnt = gsl_spline_alloc (gsl_interp_cspline, sgs);
    double *crnt_points = g_new (double, sgs);
    size_t ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline[ii], y, y_accel[ii]);
    }
    gsl_spline_init (crnt, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y = y;
  }

  return gsl_spline_eval (crnt, x, crnt_accel);
}

// This routine is analagous to reverse_map_x, including the same
// caveats and confusing behavior.
static double
reverse_map_y (struct data_to_fit *dtf, double x, double y)
{
  // True iff this is our first time through this routine.
  static gboolean first_time_through = TRUE;
  // Accelerators and interpolators for the all the vertical columns
  // of sample points.  Filled in first time through routine.
  static gsl_interp_accel **y_accel;
  static gsl_spline **y_spline;
  // Current accelerator and interpolator.  Updated when y argument is
  // different between calls.
  static gsl_interp_accel *crnt_accel;
  static gsl_spline *crnt;
  // Value of y for which current interpolator works.
  static double last_y;

  size_t sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *ypixs = dtf->sparse_y_pix;

  if ( G_UNLIKELY (first_time_through || y != last_y) ) {
    if ( !first_time_through ) {
      // Free the spline from the last line.
      gsl_interp_accel_free (crnt_accel);
      gsl_spline_free (crnt);
    } else{
      // Its our first time through, so set up the splines for the
      // grid point columns.
      y_accel = g_new (gsl_interp_accel *, sgs);
      y_spline = g_new (gsl_spline *, sgs);
      size_t ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
	// Current y projection value.
	gsl_vector *cypv = gsl_vector_alloc (sgs);
	// Current y pixel value.
	gsl_vector *cypixv = gsl_vector_alloc (sgs);
	size_t jj;
	for ( jj = 0 ; jj < sgs ; jj++ ) {
	  gsl_vector_set (cypv, jj, yprojs[jj * sgs + ii]);
	  gsl_vector_set (cypixv, jj, ypixs[jj * sgs + ii]);
	}
	y_accel[ii] = gsl_interp_accel_alloc ();
	y_spline[ii] = gsl_spline_alloc (gsl_interp_cspline, sgs);
	gsl_spline_init (y_spline[ii], cypv->data, cypixv->data, sgs);
	gsl_vector_free (cypixv);
	gsl_vector_free (cypv);
      }
      first_time_through = FALSE;
    }
    // Set up the spline that runs horizontally, between the column
    // splines.
    crnt_accel = gsl_interp_accel_alloc ();
    crnt = gsl_spline_alloc (gsl_interp_cspline, sgs);
    double *crnt_points = g_new (double, sgs);
    size_t ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline[ii], y, y_accel[ii]);
    }
    gsl_spline_init (crnt, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y = y;
  }

  return gsl_spline_eval (crnt, x, crnt_accel);
}

// Main routine.
int
main (int argc, char **argv)
{
  // Install handler to trap segmentation faults and print a backtrace.
  sigset_t sigsegv_mask;
  int return_code = sigemptyset(&sigsegv_mask);
  g_assert (return_code == 0);
  return_code = sigaddset(&sigsegv_mask, SIGSEGV);
  g_assert (return_code == 0);
  struct sigaction backtrace_action;
  backtrace_action.sa_handler = sigsegv_handler;
  backtrace_action.sa_mask = sigsegv_mask;
  backtrace_action.sa_flags = 0;
  return_code = sigaction (SIGSEGV, &backtrace_action, NULL);
  g_assert (return_code == 0);
  int force_flag = FALSE;

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

  // Detect & Process logging arguments
  if ((logflag = detect_string_options(argc, argv, logFile,
				      "-log", "--log", NULL))) {
      fLog = fopen (logFile, "w");
      if ( fLog == NULL ) {
	// Couldn't open the log file, so just don't do logging.
	logflag = FALSE;
      }
  }
  quietflag = detect_flag_options(argc, argv, "-quiet", "--quiet", NULL);

  asfSplashScreen(argc, argv);

  project_parameters_t *pp
    = get_geocode_options (&argc, &argv, &projection_type, &average_height,
			   &pixel_size, &datum, &resample_method,
			   &force_flag);

  if (detect_flag_options(argc, argv, "--help", "-help", "-h", NULL)) {
    print_help();
  }
  if (detect_flag_options(argc, argv, "--license", "-license", NULL)) {
    print_license(ASF_BSD_ID);
  }
  if (detect_flag_options(argc, argv, "--version", "-version", "-v", NULL)) {
    print_version();
  }

  // Get non-option command line arguments.
  if ( argc != 3 ) {
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

  GString *input_image = g_string_new (argv[1]);
  GString *input_meta_data = g_string_new (input_image->str);
  g_string_append (input_meta_data, ".meta");
  GString *output_image = g_string_new (argv[2]);
  GString *output_meta_data = g_string_new (output_image->str);
  g_string_append (output_meta_data, ".meta");

  // Input metadata.
  meta_parameters *imd = meta_read (input_meta_data->str);
  // We can't handle slant range images at the moment.  Happily, there
  // are only a very small number of these products around.
  if ( imd->sar->image_type == 'S' ) {
    asfPrintError ("Can't handle slant range images (i.e. almost certainly \n"
		   "left-looking AMM-1 era images) at present.\n");
  }


  // If we have an already projected image as input, we will need to
  // be able to unproject its coordinates back to lat long before we
  // can reproject them into the desired projection, so here we select
  // a function that can do that.

  // Flag true iff input image not map projected or pseudoprojected.
  gboolean input_projected = FALSE;
  // Convenience alias (valid iff input_projected).
  meta_projection *ipb = imd->projection;
  project_parameters_t *ipp = &imd->projection->param;
  int (*project_input) (project_parameters_t *pps, double lat, double lon,
			double *x, double *y);
  project_input = NULL;		// Silence compiler warnings.
  int (*unproject_input) (project_parameters_t *pps, double x, double y,
			  double *lat, double *lon);
  unproject_input = NULL;	// Silence compiler warnings.
  if ( (imd->sar->image_type == 'P' || imd->general->image_data_type == DEM)
       && imd->projection->type != SCANSAR_PROJECTION && imd->projection ) {
    input_projected = TRUE;

    switch ( imd->projection->type) {
    case UNIVERSAL_TRANSVERSE_MERCATOR:
      project_input = project_utm;
      unproject_input = project_utm_inv;
      break;
    case POLAR_STEREOGRAPHIC:
      project_input = project_ps;
      unproject_input = project_ps_inv;
      break;
    case ALBERS_EQUAL_AREA:
      project_input = project_albers;
      unproject_input = project_albers_inv;
      break;
    case LAMBERT_CONFORMAL_CONIC:
      project_input = project_lamcc;
      unproject_input = project_lamcc_inv;
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      project_input = project_lamaz;
      unproject_input = project_lamaz_inv;
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      project_input = project_lat_long_pseudo;
      unproject_input = project_lat_long_pseudo_inv;
    default:
      g_assert_not_reached ();
    }
  }

  void (*report_func) (const char *format, ...);
  report_func = force_flag ? asfPrintWarning : asfPrintError;

  // Set items in the projection parameters not on command-line
  apply_defaults (projection_type, pp, imd, &average_height, &pixel_size);

  // Check whether projection parameters are valid, dying with an
  // error if they aren't.
  check_parameters (projection_type, pp, imd, force_flag);

  // Convert all angle measures in the project_parameters to radians.
  to_radians (projection_type, pp);

  // Set Projection Datum & Average Height
  project_set_datum (datum);
  project_set_avg_height (average_height);

  // Assign our transformation function pointers to point to the
  // appropriate functions.
  int (*project) (project_parameters_t *pps, double lat, double lon, double *x,
		  double *y);
  int (*project_arr) (project_parameters_t *pps, double *lat, double *lon,
		      double **projected_x, double **projected_y,
		      long length);
  int (*unproject) (project_parameters_t *pps, double x, double y, double *lat,
		    double *lon);
  project = NULL;		// Silence compiler warnings.
  project_arr = NULL;		// Silence compiler warnings.
  unproject = NULL;		// Silence compiler warnings.
  switch ( projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR:
    project = project_utm;
    project_arr = project_utm_arr;
    unproject = project_utm_inv;
    break;
  case POLAR_STEREOGRAPHIC:
    project = project_ps;
    project_arr = project_ps_arr;
    unproject = project_ps_inv;
    break;
  case ALBERS_EQUAL_AREA:
    project = project_albers;
    project_arr = project_albers_arr;
    unproject = project_albers_inv;
    break;
  case LAMBERT_CONFORMAL_CONIC:
    project = project_lamcc;
    project_arr = project_lamcc_arr;
    unproject = project_lamcc_inv;
    break;
  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    project = project_lamaz;
    project_arr = project_lamaz_arr;
    unproject = project_lamaz_inv;
    break;
  default:
    g_assert_not_reached ();
    break;
  }

  // Input image dimensions in pixels in x and y directions.
  size_t ii_size_x = imd->general->sample_count;
  size_t ii_size_y = imd->general->line_count;

  // The latitude and longitude of the center of the image.
  double lat_0, lon_0;
  meta_get_latLon (imd, ii_size_y / 2.0, ii_size_x / 2.0, average_height,
		   &lat_0, &lon_0);

  // First we march around the entire outside of the image and compute
  // projection coordinates for every pixel, keeping track of the
  // minimum and maximum projection coordinates in each dimension.
  // This lets us determine the exact extent of the projected image in
  // projection coordinates.
  asfPrintStatus ("Determining input image extent in projection coordinate "
		  "space... ");

  double min_x = DBL_MAX;
  double max_x = -DBL_MAX;
  double min_y = DBL_MAX;
  double max_y = -DBL_MAX;

  { // Scoping block.
    // Number of pixels in the edge of the image.
    size_t edge_point_count = 2 * ii_size_x + 2 * ii_size_y - 4;
    double *lats = g_new (double, edge_point_count);
    double *lons = g_new (double, edge_point_count);
    size_t current_edge_point = 0;
    size_t ii = 0, jj = 0;
    for ( ; ii < ii_size_x - 1 ; ii++ ) {
      if ( input_projected ) {
	double xpc = ipb->startX + ipb->perX * ii;
	double ypc = ipb->startY - ipb->perY * jj;
	return_code = unproject_input (ipp, xpc, ypc,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]));
	g_assert (return_code);
      }
      else {
	meta_get_latLon (imd, (double)jj, (double)ii, average_height,
			 &(lats[current_edge_point]),
			 &(lons[current_edge_point]));
	lats[current_edge_point] *= D2R;
	lons[current_edge_point] *= D2R;
      }
      current_edge_point++;
    }
    for ( ; jj < ii_size_y - 1 ; jj++ ) {
      if ( input_projected ) {
	double xpc = ipb->startX + ipb->perX * ii;
	double ypc = ipb->startY - ipb->perY * jj;
	return_code = unproject_input (ipp, xpc, ypc,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]));
	g_assert (return_code);
      }
      else {
	meta_get_latLon (imd, (double)jj, (double)ii, average_height,
			 &(lats[current_edge_point]),
			 &(lons[current_edge_point]));
	lats[current_edge_point] *= D2R;
	lons[current_edge_point] *= D2R;
      }
      current_edge_point++;
    }
    for ( ; ii > 0 ; ii-- ) {
      if ( input_projected ) {
	double xpc = ipb->startX + ipb->perX * ii;
	double ypc = ipb->startY - ipb->perY * jj;
	return_code = unproject_input (ipp, xpc, ypc,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]));
	g_assert (return_code);
      }
      else {
	meta_get_latLon (imd, (double)jj, (double)ii, average_height,
			 &(lats[current_edge_point]),
			 &(lons[current_edge_point]));
	lats[current_edge_point] *= D2R;
	lons[current_edge_point] *= D2R;
      }
      current_edge_point++;
    }
    for ( ; jj > 0 ; jj-- ) {
      if ( input_projected ) {
	double xpc = ipb->startX + ipb->perX * ii;
	double ypc = ipb->startY - ipb->perY * jj;
	return_code = unproject_input (ipp, xpc, ypc,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]));
	g_assert (return_code);
      }
      else {
	meta_get_latLon (imd, (double)jj, (double)ii, average_height,
			 &(lats[current_edge_point]),
			 &(lons[current_edge_point]));
	lats[current_edge_point] *= D2R;
	lons[current_edge_point] *= D2R;
      }
      current_edge_point++;
    }
    g_assert (current_edge_point == edge_point_count);
    // Pointers to arrays of projected coordinates to be filled in.
    // The projection function will allocate this memory itself.
    double *x = NULL, *y = NULL;
    x = y = NULL;
    // Project all the edge pixels.
    return_code = project_arr (pp, lats, lons, &x, &y, edge_point_count);
    g_assert (return_code == TRUE);
    // Find the extents of the image in projection coordinates.
    for ( ii = 0 ; ii < edge_point_count ; ii++ ) {
      if ( x[ii] < min_x ) { min_x = x[ii]; }
      if ( x[ii] > max_x ) { max_x = x[ii]; }
      if ( y[ii] < min_y ) { min_y = y[ii]; }
      if ( y[ii] > max_y ) { max_y = y[ii]; }
    }

    free (y);
    free (x);
    g_free (lons);
    g_free (lats);
  }

  // Issue a warning when the chosen pixel size is smaller than the
  // input pixel size.
  if ( GSL_MIN(imd->general->x_pixel_size,
	       imd->general->y_pixel_size) > pixel_size ) {
    asfPrintWarning
      ("Requested pixel size %lf is smaller then the input image resolution "
       "(%le meters).\n", pixel_size,
       GSL_MIN (imd->general->x_pixel_size, imd->general->y_pixel_size));
  }

  // The pixel size requested by the user better not oversample by
  // the factor of 2.  Specifying --force will skip this check
  if (!force_flag && GSL_MIN(imd->general->x_pixel_size,
	       imd->general->y_pixel_size) > (2*pixel_size) ) {
    report_func
      ("Requested pixel size %lf is smaller then the minimum implied by half \n"
       "the input image resolution (%le meters), this is not supported.\n",
       pixel_size, GSL_MIN (imd->general->x_pixel_size,
			    imd->general->y_pixel_size));
  }

  asfPrintStatus ("done.\n\n");

  // Generate some mappings between output image projection
  // coordinates and input image pixel coordinates, using proj.  We
  // compute transformations for points on a grid_size * grid_size
  // grid and a sparse_grid_size * sparse_grid_size grid.
  asfPrintStatus ("Performing analytical projection of a spatially "
		  "distributed\nsubset of input image pixels... ");
  fflush (stdout);
  double x_range_size = max_x - min_x, y_range_size = max_y - min_y;
  const size_t grid_size = 131;
  g_assert (grid_size % 2 == 1);
  size_t mapping_count = pow ((double) grid_size, 2.0);
  struct data_to_fit dtf;
  dtf.grid_size = grid_size;
  dtf.n = mapping_count;
  dtf.x_proj = g_new0 (double, mapping_count);
  dtf.y_proj = g_new0 (double, mapping_count);
  dtf.x_pix = g_new0 (double, mapping_count);
  dtf.y_pix = g_new0 (double, mapping_count);
  // Determine the density and stride for the sparse grid.
  const size_t sparse_grid_sample_stride = 2;
  const size_t sparse_grid_size = grid_size / 2 + 1;
  size_t sparse_mapping_count = pow ((double) sparse_grid_size, 2.0);
  dtf.sparse_grid_size = sparse_grid_size;
  dtf.sparse_n = sparse_mapping_count;
  dtf.sparse_x_proj = g_new0 (double, sparse_mapping_count);
  dtf.sparse_y_proj = g_new0 (double, sparse_mapping_count);
  dtf.sparse_x_pix = g_new0 (double, sparse_mapping_count);
  dtf.sparse_y_pix = g_new0 (double, sparse_mapping_count);
  // Spacing between grid points, in projection coordinates.
  double x_spacing = x_range_size / (grid_size - 1);
  double y_spacing = y_range_size / (grid_size - 1);
  // Index into the flattened list of mappings we want to produce.
  size_t current_mapping = 0;
  size_t current_sparse_mapping = 0;
  size_t ii;
  for ( ii = 0 ; ii < grid_size ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < grid_size ; jj++ ) {
      g_assert (sizeof (long int) >= sizeof (size_t));
      // Projection coordinates for the current grid point.
      double cxproj = min_x + x_spacing * jj;
      double cyproj = min_y + y_spacing * ii;
      // Corresponding latitude and longitude.
      double lat, lon;
      return_code = unproject (pp, cxproj, cyproj, &lat, &lon);
      if ( !return_code ) {
	    // Details of the error should have already been printed.
	    asfPrintError ("Projection Error!\n");
      }
      lat *= R2D;
      lon *= R2D;
      // Corresponding pixel indicies in input image.
      double x_pix, y_pix;
      if ( input_projected ) {
	    // Input projection coordinates of the current pixel.
	    double ipcx, ipcy;
	    return_code = project_input (ipp, D2R * lat, D2R * lon,
				                     &ipcx, &ipcy);
	if ( return_code == 0 ) {
	  g_assert_not_reached ();
	}
	g_assert (return_code);
	// Find the input image pixel indicies corresponding to input
	// projection coordinates.
	x_pix = (ipcx - ipb->startX) / ipb->perX;
	y_pix = (-ipcy + ipb->startY) / ipb->perY;
      }
      else {
	meta_get_lineSamp (imd, lat, lon, average_height, &y_pix, &x_pix);
      }
      dtf.x_proj[current_mapping] = cxproj;
      dtf.y_proj[current_mapping] = cyproj;
      dtf.x_pix[current_mapping] = x_pix;
      dtf.y_pix[current_mapping] = y_pix;

      if ( ii % sparse_grid_sample_stride == 0
	   && jj % sparse_grid_sample_stride == 0 ) {
	dtf.sparse_x_proj[current_sparse_mapping] = cxproj;
	dtf.sparse_y_proj[current_sparse_mapping] = cyproj;
	dtf.sparse_x_pix[current_sparse_mapping] = x_pix;
	dtf.sparse_y_pix[current_sparse_mapping] = y_pix;
	current_sparse_mapping++;
      }
      current_mapping++;
    }
  }

  asfPrintStatus ("done.\n\n");

  // Here are some convenience macros for the spline model.
#define X_PIXEL(x, y) reverse_map_x (&dtf, x, y)
#define Y_PIXEL(x, y) reverse_map_y (&dtf, x, y)

  // We want to choke if our worst point in the model is off by this
  // many pixels or more.
  double max_allowable_error = 1.25;

  // Check the health of the our spline model by comparing the input
  // image pixel coordinates predicted by the model for each point
  // with the known values.
  {
    // This is a small image which will show a visual of the
    // distribution of errors in the output grid.
    FloatImage *error_map = float_image_new (grid_size, grid_size);

    gsl_vector *model_x_errors = gsl_vector_alloc (dtf.n);
    gsl_vector *model_y_errors = gsl_vector_alloc (dtf.n);
    gsl_vector *model_errors = gsl_vector_alloc (dtf.n);
    for ( ii = 0 ; ii < dtf.n ; ii++ ) {
      // x pixel index in input image as predicted by model.
      double xpfm = X_PIXEL (dtf.x_proj[ii], dtf.y_proj[ii]);
      double ypfm = Y_PIXEL (dtf.x_proj[ii], dtf.y_proj[ii]);
      double x_error = xpfm - dtf.x_pix[ii];
      double y_error = ypfm - dtf.y_pix[ii];
      double error_distance = sqrt (pow (x_error, 2) + pow (y_error, 2));
      float_image_set_pixel (error_map, ii % grid_size, ii / grid_size,
			     error_distance);
      gsl_vector_set (model_x_errors, ii, x_error);
      gsl_vector_set (model_y_errors, ii, y_error);
      gsl_vector_set (model_errors, ii, error_distance);
    }
    // Uncomment this line to get an image showing the distribution of
    // errors in the approximating grid.
    // float_image_export_as_jpeg (error_map, "error_map.jpeg", grid_size);
    float_image_free (error_map);
    double mean_error
      = gsl_stats_mean (model_errors->data, model_errors->stride,
			model_errors->size);
    double error_standard_deviation
      = gsl_stats_sd_m (model_errors->data, model_errors->stride,
			model_errors->size, mean_error);
    double max_x_error = gsl_vector_max (model_x_errors);
    double min_x_error = gsl_vector_min (model_x_errors);
    double largest_x_error;
    if ( fabs (max_x_error) > fabs (min_x_error) ) {
      largest_x_error = max_x_error;
    }
    else {
      largest_x_error = min_x_error;
    }
    double max_y_error = gsl_vector_max (model_y_errors);
    double min_y_error = gsl_vector_min (model_y_errors);
    double largest_y_error;
    if ( fabs (max_y_error) > fabs (min_y_error) ) {
      largest_y_error = max_y_error;
    }
    else {
      largest_y_error = min_y_error;
    }
    double largest_error = gsl_vector_max (model_errors);
    if ( largest_error > max_allowable_error ) {
            print_large_error_blurb(force_flag);
	    report_func("Largest Error was larger than maximum allowed! "
		            "%f > %f\n", largest_error, max_allowable_error);
    }
    asfPrintStatus ("For the differences between spline model values and "
		    "projected values\nfor the analytically projected "
		    "control points:\n");
    asfPrintStatus ("Mean: %g\n", mean_error);
    asfPrintStatus ("Standard deviation: %g\n", error_standard_deviation);
    asfPrintStatus ("Maximum (Worst observed error in pixel index distance): "
		    "%g\n", largest_error);
    asfPrintStatus ("Maximum x error (worst observed error in x pixel index): "
		    "%g\n", largest_x_error);
    asfPrintStatus ("Maximum y error (worst observed error in y pixel index): "
		    "%g\n", largest_y_error);
    gsl_vector_free (model_errors);
    gsl_vector_free (model_y_errors);
    gsl_vector_free (model_x_errors);
  }

  // If we don't have a projected image, we are basing things on
  // meta_get_lineSamp, god help us.  Check correctness of reverse
  // mappings of some corners, as an extra paranoid check.  We insist
  // on the model being within this many pixels for reverse
  // transformations of the projection coordinates of the corners of
  // the output image back to the pixel indicies in the input image.
  if ( !input_projected ) {

    // The maximum corner error we are willing to tolerate.
    double max_corner_error;

    // The so called scansar projection has problems that prevent us
    // from getting as good a match as we would like (see below about
    // asymmetry or meta_get_latLon and meta_get_lineSamp).
    // FIXME: Fix the broken scansar crap *HARD*.
    if (  imd->sar->image_type == 'P' ) {
      g_assert (imd->projection->type == SCANSAR_PROJECTION);
      max_corner_error = 3 * max_allowable_error;
    }
    else {
      max_corner_error = max_allowable_error;
    }

    // Upper left corner.
    double ul_lat, ul_lon;
    meta_get_latLon (imd, 0.0, 0.0, average_height, &ul_lat, &ul_lon);

    // Test the symmetry of meta_get_latLon/meta_get_lineSamp.  I
    // believe it is pretty good for everything but scansar projected
    // input data, where it is off by 1.5% or so and therefore throws
    // this error check just a bit outside of a pixel.  But if the
    // problem is somewhere else I want to know.
    if (  imd->sar->image_type != 'P' ) {
      const double stpx = 1.0, stpy = 1.0;   // Symmetry test pixel indicies.
      double st_lat, st_lon;   // Symmetry test lat and long values.
      meta_get_latLon (imd, stpx, stpy, average_height, &st_lat, &st_lon);
      double strx, stry;       // Symmetry test result values.
      meta_get_lineSamp (imd, st_lat, st_lon, average_height, &strx, &stry);
      // We will insist that the results are symmetric to within this
      // fraction after transforming out and back.
      // const double sym_th = 0.001;   // Symmetry threshold.
      // g_assert (fabs (strx - stpx) < sym_th && fabs (stry - stpy) < sym_th);
      // Hmm, looke like they are all pretty bad.  Oh well, the
      // problem of large corner errors when none of the intermediate
      // grid points were off by much still seems specific to scansar.
    }

    double ul_x, ul_y;
    project (pp, D2R * ul_lat, D2R * ul_lon, &ul_x, &ul_y);
    double ul_x_pix_approx = X_PIXEL (ul_x, ul_y);
    if (fabs (ul_x_pix_approx) > max_corner_error ) {
      print_large_error_blurb(force_flag);
      report_func("Upper left x corner error was too large!  %f > %f\n",
		  fabs (ul_x_pix_approx), max_corner_error );
    }
    else {
      asfPrintStatus ("Upper left x corner error: %f\n",
		      fabs (ul_x_pix_approx));
    }

    double ul_y_pix_approx = Y_PIXEL (ul_x, ul_y);
    if (fabs (ul_y_pix_approx) > max_corner_error ) {
      print_large_error_blurb(force_flag);
      report_func ("Upper left y corner error was too large! %f > %f\n",
		   fabs (ul_y_pix_approx), max_corner_error );
    }
    else {
      asfPrintStatus ("Upper left y corner error: %f\n",
		      fabs (ul_y_pix_approx));
    }

    // Lower right corner.
    double lr_lat, lr_lon;
    meta_get_latLon (imd, (float) (ii_size_y - 1), (float) (ii_size_x - 1),
		     average_height, &lr_lat, &lr_lon);
    double lr_x, lr_y;
    project (pp, D2R * lr_lat, D2R * lr_lon, &lr_x, &lr_y);
    double lr_x_pix_approx = X_PIXEL (lr_x, lr_y);
    double lr_x_corner_error = fabs (lr_x_pix_approx - (ii_size_x - 1));
    if ( lr_x_corner_error > max_corner_error ) {
      print_large_error_blurb(force_flag);
      report_func ("Lower right x corner error was too large! %f > %f\n",
		   lr_x_corner_error, max_corner_error);
    }
    else {
      asfPrintStatus ("Lower right x corner error: %f\n", lr_x_corner_error);
    }
    double lr_y_pix_approx = Y_PIXEL (lr_x, lr_y);
    double lr_y_corner_error = fabs (lr_y_pix_approx - (ii_size_y - 1));
    if ( lr_y_corner_error > max_corner_error ) {
      print_large_error_blurb(force_flag);
      report_func ("Lower right Y corner error was too large! %f > %f\n",
		     lr_y_corner_error, max_corner_error);
    }
    else {
      asfPrintStatus ("Lower right Y corner error: %f\n", lr_y_corner_error);
    }
  }

  asfPrintStatus ("\n");

  // Done with the input metadata.
  meta_free (imd);

  // Now we are ready to produce our output image.
  asfPrintStatus ("Resampling input image into output image "
		  "coordinate space...\n");

  // Projection coordinates per pixel in output image.  There is a
  // significant assumption being made here: we assume that the
  // projection coordinates (which are in meters) come at least
  // reasonably close to the ground distances.  This way, when we set
  // the projection coordinates per pixel in the output image equal to
  // the pixel size of the input image, we should be resampling at
  // close to one-to-one (which is where resampling works and we don't
  // have to worry about pixel averaging or anything).
  double pc_per_x = pixel_size;
  double pc_per_y = pixel_size;

  // Maximum pixel indicies in output image.
  size_t oix_max = ceil ((max_x - min_x) / pc_per_x);
  size_t oiy_max = ceil ((max_y - min_y) / pc_per_y);

  // Input image.
  GString *input_data_file = g_string_new (input_image->str);
  g_string_append (input_data_file, ".img");
  FloatImage *iim
    = float_image_new_from_file (ii_size_x, ii_size_y, input_data_file->str, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_string_free (input_data_file, TRUE);

  // Output image.
  FloatImage *oim = float_image_new (oix_max + 1, oiy_max + 1);

  // Convenience macros for getting a pixel.
#define SET_PIXEL(x, y, value) float_image_set_pixel (oim, x, y, value)

  // Translate the command line notion of the resampling method into
  // the lingo known by the float_image class.  The compiler is
  // reassured with a default.
  float_image_sample_method_t float_image_sample_method
    = FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
  switch ( resample_method ) {
  case RESAMPLE_NEAREST_NEIGHBOR:
    float_image_sample_method = FLOAT_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR;
    break;
  case RESAMPLE_BILINEAR:
    float_image_sample_method = FLOAT_IMAGE_SAMPLE_METHOD_BILINEAR;
    break;
  case RESAMPLE_BICUBIC:
    float_image_sample_method = FLOAT_IMAGE_SAMPLE_METHOD_BICUBIC;
    break;
  default:
    g_assert_not_reached ();
  }

  // Set the pixels of the output image.
  size_t oix, oiy;		// Output image pixel indicies.
  for ( oiy = 0 ; oiy <= oiy_max ; oiy++ ) {
    for ( oix = 0 ; oix <= oix_max ; oix++ ) {
      // Projection coordinates for the center of this pixel.
      double oix_pc = ((double) oix / oix_max) * (max_x - min_x) + min_x;
      // We want projection coordinates to increase as we move from
      // the bottom of the image to the top, so that north ends up up.
      double oiy_pc = (1.0 - (double) oiy / oiy_max) * (max_y - min_y) + min_y;
      // Determine pixel of interest in input image.  The fractional
      // part is desired, we will use some sampling method to
      // interpolate between pixel values.
      double input_x_pixel = X_PIXEL (oix_pc, oiy_pc);
      double input_y_pixel = Y_PIXEL (oix_pc, oiy_pc);
      // If we are outside the extent of the input image, set to the
      // fill value.  FIXME: user should be able to specify fill value?
      const float fill_value = 0.0;
      g_assert (ii_size_x <= SSIZE_MAX);
      g_assert (ii_size_y <= SSIZE_MAX);
      if (    input_x_pixel < 0
	   || input_x_pixel > (ssize_t) ii_size_x - 1.0
	   || input_y_pixel < 0
	   || input_y_pixel > (ssize_t) ii_size_y - 1.0 ) {
	SET_PIXEL (oix, oiy, (float) fill_value);
      }
      // Otherwise, set to the value from the appropriate position in
      // the input image.
      else {
	SET_PIXEL (oix, oiy,
		   float_image_sample
		     (iim, input_x_pixel, input_y_pixel,
		      float_image_sample_method));
      }
    }
    asfLineMeter(oiy, oiy_max + 1 );
  }

  asfPrintStatus ("\nDone resampling image.\n\n");

  float_image_free (iim);

  // Now we need some metadata for the output image.  We will just
  // start with the metadata from the input image and add the
  // geocoding parameters.
  meta_parameters *omd = meta_read (input_meta_data->str);
  double x_pixel_size = omd->general->x_pixel_size;
  double y_pixel_size = omd->general->y_pixel_size;
  double x_scale = pixel_size / x_pixel_size;
  double y_scale = pixel_size / y_pixel_size;

  // Flip the image if the y pixel size is negative
  if (y_pixel_size < 0) {
    asfPrintStatus("Negative y pixel size, flipping output image.\n");
    float_image_flip_y(oim);
    y_pixel_size = -y_pixel_size;
  }

  // Store the output image, and free image resources.
  GString *output_data_file = g_string_new (output_image->str);
  g_string_append (output_data_file, ".img");
  asfPrintStatus ("Storing geocoded image...\n");
  return_code = float_image_store (oim, output_data_file->str,
				   FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  asfPrintStatus ("\nDone storing geocoded image.\n\n");
  asfRequire (return_code == 0, "Error saving image.\n");
  float_image_free (oim);
  g_string_free (output_data_file, TRUE);

  // Fix up the output metadata to match the user's selected pixel size
  omd->general->x_pixel_size = pixel_size;
  omd->general->y_pixel_size = pixel_size;
  omd->general->line_count = oiy_max + 1;
  omd->general->sample_count = oix_max + 1;
  omd->sar->image_type = 'P';
  omd->sar->range_time_per_pixel *= x_scale;
  omd->sar->azimuth_time_per_pixel *= y_scale;
  omd->sar->range_doppler_coefficients[1] *= x_scale;
  omd->sar->range_doppler_coefficients[2] *= x_scale * x_scale;
  omd->sar->azimuth_doppler_coefficients[1] *= y_scale;
  omd->sar->azimuth_doppler_coefficients[2] *= y_scale * y_scale;
  if (omd->projection) {
    if (omd->projection->perY > 0)
      pc_per_y = (int) (omd->projection->perY / y_pixel_size + 0.5) * pixel_size;
    else
      pc_per_y = (int) (-omd->projection->perY / y_pixel_size + 0.5) * pixel_size;
  }
  omd->projection = MALLOC(sizeof(meta_projection));
  memset (omd->projection, 0, sizeof(meta_projection));
  omd->projection->type = projection_type;
  omd->projection->startX = min_x;
  omd->projection->startY = max_y;
  omd->projection->perX = pc_per_x;
  omd->projection->perY = pc_per_y;
  strcpy (omd->projection->units, "meters");
  if ( lat_0 > 0.0 ) {
    omd->projection->hem = 'N';
  }
  else {
    omd->projection->hem = 'S';
  }

  /* output the correct earth radii based on the datum that was used
     to do the projection */
  omd->projection->datum = datum;
  if (datum == WGS84_DATUM) {
    const double wgs84_semiminor_axis
      = WGS84_SEMIMAJOR * (1 - (1 / WGS84_INV_FLATTENING));
    omd->projection->re_major = WGS84_SEMIMAJOR;
    omd->projection->re_minor = wgs84_semiminor_axis;
  } else if (datum == NAD27_DATUM) {
    // NAD27 datum is based on clark 1866 ellipsoid
    const double nad27_semiminor_axis
      = CLARKE1866_SEMIMAJOR * (1 - (1 / CLARKE1866_INV_FLATTENING));
    omd->projection->re_major = CLARKE1866_SEMIMAJOR;
    omd->projection->re_minor = nad27_semiminor_axis;
  } else if (datum == NAD83_DATUM) {
    // NAD83 datum is based on GRS80 ellipsoid
    const double nad83_semiminor_axis
      = GRS1980_SEMIMAJOR * (1 - (1 / GRS1980_INV_FLATTENING));
    omd->projection->re_major = GRS1980_SEMIMAJOR;
    omd->projection->re_minor = nad83_semiminor_axis;
  } else {
    asfPrintError("Unsupported datum! %d\n", datum);
  }

  // We need to convert things in this structure back to degrees.
  to_degrees (projection_type, pp);
  omd->projection->param = *pp;
  meta_write (omd, output_meta_data->str);
  meta_free (omd);

  // Done with the data being modeled.  Can't call revers_map_*
  // functions anymore after this (so can't use *_PIXEL macros
  // either).
  g_free (dtf.sparse_y_pix);
  g_free (dtf.sparse_x_pix);
  g_free (dtf.sparse_y_proj);
  g_free (dtf.sparse_x_proj);
  g_free (dtf.y_pix);
  g_free (dtf.x_pix);
  g_free (dtf.y_proj);
  g_free (dtf.x_proj);

  // Done with the file name arguments.
  g_string_free (input_meta_data, TRUE);
  g_string_free (input_image, TRUE);
  g_string_free (output_meta_data, TRUE);
  g_string_free (output_image, TRUE);

  // Close Log, if needed
  if (logflag)
    FCLOSE (fLog);

  exit (EXIT_SUCCESS);
}
