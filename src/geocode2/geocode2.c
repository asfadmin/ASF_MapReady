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
"   asf_geocode"

#define ASF_USAGE_STRING \
"     --help"

#define ASF_DESCRIPTION_STRING \
"     This program takes an unprojected image in the ASF internal\n"\
"     format and geocodes it, i.e. swizzles it around into one of the\n"\
"     standard projections used for maps (universal transverse\n"\
"     mercator, polar stereo, etc).  The output is a new image in ASF\n"\
"     internal format."

#define ASF_INPUT_STRING \
"     Most of the \"options\" are actually required.  The specification\n"\
"     of a certain projection type implies that all the parameters\n"\
"     required to fully specify a projection of that type be included.\n"\
"     \n"\
"     This must be an ASF internal format image base name."

#define ASF_OUTPUT_STRING \
"     The base name of the geocoded image to produce."

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
"     --projection , -p : Projection\n"\
"          Projection to use.  Argument must be one of the following:\n"\
"               utm    - Universal Transverse Mercator\n"\
"               ps     - Polar stereo\n"\
"               lamcc  - Lambert conformal conic\n"\
"	       lamaz  - Lambert azimuthal equal area\n"\
"               albers - Albers conical equal area\n"\
"\n"\
"     UTM\n"\
"     ---\n"\
"          --zone,                     : Zone\n"\
"          --central-meridian          : Longitude\n"\
"	  \n"\
"	  Either the zone or center_longitude must be specified.\n"\
"\n"\
"          Example:\n"\
"               --projection utm --zone <zone>\n"\
"\n"\
"\n"\
"     POLAR STEREO\n"\
"     ------------\n"\
"          --center-latitude            : Center Latitude\n"\
"          --central-meridian           : Center Longitude\n"\
"          -n, --north_pole             : Center on North Pole (no argument)\n"\
"          -s, --south_pole             : Center on South Pole (no argument)\n"\
"          --false-easting              : False Easting\n"\
"          --false-northing             : False Northing\n"\
"\n"\
"        Examples:\n"\
"          --projection ps --center-latitude <lat> --central-meridian <lon> -n\n"\
"          -p ps --center-latitude <lat> --central-meridian <lon> --south-pole\n"\
"\n"\
"     LAMBERT CONFORMAL CONIC\n"\
"     -----------------------\n"\
"          --first-standard-parallel   : First Standard Parallel\n"\
"          --second-standard-parallel  : Second Standard Parallel\n"\
"          --center-latitude           : Original lat\n"\
"          --central-meridian          : Original lon\n"\
"          --false-easting             : False Easting\n"\
"          --false-northing            : False Northing\n"\
"          --scale-factor              : Scale Factor\n"\
"\n"\
"     LAMBERT AZIMUTHAL EQUAL AREA\n"\
"     ----------------------------\n"\
"          --center-latitude           : Original lat\n"\
"          --central-meridian          : Original lon\n"\
"          --false-easting             : False Easting\n"\
"          --false-northing            : False Northing\n"\
"\n"\
"     ALBERS CONICAL EQUAL AREA\n"\
"     -------------------------\n"\
"          --first-standard-parallel   : First Standard Parallel\n"\
"          --second-standard-parallel  : Second Standard Parallel\n"\
"          --center-latitude           : Original lat\n"\
"          --central-meridian          : Original lon\n"\
"          --false-easting             : False Easting\n"\
"          --false-northing            : False Northing\n"\
"          --scale-factor              : Scale Factor\n"\
"\n"\
"     Using a Projection Parameters File\n"\
"     ==================================\n"\
"\n"\
"     --write-proj-file <file>\n"\
"          Save the specified projection information to a file with\n"\
"          the given name.  The file may be used for subsequent projections\n"\
"          with '--read-proj-file'.\n"\
"\n"\
"     --read-proj-file <file>\n"\
"          Read projection information from the given file.  The format of\n"\
"          the file must match what is used with '--write-proj-file'.\n"\
"\n"\
"     Other Options\n"\
"     =============\n"\
"\n"\
"     --height <height> \n"\
"          Assume that terrain in the image is <height> meters above\n"\
"          the reference GEM6 ellipsoid.  Optimal geolocation accuracy\n"\
"          will then be achieved for pixels on terrain at this height.\n"\
"          The geolocation of terrain at other height will be off by\n"\
"          about the height difference between the terrain and\n"\
"          <height>, assuming a satellite look angle 45 degrees from\n"\
"          horizontal.\n"\
"\n"\
"     --pixel-size <size>\n"\
"          Scale output image such that each pixel is <size> projection\n"\
"          coordinates (i.e. meters) on a side."

#define ASF_EXAMPLES_STRING \
"     To map project an image with centerpoint at -147 degrees\n"\
"     longitude and average height 466 meters into universal transverse\n"\
"     mercator projection, with one pixel 50 meters on a side:\n"\
"\n"\
"     asf_project --projection utm --central-meridian -147.0 --height 466\n"\
"                 --pixel-size 50 input_image output_image\n"\
""

#define ASF_LIMITATIONS_STRING \
"     May fail badly if bad projection parameters are supplied for the\n"\
"     area in the image.\n"\
"\n"\
"     Does not reproject images."

#define ASF_SEE_ALSO_STRING \
"     asf_import, asf_export"

#define ASF_COPYRIGHT_STRING \
"Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks\n"\
"All rights reserved.\n"\
"\n"\
"Redistribution and use in source and binary forms, with or without\n"\
"modification, are permitted provided that the following conditions are met:\n"\
"\n"\
"    * Redistributions of source code must retain the above copyright notice,\n"\
"      this list of conditions and the following disclaimer.\n"\
"    * Redistributions in binary form must reproduce the above copyright\n"\
"      notice, this list of conditions and the following disclaimer in the\n"\
"      documentation and/or other materials provided with the distribution.\n"\
"    * Neither the name of the Geophysical Institute nor the names of its\n"\
"      contributors may be used to endorse or promote products derived from\n"\
"      this software without specific prior written permission.\n"\
"\n"\
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"\n"\
"AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"\
"IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n"\
"ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE\n"\
"LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"\
"CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF\n"\
"SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS\n"\
"INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN\n"\
"CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)\n"\
"ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE\n"\
"POSSIBILITY OF SUCH DAMAGE.\n"\
"\n"\
"       For more information contact us at:\n"\
"\n"\
"       Alaska Satellite Facility\n"\
"       Geophysical Institute\n"\
"       University of Alaska Fairbanks\n"\
"       P.O. Box 757320\n"\
"       Fairbanks, AK 99775-7320\n"\
"\n"\
"       http://www.asf.alaska.edu\n"\
"       uso@asf.alaska.edu"

#define ASF_PROGRAM_HISTORY_STRING \
"   No history."

#define ASF_VERSION_MAJOR_STRING \
"0.10"

/*===================END ASF AUTO-GENERATED DOCUMENTATION===================*/

// Standard libraries.
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Libraries from packages outside ASF.
#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>
#include <gsl/gsl_statistics_double.h>

// Libraries developed at ASF.
#include <asf.h>
#include <asf_meta.h>
#include <asf_raster.h>
#include "float_image.h"
#include <libasf_proj.h>

// Headers used by this program.
#include "geocode_options.h"

// Print invocation information.  */
static void
usage (void)
{
  printf ("\n"
	  "USAGE:\n"
	  ASF_NAME_STRING
	  " "
	  ASF_USAGE_STRING
	  "\n\n");
  exit (EXIT_FAILURE);
}

// Display a manual page using the pager.
static void
help_page (void)
{
  // Documentation text to display.
  GString *documentation = g_string_new ("");
  // System command to use to display it.
  GString *command = g_string_new ("");

  /* What to print out for help */
  g_string_append_printf 
    (documentation, 
     "\n\n\n"
     "Tool name:\n" ASF_NAME_STRING "\n\n\n"
     "Usage:\n" ASF_NAME_STRING " " ASF_USAGE_STRING "\n\n\n"
     "Description:\n" ASF_DESCRIPTION_STRING "\n\n\n"
     "Input:\n" ASF_INPUT_STRING "\n\n\n"
     "Output:\n"ASF_OUTPUT_STRING "\n\n\n"
     "Options:\n" ASF_OPTIONS_STRING "\n\n\n"
     "Examples:\n" ASF_EXAMPLES_STRING "\n\n\n"
     "Limitations:\n" ASF_LIMITATIONS_STRING "\n\n\n"
     "See also:\n" ASF_SEE_ALSO_STRING "\n\n\n"
     "Version:\n" CONVERT_PACKAGE_VERSION_STRING "\n\n\n"
     "Copyright:\n" ASF_COPYRIGHT_STRING "\n\n\n");
  
  // If we can, use less.
  g_string_append_printf 
    (command, 
     "echo '%s' | less --prompt='Type q to quit help, h for help with help "
     "browser'", documentation->str);
  if ( system (command->str) == 0 )
    exit (EXIT_SUCCESS);

  // Hmmm, less didn't work cause we got here, try using more.
  g_string_append_printf (command, "echo '%s' | more", documentation->str);
  if ( system (command->str) == 0 )
    exit (EXIT_SUCCESS);

  // Okay, neither less or more work (obviously if we made it here),
  // just print the info straight to stdout and exit
  printf (documentation->str);
  exit (EXIT_SUCCESS);
}

// Factors for going between degrees and radians.
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	0.0174532925199432958

///////////////////////////////////////////////////////////////////////////////
// 
// We want to find two 2D cubics that map points in the output space
// to x and y pixel indicies of points in the input space.  In
// equations, we want:
// 
//      X(x, y) = ax^3 + by^3 + cx^2y + dy^2x + ex^2 + gy^2 + hxy + ix + jy + k
//      Y(x, y) = lx^3 + my^3 + nx^2y + oy^2x + px^2 + qy^2 + rxy + sx + ty + u
//
// We need to find model coefficients a through u, excluding f.  Then
// we will be able to take projection coordinates x, y and find the
// corresponding pixel indicies (X, Y) in the input image.
//
// This is a simple case of the general method described in "Map
// Projections a Reference Manual", section 10.2.2, by Lev
// M. Bugayevskiy and John P. Snyder.
//
///////////////////////////////////////////////////////////////////////////////

// This is the input data we want to fit curves to.
struct data_to_fit {
  size_t n;			// Number of transformed points.
  double *x_proj;		// Projection x coordinates.
  double *y_proj;		// Projection y coordinates.
  // Input image pixel coordinates put 0, 0 at the top left.
  double *x_pix;		// Input image pixel x coordinate.
  double *y_pix;		// Input image pixel y coordinate.
};

// The GNU Scientific Library (GSL) needs functions that take vectors
// of the model parameters as arguments.  The mapping of cubic
// coefficients for a cubic of the form 
//
//      ax^3 + by^3 + cx^2y + dy^2x + ex^2 + gy^2 + hxy + ix + jy + k 
//
// to vector positions is shown below.  Note that all cubic
// coefficient offsets are refered to with these offsets, including
// those for cubic Y(x, y) (for which different coefficients were
// shown in above comments).
static const size_t a_index = 0;
static const size_t b_index = 1;
static const size_t c_index = 2;
static const size_t d_index = 3;
static const size_t e_index = 4;
static const size_t g_index = 5;
static const size_t h_index = 6;
static const size_t i_index = 7;
static const size_t j_index = 8;
static const size_t k_index = 9;

// Evalutate cubic 
//
//      ax^3 + by^3 + cx^2y + dy^2x + ex^2 + gy^2 + hxy + ix + jy + k 
//
// where a, b, c, d, e, g, h, i, j, and k are the first ten elements
// of the coefficients vector.
static double
evaluate_cubic (const gsl_vector *coefficients, double x, double y)
{
  double a = gsl_vector_get (coefficients, a_index);
  double b = gsl_vector_get (coefficients, b_index);
  double c = gsl_vector_get (coefficients, c_index);
  double d = gsl_vector_get (coefficients, d_index);
  double e = gsl_vector_get (coefficients, e_index);
  double g = gsl_vector_get (coefficients, g_index);
  double h = gsl_vector_get (coefficients, h_index);
  double i = gsl_vector_get (coefficients, i_index);
  double j = gsl_vector_get (coefficients, j_index);
  double k = gsl_vector_get (coefficients, k_index);

  return (a * pow (x, 3.0) + b * pow (y, 3.0) + c * pow (x, 2.0) * y
	  + d * pow (y, 2.0) * x + e * pow (x, 2.0) + g * pow (y, 2.0)
	  + h * x * y + i * x + j * y + k);
}

// To get the best fit, we will perform least squares minimization on
// the difference between modeled coordinates and a batch of
// "measured" (actually transformed with libproj) ones.  This function
// is to be called by the minimization routines in the GSL.  The first
// parameter (named x) is a vector of the set of cubic coefficients
// currently under consideration by the minimizer, and has nothing to
// do with an x coordinate.  The parameter names used were chosen for
// consistency with the GSL types and examples in the GSL
// documentation.
static int
fit_x_coordinates_cubic_f (const gsl_vector *x, void *params, gsl_vector *f)
{
  struct data_to_fit *dtfs = (struct data_to_fit *)params;
  size_t n = dtfs->n;
  double *x_proj = dtfs->x_proj;
  double *y_proj = dtfs->y_proj;
  double *x_pix = dtfs->x_pix;

  size_t i;
  for ( i = 0 ; i < n ; i++ ) {
    double x_pix_modeled = evaluate_cubic (x, x_proj[i], y_proj[i]);
    gsl_vector_set (f, i, x_pix_modeled - x_pix[i]);
  }

  return GSL_SUCCESS;
}

// Function to minimize to determine coefficients for the Y coordinate model.
static int
fit_y_coordinates_cubic_f (const gsl_vector *x, void *params, gsl_vector *f)
{
  struct data_to_fit *dtfs = (struct data_to_fit *)params;
  size_t n = dtfs->n;
  double *x_proj = dtfs->x_proj;
  double *y_proj = dtfs->y_proj;
  double *y_pix = dtfs->y_pix;

  size_t i;
  for ( i = 0 ; i < n ; i++ ) {
    double y_pix_modeled = evaluate_cubic (x, x_proj[i], y_proj[i]);
    gsl_vector_set (f, i, y_pix_modeled - y_pix[i]);
  }

  return GSL_SUCCESS;
}

// We also need routines to compute the jacobian matrices of the
// fitting functions with respect to the cubic coefficients.  The
// Jacobians of the X and Y approximating functions are the same, so
// we don't need seperate jacobian computers for the X and Y
// cubics.
static int 
fit_coordinates_cubic_df (const gsl_vector *x, void *params, gsl_matrix *J)
{
  // Reassure compiler that we know we don't use x.
  x = x;		

  struct data_to_fit *dtfs = (struct data_to_fit *)params;
  size_t n = dtfs->n;
  double *x_proj = dtfs->x_proj;
  double *y_proj = dtfs->y_proj;

  size_t i;
  for ( i = 0 ; i < n ; i++ ) {
    gsl_matrix_set (J, i, a_index, pow (x_proj[i], 3.0));
    gsl_matrix_set (J, i, b_index, pow (y_proj[i], 3.0));
    gsl_matrix_set (J, i, c_index, pow (x_proj[i], 2.0) * y_proj[i]);
    gsl_matrix_set (J, i, d_index, pow (y_proj[i], 2.0) * x_proj[i]);
    gsl_matrix_set (J, i, e_index, pow (x_proj[i], 2.0));
    gsl_matrix_set (J, i, g_index, pow (y_proj[i], 2.0));
    gsl_matrix_set (J, i, h_index, x_proj[i] * y_proj[i]);
    gsl_matrix_set (J, i, i_index, x_proj[i]);
    gsl_matrix_set (J, i, j_index, y_proj[i]);
    gsl_matrix_set (J, i, k_index, 1.0);
  }

  return GSL_SUCCESS;
}

// For reasons I don't quite understand, it looks like the GSL makes
// us fill in a function-and-jacobian routine even if we haven't done
// any optimizations to simultaneoiusly compute the minimization
// function and the jacobian.  So here we have those routines.
static int
fit_x_coordinates_cubic_fdf (const gsl_vector *x, void *params, gsl_vector *f,
			     gsl_matrix *J)
{
  fit_x_coordinates_cubic_f (x, params, f);
  fit_coordinates_cubic_df (x, params, J);

  return GSL_SUCCESS;
}

static int
fit_y_coordinates_cubic_fdf (const gsl_vector *x, void *params, gsl_vector *f,
			     gsl_matrix *J)
{
  fit_y_coordinates_cubic_f (x, params, f);
  fit_coordinates_cubic_df (x, params, J);

  return GSL_SUCCESS;
}

// Print out information about the state of the solver.
static int
solver_print_state (gsl_multifit_fdfsolver *s, size_t iter)
{
  printf ("iteration: %3u Coefficients: %.9e %.9e %.9e %.9e %.9e %.9e %.9e "
	  "%.9e %.9e %.9e"
	  "|Minimization Criteria| = %g\n",
	  iter,
	  gsl_vector_get (s->x, a_index),
	  gsl_vector_get (s->x, b_index),
	  gsl_vector_get (s->x, c_index),
	  gsl_vector_get (s->x, d_index),
	  gsl_vector_get (s->x, e_index),
	  gsl_vector_get (s->x, g_index),
	  gsl_vector_get (s->x, h_index),
	  gsl_vector_get (s->x, i_index),
	  gsl_vector_get (s->x, j_index),	  
	  gsl_vector_get (s->x, k_index),
	  gsl_blas_dnrm2 (s->f));

  return GSL_SUCCESS;
}

// Main routine.
int
main (int argc, char **argv)
{
  // Get the projection parameters from the command line.
  projection_type_t projection_type;
  // Terrain height to assume.  Defaults to 0.
  double average_height = 0;	// FIXME: Put this default in docs
  // Pixel size to use for output image, in projection coordinate
  // units (presumably meters, but you never know when we might lose
  // our heads and decide to add some dumb projection).
  double pixel_size;
  project_parameters_t *pp 
    = get_geocode_options(&argc, &argv, &projection_type, &average_height, 
			  &pixel_size);
  // If help was requested, display it.
  { // Scoping block.				
    int ii;
    for ( ii = 0 ; ii < argc ; ii++ ) {
      if ( strncmp (argv[ii], "-help", strlen ("-help")) == 0
	   || strncmp (argv[ii], "--help", strlen ("--help")) == 0 ) {
	help_page ();
      }
    }
  }
  // Get non-option command line arguments.
  if ( argc != 3 ) {
    usage ();
  }
  GString *input_image = g_string_new (argv[1]);
  GString *output_image = g_string_new (argv[2]);

  // Convert all angle measures in the project_parameters to radians.
  to_radians (projection_type, pp);

  // Assign our transformation function pointers to point to the
  // appropriate functions.
  int (*project) (project_parameters_t *pps, double lat, double lon, double *x,
		  double *y);
  int (*project_arr) (project_parameters_t *pps, double *lat, double *lon,
		      double **projected_x, double ** projected_y, 
		      long length);
  project = NULL;		// Silence compiler warnings.
  project_arr = NULL;		// Silence compiler warnings.
  switch ( projection_type ) {
  case UNIVERSAL_TRANSVERSE_MERCATOR: 
    project = project_utm;
    project_arr = project_utm_arr;
    break;
  case POLAR_STEREOGRAPHIC:
    project = project_ps;
    project_arr = project_ps_arr;
    break;
  case ALBERS_EQUAL_AREA:
    project = project_albers;
    project_arr = project_albers_arr;
    break;
  case LAMBERT_CONFORMAL_CONIC:
    project = project_lamcc;
    project_arr = project_lamcc_arr;
    break;
  case LAMBERT_AZIMUTHAL_EQUAL_AREA:
    project = project_lamaz;
    project_arr = project_lamaz_arr;
    break;
  default:
    g_assert_not_reached ();
    break;
  }

  // Input metadata.
  meta_parameters *imd = meta_read (input_image->str);

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
  printf ("Determining input image extents in projection coordinate "
	  "space...\n");
  double min_x = DBL_MAX;
  double max_x = - DBL_MAX;
  double min_y = DBL_MAX;
  double max_y = - DBL_MAX;

  { // Scoping block.
    // Number of pixels in the edge of the image.
    size_t edge_point_count = 2 * ii_size_x + 2 * ii_size_y - 4;
    double *lats = g_new (double, edge_point_count);
    double *lons = g_new (double, edge_point_count);
    size_t current_edge_point = 0;
    size_t ii = 0, jj = 0;
    for ( ; ii < ii_size_x - 1 ; ii++ ) {
      meta_get_latLon (imd, (double)jj, (double)ii, average_height, 
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    for ( ; jj < ii_size_y - 1 ; jj++ ) {
      meta_get_latLon (imd, (double)jj, (double)ii, average_height,
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    for ( ; ii > 0 ; ii-- ) {
      meta_get_latLon (imd, (double)jj, (double)ii, average_height, 
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    for ( ; jj > 0 ; jj-- ) {
      meta_get_latLon (imd, (double)jj, (double)ii, average_height, 
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    g_assert (current_edge_point == edge_point_count);
    // Pointers to arrays of projected coordinates to be filled in.
    // The projection function will allocate this memory itself.
    double *x = NULL, *y = NULL;
    x = y = NULL;
    // Project all the edge pixels.
    int return_code = project_arr (pp, lats, lons, &x, &y, edge_point_count);
    g_assert (return_code == TRUE);
    // Find the extents of the image in projection coordinates.
    for ( ii = 0 ; ii < edge_point_count ; ii++ ) {
      if ( x[ii] < min_x ) { min_x = x[ii]; }
      if ( x[ii] > max_x ) { max_x = x[ii]; }
      if ( y[ii] < min_y ) { min_y = y[ii]; }
      if ( y[ii] > max_y ) { max_y = y[ii]; }
    }

    g_free (y);
    g_free (x);
    g_free (lons);
    g_free (lats);
  }

  printf ("\n");

  // Generate some mappings between input image pixel coordinates and
  // output projection coordinates, using proj.  For fun and tradition,
  // we compute transformation for points on a grid_size * grid_size
  // grid.
  printf ("Performing analytical projection of a spacially distributed\n"
	  "subset of input image pixels...\n");
  const size_t grid_size = 10;
  size_t mapping_count = pow ((double)grid_size, 2.0);
  struct data_to_fit dtf;
  dtf.n = mapping_count;
  dtf.x_proj = g_new0 (double, mapping_count);
  dtf.y_proj = g_new0 (double, mapping_count);
  dtf.x_pix = g_new0 (double, mapping_count);
  dtf.y_pix = g_new0 (double, mapping_count);
  // Given the grid size and the image dimensions, how many pixels
  // between points we want to get the "truth" about from proj?
  double x_pix_stride = ii_size_x / (grid_size - 1);
  double y_pix_stride = ii_size_y / (grid_size - 1);
  // Index into the flattened list of mappings we want to produce.
  int current_mapping = 0;	
  size_t ii;
  for ( ii = 0 ; ii < grid_size ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < grid_size ; jj++ ) {
      // Input image x and y pixel coordinates.
      size_t xpc = floor (jj * x_pix_stride);
      g_assert (xpc < ii_size_x);
      size_t ypc = floor (ii * y_pix_stride);
      g_assert (ypc < ii_size_y);
      // Corresponding latitude and longitude.
      double lat, lon;
      meta_get_latLon (imd, (double)ypc, (double)xpc, average_height, &lat, 
		       &lon);
      // Corresponding projection coordinates.  */
      lat *= DEG_TO_RAD;
      lon *= DEG_TO_RAD;
      double x, y;
      gboolean return_code = project (pp, lat, lon, &x, &y);
      g_assert (return_code);
      dtf.x_proj[current_mapping] = x;
      dtf.y_proj[current_mapping] = y;
      dtf.x_pix[current_mapping] = xpc;
      dtf.y_pix[current_mapping] = ypc;
      current_mapping++;
    }
  }

  printf ("\n");
  
  // Many projections have huge constants offsets for most areas
  // (unless weird parameters like false easting/false northing are
  // used), which make life tough for least squares fitting
  // algorithms.  So we find the mean value of the transformed
  // coordinates in each dimension and subtract it from all the data
  // points, in effect doing our own generalized false easting/false
  // northing without requiring the user to know about it.  We have to
  // remember to undo this on the way out!
  double x_proj_mean = gsl_stats_mean (dtf.x_proj, 1, dtf.n);
  for ( ii = 0 ; ii < dtf.n ; ii++ ) {
    dtf.x_proj[ii] -= x_proj_mean;
  }
  double y_proj_mean = gsl_stats_mean (dtf.y_proj, 1, dtf.n);
  for ( ii = 0 ; ii < dtf.n ; ii++ ) {
    dtf.y_proj[ii] -= y_proj_mean;
  }  

  // Here we find our cubic models for input pixel coorinates x_pix,
  // y_pix in term of projection coordinates x, y.  For easy
  // reference, we put some variables in the terms used in the GSL
  // documentation.
  printf ("Trying to fit input image x pixel indicies to 2-D cubic\n"
	  "function of output image pixel projection coordinates using\n"
	  "nonlinear least-squares fitting...\n");
  const size_t n = mapping_count;
  const size_t p = 10;		// 2D cubics have six parameters.
  gsl_matrix *covariance = gsl_matrix_alloc (p, p);
  // We don't have a good initial guess at the moment, so we try 0.
  double *x_init = g_new0 (double, p);
  for ( ii = 0 ; ii < p ; ii++ ) {
    g_assert (x_init[ii] == 0.0);
  }
  gsl_vector_view x = gsl_vector_view_array (x_init, p);
  const gsl_multifit_fdfsolver_type *T = gsl_multifit_fdfsolver_lmsder;
  gsl_multifit_fdfsolver *s = gsl_multifit_fdfsolver_alloc (T, n, p);
  gsl_multifit_function_fdf x_f;
  x_f.f = fit_x_coordinates_cubic_f;
  x_f.df = fit_coordinates_cubic_df;
  x_f.fdf = fit_x_coordinates_cubic_fdf;
  x_f.n = n;
  x_f.p = p;
  x_f.params = &dtf;
  gsl_multifit_function_fdf y_f;
  y_f.f = fit_y_coordinates_cubic_f;
  y_f.df = fit_coordinates_cubic_df;
  y_f.fdf = fit_y_coordinates_cubic_fdf;
  y_f.n = n;
  y_f.p = p;
  y_f.params = &dtf;
  int status;			// Status of the fit.
  size_t iteration = 0;		// Current iteration of the fit.
  // Find the X model of interest.
  gsl_multifit_fdfsolver_set (s, &x_f, &x.vector);
  solver_print_state (s, iteration);
  // If we haven't converged after this many iterations, we give up.
  size_t maximum_iterations = 500;
  do {
    iteration++;
    status = gsl_multifit_fdfsolver_iterate (s);
    if ( status == GSL_ETOLF ) { printf ("GSL_ETOLF\n"); }
    if ( status == GSL_ETOLX ) { printf ("GSL_ETOLX\n"); }
    if ( status == GSL_ETOLG ) { printf ("GSL_ETOLG\n"); } 
    printf ("iteration status = %s\n", gsl_strerror (status));
    solver_print_state (s, iteration);
    if ( status != GSL_SUCCESS ) {
      break;
    }
    // There's nothing particularly insightful about these termination
    // conditions, but they seem to work...
    status = gsl_multifit_test_delta (s->dx, s->x, 1e-8, 1e-8);
  } while ( status == GSL_CONTINUE && iteration < maximum_iterations );
  gsl_multifit_covar (s->J, 0.0, covariance);
	  
#define FIT(i) gsl_vector_get (s->x, i)
#define ERR(i) sqrt (gsl_matrix_get (covariance, i, i))

  printf ("Cubicic coefficients: \n"
	  "ax^3 + by^3 + cx^2y + dy^2x + ex^2 + gy^2 + hxy + ix + jy + k: \n");
  printf ("a = %12.5e +/-%12.5e\n", FIT (a_index), ERR (a_index));
  printf ("b = %12.5e +/-%12.5e\n", FIT (b_index), ERR (b_index));
  printf ("c = %12.5e +/-%12.5e\n", FIT (c_index), ERR (c_index));
  printf ("d = %12.5e +/-%12.5e\n", FIT (d_index), ERR (d_index));
  printf ("e = %12.5e +/-%12.5e\n", FIT (e_index), ERR (e_index));
  printf ("g = %12.5e +/-%12.5e\n", FIT (g_index), ERR (g_index));
  printf ("h = %12.5e +/-%12.5e\n", FIT (h_index), ERR (g_index));
  printf ("i = %12.5e +/-%12.5e\n", FIT (i_index), ERR (g_index));
  printf ("j = %12.5e +/-%12.5e\n", FIT (j_index), ERR (g_index));
  printf ("k = %12.5e +/-%12.5e\n", FIT (k_index), ERR (g_index));
  
  // Check the health of the fit.
  {
    double mean_error 
      = gsl_stats_mean (s->f->data, s->f->stride, s->f->size);
    double error_standard_deviation 
      = gsl_stats_sd_m (s->f->data, s->f->stride, s->f->size, mean_error);
    double largest_error = gsl_vector_max (s->f);
    // We want to choke if our worst point in the model is off by this
    // many pixels or more.
    double max_allowable_error = 3.0;
    g_assert (largest_error < max_allowable_error);
    printf ("For the differences between cubic model values and projected "
	    "values:\n");
    printf ("Mean: %g\n", mean_error);
    printf ("Standard deviation: %g\n", error_standard_deviation); 
    printf ("Maximum (Worst observed error in x pixel index): %g\n", 
	    largest_error);
  }

  // Save our results for the x pixel model.
  gsl_vector *x_pix_model_coefficients = gsl_vector_alloc (s->x->size);
  gsl_vector_memcpy (x_pix_model_coefficients, s->x);

  printf ("\n");

  // Find the Y model of interest.
  printf ("Trying to fit input image y pixel indicies to 2-D cubic\n"
	  "function of output image pixel projection coordinates using\n"
	  "nonlinear least-squares fitting...\n");
  // Out of caution, we free and reallocat some stuff.
  gsl_matrix_free (covariance);
  covariance = gsl_matrix_alloc (p, p);
  gsl_multifit_fdfsolver_free (s);
  s = gsl_multifit_fdfsolver_alloc (T, n, p);
  gsl_vector_set_zero (&x.vector); // Set initial guess back to zero.
  gsl_multifit_fdfsolver_set (s, &y_f, &x.vector);
  iteration = 0;		// Reset iteration counter to zero.
  solver_print_state (s, iteration);
  do {
    iteration++;
    status = gsl_multifit_fdfsolver_iterate (s);
    if ( status == GSL_ETOLF ) { printf ("GSL_ETOLF\n"); }
    if ( status == GSL_ETOLX ) { printf ("GSL_ETOLX\n"); }
    if ( status == GSL_ETOLG ) { printf ("GSL_ETOLG\n"); } 
    printf ("iteration status = %s\n", gsl_strerror (status));
    solver_print_state (s, iteration);
    if ( status != GSL_SUCCESS ) {
      break;
    }
    // There's nothing particularly insightful about these termination
    // conditions, but they seem to work...
    status = gsl_multifit_test_delta (s->dx, s->x, 1e-8, 1e-8);
  } while ( status == GSL_CONTINUE && iteration < 500 );
  gsl_multifit_covar (s->J, 0.0, covariance);

  printf ("Cubicic coefficients: \n"
	  "ax^3 + by^3 + cx^2y + dy^2x + ex^2 + gy^2 + hxy + ix + jy + k: \n");
  printf ("a = %12.5e +/-%12.5e\n", FIT (a_index), ERR (a_index));
  printf ("b = %12.5e +/-%12.5e\n", FIT (b_index), ERR (b_index));
  printf ("c = %12.5e +/-%12.5e\n", FIT (c_index), ERR (c_index));
  printf ("d = %12.5e +/-%12.5e\n", FIT (d_index), ERR (d_index));
  printf ("e = %12.5e +/-%12.5e\n", FIT (e_index), ERR (e_index));
  printf ("g = %12.5e +/-%12.5e\n", FIT (g_index), ERR (g_index));
  printf ("h = %12.5e +/-%12.5e\n", FIT (h_index), ERR (g_index));
  printf ("i = %12.5e +/-%12.5e\n", FIT (i_index), ERR (g_index));
  printf ("j = %12.5e +/-%12.5e\n", FIT (j_index), ERR (g_index));
  printf ("k = %12.5e +/-%12.5e\n", FIT (k_index), ERR (g_index));
  
  // Check the health of the fit.
  {
    double mean_error 
      = gsl_stats_mean (s->f->data, s->f->stride, s->f->size);
    double error_standard_deviation 
      = gsl_stats_sd_m (s->f->data, s->f->stride, s->f->size, mean_error);
    double largest_error = gsl_vector_max (s->f);
    // We want to choke if it our worst point in the model is off by
    // this many pixels or more.
    double max_allowable_error = 4.0;
    g_assert (largest_error < max_allowable_error);
    printf ("For the differences between cubic model values and projected "
	    "values:\n");
    printf ("Mean: %g\n", mean_error);
    printf ("Standard deviation: %g\n", error_standard_deviation); 
    printf ("Maximum (worst observed error in y pixel index): %g\n", 
	    largest_error);
  }

  // Save our results for the y pixel model.
  gsl_vector *y_pix_model_coefficients = gsl_vector_alloc (s->x->size);
  gsl_vector_memcpy (y_pix_model_coefficients, s->x);

  // Done with the solver and the data being modeled.
  gsl_multifit_fdfsolver_free (s);
  gsl_matrix_free (covariance);
  g_free (dtf.y_pix);
  g_free (dtf.x_pix);
  g_free (dtf.y_proj);
  g_free (dtf.x_proj);

  // Check correctness of reverse mappings of some corners, as an
  // extra paranoid check.  We insist on the model being within this
  // many pixels for reverse transformations of the projection
  // coordinates of the corners of the output image back to the pixel
  // indicies in the input image.
  double max_corner_error = 2.0;
  // Upper left corner.
  double ul_lat, ul_lon;
  meta_get_latLon (imd, (float)0, (float)0, average_height, &ul_lat, &ul_lon);
  double ul_x, ul_y;
  project (pp, DEG_TO_RAD * ul_lat, DEG_TO_RAD * ul_lon, &ul_x, &ul_y);
  double ul_x_pix_approx = evaluate_cubic (x_pix_model_coefficients, 
					       ul_x - x_proj_mean, 
					       ul_y - y_proj_mean);
  g_assert (fabs (ul_x_pix_approx) < max_corner_error);
  double ul_y_pix_approx = evaluate_cubic (y_pix_model_coefficients, 
					       ul_x - x_proj_mean, 
					       ul_y - y_proj_mean);
  g_assert (fabs (ul_y_pix_approx) < max_corner_error);
  // Lower right corner.
  double lr_lat, lr_lon;
  meta_get_latLon (imd, (float)(ii_size_y - 1), (float)(ii_size_x - 1), 
		   average_height, &lr_lat, &lr_lon);
  double lr_x, lr_y;
  project (pp, DEG_TO_RAD * lr_lat, DEG_TO_RAD * lr_lon, &lr_x, &lr_y);
  double lr_x_pix_approx = evaluate_cubic (x_pix_model_coefficients, 
					   lr_x - x_proj_mean, 
					   lr_y - y_proj_mean);
  g_assert (fabs (lr_x_pix_approx - (ii_size_x - 1)) < max_corner_error);
  double lr_y_pix_approx = evaluate_cubic (y_pix_model_coefficients, 
					   lr_x - x_proj_mean, 
					   lr_y - y_proj_mean);
  g_assert (fabs (lr_y_pix_approx - (ii_size_y - 1)) < max_corner_error);

  // Done with the input metadata.
  meta_free (imd);

  // Ok, we now have model functions we are happy with.  Make some
  // convenience macros for using them.
#define X_PIXEL(x, y) evaluate_cubic (x_pix_model_coefficients, \
				      x - x_proj_mean, y - y_proj_mean)
#define Y_PIXEL(x, y) evaluate_cubic (y_pix_model_coefficients, \
				      x - x_proj_mean, y - y_proj_mean)

  printf ("\n");

  // Now we are ready to produce our output image.  
  printf ("Resampling input image into output image coordinate space...\n");

  // Maximum output image pixel indicies.
  size_t oix_max = ii_size_x - 1, oiy_max = ii_size_y - 1;
  // Projection coordinates per pixel in output image.
  double pc_per_x = (max_x - min_x) / oix_max;
  double pc_per_y = (max_y - min_y) / oiy_max;

  // Input image.
  GString *input_data_file = g_string_new (input_image->str);
  g_string_append (input_data_file, ".img");
  FloatImage *iim 
    = float_image_new_from_file (ii_size_x, ii_size_y, input_data_file->str, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_string_free (input_data_file, TRUE);

  // Output image.
  FloatImage *oim = float_image_new (ii_size_x, ii_size_y);

  // Convenience macros for getting and setting pixels.
#define GET_PIXEL(x, y) float_image_get_pixel (iim, x, y)
#define SET_PIXEL(x, y, value) float_image_set_pixel (oim, x, y, value)

  // Set the pixels of the output image.
  size_t oix, oiy;		// Output image pixel indicies.
  for ( oiy = 0 ; oiy <= oiy_max ; oiy++ ) {
    for ( oix = 0 ; oix <= oix_max ; oix++ ) {
      // Projection coordinates for the center of this pixel.    
      double oix_pc = ((double)oix / oix_max) * (max_x - min_x) + min_x;
      double oiy_pc = ((double)oiy / oiy_max) * (max_y - min_y) + min_y;
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
      if ( input_x_pixel < 0 || input_x_pixel >= (ssize_t)ii_size_x
	   || input_y_pixel < 0 || input_y_pixel >= (ssize_t)ii_size_y ) {
	SET_PIXEL (oix, oiy, (float)fill_value);
      }
      // Otherwise, set to the value from the appropriate position in
      // the input image.
      else {
	SET_PIXEL (oix, oiy, GET_PIXEL (floor (input_x_pixel + 0.5),
					floor (input_y_pixel + 0.5)));
	//SET_PIXEL (oix, oiy, interpolate (NEAREST, iim->data, iim->size1,
	//		                    iim->size2, input_x_pixel,
	//			            input_y_pixel, NO_WEIGHT, 8));
      }
    }
    if ( oiy % 100 == 0 || oiy == oiy_max ) {
      printf ("Finished output image line %d\n", oiy);
    }
  }

  // Done with the models and the input image data.
  float_image_free (iim);
  gsl_vector_free (y_pix_model_coefficients);
  gsl_vector_free (x_pix_model_coefficients);

  GString *output_data_file = g_string_new (output_image->str);
  g_string_append (output_data_file, ".img");
  int return_code = float_image_store (oim, output_data_file->str,
				       FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);
  g_assert (return_code == 0);
  float_image_free (oim);
  g_string_free (output_data_file, TRUE);

  // Now we need some metadata for the output image.  We will just
  // start with the metadata from the input image and add the
  // geocoding parameters.
  meta_parameters *omd = meta_read (input_image->str);
  if ( omd->sar->image_type == 'P' ) {
    g_assert (omd->projection->type == SCANSAR_PROJECTION);
  }
  else {
    g_assert (omd->projection == NULL);
  }
  omd->sar->image_type = 'P';
  omd->projection = g_new0 (meta_projection, 1);
  omd->projection->type = projection_type;
  omd->projection->startX = min_x;
  omd->projection->startY = min_y;
  omd->projection->perX = pc_per_x;
  omd->projection->perY = -pc_per_y;
  strcpy (omd->projection->units, "meters");
  if ( lat_0 > 0.0 ) {
    omd->projection->hem = 'N';
  }
  else {
    omd->projection->hem = 'S';
  }
  const double wgs84_semimajor_axis = 6378137.0;
  const double wgs84_earth_flattening = 1.0 / 298.257223563;
  double wgs84_eccentricity 
    = 2 * wgs84_earth_flattening - pow (wgs84_earth_flattening, 2.0);
  double wgs84_parameter_of_ellipse 
    = wgs84_semimajor_axis * (1.0 - pow (wgs84_eccentricity, 2.0));
  double wgs84_semiminor_axis 
    = sqrt (wgs84_semimajor_axis * wgs84_parameter_of_ellipse);
  omd->projection->re_major = wgs84_semimajor_axis;
  omd->projection->re_minor = wgs84_semiminor_axis;
  // We need to convert things in this structure back to degrees.
  to_degrees (projection_type, pp);
  omd->projection->param = *pp;
  meta_write (omd, output_image->str);
  meta_free (omd);

  // Done with the file name arguments.
  g_string_free (input_image, TRUE);
  g_string_free (output_image, TRUE);

  exit (EXIT_SUCCESS);
}
