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
#include <float_image.h>
#include <libasf_proj.h>
#include <spheroids.h>
#include <asf_contact.h>

// Headers defined by this library.
#include "asf_geocode.h"

// FIXME: *** KLUDGE on windows, need to look further into how we
//            can avoid having to do this.
#ifdef win32
void g_assert_warning(const char *s1, const char *s2, const int i1,
                    const char *s3, const char *s4)
{
  printf("Warning: %s %s %d %s %s\n", s1, s2, i1, s3, s4);
  exit(EXIT_FAILURE);
}
#endif

// Prototype
void check_parameters(projection_type_t projection_type,
		      project_parameters_t *pp, meta_parameters *meta,
		      int override_checks);

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

// We want to trap segmentation faults (signal number 11,
// i.e. SIGSEGV) and try to produce a backtrace.  Here is a signal
// handler that does that.
void
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
// from a lat long pseudoprojected image requires us to do almost
// nothing.  Pseudoprojected images have units of degrees, so all we
// have to do is convert as appropriate.  But we still need these
// functions to use when we need to use a function pointer to perform
// a generic operation.
static int 
project_lat_long_pseudo (project_parameters_t *pps, double lat, double lon,
			 double height, double *x, double *y, double *z)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps;		

  *x = lon * R2D;
  *y = lat * R2D;
  *z = height;

  return TRUE;
}
static int
project_lat_long_pseudo_inv (project_parameters_t *pps, double x, double y,
			     double z, double *lat, double *lon,
			     double *height)
{
  /* Silence compiler warning about unused argument.  */
  pps = pps;

  *lat = y * D2R;
  *lon = x * D2R;
  *height = z;

  return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
//
// This comment block describes the general procedure for geocoding
// images stored in a SAR geometry.  For map projecting or
// reprojecting other things (notably DEMs) a seperate code thread
// using a different procedure is used.
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

///////////////////////////////////////////////////////////////////////////////
//
// Hold your nose...
//
// These used to be function scoped statics in reverse_map_x and
// reverse_map_y routines, but that broke horribly when this program
// got converted into a library function because they never got reset
// between calls, so now these are globals so that we can reset them
// all to their initial state when we get to the end of the function.
// The little '_rmx' and '_rmy' suffixes help remind us that these
// globals go with the reverse_map_x and reverse_map_y routines,
// repsectively.

  // True iff this is our first time through this routine.
  static gboolean first_time_through_rmx = TRUE;
  // Accelerators and interpolators for the all the vertical columns
  // of sample points.  Filled in first time through routine.
  static gsl_interp_accel **y_accel_rmx;
  static gsl_spline **y_spline_rmx;
  // Current accelerator and interpolator.  Updated when y argument is
  // different between calls.
  static gsl_interp_accel *crnt_accel_rmx;
  static gsl_spline *crnt_rmx;
  // Value of y for which current interpolator works.
  static double last_y_rmx;

  // True iff this is our first time through this routine.
  static gboolean first_time_through_rmy = TRUE;
  // Accelerators and interpolators for the all the vertical columns
  // of sample points.  Filled in first time through routine.
  static gsl_interp_accel **y_accel_rmy;
  static gsl_spline **y_spline_rmy;
  // Current accelerator and interpolator.  Updated when y argument is
  // different between calls.
  static gsl_interp_accel *crnt_accel_rmy;
  static gsl_spline *crnt_rmy;
  // Value of y for which current interpolator works.
  static double last_y_rmy;

///////////////////////////////////////////////////////////////////////////////

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
  // Convenience aliases.
  size_t sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *xpixs = dtf->sparse_x_pix;

  if ( G_UNLIKELY (first_time_through_rmx || y != last_y_rmx) ) {
    if ( !first_time_through_rmx ) {
      // Free the spline from the last line.
      gsl_interp_accel_free (crnt_accel_rmx);
      gsl_spline_free (crnt_rmx);
    } else {
      // Its our first time through, so set up the splines for the
      // grid point columns.
      y_accel_rmx = g_new (gsl_interp_accel *, sgs);
      y_spline_rmx = g_new (gsl_spline *, sgs);
      size_t ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
	gsl_vector *cypv = gsl_vector_alloc (sgs);
	gsl_vector *cxpixv = gsl_vector_alloc (sgs);
	size_t jj;
	for ( jj = 0 ; jj < sgs ; jj++ ) {
	  gsl_vector_set (cypv, jj, yprojs[jj * sgs + ii]);
	  gsl_vector_set (cxpixv, jj, xpixs[jj * sgs + ii]);
	}
	y_accel_rmx[ii] = gsl_interp_accel_alloc ();
	y_spline_rmx[ii] = gsl_spline_alloc (gsl_interp_cspline, sgs);
	gsl_spline_init (y_spline_rmx[ii], cypv->data, cxpixv->data, sgs);
	gsl_vector_free (cxpixv);
	gsl_vector_free (cypv);
      }
      first_time_through_rmx = FALSE;
    }
    // Set up the spline that runs horizontally, between the column
    // splines.
    crnt_accel_rmx = gsl_interp_accel_alloc ();
    crnt_rmx = gsl_spline_alloc (gsl_interp_cspline, sgs);
    double *crnt_points = g_new (double, sgs);
    size_t ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline_rmx[ii], y, y_accel_rmx[ii]);
    }
    gsl_spline_init (crnt_rmx, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y_rmx = y;
  }

  return gsl_spline_eval (crnt_rmx, x, crnt_accel_rmx);
}

// This routine is analagous to reverse_map_x, including the same
// caveats and confusing behavior.
static double
reverse_map_y (struct data_to_fit *dtf, double x, double y)
{
  size_t sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *ypixs = dtf->sparse_y_pix;

  if ( G_UNLIKELY (first_time_through_rmy || y != last_y_rmy) ) {
    if ( !first_time_through_rmy ) {
      // Free the spline from the last line.
      gsl_interp_accel_free (crnt_accel_rmy);
      gsl_spline_free (crnt_rmy);
    } else {
      // Its our first time through, so set up the splines for the
      // grid point columns.
      y_accel_rmy = g_new (gsl_interp_accel *, sgs);
      y_spline_rmy = g_new (gsl_spline *, sgs);
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
	y_accel_rmy[ii] = gsl_interp_accel_alloc ();
	y_spline_rmy[ii] = gsl_spline_alloc (gsl_interp_cspline, sgs);
	gsl_spline_init (y_spline_rmy[ii], cypv->data, cypixv->data, sgs);
	gsl_vector_free (cypixv);
	gsl_vector_free (cypv);
      }
      first_time_through_rmy = FALSE;
    }
    // Set up the spline that runs horizontally, between the column
    // splines.
    crnt_accel_rmy = gsl_interp_accel_alloc ();
    crnt_rmy = gsl_spline_alloc (gsl_interp_cspline, sgs);
    double *crnt_points = g_new (double, sgs);
    size_t ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline_rmy[ii], y, y_accel_rmy[ii]);
    }
    gsl_spline_init (crnt_rmy, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y_rmy = y;
  }

  return gsl_spline_eval (crnt_rmy, x, crnt_accel_rmy);
}


int asf_geocode_from_proj_file(const char *projection_file,
		 int force_flag, resample_method_t resample_method, 
		 double average_height, datum_type_t datum, double pixel_size,
		 char *in_base_name, char *out_base_name)
{
  project_parameters_t pp;
  projection_type_t projection_type;

  parse_proj_args_file(projection_file, &pp, &projection_type);

  return asf_geocode(&pp, projection_type, force_flag, resample_method,
		     average_height, datum, pixel_size, in_base_name,
		     out_base_name);
}

int asf_geocode (project_parameters_t *pp, projection_type_t projection_type, 
		 int force_flag, resample_method_t resample_method, 
		 double average_height, datum_type_t datum, double pixel_size,
		 char *in_base_name, char *out_base_name)
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
  int debug_dump = FALSE;

  // Asssign various filenames
  GString *input_image = g_string_new (in_base_name);
  GString *input_meta_data = g_string_new (input_image->str);
  g_string_append (input_meta_data, ".meta");
  GString *output_image = g_string_new (out_base_name);
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

  // We don't allow reprojection of a DEM (including from
  // pseudoprojected form) to change the underlying datum, since this
  // changes all the height values in a position-dependent way, making
  // it a bit of a pain to implement.  We have no immediate need for
  // this functionality.
  if ( (imd->sar->image_type == 'P' || imd->general->image_data_type == DEM)
       && imd->projection != NULL ) {
    // FIXME: revive this assertion or fix the underlying problem.
    asfRequire (datum == imd->projection->datum, "For input images of type "
    		"DEM, changing the datum is not supported.\n");
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
			double height, double *x, double *y, double *z);
  project_input = NULL;		// Silence compiler warnings.
  int (*unproject_input) (project_parameters_t *pps, double x, double y,
			  double z, double *lat, double *lon, double *height);
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
      break;
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

  // When working with an map projected input, it doesn't really
  // matter what spheroid is used for the lat/longs since they are
  // only intermediate values.  When working with pseudoprojected
  // images (e.g. USGS seamless stuff), the lat/longs are computed
  // directly from the pixel coordinates by the conversion function,
  // so what matters is that we set libasf_proj's notion of the
  // spheroid correctly (to whatever the pseudoprojected image uses).
  // Bleah.
  if (imd->projection != NULL 
      && imd->projection->type == LAT_LONG_PSEUDO_PROJECTION ) {
    project_set_input_spheroid (imd->projection->spheroid);
  }

  // Note that we can't get away with using a single datum for all
  // projection and unprojection calls, since the input and output
  // image might both be projected and might use different datums.
  // Therefore, we make a project_set_datum() call before each project
  // or unproject call.

  // Note that the average height isn't used at the moment, since for
  // SAR images there is no height information, and for DEMs we try to
  // do better and use the height values from the DEM for each pixel
  // we transform.
  project_set_avg_height (average_height);

  // Assign our transformation function pointers to point to the
  // appropriate functions.
  int (*project) (project_parameters_t *pps, double lat, double lon,
		  double height, double *x, double *y, double *z);
  int (*project_arr) (project_parameters_t *pps, double *lat, double *lon,
		      double *height, double **projected_x,
		      double **projected_y, double **projected_z, long length);
  int (*unproject) (project_parameters_t *pps, double x, double y, double z,
		    double *lat, double *lon, double *height);
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

  // Projecting or reprojecting DEMs with a potential datum change is
  // an intrinsicly three dimension operation, and is handled in its
  // own specialized function.
  if ( imd->general->image_data_type == DEM ) {
    return geocode_dem (projection_type, pp, datum, pixel_size,
			resample_method, input_image, imd, output_image);
  }
  else {
    // Do what we have always done (the whole remainder of this file
    // :) We don't maintain the indentation for this else, the
    // remainder of this routine should probably be made a function.

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
	project_set_datum (imd->projection->datum);
	return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]), NULL);
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
	project_set_datum (imd->projection->datum);
	return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]), NULL);
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
	project_set_datum (imd->projection->datum);
	return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]), NULL);
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
	project_set_datum (imd->projection->datum); 
	return_code = unproject_input (ipp, xpc, ypc, ASF_PROJ_NO_HEIGHT,
				       &(lats[current_edge_point]),
				       &(lons[current_edge_point]), NULL);
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
    project_set_datum (datum);
    return_code = project_arr (pp, lats, lons, NULL, &x, &y, NULL,
			       edge_point_count);
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
  // This grid size seems to work pretty well in general for our
  // products (good accuracy everywhere, decent speed).
  size_t grid_size = 131;
  // However, there isn't much point in using as many grid points as
  // we have pixels, so for small tiles, we set this to about 10
  // percent of larger image dimension in pixels.
  if ( ii_size_x / grid_size < 10 && ii_size_y / grid_size < 10 ) {
    grid_size = GSL_MAX (ii_size_x, ii_size_y) / 10;
    if ( grid_size % 2 != 1 ) {
      grid_size++;
    }
  }
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
  // Spacing between grid points, in output projection coordinates.
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
      project_set_datum (datum);
      return_code = unproject (pp, cxproj, cyproj, ASF_PROJ_NO_HEIGHT,
			       &lat, &lon, NULL);
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
	project_set_datum (imd->projection->datum);
	return_code = project_input (ipp, D2R * lat, D2R * lon,
				     ASF_PROJ_NO_HEIGHT, &ipcx, &ipcy, NULL);
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

  if (debug_dump) {
    asfPrintStatus("Dumping debug 8x8 projection information grid...\n");

    const char *grid_filename = "grid_dump.txt";
    FILE *g4r = FOPEN(grid_filename, "wt");
    double lat, lon, proj_x, proj_y, h;
    // double lat2, lon2;
    int iii, jjj;
    // double line2, samp2;

    h = average_height;
    int gridsz=8;
    for (jjj = 0; jjj <= gridsz; ++jjj) {
      for (iii = 0; iii <= gridsz; ++iii) {

	//double line1 = iii* ((double)ii_size_y)/((double)gridsz-1);
	//double samp1 = jjj* ((double)ii_size_x)/((double)gridsz-1);

	double samp1 = 1024*iii;
	double line1 = 1024*jjj;

	meta_get_latLon(imd, line1, samp1, h, &lat, &lon);
	project_set_datum (datum);
	project (pp, lat*D2R, lon*D2R, ASF_PROJ_NO_HEIGHT,
		 &proj_x, &proj_y, NULL);
	//unproject(pp, proj_x, proj_y, &lat2, &lon2);
	//lat2 *= R2D; lon2 *= R2D;
	//meta_get_lineSamp(imd, lat2, lon2, h, &samp2, &line2);

	double line3 = Y_PIXEL(proj_x, proj_y);
	double samp3 = X_PIXEL(proj_x, proj_y);

	double lat3,lon3;
	meta_get_latLon(imd, line3, samp3, h, &lat3, &lon3);
/*
	fprintf(g4r,
	      "%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,"
	      "%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,"
	      "%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,%.10lf,%.10lf\n",
	      line1, samp1, lat, lon, proj_x, proj_y, lat2, lon2, line2, samp2,
		fabs(line1-line2),fabs(samp1-samp2),line3,
		samp3,lat3,lon3,fabs(line1-line3),fabs(samp1-samp3),
		fabs(lat2-lat3),fabs(lon2-lon3));
*/

	fprintf(g4r, "%.10lf %.10lf %d %d\n",
		lat3, lon3, (int)samp1, (int)line1);
      }
    }

    fclose(g4r);

    const char *edge_filename = "edge_dump.txt";
    g4r = FOPEN(edge_filename, "wt");

    // dump the edges of the image
    int edge_point_count = 2 * (gridsz+1) + 2 * (gridsz+1) - 4;
    double *lats = g_new (double, edge_point_count);
    double *lons = g_new (double, edge_point_count);
    int *lines = g_new (int, edge_point_count);
    int *samps = g_new (int, edge_point_count);
    int current_edge_point = 0;
    double xsamp = 0, yline = 0;

    iii = 0;
    jjj = 0;

    for ( ; iii < gridsz; ++iii) {
      xsamp = ((double)iii* (double)ii_size_y - 1)/(double)(gridsz);
      yline = ((double)jjj* (double)ii_size_x - 1)/(double)(gridsz);
      lines[current_edge_point] = (int)(yline+.5);
      samps[current_edge_point] = (int)(xsamp+.5);
      printf("%d %d %d %d %g %g\n", iii, jjj, samps[current_edge_point],
	     lines[current_edge_point], xsamp, yline);
      meta_get_latLon (imd, 
		       (double)lines[current_edge_point],
		       (double)samps[current_edge_point],
		       average_height,
		       &(lats[current_edge_point]),
		       &(lons[current_edge_point]));
      current_edge_point++;
    }
    for ( ; jjj < gridsz ; ++jjj ) {
      xsamp = ((double)iii* (double)ii_size_y - 1)/(double)(gridsz);
      yline = ((double)jjj* (double)ii_size_x - 1)/(double)(gridsz);
      lines[current_edge_point] = (int)(yline+.5);
      samps[current_edge_point] = (int)(xsamp+.5);
      printf("%d %d %d %d %g %g\n", iii, jjj, samps[current_edge_point],
	     lines[current_edge_point], xsamp, yline);
      meta_get_latLon (imd, 
		       (double)lines[current_edge_point],
		       (double)samps[current_edge_point],
		       average_height,
		       &(lats[current_edge_point]),
		       &(lons[current_edge_point]));
      current_edge_point++;
    }
    for ( ; iii > 0 ; iii-- ) {
      xsamp = ((double)iii* (double)ii_size_y - 1)/(double)(gridsz);
      yline = ((double)jjj* (double)ii_size_x - 1)/(double)(gridsz);
      lines[current_edge_point] = (int)(yline+.5);
      samps[current_edge_point] = (int)(xsamp+.5);
      printf("%d %d %d %d %g %g\n", iii, jjj, samps[current_edge_point],
	     lines[current_edge_point], xsamp, yline);
      meta_get_latLon (imd, 
		       (double)lines[current_edge_point],
		       (double)samps[current_edge_point],
		       average_height,
		       &(lats[current_edge_point]),
		       &(lons[current_edge_point]));
      current_edge_point++;
    }
    for ( ; jjj > 0 ; jjj-- ) {
      xsamp = ((double)iii* (double)ii_size_y - 1)/(double)(gridsz);
      yline = ((double)jjj* (double)ii_size_x - 1)/(double)(gridsz);
      lines[current_edge_point] = (int)(yline+.5);
      samps[current_edge_point] = (int)(xsamp+.5);
      printf("%d %d %d %d %g %g\n", iii, jjj, samps[current_edge_point],
	     lines[current_edge_point], xsamp, yline);
      meta_get_latLon (imd, 
		       (double)lines[current_edge_point],
		       (double)samps[current_edge_point],
		       average_height,
		       &(lats[current_edge_point]),
		       &(lons[current_edge_point]));
      current_edge_point++;
    }
    g_assert (current_edge_point == edge_point_count);

    for (iii = 0; iii < edge_point_count; ++iii) {
      lats[iii] *= D2R;
      lons[iii] *= D2R;
    }

    // Pointers to arrays of projected coordinates to be filled in.
    // The projection function will allocate this memory itself.
    double *x = NULL, *y = NULL;
    x = y = NULL;
    // Project all the edge pixels.
    project_set_datum (datum);
    return_code = project_arr (pp, lats, lons, NULL, &x, &y, NULL,
			       edge_point_count);
    g_assert (return_code == TRUE);

    // Reverse map all the edge pixels
    for (iii = 0; iii < edge_point_count; ++iii) {
      meta_get_latLon(imd, Y_PIXEL(x[iii], y[iii]), X_PIXEL(x[iii], y[iii]),
		      average_height,
		      &(lats[iii]), &(lons[iii]));
    }

    for (iii = 0; iii < edge_point_count; ++iii) {
      fprintf(g4r, "%.10lf %.10lf %d %d\n",
	      lats[iii], lons[iii], samps[iii], lines[iii]);
    }
    fclose(g4r);

    g_free(lats);
    g_free(lons);
    g_free(samps);
    g_free(lines);

    asfPrintStatus("Done\n\n");
  }

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
      // printf ("DEBUG: symmetry testing latLong vs. linsSamp...\n");
      // const double sym_th = 0.1;   // Symmetry threshold.
      // g_assert (fabs (strx - stpx) < sym_th && fabs (stry - stpy) < sym_th);
      // Hmm, looke like they are all pretty bad.  Oh well, the
      // problem of large corner errors when none of the intermediate
      // grid points were off by much still seems specific to scansar.
    }

    double ul_x, ul_y;
    project_set_datum (datum);
    project (pp, D2R * ul_lat, D2R * ul_lon, ASF_PROJ_NO_HEIGHT,
	     &ul_x, &ul_y, NULL);
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
    project_set_datum (datum);
    project (pp, D2R * lr_lat, D2R * lr_lon, ASF_PROJ_NO_HEIGHT,
	     &lr_x, &lr_y, NULL);
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
      asfPrintStatus ("Lower right y corner error: %f\n", lr_y_corner_error);
    }
  }

  asfPrintStatus ("\n");

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

  // Done with the input metadata.
  meta_free (imd);

  // Now we need some metadata for the output image.  We will just
  // start with the metadata from the input image and add the
  // geocoding parameters.
  meta_parameters *omd = meta_read (input_meta_data->str);
  double x_pixel_size = omd->general->x_pixel_size;
  double y_pixel_size = omd->general->y_pixel_size;
  double x_scale = pixel_size / x_pixel_size;
  double y_scale = pixel_size / y_pixel_size;

  // Flip the non-reprojected image if the y pixel size is negative.
  if ( y_pixel_size < 0 && omd->projection == NULL ) {
    asfPrintStatus ("Negative y pixel size, flipping output image.\n");
    g_assert (0); 		/* Shouldn't be here.  */
    float_image_flip_y (oim);
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
    if (omd->projection->perY > 0) {
      g_assert (0);		/* Shouldn't happen.  */
      pc_per_y = (int) (omd->projection->perY / y_pixel_size + 0.5) * pixel_size;
    }
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

  /////////////////////////////////////////////////////////////////////////////
  //
  // Clear out all the persistent spline memory goop used by the
  // reverse_map_x and reverse_map_y routines (see comments near the
  // declarations of these variables).
  //

  size_t sgs = dtf.sparse_grid_size; // Convenience alias.

  first_time_through_rmx = TRUE;
  first_time_through_rmy = TRUE;
  for ( ii = 0 ; ii < sgs ; ii++ ) {
    gsl_interp_accel_free (y_accel_rmx[ii]);
    gsl_interp_accel_free (y_accel_rmy[ii]);
    gsl_spline_free (y_spline_rmx[ii]);
    gsl_spline_free (y_spline_rmy[ii]);
  }
  g_free (y_accel_rmx);
  g_free (y_accel_rmy);
  g_free (y_spline_rmx);
  g_free (y_spline_rmy);
  gsl_interp_accel_free (crnt_accel_rmx);
  gsl_interp_accel_free (crnt_accel_rmy);
  gsl_spline_free (crnt_rmx);
  gsl_spline_free (crnt_rmy);

  /////////////////////////////////////////////////////////////////////////////

  // Done with the data being modeled.
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

  return 0;
  }
}
