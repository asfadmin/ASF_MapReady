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
#include <asf_meta.h>
#include <libasf_proj.h>
#include <asf_raster.h>

// Headers used by this program.
#include "geocode_options.h"

// Factors for going between degrees and radians.
#define RAD_TO_DEG	57.29577951308232
#define DEG_TO_RAD	0.0174532925199432958

///////////////////////////////////////////////////////////////////////////////
// 
// We want to find two 2D quadratics that that map points in the
// output space to x and y values of points in the input space.
// In equations, we want:
// 
//      X(x, y) = ax^2 + by^2 + cxy + dx + ey + g
//      Y(x, y) = hx^2 + iy^2 + jxy + kx + ly + m
//
// We need to find model coefficients a through m, excluding f.  Then
// we will be able to take projection coordinates x, y and find the
// corresponding pixel indicies (X, Y) in the input image.
//
// This is a simple case of the general method described in "Map
// Projections a Reference Manual", section 10.3, by Lev
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
// of the model parameters as arguments.  The mapping of quadratic
// coefficients for a quadratic ax^2 + by^2 + cxy + dx + ey + f to
// vector positions is shown below.  Note that all quadratic
// coefficient offsets are refered to with these offsets, including
// those for quadratinc Y(x, y) (for which different coefficients were
// shown in above comments).
static const size_t a_index = 0;
static const size_t b_index = 1;
static const size_t c_index = 2;
static const size_t d_index = 3;
static const size_t e_index = 4;
static const size_t g_index = 5;

// Evalutate quadratic ax^2 + by^2 + cxy + dx + ey + g at x, y, where
// a, b, c, d, e, and g are the first six elements of the coefficients
// vector.
static double
evaluate_quadratic (const gsl_vector *coefficients, double x, double y)
{
  double a = gsl_vector_get (coefficients, a_index);
  double b = gsl_vector_get (coefficients, b_index);
  double c = gsl_vector_get (coefficients, c_index);
  double d = gsl_vector_get (coefficients, d_index);
  double e = gsl_vector_get (coefficients, e_index);
  double g = gsl_vector_get (coefficients, g_index);
  
  return a * pow (x, 2.0) + b * pow (y, 2.0) + c * x * y + d * x + e * y + g;
}

// To get the best fit, we will perform least squares minimization on
// the difference between modeled coordinates and a batch of
// "measured" (actually transformed with libproj) ones.  This function
// is to be called by the minimization routines in the GSL.  The first
// parameter (named x) is a vector of the set of quadratic
// coefficients currently under consideration by the minimizer, and
// has nothing to do with an x coordinate.  The parameter names used
// were chosen for consistency with the GSL types and examples in the
// GSL documentation.
static int
fit_x_coordinates_f (const gsl_vector *x, void *params, gsl_vector *f)
{
  struct data_to_fit *dtfs = (struct data_to_fit *)params;
  size_t n = dtfs->n;
  double *x_proj = dtfs->x_proj;
  double *y_proj = dtfs->y_proj;
  double *x_pix = dtfs->x_pix;

  size_t i;
  for ( i = 0 ; i < n ; i++ ) {
    double x_pix_modeled = evaluate_quadratic (x, x_proj[i], y_proj[i]);
    gsl_vector_set (f, i, x_pix_modeled - x_pix[i]);
  }

  return GSL_SUCCESS;
}

// Function to minimize to determine coefficients for the Y coordinate model.
static int
fit_y_coordinates_f (const gsl_vector *x, void *params, gsl_vector *f)
{
  struct data_to_fit *dtfs = (struct data_to_fit *)params;
  size_t n = dtfs->n;
  double *x_proj = dtfs->x_proj;
  double *y_proj = dtfs->y_proj;
  double *y_pix = dtfs->y_pix;

  size_t i;
  for ( i = 0 ; i < n ; i++ ) {
    double y_pix_modeled = evaluate_quadratic (x, x_proj[i], y_proj[i]);
    gsl_vector_set (f, i, y_pix_modeled - y_pix[i]);
  }

  return GSL_SUCCESS;
}

// We also need routines to compute the jacobian matrices of the
// fitting functions with respect to the quadratic coefficients.  The
// Jacobians of the X and Y approxomating functions are the same, so
// we don't need seperate jocobian computers for the X and Y
// quadratics.
static int 
fit_coordinates_df (const gsl_vector *x, void *params, gsl_matrix *J)
{
  // Reassure compiler that we know we don't use x.
  x = x;		

  struct data_to_fit *dtfs = (struct data_to_fit *)params;
  size_t n = dtfs->n;
  double *x_proj = dtfs->x_proj;
  double *y_proj = dtfs->y_proj;

  size_t i;
  for ( i = 0 ; i < n ; i++ ) {
    gsl_matrix_set (J, i, a_index, pow (x_proj[i], 2.0));
    gsl_matrix_set (J, i, b_index, pow (y_proj[i], 2.0));
    gsl_matrix_set (J, i, c_index, x_proj[i] * y_proj[i]);
    gsl_matrix_set (J, i, d_index, x_proj[i]);
    gsl_matrix_set (J, i, e_index, y_proj[i]);
    gsl_matrix_set (J, i, g_index, 1.0);
  }

  return GSL_SUCCESS;
}

// For reasons I don't quite understand, it looks like the GSL makes
// us fill in a function-and-jacobian routine even if we haven't done
// any optimizations to simultaneoiusly compute the minimization
// function and the jacobian.  So here we have those routines.
static int
fit_x_coordinates_fdf (const gsl_vector *x, void *params, gsl_vector *f, 
		       gsl_matrix *J)
{
  fit_x_coordinates_f (x, params, f);
  fit_coordinates_df (x, params, J);

  return GSL_SUCCESS;
}

// This routine is analogous to fit_x_coordinates_fdf.
static int
fit_y_coordinates_fdf (const gsl_vector *x, void *params, gsl_vector *f, 
		       gsl_matrix *J)
{
  fit_y_coordinates_f (x, params, f);
  fit_coordinates_df (x, params, J);

  return GSL_SUCCESS;
}

// Print out information about the state of the solver.
static int
solver_print_state (gsl_multifit_fdfsolver *s, size_t iter)
{
  printf ("iteration: %3u Coefficients: %.3e %.3e %.3e %.3e %.3e %.3e "
	  "|Minimization Criteria| = %g\n",
	  iter,
	  gsl_vector_get (s->x, a_index),
	  gsl_vector_get (s->x, b_index),
	  gsl_vector_get (s->x, c_index),
	  gsl_vector_get (s->x, d_index),
	  gsl_vector_get (s->x, e_index),
	  gsl_vector_get (s->x, g_index),
	  gsl_blas_dnrm2 (s->f));

  return GSL_SUCCESS;
}

// Main routine.
int
main (int argc, char **argv)
{
  // Get the projection parameters from the command line.
  projection_type_t projection_type;
  project_parameters_t *pp = get_geocode_options(&argc, &argv, 
						 &projection_type);

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

  g_assert (argc == 3);
  GString *input_image = g_string_new (argv[1]);
  GString *output_image = g_string_new (argv[2]);
  // Input metadata.
  meta_parameters *imd = meta_read (input_image->str);
  // x and y dimensions of input image.
  size_t ixdim = imd->general->line_count;
  size_t iydim = imd->general->sample_count;

  // The latitude and longitude of the center of the image.
  double lat_0, lon_0;
  meta_get_latLon (imd, ixdim / 2.0, iydim / 2.0, 0.0, &lat_0, &lon_0);

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
    size_t edge_point_count = 2 * ixdim + 2 * iydim - 4;
    double *lats = g_new (double, edge_point_count);
    double *lons = g_new (double, edge_point_count);
    size_t current_edge_point = 0;
    size_t ii = 0, jj = 0;
    for ( ; ii < ixdim - 1 ; ii++ ) {
      meta_get_latLon (imd, (double)ii, (double)jj, 0.0, 
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    for ( ; jj < iydim - 1 ; jj++ ) {
      meta_get_latLon (imd, (double)ii, (double)jj, 0.0,
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    for ( ; ii > 0 ; ii-- ) {
      meta_get_latLon (imd, (double)ii, (double)jj, 0.0, 
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    for ( ; jj > 0 ; jj-- ) {
      meta_get_latLon (imd, (double)ii, (double)jj, 0.0, 
		       &(lats[current_edge_point]), 
		       &(lons[current_edge_point]));
      lats[current_edge_point] *= DEG_TO_RAD;
      lons[current_edge_point] *= DEG_TO_RAD;
      current_edge_point++;
    }
    g_assert (current_edge_point == edge_point_count);
    // Pointers to arrays of projected coordinates to be filled in.
    double *x, *y;
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
	  "subsetof input image pixels...\n");
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
  double x_pix_stride = ixdim / (grid_size - 1);
  double y_pix_stride = iydim / (grid_size - 1);
  // Index into the flattened list of mappings we want to produce.
  int current_mapping = 0;	
  size_t ii;
  for ( ii = 0 ; ii < grid_size ; ii++ ) {
    size_t jj;
    for ( jj = 0 ; jj < grid_size ; jj++ ) {
      // Input image x and y pixel coordinates.
      size_t xpc = floor (jj * x_pix_stride);
      g_assert (xpc < ixdim);
      size_t ypc = floor (ii * y_pix_stride);
      g_assert (ypc < iydim);
      // Corresponding latitude and longitude.
      double lat, lon;
      meta_get_latLon (imd, (double)xpc, (double)ypc, 0.0, &lat, &lon);
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

  // Here we find out quadratic models for input pixel coorinates
  // x_pix, y_pix in term of projection coordinates x, y.  For easy
  // reference, we put some variables in the terms used in the GSL
  // documentation.
  printf ("Trying to fit input image x pixel indicies to 2-D quadratic\n"
	  "function of output image pixel projection coordinates using\n"
	  "nonlinear least-squares fitting...\n");
  const size_t n = mapping_count;
  const size_t p = 6;		// 2D quadratics have six parameters.
  gsl_matrix *covariance = gsl_matrix_alloc (p, p);
  // We don't have a good initial guess at the moment.
  double x_init[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  gsl_vector_view x = gsl_vector_view_array (x_init, p);
  const gsl_multifit_fdfsolver_type *T = gsl_multifit_fdfsolver_lmsder;
  gsl_multifit_fdfsolver *s = gsl_multifit_fdfsolver_alloc (T, n, p);
  gsl_multifit_function_fdf x_f;
  x_f.f = fit_x_coordinates_f;
  x_f.df = fit_coordinates_df;
  x_f.fdf = fit_x_coordinates_fdf;
  x_f.n = n;
  x_f.p = p;
  x_f.params = &dtf;
  gsl_multifit_function_fdf y_f;
  y_f.f = fit_y_coordinates_f;
  y_f.df = fit_coordinates_df;
  y_f.fdf = fit_y_coordinates_fdf;
  y_f.n = n;
  y_f.p = p;
  y_f.params = &dtf;
  int status;			// Status of the fit.
  size_t iter = 0;		// Current iteration of the fit.
  // Find the X model of interest.
  gsl_multifit_fdfsolver_set (s, &x_f, &x.vector);
  solver_print_state (s, iter);
  // If we haven't converged after this many iterations, we give up.
  size_t maximum_iterations = 500;
  do {
    iter++;
    status = gsl_multifit_fdfsolver_iterate (s);
    printf ("iteration status = %s\n", gsl_strerror (status));
    solver_print_state (s, iter);
    if ( status != GSL_SUCCESS ) {
      break;
    }
    // There's nothing particularly insightful about these termination
    // conditions, but they seem to work...
    status = gsl_multifit_test_delta (s->dx, s->x, 1e-4, 1e-4);
  } while ( status == GSL_CONTINUE && iter < maximum_iterations );
  gsl_multifit_covar (s->J, 0.0, covariance);
	  
#define FIT(i) gsl_vector_get (s->x, i)
#define ERR(i) sqrt (gsl_matrix_get (covariance, i, i))

  printf ("Quadratic coefficients: ax^2 + by^2 + cxy + dx + ey + g: \n");
  printf ("a = %12.5e +/-%12.5e\n", FIT (a_index), ERR (a_index));
  printf ("b = %12.5e +/-%12.5e\n", FIT (b_index), ERR (b_index));
  printf ("c = %12.5e +/-%12.5e\n", FIT (c_index), ERR (c_index));
  printf ("d = %12.5e +/-%12.5e\n", FIT (d_index), ERR (d_index));
  printf ("e = %12.5e +/-%12.5e\n", FIT (e_index), ERR (e_index));
  printf ("g = %12.5e +/-%12.5e\n", FIT (g_index), ERR (g_index));
  
  // Check the health of the fit.
  {
    double mean_error 
      = gsl_stats_mean (s->f->data, s->f->stride, s->f->size);
    double error_standard_deviation 
      = gsl_stats_sd_m (s->f->data, s->f->stride, s->f->size, mean_error);
    double largest_error = gsl_vector_max (s->f);
    // We want to choke if it our worst point in the quadratic model
    // is off by this many pixels or more.
    double max_allowable_error = 3.0;
    g_assert (largest_error < max_allowable_error);
    printf ("For the differences between quadratic model values and projected "
	    "values:\n");
    printf ("Mean: %g\n", mean_error);
    printf ("Standard deviation: %g\n", error_standard_deviation); 
    printf ("Maximum (Worst observed error in x pixel index): %g\n", 
	    largest_error);
  }

  // Save our results for the x pixel quadratic model.
  gsl_vector *x_pix_model_coefficients = gsl_vector_alloc (s->x->size);
  gsl_vector_memcpy (x_pix_model_coefficients, s->x);

  printf ("\n");

  // Find the Y model of interest.
  printf ("Trying to fit input image y pixel indicies to 2-D quadratic\n"
	  "function of output image pixel projection coordinates using\n"
	  "nonlinear least-squares fitting...\n");
  // Out of caution, we free and reallocat some stuff.
  gsl_matrix_free (covariance);
  covariance = gsl_matrix_alloc (p, p);
  gsl_multifit_fdfsolver_free (s);
  s = gsl_multifit_fdfsolver_alloc (T, n, p);
  gsl_vector_set_zero (&x.vector); // Set initial guess back to zero.
  gsl_multifit_fdfsolver_set (s, &y_f, &x.vector);
  solver_print_state (s, iter);
  do {
    iter++;
    status = gsl_multifit_fdfsolver_iterate (s);
    printf ("iteration status = %s\n", gsl_strerror (status));
    solver_print_state (s, iter);
    if ( status != GSL_SUCCESS ) {
      break;
    }
    // There's nothing particularly insightful about these termination
    // conditions, but they seem to work...
    status = gsl_multifit_test_delta (s->dx, s->x, 1e-4, 1e-4);
  } while ( status == GSL_CONTINUE && iter < 500 );
  gsl_multifit_covar (s->J, 0.0, covariance);

  printf ("Quadratic coefficients: ax^2 + by^2 + cxy + dx + ey + g: \n");
  printf ("a = %12.5e +/-%12.5e\n", FIT (a_index), ERR (a_index));
  printf ("b = %12.5e +/-%12.5e\n", FIT (b_index), ERR (b_index));
  printf ("c = %12.5e +/-%12.5e\n", FIT (c_index), ERR (c_index));
  printf ("d = %12.5e +/-%12.5e\n", FIT (d_index), ERR (d_index));
  printf ("e = %12.5e +/-%12.5e\n", FIT (e_index), ERR (e_index));
  printf ("g = %12.5e +/-%12.5e\n", FIT (g_index), ERR (g_index));
  
  // Check the health of the fit.
  {
    double mean_error 
      = gsl_stats_mean (s->f->data, s->f->stride, s->f->size);
    double error_standard_deviation 
      = gsl_stats_sd_m (s->f->data, s->f->stride, s->f->size, mean_error);
    double largest_error = gsl_vector_max (s->f);
    // We want to choke if it our worst point in the quadratic model
    // is off by this many pixels or more.
    double max_allowable_error = 3.0;
    g_assert (largest_error < max_allowable_error);
    printf ("For the differences between quadratic model values and projected "
	    "values:\n");
    printf ("Mean: %g\n", mean_error);
    printf ("Standard deviation: %g\n", error_standard_deviation); 
    printf ("Maximum (worst observed error in y pixel index): %g\n", 
	    largest_error);
  }

  // Save our results for the y pixel quadratic model.
  gsl_vector *y_pix_model_coefficients = gsl_vector_alloc (s->x->size);
  gsl_vector_memcpy (y_pix_model_coefficients, s->x);

  gsl_multifit_fdfsolver_free (s);
  gsl_matrix_free (covariance);

  // Check correctness of reverse mappings of some corners.
  // We insist on the quadratic model being within this many pixels
  // for reverse transformations of the projection coordinates of the
  // corners of the output image back to the pixel indicies in the
  // input image.
  double max_corner_error = 3.0;
  // Upper left corner.
  double ul_lat, ul_lon;
  meta_get_latLon (imd, (float)0, (float)0, 0.0, &ul_lat, &ul_lon);
  double ul_x, ul_y;
  project (pp, DEG_TO_RAD * ul_lat, DEG_TO_RAD * ul_lon, &ul_x, &ul_y);
  double ul_x_pix_approx = evaluate_quadratic (x_pix_model_coefficients, 
					       ul_x - x_proj_mean, 
					       ul_y - y_proj_mean);
  g_assert (fabs (ul_x_pix_approx) < max_corner_error);
  double ul_y_pix_approx = evaluate_quadratic (y_pix_model_coefficients, 
					       ul_x - x_proj_mean, 
					       ul_y - y_proj_mean);
  g_assert (fabs (ul_y_pix_approx) < max_corner_error);
  // Lower right corner.
  double lr_lat, lr_lon;
  meta_get_latLon (imd, (float)(ixdim - 1), (float)(iydim - 1), 0.0, &lr_lat, 
		   &lr_lon);
  double lr_x, lr_y;
  project (pp, DEG_TO_RAD * lr_lat, DEG_TO_RAD * lr_lon, &lr_x, &lr_y);
  double lr_x_pix_approx = evaluate_quadratic (x_pix_model_coefficients, 
					       lr_x - x_proj_mean, 
					       lr_y - y_proj_mean);
  g_assert (fabs (lr_x_pix_approx - ixdim - 1) < max_corner_error);
  double lr_y_pix_approx = evaluate_quadratic (y_pix_model_coefficients, 
					       lr_x - x_proj_mean, 
					       lr_y - y_proj_mean);
  g_assert (fabs (lr_y_pix_approx - iydim - 1) < max_corner_error);

  // Ok, we now have quadratic functions we are happy with.  Make some
  // convenience macros for using them.
#define X_PIXEL(x, y) evaluate_quadratic (x_pix_model_coefficients, \
					  x - x_proj_mean, y - y_proj_mean)
#define Y_PIXEL(x, y) evaluate_quadratic (y_pix_model_coefficients, \
					  x - x_proj_mean, y - y_proj_mean)
  printf ("\n");

  // Now we are ready to produce our output image.  
  printf ("Resampling input image into output image coordinate space...\n");

  // Maximum output image pixel indicies.
  size_t oix_max = ixdim - 1, oiy_max = iydim - 1;
  // Projection coordinates per pixel in output image.
  double pc_per_x = (max_x - min_x) / oix_max;
  double pc_per_y = (max_y - min_y) / oiy_max;
  // Input image as matrix, top left is at 0, 0.
  gsl_matrix_float *iim = gsl_matrix_float_alloc (iydim, ixdim);  
  // FIXME: BAD BAD BOY MACHINE DEPENDENT FLOATING POINT READ AFTER
  // ALL OUR HARD WORK TO AVOID THIS KIND OF SCHLOP.
  GString *input_data_file = g_string_new (input_image->str);
  g_string_append (input_data_file, ".img");
  FILE *iim_stream = fopen (input_data_file->str, "r");
  g_assert (iim_stream != NULL);
  int return_code = gsl_matrix_float_fread (iim_stream, iim);
  g_assert (return_code == GSL_SUCCESS);
  return_code = fclose (iim_stream);
  g_assert (return_code == 0);
  g_string_free (input_data_file, TRUE);
  // Output image a matrix, top left is at 0, 0.
  gsl_matrix_float *oim = gsl_matrix_float_alloc (oix_max + 1, oiy_max + 1);

  // Convenience macros for getting and setting pixels.
#define GET_PIXEL(x, y) gsl_matrix_float_get (iim, x, y)
#define SET_PIXEL(x, y, value) gsl_matrix_float_set (oim, x, y, value);

  // Set the pixels of the output image.
  size_t oix, oiy;		// Output image pixel indicies.
  for ( oiy = 0 ; oiy <= oiy_max ; oiy++ ) {
    for ( oix = 0 ; oix <= oix_max ; oix++ ) {
      // Projection coordinates for the center of this pixel.    
      double oix_pc = ((double)oix / oix_max) * (max_x - min_x) + min_x;
      double oiy_pc = ((double)oiy / oiy_max) * (max_y - min_y) + min_y;
      // Determine pixel of interest in input image which we will use
      // to perform nearest neighbot sampling.
      ssize_t input_x_pixel = round (X_PIXEL (oix_pc, oiy_pc));
      ssize_t input_y_pixel = round (Y_PIXEL (oix_pc, oiy_pc));
      // If we are outside the extent of the input image, set to the
      // fill value.  FIXME: user should be able to specify fill value?
      const float fill_value = 0.0;
      g_assert (ixdim <= SSIZE_MAX);
      g_assert (iydim <= SSIZE_MAX);
      if ( input_x_pixel < 0 || input_x_pixel >= (ssize_t)ixdim
	   || input_y_pixel < 0 || input_y_pixel >= (ssize_t)iydim ) {
	SET_PIXEL (oix, oiy, (float)fill_value);
      }
      // Otherwise, set to the value from the appropriate position in
      // the input image.
      else {
	// If the GSL is inserting extra padding into our matricies,
	// we are in trouble, and will need to do something more
	// complicated.
	g_assert (iim->tda == iim->size2);
	SET_PIXEL (oix, oiy, interpolate (NEAREST, iim->data, iim->size1,
					  iim->size2, input_x_pixel,
					  input_y_pixel, NO_WEIGHT, 8));
      }
    }
    if ( oiy % 100 == 0 || oiy == oiy_max ) {
      printf ("Finished output image line %d\n", oiy);
    }
  }

  // FIXME: machine independent write.  Use our machine neutralized
  // interfaces instead.
  GString *output_data_file = g_string_new (output_image->str);
  g_string_append (output_data_file, ".img");
  FILE *oim_stream = fopen (output_data_file->str, "w");
  g_assert (oim_stream != NULL);
  return_code = gsl_matrix_float_fwrite (oim_stream, oim);
  return_code = fclose (oim_stream);
  g_assert (return_code == 0);
  g_assert (return_code == GSL_SUCCESS);
  g_string_free (output_data_file, TRUE);

  // Now we need some metadata for the output image.  We will just
  // start with the metadata from the input image and add the
  // geocoding parameters.
  meta_parameters *omd = meta_read (input_image->str);
  omd->sar->image_type = 'P';
  g_assert (omd->projection == NULL);
  omd->projection = g_new0 (meta_projection, 1);
  omd->projection->type = projection_type;
  omd->projection->startX = min_x;
  omd->projection->startY = min_y;
  omd->projection->perX = pc_per_x;
  omd->projection->perY = pc_per_y;
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
  omd->projection->param = *pp;
  meta_write (omd, output_image->str);

  // FIXME: Free stuff and close files and such in case this ever
  // becomes a library function.
  exit (EXIT_SUCCESS);
}
