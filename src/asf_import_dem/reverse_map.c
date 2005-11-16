#include <glib.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_spline.h>

#include "asf_import_dem.h"

// Reverse map from projection coordinates x, y to input pixel
// coordinate X.  This function looks at the data to fit on the first
// time through, in order to set up vertical splines, after which this
// argument is mostly ignored.  But not entirely, so you can't free or
// change the dtf.  Mapping is efficient only if the y coordinates are
// usually identical between calls, since when y changes a new spline
// between splines has to be created.
double
reverse_map_x (data_to_fit_t *dtf, double x, double y)
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
  int sgs = dtf->sparse_grid_size;
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
      int ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
        gsl_vector *cypv = gsl_vector_alloc (sgs);
        gsl_vector *cxpixv = gsl_vector_alloc (sgs);
        int jj;
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
    int ii;
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
double
reverse_map_y (data_to_fit_t *dtf, double x, double y)
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

  int sgs = dtf->sparse_grid_size;
  double *xprojs = dtf->sparse_x_proj;
  double *yprojs = dtf->sparse_y_proj;
  double *ypixs = dtf->sparse_y_pix;

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
      int ii;
      for ( ii = 0 ; ii < sgs ; ii++ ) {
        // Current y projection value.
        gsl_vector *cypv = gsl_vector_alloc (sgs);
        // Current y pixel value.
        gsl_vector *cypixv = gsl_vector_alloc (sgs);
        int jj;
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
    int ii;
    for ( ii = 0 ; ii < sgs ; ii++ ) {
      crnt_points[ii] = gsl_spline_eval (y_spline[ii], y, y_accel[ii]);
    }
    gsl_spline_init (crnt, xprojs, crnt_points, sgs);
    g_free (crnt_points);
    last_y = y;
  }

  return gsl_spline_eval (crnt, x, crnt_accel);
}
