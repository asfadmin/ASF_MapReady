
#include <glib.h>
#include <gsl/gsl_statistics_double.h>

#include <asf_reporting.h>
#include <asf_raster.h>

#include "asf_import_dem.h"


// Check the health of the our spline model by comparing the input
// image pixel coordinates predicted by the model for each point
// with the known values.
void
check_splines (data_to_fit_t *dtf, const int grid_size)
{
  // This is a small image which will show a visual of the
  // distribution of errors in the output grid.
  FloatImage *error_map = float_image_new (grid_size, grid_size);

  gsl_vector *model_x_errors = gsl_vector_alloc (dtf->n);
  gsl_vector *model_y_errors = gsl_vector_alloc (dtf->n);
  gsl_vector *model_errors = gsl_vector_alloc (dtf->n);
  int ii=0;
  for ( ii = 0 ; ii < dtf->n ; ii++ ) {
    // x pixel index in input image as predicted by model.
    double xpfm = X_PIXEL (dtf->x_proj[ii], dtf->y_proj[ii]);
    double ypfm = Y_PIXEL (dtf->x_proj[ii], dtf->y_proj[ii]);
    double x_error = xpfm - dtf->x_pix[ii];
    double y_error = ypfm - dtf->y_pix[ii];
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
  // We want to choke if our worst point in the model is off by this
  // many pixels or more.
  double max_allowable_error = 1.0;
  if ( largest_error > max_allowable_error ) {
      asfPrintError("Largest Error was larger than maximum allowed! "
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

