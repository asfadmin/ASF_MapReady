// Implementation of the interface described in slant_range_image.h.

#include <stdio.h>

#include <glib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>

#include <asf_meta.h>
#include "slant_range_image.h"

SlantRangeImage *
slant_range_image_new_from_ground_range_image (char *metadata_file,
					       char *data_file)
{
  SlantRangeImage *self = g_new (SlantRangeImage, 1);

  meta_parameters *imd = meta_read (metadata_file);

  // Convenience aliases.
  size_t sc = imd->general->sample_count, lc = imd->general->line_count;

  self->data = float_image_new (sc, lc);

  FloatImage *gri 
    = float_image_new_from_file (sc, lc, data_file, 0,
				 FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  // We will use a cubic spline along each line of samples to perform
  // the resampling into slant range.
  gsl_interp_accel *accel = gsl_interp_accel_alloc ();
  gsl_spline *spline = gsl_spline_alloc (gsl_interp_cspline, sc);

  // The slant range to the first sample and last sample in the first
  // line, and the implied slant range sample spacing.
  double sr_fs = meta_get_slant (imd, 0, 0);
  double sr_ls = meta_get_slant (imd, 0, sc - 1);
  double sr_spacing = (sr_ls - sr_fs) / (sc - 1);

  size_t ii;
  // Control point x values for a line.
  double *cp_xs = g_new (double, sc);
  // Control point y values for a line.
  double *cp_ys = g_new (double, sc);
  for ( ii = 0 ; ii < lc ; ii++ ) {
    
    size_t jj;
    for ( jj = 0 ; jj < sc ; jj++ ) {
      cp_xs[jj] = meta_get_slant (imd, ii, jj);
      cp_ys[jj] = float_image_get_pixel (gri, jj, ii);
    }

    gsl_spline_init (spline, cp_xs, cp_ys, sc);

    // Confirm that the slant range spacing for this line is the same
    // as the one for line zero, which we are considering the basis
    // for the whole image.
    double current_sr_fs = meta_get_slant (imd, ii, 0);
    g_assert (gsl_fcmp (current_sr_fs, sr_fs, 1e-16) == 0);
    double current_sr_ls = meta_get_slant (imd, ii, sc - 1);
    g_assert (gsl_fcmp (current_sr_ls, sr_ls, 1e-16) == 0);
    double current_sr_spacing = (sr_ls - sr_fs) / (sc - 1);
    g_assert (gsl_fcmp (current_sr_spacing, sr_spacing, 1e-16) == 0);    

    for ( jj = 0 ; jj < sc ; jj++ ) {
      double sample_value 
	= gsl_spline_eval (spline, sr_fs + jj * sr_spacing, accel);
      float_image_set_pixel (self->data, jj, ii, sample_value);
    }

    // FIXME: remove this development metering.
    if ( ii % 100 == 0 ) {
      g_assert (sizeof (long long int) >= sizeof (size_t));
      printf ("Done with line %lld\n", (long long int) ii);
    }

  }

  g_free (cp_ys);
  g_free (cp_xs);
  gsl_spline_free (spline);
  gsl_interp_accel_free (accel);
  float_image_free (gri);

  self->upper_left_pixel_time = meta_get_time (imd, 0, 0);
  self->time_per_pixel = imd->sar->azimuth_time_per_pixel;
  self->upper_left_pixel_range = sr_fs;
  self->slant_range_per_pixel = sr_spacing;

  meta_free (imd);

  // FIXME: remove this debugging dumpage.
  int return_code 
    = float_image_export_as_jpeg (self->data, "test_file.jpg",
				  GSL_MAX (self->data->size_x / 6, 
					   self->data->size_y / 6));
  g_assert (return_code == 0);

  return self;
}

double
slant_range_image_sample (SlantRangeImage *self, double time, double range,
			  float_image_sample_method_t sample_method)
{
  double x = ((range - self->upper_left_pixel_range)
	      / self->slant_range_per_pixel);
  double y = ((time - self->upper_left_pixel_time) / self->time_per_pixel);

  return float_image_sample (self->data, x, y, sample_method);
}

void
slant_range_image_free (SlantRangeImage *self)
{
  float_image_free (self->data);
  free (self);
}
