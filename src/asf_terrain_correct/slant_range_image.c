// Implementation of the interface described in slant_range_image.h.

#include <stdio.h>

#include <glib.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_spline.h>

#include <asf_meta.h>
#include "slant_range_image.h"

SlantRangeImage *
slant_range_image_thaw (const char *file)
{
  // This method is untested.
  //g_assert_not_reached ();

  SlantRangeImage *self = g_new (SlantRangeImage, 1);

  FILE *sv = fopen (file, "r");   // Serialized version.
  g_assert (sv != NULL);

  // Read the title portion.
  int assignment_count 
    = fscanf (sv, "Serialized slant_range_image instance\n");
  g_assert (assignment_count == 0);

  // Read the metadata.  We don't really want to keep this since being
  // text we may not get back out a bit-for-bit version of what we put
  // in, but we read it as a simple way of getting through it.
  long int dummy_long_int;
  assignment_count = fscanf (sv, "range_direction_pixel_count: %ld\n",
			     &dummy_long_int);
  g_assert (assignment_count == 1);
  assignment_count = fscanf (sv, "time_direction_pixel_count: %ld\n",
			     &dummy_long_int);
  double dummy_double;
  assignment_count = fscanf (sv, "upper_left_pixel_time: %lf\n", 
			     &dummy_double);
  g_assert (assignment_count == 1);
  assignment_count = fscanf (sv, "upper_left_pixel_range: %lf\n",
			     &dummy_double);
  g_assert (assignment_count == 1);
  assignment_count = fscanf (sv, "time_per_pixel: %lf\n", &dummy_double);
  g_assert (assignment_count == 1);
  assignment_count = fscanf (sv, "slant_range_per_pixel: %lf\n", 
			     &dummy_double);
  g_assert (assignment_count == 1);

  // Read the little text that introduces the binary data part:
  assignment_count = fscanf (sv, "Binary data follows:\n");
  g_assert (assignment_count == 0);

  // Read the binary form of the above metadata.
  size_t size_x, size_y;
  size_t read_count = fread (&size_x, sizeof (size_t), 1, sv);
  g_assert (read_count == 1);
  g_assert (size_x < SSIZE_MAX);
  read_count = fread (&size_y, sizeof (size_t), 1, sv);
  g_assert (read_count == 1);
  g_assert (size_y < SSIZE_MAX);
  read_count = fread (&(self->upper_left_pixel_time), sizeof (double), 1, sv);
  g_assert (read_count == 1);
  read_count = fread (&(self->upper_left_pixel_range), sizeof (double), 1, sv);
  g_assert (read_count == 1);
  read_count = fread (&(self->time_per_pixel), sizeof (double), 1, sv);
  g_assert (read_count == 1);
  read_count = fread (&(self->slant_range_per_pixel), sizeof (double), 1, sv);
  g_assert (read_count == 1);

  // Read the binary floating point image.
  self->data = float_image_new_from_file_pointer 
    (size_x, size_y, sv, 0, FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

  // If everything worked right that should have been all the data in
  // the file, so make sure we have.
  int8_t dummy_byte;
  read_count = fread (&dummy_byte, sizeof (int8_t), 1, sv);
  g_assert (read_count == 0);
  g_assert (feof (sv));

  // Done with the serialized instance, close it.
  int return_code = fclose (sv);
  g_assert (return_code == 0);

  return self;
}

// Return true iff byte_order is not the native byte order on the
// current platform.
static gboolean
non_native_byte_order (float_image_byte_order_t byte_order)
{
  return ((G_BYTE_ORDER == G_LITTLE_ENDIAN
           && byte_order == FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN)
          || (G_BYTE_ORDER == G_BIG_ENDIAN
              && byte_order == FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN));
}

// Swap the byte order of a 32 bit value, hopefully converting from
// big endian to little endian or vice versa.  There is unfortunately
// some question whether or not this always works right for floating
// point values.
static void
swap_bytes_32 (unsigned char *in)
{
  g_assert (sizeof (unsigned char) == 1);
  int tmp = in[0];
  in[0] = in[3];
  in[3] = tmp;
  tmp = in[1];
  in[1] = in[2];
  in[2] = tmp;
}

void
slant_range_image_freeze (SlantRangeImage *self, const char *file)
{
  // This method is untested.
  //  g_assert_not_reached ();

  FILE *sv = fopen (file, "w");   // Serialized version.
  g_assert (sv != NULL);

  // Write the title portion.
  int print_count = fprintf (sv, "Serialized slant_range_image instance\n");
  g_assert (print_count > 0);

  // Write the metadata as text.
  g_assert (self->data->size_x < LONG_MAX);  // FIXME: is this right check?
  print_count = fprintf (sv, "range_direction_pixel_count: %ld\n",
			 (long int) self->data->size_x);
  g_assert (print_count > 0);
  g_assert (self->data->size_y < LONG_MAX);  // FIXME: is this right check?
  print_count = fprintf (sv, "time_direction_pixel_count: %ld\n",
			 (long int) self->data->size_y);
  g_assert (print_count > 0);
  print_count = fprintf (sv, "upper_left_pixel_time: %.16le\n",
			 self->upper_left_pixel_time);
  g_assert (print_count > 0);
  print_count = fprintf (sv, "upper_left_pixel_range: %.16le\n",
			 self->upper_left_pixel_range);
  g_assert (print_count > 0);
  print_count = fprintf (sv, "time_per_pixel: %.16le\n",
			 self->time_per_pixel);
  g_assert (print_count > 0);
  print_count = fprintf (sv, "slant_range_per_pixel: %.16le\n",
			self->slant_range_per_pixel);
  g_assert (print_count > 0);

  // Write the little text that introduces the binary data part:
  print_count
    = fprintf (sv, "Binary data follows:\n");
  g_assert (print_count > 0);

  // Write the metadata in binary form.
  size_t write_count = fwrite (&(self->data->size_x), sizeof (size_t), 1, sv);
  g_assert (write_count == 1);
  write_count = fwrite (&(self->data->size_y), sizeof (size_t), 1, sv);
  g_assert (write_count == 1);
  write_count = fwrite (&(self->upper_left_pixel_time), sizeof (double), 1, 
			sv);
  g_assert (write_count == 1);
  write_count = fwrite (&(self->upper_left_pixel_range), sizeof (double), 1, 
			sv);
  g_assert (write_count == 1);
  write_count = fwrite (&(self->time_per_pixel), sizeof (double), 1, sv);
  g_assert (write_count == 1);
  write_count = fwrite (&(self->slant_range_per_pixel), sizeof (double), 1, 
			sv);
  g_assert (write_count == 1);

  // Write the binary floating point image data.
  float *row_buffer = g_new (float, self->data->size_x);
  size_t ii;
  for ( ii = 0 ; ii < self->data->size_y ; ii++ ) {
    // Fetch an image row.
    float_image_get_row (self->data, ii, row_buffer);
    // Swap bytes if required.
    if ( non_native_byte_order (FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN) ) {
      size_t jj;
      for ( jj = 0 ; jj < self->data->size_x ; jj++ ) {
	swap_bytes_32 ((unsigned char *) &(row_buffer[jj]));
      }
    }
    // Write the data to the file.
    write_count = fwrite (row_buffer, sizeof (float), self->data->size_x, sv);
    g_assert (write_count == self->data->size_x);
  }
  g_free (row_buffer);

  // Done with the serialized instance, close it.
  int return_code = fclose (sv);
  g_assert (return_code == 0);
}

SlantRangeImage *
slant_range_image_new_empty (double upper_left_pixel_slant_range,
			     double upper_left_pixel_time, 
			     double slant_range_per_pixel,
			     double time_per_pixel,
			     ssize_t slant_range_pixels,
			     ssize_t time_pixels)
{
  // Convenience aliases.
  double ult = upper_left_pixel_time;
  double ulr = upper_left_pixel_slant_range;
  double tpp = time_per_pixel;
  double rpp = slant_range_per_pixel;
  double tpc = time_pixels;
  double rpc = slant_range_pixels;

  SlantRangeImage *self = g_new (SlantRangeImage, 1);

  self->upper_left_pixel_time = ult;
  self->upper_left_pixel_range = ulr;
  self->time_per_pixel = tpp;
  self->slant_range_per_pixel = rpp;
  self->data = float_image_new_with_value (rpc, tpc, 0.0);

  return self;
}

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

  // It might be nice to use cubic splines to interpolate instead of
  // plain linear interpolation, but some images have unatural
  // discontinuities (for example where there is a fringe of black
  // pixels set to zero) that can result in nasty big overshoots at
  // discontinuities unless we use linear interpolation.
  gsl_interp_accel *accel = gsl_interp_accel_alloc ();
  gsl_spline *spline = gsl_spline_alloc (gsl_interp_linear, sc);
  //gsl_spline *spline = gsl_spline_alloc (gsl_interp_cspline, sc);

  // The slant range to the first sample and last sample in the first
  // line, and the implied slant range sample spacing.
  double sr_fs = meta_get_slant (imd, 0, 0);
  double sr_ls = meta_get_slant (imd, 0, sc);
  double sr_spacing = (sr_ls - sr_fs) / sc;

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
    g_assert (gsl_fcmp (current_sr_fs, sr_fs, 1e-12) == 0);
    double current_sr_ls = meta_get_slant (imd, ii, sc);
    g_assert (gsl_fcmp (current_sr_ls, sr_ls, 1e-12) == 0);
    double current_sr_spacing = (sr_ls - sr_fs) / sc;
    if ( gsl_fcmp (current_sr_spacing, sr_spacing, 1e-12) == 0 ) {
      g_assert (gsl_fcmp (current_sr_spacing, sr_spacing, 1e-12) == 0);
    }

    for ( jj = 0 ; jj < sc ; jj++ ) {
      double sample_value 
	= gsl_spline_eval (spline, sr_fs + jj * sr_spacing, accel);
      float_image_set_pixel (self->data, jj, ii, sample_value);
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

  return self;
}

// Form a new instance which is a rectanglar slice taken out of an
// existend model.
SlantRangeImage *
slant_range_image_new_subimage (SlantRangeImage *model, ssize_t start_x,
				ssize_t width, ssize_t start_y, ssize_t height)
{
  SlantRangeImage *self = g_new (SlantRangeImage, 1);

  // Convenience variables.
  g_assert (model->data->size_x < SSIZE_MAX);
  ssize_t mw = model->data->size_x;
  g_assert (model->data->size_y < SSIZE_MAX);
  ssize_t mh = model->data->size_y;

  // Ensure that we have valid subimage indicies.
  g_assert (start_x >= 0);
  g_assert (width >= 1);
  g_assert (start_x + width < mw);
  g_assert (start_y >= 0);
  g_assert (height >= 1);
  g_assert (start_y + height < mh);

  // Compute the pixel to time/slant range relationships for the new
  // instance.
  self->upper_left_pixel_time 
    = model->upper_left_pixel_time + model->time_per_pixel * start_y;
  self->upper_left_pixel_range
    = model->upper_left_pixel_range + model->slant_range_per_pixel * start_x;
  self->time_per_pixel = model->time_per_pixel;
  self->slant_range_per_pixel = model->slant_range_per_pixel;
  
  // Copy the pixels of interest between instances.
  self->data = float_image_new (width, height);
  ssize_t ii;
  for ( ii = 0 ; ii < height ; ii++ ) {
    ssize_t jj;
    for ( jj = 0 ; jj < width ; jj++ ) {
      float_image_set_pixel (self->data, jj, ii,
			     float_image_get_pixel (model->data, start_x + jj,
						    start_y + ii));
    }
  }

  return self;
}

SlantRangeImage *
slant_range_image_new_from_model_scaled (SlantRangeImage *model, 
					 ssize_t scale_factor)
{
  g_assert (scale_factor > 0);
  //g_assert (scale_factor % 2 == 1);

  SlantRangeImage *self = g_new (SlantRangeImage, 1);

  self->upper_left_pixel_time = model->upper_left_pixel_time;
  self->upper_left_pixel_range = model->upper_left_pixel_range;
  self->time_per_pixel = model->time_per_pixel * scale_factor;
  self->slant_range_per_pixel = model->slant_range_per_pixel * scale_factor;
  self->data = float_image_new_from_model_scaled (model->data, scale_factor);
  
  return self;
}

// Return true iff arg is inside l1 and l2 by at least epsilon =
// relative_guard * fabs (l2 - l1), in other words, return true iff
// arg is in [l1 + epsilon, l2 - epsilon] if l2 >= l1, or arg is in
// [l2 + epsilon, l1 - epsilon] if l1 > l2.
static gboolean
is_in_range (double arg, double l1, double l2, double relative_guard)
{
  double epsilon = relative_guard * fabs (l2 - l1);

  if ( l2 >= l1 ) {
    return l1 + epsilon <= arg && arg <= l2 - epsilon;
  }
  else {
    return l2 + epsilon <= arg && arg <= l1 - epsilon;
  }
}

gboolean
slant_range_image_contains (SlantRangeImage *self, double range, double time,
			    double relative_guard)
{
  // Convenience aliases.
  double ulpr = self->upper_left_pixel_range;
  double ulpt = self->upper_left_pixel_time;
  double rpp = self->slant_range_per_pixel;
  double tpp = self->time_per_pixel;

  // Make sure the time/slant range we want to sample is in the image.
  return (is_in_range (range, ulpr, ulpr + rpp * (self->data->size_x - 1),
		       relative_guard)
	  && is_in_range (time, ulpt, ulpt + tpp * (self->data->size_y - 1), 
			  relative_guard));
}

void
slant_range_image_get_coords(SlantRangeImage *self, double range, double time,
			     double *x, double *y)
{
  *x = ((range - self->upper_left_pixel_range)
	      / self->slant_range_per_pixel);
  *y = ((time - self->upper_left_pixel_time) / self->time_per_pixel);
}

double
slant_range_image_sample (SlantRangeImage *self, double range, double time,
			  float_image_sample_method_t sample_method)
{
  // We insist that the requested (range, time) be within the extent
  // of the image by at least this fraction of the image size.
  const double relative_guard = 1e-6;
  g_assert (slant_range_image_contains (self, range, time, relative_guard));

  double x = ((range - self->upper_left_pixel_range)
	      / self->slant_range_per_pixel);
  double y = ((time - self->upper_left_pixel_time) / self->time_per_pixel);

  return float_image_sample (self->data, x, y, sample_method);
}

void
slant_range_image_add_energy (SlantRangeImage *self, double range, double time,
			      double energy)
{
  // We insist that the requested (range, time) be within the extent
  // of the image by at least this fraction of the image size.
  const double relative_guard = 1e-6;
  g_assert (slant_range_image_contains (self, range, time, relative_guard));

  double x = ((range - self->upper_left_pixel_range)
	      / self->slant_range_per_pixel);
  double y = ((time - self->upper_left_pixel_time) / self->time_per_pixel);

  // Fraction of added energy to put in the pixels above the point supplied.
  double uf = ceil (y) - y;
  
  // Fraction of added energy to put in the pixels left of the point supplied.
  double lf = ceil (x) - x;

  // Energy to be distributed to pixels above and below the given point.
  double energy_above = uf * energy;
  double energy_below = energy - energy_above;

  // Convenience aliases for surrounding pixel indicies.
  ssize_t lx = floor (x);
  ssize_t rx = lx + 1;
  ssize_t ay = floor (y);
  ssize_t by = ay + 1;

  // Add the energy to the neighboring pixels.
  float_image_set_pixel (self->data, lx, ay, 
			 float_image_get_pixel (self->data, lx, ay)
			 + energy_above * lf);
  float_image_set_pixel (self->data, rx, ay, 
			 float_image_get_pixel (self->data, rx, ay)
			 + energy_above * (1.0 - lf));
  float_image_set_pixel (self->data, lx, by, 
			 float_image_get_pixel (self->data, lx, by)
			 + energy_below * lf);
  float_image_set_pixel (self->data, rx, by, 
			 float_image_get_pixel (self->data, rx, by)
			 + energy_below * (1.0 - lf));
}

gboolean
slant_range_image_equal (SlantRangeImage *self, SlantRangeImage *other)
{
  // Compare the slant range specific metadata.
  if ( self->upper_left_pixel_time != other->upper_left_pixel_time ) {
    return FALSE;
  }
  if ( self->upper_left_pixel_range != other->upper_left_pixel_range ) {
    return FALSE;
  }
  if ( self->time_per_pixel != other->time_per_pixel ) {
    return FALSE;
  }
  if ( self->slant_range_per_pixel != other->slant_range_per_pixel ) {
    return FALSE;
  }

  // Compare the actual floating point images.
  if ( self->data->size_x != other->data->size_x ) {
    return FALSE;
  }
  if ( self->data->size_y != other->data->size_y ) {
    return FALSE;
  }
  size_t ii, jj;
  for ( ii = 0 ; ii < self->data->size_y ; ii++ ) {
    for ( jj = 0 ; jj < self->data->size_x ; jj++ ) {
      if ( float_image_get_pixel (self->data, jj, ii)
	   != float_image_get_pixel (other->data, jj, ii) ) {
	return FALSE;
      }
    }
  }

  return TRUE;
}

void
slant_range_image_free (SlantRangeImage *self)
{
  float_image_free (self->data);
  free (self);
}
