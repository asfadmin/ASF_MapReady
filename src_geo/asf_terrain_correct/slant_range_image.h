// A slant range synthetic aperature radar image.

#ifndef SLANT_RANGE_IMAGE_H
#define SLANT_RANGE_IMAGE_H

#include <float_image.h>

typedef struct {
  // Imaging time of upper left pixel.
  double upper_left_pixel_time;
  // Range of upper left pixel.
  double upper_left_pixel_range;
  // Time per pixel in -y direction
  double time_per_pixel;
  // Slant range per pixel in x direction.
  double slant_range_per_pixel;
  FloatImage *data;
} SlantRangeImage;

// Create a new instance from the Alaska Satellite Facility (ASF)
// internal format metadata_file and data_file.  The new instance will
// have the same dimensions as the image it is produced from, with the
// pixels resampled at evenly spaced slant range intervals.
SlantRangeImage *
slant_range_image_new_from_ground_range_image (char *metadata_file,
					       char *data_file);

// Sample the image at (slant_range, time) using sample_method.
double
slant_range_image_sample (SlantRangeImage *self, double slant_range, 
			  double time, 
			  float_image_sample_method_t sample_method);

// Free resources associated with self.
void
slant_range_image_free (SlantRangeImage *self);

#endif // #ifndef SLANT_RANGE_IMAGE_H
