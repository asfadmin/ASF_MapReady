// A slant range synthetic aperature radar image.

#ifndef SLANT_RANGE_IMAGE_H
#define SLANT_RANGE_IMAGE_H

#include <float_image.h>

typedef struct {
  // Imaging time of upper left pixel.
  double upper_left_pixel_time;
  // Range of upper left pixel.
  double upper_left_pixel_range;
  // Time per pixel in -y direction.
  double time_per_pixel;
  // Slant range per pixel in x direction.
  double slant_range_per_pixel;
  FloatImage *data;
} SlantRangeImage;

// Instantiate a new living instance by "thawing out" the serialized
// version in file (which must have been created with the _freeze
// method oequivalent code).
SlantRangeImage *
slant_range_image_thaw (const char *file);

// Freeze a copy of the current image in file, for later recovery with
// the _thaw method.  The instance this method is called on is still
// fully usable after this method returns.
void
slant_range_image_freeze (SlantRangeImage *self, const char *file);

// Create a new instance from the Alaska Satellite Facility (ASF)
// internal format metadata_file and data_file.  The new instance will
// have the same dimensions as the image it is produced from, with the
// pixels resampled at evenly spaced slant range intervals.
SlantRangeImage *
slant_range_image_new_from_ground_range_image (char *metadata_file,
					       char *data_file);

SlantRangeImage *
slant_range_image_new_subimage (SlantRangeImage *model, ssize_t start_x,
				ssize_t width, ssize_t start_y, 
				ssize_t height);

// Return true iff self contains (slant_range, time) by at least
// relative_guard * (supported_range) in each dimension.  The
// relative_guard argument is intended to help with the problem of
// floating point arithmetic inaccuracy: a call may be made with a
// small relative_guard value in order to ensure that subsequent
// references at the same location will work (due to the vagaries of
// floating point arithmetic, its possible to have the exact same
// query succeed once then fail next time, since registers have less
// floating point precision than memory).
gboolean
slant_range_image_contains (SlantRangeImage *self, double range, double time,
			    double relative_guard);

// Sample the image at (slant_range, time) using sample_method.
// Requires slant_range_image_contains to be true.
double
slant_range_image_sample (SlantRangeImage *self, double range, double time, 
			  float_image_sample_method_t sample_method);

// Return true iff self is bit-for-bit identical to other.  This
// method uses exact comparison of the floating point slant range
// metadata values and the floating point data pixels, and so is only
// really useful for doing things like verifying that a _freeze/_thaw
// pair has behaved as expected.
gboolean
slant_range_image_equal (SlantRangeImage *self, SlantRangeImage *other);

// Free resources associated with self.
void
slant_range_image_free (SlantRangeImage *self);

#endif // #ifndef SLANT_RANGE_IMAGE_H
