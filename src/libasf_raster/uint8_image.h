// A rectangular block of data of type uint8_t.  A transparent block
// cache is used for each instance of this class if necessary, so the
// image can be much bigger than will fit in memory, and pixels can
// still be accessed and written efficiently, provided subsequent
// accesses are spatially correlated.  A variety of useful methods are
// implemented (filtering, subsetting, interpolating, etc.)
//
// Don't try to access the same instance concurrently.  Split your
// images up into separate instances if you must parallelize things.
//
// For many methods, arguments of type ssize_t are used, but are not
// allowed to be negative.  This is to help prevent people from
// shooting themselves in the foor by accidently passing negative
// values which don't yield warnings and can't be caught with
// assertions.


#ifndef UINT8_IMAGE_H
#define UINT8_IMAGE_H

#ifndef solaris
#  include <stdint.h>
#endif
#include "asf_meta.h"
#include <stdio.h>
#include <sys/types.h>

#include <glib.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_histogram.h>

// sometimes we don't have these - choose conservative values
#ifndef SSIZE_MAX
#define SSIZE_MAX 32767
#endif

#ifndef UINT8_MAX
#define UINT8_MAX 255
#endif

// Instance structure.  Everything here is private and need not be
// used or understood by client code, except for the size_x and size_y
// fields.
typedef struct {
  size_t size_x, size_y;	// Image dimensions.
  size_t cache_space;		// Memory cache space in bytes.
  size_t cache_area;		// Memory cache area in pixels.
  size_t tile_size;		// Tile size in pixels on a side.
  size_t cache_size_in_tiles;	// Number of tiles in cache.
  size_t tile_count_x;		// Number of tiles in image in x direction.
  size_t tile_count_y;		// Number of tiles in image in y direction.
  size_t tile_count;		// Total number of tiles in image.
  size_t tile_area;             // Area of a tile, in pixels.
  uint8_t *cache;		// Memory cache.
  uint8_t **tile_addresses;	// Addresss of individual tiles in the cache.
  GQueue *tile_queue;		// Queue of tile offsets kept in load order.
  FILE *tile_file;              // File with tiles stored contiguously.
} UInt8Image;

///////////////////////////////////////////////////////////////////////////////
//
// Creating New Instances
//
///////////////////////////////////////////////////////////////////////////////

// Thaw out a previously frozen instance (produced with
// uint8_image_freeze) using data pointed to by file_pointer.  Frozen
// instances aren't portable between platforms.  After thawing
// file_pointer points to the data immediately following the data from
// the thawed instance (or to the end of the file).
UInt8Image *
uint8_image_thaw (FILE *file_pointer);

// Create a new image filled with zero pixel values.
UInt8Image *
uint8_image_new (ssize_t size_x, ssize_t size_y);

// Create a new image with pixels initialized to value.
UInt8Image *
uint8_image_new_with_value (ssize_t size_x, ssize_t size_y, uint8_t value);

// Create a new image from memory.  This pixels are assumed to be
// layed out in memory in the usual way, i.e. contiguous rows of
// pixels in the x direction are contiguous in memory.
UInt8Image *
uint8_image_new_from_memory (ssize_t size_x, ssize_t size_y, uint8_t *buffer);

// Create a new independent copy of model.
UInt8Image *
uint8_image_copy (UInt8Image *model);

// Form reduced resolution version of the model.  The scale_factor
// must be positive and odd.  The new image will be round ((double)
// model->size_x / scale_factor) pixels by round ((double)
// model->size_y / scale_factor) pixels.  Scaling is performed by
// averaging blocks of pixels together, using odd pixel reflection
// around the image edges (see the description of the apply_kernel
// method).  The average for a block of pixels is first computed as a
// double precision floating point number, then rounded with the C99
// round() function.  The upper and leftmost blocks of pixels averaged
// together are always centered at the 0 index in the direction in
// question, so reflection is always used for these edges.  Whether
// reflection is used for the right and lower edges depends on the
// relationship between the model dimensions and the scale factor.
UInt8Image *
uint8_image_new_from_model_scaled (UInt8Image *model, ssize_t scale_factor);

// Create a new image by copying the portion of model with upper left
// corner at model coordinates (x, y), width size_x, and height
// size_y.
UInt8Image *
uint8_image_new_subimage (UInt8Image *model, ssize_t x, ssize_t y,
			  ssize_t size_x, ssize_t size_y);

// Create a new image from data at byte offset in file.  The pixel
// layout in the file is assumed to be the same as for the
// uint8_image_new_from_memory method.  The byte order of individual
// pixels in the file should be byte_order.
UInt8Image *
uint8_image_new_from_file (ssize_t size_x, ssize_t size_y, const char *file,
			   off_t offset);

// The method is like new_from_file, but takes a file pointer instead
// of a file name, and the offset argument is with respect to the
// current position in the file_pointer stream.
UInt8Image *
uint8_image_new_from_file_pointer (ssize_t size_x, ssize_t size_y,
				   FILE *file_pointer, off_t offset);

// Form a low quality reduced resolution version of the
// original_size_x by original_size_y image in file.  The new image
// will be size_x by size_y pixels.  This method is like new_from_file
// method, but gets its data by sampling in each dimension using
// bilinear interpolation, and rounding the interpolated values using
// the C99 round() routine.  This is a decent way of forming quick
// thumbnails of images, or of scaling images down just slightly (by a
// factor of say 1.5 or less), but not much else.
UInt8Image *
uint8_image_new_from_file_scaled (ssize_t size_x, ssize_t size_y,
				  ssize_t original_size_x,
				  ssize_t original_size_y,
				  const char *file, off_t offset);

// The function that does it all, generating an instance of UInt8Image
// from a file and the metadata
UInt8Image *
uint8_image_new_from_metadata(meta_parameters *meta, const char *file);

// For multi-band imagery the previous function needs to be more specific.
UInt8Image *
uint8_image_band_new_from_metadata(meta_parameters *meta,
				   int band, const char *file);

///////////////////////////////////////////////////////////////////////////////
//
// Getting and Setting Image Pixels and Regions
//
///////////////////////////////////////////////////////////////////////////////

// Get pixel at 0-indexed position x, y.  A cache is used so pixel
// access does not usually involve the hard disk, provided subsequent
// pixel lookups are spatially close together.  You can probably get
// away with trating this function just as if everything was in
// memory.  For details, see the cache control methods below.
//
// The x and y arguments should always be positive, ssize_t is used
// only so people can't as easily shoot themselves in the foot by
// accidently supplying negative arguments which get promoted in some
// strange way.
uint8_t
uint8_image_get_pixel (UInt8Image *self, ssize_t x, ssize_t y);

// Set pixel at 0-indexed position x, y to value.  A cache is used to
// make this fast, as for the uint8_image_get_pixel method.
void
uint8_image_set_pixel (UInt8Image *self, ssize_t x, ssize_t y, uint8_t value);

// Get rectangular image region of size_x, size_y having upper left
// corner at x, y and copy it into already allocated buffer.  There is
// not necessarily any caching help for this method, i.e. it may
// always involve disk access and always be slow.
void
uint8_image_get_region (UInt8Image *self, ssize_t x, ssize_t y,
			ssize_t size_x, ssize_t size_y, uint8_t *buffer);

// This method is analogous to uint8_image_get_region.
void
uint8_image_set_region (UInt8Image *self, size_t x, size_t y, size_t size_x,
			size_t size_y, uint8_t *buffer);

// Get a full row of pixels, copying the data into already allocated
// buffer.  This method will be fast on the average for calls with
// sequential row numbers.
void
uint8_image_get_row (UInt8Image *self, size_t row, uint8_t *buffer);

// Get a pixel, performing odd reflection at image edges if the pixel
// indicies fall outside the image.  See the description of the
// apply_kernel method for an explanation of reflection.
uint8_t
uint8_image_get_pixel_with_reflection (UInt8Image *self, ssize_t x, ssize_t y);

///////////////////////////////////////////////////////////////////////////////
//
// Image Analysis and Statistics
//
///////////////////////////////////////////////////////////////////////////////

// Default mask value when figuring image stats
#define UINT8_IMAGE_DEFAULT_MASK (0.0)

// Finds the minimum and maximum pixel values in the image, and the
// mean and standard deviation of all pixels.  This function considers
// every pixel in the image when mask is NAN, otherwise it discounts
// all values within .00000000001 of the mask
void
uint8_image_statistics (UInt8Image *self, uint8_t *min, uint8_t *max,
			double *mean, double *standard_deviation,
			gboolean use_mask_value, uint8_t mask_value);

// Does the same thing as uint8_image_statistics() except that the
// statistics are calculated for a particular band selection within
// a UInt8Image which contains multiple bands or channels in sequential
// order.  Returns 1 on error, otherwise 0
int
uint8_image_band_statistics (UInt8Image *self, meta_stats *stats,
                             int line_count, int band_no,
                             gboolean use_mask_value, uint8_t mask_value);

// This method works like the statistics method, except values in the
// interval [interval_start, interval_end] are not considered at all
// for the purposes of determining any of the outputs.
void
uint8_image_statistics_with_mask_interval (UInt8Image *self, uint8_t *min,
					   uint8_t *max, double *mean,
					   double *standard_deviation,
					   uint8_t interval_start,
					   uint8_t interval_end);

// Compute an efficient estimate of the mean and standard deviation of
// the pixels in the image, by sampling every stride th pixel in each
// dimension, beginning with pixel (0, 0). If the mask is a non-NAN
// value, this function will discount all values within .00000000001
// of the mask
void
uint8_image_approximate_statistics (UInt8Image *self, size_t stride,
                                    double *mean, double *standard_deviation,
				    gboolean use_mask_value,
				    uint8_t mask_value);

// This method is a logical combination of the
// statistics_with_mask_interval and approximate_statistics methods.
void
uint8_image_approximate_statistics_with_mask_interval
  (UInt8Image *self, size_t stride, double *mean, double *standard_deviation,
   uint8_t interval_start, uint8_t interval_end);

// Creates a gsl_histogram with bin_count bins evenly spaced over the
// interval [min, max].  This function considers every pixel in the
// image.
gsl_histogram *
uint8_image_gsl_histogram (UInt8Image *self, double min, double max,
                           size_t bin_count);

///////////////////////////////////////////////////////////////////////////////
//
// Kernels, Interpolation, and Sampling
//
///////////////////////////////////////////////////////////////////////////////

// Apply kernel centerd at pixel x, y and return the value.  The
// kernel matrix must be have equal odd dimensions.  The values in the
// kernel are multiplied by the pixels, and the sum of the products
// returned.  When part of the kernel would fall outside the image
// extents, the values used for the out-of-image pixels are the mirror
// images of the corresponding in-image pixels, with the edge pixels
// not duplicated, i.e. reflection about the middle of the edge pixels
// is used.
double
uint8_image_apply_kernel (UInt8Image *self, ssize_t x, ssize_t y,
			  gsl_matrix *kern);

// Type used to specify whether disk files should be in big or little
// endian byte order.

// Sample method types.  These dictate how nearby pixels are
// considered when we want to find the approximate value for a point
// which falls between pixel indicies.
typedef enum {
  // Nearest pixel.
  UINT8_IMAGE_SAMPLE_METHOD_NEAREST_NEIGHBOR,
  // Linearly weited average of four nearest pixels
  UINT8_IMAGE_SAMPLE_METHOD_BILINEAR,
  // Bicubic spline interpolation (which consideres the nearest 16 pixels).
  UINT8_IMAGE_SAMPLE_METHOD_BICUBIC
} uint8_image_sample_method_t;

double
uint8_image_sample (UInt8Image *self, double x, double y,
		    uint8_image_sample_method_t sample_method);

///////////////////////////////////////////////////////////////////////////////
//
// Comparing Images
//
///////////////////////////////////////////////////////////////////////////////

// Return true iff self and other have identical sizes and pixels.
gboolean
uint8_image_equals (UInt8Image *self, UInt8Image *other);

///////////////////////////////////////////////////////////////////////////////
//
// Storing Images in Files
//
///////////////////////////////////////////////////////////////////////////////

// Store instance self at position pointed to by file_pointer, for
// later retrieval using uint8_image_thaw.  The serialized version of
// self is not portable between platforms.
void
uint8_image_freeze (UInt8Image *self, FILE *file_pointer);

// Store image pixels in file.  The image is stored in the usual
// order, i.e. contiguous rows of pixels in the x direction are stored
// contiguously in memory.  Individual pixels are stored in byte order
// byte_order.  Returns 0 on success, nonzero on error.
int
uint8_image_store (UInt8Image *self, const char *file);

int
uint8_image_store_ext(UInt8Image *self, const char *file, int append_flag);

int
uint8_image_band_store(UInt8Image *self, const char *file,
		       meta_parameters *meta, int append_flag);

///////////////////////////////////////////////////////////////////////////////
//
// Exporting Images in Various Image File Formats
//
///////////////////////////////////////////////////////////////////////////////

// Export image to fila as a gray scaled jpeg image, with largest
// dimension no larger than max_dimension.  The max_dimension argument
// must be less than or equal to the largest dimension of the image.
// The image may be scaled st its largest dimension is considerably
// less than max_dimension.  Scaling is performed by averaging blocks
// of pixels together, using odd pixel reflection around the image
// edges (see the description of the apply_kernel method), and
// rounding pixel averages with the C99 round() routine.  If
// use_mask_value is true, then in determining the image statistics
// (mean and standard deviation), values equal to mask are not
// considered.  This routine slurps the whole image into memory, so
// beware.  Returns 0 on success, nonzero on error.
int
uint8_image_export_as_jpeg (UInt8Image *self, const char *file,
			    size_t max_dimension, gboolean use_mask_value,
			    uint8_t mask_value);

///////////////////////////////////////////////////////////////////////////////
//
// Controlling Image Data Caching
//
// It probably isn't necessary to use these methods.  They are
// provided largely to make it clear how the cache works.  The major
// tunable parameter is the size of the in-memory cache to use.
//
// When a new image is created, the following steps are performed:
//
//      1. The image is divided up into square tiles st two full rows
//         or columns of tiles will fit in the memory cache.
//
//      2. A copy of the image is created on disk with the memory
//         layout rearranged st individual tiles are contiguous in
//         memory.  This allows tiles to be quickly retrieved later.
//
// When a pixel is accessed (read or set), the following happens:
//
//      1. If the pixel is in a tile already loaded into the cache,
//         it is simply fetched or set.
//
//      2. Otherwise, the tile containing the pixel is loaded,
//         possibly displacing an already loaded tile, and then the
//         pixel is fetched or set.  The tile displaced is the one
//         loaded longest ago (there is no most-recently-accessed
//         heuristic, as this would make pixel access too slow).
//
// Thus, using a larger memory cache will result in larger tiles being
// used, and fewer tile loads being needed.  In general, the default
// behavior is pretty good, but if you know will be performing lots of
// widely (but not too widely) scattered accesses, you might want to
// make it bigger.
//
///////////////////////////////////////////////////////////////////////////////

// Get the image memory cache size setting, in bytes.  Note that this
// is the memory cache used per image, not the class-wide cache usage.
// If you will have a lot of objects instantiated simultaneously, you
// may find it necessary to use a smaller cache for each image.
size_t
uint8_image_get_cache_size (UInt8Image *self);

// Set the image memory cache to size bytes.  Changing the cache size
// requires the tiling to be recomputed, the on-disk tile cache to be
// regenerated, and the in memory cache to be flushed, so its slow.
void
uint8_image_set_cache_size (UInt8Image *self, size_t size);

///////////////////////////////////////////////////////////////////////////////
//
// Freeing Instances
//
///////////////////////////////////////////////////////////////////////////////

// Destroy self.
void
uint8_image_free (UInt8Image *self);

#endif // #ifndef UINT8_IMAGE_H
