// A rectangular block of data of type float.  A transparent block
// cache is used for each instance of this class, so the image can be
// much bigger than will fit in memory, and pixels can still be
// accessed and written efficiently, provided subsequent accesses are
// spatially correlated.  A variety of useful methods are implemented
// (filtering, subsetting, interpolating, etc.)

#ifndef FLOAT_IMAGE_H
#define FLOAT_IMAGE_H

#include <stdio.h>
#include <sys/types.h>

#include <glib.h>

// Instance structure.  Everything here is private and need not be
// used or understood by client code.
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
  float *cache;                 // Memory cache.
  float **tile_addresses;	// Addresss of individual tiles in the cache.
  GQueue *tile_queue;		// Queue of tile offsets kept in load order.
  FILE *tile_file;              // File with tiles stored contiguously.
} FloatImage;

///////////////////////////////////////////////////////////////////////////////
//
// Creating New Instances
//
///////////////////////////////////////////////////////////////////////////////

// Create a new image filled with zero pixel values.
FloatImage *
float_image_new (size_t size_x, size_t size_y);

// Create a new image with pixels initialized to value.
FloatImage *
float_image_new_with_value (size_t size_x, size_t size_y, float value);

// Create a new image from memory.  This pixels are assumed to be
// layed out in memory in the usual way, i.e. contiguous rows of
// pixels in the x direction are contiguous in memory.
FloatImage *
float_image_new_from_memory (size_t size_x, size_t size_y, float *buffer);

// Type used to specify whether disk files should be in big or little
// endian byte order.
typedef enum {
  FLOAT_IMAGE_BYTE_ORDER_LITTLE_ENDIAN,
  FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN
} float_image_byte_order_t;

// Create a new image from data at byte offset in file.  The pixel
// layout in the file is assumed to be the same as for the
// float_image_new_from_memory method.  The byte order of individual
// pixels in the file should be byte_order.
FloatImage *
float_image_new_from_file (size_t size_x, size_t size_y, char *file,
			   off_t offset, float_image_byte_order_t byte_order);

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
float
float_image_get_pixel (FloatImage *self, ssize_t x, ssize_t y);

// Set pixel at 0-indexed position x, y to value.  A cache is used to
// make this fast, as for the float_image_get_pixel method.
void
float_image_set_pixel (FloatImage *self, ssize_t x, ssize_t y, float value);

// Get rectangular image region of size_x, size_y starting at x, y and
// copy it into already allocated buffer.  There is not necessarily
// any caching help for this method, i.e. it may involve disk access
// and be slow.
void
float_image_get_region (FloatImage *self, size_t x, size_t y, size_t size_x, 
			size_t size_y, float *buffer);

// This method is analogous to float_image_get_region.
void
float_image_set_region (FloatImage *self, size_t x, size_t y, size_t size_x, 
			size_t size_y, float *buffer);

///////////////////////////////////////////////////////////////////////////////
//
// Storing Images in Files
//
///////////////////////////////////////////////////////////////////////////////

// Store image in file.  The image is stored in the usual order,
// i.e. contiguous rows of pixels in the x direction are stored
// contiguously in memory.  Individual pixels are stored in byte order
// byte_order.  Returns 0 on success, nonzero on error.
int
float_image_store (FloatImage *self, char *file, 
		   float_image_byte_order_t byte_order);

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
float_image_get_cache_size (FloatImage *self);

// Set the image memory cache to size bytes.  Changing the cache size
// requires the tiling to be recomputed, the on-disk tile cache to be
// regenerated, and the in memory cache to be flushed, so its slow.
void
float_image_set_cache_size (FloatImage *self, size_t size);

///////////////////////////////////////////////////////////////////////////////
//
// Freeing Instances
//
///////////////////////////////////////////////////////////////////////////////

// Destroy self.
void
float_image_free (FloatImage *self);

#endif // FLOAT_IMAGE_H
