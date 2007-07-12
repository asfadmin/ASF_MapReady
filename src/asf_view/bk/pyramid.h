// An image pyramid, with the base layer corresponding to an image of
// interest to the user, and each layer above corresponding to a
// version of the image with resolution half that of the previous
// layer.  Useful for fast image zooming.

#ifndef PYRAMID_H
#define PYRAMID_H

#include <float_image.h>

#include "pix_maps_spec.h"
#include "pyramid_cache.h"

// Signature of callback to be supplied by user to get notification of
// when a new pyramid is fully constructed.
typedef void (*completion_callback)(void *);

// A single layer of a pyramid.
typedef struct {
  // Public read-only data members.
  size_t size_x, size_y;	// Layer dimensions.
  gboolean is_float_blob;	// Is data a FloatBlob or a pointer to floats?
  gpointer data;		// Either a FloatBlob or a pointer to floats.
} pyramid_layer;

typedef struct {
  // Public read-only data members.
  GString *base_name;		// Base name of .img/.meta pair.
  GPtrArray *layers;		// Array of pyramid_layer, index 0 is largest.
  PixMapsSpec *pixmaps;         // Pixel maps used in creating upper layers.

  // Private data members.
  //
  // Cache being used, or NULL if no cache is in use.
  PyramidCache *cache;
  // Signature used as cache key, or NULL if no cash is in use.
  GString *signature;
  // Place to store temporary layer files, or NULL if a cache is being
  // use.
  GString *scratch_dir;
  gint reference_count;		
} Pyramid;

// Largest layer, in bytes, that will be stored in memory instead of
// in a FloatBlob.
#define PYRAMID_BIGGEST_LAYER_IN_MEMORY 4000000

// Form a new pyramid from base_name.meta and base_name.img, with
// pixels for all layers above the first computed using base layer
// pixels remapped according to pixmaps, using scratch_dir to store
// temporary layer files.  Internally, the base layer is stored
// without pixmaps applied (because in fact the original file is
// generally used), but the get_region method automaticly applies the
// pixmaps.
Pyramid *
pyramid_new (const char *base_name, PixMapsSpec *pixmaps,
	     const char *scratch_dir);

// Form a new pyramid from base_name.meta and base_name.img subject to
// pixmaps (which is referenced, not copied, and should not be
// modified after this call), using data from cache if available, and
// installing produced data in the cache if it isn't.  If some other
// program or thread is already generating a pyramid for the base_name
// and pixmaps in question, we wait for them to finish and use the
// version they produce.  The new pyramid acquires its own new
// reference to cache and maintains it until it itself is freed.
Pyramid *
pyramid_new_using_cache (const char *base_name, PixMapsSpec *pixmaps,
			 PyramidCache *cache);

// Get a region of the base image, subject to pixmaps, covering at
// least a region defined by upper left pixel (start_x, start_y)
// (counting down and right from upper left corner of base image),
// width w, and height h (in base image pixels), at zoom factor zoom.
// The zoom factor must be a power of two and is equal to the square
// root of the number of base image pixels which have been averaged
// together to form each pixel in the returned region.
//
// The region actually returned may be substantially bigger that the
// requested region (if a superset of the requested region is already
// in memory, a pointer to this memory is returned). We return a
// pointer to the actual data, the upper left corner of the region
// with respect to the base, the dimensions of the returned region in
// pixels (actual pixels, not base image pixels), and a flag which is
// true iff the returned memory is otherwise unwoned (if it is, then
// the caller is responsible for freeing it with g_free (NOT with
// free), otherwise the caller must not free the region).
void
pyramid_get_region (Pyramid *self, ssize_t start_x, ssize_t start_y,
		    ssize_t w, ssize_t h, ssize_t zoom, 
		    float **region, size_t *rstart_x, size_t *rstart_y,
		    size_t *rw, size_t *rh, gboolean *unowned_memory);

// Export the entire pyramid as a batch of jpeg files names
// "base_0.jpg", "base_1.jpg", etc.  IMPROVEME: this is not as
// efficient as it could be (it create one FloatImage per layer).
void
pyramid_export_as_jpeg_files (Pyramid *self, const char *base);

// Export the entire pyramid as a batch of binary floating point blobs
// in the native machine endian format, using file names "base_0.raw",
// "base_1.raw", etc.
void
pyramid_export_as_float_blobs (Pyramid *self, const char *base);

// Increment reference count.  Return pointer to self for convenience.
Pyramid *
pyramid_ref (Pyramid *self);

// Decrement reference count, freeing instance if count falls to 0.
void
pyramid_unref (Pyramid *self);

#endif // PYRAMID_H
