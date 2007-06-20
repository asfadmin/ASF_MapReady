// A ssv-ready datastructure which holds an image and associated
// pyramids, metadata, display state information, etc.

#ifndef VIEWER_IMAGE_H
#define VIEWER_IMAGE_H

#include <glib.h>

#include <asf_meta.h>
#include <float_image.h>

#include "pix_maps_spec.h"
#include "pyramid.h"

// WARNING: access the private data members and die!!! (probly with a
// seg fault).
typedef struct {

  // Public read-only data members.
  GString *base_name;		// Base name of image.
  meta_parameters *meta;	// Associted metadata.
  size_t size_x, size_y;	// Dimensions of base image (for convenience).
  // Offset of upper left in big picture coordinates.
  ssize_t x_offset, y_offset;
  PixMapsSpec *pixmaps;		// Pixel maps associated with this image.
  gdouble sigmas;               // See description of options.
  // Quick view of image, formed by grabbing ever quick_view_stride'th
  // (or so) sample in each direction (and subjecting them to pixmaps).
  float *quick_view;	
  size_t quick_view_stride;
  size_t qvw, qvh;              // Width and height of quick_view image.
  // Stats gathered from quick_view pixels.  We treat these as
  // representative and use them to scale the pixels of the image into
  // the range acceptable to OpenGL for both the quick_view and for
  // the base image and pyramids.
  double min, max, mean, sdev;

  // Private data members.
  // Exactly one of pyramid_cache and scratch_dir is non-NULL, depending
  // on whether a cache is being used or not.
  PyramidCache *pyramid_cache;  // Pyramid cache to use, or NULL.
  GString *scratch_dir;         // Scratch directory to use, or NULL.
  gboolean pyramid_ready;	// Flag true iff pyramid is fully built.
  GMutex *pyramid_mutex;	// Concurrency protection for pyramid_ready.
  Pyramid *pyramid;		// Pyramid for fast rendering.
  int reference_count;		// For reference counting.n

} ViewerImage;

// Create a new instance from image with base name base_name, subject
// to pixmaps, with x_offset and y_offset value of 0, using the
// pyramid_cache.  This method returns as soon as a quick subsampled
// form of the image in 'base_name.img' has been constructed, and
// spawns a thread to allow construction of a FloatImage instance
// version of the image and a full image pyramid to continue in the
// background.  Actually, the only thing that is safe to do until the
// instance is fully constructed is to look at the quick_view data.
// See comments for other methods for more details.
ViewerImage *
viewer_image_new (const char *base_name, PixMapsSpec *pixmaps, gdouble sigmas,
		  size_t x_offset, size_t y_offset,
		  PyramidCache *pyramid_cache);

// Returns the image pyramid pointer if the pyramid has been fully
// constructed, or NULL otherwise.
Pyramid *
viewer_image_pyramid (ViewerImage *self);

// Increment reference count, returning self as a convenience.
ViewerImage *
viewer_image_ref (ViewerImage *self);

// Decrement reference count, doing a viewer_image_free if the count
// falls to zero, BUUUT... since the image may not be fully
// constructed yet (i.e. base and/or pyramids not done) this method
// may have to wait for that to complete (it contains a polling loop).
// Of course, if you don't bother to unref things that are the whole
// point of your program, the operating system will do it for you...
//
// FIIXME: but it won't unlink files for you, so be sure there won't
// be huge float_blobs lying around somehow unfortunate, if possible.
// Hopefully at the moment they should all be part of user data files
// or in /tmp or in the cache, so it shouldn't be too terrible if they
// are.
void
viewer_image_unref (ViewerImage *self);

// Try to free viewer_image, regardless of reference count.  The catch
// is that you can't free what isn't fully constructed yet (at least
// not without painful stack unwinding issues), and since construction
// is partly done in a seperate thread, we might not be finished yet,
// in which case we don't deconstruct anything and return FALSE.  If
// we return TRUE then deconstruction has suceeded.  Note that this
// method will always succeed if viewer_image_is_fully_constructed is
// true.
gboolean
viewer_image_free (ViewerImage *self);

#endif // VIEWER_IMAGE_H
