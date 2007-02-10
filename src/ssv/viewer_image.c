// Implementation of interface described in viewer_image.h.

#include <math.h>
#include <unistd.h>

#include <gsl/gsl_statistics_double.h>

#include "meta_read_wrapper.h"
#include "utilities.h"
#include "viewer_image.h"

// We use a seperate thread to fill in the stuff that takes some time
// to create.
static gpointer
create_pyramid (ViewerImage *self)
{
  g_assert (self->pixmaps != NULL);

  if ( self->pyramid_cache != NULL ) {
    g_assert (self->scratch_dir == NULL);
    // We never unref this pyramid.  The reason is that we don't want
    // to keep this thread alive just for that purpose, and since the
    // unreffing the pyramid triggers a pyramid_cache_release, it has
    // to be done by its creator.  It will get reclaimed anyway by the
    // OS.
    self->pyramid
      = pyramid_new_using_cache (self->base_name->str, self->pixmaps,
				 self->pyramid_cache);
  }
  else {
    g_assert (self->scratch_dir != NULL);
    g_assert (self->pyramid_cache == NULL);
    self->pyramid = pyramid_new (self->base_name->str, self->pixmaps,
				 self->scratch_dir->str);
  }

  // Uncomment to get debugging snapshots of the pyramid layers.
  // pyramid_export_as_float_blobs (self->pyramid, "pyramid_snapshot");
  // pyramid_export_as_jpeg_files (self->pyramid, "pyramid_snapshot");

  g_mutex_lock (self->pyramid_mutex);
  self->pyramid_ready = TRUE;
  g_mutex_unlock (self->pyramid_mutex);

  // This thread isn't even joinable, but we keep the compiler happy
  // with a return statement anyway.
  return NULL;
}

ViewerImage *
viewer_image_new (const char *base_name, PixMapsSpec *pixmaps, gdouble sigmas,
		  size_t x_offset, size_t y_offset,
		  PyramidCache *pyramid_cache)
{
  ViewerImage *self = g_new (ViewerImage, 1);

  self->base_name = g_string_new (base_name);

  GString *mn = g_string_append (g_string_new (base_name), ".meta");
  GString *dn = g_string_append (g_string_new (base_name), ".img");

  self->meta = meta_read_wrapper (mn->str);
  meta_parameters *md = self->meta; // Convenience alias.

  self->size_x = md->general->sample_count;
  self->size_y = md->general->line_count;

  self->x_offset = x_offset;
  self->y_offset = y_offset;

  self->pixmaps = pix_maps_spec_ref (pixmaps);

  self->sigmas = sigmas;

  g_assert (self->size_x <= INT_MAX);
  gint sx = self->size_x;
  g_assert (self->size_y <= INT_MAX);
  gint sy = self->size_y;

  FILE *df = fopen (dn->str, "r");
  g_assert (df != NULL);

  // We will form the quick look by sampling every pixel_strid'th
  // pixel.  This number reflects a critical tradeoff -- bigger means
  // that we won't have a quick view of sufficiently high resolution
  // ready when the user tries to zoom in on an image, and interactive
  // performance will suffer when we create one on the fly.  On the
  // other hand, setting this number too small may run some machines
  // out of RAM and into virtual memory, which is also likely to
  // degrade interactive performance significantly.  Both these
  // problems go away once we have the pyramid fully biult, but of
  // course this takes a few seconds to do.
  const size_t pixel_stride = 8;

  self->quick_view_stride = pixel_stride;

  // Quick view dimensions.
  self->qvw = ceil ((double) sx / pixel_stride);
  self->qvh = ceil ((double) sy / pixel_stride);

  // Memory for quick_view.
  self->quick_view = g_new (float, self->qvw * self->qvh);

  // Storage for one image row.
  float *row_buffer = g_new (float, sx);

  size_t ii, jj;
  size_t cqvr = 0, cqvc = 0;		// Current quick_view row and column.
  for ( jj = 0 ; jj < sy ; jj += pixel_stride, cqvr++ ) {
    cqvc = 0;
    size_t read_count = fread (row_buffer, sizeof (float), sx, df);
    g_assert (read_count == sx);
    int return_code = fseek (df, sizeof (float)  * sx * (pixel_stride - 1),
			     SEEK_CUR);
    g_assert (return_code == 0);
    for ( ii = 0 ; ii < sx ; ii += pixel_stride, cqvc++ ) {
      float pv = row_buffer[ii];
      if ( G_BYTE_ORDER == G_LITTLE_ENDIAN ) {
	swap_bytes_32 ((unsigned char *) &pv);
      }
      (self->quick_view)[cqvr * self->qvw + cqvc] = pv;
    }
  }

  size_t qvpc = self->qvw * self->qvh;   // Quick view pixel count.

  // Apply pixmaps.
  for ( ii = 0 ; ii < qvpc ; ii++ ) {
    (self->quick_view)[ii]
      = pix_maps_spec_map (self->pixmaps, (self->quick_view)[ii]);
  }

  // Double precision version of quickview data so we can use GSL
  // library functions for statistics.
  double *qvad = g_new (double, qvpc);
  for ( ii = 0 ; ii < qvpc ; ii++ ) {
    qvad[ii] = (self->quick_view)[ii];
  }

  // Compute statistics for OpenGL to use for scaling.  IMPROVEME: it
  // might be possible to use OpenGL to do this or near enough using
  // the faster functions from the OpenGL imaging subset.
  gsl_stats_minmax (&(self->min), &(self->max), qvad, 1, qvpc);
  self->mean = gsl_stats_mean (qvad, 1, qvpc);
  self->sdev = gsl_stats_sd_m (qvad, 1, qvpc, self->mean);

  g_free (qvad);

  g_free (row_buffer);

  if ( pyramid_cache == NULL ) {
    self->pyramid_cache = NULL;
    self->scratch_dir = g_string_new ("/tmp/");
  }
  else {
    self->pyramid_cache = pyramid_cache_ref (pyramid_cache);
    self->scratch_dir = NULL;
  }

  if ( !g_thread_supported () ) {
    g_thread_init (NULL);
  }

  self->pyramid_ready = FALSE;
  self->pyramid_mutex = g_mutex_new ();
  self->pyramid = NULL;

  GThread *pyramid_thread
    = g_thread_create ((GThreadFunc) create_pyramid, self, FALSE, NULL);
  g_assert (pyramid_thread != NULL);
  // We try to keep things snappy for the use during the time before
  // the pyramids are finished.
  g_thread_set_priority (pyramid_thread, G_THREAD_PRIORITY_LOW);
  
  g_string_free (mn, TRUE);
  g_string_free (dn, TRUE);

  self->reference_count = 1;

  return self;
}

Pyramid *
viewer_image_pyramid (ViewerImage *self)
{
  Pyramid *result = NULL;
  g_mutex_lock (self->pyramid_mutex);
  if ( self->pyramid_ready ) {
    result = self->pyramid;
  }
  g_mutex_unlock (self->pyramid_mutex);

  return result;
}

ViewerImage *
viewer_image_ref (ViewerImage *self)
{
  self->reference_count++;

  return self;
}

void
viewer_image_unref (ViewerImage *self)
{
  self->reference_count--;

  if ( self->reference_count == 0 ) {
    while ( viewer_image_pyramid (self) == NULL ) {
      // oo icky poo a polling loop in a destructor!
      const guint sleep_time = 1;
      sleep (sleep_time);
    }
    viewer_image_free (self);
  }
}

gboolean
viewer_image_free (ViewerImage *self)
{
  if ( viewer_image_pyramid (self) == NULL ) {
    return FALSE;
  }

  g_string_free (self->base_name, TRUE);
  meta_free (self->meta);
  pix_maps_spec_unref (self->pixmaps);
  g_free (self->quick_view);
  if ( self->pyramid_cache != NULL ) {
    g_assert (self->scratch_dir == NULL);
    pyramid_cache_unref (self->pyramid_cache);
    // Note that we deliberately don't unref the pyramid here; see
    // comments in create_pyramid().
  }
  else {
    g_assert (self->scratch_dir != NULL);
    g_assert (self->pyramid_cache == NULL);
    g_string_free (self->scratch_dir, TRUE);
    pyramid_unref (self->pyramid);
  }
  g_mutex_free (self->pyramid_mutex);
  g_free (self);
  
  return TRUE;
}
