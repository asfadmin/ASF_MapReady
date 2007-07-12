// A simple stinkin' viewer for ASF data that doesn't depend on Tcl/Tk
// and implements a few features we always wanted (flicker, region
// analysis).

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <gtk/gtk.h>

#include <gtk/gtkgl.h>

#include <GL/gl.h>
#include <GL/glu.h>

#include <asf_meta.h>
#include <float_image.h>

#include "pix_maps_spec.h"
#include "ssv_command_line.h"
#include "utilities.h"
#include "viewer_image.h"
#include "viewer_window.h"

// When a window receives a delete event, we remove it from the
// application window list (passed in user_data pointer), unref it,
// and if it was the last window, call gtk_main_quit().
static gboolean
on_window_delete_event (GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
  GPtrArray *windows = user_data;

  // Find the window which got the signal in the list of windows.
  gint ii;
  ViewerWindow *vw;
  for ( ii = 0 ; ii < windows->len ; ii++ ) {
    vw = g_ptr_array_index (windows, ii);
    if ( vw->w == widget ) {
      break;
    }
  }
  g_assert (ii < windows->len);	// We better have found the window.

  // Top level program should be the only reference holder to the window.
  g_assert (vw->reference_count == 1);

  g_ptr_array_remove_index (windows, ii);

  viewer_window_unref (vw);

  if ( windows->len == 0 ) {
    // Well gtk seems to get itself all confused and seg fault somehow
    // when we end up calling gtk_main_quit here as a result of Escape
    // having been pressed (which emits delete_event on the window).
    // Works fine when it gets to emit the signal itself when we press
    // the close box.  Not sure what's going on, and at this point we
    // might as well just exit. 
    exit (0);
    gtk_main_quit ();
  }

  // Gtk also seg faults after we leave this function, even if we skip
  // gtk_main_quit.  It dies in some signal handler, but stopping
  // emission of the signals mentioned in the backtrace before
  // returning from this function doesn't help, so I'm not sure what
  // to do.  This would have to be sorted out before multiple windows
  // could be used I guess.
  //
  // g_signal_stop_emission_by_name (widget, "delete-event::");
  // g_signal_stop_emission_by_name (widget, "key-press-event::");

  return TRUE;
}

int
main (int argc, char **argv)
{
  gtk_init (&argc, &argv);

  // The reality is that warnings shouldn't happen in decent code.
  g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING);

  SSVCommandLine *cl = ssv_command_line_new (&argc, &argv);

  // If we are using the default cache directory, and it doesn't
  // exist, we create it.
  GString *default_cache_dir = g_string_new (getenv ("HOME"));
  // IMPROVEME: this must be manually synced with the notion of the
  // default in ssv_command_line.c.
  g_string_append (default_cache_dir, "/.ssv_cache");
  if ( g_string_equal (cl->cache_dir, default_cache_dir) ) {
    GError *err = NULL;
    if ( ! my_is_writable_directory (cl->cache_dir->str, &err) ) {
      g_assert (err->code == MY_G_UTILITIES_ERROR_G_STAT_FAILED);
      int return_code = mkdir (cl->cache_dir->str, S_IRWXU);
      g_assert (return_code == 0);
    }
  }

  // Open the interface to the pyramid_cache.
  PyramidCache *pyramid_cache = pyramid_cache_new (cl->cache_dir->str,
						   cl->max_cache_size);

  // List of all the ViewerImage instances being viewed.
  GPtrArray *images = g_ptr_array_new ();

  // The big picture coordinate system is defined (internally at
  // least) to have upper left corner (0, 0) and positive y axis
  // pointing down, with the upper-left-most image being examined
  // having its upper left corner at (0, 0) (big picture coordinates).
  // Since some image offsets specified on the command line may be
  // negative, we need to find the smallest offsets (assuming that any
  // offset arguments have been supplied).  The first image argument
  // is understood implicitly to have command line offset arguments of
  // (0, 0) (so these are the largest minimums possible).
  ssize_t min_x_offset = 0, min_y_offset = 0;
  guint ii;
  for ( ii = 0 ; ii < cl->x_offsets->len ; ii++ ) {
    gint cxo = g_array_index (cl->x_offsets, gint, ii);	// Current x offset.
    gint cyo = g_array_index (cl->y_offsets, gint, ii); // Current y offset.
    if ( cxo < min_x_offset ) { min_x_offset = cxo; }
    if ( cyo < min_y_offset ) { min_y_offset = cyo; }
  }

  // Create ViewerImage instances for all the image base name
  // arguments.
  for ( ii = 0 ; ii < cl->images->len ; ii++ ) {
    GString *cin = g_ptr_array_index (cl->images, ii);
    // If first image (which establishes coordinates system) or if
    // offset arguments haven't been used, we don't have any offsets to look
    // up for this image, so use the minimum offsets by themselves.
    ViewerImage *ci;
    if ( ii == 0 || cl->x_offsets->len == 0 ) {
      ci = viewer_image_new (cin->str, cl->pixmaps, cl->sigmas,
			     -min_x_offset, -min_y_offset, pyramid_cache);
    }
    else {
      // Current x offset.
      gint cxo = g_array_index (cl->x_offsets, gint, ii - 1);
      // Current y offset.
      gint cyo = g_array_index (cl->y_offsets, gint, ii - 1);
      ci = viewer_image_new (cin->str, cl->pixmaps, cl->sigmas,
			     cxo - min_x_offset, cyo - min_y_offset,
			     pyramid_cache);
    }
    g_ptr_array_add (images, ci);
  }

  // We can now compute the initial dimensions of the big picture
  // coordinate system domain.  These results will be used to
  // initialize the big picture understanding in the ViewerWindow
  // instances.  Note that if the user is later allowed to change an
  // image's offsets, the size of the big picture domain may change,
  // and all windows will need to be notified of this somehow, and
  // react appropriately...  This is an interwindow event, and would
  // be a pain to implement.  The best way is probably for all windows
  // to hold references to all the images, since they will need to do
  // something like that anyway to get flicke working.
  ssize_t min_x = SSIZE_MAX, max_x = -(SSIZE_MAX - 1);
  g_assert (max_x < 0);   // Roll over check, since SSIZE_MIN not defined.
  ssize_t min_y = SSIZE_MAX, max_y = -(SSIZE_MAX - 1);
  g_assert (max_y < 0);   // Roll over check, since SSIZE_MIN not defined.
  for ( ii = 0 ; ii < images->len ; ii++ ) {
    ViewerImage *ci = g_ptr_array_index (images, ii);
    // Lower right corner x and y indicies (last addressable pixel in
    // for example x direction is lr_x - 1).
    ssize_t lr_x = ci->x_offset + ci->size_x;
    ssize_t lr_y = ci->y_offset + ci->size_y;
    if ( ci->x_offset < min_x ) min_x = ci->x_offset;
    if ( lr_x > max_x ) max_x = lr_x;
    if ( ci->y_offset < min_y ) min_y = ci->y_offset;
    if ( lr_y > max_y ) max_y = lr_y;
  }
  // We normalize the big picture coordinate system so its upper left
  // corner is (0, 0) (with the positive y axis pointing down).
  size_t bp_max_x = max_x - min_x, bp_max_y = max_y - min_y;
  
  // List of all the windows currently in existence.
  GPtrArray *windows = g_ptr_array_new ();

  // We start off with one window of the first image.
  ViewerImage *i1 = g_ptr_array_index (images, 0);
  ViewerWindow *w1 = viewer_window_new (i1, images, bp_max_x - 1, bp_max_y - 1,
					0, 0, bp_max_x - 1, bp_max_y - 1,
					cl->analysis_program,
					cl->async_analysis,
					cl->analysis_tile_size);
  g_ptr_array_add (windows, w1);
  g_signal_connect (G_OBJECT (w1->w), "delete_event",
		    G_CALLBACK (on_window_delete_event), windows);

  gtk_main ();

  viewer_image_unref (i1);

  // Note that the ViewerWindow instances in this array have already
  // been unrefed by the delete_event callback.
  my_g_ptr_array_really_free (windows, NULL);

  my_g_ptr_array_really_free (images, (FreeFunc) viewer_image_unref);

  exit (EXIT_SUCCESS);
}

