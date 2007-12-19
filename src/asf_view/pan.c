// all this windows garbage is up here for the SetCursor() stuff - we want
// a "hand" cursor on windows when the user is panning
#ifdef win32
#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif

#include "asf_view.h"

// have to use static vars... actually, we probably should be using
// the "user_data" stuff that is always being passed around
static int dragging = FALSE;
static int start_x=0, start_y=0;
static GdkPixbuf *pb=NULL;
static GdkPixbuf *pb2=NULL;
static GtkWidget *img=NULL;
static GtkWidget *win=NULL;

#ifndef win32
static GdkCursor *pan_cursor=NULL;
#endif

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

// called when the user releases button -- must ensure user was dragging
// before doing anything.  If not, event has to be passed on as a normal
// button click
SIGNAL_CALLBACK int
on_button_release_event(GtkWidget *w, GdkEventButton *event, gpointer data)
{
  if (dragging) {
    dragging = FALSE;
    int x = (int) event->x;
    int y = (int) event->y;
    center_line -= zoom*(y-start_y);
    center_samp -= zoom*(x-start_x);
    gdk_pixbuf_unref(pb);

#ifdef win32
    //SetCursor(LoadCursor(NULL,IDC_HAND));
#else
    gdk_window_set_cursor(GDK_WINDOW(win->window), NULL);
#endif

    fill_big(curr);
    return TRUE;
  }
  return FALSE;
}

// called when user is dragging
SIGNAL_CALLBACK int
on_motion_notify_event(
    GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
  int x,y;
  GdkModifierType state;
  if (event->is_hint) {
      // I actually am not getting any hints... but the documentation
      // example of a motion notify handler has this in here...
      // so what the hey
    gdk_window_get_pointer(event->window, &x, &y, &state);
  } else {
    state = (GdkModifierType) event->state;
  } 

  if (state & GDK_BUTTON1_MASK) {
    // motion while Button 1 is pressed!
    x = (int) event->x;
    y = (int) event->y;

    if (!dragging) {
      // user just started dragging
      start_x = x;
      start_y = y;
      dragging = TRUE;

      if (!win)
        win = get_widget_checked("ssv_main_window");

      if (!img)
        img = get_widget_checked("big_image");

      pb = gtk_image_get_pixbuf(GTK_IMAGE(img));

#ifdef win32
      SetCursor(LoadCursor(NULL,IDC_HAND));
#else
      if (!pan_cursor)
        pan_cursor = gdk_cursor_new(GDK_FLEUR);

      gdk_window_set_cursor(GDK_WINDOW(win->window), pan_cursor);
#endif
    }

    assert(pb);
    int ii;
    int off_x = x-start_x;
    int off_y = y-start_y;

    //printf ("Button 1 motion  (%d, %d)\n", off_x, off_y);

    // put the panned image in a new pixbuf
    int nchan = 3;
    int biw = get_big_image_width();
    int bih = get_big_image_height();
    unsigned char *bdata = CALLOC(sizeof(unsigned char), biw*bih*nchan);

    // we refer to "pb" here -- the pixbuf that was being shown before
    // panning -- instead of panning event-to-event (i.e., using "pb2")
    int rowstride = gdk_pixbuf_get_rowstride(pb);
    unsigned char *pixels = gdk_pixbuf_get_pixels(pb);
    
    // copy pixels over
    // handle left&right completely off first
    // vertically off will be ok, loop will have 0 iterations
    if (off_x > biw || off_x < -biw) {
      // image is all black! no action needed
      ;
    }
    // switched to using these loops with memcpy()s, is much faster
    else if (off_x >= 0 && off_y >= 0) {
      for (ii=off_y; ii<bih; ++ii)
        memcpy(bdata + ii*rowstride + off_x*3,
               pixels + (ii-off_y)*rowstride,
               (biw-off_x)*3);
    } else if (off_x < 0 && off_y >= 0) {
      for (ii=off_y; ii<bih; ++ii)
        memcpy(bdata + ii*rowstride,
               pixels + (ii-off_y)*rowstride - off_x*3,
               (biw+off_x)*3);
    } else if (off_x >= 0 && off_y < 0) {
      for (ii=0; ii<bih+off_y; ++ii)
        memcpy(bdata + ii*rowstride + off_x*3,
               pixels + (ii-off_y)*rowstride,
               (biw-off_x)*3);
    } else if (off_x < 0 && off_y < 0) {
      for (ii=0; ii<bih+off_y; ++ii)
        memcpy(bdata + ii*rowstride,
               pixels + (ii-off_y)*rowstride - off_x*3,
               (biw+off_x)*3);
    } else {
      // The above cases should handle everything ...
      assert(0);
    }

    if (pb2)
      gdk_pixbuf_unref(pb2);

    pb2 = 
      gdk_pixbuf_new_from_data(bdata, GDK_COLORSPACE_RGB, FALSE,
                               8, biw, bih, biw*3, destroy_pb_data, NULL);

    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb2);
    return FALSE;
  }
  return TRUE;
}
