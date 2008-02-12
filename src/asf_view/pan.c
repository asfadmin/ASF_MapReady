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
static int panning = FALSE;
static int planner_panning = FALSE;
static int dragging = FALSE;
static int start_x=0, start_y=0;
static GdkPixbuf *pb=NULL;
static GdkPixbuf *pb2=NULL;
static GtkWidget *img=NULL;
static GtkWidget *win=NULL;
extern GdkPixbuf *pixbuf_small;
extern int small_image_x_dim;
extern int small_image_y_dim;

int ran_cb_callback=FALSE;
static int ran_nb_callback=FALSE;

#ifndef win32
static GdkCursor *pan_cursor=NULL;
#endif

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

static int small_image_clicked=FALSE;
static int big_image_clicked=FALSE;

SIGNAL_CALLBACK int
on_small_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
  small_image_clicked=TRUE;
  return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
  big_image_clicked=TRUE;
  return TRUE;
}

SIGNAL_CALLBACK int on_planner_notebook_switch_page(GtkNotebook *nb,
                                                    GtkNotebookPage *pg,
                                                    gint num,
                                                    gpointer user_data)
{
  ran_nb_callback=TRUE;
  return TRUE;
}

// called when the user releases button -- must ensure user was panning
// before doing anything.  If not, event has to be passed on as a normal
// button click
SIGNAL_CALLBACK int
on_button_release_event(GtkWidget *w, GdkEventButton *event, gpointer data)
{
  int x = (int) event->x;
  int y = (int) event->y;

  // kludge to prevent clicking on the checkboxes, or switching notebook
  // tabs, triggering the re-centering/etc code
  if (ran_cb_callback || ran_nb_callback) {
    ran_cb_callback=ran_nb_callback=FALSE;
    return FALSE;
  }

  //printf("  --> %d %d\n", x, y);

  if (panning) {
    panning = FALSE;

    if (planner_panning) {
      planner_panning = FALSE;

      double l1, l2, s1, s2;
      img2ls(start_x,start_y,&l1,&s1);
      img2ls(x,y,&l2,&s2);

      crosshair_line = l1;
      crosshair_samp = s1;

      g_polys[0].line[0] = l1;
      g_polys[0].samp[0] = s2;

      g_polys[0].line[1] = l2;
      g_polys[0].samp[1] = s2;

      g_polys[0].line[2] = l2;
      g_polys[0].samp[2] = s1;

      g_polys[0].line[3] = l1;
      g_polys[0].samp[3] = s1;

      g_polys[0].n = 4;
      g_polys[0].c = 3;

      update_pixel_info(curr);
    }
    else {
      center_line -= zoom*(y-start_y);
      center_samp -= zoom*(x-start_x);

#ifdef win32
      //SetCursor(LoadCursor(NULL,IDC_HAND));
#else
      gdk_window_set_cursor(GDK_WINDOW(win->window), NULL);
#endif
    }

    gdk_pixbuf_unref(pb);
    fill_small(curr);
    fill_big(curr);

    win = img = NULL;
    return TRUE;
  }

  if (dragging) {
    dragging = FALSE;

    int minx = MIN(start_x, x);
    int maxx = MAX(start_x, x);
    int miny = MIN(start_y, y);
    int maxy = MAX(start_y, y);

    //int width = gdk_pixbuf_get_width(pb);
    //int height = gdk_pixbuf_get_height(pb);

    double w = (double)(maxx-minx)/(double)small_image_x_dim * curr->ns;
    double z1 = w/(double)get_big_image_width();

    double h = (double)(maxy-miny)/(double)small_image_y_dim * curr->nl;
    double z2 = h/(double)get_big_image_height();

    //double z1 = (double)(curr->nl)/h;
    //double z2 = (double)(curr->ns)/w;
    zoom = z1 > z2 ? z1 : z2;

    center_line = ((double)(maxy+miny))/(double)small_image_y_dim*curr->nl/2.;
    center_samp = ((double)(maxx+minx))/(double)small_image_x_dim*curr->ns/2.;

    update_zoom();
    fill_small(curr);
    fill_big(curr);

    win = img = NULL;
    return TRUE;
  }

  if (big_image_clicked) { //event->x > 256)
    big_image_clicked=FALSE;
    big_clicked(event);
  }
  else if (small_image_clicked) { //event->y < 256)
    small_image_clicked=FALSE;
    small_clicked(event);
  }

  return FALSE;
}

static void put_box(GdkPixbuf *pixbuf, int x1, int x2, int y1, int y2)
{
    int i, width, height, rowstride, n_channels;
    guchar *pixels, *p;

    n_channels = gdk_pixbuf_get_n_channels (pixbuf);

    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);

    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    if (x1 < 0) x1 = 0;
    if (x1 >= width) x1 = width;

    if (x2 < 0) x2 = 0;
    if (x2 >= width) x2 = width;

    if (y1 < 0) y1 = 0;
    if (y1 >= height) y1 = height;

    if (y2 < 0) y2 = 0;
    if (y2 >= height) y2 = height;

    for (i=x1; i<=x2; ++i) {
        p = pixels + y1*rowstride + i*n_channels;
        p[0]=255;
        p[1]=p[2]=0;
        p = pixels + y2*rowstride + i*n_channels;
        p[0]=255;
        p[1]=p[2]=0;
    }

    for (i=y1+1; i<y2; ++i) {
        p = pixels + i*rowstride + x1*n_channels;
        p[0]=255;
        p[1]=p[2]=0;
        p = pixels + i*rowstride + x2*n_channels;
        p[0]=255;
        p[1]=p[2]=0;
    }
}

// called when user is panning
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

    // in planning mode, we replace panning with dragging, to create
    // a rectangle in the main window.  User pans with ctrl-click&drag
    if (!dragging && (event->x_root > 256 || panning)) {
        // This is motion in the MAIN window
        // i.e. -- a panning operation

      if (!panning) {
        // user just started panning
        start_x = x;
        start_y = y;
        panning = TRUE;
        
        if (planner_is_active() && !(state & GDK_CONTROL_MASK))
          planner_panning = TRUE;

        if (!win)
          win = get_widget_checked("ssv_main_window");
        
        if (!img)
          img = get_widget_checked("big_image");
        
        pb = gtk_image_get_pixbuf(GTK_IMAGE(img));
        
        if (!planner_panning) {
#ifdef win32
          SetCursor(LoadCursor(NULL,IDC_HAND));
#else
          if (!pan_cursor)
            pan_cursor = gdk_cursor_new(GDK_FLEUR);
        
          gdk_window_set_cursor(GDK_WINDOW(win->window), pan_cursor);
#endif
        }
      }
      
      assert(pb);
      int ii;
      int off_x = x-start_x;
      int off_y = y-start_y;

      // put the modified image in a new pixbuf
      int nchan = 3;
      int biw = get_big_image_width();
      int bih = get_big_image_height();
      unsigned char *bdata = CALLOC(sizeof(unsigned char), biw*bih*nchan);

      // we refer to "pb" here -- the pixbuf that was being shown before
      // panning -- instead of panning event-to-event (i.e., using "pb2")
      int rowstride = gdk_pixbuf_get_rowstride(pb);
      unsigned char *pixels = gdk_pixbuf_get_pixels(pb);

      if (pb2)
        gdk_pixbuf_unref(pb2);

      if (planner_panning) {
        memcpy(bdata, pixels, biw*bih*3);
        pb2 = 
          gdk_pixbuf_new_from_data(bdata, GDK_COLORSPACE_RGB, FALSE,
                                   8, biw, bih, biw*3, destroy_pb_data, NULL);

        int minx = MIN(start_x, x);
        int maxx = MAX(start_x, x);
        int miny = MIN(start_y, y);
        int maxy = MAX(start_y, y);

        put_box(pb2, minx, maxx, miny, maxy);
      }
      else {
        //printf ("Button 1 motion  (%d, %d)\n", off_x, off_y);
                
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

        pb2 = 
          gdk_pixbuf_new_from_data(bdata, GDK_COLORSPACE_RGB, FALSE,
                                   8, biw, bih, biw*3, destroy_pb_data, NULL);
      }
                
      gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb2);
      return FALSE;
    }
    else if ((event->x_root <= 256 && event->y_root <= 256) || 
             (!panning && dragging))
    {
      // This is motion in the THUMBNAIL window
      // i.e. -- a drag operation, to create a new zoom level

      if (!dragging) {
        // user just started dragging
        start_x = x;
        start_y = y;
        dragging = TRUE;

        if (!win)
          win = get_widget_checked("ssv_main_window");
        
        if (!img)
          img = get_widget_checked("small_image");
      }

      pb = gdk_pixbuf_copy(pixbuf_small);

      int minx = MIN(start_x, x);
      int maxx = MAX(start_x, x);
      int miny = MIN(start_y, y);
      int maxy = MAX(start_y, y);

      put_box(pb, minx, maxx, miny, maxy);
      gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
      return FALSE;
    }
    else {
      // Hmph.
      printf("Unknown drag event: %s %d %d %f %f\n",
             gtk_widget_get_name(widget), x, y, event->x_root, event->y_root);
    }
  }
  return TRUE;
}
