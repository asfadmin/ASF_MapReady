#include "winshl.h"
#include "asf_view.h"

// have to use static vars... actually, we probably should be using
// the "user_data" stuff that is always being passed around
static int big_image_drag = FALSE;
static int planner_big_image_drag = FALSE;
static int small_image_drag = FALSE;
static int start_x=0, start_y=0;
static GdkPixbuf *pb=NULL;
static GdkPixbuf *pb2=NULL;
static GtkWidget *img=NULL;
static GtkWidget *win=NULL;
extern GdkPixbuf *pixbuf_small;
extern int small_image_x_dim;
extern int small_image_y_dim;

int ran_cb_callback=FALSE;

#ifndef win32
static GdkCursor *pan_cursor=NULL;
#endif

static double max2(double a, double b)
{
  return a>b ? a : b;
}

static double min2(double a, double b)
{
  return a<b ? a : b;
}

static double max4(double a, double b, double c, double d)
{
  return max2(max2(a,b),max2(c,d));
}

static double min4(double a, double b, double c, double d)
{
  return min2(min2(a,b),min2(c,d));
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

// kludges to allow handling button clicks in the release event
static int small_image_clicked=FALSE;
static int big_image_clicked=FALSE;
static int ran_nb_callback=FALSE;

SIGNAL_CALLBACK int
on_small_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
  //printf("Small image button press\n");
  small_image_clicked=TRUE;
  big_image_clicked=FALSE;
  return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
  //printf("Big image button press\n");
  big_image_clicked=TRUE;
  small_image_clicked=FALSE;
  return TRUE;
}

SIGNAL_CALLBACK int
on_planner_notebook_switch_page(GtkNotebook *nb, GtkNotebookPage *pg,
                                gint num, gpointer user_data)
{
  ran_nb_callback=TRUE;
  return TRUE;
}

void clear_nb_callback()
{
  ran_nb_callback=FALSE;
  big_image_clicked=small_image_clicked=FALSE;
}

// called when the user releases button -- must ensure user was panning
// before doing anything.  If not, event has to be passed on as a normal
// button click, & we have kludgey static vars to help figure out which
// widget the click applies to
SIGNAL_CALLBACK int
on_button_release_event(GtkWidget *w, GdkEventButton *event, gpointer data)
{
  int x = (int) event->x;
  int y = (int) event->y;

  //printf("Release event! Small:%s Big:%s\n",
  //       small_image_clicked ? "TRUE" : "FALSE",
  //       big_image_clicked ? "TRUE" : "FALSE");

  // kludge to prevent clicking on the checkboxes, or switching notebook
  // tabs, triggering the re-centering/etc code
  if (ran_cb_callback || ran_nb_callback) {
    ran_cb_callback=ran_nb_callback=FALSE;
    return FALSE;
  }

  //printf("  --> %d %d\n", x, y);

  if (big_image_drag) {
    big_image_drag = FALSE;

    if (planner_big_image_drag) {
      planner_big_image_drag = FALSE;

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

      double lat1, lat2, lat3, lat4, lon1, lon2, lon3, lon4;
      meta_get_latLon(curr->meta, l1, s1, 0, &lat1, &lon1);
      meta_get_latLon(curr->meta, l1, s2, 0, &lat2, &lon2);
      meta_get_latLon(curr->meta, l2, s2, 0, &lat3, &lon3);
      meta_get_latLon(curr->meta, l2, s1, 0, &lat4, &lon4);

      double lat_min = min4(lat1,lat2,lat3,lat4);
      double lat_max = max4(lat1,lat2,lat3,lat4);

      double lon_min = min4(lon1,lon2,lon3,lon4);
      double lon_max = max4(lon1,lon2,lon3,lon4);

      put_double_to_entry_fmt("lat_min_entry", lat_min, "%.2f");
      put_double_to_entry_fmt("lat_max_entry", lat_max, "%.2f");

      put_double_to_entry_fmt("lon_min_entry", lon_min, "%.2f");
      put_double_to_entry_fmt("lon_max_entry", lon_max, "%.2f");

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

  if (small_image_drag) {
    small_image_drag = FALSE;

    int minx = MIN(start_x, x);
    int maxx = MAX(start_x, x);
    int miny = MIN(start_y, y);
    int maxy = MAX(start_y, y);

    int width = gdk_pixbuf_get_width(pb)-1;
    int height = gdk_pixbuf_get_height(pb)-1;

    if (minx < 0) minx = 0;
    if (maxx > width) maxx = width;
    if (miny < 0) miny = 0;
    if (maxy > height) maxy = height;

    double w = (double)(maxx-minx)/(double)small_image_x_dim * curr->ns;
    double z1 = w/(double)get_big_image_width();

    double h = (double)(maxy-miny)/(double)small_image_y_dim * curr->nl;
    double z2 = h/(double)get_big_image_height();

    center_line = ((double)(maxy+miny))/(double)small_image_y_dim*curr->nl/2.;
    center_samp = ((double)(maxx+minx))/(double)small_image_x_dim*curr->ns/2.;

    // This check is here to handle the case where the user clicks in the
    // image, but the window manager interpreted it as a click&drag.
    // They'd get a super-zoomed-in image that is kind of confusing if you
    // are not expecting it.  Skipping update_zoom() will handle it like
    // a regular left-click.
    if (maxx-minx > 2 || maxy-miny > 2) {
      zoom = z1 > z2 ? z1 : z2;
      update_zoom();
    }

    fill_small(curr);
    fill_big(curr);

    win = img = NULL;
    return TRUE;
  }

  if (big_image_clicked) {
    big_image_clicked=FALSE;
    big_clicked(event);
  }
  else if (small_image_clicked) {
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

#define DEBUG_STATE 0
static void print_state(int state)
{
  char str[256];
  strcpy(str, "");

  if (state & GDK_SHIFT_MASK)
    strcat(str, "Shift ");
  if (state & GDK_LOCK_MASK)
    strcat(str, "Lock ");
  if (state & GDK_CONTROL_MASK)
    strcat(str, "Ctrl ");
  if (state & GDK_MOD1_MASK)
    strcat(str, "Alt ");
  if (state & GDK_MOD2_MASK)
    strcat(str, "Mod2 ");
  if (state & GDK_MOD3_MASK)
    strcat(str, "Mod3 ");
  if (state & GDK_MOD4_MASK)
    strcat(str, "Mod4 ");
  if (state & GDK_MOD5_MASK)
    strcat(str, "Mod5 ");
  if (state & GDK_BUTTON1_MASK)
    strcat(str, "Button1 ");
  if (state & GDK_BUTTON2_MASK)
    strcat(str, "Button2 ");
  if (state & GDK_BUTTON3_MASK)
    strcat(str, "Button3 ");
  if (state & GDK_BUTTON4_MASK)
    strcat(str, "Button4 ");
  if (state & GDK_BUTTON5_MASK)
    strcat(str, "Button5 ");
  if (state & GDK_RELEASE_MASK)
    strcat(str, "RELEASE ");

  printf("State: %s\n", str);
}

static GdkWindow *big_image_ptr = NULL;

// called when user is panning (big_image_drag)
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

  if (DEBUG_STATE)
    print_state(state);

  // in normal asf view, left-click&drag is a pan operation, but in
  // the planner, right-click&drag is a pan operation, and left-click&drag
  // draws a box
  if ((state & GDK_BUTTON1_MASK) ||
      (planner_is_active() && (state & GDK_BUTTON3_MASK))) {

    // motion while Button 1 is pressed!
    x = (int) event->x;
    y = (int) event->y;

    // in planning mode, we replace panning with dragging, to create
    // a rectangle in the main window.  User pans with ctrl-click&drag
    if (!small_image_drag && (big_image_clicked || big_image_drag)) {
        // This is motion in the MAIN window
        // i.e. -- a panning operation

      if (!big_image_drag) {
        // user just started panning
        start_x = x;
        start_y = y;
        big_image_drag = TRUE;

        if (planner_is_active() && (state & GDK_BUTTON1_MASK))
          planner_big_image_drag = TRUE;

        if (!win)
          win = get_widget_checked("ssv_main_window");

        if (!img)
          img = get_widget_checked("big_image");

        pb = gtk_image_get_pixbuf(GTK_IMAGE(img));

        if (!planner_big_image_drag) {
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

      if (planner_big_image_drag) {
        // planner mode: just copy the pixels over, so pb2 is the same
        // as pb.  Then we will add the user-dragged red box
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
        // (horizontally off would blow up the memcpys)
        if (off_x > biw || off_x < -biw) {
          // image is all black! no action needed
          ;
        }

        // 4 cases: user has panned up&left, up&right, down&left, down&right
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
    else if (small_image_clicked || (!big_image_drag && small_image_drag))
    {
      // This is motion in the THUMBNAIL window
      // i.e. -- a drag operation to create a new zoom level

      if (!small_image_drag) {
        // user just started dragging
        start_x = x;
        start_y = y;
        small_image_drag = TRUE;

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

      int width = gdk_pixbuf_get_width(pb)-1;
      int height = gdk_pixbuf_get_height(pb)-1;

      if (minx < 0) minx = 0;
      if (maxx > width) maxx = width;
      if (miny < 0) miny = 0;
      if (maxy > height) maxy = height;

      put_box(pb, minx, maxx, miny, maxy);
      gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
      return FALSE;
    }
    else {
      // Hmph.
      // 
      // Commented this out ...it creates annoying error messages 
      // when you click and drag silly things like tab labels in the GUI
      //printf("Unknown drag event: %s %d %d %f %f\n",
          //gtk_widget_get_name(widget), x, y, event->x_root, event->y_root);
    }
  }
  else if (big_image_ptr && big_image_ptr == event->window) {
    x = (int) event->x;
    y = (int) event->y;

    double line, samp;
    img2ls(x, y, &line, &samp);

    if (meta_supports_meta_get_latLon(curr->meta)) {
      char buf[128];
      double lat, lon;
      meta_get_latLon(curr->meta, line, samp, 0, &lat, &lon);
      sprintf(buf, "Lat: %7.3f, Lon: %7.3f", lat, lon);
      put_string_to_label("motion_label", buf);
    }
  }

  return TRUE;
}

void setup_gdk_window_ids()
{
  GtkWidget *w = get_widget_checked("big_image");
  big_image_ptr = w->window;
}
