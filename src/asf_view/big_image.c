#include "asf_view.h"
#include <gdk/gdkkeysyms.h>
#include "libasf_proj.h"

UserPolygon g_polys[MAX_POLYS];

// pointer to the currently active one
UserPolygon *g_poly;
int which_poly=0;

// current sizes of the large image.
// keep half the size around, we need that during the redraw, which
// is supposed to be quick
static int big_img_width=800;
static int big_img_height=800;
static int big_img_width2=400;
static int big_img_height2=400;

int get_big_image_width()
{
    return big_img_width;
}

int get_big_image_height()
{
    return big_img_height;
}

int get_big_image_width2()
{
    return big_img_width2;
}

int get_big_image_height2()
{
    return big_img_height2;
}

SIGNAL_CALLBACK void on_big_image_resize(GtkWidget *w, 
    GtkAllocation *alloc, gpointer user_data)
{
    // User resized!
    // Recalculate the width/height and half-width/half-height values
    // This if statement just prevents doing the redraw if the user didn't
    // actually resize (i.e., just grabbed the edge but didn't drag it).
    // (It also is useful to stop an infinate cascade-of-resizes that GTK
    // sometimes seems to get stuck in -- the "fill_small/big" will
    // occasionally trigger a resize that runs after this method completes)
    if (big_img_width != alloc->width || big_img_height != alloc->height)
    {
        big_img_width = alloc->width;
        big_img_height = alloc->height;
        big_img_width2 = big_img_width/2;
        big_img_height2 = big_img_height/2;

        fill_small(curr);
        fill_big(curr);
    }
}

// I am not sure this is even necessary
SIGNAL_CALLBACK void on_big_image_repaint(GtkWidget *w)
{
    fill_small(curr);
    fill_big(curr);
}

static void ls2img(double line, double samp, int *x, int *y)
{
    // convert from line/sample coordinates (SAR image coordinates)
    // to screen coordinates
    *x = (samp - (double)center_samp)/zoom + get_big_image_width2();
    *y = (line - (double)center_line)/zoom + get_big_image_height2();
}

void img2ls(int x, int y, double *line, double *samp)
{
    // convert from screen coordinates to line/sample coordinates
    *line = ((double)y - get_big_image_height2())*zoom + (double)center_line;
    *samp = ((double)x - get_big_image_width2())*zoom + (double)center_samp;
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

static void show_or_hide_save_subset_button()
{
    // we show it when there is a user polygon defined
    // that's when saving a subset would make sense
    //show_widget("save_subset_button", g_poly->n > 0);
    GtkWidget * w = get_widget_checked("save_subset_button");
    gtk_widget_set_sensitive(w, g_poly->n);
}

// draws a crosshair at x,y (image coords)
static void put_crosshair (GdkPixbuf *pixbuf, double line, double samp,
                           int green, ImageInfo *ii)
{
    if (samp < 0 || line < 0 || samp >= ii->ns || line >= ii->nl)
        return;

    int i, lo, hi;
    int width, height, rowstride, n_channels;
    guchar *pixels, *p;
    
    n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    
    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);

    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);
    
    // convert from image coords to screen coords
    int ix, iy;
    ls2img(line, samp, &ix, &iy);

    if (ix <= 0 || ix > width || iy <= 0 || iy > height)
        return;

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    // 15 is the size of the crosshair in each direction (30 pixels total)
    // these are screen pixels, so the crosshair is the same for all zooms
    lo = ix - 15;   if (lo < 0)      lo = 0;
    hi = ix + 15;   if (hi >= width) hi = width-1;

    // crosshair is drawn in red, unless "green" was set
    int r = 255;
    int g = 0;
    int b = 0;
    if (green) {
        r = b = 0;
        g = 255;
    }

    for (i = lo; i < hi; ++i) {
        if (i > ix-3 && i < ix+3) i = ix+3;
        p = pixels + iy * rowstride + i * n_channels;
        p[0] = r;
        p[1] = g;
        p[2] = b;
    }

    lo = iy - 15;   if (lo < 0)       lo = 0;
    hi = iy + 15;   if (hi >= height) hi = height-1;

    for (i = lo; i < hi; ++i) {
        if (i > iy-3 && i < iy+3) i = iy+3;
        p = pixels + i * rowstride + ix * n_channels;
        p[0] = r;
        p[1] = g;
        p[2] = b;
    }
}

static int iabs(int i)
{
    return i<0 ? -i : i;
}

// colors supported by put_line
#define RED 1
#define PURPLE 2
#define BLUE 3

void get_color(int color, unsigned char *r, unsigned char *g,
               unsigned char *b)
{
    switch (color) {
      case RED:
        *r = 255;
        *g = *b = 0;
        break;

      case PURPLE: 
      case 10: // PURPLE
        *r = *b = 255;
        *g = 0;
        break;

      case 11: // OLIVE GREEN
        *r = 202;
        *g = 255;
        *b = 112;
        break;

      case BLUE:
      case 12: // BLUE
        *b = 255;
        *r = *g = 0;
        break;

      case 13: // TURQUOISE
        *r = 0;
        *g = 245;
        *b = 255;
        break;
         
      case 14: // ORANGE
        *r = 255;
        *g = 165;
        *b = 0;
        break;

      case 15: // PINK
        *r = 255;
        *g = 192;
        *b = 203;
        break;
        
      case 16: // GOLD
        *r = 255;
        *g = 215;
        *b = 0;
        break;
        
      case 17: // GREEN
        *r = 0;
        *g = 205;
        *b = 0;
        break;
        
      case 18: // INDIAN RED
        *r = 255;
        *g = 106;
        *b = 106;
        break;
        
      case 19: // MEDIUM ORCHID
        *r = 180;
        *g = 82;
        *b = 205;
        break;
                
      case 20: // CHARTREUSE
        *r = 127;
        *g = 255;
        *b = 0;
        break;

      case 21: // KHAKI
        *r = 240;
        *g = 230;
        *b = 140;
        break;
        
      case 22: // SANDY BROWN
        *r = 244;
        *g = 164;
        *b = 96;
        break;
        
      case 23: // TOMATO
        *r = 255;
        *g = 99;
        *b = 71;
        break;
        
      case 24: // AZURE
        *r = 193;
        *g = 205;
        *b = 205;
        break;
        
      case 25: // SKY BLUE
        *r = 30;
        *g = 144;
        *b = 255;
        break;
        
      case 26: // SPRING GREEN
        *r = 0;
        *g = 255;
        *b = 127;
        break;
                
      case 27: // DARK SLATE GREY
        *r = 47;
        *g = 79;
        *b = 79;
        break;

      case 28: // BISQUE
        *r = 255;
        *g = 228;
        *b = 196;
        break;

      case 29: // MEDIUM VIOLET RED
        *r = 199;
        *g = 21;
        *b = 133;
        break;

      case 30: // DARK OLIVE GREEN
        *r = 202;
        *g = 255;
        *b = 112;
        break;

      case 31: // HONEYDEW
        *r = 240;
        *g = 255;
        *b = 240;
        break;

      case 32: // LAVENDER
        *r = 230;
        *g = 230;
        *b = 250;
        break;

      case 33: // NAVY BLUE
        *r = 0;
        *g = 0;
        *b = 128;
        break;

      case 34: // DARK ORCHID
        *r = 153;
        *g = 50;
        *b = 204;
        break;

      case 35: // DEEP SKY BLUE
        *r = 0;
        *g = 191;
        *b = 255;
        break;

      case 36: // ROSY BROWN
        *r = 255;
        *g = 193;
        *b = 193;
        break;

      default:
        // this shouldn't happen
        assert(0);
        *r = *g = *b = 0;
        break;
    }
}

static void put_line(GdkPixbuf *pixbuf, double line0, double samp0, 
                     double line1, double samp1, int color,
                     ImageInfo *ii)
{
  if (fabs(samp0-samp1)>20000) return;
  //if (samp0 < 0 || line0 < 0 || samp1 < 0 || line1 < 0 ||
  //      samp0 >= ii->ns || samp1 >= ii->ns ||
  //      line0 >= ii->nl || line1 >= ii->nl) return;

    int i, j, width, height, rowstride, n_channels;
    guchar *pixels, *p;

    n_channels = gdk_pixbuf_get_n_channels (pixbuf);

    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);

    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);

    // convert from image coords to screen coords
    int ix0, iy0, ix1, iy1;
    ls2img(line0, samp0, &ix0, &iy0);
    ls2img(line1, samp1, &ix1, &iy1);

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    // color of the drawn line
    unsigned char r, g, b;
    get_color(color, &r, &g, &b);

    // What a mess!  But the concept is trivial:
    //   (1) Loop goes on the x or y
    //   (2) linearly interpolate to find the other coordinate
    //   (3) place that pixel
    // i,j is the interpolated sample,line where the pixel goes
    // we loop on whichever distance (x or y) is greater
    if (iabs(ix1-ix0) > iabs(iy1-iy0)) {
        int incr = ix1>ix0 ? 1 : -1;
        for (i=ix0; i!=ix1; i+=incr) {
            j = iy0 + (float)(i-ix0)/(ix1-ix0) * (iy1-iy0);
            if (j >= 0 && i >= 0 && i <= width && j <= height) {
              p = pixels + j * rowstride + i * n_channels;
              p[0] = r; p[1] = g; p[2] = b;
            }
        }
    } else {
        int incr = iy1>iy0 ? 1 : -1;
        for (j=iy0; j!=iy1; j+=incr) {
            i = ix0 + (float)(j-iy0)/(iy1-iy0) * (ix1-ix0);
            if (j >= 0 && i >= 0 && i <= width && j <= height) {
              p = pixels + j * rowstride + i * n_channels;
              p[0] = r; p[1] = g; p[2] = b;
            }
        }
    }
}

static GdkPixbuf * make_big_image(ImageInfo *ii)
{
    assert(ii->data_ci);
    assert(ii->meta);

    int i, j, k;
    int nchan = 3; // RGB for now, don't support RGBA yet
    int biw = get_big_image_width();
    int bih = get_big_image_height();
    unsigned char *bdata = MALLOC(sizeof(unsigned char)*biw*bih*nchan);
    int background_red=0, background_blue=0, background_green=0;

    // kludge! When showing the startup image, change the background
    // to match the background of that image.  If we ever change the
    // startup image, this is going to look pretty ugly...
    int is_startup = strstr(curr->filename, "startup.jpg") != NULL;
    if (is_startup) {
      background_red = 43;
      background_green = 77;
      background_blue = 138;
    }

    int mm = 0;
    for (i=0; i<bih; ++i) {
        for (j=0; j<biw; ++j) {
            double l, s;
            img2ls(j,i,&l,&s);

            unsigned char r, g, b;

            if (l<0 || l>=ii->nl || s<0 || s>=ii->ns) {
                r = background_red;
                g = background_green;
                b = background_blue;
            } else {
                cached_image_get_rgb(ii->data_ci, (int)floor(l),
                    (int)floor(s), &r, &g, &b);
            }

            int n = 3*mm;
            bdata[n] = r;
            bdata[n+1] = g;
            bdata[n+2] = b;
            ++mm;
        }
    }

    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(bdata, GDK_COLORSPACE_RGB, FALSE, 
                                 8, biw, bih, biw*3, destroy_pb_data, NULL);

    if (!pb)
        asfPrintError("Failed to create the larger pixbuf.\n");

    // put the red crosshair at the "active" point along the polygon
    if (g_poly->c < g_poly->n)
        put_crosshair(pb, g_poly->line[g_poly->c], g_poly->samp[g_poly->c],
            FALSE, ii);

    // draw old polygons
    for (k=0; k<MAX_POLYS; ++k) {
      if (g_polys[k].n > 0 && row_is_checked(k)) {
        for (i=0; i<g_polys[k].n-1; ++i) {
            put_line(pb, g_polys[k].line[i], g_polys[k].samp[i],
                g_polys[k].line[i+1], g_polys[k].samp[i+1], 10+k, ii);
        }
      }
    }

    // draw the polygon
    if (g_poly->n > 0) {
        put_line(pb, crosshair_line, crosshair_samp,
            g_poly->line[0], g_poly->samp[0], RED, ii);
        for (i=0; i<g_poly->n-1; ++i) {
            put_line(pb, g_poly->line[i], g_poly->samp[i],
                g_poly->line[i+1], g_poly->samp[i+1], RED, ii);
        }
    } else if (g_poly->show_extent) {
        // no polygon -- close "save subset" window, if open
        show_widget("save_subset_window", FALSE);
        g_poly->show_extent = FALSE;
    }

    // green crosshair goes second, so if the two overlap, we will see
    // the green one (the main one)
    put_crosshair(pb, crosshair_line, crosshair_samp, TRUE, ii);

    // draw bounding box if requested
    if (g_poly->show_extent) {
        update_poly_extents(ii->meta);
        put_line(pb, g_poly->extent_y_min, g_poly->extent_x_min,
                     g_poly->extent_y_max, g_poly->extent_x_min, PURPLE, ii);
        put_line(pb, g_poly->extent_y_max, g_poly->extent_x_min,
                     g_poly->extent_y_max, g_poly->extent_x_max, PURPLE, ii);
        put_line(pb, g_poly->extent_y_max, g_poly->extent_x_max,
                     g_poly->extent_y_min, g_poly->extent_x_max, PURPLE, ii);
        put_line(pb, g_poly->extent_y_min, g_poly->extent_x_max,
                     g_poly->extent_y_min, g_poly->extent_x_min, PURPLE, ii);
    }

    return pb;
}

void fill_big(ImageInfo *ii)
{
    GdkPixbuf *pb = make_big_image(ii);
    GtkWidget *img = get_widget_checked("big_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
    //g_object_unref(pb);

    // might as well do this here
    show_or_hide_save_subset_button();
}

void small_clicked(GdkEventButton *event)
{
    // clicking in the small image moves the big image
    GtkWidget *img = get_widget_checked("small_image");
    GdkPixbuf *pb = gtk_image_get_pixbuf(GTK_IMAGE(img));

    int w = gdk_pixbuf_get_width(pb);
    int h = gdk_pixbuf_get_height(pb);

    center_samp = ((int)event->x * curr->ns) / (double)w;
    center_line = ((int)event->y * curr->nl) / (double)h;

    fill_small(curr);
    fill_big(curr);
}

// Keeps track of which crosshair should be affected when the user
// presses an arrow key.
static int last_was_crosshair = TRUE;

void big_clicked(GdkEventButton *event)
{
    if (event->button == 1) {
        // ctrl-left-click: measure distance
        if (((int)event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) {
            if (g_poly->n < MAX_POLY_LEN) {
                double l, s;
                img2ls((int)event->x, (int)event->y, &l, &s);
                g_poly->line[g_poly->n] = l;
                g_poly->samp[g_poly->n] = s;
                ++g_poly->n;
                if (g_poly->n == 1)
                    g_poly->c = 0;
                else
                    g_poly->c = g_poly->n-1;
            } else {
                asfPrintWarning("Exceeded maximum polygon length.\n"
                                "No more points can be added.\n");
            }
            last_was_crosshair = FALSE;
        }
        // left-click: move crosshair
        else {
            img2ls((int)event->x, (int)event->y, &crosshair_line, &crosshair_samp);
            last_was_crosshair = TRUE;
        }
        update_pixel_info(curr);
        fill_big(curr);
    } else if (event->button == 3) {
        // right-click: re-center
        img2ls((int)event->x, (int)event->y, &center_line, &center_samp);
        fill_small(curr);
        fill_big(curr);
    }
}

void update_zoom()
{
   char buf[256];
   if (zoom >= 1)
       sprintf(buf, "Zoom: %.1f X", zoom);
   else if (zoom > .1)
       sprintf(buf, "Zoom: %.2f X", zoom);
   else if (zoom >= .01)
       sprintf(buf, "Zoom: %.3f X", zoom);
   else if (zoom >= .001)
       sprintf(buf, "Zoom: %.4f X", zoom);
   else if (zoom >= .0001)
       sprintf(buf, "Zoom: %.5f X", zoom);
   else
       sprintf(buf, "Zoom: %f X", zoom);
   put_string_to_label("zoom_label", buf);
}

static void zoom_in(ImageInfo *ii)
{
    // zooming in when larger than 1:1 decreases by 1 each time
    // until 1:1, then we halve the zoom factor
    if (zoom > 1)
        zoom = (int)(zoom+.98) - 1;
    else // zoom <= 1
        zoom /= 2;

    update_zoom();
    fill_small(ii);
}

static void zoom_out(ImageInfo *ii)
{
    // zooming out when larger than 1:1 increases by 1 each time
    // when less than 1:1, then we double the zoom factor
    if (zoom <= 1)
        zoom *= 2;
    else // zoom > 1
        zoom = (int)zoom + 1;

    update_zoom();
    fill_small(ii);
}

static void zoom_default(ImageInfo *ii)
{
    zoom = 1;
    update_zoom();
    fill_small(ii);
}

SIGNAL_CALLBACK int on_big_image_scroll_event(
    GtkWidget *widget, GdkEventScroll *event, gpointer user_data)
{
    if (event->direction == GDK_SCROLL_UP) {
        zoom_in(curr);
    } else if (event->direction == GDK_SCROLL_DOWN) {
        zoom_out(curr);
    }

    fill_big(curr);
    return TRUE;
}

static int has_focus(const char *widget_name)
{
  GtkWidget *w = get_widget_checked(widget_name);
  return GTK_WIDGET_HAS_FOCUS(w);
}

static int handle_keypress(GdkEventKey *event, ImageInfo *ii)
{
    // This here is a kludge.  The main window's image "big_image" and
    // its event box "big_image_eventbox" don't seem to be properly
    // receiving keyboard event notifications... something I am doing wrong,
    // probably, but I can't figure it out.  So, we have this kludge --
    // this event fires when the main window receives a keyboard event
    // (these events work fine), and processes the event at that level.
    // (In fact, this is almost better - user doesn't have to click in
    // the big image to start using keyboard commands...)
    // However, any other widget that wants to receive keyboard events
    // needs to be listed here, so that the main window can pass along
    // the keypresses when the user is trying to use those widgets.
    // Currently just have the date widgets of the acquisition planner.
    if ((has_focus("start_date_entry") ||
         has_focus("end_date_entry") ||
         has_focus("lat_min_entry") ||
         has_focus("lat_max_entry") ||
         has_focus("lon_min_entry") ||
         has_focus("lon_max_entry") ||
         has_focus("look_angle_entry") ||
         has_focus("show_box_button") ||
         has_focus("mode_combobox") ||
         has_focus("orbit_direction_combobox"))
        &&
        (event->keyval == GDK_1 ||
         event->keyval == GDK_2 ||
         event->keyval == GDK_3 ||
         event->keyval == GDK_4 ||
         event->keyval == GDK_5 ||
         event->keyval == GDK_6 ||
         event->keyval == GDK_7 ||
         event->keyval == GDK_8 ||
         event->keyval == GDK_9 ||
         event->keyval == GDK_0 ||
         event->keyval == GDK_KP_1 ||
         event->keyval == GDK_KP_2 ||
         event->keyval == GDK_KP_3 ||
         event->keyval == GDK_KP_4 ||
         event->keyval == GDK_KP_5 ||
         event->keyval == GDK_KP_6 ||
         event->keyval == GDK_KP_7 ||
         event->keyval == GDK_KP_8 ||
         event->keyval == GDK_KP_9 ||
         event->keyval == GDK_KP_0 ||
         event->keyval == GDK_period ||
         event->keyval == GDK_minus ||
         event->keyval == GDK_plus ||
         event->keyval == GDK_Tab ||
         event->keyval == GDK_Left ||
         event->keyval == GDK_Right ||
         event->keyval == GDK_End ||
         event->keyval == GDK_Home ||
         event->keyval == GDK_BackSpace ||
         event->keyval == GDK_Delete ||
         event->keyval == GDK_KP_Delete))
    {
        return FALSE;
    }

    if (has_focus("output_dir_entry") || 
        has_focus("output_file_entry") ||
        has_focus("max_days_entry"))
    {
        return FALSE;
    }
    // end of kludge

    // Now the rest of the code deals with the normal keyboard events,
    // that pertain to the main image.
    if (event->keyval == GDK_Page_Up || 
        event->keyval == GDK_Prior || 
        event->keyval == GDK_plus)
    {
        // Page Up or Plus: Zoom IN
        zoom_in(ii);
    }
    else if (event->keyval == GDK_Page_Down || 
        event->keyval == GDK_Next || 
        event->keyval == GDK_minus)
    {
        // Page Down or Minus: Zoom OUT
        zoom_out(ii);
    }
    else if (event->keyval == GDK_Home) {
        // Home: Revert to the normal zoom level
        zoom_default(ii);
    }
    else if (event->keyval == GDK_End) {
        // End: Fit image to window
        int h = get_big_image_height();
        int w = get_big_image_width();

        // choose the larger of the horiz/vert zoom
        double z1 = (double)(ii->nl)/(double)h;
        double z2 = (double)(ii->ns)/(double)w;
        zoom = z1 > z2 ? z1 : z2;

        // recenter the image as well
        center_line = (ii->nl)/2.;
        center_samp = (ii->ns)/2.;

        update_zoom();
        fill_small(ii);
    }
    else if (event->keyval == GDK_Tab) {
        // Tab key: Cycle between the crosshairs (which one is affected by
        // subsequent arrow movements) Shift- or ctrl-tab: other direction
        if (g_poly->n > 0) {
            if (event->state & GDK_SHIFT_MASK ||
                event->state & GDK_CONTROL_MASK)
            {
                if (g_poly->c == 0) {
                    last_was_crosshair = TRUE;
                    g_poly->c = -1;
                } else if (g_poly->c == -1) {
                    last_was_crosshair = FALSE;
                    g_poly->c = g_poly->n - 1;
                } else {
                    last_was_crosshair = FALSE;
                    --g_poly->c;
                }
            } else {
                if (g_poly->c == g_poly->n-1) {
                    last_was_crosshair = TRUE;
                    g_poly->c = -1;
                } else {
                    last_was_crosshair = FALSE;
                    ++g_poly->c;
                }
            }
        } else
            last_was_crosshair = TRUE;
    }
    else if (event->keyval == GDK_slash) {
        // /: clear most recently added point
        if (g_poly->n > 0) {
            --g_poly->n;
            if (g_poly->c >= g_poly->n)
                g_poly->c = g_poly->n-1;
            update_pixel_info(ii);
        }
    }
    else if (event->keyval == GDK_Escape) {
        // Escape: clear the ctrl-clicked path
        g_poly->n = g_poly->c = 0;
        update_pixel_info(ii);
    }
    else if (event->keyval == GDK_c) {
        // c: Center image view on crosshair
        if (event->state & GDK_CONTROL_MASK &&
            g_poly->n > 0 && g_poly->c >= 0)
        {
            center_line = g_poly->line[g_poly->c];
            center_samp = g_poly->samp[g_poly->c];
        }
        else {
            if (crosshair_line > 0 && crosshair_samp > 0) {
                center_line = crosshair_line;
                center_samp = crosshair_samp;
            } else {
                // I am not sure it is possible to get in here
                // we should always have a green crosshair
                center_line = crosshair_line = (ii->nl)/2.;
                center_samp = crosshair_samp = (ii->ns)/2.;
                last_was_crosshair = TRUE;
            }
        }
        fill_small(ii);
    }
    else if (event->keyval == GDK_g) {
        // g: open google earth
        open_google_earth();
        return TRUE;
    }
    else if (event->keyval == GDK_m) {
        // m: open metadata viewer
        open_mdv();
        return TRUE;
    }
    else if (event->keyval == GDK_s && event->state & GDK_CONTROL_MASK) {
        // ctrl-s: save subset
        if (g_poly->n > 0)
            save_subset(ii);
        return TRUE;
    }
    else if (event->keyval == GDK_l) {
        // l: move to a local maxima (30x30 pixel search area)
        //    if ctrl-l is clicked, search 300x300 area
        // affects the same crosshair that would be moved with
        // the arrow keys
        int line, samp;
        if (!last_was_crosshair && g_poly->c < g_poly->n) {
            line=g_poly->line[g_poly->c];
            samp=g_poly->samp[g_poly->c];
        } else {
            line=crosshair_line;
            samp=crosshair_samp;
        }
        int line_max=line, samp_max=samp;
        float max_val=-99999;
        int i,j,radius=15;
        if (event->state & GDK_CONTROL_MASK)
            radius*=10;
        for (i=line-radius; i<=line+radius; ++i) {
            for (j=samp-radius; j<=samp+radius; ++j) {
                if (i>=0 && i>=0 && i<ii->nl && j<ii->ns) {
                    float val = cached_image_get_pixel(ii->data_ci,i,j);
                    if (val>max_val) {
                        max_val = val;
                        line_max = i;
                        samp_max = j;
                    }
                }
            }
        }
        if (!last_was_crosshair && g_poly->c < g_poly->n) {
            g_poly->line[g_poly->c]=line_max;
            g_poly->samp[g_poly->c]=samp_max;
        } else {
            crosshair_line=line_max;
            crosshair_samp=samp_max;
        }
        update_pixel_info(ii);
    }
    else if (event->keyval == GDK_n) {
        // n: next polygon
        if (which_poly < MAX_POLYS-1) {
          int i;
          for (i=g_poly->n-1; i>=0; --i) {
            g_poly->line[i+1] = g_poly->line[i];
            g_poly->samp[i+1] = g_poly->samp[i];
          }
          g_poly->line[0] = crosshair_line;
          g_poly->samp[0] = crosshair_samp;
          ++g_poly->n;
          g_poly->c=0;
          
          ++which_poly;
          g_poly = &g_polys[which_poly];

          if (g_poly->n > 0) {
            crosshair_line = g_poly->line[0];
            crosshair_samp = g_poly->samp[0];
            for (i=0; i<g_poly->n-1; ++i) {
              g_poly->line[i] = g_poly->line[i+1];
              g_poly->samp[i] = g_poly->samp[i+1];
            }

            --g_poly->n;
            g_poly->c = g_poly->n-1;
            last_was_crosshair = TRUE;
          }
        } else {
          printf("No more polygons.\n");
        }
        update_pixel_info(ii);
    }
    else if (event->keyval == GDK_p) {
        // p: previous polygon
        if (which_poly > 0) {
          int i;
          for (i=g_poly->n-1; i>=0; --i) {
            g_poly->line[i+1] = g_poly->line[i];
            g_poly->samp[i+1] = g_poly->samp[i];
          }
          g_poly->line[0] = crosshair_line;
          g_poly->samp[0] = crosshair_samp;
          ++g_poly->n;
          g_poly->c=0;
          
          --which_poly;
          g_poly = &g_polys[which_poly];

          if (g_poly->n > 0) {
            crosshair_line = g_poly->line[0];
            crosshair_samp = g_poly->samp[0];
            for (i=0; i<g_poly->n-1; ++i) {
              g_poly->line[i] = g_poly->line[i+1];
              g_poly->samp[i] = g_poly->samp[i+1];
            }

            --g_poly->n;
            g_poly->c = g_poly->n-1;
            last_was_crosshair = TRUE;
          }
        } else {
          printf("No more polygons.\n");
        }
        update_pixel_info(ii);
    }
    else {
        // arrow key event (or a key we don't handle)
        // moves the crosshair (or ctrl-click crosshair) the specified
        // direction.  ctrl-arrow and shift-arrow will move the
        // arrow farther
        int z = (int)zoom;
        if (z==0) z=1; // z will be 0 when the zoom factor is <= .5

        int incr = z;

        // ALT: Recenters, instead of moves the crosshair
        // CTRL: Multiplies the movement amount by 10.
        // SHIFT: Multiplies the movement amount by 25.
        // So, CTRL-SHIFT will multiply the movement amount by 250

        if (event->state & GDK_CONTROL_MASK)
            incr *= 10;
        if (event->state & GDK_SHIFT_MASK)
            incr *= 25;

        if (event->state & GDK_MOD1_MASK) {
            // Alt was pressed --> move center
            switch (event->keyval) {
                case GDK_Up: center_line -= incr; break;
                case GDK_Down: center_line += incr; break;
                case GDK_Left: center_samp -= incr; break;
                case GDK_Right: center_samp += incr; break;
                default: return TRUE;
            }
            fill_small(ii);
        }
        else {
            // Move one of the crosshairs
            if (last_was_crosshair) {
                switch (event->keyval) {
                    case GDK_Up: crosshair_line -= incr; break;
                    case GDK_Down: crosshair_line += incr; break;
                    case GDK_Left: crosshair_samp -= incr; break;
                    case GDK_Right: crosshair_samp += incr; break;
                    default: return TRUE;
                }
            }
            else {
                switch (event->keyval) {
                    if (g_poly->c < g_poly->n) {
                        case GDK_Up:
                            g_poly->line[g_poly->c] -= incr;
                            break;
                        case GDK_Down:
                            g_poly->line[g_poly->c] += incr; 
                            break;
                        case GDK_Left:
                            g_poly->samp[g_poly->c] -= incr; 
                            break;
                        case GDK_Right:
                            g_poly->samp[g_poly->c] += incr; 
                            break;
                    }
                    default: return TRUE;
                }
            }
            update_pixel_info(ii);
        }
    }

    fill_big(ii);
    return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_key_press_event(
    GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    // this event never seems to fire -- all kepresses are grabbed
    // by the "main_window" keypress handler below
    return handle_keypress(event, curr);
}

SIGNAL_CALLBACK int
on_ssv_main_window_key_press_event(
    GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    return handle_keypress(event, curr);
}
