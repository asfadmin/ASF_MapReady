#include "asf_view.h"
#include <gdk/gdkkeysyms.h>
#include "libasf_proj.h"
//#include "cr.h"

UserPolygon g_polys[MAX_POLYS];

// pointer to the currently active one
UserPolygon *g_poly;
int which_poly=0;

int g_show_north_arrow = FALSE;
int g_lhs_visible = TRUE;
int g_outline = FALSE;

// current sizes of the large image.
// keep half the size around, we need that during the redraw, which
// is supposed to be quick
static int big_img_width=800;
static int big_img_height=800;
static int big_img_width2=400;
static int big_img_height2=400;

int get_big_image_width_full()
{
    return big_img_width;
}

int get_big_image_height_full()
{
    return big_img_height;
}

int get_big_image_width2_full()
{
    return big_img_width2;
}

int get_big_image_height2_full()
{
    return big_img_height2;
}

// This is for the subimage display, when the user is showing
// several images panelled together.  Each panel should be
// the same size, the following functions return those sizes
// subimages can be 1, 2 or 4.
int subimages = 1;

int get_big_image_width_sub()
{
    switch (subimages) {
        default:
        case 1:
            return big_img_width;
        case 2:
        case 4:
            return big_img_width/2;
    }
}

int get_big_image_height_sub()
{
    switch (subimages) {
        default:
        case 1:
        case 2:
            return big_img_height;
        case 4:
            return big_img_height/2;
    }
}

int get_big_image_width2_sub()
{
    switch (subimages) {
        default:
        case 1:
            return big_img_width2;
        case 2:
        case 4:
            return big_img_width2/2;
    }
}

int get_big_image_height2_sub()
{
    switch (subimages) {
        default:
        case 1:
        case 2:
            return big_img_height2;
        case 4:
            return big_img_height2/2;
    }
}


SIGNAL_CALLBACK void on_big_image_resize(GtkWidget *w, 
    GtkAllocation *alloc, gpointer user_data)
{
    // User resized!
    // Recalculate the width/height and half-width/half-height values
    // The first if statement just prevents doing the redraw if the user didn't
    // actually resize (i.e., just grabbed the edge but didn't drag it).
    // It also is useful to stop an infinite cascade-of-resizes that GTK
    // sometimes seems to get stuck in -- the "fill_small/big" will
    // occasionally trigger a resize that runs after this method completes.
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
    *x = (samp - (double)center_samp)/zoom + get_big_image_width2_sub();
    *y = (line - (double)center_line)/zoom + get_big_image_height2_sub();
}

void img2ls(int x, int y, double *line, double *samp)
{
    // convert from screen coordinates to line/sample coordinates
    *line = ((double)y - get_big_image_height2_sub())*zoom + (double)center_line;
    *samp = ((double)x - get_big_image_width2_sub())*zoom + (double)center_samp;
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
#define GREEN 4

void get_color(int color, unsigned char *r, unsigned char *g,
               unsigned char *b)
{
    int biggest = 36;
    if (color>biggest) {
      while (color>biggest)
        color -= biggest-11;
    }

    switch (color) {
      case RED:
        *r = 255;
        *g = *b = 0;
        break;

      case GREEN: // crosshair color
        *g = 255;
        *r = *b = 0;
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

void put_line(GdkPixbuf *pixbuf, double line0, double samp0, 
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
            if (j >= 0 && i >= 0 && i < width && j < height) {
              p = pixels + j * rowstride + i * n_channels;
              p[0] = r; p[1] = g; p[2] = b;
            }
        }
    } else {
        int incr = iy1>iy0 ? 1 : -1;
        for (j=iy0; j!=iy1; j+=incr) {
            i = ix0 + (float)(j-iy0)/(iy1-iy0) * (ix1-ix0);
            if (j >= 0 && i >= 0 && i < width && j < height) {
              p = pixels + j * rowstride + i * n_channels;
              p[0] = r; p[1] = g; p[2] = b;
            }
        }
    }
}

static void put_marker(GdkPixbuf *pixbuf, double line, double samp,
                       int marker_code, int color, ImageInfo *ii)
{
    if (marker_code == 0)
      return;

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
    int ix, iy;
    ls2img(line, samp, &ix, &iy);

    if (ix <= 0 || ix > width || iy <= 0 || iy > height)
        return;

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    // color of the drawn line
    unsigned char r, g, b;
    get_color(color, &r, &g, &b);

    switch (marker_code) {
      case 1: // square, 5 pixels per side
      {
        const int ss = 2; // actual side length = ss*2+1

        int lo = ix-ss;
        if (lo<0) lo=0;

        int hi = ix+ss;
        if (hi>=width) hi=width-1;

        if (iy>ss) {
          for (i=lo; i<=hi; ++i) {
            p = pixels + (iy-ss)*rowstride + i*n_channels;
            p[0] = r;
            p[1] = g;
            p[2] = b;
          }
        }
        if (iy<height-ss) {
          for (i=lo; i<=hi; ++i) {
            p = pixels + (iy+ss)*rowstride + i*n_channels;
            p[0] = r;
            p[1] = g;
            p[2] = b;
          }
        }

        lo = iy-(ss-1);
        if (lo<0) lo=0;

        hi = iy+(ss-1);
        if (hi>=height) hi=height-1;

        if (ix>ss) {
          for (j=lo; j<=hi; ++j) {
            p = pixels + j*rowstride + (ix-ss)*n_channels;
            p[0] = r;
            p[1] = g;
            p[2] = b;
          }
        }
        if (ix<width-ss) {
          for (j=lo; j<=hi; ++j) {
            p = pixels + j*rowstride + (ix+ss)*n_channels;
            p[0] = r;
            p[1] = g;
            p[2] = b;
          }
        }
        break;
      }

      case 0:
      default: // no marker at each point
        break;
    }
}

int g_show_cr = TRUE;

/*
static void add_cr(GdkPixbuf *pb, ImageInfo *ii)
{
  if (!g_show_cr)
    return;

  meta_parameters *meta = ii->meta;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  int i;
  for (i=0; i<100; ++i) {
    char *name = djArr[i].name;
    if (!name) break;

    double lat = djArr[i].lat;
    double lon = djArr[i].lon;
    double ht = djArr[i].elev;
    ht=0;

    double line, samp;
    int bad = meta_get_lineSamp(meta, lat, lon, ht, &line, &samp);
   
    if (bad || line<0 || line>nl || samp<0 || samp>ns)
      continue;

    put_marker(pb, line, samp, 1, PURPLE, ii);

    // convert from image coords to screen coords
    int ix, iy;
    ls2img(line, samp, &ix, &iy);
  }
}
*/

static int g_current_pt_index = -1;

static void add_pts(GdkPixbuf *pb, ImageInfo *ii)
{
  if (!pt_specified)
    return;

  meta_parameters *meta = ii->meta;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  double line, samp;
  int jj=0;
  while (pt_lat[jj] != -999) {
    int bad = meta_get_lineSamp(meta, pt_lat[jj], pt_lon[jj], 0, &line, &samp);
    ++jj;
   
    if (bad || line<0 || line>nl || samp<0 || samp>ns)
      continue;
  
    put_marker(pb, line, samp, 1, PURPLE, ii);
  }
}
  
static void add_outline(GdkPixbuf *pb, ImageInfo *ii)
{
  if (!g_outline)
    return;

  printf("Drawing outline...\n");

  meta_parameters *meta = ii->meta;
  int k=0, n = (current_image_info_index + 1) % n_images_loaded;
  while (n != current_image_info_index) {
    ImageInfo *ii1 = &image_info[n];
    meta_parameters *meta1 = ii1->meta;
    int nl = meta1->general->line_count;
    int ns = meta1->general->sample_count;
    double lat, lon, l1, s1, l2, s2, l3, s3, l4, s4;

    if (meta1->location) {
      lat = meta1->location->lat_start_near_range;
      lon = meta1->location->lon_start_near_range;
      meta_get_lineSamp(meta, lat, lon, 0, &l1, &s1);
      lat = meta1->location->lat_start_far_range;
      lon = meta1->location->lon_start_far_range;
      meta_get_lineSamp(meta, lat, lon, 0, &l2, &s2);
      lat = meta1->location->lat_end_far_range;
      lon = meta1->location->lon_end_far_range;
      meta_get_lineSamp(meta, lat, lon, 0, &l3, &s3);
      lat = meta1->location->lat_end_near_range;
      lon = meta1->location->lon_end_near_range;
      meta_get_lineSamp(meta, lat, lon, 0, &l4, &s4);
    }
    else {
      meta_get_latLon(meta1, 0, 0, 0, &lat, &lon);
      meta_get_lineSamp(meta, lat, lon, 0, &l1, &s1);
      //printf("Corner 1: %f %f -> %f %f\n", lat, lon, l1, s1);
      meta_get_latLon(meta1, nl-1, 0, 0, &lat, &lon);
      meta_get_lineSamp(meta, lat, lon, 0, &l2, &s2);
      //printf("Corner 2: %f %f -> %f %f\n", lat, lon, l2, s2);
      meta_get_latLon(meta1, nl-1, ns-1, 0, &lat, &lon);
      meta_get_lineSamp(meta, lat, lon, 0, &l3, &s3);
      //printf("Corner 3: %f %f -> %f %f\n", lat, lon, l3, s3);
      meta_get_latLon(meta1, 0, ns-1, 0, &lat, &lon);
      meta_get_lineSamp(meta, lat, lon, 0, &l4, &s4);
      //printf("Corner 4: %f %f -> %f %f\n", lat, lon, l4, s4);
    }
 
    int clr = GREEN;
    if (k > 0) clr = 11 + k;
    k++;

    put_line(pb, l1, s1, l2, s2, clr, ii);
    put_line(pb, l2, s2, l3, s3, clr, ii);
    put_line(pb, l3, s3, l4, s4, clr, ii);
    put_line(pb, l4, s4, l1, s1, clr, ii);

    n++;
    n %= n_images_loaded;
  }
  
}
 
static void add_north_arrow(GdkPixbuf *pb, ImageInfo *ii)
{
  if (!g_show_north_arrow)
    return;

  // arrow location: bottom right corner
  int ax = 20;
  int ay = get_big_image_height_full() - 20;
  double arrow_line, arrow_samp, lat, lon;
  img2ls(ax,ay,&arrow_line,&arrow_samp);
  meta_get_latLon(ii->meta,arrow_line,arrow_samp,0,&lat,&lon);

  // to get the direction the arrow will point, figure the line/samp of
  // a point slightly north (same longitude, but a slightly greater lat)
  double line_up, samp_up, lat_up = lat + .2;
  meta_get_lineSamp(ii->meta,lat_up,lon,0,&line_up,&samp_up);
  double angle = atan2(line_up - arrow_line, samp_up - arrow_samp);

  // the arrow will be 25 pixels long
  const double len = 25.0 / 2.0;
  // now calculate the line/samp of the head & tail of the arrow
  int hx = ax + (int)floor(0.5 + len*cos(angle));
  int hy = ay + (int)floor(0.5 + len*sin(angle));
  int tx = ax - (int)floor(0.5 + len*cos(angle));
  int ty = ay - (int)floor(0.5 + len*sin(angle));

  // draw the line -- put_line() requires image coordinates, so we do have
  // a double conversion for no good reason
  double x0,y0,x1,y1;
  img2ls(tx,ty,&y0,&x0);
  img2ls(hx,hy,&y1,&x1);
  put_line(pb, y0, x0, y1, x1, GREEN, ii);

  // now... put on the arrowhead
  const double head_len = 10.0;

  // first head goes from the head, at angle+135, the other at angle-135
  double ang = 135 * D2R;
  int h1x = hx + (int)floor(0.5 + head_len*cos(angle + ang));
  int h1y = hy + (int)floor(0.5 + head_len*sin(angle + ang));
  int h2x = hx + (int)floor(0.5 + head_len*cos(angle - ang));
  int h2y = hy + (int)floor(0.5 + head_len*sin(angle - ang));

  img2ls(h1x,h1y,&y0,&x0);
  put_line(pb, y0, x0, y1, x1, GREEN, ii);

  img2ls(h2x,h2y,&y0,&x0);
  put_line(pb, y0, x0, y1, x1, GREEN, ii);
}

GdkPixbuf * make_big_image(ImageInfo *ii, int show_crosshair)
{
    assert(ii->data_ci);
    assert(ii->meta);

    int i, j, k, m, n;
    int nchan = 3; // RGB for now, don't support RGBA yet
    int biw = get_big_image_width_sub();
    int bih = get_big_image_height_sub();
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

    if (!mask) {
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
                }
                else {
                    // here we have some averaging, that will make the
                    // images look a bit smoother when zoomed out
                    if (zoom<2) {
                        // one-to-one (or thereabouts) view -- no averaging
                        cached_image_get_rgb(ii->data_ci, (int)floor(l),
                            (int)floor(s), &r, &g, &b);
                    }
                    else if (zoom<3) {
                        // 2x view -- average 4 pixels to produce 1.
                        int l2 = (int)floor(l);
                        int s2 = (int)floor(s);
                        unsigned char r1,r2,r3,r4,g1,g2,g3,g4,b1,b2,b3,b4;
                        cached_image_get_rgb(ii->data_ci, l2,   s2,   &r1,&g1,&b1);
                        cached_image_get_rgb(ii->data_ci, l2+1, s2,   &r2,&g2,&b2);
                        cached_image_get_rgb(ii->data_ci, l2,   s2+1, &r3,&g3,&b3);
                        cached_image_get_rgb(ii->data_ci, l2+1, s2+1, &r4,&g4,&b4);
                        r=(r1+r2+r3+r4)/4;
                        g=(g1+g2+g3+g4)/4;
                        b=(b1+b2+b3+b4)/4;
                    }
                    else if (zoom<4) {
                        // 3x view -- average 9 pixels to produce 1.
                        int l2 = (int)floor(l);
                        int s2 = (int)floor(s);
                        int rt=0, gt=0, bt=0;
                        for (m=0; m<3; ++m) {
                            for (n=0; n<3; ++n) {
                                cached_image_get_rgb(ii->data_ci, l2+m, s2+n,
                                                    &r, &g, &b);
                                rt += (int)r;
                                gt += (int)g;
                                bt += (int)b;
                            }
                        }
                        r = (unsigned char) (rt/9);
                        g = (unsigned char) (gt/9);
                        b = (unsigned char) (bt/9);
                    }
                    else {
                        // 4x or greater view -- average 9 pixels to produce 1.
                        int l2 = (int)floor(l);
                        int s2 = (int)floor(s);
                        int fac = (int)floor(zoom/3);
                        int rt=0, gt=0, bt=0;
                        for (m=0; m<3; ++m) {
                            for (n=0; n<3; ++n) {
                                cached_image_get_rgb(ii->data_ci,
                                                    l2+m*fac, s2+n*fac,
                                                    &r, &g, &b);
                                rt += (int)r;
                                gt += (int)g;
                                bt += (int)b;
                            }
                        }
                        r = (unsigned char) (rt/9);
                        g = (unsigned char) (gt/9);
                        b = (unsigned char) (bt/9);
                    }
                }

                int p = 3*mm;
                bdata[p] = r;
                bdata[p+1] = g;
                bdata[p+2] = b;
                ++mm;
            }
        }
    }
    else { // mask applied
        // this code is largely the same as above except we need to
        // check for ignored values before populating a pixel
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
                }
                else {
                    // here we have some averaging, that will make the
                    // images look a bit smoother when zoomed out
                    if (zoom<2) {
                        // one-to-one (or thereabouts) view -- no averaging
                        get_rgb_with_masking(ii, mask, (int)floor(l), (int)floor(s),
                                             &r, &g, &b);
                    }
                    else if (zoom<3) {
                        // 2x view -- average 4 pixels to produce 1.
                        int l2 = (int)floor(l);
                        int s2 = (int)floor(s);
                        unsigned char r1,r2,r3,r4,g1,g2,g3,g4,b1,b2,b3,b4;
                        get_rgb_with_masking(ii, mask, l2,   s2,   &r1,&g1,&b1);
                        get_rgb_with_masking(ii, mask, l2+1, s2,   &r2,&g2,&b2);
                        get_rgb_with_masking(ii, mask, l2,   s2+1, &r3,&g3,&b3);
                        get_rgb_with_masking(ii, mask, l2+1, s2+1, &r4,&g4,&b4);
                        r=(r1+r2+r3+r4)/4;
                        g=(g1+g2+g3+g4)/4;
                        b=(b1+b2+b3+b4)/4;
                    }
                    else {
                        // 3x or greater view -- average 9 pixels to produce 1.
                        int l2 = (int)floor(l);
                        int s2 = (int)floor(s);
                        int fac = (int)floor(zoom/3);
                        int rt=0, gt=0, bt=0;
                        for (m=0; m<3; ++m) {
                            for (n=0; n<3; ++n) {
                                get_rgb_with_masking(ii, mask,
                                                     l2+m*fac, s2+n*fac,
                                                     &r, &g, &b);
                                rt += (int)r;
                                gt += (int)g;
                                bt += (int)b;
                            }
                        }
                        r = (unsigned char) (rt/9);
                        g = (unsigned char) (gt/9);
                        b = (unsigned char) (bt/9);
                    }
                }
                
                int p = 3*mm;
                bdata[p] = r;
                bdata[p+1] = g;
                bdata[p+2] = b;
                ++mm;
            }
        }
    }
    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(bdata, GDK_COLORSPACE_RGB, FALSE, 
                                 8, biw, bih, biw*3, destroy_pb_data, NULL);

    if (!pb)
        asfPrintError("Failed to create the larger pixbuf.\n");

    // put the red crosshair at the "active" point along the polygon
    if (show_crosshair && g_poly->c < g_poly->n)
        put_crosshair(pb, g_poly->line[g_poly->c], g_poly->samp[g_poly->c],
            FALSE, ii);

    // green crosshair goes second, so if the two overlap, we will see
    // the green one (the main one)
    if (show_crosshair)
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

    // draw stuff read in from CSV/Shapefile
    if (num_shapes > 0) {
        for (i=0; i<num_shapes; ++i) {
            Shape *s = g_shapes[i];
            for (j=0; j<s->num_points; ++j) {
                if (j>0)
                    put_line(pb, s->lines[j-1], s->samps[j-1],
                             s->lines[j], s->samps[j], s->color_code, ii);
                put_marker(pb, s->lines[j], s->samps[j], s->marker_code,
                           s->color_code, ii);
            }
        }
    }

    // Add a "north" arrow if possible
    if (meta_supports_meta_get_latLon(ii->meta)) {
      add_north_arrow(pb, ii);
      //add_cr(pb, ii);
      add_pts(pb, ii);
      add_outline(pb, ii);
    }

    // draw the polygon
    if (g_poly->n > 0) {
        put_line(pb, crosshair_line, crosshair_samp,
            g_poly->line[0], g_poly->samp[0], RED, ii);
        for (i=0; i<g_poly->n-1; ++i) {
            put_line(pb, g_poly->line[i], g_poly->samp[i],
                g_poly->line[i+1], g_poly->samp[i+1], RED, ii);
        }
    }
    else if (g_poly->show_extent) {
        // no polygon -- close "save subset" window, if open
        show_widget("save_subset_window", FALSE);
        g_poly->show_extent = FALSE;
    }

    // draw old polygons
    if (planner_is_active()) {
      // decided to put in a cheat for the planner... to make the frames
      // look more distinct
      for (k=0; k<MAX_POLYS; ++k) {
        if (g_polys[k].n > 0 && row_is_checked(k)) {
          for (j=0; j<g_polys[k].n-1; j+=5) {
            for (i=j; i<j+4; ++i) {
              put_line(pb, g_polys[k].line[i], g_polys[k].samp[i],
                       g_polys[k].line[i+1], g_polys[k].samp[i+1], 10+k, ii);
            }
          }
        }
      }
    }
    else {
      for (k=0; k<MAX_POLYS; ++k) {
        if (g_polys[k].n > 0 && row_is_checked(k)) {
          for (i=0; i<g_polys[k].n-1; ++i) {
            put_line(pb, g_polys[k].line[i], g_polys[k].samp[i],
                     g_polys[k].line[i+1], g_polys[k].samp[i+1], 10+k, ii);
          }
        }
      }

    }

    return pb;
}

void fill_big(ImageInfo *ii)
{
    GdkPixbuf *pb = NULL;
    if (subimages == 1) {
        // never show the crosshair for the planner -- not needed any longer
        pb = make_big_image(ii, !planner_is_active());
    }
    else if (subimages == 2) {
        int next_image_info_index = (current_image_info_index + 1) % n_images_loaded;
        ImageInfo *ii1 = &image_info[next_image_info_index];

        GdkPixbuf *pb1 = make_big_image(ii, !planner_is_active());
        GdkPixbuf *pb2 = make_big_image(ii1, !planner_is_active());

        int w = get_big_image_width_sub();
        int h = get_big_image_height_sub();
        int biw = get_big_image_width_full();
        int bih = get_big_image_height_full();

        pb = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, biw, bih);
        gdk_pixbuf_copy_area(pb1, 0, 0, w, h, pb, 0, 0);
        gdk_pixbuf_copy_area(pb2, 0, 0, w, h, pb, w, 0);

        g_object_unref(pb1);
        g_object_unref(pb2);
    }
    else if (subimages == 4) {
        int ind1 = (current_image_info_index + 1) % n_images_loaded;
        int ind2 = (current_image_info_index + 2) % n_images_loaded;
        int ind3 = (current_image_info_index + 3) % n_images_loaded;

        ImageInfo *ii1 = &image_info[ind1];
        ImageInfo *ii2 = &image_info[ind2];
        ImageInfo *ii3 = &image_info[ind3];

        GdkPixbuf *pb0 = make_big_image(ii, !planner_is_active());
        GdkPixbuf *pb1 = make_big_image(ii1, !planner_is_active());
        GdkPixbuf *pb2 = make_big_image(ii2, !planner_is_active());
        GdkPixbuf *pb3 = make_big_image(ii3, !planner_is_active());

        int w = get_big_image_width_sub();
        int h = get_big_image_height_sub();
        int biw = get_big_image_width_full();
        int bih = get_big_image_height_full();

        pb = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, biw, bih);
        gdk_pixbuf_copy_area(pb0, 0, 0, w, h, pb, 0, 0);
        gdk_pixbuf_copy_area(pb1, 0, 0, w, h, pb, w, 0);
        gdk_pixbuf_copy_area(pb2, 0, 0, w, h, pb, 0, h);
        gdk_pixbuf_copy_area(pb3, 0, 0, w, h, pb, w, h);

        g_object_unref(pb0);
        g_object_unref(pb1);
        g_object_unref(pb2);
        g_object_unref(pb3);
    }

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
int last_was_crosshair = TRUE;

void big_clicked(GdkEventButton *event)
{
    int x = (int)event->x;
    int y = (int)event->y;

    // if user clicked in one of the other sub-windows, pretend it
    // was in the upper-left one
    if (subimages == 2) {
        if (x > get_big_image_width_sub())
            x -= get_big_image_width_sub();
    }
    else if (subimages == 4) {
        if (x > get_big_image_width_sub())
            x -= get_big_image_width_sub();
        if (y > get_big_image_height_sub())
            y -= get_big_image_height_sub();
    }

    if (event->button == 1) {
        // ctrl-left-click: measure distance
        if (((int)event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) {
            if (g_poly->n < MAX_POLY_LEN) {
                double l, s;
                img2ls(x, y, &l, &s);
                g_poly->line[g_poly->n] = l;
                g_poly->samp[g_poly->n] = s;
                ++g_poly->n;
                g_poly->c = g_poly->n-1;
            } else {
                asfPrintWarning("Exceeded maximum polygon length.\n"
                                "No more points can be added.\n");
            }
            last_was_crosshair = FALSE;
        }
        // left-click: move crosshair
        // (planner: select point's matched acquisitions)
        else {
          if (planner_is_active()) {
            double l, s;
            img2ls(x, y, &l, &s);
            planner_click((int)(l+.5),(int)(s+.5));
          }
          else {
            img2ls(x, y, &crosshair_line, &crosshair_samp);
            last_was_crosshair = TRUE;
          }
        }
        update_pixel_info(curr);
        fill_big(curr);
    } else if (event->button == 3) {
        // right-click: re-center
        img2ls(x, y, &center_line, &center_samp);
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
         has_focus("go_to_lat_entry") ||
         has_focus("go_to_lon_entry") ||
         has_focus("go_to_line_entry") ||
         has_focus("go_to_samp_entry") ||
         has_focus("go_to_projx_entry") ||
         has_focus("go_to_projy_entry") ||
         has_focus("gs_custom_min_entry") ||
         has_focus("gs_custom_max_entry") ||
         has_focus("gs_ignore_value_entry") ||
         has_focus("gs_ignore_range_min_entry") ||
         has_focus("gs_ignore_range_max_entry") ||
         has_focus("red_custom_min_entry") ||
         has_focus("red_custom_max_entry") ||
         has_focus("red_ignore_value_entry") ||
         has_focus("red_ignore_range_min_entry") ||
         has_focus("red_ignore_range_max_entry") ||
         has_focus("green_custom_min_entry") ||
         has_focus("green_custom_max_entry") ||
         has_focus("green_ignore_value_entry") ||
         has_focus("green_ignore_range_min_entry") ||
         has_focus("green_ignore_range_max_entry") ||
         has_focus("blue_custom_min_entry") ||
         has_focus("blue_custom_max_entry") ||
         has_focus("blue_ignore_value_entry") ||
         has_focus("blue_ignore_range_min_entry") ||
         has_focus("blue_ignore_range_max_entry") ||
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
         event->keyval == GDK_KP_Add ||
         event->keyval == GDK_KP_Subtract ||
         event->keyval == GDK_KP_Decimal ||
         event->keyval == GDK_period ||
         event->keyval == GDK_comma ||
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
    else if (event->keyval == GDK_Home || event->keyval == GDK_equal) {
        // Home: Revert to the normal zoom level
        zoom_default(ii);
    }
    else if (event->keyval == GDK_End || event->keyval == GDK_0) {
        // End: Fit image to window
        int h = get_big_image_height_sub();
        int w = get_big_image_width_sub();

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
            if ((event->state & GDK_SHIFT_MASK) ||
                (event->state & GDK_CONTROL_MASK))
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
    else if (event->keyval == GDK_c || event->keyval == GDK_C) {
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
    else if (event->keyval == GDK_g || event->keyval == GDK_G) {
        // g: open google earth
        open_google_earth();
        return TRUE;
    }
    else if (event->keyval == GDK_m || event->keyval == GDK_M) {
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
    else if (event->keyval == GDK_l || event->keyval == GDK_L) {
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
    else if (event->keyval == GDK_1) {
        if (subimages != 1) {
            subimages = 1;
        }
    }
    else if (event->keyval == GDK_2) {
        if (subimages != 2) {
            subimages = 2;
        }
    }
    else if (event->keyval == GDK_4) {
        if (subimages != 4) {
            subimages = 4;
        }
    }
    else if (event->keyval == GDK_o || event->keyval == GDK_O) {
        g_outline = !g_outline;
    }
    else if (event->keyval == GDK_f || event->keyval == GDK_F) {
        double lat=0, lon=0; //, x, y;
        meta_parameters *old_meta = meta_copy(curr->meta);

        current_image_info_index = (current_image_info_index + 1) % n_images_loaded;
        ii = curr = &image_info[current_image_info_index];
        asfPrintStatus("Switching to: %s\n", curr->filename);

        // update crosshair and any polygon info, if possible
        if (meta_supports_meta_get_latLon(old_meta) &&
            meta_supports_meta_get_latLon(curr->meta))
        {
            meta_get_latLon(old_meta, crosshair_line, crosshair_samp, 0,
                            &lat, &lon);
            meta_get_lineSamp(curr->meta, lat, lon, 0,
                              &crosshair_line, &crosshair_samp);

            int i;
            for (i=0; i<g_poly->n; ++i) {
                meta_get_latLon(old_meta, g_poly->line[i], g_poly->samp[i], 0,
                                &lat, &lon);
                meta_get_lineSamp(curr->meta, lat, lon, 0,
                                  &g_poly->line[i], &g_poly->samp[i]);
            }

            // move cursor to same lat,lon
            double line, samp;
            meta_get_latLon(old_meta, center_line, center_samp, 0,
                            &lat, &lon);
            int bad = meta_get_lineSamp(curr->meta, lat, lon, 0, &line, &samp);
            int nl = curr->meta->general->line_count;
            int ns = curr->meta->general->sample_count;
            if (!bad && line>0 && line<nl && samp>0 && samp<ns) {
                center_line = line;
                center_samp = samp;
          }
        }

        meta_free(old_meta);

        fill_small_force_reload(ii);
        update_pixel_info(ii);
        fill_meta_info();
        fill_stats(curr);
        set_title(FALSE, NULL);
        setup_bands_tab(curr->meta);
    }
    else if (event->keyval == GDK_h || event->keyval == GDK_H) {
        g_lhs_visible = !g_lhs_visible;
        show_widget("vbox_lhs", g_lhs_visible);
    }
    else if (event->keyval == GDK_n || event->keyval == GDK_N) {
        g_show_north_arrow = !g_show_north_arrow;
    }
/*
    else if (event->keyval == GDK_n || event->keyval == GDK_N) {
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
    else if (event->keyval == GDK_p || event->keyval == GDK_P) {
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
*/
    else if (event->keyval == GDK_p || event->keyval == GDK_P) {
        meta_parameters *meta = curr->meta;
        if (pt_specified &&
             (meta->projection || (meta->sar&&meta->state_vectors) ||
              meta->transform || meta->airsar))
        {
          int nl = meta->general->line_count;
          int ns = meta->general->sample_count;

          g_current_pt_index++;
          if (g_current_pt_index >= MAX_PTS || pt_lat[g_current_pt_index] == -999)
            g_current_pt_index = 0;

          double lat = pt_lat[g_current_pt_index];
          double lon = pt_lon[g_current_pt_index];
          double line, samp;

          int bad = meta_get_lineSamp(curr->meta, lat, lon, 0, &line, &samp);
          if (!bad) {
            if (line < 0 || line > nl || samp < 0 || samp > ns) {
              printf("Outside image: %f %f\n", lat, lon);
            } else {
              if (pt_name[g_current_pt_index] && strlen(pt_name[g_current_pt_index]) > 0)
                printf("Moving to %s: %f,%f\n", pt_name[g_current_pt_index], lat, lon);
              else
                printf("Moving to %f,%f\n", lat, lon);
              center_line = crosshair_line = line;
              center_samp = crosshair_samp = samp;

              update_pixel_info(curr);
              fill_small(curr);
            }
          } else {
            printf("Bad: %f %f\n", lat, lon);
          }
        }
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
