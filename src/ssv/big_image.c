#include "ssv.h"
#include <gdk/gdkkeysyms.h>
#include "libasf_proj.h"

// current sizes of the large image.
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
    //GtkWidget *img = get_widget_checked("big_image");
    if (big_img_width != alloc->width || big_img_height != alloc->height) {
        big_img_width = alloc->width;
        big_img_height = alloc->height;
        big_img_width2 = big_img_width/2;
        big_img_height2 = big_img_height/2;
        fill_small();
        fill_big();
    }
}

SIGNAL_CALLBACK void on_big_image_repaint(GtkWidget *w)
{
    fill_small();
    fill_big();
}

static void ls2img(double line, double samp, int *x, int *y)
{
    *x = (samp - (double)center_samp)/zoom + get_big_image_width2();
    *y = (line - (double)center_line)/zoom + get_big_image_height2();
}

static void img2ls(int x, int y, double *line, double *samp)
{
    *line = ((double)y - get_big_image_height2())*zoom + (double)center_line;
    *samp = ((double)x - get_big_image_width2())*zoom + (double)center_samp;
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

// draws a crosshair at x,y (image coords)
static void put_crosshair (GdkPixbuf *pixbuf, double line, double samp)
{
    if (samp < 0 || line < 0 || samp >= ns || line >= nl)
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

    if (ix <= 0 || ix >= width || iy <= 0 || iy >= width)
        return;

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    lo = ix - 15;   if (lo < 0)     lo = 0;
    hi = ix + 15;   if (hi > width) hi = width;

    for (i = lo; i < hi; ++i) {
        if (i > ix-3 && i < ix+3) i = ix+3;
        p = pixels + iy * rowstride + i * n_channels;
        p[1] = 255;
        p[0] = p[2] = 0;
    }

    lo = iy - 15;   if (lo < 0)      lo = 0;
    hi = iy + 15;   if (hi > height) hi = height;

    for (i = lo; i < hi; ++i) {
        if (i > iy-3 && i < iy+3) i = iy+3;
        p = pixels + i * rowstride + ix * n_channels;
        p[1] = 255;
        p[0] = p[2] = 0;
    }
}

static int iabs(int i)
{
    return i<0 ? -i : i;
}

static void put_line(GdkPixbuf *pixbuf, double samp0, double line0, 
                     double samp1, double line1)
{
    if (samp0 < 0 || line0 < 0 || samp1 < 0 || line1 < 0 ||
        samp0 >= ns || samp1 >= ns || line0 >= nl || line1 >= nl)
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
    int ix0, iy0, ix1, iy1;
    ls2img(samp0, line0, &ix0, &iy0);
    ls2img(samp1, line1, &ix1, &iy1);

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    // loop on the distance which is greater
    if (iabs(ix1-ix0) > iabs(iy1-iy0)) {
        int incr = ix1>ix0 ? 1 : -1;
        for (i=ix0; i!=ix1; i+=incr) {
            j = iy0 + (float)(i-ix0)/(ix1-ix0) * (iy1-iy0);
            if (j >= 0 && i >= 0 && i <= width && j <= height) {
              p = pixels + j * rowstride + i * n_channels;
              p[0] = 255; p[1] = p[2] = 0;
            }
        }
    } else {
        int incr = iy1>iy0 ? 1 : -1;
        for (j=iy0; j!=iy1; j+=incr) {
            i = ix0 + (float)(j-iy0)/(iy1-iy0) * (ix1-ix0);
            if (j >= 0 && i >= 0 && i <= width && j <= height) {
              p = pixels + j * rowstride + i * n_channels;
              p[0] = 255; p[1] = p[2] = 0;
            }
        }
    }
}

static int calc_scaled_pixel_value(float val)
{
    if (val < g_min)
        return 0;
    else if (val > g_max)
        return 255;
    else
        return (int) round(((val-g_min)/(g_max-g_min))*255);
}

static GdkPixbuf * make_big_image()
{
    assert((data||data_fi) && meta);

    int ii, jj;
    int biw = get_big_image_width();
    int bih = get_big_image_height();
    unsigned char *bdata = MALLOC(sizeof(unsigned char)*biw*bih*3);

    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    int mm = 0;
    for ( ii = 0 ; ii < bih; ii++ ) {
        for ( jj = 0 ; jj < biw; jj++ ) {
            double l, s;
            img2ls(jj,ii,&l,&s);
            
            unsigned char uval;

            if (l<0 || l>=nl || s<0 || s>=ns) {
                uval = 0;
            } else {
                uval = (unsigned char)calc_scaled_pixel_value(
                    get_pixel((int)floor(l),(int)floor(s)));
            }

            int n = 3*mm;
            bdata[n] = uval;
            bdata[n+1] = uval;
            bdata[n+2] = uval;
            ++mm;
        }
    }

    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(bdata, GDK_COLORSPACE_RGB, FALSE, 
                                 8, biw, bih, biw*3, destroy_pb_data, NULL);
    
    if (!pb)
        asfPrintError("Failed to create the larger pixbuf.\n");

    put_crosshair(pb, crosshair_line, crosshair_samp);
    put_crosshair(pb, ctrl_clk_line, ctrl_clk_samp);
    put_line(pb, crosshair_line, crosshair_samp, ctrl_clk_line, ctrl_clk_samp);
    return pb;
}

static void line_samp_to_proj(double line, double samp, double *x, double *y)
{
    double lat, lon, projZ;
    meta_get_latLon(meta, line, samp, 0, &lat, &lon);
    if (meta->projection) {
        latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0,
            x, y, &projZ);
    } else {
        latLon2UTM(lat, lon, 0, x, y);
    }
}


void update_pixel_info()
{
    char buf[512];

    GtkWidget *img = get_widget_checked("big_image");
    GdkPixbuf *shown_pixbuf = gtk_image_get_pixbuf(GTK_IMAGE(img));

    double x = crosshair_samp;
    double y = crosshair_line;

    sprintf(buf, "Line: %.1f, Sample: %.1f\n", y, x);

    if (x < 0 || x >= ns || y < 0 || y >= nl)
    {
        // outside of the image
        sprintf(&buf[strlen(buf)], "Pixel Value: (outside image)\n");
    }
    else
    {
        assert(meta);
        assert(shown_pixbuf);
        
        float fval = get_pixel(crosshair_line, crosshair_samp);
        int uval = calc_scaled_pixel_value(fval);

        sprintf(&buf[strlen(buf)], "Pixel Value: %f -> %d\n", fval, uval);
    }

    double lat, lon;
    meta_get_latLon(meta, y, x, 0, &lat, &lon);
    sprintf(&buf[strlen(buf)], "Lat: %.3f, Lon: %.3f\n", lat, lon);

    if (meta->projection) {
        double projX, projY, projZ;
        latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0,
            &projX, &projY, &projZ);
        sprintf(&buf[strlen(buf)], "Proj X,Y: %.1f, %.1f m\n",
            projX, projY);
    }

    if (meta->state_vectors) {
        double s,t;
        meta_get_timeSlantDop(meta, y, x, &t, &s, NULL);
        sprintf(&buf[strlen(buf)], "Incid: %.3f, Look: %.3f (deg)\nSlant: %.1f m\n",
            R2D*meta_incid(meta,y,x), R2D*meta_look(meta,y,x), s);
    }

    if (ctrl_clk_samp >= 0 && ctrl_clk_line >= 0) {
        // crosshair coords
        double proj_x_cr, proj_y_cr; 
        line_samp_to_proj(y, x, &proj_x_cr, &proj_y_cr);

        // ctrl-click coords
        double proj_x_cc, proj_y_cc;       
        line_samp_to_proj(ctrl_clk_line, ctrl_clk_samp, &proj_x_cc, &proj_y_cc);

        double d = hypot(proj_x_cc-proj_x_cr, proj_y_cc-proj_y_cr);

        sprintf(&buf[strlen(buf)], "Distance to %.1f,%.1f: %.1f m",
            ctrl_clk_line, ctrl_clk_samp, d);
    } else {
        sprintf(&buf[strlen(buf)], "Distance: (ctrl-click to measure)");
    }

    GtkWidget *lbl = get_widget_checked("upper_label");
    gtk_label_set_text(GTK_LABEL(lbl), buf);
}

void fill_big()
{
    GdkPixbuf *pb = make_big_image();
    GtkWidget *img = get_widget_checked("big_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
}

SIGNAL_CALLBACK int on_small_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    // temporarily block keypress events...
    //widget = get_widget_checked("small_image_eventbox");
    //gtk_widget_set_events(widget, 0);
    
    GtkWidget *img = get_widget_checked("small_image");
    GdkPixbuf *pb = gtk_image_get_pixbuf(GTK_IMAGE(img));
    int w = gdk_pixbuf_get_width(pb);
    int h = gdk_pixbuf_get_height(pb);

    center_samp = ((int)event->x * ns) / (double)w;
    center_line = ((int)event->y * nl) / (double)h;

    fill_small();
    fill_big();

    while (gtk_events_pending())
        gtk_main_iteration();    

    //gtk_widget_set_events(widget, GDK_BUTTON_PRESS_MASK);
    return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    // temporarily block keypress events...
    //widget = get_widget_checked("big_image_eventbox");
    //gtk_widget_set_events(widget, 0);

    if (event->button == 1) {
        // ctrl-left-click: measure distance
        if (((int)event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) {
            img2ls((int)event->x, (int)event->y, &ctrl_clk_line, &ctrl_clk_samp);
        }
        // left-click: move crosshair
        else {
            img2ls((int)event->x, (int)event->y, &crosshair_line, &crosshair_samp);
            ctrl_clk_line = ctrl_clk_samp = -1;
        }
        update_pixel_info();
        fill_big();
    } else if (event->button == 3) {
        // right-click: re-center
        img2ls((int)event->x, (int)event->y, &center_line, &center_samp);
        fill_small();
        fill_big();
    }

    while (gtk_events_pending())
        gtk_main_iteration();    

    //gtk_widget_set_events(widget, GDK_BUTTON_PRESS_MASK);
    return TRUE;
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

SIGNAL_CALLBACK int
on_big_image_scroll_event(
    GtkWidget *widget, GdkEventScroll *event, gpointer user_data)
{
    // temporarily block keypress events...
    //widget = get_widget_checked("big_image_eventbox");
    //gtk_widget_set_events(widget, 0);

    if (event->direction == GDK_SCROLL_UP) {
        if (zoom > 1)
            --zoom;
        else if (zoom <= 1)
            zoom /= 2;
    } else if (event->direction == GDK_SCROLL_DOWN) {
        if (zoom <= 1)
            zoom *= 2;
        else
            ++zoom;
    }

    update_zoom();
    fill_small();
    fill_big();

    while (gtk_events_pending())
        gtk_main_iteration();    

    //gtk_widget_set_events(widget, GDK_BUTTON_PRESS_MASK);
    return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_key_press_event(
    GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    printf("keypress...\n");
    int incr = zoom;
    if (event->state & GDK_CONTROL_MASK) incr = 10*zoom;
    else if (event->state & GDK_SHIFT_MASK) incr = 25*zoom;

    switch (event->keyval) {
        case GDK_Up: crosshair_line -= incr; break;
        case GDK_Down: crosshair_line += incr; break;
        case GDK_Left: crosshair_samp -= incr; break;
        case GDK_Right: crosshair_samp += incr; break;
        default: return TRUE;
    }

    update_pixel_info();
    fill_big();
    return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_motion_notify_event(
    GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
    printf("motion...\n");

    double line, samp;
    img2ls((int)event->x, (int)event->y, &line, &samp);

    static GtkWidget *lbl = NULL;
    if (!lbl)
        lbl = get_widget_checked("upper_label");

    char buf[512];
    if (samp < 0 || samp >= ns || line < 0 || line >= nl)
    {
        // outside of the image
        strcpy(buf, "");
    }
    else
    {
        float fval = get_pixel(line, samp);

        double lat, lon;
        meta_get_latLon(meta, line, samp, 0, &lat, &lon);

        sprintf(buf, "Line: %.1f, Sample: %.1f\n"
            "Pixel Value: %f\n"
            "Lat: %f, Lon: %f\n", crosshair_line, crosshair_samp,
            fval, lat, lon);
    }

    gtk_label_set_text(GTK_LABEL(lbl), buf);
    return TRUE;
}
