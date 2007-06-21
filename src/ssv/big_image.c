#include "ssv.h"
#include <gdk/gdkkeysyms.h>
#include "libasf_proj.h"

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

// draws a crosshair at x,y (image coords)
static void put_crosshair (GdkPixbuf *pixbuf, int x, int y)
{
    if (x < 0 || y < 0)
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
    x = (x - cx)/zoom + 450;
    y = (y - cy)/zoom + 450;

    if (x < 0 || x >= width || y < 0 || y >= width)
        return;

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    lo = x - 15;   if (lo < 0)     lo = 0;
    hi = x + 15;   if (hi > width) hi = width;

    for (i = lo; i < hi; ++i) {
        if (i > x-3 && i < x+3) i = x+3;
        p = pixels + y * rowstride + i * n_channels;
        p[1] = 255;
        p[0] = p[2] = 0;
    }

    lo = y - 15;   if (lo < 0)      lo = 0;
    hi = y + 15;   if (hi > height) hi = height;

    for (i = lo; i < hi; ++i) {
        if (i > y-3 && i < y+3) i = y+3;
        p = pixels + i * rowstride + x * n_channels;
        p[1] = 255;
        p[0] = p[2] = 0;
    }
}

static GdkPixbuf * make_big_image(int size, int cx, int cy, int zoom)
{
    assert((data||data_fi) && meta);

    int ii, jj;
    unsigned char *bdata = MALLOC(sizeof(unsigned char)*size*size*3);

    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    int min_x = cx - size*zoom/2;
    int min_y = cy - size*zoom/2;
    int mm = 0;
    for ( ii = min_y ; ii < min_y + size*zoom ; ii += zoom ) {
        for ( jj = min_x ; jj < min_x + size*zoom ; jj += zoom ) {
            unsigned char uval;

            if (ii<0 || ii>=nl || jj<0 || jj>=ns) {
                uval = 0;
            } else {
                float val = get_pixel(ii, jj);

                if (val < g_min)
                    uval = 0;
                else if (val > g_max)
                    uval = 255;
                else
                    uval = (unsigned char) round(((val-g_min)/(g_max-g_min))*255);
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
                                 8, size, size, size*3, destroy_pb_data, NULL);
    
    if (!pb)
        asfPrintError("Failed to create the larger pixbuf.\n");

    put_crosshair(pb, crosshair_x, crosshair_y);
    return pb;
}

static guchar get_pb_pixel (GdkPixbuf *pixbuf, int x, int y)
{
    int width, height, rowstride, n_channels;
    guchar *pixels, *p;
    
    n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    
    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);
    
    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);
    
    g_assert (x >= 0 && x < width);
    g_assert (y >= 0 && y < height);
    
    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);
    
    p = pixels + y * rowstride + x * n_channels;
    return p[0];
}

static void line_samp_to_proj(int line, int samp, double *x, double *y)
{
    double lat, lon, projZ;
    meta_get_latLon(meta, line, samp, 0, &lat, &lon);
    if (meta->projection) {
        latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0,
            x, y, &projZ);
    } else {
        latLon2UTM(lat*D2R, lon*D2R, 0, x, y);
    }
}


void update_pixel_info()
{
    char buf[512];

    GtkWidget *img = get_widget_checked("big_image");
    GdkPixbuf *shown_pixbuf = gtk_image_get_pixbuf(GTK_IMAGE(img));

    if (crosshair_x < 0 || crosshair_x >= ns || 
        crosshair_y < 0 || crosshair_y >= nl)
    {
        // outside of the image
        strcpy(buf, "");
        crosshair_x = crosshair_y = 0;
    }
    else
    {
        assert(meta);
        assert(shown_pixbuf);
        
        int x = (crosshair_x - cx)/zoom + 450;
        int y = (crosshair_y - cy)/zoom + 450;

        guchar uval = get_pb_pixel(shown_pixbuf, x, y);
        float fval = get_pixel(crosshair_y, crosshair_x);

        double lat, lon;
        meta_get_latLon(meta, crosshair_y, crosshair_x, 0, &lat, &lon);

        sprintf(buf, "Line: %5d, Sample: %5d\n"
            "Pixel Value: %f -> %d\n"
            "Lat: %.3f, Lon: %.3f\n", crosshair_y, crosshair_x,
            fval, (int)uval, lat, lon);

        if (meta->projection) {
            double projX, projY, projZ;
            latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0,
                &projX, &projY, &projZ);
            sprintf(&buf[strlen(buf)], "ProjX: %.1f m, ProjY: %.1f m\n",
                projX, projY);
        }

        if (meta->state_vectors) {
            double s,t;
            meta_get_timeSlantDop(meta, y, x, &t, &s, NULL);
            sprintf(&buf[strlen(buf)], "Incid: %.3f (deg), Slant: %.1f m\n",
                R2D*meta_incid(meta, y, x), s);
        }

        if (cc_x >= 0 && cc_y >= 0) {
            double proj_x_cr, proj_y_cr; // crosshair coords
            line_samp_to_proj(crosshair_y, crosshair_x, &proj_x_cr, &proj_y_cr);
            double proj_x, proj_y;       // ctrl-click coords
            line_samp_to_proj(cc_y, cc_x, &proj_x, &proj_y);
            double d = hypot(proj_x-proj_x_cr, proj_y-proj_y_cr);
            sprintf(&buf[strlen(buf)], "Distance to %d,%d: %.1f m", cc_y, cc_x, d);
        } else {
            sprintf(&buf[strlen(buf)], "Distance: (ctrl-click to measure)");
        }
    }

    GtkWidget *lbl = get_widget_checked("upper_label");
    gtk_label_set_text(GTK_LABEL(lbl), buf);
}

void fill_big()
{
    GdkPixbuf *pb = make_big_image(900, cx, cy, zoom);
    GtkWidget *img = get_widget_checked("big_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
}

SIGNAL_CALLBACK int on_small_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    // temporarily block keypress events...
    widget = get_widget_checked("small_image_eventbox");
    gtk_widget_set_events(widget, 0);
    
    GtkWidget *img = get_widget_checked("small_image");
    GdkPixbuf *pb = gtk_image_get_pixbuf(GTK_IMAGE(img));
    int w = gdk_pixbuf_get_width(pb);
    int h = gdk_pixbuf_get_height(pb);

    cx = (int)event->x * ns / w;
    cy = (int)event->y * nl / h;

    fill_small();
    fill_big();

    while (gtk_events_pending())
        gtk_main_iteration();    

    gtk_widget_set_events(widget, GDK_BUTTON_PRESS_MASK);
    return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    // temporarily block keypress events...
    widget = get_widget_checked("big_image_eventbox");
    gtk_widget_set_events(widget, 0);

    if (event->button == 1) {
        // ctrl-left-click: measure distance
        if (((int)event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) {
            cc_x = ((int)event->x - 450)*zoom + cx;
            cc_y = ((int)event->y - 450)*zoom + cy;
        }
        // left-click: move crosshair
        else {
            crosshair_x = ((int)event->x - 450)*zoom + cx;
            crosshair_y = ((int)event->y - 450)*zoom + cy;
            cc_x = cc_y = -1;
        }
        update_pixel_info();
        fill_big();
    } else if (event->button == 3) {
        // right-click: re-center
        cx = ((int)event->x - 450)*zoom + cx; // gotta fix this hard-coded value
        cy = ((int)event->y - 450)*zoom + cy;
        fill_small();
        fill_big();
    }

    while (gtk_events_pending())
        gtk_main_iteration();    

    gtk_widget_set_events(widget, GDK_BUTTON_PRESS_MASK);
    return TRUE;
}

SIGNAL_CALLBACK int
on_big_image_scroll_event(
    GtkWidget *widget, GdkEventScroll *event, gpointer user_data)
{
    // temporarily block keypress events...
    widget = get_widget_checked("big_image_eventbox");
    gtk_widget_set_events(widget, 0);

    if (event->direction == GDK_SCROLL_UP) {
        if (zoom > 1) --zoom;
    } else if (event->direction == GDK_SCROLL_DOWN) {
        ++zoom;
    }

    fill_small();
    fill_big();

    while (gtk_events_pending())
        gtk_main_iteration();    

    gtk_widget_set_events(widget, GDK_BUTTON_PRESS_MASK);
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
        case GDK_Up: crosshair_y -= incr; break;
        case GDK_Down: crosshair_y += incr; break;
        case GDK_Left: crosshair_x -= incr; break;
        case GDK_Right: crosshair_x += incr; break;
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

    int x = ((int)event->x - 450)*zoom + cx; // gotta fix this hard-coded value
    int y = ((int)event->y - 450)*zoom + cy;

    static GtkWidget *lbl = NULL;
    if (!lbl)
        lbl = get_widget_checked("upper_label");

    char buf[512];
    if (x < 0 || x >= ns || y < 0 || y >= nl)
    {
        // outside of the image
        strcpy(buf, "");
    }
    else
    {
        float fval = get_pixel(y, x);

        double lat, lon;
        meta_get_latLon(meta, y, x, 0, &lat, &lon);

        sprintf(buf, "Line: %d, Sample: %d\n"
            "Pixel Value: %f\n"
            "Lat: %f, Lon: %f\n", crosshair_y, crosshair_x,
            fval, lat, lon);
    }

    gtk_label_set_text(GTK_LABEL(lbl), buf);
    return TRUE;
}
