#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <sys/wait.h>
#include <errno.h>
#include <assert.h>

#define VERSION_STRING "0.1.0"

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

/* for win32, set the font to the standard windows one */
#if defined(win32)
#include <pango/pango.h>

#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR

static char appfontname[128] = "tahoma 8"; /* fallback value */

static void set_app_font (const char *fontname)
{
    GtkSettings *settings;

    if (fontname != NULL && *fontname == 0) return;

    settings = gtk_settings_get_default();

    if (fontname == NULL) {
        g_object_set(G_OBJECT(settings), "gtk-font-name", appfontname, NULL);
    } else {
        GtkWidget *w;
        PangoFontDescription *pfd;
        PangoContext *pc;
        PangoFont *pfont;

        w = gtk_label_new(NULL);
        pfd = pango_font_description_from_string(fontname);
        pc = gtk_widget_get_pango_context(w);
        pfont = pango_context_load_font(pc, pfd);

        if (pfont != NULL) {
            strcpy(appfontname, fontname);
            g_object_set(G_OBJECT(settings), "gtk-font-name", appfontname,
                NULL);
        }

        gtk_widget_destroy(w);
        pango_font_description_free(pfd);
    }
}

char *default_windows_menu_fontspec (void)
{
    gchar *fontspec = NULL;
    NONCLIENTMETRICS ncm;

    memset(&ncm, 0, sizeof ncm);
    ncm.cbSize = sizeof ncm;

    if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0)) {
        HDC screen = GetDC(0);
        double y_scale = 72.0 / GetDeviceCaps(screen, LOGPIXELSY);
        int point_size = (int) (ncm.lfMenuFont.lfHeight * y_scale);

        if (point_size < 0) point_size = -point_size;
        fontspec = g_strdup_printf("%s %d", ncm.lfMenuFont.lfFaceName,
            point_size);
        ReleaseDC(0, screen);
    }

    return fontspec;
}

static void try_to_get_windows_font (void)
{
    gchar *fontspec = default_windows_menu_fontspec();

    if (fontspec != NULL) {
        int match = 0;
        PangoFontDescription *pfd;
        PangoFont *pfont;
        PangoContext *pc;
        GtkWidget *w;

        pfd = pango_font_description_from_string(fontspec);

        w = gtk_label_new(NULL);
        pc = gtk_widget_get_pango_context(w);
        pfont = pango_context_load_font(pc, pfd);
        match = (pfont != NULL);

        pango_font_description_free(pfd);
        g_object_unref(G_OBJECT(pc));
        gtk_widget_destroy(w);

        if (match) set_app_font(fontspec);
        g_free(fontspec);
    }
}

void set_font ()
{
    try_to_get_windows_font();
}

#else /* defined(win32) */

#include "asf.h"
#include "asf_meta.h"

/* on unix, GTK will select the appropriate fonts */
void set_font () {}

#endif /* defined(win32) */

#include <asf_raster.h>
#include <asf_meta.h>

#ifdef win32
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
#ifdef PATH_SEPARATOR
#undef PATH_SEPARATOR
#endif
#ifdef DIR_SEPARATOR
#undef DIR_SEPARATOR
#endif
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR = '/';
#endif

GladeXML *glade_xml;

typedef struct fis
{
    GdkPixbuf *i1, *i2;
    char *file1, *file2;
} FlickerItems;

FlickerItems flicker_items;
int keep_going = TRUE;

static char * escapify(const char * s)
{
    int i,j;
    char * ret = MALLOC(2*strlen(s)*sizeof(char));
    for (i = 0, j = 0; i <= strlen(s); ++i)
    {
        switch(s[i])
        {
            case '\\':
                ret[j] = ret[j+1] = s[i];
                ++j;
                break;
            default:
                ret[j] = s[i];
                break;
        }
        ++j;
    }

    return ret;
}

static char *
find_in_share(const char * filename)
{
    char * escaped_dir = escapify(get_asf_share_dir());
    char * ret = (char *) MALLOC(sizeof(char) *
        (strlen(escaped_dir) + strlen(filename) + 2));
    sprintf(ret, "%s/%s", escaped_dir, filename);
    free(escaped_dir);
    return ret;
}

void
set_app_title(char *file)
{
    /* add version number to window title */
    char title [256];
    GtkWidget *widget;

    if (file)
        sprintf (title, "Flicker: Version %s - %s", VERSION_STRING, file);
    else
        sprintf (title, "Flicker: Version %s", VERSION_STRING);

    widget = glade_xml_get_widget (glade_xml, "flicker_window");
    gtk_window_set_title(GTK_WINDOW(widget), title);
}

static void quit()
{
    keep_going = FALSE;
    //gtk_main_quit();
}

SIGNAL_CALLBACK void
on_flicker_window_destroy(GtkWidget *w, gpointer data)
{
    quit();
}

SIGNAL_CALLBACK void
on_flicker_window_destroy_event(GtkWidget *w, gpointer data)
{
    quit();
}

SIGNAL_CALLBACK void
on_flicker_window_delete_event(GtkWidget *w, gpointer data)
{
    quit();
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    g_free(pixels);
}

static GdkPixbuf *img2pb(meta_parameters *meta, const char *img_file)
{
    asfPrintStatus("Flicker: Gathering stats for %s\n", img_file);

    FILE *fpIn = fopen(img_file, "rb");
    if (!fpIn)
        return NULL;

    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    // Form the thumbnail image by grabbing individual pixels.
    int ii, jj;

    // First compute stats by reading in a 1/50th scale image
    int f = 50;
    int l = nl / f;
    int s = ns / f;
    float *fdata = MALLOC(sizeof(float)*l*s);
    float *line = MALLOC(sizeof(float)*ns);

    // Keep track of the average pixel value, so later we can do a 2-sigma
    // scaling - makes the thumbnail look a little nicer and more like what
    // they'd get if they did the default jpeg export.
    int nn = 0;
    double avg = 0.0;
    for ( ii = 0 ; ii < l ; ii++ ) {

        get_float_line(fpIn, meta, ii*f, line);

        for (jj = 0; jj < s; ++jj) {
            fdata[nn] = line[jj*f];
            avg += fdata[nn++];
            if (nn>l*s)
              asfPrintError("Bad: %d,%d -> %d : %d,%d\n", ii,jj,nn,l,s);
            assert(nn<=l*s);
        }

        //asfPercentMeter((float)(ii+1)/l);
    }
    fclose(fpIn);

    // Compute the std devation
    avg /= l*s;
    double stddev = 0.0;
    for (ii = 0; ii < l*s; ++ii)
        stddev += ((double)fdata[ii] - avg) * ((double)fdata[ii] - avg);
    stddev = sqrt(stddev / (l*s));
    FREE(fdata);

    // Set the limits of the scaling - 2-sigma on either side of the mean
    double lmin = avg - 2*stddev;
    double lmax = avg + 2*stddev;

    guchar *data = g_new(guchar, 3*nl*ns);
    fpIn = fopen(img_file, "rb");
    assert(fpIn);

    //asfPrintStatus("Loading image data...\n");

    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    for (ii = 0; ii < nl; ++ii) {

        get_float_line(fpIn, meta, ii, line);

        for (jj = 0; jj < ns; ++jj) {

          float val = line[jj];
          guchar uval;
          if (val < lmin)
            uval = 0;
          else if (val > lmax)
            uval = 255;
          else
            uval = (guchar) round(((val - lmin) / (lmax - lmin)) * 255);
          
          nn = 3*(ii*ns + jj);
          assert(nn+2 < nl*ns*3);

          data[nn] = uval;
          data[nn+1] = uval;
          data[nn+2] = uval;
        }

        asfPercentMeter((float)(ii+1)/nl);
    }

    fclose(fpIn);
    free (line);

    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE,
                                 8, ns, nl, ns*3, destroy_pb_data, NULL);

    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", img_file);
        meta_free(meta);
        g_free(data);
        return NULL;
    }

    return pb;
}

static void load_images(const char *f1, const char *f2)
{
    meta_parameters *m1 = meta_read(f1);
    assert(m1);

    meta_parameters *m2 = meta_read(f2);
    assert(m2);

    flicker_items.i1 = img2pb(m1, f1);
    flicker_items.i2 = img2pb(m2, f2);

    if (!flicker_items.i1)
        asfPrintError("Failed to load %s!\n", f1);
    if (!flicker_items.i2)
        asfPrintError("Failed to load %s!\n", f2);

    flicker_items.file1 = get_basename(f1);
    flicker_items.file2 = get_basename(f2);
    
    meta_free(m1);
    meta_free(m2);
}

static void do_wait(double secs)
{
    double micro_secs = 500000 * secs;
    int interval = 500;
    int count = 0;

    while (count < micro_secs) {
        while (gtk_events_pending())
            gtk_main_iteration();

        if (!keep_going)
            break;

        g_usleep(interval);
        count += interval;
    }
}

static void start_flicker()
{
    GtkWidget *img = glade_xml_get_widget(glade_xml, "flicker_image");

    while (keep_going) {
        gtk_image_set_from_pixbuf(GTK_IMAGE(img), flicker_items.i1);
        set_app_title(flicker_items.file1);

        do_wait(1);

        if (!keep_going) break;

        gtk_image_set_from_pixbuf(GTK_IMAGE(img), flicker_items.i2);
        set_app_title(flicker_items.file2);

        do_wait(1);

        if (!keep_going) break;
    }
}

int
main(int argc, char **argv)
{

    if (argc != 3) {
        printf("Flicker\n\n"
               "Assumes the images have the same pixel size and are aligned\n"
               "at the corners.  Each file must be in the ASF Internal "
               "format.\n\n");

        printf("Usage: flicker <file1> <file2>\n");
        exit(EXIT_FAILURE);
    }

    gchar *glade_xml_file;

    gtk_init(&argc, &argv);

    glade_xml_file = (gchar *) find_in_share("flicker.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    set_app_title(NULL);
    set_font();
    
    char *img1 = appendExt(argv[1], ".img");
    char *img2 = appendExt(argv[2], ".img");

    load_images(img1, img2);

    glade_xml_signal_autoconnect(glade_xml);
    start_flicker(&flicker_items);

    if (keep_going)
        gtk_main ();

    free(img1);
    free(img2);

    exit (EXIT_SUCCESS);
}
