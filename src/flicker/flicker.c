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

#define VERSION_STRING "0.0.1"

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

gchar *
find_in_path(gchar * file)
{
    gchar *path, *buf, *name, *p;
    int len, pathlen;

    path = (gchar *)g_getenv("PATH");

    len = strlen(file) + 1;
    pathlen = strlen(path);

    /* work area */
    buf = (gchar *) g_malloc( sizeof(gchar) * (pathlen + len + 2) ); 

    /* put separator + filename at the end of the buffer */
    name = buf + pathlen + 1;
    *name = DIR_SEPARATOR;
    memcpy(name + 1, file, len);

    /* now try each path item, prepended to the filename in the work area */
    p = path;
    do
    {
        gchar * start;
        gchar * q = strchr(p + 1, PATH_SEPARATOR);

        /* if separator not found, point to the end */
        if ( !q ) 
            q = path + pathlen;

        start = name - (q - p);

        /* copy path portion to the work area */
        memcpy( start, p, q - p );

        if (g_file_test( start, G_FILE_TEST_EXISTS ))
        {
            gchar * ret = g_strdup(start);
            g_free(buf);
            return ret; 
        }

        p = q;
    } 
    while (*p++ != '\0');

    /* not found! */ 
    g_free(buf);
    return NULL;
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
    if (fileExists("tmp1.jpg"))
        unlink("tmp1.jpg");
    if (fileExists("tmp2.jpg"))
        unlink("tmp2.jpg");
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

static int imx(int a, int b) { return a>b?a:b; }

static char * get_basename(const char *name)
{
    char dirName[255];
    char fileName[255];
    split_dir_and_file(name, dirName, fileName);

    return strdup(fileName);
}

static void load_images(const char *f1, const char *f2)
{
    meta_parameters *m1 = meta_read(f1);
    assert(m1);

    meta_parameters *m2 = meta_read(f2);
    assert(m2);

//    assert(m1->general->line_count == m2->general->line_count);
//    assert(m1->general->sample_count == m2->general->sample_count);

    int lines = imx(m1->general->line_count, m2->general->sample_count);
    int samps = imx(m1->general->sample_count, m2->general->sample_count);
    int max = lines > samps ? lines : samps;
    int ret;

    FloatImage *fi1 = 
        float_image_new_from_file(m1->general->sample_count, 
                                  m1->general->line_count, f1, 0,
                                  FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

    FloatImage *fi2 =
        float_image_new_from_file(m2->general->sample_count, 
                                  m2->general->line_count, f2, 0,
                                  FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN);

    ret = float_image_export_as_jpeg(fi1, "tmp1.jpg", max, 0);
    assert(ret == 0);

    ret = float_image_export_as_jpeg(fi2, "tmp2.jpg", max, 0);
    assert(ret == 0);

    GError *err = NULL;
    flicker_items.i1 = gdk_pixbuf_new_from_file("tmp1.jpg", &err);
    if (!flicker_items.i1)
        g_error("Failed to load tmp1.jpg: %s\n", err->message);
    flicker_items.i2 = gdk_pixbuf_new_from_file("tmp2.jpg", &err);
    if (!flicker_items.i1)
        g_error("Failed to load tmp2.jpg: %s\n", err->message);

    flicker_items.file1 = get_basename(f1);
    flicker_items.file2 = get_basename(f2);
}

static void do_wait(double secs)
{
    double micro_secs = 100000 * secs;
    int interval = 100;
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

static void flicker_thread(FlickerItems *f_items, gpointer user_data)
{
    GtkWidget *img = glade_xml_get_widget(glade_xml, "flicker_image");

    while (1) {
        gtk_image_set_from_pixbuf(GTK_IMAGE(img), f_items->i1);
        set_app_title(f_items->file1);

        do_wait(1);

        if (!keep_going) break;

        gtk_image_set_from_pixbuf(GTK_IMAGE(img), f_items->i2);
        set_app_title(f_items->file2);

        do_wait(1);

        if (!keep_going) break;
    }
}

static void start_flicker()
{
    flicker_thread(&flicker_items, NULL);
    return;

    assert(flicker_items.i1);
    assert(flicker_items.i2);

    static GThreadPool *ttp = NULL;
    GError *err = NULL;

    if (!ttp)
    {
        if (!g_thread_supported ()) g_thread_init (NULL);
        ttp = g_thread_pool_new ((GFunc) flicker_thread, NULL,
                                 2, TRUE, &err);
        assert (err == NULL);
    }

    g_thread_pool_push(ttp, &flicker_items, &err);
    assert(err == NULL);
}

int
main(int argc, char **argv)
{

    if (argc != 3) {
        printf("Flicker!\n\n"
               "Assumes the images have the same pixel size and are aligned\n"
               "at the top left corner.\n\n");

        printf("Usage: flicker <file1.img> <file2.img>\n");
        exit(EXIT_FAILURE);
    }

    gchar *glade_xml_file;

    gtk_init(&argc, &argv);

    glade_xml_file = (gchar *) find_in_share("flicker.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    set_app_title(NULL);
    set_font();
    load_images(argv[1], argv[2]);

    glade_xml_signal_autoconnect(glade_xml);
    start_flicker();

    if (keep_going)
        gtk_main ();

    exit (EXIT_SUCCESS);
}
