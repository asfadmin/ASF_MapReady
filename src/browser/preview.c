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
#include "metadisplay.h"
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
#include "metadisplay.h"

/* on unix, GTK will select the appropriate fonts */
void set_font () {}

#endif /* defined(win32) */

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
const char *cache_dir = "~/.asf/cache";

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

GtkWidget *get_widget_checked(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_widget_checked() failed: "
            "The widget %s was not found.\n", widget_name);
    }
    return w;
}

void
set_app_title()
{
    /* add version number to window title */
    char title [256];
    
    sprintf (title,
	     "ASF CEOS Previewer: Version %s",
	     CONVERT_PACKAGE_VERSION_STRING);

    GtkWidget *widget = get_widget_checked ("preview_window");
    gtk_window_set_title(GTK_WINDOW(widget), title);
}

void put_cached_image(const char *basename, const char *append,
                      const char *widget_name)
{
    char *filename = MALLOC(sizeof(char)*(strlen(basename)+strlen(cache_dir)+
        strlen(append)+10));
    if (strlen(append) > 0)
        sprintf(filename, "%s/%s/%s_%s.jpg", cache_dir, basename, basename, append);
    else
        sprintf(filename, "%s/%s/%s.jpg", cache_dir, basename, basename);
    GtkWidget *img = get_widget_checked(widget_name);
    gtk_image_set_from_file(GTK_IMAGE(img), filename);
}

void add_file(const char *data_file)
{
    char *base = get_basename(data_file);

    put_cached_image(base, "loc1", "image_loc1");
    put_cached_image(base, "loc2", "image_loc2");
    put_cached_image(base, "loc3", "image_loc3");
    put_cached_image(base, "thumb_sml", "image_radar");
}

int
main(int argc, char **argv)
{
    gchar *glade_xml_file;

    gtk_init(&argc, &argv);

    glade_xml_file = (gchar *) find_in_share("preview.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    g_free(glade_xml_file);

    set_app_title();
    set_font();

    if (argc > 1)
        add_file(argv[1]);

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    exit (EXIT_SUCCESS);
}
