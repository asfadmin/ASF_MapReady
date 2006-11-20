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

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

#define IPS_GUI_VERSION "0.0.1"

/* for win32, set the font to the standard windows one */
#if defined(win32)
#include <pango/pango.h>

#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <Windows.h>
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
#if defined(DIR_SEPARATOR)
#undef DIR_SEPARATOR
#endif

/* on unix, GTK will select the appropriate fonts */
void set_font () {}

#endif /* defined(win32) */

#ifdef win32
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR = '/';
#endif

#include "ips.h"

GladeXML *glade_xml;

static char *
find_in_share(const char * filename)
{
    char * ret = (char *) malloc(sizeof(char) *
                      (strlen(get_asf_share_dir()) + strlen(filename) + 5));
    sprintf(ret, "%s/%s", get_asf_share_dir(), filename);
    return ret;
}

void
add_file(const char *config_file)
{
}

int
main(int argc, char **argv)
{
    gchar *glade_xml_file;
    gtk_init(&argc, &argv);

    glade_xml_file = (gchar *) find_in_share("gips.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    if (argc > 1)
        add_file(argv[1]);

    /* add version number to window title */
    char title[256];
    sprintf(title,
            "ASF Interferometric Processing System: Version %s", IPS_GUI_VERSION);

    GtkWidget *widget = glade_xml_get_widget (glade_xml, "ips_main");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    //set_font();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    exit (EXIT_SUCCESS);
}
