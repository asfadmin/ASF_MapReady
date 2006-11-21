#include "ait.h"
#include <stdarg.h>

/* for win32, set the font to the standard windows one */
#if defined(win32)

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

#else /* #if defined(win32) */

/* on unix, GTK will select the appropriate fonts */
void set_font () {}

#endif /* defined(win32) */

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

// We aren't using the asf.h ones because of conflicts in Windows.h
#if defined(win32)
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR = '/';
#endif

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

SIGNAL_CALLBACK void
on_ait_main_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

SIGNAL_CALLBACK void
on_execute_button_clicked(GtkWidget *w)
{
}

SIGNAL_CALLBACK void
on_save_button_clicked(GtkWidget *w)
{
    char cfg_name[512];
    dem_config *cfg = get_settings_from_gui(cfg_name);
    if (strlen(cfg_name) > 0) {
        write_config(cfg_name, cfg);
        message_box("Wrote configuration file: %s\n", cfg_name);
    }
    else
        message_box("Please enter a name for the configuration file.");
    free(cfg);
}

SIGNAL_CALLBACK void
on_stop_button_clicked(GtkWidget *w)
{
}

void
message_box(const char *format, ...)
{
    char buf[1024];
    int len;

    va_list ap;
    va_start(ap, format);
    len = vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);

    if (len > 1022)
        asfPrintWarning("Lengthy message may have been truncated.\n");

    GtkWidget *dialog, *label;

    dialog = gtk_dialog_new_with_buttons( "Message",
        NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_STOCK_OK,
        GTK_RESPONSE_NONE,
        NULL);

    label = gtk_label_new(buf);

    g_signal_connect_swapped(dialog, 
        "response", 
        G_CALLBACK(gtk_widget_destroy),
        dialog);

    gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

    gtk_widget_show_all(dialog);
}

void show_summary(int show)
{
    GtkWidget *summary_scrolledwindow =
        glade_xml_get_widget(glade_xml, "summary_scrolledwindow");

    GtkWidget *viewed_image_scrolledwindow =
        glade_xml_get_widget(glade_xml, "viewed_image_scrolledwindow");

    if (show) {
        gtk_widget_show(summary_scrolledwindow);
        gtk_widget_hide(viewed_image_scrolledwindow);
    } else {
        gtk_widget_hide(summary_scrolledwindow);
        gtk_widget_show(viewed_image_scrolledwindow);
    }
}

int
main(int argc, char **argv)
{
    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *) find_in_share("ait.glade");
    //printf("Found ait.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    if (argc > 1)
        add_file(argv[1]);

    // add version number to window title
    char title[256];
    sprintf(title,
            "AIT - The ASF Interferometry Tool: Version %s", IPS_GUI_VERSION);

    // show the summary view, not the image preview
    show_summary(TRUE);

    GtkWidget *widget = glade_xml_get_widget (glade_xml, "ait_main");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    //set_font();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    exit (EXIT_SUCCESS);
}

