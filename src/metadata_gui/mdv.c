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

static void add_file (const gchar * filename);

SIGNAL_CALLBACK void
on_browse_button_clicked(GtkWidget *button)
{
#if defined(win32)
    OPENFILENAME of;
    int retval;
    char fname[1024];

    fname[0] = '\0';

    memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
    of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
    of.lStructSize = sizeof(of);
#endif

    of.hwndOwner = NULL;
    of.lpstrFilter = "CEOS Leader File (*.L)\0*.L\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            printf("File dialog box error");
        return;
    }

    add_file(fname);

#else

    GtkWidget * file_selection_dialog =
        glade_xml_get_widget (glade_xml, "file_selection_dialog");

    gtk_widget_show (file_selection_dialog);

#endif
}

static void
hide_file_selection_dialog ()
{
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "file_selection_dialog");

    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_file_selection_dialog_cancel_button_clicked(GtkWidget *w)
{
    hide_file_selection_dialog ();
}

SIGNAL_CALLBACK void
on_metadata_viewer_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

SIGNAL_CALLBACK void
on_metadata_viewer_destroy_event(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

SIGNAL_CALLBACK void
on_metadata_viewer_delete_event(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

void
append_output(const gchar * txt, GtkWidget * textview_output)
{
    GtkTextBuffer * text_buffer;
    GtkTextIter end;
    GtkTextTag *tt;
    GtkTextTagTable *tt_table;

    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview_output));
    tt_table = gtk_text_buffer_get_tag_table(text_buffer);    
    tt = gtk_text_tag_table_lookup(tt_table, "mono");

    if (!tt)
    {

#ifdef win32
        const char *fnt = "Courier";
#else
	const char *fnt = "Mono";
#endif


	tt = gtk_text_buffer_create_tag(text_buffer, "mono", 
					"font", fnt, NULL);
    }

    if (gtk_text_buffer_get_char_count(text_buffer) > 0)
    {
        GtkTextIter b, e;

        gtk_text_buffer_get_start_iter(text_buffer, &b);
        gtk_text_buffer_get_end_iter(text_buffer, &e);
        gtk_text_buffer_delete(text_buffer, &b, &e);
    }

    gtk_text_buffer_get_end_iter(text_buffer, &end);
    gtk_text_buffer_insert_with_tags(text_buffer, &end, txt, -1, tt, NULL);
}

static void
put_text_in_textview(const char * text, const char * tv)
{
    gchar widget_name[128];
    sprintf(widget_name, "%s_textview", tv);

    GtkWidget * w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w) { printf("Bad: %s\n", widget_name); return; }

    append_output(text, w);
}

static void
put_metadata_in_textview(const char * file, int reqrec, const char * tv)
{
    char *rec = get_record_as_string((char*)file, reqrec);
    put_text_in_textview(rec, tv);

    FREE(rec);
}

static void execute()
{
    GtkWidget * input_file_entry =
        glade_xml_get_widget(glade_xml, "input_file_entry");

    const char * input_file =
        gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    put_metadata_in_textview(input_file, 10, "data_set_summary");
    put_metadata_in_textview(input_file, 20, "map_projection_data");
    put_metadata_in_textview(input_file, 30, "platform_position_data");
    put_metadata_in_textview(input_file, 40, "attitude_data");
    put_metadata_in_textview(input_file, 50, "radiometric_data");
    put_metadata_in_textview(input_file, 60, "data_quality_summary");
    put_metadata_in_textview(input_file, 70, "processed_data_histograms");
    put_metadata_in_textview(input_file, 71, "signal_data_histograms");
    put_metadata_in_textview(input_file, 80, "range_spectra");
    put_metadata_in_textview(input_file, 200, "facility_related_data");
    put_metadata_in_textview(input_file, 192, "image_file_descriptor");
    put_metadata_in_textview(input_file, 300, "leader_file_descriptor");
}

static void add_file (const gchar * filename)
{
    GtkWidget *input_file_entry;

    input_file_entry =
        glade_xml_get_widget(glade_xml, "input_file_entry");

    gtk_entry_set_text(GTK_ENTRY(input_file_entry), filename);

    execute();
}

SIGNAL_CALLBACK void
on_execute_button_clicked(GtkWidget *button, gpointer user_data)
{
    execute();
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_delete_event(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_destroy_event(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_destroy(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK void
on_file_selection_dialog_ok_button_clicked(GtkWidget *w)
{
    GtkWidget *file_selection_dialog;

    gchar **selections;
    gchar **current;

    file_selection_dialog =
        glade_xml_get_widget(glade_xml, "file_selection_dialog");

    selections = gtk_file_selection_get_selections(
        GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;

    while (*current)
    {	
        add_file(*current);
        ++current;
    }

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);

    execute();
}

void
set_app_title()
{
    /* add version number to window title */
    char title [256];
    GtkWidget *widget;

    sprintf (title,
	     "Alaska Satellite Facility Metadata Viewer: Version %s",
	     CONVERT_PACKAGE_VERSION_STRING);

    widget = glade_xml_get_widget (glade_xml, "metadata_viewer");
    gtk_window_set_title(GTK_WINDOW(widget), title);
}

int
main(int argc, char **argv)
{
    gchar *glade_xml_file;

    gtk_init(&argc, &argv);

    // glade_xml_file = (gchar *) find_in_path("mdv.glade");
    glade_xml_file = (gchar *) find_in_share("mdv.glade");
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
