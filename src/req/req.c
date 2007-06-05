#include "req.h"

#define VERSION "1.0"

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

char *find_in_share(const char * filename)
{
    char * ret = MALLOC(sizeof(char) *
        (strlen(get_asf_share_dir()) + strlen(filename) + 5));
    sprintf(ret, "%s/%s", get_asf_share_dir(), filename);
    return ret;
}

SIGNAL_CALLBACK void
on_req_main_window_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

int
main(int argc, char **argv)
{
    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *) find_in_share("req.glade");
    printf("Found req.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    g_free(glade_xml_file);

    // add version number to window title
    char title[256];
    sprintf(title,
            "The ALOS Acquisition File Generator: Version %s", VERSION);

    // pull out what is in the saved settings file
    apply_saved_settings();
    populate_csvs();

    GtkWidget *widget = get_widget_checked("req_main_window");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    set_font();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    exit (EXIT_SUCCESS);
}

