#include "req.h"

#define VERSION "1.0.2"

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
    if (fileExists(ret))
        return ret;
    else {
        free(ret);
        return NULL;
    }
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
    block_processing = FALSE;

    char *glade_xml_file = find_in_share("req.glade");
    if (!glade_xml_file) {
        printf("Couldn't find the req.glade file.  Aborting...\n");
        exit(1);
    }

    printf("Found req.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    g_free(glade_xml_file);

    // add version number to window title
    char title[256];
    sprintf(title,
        "The ALOS Observation Request Generator: Version %s", VERSION);

    // pull out what is in the saved settings file
    apply_saved_settings();
    populate_csvs();
    update_output_file();
    hook_up_csv_dir_entry_changed();

    GtkWidget *widget = get_widget_checked("req_main_window");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    set_font();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    exit (EXIT_SUCCESS);
}

