#include "proj2proj.h"

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
on_proj2proj_main_window_delete_event(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

int
main(int argc, char **argv)
{
    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *) find_in_share("proj2proj.glade");
    printf("Found proj2proj.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    set_font();

    glade_xml_signal_autoconnect(glade_xml);

    geocode_options_changed(TRUE);
    geocode_options_changed(FALSE);

    gtk_main ();

    exit (EXIT_SUCCESS);
}

