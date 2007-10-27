#include "asf_view.h"

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

// loaded metadata
meta_parameters *meta;

// loaded image data
CachedImage *data_ci;

// various values
int nl, ns;
double zoom;
double center_samp, center_line;
double crosshair_line, crosshair_samp;
int g_saved_line_count;

// loaded filename
char *g_filename;

// data & meta filenames
char *g_data_name;
char *g_meta_name;

char *find_in_share(const char * filename)
{
    char * ret = MALLOC(sizeof(char) *
        (strlen(get_asf_share_dir()) + strlen(filename) + 5));
    sprintf(ret, "%s/%s", get_asf_share_dir(), filename);
    return ret;
}

SIGNAL_CALLBACK void
on_ssv_main_window_delete_event(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

/* danger: returns pointer to static data!! */
static const char * imgloc(char * file)
{
    static char loc[1024];
    gchar * tmp = find_in_share(file);
    if (tmp) {
      strcpy(loc, tmp);
      g_free(tmp);
    } else {
      strcpy(loc, file);
    }

    return loc;
}

void set_toolbar_images()
{
    GtkWidget * w = get_widget_checked("google_earth_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("earth2.gif"));

    w = get_widget_checked("mdv_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("information_icon.gif"));
}

int
main(int argc, char **argv)
{
    char band[512];
    int band_specified = extract_string_options(&argc, &argv, band,
        "-band", "--band", "-b", NULL);

    if (argc < 2) {
        g_filename = STRDUP(find_in_share("startup.jpg"));
    } else {
        if (argc > 2)
          asfPrintWarning("Extraneous command-line arguments ignored.\n");
        g_filename = STRDUP(argv[1]);
    }

    // we could call load_file() here, but don't because this way we can
    // interleave the call to gtk_init() with some of the loading code --
    // which keeps the window from showing up until after it has been loaded,
    // which looks much nicer

    // initialize globals
    reset_globals(TRUE);

    g_data_name = g_meta_name = NULL;

    // strip off a trailing "."
    if (g_filename[strlen(g_filename)-1] == '.')
        g_filename[strlen(g_filename)-1] = '\0';

    read_file(g_filename, band_specified ? band : NULL, FALSE, TRUE);

    assert(g_data_name);
    assert(g_meta_name);

    // we load the thumbnail data before bringing up the window, looks
    // much nicer.  When loading an image within the GUI, we don't need
    // to do get_thumbnail_data() as a separate step.
    ThumbnailData *thumbnail_data = get_thumbnail_data();
    gtk_init(&argc, &argv);

    //GtkWidget *eb = get_widget_checked("big_image_eventbox");
    //int e = gtk_widget_get_events(eb);
    //gtk_widget_set_events(eb, e | GDK_KEY_PRESS_MASK | GDK_POINTER_MOTION_MASK);

    gchar *glade_xml_file = (gchar *) find_in_share("asf_view.glade");
    printf("Found asf_view.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    // set up window title, etc
    set_title(band_specified, band);
    set_toolbar_images();

    // populate the look up table list, and apply the default
    // look-up-table, if there is one.  In this case, we will need to
    // apply it retroactively to the thumbnail data we already loaded
    // (In new.c, this kludge isn't required - we load/apply in the
    // same pass -- here it is required because we pre-load the thumbnail)
    populate_lut_combo();
    if (meta && meta->general)  {
        if (set_lut_based_on_image_type(meta->general->image_data_type)) {
            check_lut();
            // data we loaded needs to be lutted
            apply_lut_to_data(thumbnail_data);
        }
    }

    // load the metadata & image data, other setup
    fill_small_have_data(thumbnail_data);
    fill_big();
    update_pixel_info();
    update_zoom();
    set_font();
    fill_meta_info();
    fill_stats();
    setup_bands_tab(meta);

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    if (data_ci) cached_image_free(data_ci);
    if (meta) meta_free(meta);
    if (g_filename) free(g_filename);
    if (g_data_name) free(g_meta_name);
    if (g_meta_name) free(g_data_name);

    exit (EXIT_SUCCESS);
}

