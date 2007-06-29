#include "ssv.h"

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

// loaded metadata
meta_parameters *meta;

// loaded image data -- only one of these will be non-NULL
float *data;
FloatImage *data_fi;

// various values
int nl, ns;
double g_min, g_max;
double zoom;
double center_samp, center_line;
double crosshair_line, crosshair_samp;
double ctrl_clk_samp, ctrl_clk_line;

// loaded filename
char *g_filename;

float get_pixel(int line, int sample)
{
    if (data)
        return data[sample + line*ns];
    else
        return float_image_get_pixel(data_fi, sample, line);
}

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

int
main(int argc, char **argv)
{
    char band[512];
    int band_specified = extract_string_options(&argc, &argv, band,
        "-band", "--band", "-b", NULL);

    if (argc != 2) {
        printf("Usage: ssv <filename>\n\n"
            "<filename> should be an ASF internal format image file.\n");
        exit(1);
    }

    // we could call load_file() here, but don't because this way we can
    // interleave the call to gtk_init() with some of the loading code --
    // which keeps the window from showing up until after it has been loaded,
    // which looks much nicer

    // initialize globals
    reset_globals();

    g_filename = STRDUP(argv[1]);

    // strip off a trailing "."
    if (g_filename[strlen(g_filename)-1] == '.')
        g_filename[strlen(g_filename)-1] = '\0';

    read_file(g_filename, band_specified ? band : NULL);

    gtk_init(&argc, &argv);

    //GtkWidget *eb = get_widget_checked("big_image_eventbox");
    //int e = gtk_widget_get_events(eb);
    //gtk_widget_set_events(eb, e | GDK_KEY_PRESS_MASK | GDK_POINTER_MOTION_MASK);

    gchar *glade_xml_file = (gchar *) find_in_share("ssv.glade");
    printf("Found ssv.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    // set up window title
    set_title(band_specified, band);

    // load the metadata & image data, other setup
    fill_small();
    fill_big();
    update_pixel_info();
    update_zoom();
    set_font();
    fill_meta_info();
    calc_image_stats(); // starts a thread

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    if (data) free(data);
    if (data_fi) float_image_free(data_fi);
    if (meta) meta_free(meta);
    if (g_filename) free(g_filename);

    exit (EXIT_SUCCESS);
}

