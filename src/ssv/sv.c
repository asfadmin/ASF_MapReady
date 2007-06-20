#include "sv.h"

#define VERSION "1.0"

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
double g_min;
double g_max;
int cx, cy;
int nl, ns;
int zoom;
int crosshair_x, crosshair_y;

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
on_sv_main_window_delete_event(GtkWidget *w, gpointer data)
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
        printf("Usage: sv <filename>\n\n"
            "<filename> should be an ASF internal format image file.\n");
        exit(1);
    }

    // initialize globals
    data = NULL;
    data_fi = NULL;
    meta = NULL;
    g_min = g_max = 0;
    cx = cy = crosshair_x = crosshair_y = 0;
    zoom = 1;

    read_file(argv[1], band_specified ? band : NULL);

    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *) find_in_share("sv.glade");
    printf("Found sv.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    g_free(glade_xml_file);

    // set up window title
    char title[256];
    if (band_specified) {
        sprintf(title, "sv ver %s: %s (%s)", VERSION, argv[1], band);
    } else {
        sprintf(title, "sv ver %s: %s", VERSION, argv[1]);
    }
    GtkWidget *widget = get_widget_checked("sv_main_window");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    // load the metadata & image data, other setup
    fill_small();
    fill_big();
    update_pixel_info();
    set_font();
    fill_meta_info();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    if (data) free(data);
    if (data_fi) float_image_free(data_fi);
    if (meta) meta_free(meta);

    exit (EXIT_SUCCESS);
}

