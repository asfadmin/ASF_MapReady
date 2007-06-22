#include "ssv.h"

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
int nl, ns;
double g_min, g_max;
double zoom;
double center_samp, center_line;
double crosshair_line, crosshair_samp;
double ctrl_clk_samp, ctrl_clk_line;

// loaded filename
char *g_file;

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

    // initialize globals
    data = NULL;
    data_fi = NULL;
    meta = NULL;
    g_min = g_max = 0;
    center_line = center_samp = crosshair_samp = crosshair_line = -1;
    ctrl_clk_line = ctrl_clk_samp = -1;
    zoom = 1;
    g_file = STRDUP(argv[1]);

    read_file(g_file, band_specified ? band : NULL);

    gtk_init(&argc, &argv);

    //GtkWidget *eb = get_widget_checked("big_image_eventbox");
    //int e = gtk_widget_get_events(eb);
    //gtk_widget_set_events(eb, e | GDK_KEY_PRESS_MASK | GDK_POINTER_MOTION_MASK);

    gchar *glade_xml_file = (gchar *) find_in_share("ssv.glade");
    printf("Found ssv.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    // set up window title
    char title[256];
    sprintf(title, "ssv ver %s: %s", VERSION, g_file);
    if (band_specified) {
        sprintf(&title[strlen(title)], " (%s)", band);
    } else if (meta && meta->general && meta->general->band_count > 1) {
        if (strlen(meta->general->bands) > 0) {
            strcpy(band, meta->general->bands);
            char *p = strchr(band, ',');
            if (p) *p = '\0';
        } else if (strncmp_case(g_file, "IMG-", 4) == 0) {
            strcpy(band, g_file+4);
            char *p = strchr(band, '-');
            if (p) *p = '\0';
        } else {
            strcpy(band, "");
        }
        if (strlen(band) > 0)
            sprintf(&title[strlen(title)], " (%s)", band);
    }

    GtkWidget *widget = get_widget_checked("ssv_main_window");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    // load the metadata & image data, other setup
    fill_small();
    fill_big();
    update_pixel_info();
    update_zoom();
    set_font();
    fill_meta_info();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    if (data) free(data);
    if (data_fi) float_image_free(data_fi);
    if (meta) meta_free(meta);
    if (g_file) free(g_file);

    exit (EXIT_SUCCESS);
}

