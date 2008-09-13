#include "asf_view.h"

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

ImageInfo image_info[2];
ImageInfo *curr=NULL;
int n_images_loaded=1;

// various values
double zoom;
double center_samp, center_line;
double crosshair_line, crosshair_samp;
int g_saved_line_count;

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

static void set_button_images()
{
    GtkWidget * w = get_widget_checked("google_earth_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("google_earth_button.gif"));

    w = get_widget_checked("mdv_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("ceos_metadata.png"));

    w = get_widget_checked("save_subset_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("save_as.png"));

    w = get_widget_checked("save_setup_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("save_as.png"));
}

int
main(int argc, char **argv)
{
    char band[512], lut[512];
    int band_specified = extract_string_options(&argc, &argv, band,
        "-band", "--band", "-b", NULL);
    int lut_specified = extract_string_options(&argc, &argv, lut,
        "-lut", "--lut", NULL);
    int planner_mode = extract_flag_options(&argc, &argv,
        "-plan", "--plan", NULL);

    handle_common_asf_args(&argc, &argv, "ASF View");

    // point to "polygon 0" as the one we initially work on
    g_poly = &g_polys[0];

    // set up image array
    curr = &image_info[0];
    curr->data_name = curr->meta_name = NULL;

    if (argc < 2) {
        curr->filename = STRDUP(find_in_share("startup.jpg"));
    }
    else {
        if (argc > 3)
          asfPrintWarning("Extraneous command-line arguments ignored.\n");
        if (argc > 2)
          image_info[1].filename = STRDUP(argv[2]);
        curr->filename = STRDUP(argv[1]);
    }

    // we could call load_file() here, but don't because this way we can
    // interleave the call to gtk_init() with some of the loading code --
    // which keeps the window from showing up until after it has been loaded,
    // which looks much nicer

    // initialize globals
    reset_globals(TRUE);

    // Get rid of leftover (temporary) colormap luts if they exist, say if asf_view errored out
    // rather than being exited normally
    char embedded_tiff_lut_file[1024];
    char embedded_asf_colormap_file[1024];
    char *lut_loc = (char *)MALLOC(sizeof(char)*(strlen(get_asf_share_dir())+64));
    sprintf(lut_loc, "%s%clook_up_tables", get_asf_share_dir(), DIR_SEPARATOR);
    sprintf(embedded_tiff_lut_file,"%s%c%s", lut_loc, DIR_SEPARATOR, EMBEDDED_TIFF_COLORMAP_LUT_FILE);
    sprintf(embedded_asf_colormap_file,"%s%c%s", lut_loc, DIR_SEPARATOR, EMBEDDED_ASF_COLORMAP_LUT_FILE);
    FREE(lut_loc);
    if (fileExists(embedded_tiff_lut_file)) remove(embedded_tiff_lut_file);
    if (fileExists(embedded_asf_colormap_file)) remove(embedded_asf_colormap_file);

    // strip off any trailing "."
    if (curr->filename[strlen(curr->filename)-1] == '.')
        curr->filename[strlen(curr->filename)-1] = '\0';

    read_file(curr->filename, band_specified ? band : NULL, FALSE, TRUE);
    if (check_for_embedded_tiff_lut(curr->filename, &lut_specified, lut)) {
      populate_lut_combo();
    }
    if (lut_specified)
      set_lut(lut);

    assert(curr->data_name);
    assert(curr->meta_name);

    // we load the thumbnail data before bringing up the window, looks
    // much nicer.  When loading an image within the GUI, we don't need
    // to do get_thumbnail_data() as a separate step.
    ThumbnailData *thumbnail_data = get_thumbnail_data(curr);
    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *)find_in_share("asf_view.glade");
    printf("Found asf_view.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    // set up window title, etc
    set_title(band_specified, band);
    set_button_images();

    // set up the acquisition planner, if we are in that mode
    if (planner_mode) setup_planner();

    // populate the look up table list, and apply the default
    // look-up-table, if there is one.  In this case, we will need to
    // apply it retroactively to the thumbnail data we already loaded
    // (In new.c, this kludge isn't required - we load/apply in the
    // same pass -- here it is required because we pre-load the thumbnail)
    populate_lut_combo();
    if (curr->meta && curr->meta->general)  {
        if (set_lut_based_on_image_type(curr->meta->general->image_data_type))
        {
            check_lut();
            // data we loaded needs to be lutted
            apply_lut_to_data(thumbnail_data);
        }
    }

    // load the metadata & image data, other setup
    fill_small_have_data(thumbnail_data, curr);
    fill_big(curr);
    update_pixel_info(curr);
    update_zoom();
    set_font();
    fill_meta_info();
    fill_stats(curr);
    setup_bands_tab(curr->meta);
    disable_meta_button_if_necessary();
    if (lut_specified)
      select_lut(lut);

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    // If the last viewed file left behind a (temporary) color map lut, then get rid of it
    if (fileExists(embedded_tiff_lut_file)) {
        remove(embedded_tiff_lut_file);
    }
    if (fileExists(embedded_asf_colormap_file)) {
      remove(embedded_asf_colormap_file);
    }

    image_info_free(curr);
    free_shapes();
    exit (EXIT_SUCCESS);
}

void image_info_free(ImageInfo *ii)
{
    if (ii->data_ci) cached_image_free(ii->data_ci);
    if (ii->meta) meta_free(ii->meta);
    if (ii->filename) free(ii->filename);
    if (ii->data_name) free(ii->meta_name);
    if (ii->meta_name) free(ii->data_name);
}

