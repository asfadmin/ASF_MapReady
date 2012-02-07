#include "asf_view.h"
#include <asf_contact.h>

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

ImageInfo image_info[MAX_IMAGES];
ImageInfo *curr=NULL;
ImageInfo mask_info;
ImageInfo *mask=NULL;
int n_images_loaded=1;
int current_image_info_index=0;

// various values
double zoom;
double center_samp, center_line;
double crosshair_line, crosshair_samp;
int g_saved_line_count;

// these are used when the command line option "-generic" is given
// (trying to read in a generic binary file)
int generic_specified;
int generic_bin_width, generic_bin_height, generic_bin_datatype;
int generic_bin_byteswap=0;

/************************************************************************
 * End of globals
 */

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

static void help()
{
  asfPrintStatus(
    "Usage:\n"
    "   asf_view [-band <band_name>] [-colormap <colormap_name>] <filename>\n"
    "\n"
    "Description:\n"
    "   Viewer for most image types that ASF software can ingest or export:\n"
    "   CEOS (ERS-1, ERS-2, RADARSAT-1, ALOS are officially supported,\n"
    "   many others will work), AirSAR, TerraSAR-X, ASF Internal, GeoTIFF,\n"
    "   JPEG, TIFF, PNG, PGM.  ENVI .img/.hdr files may also work.\n"
    "\n"
    "Options:\n"
    "   -band <band_name> (-b)\n"
    "       Load the specified band.  Generally this is only used for multiband\n"
    "       ASF Internal format files.  If not specified, the first band is\n"
    "       shown.  You may select other bands within ASF View on the 'Display'\n"
    "       tab.\n"
    "\n"
    "   -colormap <colormap_name>\n"
    "       Apply a colormap to the data when displaying it.  Colormaps convert\n"
    "       greyscale pixel values to RGB values.  If the specified colormap\n"
    "       is not found in the current directory, the ASF share directory is\n"
    "       checked.\n"
    "\n");
  asfPrintStatus("Contact:\n" ASF_CONTACT_STRING "\n");
  asfPrintStatus("Version:\n   " SVN_REV " (part of " TOOL_SUITE_NAME " " MAPREADY_VERSION_STRING ")\n\n");
  exit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
    if (detect_flag_options(argc, argv, "-help", "--help", NULL))
      help();

    char band[512], lut[512], mask_file_name[512];

    strcpy(band, "");
    strcpy(mask_file_name, "");

    int band_specified = extract_string_options(&argc, &argv, band,
        "-band", "--band", "-b", NULL);
    int lut_specified = extract_string_options(&argc, &argv, lut,
        "-colormap", "--colormap", "-lut", "--lut", NULL);
    int planner_mode = extract_flag_options(&argc, &argv,
        "-plan", "--plan", NULL);
    int mask_specified = extract_string_options(&argc, &argv, mask_file_name,
        "-mask", "--mask", "--layover-mask", "--layover-mask", NULL);
    generic_specified = extract_flag_options(&argc, &argv,
        "-generic", "--generic", NULL);
    if (generic_specified) {
       char type[512];
       if (!extract_int_options(&argc, &argv, &generic_bin_width,
                "-width", "--width", "-cols", "--cols", NULL) ||
           !extract_int_options(&argc, &argv, &generic_bin_height,
                "-height", "--height", "-rows", "--rows", NULL)) {
         asfPrintError("When reading generic data, specify the size "
            "(--width, --height).\n");
       }
       generic_bin_byteswap =
         extract_flag_options(&argc, &argv,
                              "--byteswap", "-byteswap", NULL);
       if (extract_string_options(&argc, &argv, type,
                "-type", "--type", NULL))
       {
         if (strcmp_case(type, "BYTE") == 0 ||
             strcmp_case(type, "INT8") == 0) {
           generic_bin_datatype = BYTE;
         }
         else if (strcmp_case(type, "FLOAT") == 0 ||
                  strcmp_case(type, "REAL32") == 0) {
           generic_bin_datatype = REAL32;
         }
         else {
           asfPrintError("Unknown generic data type: %s\n", type);
         }
       } else {
         asfPrintStatus("Generic binary: assuming REAL32 data.\n");
         generic_bin_datatype = REAL32;
       }
    }

    if (planner_mode) {
      if (detect_flag_options(argc, argv, "-calibrate-reference", NULL)) {
        calibrate_planner_reference();
        exit(EXIT_SUCCESS);
      }
    }

    handle_common_asf_args(&argc, &argv, "ASF View");

    // point to "polygon 0" as the one we initially work on
    g_poly = &g_polys[0];

    // set up image array
    curr = &image_info[0];
    curr->data_name = curr->meta_name = NULL;
    int ii;

    if (argc < 2) {
        curr->filename = STRDUP(find_in_share("startup.jpg"));
    }
    else {
        n_images_loaded = 0;
	for (ii=1; ii<argc; ++ii) {
           if (strlen(argv[ii]) > 0) {
               image_info[n_images_loaded].filename = STRDUP(argv[ii]);
               ++n_images_loaded;
           }
        }    
    }


    if (n_images_loaded == 1) {
        asfPrintStatus("Loading 1 image: %s\n", image_info[0].filename);
    }
    else {
        asfPrintStatus("Loading %d images:\n", n_images_loaded);
        for (ii=0; ii<n_images_loaded; ++ii)
            asfPrintStatus("%d: %s\n", ii+1, image_info[ii].filename);
    }

    if (mask_specified)
        asfPrintStatus("Mask: %s\n", mask_file_name);

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
    sprintf(embedded_asf_colormap_file,"%s%c%s", lut_loc, DIR_SEPARATOR,
            EMBEDDED_ASF_COLORMAP_LUT_FILE);
    FREE(lut_loc);
    if (fileExists(embedded_tiff_lut_file)) remove(embedded_tiff_lut_file);
    if (fileExists(embedded_asf_colormap_file)) remove(embedded_asf_colormap_file);

    if (mask_specified) {
        curr = mask = &mask_info;
        mask->filename = STRDUP(mask_file_name);

        if (mask->filename[strlen(mask->filename)-1] == '.')
            mask->filename[strlen(mask->filename)-1] = '\0';
        
        read_file(mask->filename, NULL, FALSE, TRUE);
        //set_lut("layover_mask");
    }
    
    // load the image we're going to actually show last
    for (ii=n_images_loaded-1; ii>=0; --ii)
    {
        curr = &image_info[ii];
        
        // strip off any trailing "."
        if (curr->filename[strlen(curr->filename)-1] == '.')
            curr->filename[strlen(curr->filename)-1] = '\0';
        
        read_file(curr->filename, band_specified ? band : NULL, FALSE, TRUE);
        check_for_embedded_tiff_lut(curr->filename, &lut_specified, lut);
        if (lut_specified)
            set_lut(lut);
        
        assert(curr->data_name);
        assert(curr->meta_name);
        
        // we load the thumbnail data before bringing up the window, looks
        // much nicer.  When loading an image within the GUI, we don't need
        // to do get_thumbnail_data() as a separate step.
        ThumbnailData *thumbnail_data = get_thumbnail_data(curr);
        
        // first time through the loop only, set up GTK
        if (ii == n_images_loaded-1) {
            gtk_init(&argc, &argv);
            
            gchar *glade_xml_file = (gchar *)find_in_share("asf_view.glade");
            printf("Found asf_view.glade: %s\n", glade_xml_file);
            glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
            free(glade_xml_file);
            
            // set up window title, etc
            set_button_images();
            
            // set up the acquisition planner, if we are in that mode
            if (planner_mode) {
                setup_planner();
                
                // getting rid of the info section makes more room for the found
                // acquisitions, and isn't really necessary in the planner
                show_widget("info_hbox", FALSE);
            }
            
            // populate the look up table list, and apply the default
            // look-up-table, if there is one.  In this case, we will need to
            // apply it retroactively to the thumbnail data we already loaded
            // (In new.c, this kludge isn't required - we load/apply in the
            // same pass -- here it is required because we pre-load the thumbnail)
            populate_lut_combo();
            if (check_for_embedded_tiff_lut(curr->filename, &lut_specified, lut)) {
                GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
                gtk_option_menu_set_history(GTK_OPTION_MENU(option_menu), get_tiff_lut_index());
                set_current_index(get_tiff_lut_index());
            }
            else if (is_colormap_ASF_file(curr->filename)) {
                /*
                * lut_specified = 1;
                * strcpy(lut, EMBEDDED_ASF_COLORMAP_LUT);
                * GtkWidget *option_menu = get_widget_checked("lut_optionmenu");
                * gtk_option_menu_set_history(GTK_OPTION_MENU(option_menu), get_asf_lut_index());
                * set_current_index(get_asf_lut_index());
                * check_lut();
                * apply_lut_to_data(thumbnail_data);
                */
            }
        }
        else if (ii == 0) {
            set_title(band_specified, band);
        }

        if (curr->meta && curr->meta->general)  {
            if (set_lut_based_on_image_type(curr->meta->general->image_data_type))
            {
                check_lut();
                // data we loaded needs to be lutted
                apply_lut_to_data(thumbnail_data);
            }
        }
        
        // load the metadata & image data, other setup
        setup_gdk_window_ids();
        setup_small_image_size();
        fill_small_have_data(thumbnail_data, curr);
        fill_big(curr);
        update_pixel_info(curr);
        update_zoom();
        set_font();
        fill_meta_info();
        update_map_settings(curr);
        fill_stats(curr);
        set_mapping_defaults(curr);
        setup_bands_tab(curr->meta);
        disable_meta_button_if_necessary();
        if (lut_specified)
            select_lut(lut);
    }

    if (n_images_loaded>0) {
        asfPrintStatus("Currently displaying %d: %s\n",
                       current_image_info_index, curr->filename);
    }

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    // If the last viewed file left behind a (temporary) color map lut,
    // then get rid of it
    if (fileExists(embedded_tiff_lut_file))
      remove(embedded_tiff_lut_file);
    if (fileExists(embedded_asf_colormap_file))
      remove(embedded_asf_colormap_file);

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
