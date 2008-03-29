#include "c2v.h"
#include "asf_version.h"

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
    if (fileExists(ret)) {
        return ret;
    } else {
        printf("Trying to find %s file: %s\n", filename, ret);
        free(ret);
        return NULL;
    }
}

SIGNAL_CALLBACK void
on_c2v_window_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

static void select_defaults()
{
    set_combo_box_item("input_format_combobox", INPUT_ALOS_CSV);
    set_combo_box_item("output_format_combobox", OUTPUT_KML);
}

static void select_defaults_by_file_type(const char *f)
{
    char *ext = findExt(f);
  
    if (strcmp_case(ext, ".csv") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_ALOS_CSV);
      set_combo_box_item("output_format_combobox", OUTPUT_KML);      
    }
    else if (strcmp_case(ext, ".meta") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_META);
      set_combo_box_item("output_format_combobox", OUTPUT_KML);      
    }
    else if (strcmp_case(ext, ".ldr") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_LDR);
      set_combo_box_item("output_format_combobox", OUTPUT_KML);      
    }
    else if (strcmp_case(ext, ".shp") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_SHAPE);
      set_combo_box_item("output_format_combobox", OUTPUT_KML);      
    }
    else if (strcmp_case(ext, ".kml") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_KML);
      set_combo_box_item("output_format_combobox", OUTPUT_ALOS_CSV);      
    }
    else if (strcmp_case(ext, ".tif") == 0) {
      set_combo_box_item("input_format_combobox", INPUT_GEOTIFF);
      set_combo_box_item("output_format_combobox", OUTPUT_KML);      
    }
}

int
main(int argc, char **argv)
{
    gtk_init(&argc, &argv);

    char *glade_xml_file = find_in_share("c2v.glade");
    if (!glade_xml_file) {
        printf("Couldn't find the glade file: c2v.glade.  "
               "Aborting...\n");
        exit(1);
    }

    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    set_font();
    select_defaults();

    if (argc>1) {
      add_input_file(argv[1]);
      select_defaults_by_file_type(argv[1]);
      int output_format = get_combo_box_item("output_format_combobox");
      process();
      // if we opened up an external application, we can just exit now
      // otherwise we will stay open
      if (output_format == OUTPUT_KML || output_format == OUTPUT_ALOS_CSV)
        exit(EXIT_SUCCESS);
    }

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();

    exit (EXIT_SUCCESS);
}

