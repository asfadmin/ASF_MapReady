/* This program is a simple GUI wrapper around the asf_convert
tool.  */

#include "asf_convert_gui.h"
#include "asf_version.h"

GladeXML *glade_xml;
GtkListStore *list_store = NULL;
gboolean processing;
Settings *settings_on_execute;
gchar * output_directory = NULL;
NamingScheme * current_naming_scheme = NULL;
gboolean use_thumbnails = FALSE;

int
main(int argc, char **argv)
{
    GtkWidget *widget;
    gchar *glade_xml_file;

    gtk_init(&argc, &argv);
    set_font();

    glade_xml_file = (gchar *)find_in_share("asf_convert_gui.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    /* thumbnails supported in GTK 2.4 or greater */
#ifdef G_THREADS_ENABLED
    use_thumbnails = gtk_major_version >= 2 && gtk_minor_version >= 4;
#else
    use_thumbnails = FALSE;
#endif

#ifdef win32
        // On windows, ensure that our installed sh.exe is the one that is found,
        // by severely restricting the path.
        char pathenv[1024];
        sprintf(pathenv, "PATH=%s", get_asf_bin_dir());
        putenv(pathenv);
#endif

    if (!use_thumbnails)
    {
        printf("GTK Version < 2.4 -- output thumbnails disabled.\n");
    }
    else
    {
        // We will want to load thumbnails in other threads.
        if ( !g_thread_supported () ) {
            g_thread_init (NULL);
        }
    }

    /* allow FOPEN, FREAD, FWRITE to fail without aborting */
    caplib_behavior_on_error = BEHAVIOR_ON_ERROR_CONTINUE;

    /* add version number to window title */
    char title [256];
    sprintf (title,
        "Alaska Satellite Facility ALOS Data Conversion Tool: Version %s",
        CONVERT_PACKAGE_VERSION_STRING);

    widget = glade_xml_get_widget (glade_xml, "asf_convert");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    /* select defaults for dropdowns */
    widget = glade_xml_get_widget (glade_xml, "scaling_method_combobox");
    set_combo_box_item(widget, SCALING_METHOD_SIGMA);

    widget = glade_xml_get_widget (glade_xml, "input_data_format_combobox");
    set_combo_box_item(widget, INPUT_FORMAT_CEOS_LEVEL1);

    widget = glade_xml_get_widget (glade_xml, "input_data_type_combobox");
    set_combo_box_item(widget, INPUT_TYPE_AMP);

    widget = glade_xml_get_widget (glade_xml, "resample_option_menu");
    set_combo_box_item(widget, RESAMPLE_BILINEAR);

    widget = glade_xml_get_widget (glade_xml, "output_format_combobox");
    set_combo_box_item(widget, OUTPUT_FORMAT_JPEG);

    /* fire handlers for hiding/showing stuff */
    output_format_combobox_changed();
    input_data_format_combobox_changed();
    input_data_type_changed();
    set_toolbar_images();
    show_execute_button(TRUE);

    /* build columns in the files section */
    setup_files_list(argc, argv);

    /* allow multiple selects */
    widget = glade_xml_get_widget(glade_xml, "input_file_selection");
    gtk_file_selection_set_select_multiple(GTK_FILE_SELECTION(widget), TRUE);

    /* drag-n-drop setup */
    setup_dnd();

    /* right-click menu setup */
    setup_popup_menu();

    /* bands dropdown setup*/
    setup_band_comboboxes();

    current_naming_scheme = naming_scheme_default();

    /* set initial vpanel setting */
    widget = glade_xml_get_widget(glade_xml, "vertical_pane");
    gtk_paned_set_position(GTK_PANED(widget), 240);

    /* Connect signal handlers.  */
    glade_xml_signal_autoconnect (glade_xml);

    /* initial flag settings */
    processing = FALSE;
    settings_on_execute = NULL;

    /* explicit call to the function that refreshes the "summary" */
    /* section when options are changed, so get the settings      */
    /* initially in there                                         */
    default_to_terrcorr_on();
    terrcorr_options_changed();

    gtk_main ();

    if (settings_on_execute)
        settings_delete(settings_on_execute);

    if (output_directory)
        g_free(output_directory);

    if (current_naming_scheme)
        naming_scheme_delete(current_naming_scheme);

    release_predefined_projections();

    exit (EXIT_SUCCESS);
}

