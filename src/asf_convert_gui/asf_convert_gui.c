/* This program is a simple GUI wrapper around the asf_convert
   tool.  */

#include "asf_convert_gui.h"
#include "asf_version.h"

GladeXML *glade_xml;
GtkListStore *list_store = NULL;
GtkListStore *completed_list_store = NULL;
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
    get_asf_share_dir_with_argv0(argv[0]);

    glade_xml_file = (gchar *)find_in_share("asf_convert_gui.glade");

    asfPrintStatus("\n\nASF Convert:\n");
    asfPrintStatus("Using share files directory: %s\n\n", get_asf_share_dir());
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
        "Alaska Satellite Facility Data Conversion Tool: Version %s",
        CONVERT_PACKAGE_VERSION_STRING);

    widget = get_widget_checked("asf_convert");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    /* select defaults for dropdowns */
    widget = get_widget_checked("scaling_method_combobox");
    set_combo_box_item(widget, SCALING_METHOD_SIGMA);

    widget = get_widget_checked("input_data_format_combobox");
    set_combo_box_item(widget, INPUT_FORMAT_CEOS_LEVEL1);

    widget = get_widget_checked("input_data_type_combobox");
    set_combo_box_item(widget, INPUT_TYPE_AMP);

    widget = get_widget_checked("resample_option_menu");
    set_combo_box_item(widget, RESAMPLE_BILINEAR);

    widget = get_widget_checked("output_format_combobox");
    set_combo_box_item(widget, OUTPUT_FORMAT_JPEG);

    /* fire handlers for hiding/showing stuff */
    output_format_combobox_changed();
    input_data_format_combobox_changed();
    input_data_type_changed();
    set_toolbar_images();
    show_execute_button(TRUE);

    /* build columns in the files section */
    setup_files_list();

    /* allow multiple selects */
    widget = get_widget_checked("input_file_selection");
    gtk_file_selection_set_select_multiple(GTK_FILE_SELECTION(widget), TRUE);

    /* drag-n-drop setup */
    setup_dnd();

    /* right-click menu setup */
    setup_popup_menu();

    /* bands dropdown setup*/
    setup_band_comboboxes();

    current_naming_scheme = naming_scheme_default();

    /* set initial vpanel setting */
    widget = get_widget_checked("vertical_pane");
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

    /* put files on the command-line into the files section */
    populate_files_list(argc, argv);

    /* set up the rgb stuff on the export tab */
    rgb_combo_box_setup();
    
    /* enters the main GTK loop */
    gtk_main ();

    /* clean up, application has been closed */
    if (settings_on_execute)
        settings_delete(settings_on_execute);

    if (output_directory)
        g_free(output_directory);

    if (current_naming_scheme)
        naming_scheme_delete(current_naming_scheme);

    release_predefined_projections();

    exit (EXIT_SUCCESS);
}

