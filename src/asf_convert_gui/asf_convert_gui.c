/* This program is a simple GUI wrapper around the asf_mapready
   tool.  */

#include "asf_convert_gui.h"
#include "asf_version.h"
#include "asf_geocode.h"

// FIXME: This is from license.c ...need to either move it into a header or write
// a function that just returns the string (without a trailing \n)
#define ASF_COPYRIGHT_STRING \
 "Copyright (c) 2008, University of Alaska Fairbanks, Alaska Satellite Facility.\n"\
 "All rights reserved."

GladeXML *glade_xml;
GtkListStore *list_store = NULL;
GtkListStore *completed_list_store = NULL;
gboolean processing;
gboolean show_full_paths;
gboolean show_ancillary_files;
gboolean animate_ancillary_files_button;
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

    asfPrintStatus("\nASF MapReady:\n");
    const char *share_dir = get_asf_share_dir();

    if (!share_dir)
      // this actually should never happen with the current implementation
      // of get_asf_share_dir() -- always sets the share dir to something
      // even if it is a bad guess... in which case the next check will fail
      asfPrintError("Could not find the ASF share directory!\n");

    glade_xml_file = (gchar *)find_in_share("mapready.glade");
    if (!glade_xml_file)
      asfPrintError("Could not find the mapready.glade file!\n"
                    "It should be in the share files directory, here:\n"
                    "  %s\n", share_dir);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    if (!glade_xml)
      asfPrintError("Could not load the mapready.glade file!\n"
                    "This file may be corrupt. mapready.glade was found in:\n"
                    "  %s\n", share_dir);
    g_free(glade_xml_file);

    asfPrintStatus("Using share files directory: %s\n\n", share_dir);

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


    /* add version number to window title, request a default size */
    char gtitle [256];
    sprintf (gtitle, "ASF MapReady: Version %s",
             MAPREADY_VERSION_STRING);

    widget = get_widget_checked("asf_convert");
    gtk_window_set_title(GTK_WINDOW(widget), gtitle);
    // commenting this out - now supported within glade
    //gtk_window_resize(GTK_WINDOW(widget), 1000, 700);

    /* select defaults for dropdowns & buttons & labeling */
    widget = get_widget_checked("scaling_method_combobox");
    set_combo_box_item(widget, SCALING_METHOD_SIGMA);

    widget = get_widget_checked("import_checkbutton");
    gtk_widget_set_sensitive(widget, FALSE);

    widget = get_widget_checked("input_data_type_combobox");
    set_combo_box_item(widget, INPUT_TYPE_AMP);

    widget = get_widget_checked("resample_option_menu");
    set_combo_box_item(widget, RESAMPLE_BILINEAR);

    widget = get_widget_checked("output_format_combobox");
    set_combo_box_item(widget, OUTPUT_FORMAT_JPEG);

    widget = get_widget_checked("geocode_checkbutton");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), TRUE);
    geocode_options_changed();
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), FALSE);

    widget = get_widget_checked("about_dialog_copyright_label");
    gtk_label_set_text(GTK_LABEL(widget), ASF_COPYRIGHT_STRING);

    // Hide latitude selection stuff until we start supporting
    // swath products (level 0) again
    widget = get_widget_checked("latitude_checkbutton");
    gtk_widget_hide(widget);
    widget = get_widget_checked("latitude_low_label");
    gtk_widget_hide(widget);
    widget = get_widget_checked("latitude_hi_label");
    gtk_widget_hide(widget);
    widget = get_widget_checked("latitude_low_entry");
    gtk_widget_hide(widget);
    widget = get_widget_checked("latitude_hi_entry");
    gtk_widget_hide(widget);

    // This option is deprecated -- we always apply the fix now
    // and don't give the user the option of turning it off.  Probably
    // we can just delete all code associated with it, but for now we
    // just turn it on, and hide it.
    set_checked("apply_metadata_fix_checkbutton", TRUE);
    widget = get_widget_checked("apply_metadata_fix_checkbutton");
    gtk_widget_hide(widget);

    // Muck with the fonts in the About dialog
    widget = get_widget_checked("about_dialog_mapready_label");
    gchar *str = gtitle;
    gchar *text;
    PangoAttrList *attrs;
    sprintf(gtitle,
                "\n<b>ASF MapReady</b>\n"
                "<i>Remote Sensing Toolkit</i>\n"
                "ver. %s",
                MAPREADY_VERSION_STRING);
    if (strlen(SVN_REV)>0)
        sprintf(gtitle, "%s (build %s)", gtitle, SVN_REV);
    else
        strcat(gtitle, " (custom build)");

    pango_parse_markup(str, -1, 0, &attrs, &text, NULL, NULL);
    gtk_label_set_attributes(GTK_LABEL(widget), attrs);
    gtk_label_set_text(GTK_LABEL(widget), text);
    PangoFontDescription *font_desc =
      pango_font_description_from_string("Sans 12");
    gtk_widget_modify_font(widget, font_desc);

    // Muck with the "Select Processing Steps" label
    widget = get_widget_checked("select_processing_steps_label");
    str = gtitle;
    sprintf(gtitle, "<b><i>  Select Processing Steps:</i></b>");
    pango_parse_markup(str, -1, 0, &attrs, &text, NULL, NULL);
    gtk_label_set_attributes(GTK_LABEL(widget), attrs);
    gtk_label_set_text(GTK_LABEL(widget), text);
    font_desc = pango_font_description_from_string("Sans 12");
    gtk_widget_modify_font(widget, font_desc);

    /* fire handlers for hiding/showing stuff */
    output_format_combobox_changed();
    input_data_type_changed();
    geocode_options_changed();
    load_external_commands();
    external_settings_changed();
    set_toolbar_images();
    show_execute_button(TRUE);

    /* build columns in the files section */
    show_full_paths = FALSE; // Set before setup_files_list(), default to FALSE
    show_ancillary_files = FALSE; // Set before setup_files_list(), default to FALSE
    animate_ancillary_files_button = TRUE; // ...Until first click of Add Ancillary Files button
    widget = get_widget_checked("show_full_path_names_checkbutton");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), FALSE);
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
    //widget = get_widget_checked("vertical_pane");
    //gtk_paned_set_position(GTK_PANED(widget), 240);

    /* Connect signal handlers.  */
    glade_xml_signal_autoconnect (glade_xml);

    /* initial flag settings */
    processing = FALSE;
    settings_on_execute = NULL;

    /* explicit call to the function that refreshes the "summary" */
    /* section when options are changed, so get the settings      */
    /* initially in there                                         */
    input_data_formats_changed();
    input_data_type_combobox_changed();
    default_to_terrcorr_on();
    default_to_keep_temp();
    terrcorr_options_changed();

    /* For some reason, it did not work to set this via glade        */
    /* So, we have to select our default faraday rotation style here */
    rb_select("rb_fr_global", TRUE);
    polarimetry_settings_changed();

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

