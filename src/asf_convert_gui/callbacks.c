#include "asf_convert_gui.h"

void
show_execute_button(gboolean show)
{
    GtkWidget 
      *execute_button,
      *stop_button,
      *load_button,
      *process_button,
      *remove_button,
      *rename_button,
      *google_earth_button,
      *view_output_button;

    execute_button = get_widget_checked("execute_button");
    stop_button = get_widget_checked("stop_button");
    load_button = get_widget_checked("load_button");
    process_button = get_widget_checked("process_button");
    remove_button = get_widget_checked("remove_button");
    rename_button = get_widget_checked("rename_button");
    google_earth_button = get_widget_checked("google_earth_button");
    view_output_button = get_widget_checked("view_output_button");

    gtk_widget_set_sensitive(execute_button, show);
    gtk_widget_set_sensitive(stop_button, !show);
    gtk_widget_set_sensitive(load_button, show);
    gtk_widget_set_sensitive(process_button, show);
    gtk_widget_set_sensitive(remove_button, show);
    gtk_widget_set_sensitive(rename_button, show);
    gtk_widget_set_sensitive(google_earth_button, show);
    gtk_widget_set_sensitive(view_output_button, show);
}

void latitude_checkbutton_toggle()
{
    GtkWidget
        *latitude_checkbutton,
        *latitude_low_label,
        *latitude_low_entry,
        *latitude_hi_label,
        *latitude_hi_entry;

    latitude_checkbutton = get_widget_checked("latitude_checkbutton");
    latitude_low_label = get_widget_checked("latitude_low_label");
    latitude_low_entry = get_widget_checked("latitude_low_entry");
    latitude_hi_label = get_widget_checked("latitude_hi_label");
    latitude_hi_entry = get_widget_checked("latitude_hi_entry");

    gboolean is_checked =
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(latitude_checkbutton));

    gtk_widget_set_sensitive(latitude_low_label, is_checked);
    gtk_widget_set_sensitive(latitude_low_entry, is_checked);
    gtk_widget_set_sensitive(latitude_hi_label, is_checked);
    gtk_widget_set_sensitive(latitude_hi_entry, is_checked);
}

void 
input_data_format_combobox_changed()
{
    GtkWidget *input_data_type_combobox,
        *input_data_type_label,
        *input_data_format_combobox,
        *latitude_checkbutton,
        *latitude_low_label,
        *latitude_low_entry,
        *latitude_hi_label,
        *latitude_hi_entry,
        *process_to_level1_checkbutton,
        *vbox_export,
        *vbox_terrain_correction,
        *vbox_geocode;

    gint input_data_format;
    gboolean show_data_type_combobox;
    gboolean show_latitude_spinbuttons;
    gboolean show_export_section;
    gboolean show_terrain_correction_section;
    gboolean show_geocode_section;
    gboolean show_process_to_level1_checkbutton;

    input_data_format_combobox =
        get_widget_checked("input_data_format_combobox");

    input_data_format =
        gtk_option_menu_get_history(GTK_OPTION_MENU(input_data_format_combobox));
    switch (input_data_format)
    {
        case INPUT_FORMAT_STF:
            show_data_type_combobox = FALSE;
            show_latitude_spinbuttons = TRUE;
            show_process_to_level1_checkbutton = FALSE;
            break;
        case INPUT_FORMAT_COMPLEX:
            show_data_type_combobox = FALSE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            break;
        case INPUT_FORMAT_CEOS_LEVEL0:
            show_data_type_combobox = FALSE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = TRUE;
            break;
        default:
        case INPUT_FORMAT_CEOS_LEVEL1:
        case INPUT_FORMAT_ESRI:
        case INPUT_FORMAT_ENVI:
            show_data_type_combobox = TRUE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            break;
        case INPUT_FORMAT_ASF_INTERNAL:
            show_data_type_combobox = FALSE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            break;
    }

    latitude_checkbutton = get_widget_checked("latitude_checkbutton");
    latitude_low_label = get_widget_checked("latitude_low_label");
    latitude_low_entry = get_widget_checked("latitude_low_entry");
    latitude_hi_label = get_widget_checked("latitude_hi_label");
    latitude_hi_entry = get_widget_checked("latitude_hi_entry");
    process_to_level1_checkbutton =
        get_widget_checked("process_to_level1_checkbutton");

    gtk_widget_set_sensitive(latitude_checkbutton, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_low_label, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_low_entry, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_hi_label, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_hi_entry, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(process_to_level1_checkbutton,
                             show_process_to_level1_checkbutton);

    show_export_section = TRUE;
    show_geocode_section = TRUE;
    show_terrain_correction_section = TRUE;

    if (show_process_to_level1_checkbutton) 
    {
        gboolean process_to_level1_checkbutton_is_checked =
            gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(process_to_level1_checkbutton));

        if (!process_to_level1_checkbutton_is_checked)
        {
            show_export_section = FALSE;
            show_geocode_section = FALSE;
            show_terrain_correction_section = FALSE;
        }
        else
        {
            show_data_type_combobox = TRUE;
        }
    }
    else
    {
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(process_to_level1_checkbutton), FALSE);
    }

    input_data_type_combobox = get_widget_checked("input_data_type_combobox");
    input_data_type_label = get_widget_checked("input_data_type_label");

    gtk_widget_set_sensitive(input_data_type_combobox, show_data_type_combobox);
    gtk_widget_set_sensitive(input_data_type_label, show_data_type_combobox);


    if (!show_latitude_spinbuttons)
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(latitude_checkbutton),
            FALSE);
    }

    latitude_checkbutton_toggle();

    vbox_export = get_widget_checked("vbox_export");
    gtk_widget_set_sensitive(vbox_export, show_export_section);

    if (!show_export_section)
    {
        GtkWidget *export_checkbutton =
            get_widget_checked("export_checkbutton");

        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(export_checkbutton),
            FALSE);
    }

    vbox_terrain_correction = get_widget_checked("vbox_terrain_correction");
    gtk_widget_set_sensitive(vbox_terrain_correction, 
                             show_terrain_correction_section);

    if (!show_terrain_correction_section)
    {
        GtkWidget *dem_checkbutton =
            get_widget_checked("dem_checkbutton");

        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dem_checkbutton),
                                     FALSE);
    }

    output_format_combobox_changed();

    vbox_geocode = get_widget_checked("vbox_geocode");
    gtk_widget_set_sensitive(vbox_geocode, show_geocode_section);

    if (!show_geocode_section)
    {
        GtkWidget *geocode_checkbutton =
            get_widget_checked("geocode_checkbutton");

        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(geocode_checkbutton),
            FALSE);
    }

    geocode_options_changed();
}

void
rgb_checkbutton_toggle()
{
    GtkWidget *rgb_checkbutton, *rgb_hbox;
    gboolean is_checked;

    rgb_checkbutton = get_widget_checked("rgb_checkbutton");
    rgb_hbox = get_widget_checked("rgb_hbox");

    is_checked =
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rgb_checkbutton));

    gtk_widget_set_sensitive(rgb_hbox, is_checked);
}

void
output_bytes_checkbutton_toggle()
{
    GtkWidget *output_bytes_checkbutton,
        *scaling_method_combobox,
        *scaling_method_label;

    gboolean is_checked;

    output_bytes_checkbutton =
        get_widget_checked("output_bytes_checkbutton");

    scaling_method_combobox =
        get_widget_checked("scaling_method_combobox");

    scaling_method_label =
        get_widget_checked("scaling_method_label");

    is_checked =
        gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(output_bytes_checkbutton));

    gtk_widget_set_sensitive(scaling_method_combobox, is_checked);
    gtk_widget_set_sensitive(scaling_method_label, is_checked);
}

void export_checkbutton_toggle();

void
output_format_combobox_changed()
{
    export_checkbutton_toggle();
}

void
export_checkbutton_toggle()
{
    GtkWidget *export_checkbutton,
        *output_format_combobox,
        *output_bytes_checkbutton,
        *scaling_method_combobox,
        *scaling_method_label,
        *rgb_checkbutton,
        *rgb_hbox;
    
    gint output_format;
    gboolean export_checked;

    export_checkbutton =
        get_widget_checked("export_checkbutton");

    export_checked =
        gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(export_checkbutton));

    output_format_combobox = 
        get_widget_checked("output_format_combobox");

    output_bytes_checkbutton =
        get_widget_checked("output_bytes_checkbutton");

    scaling_method_combobox =
        get_widget_checked("scaling_method_combobox");

    scaling_method_label =
        get_widget_checked("scaling_method_label");

    rgb_checkbutton =
        get_widget_checked("rgb_checkbutton");

    rgb_hbox =
        get_widget_checked("rgb_hbox");

    if (export_checked)
    {
        gtk_widget_set_sensitive(output_format_combobox, TRUE);

        output_format = get_combo_box_item(output_format_combobox);

        switch (output_format)
        {
        default:
        case OUTPUT_FORMAT_JPEG:
        case OUTPUT_FORMAT_PGM:
        case OUTPUT_FORMAT_TIFF:
        case OUTPUT_FORMAT_ASF_INTERNAL:
        case OUTPUT_FORMAT_CEOS:          
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(output_bytes_checkbutton), TRUE);

            set_combo_box_item(scaling_method_combobox, SCALING_METHOD_SIGMA);

            gtk_widget_set_sensitive(output_bytes_checkbutton, FALSE);
            gtk_widget_set_sensitive(scaling_method_combobox, TRUE);
            gtk_widget_set_sensitive(scaling_method_label, TRUE);

            break;
        case OUTPUT_FORMAT_GEOTIFF:
            gtk_widget_set_sensitive(output_bytes_checkbutton, TRUE);
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(output_bytes_checkbutton), FALSE);

            output_bytes_checkbutton_toggle();
            break;
        }

        gtk_widget_set_sensitive(rgb_checkbutton, TRUE);
        rgb_checkbutton_toggle();
    }
    else
    {
        gtk_widget_set_sensitive(output_format_combobox, FALSE);
        gtk_widget_set_sensitive(output_bytes_checkbutton, FALSE);
        gtk_widget_set_sensitive(scaling_method_combobox, FALSE);
        gtk_widget_set_sensitive(scaling_method_label, FALSE);
        gtk_widget_set_sensitive(rgb_checkbutton, FALSE);
        gtk_widget_set_sensitive(rgb_hbox, FALSE);
    }

    update_all_extensions();
}

SIGNAL_CALLBACK void
on_asf_convert_destroy(GtkWidget *widget, gpointer data)
{
    gtk_main_quit();
}

#ifndef USE_GTK_22
SIGNAL_CALLBACK void
on_input_data_format_combobox_changed(GtkWidget *widget)
{
    input_data_format_combobox_changed();
    update_summary();
}
#else
SIGNAL_CALLBACK void
on_ceos_level_0_activate(GtkWidget *widget)
{
    input_data_format_combobox_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_ceos_level_1_activate(GtkWidget *widget)
{
    input_data_format_combobox_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_stf_activate(GtkWidget *widget)
{
    input_data_format_combobox_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_complex_activate(GtkWidget *widget)
{
    input_data_format_combobox_changed();
    update_summary();
}
#endif

SIGNAL_CALLBACK void
on_process_to_level1_checkbutton_toggled(GtkWidget *widget)
{
    input_data_format_combobox_changed();
    update_summary();
}

#ifndef USE_GTK_22
SIGNAL_CALLBACK void
on_output_format_combobox_changed(GtkWidget *widget)
{
    output_format_combobox_changed();
    update_summary();
}
#else

SIGNAL_CALLBACK void
on_jpeg_activate(GtkWidget *widget)
{
    output_format_combobox_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_tiff_activate(GtkWidget *widget)
{
    output_format_combobox_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_geotiff_activate(GtkWidget *widget)
{
    output_format_combobox_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_pgm_activate(GtkWidget *widget)
{
    output_format_combobox_changed();
    update_summary();
}

#endif

SIGNAL_CALLBACK void
on_rgb_checkbutton_toggled(GtkWidget *widget)
{
    rgb_checkbutton_toggle();
    update_summary();
}

SIGNAL_CALLBACK void
on_latitude_checkbutton_toggled(GtkWidget *widget)
{
    latitude_checkbutton_toggle();
    update_summary();
}

SIGNAL_CALLBACK void
on_output_bytes_checkbutton_toggled(GtkWidget *widget)
{
    output_bytes_checkbutton_toggle();
    update_summary();
}

SIGNAL_CALLBACK void
on_export_checkbutton_toggled(GtkWidget *widget)
{
    export_checkbutton_toggle();
    update_summary();
}

void
input_data_type_changed()
{
    GtkWidget *input_data_type_combobox =
        get_widget_checked("input_data_type_combobox");

    int input_data_type =
        gtk_option_menu_get_history(GTK_OPTION_MENU(input_data_type_combobox));

    gboolean checkbutton_db_active =
        input_data_type == INPUT_TYPE_SIGMA ||
        input_data_type == INPUT_TYPE_BETA ||
        input_data_type == INPUT_TYPE_GAMMA;

    GtkWidget *checkbutton_db =
        get_widget_checked("checkbutton_db");

    if (!checkbutton_db_active)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton_db), FALSE);

    gtk_widget_set_sensitive(checkbutton_db, checkbutton_db_active);
    update_summary();
}

SIGNAL_CALLBACK void
on_input_data_type_sigma_activate(GtkWidget *widget)
{
    input_data_type_changed();
}

SIGNAL_CALLBACK void
on_input_data_type_beta_activate(GtkWidget *widget)
{
    input_data_type_changed();
}

SIGNAL_CALLBACK void
on_input_data_type_amplitude_activate(GtkWidget *widget)
{
    input_data_type_changed();
}

SIGNAL_CALLBACK void
on_input_data_type_gamma_activate(GtkWidget *widget)
{
    input_data_type_changed();
}

SIGNAL_CALLBACK void
on_input_data_type_power_activate(GtkWidget *widget)
{
    input_data_type_changed();
}

SIGNAL_CALLBACK void
on_clear_button_clicked(GtkWidget *widget)
{
    gtk_list_store_clear(completed_list_store);
}
