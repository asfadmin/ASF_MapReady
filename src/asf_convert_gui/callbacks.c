#include "asf_convert_gui.h"

void
show_execute_button(gboolean show)
{
    GtkWidget *execute_button = get_widget_checked("execute_button");
    GtkWidget *stop_button = get_widget_checked("stop_button");
    GtkWidget *load_button = get_widget_checked("load_button");
    GtkWidget *process_button = get_widget_checked("process_button");
    GtkWidget *remove_button = get_widget_checked("remove_button");
    GtkWidget *rename_button = get_widget_checked("rename_button");
    GtkWidget *google_earth_button = get_widget_checked("google_earth_button");

    gtk_widget_set_sensitive(execute_button, show);
    gtk_widget_set_sensitive(stop_button, !show);
    gtk_widget_set_sensitive(load_button, show);
    gtk_widget_set_sensitive(process_button, show);
    gtk_widget_set_sensitive(remove_button, show);
    gtk_widget_set_sensitive(rename_button, show);
    gtk_widget_set_sensitive(google_earth_button, show);
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
        *rb_all,
        *rb_rgb,
        *rgb_vbox;
    
    gint output_format;

    export_checkbutton = get_widget_checked("export_checkbutton");

    gboolean export_checked =
        gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(export_checkbutton));

    output_format_combobox = get_widget_checked("output_format_combobox");
    output_bytes_checkbutton = get_widget_checked("output_bytes_checkbutton");
    scaling_method_combobox = get_widget_checked("scaling_method_combobox");
    scaling_method_label = get_widget_checked("scaling_method_label");
    rb_rgb = get_widget_checked("rb_rgb");
    rb_all = get_widget_checked("rb_all");
    rgb_vbox = get_widget_checked("rgb_vbox");

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

        gtk_widget_set_sensitive(rb_all, export_checked);
        gtk_widget_set_sensitive(rb_rgb, export_checked);

        // turn off the RGB selection for greyscale only output formats
        int output_format_supports_color = output_format != OUTPUT_FORMAT_PGM;

        if (!output_format_supports_color) {
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rb_all), TRUE);
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rb_rgb), FALSE);
        }

        gtk_widget_set_sensitive(rb_all, output_format_supports_color);
        gtk_widget_set_sensitive(rb_rgb, output_format_supports_color);

        rgb_settings_changed();
    }
    else
    {
        gtk_widget_set_sensitive(output_format_combobox, FALSE);
        gtk_widget_set_sensitive(output_bytes_checkbutton, FALSE);
        gtk_widget_set_sensitive(scaling_method_combobox, FALSE);
        gtk_widget_set_sensitive(scaling_method_label, FALSE);
        gtk_widget_set_sensitive(rgb_vbox, FALSE);
    }

    update_all_extensions();
}

SIGNAL_CALLBACK void
on_asf_convert_destroy(GtkWidget *widget, gpointer data)
{
    set_stop();
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
    GtkWidget *rb_all = get_widget_checked("rb_all");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rb_all), TRUE);

    output_format_combobox_changed();
    update_summary();
}

#endif

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

void rgb_combo_box_setup()
{
    rb_select("rb_truecolor", TRUE);
    rb_select("rb_falsecolor", FALSE);
    rb_select("rb_user_defined", FALSE);

    rb_select("rb_radar", TRUE);
    rb_select("rb_optical", FALSE);

    rb_select("rb_all", TRUE);
    rb_select("rb_rgb", FALSE);

    rgb_settings_changed();
}

void rgb_settings_changed()
{
    GtkWidget *rb_rgb = get_widget_checked("rb_rgb");
    GtkWidget *rgb_vbox = get_widget_checked("rgb_vbox");

    gboolean is_rgb = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rb_rgb));
    gtk_widget_set_sensitive(rgb_vbox, is_rgb);

    if (is_rgb)
    {
        // "Export Multiple Bands in a single RGB Image"
        GtkWidget *rb_radar = get_widget_checked("rb_radar");
        GtkWidget *radar_vbox = get_widget_checked("radar_vbox");
        GtkWidget *optical_vbox = get_widget_checked("optical_vbox");
        
        gboolean is_radar = gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(rb_radar));

        gtk_widget_set_sensitive(radar_vbox, is_radar);
        gtk_widget_set_sensitive(optical_vbox, !is_radar);

        if (is_radar)
        {
            GtkWidget *rb_user_defined =
                get_widget_checked("rb_user_defined_radar");
            GtkWidget *rb_pauli = get_widget_checked("rb_pauli");
            GtkWidget *rb_sinclair = get_widget_checked("rb_sinclair");

            int is_user_defined = gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_user_defined));
            int is_pauli = gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_pauli));
            int is_sinclair = gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_sinclair));
            g_assert(is_user_defined + is_pauli + is_sinclair == 1);

            GtkWidget *radar_bands_hbox =
                get_widget_checked("radar_bands_hbox");

            gtk_widget_set_sensitive(radar_bands_hbox, is_user_defined);

            set_combo_box_item_checked("red_optical_combo", 0);
            set_combo_box_item_checked("green_optical_combo", 0);
            set_combo_box_item_checked("blue_optical_combo", 0);
        }
        else
        {
            GtkWidget *rb_user_defined = get_widget_checked("rb_user_defined");
            GtkWidget *rb_truecolor = get_widget_checked("rb_truecolor");
            GtkWidget *rb_falsecolor = get_widget_checked("rb_falsecolor");

            int is_user_defined = gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_user_defined));
            int is_truecolor = gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_truecolor));
            int is_falsecolor = gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_falsecolor));
            g_assert(is_user_defined + is_truecolor + is_falsecolor == 1);

            GtkWidget *optical_bands_hbox =
                get_widget_checked("optical_bands_hbox");

            gtk_widget_set_sensitive(optical_bands_hbox, is_user_defined);

            if (is_truecolor) {
                set_combo_box_item_checked("red_optical_combo", 3);
                set_combo_box_item_checked("green_optical_combo", 2);
                set_combo_box_item_checked("blue_optical_combo", 1);
            } else if (is_falsecolor) {
                set_combo_box_item_checked("red_optical_combo", 4);
                set_combo_box_item_checked("green_optical_combo", 3);
                set_combo_box_item_checked("blue_optical_combo", 2);
            }
        }
    }
    else
    {
        // "Export All Bands as Separate Images"
        // don't need to do anything further -- we've already disabled the
        // whole "RGB" options section
    }
}

SIGNAL_CALLBACK void
on_rb_all_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_rgb_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_radar_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_optical_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_truecolor_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_falsecolor_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_user_defined_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_pauli_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_sinclair_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_user_defined_radar_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

