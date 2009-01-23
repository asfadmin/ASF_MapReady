#include "asf_convert_gui.h"

static int db_was_checked = 0;

void export_checkbutton_toggle();

void
show_full_paths_checkbutton_toggle()
{
  GtkWidget *show_full_paths_checkbutton;

  show_full_paths_checkbutton = get_widget_checked("show_full_paths_checkbutton");
  show_full_paths =
      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(show_full_paths_checkbutton));

  refresh_file_names();
}

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
        *airsar_label,
        *airsar_p_pol_checkbutton,
        *airsar_l_pol_checkbutton,
        *airsar_c_pol_checkbutton,
        *airsar_c_vv_checkbutton,
        *airsar_l_vv_checkbutton;

    gint input_data_format;
    gboolean show_data_type_combobox;
    gboolean show_latitude_spinbuttons;
    // Leave this next one false until we support level 0 again
    gboolean show_process_to_level1_checkbutton = FALSE;
    gboolean show_airsar_checkbuttons;
    gboolean enable_terrain_correction;
    gboolean enable_polarimetry;

    input_data_format_combobox =
        get_widget_checked("input_data_format_combobox");

    input_data_format =
        gtk_option_menu_get_history(GTK_OPTION_MENU(input_data_format_combobox));
    switch (input_data_format)
    {
        //case INPUT_FORMAT_STF:
            //show_data_type_combobox = FALSE;
            //show_latitude_spinbuttons = TRUE;
            //show_process_to_level1_checkbutton = FALSE;
            //show_airsar_checkbuttons = FALSE;
            //break;
        //case INPUT_FORMAT_COMPLEX:
            //show_data_type_combobox = FALSE;
            //show_latitude_spinbuttons = FALSE;
            //show_process_to_level1_checkbutton = FALSE;
            //show_airsar_checkbuttons = FALSE;
            //break;
        //case INPUT_FORMAT_CEOS_LEVEL0:
            //show_data_type_combobox = FALSE;
            //show_latitude_spinbuttons = FALSE;
            //show_process_to_level1_checkbutton = TRUE;
            //show_airsar_checkbuttons = FALSE;
            //break;
        default:
        case INPUT_FORMAT_CEOS_LEVEL1:
        case INPUT_FORMAT_ESRI:
        case INPUT_FORMAT_ENVI:
        case INPUT_FORMAT_POLSARPRO:
            show_data_type_combobox = TRUE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            show_airsar_checkbuttons = FALSE;
            enable_terrain_correction = TRUE;
            enable_polarimetry = TRUE;
            break;
        case INPUT_FORMAT_GEOTIFF:
            show_data_type_combobox = TRUE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            show_airsar_checkbuttons = FALSE;
            enable_terrain_correction = FALSE;
            enable_polarimetry = FALSE;
            break;
        case INPUT_FORMAT_ASF_INTERNAL:
            show_data_type_combobox = FALSE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            show_airsar_checkbuttons = FALSE;
            enable_terrain_correction = TRUE;
            enable_polarimetry = TRUE;
            break;
      case INPUT_FORMAT_AIRSAR:
            show_data_type_combobox = FALSE;
            show_latitude_spinbuttons = FALSE;
            show_process_to_level1_checkbutton = FALSE;
            show_airsar_checkbuttons = TRUE;
            enable_terrain_correction = FALSE;
            // "temporarily" turning of polarimetry for AirSAR -- this is
            // not yet working for 2.1, hopefully can be turned back on later
            //enable_polarimetry = TRUE;
            enable_polarimetry = FALSE;
            break;
    }

    latitude_checkbutton = get_widget_checked("latitude_checkbutton");
    latitude_low_label = get_widget_checked("latitude_low_label");
    latitude_low_entry = get_widget_checked("latitude_low_entry");
    latitude_hi_label = get_widget_checked("latitude_hi_label");
    latitude_hi_entry = get_widget_checked("latitude_hi_entry");
    process_to_level1_checkbutton = get_widget_checked("process_to_level1_checkbutton");

    gtk_widget_set_sensitive(latitude_checkbutton, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_low_label, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_low_entry, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_hi_label, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(latitude_hi_entry, show_latitude_spinbuttons);
    gtk_widget_set_sensitive(process_to_level1_checkbutton,
                             show_process_to_level1_checkbutton);

    airsar_label = get_widget_checked("airsar_label");
    airsar_p_pol_checkbutton = get_widget_checked("airsar_p_pol_checkbutton");
    airsar_l_pol_checkbutton = get_widget_checked("airsar_l_pol_checkbutton");
    airsar_c_pol_checkbutton = get_widget_checked("airsar_c_pol_checkbutton");
    airsar_c_vv_checkbutton = get_widget_checked("airsar_c_vv_checkbutton");
    airsar_l_vv_checkbutton = get_widget_checked("airsar_l_vv_checkbutton");

    if (show_airsar_checkbuttons) {
      gtk_widget_show(airsar_p_pol_checkbutton);
      gtk_widget_show(airsar_l_pol_checkbutton);
      gtk_widget_show(airsar_c_pol_checkbutton);
      gtk_widget_show(airsar_c_vv_checkbutton);
      gtk_widget_show(airsar_l_vv_checkbutton);
      gtk_widget_show(airsar_label);
    } else {
      gtk_widget_hide(airsar_p_pol_checkbutton);
      gtk_widget_hide(airsar_l_pol_checkbutton);
      gtk_widget_hide(airsar_c_pol_checkbutton);
      gtk_widget_hide(airsar_c_vv_checkbutton);
      gtk_widget_hide(airsar_l_vv_checkbutton);
      gtk_widget_hide(airsar_label);
    }

    if (show_process_to_level1_checkbutton)
    {
        gboolean process_to_level1_checkbutton_is_checked =
            gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(process_to_level1_checkbutton));

        if (!process_to_level1_checkbutton_is_checked)
        {
            enable_terrain_correction = FALSE;
            enable_polarimetry = FALSE;
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

    GtkWidget *dem_checkbutton = get_widget_checked("dem_checkbutton");
    if (!enable_terrain_correction)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dem_checkbutton),
                                     FALSE);
    gtk_widget_set_sensitive(dem_checkbutton, enable_terrain_correction);
    terrcorr_options_changed();

    GtkWidget *polarimetry_checkbutton =
        get_widget_checked("polarimetry_checkbutton");
    if (!enable_polarimetry)
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(polarimetry_checkbutton), FALSE);
    gtk_widget_set_sensitive(polarimetry_checkbutton, enable_polarimetry);
    polarimetry_settings_changed();
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

void
output_format_combobox_changed()
{
    GtkWidget *output_format_combobox =
      get_widget_checked("output_format_combobox");

    gint output_format = get_combo_box_item(output_format_combobox);

    GtkWidget *output_bytes_checkbutton =
      get_widget_checked("output_bytes_checkbutton");
    GtkWidget *scaling_method_combobox =
      get_widget_checked("scaling_method_combobox");

    switch (output_format)
    {
      default:
      case OUTPUT_FORMAT_JPEG:
      case OUTPUT_FORMAT_PNG:
      case OUTPUT_FORMAT_PGM:
      case OUTPUT_FORMAT_TIFF:
      case OUTPUT_FORMAT_ASF_INTERNAL:
      case OUTPUT_FORMAT_CEOS:
        gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(output_bytes_checkbutton), TRUE);
        set_combo_box_item(scaling_method_combobox, SCALING_METHOD_SIGMA);
        break;
      case OUTPUT_FORMAT_GEOTIFF:
        gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(output_bytes_checkbutton), FALSE);
        output_bytes_checkbutton_toggle();
        break;
    }

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
        *rgb_vbox,
        *vbox_export,
        *export_tab_label;

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
    vbox_export = get_widget_checked("vbox_export");
    export_tab_label = get_widget_checked("export_tab_label");

    if (export_checked)
    {
        output_format = get_combo_box_item(output_format_combobox);

        switch (output_format)
        {
          default:
          case OUTPUT_FORMAT_JPEG:
          case OUTPUT_FORMAT_PNG:
          case OUTPUT_FORMAT_PGM:
          case OUTPUT_FORMAT_TIFF:
          case OUTPUT_FORMAT_ASF_INTERNAL:
          case OUTPUT_FORMAT_CEOS:
            gtk_widget_set_sensitive(output_bytes_checkbutton, FALSE);
            gtk_widget_set_sensitive(scaling_method_combobox, TRUE);
            gtk_widget_set_sensitive(scaling_method_label, TRUE);

            break;
          case OUTPUT_FORMAT_GEOTIFF:
            gtk_widget_set_sensitive(output_bytes_checkbutton, TRUE);
            output_bytes_checkbutton_toggle();
            break;
        }

        gtk_widget_set_sensitive(vbox_export, TRUE);
        gtk_widget_set_sensitive(export_tab_label, TRUE);
        gtk_widget_set_sensitive(output_format_combobox, TRUE);

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
        gtk_widget_set_sensitive(vbox_export, FALSE);
        gtk_widget_set_sensitive(export_tab_label, FALSE);
        gtk_widget_set_sensitive(output_format_combobox, FALSE);
        gtk_widget_set_sensitive(output_bytes_checkbutton, FALSE);
        gtk_widget_set_sensitive(scaling_method_combobox, FALSE);
        gtk_widget_set_sensitive(scaling_method_label, FALSE);
        gtk_widget_set_sensitive(rgb_vbox, FALSE);
    }

    update_all_extensions();
}
/*
static int confirm_quit()
{
    GtkWidget *dialog = gtk_dialog_new_with_buttons( "Confirm Exit",
        NULL,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_STOCK_YES,
        GTK_RESPONSE_ACCEPT,
        GTK_STOCK_NO,
        GTK_RESPONSE_REJECT,
        NULL);

    GtkWidget *label = gtk_label_new("Do You Really Want To Quit?");

    g_signal_connect_swapped(dialog,
        "response",
        G_CALLBACK(gtk_widget_destroy),
        dialog);

    gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

    // Seems that sometimes the message box ends up hidden behind other
    // windows... this might bring it to the front
    gtk_window_present(GTK_WINDOW(dialog));

    int ret = gtk_dialog_run(GTK_DIALOG(dialog));

    return ret == GTK_RESPONSE_ACCEPT;
}
*/

SIGNAL_CALLBACK void
on_asf_convert_destroy(GtkWidget *widget, gpointer data)
{
  //if (confirm_quit()) {
    set_stop();
    clear_completed_tmp_dirs();
    gtk_main_quit();
  //}
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

//SIGNAL_CALLBACK void
//on_complex_activate(GtkWidget *widget)
//{
//    input_data_format_combobox_changed();
//    update_summary();
//}

SIGNAL_CALLBACK void
on_geotiff_input_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
  update_summary();
}

SIGNAL_CALLBACK void
on_asf_internal_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
  update_summary();
}

SIGNAL_CALLBACK void
on_airsar_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
  update_summary();
}

SIGNAL_CALLBACK void
on_polsarpro_activate(GtkWidget *widget)
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
on_png_activate(GtkWidget *widget)
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

SIGNAL_CALLBACK void
on_show_full_paths_checkbutton_toggled(GtkWidget *widget)
{
  show_full_paths_checkbutton_toggle();
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

void clear_completed_tmp_dirs()
{
    if (get_checked("rb_keep_temp")) {

        GtkTreeIter iter;
        gboolean valid = gtk_tree_model_get_iter_first(
          GTK_TREE_MODEL(completed_list_store), &iter);

        int first = TRUE;
        while (valid)
        {
            if (first) {
                asfPrintStatus("Removing temporary directories...\n");
                first = FALSE;
            }

            gchar *tmp_dir;
            gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter,
                               COMP_COL_TMP_DIR, &tmp_dir, -1);

            if (tmp_dir && strlen(tmp_dir) > 0) {
              asfPrintStatus("Removing: %s\n", tmp_dir);
              remove_dir(tmp_dir);
            }

            valid = gtk_tree_model_iter_next(
              GTK_TREE_MODEL(completed_list_store), &iter);
        }
    }
}

SIGNAL_CALLBACK void
on_clear_button_clicked(GtkWidget *widget)
{
  clear_completed_tmp_dirs();
  gtk_list_store_clear(completed_list_store);
}

void rgb_combo_box_setup()
{
    rb_select("rb_all", TRUE);
    rb_select("rb_rgb", FALSE);
    rb_select("rb_rgb_polar", FALSE);

    rb_select("rb_truecolor", FALSE);
    rb_select("rb_falsecolor", FALSE);
    rb_select("rb_user_defined", TRUE);

    rgb_settings_changed();
}

void rgb_settings_changed()
{
    GtkWidget *rb_rgb = get_widget_checked("rb_rgb");
    GtkWidget *rb_rgb_polar = get_widget_checked("rb_rgb_polar");
    GtkWidget *rgb_vbox = get_widget_checked("rgb_vbox");

    int is_polarimetric= get_checked("polarimetry_checkbutton");
    int is_polarimetric_decomp= get_checked("polarimetric_decomp_checkbutton");
    int is_cloude_noclassify;
    if (is_polarimetric && is_polarimetric_decomp) {
        is_cloude_noclassify = get_checked("rb_cloude_noclassify");
        if (!is_cloude_noclassify) {
            rb_select("rb_all", FALSE);
            rb_select("rb_rgb_polar", TRUE);
            rb_select("rb_rgb", FALSE);

            enable_widget("rb_all", FALSE);
            enable_widget("rb_rgb", FALSE);
            enable_widget("rb_rgb_polar", TRUE);
        }
        else {
            int rgb_polar_checked = get_checked("rb_rgb_polar");
            if (rgb_polar_checked) {
              rb_select("rb_rgb_polar", FALSE);
              rb_select("rb_all", TRUE);
            }

            enable_widget("rb_all", TRUE);
            enable_widget("rb_rgb", TRUE);
            enable_widget("rb_rgb_polar", FALSE);
        }
    }
    else {
        is_cloude_noclassify = FALSE;

        if (get_checked("rb_rgb_polar")) {
            // user had selected a no-longer-valid option
            rb_select("rb_all", TRUE);
            rb_select("rb_rgb_polar", FALSE);
        }

        enable_widget("rb_all", TRUE);
        enable_widget("rb_rgb", TRUE);
        enable_widget("rb_rgb_polar", FALSE);
    }

    gboolean is_rgb = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rb_rgb));
    gtk_widget_set_sensitive(rgb_vbox, is_rgb);

    gboolean is_rgb_polar =
      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rb_rgb_polar));

    if (is_rgb)
    {
        // "Export Multiple Bands in a single RGB Image"
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

      GtkWidget *bands_hbox = get_widget_checked("bands_hbox");
      gtk_widget_set_sensitive(bands_hbox, is_user_defined);

      // offset to get to the start of the optical bands
      const int OPT_BASE = 4;

      if (is_truecolor) {
        set_combo_box_item_checked("red_combo", 3 + OPT_BASE);
        set_combo_box_item_checked("green_combo", 2 + OPT_BASE);
        set_combo_box_item_checked("blue_combo", 1 + OPT_BASE);
      } else if (is_falsecolor) {
        set_combo_box_item_checked("red_combo", 4 + OPT_BASE);
        set_combo_box_item_checked("green_combo", 3 + OPT_BASE);
        set_combo_box_item_checked("blue_combo", 2 + OPT_BASE);
      }
    }
    else if (is_rgb_polar)
    {
        // "Export Using Polarimetric Settings"
        // Here, we want to enable the band selection if the user
        // has chosen the "rb_cloude_noclassify" option, which has entropy etc
        gtk_widget_set_sensitive(rgb_vbox, is_cloude_noclassify);
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
on_rb_rgb_polar_toggled(GtkWidget *widget)
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
on_rb_cloude8_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_freeman_durden_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_cloude16_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_cloude_noclassify_toggled(GtkWidget *widget)
{
    rgb_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_fr_local_toggled(GtkWidget *widget)
{
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_fr_global_toggled(GtkWidget *widget)
{
    update_summary();
}

void polarimetry_settings_changed()
{
    int polarimetry_is_checked = get_checked("polarimetry_checkbutton");

    GtkWidget *input_data_type_combobox =
      get_widget_checked("input_data_type_combobox");
    GtkWidget *polarimetry_tab_label =
      get_widget_checked("polarimetry_tab_label");
    GtkWidget *vbox_polarimetry =
      get_widget_checked("vbox_polarimetry");

    gtk_widget_set_sensitive(polarimetry_tab_label, polarimetry_is_checked);
    gtk_widget_set_sensitive(vbox_polarimetry, polarimetry_is_checked);

    if (polarimetry_is_checked) {
      int decomp_checked = get_checked("polarimetric_decomp_checkbutton");

      GtkWidget *polarimetric_decomp_vbox =
        get_widget_checked("polarimetric_decomp_vbox");
      gtk_widget_set_sensitive(polarimetric_decomp_vbox, decomp_checked);

      if (decomp_checked) {
        // when a polarimetric decomposition is turned on,
        // automatically select "SIGMA" and no db
        set_combo_box_item(input_data_type_combobox, INPUT_TYPE_SIGMA);
        gtk_widget_set_sensitive(input_data_type_combobox, FALSE);
        if (get_checked("checkbutton_db")) {
          db_was_checked = get_checked("checkbutton_db"); // see input_data_type_combobox_changed()
          set_checked("checkbutton_db", FALSE);
        }
      }
      else {
          // Make sure combobox is enabled
          gtk_widget_set_sensitive(input_data_type_combobox, TRUE);
      }

      int farcorr_checked = get_checked("farcorr_checkbutton");

      GtkWidget *farcorr_vbox = get_widget_checked("farcorr_vbox");
      gtk_widget_set_sensitive(farcorr_vbox, farcorr_checked);

      input_data_type_combobox_changed();
    }

    rgb_settings_changed();
}

SIGNAL_CALLBACK void
on_polarimetry_checkbutton_toggled(GtkWidget * widget)
{
    polarimetry_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_external_checkbutton_toggled(GtkWidget * widget)
{
    external_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_polarimetric_decomp_checkbutton_toggled(GtkWidget *widget)
{
    polarimetry_settings_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_farcorr_checkbutton_toggled(GtkWidget *widget)
{
    polarimetry_settings_changed();
    update_summary();
}

static int files_is_shown = TRUE;
static int completed_files_is_shown = FALSE;

static void show_widget(const char *widget_name, int show)
{
    GtkWidget *w = get_widget_checked(widget_name);
    if (show)
        gtk_widget_show(w);
    else
        gtk_widget_hide(w);
}

SIGNAL_CALLBACK void
on_settings_button_expanded_clicked(GtkWidget *widget)
{
    show_widget("settings_hbox_collapsed", TRUE);
    show_widget("settings_hbox_expanded", FALSE);
}

SIGNAL_CALLBACK void
on_settings_button_collapsed_clicked(GtkWidget *widget)
{
    show_widget("settings_hbox_collapsed", FALSE);
    show_widget("settings_hbox_expanded", TRUE);
}

SIGNAL_CALLBACK void
on_files_button_expanded_clicked(GtkWidget *widget)
{
    files_is_shown = FALSE;
    show_widget("files_hbox_collapsed", TRUE);
    show_widget("files_hbox_expanded", FALSE);
    show_widget("hbox_spacer",
                !completed_files_is_shown && !files_is_shown);
}

SIGNAL_CALLBACK void
on_files_button_collapsed_clicked(GtkWidget *widget)
{
    files_is_shown = TRUE;
    show_widget("files_hbox_collapsed", FALSE);
    show_widget("files_hbox_expanded", TRUE);
    show_widget("hbox_spacer",
                !completed_files_is_shown && !files_is_shown);
}

SIGNAL_CALLBACK void
on_completed_files_button_expanded_clicked(GtkWidget *widget)
{
    completed_files_is_shown = FALSE;
    show_widget("completed_files_hbox_collapsed", TRUE);
    show_widget("completed_files_hbox_expanded", FALSE);
    show_widget("hbox_spacer",
                !completed_files_is_shown && !files_is_shown);
}

SIGNAL_CALLBACK void
on_completed_files_button_collapsed_clicked(GtkWidget *widget)
{
    completed_files_is_shown = TRUE;
    show_widget("completed_files_hbox_collapsed", FALSE);
    show_widget("completed_files_hbox_expanded", TRUE);
    show_widget("hbox_spacer",
                !completed_files_is_shown && !files_is_shown);
}

void hide_sections_for_execute()
{
    on_settings_button_expanded_clicked(NULL);
    on_completed_files_button_collapsed_clicked(NULL);
}

SIGNAL_CALLBACK void
on_about_button_clicked(GtkWidget *widget)
{
    GtkWidget *about_dialog;
    GtkStyle *defStyle, *copyStyle;
    GdkColor whiteColor = {0, 0xFFFF, 0xFFFF, 0xFFFF};

    about_dialog = get_widget_checked("about_dialog");

    // Change background of window to white ...hackish, but this is
    // what works with GTK+
    defStyle = gtk_widget_get_default_style();
    copyStyle = gtk_style_copy(defStyle);
    copyStyle->bg[GTK_STATE_NORMAL] = whiteColor;
    defStyle = gtk_style_copy(copyStyle);
    gtk_widget_set_style(about_dialog, copyStyle);

    gtk_widget_show(about_dialog);
}

void close_about_dialog()
{
  GtkWidget *about_dialog;

  about_dialog = get_widget_checked("about_dialog");
  gtk_widget_hide(about_dialog);
}

SIGNAL_CALLBACK void
on_about_dialog_close(GtkWidget *widget)
{
  close_about_dialog();
}

SIGNAL_CALLBACK void
on_about_dialog_delete_event(GtkWidget *widget)
{
  close_about_dialog();
}

SIGNAL_CALLBACK void
on_about_dialog_destroy_event(GtkWidget *widget)
{
  close_about_dialog();
}

SIGNAL_CALLBACK void
on_about_dialog_destroy(GtkWidget *widget)
{
  close_about_dialog();
}

SIGNAL_CALLBACK void
on_about_dialog_ok_button_clicked(GtkWidget *widget)
{
  close_about_dialog();
}

SIGNAL_CALLBACK void
on_about_dialog_key_press_event(GtkWidget *widget)
{
  close_about_dialog();
}

void input_data_type_combobox_changed()
{
    int polarimetry_is_checked = get_checked("polarimetry_checkbutton");
    int decomp_checked = get_checked("polarimetric_decomp_checkbutton");
    GtkWidget *input_data_type_combobox =
        get_widget_checked("input_data_type_combobox");
    GtkWidget *checkbutton_db =
        get_widget_checked("checkbutton_db");
    int data_type = get_combo_box_item(input_data_type_combobox);

    // Manage the 'Scale output to decibels (dB)' check box
    // Note: If polarimetry is checked, the radiometry is forced to sigma before
    // input_data_type_combobox_changed() is called
    if (data_type == INPUT_TYPE_SIGMA ||
        data_type == INPUT_TYPE_BETA  ||
        data_type == INPUT_TYPE_GAMMA)
    {
        // Sigma, Beta, or Gamma
        if (polarimetry_is_checked && decomp_checked) {
            db_was_checked = get_checked("checkbutton_db");
            set_checked("checkbutton_db", FALSE);
            gtk_widget_set_sensitive(checkbutton_db, FALSE);
        }
        else {
            set_checked("checkbutton_db", db_was_checked);
            gtk_widget_set_sensitive(checkbutton_db, TRUE);
        }
    }
    else {
        // Power & Amplitude
        db_was_checked = get_checked("checkbutton_db");
        set_checked("checkbutton_db", FALSE);
        gtk_widget_set_sensitive(checkbutton_db, FALSE);
    }
    if (!(polarimetry_is_checked && decomp_checked)) {
        // Make sure combobox is enabled
        GtkWidget *input_data_type_combobox = get_widget_checked("input_data_type_combobox");
        gtk_widget_set_sensitive(input_data_type_combobox, TRUE);
    }

    // Manage the ERS2 gain fix checkbutton
    GtkWidget *ers2_gain_fix_checkbutton = get_widget_checked("ers2_gain_fix_checkbutton");
    if (data_type == INPUT_TYPE_AMP ||
        data_type == INPUT_TYPE_POWER)
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ers2_gain_fix_checkbutton), FALSE);
      gtk_widget_set_sensitive(ers2_gain_fix_checkbutton, FALSE);
    }
    else {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ers2_gain_fix_checkbutton), TRUE);
      gtk_widget_set_sensitive(ers2_gain_fix_checkbutton, TRUE);
    }

    update_summary();
}

SIGNAL_CALLBACK void
on_input_data_type_combobox_changed(GtkWidget *widget)
{
  input_data_type_combobox_changed();
}

SIGNAL_CALLBACK void
on_ers2_gain_fix_checkbutton_toggled(GtkWidget *widget)
{
  update_summary();
}

SIGNAL_CALLBACK void
on_checkbutton_db_toggled(GtkWidget *widget)
{
  update_summary();
}

SIGNAL_CALLBACK void
on_view_dem_button_clicked(GtkWidget *widget)
{
    GtkWidget *dem_entry = get_widget_checked("dem_entry");
    char *dem = STRDUP(gtk_entry_get_text(GTK_ENTRY(dem_entry)));
    show_image_with_asf_view(dem);
    free(dem);
}
