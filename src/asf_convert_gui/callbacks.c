#include "asf_convert_gui.h"

static int db_was_checked = 0;

void export_checkbutton_toggle();
int polsarpro_geocoding_check();
int polsarpro_data_check();

// When the polsarpro classification optionmenu changes, make new (colormapped) thumbnails
// for all polsarpro files in the input files list
void update_polsarpro_input_file_thumbnails()
{
  GtkTreeIter iter;
  gboolean more_items = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
  while (more_items) {
    gchar *input_file;
    gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                       COL_INPUT_FILE, &input_file,
                       -1);
    if (input_file && is_polsarpro(input_file)) {
      add_thumbnail(&iter);
    }
    g_free(input_file);
    more_items = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
  }
}

void
show_full_path_names_checkbutton_toggled()
{
  GtkWidget *show_full_path_names_checkbutton;

  show_full_path_names_checkbutton = get_widget_checked("show_full_path_names_checkbutton");
  show_full_paths =
      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(show_full_path_names_checkbutton));

  refresh_file_names();
}

void
show_thumbnails_checkbutton_toggled()
{
    g_show_thumbnail_columns = !g_show_thumbnail_columns;
    show_thumbnail_columns();
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
input_data_formats_changed()
{
    GtkWidget *input_data_type_combobox,
        *input_data_type_label,
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

    gboolean show_data_type_combobox;
    gboolean show_latitude_spinbuttons;
    // Leave this next one false until we support level 0 again
    gboolean show_process_to_level1_checkbutton = FALSE;
    gboolean show_airsar_checkbuttons;
    gboolean enable_terrain_correction;
    gboolean enable_polarimetry;
    gboolean enable_polarimetry_pauli;
    gboolean enable_polarimetry_sinclair;
    gboolean enable_polarimetry_cloude8;
    gboolean enable_polarimetry_cloude16;
    gboolean enable_polarimetry_noclassify;
    gboolean enable_polarimetry_freeman;
    gboolean enable_faraday_correction;

    // go through the input files list to see what kinds of data we have
    // initially, everything that is conditionally enabled is set to
    // false.  If we find that no data has been added yet, we'll turn a
    // few things back on, though.
    show_data_type_combobox = FALSE;
    show_latitude_spinbuttons = FALSE;
    show_process_to_level1_checkbutton = FALSE;
    show_airsar_checkbuttons = FALSE;
    enable_terrain_correction = FALSE;
    enable_polarimetry = FALSE;
    enable_polarimetry_pauli = FALSE;
    enable_polarimetry_sinclair = FALSE;
    enable_polarimetry_cloude8 = FALSE;
    enable_polarimetry_cloude16 = FALSE;
    enable_polarimetry_noclassify = FALSE;
    enable_polarimetry_freeman = FALSE;
    enable_faraday_correction = FALSE;
    char formats[512];
    strcpy(formats, "");

    int valid,num=0;
    GtkTreeIter iter;
    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
    if(!valid) {
      enable_polarimetry_pauli = TRUE;
      enable_polarimetry_sinclair = TRUE;
      enable_polarimetry_cloude8 = TRUE;
      enable_polarimetry_cloude16 = TRUE;
      enable_polarimetry_noclassify = TRUE;
      enable_polarimetry_freeman = TRUE;
    }
    while (valid) {
      ++num;

      gchar *file, *metadata_file;
      gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                         COL_INPUT_FILE, &file,
                         COL_METADATA_FILE, &metadata_file,
                         -1);

      if (is_polsarpro(file)) {
        show_data_type_combobox = TRUE;
        enable_terrain_correction = TRUE;
        enable_polarimetry = TRUE;
        enable_polarimetry_pauli = TRUE;
        enable_polarimetry_sinclair = TRUE;
        enable_polarimetry_cloude8 = TRUE;
        enable_polarimetry_cloude16 = TRUE;
        enable_polarimetry_noclassify = TRUE;
        enable_polarimetry_freeman = TRUE;
        enable_faraday_correction = TRUE;
        if (!strstr(formats, "PolSARpro"))
          strcat(formats, "PolSARpro, ");
      }
      else if (is_geotiff(file)) {
        //show_data_type_combobox = TRUE;
        if (!strstr(formats, "GeoTIFF"))
          strcat(formats, "GeoTIFF, ");
      }
      else if (is_asf_internal(file)) {
        enable_terrain_correction = TRUE;
        enable_polarimetry = TRUE;
        enable_polarimetry_pauli = TRUE;
        enable_polarimetry_sinclair = TRUE;
        enable_polarimetry_cloude8 = TRUE;
        enable_polarimetry_cloude16 = TRUE;
        enable_polarimetry_noclassify = TRUE;
        enable_polarimetry_freeman = TRUE;
        enable_faraday_correction = TRUE;
        if (!strstr(formats, "ASF Internal"))
          strcat(formats, "ASF Internal, ");
      }
      else if (is_uavsar_polsar(file) || is_uavsar_insar(file)) {
        enable_polarimetry = TRUE;
        enable_polarimetry_pauli = TRUE;
        enable_terrain_correction = TRUE;
        if(!strstr(formats, "UAVSAR"))
          strcat(formats, "UAVSAR, ");
      }
      else if (is_airsar(file)) {
        show_airsar_checkbuttons = TRUE;
        if (!strstr(formats, "AirSAR"))
          strcat(formats, "AirSAR, ");
      }
      else if (is_terrasarx(file)) {
        if (!strstr(formats, "TerraSAR-X"))
          strcat(formats, "TerraSAR-X, ");
        enable_terrain_correction = TRUE;
      }
      else if (is_radarsat2(file)) {
        if (!strstr(formats, "Radarsat-2"))
          strcat(formats, "Radarsat-2, ");
        enable_terrain_correction = TRUE;
      }
      else if (is_roipac(file)) {
        if (!strstr(formats, "ROI_PAC"))
          strcat(formats, "ROI_PAC, ");
        enable_terrain_correction = TRUE;
      }
      else if (strlen(metadata_file)>0) {
        // as of now, only gamma uses the metadata column...
        if (!strstr(formats, "GAMMA"))
          strcat(formats, "GAMMA, ");
        enable_terrain_correction = TRUE;
      }
      else { // probably CEOS L1... ?
        show_data_type_combobox = TRUE;
        enable_terrain_correction = TRUE;
        enable_polarimetry = TRUE;
        enable_polarimetry_pauli = TRUE;
        enable_polarimetry_sinclair = TRUE;
        enable_polarimetry_cloude8 = TRUE;
        enable_polarimetry_cloude16 = TRUE;
        enable_polarimetry_noclassify = TRUE;
        enable_polarimetry_freeman = TRUE;
        enable_faraday_correction = TRUE;
        if (!strstr(formats, "CEOS L1"))
          strcat(formats, "CEOS L1, ");
      }

      g_free(file);
      valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }

    char input_formats[512];
    if (num>0 && strlen(formats)>2) {
      if (num>1)
        strcpy(input_formats, "Input data formats: ");
      else
        strcpy(input_formats, "Input data format: ");
      strcat(input_formats, formats);
      input_formats[strlen(input_formats)-2]='\0'; // remove last comma/space
    }
    else {
      strcpy(input_formats, "Input data formats: none");
    }
    put_string_to_label("input_formats_label", input_formats);

    if (num==0) {
      // no files added yet -- enable terrcorr & polarimetry, so the
      // user can check those features out
      enable_terrain_correction = TRUE;
      enable_polarimetry = TRUE;
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
    gtk_widget_set_sensitive(get_widget_checked("rb_pauli"), enable_polarimetry_pauli);
    gtk_widget_set_sensitive(get_widget_checked("rb_sinclair"), enable_polarimetry_sinclair);
    gtk_widget_set_sensitive(get_widget_checked("rb_cloude8"), enable_polarimetry_cloude8);
    gtk_widget_set_sensitive(get_widget_checked("rb_cloude16"), enable_polarimetry_cloude16);
    gtk_widget_set_sensitive(get_widget_checked("rb_cloude_noclassify"), enable_polarimetry_noclassify);
    gtk_widget_set_sensitive(get_widget_checked("rb_freeman_durden"), enable_polarimetry_freeman);

    GtkWidget *faraday_correction_checkbutton =
        get_widget_checked("farcorr_checkbutton");
    if (!enable_faraday_correction)
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(faraday_correction_checkbutton), FALSE);
    gtk_widget_set_sensitive(faraday_correction_checkbutton, enable_faraday_correction);

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
      case OUTPUT_FORMAT_POLSARPRO:
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
  	  case OUTPUT_FORMAT_POLSARPRO:
            gtk_widget_set_sensitive(output_bytes_checkbutton, FALSE);
            output_bytes_checkbutton_toggle();
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

SIGNAL_CALLBACK void
on_process_to_level1_checkbutton_toggled(GtkWidget *widget)
{
    input_data_formats_changed();
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

SIGNAL_CALLBACK void
on_polsarpro_export_activate(GtkWidget *widget)
{
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
on_show_full_path_names_checkbutton_toggled(GtkWidget *widget)
{
  show_full_path_names_checkbutton_toggled();
}

SIGNAL_CALLBACK void
on_show_thumbnails_checkbutton_toggled(GtkWidget *widget)
{
  show_thumbnails_checkbutton_toggled();
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

            g_free(tmp_dir);
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

    }
    else {
      set_checked("polarimetric_decomp_checkbutton", FALSE);
    }

    input_data_type_combobox_changed();

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

void polsarpro_classification_checkbutton_toggled()
{
  int is_checked = get_checked("polsarpro_classification_checkbutton");
  if (!is_checked) {
    // select colormap "None", then disable selection.
    GtkWidget *option_menu =
      get_widget_checked("browse_select_colormap_optionmenu");
    gtk_option_menu_set_history(GTK_OPTION_MENU(option_menu), 0);
  }
  enable_widget("browse_select_colormap_optionmenu", is_checked);
}

SIGNAL_CALLBACK void
on_polsarpro_classification_checkbutton_toggled(GtkWidget *widget)
{
  polsarpro_classification_checkbutton_toggled();
}

void polsarpro_image_data_type_changed()
{
  GtkWidget *look_up_table = 
    get_widget_checked("browse_select_colormap_optionmenu");
  GtkWidget *combo = 
    get_widget_checked("browse_select_image_data_type_optionmenu");
  int selected = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));
  GtkWidget *polsarpro_ancillary_file =
    get_widget_checked("add_file_with_ancillary_polsarpro_ceos_entry");
  GtkWidget *polsarpro_ancillary_browse_button = 
    get_widget_checked("add_file_with_ancillary_polsarpro_ceos_browse_button");
  switch (selected) 
    {
    case SELECT_POLARIMETRIC_SEGMENTATION:
      gtk_option_menu_set_history(GTK_OPTION_MENU(look_up_table), 0);
      enable_widget("browse_select_colormap_optionmenu", TRUE);  
      gtk_widget_set_sensitive(polsarpro_ancillary_file, FALSE);
      gtk_widget_set_sensitive(polsarpro_ancillary_browse_button, FALSE);
      put_string_to_label("polsarpro_data_label", "PolSARPro Data File:");
      break;
    case SELECT_POLARIMETRIC_DECOMPOSITION:
      gtk_option_menu_set_history(GTK_OPTION_MENU(look_up_table), 0);
      enable_widget("browse_select_colormap_optionmenu", FALSE);  
      gtk_widget_set_sensitive(polsarpro_ancillary_file, FALSE);
      gtk_widget_set_sensitive(polsarpro_ancillary_browse_button, FALSE);
      put_string_to_label("polsarpro_data_label", "PolSARPro Data File:");
      break;
    case SELECT_POLARIMETRIC_PARAMETER:
      gtk_option_menu_set_history(GTK_OPTION_MENU(look_up_table), 0);
      //enable_widget("browse_select_colormap_optionmenu", FALSE);  
      enable_widget("browse_select_colormap_optionmenu", TRUE);  
      gtk_widget_set_sensitive(polsarpro_ancillary_file, FALSE);
      gtk_widget_set_sensitive(polsarpro_ancillary_browse_button, FALSE);
      put_string_to_label("polsarpro_data_label", "PolSARPro Data File:");
      break;
    case SELECT_POLARIMETRIC_MATRIX:
      gtk_option_menu_set_history(GTK_OPTION_MENU(look_up_table), 0);
      enable_widget("browse_select_colormap_optionmenu", FALSE);  
      gtk_widget_set_sensitive(polsarpro_ancillary_file, FALSE);
      gtk_widget_set_sensitive(polsarpro_ancillary_browse_button, FALSE);
      put_string_to_label("polsarpro_data_label", "PolSARPro Matrix File:");
      break;
    }
  put_string_to_label("add_with_ancillary_error_label", "");
  clear_entries();
  GtkWidget *ok_button =
    get_widget_checked("add_file_with_ancillary_ok_button");
  if (polsarpro_geocoding_check() && polsarpro_data_check())
    gtk_widget_set_sensitive(ok_button, TRUE);
}

SIGNAL_CALLBACK void
on_browse_select_image_data_type_optionmenu_changed(GtkWidget *widget)
{
  polsarpro_image_data_type_changed();
}

int polsarpro_geocoding_check()
{
  int ret = FALSE;
  char *matrixType, *decompositionType;
  char infile[1024];
  char *errorMatrix = NULL, *errorDecomposition = NULL; 
  char *errorSegmentation = NULL, *errorParameter = NULL;
  envi_header *envi = NULL;
  meta_parameters *meta = NULL;
  char *inFile = 
    get_string_from_entry("add_file_with_ancillary_polsarpro_image_entry");
  strcpy(infile, inFile);
  GtkWidget *combo = 
    get_widget_checked("browse_select_image_data_type_optionmenu");
  int type = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));
  GtkWidget *ok_button =
    get_widget_checked("add_file_with_ancillary_ok_button");
  int is_polsarpro_matrix = (type == SELECT_POLARIMETRIC_MATRIX) ?
    isPolsarproMatrix(infile, &matrixType, &errorMatrix) : 0;
  int is_polsarpro_decomposition = 
    (type == SELECT_POLARIMETRIC_DECOMPOSITION) ?
    isPolsarproDecomposition(infile, &decompositionType, &errorDecomposition) :
    0;
  int is_polsarpro_segmentation =
    (type == SELECT_POLARIMETRIC_SEGMENTATION) ?    
    isPolsarproSegmentation(infile, &errorSegmentation) : 0;
  int is_polsarpro_parameter =
    (type == SELECT_POLARIMETRIC_PARAMETER) ?
    isPolsarproParameter(infile, &errorParameter) : 0;
  if ((is_polsarpro_matrix && !errorMatrix) ||
      (is_polsarpro_decomposition && !errorDecomposition) ||
      (is_polsarpro_segmentation && !errorSegmentation) ||
      (is_polsarpro_parameter && !errorParameter)) {
    if (is_polsarpro_matrix) {
      // Temporary fix - only for the case that we select file in matrix
      // directory
      char dirName[1024], fileName[1024];
      split_dir_and_file(infile, dirName, fileName);
      dirName[strlen(dirName)-1] = '\0';
      sprintf(infile, "%s", dirName);
      // This part works out of the box once the input is a directory
      if (strcmp(matrixType, "T3") == 0 || strcmp(matrixType, "T4") == 0)
	strcat(infile, "/T11.bin.hdr");
      else if (strcmp(matrixType, "C2") == 0 || 
	       strcmp(matrixType, "C3") == 0 ||
	       strcmp(matrixType, "C4") == 0)
	strcat(infile, "/C11.bin.hdr");
    }
    else if (is_polsarpro_decomposition || is_polsarpro_segmentation ||
	     is_polsarpro_parameter)
      strcat(infile, ".hdr");
    else if (strlen(infile) > 0)
      strcat(infile, ".hdr");
    if (fileExists(infile) && strlen(infile) > 0 && 
	(!is_dir(inFile) || is_polsarpro_matrix)) {
      envi = read_envi(infile);
      meta = envi2meta(envi);
      if (envi)
	free(envi);
      GtkWidget *polsarpro_ancillary_file =
	get_widget_checked("add_file_with_ancillary_polsarpro_ceos_entry");
      GtkWidget *polsarpro_ancillary_browse_button = 
	get_widget_checked("add_file_with_ancillary_polsarpro_ceos_browse_button");
      if (meta && !meta->projection) {
	gtk_widget_set_sensitive(polsarpro_ancillary_file, TRUE);
	gtk_widget_set_sensitive(polsarpro_ancillary_browse_button, TRUE);
	gtk_widget_set_sensitive(ok_button, FALSE);
      }
      else {
	gtk_widget_set_sensitive(polsarpro_ancillary_file, FALSE);
	gtk_widget_set_sensitive(polsarpro_ancillary_browse_button, FALSE);
	gtk_widget_set_sensitive(ok_button, TRUE);
      }
      if (meta)
	meta_free(meta);
    }
    put_string_to_label("add_with_ancillary_error_label", "");
    ret = TRUE;
  }
  if (errorSegmentation && type == SELECT_POLARIMETRIC_SEGMENTATION) {
    put_string_to_label("add_with_ancillary_error_label", errorSegmentation);
    gtk_widget_set_sensitive(ok_button, FALSE);
  }
  if (errorDecomposition && type == SELECT_POLARIMETRIC_DECOMPOSITION) {
    put_string_to_label("add_with_ancillary_error_label", errorDecomposition);
    gtk_widget_set_sensitive(ok_button, FALSE);
  }
  if (errorParameter && type == SELECT_POLARIMETRIC_PARAMETER) {
    put_string_to_label("add_with_ancillary_error_label", errorParameter);
    gtk_widget_set_sensitive(ok_button, FALSE);
  }
  if (errorMatrix && type == SELECT_POLARIMETRIC_MATRIX) {
    put_string_to_label("add_with_ancillary_error_label", errorMatrix);
    gtk_widget_set_sensitive(ok_button, FALSE);
  }

  return ret;
}

int polsarpro_data_check()
{
  char *error = NULL;
  int ret = FALSE;
  char *inFile = 
    get_string_from_entry("add_file_with_ancillary_polsarpro_ceos_entry");
  GtkWidget *ok_button =
    get_widget_checked("add_file_with_ancillary_ok_button");
  int is_ceos = isCEOS(inFile, &error);
  int is_terrasar = isTerrasar(inFile, &error);
  int is_radarsat2 = isRadarsat2(inFile, &error);
  if (is_ceos || is_terrasar || is_radarsat2)
    ret = TRUE;
  else {
    put_string_to_label("add_with_ancillary_error_label",
			"Could not find any complex CEOS, TerraSAR-X or "
			"Radarsat-2 data");
    gtk_widget_set_sensitive(ok_button, FALSE);
  }
 
  return ret;
}	

SIGNAL_CALLBACK void
on_add_file_with_ancillary_polsarpro_image_entry_changed(GtkWidget *widget)
{
  polsarpro_geocoding_check();
}
	
SIGNAL_CALLBACK void
on_add_file_with_ancillary_polsarpro_ceos_entry_changed(GtkWidget *widget)
{
  GtkWidget *ok_button =
    get_widget_checked("add_file_with_ancillary_ok_button");
  if (polsarpro_geocoding_check() && polsarpro_data_check())
    gtk_widget_set_sensitive(ok_button, TRUE);
}

SIGNAL_CALLBACK void
on_uavsar_polsar_all_checkbutton_toggled(GtkWidget *widget)
{
  GtkWidget *all_button = get_widget_checked("uavsar_polsar_all_proc_types");
  GtkWidget *polsar_table = get_widget_checked("uavsar_polsar_select_table");
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(all_button))) {
    GList *l, *table_children = gtk_container_get_children(GTK_CONTAINER(polsar_table));
    for(l = table_children; l; l = l->next) {
      if(GTK_WIDGET_SENSITIVE(l->data))
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(l->data), TRUE);
    }
    g_list_free(table_children);
  }
}

SIGNAL_CALLBACK void
on_uavsar_insar_all_checkbutton_toggled(GtkWidget *widget)
{
  GtkWidget *all_button = get_widget_checked("uavsar_insar_all_proc_types");
  GtkWidget *polsar_table = get_widget_checked("uavsar_insar_select_table");
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(all_button))) {
    GList *l, *table_children = gtk_container_get_children(GTK_CONTAINER(polsar_table));
    for(l = table_children; l; l = l->next) {
      if(GTK_WIDGET_SENSITIVE(l->data))
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(l->data), TRUE);
    }
    g_list_free(table_children);
  }
}

SIGNAL_CALLBACK void
on_uavsar_polsar_type_toggled(GtkWidget *widget)
{
  if(!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
    GtkWidget *all_button = get_widget_checked("uavsar_polsar_all_proc_types");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(all_button), FALSE);
  }

  GtkWidget *ok_button = get_widget_checked("add_file_with_ancillary_ok_button");
  gtk_widget_set_sensitive(ok_button, FALSE);
  GList *l, *uavsar_types_checkboxes = get_widgets_prefix_checked("uavsar_proc_type");
  for(l = uavsar_types_checkboxes; l; l = l->next)
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(l->data))) {
      gtk_widget_set_sensitive(ok_button, TRUE);
      break;
    }

  g_list_free(uavsar_types_checkboxes);
}

SIGNAL_CALLBACK void
on_uavsar_insar_type_toggled(GtkWidget *widget)
{
  if(!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget))) {
    GtkWidget *all_button = get_widget_checked("uavsar_insar_all_proc_types");
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(all_button), FALSE);
  }

  GtkWidget *ok_button = get_widget_checked("add_file_with_ancillary_ok_button");
  gtk_widget_set_sensitive(ok_button, FALSE);
  GList *l, *uavsar_types_checkboxes = get_widgets_prefix_checked("uavsar_proc_type");
  for(l = uavsar_types_checkboxes; l; l = l->next)
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(l->data))) {
      gtk_widget_set_sensitive(ok_button, TRUE);
      break;
    }

  g_list_free(uavsar_types_checkboxes);
}

/* When a user selects a valid uavsar annotation file, check to see if it is POLSAR or InSAR 
 * and enable the appropriate set of options. Furthermore, check to see what types of data
 * are available for the scene (i.e. MLC, GRD, DAT) and only enable those options
 * that we have data for.
 */
SIGNAL_CALLBACK void
on_add_file_with_ancillary_uavsar_annotation_file_entry_changed(GtkEditable *entry)
{
  const gchar *filename = gtk_entry_get_text(GTK_ENTRY(entry));

  GList *l, *uavsar_types_checkboxes = get_widgets_prefix_checked("uavsar_proc_type");
  for(l = uavsar_types_checkboxes; l; l = l->next)
    gtk_widget_set_sensitive(l->data, FALSE);
  uavsar_types_checkboxes = g_list_append(uavsar_types_checkboxes, get_widget_checked("uavsar_polsar_all_proc_types"));
  uavsar_types_checkboxes = g_list_append(uavsar_types_checkboxes, get_widget_checked("uavsar_insar_all_proc_types"));
  for(l = uavsar_types_checkboxes; l; l = l->next) {
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(l->data), FALSE);
    gtk_widget_set_sensitive(l->data, FALSE);
  }
  g_list_free(uavsar_types_checkboxes);

  enable_widget("uavsar_polsar_select_frame", FALSE);
  enable_widget("uavsar_insar_select_frame", FALSE);
  if(fileExists(filename)) {
    if(is_uavsar_polsar(filename)) {
      enable_widget("uavsar_polsar_select_frame", TRUE);

      char *types[4] = {"MLC", "HGT", "GRD", "DAT"};
      char **dataName, **element;
      int *dataType, nBands, i, j;
      for(i = 0; i < 4; i++) {
        get_uavsar_file_names(filename, uavsar_type_name_to_enum(types[i]), &dataName, &element, &dataType, &nBands);
        if(nBands) {
          GString *checkbutton_name = g_string_new(types[i]);
          g_string_prepend(checkbutton_name, "uavsar_proc_type_");
          enable_widget(checkbutton_name->str, TRUE);
          enable_widget("uavsar_polsar_all_proc_types", TRUE);
          for(j = 0; j < nBands; j++) {
            FREE(dataName[j]);
            FREE(element[j]);
          }
          FREE(dataType);
          FREE(dataName);
          FREE(element);
          g_string_free(checkbutton_name, TRUE);
        }
      }
    }
    else if(is_uavsar_insar(filename)) {
      enable_widget("uavsar_insar_select_frame", TRUE);

      char *types[9] = {"AMP", "AMP_GRD", "INT", "INT_GRD", "UNW", "UNW_GRD", "COR", "COR_GRD", "HGT_GRD"};
      char **dataName, **element;
      int *dataType, nBands, i, j;
      for(i = 0; i < 9; i++) {
        get_uavsar_file_names(filename, uavsar_type_name_to_enum(types[i]), &dataName, &element, &dataType, &nBands);
        if(nBands) {
          GString *checkbutton_name = g_string_new(types[i]);
          g_string_prepend(checkbutton_name, "uavsar_proc_type_");
          enable_widget(checkbutton_name->str, TRUE);
          enable_widget("uavsar_insar_all_proc_types", TRUE);
          for(j = 0; j < nBands; j++) {
            FREE(dataName[j]);
            FREE(element[j]);
          }
          FREE(dataType);
          FREE(dataName);
          FREE(element);
          g_string_free(checkbutton_name, TRUE);
        }
      }
    }
  }
}
