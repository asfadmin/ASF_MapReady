#include "asf_convert_gui.h"
#include "asf_convert.h"
#include "asf_raster.h"
#include "libasf_proj.h"
#include "asf_geocode.h"
#include "asf.h"
#include "asf_meta.h"
#include "get_ceos_names.h"
#include <ctype.h>
#include <errno.h>

datum_type_t get_datum_from_proj_file(char *file, projection_type_t type);

static int
settings_get_input_data_format_allows_latitude(const Settings *s)
{
    return -1;
}

static int
settings_get_output_format_requires_byte(const Settings *s)
{
    return s->output_format == OUTPUT_FORMAT_JPEG ||
        s->output_format == OUTPUT_FORMAT_PNG ||
        s->output_format == OUTPUT_FORMAT_PGM ||
        s->output_format == OUTPUT_FORMAT_TIFF;
}

static void
set_combo_box_entry_item(const char *widget_name, const char *entry_text)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    gtk_entry_set_text(e, entry_text);
}

static void
get_combo_box_entry_item(const char *widget_name, char *dest)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    strcpy(dest, gtk_entry_get_text(e));
}

void
settings_apply_to_gui(const Settings * s)
{
    GtkWidget
        *input_data_type_combobox,
        *export_checkbutton,
        *external_checkbutton,
        *output_format_combobox,
        *output_bytes_checkbutton,
        *scaling_method_combobox;

    input_data_type_combobox =
        get_widget_checked("input_data_type_combobox");

    external_checkbutton =
        get_widget_checked("external_checkbutton");

    export_checkbutton =
        get_widget_checked("export_checkbutton");

    output_format_combobox =
        get_widget_checked("output_format_combobox");

    set_combo_box_item(input_data_type_combobox, s->data_type);

    if (s->process_to_level1)
    {
        GtkWidget *process_to_level1_checkbutton =
            get_widget_checked("process_to_level1_checkbutton");

        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(process_to_level1_checkbutton),
            s->process_to_level1);
    }

    if (s->output_db)
    {
        GtkWidget *checkbutton_db = get_widget_checked("checkbutton_db");
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton_db), TRUE);
    }

    input_data_type_changed();

    if (settings_get_input_data_format_allows_latitude(s) > 0)
    {
        GtkWidget
            *latitude_checkbutton,
            *latitude_low_entry,
            *latitude_hi_entry;

        latitude_checkbutton =
            get_widget_checked("latitude_checkbutton");

        if (s->latitude_checked)
        {
            gchar tmp[32];

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(latitude_checkbutton), TRUE);

            latitude_low_entry =
                get_widget_checked("latitude_low_entry");

            latitude_hi_entry =
                get_widget_checked("latitude_hi_entry");

            sprintf(tmp, "%f", s->latitude_low);
            gtk_entry_set_text(GTK_ENTRY(latitude_low_entry), tmp);

            sprintf(tmp, "%f", s->latitude_hi);
            gtk_entry_set_text(GTK_ENTRY(latitude_hi_entry), tmp);
        }
        else
        {
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(latitude_checkbutton), FALSE);
        }
    }

    input_data_formats_changed();
    refresh_file_names();

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(external_checkbutton), s->external_is_checked);
    if (s->external_is_checked) {
      GtkWidget *external_optionmenu =
        get_widget_checked("external_optionmenu");
      gtk_option_menu_set_history(GTK_OPTION_MENU(external_optionmenu),
                                  s->external_selected);
    }

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(export_checkbutton), s->export_is_checked);

    set_combo_box_item(output_format_combobox, s->output_format);
    output_format_combobox_changed();

    int polarimetry_on =
        s->polarimetric_decomp_setting != POLARIMETRY_NONE || s->do_farcorr;

    if (polarimetry_on)
    {
        GtkWidget *polarimetry_checkbutton =
            get_widget_checked("polarimetry_checkbutton");
        gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(polarimetry_checkbutton), TRUE);

        GtkWidget *polarimetric_decomp_checkbutton =
            get_widget_checked("polarimetric_decomp_checkbutton");

        if (s->polarimetric_decomp_setting == POLARIMETRY_NONE) {
          gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(polarimetric_decomp_checkbutton), FALSE);
        }
        else {
          gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(polarimetric_decomp_checkbutton), TRUE);

          rb_select("rb_pauli",
                s->polarimetric_decomp_setting==POLARIMETRY_PAULI);
          rb_select("rb_sinclair",
                s->polarimetric_decomp_setting==POLARIMETRY_SINCLAIR);
          rb_select("rb_cloude8",
                s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE8);
          rb_select("rb_cloude16",
                s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE16);
          rb_select("rb_cloude_noclassify",
                s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE_NOCLASSIFY);
          rb_select("rb_freeman_durden",
                s->polarimetric_decomp_setting==POLARIMETRY_FREEMAN_DURDEN);
        }

        GtkWidget *farcorr_checkbutton =
          get_widget_checked("farcorr_checkbutton");

        if (s->do_farcorr) {
          gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(farcorr_checkbutton), TRUE);

          rb_select("rb_fr_local", !s->farcorr_global_avg);
          rb_select("rb_fr_global", s->farcorr_global_avg);

          if (s->farcorr_threshold > 0) {
            put_double_to_entry("farcorr_threshold_entry",
                                s->farcorr_threshold);
          }
          else {
            put_string_to_entry("farcorr_threshold_entry", "");
          }
        }
        else {
          gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(farcorr_checkbutton), FALSE);
        }

        polarimetry_settings_changed();
    }
    else
    {
        GtkWidget *polarimetry_checkbutton =
            get_widget_checked("polarimetry_checkbutton");
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(polarimetry_checkbutton), FALSE);
    }

    set_checked("ers2_gain_fix_checkbutton", s->apply_ers2_gain_fix);

    if (s->export_is_checked)
    {
        output_bytes_checkbutton =
            get_widget_checked("output_bytes_checkbutton");

        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(output_bytes_checkbutton),
            s->output_bytes);

        scaling_method_combobox =
            get_widget_checked("scaling_method_combobox");

        set_combo_box_item(scaling_method_combobox, s->scaling_method);

        if (s->export_bands) {
            rb_select("rb_rgb", TRUE);
            if (s->truecolor_is_checked)
            {
                // true color
                rb_select("rb_truecolor", TRUE);
            }
            else if (s->falsecolor_is_checked)
            {
                // false color
                rb_select("rb_falsecolor", TRUE);
            }
            else
            {
                // user defined
                rb_select("rb_user_defined", TRUE);
                rb_select("rb_rgb", TRUE);
                set_combo_box_entry_item("red_combo", s->red);
                set_combo_box_entry_item("green_combo", s->green);
                set_combo_box_entry_item("blue_combo", s->blue);
            }
        }
        else {
            set_combo_box_entry_item("red_combo", "-");
            set_combo_box_entry_item("green_combo", "-");
            set_combo_box_entry_item("blue_combo", "-");
            rb_select("rb_all", TRUE);
        }

        rgb_settings_changed();
    }

    if (s->geocode_is_checked)
    {
        gchar tmp[32];

        GtkWidget * geocode_checkbutton;

        geocode_checkbutton =
            get_widget_checked("geocode_checkbutton");

        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(geocode_checkbutton), s->geocode_is_checked);

        if (s->geocode_is_checked)
        {
            GtkWidget * projection_option_menu;
            GtkWidget * utm_zone_entry;
            GtkWidget * datum_option_menu;
            GtkWidget * resample_option_menu;

            projection_option_menu =
                get_widget_checked("projection_option_menu");

            set_combo_box_item(projection_option_menu, s->projection);
            geocode_options_changed();

            utm_zone_entry =
                get_widget_checked("utm_zone_entry");

            if (s->projection == UNIVERSAL_TRANSVERSE_MERCATOR)
            {
                if (s->zone != MAGIC_UNSET_INT && s->zone != 0)
                    sprintf(tmp, "%d", s->zone);
                else
                    strcpy(tmp, "");

                gtk_entry_set_text(GTK_ENTRY(utm_zone_entry), tmp);
            }
            else
            {
                gtk_entry_set_text(GTK_ENTRY(utm_zone_entry), "");
            }

            put_double_to_entry("central_meridian_entry", s->lon0);
            put_double_to_entry("latitude_of_origin_entry", s->lat0);
            put_double_to_entry("first_standard_parallel_entry", s->plat1);
            put_double_to_entry("second_standard_parallel_entry", s->plat2);
            put_double_to_entry("false_northing_entry", s->false_northing);
            put_double_to_entry("false_easting_entry", s->false_easting);

            set_checked("average_height_checkbutton", s->specified_height);

            if (s->specified_height)
                put_double_to_entry("average_height_entry", s->height);

            set_checked("pixel_size_checkbutton", s->specified_pixel_size);

            if (s->specified_pixel_size)
                put_double_to_entry("pixel_size_entry", s->pixel_size);

            datum_option_menu =
                get_widget_checked("datum_option_menu");

            set_combo_box_item(datum_option_menu, s->datum);

            resample_option_menu =
                get_widget_checked("resample_option_menu");

            set_combo_box_item(resample_option_menu, s->resample_method);

	    put_string_to_entry("spheroid_entry", 
				spheroid_string(s->spheroid));

            set_checked("force_checkbutton", s->geocode_force);
        }
    }

    if (s->terrcorr_is_checked || s->refine_geolocation_is_checked)
    {
        GtkWidget *dem_checkbutton;
        GtkWidget *dem_entry;
        GtkWidget *tc_pixel_size_checkbutton;
        GtkWidget *tc_pixel_size_entry;
        GtkWidget *interpolate_checkbutton;
        GtkWidget *rb_terrcorr;
        GtkWidget *rb_refine_geolocation;
        GtkWidget *mask_checkbutton;
        GtkWidget *interp_dem_holes_checkbutton;

        dem_checkbutton = get_widget_checked("dem_checkbutton");
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dem_checkbutton), TRUE);

        dem_entry = get_widget_checked("dem_entry");
        gtk_entry_set_text(GTK_ENTRY(dem_entry), s->dem_file);
        interp_dem_holes_checkbutton =
            get_widget_checked("interp_dem_holes_checkbutton");

        rb_terrcorr =
            get_widget_checked("rb_terrcorr");
        rb_refine_geolocation =
            get_widget_checked("rb_refine_geolocation");

        if (s->refine_geolocation_is_checked)
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(rb_refine_geolocation), TRUE);
        else if (s->terrcorr_is_checked)
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(rb_terrcorr), TRUE);

        if (s->terrcorr_is_checked)
        {
            GtkWidget *save_dem_checkbutton;
            GtkWidget *layover_mask_checkbutton;
            GtkWidget *radiometric_checkbutton;
            GtkWidget *save_incid_angles_checkbutton;
            GtkWidget *tc_matching_checkbutton;

            tc_pixel_size_checkbutton =
                get_widget_checked("tc_pixel_size_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(tc_pixel_size_checkbutton),
                s->specified_tc_pixel_size);

            tc_pixel_size_entry =
                get_widget_checked("tc_pixel_size_entry");

            if (s->specified_tc_pixel_size)
            {
                gchar tmp[32];
                sprintf(tmp, "%f", s->tc_pixel_size);
                gtk_entry_set_text(GTK_ENTRY(tc_pixel_size_entry), tmp);
            }
            else
            {
                gtk_entry_set_text(GTK_ENTRY(tc_pixel_size_entry), "");
            }

            tc_matching_checkbutton =
                get_widget_checked("tc_matching_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(tc_matching_checkbutton),
                !s->no_matching);

            if (s->no_matching)
            {
                put_double_to_entry("offset_x_entry", s->offset_x);
                put_double_to_entry("offset_y_entry", s->offset_y);
            }
            else
            {
                put_string_to_entry("offset_x_entry", "");
                put_string_to_entry("offset_y_entry", "");
            }

            interpolate_checkbutton =
                get_widget_checked("interpolate_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(interpolate_checkbutton), s->interp);

            save_dem_checkbutton =
                get_widget_checked("save_dem_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(save_dem_checkbutton), s->generate_dem);

            layover_mask_checkbutton =
                get_widget_checked("layover_mask_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(layover_mask_checkbutton),
                s->generate_layover_mask);

            radiometric_checkbutton =
                get_widget_checked("radiometric_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(radiometric_checkbutton),
                s->do_radiometric);

            if(s->do_radiometric)
            {
                save_incid_angles_checkbutton = get_widget_checked("save_incid_angles_checkbutton");

                gtk_toggle_button_set_active(
                    GTK_TOGGLE_BUTTON(save_incid_angles_checkbutton),
                    s->save_incid_angles);
            }

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(interp_dem_holes_checkbutton),
                s->interp_dem_holes);
        }


        mask_checkbutton =
            get_widget_checked("mask_checkbutton");

        if (s->auto_water_mask_is_checked || s->mask_file_is_checked)
        {
            GtkWidget *mask_entry;
            GtkWidget *rb_auto_water_mask, *rb_mask_file;

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(mask_checkbutton), TRUE);

            rb_auto_water_mask =
                get_widget_checked("rb_auto_water_mask");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(rb_auto_water_mask),
                s->auto_water_mask_is_checked);

            rb_mask_file =
                get_widget_checked("rb_mask_file");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(rb_mask_file),
                s->mask_file_is_checked);

            mask_entry =
                get_widget_checked("mask_entry");

            gtk_entry_set_text(GTK_ENTRY(mask_entry), s->mask_file);
        }
        else
        {
            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(mask_checkbutton), FALSE);
        }
    }
    else
    {
        GtkWidget *dem_entry;
        GtkWidget *dem_checkbutton;

        dem_entry = get_widget_checked("dem_entry");
        gtk_entry_set_text(GTK_ENTRY(dem_entry), "");

        dem_checkbutton = get_widget_checked("dem_checkbutton");
        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(dem_checkbutton), FALSE);
    }

    switch (s->keep_files) {
      case 0:
        rb_select("rb_keep_none", TRUE);
        rb_select("rb_keep_temp", FALSE);
        rb_select("rb_keep_all", FALSE);
        break;
      default:
      case 1:
        rb_select("rb_keep_none", FALSE);
        rb_select("rb_keep_temp", TRUE);
        rb_select("rb_keep_all", FALSE);
        break;
      case 2:
        rb_select("rb_keep_none", FALSE);
        rb_select("rb_keep_temp", FALSE);
        rb_select("rb_keep_all", TRUE);
        break;
    }
}

Settings *
settings_get_from_gui()
{
    GtkWidget
        *input_data_type_combobox,
        *output_format_combobox,
        *scaling_method_combobox;

    Settings *ret = (Settings *) g_malloc0 (sizeof(Settings));

    input_data_type_combobox =
        get_widget_checked("input_data_type_combobox");

    output_format_combobox =
        get_widget_checked("output_format_combobox");

    ret->data_type = get_combo_box_item(input_data_type_combobox);
    ret->output_format = get_combo_box_item(output_format_combobox);

    ret->airsar_p_pol = get_checked("airsar_p_pol_checkbutton");
    ret->airsar_l_pol = get_checked("airsar_l_pol_checkbutton");
    ret->airsar_c_pol = get_checked("airsar_c_pol_checkbutton");
    ret->airsar_c_vv = get_checked("airsar_c_vv_checkbutton");
    ret->airsar_l_vv = get_checked("airsar_l_vv_checkbutton");

    ret->output_db = get_checked("checkbutton_db") &&
        (ret->data_type == INPUT_TYPE_SIGMA ||
         ret->data_type == INPUT_TYPE_BETA ||
         ret->data_type == INPUT_TYPE_GAMMA);
    ret->do_calibrate = (ret->data_type == INPUT_TYPE_SIGMA ||
			 ret->data_type == INPUT_TYPE_BETA ||
			 ret->data_type == INPUT_TYPE_GAMMA);

    // this one is set on a per-file basis
    strcpy(ret->polsarpro_colormap, "");

    ret->process_to_level1 = 0; // Was set equal to the following expression
        //gtk_toggle_button_get_active(
            //GTK_TOGGLE_BUTTON(process_to_level1_checkbutton));

    ret->apply_scaling = FALSE;
    ret->latitude_low = -99;
    ret->latitude_hi = -99;

    if (settings_get_input_data_format_allows_latitude(ret) > 0)
    {
        ret->latitude_checked = get_checked("latitude_checkbutton");
        if (ret->latitude_checked)
        {
            ret->latitude_low = get_double_from_entry("latitude_low_entry");
            ret->latitude_hi = get_double_from_entry("latitude_hi_entry");
        }
    }

    ret->keep_files = 1;
    if (get_checked("rb_keep_none"))
      ret->keep_files = 0;
    else if (get_checked("rb_keep_all"))
      ret->keep_files = 2;

    ret->apply_metadata_fix = get_checked("apply_metadata_fix_checkbutton");
    ret->apply_ers2_gain_fix = get_checked("ers2_gain_fix_checkbutton");

    ret->external_is_checked = get_checked("external_checkbutton");
    if (ret->external_is_checked) {
      GtkWidget *external_optionmenu =
        get_widget_checked("external_optionmenu");
      ret->external_selected =
        gtk_option_menu_get_history(GTK_OPTION_MENU(external_optionmenu));
      strncpy_safe(ret->cmd, get_external_command_line(), 511);
    }
    else {
      ret->external_selected = 0;
      strcpy(ret->cmd, "");
    }

    ret->polarimetric_decomp_setting = POLARIMETRY_NONE;
    ret->do_farcorr = FALSE;
    if (get_checked("polarimetry_checkbutton")) {
      if (get_checked("polarimetric_decomp_checkbutton")) {
        if (get_checked("rb_pauli"))
          ret->polarimetric_decomp_setting = POLARIMETRY_PAULI;
        else if (get_checked("rb_sinclair"))
          ret->polarimetric_decomp_setting = POLARIMETRY_SINCLAIR;
        else if (get_checked("rb_cloude8"))
          ret->polarimetric_decomp_setting = POLARIMETRY_CLOUDE8;
        else if (get_checked("rb_cloude16"))
          ret->polarimetric_decomp_setting = POLARIMETRY_CLOUDE16;
        else if (get_checked("rb_cloude_noclassify"))
          ret->polarimetric_decomp_setting = POLARIMETRY_CLOUDE_NOCLASSIFY;
        else if (get_checked("rb_freeman_durden"))
          ret->polarimetric_decomp_setting = POLARIMETRY_FREEMAN_DURDEN;
        else
          ret->polarimetric_decomp_setting = POLARIMETRY_NONE;
      }
      if (get_checked("farcorr_checkbutton")) {
        ret->do_farcorr = TRUE;
        if (get_checked("rb_fr_local"))
          ret->farcorr_global_avg = FALSE;
        else if (get_checked("rb_fr_global"))
          ret->farcorr_global_avg = TRUE;
        else {
          // not possible
          assert(FALSE);
          ret->do_farcorr = FALSE;
        }
        double thresh = get_double_from_entry("farcorr_threshold_entry");
        if (thresh <= 0.0)
          ret->farcorr_threshold = -1;
        else
          ret->farcorr_threshold = thresh;
      }
    }

    ret->export_is_checked = get_checked("export_checkbutton");

    if (ret->export_is_checked)
    {
        if (settings_get_output_format_requires_byte(ret))
        {
            scaling_method_combobox =
                get_widget_checked("scaling_method_combobox");

            ret->output_bytes = TRUE;
            ret->scaling_method = get_combo_box_item(scaling_method_combobox);
        }
        else
        {
            ret->output_bytes = get_checked("output_bytes_checkbutton");

            if (ret->output_bytes)
            {
                scaling_method_combobox =
                    get_widget_checked("scaling_method_combobox");

                ret->scaling_method =
                    get_combo_box_item(scaling_method_combobox);
            }
        }

        GtkWidget * rb_rgb = get_widget_checked("rb_rgb");
        ret->export_bands = gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(rb_rgb));

        if (ret->export_bands) {
          GtkWidget *rb_user_defined =
            get_widget_checked("rb_user_defined");
          GtkWidget *rb_truecolor = get_widget_checked("rb_truecolor");
          GtkWidget *rb_falsecolor = get_widget_checked("rb_falsecolor");

          if (gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_user_defined)))
          {
            ret->user_defined_is_checked = 1;
            get_combo_box_entry_item("red_combo",ret->red);
            get_combo_box_entry_item("green_combo",ret->green);
            get_combo_box_entry_item("blue_combo",ret->blue);
          }
          else if (gtk_toggle_button_get_active(
                     GTK_TOGGLE_BUTTON(rb_truecolor)))
          {
            ret->truecolor_is_checked = 1;
            strcpy(ret->red, "3");
            strcpy(ret->green, "2");
            strcpy(ret->blue, "1");
          }
          else if (gtk_toggle_button_get_active(
                     GTK_TOGGLE_BUTTON(rb_falsecolor)))
          {
            ret->falsecolor_is_checked = 1;
            strcpy(ret->red, "4");
            strcpy(ret->green, "3");
            strcpy(ret->blue, "2");
          }
        }
        else {
            strcpy(ret->red, "");
            strcpy(ret->green, "");
            strcpy(ret->blue, "");
        }
    }

    ret->geocode_is_checked = get_checked("geocode_checkbutton");

    if (ret->geocode_is_checked)
    {
        GtkWidget
            *projection_option_menu,
            *datum_option_menu,
            *resample_option_menu;

        projection_option_menu =
            get_widget_checked("projection_option_menu");

        ret->projection =
            gtk_option_menu_get_history(
            GTK_OPTION_MENU(projection_option_menu));


        ret->zone = get_int_from_entry("utm_zone_entry");
        ret->lon0 = get_double_from_entry("central_meridian_entry");
        ret->lat0 = get_double_from_entry("latitude_of_origin_entry");
        ret->plat1 = get_double_from_entry("first_standard_parallel_entry");
        ret->plat2 = get_double_from_entry("second_standard_parallel_entry");
        ret->false_northing = get_double_from_entry("false_northing_entry");
        ret->false_easting = get_double_from_entry("false_easting_entry");

        ret->specified_height = get_checked("average_height_checkbutton");
        ret->specified_pixel_size = get_checked("pixel_size_checkbutton");

        if (ret->specified_height)
            ret->height = get_double_from_entry("average_height_entry");

        if (ret->specified_pixel_size)
            ret->pixel_size = get_double_from_entry("pixel_size_entry");

        datum_option_menu =
            get_widget_checked("datum_option_menu");

        ret->datum =
            gtk_option_menu_get_history(
              GTK_OPTION_MENU(datum_option_menu));

	char *spheroid_str = get_string_from_entry("spheroid_entry");
	if (strcmp_case(spheroid_str, "WGS84") == 0)
	  ret->spheroid = SPHEROID_WGS84;
	else if (strcmp_case(spheroid_str, "HUGHES") == 0)
	  ret->spheroid = SPHEROID_HUGHES;
	else if (strcmp_case(spheroid_str, "GRS1967") == 0)
	  ret->spheroid = SPHEROID_GRS1967;
	else if (strcmp_case(spheroid_str, "GRS1980") == 0)
	  ret->spheroid = SPHEROID_GRS1980;
	else if (strcmp_case(spheroid_str, "INTERNATIONAL1924") == 0)
	  ret->spheroid = SPHEROID_INTERNATIONAL1924;
	else
	  ret->spheroid = SPHEROID_UNKNOWN;

        resample_option_menu =
            get_widget_checked("resample_option_menu");

        ret->resample_method =
            gtk_option_menu_get_history(GTK_OPTION_MENU(resample_option_menu));

        ret->geocode_force = get_checked("force_checkbutton");
    }

    if (get_checked("dem_checkbutton"))
    {
        GtkWidget *rb_terrcorr;
        GtkWidget *dem_entry;
        GtkWidget *tc_pixel_size_checkbutton;

        rb_terrcorr =
            get_widget_checked("rb_terrcorr");
        ret->terrcorr_is_checked =
            gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(rb_terrcorr));

        dem_entry = get_widget_checked("dem_entry");
        strcpy(ret->dem_file, gtk_entry_get_text(GTK_ENTRY(dem_entry)));

        if (ret->terrcorr_is_checked)
        {
            tc_pixel_size_checkbutton =
                get_widget_checked("tc_pixel_size_checkbutton");

            ret->specified_tc_pixel_size =
                gtk_toggle_button_get_active(
                    GTK_TOGGLE_BUTTON(tc_pixel_size_checkbutton));

            if (ret->specified_tc_pixel_size)
            {
                ret->tc_pixel_size =
                    get_double_from_entry("tc_pixel_size_entry");
            }

            ret->no_matching = !get_checked("tc_matching_checkbutton");
            if (ret->no_matching)
            {
                ret->offset_x = get_double_from_entry("offset_x_entry");
                ret->offset_y = get_double_from_entry("offset_y_entry");
            }
            else
            {
                ret->offset_x = 0;
                ret->offset_y = 0;
            }

            ret->interp = get_checked("interpolate_checkbutton");
            ret->refine_geolocation_is_checked = FALSE;
            ret->generate_dem = get_checked("save_dem_checkbutton");
            ret->generate_layover_mask =
                get_checked("layover_mask_checkbutton");
            ret->do_radiometric = get_checked("radiometric_checkbutton");

            if(ret->do_radiometric)
                ret->save_incid_angles = get_checked("save_incid_angles_checkbutton"); 
        }
        else
        {
            ret->refine_geolocation_is_checked =
                get_checked("rb_refine_geolocation");

            // this should be checked if we aren't terrain correcting
            assert(ret->refine_geolocation_is_checked);
        }

        ret->interp_dem_holes = get_checked("interp_dem_holes_checkbutton");

        if (get_checked("mask_checkbutton"))
        {
            if (get_checked("rb_mask_file"))
            {
                GtkWidget *mask_entry = get_widget_checked("mask_entry");
                strcpy(ret->mask_file,
                       gtk_entry_get_text(GTK_ENTRY(mask_entry)));
                ret->mask_file_is_checked = TRUE;
                ret->auto_water_mask_is_checked = FALSE;
            }
            else    // auto water mask
            {
                ret->mask_file_is_checked = FALSE;
                ret->auto_water_mask_is_checked = TRUE;
                strcpy(ret->mask_file, "");
            }
        }
        else
        {
            ret->mask_file_is_checked = FALSE;
            ret->auto_water_mask_is_checked = FALSE;
            strcpy(ret->mask_file, "");
        }
    }
    else
    {
        ret->terrcorr_is_checked = FALSE;
        ret->refine_geolocation_is_checked = FALSE;
    }

    return ret;
}

const gchar *
settings_get_latitude_argument(const Settings *s)
{
    static gchar latitude_arg[128];

    if (settings_get_input_data_format_allows_latitude(s) > 0 &&
        s->latitude_checked)
    {
        g_snprintf(latitude_arg, sizeof(latitude_arg),
            "-lat %g %g", s->latitude_low, s->latitude_hi);
    }
    else
    {
        latitude_arg[0] = '\0';
    }

    return latitude_arg;
}

const gchar *
settings_get_apply_metadata_fix_argument(const Settings *s)
{
    return s->apply_metadata_fix ? "-fix-meta-ypix" : "";
}

const gchar *
settings_get_size_argument(const Settings *s)
{
    static gchar size_arg[32];

    if (s->apply_scaling)
    {
        GtkWidget *longest_dimension_spinbutton;
        gdouble d;

        longest_dimension_spinbutton =
            get_widget_checked("longest_dimension_spinbutton");

        d = gtk_spin_button_get_value(
            GTK_SPIN_BUTTON(longest_dimension_spinbutton));

        g_snprintf(size_arg, sizeof(size_arg),
            "-size %d", (int)floor(d + 0.5));
    }
    else
    {
        size_arg[0] = '\0';
    }

    return size_arg;
}

static const gchar * scaling_method_string(int scaling_method)
{
  switch (scaling_method)
  {
    default:
    case SCALING_METHOD_SIGMA:
      return "sigma";

    case SCALING_METHOD_MINMAX:
      return "minmax";

    case SCALING_METHOD_TRUNCATE:
      return "truncate";

    case SCALING_METHOD_HISTOGRAM_EQUALIZE:
      return "histogram_equalize";
  }
}

const gchar *
settings_get_output_bytes_argument(const Settings *s)
{
    static gchar byte_arg[64];

    if (s->output_bytes)
    {
        g_snprintf(byte_arg, sizeof(byte_arg),
            "-byte %s", scaling_method_string(s->scaling_method));
    }
    else
    {
        byte_arg[0] = '\0';
    }

    return byte_arg;
}

const gchar *
settings_get_data_type_arg_string(const Settings *s)
{
    static gchar buf[64];

    const gchar * type_arg = settings_get_data_type_string(s);

    if (strlen(type_arg) > 0)
    {
        strcpy(buf, "-");
        strcat(buf, type_arg);
    }
    else
    {
        strcpy(buf, "");
    }

    if (s->output_db)
    {
        strcat(buf, " -db");
    }

    return buf;
}

const gchar *
settings_get_data_type_string(const Settings *s)
{
    const gchar * ret;

    switch (s->data_type)
    {
    case INPUT_TYPE_SIGMA:
        ret = "sigma";
        break;

    case INPUT_TYPE_BETA:
        ret = "beta";
        break;

    case INPUT_TYPE_GAMMA:
        ret = "gamma";
        break;

    default:
    case INPUT_TYPE_AMP:
        ret = "amplitude";
        break;

    case INPUT_TYPE_POWER:
        ret = "power";
        break;
    }

    return ret;
}

const gchar *settings_get_db_string(const Settings *s)
{
  const gchar *ret;  

  if (s->output_db)
    ret = "_db";
  else
    ret = "";

  return ret;
}

int
settings_equal(const Settings *s1, const Settings *s2)
{
    gboolean equal = FALSE;

    assert(s1);
    assert(s2);

    /* settings_print(s1); */
    /* settings_print(s2); */

    if (s1->data_type == s2->data_type &&
        s1->output_db == s2->output_db &&
        s1->output_format == s2->output_format &&
        s1->keep_files == s2->keep_files &&
        s1->terrcorr_is_checked == s2->terrcorr_is_checked &&
        s1->apply_metadata_fix == s2->apply_metadata_fix &&
        s1->apply_ers2_gain_fix == s2->apply_ers2_gain_fix)
    {
        gchar * lat1 =
            g_strdup(settings_get_latitude_argument(s1));

        gchar * lat2 =
            g_strdup(settings_get_latitude_argument(s2));

        if (0 == strcmp(lat1, lat2))
        {
            gchar * siz1 =
                g_strdup(settings_get_size_argument(s1));

            gchar * siz2 =
                g_strdup(settings_get_size_argument(s2));

            if (0 == strcmp(siz1, siz2))
            {
                gchar * byt1 =
                    g_strdup(settings_get_output_bytes_argument(s1));

                gchar * byt2 =
                    g_strdup(settings_get_output_bytes_argument(s2));

                if (0 == strcmp(byt1, byt2))
                {
                    gchar * geo1 =
                        g_strdup(settings_get_geocode_options(s1));

                    gchar * geo2 =
                        g_strdup(settings_get_geocode_options(s2));

                    if (0 == strcmp(geo1, geo2))
                    {
                        if (s1->terrcorr_is_checked == s2->terrcorr_is_checked)
                        {
                            if (!s1->terrcorr_is_checked ||
                                strcmp(s1->dem_file, s2->dem_file) == 0)
                            {
                                equal = TRUE;
                            }
                        }
                    }

                    g_free(geo1);
                    g_free(geo2);
                }

                g_free(byt1);
                g_free(byt2);
            }

            g_free(siz1);
            g_free(siz2);
        }

        g_free(lat1);
        g_free(lat2);
    }

    /* printf("Equal = %s\n", equal ? "yes" : "no"); */
    return equal;
}

Settings *
settings_copy(const Settings *s)
{
    Settings * ret;

    ret = (Settings *)g_malloc0(sizeof(Settings));
    memcpy(ret, s, sizeof(Settings));

    assert(settings_equal(s, ret));

    return ret;
}

const gchar *
settings_get_output_format_extension(const Settings *s)
{
    const gchar * out_extension;

    if (s->export_is_checked)
    {
        switch (s->output_format)
        {
            case OUTPUT_FORMAT_ASF_INTERNAL:
                out_extension = "";
                break;

            case OUTPUT_FORMAT_CEOS:
                out_extension = "D";
                break;

            default:
            case OUTPUT_FORMAT_JPEG:
                out_extension = "jpg";
                break;

            case OUTPUT_FORMAT_PNG:
                out_extension = "png";
                break;

            case OUTPUT_FORMAT_PGM:
                out_extension = "pgm";
                break;

            case OUTPUT_FORMAT_GEOTIFF:
            case OUTPUT_FORMAT_TIFF:
                out_extension = "tif";
                break;

	    case OUTPUT_FORMAT_POLSARPRO:
	        out_extension = "bin";
		break;
        }
    }
    else
    {
        out_extension = "img";
    }

    return out_extension;
}

const gchar *
settings_get_output_format_string(const Settings *s)
{
    const gchar * format_arg_to_export;
    switch (s->output_format)
    {
    case OUTPUT_FORMAT_ASF_INTERNAL:
        format_arg_to_export = "";
        break;

    case OUTPUT_FORMAT_CEOS:
        format_arg_to_export = "ceos";
        break;

    case OUTPUT_FORMAT_JPEG:
    default:
        format_arg_to_export = "jpeg";
        break;

    case OUTPUT_FORMAT_PNG:
        format_arg_to_export = "png";
        break;

    case OUTPUT_FORMAT_PGM:
        format_arg_to_export = "pgm";
        break;

    case OUTPUT_FORMAT_GEOTIFF:
        format_arg_to_export = "geotiff";
        break;

    case OUTPUT_FORMAT_TIFF:
        format_arg_to_export = "tiff";
        break;

    case OUTPUT_FORMAT_POLSARPRO:
        format_arg_to_export = "polsarpro";
	break;
    }

    return format_arg_to_export;
}

const gchar *
settings_get_geocode_options(const Settings *s)
{
    return geocode_options_string(s);
}

const gchar *
settings_get_terrcorr_options(const Settings *s)
{
    return terrcorr_options_string(s);
}

const gchar *
settings_get_projection_abbrev(const Settings *s)
{
    switch(s->projection)
    {
    default:
        return "";
    case PROJ_UTM:
        return "utm";
    case PROJ_PS:
        return "ps";
    case PROJ_LAMCC:
        return "lamcc";
    case PROJ_LAMAZ:
        return "lamaz";
    case PROJ_ALBERS:
        return "albers";
    }
}

int
settings_get_run_geocode(const Settings *s)
{
    return s->geocode_is_checked;
}

int
settings_get_run_terrcorr(const Settings *s)
{
    return s->terrcorr_is_checked;
}

int
settings_get_run_import(const Settings *s)
{
    return s->output_format != OUTPUT_FORMAT_ASF_INTERNAL;
}

int
settings_get_run_export(const Settings *s)
{
    return s->export_is_checked;
}

int
settings_get_output_format_can_be_thumbnailed(const Settings *s)
{
    return s->output_format == OUTPUT_FORMAT_JPEG ||
        s->output_format == OUTPUT_FORMAT_TIFF ||
        s->output_format == OUTPUT_FORMAT_PNG ||
        s->output_format == OUTPUT_FORMAT_PGM;
}

void
settings_delete(Settings * s)
{
    g_free(s);
}

/* Returns TRUE if the filename has a TIFF extension */
static int has_tiff_ext(const char *f)
{
    char *ext = findExt(f);
    if (!ext) return FALSE;

    return
        strcmp(ext, ".tif") == 0 ||
        strcmp(ext, ".tiff") == 0 ||
        strcmp(ext, ".TIF") == 0 ||
        strcmp(ext, ".TIFF") == 0;
}

void
settings_update_dem(Settings *s, const char *output_path)
{
    // If this is the second or later file in a list of files,
    // AND the user has specified a .TIFF dem ...
    if ((s->terrcorr_is_checked || s->refine_geolocation_is_checked) &&
        has_tiff_ext(s->dem_file))
    {
        // ... we point to the saved DEM instead of the TIFF
        char *file = get_basename(s->dem_file);
        // note that the file name here is hard-coded to match what is
        // set up in libasf_convert's convert_tiff() code.
        sprintf(s->dem_file, "%s/geocoded_dem_%s.img", output_path, file);
        free(file);

        s->dem_was_generated = TRUE;
    }
}

void settings_delete_dem_and_mask(Settings *s)
{
    if (s->dem_was_generated && !s->keep_files) {
        printf("Removing generated DEM: %s\n", s->dem_file);
        remove_file(s->dem_file);
    }

    if (s->mask_was_generated && !s->keep_files) {
        printf("Removing generated MASK: %s\n", s->mask_file);
        remove_file(s->mask_file);
    }
}

void
settings_update_mask(Settings *s, const char *output_path)
{
    // If this is the second or later file in a list of files,
    // AND the user has specified a .TIFF Mask ...
    if ((s->terrcorr_is_checked || s->refine_geolocation_is_checked) &&
        has_tiff_ext(s->mask_file))
    {
        // ... we point to the saved mask instead of the TIFF
        char *file = get_basename(s->mask_file);
        // note that the file name here is hard-coded to match what is
        // set up in libasf_convert's convert_tiff() code.
        sprintf(s->mask_file, "%s/geocoded_mask_%s.img", output_path, file);
        free(file);

        s->mask_was_generated = TRUE;
    }
}

static int get_input_data_format(const char *infile)
{
  if (is_geotiff(infile)) {
    return INPUT_FORMAT_GEOTIFF;
  }
  else if (is_asf_internal(infile)) {
    return INPUT_FORMAT_ASF_INTERNAL;
  }
  else if (is_airsar(infile)) {
    return INPUT_FORMAT_AIRSAR;
  }
  else if (is_terrasarx(infile)) {
    return INPUT_FORMAT_TERRASARX;
  }
  else if (is_radarsat2(infile)) {
    return INPUT_FORMAT_RADARSAT2;
  }
  else if (is_roipac(infile)) {
    return INPUT_FORMAT_ROIPAC;
  }
  else if (is_polsarpro(infile)) {
    return INPUT_FORMAT_POLSARPRO;
  }
  else if (is_alos_mosaic(infile)) {
    return INPUT_FORMAT_ALOS_MOSAIC;
  }
  else {
    // catch-all...
    return INPUT_FORMAT_CEOS_LEVEL1;
  }
}

static const gchar *
get_input_data_format_string(int input_data_format)
{
    const gchar * format_arg_to_import;
    switch (input_data_format)
    {
      default:
      case INPUT_FORMAT_CEOS_LEVEL1:
        format_arg_to_import = "ceos";
        break;

      case INPUT_FORMAT_ASF_INTERNAL:
        format_arg_to_import = "asf";
        break;

      case INPUT_FORMAT_GEOTIFF:
        format_arg_to_import = "geotiff";
        break;

      case INPUT_FORMAT_AIRSAR:
        format_arg_to_import = "airsar";
        break;

      case INPUT_FORMAT_POLSARPRO:
        format_arg_to_import = "polsarpro";
        break;

      case INPUT_FORMAT_TERRASARX:
        format_arg_to_import = "terrasarx";
        break;

      case INPUT_FORMAT_RADARSAT2:
        format_arg_to_import = "radarsat2";
	break;

      case INPUT_FORMAT_ALOS_MOSAIC:
	format_arg_to_import = "alos_mosaic";
	break;

      case INPUT_FORMAT_GAMMA:
        format_arg_to_import = "gamma";
        break;

      case INPUT_FORMAT_ROIPAC:
        format_arg_to_import = "roipac";
        break;
    }

    return format_arg_to_import;
}

char *
settings_to_config_file(const Settings *s,
			const gchar *input_file, 
			const gchar *ancillary_file,
			const gchar *meta_file, 
			const gchar *output_full,
			const gchar *output_path, 
			const gchar *tmp_dir,
			const gchar *polsarpro_aux_info,\
			const gchar *interferogram,
			const gchar *coherence,
			const gchar *slave_metadata,
			const gchar *baseline)
{
    char *tmp_projfile = NULL;
    char *tmp_cfgfile;
    char *tmp_statfile;
    char *output_file;
    char *output_basename;
    char *input_basename;
    char *lut_basename = extract_lut_name(polsarpro_aux_info);
    int image_data_type = extract_image_data_type(polsarpro_aux_info);
    int input_data_format;

    if (strlen(meta_file)>0)
      input_data_format = INPUT_FORMAT_GAMMA;
    else
      input_data_format = get_input_data_format(input_file);

    if (s->export_is_checked) {
        output_file = STRDUP(output_full);
    } else {
        output_file = stripExt(output_full);
    }

    output_basename = get_basename(output_file);

    char *base = get_basename(input_file);
    char *path = g_path_get_dirname(input_file);
    input_basename = MALLOC(sizeof(char)*(strlen(base)+strlen(path)+32));

    // handle prepensions in the input filename
    int prepension = has_prepension(input_file);
    if (prepension > 0)
        sprintf(input_basename, "%s/%s", path, base+prepension);
    else
        sprintf(input_basename, "%s/%s", path, base);

    FREE(base);
    g_free(path);

    if (s->geocode_is_checked) {

      tmp_projfile =
          MALLOC(sizeof(char)*(9 + strlen(output_basename) + strlen(tmp_dir)));
      sprintf(tmp_projfile, "%s/%s.proj", tmp_dir, output_basename);

      FILE * pf = fopen(tmp_projfile, "wt");
      if (!pf) return NULL; /* FIXME, need better error handling here */

      switch (s->projection)
      {
        case PROJ_UTM:
          fprintf(pf, "[Universal Transverse Mercator]\n");
          fprintf(pf, "Zone=%d\n", s->zone != 0 ? s->zone : 0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
          break;

        case PROJ_PS:
          {
            fprintf(pf, "[Polar Stereographic]\n");
            fprintf(pf, "First Standard Parallel=%.10f\n", s->plat1);
            fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
            fprintf(pf, "Northern Projection=%d\n", s->lat0>0 || s->plat1>0);
	    fprintf(pf, "Datum=%s\n", datum_string(s->datum));
            fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
          }
          break;

        case PROJ_ALBERS:
          fprintf(pf, "[Albers Conical Equal Area]\n");
          fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
          fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
          fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
          fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	  fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
          break;

        case PROJ_LAMAZ:
          fprintf(pf, "[Lambert Azimuthal Equal Area]\n");
          fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
          fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	  fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
          break;

        case PROJ_LAMCC:
          fprintf(pf, "[Lambert Conformal Conic]\n");
          fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
          fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
          fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
          fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	  fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
          break;

        case PROJ_MER:
	  fprintf(pf, "[Mercator]\n");
	  fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
	  fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
	  fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
	  fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	  fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
	  break;

        case PROJ_EQR:
	  fprintf(pf, "[Equirectangular]\n");
	  fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
	  fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
	  fprintf(pf, "Datum=%s\n", datum_string(s->datum));
	  fprintf(pf, "Spheroid=%s\n", spheroid_string(s->spheroid));
	  break;

        default:
          break;
      }
      fclose(pf);
    }

    tmp_cfgfile = MALLOC(sizeof(char)*(128 + strlen(tmp_dir)));
    sprintf(tmp_cfgfile, "%s%c%s.cfg", tmp_dir, DIR_SEPARATOR, "mapready");
    tmp_statfile = appendExt(tmp_cfgfile, ".status");

    FILE * cf = fopen(tmp_cfgfile, "w");
    if (!cf) {
      printf("Failed to open config file: %s\n", tmp_cfgfile);
      printf("Reason: %s\n", strerror(errno));
      return NULL; /* FIXME, need better error handling here */
    }

    fprintf(cf, "Temporary config file, generated by the ASF MapReady Tool\n");
    fprintf(cf, "File was generated on: %s\n\n", date_time_stamp());

    fprintf(cf, "[General]\n");
    // must strip .img for asf internal
    if (input_data_format == INPUT_FORMAT_ASF_INTERNAL) {
        char *tmp = stripExt(input_basename);
        fprintf(cf, "input file = %s\n", tmp);
        free(tmp);
    }
    // must strip _meta.airsar for airsar
    else if (input_data_format == INPUT_FORMAT_AIRSAR) {
        char *tmp = STRDUP(input_file);
        char *p = strstr(tmp, "_meta.airsar");
        if (p) *p = '\0';
        fprintf(cf, "input file = %s\n", tmp);
        free(tmp);
    }
    // must extract the directory name
    else if (image_data_type == SELECT_POLARIMETRIC_MATRIX){
      char *dirName = (char *) MALLOC(sizeof(char)*1024);
      char *fileName = (char *) MALLOC(sizeof(char)*1024);
      split_dir_and_file(input_file, dirName, fileName);
      dirName[strlen(dirName)-1] = '\0';
      fprintf(cf, "input file = %s\n", dirName);
      free(dirName);
      free(fileName);
    }
    else {
        fprintf(cf, "input file = %s\n", input_basename);
    }
    if (ancillary_file && strlen(ancillary_file)) {
      fprintf(cf, "ancillary file = %s\n", ancillary_file);
    }
    fprintf(cf, "output file = %s\n", output_file);
    fprintf(cf, "project = 0\n");
    fprintf(cf, "files = 0\n");
    fprintf(cf, "import = %d\n",
        input_data_format == INPUT_FORMAT_ASF_INTERNAL ? 0 : 1);
    fprintf(cf, "external = %d\n", s->external_is_checked);
    fprintf(cf, "sar processing = %d\n", s->process_to_level1);
    // fprintf(cf, "image stats=0\n");
    // fprintf(cf, "detect corner reflectors = 0\n");
    int polarimetry_on =
      s->polarimetric_decomp_setting != POLARIMETRY_NONE || s->do_farcorr;
    fprintf(cf, "polarimetry = %d\n", polarimetry_on ? 1 : 0);
    fprintf(cf, "terrain correction = %d\n",
            s->terrcorr_is_checked || s->refine_geolocation_is_checked);
    fprintf(cf, "calibration = %d\n", s->do_calibrate);
    fprintf(cf, "geocoding = %d\n", s->geocode_is_checked);
    fprintf(cf, "export = %d\n", s->export_is_checked);
    fprintf(cf, "intermediates = 1\n");
    fprintf(cf, "status file = %s\n", tmp_statfile);
    fprintf(cf, "short configuration file = 0\n");
    FILE *fpDefs = fopen_share_file("asf_mapready/asf_mapready.defaults", "rt");
    if (fpDefs) {
        fprintf(cf, "default values = %s/%s\n", get_asf_share_dir(),
                "asf_mapready/asf_mapready.defaults");
        FCLOSE(fpDefs);
    }
    fprintf(cf, "tmp dir = %s\n", tmp_dir);
    fprintf(cf, "thumbnail = %d\n",
            (input_data_format == INPUT_FORMAT_CEOS_LEVEL1  ||
             input_data_format == INPUT_FORMAT_AIRSAR       ||
             input_data_format == INPUT_FORMAT_ASF_INTERNAL ||
             input_data_format == INPUT_FORMAT_GAMMA        ||
             input_data_format == INPUT_FORMAT_TERRASARX    ||
	     input_data_format == INPUT_FORMAT_RADARSAT2    ||
             input_data_format == INPUT_FORMAT_POLSARPRO) ? 1 : 0);
    fprintf(cf, "\n");

    fprintf(cf, "[Import]\n");
    if (input_data_format == INPUT_FORMAT_CEOS_LEVEL1)
        fprintf(cf, "format = CEOS (1)\n");
    //else if (input_data_format == INPUT_FORMAT_CEOS_LEVEL0)
        //fprintf(cf, "format = CEOS (0)\n");
    else
        fprintf(cf, "format = %s\n",
                get_input_data_format_string(input_data_format));

    if (input_data_format == INPUT_FORMAT_CEOS_LEVEL1)
        fprintf(cf, "radiometry = %s_image\n",
                settings_get_data_type_string(s));
    // fprintf(cf, "look up table = \n");
    if (settings_get_input_data_format_allows_latitude(s) > 0) {
      fprintf(cf, "lat begin = %.2f\n", s->latitude_low);
      fprintf(cf, "lat end = %.2f\n", s->latitude_hi);
    }
    // fprintf(cf, "precise =\n");
    fprintf(cf, "output db = %d\n", s->output_db);
    // can't currently handle complex, L0 data in the envi dump
    if (input_data_format != INPUT_FORMAT_CEOS_LEVEL1 &&
        input_data_format != INPUT_FORMAT_ASF_INTERNAL)
        fprintf(cf, "dump envi header = 0\n");

    // the "multilook SLC" option is ignored on non-SLC data
    // we multilook on import if we are going to be geocoding,
    // though we don't multilook if we going to be doing a polarimetric
    // decomposition, since in that case we'll multilook as we do Pauli, etc
    // Also, if we are going to be doing Faraday Rotation correction, then
    // we never multilook on import -- user will get non-multilooked data
    // if they JUST do FR correction.  (which is ok)
    int multilook_on_import = s->geocode_is_checked;
    if (s->polarimetric_decomp_setting != POLARIMETRY_NONE || s->do_farcorr)
      multilook_on_import = FALSE;

    fprintf(cf, "multilook SLC = %d\n", multilook_on_import ? 1 : 0);
    fprintf(cf, "apply ers2 gain fix = %d\n", s->apply_ers2_gain_fix);
    if (input_data_format == INPUT_FORMAT_POLSARPRO) {
      if (strlen(lut_basename)>0 &&
          strcmp_case(lut_basename,"none")!=0)
        fprintf(cf, "polsarpro colormap = %s\n", lut_basename);
      switch (image_data_type)
	{
	case SELECT_POLARIMETRIC_SEGMENTATION:
	  fprintf(cf, "image data type = POLARIMETRIC_SEGMENTATION\n");
	  break;
	case SELECT_POLARIMETRIC_DECOMPOSITION:
	  fprintf(cf, "image data type = POLARIMETRIC_DECOMPOSITION\n");
	  break;
	case SELECT_POLARIMETRIC_PARAMETER:
	  fprintf(cf, "image data type = POLARIMETRIC_PARAMETER\n");
	  break;
	case SELECT_POLARIMETRIC_MATRIX:
	  fprintf(cf, "image data type = POLARIMETRIC_MATRIX\n");
	  break;
	}
    }
    if (meta_file && strlen(meta_file)>0) {
      fprintf(cf, "metadata file = %s\n", meta_file);
    }
    if (interferogram && strlen(interferogram) > 0)
      fprintf(cf, "interferogram = %s\n", interferogram);
    if (coherence && strlen(coherence) > 0)
      fprintf(cf, "coherence = %s\n", coherence);
    if (slave_metadata && strlen(slave_metadata) > 0)
      fprintf(cf, "slave metadata = %s\n", slave_metadata);
    if (baseline && strlen(baseline) > 0)
      fprintf(cf, "baseline = %s\n", baseline);
    fprintf(cf, "\n");

    if (input_data_format == INPUT_FORMAT_AIRSAR) {
        fprintf(cf, "[AirSAR]\n");
        fprintf(cf, "airsar c interferometric = %d\n", s->airsar_c_vv);
        fprintf(cf, "airsar l interferometric = %d\n", s->airsar_l_vv);
        fprintf(cf, "airsar c polarimetric = %d\n", s->airsar_c_pol);
        fprintf(cf, "airsar l polarimetric = %d\n", s->airsar_l_pol);
        fprintf(cf, "airsar p polarimetric = %d\n", s->airsar_p_pol);
        fprintf(cf, "\n");
    }

    if (s->external_is_checked) {
        fprintf(cf, "[External]\n");
        fprintf(cf, "command = %s\n", s->cmd);
        fprintf(cf, "\n");
    }

    if (s->process_to_level1) {
        fprintf(cf, "[SAR processing]\n");
        fprintf(cf, "radiometry = %s_image\n",
                settings_get_data_type_string(s));
        fprintf(cf, "\n");
    }

    if (polarimetry_on) {
        fprintf(cf, "[Polarimetry]\n");
        fprintf(cf, "pauli = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_PAULI?1:0);
        fprintf(cf, "sinclair = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_SINCLAIR?1:0);
        fprintf(cf, "cloude pottier = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE8?1:0);
        fprintf(cf, "extended cloude pottier = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE16?1:0);
        fprintf(cf, "entropy anisotropy alpha = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE_NOCLASSIFY?1:0);
        fprintf(cf, "freeman durden = %d\n",
            s->polarimetric_decomp_setting==POLARIMETRY_FREEMAN_DURDEN?1:0);

        int farcorr_code = FARCORR_OFF;
        if (s->do_farcorr)
          farcorr_code = s->farcorr_global_avg ? FARCORR_MEAN : FARCORR_SMOOTH;
        fprintf(cf, "faraday correction = %d\n", farcorr_code);
        fprintf(cf, "farcorr threshold = %f\n", s->farcorr_threshold);
        fprintf(cf, "\n");
    }

    if (s->terrcorr_is_checked || s->refine_geolocation_is_checked) {
        // Terrain correction section
        fprintf(cf, "[Terrain correction]\n");
        fprintf(cf, "digital elevation model = %s\n", s->dem_file);

        // items specific to either terrain correction or refine geolocation
        if (s->terrcorr_is_checked) {
            if (s->specified_tc_pixel_size)
                fprintf(cf, "pixel spacing = %.2lf\n", s->tc_pixel_size);
	    // Apparently applying the geocoding pixel size caused some
	    // issues with some of InSAR products. Will comment out the
	    // automatic assignment of the pixel size defined in geocoding
	    // section to the terrain correction.
	    /*
            else if (s->specified_pixel_size) // geocode pixel size
                fprintf(cf, "pixel spacing = %.2lf\n", s->pixel_size);
	    */
            fprintf(cf, "refine geolocation only = 0\n");
            fprintf(cf, "interpolate = %d\n", s->interp);
            fprintf(cf, "do radiometric = %d\n", s->do_radiometric);
            fprintf(cf, "save incidence angles = %d\n", s->save_incid_angles);
            fprintf(cf, "save terrcorr dem = %d\n", s->generate_dem);
            fprintf(cf, "save terrcorr layover mask = %d\n",
                    s->generate_layover_mask);

            // for now, we don't support specifying offsets via the GUI
            if (s->no_matching) {
                fprintf(cf, "no matching = 1\n");
            }
            // for now, the "no matching" checkbutton will be "skip matching"
            // if it fails
            //if (s->no_matching) {
            //  fprintf(cf, "no matching = 1\n");
            //  fprintf(cf, "range offset = %f\n", s->offset_x);
            //  fprintf(cf, "azimuth offset = %f\n", s->offset_y);
            //}
            //else {
            //  fprintf(cf, "no matching = 0\n");
            //}

            // "Skip coregistration if it fails" not supported via GUI
            //fprintf(cf, "use zero offsets if match fails = %d\n",
            //        s->no_matching);

            // maybe we should use "1" for quad-pol??
            //   commenting this out ... let users set a default in
            //   asf_mapready.defaults
            // fprintf(cf, "use gr dem = 0\n");
        } else if (s->refine_geolocation_is_checked) {
            fprintf(cf, "refine geolocation only = 1\n");
        }

        // terrain correction continued... stuff that applies to both
        fprintf(cf, "auto mask water = %d\n", s->auto_water_mask_is_checked);
        if (s->mask_file_is_checked) {
            fprintf(cf, "mask = %s\n", s->mask_file);
        }
        fprintf(cf, "smooth dem holes = %d\n", s->interp_dem_holes);
        fprintf(cf, "\n");
    }

    if (s->do_calibrate) {
      fprintf(cf, "[Calibration]\n");
      fprintf(cf, "radiometry = %s%s\n", 
	      settings_get_data_type_string(s), settings_get_db_string(s));
      fprintf(cf, "woods hole scale = 0\n\n");
    }

    if (s->geocode_is_checked) {
      fprintf(cf, "[Geocoding]\n");
      fprintf(cf, "projection = %s\n", tmp_projfile);
      if (s->specified_pixel_size)
        fprintf(cf, "pixel spacing = %.2f\n", s->pixel_size);
      if (s->specified_height)
        fprintf(cf, "height = %.2f\n", s->height);
      // The user-selected datum is written to the temporary proj file above,
      // but we also write it here, to avoid an annoying warning in
      // asf_mapready.
      fprintf(cf, "datum = %s\n", datum_string(s->datum));
      fprintf(cf, "spheroid = %s\n", spheroid_string(s->spheroid));
      // Always write nearest neighbor for Cloude-Pottier...
      // Actually... let's not do this, instead we do it in command-line...
      //if (s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE8 ||
      //    s->polarimetric_decomp_setting==POLARIMETRY_CLOUDE16) {
      //  fprintf(cf, "resampling = %s\n",
      //          resample_method_string(RESAMPLE_NEAREST_NEIGHBOR));
      //}
      //else {
        fprintf(cf, "resampling = %s\n",
                resample_method_string(s->resample_method));
      //}
      fprintf(cf, "force = %d\n", s->geocode_force);
      fprintf(cf, "\n");
    }

    if (s->export_is_checked) {
      fprintf(cf, "[Export]\n");
      fprintf(cf, "format = %s\n", settings_get_output_format_string(s));
      if (s->truecolor_is_checked || s->falsecolor_is_checked) {
          fprintf(cf, "byte conversion = sigma\n");
      }
      else if (s->output_bytes) {
          fprintf(cf, "byte conversion = %s\n",
                  scaling_method_string(s->scaling_method));
      }
      else if (s->polarimetric_decomp_setting == POLARIMETRY_CLOUDE8 ||
               s->polarimetric_decomp_setting == POLARIMETRY_CLOUDE16) {
          fprintf(cf, "byte conversion = truncate\n");
      }
      else {
          fprintf(cf, "byte conversion = none\n");
      }
      if (polarimetry_on &&
          s->polarimetric_decomp_setting == POLARIMETRY_CLOUDE8)
      {
        fprintf(cf,"rgb look up table = cloude8\n");
      }
      else if (polarimetry_on &&
               s->polarimetric_decomp_setting == POLARIMETRY_CLOUDE16)
      {
        fprintf(cf,"rgb look up table = cloude16\n");
      }
      else if (s->export_bands || (polarimetry_on &&
               (s->polarimetric_decomp_setting == POLARIMETRY_PAULI ||
                s->polarimetric_decomp_setting == POLARIMETRY_SINCLAIR ||
                s->polarimetric_decomp_setting == POLARIMETRY_FREEMAN_DURDEN)))
      {
        if (polarimetry_on &&
            s->polarimetric_decomp_setting == POLARIMETRY_PAULI)
        {
          fprintf(cf, "rgb banding = HH-VV,HV+VH,HH+VV\n");
        }
        else if (polarimetry_on &&
                 s->polarimetric_decomp_setting == POLARIMETRY_SINCLAIR)
        {
          fprintf(cf, "rgb banding = HH,HV+VH_2,VV\n");
        }
        else if (polarimetry_on &&
                 s->polarimetric_decomp_setting == POLARIMETRY_FREEMAN_DURDEN)
        {
          fprintf(cf, "rgb banding = Pd,Pv,Ps\n");
        }
        else if (!s->truecolor_is_checked && !s->falsecolor_is_checked)
        {
          const char *r =
            strlen(s->red)>0 && strcmp(s->red,"-")!=0 ? s->red : "ignore";
          const char *g =
            strlen(s->green)>0 && strcmp(s->green,"-")!=0 ? s->green : "ignore";
          const char *b =
            strlen(s->blue)>0 && strcmp(s->blue,"-")!=0 ? s->blue : "ignore";
          fprintf(cf, "rgb banding = %s,", r);
          fprintf(cf, "%s,", g);
          fprintf(cf, "%s\n", b);
        }
        else {
          fprintf(cf, "rgb banding = \n");
        }
        fprintf(cf, "truecolor = %d\n", s->truecolor_is_checked ? 1 : 0);
        fprintf(cf, "falsecolor = %d\n", s->falsecolor_is_checked ? 1 : 0);
      }
      fprintf(cf, "\n");
    }

    fclose(cf);
    free(tmp_statfile);
    if (tmp_projfile)
      free(tmp_projfile);
    free(output_file);
    free(output_basename);
    free(input_basename);
    return tmp_cfgfile;
}

int apply_settings_from_config_file(char *configFile)
{
    convert_config *cfg = read_convert_config(configFile);
    if (!cfg) return 1;

    Settings s;

    s.airsar_c_vv = cfg->airsar->c_vv;
    s.airsar_l_vv = cfg->airsar->l_vv;
    s.airsar_c_pol = cfg->airsar->c_pol;
    s.airsar_l_pol = cfg->airsar->l_pol;
    s.airsar_p_pol = cfg->airsar->p_pol;

    s.data_type = INPUT_TYPE_AMP;
    s.do_calibrate = 0;
    if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0)
        s.data_type = INPUT_TYPE_AMP;
    else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0)
        s.data_type = INPUT_TYPE_POWER;
    else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0) {
        s.data_type = INPUT_TYPE_SIGMA;
	s.do_calibrate = 1;
    }
    else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0) {
        s.data_type = INPUT_TYPE_GAMMA;
	s.do_calibrate = 1;
    }
    else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0) {
        s.data_type = INPUT_TYPE_BETA;
	s.do_calibrate = 1;
    }

    s.process_to_level1 = cfg->general->sar_processing;
    s.output_db = cfg->import->output_db;
    s.latitude_checked = 0; //s.input_data_format == INPUT_FORMAT_STF &&
        //(cfg->import->lat_begin != -99 || cfg->import->lat_end != -99);
    s.latitude_low = -99; //cfg->import->lat_begin;
    s.latitude_hi = -99; //cfg->import->lat_end;
    s.apply_ers2_gain_fix = cfg->import->ers2_gain_fix;

    /* external */
    // this one is a little weird... we populate right to the textboxes,
    // somewhat breaking our file->settings->gui (going directly file->gui)
    // we have to open the file up again...
    if (cfg->general->external) {
      s.external_is_checked = 1;
      FILE *fp = FOPEN(configFile, "r");
      if (fp) {
        char line[1025];
        while (fgets(line, 1024, fp) != NULL) {
          if (strncmp_case(line, "# external selected", 19)==0) {
            char *p = line+19;
            while (*p==' ' || *p=='=')
              ++p;
            s.external_selected = atoi(p);
          }
          else if (strncmp_case(line, "# external params", 17)==0) {
            char *p = line+17;
            while (*p==' ' || *p=='=')
              ++p;
            populate_external_params_from_csv(p);
            break;
          }
        }
        fclose(fp);
      }
    }

    /* polarimetry */
    s.polarimetric_decomp_setting = POLARIMETRY_NONE;
    s.do_farcorr = FALSE;
    s.farcorr_threshold = -1;

    if (cfg->general->polarimetry) {
      if (cfg->polarimetry->pauli)
        s.polarimetric_decomp_setting = POLARIMETRY_PAULI;
      else if (cfg->polarimetry->sinclair)
        s.polarimetric_decomp_setting = POLARIMETRY_SINCLAIR;
      else if (cfg->polarimetry->cloude_pottier)
        s.polarimetric_decomp_setting = POLARIMETRY_CLOUDE8;
      else if (cfg->polarimetry->cloude_pottier_ext)
        s.polarimetric_decomp_setting = POLARIMETRY_CLOUDE16;
      else if (cfg->polarimetry->cloude_pottier_nc)
        s.polarimetric_decomp_setting = POLARIMETRY_CLOUDE_NOCLASSIFY;
      else if (cfg->polarimetry->freeman_durden)
        s.polarimetric_decomp_setting = POLARIMETRY_FREEMAN_DURDEN;

      if (cfg->polarimetry->farcorr != FARCORR_OFF) {
        s.do_farcorr = TRUE;
        if (cfg->polarimetry->farcorr == FARCORR_MEAN)
          s.farcorr_global_avg = TRUE;
        else
          s.farcorr_global_avg = FALSE;
        s.farcorr_threshold = cfg->polarimetry->farcorr_threshold;
      }
    }

    /* export */
    s.export_is_checked = cfg->general->export;

    if (strncmp(uc(cfg->export->format), "TIFF", 4) == 0)
        s.output_format = OUTPUT_FORMAT_TIFF;
    else if (strncmp(uc(cfg->export->format), "GEOTIFF", 7) == 0)
        s.output_format = OUTPUT_FORMAT_GEOTIFF;
    else if (strncmp(uc(cfg->export->format), "JPEG", 4) == 0)
        s.output_format = OUTPUT_FORMAT_JPEG;
    else if (strncmp(uc(cfg->export->format), "PGM", 3) == 0)
        s.output_format = OUTPUT_FORMAT_PGM;
    else if (strncmp(uc(cfg->export->format), "PNG", 3) == 0)
        s.output_format = OUTPUT_FORMAT_PNG;
    else if (strncmp(uc(cfg->export->format), "POLSARPRO", 9) == 0)
        s.output_format = OUTPUT_FORMAT_POLSARPRO;

    s.apply_scaling = 0;
    s.longest_dimension = 0;
    s.output_bytes = strlen(cfg->export->byte) > 0;

    s.scaling_method = SCALING_METHOD_SIGMA;
    if (strncmp(uc(cfg->export->byte), "TRUNCATE", 8) == 0)
        s.scaling_method = SCALING_METHOD_TRUNCATE;
    else if (strncmp(uc(cfg->export->byte), "MINMAX", 6) == 0)
        s.scaling_method = SCALING_METHOD_MINMAX;
    else if (strncmp(uc(cfg->export->byte), "SIGMA", 5) == 0)
        s.scaling_method = SCALING_METHOD_SIGMA;
    else if (strncmp(uc(cfg->export->byte), "HISTOGRAM_EQUALIZE", 18) == 0)
        s.scaling_method = SCALING_METHOD_HISTOGRAM_EQUALIZE;

    s.truecolor_is_checked = cfg->export->truecolor;
    s.falsecolor_is_checked = cfg->export->falsecolor;
    s.user_defined_is_checked = strlen(cfg->export->rgb) > 0 &&
        ! (s.truecolor_is_checked ||
           s.falsecolor_is_checked);
    s.export_bands = s.truecolor_is_checked ||
        s.falsecolor_is_checked ||
        s.user_defined_is_checked;

    if (strlen(cfg->export->rgb) > 0) {
        int i;
        for (i=0; i<10; ++i) {
            s.red[i] = s.green[i] = s.blue[i] = '\0';
        }
        char *red, *green, *blue;
        split3(cfg->export->rgb, &red, &green, &blue, ',');
        strncpy(s.red, red, 9);
        strncpy(s.green, green, 9);
        strncpy(s.blue, blue, 9);
        FREE(red);
        FREE(green);
        FREE(blue);
    }

    /* geocode */
    s.geocode_is_checked = cfg->general->geocoding && cfg->geocoding;
    s.projection = PROJ_UTM;
    s.plat1 = s.plat2 = s.lat0 = s.lon0 = 0;
    s.false_easting = s.false_northing = 0;

    if (s.geocode_is_checked) {
        project_parameters_t pps;
        projection_type_t type;
	datum_type_t datum;
	spheroid_type_t spheroid;
        read_proj_file(cfg->geocoding->projection, 
		       &pps, &type, &datum, &spheroid);
	printf("read_proj_file - datum: %s, spheroid: %s\n",
	       datum_toString(datum), spheroid_toString(spheroid));

        if (type == UNIVERSAL_TRANSVERSE_MERCATOR) {
            s.projection = PROJ_UTM;
            s.zone = pps.utm.zone;
        } else if (type == POLAR_STEREOGRAPHIC) {
            s.projection = PROJ_PS;
            s.lat0 = pps.ps.slat;
            s.lon0 = pps.ps.slon;
        } else if (type == ALBERS_EQUAL_AREA) {
            s.projection = PROJ_ALBERS;
            s.plat1 = pps.albers.std_parallel1;
            s.plat2 = pps.albers.std_parallel2;
            s.lat0 = pps.albers.orig_latitude;
            s.lon0 = pps.albers.center_meridian;
        } else if (type == LAMBERT_CONFORMAL_CONIC) {
            s.projection = PROJ_LAMCC;
            s.plat1 = pps.lamcc.plat1;
            s.plat2 = pps.lamcc.plat2;
            s.lat0 = pps.lamcc.lat0;
            s.lon0 = pps.lamcc.lon0;
        } else if (type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
            s.projection = PROJ_LAMAZ;
            s.lat0 = pps.lamaz.center_lat;
            s.lon0 = pps.lamaz.center_lon;
        } else if (type == MERCATOR) {
	  s.projection = PROJ_MER;
	  s.lat0 = pps.mer.orig_latitude;
	  s.lon0 = pps.mer.central_meridian;
	  s.plat1 = pps.mer.standard_parallel;
	} else if (type == EQUI_RECTANGULAR) {
	  s.projection = PROJ_EQR;
	  s.lat0 = pps.eqr.orig_latitude;
	  s.lon0 = pps.eqr.central_meridian;
	}

        s.specified_height = cfg->geocoding->height != -99 &&
            cfg->geocoding->height != 0;
        s.height = cfg->geocoding->height;
        s.specified_pixel_size = cfg->geocoding->pixel > 0;
        s.pixel_size = cfg->geocoding->pixel;
        s.geocode_force = cfg->geocoding->force;

        // GET DATUM
        s.datum = DATUM_WGS84;
        if (datum == WGS84_DATUM) {
          s.datum = DATUM_WGS84;
        }
        else if (datum == NAD27_DATUM) {
          s.datum = DATUM_NAD27;
        }
        else if (datum == NAD83_DATUM) {
          s.datum = DATUM_NAD83;
        }
        else if (datum == HUGHES_DATUM) {
          s.datum = DATUM_HUGHES;
        }
	else if (datum == ITRF97_DATUM) {
	  s.datum = DATUM_ITRF97;
	}
	else if (datum == ED50_DATUM) {
	  s.datum = DATUM_ED50;
	}
	else if (datum == SAD69_DATUM) {
	  s.datum = DATUM_SAD69;
	}

	if (spheroid == WGS84_SPHEROID) {
	  s.spheroid = SPHEROID_WGS84;
	}
	else if (spheroid == HUGHES_SPHEROID) {
	  s.spheroid = SPHEROID_HUGHES;
	}
	else if (spheroid == GRS1967_SPHEROID) {
	  s.spheroid = SPHEROID_GRS1967;
	}
	else if (spheroid == GRS1980_SPHEROID) {
	  s.spheroid = SPHEROID_GRS1980;
	}
	else if (spheroid == INTERNATIONAL1924_SPHEROID) {
	  s.spheroid = SPHEROID_INTERNATIONAL1924;
	}

        s.resample_method = RESAMPLE_BILINEAR;
        if (strncmp(uc(cfg->geocoding->resampling),"NEAREST_NEIGHBOR",16) == 0)
            s.resample_method = RESAMPLE_NEAREST_NEIGHBOR;
        if (strncmp(uc(cfg->geocoding->resampling),"BILINEAR", 8) == 0)
            s.resample_method = RESAMPLE_BILINEAR;
        if (strncmp(uc(cfg->geocoding->resampling),"BICUBIC", 7) == 0)
            s.resample_method = RESAMPLE_BICUBIC;
    }

    /* terrcorr options */
    s.terrcorr_is_checked = cfg->general->terrain_correct &&
        strlen(cfg->terrain_correct->dem) > 0;
    s.refine_geolocation_is_checked = cfg->general->terrain_correct &&
        strlen(cfg->terrain_correct->dem) > 0;

    strcpy(s.dem_file, "");
    strcpy(s.mask_file, "");

    if (s.terrcorr_is_checked) {
        s.refine_geolocation_is_checked =
            cfg->terrain_correct->refine_geolocation_only;
        s.terrcorr_is_checked = !s.refine_geolocation_is_checked;
        strcpy(s.dem_file, cfg->terrain_correct->dem);
        s.specified_tc_pixel_size = cfg->terrain_correct->pixel != -99;
        s.no_matching = cfg->terrain_correct->no_matching;
        if (s.no_matching) {
          s.offset_x = cfg->terrain_correct->range_offset;
          s.offset_y = cfg->terrain_correct->azimuth_offset;
        }
        s.tc_pixel_size = cfg->terrain_correct->pixel;
        s.interp = cfg->terrain_correct->interp;
        s.auto_water_mask_is_checked = cfg->terrain_correct->auto_mask_water;
        s.mask_file_is_checked = strlen(cfg->terrain_correct->mask) > 0;
        strcpy(s.mask_file, cfg->terrain_correct->mask);
        s.generate_layover_mask =
            cfg->terrain_correct->save_terrcorr_layover_mask;
        s.generate_dem = cfg->terrain_correct->save_terrcorr_dem;
        s.do_radiometric = cfg->terrain_correct->do_radiometric;
        if(s.do_radiometric)
          s.save_incid_angles = cfg->terrain_correct->save_incid_angles;
        s.interp_dem_holes = cfg->terrain_correct->smooth_dem_holes;
    }

    // calibration
    s.do_calibrate = cfg->general->calibration;
    if (cfg->general->calibration) {
      if (strncmp_case(cfg->calibrate->radiometry, "SIGMA_DB", 8) == 0) {
        s.data_type = INPUT_TYPE_SIGMA;	
	s.output_db = 1;
      }
      if (strncmp_case(cfg->calibrate->radiometry, "GAMMA_DB", 8) == 0) {
        s.data_type = INPUT_TYPE_GAMMA;	
	s.output_db = 1;
      }
      if (strncmp_case(cfg->calibrate->radiometry, "BETA_DB", 8) == 0) {
        s.data_type = INPUT_TYPE_BETA;	
	s.output_db = 1;
      }
      if (strncmp_case(cfg->calibrate->radiometry, "SIGMA", 5) == 0) {
        s.data_type = INPUT_TYPE_SIGMA;	
	s.output_db = 0;
      }
      if (strncmp_case(cfg->calibrate->radiometry, "GAMMA", 5) == 0) {
        s.data_type = INPUT_TYPE_GAMMA;	
	s.output_db = 0;
      }
      if (strncmp_case(cfg->calibrate->radiometry, "BETA", 4) == 0) {
        s.data_type = INPUT_TYPE_BETA;	
	s.output_db = 0;
      }
    }

    /* misc */
    s.keep_files = cfg->general->intermediates - 1;
    if (s.keep_files < 0) s.keep_files = 0;
    s.apply_metadata_fix = 1;

    settings_apply_to_gui(&s);

    if (cfg->general->in_name && strlen(cfg->general->in_name) > 0) {
      /* Files from the config file */
      GtkTreeIter iter;

      /* The config file contains the basename -- we must pass the actual
       * data file name (CEOS), or leader file name (ALOS) to add_to_files_list
       */
      char **dataNames=NULL, **metaName=NULL, *baseName=NULL;
      int trailer;

      baseName = (char *) MALLOC(sizeof(char)*255);

      ceos_metadata_ext_t ext_type =
        get_ceos_metadata_name(cfg->general->in_name, &metaName, &trailer);

      if (ext_type == CEOS_LED)
      {
        // alos -- pass in metadata name
        add_to_files_list_iter(metaName[0], NULL, NULL, NULL, NULL, NULL, NULL, NULL, &iter);
      }
      else if (is_polsarpro(cfg->general->in_name)) {
        // PolSARpro -- pass in the data name
        add_to_files_list_iter(cfg->general->in_name, NULL, NULL, "None", NULL, NULL, NULL, NULL, &iter);
      }
      else
      {
        // regular ceos -- determine data file name
        int nBands;

        add_to_files_list_iter(metaName[0], NULL, NULL, NULL, NULL, NULL, NULL, NULL, &iter);
        get_ceos_data_name(cfg->general->in_name, baseName, &dataNames, &nBands);
        assert(nBands == 1);

        add_to_files_list_iter(dataNames[0], NULL, NULL, NULL, NULL, NULL, NULL, NULL, &iter);
      }

      free_ceos_names(dataNames, metaName);
      set_output_name(&iter, cfg->general->out_name);

      FREE(baseName);
    }

    FREE(cfg);

    return 0;
}

datum_type_t get_datum_from_proj_file(char *file, projection_type_t type)
{
  datum_type_t datum;
  FILE *fp = FOPEN(file, "r");

  if (type != POLAR_STEREOGRAPHIC) {
    datum = get_datum(fp);
  }
  else {
    spheroid_type_t spheroid = get_spheroid(fp);
    switch(spheroid) {
      case HUGHES_SPHEROID:
        datum = HUGHES_DATUM;
        break;
      case WGS84_SPHEROID:
      case BESSEL_SPHEROID:
      case CLARKE1866_SPHEROID:
      case CLARKE1880_SPHEROID:
      case GEM6_SPHEROID:
      case GEM10C_SPHEROID:
      case GRS1980_SPHEROID:
      case INTERNATIONAL1924_SPHEROID:
      case INTERNATIONAL1967_SPHEROID:
      case WGS72_SPHEROID:
      case UNKNOWN_SPHEROID:
      default:
        datum = WGS84_DATUM;
        break;
    }
  }
  FCLOSE(fp);

  return datum;
}

void default_to_keep_temp()
{
  rb_select("rb_keep_none", FALSE);
  rb_select("rb_keep_temp", TRUE);
  rb_select("rb_keep_all", FALSE);
}
