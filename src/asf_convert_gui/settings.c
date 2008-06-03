#include "asf_convert_gui.h"
#include "asf_convert.h"
#include "asf_raster.h"
#include "libasf_proj.h"
#include "asf_geocode.h"
#include "asf.h"
#include "asf_meta.h"
#include "get_ceos_names.h"
#include <ctype.h>

datum_type_t get_datum_from_proj_file(char *file, projection_type_t type);

static int
settings_get_input_data_format_allows_latitude(const Settings *s)
{
    return /*s->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 ||*/
        s->input_data_format == INPUT_FORMAT_STF;
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
        *input_data_format_combobox,
        *export_checkbutton,
        *output_format_combobox,
        *output_bytes_checkbutton,
        *scaling_method_combobox,
        *keep_files_checkbutton,
        *apply_metadata_fix_checkbutton;

    input_data_type_combobox =
        get_widget_checked("input_data_type_combobox");

    input_data_format_combobox =
        get_widget_checked("input_data_format_combobox");

    export_checkbutton =
        get_widget_checked("export_checkbutton");

    output_format_combobox =
        get_widget_checked("output_format_combobox");

    keep_files_checkbutton =
        get_widget_checked("keep_files_checkbutton");

    apply_metadata_fix_checkbutton =
        get_widget_checked("apply_metadata_fix_checkbutton");

    // Note: The second arg in set_combo_box_item() must match the zero-ordered position
    // of the input data type format items in the input data format combo box
    switch (s->input_data_format) {
      case INPUT_FORMAT_CEOS_LEVEL0:
        set_combo_box_item(input_data_format_combobox, 0);
        break;
      default:
      case INPUT_FORMAT_CEOS_LEVEL1:
        set_combo_box_item(input_data_format_combobox, 1);
        break;
      case INPUT_FORMAT_STF:
        set_combo_box_item(input_data_format_combobox, 2);
        break;
      case INPUT_FORMAT_GEOTIFF:
        set_combo_box_item(input_data_format_combobox, 3);
        break;
      case INPUT_FORMAT_COMPLEX:
        set_combo_box_item(input_data_format_combobox, 4);
        break;
      case INPUT_FORMAT_ESRI:
        // Caution: Not implemented in the GUI
        set_combo_box_item(input_data_format_combobox, 5);
        break;
      case INPUT_FORMAT_ENVI:
        // Caution: Not implemented in the GUI
        set_combo_box_item(input_data_format_combobox, 6);
        break;
      case INPUT_FORMAT_ASF_INTERNAL:
        set_combo_box_item(input_data_format_combobox, 5);
        break;
      case INPUT_FORMAT_AIRSAR:
        set_combo_box_item(input_data_format_combobox, 6);
        break;
    }

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

    if (settings_get_input_data_format_allows_latitude(s))
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

            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(latitude_checkbutton),
                TRUE);

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
            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(latitude_checkbutton),
                FALSE);
        }
    }

    input_data_format_combobox_changed();

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(export_checkbutton), s->export_is_checked);

    set_combo_box_item(output_format_combobox, s->output_format);
    output_format_combobox_changed();

    if (s->polarimetry_setting != POLARIMETRY_NONE)
    {
        GtkWidget *polarimetry_checkbutton =
            get_widget_checked("polarimetry_checkbutton");
        gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(polarimetry_checkbutton), TRUE);

        rb_select("rb_pauli", s->polarimetry_setting==POLARIMETRY_PAULI);
        rb_select("rb_sinclair", s->polarimetry_setting==POLARIMETRY_SINCLAIR);
        rb_select("rb_cloude8", s->polarimetry_setting==POLARIMETRY_CLOUDE8);
        rb_select("rb_cloude16", s->polarimetry_setting==POLARIMETRY_CLOUDE16);
        rb_select("rb_cloude_noclassify",
                  s->polarimetry_setting==POLARIMETRY_CLOUDE_NOCLASSIFY);

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

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(keep_files_checkbutton),
        s->keep_files);
}

Settings *
settings_get_from_gui()
{
    GtkWidget
        *input_data_type_combobox,
        *input_data_format_combobox,
        *process_to_level1_checkbutton,
        *output_format_combobox,
        *scaling_method_combobox;

    Settings *ret = (Settings *) g_malloc0 (sizeof(Settings));

    input_data_type_combobox =
        get_widget_checked("input_data_type_combobox");

    input_data_format_combobox =
        get_widget_checked("input_data_format_combobox");

    output_format_combobox =
        get_widget_checked("output_format_combobox");

    process_to_level1_checkbutton =
        get_widget_checked("process_to_level1_checkbutton");

    ret->data_type = get_combo_box_item(input_data_type_combobox);
    ret->output_format = get_combo_box_item(output_format_combobox);

    // Switch statement must match order of items in the input data format combobox
    gint input_data_format_selection = get_combo_box_item(input_data_format_combobox);
    switch (input_data_format_selection) {
      case 0:
        ret->input_data_format = INPUT_FORMAT_CEOS_LEVEL0;
        break;
      default:
      case 1:
        ret->input_data_format = INPUT_FORMAT_CEOS_LEVEL1;
        break;
      case 2:
        ret->input_data_format = INPUT_FORMAT_STF;
        break;
      case 3:
        ret->input_data_format = INPUT_FORMAT_GEOTIFF;
        break;
      case 4:
        ret->input_data_format = INPUT_FORMAT_COMPLEX;
        break;
      case 5:
        ret->input_data_format = INPUT_FORMAT_ASF_INTERNAL;
        break;
      case 6:
        ret->input_data_format = INPUT_FORMAT_AIRSAR;
        ret->airsar_p_pol = get_checked("airsar_p_pol_checkbutton");
        ret->airsar_l_pol = get_checked("airsar_l_pol_checkbutton");
        ret->airsar_c_pol = get_checked("airsar_c_pol_checkbutton");
        ret->airsar_c_vv = get_checked("airsar_c_vv_checkbutton");
        ret->airsar_l_vv = get_checked("airsar_l_vv_checkbutton");
        break;
      case 7:
        // Caution: Not implemented in the GUI
        ret->input_data_format = INPUT_FORMAT_ENVI;
        break;
      case 8:
        // Caution: Not implemented in the GUI
        ret->input_data_format = INPUT_FORMAT_ESRI;
        break;
    }

    ret->output_db = get_checked("checkbutton_db") &&
        (ret->data_type == INPUT_TYPE_SIGMA ||
         ret->data_type == INPUT_TYPE_BETA ||
         ret->data_type == INPUT_TYPE_GAMMA);

    ret->process_to_level1 =
        ret->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 &&
        gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(process_to_level1_checkbutton));

    ret->apply_scaling = FALSE;
    ret->latitude_low = -99;
    ret->latitude_hi = -99;

    if (settings_get_input_data_format_allows_latitude(ret))
    {
        ret->latitude_checked = get_checked("latitude_checkbutton");
        if (ret->latitude_checked)
        {
            ret->latitude_low = get_double_from_entry("latitude_low_entry");
            ret->latitude_hi = get_double_from_entry("latitude_hi_entry");
        }
    }

    ret->keep_files = get_checked("keep_files_checkbutton");
    ret->apply_metadata_fix = get_checked("apply_metadata_fix_checkbutton");
    ret->apply_ers2_gain_fix = get_checked("ers2_gain_fix_checkbutton");

    ret->polarimetry_setting = POLARIMETRY_NONE;
    if (get_checked("polarimetry_checkbutton")) {
      if (get_checked("rb_pauli"))
        ret->polarimetry_setting = POLARIMETRY_PAULI;
      else if (get_checked("rb_sinclair"))
        ret->polarimetry_setting = POLARIMETRY_SINCLAIR;
      else if (get_checked("rb_cloude8"))
        ret->polarimetry_setting = POLARIMETRY_CLOUDE8;
      else if (get_checked("rb_cloude16"))
        ret->polarimetry_setting = POLARIMETRY_CLOUDE16;
      else if (get_checked("rb_cloude_noclassify"))
        ret->polarimetry_setting = POLARIMETRY_CLOUDE_NOCLASSIFY;
      else
        ret->polarimetry_setting = POLARIMETRY_NONE;
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

        resample_option_menu =
            get_widget_checked("resample_option_menu");

        ret->resample_method =
            gtk_option_menu_get_history(GTK_OPTION_MENU(resample_option_menu));

        ret->geocode_force = get_checked("force_checkbutton");
    }

    if (get_checked("dem_checkbutton"))
    {
        GtkWidget *rb_terrcorr, *rb_refine_geolocation;
        GtkWidget *rb_auto_water_mask, *rb_mask_file;
        GtkWidget *dem_entry;
        GtkWidget *tc_pixel_size_checkbutton;

        rb_terrcorr =
            get_widget_checked("rb_terrcorr");
        rb_refine_geolocation =
            get_widget_checked("rb_refine_geolocation");

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

            ret->interp = get_checked("interpolate_checkbutton");
            ret->refine_geolocation_is_checked = FALSE;
            ret->generate_dem = get_checked("save_dem_checkbutton");
            ret->generate_layover_mask =
                get_checked("layover_mask_checkbutton");
            ret->do_radiometric = get_checked("radiometric_checkbutton");
        }
        else
        {
            ret->refine_geolocation_is_checked =
                get_checked("rb_refine_geolocation");

            // this should be checked if we aren't terrain correcting
            assert(ret->refine_geolocation_is_checked);
        }

        ret->interp_dem_holes = get_checked("interp_dem_holes_checkbutton");

        rb_auto_water_mask =
            get_widget_checked("rb_auto_water_mask");
        rb_mask_file =
            get_widget_checked("rb_mask_file");

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

    if (settings_get_input_data_format_allows_latitude(s) && s->latitude_checked)
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

    if (s->input_data_format == INPUT_FORMAT_CEOS_LEVEL1)
    {
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
    }
    else
    {
        ret = "";
    }

    return ret;
}

const gchar *
settings_get_input_data_format_string(const Settings *s)
{
    const gchar * format_arg_to_import;

    switch (s->input_data_format)
    {
    case INPUT_FORMAT_CEOS_LEVEL0:
        format_arg_to_import = "ceos";
        break;

    default:
    case INPUT_FORMAT_CEOS_LEVEL1:
        format_arg_to_import = "ceos";
        break;

    case INPUT_FORMAT_STF:
        format_arg_to_import = "stf";
        break;

    case INPUT_FORMAT_ESRI:
        format_arg_to_import = "esri";
        break;

    case INPUT_FORMAT_ENVI:
        format_arg_to_import = "envi";
        break;

    case INPUT_FORMAT_COMPLEX:
        format_arg_to_import = "ceos";  /* FIXME: is this correct? */
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
    }

    return format_arg_to_import;
}

int
settings_equal(const Settings *s1, const Settings *s2)
{
    gboolean equal = FALSE;

    assert(s1);
    assert(s2);

    /* settings_print(s1); */
    /* settings_print(s2); */

    if (s1->input_data_format == s2->input_data_format &&
        s1->data_type == s2->data_type &&
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
    switch (s->input_data_format)
    {
    default:
    case INPUT_FORMAT_CEOS_LEVEL0:
        if (!s->process_to_level1)
        {
            out_extension = "raw";
            break;
        }
        /* else, fall through */

    case INPUT_FORMAT_ASF_INTERNAL:
    case INPUT_FORMAT_GEOTIFF:
    case INPUT_FORMAT_CEOS_LEVEL1:
    case INPUT_FORMAT_AIRSAR:
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
            }
        }
        else
        {
            out_extension = "img";
        }
        break;

    case INPUT_FORMAT_COMPLEX:
        out_extension = "cpx";
        break;

    case INPUT_FORMAT_STF:
        out_extension = "raw";
        break;
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
    return s->export_is_checked &&
        s->input_data_format == INPUT_FORMAT_CEOS_LEVEL1;
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
settings_update_dem(Settings *s, const char *output_path, int is_first)
{
    // If this is the second or later file in a list of files,
    // AND the user has specified a .TIFF dem ...
    if (!is_first &&
        (s->terrcorr_is_checked || s->refine_geolocation_is_checked) &&
        has_tiff_ext(s->dem_file))
    {
        // ... we point to the saved DEM instead of the TIFF
        char *file = get_basename(s->dem_file);
        // note that the file name here is hard-coded to match what is
        // set up in libasf_convert's convert_tiff() code.
        sprintf(s->dem_file, "%s/geocoded_dem_%s.img", output_path, file);
        free(file);
    }
}

void
settings_update_mask(Settings *s, const char *output_path, int is_first)
{
    // If this is the second or later file in a list of files,
    // AND the user has specified a .TIFF Mask ...
    if (!is_first &&
        (s->terrcorr_is_checked || s->refine_geolocation_is_checked) &&
        has_tiff_ext(s->mask_file))
    {
        // ... we point to the saved mask instead of the TIFF
        char *file = get_basename(s->mask_file);
        // note that the file name here is hard-coded to match what is
        // set up in libasf_convert's convert_tiff() code.
        sprintf(s->mask_file, "%s/geocoded_mask_%s.img", output_path, file);
        free(file);
    }
}

char *
settings_to_config_file(const Settings *s,
      const gchar *input_file, const gchar *output_full,
      const gchar *output_path, const gchar *tmp_dir)
{
    char *tmp_projfile = NULL;
    char *tmp_cfgfile;
    char *tmp_statfile;
    char *output_file;
    char *output_basename;
    char *input_basename;

    if (s->export_is_checked) {
        output_file = STRDUP(output_full);
    } else {
        output_file = stripExt(output_full);
    }

    output_basename = get_basename(output_file);

    char *base = get_basename(input_file);
    char *path = g_path_get_dirname(input_file);
    input_basename = MALLOC(sizeof(char)*(strlen(base)+strlen(path)+2));

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
            char spheroid_str[256];
            strcpy(spheroid_str, "WGS84");
            if (s->datum == DATUM_HUGHES) {
              strcpy(spheroid_str, "WGS84");
            }
            fprintf(pf, "[Polar Stereographic]\n");
            fprintf(pf, "First Standard Parallel=%.10f\n", s->plat1);
            fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
            fprintf(pf, "Northern Projection=%d\n", (s->lat0 > 0 ? 1 : 0) || (s->plat1 > 0 ? 1 : 0));
            fprintf(pf, "Spheroid=%s\n", spheroid_str);
          }
          break;

        case PROJ_ALBERS:
          fprintf(pf, "[Albers Conical Equal Area]\n");
          fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
          fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
          fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
          fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
          break;

        case PROJ_LAMAZ:
          fprintf(pf, "[Lambert Azimuthal Equal Area]\n");
          fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
          fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
          break;

        case PROJ_LAMCC:
          fprintf(pf, "[Lambert Conformal Conic]\n");
          fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
          fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
          fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
          fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
          fprintf(pf, "Datum=%s\n", datum_string(s->datum));
          break;

        default:
          break;
      }
      fclose(pf);
    }

    tmp_cfgfile =
        MALLOC(sizeof(char)*(9 + strlen(output_basename) + strlen(tmp_dir)));
    sprintf(tmp_cfgfile, "%s/%s.cfg", tmp_dir, output_basename);
    tmp_statfile = appendExt(tmp_cfgfile, ".status");

    FILE * cf = fopen(tmp_cfgfile, "wt");
    if (!cf) return NULL; /* FIXME, need better error handling here */

    fprintf(cf, "Temporary config file, generated by the ASF MapReady Tool\n");
    fprintf(cf, "File was generated on: %s\n\n", date_time_stamp());

    fprintf(cf, "[General]\n");
    // must strip .img for asf internal
    if (s->input_data_format == INPUT_FORMAT_ASF_INTERNAL) {
        char *tmp = stripExt(input_basename);
        fprintf(cf, "input file = %s\n", tmp);
        free(tmp);
    }
    // must strip _meta.airsar for airsar
    else if (s->input_data_format == INPUT_FORMAT_AIRSAR) {
        char *tmp = STRDUP(input_basename);
        char *p = strstr(tmp, "_meta.airsar");
        if (p) *p = '\0';
        fprintf(cf, "input file = %s\n", tmp);
        free(tmp);
    }
    else {
        fprintf(cf, "input file = %s\n", input_basename);
    }
    fprintf(cf, "output file = %s\n", output_file);
    fprintf(cf, "import = %d\n",
        s->input_data_format == INPUT_FORMAT_ASF_INTERNAL ? 0 : 1);
    fprintf(cf, "sar processing = %d\n", s->process_to_level1);
    // fprintf(cf, "image stats=0\n");
    // fprintf(cf, "detect corner reflectors = 0\n");
    fprintf(cf, "polarimetry = %d\n",
            s->polarimetry_setting==POLARIMETRY_NONE ? 0 : 1);
    fprintf(cf, "terrain correction = %d\n",
            s->terrcorr_is_checked || s->refine_geolocation_is_checked);
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
            (s->input_data_format == INPUT_FORMAT_CEOS_LEVEL1 ||
             s->input_data_format == INPUT_FORMAT_AIRSAR      ||
             s->input_data_format == INPUT_FORMAT_ASF_INTERNAL) ? 1 : 0);
    fprintf(cf, "\n");

    fprintf(cf, "[Import]\n");
    if (s->input_data_format == INPUT_FORMAT_CEOS_LEVEL1)
        fprintf(cf, "format = CEOS (1)\n");
    else if (s->input_data_format == INPUT_FORMAT_CEOS_LEVEL0)
        fprintf(cf, "format = CEOS (0)\n");
    else
        fprintf(cf, "format = %s\n", settings_get_input_data_format_string(s));

    if (s->input_data_format == INPUT_FORMAT_CEOS_LEVEL1)
        fprintf(cf, "radiometry = %s_image\n",
                settings_get_data_type_string(s));
    // fprintf(cf, "look up table = \n");
    if (settings_get_input_data_format_allows_latitude(s)) {
      fprintf(cf, "lat begin = %.2f\n", s->latitude_low);
      fprintf(cf, "lat end = %.2f\n", s->latitude_hi);
    }
    // fprintf(cf, "precise =\n");
    fprintf(cf, "output db = %d\n", s->output_db);
    // can't currently handle complex, L0 data in the envi dump
    if (s->input_data_format != INPUT_FORMAT_CEOS_LEVEL1 &&
        s->input_data_format != INPUT_FORMAT_ASF_INTERNAL)
        fprintf(cf, "dump envi header = 0\n");

    // the "multilook SLC" option is ignored on non-SLC data
    // we multilook on import if we are going to be geocoding,
    // though we don't multilook if we going to be doing a polarimetric
    // decomposition, since in that case we'll multilook as we do Pauli, etc
    int multilook_on_import = s->geocode_is_checked;
    if (s->polarimetry_setting != POLARIMETRY_NONE)
      multilook_on_import = FALSE;

    fprintf(cf, "multilook SLC = %d\n", multilook_on_import ? 1 : 0);
    fprintf(cf, "apply ers2 gain fix = %d\n", s->apply_ers2_gain_fix);
    fprintf(cf, "\n");

    if (s->input_data_format == INPUT_FORMAT_AIRSAR) {
        fprintf(cf, "[AirSAR]\n");
        fprintf(cf, "airsar c interferometric = %d\n", s->airsar_c_vv);
        fprintf(cf, "airsar l interferometric = %d\n", s->airsar_l_vv);
        fprintf(cf, "airsar c polarimetric = %d\n", s->airsar_c_pol);
        fprintf(cf, "airsar l polarimetric = %d\n", s->airsar_l_pol);
        fprintf(cf, "airsar p polarimetric = %d\n", s->airsar_p_pol);
        fprintf(cf, "\n");
    }

    if (s->process_to_level1) {
        fprintf(cf, "[SAR processing]\n");
        fprintf(cf, "radiometry = %s_image\n",
                settings_get_data_type_string(s));
        fprintf(cf, "\n");
    }

    if (s->polarimetry_setting != POLARIMETRY_NONE) {
        fprintf(cf, "[Polarimetry]\n");
        fprintf(cf, "pauli = %d\n",
                s->polarimetry_setting==POLARIMETRY_PAULI?1:0);
        fprintf(cf, "sinclair = %d\n",
                s->polarimetry_setting==POLARIMETRY_SINCLAIR?1:0);
        fprintf(cf, "cloude pottier = %d\n",
                s->polarimetry_setting==POLARIMETRY_CLOUDE8?1:0);
        fprintf(cf, "extended cloude pottier = %d\n",
                s->polarimetry_setting==POLARIMETRY_CLOUDE16?1:0);
        fprintf(cf, "entropy anisotropy alpha = %d\n",
                s->polarimetry_setting==POLARIMETRY_CLOUDE_NOCLASSIFY?1:0);
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
            else if (s->specified_pixel_size) // geocode pixel size
                fprintf(cf, "pixel spacing = %.2lf\n", s->pixel_size);
            fprintf(cf, "refine geolocation only = 0\n");
            fprintf(cf, "interpolate = %d\n", s->interp);
            fprintf(cf, "do radiometric = %d\n", s->do_radiometric);
            fprintf(cf, "save terrcorr dem = %d\n", s->generate_dem);
            fprintf(cf, "save terrcorr layover mask = %d\n",
                    s->generate_layover_mask);
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

    if (s->geocode_is_checked) {
      fprintf(cf, "[Geocoding]\n");
      fprintf(cf, "projection = %s\n", tmp_projfile);
      if (s->specified_pixel_size)
        fprintf(cf, "pixel spacing = %.2f\n", s->pixel_size);
      if (s->specified_height)
        fprintf(cf, "height = %.2f\n", s->height);
      // As of MapReady v1.1.x, the datum is no longer written to the
      // config file ...the datum in the (temporary) proj file is used instead.
      // The user-selected datum is written to the temporary proj file above.
      //fprintf(cf, "datum = %s\n", datum_string(s->datum));
      fprintf(cf, "resampling = %s\n",
        resample_method_string(s->resample_method));
      fprintf(cf, "force = %d\n", s->geocode_force);
      fprintf(cf, "\n");
    }

    if (s->export_is_checked) {
      fprintf(cf, "[Export]\n");
      fprintf(cf, "format = %s\n", settings_get_output_format_string(s));
      if (s->output_bytes && !s->truecolor_is_checked &&
          !s->falsecolor_is_checked)
      {
          fprintf(cf, "byte conversion = %s\n",
            scaling_method_string(s->scaling_method));
      } else {
          fprintf(cf, "byte conversion = none\n");
      }
      if (s->polarimetry_setting == POLARIMETRY_CLOUDE8)
        fprintf(cf,"rgb look up table = cloude8\n");
      else if (s->polarimetry_setting == POLARIMETRY_CLOUDE16)
        fprintf(cf,"rgb look up table = cloude16\n");
      else if (s->export_bands ||
               s->polarimetry_setting == POLARIMETRY_PAULI ||
               s->polarimetry_setting == POLARIMETRY_SINCLAIR)
      {
        if (s->polarimetry_setting == POLARIMETRY_PAULI) {
          fprintf(cf, "rgb banding = HH-VV,HV+VH,HH+VV\n");
        }
        else if (s->polarimetry_setting == POLARIMETRY_SINCLAIR) {
          fprintf(cf, "rgb banding = HH,half(HV+VH),VV\n");
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

    s.input_data_format = INPUT_FORMAT_CEOS_LEVEL1;
    if (strncmp(uc(cfg->import->format), "CEOS (1)", 8) == 0)
        s.input_data_format = INPUT_FORMAT_CEOS_LEVEL1;
    if (strncmp(uc(cfg->import->format), "CEOS (0)", 8) == 0)
        s.input_data_format = INPUT_FORMAT_CEOS_LEVEL0;
    if (strncmp(uc(cfg->import->format), "CEOS", 4) == 0)
        s.input_data_format = INPUT_FORMAT_CEOS_LEVEL1;
    else if (strncmp(uc(cfg->import->format), "STF", 3) == 0)
        s.input_data_format = INPUT_FORMAT_STF;
    else if (strncmp(uc(cfg->import->format), "ASF", 3) == 0)
        s.input_data_format = INPUT_FORMAT_ASF_INTERNAL;
    else if (strncmp(uc(cfg->import->format), "GEOTIFF", 7) == 0)
      s.input_data_format = INPUT_FORMAT_GEOTIFF;
    else if (strncmp(uc(cfg->import->format), "AIRSAR", 6) == 0)
      s.input_data_format = INPUT_FORMAT_AIRSAR;

    if (s.input_data_format == INPUT_FORMAT_AIRSAR) {
      s.airsar_c_vv = cfg->airsar->c_vv;
      s.airsar_l_vv = cfg->airsar->l_vv;
      s.airsar_c_pol = cfg->airsar->c_pol;
      s.airsar_l_pol = cfg->airsar->l_pol;
      s.airsar_p_pol = cfg->airsar->p_pol;
    } else {
      s.airsar_c_vv=0;
      s.airsar_l_vv=0;
      s.airsar_c_pol=0;
      s.airsar_l_pol=0;
      s.airsar_p_pol=0;
    }

    s.data_type = INPUT_TYPE_AMP;
    if (strncmp(uc(cfg->import->radiometry), "AMPLITUDE_IMAGE", 15) == 0)
        s.data_type = INPUT_TYPE_AMP;
    else if (strncmp(uc(cfg->import->radiometry), "POWER_IMAGE", 11) == 0)
        s.data_type = INPUT_TYPE_POWER;
    else if (strncmp(uc(cfg->import->radiometry), "SIGMA_IMAGE", 11) == 0)
        s.data_type = INPUT_TYPE_SIGMA;
    else if (strncmp(uc(cfg->import->radiometry), "GAMMA_IMAGE", 11) == 0)
        s.data_type = INPUT_TYPE_GAMMA;
    else if (strncmp(uc(cfg->import->radiometry), "BETA_IMAGE", 10) == 0)
        s.data_type = INPUT_TYPE_BETA;

    s.process_to_level1 = cfg->general->sar_processing;
    s.output_db = cfg->import->output_db;
    s.latitude_checked = s.input_data_format == INPUT_FORMAT_STF &&
        (cfg->import->lat_begin != -99 || cfg->import->lat_end != -99);
    s.latitude_low = cfg->import->lat_begin;
    s.latitude_hi = cfg->import->lat_end;
    s.apply_ers2_gain_fix = cfg->import->ers2_gain_fix;

    /* polarimetry */
    s.polarimetry_setting = POLARIMETRY_NONE;
    if (cfg->general->polarimetry) {
      if (cfg->polarimetry->pauli)
        s.polarimetry_setting = POLARIMETRY_PAULI;
      else if (cfg->polarimetry->sinclair)
        s.polarimetry_setting = POLARIMETRY_SINCLAIR;
      else if (cfg->polarimetry->cloude_pottier)
        s.polarimetry_setting = POLARIMETRY_CLOUDE8;
      else if (cfg->polarimetry->cloude_pottier_ext)
        s.polarimetry_setting = POLARIMETRY_CLOUDE16;
      else if (cfg->polarimetry->cloude_pottier_nc)
        s.polarimetry_setting = POLARIMETRY_CLOUDE_NOCLASSIFY;
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
        read_proj_file(cfg->geocoding->projection, &pps, &type);

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
        }

        s.specified_height = cfg->geocoding->height != -99 &&
            cfg->geocoding->height != 0;
        s.height = cfg->geocoding->height;
        s.specified_pixel_size = cfg->geocoding->pixel > 0;
        s.pixel_size = cfg->geocoding->pixel;
        s.geocode_force = cfg->geocoding->force;

        // GET DATUM
        // - Default to WGS84, then
        // - If the datum is appropriately set in the config file, use that, then
        // - If reading from a proj file, allow THAT datum to override all else
        s.datum = DATUM_WGS84;
        if (strncmp(uc(cfg->geocoding->datum), "WGS84", 5) == 0) {
          s.datum = DATUM_WGS84;
        }
        else if (strncmp(uc(cfg->geocoding->datum), "NAD27", 5) == 0) {
          s.datum = DATUM_NAD27;
        }
        else if (strncmp(uc(cfg->geocoding->datum), "NAD83", 5) == 0) {
          s.datum = DATUM_NAD83;
        }
        else if (strncmp(uc(cfg->geocoding->datum), "HUGHES", 6) == 0) {
          s.datum = DATUM_HUGHES;
        }
        datum_type_t datum_type = get_datum_from_proj_file(cfg->geocoding->projection, type);
        switch(datum_type) {
          case WGS84_DATUM:
            s.datum = DATUM_WGS84;
            break;
          case NAD27_DATUM:
            s.datum = DATUM_NAD27;
            break;
          case NAD83_DATUM:
            s.datum = DATUM_NAD83;
            break;
          case HUGHES_DATUM:
            s.datum = DATUM_HUGHES;
            break;
          case EGM96_DATUM:
          case ED50_DATUM:
          case ETRF89_DATUM:
          case ETRS89_DATUM:
          case ITRF97_DATUM:
          case UNKNOWN_DATUM:
          default:
            s.datum = DATUM_WGS84;
            break;
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
        s.tc_pixel_size = cfg->terrain_correct->pixel;
        s.interp = cfg->terrain_correct->interp;
        s.auto_water_mask_is_checked = cfg->terrain_correct->auto_mask_water;
        s.mask_file_is_checked = strlen(cfg->terrain_correct->mask) > 0;
        strcpy(s.mask_file, cfg->terrain_correct->mask);
        s.generate_layover_mask =
            cfg->terrain_correct->save_terrcorr_layover_mask;
        s.generate_dem = cfg->terrain_correct->save_terrcorr_dem;
        s.do_radiometric = cfg->terrain_correct->do_radiometric;
        s.interp_dem_holes = cfg->terrain_correct->smooth_dem_holes;
    }

    /* misc */
    s.keep_files = cfg->general->intermediates;
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
        add_to_files_list_iter(metaName[0], &iter);
      }
      else
      {
        // regular ceos -- determine data file name
        int nBands;

        add_to_files_list_iter(metaName[0], &iter);
        get_ceos_data_name(cfg->general->in_name, baseName, &dataNames, &nBands);
        assert(nBands == 1);

        add_to_files_list_iter(dataNames[0], &iter);
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

