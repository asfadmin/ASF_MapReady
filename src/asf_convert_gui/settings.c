#include "asf_convert_gui.h"
#include <asf.h>

static int
settings_get_input_data_format_allows_latitude(const Settings *s)
{
    return /*s->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 ||*/
        s->input_data_format == INPUT_FORMAT_STF;
}

static int
settings_get_output_format_allows_size(const Settings *s)
{
    return s->output_format == OUTPUT_FORMAT_JPEG ||
        s->output_format == OUTPUT_FORMAT_PPM ||
        s->output_format == OUTPUT_FORMAT_TIFF ||
        s->output_format == OUTPUT_FORMAT_GEOTIFF;
}

static int
settings_get_output_format_requires_byte(const Settings *s)
{
    return s->output_format == OUTPUT_FORMAT_JPEG ||
        s->output_format == OUTPUT_FORMAT_PPM ||
        s->output_format == OUTPUT_FORMAT_TIFF;
}

void
settings_apply_to_gui(const Settings * s)
{
    GtkWidget
        *input_data_type_combobox,
        *input_data_format_combobox,
        *export_checkbutton,
        *output_format_combobox,
        *scale_checkbutton,
        *output_bytes_checkbutton,
        *scaling_method_combobox,
        *keep_files_checkbutton,
        *apply_metadata_fix_checkbutton,
        *terrcorr_checkbutton;

    input_data_type_combobox = 
        glade_xml_get_widget(glade_xml, "input_data_type_combobox");

    input_data_format_combobox = 
        glade_xml_get_widget(glade_xml, "input_data_format_combobox");

    export_checkbutton =
        glade_xml_get_widget(glade_xml, "export_checkbutton");

    output_format_combobox = 
        glade_xml_get_widget(glade_xml, "output_format_combobox");

    scale_checkbutton = 
        glade_xml_get_widget(glade_xml, "scale_checkbutton");

    keep_files_checkbutton = 
        glade_xml_get_widget(glade_xml, "keep_files_checkbutton");

    apply_metadata_fix_checkbutton = 
        glade_xml_get_widget(glade_xml, "apply_metadta_fix_checkbutton");

    set_combo_box_item(input_data_format_combobox, s->input_data_format);
    set_combo_box_item(output_format_combobox, s->output_format);
    set_combo_box_item(input_data_type_combobox, s->data_type);

    if (s->process_to_level1)
    {
        GtkWidget *process_to_level1_checkbutton =
            glade_xml_get_widget(glade_xml, "process_to_level1_checkbutton");

        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(process_to_level1_checkbutton),
            s->process_to_level1);
    }

    if (s->output_db)
    {
        GtkWidget *checkbutton_db =
	  glade_xml_get_widget(glade_xml, "checkbutton_db");

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbutton_db), TRUE);
    }

    input_data_type_changed();

    if (settings_get_output_format_allows_size(s))
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(scale_checkbutton),
            s->apply_scaling);

        if (s->apply_scaling)
        {
            GtkWidget *longest_dimension_spinbutton;

            longest_dimension_spinbutton = 
                glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

            gtk_spin_button_set_value(GTK_SPIN_BUTTON(longest_dimension_spinbutton),
                s->longest_dimension);

        }
    }

    if (settings_get_input_data_format_allows_latitude(s))
    {
        GtkWidget 
            *latitude_checkbutton,
            *latitude_low_entry,
            *latitude_hi_entry;

        latitude_checkbutton =
            glade_xml_get_widget(glade_xml, "latitude_checkbutton");

        if (s->latitude_checked)
        {
            gchar tmp[32];

            gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(latitude_checkbutton),
                TRUE);

            latitude_low_entry =
                glade_xml_get_widget(glade_xml, "latitude_low_entry");

            latitude_hi_entry =
                glade_xml_get_widget(glade_xml, "latitude_hi_entry");

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

    if (settings_get_output_format_requires_byte(s))
    {
        /* seems like we should do something! */
    }
    else if (s->export_is_checked)
    {
        output_bytes_checkbutton =
            glade_xml_get_widget(glade_xml, "output_bytes_checkbutton");

        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(output_bytes_checkbutton),
            s->output_bytes);

        scaling_method_combobox =
            glade_xml_get_widget(glade_xml, "scaling_method_combobox");

        set_combo_box_item(scaling_method_combobox, s->scaling_method);
    }        

    output_format_combobox_changed();

    if (s->geocode_is_checked)
    {
        gchar tmp[32];

        GtkWidget * geocode_checkbutton;

        geocode_checkbutton =
            glade_xml_get_widget(glade_xml, "geocode_checkbutton");

        gtk_toggle_button_set_active(
            GTK_TOGGLE_BUTTON(geocode_checkbutton), s->geocode_is_checked);

        if (s->geocode_is_checked)
        {
            GtkWidget * projection_option_menu;
            GtkWidget * utm_zone_entry;
            GtkWidget * central_meridian_entry;
            GtkWidget * latitude_of_origin_entry;
            GtkWidget * first_standard_parallel_entry;
            GtkWidget * second_standard_parallel_entry;
            GtkWidget * false_northing_entry;
            GtkWidget * false_easting_entry;
            GtkWidget * average_height_checkbutton;
            GtkWidget * pixel_size_checkbutton;
            GtkWidget * datum_option_menu;
            GtkWidget * resample_option_menu;
            GtkWidget * force_checkbutton;

            projection_option_menu =
                glade_xml_get_widget(glade_xml, "projection_option_menu");

            set_combo_box_item(projection_option_menu, s->projection);

            utm_zone_entry =
                glade_xml_get_widget(glade_xml, "utm_zone_entry");

            central_meridian_entry =
                glade_xml_get_widget(glade_xml, "central_meridian_entry");

            latitude_of_origin_entry =
                glade_xml_get_widget(glade_xml, "latitude_of_origin_entry");

            first_standard_parallel_entry =
                glade_xml_get_widget(glade_xml, "first_standard_parallel_entry");

            second_standard_parallel_entry =
                glade_xml_get_widget(glade_xml,
                "second_standard_parallel_entry");

            false_northing_entry =
                glade_xml_get_widget(glade_xml, "false_northing_entry");

            false_easting_entry =
                glade_xml_get_widget(glade_xml, "false_easting_entry");

            if (s->projection == UNIVERSAL_TRANSVERSE_MERCATOR)
            {
                sprintf(tmp, "%d", s->zone);
                gtk_entry_set_text(GTK_ENTRY(utm_zone_entry), tmp);
            }
            else
            {
                gtk_entry_set_text(GTK_ENTRY(utm_zone_entry), "");
            }

            sprintf(tmp, "%f", s->lon0);
            gtk_entry_set_text(GTK_ENTRY(central_meridian_entry), tmp);

            sprintf(tmp, "%f", s->lat0);
            gtk_entry_set_text(GTK_ENTRY(latitude_of_origin_entry), tmp);

            sprintf(tmp, "%f", s->plat1);
            gtk_entry_set_text(GTK_ENTRY(first_standard_parallel_entry), tmp);

            sprintf(tmp, "%f", s->plat2);
            gtk_entry_set_text(GTK_ENTRY(second_standard_parallel_entry), tmp);

            sprintf(tmp, "%f", s->false_easting);
            gtk_entry_set_text(GTK_ENTRY(false_easting_entry), tmp);

            sprintf(tmp, "%f", s->false_northing);
            gtk_entry_set_text(GTK_ENTRY(false_northing_entry), tmp);

            average_height_checkbutton =
                glade_xml_get_widget(glade_xml, "average_height_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(average_height_checkbutton), 
                s->specified_height);

            if (s->specified_height)
            {
                GtkWidget * average_height_entry;

                average_height_entry =
                    glade_xml_get_widget(glade_xml, "average_height_entry");

                sprintf(tmp, "%f", s->height);
                gtk_entry_set_text(GTK_ENTRY(average_height_entry), tmp);
            }

            pixel_size_checkbutton =
                glade_xml_get_widget(glade_xml, "pixel_size_checkbutton");

            gtk_toggle_button_set_active(
                GTK_TOGGLE_BUTTON(pixel_size_checkbutton), 
                s->specified_pixel_size);

            if (s->specified_pixel_size)
            {
                GtkWidget * pixel_size_entry;

                pixel_size_entry =
                    glade_xml_get_widget(glade_xml, "pixel_size_entry");

                sprintf(tmp, "%f", s->pixel_size);
                gtk_entry_set_text(GTK_ENTRY(pixel_size_entry), tmp);
            }

            datum_option_menu =
                glade_xml_get_widget(glade_xml, "datum_option_menu");

            set_combo_box_item(datum_option_menu, s->datum);

            resample_option_menu =
                glade_xml_get_widget(glade_xml, "resample_option_menu");

            set_combo_box_item(resample_option_menu, s->resample_method);

	    force_checkbutton =
	        glade_xml_get_widget(glade_xml, "force_checkbutton");

	    gtk_toggle_button_set_active(
	        GTK_TOGGLE_BUTTON(force_checkbutton),
		s->geocode_force);
        }
    }

    terrcorr_checkbutton =
        glade_xml_get_widget(glade_xml, "terrcorr_checkbutton");

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(terrcorr_checkbutton),
				 s->terrcorr_is_checked);

    if (s->terrcorr_is_checked)
    {
        GtkWidget *dem_entry;
	dem_entry = glade_xml_get_widget(glade_xml, "dem_entry");
	gtk_entry_set_text(GTK_ENTRY(dem_entry), s->dem_file);
    }
    else
    {
        GtkWidget *dem_entry;
	dem_entry = glade_xml_get_widget(glade_xml, "dem_entry");
	gtk_entry_set_text(GTK_ENTRY(dem_entry), "");
    }

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(keep_files_checkbutton),
        s->keep_files);

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
        apply_metadata_fix_checkbutton),
        s->apply_metadata_fix);
}

Settings *
settings_get_from_gui()
{
    GtkWidget
        *input_data_type_combobox,
        *checkbutton_db,
        *input_data_format_combobox,
        *process_to_level1_checkbutton,
        *export_checkbutton,
        *output_format_combobox,
        *scale_checkbutton,
        *output_bytes_checkbutton,
        *scaling_method_combobox,
        *geocode_checkbutton,
        *keep_files_checkbutton,
        *apply_metadata_fix_checkbutton,
        *terrcorr_checkbutton;

    Settings *ret;

    ret = (Settings *) g_malloc0 (sizeof(Settings));

    input_data_type_combobox = 
        glade_xml_get_widget(glade_xml, "input_data_type_combobox");

    input_data_format_combobox = 
        glade_xml_get_widget(glade_xml, "input_data_format_combobox");

    export_checkbutton = 
        glade_xml_get_widget(glade_xml, "export_checkbutton");

    output_format_combobox = 
        glade_xml_get_widget(glade_xml, "output_format_combobox");

    scale_checkbutton = 
        glade_xml_get_widget(glade_xml, "scale_checkbutton");

    keep_files_checkbutton = 
        glade_xml_get_widget(glade_xml, "keep_files_checkbutton");

    apply_metadata_fix_checkbutton = 
        glade_xml_get_widget(glade_xml, "apply_metadata_fix_checkbutton");

    checkbutton_db =
        glade_xml_get_widget(glade_xml, "checkbutton_db");

    process_to_level1_checkbutton =
        glade_xml_get_widget(glade_xml, "process_to_level1_checkbutton");

    ret->data_type = get_combo_box_item(input_data_type_combobox);
    ret->output_format = get_combo_box_item(output_format_combobox);
    ret->input_data_format = get_combo_box_item(input_data_format_combobox);

    ret->output_db =
        (ret->data_type == INPUT_TYPE_SIGMA ||
         ret->data_type == INPUT_TYPE_BETA ||
         ret->data_type == INPUT_TYPE_GAMMA) &&
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton_db));

    ret->process_to_level1 =
        ret->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 &&
        gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(process_to_level1_checkbutton));

    ret->apply_scaling =
        settings_get_output_format_allows_size(ret) &&
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(scale_checkbutton));

    if (ret->apply_scaling)
    {
        GtkWidget *longest_dimension_spinbutton;

        longest_dimension_spinbutton = 
            glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

        ret->longest_dimension = (float) gtk_spin_button_get_value(
            GTK_SPIN_BUTTON(longest_dimension_spinbutton));
    }

    ret->latitude_low = -999;
    ret->latitude_hi = -999;

    if (settings_get_input_data_format_allows_latitude(ret))
    {
        GtkWidget 
            *latitude_checkbutton,
            *latitude_low_entry,
            *latitude_hi_entry;

        latitude_checkbutton =
            glade_xml_get_widget(glade_xml, "latitude_checkbutton");

        ret->latitude_checked =
            gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(latitude_checkbutton));

        if (ret->latitude_checked)
        {
            latitude_low_entry =
                glade_xml_get_widget(glade_xml, "latitude_low_entry");

            latitude_hi_entry =
                glade_xml_get_widget(glade_xml, "latitude_hi_entry");

            ret->latitude_low = atof(
                gtk_entry_get_text(GTK_ENTRY(latitude_low_entry)));

            ret->latitude_hi = atof(
                gtk_entry_get_text(GTK_ENTRY(latitude_hi_entry)));
        }
    }

    ret->export_is_checked = gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(export_checkbutton));

    if (ret->export_is_checked)
    {
        if (settings_get_output_format_requires_byte(ret))
        {
            scaling_method_combobox =
                glade_xml_get_widget(glade_xml, "scaling_method_combobox");

            ret->output_bytes = TRUE;
            ret->scaling_method = get_combo_box_item(scaling_method_combobox);
        }
        else
        {
            output_bytes_checkbutton =
                glade_xml_get_widget(glade_xml, "output_bytes_checkbutton");

            ret->output_bytes =
                gtk_toggle_button_get_active(
                GTK_TOGGLE_BUTTON(output_bytes_checkbutton));

            if (ret->output_bytes)
            {
                scaling_method_combobox =
                    glade_xml_get_widget(glade_xml, "scaling_method_combobox");

                ret->scaling_method =
                    get_combo_box_item(scaling_method_combobox);
            }
        }
    }

    geocode_checkbutton =
        glade_xml_get_widget(glade_xml, "geocode_checkbutton");

    ret->geocode_is_checked =
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(geocode_checkbutton));

    if (ret->geocode_is_checked)
    {
        GtkWidget 
            *projection_option_menu,
            *utm_zone_entry,
            *central_meridian_entry,
            *latitude_of_origin_entry,
            *first_standard_parallel_entry,
            *second_standard_parallel_entry,
            *false_northing_entry,
            *false_easting_entry, 
            *average_height_checkbutton,
            *average_height_entry,
            *pixel_size_checkbutton,
            *pixel_size_entry,
            *datum_option_menu,
	    *resample_option_menu,
            *force_checkbutton;

        projection_option_menu =
            glade_xml_get_widget(glade_xml, "projection_option_menu");

        ret->projection =
            gtk_option_menu_get_history(
            GTK_OPTION_MENU(projection_option_menu));

        utm_zone_entry =
            glade_xml_get_widget(glade_xml, "utm_zone_entry");

        central_meridian_entry =
            glade_xml_get_widget(glade_xml, "central_meridian_entry");

        latitude_of_origin_entry =
            glade_xml_get_widget(glade_xml, "latitude_of_origin_entry");

        first_standard_parallel_entry =
            glade_xml_get_widget(glade_xml, "first_standard_parallel_entry");

        second_standard_parallel_entry =
            glade_xml_get_widget(glade_xml, "second_standard_parallel_entry");

        false_northing_entry =
            glade_xml_get_widget(glade_xml, "false_northing_entry");

        false_easting_entry =
            glade_xml_get_widget(glade_xml, "false_easting_entry");

        ret->zone = atoi(gtk_entry_get_text(
            GTK_ENTRY(utm_zone_entry)));
        ret->lon0 = atof(gtk_entry_get_text(
            GTK_ENTRY(central_meridian_entry)));
        ret->lat0 = atof(gtk_entry_get_text(
            GTK_ENTRY(latitude_of_origin_entry)));
        ret->plat1 = atof(gtk_entry_get_text(
            GTK_ENTRY(first_standard_parallel_entry)));
        ret->plat2 = atof(gtk_entry_get_text(
            GTK_ENTRY(second_standard_parallel_entry)));
        ret->false_northing = atof(gtk_entry_get_text(
            GTK_ENTRY(false_northing_entry)));
        ret->false_easting = atof(gtk_entry_get_text(
            GTK_ENTRY(false_easting_entry)));

        average_height_checkbutton =
            glade_xml_get_widget(glade_xml, "average_height_checkbutton");

        ret->specified_height = 
            gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
            average_height_checkbutton));

        if (ret->specified_height)
        {
            average_height_entry =
                glade_xml_get_widget(glade_xml, "average_height_entry");

            ret->height =
                atof(gtk_entry_get_text(GTK_ENTRY(average_height_entry)));
        }

        pixel_size_checkbutton =
            glade_xml_get_widget(glade_xml, "pixel_size_checkbutton");

        ret->specified_pixel_size = 
            gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
            pixel_size_checkbutton));

        if (ret->specified_pixel_size)
        {
            pixel_size_entry =
                glade_xml_get_widget(glade_xml, "pixel_size_entry");

            ret->pixel_size =
                atof(gtk_entry_get_text(GTK_ENTRY(pixel_size_entry)));
        }

        datum_option_menu =
            glade_xml_get_widget(glade_xml, "datum_option_menu");

        ret->datum =
            gtk_option_menu_get_history(
            GTK_OPTION_MENU(datum_option_menu));

        resample_option_menu =
            glade_xml_get_widget(glade_xml, "resample_option_menu");

        ret->resample_method =
            gtk_option_menu_get_history(GTK_OPTION_MENU(resample_option_menu));

	force_checkbutton =
	    glade_xml_get_widget(glade_xml, "force_checkbutton");

	ret->geocode_force =
	    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
					   force_checkbutton));
    }

    ret->keep_files = 
        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(keep_files_checkbutton));

    ret->apply_metadata_fix = 
        gtk_toggle_button_get_active(
        GTK_TOGGLE_BUTTON(apply_metadata_fix_checkbutton));

    terrcorr_checkbutton =
        glade_xml_get_widget(glade_xml, "terrcorr_checkbutton");

    ret->terrcorr_is_checked =
        gtk_toggle_button_get_active(
	  GTK_TOGGLE_BUTTON(terrcorr_checkbutton));

    if (ret->terrcorr_is_checked)
    {
        GtkWidget *dem_entry;
	dem_entry = glade_xml_get_widget(glade_xml, "dem_entry");
	strcpy(ret->dem_file, gtk_entry_get_text(GTK_ENTRY(dem_entry)));
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
            glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

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
        format_arg_to_import = "";
        break;
    }

    return format_arg_to_import;
}

/*
static void settings_print(Settings *s)
{
printf("(%d,%d,%d,%s,%s,%s)\n",
s->input_data_format,
s->data_type,
s->output_format,
settings_get_latitude_argument(s),
settings_get_size_argument(s),
settings_get_output_bytes_argument(s));
}
*/

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
        s1->apply_metadata_fix == s2->apply_metadata_fix)
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

    case INPUT_FORMAT_CEOS_LEVEL1:
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

            case OUTPUT_FORMAT_PPM:
                out_extension = "ppm";
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

    case OUTPUT_FORMAT_PPM:
        format_arg_to_export = "ppm";
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
        s->output_format == OUTPUT_FORMAT_PPM;
}

void
settings_delete(Settings * s)
{
    g_free(s);
}

char *
settings_to_config_file(const Settings *s,
			const gchar *input_file, const gchar *output_full,
			const gchar *output_path)
{
    char *tmp_projfile = NULL;
    char *tmp_cfgfile;
    char *tmp_statfile;
    char *dts;
    char *output_file;

    if (s->export_is_checked) {
        output_file = strdup(output_full);
    } else {
        output_file = stripExt(output_full);
    }

    if (s->geocode_is_checked) {

      tmp_projfile = appendExt(output_file, ".proj");

      FILE * pf = fopen(tmp_projfile, "wt");
      if (!pf) return NULL; /* FIXME, need better error handling here */

      switch (s->projection)
      {
	case PROJ_UTM:
	  fprintf(pf, "[Universal Transverse Mercator]\n");
	  fprintf(pf, "Zone=%d\n", s->zone != 0 ? s->zone : 0);
	  break;
	  
	case PROJ_PS:
	  fprintf(pf, "[Polar Stereographic]\n");
	  fprintf(pf, "First Standard Parallel=%.10f\n", s->plat1);
	  fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
	  fprintf(pf, "Northern Projection=%d\n", s->lat0 >=0 ? 1 : 0);
	  break;
	  
	case PROJ_ALBERS:
	  fprintf(pf, "[Albers Conical Equal Area]\n");
	  fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
	  fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
	  fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
	  fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
	  break;
	  
	case PROJ_LAMAZ:
	  fprintf(pf, "[Lambert Azimuthal Equal Area]\n");
	  fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
	  fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
	  break;
	  
	case PROJ_LAMCC:
	  fprintf(pf, "[Lambert Conformal Conic]\n");
	  fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
	  fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
	  fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
	  fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
      }
      fclose(pf);
    }

    tmp_cfgfile = appendExt(output_file, ".cfg");
    tmp_statfile = appendExt(tmp_cfgfile, ".status");

    FILE * cf = fopen(tmp_cfgfile, "wt");
    if (!cf) return NULL; /* FIXME, need better error handling here */

    fprintf(cf, "Temporary config file, generated by the ASF Convert Tool\n");
    dts = date_time_stamp();
    fprintf(cf, "File was generated on: %s\n\n", dts);
    free(dts);

    fprintf(cf, "[General]\n");
    fprintf(cf, "input file = %s\n", input_file);
    fprintf(cf, "output file = %s\n", output_file);
    fprintf(cf, "import = 1\n");
    fprintf(cf, "sar processing = %d\n", s->process_to_level1);
    // fprintf(cf, "image stats=0\n");
    // fprintf(cf, "detect corner reflectors = 0\n");
    fprintf(cf, "terrain correction = %d\n", s->terrcorr_is_checked);
    fprintf(cf, "geocoding = %d\n", s->geocode_is_checked);
    fprintf(cf, "export = %d\n", s->export_is_checked);
    // fprintf(cf, "default values =\n");
    fprintf(cf, "intermediates = %d\n", s->keep_files);
    fprintf(cf, "status file = %s\n", tmp_statfile);
    fprintf(cf, "short configuration file = 0\n");
    fprintf(cf, "\n");

    fprintf(cf, "[Import]\n");
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
    fprintf(cf, "\n");

    if (s->process_to_level1) {
        fprintf(cf, "[SAR processing]\n");
        fprintf(cf, "radiometry = %s_image\n", 
                settings_get_data_type_string(s));
        fprintf(cf, "\n");
    }

    if (s->terrcorr_is_checked) {
      fprintf(cf, "[Terrain correction]\n");
      if (s->specified_tc_pixel_size)
	fprintf(cf, "pixel spacing = %.2lf\n", s->tc_pixel_size);
      fprintf(cf, "digital elevation model = %s\n", s->dem_file);
      fprintf(cf, "\n");
    }

    if (s->geocode_is_checked) {
      fprintf(cf, "[Geocoding]\n");
      fprintf(cf, "projection = %s\n", tmp_projfile);
      if (s->specified_pixel_size)
	fprintf(cf, "pixel spacing = %.2f\n", s->pixel_size);
      if (s->specified_height)
	fprintf(cf, "height = %.2f\n", s->height);
      fprintf(cf, "datum = %s\n", datum_string(s->datum));
      fprintf(cf, "resampling = %s\n",
	      resample_method_string(s->resample_method));
      fprintf(cf, "force = %d\n", s->geocode_force);
      fprintf(cf, "\n");
    }

    if (s->export_is_checked) {
      fprintf(cf, "[Export]\n");
      fprintf(cf, "format = %s\n", settings_get_output_format_string(s));
      if (s->output_bytes) {
          fprintf(cf, "byte conversion = %s\n", 
            scaling_method_string(s->scaling_method));
      } else {
          fprintf(cf, "byte conversion = none\n");
      }
      fprintf(cf, "\n");
      fprintf(cf, "\n");
    }

    fprintf(cf, "\n");
    fprintf(cf, "\n");

    fclose(cf);
    free(tmp_statfile);
    if (tmp_projfile)
      free(tmp_projfile);
    free(output_file);
    return tmp_cfgfile;
}
