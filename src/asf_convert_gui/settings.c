#include "asf_convert_gui.h"

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
    s->output_format == OUTPUT_FORMAT_TIFF;
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
    *output_format_combobox,
    *scale_checkbutton,
    *output_bytes_checkbutton,
    *scaling_method_combobox;

  input_data_type_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_format_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  scale_checkbutton = 
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  set_combo_box_item(input_data_format_combobox, s->input_data_format);
  set_combo_box_item(input_data_type_combobox, s->data_type);
  set_combo_box_item(output_format_combobox, s->output_format);

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

  if (settings_get_output_format_requires_byte(s))
  {
      /* seems like we should do something! */
  }
  else
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
	  GtkWidget * central_meridian_entry;
	  GtkWidget * latitude_of_origin_entry;
	  GtkWidget * first_standard_parallel_entry;
	  GtkWidget * second_standard_parallel_entry;
	  GtkWidget * false_northing_entry;
	  GtkWidget * false_easting_entry;
	  GtkWidget * average_height_checkbutton;
	  GtkWidget * pixel_size_checkbutton;
	  GtkWidget * datum_option_menu;

	  projection_option_menu =
	      glade_xml_get_widget(glade_xml, "projection_option_menu");

	  set_combo_box_item(projection_option_menu, s->projection);
	  
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
      }
  }
}

Settings *
settings_get_from_gui()
{
  GtkWidget
    *input_data_type_combobox,
    *input_data_format_combobox,
    *output_format_combobox,
    *scale_checkbutton,
    *output_bytes_checkbutton,
    *scaling_method_combobox,
    *geocode_checkbutton;

  Settings *ret;

  ret = (Settings *) g_malloc0 (sizeof(Settings));

  input_data_type_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_format_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  scale_checkbutton = 
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  ret->data_type = get_combo_box_item(input_data_type_combobox);
  ret->input_data_format = get_combo_box_item(input_data_format_combobox);
  ret->output_format = get_combo_box_item(output_format_combobox);

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

          ret->scaling_method = get_combo_box_item(scaling_method_combobox);
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
	  *central_meridian_entry,
	  *latitude_of_origin_entry,
	  *first_standard_parallel_entry,
	  *second_standard_parallel_entry,
	  *false_northing_entry,
	  *false_easting_entry, 
	  *average_height_checkbutton,
	  *pixel_size_checkbutton, 
	  *average_height_entry,
	  *pixel_size_entry,
	  *datum_option_menu;

      projection_option_menu =
	  glade_xml_get_widget(glade_xml, "projection_option_menu");

      ret->projection =
	  gtk_option_menu_get_history(
	      GTK_OPTION_MENU(projection_option_menu));
      
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

      pixel_size_checkbutton =
	  glade_xml_get_widget(glade_xml, "pixel_size_checkbutton");

      ret->specified_height = 
	  gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
					   average_height_checkbutton));

      ret->specified_pixel_size = 
	  gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(
					     pixel_size_checkbutton));

      if (ret->specified_height)
      {
	  average_height_entry =
	      glade_xml_get_widget(glade_xml, "average_height_entry");

	  ret->height =
	      atof(gtk_entry_get_text(GTK_ENTRY(average_height_entry)));
      }
      
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

const gchar *
settings_get_output_bytes_argument(const Settings *s)
{
    static gchar byte_arg[64];

    if (s->output_bytes)
    {
        gchar * arg;
        
        switch (s->scaling_method)
        {
            default:
            case SCALING_METHOD_SIGMA:
                arg = "sigma";
                break;
                
            case SCALING_METHOD_MINMAX:
                arg = "minmax";
                break;
                
            case SCALING_METHOD_TRUNCATE:
                arg = "truncate";
                break;
        }
        
        g_snprintf(byte_arg, sizeof(byte_arg),
                   "-byte %s", arg);
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
  static gchar buf[32];

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
        s1->output_format == s2->output_format)
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
			equal = TRUE;

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
    case INPUT_FORMAT_CEOS_LEVEL1:
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
      break;

    case INPUT_FORMAT_COMPLEX:
      out_extension = "cpx";
      break;

    case INPUT_FORMAT_STF:
    case INPUT_FORMAT_CEOS_LEVEL0:
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
settings_get_projection_abbrev(const Settings *s)
{
    switch(s->projection)
    {
	default:
	    return "";
	case UNIVERSAL_TRANSVERSE_MERCATOR:
	    return "utm";
	case POLAR_STEREOGRAPHIC:
	    return "ps";
	case LAMBERT_CONFORMAL_CONIC:
	    return "lamcc";
	case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	    return "lamaz";
	case ALBERS_EQUAL_AREA:
	    return "albers";
    }
}

int
settings_get_run_geocode(const Settings *s)
{
    return s->geocode_is_checked;
}

int
settings_get_run_import(const Settings *s)
{
  return s->output_format != OUTPUT_FORMAT_ASF_INTERNAL;
}

int 
settings_get_run_export(const Settings *s)
{
  return s->input_data_format == INPUT_FORMAT_CEOS_LEVEL1;
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
