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
    s->output_format == OUTPUT_FORMAT_PPM;
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
    GtkWidget *latitude_low_spinbutton, *latitude_hi_spinbutton;

    latitude_low_spinbutton =
      glade_xml_get_widget(glade_xml, "latitude_low_spinbutton");

    latitude_hi_spinbutton =
      glade_xml_get_widget(glade_xml, "latitude_hi_spinbutton");

   gtk_spin_button_set_value(GTK_SPIN_BUTTON(latitude_low_spinbutton),
			     s->latitude_low);
   gtk_spin_button_set_value(GTK_SPIN_BUTTON(latitude_hi_spinbutton),
			     s->latitude_hi);
  }

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
    *scaling_method_combobox;

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

  if (settings_get_input_data_format_allows_latitude(ret))
  {
    GtkWidget *latitude_low_spinbutton, *latitude_hi_spinbutton;

    latitude_low_spinbutton =
      glade_xml_get_widget(glade_xml, "latitude_low_spinbutton");

    latitude_hi_spinbutton =
      glade_xml_get_widget(glade_xml, "latitude_hi_spinbutton");

    ret->latitude_low =
      gtk_spin_button_get_value(GTK_SPIN_BUTTON(latitude_low_spinbutton));
    
    ret->latitude_hi =
      gtk_spin_button_get_value(GTK_SPIN_BUTTON(latitude_hi_spinbutton));
  }
  else
  {
    ret->latitude_low = -999;
    ret->latitude_hi = -999;
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
  
  return ret;
}

const gchar *
settings_get_latitude_argument(const Settings *s)
{
  static gchar latitude_arg[128];

  if (settings_get_input_data_format_allows_latitude(s))
  {
    g_snprintf(latitude_arg, sizeof(latitude_arg),
	       "-lat %d %d", (int) s->latitude_low, (int) s->latitude_hi);
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
                    equal = TRUE;

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
  switch (s->output_format)
  {
    case OUTPUT_FORMAT_ASF_INTERNAL:
      out_extension = "";
      break;

    case OUTPUT_FORMAT_CEOS:
      out_extension = "D";
      break;

    case OUTPUT_FORMAT_JPEG:
    default:
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

int
settings_get_run_import(const Settings *s)
{
  return s->output_format != OUTPUT_FORMAT_ASF_INTERNAL;
}

int 
settings_get_run_export(const Settings *s)
{
  return s->input_data_format != INPUT_FORMAT_ASF_INTERNAL;
}

void
settings_delete(Settings * s)
{
    g_free(s);
}
