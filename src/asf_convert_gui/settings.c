#include "asf_convert_gui.h"

static int
settings_get_input_data_format_allows_latitude(Settings *s)
{
  return /*s->input_data_format == INPUT_FORMAT_CEOS_LEVEL0 ||*/
    s->input_data_format == INPUT_FORMAT_STF;
}

static int
settings_get_output_format_allows_size(Settings *s)
{
  return s->output_format == OUTPUT_FORMAT_JPEG ||
    s->output_format == OUTPUT_FORMAT_PPM;
}

void
settings_apply_to_gui(Settings * s)
{
  GtkWidget
    *input_data_type_combobox,
    *input_data_format_combobox,
    *output_format_combobox,
    *scale_checkbutton;

  input_data_type_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_format_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  scale_checkbutton = 
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  gtk_combo_box_set_active(GTK_COMBO_BOX(input_data_format_combobox),
			   s->input_data_format);

  gtk_combo_box_set_active(GTK_COMBO_BOX(input_data_type_combobox),
			   s->data_type);

  gtk_combo_box_set_active(GTK_COMBO_BOX(output_format_combobox),
			   s->output_format);

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
}

Settings *
settings_get_from_gui()
{
  GtkWidget
    *input_data_type_combobox,
    *input_data_format_combobox,
    *output_format_combobox,
    *scale_checkbutton;

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

  ret->data_type =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_type_combobox));

  ret->input_data_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_format_combobox));

  ret->output_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(output_format_combobox));

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
 
  return ret;
}

const gchar *
settings_get_latitude_argument(Settings *s)
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
settings_get_size_argument(Settings *s)
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
settings_get_data_type_string(Settings *s)
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
settings_get_input_data_format_string(Settings *s)
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

static void settings_print(Settings *s)
{
    printf("(%d,%d,%d,%s,%s)\n",
           s->input_data_format,
           s->data_type,
           s->output_format,
           settings_get_latitude_argument(s),
           settings_get_size_argument(s));
}
    
int
settings_equal(Settings *s1, Settings *s2)
{
  gboolean equal = FALSE;
  
  assert(s1);
  assert(s2);

  /*settings_print(s1);*/
  /*settings_print(s2);*/

  equal =
    s1->input_data_format == s2->input_data_format &&
    s1->data_type == s2->data_type &&
    0 == strcmp(
        settings_get_latitude_argument(s1),
        settings_get_latitude_argument(s2)) &&
    s1->output_format == s2->output_format &&
    0 == strcmp(
        settings_get_size_argument(s1),
        settings_get_size_argument(s2));

  /*printf("Equal = %s\n", equal ? "yes" : "no");*/
  return equal;
}

Settings *
settings_copy(Settings *s)
{
  Settings * ret;

  ret = (Settings *)g_malloc0(sizeof(Settings));
  memcpy(ret, s, sizeof(Settings));

  assert(settings_equal(s, ret));

  return ret;
}

const gchar *
settings_get_output_format_extension(Settings *s)
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
      out_extension = "tif";
      break;
  }

  return out_extension;
}

const gchar *
settings_get_output_format_string(Settings *s)
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
  }

  return format_arg_to_export;
}

int
settings_get_run_import(Settings *s)
{
  return s->output_format != OUTPUT_FORMAT_ASF_INTERNAL;
}

int 
settings_get_run_export(Settings *s)
{
  return s->input_data_format != INPUT_FORMAT_ASF_INTERNAL;
}

