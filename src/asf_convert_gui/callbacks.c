#include "asf_convert_gui.h"

void
show_execute_button(gboolean show)
{
  GtkWidget *execute_button, *stop_button, *load_button;
 
  execute_button =
    glade_xml_get_widget(glade_xml, "execute_button");

  stop_button =
    glade_xml_get_widget(glade_xml, "stop_button");

  load_button =
    glade_xml_get_widget(glade_xml, "load_button");

  gtk_widget_set_sensitive(execute_button, show);
  gtk_widget_set_sensitive(stop_button, !show);
  gtk_widget_set_sensitive(load_button, show);
}

void 
input_data_format_combobox_changed()
{
  GtkWidget *input_data_type_combobox,
    *input_data_type_label,
    *input_data_format_combobox,
    *latitude_label,
    *latitude_low_label,
    *latitude_low_spinbutton,
    *latitude_hi_label,
    *latitude_hi_spinbutton,
    *vbox_export;

  gint input_data_format;
  gboolean show_data_type_combobox;
  gboolean show_latitude_spinbuttons;
  gboolean show_export_section;

  input_data_format_combobox =
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");
  /*
  input_data_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_format_combobox));
  */
  input_data_format =
    gtk_option_menu_get_history(GTK_OPTION_MENU(input_data_format_combobox));

  switch (input_data_format)
  {
    case INPUT_FORMAT_STF:
      show_export_section = FALSE;
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = TRUE;
      break;
    case INPUT_FORMAT_COMPLEX:
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = FALSE;
      show_export_section = FALSE;
      break;
    case INPUT_FORMAT_CEOS_LEVEL0:
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = FALSE;
      show_export_section = FALSE;
      break;
    default:
    case INPUT_FORMAT_CEOS_LEVEL1:
    case INPUT_FORMAT_ESRI:
    case INPUT_FORMAT_ENVI:
      show_data_type_combobox = TRUE;
      show_latitude_spinbuttons = FALSE;
      show_export_section = TRUE;
      break;
    case INPUT_FORMAT_ASF_INTERNAL:
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = FALSE;
      show_export_section = TRUE;
      break;
  }

  input_data_type_combobox =
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_type_label =
    glade_xml_get_widget(glade_xml, "input_data_type_label");

  gtk_widget_set_sensitive(input_data_type_combobox, show_data_type_combobox);
  gtk_widget_set_sensitive(input_data_type_label, show_data_type_combobox);

  latitude_label =
    glade_xml_get_widget(glade_xml, "latitude_label");

  latitude_low_label =
    glade_xml_get_widget(glade_xml, "latitude_low_label");

  latitude_low_spinbutton =
    glade_xml_get_widget(glade_xml, "latitude_low_spinbutton");

  latitude_hi_label =
    glade_xml_get_widget(glade_xml, "latitude_hi_label");

  latitude_hi_spinbutton =
    glade_xml_get_widget(glade_xml, "latitude_hi_spinbutton");

  /* I bet I could have disabled the hbox instead ... */
  gtk_widget_set_sensitive(latitude_label, show_latitude_spinbuttons);
  gtk_widget_set_sensitive(latitude_low_label, show_latitude_spinbuttons);
  gtk_widget_set_sensitive(latitude_low_spinbutton, show_latitude_spinbuttons);
  gtk_widget_set_sensitive(latitude_hi_label, show_latitude_spinbuttons);
  gtk_widget_set_sensitive(latitude_hi_spinbutton, show_latitude_spinbuttons);

  vbox_export =
    glade_xml_get_widget(glade_xml, "vbox_export");

  gtk_widget_set_sensitive(vbox_export, show_export_section);

  output_format_combobox_changed();
}

void
scale_checkbutton_toggle()
{
  GtkWidget *longest_dimension_label,
    *longest_dimension_spinbutton,
    *scale_checkbutton;
 
 gboolean is_checked;

  longest_dimension_label =
    glade_xml_get_widget(glade_xml, "longest_dimension_label");

  longest_dimension_spinbutton =
    glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

  scale_checkbutton =
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  is_checked =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(scale_checkbutton));

  gtk_widget_set_sensitive(longest_dimension_label, is_checked);
  gtk_widget_set_sensitive(longest_dimension_spinbutton, is_checked);
}

void
output_bytes_checkbutton_toggle()
{
    GtkWidget *output_bytes_checkbutton,
    *scaling_method_combobox,
    *scaling_method_label;
 
    gboolean is_checked;

    output_bytes_checkbutton =
         glade_xml_get_widget(glade_xml, "output_bytes_checkbutton");

    scaling_method_combobox =
         glade_xml_get_widget(glade_xml, "scaling_method_combobox");

    scaling_method_label =
         glade_xml_get_widget(glade_xml, "scaling_method_label");

    is_checked =
         gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(output_bytes_checkbutton));

    gtk_widget_set_sensitive(scaling_method_combobox, is_checked);
    gtk_widget_set_sensitive(scaling_method_label, is_checked);
}

void
output_format_combobox_changed()
{
  GtkWidget *output_format_combobox,
    *longest_dimension_label,
    *longest_dimension_spinbutton,
    *scale_checkbutton,
    *output_bytes_checkbutton,
    *scaling_method_combobox,
    *scaling_method_label;

  gint output_format;
  gboolean show;

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  output_format = get_combo_box_item(output_format_combobox);

  switch (output_format)
  {
    default:
    case OUTPUT_FORMAT_JPEG:
    case OUTPUT_FORMAT_PPM:
    case OUTPUT_FORMAT_TIFF:
      show = TRUE;
      break;
    case OUTPUT_FORMAT_GEOTIFF:
    case OUTPUT_FORMAT_ASF_INTERNAL:
    case OUTPUT_FORMAT_CEOS:  
      show = FALSE;
      break;
  }

  longest_dimension_label =
    glade_xml_get_widget(glade_xml, "longest_dimension_label");

  longest_dimension_spinbutton =
    glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

  scale_checkbutton =
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  gtk_widget_set_sensitive(longest_dimension_label, show);
  gtk_widget_set_sensitive(longest_dimension_spinbutton, show);
  gtk_widget_set_sensitive(scale_checkbutton, show);

  if (show)
    scale_checkbutton_toggle();

  output_bytes_checkbutton =
    glade_xml_get_widget(glade_xml, "output_bytes_checkbutton");

  scaling_method_combobox =
    glade_xml_get_widget(glade_xml, "scaling_method_combobox");

  scaling_method_label =
    glade_xml_get_widget(glade_xml, "scaling_method_label");
  
  switch (output_format)
  {
      default:
      case OUTPUT_FORMAT_JPEG:
      case OUTPUT_FORMAT_PPM:
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
}
#else
SIGNAL_CALLBACK void
on_ceos_level_0_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_ceos_level_1_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_stf_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_complex_activate(GtkWidget *widget)
{
  input_data_format_combobox_changed();
}
#endif

#ifndef USE_GTK_22
SIGNAL_CALLBACK void
on_output_format_combobox_changed(GtkWidget *widget)
{
  output_format_combobox_changed();
}
#else

SIGNAL_CALLBACK void
on_jpeg_activate(GtkWidget *widget)
{
  output_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_tiff_activate(GtkWidget *widget)
{
  output_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_geotiff_activate(GtkWidget *widget)
{
  output_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_ppm_activate(GtkWidget *widget)
{
  output_format_combobox_changed();
}

#endif

SIGNAL_CALLBACK void
on_scale_checkbutton_toggled(GtkWidget *widget)
{
  scale_checkbutton_toggle();
}

SIGNAL_CALLBACK void
on_output_bytes_checkbutton_toggled(GtkWidget *widget)
{
  output_bytes_checkbutton_toggle();
}
