/* This program is a simple GUI wrapper around the asf_convert
   tool.  */

#include "asf_convert_gui.h"

GladeXML *glade_xml;

SIGNAL_CALLBACK void
on_browse_input_files_button_clicked(GtkWidget *widget)
{
  GtkWidget *file_selection_dialog =
    glade_xml_get_widget(glade_xml, "input_file_selection");

  gtk_widget_show(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_input_file_selection_cancel_button_clicked(GtkWidget *widget)
{
  GtkWidget *file_selection_dialog =
    glade_xml_get_widget(glade_xml, "input_file_selection");

  gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_input_file_selection_ok_button_clicked(GtkWidget *widget)
{
  GtkWidget *file_selection_dialog =
    glade_xml_get_widget(glade_xml, "input_file_selection");

  gchar **selections = gtk_file_selection_get_selections(
			 GTK_FILE_SELECTION(file_selection_dialog));

  /* only allow selection of 1 file currently */
  gchar *selected_file_name = selections[0];

  GtkWidget *input_entry = glade_xml_get_widget(glade_xml, "input_entry");

  gtk_entry_set_text(GTK_ENTRY(input_entry), selected_file_name);

  gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_asf_convert_destroy(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

SIGNAL_CALLBACK void
on_execute_button_clicked (GtkWidget *button)
{
  GtkWidget *input_entry, 
    *output_entry,
    *input_data_type_combobox,
    *input_data_format_combobox,
    *output_format_combobox,
    *scale_checkbutton,
    *longest_dimension_spinbutton;

  gint input_data_type,
    input_data_format,
    output_format;

  gchar *format,
    *in_meta,
    *size_arg;

  G_CONST_RETURN gchar *in_data,
    *out_full;

  gchar convert_cmd[1024];

  input_entry = 
    glade_xml_get_widget(glade_xml, "input_entry");

  in_data =
    gtk_entry_get_text(GTK_ENTRY(input_entry));

  /* FIXME */
  in_meta =
    (char *)malloc(strlen(in_data) + 1);
  strcpy(in_meta, in_data);
  in_meta[strlen(in_data) - 1] = 'L';

  output_entry = 
    glade_xml_get_widget(glade_xml, "output_entry");

  out_full =
    gtk_entry_get_text(GTK_ENTRY(output_entry));

  input_data_type_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_type =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_type_combobox));

  input_data_format_combobox = 
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  input_data_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_format_combobox));

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  output_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(output_format_combobox));

  scale_checkbutton = 
    glade_xml_get_widget(glade_xml, "scale_checkbutton");

  longest_dimension_spinbutton =
    glade_xml_get_widget(glade_xml, "longest_dimension_spinbutton");

  size_arg = "";
  switch (output_format)
    {
    default:
    case JPEG:
      format = "jpeg";
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(scale_checkbutton)))
	{
	  gint s;
	  gdouble d = 
	    gtk_spin_button_get_value(
	       GTK_SPIN_BUTTON(longest_dimension_spinbutton));
	  s = (int)floor(d+0.5);
	  printf("%g %d\n", d, s);
	  size_arg = (char *)malloc(30);
	  sprintf(size_arg, "-size %ud", s);
	}
      break;
    case PPM:
      format = "ppm";
      break;
    case GEOTIFF:
      format = "geotiff";
      break;
    }

  snprintf (convert_cmd, 1024, "asf_convert -format %s %s %s %s %s",
	    format,
	    size_arg,
	    in_data,
	    in_meta,
	    out_full);

  printf("%s\n", convert_cmd);

  system(convert_cmd);
}

int
main(int argc, char **argv)
{
    /* GtkWidget *widget; */

    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *)find_in_path("asf_convert_gui.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    /* get a widget (useful if you want to change something) */
    /* widget = glade_xml_get_widget (xml, "widgetname"); */

    /* Connect signal handlers.  */
    glade_xml_signal_autoconnect (glade_xml);

    gtk_main ();

    exit (EXIT_SUCCESS);
}
