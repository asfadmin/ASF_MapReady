#include "asf_convert_gui.h"

void
add_to_files_list(gchar * data_file, gchar * meta_file)
{
  GtkWidget *files_list;
  GtkTreeIter iter;

  files_list =
    glade_xml_get_widget(glade_xml, "files_list");

  gtk_list_store_append(list_store, &iter);

  gtk_list_store_set(list_store, &iter, 
		     0, data_file, 1, meta_file, 2, "", -1);
}

void
show_execute_button(gboolean show)
{
  GtkWidget *execute_button, *stop_button;
 
  execute_button =
    glade_xml_get_widget(glade_xml, "execute_button");

  stop_button =
    glade_xml_get_widget(glade_xml, "stop_button");

  gtk_widget_set_sensitive(execute_button, show);
  gtk_widget_set_sensitive(stop_button, !show);
}

void 
input_data_format_combobox_changed()
{
  GtkWidget *input_data_type_combobox,
    *input_data_type_label,
    *input_data_format_combobox;

  gint input_data_format;
  gboolean show;

  input_data_type_combobox =
    glade_xml_get_widget(glade_xml, "input_data_type_combobox");

  input_data_type_label =
    glade_xml_get_widget(glade_xml, "input_data_type_label");

  input_data_format_combobox =
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  input_data_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_format_combobox));

  switch (input_data_format)
  {
    default:
    case INPUT_FORMAT_CEOS:
    case INPUT_FORMAT_STF:
    case INPUT_FORMAT_ESRI:
    case INPUT_FORMAT_ENVI:
      show = TRUE;
      break;
    case INPUT_FORMAT_ASF_INTERNAL:
      show = FALSE;
      break;
  }

  gtk_widget_set_sensitive(input_data_type_combobox, show);
  gtk_widget_set_sensitive(input_data_type_label, show);
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
output_format_combobox_changed()
{
  GtkWidget *output_format_combobox,
    *longest_dimension_label,
    *longest_dimension_spinbutton,
    *scale_checkbutton;

  gint output_format;
  gboolean show;

  output_format_combobox = 
    glade_xml_get_widget(glade_xml, "output_format_combobox");

  output_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(output_format_combobox));

  switch (output_format)
  {
    default:
    case OUTPUT_FORMAT_JPEG:
    case OUTPUT_FORMAT_PPM:
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
}

SIGNAL_CALLBACK void
on_add_button_clicked(GtkWidget *widget)
{
  GtkWidget *input_entry;
  G_CONST_RETURN gchar *in_data;

  input_entry = 
    glade_xml_get_widget(glade_xml, "input_entry");

  in_data =
    gtk_entry_get_text(GTK_ENTRY(input_entry));

  if (strlen(in_data) == 0)
  {
    message_box("Enter the name of a data file (.D) to add to the list.");
    return;
  }

  if (!g_file_test(in_data, G_FILE_TEST_EXISTS))
  {
    char * message = (char *)malloc(strlen(in_data) + 1024);
    sprintf(message, "Couldn't find the file \"%s\".", in_data);
    message_box(message);
    return;
  }

  /* add file & meta to the list */
  gchar * data = g_strdup(in_data);
  gchar * meta = g_strdup(in_data);
  meta[strlen(meta) - 1] = 'L'; /* gotta fix this */

  add_to_files_list(data, meta);
  g_free(meta);
  g_free(data);
}

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

  gchar * meta = g_strdup(selected_file_name);
  meta[strlen(meta) - 1] = 'L'; /* gotta fix this */

  add_to_files_list(selected_file_name, meta);

  g_free(meta);
  gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_asf_convert_destroy(GtkWidget *widget, gpointer data)
{
  gtk_main_quit();
}

SIGNAL_CALLBACK void
on_input_data_format_combobox_changed(GtkWidget *widget)
{
  input_data_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_output_format_combobox_changed(GtkWidget *widget)
{
  output_format_combobox_changed();
}

SIGNAL_CALLBACK void
on_scale_checkbutton_toggled(GtkWidget *widget)
{
  scale_checkbutton_toggle();
}
