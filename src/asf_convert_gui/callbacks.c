#include "asf_convert_gui.h"

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
    *input_data_format_combobox,
    *latitude_label,
    *latitude_low_label,
    *latitude_low_spinbutton,
    *latitude_hi_label,
    *latitude_hi_spinbutton;

  gint input_data_format;
  gboolean show_data_type_combobox;
  gboolean show_latitude_spinbuttons;

  input_data_format_combobox =
    glade_xml_get_widget(glade_xml, "input_data_format_combobox");

  input_data_format =
    gtk_combo_box_get_active(GTK_COMBO_BOX(input_data_format_combobox));

  switch (input_data_format)
  {
    case INPUT_FORMAT_STF:
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = TRUE;
      break;
    case INPUT_FORMAT_COMPLEX:
    case INPUT_FORMAT_CEOS_LEVEL0:
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = FALSE;
      break;
    default:
    case INPUT_FORMAT_CEOS_LEVEL1:
    case INPUT_FORMAT_ESRI:
    case INPUT_FORMAT_ENVI:
      show_data_type_combobox = TRUE;
      show_latitude_spinbuttons = FALSE;
      break;
    case INPUT_FORMAT_ASF_INTERNAL:
      show_data_type_combobox = FALSE;
      show_latitude_spinbuttons = FALSE;
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

  update_all_extensions();
}

SIGNAL_CALLBACK void
on_add_button_clicked(GtkWidget *widget)
{
  GtkWidget *input_entry;
  G_CONST_RETURN gchar *in_data;
  gchar *data, *meta;

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
  data = g_strdup(in_data);
  meta = meta_file_name(data);

  if (strlen(meta) > 0)
  {
    add_to_files_list(data, meta);
    g_free(meta);
  }
   
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
  GtkWidget *file_selection_dialog;
  gchar **selections;

  file_selection_dialog =
    glade_xml_get_widget(glade_xml, "input_file_selection");

  selections = gtk_file_selection_get_selections(
			 GTK_FILE_SELECTION(file_selection_dialog));

  gchar ** current = selections;

  while (*current)
  {
    gchar * meta = meta_file_name(*current);

    if (strlen(meta) > 0)
    {
      add_to_files_list(*current, meta);
      g_free(meta);
    }

    ++current;
  }

  g_strfreev(selections);
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

SIGNAL_CALLBACK void
on_change_output_name_button_cancel_clicked(GtkWidget *widget)
{
  GtkWidget *change_output_name_dialog =
    glade_xml_get_widget(glade_xml, "change_output_name_dialog");

  gtk_widget_hide(change_output_name_dialog);
}

SIGNAL_CALLBACK void
on_change_output_name_button_ok_clicked(GtkWidget *widget)
{
  GtkWidget *change_output_name_dialog;
  GtkWidget *entry_new_output_filename;
  const gchar * new_name;

  change_output_name_dialog =
    glade_xml_get_widget(glade_xml, "change_output_name_dialog");

  entry_new_output_filename =
    glade_xml_get_widget(glade_xml, "entry_new_output_filename");

  new_name = gtk_entry_get_text(GTK_ENTRY(entry_new_output_filename));

  if (strlen(new_name) > 0)
  {
    GtkTreeSelection * selection;
    GtkWidget * files_list;
    GtkTreeIter iter;
    GtkTreeModel * model;

    /* since dialog is modal, can assume same row is selected */
    files_list = glade_xml_get_widget(glade_xml, "files_list");
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    
    assert(gtk_tree_selection_count_selected_rows(selection) == 1);
    
    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
      const char * ext;
      char *user_ext, *basename, *name_without_path, *p, *fixed_name,
	*data_file_name, *path;
      Settings * user_settings;

      user_settings = settings_get_from_gui();
      ext = settings_get_output_format_extension(user_settings);

      gtk_tree_model_get(model, &iter, 0, &data_file_name, -1);
      path = g_path_get_dirname(data_file_name);
      if (strcmp(path, ".") == 0)
      {
	*path = '\0';
      }
      else
      {
	int len = strlen(path);
	path = (char *)realloc(path, len + 1);
	*(path + len) = DIR_SEPARATOR;
	*(path + len + 1) = '\0';	
      }

      /* do not allow user to move output file to a different location */
      name_without_path = g_path_get_basename(new_name);

      /* add appropriate extension if was not given by user */
      basename = strdup(name_without_path);
      p = strrchr(basename, '.');
      if (p)
      {
	*p = '\0';
	user_ext = p + 1;
      }
      else
      {
	user_ext = NULL;
      }

      if (user_ext == NULL)
      {
	fixed_name = (char *)malloc(strlen(path) + strlen(basename) + 
				    strlen(ext) + 2);

	sprintf(fixed_name, "%s%s.%s", path, basename, ext);
      }
      else if (strcmp(user_ext, ext) != 0)
      {
	fixed_name = (char *)malloc(strlen(path) + strlen(name_without_path) + 
				    strlen(ext) + 2);
	sprintf(fixed_name, "%s%s.%s", path, name_without_path, ext);
      }
      else
      {
	fixed_name = (char *)malloc(strlen(path) + strlen(name_without_path));
	sprintf(fixed_name, "%s%s", path, name_without_path);
      }

      free(basename);
      free(name_without_path);
      free(path);
    
      gtk_list_store_set(list_store, &iter, 1, fixed_name, -1);

      free(fixed_name);
    }
  }
  
  gtk_widget_hide(change_output_name_dialog);  
}

SIGNAL_CALLBACK void
on_help_button_clicked(GtkWidget *widget)
{
  GtkWidget *help_dialog;
  GtkWidget *help_text;
  GtkTextBuffer * text_buffer;
  FILE * help_file;
  gchar * help_filename;

  help_dialog =
    glade_xml_get_widget(glade_xml, "help_dialog");

  help_text =
    glade_xml_get_widget(glade_xml, "help_text");

  text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));
  gtk_text_buffer_set_text(text_buffer, "", -1);

  help_filename = (gchar *)find_in_path("asf_convert_gui_help.txt");
  help_file = fopen(help_filename, "rt");
  if (help_file)
  {
    while (!feof(help_file))
    {
      char buffer[1024];
      char *p = fgets(buffer, 1024, help_file);
      if (p)
      {
	GtkTextIter end;
	gtk_text_buffer_get_end_iter(text_buffer, &end);
	gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
      }
    }

    fclose(help_file);
  }

  gtk_widget_show(help_dialog);
}

SIGNAL_CALLBACK void
on_help_dialog_ok_button_clicked(GtkWidget *widget)
{
  GtkWidget *help_dialog =
    glade_xml_get_widget(glade_xml, "help_dialog");

  gtk_widget_hide(help_dialog);
}
