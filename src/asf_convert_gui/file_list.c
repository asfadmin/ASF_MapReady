#include "asf_convert_gui.h"

static gchar *
determine_default_output_file_name(gchar * data_file_name)
{
  Settings * user_settings;
  const gchar * ext;
  gchar * output_name_full;
  gchar * basename;
  gchar * p;

  basename = g_strdup(data_file_name);
  p = strrchr(basename, '.');
  if (p)
    *p = '\0';
  
  user_settings = settings_get_from_gui();
  ext = settings_get_output_format_extension(user_settings);
  output_name_full = (gchar *)g_malloc(strlen(basename) + strlen(ext) + 2);
  g_sprintf(output_name_full, "%s.%s", basename, ext);

  g_free(basename);

  return output_name_full;
}

void
add_to_files_list(gchar * data_file, gchar * meta_file)
{
  GtkWidget *files_list;
  GtkTreeIter iter;
  gchar * out_name_full;

  files_list =
    glade_xml_get_widget(glade_xml, "files_list");

  gtk_list_store_append(list_store, &iter);

  out_name_full = determine_default_output_file_name(data_file);

  gtk_list_store_set(list_store, &iter, 
		     0, data_file, 1, out_name_full, 2, "-", -1);

  g_free(out_name_full);
}

void
update_all_extensions()
{
  Settings * user_settings;
  const gchar * ext;
  gboolean valid;
  GtkTreeIter iter;

  if (list_store)
  {
    user_settings = settings_get_from_gui();
    ext = settings_get_output_format_extension(user_settings);
    
    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
    while (valid)
    {
      gchar * current_output_name;
      gchar * new_output_name;
      gchar * basename;
      gchar * p;
      
      gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
			 1, &current_output_name, -1);
      
      basename = g_strdup(current_output_name);
      p = strrchr(basename, '.');
      if (p)
	*p = '\0';
      
      new_output_name = (gchar *)g_malloc(strlen(basename) + strlen(ext) + 1);
      g_sprintf(new_output_name, "%s.%s", basename, ext);
      
      gtk_list_store_set(list_store, &iter, 1, new_output_name, -1);
      
      g_free(basename);
      g_free(new_output_name);
      
      valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }
  }
}

void
edited_handler(GtkCellRendererText *ce, gchar *arg1, gchar *arg2, 
	       gpointer user_data)
{
  /* arg1 indicates which row -- should assert() that it matches
     the selected row, since we're asssuming that */
  do_rename_selected(arg2);
}

void
setup_files_list(int argc, char *argv[])
{
  gint i;
  GtkWidget *files_list;
  GtkTreeViewColumn *col;
  GtkCellRenderer *renderer;
  GtkTreeIter iter;

  list_store = gtk_list_store_new(3, 
				  G_TYPE_STRING, 
				  G_TYPE_STRING, 
				  G_TYPE_STRING);

  for (i = 1; i < argc; ++i)
  {
    gchar * data_file = argv[i];
    gchar * output_file = determine_default_output_file_name(data_file);

    gtk_list_store_append(list_store, &iter);
    gtk_list_store_set(list_store, &iter,
		       0, data_file, 1, output_file, 2, "-", -1);

    g_free(output_file);
  }

  files_list =
    glade_xml_get_widget(glade_xml, "files_list");

  /* First Column */
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, "Data File");
  gtk_tree_view_column_set_resizable(col, TRUE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  g_object_set(renderer, "text", "?", NULL);
  gtk_tree_view_column_add_attribute(col, renderer, "text", 0);

  /* Second Column */
  /* -- this was the "meta" column -- removed
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, "Meta File");
  gtk_tree_view_column_set_resizable(col, TRUE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  gtk_tree_view_column_add_attribute(col, renderer, "text", 1);
  */

  /* Third (now 2nd) Column */
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, "Output File");
  gtk_tree_view_column_set_resizable(col, TRUE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
  renderer = gtk_cell_renderer_text_new();

  /* allow editing the output filename right in the grid */
  GValue val = {0,};
  g_value_init(&val, G_TYPE_BOOLEAN);
  g_value_set_boolean(&val, TRUE);
  g_object_set_property(G_OBJECT(renderer), "editable", &val);

  /* connect "editing-done" signal */
  g_signal_connect(G_OBJECT(renderer), "edited",
		   G_CALLBACK(edited_handler), NULL);

  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  gtk_tree_view_column_add_attribute(col, renderer, "text", 1);

  /* Last Column: Current Status */
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, "Status");
  gtk_tree_view_column_set_resizable(col, TRUE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  gtk_tree_view_column_add_attribute(col, renderer, "text", 2);

  gtk_tree_view_set_model(GTK_TREE_VIEW(files_list), 
			  GTK_TREE_MODEL(list_store));  

  g_object_unref(list_store);

  gtk_tree_selection_set_mode(
      gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list)),
      GTK_SELECTION_SINGLE);
}
