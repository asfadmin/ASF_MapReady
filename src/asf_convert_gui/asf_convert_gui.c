/* This program is a simple GUI wrapper around the asf_convert
   tool.  */

#include "asf_convert_gui.h"

GladeXML *glade_xml;
GtkListStore *list_store;
gboolean keep_going;

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
    char * data_file = argv[i];
    char * meta_file = strdup(argv[i]);
    if (strlen(meta_file) > 0)
    {
      meta_file[strlen(meta_file) - 1] = 'L';

      gtk_list_store_append(list_store, &iter);

      gtk_list_store_set(list_store, &iter,
		     0, data_file, 1, meta_file, 2, "", -1);
    }

    free(meta_file);
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
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, "Meta File");
  gtk_tree_view_column_set_resizable(col, TRUE);
  gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  gtk_tree_view_column_add_attribute(col, renderer, "text", 1);

  /* Third Column */
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, "Output File");
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
      GTK_SELECTION_NONE);
}

int
main(int argc, char **argv)
{
    GtkWidget *widget;

    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *)find_in_path("asf_convert_gui.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    /* select defaults for dropdowns */
    widget = glade_xml_get_widget (glade_xml, "input_data_type_combobox");
    gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 3);
    widget = glade_xml_get_widget (glade_xml, "input_data_format_combobox");
    gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 0);
    widget = glade_xml_get_widget (glade_xml, "output_format_combobox");
    gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 0);

    /* fire handlers for hiding/showing stuff */
    output_format_combobox_changed();
    input_data_format_combobox_changed();
    show_execute_button(TRUE);

    /* build columns in the files section */
    setup_files_list(argc, argv);

    /* Connect signal handlers.  */
    glade_xml_signal_autoconnect (glade_xml);

    gtk_main ();

    exit (EXIT_SUCCESS);
}
