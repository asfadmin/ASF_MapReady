#include "asf_convert_gui.h"

static void
target_drag_data_received(GtkWidget *widget,
			  GdkDragContext *context,
			  gint x,
			  gint y,
			  GtkSelectionData *data,
			  guint info,
			  guint time)
{
  printf("Drop!\n");

  gchar ** list, ** iter;
  gchar * delim = "\n";

  list = g_strsplit(data->data, delim, 0);
  iter = list;

  while (*iter)
  {
    gchar * data_file = g_strdup( *iter );
    g_strstrip( data_file );
    
    if (data_file && strlen(data_file) > 0)
    {
      gchar * meta_file;
      gchar * p = data_file;

      if (g_str_has_prefix(p, "file://"))
	p += 7;
      
      meta_file = meta_file_name(p);
    
      add_to_files_list(p, meta_file);

      g_free(meta_file);
    }

    ++iter;
    g_free(data_file);
  }

  g_strfreev(list);
}

static GtkTargetEntry target_table[] = {
  { "text/plain", 0, 0 }
};

void
setup_dnd()
{
  GtkWidget *widget;

  widget = glade_xml_get_widget(glade_xml, "files_list");

  gtk_drag_dest_set (widget, GTK_DEST_DEFAULT_ALL, 
		     target_table, 1, GDK_ACTION_COPY);

  gtk_signal_connect( GTK_OBJECT(widget), "drag_data_received",
		      GTK_SIGNAL_FUNC(target_drag_data_received), NULL );
}
