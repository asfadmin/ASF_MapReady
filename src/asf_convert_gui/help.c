#include "asf_convert_gui.h"

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
      gchar buffer[1024];
      gchar *p = fgets(buffer, sizeof(buffer), help_file);
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

void
help_hide()
{
  GtkWidget *help_dialog =
    glade_xml_get_widget(glade_xml, "help_dialog");

  gtk_widget_hide(help_dialog);
}


SIGNAL_CALLBACK void
on_help_dialog_ok_button_clicked(GtkWidget *widget)
{
  help_hide();
}

SIGNAL_CALLBACK gboolean
on_help_dialog_delete_event(GtkWidget *widget)
{
  help_hide();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
on_help_dialog_destroy_event(GtkWidget *widget)
{
  help_hide();
  return TRUE;
}

SIGNAL_CALLBACK gboolean
on_help_dialog_destroy(GtkWidget *widget)
{
  help_hide();
  return TRUE;
}
