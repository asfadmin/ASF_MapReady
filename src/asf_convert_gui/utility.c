#include "asf_convert_gui.h"

void
message_box(gchar * message)
{
  GtkWidget *dialog, *label;

  dialog = gtk_dialog_new_with_buttons( "Message",
	NULL,
	GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
	GTK_STOCK_OK,
	GTK_RESPONSE_NONE,
	NULL);

  label = gtk_label_new(message);

  g_signal_connect_swapped(dialog, 
			   "response", 
			   G_CALLBACK(gtk_widget_destroy),
			   dialog);

  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

  gtk_widget_show_all(dialog);
}
