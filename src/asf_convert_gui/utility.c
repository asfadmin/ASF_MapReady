#include "asf_convert_gui.h"

void
set_combo_box_item(GtkWidget * drop_down_list, gint index)
{
#ifdef USE_GTK_22
  gtk_option_menu_set_history(GTK_OPTION_MENU(drop_down_list), index);
#else
  gtk_combo_box_set_active(GTK_COMBO_BOX(drop_down_list), index);
#endif
}

gint
get_combo_box_item(GtkWidget * drop_down_list)
{
#ifdef USE_GTK_22
  return gtk_option_menu_get_history(GTK_OPTION_MENU(drop_down_list));
#else
  return gtk_combo_box_get_active(GTK_COMBO_BOX(drop_down_list));
#endif
}

void
message_box(const gchar * message)
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

/* --- this appears to no longer be needed ---
gchar *
meta_file_name(gchar * data_file_name)
{
  gchar * p = strrchr(data_file_name, '.');
  if (!p)
  {
    gchar * ret =
      (gchar *) g_malloc (sizeof(gchar) * (strlen(data_file_name) + 3));
    
    strcpy(ret, data_file_name);
    strcat(ret, ".L");
    return ret;
  }

  if (strcmp(p + 1, "D") == 0)
  {
    gchar * ret = g_strdup(data_file_name);
    ret[strlen(data_file_name) - 1] = 'L';
    return ret;
  }

  if (strcmp(p + 1, ".img") == 0)
  {
    gchar * ret =
      (gchar *) g_malloc(sizeof(gchar) * (strlen(data_file_name) + 2));
    strcpy(ret, data_file_name);
    *(ret + (data_file_name - p + 1)) = '\0';
    strcat(ret, ".meta");    
    return ret;
  }

  return "";
}
*/
