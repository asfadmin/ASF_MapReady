#include "asf_convert_gui.h"

static void set_combobox_entry_maxlen(const char *widget_name, int maxlen)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    gtk_entry_set_max_length(e, maxlen);
}

static void set_combobox_items_radar(const char *widget_name)
{
    GtkComboBox *w = GTK_COMBO_BOX(get_widget_checked(widget_name));
    
    gtk_combo_box_remove_text(w, 0);
    gtk_combo_box_append_text(w, "-");
    gtk_combo_box_append_text(w, "HH");
    gtk_combo_box_append_text(w, "HV");
    gtk_combo_box_append_text(w, "VH");
    gtk_combo_box_append_text(w, "VV");
}

static void set_combobox_items_optical(const char *widget_name)
{
    GtkComboBox *w = GTK_COMBO_BOX(get_widget_checked(widget_name));

    gtk_combo_box_remove_text(w, 0);
    gtk_combo_box_append_text(w, "-");
    gtk_combo_box_append_text(w, "1");
    gtk_combo_box_append_text(w, "2");
    gtk_combo_box_append_text(w, "3");
    gtk_combo_box_append_text(w, "4");
}

void setup_band_comboboxes()
{
    set_combobox_items_radar("red_radar_combo");
    set_combobox_items_radar("green_radar_combo");
    set_combobox_items_radar("blue_radar_combo");

    set_combobox_items_optical("red_optical_combo");
    set_combobox_items_optical("green_optical_combo");
    set_combobox_items_optical("blue_optical_combo");

    set_combobox_entry_maxlen("red_optical_combo", 8);
    set_combobox_entry_maxlen("green_optical_combo", 8);
    set_combobox_entry_maxlen("blue_optical_combo", 8);

    set_combobox_entry_maxlen("red_radar_combo", 8);
    set_combobox_entry_maxlen("green_radar_combo", 8);
    set_combobox_entry_maxlen("blue_radar_combo", 8);
}

void
set_combo_box_item(GtkWidget * drop_down_list, gint index)
{
#ifdef USE_GTK_22
    gtk_option_menu_set_history(GTK_OPTION_MENU(drop_down_list), index);
#else
    gtk_combo_box_set_active(GTK_COMBO_BOX(drop_down_list), index);
#endif
}

void
set_combo_box_item_checked(const char *widget_name, gint index)
{
    GtkWidget *ddl = get_widget_checked(widget_name);
    gtk_combo_box_set_active(GTK_COMBO_BOX(ddl), index);
}

void
rb_select(const char *widget_name, gboolean is_on)
{
    GtkWidget *rb = get_widget_checked(widget_name);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rb), is_on);
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

gchar *
meta_file_name(const gchar * data_file_name)
{
    if (has_prepension(data_file_name))
    {
        // data file is really the meta file, for ALOS
        return g_strdup(data_file_name);
    }

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

    return g_strdup("");
}

char *
getPath(const char *in)
{
  char *dir = malloc(sizeof(char)*(strlen(in) + 2));
  char *file = malloc(sizeof(char)*(strlen(in) + 2));

  split_dir_and_file(in, dir, file);
  free(file);

  if (dir[strlen(dir) - 1] == DIR_SEPARATOR)
      dir[strlen(dir) - 1] = '\0';

  return dir;
}

GtkWidget *get_widget_checked(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_widget_checked() failed: "
            "The widget %s was not found.\n", widget_name);
    }
    return w;
}
