#include "proj2proj.h"

void img_loc(gchar *loc, gchar* file);
void set_toolbar_images();

/************************************************************************
 * Global variables...
 */

// pointer to the loaded XML file's internal struct
GladeXML *glade_xml;

char *find_in_share(const char * filename)
{
    char * ret = MALLOC(sizeof(char) *
        (strlen(get_asf_share_dir()) + strlen(filename) + 5));
    sprintf(ret, "%s/%s", get_asf_share_dir(), filename);
    return ret;
}

SIGNAL_CALLBACK void
on_proj2proj_main_window_delete_event(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

int
main(int argc, char **argv)
{
    gtk_init(&argc, &argv);

    gchar *glade_xml_file = (gchar *) find_in_share("proj2proj.glade");
    printf("Found proj2proj.glade: %s\n", glade_xml_file);
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);
    free(glade_xml_file);

    set_font();

    glade_xml_signal_autoconnect(glade_xml);

    // default to lat/lon -> UTM
    GtkWidget *w = get_widget_checked("source_projection_option_menu");
    set_combo_box_item(w, 5);

    geocode_options_changed(TRUE);
    geocode_options_changed(FALSE);

    if (argc > 1) {
        put_file_in_textview(argv[1], "source_textview");
        forward();
    }

    set_toolbar_images();

    gtk_main ();

    exit (EXIT_SUCCESS);
}

void img_loc(gchar *loc, gchar* file)
{
  gchar * tmp = find_in_share(file);
  if (tmp) {
    strcpy(loc, tmp);
    g_free(tmp);
  } else {
    strcpy(loc, file);
  }
}
void set_toolbar_images()
{
  GtkWidget *w;
  gchar loc[1024];

  w = get_widget_checked("source_load_button");
  //gtk_button_set_label(GTK_BUTTON(w), " Browse...");
  w = get_widget_checked("source_load_button_image");
  img_loc(loc, "folder_s.png");
  gtk_image_set_from_file(GTK_IMAGE(w), loc);

  w = get_widget_checked("source_save_button");
  //gtk_button_set_label(GTK_BUTTON(w), " Save...");
  w = get_widget_checked("source_save_button_image");
  img_loc(loc, "save_as.png");
  gtk_image_set_from_file(GTK_IMAGE(w), loc);

  w = get_widget_checked("target_load_button");
  //gtk_button_set_label(GTK_BUTTON(w), " Browse...");
  w = get_widget_checked("target_load_button_image");
  img_loc(loc, "folder_s.png");
  gtk_image_set_from_file(GTK_IMAGE(w), loc);

  w = get_widget_checked("target_save_button");
  //gtk_button_set_label(GTK_BUTTON(w), " Save...");
  w = get_widget_checked("target_save_button_image");
  img_loc(loc, "save_as.png");
  gtk_image_set_from_file(GTK_IMAGE(w), loc);
}

