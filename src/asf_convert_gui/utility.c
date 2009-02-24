#include "asf_convert_gui.h"

static void set_combobox_entry_maxlen(const char *widget_name, int maxlen)
{
    GtkWidget *w = get_widget_checked(widget_name);
    GtkEntry *e = GTK_ENTRY (GTK_BIN (w)->child);
    gtk_entry_set_max_length(e, maxlen);
}

static void set_combobox_items_user_defined(const char *widget_name)
{
    GtkComboBox *w = GTK_COMBO_BOX(get_widget_checked(widget_name));

    gtk_combo_box_remove_text(w, 0);
    gtk_combo_box_append_text(w, "-");
    gtk_combo_box_append_text(w, "HH");
    gtk_combo_box_append_text(w, "HV");
    gtk_combo_box_append_text(w, "VH");
    gtk_combo_box_append_text(w, "VV");
    gtk_combo_box_append_text(w, "1");
    gtk_combo_box_append_text(w, "2");
    gtk_combo_box_append_text(w, "3");
    gtk_combo_box_append_text(w, "4");
    gtk_combo_box_append_text(w, "Entropy");
    gtk_combo_box_append_text(w, "Anisotropy");
    gtk_combo_box_append_text(w, "Alpha");
}

void setup_band_comboboxes()
{
    set_combobox_items_user_defined("red_combo");
    set_combobox_items_user_defined("green_combo");
    set_combobox_items_user_defined("blue_combo");

    set_combobox_entry_maxlen("red_combo", 8);
    set_combobox_entry_maxlen("green_combo", 8);
    set_combobox_entry_maxlen("blue_combo", 8);
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

double get_double_from_entry(const char *widget_name)
{
    GtkWidget *e = get_widget_checked(widget_name);
    return atof(gtk_entry_get_text(GTK_ENTRY(e)));
}

void put_double_to_entry(const char *widget_name, double val)
{
    GtkWidget *e = get_widget_checked(widget_name);

    char tmp[64];
    sprintf(tmp, "%f", val);

    gtk_entry_set_text(GTK_ENTRY(e), tmp);
}

int get_int_from_entry(const char *widget_name)
{
    GtkWidget *e = get_widget_checked(widget_name);
    return atoi(gtk_entry_get_text(GTK_ENTRY(e)));
}

void put_int_to_entry(const char *widget_name, int val)
{
    GtkWidget *e = get_widget_checked(widget_name);

    char tmp[64];
    sprintf(tmp, "%d", val);

    gtk_entry_set_text(GTK_ENTRY(e), tmp);
}

void put_string_to_entry(const char *widget_name, const char *txt)
{
    GtkWidget *e = get_widget_checked(widget_name);
    gtk_entry_set_text(GTK_ENTRY(e), txt);
}

int get_checked(const char *widget_name)
{
    GtkWidget *cb = get_widget_checked(widget_name);
    return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cb));
}

void set_checked(const char *widget_name, int checked)
{
    GtkWidget *cb = get_widget_checked(widget_name);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(cb), checked);
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

    gchar *msg = g_new(gchar, strlen(message) + 16);
    if (msg) {
        sprintf(msg, "\n  %s   \n", message);
        label = gtk_label_new(msg);
        g_free(msg);
    }
    else {
        label = gtk_label_new(message);
    }

    g_signal_connect_swapped(dialog,
        "response",
        G_CALLBACK(gtk_widget_destroy),
        dialog);

    gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

    gtk_widget_show_all(dialog);

    // Seems that sometimes the message box ends up hidden behind other
    // windows... this might bring it to the front
    gtk_window_present(GTK_WINDOW(dialog));
}

gchar *
meta_file_name(const gchar * file_name)
{
  // first, handle ASF Internal
  char *ext = findExt(file_name);
  if (ext && strcmp_case(ext, ".meta")==0) {
    return g_strdup(file_name);
  }
  else if (ext && strcmp_case(ext, ".img")==0) {
    char *tmp = appendExt(file_name, ".meta");
    gchar *ret = g_strdup(tmp);
    FREE(tmp);
    return ret;
  }

  // airsar
  else if (ext && strcmp_case(ext, ".airsar")==0) {
    return g_strdup(file_name);
  }

  // geotiff
  else if (ext && (strcmp_case(ext, ".tif") == 0 || strcmp_case(ext, ".tiff") == 0)) {
      return g_strdup(file_name);
  }

  // second, try CEOS
  char *basename = MALLOC(sizeof(char)*(strlen(file_name)+10));
  char **dataName = NULL, **metaName = NULL;
  int nBands, trailer;

  ceos_file_pairs_t s = get_ceos_names(file_name, basename,
                            &dataName, &metaName, &nBands, &trailer);

  gchar *ret;
  if (s != NO_CEOS_FILE_PAIR && nBands > 0) {
    ret = g_strdup(metaName[0]);
  }
  else {
    // not found
    ret = g_strdup("");
  }

  FREE(basename);
  free_ceos_names(dataName, metaName);

  return ret;
}

gchar *
data_file_name(const gchar * file_name)
{
  // first, handle ASF Internal
  char *ext = findExt(file_name);
  if (ext && strcmp_case(ext, ".img")==0) {
    return g_strdup(file_name);
  }
  else if (ext && strcmp_case(ext, ".meta")==0) {
    char *tmp = appendExt(file_name, ".img");
    gchar *ret = g_strdup(tmp);
    FREE(tmp);
    return ret;
  }

  // PolSARpro
  else if (ext && strcmp_case(ext, ".bin")==0) {
    return g_strdup(file_name);
  }

  // airsar
  else if (ext && strcmp_case(ext, ".airsar")==0) {
    char *data_name = STRDUP(file_name);
    char *p = strstr(data_name, "_meta.airsar");
    if (p) {
      // c-band first
      *p = '\0';
      strcat(data_name, "_c.vvi2");
      if (fileExists(data_name)) {
        gchar *ret = g_strdup(data_name);
        FREE(data_name);
        return ret;
      }

      // l-band next
      *p = '\0';
      strcat(data_name, "_l.vvi2");
      if (fileExists(data_name)) {
        gchar *ret = g_strdup(data_name);
        FREE(data_name);
        return ret;
      }

      // c-band polarimetric next
      *p = '\0';
      strcat(data_name, "_c.dat");
      if (fileExists(data_name)) {
        gchar *ret = g_strdup(data_name);
        FREE(data_name);
        return ret;
      }

      // l-band polarimetric next
      *p = '\0';
      strcat(data_name, "_l.dat");
      if (fileExists(data_name)) {
        gchar *ret = g_strdup(data_name);
        FREE(data_name);
        return ret;
      }

      // p-band next
      *p = '\0';
      strcat(data_name, "_p.dat");
      if (fileExists(data_name)) {
        gchar *ret = g_strdup(data_name);
        FREE(data_name);
        return ret;
      }

      // ?
      FREE(data_name);
      return g_strdup("");
    }
    else {
      // ?
      FREE(data_name);
      return g_strdup("");
    }
  }

  // geotiff
  else if (ext && (strcmp_case(ext, ".tif") == 0 || strcmp_case(ext, ".tiff") == 0)) {
      return g_strdup(file_name);
  }

  // second, try CEOS
  char *basename = MALLOC(sizeof(char)*(strlen(file_name)+10));
  char **dataName = NULL, **metaName = NULL;
  int nBands, trailer;

  ceos_file_pairs_t s = get_ceos_names(file_name, basename,
                            &dataName, &metaName, &nBands, &trailer);

  gchar *ret;
  if (s != NO_CEOS_FILE_PAIR && nBands > 0) {
    ret = g_strdup(dataName[0]);
  }
  else {
    // not found
    ret = g_strdup("");
  }

  FREE(basename);
  free_ceos_names(dataName, metaName);

  return ret;
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

void enable_widget(const char *widget_name, int enable)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_widget_set_sensitive(w, enable);
}

void put_string_to_label(const char *widget_name, const char *txt)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_label_set_text(GTK_LABEL(w), txt);
}

const char *get_string_from_label(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    return gtk_label_get_text(GTK_LABEL(w));
}

/* Returns true if a PolSARpro file set is detected based on the */
/* filename passed in.                                           */
gboolean is_polsarpro(const gchar * infile)
{
  gboolean found_bin = FALSE;
  gboolean found_bin_hdr = FALSE;
  char *bin = NULL, *bin_hdr = NULL, *dupe = NULL, *ext = NULL;

  ext = findExt(infile);
  if (!ext) {
    // If no file extension exists, then maybe it has been stripped
    // off.  Guess .bin and check for existence...
    char *inFile = (char *)MALLOC(sizeof(char) * strlen(infile) + 5);
    sprintf(inFile, "%s.bin", infile);
    gboolean ret = is_polsarpro(inFile);
    FREE(inFile);
    return ret;
  }
  if (strcmp_case(ext, ".bin")==0) {
    bin = (char *)infile;
    bin_hdr = (char *)MALLOC(sizeof(char) * (strlen(infile) + 5));
    sprintf(bin_hdr, "%s.hdr", infile);
    found_bin = fileExists(bin);
    found_bin_hdr = fileExists(bin_hdr);
    FREE(bin_hdr);
  }
  else if (strcmp_case(ext, ".hdr")==0) {
    dupe = STRDUP(infile);
    bin_hdr = (char *)infile;
    ext = findExt(dupe);
    *ext = '\0';
    ext = findExt(dupe);
    if (ext && (strcmp_case(ext, ".bin")==0)) {
      bin = dupe;
    }
    found_bin = (gboolean)fileExists(bin);
    found_bin_hdr = (gboolean)fileExists(bin_hdr);
    FREE(dupe);
  }

  return (found_bin && found_bin_hdr);
}

gboolean is_geotiff(const char *infile)
{
  char *ext = findExt(infile);
  if (!ext) {
    return FALSE; // extension must be added
  }
  else if (strcmp_case(ext, ".tif")==0) {
    return TRUE;
  }
  else if (strcmp_case(ext, ".tiff")==0) {
    return TRUE;
  }
  return FALSE;
}

gboolean is_asf_internal(const char *infile)
{
  int ret;
  char *ext = findExt(infile);
  if (!ext) {
    ret = FALSE;
  }
  else if (strcmp_case(ext, ".img")==0) {
    // have .img, make sure we have .meta
    char *meta = appendExt(infile, ".meta");
    ret = fileExists(meta);
    free(meta);
  }
  else if (strcmp_case(ext, ".meta")==0) {
    // have .meta, make sure we have .img
    char *img = appendExt(infile, ".img");
    ret = fileExists(img);
    free(img);
  }
  else
    ret = FALSE;
  return ret;
}

gboolean is_airsar(const char *infile)
{
  // here we must be given the metadata file, only reliable file to check for
  char *ext = findExt(infile);
  if (ext && strcmp_case(ext, ".airsar")==0)
    return TRUE;
  return FALSE;
}

gboolean is_terrasarx(const char *infile)
{
  // not sure exactly what we should do here, for now just check for .xml
  char *ext = findExt(infile);
  if (ext && strcmp_case(ext, ".xml")==0)
    return TRUE;
  return FALSE;  
}
