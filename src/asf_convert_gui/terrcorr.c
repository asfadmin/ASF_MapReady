#ifdef win32

/* ugly hack here... windef.h and asf_meta.h both define a BYTE symbol. */
/* since we don't use the BYTE from asf_meta.h here, we'll #define BYTE */
/* to something else during the processing of that header, leaving BYTE */
/* defined in windef.h alone (that's the one we want)                   */

#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif

#include "asf_convert_gui.h"

void default_to_terrcorr_on()
{
// The "initially on" value on the .glade file for the terrain correction
// radio button doesn't seem to be working... turn it on manually.
  GtkWidget *rb_terrcorr;
  rb_terrcorr = get_widget_checked("rb_terrcorr");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rb_terrcorr), TRUE);
}

void terrcorr_options_changed()
{
  GtkWidget *terrcorr_vbox;
  GtkWidget *dem_checkbutton;
  GtkWidget *hbox_terrcorr_items;

  hbox_terrcorr_items = get_widget_checked("hbox_terrcorr_items");

  gboolean dem_is_checked;

  terrcorr_vbox = get_widget_checked("terrcorr_vbox");
  dem_checkbutton = get_widget_checked("dem_checkbutton");

  dem_is_checked =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dem_checkbutton));

  gtk_widget_set_sensitive(terrcorr_vbox, dem_is_checked);

  if (dem_is_checked) {
      GtkWidget *rb_terrcorr;
      GtkWidget *hbox_tc_pixel_size;
      GtkWidget *rb_mask_file, *rb_auto_water_mask;
      GtkWidget *tc_pixel_size_checkbutton;
      GtkWidget *interpolate_checkbutton;
      GtkWidget *mask_checkbutton, *mask_entry;
      GtkWidget *radiometric_checkbutton;
      GtkWidget *layover_mask_checkbutton;
      GtkWidget *save_dem_checkbutton;

      gboolean tc_pixel_size_is_checked;
      gboolean terrcorr_is_checked;
      gboolean mask_is_checked, mask_file_is_checked;

      gtk_widget_set_sensitive(hbox_terrcorr_items, TRUE);

      rb_terrcorr = get_widget_checked("rb_terrcorr");
      terrcorr_is_checked =
          gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(rb_terrcorr));

      rb_mask_file = get_widget_checked("rb_mask_file");
      rb_auto_water_mask = get_widget_checked("rb_auto_water_mask");
      mask_checkbutton = get_widget_checked("mask_checkbutton");
      mask_entry = get_widget_checked("mask_entry");

      mask_is_checked =
          gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(mask_checkbutton));

      mask_file_is_checked = mask_is_checked &&
          gtk_toggle_button_get_active(
              GTK_TOGGLE_BUTTON(rb_mask_file));

      tc_pixel_size_checkbutton =
          get_widget_checked("tc_pixel_size_checkbutton");

      tc_pixel_size_is_checked = terrcorr_is_checked &&
          gtk_toggle_button_get_active(
              GTK_TOGGLE_BUTTON(tc_pixel_size_checkbutton));

      hbox_tc_pixel_size = get_widget_checked("hbox_tc_pixel_size");

      gtk_widget_set_sensitive(hbox_tc_pixel_size, tc_pixel_size_is_checked);
      gtk_widget_set_sensitive(mask_entry, mask_file_is_checked);
      //gtk_widget_set_sensitive(hbox_terrcorr_items, terrcorr_is_checked);
      gtk_widget_set_sensitive(rb_auto_water_mask, mask_is_checked);
      gtk_widget_set_sensitive(rb_mask_file, mask_is_checked);

      interpolate_checkbutton = get_widget_checked("interpolate_checkbutton");
      radiometric_checkbutton = get_widget_checked("radiometric_checkbutton");
      save_dem_checkbutton = get_widget_checked("save_dem_checkbutton");
      layover_mask_checkbutton =
          get_widget_checked("layover_mask_checkbutton");

      gtk_widget_set_sensitive(tc_pixel_size_checkbutton, terrcorr_is_checked);
      gtk_widget_set_sensitive(interpolate_checkbutton, terrcorr_is_checked);
      gtk_widget_set_sensitive(radiometric_checkbutton, terrcorr_is_checked);
      gtk_widget_set_sensitive(layover_mask_checkbutton, terrcorr_is_checked);
      gtk_widget_set_sensitive(save_dem_checkbutton, terrcorr_is_checked);
  }
  else
  {
      gtk_widget_set_sensitive(hbox_terrcorr_items, FALSE);
  }

  // must now update the geocode settings as well, since the average
  // height checkbutton is disabled by terrain correction
  geocode_options_changed();
}

SIGNAL_CALLBACK void
on_dem_checkbutton_toggled(GtkWidget * widget)
{
    terrcorr_options_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_mask_checkbutton_toggled(GtkWidget * widget)
{
    terrcorr_options_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_refine_geolocation_checkbutton_toggled(GtkWidget * widget)
{
    terrcorr_options_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_tc_pixel_size_checkbutton_toggled(GtkWidget *widget)
{
    terrcorr_options_changed();
    update_summary();
}

int set_dem_file(const char *file)
{
    GtkWidget *dem_entry;
    dem_entry = get_widget_checked("dem_entry");
    gtk_entry_set_text(GTK_ENTRY(dem_entry), file);
    return TRUE;
}

int set_mask_file(const char *file)
{
    GtkWidget *mask_entry;
    mask_entry = get_widget_checked("mask_entry");
    gtk_entry_set_text(GTK_ENTRY(mask_entry), file);
    return TRUE;
}

#ifdef USE_GTK_FILE_CHOOSER

static GtkWidget *dem_browse_widget = NULL;
static GtkWidget *mask_browse_widget = NULL;

static SIGNAL_CALLBACK void dem_cancel_clicked()
{
    gtk_widget_hide(dem_browse_widget);
}

static SIGNAL_CALLBACK void mask_cancel_clicked()
{
    gtk_widget_hide(mask_browse_widget);
}

static SIGNAL_CALLBACK void dem_ok_clicked()
{
    gchar *file = gtk_file_chooser_get_filename(
        GTK_FILE_CHOOSER(dem_browse_widget));

    gtk_widget_hide(dem_browse_widget);

    if (file)
        set_dem_file(file);
}

static SIGNAL_CALLBACK void mask_ok_clicked()
{
    gchar *file = gtk_file_chooser_get_filename(
        GTK_FILE_CHOOSER(mask_browse_widget));

    gtk_widget_hide(mask_browse_widget);

    if (file)
        set_mask_file(file);
}

static void create_file_chooser_dialogs() 
{
    GtkWidget *parent = get_widget_checked("asf_convert");

    {
        dem_browse_widget = gtk_file_chooser_dialog_new(
            "Open DEM", GTK_WINDOW(parent),
            GTK_FILE_CHOOSER_ACTION_OPEN,
            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
            GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
            NULL);
        
        // we need to extract the buttons, so we can connect them to our
        // button handlers, above
        GtkHButtonBox *box = 
            (GtkHButtonBox*)(((GtkDialog*)dem_browse_widget)->action_area);
        GList *buttons = box->button_box.box.children;
        
        GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
        GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;
        
        g_signal_connect((gpointer)cancel_btn, "clicked",
                         G_CALLBACK(dem_cancel_clicked), NULL);
        g_signal_connect((gpointer)ok_btn, "clicked",
                         G_CALLBACK(dem_ok_clicked), NULL);
        
        // add the filters
        GtkFileFilter *img_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(img_filt, "DEM Image Files (*.img)");
        gtk_file_filter_add_pattern(img_filt, "*.img");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dem_browse_widget),
                                    img_filt);

        GtkFileFilter *tif_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(tif_filt, "GeoTIFF Files (*.tif)");
        gtk_file_filter_add_pattern(tif_filt, "*.tif");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dem_browse_widget),
                                    tif_filt);

        GtkFileFilter *all_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(all_filt, "All Files (*.*)");
        gtk_file_filter_add_pattern(all_filt, "*");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dem_browse_widget),
                                    all_filt);

        // we need to make these modal -- if the user opens multiple "open"
        // dialogs, we'll get confused on the callbacks
        gtk_window_set_modal(GTK_WINDOW(dem_browse_widget), TRUE);
        gtk_window_set_destroy_with_parent(GTK_WINDOW(dem_browse_widget),
                                           TRUE);
        gtk_dialog_set_default_response(GTK_DIALOG(dem_browse_widget),
                                        GTK_RESPONSE_OK);
    }

    {
        mask_browse_widget = gtk_file_chooser_dialog_new(
            "Open DEM", GTK_WINDOW(parent),
            GTK_FILE_CHOOSER_ACTION_OPEN,
            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
            GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
            NULL);
        
        // we need to extract the buttons, so we can connect them to our
        // button handlers, above
        GtkHButtonBox *box = 
            (GtkHButtonBox*)(((GtkDialog*)mask_browse_widget)->action_area);
        GList *buttons = box->button_box.box.children;
        
        GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
        GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;
        
        g_signal_connect((gpointer)cancel_btn, "clicked",
                         G_CALLBACK(mask_cancel_clicked), NULL);
        g_signal_connect((gpointer)ok_btn, "clicked",
                         G_CALLBACK(mask_ok_clicked), NULL);
        
        // add the filters
        GtkFileFilter *img_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(img_filt, "DEM Image Files (*.img)");
        gtk_file_filter_add_pattern(img_filt, "*.img");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(mask_browse_widget),
                                    img_filt);

        GtkFileFilter *tif_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(tif_filt, "GeoTIFF Files (*.tif)");
        gtk_file_filter_add_pattern(tif_filt, "*.tif");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(mask_browse_widget),
                                    tif_filt);

        GtkFileFilter *all_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(all_filt, "All Files (*.*)");
        gtk_file_filter_add_pattern(all_filt, "*");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(mask_browse_widget),
                                    all_filt);

        // we need to make these modal -- if the user opens multiple "open"
        // dialogs, we'll get confused on the callbacks
        gtk_window_set_modal(GTK_WINDOW(mask_browse_widget), TRUE);
        gtk_window_set_destroy_with_parent(GTK_WINDOW(mask_browse_widget),
                                           TRUE);
        gtk_dialog_set_default_response(GTK_DIALOG(mask_browse_widget),
                                        GTK_RESPONSE_OK);
    }
}

#endif

SIGNAL_CALLBACK void
on_dem_browse_button_clicked(GtkWidget *widget)
{
#ifdef win32
    OPENFILENAME of;
    int retval;
    char fname[1024];

    fname[0] = '\0';

    memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
    of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
    of.lStructSize = sizeof(of);
#endif

    of.hwndOwner = NULL;
    of.lpstrFilter = "DEM Imagery Files (*.img)\0*.img\0"
        "GeoTIFF Files (*.tif)\0*.tif\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    /* the returned "fname" has the following form:            */
    /*   <directory>\0<first file>\0<second file>\0<third ...  */ 
    char * dir = strdup(fname);
    char * p = fname + strlen(dir) + 1;

    if (*p) { 
        while (*p) {
            char * dir_and_file = malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
            sprintf(dir_and_file, "%s%c%s", dir, DIR_SEPARATOR, p);
            set_dem_file(dir_and_file);
            p += strlen(p) + 1;
            free(dir_and_file);
        }
    } else {
        set_dem_file(dir);
    }

    free(dir);

#else // #ifdef win32

    /* Linux version -- use GtkFileChooser if possible */

#ifdef USE_GTK_FILE_CHOOSER

    if (!dem_browse_widget)
        create_file_chooser_dialogs();

    gtk_widget_show(dem_browse_widget);

#else // #ifdef USE_GTK_FILE_CHOOSER

    GtkWidget *file_selection_dialog =
        get_widget_checked("dem_file_selection");

    gtk_widget_show(file_selection_dialog);

#endif // #ifdef USE_GTK_FILE_CHOOSER
#endif // #ifdef win32
}

static void
hide_dem_file_selection_dialog()
{
    GtkWidget *file_selection_dialog =
        get_widget_checked("dem_file_selection");

    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_dem_file_selection_cancel_button_clicked(GtkWidget *widget)
{
    hide_dem_file_selection_dialog();
}

SIGNAL_CALLBACK gboolean
on_dem_file_selection_delete_event(GtkWidget *w)
{
    hide_dem_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_dem_file_selection_destroy_event(GtkWidget *w)
{
    hide_dem_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_dem_file_selection_destroy(GtkWidget *w)
{
    hide_dem_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_dem_file_selection_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_selection_dialog;
    gchar **selections;
    gchar **current;
    int i, n;

    file_selection_dialog = get_widget_checked("dem_file_selection");

    selections = gtk_file_selection_get_selections(
        GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;
    i = n = 0;

    while (*current)
    {
        if (set_dem_file(*current))
            ++i;

        ++current;
        ++n;
    }

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);
}

void
hide_dem_file_chooser_dialog()
{
    GtkWidget *file_selection_dialog = get_widget_checked("dem_file_chooser");
    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_dem_file_chooser_cancel_button_clicked(GtkWidget *widget)
{
    hide_dem_file_chooser_dialog();
}

SIGNAL_CALLBACK gboolean
on_dem_file_chooser_delete_event(GtkWidget *w)
{
    hide_dem_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_dem_file_chooser_destroy_event(GtkWidget *w)
{
    hide_dem_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_dem_file_chooser_destroy(GtkWidget *w)
{
    hide_dem_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_dem_file_chooser_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_chooser_dialog;
    GSList *selections;
    GSList *current;
    int i, n;

    file_chooser_dialog = get_widget_checked("dem_file_chooser");

    selections =
        gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(file_chooser_dialog));

    current = selections;
    i = n = 0;

    while (current)
    {
        gchar * file = (gchar *) current->data;

        /* second clause here allows silent fail for .L files, PR 92 */
        if (set_dem_file(file))
            ++i;

        current = g_slist_next(current);
        ++n;

        g_free(file);
    }

    g_slist_free(selections);
    gtk_widget_hide(file_chooser_dialog);
}

SIGNAL_CALLBACK void
on_mask_browse_button_clicked(GtkWidget *widget)
{
#ifdef win32
    OPENFILENAME of;
    int retval;
    char fname[1024];

    fname[0] = '\0';

    memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
    of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
    of.lStructSize = sizeof(of);
#endif

    of.hwndOwner = NULL;
    of.lpstrFilter = "Mask Files (*.img)\0*.img\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    /* the returned "fname" has the following form:            */
    /*   <directory>\0<first file>\0<second file>\0<third ...  */ 
    char * dir = strdup(fname);
    char * p = fname + strlen(dir) + 1;

    if (*p) { 
        while (*p) {
            char * dir_and_file = malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
            sprintf(dir_and_file, "%s%c%s", dir, DIR_SEPARATOR, p);
            set_mask_file(dir_and_file);
            p += strlen(p) + 1;
            free(dir_and_file);
        }
    } else {
        set_mask_file(dir);
    }

    free(dir);

#else // #ifdef win32

    /* Linux version -- use GtkFileChooser if possible */

#ifdef USE_GTK_FILE_CHOOSER

    if (!mask_browse_widget)
        create_file_chooser_dialogs();

    gtk_widget_show(mask_browse_widget);

#else // #ifdef USE_GTK_FILE_CHOOSER

    GtkWidget *file_selection_dialog =
        get_widget_checked("mask_file_selection");

    gtk_widget_show(file_selection_dialog);

#endif // #ifdef USE_GTK_FILE_CHOOSER
#endif // #ifdef win32
}

static void
hide_mask_file_selection_dialog()
{
    GtkWidget *file_selection_dialog =
        get_widget_checked("mask_file_selection");

    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_mask_file_selection_cancel_button_clicked(GtkWidget *widget)
{
    hide_mask_file_selection_dialog();
}

SIGNAL_CALLBACK gboolean
on_mask_file_selection_delete_event(GtkWidget *w)
{
    hide_mask_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_mask_file_selection_destroy_event(GtkWidget *w)
{
    hide_mask_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_mask_file_selection_destroy(GtkWidget *w)
{
    hide_mask_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_mask_file_selection_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_selection_dialog;
    gchar **selections;
    gchar **current;
    int i, n;

    file_selection_dialog = get_widget_checked("mask_file_selection");

    selections = gtk_file_selection_get_selections(
        GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;
    i = n = 0;

    while (*current)
    {
        if (set_mask_file(*current))
            ++i;

        ++current;
        ++n;
    }

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);
}

void
hide_mask_file_chooser_dialog()
{
    GtkWidget *file_selection_dialog = get_widget_checked("mask_file_chooser");
    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_mask_file_chooser_cancel_button_clicked(GtkWidget *widget)
{
    hide_mask_file_chooser_dialog();
}

SIGNAL_CALLBACK gboolean
on_mask_file_chooser_delete_event(GtkWidget *w)
{
    hide_mask_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_mask_file_chooser_destroy_event(GtkWidget *w)
{
    hide_mask_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_mask_file_chooser_destroy(GtkWidget *w)
{
    hide_mask_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_mask_file_chooser_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_chooser_dialog;
    GSList *selections;
    GSList *current;
    int i, n;

    file_chooser_dialog = get_widget_checked("mask_file_chooser");

    selections =
        gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(file_chooser_dialog));

    current = selections;
    i = n = 0;

    while (current)
    {
        gchar * file = (gchar *) current->data;

        if (set_mask_file(file))
            ++i;

        current = g_slist_next(current);
        ++n;

        g_free(file);
    }

    g_slist_free(selections);
    gtk_widget_hide(file_chooser_dialog);
}

const char * terrcorr_options_string(const Settings *settings)
{
    return "";
}

SIGNAL_CALLBACK void
on_rb_refine_geolocation_toggled(GtkWidget *widget)
{
    terrcorr_options_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_terrcorr_toggled(GtkWidget *widget)
{
    terrcorr_options_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_auto_water_mask_toggled(GtkWidget *widget)
{
    terrcorr_options_changed();
    update_summary();
}

SIGNAL_CALLBACK void
on_rb_mask_file_toggled(GtkWidget *widget)
{
    terrcorr_options_changed();
    update_summary();
}

