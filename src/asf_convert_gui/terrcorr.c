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

void terrcorr_options_changed()
{
  GtkWidget *terrcorr_vbox;
  GtkWidget *dem_checkbutton;

  GtkWidget *terrcorr_checkbutton;
  GtkWidget *tc_pixel_size_checkbutton;
  GtkWidget *refine_geolocation_checkbutton;
  GtkWidget *interpolate_checkbutton;

  gboolean dem_is_checked;

  terrcorr_vbox = glade_xml_get_widget(glade_xml, "terrcorr_vbox");
  dem_checkbutton =
    glade_xml_get_widget(glade_xml, "dem_checkbutton");

  dem_is_checked =
    gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dem_checkbutton));

  gtk_widget_set_sensitive(terrcorr_vbox, dem_is_checked);

  terrcorr_checkbutton =
      glade_xml_get_widget(glade_xml, "terrcorr_checkbutton");
  tc_pixel_size_checkbutton =
      glade_xml_get_widget(glade_xml, "tc_pixel_size_checkbutton");
  refine_geolocation_checkbutton =
      glade_xml_get_widget(glade_xml, "refine_geolocation_checkbutton");
  interpolate_checkbutton =
      glade_xml_get_widget(glade_xml, "interpolate_checkbutton");

  if (dem_is_checked) {
      GtkWidget *hbox_tc_pixel_size;
      gboolean tc_pixel_size_is_checked;
      gboolean terrcorr_is_checked;

      terrcorr_is_checked =
          gtk_toggle_button_get_active(
              GTK_TOGGLE_BUTTON(terrcorr_checkbutton));

      gtk_widget_set_sensitive(tc_pixel_size_checkbutton, terrcorr_is_checked);

      tc_pixel_size_is_checked = terrcorr_is_checked &&
          gtk_toggle_button_get_active(
              GTK_TOGGLE_BUTTON(tc_pixel_size_checkbutton));

      hbox_tc_pixel_size =
          glade_xml_get_widget(glade_xml, "hbox_tc_pixel_size");

      gtk_widget_set_sensitive(hbox_tc_pixel_size, tc_pixel_size_is_checked);

      gtk_widget_set_sensitive(interpolate_checkbutton,
                               terrcorr_is_checked);

      gtk_widget_set_sensitive(refine_geolocation_checkbutton,
                               !terrcorr_is_checked);
      if (terrcorr_is_checked)
          gtk_toggle_button_set_active(
              GTK_TOGGLE_BUTTON(refine_geolocation_checkbutton), TRUE);
  }
  else
  {
      gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(terrcorr_checkbutton), FALSE);
      gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(tc_pixel_size_checkbutton), FALSE);
      gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(refine_geolocation_checkbutton), FALSE);
      gtk_toggle_button_set_active(
          GTK_TOGGLE_BUTTON(interpolate_checkbutton), FALSE);
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
on_terrcorr_checkbutton_toggled(GtkWidget * widget)
{
    terrcorr_options_changed();
    update_summary();

    GtkWidget *terrcorr_checkbutton;
    GtkWidget *interpolate_checkbutton;
    gboolean terrcorr_is_checked;

    terrcorr_checkbutton =
        glade_xml_get_widget(glade_xml, "terrcorr_checkbutton");
    interpolate_checkbutton =
        glade_xml_get_widget(glade_xml, "interpolate_checkbutton");

    terrcorr_is_checked =
        gtk_toggle_button_get_active(
            GTK_TOGGLE_BUTTON(terrcorr_checkbutton));

    gtk_toggle_button_set_active(
        GTK_TOGGLE_BUTTON(interpolate_checkbutton), terrcorr_is_checked);
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
    dem_entry = glade_xml_get_widget(glade_xml, "dem_entry");
    gtk_entry_set_text(GTK_ENTRY(dem_entry), file);
    return TRUE;
}

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

#else
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "dem_file_selection");

    gtk_widget_show(file_selection_dialog);
#endif
}

static void
hide_dem_file_selection_dialog()
{
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "dem_file_selection");

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

    file_selection_dialog =
        glade_xml_get_widget(glade_xml, "dem_file_selection");

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

    if (i != n)
    {
        if (n == 1 || i == 0)
        {
            message_box("Error: Unrecognized extension.");
        }
        else
        {
            message_box("Some of the files were not added -- unknown extensions.");
        }
    }

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);
}

void
hide_dem_file_chooser_dialog()
{
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "dem_file_chooser");

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

    file_chooser_dialog =
        glade_xml_get_widget(glade_xml, "dem_file_chooser");

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

    if (i != n)
    {
        if (n == 1 || i == 0)
        {
            message_box("Error: Unrecognized extension.");
        }
        else
        {
            message_box("Some of the files were not added -- unknown extensions.");
        }
    }

    g_slist_free(selections);
    gtk_widget_hide(file_chooser_dialog);
}

const char * terrcorr_options_string(const Settings *settings)
{
    return "";
}
