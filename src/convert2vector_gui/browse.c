#ifdef win32

/* ugly hack here... windef.h and asf_meta.h both define a BYTE symbol. */
/* since we don't use the BYTE from asf_meta.h here, we'll #define BYTE */
/* to something else during the processing of that header, leaving BYTE */
/* defined in windef.h alone (that's the one we want)                   */

#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif

#include "c2v.h"

#ifndef win32

static GtkWidget *input_browse_widget = NULL;
static GtkWidget *output_browse_widget = NULL;

static void add_input_file(char *file)
{
    put_string_to_entry("input_file_entry", file);

    // set the output directory, if it is currently blank, to the
    // same as the input directory
    if (strlen(get_string_from_entry("output_directory_entry"))==0) {
      char *dir = get_dirname(file);
      put_string_to_entry("output_directory_entry", dir);
      free(dir);
    }

    // change output filename, regardless
    char *ext=NULL;
    int output_format = get_combo_box_item("output_format_combobox");
    switch (output_format) {
      case OUTPUT_FORMAT_KML:
        ext=".kml";
        break;
      default:
        break;
    }

    if (ext) {
      char *s = get_filename(file);
      char *out_file = appendExt(s, ext);
      put_string_to_entry("output_file_entry", out_file);
      free(out_file);
      free(s);
    }
}

// called when "cancel" clicked on the GtkFileChooser
SIGNAL_CALLBACK void input_browse_cancel_clicked()
{
    gtk_widget_hide(input_browse_widget);
}

SIGNAL_CALLBACK void output_browse_cancel_clicked()
{
    gtk_widget_hide(output_browse_widget);
}

// called when "ok" clicked on the GtkFileChooser
SIGNAL_CALLBACK void input_browse_ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(input_browse_widget));

    gtk_widget_hide(input_browse_widget);
    if (files)
    {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          add_input_file(s);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void output_browse_ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(output_browse_widget));

    gtk_widget_hide(output_browse_widget);
    if (files)
    {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          put_string_to_entry("output_directory_entry", s);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void input_browse_widget_destroy()
{
    gtk_widget_destroy(input_browse_widget);
    input_browse_widget = NULL;
}

SIGNAL_CALLBACK void output_browse_widget_destroy()
{
    gtk_widget_destroy(output_browse_widget);
    output_browse_widget = NULL;
}

// sets up the file chooser dialog
static void create_input_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("c2v_window");

    input_browse_widget = gtk_file_chooser_dialog_new(
        "Open Image File", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_OPEN,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)input_browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(input_browse_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(input_browse_ok_clicked), NULL);
    g_signal_connect(input_browse_widget, "destroy",
        G_CALLBACK(input_browse_widget_destroy), NULL);
    g_signal_connect(input_browse_widget, "destroy_event",
        G_CALLBACK(input_browse_widget_destroy), NULL);
    g_signal_connect(input_browse_widget, "delete_event",
        G_CALLBACK(input_browse_widget_destroy), NULL);

    // add the filters
    GtkFileFilter *csv_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(csv_filt, "CSV Files (*.csv)");
    gtk_file_filter_add_pattern(csv_filt, "*.csv");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                csv_filt);

    GtkFileFilter *all_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(all_filt, "All Files (*.*)");
    gtk_file_filter_add_pattern(all_filt, "*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(input_browse_widget),
                                all_filt);

    // allow multi-select
    //gtk_file_chooser_set_select_multiple(
    //    GTK_FILE_CHOOSER(input_browse_widget), TRUE);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(input_browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(input_browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(input_browse_widget),
                                    GTK_RESPONSE_OK);
}

static void create_output_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("c2v_window");

    output_browse_widget = gtk_file_chooser_dialog_new(
        "Select Output Directory", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)output_browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(output_browse_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(output_browse_ok_clicked), NULL);
    g_signal_connect(output_browse_widget, "destroy",
        G_CALLBACK(output_browse_widget_destroy), NULL);
    g_signal_connect(output_browse_widget, "destroy_event",
        G_CALLBACK(output_browse_widget_destroy), NULL);
    g_signal_connect(output_browse_widget, "delete_event",
        G_CALLBACK(output_browse_widget_destroy), NULL);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(output_browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(output_browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(output_browse_widget),
                                    GTK_RESPONSE_OK);
}

#endif // #ifndef win32

static void input_file_browse(void)
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
    of.lpstrFilter = "CSV Files (*.csv)\0*.csv\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    add_input_file(fname);
#else // #ifdef win32
    if (!input_browse_widget)
        create_input_file_chooser_dialog();

    gtk_widget_show(input_browse_widget);
#endif // #ifdef win32
}

static void output_file_browse(void)
{
#ifdef win32
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = "Select Output Directory";
    LPITEMIDLIST pidl = SHBrowseForFolder(&bi);
    if (pidl != 0)
    {
        TCHAR path[MAX_PATH];
        if (SHGetPathFromIDList(pidl, path))
            put_string_to_entry("output_directory_entry", path);
    }
#else // #ifdef win32
    if (!output_browse_widget)
        create_output_file_chooser_dialog();

    gtk_widget_show(output_browse_widget);
#endif // #ifdef win32
}

SIGNAL_CALLBACK void
on_input_browse_button_clicked(GtkWidget *w)
{
    input_file_browse();
}

SIGNAL_CALLBACK void
on_output_browse_button_clicked(GtkWidget *w)
{
    output_file_browse();
}
