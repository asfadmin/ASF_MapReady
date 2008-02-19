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

#include "proj2proj.h"

static void save(const char *filename, const char *widget_name)
{
    GtkTextIter start, end;
    GtkWidget *tv = get_widget_checked(widget_name);
    GtkTextBuffer *tb = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tv));
    gtk_text_buffer_get_start_iter(tb, &start);
    gtk_text_buffer_get_end_iter(tb, &end);
    char *txt = gtk_text_buffer_get_text(tb, &start, &end, FALSE);

    FILE *fout = FOPEN(filename, "w");
    fprintf(fout, "%s", txt);
    fclose(fout);
}

#ifndef win32

static GtkWidget *save_widget = NULL;
static GtkWidget *browse_widget = NULL;
static const char *last_tv_name = NULL;

// called when "cancel" clicked on the "Load" GtkFileChooser
SIGNAL_CALLBACK void new_cancel_clicked()
{
    gtk_widget_hide(browse_widget);
}

// called when "ok" clicked on the "Load" GtkFileChooser
SIGNAL_CALLBACK void new_ok_clicked()
{
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(browse_widget));

    gtk_widget_hide(browse_widget);
    if (files)
    {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          put_file_in_textview(s, last_tv_name);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void browse_widget_destroy()
{
    gtk_widget_destroy(browse_widget);
    browse_widget = NULL;
}

SIGNAL_CALLBACK void save_widget_destroy()
{
    gtk_widget_destroy(save_widget);
    save_widget = NULL;
}

// called when "cancel" clicked on the "Save" GtkFileChooser
static SIGNAL_CALLBACK void save_cancel_clicked()
{
  gtk_widget_hide(save_widget);
}

// called when "ok" clicked on the "Save" GtkFileChooser
static SIGNAL_CALLBACK void save_ok_clicked()
{
  GSList *files = gtk_file_chooser_get_filenames(
    GTK_FILE_CHOOSER(save_widget));

  gtk_widget_hide(save_widget);

  if (files) {
    save((char *) files->data, last_tv_name);
    char msg[2304];
    char *filename = get_filename((char*)files->data);
    sprintf(msg," Saved: %s", filename);
    char name[255];
    strncpy_safe(name, last_tv_name, 7);
    strcat(name, "_message_label");
    put_string_to_label(name, msg);
    free(filename);

    g_slist_free(files);
  }
}

// sets up the save file chooser dialog
static void create_save_file_chooser_dialog()
{
  GtkWidget *parent = get_widget_checked("proj2proj_main_window");

  save_widget = gtk_file_chooser_dialog_new(
      "Save File", GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,  //Cancel button
      GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,    //Save button
      NULL);

  // we need to extract the buttons, so we can connect them to our
  // button handlers, above
  GtkHButtonBox *box = 
    (GtkHButtonBox*)(((GtkDialog*)save_widget)->action_area);
  GList *buttons = box->button_box.box.children;

  GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
  GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

  g_signal_connect((gpointer)cancel_btn, "clicked",
                    G_CALLBACK(save_cancel_clicked), NULL);
  g_signal_connect((gpointer)ok_btn, "clicked",
                    G_CALLBACK(save_ok_clicked), NULL);
  g_signal_connect(save_widget, "destroy",
                   G_CALLBACK(save_widget_destroy), NULL);
  g_signal_connect(save_widget, "destroy_event",
                   G_CALLBACK(save_widget_destroy), NULL);
  g_signal_connect(save_widget, "delete_event",
                   G_CALLBACK(save_widget_destroy), NULL);

  // add the filters
  GtkFileFilter *txt_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(txt_filt, "Text Files (*.txt)");
  gtk_file_filter_add_pattern(txt_filt, "*.cfg");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(save_widget), txt_filt);

  GtkFileFilter *all_filt = gtk_file_filter_new();
  gtk_file_filter_set_name(all_filt, "All Files (*.*)");
  gtk_file_filter_add_pattern(all_filt, "*");
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(save_widget), all_filt);

  // we need to make these modal -- if the user opens multiple "save"
  // dialogs, we'll get confused on the callbacks
  gtk_window_set_modal(GTK_WINDOW(save_widget), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(save_widget), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(save_widget), GTK_RESPONSE_OK);
}

// sets up the file chooser dialog
static void create_file_chooser_dialog()
{
    GtkWidget *parent = get_widget_checked("proj2proj_main_window");

    browse_widget = gtk_file_chooser_dialog_new(
        "Open File", GTK_WINDOW(parent),
        GTK_FILE_CHOOSER_ACTION_OPEN,
        GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
        GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
        NULL);

    // we need to extract the buttons, so we can connect them to our
    // button handlers, above
    GtkHButtonBox *box =
        (GtkHButtonBox*)(((GtkDialog*)browse_widget)->action_area);
    GList *buttons = box->button_box.box.children;

    GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
    GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

    g_signal_connect((gpointer)cancel_btn, "clicked",
        G_CALLBACK(new_cancel_clicked), NULL);
    g_signal_connect((gpointer)ok_btn, "clicked",
        G_CALLBACK(new_ok_clicked), NULL);
    g_signal_connect(browse_widget, "destroy",
        G_CALLBACK(browse_widget_destroy), NULL);
    g_signal_connect(browse_widget, "destroy_event",
        G_CALLBACK(browse_widget_destroy), NULL);
    g_signal_connect(browse_widget, "delete_event",
        G_CALLBACK(browse_widget_destroy), NULL);

    // add the filters
    GtkFileFilter *txt_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(txt_filt, "Text Files (*.txt)");
    gtk_file_filter_add_pattern(txt_filt, "*.txt");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), txt_filt);

    GtkFileFilter *all_filt = gtk_file_filter_new();
    gtk_file_filter_set_name(all_filt, "All Files (*.*)");
    gtk_file_filter_add_pattern(all_filt, "*");
    gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(browse_widget), all_filt);

    // allow multi-select
    gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(browse_widget), TRUE);

    // we need to make these modal -- if the user opens multiple "open"
    // dialogs, we'll get confused on the callbacks
    gtk_window_set_modal(GTK_WINDOW(browse_widget), TRUE);
    gtk_window_set_destroy_with_parent(GTK_WINDOW(browse_widget), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(browse_widget),
                                    GTK_RESPONSE_OK);
}
#endif

void save_tv(const char *tv_name)
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
  of.lpstrFilter = "Text Files (*.txt)\0*.txt\0"
      "All Files\0*\0";
  of.lpstrCustomFilter = NULL;
  of.nFilterIndex = 1;
  of.lpstrFile = fname;
  of.nMaxFile = sizeof(fname);
  of.lpstrFileTitle = NULL;
  of.lpstrInitialDir = ".";
  of.lpstrTitle = "Save File";
  of.lpstrDefExt = NULL;
  of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

  retval = GetSaveFileName(&of);

  if (!retval) {
    if (CommDlgExtendedError()) {
      message_box("File dialog box error");
    }
  }

  char msg[2304];
  if (strlen(fname) > 0) {
    save(fname, tv_name);
    char msg[2304];
    char *filename = get_filename(fname);
    sprintf(msg," Saved: %s", filename);
    char name[255];
    strncpy_safe(name, tv_name, 7);
    strcat(name, "_message_label");
    put_string_to_label(name, msg);
    free(filename);
  }
  else {
    sprintf(msg, " Zero length filename found!");
    message_box(msg);
  }

#else // #ifdef win32

  /* Linux version -- use GtkFileChooser if possible */

  if (!save_widget)
    create_save_file_chooser_dialog();

  last_tv_name = tv_name;
  gtk_widget_show(save_widget);

#endif // #ifdef win32
}

void load_into_tv(const char *tv_name)
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
    of.lpstrFilter = "Text Files (*.txt)\0*.txt\0"
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

    put_file_in_textview(fname, tv_name);

#else // #ifdef win32

    if (!browse_widget)
        create_file_chooser_dialog();

    last_tv_name = tv_name;
    gtk_widget_show(browse_widget);
#endif // #ifdef win32
}

SIGNAL_CALLBACK void
on_source_load_button_clicked(GtkWidget *w)
{
  load_into_tv("source_textview");
}

SIGNAL_CALLBACK void
on_source_save_button_clicked(GtkWidget *w)
{
  save_tv("source_textview");
}

SIGNAL_CALLBACK void
on_target_load_button_clicked(GtkWidget *w)
{
  load_into_tv("target_textview");
}

SIGNAL_CALLBACK void
on_target_save_button_clicked(GtkWidget *w)
{
  save_tv("target_textview");
}
