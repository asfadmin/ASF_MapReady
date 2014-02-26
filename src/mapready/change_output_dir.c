#include <ctype.h>
#include <gdk/gdkkeysyms.h>

#include "mapready.h"

#ifdef win32
#include <windows.h>
//#include <shlobj.h>
#endif

static void
change_output_output_directory_hide()
{
    GtkWidget *change_output_directory_dialog;

    change_output_directory_dialog =
        get_widget_checked("change_output_directory_dialog");

    gtk_widget_hide(change_output_directory_dialog);
}

void do_change_output_directory(const gchar * new_dir )
{
    gboolean valid;
    GtkTreeIter iter;
    gchar * new_dir_fixed;

    gchar sep[2];

    assert(list_store);

    sep[0] = DIR_SEPARATOR;
    sep[1] = '\0';

    if (!g_str_has_suffix(new_dir, sep))
    {
        new_dir_fixed =
            (gchar *) g_malloc( sizeof(gchar) * (strlen(new_dir) + 5));

        strcpy(new_dir_fixed, new_dir);
        strcat(new_dir_fixed, sep);
    }
    else
    {
        new_dir_fixed = g_strdup(new_dir);
    }

    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
    while (valid)
    {
        gchar * current_output_name;
        gchar * new_output_name;
        gchar * basename;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
            COL_OUTPUT_FILE, &current_output_name, -1);

        basename = g_path_get_basename(current_output_name);

        new_output_name =
            (gchar *) g_malloc( sizeof(gchar) *
            (strlen(basename) + strlen(new_dir_fixed) + 5) );

        g_sprintf(new_output_name, "%s%s", new_dir_fixed, basename);

        gtk_list_store_set(list_store, &iter, 
            COL_OUTPUT_FILE, new_output_name, -1);

        g_free(basename);
        g_free(new_output_name);
        g_free(current_output_name);

        valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }

    output_directory = g_strdup(new_dir_fixed);

    g_free(new_dir_fixed);
}

gboolean
prepare_change_output_directory_dialog()
{
    GtkTreeIter iter;

    GtkWidget *change_output_directory_dialog,
        *label_current_output_directory,
        *entry_new_output_directory;

    change_output_directory_dialog =
        get_widget_checked("change_output_directory_dialog");

    label_current_output_directory =
        get_widget_checked("label_current_output_directory");

    entry_new_output_directory =
        get_widget_checked("entry_new_output_directory");

    /* go through all of the output files... if only 1 directory is being
    used, can report it -- otherwise, we'll just say "Multiple" */
    if (list_store)
    {
        gboolean first = TRUE;
        gboolean valid;
        gboolean all_identical = TRUE;
        gchar * first_path = NULL;

        valid =
            gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);

        while (valid)
        {
            gchar * current_output_name;
            gchar * path;

            gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                COL_OUTPUT_FILE, &current_output_name, -1);

            path = g_path_get_dirname(current_output_name);

            if( first )
            {
                first_path = g_strdup(path);
                first = FALSE;
            }
            else
            {
                if (!g_str_equal(path, first_path))
                    all_identical = FALSE;
            }

            g_free(path);
            g_free(current_output_name);

            valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), 
                &iter);
        }

        if (first_path)
        {
            if (all_identical)
            {
                gtk_label_set_text(GTK_LABEL(label_current_output_directory),
                    first_path);
            }
            else
            {
                gtk_label_set_text(GTK_LABEL(label_current_output_directory),
                    "Multiple");
            }

            gtk_entry_set_text(GTK_ENTRY(entry_new_output_directory),
                first_path);
            g_free(first_path);
        }
        else
        {
            gtk_label_set_text(GTK_LABEL(label_current_output_directory), "-");
        }

        gtk_widget_grab_focus(entry_new_output_directory);
        gtk_widget_show(change_output_directory_dialog);
    }

	return TRUE;
}

SIGNAL_CALLBACK void
on_change_output_directory_button_clicked(GtkWidget *widget)
{
    prepare_change_output_directory_dialog();
}

SIGNAL_CALLBACK void
on_change_output_directory_button_cancel_clicked(GtkWidget *widget)
{
    change_output_output_directory_hide();
}

static void change_output_directory_button_ok_clicked()
{
    GtkWidget *change_output_directory_dialog;
    GtkWidget *entry_new_output_directory;
    const gchar * new_dir;

    change_output_directory_dialog =
        get_widget_checked("change_output_directory_dialog");

    entry_new_output_directory =
        get_widget_checked("entry_new_output_directory");

    new_dir = gtk_entry_get_text(GTK_ENTRY(entry_new_output_directory));

    if (strlen(new_dir) > 0)
    {
        do_change_output_directory((gchar *)new_dir);
    }

    gtk_widget_hide(change_output_directory_dialog);
}

SIGNAL_CALLBACK void
on_change_output_directory_button_ok_clicked(GtkWidget *widget)
{
    change_output_directory_button_ok_clicked();
}

SIGNAL_CALLBACK gboolean
on_change_output_directory_dialog_destroy(GtkWidget *w)
{
    change_output_output_directory_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_change_output_directory_dialog_delete_event(GtkWidget *w)
{
    change_output_output_directory_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_change_output_directory_dialog_destroy_event(GtkWidget *w)
{
    change_output_output_directory_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_change_output_directory_dialog_key_press_event(GtkWidget * widget, 
                                                  GdkEventKey * event,
                                                  GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
        change_output_directory_button_ok_clicked();
        return TRUE;
    }

    return FALSE;
}

static GtkWidget *browse_widget = NULL;

// called when "cancel" clicked on the "Save" GtkFileChooser
SIGNAL_CALLBACK void browse_cancel_clicked()
{
  gtk_widget_hide(browse_widget);
}

// called when "ok" clicked on the "Save" GtkFileChooser
SIGNAL_CALLBACK void browse_ok_clicked()
{
  const char *folder =
      gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(browse_widget));
  if (folder) {
    GtkWidget *entry_new_output_directory =
        get_widget_checked("entry_new_output_directory");
    gtk_entry_set_text(GTK_ENTRY(entry_new_output_directory), folder);
  }
  gtk_widget_hide(browse_widget);
}

static void create_browse_file_chooser_dialog()
{
  GtkWidget *parent =
      get_widget_checked("change_output_directory_dialog");

  browse_widget = gtk_file_chooser_dialog_new(
      "Change Output Directory", GTK_WINDOW(parent),
      GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,  //Cancel button
      GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,      //OK button
      NULL);

  // we need to extract the buttons, so we can connect them to our
  // button handlers, above
  GtkHButtonBox *box = 
    (GtkHButtonBox*)(((GtkDialog*)browse_widget)->action_area);
  GList *buttons = box->button_box.box.children;

  GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
  GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

  g_signal_connect((gpointer)cancel_btn, "clicked",
                    G_CALLBACK(browse_cancel_clicked), NULL);
  g_signal_connect((gpointer)ok_btn, "clicked",
                    G_CALLBACK(browse_ok_clicked), NULL);
  g_signal_connect(browse_widget, "destroy",
                   G_CALLBACK(browse_cancel_clicked), NULL);
  g_signal_connect(browse_widget, "destroy_event",
                   G_CALLBACK(browse_cancel_clicked), NULL);
  g_signal_connect(browse_widget, "delete_event",
                   G_CALLBACK(browse_cancel_clicked), NULL);

  gtk_window_set_modal(GTK_WINDOW(browse_widget), TRUE);
  gtk_window_set_destroy_with_parent(GTK_WINDOW(browse_widget), TRUE);
  gtk_dialog_set_default_response(GTK_DIALOG(browse_widget), GTK_RESPONSE_OK);
}

SIGNAL_CALLBACK void
on_browse_output_directory_button_clicked(GtkWidget *widget)
{
#ifdef win32XXX
    GtkWidget *entry_new_output_directory;

    entry_new_output_directory =
        get_widget_checked("entry_new_output_directory");
    
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = "Select Output Directory";
    bi.ulFlags |= BIF_USENEWUI;
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if ( pidl != 0 )
    {
        TCHAR path[MAX_PATH];
        if ( SHGetPathFromIDList ( pidl, path ) )
        {
            gtk_entry_set_text(GTK_ENTRY(entry_new_output_directory), path);
        }
    }
#else
    if (!browse_widget)
        create_browse_file_chooser_dialog();

    GtkWidget *entry_new_output_directory =
        get_widget_checked("entry_new_output_directory");
    const char *dir = gtk_entry_get_text(
        GTK_ENTRY(entry_new_output_directory));
    if (dir && strlen(dir) > 0) {
        gtk_file_chooser_set_current_folder(
            GTK_FILE_CHOOSER(browse_widget), dir);
    }

    gtk_widget_show(browse_widget);
#endif
}

void
hide_output_directory_selection_dialog()
{
    GtkWidget *output_directory_selection_dialog =
        get_widget_checked("output_directory_selection");

    gtk_widget_show(output_directory_selection_dialog);
}

SIGNAL_CALLBACK void
on_output_directory_selection_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *output_directory_selection_dialog;
    gchar **selections;
    gchar **current;

    output_directory_selection_dialog =
        get_widget_checked("output_directory_selection");

    selections = gtk_file_selection_get_selections(
        GTK_FILE_SELECTION(output_directory_selection_dialog));

    current = selections;

    while (*current)
    {
        GtkWidget *entry_new_output_directory;

	    entry_new_output_directory =
	        get_widget_checked("entry_new_output_directory");

        if (g_file_test(*current, G_FILE_TEST_IS_DIR))
        {
            gtk_entry_set_text(GTK_ENTRY(entry_new_output_directory), *current);
        } 
        else
        {
            char *dir = getPath(*current);
            gtk_entry_set_text(GTK_ENTRY(entry_new_output_directory), dir);
            free(dir);
        }

        break;
    }

    g_strfreev(selections);
    gtk_widget_hide(output_directory_selection_dialog);
}

SIGNAL_CALLBACK void
on_output_directory_selection_cancel_button_clicked(GtkWidget *widget)
{
    hide_output_directory_selection_dialog();
}

SIGNAL_CALLBACK gboolean
on_output_directory_selection_delete_event(GtkWidget *w)
{
    hide_output_directory_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_output_directory_selection_destroy_event(GtkWidget *w)
{
    hide_output_directory_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_output_directory_selection_destroy(GtkWidget *w)
{
    hide_output_directory_selection_dialog();
    return TRUE;
}
