#include "mapready.h"
#include <ctype.h>
#include <gdk/gdkkeysyms.h>

static void
change_output_name_dialog_hide()
{
    GtkWidget *change_output_name_dialog;

    change_output_name_dialog =
        get_widget_checked("change_output_name_dialog");

    gtk_widget_hide(change_output_name_dialog);
}

void
do_rename(GtkTreeModel *model, GtkTreeIter *iter, const gchar *new_name)
{
    const gchar * ext;
    gchar *user_ext, *basename, *name_without_path, *p, *fixed_name,
      *file_name, *path, *polsarpro_aux_info;
    Settings * user_settings;

    user_settings = settings_get_from_gui();
    ext = settings_get_output_format_extension(user_settings);

    gtk_tree_model_get(model, iter, 
		       COL_INPUT_FILE, &file_name, 
		       COL_POLSARPRO_INFO, &polsarpro_aux_info, -1);
    int image_data_type = extract_image_data_type(polsarpro_aux_info);

    if (output_directory)
    {
        path = g_strdup(output_directory);
    }
    else
    {
        path = g_path_get_dirname(file_name);
        if (strcmp(path, ".") == 0)
        {
            *path = '\0';
        }
        else
        {
            int len = strlen(path);
            path = (gchar *) g_realloc(path, sizeof(gchar) * (len + 2));
            *(path + len) = DIR_SEPARATOR;
            *(path + len + 1) = '\0';
        }
    }

    g_free(file_name);
    g_free(polsarpro_aux_info);

    /* do not allow user to move output file to a different location */
    name_without_path = g_path_get_basename(new_name);

    /* replace illegal characters with _ */
    p = name_without_path;
    do
    {
        /* figure out a better way here */
        if (*p == '?' || isspace(*p) || *p == '>' || *p == '<' || *p == '|')
            *p = '_';
    }
    while (*p++);

    /* add appropriate extension if was not given by user */
    basename = g_strdup(name_without_path);
    p = findExt(basename);
    if (p)
    {
        *p = '\0';
        user_ext = p + 1;
    }
    else
    {
        user_ext = NULL;
    }

    if (user_ext == NULL)
    {
        int len = strlen(path) + strlen(basename) + strlen(ext) + 2;
        fixed_name = (gchar *) g_malloc( sizeof(gchar) * len );

	if (image_data_type == SELECT_POLARIMETRIC_MATRIX ||
	    image_data_type == SELECT_POLARIMETRIC_DECOMPOSITION)
	  g_snprintf(fixed_name, len, "%s%s", path, basename);
	else
	  g_snprintf(fixed_name, len, "%s%s.%s", path, basename, ext);
    }
    else if (strcmp(user_ext, ext) != 0)
    {
        int len = strlen(path) + strlen(name_without_path) + strlen(ext) + 2;
        fixed_name = (gchar *) g_malloc( sizeof(gchar) * len );

	if (image_data_type == SELECT_POLARIMETRIC_MATRIX ||
	    image_data_type == SELECT_POLARIMETRIC_DECOMPOSITION)
	  g_snprintf(fixed_name, len, "%s%s", path, name_without_path);
	else
	  g_snprintf(fixed_name, len, "%s%s.%s", path, name_without_path, ext);
    }
    else
    {
        int len = strlen(path) + strlen(name_without_path) + 2;
        fixed_name = (gchar *) g_malloc( sizeof(gchar) * len );

        g_snprintf(fixed_name, len, "%s%s", path, name_without_path);
    }

    g_free(basename);
    g_free(name_without_path);
    g_free(path);

    set_output_name(iter, fixed_name);

    g_free(fixed_name);
    settings_delete(user_settings);
}

void
do_rename_selected(const gchar *new_name)
{
    GtkWidget * files_list;
    GtkTreeIter iter;

    files_list = get_widget_checked("files_list");

    if (get_iter_to_first_selected_row(files_list, list_store, &iter))
    {
        do_rename(GTK_TREE_MODEL(list_store), &iter, new_name);
    }
}

gboolean
rename_selected_output_filename()
{
    GtkWidget *files_list;
    GtkTreeIter iter;

    files_list = get_widget_checked("files_list");

    if (get_iter_to_first_selected_row(files_list, list_store, &iter))
    {
        gchar *current_output_name;
        gchar *name_without_path;

        GtkWidget *change_output_name_dialog,
            *label_current_output_filename,
            *entry_new_output_filename;

        change_output_name_dialog =
            get_widget_checked("change_output_name_dialog");

        label_current_output_filename =
            get_widget_checked("label_current_output_filename");

        entry_new_output_filename =
            get_widget_checked("entry_new_output_filename");

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
            COL_OUTPUT_FILE, &current_output_name, -1);

        name_without_path = g_path_get_basename(current_output_name);

        gtk_label_set_text(GTK_LABEL(label_current_output_filename),
            name_without_path);

        gtk_entry_set_text(GTK_ENTRY(entry_new_output_filename),
            name_without_path);

        gtk_widget_grab_focus(entry_new_output_filename);

        g_free(name_without_path);
        g_free(current_output_name);

        gtk_widget_show(change_output_name_dialog);
    }
    else
    {
        show_please_select_message();
    }

    return TRUE;
}

SIGNAL_CALLBACK void
on_change_output_name_button_cancel_clicked(GtkWidget *widget)
{
    change_output_name_dialog_hide();
}

static void change_output_name_button_ok_clicked()
{
    GtkWidget *change_output_name_dialog;
    GtkWidget *entry_new_output_filename;
    const gchar * new_name;

    change_output_name_dialog =
        get_widget_checked("change_output_name_dialog");

    entry_new_output_filename =
        get_widget_checked("entry_new_output_filename");

    new_name = gtk_entry_get_text(GTK_ENTRY(entry_new_output_filename));

    if (strlen(new_name) > 0)
    {
        /* since dialog is modal, can assume same row is selected */
        do_rename_selected((gchar *)new_name);
    }

    gtk_widget_hide(change_output_name_dialog);
}

SIGNAL_CALLBACK void
on_change_output_name_button_ok_clicked(GtkWidget *widget)
{
    change_output_name_button_ok_clicked();
}

SIGNAL_CALLBACK gboolean
on_change_output_name_dialog_destroy(GtkWidget *w)
{
    change_output_name_dialog_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_change_output_name_dialog_delete_event(GtkWidget *w)
{
    change_output_name_dialog_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_change_output_name_dialog_destroy_event(GtkWidget *w)
{
    change_output_name_dialog_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_change_output_name_dialog_key_press_event(GtkWidget * widget,
                                             GdkEventKey * event,
                                             GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
        change_output_name_button_ok_clicked();
        return TRUE;
    }

    return FALSE;
}
