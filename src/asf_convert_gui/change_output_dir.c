#include "asf_convert_gui.h"
#include <ctype.h>
#include <gdk/gdkkeysyms.h>

static void
change_output_output_directory_hide()
{
    GtkWidget *change_output_directory_dialog;

    change_output_directory_dialog =
            glade_xml_get_widget(glade_xml, "change_output_directory_dialog");
  
    gtk_widget_hide(change_output_directory_dialog);
}

void do_change_output_directory(const gchar * new_dir )
{
  LSL;
    gboolean valid;
    GtkTreeIter iter;
    gchar * new_dir_fixed;
    
    gchar sep[2];

    assert(list_store);

    sep[0] = DIR_SEPARATOR;
    sep[1] = 0;
    
    if (!g_str_has_suffix(new_dir, sep))
    {
        new_dir_fixed =
            (gchar *) g_malloc( sizeof(gchar) * (strlen(new_dir) + 1));

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
                    (strlen(basename) + strlen(new_dir_fixed) + 2) );

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

    LSU;
}

gboolean
prepare_change_output_directory_dialog()
{
  LSL;

    GtkTreeIter iter;

    GtkWidget *change_output_directory_dialog,
        *label_current_output_directory,
        *entry_new_output_directory;
    
    change_output_directory_dialog =
            glade_xml_get_widget(glade_xml, "change_output_directory_dialog");

    label_current_output_directory =
            glade_xml_get_widget(glade_xml, "label_current_output_directory");

    entry_new_output_directory =
            glade_xml_get_widget(glade_xml, "entry_new_output_directory");

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
  
    LSU;
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
            glade_xml_get_widget(glade_xml, "change_output_directory_dialog");

    entry_new_output_directory =
            glade_xml_get_widget(glade_xml, "entry_new_output_directory");

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


