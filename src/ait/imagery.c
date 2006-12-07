#include "ait.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <sys/wait.h>

static const int COL_DATA_FILE = 0;
static const int COL_EXISTS = 1;

int add_to_image_list(const char * data_file)
{
    int file_exists = fileExists(data_file);

    printf("Adding: %s\n", data_file);

    char *dir = MALLOC(sizeof(char)*(strlen(data_file)+2));
    char *file = MALLOC(sizeof(char)*(strlen(data_file)+2));

    split_dir_and_file(data_file, dir, file);
    const char *exists = file_exists ? "Yes" : "No";

    GtkTreeIter iter;
    gtk_list_store_append(images_list, &iter);
    gtk_list_store_set(images_list, &iter,
        COL_DATA_FILE, file,
        COL_EXISTS, exists, 
        -1);

    free(dir);
    free(file);

    return TRUE;
}

void render_filename(GtkTreeViewColumn *tree_column,
                     GtkCellRenderer *cell,
                     GtkTreeModel *tree_model,
                     GtkTreeIter *iter,
                     gpointer data)
{
    char *data_file;
    char *status;

    gtk_tree_model_get (tree_model, iter, 
        COL_DATA_FILE, &data_file, 
        COL_EXISTS, &status, -1);

    if (fileExists(data_file))
    {
        GdkColor c;

        c.red = 65535;
        c.green = c.blue = 0;

        g_object_set( G_OBJECT (cell), "foreground-gdk", &c, NULL);
    }
    else
    {
        g_object_set( G_OBJECT (cell), "foreground-gdk", NULL, NULL);
    }

    g_object_set (G_OBJECT (cell), "text", data_file, NULL);

    g_free(data_file);
    g_free(status);
}

void
setup_images_treeview()
{
    GtkTreeViewColumn *col;
    GtkCellRenderer *renderer;

    images_list = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
    GtkWidget *images_treeview = get_widget_checked("images_treeview");

    /* First Column: File Name */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Data File");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(images_treeview), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_DATA_FILE);

    /* add our custom renderer (turns existing files red) */
    gtk_tree_view_column_set_cell_data_func(col, renderer,
        render_filename, NULL, NULL);

    /* Next Column: File Exists Yes/No (if the coloring isn't enough) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Done");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(images_treeview), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_EXISTS);

    gtk_tree_view_set_model(GTK_TREE_VIEW(images_treeview), 
        GTK_TREE_MODEL(images_list));  

    g_object_unref(images_list);

    gtk_tree_selection_set_mode(
        gtk_tree_view_get_selection(GTK_TREE_VIEW(images_treeview)),
        GTK_SELECTION_MULTIPLE);
}

