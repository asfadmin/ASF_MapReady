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
static const int COL_DATA_FILE_FULL = 1; // hidden column, contains full path
static const int COL_EXISTS = 2;

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
        COL_DATA_FILE_FULL, data_file,
        COL_EXISTS, exists, -1);

    free(dir);
    free(file);

    return TRUE;
}

int add_to_image_list2(const char * path, const char * data_file)
{
    char *f = MALLOC(sizeof(char)*(strlen(path)+strlen(data_file)+10));
    sprintf(f, "%s/%s", path, data_file);

    int file_exists = fileExists(f);

    printf("Adding: %s\n", f);

    const char *exists = file_exists ? "Yes" : "No";

    GtkTreeIter iter;
    gtk_list_store_append(images_list, &iter);
    gtk_list_store_set(images_list, &iter,
        COL_DATA_FILE, data_file,
        COL_DATA_FILE_FULL, f,
        COL_EXISTS, exists, 
        -1);

    free(f);

    return TRUE;
}

void clear_image_list()
{
    printf("Clearing list...\n");
    gtk_list_store_clear(images_list);
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

    /* Next Column: File Name include the path (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Data File (Full)");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(images_treeview), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_DATA_FILE_FULL);

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

static int
get_iter_to_first_selected_row(GtkWidget *images_treeview, GtkTreeIter *iter)
{
    GList * selected_rows;
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    int found;

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(images_treeview));
    model = GTK_TREE_MODEL(images_list);

    selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);
    if (selected_rows)
    {
        GtkTreePath * path;

        path = (GtkTreePath *) selected_rows->data;
        gtk_tree_model_get_iter(model, iter, path);

        g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
        g_list_free(selected_rows);

        found = TRUE;
    }
    else
    {
        found = FALSE;
    }

    return found;
}

static void
show_please_select_message()
{
    static const char *msg = 
      "Please select a file first.\n";

    message_box(msg);
}

static double get_scale()
{
    return .5;
}

static void show_it(const char * filename, int is_new)
{
    GtkWidget *viewed_image;
    static GdkPixbuf *output_pixbuf = NULL;
    static GdkPixbuf *shown_pixbuf = NULL;

    GtkWidget *scrolled_window =
        glade_xml_get_widget(glade_xml, "viewed_image_scrolledwindow");
    GtkAdjustment *hadjust = gtk_scrolled_window_get_hadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));
    GtkAdjustment *vadjust = gtk_scrolled_window_get_vadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));

    double hpos = 0.5, vpos = 0.5;
    if (!is_new)
    {
        if (hadjust->upper == hadjust->page_size)
            hpos = hadjust->page_size / 2.0 / hadjust->upper;
        else
            hpos = (hadjust->value + hadjust->page_size / 2) / hadjust->upper;
        
        if (vadjust->upper == vadjust->page_size)
            vpos = vadjust->page_size / 2.0 / vadjust->upper;
        else
            vpos = (vadjust->value + vadjust->page_size / 2) / vadjust->upper;
    }

    if (is_new)
    {
        if (output_pixbuf) {
            // free any image from a previous "view output"
            gdk_pixbuf_unref(output_pixbuf);
            output_pixbuf = NULL;
        }
    }
    else
    {
        // should already have an output image loaded
        assert(output_pixbuf);
        assert(shown_pixbuf);
    }

    if (shown_pixbuf) {
        gdk_pixbuf_unref(shown_pixbuf);
        shown_pixbuf = NULL;
    }

    if (!output_pixbuf) {
        assert(filename);
        GError *err = NULL;
        output_pixbuf = gdk_pixbuf_new_from_file(filename, &err);
        if ( err != NULL ) {
            printf ("Couldn't open output image: %s\n", err->message);
            message_box("Error opening output image");
            return;
        }
    }

    double s = get_scale();
    int w = gdk_pixbuf_get_width(output_pixbuf);
    int h = gdk_pixbuf_get_height(output_pixbuf);
    int scaled_w = (int)(w*s);
    int scaled_h = (int)(h*s);
    int bps = gdk_pixbuf_get_bits_per_sample(output_pixbuf);
    shown_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE,
                                  bps, scaled_w, scaled_h);
    GdkInterpType interp = s>.2 ? GDK_INTERP_BILINEAR : GDK_INTERP_NEAREST;
    gdk_pixbuf_scale(output_pixbuf, shown_pixbuf, 0, 0, scaled_w, scaled_h,
                     0, 0, s, s, interp);

    viewed_image = get_widget_checked("viewed_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(viewed_image), shown_pixbuf);

    if (is_new)
    {
        GtkWidget *displayed_filename_label =
            get_widget_checked("displayed_filename_label");

        gtk_label_set_text(GTK_LABEL(displayed_filename_label), filename);
    }

    double pos;
    hadjust = gtk_scrolled_window_get_hadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));
    pos = hpos * hadjust->upper - hadjust->page_size/2;
    gtk_adjustment_set_value(hadjust, pos);
    gtk_adjustment_changed(hadjust);

    vadjust = gtk_scrolled_window_get_vadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));
    pos = vpos * vadjust->upper - vadjust->page_size/2;
    gtk_adjustment_set_value(vadjust, pos);
    gtk_adjustment_changed(vadjust);
}

void show_output_image(const char * filename)
{
    show_it(filename, TRUE);
}

SIGNAL_CALLBACK void on_view_button_clicked(GtkWidget *w)
{
    GtkWidget *images_treeview;
    GtkTreeIter iter;

    images_treeview = get_widget_checked("images_treeview");
    if (get_iter_to_first_selected_row(images_treeview, &iter))
    {
        char *file_name;

        gtk_tree_model_get(GTK_TREE_MODEL(images_list), &iter, 
            COL_DATA_FILE_FULL, &file_name, -1);

        printf("Displaying: %s\n", file_name);

        if (g_file_test(file_name, G_FILE_TEST_EXISTS))
        {
            show_output_image(file_name);
        }
        else
        {
            char msg[2048];
            sprintf(msg, "The file was not found:\n"
                "   %s\n", file_name);
            message_box(msg);
        }
    }
    else
    {
        show_please_select_message();
    }
}
