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

#include <ceos_io.h>

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
                       COL_EXISTS, exists,
        -1);

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

    gtk_tree_model_get (tree_model, iter, 
        COL_DATA_FILE, &data_file, -1);

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
}

void
setup_images_treeview()
{
    GtkTreeViewColumn *col;
    GtkCellRenderer *renderer;

    images_list = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
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

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    g_free(pixels);
}

static GdkPixbuf *
make_ceos_image_pixbuf (const char *input_metadata, 
                        const char *input_data,
                        size_t max_dimension)
{
    /* This can happen if we don't get around to drawing the thumbnail
       until the file has already been processes & cleaned up, don't want
       to crash in that case. */
    if (!fileExists(input_metadata))
        return NULL;

    meta_parameters *imd = meta_create (input_metadata); // Input metadata.
    /* Make a copy of one of the arguments so the compilers doesn't
    complain about us ignoring the const qualifier when we pass it fo
    fopenCeos().  */

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16 &&
        imd->general->data_type != INTEGER32 &&
        imd->general->data_type != REAL32 &&
        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        return NULL;
    }

    gchar *tmp = g_strdup (input_data);
    g_assert (tmp != NULL);
    CEOS_FILE *id = fopenCeos (tmp); // Input data file.
    g_free (tmp);

    if (!id->f_in) {
        // failed for some reason, just quit without thumbnailing...
        // possibly the file is bad, or perhaps it was removed from the
        // file list before we could get around to thumbnailing it.
        meta_free(imd);
        return NULL;
    }

    // use a larger dimension at first, for our crude scaling.  We will
    // use a better scaling method later, from GdbPixbuf
    int larger_dim = 512;

    // Vertical and horizontal scale factors required to meet the
    // max_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Thumbnail image sizes.
    size_t tsx = imd->general->sample_count / sf;
    size_t tsy = imd->general->line_count / sf;

    // Thumbnail image buffers - 'idata' is the temporary data prior
    // to scaling to 2-sigma, 'data' is the byte buffer used to create the
    // pixbuf, it will need 3 bytes per value, all equal, since the pixbuf
    // wants an RGB value.
    int *idata = g_new(int, tsx*tsy);
    guchar *data = g_new(guchar, 3*tsx*tsy);

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    size_t ii;
    int *line = g_new (int, imd->general->sample_count);

    // Keep track of the average pixel value, so later we can do a 2-sigma
    // scaling - makes the thumbnail look a little nicer and more like what
    // they'd get if they did the default jpeg export.
    double avg = 0.0;
    for ( ii = 0 ; ii < tsy ; ii++ ) {
        readCeosLine (line, ii * sf, id);
        size_t jj;
        for ( jj = 0 ; jj < tsx ; jj++ ) {
            // Current sampled value.
            double csv;		

            // We will average a couple pixels together.
            if ( jj * sf < imd->general->line_count - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }

            idata[ii*tsx + jj] = (int)csv;
            avg += csv;
        }
    }
    g_free (line);
    closeCeos(id);

    // Compute the std devation
    avg /= tsx*tsy;
    double stddev = 0.0;
    for (ii = 0; ii < tsx*tsy; ++ii)
        stddev += ((double)idata[ii] - avg) * ((double)idata[ii] - avg);
    stddev = sqrt(stddev / (tsx*tsy));
    
    // Set the limits of the scaling - 2-sigma on either side of the mean
    double lmin = avg - 2*stddev;
    double lmax = avg + 2*stddev;
    
    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    for (ii = 0; ii < tsx*tsy; ++ii) {
        int val = idata[ii];
        guchar uval;
        if (val < lmin)
            uval = 0;
        else if (val > lmax)
            uval = 255;
        else
            uval = (guchar) round(((val - lmin) / (lmax - lmin)) * 255);
        
        int n = 3*ii;
        data[n] = uval;
        data[n+1] = uval;
        data[n+2] = uval;
    }
    
    g_free(idata);
    
    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE, 
                                 8, tsx, tsy, tsx*3, destroy_pb_data, NULL);
    
    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", input_data);
        meta_free(imd);
        g_free(data);
        return NULL;
    }
    
    // Scale down to the size we actually want, using the built-in Gdk
    // scaling method, much nicer than what we did above
    GdkPixbuf *pb_s =
        gdk_pixbuf_scale_simple(pb, max_dimension, 
                                max_dimension, GDK_INTERP_BILINEAR);
    gdk_pixbuf_unref(pb);
    
    if (!pb_s) {
        printf("Failed to allocate scaled thumbnail pixbuf: %s\n", input_data);
        meta_free(imd);
        return NULL;
    }

    meta_free(imd);
    return pb_s;
}

static GdkPixbuf *
make_asf_image_pixbuf (const char *input_metadata, 
                       const char *input_data,
                       size_t max_dimension)
{
    meta_parameters *imd = meta_read (input_metadata); // Input metadata.

    if (imd->general->data_type != BYTE &&
        imd->general->data_type != INTEGER16 &&
        imd->general->data_type != INTEGER32 &&
        imd->general->data_type != REAL32 &&
        imd->general->data_type != REAL64)
    {
        /* don't know how to make a thumbnail for this type ... */
        return NULL;
    }

    FILE *img = fopenImage(input_data, "rb");
    if (!img) {
        // failed for some reason, just quit without thumbnailing...
        // possibly the file is bad, or perhaps it was removed from the
        // file list before we could get around to thumbnailing it.
        meta_free(imd);
        return NULL;
    }

    // use a larger dimension at first, for our crude scaling.  We will
    // use a better scaling method later, from GdbPixbuf
    int larger_dim = 4096;

    // Vertical and horizontal scale factors required to meet the
    // max_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : (vsf > 0 ? vsf : 1));

    // Thumbnail image sizes.
    int tsx = imd->general->sample_count / sf;
    int tsy = imd->general->line_count / sf;

    // Thumbnail image buffers - 'idata' is the temporary data prior
    // to scaling to 2-sigma, 'data' is the byte buffer used to create the
    // pixbuf, it will need 3 bytes per value, all equal, since the pixbuf
    // wants an RGB value.
    int *idata = g_new(int, tsx*tsy);
    guchar *data = g_new(guchar, 3*tsx*tsy);

    // Form the thumbnail image by grabbing individual pixels.  FIXME:
    // Might be better to do some averaging or interpolating.
    int ii;
    float *line = g_new (float, imd->general->sample_count);

    // Keep track of the average pixel value, so later we can do a 2-sigma
    // scaling - makes the thumbnail look a little nicer and more like what
    // they'd get if they did the default jpeg export.
    double avg = 0.0;
    for ( ii = 0 ; ii < tsy ; ii++ ) {
        get_float_line (img, imd, ii*sf, line);
        size_t jj;
        for ( jj = 0 ; jj < tsx ; jj++ ) {
            // Current sampled value.
            double csv;		

            // We will average a couple pixels together.
            if ( jj * sf < imd->general->line_count - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }

            idata[ii*tsx + jj] = (int)csv;
            avg += csv;
        }
    }
    g_free (line);
    fclose(img);

    // Compute the std devation
    avg /= tsx*tsy;
    double stddev = 0.0;
    for (ii = 0; ii < tsx*tsy; ++ii)
        stddev += ((double)idata[ii] - avg) * ((double)idata[ii] - avg);
    stddev = sqrt(stddev / (tsx*tsy));
    
    // Set the limits of the scaling - 2-sigma on either side of the mean
    double lmin = avg - 2*stddev;
    double lmax = avg + 2*stddev;
    
    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    for (ii = 0; ii < tsx*tsy; ++ii) {
        int val = idata[ii];
        guchar uval;
        if (val < lmin)
            uval = 0;
        else if (val > lmax)
            uval = 255;
        else
            uval = (guchar) round(((val - lmin) / (lmax - lmin)) * 255);
        
        int n = 3*ii;
        data[n] = uval;
        data[n+1] = uval;
        data[n+2] = uval;
    }
    
    g_free(idata);
    
    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE, 
                                 8, tsx, tsy, tsx*3, destroy_pb_data, NULL);
    
    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", input_data);
        meta_free(imd);
        g_free(data);
        return NULL;
    }
    
    // Scale down to the size we actually want, using the built-in Gdk
    // scaling method, much nicer than what we did above
    GdkPixbuf *pb_s =
        gdk_pixbuf_scale_simple(pb, max_dimension, 
                                max_dimension, GDK_INTERP_BILINEAR);
    gdk_pixbuf_unref(pb);
    
    if (!pb_s) {
        printf("Failed to allocate scaled thumbnail pixbuf: %s\n", input_data);
        meta_free(imd);
        return NULL;
    }

    meta_free(imd);
    return pb_s;
}

static void show_it(const char * filename, int is_new)
{
    GtkWidget *viewed_image;
    static GdkPixbuf *output_pixbuf = NULL;
    static GdkPixbuf *shown_pixbuf = NULL;

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

        int max_size = 2048;

        char *ext = findExt(filename);
        printf("%s Ext: %s\n", filename, ext);
        if (strcmp(ext, ".D") == 0) {
            char *metadata_filename = appendExt(filename, ".L");
            output_pixbuf = make_ceos_image_pixbuf(metadata_filename,
                                                   filename, max_size);
            free(metadata_filename);
        } else if (strcmp(ext, ".img") == 0) {
            char *metadata_filename = appendExt(filename, ".meta");
            printf("Pulling %s\n", metadata_filename);
            output_pixbuf = make_asf_image_pixbuf(metadata_filename,
                                                  filename, max_size);
            free(metadata_filename);
        }

        if ( output_pixbuf == NULL ) {
            printf ("Couldn't open output image: %s\n", filename);
            message_box("Error opening output image");
            return;
        }
    }

    printf("Read in the pixbuf, generating sized version...\n");

    double s = get_scale();
    int w = gdk_pixbuf_get_width(output_pixbuf);
    int h = gdk_pixbuf_get_height(output_pixbuf);

    GtkRequisition rec;
    GtkWidget *viewport = get_widget_checked("viewed_image_viewport");
    gtk_widget_size_request(viewport, &rec);

    int scaled_w = rec.width;
    int scaled_h = rec.height;
    printf("%dx%d -> %dx%d\n", w, h, scaled_w, scaled_h);
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
