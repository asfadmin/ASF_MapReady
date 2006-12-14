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
#include <gdk/gdkkeysyms.h>

#include <ceos_io.h>

static const int COL_DATA_FILE = 0;
static const int COL_DATA_FILE_FULL = 1; // hidden column, contains full path
static const int COL_EXISTS = 2;

static double constant_term = 0.0;
static double linear_term = 1.0;
static GdkPixbuf *output_pixbuf = NULL;
static GdkPixbuf *shown_pixbuf = NULL;
static meta_parameters *meta = NULL;
static double scale_factor = 1.0;
static int crosshair_x = -1;
static int crosshair_y = -1;

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

    if (path[strlen(path)-1]==DIR_SEPARATOR)
        sprintf(f, "%s%s", path, data_file);
    else
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
    gtk_tree_view_column_set_title(col, "Done?");
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

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    g_free(pixels);
}

static void update_image_stats(double avg, double stddev,
                               double lmin, double lmax)
{
    GtkWidget *lbl = get_widget_checked("image_info_label");

    char buf[256];
    linear_term = 255.0/(lmax-lmin);
    constant_term = -lmin*linear_term;

    sprintf(buf, "Mean: %f\nStd Dev: %f\n"
            "Mapping: y = %.2f * x %c %.2f\n",
            avg, stddev, linear_term, constant_term > 0 ? '+' : '-',
            fabs(constant_term));

    gtk_label_set_text(GTK_LABEL(lbl), buf);
}

static void update_size_info(int line_count, int sample_count,
                             int scaled_height, int scaled_width)
{
    GtkWidget *lbl = get_widget_checked("image_size_label");

    char buf[256];
    sprintf(buf, "Size: %dx%d LxS -> %dx%d LxS",
            line_count, sample_count, scaled_height, scaled_width);

    gtk_label_set_text(GTK_LABEL(lbl), buf);
}

static GdkPixbuf *
make_ceos_image_pixbuf (const char *input_metadata, 
                        const char *input_data,
                        size_t max_dimension,
                        meta_parameters **meta)
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
    int larger_dim = max_dimension;

    // Vertical and horizontal scale factors required to meet the
    // max_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : (vsf > 0 ? vsf : 1));

    // Thumbnail image sizes.
    size_t tsx = imd->general->sample_count / sf;
    size_t tsy = imd->general->line_count / sf;

    // Thumbnail image buffers - 'fdata' is the temporary data prior
    // to scaling to 2-sigma, 'data' is the byte buffer used to create the
    // pixbuf, it will need 3 bytes per value, all equal, since the pixbuf
    // wants an RGB value.
    float *fdata = g_new(float, tsx*tsy);
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

            fdata[ii*tsx + jj] = (float)csv;
            avg += csv;
        }
    }
    g_free (line);
    closeCeos(id);

    // Compute the std devation
    avg /= tsx*tsy;
    double stddev = 0.0;
    for (ii = 0; ii < tsx*tsy; ++ii)
        stddev += ((double)fdata[ii] - avg) * ((double)fdata[ii] - avg);
    stddev = sqrt(stddev / (tsx*tsy));
    
    // Set the limits of the scaling - 2-sigma on either side of the mean
    double lmin = avg - 2*stddev;
    double lmax = avg + 2*stddev;
    
    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    for (ii = 0; ii < tsx*tsy; ++ii) {
        float val = fdata[ii];
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
    
    g_free(fdata);
    
    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE, 
                                 8, tsx, tsy, tsx*3, destroy_pb_data, NULL);

    update_image_stats(avg, stddev, lmin, lmax);
    *meta = imd;
    
    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", input_data);
        g_free(data);
    }

    return pb;
}

static GdkPixbuf *
make_asf_image_pixbuf (const char *input_metadata, 
                       const char *input_data,
                       size_t max_dimension,
                       meta_parameters **meta)
{
    FILE *img = fopenImage(input_data, "rb");
    if (!img) {
        // failed for some reason, just quit without thumbnailing...
        // possibly the file is bad, or perhaps it was removed from the
        // file list before we could get around to thumbnailing it.
        return NULL;
    }

    meta_parameters *imd = meta_read (input_metadata); // Input metadata.

    // use a larger dimension at first, for our crude scaling.  We will
    // use a better scaling method later, from GdbPixbuf
    int larger_dim = max_dimension;

    // Vertical and horizontal scale factors required to meet the
    // max_dimension part of the interface contract.
    int vsf = ceil (imd->general->line_count / larger_dim);
    int hsf = ceil (imd->general->sample_count / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : (vsf > 0 ? vsf : 1));
    printf("Scale factor: %d\n", sf);

    // Thumbnail image sizes.
    int tsx = imd->general->sample_count / sf;
    int tsy = imd->general->line_count / sf;

    // Thumbnail image buffers - 'fdata' is the temporary data prior
    // to scaling to 2-sigma, 'data' is the byte buffer used to create the
    // pixbuf, it will need 3 bytes per value, all equal, since the pixbuf
    // wants an RGB value.
    float *fdata = g_new(float, tsx*tsy);
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
            if ( jj * sf < imd->general->sample_count - 1 ) {
                csv = (line[jj * sf] + line[jj * sf + 1]) / 2;
            }
            else {
                csv = (line[jj * sf] + line[jj * sf - 1]) / 2;
            }

            fdata[ii*tsx + jj] = (float)csv;
            avg += csv;
        }
    }
    g_free (line);
    fclose(img);

    // Compute the std devation
    avg /= tsx*tsy;
    double stddev = 0.0;
    for (ii = 0; ii < tsx*tsy; ++ii)
        stddev += ((double)fdata[ii] - avg) * ((double)fdata[ii] - avg);
    stddev = sqrt(stddev / (tsx*tsy));
    
    // Set the limits of the scaling - 2-sigma on either side of the mean
    double lmin = avg - 2*stddev;
    double lmax = avg + 2*stddev;

    // Now actually scale the data, and convert to bytes.
    // Note that we need 3 values, one for each of the RGB channels.
    for (ii = 0; ii < tsx*tsy; ++ii) {
        float val = fdata[ii];
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
    
    g_free(fdata);
    
    // Create the pixbuf
    GdkPixbuf *pb =
        gdk_pixbuf_new_from_data(data, GDK_COLORSPACE_RGB, FALSE, 
                                 8, tsx, tsy, tsx*3, destroy_pb_data, NULL);

    update_image_stats(avg, stddev, lmin, lmax);
    *meta = imd;

    if (!pb) {
        printf("Failed to create the thumbnail pixbuf: %s\n", input_data);
        g_free(data);
    }

    return pb;
}

static void show_loading_label(int show)
{
    return;

    GtkWidget *loading_label = get_widget_checked("loading_label");
    GtkWidget *viewed_image = get_widget_checked("viewed_image");

    if (show) {
        gtk_widget_show(loading_label);
        gtk_widget_hide(viewed_image);
    } else {
        gtk_widget_hide(loading_label);
        gtk_widget_show(viewed_image);
    }
}

static void put_crosshair (GdkPixbuf *pixbuf)
{
    if (crosshair_x < 0 || crosshair_y < 0)
        return;

    int i, lo, hi;
    int width, height, rowstride, n_channels;
    guchar *pixels, *p;
    
    n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    
    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);
    
    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);
    
    g_assert (crosshair_x >= 0 && crosshair_x < width);
    g_assert (crosshair_y >= 0 && crosshair_y < height);

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    lo = crosshair_x - 15;   if (lo < 0)     lo = 0;
    hi = crosshair_x + 15;   if (hi > width) hi = width;

    for (i = lo; i < hi; ++i) {
        if (i > crosshair_x-3 && i < crosshair_x+3) i = crosshair_x+3;
        p = pixels + crosshair_y * rowstride + i * n_channels;
        p[1] = 255;
        p[0] = p[2] = 0;
    }

    lo = crosshair_y - 15;   if (lo < 0)      lo = 0;
    hi = crosshair_y + 15;   if (hi > height) hi = height;

    for (i = lo; i < hi; ++i) {
        if (i > crosshair_y-3 && i < crosshair_y+3) i = crosshair_y+3;
        p = pixels + i * rowstride + crosshair_x * n_channels;
        p[1] = 255;
        p[0] = p[2] = 0;
    }
}

static void show_it(const char * filename, int is_new)
{
    GtkWidget *viewed_image;

    if (!output_pixbuf && !filename) return;
 
    show_loading_label(TRUE);

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
        assert(meta);
        assert(!filename);
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
                                                   filename, max_size, &meta);
            free(metadata_filename);
        } else if (strcmp(ext, ".img") == 0) {
            char *metadata_filename = appendExt(filename, ".meta");
            printf("Pulling %s\n", metadata_filename);
            output_pixbuf = make_asf_image_pixbuf(metadata_filename,
                                                  filename, max_size, &meta);
            free(metadata_filename);
        }

        if (!output_pixbuf) {
            printf ("Couldn't open output image: %s\n", filename);
            message_box("Error opening output image");
            return;
        }

        // clear out the "clicked pixel"
        GtkWidget *pixel_info_label = get_widget_checked("pixel_info_label");
        gtk_label_set_text(GTK_LABEL(pixel_info_label), "");
        crosshair_x = crosshair_y = -1;

        printf("Read in the pixbuf, generating sized version...\n");
    }

    int w = gdk_pixbuf_get_width(output_pixbuf);
    int h = gdk_pixbuf_get_height(output_pixbuf);

    int scaled_w;
    int scaled_h;
    GtkWindow *win = (GtkWindow*)get_widget_checked("ait_main");

    gtk_window_get_size(win, &scaled_w, &scaled_h);

    // I got these by empirical trial & error... we can only get the width
    // of the full window, but we want just the width of the GtkImage.  So,
    // we subtract off the mostly-static width & heights of the "other stuff"
    // A kludge... need to find a better way.  FIXME.
    scaled_w -= 522;
    scaled_h -= 85;

    double sw = scaled_w/(double)w;
    double sh = scaled_h/(double)h;

    scale_factor = sw > sh ? sh : sw;

    scaled_w = (int)(scale_factor * w);
    scaled_h = (int)(scale_factor * h);

    update_size_info(meta->general->line_count, meta->general->sample_count,
                     scaled_w, scaled_h);

    int bps = gdk_pixbuf_get_bits_per_sample(output_pixbuf);
    shown_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE,
                                  bps, scaled_w, scaled_h);
    GdkInterpType interp = scale_factor > 0.2  ?
        GDK_INTERP_BILINEAR : GDK_INTERP_NEAREST;
    gdk_pixbuf_scale(output_pixbuf, shown_pixbuf, 0, 0, scaled_w, scaled_h,
                     0, 0, scale_factor, scale_factor, interp);

    put_crosshair(shown_pixbuf);

    viewed_image = get_widget_checked("viewed_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(viewed_image), shown_pixbuf);
    //gtk_image_set_from_pixbuf(GTK_IMAGE(viewed_image), output_pixbuf);

    if (is_new)
    {
        GtkWidget *displayed_filename_label =
            get_widget_checked("displayed_filename_label");

        gtk_label_set_text(GTK_LABEL(displayed_filename_label), filename);
    }

    show_loading_label(FALSE);
}

void show_output_image(const char * filename)
{
    show_it(filename, TRUE);
}

SIGNAL_CALLBACK void on_resize(GtkWidget *w)
{
    //printf("signal!!\n");
    show_it(NULL, FALSE);
}

static guchar get_pixel (GdkPixbuf *pixbuf, int x, int y)
{
    int width, height, rowstride, n_channels;
    guchar *pixels, *p;
    
    n_channels = gdk_pixbuf_get_n_channels (pixbuf);
    
    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);
    
    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);
    
    g_assert (x >= 0 && x < width);
    g_assert (y >= 0 && y < height);
    
    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);
    
    p = pixels + y * rowstride + x * n_channels;
    return p[0];
}

static void update_pixel_info()
{
    char buf[256];
    GtkWidget *lbl = get_widget_checked("pixel_info_label");

    int w = gdk_pixbuf_get_width(shown_pixbuf);
    int h = gdk_pixbuf_get_height(shown_pixbuf);

    if (crosshair_x < 0 ||
        crosshair_x >= w || 
        crosshair_y < 0 || 
        crosshair_y >= h)
    {
        // outside of the image
        strcpy(buf, "");
        crosshair_x = crosshair_y = 0;
    }
    else
    {
        assert(meta);
        assert(shown_pixbuf);
        
        int line = crosshair_y / scale_factor;
        int sample = crosshair_x / scale_factor;
        
        guchar uval = get_pixel(shown_pixbuf, crosshair_x, crosshair_y);
        float fval = ((float)uval - constant_term)/linear_term;
        
        sprintf(buf,
                "Clicked: x=%d y=%d\n"
                "Line: %d, Sample: %d\n"
                "Pixel Value: %.2f -> %d\n",
                crosshair_x, crosshair_y, line, sample, fval, (int)uval);
    }

    gtk_label_set_text(GTK_LABEL(lbl), buf);
}

SIGNAL_CALLBACK int
on_viewed_image_eventbox_button_press_event(
    GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    crosshair_x = (int)event->x;
    crosshair_y = (int)event->y;
    update_pixel_info();
    show_it(NULL, FALSE);
    return TRUE;
}

SIGNAL_CALLBACK int
on_viewed_image_eventbox_key_press_event(
    GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    switch (event->keyval) {
        case GDK_Up: --crosshair_y; break;
        case GDK_Down: ++crosshair_y; break;
        case GDK_Left: --crosshair_x; break;
        case GDK_Right: ++crosshair_x; break;
        default: return TRUE;
    }

    update_pixel_info();
    show_it(NULL, FALSE);
    return TRUE;
}

static void show_metadata(char *metadata_filename)
{
    GtkWidget *metadata_textview = get_widget_checked("metadata_textview");
    GtkTextBuffer *text_buffer =
        gtk_text_view_get_buffer(GTK_TEXT_VIEW(metadata_textview));

    // clear out current contents
    gtk_text_buffer_set_text(text_buffer, "", -1);

    // read the metadata file, populate!
    FILE *metadata_file = fopen(metadata_filename, "rt");
    const int max_line_len = 256;

    if (metadata_file)
    {
        char *buffer = (char *)MALLOC(sizeof(char) * max_line_len);
        while (!feof(metadata_file))
        {
            char *p = fgets(buffer, max_line_len, metadata_file);
            if (p)
            {
                GtkTextIter end;

                gtk_text_buffer_get_end_iter(text_buffer, &end);
                gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
            }
        }

        fclose(metadata_file);
        free(buffer);

        /* change to a fixed-width font in the window */
        GtkTextIter start, end;
        static GtkTextTag *tt = NULL;

        if (!tt)
        {
#ifdef win32
            const char *fnt = "Courier";
#else
            const char *fnt = "Mono";
#endif
            tt = gtk_text_buffer_create_tag(text_buffer, "mono",
                                            "font", fnt, NULL);
        }

        gtk_text_buffer_get_start_iter(text_buffer, &start);
        gtk_text_buffer_get_end_iter(text_buffer, &end);

        gtk_text_buffer_apply_tag(text_buffer, tt, &start, &end);    

        GtkWidget *metadata_label = get_widget_checked("metadata_label");
        gtk_label_set_text(GTK_LABEL(metadata_label), metadata_filename);
    }
    else
    {
        char *buf = MALLOC(sizeof(char)*(strlen(metadata_filename) + 64));
        sprintf(buf, "Error opening metadata file: %s", metadata_filename);
        gtk_text_buffer_set_text(text_buffer, buf, -1);
        free(buf);

        GtkWidget *metadata_label = get_widget_checked("metadata_label");
        gtk_label_set_text(GTK_LABEL(metadata_label), metadata_filename);
    }
}

SIGNAL_CALLBACK void on_view_button_clicked(GtkWidget *w)
{
    GtkWidget *images_treeview;
    GtkTreeIter iter;

    images_treeview = get_widget_checked("images_treeview");
    if (get_iter_to_first_selected_row(images_treeview, &iter))
    {
        GtkWidget *metadata_label = get_widget_checked("metadata_label");
        GtkWidget *metadata_textview =
            get_widget_checked("metadata_textview");
        GtkTextBuffer *tb =
            gtk_text_view_get_buffer(GTK_TEXT_VIEW(metadata_textview));

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

        char *meta_name = meta_file_name(file_name);
        char *ext = findExt(meta_name);
        if (strcmp(ext, ".L") == 0)
        {
            gtk_text_buffer_set_text(tb, 
                "Display of CEOS Metadata not yet supported!", -1);
            gtk_label_set_text(GTK_LABEL(metadata_label), meta_name);            
        }
        else if (g_file_test(meta_name, G_FILE_TEST_EXISTS))
        {
            show_metadata(meta_name);
        }
        else
        {
            char *buf = MALLOC(sizeof(char)*(strlen(meta_name) + 64));
            sprintf(buf, "Metadata file not found: %s", meta_name);
            gtk_text_buffer_set_text(tb, buf, -1);
            free(buf);

            gtk_label_set_text(GTK_LABEL(metadata_label), meta_name);
        }

        free(meta_name);
    }
    else
    {
        show_please_select_message();
    }
}

static void update_done_column()
{
    if (images_list)
    {
        GtkTreeIter iter;
        int ok = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(images_list), &iter);
        while (ok)
        {
            char *filename;
            gtk_tree_model_get(GTK_TREE_MODEL(images_list), &iter, 
                COL_DATA_FILE_FULL, &filename, -1);

            int file_exists = fileExists(filename);
            const char *exists = file_exists ? "Yes" : "No";
            printf("%s? %s\n", filename, exists);

            gtk_list_store_set(images_list, &iter, COL_EXISTS, exists, -1);

            g_free(filename);

            ok = gtk_tree_model_iter_next(GTK_TREE_MODEL(images_list), &iter);
        }
    }
}

SIGNAL_CALLBACK void on_refresh_button_clicked(GtkWidget *w)
{
    update_done_column();
}
