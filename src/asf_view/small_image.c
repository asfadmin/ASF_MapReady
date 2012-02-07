#include "asf_view.h"

// size of the smaller "preview" image in the top-left corner
static int THUMB_SIZE = 256;

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

static void put_bounding_box(GdkPixbuf *pixbuf, ImageInfo *ii)
{
    int i, width, height, rowstride, n_channels;
    guchar *pixels, *p;
    const int bb_width = get_big_image_width();
    const int bb_height = get_big_image_height();
    int ns = ii->ns;
    int nl = ii->nl;

    n_channels = gdk_pixbuf_get_n_channels (pixbuf);

    g_assert (gdk_pixbuf_get_colorspace (pixbuf) == GDK_COLORSPACE_RGB);
    g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
    g_assert (!gdk_pixbuf_get_has_alpha (pixbuf));
    g_assert (n_channels == 3);

    width = gdk_pixbuf_get_width (pixbuf);
    height = gdk_pixbuf_get_height (pixbuf);

    rowstride = gdk_pixbuf_get_rowstride (pixbuf);
    pixels = gdk_pixbuf_get_pixels (pixbuf);

    int bb_min_x = (center_samp - bb_width*zoom/2) * width / ns;
    if (bb_min_x < 0) bb_min_x = 0;
    if (bb_min_x > width-1) bb_min_x = width-1;

    int bb_max_x = (center_samp + bb_width*zoom/2) * width / ns;
    if (bb_max_x < 0) bb_max_x = 0;
    if (bb_max_x > width-1) bb_max_x = width-1;

    int bb_min_y = (center_line - bb_height*zoom/2) * height / nl;
    if (bb_min_y < 0) bb_min_y = 0;
    if (bb_min_y > height-1) bb_min_y = height-1;

    int bb_max_y = (center_line + bb_height*zoom/2) * height / nl;
    if (bb_max_y < 0) bb_max_y = 0;
    if (bb_max_y > height-1) bb_max_y = height-1;

    for (i=bb_min_x; i<=bb_max_x; ++i) {
        p = pixels + bb_min_y * rowstride + i * n_channels;
        p[0] = 255;
        p[1] = p[2] = 0;
        p = pixels + bb_max_y * rowstride + i * n_channels;
        p[0] = 255;
        p[1] = p[2] = 0;
    }

    for (i=bb_min_y+1; i<bb_max_y; ++i) {
        p = pixels + i * rowstride + bb_min_x * n_channels;
        p[0] = 255;
        p[1] = p[2] = 0;
        p = pixels + i * rowstride + bb_max_x * n_channels;
        p[0] = 255;
        p[1] = p[2] = 0;
    }
}

GdkPixbuf *pixbuf_small = NULL;

ThumbnailData *get_thumbnail_data(ImageInfo *ii)
{
    assert(ii && ii->data_ci && ii->meta);

    int larger_dim = THUMB_SIZE*4;
    if (larger_dim > ii->meta->general->line_count)
        larger_dim = ii->meta->general->line_count;

    //printf("Larger size: %d\n", larger_dim);

    // Vertical and horizontal scale factors required to meet the
    // max_thumbnail_dimension part of the interface contract.
    int vsf = ceil (ii->nl / larger_dim);
    int hsf = ceil (ii->ns / larger_dim);
    // Overall scale factor to use is the greater of vsf and hsf.
    int sf = (hsf > vsf ? hsf : vsf);

    // Image sizes.
    int tsx = ii->meta->general->sample_count / sf;
    int tsy = ii->meta->general->line_count / sf;
    //printf("Sizes: %d, %d\n", tsx, tsy);

    // this will also calculate the image statistics (estimates)
    unsigned char *data = generate_thumbnail_data(ii, tsx, tsy);

    // now add the mask if we have one
    if (mask) {
        int i, j;
        unsigned char r, g, b;
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int f = (int)cached_image_get_pixel(mask->data_ci, i*sf, j*sf);
                if (apply_mask(f, &r, &g, &b)) {
                    assert(3*(i*tsx+j)+2 < 3*tsx*tsy);
                    data[3*(i*tsx+j)] = r;
                    data[3*(i*tsx+j)+1] = g;
                    data[3*(i*tsx+j)+2] = b;
                }
            }
        }
    }

    ThumbnailData *ret = MALLOC(sizeof(ThumbnailData));
    ret->size_x = tsx;
    ret->size_y = tsy;
    ret->data = data;

    return ret;
}

// had to make these global for the dragging in the small window
int small_image_x_dim;
int small_image_y_dim;

static GdkPixbuf * make_small_image(int force, ThumbnailData *tdata,
                                    ImageInfo *ii)
{
    if (!pixbuf_small || force) {
        if (pixbuf_small) {
            g_object_unref(pixbuf_small);
            pixbuf_small = NULL;
        }

        if (!tdata)
            tdata = get_thumbnail_data(ii);

        int tsx = tdata->size_x;
        int tsy = tdata->size_y;

        // Create the pixbuf
        GdkPixbuf *pb =
          gdk_pixbuf_new_from_data(tdata->data, GDK_COLORSPACE_RGB, FALSE,
                                   8, tsx, tsy, tsx*3, destroy_pb_data, NULL);

        if (!pb)
            asfPrintError("Failed to create the small pixbuf.\n");

        // Scale down to the size we actually want, using the built-in Gdk
        // scaling method, much nicer than what we did above

        // Must ensure we scale the same in each direction
        double scale_y = (double)tsy / THUMB_SIZE;
        double scale_x = (double)tsx / THUMB_SIZE;
        double scale = scale_y > scale_x ? scale_y : scale_x;
        small_image_x_dim = tsx / scale;
        small_image_y_dim = tsy / scale;

        //printf("Scaling to %dx%d\n", small_image_x_dim, small_image_y_dim);

        pixbuf_small =
            gdk_pixbuf_scale_simple(pb, small_image_x_dim, small_image_y_dim,
                                    GDK_INTERP_BILINEAR);
        gdk_pixbuf_unref(pb);

        if (!pixbuf_small)
            asfPrintError("Failed to allocate scaled thumbnail pixbuf\n");

        free(tdata);
    }

    GdkPixbuf *pb2 = gdk_pixbuf_copy(pixbuf_small);
    put_bounding_box(pb2, ii);
    return pb2;
}

void fill_small_force_reload(ImageInfo *ii)
{
    GdkPixbuf *pb = make_small_image(TRUE, NULL, ii);
    GtkWidget *img = get_widget_checked("small_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
}

void fill_small(ImageInfo *ii)
{
    GdkPixbuf *pb = make_small_image(FALSE, NULL, ii);
    GtkWidget *img = get_widget_checked("small_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
}

void fill_small_have_data(ThumbnailData *thumbnail_data,
                          ImageInfo *ii)
{
    GdkPixbuf *pb = make_small_image(TRUE, thumbnail_data, ii);
    GtkWidget *img = get_widget_checked("small_image");
    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
}

void setup_small_image_size()
{
  // The planner will have a larger left-side section, so that the
  // list of found acquisitions can display all the columns.
  if (planner_is_active())
    THUMB_SIZE = 360;
}
