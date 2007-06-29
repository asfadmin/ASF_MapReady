#include "ssv.h"

double avg, stddev;
double stat_max, stat_min;
int hist[256];

// whether the stats have actually been calculated in the background thread
int stats_calced = FALSE;    
// whether the calculated stats have been applied to the gui's stats_label, etc
int stats_generated = FALSE;

static void fill_stats_label()
{
    char s[1024];
    strcpy(s, "");

    // y = m*x + b
    double m = 255.0/(g_max-g_min);
    double b = -g_min*255.0/(g_max-g_min);

    // we will take charge of displaying the sign
    char c = b>0 ? '+' : '-';
    b = fabs(b);

    sprintf(&s[strlen(s)],
        "Average: %.3f\n"
        "Standard Deviation: %.3f\n"
        "Min Value: %.2f\n"
        "Max Value: %.2f\n"
        "Mapping Fn for pixels:\n"
        "  Y = %.3f * X %c %.3f",
        avg, stddev, stat_min, stat_max, m, c, b);

    put_string_to_label("stats_label", s);
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

void calc_stats_thread(gpointer user_data)
{
    int i, j;
    int n_nodata=0;
    int n=0;

    for (i=0; i<256; ++i)
        hist[i] = 0;

    avg = stddev = 0.0;
    stat_max = -99999;
    stat_min = 99999;

    if (meta_is_valid_double(meta->general->no_data)) {
        for (i=0; i<nl; i+=2) {
            for (j=0; j<ns; j+=2) {
                float v = get_pixel(i,j);
                if (v != meta->general->no_data) {
                    avg += v;

                    if (v > stat_max) stat_max = v;
                    if (v < stat_min) stat_min = v;

                    int u = calc_scaled_pixel_value(v);
                    hist[u] += 1;

                    ++n;
                } else {
                    ++n_nodata;
                }
            }
        }
        avg /= (double)n;
        for (i=0; i<nl; i+=2) {
            for (j=0; j<ns; j+=2) {
                float v = get_pixel(i,j);
                if (v != meta->general->no_data)
                    stddev += (v-avg)*(v-avg);
            }
        }
    } else {
        n = nl*ns/4;
        for (i=0; i<nl; i+=2) {
            for (j=0; j<ns; j+=2) {
                float v = get_pixel(i,j);
                avg += v;

                if (v > stat_max) stat_max = v;
                if (v < stat_min) stat_min = v;

                int u = calc_scaled_pixel_value(v);
                hist[u] += 1;
            }
        }
        avg /= (double)n;
        for (i=0; i<nl; i+=2) {
            for (j=0; j<ns; j+=2) {
                float v = get_pixel(i,j);
                stddev += (v-avg)*(v-avg);
            }
        }
    }

    stddev = sqrt(stddev / (double)n);

    stats_calced = TRUE;
}

void calc_image_stats()
{
    asfPrintStatus("Calculating stats asynchronously ...\n");

    static GThreadPool *ttp = NULL;
    GError *err = NULL;

    if (!ttp) {
        if (!g_thread_supported ()) g_thread_init (NULL);
        ttp = g_thread_pool_new ((GFunc)calc_stats_thread, NULL, 4, TRUE, &err);
        g_assert(!err);
    }
    g_thread_pool_push (ttp, "ignored", &err);
    g_assert(!err);
}

static void pop_hist()
{
    int i,j;

    int bin_max = 0;
    for (i=0; i<256; ++i) {
        if (hist[i] > bin_max) bin_max = hist[i];
    }

    const int w = 200;
    unsigned char *histogram_data = MALLOC(sizeof(unsigned char)*256*w*4);
    for (i=0; i<256; ++i) {
        int l = (int)((double)hist[i] / (double)bin_max * (double)w);
        for (j=0; j<l*4; j += 4) {
            histogram_data[j+i*w*4] = (unsigned char)0;
            histogram_data[j+i*w*4+1] = (unsigned char)0;
            histogram_data[j+i*w*4+2] = (unsigned char)0;
            histogram_data[j+i*w*4+3] = (unsigned char)255; // alpha
        }
        for (j=l*4; j<w*4; j+=4) {
            histogram_data[j+i*w*4] = (unsigned char)0;
            histogram_data[j+i*w*4+1] = (unsigned char)0;
            histogram_data[j+i*w*4+2] = (unsigned char)0;
            histogram_data[j+i*w*4+3] = (unsigned char)0;   // alpha
        }
    }

    GdkPixbuf *pb = gdk_pixbuf_new_from_data(histogram_data,
        GDK_COLORSPACE_RGB, TRUE, 8, w, 256, w*4, destroy_pb_data, NULL);

    GtkWidget *img = get_widget_checked("histogram_image");

    GdkPixbuf *old_pb = gtk_image_get_pixbuf(GTK_IMAGE(img));
    g_object_unref(old_pb);

    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);

}

int fill_stats()
{
    {
        int i, j;

        const int w = 12; // width of the little "scale" image
        unsigned char *histogram_scale_data = MALLOC(sizeof(unsigned char)*256*w*3);
        for (i=0; i<256; ++i) {
            for (j=0; j<w*3; ++j) {
                histogram_scale_data[j+i*w*3] = (unsigned char)i;
            }
        }

        GdkPixbuf *pb = gdk_pixbuf_new_from_data(histogram_scale_data,
            GDK_COLORSPACE_RGB, FALSE, 8, w, 256, w*3, destroy_pb_data, NULL);

        GtkWidget *img = get_widget_checked("histogram_scale_image");

        GdkPixbuf *old_pb = gtk_image_get_pixbuf(GTK_IMAGE(img));
        g_object_unref(old_pb);

        gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);
    }

    // stats may not have been fully calculated yet...
    if (!stats_calced) {
        // it usually doesn't take that long...
        // wait a sec and try it again
        g_usleep(500000);
        if (!stats_calced) {
            // tell the user to try again later.
            put_string_to_label("stats_label", "");

            int i;
            for (i=0; i<256; ++i)
                hist[i] = 0;
            pop_hist();

            return FALSE;
        }
    }

    fill_stats_label();
    pop_hist();

    return TRUE;
}

SIGNAL_CALLBACK int
on_change_current_page(GtkNotebook *w, GtkNotebookPage *p, guint page_num,
                       gpointer user_data)
{
    if (page_num == 1 && !stats_generated) {
        if (fill_stats())
            stats_generated = TRUE;
    }
    return TRUE;
}
