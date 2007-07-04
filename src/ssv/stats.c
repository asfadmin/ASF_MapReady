#include "ssv.h"

double g_avg, g_stddev;
double g_stat_max, g_stat_min;
int g_hist[256];

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
        g_avg, g_stddev, g_stat_min, g_stat_max, m, c, b);

    put_string_to_label("stats_label", s);
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

static void pop_hist()
{
    int i,j;

    int bin_max = 0;
    for (i=0; i<256; ++i) {
        if (g_hist[i] > bin_max) bin_max = g_hist[i];
    }

    const int w = 200;
    unsigned char *histogram_data = MALLOC(sizeof(unsigned char)*256*w*4);
    for (i=0; i<256; ++i) {
        int l = (int)((double)g_hist[i] / (double)bin_max * (double)w);
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

    fill_stats_label();
    pop_hist();

    return TRUE;
}

SIGNAL_CALLBACK int
on_change_current_page(GtkNotebook *w, GtkNotebookPage *p, guint page_num,
                       gpointer user_data)
{
    return TRUE;
}
