#include "asf_view.h"

// global variable for the image stats
// ImageStats g_stats;

void clear_stats()
{
    curr->stats.avg = 0.0;
    curr->stats.stddev = 0.0;

    curr->stats.act_max = 0.0;
    curr->stats.act_min = 0.0;

    curr->stats.map_min = 0.0;
    curr->stats.map_max = 0.0;

    int ii;
    for (ii=0; ii<256; ++ii)
        curr->stats.hist[ii] = 0;
}

// Now the thumbnail generation is combined with the stats calculations,
// to speed things up a little.  Both require a pass through the entire
// image, so it seems natural, and the stats calculation doesn't add much
// overhead.  (The user is waiting while the thumbnail is being generated,
// so we do want this to be as quick as possible.)
unsigned char *generate_thumbnail_data(int tsx, int tsy)
{
    int ii, jj;
    unsigned char *bdata = MALLOC(sizeof(unsigned char)*tsx*tsy*3);

    // we will estimate the stats from the thumbnail data
    clear_stats();

    // Here we do the rather ugly thing of making the thumbnail
    // loading code specific to each supported data type.  This is
    // because we've combined the stats calculation into this...
    if (curr->data_ci->data_type == GREYSCALE_FLOAT) {
        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        float *fdata = MALLOC(sizeof(float)*tsx*tsy);

        load_thumbnail_data(curr->data_ci, tsx, tsy, fdata);

        // split out the case where we have no ignore value --
        // should be quite a bit faster...
        if (meta_is_valid_double(curr->meta->general->no_data)) {
            // Compute stats -- ignore "no data" value
            int n=0;
            for ( ii = 0 ; ii < tsy ; ii++ ) {
                for ( jj = 0 ; jj < tsx ; jj++ ) {
                    float v = fdata[jj+ii*tsx];
                    if (meta_is_valid_double(v) &&
                        v != curr->meta->general->no_data)
                    {
                        curr->stats.avg += v;

                        // first valid pixel --> initialize max/min
                        // subsequent pixels --> update max/min if needed
                        if (n==0) {
                            curr->stats.act_max = curr->stats.act_min = v;
                        } else {
                            if (v > curr->stats.act_max)
                              curr->stats.act_max = v;
                            if (v < curr->stats.act_min)
                              curr->stats.act_min = v;
                        }
                        ++n;
                    }
                }
            }
            curr->stats.avg /= (double)n;
            for ( ii = 0 ; ii < tsy ; ii++ ) {
                for ( jj = 0 ; jj < tsx ; jj++ ) {
                    float v = fdata[jj+ii*tsx];
                    if (meta_is_valid_double(v) && v != curr->meta->general->no_data)
                        curr->stats.stddev +=
                          (v - curr->stats.avg) * (v - curr->stats.avg);
                }
            }
            curr->stats.stddev = sqrt(curr->stats.stddev / (double)n);
        } else {
            // Compute stats, no ignore (actually, do ignore data that is NaN)
            curr->stats.act_max = curr->stats.act_min = fdata[0];
            for ( ii = 0 ; ii < tsy ; ii++ ) {
                for ( jj = 0 ; jj < tsx ; jj++ ) {
                    float v = fdata[jj+ii*tsx];
                    if (meta_is_valid_double(v)) {
                        curr->stats.avg += v;
                        if (v > curr->stats.act_max) curr->stats.act_max = v;
                        if (v < curr->stats.act_min) curr->stats.act_min = v;
                    }
                }
            }
            curr->stats.avg /= (double)(tsx*tsy);
            for ( ii = 0 ; ii < tsy ; ii++ ) {
                for ( jj = 0 ; jj < tsx ; jj++ ) {
                    float v = fdata[jj+ii*tsx];
                    if (meta_is_valid_double(v))
                        curr->stats.stddev +=
                          (v - curr->stats.avg) * (v - curr->stats.avg);
                }
            }
            curr->stats.stddev = sqrt(curr->stats.stddev / (double)(tsx*tsy));
        }

        //printf("Avg, StdDev: %f, %f\n", curr->stats.avg, curr->stats.stddev);

        // Set the limits of the scaling - 2-sigma on either side of the mean
        // These are globals, we will use them in the big image, too.
        curr->stats.map_min = curr->stats.avg - 2*curr->stats.stddev;
        curr->stats.map_max = curr->stats.avg + 2*curr->stats.stddev;

        // Now actually scale the data, and convert to bytes.
        // Note that we need 3 values, one for each of the RGB channels.
        int have_no_data = meta_is_valid_double(curr->meta->general->no_data);
        if (have_lut()) {
            // look up table case -- no scaling, just use the lut
            // to convert from float to rgb byte
            for ( ii = 0 ; ii < tsy ; ii++ ) {
                for ( jj = 0 ; jj < tsx ; jj++ ) {
                    int index = jj+ii*tsx;
                    int n = 3*index;
                    float val = fdata[index];

                    int ival;
                    if (!meta_is_valid_double(val) ||
                        (have_no_data && val==curr->meta->general->no_data) ||
                        val < 0)
                        ival = 0;
                    else
                        ival = (int)val;

                    apply_lut(ival, &bdata[n], &bdata[n+1], &bdata[n+2]);

                    // histogram will appear as if we were scaling
                    // to greyscale byte
                    unsigned char uval;
                    if (ival <= 0 || val < curr->stats.map_min)
                        uval = 0;
                    else if (val > curr->stats.map_max)
                        uval = 255;
                    else
                        uval = (unsigned char)(((val-curr->stats.map_min)/(curr->stats.map_max-curr->stats.map_min))*255+0.5);

                    curr->stats.hist[uval] += 1;
                }
            }

        } else {
            // normal case -- no lut, apply 2-sigma scaling to convert from
            // floating point to byte for display
            for ( ii = 0 ; ii < tsy ; ii++ ) {
                for ( jj = 0 ; jj < tsx ; jj++ ) {
                    int index = jj+ii*tsx;
                    float val = fdata[index];

                    unsigned char uval;
                    if (!meta_is_valid_double(val))
                        uval = 0;
                    else if (have_no_data && val == curr->meta->general->no_data)
                        uval = 0;
                    else if (val < curr->stats.map_min)
                        uval = 0;
                    else if (val > curr->stats.map_max)
                        uval = 255;
                    else
                        uval = (unsigned char)(((val-curr->stats.map_min)/(curr->stats.map_max-curr->stats.map_min))*255+0.5);
                
                    int n = 3*index;
                    bdata[n] = uval;
                    bdata[n+1] = uval;
                    bdata[n+2] = uval;

                    curr->stats.hist[uval] += 1;
                }
            }
        }

        // done with our subset
        free(fdata);
    }
    else if (curr->data_ci->data_type == RGB_BYTE) {

        load_thumbnail_data(curr->data_ci, tsx, tsy, (void*)bdata);

        // initialize to opposite extrema
        curr->stats.act_max = 0;
        curr->stats.act_min = 255;

        // don't really have a mapping for byte data...
        curr->stats.map_min = 0;
        curr->stats.map_max = 255;

        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                int kk = 3*(jj+ii*tsx);
                unsigned char uval = 
                    (bdata[kk] + bdata[kk+1] + bdata[kk+2])/3;

                curr->stats.avg += uval;
                curr->stats.hist[uval] += 1;

                if (uval > curr->stats.act_max) curr->stats.act_max = uval;
                if (uval < curr->stats.act_min) curr->stats.act_min = uval;
            }
        }

        curr->stats.avg /= (double)(tsx*tsy);

        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                int kk = 3*(jj+ii*tsx);
                unsigned char uval = 
                    (bdata[kk] + bdata[kk+1] + bdata[kk+2])/3;

                curr->stats.stddev +=
                  (uval - curr->stats.avg) * (uval - curr->stats.avg);
            }
        }
        curr->stats.stddev = sqrt(curr->stats.stddev / (double)(tsx*tsy));

    }
    else if (curr->data_ci->data_type == GREYSCALE_BYTE) {

        // this case is very similar to the RGB case, above, except we
        // have to first grab the data into a greyscale buffer, and
        // then copy it over to the 3-band buffer we're supposed to return
        unsigned char *gsdata = MALLOC(sizeof(unsigned char)*tsx*tsy);
        load_thumbnail_data(curr->data_ci, tsx, tsy, (void*)gsdata);

        curr->stats.act_max = 0;
        curr->stats.act_min = 255;
        curr->stats.map_min = 0;
        curr->stats.map_max = 255;

        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                unsigned char uval = gsdata[jj+ii*tsx];
                int kk = 3*(jj+ii*tsx);

                if (have_lut()) {
                    apply_lut(uval, &bdata[kk], &bdata[kk+1], &bdata[kk+2]);
                } else {
                    bdata[kk] = bdata[kk+1] = bdata[kk+2] = uval;
                }

                curr->stats.avg += uval;
                curr->stats.hist[uval] += 1;

                if (uval > curr->stats.act_max) curr->stats.act_max = uval;
                if (uval < curr->stats.act_min) curr->stats.act_min = uval;
            }
        }

        curr->stats.avg /= (double)(tsx*tsy);

        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                unsigned char uval = gsdata[jj+ii*tsx];
                curr->stats.stddev +=
                  (uval - curr->stats.avg) * (uval - curr->stats.avg);
            }
        }
        curr->stats.stddev = sqrt(curr->stats.stddev / (double)(tsx*tsy));

        free(gsdata);
    }
    else if (curr->data_ci->data_type == RGB_FLOAT) {
        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        float *fdata = MALLOC(sizeof(float)*tsx*tsy*3);
        load_thumbnail_data(curr->data_ci, tsx, tsy, fdata);

        curr->stats.act_max = curr->stats.act_min = fdata[0];

        int have_nd = meta_is_valid_double(curr->meta->general->no_data);
        double nd = curr->meta->general->no_data;

        // Use the average of the three RGB channels
        int n=0;
        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                int kk = 3*(jj+ii*tsx);

                int is_valid_data = 
                    meta_is_valid_double(fdata[kk]) &&
                    meta_is_valid_double(fdata[kk+1]) &&
                    meta_is_valid_double(fdata[kk+2]) &&
                    (!have_nd ||
                        (fdata[kk]!=nd && fdata[kk+1]!=nd && fdata[kk+2]!=nd));
    
                if (is_valid_data)
                {
                    float v = (fdata[kk]+fdata[kk+1]+fdata[kk+2])/3;
                    curr->stats.avg += v;
                    if (v > curr->stats.act_max) curr->stats.act_max = v;
                    if (v < curr->stats.act_min) curr->stats.act_min = v;
                    ++n;
                }
            }
        }
        curr->stats.avg /= (double)n;
        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                int kk = 3*(jj+ii*tsx);

                int is_valid_data = 
                    meta_is_valid_double(fdata[kk]) &&
                    meta_is_valid_double(fdata[kk+1]) &&
                    meta_is_valid_double(fdata[kk+2]) &&
                    (!have_nd ||
                        (fdata[kk]!=nd && fdata[kk+1]!=nd && fdata[kk+2]!=nd));

                if (is_valid_data)
                {
                    float v = (fdata[kk]+fdata[kk+1]+fdata[kk+2])/3;
                    curr->stats.stddev +=
                      (v - curr->stats.avg) * (v - curr->stats.avg);
                }
            }
        }
        curr->stats.stddev = sqrt(curr->stats.stddev / (double)n);
        curr->stats.map_min = curr->stats.avg - 2*curr->stats.stddev;
        curr->stats.map_max = curr->stats.avg + 2*curr->stats.stddev;

        // Scale the data
        for (ii=0; ii<tsx*tsy*3; ++ii) {
            if (!meta_is_valid_double(fdata[ii]))
                bdata[ii] = 0;
            else if (have_nd && fdata[ii] == nd)
                bdata[ii] = 0;
            else if (fdata[ii] < curr->stats.map_min)
                bdata[ii] = 0;
            else if (fdata[ii] > curr->stats.map_max)
                bdata[ii] = 255;
            else
                bdata[ii] = (unsigned char)(((fdata[ii]-curr->stats.map_min)/(curr->stats.map_max-curr->stats.map_min))*255+0.5);
        }

        // Update the histogram
        for ( ii = 0 ; ii < tsy ; ii++ ) {
            for ( jj = 0 ; jj < tsx ; jj++ ) {
                int kk = 3*(jj+ii*tsx);
                unsigned char uval = (bdata[kk]+bdata[kk+1]+bdata[kk+2])/3;
                curr->stats.hist[uval] += 1;
            }
        }

        // done with our subset
        free(fdata);
    }
    else {
	    asfPrintError("Unexpected data type: %d!\n",
                          curr->data_ci->data_type);
    }

    return bdata;
}

int calc_scaled_pixel_value(float val)
{
    if (meta_is_valid_double(curr->meta->general->no_data) && 
        val == curr->meta->general->no_data)
        return 0;
    if (val < curr->stats.map_min)
        return 0;
    else if (val > curr->stats.map_max)
        return 255;
    else
        return (int) round(((val-curr->stats.map_min)/
            (curr->stats.map_max-curr->stats.map_min))*255);
}

static void fill_stats_label()
{
    char s[1024];
    strcpy(s, "");

    // y = m*x + b
    double m = 255.0/(curr->stats.map_max-curr->stats.map_min);
    double b = -curr->stats.map_min*255.0/
      (curr->stats.map_max-curr->stats.map_min);

    // we will take charge of displaying the sign
    char c = b>0 ? '+' : '-';
    b = fabs(b);

    // Not sure we should put the Max/Min in here... after all, these
    // are only from a subset.  The aggregate values (avg, stddev, mapping)
    // will be fine, but max/min could be quite far off, if there are
    // an outlier or two.
    sprintf(&s[strlen(s)],
        "Average: %.3f\n"
        "Standard Deviation: %.3f\n"
        "Min Value: %.2f\n"
        "Max Value: %.2f\n"
        "Mapping Fn for pixels:\n"
        "  Y = %.3f * X %c %.3f",
        curr->stats.avg, curr->stats.stddev,
        curr->stats.act_min, curr->stats.act_max, 
        m, c, b);

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
        if (curr->stats.hist[i] > bin_max) bin_max = curr->stats.hist[i];
    }

    const int w = 200;
    unsigned char *histogram_data = MALLOC(sizeof(unsigned char)*256*w*4);
    for (i=0; i<256; ++i) {
        int l = (int)((double)curr->stats.hist[i] / 
                      (double)bin_max * (double)w);
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
    if (old_pb)
        g_object_unref(old_pb);

    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);

}

int fill_stats()
{
    int i, j;

    const int w = 12; // width of the little "scale" image
    unsigned char *histogram_scale_data = 
      MALLOC(sizeof(unsigned char)*256*w*3);

    for (i=0; i<256; ++i) {
        for (j=0; j<w*3; ++j) {
            histogram_scale_data[j+i*w*3] = (unsigned char)i;
        }
    }

    GdkPixbuf *pb = gdk_pixbuf_new_from_data(histogram_scale_data,
        GDK_COLORSPACE_RGB, FALSE, 8, w, 256, w*3, destroy_pb_data, NULL);

    GtkWidget *img = get_widget_checked("histogram_scale_image");

    GdkPixbuf *old_pb = gtk_image_get_pixbuf(GTK_IMAGE(img));
    if (old_pb)
        g_object_unref(old_pb);

    gtk_image_set_from_pixbuf(GTK_IMAGE(img), pb);

    fill_stats_label();
    pop_hist();

    return TRUE;
}
