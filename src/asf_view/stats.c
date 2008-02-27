#include "asf_view.h"

// global variable for the image stats
// ImageStats g_stats;

void clear_stats(ImageInfo *ii)
{
    ii->stats.avg = 0.0;
    ii->stats.stddev = 0.0;

    ii->stats.act_max = 0.0;
    ii->stats.act_min = 0.0;

    ii->stats.map_min = 0.0;
    ii->stats.map_max = 0.0;

    int i;
    for (i=0; i<256; ++i)
        ii->stats.hist[i] = 0;
}

// Now the thumbnail generation is combined with the stats calculations,
// to speed things up a little.  Both require a pass through the entire
// image, so it seems natural, and the stats calculation doesn't add much
// overhead.  (The user is waiting while the thumbnail is being generated,
// so we do want this to be as quick as possible.)
unsigned char *generate_thumbnail_data(ImageInfo *ii, int tsx, int tsy)
{
    int i,j;
    unsigned char *bdata = MALLOC(sizeof(unsigned char)*tsx*tsy*3);

    // we will estimate the stats from the thumbnail data
    clear_stats(ii);

    if (meta_is_valid_double(ii->meta->general->no_data)) {
      ii->stats.have_no_data = TRUE;
      ii->stats.no_data_value = ii->meta->general->no_data;
    } else {
      ii->stats.have_no_data = FALSE;
      ii->stats.no_data_value = -99999; // should never be checked
    }
    // Here we do the rather ugly thing of making the thumbnail
    // loading code specific to each supported data type.  This is
    // because we've combined the stats calculation into this...
    if (ii->data_ci->data_type == GREYSCALE_FLOAT) {
        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        float *fdata = CALLOC(sizeof(float), tsx*tsy);

        load_thumbnail_data(ii->data_ci, tsx, tsy, fdata);

        // split out the case where we have no ignore value --
        // should be quite a bit faster...
        if (ii->stats.have_no_data) {
            // Compute stats -- ignore "no data" value
            int n=0;
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    if (meta_is_valid_double(v) && 
                        v!=ii->stats.no_data_value &&
                        fabs(v)<999999999)
                    {
                        ii->stats.avg += v;

                        // first valid pixel --> initialize max/min
                        // subsequent pixels --> update max/min if needed
                        if (n==0) {
                            ii->stats.act_max = ii->stats.act_min = v;
                        } else {
                            if (v > ii->stats.act_max)
                              ii->stats.act_max = v;
                            if (v < ii->stats.act_min)
                              ii->stats.act_min = v;
                        }
                        ++n;
                    }
                }
            }
            ii->stats.avg /= (double)n;
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    if (meta_is_valid_double(v) &&
                        v!=ii->stats.no_data_value &&
                        fabs(v)<999999999)
                    {
                        ii->stats.stddev +=
                          (v - ii->stats.avg)*(v - ii->stats.avg);
                    }
                }
            }
            ii->stats.stddev = sqrt(ii->stats.stddev / (double)n);
        } else {
            // Compute stats, no ignore (actually, do ignore data that is NaN)
            ii->stats.act_max = ii->stats.act_min = fdata[0];
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    // added in the fabs<999... thing... sometimes values
                    // are just ridiculous and we must ignore them
                    if (meta_is_valid_double(v) && fabs(v)<999999999) {
                        ii->stats.avg += v;
                        if (v > ii->stats.act_max) ii->stats.act_max = v;
                        if (v < ii->stats.act_min) ii->stats.act_min = v;
                    }
                }
            }
            ii->stats.avg /= (double)(tsx*tsy);
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    if (meta_is_valid_double(v) && fabs(v)<999999999)
                        ii->stats.stddev +=
                          (v - ii->stats.avg) * (v - ii->stats.avg);
                }
            }
            ii->stats.stddev = sqrt(ii->stats.stddev / (double)(tsx*tsy));
        }

        //printf("Avg, StdDev: %f, %f\n", ii->stats.avg, ii->stats.stddev);

        // Set the limits of the scaling - 2-sigma on either side of the mean
        // These are globals, we will use them in the big image, too.
        ii->stats.map_min = ii->stats.avg - 2*ii->stats.stddev;
        ii->stats.map_max = ii->stats.avg + 2*ii->stats.stddev;

        // Now actually scale the data, and convert to bytes.
        // Note that we need 3 values, one for each of the RGB channels.
        int have_no_data = ii->stats.have_no_data;
        if (have_lut()) {
            // look up table case -- no scaling, just use the lut
            // to convert from float to rgb byte
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    int index = j+i*tsx;
                    int n = 3*index;
                    float val = fdata[index];

                    int ival;
                    if (!meta_is_valid_double(val) ||
                        (have_no_data && val==ii->stats.no_data_value) ||
                        val < 0)
                        ival = 0;
                    else
                        ival = (int)val;

                    apply_lut(ival, &bdata[n], &bdata[n+1], &bdata[n+2]);

                    // histogram will appear as if we were scaling
                    // to greyscale byte
                    unsigned char uval;
                    if (ival <= 0 || val < ii->stats.map_min)
                        uval = 0;
                    else if (val > ii->stats.map_max)
                        uval = 255;
                    else
                        uval = (unsigned char)(((val-ii->stats.map_min)/(ii->stats.map_max-ii->stats.map_min))*255+0.5);

                    ii->stats.hist[uval] += 1;
                }
            }

        } else {
            // normal case -- no lut, apply 2-sigma scaling to convert from
            // floating point to byte for display
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    int index = j+i*tsx;
                    float val = fdata[index];

                    unsigned char uval;
                    if (!meta_is_valid_double(val))
                        uval = 0;
                    else if (have_no_data && val == ii->stats.no_data_value)
                        uval = 0;
                    else if (val < ii->stats.map_min)
                        uval = 0;
                    else if (val > ii->stats.map_max)
                        uval = 255;
                    else
                        uval = (unsigned char)(((val-ii->stats.map_min)/(ii->stats.map_max-ii->stats.map_min))*255+0.5);
                
                    int n = 3*index;
                    bdata[n] = uval;
                    bdata[n+1] = uval;
                    bdata[n+2] = uval;

                    ii->stats.hist[uval] += 1;
                }
            }
        }

        // done with our subset
        free(fdata);
    }
    else if (ii->data_ci->data_type == RGB_BYTE) {

        load_thumbnail_data(ii->data_ci, tsx, tsy, (void*)bdata);

        // initialize to opposite extrema
        ii->stats.act_max = 0;
        ii->stats.act_min = 255;

        // don't really have a mapping for byte data...
        ii->stats.map_min = 0;
        ii->stats.map_max = 255;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);
                unsigned char uval = 
                    (bdata[kk] + bdata[kk+1] + bdata[kk+2])/3;

                ii->stats.avg += uval;
                ii->stats.hist[uval] += 1;

                if (uval > ii->stats.act_max) ii->stats.act_max = uval;
                if (uval < ii->stats.act_min) ii->stats.act_min = uval;
            }
        }

        ii->stats.avg /= (double)(tsx*tsy);

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);
                unsigned char uval = 
                    (bdata[kk] + bdata[kk+1] + bdata[kk+2])/3;

                ii->stats.stddev +=
                  (uval - ii->stats.avg) * (uval - ii->stats.avg);
            }
        }
        ii->stats.stddev = sqrt(ii->stats.stddev / (double)(tsx*tsy));

    }
    else if (ii->data_ci->data_type == GREYSCALE_BYTE) {

        // this case is very similar to the RGB case, above, except we
        // have to first grab the data into a greyscale buffer, and
        // then copy it over to the 3-band buffer we're supposed to return
        unsigned char *gsdata = MALLOC(sizeof(unsigned char)*tsx*tsy);
        load_thumbnail_data(ii->data_ci, tsx, tsy, (void*)gsdata);

        ii->stats.act_max = 0;
        ii->stats.act_min = 255;
        ii->stats.map_min = 0;
        ii->stats.map_max = 255;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                unsigned char uval = gsdata[j+i*tsx];
                int kk = 3*(j+i*tsx);

                if (have_lut()) {
                    apply_lut(uval, &bdata[kk], &bdata[kk+1], &bdata[kk+2]);
                } else {
                    bdata[kk] = bdata[kk+1] = bdata[kk+2] = uval;
                }

                ii->stats.avg += uval;
                ii->stats.hist[uval] += 1;

                if (uval > ii->stats.act_max) ii->stats.act_max = uval;
                if (uval < ii->stats.act_min) ii->stats.act_min = uval;
            }
        }

        ii->stats.avg /= (double)(tsx*tsy);

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                unsigned char uval = gsdata[j+i*tsx];
                ii->stats.stddev +=
                  (uval - ii->stats.avg) * (uval - ii->stats.avg);
            }
        }
        ii->stats.stddev = sqrt(ii->stats.stddev / (double)(tsx*tsy));

        free(gsdata);
    }
    else if (ii->data_ci->data_type == RGB_FLOAT) {
        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        float *fdata = MALLOC(sizeof(float)*tsx*tsy*3);
        load_thumbnail_data(ii->data_ci, tsx, tsy, fdata);

        ii->stats.act_max = ii->stats.act_min = fdata[0];

        int have_nd = meta_is_valid_double(ii->stats.no_data_value);
        double nd = ii->stats.no_data_value;

        // Use the average of the three RGB channels
        int n=0;
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                int is_valid_data = 
                    meta_is_valid_double(fdata[kk]) &&
                    meta_is_valid_double(fdata[kk+1]) &&
                    meta_is_valid_double(fdata[kk+2]) &&
                    (!have_nd ||
                        (fdata[kk]!=nd && fdata[kk+1]!=nd && fdata[kk+2]!=nd));
    
                if (is_valid_data)
                {
                    float v = (fdata[kk]+fdata[kk+1]+fdata[kk+2])/3;
                    ii->stats.avg += v;
                    if (v > ii->stats.act_max) ii->stats.act_max = v;
                    if (v < ii->stats.act_min) ii->stats.act_min = v;
                    ++n;
                }
            }
        }
        ii->stats.avg /= (double)n;
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                int is_valid_data = 
                    meta_is_valid_double(fdata[kk]) &&
                    meta_is_valid_double(fdata[kk+1]) &&
                    meta_is_valid_double(fdata[kk+2]) &&
                    (!have_nd ||
                        (fdata[kk]!=nd && fdata[kk+1]!=nd && fdata[kk+2]!=nd));

                if (is_valid_data)
                {
                    float v = (fdata[kk]+fdata[kk+1]+fdata[kk+2])/3;
                    ii->stats.stddev +=
                      (v - ii->stats.avg) * (v - ii->stats.avg);
                }
            }
        }
        ii->stats.stddev = sqrt(ii->stats.stddev / (double)n);
        ii->stats.map_min = ii->stats.avg - 2*ii->stats.stddev;
        ii->stats.map_max = ii->stats.avg + 2*ii->stats.stddev;

        // Scale the data
        for (i=0; i<tsx*tsy*3; ++i) {
            if (!meta_is_valid_double(fdata[i]))
                bdata[i] = 0;
            else if (have_nd && fdata[i] == nd)
                bdata[i] = 0;
            else if (fdata[i] < ii->stats.map_min)
                bdata[i] = 0;
            else if (fdata[i] > ii->stats.map_max)
                bdata[i] = 255;
            else
                bdata[i] = (unsigned char)(((fdata[i]-ii->stats.map_min)/(ii->stats.map_max-ii->stats.map_min))*255+0.5);
        }

        // Update the histogram
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);
                unsigned char uval = (bdata[kk]+bdata[kk+1]+bdata[kk+2])/3;
                ii->stats.hist[uval] += 1;
            }
        }

        // done with our subset
        free(fdata);
    }
    else {
	    asfPrintError("Unexpected data type: %d!\n",
                          ii->data_ci->data_type);
    }

    return bdata;
}

int calc_scaled_pixel_value(ImageStats *stats, float val)
{
    if (stats->have_no_data && val == stats->no_data_value)
        return 0;
    if (val < stats->map_min)
        return 0;
    else if (val > stats->map_max)
        return 255;
    else
        return (int) round(((val-stats->map_min)/(stats->map_max-stats->map_min))*255);
}

static void fill_stats_label(ImageInfo *ii)
{
    char s[1024];
    strcpy(s, "");

    // y = m*x + b
    double m = 255.0/(ii->stats.map_max-ii->stats.map_min);
    double b = -ii->stats.map_min*255.0/
      (ii->stats.map_max-ii->stats.map_min);

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
        ii->stats.avg, ii->stats.stddev,
        ii->stats.act_min, ii->stats.act_max, 
        m, c, b);

    put_string_to_label("stats_label", s);
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

static void pop_hist(ImageInfo *ii)
{
    int i,j;

    int bin_max = 0;
    for (i=0; i<256; ++i) {
        if (ii->stats.hist[i] > bin_max) bin_max = ii->stats.hist[i];
    }

    const int w = 200;
    unsigned char *histogram_data = MALLOC(sizeof(unsigned char)*256*w*4);
    for (i=0; i<256; ++i) {
        int l = (int)((double)ii->stats.hist[i] / 
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

int fill_stats(ImageInfo *ii)
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

    fill_stats_label(ii);
    pop_hist(ii);

    return TRUE;
}
