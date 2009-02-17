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

static int have_ignore(ImageStats *stats)
{
  return stats->have_no_data || stats->have_no_data_range;
}

int is_ignored(ImageStats *stats, float val)
{
  if (!meta_is_valid_double(val)) // always ignore NaN
    return TRUE;
  else if (stats->have_no_data && val == stats->no_data_value)
    return TRUE;
  else if (stats->have_no_data_range &&
           val >= stats->no_data_min && val <= stats->no_data_max)
    return TRUE;
  else
    return FALSE;
}

static void set_ignores(ImageInfo *ii, int from_gui)
{
  ImageStats *stats = &(ii->stats);
  if (from_gui) {
    stats->have_no_data = get_checked("rb_gs_ignore_value_checkbutton");
    if (stats->have_no_data)
      stats->no_data_value = get_double_from_entry("gs_ignore_value_entry");
    else
      stats->no_data_value = 0;
    
    stats->have_no_data_range = get_checked("rb_gs_ignore_range_checkbutton");
    if (stats->have_no_data_range) {
      stats->no_data_min = get_double_from_entry("gs_ignore_range_min_entry");
      stats->no_data_max = get_double_from_entry("gs_ignore_range_max_entry");
    }
    else {
      stats->no_data_min = 0;
      stats->no_data_max = 0;
    }
  }
  else {
    if (meta_is_valid_double(ii->meta->general->no_data)) {
      stats->have_no_data = TRUE;
      stats->no_data_value = ii->meta->general->no_data;
    } else {
      stats->have_no_data = FALSE;
      stats->no_data_value = -99999; // should never be checked
    }

    stats->have_no_data_range = FALSE;
  }
}

static void set_mapping(ImageInfo *ii, int from_gui)
{
  ImageStats *stats = &(ii->stats);
  if (from_gui) {
    if (get_checked("rb_gs_2sigma")) {
      stats->map_min = stats->avg - 2*stats->stddev;
      stats->map_max = stats->avg + 2*stats->stddev;
      stats->truncate = FALSE;
    }
    else if (get_checked("rb_gs_3sigma")) {
      stats->map_min = stats->avg - 3*stats->stddev;
      stats->map_max = stats->avg + 3*stats->stddev;
      stats->truncate = FALSE;
    }
    else if (get_checked("rb_gs_minmax")) {
      stats->map_min = stats->act_min;
      stats->map_max = stats->act_max;
      stats->truncate = FALSE;
    }
    else if (get_checked("rb_gs_truncate")) {
      stats->truncate = TRUE;
    }
    else if (get_checked("rb_gs_custom")) {
      stats->map_min = get_double_from_entry("gs_custom_min_entry");
      stats->map_max = get_double_from_entry("gs_custom_max_entry");
      stats->truncate = FALSE;
    }
  }
  else {
    // initialize to defaults
    stats->map_min = stats->avg - 2*stats->stddev;
    stats->map_max = stats->avg + 2*stats->stddev;
    stats->truncate = FALSE;
  }
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
    ImageStats *stats = &(ii->stats);

    // we will estimate the stats from the thumbnail data
    clear_stats(ii);

    // Here we do the rather ugly thing of making the thumbnail
    // loading code specific to each supported data type.  This is
    // because we've combined the stats calculation into this...
    if (ii->data_ci->data_type == GREYSCALE_FLOAT) {
        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        float *fdata = CALLOC(sizeof(float), tsx*tsy);

        load_thumbnail_data(ii->data_ci, tsx, tsy, fdata);
        set_ignores(ii, glade_xml!=NULL);

        // split out the case where we have no ignore value --
        // should be quite a bit faster...
        if (have_ignore(stats)) {
            // Compute stats -- ignore "no data" value
            int n=0;
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    if (!is_ignored(stats, v) && fabs(v)<999999999)
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
                    if (!is_ignored(stats, v) && fabs(v)<999999999)
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

        set_mapping(ii, glade_xml!=NULL);

        // Now actually scale the data, and convert to bytes.
        // Note that we need 3 values, one for each of the RGB channels.
        if (have_lut()) {
            // look up table case -- no scaling, just use the lut
            // to convert from float to rgb byte
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    int index = j+i*tsx;
                    int n = 3*index;
                    float val = fdata[index];

                    int ival;
                    if (is_ignored(stats, val) || ival<0)
                        ival = 0;
                    else
                        ival = (int)val;

                    apply_lut(ival, &bdata[n], &bdata[n+1], &bdata[n+2]);

                    // histogram will appear as if we were scaling
                    // to greyscale byte
                    unsigned char uval = (unsigned char)
                      calc_scaled_pixel_value(&(ii->stats), val);
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

                    unsigned char uval = (unsigned char)
                      calc_scaled_pixel_value(&(ii->stats), val);
                
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

        ii->stats.act_min = 0;
        ii->stats.act_max = 0;

        int n=0;
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                int is_valid_data = 
                    meta_is_valid_double(fdata[kk]) &&
                    meta_is_valid_double(fdata[kk+1]) &&
                    meta_is_valid_double(fdata[kk+2]);
    
                if (is_valid_data)
                {
                    ii->stats_r.avg += fdata[kk];
                    ii->stats_g.avg += fdata[kk+1];
                    ii->stats_b.avg += fdata[kk+2];
                    ++n;
                }
            }
        }
        
        ii->stats_r.avg /= (double)n;
        ii->stats_g.avg /= (double)n;
        ii->stats_b.avg /= (double)n;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                int is_valid_data = 
                    meta_is_valid_double(fdata[kk]) &&
                    meta_is_valid_double(fdata[kk+1]) &&
                    meta_is_valid_double(fdata[kk+2]);

                if (is_valid_data)
                {
                    float d = fdata[kk] - ii->stats_r.avg;
                    ii->stats_r.stddev += d*d;

                    d = fdata[kk+1] - ii->stats_g.avg;
                    ii->stats_g.stddev += d*d;

                    d = fdata[kk+2] - ii->stats_b.avg;
                    ii->stats_b.stddev += d*d;
                }
            }
        }

        ii->stats_r.stddev = sqrt(ii->stats_r.stddev / (double)n);
        if (ii->stats_r.stddev > 0) {
          ii->stats_r.map_min = ii->stats_r.avg - 2*ii->stats_r.stddev;
          ii->stats_r.map_max = ii->stats_r.avg + 2*ii->stats_r.stddev;
        }
        else {
          ii->stats_r.map_min = ii->stats_r.avg - 1;
          ii->stats_r.map_max = ii->stats_r.avg + 1;
        }

        ii->stats_g.stddev = sqrt(ii->stats_g.stddev / (double)n);
        if (ii->stats_g.stddev > 0) {
          ii->stats_g.map_min = ii->stats_g.avg - 2*ii->stats_g.stddev;
          ii->stats_g.map_max = ii->stats_g.avg + 2*ii->stats_g.stddev;
        }
        else {
          ii->stats_g.map_min = ii->stats_g.avg - 1;
          ii->stats_g.map_max = ii->stats_g.avg + 1;
        }

        ii->stats_b.stddev = sqrt(ii->stats_b.stddev / (double)n);
        if (ii->stats_b.stddev > 0) {
          ii->stats_b.map_min = ii->stats_b.avg - 2*ii->stats_b.stddev;
          ii->stats_b.map_max = ii->stats_b.avg + 2*ii->stats_b.stddev;
        }
        else {
          ii->stats_b.map_min = ii->stats_b.avg - 1;
          ii->stats_b.map_max = ii->stats_b.avg + 1;
        }

        // clear out the stats for the greyscale image - we'll just use
        // the histogram
        ii->stats.avg = 0;
        ii->stats.stddev = 0;
        ii->stats.map_min = 0;
        ii->stats.map_max = 0;

        // Scale the data: Red
        float red_range = ii->stats_r.map_max - ii->stats_r.map_min;
        float green_range = ii->stats_g.map_max - ii->stats_g.map_min;
        float blue_range = ii->stats_b.map_max - ii->stats_b.map_min;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!meta_is_valid_double(fdata[kk]))
                    bdata[kk] = 0;
                else if (fdata[kk] < ii->stats_r.map_min)
                    bdata[kk] = 0;
                else if (fdata[kk] > ii->stats_r.map_max)
                    bdata[kk] = 255;
                else {
                   float rel = fdata[kk]-ii->stats_r.map_min;
                   bdata[kk] = (unsigned char)(rel/red_range*255.+0.5);
                }

                // kk = 3*(j+i*tsx)+1;
                ++kk;

                if (!meta_is_valid_double(fdata[kk]))
                    bdata[kk] = 0;
                else if (fdata[kk] < ii->stats_g.map_min)
                    bdata[kk] = 0;
                else if (fdata[kk] > ii->stats_g.map_max)
                    bdata[kk] = 255;
                else {
                    float rel = fdata[kk]-ii->stats_g.map_min;
                    bdata[kk] = (unsigned char)(rel/green_range*255.+0.5);
                }

                // kk = 3*(j+i*tsx)+2;
                ++kk;

                if (!meta_is_valid_double(fdata[kk]))
                    bdata[kk] = 0;
                else if (fdata[kk] < ii->stats_b.map_min)
                    bdata[kk] = 0;
                else if (fdata[kk] > ii->stats_b.map_max)
                    bdata[kk] = 255;
                else {
                    float rel = fdata[kk]-ii->stats_b.map_min;
                    bdata[kk] = (unsigned char)(rel/blue_range*255.+0.5);
                }
            }
        }

        // Update the histogram -- use greyscale average
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
    if (is_ignored(stats, val))
        return 0;
    else if (stats->truncate) {
      if (val < 0)
        return 0;
      else if (val > 255)
        return 255;
      else
        return (int)(val+.5);
    }
    else if (val < stats->map_min)
        return 0;
    else if (val > stats->map_max)
        return 255;
    else
        return (int) round(((val-stats->map_min)/(stats->map_max-stats->map_min))*255);
}

int calc_rgb_scaled_pixel_value(ImageStatsRGB *stats, float val)
{
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

    // Not sure we should put the Max/Min in here... after all, these
    // are only from a subset.  The aggregate values (avg, stddev, mapping)
    // will be fine, but max/min could be quite far off, if there are
    // an outlier or two.
    if (ii->data_ci->data_type == RGB_FLOAT) {
      double rb = -ii->stats_r.map_min*255.0/
        (ii->stats_r.map_max-ii->stats_r.map_min);
      double gb = -ii->stats_g.map_min*255.0/
        (ii->stats_g.map_max-ii->stats_g.map_min);
      double bb = -ii->stats_b.map_min*255.0/
        (ii->stats_b.map_max-ii->stats_b.map_min);

      sprintf(&s[strlen(s)],
              "Average: %.3f, %.3f, %.3f\n"
              "Standard Deviation: %.3f, %.3f, %.3f\n"
              "Mapping Fn for pixels:\n"
              "  Y = %.3f * X %c %.3f (red)\n"
              "  Y = %.3f * X %c %.3f (green)\n"
              "  Y = %.3f * X %c %.3f (blue)\n",
              ii->stats_r.avg, 
              ii->stats_g.avg, 
              ii->stats_b.avg, 
              ii->stats_r.stddev,
              ii->stats_g.stddev,
              ii->stats_b.stddev,
              255.0/(ii->stats_r.map_max-ii->stats_r.map_min),
              rb > 0 ? '+' : '-',
              fabs(rb),
              255.0/(ii->stats_g.map_max-ii->stats_g.map_min),
              gb > 0 ? '+' : '-',
              fabs(gb),
              255.0/(ii->stats_b.map_max-ii->stats_b.map_min),
              bb > 0 ? '+' : '-',
              fabs(bb));
    }
    else {
      // y = m*x + b
      double m, b;

      if (ii->stats.truncate) {
        m = 1.0;
        b = 0.0;
      }
      else {
        m = 255.0/(ii->stats.map_max-ii->stats.map_min);
        b = -ii->stats.map_min*255.0/(ii->stats.map_max-ii->stats.map_min);
      }

      // we will take charge of displaying the sign
      char c = b>0 ? '+' : '-';
      b = fabs(b);

      sprintf(&s[strlen(s)],
              "Average: %.3f\n"
              "Standard Deviation: %.3f\n"
              "Mapping Fn for pixels:\n"
              "  Y = %.3f * X %c %.3f",
              ii->stats.avg, ii->stats.stddev,
              m, c, b);
    }

    put_string_to_label("stats_label", s);
}

static void destroy_pb_data(guchar *pixels, gpointer data)
{
    free(pixels);
}

static void put_double_to_label(const char *widget_name, const char *format,
                                double value)
{
  char buf[128];
  snprintf(buf, sizeof(buf), format, value);
  put_string_to_label(widget_name, buf);
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
        for (j=l*4; j<w*4; j += 4) {
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

    // populate the low/mid/hi labels on the histogram
    double max, min;
    int n;
    if (ii->stats.truncate) {
      max = 255;
      min = 0;
      n = 0;
    }
    else {
      max = ii->stats.map_max;
      min = ii->stats.map_min;
      n=2;
      if (max>1000 || min<-1000) n=1;
      if (max>10000 || min<-10000) n=0;
      if (max<100 && min>-100) n=3;
      if (max<10 && min>-10) n=4;
      if (max<1 && min>-1) n=5;
    }
    char fmt[32];
    sprintf(fmt, "%%%d.%df", 6, n);
    put_double_to_label("hist_hi_label", fmt, max);
    put_double_to_label("hist_mid_label", fmt, 0.5*(max+min));
    put_double_to_label("hist_lo_label", fmt, min);
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

void update_map_settings()
{
  int on;

  on = get_checked("rb_gs_custom");
  enable_widget("hbox_gs_custom_range", on);
  on = get_checked("rb_gs_ignore_range_checkbutton");
  enable_widget("hbox_gs_ignore_range", on);
  on = get_checked("rb_gs_ignore_value_checkbutton");
  enable_widget("hbox_gs_ignore_value", on);
}

SIGNAL_CALLBACK void on_rb_gs_toggled(GtkWidget *w)
{
  update_map_settings();
}

SIGNAL_CALLBACK void on_rb_gs_ignore_range_checkbutton_toggled(GtkWidget *w)
{
  update_map_settings();
}

SIGNAL_CALLBACK void on_rb_gs_ignore_value_checkbutton_toggled(GtkWidget *w)
{
  update_map_settings();
}

SIGNAL_CALLBACK void on_map_apply_button_clicked(GtkWidget *w)
{
  //set_mapping();

  fill_small_force_reload(curr);
  fill_big(curr);
  fill_stats(curr);
  update_pixel_info(curr);
}
