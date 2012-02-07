#include "asf_view.h"

// global variable for the image stats
// ImageStats g_stats;

// note that these ignore values aren't fully hooked up yet -- decided
// that to put the checks for this in the make_big_image() code would
// be too expensive
int ignore_grey_value = 0;
int ignore_red_value = 0;
int ignore_green_value = 0;
int ignore_blue_value = 0;

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

int is_ignored_rgb(ImageStatsRGB *stats, float val)
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

static void set_ignores_rgb(ImageStatsRGB *stats, const char *clr)
{
  char buf[128];
  sprintf(buf, "%s_ignore_value_checkbutton", clr);
  stats->have_no_data = get_checked(buf);

  if (stats->have_no_data) {
    sprintf(buf, "%s_ignore_value_entry", clr);
    stats->no_data_value = get_double_from_entry(buf);
  }
  else {
    stats->no_data_value = 0;
  }

  sprintf(buf, "%s_ignore_range_checkbutton", clr);
  stats->have_no_data_range = get_checked(buf);
  if (stats->have_no_data_range) {
    sprintf(buf, "%s_ignore_range_min_entry", clr);
    stats->no_data_min = get_double_from_entry(buf);
    sprintf(buf, "%s_ignore_range_max_entry", clr);
    stats->no_data_max = get_double_from_entry(buf);
  }
  else {
    stats->no_data_min = 0;
    stats->no_data_max = 0;
  }
}

static void set_ignores(ImageInfo *ii, int from_gui)
{
  if (ii->data_ci->data_type == GREYSCALE_FLOAT ||
      ii->data_ci->data_type == GREYSCALE_BYTE)
  {  
    ImageStats *stats = &ii->stats;
    if (from_gui) {
      stats->have_no_data = get_checked("gs_ignore_value_checkbutton");
      if (stats->have_no_data)
        stats->no_data_value = get_double_from_entry("gs_ignore_value_entry");
      else
        stats->no_data_value = 0;
      
      stats->have_no_data_range = get_checked("gs_ignore_range_checkbutton");
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
  else {
    if (from_gui) {
      set_ignores_rgb(&ii->stats_r, "red");
      set_ignores_rgb(&ii->stats_g, "green");
      set_ignores_rgb(&ii->stats_b, "blue");
    }
    else {
      ii->stats_r.have_no_data = FALSE;
      ii->stats_r.have_no_data_range = FALSE;
      ii->stats_g.have_no_data = FALSE;
      ii->stats_g.have_no_data_range = FALSE;
      ii->stats_b.have_no_data = FALSE;
      ii->stats_b.have_no_data_range = FALSE;
    }

  }
}

void set_mapping_defaults(ImageInfo *ii)
{
  // only do this for greyscale...
  if (ii->data_ci->data_type == GREYSCALE_FLOAT ||
      ii->data_ci->data_type == GREYSCALE_BYTE)
  {
    ImageStats *stats = &ii->stats;
    set_checked("gs_ignore_value_checkbutton", stats->have_no_data);
    if (stats->have_no_data)
      put_double_to_entry("gs_ignore_value_entry", stats->no_data_value);
    set_checked("gs_ignore_range_checkbutton", stats->have_no_data_range);
    if (stats->have_no_data_range) {
      put_double_to_entry("gs_ignore_range_min_entry", stats->no_data_min);
      put_double_to_entry("gs_ignore_range_max_entry", stats->no_data_max);
    }
  }

  set_checked("rb_red_truncate", TRUE);
  set_checked("rb_green_truncate", TRUE);
  set_checked("rb_blue_truncate", TRUE);

  update_map_settings(ii);
}

static void set_mapping_rgb(ImageStatsRGB *stats, const char *color)
{
  char buf[64];
  sprintf(buf, "rb_%s_truncate", color);
  if (get_checked(buf)) {
    stats->truncate = TRUE;
    return;
  }

  stats->truncate = FALSE;

  sprintf(buf, "rb_%s_minmax", color);
  if (get_checked(buf)) {
    stats->map_min = stats->act_min;
    stats->map_max = stats->act_max;
    return;
  }

  sprintf(buf, "rb_%s_custom", color);
  if (get_checked(buf)) {
    sprintf(buf, "%s_custom_min_entry", color);
    stats->map_min = get_double_from_entry(buf);
    sprintf(buf, "%s_custom_max_entry", color);
    stats->map_max = get_double_from_entry(buf);
    return;
  }

  // remaining mappings use the standard deviation... guard against
  // that being zero (all data identical)
  if (stats->stddev <= 0) {
    stats->map_min = stats->avg - 1;
    stats->map_max = stats->avg + 1;
    return;
  }

  sprintf(buf, "rb_%s_2sigma", color);
  if (get_checked(buf)) {
    stats->map_min = stats->avg - 2*stats->stddev;
    stats->map_max = stats->avg + 2*stats->stddev;
    return;
  }

  sprintf(buf, "rb_%s_3sigma", color);
  if (get_checked(buf)) {
    stats->map_min = stats->avg - 3*stats->stddev;
    stats->map_max = stats->avg + 3*stats->stddev;
    return;
  }
}

static void set_mapping_defaults_rgb(ImageStatsRGB *stats)
{
  stats->truncate = TRUE;
}

static void set_mapping(ImageInfo *ii, int from_gui)
{
  if (ii->data_ci->data_type == GREYSCALE_FLOAT ||
      ii->data_ci->data_type == GREYSCALE_BYTE)
  {  
    ImageStats *stats = &(ii->stats);
    if (from_gui) {
      if (get_checked("rb_gs_minmax")) {
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
      else if (stats->stddev <= 0) {
        stats->map_min = stats->avg - 1;
        stats->map_max = stats->avg + 1;
        stats->truncate = FALSE;
      }
      else if (get_checked("rb_gs_2sigma")) {
        stats->map_min = stats->avg - 2*stats->stddev;
        stats->map_max = stats->avg + 2*stats->stddev;
        stats->truncate = FALSE;
      }
      else if (get_checked("rb_gs_3sigma")) {
        stats->map_min = stats->avg - 3*stats->stddev;
        stats->map_max = stats->avg + 3*stats->stddev;
        stats->truncate = FALSE;
      }
    }
    else {
      // initialize to defaults
      stats->truncate = FALSE;
      if (stats->stddev <= 0) {
        stats->map_min = stats->avg - 1;
        stats->map_max = stats->avg + 1;
      }
      else {
        stats->map_min = stats->avg - 2*stats->stddev;
        stats->map_max = stats->avg + 2*stats->stddev;
      }
    }
  }
  else {
    if (from_gui) {
      set_mapping_rgb(&ii->stats_r, "red");
      set_mapping_rgb(&ii->stats_g, "green");
      set_mapping_rgb(&ii->stats_b, "blue");
    }
    else {
      set_mapping_defaults_rgb(&ii->stats_r);
      set_mapping_defaults_rgb(&ii->stats_g);
      set_mapping_defaults_rgb(&ii->stats_b);
    }
  }
}

static double calc_rgb_min(ImageStatsRGB *stats)
{
  return stats->truncate ? 0 : stats->map_min;
}

static double calc_rgb_max(ImageStatsRGB *stats)
{
  return stats->truncate ? 255 : stats->map_max;
}

static double calc_fake_min(ImageInfo *ii)
{
  if (ii->stats_r.truncate && ii->stats_b.truncate && ii->stats_g.truncate) {
    return 0.;
  }
  else {
    // average three channel mins
    double r_min = calc_rgb_min(&ii->stats_r);
    double g_min = calc_rgb_min(&ii->stats_g);
    double b_min = calc_rgb_min(&ii->stats_b);

    return (r_min + g_min + b_min)/3.;
  }
}

static double calc_fake_max(ImageInfo *ii)
{
  if (ii->stats_r.truncate && ii->stats_b.truncate && ii->stats_g.truncate) {
    return 255.;
  }
  else {
    // average three channel maxs
    double r_max = calc_rgb_max(&ii->stats_r);
    double g_max = calc_rgb_max(&ii->stats_g);
    double b_max = calc_rgb_max(&ii->stats_b);

    return (r_max + g_max + b_max)/3.;
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
                        stats->avg += v;

                        // first valid pixel --> initialize max/min
                        // subsequent pixels --> update max/min if needed
                        if (n==0) {
                            stats->act_max = stats->act_min = v;
                        } else {
                            if (v > stats->act_max)
                              stats->act_max = v;
                            if (v < stats->act_min)
                              stats->act_min = v;
                        }
                        ++n;
                    }
                }
            }
            stats->avg /= (double)n;
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    if (!is_ignored(stats, v) && fabs(v)<999999999)
                    {
                        stats->stddev +=
                          (v - stats->avg)*(v - stats->avg);
                    }
                }
            }
            stats->stddev = sqrt(stats->stddev / (double)n);
        } else {
            // Compute stats, no ignore (actually, do ignore data that is NaN)
            stats->act_max = stats->act_min = fdata[0];
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    // added in the fabs<999... thing... sometimes values
                    // are just ridiculous and we must ignore them
                    if (meta_is_valid_double(v) && fabs(v)<999999999) {
                        stats->avg += v;
                        if (v > stats->act_max) stats->act_max = v;
                        if (v < stats->act_min) stats->act_min = v;
                    }
                }
            }
            stats->avg /= (double)(tsx*tsy);
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    float v = fdata[j+i*tsx];
                    if (meta_is_valid_double(v) && fabs(v)<999999999)
                        stats->stddev +=
                          (v - stats->avg) * (v - stats->avg);
                }
            }
            stats->stddev = sqrt(stats->stddev / (double)(tsx*tsy));
        }

        //printf("Avg, StdDev: %f, %f\n", stats->avg, stats->stddev);

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
                    if (is_ignored(stats, val) || val<0)
                        ival = ignore_grey_value;
                    else
                        ival = (int)val;

                    apply_lut(ival, &bdata[n], &bdata[n+1], &bdata[n+2]);

                    // histogram will appear as if we were scaling
                    // to greyscale byte
                    unsigned char uval = (unsigned char)
                      calc_scaled_pixel_value(stats, val);
                    stats->hist[uval] += 1;
                }
            }

        } else {
            // normal case -- no lut, apply selected scaling to convert from
            // floating point to byte for display
            for (i=0; i<tsy; ++i) {
                for (j=0; j<tsx; ++j) {
                    int index = j+i*tsx;
                    float val = fdata[index];
                    int n = 3*index;

                    unsigned char uval = (unsigned char)
                      calc_scaled_pixel_value(stats, val);

                    if (is_ignored(stats, val)) {
                      bdata[n] = bdata[n+1] = bdata[n+2] = ignore_grey_value;
                    }
                    else {
                      bdata[n] = uval;
                      bdata[n+1] = uval;
                      bdata[n+2] = uval;

                      stats->hist[uval] += 1;
                    }
                }
            }
        }

        // done with our subset
        free(fdata);
    }
    else if (ii->data_ci->data_type == RGB_BYTE) {

        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        unsigned char *rgbdata = CALLOC(sizeof(unsigned char), tsx*tsy*3);
        load_thumbnail_data(ii->data_ci, tsx, tsy, (void*)rgbdata);
        set_ignores(ii, glade_xml!=NULL);

        ImageStatsRGB *stats_r = &ii->stats_r;
        ImageStatsRGB *stats_g = &ii->stats_g;
        ImageStatsRGB *stats_b = &ii->stats_b;

        stats_r->act_min = rgbdata[0];
        stats_r->act_max = rgbdata[0];

        stats_g->act_min = rgbdata[1];
        stats_g->act_max = rgbdata[1];

        stats_b->act_min = rgbdata[2];
        stats_b->act_max = rgbdata[2];

        int nr=0, ng=0, nb=0;
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!is_ignored_rgb(stats_r, rgbdata[kk])) {
                    stats_r->avg += rgbdata[kk];
                    ++nr;

                    if (rgbdata[kk] < stats_r->act_min)
                      stats_r->act_min = rgbdata[kk];
                    if (rgbdata[kk] > stats_r->act_max)
                      stats_r->act_max = rgbdata[kk];
                }

                if (!is_ignored_rgb(stats_g, rgbdata[kk+1])) {
                    stats_g->avg += rgbdata[kk+1];
                    ++ng;

                    if (rgbdata[kk+1] < stats_g->act_min)
                      stats_g->act_min = rgbdata[kk+1];
                    if (rgbdata[kk+1] > stats_g->act_max)
                      stats_g->act_max = rgbdata[kk+1];
                }

                if (!is_ignored_rgb(stats_b, rgbdata[kk+2])) {
                    stats_b->avg += rgbdata[kk+2];
                    ++nb;

                    if (rgbdata[kk+2] < stats_b->act_min)
                      stats_b->act_min = rgbdata[kk+2];
                    if (rgbdata[kk+2] > stats_b->act_max)
                      stats_b->act_max = rgbdata[kk+2];
                }
            }
        }
        
        if (nr>1) stats_r->avg /= (double)nr;
        if (ng>1) stats_g->avg /= (double)ng;
        if (nb>1) stats_b->avg /= (double)nb;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!is_ignored_rgb(stats_r, rgbdata[kk])) {
                    float d = rgbdata[kk] - stats_r->avg;
                    stats_r->stddev += d*d;
                }

                if (!is_ignored_rgb(stats_g, rgbdata[kk+1])) {
                    float d = rgbdata[kk+1] - stats_g->avg;
                    stats_g->stddev += d*d;
                }

                if (!is_ignored_rgb(stats_b, rgbdata[kk+2])) {
                    float d = rgbdata[kk+2] - stats_b->avg;
                    stats_b->stddev += d*d;
                }
            }
        }

        stats_r->stddev = sqrt(stats_r->stddev / (double)nr);
        stats_g->stddev = sqrt(stats_g->stddev / (double)ng);
        stats_b->stddev = sqrt(stats_b->stddev / (double)nb);

        set_mapping(ii, glade_xml!=NULL);

        // clear out the stats for the greyscale image - we'll just use
        // the histogram
        stats->avg = 0;
        stats->stddev = 0;

	// these are used for the axis labels on the histogram, so we put
	// in the averages of the mins... these are sort of bogus anyway, we
	// really should have 3 histograms
        stats->map_min = calc_fake_min(ii);
	stats->map_max = calc_fake_max(ii);

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!is_ignored_rgb(stats_r, rgbdata[kk]) &&
                    !is_ignored_rgb(stats_g, rgbdata[kk+1]) &&
                    !is_ignored_rgb(stats_b, rgbdata[kk+2]))
                {
                  bdata[kk] = (unsigned char)calc_rgb_scaled_pixel_value(
                    stats_r, rgbdata[kk]);
                  bdata[kk+1] = (unsigned char)calc_rgb_scaled_pixel_value(
                    stats_g, rgbdata[kk+1]);
                  bdata[kk+2] = (unsigned char)calc_rgb_scaled_pixel_value(
                    stats_b, rgbdata[kk+2]);
                }
                else {
                  bdata[kk] = ignore_red_value;
                  bdata[kk+1] = ignore_green_value;
                  bdata[kk+2] = ignore_blue_value;
                }
            }
        }

        // Update the histogram -- use greyscale average
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);
                if (!is_ignored_rgb(stats_r, rgbdata[kk]) &&
                    !is_ignored_rgb(stats_g, rgbdata[kk+1]) &&
                    !is_ignored_rgb(stats_b, rgbdata[kk+2]))
                {   
                  unsigned char uval = (bdata[kk]+bdata[kk+1]+bdata[kk+2])/3;
                  stats->hist[uval] += 1;
                }
            }
        }

        free(rgbdata);
    }
    else if (ii->data_ci->data_type == GREYSCALE_BYTE) {

        // this case is very similar to the RGB case, above, except we
        // have to first grab the data into a greyscale buffer, and
        // then copy it over to the 3-band buffer we're supposed to return
        unsigned char *gsdata = MALLOC(sizeof(unsigned char)*tsx*tsy);
        load_thumbnail_data(ii->data_ci, tsx, tsy, (void*)gsdata);
        set_ignores(ii, glade_xml!=NULL);

        stats->act_max = 0;
        stats->act_min = 255;
        stats->map_min = 0;
        stats->map_max = 255;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                unsigned char uval = gsdata[j+i*tsx];

                stats->avg += uval;
                if (uval > stats->act_max) stats->act_max = uval;
                if (uval < stats->act_min) stats->act_min = uval;
            }
        }

        stats->avg /= (double)(tsx*tsy);

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                unsigned char uval = gsdata[j+i*tsx];
                stats->stddev += (uval - stats->avg) * (uval - stats->avg);
            }
        }
        stats->stddev = sqrt(stats->stddev / (double)(tsx*tsy));

        set_mapping(ii, glade_xml!=NULL);

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                unsigned char uval = gsdata[j+i*tsx];
                int kk = 3*(j+i*tsx);

                if (have_lut()) {
                    apply_lut(uval, &bdata[kk], &bdata[kk+1], &bdata[kk+2]);
                    stats->hist[uval] += 1;
                }
                else if (!is_ignored(stats, uval)) {
                    // apply selected scaling to display values
                    unsigned char display_value = (unsigned char)
                        calc_scaled_pixel_value(stats, uval);
                    bdata[kk] = bdata[kk+1] = bdata[kk+2] = display_value;
                    stats->hist[display_value] += 1;
                }
                else {
                    bdata[kk] = bdata[kk+1] = bdata[kk+2] = ignore_grey_value;
                }
            }
        }

        free(gsdata);
    }
    else if (ii->data_ci->data_type == RGB_FLOAT) {
        // store data used to build the small image pixmap
        // we will calculate the stats on this subset
        float *fdata = MALLOC(sizeof(float)*tsx*tsy*3);
        load_thumbnail_data(ii->data_ci, tsx, tsy, fdata);
        set_ignores(ii, glade_xml!=NULL);

        ImageStatsRGB *stats_r = &ii->stats_r;
        ImageStatsRGB *stats_g = &ii->stats_g;
        ImageStatsRGB *stats_b = &ii->stats_b;

        stats_r->act_min = fdata[0];
        stats_r->act_max = fdata[0];

        stats_g->act_min = fdata[1];
        stats_g->act_max = fdata[1];

        stats_b->act_min = fdata[2];
        stats_b->act_max = fdata[2];

        int nr=0, ng=0, nb=0;
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!is_ignored_rgb(stats_r, fdata[kk])) {
                    stats_r->avg += fdata[kk];
                    ++nr;

                    if (fdata[kk] < stats_r->act_min)
                      stats_r->act_min = fdata[kk];
                    if (fdata[kk] > stats_r->act_max)
                      stats_r->act_max = fdata[kk];
                }

                if (!is_ignored_rgb(stats_g, fdata[kk+1])) {
                    stats_g->avg += fdata[kk+1];
                    ++ng;

                    if (fdata[kk+1] < stats_g->act_min)
                      stats_g->act_min = fdata[kk+1];
                    if (fdata[kk+1] > stats_g->act_max)
                      stats_g->act_max = fdata[kk+1];
                }

                if (!is_ignored_rgb(stats_b, fdata[kk+2])) {
                    stats_b->avg += fdata[kk+2];
                    ++nb;

                    if (fdata[kk+2] < stats_b->act_min)
                      stats_b->act_min = fdata[kk+2];
                    if (fdata[kk+2] > stats_b->act_max)
                      stats_b->act_max = fdata[kk+2];
                }
            }
        }
        
        if (nr>1) stats_r->avg /= (double)nr;
        if (ng>1) stats_g->avg /= (double)ng;
        if (nb>1) stats_b->avg /= (double)nb;

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!is_ignored_rgb(stats_r, fdata[kk])) {
                    float d = fdata[kk] - stats_r->avg;
                    stats_r->stddev += d*d;
                }

                if (!is_ignored_rgb(stats_g, fdata[kk+1])) {
                    float d = fdata[kk+1] - stats_g->avg;
                    stats_g->stddev += d*d;
                }

                if (!is_ignored_rgb(stats_b, fdata[kk+2])) {
                    float d = fdata[kk+2] - stats_b->avg;
                    stats_b->stddev += d*d;
                }
            }
        }

        stats_r->stddev = sqrt(stats_r->stddev / (double)nr);
        stats_g->stddev = sqrt(stats_g->stddev / (double)ng);
        stats_b->stddev = sqrt(stats_b->stddev / (double)nb);

        set_mapping(ii, glade_xml!=NULL);

        // clear out the stats for the greyscale image - we'll just use
        // the histogram
        stats->avg = 0;
        stats->stddev = 0;

	// these are used for the axis labels on the histogram, so we put
	// in the averages of the mins... these are sort of bogus anyway, we
	// really should have 3 histograms
        stats->map_min = calc_fake_min(ii);
	stats->map_max = calc_fake_max(ii);

        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);

                if (!is_ignored_rgb(stats_r, fdata[kk]) &&
                    !is_ignored_rgb(stats_g, fdata[kk+1]) &&
                    !is_ignored_rgb(stats_b, fdata[kk+2]))
                {
                  bdata[kk] = (unsigned char)calc_rgb_scaled_pixel_value(
                    stats_r, fdata[kk]);
                  bdata[kk+1] = (unsigned char)calc_rgb_scaled_pixel_value(
                    stats_g, fdata[kk+1]);
                  bdata[kk+2] = (unsigned char)calc_rgb_scaled_pixel_value(
                    stats_b, fdata[kk+2]);
                }
                else {
                  bdata[kk] = ignore_red_value;
                  bdata[kk+1] = ignore_green_value;
                  bdata[kk+2] = ignore_blue_value;
                }
            }
        }

        // Update the histogram -- use greyscale average
        for (i=0; i<tsy; ++i) {
            for (j=0; j<tsx; ++j) {
                int kk = 3*(j+i*tsx);
                if (!is_ignored_rgb(stats_r, fdata[kk]) &&
                    !is_ignored_rgb(stats_g, fdata[kk+1]) &&
                    !is_ignored_rgb(stats_b, fdata[kk+2]))
                {   
                  unsigned char uval = (bdata[kk]+bdata[kk+1]+bdata[kk+2])/3;
                  stats->hist[uval] += 1;
                }
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
    if (is_ignored_rgb(stats, val))
        return 0;
    else if (stats->truncate) {
      if (val < 0)
        return 0;
      else if (val > 255)
        return 255;
      else
        return (int)(val+.5);
    }      
    if (val < stats->map_min)
        return 0;
    else if (val > stats->map_max)
        return 255;
    else
        return (int) round(((val-stats->map_min)/(stats->map_max-stats->map_min))*255);
}

int apply_mask(int v, unsigned char *r, unsigned char *g, unsigned char *b)
{
    int mask_was_applied = TRUE;
    // right now this is hard-coded for the layover/shadow lut
    // FIXME: after the lut stuff is put into the ImageInfo structure,
    // this code should consult mask->lut to apply the mask
    switch (v) {
        case 2:
            *r = *g = 0;
            *b = 153;
            break;
        case 3:
            *r = 255;
            *g = 64;
            *b = 0;
            break;
        case 4:
            *r = 0;
            *g = 153;
            *b = 32;
            break;
        case 5:
            *r = *g = *b = 64;
            break;
        default:
            // not masked, no action
            mask_was_applied = FALSE;
            break;
    }
    return mask_was_applied;
}

void
get_rgb_with_masking(ImageInfo *ii, ImageInfo *mask,
                     int l, int s,
                     unsigned char *r, unsigned char *g, unsigned char *b)
{
    int f = (int)cached_image_get_pixel(mask->data_ci, l, s);
    if (!apply_mask(f, r, g, b)) {
        // not masked, show the regular image's pixel value
        cached_image_get_rgb(ii->data_ci, l, s, r, g, b);
    }
}

static void fill_stats_label(ImageInfo *ii)
{
    char s[1024];
    strcpy(s, "");

    if (ii->data_ci->data_type == RGB_FLOAT ||
        ii->data_ci->data_type == RGB_BYTE)
    {
      double r_min = calc_rgb_min(&ii->stats_r);
      double r_max = calc_rgb_max(&ii->stats_r);
      double g_min = calc_rgb_min(&ii->stats_g);
      double g_max = calc_rgb_max(&ii->stats_g);
      double b_min = calc_rgb_min(&ii->stats_b);
      double b_max = calc_rgb_max(&ii->stats_b);

      double rb = -r_min*255.0/(r_max-r_min);
      double gb = -g_min*255.0/(g_max-g_min);
      double bb = -b_min*255.0/(b_max-b_min);

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
              255.0/(r_max-r_min),
              rb > 0 ? '+' : '-',
              fabs(rb),
              255.0/(g_max-g_min),
              gb > 0 ? '+' : '-',
              fabs(gb),
              255.0/(b_max-b_min),
              bb > 0 ? '+' : '-',
              fabs(bb));
    }
    else {
      // greyscale case
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

void disable_map_tabs(ImageInfo *ii)
{
  if (ii->data_ci->data_type == GREYSCALE_FLOAT ||
      ii->data_ci->data_type == GREYSCALE_BYTE)
  {
    enable_widget("greyscale_label", TRUE);
    enable_widget("greyscale_vbox", TRUE);

    enable_widget("red_label", FALSE);
    enable_widget("red_vbox", FALSE);
    enable_widget("green_label", FALSE);
    enable_widget("green_vbox", FALSE);
    enable_widget("blue_label", FALSE);
    enable_widget("blue_vbox", FALSE);
  }
  else {
    enable_widget("greyscale_label", FALSE);
    enable_widget("greyscale_vbox", FALSE);

    enable_widget("red_label", TRUE);
    enable_widget("red_vbox", TRUE);
    enable_widget("green_label", TRUE);
    enable_widget("green_vbox", TRUE);
    enable_widget("blue_label", TRUE);
    enable_widget("blue_vbox", TRUE);
  }
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
    disable_map_tabs(ii);

    return TRUE;
}

static void update_gs_map_settings()
{
  int on;

  on = get_checked("rb_gs_custom");
  enable_widget("hbox_gs_custom_range", on);
  on = get_checked("gs_ignore_range_checkbutton");
  enable_widget("hbox_gs_ignore_range", on);
  on = get_checked("gs_ignore_value_checkbutton");
  enable_widget("hbox_gs_ignore_value", on);
}

SIGNAL_CALLBACK void on_rb_gs_toggled(GtkWidget *w)
{
  update_gs_map_settings();
}

SIGNAL_CALLBACK void on_gs_ignore_range_checkbutton_toggled(GtkWidget *w)
{
  update_gs_map_settings();
}

SIGNAL_CALLBACK void on_gs_ignore_value_checkbutton_toggled(GtkWidget *w)
{
  update_gs_map_settings();
}

SIGNAL_CALLBACK void on_map_gs_apply_button_clicked(GtkWidget *w)
{
  fill_small_force_reload(curr);
  fill_big(curr);
  fill_stats(curr);
  update_pixel_info(curr);
}

static void do_checkbutton(const char *template, const char *src,
                        const char *dest1, const char *dest2)
{
  char buf1[128], buf2[128], buf3[128];

  sprintf(buf1, template, src);
  sprintf(buf2, template, dest1);
  sprintf(buf3, template, dest2);

  set_checked(buf2, get_checked(buf1));
  set_checked(buf3, get_checked(buf1));
}

static void do_entry(const char *template, const char *src,
                     const char *dest1, const char *dest2)
{
  char buf1[128], buf2[128], buf3[128];

  sprintf(buf1, template, src);
  sprintf(buf2, template, dest1);
  sprintf(buf3, template, dest2);

  char *str = get_string_from_entry(buf1);
  put_string_to_entry(buf2, str);
  put_string_to_entry(buf3, str);
}

static void apply_color_to_all(const char *src,
                               const char *dst1, const char *dst2)
{
  do_checkbutton("rb_%s_2sigma", src, dst1, dst2);
  do_checkbutton("rb_%s_3sigma", src, dst1, dst2);
  do_checkbutton("rb_%s_minmax", src, dst1, dst2);
  do_checkbutton("rb_%s_truncate", src, dst1, dst2);
  do_checkbutton("rb_%s_custom", src, dst1, dst2);
  do_checkbutton("%s_ignore_value_checkbutton", src, dst1, dst2);
  do_checkbutton("%s_ignore_range_checkbutton", src, dst1, dst2);
  do_entry("%s_ignore_value_entry", src, dst1, dst2);
  do_entry("%s_ignore_range_min_entry", src, dst1, dst2);
  do_entry("%s_ignore_range_max_entry", src, dst1, dst2);
  do_entry("%s_custom_min_entry", src, dst1, dst2);
  do_entry("%s_custom_max_entry", src, dst1, dst2);
}

static void update_red_map_settings()
{
  int on;

  on = get_checked("rb_red_custom");
  enable_widget("hbox_red_custom_range", on);
  on = get_checked("red_ignore_range_checkbutton");
  enable_widget("hbox_red_ignore_range", on);
  on = get_checked("red_ignore_value_checkbutton");
  enable_widget("hbox_red_ignore_value", on);
}

SIGNAL_CALLBACK void on_rb_red_toggled(GtkWidget *w)
{
  update_red_map_settings();
}

SIGNAL_CALLBACK void on_red_ignore_range_checkbutton_toggled(GtkWidget *w)
{
  update_red_map_settings();
}

SIGNAL_CALLBACK void on_red_ignore_value_checkbutton_toggled(GtkWidget *w)
{
  update_red_map_settings();
}

SIGNAL_CALLBACK void on_map_red_apply_button_clicked(GtkWidget *w)
{
  if (get_checked("apply_red_to_all_checkbutton"))
    apply_color_to_all("red", "green", "blue");

  fill_small_force_reload(curr);
  fill_big(curr);
  fill_stats(curr);
  update_pixel_info(curr);
}

static void update_green_map_settings()
{
  int on;

  on = get_checked("rb_green_custom");
  enable_widget("hbox_green_custom_range", on);
  on = get_checked("green_ignore_range_checkbutton");
  enable_widget("hbox_green_ignore_range", on);
  on = get_checked("green_ignore_value_checkbutton");
  enable_widget("hbox_green_ignore_value", on);
}

SIGNAL_CALLBACK void on_rb_green_toggled(GtkWidget *w)
{
  update_green_map_settings();
}

SIGNAL_CALLBACK void on_green_ignore_range_checkbutton_toggled(GtkWidget *w)
{
  update_green_map_settings();
}

SIGNAL_CALLBACK void on_green_ignore_value_checkbutton_toggled(GtkWidget *w)
{
  update_green_map_settings();
}

SIGNAL_CALLBACK void on_map_green_apply_button_clicked(GtkWidget *w)
{
  if (get_checked("apply_green_to_all_checkbutton"))
    apply_color_to_all("green", "red", "blue");

  fill_small_force_reload(curr);
  fill_big(curr);
  fill_stats(curr);
  update_pixel_info(curr);
}

static void update_blue_map_settings()
{
  int on;

  on = get_checked("rb_blue_custom");
  enable_widget("hbox_blue_custom_range", on);
  on = get_checked("blue_ignore_range_checkbutton");
  enable_widget("hbox_blue_ignore_range", on);
  on = get_checked("blue_ignore_value_checkbutton");
  enable_widget("hbox_blue_ignore_value", on);
}

SIGNAL_CALLBACK void on_rb_blue_toggled(GtkWidget *w)
{
  update_blue_map_settings();
}

SIGNAL_CALLBACK void on_blue_ignore_range_checkbutton_toggled(GtkWidget *w)
{
  update_blue_map_settings();
}

SIGNAL_CALLBACK void on_blue_ignore_value_checkbutton_toggled(GtkWidget *w)
{
  update_blue_map_settings();
}

SIGNAL_CALLBACK void on_map_blue_apply_button_clicked(GtkWidget *w)
{
  if (get_checked("apply_blue_to_all_checkbutton"))
    apply_color_to_all("blue", "red", "green");

  fill_small_force_reload(curr);
  fill_big(curr);
  fill_stats(curr);
  update_pixel_info(curr);
}

void update_map_settings(ImageInfo *ii)
{
  if (ii->data_ci->data_type == GREYSCALE_FLOAT ||
      ii->data_ci->data_type == GREYSCALE_BYTE)
  {
    update_gs_map_settings();
  }
  else
  {
    update_red_map_settings();
    update_green_map_settings();
    update_blue_map_settings();
  }
}
