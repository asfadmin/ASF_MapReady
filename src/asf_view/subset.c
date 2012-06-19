#include <unistd.h>
#include "asf_view.h"

#define FORMAT_ASF_INTERNAL 1
#define FORMAT_CSV 2

#define PIXEL_VALUE 0
#define INCIDENCE_ANGLES 1
#define LOOK_ANGLE 2
#define SLANT_RANGE 3
#define SCALED_PIXEL_VALUE 4
#define TIME 5
#define DOPPLER 6
#define LAT_LON_2_BAND 7


static int get_format()
{
    if (get_checked("format_asf_internal_radiobutton"))
        return FORMAT_ASF_INTERNAL;
    else if (get_checked("format_csv_radiobutton"))
        return FORMAT_CSV;
    
    // not reached
    assert(0);
    return -1;
}

// mapping between combobox entry indexes and the #defines
static int get_what_to_save()
{
    int what = get_combo_box_item("data_combobox");

    // I know that at the moment the value of "what" corresponds with
    // these defined values...
    switch (what) {
        case 0: return PIXEL_VALUE;
        case 1: return INCIDENCE_ANGLES;
        case 2: return LOOK_ANGLE;
        case 3: return SLANT_RANGE;
        case 4: return TIME;
        case 5: return DOPPLER;
        case 6: return SCALED_PIXEL_VALUE;
        case 7: return LAT_LON_2_BAND;
        //case 8: return LATITUDE;
        //case 9: return LONGITUDE;
    }

    // not reached
    assert(0);
    return -1;
}

static void update_save_subset_info()
{
    // "load subset" option is disabed (and set unchecked) when choosing
    // csv, unless we think Excel or some such will be available
    const char *csv = detect_csv_assoc();
    int have_csv_viewer = csv && strlen(csv) > 0;
    int enabled = get_format()!=FORMAT_CSV || have_csv_viewer;
    if (!enabled)
        set_checked("load_saved_subset_checkbutton", FALSE);
    enable_widget("load_saved_subset_checkbutton", enabled);

    // "strict boundary" option is disabled (and set unchecked) when
    // polygon only has 2 points in it (line)
    enabled = g_poly->n > 1;
    if (!enabled)
        set_checked("strict_boundary_checkbutton", FALSE);
    enable_widget("strict_boundary_checkbutton", enabled);
}

static void close_subset_window(ImageInfo *ii)
{
    g_poly->show_extent = FALSE;
    fill_big(ii);

    show_widget("save_subset_window", FALSE);
}

SIGNAL_CALLBACK void 
on_format_asf_internal_radiobutton_toggled(GtkWidget *w)
{
    update_save_subset_info();
}

SIGNAL_CALLBACK void on_format_csv_radiobutton_toggled(GtkWidget *w)
{
    update_save_subset_info();
}

SIGNAL_CALLBACK void on_save_subset_window_delete_event(GtkWidget *w)
{
    close_subset_window(curr);
}

SIGNAL_CALLBACK void on_cancel_button_clicked(GtkWidget *w)
{
    close_subset_window(curr);
}

static void show_save_subset_window()
{
    GtkWidget *w = get_widget_checked("save_subset_window");
    gtk_widget_show(w);
    gtk_window_present(GTK_WINDOW(w));
}

static void set_defaults(ImageInfo *ii)
{
    // default filename
    char *basename = get_basename(ii->filename);
    char *def = appendStr(basename, "_aoi");
    if (strncmp_case(def, "LED-", 4) == 0 ||
        strncmp_case(def, "IMG-", 4) == 0 ||
        strncmp_case(def, "TRL-", 4) == 0 ||
        strncmp_case(def, "VOL-", 4) == 0)
    {
        char *tmp = STRDUP(def+4);
        free(def);
        def = tmp;
    }
    free(basename);
    put_string_to_entry("filename_entry", def);
    free(def);

    // default directory
    char *dir = get_dirname(ii->filename);
    if (dir && strlen(dir) > 0)
        put_string_to_entry("dir_entry", dir);
    else
        put_string_to_entry("dir_entry", "");
    FREE(dir);

    // default data to save (Pixel Values)
    set_combo_box_item("data_combobox", 0);
}

static void compute_extent(meta_parameters *meta,
                           int *line_min, int *line_max,
                           int *samp_min, int *samp_max,
                           int *nl, int *ns)
{
    // find the extent of the selected points in line/sample space
    *line_min = crosshair_line;
    *line_max = crosshair_line;

    *samp_min = crosshair_samp;
    *samp_max = crosshair_samp;

    int i;
    for (i=0; i<g_poly->n; ++i) {
        int l = g_poly->line[i];
        int s = g_poly->samp[i];

        if (l < *line_min) *line_min = l;
        if (l > *line_max) *line_max = l;

        if (s < *samp_min) *samp_min = s;
        if (s > *samp_max) *samp_max = s;
    }

    // clip to image extents if necessary
    int onl = meta->general->line_count;
    int ons = meta->general->sample_count;

    if (*line_min < 0) *line_min = 0;
    if (*line_min >= onl) *line_min = onl-1;

    if (*line_max < 0) *line_max = 0;
    if (*line_max >= onl) *line_max = onl-1;

    if (*samp_min < 0) *samp_min = 0;
    if (*samp_min >= ons) *samp_min = ons-1;

    if (*samp_max < 0) *samp_max = 0;
    if (*samp_max >= ons) *samp_max = ons-1;

    *ns = *samp_max - *samp_min + 1;
    *nl = *line_max - *line_min + 1;
}

void update_poly_extents(meta_parameters *meta)
{
    int size_x, size_y;
    compute_extent(meta, &g_poly->extent_y_min, &g_poly->extent_y_max,
        &g_poly->extent_x_min, &g_poly->extent_x_max, &size_y, &size_x);

    char subset_info[128];
    snprintf(subset_info, 128,
        "Extent: Line %d-%d, Sample %d-%d\nOutput will be %dx%d LxS",
        g_poly->extent_y_min, g_poly->extent_y_max,
        g_poly->extent_x_min, g_poly->extent_x_max,
        size_y, size_x);

    put_string_to_label("subset_info_label", subset_info);
}

void save_subset(ImageInfo *ii)
{
    if (g_poly->n > 0) {
        if (crosshair_line > 0 && crosshair_samp > 0) {

            // clamp crosshair to image extent, if needed
            if (crosshair_line >= ii->meta->general->line_count)
                crosshair_line = ii->meta->general->line_count - 1;
            if (crosshair_samp >= ii->meta->general->sample_count)
                crosshair_samp = ii->meta->general->sample_count - 1;

            show_save_subset_window();
            set_defaults(ii);
            
            g_poly->show_extent = TRUE;
            fill_big(ii);

            update_save_subset_info();
        } else {
            asfPrintWarning("Can't save subset: No crosshair.\n");
        }
    } else {
        // shouldn't ever get in here...
        asfPrintWarning("Can't save subset: Polygon not defined.\n");
    }
}

static int pnpoly(int npol, double *xp, double *yp, double x, double y)
{
  int i, j, c = 0;
  for (i = 0, j = npol-1; i < npol; j = i++) {
    if ((((yp[i]<=y) && (y<yp[j])) ||
      ((yp[j]<=y) && (y<yp[i]))) &&
      (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))
      c = !c;
  }
  return c;
}

static float get_data(ImageInfo *ii, int what_to_save, int line, int samp)
{
    double t, s, d;
    meta_parameters *meta = ii->meta;
    CachedImage *data_ci = ii->data_ci;

    switch (what_to_save) {
        case PIXEL_VALUE:
            if (ii->data_ci->data_type == RGB_FLOAT) {
              // can't handle RGB subsets... return average of RGB values
              float r, g, b;
              cached_image_get_rgb_float(data_ci, line, samp, &r, &g, &b);
              return (r+g+b)/3.;
            }
            else {
              return cached_image_get_pixel(data_ci, line, samp);
            }

        case INCIDENCE_ANGLE:
            if (meta->sar)
                return R2D*meta_incid(meta, line, samp);
            else
                return 0;

        case LOOK_ANGLE:
            if (meta->sar)
                return R2D*meta_look(meta, line ,samp);
            else
                return 0;

        case SLANT_RANGE:
            if (meta->sar) {
                meta_get_timeSlantDop(meta, line, samp, &t, &s, NULL);
                return s;
            } else
                return 0;

        case SCALED_PIXEL_VALUE:
            if (ii->data_ci->data_type == RGB_FLOAT) {
              // can't handle RGB subsets... return average of scaled values
              float r, g, b;
              cached_image_get_rgb_float(data_ci, line, samp, &r, &g, &b);
              int rs = calc_rgb_scaled_pixel_value(&(ii->stats_r), r);
              int gs = calc_rgb_scaled_pixel_value(&(ii->stats_g), g);
              int bs = calc_rgb_scaled_pixel_value(&(ii->stats_b), b);
              return (rs+gs+bs)/3.;
            }
            else {
              return calc_scaled_pixel_value(&(ii->stats),
                cached_image_get_pixel(data_ci, line, samp));
            }

        case TIME:
            if (meta->sar) {
                meta_get_timeSlantDop(meta, line, samp, &t, &s, NULL);
                return t;
            } else
                return 0;

        case DOPPLER:
            if (meta->sar) {
                meta_get_timeSlantDop(meta, line, samp, &t, &s, &d);
                return d;
            } else
                return 0;
 
        default:
            assert(0);
            return 0;
    }
}

static meta_parameters *build_metadata(meta_parameters *meta,
                                       const char *out_file, int nl, int ns,
                                       int line_min, int samp_min,
                                       data_type_t data_type, int what_to_save)
{
    meta_parameters *out_meta = meta_copy(meta);
    if (out_meta->sar) {
        if (out_meta->sar->original_line_count == meta->general->line_count)
            out_meta->sar->original_line_count = meta->general->line_count;
        if (out_meta->sar->original_sample_count == meta->general->sample_count)
            out_meta->sar->original_sample_count = meta->general->sample_count;
    }
    out_meta->general->line_count = nl;
    out_meta->general->sample_count = ns;
    out_meta->general->data_type = data_type;
    out_meta->general->image_data_type = IMAGE;
    out_meta->general->start_line += line_min;
    out_meta->general->start_sample += samp_min;

    out_meta->general->band_count = 1;

    switch (what_to_save) {
      default:
        strcpy(out_meta->general->bands, "");
        break;
      case PIXEL_VALUE:
        // can leave the bands string as it is
        break;
      case INCIDENCE_ANGLE:
        strcpy(out_meta->general->bands, "INCIDENCE_ANGLE");
        break;
      case LOOK_ANGLE:
        strcpy(out_meta->general->bands, "LOOK_ANGLE");
        break;
      case SLANT_RANGE:
        strcpy(out_meta->general->bands, "SLANT_RANGE");
        break;
      case SCALED_PIXEL_VALUE:
        strcpy(out_meta->general->bands, "SCALED_PIXEL_VALUE");
        break;
      case TIME:
        strcpy(out_meta->general->bands, "TIME");
        break;
      case DOPPLER:
        strcpy(out_meta->general->bands, "DOPPLER");
        break;
    }

    // From what I can figure, we don't need to update the projection block,
    // the startX/Y values are calculated from the original line/sample coords

    //if (out_meta->projection) {
    //    double bX, mX, bY, mY;
    //    bY = out_meta->projection->startY;
    //    bX = out_meta->projection->startX;
    //    mY = out_meta->projection->perY;
    //    mX = out_meta->projection->perX;
    //    out_meta->projection->startY = bY + mY * line_min;
    //    out_meta->projection->startX = bX + mX * samp_min;
    //}

    return out_meta;
}

static void define_clipping_region(meta_parameters *meta,
                                   int *n, double *xp, double *yp)
{
    // starts at the crosshair
    xp[0] = crosshair_samp;
    yp[0] = crosshair_line;

    // goes through the control-clicked points
    int i;
    for (i=0; i<g_poly->n; ++i) {
        xp[i+1] = g_poly->samp[i];
        yp[i+1] = g_poly->line[i];
    }

    // clamp polygon points to the image extents
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    for (i=0; i<=g_poly->n; ++i) {
        if (yp[i]<0) yp[i]=0;
        if (yp[i]>=nl) yp[i]=nl-1;

        if (xp[i]<0) xp[i]=0;
        if (xp[i]>=ns) xp[i]=ns-1;
    }

    // close the polygon
    xp[g_poly->n+1] = xp[0];
    yp[g_poly->n+1] = yp[0];

    *n = g_poly->n + 2;
}

static int save_as_asf(ImageInfo *ii,
                       const char *out_file, int what_to_save,
                       int strict_boundary, int load)
{
    // See if we can open the output file up front
    FILE *outFp = fopen(out_file, "wb");
    if (!outFp) {
        // failed to open the output file!
        char errbuf[1024];
        snprintf(errbuf, 1024, "Failed to open %s: %s", out_file,
            strerror(errno));
        message_box(errbuf);
        strcat(errbuf, "\n");
        printf("%s", errbuf);
        return FALSE; // failure
    }

    assert (g_poly->n > 0);
    assert (crosshair_line > 0 && crosshair_samp > 0);

    meta_parameters *meta = ii->meta;

    // figure out where to chop
    int line_min, line_max, samp_min, samp_max, nl, ns;
    compute_extent(meta, &line_min, &line_max, &samp_min, &samp_max,
        &nl, &ns);

    // generate metadata
    char *out_metaname = appendExt(out_file, ".meta");
    printf("Generating %s...\n", out_metaname);

    // data will be saved as floating point, except scaled pixel values,
    // which we can make bytes.
    data_type_t data_type = REAL32;
    if (what_to_save == SCALED_PIXEL_VALUE)
      data_type = ASF_BYTE;

    meta_parameters *out_meta =
      build_metadata(meta, out_file, nl, ns, line_min, samp_min, data_type,
                     what_to_save);

    // put_float_line() will always dump BYTE data if the optical block
    // is present... we want to be in control of the data type, so we must
    // wipe out this block
    if (out_meta->optical) {
      FREE(out_meta->optical);
      out_meta->optical=NULL;
    }

    if (what_to_save == LAT_LON_2_BAND) {
      out_meta->general->band_count = 2;
      strcpy(out_meta->general->bands, "LAT,LON");
    }

    // define clipping region, if necessary
    double xp[MAX_POLY_LEN+2], yp[MAX_POLY_LEN+2];
    int i,j,n=0;

    if (strict_boundary)
        define_clipping_region(meta, &n, xp, yp);

    float ndv = 0;
    if (meta_is_valid_double(out_meta->general->no_data))
        ndv = out_meta->general->no_data;
    else if (strict_boundary) // need to set a no data value in this case
        out_meta->general->no_data = 0.;

    meta_write(out_meta, out_metaname);

    // now actually write the data
    printf("Generating %s...\n", out_file);

    if (what_to_save == LAT_LON_2_BAND) {
      // dump a 2-band image, lat & lon data
      float *lats = MALLOC(sizeof(float)*ns);
      float *lons = MALLOC(sizeof(float)*ns);
      for (i=0; i<nl; ++i) {
        int l = line_min+i;
        for (j=0; j<ns; ++j) {
            int s = samp_min+j;
            if (!strict_boundary || pnpoly(n, xp, yp, s, l)) {
                double lat, lon;
                meta_get_latLon(meta, l, s, 0, &lat, &lon);
                lats[j] = (float)lat;
                lons[j] = (float)lon;
            }
            else {
                lats[j] = ndv;
                lons[j] = ndv;
            }
        }
        put_band_float_line(outFp, out_meta, 0, i, lats);
        put_band_float_line(outFp, out_meta, 1, i, lons);
        asfLineMeter(i,nl);
      }
      free(lats);
      free(lons);
    }
    else {
      // normal case
      float *buf = MALLOC(sizeof(float)*ns);
      for (i=0; i<nl; ++i) {
        int l = line_min+i;
        for (j=0; j<ns; ++j) {
            int s = samp_min+j;
            float val;
            if (!strict_boundary || pnpoly(n, xp, yp, s, l)) {
                val = get_data(ii, what_to_save, l, s);
            }
            else {
                val = ndv;
            }
            buf[j] = val;
        }
        put_float_line(outFp, out_meta, i, buf);
        asfLineMeter(i,nl);
      }
      free(buf);
    }
    fclose(outFp);
    meta_free(out_meta);

    // load the generated file if we were told to
    if (load)
        load_file(out_file);

    return TRUE;
}

static int save_as_csv(ImageInfo *ii, 
                       const char *out_file, int what_to_save,
                       int strict_boundary, int load)
{
    int i,j;

    assert (g_poly->n > 0);
    assert (crosshair_line > 0 && crosshair_samp > 0);

    meta_parameters *meta = ii->meta;

    int line_min, line_max, samp_min, samp_max, nl, ns;
    compute_extent(meta, &line_min, &line_max, &samp_min, &samp_max,
        &nl, &ns);

    if (nl>500 || ns>500) {
        // too big for csv -- Excel etc. will choke
        char errbuf[1024];
        snprintf(errbuf, 1024,
             "\nRegion is too large (%dx%d) to export as CSV (500x500 max)\n\n",
             nl, ns);
        message_box(errbuf);
        printf("%s", errbuf);
        return FALSE; // failure
    }

    FILE *outFp = fopen(out_file, "w");
    if (!outFp) {
        // failed to open the output file!
        char errbuf[1024];
        snprintf(errbuf, 1024, "Failed to open %s: %s", out_file,
            strerror(errno));
        message_box(errbuf);
        strcat(errbuf, "\n");
        printf("%s", errbuf);
        return FALSE; // failure
    }

    printf("Generating %s...\n", out_file);

    // define clipping region, if necessary
    double xp[MAX_POLY_LEN+2], yp[MAX_POLY_LEN+2];
    int n=0;

    if (strict_boundary)
        define_clipping_region(meta, &n, xp, yp);

    // generate csv
    fprintf(outFp, ",");
    for (j=0; j<ns; ++j) {
      if (what_to_save==LAT_LON_2_BAND)        
        fprintf(outFp, "%d,%s", samp_min+j, j==ns-1 ? "\n" : ",");
      else
        fprintf(outFp, "%d%s", samp_min+j, j==ns-1 ? "\n" : ",");
    }
    if (what_to_save==LAT_LON_2_BAND) {
      fprintf(outFp, ",");
      for (j=0; j<ns; ++j) {
        fprintf(outFp, "Lat,Lon%s", j==ns-1 ? "\n" : ",");
      }
    }
    for (i=0; i<nl; ++i) {
        int l = line_min+i;
        fprintf(outFp, "%d,", l);
        for (j=0; j<ns; ++j) {
            int s = samp_min+j;
            if (what_to_save==LAT_LON_2_BAND) {
              float lat, lon;
              if (!strict_boundary || pnpoly(n, xp, yp, s, l)) {
                double dlat, dlon;
                meta_get_latLon(meta, l, s, 0, &dlat, &dlon);
                lat = (float)dlat;
                lon = (float)dlon;
              } else {
                lat = lon = 0.;
              }
              fprintf(outFp, "%f,%f%s", lat, lon, j==ns-1 ? "\n" : ",");
            }
            else {
              float val;
              if (!strict_boundary || pnpoly(n, xp, yp, s, l)) {
                val = get_data(ii, what_to_save, l, s);
              } else {
                val = 0;
              }
              fprintf(outFp, "%f%s", val, j==ns-1 ? "\n" : ",");
            }
        }
        asfLineMeter(i,nl);
    }

    fclose(outFp);

    // if requested, open up the csv with an external viewer
    if (load)
        open_csv(out_file);

    return TRUE;
}

// For now, I'm taking out the use of windows browse widget
// the GTK FileChooser implementation is actually nicer!
// Remove "_XXX" from the line below to revert to windows-specific version
#ifdef win32_XXX
SIGNAL_CALLBACK void on_browse_button_clicked(GtkWidget *w)
{
    BROWSEINFO bi = { 0 };
    bi.lpszTitle = "Select Output Directory";
    LPITEMIDLIST pidl = SHBrowseForFolder ( &bi );
    if ( pidl != 0 )
    {
        TCHAR path[MAX_PATH];
        if ( SHGetPathFromIDList ( pidl, path ) )
            put_string_to_entry("dir_entry", path);
    }
}
#else
SIGNAL_CALLBACK void cancel_clicked(GtkWidget *w, gpointer user_data)
{
    GtkWidget *browse_widget = (GtkWidget *)user_data;
    gtk_widget_hide(browse_widget);
}

SIGNAL_CALLBACK void ok_clicked(GtkWidget *w, gpointer user_data)
{
    GtkWidget *browse_widget = (GtkWidget *)user_data;
    GSList *files = gtk_file_chooser_get_filenames(
        GTK_FILE_CHOOSER(browse_widget));

    gtk_widget_hide(browse_widget);

    // we will actually never get more than one file back
    if (files) {
        GSList *iter = files;

        do {
          gchar *s = (gchar *) iter->data;
          put_string_to_entry("dir_entry", s);
          g_free(s);
          iter =  iter->next;
        }
        while(iter);

        g_slist_free(files);
    }
}

SIGNAL_CALLBACK void on_browse_button_clicked(GtkWidget *w)
{
    GtkWidget *parent = get_widget_checked("save_subset_window");

    GtkWidget *browse_widget = NULL;
    
    if (!browse_widget) {
        browse_widget = gtk_file_chooser_dialog_new(
            "Select Output Directory", GTK_WINDOW(parent),
            GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
            GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
            NULL);

        // we need to extract the buttons, so we can connect them to our
        // button handlers, above
        GtkHButtonBox *box =
            (GtkHButtonBox*)(((GtkDialog*)browse_widget)->action_area);
        GList *buttons = box->button_box.box.children;

        GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
        GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

        g_signal_connect((gpointer)cancel_btn, "clicked",
            G_CALLBACK(cancel_clicked), browse_widget);
        g_signal_connect((gpointer)ok_btn, "clicked",
            G_CALLBACK(ok_clicked), browse_widget);

        // we need to make these modal -- if the user opens multiple "open"
        // dialogs, we'll get confused on the callbacks
        gtk_window_set_modal(GTK_WINDOW(browse_widget), TRUE);
        gtk_window_set_destroy_with_parent(GTK_WINDOW(browse_widget), TRUE);
        gtk_dialog_set_default_response(GTK_DIALOG(browse_widget),
                                        GTK_RESPONSE_OK);
    }

    gtk_widget_show(browse_widget);
}
#endif

SIGNAL_CALLBACK void on_save_button_clicked(GtkWidget *w)
{
    // save the specified subset!
    const char *dir = get_string_from_entry("dir_entry");
    const char *file = get_string_from_entry("filename_entry");
    const char *ext = get_format()==FORMAT_CSV ? "csv" : "img";

    if (strlen(file) == 0) {
        message_box("Please provide a name for the file.");
        return;
    }

    char *save_file = 
        MALLOC(sizeof(char)*(strlen(dir)+strlen(file)+strlen(ext)+5));

    if (strlen(dir) > 0) {
        if (dir[strlen(dir)-1] == DIR_SEPARATOR || dir[strlen(dir)-1] == '/')
            sprintf(save_file, "%s%s.%s", dir, file, ext);
        else
            sprintf(save_file, "%s/%s.%s", dir, file, ext);
    } else
        sprintf(save_file, "%s.%s", file, ext);

    int strict_boundary = get_checked("strict_boundary_checkbutton");
    int load = get_checked("load_saved_subset_checkbutton");
    int ok;

    if (get_format()==FORMAT_CSV) {
        ok = save_as_csv(curr, save_file, get_what_to_save(),
                         strict_boundary, load);
    } else {
        assert(get_format()==FORMAT_ASF_INTERNAL);
        ok = save_as_asf(curr, save_file, get_what_to_save(),
                         strict_boundary, load);
    }

    free(save_file);

    if (ok)
        close_subset_window(curr);
}

SIGNAL_CALLBACK void on_save_subset_button_clicked(GtkWidget *w)
{
    // the naming is confusing -- here, the user hasn't actually saved
    // anything yet, this is just the event handler for when they click
    // the button that brings up the "save" window.  The function
    // "on_save_button_clicked" above is when they've clicked "save" in
    // that window, and where we actually will save the data
    save_subset(curr);
}
