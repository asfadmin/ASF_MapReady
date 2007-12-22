#include <unistd.h>

#define FORMAT_ASF_INTERNAL 1
#define FORMAT_CSV 2

#define PIXEL_VALUE 0
#define INCIDENCE_ANGLE 1
#define LOOK_ANGLE 2
#define SLANT_RANGE 3
#define SCALED_PIXEL_VALUE 4
#define TIME 5
#define DOPPLER 6

#ifdef win32
#define BYTE __byte
#include "asf.h"
#include "asf_meta.h"
#undef BYTE
#include <windows.h>
#include <shellapi.h>
#include <shlobj.h>
#endif

#include "asf_view.h"

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

// try to find a program we can use to view the generated csv files
static const char * detect_csv_assoc()
{
    static char *csv_app = NULL;
    if (!csv_app) {
        char *csv_file = find_in_share("asf_view_cfg.csv");

#ifdef win32
        // On Windows, use the file association table to find out what we
        // can do with csv files.
        char path[1024];
        int ret = (int)FindExecutable((LPCTSTR)csv_file,
            (LPCTSTR)get_asf_share_dir(), (LPTSTR)path);
        if (ret > 32 && strlen(path) > 0) {
            csv_app = escapify(path);
            printf("Path to CSV Application: %s\n", csv_app);
        } else {
            if (ret==SE_ERR_FNF)
                printf("File not found: %s\n", csv_file);
            else if (ret==SE_ERR_NOASSOC)
                printf("No association for: %s\n", csv_file);
            else if (ret==SE_ERR_OOM)
                printf("Out of resources.\n");
            else
                printf("Unknown error! (return value: %d)\n", ret);

            csv_app = STRDUP("notepad.exe");
            printf("CSV Application not found -- using notepad.\n");
        }
        FREE(csv_file);
#else
        // On Linux, get the app from the configuration file
        FILE *cfg = fopen(csv_file, "r");
        if (cfg) {
            char tmp[1024];
            while (fgets(tmp, 1024, cfg) != NULL) {
                if (strncmp_case(tmp, "CSV,", 4) == 0) {
                    csv_app = STRDUP(tmp+4);
                    printf("CSV Application from config file: %s\n", csv_app);
                }
            }
            if (!csv_app)
                csv_app = STRDUP("");
            fclose(cfg);
        } else {
            printf("Failed to open %s: %s\n", csv_file, strerror(errno));
            csv_app = STRDUP("");
            printf("CSV Application not found.\n");
        }
#endif
    }
    return csv_app;
}

// mapping between combobox entry indexes and the #defines
static int get_what_to_save()
{
    GtkWidget *dcb = get_widget_checked("data_combobox");
    int what = gtk_combo_box_get_active(GTK_COMBO_BOX(dcb));

    // I know that at the moment the value of "what" corresponds with
    // these defined values...
    switch (what) {
        case 0: return PIXEL_VALUE;
        case 1: return INCIDENCE_ANGLE;
        case 2: return LOOK_ANGLE;
        case 3: return SLANT_RANGE;
        case 4: return TIME;
        case 5: return DOPPLER;
        case 6: return SCALED_PIXEL_VALUE;
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
    enable_widget("strict_boundary_checkbutton", g_poly->n > 1);
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
    set_combo_box_item_checked("data_combobox", 0);
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
            return cached_image_get_pixel(data_ci, line, samp);

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
            return calc_scaled_pixel_value(&(ii->stats),
                cached_image_get_pixel(data_ci, line, samp));

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
    const char *out_file, int nl, int ns, int line_min, int samp_min)
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
    out_meta->general->data_type = REAL32; //should we really do this?
    out_meta->general->band_count = 1;
    out_meta->general->start_line += line_min;
    out_meta->general->start_sample += samp_min;
    strcpy(out_meta->general->bands, "");

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
        printf(errbuf);
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

    meta_parameters *out_meta =
        build_metadata(meta, out_file, nl, ns, line_min, samp_min);

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
    float *buf = MALLOC(sizeof(float)*ns);
    printf("Generating %s...\n", out_file);

    for (i=0; i<nl; ++i) {
        int l = line_min+i;
        for (j=0; j<ns; ++j) {
            int s = samp_min+j;
            float val;
            if (!strict_boundary || pnpoly(n, xp, yp, s, l)) {
                val = get_data(ii, what_to_save, l, s);
            } else {
                val = ndv;
            }
            buf[j] = val;
        }
        put_float_line(outFp, out_meta, i, buf);
        asfLineMeter(i,nl);
    }

    free(buf);
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

    printf("Generating %s...\n", out_file);
    FILE *outFp = fopen(out_file, "w");
    if (!outFp) {
        // failed to open the output file!
        char errbuf[1024];
        snprintf(errbuf, 1024, "Failed to open %s: %s", out_file,
            strerror(errno));
        message_box(errbuf);
        strcat(errbuf, "\n");
        printf(errbuf);
        return FALSE; // failure
    }

    assert (g_poly->n > 0);
    assert (crosshair_line > 0 && crosshair_samp > 0);

    meta_parameters *meta = ii->meta;

    int line_min, line_max, samp_min, samp_max, nl, ns;
    compute_extent(meta, &line_min, &line_max, &samp_min, &samp_max,
        &nl, &ns);

    // define clipping region, if necessary
    double xp[MAX_POLY_LEN+2], yp[MAX_POLY_LEN+2];
    int n=0;

    if (strict_boundary)
        define_clipping_region(meta, &n, xp, yp);

    // generate csv
    fprintf(outFp, ",");
    for (j=0; j<ns; ++j) {
        fprintf(outFp, "%d%s", samp_min+j, j==ns-1 ? "\n" : ",");
    }
    for (i=0; i<nl; ++i) {
        int l = line_min+i;
        fprintf(outFp, "%d,", l);
        for (j=0; j<ns; ++j) {
            int s = samp_min+j;
            float val;
            if (!strict_boundary || pnpoly(n, xp, yp, s, l)) {
                val = get_data(ii, what_to_save, l, s);
            } else {
                val = 0;
            }
            fprintf(outFp, "%f%s", val, j==ns-1 ? "\n" : ",");
        }
        asfLineMeter(i,nl);
    }

    fclose(outFp);

    // if requested, open up the csv with an external viewer
    if (load) {
        const char *csv = detect_csv_assoc();
        int have_csv_viewer = csv && strlen(csv) > 0;
        if (have_csv_viewer) {
            const char *csv_app = detect_csv_assoc(out_file);
            int pid = fork();
            if (pid == 0) {
                asfSystem("\"%s\" \"%s\"", csv_app, out_file);
                exit(EXIT_SUCCESS);
            }
        } else {
            char errbuf[1024];
            snprintf(errbuf, 1024, "Don't know how to load: %s", out_file);
            message_box(errbuf);
            strcat(errbuf, "\n");
            printf(errbuf);
        }
    }

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
