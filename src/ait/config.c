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

// private struct for tracking the location of font-modified text in
// the "summary" section.
typedef struct section_mark
{
    int section_start;
    int section_end;
    int type; // 1= Section heading, 2= bad data (consts below)
} section_mark_t;

static const int MarkerTypeHeading = 1;
static const int MarkerTypeBadData = 2;

static int double_equals_tol(double a, double b, double tol)
{
    return fabs(a-b)<tol;
}

static double get_double_from_entry(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));
    double val = atof(str);
    return val;
}

static double get_double_from_entry_blank(const char *widget_name,
                                          double blank_val)
{
    GtkWidget *w = get_widget_checked(widget_name);
    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));
    if (strlen(str) > 0) {
        double val = atof(str);
        return val;
    } else {
        return blank_val;
    }
}

static int get_int_from_entry(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));
    int val = atoi(str);
    return val;
}

static long get_long_from_entry(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));
    long val = atol(str);
    return val;
}

static int get_bool_from_checkbutton(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
}

static void get_string_from_entry(char *dest, const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));

    if (strlen(str) > MAX_STRING_LEN-1)
    {
        asfPrintWarning("Truncating string to %d characters: %s\n==>%s\n",
            MAX_STRING_LEN-1, widget_name, str);

        strncpy(dest, str, MAX_STRING_LEN-1);
    }
    else
    {
        strcpy(dest, str);
    }
}

static void get_str(char *dest, const char *widget)
{
    get_string_from_entry(dest, widget);
}

static void get_dbl(double *val, const char *widget)
{
    *val = get_double_from_entry(widget);
}

static void get_dbl_blank(double *val, const char *widget, double blank_val)
{
    *val = get_double_from_entry_blank(widget, blank_val);
}

static void get_chk(int *val, const char *widget)
{
    *val = get_bool_from_checkbutton(widget);
}

static void get_int(int *val, const char *widget)
{
    *val = get_int_from_entry(widget);
}

static void get_lng(long *val, const char *widget)
{
    *val = get_long_from_entry(widget);
}

static meta_projection *
get_projection_options(int *force, resample_method_t *resample_method)
{
    meta_projection *proj;
    int geocode_on;

    get_chk(&geocode_on, "geocode_checkbutton");

    if (geocode_on) {
        proj = meta_projection_init();

        GtkWidget *w = get_widget_checked("projection_optionmenu");
        int index = gtk_option_menu_get_history(GTK_OPTION_MENU(w));

        // we set it up so that the projections in the optionmenu are in the same
        // order listed in the enum projection_type_t
        switch (index)
        {
            case 0: proj->type = PROJ_UTM; break;
            case 1: proj->type = PROJ_PS; break;
            case 2: proj->type = PROJ_ALBERS; break;
            case 3: proj->type = PROJ_LAMAZ; break;
            case 4: proj->type = PROJ_LAMCC; break;
            default: assert(FALSE); break;
        }

        // Projection Parameters
        switch (proj->type) {
            case PROJ_UTM:
                get_int(&proj->param.utm.zone, "zone_entry");
                break;

            case PROJ_PS:
                get_dbl(&proj->param.ps.slat, "first_standard_parallel_entry");
                get_dbl(&proj->param.ps.slon, "central_meridian_entry");
                break;

            case PROJ_LAMCC:
                get_dbl(&proj->param.lamcc.plat1, "first_standard_parallel_entry");
                get_dbl(&proj->param.lamcc.plat2, "second_standard_parallel_entry");
                get_dbl(&proj->param.lamcc.lon0, "central_meridian_entry");
                get_dbl(&proj->param.lamcc.lat0, "latitude_of_origin_entry");
                break;

            case PROJ_LAMAZ:
                get_dbl(&proj->param.lamaz.center_lon, "central_meridian_entry");
                get_dbl(&proj->param.lamaz.center_lat, "latitude_of_origin_entry");

            case PROJ_ALBERS:
                get_dbl(&proj->param.albers.std_parallel1, "first_standard_parallel_entry");
                get_dbl(&proj->param.albers.std_parallel2, "second_standard_parallel_entry");
                get_dbl(&proj->param.albers.center_meridian, "central_meridian_entry");
                get_dbl(&proj->param.albers.orig_latitude, "latitude_of_origin_entry");
                break;

            default:
                assert(FALSE);
                break;
        }

        // Datum
        w = get_widget_checked("datum_optionmenu");
        index = gtk_option_menu_get_history(GTK_OPTION_MENU(w));

        switch (index) {
            case 0: proj->datum = WGS84_DATUM; break;
            case 1: proj->datum = NAD27_DATUM; break;
            case 2: proj->datum = NAD83_DATUM; break;
            default: assert(FALSE); break;
        }

        // Average Height
        int height_checked;
        get_chk(&height_checked, "height_checkbutton");
        if (height_checked)
            get_dbl(&proj->height, "height_entry");
        else
            proj->height = 0.0;

        // Pixel Size
        int pixel_size_checked;
        get_chk(&pixel_size_checked, "pixel_size_checkbutton");
        if (pixel_size_checked) {
            get_dbl(&proj->perX, "pixel_size_entry");
            if (proj->perX > 0)
                proj->perY = proj->perX;
            else
                proj->perX = MAGIC_UNSET_DOUBLE; //user didn't enter anything
        }

        // resampling method
        w = get_widget_checked("datum_optionmenu");
        index = gtk_option_menu_get_history(GTK_OPTION_MENU(w));

        switch (index) {
            case 0: *resample_method = RESAMPLE_NEAREST_NEIGHBOR; break;
            case 1: *resample_method = RESAMPLE_BILINEAR; break;
            case 2: *resample_method = RESAMPLE_BICUBIC; break;
            default: assert(FALSE); break;
        }

        // ignore projection errors
        get_chk(force, "force_checkbutton");
    }
    else
    {
        // no geocoding selected
        proj = NULL;
    }

    return proj;
}

static int
text_to_index(const char *widget_name, const char *selected_text)
{
    if (strcmp(widget_name, "mode_optionmenu") == 0) {
        // DEM, DINSAR
        if (strcmp(uc(selected_text), "DEM") == 0) return 0;
        if (strcmp(uc(selected_text), "DINSAR") == 0) return 1;
        return 0; //default
    } else if (strcmp(widget_name, "data_type_optionmenu") == 0) {
        // STF, RAW, SLC
        if (strcmp(uc(selected_text), "STF") == 0) return 0;
        if (strcmp(uc(selected_text), "RAW") == 0) return 1;
        if (strcmp(uc(selected_text), "SLC") == 0) return 2;
        return 2; //default
    } else if (strcmp(widget_name, "phase_unwrapping_optionmenu") == 0) {
        // Escher, Snaphu
        if (strcmp(uc(selected_text), "ESCHER") == 0) return 0;
        if (strcmp(uc(selected_text), "SNAPHU") == 0) return 1;
        return 0; //defualt
    } else if (strcmp(widget_name, "datum_optionmenu") == 0) {
        // WGS84, NAD27, NAD83
        if (strcmp(uc(selected_text), "WGS84") == 0) return 0;
        if (strcmp(uc(selected_text), "NAD27") == 0) return 1;
        if (strcmp(uc(selected_text), "NAD83") == 0) return 2;
        return 0; //defualt
    } else if (strcmp(widget_name, "resample_optionmenu") == 0) {
        // WGS84, NAD27, NAD83
        if (strcmp(uc(selected_text), "NEAREST NEIGHBOR") == 0) return 0;
        if (strcmp(uc(selected_text), "BILINEAR") == 0) return 1;
        if (strcmp(uc(selected_text), "BICUBIC") == 0) return 2;
        return 1; //defualt
    }
    assert(FALSE);
    return -1;
}

static void 
set_combo_box_item(const char *widget_name, const char *selected_text)
{
    GtkWidget *w = get_widget_checked(widget_name);
    int index = text_to_index(widget_name, selected_text);
    gtk_option_menu_set_history(GTK_OPTION_MENU(w), index);
}

static const char *
index_to_text(const char *widget_name, int index)
{
    if (strcmp(widget_name, "mode_optionmenu") == 0) {
        // DEM, DINSAR
        switch (index) {
            case 0: return "DEM";
            case 1: return "DINSAR";
        }
        assert(FALSE);
    } else if (strcmp(widget_name, "data_type_optionmenu") == 0) {
        // STF, RAW, SLC
        switch (index) {
            case 0: return "STF";
            case 1: return "RAW";
            case 2: return "SLC";
        }
        assert(FALSE);
    } else if (strcmp(widget_name, "phase_unwrapping_optionmenu") == 0) {
        // Escher, Snaphu
        switch (index) {
            case 0: return "Escher";
            case 1: return "Snaphu";
        }
        assert(FALSE);
    }
    assert(FALSE);
    return "";    
}

static const char *
get_combo_box_item(const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    int index = gtk_option_menu_get_history(GTK_OPTION_MENU(w));
    return index_to_text(widget_name, index);    
}

static const char * projection_type_str(projection_type_t proj_type)
{
    switch (proj_type)
    {
    case PROJ_UTM:
        return "UTM";

    case PROJ_PS:
        return "Polar Stereographic";

    case PROJ_ALBERS:
        return "Albers Conical Equal Area";

    case PROJ_LAMAZ:
        return "Lambert Azimuthal Equal Area";

    case PROJ_LAMCC:
        return "Lambert Conformal Conic";

    default:
        return MAGIC_UNSET_STRING;
    }
}

void free_ait_params(ait_params_t *params)
{
    if (params) {
        if (params->name)
            free(params->name);
        if (params->proj)
            free(params->proj);
        if (params->cfg)
            free(params->cfg);
        free(params);
    }
}

ait_params_t *get_settings_from_gui()
{
    ait_params_t *ret = MALLOC(sizeof(ait_params_t));

    char cfg_name[MAX_STRING_LEN+1];
    get_str(cfg_name, "configuration_file_entry");
    ret->name = MALLOC(sizeof(char)*(strlen(cfg_name)+2));
    strcpy(ret->name, cfg_name);

    dem_config *cfg = create_config_with_defaults();

    // "base" is the config file's basename
    cfg->general->base = get_basename(cfg_name);

    strcpy(cfg->general->mode, get_combo_box_item("mode_optionmenu"));
    get_str(cfg->general->dem, "reference_dem_entry");
    //cfg->general->def_val = new_blank_str();
    strcpy(cfg->general->data_type,
           get_combo_box_item("data_type_optionmenu"));
    get_chk(&cfg->general->deskew, "deskew_checkbutton");
    //cfg->general->doppler = new_blank_str();
    get_dbl_blank(&cfg->general->lat_begin, "lat_begin_entry", -99);
    get_dbl_blank(&cfg->general->lat_end, "lat_end_entry", -99);
    //cfg->general->coreg = new_blank_str();
    get_int(&cfg->general->max_off, "maximum_offset_entry");
    //cfg->general->mflag = 0;
    //cfg->general->mask = new_blank_str();
    //cfg->general->test = 0;
    get_chk(&cfg->general->short_config, "short_configuration_file_checkbutton");

    get_str(cfg->master->path, "master_image_path_entry");
    get_str(cfg->master->data, "master_image_data_entry");
    get_str(cfg->master->meta, "master_image_metadata_entry");

    get_str(cfg->slave->path, "slave_image_path_entry");
    get_str(cfg->slave->data, "slave_image_data_entry");
    get_str(cfg->slave->meta, "slave_image_metadata_entry");

    get_str(cfg->ingest->prc_master, "ingest_precise_master_entry");
    get_str(cfg->ingest->prc_slave, "ingest_precise_slave_entry");
    get_chk(&cfg->ingest->prcflag, "ingest_precise_orbits_checkbutton");
    //cfg->ingest->status = new_str("new");

    //cfg->doppler->status = new_str("new");

    get_int(&cfg->coreg_p1->patches, "coregister_first_patches_entry");
    get_lng(&cfg->coreg_p1->start_master, "coregister_first_start_master_entry");
    get_lng(&cfg->coreg_p1->start_slave, "coregister_first_start_slave_entry");
    get_int(&cfg->coreg_p1->grid, "coregister_first_grid_entry");
    get_chk(&cfg->coreg_p1->fft, "coregister_first_fft_checkbutton");
    get_int(&cfg->coreg_p1->off_az, "coregister_first_offset_azimuth_entry");
    get_int(&cfg->coreg_p1->off_rng, "coregister_first_offset_range_entry");
    //cfg->coreg_p1->status = new_str("new");

    get_int(&cfg->coreg_pL->patches, "coregister_last_patches_entry");
    get_lng(&cfg->coreg_pL->start_master, "coregister_last_start_master_entry");
    get_lng(&cfg->coreg_pL->start_slave, "coregister_last_start_slave_entry");
    get_int(&cfg->coreg_pL->grid, "coregister_last_grid_entry");
    get_chk(&cfg->coreg_pL->fft, "coregister_last_fft_checkbutton");
    get_int(&cfg->coreg_pL->off_az, "coregister_last_offset_azimuth_entry");
    get_int(&cfg->coreg_pL->off_rng, "coregister_last_offset_range_entry");
    //cfg->coreg_pL->status = new_str("new");

    //cfg->doppler_per_patch->status = new_str("new");

    get_lng(&cfg->ardop_master->start_offset, "ardop_master_start_offset_entry");
    get_lng(&cfg->ardop_master->end_offset, "ardop_master_end_offset_entry");
    get_int(&cfg->ardop_master->patches, "ardop_master_patches_entry");
    get_chk(&cfg->ardop_master->power, "ardop_master_power_flag_checkbutton");
    //cfg->ardop_master->power_img = new_blank_str();
    //cfg->ardop_master->status = new_str("new");

    get_lng(&cfg->ardop_slave->start_offset, "ardop_slave_start_offset_entry");
    get_lng(&cfg->ardop_slave->end_offset, "ardop_slave_end_offset_entry");
    get_int(&cfg->ardop_slave->patches, "ardop_slave_patches_entry");
    get_chk(&cfg->ardop_slave->power, "ardop_slave_power_flag_checkbutton");
    //cfg->ardop_slave->power_img = new_blank_str();
    //cfg->ardop_slave->status = new_str("new");

    //cfg->cpx_autofilter->status = new_str("new");

    //cfg->coreg_slave->grid = 20;
    //cfg->coreg_slave->fft = 1;
    //cfg->coreg_slave->sinc = 0;
    //cfg->coreg_slave->warp = 0;
    //cfg->coreg_slave->status = new_str("new");

    //cfg->igram_coh->igram = new_blank_str();
    //cfg->igram_coh->coh = new_blank_str();
    get_dbl(&cfg->igram_coh->min, "interferogram_min_coherence_entry");
    get_chk(&cfg->igram_coh->ml, "interferogram_multilook_checkbutton");
    //cfg->igram_coh->status = new_str("new");

    get_dbl(&cfg->offset_match->max, "offset_matching_max_entry");
    //cfg->offset_match->status = new_str("new");

    //cfg->sim_phase->seeds = new_blank_str();
    //cfg->sim_phase->status = new_str("new");

    //cfg->dinsar->igram = new_blank_str();
    //cfg->dinsar->status = new_str("new");

    //cfg->deramp_ml->status = new_str("new");

    strcpy(cfg->unwrap->algorithm,
           get_combo_box_item("phase_unwrapping_optionmenu"));
    get_chk(&cfg->unwrap->flattening, "phase_unwrapping_flattening_checkbutton");
    get_int(&cfg->unwrap->procs, "phase_unwrapping_processors_entry");
    get_int(&cfg->unwrap->tiles_azimuth, "phase_unwrapping_tiles_azimuth_entry");
    get_int(&cfg->unwrap->tiles_range, "phase_unwrapping_tiles_range_entry");
    get_int(&cfg->unwrap->overlap_azimuth, "phase_unwrapping_overlap_azimuth_entry");
    get_int(&cfg->unwrap->overlap_range, "phase_unwrapping_overlap_range_entry");
    get_dbl(&cfg->unwrap->filter, "phase_unwrapping_filter_entry");
    get_int(&cfg->unwrap->tiles_per_degree, "phase_unwrapping_tiles_per_degree_entry");
    //cfg->unwrap->qc = new_blank_str();
    //cfg->unwrap->status = new_str("new");

    get_int(&cfg->refine->iter, "baseline_refinement_iterations_entry");
    get_int(&cfg->refine->max, "baseline_refinement_max_iterations_entry");
    //cfg->refine->status = new_str("new");

    //cfg->elevation->dem = new_blank_str();
    //cfg->elevation->error = new_blank_str();
    //cfg->elevation->status = new_str("new");

    //cfg->ground_range->status = new_str("new");
    //cfg->geocode->dem = new_blank_str();
    //cfg->geocode->amp = new_blank_str();
    //cfg->geocode->error = new_blank_str();
    //cfg->geocode->coh = new_blank_str();
    //cfg->geocode->name = new_str("utm");
    //cfg->geocode->proj = new_blank_str();
    //sprintf(cfg->geocode->proj, "%s/projections/utm/utm.proj", 
    //get_asf_share_dir());
    //cfg->geocode->resample = new_str("bilinear");
    //cfg->geocode->pixel_spacing = 20;
    //cfg->geocode->status = new_str("new");

    //cfg->export->format = new_str("geotiff");
    //cfg->export->status = new_str("new");
    /*
    get_string_from_entry(cfg->ingest->prc_master,
    "ingest_precise_master_entry");
    cfg->igram_coh->min = get_double_from_entry(
    "interferogram_min_coherence_entry");
    cfg->ingest->prcflag = get_bool_from_checkbutton(
    "ingest_precise_orbits_checkbutton");
    cfg->coreg_p1->patches = get_int_from_entry(
    "coregister_first_patches_entry");
    */
    ret->cfg = cfg;

    ret->proj = get_projection_options(&ret->force, &ret->resample_method);
    cfg->geocode->pixel_spacing = ret->proj ? ret->proj->perX : 0.0;

    return ret;
}

static void put_string_to_entry(const char *str, const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_entry_set_text(GTK_ENTRY(w), str);
}

static void put_double_to_entry(double d, const char *widget_name)
{
    char str[64];
    sprintf(str, "%lf", d);
    put_string_to_entry(str, widget_name);
}

static void put_long_to_entry(long l, const char *widget_name)
{
    char str[64];
    sprintf(str, "%ld", l);
    put_string_to_entry(str, widget_name);
}

static void put_bool_to_checkbutton(int b, const char *widget_name)
{
    GtkWidget *w = get_widget_checked(widget_name);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), b);
}

static void put_str(const char *str, const char *widget)
{
    put_string_to_entry(str, widget);
}

static void put_dbl(double val, char *widget)
{
    put_double_to_entry(val, widget);
}

static void put_dbl_blank(double val, char *widget, double blank_val)
{
    if (double_equals_tol(val, blank_val, 0.000001))
        put_string_to_entry("", widget);
    else
        put_double_to_entry(val, widget);
}

static void put_chk(int val, char *widget)
{
    assert(val==1 || val==0);
    put_bool_to_checkbutton(val, widget);
}

static void put_lng(long val, char *widget)
{
    put_long_to_entry(val, widget);
}

static void put_int(int val, char *widget)
{
    put_long_to_entry((long)val, widget);
}

static void
add_to_summary_text_bad(int bad, section_mark_t *markers, int *nmarks,
                        char **summary_text, int *current_len,
                        int increment, const char *format, ...)
{
    char buf[1024];
    int len;

    va_list ap;
    va_start(ap, format);
    len = vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);

    if (len > 1000)
        asfPrintWarning("Lengthy message may have been truncated!\n");

    if (bad) {
        assert(*nmarks < 64); // we hard-coded the length to 64, below
        markers[*nmarks].section_start = strlen(*summary_text);
    }

    if (strlen(*summary_text) + len >= *current_len) {
        *current_len += increment;
        char *new = MALLOC(sizeof(char)*(*current_len));
        strcpy(new, *summary_text);
        strcat(new, buf);
        free(*summary_text);
        *summary_text = new;
    } else {
        strcat(*summary_text, buf);
    }

    if (bad) {
        markers[*nmarks].section_end = markers[*nmarks].section_start + len;
        markers[*nmarks].type = MarkerTypeBadData;
        ++(*nmarks);
    }
}

static void
add_to_summary_text(char **summary_text, int *current_len,
                    int increment, const char *format, ...)
{
    char buf[1024];
    int len;

    va_list ap;
    va_start(ap, format);
    len = vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);

    if (len > 1020)
        asfPrintWarning("Lengthy message may have been truncated!\n");

    if (strlen(*summary_text) + len >= *current_len) {
        *current_len += increment;
        char *new = MALLOC(sizeof(char)*(*current_len));
        strcpy(new, *summary_text);
        strcat(new, buf);
        free(*summary_text);
        *summary_text = new;
    } else {
        strcat(*summary_text, buf);
    }
}

static void
add_to_summary_text_section(char **summary_text, int *current_array_len,
                            int increment, const char *heading,
                            section_mark_t *markers, int *nmarks)
{
    assert(*nmarks < 64); // we hard-coded the length to 64, below

    int summary_strlen = strlen(*summary_text);
    int heading_strlen = strlen(heading);

    markers[*nmarks].section_start = summary_strlen;

    if (summary_strlen + heading_strlen + 2 >= *current_array_len) {
        *current_array_len += increment;
        char *newstr = MALLOC(sizeof(char)*(*current_array_len));
        strcpy(newstr, *summary_text);
        strcat(newstr, heading);
        free(*summary_text);
        *summary_text = newstr;
    } else {
        strcat(*summary_text, heading);
    }

    strcat(*summary_text, "\n");
    markers[*nmarks].section_end = summary_strlen + heading_strlen;
    markers[*nmarks].type = MarkerTypeHeading;
    ++(*nmarks);
}

static const char *noneify(const char *s)
{
    static const char *none = "<none>";
    return strlen(s)==0 ? none : s;
}

static const char *yesify(int b)
{
    static const char *yes = "Yes";
    static const char *no = "No";

    return b ? yes : no;
}

/* returns newly allocated memory that must be free'd */
static char *ninetynineify(double d)
{
    char *ret = MALLOC(sizeof(char)*32);
    if (double_equals_tol(d, -99, 0.0000001))
        strcpy(ret, "none");
    else
        sprintf(ret, "%f", d);
    return ret;
}

static int check_lats(double lat_begin, double lat_end)
{
    int lat_begin_is_blank = double_equals_tol(lat_begin, -99, .00001);
    if (!lat_begin_is_blank) {
        if (lat_begin < -90) return TRUE;
        if (lat_begin > 90) return TRUE;
    }

    int lat_end_is_blank = double_equals_tol(lat_end, -99, .00001);
    if (!lat_end_is_blank) {
        if (lat_end < -90) return TRUE;
        if (lat_end > 90) return TRUE;
    }

    if (!lat_begin_is_blank && !lat_end_is_blank && lat_begin >= lat_end)
        return TRUE;

    return FALSE;
}

void add_to_summary_text_lats(char **summary_text, int *current_len, int increment,
                              double lat_begin, double lat_end,
                              section_mark_t *marks, int *n_marks)
{
    int bad = check_lats(lat_begin, lat_end);

    if (double_equals_tol(lat_begin, -99, 0.000001) &&
        double_equals_tol(lat_end, -99, 0.000001))
    {
        // user has not specified a latitude top or botton lat constraint
        add_to_summary_text_bad(bad, marks, n_marks, summary_text, current_len,
            increment, "Latitude Range: None\n");
    } else if (
        !double_equals_tol(lat_begin, -99, 0.000001) &&
        !double_equals_tol(lat_end, -99, 0.000001))
    {
        // user has both constraints specified
        add_to_summary_text_bad(bad, marks, n_marks, summary_text, current_len,
            increment, "Latitude Range: (%f, %f)\n", lat_begin, lat_end);
    } else {
        char *begin = ninetynineify(lat_begin);
        char *end = ninetynineify(lat_end);
        add_to_summary_text_bad(bad, marks, n_marks, summary_text, current_len,
            increment, "Latitude Range: (%s, %s)\n", begin, end);
        free(begin);
        free(end);
    }
}

SIGNAL_CALLBACK
void update_summary()
{
    ait_params_t *params = get_settings_from_gui();

    // a couple convenience aliases
    dem_config *cfg = params->cfg;
    assert(cfg != NULL); // this one can't be NULL ...
    meta_projection *proj = params->proj; // ... though this one can

    // "increment" needs to be big enough for each line in the summary
    const int increment = 10240; 
    // these keep track of the locations (indexes into "summary text") where
    // we need to mark text for later font modifications
    section_mark_t marks[64];
    int n_marks = 0;
    int bad;

    char *summary_text = MALLOC(sizeof(char) * increment);
    int current_len = increment;

    // From "Configuration File" tab
    bad = strlen(params->name) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Configuration File: %s\n", noneify(params->name));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Short Config File: %s\n\n",
        yesify(cfg->general->short_config));

    // "Input" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "General", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Mode: %s\n", cfg->general->mode);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Reference DEM: %s\n", cfg->general->dem);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Base name: %s\n", cfg->general->base);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Data Type: %s\n", cfg->general->data_type);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Deskew: %s\n", yesify(cfg->general->deskew));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Doppler: %s\n", cfg->general->doppler);
    add_to_summary_text_lats(&summary_text, &current_len, increment,
        cfg->general->lat_begin, cfg->general->lat_end, marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Coregistration: %s\n", cfg->general->coreg);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Maximum Offset: %d\n", cfg->general->max_off);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Mask Flag: %s\n", yesify(cfg->general->mflag));
    //if (cfg->general->mflag)
    add_to_summary_text(&summary_text, &current_len, increment,
        "Mask: %s\n", cfg->general->mask);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Test Mode: %s\n", yesify(cfg->general->test));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->general->status);

    // More from the "Input" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Master Image", marks, &n_marks);
    bad = strlen(cfg->master->path) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Path: %s\n", noneify(cfg->master->path));
    bad = strlen(cfg->master->data) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len, 
        increment, "Data: %s\n", noneify(cfg->master->data));
    bad = strlen(cfg->master->meta) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Metadata: %s\n\n", noneify(cfg->master->meta));

    // More from the "Input" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Slave Image", marks, &n_marks);
    bad = strlen(cfg->slave->path) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Path: %s\n", noneify(cfg->slave->path));
    bad = strlen(cfg->slave->data) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Data: %s\n", noneify(cfg->slave->data));
    bad = strlen(cfg->slave->meta) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Metadata: %s\n\n", noneify(cfg->slave->meta));

    // "Ingest" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Ingest", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Precise Master: %s\n", cfg->ingest->prc_master);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Precise Slave: %s\n", cfg->ingest->prc_slave);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Precise Orbits: %s\n", yesify(cfg->ingest->prcflag));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->ingest->status);

    // "Coregister First" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Coregister First Patch", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Patches: %d\n", cfg->coreg_p1->patches);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Start Master: %ld\n", cfg->coreg_p1->start_master);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Start Slave: %ld\n", cfg->coreg_p1->start_slave);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Grid: %d\n", cfg->coreg_p1->grid);
    add_to_summary_text(&summary_text, &current_len, increment,
        "FFT: %s\n", yesify(cfg->coreg_p1->fft));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Sinc: %s\n", yesify(cfg->coreg_p1->sinc));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Warp: %s\n", yesify(cfg->coreg_p1->warp));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Offset Azimuth: %d\n", cfg->coreg_p1->off_az);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Offset Range: %d\n", cfg->coreg_p1->off_rng);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->coreg_p1->status);

    // "Coregister Last" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Coregister Last Patch", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Patches: %d\n", cfg->coreg_pL->patches);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Start Master: %ld\n", cfg->coreg_pL->start_master);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Start Slave: %ld\n", cfg->coreg_pL->start_slave);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Grid: %d\n", cfg->coreg_pL->grid);
    add_to_summary_text(&summary_text, &current_len, increment,
        "FFT: %s\n", yesify(cfg->coreg_pL->fft));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Sinc: %s\n", yesify(cfg->coreg_pL->sinc));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Warp: %s\n", yesify(cfg->coreg_pL->warp));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Offset Azimuth: %d\n", cfg->coreg_pL->off_az);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Offset Range: %d\n", cfg->coreg_pL->off_rng);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->coreg_pL->status);

    // "ArDOP - Master" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "ArDOP - Master Image", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Start Offset: %d\n", cfg->ardop_master->start_offset);
    add_to_summary_text(&summary_text, &current_len, increment,
        "End Offset: %d\n", cfg->ardop_master->end_offset);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Patches: %d\n", cfg->ardop_master->patches);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Power Flag: %s\n", yesify(cfg->ardop_master->power));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Power Image: %s\n", cfg->ardop_master->power_img);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->ardop_master->status);

    // "ArDOP - Slave" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "ArDOP - Slave Image", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Start Offset: %d\n", cfg->ardop_slave->start_offset);
    add_to_summary_text(&summary_text, &current_len, increment,
        "End Offset: %d\n", cfg->ardop_slave->end_offset);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Patches: %d\n", cfg->ardop_slave->patches);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Power Flag: %s\n", yesify(cfg->ardop_slave->power));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Power Image: %s\n", cfg->ardop_slave->power_img);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->ardop_slave->status);

    // "Interferogram" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Interferogram/Coherence", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Interferogram: %s\n", cfg->igram_coh->igram);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Coherence Image: %s\n", cfg->igram_coh->coh);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Minimum Coherence: %f\n", cfg->igram_coh->min);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Multilook: %s\n", yesify(cfg->igram_coh->ml));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->igram_coh->status);

    // "Offset Matching" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Offset Matching", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Maximum: %f\n", cfg->offset_match->max);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->offset_match->status);

    // "Simulated Phase" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Simulated Phase", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Seeds: %s\n", cfg->sim_phase->seeds);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->sim_phase->status);

    // "Deramp" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Deramp/Multilook", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->deramp_ml->status);

    // "Phase Unwrapping" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Phase Unwrapping", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Algorithm: %s\n", cfg->unwrap->algorithm);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Flattening: %s\n", yesify(cfg->unwrap->flattening));
    add_to_summary_text(&summary_text, &current_len, increment,
        "Processors: %d\n", cfg->unwrap->procs);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Tiles Azimuth: %d\n", cfg->unwrap->tiles_azimuth);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Tiles Range: %d\n", cfg->unwrap->tiles_range);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Tiles Per Degree: %d\n", cfg->unwrap->tiles_per_degree);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Overlap Azimuth: %d\n", cfg->unwrap->overlap_azimuth);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Overlap Range: %d\n", cfg->unwrap->overlap_range);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Filter: %f\n", cfg->unwrap->filter);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Quality Control: %s\n", cfg->unwrap->qc);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->unwrap->status);

    // "Refine Baseline" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Refine Baseline", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Iterations: %d\n", cfg->refine->iter);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Max Iterations: %d\n", cfg->refine->max);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->refine->status);

    // "Ground Range DEM" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Ground Range DEM", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->ground_range->status);

    // "Geocoding" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Geocoding", marks, &n_marks);
    if (proj)
    {
        add_to_summary_text(&summary_text, &current_len, increment,
            "Projection: %s\n", projection_type_str(proj->type));

        switch (proj->type) {
            case PROJ_UTM:
                if (proj->param.utm.zone != 0)
                    add_to_summary_text(&summary_text, &current_len, increment,
                        "Zone: %d\n", proj->param.utm.zone);
                else
                    add_to_summary_text(&summary_text, &current_len, increment,
                        "Zone: <from metadata>\n");
                break;

            case PROJ_PS:
                add_to_summary_text(&summary_text, &current_len, increment,
                    "Center: (%.2f, %.2f)\n", proj->param.ps.slat,
                    proj->param.ps.slon);
                break;

            case PROJ_LAMCC:
                add_to_summary_text(&summary_text, &current_len, increment,
                    "Center: (%.2f, %.2f)\n", proj->param.lamcc.lat0,
                    proj->param.lamcc.lon0);
                add_to_summary_text(&summary_text, &current_len, increment,
                    "Standard Parallels: (%.2f, %.2f)\n",
                    proj->param.lamcc.plat1,
                    proj->param.lamcc.plat2);
                break;

            case PROJ_LAMAZ:
                add_to_summary_text(&summary_text, &current_len, increment,
                    "Center: (%.2f, %.2f)\n", proj->param.lamaz.center_lat,
                    proj->param.lamaz.center_lon);
                break;

            case PROJ_ALBERS:
                add_to_summary_text(&summary_text, &current_len, increment,
                    "Center: (%.2f, %.2f)\n", proj->param.albers.orig_latitude,
                    proj->param.albers.center_meridian);
                add_to_summary_text(&summary_text, &current_len, increment,
                    "Standard Parallels: (%.2f, %.2f)\n",
                    proj->param.albers.std_parallel1,
                    proj->param.albers.std_parallel2);
                break;

            default:
                assert(FALSE);
                break;
        }
        add_to_summary_text(&summary_text, &current_len, increment,
            "Height: %f\n", proj->height);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Pixel Size: %f\n", proj->perX);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Datum: %s\n", datum_string(proj->datum));
        add_to_summary_text(&summary_text, &current_len, increment,
            "Resampling Method: %s\n", resample_method_string(params->resample_method));
        add_to_summary_text(&summary_text, &current_len, increment,
            "Force: %s\n", yesify(params->force));
        add_to_summary_text(&summary_text, &current_len, increment,
            "DEM: %s\n", cfg->geocode->dem);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Error Map: %s\n", cfg->geocode->error);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Amplitude: %s\n", cfg->geocode->amp);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Coherence: %s\n", cfg->geocode->coh);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Projection Name: %s\n", cfg->geocode->name);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Projection File: %s\n", cfg->geocode->proj);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Resampling Method: %s\n", cfg->geocode->resample);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Pixel Spacing: %f\n", cfg->geocode->pixel_spacing);
        add_to_summary_text(&summary_text, &current_len, increment,
            "Status: %s\n\n", cfg->geocode->status);
    }
    else
    {
        // no geocoding
        add_to_summary_text(&summary_text, &current_len, increment, "Off\n\n");
    }

    // "Export" tab
    add_to_summary_text_section(&summary_text, &current_len,
        increment, "Export", marks, &n_marks);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Format: %s\n", cfg->export->format);
    add_to_summary_text(&summary_text, &current_len, increment,
        "Status: %s\n\n", cfg->export->status);

    GtkWidget *summary_textview =
        glade_xml_get_widget(glade_xml, "summary_textview");

    GtkTextBuffer *tb =
        gtk_text_view_get_buffer(GTK_TEXT_VIEW(summary_textview));

    // save the current scroll position
    GtkTextIter iter;
    gtk_text_buffer_get_start_iter(tb, &iter);
    gtk_text_buffer_create_mark(tb, "tmp", &iter, TRUE);
    GtkTextMark *mark = gtk_text_buffer_get_mark(tb, "tmp");
    gtk_text_view_move_mark_onscreen(GTK_TEXT_VIEW(summary_textview), mark);
    gtk_text_buffer_get_iter_at_mark(tb, &iter, mark);
    int offset = gtk_text_iter_get_offset(&iter);
    //printf("Offset: %d\n", offset);

    gtk_text_buffer_set_text(tb, summary_text, -1);
    
    // scroll to saved position
    gtk_text_buffer_get_iter_at_offset(tb, &iter, offset);
    gtk_text_buffer_create_mark(tb, "tmp", &iter, TRUE);
    mark = gtk_text_buffer_get_mark(tb, "tmp");
    gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(summary_textview), mark, 0, 1, 0, 0);
    gtk_text_buffer_delete_mark(tb, mark);

    static GtkTextTag *bold_tag = NULL;
    if (!bold_tag) {
        bold_tag = gtk_text_buffer_create_tag(tb, "section",
            "weight", PANGO_WEIGHT_BOLD, 
            "foreground", "black",
            NULL);
    }

    static GtkTextTag *bad_tag = NULL;
    if (!bad_tag) {
        bad_tag = gtk_text_buffer_create_tag(tb, "baddata",
            //"weight", PANGO_WEIGHT_BOLD, 
            "foreground", "red",
            NULL);
    }

    // Apply the font tags
    int i;
    for (i=0; i<n_marks; ++i) {
        GtkTextIter begin_iter, end_iter;
        gtk_text_buffer_get_iter_at_offset(tb, &begin_iter, marks[i].section_start);
        gtk_text_buffer_get_iter_at_offset(tb, &end_iter, marks[i].section_end);

        if (marks[i].type == MarkerTypeHeading)
            gtk_text_buffer_apply_tag(tb, bold_tag, &begin_iter, &end_iter);
        else if (marks[i].type == MarkerTypeBadData)
            gtk_text_buffer_apply_tag(tb, bad_tag, &begin_iter, &end_iter);
    }

    free_ait_params(params);
}

void apply_settings_to_gui(ait_params_t *params)
{
    dem_config *cfg = params->cfg;
    assert(cfg != NULL);
    meta_projection *proj = params->proj;

    set_combo_box_item("mode_optionmenu", cfg->general->mode);
    put_str(params->name, "configuration_file_entry");
    put_str(cfg->general->dem, "reference_dem_entry");
    put_chk(cfg->general->deskew, "deskew_checkbutton");
    set_combo_box_item("data_type_optionmenu", cfg->general->data_type);
    put_dbl_blank(cfg->general->lat_begin, "lat_begin_entry", -99);
    put_dbl_blank(cfg->general->lat_end, "lat_end_entry", -99);
    put_int(cfg->general->max_off, "maximum_offset_entry");
    put_chk(cfg->general->short_config, "short_configuration_file_checkbutton");
    put_str(cfg->ingest->prc_master, "ingest_precise_master_entry");
    put_str(cfg->ingest->prc_slave, "ingest_precise_slave_entry");
    put_chk(cfg->ingest->prcflag, "ingest_precise_orbits_checkbutton");
    put_str(cfg->master->path, "master_image_path_entry");
    put_str(cfg->master->data, "master_image_data_entry");
    put_str(cfg->master->meta, "master_image_metadata_entry");
    put_str(cfg->slave->path, "slave_image_path_entry");
    put_str(cfg->slave->data, "slave_image_data_entry");
    put_str(cfg->slave->meta, "slave_image_metadata_entry");
    put_int(cfg->coreg_p1->patches, "coregister_first_patches_entry");
    put_lng(cfg->coreg_p1->start_master, "coregister_first_start_master_entry");
    put_lng(cfg->coreg_p1->start_slave, "coregister_first_start_slave_entry");
    put_int(cfg->coreg_p1->grid, "coregister_first_grid_entry");
    put_chk(cfg->coreg_p1->fft, "coregister_first_fft_checkbutton");
    put_int(cfg->coreg_p1->off_az, "coregister_first_offset_azimuth_entry");
    put_int(cfg->coreg_p1->off_rng, "coregister_first_offset_range_entry");
    put_int(cfg->coreg_pL->patches, "coregister_last_patches_entry");
    put_lng(cfg->coreg_pL->start_master, "coregister_last_start_master_entry");
    put_lng(cfg->coreg_pL->start_slave, "coregister_last_start_slave_entry");
    put_int(cfg->coreg_pL->grid, "coregister_last_grid_entry");
    put_chk(cfg->coreg_pL->fft, "coregister_last_fft_checkbutton");
    put_int(cfg->coreg_pL->off_az, "coregister_last_offset_azimuth_entry");
    put_int(cfg->coreg_pL->off_rng, "coregister_last_offset_range_entry");
    put_lng(cfg->ardop_master->start_offset, "ardop_master_start_offset_entry");
    put_lng(cfg->ardop_master->end_offset, "ardop_master_end_offset_entry");
    put_int(cfg->ardop_master->patches, "ardop_master_patches_entry");
    put_chk(cfg->ardop_master->power, "ardop_master_power_flag_checkbutton");
    put_lng(cfg->ardop_slave->start_offset, "ardop_slave_start_offset_entry");
    put_lng(cfg->ardop_slave->end_offset, "ardop_slave_end_offset_entry");
    put_int(cfg->ardop_slave->patches, "ardop_slave_patches_entry");
    put_chk(cfg->ardop_slave->power, "ardop_slave_power_flag_checkbutton");
    put_dbl(cfg->igram_coh->min, "interferogram_min_coherence_entry");
    put_chk(cfg->igram_coh->ml, "interferogram_multilook_checkbutton");
    put_dbl(cfg->offset_match->max, "offset_matching_max_entry");
    set_combo_box_item("phase_unwrapping_optionmenu", cfg->unwrap->algorithm);
    put_chk(cfg->unwrap->flattening, "phase_unwrapping_flattening_checkbutton");
    put_int(cfg->unwrap->procs, "phase_unwrapping_processors_entry");
    put_int(cfg->unwrap->tiles_azimuth, "phase_unwrapping_tiles_azimuth_entry");
    put_int(cfg->unwrap->tiles_range, "phase_unwrapping_tiles_range_entry");
    put_int(cfg->unwrap->overlap_azimuth, "phase_unwrapping_overlap_azimuth_entry");
    put_int(cfg->unwrap->overlap_range, "phase_unwrapping_overlap_range_entry");
    put_dbl(cfg->unwrap->filter, "phase_unwrapping_filter_entry");
    put_int(cfg->unwrap->tiles_per_degree, "phase_unwrapping_tiles_per_degree_entry");
    put_int(cfg->refine->iter, "baseline_refinement_iterations_entry");
    put_int(cfg->refine->max, "baseline_refinement_max_iterations_entry");
    put_chk(params->proj != NULL, "geocode_checkbutton");

    if (params->proj != NULL)
    {
        switch (proj->type) {
            case PROJ_UTM:
                put_int(proj->param.utm.zone, "zone_entry");
                break;

            case PROJ_PS:
                put_dbl(proj->param.ps.slat, "latitude_of_origin_entry");
                put_dbl(proj->param.ps.slon, "center_meridian_entry");
                break;

            case PROJ_LAMCC:
                put_dbl(proj->param.lamcc.lat0, "latitude_of_origin_entry");
                put_dbl(proj->param.lamcc.lon0, "center_meridian_entry");
                put_dbl(proj->param.lamcc.plat1, "first_standard_parallel_entry");
                put_dbl(proj->param.lamcc.plat2, "second_standard_parallel_entry");
                break;

            case PROJ_LAMAZ:
                put_dbl(proj->param.lamaz.center_lat, "latitude_of_origin_entry");
                put_dbl(proj->param.lamaz.center_lon, "center_meridian_entry");
                break;

            case PROJ_ALBERS:
                put_dbl(proj->param.albers.orig_latitude, "latitude_of_origin_entry");
                put_dbl(proj->param.albers.center_meridian, "center_meridian_entry");
                put_dbl(proj->param.albers.std_parallel1, "first_standard_parallel_entry");
                put_dbl(proj->param.albers.std_parallel2, "second_standard_parallel_entry");
                break;

            default:
                assert(FALSE);
                break;
        }

        if (proj->height != 0.0 && !ISNAN(proj->height)) {
            put_chk(TRUE, "height_checkbutton");
            put_dbl(proj->height, "height_entry");
        } else {
            put_chk(FALSE, "height_checkbutton");
            put_str("", "height_entry");
        }

        if (proj->perX != 0.0 && !ISNAN(proj->perX)) {
            put_chk(TRUE, "pixel_size_checkbutton");
            put_dbl(proj->perX, "pixel_size_entry");
        } else {
            put_chk(FALSE, "pixel_size_checkbutton");
            put_str("", "pixel_size_entry");
        }

        switch (proj->datum) {
            default:
            case WGS84_DATUM:
                set_combo_box_item("datum_optionmenu", "WGS84");
                break;

            case NAD27_DATUM:
                set_combo_box_item("datum_optionmenu", "NAD27");
                break;

            case NAD83_DATUM:
                set_combo_box_item("datum_optionmenu", "NAD83");
                break;
        }

        switch (params->resample_method) {
            case RESAMPLE_NEAREST_NEIGHBOR: 
                set_combo_box_item("resample_optionmenu", "Nearest Neighbor");
                break;

            default:
            case RESAMPLE_BILINEAR: 
                set_combo_box_item("resample_optionmenu", "Bilinear");
                break;

            case RESAMPLE_BICUBIC: 
                set_combo_box_item("resample_optionmenu", "Bicubic");
                break;
        }

        put_chk(params->force, "force_checkbutton");
    }

    // Imagery List
    clear_image_list();

    add_section_to_image_list("Input Files");
    add_to_image_list(cfg->general->dem);
    add_to_image_list2(cfg->master->path, cfg->master->data);
    add_to_image_list2(cfg->slave->path, cfg->slave->data);

    char *path = MALLOC(sizeof(char) * (strlen(params->name) + 64));
    char *tmp = MALLOC(sizeof(char) * (strlen(params->name) + 64));
    split_dir_and_file(params->name, path, tmp);
    if (strlen(path) == 0) strcpy(path, ".");

    add_section_to_image_list("Ingest");
    add_to_image_list2(path, "a_amp.img");
    add_to_image_list2(path, "b_amp.img");

    add_section_to_image_list("Coherence");
    sprintf(tmp, "%s_coh.img", cfg->general->base);
    add_to_image_list2(path, tmp);

    add_section_to_image_list("Interferograms");
    sprintf(tmp, "%s_igram_ml_amp.img", cfg->general->base);
    add_to_image_list2(path, tmp);

    sprintf(tmp, "%s_igram_ml_amp_byte.img", cfg->general->base);
    add_to_image_list2(path, tmp);

    add_section_to_image_list("Offset Matching");
    add_to_image_list2(path, "dem_slant.img");
    add_to_image_list2(path, "dem_sim.img");
    add_to_image_list2(path, "dem_sim_byte.img");
    add_to_image_list2(path, "out_dem_phase.img");

    add_section_to_image_list("Phase Unwrapping");
    add_to_image_list2(path, "filtered_phase.img");
    add_to_image_list2(path, "escher_in_phase.img");
    add_to_image_list2(path, "unwrap_dem.img");

    sprintf(tmp, "%s_igram_ml_rgb.img", cfg->general->base);
    add_to_image_list2(path, tmp);

    add_to_image_list2(path, "unwrap_phase.img");

    add_section_to_image_list("Elevation");
    sprintf(tmp, "%s_ht.img", cfg->general->base);
    add_to_image_list2(path, tmp);

    sprintf(tmp, "%s_err_ht.img", cfg->general->base);
    add_to_image_list2(path, tmp);

    add_section_to_image_list("Export");
    add_to_image_list2(path, "elevation.img");
    add_to_image_list2(path, "amplitude.img");
    add_to_image_list2(path, "error.img");
    
    free(tmp);
    free(path);
}

void write_settings(ait_params_t *params)
{
    // convenience aliases
    dem_config *cfg = params->cfg;
    assert(cfg != NULL);

    meta_projection *proj = params->proj;
    int geocode_on = params->proj != NULL;

    // write the projection file
    if (geocode_on)
    {
        char *proj_name = appendExt(params->name, ".proj");
        strcpy(cfg->geocode->proj, proj_name);

        FILE *pf = fopen(proj_name, "w");
        if (!pf) {
            message_box("Error opening projection file: %s\n", proj_name);
            return;
        }

        switch (proj->type)
        {
        case PROJ_UTM:
            {
                proj_utm *s = &proj->param.utm;
                fprintf(pf, "[Universal Transverse Mercator]\n");
                fprintf(pf, "Zone=%d\n", s->zone != 0 ? s->zone : 0);
            }
            break;

        case PROJ_PS:
            {
                proj_ps *s = &proj->param.ps;
                fprintf(pf, "[Polar Stereographic]\n");
                fprintf(pf, "First Standard Parallel=%.10f\n", s->slat);
                fprintf(pf, "Central Meridian=%.10f\n", s->slon);
                fprintf(pf, "Northern Projection=%d\n", s->is_north_pole ? 1 : 0);
            }
            break;

        case PROJ_ALBERS:
            {
                proj_albers *s = &proj->param.albers;
                fprintf(pf, "[Albers Conical Equal Area]\n");
                fprintf(pf, "First standard parallel=%.10f\n", s->std_parallel1);
                fprintf(pf, "Second standard parallel=%.10f\n", s->std_parallel2);
                fprintf(pf, "Central Meridian=%.10f\n", s->center_meridian);
                fprintf(pf, "Latitude of Origin=%.10f\n", s->orig_latitude);
            }
            break;

        case PROJ_LAMAZ:
            {
                proj_lamaz *s = &proj->param.lamaz;
                fprintf(pf, "[Lambert Azimuthal Equal Area]\n");
                fprintf(pf, "Central Meridian=%.10f\n", s->center_lon);
                fprintf(pf, "Latitude of Origin=%.10f\n", s->center_lat);
            }
            break;

        case PROJ_LAMCC:
            {
                proj_lamcc *s = &proj->param.lamcc;
                fprintf(pf, "[Lambert Conformal Conic]\n");
                fprintf(pf, "First standard parallel=%.10f\n", s->plat1);
                fprintf(pf, "Second standard parallel=%.10f\n", s->plat2);
                fprintf(pf, "Central Meridian=%.10f\n", s->lon0);
                fprintf(pf, "Latitude of Origin=%.10f\n", s->lat0);
            }
            break;

        default:
            assert(FALSE);
            break;
        }

        fclose(pf);
        free(proj_name);
    }
    else
    {
        // no geocoding
        strcpy(cfg->geocode->proj, "");
    }

    // write the IPS configuration file
    write_config(params->name, params->cfg);
}

static ait_params_t *ait_params_new(dem_config *cfg, char *config_file)
{
    ait_params_t *params = MALLOC(sizeof(ait_params_t));

    params->proj = NULL;
    params->resample_method = RESAMPLE_BILINEAR;
    params->force = FALSE;

    params->cfg = cfg;
    params->name = MALLOC(sizeof(char)*(strlen(config_file)+2));
    strcpy(params->name, config_file);

    return params;
}

ait_params_t *read_settings(char *config_file)
{
    dem_config *cfg = read_config(config_file, FALSE);
    if (!cfg) return NULL; // failed to read the file

    ait_params_t *params = ait_params_new(cfg, config_file);
    if (strlen(cfg->geocode->proj) > 0)
    {
        params->proj = read_proj_file(cfg->geocode->proj, params);
        params->proj->perX = cfg->geocode ? cfg->geocode->pixel_spacing : 0;
    }
    else
    {
        // no geocoding
        params->proj = NULL;
    }

    return params;
}

static int diff_str(const char *field_name, const char *s1, const char *s2)
{
    int ok = strcmp(s1,s2)==0;
    if (!ok)
        printf("%s: %s != %s\n", field_name, s1, s2);

    return ok;
}

static int diff_dbl(const char *field_name, double d1, double d2)
{
    int ok = fabs(d1-d2)<.001;
    if (!ok)
        printf("%s: %.2f != %.2f\n", field_name, d1, d2);

    return ok;
}

static int diff_int(const char *field_name, int i1, int i2)
{
    int ok = i1==i2;
    if (!ok)
        printf("%s: %d != %d\n", field_name, i1, i2);

    return ok;
}

static int diff_lng(const char *field_name, long l1, long l2)
{
    int ok = l1==l2;
    if (!ok)
        printf("%s: %ld != %ld\n", field_name, l1, l2);

    return ok;
}

int params_diff(ait_params_t *p1, ait_params_t *p2)
{
    int err = 0;

    dem_config *c1 = p1->cfg;
    dem_config *c2 = p2->cfg;

#define DSTR(x) err += diff_str(#x, c1->x, c2->x)
#define DDBL(x) err += diff_dbl(#x, c1->x, c2->x)
#define DINT(x) err += diff_int(#x, c1->x, c2->x)
#define DLNG(x) err += diff_lng(#x, c1->x, c2->x)

    DSTR(general->mode);
    DSTR(general->dem);
    DSTR(general->base);
    DSTR(general->data_type);
    DINT(general->deskew);
    DSTR(general->doppler);
    DDBL(general->lat_begin);
    DDBL(general->lat_end);
    DSTR(general->coreg);
    DINT(general->max_off);
    DINT(general->mflag);
    DSTR(general->mask);
    DSTR(general->def_val);
    DINT(general->test);
    DINT(general->short_config);
    DSTR(general->status);

    DSTR(master->path);
    DSTR(master->data);
    DSTR(master->meta);

    DSTR(slave->path);
    DSTR(slave->data);
    DSTR(slave->meta);

    DSTR(ingest->prc_master);
    DSTR(ingest->prc_slave);
    DINT(ingest->prcflag);
    DSTR(ingest->status);

    DSTR(doppler->status);
    DSTR(doppler_per_patch->status);

    DINT(coreg_p1->patches);
    DLNG(coreg_p1->start_master);
    DLNG(coreg_p1->start_slave);
    DINT(coreg_p1->grid);
    DINT(coreg_p1->fft);
    DINT(coreg_p1->sinc);
    DINT(coreg_p1->warp);
    DINT(coreg_p1->off_az);
    DINT(coreg_p1->off_rng);
    DSTR(coreg_p1->status);

    DINT(coreg_pL->patches);
    DLNG(coreg_pL->start_master);
    DLNG(coreg_pL->start_slave);
    DINT(coreg_pL->grid);
    DINT(coreg_pL->fft);
    DINT(coreg_pL->sinc);
    DINT(coreg_pL->warp);
    DINT(coreg_pL->off_az);
    DINT(coreg_pL->off_rng);
    DSTR(coreg_pL->status);

    DLNG(ardop_master->start_offset);
    DLNG(ardop_master->end_offset);
    DINT(ardop_master->patches);
    DINT(ardop_master->power);
    DSTR(ardop_master->power_img);
    DSTR(ardop_master->status);

    DLNG(ardop_slave->start_offset);
    DLNG(ardop_slave->end_offset);
    DINT(ardop_slave->patches);
    DINT(ardop_slave->power);
    DSTR(ardop_slave->power_img);
    DSTR(ardop_slave->status);

    DSTR(cpx_autofilter->status);

    DINT(coreg_slave->patches);
    DLNG(coreg_slave->start_master);
    DLNG(coreg_slave->start_slave);
    DINT(coreg_slave->grid);
    DINT(coreg_slave->fft);
    DINT(coreg_slave->sinc);
    DINT(coreg_slave->warp);
    DINT(coreg_slave->off_az);
    DINT(coreg_slave->off_rng);
    DSTR(coreg_slave->status);

    DSTR(igram_coh->igram);
    DSTR(igram_coh->coh);
    DDBL(igram_coh->min);
    DINT(igram_coh->ml);
    DSTR(igram_coh->status);

    DDBL(offset_match->max);
    DSTR(offset_match->status);

    DSTR(sim_phase->seeds);
    DSTR(sim_phase->status);

    DSTR(dinsar->igram);
    DSTR(dinsar->status);

    DSTR(deramp_ml->status);

    DSTR(unwrap->algorithm);
    DINT(unwrap->flattening);
    DINT(unwrap->procs);
    DINT(unwrap->tiles_azimuth);
    DINT(unwrap->tiles_range);
    DINT(unwrap->tiles_per_degree);
    DINT(unwrap->overlap_azimuth);
    DINT(unwrap->overlap_range);
    DDBL(unwrap->filter);
    DSTR(unwrap->qc);
    DSTR(unwrap->status);

    DINT(refine->iter);
    DINT(refine->max);
    DSTR(refine->status);

    DSTR(elevation->dem);
    DSTR(elevation->error);
    DSTR(elevation->status);

    DSTR(ground_range->status);

    DSTR(geocode->dem);
    DSTR(geocode->amp);
    DSTR(geocode->error);
    DSTR(geocode->coh);
    DSTR(geocode->name);
    //DSTR(geocode->proj);
    DSTR(geocode->resample);
    DDBL(geocode->pixel_spacing);
    DSTR(geocode->status);

    DSTR(export->status);

#undef DSTR
#undef DDBL
#undef DINT
#undef DLNG

#define DSTR(x) err += diff_str(#x, p1->x, p2->x)
#define DDBL(x) err += diff_dbl(#x, p1->x, p2->x)
#define DINT(x) err += diff_int(#x, p1->x, p2->x)
#define DLNG(x) err += diff_lng(#x, p1->x, p2->x)

    DSTR(name);
    DINT(resample_method);
    DINT(force);

#undef DSTR
#undef DDBL
#undef DINT
#undef DLNG

    meta_projection *proj1 = p1->proj;
    meta_projection *proj2 = p2->proj;

#define DSTR(x) err += diff_str(#x, proj1->x, proj2->x)
#define DDBL(x) err += diff_dbl(#x, proj1->x, proj2->x)
#define DINT(x) err += diff_int(#x, proj1->x, proj2->x)
#define DLNG(x) err += diff_lng(#x, proj1->x, proj2->x)

    DINT(type);
    DDBL(perX);
    DDBL(perY);
    DINT(datum);
    DDBL(height);

    switch (proj1->type) {
        case UNIVERSAL_TRANSVERSE_MERCATOR:
            DINT(param.utm.zone);
            break;
        case POLAR_STEREOGRAPHIC:
            DDBL(param.ps.slat);
            DDBL(param.ps.slon);
            DINT(param.ps.is_north_pole);
            break;
        case ALBERS_EQUAL_AREA:
            DDBL(param.albers.std_parallel1);
            DDBL(param.albers.std_parallel2);
            DDBL(param.albers.center_meridian);
            DDBL(param.albers.orig_latitude);
            break;
        case LAMBERT_CONFORMAL_CONIC:
            DDBL(param.lamcc.plat1);
            DDBL(param.lamcc.plat2);
            DDBL(param.lamcc.lat0);
            DDBL(param.lamcc.lon0);
            break;
        case LAMBERT_AZIMUTHAL_EQUAL_AREA:
            DDBL(param.lamaz.center_lon);
            DDBL(param.lamaz.center_lat);
            break;
        default:
            printf("Invalid projection type: %d\n", proj1->type);
            ++err;
            break;
    }

#undef DSTR
#undef DDBL
#undef DINT
#undef DLNG
    
    return err > 0;
}

/* And new a whole slew of signal handlers... */

SIGNAL_CALLBACK void on_configuration_file_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_short_configuration_file_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_master_image_path_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_master_image_data_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_master_image_metadata_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_slave_image_path_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_slave_image_data_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_slave_image_metadata_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_reference_dem_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_lat_begin_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_lat_end_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_maximum_offset_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_deskew_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ingest_precise_master_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ingest_precise_slave_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ingest_precise_orbits_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_patches_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_start_master_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_start_slave_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_grid_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_fft_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_offset_azimuth_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_first_offset_range_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_patches_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_start_master_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_start_slave_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_grid_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_fft_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_offset_azimuth_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_coregister_last_offset_range_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_master_start_offset_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_master_end_offset_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_master_patches_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_master_power_flag_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_slave_start_offset_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_slave_end_offset_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_slave_patches_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ardop_slave_power_flag_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_interferogram_min_coherence_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_interferogram_multilook_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_offset_matching_max_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_flattening_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_processors_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_tiles_azimuth_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_tiles_range_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_tiles_per_degree_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_overlap_azimuth_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_overlap_range_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_phase_unwrapping_filter_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_baseline_refinement_iterations_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_baseline_refinement_max_iterations_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_mode_dem_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_mode_dinsar_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_data_type_stf_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_data_type_raw_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_data_type_slc_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_algorithm_escher_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_algorithm_snaphu_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_resample_optionmenu_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_central_meridian_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_pixel_size_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_first_standard_parallel_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_second_standard_parallel_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_datum_optionmenu_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_ppm_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_jpg_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_projection_optionmenu_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_force_checkbutton(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_latitude_of_origin_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_zone_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_height_entry_changed(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_geotiff_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_tiff_activate(GtkWidget *w)
{
    update_everything();
}

SIGNAL_CALLBACK void on_force_checkbutton_toggled(GtkWidget *w)
{
    update_everything();
}

