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

static GtkWidget *get_widget_checked(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_*_from_entry() failed: "
            "The widget %s was not found.\n", widget_name);
    }
    return w;
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

dem_config *get_settings_from_gui(char *cfg_name)
{
    get_str(cfg_name, "configuration_file_entry");

    dem_config *cfg = create_config_with_defaults();

    //cfg->general->mode = new_blank_str();
    get_str(cfg->general->dem, "reference_dem_entry");
    //cfg->general->def_val = new_blank_str();
    //cfg->general->base = new_blank_str();
    //cfg->general->data_type = new_blank_str();
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

    //cfg->master->path = new_blank_str();
    //cfg->master->data = new_blank_str();
    //cfg->master->meta = new_blank_str();

    //cfg->slave->path = new_blank_str();
    //cfg->slave->data = new_blank_str();
    //cfg->slave->meta = new_blank_str();

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

    //cfg->unwrap->algorithm = new_str("escher");
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
    return cfg;
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
    char cfg_name[255];
    dem_config *cfg = get_settings_from_gui(cfg_name);
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
    bad = strlen(cfg_name) == 0;
    add_to_summary_text_bad(bad, marks, &n_marks, &summary_text, &current_len,
        increment, "Configuration File: %s\n", noneify(cfg_name));
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

    gtk_text_buffer_set_text(tb, summary_text, -1);

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
}

void apply_settings_to_gui(dem_config *cfg, const char *cfg_name)
{
    put_str(cfg_name, "configuration_file_entry");
    put_str(cfg->general->dem, "reference_dem_entry");
    put_chk(cfg->general->deskew, "deskew_checkbutton");
    put_dbl_blank(cfg->general->lat_begin, "lat_begin_entry", -99);
    put_dbl_blank(cfg->general->lat_end, "lat_end_entry", -99);
    put_int(cfg->general->max_off, "maximum_offset_entry");
    put_chk(cfg->general->short_config, "short_configuration_file_checkbutton");
    put_str(cfg->ingest->prc_master, "ingest_precise_master_entry");
    put_str(cfg->ingest->prc_slave, "ingest_precise_slave_entry");
    put_chk(cfg->ingest->prcflag, "ingest_precise_orbits_checkbutton");
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
    update_summary();
}

/* And new a whole slew of signal handlers... */

SIGNAL_CALLBACK void on_configuration_file_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_short_configuration_file_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_master_image_path_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_master_image_data_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_master_image_metadata_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_slave_image_path_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_slave_image_data_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_slave_image_metadata_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_reference_dem_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_lat_begin_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_lat_end_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_maximum_offset_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_deskew_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ingest_precise_master_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ingest_precise_slave_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ingest_precise_orbits_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_patches_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_start_master_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_start_slave_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_grid_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_fft_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_offset_azimuth_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_first_offset_range_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_patches_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_start_master_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_start_slave_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_grid_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_fft_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_offset_azimuth_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_coregister_last_offset_range_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_master_start_offset_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_master_end_offset_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_master_patches_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_master_power_flag_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_slave_start_offset_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_slave_end_offset_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_slave_patches_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_ardop_slave_power_flag_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_interferogram_min_coherence_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_interferogram_multilook_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_offset_matching_max_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_flattening_checkbutton_toggled(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_processors_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_tiles_azimuth_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_tiles_range_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_tiles_per_degree_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_overlap_azimuth_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_overlap_range_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_phase_unwrapping_filter_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_baseline_refinement_iterations_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_baseline_refinement_max_iterations_entry_changed(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_mode_dem_activate(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_mode_dinsar_activate(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_data_type_stf_activate(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_data_type_raw_activate(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_data_type_slc_activate(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_algorithm_escher_activate(GtkWidget *w)
{
    update_summary();
}

SIGNAL_CALLBACK void on_algorithm_snaphu_activate(GtkWidget *w)
{
    update_summary();
}

