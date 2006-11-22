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
    get_dbl(&cfg->general->lat_begin, "lat_begin_entry");
    get_dbl(&cfg->general->lat_end, "lat_end_entry");
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

void apply_settings_to_gui(dem_config *cfg)
{
    put_str(cfg->general->dem, "reference_dem_entry");
    put_chk(cfg->general->deskew, "deskew_checkbutton");
    put_dbl(cfg->general->lat_begin, "lat_begin_entry");
    put_dbl(cfg->general->lat_end, "lat_end_entry");
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
}

void add_to_summary_text(char **summary_text, int *current_len, int increment,
                         const char *format, ...)
{
    char buf[1024];
    int len;

    va_list ap;
    va_start(ap, format);
    len = vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);

    if (len > 1022)
        asfPrintWarning("Lengthy message may have been truncated!\n");

    if (strlen(*summary_text) + len >= *current_len) {
        *current_len += increment;
        char *new = MALLOC(sizeof(char)*(*current_len));
        strcpy(new, *summary_text);
        strcat(new, buf);
        *summary_text = new;
    } else {
        strcat(*summary_text, buf);
    }

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

SIGNAL_CALLBACK
void update_summary()
{
    char cfg_name[255];
    dem_config *cfg = get_settings_from_gui(cfg_name);
    int increment = 10240;

    char *summary_text = MALLOC(sizeof(char) * increment);
    int current_len = increment;

    add_to_summary_text(&summary_text, &current_len, increment,
                      "Configuration File: %s\n", noneify(cfg_name));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Short Config File: %s\n\n",
                        yesify(cfg->general->short_config));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Master Image Path: %s\n", noneify(cfg->master->path));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Master Image Data: %s\n", noneify(cfg->master->data));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Master Image Meta: %s\n", noneify(cfg->master->meta));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Slave Image Path: %s\n", noneify(cfg->slave->path));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Slave Image Data: %s\n", noneify(cfg->slave->data));
    add_to_summary_text(&summary_text, &current_len, increment,
                      "Slave Image Meta: %s\n", noneify(cfg->slave->meta));

    GtkWidget *summary_textview =
        glade_xml_get_widget(glade_xml, "summary_textview");

    GtkTextBuffer *tb =
        gtk_text_view_get_buffer(GTK_TEXT_VIEW(summary_textview));

    gtk_text_buffer_set_text(tb, summary_text, -1);
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

