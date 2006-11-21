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

static double get_double_from_entry(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_double_from_entry() failed: "
            "The widget %s was not found.\n", widget_name);
    }

    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));

    double val = atof(str);
    return val;
}

static int get_int_from_entry(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_int_from_entry() failed: "
            "The widget %s was not found.\n", widget_name);
    }

    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));

    int val = atoi(str);
    return val;
}

static long get_long_from_entry(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_long_from_entry() failed: "
            "The widget %s was not found.\n", widget_name);
    }

    const gchar * str = gtk_entry_get_text(GTK_ENTRY(w));

    long val = atol(str);
    return val;
}

static int get_bool_from_checkbutton(const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_bool_from_entry() failed: "
            "The widget %s was not found.\n", widget_name);
    }

    return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
}

static void get_string_from_entry(char *dest, const char *widget_name)
{
    GtkWidget *w = glade_xml_get_widget(glade_xml, widget_name);
    if (!w)
    {
        asfPrintError("get_string_from_entry() failed: "
            "The widget %s was not found.\n", widget_name);
    }

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

static void STR(char *dest, char *widget)
{
    get_string_from_entry(dest, widget);
}

static void DBL(double *val, char *widget)
{
    *val = get_double_from_entry(widget);
}

static void CHK(int *val, char *widget)
{
    *val = get_bool_from_checkbutton(widget);
}

static void NUM(int *val, char *widget)
{
    *val = get_int_from_entry(widget);
}

static void LNG(long *val, char *widget)
{
    *val = get_long_from_entry(widget);
}

dem_config *get_settings_from_gui()
{
    dem_config *cfg = create_config_with_defaults();

    //cfg->general->mode = new_blank_str();
    STR(cfg->general->dem, "reference_dem_entry");
    //cfg->general->def_val = new_blank_str();
    //cfg->general->base = new_blank_str();
    //cfg->general->data_type = new_blank_str();
    CHK(&cfg->general->deskew, "deskew_checkbutton");
    //cfg->general->doppler = new_blank_str();
    DBL(&cfg->general->lat_begin, "lat_begin_entry");
    DBL(&cfg->general->lat_end, "lat_end_entry");
    //cfg->general->coreg = new_blank_str();
    NUM(&cfg->general->max_off, "maximum_offset_entry");
    //cfg->general->mflag = 0;
    //cfg->general->mask = new_blank_str();
    //cfg->general->test = 0;
    CHK(&cfg->general->short_config, "short_configuration_file_checkbutton");
  
    //cfg->master->path = new_blank_str();
    //cfg->master->data = new_blank_str();
    //cfg->master->meta = new_blank_str();
  
    //cfg->slave->path = new_blank_str();
    //cfg->slave->data = new_blank_str();
    //cfg->slave->meta = new_blank_str();
  
    STR(cfg->ingest->prc_master, "ingest_precise_master_entry");
    STR(cfg->ingest->prc_slave, "ingest_precise_slave_entry");
    CHK(&cfg->ingest->prcflag, "ingest_precise_orbits_checkbutton");
    //cfg->ingest->status = new_str("new");
  
    //cfg->doppler->status = new_str("new");
  
    NUM(&cfg->coreg_p1->patches, "coregister_first_patches_entry");
    LNG(&cfg->coreg_p1->start_master, "coregister_first_start_master_entry");
    LNG(&cfg->coreg_p1->start_slave, "coregister_first_start_slave_entry");
    NUM(&cfg->coreg_p1->grid, "coregister_first_grid_entry");
    CHK(&cfg->coreg_p1->fft, "coregister_first_fft_checkbutton");
    NUM(&cfg->coreg_p1->off_az, "coregister_first_offset_azimuth_entry");
    NUM(&cfg->coreg_p1->off_rng, "coregister_first_offset_range_entry");
    //cfg->coreg_p1->status = new_str("new");
  
    NUM(&cfg->coreg_pL->patches, "coregister_last_patches_entry");
    LNG(&cfg->coreg_pL->start_master, "coregister_last_start_master_entry");
    LNG(&cfg->coreg_pL->start_slave, "coregister_last_start_slave_entry");
    NUM(&cfg->coreg_pL->grid, "coregister_last_grid_entry");
    CHK(&cfg->coreg_pL->fft, "coregister_last_fft_checkbutton");
    NUM(&cfg->coreg_pL->off_az, "coregister_last_offset_azimuth_entry");
    NUM(&cfg->coreg_pL->off_rng, "coregister_last_offset_range_entry");
    //cfg->coreg_pL->status = new_str("new");
  
    //cfg->doppler_per_patch->status = new_str("new");
  
    LNG(&cfg->ardop_master->start_offset, "ardop_master_start_offset_entry");
    LNG(&cfg->ardop_master->end_offset, "ardop_master_end_offset_entry");
    NUM(&cfg->ardop_master->patches, "ardop_master_patches_entry");
    CHK(&cfg->ardop_master->power, "ardop_master_power_flag_checkbutton");
    //cfg->ardop_master->power_img = new_blank_str();
    //cfg->ardop_master->status = new_str("new");
  
    LNG(&cfg->ardop_slave->start_offset, "ardop_slave_start_offset_entry");
    LNG(&cfg->ardop_slave->end_offset, "ardop_slave_end_offset_entry");
    NUM(&cfg->ardop_slave->patches, "ardop_slave_patches_entry");
    CHK(&cfg->ardop_slave->power, "ardop_slave_power_flag_checkbutton");
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
    DBL(&cfg->igram_coh->min, "interferogram_min_coherence_entry");
    CHK(&cfg->igram_coh->ml, "interferogram_multilook_checkbutton");
    //cfg->igram_coh->status = new_str("new");
  
    DBL(&cfg->offset_match->max, "offset_matching_max_entry");
    //cfg->offset_match->status = new_str("new");
  
    //cfg->sim_phase->seeds = new_blank_str();
    //cfg->sim_phase->status = new_str("new");
  
    //cfg->dinsar->igram = new_blank_str();
    //cfg->dinsar->status = new_str("new");
  
    //cfg->deramp_ml->status = new_str("new");
  
    //cfg->unwrap->algorithm = new_str("escher");
    CHK(&cfg->unwrap->flattening, "phase_unwrapping_flattening_checkbutton");
    NUM(&cfg->unwrap->procs, "phase_unwrapping_processors_entry");
    NUM(&cfg->unwrap->tiles_azimuth, "phase_unwrapping_tiles_azimuth_entry");
    NUM(&cfg->unwrap->tiles_range, "phase_unwrapping_tiles_range_entry");
    NUM(&cfg->unwrap->overlap_azimuth, "phase_unwrapping_overlap_azimuth_entry");
    NUM(&cfg->unwrap->overlap_range, "phase_unwrapping_overlap_range_entry");
    DBL(&cfg->unwrap->filter, "phase_unwrapping_filter_entry");
    NUM(&cfg->unwrap->tiles_per_degree, "phase_unwrapping_tiles_per_degree_entry");
    //cfg->unwrap->qc = new_blank_str();
    //cfg->unwrap->status = new_str("new");
  
    NUM(&cfg->refine->iter, "baseline_refinement_iterations_entry");
    NUM(&cfg->refine->max, "baseline_refinement_max_iterations_entry");
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
  sprintf(cfg->geocode->proj, "%s/projections/utm/utm.proj", 
	  get_asf_share_dir());
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
