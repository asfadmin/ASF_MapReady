/****************************************************************
FUNCTION NAME:

DESCRIPTION:

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h" /* needed for MAGIC_UNSET_DOUBLE */

META_DDR_STRUCT meta_ddr_structs[NUM_META_DDR_STRUCTS] = {
  { "", NULL, NULL},
  { "", NULL, NULL},
  { "", NULL, NULL}
};

//meta_general       *meta_general_init(void);
//meta_sar           *meta_sar_init(void);
//meta_projection    *meta_projection_init(void);
//meta_state_vectors *meta_state_vectors_init(int vector_count);
//meta_statistics    *meta_statistics_init(int band_count);


/********************************************************
 * meta_general_init():
 * Allocate memory for and initialize elements of a meta
 * general structure */
meta_general *meta_general_init(void)
{
  meta_general *general = (meta_general *)MALLOC(sizeof(meta_general));

  /* Fill with ludicrous values.  */
  strcpy(general->basename, MAGIC_UNSET_STRING);
  strcpy(general->sensor, MAGIC_UNSET_STRING);
  strcpy(general->sensor_name, MAGIC_UNSET_STRING);
  strcpy(general->mode, MAGIC_UNSET_STRING);
  strcpy(general->receiving_station, MAGIC_UNSET_STRING);
  strcpy(general->processor, MAGIC_UNSET_STRING);
  general->data_type = MAGIC_UNSET_INT;
  general->image_data_type = MAGIC_UNSET_INT;
  general->radiometry = r_AMP;
  strcpy(general->acquisition_date, MAGIC_UNSET_STRING);
  general->orbit = MAGIC_UNSET_INT;
  general->orbit_direction = MAGIC_UNSET_CHAR;
  general->frame = MAGIC_UNSET_INT;
  general->band_count = 1;
  strcpy(general->bands, MAGIC_UNSET_STRING);
  general->line_count = MAGIC_UNSET_INT;
  general->sample_count = MAGIC_UNSET_INT;
  general->start_line = MAGIC_UNSET_INT;
  general->start_sample = MAGIC_UNSET_INT;
  general->line_scaling = 1;
  general->sample_scaling = 1;
  general->x_pixel_size = MAGIC_UNSET_DOUBLE;
  general->y_pixel_size = MAGIC_UNSET_DOUBLE;
  general->center_latitude = MAGIC_UNSET_DOUBLE;
  general->center_longitude = MAGIC_UNSET_DOUBLE;
  general->re_major = MAGIC_UNSET_DOUBLE;
  general->re_minor = MAGIC_UNSET_DOUBLE;
  general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  general->missing_lines = MAGIC_UNSET_INT;
  general->no_data = MAGIC_UNSET_DOUBLE;
  return general;
}

/********************************************************
 * meta_sar_init():
 * Allocate memory for and initialize elements of a meta
 * sar structure */
meta_sar *meta_sar_init(void)
{
  int ii;

  meta_sar *sar = (meta_sar *)MALLOC(sizeof(meta_sar));

  /* Fill with ludicrous values.  */
  strcpy(sar->polarization, MAGIC_UNSET_STRING);
  sar->image_type = MAGIC_UNSET_CHAR;
  sar->look_direction = MAGIC_UNSET_CHAR;
  sar->azimuth_look_count = MAGIC_UNSET_INT;
  sar->range_look_count = MAGIC_UNSET_INT;
  sar->deskewed = MAGIC_UNSET_INT;
  sar->multilook = TRUE;
  sar->original_line_count = MAGIC_UNSET_INT;
  sar->original_sample_count = MAGIC_UNSET_INT;
  sar->line_increment = 1;
  sar->sample_increment = 1;
  sar->range_time_per_pixel = MAGIC_UNSET_DOUBLE;
  sar->azimuth_time_per_pixel = MAGIC_UNSET_DOUBLE;
  sar->slant_shift = MAGIC_UNSET_DOUBLE;
  sar->time_shift = MAGIC_UNSET_DOUBLE;
  sar->slant_range_first_pixel = MAGIC_UNSET_DOUBLE;
  sar->wavelength = MAGIC_UNSET_DOUBLE;
  sar->prf = MAGIC_UNSET_DOUBLE;
  sar->earth_radius = MAGIC_UNSET_DOUBLE;
  sar->earth_radius_pp = MAGIC_UNSET_DOUBLE;
  sar->satellite_height = MAGIC_UNSET_DOUBLE;
  strcpy(sar->satellite_binary_time, MAGIC_UNSET_STRING);
  strcpy(sar->satellite_clock_time, MAGIC_UNSET_STRING);
  sar->range_doppler_coefficients[0] = 0.0;
  sar->range_doppler_coefficients[1] = 0.0;
  sar->range_doppler_coefficients[2] = 0.0;
  sar->azimuth_doppler_coefficients[0] = 0.0;
  sar->azimuth_doppler_coefficients[1] = 0.0;
  sar->azimuth_doppler_coefficients[2] = 0.0;
  sar->chirp_rate = MAGIC_UNSET_DOUBLE;
  sar->pulse_duration = MAGIC_UNSET_DOUBLE;
  sar->range_sampling_rate = MAGIC_UNSET_DOUBLE;
  strcpy(sar->polarization, MAGIC_UNSET_STRING);
  sar->multilook = MAGIC_UNSET_INT;
  sar->pitch = MAGIC_UNSET_DOUBLE;
  sar->roll = MAGIC_UNSET_DOUBLE;
  sar->yaw = MAGIC_UNSET_DOUBLE;
  sar->azimuth_processing_bandwidth = MAGIC_UNSET_DOUBLE;
  for (ii=0; ii<6; ii++) {
    sar->incid_a[ii] = MAGIC_UNSET_DOUBLE;
  }

  return sar;
}

/********************************************************
 * meta_optical_init():
 * Allocate memory for and initialize elements of a meta
 * optical structure */
meta_optical *meta_optical_init(void)
{
  meta_optical *optical = (meta_optical *)MALLOC(sizeof(meta_optical));

  /* Fill with ludicrous values.  */
  strcpy(optical->correction_level, MAGIC_UNSET_STRING);
  optical->cloud_percentage = MAGIC_UNSET_DOUBLE;
  optical->sun_azimuth_angle = MAGIC_UNSET_DOUBLE;
  optical->sun_elevation_angle = MAGIC_UNSET_DOUBLE;

  return optical;
}

/********************************************************
 * meta_thermal_init():
 * Allocate memory for and initialize elements of a meta
 * thermal structure */
meta_thermal *meta_thermal_init(void)
{
  meta_thermal *thermal = (meta_thermal *)MALLOC(sizeof(meta_thermal));

  /* Fill with ludicrous values.  */
  thermal->band_gain = MAGIC_UNSET_DOUBLE;
  thermal->band_gain_change = MAGIC_UNSET_DOUBLE;

  return thermal;
}

/********************************************************
 * meta_projection_init():
 * Allocate memory for and initialize elements of a meta
 * projection structure */
meta_projection *meta_projection_init(void)
{
  meta_projection *projection = (meta_projection *)MALLOC(sizeof(meta_projection));
  projection->type = MAGIC_UNSET_INT;
  projection->startX = MAGIC_UNSET_DOUBLE;
  projection->startY = MAGIC_UNSET_DOUBLE;
  projection->perX = MAGIC_UNSET_DOUBLE;
  projection->perY = MAGIC_UNSET_DOUBLE;
  strcpy (projection->units, MAGIC_UNSET_STRING);
  projection->hem = MAGIC_UNSET_CHAR;
  projection->re_major = MAGIC_UNSET_DOUBLE;
  projection->re_minor = MAGIC_UNSET_DOUBLE;
  projection->height = 0.0;
  projection->spheroid = UNKNOWN_SPHEROID;
  projection->datum = UNKNOWN_DATUM;
/*  projection->ecc = MAGIC_UNSET_DOUBLE;        * DEPRECATED */
/* Can't really initalize projection->param to a dummy value, so just leave it.*/
  return projection;
}

meta_transform *meta_transform_init(void)
{
  int ii;

  meta_transform *map = (meta_transform *) MALLOC(sizeof(meta_transform));
  map->source_pixel_size = MAGIC_UNSET_DOUBLE;
  map->target_pixel_size = MAGIC_UNSET_DOUBLE;
  map->parameter_count = MAGIC_UNSET_INT;
  for (ii=0; ii<10; ii++) {
    map->x[ii] = MAGIC_UNSET_DOUBLE;
    map->y[ii] = MAGIC_UNSET_DOUBLE;
    map->l[ii] = MAGIC_UNSET_DOUBLE;
    map->s[ii] = MAGIC_UNSET_DOUBLE;
    map->map2ls_a[ii] = MAGIC_UNSET_DOUBLE;
    map->map2ls_b[ii] = MAGIC_UNSET_DOUBLE;
  }
  map->use_reverse_transform = MAGIC_UNSET_INT;
  return map;
}

meta_calibration *meta_calibration_init(void)
{
  meta_calibration *cal =
    (meta_calibration *) MALLOC(sizeof(meta_calibration));
  cal->type = unknown_cal;
  cal->asf = NULL;
  cal->asf_scansar = NULL;
  cal->esa = NULL;
  cal->rsat = NULL;
  cal->alos = NULL;
  cal->tsx = NULL;
  cal->uavsar = NULL;

  return cal;
}

meta_doppler *meta_doppler_init(void)
{
  meta_doppler *dop = (meta_doppler *) MALLOC(sizeof(meta_doppler));
  dop->type = unknown_doppler;
  dop->tsx = NULL;

  return dop;
}

meta_airsar *meta_airsar_init(void)
{
  meta_airsar *airsar = (meta_airsar *) MALLOC(sizeof(meta_airsar));
  airsar->scale_factor = MAGIC_UNSET_DOUBLE;
  airsar->gps_altitude = MAGIC_UNSET_DOUBLE;
  airsar->lat_peg_point = MAGIC_UNSET_DOUBLE;
  airsar->lon_peg_point = MAGIC_UNSET_DOUBLE;
  airsar->head_peg_point = MAGIC_UNSET_DOUBLE;
  airsar->along_track_offset = MAGIC_UNSET_DOUBLE;
  airsar->cross_track_offset = MAGIC_UNSET_DOUBLE;
  airsar->elevation_increment = MAGIC_UNSET_DOUBLE;
  airsar->elevation_offset = MAGIC_UNSET_DOUBLE;
  return airsar;
}

meta_uavsar *meta_uavsar_init(void)
{
  meta_uavsar *uavsar = (meta_uavsar *) MALLOC(sizeof(meta_uavsar));
  uavsar->scale_factor = MAGIC_UNSET_DOUBLE;
  uavsar->gps_altitude = MAGIC_UNSET_DOUBLE;
  uavsar->lat_peg_point = MAGIC_UNSET_DOUBLE;
  uavsar->lon_peg_point = MAGIC_UNSET_DOUBLE;
  uavsar->head_peg_point = MAGIC_UNSET_DOUBLE;
  uavsar->along_track_offset = MAGIC_UNSET_DOUBLE;
  uavsar->cross_track_offset = MAGIC_UNSET_DOUBLE;
  return uavsar;
}

meta_dem *meta_dem_init(void)
{
  meta_dem *dem = (meta_dem *) MALLOC(sizeof(meta_dem));
  strcpy(dem->source, MAGIC_UNSET_STRING);
  strcpy(dem->format, MAGIC_UNSET_STRING);
  strcpy(dem->tiles, MAGIC_UNSET_STRING);
  dem->min_value = MAGIC_UNSET_DOUBLE;
  dem->max_value = MAGIC_UNSET_DOUBLE;
  dem->mean_value = MAGIC_UNSET_DOUBLE;
  dem->standard_deviation = MAGIC_UNSET_DOUBLE;
  strcpy(dem->unit_type, MAGIC_UNSET_STRING);
  dem->no_data = MAGIC_UNSET_DOUBLE;
  return dem;
}

/*******************************************************************************
 * meta_state_vectors_init():
 * Allocate memory for and initialize elements of a meta_state_vectors structure.
 * This one is a little hairy as it REQUIRES that the 'vecs' element be an array
 * rather than a pointer in order to initialize correctly. This puts the entire
 * state vectors block (including vecs) in one block of memory, which is how our
 * client code expects it to be.  */
meta_state_vectors *meta_state_vectors_init(int vector_count)
{
  meta_state_vectors *state_vectors;
  int ii=0;
  int vc;

  vc = vector_count > 0 ? vector_count : 0;
  state_vectors = (meta_state_vectors *)MALLOC(  sizeof(meta_state_vectors)
                                               + vc * sizeof(state_loc)
                                              );
  /* Fill with ludicrous values.  */
  state_vectors->vector_count = vc;
  state_vectors->year = MAGIC_UNSET_INT;
  state_vectors->julDay = MAGIC_UNSET_INT;
  state_vectors->second = MAGIC_UNSET_DOUBLE;
  state_vectors->num = state_vectors->vector_count;
  for (ii=0; ii<state_vectors->vector_count; ii++) {
    state_vectors->vecs[ii].time = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.pos.x = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.pos.y = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.pos.z = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.vel.x = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.vel.y = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.vel.z = MAGIC_UNSET_DOUBLE;
  }
  return state_vectors;
}

/********************************************************
 * meta_statistics_init():
 * Allocate memory for and initialize elements of a meta
 * statistics structure (metadata v2.4+) */
meta_statistics *meta_statistics_init(int band_count)
{
  int i = 0;
  int bc;
  meta_statistics *statistics;

  bc = band_count > 0 ? band_count : 0;
  statistics = (meta_statistics *)MALLOC(sizeof(meta_statistics)
                                         + bc * sizeof(meta_stats)
                                        );
  if (statistics) {
    statistics->band_count = bc;
    for (i=0; i<statistics->band_count; i++) {
      strcpy (statistics->band_stats[i].band_id, MAGIC_UNSET_STRING);
      statistics->band_stats[i].min            = MAGIC_UNSET_DOUBLE;
      statistics->band_stats[i].max            = MAGIC_UNSET_DOUBLE;
      statistics->band_stats[i].mean           = MAGIC_UNSET_DOUBLE;
      statistics->band_stats[i].rmse           = MAGIC_UNSET_DOUBLE;
      statistics->band_stats[i].std_deviation  = MAGIC_UNSET_DOUBLE;
      statistics->band_stats[i].mask           = MAGIC_UNSET_DOUBLE;
    }
  }
  return statistics;
}

meta_colormap *meta_colormap_init(void)
{
  meta_colormap *colormap = NULL;

  colormap = (meta_colormap *)CALLOC(1, sizeof(meta_colormap));
  strcpy(colormap->look_up_table, MAGIC_UNSET_STRING);
  strcpy(colormap->band_id, MAGIC_UNSET_STRING);
  colormap->num_elements = MAGIC_UNSET_INT;
  colormap->rgb=NULL;

  return colormap;
}

/********************************************************
 * meta_stats_init():
 * Allocate memory for and initialize elements of a meta
 * stats structure (metadata prior to v2.4)
meta_stats *meta_stats_init()
{
  meta_stats *stats = (meta_stats *)MALLOC(sizeof(meta_stats));
  strcpy (stats->band_id, MAGIC_UNSET_STRING);
  stats->min            = MAGIC_UNSET_DOUBLE;
  stats->max            = MAGIC_UNSET_DOUBLE;
  stats->mean           = MAGIC_UNSET_DOUBLE;
  stats->rmse           = MAGIC_UNSET_DOUBLE;
  stats->std_deviation  = MAGIC_UNSET_DOUBLE;
  stats->mask           = MAGIC_UNSET_DOUBLE;
  return stats;
}
*/

/*******************************************************************
 * meta_location_init():
 * Allocate memory for and initialize elements of a meta
 * location structure */
meta_location *meta_location_init(void)
{
  meta_location *location = (meta_location *)MALLOC(sizeof(meta_location));
  location->lat_start_near_range = MAGIC_UNSET_DOUBLE;
  location->lon_start_near_range = MAGIC_UNSET_DOUBLE;
  location->lat_start_far_range = MAGIC_UNSET_DOUBLE;
  location->lon_start_far_range = MAGIC_UNSET_DOUBLE;
  location->lat_end_near_range = MAGIC_UNSET_DOUBLE;
  location->lon_end_near_range = MAGIC_UNSET_DOUBLE;
  location->lat_end_far_range = MAGIC_UNSET_DOUBLE;
  location->lon_end_far_range = MAGIC_UNSET_DOUBLE;
  return location;
}

/*******************************************************************
 * meta_insar_init():
 * Allocate memory for and initialize elements of a meta insar
 * structure */
meta_insar *meta_insar_init(void)
{
  meta_insar *insar = (meta_insar *) MALLOC(sizeof(meta_insar));
  strcpy(insar->processor, MAGIC_UNSET_STRING);
  strcpy(insar->master_image, MAGIC_UNSET_STRING);
  strcpy(insar->slave_image, MAGIC_UNSET_STRING);
  strcpy(insar->master_acquisition_date, MAGIC_UNSET_STRING);
  strcpy(insar->slave_acquisition_date, MAGIC_UNSET_STRING);
  insar->center_look_angle = MAGIC_UNSET_DOUBLE;
  insar->doppler = MAGIC_UNSET_DOUBLE;
  strcpy(insar->doppler_units, MAGIC_UNSET_STRING);
  insar->doppler_rate = MAGIC_UNSET_DOUBLE;
  strcpy(insar->doppler_rate_units, MAGIC_UNSET_STRING);
  insar->baseline_length = MAGIC_UNSET_DOUBLE;
  strcpy(insar->baseline_length_units, MAGIC_UNSET_STRING);
  insar->baseline_parallel = MAGIC_UNSET_DOUBLE;
  strcpy(insar->baseline_parallel_units, MAGIC_UNSET_STRING);
  insar->baseline_parallel_rate = MAGIC_UNSET_DOUBLE;
  strcpy(insar->baseline_parallel_rate_units, MAGIC_UNSET_STRING);
  insar->baseline_perpendicular = MAGIC_UNSET_DOUBLE;
  strcpy(insar->baseline_perpendicular_units, MAGIC_UNSET_STRING);
  insar->baseline_perpendicular_rate = MAGIC_UNSET_DOUBLE;
  strcpy(insar->baseline_perpendicular_rate_units, MAGIC_UNSET_STRING);
  insar->baseline_temporal = MAGIC_UNSET_INT;
  strcpy(insar->baseline_temporal_units, MAGIC_UNSET_STRING);
  insar->baseline_critical = MAGIC_UNSET_DOUBLE;
  strcpy(insar->baseline_critical_units, MAGIC_UNSET_STRING);
  return insar;
}

meta_latlon *meta_latlon_init(int line_count, int sample_count)
{
  meta_latlon *latlon = (meta_latlon *) MALLOC(sizeof(meta_latlon));
  latlon->lat = (float *) MALLOC(sizeof(float)*line_count*sample_count);
  latlon->lon = (float *) MALLOC(sizeof(float)*line_count*sample_count);
  return latlon;
}

/****************************************************
 * raw_init:
 * Allocate memory for a fresh meta structure and fill
 * existing elements with bogus values */
meta_parameters *raw_init(void)
{
  meta_parameters *meta = (meta_parameters *)MALLOC(sizeof(meta_parameters));
  meta->general         = meta_general_init();
  meta->sar             = NULL;
  meta->optical         = NULL;
  meta->thermal         = NULL;  /* Not yet in use */
  meta->projection      = NULL;  /* Allocated later if geocoded */
  meta->transform       = NULL;
  meta->airsar          = NULL;
  meta->uavsar          = NULL;
  meta->stats           = NULL;
  meta->state_vectors   = NULL;  /* Allocated upon discovery of state vectors */
  meta->calibration     = NULL;
  meta->location        = NULL;
  meta->colormap        = NULL;  /* Allocated upon discovery of Palette Color TIFF embedded color map */
  meta->doppler         = NULL;
  meta->insar           = NULL;
  meta->dem             = NULL;
  meta->latlon          = NULL;

  meta->meta_version = META_VERSION;

/* Initialize deprecated structure elements: Creates and initializes a
   meta_parameters structure, guessing at conceivable values.  These
   bogus values always end in "989", so you can tell them from real
   values.
  meta->geo   = MALLOC(sizeof(geo_parameters));
  meta->ifm   = MALLOC(sizeof(ifm_parameters));
  meta->stVec = meta->state_vectors; // Compatability alias.
  meta->info  = NULL;

  // Guess at conceivable values for deprecated elements.
  meta->geo->type = 'G';
  meta->geo->proj = NULL;
  meta->geo->lookDir = 'R';
  meta->geo->deskew = 0;
  meta->geo->xPix = meta->geo->yPix = 10.989;
  meta->geo->rngPixTime = 5.989e-8;
  meta->geo->azPixTime = 7.989e-4;
  meta->geo->timeShift = meta->geo->slantShift = 0;
  meta->geo->slantFirst = 800000.989;
  meta->geo->wavelen = 0.056989;
  meta->geo->dopRange[0] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopRange[1] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopRange[2] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopAz[0] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopAz[1] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopAz[2] = MAGIC_UNSET_DOUBLE;

  meta->ifm->er = 6360000.989;
  meta->ifm->ht = meta->ifm->er+700000.0;
  meta->ifm->nLooks = 5;
  meta->ifm->orig_nLines = 25989;
  meta->ifm->orig_nSamples = 5989;
  //  meta->ifm->lookCenter = 19.989;*/

  return meta;
}

/******************************************************
 * meta_init_old:
 * Reads in a new meta_parameters record from disk with
 * the given filename.  If no .meta exists, it calls
 * meta_create to construct one.
 * THIS SHOULD EVENTUALLY BE REMOVED AND raw_init SHOULD
 * TAKE ITS NAME   */
meta_parameters *meta_init(const char *fName)
{
  if (extExists(fName,".meta")) /*Read .meta file if possible*/
    return meta_read(fName);
  else
    return meta_create(fName);
}

/************************************************************
 * add_meta_ddr_struct:
 * Adds or updates meta & ddr pointers associated with a
 * particular file name. If you are adding a meta pointer,
 * it expects NULL in the ddr argument, and visa versa for
 * a ddr addition/update. Hey, I know this ain't the best
 * system to keep track of different metadata structures.
 * How about YOU write a better one! */
void add_meta_ddr_struct(const char *name, meta_parameters *meta, struct DDR *ddr)
{
  int ii;
  char base_name[1024];
/* Put name in base_name but without the extention */
  create_name(base_name, name, "");
/* First check to see if this filename already has a structure in it */
  for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++)
  {
    if (0==strcmp(meta_ddr_structs[ii].base_name, base_name)) {
      if (meta) meta_ddr_structs[ii].meta = meta;
      if (ddr) meta_ddr_structs[ii].ddr = ddr;
      return;
    }
  }
/* Otherwise look for an empty slot and fill it */
  for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++)
  {
    if (0==strcmp(meta_ddr_structs[ii].base_name, "")) {
      strcpy(meta_ddr_structs[ii].base_name, base_name);
      meta_ddr_structs[ii].meta = meta;
      meta_ddr_structs[ii].ddr = ddr;
      return;
    }
  }
/* Report if we made it here */
  printf("\n"
         "WARNING: * asf_meta library function add_meta_ddr_struct() failed.\n"
         "         * Metadata may not be properly updated when written to file.\n");
}

/************************************************************
 * get_meta_ddr_struct_index:
 * Find the meta/ddr pointers associated with a particular
 * file name */
int get_meta_ddr_struct_index(const char *name)
{
  int ii;
  char base_name[1024];
/* Put name in base_name but without the extention */
  create_name(base_name, name, "");
  for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++)
  {
    if (0==strcmp(meta_ddr_structs[ii].base_name, base_name)) {
      return ii;
    }
  }
  return -1;
}


/****************************************************
 * meta_free:
 * Disposes of a given metadata parameters record.  */
void meta_free(meta_parameters *meta)
{

  int ii;

  if (meta != NULL) {
    FREE(meta->general);
    meta->general = NULL;
    FREE(meta->sar);
    meta->sar = NULL;
    FREE(meta->optical);
    meta->optical = NULL;
    FREE(meta->thermal);
    meta->thermal = NULL;
    FREE(meta->projection);
    meta->projection = NULL;
    FREE(meta->stats);
    meta->stats = NULL;
    FREE(meta->state_vectors);
    meta->state_vectors = NULL;
    FREE(meta->location);
    meta->location = NULL;
    FREE(meta->transform);
    meta->transform = NULL;
    FREE(meta->airsar);
    meta->airsar = NULL;
    FREE(meta->uavsar);
    meta->uavsar = NULL;
    FREE(meta->insar);
    meta->insar = NULL;
    FREE(meta->dem);
    meta->dem = NULL;
    if (meta->latlon) {
      FREE(meta->latlon->lat);
      FREE(meta->latlon->lon);
      FREE(meta->latlon);
      meta->latlon = NULL;
    }
    if (meta->colormap) {
      FREE(meta->colormap->rgb);
      FREE(meta->colormap);
    }
    if (meta->doppler && meta->doppler->tsx) {
      for (ii=0; ii<meta->doppler->tsx->doppler_count; ii++) {
	FREE(meta->doppler->tsx->dop[ii].coefficient);
	meta->doppler->tsx->dop[ii].coefficient = NULL;
      }
      FREE(meta->doppler->tsx->dop);
      meta->doppler->tsx->dop = NULL;
      FREE(meta->doppler->tsx);
      meta->doppler->tsx = NULL;
    }
    FREE(meta->doppler);
    meta->doppler = NULL;
    if (meta->calibration) {
      FREE(meta->calibration->alos);
      FREE(meta->calibration->rsat);
      FREE(meta->calibration->esa);
      FREE(meta->calibration->asf);
      FREE(meta->calibration->asf_scansar);
      FREE(meta->calibration->tsx);
      FREE(meta->calibration->uavsar);
      FREE(meta->calibration);
      meta->calibration = NULL;
    }
    /*
    FREE(meta->geo);
    meta->geo = NULL;
    FREE(meta->ifm);
    meta->ifm = NULL;
    meta->stVec = NULL;
    FREE(meta->info);
    meta->info = NULL;
    */

    for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++) {
      if (meta_ddr_structs[ii].meta == meta)
        {
    meta_ddr_structs[ii].meta = NULL;
    strcpy(meta_ddr_structs[ii].base_name, "");
  }
    }

    FREE(meta);
    meta = NULL;
  }
}
