#include "libasf_meta.h"

#define METADATA_VERSION 1.4

/********************************************************
 * meta_general_init():
 * Allocate memory for and initialize elements of a meta
 * general structure */
meta_general *asf_meta_general_init(void)
{
  meta_general *general = (meta_general *)MALLOC(sizeof(meta_general));

  /* Fill with ludicrous values.  */
  strcpy(general->sensor, MAGIC_UNSET_STRING);
  strcpy(general->mode, MAGIC_UNSET_STRING);
  strcpy(general->processor, MAGIC_UNSET_STRING);
  general->data_type = MAGIC_UNSET_INT;
  general->image_data_type = MAGIC_UNSET_INT;
  strcpy(general->system, MAGIC_UNSET_STRING);
  general->orbit = MAGIC_UNSET_INT;
  general->orbit_direction = MAGIC_UNSET_CHAR;
  general->frame = MAGIC_UNSET_INT;
  general->band_number = 0;
  general->line_count = MAGIC_UNSET_INT;
  general->sample_count = MAGIC_UNSET_INT;
  general->start_line = MAGIC_UNSET_INT;
  general->start_sample = MAGIC_UNSET_INT;
  general->x_pixel_size = MAGIC_UNSET_DOUBLE;
  general->y_pixel_size = MAGIC_UNSET_DOUBLE;
  general->center_latitude = MAGIC_UNSET_DOUBLE;
  general->center_longitude = MAGIC_UNSET_DOUBLE;
  general->re_major = MAGIC_UNSET_DOUBLE;
  general->re_minor = MAGIC_UNSET_DOUBLE;
  general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  general->missing_lines = MAGIC_UNSET_INT;

  return general;
}

/********************************************************
 * meta_sar_init():
 * Allocate memory for and initialize elements of a meta
 * sar structure */
meta_sar *asf_meta_sar_init(void)
{
  meta_sar *sar = (meta_sar *)MALLOC(sizeof(meta_sar));

  /* Fill with ludicrous values.  */
  sar->image_type = MAGIC_UNSET_CHAR; 
  sar->look_direction = MAGIC_UNSET_CHAR;
  sar->look_count = MAGIC_UNSET_INT;
  sar->deskewed = MAGIC_UNSET_INT;
  sar->original_line_count = MAGIC_UNSET_INT;
  sar->original_sample_count = MAGIC_UNSET_INT;
  sar->line_increment = MAGIC_UNSET_DOUBLE;
  sar->sample_increment = MAGIC_UNSET_DOUBLE;
  sar->range_time_per_pixel = MAGIC_UNSET_DOUBLE;
  sar->azimuth_time_per_pixel = MAGIC_UNSET_DOUBLE;
  sar->slant_shift = MAGIC_UNSET_DOUBLE;
  sar->time_shift = MAGIC_UNSET_DOUBLE;
  sar->slant_range_first_pixel = MAGIC_UNSET_DOUBLE;
  sar->wavelength = MAGIC_UNSET_DOUBLE;
  sar->prf = MAGIC_UNSET_DOUBLE;
  sar->earth_radius = MAGIC_UNSET_DOUBLE;
  sar->satellite_height = MAGIC_UNSET_DOUBLE;
  strcpy(sar->satellite_binary_time, MAGIC_UNSET_STRING);
  strcpy(sar->satellite_clock_time, MAGIC_UNSET_STRING);
  sar->range_doppler_coefficients[0] = MAGIC_UNSET_DOUBLE;
  sar->range_doppler_coefficients[1] = MAGIC_UNSET_DOUBLE;
  sar->range_doppler_coefficients[2] = MAGIC_UNSET_DOUBLE;
  sar->azimuth_doppler_coefficients[0] = MAGIC_UNSET_DOUBLE; 
  sar->azimuth_doppler_coefficients[1] = MAGIC_UNSET_DOUBLE; 
  sar->azimuth_doppler_coefficients[2] = MAGIC_UNSET_DOUBLE; 
  sar->azimuth_processing_bandwidth = MAGIC_UNSET_DOUBLE;
  sar->chirp_rate = MAGIC_UNSET_DOUBLE;
  sar->pulse_duration = MAGIC_UNSET_DOUBLE;
  sar->range_sampling_rate = MAGIC_UNSET_DOUBLE;
  return sar;
}

/********************************************************
 * meta_projection_init():
 * Allocate memory for and initialize elements of a meta
 * projection structure */
meta_projection *asf_meta_projection_init(void)
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
  projection->param.atct.rlocal = MAGIC_UNSET_DOUBLE;
  projection->param.atct.alpha1 = MAGIC_UNSET_DOUBLE;
  projection->param.atct.alpha2 = MAGIC_UNSET_DOUBLE;
  projection->param.atct.alpha3 = MAGIC_UNSET_DOUBLE;
  projection->param.albers.std_parallel1 = MAGIC_UNSET_DOUBLE;
  projection->param.albers.std_parallel2 = MAGIC_UNSET_DOUBLE;
  projection->param.albers.center_meridian = MAGIC_UNSET_DOUBLE;
  projection->param.albers.orig_latitude = MAGIC_UNSET_DOUBLE;
  projection->param.albers.false_easting = MAGIC_UNSET_DOUBLE;
  projection->param.albers.false_northing = MAGIC_UNSET_DOUBLE;
  projection->param.lamaz.center_lon = MAGIC_UNSET_DOUBLE;
  projection->param.lamaz.center_lat = MAGIC_UNSET_DOUBLE;
  projection->param.lamaz.false_easting = MAGIC_UNSET_DOUBLE;
  projection->param.lamaz.false_northing = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.plat1 = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.plat2 = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.lat0 = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.lon0 = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.false_easting = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.false_northing = MAGIC_UNSET_DOUBLE;
  projection->param.lamcc.scale_factor = MAGIC_UNSET_DOUBLE;
  projection->param.ps.slat = MAGIC_UNSET_DOUBLE;
  projection->param.ps.slon = MAGIC_UNSET_DOUBLE;
  projection->param.ps.false_easting = MAGIC_UNSET_DOUBLE;
  projection->param.ps.false_northing = MAGIC_UNSET_DOUBLE;
  projection->param.utm.zone = MAGIC_UNSET_INT;
  projection->param.utm.false_easting = MAGIC_UNSET_DOUBLE;
  projection->param.utm.false_northing = MAGIC_UNSET_DOUBLE;
  projection->param.utm.lat0 = MAGIC_UNSET_DOUBLE;
  projection->param.utm.lon0 = MAGIC_UNSET_DOUBLE;
  projection->param.utm.scale_factor = 0.9996;
  projection->param.state.zone = MAGIC_UNSET_INT;
  return projection;
}

/*******************************************************************************
 * meta_state_vectors_init():
 * Allocate memory for and initialize elements of a meta_state_vectors structure.
 * This one is a little hairy as it REQUIRES that the 'vecs' element be an array
 * rather than a pointer in order to initialize correctly. This puts the entire
 * state vectors block (including vecs) in one block of memory, which is how our
 * client code expects it to be.  */
meta_state_vectors *asf_meta_state_vectors_init(int vector_count)
{
  meta_state_vectors *state_vectors;
  int ii=0;
  state_vectors = (meta_state_vectors *)MALLOC(  sizeof(meta_state_vectors)
                                               + vector_count * sizeof(state_loc)
                                              );
  /* Fill with ludicrous values.  */
  state_vectors->year = MAGIC_UNSET_INT;
  state_vectors->julDay = MAGIC_UNSET_INT;
  state_vectors->second = MAGIC_UNSET_DOUBLE;
  state_vectors->vector_count = vector_count;
  state_vectors->num = state_vectors->vector_count;
  state_vectors->vecs = (state_loc *) MALLOC(vector_count * sizeof(state_loc));
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

/*******************************************************************************
 * meta_geo_init():
 * Initialize the geometry structure */
geo_parameters *asf_meta_geo_init(void)
{
  geo_parameters *geo = (geo_parameters *) MALLOC(sizeof(geo_parameters));
  geo->type = 'G';
  geo->proj = NULL;
  geo->lookDir = 'R';
  geo->deskew = 0;
  geo->xPix = geo->yPix = 10.989;
  geo->rngPixTime = 5.989e-8;
  geo->azPixTime = 7.989e-4;
  geo->timeShift = geo->slantShift = 0;
  geo->slantFirst = 800000.989;
  geo->wavelen = 0.056989;
  geo->dopRange[0] = MAGIC_UNSET_DOUBLE;
  geo->dopRange[1] = MAGIC_UNSET_DOUBLE;
  geo->dopRange[2] = MAGIC_UNSET_DOUBLE;
  geo->dopAz[0] = MAGIC_UNSET_DOUBLE;
  geo->dopAz[1] = MAGIC_UNSET_DOUBLE;
  geo->dopAz[2] = MAGIC_UNSET_DOUBLE;
  return geo;
}

/*****************************************************************************
 * meta_ifm_init():
 * Allocate memory and initialize the interferometry structure */
ifm_parameters *asf_meta_ifm_init(void)
{  
  ifm_parameters *ifm = (ifm_parameters *) MALLOC(sizeof(ifm_parameters));
  ifm->er = 6360000.989;
  ifm->ht = ifm->er+700000.0;
  ifm->nLooks = 5;
  ifm->orig_nLines = 25989;
  ifm->orig_nSamples = 5989;
/* ifm->lookCenter = 19.989; */
  return ifm;
}

/*********************************************************
 * meta_info_init():
 * Allocate memory for and initialize elements of a meta
 * extra_info structure */
extra_info *asf_meta_info_init(void)
{
  extra_info *info = (extra_info *) MALLOC(sizeof(extra_info));
  strcpy(info->sensor, "unknown");
  strcpy(info->mode, "unknown");
  strcpy(info->processor, "unknown");
  info->orbit = -99;
  info->bitErrorRate = -99;
  strcpy(info->satBinTime, "unknown");
  strcpy(info->satClkTime, "unknown");
  info->prf = -99;
  return info;
}

/********************************************************
 * meta_stats_init():
 * Allocate memory for and initialize elements of a meta
 * stats structure */
meta_stats *asf_meta_stats_init(void)
{
  meta_stats *stats = (meta_stats *)MALLOC(sizeof(meta_stats));
  stats->min = MAGIC_UNSET_DOUBLE;
  stats->max = MAGIC_UNSET_DOUBLE;
  stats->mean = MAGIC_UNSET_DOUBLE;
  stats->rmse = MAGIC_UNSET_DOUBLE;
  stats->std_deviation = MAGIC_UNSET_DOUBLE;
  stats->mask = MAGIC_UNSET_DOUBLE;
  return stats;
}


/****************************************************
 * asf_meta_init:
 * Allocate memory for a fresh meta structure and fill
 * existing elements with bogus values */
meta_parameters *asf_meta_init(char *version)
{
  meta_parameters *meta = (meta_parameters *)MALLOC(sizeof(meta_parameters));

  if (strncmp(version, "0.9", 3) == 0) {
    meta->geo = asf_meta_geo_init();
    meta->ifm = asf_meta_ifm_init();
    meta->stVec = NULL;
    meta->info = asf_meta_info_init();
    meta->general = NULL;
    meta->sar = NULL;
    meta->projection = NULL;
    meta->state_vectors = NULL;
    meta->stats = NULL;
  }
  else if (strncmp(version, "1.1", 3) == 0 || strncmp(version, "1.2", 3) == 0 ||
	   strncmp(version, "1.3", 3) == 0 || strncmp(version, "1.4", 3) == 0) { 
    meta->geo = NULL;
    meta->ifm = NULL;
    meta->stVec = NULL;
    meta->info = NULL;
    meta->general = asf_meta_general_init();
    meta->sar = asf_meta_sar_init();
    meta->projection = NULL;
    meta->state_vectors = NULL;
    meta->stats = NULL;
  }
  
  meta->meta_version = atof(version);
  
  return meta;
}

