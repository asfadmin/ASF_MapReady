#include "libasf_meta.h"

#define METADATA_VERSION 1.4

meta_parameters *asf_meta_read1x(meta_parameter_t *meta_struct)
{
  meta_parameters *meta;
  char *meta_version, *str, *data_type_str, *image_data_type_str, *projection_type_str;
  char *spheroid_type_str, *datum_type_str;
  int ii, vector_count, stats;

  /* Allocate memory */
  meta_version = (char *)MALLOC(10*sizeof(char));
  str = (char *)MALLOC(255*sizeof(char));
  data_type_str = (char *)MALLOC(30*sizeof(char));
  image_data_type_str = (char *)MALLOC(30*sizeof(char));
  projection_type_str = (char *)MALLOC(30*sizeof(char));
  spheroid_type_str = (char *)MALLOC(30*sizeof(char));
  datum_type_str = (char *)MALLOC(30*sizeof(char));

  /* Read metadata version as string for safe comparison */
  meta_version = metaString(meta_struct, "meta_version:"); 

  /* Initialize the metadata structure */
  meta = asf_meta_init(meta_version);

  /* General structure */
  str = metaString(meta_struct, "general.sensor:");
  sprintf(meta->general->sensor, "%s", str);
  str = metaString(meta_struct, "general.mode:");
  sprintf(meta->general->mode, "%s", str);
  str = metaString(meta_struct, "general.processor:");
  sprintf(meta->general->processor, "%s", str);
  if (strncmp(meta_version, "1.2", 3) == 0 ||
      strncmp(meta_version, "1.3", 3) == 0 ||
      strncmp(meta_version, "1.4", 3) == 0) {
    data_type_str = metaString(meta_struct, "general.data_type:");
    if (strncmp(data_type_str, "BYTE", 4) == 0)
      meta->general->data_type = BYTE;
    else if (strncmp(data_type_str, "INTEGER16", 9) == 0)
      meta->general->data_type = INTEGER16;
    else if (strncmp(data_type_str, "INTEGER32", 9) == 0)
      meta->general->data_type = INTEGER32;
    else if (strncmp(data_type_str, "REAL32", 6) == 0)
      meta->general->data_type = REAL32;
    else if (strncmp(data_type_str, "REAL64", 6) == 0)
      meta->general->data_type = REAL64;
    else if (strncmp(data_type_str, "COMPLEX_BYTE", 12) == 0)
      meta->general->data_type = COMPLEX_BYTE;
    else if (strncmp(data_type_str, "COMPLEX_INTEGER16", 17) == 0)
      meta->general->data_type = COMPLEX_INTEGER16;
    else if (strncmp(data_type_str, "COMPLEX_INTEGER32", 17) == 0)
      meta->general->data_type = COMPLEX_INTEGER32;
    else if (strncmp(data_type_str, "COMPLEX_REAL32", 14) == 0)
      meta->general->data_type = COMPLEX_REAL32;
    else if (strncmp(data_type_str, "COMPLEX_REAL64", 14) == 0)
      meta->general->data_type = COMPLEX_REAL64;
  }
  if (strncmp(meta_version, "1.2", 3) == 0 ||
      strncmp(meta_version, "1.3", 3) == 0 ||
      strncmp(meta_version, "1.4", 3) == 0) {
    image_data_type_str = metaString(meta_struct, "general.image_data_type:");
    if (strncmp(image_data_type_str, "RAW_IMAGE", 9) == 0)
      meta->general->image_data_type = RAW_IMAGE;
    else if (strncmp(image_data_type_str, "COMPLEX_IMAGE", 13) == 0)
      meta->general->image_data_type = COMPLEX_IMAGE;
    else if (strncmp(image_data_type_str, "AMPLITUDE_IMAGE", 15) == 0)
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    else if (strncmp(image_data_type_str, "PHASE_IMAGE", 11) == 0)
      meta->general->image_data_type = PHASE_IMAGE;
    else if (strncmp(image_data_type_str, "POWER_IMAGE", 11) == 0)
      meta->general->image_data_type = POWER_IMAGE;
    else if (strncmp(image_data_type_str, "SIGMA_IMAGE", 11) == 0)
      meta->general->image_data_type = SIGMA_IMAGE;
    else if (strncmp(image_data_type_str, "GAMMA_IMAGE", 11) == 0)
      meta->general->image_data_type = GAMMA_IMAGE;
    else if (strncmp(image_data_type_str, "BETA_IMAGE", 11) == 0)
      meta->general->image_data_type = BETA_IMAGE;
    else if (strncmp(image_data_type_str, "COHERENCE_IMAGE", 15) == 0)
      meta->general->image_data_type = COHERENCE_IMAGE;
    else if (strncmp(image_data_type_str, "GEOCODED_IMAGE", 14) == 0)
      meta->general->image_data_type = GEOCODED_IMAGE;
    else if (strncmp(image_data_type_str, "ELEVATION", 9) == 0)
      meta->general->image_data_type = ELEVATION;
    else if (strncmp(image_data_type_str, "DEM", 3) == 0)
      meta->general->image_data_type = DEM;
    else if (strncmp(image_data_type_str, "IMAGE", 5) == 0)
      meta->general->image_data_type = IMAGE;
  }
  str = metaString(meta_struct, "general.system:");
  sprintf(meta->general->system, "%s", str);
  meta->general->orbit = metaInt(meta_struct, "general.orbit:");
  meta->general->orbit_direction = 
    metaChar(meta_struct, "general.orbit_direction:");
  meta->general->frame = metaInt(meta_struct, "general.frame:");
  meta->general->band_number = 
    metaInt(meta_struct, "general.band_number:");
  meta->general->line_count = 
    metaInt(meta_struct, "general.line_count:");
  meta->general->sample_count = 
    metaInt(meta_struct, "general.sample_count:");
  meta->general->start_line = 
    metaInt(meta_struct, "general.start_line:");
  meta->general->start_sample = 
    metaInt(meta_struct, "general.start_sample:");
  meta->general->x_pixel_size = 
    metaDouble(meta_struct, "general.x_pixel_size:");
  meta->general->y_pixel_size = 
    metaDouble(meta_struct, "general.y_pixel_size:");
  meta->general->center_latitude = 
    metaDouble(meta_struct, "general.center_latitude:");
  meta->general->center_longitude = 
    metaDouble(meta_struct, "general.center_longitude:");
  meta->general->re_major = 
    metaDouble(meta_struct, "general.re_major:");
  meta->general->re_minor = 
    metaDouble(meta_struct, "general.re_minor:");
  meta->general->bit_error_rate = 
    metaDouble(meta_struct, "general.bit_error_rate:");
  meta->general->missing_lines = 
    metaDouble(meta_struct, "general.missing_lines:");

  /* SAR structure */
  meta->sar->image_type = metaChar(meta_struct, "sar.image_type:");
  meta->sar->look_direction = 
    metaChar(meta_struct, "sar.look_direction:");
  meta->sar->look_count = metaInt(meta_struct, "sar.look_count:");
  meta->sar->deskewed = metaInt(meta_struct, "sar.deskewed:");
  meta->sar->original_line_count = 
    metaInt(meta_struct, "sar.original_line_count:");
  meta->sar->original_sample_count = 
    metaInt(meta_struct, "sar.original_sample_count:");
  meta->sar->line_increment = 
    metaDouble(meta_struct, "sar.line_increment:");
  meta->sar->sample_increment = 
    metaDouble(meta_struct, "sar.sample_increment:");
  meta->sar->range_time_per_pixel = 
    metaDouble(meta_struct, "sar.range_time_per_pixel:");
  meta->sar->azimuth_time_per_pixel = 
    metaDouble(meta_struct, "sar.azimuth_time_per_pixel:");
  meta->sar->slant_range_first_pixel = 
    metaDouble(meta_struct, "sar.slant_range_first_pixel:");
  meta->sar->slant_shift = 
    metaDouble(meta_struct, "sar.slant_shift:");
  meta->sar->time_shift = metaDouble(meta_struct, "sar.time_shift:");
  meta->sar->wavelength = metaDouble(meta_struct, "sar.wavelength:");
  meta->sar->prf = metaDouble(meta_struct, "sar.prf:");
  meta->sar->earth_radius = 
    metaDouble(meta_struct, "sar.earth_radius:");
  meta->sar->satellite_height = 
    metaDouble(meta_struct, "sar.satellite_height:");
  str = metaString(meta_struct, "sar.satellite_binary_time:");
  sprintf(meta->sar->satellite_binary_time, "%s", str);
  str = metaString(meta_struct, "sar.satellite_clock_time:");
  sprintf(meta->sar->satellite_clock_time, "%s", str);
  meta->sar->range_doppler_coefficients[0] = 
    metaDouble(meta_struct, "sar.dopRangeCen:");
  meta->sar->range_doppler_coefficients[1] = 
    metaDouble(meta_struct, "sar.dopRangeLin:");
  meta->sar->range_doppler_coefficients[2] = 
    metaDouble(meta_struct, "sar.dopRangeQuad:");
  meta->sar->azimuth_doppler_coefficients[0] = 
    metaDouble(meta_struct, "sar.dopAzCen:");
  meta->sar->azimuth_doppler_coefficients[1] = 
    metaDouble(meta_struct, "sar.dopAzLin:");
  meta->sar->azimuth_doppler_coefficients[2] = 
    metaDouble(meta_struct, "sar.dopAzQuad:");
  if (strncmp(meta_version, "1.4", 3) == 0) {
    meta->sar->azimuth_processing_bandwidth = 
      metaDouble(meta_struct, "sar.azimuth_bandwidth:");
    meta->sar->chirp_rate =
      metaDouble(meta_struct, "sar.chirp_rate:");
    meta->sar->pulse_duration =
      metaDouble(meta_struct, "sar.pulse_duration:");
    meta->sar->range_sampling_rate =
      metaDouble(meta_struct, "sar.range_samp_rate:");
  }
  
  /* State vector structure */
  vector_count = metaInt(meta_struct, "state.vector_count:");
  meta->state_vectors = asf_meta_state_vectors_init(vector_count);
  meta->state_vectors->year = metaInt(meta_struct, "state.year:");
  meta->state_vectors->julDay = metaInt(meta_struct, "state.julDay:");
  meta->state_vectors->second = metaDouble(meta_struct, "state.second:");
  meta->state_vectors->vector_count = vector_count;
  for (ii=0; ii<vector_count; ii++) {
    sprintf(str, "state.vector[%i].time:", ii);
    meta->state_vectors->vecs[ii].time = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].x:", ii);
    meta->state_vectors->vecs[ii].vec.pos.x = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].y:", ii);
    meta->state_vectors->vecs[ii].vec.pos.y = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].z:", ii);
    meta->state_vectors->vecs[ii].vec.pos.z = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].vx:", ii);
    meta->state_vectors->vecs[ii].vec.vel.x = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].vy:", ii);
    meta->state_vectors->vecs[ii].vec.vel.y = 
      metaDouble(meta_struct, str);
    sprintf(str, "state.vector[%i].vz:", ii);
    meta->state_vectors->vecs[ii].vec.vel.z = 
      metaDouble(meta_struct, str);
  }
  
  /* Projection block */
  if (meta->sar->image_type == 'P') {
    meta->projection = asf_meta_projection_init();
    meta->projection->startX = 
      metaDouble(meta_struct, "projection.startX:");
    meta->projection->startY = 
      metaDouble(meta_struct, "projection.startY:");
    meta->projection->perX = 
      metaDouble(meta_struct, "projection.perX:");
    meta->projection->perY = 
      metaDouble(meta_struct, "projection.perY:");
    str = metaString(meta_struct, "projection.units:");
    sprintf(meta->projection->units, "%s", str);
    meta->projection->hem = 
      metaChar(meta_struct, "projection.hem:");
    spheroid_type_str = metaString(meta_struct, "projection.spheroid:");
    meta->projection->re_major = 
      metaDouble(meta_struct, "projection.re_major:");
    meta->projection->re_minor = 
      metaDouble(meta_struct, "projection.re_minor:");
    if (strncmp(meta_version, "1.3", 3) == 0 ||
	strncmp(meta_version, "1.4", 3) == 0) {
      datum_type_str = metaString(meta_struct, "projection.datum:");
      if (strncmp(datum_type_str, "EGM96_DATUM", 11) == 0)
	meta->projection->datum = EGM96_DATUM;
      else if (strncmp(datum_type_str, "ED50_DATUM", 10) == 0)
	meta->projection->datum = ED50_DATUM;
      else if (strncmp(datum_type_str, "ETRF89_DATUM", 12) == 0)
	meta->projection->datum = ETRF89_DATUM;
      else if (strncmp(datum_type_str, "ETRS89_DATUM", 12) == 0)
	meta->projection->datum = ETRS89_DATUM;
      else if (strncmp(datum_type_str, "ITRF_DATUM", 10) == 0)
	meta->projection->datum = ITRF_DATUM;
      else if (strncmp(datum_type_str, "NAD27_DATUM", 11) == 0)
	meta->projection->datum = NAD27_DATUM;
      else if (strncmp(datum_type_str, "NAD83_DATUM", 11) == 0)
	meta->projection->datum = NAD83_DATUM;
      else if (strncmp(datum_type_str, "WGS72_DATUM", 11) == 0)
	meta->projection->datum = WGS72_DATUM;
      else if (strncmp(datum_type_str, "WGS84_DATUM", 11) == 0)
	meta->projection->datum = WGS84_DATUM;
    } 
    projection_type_str = metaString(meta_struct, "projection.type:");
    if (strncmp(meta_version, "1.1", 3) == 0) {
      if (strncmp(projection_type_str, "A", 1) == 0)
	meta->projection->type = SCANSAR_PROJECTION;
      else if (strncmp(projection_type_str, "P", 1) == 0)
	meta->projection->type = POLAR_STEREOGRAPHIC;
      else if (strncmp(projection_type_str, "L", 1) == 0)
	meta->projection->type = LAMBERT_CONFORMAL_CONIC;
      else if (strncmp(projection_type_str, "U", 1) == 0)
	meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
    }
    else {
      if (strncmp(projection_type_str, "UNIVERSAL_TRANSVERSE_MERCATOR", 29) == 0)
	meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      if (strncmp(projection_type_str, "POLAR_STEREOGRAPHIC", 19) == 0)
	meta->projection->type = POLAR_STEREOGRAPHIC;
      if (strncmp(projection_type_str, "ALBERS_EQUAL_AREA", 17) == 0)
	meta->projection->type = ALBERS_EQUAL_AREA;
      if (strncmp(projection_type_str, "LAMBERT_CONFORMAL_CONIC", 23) == 0)
	meta->projection->type = LAMBERT_CONFORMAL_CONIC;
      if (strncmp(projection_type_str, "LAMBERT_AZIMUTHAL_EQUAL_AREA", 28) == 0)
	meta->projection->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
      if (strncmp(projection_type_str, "STATE_PLANE", 11) == 0)
	meta->projection->type = STATE_PLANE;
      if (strncmp(projection_type_str, "SCANSAR_PROJECTION", 18) == 0)
	meta->projection->type = SCANSAR_PROJECTION;
    }
    switch (meta->projection->type) 
      {
      case SCANSAR_PROJECTION:
	meta->projection->param.atct.rlocal = 
	  metaDouble(meta_struct, "projection.param.atct.rlocal:");
	meta->projection->param.atct.alpha1 =
	  metaDouble(meta_struct, "projection.param.atct.alpha1:");
	meta->projection->param.atct.alpha2 =
	  metaDouble(meta_struct, "projection.param.atct.alpha3:");
	meta->projection->param.atct.alpha3 =
	  metaDouble(meta_struct, "projection.param.atct.alpha3:");
	break;
      case ALBERS_EQUAL_AREA:
	meta->projection->param.albers.std_parallel1 =
	  metaDouble(meta_struct, "projection.param.albers.std_parallel1:");
	meta->projection->param.albers.std_parallel2 =
	  metaDouble(meta_struct, "projection.param.albers.std_parallel2:");
	meta->projection->param.albers.center_meridian =
	  metaDouble(meta_struct, "projection.param.albers.center_meridian:");
	meta->projection->param.albers.orig_latitude =
	  metaDouble(meta_struct, "projection.param.albers.orig_latitude:");
	if (strncmp(meta_version, "1.3", 3) == 0 ||
	    strncmp(meta_version, "1.4", 3) == 0) {
	  meta->projection->param.albers.false_easting =
	    metaDouble(meta_struct, "projection.param.albers.false_easting:");
	  meta->projection->param.albers.false_northing =
	    metaDouble(meta_struct, "projection.param.albers.false_northing:");
	}
	break;
      case LAMBERT_AZIMUTHAL_EQUAL_AREA:
	meta->projection->param.lamaz.center_lon = 
	  metaDouble(meta_struct,"projection.param.lamaz.center_lon:");
	meta->projection->param.lamaz.center_lat =
	  metaDouble(meta_struct,"projection.param.lamaz.center_lat:");
	if (strncmp(meta_version, "1.3", 3) == 0 ||
	    strncmp(meta_version, "1.4", 3) == 0) {
	  meta->projection->param.lamaz.false_easting =
	    metaDouble(meta_struct, "projection.param.lamaz.false_easting:");
	  meta->projection->param.lamaz.false_northing = 
	    metaDouble(meta_struct, "projection.param.lamaz.false_northing:");
	}
	break;
      case LAMBERT_CONFORMAL_CONIC:
	meta->projection->param.lamcc.plat1 =
	  metaDouble(meta_struct, "projection.param.lamcc.plat1:");
	meta->projection->param.lamcc.plat2 =
	  metaDouble(meta_struct, "projection.param.lamcc.plat2:");
	meta->projection->param.lamcc.lat0 =
	  metaDouble(meta_struct, "projection.param.lamcc.lat0:");
	meta->projection->param.lamcc.lon0 =
	  metaDouble(meta_struct, "projection.param.lamcc.lon0:");
	if (strncmp(meta_version, "1.3", 3) == 0 ||
	    strncmp(meta_version, "1.4", 3) == 0) {
	  meta->projection->param.lamcc.false_easting =
	    metaDouble(meta_struct, "projection.param.lamcc.false_easting:");
	  meta->projection->param.lamcc.false_northing =
	    metaDouble(meta_struct, "projection.param.lamcc.false_northing:");
	  meta->projection->param.lamcc.scale_factor =
	    metaDouble(meta_struct, "projection.param.lamcc.scale_factor:");
	}
	break;
      case POLAR_STEREOGRAPHIC:/* Polar stereographic projection. */
	meta->projection->param.ps.slat =
	  metaDouble(meta_struct, "projection.param.ps.slat:");
	meta->projection->param.ps.slon = 
	  metaDouble(meta_struct, "projection.param.ps.slon:");
	if (strncmp(meta_version, "1.3", 3) == 0 ||
	    strncmp(meta_version, "1.4", 3) == 0) {
	  meta->projection->param.ps.false_easting =
	    metaDouble(meta_struct, "projection.param.ps.false_easting:");
	  meta->projection->param.ps.false_northing =
	    metaDouble(meta_struct, "projection.param.ps.false_northing:");
	}
	break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:
	meta->projection->param.utm.zone =
	  metaInt(meta_struct, "projection.param.utm.zone:");
	if (strncmp(meta_version, "1.3", 3) == 0 ||
	    strncmp(meta_version, "1.4", 3) == 0) {
	  meta->projection->param.utm.false_easting =
	    metaDouble(meta_struct,"projection.param.utm.false_easting:");
	  meta->projection->param.utm.false_northing =
	    metaDouble(meta_struct, "projection.param.utm.false_northing:");
	  meta->projection->param.utm.lat0 =
	    metaDouble(meta_struct, "projection.param.utm.latitude:");
	  meta->projection->param.utm.lon0 =
	    metaDouble(meta_struct, "projection.param.utm.longitude:");
	  meta->projection->param.utm.scale_factor =
	    metaDouble(meta_struct, "projection.param.utm.scale_factor:");
	}
	break;
      case STATE_PLANE:/* State plane coordinates projection. */
	meta->projection->param.state.zone =
	  metaInt(meta_struct, "projection.param.state.zone:");
	break;
      }
  }
  
  /* Statistics structure */
  stats = metaDouble(meta_struct, "stats:");
  if (stats) {
    meta->stats = asf_meta_stats_init();
    meta->stats->min = metaDouble(meta_struct, "stats.min:");
    meta->stats->max = metaDouble(meta_struct, "stats.max:");
    meta->stats->mean = metaDouble(meta_struct, "stats.mean:");
    meta->stats->rmse = metaDouble(meta_struct, "stats.rmse:");
    meta->stats->std_deviation = 
      metaDouble(meta_struct, "stats.std_deviation:");
    if (strncmp(meta_version, "1.2", 3) == 0 ||
	strncmp(meta_version, "1.3", 3) == 0 ||
	strncmp(meta_version, "1.4", 3) == 0)
      meta->stats->mask = metaDouble(meta_struct, "stats.mask:");
  }
  
  return meta;

}
