#include "asf_vector.h"

meta_parameters *meta2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords)
{
  // Read header information
  dbf_header_t *header;
  int n;
  char shape_type[25];
  read_header_config("META", &header, &n, shape_type);
  dbf_header_t *values = (dbf_header_t *) MALLOC(sizeof(dbf_header_t)*n);
  
  // Read metadata
  char *error;
  meta_parameters *meta = NULL;
  terrasar_meta *terrasar = NULL;
  radarsat2_meta *radarsat2 = NULL;
  if (isgeotiff(inFile)) {
      int i, ignore[MAX_BANDS];
      for (i=0; i<MAX_BANDS; i++) ignore[i] = 0; // Default to ignoring no bands
      meta = read_generic_geotiff_metadata(inFile, ignore, NULL);
  }
  else if (isterrasar(inFile)) {
    terrasar = read_terrasar_meta(inFile);
    meta = terrasar2meta(terrasar);
    FREE(terrasar);
  }
  else if (isRadarsat2_ext(inFile, FALSE, &error)) {
    radarsat2 = read_radarsat2_meta_ext(inFile, FALSE);
    meta = radarsat2meta(radarsat2);
    FREE(radarsat2);
  }
  else if (isleader(inFile)) {
    ceos_description *ceos = 
      get_ceos_description_ext(inFile, REPORT_LEVEL_NONE, FALSE);
    if (ceos->product == RAW)
      meta = meta_read_raw(inFile);
    else
      meta = meta_read_only(inFile);
  }
  else
    meta = meta_read(inFile);

  // Assign values
  int ii, m = 0;
  for (ii=0; ii<n; ii++) 
  {
    // General block
    if (strcmp_case(header[ii].meta, "meta.general.basename") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(meta->general->basename);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.sensor") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(meta->general->sensor);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.sensor_name") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(meta->general->sensor_name);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.mode") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(meta->general->mode);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.receiving_station") == 0 
      && meta->general) {
      header[ii].sValue = STRDUP(meta->general->receiving_station);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.processor") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(meta->general->processor);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.data_type") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(data_type2str(meta->general->data_type));
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.image_data_type") == 0 &&
      meta->general) {
      header[ii].sValue = 
        STRDUP(image_data_type2str(meta->general->image_data_type));
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.radiometry") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(radiometry2str(meta->general->radiometry));
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.general.acquisition_date") == 0 && meta->general) {
      header[ii].sValue = STRDUP(meta->general->acquisition_date);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.orbit") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->orbit;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.orbit_direction") == 0 &&
      meta->general) {
      if (meta->general->orbit_direction == 'A')
        header[ii].sValue = STRDUP("ascending");
      else if (meta->general->orbit_direction == 'D')
        header[ii].sValue = STRDUP("descending");
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.frame") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->frame;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.band_count") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->band_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.bands") == 0 &&
      meta->general) {
      header[ii].sValue = STRDUP(meta->general->bands);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.line_count") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->line_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.sample_count") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->sample_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.start_line") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->start_line;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.start_sample") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->start_sample;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.x_pixel_size") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->x_pixel_size;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.y_pixel_size") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->y_pixel_size;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.center_latitude") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->center_latitude;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.general.center_longitude") == 0 && meta->general) {
      header[ii].fValue = meta->general->center_longitude;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.re_major") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->re_major;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.re_minor") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->re_minor;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.bit_error_rate") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->bit_error_rate;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.missing_lines") == 0 &&
      meta->general) {
      header[ii].nValue = meta->general->missing_lines;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.general.no_data") == 0 &&
      meta->general) {
      header[ii].fValue = meta->general->no_data;
      values[m++] = header[ii];
    }

    // SAR block
    if (strcmp_case(header[ii].meta, "meta.sar.image_type") == 0 && meta->sar) {
      if (meta->sar->image_type == 'S')
        header[ii].sValue = STRDUP("slant range");
      else if (meta->sar->image_type == 'G')
        header[ii].sValue = STRDUP("ground range");
      else if (meta->sar->image_type == 'P')
        header[ii].sValue = STRDUP("map projected");
      else if (meta->sar->image_type == 'R')
        header[ii].sValue = STRDUP("georeferenced");
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.look_direction") == 0 &&
      meta->sar) {
      if (meta->sar->look_direction == 'R')
        header[ii].sValue = STRDUP("right");
      else if (meta->sar->look_direction == 'L')
        header[ii].sValue = STRDUP("left");
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.azimuth_look_count") == 0 &&
      meta->sar) {
      header[ii].nValue = meta->sar->azimuth_look_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.range_look_count") == 0 &&
      meta->sar) {
      header[ii].nValue = meta->sar->range_look_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.deskewed") == 0 && 
      meta->sar) {
      header[ii].nValue = meta->sar->deskewed;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.original_line_count") == 0 &&
      meta->sar) {
      header[ii].nValue = meta->sar->original_line_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.original_sample_count") == 0 &&
      meta->sar) {
      header[ii].nValue = meta->sar->original_sample_count;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.line_increment") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->line_increment;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.sample_increment") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->sample_increment;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.range_time_per_pixel") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->range_time_per_pixel;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.azimuth_time_per_pixel") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->azimuth_time_per_pixel;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.slant_shift") == 0 && 
      meta->sar) {
      header[ii].fValue = meta->sar->slant_shift;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.time_shift") == 0 && 
      meta->sar) {
      header[ii].fValue = meta->sar->time_shift;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.slant_range_first_pixel") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->slant_range_first_pixel;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.wavelength") == 0 && 
      meta->sar) {
      header[ii].fValue = meta->sar->wavelength;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.prf") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->prf;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.earth_radius") == 0 && 
      meta->sar) {
      header[ii].fValue = meta->sar->earth_radius;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.earth_radius_pp") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->earth_radius_pp;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.satellite_height") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->satellite_height;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.satellite_binary_time") == 0 &&
      meta->sar) {
      header[ii].sValue = STRDUP(meta->sar->satellite_binary_time);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.satellite_clock_time") == 0 &&
      meta->sar) {
      header[ii].sValue = STRDUP(meta->sar->satellite_clock_time);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.range_doppler_coefficients[0]") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->range_doppler_coefficients[0];
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.range_doppler_coefficients[1]") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->range_doppler_coefficients[1];
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.range_doppler_coefficients[2]") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->range_doppler_coefficients[2];
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.azimuth_doppler_coefficients[0]") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->azimuth_doppler_coefficients[0];
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.azimuth_doppler_coefficients[1]") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->azimuth_doppler_coefficients[1];
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.azimuth_doppler_coefficients[2]") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->azimuth_doppler_coefficients[2];
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, 
      "meta.sar.azimuth_processing_bandwidth") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->azimuth_processing_bandwidth;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.chirp_rate") == 0 && 
      meta->sar) {
      header[ii].fValue = meta->sar->chirp_rate;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.pulse_duration") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->pulse_duration;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.range_sampling_rate") == 0 &&
      meta->sar) {
      header[ii].fValue = meta->sar->range_sampling_rate;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.polarization") == 0 && 
      meta->sar) {
      header[ii].sValue = STRDUP(meta->sar->polarization);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.multilook") == 0 && 
      meta->sar) {
      header[ii].nValue = meta->sar->multilook;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.pitch") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->pitch;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.roll") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->roll;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.sar.yaw") == 0 && meta->sar) {
      header[ii].fValue = meta->sar->yaw;
      values[m++] = header[ii];
    }

    // Optical block
    if (strcmp_case(header[ii].meta, "meta.optical.pointing_direction") == 0 &&
      meta->optical) {
      header[ii].sValue = STRDUP(meta->optical->pointing_direction);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.optical.off_nadir_angle") == 0 &&
      meta->optical) {
      header[ii].fValue = meta->optical->off_nadir_angle;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.optical.correction_level") == 0 &&
      meta->optical) {
      header[ii].sValue = STRDUP(meta->optical->correction_level);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.optical.cloud_percentage") == 0 &&
      meta->optical) {
      header[ii].fValue = meta->optical->cloud_percentage;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.optical.sun_azimuth_angle") == 0 &&
      meta->optical) {
      header[ii].fValue = meta->optical->sun_azimuth_angle;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.optical.sun_elevation_angle") == 0 &&
      meta->optical) {
      header[ii].fValue = meta->optical->sun_elevation_angle;
      values[m++] = header[ii];
    }

    // AirSAR block
    if (strcmp_case(header[ii].meta, "meta.airsar.scale_factor") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->scale_factor;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.airsar.gps_altitude") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->gps_altitude;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.airsar.lat_peg_point") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->lat_peg_point;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.airsar.lon_peg_point") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->lon_peg_point;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.airsar.head_peg_point") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->head_peg_point;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.airsar.along_track_offset") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->along_track_offset;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.airsar.cross_track_offset") == 0 &&
      meta->airsar) {
      header[ii].fValue = meta->airsar->cross_track_offset;
      values[m++] = header[ii];
    }

    // UAVSAR block
    if (strcmp_case(header[ii].meta, "meta.uavsar.id") == 0 &&
      meta->uavsar) {
      header[ii].sValue = STRDUP(meta->uavsar->id);
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.scale_factor") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->scale_factor;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.gps_altitude") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->gps_altitude;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.lat_peg_point") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->lat_peg_point;
       values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.lon_peg_point") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->lon_peg_point;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.head_peg_point") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->head_peg_point;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.along_track_offset") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->along_track_offset;
      values[m++] = header[ii];
    }
    else if (strcmp_case(header[ii].meta, "meta.uavsar.cross_track_offset") == 0 &&
      meta->uavsar) {
      header[ii].fValue = meta->uavsar->cross_track_offset;
      values[m++] = header[ii];
    }

    // Map projection block
    if (meta->projection && meta->projection->type != SCANSAR_PROJECTION) {
      if (strcmp_case(header[ii].meta, "meta.projection") == 0) {
        header[ii].sValue = STRDUP(meta2esri_proj(meta, NULL));
        values[m++] = header[ii];
      }
    }

    // Location block
    if (strcmp_case(header[ii].meta, 
      "meta.location.lat_start_near_range") == 0 && meta->location) {
      header[ii].fValue = meta->location->lat_start_near_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, 
      "meta.location.lon_start_near_range") == 0 && meta->location) {
      header[ii].fValue = meta->location->lon_start_near_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, "meta.location.lat_start_far_range") == 0
      && meta->location) {
      header[ii].fValue = meta->location->lat_start_far_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, "meta.location.lon_start_far_range") == 0
      && meta->location) {
      header[ii].fValue = meta->location->lon_start_far_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, "meta.location.lat_end_near_range") == 0
      && meta->location) {
      header[ii].fValue = meta->location->lat_end_near_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, "meta.location.lon_end_near_range") == 0
      && meta->location) {
      header[ii].fValue = meta->location->lon_end_near_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, "meta.location.lat_end_far_range") == 0
      && meta->location) {
      header[ii].fValue = meta->location->lat_end_far_range;
      values[m++] = header[ii];
    }
    if (strcmp_case(header[ii].meta, "meta.location.lon_end_far_range") == 0
      && meta->location) {
      header[ii].fValue = meta->location->lon_end_far_range;
      values[m++] = header[ii];
    }
  }

  // Determine corner coordinates
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  double *lat = (double *) MALLOC(sizeof(double)*5);
  double *lon = (double *) MALLOC(sizeof(double)*5);

  if (meta->location) {
    lat[0] = meta->location->lat_start_near_range;
    lon[0] = meta->location->lon_start_near_range;
    lat[3] = meta->location->lat_start_far_range;
    lon[3] = meta->location->lon_start_far_range;
    lat[1] = meta->location->lat_end_near_range;
    lon[1] = meta->location->lon_end_near_range;
    lat[2] = meta->location->lat_end_far_range;
    lon[2] = meta->location->lon_end_far_range;
  }
  else {
    meta_get_latLon(meta, 0, 0, 0.0, &lat[0], &lon[0]);
    meta_get_latLon(meta, 0, ns, 0.0, &lat[1], &lon[1]);
    meta_get_latLon(meta, nl, ns, 0.0, &lat[2], &lon[2]);
    meta_get_latLon(meta, nl, 0, 0.0, &lat[3], &lon[3]);
  }
  lat[4] = lat[0];
  lon[4] = lon[0];
  
  *dbf = values;
  *nAttr = m;
  *latArray = lat;
  *lonArray = lon;
  *nCoords = 5;
  FREE(header);
  
  return meta;
}

void geotiff2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords)
{
  // Read header information
  dbf_header_t *header;
  int n;
  char shape_type[25];
  read_header_config("GEOTIFF", &header, &n, shape_type);

  // Read parameter out of TIFF structure
  data_type_t data_type;
  short num_bands;
  short int sample_format, bits_per_sample, planar_config;
  int ignore[MAX_BANDS], is_scanline_format, is_palette_color_tiff;
  TIFF *input_tiff = XTIFFOpen(inFile, "r");
  get_tiff_data_config(input_tiff, &sample_format, &bits_per_sample,
                       &planar_config, &data_type, &num_bands,
                       &is_scanline_format, &is_palette_color_tiff, 
                       REPORT_LEVEL_NONE);
  XTIFFClose(input_tiff);
  
  // Read generic metadata to get geolocation
  double *lat = (double *) MALLOC(sizeof(double)*5);
  double *lon = (double *) MALLOC(sizeof(double)*5);
  int ii;
  for (ii=0; ii<MAX_BANDS; ii++) 
    ignore[ii] = 0; // Default to ignoring no bands
  meta_parameters *meta = read_generic_geotiff_metadata(inFile, ignore, NULL);
  if (meta && meta->location) {
    meta_location *ml = meta->location; // Convenience pointer
    lon[0] = ml->lon_start_near_range;
    lat[0] = ml->lat_start_near_range;
    lon[1] = ml->lon_start_far_range;
    lat[1] = ml->lat_start_far_range;
    lon[2] = ml->lon_end_far_range;
    lat[2] = ml->lat_end_far_range;
    lon[3] = ml->lon_end_near_range;
    lat[3] = ml->lat_end_near_range;
    lon[4] = lon[0];
    lat[4] = lat[0];
  }
  else
    asfPrintError("File (%s) contains no geolocation information\n", inFile);
  double startX = meta->projection->startX;
  double startY = meta->projection->startY;
  double endX = startX + meta->general->sample_count*meta->projection->perX;
  double endY = startY + meta->general->line_count*meta->projection->perY;
  meta_free(meta);

  // Assign values
  for (ii=0; ii<n; ii++) {
    if (strcmp_case(header[ii].meta, "ID") == 0)
      header[ii].sValue = STRDUP(inFile);
    else if (strcmp_case(header[ii].meta, "format") == 0) {
      if (sample_format == SAMPLEFORMAT_UINT)
        header[ii].sValue = STRDUP("unsigned integer");
      else if (sample_format == SAMPLEFORMAT_INT)
        header[ii].sValue = STRDUP("signed integer");
      else if (sample_format == SAMPLEFORMAT_IEEEFP)
        header[ii].sValue = STRDUP("floating point");
    }
    else if (strcmp_case(header[ii].meta, "bits_samples") == 0)
      header[ii].nValue = (int) bits_per_sample;
    else if (strcmp_case(header[ii].meta, "planes") == 0) {
      if (planar_config == PLANARCONFIG_CONTIG)
        header[ii].sValue = STRDUP("contiguous interlaced");
      else if (planar_config == PLANARCONFIG_SEPARATE)
        header[ii].sValue = STRDUP("separate planes");
    }
    else if (strcmp_case(header[ii].meta, "band_count") == 0)
      header[ii].nValue = (int) num_bands;
    else if (strcmp_case(header[ii].meta, "upper_left_x") == 0)
      header[ii].fValue = startX;
    else if (strcmp_case(header[ii].meta, "upper_left_y") == 0)
      header[ii].fValue = startY;
    else if (strcmp_case(header[ii].meta, "lower_right_x") == 0)
      header[ii].fValue = endX;
    else if (strcmp_case(header[ii].meta, "lower_right_y") == 0)
      header[ii].fValue = endY;
  }

  *dbf = header;  
  *nAttr = n;
  *latArray = lat;
  *lonArray = lon;
  *nCoords = 5;
}

void polygon2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords)
{
  // Read header file
  dbf_header_t *header;
  char shape_type[25];
  int n;
  read_header_config("POLYGON", &header, &n, shape_type);
  n = 1;
  
  // Figure out how many vertices we got
  char line[1024];
  int nVertices = 0;
  FILE *fpIn = FOPEN(inFile, "r");
  while (fgets(line, 1024, fpIn))
    nVertices++;
  FCLOSE(fpIn);
  
  // Read polygon information
  double *lat = (double *) MALLOC(sizeof(double)*nVertices);
  double *lon = (double *) MALLOC(sizeof(double)*nVertices); 
  fgets(line, 1024, fpIn); // header line
  nVertices = 0;
  while (fgets(line, 1024, fpIn)) {
    chomp(line);
    lat[nVertices] = atof(get_str(line, 1));
    lon[nVertices] = atof(get_str(line, 2));
    nVertices++;
  }
  lat[n] = lat[0];
  lon[n] = lon[0];
  
  // Assign values
  header[0].sValue = STRDUP(inFile);

  *dbf = header;  
  *nAttr = n;
  *latArray = lat;
  *lonArray = lon;
  *nCoords = nVertices;

}

static void check_smap_file(char *inDataName, int *line_count, int *sample_count)
{
  hsize_t dims[2];
  hid_t file, group, dataset, dataspace;

  // Check whether the file is actually a real HDF5 data set
  if (!H5Fis_hdf5(inDataName))
    asfPrintError("File (%s) is not in HDF5 format!\n", inDataName);

  // Check dimensions
  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  dataspace = H5Dget_space(dataset);
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  H5Dclose(dataset);
  H5Sclose(dataspace);
  H5Gclose(group);
  H5Fclose(file);

  *line_count = dims[0];
  *sample_count = dims[1];
}

static void read_smap_outline(char *inDataName, int *vertex_count, 
			      double *lat, double *lon)
{
  hsize_t dims[2], count[2], offset[2], pixels[1];
  hid_t file, group, dataset, datatype, dataspace, memspace;

  file = H5Fopen(inDataName, H5F_ACC_RDONLY, H5P_DEFAULT);
  group = H5Gopen(file, "Sigma0_Data", H5P_DEFAULT);

  // Look up latitude
  dataset = H5Dopen(group, "cell_lat", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  H5Sget_simple_extent_dims(dataspace, dims, NULL);
  int nl = dims[0];
  int ns = dims[1];

  // Determine dimensions
  offset[0] = 0;
  offset[1] = 0;
  count[0] = nl;
  count[1] = ns;
  pixels[0] = nl*ns;
  int ii, kk, counts=0;
  float *values = (float *) MALLOC(sizeof(float)*ns*nl);

  // Read latitude
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  memspace = H5Screate_simple(1, pixels, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  
  // First line
  kk = 0;
  for (ii=0; ii<ns; ii++) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lat[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Right boundary
  for (kk=1; kk<nl-1; kk++) {
    ii = ns - 1;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii--;
    lat[counts] = (double) values[kk*ns+ii];
    counts++;
  }
  // Last line
  kk = nl - 1;
  for (ii=ns-1; ii>0; ii--) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lat[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Left boundary
  for (kk=nl-1; kk>0; kk--) {
    ii = 0;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii++;
    lat[counts] = (double) values[kk*ns+ii];
    counts++;
  }

  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  // Read longitudes
  counts = 0;
  dataset = H5Dopen(group, "cell_lon", H5P_DEFAULT);
  datatype = H5Dget_type(dataset);
  dataspace = H5Dget_space(dataset);
  offset[0] = 0;
  H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL, count, NULL);
  H5Dread(dataset, H5T_NATIVE_FLOAT, memspace, dataspace, H5P_DEFAULT, values);
  
  // First line
  kk = 0;
  for (ii=0; ii<ns; ii++) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lon[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Right boundary
  for (kk=1; kk<nl-1; kk++) {
    ii = ns - 1;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii--;
    lon[counts] = (double) values[kk*ns+ii];
    counts++;
  }
  // Last line
  kk = nl - 1;
  for (ii=ns-1; ii>0; ii--) {
    if (meta_is_valid_double(values[kk*ns+ii])) {
      lon[counts] = (double) values[kk*ns+ii];
      counts++;
    }
  }
  // Left boundary
  for (kk=nl-1; kk>0; kk--) {
    ii = 0;
    while (!meta_is_valid_double(values[kk*ns+ii]))
      ii++;
    lon[counts] = (double) values[kk*ns+ii];
    counts++;
  }

  H5Tclose(datatype);
  H5Dclose(dataset);
  H5Sclose(dataspace);

  H5Sclose(memspace);
  H5Gclose(group);
  H5Fclose(file);
  
	FREE(values);

  *vertex_count = counts;
}

void smap2vector(char *inFile, dbf_header_t **dbf, int *nAttr, 
  double **latArray, double **lonArray, int *nCoords)
{
  // Read header information
  dbf_header_t *header;
  int n;
  char shape_type[25];
  read_header_config("SMAP", &header, &n, shape_type);

  // Assign values
  smap_meta *smap = read_smap_meta(inFile);	
  int ii;
  for (ii=0; ii<n; ii++) {
    if (strcmp_case(header[ii].meta, "file_name") == 0)
      header[ii].sValue = STRDUP(smap->file_name);
    else if (strcmp_case(header[ii].meta, "start_time") == 0)
      header[ii].sValue = STRDUP(smap->orbit_start_date_time);
    else if (strcmp_case(header[ii].meta, "stop_time") == 0)
      header[ii].sValue = STRDUP(smap->orbit_stop_date_time);
    else if (strcmp_case(header[ii].meta, "direction") == 0)
      header[ii].sValue = STRDUP(smap->orbit_direction);
  }
  FREE(smap);

  // Read lat/lon for boundary of SMAP data set
  double *lat, *lon;
  int line_count, sample_count, vertex_count;
  check_smap_file(inFile, &line_count, &sample_count);
  lat = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  lon = (double *) MALLOC(sizeof(double)*(line_count+sample_count)*2+1);
  read_smap_outline(inFile, &vertex_count, lat, lon);  
  lat[vertex_count] = lat[0];
  lon[vertex_count] = lon[0];

  *dbf = header;  
  *nAttr = n;
  *latArray = lat;
  *lonArray = lon;
  *nCoords = vertex_count+1;
}
