#include "asf.h"
#include "shapefil.h"
#include "asf_vector.h"
#include "asf_raster.h"
#include "terrasar.h"

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
  meta_parameters *meta = NULL;
  terrasar_meta *terrasar = NULL;
  if (isleader(inFile)) {
    ceos_description *ceos = 
      get_ceos_description_ext(inFile, REPORT_LEVEL_NONE, FALSE);
    if (ceos->product == RAW)
      meta = meta_read_raw(inFile);
    else
      meta = meta_read_only(inFile);
  }
  else if (isparfile(inFile)) {
    meta = meta_read_stf(inFile);
  }
  else if (isterrasar(inFile)) {
    terrasar = read_terrasar_meta(inFile);
    meta = terrasar2meta(terrasar);
    FREE(terrasar);
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

  if (meta->projection) {
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;
    double xPix = meta->projection->perX;
    double yPix = fabs(meta->projection->perY);
    double startX = meta->projection->startX;
    double startY = meta->projection->startY;
    lon[0] = startX;
    lat[0] = startY;
    lon[1] = startX + ns*xPix;
    lat[1] = startY;
    lon[2] = startX + ns*xPix;
    lat[2] = startY + nl*yPix;
    lon[3] = startX;
    lat[3] = startY + nl*yPix;
  }
  else if (meta->location) {
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

/*
// Some of the functionality here is used to convert CEOS leader files as well
// as metadata into the various. When comparing the results from the two
// sources you will find a few differences.
//
// The following parameters can vary depending on the ingest parameters:
// data_type
// image_data_type
// bands
// earth_radius_pp
//
// All these are not deal with in particular within 'meta_init_ceos' but in
// 'import_ceos' instead.

void shape_meta_init(char *inFile, meta_parameters *meta)
{
  char *dbaseFile, shape_type[25];
  DBFHandle dbase;
  SHPHandle shape;
  dbf_header_t *dbf;
  int ii, nCols, length;

  // Read configuration file
  read_header_config("META", &dbf, &nCols, shape_type);

  // Open database for initialization
  dbaseFile = appendExt(inFile, ".dbf");
  dbase = DBFCreate(dbaseFile);
  if (!dbase)
    asfPrintError("Could not create database file '%s'\n", dbaseFile);

  // Add fields to database
  for (ii=0; ii<nCols; ii++) {
    if (strncmp(dbf[ii].meta, "meta.general.basename", 21) == 0 &&
        dbf[ii].visible) {
      length = strlen(meta->general->basename);
      if (DBFAddField(dbase, "Basename", FTString, length, 0) == -1)
        asfPrintError("Could not add basename field to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.sensor", 19) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Sensor", FTString, 15, 0) == -1)
        asfPrintError("Could not add sensor field to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.sensor_name", 24) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Name", FTString, 15, 0) == -1)
        asfPrintError("Could not add sensor name to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.mode", 17) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Mode", FTString, 15, 0) == -1)
        asfPrintError("Could not add mode to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.processor", 22) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Processor", FTString, 25, 0) == -1)
        asfPrintError("Could not add processor to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.data_type", 22) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Data_type", FTString, 25, 0) == -1)
        asfPrintError("Could not add data type to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.image_data_type", 27) == 0 
	     && dbf[ii].visible) {
      if (DBFAddField(dbase, "Img_data_t", FTString, 25, 0) == -1)
        asfPrintError("Could not add image data type to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.radiometry", 22) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Radiometry", FTString, 15, 0) == -1)
        asfPrintError("Could not add radiometry to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.acquisition_date", 29) == 0 
	     && dbf[ii].visible) {
      if (DBFAddField(dbase, "Acq_date", FTString, 25, 0) == -1)
        asfPrintError("Could not add acquisition date to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.orbit_direction", 27) == 0 
	     && dbf[ii].visible) {
      if (DBFAddField(dbase, "Direction", FTString, 20, 0) == -1)
        asfPrintError("Could not add orbit direction to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.orbit", 17) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Orbit", FTInteger, 7, 0) == -1)
        asfPrintError("Could not add orbit to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.frame", 18) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Frame", FTInteger, 5, 0) == -1)
        asfPrintError("Could not add processor to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.band_count", 22) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Band_count", FTInteger, 3, 0) == -1)
        asfPrintError("Could not add band count to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.bands", 17) == 0 &&
             dbf[ii].visible) {
      length = strlen(meta->general->bands) + 1;
      if (DBFAddField(dbase, "Bands", FTString, length, 0) == -1)
        asfPrintError("Could not add bands to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.line_count", 22) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Lines", FTInteger, 7, 0) == -1)
        asfPrintError("Could not add line count to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.sample_count", 24) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Samples", FTInteger, 7, 0) == -1)
        asfPrintError("Could not add sample count to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.start_line", 22) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Start_line", FTInteger, 7, 0) == -1)
        asfPrintError("Could not add start line to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.start_sample", 24) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Start_sample", FTInteger, 7, 0) == -1)
        asfPrintError("Could not add start sample to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.x_pixel_size", 24) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "X_pix_size", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add x pixel size to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.y_pixel_size", 24) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Y_pix_size", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add y pixel size to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.center_latitude", 27) == 0 
	     && dbf[ii].visible) {
      if (DBFAddField(dbase, "Center_lat", FTDouble, 9, 4) == -1)
        asfPrintError("Could not add center latitude to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.center_longitude", 28) == 0 
	     && dbf[ii].visible) {
      if (DBFAddField(dbase, "Center_lon", FTDouble, 9, 4) == -1)
        asfPrintError("Could not add center longitude to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.re_major", 20) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "RE_major", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add RE major to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.re_minor", 20) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "RE major", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add RE minor to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.bit_error_rate", 26) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "BER", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add bit error rate to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.missing_lines", 25) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "Miss_lines", FTInteger, 7, 0) == -1)
        asfPrintError("Could not add missing lines to database file\n");
    }
    else if (strncmp(dbf[ii].meta, "meta.general.no_data", 19) == 0 &&
             dbf[ii].visible) {
      if (DBFAddField(dbase, "No_data", FTDouble, 16, 7) == -1)
        asfPrintError("Could not add no data to database file\n");
    }
    if (meta->sar) {
      int kk;
      char header[12];
      if (strncmp(dbf[ii].meta, "meta.sar.image_type", 19) == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Image_type", FTString, 25, 0) == -1)
          asfPrintError("Could not add image type to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.look_direction", 23) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Look_dir", FTString, 25, 0) == -1)
          asfPrintError("Could not add look direction to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.look_count", 19) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Looks", FTInteger, 3, 0) == -1)
          asfPrintError("Could not add look count field to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.deskewed", 17) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Deskewed", FTInteger, 1, 0) == -1)
          asfPrintError("Could not add deskewed field to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.original_line_count", 28) == 0
	       && dbf[ii].visible) {
        if (DBFAddField(dbase, "Org_lines", FTInteger, 7, 0) == -1)
          asfPrintError("Could not add original lines to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.original_sample_count", 30) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Org_samples", FTInteger, 7, 0) == -1)
          asfPrintError("Could not add original samples to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.line_increment", 23) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Line_inc", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add line increment to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.sample_increment", 25) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Sample_inc", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add sample increment to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.range_time_per_pixel", 29) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Rng_t_pix", FTDouble, 16, 12) == -1)
          asfPrintError("Could not add range time per pix to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.azimuth_time_per_pixel", 31) == 0
               && dbf[ii].visible) {
        if (DBFAddField(dbase, "Az_t_pix", FTDouble, 16, 12) == -1)
          asfPrintError("Could not add azimuth time pixel to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.slant_range_first_pixel", 32) == 0
               && dbf[ii].visible) {
        if (DBFAddField(dbase, "Slnt_range", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add slant range 1. pix to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.slant_shift", 20) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Slnt_shift", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add slant shift to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.time_shift", 19) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Time_shift", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add time shift to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.wavelength", 19) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Wavelength", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add wavelength to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.prf", 12) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "PRF", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add PRF to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.earth_radius", 21) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Earth_rad", FTDouble, 12, 4) == -1)
          asfPrintError("Could not add earth radius to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.earth_radius_pp", 24) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "ER_pp", FTDouble, 12, 4) == -1)
          asfPrintError("Could not add earth radius pp to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.satellite_height", 25) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Sat_height", FTDouble, 12, 4) == -1)
          asfPrintError("Could not add satellite height to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.satellite_binary_time", 30) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Sat_bin_t", FTString, 25, 0) == -1)
          asfPrintError("Could not add sat binary time to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.satellite_clock_time", 29) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Sat_clock_t", FTString, 25, 0) == -1)
          asfPrintError("Could not add sat clock time to database file\n");
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.sar.range_doppler_coefficients", 35) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Rng_dop_1", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add range doppler 1 to database file\n");
        if (DBFAddField(dbase, "Rng_dop_2", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add range doppler 2 to database file\n");
        if (DBFAddField(dbase, "Rng_dop_3", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add range doppler 3 to database file\n");
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.sar.azimuth_doppler_coefficients", 37) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Az_dop_1", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add azimuth doppler 1 to database file\n");
        if (DBFAddField(dbase, "Az_dop_2", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add azimuth doppler 2 to database file\n");
        if (DBFAddField(dbase, "Az_dop_3", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add azimuth doppler 3 to database file\n");
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.sar.azimuth_processing_bandwidth", 37) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Az_proc_bw", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add az processing bw to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.chirp_rate", 19) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Chirp_rate", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add chirp rate to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.pulse_duration", 23) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Pulse_dur", FTDouble, 16, 12) == -1)
          asfPrintError("Could not add pulse duration to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.range_sampling_rate", 28) == 0
	       && dbf[ii].visible) {
        if (DBFAddField(dbase, "Rng_samp_r", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add range samp rate to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.polarization", 21) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Polarize", FTString, 25, 0) == -1)
          asfPrintError("Could not add polarization to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.multilook", 18) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Multilook", FTInteger, 1, 0) == -1)
          asfPrintError("Could not add multiook to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.pitch", 14) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Pitch", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add pitch to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.roll", 13) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Roll", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add roll to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.yaw", 12) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Yaw", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add yaw to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.incid_a", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<6; kk++) {
          sprintf(header, "Incid_a[%d]", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add incidence angle to database file\n");
        }
      }
    }
    if (meta->optical) {
      if (strncmp(dbf[ii].meta, "meta.optical.pointing_direction", 31) == 0 
	  && dbf[ii].visible) {
        if (DBFAddField(dbase, "Point_dir", FTString, 15, 0) == -1)
          asfPrintError("Could not add pointing direction to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.optical.off_nadir_angle", 28) == 0
	       && dbf[ii].visible) {
        if (DBFAddField(dbase, "Off_nadir", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add off nadir angle to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.optical.correction_level", 29) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Corr_level", FTString, 5, 0) == -1)
          asfPrintError("Could not add correction level to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.optical.cloud_percentage", 29) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Cloud_perc", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add cloud percentage to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		      "meta.optical.sun_azimuth_angle", 30) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Sun_az_ang", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add sun azimuth angle to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		      "meta.optical.sun_elevation_angle", 32) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Sun_elev", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add sun elevation to database file\n");
      }
    }
    if (meta->thermal) {
      if (strncmp(dbf[ii].meta, "meta.thermal.band_gain", 22) == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Band_gain", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add band gain to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.thermal.band_gain_change", 29) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Gain_change", FTDouble, 16, 7) == -1)
          asfPrintError("Could not band gain change to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.thermal.day", 16) == 0 &&
               dbf[ii].visible) {
      }
      if (DBFAddField(dbase, "Day", FTInteger, 1, 0) == -1)
        asfPrintError("Could not band day flag to database file\n");
    }
    if (meta->transform) {
      int kk;
      char header[12];
      int n = meta->transform->parameter_count;
      if (strncmp(dbf[ii].meta, "meta.transform.parameter_count", 30) == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Parameters", FTInteger, 2, 0) == -1)
          asfPrintError("Could not add parameter count to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.transform.x", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(header, "X[%d]", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add parameter x to database file\n");
        }
      }
      else if (strncmp(dbf[ii].meta, "meta.transform.y", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(header, "Y[%d]", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add parameter y to database file\n");
        }
      }
      else if (strncmp(dbf[ii].meta, "meta.transform.l", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(header, "L[%d]", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add parameter l to database file\n");
        }
      }
      else if (strncmp(dbf[ii].meta, "meta.transform.s", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(header, "S[%d]", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add parameter s to database file\n");
        }
      }
    }
    if (meta->airsar) {
      if (strncmp(dbf[ii].meta, "meta.airsar.scale_factor", 24) == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Scale", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add scale factor to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.gps_altitude", 24) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "GPS_height", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add GPS altitude to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.lat_peg_point", 25) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Lat_peg_pt", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add lat peg point to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.lon_peg_point", 25) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Lon_peg_pt", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add lon peg point to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.head_peg_point", 26) == 0 
	       && dbf[ii].visible) {
        if (DBFAddField(dbase, "Head_pegpt", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add heading peg point to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.airsar.along_track_offset", 30) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "AT_offset", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add at offset  to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.airsar.cross_track_offset", 30) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "CT_offset", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add ct offset to database file\n");
      }
    }
    if (meta->projection) {
      if (strncmp(dbf[ii].meta, "meta.projection.type", 20) == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Proc_type", FTString, 16, 0) == -1)
          asfPrintError("Could not add projection type to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.startX", 22) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Start_x", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add start x to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.startY",22) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Start_y", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add start y to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.perX", 20) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Per_x", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add per x to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.perY", 20) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Per_y", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add per y to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.units", 21) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Units", FTString, 12, 0) == -1)
          asfPrintError("Could not add units to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.hem", 19) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Hemisphere", FTString, 10, 0) == -1)
          asfPrintError("Could not add hemisphere to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.spheroid", 24) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Spheroid", FTString, 16, 0) == -1)
          asfPrintError("Could not add speroid to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.re_major", 24) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Proc_major", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add re major to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.re_minor", 24) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Proc_minor", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add re minor to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.datum", 21) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Datum", FTDouble, 35, 0) == -1)
          asfPrintError("Could not add datum to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.height", 22) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Proc_ht", FTDouble, 16, 7) == -1)
          asfPrintError("Could not projection height to database file\n");
      }
      if (meta->projection->type == ALBERS_EQUAL_AREA) {
        if (strncmp(dbf[ii].meta,
		    "meta.projection.param.albers.std_parallel1", 42) == 0 &&
            dbf[ii].visible) {
          if (DBFAddField(dbase, "Std_par_1", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add std parallel 1 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.std_parallel2", 42) == 0
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Std_par_2", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add std parallel 2 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.center_meridian", 
			 44) == 0 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Center_mer", FTDouble, 9, 4) == -1)
            asfPrintError("Could not central meridian to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.orig_latitude", 42) == 0
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Orig_lat", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add original lat to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.false_easting", 42) == 0
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "False_east", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false easting to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.false_northing", 
			 43) == 0 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Flse_north", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add std parallel 1 to database file\n");
        }
      }
      else if (meta->projection->type == SCANSAR_PROJECTION) {
        if (strncmp(dbf[ii].meta, 
		    "meta.projection.param.atct.rlocal", 33) == 0 &&
            dbf[ii].visible) {
          if (DBFAddField(dbase, "R_local", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add rlocal to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.atct.alpha1", 33) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Alpha_1", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add alpha1 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.atct.alpha2", 33) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Alpha_2", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add alpha 2 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.atct.alpha3", 33) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Alphar_3", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add alpha 3 to database file\n");
        }
      }
      else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
        if (strncmp(dbf[ii].meta,
		    "meta.projection.param.lamaz.center_lat", 38) == 0 &&
            dbf[ii].visible) {
          if (DBFAddField(dbase, "Center_lat", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add center latitude to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamaz.center_lon", 38) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Center_lon", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add center longitude to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamaz.false_easting", 41) == 0 
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "False_east", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false easting to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamaz.false_northing", 42) == 0
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Flse_north", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false northing to database file\n");
        }
      }
      else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC) {
        if (strncmp(dbf[ii].meta, 
		    "meta.projection.param.lamcc.plat1", 33) == 0 &&
            dbf[ii].visible) {
          if (DBFAddField(dbase, "Std_par_1", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add std parallel 1 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.plat2", 33) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Std_par_2", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add std parallel 1 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.lat0", 32) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Orig_lat", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add lat of origin to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.lon0", 32) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Center_mer", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add central meridian to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.false_easting", 41) == 0 
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "False_east", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false easting to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.false_northing", 42) == 0
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Flse_north", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false northing to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.scale_factor", 40) == 0 
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Proc_scale", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add scale factor to database file\n");
        }
      }
      else if (meta->projection->type == POLAR_STEREOGRAPHIC) {
        if (strncmp(dbf[ii].meta, "meta.projection.param.ps.slat", 29) == 0 
	    && dbf[ii].visible) {
          if (DBFAddField(dbase, "Std_par", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add std parallel to database file\n");
        }
        else if (strncmp(dbf[ii].meta, 
			 "meta.projection.param.ps.slon", 29) == 0 && 
		 dbf[ii].visible) {
          if (DBFAddField(dbase, "Center_mer", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add central meridian to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.ps.false_easting", 38) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "False_east", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false easting to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.ps.false_northing", 39) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Flse_north", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false northing to database file\n");
        }
      }
      else if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
        if (strncmp(dbf[ii].meta, "meta.projection.param.utm.zone", 30) == 0 
	    && dbf[ii].visible) {
          if (DBFAddField(dbase, "Zone", FTDouble, 2, 0) == -1)
            asfPrintError("Could not add zone to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.false_easting", 39) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "False_east", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false easting to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.false_northing", 40) == 0 
		 && dbf[ii].visible) {
          if (DBFAddField(dbase, "Flse_north", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add false northing to database file\n");
        }
        else if (strncmp(dbf[ii].meta, 
			 "meta.projection.param.utm.lat0", 30) == 0 && 
		 dbf[ii].visible) {
          if (DBFAddField(dbase, "Proc_lat", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add latitude to database file\n");
        }
        else if (strncmp(dbf[ii].meta, 
			 "meta.projection.param.utm.lon0", 30) == 0 && 
		 dbf[ii].visible) {
          if (DBFAddField(dbase, "Proc_lon", FTDouble, 9, 4) == -1)
            asfPrintError("Could not add std parallel 1 to database file\n");
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.scale_factor", 38) == 0 &&
                 dbf[ii].visible) {
          if (DBFAddField(dbase, "Proc_scale", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add scale factor to database file\n");
        }
      }
      else if (meta->projection->type == STATE_PLANE) {
        if (strncmp(dbf[ii].meta, 
		    "meta.projection.param.state.zone", 32) == 0 &&
            dbf[ii].visible) {
          if (DBFAddField(dbase, "Zone", FTDouble, 16, 7) == -1)
            asfPrintError("Could not add std parallel 1 to database file\n");
        }
      }
    }
    if (meta->stats) {
      int kk;
      int n = meta->stats->band_count;
      char header[12];
      if (strcmp(dbf[ii].meta, "meta.stats.band_count") == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Stat_bands", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add band count to database file\n");
      }
      for (kk=0; kk<n; kk++) {
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.band_id") == 0 &&
            dbf[ii].visible) {
          sprintf(header, "Band_ID_%d", kk+1);
          length = strlen(meta->stats->band_stats[kk].band_id);
          if (DBFAddField(dbase, header, FTString, length, 0) == -1)
            asfPrintError("Could not add band ID to database file\n");
        }
        else if (strcmp(dbf[ii].meta, "meta.stats.band_stats.min") == 0 &&
                 dbf[ii].visible) {
          sprintf(header, "Minimum_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add minimum to database file\n");
        }
        else if (strcmp(dbf[ii].meta, "meta.stats.band_stats.max") == 0 &&
                 dbf[ii].visible) {
          sprintf(header, "Maximum_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add maximum to database file\n");
        }
        else if (strcmp(dbf[ii].meta, "meta.stats.band_stats.mean") == 0 &&
                 dbf[ii].visible) {
          sprintf(header, "Mean_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add mean to database file\n");
        }
        else if (strcmp(dbf[ii].meta, "meta.stats.band_stats.rmse") == 0 &&
                 dbf[ii].visible) {
          sprintf(header, "RMSE_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add root mean square to database file\n");
        }
        else if (strcmp(dbf[ii].meta,
                        "meta.stats.band_stats.std_deviation") == 0 &&
                 dbf[ii].visible) {
          sprintf(header, "Std_dev_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add std deviation to database file\n");
        }
        else if (strcmp(dbf[ii].meta, "meta.stats.band_stats.mask") == 0 &&
                 dbf[ii].visible) {
          sprintf(header, "Mask_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add mask to database file\n");
        }
      }
    }
    if (meta->state_vectors) {
      int kk;
      int n = meta->state_vectors->vector_count;
      char header[12];
      if (strncmp(dbf[ii].meta, "meta.state.year", 15) == 0 && 
	  dbf[ii].visible) {
        if (DBFAddField(dbase, "Year", FTInteger, 4, 0) == -1)
          asfPrintError("Could not add vector count to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.state.julDay", 17) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Julian_day", FTInteger, 3, 0) == -1)
          asfPrintError("Could not add julian day to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.state.second", 17) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Second", FTDouble, 16, 7) == -1)
          asfPrintError("Could not add second to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.state.vector_count", 23) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Num_stVec", FTInteger, 2, 0) == -1)
          asfPrintError("Could not add std parallel 1 to database file\n");
      }
      else if (strncmp(dbf[ii].meta, "meta.state.vectors", 18) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          sprintf(header, "Time_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
          sprintf(header, "Pos_x_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
          sprintf(header, "Pos_y_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
          sprintf(header, "Pos_z_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
          sprintf(header, "Vel_x_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
          sprintf(header, "Vel_y_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
          sprintf(header, "Vel_z_%d", kk+1);
          if (DBFAddField(dbase, header, FTDouble, 16, 7) == -1)
            asfPrintError("Could not add stVec time to database file\n");
        }
      }
    }
    if (meta->location) {
      if (strncmp(dbf[ii].meta, 
		  "meta.location.lat_start_near_range", 34) == 0 &&
          dbf[ii].visible) {
        if (DBFAddField(dbase, "Lat_1", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lat start near rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lon_start_near_range", 34) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Lon_1", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lon start near rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lat_start_far_range", 33) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Lat_2", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lat start far rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lon_start_far_range", 33) == 0 &&
               dbf[ii].visible) {
        if (DBFAddField(dbase, "Lon_2", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lon start far rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.location.lat_end_near_range", 32) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Lat_3", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lat end near rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.location.lon_end_near_range", 32) == 0 && 
		       dbf[ii].visible) {
        if (DBFAddField(dbase, "Lon_3", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lon end near rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.location.lat_end_far_range", 31) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Lat_4", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lat end far rng to database file\n");
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.location.lon_end_far_range", 31) == 0 && 
	       dbf[ii].visible) {
        if (DBFAddField(dbase, "Lon_4", FTDouble, 9, 4) == -1)
          asfPrintError("Could not add lon end far rng to database file\n");
      }
    }
  }
  
  // Close the database for initialization
  DBFClose(dbase);
  
  // Open shapefile for initialization
  char tmpInFile[1024];
  strcpy(tmpInFile, inFile);
  char *ext = findExt(inFile);
  if (!ext && strcmp_case(meta->general->sensor, "ALOS") == 0) {
    // KLUDGE ALERT!  SHPCreate() below replaces the file extension in inFile
    // by searching from the end of the filename in reverse for a '.' character,
    // then appends .shx and .shp to the two filenames that it produces ...
    // Unfortunately, this results in truncated ALOS basenames in the output
    // files AND we don't own the shape library.  So, for ALOS only, append a
    // dummy extension for the shape library to snip off and consequently keep
    // the original ALOS basename intact:
    sprintf(tmpInFile, "%s.dummy", inFile);
  }
  shape = SHPCreate(tmpInFile, SHPT_POLYGON);
  if (!shape)
    asfPrintError("Could not create shapefile '%s'\n", inFile);
  
  // Close shapefile for initialization
  SHPClose(shape);

  FREE(dbaseFile);

  return;
}

// Convert metadata to kml file
int meta2kml(char *inFile, char *outFile, format_type_t inFormat, 
	     c2v_config *cfg)
{
  meta_parameters *meta=NULL;
  FILE *fpIn, *fpOut;
  char *line = (char *) MALLOC(sizeof(char)*1024);
  if (cfg->list) {
    fpIn = FOPEN(inFile, "r");
    fpOut = FOPEN(outFile, "w");
    kml_header(fpOut);
    while (fgets(line, 1024, fpIn)) {
      strip_end_whitesp_inplace(line);
      asfPrintStatus("File: %s\n\n", line);
      if (inFormat == LEADER && isleader(line)) {
        ceos_description *ceos = 
          get_ceos_description_ext(line, REPORT_LEVEL_NONE, FALSE);
        if (ceos->product == RAW)
          meta = meta_read_raw(line);
        else
          meta = meta_read_only(line);
      }
      else if (inFormat == STF_META && isparfile(line))
      	meta = meta_read_stf(line);
      else if (inFormat == META)
      	meta = meta_read(line);
      else
	      asfPrintError("Chosen file format (%s) does not match provided file "
		      "(%s)\n", format2str(inFormat), line);
          kml_entry_ext(fpOut, meta, meta->general->basename, cfg);
      meta_free(meta);
    }
    kml_footer(fpOut);
    FCLOSE(fpIn);
    FCLOSE(fpOut);
  }
  else {
    if (inFormat == LEADER && isleader(inFile)) {
      ceos_description *ceos = 
	      get_ceos_description_ext(inFile, REPORT_LEVEL_NONE, FALSE);
      if (ceos->product == RAW)
	      meta = meta_read_raw(inFile);
      else
	      meta = meta_read_only(inFile);
    }
    else if (inFormat == STF_META && isparfile(inFile))
      meta = meta_read_stf(inFile);
    else if (inFormat == META)
      meta = meta_read(inFile);
    else
      asfPrintError("Chosen file format (%s) does not match provided file "
		    "(%s)\n", format2str(inFormat), inFile);
    fpOut = FOPEN(outFile, "w");
    kml_header(fpOut);
    kml_entry_ext(fpOut, meta, meta->general->basename, cfg);
    kml_footer(fpOut);
    FCLOSE(fpOut);
    meta_free(meta);
  }

  FREE(line);
  return 1;
}

// Convert metadata to shapefile
static int convert_meta2shape(char *inFile, DBFHandle dbase, SHPHandle shape,
			      int n)
{
  dbf_header_t *dbf;
  meta_parameters *meta;
  double lat[5], lon[5];
  char shape_type[25];
  int ii, field=0, nCols;

  // Read metadata
  meta = meta_read(inFile);
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  // Determine corner coordinates
  if (meta->projection) {
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;
    double xPix = meta->projection->perX;
    double yPix = fabs(meta->projection->perY);
    double startX = meta->projection->startX;
    double startY = meta->projection->startY;
    lon[0] = startX;
    lat[0] = startY;
    lon[1] = startX + ns*xPix;
    lat[1] = startY;
    lon[2] = startX + ns*xPix;
    lat[2] = startY + nl*yPix;
    lon[3] = startX;
    lat[3] = startY + nl*yPix;
  }
  else if (meta->location) {
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

  // Read configuration file
  read_header_config("META", &dbf, &nCols, shape_type);

  // Write information into database file
  for (ii=0; ii<nCols; ii++) {
    // General block
    if (strncmp(dbf[ii].meta, "meta.general.basename", 21) == 0 &&
    dbf[ii].visible) {
      char *str = (char *)
        MALLOC(sizeof(char)*strlen(meta->general->basename));
      strcpy(str, meta->general->basename);
      DBFWriteStringAttribute(dbase, n, field, str);
      FREE(str);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.sensor", 19) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->sensor);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.sensor_name", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->sensor_name);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.mode", 17) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->mode);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.processor", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->processor);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.data_type", 22) == 0 &&
             dbf[ii].visible) {
      char data_type[25];
      if (meta->general->data_type == ASF_BYTE)
        strcpy(data_type, "BYTE");
      else if (meta->general->data_type == INTEGER16)
        strcpy(data_type, "INTEGER16");
      else if (meta->general->data_type == INTEGER32)
        strcpy(data_type, "INTEGER32");
      else if (meta->general->data_type == REAL32)
        strcpy(data_type, "REAL32");
      else if (meta->general->data_type == REAL64)
        strcpy(data_type, "REAL64");
      else if (meta->general->data_type == COMPLEX_BYTE)
        strcpy(data_type, "COMPLEX_BYTE");
      else if (meta->general->data_type == COMPLEX_INTEGER16)
        strcpy(data_type, "COMPLEX_INTEGER16");
      else if (meta->general->data_type == COMPLEX_INTEGER32)
        strcpy(data_type, "COMPLEX_INTEGER32");
      else if (meta->general->data_type == COMPLEX_REAL32)
        strcpy(data_type, "COMPLEX_REAL32");
      else if (meta->general->data_type == COMPLEX_REAL64)
        strcpy(data_type, "COMPLEX_REAL64");
      DBFWriteStringAttribute(dbase, n, field, data_type);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.image_data_type", 27) == 0 
	     && dbf[ii].visible) {
      char image_data_type[128];
      if (meta->general->image_data_type == RAW_IMAGE)
        strcpy(image_data_type, "RAW_IMAGE");
      else if (meta->general->image_data_type == COMPLEX_IMAGE)
        strcpy(image_data_type, "COMPLEX_IMAGE");
      else if (meta->general->image_data_type == AMPLITUDE_IMAGE)
        strcpy(image_data_type, "AMPLITUDE_IMAGE");
      else if (meta->general->image_data_type == PHASE_IMAGE)
        strcpy(image_data_type, "PHASE_IMAGE");
      else if (meta->general->image_data_type == INTERFEROGRAM)
        strcpy(image_data_type, "INTERFEROGRAM");
      else if (meta->general->image_data_type == COHERENCE_IMAGE)
        strcpy(image_data_type, "COHERENCE_IMAGE");
      else if (meta->general->image_data_type == POLARIMETRIC_IMAGE)
	strcpy(image_data_type, "POLARIMETRIC_IMAGE");
      else if (meta->general->image_data_type == POLARIMETRIC_SEGMENTATION)
        strcpy(image_data_type, "POLARIMETRIC_SEGMENTATION");
      else if (meta->general->image_data_type == POLARIMETRIC_DECOMPOSITION)
        strcpy(image_data_type, "POLARIMETRIC_DECOMPOSITION");
      else if (meta->general->image_data_type == POLARIMETRIC_PARAMETER)
        strcpy(image_data_type, "POLARIMETRIC_PARAMETER");
      else if (meta->general->image_data_type == POLARIMETRIC_C2_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C2_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_C3_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C3_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_C4_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_C4_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_T3_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_T3_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_T4_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_T4_MATRIX");
      else if (meta->general->image_data_type == POLARIMETRIC_STOKES_MATRIX)
        strcpy(image_data_type, "POLARIMETRIC_STOKES_MATRIX");
      else if (meta->general->image_data_type == LUT_IMAGE)
        strcpy(image_data_type, "LUT_IMAGE");
      else if (meta->general->image_data_type == ELEVATION)
        strcpy(image_data_type, "ELEVATION");
      else if (meta->general->image_data_type == DEM)
        strcpy(image_data_type, "DEM");
      else if (meta->general->image_data_type == IMAGE)
        strcpy(image_data_type, "IMAGE");
      else if (meta->general->image_data_type == MASK)
        strcpy(image_data_type, "MASK");
      DBFWriteStringAttribute(dbase, n, field, image_data_type);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.radiometry", 22) == 0 &&
             dbf[ii].visible) {
      char radiometry[20];
      if (meta->general->radiometry == r_AMP)
        strcpy(radiometry, "AMPLITUDE");
      else if (meta->general->radiometry == r_SIGMA)
        strcpy(radiometry, "SIGMA");
      else if (meta->general->radiometry == r_BETA)
        strcpy(radiometry, "BETA");
      else if (meta->general->radiometry == r_GAMMA)
        strcpy(radiometry, "GAMMA");
      else if (meta->general->radiometry == r_SIGMA_DB)
        strcpy(radiometry, "SIGMA_DB");
      else if (meta->general->radiometry == r_BETA_DB)
        strcpy(radiometry, "BETA_DB");
      else if (meta->general->radiometry == r_GAMMA_DB)
        strcpy(radiometry, "GAMMA_DB");
      else if (meta->general->radiometry == r_POWER)
        strcpy(radiometry, "POWER");
      DBFWriteStringAttribute(dbase, n, field, radiometry);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.acquisition_date", 29) == 0 
	     && dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field,
                  meta->general->acquisition_date);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.orbit", 17) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->orbit);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.orbit_direction", 27) == 0 
	     && dbf[ii].visible) {
      char orbit_direction[15];
      if (meta->general->orbit_direction == 'A')
    strcpy(orbit_direction, "Ascending");
      else
       strcpy(orbit_direction, "Descending");
      DBFWriteStringAttribute(dbase, n, field, orbit_direction);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.frame", 18) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->frame);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.band_count", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->band_count);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.bands", 17) == 0 &&
             dbf[ii].visible) {
      DBFWriteStringAttribute(dbase, n, field, meta->general->bands);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.line_count", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->line_count);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.sample_count", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->sample_count);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.start_line", 22) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->start_line);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.start_sample", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->start_sample);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.x_pixel_size", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->x_pixel_size);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.y_pixel_size", 24) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->y_pixel_size);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.center_latitude", 27) == 0 
	     && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->center_latitude);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.center_longitude", 28) == 0 
	     && dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field,
                  meta->general->center_longitude);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.re_major", 20) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->re_major);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.re_minor", 20) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->re_minor);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.bit_error_rate", 26) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->bit_error_rate);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.missing_lines", 25) == 0 &&
             dbf[ii].visible) {
      DBFWriteIntegerAttribute(dbase, n, field, meta->general->missing_lines);
      field++;
    }
    else if (strncmp(dbf[ii].meta, "meta.general.no_data", 19) == 0 &&
             dbf[ii].visible) {
      DBFWriteDoubleAttribute(dbase, n, field, meta->general->no_data);
      field++;
    }
  }
  for (ii=0; ii<nCols; ii++) {

    // SAR block
    if (meta->sar) {
      int kk;
      char str[12];
      if (strncmp(dbf[ii].meta, "meta.sar.image_type", 19) == 0 &&
          dbf[ii].visible) {
        sprintf(str, "%c", meta->sar->image_type);
        DBFWriteStringAttribute(dbase, n, field, str);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.look_direction", 23) == 0 &&
               dbf[ii].visible) {
        sprintf(str, "%c", meta->sar->look_direction);
        DBFWriteStringAttribute(dbase, n, field, str);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.azimuth_look_count", 27) == 0 
	       && dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, 
				 meta->sar->azimuth_look_count);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.range_look_count", 25) == 0 
	       && dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, 
				 meta->sar->range_look_count);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.deskewed", 17) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->sar->deskewed);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.original_line_count", 28) == 0
	       && dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                                 meta->sar->original_line_count);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.original_sample_count", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                                 meta->sar->original_sample_count);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.line_increment", 23) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->line_increment);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.sample_increment", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->sample_increment);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.range_time_per_pixel", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_time_per_pixel);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.azimuth_time_per_pixel", 31) == 0 && 
	       dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_time_per_pixel);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.slant_shift", 20) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->slant_shift);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.time_shift", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->time_shift);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.slant_range_first_pixel", 32) == 0 && 
	       dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->slant_range_first_pixel);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.wavelength", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->wavelength);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.prf", 12) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->prf);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.earth_radius", 21) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->earth_radius);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.earth_radius_pp", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->earth_radius_pp);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.satellite_height", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->satellite_height);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.satellite_binary_time", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->sar->satellite_binary_time);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.sar.satellite_clock_time", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->sar->satellite_clock_time);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.sar.range_doppler_coefficients", 35) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_doppler_coefficients[0]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_doppler_coefficients[1]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_doppler_coefficients[2]);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.sar.azimuth_doppler_coefficients", 37) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_doppler_coefficients[0]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_doppler_coefficients[1]);
        field++;
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_doppler_coefficients[2]);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.sar.azimuth_processing_bandwidth", 37) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->azimuth_processing_bandwidth);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.chirp_rate", 19) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->chirp_rate);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.pulse_duration", 23) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->pulse_duration);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.range_sampling_rate", 28) == 0
	       && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->sar->range_sampling_rate);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.polarization", 21) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field, meta->sar->polarization);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.multilook", 18) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->sar->multilook);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.pitch", 14) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->pitch);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.roll", 13) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->roll);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.yaw", 12) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->sar->yaw);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.sar.incid_a", 16) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<6; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->sar->incid_a[kk]);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Optical block
    if (meta->optical) {
      if (strncmp(dbf[ii].meta, "meta.optical.pointing_direction", 31) == 0 
	  && dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->optical->pointing_direction);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.optical.off_nadir_angle", 28) == 0
	       && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->off_nadir_angle);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.optical.correction_level", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field,
                                meta->optical->correction_level);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.optical.cloud_percentage", 29) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->cloud_percentage);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.optical.sun_azimuth_angle", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->sun_azimuth_angle);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.optical.sun_elevation_angle", 32) == 0
               && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->optical->sun_elevation_angle);
        field++;
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // AirSAR block
    if (meta->airsar) {
      if (strncmp(dbf[ii].meta, "meta.airsar.scale_factor", 24) == 0 &&
          dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->scale_factor);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.gps_altitude", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->gps_altitude);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.lat_peg_point", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->lat_peg_point);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.lon_peg_point", 25) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->lon_peg_point);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.airsar.head_peg_point", 26) == 0 
	       && dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->airsar->head_peg_point);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.airsar.along_track_offset", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->airsar->along_track_offset);
        field++;
      }
      else if (strncmp(dbf[ii].meta, 
		       "meta.airsar.cross_track_offset", 30) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->airsar->cross_track_offset);
        field++;
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Projection block
    if (meta->projection) {
      if (strncmp(dbf[ii].meta, "meta.projection.type", 20) == 0 &&
      dbf[ii].visible) {
    char type[50];
        if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR)
          strcpy(type, "UNIVERSAL TRANSVERSE MERCATOR");
        else if (meta->projection->type == POLAR_STEREOGRAPHIC)
          strcpy(type, "POLAR STEREOGRAPHIC");
        else if (meta->projection->type == ALBERS_EQUAL_AREA)
          strcpy(type, "ALBERS EQUAL AREA");
        else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC)
          strcpy(type, "LAMBERT CONFORMAL CONIC");
        else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA)
          strcpy(type, "LAMBERT AZIMUTHAL EQUAL AREA");
        else if (meta->projection->type == STATE_PLANE)
          strcpy(type, "STATE PLANE");
        else if (meta->projection->type == SCANSAR_PROJECTION)
          strcpy(type, "SCANSAR PROJECTION");
        else if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION)
          strcpy(type, "LAT LONG PSEUDO PROJECTION");
        else if (meta->projection->type == UNKNOWN_PROJECTION)
          strcpy(type, "UNKNOWN PROJECTION");
        DBFWriteStringAttribute(dbase, n, field, type);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.startX", 22) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->startX);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.startY", 22) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->startY);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.perX", 20) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->perX);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.perY", 20) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->perY);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.units", 21) == 0 &&
               dbf[ii].visible) {
        DBFWriteStringAttribute(dbase, n, field, meta->projection->units);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.hem", 19) == 0 &&
               dbf[ii].visible) {
        char hemisphere[15];
        if (meta->projection->hem == 'N')
          strcpy(hemisphere, "North");
        else if (meta->projection->hem == 'S')
          strcpy(hemisphere, "South");
        DBFWriteStringAttribute(dbase, n, field, hemisphere);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.spheroid", 24) == 0 &&
               dbf[ii].visible) {
        char spheroid[25];
        if (meta->projection->spheroid == BESSEL_SPHEROID)
          strcpy(spheroid, "BESSEL");
        else if (meta->projection->spheroid == CLARKE1866_SPHEROID)
          strcpy(spheroid, "CLARKE1866");
        else if (meta->projection->spheroid == CLARKE1880_SPHEROID)
          strcpy(spheroid, "CLARKE1880");
        else if (meta->projection->spheroid == GEM6_SPHEROID)
          strcpy(spheroid, "GEM6");
        else if (meta->projection->spheroid == GEM10C_SPHEROID)
          strcpy(spheroid, "GEM10C");
        else if (meta->projection->spheroid == GRS1980_SPHEROID)
          strcpy(spheroid, "GRS1980");
        else if (meta->projection->spheroid == INTERNATIONAL1924_SPHEROID)
          strcpy(spheroid, "INTERNATIONAL1924");
        else if (meta->projection->spheroid == INTERNATIONAL1967_SPHEROID)
          strcpy(spheroid, "INTERNATIONAL1967");
        else if (meta->projection->spheroid == WGS72_SPHEROID)
          strcpy(spheroid, "WGS72");
        else if (meta->projection->spheroid == WGS84_SPHEROID)
          strcpy(spheroid, "WGS84");
        else if (meta->projection->spheroid == HUGHES_SPHEROID)
          strcpy(spheroid, "HUGHES");
        else
          strcpy(spheroid, "UNKNOWN");
        DBFWriteStringAttribute(dbase, n, field, spheroid);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.re_major", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->re_major);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.re_minor", 24) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->re_minor);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.datum", 21) == 0 &&
               dbf[ii].visible) {
        char datum[25];
        if (meta->projection->datum == EGM96_DATUM)
          strcpy(datum, "EGM96");
        else if (meta->projection->datum == ED50_DATUM)
          strcpy(datum, "ED50");
        else if (meta->projection->datum == ETRF89_DATUM)
          strcpy(datum, "ETRF89");
        else if (meta->projection->datum == ETRS89_DATUM)
          strcpy(datum, "ETRS89");
        else if (meta->projection->datum == ITRF97_DATUM)
          strcpy(datum, "ITRF97");
        else if (meta->projection->datum == NAD27_DATUM)
          strcpy(datum, "NAD27");
        else if (meta->projection->datum == NAD83_DATUM)
          strcpy(datum, "NAD83");
        else if (meta->projection->datum == WGS72_DATUM)
          strcpy(datum, "WGS72");
        else if (meta->projection->datum == WGS84_DATUM)
          strcpy(datum, "WGS84");
        else if (meta->projection->datum == HUGHES_DATUM)
          strcpy(datum, "HUGHES");
        else
          strcpy(datum, "UNKNOWN");
        DBFWriteStringAttribute(dbase, n, field, datum);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.projection.height",22) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->projection->height);
        field++;
      }
      if (meta->projection->type == ALBERS_EQUAL_AREA) {
        if (strncmp(dbf[ii].meta,
		    "meta.projection.param.albers.std_parallel1", 42) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.std_parallel1);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.std_parallel2", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.std_parallel2);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.center_meridian", 
			 44) == 0 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.center_meridian);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.orig_latitude", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.orig_latitude);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.false_easting", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.albers.false_northing", 
			 43) == 0 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.albers.false_northing);
          field++;
        }
      }
      else if (meta->projection->type == SCANSAR_PROJECTION) {
        if (strncmp(dbf[ii].meta, 
		    "meta.projection.param.atct.rlocal", 33) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.rlocal);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.atct.alpha1", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.alpha1);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.atct.alpha2", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.alpha2);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.atct.alpha3", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.atct.alpha3);
          field++;
        }
      }
      else if (meta->projection->type == LAMBERT_AZIMUTHAL_EQUAL_AREA) {
        if (strncmp(dbf[ii].meta,
		    "meta.projection.param.lamaz.center_lat", 38) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.center_lat);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamaz.center_lon", 38) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.center_lon);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamaz.false_easting", 41) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamaz.false_northing", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamaz.false_northing);
          field++;
        }
      }
      else if (meta->projection->type == LAMBERT_CONFORMAL_CONIC) {
        if (strncmp(dbf[ii].meta, 
		    "meta.projection.param.lamcc.plat1", 33) == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.plat1);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.plat2", 33) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.plat2);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.lat0", 32) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.lat0);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.lon0", 32) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.lon0);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.false_easting", 41) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.false_northing", 42) == 0
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.false_northing);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.lamcc.scale_factor", 40) == 0 
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.lamcc.scale_factor);
          field++;
        }
      }
      else if (meta->projection->type == POLAR_STEREOGRAPHIC) {
        if (strncmp(dbf[ii].meta, "meta.projection.param.ps.slat", 29) == 0 
	    && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.slat);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.ps.slon", 29) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.slon);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.ps.false_easting", 38) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.ps.false_northing", 39) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.ps.false_northing);
          field++;
        }
      }
      else if (meta->projection->type == UNIVERSAL_TRANSVERSE_MERCATOR) {
        if (strncmp(dbf[ii].meta, "meta.projection.param.utm.zone", 30) == 0 
	    && dbf[ii].visible) {
          DBFWriteIntegerAttribute(dbase, n, field,
                                   meta->projection->param.utm.zone);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.false_easting", 39) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.false_easting);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.false_northing", 40) == 0 
		 && dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.false_northing);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.lat0", 30) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.lat0);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.lon0", 30) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.lon0);
          field++;
        }
        else if (strncmp(dbf[ii].meta,
			 "meta.projection.param.utm.scale_factor", 38) == 0 &&
                 dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->projection->param.utm.scale_factor);
          field++;
        }
      }
      else if (meta->projection->type == STATE_PLANE) {
        if (strncmp(dbf[ii].meta, 
		    "meta.projection.param.state.zone", 32) == 0 &&
            dbf[ii].visible) {
          DBFWriteIntegerAttribute(dbase, n, field,
                                   meta->projection->param.state.zone);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Stats block
    if (meta->stats) {
      int kk;
      if (strcmp(dbf[ii].meta, "meta.stats.band_count") == 0 &&
          dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->stats->band_count);
        field++;
      }
      for (kk=0; kk<meta->stats->band_count; kk++) {
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.band_id") == 0 &&
            dbf[ii].visible) {
          DBFWriteStringAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].band_id);
          field++;
        }
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.min") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].min);
          field++;
        }
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.max") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].max);
          field++;
        }
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.mean") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].mean);
          field++;
        }
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.rmse") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].rmse);
          field++;
        }
        if (strcmp(dbf[ii].meta,
                   "meta.stats.band_stats.std_deviation") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].std_deviation);
          field++;
        }
        if (strcmp(dbf[ii].meta, "meta.stats.band_stats.mask") == 0 &&
            dbf[ii].visible) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->stats->band_stats[kk].mask);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // State vector block
    if (meta->state_vectors) {
      int kk;
      int n = meta->state_vectors->vector_count;
      if (strncmp(dbf[ii].meta, "meta.state.year", 15) == 0 &&
          dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->state_vectors->year);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.state.julDay", 17) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field, meta->state_vectors->julDay);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.state.second", 17) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field, meta->state_vectors->second);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.state.vector_count", 23) == 0 &&
               dbf[ii].visible) {
        DBFWriteIntegerAttribute(dbase, n, field,
                                 meta->state_vectors->vector_count);
        field++;
      }
      else if (strncmp(dbf[ii].meta, "meta.state.vectors", 18) == 0 &&
               dbf[ii].visible) {
        for (kk=0; kk<n; kk++) {
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].time);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.pos.x);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.pos.y);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.pos.z);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.vel.x);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.vel.y);
          field++;
          DBFWriteDoubleAttribute(dbase, n, field,
                                  meta->state_vectors->vecs[kk].vec.vel.z);
          field++;
        }
      }
    }
  }
  for (ii=0; ii<nCols; ii++) {
    // Location block
    if (meta->location) {
      if (strncmp(dbf[ii].meta, 
		  "meta.location.lat_start_near_range", 34) == 0 &&
          dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                                meta->location->lat_start_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lon_start_near_range", 34) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_start_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lat_start_far_range", 33) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lat_start_far_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lon_start_far_range", 33) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_start_far_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lat_end_near_range", 32) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lat_end_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lon_end_near_range", 32) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_end_near_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lat_end_far_range", 31) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lat_end_far_range);
        field++;
      }
      else if (strncmp(dbf[ii].meta,
		       "meta.location.lon_end_far_range", 31) == 0 &&
               dbf[ii].visible) {
        DBFWriteDoubleAttribute(dbase, n, field,
                meta->location->lon_end_far_range);
        field++;
      }
    }
  }

  // Write shape object
  SHPObject *shapeObject=NULL;
  shapeObject = SHPCreateSimpleObject(SHPT_POLYGON, 5, lon, lat, NULL);
  SHPWriteObject(shape, -1, shapeObject);
  SHPDestroyObject(shapeObject);

  // Clean up
  meta_free(meta);

  return 1;
}

// Convert metadata to shapefile
int meta2shape(char *inFile, char *outFile, int listFlag)
{
  FILE *fp;
  DBFHandle dbase;
  SHPHandle shape;
  dbf_header_t *dbf = NULL;
  char line[1024], metaFile[1024];
  int n=0, nAttr, nCoords;

  // Initialize the shape file.
  if (listFlag) {
    fp = FOPEN(inFile, "r");
    fgets(line, 1024, fp);
    strip_end_whitesp_inplace(line);
    strcpy(metaFile, line);
    FCLOSE(fp);
  }
  else
    strcpy(metaFile, inFile);
  //meta = meta_read(metaFile);
  //shape_meta_init(outFile, meta);
  double *lat = (double *) MALLOC(sizeof(double)*5);
  double *lon = (double *) MALLOC(sizeof(double)*5);
  meta_parameters *meta = 
    meta2vector(metaFile, &dbf, &nAttr, lat, lon, &nCoords);
  shapefile_init(outFile, "META", meta);
  open_shape(outFile, &dbase, &shape);
  
  if (listFlag) {
    fp = FOPEN(inFile, "r");
    while (fgets(line, 1024, fp)) {
      strip_end_whitesp_inplace(line);
      convert_meta2shape(line, dbase, shape, n);
      n++;
    }
    FCLOSE(fp);
  }
  else {
    //convert_meta2shape(inFile, dbase, shape, 0);
    write_shape_attributes(dbase, nAttr, 0, dbf);
    write_shape_object(shape, 5, 0, lat, lon);
  }

  // Clean up
  close_shape(dbase, shape);
  if (meta->projection)
    write_asf2esri_proj(meta, NULL, outFile);
  else
    write_esri_proj_file(outFile);

  meta_free(meta);

  return TRUE;
}
*/