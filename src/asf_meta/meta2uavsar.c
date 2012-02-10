#include "asf_meta.h"
#include "dateUtil.h"
#include "uavsar.h"
#include "asf_nan.h"

#define FLOAT_COMPARE_TOLERANCE(a, b, t) (fabs (a - b) <= t ? 1: 0)
#define ASF_EXPORT_FLOAT_MICRON 0.000000001
#define FLOAT_EQUIVALENT(a, b) (FLOAT_COMPARE_TOLERANCE \
                                (a, b, ASF_EXPORT_FLOAT_MICRON))

meta_parameters* uavsar_polsar2meta(uavsar_polsar *params)
{
  meta_parameters *meta;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  sprintf(meta->general->basename, "%s", params->site);
  sprintf(meta->general->sensor, "UAVSAR");
  strcpy(meta->general->sensor_name, "PolSAR");
  strcpy(meta->general->processor, "JPL version ");
  strcat(meta->general->processor, params->processor);
  if (params->type == POLSAR_SLC) {
    strcpy(meta->general->mode, "SLC");
    meta->general->image_data_type = POLARIMETRIC_S2_MATRIX;
  }
  else if (params->type == POLSAR_MLC) {
    strcpy(meta->general->mode, "MLC");
    meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
  }
  else if (params->type == POLSAR_DAT) {
    strcpy(meta->general->mode, "DAT");
    meta->general->image_data_type = POLARIMETRIC_STOKES_MATRIX;
  }
  else if (params->type == POLSAR_GRD) {
    strcpy(meta->general->mode, "GRD");
    meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
    meta->general->no_data = 0;
  }
  else if (params->type == POLSAR_HGT) {
    strcpy(meta->general->mode, "HGT");
    meta->general->image_data_type = DEM;
  }
  else
    strcpy(meta->general->mode, "unknown");
  meta->general->data_type = REAL32;
  strcpy(meta->general->acquisition_date, params->acquisition_date);
  meta->general->band_count = 1;
  meta->general->line_count = params->row_count;
  meta->general->sample_count = params->column_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = params->range_pixel_spacing;
  meta->general->y_pixel_size = fabs(params->azimuth_pixel_spacing);
  meta->general->re_major = params->semi_major;
  double a = params->semi_major;
  double ecc2 = params->eccentricity;
  meta->general->re_minor = sqrt((1-ecc2)*a*a);
  // no information on bit error rate, missing lines and no data

  // SAR block
  meta->sar = meta_sar_init();
  strcpy(meta->sar->polarization, "QUAD-POL");
  if (strcmp_case(params->projection, "SCX") == 0)
    meta->sar->image_type = 'S';
  else if (strcmp_case(params->projection, "EQA") == 0)
    meta->sar->image_type = 'P';
  if (strcmp_case(params->look_direction, "Left") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(params->look_direction, "Right") == 0)
    meta->sar->look_direction = 'R';
  if (params->type == POLSAR_SLC) {
    meta->sar->look_count = 1;
    meta->sar->multilook = 0;
  }
  else {
    // FIXME: Update once range look count is introduced to metadata structure
    meta->sar->look_count = params->azimuth_look_count;
    meta->sar->multilook = 1;
  }
  meta->sar->deskewed = 1;
  meta->sar->original_line_count = params->row_count;
  meta->sar->original_sample_count = params->column_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  // no information on azimuth and range time per pixel
  meta->sar->time_shift = 0.0;
  meta->sar->slant_shift = 0.0;
  meta->sar->slant_range_first_pixel = params->slant_range_first_pixel * 1000.0;
  meta->sar->wavelength = params->wavelength / 100.0;
  // no information on pulse repetition frequency
  // no information on earth radius and satellite height
  // no time information
  // no Doppler information
  meta->sar->yaw = params->yaw;
  meta->sar->pitch = params->pitch;
  meta->sar->roll = params->roll;
  meta->sar->azimuth_processing_bandwidth = params->bandwidth;
  // no chirp rate information
  meta->sar->pulse_duration = params->pulse_length / 1000.0;
  // no information on range sampling rate
  // FIXME: check polarizations for interferometric/polarimetric data
  // FIXME: multilook flag depends on data type
  
  // UAVSAR block
  meta->uavsar = meta_uavsar_init();
  strcpy(meta->uavsar->id, params->id);
  meta->uavsar->scale_factor = 1.0;
  meta->uavsar->gps_altitude = params->altitude;
  meta->uavsar->lat_peg_point = params->lat_peg_point;
  meta->uavsar->lon_peg_point = params->lon_peg_point;
  meta->uavsar->head_peg_point = params->head_peg_point;
  meta->uavsar->along_track_offset = params->along_track_offset;
  meta->uavsar->cross_track_offset = params->cross_track_offset;

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = params->lat_upper_left;
  meta->location->lon_start_near_range = params->lon_upper_left;
  meta->location->lat_start_far_range = params->lat_upper_right;
  meta->location->lon_start_far_range = params->lon_upper_right;
  meta->location->lat_end_near_range = params->lat_lower_left;
  meta->location->lon_end_near_range = params->lon_lower_left;
  meta->location->lat_end_far_range = params->lat_lower_right;
  meta->location->lon_end_far_range = params->lon_lower_right;

  // Projection block
  if (params->type == POLSAR_GRD || params->type == POLSAR_HGT) {
    meta->projection = meta_projection_init();
    //strcpy (meta->projection->units, "meters");
    strcpy (meta->projection->units, "degrees");
    if (params->along_track_offset >= 0.0)
      meta->projection->hem = 'N';
    else
      meta->projection->hem = 'S';
    meta->projection->re_major = meta->general->re_major;
    meta->projection->re_minor = meta->general->re_minor;
    meta->projection->height = 0.0;
    meta->projection->spheroid = WGS84_SPHEROID;
    meta->projection->datum = WGS84_DATUM;

    // Convert geographic coordinates into map projected coordinates
    //double lat = params->along_track_offset;
    //double lon = params->cross_track_offset;
    //double x1, y1, z1, x2, y2, z2;
    if (strcmp_case(params->projection, "EQA") == 0) {
      //meta->projection->type = EQUI_RECTANGULAR;
      //meta->projection->param.eqr.central_meridian = 0.0;
      //meta->projection->param.eqr.orig_latitude = 0.0;
      //meta->projection->param.eqr.false_easting = 0.0;
      //meta->projection->param.eqr.false_northing = 0.0;
      meta->projection->type = LAT_LONG_PSEUDO_PROJECTION;
    }
    /*
    latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0.0, &x1, &y1, &z1);
    lat += params->azimuth_pixel_spacing;
    lon += params->range_pixel_spacing;
    latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0.0, &x2, &y2, &z2);
    meta->projection->startX = x1;
    meta->projection->startY = y1;
    meta->projection->perX = fabs(x1 - x2);
    meta->projection->perY = -fabs(y1 - y2);
    */

    // UAVSAR lat/lon projected data appears to be calculated from the center of
    // the pixel at (-1,-1), instead of the upper-left of pixel (0,0).  Thus we
    // have to add in a half-pixel shift.
    meta->projection->startX = params->cross_track_offset + .5*params->range_pixel_spacing;
    meta->projection->startY = params->along_track_offset + .5*params->azimuth_pixel_spacing;
    meta->projection->perX = params->range_pixel_spacing;
    meta->projection->perY = params->azimuth_pixel_spacing;

    //meta->general->x_pixel_size = fabs(meta->projection->perX);
    //meta->general->y_pixel_size = fabs(meta->projection->perY);
    meta->general->center_latitude = params->along_track_offset + 
      params->azimuth_pixel_spacing * params->row_count / 2.0;
    meta->general->center_longitude = params->cross_track_offset +
      params->range_pixel_spacing * params->column_count / 2.0;
  }

  return meta;
}

meta_parameters* uavsar_insar2meta(uavsar_insar *params)
{
  meta_parameters *meta;

  // Allocate memory for metadata structure
  meta = raw_init();
  
  // General block
  sprintf(meta->general->basename, "%s", params->site);
  sprintf(meta->general->sensor, "UAVSAR");
  strcpy(meta->general->sensor_name, "InSAR");
  strcpy(meta->general->processor, "JPL version ");
  strcat(meta->general->processor, params->processor);
  if (params->type == INSAR_AMP || params->type == INSAR_AMP_GRD) {
    if (params->type == INSAR_AMP) 
      strcpy(meta->general->mode, "AMP");
    else if (params->type == INSAR_AMP_GRD) 
      strcpy(meta->general->mode, "AMP_GRD");
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  }
  else if (params->type == INSAR_INT || params->type == INSAR_INT_GRD) {
    if (params->type == INSAR_INT)
      strcpy(meta->general->mode, "INT");
    else if (params->type == INSAR_INT_GRD)
      strcpy(meta->general->mode, "INT_GRD");
    meta->general->image_data_type = INTERFEROGRAM;
  }
  else if (params->type == INSAR_UNW || params->type == INSAR_UNW_GRD) {
    if (params->type == INSAR_UNW)
      strcpy(meta->general->mode, "UNW");
    else if (params->type == INSAR_UNW_GRD)
      strcpy(meta->general->mode, "UNW_GRD");
    meta->general->image_data_type = UNWRAPPED_PHASE;
  }
  else if (params->type == INSAR_COR || params->type == INSAR_COR_GRD) {
    if (params->type == INSAR_COR)
      strcpy(meta->general->mode, "COR");
    else if (params->type == INSAR_COR_GRD)
      strcpy(meta->general->mode, "COR_GRD");
    meta->general->image_data_type = COHERENCE_IMAGE;
  }
  else if (params->type == INSAR_HGT_GRD) {
    strcpy(meta->general->mode, "HGT_GRD");
    meta->general->image_data_type = DEM;
  }
  else
    strcpy(meta->general->mode, "unknown");
  meta->general->data_type = REAL32;
  strcpy(meta->general->acquisition_date, params->acquisition_date);
  meta->general->band_count = 1;
  meta->general->line_count = params->row_count;
  meta->general->sample_count = params->column_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = params->range_pixel_spacing;
  meta->general->y_pixel_size = fabs(params->azimuth_pixel_spacing);
  meta->general->re_major = params->semi_major;
  double a = params->semi_major;
  double ecc2 = params->eccentricity;
  meta->general->re_minor = sqrt((1-ecc2)*a*a);
  // no information on bit error rate, missing lines and no data
  
  // SAR block
  meta->sar = meta_sar_init();
  strcpy(meta->sar->polarization, params->polarization);
  if (strcmp_case(params->projection, "SCX") == 0)
    meta->sar->image_type = 'S';
  else if (strcmp_case(params->projection, "EQA") == 0)
    meta->sar->image_type = 'P';
  if (strcmp_case(params->look_direction, "Left") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(params->look_direction, "Right") == 0)
    meta->sar->look_direction = 'R';
  // FIXME: Update once range look count is introduced to metadata structure
  meta->sar->look_count = params->azimuth_look_count;
  meta->sar->multilook = 1;
  meta->sar->deskewed = 1;
  meta->sar->original_line_count = params->row_count;
  meta->sar->original_sample_count = params->column_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  // no information on azimuth and range time per pixel
  meta->sar->time_shift = 0.0;
  meta->sar->slant_shift = 0.0;
  meta->sar->slant_range_first_pixel = params->slant_range_first_pixel * 1000.0;
  meta->sar->wavelength = params->wavelength / 100.0;
  // no information on pulse repetition frequency
  // no information on earth radius and satellite height
  // no time information
  // no Doppler information
  meta->sar->yaw = params->yaw;
  meta->sar->pitch = params->pitch;
  meta->sar->roll = params->roll;
  meta->sar->azimuth_processing_bandwidth = params->bandwidth;
  // no chirp rate information
  meta->sar->pulse_duration = params->pulse_length / 1000.0;
  // no information on range sampling rate
  // FIXME: check polarizations for interferometric/polarimetric data
  // FIXME: multilook flag depends on data type
  
  // UAVSAR block
  meta->uavsar = meta_uavsar_init();
  strcpy(meta->uavsar->id, params->id);
  meta->uavsar->scale_factor = 1.0;
  meta->uavsar->gps_altitude = params->altitude;
  meta->uavsar->lat_peg_point = params->lat_peg_point;
  meta->uavsar->lon_peg_point = params->lon_peg_point;
  meta->uavsar->head_peg_point = params->head_peg_point;
  meta->uavsar->along_track_offset = params->along_track_offset;
  meta->uavsar->cross_track_offset = params->cross_track_offset;

  // Projection block
  if (params->type >= INSAR_AMP_GRD && params->type <= INSAR_HGT_GRD) {
    meta->projection = meta_projection_init();
    //strcpy (meta->projection->units, "meters");
    strcpy (meta->projection->units, "degrees");
    if (params->along_track_offset >= 0.0)
      meta->projection->hem = 'N';
    else
      meta->projection->hem = 'S';
    meta->projection->re_major = meta->general->re_major;
    meta->projection->re_minor = meta->general->re_minor;
    meta->projection->height = 0.0;
    meta->projection->spheroid = WGS84_SPHEROID;
    meta->projection->datum = WGS84_DATUM;

    // Convert geographic coordinates into map projected coordinates
    //double lat = params->along_track_offset;
    //double lon = params->cross_track_offset;
    //double x1, y1, z1, x2, y2, z2;
    if (strcmp_case(params->projection, "EQA") == 0) {
      //meta->projection->type = EQUI_RECTANGULAR;
      //meta->projection->param.eqr.central_meridian = 0.0;
      //meta->projection->param.eqr.orig_latitude = 0.0;
      //meta->projection->param.eqr.false_easting = 0.0;
      //meta->projection->param.eqr.false_northing = 0.0;
      meta->projection->type = LAT_LONG_PSEUDO_PROJECTION;
    }
    /*
    latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0.0, &x1, &y1, &z1);
    lat += params->azimuth_pixel_spacing;
    lon += params->range_pixel_spacing;
    latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0.0, &x2, &y2, &z2);
    meta->projection->startX = x1;
    meta->projection->startY = y1;
    meta->projection->perX = fabs(x1 - x2);
    meta->projection->perY = -fabs(y1 - y2);
    */
    meta->projection->startX = params->cross_track_offset;
    meta->projection->startY = params->along_track_offset;
    meta->projection->perX = params->range_pixel_spacing;
    meta->projection->perY = params->azimuth_pixel_spacing;

    //meta->general->x_pixel_size = fabs(meta->projection->perX);
    //meta->general->y_pixel_size = fabs(meta->projection->perY);
    meta->general->center_latitude = params->along_track_offset + 
      params->azimuth_pixel_spacing * params->row_count / 2.0;
    meta->general->center_longitude = params->cross_track_offset +
      params->range_pixel_spacing * params->column_count / 2.0;
  }

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = params->lat_upper_left;
  meta->location->lon_start_near_range = params->lon_upper_left;
  meta->location->lat_start_far_range = params->lat_upper_right;
  meta->location->lon_start_far_range = params->lon_upper_right;
  meta->location->lat_end_near_range = params->lat_lower_left;
  meta->location->lon_end_near_range = params->lon_lower_left;
  meta->location->lat_end_far_range = params->lat_lower_right;
  meta->location->lon_end_far_range = params->lon_lower_right;

  return meta;
}
