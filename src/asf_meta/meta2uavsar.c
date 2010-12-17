#include "asf_meta.h"
#include "dateUtil.h"
#include "uavsar.h"
#include "asf_nan.h"

meta_parameters* uavsar_polsar2meta(uavsar_polsar *params)
{
  meta_parameters *meta;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  sprintf(meta->general->basename, "%s", params->site);
  sprintf(meta->general->sensor, "UAVSAR");
  strcpy(meta->general->sensor_name, "SAR");
  strcpy(meta->general->processor, "JPL version ");
  strcat(meta->general->processor, params->processor);
  strcpy(meta->general->mode, "PolSAR");
    meta->general->data_type = REAL32;
  if (params->bytes_per_pixel == 4) 
    meta->general->band_count = 1;
  else if (params->bytes_per_pixel == 8)
    meta->general->band_count = 2;
  meta->general->image_data_type = POLARIMETRIC_IMAGE;
  strcpy(meta->general->acquisition_date, params->acquisition_date);
  meta->general->band_count = 1;
  meta->general->line_count = params->row_count;
  meta->general->sample_count = params->column_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = params->range_pixel_spacing;
  meta->general->y_pixel_size = params->azimuth_pixel_spacing;
  meta->general->re_major = params->semi_major;
  double a = params->semi_major;
  double ecc2 = params->eccentricity;
  meta->general->re_minor = sqrt((1-ecc2)*a*a);
  // no information on bit error rate, missing lines and no data

  // SAR block
  meta->sar = meta_sar_init();
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
  // no information on slant and time shift
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
  meta->sar->pulse_duration = params->pulse_length;
  // no information on range sampling rate
  // FIXME: check polarizations for interferometric/polarimetric data
  // FIXME: multilook flag depends on data type
  
  // UAVSAR block
  meta->uavsar = meta_uavsar_init();
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
  
  return meta;
}
