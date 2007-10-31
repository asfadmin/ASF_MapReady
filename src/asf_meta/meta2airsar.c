#include "asf_meta.h"
#include "dateUtil.h"
#include "airsar.h"
#include "asf_nan.h"

meta_parameters* airsar2meta(airsar_general *general, 
			     airsar_parameters *params)
{
  meta_parameters *meta;
  hms_time hms;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  sprintf(meta->general->basename, "%s", general->scene_id);
  sprintf(meta->general->sensor, "AIRSAR");
  strcpy(meta->general->mode, general->mode);
  // no sensor mode
  if (strlen(params->processor)>0) {
    strcpy(meta->general->processor, "airsar ");
    strcat(meta->general->processor, params->processor);
  } else {
    strcpy(meta->general->processor, MAGIC_UNSET_STRING);
  }
  // FIXME: various data types - InSAR vs. polarimetric
  sprintf(meta->general->system, "big_ieee");
  date_sec2hms(params->acquisition_seconds, &hms);
  sprintf(meta->general->acquisition_date, "%s %d:%d:%.3lf",
	  params->acquisition_date, hms.hour, hms.min, hms.sec);
  // no orbit and orbit direction
  // no frame number
  // FIXME: decide how to separate interferometric/polarimetric data
  meta->general->band_count = 1;
  strcpy(meta->general->bands, "tbd");
  if (params) {
    meta->general->line_count = 
      params->line_count - params->first_data_offset / params->sample_count;
    meta->general->sample_count = params->sample_count;
  }
  else {
    meta->general->line_count = 
      general->length * 1000 / general->azimuth_pixel_spacing;
    meta->general->sample_count =
      general->width * 1000 / general->range_pixel_spacing;
  }
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  if (params) {
    meta->general->x_pixel_size = params->x_pixel_size;
    meta->general->y_pixel_size = params->y_pixel_size;
    meta->general->center_latitude = params->center_lat;
    meta->general->center_longitude = params->center_lon;
  }
  else {
    meta->general->x_pixel_size = general->range_pixel_spacing;
    meta->general->y_pixel_size = general->azimuth_pixel_spacing;
  }
  meta->general->re_major = 6378137.0; // WGS84
  meta->general->re_minor = 6356752.314; // WGS84
  // no information on bit error rate, missing lines and no data

  if (params) {
    // SAR block
    meta->sar = meta_sar_init();
    if (strncmp(uc(params->range_projection), "GROUND", 6) == 0)
      meta->sar->image_type = 'G';
    // no information on look direction
    if (params->deskewed == 1)
      meta->sar->deskewed = 1;
    else if (params->deskewed == 2)
      meta->sar->deskewed = 0;
    meta->sar->original_line_count = params->line_count;
    meta->sar->original_sample_count = params->sample_count;
    meta->sar->line_increment = 1;
    meta->sar->sample_increment = 1;
    // no information on azimuth and range time per pixel
    // no informaiton on slant and time shift
    meta->sar->slant_range_first_pixel = params->near_slant_range;
    meta->sar->wavelength = params->wavelength;
    meta->sar->prf = params->prf;
    // no information on earth radius and satellite height
    // no time information
    // no Doppler information
    meta->sar->azimuth_processing_bandwidth = params->chirp_bandwidth;
    // no chirp rate information
    meta->sar->pulse_duration = params->pulse_length;
    meta->sar->range_sampling_rate = params->range_sampling_rate;
    // FIXME: check polarizations for interferometric/polarimetric data
    // FIXME: multilook flag depends on data type
    
    // AirSAR block
    meta->airsar = meta_airsar_init();
    meta->airsar->scale_factor = params->scale_factor;
    meta->airsar->gps_altitude = params->gps_altitude;
    meta->airsar->lat_peg_point = params->lat_peg_point;
    meta->airsar->lon_peg_point = params->lon_peg_point;
    meta->airsar->head_peg_point = params->head_peg_point;
    meta->airsar->along_track_offset = params->along_track_offset;
    meta->airsar->cross_track_offset = params->cross_track_offset;
  }

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = general->corner1_lat;
  meta->location->lon_start_near_range = general->corner1_lon;
  meta->location->lat_start_far_range = general->corner2_lat;
  meta->location->lon_start_far_range = general->corner2_lon;
  meta->location->lat_end_near_range = general->corner4_lat;
  meta->location->lon_end_near_range = general->corner4_lon;
  meta->location->lat_end_far_range = general->corner3_lat;
  meta->location->lon_end_far_range = general->corner3_lon;

  return meta;
}
