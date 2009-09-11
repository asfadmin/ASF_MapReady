#include "asf_meta.h"
#include "dateUtil.h"
#include "airsar.h"
#include "asf_nan.h"

meta_parameters* airsar2meta(airsar_header *header,
			     airsar_param_header *params,
			     airsar_dem_header *dem)
{
  meta_parameters *meta;
  hms_time hms;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  sprintf(meta->general->basename, "%s", params->site_name);
  sprintf(meta->general->sensor, "AIRSAR");
  strcpy(meta->general->sensor_name, "SAR");
  if (strlen(header->processor)>0) {
    header->processor[4] = '\0';
    strcpy(meta->general->processor, "JPL version ");
    strcat(meta->general->processor, header->processor);
  } else {
    strcpy(meta->general->processor, MAGIC_UNSET_STRING);
  }
  // FIXME: various data types - InSAR vs. polarimetric
  date_sec2hms(params->acquisition_seconds, &hms);
  sprintf(meta->general->acquisition_date, "%s %d:%d:%.3lf",
      params->acquisition_date, hms.hour, hms.min, hms.sec);
  meta->general->orbit = params->cct_id; // close enough
  // no frame number
  // FIXME: decide how to separate interferometric/polarimetric data
  meta->general->band_count = 1;
  meta->general->line_count = header->line_count;
    //header->line_count - header->first_data_offset / header->sample_count;
  meta->general->sample_count = header->sample_count;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = header->x_pixel_size;
  meta->general->y_pixel_size = header->y_pixel_size;
  meta->general->center_latitude = params->center_lat;
  meta->general->center_longitude = params->center_lon;
  meta->general->re_major = 6378137.0; // WGS84
  meta->general->re_minor = 6356752.314; // WGS84
  // no information on bit error rate, missing lines and no data

  // SAR block
  meta->sar = meta_sar_init();
  // The CCT type 'CM' stands for 'Compressed Stokes Matrix' which implies
  // that you have fully polarimetric data
  if (strncmp_case(params->cct_type, "CM", 2) == 0) {
    meta->general->image_data_type = POLARIMETRIC_IMAGE;
    meta->general->radiometry = r_SIGMA;
    strcpy(meta->sar->polarization, "QUAD-POL");
    meta->general->band_count = 8;
  }
  if (strncmp_case(header->range_projection, "GROUND", 6) == 0)
    meta->sar->image_type = 'G';
  // no information on look direction
  if (params->deskewed == 1)
    meta->sar->deskewed = 1;
  else if (params->deskewed == 2)
    meta->sar->deskewed = 0;
  meta->sar->original_line_count = header->line_count;
  meta->sar->original_sample_count = header->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  // no information on azimuth and range time per pixel
  // no information on slant and time shift
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
  meta->airsar->scale_factor = 1.0; // Bruce Chapman said so.
  meta->airsar->gps_altitude = params->gps_altitude;
  if (dem) {
    meta->airsar->lat_peg_point = dem->lat_peg_point;
    meta->airsar->lon_peg_point = dem->lon_peg_point;
    meta->airsar->head_peg_point = dem->head_peg_point;
    meta->airsar->along_track_offset = dem->along_track_offset;
    meta->airsar->cross_track_offset = dem->cross_track_offset;
  }
  else {
    meta->airsar->lat_peg_point = params->lat_peg_point;
    meta->airsar->lon_peg_point = params->lon_peg_point;
    meta->airsar->head_peg_point = params->head_peg_point;
    meta->airsar->along_track_offset = params->along_track_offset;
    meta->airsar->cross_track_offset = params->cross_track_offset;
  }
  // The DEM header of Rick's prime test data did not have a valid peg point.
  // Without that the other numbers (along-track and cross-track offsets
  // as well as corner coordinates) don't make sense.
  // Will take the numbers out of the parameter header. Seems to work fine
  // with the prime test data set. Extract only elevation offset and increment
  // out of the DEM header
  if (dem) {
    meta->airsar->elevation_increment = dem->elevation_increment;
    meta->airsar->elevation_offset = dem->elevation_offset;
  }

  // Location block
  meta->location = meta_location_init();
  if (dem) {
    meta->location->lat_start_near_range = dem->corner1_lat;
    meta->location->lon_start_near_range = dem->corner1_lon;
    meta->location->lat_start_far_range = dem->corner2_lat;
    meta->location->lon_start_far_range = dem->corner2_lon;
    meta->location->lat_end_near_range = dem->corner4_lat;
    meta->location->lon_end_near_range = dem->corner4_lon;
    meta->location->lat_end_far_range = dem->corner3_lat;
    meta->location->lon_end_far_range = dem->corner3_lon;
  }
    
  return meta;
}
