#include "asf_meta.h"
#include "dateUtil.h"
#include "terrasar.h"
#include "asf_nan.h"

meta_parameters* terrasar2meta(terrasar_meta *terrasar)
{
  ymd_date date, imgStartDate, imgStopDate;
  hms_time time, imgStartTime, imgStopTime;
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  double lat, re, rp;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  strcpy(meta->general->basename, terrasar->filename);
  strcpy(meta->general->sensor, terrasar->mission);
  strcpy(meta->general->sensor_name, terrasar->sensor);
  strcpy(meta->general->mode, terrasar->elevationBeamConfiguration);
  strcpy(meta->general->processor, "");
  // terrsar->imagingMode determines stripMap, scanSAR and spotLight modes
  // not using the information yet
  meta->general->data_type = REAL32;
  if (strcmp_case(terrasar->imageDataType, "COMPLEX") == 0)
    meta->general->image_data_type = COMPLEX_IMAGE;
  else if (strcmp_case(terrasar->imageDataType, "DETECTED") == 0)
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  strcpy(meta->general->system, "big_ieee");
  date_terrasar2date(terrasar->azimuthTimeUTC, &date, &time);
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0lf",
	  date.day, mon[date.month], date.year, time.hour, time.min, time.sec);
  meta->general->orbit = terrasar->absOrbit;
  if (strcmp_case(terrasar->orbitDirection, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(terrasar->orbitDirection, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  meta->general->band_count = terrasar->numberOfLayers;
  meta->general->line_count = terrasar->numberOfRows;
  meta->general->sample_count = terrasar->numberOfColumns;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = terrasar->rangeResolution;
  meta->general->y_pixel_size = terrasar->azimuthResolution;
  meta->general->center_latitude = terrasar->sceneCenterCoordLat;
  meta->general->center_longitude = terrasar->sceneCenterCoordLon;
  meta->general->re_major = 6378137.000; // WGS84
  meta->general->re_minor = 6356752.314; // WGS84

  // SAR block
  meta->sar = meta_sar_init();
  if (strcmp_case(terrasar->projection, "SLANTRANGE") == 0)
    meta->sar->image_type = 'S';
  else if (strcmp_case(terrasar->projection, "GROUNDRANGE") == 0)
    meta->sar->image_type = 'G';
  // FIXME: MAP case not covered yet
  if (strcmp_case(terrasar->lookDirection, "LEFT") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(terrasar->lookDirection, "RIGHT") == 0)
    meta->sar->look_direction = 'R';
  meta->sar->look_count = terrasar->azimuthLooks;
  // TO BE ADDED: terrasar->rangeLooks
  if (strcmp_case(terrasar->imageCoordinateType, "ZERODOPPLER") == 0)
    meta->sar->deskewed = 1;
  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->range_time_per_pixel = terrasar->rowSpacing;
  meta->sar->azimuth_time_per_pixel = terrasar->columnSpacing;
  meta->sar->slant_range_first_pixel = terrasar->rangeTime * SPD_LIGHT / 2.0;
  meta->sar->slant_shift = 0.0;
  meta->sar->time_shift = 0.0;
  meta->sar->wavelength = SPD_LIGHT / terrasar->centerFrequency;
  meta->sar->prf = terrasar->prf;
  lat = meta->general->center_latitude * D2R;
  re = meta->general->re_major;
  rp = meta->general->re_minor;
  meta->sar->earth_radius = 
    (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  // FIXME: Doppler values need to be filled in
  meta->sar->azimuth_processing_bandwidth = 
    terrasar->totalProcessedAzimuthBandwidth;
  // FIXME: chirp_rate ???
  meta->sar->pulse_duration = terrasar->pulseLength;
  meta->sar->range_sampling_rate = terrasar->rsf;
  strcpy(meta->sar->polarization, terrasar->polarisationMode);
  if (strcmp_case(terrasar->imageDataType, "COMPLEX") == 0)
    meta->sar->multilook = FALSE;
  else if (strcmp_case(terrasar->imageDataType, "DETECTED") == 0)
    meta->sar->multilook = TRUE;
  // FIXME: pitch, roll, yaw ???

  // Doppler block
  meta->doppler = terrasar->doppler;

  // State vectors
  meta->state_vectors = terrasar->state_vectors;

  // Propagate the state vectors to start, center, end
  date_terrasar2date(terrasar->sceneStart, &imgStartDate, &imgStartTime);
  date_terrasar2date(terrasar->sceneStop, &imgStopDate, &imgStopTime);
  int vector_count = 3;
  double data_int = date_difference(&imgStopDate, &imgStopTime, 
				    &imgStartDate, &imgStartTime) / 2.0;
  while (fabs(data_int) > 10.0) {
    data_int /= 2;
    vector_count = vector_count*2-1;
  }
  propagate_state(meta, vector_count, data_int);

  data_int = date_difference(&imgStopDate, &imgStopTime, 
			     &imgStartDate, &imgStartTime) / 2.0;
  stateVector stVec = meta_get_stVec(meta, data_int);
  meta->sar->satellite_height = sqrt(stVec.pos.x * stVec.pos.x +
				     stVec.pos.y * stVec.pos.y +
				     stVec.pos.z * stVec.pos.z);

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = terrasar->sceneCornerCoord1Lat;
  meta->location->lon_start_near_range = terrasar->sceneCornerCoord1Lon;
  meta->location->lat_start_far_range = terrasar->sceneCornerCoord2Lat;
  meta->location->lon_start_far_range = terrasar->sceneCornerCoord2Lon;
  meta->location->lat_end_near_range = terrasar->sceneCornerCoord3Lat;
  meta->location->lon_end_near_range = terrasar->sceneCornerCoord3Lon;
  meta->location->lat_end_far_range = terrasar->sceneCornerCoord4Lat;
  meta->location->lon_end_far_range = terrasar->sceneCornerCoord4Lon;
    
  return meta;
}
