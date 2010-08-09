#include "asf_meta.h"
#include "meta_init.h"
#include "dateUtil.h"
#include "radarsat2.h"
#include "asf_nan.h"

meta_parameters* radarsat2meta(radarsat2_meta *radarsat2)
{

  ymd_date date, imgStartDate, imgStopDate;
  hms_time time, imgStartTime, imgStopTime;
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  double lat, lon, height, re, rp;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  strcpy(meta->general->basename, radarsat2->filename);
  strcpy(meta->general->sensor, radarsat2->satellite);
  strcpy(meta->general->sensor_name, radarsat2->sensor);
  strcpy(meta->general->mode, radarsat2->beamModeMnemonic);
  sprintf(meta->general->processor, "%s %s", 
	  radarsat2->processingFacility, radarsat2->softwareVersion);
  meta->general->data_type = REAL32;
  if (strcmp_case(radarsat2->dataType, "COMPLEX") == 0)
    meta->general->image_data_type = COMPLEX_IMAGE;
  //else if (strcmp_case(radarsat2->dataType, "DETECTED") == 0)
  //  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  date_terrasar2date(radarsat2->zeroDopplerAzimuthTime, &date, &time);
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0lf",
	  date.day, mon[date.month], date.year, time.hour, time.min, time.sec);
  //meta->general->orbit = radarsat2->absOrbit;
  if (strcmp_case(radarsat2->passDirection, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(radarsat2->passDirection, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  if (strcmp_case(radarsat2->dataType, "COMPLEX") == 0)
    meta->general->band_count = radarsat2->band_count * 2;
  else
    meta->general->band_count = radarsat2->band_count;
  strcpy(meta->general->bands, radarsat2->bands);
  meta->general->line_count = radarsat2->numberOfLines;
  meta->general->sample_count = radarsat2->numberOfSamplesPerLine;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = radarsat2->sampledPixelSpacing;
  meta->general->y_pixel_size = radarsat2->sampledLineSpacing;
  //meta->general->center_latitude = radarsat2->sceneCenterCoordLat;
  //meta->general->center_longitude = radarsat2->sceneCenterCoordLon;
  meta->general->re_major = radarsat2->semiMajorAxis;
  meta->general->re_minor = radarsat2->semiMinorAxis;

  // SAR block
  meta->sar = meta_sar_init();
  if (strcmp_case(radarsat2->productType, "SLC") == 0)
    meta->sar->image_type = 'S';
  if (strcmp_case(radarsat2->antennaPointing, "LEFT") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(radarsat2->antennaPointing, "RIGHT") == 0)
    meta->sar->look_direction = 'R';
  //meta->sar->look_count = 4;
  meta->sar->look_count = radarsat2->numberOfAzimuthLooks;
  // TO BE ADDED: radarsat2->numberOfRangeLooks
  meta->sar->deskewed = 1;
  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  date_terrasar2date(radarsat2->zeroDopplerTimeFirstLine, 
		     &imgStartDate, &imgStartTime);
  date_terrasar2date(radarsat2->zeroDopplerTimeLastLine, 
		     &imgStopDate, &imgStopTime);
  meta->sar->azimuth_time_per_pixel =
    date_difference(&imgStopDate, &imgStopTime, 
		    &imgStartDate, &imgStartTime) / meta->general->line_count;
  meta->sar->range_time_per_pixel = 
    meta->general->x_pixel_size * 2.0 / speedOfLight;
  meta->sar->slant_range_first_pixel = radarsat2->slantRangeNearEdge;
  meta->sar->slant_shift = 0.0;
  meta->sar->time_shift = 0.0;
  meta->sar->wavelength = SPD_LIGHT / radarsat2->radarCenterFrequency;
  meta->sar->prf = radarsat2->pulseRepetitionFrequency;
  meta->sar->satellite_height = radarsat2->satelliteHeight;
  meta->sar->azimuth_processing_bandwidth = 
    radarsat2->totalProcessedAzimuthBandwidth;
  // FIXME: chirp_rate ???
  meta->sar->pulse_duration = radarsat2->pulseLength;
  meta->sar->range_sampling_rate = radarsat2->adcSamplingRate;
  strcpy(meta->sar->polarization, radarsat2->polarizations);
  if (strcmp_case(radarsat2->dataType, "COMPLEX") == 0)
    meta->sar->multilook = FALSE;
  // FIXME: pitch, roll, yaw ???

  // Doppler block
  meta->doppler = radarsat2->doppler;

  // State vectors
  meta->state_vectors = radarsat2->state_vectors;

  // Propagate the state vectors to start, center, end
  int vector_count = 3;
  double data_int = date_difference(&imgStopDate, &imgStopTime, 
				    &imgStartDate, &imgStartTime) / 2.0;
  while (fabs(data_int) > 10.0) {
    data_int /= 2;
    vector_count = vector_count*2-1;
  }
  propagate_state(meta, vector_count, data_int);

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = radarsat2->sceneCornerCoord1Lat;
  meta->location->lon_start_near_range = radarsat2->sceneCornerCoord1Lon;
  meta->location->lat_start_far_range = radarsat2->sceneCornerCoord2Lat;
  meta->location->lon_start_far_range = radarsat2->sceneCornerCoord2Lon;
  meta->location->lat_end_near_range = radarsat2->sceneCornerCoord3Lat;
  meta->location->lon_end_near_range = radarsat2->sceneCornerCoord3Lon;
  meta->location->lat_end_far_range = radarsat2->sceneCornerCoord4Lat;
  meta->location->lon_end_far_range = radarsat2->sceneCornerCoord4Lon;

  // Still need to determine center location, really only needed to get
  // the earth radius straight
  meta->general->center_longitude = (radarsat2->sceneCornerCoord1Lon +
				     radarsat2->sceneCornerCoord2Lon +
				     radarsat2->sceneCornerCoord3Lon +
				     radarsat2->sceneCornerCoord4Lon) / 4.0;
  location_to_latlon(meta, meta->general->sample_count/2, 
		     meta->general->line_count/2, 0.0, &lat, &lon, &height);
  meta->general->center_latitude = lat;
  meta->general->center_longitude = lon;
  lat = meta->general->center_latitude * D2R;
  re = meta->general->re_major;
  rp = meta->general->re_minor;
  meta->sar->earth_radius = 
    (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  meta->sar->satellite_height += meta->sar->earth_radius;
    
  return meta;
}
