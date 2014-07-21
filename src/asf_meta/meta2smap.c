#include "asf_meta.h"
#include "meta_init.h"
#include "dateUtil.h"
#include "smap.h"
#include "asf_nan.h"

static void read_smap_pixel(meta_parameters *meta, int line, int sample,
  float *lat, float *lon)
{
  int ii;
  int ns = meta->general->sample_count;

  // determine latitude
  if (sample == 0) {
    ii = line*ns;
    while (!meta_is_valid_double(meta->latlon->lat[ii]) && (ii < ns))
      ii++;
    *lat = meta->latlon->lat[ii];
  }
  else if (sample == ns-1) {
    ii = line*ns + sample;
    while (!meta_is_valid_double(meta->latlon->lat[ii]) && (ii >= 0))
      ii--;
    *lat = meta->latlon->lat[ii];
  }
  else
    *lat = meta->latlon->lat[line*ns+sample];

  // determine longitude
  if (sample == 0) {
    ii = line*ns;
    while (!meta_is_valid_double(meta->latlon->lon[ii]) && (ii < ns))
      ii++;
    *lon = meta->latlon->lon[ii];
  }
  else if (sample == ns-1) {
    ii = line*ns + sample;
    while (!meta_is_valid_double(meta->latlon->lon[ii]) && (ii >= 0))
      ii--;
    *lon = meta->latlon->lon[ii];
  }
  else
    *lon = meta->latlon->lon[line*ns+sample];  
}

void update_smap_geolocation(char *inDataName, char *outDataName) 
{
	float lat, lon;
	copyImgAndMeta(inDataName, outDataName);
	meta_parameters *meta = meta_read(inDataName);

  // Set background value
  meta->general->no_data = 0.0;

  // Reset subset position
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  
	int startLine = meta->general->start_line;
	int startSample = meta->general->start_sample;
	int endLine = meta->general->line_count - 1;
	int endSample = meta->general->sample_count - 1;
	int centerLine = (endLine - startLine) / 2;
	int centerSample = (endSample - startSample) / 2;

	// Update center coordinates
	read_smap_pixel(meta, centerLine, centerSample, &lat, &lon);
	meta->general->center_latitude = (double) lat;
	meta->general->center_longitude = (double) lon;

	// Update location block
  if (!meta->location)
  	meta->location = meta_location_init();
  read_smap_pixel(meta, startLine, startSample, &lat, &lon);
  meta->location->lat_start_near_range = (double) lat;
  meta->location->lon_start_near_range = (double) lon;
  read_smap_pixel(meta, startLine, endSample, &lat, &lon);
  meta->location->lat_start_far_range = (double) lat;
  meta->location->lon_start_far_range = (double) lon;
  read_smap_pixel(meta, endLine, startSample, &lat, &lon);
  meta->location->lat_end_near_range = (double) lat;
  meta->location->lon_end_near_range = (double) lon;
  read_smap_pixel(meta, endLine, endSample, &lat, &lon);
  meta->location->lat_end_far_range = (double) lat;
  meta->location->lon_end_far_range = (double) lon;
  
  meta_write(meta, outDataName);
  meta_free(meta);
}


meta_parameters* smap2meta(smap_meta *smap)
{

  ymd_date date, imgStartDate, imgStopDate;
  hms_time time, imgStartTime, imgStopTime;
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  strcpy(meta->general->basename, smap->file_name);
  strcpy(meta->general->sensor, "SMAP");
  strcpy(meta->general->sensor_name, "L-band radar");
  strcpy(meta->general->mode, smap->short_name);
  // meta->general->processor
  meta->general->data_type = REAL32;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  sscanf(smap->orbit_start_date_time, "%4d-%2d-%2dT%2d:%2d:%lfZ", 
		&imgStartDate.year, &imgStartDate.month, &imgStartDate.day,
  	&imgStartTime.hour, &imgStartTime.min, &imgStartTime.sec);
  sscanf(smap->orbit_stop_date_time, "%4d-%2d-%2dT%2d:%2d:%lfZ", 
		&imgStopDate.year, &imgStopDate.month, &imgStopDate.day,
  	&imgStopTime.hour, &imgStopTime.min, &imgStopTime.sec);
  average_ymdTimes(&imgStartDate, &imgStopDate, &imgStartTime, &imgStopTime,
		&date, &time);
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0f",
	  date.day, mon[date.month], date.year, time.hour, time.min, time.sec);
  // meta->general->orbit
  if (strcmp_case(smap->orbit_direction, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(smap->orbit_direction, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  meta->general->frame = -1;
  meta->general->line_scaling = 1;
  meta->general->sample_scaling = 1;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = 1000;
  meta->general->y_pixel_size = 1000;
  meta->general->re_major = 6371228.0; // apparently constant earth radius
  meta->general->re_minor = 6371228.0;

  // SAR block
  meta->sar = meta_sar_init();
  meta->sar->image_type = 'R';
  meta->sar->look_direction = 'R';
  // No constant look count information - rotating antenna
  meta->sar->deskewed = 1; // assumption
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->wavelength =  0.23793; // from website 1.62 GHz
  strcpy(meta->sar->polarization, "HH,HV,VV"); // from website
  meta->sar->multilook = 1; // as per discussion with Barry

  return meta;
}
