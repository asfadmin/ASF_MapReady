#include "asf_meta.h"
#include "meta_init.h"
#include "dateUtil.h"
#include "smap.h"
#include "asf_nan.h"

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
  strcpy(meta->general->basename, smap->granule_name);
  strcpy(meta->general->sensor, smap->project_id);
  strcpy(meta->general->sensor_name, "L-band radar");
  strcpy(meta->general->mode, smap->short_name);
  // meta->general->processor;
  meta->general->data_type = REAL32;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_SIGMA_DB;
  sscanf(smap->range_beginning_date, "%4d-%2d-%2d", 
	 &imgStartDate.year, &imgStartDate.month, &imgStartDate.day);
  sscanf(smap->range_beginning_time, "%2d:%2d:%fZ",
	 &imgStartTime.hour, &imgStartTime.min, &imgStartTime.sec);
  sscanf(smap->range_ending_date, "%4d-%2d-%2d", 
	 &imgStopDate.year, &imgStopDate.month, &imgStopDate.day);
  sscanf(smap->range_ending_time, "%2d:%2d:%fZ",
	 &imgStopTime.hour, &imgStopTime.min, &imgStopTime.sec);
  average_ymdTimes(&imgStartDate, &imgStopDate, &imgStartTime, &imgStopTime,
		   &date, &time);
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0lf",
	  date.day, mon[date.month], date.year, time.hour, time.min, time.sec);
  meta->general->orbit = smap->start_orbit_number;
  if (strcmp_case(smap->orbit_direction, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(smap->orbit_direction, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  meta->general->frame = -1;
  meta->general->line_scaling = 1;
  meta->general->sample_scaling = 1;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = smap->grid_spacing * 1000;
  meta->general->y_pixel_size = smap->grid_spacing * 1000;
  meta->general->re_major = 6371228.0; // apparently constant earth radius
  meta->general->re_minor = 6371228.0;

  // SAR block
  meta->sar = meta_sar_init();
  meta->sar->image_type = 'P'; // geographic but nevertheless
  meta->sar->look_direction = 'R';
  meta->sar->look_count = 1; // Number of looks?
  meta->sar->deskewed = 1; // assumption
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->wavelength =  0.23793; // from website 1.62 GHz
  strcpy(meta->sar->polarization, "HH,HV,VV"); // from website
  meta->sar->multilook = 1; // as per discussion with Barry

  return meta;
}
