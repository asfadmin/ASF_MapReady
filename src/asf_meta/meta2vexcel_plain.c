#include "asf_meta.h"
#include "dateUtil.h"
#include "vexcel_plain.h"
#include "asf_nan.h"

meta_parameters* vp2meta(vexcel_plain *vp)
{
  vp_doppler_centroid_parameters doppler_params;
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  ymd_date ymd, ref_date;
  hms_time time, ref_time;
  julian_date jd;
  int i, nStVec;

  // Initialize the meta structure
  meta = raw_init();

  // Fill general block
  strcpy(meta->general->basename, vp->gli_product.image_desc.title);
  strcpy(meta->general->sensor, vp->sensor.instrument_name);
  strcpy(meta->general->sensor_name, vp->sensor.sensor_name);
  // FIXME: need to handle multibeam data
  strcpy(meta->general->mode, vp->sensor.beam[0].beam_name);
  strcpy(meta->general->processor, vp->gli_product.processor_name);
  if (vp->gli_product.image_desc.bytes_per_pixel == 1)
    meta->general->data_type = BYTE;
  else if (vp->gli_product.image_desc.bytes_per_pixel == 2)
    meta->general->data_type = INTEGER16;
  else if (vp->gli_product.image_desc.bytes_per_pixel == 4)
    meta->general->data_type = REAL32;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  date_dssr2date(vp->gli_product.orbit_nr_date, &ymd, &time);
  sprintf(meta->general->acquisition_date, "%2d-%s-%4d", 
	  ymd.day, mon[ymd.month], ymd.year);
  meta->general->orbit = vp->gli_product.orbit_nr;
  if (strncmp(vp->flight_path_direction, "ASCENDING", 9) == 0)
    meta->general->orbit_direction = 'A';
  else
    meta->general->orbit_direction = 'D';
  meta->general->frame = MAGIC_UNSET_INT;
  meta->general->band_count = vp->sensor.nr_beams;
  strcpy(meta->general->bands, "AMP");
  meta->general->line_count = vp->gli_product.image_desc.nr_lines;;
  meta->general->sample_count = vp->gli_product.image_desc.nr_pixels;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = vp->gli_product.image_desc.line_spacing;
  meta->general->y_pixel_size = vp->gli_product.image_desc.pixel_spacing;
  meta->general->center_latitude = 
    vp->gli_product.image_desc.coord.center_line_center_pixel.lat;
  meta->general->center_longitude = 
    vp->gli_product.image_desc.coord.center_line_center_pixel.lon;;
  meta->general->re_major = 
    vp->gli_product.image_desc.coord.earth_model.major;
  meta->general->re_minor = 
    vp->gli_product.image_desc.coord.earth_model.minor;
  meta->general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  meta->general->missing_lines = 0;
  meta->general->no_data = MAGIC_UNSET_DOUBLE;

  // Fill SAR block
  meta->sar = meta_sar_init();
  // working with assumptions here
  meta->sar->image_type = 'G';
  if (vp->sensor.clock_angle >= 0.0)
    meta->sar->look_direction = 'R';
  else
    meta->sar->look_direction = 'L';
  meta->sar->look_count = vp->gli_product.azimuth_looks;
  meta->sar->deskewed = vp->gli_product.skew_flag;
  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->range_time_per_pixel = 
    fabs((2.0 * vp->gli_product.image_desc.pixel_spacing) / SPD_LIGHT);
  meta->sar->azimuth_time_per_pixel = vp->gli_product.time_per_line;
  meta->sar->slant_range_first_pixel = vp->gli_product.near_range;
  meta->sar->slant_shift = 0.0;
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = 
      fabs(meta->sar->original_line_count * meta->sar->azimuth_time_per_pixel);
  else
    meta->sar->time_shift = MAGIC_UNSET_DOUBLE;
  meta->sar->wavelength = SPD_LIGHT / vp->sensor.beam[0].carrier_freq;
  meta->sar->prf = vp->sensor.beam[0].prf;
  meta->sar->earth_radius_pp = MAGIC_UNSET_DOUBLE;
  strcpy(meta->sar->satellite_binary_time, MAGIC_UNSET_STRING);
  strcpy(meta->sar->satellite_clock_time, MAGIC_UNSET_STRING);
  doppler_params = vp->sensor.beam[0].doppler_centroid_parameters;
  // FIXME: Check units for higher coefficients
  meta->sar->range_doppler_coefficients[0] =
    doppler_params.doppler_centroid_coefficients.a[0];
  meta->sar->range_doppler_coefficients[1] =
    doppler_params.doppler_centroid_coefficients.a[1];
  meta->sar->range_doppler_coefficients[2] =
    doppler_params.doppler_centroid_coefficients.a[2];

  for (i=0; i<3; i++)
    meta->sar->azimuth_doppler_coefficients[i] = 0.0;
  meta->sar->azimuth_processing_bandwidth = 
    vp->gli_product.processor_bandwidth;
  meta->sar->chirp_rate = vp->sensor.beam[0].chirp_rate;
  meta->sar->pulse_duration = vp->sensor.beam[0].pulse_length;
  meta->sar->range_sampling_rate = vp->sensor.beam[0].sampling_freq;
  strcpy(meta->sar->polarization,
	 vp->sensor.beam[0].polarization_block.polarization[0].polarization);
  meta->sar->multilook = 1;
  meta->sar->pitch = vp->sensor.ephemeris.attitude.pitch;
  meta->sar->roll = vp->sensor.ephemeris.attitude.roll;
  meta->sar->yaw = vp->sensor.ephemeris.attitude.yaw;

  // Fill state vector structure
  nStVec = vp->sensor.ephemeris.sv_block.nr_sv;
  meta->state_vectors = meta_state_vectors_init(nStVec);
  date_dssr2date(vp->sensor.ephemeris.sv_block.state_vector[0].date, 
		 &ref_date, &ref_time);
  meta->state_vectors->year = ref_date.year;
  date_ymd2jd(&ref_date, &jd);
  meta->state_vectors->julDay = jd.jd;
  meta->state_vectors->second = date_hms2sec(&ref_time);
  meta->state_vectors->vector_count = nStVec;
  for (i=0; i<nStVec; i++) {
    date_dssr2date(vp->sensor.ephemeris.sv_block.state_vector[i].date, 
		   &ymd, &time);
    meta->state_vectors->vecs[i].time = 
      date_difference(&ref_date, &ref_time, &ymd, &time);
    meta->state_vectors->vecs[i].vec.pos.x = 
      vp->sensor.ephemeris.sv_block.state_vector[i].x;
    meta->state_vectors->vecs[i].vec.pos.y =
      vp->sensor.ephemeris.sv_block.state_vector[i].y;
    meta->state_vectors->vecs[i].vec.pos.z =
      vp->sensor.ephemeris.sv_block.state_vector[i].z;
    meta->state_vectors->vecs[i].vec.vel.x = 
      vp->sensor.ephemeris.sv_block.state_vector[i].xv;
    meta->state_vectors->vecs[i].vec.vel.y =
      vp->sensor.ephemeris.sv_block.state_vector[i].yv;
    meta->state_vectors->vecs[i].vec.vel.z =
      vp->sensor.ephemeris.sv_block.state_vector[i].zv;
  }
  date_dssr2date(vp->gli_product.first_line, &ymd, &time);
  double shift = date_difference(&ref_date, &ref_time, &ymd, &time);
  int sign;
  if (compare_time(&ymd, &time, &ref_date, &ref_time) > 0)
    sign = -1;
  else
    sign = 1;
  for (i=0; i<nStVec; i++)
    meta->state_vectors->vecs[i].time += sign * shift;
  meta->state_vectors->second = date_hms2sec(&time);
  double interval = 
    meta->general->line_count * meta->sar->azimuth_time_per_pixel / 2;
  propagate_state(meta, 3, interval);
  meta->sar->earth_radius = 
    meta_get_earth_radius(meta, meta->general->line_count/2, 
			  meta->general->sample_count/2);
  meta->sar->satellite_height = 
    meta_get_sat_height(meta, meta->general->line_count/2,
			meta->general->sample_count/2);  

  // Fill location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range =
    vp->gli_product.image_desc.coord.first_line_first_pixel.lat;
  meta->location->lon_start_near_range =
    vp->gli_product.image_desc.coord.first_line_first_pixel.lon;
  meta->location->lat_start_far_range =
    vp->gli_product.image_desc.coord.first_line_last_pixel.lat;
  meta->location->lon_start_far_range =
    vp->gli_product.image_desc.coord.first_line_last_pixel.lon;
  meta->location->lat_end_near_range =
    vp->gli_product.image_desc.coord.last_line_first_pixel.lat;
  meta->location->lon_end_near_range =
    vp->gli_product.image_desc.coord.last_line_first_pixel.lon;
  meta->location->lat_end_far_range =
    vp->gli_product.image_desc.coord.last_line_last_pixel.lat;
  meta->location->lon_end_far_range =
    vp->gli_product.image_desc.coord.last_line_last_pixel.lon;

  return meta;
}
