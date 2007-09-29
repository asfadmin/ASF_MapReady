#include "asf_meta.h"
#include "dateUtil.h"
#include "gamma.h"
#include "asf_nan.h"

meta_parameters* gamma_isp2meta(gamma_isp *gamma, meta_state_vectors *stVec)
{
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};

  // Initialize the meta structure
  meta = raw_init();

  // Fill general block
  strcpy(meta->general->basename, gamma->title); // FIXME: Shouldn't use "title" ...use basename of data and parms files
  strcpy(meta->general->sensor, gamma->sensor);
  // no sensor name, mode and processor          // FIXME: Set to MAGIC_UNSET_STRING
  if (strncmp(gamma->image_format, "FCOMPLEX", 8) == 0) // FIXME: Use full list of data types in Gamma's type .h file
    meta->general->data_type = COMPLEX_REAL32;
  meta->general->image_data_type = COMPLEX_IMAGE;  // FIXME: Base this on the image_format
  strcpy(meta->general->system, meta_get_system());
  sprintf(meta->general->acquisition_date, "%2d-%s-%4d",
    gamma->acquisition[2], mon[gamma->acquisition[1]],
    gamma->acquisition[0]);
  // no orbit number
  // orbit direction from heading or azimuth angle?
  // no frame number
  meta->general->band_count = 1;
  // no bands information
  meta->general->line_count = gamma->azimuth_lines;
  meta->general->sample_count = gamma->range_samples;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = gamma->range_pixel_spacing;
  meta->general->y_pixel_size = gamma->azimuth_pixel_spacing;
  meta->general->center_latitude = gamma->center_latitude;
  meta->general->center_longitude = gamma->center_longitude;
  meta->general->re_major = gamma->earth_semi_major_axis;
  meta->general->re_minor = gamma->earth_semi_minor_axis;
  // no information on bit error rate, missing lines and no data

  // Fill SAR block
  meta->sar = meta_sar_init();
  if (strncmp(gamma->image_geometry, "SLANT_RANGE", 11) == 0)
    meta->sar->image_type = 'S';
  meta->sar->look_direction = 'R';
  meta->sar->look_count = gamma->azimuth_looks;
  meta->sar->deskewed = gamma->azimuth_deskew;
  meta->sar->original_line_count = 0;
  meta->sar->original_sample_count = 0;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  // no range time per pixel
  meta->sar->azimuth_time_per_pixel = gamma->azimuth_line_time;
  meta->sar->slant_range_first_pixel = gamma->near_range_slc;
  meta->sar->slant_shift = 0.0;
  meta->sar->time_shift = 0.0;
  meta->sar->wavelength = SPD_LIGHT / gamma->radar_frequency;
  meta->sar->prf = gamma->prf;
  meta->sar->earth_radius = gamma->earth_radius_below_sensor;
  // earth radius pp ignored
  meta->sar->satellite_height = gamma->sar_to_earth_center;
  // satellites times ignored
  // FIXME: work out Doppler values
  double doppler_polynomial[4];           // Doppler polynomial [Hz]
  double doppler_poly_dot[4];             // Doppler rate polynomial [Hz]
  double doppler_poly_ddot[4];            // Doppler second order [Hz]
  meta->sar->azimuth_processing_bandwidth = gamma->azimuth_proc_bandwidth;
  meta->sar->chirp_rate = gamma->chirp_bandwidth;
  // no pulse duration
  meta->sar->range_sampling_rate = gamma->adc_sampling_rate;
  // no polarization
  meta->sar->multilook = 0;

  // Fill state vector structure
  meta->state_vectors = meta_state_vectors_init(3);
  meta->state_vectors = stVec;

  return meta;
}

gamma_isp* meta2gamma_isp(meta_parameters *meta)
{
  gamma_isp *gamma = MALLOC(sizeof(gamma_isp));
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                 "Oct","Nov","Dec"};
  char str[5];
  int ii;

  strcpy(gamma->title, meta->general->basename);
  strcpy(gamma->sensor, meta->general->sensor);
  sscanf(meta->general->acquisition_date, "%d-%s-%d",
   &gamma->acquisition[2], str, &gamma->acquisition[0]);
  for (ii=1; ii<13; ii++) {
    if (strcmp(str, mon[ii]) == 0)
      gamma->acquisition[1] = ii;
  }
  gamma->start_time = meta->state_vectors->vecs[0].time;
  gamma->end_time = meta->state_vectors->vecs[2].time;
  gamma->center_time =
    gamma->start_time + (gamma->end_time - gamma->start_time) / 2;
  gamma->azimuth_line_time = meta->sar->azimuth_time_per_pixel;
  gamma->line_header_size = 0;
  gamma->range_samples = meta->general->sample_count;
  gamma->azimuth_lines = meta->general->line_count;
  gamma->range_looks = 1;
  gamma->azimuth_looks = meta->sar->look_count;
  if (meta->general->data_type == COMPLEX_REAL32)
    strcpy(gamma->image_format, "FCOMPLEX");
  if (meta->sar->image_type == 'S')
    strcpy(gamma->image_geometry, "SLANT_RANGE");
  gamma->range_scale_factor = 1;
  gamma->azimuth_scale_factor = 1;
  gamma->center_latitude = meta->general->center_latitude;
  gamma->center_longitude = meta->general->center_longitude;
  gamma->heading = 0.0;
  gamma->range_pixel_spacing = meta->general->x_pixel_size;
  gamma->azimuth_pixel_spacing = meta->general->y_pixel_size;
  gamma->near_range_slc = meta->sar->slant_range_first_pixel;
  gamma->center_range_slc = gamma->near_range_slc +
    gamma->range_samples/2 * gamma->range_pixel_spacing;
  gamma->far_range_slc = gamma->near_range_slc +
    gamma->range_samples * gamma->range_pixel_spacing;
  // FIXME: Work out polynomial stuff
  double first_slant_range_polynomial[6]; // First slant range polynomial
  double center_slant_range_polynomial[6];// Center slant range polynomial
  double last_slant_range_polynomial[6];  // Last slant range polynomial
  gamma->incidence_angle =
    meta_incid(meta, gamma->azimuth_lines/2, gamma->range_samples/2);
  gamma->azimuth_deskew = meta->sar->deskewed;
  // FIXME: Work out azimuth angle
  double azimuth_angle;                   // Azimuth angle [degrees]
  gamma->radar_frequency = SPD_LIGHT / meta->sar->wavelength;
  gamma->adc_sampling_rate = meta->sar->range_sampling_rate;
  gamma->chirp_bandwidth = meta->sar->chirp_rate;
  gamma->prf = meta->sar->prf;
  gamma->azimuth_proc_bandwidth = meta->sar->azimuth_processing_bandwidth;
  // FIXME: Work out Doppler polynomial
  double doppler_polynomial[4];           // Doppler polynomial [Hz]
  double doppler_poly_dot[4];             // Doppler rate polynomial [Hz]
  double doppler_poly_ddot[4];            // Doppler second order [Hz]
  gamma->receiver_gain = 0.0;
  gamma->calibration_gain = 0.0;
  gamma->sar_to_earth_center = meta->sar->satellite_height;
  gamma->earth_radius_below_sensor = meta->sar->earth_radius;
  gamma->earth_semi_major_axis = meta->general->re_major;
  gamma->earth_semi_minor_axis = meta->general->re_minor;

  return gamma;
}

void write_gamma_isp_header(const char *inFile, gamma_isp *gamma,
          meta_state_vectors *stVec)
{
  FILE *fp;
  int ii;
  double interval;

  fp = FOPEN(inFile, "w");
  fprintf(fp, "Gamma Interferometric SAR Processor (ISP) - "
    "Image Parameter File\n\n");
  fprintf(fp, "title:\t%s\n", gamma->title);
  fprintf(fp, "sensor:\t%s\n", gamma->sensor);
  fprintf(fp, "date: \t%d\t%d\t%d\n", gamma->acquisition[0],
    gamma->acquisition[1], gamma->acquisition[2]);
  fprintf(fp, "start_time:\t\t%15.5lf   s\n", gamma->start_time);
  fprintf(fp, "center_time:\t\t%15.5lf   s\n", gamma->center_time);
  fprintf(fp, "end_time:\t\t%15.5lf   s\n", gamma->end_time);
  fprintf(fp, "azimuth_line_time:\t%e  s\n", gamma->azimuth_line_time);
  fprintf(fp, "line_header_size:\t\t%7d\n", gamma->line_header_size);
  fprintf(fp, "range_samples:\t\t%7d\n", gamma->range_samples);
  fprintf(fp, "azimuth_lines:\t\t%7d\n", gamma->azimuth_lines);
  fprintf(fp, "range_looks:\t\t\t%7d\n", gamma->range_looks);
  fprintf(fp, "azimuth_looks:\t\t%7d\n", gamma->azimuth_looks);
  fprintf(fp, "image_format:\t\t\t%s\n", gamma->image_format);
  fprintf(fp, "image_geometry:\t\t%s\n", gamma->image_geometry);
  fprintf(fp, "range_scale_factor:\t%e\n", gamma->range_scale_factor);
  fprintf(fp, "azimuth_scale_factor:\t%e\n", gamma->azimuth_scale_factor);
  fprintf(fp, "center_latitude:\t\t%12.7lf   degrees\n",
    gamma->center_latitude);
  fprintf(fp, "center_longitude:\t\t%12.7lf   degrees\n",
    gamma->center_longitude);
  fprintf(fp, "heading:\t\t\t%12.7lf   degrees\n", gamma->heading);
  fprintf(fp, "range_pixel_spacing:\t\t%12.6lf   m\n",
    gamma->range_pixel_spacing);
  fprintf(fp, "azimuth_pixel_spacing:\t%12.6lf   m\n",
    gamma->azimuth_pixel_spacing);
  fprintf(fp, "near_range_slc:\t\t%12.4lf   m\n", gamma->near_range_slc);
  fprintf(fp, "center_range_slc:\t\t%12.4lf   m\n", gamma->center_range_slc);
  fprintf(fp, "far_range_slc:\t\t%12.4lf   m\n", gamma->far_range_slc);
  fprintf(fp, "first_slant_range_polynomial: %12.5lf %12.5lf %e %e %e %e  "
    "s m 1 m^-1 m^-2 m^-3\n",
    gamma->first_slant_range_polynomial[0],
    gamma->first_slant_range_polynomial[1],
    gamma->first_slant_range_polynomial[2],
    gamma->first_slant_range_polynomial[3],
    gamma->first_slant_range_polynomial[4],
    gamma->first_slant_range_polynomial[5]);
  fprintf(fp, "center_slant_range_polynomial: %12.5lf %12.5lf %e %e %e %e"
    "  s m 1 m^-1 m^-2 m^-3\n",
          gamma->center_slant_range_polynomial[0],
          gamma->center_slant_range_polynomial[1],
          gamma->center_slant_range_polynomial[2],
          gamma->center_slant_range_polynomial[3],
          gamma->center_slant_range_polynomial[4],
          gamma->center_slant_range_polynomial[5]);
  fprintf(fp, "last_slant_range_polynomial: %12.5lf %12.5lf %e %e %e %e  "
          "s m 1 m^-1 m^-2 m^-3\n",
          gamma->last_slant_range_polynomial[0],
          gamma->last_slant_range_polynomial[1],
          gamma->last_slant_range_polynomial[2],
          gamma->last_slant_range_polynomial[3],
          gamma->last_slant_range_polynomial[4],
          gamma->last_slant_range_polynomial[5]);
  fprintf(fp, "incidence_angle:\t\t%12.4lf   degrees\n",
    gamma->incidence_angle);
  if (gamma->azimuth_deskew == 1)
    fprintf(fp, "azimuth_deskew:          ON\n");
  else
    fprintf(fp, "azimuth_deskew:          OFF\n");
  fprintf(fp, "azimuth_angle:\t%15.4lf   degrees\n", gamma->azimuth_angle);
  fprintf(fp, "radar_frequency:\t%15e   Hz\n", gamma->radar_frequency);
  fprintf(fp, "adc_sampling_rate:\t%15e   Hz\n", gamma->adc_sampling_rate);
  fprintf(fp, "chirp_bandwidth:\t%15e   Hz\n", gamma->chirp_bandwidth);
  fprintf(fp, "prf:\t\t\t%14.5lf   Hz\n", gamma->prf);
  fprintf(fp, "azimuth_proc_bandwidth:%13.5lf   Hz\n",
    gamma->azimuth_proc_bandwidth);
  fprintf(fp, "doppler_polynomial:\t%14.5lf %e %e %e    "
    "Hz     Hz/m     Hz/m^2     Hz/m^3\n",
    gamma->doppler_polynomial[0],
          gamma->doppler_polynomial[1],
          gamma->doppler_polynomial[2],
          gamma->doppler_polynomial[3]);
  fprintf(fp, "doppler_poly_dot:\t%12.5lf %e %e %e    "
          "Hz/s     Hz/s/m     Hz/s/m^2     Hz/s/m^3\n",
          gamma->doppler_poly_dot[0],
          gamma->doppler_poly_dot[1],
          gamma->doppler_poly_dot[2],
          gamma->doppler_poly_dot[3]);
  fprintf(fp, "doppler_poly_ddot:\t%12.5lf %e %e %e    "
          "Hz/s^2     Hz/s^2/m     Hz/s^2/m^2     Hz/s^2/m^3\n",
          gamma->doppler_poly_ddot[0],
          gamma->doppler_poly_ddot[1],
          gamma->doppler_poly_ddot[2],
          gamma->doppler_poly_ddot[3]);
  fprintf(fp, "receiver_gain:\t%15.5lf   dB\n", gamma->receiver_gain);
  fprintf(fp, "calibration_gain:\t%15.5lf   dB\n", gamma->calibration_gain);
  fprintf(fp, "sar_to_earth_center:\t\t%15.4lf   m\n",
    gamma->sar_to_earth_center);
  fprintf(fp, "earth_radius_below_sensor:\t%15.4lf   m\n",
    gamma->earth_radius_below_sensor);
  fprintf(fp, "earth_semi_major_axis:\t%15.4lf   m\n",
    gamma->earth_semi_major_axis);
  fprintf(fp, "earth_semi_minor_axis:\t%15.4lf   m\n",
    gamma->earth_semi_minor_axis);
  fprintf(fp, "number_of_state_vectors:\t\t%7d\n", stVec->vector_count);
  fprintf(fp, "time_of_first_state_vector:\t%15.5lf   s\n",
    stVec->vecs[0].time);
  interval = stVec->vecs[1].time - stVec->vecs[0].time;
  fprintf(fp, "state_vector_interval:\t%15.5lf   s\n", interval);
  for (ii=0; ii<stVec->vector_count; ii++) {
    fprintf(fp, "state_vector_position_%d:%15.4lf %15.4lf %15.4lf   m   m   "
      "m\n", ii+1, stVec->vecs[ii].vec.pos.x, stVec->vecs[ii].vec.pos.y,
      stVec->vecs[ii].vec.pos.z);
    fprintf(fp, "state_vector_velocity_%d:%15.4lf %15.4lf %15.4lf   m/s m/s "
      "m/s\n", ii+1, stVec->vecs[ii].vec.vel.x,
      stVec->vecs[ii].vec.vel.y, stVec->vecs[ii].vec.vel.z);
  }
  FCLOSE(fp);

  return;
}
