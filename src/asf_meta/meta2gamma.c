#include "asf_meta.h"
#include "dateUtil.h"
#include "gamma.h"
#include "asf_nan.h"

double get_gamma_msp_azimuth_time_per_pixel(gamma_msp *g);
double get_gamma_msp_earth_radius_below_sensor(gamma_msp *gamma);

meta_parameters* gamma_isp2meta(gamma_isp *gamma)
{
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};

  // Initialize the meta structure
  meta = raw_init();

  // Fill general block
  strcpy(meta->general->basename, gamma->title);
  strcpy(meta->general->sensor, gamma->sensor);
  strcpy(meta->general->sensor_name, MAGIC_UNSET_STRING); // Sensor name not available in ISP metadata
  strcpy(meta->general->mode, MAGIC_UNSET_STRING); // Mode not available in ISP metadata
  strcpy(meta->general->processor, "GAMMA ISP");
  if (strncmp_case(gamma->image_format, "FCOMPLEX", 8) == 0)
    meta->general->data_type = COMPLEX_REAL32;
  else if (strncmp_case(gamma->image_format, "SCOMPLEX", 8) == 0)
    meta->general->data_type = COMPLEX_INTEGER16;
  else if (strncmp_case(gamma->image_format, "FLOAT", 8) == 0)
    meta->general->data_type = REAL32;
  else if (strncmp_case(gamma->image_format, "SHORT", 8) == 0)
    meta->general->data_type = INTEGER16;
  else if (strncmp_case(gamma->image_format, "BYTE", 8) == 0)
    meta->general->data_type = BYTE;
  if (strcmp(gamma->image_data_type, "UNKNOWN") == 0) {
    switch(meta->general->data_type) 
      {
      case COMPLEX_REAL32:
      case COMPLEX_INTEGER16:
	meta->general->image_data_type = COMPLEX_IMAGE;
	break;
      default:
	meta->general->image_data_type = IMAGE;
	break;
      }
  }
  else {
    if (strncmp_case(gamma->image_data_type, "RAW_IMAGE", 9) == 0)
      meta->general->image_data_type = RAW_IMAGE;
    if (strncmp_case(gamma->image_data_type, "COMPLEX_IMAGE", 13) == 0)
      meta->general->image_data_type = COMPLEX_IMAGE;
    if (strncmp_case(gamma->image_data_type, "AMPLITUDE_IMAGE", 15) == 0)
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    if (strncmp_case(gamma->image_data_type, "PHASE_IMAGE", 11) == 0)
      meta->general->image_data_type = PHASE_IMAGE;
    if (strncmp_case(gamma->image_data_type, "COHERENCE_IMAGE", 15) == 0)
      meta->general->image_data_type = COHERENCE_IMAGE;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_IMAGE", 18) == 0)
      meta->general->image_data_type = POLARIMETRIC_IMAGE;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_SEGMENTATION", 25) == 0)
      meta->general->image_data_type = POLARIMETRIC_SEGMENTATION;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_DECOMPOSITION", 26) == 0)
      meta->general->image_data_type = POLARIMETRIC_DECOMPOSITION;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_PARAMETER", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_PARAMETER;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_C2_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_C2_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_C3_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_C4_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_C4_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_T3_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_T3_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_T4_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_T4_MATRIX;
    if (strncmp_case(gamma->image_data_type, 
		     "POLARIMETRIC_STOKES_MATRIX", 26) == 0)
      meta->general->image_data_type = POLARIMETRIC_STOKES_MATRIX;
    if (strncmp_case(gamma->image_data_type, "LUT_IMAGE", 9) == 0)
      meta->general->image_data_type = LUT_IMAGE;
    if (strncmp_case(gamma->image_data_type, "ELEVATION", 9) == 0)
      meta->general->image_data_type = ELEVATION;
    if (strncmp_case(gamma->image_data_type, "DEM", 3) == 0)
      meta->general->image_data_type = DEM;
    if (strncmp_case(gamma->image_data_type, "IMAGE", 5) == 0)
      meta->general->image_data_type = IMAGE;
    if (strncmp_case(gamma->image_data_type, "MASK", 4) == 0)
      meta->general->image_data_type = MASK;
    if (strncmp_case(gamma->image_data_type, "IMAGE_LAYER_STACK", 17) == 0)
      meta->general->image_data_type = IMAGE_LAYER_STACK;
    if (strncmp_case(gamma->image_data_type, "INSAR_STACK", 11) == 0)
      meta->general->image_data_type = INSAR_STACK;
  }
  sprintf(meta->general->acquisition_date, "%2d-%s-%4d",
    gamma->acquisition.day, mon[gamma->acquisition.month],
    gamma->acquisition.year);
  meta->general->orbit = gamma->orbit;
  if (gamma->heading > 90.0 && gamma->heading < 270.0)
    meta->general->orbit_direction = 'D';
  else
    meta->general->orbit_direction = 'A';
  meta->general->frame = MAGIC_UNSET_INT;
  meta->general->band_count = 1;
  strcpy(meta->general->bands, MAGIC_UNSET_STRING);
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
  meta->general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  meta->general->missing_lines = 0;
  meta->general->no_data = MAGIC_UNSET_DOUBLE;

  // Fill SAR block
  meta->sar = meta_sar_init();
  if (strncmp(uc(gamma->image_geometry), "SLANT_RANGE", 11) == 0)
    meta->sar->image_type = 'S';
  else if (strncmp(uc(gamma->image_geometry), "GROUND_RANGE", 11) == 0)
    meta->sar->image_type = 'G';
  else
    meta->sar->image_type = MAGIC_UNSET_CHAR;
  if (gamma->azimuth_angle >= 0.0)
    meta->sar->look_direction = 'R';
  else
    meta->sar->look_direction = 'L';
  meta->sar->look_count = gamma->azimuth_looks / gamma->range_looks;
  if (gamma->azimuth_looks == gamma->range_looks)
    meta->sar->multilook = 0;
  else
    meta->sar->multilook = 1;
  meta->sar->deskewed = gamma->azimuth_deskew;
  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->range_time_per_pixel = fabs((2.0 * gamma->range_pixel_spacing) / 
					 gamma->range_looks /
					 SPD_LIGHT);
  meta->sar->azimuth_time_per_pixel = gamma->azimuth_line_time;
  meta->sar->slant_range_first_pixel = gamma->near_range_slc;
  meta->sar->slant_shift = 0.0;
  meta->sar->time_shift = 0.0;
  /* Under testing - does not seem to apply to the test data set.
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count * meta->sar->azimuth_time_per_pixel);
  else
    meta->sar->time_shift = MAGIC_UNSET_DOUBLE;
  */
  meta->sar->wavelength = SPD_LIGHT / gamma->radar_frequency;
  meta->sar->prf = gamma->prf;
  meta->sar->earth_radius = gamma->earth_radius_below_sensor;
  meta->sar->earth_radius_pp = meta->sar->earth_radius; // KLUDGE: This value is actually unknown in ISP metadata
  meta->sar->satellite_height = gamma->sar_to_earth_center;
  strcpy(meta->sar->satellite_binary_time, MAGIC_UNSET_STRING);
  strcpy(meta->sar->satellite_clock_time, MAGIC_UNSET_STRING);
  int i;
  if (gamma->doppler_polynomial[3] > 0.0001) {
    // FIXME: If this error ever fires, then we should insert a function that does a
    // quadratic fit to the cubic function.  Then we can derive close 'nuf quadratic
    // values from a set of points generated by the cubic and use those.
    asfPrintError("GAMMA doppler polynomial has a large cubic term\n"
        "(%lf versus limit of 0.0001) and is not well modeled by a\nquadratic.",
        gamma->doppler_polynomial[3]);
  }
  for (i=0; i<3; i++) {
    meta->sar->range_doppler_coefficients[i] = gamma->doppler_polynomial[i]; 
    meta->sar->azimuth_doppler_coefficients[i] = 0.0;
  }
  // Adjust for difference in units [Hz/m] -> [Hz/pixel]
  meta->sar->range_doppler_coefficients[1] /= gamma->range_pixel_spacing;
  meta->sar->range_doppler_coefficients[2] /= 
    gamma->range_pixel_spacing * gamma->range_pixel_spacing;

  meta->sar->azimuth_doppler_coefficients[0] = gamma->doppler_polynomial[0];
  meta->sar->azimuth_processing_bandwidth = gamma->azimuth_proc_bandwidth;
  meta->sar->chirp_rate = gamma->chirp_bandwidth;
  meta->sar->pulse_duration = MAGIC_UNSET_DOUBLE;
  meta->sar->range_sampling_rate = gamma->adc_sampling_rate;
  strcpy(meta->sar->polarization, MAGIC_UNSET_STRING);

  // Fill state vector structure
  meta->state_vectors = meta_state_vectors_init(3);
  meta->state_vectors = gamma->stVec;

  // Propagate the state vectors to start, center, end
  int vector_count = 3;
  double data_int = gamma->center_time - gamma->start_time;
  while (fabs(data_int) > 15.0) {
    data_int /= 2;
    vector_count = vector_count*2-1;
  }
  propagate_state(meta, vector_count, data_int);
  
  // Generate location block
  meta_get_corner_coords(meta);

  return meta;
}

/*
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
    strcpy(uc(gamma->image_format), "FCOMPLEX");
  if (meta->sar->image_type == 'S')
    strcpy(uc(gamma->image_geometry), "SLANT_RANGE");
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
*/

meta_parameters* gamma_msp2meta(gamma_msp *gamma)
{
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
    "Oct","Nov","Dec"};

  // Initialize the meta structure
  meta = raw_init();

  // Fill general block
  strcpy(meta->general->basename, gamma->title);
  strcpy(meta->general->sensor, MAGIC_UNSET_STRING); // Sensor not available in MSP metadata
  strcpy(meta->general->sensor_name, MAGIC_UNSET_STRING); // Sensor name not available in MSP metadata
  strcpy(meta->general->mode, MAGIC_UNSET_STRING); // Mode not available in MSP metadata
  strcpy(meta->general->processor, "GAMMA MSP");
  if (strncmp(uc(gamma->image_format), "FCOMPLEX", 8) == 0)
    meta->general->data_type = COMPLEX_REAL32;
  else if (strncmp(uc(gamma->image_format), "SCOMPLEX", 8) == 0)
    meta->general->data_type = COMPLEX_INTEGER16;
  else if (strncmp(uc(gamma->image_format), "FLOAT", 8) == 0)
    meta->general->data_type = REAL32;
  else if (strncmp(uc(gamma->image_format), "SHORT", 8) == 0)
    meta->general->data_type = INTEGER16;
  else if (strncmp(uc(gamma->image_format), "BYTE", 8) == 0)
    meta->general->data_type = BYTE;
  if (strcmp(gamma->image_data_type, "UNKNOWN") == 0) {
    switch(meta->general->data_type) 
      {
      case COMPLEX_REAL32:
      case COMPLEX_INTEGER16:
	meta->general->image_data_type = COMPLEX_IMAGE;
	break;
      default:
	meta->general->image_data_type = IMAGE;
	break;
      }
  }
  else {
    if (strncmp(uc(gamma->image_data_type), "RAW_IMAGE", 9) == 0)
      meta->general->image_data_type = RAW_IMAGE;
    if (strncmp(uc(gamma->image_data_type), "COMPLEX_IMAGE", 13) == 0)
      meta->general->image_data_type = COMPLEX_IMAGE;
    if (strncmp(uc(gamma->image_data_type), "AMPLITUDE_IMAGE", 15) == 0)
      meta->general->image_data_type = AMPLITUDE_IMAGE;
    if (strncmp(uc(gamma->image_data_type), "PHASE_IMAGE", 11) == 0)
      meta->general->image_data_type = PHASE_IMAGE;
    if (strncmp(uc(gamma->image_data_type), "COHERENCE_IMAGE", 15) == 0)
      meta->general->image_data_type = COHERENCE_IMAGE;
    if (strncmp(uc(gamma->image_data_type), "POLARIMETRIC_IMAGE", 18) == 0)
      meta->general->image_data_type = POLARIMETRIC_IMAGE;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_SEGMENTATION", 25) == 0)
      meta->general->image_data_type = POLARIMETRIC_SEGMENTATION;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_DECOMPOSITION", 26) == 0)
      meta->general->image_data_type = POLARIMETRIC_DECOMPOSITION;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_PARAMETER", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_PARAMETER;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_C2_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_C2_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_C3_MATRIX", 22) == 0)
      meta->general->image_data_type = POLARIMETRIC_C3_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_C4_MATRIX", 19) == 0)
      meta->general->image_data_type = POLARIMETRIC_C4_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_T3_MATRIX", 19) == 0)
      meta->general->image_data_type = POLARIMETRIC_T3_MATRIX;
    if (strncmp_case(gamma->image_data_type, "POLARIMETRIC_T4_MATRIX", 19) == 0)
      meta->general->image_data_type = POLARIMETRIC_T4_MATRIX;
    if (strncmp_case(gamma->image_data_type, 
		     "POLARIMETRIC_STOKES_MATRIX", 26) == 0)
      meta->general->image_data_type = POLARIMETRIC_STOKES_MATRIX;
    if (strncmp(uc(gamma->image_data_type), "LUT_IMAGE", 9) == 0)
      meta->general->image_data_type = LUT_IMAGE;
    if (strncmp(uc(gamma->image_data_type), "ELEVATION", 9) == 0)
      meta->general->image_data_type = ELEVATION;
    if (strncmp(uc(gamma->image_data_type), "DEM", 3) == 0)
      meta->general->image_data_type = DEM;
    if (strncmp(uc(gamma->image_data_type), "IMAGE", 5) == 0)
      meta->general->image_data_type = IMAGE;
    if (strncmp(uc(gamma->image_data_type), "MASK", 4) == 0)
      meta->general->image_data_type = MASK;
  }
  sprintf(meta->general->acquisition_date, "%2d-%s-%4d",
          gamma->acquisition.day, mon[gamma->acquisition.month],
          gamma->acquisition.year);
  meta->general->orbit = gamma->orbit;
  if (gamma->track_angle < -90.0)
    meta->general->orbit_direction = 'D';
  else
    meta->general->orbit_direction = 'A';
  meta->general->frame = MAGIC_UNSET_INT;
  meta->general->band_count = 1;
  strcpy(meta->general->bands, gamma->band);
  meta->general->line_count = gamma->azimuth_pixels;
  meta->general->sample_count = gamma->range_pixels;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = gamma->range_pixel_spacing;
  meta->general->y_pixel_size = gamma->azimuth_pixel_spacing;
  meta->general->center_latitude = gamma->scene_center_latitude;
  meta->general->center_longitude = gamma->scene_center_longitude;
  meta->general->re_major = gamma->earth_semi_major_axis;
  meta->general->re_minor = gamma->earth_semi_minor_axis;
  meta->general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  meta->general->missing_lines = 0;
  meta->general->no_data = MAGIC_UNSET_DOUBLE;
  // Fill SAR block
  meta->sar = meta_sar_init();
  meta->sar->image_type = MAGIC_UNSET_CHAR;
  meta->sar->look_direction = MAGIC_UNSET_CHAR;
  meta->sar->look_count = gamma->azimuth_looks;
  meta->sar->deskewed = gamma->azimuth_deskew;
  meta->sar->original_line_count = gamma->offset_to_first_echo_to_process + gamma->echoes_to_process;
  meta->sar->original_sample_count = gamma->range_offset + gamma->raw_range_samples;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->range_time_per_pixel = fabs((2.0 * gamma->range_pixel_spacing) / SPD_LIGHT);
  meta->sar->azimuth_time_per_pixel = get_gamma_msp_azimuth_time_per_pixel(gamma);
  meta->sar->slant_range_first_pixel = gamma->near_range_slc;
  meta->sar->slant_shift = 0.0;
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count * meta->sar->azimuth_time_per_pixel);
  else
    meta->sar->time_shift = MAGIC_UNSET_DOUBLE;
  meta->sar->wavelength = MAGIC_UNSET_DOUBLE;
  meta->sar->prf = gamma->prf;
  meta->sar->earth_radius = get_gamma_msp_earth_radius_below_sensor(gamma);
  meta->sar->earth_radius_pp = meta->sar->earth_radius; // KLUDGE: This value is actually unknown in MSP metadata
  meta->sar->satellite_height = sqrt(gamma->sensor_position_vector.x*gamma->sensor_position_vector.x +
      gamma->sensor_position_vector.y*gamma->sensor_position_vector.y +
      gamma->sensor_position_vector.z*gamma->sensor_position_vector.z);
  strcpy(meta->sar->satellite_binary_time, MAGIC_UNSET_STRING);
  strcpy(meta->sar->satellite_clock_time, MAGIC_UNSET_STRING);
  int i;
  if (gamma->doppler_polynomial[3] > 0.0001) {
    // FIXME: If this error ever fires, then we should insert a function that does a
    // quadratic fit to the cubic function.  Then we can derive close 'nuf quadratic
    // values from a set of points generated by the cubic and use those.
    asfPrintError("GAMMA doppler polynomial has a large cubic term\n"
           "(%lf versus limit of 0.0001) and is not well modeled by a\nquadratic.",
       gamma->doppler_polynomial[3]);
  }
  for (i=0; i<3; i++) {
    meta->sar->range_doppler_coefficients[i] = gamma->doppler_polynomial[i];
    meta->sar->azimuth_doppler_coefficients[i] = 0.0; // FIXME: We have gamma->radar_frequency and state vectors ...we should estimate the azimuth doppler stuff
  }
  meta->sar->azimuth_processing_bandwidth = MAGIC_UNSET_DOUBLE;
  meta->sar->chirp_rate = MAGIC_UNSET_DOUBLE;
  meta->sar->pulse_duration = MAGIC_UNSET_DOUBLE;
  meta->sar->range_sampling_rate = MAGIC_UNSET_DOUBLE;
  strcpy(meta->sar->polarization, MAGIC_UNSET_STRING);
  meta->sar->multilook = 0;

  // Fill state vector structure
  meta->state_vectors = meta_state_vectors_init(3);
  meta->state_vectors = gamma->stVec;

  // Fill location block
  meta->location = meta_location_init();
  if (meta->general->orbit_direction == 'D') {
    // See comments in gamma.h for map coordinate identification
    meta->location->lat_start_near_range = gamma->map_coordinate_2.lat;
    meta->location->lon_start_near_range = gamma->map_coordinate_2.lon;
    meta->location->lat_start_far_range = gamma->map_coordinate_1.lat;
    meta->location->lon_start_far_range = gamma->map_coordinate_1.lon;
    meta->location->lat_end_near_range = gamma->map_coordinate_4.lat;
    meta->location->lon_end_near_range = gamma->map_coordinate_4.lon;
    meta->location->lat_end_far_range = gamma->map_coordinate_3.lat;
    meta->location->lon_end_far_range = gamma->map_coordinate_3.lon;
  }
  else {
    meta->location->lat_start_near_range = gamma->map_coordinate_3.lat;
    meta->location->lon_start_near_range = gamma->map_coordinate_3.lon;
    meta->location->lat_start_far_range = gamma->map_coordinate_4.lat;
    meta->location->lon_start_far_range = gamma->map_coordinate_4.lon;
    meta->location->lat_end_near_range = gamma->map_coordinate_1.lat;
    meta->location->lon_end_near_range = gamma->map_coordinate_1.lon;
    meta->location->lat_end_far_range = gamma->map_coordinate_2.lat;
    meta->location->lon_end_far_range = gamma->map_coordinate_2.lon;
  }

  return meta;
}

double get_gamma_msp_azimuth_time_per_pixel(gamma_msp *g)
{
  double  re = g->earth_semi_major_axis;
  double  rp = g->earth_semi_minor_axis;
  double lat = g->sensor_latitude * D2R;        // Platform geodetic latitude at scene center
  double  ht;                                   // Satellite height from earth center
  double  er;                                   // Earth radius at scene center
  POS pos    = g->sensor_position_vector;       // Platform position (x,y,z) at scene center
  double  atpp, orbit_vel, swath_vel, grav = 9.81;

  er = (re*rp) /
      sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  ht = sqrt(pos.x*pos.x + pos.y*pos.y + pos.z*pos.z);
  orbit_vel = sqrt(grav*er*er/ht);
  swath_vel = orbit_vel*er/ht;
  atpp = g->azimuth_pixel_spacing/swath_vel;

  return atpp;
}

double get_gamma_msp_earth_radius_below_sensor(gamma_msp *g)
{
  double  re = g->earth_semi_major_axis;
  double  rp = g->earth_semi_minor_axis;
  double lat = g->sensor_latitude * D2R;        // Platform geodetic latitude at scene center
  double  er;                                   // Earth radius at scene center

  er = (re*rp) /
      sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));

  return er;
}




















