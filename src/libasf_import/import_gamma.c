#include "gamma.h"
#include "asf_meta.h"
#include "dateUtil.h"

gamma_isp *gamma_isp_init();

void import_gamma_isp(const char *inBaseName, const char *outBaseName)
{
  gamma_isp *gamma;
  meta_state_vectors *stVec=NULL;
  meta_parameters *meta=NULL;
  FILE *fp;
  ymd_date ymd;
  julian_date jd;
  char inFile[255], outFile[255], line[1024], *p, key[512], *value, *str;
  int ii, vectors;
  double interval;

  // Initialize the gamma ISP struct
  gamma = gamma_isp_init();

  // Read GAMMA ISP parameter file
  sprintf(inFile, "%s.slc.par", inBaseName);

  fp = FOPEN(inFile, "r");
  while (fgets(line, 1024, fp)) {
    p = strchr(line, ':');
    if (p) {
      sscanf(line, "%s:", key);
      value = p+1;
      if (strncmp(key, "title:", 6) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->title, "%s, %s", inBaseName, str);
        char *s;
        s = &str[strlen(str)-1];
        while (isdigit((int)*s) || *s == '.') s--;
        s++;
        if (isdigit((int)*s)) g->orbit = atoi(s);
      }
      else if (strncmp(key, "sensor:", 7) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->sensor, "%s", str);
      }
      else if (strncmp(key, "date:", 5) == 0)
        sscanf(value, "%d %d %d %d %d %f",
               &gamma->acquisition.year,   &gamma->acquisition.month,
               &gamma->acquisition.day,    &gamma->acquisition.hour,
               &gamma->acquisition.minute, &gamma->acquisition.seconds);
      else if (strncmp(key, "start_time:", 11) == 0)
        gamma->start_time = atof(value);
      else if (strncmp(key, "center_time:", 12) == 0)
        gamma->center_time = atof(value);
      else if (strncmp(key, "end_time:", 9) == 0)
        gamma->end_time = atof(value);
      else if (strncmp(key, "azimuth_line_time:", 18) == 0)
        gamma->azimuth_line_time = atof(value);
      else if (strncmp(key, "line_header_size:", 17) == 0)
        gamma->line_header_size = atoi(value);
      else if (strncmp(key, "range_samples:", 14) == 0)
        gamma->range_samples = atoi(value);
      else if (strncmp(key, "azimuth_lines:", 14) == 0)
        gamma->azimuth_lines = atoi(value);
      else if (strncmp(key, "range_looks:", 12) == 0)
        gamma->range_looks = atoi(value);
      else if (strncmp(key, "azimuth_looks:", 14) == 0)
        gamma->azimuth_looks = atoi(value);
      else if (strncmp(key, "image_format:", 13) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->image_format, "%s", str);
      }
      else if (strncmp(key, "image_geometry:", 15) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->image_geometry, "%s", str);
      }
      else if (strncmp(key, "range_scale_factor:", 19) == 0)
        gamma->range_scale_factor = atof(value);
      else if (strncmp(key, "azimuth_scale_factor:", 21) ==0)
        gamma->azimuth_scale_factor = atof(value);
      else if (strncmp(key, "center_latitude:", 16) == 0)
        gamma->center_latitude = atof(value);
      else if (strncmp(key, "center_longitude:", 17) == 0)
        gamma->center_longitude = atof(value);
      else if (strncmp(key, "heading:", 8) == 0)
        gamma->heading = atof(value);
      else if (strncmp(key, "range_pixel_spacing:", 20) == 0)
        gamma->range_pixel_spacing = atof(value);
      else if (strncmp(key, "azimuth_pixel_spacing:", 22) == 0)
        gamma->azimuth_pixel_spacing = atof(value);
      else if (strncmp(key, "near_range_slc:", 15) == 0)
        gamma->near_range_slc = atof(value);
      else if (strncmp(key, "center_range_slc:", 17) == 0)
        gamma->center_range_slc = atof(value);
      else if (strncmp(key, "far_range_slc:", 14) == 0)
        gamma->far_range_slc = atof(value);
      else if (strncmp(key, "first_slant_range_polynomial:", 29) == 0)
        sscanf(value, "%lf %lf %lf %lf %lf %lf",
               &gamma->first_slant_range_polynomial[0],
               &gamma->first_slant_range_polynomial[1],
               &gamma->first_slant_range_polynomial[2],
               &gamma->first_slant_range_polynomial[3],
               &gamma->first_slant_range_polynomial[4],
               &gamma->first_slant_range_polynomial[5]);
      else if (strncmp(key, "center_slant_range_polynomial:", 30) == 0)
        sscanf(value, "%lf %lf %lf %lf %lf %lf",
               &gamma->center_slant_range_polynomial[0],
               &gamma->center_slant_range_polynomial[1],
               &gamma->center_slant_range_polynomial[2],
               &gamma->center_slant_range_polynomial[3],
               &gamma->center_slant_range_polynomial[4],
               &gamma->center_slant_range_polynomial[5]);
      else if (strncmp(key, "last_slant_range_polynomial:", 28) == 0)
        sscanf(value, "%lf %lf %lf %lf %lf %lf",
               &gamma->last_slant_range_polynomial[0],
               &gamma->last_slant_range_polynomial[1],
               &gamma->last_slant_range_polynomial[2],
               &gamma->last_slant_range_polynomial[3],
               &gamma->last_slant_range_polynomial[4],
               &gamma->last_slant_range_polynomial[5]);
      else if (strncmp(key, "incidence_angle:", 16) == 0)
        gamma->incidence_angle = atof(value);
      else if (strncmp(key, "azimuth_deskew:", 15) == 0) {
        str = trim_spaces(value);
        if (strcmp(str, "ON") == 0)
          gamma->azimuth_deskew = 1;
        else
          gamma->azimuth_deskew = 0;
      }
      else if (strncmp(key, "azimuth_angle:", 14) == 0)
        gamma->azimuth_angle = atof(value);
      else if (strncmp(key, "radar_frequency:", 16) == 0)
        gamma->radar_frequency = atof(value);
      else if (strncmp(key, "adc_sampling_rate:", 18) == 0)
        gamma->adc_sampling_rate = atof(value);
      else if (strncmp(key, "chirp_bandwidth:", 16) == 0)
        gamma->chirp_bandwidth = atof(value);
      else if (strncmp(key, "prf:", 4) == 0)
        gamma->prf = atof(value);
      else if (strncmp(key, "azimuth_proc_bandwidth:", 23) == 0)
        gamma->azimuth_proc_bandwidth = atof(value);
      else if (strncmp(key, "doppler_polynomial:", 19) == 0)
        sscanf(value, "%lf %lf %lf %lf",
               &gamma->doppler_polynomial[0],
               &gamma->doppler_polynomial[1],
               &gamma->doppler_polynomial[2],
               &gamma->doppler_polynomial[3]);
      else if (strncmp(key, "doppler_poly_dot:", 17) == 0)
        sscanf(value, "%lf %lf %lf %lf",
               &gamma->doppler_poly_dot[0],
               &gamma->doppler_poly_dot[1],
               &gamma->doppler_poly_dot[2],
               &gamma->doppler_poly_dot[3]);
      else if (strncmp(key, "doppler_poly_ddot:", 18) == 0)
        sscanf(value, "%lf %lf %lf %lf",
               &gamma->doppler_poly_ddot[0],
               &gamma->doppler_poly_ddot[1],
               &gamma->doppler_poly_ddot[2],
               &gamma->doppler_poly_ddot[3]);
      else if (strncmp(key, "receiver_gain:", 14) == 0)
        gamma->receiver_gain = atof(value);
      else if (strncmp(key, "calibration_gain:", 17) == 0)
        gamma->calibration_gain = atof(value);
      else if (strncmp(key, "sar_to_earth_center:", 20) == 0)
        gamma->sar_to_earth_center = atof(value);
      else if (strncmp(key, "earth_radius_below_sensor:", 26) == 0)
        gamma->earth_radius_below_sensor = atof(value);
      else if (strncmp(key, "earth_semi_major_axis:", 22) == 0)
        gamma->earth_semi_major_axis = atof(value);
      else if (strncmp(key, "earth_semi_minor_axis:", 22) == 0)
        gamma->earth_semi_minor_axis = atof(value);
      else if (strncmp(key, "number_of_state_vectors:", 24) == 0) {
        vectors = atoi(value);
        stVec = meta_state_vectors_init(vectors);
        fscanf(fp, "time_of_first_state_vector: %lf   s\n", &stVec->second);
        fscanf(fp, "state_vector_interval: %lf   s\n", &interval);
        stVec->year = gamma->acquisition[0];
        ymd.year = gamma->acquisition[0];
        ymd.month = gamma->acquisition[1];
        ymd.day = gamma->acquisition[2];
        date_ymd2jd(&ymd, &jd);
        stVec->julDay = jd.jd;
        for (ii=0; ii<stVec->vector_count; ii++) {
          stVec->vecs[ii].time = stVec->second + ii*interval;
          fgets(line, 1024, fp);
          sscanf(line, "%s:%s", key, value);
          sscanf(value, "%lf %lf %lf",
                 &stVec->vecs[ii].vec.pos.x,
                 &stVec->vecs[ii].vec.pos.y,
                 &stVec->vecs[ii].vec.pos.z);
          fgets(line, 1024, fp);
          sscanf(line, "%s:%s", key, value);
          sscanf(value, "%lf %lf %lf",
                 &stVec->vecs[ii].vec.vel.x,
                 &stVec->vecs[ii].vec.vel.y,
                 &stVec->vecs[ii].vec.vel.z);
        }
      }
    }
  }
  FCLOSE(fp);

  // Generate metadata
  sprintf(outFile, "%s.meta", outBaseName);
  meta = gamma_isp2meta(gamma, stVec);
  meta_write(meta, outFile);

  // Copy generic binary file
  sprintf(inFile, "%s.slc", inBaseName);
  sprintf(outFile, "%s.img", outBaseName);
  fileCopy(inFile, outFile);

  // Cleanup
  if(gamma)FREE(gamma);
}

gamma_isp *gamma_isp_init();
{
  int i;
  gamma_isp *g = (gamma_isp *) MALLOC(sizeof(gamma_isp));

  strcpy(g->title, MAGIC_UNSET_STRING);
  g->orbit = MAGIC_UNSET_INT;
  strcpy(g->sensor, MAGIC_UNSET_STRING);
  g->acquisition.year    = MAGIC_UNSET_INT;
  g->acquisition.month   = MAGIC_UNSET_INT;
  g->acquisition.day     = MAGIC_UNSET_INT;
  g->acquisition.hour    = MAGIC_UNSET_INT;
  g->acquisition.minute  = MAGIC_UNSET_INT;
  g->acquisition.seconds = MAGIC_UNSET_DOUBLE;
  g->start_time = MAGIC_UNSET_DOUBLE;
  g->center_time = MAGIC_UNSET_DOUBLE;
  g->end_time = MAGIC_UNSET_DOUBLE;
  g->azimuth_line_time = MAGIC_UNSET_DOUBLE;
  g->line_header_size = MAGIC_UNSET_INT;
  g->range_samples = MAGIC_UNSET_INT;
  g->azimuth_lines = MAGIC_UNSET_INT;
  g->range_looks = MAGIC_UNSET_INT;
  g->azimuth_looks = MAGIC_UNSET_INT;
  strcpy(g->image_format, MAGIC_UNSET_STRING);
  strcpy(g->image_geometry, MAGIC_UNSET_STRING);
  g->range_scale_factor = MAGIC_UNSET_DOUBLE;
  g->azimuth_scale_factor = MAGIC_UNSET_DOUBLE;
  g->center_latitude = MAGIC_UNSET_DOUBLE;
  g->center_longitude = MAGIC_UNSET_DOUBLE;
  g->heading = MAGIC_UNSET_DOUBLE;
  g->range_pixel_spacing = MAGIC_UNSET_DOUBLE;
  g->azimuth_pixel_spacing = MAGIC_UNSET_DOUBLE;
  g->near_range_slc = MAGIC_UNSET_DOUBLE;
  g->center_range_slc = MAGIC_UNSET_DOUBLE;
  g->far_range_slc = MAGIC_UNSET_DOUBLE;
  for (i=0; i<6; i++) g->first_slant_range_polynomial[i] = MAGIC_UNSET_DOUBLE;
  for (i=0; i<6; i++) g->center_slant_range_polynomial[i] = MAGIC_UNSET_DOUBLE;
  for (i=0; i<6; i++) g->last_slant_range_polynomial[i] = MAGIC_UNSET_DOUBLE;
  g->incidence_angle = MAGIC_UNSET_DOUBLE;
  g->azimuth_deskew = MAGIC_UNSET_INT;
  g->azimuth_angle = MAGIC_UNSET_DOUBLE;
  g->radar_frequency = MAGIC_UNSET_DOUBLE;
  g->adc_sampling_rate = MAGIC_UNSET_DOUBLE;
  g->chirp_bandwidth = MAGIC_UNSET_DOUBLE;
  g->prf = MAGIC_UNSET_DOUBLE;
  g->azimuth_proc_bandwidth = MAGIC_UNSET_DOUBLE;
  for (i=0; i<4; i++) g->doppler_polynomial[i] = MAGIC_UNSET_DOUBLE;
  for (i=0; i<4; i++) g->doppler_poly_dot[i] = MAGIC_UNSET_DOUBLE;
  for (i=0; i<4; i++) g->doppler_poly_ddot[i] = MAGIC_UNSET_DOUBLE;
  g->receiver_gain = MAGIC_UNSET_DOUBLE;
  g->calibration_gain = MAGIC_UNSET_DOUBLE;
  g->sar_to_earth_center = MAGIC_UNSET_DOUBLE;
  g->earth_radius_below_sensor = MAGIC_UNSET_DOUBLE;
  g->earth_semi_major_axis = MAGIC_UNSET_DOUBLE;
  g->earth_semi_minor_axis = MAGIC_UNSET_DOUBLE;

  return g;
}

gamma_msp *gamma_msp_init();
{
  int i;
  gamma_msp *g = (gamma_msp *) MALLOC(sizeof(gamma_msp));

  strcpy(g->title, MAGIC_UNSET_STRING);
  g->acquisition.year    = MAGIC_UNSET_INT;
  g->acquisition.month   = MAGIC_UNSET_INT;
  g->acquisition.day     = MAGIC_UNSET_INT;
  g->acquisition.hour    = MAGIC_UNSET_INT;
  g->acquisition.minute  = MAGIC_UNSET_INT;
  g->acquisition.seconds = MAGIC_UNSET_DOUBLE;
  g->raw_data_start_time.hour = MAGIC_UNSET_INT;
  g->raw_data_start_time.minute = MAGIC_UNSET_INT;
  g->raw_data_start_time.seconds = MAGIC_UNSET_DOUBLE;
  strcpy(g->band, MAGIC_UNSET_STRING);
  g->earth_semi_major_axis = MAGIC_UNSET_DOUBLE;
  g->earth_semi_minor_axis = MAGIC_UNSET_DOUBLE;
  g->scene_center_latitude = MAGIC_UNSET_DOUBLE;
  g->scene_center_longitude = MAGIC_UNSET_DOUBLE;
  g->track_angle = MAGIC_UNSET_DOUBLE;
  g->platform_altitude = MAGIC_UNSET_DOUBLE;
  g->terrain_height = MAGIC_UNSET_DOUBLE;
  g->sensor_position_vector.x = MAGIC_UNSET_DOUBLE;
  g->sensor_position_vector.y = MAGIC_UNSET_DOUBLE;
  g->sensor_position_vector.z = MAGIC_UNSET_DOUBLE;
  g->sensor_velocity_vector.vx = MAGIC_UNSET_DOUBLE;
  g->sensor_velocity_vector.vy = MAGIC_UNSET_DOUBLE;
  g->sensor_velocity_vector.vz = MAGIC_UNSET_DOUBLE;
  g->sensor_acceleration_vector.ax = MAGIC_UNSET_DOUBLE;
  g->sensor_acceleration_vector.ay = MAGIC_UNSET_DOUBLE;
  g->sensor_acceleration_vector.az = MAGIC_UNSET_DOUBLE;
  g->prf = MAGIC_UNSET_DOUBLE;
  g->I_bias = MAGIC_UNSET_DOUBLE;
  g->Q_bias = MAGIC_UNSET_DOUBLE;
  g->I_sigma = MAGIC_UNSET_DOUBLE;
  g->Q_sigma = MAGIC_UNSET_DOUBLE;
  g->IQ_corr = MAGIC_UNSET_DOUBLE;
  g->SNR_range_spectrum = MAGIC_UNSET_DOUBLE;
  g->DAR_doppler = MAGIC_UNSET_DOUBLE;
  g->DAR_snr = MAGIC_UNSET_DOUBLE;
  for (i=0; i<4; i++) g->doppler_polynomial[i] = MAGIC_UNSET_DOUBLE;
  for (i=0; i<4; i++) g->doppler_poly_dot[i] = MAGIC_UNSET_DOUBLE;
  for (i=0; i<4; i++) g->doppler_poly_ddot[i] = MAGIC_UNSET_DOUBLE;
  g->sec_range_mig = MAGIC_UNSET_INT;
  g->az_deskew = MAGIC_UNSET_INT;
  g->autofocus_snr = MAGIC_UNSET_DOUBLE;
  g->echo_time_delay = MAGIC_UNSET_DOUBLE;
  g->receiver_gain = MAGIC_UNSET_DOUBLE;
  g->calibration_gain = MAGIC_UNSET_DOUBLE;
  g->near_range_raw = MAGIC_UNSET_DOUBLE;
  g->center_range_raw = MAGIC_UNSET_DOUBLE;
  g->far_range_raw = MAGIC_UNSET_DOUBLE;
  g->near_range_slc = MAGIC_UNSET_DOUBLE;
  g->center_range_slc = MAGIC_UNSET_DOUBLE;
  g->far_range_slc = MAGIC_UNSET_DOUBLE;
  g->range_pixel_spacing = MAGIC_UNSET_DOUBLE;
  g->range_resolution = MAGIC_UNSET_DOUBLE;
  g->azimuth_bandwidth_fraction = MAGIC_UNSET_DOUBLE;
  g->prefilter_azimuth_offset = MAGIC_UNSET_INT;
  g->total_raw_echoes = MAGIC_UNSET_INT;
  g->range_offset = MAGIC_UNSET_INT;
  g->raw_range_samples = MAGIC_UNSET_INT;
  g->near_range_extension = MAGIC_UNSET_INT;
  g->far_range_extension = MAGIC_UNSET_INT;
  g->range_looks = MAGIC_UNSET_INT;
  g->azimuth_looks = MAGIC_UNSET_INT;
  g-> = MAGIC_UNSET_DOUBLE;




  g-> = MAGIC_UNSET_DOUBLE;
  g-> = MAGIC_UNSET_INT;

  return g;
}

