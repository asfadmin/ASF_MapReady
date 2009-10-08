#include <ctype.h>
#include "gamma.h"
#include "asf_import.h"
#include "asf_raster.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "asf_nan.h"
#include "asf_sar.h"

gamma_isp *gamma_isp_init();
gamma_msp *gamma_msp_init();

meta_parameters *meta_read_gamma_isp(const char *inName,
				     const char *data_type,
				     const char *image_data_type)
{
  gamma_isp *gamma;
  meta_parameters *meta=NULL;
  stateVector stVec;
  FILE *fp;
  ymd_date ymd;
  julian_date jd;
  char line[1024], *p, key[512], *value, *str;
  int ii;
  double interval;

  // Initialize the gamma ISP struct
  gamma = gamma_isp_init();

  // Read GAMMA ISP parameter file
  fp = FOPEN(inName, "r");
  while (fgets(line, 1024, fp)) {
    p = strchr(line, ':');
    if (p) {
      sscanf(line, "%s:", key);
      value = p+1;
      if (strncmp(key, "title:", 6) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->title, "%s", str);
      }
      else if (strncmp(key, "sensor:", 7) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->sensor, "%s", str);
      }
      else if (strncmp(key, "date:", 5) == 0)
        sscanf(value, "%d %d %d",
               &gamma->acquisition.year, &gamma->acquisition.month,
               &gamma->acquisition.day);
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
      // overwrite the image format that is given in the file
      sprintf(gamma->image_format, "%s", data_type);
      if (strncmp(key, "image_geometry:", 15) == 0) {
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
        gamma->number_of_state_vectors = atoi(value);
        gamma->stVec = meta_state_vectors_init(gamma->number_of_state_vectors);
        fscanf(fp, "time_of_first_state_vector: %lf   s\n", 
	       &gamma->stVec->second);
	gamma->stVec->vector_count = gamma->number_of_state_vectors;
        fscanf(fp, "state_vector_interval: %lf   s\n", &interval);
        gamma->stVec->year = gamma->acquisition.year;
        ymd.year = gamma->acquisition.year;
        ymd.month = gamma->acquisition.month;
        ymd.day = gamma->acquisition.day;
        date_ymd2jd(&ymd, &jd);
        gamma->stVec->julDay = jd.jd;
        for (ii=0; ii<gamma->stVec->vector_count; ii++) {
          gamma->stVec->vecs[ii].time = ii*interval;
          fgets(line, 1024, fp);
          sscanf(line, "%s:%s", key, value);
          sscanf(value, "%lf %lf %lf",
                 &gamma->stVec->vecs[ii].vec.pos.x,
                 &gamma->stVec->vecs[ii].vec.pos.y,
                 &gamma->stVec->vecs[ii].vec.pos.z);
          fgets(line, 1024, fp);
          sscanf(line, "%s:%s", key, value);
          sscanf(value, "%lf %lf %lf",
                 &gamma->stVec->vecs[ii].vec.vel.x,
                 &gamma->stVec->vecs[ii].vec.vel.y,
                 &gamma->stVec->vecs[ii].vec.vel.z);
        }

	// Apply offset between image starting time and state vectors
	double offset = gamma->start_time - gamma->stVec->second;
	for (ii=0; ii<gamma->stVec->vector_count; ii++) {
	  stVec = propagate(gamma->stVec->vecs[ii].vec,
			    gamma->stVec->vecs[ii].time,
			    gamma->stVec->vecs[ii].time + offset);
	  gamma->stVec->vecs[ii].vec = stVec;
	}
	gamma->stVec->second = gamma->start_time;
      }
    }
  }
  FCLOSE(fp);

  if (image_data_type)
    sprintf(gamma->image_data_type, "%s", image_data_type);
  else
    sprintf(gamma->image_data_type, "UNKNOWN");

  // Convert gamma_isp to metadata structure
  meta = gamma_isp2meta(gamma);

  // No clean up
  // Can't free gamma as the metadata structure uses the gamma state vector
  // structure allocated over here.

  return meta;
}

void import_gamma_isp(const char *inDataName, const char *inMetaName,
		      const char *data_type, const char *image_data_type, 
		      int complex_flag, int multilook_flag, 
		      const char *outBaseName)
{
  meta_parameters *meta=NULL;
  char tmpFile[255], *outFile;

  // Read metadata from gamma file
  meta = meta_read_gamma_isp(inMetaName, data_type, image_data_type);
  if (complex_flag || meta->general->data_type <= REAL64) {
    sprintf(outFile, "%s.meta", outBaseName);
    meta_write(meta, outFile);
  }
  else {
    sprintf(tmpFile, "%s_cpx.meta", outBaseName);
    meta_write(meta, tmpFile);
  }

  // Generate output file
  outFile = (char *) MALLOC(sizeof(char)*512);
  sprintf(outFile, "%s.img", outBaseName);
  if (meta->general->data_type >= COMPLEX_BYTE) {
    if (complex_flag && !multilook_flag)
      fileCopy(inDataName, outFile);
    else 
      c2p_ext(inDataName, tmpFile, outFile, multilook_flag, TRUE);
  }
  else
    fileCopy(inDataName, outFile);

  FREE(outFile);
}


meta_parameters *meta_read_gamma_msp(const char *inName,
				     const char *data_type,
				     const char *image_data_type)
{
  gamma_msp *gamma;
  meta_parameters *meta=NULL;
  FILE *fp;
  ymd_date ymd;
  julian_date jd;
  char line[1024], *p, key[512], *value, *str;
  int ii;
  double interval;

  // Initialize the gamma ISP struct
  gamma = gamma_msp_init();

  // Read GAMMA MSP parameter file
  fp = FOPEN(inName, "r");
  while (fgets(line, 1024, fp)) {
    p = strchr(line, ':');
    if (p) {
      sscanf(line, "%s:", key);
      value = p+1;
      if (strncmp(key, "title:", 6) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->title, "%s", str);
      }
      else if (strncmp(key, "date:", 5) == 0)
        sscanf(value, "%d %d %d",
               &gamma->acquisition.year, &gamma->acquisition.month,
               &gamma->acquisition.day);
      else if (strncmp(key, "raw_data_start_time:", 20) == 0)
        sscanf(value, "%d %d %lf",
               &gamma->raw_data_start_time.hour,
               &gamma->raw_data_start_time.minute,
               &gamma->raw_data_start_time.seconds);
      else if (strncmp(key, "channel", 7) == 0) {
        str = trim_spaces(value);
        sprintf(gamma->band, "%s", str);
      }
      else if (strncmp(key, "earth_semi_major_axis:", 22) == 0)
        gamma->earth_semi_major_axis = atof(value);
      else if (strncmp(key, "earth_semi_minor_axis:", 22) == 0)
        gamma->earth_semi_minor_axis = atof(value);
      else if (strncmp(key, "scene_center_latitude:", 22) == 0)
        gamma->scene_center_latitude = atof(value);
      else if (strncmp(key, "scene_center_longitude:", 23) == 0)
        gamma->scene_center_longitude = atof(value);
      else if (strncmp(key, "track_angle:", 12) == 0)
        gamma->track_angle = atof(value);
      else if (strncmp(key, "platform_altitude:", 18) == 0)
        gamma->platform_altitude = atof(value);
      else if (strncmp(key, "terrain_height:", 15) == 0)
        gamma->terrain_height = atof(value);
      else if (strncmp(key, "sensor_position_vector:", 23) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->sensor_position_vector.x,
               &gamma->sensor_position_vector.y,
               &gamma->sensor_position_vector.z);
      else if (strncmp(key, "sensor_velocity_vector:", 23) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->sensor_velocity_vector.vx,
               &gamma->sensor_velocity_vector.vy,
               &gamma->sensor_velocity_vector.vz);
      else if (strncmp(key, "sensor_acceleration_vector:", 27) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->sensor_acceleration_vector.ax,
               &gamma->sensor_acceleration_vector.ay,
               &gamma->sensor_acceleration_vector.az);
      else if (strncmp(key, "pulse_repetition_frequency:", 27) == 0)
        gamma->prf = atof(value);
      else if (strncmp(key, "I_bias:", 7) == 0)
        gamma->I_bias = atof(value);
      else if (strncmp(key, "Q_bias:", 7) == 0)
        gamma->Q_bias = atof(value);
      else if (strncmp(key, "I_sigma:", 8) == 0)
        gamma->I_sigma = atof(value);
      else if (strncmp(key, "Q_sigma:", 8) == 0)
        gamma->Q_sigma = atof(value);
      else if (strncmp(key, "IQ_corr:", 8) == 0)
        gamma->IQ_corr = atof(value);
      else if (strncmp(key, "SNR_range_spectrum:", 19) == 0)
        gamma->SNR_range_spectrum = atof(value);
      else if (strncmp(key, "DAR_doppler:", 12) == 0)
        gamma->DAR_doppler = atof(value);
      else if (strncmp(key, "DAR_snr:", 8) == 0)
        gamma->DAR_snr = atof(value);
      else if (strncmp(key, "doppler_polynomial:", 19) == 0)
        sscanf(value, "%lf %lf %lf %lf",
               &gamma->doppler_polynomial[0], &gamma->doppler_polynomial[1],
               &gamma->doppler_polynomial[2], &gamma->doppler_polynomial[3]); // Might need to be exp. format
      else if (strncmp(key, "doppler_poly_dot:", 19) == 0)
        sscanf(value, "%lf %lf %lf %lf",
               &gamma->doppler_poly_dot[0], &gamma->doppler_poly_dot[1],
               &gamma->doppler_poly_dot[2], &gamma->doppler_poly_dot[3]); // Might need to be exp. format
      else if (strncmp(key, "doppler_poly_ddot:", 19) == 0)
        sscanf(value, "%lf %lf %lf %lf",
               &gamma->doppler_poly_ddot[0], &gamma->doppler_poly_ddot[1],
               &gamma->doppler_poly_ddot[2], &gamma->doppler_poly_ddot[3]); // Might need to be exp. format
      else if (strncmp(key, "sec_range_migration:", 20) == 0)
        gamma->sec_range_migration = strncmp(uc(value), "ON", 2) == 0 ? 1 : 0;
      else if (strncmp(key, "azimuth_deskew:", 15) == 0)
        gamma->azimuth_deskew = strncmp(uc(value), "ON", 2) == 0 ? 1 : 0;
      else if (strncmp(key, "autofocus_snr:", 14) == 0)
        gamma->autofocus_snr = atof(value);
      else if (strncmp(key, "echo_time_delay:", 16) == 0)
        gamma->echo_time_delay = atof(value);
      else if (strncmp(key, "receiver_gain:", 14) == 0)
        gamma->receiver_gain = atof(value);
      else if (strncmp(key, "calibration_gain:", 17) == 0)
        gamma->calibration_gain = atof(value);
      else if (strncmp(key, "near_range_raw:", 15) == 0)
        gamma->near_range_raw = atof(value);
      else if (strncmp(key, "center_range_raw:", 15) == 0)
        gamma->center_range_raw = atof(value);
      else if (strncmp(key, "far_range_raw:", 15) == 0)
        gamma->far_range_raw = atof(value);
      else if (strncmp(key, "range_pixel_spacing:", 20) == 0)
        gamma->range_pixel_spacing = atof(value);
      else if (strncmp(key, "range_resolution:", 17) == 0)
        gamma->range_resolution = atof(value);
      else if (strncmp(key, "azimuth_bandwidth_fraction:", 27) == 0)
        gamma->azimuth_bandwidth_fraction = atof(value);
      else if (strncmp(key, "prefilter_azimuth_offset:", 25) == 0)
        gamma->prefilter_azimuth_offset = atoi(value);
      else if (strncmp(key, "total_raw_echoes:", 17) == 0)
        gamma->total_raw_echoes = atoi(value);
      else if (strncmp(key, "offset_to_first_echo_to_process:", 32) == 0)
        gamma->offset_to_first_echo_to_process = atoi(value);
      else if (strncmp(key, "echoes_to_process:", 18) == 0)
        gamma->echoes_to_process = atoi(value);
      else if (strncmp(key, "range_offset:", 13) == 0)
        gamma->range_offset = atoi(value);
      else if (strncmp(key, "raw_range_samples:", 18) == 0)
        gamma->raw_range_samples = atoi(value);
      else if (strncmp(key, "near_range_extension:", 21) == 0)
        gamma->near_range_extension = atoi(value);
      else if (strncmp(key, "far_range_extension:", 20) == 0)
        gamma->far_range_extension = atoi(value);
      else if (strncmp(key, "range_looks:", 12) == 0)
        gamma->range_looks = atoi(value);
      else if (strncmp(key, "azimuth_looks:", 14) == 0)
        gamma->azimuth_looks = atoi(value);
      else if (strncmp(key, "azimuth_offset:", 15) == 0)
        gamma->azimuth_offset = atof(value);
      else if (strncmp(key, "azimuth_pixel_spacing:", 22) == 0)
        gamma->azimuth_pixel_spacing = atof(value);
      else if (strncmp(key, "azimuth_resolution:", 19) == 0)
        gamma->azimuth_resolution = atof(value);
      else if (strncmp(key, "range_pixels:", 13) == 0)
        gamma->range_pixels = atoi(value);
      else if (strncmp(key, "azimuth_pixels:", 15) == 0)
        gamma->azimuth_pixels = atof(value);
      // overwrite the image format from the file
      sprintf(gamma->image_format, "%s", data_type);
      if (strncmp(key, "sensor_latitude:", 16) == 0)
        gamma->sensor_latitude = atof(value);
      else if (strncmp(key, "sensor_longitude:", 17) == 0)
        gamma->sensor_longitude = atof(value);
      else if (strncmp(key, "sensor_track_angle:", 19) == 0)
        gamma->sensor_track_angle = atof(value);
      else if (strncmp(key, "map_coordinate_1:", 17) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->map_coordinate_1.lat,
               &gamma->map_coordinate_1.lon,
               &gamma->map_coordinate_1.alt);
      else if (strncmp(key, "map_coordinate_2:", 17) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->map_coordinate_2.lat,
               &gamma->map_coordinate_2.lon,
               &gamma->map_coordinate_2.alt);
      else if (strncmp(key, "map_coordinate_3:", 17) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->map_coordinate_3.lat,
               &gamma->map_coordinate_3.lon,
               &gamma->map_coordinate_3.alt);
      else if (strncmp(key, "map_coordinate_4:", 17) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->map_coordinate_4.lat,
               &gamma->map_coordinate_4.lon,
               &gamma->map_coordinate_4.alt);
      else if (strncmp(key, "map_coordinate_5:", 17) == 0)
        sscanf(value, "%lf %lf %lf",
               &gamma->map_coordinate_5.lat,
               &gamma->map_coordinate_5.lon,
               &gamma->map_coordinate_5.alt);
      else if (strncmp(key, "number_of_state_vectors:", 24) == 0) {
        gamma->number_of_state_vectors = atoi(value);
        gamma->stVec = meta_state_vectors_init(gamma->number_of_state_vectors);
        fscanf(fp, "time_of_first_state_vector: %lf   s\n", 
	       &gamma->stVec->second);
        fscanf(fp, "state_vector_interval: %lf   s\n", &interval);
        gamma->stVec->year = gamma->acquisition.year;
        ymd.year = gamma->acquisition.year;
        ymd.month = gamma->acquisition.month;
        ymd.day = gamma->acquisition.day;
        date_ymd2jd(&ymd, &jd);
        gamma->stVec->julDay = jd.jd;
        for (ii=0; ii<gamma->stVec->vector_count; ii++) {
          gamma->stVec->vecs[ii].time = ii*interval;
          fgets(line, 1024, fp);
          sscanf(line, "%s:%s", key, value);
          sscanf(value, "%lf %lf %lf",
                 &gamma->stVec->vecs[ii].vec.pos.x,
                 &gamma->stVec->vecs[ii].vec.pos.y,
                 &gamma->stVec->vecs[ii].vec.pos.z);
          fgets(line, 1024, fp);
          sscanf(line, "%s:%s", key, value);
          sscanf(value, "%lf %lf %lf",
                 &gamma->stVec->vecs[ii].vec.vel.x,
                 &gamma->stVec->vecs[ii].vec.vel.y,
                 &gamma->stVec->vecs[ii].vec.vel.z);
        }
	double offset = gamma->stVec->second - gamma->acquisition.seconds;
	for (ii=0; ii<gamma->stVec->vector_count; ii++)
	  propagate(gamma->stVec->vecs[ii].vec,
		    gamma->stVec->vecs[ii].time,
		    gamma->stVec->vecs[ii].time + offset);
	gamma->stVec->second = gamma->acquisition.seconds;
      }
    }
  }
  FCLOSE(fp);

  if (image_data_type)
    sprintf(gamma->image_data_type, "%s", image_data_type);
  else
    sprintf(gamma->image_data_type, "IMAGE");

  // Generate metadata
  meta = gamma_msp2meta(gamma);

  // No clean up
  // Can't free gamma as the metadata structure uses the gamma state vector
  // structure allocated over here.

  return meta;
}

void import_gamma_msp(const char *inDataName, const char *inMetaName,
		      const char *data_type, const char *image_data_type, 
		      const char *outBaseName)
{
  meta_parameters *meta=NULL;
  char outFile[255];

  // Read  metadata from gamma_msp file
  meta = meta_read_gamma_msp(inMetaName, data_type, image_data_type);
  sprintf(outFile, "%s.meta", outBaseName);
  meta_write(meta, outFile);

  // Copy generic binary file
  sprintf(outFile, "%s.img", outBaseName);
  fileCopy(inDataName, outFile);
}

gamma_isp *gamma_isp_init()
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
  strcpy(g->image_data_type, "UNKNOWN");
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
  g->stVec = NULL;

  return g;
}

gamma_msp *gamma_msp_init()
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
  strcpy(g->image_data_type, "UNKNOWN");
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
  g->sec_range_migration = MAGIC_UNSET_INT;
  g->azimuth_deskew = MAGIC_UNSET_INT;
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
  g->azimuth_offset = MAGIC_UNSET_DOUBLE;
  g->azimuth_pixel_spacing = MAGIC_UNSET_DOUBLE;
  g->azimuth_resolution = MAGIC_UNSET_DOUBLE;
  g->range_pixels = MAGIC_UNSET_INT;
  strcpy(g->image_format, MAGIC_UNSET_STRING);
  g->sensor_latitude = MAGIC_UNSET_DOUBLE;
  g->sensor_longitude = MAGIC_UNSET_DOUBLE;
  g->sensor_track_angle = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_1.lat = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_1.lon = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_1.alt = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_2.lat = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_2.lon = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_2.alt = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_3.lat = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_3.lon = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_3.alt = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_4.lat = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_4.lon = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_4.alt = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_5.lat = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_5.lon = MAGIC_UNSET_DOUBLE;
  g->map_coordinate_5.alt = MAGIC_UNSET_DOUBLE;
  g->stVec = NULL;

  return g;
}

void import_gamma(char *dataName, char *metaName, char *ceosName, 
		  char *image_data_type, char *outBaseName)
{
  char line[1024], key[512], tmpDir[1024], tmpFile[1024];

  // GAMMA data does not necessarily follow the regular naming convention,
  // so the basename concept does not apply. Hence, we deal with the data
  // and metadata files directly. The CEOS file gives us the SAR geometry
  // as any regular CEOS ingest.

  // Determine the dimensions and multilooking factors from the GAMMA 
  // metadata file.
  gamma_meta *gamma = (gamma_meta *) MALLOC(sizeof(gamma_meta));
  FILE *fpIn = FOPEN(metaName, "r");
  fgets(line, 1024, fpIn);
  if (!strncmp_case(line, "GAMMA INTERFEROMETRIC SAR PROCESSOR (ISP)", 41) &&
      !strncmp_case(line, "GAMMA MODULAR SAR PROCESSOR (MSP)", 33))
    asfPrintStatus("Metadata file (%s) is not in Gamma format\n");
  while (fgets(line, 1024, fpIn)) {
    char *p = strchr(line, ':');
    if (p) {
      sscanf(line, "%s:", key);
      char *value = p + 1;
      if (strncmp(key, "azimuth_pixels:", 15) == 0)
	gamma->line_count = atoi(value);
      else if (strncmp(key, "azimuth_lines:", 14) == 0)
	gamma->line_count = atoi(value);
      else if (strncmp(key, "range_pixels:", 13) == 0)
	gamma->sample_count = atoi(value);
      else if (strncmp(key, "range_samples:", 15) == 0)
	gamma->sample_count = atoi(value);
      else if (strncmp(key, "azimuth_looks:", 14) == 0)
	gamma->azimuth_look_count = atoi(value);
      else if (strncmp(key, "range_looks:", 12) == 0)
	gamma->range_look_count = atoi(value);
      else if (strncmp(key, "image_format:", 13) == 0) {
	char *str = trim_spaces(value);
	if (strcmp_case(str, "FCOMPLEX") == 0)
	  gamma->data_type = COMPLEX_REAL32;
	else if (strcmp_case(str, "SCOMPLEX") == 0)
	  gamma->data_type = COMPLEX_INTEGER16;
	else if (strcmp_case(str, "FLOAT") == 0)
	  gamma->data_type = REAL32;
	else if (strcmp_case(str, "SHORT") == 0)
	  gamma->data_type = INTEGER16;
	else if (strcmp_case(str, "BYTE") == 0)
	  gamma->data_type = BYTE;
      }
    }
  }
  FCLOSE(fpIn);

  // Create temporary directory
  char *baseName = get_basename(outBaseName);
  strcpy(tmpDir, baseName);
  strcat(tmpDir, "-");
  strcat(tmpDir, time_stamp_dir());
  create_clean_dir(tmpDir);
  sprintf(tmpFile, "%s/import", tmpDir);

  char *outFile = appendExt(outBaseName, ".img");

  // Generate an amplitude image from the first band of the ancillary CEOS file.
  // This ensures that we can always terrain corrected the data.
  char **inBandName=NULL, **inMetaName=NULL;
  int nBands, trailer;
  require_ceos_pair(ceosName, &inBandName, &inMetaName, &nBands, &trailer);
  meta_parameters *metaIn = meta_read(inMetaName[0]);
  if (!metaIn && !metaIn->sar)
    asfPrintError("Ancillary file (%s) does not contain a SAR image.\n",
		  ceosName);
  char *polarization = get_polarization(inBandName[0]);
  double range_scale = 1.0 / gamma->range_look_count;
  double azimuth_scale = 1.0 / gamma->azimuth_look_count;
  int multilook = FALSE;
  if (gamma->range_look_count != 1 || gamma->azimuth_look_count != 1)
    multilook = TRUE;
  import_ceos(ceosName, tmpFile, polarization, NULL, &range_scale, 
	      &azimuth_scale, NULL, 0, 0, -99, -99, NULL, r_AMP, FALSE, 
	      FALSE, FALSE, FALSE, FALSE);
  meta_free(metaIn);  

  // Write Gamma bands depending on what image data type was passed in.
  strcat(tmpFile, ".img");
  metaIn = meta_read(tmpFile);
  char **band_names = 
    extract_band_names(metaIn->general->bands, metaIn->general->band_count);
  meta_parameters *metaOut = meta_read(tmpFile);
  int line_count = metaIn->general->line_count;
  int sample_count = metaIn->general->sample_count;
  int ii, kk;

  if (!image_data_type ||
      strlen(image_data_type) == 0 ||
      strcmp(image_data_type, MAGIC_UNSET_STRING) == 0)
  {
    // user did not specify image data type... we will try to figure
    // it out based on the size of the file!
    long long sz = fileSize(dataName);
    long long coherence_sz = line_count*sample_count*4;
    long long interferogram_sz = coherence_sz * 2;
    // we are reassigning the pointer image_data_type, but since the caller
    // frees this anyway, we aren't going to leak the original pointed-to val
    if (sz == coherence_sz)
      image_data_type = "COHERENCE";
    else if (sz == interferogram_sz)
      image_data_type = "INTERFEROGRAM";
    else
      asfPrintError("Could not determine what type of GAMMA data this is!\n"
                    "  The file size is: %lld\n"
                    "  A coherence image would be: %lld\n"
                    "  An interferogram image would be: %lld\n"
                    "Import using the command-line asf_import, and specify\n"
                    "the image data type using the command-line option.\n",
                    sz, coherence_sz, interferogram_sz);

    asfPrintStatus("Detected GAMMA image: %s\n\n", image_data_type);
  }

  if (strcmp_case(image_data_type, "COHERENCE") == 0) {
    asfPrintStatus("Importing GAMMA coherence image ...\n");
    FILE *fpOut = FOPEN(outFile, "wb");
    float *floatBuf = (float *) MALLOC(sizeof(float)*sample_count);
    metaIn->general->band_count = 1;
    metaIn->general->data_type = REAL32;
    metaOut->general->band_count = 2;
    metaOut->general->data_type = REAL32;
    metaOut->general->image_data_type = IMAGE_LAYER_STACK;
    metaOut->sar->multilook = multilook;
    meta_write(metaOut, outFile);
    sprintf(metaOut->general->bands, "%s,COHERENCE", band_names[0]);
    fpIn = FOPEN(tmpFile, "rb");
    for (ii=0; ii<line_count; ii++) {
      get_float_line(fpIn, metaIn, ii, floatBuf);
      put_band_float_line(fpOut, metaOut, 0, ii, floatBuf);
      asfLineMeter(ii, line_count);
    }
    FCLOSE(fpIn);
    fpIn = FOPEN(dataName, "rb");
    for (ii=0; ii<line_count; ii++) {
      get_float_line(fpIn, metaIn, ii, floatBuf);
      put_band_float_line(fpOut, metaOut, 1, ii, floatBuf);
      asfLineMeter(ii, line_count);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(floatBuf);
    meta_write(metaOut, outFile);
    meta_free(metaIn);
    meta_free(metaOut);
  }
  else if (strcmp_case(image_data_type, "INTERFEROGRAM") == 0) {
    asfPrintStatus("   Importing GAMMA coherence and interferogram ...\n");
    FILE *fpOut = FOPEN(outFile, "wb");
    complexFloat *floatCpxBuf = 
      (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count);
    float *floatAmpBuf = (float *) MALLOC(sizeof(float)*sample_count);
    float *floatPhaseBuf = (float *) MALLOC(sizeof(float)*sample_count);
    metaIn->general->band_count = 1;
    metaIn->general->data_type = REAL32;
    metaOut->general->band_count = 3;
    metaOut->general->data_type = REAL32;
    metaOut->general->image_data_type = IMAGE_LAYER_STACK;
    metaOut->sar->multilook = multilook;
    meta_write(metaOut, outFile);
    sprintf(metaOut->general->bands, "%s,COHERENCE,INTERFEROGRAM", 
	    band_names[0]);
    fpIn = FOPEN(tmpFile, "rb");
    for (ii=0; ii<line_count; ii++) {
      get_float_line(fpIn, metaIn, ii, floatAmpBuf);
      put_band_float_line(fpOut, metaOut, 0, ii, floatAmpBuf);
      asfLineMeter(ii, line_count);
    }
    FCLOSE(fpIn);
    metaIn->general->data_type = COMPLEX_REAL32;
    fpIn = FOPEN(dataName, "rb");
    for (ii=0; ii<line_count; ii++) {
      get_complexFloat_line(fpIn, metaIn, ii, floatCpxBuf);
      for (kk=0; kk<sample_count; kk++) {
	float re = floatCpxBuf[kk].real;
	float im = floatCpxBuf[kk].imag;
	if (re != 0.0 || im != 0.0) {
	  floatAmpBuf[kk] = sqrt(re*re + im*im);
	  floatPhaseBuf[kk] = atan2(im, re);
	} 
	else
	  floatAmpBuf[kk] = floatPhaseBuf[kk] = 0.0;
      }
      put_band_float_line(fpOut, metaOut, 1, ii, floatAmpBuf);
      put_band_float_line(fpOut, metaOut, 2, ii, floatPhaseBuf);
      asfLineMeter(ii, line_count);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    FREE(floatCpxBuf);
    FREE(floatAmpBuf);
    FREE(floatPhaseBuf);
    meta_write(metaOut, outFile);
    meta_free(metaIn);
    meta_free(metaOut);
  }
  else {
    meta_free(metaIn);
    meta_free(metaOut);
    asfPrintError("Unsupported GAMMA data type: %s\n", image_data_type);
  }

  remove_dir(tmpDir);
}
