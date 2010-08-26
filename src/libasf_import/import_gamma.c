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

void import_gamma(char *dataName, char *metaName, char *slaveName,
                  char *igramName, char *cohName, char *baselineName, 
		  char *outBaseName)
{
  meta_parameters *metaIn, *metaOut;
  FILE *fpIn, *fpOut;
  float *floatBuf, *floatAmpBuf, *floatPhaseBuf, amp;
  complexFloat *floatCpxBuf;
  char tmp[1024], reason[1024];
  int ii, kk, bands=0, status=TRUE;

  // GAMMA data does not necessarily follow the regular naming convention,
  // so the basename concept does not apply. Hence, we deal with the data
  // and metadata files directly.

  // Check the existence of files
  if (!fileExists(dataName)) {
    sprintf(tmp, "Missing amplitude image file (%s)\n", dataName);
    strcat(reason, tmp);
    status = FALSE;
  }
  else 
    bands = 1;
  if (!fileExists(metaName)) {
    sprintf(tmp, "Missing metadata file (%s)\n", metaName);
    strcat(reason, tmp);
    status = FALSE;
  }
  if (igramName && fileExists(igramName))
    bands += 2;
  if (cohName && fileExists(cohName))
    bands++;
  if (!status)
    asfPrintError("Could not find all files!\n%s\n", reason);

  // Check metadata
  fpIn = FOPEN(metaName, "r");
  char *line = (char *) MALLOC(sizeof(char)*1024);
  fgets(line, 1024, fpIn);
  if (strstr(line, "ISP")) {
    metaIn = meta_read_gamma_isp(metaName, "FLOAT", "INSAR_STACK");
    metaOut = meta_read_gamma_isp(metaName, "FLOAT", "INSAR_STACK");
  }
  else if (strstr(line, "MSP")) {
    metaIn = meta_read_gamma_msp(metaName, "FLOAT", "AMPLITUDE");
    metaOut = meta_read_gamma_msp(metaName, "FLOAT", "AMPLITUDE");
  }
  else
    asfPrintError("Metadata file (%s) is not in Gamma format\n", metaName);
  FCLOSE(fpIn);

  char *outFile = appendExt(outBaseName, ".img");

  // Read the metadata parameter file. We will update the metadata
  // structure as we go along
  metaOut->general->band_count = bands;
  int sample_count = metaOut->general->sample_count;
  int line_count = metaOut->general->line_count;
  int current_band = 0;

  // The first band is always going to be the amplitude image. Otherwise
  // we can't guarantee terrain correction and such.
  floatBuf = (float *) MALLOC(sizeof(float)*sample_count);
  outFile = appendExt(outBaseName, ".img");
  fpIn = FOPEN(dataName, "rb");
  fpOut = FOPEN(outFile, "wb");
  asfPrintStatus("Writing amplitude image ...\n");
  for (ii=0; ii<line_count; ii++) {
    get_float_line(fpIn, metaIn, ii, floatBuf);
    for (kk=0; kk<sample_count; kk++) {
      amp = sqrt(floatBuf[kk]);
      floatBuf[kk] = amp;
    }
    put_band_float_line(fpOut, metaOut, current_band, ii, floatBuf);
    asfLineMeter(ii, line_count);
  }
  FCLOSE(fpIn);
  FREE(floatBuf);
  strcpy(metaOut->general->bands, "AMP");
  
  // Lets add an interferogram. This is a little trickier, since it comes
  // in complex form, and we need to store it a two bands
  if (igramName && strlen(igramName) > 0) {
    fpIn = FOPEN(igramName, "rb");
    current_band += 2;
    asfPrintStatus("\nWriting interferogram ...\n");
    floatCpxBuf = 
      (complexFloat *) MALLOC(sizeof(complexFloat)*sample_count);
    floatAmpBuf = (float *) MALLOC(sizeof(float)*sample_count);
    floatPhaseBuf = (float *) MALLOC(sizeof(float)*sample_count);
    metaIn->general->data_type = COMPLEX_REAL32;
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
      put_band_float_line(fpOut, metaOut, current_band, ii, floatAmpBuf);
      put_band_float_line(fpOut, metaOut, current_band+1, ii, floatPhaseBuf);
      asfLineMeter(ii, line_count);
    }
    FCLOSE(fpIn);
    FREE(floatCpxBuf);
    FREE(floatAmpBuf);
    FREE(floatPhaseBuf);
    strcat(metaOut->general->bands, ",INTERFEROGRAM_AMP,INTERFEROGRAM_PHASE");
  }

  // Lets add a coherence image. This is the simple case, because it
  // comes as floating point.
  if (cohName && strlen(cohName) > 0) {
    fpIn = FOPEN(cohName, "rb");
    current_band++;
    asfPrintStatus("\nWriting coherence image ...\n");
    floatBuf = (float *) MALLOC(sizeof(float)*sample_count);
    metaIn->general->data_type = REAL32;
    for (ii=0; ii<line_count; ii++) {
      get_float_line(fpIn, metaIn, ii, floatBuf);
      put_band_float_line(fpOut, metaOut, current_band, ii, floatBuf);
      asfLineMeter(ii, line_count);
    }
    FCLOSE(fpIn);
    FREE(floatBuf);
    strcat(metaOut->general->bands, ",COHERENCE");
  }
  FCLOSE(fpOut);

  // Add the InSAR block
  if (slaveName && strlen(slaveName) > 0 && 
      baselineName && strlen(baselineName) > 0) {
    metaOut->insar = meta_insar_init();
    strcpy(metaOut->insar->processor, "GAMMA");
    sprintf(metaOut->insar->master_image, "%s", get_basename(metaName));
    sprintf(metaOut->insar->slave_image, "%s", get_basename(slaveName));
    int center_line = metaOut->general->line_count / 2;
    int center_sample = metaOut->general->sample_count / 2;
    metaOut->insar->center_look_angle = 
      meta_look(metaOut, center_line, center_sample) * R2D;
    metaOut->insar->doppler = 
      metaIn->sar->range_doppler_coefficients[0];
    metaOut->insar->doppler_rate =
      metaIn->sar->range_doppler_coefficients[1] * 
      metaIn->general->y_pixel_size;

    // Read baseline components from baseline file
    double sin_look = sin(metaOut->insar->center_look_angle*D2R);
    double cos_look = cos(metaOut->insar->center_look_angle*D2R);
    double base_t, base_c, base_n, base_dt, base_dc, base_dn;
    fpIn = FOPEN(baselineName, "r");
    fscanf(fpIn, "initial_baseline(TCN): %lf %lf %lf  m   m   m\n", 
	   &base_t, &base_c, &base_n);
    fscanf(fpIn, "initial_baseline_rate: %lf %lf %lf  m/s m/s m/s\n", 
	   &base_dt, &base_dc, &base_dn);
    metaOut->insar->baseline_length = sqrt(base_c*base_c + base_n*base_n);
    metaOut->insar->baseline_parallel = base_c*sin_look - base_n*cos_look;
    metaOut->insar->baseline_parallel_rate = 
      base_dc*sin_look - base_dn*cos_look;
    metaOut->insar->baseline_perpendicular = base_c*cos_look + base_n*sin_look;
    metaOut->insar->baseline_perpendicular_rate = 
      base_dc*cos_look + base_dn*sin_look;
    fscanf(fpIn, "precision_baseline(TCN): %lf %lf %lf  m   m   m\n", 
	   &base_t, &base_c, &base_n);
    fscanf(fpIn, "precision_baseline_rate: %lf %lf %lf  m/s m/s m/s\n", 
	   &base_dt, &base_dc, &base_dn);
    if (!FLOAT_EQUIVALENT(base_c, 0.0) && !FLOAT_EQUIVALENT(base_n, 0.0)) {
      metaOut->insar->baseline_length = sqrt(base_c*base_c + base_n*base_n);
      metaOut->insar->baseline_parallel = base_c*sin_look - base_n*cos_look;
      metaOut->insar->baseline_parallel_rate = 
	base_dc*sin_look - base_dn*cos_look;
      metaOut->insar->baseline_perpendicular = 
	base_c*cos_look + base_n*sin_look;
      metaOut->insar->baseline_perpendicular_rate = 
	base_dc*cos_look + base_dn*sin_look;
    }
    FCLOSE(fpIn);
    
    // Determine temporal baseline
    char *p, key[512], *value;
    hms_time hms;
    hms.hour = hms.min = hms.sec = 0;
    ymd_date master, slave;
    fpIn = FOPEN(metaName, "r");
    while (fgets(line, 1024, fpIn)) {
      p = strchr(line, ':');
      if (p) {
	sscanf(line, "%s:", key);
	value = p+1;
	if (strncmp(key, "date:", 5) == 0)
	  sscanf(value, "%d %d %d", &master.year, &master.month, &master.day);
      }
    }
    FCLOSE(fpIn);
    fpIn = FOPEN(slaveName, "r");
    while (fgets(line, 1024, fpIn)) {
      p = strchr(line, ':');
      if (p) {
	sscanf(line, "%s:", key);
	value = p+1;
	if (strncmp(key, "date:", 5) == 0)
	  sscanf(value, "%d %d %d", &slave.year, &slave.month, &slave.day);
      }
    }
    FCLOSE(fpIn);
    metaOut->insar->baseline_temporal = 
      (int) fabs(date_difference(&master, &hms, &slave, &hms) / 86400 + 0.5);

    // Calculate critical baseline
    double range = meta_get_slant(metaOut, center_line, center_sample);
    double wavelength = metaOut->sar->wavelength;
    double bandwidth = metaOut->sar->chirp_rate;
    double tan_incid = tan(meta_incid(metaOut, center_line, center_sample));
    metaOut->insar->baseline_critical =
      wavelength * range * bandwidth * tan_incid / SPD_LIGHT;
  }

  // Time to write the metadata file
  meta_write(metaOut, outFile);
  
  // Clean up time
  meta_free(metaIn);
  meta_free(metaOut);
  FREE(line);
}
