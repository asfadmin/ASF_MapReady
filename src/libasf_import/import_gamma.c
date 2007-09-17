#include "gamma.h"
#include "asf_meta.h"
#include "dateUtil.h"

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

  // Allocate memory
  gamma = (gamma_isp *) MALLOC(sizeof(gamma_isp));

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
        sprintf(gamma->title, "%s", str);
      }
      else if (strncmp(key, "sensor:", 7) == 0) {
	str = trim_spaces(value);
	sprintf(gamma->sensor, "%s", str);
      }
      else if (strncmp(key, "date:", 5) == 0)
	sscanf(value, "%d\t%d\t%d", &gamma->acquisition[0],
	       &gamma->acquisition[1], &gamma->acquisition[2]);
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
  sprintf(outFile, "%s.img", outBaseName);
  meta = gamma_isp2meta(gamma, stVec);
  meta_write(meta, outFile);
  
  // Copy generic binary file
  sprintf(inFile, "%s.slc", inBaseName);
  fileCopy(inFile, outFile);
}

