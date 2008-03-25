#include <ctype.h>
#include "vexcel_plain.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "asf_nan.h"

//vexcel_plain *vp_init();

void import_vexcel_plain(const char *inBaseName, const char *outBaseName)
{
  /*
  vp_isp *vp;
  meta_parameters *meta=NULL;
  FILE *fp;
  ymd_date ymd;
  julian_date jd;
  int structDepth;
  char structName[16][255];
  char inFile[255], outFile[255], line[1024], *p, key[512], *value, *str;
  char *facility, *processor, *version;
  int indexBeam=0, indexPolarization=0, indexStVec=0, indexGr2Sr=0;
  int ii, vectors, i, k, l, m, n;
  double interval;

  // Initialize the vexcel plain struct
  vp = vp_init();

  // Initialize structure stack
  structDepth = 0;
  strcpy(structName[structDepth],"");

  // Read vexcel plain parameter file
  sprintf(inFile, "%s.par", inBaseName);

  fp = FOPEN(inFile, "r");
  while (fgets(line, 1024, fp)) {
    // Update structure
    if (strchr(line, '{') != NULL) {
      char newStruct[255];
      char index[255];
      sscanf(line, "%s", newStruct);
      if (strcmp(newStruct, "beam") == 0) {
	sprintf(index, "[%d]", indexBeam);
	indexBeam++;
      }
      else if (strcmp(newStruct, "Polarization") == 0) {
	sprintf(index, "[%d]", indexPolarization);
	indexPolarization++;
      }
      else if (strcmp(newStruct, "state_vector") == 0) {
	sprintf(index, "[%d]", indexStVec);
	indexStVec++;
      }
      else if (strcmp(newStruct, "gr2sr") == 0) {
	sprintf(index, "[%d]", indexGr2Sr);
	indexGr2Sr++;
      }
      else
	strcpy(index, "");
      strcpy(structName[structDepth+1],structName[structDepth]);
      if (structDepth != 0)
	strcat(structName[structDepth+1], ".");
      strcat(structName[structDepth+1], newStruct);
      strcat(structName[structDepth+1], index);
      structDepth++;
    }
    else if (strchr(line, '}') != NULL)
      structDepth--;
    // Read actual contents
    else {
      p = strchr(line, ':');
      if (p) {
	char thisParam[255];
	sscanf(line, "%s:", key);
	strcpy(thisParam, structName[structDepth]);
	if (structDepth != 0)
	  strcat(thisParam, ".");
	strcat(thisParam, key);
	printf("param: %s\n", thisParam);
	value = p+1;

        // sensor block
        if (strcmp(thisParam, "sensor.sensor_name:") == 0)
          sprintf(vp->sensor.sensor_name, "%s", value);
	else if (strcmp(thisParam, "sensor.instrument_name:") == 0)
	  sprintf(vp->sensor.instrument_name, "%s", value);
        else if (strcmp(thisParam, "sensor.instrument_type:") == 0)
	  sprintf(vp->sensor.instrument_type, "%s", value);
        else if (strcmp(thisParam, "sensor.clock_angle:") == 0)
	  vp->sensor.clock_angle = atof(value);
        else if (strcmp(thisParam, "sensor.nr_temperatures:") == 0)
	  vp->sensor.nr_temperatures = atoi(value);
	// FIXME: no idea what a temperature block looks like
        else if (strcmp(thisParam, "sensor.nr_beams:") == 0)
	  vp->sensor.nr_beams = atoi(value);
	if (vp->sensor.nr_beams > 0) {
	  vp->sensor.beam = 
	    (vp_beam *) MALLOC(sizeof(vp_beam)*vp->sensor.nr_beams);
	  for (i=0; i<vp->sensor.nr_beams; i++) {
	    char tmp[255];
	    fgets(line, 1024, fp);
	    sscanf(line, "%s:", key);
	    strcpy(thisParam, structName[structDepth]);
	    if (structDepth != 0)
	      strcat(thisParam, ".");
	    strcat(thisParam, key);
	    p = strchr(line, ':');
	    value = p+1;

	    // beam
	    sprintf(tmp, "sensor.beam[%d].beam_name:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      sprintf(vp->sensor.beam[i].beam_name, "%s", value);
	    sprintf(tmp, "sensor.beam[%d].nr_of_samples:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].nr_of_samples = atoi(value);
	    sprintf(tmp, "sensor.beam[%d].echo_delay:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].echo_delay = atof(value);
	    sprintf(tmp, "sensor.beam[%d].carrier_freq:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].carrier_freq = atof(value);
	    sprintf(tmp, "sensor.beam[%d].sampling_freq:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].sampling_freq = atof(value);
	    sprintf(tmp, "sensor.beam[%d].PRF:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].prf = atof(value);
	    sprintf(tmp, "sensor.beam[%d].chirp_rate:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].chirp_rate = atof(value);
	    sprintf(tmp, "sensor.beam[%d].pulse_length:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].pulse_length = atof(value);
	    sprintf(tmp, "sensor.beam[%d].look_angle:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].look_angle = atof(value);
	    sprintf(tmp, "sensor.beam[%d].incidence_angle:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].incidence_angle = atof(value);
	    sprintf(tmp, "sensor.beam[%d].range_spectrum_snr:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].range_spectrum_snr = atof(value);
	    sprintf(tmp, "sensor.beam[%d].replica_energy_ref_level:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].replica_energy_ref_level = atof(value);
	    sprintf(tmp, "sensor.beam[%d].cal1_cal2_diff_ref_level:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].cal1_cal1_diff_level = atof(value);
	    sprintf(tmp, "sensor.beam[%d].thermal_noise_ref_level:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].thermal_noise_ref_level = atof(value);
	    sprintf(tmp, "sensor.beam[%d].gain_corctn_factor:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].gain_corctn_factor = atof(value);
	    sprintf(tmp, "sensor.beam[%d].gain_scale:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      vp->sensor.beam[i].gain_scale = atof(value);

	    // Polarization block
	    sprintf(tmp, 
		    "sensor.beam[%d].PolarizationBlock.NrPolarizations:", i);
	    if (strcmp(thisParam, tmp) == 0) {
	      n = atof(value);
	      vp->sensor.beam[i].polarization_block.nr_polarizations = n;
	      vp->sensor.beam[i].polarization_block.polarization =
		(vp_polarization *) MALLOC(sizeof(vp_polarization)*n);
	      for (k=0; k<n; k++) {
		vp_polarization polarization;
		fgets(line, 1024, fp);
		sscanf(line, "%s:", key);
		strcpy(thisParam, structName[structDepth]);
		if (structDepth != 0)
		  strcat(thisParam, ".");
		strcat(thisParam, key);
		p = strchr(line, ':');
		value = p+1;
		sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
			"Polarization[%d].polarization:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  strcpy(polarization.polarization, "%s", value);
                sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].polarization_amplitude:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.polarization_amplitude = atof(value);
                sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].polarization_phase:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.polarization_phase = atof(value);
                sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].stc_pattern_id:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.stc_pattern_id = atoi(value);
		sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].IQStatistics.I_mean:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.iq_statistics.I_mean = atof(value);
                sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].IQStatistics.Q_mean:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.iq_statistics.Q_mean = atof(value);
                sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].IQStatistics.I_std:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.iq_statistics.I_std = atof(value);
		sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].IQStatistics.Q_std:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.iq_statistics.Q_std = atof(value);
		sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
                        "Polarization[%d].IQStatistics.IQ_corr:", i, k);
		if (strcmp(thisParam, tmp) == 0)
		  polarization.iq_statistics.IQ_corr = atof(value);
		vp->sensor.beam[1].polarization_block.polarization[k] =
		  polarization;
	      }

	      // Doppler centroid parameters
	      vp_doppler_centroid_parameters doppler_params;
	      vp_coefficients doppler_centroid;
	      doppler_params.doppler_centroid_coefficients = doppler_centroid;
	      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
		      "doppler_centroid_coefficients."
		      "reference_first_dimension:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_centroid.reference_first_dimension = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "doppler_centroid_coefficients."
		      "reference_second_dimension:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_centroid.reference_second_dimension = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "doppler_centroid_coefficients."
		      "number_of_coefficients_first_dimension:", i);
	      if (strcmp(thisParam, tmp) == 0) {
		m = atoi(value);
		doppler_centroid.number_of_coefficients_first_dimension = m;
	      }
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "doppler_centroid_coefficients."
		      "number_of_coefficients_second_dimension:", i);
	      if (strcmp(thisParam, tmp) == 0) {
		n = atoi(value);
		doppler_centroid.number_of_coefficients_second_dimension = n;
	      }
	      doppler_centroid.a = (double *) MALLOC(sizeof(double) * m * n);
	      for (k=0; k<n; k++) {
		for (l=0; l<m; l++) {
		  sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
			  "doppler_centroid_coefficients.a%d%d", i, k, l);
		  doppler_centroid.a[k*m+l] = atof(value);
		}
	      }
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "reference_range:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.reference_range = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "reference_date:", i);
	      if (strcmp(thisParam, tmp) == 0)
		strcpy(doppler_params.reference_date, "%s", value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "ambiguity_number:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.ambiguity_number = atoi(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "MLCC_ambiguity_number_occurence:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.MLCC_ambiguity_number_occurence = atoi(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "MLBF_ambiguity_number_occurence:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.MLBF_ambiguity_number_occurence = atoi(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "DAR_doppler:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.DAR_doppler = atof(value);
	      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "Predict_doppler:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.Predict_doppler = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
                      "DAR_confidence:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.DAR_confidence = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
		      "doppler_fit_correlation:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_params.doppler_fit_correlation = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
		      "doppler_status:", i);
	      if (strcmp(thisParam, tmp) == 0)
		strcpy(doppler_params.doppler_status, "%s", value);
	      vp->sensor.beam[1].doppler_centroid_parameters  = doppler_params;

	      // Doppler rate parameters
              vp_doppler_rate_parameters doppler_rate;
              vp_coefficients velocity;
              doppler_params.doppler_rate_parameters = doppler_rate;
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
                      "effective_velocity_coefficients."
                      "reference_first_dimension:", i);
              if (strcmp(thisParam, tmp) == 0)
                velocity.reference_first_dimension = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
                      "effectve_velocity_coefficients."
                      "reference_second_dimension:", i);
              if (strcmp(thisParam, tmp) == 0)
                velocity.reference_second_dimension = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
                      "effective_velocity_coefficients."
                      "number_of_coefficients_first_dimension:", i);
              if (strcmp(thisParam, tmp) == 0) {
                m = atoi(value);
                velocity.number_of_coefficients_first_dimension = m;
              }
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
                      "effective_velocity_coefficients."
                      "number_of_coefficients_second_dimension:", i);
              if (strcmp(thisParam, tmp) == 0) {
                n = atoi(value);
                velocity.number_of_coefficients_second_dimension = n;
              }
              velocity.a = (double *) MALLOC(sizeof(double) * m * n);
              for (k=0; k<n; k++) {
                for (l=0; l<m; l++) {
                  sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
                          "effective_velocity_coefficients.a%d%d", i, k, l);
                  velocity.a[k*m+l] = atof(value);
                }
              }
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters.veff:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_rate.veff = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
		      "reference_range:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_rate.reference_range = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
		      "reference_date:", i);
	      if (strcmp(thisParam, tmp) == 0)
		strcpy(doppler_rate.reference_date, "%s", value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
		      "autofocus_scale_factor:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_rate.autofocus_scale_factor = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
		      "autofocus_snr:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_rate.autofocus_snr = atof(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
		      "autofocus_suggested_ambiguity_number:", i);
	      if (strcmp(thisParam, tmp) == 0)
		doppler_rate.autofocus_suggested_ambiguity_number = 
		  atoi(value);
              sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
		      "autofocus_status:", i);
	      if (strcmp(thisParam, tmp) == 0)
		strcpy(doppler_rate.autofocus_status, "%s", value);
	      vp->sensor.beam[1].doppler_rate_parameters  = doppler_rate;
	    }
	  }
	}

	// ephemeris
	if (strcmp(thisParam, "sensor.ephemeris.sv_block.nr_sv:") == 0) {
	  n = atoi(value);
	  vp->sensor.ephemeris.sv_block.nr_sv = n;
	  vp_state_vectors stVec;
	  vp->sensor.ephemeris.sv_block.state_vector = 
	    (vp_state_vectors *) MALLOC(sizeof(vp_state_vectors) * n);
	  for (i=0; i<n; i++) {
	    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].x:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      stVec.x = atof(value);
            sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].y:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      stVec.y = atof(value);
            sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].z:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      stVec.z = atof(value);
            sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].xv:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      stVec.xv = atof(value);
            sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].xy:", i);
            if (strcmp(thisParam, tmp) == 0)
              stVec.xy = atof(value);
            sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].xz:", i);
            if (strcmp(thisParam, tmp) == 0)
              stVec.xz = atof(value);
	    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[i].Date:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      strcpy(stVec.Date, "%s", value);
	    vp->sensor.ephemeris.sv_block.state_vector[i] = stVec;
	  }
	}
	vp_attitude attitude;
	if (strcmp(thisParam, "sensor.ephemeris.Attitude.yaw:") == 0)
	  attitude.yaw = atof(value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.roll:") == 0)
	  attitude.roll = atof(value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.pitch:") == 0)
	  attitude.pitch = atof(value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.Date:") == 0)
	  strcpy(attitude.date, "%s", value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.yawpoly."
			"reference:") == 0)
	  attitude.yawpoly.reference = atof(value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.yawpoly."
			"number_of_coefficients:") == 0) {
	  n = atoi(value);
	  attitude.yawpoly.number_of_coefficients = n;
	  attitude.yawpoly.a = (double *) MALLOC(sizeof(double) * n);
	  for (i=0; i<n; i++) {
	    sprintf(tmp, "sensor.ephemeris.Attitude.yawpoly.a%d:", i);
	    if (strcmp(thisParam, tmp) == 0)
	      attitude.yawpoly.a[i] = atof(value);
	  }
	}
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.rollpoly."
                        "reference:") == 0)
          attitude.rollpoly.reference = atof(value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.rollpoly."
                        "number_of_coefficients:") == 0) {
          n = atoi(value);
          attitude.rollpoly.number_of_coefficients = n;
          attitude.rollpoly.a = (double *) MALLOC(sizeof(double) * n);
          for (i=0; i<n; i++) {
            sprintf(tmp, "sensor.ephemeris.Attitude.rollpoly.a%d:", i);
            if (strcmp(thisParam, tmp) == 0)
              attitude.rollpoly.a[i] = atof(value);
          }
        }
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.pitchpoly."
                        "reference:") == 0)
          attitude.pitchpoly.reference = atof(value);
        else if (strcmp(thisParam, "sensor.ephemeris.Attitude.pitchpoly."
                        "number_of_coefficients:") == 0) {
          n = atoi(value);
          attitude.pitchpoly.number_of_coefficients = n;
          attitude.pitchpoly.a = (double *) MALLOC(sizeof(double) * n);
          for (i=0; i<n; i++) {
            sprintf(tmp, "sensor.ephemeris.Attitude.pitchpoly.a%d:", i);
            if (strcmp(thisParam, tmp) == 0)
              attitude.pitchpoly.a[i] = atof(value);
          }
        }
	vp->sensor.ephemeris.attitude = attitude;
	if (strcmp(thisParam, "sensor.ephemeris.OrbitNr:") == 0)
	  vp->sensor.ephemeris.orbit_nr = atoi(value);
	else if (strcmp(thisParam, "sensor.ephemeris.OrbitNr_Date:") == 0)
	  strcpy(vp->sensor.ephemis.orbit_nr_date, "%s", value);
	else if (strcmp(thisParam, "sensor.ephemeris.Gha.angle:") == 0)
	  vp->sensor.ephemeris.gha.angle = atof(value);
	else if (strcmp(thisParam, "sensor.ephemeris.Gha.date:") == 0)
	  strcpy(vp->sensor.ephemeris.gha.date, "%s", value);
	else if (strcmp(thisParam, "sensor.ephemeris.Type:") == 0)
	  strcpy(vp->sensor.ephemeris.type, "%s", value);

  // raw SAR image block
  strcpy(v->raw_sar_image.image_desc.facility, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.image_desc.format, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.image_desc.type, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.bytes_per_pixel = MAGIC_UNSET_INT;
  strcpy(v->raw_sar_image.image_desc.title, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.pixel_resolution = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.line_spacing = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.line_resolution = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.nr_pixels = MAGIC_UNSET_INT;
  v->raw_sar_image.image_desc.nr_lines = MAGIC_UNSET_INT;
  v->raw_sar_image.image_desc.min_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.max_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.mean_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.sigma_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.mean_intensity_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.sigma_intensity_value = MAGIC_UNSET_DOUBLE;
  strcpy(v->raw_sar_image.image_desc.coord.earth_model.name,
         MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.image_desc.coord.earth_model.ellipsoid_name,
         MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.coord.earth_model.major = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.minor = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.terrain_height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.mass = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.delta_x = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.delta_y = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.delta_z = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.g = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.j2 = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.j3 = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.j4 = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.center_line_center_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.center_line_center_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.center_line_center_pixel.height
    = MAGIC_UNSET_DOUBLE;
  strcpy(v->raw_sar_image.processor_name, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.processor_version, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.first_line, MAGIC_UNSET_STRING);
  v->raw_sar_image.first_line_txpol = MAGIC_UNSET_CHAR;
  v->raw_sar_image.time_per_line = MAGIC_UNSET_DOUBLE;

  // GLI product block
  strcpy(v->gli_product.image_desc.facility, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_desc.format, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_desc.type, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.bytes_per_pixel = MAGIC_UNSET_INT;
  strcpy(v->gli_product.image_desc.title, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.pixel_resolution = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.line_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.line_resolution = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.nr_pixels = MAGIC_UNSET_INT;
  v->gli_product.image_desc.nr_lines = MAGIC_UNSET_INT;
  v->gli_product.image_desc.min_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.max_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.mean_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.sigma_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.mean_intensity_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.sigma_intensity_value = MAGIC_UNSET_DOUBLE;
  strcpy(v->gli_product.image_desc.coord.earth_model.name, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_desc.coord.earth_model.ellipsoid_name,
         MAGIC_UNSET_STRING);
  v->gli_product.image_desc.coord.earth_model.major = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.minor = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.terrain_height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.mass = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.delta_x = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.delta_y = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.delta_z = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.g = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.j2 = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.j3 = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.j4 = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.center_line_center_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.center_line_center_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.center_line_center_pixel.height
    = MAGIC_UNSET_DOUBLE;
  strcpy(v->gli_product.processor_name, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.processor_version, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_type, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.polarization, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.first_line, MAGIC_UNSET_STRING);
  v->gli_product.time_per_line = MAGIC_UNSET_DOUBLE;
  v->gli_product.orbit_nr = MAGIC_UNSET_DOUBLE;
  strcpy(v->gli_product.orbit_nr_date, MAGIC_UNSET_STRING);
  v->gli_product.track_angle = MAGIC_UNSET_DOUBLE;
  v->gli_product.near_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.center_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.far_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.skew_flag = MAGIC_UNSET_INT;
  v->gli_product.kaiser_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.kaiser_azimuth = MAGIC_UNSET_DOUBLE;
  v->gli_product.range_looks = MAGIC_UNSET_INT;
  v->gli_product.azimuth_looks = MAGIC_UNSET_INT;
  v->gli_product.range_block_average_factor = MAGIC_UNSET_INT;
  v->gli_product.azimuth_block_average_factor = MAGIC_UNSET_INT;
  v->gli_product.calibration_mode = MAGIC_UNSET_INT;
  v->gli_product.scan_id = MAGIC_UNSET_INT;
  v->gli_product.gr2sr_block.nr_gr2sr = MAGIC_UNSET_INT;
  v->gli_product.gr2sr_block.gr2sr = NULL;
  v->gli_product.range_FFT_size = MAGIC_UNSET_INT;
  v->gli_product.azimuth_offset = MAGIC_UNSET_DOUBLE;
  v->gli_product.processor_bandwidth = MAGIC_UNSET_DOUBLE;
  v->gli_product.kaiser_multilook = MAGIC_UNSET_DOUBLE;
  v->gli_product.bandwidth_per_look = MAGIC_UNSET_DOUBLE;
  v->gli_product.multi_look_overlap = MAGIC_UNSET_DOUBLE;
  v->gli_product.output_range_pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.output_azimuth_pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.eff_azimuth_looks = MAGIC_UNSET_DOUBLE;
  v->gli_product.total_multilook_bw = MAGIC_UNSET_DOUBLE;
  v->gli_product.raw_start_line = MAGIC_UNSET_INT;
  v->gli_product.nr_raw_lines = MAGIC_UNSET_INT;
  v->gli_product.raw_start_pixel = MAGIC_UNSET_INT;
  v->gli_product.nr_raw_pixels = MAGIC_UNSET_INT;



	if (strcmp(thisParam, "GLIProduct.image_desc.Title:") == 0)
	  sprintf(vp->gli_product.image_desc.facility, "%s", value);
	else if (strcmp(thisParam, "GLIProduct.image_desc.Facility") == 0) {
	  facility = trim_spaces(value);
	}
	else if (strncmp(key, "sensor:", 7) == 0) {
	  str = trim_spaces(value);
	  sprintf(vp->sensor, "%s", str);
	}
	else if (strncmp(key, "date:", 5) == 0)
	  sscanf(value, "%d %d %d %d %d %lf",
		 &vp->acquisition.year,   &vp->acquisition.month,
		 &vp->acquisition.day,    &vp->acquisition.hour,
		 &vp->acquisition.minute, &vp->acquisition.seconds);
	else if (strncmp(key, "start_time:", 11) == 0)
	  vp->start_time = atof(value);
	else if (strncmp(key, "center_time:", 12) == 0)
	  vp->center_time = atof(value);
	else if (strncmp(key, "end_time:", 9) == 0)
	  vp->end_time = atof(value);
	else if (strncmp(key, "azimuth_line_time:", 18) == 0)
	  vp->azimuth_line_time = atof(value);
	else if (strncmp(key, "line_header_size:", 17) == 0)
	  vp->line_header_size = atoi(value);
	else if (strncmp(key, "range_samples:", 14) == 0)
	  vp->range_samples = atoi(value);
	else if (strncmp(key, "azimuth_lines:", 14) == 0)
	  vp->azimuth_lines = atoi(value);
	else if (strncmp(key, "range_looks:", 12) == 0)
	  vp->range_looks = atoi(value);
	else if (strncmp(key, "azimuth_looks:", 14) == 0)
	  vp->azimuth_looks = atoi(value);
	else if (strncmp(key, "image_format:", 13) == 0) {
	  str = trim_spaces(value);
	  sprintf(vp->image_format, "%s", str);
	}
	else if (strncmp(key, "image_geometry:", 15) == 0) {
	  str = trim_spaces(value);
	  sprintf(vp->image_geometry, "%s", str);
	}
	else if (strncmp(key, "range_scale_factor:", 19) == 0)
	  vp->range_scale_factor = atof(value);
	else if (strncmp(key, "azimuth_scale_factor:", 21) ==0)
	  vp->azimuth_scale_factor = atof(value);
	else if (strncmp(key, "center_latitude:", 16) == 0)
	  vp->center_latitude = atof(value);
	else if (strncmp(key, "center_longitude:", 17) == 0)
	  vp->center_longitude = atof(value);
	else if (strncmp(key, "heading:", 8) == 0)
	  vp->heading = atof(value);
	else if (strncmp(key, "range_pixel_spacing:", 20) == 0)
	  vp->range_pixel_spacing = atof(value);
	else if (strncmp(key, "azimuth_pixel_spacing:", 22) == 0)
	  vp->azimuth_pixel_spacing = atof(value);
	else if (strncmp(key, "near_range_slc:", 15) == 0)
	  vp->near_range_slc = atof(value);
	else if (strncmp(key, "center_range_slc:", 17) == 0)
	  vp->center_range_slc = atof(value);
	else if (strncmp(key, "far_range_slc:", 14) == 0)
	  vp->far_range_slc = atof(value);
	else if (strncmp(key, "first_slant_range_polynomial:", 29) == 0)
	  sscanf(value, "%lf %lf %lf %lf %lf %lf",
		 &vp->first_slant_range_polynomial[0],
		 &vp->first_slant_range_polynomial[1],
		 &vp->first_slant_range_polynomial[2],
		 &vp->first_slant_range_polynomial[3],
		 &vp->first_slant_range_polynomial[4],
		 &vp->first_slant_range_polynomial[5]);
	else if (strncmp(key, "center_slant_range_polynomial:", 30) == 0)
	  sscanf(value, "%lf %lf %lf %lf %lf %lf",
		 &vp->center_slant_range_polynomial[0],
		 &vp->center_slant_range_polynomial[1],
		 &vp->center_slant_range_polynomial[2],
		 &vp->center_slant_range_polynomial[3],
		 &vp->center_slant_range_polynomial[4],
		 &vp->center_slant_range_polynomial[5]);
	else if (strncmp(key, "last_slant_range_polynomial:", 28) == 0)
	  sscanf(value, "%lf %lf %lf %lf %lf %lf",
		 &vp->last_slant_range_polynomial[0],
		 &vp->last_slant_range_polynomial[1],
		 &vp->last_slant_range_polynomial[2],
		 &vp->last_slant_range_polynomial[3],
		 &vp->last_slant_range_polynomial[4],
		 &vp->last_slant_range_polynomial[5]);
	else if (strncmp(key, "incidence_angle:", 16) == 0)
	  vp->incidence_angle = atof(value);
	else if (strncmp(key, "azimuth_deskew:", 15) == 0) {
	  str = trim_spaces(value);
	  if (strcmp(str, "ON") == 0)
	    vp->azimuth_deskew = 1;
	  else
	    vp->azimuth_deskew = 0;
	}
	else if (strncmp(key, "azimuth_angle:", 14) == 0)
	  vp->azimuth_angle = atof(value);
	else if (strncmp(key, "radar_frequency:", 16) == 0)
	  vp->radar_frequency = atof(value);
	else if (strncmp(key, "adc_sampling_rate:", 18) == 0)
	  vp->adc_sampling_rate = atof(value);
	else if (strncmp(key, "chirp_bandwidth:", 16) == 0)
	  vp->chirp_bandwidth = atof(value);
	else if (strncmp(key, "prf:", 4) == 0)
	  vp->prf = atof(value);
	else if (strncmp(key, "azimuth_proc_bandwidth:", 23) == 0)
	  vp->azimuth_proc_bandwidth = atof(value);
	else if (strncmp(key, "doppler_polynomial:", 19) == 0)
	  sscanf(value, "%lf %lf %lf %lf",
		 &vp->doppler_polynomial[0],
		 &vp->doppler_polynomial[1],
		 &vp->doppler_polynomial[2],
		 &vp->doppler_polynomial[3]);
	else if (strncmp(key, "doppler_poly_dot:", 17) == 0)
	  sscanf(value, "%lf %lf %lf %lf",
		 &vp->doppler_poly_dot[0],
		 &vp->doppler_poly_dot[1],
		 &vp->doppler_poly_dot[2],
		 &vp->doppler_poly_dot[3]);
	else if (strncmp(key, "doppler_poly_ddot:", 18) == 0)
	  sscanf(value, "%lf %lf %lf %lf",
		 &vp->doppler_poly_ddot[0],
		 &vp->doppler_poly_ddot[1],
		 &vp->doppler_poly_ddot[2],
		 &vp->doppler_poly_ddot[3]);
	else if (strncmp(key, "receiver_gain:", 14) == 0)
	  vp->receiver_gain = atof(value);
	else if (strncmp(key, "calibration_gain:", 17) == 0)
	  vp->calibration_gain = atof(value);
	else if (strncmp(key, "sar_to_earth_center:", 20) == 0)
	  vp->sar_to_earth_center = atof(value);
	else if (strncmp(key, "earth_radius_below_sensor:", 26) == 0)
	  vp->earth_radius_below_sensor = atof(value);
	else if (strncmp(key, "earth_semi_major_axis:", 22) == 0)
	  vp->earth_semi_major_axis = atof(value);
	else if (strncmp(key, "earth_semi_minor_axis:", 22) == 0)
	  vp->earth_semi_minor_axis = atof(value);
	else if (strncmp(key, "number_of_state_vectors:", 24) == 0) {
	  vp->number_of_state_vectors = atoi(value);
	  vp->stVec = meta_state_vectors_init(vectors);
	  fscanf(fp, "time_of_first_state_vector: %lf   s\n", &vp->stVec->second);
	  fscanf(fp, "state_vector_interval: %lf   s\n", &interval);
	  vp->stVec->year = vp->acquisition.year;
	  ymd.year = vp->acquisition.year;
	  ymd.month = vp->acquisition.month;
	  ymd.day = vp->acquisition.day;
	  date_ymd2jd(&ymd, &jd);
	  vp->stVec->julDay = jd.jd;
	  for (ii=0; ii<vp->stVec->vector_count; ii++) {
	    vp->stVec->vecs[ii].time = vp->stVec->second + ii*interval;
	    fgets(line, 1024, fp);
	    sscanf(line, "%s:%s", key, value);
	    sscanf(value, "%lf %lf %lf",
		   &vp->stVec->vecs[ii].vec.pos.x,
		   &vp->stVec->vecs[ii].vec.pos.y,
		   &vp->stVec->vecs[ii].vec.pos.z);
	    fgets(line, 1024, fp);
	    sscanf(line, "%s:%s", key, value);
	    sscanf(value, "%lf %lf %lf",
		   &vp->stVec->vecs[ii].vec.vel.x,
		   &vp->stVec->vecs[ii].vec.vel.y,
		   &vp->stVec->vecs[ii].vec.vel.z);
	  }
	}
      }
    }
  }
  FCLOSE(fp);

  // Generate metadata
  sprintf(outFile, "%s.meta", outBaseName);
  meta = vp2meta(vp);
  meta_write(meta, outFile);

  // Copy generic binary file
  sprintf(inFile, "%s.slc", inBaseName);
  sprintf(outFile, "%s.img", outBaseName);
  //fileCopy(inFile, outFile);

  // Cleanup
  if(vp)
    FREE(vp);
}

vexcel_plain *vp_init()
{
  vexcel_plain *v = (vexcel_plain *) MALLOC(sizeof(vexcel_plain));

  // sensor block
  strcpy(v->sensor.sensor_name, MAGIC_UNSET_STRING);
  strcpy(v->sensor.instrument_name, MAGIC_UNSET_STRING);
  strcpy(v->sensor.instrument_type, MAGIC_UNSET_STRING);
  v->sensor.clock_angle = MAGIC_UNSET_DOUBLE;
  v->sensor.nr_temperatures = MAGIC_UNSET_INT;
  v->sensor.nr_beams = MAGIC_UNSET_INT;
  v->sensor.beam = NULL;
  v->sensor.ephemeris.sv_block.nr_sv = MAGIC_UNSET_INT;
  v->sensor.ephemeris.sv_block.state_vector = NULL;
  v->sensor.ephemeris.attitude.yaw = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.roll = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.pitch = MAGIC_UNSET_INT;
  strcpy(v->sensor.ephemeris.attitude.date, MAGIC_UNSET_STRING);
  v->sensor.ephemeris.attitude.yawpoly.reference = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.attitude.yawpoly.number_of_coefficients 
    = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.yawpoly.coefficients = NULL;
  v->sensor.ephemeris.attitude.rollpoly.reference = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.attitude.rollpoly.number_of_coefficients
    = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.rollpoly.coefficients = NULL;
  v->sensor.ephemeris.attitude.pitchpoly.reference = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.attitude.pitchpoly.number_of_coefficients
    = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.pitchpoly.coefficients = NULL;
  v->sensor.ephemeris.orbit_nr = MAGIC_UNSET_INT;
  strcpy(v->sensor.ephemeris.orbit_nr_date, MAGIC_UNSET_STRING);
  v->sensor.ephemeris.gha.angle = MAGIC_UNSET_DOUBLE;
  strcpy(v->sensor.ephemeris.gha.date, MAGIC_UNSET_STRING);
  strcpy(v->sensor.ephemeris.type, MAGIC_UNSET_STRING);

  strcpy(v->flight_path_direction, MAGIC_UNSET_STRING);

  // raw SAR image block
  strcpy(v->raw_sar_image.image_desc.facility, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.image_desc.format, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.image_desc.type, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.bytes_per_pixel = MAGIC_UNSET_INT;
  strcpy(v->raw_sar_image.image_desc.title, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.pixel_resolution = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.line_spacing = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.line_resolution = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.nr_pixels = MAGIC_UNSET_INT;
  v->raw_sar_image.image_desc.nr_lines = MAGIC_UNSET_INT;
  v->raw_sar_image.image_desc.min_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.max_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.mean_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.sigma_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.mean_intensity_value = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.sigma_intensity_value = MAGIC_UNSET_DOUBLE;
  strcpy(v->raw_sar_image.image_desc.coord.earth_model.name, 
	 MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.image_desc.coord.earth_model.ellipsoid_name,
         MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.coord.earth_model.major = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.minor = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.terrain_height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.mass = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.delta_x = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.delta_y = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.delta_z = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.g = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.j2 = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.j3 = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.earth_model.j4 = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.first_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.last_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.center_line_center_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.center_line_center_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->raw_sar_image.image_desc.coord.center_line_center_pixel.height
    = MAGIC_UNSET_DOUBLE;
  strcpy(v->raw_sar_image.processor_name, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.processor_version, MAGIC_UNSET_STRING);
  strcpy(v->raw_sar_image.first_line, MAGIC_UNSET_STRING);
  v->raw_sar_image.first_line_txpol = MAGIC_UNSET_CHAR;
  v->raw_sar_image.time_per_line = MAGIC_UNSET_DOUBLE;

  // GLI product block
  strcpy(v->gli_product.image_desc.facility, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_desc.format, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_desc.type, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.bytes_per_pixel = MAGIC_UNSET_INT;
  strcpy(v->gli_product.image_desc.title, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.pixel_resolution = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.line_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.line_resolution = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.nr_pixels = MAGIC_UNSET_INT;
  v->gli_product.image_desc.nr_lines = MAGIC_UNSET_INT;
  v->gli_product.image_desc.min_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.max_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.mean_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.sigma_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.mean_intensity_value = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.sigma_intensity_value = MAGIC_UNSET_DOUBLE;
  strcpy(v->gli_product.image_desc.coord.earth_model.name, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_desc.coord.earth_model.ellipsoid_name, 
	 MAGIC_UNSET_STRING);
  v->gli_product.image_desc.coord.earth_model.major = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.minor = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.terrain_height 
  = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.mass = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.delta_x = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.delta_y = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.delta_z = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.g = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.j2 = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.j3 = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.earth_model.j4 = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_first_pixel.lat 
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.first_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_first_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_first_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_first_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_last_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_last_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.last_line_last_pixel.height
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.center_line_center_pixel.lat
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.center_line_center_pixel.lon
    = MAGIC_UNSET_DOUBLE;
  v->gli_product.image_desc.coord.center_line_center_pixel.height
    = MAGIC_UNSET_DOUBLE;
  strcpy(v->gli_product.processor_name, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.processor_version, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.image_type, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.polarization, MAGIC_UNSET_STRING);
  strcpy(v->gli_product.first_line, MAGIC_UNSET_STRING);
  v->gli_product.time_per_line = MAGIC_UNSET_DOUBLE;
  v->gli_product.orbit_nr = MAGIC_UNSET_DOUBLE;
  strcpy(v->gli_product.orbit_nr_date, MAGIC_UNSET_STRING);
  v->gli_product.track_angle = MAGIC_UNSET_DOUBLE;
  v->gli_product.near_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.center_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.far_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.skew_flag = MAGIC_UNSET_INT;
  v->gli_product.kaiser_range = MAGIC_UNSET_DOUBLE;
  v->gli_product.kaiser_azimuth = MAGIC_UNSET_DOUBLE;
  v->gli_product.range_looks = MAGIC_UNSET_INT;
  v->gli_product.azimuth_looks = MAGIC_UNSET_INT;
  v->gli_product.range_block_average_factor = MAGIC_UNSET_INT;
  v->gli_product.azimuth_block_average_factor = MAGIC_UNSET_INT;
  v->gli_product.calibration_mode = MAGIC_UNSET_INT;
  v->gli_product.scan_id = MAGIC_UNSET_INT;
  v->gli_product.gr2sr_block.nr_gr2sr = MAGIC_UNSET_INT;
  v->gli_product.gr2sr_block.gr2sr = NULL;
  v->gli_product.range_FFT_size = MAGIC_UNSET_INT;
  v->gli_product.azimuth_offset = MAGIC_UNSET_DOUBLE;
  v->gli_product.processor_bandwidth = MAGIC_UNSET_DOUBLE;
  v->gli_product.kaiser_multilook = MAGIC_UNSET_DOUBLE;
  v->gli_product.bandwidth_per_look = MAGIC_UNSET_DOUBLE;
  v->gli_product.multi_look_overlap = MAGIC_UNSET_DOUBLE;
  v->gli_product.output_range_pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.output_azimuth_pixel_spacing = MAGIC_UNSET_DOUBLE;
  v->gli_product.eff_azimuth_looks = MAGIC_UNSET_DOUBLE;
  v->gli_product.total_multilook_bw = MAGIC_UNSET_DOUBLE;
  v->gli_product.raw_start_line = MAGIC_UNSET_INT;
  v->gli_product.nr_raw_lines = MAGIC_UNSET_INT;
  v->gli_product.raw_start_pixel = MAGIC_UNSET_INT;
  v->gli_product.nr_raw_pixels = MAGIC_UNSET_INT;

  return v;
*/
}

