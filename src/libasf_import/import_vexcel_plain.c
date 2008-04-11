#include <ctype.h>
#include "vexcel_plain.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "asf_nan.h"

vexcel_plain *vp_init();

char *getString(char **lines, char *want, int n)
{
  int i, structDepth;
  int indexBeam=0, indexPolarization=0, indexStVec=0, indexGr2Sr=0;
  char structName[16][255];

  // Initialize structure stack
  structDepth = 0;
  strcpy(structName[structDepth],"");

  // Fill in structure
  for (i=0; i<n; i++) {
    if (strchr(lines[i], '{') != NULL) {
      char newStruct[255], index[255];
      sscanf(lines[i], "%s", newStruct);
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
    else if (strchr(lines[i], '}') != NULL)
      structDepth--;
    else if (strlen(lines[i]) <= 1)
      continue;
    else {
      char got[255], name[255];
      sscanf(lines[i], "%s", name);
      strcpy(got, structName[structDepth]);
      if (structDepth != 0)
    strcat(got, ".");
      strcat(got, name);
      if (strncmp(got, want, strlen(want)) == 0) {
    char *s;
    char *str = (char *) MALLOC(sizeof(char)*255);
    s = strchr(lines[i], ':');
    s++;
    while (isspace(*s))
      s++;
    strcpy(str, s);
    return str;
      }
    }
  }
  asfPrintWarning("getStr: %s could not be found!\n", want);
  return MAGIC_UNSET_STRING;
}

char *getStr(char **lines, char *want, int n)
{
  char *str = trim_spaces(getString(lines, want, n));
  return str;
}

double getDouble(char **lines, char *want, int n)
{
  double value;
  char *str = getStr(lines, want, n);
  if (str) {
    value = atof(str);
    FREE(str);
    return value;
  }
  else {
    asfPrintWarning("getDouble: %s could not be found!\n", want);
    return MAGIC_UNSET_DOUBLE;
  }
}

int getInt(char **lines, char *want, int n)
{
  int value;
  char *str = getStr(lines, want, n);
  if (str) {
    value = atoi(str);
    FREE(str);
    return value;
  }
  else {
    asfPrintWarning("getInt: %s could not be found!\n", want);
    return MAGIC_UNSET_INT;
  }
}

void getXYZ(char **lines, char *want, int n,
            double *lat, double *lon, double *height)
{
  char *str = getString(lines, want, n);
  sscanf(str, "%lf %lf %lf", lat, lon, height);
  FREE(str);
}

void import_vexcel_plain(const char *inBaseName, const char *outBaseName)
{
  vexcel_plain *vp;
  vp_polarization polarization;
  vp_doppler_centroid_parameters doppler_params;
  vp_doppler_rate_parameters doppler_rate;
  vp_coefficients velocity, doppler_centroid;
  vp_state_vector stVec;
  vp_attitude attitude;
  vp_gr2sr gr2sr;
  meta_parameters *meta=NULL;
  FILE *fp;
  char inFile[255], outFile[255], line[1024], tmp[255];
  char **lines;
  int i, k, l, n=0, nPol, nFirst, nSecond, nStVec, nGr2Sr;
  //int m;
  double lat, lon, height;

  // Initialize the vexcel plain struct
  vp = vp_init();

  // Read vexcel plain parameter file
  sprintf(inFile, "%s.par", inBaseName);

  // Determine how many lines are in the file
  fp = FOPEN(inFile, "r");
  while (fgets(line, 255, fp))
    n++;
  FCLOSE(fp);

  // Assign memory and read lines in
  lines = (char **) MALLOC(sizeof(char *)*n);
  for (i=0; i<n; i++)
    lines[i] = (char *) MALLOC(sizeof(char)*255);
  fp = FOPEN(inFile, "r");
  for (i=0; i<n; i++)
    fgets(lines[i], 255, fp);
  FCLOSE(fp);

  // sensor block
  vp->sensor.sensor_name = getStr(lines, "sensor.sensor_name:", n);
  vp->sensor.instrument_name = getStr(lines, "sensor.instrument_name:", n);
  vp->sensor.instrument_type = getStr(lines, "sensor.instrument_type:", n);
  vp->sensor.clock_angle = getDouble(lines, "sensor.clock_angle:", n);
  vp->sensor.nr_temperatures = getInt(lines, "sensor.nr_temperatures:", n);
  // FIXME: no idea what a temperature block looks like
  vp->sensor.nr_beams = getInt(lines, "sensor.nr_beams:", n);
  if (vp->sensor.nr_beams > 0) {
    vp->sensor.beam = (vp_beam *) MALLOC(sizeof(vp_beam)*vp->sensor.nr_beams);
    for (i=0; i<vp->sensor.nr_beams; i++) {
      // beam
      sprintf(tmp, "sensor.beam[%d].beam_name:", i);
      vp->sensor.beam[i].beam_name = getStr(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].nr_of_samples:", i);
      vp->sensor.beam[i].nr_of_samples = getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].echo_delay:", i);
      vp->sensor.beam[i].echo_delay = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].carrier_freq:", i);
      vp->sensor.beam[i].carrier_freq = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].sampling_freq:", i);
      vp->sensor.beam[i].sampling_freq = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].PRF:", i);
      vp->sensor.beam[i].prf = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].chirp_rate:", i);
      vp->sensor.beam[i].chirp_rate = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].pulse_length:", i);
      vp->sensor.beam[i].pulse_length = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].look_angle:", i);
      vp->sensor.beam[i].look_angle = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].incidence_angle:", i);
      vp->sensor.beam[i].incidence_angle = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].range_spectrum_snr:", i);
      vp->sensor.beam[i].range_spectrum_snr = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].replica_energy_ref_level:", i);
      vp->sensor.beam[i].replica_energy_ref_level = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].cal1_cal2_diff_ref_level:", i);
      vp->sensor.beam[i].cal1_cal2_diff_ref_level = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].thermal_noise_ref_level:", i);
      vp->sensor.beam[i].thermal_noise_ref_level = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].gain_corctn_factor:", i);
      vp->sensor.beam[i].gain_corctn_factor = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].gain_scale:", i);
      vp->sensor.beam[i].gain_scale = getDouble(lines, tmp, n);

      // Polarization block
      sprintf(tmp, "sensor.beam[%d].PolarizationBlock.NrPolarizations:", i);
      vp->sensor.beam[i].polarization_block.nr_polarizations = nPol =
    getInt(lines, tmp, n);
      vp->sensor.beam[i].polarization_block.polarization =
    (vp_polarization *) MALLOC(sizeof(vp_polarization)*nPol);
      for (k=0; k<nPol; k++) {
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].polarization:", i, k);
    polarization.polarization = getStr(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].polarization_amplitude:", i, k);
    polarization.polarization_amplitude = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].polarization_phase:", i, k);
    polarization.polarization_phase = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].stc_pattern_id:", i, k);
    polarization.stc_pattern_id = getInt(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].IQStatistics.I_mean:", i, k);
    polarization.iq_statistics.I_mean= getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].IQStatistics.Q_mean:", i, k);
    polarization.iq_statistics.Q_mean = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].IQStatistics.I_std:", i, k);
    polarization.iq_statistics.I_std = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
        "Polarization[%d].IQStatistics.Q_std:", i, k);
    polarization.iq_statistics.Q_std = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.beam[%d].PolarizationBlock."
          "Polarization[%d].IQStatistics.IQ_corr:", i, k);
    polarization.iq_statistics.IQ_corr= getDouble(lines, tmp, n);
    vp->sensor.beam[i].polarization_block.polarization[k] =
      polarization;
      }

      // Doppler centroid parameters
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_centroid_coefficients.reference_first_dimension:", i);
      doppler_centroid.reference_first_dimension = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_centroid_coefficients."
          "reference_second_dimension:", i);
      doppler_centroid.reference_second_dimension = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_centroid_coefficients."
          "number_of_coefficients_first_dimension:", i);
      doppler_centroid.number_of_coefficients_first_dimension =
    nFirst = getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_centroid_coefficients."
          "number_of_coefficients_second_dimension:", i);
      doppler_centroid.number_of_coefficients_second_dimension =
    nSecond = getInt(lines, tmp, n);

      doppler_centroid.a = (double *) MALLOC(sizeof(double)*nFirst*nSecond);
      for (k=0; k<nSecond; k++) {
    for (l=0; l<nFirst; l++) {
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_centroid_coefficients.a%d%d:", i, k, l);
      doppler_centroid.a[k*nSecond+l] = getDouble(lines, tmp, n);
    }
      }
      doppler_params.doppler_centroid_coefficients = doppler_centroid;
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "reference_range:", i);
      doppler_params.reference_range = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "reference_date:", i);
      doppler_params.reference_date = getStr(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "ambiguity_number:", i);
      doppler_params.ambiguity_number = getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "MLCC_ambiguity_number_occurence:", i);
      doppler_params.MLCC_ambiguity_number_occurence = getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "MLBF_ambiguity_number_occurence:", i);
      doppler_params.MLBF_ambiguity_number_occurence = getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "DAR_doppler:", i);
      doppler_params.DAR_doppler = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "Predict_doppler:", i);
      doppler_params.Predict_doppler = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "DAR_confidence:", i);
      doppler_params.DAR_confidence = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_fit_correlation:", i);
      doppler_params.doppler_fit_correlation = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerCentroidParameters."
          "doppler_status:", i);
      doppler_params.doppler_status = getStr(lines, tmp, n);
      vp->sensor.beam[i].doppler_centroid_parameters  = doppler_params;

      // Doppler rate parameters
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "effective_velocity_coefficients."
          "reference_first_dimension:", i);
      velocity.reference_first_dimension = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "effective_velocity_coefficients."
          "reference_second_dimension:", i);
      velocity.reference_second_dimension = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "effective_velocity_coefficients."
          "number_of_coefficients_first_dimension:", i);
      velocity.number_of_coefficients_first_dimension =
    nFirst = getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "effective_velocity_coefficients."
          "number_of_coefficients_second_dimension:", i);
      velocity.number_of_coefficients_second_dimension =
    nSecond = getInt(lines, tmp, n);
      velocity.a = (double *) MALLOC(sizeof(double)*nFirst*nSecond);
      for (k=0; k<nSecond; k++) {
    for (l=0; l<nFirst; l++) {
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "effective_velocity_coefficients.a%d%d", i, k, l);
      velocity.a[k*nSecond+l] = getDouble(lines, tmp, n);
    }
      }
      doppler_rate.effective_velocity_coefficients = velocity;
      vp->sensor.beam[i].doppler_rate_parameters = doppler_rate;
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters.veff:", i);
      doppler_rate.veff = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "reference_range:", i);
      doppler_rate.reference_range = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "reference_date:", i);
      doppler_rate.reference_date = getStr(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "autofocus_scale_factor:", i);
      doppler_rate.autofocus_scale_factor = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "autofocus_snr:", i);
      doppler_rate.autofocus_snr = getDouble(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "autofocus_suggested_ambiguity_number:", i);
      doppler_rate.autofocus_suggested_ambiguity_number =
    getInt(lines, tmp, n);
      sprintf(tmp, "sensor.beam[%d].DopplerRateParameters."
          "autofocus_status:", i);
      doppler_rate.autofocus_status = getStr(lines, tmp, n);
      vp->sensor.beam[i].doppler_rate_parameters  = doppler_rate;
    }
  }

  // ephemeris
  vp->sensor.ephemeris.sv_block.nr_sv = nStVec =
    getInt(lines, "sensor.ephemeris.sv_block.NrSV:", n);
  vp->sensor.ephemeris.sv_block.state_vector =
    (vp_state_vector *) MALLOC(sizeof(vp_state_vector) * nStVec);
  for (i=0; i<nStVec; i++) {
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].x:", i);
    stVec.x = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].y:", i);
    stVec.y = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].z:", i);
    stVec.z = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].xv:", i);
    stVec.xv = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].yv:", i);
    stVec.yv = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].zv:", i);
    stVec.zv = getDouble(lines, tmp, n);
    sprintf(tmp, "sensor.ephemeris.sv_block.state_vector[%d].Date:", i);
    stVec.date = getStr(lines, tmp, n);
    vp->sensor.ephemeris.sv_block.state_vector[i] = stVec;
  }
  attitude.yaw = getDouble(lines, "sensor.ephemeris.Attitude.yaw:", n);
  attitude.roll = getDouble(lines, "sensor.ephemeris.Attitude.roll:", n);
  attitude.pitch = getDouble(lines, "sensor.ephemeris.Attitude.pitch:", n);
  attitude.date = getStr(lines, "sensor.ephemeris.Attitude.Date:", n);
  attitude.yawpoly.reference =
    getDouble(lines, "sensor.ephemeris.Attitude.yawpoly.reference:", n);
  attitude.yawpoly.number_of_coefficients = nPol =
    getInt(lines,
       "sensor.ephemeris.Attitude.yawpoly.number_of_coefficients:", n);
  attitude.yawpoly.a = (double *) MALLOC(sizeof(double) * nPol);
  for (i=0; i<nPol; i++) {
    sprintf(tmp, "sensor.ephemeris.Attitude.yawpoly.a%d:", i);
    attitude.yawpoly.a[i] = getDouble(lines, tmp, n);
  }
  attitude.rollpoly.reference =
    getDouble(lines, "sensor.ephemeris.Attitude.rollpoly.reference:", n);
  attitude.rollpoly.number_of_coefficients = nPol =
    getInt(lines,
       "sensor.ephemeris.Attitude.rollpoly.number_of_coefficients:", n);
  attitude.rollpoly.a = (double *) MALLOC(sizeof(double) * nPol);
  for (i=0; i<nPol; i++) {
    sprintf(tmp, "sensor.ephemeris.Attitude.rollpoly.a%d:", i);
    attitude.rollpoly.a[i] = getDouble(lines, tmp, n);
  }
  attitude.pitchpoly.reference =
    getDouble(lines, "sensor.ephemeris.Attitude.pitchpoly.reference:", n);
  attitude.pitchpoly.number_of_coefficients = nPol =
    getInt(lines,
       "sensor.ephemeris.Attitude.pitchpoly.number_of_coefficients:", n);
  attitude.pitchpoly.a = (double *) MALLOC(sizeof(double) * nPol);
  for (i=0; i<nPol; i++) {
    sprintf(tmp, "sensor.ephemeris.Attitude.pitchpoly.a%d:", i);
    attitude.pitchpoly.a[i] = getDouble(lines, tmp, n);
  }
  vp->sensor.ephemeris.attitude = attitude;
  vp->sensor.ephemeris.orbit_nr =
    getInt(lines, "sensor.ephemeris.OrbitNr:", n);
  vp->sensor.ephemeris.orbit_nr_date =
    getStr(lines, "sensor.ephemeris.OrbitNr_Date:", n);
  vp->sensor.ephemeris.gha.angle =
    getDouble(lines, "sensor.ephemeris.GHA.angle:", n);
  vp->sensor.ephemeris.gha.date =
    getStr(lines, "sensor.ephemeris.GHA.date:", n);
  vp->sensor.ephemeris.type = getStr(lines, "sensor.ephemeris.Type:", n);

  vp->flight_path_direction = getStr(lines, "flight_path_direction:", n);

  // raw SAR image block
  vp->raw_sar_image.image_desc.facility =
    getStr(lines, "RawSARImage.image_desc.Facility:", n);
  vp->raw_sar_image.image_desc.format =
    getStr(lines, "RawSARImage.image_desc.Format:", n);
  vp->raw_sar_image.image_desc.type =
    getStr(lines, "RawSARImage.image_desc.Type:", n);
  vp->raw_sar_image.image_desc.bytes_per_pixel =
    getInt(lines, "RawSARImage.image_desc.BytesPerPixel:", n);
  vp->raw_sar_image.image_desc.title =
    getStr(lines, "RawSARImage.image_desc.Title:", n);
  vp->raw_sar_image.image_desc.pixel_spacing =
    getDouble(lines, "RawSARImage.image_desc.PixelSpacing:", n);
  vp->raw_sar_image.image_desc.pixel_resolution =
    getDouble(lines, "RawSARImage.image_desc.PixelResolution:", n);
  vp->raw_sar_image.image_desc.line_spacing =
    getDouble(lines, "RawSARImage.image_desc.LineSpacing:", n);
  vp->raw_sar_image.image_desc.line_resolution =
    getDouble(lines, "RawSARImage.image_desc.LineResolution:", n);
  vp->raw_sar_image.image_desc.nr_pixels =
    getInt(lines, "RawSARImage.image_desc.NrPixels:", n);
  vp->raw_sar_image.image_desc.nr_lines =
    getInt(lines, "RawSARImage.image_desc.NrLines:", n);
  vp->raw_sar_image.image_desc.min_value =
    getDouble(lines, "RawSARImage.image_desc.MinValue:", n);
  vp->raw_sar_image.image_desc.max_value =
    getDouble(lines, "RawSARImage.image_desc.MaxValue:", n);
  vp->raw_sar_image.image_desc.mean_value =
    getDouble(lines, "RawSARImage.image_desc.MeanValue:", n);
  vp->raw_sar_image.image_desc.sigma_value =
    getDouble(lines, "RawSARImage.image_desc.SigmaValue:", n);
  vp->raw_sar_image.image_desc.mean_intensity_value =
    getDouble(lines, "RawSARImage.image_desc.MeanIntensityValue:", n);
  vp->raw_sar_image.image_desc.sigma_intensity_value =
    getDouble(lines, "RawSARImage.image_desc.SigmaIntensityValue:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.name =
    getStr(lines, "RawSARImage.image_desc.coord.earth_model.name:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.ellipsoid_name =
    getStr(lines,
       "RawSARImage.image_desc.coord.earth_model.ellipsoid_name:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.major =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.major:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.minor =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.minor:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.terrain_height =
    getDouble(lines,
          "RawSARImage.image_desc.coord.earth_model.terrain_height:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.mass =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.mass:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.delta_x =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.delta_x:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.delta_y =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.delta_y:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.delta_z =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.delta_z:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.g =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.g:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.j2 =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.j2:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.j3 =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.j3:", n);
  vp->raw_sar_image.image_desc.coord.earth_model.j4 =
    getDouble(lines, "RawSARImage.image_desc.coord.earth_model.j4:", n);
  getXYZ(lines, "RawSARImage.image_desc.coord.first_line_first_pixel:", n,
     &lat, &lon, &height);
  vp->raw_sar_image.image_desc.coord.first_line_first_pixel.lat = lat;
  vp->raw_sar_image.image_desc.coord.first_line_first_pixel.lon = lon;
  vp->raw_sar_image.image_desc.coord.first_line_first_pixel.height = height;
  getXYZ(lines, "RawSARImage.image_desc.coord.first_line_last_pixel:", n,
     &lat, &lon, &height);
  vp->raw_sar_image.image_desc.coord.first_line_last_pixel.lat = lat;
  vp->raw_sar_image.image_desc.coord.first_line_last_pixel.lon = lon;
  vp->raw_sar_image.image_desc.coord.first_line_last_pixel.height = height;
  getXYZ(lines, "RawSARImage.image_desc.coord.last_line_first_pixel:", n,
     &lat, &lon, &height);
  vp->raw_sar_image.image_desc.coord.last_line_first_pixel.lat = lat;
  vp->raw_sar_image.image_desc.coord.last_line_first_pixel.lon = lon;
  vp->raw_sar_image.image_desc.coord.last_line_first_pixel.height = height;
  getXYZ(lines, "RawSARImage.image_desc.coord.last_line_last_pixel:", n,
     &lat, &lon, &height);
  vp->raw_sar_image.image_desc.coord.last_line_last_pixel.lat = lat;
  vp->raw_sar_image.image_desc.coord.last_line_last_pixel.lon = lon;
  vp->raw_sar_image.image_desc.coord.last_line_last_pixel.height = height;
  getXYZ(lines, "RawSARImage.image_desc.coord.center_line_center_pixel:", n,
     &lat, &lon, &height);
  vp->raw_sar_image.image_desc.coord.center_line_center_pixel.lat = lat;
  vp->raw_sar_image.image_desc.coord.center_line_center_pixel.lon = lon;
  vp->raw_sar_image.image_desc.coord.center_line_center_pixel.height = height;
  vp->raw_sar_image.processor_name =
    getStr(lines, "RawSARImage.processor_name:", n);
  vp->raw_sar_image.processor_version =
    getStr(lines, "RawSARImage.processor_version:", n);
  vp->raw_sar_image.first_line = getStr(lines, "RawSARImage.first_line:", n);
  vp->raw_sar_image.first_line_txpol =
    getStr(lines, "RawSARImage.first_line_txpol:", n);
  vp->raw_sar_image.time_per_line =
    getDouble(lines, "RawSARImage.time_per_line:", n);

  // GLI product block
  vp->gli_product.image_desc.facility =
    getStr(lines, "GLIProduct.image_desc.Facility:", n);
  vp->gli_product.image_desc.format =
    getStr(lines, "GLIProduct.image_desc.Format:", n);
  vp->gli_product.image_desc.type =
    getStr(lines, "GLIProduct.image_desc.Type:", n);
  vp->gli_product.image_desc.bytes_per_pixel =
    getInt(lines, "GLIProduct.image_desc.BytesPerPixel:", n);;
  vp->gli_product.image_desc.title =
    getStr(lines, "GLIProduct.image_desc.Title:", n);
  vp->gli_product.image_desc.pixel_spacing =
    getDouble(lines, "GLIProduct.image_desc.PixelSpacing:", n);
  vp->gli_product.image_desc.pixel_resolution =
    getDouble(lines, "GLIProduct.image_desc.PixelResolution:", n);
  vp->gli_product.image_desc.line_spacing =
    getDouble(lines, "GLIProduct.image_desc.LineSpacing:", n);
  vp->gli_product.image_desc.line_resolution =
    getDouble(lines, "GLIProduct.image_desc.LineResolution:", n);
  vp->gli_product.image_desc.nr_pixels =
    getInt(lines, "GLIProduct.image_desc.NrPixels:", n);
  vp->gli_product.image_desc.nr_lines =
    getInt(lines, "GLIProduct.image_desc.NrLines:", n);
  vp->gli_product.image_desc.min_value =
    getDouble(lines, "GLIProduct.image_desc.MinValue:", n);
  vp->gli_product.image_desc.max_value =
    getDouble(lines, "GLIProduct.image_desc.MaxValue:", n);
  vp->gli_product.image_desc.mean_value =
    getDouble(lines, "GLIProduct.image_desc.MeanValue:", n);
  vp->gli_product.image_desc.sigma_value =
    getDouble(lines, "GLIProduct.image_desc.SigmaValue:", n);
  vp->gli_product.image_desc.mean_intensity_value =
    getDouble(lines, "GLIProduct.image_desc.MeanIntensityValue:", n);
  vp->gli_product.image_desc.sigma_intensity_value =
    getDouble(lines, "GLIProduct.image_desc.SigmaIntensityValue:", n);
  vp->gli_product.image_desc.coord.earth_model.name =
    getStr(lines, "GLIProduct.image_desc.coord.earth_model.name:", n);
  vp->gli_product.image_desc.coord.earth_model.ellipsoid_name =
    getStr(lines,
       "GLIProduct.image_desc.coord.earth_model.ellipsoid_name:", n);
  vp->gli_product.image_desc.coord.earth_model.major =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.major:", n);
  vp->gli_product.image_desc.coord.earth_model.minor =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.minor:", n);
  vp->gli_product.image_desc.coord.earth_model.terrain_height =
    getDouble(lines,
          "GLIProduct.image_desc.coord.earth_model.terrain_height:", n);
  vp->gli_product.image_desc.coord.earth_model.mass =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.mass:", n);
  vp->gli_product.image_desc.coord.earth_model.delta_x =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.delta_x:", n);
  vp->gli_product.image_desc.coord.earth_model.delta_y =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.delta_y:", n);
  vp->gli_product.image_desc.coord.earth_model.delta_z =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.delta_z:", n);
  vp->gli_product.image_desc.coord.earth_model.g =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.g:", n);
  vp->gli_product.image_desc.coord.earth_model.j2 =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.j2:", n);
  vp->gli_product.image_desc.coord.earth_model.j3 =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.j3:", n);
  vp->gli_product.image_desc.coord.earth_model.j4 =
    getDouble(lines, "GLIProduct.image_desc.coord.earth_model.j4:", n);
  getXYZ(lines, "GLIProduct.image_desc.coord.first_line_first_pixel:", n,
     &lat, &lon, &height);
  vp->gli_product.image_desc.coord.first_line_first_pixel.lat = lat;
  vp->gli_product.image_desc.coord.first_line_first_pixel.lon = lon;
  vp->gli_product.image_desc.coord.first_line_first_pixel.height = height;
  getXYZ(lines, "GLIProduct.image_desc.coord.first_line_last_pixel:", n,
     &lat, &lon, &height);
  vp->gli_product.image_desc.coord.first_line_last_pixel.lat = lat;
  vp->gli_product.image_desc.coord.first_line_last_pixel.lon = lon;
  vp->gli_product.image_desc.coord.first_line_last_pixel.height = height;
  getXYZ(lines, "GLIProduct.image_desc.coord.last_line_first_pixel:", n,
     &lat, &lon, &height);
  vp->gli_product.image_desc.coord.last_line_first_pixel.lat = lat;
  vp->gli_product.image_desc.coord.last_line_first_pixel.lon = lon;
  vp->gli_product.image_desc.coord.last_line_first_pixel.height = height;
  getXYZ(lines, "GLIProduct.image_desc.coord.last_line_last_pixel:", n,
     &lat, &lon, &height);
  vp->gli_product.image_desc.coord.last_line_last_pixel.lat = lat;
  vp->gli_product.image_desc.coord.last_line_last_pixel.lon = lon;
  vp->gli_product.image_desc.coord.last_line_last_pixel.height = height;
  getXYZ(lines, "GLIProduct.image_desc.coord.center_line_center_pixel:", n,
     &lat, &lon, &height);
  vp->gli_product.image_desc.coord.center_line_center_pixel.lat = lat;
  vp->gli_product.image_desc.coord.center_line_center_pixel.lon = lon;
  vp->gli_product.image_desc.coord.center_line_center_pixel.height = height;
  vp->gli_product.processor_name =
    getStr(lines, "GLIProduct.processor_name:", n);
  vp->gli_product.processor_version =
    getStr(lines, "GLIProduct.processor_version:", n);
  vp->gli_product.image_type = getStr(lines, "GLIProduct.image_type:", n);
  vp->gli_product.polarization = getStr(lines, "GLIProduct.polarization:", n);
  vp->gli_product.first_line = getStr(lines, "GLIProduct.first_line:", n);
  vp->gli_product.time_per_line =
    getDouble(lines, "GLIProduct.time_per_line:", n);
  vp->gli_product.orbit_nr = getInt(lines, "GLIProduct.OrbitNr:", n);
  vp->gli_product.orbit_nr_date = getStr(lines, "GLIProduct.OrbitNr_Date:", n);
  vp->gli_product.track_angle = getDouble(lines, "GLIProduct.track_angle:", n);
  vp->gli_product.near_range = getDouble(lines, "GLIProduct.near_range:", n);
  vp->gli_product.center_range =
    getDouble(lines, "GLIProduct.center_range:", n);
  vp->gli_product.far_range = getDouble(lines, "GLIProduct.far_range:", n);
  vp->gli_product.skew_flag = getInt(lines, "GLIProduct.skew_flag:", n);
  vp->gli_product.kaiser_range =
    getDouble(lines, "GLIProduct.Kaiser_range:", n);
  vp->gli_product.kaiser_azimuth =
    getDouble(lines, "GLIProduct.Kaiser_azimuth:", n);
  vp->gli_product.range_looks = getInt(lines, "GLIProduct.range_looks:", n);
  vp->gli_product.azimuth_looks =
    getInt(lines, "GLIProduct.azimuth_looks:", n);
  vp->gli_product.range_block_average_factor =
    getInt(lines, "GLIProduct.range_block_average_factor:", n);
  vp->gli_product.azimuth_block_average_factor =
    getInt(lines, "GLIProduct.azimuth_block_average_factor:", n);
  vp->gli_product.calibration_mode =
    getInt(lines, "GLIProduct.calibration_mode:", n);
  vp->gli_product.scan_id = getInt(lines, "GLIProduct.scan_id:", n);
  vp->gli_product.gr2sr_block.nr_gr2sr = nGr2Sr =
    getInt(lines, "GLIProduct.Gr2Sr_Block.NrGr2Sr:", n);
  vp->gli_product.gr2sr_block.gr2sr =
    (vp_gr2sr *) MALLOC(sizeof(vp_gr2sr) * nGr2Sr);
  for (i=0; i<nGr2Sr; i++) {
    sprintf(tmp, "GLIProduct.Gr2Sr_Block.gr2sr[%d].reference_date:", i);
    gr2sr.reference_date = getStr(lines, tmp, n);
    sprintf(tmp, "GLIProduct.Gr2Sr_Block.gr2sr[%d].reference_range:", i);
    gr2sr.reference_range = getDouble(lines, tmp, n);
    sprintf(tmp,
        "GLIProduct.Gr2Sr_Block.gr2sr[%d].number_of_coefficients:", i);
    gr2sr.number_of_coefficients = nPol = getInt(lines, tmp, n);
    gr2sr.a = (double *) MALLOC(sizeof(double) * nPol);
    for (k=0; k<gr2sr.number_of_coefficients; k++) {
      sprintf(tmp, "GLIProduct.Gr2Sr_Block.gr2sr[%d].a%d:", i, k);
      gr2sr.a[k] = getDouble(lines, tmp, n);
    }
    vp->gli_product.gr2sr_block.gr2sr[i] = gr2sr;
  }
  vp->gli_product.range_FFT_size =
    getInt(lines, "GLIProduct.range_FFT_size:", n);
  vp->gli_product.azimuth_offset =
    getDouble(lines, "GLIProduct.azimuth_offset:", n);
  vp->gli_product.processor_bandwidth =
    getDouble(lines, "GLIProduct.processor_bandwidth:", n);
  vp->gli_product.kaiser_multilook =
    getDouble(lines, "GLIProduct.Kaiser_multilook:", n);
  vp->gli_product.bandwidth_per_look =
    getDouble(lines, "GLIProduct.bandwidth_per_look:", n);;
  vp->gli_product.multi_look_overlap =
    getDouble(lines, "GLIProduct.multi_look_overlap:", n);
  vp->gli_product.output_range_pixel_spacing =
    getDouble(lines, "GLIProduct.output_range_pixel_spacing:", n);
  vp->gli_product.output_azimuth_pixel_spacing =
    getDouble(lines, "GLIProduct.output_range_pixel_spacing:", n);
  vp->gli_product.eff_azimuth_looks =
    getDouble(lines, "GLIProduct.eff_azimuth_looks:", n);
  vp->gli_product.total_multilook_bw =
    getDouble(lines, "GLIProduct.total_multilook_bw:", n);
  vp->gli_product.raw_start_line =
    getInt(lines, "GLIProduct.raw_start_line:", n);
  vp->gli_product.nr_raw_lines =
    getInt(lines, "GLIProduct.nr_raw_lines:", n);
  vp->gli_product.raw_start_pixel =
    getInt(lines, "GLIProduct.raw_start_pixel:", n);
  vp->gli_product.nr_raw_pixels =
    getInt(lines, "GLIProduct.nr_raw_pixels:", n);

  // Generate metadata
  sprintf(outFile, "%s.meta", outBaseName);
  meta = vp2meta(vp);
  meta_write(meta, outFile);

  // Copy generic binary file
  sprintf(inFile, "%s.slc", inBaseName);
  sprintf(outFile, "%s.img", outBaseName);
  //fileCopy(inFile, outFile);

  // Cleanup
  for (i=0; i<n; i++)
    FREE(lines[i]);
  FREE(lines);
  meta_free(meta);
  if (vp)
    FREE(vp);
}

vexcel_plain *vp_init()
{
  vexcel_plain *v = (vexcel_plain *) MALLOC(sizeof(vexcel_plain));

  // sensor block
  v->sensor.sensor_name = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->sensor.sensor_name, MAGIC_UNSET_STRING);
  v->sensor.instrument_name = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->sensor.instrument_name, MAGIC_UNSET_STRING);
  v->sensor.instrument_type = (char *) MALLOC(sizeof(char)*255);
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
  v->sensor.ephemeris.attitude.date = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->sensor.ephemeris.attitude.date, MAGIC_UNSET_STRING);
  v->sensor.ephemeris.attitude.yawpoly.reference = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.attitude.yawpoly.number_of_coefficients
    = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.yawpoly.a = NULL;
  v->sensor.ephemeris.attitude.rollpoly.reference = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.attitude.rollpoly.number_of_coefficients
    = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.rollpoly.a = NULL;
  v->sensor.ephemeris.attitude.pitchpoly.reference = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.attitude.pitchpoly.number_of_coefficients
    = MAGIC_UNSET_INT;
  v->sensor.ephemeris.attitude.pitchpoly.a = NULL;
  v->sensor.ephemeris.orbit_nr = MAGIC_UNSET_INT;
  v->sensor.ephemeris.orbit_nr_date = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->sensor.ephemeris.orbit_nr_date, MAGIC_UNSET_STRING);
  v->sensor.ephemeris.gha.angle = MAGIC_UNSET_DOUBLE;
  v->sensor.ephemeris.gha.date = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->sensor.ephemeris.gha.date, MAGIC_UNSET_STRING);
  v->sensor.ephemeris.type = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->sensor.ephemeris.type, MAGIC_UNSET_STRING);

  v->flight_path_direction = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->flight_path_direction, MAGIC_UNSET_STRING);

  // raw SAR image block
  v->raw_sar_image.image_desc.facility = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.image_desc.facility, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.format = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.image_desc.format, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.type = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.image_desc.type, MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.bytes_per_pixel = MAGIC_UNSET_INT;
  v->raw_sar_image.image_desc.title = (char *) MALLOC(sizeof(char)*255);
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
  v->raw_sar_image.image_desc.coord.earth_model.name =
    (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.image_desc.coord.earth_model.name,
     MAGIC_UNSET_STRING);
  v->raw_sar_image.image_desc.coord.earth_model.ellipsoid_name =
    (char *) MALLOC(sizeof(char)*255);
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
  v->raw_sar_image.processor_name = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.processor_name, MAGIC_UNSET_STRING);
  v->raw_sar_image.processor_version = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.processor_version, MAGIC_UNSET_STRING);
  v->raw_sar_image.first_line = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.first_line, MAGIC_UNSET_STRING);
  v->raw_sar_image.first_line_txpol = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->raw_sar_image.first_line_txpol, MAGIC_UNSET_STRING);;
  v->raw_sar_image.time_per_line = MAGIC_UNSET_DOUBLE;

  // GLI product block
  v->gli_product.image_desc.facility = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.image_desc.facility, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.format = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.image_desc.format, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.type = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.image_desc.type, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.bytes_per_pixel = MAGIC_UNSET_INT;
  v->gli_product.image_desc.title = (char *) MALLOC(sizeof(char)*255);
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
  v->gli_product.image_desc.coord.earth_model.name =
    (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.image_desc.coord.earth_model.name, MAGIC_UNSET_STRING);
  v->gli_product.image_desc.coord.earth_model.ellipsoid_name =
    (char *) MALLOC(sizeof(char)*255);
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
  v->gli_product.processor_name = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.processor_name, MAGIC_UNSET_STRING);
  v->gli_product.processor_version = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.processor_version, MAGIC_UNSET_STRING);
  v->gli_product.image_type = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.image_type, MAGIC_UNSET_STRING);
  v->gli_product.polarization = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.polarization, MAGIC_UNSET_STRING);
  v->gli_product.first_line = (char *) MALLOC(sizeof(char)*255);
  strcpy(v->gli_product.first_line, MAGIC_UNSET_STRING);
  v->gli_product.time_per_line = MAGIC_UNSET_DOUBLE;
  v->gli_product.orbit_nr = MAGIC_UNSET_INT;
  v->gli_product.orbit_nr_date = (char *) MALLOC(sizeof(char)*255);
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
}

