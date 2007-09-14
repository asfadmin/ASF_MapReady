#include "airsar.h"
#include "asf_meta.h"
#include <ctype.h>

void ingest_insar_data(const char *inBaseName, meta_parameters *metaIn,
		       const char *outBaseName, meta_parameters *metaOut,
		       char band)
{
  FILE *fpIn, *fpOut;
  char *inFile, *outFile;
  int ii;
  float *floatBuf;

  // Generate metadata file
  metaIn->general->data_type = INTEGER16;
  metaOut->general->data_type = REAL32;
  inFile = (char *) MALLOC(sizeof(char)*255);
  outFile = (char *) MALLOC(sizeof(char)*255);
  floatBuf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);

  // Ingest the DEM
  asfPrintStatus("   Ingesting DEM ...\n");
  sprintf(inFile, "%s_%c.demi2", inBaseName, band);
  sprintf(outFile, "%s_%c_dem.img", outBaseName, band);
  fpIn = FOPEN(inFile, "rb");
  fpOut = FOPEN(outFile, "wb");
  for (ii=0; ii<metaIn->general->line_count; ii++) {
    get_float_line(fpIn, metaIn, ii, floatBuf);
    put_float_line(fpOut, metaOut, ii, floatBuf);
    asfLineMeter(ii, metaIn->general->line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  metaOut->general->image_data_type = DEM;
  meta_write(metaOut, outFile);
  
  // Ingest amplitude image
  asfPrintStatus("   Ingesting amplitude image ...\n");
  sprintf(inFile, "%s_%c.vvi2", inBaseName, band);
  sprintf(outFile, "%s_%c_vv.img", outBaseName, band);
  fpIn = FOPEN(inFile, "rb");
  fpOut = FOPEN(outFile, "wb");
  for (ii=0; ii<metaIn->general->line_count; ii++) {
    get_float_line(fpIn, metaIn, ii, floatBuf);
    put_float_line(fpOut, metaOut, ii, floatBuf);
    asfLineMeter(ii, metaIn->general->line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  metaOut->general->image_data_type = AMPLITUDE_IMAGE;
  meta_write(metaOut, outFile);
  
  // Ingest coherence image
  asfPrintStatus("   Ingesting coherence image ...\n");
  metaIn->general->data_type = BYTE;
  sprintf(inFile, "%s_%c.corgr", inBaseName, band);
  sprintf(outFile, "%s_%c_coh.img", outBaseName, band);
  fpIn = FOPEN(inFile, "rb");
  fpOut = FOPEN(outFile, "wb");
  for (ii=0; ii<metaIn->general->line_count; ii++) {
    get_float_line(fpIn, metaIn, ii, floatBuf);
    put_float_line(fpOut, metaOut, ii, floatBuf);
    asfLineMeter(ii, metaIn->general->line_count);
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  metaOut->general->image_data_type = COHERENCE_IMAGE;
  meta_write(metaOut, outFile);

  // Clean up  
  FREE(floatBuf);
  FREE(inFile);
  FREE(outFile);
}

void ingest_polsar_data(const char *inBaseName, const char *outBaseName, 
			meta_parameters *meta, char band, long offset)
{
  FILE *fpIn, *fpOut;
  char *inFile, *outFile;
  int ii, kk;
  char *byteBuf;
  float *power, *shh_amp, *shh_phase, *shv_amp, *shv_phase, *svh_amp;
  float *svh_phase, *svv_amp, *svv_phase;
  float total_power, ysca, scale;
  complexFloat cpx;

  // Generate metadata file
  meta->general->data_type = REAL32;
  meta->general->image_data_type = POLARIMETRIC_IMAGE;
  meta->general->band_count = 9;
  sprintf(meta->general->bands,
          "POWER,SHH_AMP,SHH_PHASE,SHV_AMP,SHV_PHASE,SVH_AMP,SVH_PHASE,"
          "SVV_AMP,SVV_PHASE");
  inFile = (char *) MALLOC(sizeof(char)*255);
  outFile = (char *) MALLOC(sizeof(char)*255);
  power = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  shh_amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  shh_phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  shv_amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  shv_phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  svh_amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  svh_phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  svv_amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  svv_phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
  byteBuf = (char *) MALLOC(sizeof(char)*10);
  scale = (float) meta->airsar->scale_factor;

  // Ingest polarimetric data
  sprintf(inFile, "%s_%c.datgr", inBaseName, band);
  if (!fileExists(inFile))
    sprintf(inFile, "%s_%c.dat", inBaseName, band);
  sprintf(outFile, "%s_%c.img", outBaseName, band);
  fpIn = FOPEN(inFile, "rb");
  fpOut = FOPEN(outFile, "wb");
  FSEEK(fpIn, offset, 1);
  for (ii=0; ii<meta->general->line_count; ii++) {
    for (kk=0; kk<meta->general->sample_count; kk++) {
      FREAD(byteBuf, sizeof(char), 10, fpIn);
      total_power = 
	scale * ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
      ysca = 2.0 * sqrt(total_power);
      power[kk] = total_power;
      cpx.real = (float)byteBuf[2] * ysca / 127.0;
      cpx.imag = (float)byteBuf[3] * ysca / 127.0;
      shh_amp[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      shh_phase[kk] = atan2(cpx.imag, cpx.real);
      cpx.real = (float)byteBuf[4] * ysca / 127.0;
      cpx.imag = (float)byteBuf[5] * ysca / 127.0;
      shv_amp[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      svh_phase[kk] = atan2(cpx.imag, cpx.real);
      cpx.real = (float)byteBuf[6] * ysca / 127.0;
      cpx.imag = (float)byteBuf[7] * ysca / 127.0;
      svh_amp[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      svh_phase[kk] = atan2(cpx.imag, cpx.real);
      cpx.real = (float)byteBuf[8] * ysca / 127.0;
      cpx.imag = (float)byteBuf[9] * ysca / 127.0;
      svv_amp[kk] = sqrt(cpx.real*cpx.real + cpx.imag*cpx.imag);
      svv_phase[kk] = atan2(cpx.imag, cpx.real);
    }
    put_band_float_line(fpOut, meta, 0, ii, power);
    put_band_float_line(fpOut, meta, 1, ii, shh_amp);
    put_band_float_line(fpOut, meta, 2, ii, shh_phase);
    put_band_float_line(fpOut, meta, 3, ii, shv_amp);
    put_band_float_line(fpOut, meta, 4, ii, shv_phase);
    put_band_float_line(fpOut, meta, 5, ii, svh_amp);
    put_band_float_line(fpOut, meta, 6, ii, svh_phase);
    put_band_float_line(fpOut, meta, 7, ii, svv_amp);
    put_band_float_line(fpOut, meta, 8, ii, svv_phase);
    asfLineMeter(ii, meta->general->line_count);    
  }
  FCLOSE(fpIn);
  FCLOSE(fpOut);
  meta_write(meta, outFile);

  // Clean up  
  FREE(power);
  FREE(shh_amp);
  FREE(shh_phase);
  FREE(shv_amp);
  FREE(shv_phase);
  FREE(svh_amp);
  FREE(svh_phase);
  FREE(svv_amp);
  FREE(svv_phase);
  FREE(inFile);
  FREE(outFile);
}

void import_airsar(const char *inBaseName, const char *outBaseName)
{
  FILE *fpIn;
  char metaFile[255];
  char line[256]="", *value, str[50]="", *p;
  airsar_general *general=NULL;
  airsar_parameters *params=NULL;
  meta_parameters *metaIn, *metaOut;

  if (strncmp(inBaseName, "ts", 2) == 0) {
    // Read specific parameter file
    params = (airsar_parameters *) MALLOC(sizeof(airsar_parameters));
    sprintf(metaFile, "hd%s.log", inBaseName+2);
    fpIn = FOPEN(metaFile, "r");
    
    while (NULL != fgets(line, 255, fpIn)) {
      
      if (strncmp(line, "NUMBER OF SAMPLES PER RECORD =", 30) == 0)
	sscanf(line, "NUMBER OF SAMPLES PER RECORD = %d", 
	       &params->sample_count);
      else if (strncmp(line, "NUMBER OF LINES IN IMAGE =", 26) == 0)
	sscanf(line, "NUMBER OF LINES IN IMAGE = %d", &params->line_count);
      else if (strncmp(line, "JPL AIRCRAFT SAR PROCESSOR VERSION", 34) == 0) {
	sscanf(line, "JPL AIRCRAFT SAR PROCESSOR VERSION %s", str);
	value = trim_spaces(str);
	sprintf(params->processor, "%s", value);
      }
      else if (strncmp(line, "DATA TYPE =", 11) == 0) {
	sscanf(line, "DATA TYPE = %s", str);
	value = trim_spaces(str);
	if (strcmp(value, "INTEGER*2") == 0)
	  params->data_type = INTEGER16;
      }
      else if (strncmp(line, "RANGE PROJECTION =", 18) == 0) {
	sscanf(line, "RANGE PROJECTION = %s", str);
	value = trim_spaces(str);
	sprintf(params->range_projection, "%s", value);
      }
      else if (strncmp(line, "RANGE PIXEL SPACING (METERS) =", 30) == 0)
	sscanf(line, "RANGE PIXEL SPACING (METERS) = %lf", 
	       &params->x_pixel_size);
      else if (strncmp(line, "AZIMUTH PIXEL SPACING (METERS) =", 32) == 0)
	sscanf(line, "AZIMUTH PIXEL SPACING (METERS) = %lf",
	       &params->y_pixel_size);
      else if (strncmp(line, "BYTE OFFSET OF FIRST DATA RECORD =", 34) == 0)
	sscanf(line, "BYTE OFFSET OF FIRST DATA RECORD = %ld",
	       &params->first_data_offset);
      else if (strncmp(line, "BYTE OFFSET OF CALIBRATION HEADER =", 35) == 0)
	sscanf(line, "BYTE OFFSET OF CALIBRATION HEADER = %ld",
	       &params->calibration_header_offset);
      else if (strncmp(line, "BYTE OFFSET OF DEM HEADER =", 27) == 0)
	sscanf(line, "BYTE OFFSET OF DEM HEADER = %ld",
	       &params->dem_header_offset);
      else if (strncmp(line, "SITE NAME", 9) == 0) {
	sscanf(line, "SITE NAME %s", str);
	value = trim_spaces(str);
	sprintf(params->site_name, "%s", value);
      }
      else if (strncmp(line, "CCT TYPE", 8) == 0) {
	sscanf(line, "CCT TYPE %s", str);
	value = trim_spaces(str);
	sprintf(params->cct_type, "%s", value);
      }
      else if (strncmp(line, "CCT ID", 6) == 0)
	sscanf(line, "CCT ID %d", &params->cct_id);
      else if (strncmp(line, "LATITUDE AT START OF SCENE (DEGREES)", 36) == 0)
	sscanf(line, "LATITUDE AT START OF SCENE (DEGREES) %lf", 
	       &params->start_lat);
      else if (strncmp(line, "LONGITUDE AT START OF SCENE (DEGREES)", 37) == 0)
	sscanf(line, "LONGITUDE AT START OF SCENE (DEGREES) %lf",
	       &params->start_lon);
      else if (strncmp(line, "LATITUDE AT END OF SCENE (DEGREES)", 34) == 0)
	sscanf(line, "LATITUDE AT END OF SCENE (DEGREES) %lf", 
	       &params->end_lat);
      else if (strncmp(line, "LONGITUDE AT END OF SCENE (DEGREES)", 35) == 0)
	sscanf(line, "LONGITUDE AT END OF SCENE (DEGREES) %lf", 
	       &params->end_lon);
      else if (strncmp(line, "DATE OF ACQUISITION (GMT)", 25) == 0) {
	sscanf(line, "DATE OF ACQUISITION (GMT) %s", str);
	value = trim_spaces(str);
	sprintf(params->acquisition_date, "%s", value);  
      }
      else if (strncmp(line, "TIME OF ACQUISITION: SECONDS IN DAY", 35) == 0)
	sscanf(line, "TIME OF ACQUISITION: SECONDS IN DAY %lf", 
	       &params->acquisition_seconds);
      else if (strncmp(line, "FREQUENCIES COLLECTED", 21) == 0) {
	sscanf(line, "FREQUENCIES COLLECTED %s", str);
	value = trim_spaces(str);
	sprintf(params->frequencies, "%s", value);
      }
      else if (strncmp(line, "PRF AT START OF TRANSFER (HZ)", 29) == 0)
	sscanf(line, "PRF AT START OF TRANSFER (HZ) %lf", &params->prf);
      else if (strncmp(line, "SAMPLING RATE (MHZ)", 19) == 0)
	sscanf(line, "SAMPLING RATE (MHZ) %lf", &params->range_sampling_rate);
      else if (strncmp(line, "CHIRP BANDWIDTH (MHZ)", 21) == 0)
	sscanf(line, "CHIRP BANDWIDTH (MHZ) %lf", &params->chirp_bandwidth);
      else if (strncmp(line, "PULSE LENGTH (MICROSECONDS)", 27) == 0)
	sscanf(line, "PULSE LENGTH (MICROSECONDS) %lf", &params->pulse_length);
      else if (strncmp(line, "PROCESSOR WAVELENGTH (METERS)", 29) == 0)
	sscanf(line, "PROCESSOR WAVELENGTH (METERS) %lf", &params->wavelength);
      else if (strncmp(line, "NEAR SLANT RANGE (METERS)", 25) == 0)
	sscanf(line, "NEAR SLANT RANGE (METERS) %lf", 
	       &params->near_slant_range);
      else if (strncmp(line, "FAR SLANT RANGE (METERS)", 24) == 0)
	sscanf(line, "FAR SLANT RANGE (METERS) %lf", &params->far_slant_range);
      else if (strncmp(line, "NEAR LOOK ANGLE (DEGREES)", 25) == 0)
	sscanf(line, "NEAR LOOK ANGLE (DEGREES) %lf", 
	       &params->near_look_angle);
      else if (strncmp(line, "FAR LOOK ANGLE (DEGEES)", 23) == 0)
	sscanf(line, "FAR LOOK ANGLE (DEGEES) %lf", &params->far_look_angle);
      else if (strncmp(line, "NUMBER OF LOOKS PROCESSED IN AZIMUTH", 36) == 0)
	sscanf(line, "NUMBER OF LOOKS PROCESSED IN AZIMUTH %d", 
	       &params->azimuth_look_count);
      else if (strncmp(line, "NUMBER OF LOOKS PROCESSED IN RANGE", 34) == 0)
	sscanf(line, "NUMBER OF LOOKS PROCESSED IN RANGE %d",
	       &params->range_look_count);
      else if (strncmp
	       (line, "DESKEW FLAG (1=DESKEWED, 2=NOT DESKEWED)", 40) == 0)
	sscanf(line, "DESKEW FLAG (1=DESKEWED, 2=NOT DESKEWED) %d",
	       &params->deskewed);
      else if (strncmp(line, "SLANT RANGE SAMPLE SPACING (METERS)", 35) == 0)
	sscanf(line, "SLANT RANGE SAMPLE SPACING (METERS) %lf",
	       &params->sr_sample_spacing);
      else if (strncmp
	       (line, "NOMINAL SLANT RANGE RESOLUTION (METERS)", 39) == 0)
	sscanf(line, "NOMINAL SLANT RANGE RESOLUTION (METERS) %lf",
	       &params->slant_range_resolution);
      else if (strncmp(line, "AZIMUTH SAMPLE SPACING (METERS)", 31) == 0)
	sscanf(line, "AZIMUTH SAMPLE SPACING (METERS) %lf", 
	       &params->azimuth_sample_spacing);
      else if (strncmp(line, "NOMINAL AZIMUTH RESOLUTION (METERS)", 35) == 0)
	sscanf(line, "NOMINAL AZIMUTH RESOLUTION (METERS) %lf",
	       &params->azimuth_resolution);
      else if (strncmp(line, "IMAGE CENTER LATITUDE (DEGREES)", 31) == 0)
	sscanf(line, "IMAGE CENTER LATITUDE (DEGREES) %lf",
	       &params->center_lat);
      else if (strncmp(line, "IMAGE CENTER LONGITUDE (DEGREES)", 32) == 0)
	sscanf(line, "IMAGE CENTER LONGITUDE (DEGREES) %lf",
	       &params->center_lon);
      else if (strncmp(line, "GENERAL SCALE FACTOR", 20) == 0)
	sscanf(line, "GENERAL SCALE FACTOR %lf", &params->scale_factor);
      else if (strncmp(line, "GPS ALTITUDE, M", 15) == 0)
	sscanf(line, "GPS ALTITUDE, M %lf", &params->gps_altitude);
      else if (strncmp(line, "LATITUDE OF PEG POINT", 21) == 0)
	sscanf(line, "LATITUDE OF PEG POINT %lf", &params->lat_peg_point);
      else if (strncmp(line, "LONGITUDE OF PEG POINT", 22) == 0)
	sscanf(line, "LONGITUDE OF PEG POINT %lf", &params->lon_peg_point);
      else if (strncmp(line, "HEADING AT PEG POINT", 20) == 0)
	sscanf(line, "HEADING AT PEG POINT %lf", &params->head_peg_point);
      else if (strncmp(line, "ALONG-TRACK OFFSET S0  (M)", 26) == 0)
	sscanf(line, "ALONG-TRACK OFFSET S0  (M) %lf", 
	       &params->along_track_offset);
      else if (strncmp(line, "CROSS-TRACK OFFSET C0  (M)", 26) == 0)
	sscanf(line, "CROSS-TRACK OFFSET C0  (M) %lf", 
	       &params->cross_track_offset);
    }
    FCLOSE(fpIn);
  }

  // Read general metadata file
  general = (airsar_general *) MALLOC(sizeof(airsar_general));
  sprintf(metaFile, "%s_meta.airsar", inBaseName);
  fpIn = FOPEN(metaFile, "r");
  while (NULL != fgets(line, 255, fpIn)) {
    p = strchr(line, '=');
    if (p) {
      value = p+1;
      if (strncmp(line, "SCENE ID", 8) == 0)
	sprintf(general->scene_id, "%s", value);
      else if (strncmp(line, "Name of Flight-line", 19) == 0)
	sprintf(general->flight_line, "%s", value);
      else if (strncmp(line, "Date of Acquistion", 18) == 0)
	sprintf(general->date_acquisition, "%s", value);
      else if (strncmp(line, "Date of Processing", 18) == 0)
	sprintf(general->date_processing, "%s", value);
      else if (strncmp(line, "Radar Projection (for highest freq product)",
		       43) == 0)
	sprintf(general->radar_projection, "%s", value);
      else if (strncmp(line, "Width in Km(for highest freq product)", 37) == 0)
	general->width = atof(value);
      else if (strncmp
	       (line, "Length in Km(for highest freq product)", 38) == 0)
	general->length = atof(value);
      else if (strncmp(line, "Range pixel spacing", 19) == 0)
	general->range_pixel_spacing = atof(value);
      else if (strncmp(line, "Azimuth pixel spacing", 21) == 0)
	general->azimuth_pixel_spacing = atof(value);
      else if (strncmp(line, "Corner 1 latitude in degrees", 28) == 0)
	general->corner1_lat = atof(value);
      else if (strncmp(line, "Corner 1 longitude in degrees", 29) == 0)
	general->corner1_lon = atof(value);
      else if (strncmp(line, "Corner 2 latitude in degrees", 28) == 0)
	general->corner2_lat = atof(value);
      else if (strncmp(line, "Corner 2 longitude in degrees", 29) == 0)
	general->corner2_lon = atof(value);
      else if (strncmp(line, "Corner 3 latitude in degrees", 28) == 0)
	general->corner3_lat = atof(value);
      else if (strncmp(line, "Corner 3 longitude in degrees", 29) == 0)
	general->corner3_lon = atof(value);
      else if (strncmp(line, "Corner 4 latitude in degrees", 28) == 0)
	general->corner4_lat = atof(value);
      else if (strncmp(line, "Corner 4 longitude in degrees", 29) == 0)
	general->corner4_lon = atof(value);
      else if (strncmp(line, "C-band Radar Bandwidth in MHZ", 29) == 0)
	general->c_bandwidth = atof(value);
      else if (strncmp(line, "L-band Radar Bandwidth in MHZ", 29) == 0)
	general->l_bandwidth = atof(value);
      else if (strncmp(line, "P-band Radar Bandwidth in MHZ", 29) == 0)
	general->p_bandwidth = atof(value);
      else if (strncmp(line, "AIRSAR Mode", 11) == 0)
	sprintf(general->mode, "%s", value);
      else if (strncmp(line, "C-band Polarimetric data", 24) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->c_pol_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->c_pol_data = FALSE;
      }
      else if (strncmp(line, "L-band Polarimetric data", 24) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->l_pol_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->l_pol_data = FALSE;
      }
      else if (strncmp(line, "P-band Polarimetric data", 24) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->p_pol_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->p_pol_data = FALSE;
      }
      else if (strncmp
	       (line, "C-band Cross Track Interferometric data", 39) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->c_cross_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->c_cross_data = FALSE;
      }
      else if (strncmp
	       (line, "L-band Cross Track Interferometric data", 39) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->l_cross_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->l_cross_data = FALSE;  
      }
      else if (strncmp
	       (line, "C-band Along Track Interferometric data", 39) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->c_along_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->c_along_data = FALSE;
      }
      else if (strncmp
	       (line, "L-band Along Track Interferometric data", 39) == 0) {
	if (strncmp(value, "Yes", 3) == 0)
	  general->l_along_data = TRUE;
	else if (strncmp(value, "None", 4) == 0)
	  general->l_along_data = FALSE;
      }
      else if (strncmp(line, "Interferometry Baseline Length", 30) == 0)
	sprintf(general->baseline, "%s", value);
      else if (strncmp(line, "Single Frequency/Channel band 1", 31) == 0)
	sprintf(general->frequency_band1, "%s", value);
      else if (strncmp(line, "Single Frequency/Channel band 2", 31) == 0)
	sprintf(general->frequency_band2, "%s", value);
      else if (strncmp(line, "Single Frequency/Channel band 3", 31) == 0)
	sprintf(general->frequency_band3, "%s", value);
      else if (strncmp(line, "Data Format for band 1", 22) == 0)
	sprintf(general->format_band1, "%s", value);
      else if (strncmp(line, "Data Format for band 2", 22) == 0)
	sprintf(general->format_band2, "%s", value);
      else if (strncmp(line, "Data Format for band 3", 22) == 0)
	sprintf(general->format_band3, "%s", value);
    }
  }
  FCLOSE(fpIn);

  // Generate metadata
  metaIn = airsar2meta(general, params);
  metaOut = airsar2meta(general, params);

  // Check for interferometric data
  if (general->c_cross_data) {
    asfPrintStatus("\n   Ingesting C-band cross track interferometric data ..."
		   "\n\n");
    ingest_insar_data(inBaseName, metaIn, outBaseName, metaOut, 'c');
  }
  if (general->l_cross_data) {
    asfPrintStatus("\n   Ingesting L-band cross track interferometric data ..."
		   "\n\n");
    ingest_insar_data(inBaseName, metaIn, outBaseName, metaOut, 'l');
  }
  // Kept out the along-track interferometric data for the moment.
  // Only a few data sets were acquired that way and we have no real way
  // to verify the results.

  // Check for polarimetric data
  if (general->c_pol_data) {
    asfPrintStatus("\n   Ingesting C-band polarimetric data ...\n\n");
    ingest_polsar_data(inBaseName, outBaseName, metaOut, 'c',
		       params->calibration_header_offset*10);
  }
  if (general->l_pol_data) {
    asfPrintStatus("\n   Ingesting L-band polarimetric data ...\n\n");
    ingest_polsar_data(inBaseName, outBaseName, metaOut, 'l',
		       params->calibration_header_offset*10);
  }
  if (general->p_pol_data) {
    asfPrintStatus("\n   Ingesting P-band polarimetric data ...\n\n");
    ingest_polsar_data(inBaseName, outBaseName, metaOut, 'p',
		       params->calibration_header_offset*10);
  }
  
  meta_free(metaIn);
  meta_free(metaOut);
}
