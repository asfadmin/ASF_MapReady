#include "airsar.h"
#include "asf_meta.h"
#include <ctype.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

char *get_airsar(char *buf, char *str)
{
  char *p, *q, *value;

  value = (char *) MALLOC(sizeof(char)*50);
  q = (char *) MALLOC(sizeof(char)*50);
  p = strstr(buf, str);
  if (p) {
    strncpy(q, p, 50);
    strcpy(value, q+strlen(str));
  }
  else
    strcpy(value, "");

  FREE(q);

  return value;
}

airsar_header *read_airsar_header(const char *dataFile)
{
  airsar_header *header = NULL;
  FILE *fp;
  char buf[4400], *value;
  double version;

  // Allocate memory and file handling
  value = (char *) MALLOC(sizeof(char)*25);
  header = (airsar_header *) CALLOC(1, sizeof(airsar_header));
  fp = FOPEN(dataFile, "r");
  if (fgets(buf, 4400, fp) == NULL)
    asfPrintError("Could not read general header\n");
  FCLOSE(fp);

  // Check for the processor version
  // We take this as an indicator that we actually deal with AirSAR data
  version = atof(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION"));

  // Read general header
  if (version > 0.0) {
    header->record_length = atoi(get_airsar(buf, "RECORD LENGTH IN BYTES ="));
    header->number_records = atoi(get_airsar(buf, "NUMBER OF HEADER RECORDS ="));
    header->sample_count = 
      atoi(get_airsar(buf, "NUMBER OF SAMPLES PER RECORD ="));
    header->line_count = 
      atoi(get_airsar(buf, "NUMBER OF LINES IN IMAGE ="));
    sprintf(header->processor, "%s", 
      trim_spaces(get_airsar(buf, "JPL AIRCRAFT SAR PROCESSOR VERSION")));
    value = trim_spaces(get_airsar(buf, "DATA TYPE ="));
    if (strcmp(value, "INTEGER*2") == 0)
      header->data_type = INTEGER16;
    sprintf(header->range_projection, "%s",
	    trim_spaces(get_airsar(buf, "RANGE PROJECTION =")));
    header->x_pixel_size = 
      atof(get_airsar(buf, "RANGE PIXEL SPACING (METERS) ="));
    header->y_pixel_size = 
      atof(get_airsar(buf, "AZIMUTH PIXEL SPACING (METERS) ="));
    header->first_data_offset = 
      atoi(get_airsar(buf, "BYTE OFFSET OF FIRST DATA RECORD ="));
    if (header->first_data_offset == 0)
      header->first_data_offset = header->record_length*header->number_records;
    header->parameter_header_offset =
      atoi(get_airsar(buf, "BYTE OFFSET OF PARAMETER HEADER ="));
    header->calibration_header_offset =
      atoi(get_airsar(buf, "BYTE OFFSET OF CALIBRATION HEADER ="));
    header->dem_header_offset = 
      atoi(get_airsar(buf, "BYTE OFFSET OF DEM HEADER ="));
  }
  
  return header;
}

airsar_param_header *read_airsar_params(const char *dataFile)
{
  airsar_param_header *params=NULL;
  airsar_header *header = read_airsar_header(dataFile);
  FILE *fp;
  char *buf;
  int size;
 
  // Allocate memory and file handling
  params = (airsar_param_header *) CALLOC(1, sizeof(airsar_param_header));
  if (header->calibration_header_offset > 0)
    size = header->calibration_header_offset - header->parameter_header_offset;
  else if (header->dem_header_offset > 0)
    size = header->dem_header_offset - header->parameter_header_offset;
  else
    size = header->first_data_offset - header->parameter_header_offset;
  buf = (char *) MALLOC(sizeof(char)*size);
  fp = FOPEN(dataFile, "r");
  FSEEK(fp, header->parameter_header_offset, 1);
  if (fgets(buf, size, fp) == NULL)
    asfPrintError("Could not read parameter header\n");
  FCLOSE(fp);

  // Read parameter header
  sprintf(params->site_name, "%s", 
	  trim_spaces(get_airsar(buf, "SITE NAME")));
  sprintf(params->cct_type, "%s", 
	  trim_spaces(get_airsar(buf, "CCT TYPE")));
  params->cct_id = atoi(get_airsar(buf, "CCT ID"));
  params->start_lat =
    atof(get_airsar(buf, 
		    "LATITUDE AT START OF SCENE (DEGREES)"));
  params->start_lon = 
    atof(get_airsar(buf, 
		    "LONGITUDE AT START OF SCENE (DEGREES)"));
  params->end_lat = 
    atof(get_airsar(buf, 
		    "LATITUDE AT END OF SCENE (DEGREES)"));
  params->end_lon =
    atof(get_airsar(buf, 
		    "LONGITUDE AT END OF SCENE (DEGREES)"));
  sprintf(params->acquisition_date, "%s",
	  trim_spaces(get_airsar(buf, 
				 "DATE OF ACQUISITION (GMT)")));
  params->acquisition_seconds =
    atof(get_airsar(buf, 
		    "TIME OF ACQUISITION: SECONDS IN DAY"));
  sprintf(params->frequencies, "%s",
	  trim_spaces(get_airsar(buf, 
				 "FREQUENCIES COLLECTED")));
  params->prf = 
    atof(get_airsar(buf, "PRF AT START OF TRANSFER (HZ)"));
  params->range_sampling_rate =
    atof(get_airsar(buf, "SAMPLING RATE (MHZ)"));
  params->chirp_bandwidth =
    atof(get_airsar(buf, "CHIRP BANDWIDTH (MHZ)"));
  params->pulse_length =
    atof(get_airsar(buf, "PULSE LENGTH (MICROSECONDS)"));
  params->wavelength =
    atof(get_airsar(buf, "PROCESSOR WAVELENGTH (METERS)"));
  params->near_slant_range =
    atof(get_airsar(buf, "NEAR SLANT RANGE (METERS)"));
  params->far_slant_range =
    atof(get_airsar(buf, "FAR SLANT RANGE (METERS)"));
  params->near_look_angle =
    atof(get_airsar(buf, "NEAR LOOK ANGLE (DEGREES)"));
  params->far_look_angle =
    atof(get_airsar(buf, "FAR LOOK ANGLE (DEGEES)"));
  params->azimuth_look_count =
    atoi(get_airsar(buf, 
		    "NUMBER OF LOOKS PROCESSED IN AZIMUTH"));
  params->range_look_count =
    atoi(get_airsar(buf, 
		    "NUMBER OF LOOKS PROCESSED IN RANGE"));
  params->deskewed =
    atoi(get_airsar(buf, 
		    "DESKEW FLAG (1=DESKEWED, 2=NOT DESKEWED)"));
  params->sr_sample_spacing =
    atof(get_airsar(buf, 
		    "SLANT RANGE SAMPLE SPACING (METERS)"));
  params->slant_range_resolution =
    atof(get_airsar(buf, 
		    "NOMINAL SLANT RANGE RESOLUTION (METERS)"));
  params->azimuth_sample_spacing =
    atof(get_airsar(buf, "AZIMUTH SAMPLE SPACING (METERS)"));
  params->azimuth_resolution =
    atof(get_airsar(buf, 
		    "NOMINAL AZIMUTH RESOLUTION (METERS)"));
  params->center_lat =    atof(get_airsar(buf, "IMAGE CENTER LATITUDE (DEGREES)"));
  params->center_lon =
    atof(get_airsar(buf, 
		    "IMAGE CENTER LONGITUDE (DEGREES)"));
  params->scale_factor =
    atof(get_airsar(buf, "GENERAL SCALE FACTOR"));
  params->cal_factor_hh =
    atof(get_airsar(buf, "CALIBRATION FACTOR APPLIED, DB, HH"));
  params->cal_factor_hv =
    atof(get_airsar(buf, "CALIBRATION FACTOR APPLIED, DB, HV"));
  params->cal_factor_vh =
    atof(get_airsar(buf, "CALIBRATION FACTOR APPLIED, DB, VH"));
  params->cal_factor_vv =
    atof(get_airsar(buf, "CALIBRATION FACTOR APPLIED, DB, VV"));
  params->gps_altitude = 
    atof(get_airsar(buf, "GPS ALTITUDE, M"));
  params->lat_peg_point =
    atof(get_airsar(buf, "LATITUDE OF PEG POINT"));
  params->lon_peg_point =
    atof(get_airsar(buf, "LONGITUDE OF PEG POINT"));
  params->head_peg_point =
    atof(get_airsar(buf, "HEADING AT PEG POINT"));
  params->along_track_offset =
    atof(get_airsar(buf, "ALONG-TRACK OFFSET S0  (M)"));
  params->cross_track_offset =
    atof(get_airsar(buf, "CROSS-TRACK OFFSET C0  (M)"));
  
  return params;
}

airsar_dem_header *read_airsar_dem(const char *dataFile)
{
  airsar_dem_header *dem = NULL;
  airsar_header *header = read_airsar_header(dataFile);
  FILE *fp;
  char *buf;
  int size;

  // Allocate memory and file handling
  dem = (airsar_dem_header *) CALLOC(1, sizeof(airsar_dem_header));
  size = header->first_data_offset - header->dem_header_offset;
  buf = (char *) MALLOC(sizeof(char)*size);
  fp = FOPEN(dataFile, "r");
  FSEEK(fp, header->dem_header_offset, 1);
  if (fgets(buf, size, fp) == NULL)
    asfPrintError("Could not read parameter header\n");
  FCLOSE(fp);

  // Read DEM header
  dem->elevation_offset = atof(get_airsar(buf, "ELEVATION OFFSET (M) ="));
  dem->elevation_increment = 
    atof(get_airsar(buf, "ELEVATION INCREMENT (M) ="));
  dem->corner1_lat = atof(get_airsar(buf, "LATITUDE OF CORNER 1 ="));
  dem->corner1_lon = atof(get_airsar(buf, "LONGITUDE OF CORNER 1 ="));
  dem->corner2_lat = atof(get_airsar(buf, "LATITUDE OF CORNER 2 ="));
  dem->corner2_lon = atof(get_airsar(buf, "LONGITUDE OF CORNER 2 ="));
  dem->corner3_lat = atof(get_airsar(buf, "LATITUDE OF CORNER 3 ="));
  dem->corner3_lon = atof(get_airsar(buf, "LONGITUDE OF CORNER 3 ="));
  dem->corner4_lat = atof(get_airsar(buf, "LATITUDE OF CORNER 4 ="));
  dem->corner4_lon = atof(get_airsar(buf, "LONGITUDE OF CORNER 4 ="));
  dem->lat_peg_point = 
    atof(get_airsar(buf, "LATITUDE OF PEG POINT ="));
  dem->lon_peg_point = 
    atof(get_airsar(buf, "LONGITUDE OF PEG POINT ="));
  dem->head_peg_point = 
    atof(get_airsar(buf, "HEADING AT PEG POINT (DEGREES) ="));
  dem->along_track_offset = 
    atof(get_airsar(buf, "ALONG-TRACK OFFSET S0 (M) ="));
  dem->cross_track_offset =
    atof(get_airsar(buf, "CROSS-TRACK OFFSET C0 (M) ="));

  return dem;
}

airsar_cal_header *read_airsar_cal(const char *dataFile)
{
  airsar_cal_header *cal = NULL;
  airsar_header *header = read_airsar_header(dataFile);
  FILE *fp;
  char *buf;
  int size;

  // Allocate memory and file handling
  cal = (airsar_cal_header *) CALLOC(1, sizeof(airsar_cal_header));
  size = header->first_data_offset - header->calibration_header_offset;
  buf = (char *) MALLOC(sizeof(char)*size);
  fp = FOPEN(dataFile, "r");
  FSEEK(fp, header->calibration_header_offset, 1);
  if (fgets(buf, size, fp) == NULL)
    asfPrintError("Could not read parameter header\n");
  FCLOSE(fp);

  // Read DEM header
  cal->scale_factor =
    atof(get_airsar(buf, "GENERAL SCALE FACTOR (dB)"));
  cal->hh_amp_cal_factor = 
    atof(get_airsar(buf, "HH AMPLITUDE CALIBRATION FACTOR (dB)"));
  cal->hv_amp_cal_factor = 
    atof(get_airsar(buf, "HV AMPLITUDE CALIBRATION FACTOR (dB)"));
  cal->vh_amp_cal_factor = 
    atof(get_airsar(buf, "VH AMPLITUDE CALIBRATION FACTOR (dB)"));
  cal->vv_amp_cal_factor = 
    atof(get_airsar(buf, "VV AMPLITUDE CALIBRATION FACTOR (dB)"));
  cal->hh_phase_cal_factor = 
    atof(get_airsar(buf, "HH PHASE CALIBRATION FACTOR (DEGREES)"));
  cal->hv_phase_cal_factor = 
    atof(get_airsar(buf, "HV PHASE CALIBRATION FACTOR (DEGREES)"));
  cal->vh_phase_cal_factor = 
    atof(get_airsar(buf, "VH PHASE CALIBRATION FACTOR (DEGREES)"));
  cal->vv_phase_cal_factor = 
    atof(get_airsar(buf, "VV PHASE CALIBRATION FACTOR (DEGREES)"));
  cal->hh_noise_sigma0 = 
    atof(get_airsar(buf, "HH NOISE EQUIVALENT SIGMA ZERO (dB)"));
  cal->hv_noise_sigma0 = 
    atof(get_airsar(buf, "HV NOISE EQUIVALENT SIGMA ZERO (dB)"));
  cal->vv_noise_sigma0 = 
    atof(get_airsar(buf, "VV NOISE EQUIVALENT SIGMA ZERO (dB)"));
  cal->byte_offset_hh_corr = 
    atof(get_airsar(buf, "BYTE OFFSET TO HH CORRECTION VECTOR"));
  cal->byte_offset_hv_corr = 
    atof(get_airsar(buf, "BYTE OFFSET TO HV CORRECTION VECTOR"));
  cal->byte_offset_vv_corr = 
    atof(get_airsar(buf, "BYTE OFFSET TO VV CORRECTION VECTOR"));
  cal->num_bytes_corr = 
    atof(get_airsar(buf, "NUMBER OF BYTES IN CORRECTION VECTORS"));

  return cal;
}

static airsar_general *read_airsar_general(const char *inBaseName)
{
  // Read general metadata file
  airsar_general *general=NULL;
  char line[256]="", *value, *p, *q;

  general = (airsar_general *) CALLOC(1, sizeof(airsar_general));

  char *metaFile = MALLOC(sizeof(char)*(strlen(inBaseName)+20));
  if (!fileExists(inBaseName))
    sprintf(metaFile, "%s_meta.airsar", inBaseName);
  else {
    strcpy(metaFile, inBaseName);
    q = strstr(inBaseName, "_meta.airsar");
    if (q)
      *q = '\0';
  }

  FILE *fpIn = FOPEN(metaFile, "r");

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
      else if (strncmp(line, "Length in Km(for highest freq product)", 38) == 0)
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
      else if (strncmp(line, "C-band Cross Track Interferometric data", 39) == 0) {
        if (strncmp(value, "Yes", 3) == 0)
            general->c_cross_data = TRUE;
        else if (strncmp(value, "None", 4) == 0)
            general->c_cross_data = FALSE;
      }
      else if (strncmp(line, "L-band Cross Track Interferometric data", 39) == 0) {
        if (strncmp(value, "Yes", 3) == 0)
            general->l_cross_data = TRUE;
        else if (strncmp(value, "None", 4) == 0)
            general->l_cross_data = FALSE;
      }
      else if (strncmp(line, "C-band Along Track Interferometric data", 39) == 0) {
        if (strncmp(value, "Yes", 3) == 0)
            general->c_along_data = TRUE;
        else if (strncmp(value, "None", 4) == 0)
        general->c_along_data = FALSE;
      }
      else if (strncmp(line, "L-band Along Track Interferometric data", 39) == 0) {
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

  free(metaFile);
  return general;
}

meta_parameters *import_airsar_meta(const char *dataName,
				    const char *inBaseName, int force)
{
  airsar_general *general = NULL;
  airsar_header *header = read_airsar_header(dataName);
  airsar_param_header *params = read_airsar_params(dataName);
  airsar_dem_header *dem = NULL;
  airsar_cal_header *cal = NULL;

  if (!force)
    general = read_airsar_general(inBaseName);

  if (!force && (general->c_cross_data || general->l_cross_data)) {
    // Figure out the whether we have a DEM header
    char *demFile;
    demFile = (char *) MALLOC(sizeof(char)*1024);
    int found_c_dem = FALSE, found_l_dem = FALSE, found_p_dem = FALSE;
    
    // The C-Band DEM is the preferred file for extracting the metadata.
    // Look for that first. If no DEM is around then error out.
    sprintf(demFile, "%s_c.demi2", inBaseName);
    if (fileExists(demFile))
      found_c_dem = TRUE;
    sprintf(demFile, "%s_l.demi2", inBaseName);
    if (fileExists(demFile))
      found_l_dem = TRUE;
    sprintf(demFile, "%s_p.demi2", inBaseName);
    if (fileExists(demFile))
      found_p_dem = TRUE;
    if (!found_c_dem && !found_l_dem && !found_p_dem)
      return NULL;
    
    // Assign the correct DEM for generating the AirSAR metadata
    if (found_c_dem)
      sprintf(demFile, "%s_c.demi2", inBaseName);
    else if (found_l_dem)
      sprintf(demFile, "%s_l.demi2", inBaseName);
    else if (found_p_dem)
      sprintf(demFile, "%s_p.demi2", inBaseName);
    
    dem = read_airsar_dem(demFile);
  }
  if (!dem && header->dem_header_offset > 0)
    dem = read_airsar_dem(dataName);
  if (header->calibration_header_offset > 0)
    cal = read_airsar_cal(dataName);

  meta_parameters *ret = airsar2meta(header, params, dem);

  free(general);
  free(header);
  free(params);
  if (dem)
    free(dem);

  return ret;
}

static void fudge_airsar_params(meta_parameters *meta);

int ingest_insar_data(const char *inBaseName, const char *outBaseName, 
		      char band)
{
  airsar_header *header;
  meta_parameters *metaIn, *metaOut;
  FILE *fpIn, *fpOut;
  char *inFile, *outFile;
  int ii, kk, line_offset, ret=FALSE;
  float *floatBuf;

  // Generate metadata file
  inFile = (char *) MALLOC(sizeof(char)*255);
  outFile = (char *) MALLOC(sizeof(char)*255);

  // Ingest the DEM
  sprintf(inFile, "%s_%c.demi2", inBaseName, band);
  if (!fileExists(inFile))
    asfPrintStatus("   Could not find DEM file (%s) ...\n", inFile);
  else {
    asfPrintStatus("   Ingesting DEM ...\n");
    sprintf(outFile, "%s_%c_dem.img", outBaseName, band);
    header = read_airsar_header(inFile);
    metaIn = import_airsar_meta(inFile, inBaseName, FALSE);
    line_offset = header->first_data_offset/metaIn->general->sample_count/2;
    metaIn->general->line_count += line_offset;
    metaOut = import_airsar_meta(inFile, inBaseName, FALSE);
    metaIn->general->data_type = INTEGER16;
    metaOut->general->data_type = REAL32;
    floatBuf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
    fpIn = FOPEN(inFile, "rb");
    fpOut = FOPEN(outFile, "wb");
    for (ii=0; ii<metaOut->general->line_count; ii++) {
      get_float_line(fpIn, metaIn, ii+line_offset, floatBuf);
      for (kk=0; kk<metaIn->general->sample_count; kk++)
	floatBuf[kk] = floatBuf[kk]*metaIn->airsar->elevation_increment + 
	  metaIn->airsar->elevation_offset;
      put_float_line(fpOut, metaOut, ii, floatBuf);
      asfLineMeter(ii, metaOut->general->line_count);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    metaOut->general->image_data_type = DEM;
    strcpy(metaOut->general->bands, "DEM");
    //fudge_airsar_params(metaOut);
    meta_write(metaOut, outFile);
    ret = TRUE;
  }

  // Ingest amplitude image
  sprintf(inFile, "%s_%c.vvi2", inBaseName, band);
  if (!fileExists(inFile))
    asfPrintStatus("   Could not find amplitude file (%s) ...\n", inFile);
  else {
    asfPrintStatus("   Ingesting amplitude image ...\n");
    sprintf(outFile, "%s_%c_vv.img", outBaseName, band);
    header = read_airsar_header(inFile);
    line_offset = header->first_data_offset/metaIn->general->sample_count/2;
    metaIn->general->line_count = metaOut->general->line_count + line_offset;
    metaIn->general->data_type = INTEGER16;
    metaOut->general->data_type = REAL32;
    floatBuf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
    fpIn = FOPEN(inFile, "rb");
    fpOut = FOPEN(outFile, "wb");
    for (ii=0; ii<metaOut->general->line_count; ii++) {
      get_float_line(fpIn, metaIn, ii+line_offset, floatBuf);
      put_float_line(fpOut, metaOut, ii, floatBuf);
      asfLineMeter(ii, metaOut->general->line_count);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    metaOut->general->image_data_type = AMPLITUDE_IMAGE;
    strcpy(metaOut->general->bands, "AMP");
    //fudge_airsar_params(metaOut);
    meta_write(metaOut, outFile);
    ret = TRUE;
  }

  // Ingest coherence image
  sprintf(inFile, "%s_%c.corgr", inBaseName, band);
  if (!fileExists(inFile))
    asfPrintStatus("   Could not find coherence image (%s) ...\n", inFile);
  else {
    asfPrintStatus("   Ingesting coherence image ...\n");
    sprintf(outFile, "%s_%c_coh.img", outBaseName, band);
    header = read_airsar_header(inFile);
    line_offset = header->first_data_offset / metaIn->general->sample_count;
    metaIn->general->line_count = metaOut->general->line_count + line_offset;
    metaIn->general->data_type = BYTE;
    metaOut->general->data_type = REAL32;
    floatBuf = (float *) MALLOC(sizeof(float)*metaIn->general->sample_count);
    fpIn = FOPEN(inFile, "rb");
    fpOut = FOPEN(outFile, "wb");
    for (ii=0; ii<metaOut->general->line_count; ii++) {
      get_float_line(fpIn, metaIn, ii+line_offset, floatBuf);
      put_float_line(fpOut, metaOut, ii, floatBuf);
      asfLineMeter(ii, metaOut->general->line_count);
    }
    FCLOSE(fpIn);
    FCLOSE(fpOut);
    metaOut->general->image_data_type = COHERENCE_IMAGE;
    strcpy(metaOut->general->bands, "COH");
    //fudge_airsar_params(metaOut);
    meta_write(metaOut, outFile);
    ret = TRUE;
  }

  // Clean up
  if (floatBuf)
    FREE(floatBuf);
  FREE(inFile);
  FREE(outFile);

  return ret;
}

int ingest_polsar_data(const char *inBaseName, const char *outBaseName,
		       char band)
{
  FILE *fpIn, *fpOut;
  char *inFile, *outFile;
  int ii, kk, ret;
  char *byteBuf;
  float *power, *shh_amp, *shh_phase, *shv_amp, *shv_phase, *svh_amp;
  float *svh_phase, *svv_amp, *svv_phase;
  float total_power, ysca, scale, x;
  complexFloat cpx;

  // Allocate memory
  inFile = (char *) MALLOC(sizeof(char)*255);
  outFile = (char *) MALLOC(sizeof(char)*255);

  // Ingest polarimetric data
  sprintf(inFile, "%s_%c.datgr", inBaseName, band);
  if (!fileExists(inFile))
    sprintf(inFile, "%s_%c.dat", inBaseName, band);
  if (!fileExists(inFile)) {
    asfPrintStatus("   Cound not find polarimetric data set (%s_%c) ...\n",
		   inBaseName, band);
    return FALSE;
  }
  else {
    meta_parameters *meta = import_airsar_meta(inFile, inBaseName, FALSE);
    meta->general->data_type = REAL32;
    meta->general->image_data_type = POLARIMETRIC_IMAGE;
    meta->general->band_count = 9;
    sprintf(meta->general->bands,
	    "POWER,SHH_AMP,SHH_PHASE,SHV_AMP,SHV_PHASE,SVH_AMP,SVH_PHASE,"
	    "SVV_AMP,SVV_PHASE");
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
    scale = 1.0;
    airsar_header *header = read_airsar_header(inFile);
    long offset = header->first_data_offset;
    sprintf(outFile, "%s_%c.img", outBaseName, band);
    fpIn = FOPEN(inFile, "rb");
    fpOut = FOPEN(outFile, "wb");
    //printf("offset=%ld %d\n",offset,SEEK_SET);
    FSEEK(fpIn, offset, SEEK_SET);
    for (ii=0; ii<meta->general->line_count; ii++) {
      for (kk=0; kk<meta->general->sample_count; kk++) {
	FREAD(byteBuf, sizeof(char), 10, fpIn);
	total_power =
	  scale * ((float)byteBuf[1]/254.0 + 1.5) * pow(2, byteBuf[0]);
	ysca = 2.0 * sqrt(total_power);
	power[kk] = total_power;
	cpx.real = 127 * (float)byteBuf[2] / x;
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
    ret = TRUE;
  }

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
  FREE(byteBuf);

  return ret;
}

void import_airsar(const char *inBaseName, radiometry_t radiometry,
		   int db_flag, const char *outBaseName)
{
  char inFile[1024];
  int found_c_dem = FALSE, found_l_dem = FALSE;

  // Check radiometry
  if (radiometry != r_AMP &&
      radiometry != r_SIGMA && radiometry != r_SIGMA_DB) {
    asfPrintWarning("Radiometry other than AMPLITUDE and SIGMA is not "
		    "supported for AirSAR data.\nDefaulting back to "
		    "AMPLITUDE.\n");
    radiometry = r_AMP;
  }

  airsar_general *general = read_airsar_general(inBaseName);

  // Check for existence of DEMs.
  // The C-Band DEM is the preferred file for extracting the metadata.
  // Look for that first. If no DEM is around then error out.
  if (general->c_cross_data || general->l_cross_data) {
    sprintf(inFile, "%s_c.demi2", inBaseName);
    if (fileExists(inFile))
      found_c_dem = TRUE;
    sprintf(inFile, "%s_l.demi2", inBaseName);
    if (fileExists(inFile))
      found_l_dem = TRUE;
    if (!found_c_dem && found_l_dem)
      asfPrintWarning("Could not find C-band DEM.\nRequired for most reliable "
		      "metadata extraction\n");
    if (!found_c_dem && !found_l_dem)
      asfPrintError("Could not find any DEM.\nCan't reliably import this "
		    "Airsar data.\n");
  }

  // Check for interferometric data
  int insar = FALSE;
  if (general->c_cross_data) {
    asfPrintStatus("\n   Ingesting C-band cross track interferometric data ..."
           "\n\n");
    if (ingest_insar_data(inBaseName, outBaseName, 'c'))
      insar = TRUE;
  }
  if (general->l_cross_data) {
    asfPrintStatus("\n   Ingesting L-band cross track interferometric data ..."
           "\n\n");
    if (ingest_insar_data(inBaseName, outBaseName, 'l'))
      insar = TRUE;
  }

  // Kept out the along-track interferometric data for the moment.
  // Only a few data sets were acquired that way and we have no real way
  // to verify the results.

  // Check for polarimetric data
  // Do this only if you don't have ingested interferometric data yet.
  // Polarimetric data are apparently in a different geometry compared
  // to the interferometric data.
  //if (!insar) {
    if (general->c_pol_data) {
      asfPrintStatus("\n   Ingesting C-band polarimetric data ...\n\n");
      ingest_polsar_data(inBaseName, outBaseName, 'c');
    }
    if (general->l_pol_data) {
      asfPrintStatus("\n   Ingesting L-band polarimetric data ...\n\n");
      ingest_polsar_data(inBaseName, outBaseName, 'l');
    }
    if (general->p_pol_data) {
      asfPrintStatus("\n   Ingesting P-band polarimetric data ...\n\n");
      ingest_polsar_data(inBaseName, outBaseName, 'p');
    }
    /*
  }
  else
    asfPrintStatus("\n   Skipping polarimetric data (different geometry) ..."
		   "\n\n");
    */
  free(general);
}

// The purpose of this code is to refine the values of the along
// and cross track offsets, so that the meta_get_latLon() value
// returned at the corners of the image matches what the correct
// corner lat/lon values are, from the metadata.  Why do we believe
// the corners locations more than the given along/cross track
// offsets?  From the data sets we've been looking at, it seems like
// the corner coords are good, but the offsets aren't...

// We just do a 2-d minimization of the total error (in line/sample
// space -- not lat/lon (minimizing in lat/lon coords would favor
// minimizing the lon difference near the poles)) between the
// corner coordinates.
struct fudge_airsar_params {
        meta_parameters *meta;
};

static int
getObjective(const gsl_vector *x, void *params, gsl_vector *f)
{
  double c0 = gsl_vector_get(x,0);
  double s0 = gsl_vector_get(x,1);

  if (!meta_is_valid_double(c0) || !meta_is_valid_double(s0)) {
    // This does happen sometimes, when we've already found the root
    return GSL_FAILURE;
  }

  struct fudge_airsar_params *p = (struct fudge_airsar_params *)params;
  meta_parameters *meta = p->meta;

  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;

  double old_c0 = meta->airsar->cross_track_offset;
  double old_s0 = meta->airsar->along_track_offset;
  meta->airsar->cross_track_offset = c0;
  meta->airsar->along_track_offset = s0;

  double line, samp, err = 0.;

  meta_get_lineSamp(meta, meta->location->lat_start_near_range,
                          meta->location->lon_start_near_range,
                    0., &line, &samp);
  err += hypot(line,samp);

  meta_get_lineSamp(meta, meta->location->lat_start_far_range,
                          meta->location->lon_start_far_range,
                    0., &line, &samp);
  err += hypot(line,samp-(double)ns);

  meta_get_lineSamp(meta, meta->location->lat_end_near_range,
                          meta->location->lon_end_near_range,
                    0., &line, &samp);
  err += hypot(line-(double)nl,samp);

  meta_get_lineSamp(meta, meta->location->lat_end_far_range,
                          meta->location->lon_end_far_range,
                    0., &line, &samp);
  err += hypot(line-(double)nl,samp-(double)ns);

  //printf("getObjective> [%f,%f] -> %f\n", c0, s0, err);

  gsl_vector_set(f,0,err);
  gsl_vector_set(f,1,err);

  meta->airsar->cross_track_offset = old_c0;
  meta->airsar->along_track_offset = old_s0;

  return GSL_SUCCESS;
}

static void coarse_search(double c0_extent_min, double c0_extent_max,
                          double s0_extent_min, double s0_extent_max,
                          double *c0_min, double *s0_min,
                          meta_parameters *meta)
{
    struct fudge_airsar_params params;
    params.meta = meta;

    double the_min = 9999999;
    double min_c0=99, min_s0=99;
    int i,j,k=6;
    double c0_extent = c0_extent_max - c0_extent_min;
    double s0_extent = s0_extent_max - s0_extent_min;
    gsl_vector *v = gsl_vector_alloc(2);
    gsl_vector *u = gsl_vector_alloc(2);
    //printf("           ");
    //for (j = 0; j <= k; ++j) {
    //    double s0 = s0_extent_min + ((double)j)/k*s0_extent;
    //    printf("%9.3f ", s0);
    //}
    //printf("\n           ");
    //for (j = 0; j <= k; ++j)
    //    printf("--------- ");
    //printf("\n");
    for (i = 0; i <= k; ++i) {
        double c0 = c0_extent_min + ((double)i)/k*c0_extent;
        //printf("%9.3f | ", c0);

        for (j = 0; j <= k; ++j) {
            double s0 = s0_extent_min + ((double)j)/k*s0_extent;

            gsl_vector_set(v, 0, c0);
            gsl_vector_set(v, 1, s0);
            getObjective(v,(void*)(&params), u);
            double n = gsl_vector_get(u,0);
            //printf("%9.3f ", n);
            if (n<the_min) {
                the_min=n;
                min_c0=gsl_vector_get(v,0);
                min_s0=gsl_vector_get(v,1);
            }
        }
        //printf("\n");
    }

    *c0_min = min_c0;
    *s0_min = min_s0;

    gsl_vector_free(v);
    gsl_vector_free(u);
}

static void
generate_start(meta_parameters *meta, double c0, double s0,
               double *start_c0, double *start_s0)
{
    int i;

    double extent_c0_min = -100000. + c0;
    double extent_c0_max = 100000. + c0;

    double extent_s0_min = -100000. + s0;
    double extent_s0_max = 100000. + s0;

    double c0_range = extent_c0_max - extent_c0_min;
    double s0_range = extent_s0_max - extent_s0_min;

    for (i=0; i<12; ++i)
    //for (i=0; i<4; ++i)
    {
        coarse_search(extent_c0_min, extent_c0_max,
                      extent_s0_min, extent_s0_max,
                      start_c0, start_s0, meta);

        c0_range /= 3.;
        s0_range /= 3.;

        extent_c0_min = *start_c0 - c0_range/2.;
        extent_c0_max = *start_c0 + c0_range/2.;

        extent_s0_min = *start_s0 - s0_range/2.;
        extent_s0_max = *start_s0 + s0_range/2.;

        //printf("refining search to region: cross: (%9.3f,%9.3f)\n"
        //       "                           along: (%9.3f,%9.3f)\n",
        //       extent_c0_min, extent_c0_max,
        //       extent_s0_min, extent_s0_max);
    }
}

static void show_error(meta_parameters *meta, const char *descrip)
{
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  double line, samp, err=0;

  asfPrintStatus("Computing known corner coordinates vs. calculated "
                 "metadata differences:\n");

  meta_get_lineSamp(meta, meta->location->lat_start_near_range,
                          meta->location->lon_start_near_range,
                    0., &line, &samp);
  asfPrintStatus("  Start Near: %f (%d) %f (%d)\n", line, 0, samp, 0);
  err += hypot(line, samp);

  meta_get_lineSamp(meta, meta->location->lat_start_far_range,
                          meta->location->lon_start_far_range,
                    0., &line, &samp);
  asfPrintStatus("  Start Far: %f (%d) %f (%d)\n", line, 0, samp, ns);
  err += hypot(line, samp-(double)ns);

  meta_get_lineSamp(meta, meta->location->lat_end_near_range,
                          meta->location->lon_end_near_range,
                    0., &line, &samp);
  asfPrintStatus("  End Near: %f (%d) %f (%d)\n", line, nl, samp, 0);
  err += hypot(line-(double)nl, samp);

  meta_get_lineSamp(meta, meta->location->lat_end_far_range,
                          meta->location->lon_end_far_range,
                    0., &line, &samp);
  asfPrintStatus("  End Far: %f (%d) %f (%d)\n", line, nl, samp, ns);
  err += hypot(line-(double)nl, samp-(double)ns);

  asfPrintStatus("%s, average corner error: %.2f pixels\n", descrip, err/4.);
}

static void fudge_airsar_params(meta_parameters *meta)
{
  int status, iter = 0, max_iter = 1000;
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  gsl_error_handler_t *prev;
  struct fudge_airsar_params params;
  const size_t n = 2;
  double c0_initial, s0_initial;
  double out_c0, out_s0;

  params.meta = meta;

  asfPrintStatus("Refining airsar cross-track and along-track offsets to "
         "match corner locations.\n");

  show_error(meta, "Prior to airsar geolocation refinement");

  asfPrintStatus("Prior to coarse refinement (original metadata values):\n");
  asfPrintStatus("  cross track offset: %.1fm\n",
                 meta->airsar->cross_track_offset);
  asfPrintStatus("  along track offset: %.1fm\n",
                 meta->airsar->along_track_offset);
  generate_start(meta, meta->airsar->cross_track_offset,
                 meta->airsar->along_track_offset, &c0_initial, &s0_initial);
  asfPrintStatus("Starting iterative search with:\n");
  asfPrintStatus("  cross track offset: %.1fm\n", c0_initial);
  asfPrintStatus("  along track offset: %.1fm\n", s0_initial);

  gsl_multiroot_function F = {&getObjective, n, &params};
  gsl_vector *x = gsl_vector_alloc(n);

  gsl_vector_set (x, 0, c0_initial);
  gsl_vector_set (x, 1, s0_initial);

  T = gsl_multiroot_fsolver_hybrid;
  s = gsl_multiroot_fsolver_alloc(T, n);
  gsl_multiroot_fsolver_set(s, &F, x);

  prev = gsl_set_error_handler_off();

  do {
    ++iter;
    status = gsl_multiroot_fsolver_iterate(s);

    // abort if stuck
    if (status) break;

    status = gsl_multiroot_test_residual (s->f, 1e-8);
  } while (status == GSL_CONTINUE && iter < max_iter);

  // we allow GSL_ENOPROG (not making progress), since often the coarse
  // search ends up at or very close to the minimum
  if (status == GSL_SUCCESS || status == GSL_ENOPROG) {
    asfPrintStatus("Converged after %d iteration%s.\n",
                   iter, iter==1 ? "" : "s");
    out_c0 = gsl_vector_get(s->x, 0);
    out_s0 = gsl_vector_get(s->x, 1);

    asfPrintStatus("Final values:\n");
    asfPrintStatus("  cross track offset: %.1fm [adjusted by %.1fm]\n",
                   out_c0, fabs(out_c0-meta->airsar->cross_track_offset));
    asfPrintStatus("  along track offset: %.1fm [adjusted by %.1fm]\n",
                   out_s0, fabs(out_s0-meta->airsar->along_track_offset));

    meta->airsar->cross_track_offset = out_c0;
    meta->airsar->along_track_offset = out_s0;

    show_error(meta, "After airsar geolocation refinement");
  }
  else {
    asfPrintStatus("After %d iterations, failed to converge:\n  %s\n"
                   "Metadata parameters left unchanged.\n",
                   iter, gsl_strerror(status));
    out_c0 = out_s0 = 0;
  }

  gsl_multiroot_fsolver_free(s);
  gsl_vector_free(x);
  gsl_set_error_handler(prev);

}
