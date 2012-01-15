#include "asf_meta.h"
#include "dateUtil.h"
#include "terrasar.h"
#include "xml_util.h"
#include "asf_nan.h"

terrasar_meta *terrasar_meta_init(void)
{
  terrasar_meta *terrasar;
  terrasar = (terrasar_meta *) CALLOC(1, sizeof(terrasar_meta));
 
  // general block
  strcpy(terrasar->filename, MAGIC_UNSET_STRING);
  strcpy(terrasar->mission, MAGIC_UNSET_STRING);
  strcpy(terrasar->sensor, MAGIC_UNSET_STRING);
  strcpy(terrasar->imagingMode, MAGIC_UNSET_STRING);
  strcpy(terrasar->elevationBeamConfiguration, MAGIC_UNSET_STRING);
  strcpy(terrasar->imageDataType, MAGIC_UNSET_STRING);
  terrasar->imageDataDepth = MAGIC_UNSET_INT;
  strcpy(terrasar->imageDataFormat, MAGIC_UNSET_STRING);
  strcpy(terrasar->azimuthTimeUTC, MAGIC_UNSET_STRING);
  terrasar->absOrbit = MAGIC_UNSET_INT;
  strcpy(terrasar->orbitDirection, MAGIC_UNSET_STRING);
  terrasar->numberOfLayers = MAGIC_UNSET_INT;
  strcpy(terrasar->bands, MAGIC_UNSET_STRING);
  terrasar->numberOfRows = MAGIC_UNSET_INT;
  terrasar->numberOfColumns = MAGIC_UNSET_INT;
  terrasar->rangeResolution = MAGIC_UNSET_DOUBLE;
  terrasar->azimuthResolution = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCenterCoordLat = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCenterCoordLon = MAGIC_UNSET_DOUBLE;

  // SAR block
  strcpy(terrasar->projection, MAGIC_UNSET_STRING);
  strcpy(terrasar->lookDirection, MAGIC_UNSET_STRING);
  terrasar->azimuthLooks = MAGIC_UNSET_INT;
  terrasar->rangeLooks = MAGIC_UNSET_INT;
  strcpy(terrasar->imageCoordinateType, MAGIC_UNSET_STRING);
  terrasar->rowSpacing = MAGIC_UNSET_DOUBLE;
  terrasar->columnSpacing = MAGIC_UNSET_DOUBLE;
  terrasar->centerFrequency = MAGIC_UNSET_DOUBLE;
  terrasar->prf = MAGIC_UNSET_DOUBLE;
  terrasar->totalProcessedAzimuthBandwidth = MAGIC_UNSET_DOUBLE;
  terrasar->pulseLength = MAGIC_UNSET_DOUBLE;
  terrasar->rsf = MAGIC_UNSET_DOUBLE;
  strcpy(terrasar->polarisationMode, MAGIC_UNSET_STRING);

  // Doppler block
  terrasar->doppler = NULL;

  // state vectors
  strcpy(terrasar->sceneStart, MAGIC_UNSET_STRING);
  strcpy(terrasar->sceneStop, MAGIC_UNSET_STRING);
  terrasar->state_vectors = NULL;
  
  // location block
  terrasar->sceneCornerCoord1Lat = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord1Lon = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord2Lat = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord2Lon = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord3Lat = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord3Lon = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord4Lat = MAGIC_UNSET_DOUBLE;
  terrasar->sceneCornerCoord4Lon = MAGIC_UNSET_DOUBLE;

  // calibration
  terrasar->cal_factor = MAGIC_UNSET_DOUBLE;

  return terrasar;
}

tsx_doppler_params *tsx_doppler_init(int numDopplerEstimates)
{
  tsx_doppler_params *tsx;
  tsx_doppler_t *dop;
  int ii, dc;

  dc = numDopplerEstimates > 0 ? numDopplerEstimates : 0;
  tsx = (tsx_doppler_params *) MALLOC(sizeof(tsx_doppler_params));
  dop = (tsx_doppler_t *) MALLOC(sizeof(tsx_doppler_t) * dc);

  tsx->doppler_count = dc;
  tsx->year = MAGIC_UNSET_INT;
  tsx->julDay = MAGIC_UNSET_INT;
  tsx->second = MAGIC_UNSET_DOUBLE;
  tsx->dop = dop;
  for (ii=0; ii<tsx->doppler_count; ii++) {
    tsx->dop[ii].first_range_time = MAGIC_UNSET_DOUBLE;
    tsx->dop[ii].reference_time = MAGIC_UNSET_DOUBLE;
    tsx->dop[ii].poly_degree = MAGIC_UNSET_INT;
    tsx->dop[ii].coefficient = NULL;
  }

  return tsx;
}

terrasar_meta *read_terrasar_meta(const char *dataFile)
{
  int ii, kk, numStateVectors, numDopplerEstimates;
  ymd_date imgStartDate, imgDopplerDate, date;
  hms_time imgStartTime, imgDopplerTime, time;
  julian_date julianDate;
  char timeStr[30], str[50];
  tsx_doppler_params *tsx;

  terrasar_meta *terrasar = terrasar_meta_init();

  if (!fileExists(dataFile))
    asfPrintError("Metadata file (%s) does not exist!\n", dataFile);
  xmlDoc *doc = xmlReadFile(dataFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", dataFile);

  strcpy(terrasar->filename, xml_get_string_value(doc, 
     "level1Product.productComponents.annotation.file.location.filename"));
  strcpy(terrasar->mission, xml_get_string_value(doc, 
     "level1Product.productInfo.missionInfo.mission"));
  strcpy(terrasar->sensor, xml_get_string_value(doc, 
     "level1Product.productInfo.acquisitionInfo.sensor"));
  strcpy(terrasar->imagingMode, xml_get_string_value(doc,
     "level1Product.productInfo.acquisitionInfo.imagingMode"));
  strcpy(terrasar->elevationBeamConfiguration, xml_get_string_value(doc, 
     "level1Product.productInfo.acquisitionInfo.elevationBeamConfiguration"));
  strcpy(terrasar->imageDataType, xml_get_string_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageDataType"));
  terrasar->imageDataDepth = xml_get_int_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageDataDepth");
  strcpy(terrasar->imageDataFormat, xml_get_string_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageDataFormat"));
  strcpy(terrasar->azimuthTimeUTC, xml_get_string_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCenterCoord.azimuthTimeUTC"));
  terrasar->absOrbit = 
    xml_get_int_value(doc, "level1Product.productInfo.missionInfo.absOrbit");
  strcpy(terrasar->orbitDirection, xml_get_string_value(doc, 
     "level1Product.productInfo.missionInfo.orbitDirection"));
  terrasar->numberOfLayers = xml_get_int_value(doc, 
     "level1Product.productInfo.imageDataInfo.numberOfLayers");
  strcpy(terrasar->polarisationMode, xml_get_string_value(doc, 
     "level1Product.productInfo.acquisitionInfo.polarisationMode"));
  if (strcmp_case(terrasar->polarisationMode, "SINGLE") == 0)
    strcpy(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[0]"));
  else if (strcmp_case(terrasar->polarisationMode, "DUAL") == 0) {
    strcpy(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[0]"));
    strcat(terrasar->bands, ",");
    strcat(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[1]"));
  }
  else if (strcmp_case(terrasar->polarisationMode, "TWIN") == 0) {
    strcpy(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[0]"));
    strcat(terrasar->bands, ",");
    strcat(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[1]"));
  }
  else if (strcmp_case(terrasar->polarisationMode, "QUAD") == 0) {
    strcpy(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[0]"));
    strcat(terrasar->bands, ",");
    strcat(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[1]"));
    strcat(terrasar->bands, ",");
    strcat(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[2]"));
    strcat(terrasar->bands, ",");
    strcat(terrasar->bands, xml_get_string_value(doc, 
    "level1Product.productInfo.acquisitionInfo.polarisationList.polLayer[3]"));
  }
  terrasar->numberOfRows = xml_get_int_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.numberOfRows");
  terrasar->numberOfColumns = xml_get_int_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.numberOfColumns");
  strcpy(terrasar->projection, xml_get_string_value(doc, 
     "level1Product.productInfo.productVariantInfo.projection"));
  if (strcmp_case(terrasar->projection, "GROUNDRANGE") == 0)
    terrasar->rangeResolution = xml_get_double_value(doc, 
    "level1Product.productInfo.imageDataInfo.imageRaster."
    "groundRangeResolution");
  if (strcmp_case(terrasar->projection, "SLANTRANGE") == 0)
    terrasar->rangeResolution = xml_get_double_value(doc, 
       "level1Product.productSpecific.complexImageInfo.projectedSpacingRange."
       "slantRange");
  // FIXME: cover MAP case
  terrasar->azimuthResolution = xml_get_double_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.azimuthResolution");
  terrasar->sceneCenterCoordLat = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCenterCoord.lat");
  terrasar->sceneCenterCoordLon = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCenterCoord.lon");
  strcpy(terrasar->lookDirection, xml_get_string_value(doc, 
     "level1Product.productInfo.acquisitionInfo.lookDirection"));
  terrasar->azimuthLooks = xml_get_int_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.azimuthLooks");
  terrasar->rangeLooks = xml_get_int_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.rangeLooks");
  strcpy(terrasar->imageCoordinateType, xml_get_string_value(doc, 
     "level1Product.productSpecific.complexImageInfo.imageCoordinateType"));
  terrasar->rowSpacing = xml_get_double_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.rowSpacing");
  terrasar->columnSpacing =  xml_get_double_value(doc, 
     "level1Product.productInfo.imageDataInfo.imageRaster.columnSpacing");
  terrasar->rangeTimeFirst = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.rangeTime.firstPixel");
  terrasar->rangeTimeLast = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.rangeTime.lastPixel");
  terrasar->centerFrequency = xml_get_double_value(doc, 
     "level1Product.instrument.radarParameters.centerFrequency");
  terrasar->prf = xml_get_double_value(doc, 
     "level1Product.instrument.settings.settingRecord.PRF");
  terrasar->totalProcessedAzimuthBandwidth = xml_get_double_value(doc, 
     "level1Product.processing.processingParameter."
     "totalProcessedAzimuthBandwidth");
  // chirp rate ???
  terrasar->pulseLength = xml_get_double_value(doc, 
     "level1Product.processing.processingParameter.rangeCompression.chirps."
     "referenceChirp.pulseLength");
  terrasar->rsf = xml_get_double_value(doc, 
     "level1Product.instrument.settings.RSF");
  // pitch, roll, yaw ???

  // read Doppler values
  terrasar->doppler = meta_doppler_init();
  terrasar->doppler->type = tsx_doppler;
  numDopplerEstimates = xml_get_int_value(doc, 
     "level1Product.processing.doppler.dopplerCentroid[0]."
     "numberOfDopplerRecords");
  tsx = tsx_doppler_init(numDopplerEstimates);
  terrasar->doppler->tsx = tsx;
  strcpy(str, xml_get_string_value(doc, 
     "level1Product.processing.doppler.dopplerCentroid[0].dopplerEstimate[0]."
     "timeUTC"));
  date_terrasar2date(str, &imgDopplerDate, &imgDopplerTime);
  tsx->year = imgDopplerDate.year;
  date_ymd2jd(&imgDopplerDate, &julianDate);
  tsx->julDay = julianDate.jd;
  tsx->second = date_hms2sec(&imgDopplerTime);
  for (ii=0; ii<numDopplerEstimates; ii++) {
    strcpy(timeStr, xml_get_string_value(doc, 
       "level1Product.processing.doppler.dopplerCentroid[0]."
       "dopplerEstimate[%d].timeUTC", ii));
    date_terrasar2date(timeStr, &date, &time);
    tsx->dop[ii].time =
      time_difference(&date, &time, &imgDopplerDate, &imgDopplerTime);
    tsx->dop[ii].first_range_time = xml_get_double_value(doc, 
       "level1Product.processing.doppler.dopplerCentroid[0]."
       "dopplerEstimate[%d].combinedDoppler.validityRangeMin", ii);
    tsx->dop[ii].reference_time = xml_get_double_value(doc, 
       "level1Product.processing.doppler.dopplerCentroid[0]."
       "dopplerEstimate[%d].combinedDoppler.referencePoint", ii);
    tsx->dop[ii].poly_degree = xml_get_double_value(doc, 
       "level1Product.processing.doppler.dopplerCentroid[0]."
       "dopplerEstimate[%d].combinedDoppler.polynomialDegree", ii);
    tsx->dop[ii].coefficient = 
      (double *) MALLOC(sizeof(double) * (tsx->dop[ii].poly_degree+1));
    for (kk=0; kk<=tsx->dop[ii].poly_degree; kk++)
      tsx->dop[ii].coefficient[kk] = xml_get_double_value(doc, 
	 "level1Product.processing.doppler.dopplerCentroid[0]."
         "dopplerEstimate[%d].combinedDoppler.coefficient[%d]", ii, kk);
  }  

  // read state vectors
  strcpy(terrasar->sceneStart, xml_get_string_value(doc, 
     "level1Product.productInfo.sceneInfo.start.timeUTC"));
  date_terrasar2date(terrasar->sceneStart, &imgStartDate, &imgStartTime);
  strcpy(terrasar->sceneStop, xml_get_string_value(doc, 
     "level1Product.productInfo.sceneInfo.stop.timeUTC"));
  numStateVectors = xml_get_int_value(doc, 
     "level1Product.platform.orbit.orbitHeader.numStateVectors");
  terrasar->state_vectors = meta_state_vectors_init(numStateVectors);
  terrasar->state_vectors->year = imgStartDate.year;
  date_ymd2jd(&imgStartDate, &julianDate);
  terrasar->state_vectors->julDay = julianDate.jd;
  terrasar->state_vectors->second = date_hms2sec(&imgStartTime);
  for (ii=0; ii<numStateVectors; ii++) {
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].timeUTC", ii);
    strcpy(timeStr, xml_get_string_value(doc, str));
    date_terrasar2date(timeStr, &date, &time);
    terrasar->state_vectors->vecs[ii].time =
      time_difference(&date, &time, &imgStartDate, &imgStartTime);
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].posX", ii);
    terrasar->state_vectors->vecs[ii].vec.pos.x = 
      xml_get_double_value(doc, str);
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].posY", ii);
    terrasar->state_vectors->vecs[ii].vec.pos.y = 
      xml_get_double_value(doc, str);
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].posZ", ii);
    terrasar->state_vectors->vecs[ii].vec.pos.z = 
      xml_get_double_value(doc, str);
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].velX", ii);
    terrasar->state_vectors->vecs[ii].vec.vel.x = 
      xml_get_double_value(doc, str);
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].velY", ii);
    terrasar->state_vectors->vecs[ii].vec.vel.y = 
      xml_get_double_value(doc, str);
    sprintf(str, "level1Product.platform.orbit.stateVec[%d].velZ", ii);
    terrasar->state_vectors->vecs[ii].vec.vel.z = 
      xml_get_double_value(doc, str);
  }

  // read location information
  terrasar->sceneCornerCoord1Lat = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[0].lat");
  terrasar->sceneCornerCoord1Lon = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[0].lon");
  terrasar->sceneCornerCoord2Lat = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[1].lat");
  terrasar->sceneCornerCoord2Lon = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[1].lon");
  terrasar->sceneCornerCoord3Lat = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[2].lat");
  terrasar->sceneCornerCoord3Lon = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[2].lon");
  terrasar->sceneCornerCoord4Lat = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[3].lat");
  terrasar->sceneCornerCoord4Lon = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.sceneCornerCoord[3].lon");

  // read calibration information
  char calFlag[15];
  strcpy(calFlag, xml_get_string_value(doc,
     "level1Product.productInfo.productVariantInfo.radiometricCorrection"));
  if (strcmp_case(calFlag, "CALIBRATED") == 0)
    terrasar->cal_factor = xml_get_double_value(doc,
       "level1Product.calibration.calibrationConstant.calFactor");

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return terrasar;
}

char *get_terrasar_browse_file(const char *xml_file_name)
{
  xmlDoc *doc = xmlReadFile(xml_file_name, NULL, 0);
  if (!doc) {
    asfPrintWarning("Could not parse file %s\n", xml_file_name);
    return NULL;
  }

  char preview_file[2048];

  char *path = get_dirname(xml_file_name);
  if (strlen(path)>0) {
    strcpy(preview_file, path);
    if (preview_file[strlen(preview_file)-1] != '/')
      strcat(preview_file, "/");
  }
  else
    strcpy(preview_file, "");
  free(path);

  strcat(preview_file, xml_get_string_value(doc, 
         "level1Product.productComponents.browseImage.file.location.path"));
  strcat(preview_file, "/");
  strcat(preview_file, xml_get_string_value(doc, 
         "level1Product.productComponents.browseImage.file.location.filename"));
  
  return STRDUP(preview_file);
}

meta_parameters* terrasar2meta(terrasar_meta *terrasar)
{
  ymd_date date, imgStartDate, imgStopDate;
  hms_time time, imgStartTime, imgStopTime;
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  double lat, re, rp;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  strcpy(meta->general->basename, terrasar->filename);
  strcpy(meta->general->sensor, terrasar->mission);
  strcpy(meta->general->sensor_name, terrasar->sensor);
  strcpy(meta->general->mode, terrasar->elevationBeamConfiguration);
  strcpy(meta->general->processor, "");
  // terrsar->imagingMode determines stripMap, scanSAR and spotLight modes
  // not using the information yet
  meta->general->data_type = REAL32;
  if (strcmp_case(terrasar->imageDataType, "COMPLEX") == 0)
    meta->general->image_data_type = COMPLEX_IMAGE;
  else if (strcmp_case(terrasar->imageDataType, "DETECTED") == 0)
    meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  date_terrasar2date(terrasar->azimuthTimeUTC, &date, &time);
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0lf",
	  date.day, mon[date.month], date.year, time.hour, time.min, time.sec);
  meta->general->orbit = terrasar->absOrbit;
  if (strcmp_case(terrasar->orbitDirection, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(terrasar->orbitDirection, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  meta->general->band_count = terrasar->numberOfLayers;
  meta->general->line_count = terrasar->numberOfRows;
  meta->general->sample_count = terrasar->numberOfColumns;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = terrasar->rangeResolution;
  meta->general->y_pixel_size = terrasar->azimuthResolution;
  meta->general->center_latitude = terrasar->sceneCenterCoordLat;
  meta->general->center_longitude = terrasar->sceneCenterCoordLon;
  meta->general->re_major = 6378137.000; // WGS84
  meta->general->re_minor = 6356752.314; // WGS84

  // SAR block
  meta->sar = meta_sar_init();
  if (strcmp_case(terrasar->projection, "SLANTRANGE") == 0)
    meta->sar->image_type = 'S';
  else if (strcmp_case(terrasar->projection, "GROUNDRANGE") == 0)
    meta->sar->image_type = 'G';
  // FIXME: MAP case not covered yet
  if (strcmp_case(terrasar->lookDirection, "LEFT") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(terrasar->lookDirection, "RIGHT") == 0)
    meta->sar->look_direction = 'R';
  meta->sar->look_count = terrasar->azimuthLooks;
  // TO BE ADDED: terrasar->rangeLooks
  if (strcmp_case(terrasar->imageCoordinateType, "ZERODOPPLER") == 0)
    meta->sar->deskewed = 1;
  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  if (meta->sar->image_type == 'S') {
    meta->sar->range_time_per_pixel = terrasar->rowSpacing;
    meta->sar->azimuth_time_per_pixel = terrasar->columnSpacing;
  }
  else if (meta->sar->image_type == 'G') {
    meta->sar->range_time_per_pixel = 
      (terrasar->rangeTimeLast - terrasar->rangeTimeFirst) /
      terrasar->numberOfColumns;
    meta->general->x_pixel_size = terrasar->columnSpacing;
    meta->general->y_pixel_size = terrasar->rowSpacing;
  }
  meta->sar->slant_range_first_pixel = 
    terrasar->rangeTimeFirst * SPD_LIGHT / 2.0;
  meta->sar->slant_shift = 0.0;
  meta->sar->time_shift = 0.0;
  meta->sar->wavelength = SPD_LIGHT / terrasar->centerFrequency;
  meta->sar->prf = terrasar->prf;
  lat = meta->general->center_latitude * D2R;
  re = meta->general->re_major;
  rp = meta->general->re_minor;
  meta->sar->earth_radius = 
    (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  // FIXME: Doppler values need to be filled in
  meta->sar->azimuth_processing_bandwidth = 
    terrasar->totalProcessedAzimuthBandwidth;
  // FIXME: chirp_rate ???
  meta->sar->pulse_duration = terrasar->pulseLength;
  meta->sar->range_sampling_rate = terrasar->rsf;
  strcpy(meta->sar->polarization, terrasar->polarisationMode);
  if (strcmp_case(terrasar->imageDataType, "COMPLEX") == 0)
    meta->sar->multilook = FALSE;
  else if (strcmp_case(terrasar->imageDataType, "DETECTED") == 0)
    meta->sar->multilook = TRUE;
  // FIXME: pitch, roll, yaw ???

  // Doppler block
  meta->doppler = terrasar->doppler;

  // State vectors
  meta->state_vectors = terrasar->state_vectors;

  // Propagate the state vectors to start, center, end
  date_terrasar2date(terrasar->sceneStart, &imgStartDate, &imgStartTime);
  date_terrasar2date(terrasar->sceneStop, &imgStopDate, &imgStopTime);
  int vector_count = 3;
  double data_int = date_difference(&imgStopDate, &imgStopTime, 
				    &imgStartDate, &imgStartTime) / 2.0;
  while (fabs(data_int) > 10.0) {
    data_int /= 2;
    vector_count = vector_count*2-1;
  }
  propagate_state(meta, vector_count, data_int);

  data_int = date_difference(&imgStopDate, &imgStopTime, 
			     &imgStartDate, &imgStartTime);
  if (meta->sar->image_type == 'G')
    meta->sar->azimuth_time_per_pixel = data_int / meta->general->line_count;
  if (meta->general->orbit_direction == 'A') {
    meta->sar->time_shift = data_int;
    meta->sar->azimuth_time_per_pixel *= -1.0;
  }
  data_int /= 2.0;
  stateVector stVec = meta_get_stVec(meta, data_int);
  meta->sar->satellite_height = sqrt(stVec.pos.x * stVec.pos.x +
				     stVec.pos.y * stVec.pos.y +
				     stVec.pos.z * stVec.pos.z);

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = terrasar->sceneCornerCoord1Lat;
  meta->location->lon_start_near_range = terrasar->sceneCornerCoord1Lon;
  meta->location->lat_start_far_range = terrasar->sceneCornerCoord2Lat;
  meta->location->lon_start_far_range = terrasar->sceneCornerCoord2Lon;
  meta->location->lat_end_near_range = terrasar->sceneCornerCoord3Lat;
  meta->location->lon_end_near_range = terrasar->sceneCornerCoord3Lon;
  meta->location->lat_end_far_range = terrasar->sceneCornerCoord4Lat;
  meta->location->lon_end_far_range = terrasar->sceneCornerCoord4Lon;

  return meta;
}
