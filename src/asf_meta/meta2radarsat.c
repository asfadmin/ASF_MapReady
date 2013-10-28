#include "asf_meta.h"
#include "doppler.h"
#include "meta_init.h"
#include "dateUtil.h"
#include "radarsat2.h"
#include "xml_util.h"
#include "asf_nan.h"

radarsat2_meta *radarsat2_meta_init(void)
{
  radarsat2_meta *radarsat2;
  radarsat2 = (radarsat2_meta *) CALLOC(1, sizeof(radarsat2_meta));
 
  // general block
  strcpy(radarsat2->filename, MAGIC_UNSET_STRING);
  strcpy(radarsat2->satellite, MAGIC_UNSET_STRING);
  strcpy(radarsat2->sensor, MAGIC_UNSET_STRING);
  strcpy(radarsat2->beamModeMnemonic, MAGIC_UNSET_STRING);
  strcpy(radarsat2->acquisitionType, MAGIC_UNSET_STRING);
  strcpy(radarsat2->productType, MAGIC_UNSET_STRING);
  strcpy(radarsat2->dataType, MAGIC_UNSET_STRING);
  strcpy(radarsat2->processingFacility, MAGIC_UNSET_STRING);
  strcpy(radarsat2->softwareVersion, MAGIC_UNSET_STRING);
  radarsat2->bitsPerSample = MAGIC_UNSET_INT;
  //radarsat2->absOrbit = MAGIC_UNSET_INT;
  strcpy(radarsat2->passDirection, MAGIC_UNSET_STRING);
  radarsat2->band_count = MAGIC_UNSET_INT;
  strcpy(radarsat2->bands, MAGIC_UNSET_STRING);
  radarsat2->numberOfLines = MAGIC_UNSET_INT;
  radarsat2->numberOfSamplesPerLine = MAGIC_UNSET_INT;
  radarsat2->sampledPixelSpacing = MAGIC_UNSET_DOUBLE;
  radarsat2->sampledLineSpacing = MAGIC_UNSET_DOUBLE;
  radarsat2->semiMajorAxis = MAGIC_UNSET_DOUBLE;
  radarsat2->semiMinorAxis = MAGIC_UNSET_DOUBLE;
  strcpy(radarsat2->lineTimeOrdering, MAGIC_UNSET_STRING);
  strcpy(radarsat2->pixelTimeOrdering, MAGIC_UNSET_STRING);

  // SAR block
  strcpy(radarsat2->antennaPointing, MAGIC_UNSET_STRING);
  radarsat2->numberOfAzimuthLooks = MAGIC_UNSET_INT;
  radarsat2->numberOfRangeLooks = MAGIC_UNSET_INT;
  //strcpy(radarsat2->imageCoordinateType, MAGIC_UNSET_STRING);
  radarsat2->slantRangeNearEdge = MAGIC_UNSET_DOUBLE;
  radarsat2->radarCenterFrequency = MAGIC_UNSET_DOUBLE;
  radarsat2->pulseRepetitionFrequency = MAGIC_UNSET_DOUBLE;
  radarsat2->totalProcessedAzimuthBandwidth = MAGIC_UNSET_DOUBLE;
  radarsat2->satelliteHeight = MAGIC_UNSET_DOUBLE;
  radarsat2->pulseLength = MAGIC_UNSET_DOUBLE;
  radarsat2->adcSamplingRate = MAGIC_UNSET_DOUBLE;
  strcpy(radarsat2->polarizations, MAGIC_UNSET_STRING);

  // Doppler block
  radarsat2->doppler = NULL;

  // state vectors
  strcpy(radarsat2->zeroDopplerTimeFirstLine, MAGIC_UNSET_STRING);
  strcpy(radarsat2->zeroDopplerTimeLastLine, MAGIC_UNSET_STRING);
  radarsat2->state_vectors = NULL;
  
  // location block
  radarsat2->sceneCornerCoord1Lat = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord1Lon = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord2Lat = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord2Lon = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord3Lat = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord3Lon = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord4Lat = MAGIC_UNSET_DOUBLE;
  radarsat2->sceneCornerCoord4Lon = MAGIC_UNSET_DOUBLE;

  // calibration block
  radarsat2->offset = MAGIC_UNSET_DOUBLE;

  return radarsat2;
}

static int getNumParamsInString (char *str)
{
  int ii, count=0;
 
  for (ii=0; ii<strlen(str); ii++) {
    if (str[ii] == ' ')
      count++;
  }

  return count+1;
}

radarsat2_doppler_params *radarsat2_doppler_init(int numDopplerEstimates)
{
  radarsat2_doppler_params *r2;
  int ii, dc;

  dc = numDopplerEstimates > 0 ? numDopplerEstimates : 0;
  r2 = (radarsat2_doppler_params *) MALLOC(sizeof(radarsat2_doppler_params));
  r2->centroid = (double *) MALLOC(sizeof(double) * dc);
  r2->rate = (double *) MALLOC(sizeof(double) * dc);

  r2->doppler_count = dc;
  r2->time_first_sample = MAGIC_UNSET_DOUBLE;
  for (ii=0; ii<r2->doppler_count; ii++) {
    r2->centroid[ii] = MAGIC_UNSET_DOUBLE;
    r2->rate[ii] = MAGIC_UNSET_DOUBLE;
  }

  return r2;
}

static double *read_radarsat2_lut(const char *dataFile, int gain_count)
{
  int ii;
  char *gainStr = (char *) MALLOC(sizeof(char)*100000);
  char *p, *q;
  double *gain = (double *) MALLOC(sizeof(double)*gain_count);
  xmlDoc *doc = xmlReadFile(dataFile, NULL, 0);
  strcpy(gainStr, xml_get_string_value(doc, "lut.gains"));
  p = gainStr;
  for (ii=0; ii<gain_count; ii++) {
    if (ii == 0)
      q = p;
    else {
      if (strchr(p, ' ')) {
	q = strchr(p, ' ');
	q++;
      }
    }
    sscanf(q, "%lf", &gain[ii]);
    p = q;
  } 
  FREE(gainStr);
  xmlFreeDoc(doc);
  xmlCleanupParser();
  
  return gain;
}

radarsat2_meta *read_radarsat2_meta(const char *dataFile)
{
  int ii, numStateVectors, numDopplerEstimates;
  ymd_date imgStartDate, date;
  hms_time imgStartTime, time;
  julian_date julianDate;
  char timeStr[30], str[150];
  radarsat2_doppler_params *r2_doppler;

  radarsat2_meta *radarsat2 = radarsat2_meta_init();

  if (!fileExists(dataFile))
    asfPrintError("Metadata file (%s) does not exist!\n", dataFile);
  char *path = (char *) MALLOC(sizeof(char)*512);
  char *file = (char *) MALLOC(sizeof(char)*128);
  split_dir_and_file(dataFile, path, file);

  xmlDoc *doc = xmlReadFile(dataFile, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", dataFile);

  strcpy(radarsat2->satellite, xml_get_string_value(doc, 
     "product.sourceAttributes.satellite"));
  strcpy(radarsat2->sensor, xml_get_string_value(doc, 
     "product.sourceAttributes.sensor"));
  strcpy(radarsat2->beamModeMnemonic, xml_get_string_value(doc,
     "product.sourceAttributes.beamModeMnemonic"));
  strcpy(radarsat2->acquisitionType, xml_get_string_value(doc, 
     "product.sourceAttributes.radarParameters.acquisitionType"));
  strcpy(radarsat2->productType, xml_get_string_value(doc, 
     "product.imageGenerationParameters.generalProcessingInformation."
     "productType"));
  strcpy(radarsat2->dataType, xml_get_string_value(doc, 
     "product.imageAttributes.rasterAttributes.dataType"));
  strcpy(radarsat2->processingFacility, xml_get_string_value(doc, 
     "product.imageGenerationParameters.generalProcessingInformation."
     "processingFacility"));
  strcpy(radarsat2->zeroDopplerAzimuthTime, xml_get_string_value(doc, 
     "product.imageGenerationParameters.slantRangeToGroundRange."
     "zeroDopplerAzimuthTime"));
  strcpy(radarsat2->softwareVersion, xml_get_string_value(doc, 
     "product.imageGenerationParameters.generalProcessingInformation."
     "softwareVersion"));
  radarsat2->bitsPerSample = xml_get_int_value(doc, 
     "product.productInfo.imageDataInfo.imageDataDepth");
  //radarsat2->absOrbit = 
  //xml_get_int_value(doc, "product.productInfo.missionInfo.absOrbit");
  strcpy(radarsat2->passDirection, xml_get_string_value(doc, 
     "product.sourceAttributes.orbitAndAttitude.orbitInformation."
     "passDirection"));
  // Number of bands needs to be derived from polarizations string
  int band_count = 0;
  char *attribute = (char *) MALLOC(sizeof(char)*128);
  char *fileName = (char *) MALLOC(sizeof(char)*512);
  strcpy(radarsat2->polarizations, xml_get_string_value(doc, 
     "product.sourceAttributes.radarParameters.polarizations"));
  for (ii=0; ii<strlen(radarsat2->polarizations)-1; ii++)
    if (radarsat2->polarizations[ii] == ' ')
      radarsat2->polarizations[ii] = ',';
  if (strstr(radarsat2->polarizations, "HH"))
    band_count++;
  if (strstr(radarsat2->polarizations, "VV"))
    band_count++;
  if (strstr(radarsat2->polarizations, "HV"))
    band_count++;
  if (strstr(radarsat2->polarizations, "VH"))
    band_count++;
  radarsat2->band_count = band_count;
  strcpy(radarsat2->filename, "");
  strcpy(radarsat2->bands, "");
  // Park the filenames in the basename field of the metadata and replace
  // it with the directory name once we are done with importing the data
  for (ii=0; ii<band_count; ii++) {
    sprintf(str, 
	    "product.imageAttributes.fullResolutionImageData[%d].pole", ii);
    strcpy(attribute, xml_get_string_attribute(doc, str));
    sprintf(str, "product.imageAttributes.fullResolutionImageData[%d]", ii);
    strcpy(fileName, xml_get_string_value(doc, str));
    if (ii == 0) {
      sprintf(radarsat2->filename, "%s", fileName);
      sprintf(radarsat2->bands, "AMP-%s,PHASE-%s", 
	      uc(attribute), uc(attribute));
    }
    else {
      strcat(radarsat2->filename, ",");
      strcat(radarsat2->filename, fileName);
      strcat(radarsat2->bands, ",");
      sprintf(str, "AMP-%s,PHASE-%s", uc(attribute), uc(attribute));
      strcat(radarsat2->bands, str);
    }
  }
  FREE(fileName);
  FREE(attribute);
  radarsat2->numberOfLines = xml_get_int_value(doc, 
     "product.imageAttributes.rasterAttributes.numberOfLines");
  radarsat2->numberOfSamplesPerLine = xml_get_int_value(doc, 
     "product.imageAttributes.rasterAttributes.numberOfSamplesPerLine");
  radarsat2->sampledPixelSpacing = xml_get_double_value(doc, 
     "product.imageAttributes.rasterAttributes.sampledPixelSpacing");
  radarsat2->sampledLineSpacing = xml_get_double_value(doc, 
     "product.imageAttributes.rasterAttributes.sampledLineSpacing");
  radarsat2->semiMajorAxis = xml_get_double_value(doc, 
     "product.imageAttributes.geographicInformation."
     "referenceEllipsoidParameters.semiMajorAxis");
  radarsat2->semiMinorAxis = xml_get_double_value(doc, 
     "product.imageAttributes.geographicInformation."
     "referenceEllipsoidParameters.semiMinorAxis");
  strcpy(radarsat2->lineTimeOrdering, xml_get_string_value(doc, 
     "product.imageAttributes.rasterAttributes.lineTimeOrdering"));
  strcpy(radarsat2->pixelTimeOrdering, xml_get_string_value(doc, 
     "product.imageAttributes.rasterAttributes.pixelTimeOrdering"));
  strcpy(radarsat2->antennaPointing, xml_get_string_value(doc, 
     "product.sourceAttributes.radarParameters.antennaPointing"));
  radarsat2->numberOfAzimuthLooks = xml_get_int_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "numberOfAzimuthLooks");
  radarsat2->numberOfRangeLooks = xml_get_int_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "numberOfRangeLooks");
  radarsat2->slantRangeNearEdge = xml_get_double_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "slantRangeNearEdge");
  radarsat2->radarCenterFrequency = xml_get_double_value(doc, 
     "product.sourceAttributes.radarParameters.radarCenterFrequency");
  radarsat2->pulseRepetitionFrequency = xml_get_double_value(doc, 
     "product.sourceAttributes.radarParameters.pulseRepetitionFrequency");
  radarsat2->satelliteHeight = xml_get_double_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "satelliteHeight");
  radarsat2->totalProcessedAzimuthBandwidth = xml_get_double_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "totalProcessedAzimuthBandwidth");
  // chirp rate ???
  radarsat2->pulseLength = xml_get_double_value(doc, 
     "product.sourceAttributes.radarParameters.pulseLength");
  radarsat2->adcSamplingRate = xml_get_double_value(doc, 
     "product.sourceAttributes.radarParameters.adcSamplingRate");
  // pitch, roll, yaw ???

  // read Doppler values
  radarsat2->doppler = meta_doppler_init();
  radarsat2->doppler->type = radarsat2_doppler;
  char *dopplerCentroidStr;
  dopplerCentroidStr = (char *) MALLOC(sizeof(char)*512);
  strcpy(dopplerCentroidStr, xml_get_string_value(doc,
     "product.imageGenerationParameters.dopplerCentroid."
     "dopplerCentroidCoefficients"));
  numDopplerEstimates = getNumParamsInString(dopplerCentroidStr);
  r2_doppler = radarsat2_doppler_init(numDopplerEstimates);
  radarsat2->doppler->r2 = r2_doppler;
  r2_doppler->ref_time_centroid = xml_get_double_value(doc, 
     "product.imageGenerationParameters.dopplerCentroid."
     "dopplerCentroidReferenceTime");
  r2_doppler->ref_time_rate = xml_get_double_value(doc, 
     "product.imageGenerationParameters.dopplerRateValues."
     "dopplerRateReferenceTime");
  char *dopplerRateStr;
  dopplerRateStr = (char *) MALLOC(sizeof(char)*512);
  strcpy(dopplerRateStr, xml_get_string_value(doc,
     "product.imageGenerationParameters.dopplerRateValues."
     "dopplerRateValuesCoefficients"));
  r2_doppler->time_first_sample = xml_get_double_value(doc,
     "product.imageGenerationParameters.slantRangeToGroundRange."
      "slantRangeTimeToFirstRangeSample");
  char *p, *q;
  p = dopplerCentroidStr;
  for (ii=0; ii<numDopplerEstimates; ii++) {
    if (ii == 0)
      q = p;
    else {
      if (strchr(p, ' ')) {
	q = strchr(p, ' ');
	q++;
      }
    }
    sscanf(q, "%lf", &r2_doppler->centroid[ii]);
    p = q;
  } 
  FREE(dopplerCentroidStr);
  p = dopplerRateStr;
  for (ii=0; ii<numDopplerEstimates; ii++) {
    if (ii == 0)
      q = p;
    else {
      if (strchr(p, ' ')) {
	q = strchr(p, ' ');
	q++;
      }
    }
    sscanf(q, "%lf", &r2_doppler->rate[ii]);
    p = q;
  } 
  FREE(dopplerRateStr);

  // read state vectors
  strcpy(radarsat2->zeroDopplerTimeFirstLine, xml_get_string_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "zeroDopplerTimeFirstLine"));
  date_terrasar2date(radarsat2->zeroDopplerTimeFirstLine, 
		     &imgStartDate, &imgStartTime);
  strcpy(radarsat2->zeroDopplerTimeLastLine, xml_get_string_value(doc, 
     "product.imageGenerationParameters.sarProcessingInformation."
     "zeroDopplerTimeLastLine"));
  // Accommodate data stored in reverse time
  if (strcmp_case(radarsat2->lineTimeOrdering, "DECREASING") == 0)
    date_terrasar2date(radarsat2->zeroDopplerTimeLastLine, 
		       &imgStartDate, &imgStartTime);
  
  // FIXME: determine numStateVector from data - count the entries
  //numStateVectors = xml_get_int_value(doc, 
  //   "product.platform.orbit.orbitHeader.numStateVectors");
  numStateVectors = 5;
  radarsat2->state_vectors = meta_state_vectors_init(numStateVectors);
  radarsat2->state_vectors->year = imgStartDate.year;
  date_ymd2jd(&imgStartDate, &julianDate);
  radarsat2->state_vectors->julDay = julianDate.jd;
  radarsat2->state_vectors->second = date_hms2sec(&imgStartTime);
  for (ii=0; ii<numStateVectors; ii++) {
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].timeStamp", ii);
    strcpy(timeStr, xml_get_string_value(doc, str));
    date_terrasar2date(timeStr, &date, &time);
    radarsat2->state_vectors->vecs[ii].time =
      time_difference(&date, &time, &imgStartDate, &imgStartTime);
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].xPosition", ii);
    radarsat2->state_vectors->vecs[ii].vec.pos.x = 
      xml_get_double_value(doc, str);
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].yPosition", ii);
    radarsat2->state_vectors->vecs[ii].vec.pos.y = 
      xml_get_double_value(doc, str);
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].zPosition", ii);
    radarsat2->state_vectors->vecs[ii].vec.pos.z = 
      xml_get_double_value(doc, str);
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].xVelocity", ii);
    radarsat2->state_vectors->vecs[ii].vec.vel.x = 
      xml_get_double_value(doc, str);
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].yVelocity", ii);
    radarsat2->state_vectors->vecs[ii].vec.vel.y = 
      xml_get_double_value(doc, str);
    sprintf(str, "product.sourceAttributes.orbitAndAttitude.orbitInformation."
	    "stateVector[%d].zVelocity", ii);
    radarsat2->state_vectors->vecs[ii].vec.vel.z = 
      xml_get_double_value(doc, str);
  }

  // read location information from the tie points
  ii = 0;
  int line=-99, pixel=-99, found=TRUE;
  while (found) {
    sprintf(str, "product.imageAttributes.geographicInformation."
	    "geolocationGrid.imageTiePoint[%d].imageCoordinate.line", ii);
    line = (int) xml_get_double_value(doc, str);
    sprintf(str, "product.imageAttributes.geographicInformation."
	    "geolocationGrid.imageTiePoint[%d].imageCoordinate.pixel", ii);
    pixel = (int) xml_get_double_value(doc, str);
    if (line < 0 || pixel < 0)
      found = FALSE;
    if (found) {
      if (line == 0 && pixel == 0) {
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"latitude", ii);
	radarsat2->sceneCornerCoord1Lat = xml_get_double_value(doc, str);
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"longitude", ii);
	radarsat2->sceneCornerCoord1Lon = xml_get_double_value(doc, str);
      }
      if (line == 0 && pixel == radarsat2->numberOfSamplesPerLine-1) {
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"latitude", ii);
	radarsat2->sceneCornerCoord2Lat = xml_get_double_value(doc, str);
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"longitude", ii);
	radarsat2->sceneCornerCoord2Lon = xml_get_double_value(doc, str);
      }
      if (line == radarsat2->numberOfLines-1 && pixel == 0) {
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"latitude", ii);
	radarsat2->sceneCornerCoord3Lat = xml_get_double_value(doc, str);
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"longitude", ii);
	radarsat2->sceneCornerCoord3Lon = xml_get_double_value(doc, str);
      }
      if (line == radarsat2->numberOfLines-1 && 
	  pixel == radarsat2->numberOfSamplesPerLine-1) {
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"latitude", ii);
	radarsat2->sceneCornerCoord4Lat = xml_get_double_value(doc, str);
	sprintf(str, "product.imageAttributes.geographicInformation."
		"geolocationGrid.imageTiePoint[%d].geodeticCoordinate."
		"longitude", ii);
	radarsat2->sceneCornerCoord4Lon = xml_get_double_value(doc, str);
      }
    }
    ii++;
  }

  // Read calibration information
  double sample_count = radarsat2->numberOfSamplesPerLine;
  attribute = (char *) MALLOC(sizeof(char)*128);
  fileName = (char *) MALLOC(sizeof(char)*512);
  for (ii=0; ii<3; ii++) {
    sprintf(str, 
	    "product.imageAttributes.lookupTable[%d].incidenceAngleCorrection", 
	    ii);
    strcpy(attribute, xml_get_string_attribute(doc, str));
    sprintf(str, "product.imageAttributes.lookupTable[%d]", ii);
    if (strlen(path) > 0)
      sprintf(fileName, "%s%s", path, xml_get_string_value(doc, str));
    else
      strcpy(fileName, xml_get_string_value(doc, str));
    if (strcmp_case(attribute, "Beta Nought") == 0)
      radarsat2->gains_beta = read_radarsat2_lut(fileName, sample_count);
    else if (strcmp_case(attribute, "Sigma Nought") == 0)
      radarsat2->gains_sigma = read_radarsat2_lut(fileName, sample_count);
    else if (strcmp_case(attribute, "Gamma") == 0)
      radarsat2->gains_gamma = read_radarsat2_lut(fileName, sample_count);
  }
  FREE(attribute);
  FREE(fileName);

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return radarsat2;
}

meta_parameters* radarsat2meta(radarsat2_meta *radarsat2)
{

  ymd_date date, imgStartDate, imgStopDate;
  hms_time time, imgStartTime, imgStopTime;
  meta_parameters *meta;
  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  double lat, lon, height, re, rp;

  // Allocate memory for metadata structure
  meta = raw_init();

  // General block
  strcpy(meta->general->basename, radarsat2->filename);
  strcpy(meta->general->sensor, radarsat2->satellite);
  strcpy(meta->general->sensor_name, radarsat2->sensor);
  strcpy(meta->general->mode, radarsat2->beamModeMnemonic);
  sprintf(meta->general->processor, "%s %s", 
	  radarsat2->processingFacility, radarsat2->softwareVersion);
  meta->general->data_type = REAL32;
  if (strcmp_case(radarsat2->dataType, "COMPLEX") == 0)
    meta->general->image_data_type = COMPLEX_IMAGE;
  //else if (strcmp_case(radarsat2->dataType, "DETECTED") == 0)
  //  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  date_terrasar2date(radarsat2->zeroDopplerAzimuthTime, &date, &time);
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0f",
	  date.day, mon[date.month], date.year, time.hour, time.min, time.sec);
  //meta->general->orbit = radarsat2->absOrbit;
  if (strcmp_case(radarsat2->passDirection, "ASCENDING") == 0)
    meta->general->orbit_direction = 'A';
  else if (strcmp_case(radarsat2->passDirection, "DESCENDING") == 0)
    meta->general->orbit_direction = 'D';
  if (strcmp_case(radarsat2->dataType, "COMPLEX") == 0)
    meta->general->band_count = radarsat2->band_count * 2;
  else
    meta->general->band_count = radarsat2->band_count;
  strcpy(meta->general->bands, radarsat2->bands);
  meta->general->line_count = radarsat2->numberOfLines;
  meta->general->sample_count = radarsat2->numberOfSamplesPerLine;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->x_pixel_size = radarsat2->sampledPixelSpacing;
  meta->general->y_pixel_size = radarsat2->sampledLineSpacing;
  //meta->general->center_latitude = radarsat2->sceneCenterCoordLat;
  //meta->general->center_longitude = radarsat2->sceneCenterCoordLon;
  meta->general->re_major = radarsat2->semiMajorAxis;
  meta->general->re_minor = radarsat2->semiMinorAxis;

  // SAR block
  meta->sar = meta_sar_init();
  if (strcmp_case(radarsat2->productType, "SLC") == 0)
    meta->sar->image_type = 'S';
  if (strcmp_case(radarsat2->antennaPointing, "LEFT") == 0)
    meta->sar->look_direction = 'L';
  else if (strcmp_case(radarsat2->antennaPointing, "RIGHT") == 0)
    meta->sar->look_direction = 'R';
  meta->sar->azimuth_look_count = radarsat2->numberOfAzimuthLooks;
  meta->sar->range_look_count = radarsat2->numberOfRangeLooks;
  meta->sar->deskewed = 1;
  meta->sar->original_line_count = meta->general->line_count;
  meta->sar->original_sample_count = meta->general->sample_count;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  date_terrasar2date(radarsat2->zeroDopplerTimeFirstLine, 
		     &imgStartDate, &imgStartTime);
  date_terrasar2date(radarsat2->zeroDopplerTimeLastLine, 
		     &imgStopDate, &imgStopTime);
  meta->sar->azimuth_time_per_pixel =
    date_difference(&imgStopDate, &imgStopTime, 
		    &imgStartDate, &imgStartTime) / meta->general->line_count;
  meta->sar->range_time_per_pixel = 
    meta->general->x_pixel_size * 2.0 / speedOfLight;
  meta->sar->slant_range_first_pixel = radarsat2->slantRangeNearEdge;
  meta->sar->slant_shift = 0.0;
  meta->sar->time_shift = 0.0;
  meta->sar->wavelength = SPD_LIGHT / radarsat2->radarCenterFrequency;
  meta->sar->prf = radarsat2->pulseRepetitionFrequency;
  meta->sar->satellite_height = radarsat2->satelliteHeight;
  meta->sar->azimuth_processing_bandwidth = 
    radarsat2->totalProcessedAzimuthBandwidth;
  // FIXME: chirp_rate ???
  meta->sar->pulse_duration = radarsat2->pulseLength;
  meta->sar->range_sampling_rate = radarsat2->adcSamplingRate;
  if (strcmp_case(radarsat2->polarizations, "HH,VV,HV,VH") == 0) {
    meta->general->image_data_type = POLARIMETRIC_IMAGE;
    strcpy(meta->sar->polarization, "quad-pol");
  }
  else
    strcpy(meta->sar->polarization, radarsat2->polarizations);
  if (strcmp_case(radarsat2->dataType, "COMPLEX") == 0)
    meta->sar->multilook = FALSE;
  // FIXME: pitch, roll, yaw ???

  // Doppler block
  meta->doppler = radarsat2->doppler;

  // State vectors
  meta->state_vectors = radarsat2->state_vectors;

  // Propagate the state vectors to start, center, end
  int vector_count = 3;
  double data_int = date_difference(&imgStopDate, &imgStopTime, 
				    &imgStartDate, &imgStartTime) / 2.0;
  while (fabs(data_int) > 10.0) {
    data_int /= 2;
    vector_count = vector_count*2-1;
  }
  propagate_state(meta, vector_count, data_int);

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = radarsat2->sceneCornerCoord1Lat;
  meta->location->lon_start_near_range = radarsat2->sceneCornerCoord1Lon;
  meta->location->lat_start_far_range = radarsat2->sceneCornerCoord2Lat;
  meta->location->lon_start_far_range = radarsat2->sceneCornerCoord2Lon;
  meta->location->lat_end_near_range = radarsat2->sceneCornerCoord3Lat;
  meta->location->lon_end_near_range = radarsat2->sceneCornerCoord3Lon;
  meta->location->lat_end_far_range = radarsat2->sceneCornerCoord4Lat;
  meta->location->lon_end_far_range = radarsat2->sceneCornerCoord4Lon;

  // Still need to determine center location, really only needed to get
  // the earth radius straight
  meta->general->center_longitude = (radarsat2->sceneCornerCoord1Lon +
				     radarsat2->sceneCornerCoord2Lon +
				     radarsat2->sceneCornerCoord3Lon +
				     radarsat2->sceneCornerCoord4Lon) / 4.0;
  location_to_latlon(meta, meta->general->sample_count/2, 
		     meta->general->line_count/2, 0.0, &lat, &lon, &height);
  meta->general->center_latitude = lat;
  meta->general->center_longitude = lon;
  lat = meta->general->center_latitude * D2R;
  re = meta->general->re_major;
  rp = meta->general->re_minor;
  meta->sar->earth_radius = 
    (re*rp) / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));
  meta->sar->satellite_height += meta->sar->earth_radius;

  return meta;
}
