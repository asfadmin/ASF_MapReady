#include "radarsat2.h"
#include "doppler.h"
#include "asf.h"
#include "asf_nan.h"
#include "asf_import.h"
#include "asf_meta.h"
#include "asf_raster.h"
#include "asf_endian.h"
#include "dateUtil.h"
#include "xml_util.h"
#include <ctype.h>
#include "asf_tiff.h"
#include "geotiff_support.h"

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

  return radarsat2;
}

static int getNumParamsInString (char *str)
{
  int ii, count=0;
 
  for (ii>0; ii<strlen(str); ii++) {
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

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return radarsat2;
}

void import_radarsat2(const char *inBaseName, radiometry_t radiometry,
		      const char *outBaseName, int ampOnly)
{
  FILE *fp;
  radarsat2_meta *radarsat2;
  meta_parameters *meta;
  char **inDataNames=NULL, inDataName[1024], *inMetaName=NULL;
  char *outDataName=NULL, str[512];
  float *amp = NULL, *phase = NULL, *tmp = NULL, re, im;
  int band, sample;

  // Check radiometry
  if (radiometry != r_AMP) {
    asfPrintWarning("Radiometry other than AMPLITUDE is currently not "
		    "supported.\n");
    radiometry = r_AMP;
  }
  
  if (!fileExists(inBaseName))
    inMetaName = appendExt(inBaseName, ".xml");
  else {
    inMetaName = (char *) MALLOC(sizeof(char)*1024);
    strcpy(inMetaName, inBaseName);
  }
  outDataName = appendExt(outBaseName, ".img");

  radarsat2 = read_radarsat2_meta(inMetaName);
  asfPrintStatus("   DataType: %s, ProductType: %s\n",
		 radarsat2->dataType, radarsat2->productType);
  if (strcmp_case(radarsat2->dataType, "COMPLEX") != 0)
    asfPrintError("Currently only complex data supported!\n");
  meta = radarsat2meta(radarsat2);
  meta_write(meta, outDataName);

  // Let's check the GeoTIFF data.
  // Unfortunately, there is no identifier in the GeoTIFF that would identify
  // the data as Radarsat-2 data.
  //
  // The only thing that we can actually do is to look whether the data in the
  // GeoTIFF file fit the general bill. We can the image dimensions. The data
  // needs to have 2 bands (I and Q) and 16 bit. The citation geokey needs to
  // be the really non-descriptive "Uncorrected Satellite Data".

  TIFF *tiff = NULL;
  GTIF *gtif = NULL;
  data_type_t data_type;
  short sample_format, bits_per_sample, planar_config;
  short num_bands;
  int is_scanline_format, is_palette_color_tiff, wrong=FALSE;
  char *error_message = (char *) MALLOC(sizeof(char)*2048);

  inDataNames = extract_band_names(meta->general->basename, 
				   meta->general->band_count);
  fp = FOPEN(outDataName, "wb");

  int band_count = radarsat2->band_count;
  if (ampOnly) {
    strcpy(meta->general->bands, "AMP");
    meta->general->band_count = 1;
    band_count = 1;
  }
  for (band=0; band<band_count; band++) {

    // path from the xml (metadata) file
    char *path = get_dirname(inBaseName);
    if (strlen(path)>0) {
      strcpy(inDataName, path);
      if (inDataName[strlen(inDataName)-1] != '/')
        strcat(inDataName, "/");
    }
    else
      strcpy(inDataName, "");
    free(path);
    strcat(inDataName, inDataNames[band]);

    tiff = XTIFFOpen(inDataName, "r");
    if (!tiff)
      asfPrintError("Could not open data file (%s)\n", inDataName);
    gtif = GTIFNew(tiff);
    if (!gtif)
      asfPrintError("Could not read GeoTIFF keys from data file (%s)\n",
		    inDataName);

    // Check image dimensions
    uint32 tif_sample_count;
    uint32 tif_line_count;

    TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &tif_line_count);
    TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &tif_sample_count);
    if ((meta->general->sample_count != tif_sample_count) ||
	(meta->general->line_count != tif_line_count))
      asfPrintError(error_message, 
		    "Problem with image dimensions. Was looking for %d lines "
		    "and %d samples.\nFound %ld lines and %ld samples instead!"
		    "\n", 
		    meta->general->line_count, meta->general->sample_count, 
		    tif_line_count, tif_sample_count);

    // Check general TIFF tags
    get_tiff_data_config(tiff, &sample_format, &bits_per_sample, &planar_config,
			 &data_type, &num_bands, &is_scanline_format, 
			 &is_palette_color_tiff, REPORT_LEVEL_WARNING);

    // The specs say the data is supposed to be unsigned but it is not.
    // Let is pass as long as we are talking about integer data here
    strcpy(error_message, "");
    if (sample_format != SAMPLEFORMAT_UINT && 
	sample_format != SAMPLEFORMAT_INT) {
      strcat(error_message, 
	     "Problem with sampling format. Was looking for integer, ");
      if (sample_format == SAMPLEFORMAT_COMPLEXIEEEFP)
	strcat(error_message, "found complex floating point instead!\n");
      else if (sample_format == SAMPLEFORMAT_COMPLEXINT)
	strcat(error_message, "found complex integer instead!\n");
      else if (sample_format == SAMPLEFORMAT_IEEEFP)
	strcat(error_message, "found floating point instead!\n");
      else if (sample_format == SAMPLEFORMAT_VOID)
	strcat(error_message, "found void instead!\n");
      wrong = TRUE;
    }
    if (bits_per_sample != 16) {
      sprintf(str, "Problem with bits per sample. Was looking for 16, found %d "
	      "instead!\n", bits_per_sample);
      strcat(error_message, str);
      wrong = TRUE;
    }
    if (data_type != INTEGER16) {
      strcat(error_message, "Problem with data type. Was looking INTEGER16, ");
      if (data_type == BYTE)
	strcat(error_message, "found BYTE instead!\n");
      else if (data_type == INTEGER32)
	strcat(error_message, "found INTEGER32 instead!\n");
      else if (data_type == REAL32)
	strcat(error_message, "found REAL32 instead!\n");
      else if (data_type == REAL64)
	strcat(error_message, "found REAL64 instead!\n");
      else if (data_type == COMPLEX_BYTE)
	strcat(error_message, "found COMPLEX_BYTE instead!\n");
      else if (data_type == COMPLEX_INTEGER16)
	strcat(error_message, "found COMPLEX_INTEGER16 instead!\n");
      else if (data_type == COMPLEX_INTEGER32)
	strcat(error_message, "found COMPLEX_INTEGER32 instead!\n");
      else if (data_type == COMPLEX_REAL32)
	strcat(error_message, "found COMPLEX_REAL32 instead!\n");
      else if (data_type == COMPLEX_REAL64)
	strcat(error_message, "found COMPLEX_REAL64 instead!\n");
      wrong = TRUE;
    }
    if (num_bands != 2) {
      sprintf(str, "Problem with number of bands. Was looking for 2, "
	      "found %d instead!\n", num_bands);
      strcat(error_message, str);
      wrong = TRUE;
    }
    if (wrong)
      asfPrintError(error_message);

    // Check GTCitationGeoKey
    char *citation = NULL;
    int citation_length, typeSize;
    tagtype_t citation_type;

    citation_length = GTIFKeyInfo(gtif, GTCitationGeoKey, &typeSize,
				  &citation_type);
    if (citation_length > 0) {
      citation = (char *) MALLOC(citation_length * typeSize);
      GTIFKeyGet(gtif, GTCitationGeoKey, citation, 0, citation_length);
      if (citation && 
	  strcmp_case(citation, "UNCORRECTED SATELLITE DATA") != 0) {
	asfPrintError("Problem with GTCitationGeoKey. Was looking for "
		      "'Uncorrected Satellite Data',\nfound '%s' instead!\n", 
		      citation);
      }
    }
    else
      asfPrintError("Problem with GTCitationGeoKey. Was looking for "
		    "'Uncorrected Satellite Data',\ndid not find any key!\n");

    tiff_type_t tiffInfo;
    get_tiff_type(tiff, &tiffInfo);
    if (tiffInfo.format != SCANLINE_TIFF &&
	tiffInfo.format != STRIP_TIFF    &&
	tiffInfo.format != TILED_TIFF)
      asfPrintError("Can't read the GeoTIFF file (%s). Unrecognized TIFF "
		    "type!\n", inDataNames[band]);

    // If we made it here, we are reasonably sure that we have the file that
    // we are looking for.
    asfPrintStatus("\n   Importing %s ...\n", inDataNames[band]);

    uint32 scanlineSize = TIFFScanlineSize(tiff);
    tdata_t *tiff_real_buf = _TIFFmalloc(scanlineSize);
    tdata_t *tiff_imag_buf = _TIFFmalloc(scanlineSize);
    if (!tiff_real_buf || !tiff_imag_buf)
      asfPrintError("Can't allocate buffer for reading TIFF lines!\n");

    amp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);
    phase = (float *) MALLOC(sizeof(float)*meta->general->sample_count);

    // Check whether we need to flip the image in any fashion
    int flip_vertical = FALSE;
    if (strcmp_case(radarsat2->lineTimeOrdering, "DECREASING") == 0) {
      asfPrintStatus("   Data will be flipped vertically while ingesting!\n");
      flip_vertical = TRUE;
    }
    int flip_horizontal = FALSE;
    if (strcmp_case(radarsat2->pixelTimeOrdering, "DECREASING") == 0) {
      asfPrintStatus("   Data will be flipped horizontally while ingesting!\n");
      flip_horizontal = TRUE;
    }
    if (flip_horizontal)
      tmp = (float *) MALLOC(sizeof(float)*meta->general->sample_count);

    // FIXME: still need to implement flipping vertically
    // Read file line by line
    uint32 row;
    int sample_count = meta->general->sample_count;
    int line_count = meta->general->line_count;
    for (row=0; row<(uint32)meta->general->line_count; row++) {
      asfLineMeter(row, meta->general->line_count);
      if (flip_vertical) {
	switch (tiffInfo.format) 
	  {
	  case SCANLINE_TIFF:
	    TIFFReadScanline(tiff, tiff_real_buf, line_count-row-1, 0);
	    TIFFReadScanline(tiff, tiff_imag_buf, line_count-row-1, 1);
	    break;
	  case STRIP_TIFF:
	    ReadScanline_from_TIFF_Strip(tiff, tiff_real_buf, 
					 line_count-row-1, 0);
	    ReadScanline_from_TIFF_Strip(tiff, tiff_imag_buf, 
					 line_count-row-1, 1);
	    break;
	  case TILED_TIFF:
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_real_buf, 
					   line_count-row-1, 0);
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_imag_buf, 
					   line_count-row-1, 1);
	    break;
	  default:
	    asfPrintError("Can't read this TIFF format!\n");
	    break;
	  }
      }
      else {
	switch (tiffInfo.format) 
	  {
	  case SCANLINE_TIFF:
	    TIFFReadScanline(tiff, tiff_real_buf, row, 0);
	    TIFFReadScanline(tiff, tiff_imag_buf, row, 1);
	    break;
	  case STRIP_TIFF:
	    ReadScanline_from_TIFF_Strip(tiff, tiff_real_buf, row, 0);
	    ReadScanline_from_TIFF_Strip(tiff, tiff_imag_buf, row, 1);
	    break;
	  case TILED_TIFF:
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_real_buf, row, 0);
	    ReadScanline_from_TIFF_TileRow(tiff, tiff_imag_buf, row, 1);
	    break;
	  default:
	    asfPrintError("Can't read this TIFF format!\n");
	    break;
	  }
      }
      for (sample=0; sample<sample_count; sample++) {
	switch (sample_format)
	  {
	  case SAMPLEFORMAT_UINT:
	    re = (float)(((uint16*)tiff_real_buf)[sample]);
	    im = (float)(((uint16*)tiff_imag_buf)[sample]);
	    break;
	  case SAMPLEFORMAT_INT:
	    re = (float)(((int16*)tiff_real_buf)[sample]);
	    im = (float)(((int16*)tiff_imag_buf)[sample]);
	    break;
	  }
	amp[sample] = sqrt(re*re + im*im);
	phase[sample] = atan2(im, re);
      }
      if (flip_horizontal) {
	for (sample=0; sample<sample_count; sample++)
	  tmp[sample] = amp[sample];
	for (sample=0; sample<sample_count; sample++)
	  amp[sample] = tmp[sample_count-sample-1];
      }
	  
      put_band_float_line(fp, meta, band*2, (int)row, amp);
      if (!ampOnly)
	put_band_float_line(fp, meta, band*2+1, (int)row, phase);
    }
      
    FREE(amp);
    FREE(phase);
    if (tmp)
      FREE(tmp);
    _TIFFfree(tiff_real_buf);
    _TIFFfree(tiff_imag_buf);
    GTIFFree(gtif);
    XTIFFClose(tiff);
  }

  // update the name field with directory name
  char *path = get_dirname(inBaseName);
  if (strlen(path)<=0)
    path = g_get_current_dir();
  char *p = path, *q = path;
  while (q) {
    if ((q = strchr(p, DIR_SEPARATOR)) != NULL)
      p = q+1;
  }
  sprintf(meta->general->basename, "%s", p);
  FREE(path);
  meta_write(meta, outDataName);

  meta_free(meta);
  FREE(radarsat2);
  FCLOSE(fp);
}

