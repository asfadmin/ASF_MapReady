#include "terrasar.h"
#include "doppler.h"
#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "xml_util.h"
#include "dateUtil.h"
#include <ctype.h>

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
  /* No speculation here - need some ground range data to verify
  if (strcmp_case(terrasar->projection, "GROUNDRANGE") == 0)
    terrasar->rangeResolution = xml_get_double_value(doc, 
    "level1Product.productInfo.imageDataInfo.imageRaster."
    "groundRangeResolution");
  */
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
  terrasar->rangeTime = xml_get_double_value(doc, 
     "level1Product.productInfo.sceneInfo.rangeTime.firstPixel");
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

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return terrasar;
}

void import_terrasar(const char *inBaseName, radiometry_t radiometry,
		     const char *outBaseName)
{
  FILE *fpIn, *fpOut;
  terrasar_meta *terrasar;
  meta_parameters *meta;
  char inDataName[1024], *inMetaName=NULL, *outDataName=NULL;
  char polarization[10], bands[30];         ;
  unsigned char intValue[4];
  float *amp = NULL, *phase = NULL, re, im;
  short int shortReal, shortImaginary;
  int ii, kk, ll, attribute;
  int asfv, aslv, rsfv, rslv;
  long file_size;

  // Check radiometry
  if (radiometry != r_AMP &&
      radiometry != r_SIGMA && radiometry != r_SIGMA_DB) {
    asfPrintWarning("Radiometry other than AMPLITUDE and SIGMA is not "
		    "supported for ALOS mosaics.\nDefaulting back to "
		    "AMPLITUDE.\n");
    radiometry = r_AMP;
  }
  
  if (!fileExists(inBaseName))
    inMetaName = appendExt(inBaseName, ".xml");
  else {
    inMetaName = (char *) MALLOC(sizeof(char)*1024);
    strcpy(inMetaName, inBaseName);
  }
  outDataName = appendExt(outBaseName, ".img");

  terrasar = read_terrasar_meta(inMetaName);
  asfPrintStatus("   ImageDataType: %s, imageDataFormat: %s\n",
		 terrasar->imageDataType, terrasar->imageDataFormat);
  meta = terrasar2meta(terrasar);
  meta_write(meta, outDataName);
  meta_free(meta);

  if (!fileExists(inMetaName))
    asfPrintError("Metadata file (%s) does not exist!\n", inMetaName);
  xmlDoc *doc = xmlReadFile(inMetaName, NULL, 0);
  if (!doc)
    asfPrintError("Could not parse file %s\n", inMetaName);

  // FIXME: check existence and size of files first, then assign dataName
  for (ii=0; ii<terrasar->numberOfLayers; ii++) {
    meta = meta_read(outDataName);
    attribute = xml_get_int_attribute(doc, 
      "level1Product.productComponents.imageData[%d].layerIndex", ii);
    if (attribute != (ii+1)) {
      printf("attribute: %i\n", attribute);
      asfPrintError("LayerIndex of imageData in metadata out of order!\n");
    }
    strcpy(inDataName, xml_get_string_value(doc, 
      "level1Product.productComponents.imageData[%d].file.location.path", ii));
    strcat(inDataName, "/");
    strcat(inDataName, xml_get_string_value(doc, 
      "level1Product.productComponents.imageData[%d].file.location.filename", 
      ii));
    file_size = xml_get_long_value(doc, 
      "level1Product.productComponents.imageData[%d].file.size", ii);
    if (!fileExists(inDataName))
      asfPrintError("Data file (%s) does not exist!\n", inDataName);
    if (fileSize(inDataName) != file_size)
      asfPrintWarning("Size of data file (%s) differ from metadata!\n", 
		      inDataName);
    if (strcmp_case(terrasar->imageDataType, "COMPLEX") == 0 &&
	strcmp_case(terrasar->imageDataFormat, "COSAR") == 0) {
      asfPrintStatus("\nIngesting data file %d/%d (%s) ...\n\n", 
		     ii+1, terrasar->numberOfLayers, inDataName);
      strcpy(polarization, xml_get_string_value(doc, 
	"level1Product.productComponents.imageData[%d].polLayer", ii));
      if (ii == 0) {
	sprintf(meta->general->bands, "AMP-%s,PHASE-%s", 
		polarization, polarization);
	meta->general->band_count = 2;
      }
      else {
	sprintf(bands, ",AMP-%s,PHASE-%s", polarization, polarization);
	strcat(meta->general->bands, bands);
	meta->general->band_count += 2;
      }
    }
    else
      asfPrintStatus("Data type (%s) and data format (%s) currently not "
		     "supported!\n", 
		     terrasar->imageDataType, terrasar->imageDataFormat);
    
    fpIn = FOPEN(inDataName, "rb");
    if (ii == 0)
      fpOut = FOPEN(outDataName, "wb");
    else
      fpOut = FOPEN(outDataName, "ab");
    
    FREAD(&intValue, 1, 4, fpIn);
    //int bytes_in_burst = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    //int range_sample_relative_index = bigInt32(intValue);;
    FREAD(&intValue, 1, 4, fpIn);
    int range_samples = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    int azimuth_samples = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    //int burst_index = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    int rangeline_total_number_bytes = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    int total_number_lines = bigInt32(intValue);
    FREAD(&intValue, 1, 4, fpIn);
    
    // Check for the first and last azimuth and range pixel
    asfv = 1; // azimuth sample first valid
    FSEEK(fpIn, 2*rangeline_total_number_bytes+8, SEEK_SET);
    for (kk=2; kk<range_samples; kk++) {
      FREAD(&intValue, 1, 4, fpIn);
      if (bigInt32(intValue) > asfv)
	asfv = bigInt32(intValue);
    }
    
    aslv = azimuth_samples; // azimuth sample last valid
    FSEEK(fpIn, 3*rangeline_total_number_bytes+8, SEEK_SET);
    for (kk=2; kk<range_samples; kk++) {
      FREAD(&intValue, 1, 4, fpIn);
      if (bigInt32(intValue) < aslv)
	aslv = bigInt32(intValue);
    }
    
    rsfv = 1; // range sample first valid
    rslv = range_samples; // range sample last valid
    for (kk=4; kk<total_number_lines; kk++) {
      FSEEK(fpIn, 4*rangeline_total_number_bytes, SEEK_SET);
      FREAD(&intValue, 1, 4, fpIn);
      if (bigInt32(intValue) > rsfv)
	rsfv = bigInt32(intValue);
      FREAD(&intValue, 1, 4, fpIn);
      if (bigInt32(intValue) < rslv)
	rslv = bigInt32(intValue);
    }
    
    /*
      asfPrintStatus("ASFV: %d, ASLV: %d, RSFV: %d, RSLV: %d\n\n", 
      asfv, aslv, rsfv, rslv);
    */
    
    meta->general->line_count = aslv - asfv + 1;
    meta->general->sample_count = rslv - rsfv + 1;
    amp = (float *) MALLOC(sizeof(float)*(rslv-rsfv+1));
    phase = (float *) MALLOC(sizeof(float)*(rslv-rsfv+1));
    
    // Read in the image
    for (ll=asfv+3; ll<aslv+3; ll++) {
      FSEEK(fpIn, ll*rangeline_total_number_bytes+8, SEEK_SET);
      for (kk=rsfv; kk<=rslv; kk++) {
	FREAD(&shortReal, 2, 1, fpIn);
	re = (float) shortReal;
	FREAD(&shortImaginary, 2, 1, fpIn);
	im = (float) shortImaginary;
	amp[kk-rsfv] = sqrt(re*re + im*im);
	phase[kk-rsfv] = atan2(im, re);
      }
      put_band_float_line(fpOut, meta, 0, ll-3, amp);
      put_band_float_line(fpOut, meta, 1, ll-3, phase);
      asfLineMeter(ll, azimuth_samples);
    }
    meta_write(meta, outDataName);
    meta_free(meta);
  }    

  FCLOSE(fpIn);
  FCLOSE(fpOut);
  FREE(amp);
  FREE(phase);
  FREE(terrasar);
  xmlFreeDoc(doc);
  xmlCleanupParser();
}

